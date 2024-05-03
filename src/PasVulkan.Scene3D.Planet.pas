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
     PasVulkan.Raytracing,
     PasVulkan.HighResolutionTimer,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.Image2D,
     PasVulkan.Scene3D.Renderer.Array2DImage,
     PasVulkan.Scene3D.Renderer.MipmapImage2D;

type TpvScene3DPlanets=class;

     EpvScene3DPlanet=class(Exception);

     { TpvScene3DPlanet }
     TpvScene3DPlanet=class
      public
       type THeightValue=TpvFloat;
            PHeightValue=^THeightValue;
            THeightMap=array of THeightValue;
            TSourcePrimitiveMode=
             (
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

             Vertices:TVkDeviceAddress;
             Indices:TVkDeviceAddress;

             Selected:TpvVector4;

             Textures:array[0..15,0..3] of TpvUInt32;

            end;
            PPlanetData=^TPlanetData;
            TPlanetDataVulkanBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
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
       const SourcePrimitiveMode:TpvScene3DPlanet.TSourcePrimitiveMode=TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles;
       type TMeshVertex=packed record
             Position:TpvVector3;
             OctahedralEncodedNormal:TpvInt16Vector2;
            end;
            PMeshVertex=^TMeshVertex;
            TMeshVertices=TpvDynamicArrayList<TMeshVertex>;
            TMeshIndex=TpvUInt32;
            PMeshIndex=^TMeshIndex;
            TMeshIndices=TpvDynamicArrayList<TMeshIndex>;
            TMeshDistance=TpvFloat;
            PMeshDistance=^TMeshDistance;
            TMeshDistances=TpvDynamicArrayList<TMeshDistance>;
            TTileDirtyQueueItem=TpvUInt32;
            PTileDirtyQueueItem=^TTileDirtyQueueItem;
            TTileDirtyQueueItems=TpvDynamicArrayList<TTileDirtyQueueItem>;
            TTileGeneration=TpvUInt64;
            PTileGeneration=^TTileGeneration;
            TTileGenerations=array of TTileGeneration;
            TTiledMeshIndexGroup=record
             FirstIndex:TVkUInt32;
             CountIndices:TVkUInt32;
            end;
            PTiledMeshIndexGroup=^TTiledMeshIndexGroup;
            TTiledMeshIndexGroups=TpvDynamicArrayList<TTiledMeshIndexGroup>;
            TGrassMetaData=packed record
             DrawIndexedIndirectCommand:TVkDrawIndexedIndirectCommand;
             CountVertices:TpvUInt32;
            end;
            PGrassMetaData=^TGrassMetaData;
            TGrassVertex=packed record

             PositionX:TpvFloat;
             PositionY:TpvFloat;
             PositionZ:TpvFloat;
             NormalTexCoordU:TpvUInt32; // RGBA10_A2

             TangentSign:TpvUInt32; // RGBA10_A2
             TexCoordV:TpvFloat;
             BladeIndex:TpvUInt32;
             BladeID:TpvUInt32;

            end;
            PGrassVertex=^TGrassVertex;
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
                   TDoubleBufferedVulkanBuffers=array[0..1] of TpvVulkanBuffer;
                   TVisualMeshVertexBufferCopies=TpvDynamicArrayList<TVkBufferCopy>;
             private    // All 2D maps are octahedral projected maps in this implementation (not equirectangular projected maps or cube maps)
              fPlanet:TpvScene3DPlanet;
              fInFlightFrameIndex:TpvInt32; // -1 is the ground truth instance, >=0 are the in-flight frame instances
              fHeightMap:THeightMap; // only on the ground truth instance, otherwise nil
              fHeightMapImage:TpvScene3DRendererMipmapImage2D; // R32_SFLOAT (at least for now, just for the sake of simplicity, later maybe R16_UNORM or R16_SNORM)
              fNormalMapImage:TpvScene3DRendererMipmapImage2D; // R16G16B16A16_SNORM (at least for now, just for the sake of simplicity, later maybe RGBA8_SNORM)
              fBlendMapImage:TpvScene3DRendererImage2D; // A2B10G10R10_UNORM_PACK32
              fGrassMapImage:TpvScene3DRendererImage2D; // R8G8B8A8_UNORM
              fWaterMapImage:TpvScene3DRendererImage2D; // R32_SFLOAT
              fWaterVisibilityBuffer:TpvVulkanBuffer;
              fTileDirtyMap:TpvScene3DPlanet.TData.TTileDirtyMap;
              fTileExpandedDirtyMap:TpvScene3DPlanet.TData.TTileDirtyMap;
              fTileDirtyMapBuffer:TpvVulkanBuffer;
              fTileExpandedDirtyMapBuffer:TpvVulkanBuffer;
              fTileDirtyQueueBuffer:TpvVulkanBuffer;
              fTiledMeshBoundingBoxesBuffer:TpvVulkanBuffer;
              fTiledMeshBoundingSpheresBuffer:TpvVulkanBuffer;
              fTiledVisualMeshIndexGroupsBuffer:TpvVulkanBuffer;
              fVisualMeshIndexBuffer:TpvVulkanBuffer;
              fVisualMeshVertexBuffers:TDoubleBufferedVulkanBuffers; // Double-buffered
              fVisualMeshDistanceBuffers:TDoubleBufferedVulkanBuffers; // Double-buffered
              fVisualMeshVertexBufferCopies:TVisualMeshVertexBufferCopies;
              fVisualMeshDistanceBufferCopies:TVisualMeshVertexBufferCopies;
              fVisualMeshVertexBufferUpdateIndex:TPasMPInt32;
              fVisualMeshVertexBufferNextRenderIndex:TPasMPInt32;
              fVisualMeshVertexBufferRenderIndex:TPasMPInt32;
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
              fGrassMapGeneration:TpvUInt64;
              fWaterMapGeneration:TpvUInt64;
//            fVisualMeshGeneration:TpvUInt64;
              fOwnershipHolderState:TpvScene3DPlanet.TData.TOwnershipHolderState;
              fSelectedRegion:TpvVector4;
              fSelectedRegionProperty:TpvVector4Property;
              fModifyHeightMapActive:Boolean;
              fModifyHeightMapBorderRadius:TpvScalar;
              fModifyHeightMapFactor:TpvScalar;
              fModifyGrassMapActive:Boolean;
              fModifyGrassMapBorderRadius:TpvScalar;
              fModifyGrassMapFactor:TpvScalar;
              fModifyWaterMapActive:Boolean;
              fModifyWaterMapBorderRadius:TpvScalar;
              fModifyWaterMapFactor:TpvScalar;
              fWireframeActive:Boolean;
              fDisplacementMappingActive:Boolean;
              fParallaxMappingActive:Boolean;
              fMeshVertices:TMeshVertices;
              fMeshIndices:TMeshIndices;
              fTileDirtyQueueItems:TTileDirtyQueueItems;
              fTileGenerations:TTileGenerations;
              fTiledMeshBoundingBoxes:TTiledMeshBoundingBoxes;
              fTiledMeshBoundingSpheres:TTiledMeshBoundingSpheres;
              fRaytracingTileQueue:Pointer;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32); reintroduce;
              destructor Destroy; override; 
              procedure AcquireOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure ReleaseOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure AcquireOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure ReleaseOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
              procedure CheckDirtyMap;
              procedure TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aInFlightFrameData:TData;
                                   const aTransferHeightMap:Boolean;
                                   const aTransferGrass:Boolean;
                                   const aTransferWater:Boolean);
              procedure Assign(const aData:TData);
             published
              property Planet:TpvScene3DPlanet read fPlanet;
              property InFlightFrameIndex:TpvInt32 read fInFlightFrameIndex;
              property HeightMap:THeightMap read fHeightMap;              
              property HeightMapImage:TpvScene3DRendererMipmapImage2D read fHeightMapImage;
              property NormalMapImage:TpvScene3DRendererMipmapImage2D read fNormalMapImage;
              property BlendMapImage:TpvScene3DRendererImage2D read fBlendMapImage;
              property TileDirtyMapBuffer:TpvVulkanBuffer read fTileDirtyMapBuffer;
              property TileExpandedDirtyMapBuffer:TpvVulkanBuffer read fTileExpandedDirtyMapBuffer;
              property TileDirtyQueueBuffer:TpvVulkanBuffer read fTileDirtyQueueBuffer;
              property TiledMeshBoundingBoxesBuffer:TpvVulkanBuffer read fTiledMeshBoundingBoxesBuffer;
              property TiledMeshBoundingSpheresBuffer:TpvVulkanBuffer read fTiledMeshBoundingSpheresBuffer;
              property TiledVisualMeshIndexGroupsBuffer:TpvVulkanBuffer read fTiledVisualMeshIndexGroupsBuffer;
             public
              property VisualMeshVertexBuffers:TDoubleBufferedVulkanBuffers read fVisualMeshVertexBuffers;
              property VisualMeshDistanceBuffers:TDoubleBufferedVulkanBuffers read fVisualMeshDistanceBuffers;
             published
              property VisualMeshVertexBufferUpdateIndex:TPasMPInt32 read fVisualMeshVertexBufferUpdateIndex;
              property VisualMeshVertexBufferRenderIndex:TPasMPInt32 read fVisualMeshVertexBufferRenderIndex;
              property VisualMeshIndexBuffer:TpvVulkanBuffer read fVisualMeshIndexBuffer;
              property PhysicsMeshVertexBuffer:TpvVulkanBuffer read fPhysicsMeshVertexBuffer;
              property PhysicsMeshIndexBuffer:TpvVulkanBuffer read fPhysicsMeshIndexBuffer;
              property RayIntersectionResultBuffer:TpvVulkanBuffer read fRayIntersectionResultBuffer;
              property MeshVertices:TMeshVertices read fMeshVertices;
//            property MeshIndices:TMeshIndices read fMeshIndices;
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
              property ModifyGrassMapActive:Boolean read fModifyGrassMapActive write fModifyGrassMapActive;
              property ModifyGrassMapBorderRadius:TpvScalar read fModifyGrassMapBorderRadius write fModifyGrassMapBorderRadius;
              property ModifyGrassMapFactor:TpvScalar read fModifyGrassMapFactor write fModifyGrassMapFactor;
              property ModifyWaterMapActive:Boolean read fModifyWaterMapActive write fModifyWaterMapActive;
              property ModifyWaterMapBorderRadius:TpvScalar read fModifyWaterMapBorderRadius write fModifyWaterMapBorderRadius;
              property ModifyWaterMapFactor:TpvScalar read fModifyWaterMapFactor write fModifyWaterMapFactor;
              property WireframeActive:Boolean read fWireframeActive write fWireframeActive;
              property DisplacementMappingActive:Boolean read fDisplacementMappingActive write fDisplacementMappingActive;
              property ParallaxMappingActive:Boolean read fParallaxMappingActive write fParallaxMappingActive;
            end;
            TInFlightFrameDataList=TpvObjectGenericList<TData>;
            { TGrassMapInitialization }
            TGrassMapInitialization=class
             public
              type TPushConstants=packed record
                    Dummy:TpvUInt32;
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
            { TGrassMapModification }
            TGrassMapModification=class
             public
              type TPushConstants=packed record
                    PositionRadius:TpvVector4; // xyz = position, w = radius
                    InnerRadiusValueMinMax:TpvVector4; // x = inner radius, y = value, z = min, w = max
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
            { TWaterMapInitialization }
            TWaterMapInitialization=class
             public
              type TPushConstants=packed record
                    Dummy:TpvUInt32;
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
            { TWaterMapModification }
            TWaterMapModification=class
             public
              type TPushConstants=packed record
                    PositionRadius:TpvVector4; // xyz = position, w = radius
                    InnerRadiusValueMinMax:TpvVector4; // x = inner radius, y = value, z = min, w = max
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
            { THeightMapDataInitialization }
            THeightMapDataInitialization=class
             public 
              type TPushConstants=packed record
                    InputBottomRadius:TpvFloat;
                    InputTopRadius:TpvFloat;
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;
                    InputResolution:TpvUInt32;
                    TileMapResolution:TpvUInt32;
                    TileMapShift:TpvUInt32;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fVulkanDevice:TpvVulkanDevice;
              fData:TMemoryStream;
              fDataBuffer:TpvVulkanBuffer;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fPipeline:TpvVulkanComputePipeline;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public 
              constructor Create(const aPlanet:TpvScene3DPlanet;const aData:TStream); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end; 
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
              fDescriptorSets:array[0..1] of TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aPhysics:Boolean); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TMeshDistanceGeneration }
            TMeshDistanceGeneration=class
             public
              type TPushConstants=packed record
                    TileMapResolution:TpvUInt32;
                    TileResolution:TpvUInt32;
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
              fDescriptorSets:array[0..1] of TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TRaytracingTile }
            TRaytracingTile=class
             public
              fPlanet:TpvScene3DPlanet;
              fTileIndex:TpvSizeInt;
              fBLASGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry;
              fBLAS:TpvRaytracingBottomLevelAccelerationStructure;
              fBLASScratchSize:TVkDeviceSize;
              fBLASBuffer:TpvVulkanBuffer;
              fBLASInstance:TpvRaytracingBottomLevelAccelerationStructureInstance;
              fNewGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
              fGeneration:TpvUInt64;
              fMustUpdate:Boolean;
              fScratchOffset:TVkDeviceSize;
              fScratchPass:TpvUInt64;
             private
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aTileIndex:TpvSizeInt);
              destructor Destroy; override;
              function CheckAndUpdateGeneration(const aInFlightFrameIndex:TpvSizeInt):Boolean;
              function Update(const aInFlightFrameIndex:TpvSizeInt):Boolean;
             public
              property Planet:TpvScene3DPlanet read fPlanet;
              property TileIndex:TpvSizeInt read fTileIndex;
              property BLASGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry read fBLASGeometry;
              property BLAS:TpvRaytracingBottomLevelAccelerationStructure read fBLAS;
              property BLASScratchSize:TVkDeviceSize read fBLASScratchSize;
              property BLASBuffer:TpvVulkanBuffer read fBLASBuffer;
              property BLASInstance:TpvRaytracingBottomLevelAccelerationStructureInstance read fBLASInstance;
              property Generation:TpvUInt64 read fGeneration write fGeneration;
              property MustUpdate:Boolean read fMustUpdate write fMustUpdate;
              property ScratchOffset:TVkDeviceSize read fScratchOffset write fScratchOffset;
              property ScratchPass:TpvUInt64 read fScratchPass write fScratchPass;
            end;
            { TRaytracingTiles }
            TRaytracingTiles=TpvObjectGenericList<TRaytracingTile>;
            { TRaytracingTileQueues }
            TRaytracingTileQueues=array[0..1] of TRaytracingTiles;
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
            { TCullPass } // Used by multiple TpvScene3DPlanet instances per renderer instance 
            TCullPass=class
             public
              type TPlanetPushConstants=packed record
                    ModelMatrix:TpvMatrix4x4;
                    BaseViewIndex:TpvUInt32;
                    CountViews:TpvUInt32;
                    AdditionalViewIndex:TpvUInt32;
                    CountAdditionalViews:TpvUInt32;
                    TileMapResolution:TpvUInt32;
                    TileResolution:TpvUInt32;
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;
                    MinimumLODLevel:TpvUInt32;
                   end;
                   PPlanetPushConstants=^TPlanetPushConstants;
                   TGrassPushConstants=packed record
                    ModelMatrix:TpvMatrix4x4;
                    ViewBaseIndex:TpvUInt32;
                    CountViews:TpvUInt32;
                    CountAllViews:TpvUInt32;
                    MaximalCountBladesPerPatch:TpvUInt32;
                    MaximumDistance:TpvFloat;
                    GrassHeight:TpvFloat;
                    GrassThickness:TpvFloat;
                    Time:TpvFloat;
                    TileMapResolution:TpvUInt32;
                    TileResolution:TpvUInt32;
                    LOD:TpvUInt32;
                    FrameIndex:TpvUInt32;
                    MaximumCountTaskIndices:TpvUInt32;
                    MaximumCountVertices:TpvUInt32;
                    MaximumCountIndices:TpvUInt32;
                   end;
                   PGrassPushConstants=^TGrassPushConstants;
              private
               fRenderer:TObject;
               fRendererInstance:TObject;
               fScene3D:TObject;
               fCullRenderPass:TpvScene3DRendererCullRenderPass;
               fPass:TpvSizeInt;
               fVulkanDevice:TpvVulkanDevice;
               fPlanetComputeShaderModule:TpvVulkanShaderModule;
               fPlanetComputeShaderStage:TpvVulkanPipelineShaderStage;
               fPlanetPipeline:TpvVulkanComputePipeline;
               fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
               fDescriptorPool:TpvVulkanDescriptorPool;
               fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
               fPlanetPipelineLayout:TpvVulkanPipelineLayout;
               fPlanetPushConstants:TPlanetPushConstants;
               fGrassTaskComputeShaderModule:TpvVulkanShaderModule;
               fGrassTaskComputeShaderStage:TpvVulkanPipelineShaderStage;
               fGrassMeshComputeShaderModule:TpvVulkanShaderModule;
               fGrassMeshComputeShaderStage:TpvVulkanPipelineShaderStage;
               fGrassPipelineLayout:TpvVulkanPipelineLayout;
               fGrassTaskPipeline:TpvVulkanComputePipeline;
               fGrassMeshPipeline:TpvVulkanComputePipeline;
               fGrassPushConstants:TGrassPushConstants;
              public
               constructor Create(const aRenderer:TObject;const aRendererInstance:TObject;const aScene3D:TObject;const aCullRenderPass:TpvScene3DRendererCullRenderPass;const aPass:TpvSizeInt); reintroduce;
               destructor Destroy; override;               
               procedure AllocateResources;
               procedure ReleaseResources;
               procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt);
              public
               property PlanetPushConstants:TPlanetPushConstants read fPlanetPushConstants write fPlanetPushConstants;
//             property GrassPushConstants:TGrassPushConstants read fGrassPushConstants write fGrassPushConstants;
            end;
            { TWaterCullPass }
            TWaterCullPass=class
             public
              type TPushConstants=packed record
                    TileMapResolution:TpvUInt32;
                    TileShift:TpvUInt32;
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;
                    MaxWaterAdditionalWavesHeight:TpvFloat;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fPlanet:TpvScene3DPlanet;
              fScene3D:TObject;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPipeline:TpvVulkanComputePipeline;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet); reintroduce;
              destructor Destroy; override;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TWaterPrepass }
            TWaterPrepass=class
             public
              type TPushConstants=packed record
                    ModelMatrix:TpvMatrix4x4;
                    ViewBaseIndex:TpvUInt32;
                    CountViews:TpvUInt32;
                    Time:TpvFloat;
                    BottomRadius:TpvFloat;
                    TopRadius:TpvFloat;
                   end;
                   PPushConstants=^TPushConstants;
             private
              fRenderer:TObject;
              fRendererInstance:TObject;
              fScene3D:TObject;
              fVulkanDevice:TpvVulkanDevice;
              fComputeShaderModule:TpvVulkanShaderModule;
              fComputeShaderStage:TpvVulkanPipelineShaderStage;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fPipeline:TpvVulkanComputePipeline;
              fPushConstants:TPushConstants;
             public
              constructor Create(const aRenderer:TObject;const aRendererInstance:TObject;const aScene3D:TObject); reintroduce;
              destructor Destroy; override;
              procedure AllocateResources;
              procedure ReleaseResources;
              procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
            { TRenderPass } // Used by multiple TpvScene3DPlanet instances inside the TpvScene3D render passes per renderer instance 
            TRenderPass=class
             public
              type TMode=
                    (
                     ShadowMap,
                     ShadowMapDisocclusion,
                     ReflectiveShadowMap,
                     DepthPrepass,
                     DepthPrepassDisocclusion,
                     Opaque
                    );
                   PMode=^TMode;
                   TPlanetPushConstants=packed record
                    ViewBaseIndex:TpvUInt32;
                    CountViews:TpvUInt32;
                    CountQuadPointsInOneDirection:TpvUInt32;
                    CountAllViews:TpvUInt32;
                    ResolutionXY:TpvUInt32;
                    TessellationFactor:TpvFloat;
                    Jitter:TpvVector2;
                    FrameIndex:TpvUInt32;
                    Reversed:TpvUInt32;
                    PlanetData:TVkDeviceAddress;
                   end;
                   PPlanetPushConstants=^TPlanetPushConstants;
                   TGrassPushConstants=packed record
                    ModelMatrix:TpvMatrix4x4;

                    ViewBaseIndex:TpvUInt32;
                    CountViews:TpvUInt32;
                    CountAllViews:TpvUInt32;
                    MaximalCountBladesPerPatch:TpvUInt32;

                    MaximumDistance:TpvFloat;
                    GrassHeight:TpvFloat;
                    GrassThickness:TpvFloat;
                    Time:TpvFloat;

                    TileMapResolution:TpvUInt32;
                    TileResolution:TpvUInt32;
                    ResolutionXY:TpvUInt32;
                    FrameIndex:TpvUInt32;

                    Jitter:TpvVector2;
                   end;
                   PGrassPushConstants=^TGrassPushConstants;
             private
              fRenderer:TObject;
              fRendererInstance:TObject;
              fScene3D:TObject; 
              fMode:TpvScene3DPlanet.TRenderPass.TMode;
              fVulkanDevice:TpvVulkanDevice;              
              fRenderPass:TpvVulkanRenderPass;
              fPlanetVertexShaderModule:TpvVulkanShaderModule;
              fPlanetFragmentShaderModule:TpvVulkanShaderModule;
              fPlanetVertexShaderStage:TpvVulkanPipelineShaderStage;
              fPlanetFragmentShaderStage:TpvVulkanPipelineShaderStage;
              fGrassVertexShaderModule:TpvVulkanShaderModule;
              fGrassTaskShaderModule:TpvVulkanShaderModule;
              fGrassMeshShaderModule:TpvVulkanShaderModule;
              fGrassFragmentShaderModule:TpvVulkanShaderModule;
              fGrassVertexShaderStage:TpvVulkanPipelineShaderStage;
              fGrassTaskShaderStage:TpvVulkanPipelineShaderStage;
              fGrassMeshShaderStage:TpvVulkanPipelineShaderStage;
              fGrassFragmentShaderStage:TpvVulkanPipelineShaderStage;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fPlanetPipelineLayout:TpvVulkanPipelineLayout;
              fPlanetPipeline:TpvVulkanGraphicsPipeline;
              fPlanetPushConstants:TPlanetPushConstants;
              fGrassPipelineLayout:TpvVulkanPipelineLayout;
              fGrassPipeline:TpvVulkanGraphicsPipeline;
              fGrassPushConstants:TGrassPushConstants;
              fShaderStageFlags:TVkShaderStageFlags;
              fWidth:TpvInt32;
              fHeight:TpvInt32;
              fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
              fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
             public
              constructor Create(const aRenderer:TObject;
                                 const aRendererInstance:TObject;
                                 const aScene3D:TObject;
                                 const aMode:TpvScene3DPlanet.TRenderPass.TMode;
                                 const aResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
                                 const aResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource); reintroduce;
              destructor Destroy; override;
              procedure AllocateResources(const aRenderPass:TpvVulkanRenderPass;
                                          const aWidth:TpvInt32;
                                          const aHeight:TpvInt32;
                                          const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));
              procedure ReleaseResources;
              procedure Draw(const aInFlightFrameIndex,aFrameIndex,aRenderPassIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);
             public
              property PushConstants:TPlanetPushConstants read fPlanetPushConstants write fPlanetPushConstants;
            end;
            { TWaterRenderPass } // Used by multiple TpvScene3DPlanet instances inside the TpvScene3D render passes per renderer instance 
            TWaterRenderPass=class
             public
              type TPushConstants=packed record
                    ViewBaseIndex:TpvUInt32;
                    CountViews:TpvUInt32;
                    CountAllViews:TpvUInt32;
                    CountQuadPointsInOneDirection:TpvUInt32;

                    ResolutionXY:TpvUInt32;
                    TessellationFactor:TpvFloat;
                    Jitter:TpvVector2;

                    FrameIndex:TpvUInt32;
                    Time:TpvFloat;
                    PlanetData:TVkDeviceAddress;

                    TileMapResolution:TpvUInt32;

                   end;
                   PPushConstants=^TPushConstants;
             private
              fRenderer:TObject;
              fRendererInstance:TObject;
              fScene3D:TObject;
              fVulkanDevice:TpvVulkanDevice;
              fResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
              fResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource;
              fRenderPass:TpvVulkanRenderPass;
              fUnderwaterVertexShaderModule:TpvVulkanShaderModule;
              fUnderwaterFragmentShaderModule:TpvVulkanShaderModule;
              fUnderwaterVertexShaderStage:TpvVulkanPipelineShaderStage;
              fUnderwaterFragmentShaderStage:TpvVulkanPipelineShaderStage;              
              fWaterVertexShaderModule:TpvVulkanShaderModule;
              fWaterTessellationControlShaderModule:TpvVulkanShaderModule;
              fWaterTessellationEvaluationShaderModule:TpvVulkanShaderModule;
              fWaterFragmentShaderModule:TpvVulkanShaderModule;
              fWaterVertexShaderStage:TpvVulkanPipelineShaderStage;
              fWaterTessellationControlShaderStage:TpvVulkanPipelineShaderStage;
              fWaterTessellationEvaluationShaderStage:TpvVulkanPipelineShaderStage;
              fWaterFragmentShaderStage:TpvVulkanPipelineShaderStage;
              fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fPipelineLayout:TpvVulkanPipelineLayout;
              fUnderwaterPipeline:TpvVulkanGraphicsPipeline;
              fWaterPipeline:TpvVulkanGraphicsPipeline;
              fPushConstants:TPushConstants;
              fMSAA:Boolean;
              fPass:TpvSizeInt;
              fWidth:TpvInt32;
              fHeight:TpvInt32;
            public  
              constructor Create(const aRenderer:TObject;
                                 const aRendererInstance:TObject;
                                 const aScene3D:TObject;
                                 const aMSAA:Boolean;
                                 const aPass:TpvSizeInt;
                                 const aResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
                                 const aResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource); reintroduce;
              destructor Destroy; override;
              procedure AllocateResources(const aRenderPass:TpvVulkanRenderPass;
                                          const aPassVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                                          const aWidth:TpvInt32;
                                          const aHeight:TpvInt32;                                          
                                          const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT));
              procedure ReleaseResources;
              procedure Draw(const aInFlightFrameIndex,aFrameIndex,aRenderPassIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer;const aPassDescriptorSet:TpvVulkanDescriptorSet);
             public
              property PushConstants:TPushConstants read fPushConstants write fPushConstants;
            end;
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
              fPlanet:TpvScene3DPlanet;
              fRendererInstance:TObject;
              fKey:TKey;
              fMinimumLODLevel:TpvSizeInt;
             public 
              constructor Create(const aPlanet:TpvScene3DPlanet;const aRendererInstance:TObject);
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;              
            end;
            { TRendererInstances }
            TRendererInstances=TpvObjectGenericList<TRendererInstance>;
            { TRendererInstanceHashMap }
            TRendererInstanceHashMap=TpvHashMap<TRendererInstance.TKey,TRendererInstance>;
            { TRendererViewInstance }
            TRendererViewInstance=class
             public
              type { TKey }
                   TKey=record
                    public
                     fRendererInstance:TObject;
                     fRenderPassIndex:TpvSizeInt;
                    public
                     constructor Create(const aRendererInstance:TObject;const aRenderPassIndex:TpvSizeInt);
                   end;
                   PKey=^TKey;
             private
              fPlanet:TpvScene3DPlanet;
              fRendererInstance:TObject;
              fRenderPassIndex:TpvSizeInt;
              fKey:TKey;
              fVulkanVisiblityBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
              fVulkanDrawIndexedIndirectCommandBuffer:TpvVulkanBuffer;
              fVulkanVisibleTileListBuffer:TpvVulkanBuffer;
              fVulkanGrassTaskIndicesBuffer:TpvVulkanBuffer;
              fVulkanGrassMetaDataBuffer:TpvVulkanBuffer;
              fVulkanGrassVerticesBuffer:TpvVulkanBuffer;
              fVulkanGrassIndicesBuffer:TpvVulkanBuffer;
              fVulkanWaterAccelerationImage:TpvScene3DRendererArray2DImage;
              fPlanetCullDescriptorPool:TpvVulkanDescriptorPool;
              fPlanetCullDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
              fGrassCullDescriptorPool:TpvVulkanDescriptorPool;
              fGrassCullDescriptorSets:array[0..1] of TpvVulkanDescriptorSet;
              fWaterPrepassDescriptorPool:TpvVulkanDescriptorPool;
              fWaterPrepassDescriptorSet:TpvVulkanDescriptorSet;
              fWaterRenderDescriptorPool:TpvVulkanDescriptorPool;
              fWaterRenderDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aRendererInstance:TObject;const aRenderPassIndex:TpvSizeInt;const aMainViewPort:Boolean);
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            { TRendererViewInstances }
            TRendererViewInstances=TpvObjectGenericList<TRendererViewInstance>;
            { TRendererViewInstances }
            TRendererViewInstanceHashMap=TpvHashMap<TRendererViewInstance.TKey,TRendererViewInstance>;
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
       fGrassMapResolution:TpvInt32;
       fWaterMapResolution:TpvInt32;
       fTileMapResolution:TpvInt32;
       fTileMapShift:TpvInt32;
       fTileMapBits:TpvInt32;
       fVisualTileResolution:TpvInt32;
       fPhysicsTileResolution:TpvInt32;
       fVisualResolution:TpvSizeInt;
       fPhysicsResolution:TpvSizeInt;
       fBottomRadius:TpvFloat; // Start of the lowest planet ground
       fTopRadius:TpvFloat; // End of the atmosphere
       fHeightMapScale:TpvFloat; // Scale factor for the height map
       fMaxGrassVertices:TpvSizeInt;
       fMaxGrassIndices:TpvSizeInt;
       fCountVisualMeshIndices:TpvSizeInt;
       fCountVisualMeshLODLevels:TpvSizeInt;
       fCountPhysicsMeshIndices:TpvSizeInt; 
       fCountPhysicsMeshLODLevels:TpvSizeInt;
       fVisualMeshLODOffsets:TSizeIntArray;
       fVisualMeshLODCounts:TSizeIntArray;
       fPhysicsMeshLODOffsets:TSizeIntArray;
       fPhysicsMeshLODCounts:TSizeIntArray;
       fTiledVisualMeshIndices:TMeshIndices;
       fTiledVisualMeshIndexGroups:TTiledMeshIndexGroups;
       fTiledPhysicsMeshIndices:TMeshIndices;
       fTiledPhysicsMeshIndexGroups:TTiledMeshIndexGroups;
       fData:TData;
       fInFlightFrameDataList:TInFlightFrameDataList;
       fReleaseFrameCounter:TpvInt32;
       fReady:TPasMPBool32;
       fInFlightFrameReady:array[0..MaxInFlightFrames-1] of TPasMPBool32;
       fGrassMapInitialization:TGrassMapInitialization;
       fGrassMapModification:TGrassMapModification;
       fWaterMapInitialization:TWaterMapInitialization;
       fWaterMapModification:TWaterMapModification;
       fHeightMapRandomInitialization:THeightMapRandomInitialization;
       fHeightMapModification:THeightMapModification;
       fHeightMapFlatten:THeightMapFlatten;
       fTiledMeshBoundingVolumesGeneration:TTiledMeshBoundingVolumesGeneration;
       fTileDirtyExpansion:TTileDirtyExpansion;
       fTileDirtyQueueGeneration:TTileDirtyQueueGeneration;
       fNormalMapGeneration:TNormalMapGeneration;
       fHeightMapMipMapGeneration:THeightMapMipMapGeneration;
       fNormalMapMipMapGeneration:TNormalMapMipMapGeneration;
{      fVisualMeshIndexGeneration:TMeshIndexGeneration;
       fPhysicsMeshIndexGeneration:TMeshIndexGeneration;}
       fVisualMeshVertexGeneration:TMeshVertexGeneration;
       fVisualMeshDistanceGeneration:TMeshDistanceGeneration;
       fPhysicsMeshVertexGeneration:TMeshVertexGeneration;
       fWaterCullPass:TWaterCullPass;
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
       fPlanetDataVulkanBuffers:TPlanetDataVulkanBuffers;
       fMaterials:TMaterials;
       fPointerToMaterials:PMaterials;
       fDescriptorPool:TpvVulkanDescriptorPool;
       fDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fRaytracingLock:TPasMPCriticalSection;
       fRaytracingTiles:TRaytracingTiles;
       fRaytracingTileQueue:TRaytracingTiles;
       fRaytracingTileNextQueue:TRaytracingTiles;
       fRaytracingTileQueues:TRaytracingTileQueues;
       fRaytracingTileQueueUpdateIndex:TPasMPUInt32;
       fRendererInstanceListLock:TPasMPCriticalSection;
       fRendererInstances:TRendererInstances;
       fRendererInstanceHashMap:TRendererInstanceHashMap;
       fRendererViewInstanceListLock:TPasMPCriticalSection;
       fRendererViewInstances:TRendererViewInstances;
       fRendererViewInstanceHashMap:TRendererViewInstanceHashMap;
      private
       procedure GenerateMeshIndices(const aTiledMeshIndices:TpvScene3DPlanet.TMeshIndices;
                                     const aTiledMeshIndexGroups:TpvScene3DPlanet.TTiledMeshIndexGroups;
                                     const aTileResolution:TpvInt32;
                                     const aTileMapResolution:TpvInt32;
                                     out aCountMeshIndices:TpvSizeInt;
                                     out aCountMeshLODLevels:TpvSizeInt;
                                     out aMeshLODOffsets:TpvScene3DPlanet.TSizeIntArray;
                                     out aMeshLODCounts:TpvScene3DPlanet.TSizeIntArray);
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
       class function CreatePlanetDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice;const aMeshShaders:Boolean):TpvVulkanDescriptorSetLayout; static;
       class function CreatePlanetDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool; static;
       class function CreatePlanetCullDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout; static;
       class function CreatePlanetCullDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool; static;
       class function CreatePlanetGrassCullAndMeshGenerationDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice;const aMeshShaders:Boolean):TpvVulkanDescriptorSetLayout; static;
       class function CreatePlanetGrassCullAndMeshGenerationDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aMeshShaders:Boolean;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool; static;
       class function CreatePlanetWaterCullDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout; static;
       class function CreatePlanetWaterCullDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool; static;
       class function CreatePlanetWaterPrepassDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout; static;
       class function CreatePlanetWaterPrepassDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool; static;
       class function CreatePlanetWaterRenderDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout; static;
       class function CreatePlanetWaterRenderDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool; static;
       procedure BeginUpdate;
       procedure EndUpdate;
       procedure FlushUpdate;
       procedure Initialize(const aPasMPInstance:TPasMP=nil;const aData:TStream=nil;const aDataFreeOnDestroy:boolean=false);
       procedure Flatten(const aVector:TpvVector3;const aInnerRadius,aOuterRadius,aTargetHeight:TpvFloat);
       function RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;out aHitNormal:TpvVector3;out aHitTime:TpvScalar):boolean;
       procedure Update(const aInFlightFrameIndex:TpvSizeInt);
       procedure FrameUpdate(const aInFlightFrameIndex:TpvSizeInt);
       procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;const aRendererInstance:TObject;const aRenderPassIndex:TpvSizeInt;const aViewPortWidth,aViewPortHeight:TpvInt32;const aMainViewPort:Boolean);
       procedure UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
       procedure BeginFrame(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
       procedure EndFrame(const aInFlightFrameIndex:TpvSizeInt;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
       procedure ExportPhysicsMeshToOBJ(const aStream:TStream); overload;
       procedure ExportPhysicsMeshToOBJ(const aFileName:TpvUTF8String); overload;
      published
       property Scene3D:TObject read fScene3D;
       property HeightMapResolution:TpvInt32 read fHeightMapResolution;
       property GrassMapResolution:TpvInt32 read fGrassMapResolution;
       property WaterMapResolution:TpvInt32 read fWaterMapResolution;
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
       property PlanetDataVulkanBuffers:TPlanetDataVulkanBuffers read fPlanetDataVulkanBuffers;
       property Materials:PMaterials read fPointerToMaterials;
       property VisualMeshLODOffsets:TSizeIntArray read fVisualMeshLODOffsets;
       property VisualMeshLODCounts:TSizeIntArray read fVisualMeshLODCounts;
       property PhysicsMeshLODOffsets:TSizeIntArray read fPhysicsMeshLODOffsets;
       property PhysicsMeshLODCounts:TSizeIntArray read fPhysicsMeshLODCounts;
       property TiledVisualMeshIndices:TMeshIndices read fTiledVisualMeshIndices;
       property TiledVisualMeshIndexGroups:TTiledMeshIndexGroups read fTiledVisualMeshIndexGroups;
       property TiledPhysicsMeshIndices:TMeshIndices read fTiledPhysicsMeshIndices;
       property TiledPhysicsMeshIndexGroups:TTiledMeshIndexGroups read fTiledPhysicsMeshIndexGroups;
       property RaytracingTiles:TRaytracingTiles read fRaytracingTiles;
       property RaytracingTileQueue:TRaytracingTiles read fRaytracingTileQueue;
       property RaytracingTileQueues:TRaytracingTileQueues read fRaytracingTileQueues;
       property RaytracingTileQueueUpdateIndex:TPasMPUInt32 read fRaytracingTileQueueUpdateIndex;
     end;

     TpvScene3DPlanets=class(TpvObjectGenericList<TpvScene3DPlanet>)
      private
       fScene3D:TObject;
       fLock:TPasMPMultipleReaderSingleWriterLock;
      public
       constructor Create(const aScene3D:TObject); reintroduce;
       destructor Destroy; override;
      published
       property Scene3D:TObject read fScene3D;
       property Lock:TPasMPMultipleReaderSingleWriterLock read fLock;
     end;

implementation

uses PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.VirtualFileSystem;

type TVector3Array=TpvDynamicArray<TpvVector3>;
     TIndexArray=TpvDynamicArray<TpvUInt32>;

{ TpvScene3DPlanet.TData }

constructor TpvScene3DPlanet.TData.Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32);
var ImageSharingMode:TVkSharingMode;
    ImageQueueFamilyIndices:TpvVulkanQueueFamilyIndices;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 if fInFlightFrameIndex<0 then begin
  ImageSharingMode:=TVkSharingMode.VK_SHARING_MODE_EXCLUSIVE;
  ImageQueueFamilyIndices:=[];
 end else begin
  ImageSharingMode:=fPlanet.fInFlightFrameSharingMode;
  ImageQueueFamilyIndices:=fPlanet.fInFlightFrameQueueFamilyIndices;
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
  fGrassMapGeneration:=0;
  fWaterMapGeneration:=0;
 end else begin
  fHeightMapGeneration:=High(TpvUInt64);
  fGrassMapGeneration:=High(TpvUInt64);
  fWaterMapGeneration:=High(TpvUInt64);
 end;

 fHeightMapProcessedGeneration:=High(TpvUInt64);

 fModelMatrix:=TpvMatrix4x4.Identity;

 fHeightMapImage:=nil;

 fNormalMapImage:=nil;

 fBlendMapImage:=nil;

 fGrassMapImage:=nil;

 fWaterMapImage:=nil;

 fWaterVisibilityBuffer:=nil;

 fTileDirtyMap:=nil;

 fTileExpandedDirtyMap:=nil;

 fTileDirtyMapBuffer:=nil;

 fTileExpandedDirtyMapBuffer:=nil;

 fTileDirtyQueueBuffer:=nil;

 fTiledMeshBoundingBoxesBuffer:=nil;

 fTiledMeshBoundingSpheresBuffer:=nil;

 fTiledVisualMeshIndexGroupsBuffer:=nil;

 fVisualMeshVertexBuffers[0]:=nil;
 fVisualMeshVertexBuffers[1]:=nil;

 fVisualMeshDistanceBuffers[0]:=nil;
 fVisualMeshDistanceBuffers[1]:=nil;

 fVisualMeshVertexBufferCopies:=nil;

 fVisualMeshDistanceBufferCopies:=nil;

 fVisualMeshVertexBufferUpdateIndex:=0;
 fVisualMeshVertexBufferNextRenderIndex:=0;
 fVisualMeshVertexBufferRenderIndex:=0;

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
                                                          ImageQueueFamilyIndices,
                                                          pvAllocationGroupIDScene3DPlanetStatic);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fHeightMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fHeightMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fHeightMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fHeightMapImage.ImageView');

  fNormalMapImage:=TpvScene3DRendererMipmapImage2D.Create(fPlanet.fVulkanDevice,
                                                          fPlanet.fHeightMapResolution,
                                                          fPlanet.fHeightMapResolution,
                                                          VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                                                          true,
                                                          VK_SAMPLE_COUNT_1_BIT,
                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                          ImageSharingMode,
                                                          ImageQueueFamilyIndices,
                                                          pvAllocationGroupIDScene3DPlanetStatic);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fNormalMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fNormalMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fNormalMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fNormalMapImage.ImageView');

  fBlendMapImage:=TpvScene3DRendererImage2D.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fHeightMapResolution,
                                                   fPlanet.fHeightMapResolution,
                                                   VK_FORMAT_R8G8B8A8_SNORM,
                                                   true,
                                                   VK_SAMPLE_COUNT_1_BIT,
                                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                   ImageSharingMode,
                                                   ImageQueueFamilyIndices,
                                                   pvAllocationGroupIDScene3DPlanetStatic);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fBlendMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fBlendMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fBlendMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fBlendMapImage.ImageView');

  fGrassMapImage:=TpvScene3DRendererImage2D.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fGrassMapResolution,
                                                   fPlanet.fGrassMapResolution,
                                                   VK_FORMAT_R32_SFLOAT,
                                                   true,
                                                   VK_SAMPLE_COUNT_1_BIT,
                                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                   ImageSharingMode,
                                                   ImageQueueFamilyIndices,
                                                   pvAllocationGroupIDScene3DPlanetStatic);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fGrassMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fGrassMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fGrassMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fGrassMapImage.ImageView');

  fWaterMapImage:=TpvScene3DRendererImage2D.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fWaterMapResolution,
                                                   fPlanet.fWaterMapResolution,
                                                   VK_FORMAT_R32_SFLOAT,
                                                   true,
                                                   VK_SAMPLE_COUNT_1_BIT,
                                                   VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                   ImageSharingMode,
                                                   ImageQueueFamilyIndices,
                                                   pvAllocationGroupIDScene3DPlanetStatic);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fWaterMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fWaterMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fWaterMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fWaterMapImage.ImageView');

  fWaterVisibilityBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                 (((fPlanet.fTileMapResolution*fPlanet.fTileMapResolution)+31) shr 5)*SizeOf(TpvUInt32),
                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                 fPlanet.fGlobalBufferSharingMode,
                                                 fPlanet.fGlobalBufferQueueFamilyIndices,
                                                 0,
                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 [],
                                                 0,
                                                 pvAllocationGroupIDScene3DPlanetStatic
                                                );
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fWaterVisibilityBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fWaterVisibilityBuffer');                                              

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
                                               [TpvVulkanBufferFlag.PersistentMappedIfPossibe],
                                               0,
                                               pvAllocationGroupIDScene3DPlanetStatic
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
                                                       [TpvVulkanBufferFlag.PersistentMappedIfPossibe],
                                                       0,
                                                       pvAllocationGroupIDScene3DPlanetStatic
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
                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_CACHED_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 [TpvVulkanBufferFlag.PersistentMappedIfPossibe],
                                                 0,
                                                 pvAllocationGroupIDScene3DPlanetStatic
                                                );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTileDirtyQueueBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.fTileDirtyQueueBuffer['+IntToStr(fInFlightFrameIndex)+']');
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
                                                         [],
                                                         0,
                                                         pvAllocationGroupIDScene3DPlanetStatic
                                                        );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTiledMeshBoundingBoxesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.BoundingBoxesBuffer['+IntToStr(fInFlightFrameIndex)+']');

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
                                                           [],
                                                           0,
                                                           pvAllocationGroupIDScene3DPlanetStatic
                                                          );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTiledMeshBoundingSpheresBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.BoundingSpheresBuffer['+IntToStr(fInFlightFrameIndex)+']');

   fTiledVisualMeshIndexGroupsBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                             fPlanet.fTiledVisualMeshIndexGroups.Count*SizeOf(TTiledMeshIndexGroup),
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
                                                             [],
                                                             0,
                                                             pvAllocationGroupIDScene3DPlanetStatic
                                                            );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTiledVisualMeshIndexGroupsBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.MeshIndexGroupsBuffer['+IntToStr(fInFlightFrameIndex)+']');

  end;

  begin

   // All only-visual buffers doesn't need to be accessible from the CPU, just from the GPU itself
    
   if fInFlightFrameIndex<0 then begin

    fVisualMeshVertexBuffers[0]:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                        (fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution)*SizeOf(TpvScene3DPlanet.TMeshVertex),
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                        TpvScene3D(fPlanet.fScene3D).AccelerationStructureInputBufferUsageFlags or
                                                        IfThen(TpvScene3D(fPlanet.fScene3D).UseBufferDeviceAddress,TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),0),
                                                        fPlanet.fGlobalBufferSharingMode,
                                                        fPlanet.fGlobalBufferQueueFamilyIndices,
                                                        0,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                        0,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        [],
                                                        0,
                                                        pvAllocationGroupIDScene3DPlanetStatic
                                                       );
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshVertexBuffers[0].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.VisualMeshVertexBuffer['+IntToStr(fInFlightFrameIndex)+'][0]');

    fVisualMeshVertexBuffers[1]:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                        (fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution)*SizeOf(TpvScene3DPlanet.TMeshVertex),
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TpvScene3D(fPlanet.fScene3D).AccelerationStructureInputBufferUsageFlags,
                                                        fPlanet.fGlobalBufferSharingMode,
                                                        fPlanet.fGlobalBufferQueueFamilyIndices,
                                                        0,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                        0,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        [],
                                                        0,
                                                        pvAllocationGroupIDScene3DPlanetStatic
                                                       );
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshVertexBuffers[1].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.VisualMeshVertexBuffer['+IntToStr(fInFlightFrameIndex)+'][1]');

    fVisualMeshDistanceBuffers[0]:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                          (fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution)*SizeOf(TpvScene3DPlanet.TMeshDistance),
                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                          TpvScene3D(fPlanet.fScene3D).AccelerationStructureInputBufferUsageFlags or
                                                          IfThen(TpvScene3D(fPlanet.fScene3D).UseBufferDeviceAddress,TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),0),
                                                          fPlanet.fGlobalBufferSharingMode,
                                                          fPlanet.fGlobalBufferQueueFamilyIndices,
                                                          0,
                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                          0,
                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                          0,
                                                          0,
                                                          0,
                                                          0,
                                                          [],
                                                          0,
                                                          pvAllocationGroupIDScene3DPlanetStatic
                                                         );
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshDistanceBuffers[0].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.VisualMeshDistanceBuffer['+IntToStr(fInFlightFrameIndex)+'][0]');

    fVisualMeshDistanceBuffers[1]:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                          (fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution)*SizeOf(TpvScene3DPlanet.TMeshDistance),
                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TpvScene3D(fPlanet.fScene3D).AccelerationStructureInputBufferUsageFlags,
                                                          fPlanet.fGlobalBufferSharingMode,
                                                          fPlanet.fGlobalBufferQueueFamilyIndices,
                                                          0,
                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                          0,
                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                          0,
                                                          0,
                                                          0,
                                                          0,
                                                          [],
                                                          0,
                                                          pvAllocationGroupIDScene3DPlanetStatic
                                                         );
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshDistanceBuffers[1].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.VisualMeshDistanceBuffer['+IntToStr(fInFlightFrameIndex)+'][1]');

    fVisualMeshVertexBufferCopies:=TVisualMeshVertexBufferCopies.Create;

    fVisualMeshDistanceBufferCopies:=TVisualMeshVertexBufferCopies.Create;

   end;

   if fInFlightFrameIndex<0 then begin

    fVisualMeshIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fCountVisualMeshIndices*SizeOf(TpvUInt32),
                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TpvScene3D(fPlanet.fScene3D).AccelerationStructureInputBufferUsageFlags,
                                                   fPlanet.fGlobalBufferSharingMode,
                                                   fPlanet.fGlobalBufferQueueFamilyIndices,
                                                   0,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                   0,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   [],
                                                   0,
                                                   pvAllocationGroupIDScene3DPlanetStatic
                                                  );
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.VisualMeshIndexBuffer['+IntToStr(fInFlightFrameIndex)+']');

   end;
   
  end;

  if fInFlightFrameIndex<0 then begin

   fPhysicsMeshVertexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                    fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fPhysicsTileResolution*fPlanet.fPhysicsTileResolution*2*SizeOf(TpvVector4),
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                    fPlanet.fGlobalBufferSharingMode,
                                                    fPlanet.fGlobalBufferQueueFamilyIndices,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_CACHED_BIT),
                                                    0,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    [TpvVulkanBufferFlag.PersistentMappedIfPossibe],
                                                    0,
                                                    pvAllocationGroupIDScene3DPlanetStatic
                                                   );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsMeshVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.PhysicsMeshVertexBuffer['+IntToStr(fInFlightFrameIndex)+']');

   fPhysicsMeshIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fCountPhysicsMeshIndices*SizeOf(TpvUInt32),
                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                   fPlanet.fGlobalBufferSharingMode,
                                                   fPlanet.fGlobalBufferQueueFamilyIndices,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_CACHED_BIT),
                                                   0,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   [],
                                                   0,
                                                   pvAllocationGroupIDScene3DPlanetStatic
                                                  );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsMeshIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.PhysicsMeshIndexBuffer['+IntToStr(fInFlightFrameIndex)+']');

  end;

  if fInFlightFrameIndex<0 then begin

   fRayIntersectionResultBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                        SizeOf(TpvVector4),
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                        fPlanet.fGlobalBufferSharingMode,
                                                        fPlanet.fGlobalBufferQueueFamilyIndices,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_CACHED_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                        0,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        [TpvVulkanBufferFlag.PersistentMappedIfPossibe],
                                                        0,
                                                        pvAllocationGroupIDScene3DPlanetStatic
                                                       );
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fRayIntersectionResultBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.RayIntersectionResultBuffer['+IntToStr(fInFlightFrameIndex)+']');

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

 fModifyGrassMapActive:=false;

 fModifyGrassMapBorderRadius:=0.0;

 fModifyGrassMapFactor:=0.0;

 fModifyWaterMapActive:=false;

 fModifyWaterMapBorderRadius:=0.0;

 fModifyWaterMapFactor:=0.0;

 fWireframeActive:=false;

 fDisplacementMappingActive:=false;

 fParallaxMappingActive:=false;

end;

destructor TpvScene3DPlanet.TData.Destroy;
begin

 fHeightMap:=nil;

 FreeAndNil(fHeightMapImage);

 FreeAndNil(fNormalMapImage);

 FreeAndNil(fBlendMapImage);

 FreeAndNil(fGrassMapImage);

 FreeAndNil(fWaterMapImage);

 FreeAndNil(fWaterVisibilityBuffer);

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

 FreeAndNil(fTiledVisualMeshIndexGroupsBuffer);

 FreeAndNil(fVisualMeshVertexBuffers[0]);
 FreeAndNil(fVisualMeshVertexBuffers[1]);

 FreeAndNil(fVisualMeshDistanceBuffers[0]);
 FreeAndNil(fVisualMeshDistanceBuffers[1]);

 FreeAndNil(fVisualMeshVertexBufferCopies);

 FreeAndNil(fVisualMeshDistanceBufferCopies);

 FreeAndNil(fVisualMeshIndexBuffer);

 FreeAndNil(fPhysicsMeshVertexBuffer);

 FreeAndNil(fPhysicsMeshIndexBuffer);

 FreeAndNil(fRayIntersectionResultBuffer);

 FreeAndNil(fSelectedRegionProperty);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TData.AcquireOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..4] of TVkImageMemoryBarrier;
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
                                                        fBlendMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

    ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(0,
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                         fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                         fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                         fGrassMapImage.VulkanImage.Handle,
                                                         ImageSubresourceRange); 

    ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(0,
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                         VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                         fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                         fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                         fWaterMapImage.VulkanImage.Handle,
                                                         ImageSubresourceRange); 

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].AcquireOnUniversalQueue',[0.5,0.25,0.25,1.0]);
    
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or
                                     IfThen(TpvScene3D(fPlanet.fScene3D).MeshShaderSupport,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_MESH_BIT_EXT) or
                                            TVkShaderStageFlags(VK_SHADER_STAGE_TASK_BIT_EXT),
                                            0) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     5,@ImageMemoryBarriers[0]);

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

  end;

  fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.AcquiredOnUniversalQueue;

 end;

end;

procedure TpvScene3DPlanet.TData.ReleaseOnUniversalQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..4] of TVkImageMemoryBarrier;
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
                                                        fBlendMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);      

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fGrassMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fWaterMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].ReleaseOnUniversalQueue',[0.5,0.25,0.25,1.0]);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or
                                     IfThen(TpvScene3D(fPlanet.fScene3D).MeshShaderSupport,
                                            TVkShaderStageFlags(VK_SHADER_STAGE_MESH_BIT_EXT) or
                                            TVkShaderStageFlags(VK_SHADER_STAGE_TASK_BIT_EXT),
                                            0) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or 
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     5,@ImageMemoryBarriers[0]);    

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);       

  end;

  fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.ReleasedOnUniversalQueue;

 end;

end;

procedure TpvScene3DPlanet.TData.AcquireOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..4] of TVkImageMemoryBarrier;
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
                                                        fBlendMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fGrassMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange); 

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(0,
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fWaterMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange); 

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].AcquireOnComputeQueue',[0.5,0.25,0.25,1.0]);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     5,@ImageMemoryBarriers[0]);    

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);  

  end;

  fOwnershipHolderState:=TpvScene3DPlanet.TData.TOwnershipHolderState.AcquiredOnComputeQueue;

 end;

end;

procedure TpvScene3DPlanet.TData.ReleaseOnComputeQueue(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..4] of TVkImageMemoryBarrier;
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
                                                        fBlendMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);     

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fGrassMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);                

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        0,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        fPlanet.fVulkanDevice.ComputeQueueFamilyIndex,
                                                        fPlanet.fVulkanDevice.UniversalQueueFamilyIndex,
                                                        fWaterMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);                

   fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].ReleaseOnComputeQueue',[0.5,0.25,0.25,1.0]);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     5,@ImageMemoryBarriers[0]);   

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
                                            const aInFlightFrameData:TData;
                                            const aTransferHeightMap:Boolean;
                                            const aTransferGrass:Boolean;
                                            const aTransferWater:Boolean);
var MipMapIndex,CountImageMemoryBarriers:TpvSizeInt;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..9] of TVkImageMemoryBarrier;
    ImageCopies:array[0..31] of TVkImageCopy;
    ImageCopy:PVkImageCopy;
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

   CountImageMemoryBarriers:=0;

   if aTransferHeightMap then begin

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
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
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
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
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                fBlendMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
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
   inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
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
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                aInFlightFrameData.fBlendMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

   end;

   if aTransferGrass then begin

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                fGrassMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                aInFlightFrameData.fGrassMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

   end;

   if aTransferWater then begin

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                fWaterMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                aInFlightFrameData.fWaterMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

   end;

   if CountImageMemoryBarriers>0 then begin
    aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                      TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                      0,
                                      0,nil,
                                      0,nil,
                                      CountImageMemoryBarriers,@ImageMemoryBarriers[0]);
   end;

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

   if aTransferHeightMap then begin

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

    aCommandBuffer.CmdCopyImage(fBlendMapImage.VulkanImage.Handle,
                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                aInFlightFrameData.fBlendMapImage.VulkanImage.Handle,
                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                1,@ImageCopies[0]);

   end;

   if aTransferGrass then begin
    aCommandBuffer.CmdCopyImage(fGrassMapImage.VulkanImage.Handle,
                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                aInFlightFrameData.fGrassMapImage.VulkanImage.Handle,
                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                1,@ImageCopies[0]);
   end;

   if aTransferWater then begin
    aCommandBuffer.CmdCopyImage(fWaterMapImage.VulkanImage.Handle,
                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                aInFlightFrameData.fWaterMapImage.VulkanImage.Handle,
                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                1,@ImageCopies[0]);
   end;

  end;

  //////////////////////////// 

  begin

   CountImageMemoryBarriers:=0;

   if aTransferHeightMap then begin

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
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

    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
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
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                fBlendMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
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
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
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
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                aInFlightFrameData.fBlendMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

   end;

   if aTransferGrass then begin

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                fGrassMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                aInFlightFrameData.fGrassMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

   end;

   if aTransferWater then begin

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                fWaterMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

    ImageMemoryBarriers[CountImageMemoryBarriers]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                VK_QUEUE_FAMILY_IGNORED,
                                                                                aInFlightFrameData.fWaterMapImage.VulkanImage.Handle,
                                                                                ImageSubresourceRange);
    inc(CountImageMemoryBarriers);

   end;

   if CountImageMemoryBarriers>0 then begin

    aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                      TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                      0,
                                      0,
                                      nil,
                                      0,nil,
                                      CountImageMemoryBarriers,@ImageMemoryBarriers[0]);

   end;

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
 fVisualMeshVertexBufferRenderIndex:=aData.fVisualMeshVertexBufferRenderIndex;
end;

{ TpvScene3DPlanet.TGrassMapInitialization }

constructor TpvScene3DPlanet.TGrassMapInitialization.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grassmap_initialization_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TGrassMapInitialization.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
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

  fVulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TGrassMapInitialization.fPipelineLayout');

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.TGrassMapInitialization.fDescriptorPool');

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fGrassMapImage.VulkanImageView.Handle,
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

  fPushConstants.Dummy:=0;

 end;

end;

destructor TpvScene3DPlanet.TGrassMapInitialization.Destroy;
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

procedure TpvScene3DPlanet.TGrassMapInitialization.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet GrassMapInitialization',[0.25,0.5,0.5,1.0]);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fGrassMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
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

 aCommandBuffer.CmdDispatch((fPlanet.fGrassMapResolution+15) shr 4,
                            (fPlanet.fGrassMapResolution+15) shr 4,
                            1);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fGrassMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                 0,
                                                                                 1,
                                                                                 0,
                                                                                 1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 inc(fPlanet.fData.fGrassMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TGrassMapModification }

constructor TpvScene3DPlanet.TGrassMapModification.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grassmap_modification_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TGrassMapModification.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
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

  fVulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TGrassMapModification.fPipelineLayout');

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.TGrassMapModification.fDescriptorPool');

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fGrassMapImage.VulkanImageView.Handle,
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

  fPushConstants.PositionRadius:=TpvVector4.Create(0.0,0.0,0.0,0.0);
  fPushConstants.InnerRadiusValueMinMax:=TpvVector4.Create(0.0,0.0,0.0,0.0);

 end;

end;

destructor TpvScene3DPlanet.TGrassMapModification.Destroy;
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

procedure TpvScene3DPlanet.TGrassMapModification.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet GrassMapModification',[0.5,0.5,0.5,1.0]);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fGrassMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 fPushConstants.InnerRadiusValueMinMax:=TpvVector4.InlineableCreate(Max(0.0,fPlanet.fData.fSelectedRegion.w-fPlanet.fData.fModifyGrassMapBorderRadius),
                                                                    fPlanet.fData.fModifyGrassMapFactor,
                                                                    0.0,
                                                                    1.0);

 fPushConstants.PositionRadius:=fPlanet.fData.fSelectedRegion;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch((fPlanet.fGrassMapResolution+15) shr 4,
                            (fPlanet.fGrassMapResolution+15) shr 4,
                            1);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fGrassMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 inc(fPlanet.fData.fGrassMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TWaterMapInitialization }

constructor TpvScene3DPlanet.TWaterMapInitialization.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_watermap_initialization_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterMapInitialization.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
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

  fVulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TWaterMapInitialization.fPipelineLayout');

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.TWaterMapInitialization.fDescriptorPool');

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fWaterMapImage.VulkanImageView.Handle,
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

  fPushConstants.Dummy:=0;

 end;

end;

destructor TpvScene3DPlanet.TWaterMapInitialization.Destroy;
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

procedure TpvScene3DPlanet.TWaterMapInitialization.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet WaterMapInitialization',[0.25,0.5,0.5,1.0]);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fWaterMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
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

 aCommandBuffer.CmdDispatch((fPlanet.fWaterMapResolution+15) shr 4,
                            (fPlanet.fWaterMapResolution+15) shr 4,
                            1);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fWaterMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                 0,
                                                                                 1,
                                                                                 0,
                                                                                 1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 inc(fPlanet.fData.fWaterMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TWaterMapModification }

constructor TpvScene3DPlanet.TWaterMapModification.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_watermap_modification_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterMapModification.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
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

  fVulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TWaterMapModification.fPipelineLayout');

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.TWaterMapModification.fDescriptorPool');

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fWaterMapImage.VulkanImageView.Handle,
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

  fPushConstants.PositionRadius:=TpvVector4.Create(0.0,0.0,0.0,0.0);
  fPushConstants.InnerRadiusValueMinMax:=TpvVector4.Create(0.0,0.0,0.0,0.0);

 end;

end;

destructor TpvScene3DPlanet.TWaterMapModification.Destroy;
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

procedure TpvScene3DPlanet.TWaterMapModification.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet WaterMapModification',[0.5,0.5,0.5,1.0]);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fWaterMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                      fPipelineLayout.Handle,
                                      0,
                                      1,
                                      @fDescriptorSet.Handle,
                                      0,
                                      nil);

 fPushConstants.InnerRadiusValueMinMax:=TpvVector4.InlineableCreate(Max(0.0,fPlanet.fData.fSelectedRegion.w-fPlanet.fData.fModifyWaterMapBorderRadius),
                                                                    fPlanet.fData.fModifyWaterMapFactor,
                                                                    0.0,
                                                                    1.0);

 fPushConstants.PositionRadius:=fPlanet.fData.fSelectedRegion;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 aCommandBuffer.CmdDispatch((fPlanet.fWaterMapResolution+15) shr 4,
                            (fPlanet.fWaterMapResolution+15) shr 4,
                            1);

 ImageMemoryBarrier:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                  VK_IMAGE_LAYOUT_GENERAL,
                                                  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  VK_QUEUE_FAMILY_IGNORED,
                                                  fPlanet.fData.fWaterMapImage.VulkanImage.Handle,
                                                  TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                  0,
                                                                                  1,
                                                                                  0,
                                                                                  1));

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);

 inc(fPlanet.fData.fWaterMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.THeightMapDataInitialization }
            
constructor TpvScene3DPlanet.THeightMapDataInitialization.Create(const aPlanet:TpvScene3DPlanet;const aData:TStream);
type TDataSignature=array[0..3] of AnsiChar;
const DataSignature:TDataSignature='HMAP';
var Stream:TStream;
    Signature:TDataSignature;
    Version:TpvUInt32;
    Width:TpvUInt32;
    Height:TpvUInt32;
    InputBottomRadius:TpvFloat;
    InputTopRadius:TpvFloat;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  fData:=TMemoryStream.Create;

  aData.Seek(0,soBeginning);
  fData.CopyFrom(aData,aData.Size);
  fData.Seek(0,soBeginning);

  fData.ReadBuffer(Signature,SizeOf(TDataSignature));
  if Signature<>DataSignature then begin
   raise EpvScene3DPlanet.Create('Invalid height map data signature');
  end;

  fData.ReadBuffer(Version,SizeOf(TpvUInt32));
  if Version<>1 then begin
   raise EpvScene3DPlanet.Create('Invalid height map data version');
  end;

  fData.ReadBuffer(Width,SizeOf(TpvUInt32));
  fData.ReadBuffer(Height,SizeOf(TpvUInt32));
  if Width<>Height then begin
   raise EpvScene3DPlanet.Create('Invalid height map data width and height');
  end;

  fData.ReadBuffer(InputBottomRadius,SizeOf(TpvFloat));
  fData.ReadBuffer(InputTopRadius,SizeOf(TpvFloat));

  fDataBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                      fData.Size-fData.Position,
                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                      [],
                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                      0,
                                      0,
                                      0,
                                      0,
                                      0,
                                      0,
                                      [],
                                      0,
                                      pvAllocationGroupIDScene3DPlanetStatic
                                     );       
  fVulkanDevice.MemoryStaging.Upload(fPlanet.fVulkanComputeQueue,
                                     fPlanet.fVulkanComputeCommandBuffer,
                                     fPlanet.fVulkanComputeFence,
                                     Pointer(TpvPtrUInt(TpvPtrUInt(fData.Memory)+TpvPtrUInt(fData.Position)))^,
                                     fDataBuffer,
                                     0,
                                     fData.Size-fData.Position);

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_heightmap_data_initialization_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.THeightMapDataInitialization.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1, // Dirty map buffer
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(2, // Input data buffer
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
  fDescriptorSet.WriteToDescriptorSet(2,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fDataBuffer.Handle,
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

  fPushConstants.InputBottomRadius:=InputBottomRadius;
  fPushConstants.InputTopRadius:=InputTopRadius;
  fPushConstants.BottomRadius:=fPlanet.BottomRadius;
  fPushConstants.TopRadius:=fPlanet.TopRadius;
  fPushConstants.InputResolution:=Width; // = Height
  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  fPushConstants.TileMapShift:=fPlanet.fTileMapShift;

 end;

end;

destructor TpvScene3DPlanet.THeightMapDataInitialization.Destroy;
begin
 
 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);
 
 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 FreeAndNil(fDataBuffer);

 FreeAndNil(fData);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.THeightMapDataInitialization.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet HeightMapDataInitialization',[0.25,0.5,0.5,1.0]);

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

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fTileDirtyMapBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fDataBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
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

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
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
                                   1,@BufferMemoryBarriers[0],
                                   1,@ImageMemoryBarrier);                                                                                                                                                                                                 

 inc(fPlanet.fData.fHeightMapGeneration);

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

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
var Index:TpvInt32;
    Stream:TStream;
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
                                                  8);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),4);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),4);
  fDescriptorPool.Initialize;

  for Index:=0 to 1 do begin
   if fPhysics and (Index=1) then begin
    fDescriptorSets[Index]:=nil;
   end else begin
    fDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
    if fPhysics then begin
     fDescriptorSets[Index].WriteToDescriptorSet(0,
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
     fDescriptorSets[Index].WriteToDescriptorSet(0,
                                                 0,
                                                 1,
                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                 [],
                                                 [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualMeshVertexBuffers[Index].Handle,
                                                                                 0,
                                                                                 VK_WHOLE_SIZE)],
                                                 [],
                                                 false);
    end;
    fDescriptorSets[Index].WriteToDescriptorSet(1,
                                                0,
                                                1,
                                                TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                [],
                                                [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                                                0,
                                                                                VK_WHOLE_SIZE)],
                                                [],
                                                false);
    fDescriptorSets[Index].WriteToDescriptorSet(2,
                                                0,
                                                1,
                                                TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,//TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                               fPlanet.fData.fHeightMapImage.VulkanImageView.Handle,
                                                                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                [],
                                                [],
                                                false);
    fDescriptorSets[Index].WriteToDescriptorSet(3,
                                                0,
                                                1,
                                                TVkDescriptorType(VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE),
                                                [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,//TpvScene3D(fPlanet.fScene3D).GeneralComputeSampler.Handle,
                                                                               fPlanet.fData.fNormalMapImage.VulkanImageView.Handle,
                                                                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                [],
                                                [],
                                                false);
    fDescriptorSets[Index].Flush;
   end;
  end;

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

 FreeAndNil(fDescriptorSets[0]);
 FreeAndNil(fDescriptorSets[1]);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TMeshVertexGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..2] of TVkBufferMemoryBarrier;
    ImageMemoryBarriers:array[0..1] of TVkImageMemoryBarrier;
    BufferCopy:TVkBufferCopy;
begin

 if fPhysics then begin
  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet PhysicsMeshVertexGeneration',[0.5,0.75,0.75,1.0]);
 end else begin
  fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet VisualMeshVertexGeneration',[0.75,0.5,0.75,1.0]);
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

 if fPhysics then begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsMeshVertexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    2,@ImageMemoryBarriers[0]);

 end else begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,
                                    0,nil,
                                    3,@BufferMemoryBarriers[0],
                                    2,@ImageMemoryBarriers[0]);

  if fPlanet.fData.fVisualMeshVertexBufferCopies.Count=0 then begin

   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1].Size;

   aCommandBuffer.CmdCopyBuffer(fPlanet.fData.fVisualMeshVertexBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1].Handle,
                                1,
                                @BufferCopy);

  end else begin

   aCommandBuffer.CmdCopyBuffer(fPlanet.fData.fVisualMeshVertexBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1].Handle,
                                fPlanet.fData.fVisualMeshVertexBufferCopies.Count,
                                @fPlanet.fData.fVisualMeshVertexBufferCopies.ItemArray[0]);

  end;

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    0,nil);

 end;

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 if fPhysics then begin

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fDescriptorSets[0].Handle,
                                       0,
                                       nil);

 end else begin

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fDescriptorSets[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                       0,
                                       nil);

 end;

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
                                                         fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarriers[0],
                                    0,nil);

{ fPlanet.fData.fVisualMeshVertexBufferNextRenderIndex:=fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1;

  fPlanet.fData.fVisualMeshVertexBufferUpdateIndex:=(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1;}

 end;

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TMeshDistanceGeneration }

constructor TpvScene3DPlanet.TMeshDistanceGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Index:TpvInt32;
    Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=fPlanet.fVulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_tiled_neighbour_distance_map_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TMeshDistanceGeneration.fComputeShaderModule');

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
  fDescriptorSetLayout.AddBinding(2, // Distances
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
                                                  8);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),5);
  fDescriptorPool.Initialize;

  for Index:=0 to 1 do begin
   fDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
   fDescriptorSets[Index].WriteToDescriptorSet(0,
                                               0,
                                               1,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                               [],
                                               [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualMeshVertexBuffers[Index].Handle,
                                                                               0,
                                                                               VK_WHOLE_SIZE)],
                                               [],
                                               false);
   fDescriptorSets[Index].WriteToDescriptorSet(1,
                                               0,
                                               1,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                               [],
                                               [TVkDescriptorBufferInfo.Create(fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                                               0,
                                                                               VK_WHOLE_SIZE)],
                                               [],
                                               false);
   fDescriptorSets[Index].WriteToDescriptorSet(2,
                                               0,
                                               1,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                               [],
                                               [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualMeshDistanceBuffers[Index].Handle,
                                                                               0,
                                                                               VK_WHOLE_SIZE)],
                                               [],
                                               false);
   fDescriptorSets[Index].Flush;
  end;

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
  fPushConstants.TileResolution:=fPlanet.fVisualTileResolution;

 end;

end;

destructor TpvScene3DPlanet.TMeshDistanceGeneration.Destroy;
begin

 FreeAndNil(fPipeline);

 FreeAndNil(fDescriptorSets[0]);
 FreeAndNil(fDescriptorSets[1]);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TMeshDistanceGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarriers:array[0..2] of TVkBufferMemoryBarrier;
    BufferCopy:TVkBufferCopy;
begin

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'Planet VisualMeshDistanceGeneration',[0.75,0.5,0.75,1.0]);

 begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshDistanceBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshDistanceBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fTileDirtyQueueBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    0,
                                    0,nil,
                                    3,@BufferMemoryBarriers[0],
                                    0,nil);

  if fPlanet.fData.fVisualMeshDistanceBufferCopies.Count=0 then begin

   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=fPlanet.fData.fVisualMeshDistanceBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1].Size;

   aCommandBuffer.CmdCopyBuffer(fPlanet.fData.fVisualMeshDistanceBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                fPlanet.fData.fVisualMeshDistanceBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1].Handle,
                                1,
                                @BufferCopy);

  end else begin

   aCommandBuffer.CmdCopyBuffer(fPlanet.fData.fVisualMeshDistanceBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                fPlanet.fData.fVisualMeshDistanceBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1].Handle,
                                fPlanet.fData.fVisualMeshDistanceBufferCopies.Count,
                                @fPlanet.fData.fVisualMeshDistanceBufferCopies.ItemArray[0]);

  end;

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshDistanceBuffers[(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshDistanceBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    2,@BufferMemoryBarriers[0],
                                    0,nil);

 end;

 aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

 begin

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                       fPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fDescriptorSets[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                       0,
                                       nil);

 end;

 aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 0,
                                 SizeOf(TPushConstants),
                                 @fPushConstants);

 if fVulkanDevice.PhysicalDevice.RenderDocDetected then begin

  if fPlanet.fData.fCountDirtyTiles>0 then begin

   begin
    aCommandBuffer.CmdDispatch(((fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution)+255) shr 8,
                               fPlanet.fData.fCountDirtyTiles,
                               1);
   end;

  end;

 end else begin

  aCommandBuffer.CmdDispatchIndirect(fPlanet.fData.fTileDirtyQueueBuffer.Handle,0);

 end;

 begin

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualMeshDistanceBuffers[fPlanet.fData.fVisualMeshVertexBufferUpdateIndex].Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarriers[0],
                                    0,nil);

{ fPlanet.fData.fVisualMeshVertexBufferNextRenderIndex:=fPlanet.fData.fVisualMeshVertexBufferUpdateIndex and 1;

  fPlanet.fData.fVisualMeshVertexBufferUpdateIndex:=(fPlanet.fData.fVisualMeshVertexBufferUpdateIndex+1) and 1;}

 end;

 fPlanet.fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);

end;

{ TpvScene3DPlanet.TRaytracingTile }

constructor TpvScene3DPlanet.TRaytracingTile.Create(const aPlanet:TpvScene3DPlanet;const aTileIndex:TpvSizeInt);
begin
 inherited Create;

 fPlanet:=aPlanet;

 fTileIndex:=aTileIndex;

 fBLASGeometry:=nil;

 fBLAS:=nil;
 
 fBLASBuffer:=nil;

 fBLASInstance:=nil;

 fGeneration:=High(TpvUInt64);

 FillChar(fNewGenerations,SizeOf(fNewGenerations),#$ff);

 fMustUpdate:=false;

end;

destructor TpvScene3DPlanet.TRaytracingTile.Destroy;
begin
 
 FreeAndNil(fBLASInstance);

 FreeAndNil(fBLAS);

 FreeAndNil(fBLASBuffer);

 FreeAndNil(fBLASGeometry);

 inherited Destroy;

end;

function TpvScene3DPlanet.TRaytracingTile.CheckAndUpdateGeneration(const aInFlightFrameIndex:TpvSizeInt):Boolean;
var NewGeneration:TpvUInt64;
begin
 NewGeneration:=fNewGenerations[aInFlightFrameIndex];
 if NewGeneration<>High(TpvUInt64) then begin
  fNewGenerations[aInFlightFrameIndex]:=High(TpvUInt64);
  if fGeneration<>NewGeneration then begin
   fGeneration:=NewGeneration;
   result:=true;
  end else begin
   result:=false;
  end;
 end else begin
  result:=false;
 end;
end;

function TpvScene3DPlanet.TRaytracingTile.Update(const aInFlightFrameIndex:TpvSizeInt):Boolean;
var MustBeUpdated:Boolean;
begin

 result:=false;

 MustBeUpdated:=false;

 if not (assigned(fBLASGeometry) and assigned(fBLAS) and assigned(fBLASBuffer) and assigned(fBLASInstance)) then begin

  MustBeUpdated:=true;

  if not assigned(fBLASGeometry) then begin

   fBLASGeometry:=TpvRaytracingBottomLevelAccelerationStructureGeometry.Create(fPlanet.fVulkanDevice);
   fBLASGeometry.AddTriangles(fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshVertexBufferRenderIndex and 1],
                              0,
                              fPlanet.fTileMapResolution*fPlanet.fTileMapResolution*fPlanet.fVisualTileResolution*fPlanet.fVisualTileResolution,
                              SizeOf(TpvScene3DPlanet.TMeshVertex),
                              fPlanet.fData.fVisualMeshIndexBuffer,
                              fPlanet.fTiledVisualMeshIndexGroups[fTileIndex].FirstIndex*SizeOf(TVkUInt32),
                              fPlanet.fTiledVisualMeshIndexGroups[fTileIndex].CountIndices,
                              true,
                              nil,
                              0);

  end;

  if not assigned(fBLAS) then begin
   fBLAS:=TpvRaytracingBottomLevelAccelerationStructure.Create(fPlanet.fVulkanDevice,
                                                               fBLASGeometry,
                                                               TVkBuildAccelerationStructureFlagsKHR(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR){ or
                                                               TVkBuildAccelerationStructureFlagsKHR(VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR)} {or
                                                               TVkBuildAccelerationStructureFlagsKHR(VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR)},
                                                               true);
  end;

  if (not assigned(fBLASBuffer)) or
     (fBLASBuffer.Size<fBLAS.AccelerationStructureSize) then begin

   fBLAS.Finalize;

   FreeAndNil(fBLASBuffer);

   fBLASBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                       fBLAS.AccelerationStructureSize,
                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT),
                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                       [],
                                       0,
                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       [],
                                       0,
                                       pvAllocationGroupIDScene3DRaytracing);

   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fBLASBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanPlanetBLASBuffer');

   fBLAS.Initialize(fBLASBuffer,
                    0);

  end;

  if not assigned(fBLASInstance) then begin

   fBLASInstance:=TpvRaytracingBottomLevelAccelerationStructureInstance.Create(fPlanet.fVulkanDevice,
                                                                               TpvMatrix4x4.Identity,
                                                                               0,
                                                                               $ff,
                                                                               0,
                                                                               0,
                                                                               fBLAS);

  end;

 end;

 if assigned(fBLASGeometry) then begin
  if fBLASGeometry.Geometries.ItemArray[0].geometry.triangles.vertexData.deviceAddress<>fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshVertexBufferRenderIndex and 1].DeviceAddress then begin
   fBLASGeometry.Geometries.ItemArray[0].geometry.triangles.vertexData.deviceAddress:=fPlanet.fData.fVisualMeshVertexBuffers[fPlanet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshVertexBufferRenderIndex and 1].DeviceAddress;
   //fBLAS.Update(fBLASGeometry,true);
  end;
 end;

 fBLASScratchSize:=Max(1,Max(fBLAS.BuildSizesInfo.buildScratchSize,fBLAS.BuildSizesInfo.updateScratchSize));

 fBLASInstance.Transform:=fPlanet.fData.ModelMatrix;

 if CheckAndUpdateGeneration(aInFlightFrameIndex) then begin
  MustBeUpdated:=true;
 end;

 fMustUpdate:=MustBeUpdated;


end;

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

{ TpvScene3DPlanet.TCullPass }

constructor TpvScene3DPlanet.TCullPass.Create(const aRenderer:TObject;const aRendererInstance:TObject;const aScene3D:TObject;const aCullRenderPass:TpvScene3DRendererCullRenderPass;const aPass:TpvSizeInt);
var Stream:TStream;
    InFlightFrameIndex:TpvSizeInt;
begin

 inherited Create;

 fRenderer:=aRenderer;

 fRendererInstance:=aRendererInstance;

 fScene3D:=aScene3D;

 fCullRenderPass:=aCullRenderPass;

 fPass:=aPass;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 if assigned(fVulkanDevice) then begin

  case fPass of
   0:begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_cull_pass0_comp.spv');
   end;
   1:begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_cull_pass1_comp.spv');
   end;
   else begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_cull_simple_comp.spv');
   end;
  end;
  try
   fPlanetComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fPlanetComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TCullPass.fComputeShaderModule');

  fPlanetComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fPlanetComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0, // Views
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  case fPass of
   1:begin
    fDescriptorSetLayout.AddBinding(1, // Depth buffer
                                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                    1,
                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                    [],
                                    0);
   end;
   else begin
   end;
  end;
  fDescriptorSetLayout.Initialize;

  fPlanetPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPlanetPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPlanetPushConstants));
  fPlanetPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPlanetPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetCullDescriptorSetLayout);
  fPlanetPipelineLayout.Initialize;

  fPlanetPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                                   pvApplication.VulkanPipelineCache,
                                                   TVkPipelineCreateFlags(0),
                                                   fPlanetComputeShaderStage,
                                                   fPlanetPipelineLayout,
                                                   nil,
                                                   0);

  if (fPass=1) and not TpvScene3D(fScene3D).MeshShaderSupport then begin

   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_task_comp.spv');
   try
    fGrassTaskComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
   finally
    FreeAndNil(Stream);
   end;

   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_mesh_comp.spv');
   try
    fGrassMeshComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
   finally
    FreeAndNil(Stream);
   end;

   fVulkanDevice.DebugUtils.SetObjectName(fGrassTaskComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DGrass.TCullPass.fGrassTaskComputeShaderModule');

   fVulkanDevice.DebugUtils.SetObjectName(fGrassMeshComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DGrass.TCullPass.fGrassMeshComputeShaderModule');

   fGrassTaskComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fGrassTaskComputeShaderModule,'main');

   fGrassMeshComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fGrassMeshComputeShaderModule,'main');

   fGrassPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
   fGrassPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TGrassPushConstants));
   fGrassPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
   fGrassPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetGrassCullAndMeshGenerationDescriptorSetLayout);
   fGrassPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetDescriptorSetLayout); // Per planet descriptor set
   fGrassPipelineLayout.Initialize;

   fGrassTaskPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                                       pvApplication.VulkanPipelineCache,
                                                       TVkPipelineCreateFlags(0),
                                                       fGrassTaskComputeShaderStage,
                                                       fGrassPipelineLayout,
                                                       nil,
                                                       0);

   fGrassMeshPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                                       pvApplication.VulkanPipelineCache,
                                                       TVkPipelineCreateFlags(0),
                                                       fGrassMeshComputeShaderStage,
                                                       fGrassPipelineLayout,
                                                       nil,
                                                       0);

  end else begin

   fGrassTaskComputeShaderModule:=nil;

   fGrassMeshComputeShaderModule:=nil;

   fGrassTaskComputeShaderStage:=nil;

   fGrassMeshComputeShaderStage:=nil;

   fGrassPipelineLayout:=nil;

   fGrassTaskPipeline:=nil;

   fGrassMeshPipeline:=nil;

  end;

 end;

end;

destructor TpvScene3DPlanet.TCullPass.Destroy;
begin

 FreeAndNil(fGrassMeshPipeline);

 FreeAndNil(fGrassTaskPipeline);

 FreeAndNil(fGrassPipelineLayout);

 FreeAndNil(fPlanetPipeline);

 FreeAndNil(fPlanetPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fGrassMeshComputeShaderStage);

 FreeAndNil(fGrassTaskComputeShaderStage);

 FreeAndNil(fGrassMeshComputeShaderModule);

 FreeAndNil(fGrassTaskComputeShaderModule);

 FreeAndNil(fPlanetComputeShaderStage);

 FreeAndNil(fPlanetComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TCullPass.AllocateResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 TpvScene3D(fScene3D).CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),TpvScene3D(fScene3D).CountInFlightFrames);
 case fPass of
  1:begin
   fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),TpvScene3D(fScene3D).CountInFlightFrames);
  end;
  else begin
  end;
 end;
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
  case fPass of
   1:begin
    case fCullRenderPass of
     TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
      fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                               0,
                                                               1,
                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                               [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRenderer).MipMapMaxFilterSampler.Handle,
                                                                                              TpvScene3DRendererInstance(fRendererInstance).CullDepthPyramidMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                               [],
                                                               [],
                                                               false
                                                              );
     end;
     else begin
      if TpvScene3DRendererInstance(fRendererInstance).ZNear<0.0 then begin
       fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                0,
                                                                1,
                                                                TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRenderer).MipMapMinFilterSampler.Handle,
                                                                                               TpvScene3DRendererInstance(fRendererInstance).CullDepthPyramidMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                [],
                                                                [],
                                                                false
                                                               );
      end else begin
       fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                0,
                                                                1,
                                                                TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRenderer).MipMapMaxFilterSampler.Handle,
                                                                                               TpvScene3DRendererInstance(fRendererInstance).CullDepthPyramidMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                               VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                [],
                                                                [],
                                                                false
                                                               );
      end;
     end;
    end;
   end;
   else begin
   end;
  end;
  fDescriptorSets[InFlightFrameIndex].Flush;
 end;

end;

procedure TpvScene3DPlanet.TCullPass.ReleaseResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fDescriptorPool);

end;

procedure TpvScene3DPlanet.TCullPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt);
var PlanetIndex,RenderPassIndex,BaseViewIndex,CountViews,CountBufferMemoryBarriers,
    AdditionalViewIndex,CountAdditionalViews,PreviousInFlightFrameIndex:TpvSizeInt;
    Planet:TpvScene3DPlanet;
    First:Boolean;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    RendererInstance:TpvScene3DPlanet.TRendererInstance;
    RendererViewInstance:TpvScene3DPlanet.TRendererViewInstance;
    BufferMemoryBarriers:array[0..5] of TVkBufferMemoryBarrier;
    DstPipelineStageFlags:TVkPipelineStageFlags;
    BufferCopy:TVkBufferCopy;
begin

 PreviousInFlightFrameIndex:=aInFlightFrameIndex-1;
 if PreviousInFlightFrameIndex<0 then begin
  PreviousInFlightFrameIndex:=TpvScene3DRendererInstance(fRendererInstance).Scene3D.CountInFlightFrames-1;
 end;

 InFlightFrameState:=@TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex];

 TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.AcquireRead;
 try

  First:=true;

  for PlanetIndex:=0 to TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Count-1 do begin

   Planet:=TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Items[PlanetIndex];

   if Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin

    {if Planet.fData.fVisible then}begin

     if First then begin

      First:=false;

      aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPlanetPipeline.Handle);

      aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                           fPlanetPipelineLayout.Handle,
                                           0,
                                           1,
                                           @fDescriptorSets[aInFlightFrameIndex].Handle,
                                           0,
                                           nil);

     end;

     case fCullRenderPass of
      TpvScene3DRendererCullRenderPass.FinalView:begin
       RenderPassIndex:=InFlightFrameState^.ViewRenderPassIndex;
       BaseViewIndex:=InFlightFrameState^.FinalViewIndex;
       CountViews:=InFlightFrameState^.CountFinalViews;
       AdditionalViewIndex:=0;
       CountAdditionalViews:=0;
      end;
      TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
       RenderPassIndex:=InFlightFrameState^.CascadedShadowMapRenderPassIndex;
       BaseViewIndex:=InFlightFrameState^.CascadedShadowMapViewIndex;
       CountViews:=InFlightFrameState^.CountCascadedShadowMapViews;
       AdditionalViewIndex:=InFlightFrameState^.FinalViewIndex;
       CountAdditionalViews:=InFlightFrameState^.CountFinalViews;
      end;
      else begin
       Assert(false);
       RenderPassIndex:=0;
       BaseViewIndex:=0;
       CountViews:=0;
       AdditionalViewIndex:=0;
       CountAdditionalViews:=0;
      end;
     end;

     begin

      if (BaseViewIndex>=0) and (CountViews>0) then begin

       if Planet.fRendererInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererInstance.TKey.Create(fRendererInstance),
                                                 RendererInstance) and
          Planet.fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fRendererInstance,RenderPassIndex),
                                                     RendererViewInstance) then begin

        begin

         fPlanetPushConstants.ModelMatrix:=Planet.fInFlightFrameDataList[aInFlightFrameIndex].ModelMatrix;
         fPlanetPushConstants.BaseViewIndex:=BaseViewIndex;
         fPlanetPushConstants.CountViews:=CountViews;
         fPlanetPushConstants.AdditionalViewIndex:=AdditionalViewIndex;
         fPlanetPushConstants.CountAdditionalViews:=CountAdditionalViews;
         fPlanetPushConstants.TileMapResolution:=Planet.fTileMapResolution;
         fPlanetPushConstants.TileResolution:=Planet.fVisualTileResolution;
         fPlanetPushConstants.BottomRadius:=Planet.fBottomRadius;
         fPlanetPushConstants.TopRadius:=Planet.fTopRadius;
         fPlanetPushConstants.MinimumLODLevel:=RendererInstance.fMinimumLODLevel;

         begin

          CountBufferMemoryBarriers:=0;

          DstPipelineStageFlags:=0;

          if fPass<=0 then begin

           BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                          VK_QUEUE_FAMILY_IGNORED,
                                                                                          VK_QUEUE_FAMILY_IGNORED,
                                                                                          RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                                                                          0,
                                                                                          VK_WHOLE_SIZE);
           inc(CountBufferMemoryBarriers);

           DstPipelineStageFlags:=DstPipelineStageFlags or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT);

          end;

          case fPass of
           0:begin

            BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           RendererViewInstance.fVulkanVisiblityBuffers[aInFlightFrameIndex].Handle,
                                                                                           0,
                                                                                           VK_WHOLE_SIZE);
            inc(CountBufferMemoryBarriers);

            BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           RendererViewInstance.fVulkanVisiblityBuffers[PreviousInFlightFrameIndex].Handle,
                                                                                           0,
                                                                                           VK_WHOLE_SIZE);
            inc(CountBufferMemoryBarriers);

            BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                           TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                                                                           0,
                                                                                           VK_WHOLE_SIZE);
            inc(CountBufferMemoryBarriers);

            DstPipelineStageFlags:=DstPipelineStageFlags or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);

           end;
           else begin
           end;
          end;

          if CountBufferMemoryBarriers>0 then begin
           aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                             DstPipelineStageFlags,
                                             0,
                                             0,nil,
                                             CountBufferMemoryBarriers,@BufferMemoryBarriers[0],
                                             0,nil);
          end;

         end;

         if fPass<=0 then begin
          aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                       0,
                                       16*SizeOf(TVkUInt32),
                                       0);
         end;

         if fPass=0 then begin

          aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanVisiblityBuffers[aInFlightFrameIndex].Handle,
                                       0,
                                       RendererViewInstance.fVulkanVisiblityBuffers[aInFlightFrameIndex].Size,
                                       0);

          begin

           aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                        0,
                                        SizeOf(TpvUInt32),
                                        ((Planet.fVisualTileResolution*Planet.fVisualTileResolution)+127) shr 7);

           aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                        SizeOf(TpvUInt32),
                                        SizeOf(TpvUInt32),
                                        0);

           aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                        SizeOf(TpvUInt32)+SizeOf(TpvUInt32),
                                        SizeOf(TpvUInt32),
                                        1);

           if RendererViewInstance.fVulkanVisibleTileListBuffer.Size>(SizeOf(TpvUInt32)*3) then begin
            aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                         SizeOf(TpvUInt32)*3,
                                         RendererViewInstance.fVulkanVisibleTileListBuffer.Size-(SizeOf(TpvUInt32)*3),
                                         0);
           end;

          end;

         end;

         begin

          CountBufferMemoryBarriers:=0;

          if fPass<=0 then begin
           BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                          VK_QUEUE_FAMILY_IGNORED,
                                                                                          VK_QUEUE_FAMILY_IGNORED,
                                                                                          RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                                                                          0,
                                                                                          VK_WHOLE_SIZE);
           inc(CountBufferMemoryBarriers);
          end;

          case fPass of
           0:begin

            BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                           TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           RendererViewInstance.fVulkanVisiblityBuffers[aInFlightFrameIndex].Handle,
                                                                                           0,
                                                                                           VK_WHOLE_SIZE);
            inc(CountBufferMemoryBarriers);

            BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                                                                           0,
                                                                                           VK_WHOLE_SIZE);

           end;
           else begin
           end;
          end;

          if CountBufferMemoryBarriers>0 then begin
           aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                             TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                             0,
                                             0,nil,
                                             CountBufferMemoryBarriers,@BufferMemoryBarriers[0],
                                             0,nil);
          end;

         end;

         aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                              fPlanetPipelineLayout.Handle,
                                              1,
                                              1,
                                              @RendererViewInstance.fPlanetCullDescriptorSets[aInFlightFrameIndex].Handle,
                                              0,
                                              nil);

         aCommandBuffer.CmdPushConstants(fPlanetPipelineLayout.Handle,
                                         TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                         0,
                                         SizeOf(TPlanetPushConstants),
                                         @fPlanetPushConstants);

         aCommandBuffer.CmdDispatch(((Planet.fTileMapResolution*Planet.fTileMapResolution)+255) shr 8,
                                    1,
                                    1);

         begin

          CountBufferMemoryBarriers:=0;

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          case fPass of
           0,1:begin
            BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           VK_QUEUE_FAMILY_IGNORED,
                                                                                           RendererViewInstance.fVulkanVisiblityBuffers[aInFlightFrameIndex].Handle,
                                                                                           0,
                                                                                           VK_WHOLE_SIZE);
            inc(CountBufferMemoryBarriers);
           end;
           else begin
           end;
          end;

          if fPass=1 then begin

           BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                          VK_QUEUE_FAMILY_IGNORED,
                                                                                          VK_QUEUE_FAMILY_IGNORED,
                                                                                          RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                                                                          0,
                                                                                          VK_WHOLE_SIZE);
           inc(CountBufferMemoryBarriers);

          end;

          aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                            0,
                                            0,nil,
                                            CountBufferMemoryBarriers,@BufferMemoryBarriers[0],
                                            0,nil);

         end;

        end;

       end;

      end;

     end;

    end;

   end;

  end;

  if assigned(fGrassTaskPipeline) and assigned(fGrassMeshPipeline) and (fPass=1) and (fCullRenderPass=TpvScene3DRendererCullRenderPass.FinalView) then begin

   First:=true;

   for PlanetIndex:=0 to TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Count-1 do begin

    Planet:=TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Items[PlanetIndex];

    if Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin

     {if Planet.fData.fVisible then}begin

      if First then begin

       First:=false;

      end;

      case fCullRenderPass of
       TpvScene3DRendererCullRenderPass.FinalView:begin
        RenderPassIndex:=InFlightFrameState^.ViewRenderPassIndex;
        BaseViewIndex:=InFlightFrameState^.FinalViewIndex;
        CountViews:=InFlightFrameState^.CountFinalViews;
        AdditionalViewIndex:=0;
        CountAdditionalViews:=0;
       end;
       TpvScene3DRendererCullRenderPass.CascadedShadowMap:begin
        RenderPassIndex:=InFlightFrameState^.CascadedShadowMapRenderPassIndex;
        BaseViewIndex:=InFlightFrameState^.CascadedShadowMapViewIndex;
        CountViews:=InFlightFrameState^.CountCascadedShadowMapViews;
        AdditionalViewIndex:=InFlightFrameState^.FinalViewIndex;
        CountAdditionalViews:=InFlightFrameState^.CountFinalViews;
       end;
       else begin
        Assert(false);
        RenderPassIndex:=0;
        BaseViewIndex:=0;
        CountViews:=0;
        AdditionalViewIndex:=0;
        CountAdditionalViews:=0;
       end;
      end;

      begin

       if (BaseViewIndex>=0) and (CountViews>0) then begin

        if Planet.fRendererInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererInstance.TKey.Create(fRendererInstance),
                                                  RendererInstance) and
           Planet.fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fRendererInstance,RenderPassIndex),
                                                      RendererViewInstance) then begin

         fGrassPushConstants.ModelMatrix:=Planet.fInFlightFrameDataList[aInFlightFrameIndex].fModelMatrix;
         fGrassPushConstants.ViewBaseIndex:=BaseViewIndex;
         fGrassPushConstants.CountViews:=CountViews;
         fGrassPushConstants.Time:=Modulo(TpvScene3D(Planet.Scene3D).SceneTimes^[aInFlightFrameIndex],65536.0);
         fGrassPushConstants.CountAllViews:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].CountViews;
         fGrassPushConstants.TileMapResolution:=Planet.fTileMapResolution;
         fGrassPushConstants.TileResolution:=Planet.fVisualTileResolution;
         fGrassPushConstants.MaximumDistance:=Planet.fTopRadius;
         fGrassPushConstants.GrassHeight:=0.125*5.0;//1.25;
         fGrassPushConstants.GrassThickness:=0.01;
         fGrassPushConstants.MaximalCountBladesPerPatch:=8;
         fGrassPushConstants.LOD:=Max(0,IntLog2(Planet.fHeightMapResolution)-IntLog2(Planet.fVisualResolution));
         fGrassPushConstants.FrameIndex:=0;
         fGrassPushConstants.MaximumCountTaskIndices:=Planet.fVisualResolution*Planet.fVisualResolution;
         fGrassPushConstants.MaximumCountVertices:=Planet.fMaxGrassVertices;
         fGrassPushConstants.MaximumCountIndices:=Planet.fMaxGrassIndices;

         begin

          CountBufferMemoryBarriers:=0;

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassTaskIndicesBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassMetaDataBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassVerticesBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDEX_READ_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassIndicesBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                            0,
                                            0,nil,
                                            CountBufferMemoryBarriers,@BufferMemoryBarriers[0],
                                            0,nil);
         end;

         begin

          aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanGrassTaskIndicesBuffer.Handle,
                                       0,
                                       3*SizeOf(TpvUInt32),
                                       0);

          aCommandBuffer.CmdFillBuffer(RendererViewInstance.fVulkanGrassMetaDataBuffer.Handle,
                                       0,
                                       RendererViewInstance.fVulkanGrassMetaDataBuffer.Size,
                                       0);

         end;

         begin

          CountBufferMemoryBarriers:=0;

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassTaskIndicesBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassMetaDataBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                            0,
                                            0,nil,
                                            CountBufferMemoryBarriers,@BufferMemoryBarriers[0],
                                            0,nil);
         end;

         begin

          aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fGrassTaskPipeline.Handle);

          aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                               fGrassPipelineLayout.Handle,
                                               0,
                                               1,
                                               @fDescriptorSets[aInFlightFrameIndex].Handle,
                                               0,
                                               nil);

          aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                               fGrassPipelineLayout.Handle,
                                               1,
                                               1,
                                               @RendererViewInstance.fGrassCullDescriptorSets[Planet.fData.fVisualMeshVertexBufferRenderIndex and 1].Handle,
                                               0,
                                               nil);

          aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                               fGrassPipelineLayout.Handle,
                                               2,
                                               1,
                                               @Planet.fDescriptorSets[aInFlightFrameIndex].Handle,
                                               0,
                                               nil);

          aCommandBuffer.CmdPushConstants(fGrassPipelineLayout.Handle,
                                          TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                          0,
                                          SizeOf(TGrassPushConstants),
                                          @fGrassPushConstants);

          aCommandBuffer.CmdDispatchIndirect(RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,0);

{         aCommandBuffer.CmdDispatch((((Planet.fVisualTileResolution shr 0)*(Planet.fVisualTileResolution shr 0))+127) shr 7,
                                     ((Planet.fTileMapResolution*Planet.fTileMapResolution)+0) shr 0,
                                     1);//}

         end;

         begin

          CountBufferMemoryBarriers:=0;

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassTaskIndicesBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT),
                                            0,
                                            0,nil,
                                            CountBufferMemoryBarriers,@BufferMemoryBarriers[0],
                                            0,nil);

         end;

         begin
          aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fGrassMeshPipeline.Handle);
          aCommandBuffer.CmdDispatchIndirect(RendererViewInstance.fVulkanGrassTaskIndicesBuffer.Handle,0);
         end;

         begin

          CountBufferMemoryBarriers:=0;

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassMetaDataBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassVerticesBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          BufferMemoryBarriers[CountBufferMemoryBarriers]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDEX_READ_BIT),
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         VK_QUEUE_FAMILY_IGNORED,
                                                                                         RendererViewInstance.fVulkanGrassIndicesBuffer.Handle,
                                                                                         0,
                                                                                         VK_WHOLE_SIZE);
          inc(CountBufferMemoryBarriers);

          aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                            0,
                                            0,nil,
                                            CountBufferMemoryBarriers,@BufferMemoryBarriers[0],
                                            0,nil);
         end;

        end;

       end;

      end;

     end;

    end;

   end;

  end;

 finally
  TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.ReleaseRead;
 end;

end;

{ TpvScene3DPlanet.TWaterCullPass }

constructor TpvScene3DPlanet.TWaterCullPass.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin

 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=TpvScene3D(fPlanet.fScene3D).VulkanDevice;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_water_cull_comp.spv');
 try
  fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  FreeAndNil(Stream);
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterCullPass.fComputeShaderModule');
 
 fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');
 
 fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
 
 fDescriptorSetLayout.AddBinding(0, // Height map
                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 [],
                                 0);

 fDescriptorSetLayout.AddBinding(1, // Water map
                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                 [],
                                 0);

 fDescriptorSetLayout.AddBinding(2, // Water visibility buffer
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

 fVulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TWaterCullPass.fPipelineLayout');
 
 fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                            pvApplication.VulkanPipelineCache,
                                            TVkPipelineCreateFlags(0),
                                            fComputeShaderStage,
                                            fPipelineLayout,
                                            nil,
                                            0);
 fVulkanDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DPlanet.TWaterCullPass.fPipeline');

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 1);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,2);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,1);
 fDescriptorPool.Initialize;

 fVulkanDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.TWaterCullPass.fDescriptorPool');

 fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,
                                               fDescriptorSetLayout);

 fDescriptorSet.WriteToDescriptorSet(0,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                     [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                    fPlanet.fData.fHeightMapImage.VulkanImageViews[0].Handle,
                                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                     [],
                                     [],
                                     false);

 fDescriptorSet.WriteToDescriptorSet(1,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                     [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                    fPlanet.fData.fWaterMapImage.VulkanImageView.Handle,
                                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                     [],
                                     [],
                                     false);

 fDescriptorSet.WriteToDescriptorSet(2,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                     [],
                                     [fPlanet.fData.fWaterVisibilityBuffer.DescriptorBufferInfo],
                                     [],
                                     false);

 fDescriptorSet.Flush;

 fVulkanDevice.DebugUtils.SetObjectName(fDescriptorSet.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TWaterCullPass.fDescriptorSet');

end;

destructor TpvScene3DPlanet.TWaterCullPass.Destroy;
begin

 FreeAndNil(fPipeline);

 FreeAndNil(fPipelineLayout);
                                                
 FreeAndNil(fDescriptorSet);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TWaterCullPass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var BufferMemoryBarrier:TVkBufferMemoryBarrier;
begin

 fPushConstants.TileMapResolution:=fPlanet.fTileMapResolution;
 fPushConstants.TileShift:=fPlanet.fTileMapShift;
 fPushConstants.BottomRadius:=fPlanet.fBottomRadius;
 fPushConstants.TopRadius:=fPlanet.fTopRadius;
 fPushConstants.MaxWaterAdditionalWavesHeight:=0.25;

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fWaterVisibilityBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil); 

 aCommandBuffer.CmdFillBuffer(fPlanet.fData.fWaterVisibilityBuffer.Handle,
                              0,
                              fPlanet.fData.fWaterVisibilityBuffer.Size,
                              0);

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fWaterVisibilityBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);                                  

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
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

 aCommandBuffer.CmdDispatch((fPlanet.fVisualResolution*15) shr 4,
                            (fPlanet.fVisualResolution*15) shr 4,
                            1);

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fWaterVisibilityBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE); 

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil);

end;

{ TpvScene3DPlanet.TWaterPrepass }

constructor TpvScene3DPlanet.TWaterPrepass.Create(const aRenderer:TObject;
                                                  const aRendererInstance:TObject;
                                                  const aScene3D:TObject);
var Stream:TStream;
begin

 inherited Create;

 fRenderer:=aRenderer;

 fRendererInstance:=aRendererInstance;

 fScene3D:=aScene3D;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_water_prepass_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;
  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterPrepass.fComputeShaderModule');

  fComputeShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_COMPUTE_BIT,fComputeShaderModule,'main');

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);
  fDescriptorSetLayout.AddBinding(0, // Views
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.AddBinding(1, // Depth buffer
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  1,
                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                  [],
                                  0);
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetWaterPrepassDescriptorSetLayout);
  fPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TWaterPrepass.fPipelineLayout');

  fPipeline:=TpvVulkanComputePipeline.Create(fVulkanDevice,
                                             pvApplication.VulkanPipelineCache,
                                             TVkPipelineCreateFlags(0),
                                             fComputeShaderStage,
                                             fPipelineLayout,
                                             nil,
                                             0);

  fVulkanDevice.DebugUtils.SetObjectName(fPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DPlanet.TWaterPrepass.fPipeline');

 end;

end;

destructor TpvScene3DPlanet.TWaterPrepass.Destroy;
begin

 FreeAndNil(fPipeline);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fComputeShaderStage);

 FreeAndNil(fComputeShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TWaterPrepass.AllocateResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 TpvScene3D(fScene3D).CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,TpvScene3D(fScene3D).CountInFlightFrames*1);
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,TpvScene3D(fScene3D).CountInFlightFrames*1);
 fDescriptorPool.Initialize;

 fVulkanDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.TWaterPrepass.fDescriptorPool');

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
 
  fDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,
                                                                     fDescriptorSetLayout);

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
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                           [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRenderer).MipMapMaxFilterSampler.Handle,
                                                                                          TpvScene3DRendererInstance(fRendererInstance).DepthMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                           [],
                                                           [],
                                                           false
                                                          );

  fDescriptorSets[InFlightFrameIndex].Flush;

  fVulkanDevice.DebugUtils.SetObjectName(fDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TWaterPrepass.fDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

 end;

end;
 
procedure TpvScene3DPlanet.TWaterPrepass.ReleaseResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fDescriptorPool);

end;

procedure TpvScene3DPlanet.TWaterPrepass.Execute(const aCommandBuffer:TpvVulkanCommandBuffer;const aInFlightFrameIndex:TpvSizeInt);
var PlanetIndex,RenderPassIndex,BaseViewIndex,CountViews,AdditionalViewIndex,CountAdditionalViews:TpvSizeInt;
    Planet:TpvScene3DPlanet;
    First:Boolean;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    RendererInstance:TpvScene3DPlanet.TRendererInstance;
    RendererViewInstance:TpvScene3DPlanet.TRendererViewInstance;
    ImageMemoryBarriers:array[0..1] of TVkImageMemoryBarrier;
    DstPipelineStageFlags:TVkPipelineStageFlags;
    DescriptorSets:array[0..1] of TVkDescriptorSet;
    ClearColorValue:TVkClearColorValue;
    ImageSubresourceRange:TVkImageSubresourceRange;
begin

 InFlightFrameState:=@TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex];

 TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.AcquireRead;
 try

  First:=true;

  for PlanetIndex:=0 to TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Count-1 do begin

   Planet:=TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Items[PlanetIndex];

   if Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin

    if First then begin

     First:=false;

     ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          TpvScene3DRendererInstance(fRendererInstance).DepthMipmappedArray2DImage.VulkanImage.Handle,
                                                          TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                          0,
                                                                                          TpvScene3DRendererInstance(fRendererInstance).DepthMipmappedArray2DImage.MipMapLevels,
                                                                                          0,
                                                                                          TpvScene3DRendererInstance(fRendererInstance).DepthMipmappedArray2DImage.Layers));

     aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                       TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                       TVkDependencyFlags(0),
                                       0,nil,
                                       0,nil,
                                       1,@ImageMemoryBarriers[0]);

     aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_COMPUTE,fPipeline.Handle);

     aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                          fPipelineLayout.Handle,
                                          0,
                                          1,
                                          @fDescriptorSets[aInFlightFrameIndex].Handle,
                                          0,
                                          nil);

    end;

    if Planet.fRendererInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererInstance.TKey.Create(fRendererInstance),RendererInstance) and
       Planet.fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fRendererInstance,InFlightFrameState^.ViewRenderPassIndex),RendererViewInstance) then begin

     fPushConstants.ModelMatrix:=Planet.fInFlightFrameDataList[aInFlightFrameIndex].fModelMatrix;
     fPushConstants.ViewBaseIndex:=InFlightFrameState^.FinalViewIndex;
     fPushConstants.CountViews:=InFlightFrameState^.CountFinalViews;
     fPushConstants.Time:=Modulo(TpvScene3D(Planet.Scene3D).SceneTimes^[aInFlightFrameIndex],65536.0);
     fPushConstants.BottomRadius:=Planet.fBottomRadius;
     fPushConstants.TopRadius:=Planet.fTopRadius;

     ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                            0,
                                                            1,
                                                            0,
                                                            RendererViewInstance.fVulkanWaterAccelerationImage.Layers);

     begin

      ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(0,
                                                           TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                           VK_IMAGE_LAYOUT_UNDEFINED,
                                                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                           VK_QUEUE_FAMILY_IGNORED,
                                                           VK_QUEUE_FAMILY_IGNORED,
                                                           RendererViewInstance.fVulkanWaterAccelerationImage.VulkanImage.Handle,
                                                           ImageSubresourceRange);


      aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                        TVkDependencyFlags(0),
                                        0,nil,
                                        0,nil,
                                        1,@ImageMemoryBarriers[0]);
     end;

     begin

      ClearColorValue.uint32[0]:=$bf800000; // -1.0 as float
      ClearColorValue.uint32[1]:=$bf800000; // -1.0 as float
      ClearColorValue.uint32[2]:=$bf800000; // -1.0 as float
      ClearColorValue.uint32[3]:=$bf800000; // -1.0 as float

      aCommandBuffer.CmdClearColorImage(RendererViewInstance.fVulkanWaterAccelerationImage.VulkanImage.Handle,
                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                        @ClearColorValue,
                                        1,@ImageSubresourceRange);

     end;

     begin

      ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                           TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                           VK_IMAGE_LAYOUT_GENERAL,
                                                           VK_QUEUE_FAMILY_IGNORED,
                                                           VK_QUEUE_FAMILY_IGNORED,
                                                           RendererViewInstance.fVulkanWaterAccelerationImage.VulkanImage.Handle,
                                                           ImageSubresourceRange);

      aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                        TVkDependencyFlags(0),
                                        0,nil,
                                        0,nil,
                                        1,@ImageMemoryBarriers[0]);

     end;

     aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                     TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                     0,
                                     SizeOf(TPushConstants),
                                     @fPushConstants);

     DescriptorSets[0]:=RendererViewInstance.fWaterPrepassDescriptorSet.Handle;
     DescriptorSets[1]:=Planet.fDescriptorSets[aInFlightFrameIndex].Handle;

     aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                          fPipelineLayout.Handle,
                                          1,
                                          2,
                                          @DescriptorSets[0],
                                          0,
                                          nil);

     aCommandBuffer.CmdDispatch((RendererViewInstance.fVulkanWaterAccelerationImage.Width+15) shr 4,
                                (RendererViewInstance.fVulkanWaterAccelerationImage.Height+15) shr 4,
                                RendererViewInstance.fVulkanWaterAccelerationImage.Layers);

     ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT),
                                                          VK_IMAGE_LAYOUT_GENERAL,
                                                          VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          RendererViewInstance.fVulkanWaterAccelerationImage.VulkanImage.Handle,
                                                          TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                                                          0,
                                                                                          1,
                                                                                          0,
                                                                                          RendererViewInstance.fVulkanWaterAccelerationImage.Layers));

     aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                       TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT),
                                       TVkDependencyFlags(0),
                                       0,nil,
                                       0,nil,
                                       1,@ImageMemoryBarriers[0]);

    end;

   end;

  end;

 finally
  TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.ReleaseRead;
 end;

end;

{ TpvScene3DPlanet.TRenderPass }

constructor TpvScene3DPlanet.TRenderPass.Create(const aRenderer:TObject;
                                                const aRendererInstance:TObject;
                                                const aScene3D:TObject;
                                                const aMode:TpvScene3DPlanet.TRenderPass.TMode;
                                                const aResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
                                                const aResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource);
var Stream:TStream;
    Kind:TpvUTF8String;
    ShadowKind:TpvUTF8String;
    TopLevelKind:TpvUTF8String;
begin

 inherited Create;

 fRenderer:=aRenderer;

 fRendererInstance:=aRendererInstance;

 fScene3D:=aScene3D;

 fMode:=aMode;

 fResourceCascadedShadowMap:=aResourceCascadedShadowMap;

 fResourceSSAO:=aResourceSSAO;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 if assigned(fVulkanDevice) then begin

  Kind:='';

  case TpvScene3DRenderer(fRenderer).ShadowMode of
   TpvScene3DRendererShadowMode.DPCF,TpvScene3DRendererShadowMode.PCF,TpvScene3DRendererShadowMode.PCSS:begin
    ShadowKind:='pcfpcss_';
   end;
   TpvScene3DRendererShadowMode.MSM:begin
    ShadowKind:='msm_';
   end;
   else begin
    ShadowKind:='';
   end;
  end;

  if TpvScene3D(fScene3D).RaytracingActive then begin
   TopLevelKind:='raytracing_';
  end else if TpvScene3D(fScene3D).UseBufferDeviceAddress then begin
   TopLevelKind:='bufref_';
  end else begin
   TopLevelKind:='';
  end;

  if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+TopLevelKind+Kind+'velocity_vert.spv');
  end else begin
   Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+TopLevelKind+Kind+'vert.spv');
  end;
  try
   fPlanetVertexShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;
  fVulkanDevice.DebugUtils.SetObjectName(fPlanetVertexShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.fPlanetVertexShaderModule');

  if TpvScene3D(fScene3D).MeshShaderSupport then begin

   fGrassVertexShaderModule:=nil;

   if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'velocity_task.spv');
   end else begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'task.spv');
   end;
   try
    fGrassTaskShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
   finally
    FreeAndNil(Stream);
   end;
   fVulkanDevice.DebugUtils.SetObjectName(fGrassTaskShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.GrassTaskShaderModule');

   if fVulkanDevice.PhysicalDevice.MeshShaderFeaturesEXT.multiviewMeshShader<>VK_FALSE then begin
    if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
     Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'velocity_multiview_mesh.spv');
    end else begin
     Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'multiview_mesh.spv');
    end;
   end else begin
    if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
     Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'velocity_mesh.spv');
    end else begin
     Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'mesh.spv');
    end;
   end;
   try
    fGrassMeshShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
   finally
    FreeAndNil(Stream);
   end;
   fVulkanDevice.DebugUtils.SetObjectName(fGrassMeshShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.GrassMeshShaderModule');

  end else begin

   if (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,TpvScene3DPlanet.TRenderPass.TMode.Opaque]) and TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'velocity_vert.spv');
   end else begin
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+'vert.spv');
   end;
   try
    fGrassVertexShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
   finally
    FreeAndNil(Stream);
   end;
   fVulkanDevice.DebugUtils.SetObjectName(fGrassVertexShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.GrassVertexShaderModule');

   fGrassTaskShaderModule:=nil;

   fGrassMeshShaderModule:=nil;

  end;

  case TpvScene3DPlanet.SourcePrimitiveMode of
   TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
    Kind:='triangles_';
   end;
   else begin
    Kind:='';
   end;
  end;

  case fMode of
   
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
   TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,
   TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion:begin
   
    fPlanetFragmentShaderModule:=nil; // No fragment shader, because we only need write to the depth buffer in these cases

    fGrassFragmentShaderModule:=nil; // No fragment shader, because we only need write to the depth buffer in these cases

   end; 
   
   TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
   
    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+TopLevelKind+'rsm_frag.spv');
    try
     fPlanetFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
    finally
     FreeAndNil(Stream);
    end;

    Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+'rsm_frag.spv');
    try
     fGrassFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
    finally
     FreeAndNil(Stream);
    end;

   end;

   else begin

    Kind:='';

    if fVulkanDevice.FragmentShaderBarycentricFeaturesKHR.fragmentShaderBarycentric<>VK_FALSE then begin
     if TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+TopLevelKind+'wireframe_velocity_'+Kind+ShadowKind+'frag.spv');
     end else begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+TopLevelKind+'wireframe_'+Kind+ShadowKind+'frag.spv');
     end;
    end else begin
     if TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+TopLevelKind+'velocity_'+Kind+ShadowKind+'frag.spv');
     end else begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_renderpass_'+TopLevelKind+Kind+ShadowKind+'frag.spv');
     end;
    end;
    try
     fPlanetFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
    finally
     FreeAndNil(Stream);
    end;

    if fVulkanDevice.FragmentShaderBarycentricFeaturesKHR.fragmentShaderBarycentric<>VK_FALSE then begin
     if TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+'wireframe_velocity_'+Kind+ShadowKind+'frag.spv');
     end else begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+'wireframe_'+Kind+ShadowKind+'frag.spv');
     end;
    end else begin
     if TpvScene3DRenderer(fRenderer).VelocityBufferNeeded then begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+'velocity_'+Kind+ShadowKind+'frag.spv');
     end else begin
      Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_grass_'+TopLevelKind+Kind+ShadowKind+'frag.spv');
     end;
    end;
    try
     fGrassFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
    finally
     FreeAndNil(Stream);
    end;

   end;
  end;

  if assigned(fPlanetFragmentShaderModule) then begin
   fVulkanDevice.DebugUtils.SetObjectName(fPlanetFragmentShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.fPlanetFragmentShaderModule');
  end;

  if assigned(fGrassFragmentShaderModule) then begin
   fVulkanDevice.DebugUtils.SetObjectName(fGrassFragmentShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TRenderPass.fGrassFragmentShaderModule');
  end;

  fPlanetVertexShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fPlanetVertexShaderModule,'main');

  if assigned(fPlanetFragmentShaderModule) then begin
   fPlanetFragmentShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fPlanetFragmentShaderModule,'main');
  end else begin
   fPlanetFragmentShaderStage:=nil;
  end;

  if assigned(fGrassVertexShaderModule) then begin
   fGrassVertexShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fGrassVertexShaderModule,'main');
  end else begin
   fGrassVertexShaderStage:=nil;
  end;

  if assigned(fGrassTaskShaderModule) then begin
   fGrassTaskShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_TASK_BIT_EXT,fGrassTaskShaderModule,'main');
  end else begin
   fGrassTaskShaderStage:=nil;
  end;

  if assigned(fGrassMeshShaderModule) then begin
   fGrassMeshShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_MESH_BIT_EXT,fGrassMeshShaderModule,'main');
  end else begin
   fGrassMeshShaderStage:=nil;
  end;

  if assigned(fGrassFragmentShaderModule) then begin
   fGrassFragmentShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fGrassFragmentShaderModule,'main');
  end else begin
   fGrassFragmentShaderStage:=nil;
  end;

  fShaderStageFlags:=IfThen(TpvScene3D(fScene3D).MeshShaderSupport,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_MESH_BIT_EXT) or
                                 TVkShaderStageFlags(VK_SHADER_STAGE_TASK_BIT_EXT),
                                 0) or
                          TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                          TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT);

  fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);

  // Uniform buffer with the views 
  fDescriptorSetLayout.AddBinding(0,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                  1,
                                  fShaderStageFlags,
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(1,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  3,
                                  fShaderStageFlags,
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(2,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  3,
                                  fShaderStageFlags,
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(3,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                  1,
                                  fShaderStageFlags,
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(4,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  1,
                                  fShaderStageFlags,
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(5,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  2,
                                  fShaderStageFlags,
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(6,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                  1,
                                  fShaderStageFlags,
                                  [],
                                  0);

  fDescriptorSetLayout.AddBinding(7,
                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                  1,
                                  fShaderStageFlags,
                                  [],
                                  0);

  // TODO: Add more bindings for other stuff like material textures, etc.

  fDescriptorSetLayout.Initialize;

  fPlanetPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPlanetPipelineLayout.AddPushConstantRange(fShaderStageFlags,
                                             0,
                                             SizeOf(TPlanetPushConstants));
  fPlanetPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout); // Global scene descriptor set
  fPlanetPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout); // Global planet descriptor set
  fPlanetPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetDescriptorSetLayout); // Per planet descriptor set
  fPlanetPipelineLayout.Initialize;
  fVulkanDevice.DebugUtils.SetObjectName(fPlanetPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TRenderPass.fPlanetPipelineLayout');

  fGrassPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fGrassPipelineLayout.AddPushConstantRange(fShaderStageFlags,
                                            0,
                                            SizeOf(TGrassPushConstants));
  fGrassPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout); // Global scene descriptor set
  fGrassPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout); // Global planet descriptor set
  fGrassPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetDescriptorSetLayout); // Per planet descriptor set
  fGrassPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetGrassCullAndMeshGenerationDescriptorSetLayout);
  fGrassPipelineLayout.Initialize;
  fVulkanDevice.DebugUtils.SetObjectName(fGrassPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TRenderPass.fGrassPipelineLayout');

 end;

end;

destructor TpvScene3DPlanet.TRenderPass.Destroy;
begin

 FreeAndNil(fGrassPipeline);

 FreeAndNil(fGrassPipelineLayout);

 FreeAndNil(fPlanetPipeline);

 FreeAndNil(fPlanetPipelineLayout);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fGrassFragmentShaderStage);

 FreeAndNil(fGrassMeshShaderStage);

 FreeAndNil(fGrassTaskShaderStage);

 FreeAndNil(fGrassVertexShaderStage);

 FreeAndNil(fGrassFragmentShaderModule);

 FreeAndNil(fGrassMeshShaderModule);

 FreeAndNil(fGrassTaskShaderModule);

 FreeAndNil(fGrassVertexShaderModule);

 FreeAndNil(fPlanetFragmentShaderStage);

 FreeAndNil(fPlanetVertexShaderStage);

 FreeAndNil(fPlanetFragmentShaderModule);

 FreeAndNil(fPlanetVertexShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TRenderPass.AllocateResources(const aRenderPass:TpvVulkanRenderPass;
                                                         const aWidth:TpvInt32;
                                                         const aHeight:TpvInt32;
                                                         const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits);
var InFlightFrameIndex:TpvSizeInt;
begin

 fWidth:=aWidth;
 fHeight:=aHeight;

 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 TpvScene3D(fScene3D).CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),2*TpvScene3D(fScene3D).CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),10*TpvScene3D(fScene3D).CountInFlightFrames);
 fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1*TpvScene3D(fScene3D).CountInFlightFrames);
 fDescriptorPool.Initialize;
 fVulkanDevice.DebugUtils.SetObjectName(fDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.TRenderPass.fDescriptorPool');

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
  if assigned(TpvScene3DRendererInstance(fRendererInstance).ImageBasedLightingReflectionProbeCubeMaps) then begin
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                            0,
                                                            3,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TpvScene3DRendererInstance(fRendererInstance).ImageBasedLightingReflectionProbeCubeMaps.GGXDescriptorImageInfos[InFlightFrameIndex],
                                                             TpvScene3DRendererInstance(fRendererInstance).ImageBasedLightingReflectionProbeCubeMaps.CharlieDescriptorImageInfos[InFlightFrameIndex],
                                                             TpvScene3DRendererInstance(fRendererInstance).ImageBasedLightingReflectionProbeCubeMaps.LambertianDescriptorImageInfos[InFlightFrameIndex]],
                                                            [],
                                                            [],
                                                            false);
  end else begin
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
  end;
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                           0,
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                           [],
                                                           [TpvScene3DRendererInstance(fRendererInstance).CascadedShadowMapVulkanUniformBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                           [],
                                                           false);
  if assigned(fResourceCascadedShadowMap) then begin
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                            0,
                                                            1,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.ShadowMapSampler.Handle,
                                                                                           fResourceCascadedShadowMap.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                           fResourceCascadedShadowMap.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                            [],
                                                            [],
                                                            false);
  end else begin
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                            0,
                                                            1,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.ShadowMapSampler.Handle,
                                                                                           TpvScene3DRendererInstance(fRendererInstance).Renderer.EmptyAmbientOcclusionTexture.ImageView.Handle,
                                                                                           TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                            [],
                                                            [],
                                                            false);
  end;
  if TpvScene3DRendererInstance(fRendererInstance).Renderer.ScreenSpaceAmbientOcclusion and assigned(fResourceSSAO) then begin
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                            0,
                                                            2,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.AmbientOcclusionSampler.Handle,
                                                                                           fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                           fResourceSSAO.ResourceTransition.Layout),// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))],
                                                             // Duplicate as dummy really non-used opaque texture
                                                             TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.AmbientOcclusionSampler.Handle,
                                                                                           fResourceSSAO.VulkanImageViews[InFlightFrameIndex].Handle,
                                                                                           fResourceSSAO.ResourceTransition.Layout)],// TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))
                                                            [],
                                                            [],
                                                            false);
  end else begin
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                            0,
                                                            2,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.AmbientOcclusionSampler.Handle,
                                                                                           TpvScene3DRendererInstance(fRendererInstance).Renderer.EmptyAmbientOcclusionTexture.ImageView.Handle,
                                                                                           TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)),
                                                             // Duplicate as dummy really non-used opaque texture
                                                             TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.AmbientOcclusionSampler.Handle,
                                                                                           TpvScene3DRendererInstance(fRendererInstance).Renderer.EmptyAmbientOcclusionTexture.ImageView.Handle,
                                                                                           TVkImageLayout(VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))
                                                            ],
                                                            [],
                                                            [],
                                                            false);
  end;
  fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                           0,
                                                           1,
                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                           [],
                                                           [TpvScene3DRendererInstance(fRendererInstance).Renderer.EnvironmentSphericalHarmonicsMetaDataBuffer.DescriptorBufferInfo],
                                                           [],
                                                           false);
  case fMode of
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion:begin
    fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                             0,
                                                             1,
                                                             TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                             [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRenderer).MipMapMaxFilterSampler.Handle,
                                                                                            TpvScene3DRendererInstance(fRendererInstance).CullDepthPyramidMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                             [],
                                                             [],
                                                             false
                                                            );
   end;
   else begin
    if TpvScene3DRendererInstance(fRendererInstance).ZNear<0.0 then begin
     fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                              0,
                                                              1,
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                              [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRenderer).MipMapMinFilterSampler.Handle,
                                                                                             TpvScene3DRendererInstance(fRendererInstance).CullDepthPyramidMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                              [],
                                                              [],
                                                              false
                                                             );
    end else begin
     fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(7,
                                                              0,
                                                              1,
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                              [TVkDescriptorImageInfo.Create(TpvScene3DRenderer(fRenderer).MipMapMaxFilterSampler.Handle,
                                                                                             TpvScene3DRendererInstance(fRendererInstance).CullDepthPyramidMipmappedArray2DImage.VulkanArrayImageView.Handle,
                                                                                             VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                              [],
                                                              [],
                                                              false
                                                             );
    end;
   end;
  end;
  fDescriptorSets[InFlightFrameIndex].Flush;
  fVulkanDevice.DebugUtils.SetObjectName(fDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TRenderPass.fDescriptorSets['+IntToStr(InFlightFrameIndex)+']');
 end;

 begin

  fPlanetPipeline:=TpvVulkanGraphicsPipeline.Create(fVulkanDevice,
                                                    TpvScene3DRenderer(fRenderer).VulkanPipelineCache,
                                                    0,
                                                    [],
                                                    fPlanetPipelineLayout,
                                                    aRenderPass,
                                                    0,
                                                    nil,
                                                    0);

  fPlanetPipeline.AddStage(fPlanetVertexShaderStage);
  if assigned(fPlanetFragmentShaderStage) then begin
   fPlanetPipeline.AddStage(fPlanetFragmentShaderStage);
  end;

  fPlanetPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);

  fPlanetPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

  case TpvScene3DPlanet.SourcePrimitiveMode of
   TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles,
   TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
    fPlanetPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3DPlanet.TMeshVertex),VK_VERTEX_INPUT_RATE_VERTEX);
    fPlanetPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,0);
    fPlanetPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R16G16_SNORM,TpvPtrUInt(Pointer(@TpvScene3DPlanet.PMeshVertex(nil)^.OctahedralEncodedNormal)));
   end;
   else begin
   end;
  end;

  fPlanetPipeline.ViewPortState.AddViewPort(0.0,0.0,aWidth,aHeight,0.0,1.0);
  fPlanetPipeline.ViewPortState.AddScissor(0,0,aWidth,aHeight);

  fPlanetPipeline.RasterizationState.DepthClampEnable:=false;
  fPlanetPipeline.RasterizationState.RasterizerDiscardEnable:=false;
  fPlanetPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
  case fMode of
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
   TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
    fPlanetPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
   end;
   else begin
    fPlanetPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
   end;
  end;
  fPlanetPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
  fPlanetPipeline.RasterizationState.DepthBiasEnable:=false;
  fPlanetPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
  fPlanetPipeline.RasterizationState.DepthBiasClamp:=0.0;
  fPlanetPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
  fPlanetPipeline.RasterizationState.LineWidth:=1.0;

  fPlanetPipeline.MultisampleState.RasterizationSamples:=aVulkanSampleCountFlagBits;
  fPlanetPipeline.MultisampleState.SampleShadingEnable:=false;
  fPlanetPipeline.MultisampleState.MinSampleShading:=0.0;
  fPlanetPipeline.MultisampleState.CountSampleMasks:=0;
  fPlanetPipeline.MultisampleState.AlphaToCoverageEnable:=false;
  fPlanetPipeline.MultisampleState.AlphaToOneEnable:=false;

  fPlanetPipeline.ColorBlendState.LogicOpEnable:=false;
  fPlanetPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
  fPlanetPipeline.ColorBlendState.BlendConstants[0]:=0.0;
  fPlanetPipeline.ColorBlendState.BlendConstants[1]:=0.0;
  fPlanetPipeline.ColorBlendState.BlendConstants[2]:=0.0;
  fPlanetPipeline.ColorBlendState.BlendConstants[3]:=0.0;
  fPlanetPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
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
   fPlanetPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                          VK_BLEND_FACTOR_ZERO,
                                                          VK_BLEND_FACTOR_ZERO,
                                                          VK_BLEND_OP_ADD,
                                                          VK_BLEND_FACTOR_ZERO,
                                                          VK_BLEND_FACTOR_ZERO,
                                                          VK_BLEND_OP_ADD,
                                                          0);
  end;

  fPlanetPipeline.DepthStencilState.DepthTestEnable:=true;
  fPlanetPipeline.DepthStencilState.DepthWriteEnable:=true;
  case fMode of
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
   TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
    fPlanetPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
   end;
   else begin
    if TpvScene3DRendererInstance(fRendererInstance).ZFar<0.0 then begin
     fPlanetPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
    end else begin
     fPlanetPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
    end;
   end;
  end;
  fPlanetPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
  fPlanetPipeline.DepthStencilState.StencilTestEnable:=false;

  fPlanetPipeline.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fPlanetPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DPlanet.TRenderPass.fPlanetPipeline');

 end;

 begin

  fGrassPipeline:=TpvVulkanGraphicsPipeline.Create(fVulkanDevice,
                                                   TpvScene3DRenderer(fRenderer).VulkanPipelineCache,
                                                   0,
                                                   [],
                                                   fGrassPipelineLayout,
                                                   aRenderPass,
                                                   0,
                                                   nil,
                                                   0);

  if assigned(fGrassTaskShaderStage) and assigned(fGrassMeshShaderStage) then begin

   fGrassPipeline.AddStage(fGrassTaskShaderStage);
   fGrassPipeline.AddStage(fGrassMeshShaderStage);

  end else if assigned(fGrassVertexShaderStage) then begin

   fGrassPipeline.AddStage(fGrassVertexShaderStage);

  end;

  if assigned(fGrassFragmentShaderStage) then begin
   fGrassPipeline.AddStage(fGrassFragmentShaderStage);
  end;

  fGrassPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);

  fGrassPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

  if assigned(fGrassVertexShaderStage) then begin
   fGrassPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3DPlanet.TGrassVertex),VK_VERTEX_INPUT_RATE_VERTEX);
   fGrassPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TpvPtrUInt(Pointer(@TpvScene3DPlanet.PGrassVertex(nil)^.PositionX)));
   fGrassPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_A2B10G10R10_SNORM_PACK32,TpvPtrUInt(Pointer(@TpvScene3DPlanet.PGrassVertex(nil)^.NormalTexCoordU)));
   fGrassPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_A2B10G10R10_SNORM_PACK32,TpvPtrUInt(Pointer(@TpvScene3DPlanet.PGrassVertex(nil)^.TangentSign)));
   fGrassPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32_SFLOAT,TpvPtrUInt(Pointer(@TpvScene3DPlanet.PGrassVertex(nil)^.TexCoordV)));
 //fGrassPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32_UINT,TpvPtrUInt(Pointer(@TpvScene3DPlanet.PGrassVertex(nil)^.BladeIndex)));
 //fGrassPipeline.VertexInputState.AddVertexInputAttributeDescription(5,0,VK_FORMAT_R32_UINT,TpvPtrUInt(Pointer(@TpvScene3DPlanet.PGrassVertex(nil)^.BladeID)));
  end;

  fGrassPipeline.ViewPortState.AddViewPort(0.0,0.0,aWidth,aHeight,0.0,1.0);
  fGrassPipeline.ViewPortState.AddScissor(0,0,aWidth,aHeight);

  fGrassPipeline.RasterizationState.DepthClampEnable:=false;
  fGrassPipeline.RasterizationState.RasterizerDiscardEnable:=false;
  fGrassPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
  case fMode of
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
   TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
    fGrassPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
   end;
   else begin
//  fGrassPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
    fGrassPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
   end;
  end;
  fGrassPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
  fGrassPipeline.RasterizationState.DepthBiasEnable:=false;
  fGrassPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
  fGrassPipeline.RasterizationState.DepthBiasClamp:=0.0;
  fGrassPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
  fGrassPipeline.RasterizationState.LineWidth:=1.0;

  fGrassPipeline.MultisampleState.RasterizationSamples:=aVulkanSampleCountFlagBits;
  fGrassPipeline.MultisampleState.SampleShadingEnable:=false;
  fGrassPipeline.MultisampleState.MinSampleShading:=0.0;
  fGrassPipeline.MultisampleState.CountSampleMasks:=0;
  fGrassPipeline.MultisampleState.AlphaToCoverageEnable:=false;
  fGrassPipeline.MultisampleState.AlphaToOneEnable:=false;

  fGrassPipeline.ColorBlendState.LogicOpEnable:=false;
  fGrassPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
  fGrassPipeline.ColorBlendState.BlendConstants[0]:=0.0;
  fGrassPipeline.ColorBlendState.BlendConstants[1]:=0.0;
  fGrassPipeline.ColorBlendState.BlendConstants[2]:=0.0;
  fGrassPipeline.ColorBlendState.BlendConstants[3]:=0.0;
  fGrassPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
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
   fGrassPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                               VK_BLEND_FACTOR_ZERO,
                                                               VK_BLEND_FACTOR_ZERO,
                                                               VK_BLEND_OP_ADD,
                                                               VK_BLEND_FACTOR_ZERO,
                                                               VK_BLEND_FACTOR_ZERO,
                                                               VK_BLEND_OP_ADD,
                                                               0);
  end;

  fGrassPipeline.DepthStencilState.DepthTestEnable:=true;
  fGrassPipeline.DepthStencilState.DepthWriteEnable:=true;
  case fMode of
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
   TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
   TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
    fGrassPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
   end;
   else begin
    if TpvScene3DRendererInstance(fRendererInstance).ZFar<0.0 then begin
     fGrassPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
    end else begin
     fGrassPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
    end;
   end;
  end;
  fGrassPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
  fGrassPipeline.DepthStencilState.StencilTestEnable:=false;

  fGrassPipeline.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fGrassPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DPlanet.TRenderPass.fGrassPipeline');

 end;

end;

procedure TpvScene3DPlanet.TRenderPass.ReleaseResources;
var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fGrassPipeline);

 FreeAndNil(fPlanetPipeline);

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fDescriptorPool);

end;

procedure TpvScene3DPlanet.TRenderPass.Draw(const aInFlightFrameIndex,aFrameIndex,aRenderPassIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer);
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
    RendererInstance:TpvScene3DPlanet.TRendererInstance;
    RendererViewInstance:TpvScene3DPlanet.TRendererViewInstance;
    vkCmdDrawIndexedIndirectCount:TvkCmdDrawIndexedIndirectCount;
begin

 if assigned(TpvScene3D(fScene3D).VulkanDevice.Commands.Commands.CmdDrawIndexedIndirectCount) then begin
  vkCmdDrawIndexedIndirectCount:=TpvScene3D(fScene3D).VulkanDevice.Commands.Commands.CmdDrawIndexedIndirectCount;
 end else if assigned(TpvScene3D(fScene3D).VulkanDevice.Commands.Commands.CmdDrawIndexedIndirectCountKHR) then begin
  vkCmdDrawIndexedIndirectCount:=addr(TpvScene3D(fScene3D).VulkanDevice.Commands.Commands.CmdDrawIndexedIndirectCountKHR);
 end else begin
  vkCmdDrawIndexedIndirectCount:=nil;
 end;

 TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.AcquireRead;
 try

  begin

   First:=true;

   for PlanetIndex:=0 to TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Count-1 do begin

    Planet:=TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Items[PlanetIndex];

    if Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin

     {if Planet.fData.fVisible then}begin

      if First then begin

       First:=false;

       aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fPlanetPipeline.Handle);

       DescriptorSets[0]:=TpvScene3D(fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
       DescriptorSets[1]:=fDescriptorSets[aInFlightFrameIndex].Handle;

       aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                            fPlanetPipelineLayout.Handle,
                                            0,
                                            2,
                                            @DescriptorSets,
                                            0,
                                            nil);

      end;

      aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                           fPlanetPipelineLayout.Handle,
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
       TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
       TpvScene3DPlanet.TRenderPass.TMode.ReflectiveShadowMap:begin
        TessellationFactor:=1.0/4.0;
        Level:=-1;
       end;
       else begin
        TessellationFactor:=1.0/16.0;
        Level:=Min(Max(Round(Rect.Size.Length/Max(1,sqrt(sqr(fWidth)+sqr(fHeight))/4.0)),0),7);
       end;
      end;

 //     writeln(Rect.Width:1:6,' ',Rect.Height:1:6,' ',Level:1:6);

      fPlanetPushConstants.ViewBaseIndex:=aViewBaseIndex;
      fPlanetPushConstants.CountViews:=aCountViews;
      if Level<0 then begin
       fPlanetPushConstants.CountQuadPointsInOneDirection:=64;
      end else begin
       fPlanetPushConstants.CountQuadPointsInOneDirection:=64;//Min(Max(16 shl Level,2),256);
      end;
      if Level>=0 then begin
       //writeln(fPlanetPushConstants.CountQuadPointsInOneDirection,' ',Level);
      end;
      fPlanetPushConstants.CountAllViews:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].CountViews;
      fPlanetPushConstants.ResolutionXY:=(fWidth and $ffff) or ((fHeight and $ffff) shl 16);
      fPlanetPushConstants.TessellationFactor:=TessellationFactor;
      if fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,TpvScene3DPlanet.TRenderPass.TMode.Opaque] then begin
       fPlanetPushConstants.Jitter:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].Jitter.xy;
      end else begin
       fPlanetPushConstants.Jitter:=TpvVector2.Null;
      end;
      fPlanetPushConstants.FrameIndex:=aFrameIndex;
      if TpvScene3D(fScene3D).UseBufferDeviceAddress then begin
       fPlanetPushConstants.PlanetData:=Planet.fPlanetDataVulkanBuffers[aInFlightFrameIndex].DeviceAddress;
      end else begin
       fPlanetPushConstants.PlanetData:=0;
      end;

      aCommandBuffer.CmdPushConstants(fPlanetPipelineLayout.Handle,
                                      fShaderStageFlags,
                                      0,
                                      SizeOf(TPlanetPushConstants),
                                      @fPlanetPushConstants);

      case TpvScene3DPlanet.SourcePrimitiveMode of
       TpvScene3DPlanet.TSourcePrimitiveMode.VisualMeshTriangles:begin
 {      aCommandBuffer.CmdBindIndexBuffer(Planet.fData.fVisualMeshIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
        aCommandBuffer.CmdBindVertexBuffers(0,1,@Planet.fData.fVisualMeshVertexBuffer.Handle,@Offsets);}
        aCommandBuffer.CmdBindIndexBuffer(Planet.fData.fVisualMeshIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
        aCommandBuffer.CmdBindVertexBuffers(0,1,@Planet.fData.fVisualMeshVertexBuffers[Planet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshVertexBufferRenderIndex and 1].Handle,@Offsets);
 ///    aCommandBuffer.CmdBindIndexBuffer(Planet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
 //      aCommandBuffer.CmdBindVertexBuffers(0,1,@Planet.fInFlightFrameDataList[aInFlightFrameIndex].fVisualMeshVertexBuffer.Handle,@Offsets);{}
        if assigned(vkCmdDrawIndexedIndirectCount) and
           Planet.fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fRendererInstance,aRenderPassIndex),
                                                      RendererViewInstance) then begin
         case fMode of
          TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion,
          TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion:begin
           vkCmdDrawIndexedIndirectCount(aCommandBuffer.Handle,
                                         RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                         ((Planet.TileMapResolution*Planet.TileMapResolution)+1)*(16*SizeOf(TVkUInt32)),
                                         RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                         SizeOf(TVkUInt32),
                                         Planet.TileMapResolution*Planet.TileMapResolution,
                                         16*SizeOf(TVkUInt32));
          end;
          else begin
           vkCmdDrawIndexedIndirectCount(aCommandBuffer.Handle,
                                         RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                         16*SizeOf(TVkUInt32),
                                         RendererViewInstance.fVulkanDrawIndexedIndirectCommandBuffer.Handle,
                                         0,
                                         Planet.TileMapResolution*Planet.TileMapResolution,
                                         16*SizeOf(TVkUInt32));
          end;
         end;
        end else begin
         aCommandBuffer.CmdDrawIndexed(Planet.fVisualMeshLODCounts[0],
                                       1,
                                       Planet.fVisualMeshLODOffsets[0],
                                       0,
                                       0);
        end;
       end;
       TpvScene3DPlanet.TSourcePrimitiveMode.PhysicsMeshTriangles:begin
        aCommandBuffer.CmdBindIndexBuffer(Planet.fData.fPhysicsMeshIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
        aCommandBuffer.CmdBindVertexBuffers(0,1,@Planet.fData.fPhysicsMeshVertexBuffer.Handle,@Offsets);
        aCommandBuffer.CmdDrawIndexed(Planet.fPhysicsMeshLODCounts[0],
                                      1,
                                      Planet.fPhysicsMeshLODOffsets[0],
                                      0,
                                      0);
       end;
       else begin
        Assert(false);
       end;
      end;

     end;

    end;

   end;

  end;

  if assigned(fGrassPipeline) and
     not (fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,
                    //TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,
                    TpvScene3DPlanet.TRenderPass.TMode.ShadowMap,
                    TpvScene3DPlanet.TRenderPass.TMode.ShadowMapDisocclusion]) then begin

   First:=true;

   for PlanetIndex:=0 to TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Count-1 do begin

    Planet:=TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Items[PlanetIndex];

    if Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin

     {if Planet.fData.fVisible then}begin

      if First then begin

       First:=false;

       aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fGrassPipeline.Handle);

       DescriptorSets[0]:=TpvScene3D(fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
       DescriptorSets[1]:=fDescriptorSets[aInFlightFrameIndex].Handle;

       aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                            fGrassPipelineLayout.Handle,
                                            0,
                                            2,
                                            @DescriptorSets,
                                            0,
                                            nil);

      end;

      aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                           fGrassPipelineLayout.Handle,
                                           2,
                                           1,
                                           @Planet.fDescriptorSets[aInFlightFrameIndex].Handle,
                                           0,
                                           nil);

      aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                           fGrassPipelineLayout.Handle,
                                           3,
                                           1,
                                           @RendererViewInstance.fGrassCullDescriptorSets[Planet.fData.fVisualMeshVertexBufferRenderIndex and 1].Handle,
                                           0,
                                           nil);

      ViewMatrix:=@TpvScene3DRendererInstance(fRendererInstance).Views[aInFlightFrameIndex].Items[aViewBaseIndex].ViewMatrix;
      InverseViewMatrix:=@TpvScene3DRendererInstance(fRendererInstance).Views[aInFlightFrameIndex].Items[aViewBaseIndex].InverseViewMatrix;
      ProjectionMatrix:=@TpvScene3DRendererInstance(fRendererInstance).Views[aInFlightFrameIndex].Items[aViewBaseIndex].ProjectionMatrix;

      fGrassPushConstants.ModelMatrix:=Planet.fInFlightFrameDataList[aInFlightFrameIndex].fModelMatrix;
      fGrassPushConstants.ViewBaseIndex:=aViewBaseIndex;
      fGrassPushConstants.CountViews:=aCountViews;
      fGrassPushConstants.Time:=Modulo(TpvScene3D(Planet.Scene3D).SceneTimes^[aInFlightFrameIndex],65536.0);
      fGrassPushConstants.CountAllViews:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].CountViews;
      fGrassPushConstants.TileMapResolution:=Planet.fTileMapResolution;
      fGrassPushConstants.TileResolution:=Planet.fVisualTileResolution;
      fGrassPushConstants.MaximumDistance:=Planet.fTopRadius;
      fGrassPushConstants.GrassHeight:=0.125*5.0;//1.25;
      fGrassPushConstants.GrassThickness:=0.01;
      fGrassPushConstants.MaximalCountBladesPerPatch:=8;
      fGrassPushConstants.ResolutionXY:=(fWidth and $ffff) or ((fHeight and $ffff) shl 16);
      if fMode in [TpvScene3DPlanet.TRenderPass.TMode.DepthPrepass,TpvScene3DPlanet.TRenderPass.TMode.DepthPrepassDisocclusion,TpvScene3DPlanet.TRenderPass.TMode.Opaque] then begin
       fGrassPushConstants.Jitter:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].Jitter.xy;
      end else begin
       fGrassPushConstants.Jitter:=TpvVector2.Null;
      end;
      fGrassPushConstants.FrameIndex:=aFrameIndex;
{     if TpvScene3D(fScene3D).UseBufferDeviceAddress then begin
       fGrassPushConstants.PlanetData:=Planet.fPlanetDataVulkanBuffers[aInFlightFrameIndex].DeviceAddress;
      end else begin
       fGrassPushConstants.PlanetData:=0;
      end;}

      aCommandBuffer.CmdPushConstants(fGrassPipelineLayout.Handle,
                                      fShaderStageFlags,
                                      0,
                                      SizeOf(TGrassPushConstants),
                                      @fGrassPushConstants);

      if assigned(vkCmdDrawIndexedIndirectCount) and
         Planet.fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fRendererInstance,aRenderPassIndex),
                                                    RendererViewInstance) then begin

       if assigned(RendererViewInstance.fVulkanGrassVerticesBuffer) or not TpvScene3D(fScene3D).MeshShaderSupport then begin

        aCommandBuffer.CmdBindIndexBuffer(RendererViewInstance.fVulkanGrassIndicesBuffer.Handle,0,VK_INDEX_TYPE_UINT32);
        aCommandBuffer.CmdBindVertexBuffers(0,1,@RendererViewInstance.fVulkanGrassVerticesBuffer.Handle,@Offsets);

        aCommandBuffer.CmdDrawIndexedIndirect(RendererViewInstance.fVulkanGrassMetaDataBuffer.Handle,
                                              TpvPtrUInt(@PGrassMetaData(nil)^.DrawIndexedIndirectCommand),
                                              1,
                                              SizeOf(TVkDrawIndexedIndirectCommand));

       end else if TpvScene3D(fScene3D).MeshShaderSupport then begin

        if assigned(TpvScene3D(fScene3D).VulkanDevice.Commands.Commands.CmdDrawMeshTasksIndirectEXT) then begin

         TpvScene3D(fScene3D).VulkanDevice.Commands.Commands.CmdDrawMeshTasksIndirectEXT(aCommandBuffer.Handle,
                                                                                         RendererViewInstance.fVulkanVisibleTileListBuffer.Handle,
                                                                                         0,
                                                                                         1,
                                                                                         SizeOf(TVkDrawMeshTasksIndirectCommandEXT));

        end else begin

         TpvScene3D(fScene3D).VulkanDevice.Commands.Commands.CmdDrawMeshTasksEXT(aCommandBuffer.Handle,
                                                                                 (((Planet.fVisualTileResolution shr 0)*(Planet.fVisualTileResolution shr 0))+127) shr 7,
                                                                                 ((Planet.fTileMapResolution*Planet.fTileMapResolution)+0) shr 0,
                                                                                 1
                                                                                {((Planet.fTileMapResolution*Planet.fTileMapResolution)+7) shr 3,
                                                                                 (((Planet.fVisualTileResolution shr 2)*(Planet.fVisualTileResolution shr 2))+15) shr 4,
                                                                                 1});

        end;

       end;

      end;

     end;

    end;

   end;

  end;

 finally
  TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.ReleaseRead;
 end;

end;

{ TpvScene3DPlanet.TWaterRenderPass }

constructor TpvScene3DPlanet.TWaterRenderPass.Create(const aRenderer:TObject;
                                                     const aRendererInstance:TObject;
                                                     const aScene3D:TObject;
                                                     const aMSAA:Boolean;
                                                     const aPass:TpvSizeInt;
                                                     const aResourceCascadedShadowMap:TpvFrameGraph.TPass.TUsedImageResource;
                                                     const aResourceSSAO:TpvFrameGraph.TPass.TUsedImageResource);
var Stream:TStream;
    ShaderFileName:TpvUTF8String;
begin

 inherited Create;

 fRenderer:=aRenderer;

 fRendererInstance:=aRendererInstance;

 fScene3D:=aScene3D;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 fResourceCascadedShadowMap:=aResourceCascadedShadowMap;

 fResourceSSAO:=aResourceSSAO;

 fRenderPass:=nil;

 fUnderwaterVertexShaderModule:=nil;

 fUnderwaterFragmentShaderModule:=nil;

 fUnderwaterVertexShaderStage:=nil;

 fUnderwaterFragmentShaderStage:=nil;

 fWaterVertexShaderModule:=nil;

 fWaterTessellationControlShaderModule:=nil;
 
 fWaterTessellationEvaluationShaderModule:=nil;

 fWaterFragmentShaderModule:=nil;

 fWaterVertexShaderStage:=nil;

 fWaterTessellationControlShaderStage:=nil;

 fWaterTessellationEvaluationShaderStage:=nil;

 fWaterFragmentShaderStage:=nil;

 fDescriptorSetLayout:=nil;

 fDescriptorPool:=nil;

 fPipelineLayout:=nil;

 fUnderwaterPipeline:=nil;

 fWaterPipeline:=nil;

 fMSAA:=aMSAA;

 fPass:=aPass;

 fWidth:=0;

 fHeight:=0;

 ShaderFileName:='planet_water_underwater';

 if TpvScene3D(fScene3D).RaytracingActive then begin
  ShaderFileName:=ShaderFileName+'_raytracing';
 end else if TpvScene3D(fScene3D).UseBufferDeviceAddress then begin
  ShaderFileName:=ShaderFileName+'_bufref';
 end;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile(ShaderFileName+'_vert.spv');
 try
  fUnderwaterVertexShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  FreeAndNil(Stream);
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fUnderwaterVertexShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterRenderPass.fUnderwaterVertexShaderModule');

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile(ShaderFileName+'_frag.spv');
 try
  fUnderwaterFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  FreeAndNil(Stream);
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fUnderwaterFragmentShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterRenderPass.fUnderwaterFragmentShaderModule');

 ShaderFileName:='planet_water';

 if TpvScene3D(fScene3D).RaytracingActive then begin
  ShaderFileName:=ShaderFileName+'_raytracing';
 end else if TpvScene3D(fScene3D).UseBufferDeviceAddress then begin
  ShaderFileName:=ShaderFileName+'_bufref';
 end;

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile(ShaderFileName+'_vert.spv');
 try
  fWaterVertexShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  FreeAndNil(Stream);
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fWaterVertexShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterRenderPass.fWaterVertexShaderModule');

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile(ShaderFileName+'_tesc.spv');
 try
  fWaterTessellationControlShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  FreeAndNil(Stream);
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fWaterTessellationControlShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterRenderPass.fWaterTessellationControlShaderModule');

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile(ShaderFileName+'_tese.spv');
 try
  fWaterTessellationEvaluationShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  FreeAndNil(Stream);
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fWaterTessellationEvaluationShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterRenderPass.fWaterTessellationEvaluationShaderModule');

 ShaderFileName:='planet_water';

 if TpvScene3D(fScene3D).RaytracingActive then begin
  ShaderFileName:=ShaderFileName+'_raytracing';
 end;

 ShaderFileName:=ShaderFileName+'_'+TpvScene3DRenderer(aRenderer).MeshFragShadowTypeName; // pcfpcss or msm

 if TpvScene3DRendererInstance(aRendererInstance).ZFar<0.0 then begin 
  ShaderFileName:=ShaderFileName+'_reversedz';
 end;

 if TpvScene3DRenderer(aRenderer).SurfaceSampleCountFlagBits<>TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT) then begin
  if fMSAA then begin
   ShaderFileName:=ShaderFileName+'_msaa';
  end else begin
   ShaderFileName:=ShaderFileName+'_msaa_fast';
  end;
 end;

 ShaderFileName:=ShaderFileName+'_frag.spv';

 Stream:=pvScene3DShaderVirtualFileSystem.GetFile(ShaderFileName);
 try
  fWaterFragmentShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
 finally
  FreeAndNil(Stream);
 end;
 fVulkanDevice.DebugUtils.SetObjectName(fWaterFragmentShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TWaterRenderPass.fWaterFragmentShaderModule');

 fUnderwaterVertexShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fUnderwaterVertexShaderModule,'main');

 fUnderwaterFragmentShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fUnderwaterFragmentShaderModule,'main');

 fWaterVertexShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fWaterVertexShaderModule,'main');

 fWaterTessellationControlShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT,fWaterTessellationControlShaderModule,'main');

 fWaterTessellationEvaluationShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT,fWaterTessellationEvaluationShaderModule,'main');

 fWaterFragmentShaderStage:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fWaterFragmentShaderModule,'main');

end; 

destructor TpvScene3DPlanet.TWaterRenderPass.Destroy;
begin
  
 FreeAndNil(fWaterPipeline);

 FreeAndNil(fPipelineLayout);

 FreeAndNil(fDescriptorPool);

 FreeAndNil(fDescriptorSetLayout);

 FreeAndNil(fWaterFragmentShaderStage);

 FreeAndNil(fWaterTessellationEvaluationShaderStage);

 FreeAndNil(fWaterTessellationControlShaderStage);

 FreeAndNil(fWaterVertexShaderStage);

 FreeAndNil(fWaterTessellationEvaluationShaderModule);

 FreeAndNil(fWaterTessellationControlShaderModule);

 FreeAndNil(fWaterFragmentShaderModule);

 FreeAndNil(fWaterVertexShaderModule);

 FreeAndNil(fUnderwaterFragmentShaderStage);

 FreeAndNil(fUnderwaterVertexShaderStage);

 FreeAndNil(fUnderwaterFragmentShaderModule);

 FreeAndNil(fUnderwaterVertexShaderModule);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.TWaterRenderPass.AllocateResources(const aRenderPass:TpvVulkanRenderPass;
                                                              const aPassVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
                                                              const aWidth:TpvInt32;
                                                              const aHeight:TpvInt32;
                                                              const aVulkanSampleCountFlagBits:TVkSampleCountFlagBits);
//var InFlightFrameIndex:TpvSizeInt;
begin

 fWidth:=aWidth;
 fHeight:=aHeight;

 fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
 fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                                      TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                                      TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                                      TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                      0,
                                      SizeOf(TPushConstants));
 fPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).GlobalVulkanDescriptorSetLayout); // Global scene descriptor set
 fPipelineLayout.AddDescriptorSetLayout(aPassVulkanDescriptorSetLayout); // Pass descriptor set
 fPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetDescriptorSetLayout); // Per planet descriptor set
 fPipelineLayout.AddDescriptorSetLayout(TpvScene3D(fScene3D).PlanetWaterRenderDescriptorSetLayout); // Per render pass descriptor set
 fPipelineLayout.Initialize;
 fVulkanDevice.DebugUtils.SetObjectName(fPipelineLayout.Handle,VK_OBJECT_TYPE_PIPELINE_LAYOUT,'TpvScene3DPlanet.TWaterRenderPass.fPipelineLayout');

 begin

  fUnderwaterPipeline:=TpvVulkanGraphicsPipeline.Create(fVulkanDevice,
                                                        TpvScene3DRenderer(fRenderer).VulkanPipelineCache,
                                                        0,
                                                        [],
                                                        fPipelineLayout,
                                                        aRenderPass,
                                                        0,
                                                        nil,
                                                        0);

  fUnderwaterPipeline.AddStage(fUnderwaterVertexShaderStage);
  fUnderwaterPipeline.AddStage(fUnderwaterFragmentShaderStage);

  fUnderwaterPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
  fUnderwaterPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

  fUnderwaterPipeline.ViewPortState.AddViewPort(0.0,0.0,aWidth,aHeight,0.0,1.0);
  fUnderwaterPipeline.ViewPortState.AddScissor(0,0,aWidth,aHeight);

  fUnderwaterPipeline.RasterizationState.DepthClampEnable:=false;
  fUnderwaterPipeline.RasterizationState.RasterizerDiscardEnable:=false;
  fUnderwaterPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
  fUnderwaterPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
  fUnderwaterPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
  fUnderwaterPipeline.RasterizationState.DepthBiasEnable:=false;
  fUnderwaterPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
  fUnderwaterPipeline.RasterizationState.DepthBiasClamp:=0.0;
  fUnderwaterPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
  fUnderwaterPipeline.RasterizationState.LineWidth:=1.0;

  if fMSAA then begin
   fUnderwaterPipeline.MultisampleState.RasterizationSamples:=aVulkanSampleCountFlagBits;
  end else begin
   fUnderwaterPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
  end;
  fUnderwaterPipeline.MultisampleState.SampleShadingEnable:=false;
  fUnderwaterPipeline.MultisampleState.MinSampleShading:=0.0;
  fUnderwaterPipeline.MultisampleState.CountSampleMasks:=0;
  fUnderwaterPipeline.MultisampleState.AlphaToCoverageEnable:=false;
  fUnderwaterPipeline.MultisampleState.AlphaToOneEnable:=false;

  fUnderwaterPipeline.ColorBlendState.LogicOpEnable:=false;
  fUnderwaterPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
  fUnderwaterPipeline.ColorBlendState.BlendConstants[0]:=0.0;
  fUnderwaterPipeline.ColorBlendState.BlendConstants[1]:=0.0;
  fUnderwaterPipeline.ColorBlendState.BlendConstants[2]:=0.0;
  fUnderwaterPipeline.ColorBlendState.BlendConstants[3]:=0.0;
  fUnderwaterPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                                   VK_BLEND_FACTOR_ONE,
                                                                   VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                   VK_BLEND_OP_ADD,
                                                                   VK_BLEND_FACTOR_ONE,
                                                                   VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                   VK_BLEND_OP_ADD,
                                                                   TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                   TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                   TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                   TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

  fUnderwaterPipeline.DepthStencilState.DepthTestEnable:=false;
  fUnderwaterPipeline.DepthStencilState.DepthWriteEnable:=false;
  if TpvScene3DRendererInstance(fRendererInstance).ZFar<0.0 then begin
   fUnderwaterPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
  end else begin
   fUnderwaterPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
  end;
  fUnderwaterPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
  fUnderwaterPipeline.DepthStencilState.StencilTestEnable:=false;

  fUnderwaterPipeline.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fUnderwaterPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DPlanet.TWaterRenderPass.fUnderwaterPipeline');

 end;

 begin

  fWaterPipeline:=TpvVulkanGraphicsPipeline.Create(fVulkanDevice,
                                              TpvScene3DRenderer(fRenderer).VulkanPipelineCache,
                                              0,
                                              [],
                                              fPipelineLayout,
                                              aRenderPass,
                                              0,
                                              nil,
                                              0);

  fWaterPipeline.AddStage(fWaterVertexShaderStage);
  fWaterPipeline.AddStage(fWaterTessellationControlShaderStage);
  fWaterPipeline.AddStage(fWaterTessellationEvaluationShaderStage);
  fWaterPipeline.AddStage(fWaterFragmentShaderStage);

  fWaterPipeline.InputAssemblyState.Topology:=TVkPrimitiveTopology(VK_PRIMITIVE_TOPOLOGY_PATCH_LIST);
  fWaterPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

  fWaterPipeline.TessellationState.PatchControlPoints:=4;

  fWaterPipeline.ViewPortState.AddViewPort(0.0,0.0,aWidth,aHeight,0.0,1.0);
  fWaterPipeline.ViewPortState.AddScissor(0,0,aWidth,aHeight);

  fWaterPipeline.RasterizationState.DepthClampEnable:=false;
  fWaterPipeline.RasterizationState.RasterizerDiscardEnable:=false;
  fWaterPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
  fWaterPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
  fWaterPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
  fWaterPipeline.RasterizationState.DepthBiasEnable:=false;
  fWaterPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
  fWaterPipeline.RasterizationState.DepthBiasClamp:=0.0;
  fWaterPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
  fWaterPipeline.RasterizationState.LineWidth:=1.0;

  if fMSAA then begin
   fWaterPipeline.MultisampleState.RasterizationSamples:=aVulkanSampleCountFlagBits;
  end else begin
   fWaterPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
  end;
  fWaterPipeline.MultisampleState.SampleShadingEnable:=false;
  fWaterPipeline.MultisampleState.MinSampleShading:=0.0;
  fWaterPipeline.MultisampleState.CountSampleMasks:=0;
  fWaterPipeline.MultisampleState.AlphaToCoverageEnable:=false;
  fWaterPipeline.MultisampleState.AlphaToOneEnable:=false;

  fWaterPipeline.ColorBlendState.LogicOpEnable:=false;
  fWaterPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
  fWaterPipeline.ColorBlendState.BlendConstants[0]:=0.0;
  fWaterPipeline.ColorBlendState.BlendConstants[1]:=0.0;
  fWaterPipeline.ColorBlendState.BlendConstants[2]:=0.0;
  fWaterPipeline.ColorBlendState.BlendConstants[3]:=0.0;
  fWaterPipeline.ColorBlendState.AddColorBlendAttachmentState(false,
                                                              VK_BLEND_FACTOR_ONE,
                                                              VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                              VK_BLEND_OP_ADD,
                                                              VK_BLEND_FACTOR_ONE,
                                                              VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                              VK_BLEND_OP_ADD,
                                                              TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                              TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                              TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                              TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

  fWaterPipeline.DepthStencilState.DepthTestEnable:=true;
  fWaterPipeline.DepthStencilState.DepthWriteEnable:=true;
  if TpvScene3DRendererInstance(fRendererInstance).ZFar<0.0 then begin
   fWaterPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_GREATER_OR_EQUAL;
  end else begin
   fWaterPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
  end;
  fWaterPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
  fWaterPipeline.DepthStencilState.StencilTestEnable:=false;

  fWaterPipeline.Initialize;

  fVulkanDevice.DebugUtils.SetObjectName(fWaterPipeline.Handle,VK_OBJECT_TYPE_PIPELINE,'TpvScene3DPlanet.TWaterRenderPass.fWaterPipeline');

 end;

end;

procedure TpvScene3DPlanet.TWaterRenderPass.ReleaseResources;
//var InFlightFrameIndex:TpvSizeInt;
begin

 FreeAndNil(fWaterPipeline);

 FreeAndNil(fUnderwaterPipeline);

 FreeAndNil(fPipelineLayout);

end;

procedure TpvScene3DPlanet.TWaterRenderPass.Draw(const aInFlightFrameIndex,aFrameIndex,aRenderPassIndex,aViewBaseIndex,aCountViews:TpvSizeInt;const aCommandBuffer:TpvVulkanCommandBuffer;const aPassDescriptorSet:TpvVulkanDescriptorSet);
var PlanetIndex:TpvSizeInt;
    Planet:TpvScene3DPlanet;
    First:Boolean;
    DescriptorSets:array[0..1] of TVkDescriptorSet;
    InFlightFrameState:TpvScene3DRendererInstance.PInFlightFrameState;
    RendererInstance:TpvScene3DPlanet.TRendererInstance;
    RendererViewInstance:TpvScene3DPlanet.TRendererViewInstance;
begin

 InFlightFrameState:=@TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex];

 TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.AcquireRead;
 try

  begin

   First:=true;

   for PlanetIndex:=0 to TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Count-1 do begin

    Planet:=TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Items[PlanetIndex];

    if Planet.fReady and Planet.fInFlightFrameReady[aInFlightFrameIndex] then begin

     {if Planet.fData.fVisible then}begin

      if First then begin

       First:=false;

       DescriptorSets[0]:=TpvScene3D(fScene3D).GlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle;
       DescriptorSets[1]:=aPassDescriptorSet.Handle;

       aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                            fPipelineLayout.Handle,
                                            0,
                                            2,
                                            @DescriptorSets,
                                            0,
                                            nil);

      end;

      if Planet.fRendererInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererInstance.TKey.Create(fRendererInstance),RendererInstance) and
         Planet.fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fRendererInstance,InFlightFrameState^.ViewRenderPassIndex),RendererViewInstance) then begin

       DescriptorSets[0]:=Planet.fDescriptorSets[aInFlightFrameIndex].Handle;
       DescriptorSets[1]:=RendererViewInstance.fWaterRenderDescriptorSets[aInFlightFrameIndex].Handle;

       aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                            fPipelineLayout.Handle,
                                            2,
                                            2,
                                            @DescriptorSets,
                                            0,
                                            nil);

       fPushConstants.ViewBaseIndex:=aViewBaseIndex;
       fPushConstants.CountViews:=aCountViews;
       fPushConstants.CountAllViews:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].CountViews;
       fPushConstants.CountQuadPointsInOneDirection:=64;

       fPushConstants.ResolutionXY:=(fWidth and $ffff) or ((fHeight and $ffff) shl 16);
       fPushConstants.TessellationFactor:=1.0/4.0;
 //    fPushConstants.Jitter:=TpvScene3DRendererInstance(fRendererInstance).InFlightFrameStates[aInFlightFrameIndex].Jitter.xy;
       fPushConstants.Jitter:=TpvVector2.Null;

       fPushConstants.FrameIndex:=aFrameIndex;
       fPushConstants.Time:=Modulo(TpvScene3D(Planet.Scene3D).SceneTimes^[aInFlightFrameIndex],65536.0);
       if TpvScene3D(fScene3D).UseBufferDeviceAddress then begin
        fPushConstants.PlanetData:=Planet.fPlanetDataVulkanBuffers[aInFlightFrameIndex].DeviceAddress;
       end else begin
        fPushConstants.PlanetData:=0;
       end;

       fPushConstants.TileMapResolution:=Planet.TileMapResolution;

       aCommandBuffer.CmdPushConstants(fPipelineLayout.Handle,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                                       TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                                       TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       0,
                                       SizeOf(TPushConstants),
                                       @fPushConstants);

       aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fUnderwaterPipeline.Handle);
       aCommandBuffer.CmdDraw(3,1,0,0);

       aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fWaterPipeline.Handle);
       aCommandBuffer.CmdDraw(fPushConstants.CountQuadPointsInOneDirection*fPushConstants.CountQuadPointsInOneDirection*4,1,0,0);

      end;

     end;

    end;

   end;

  end;

 finally
  TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.ReleaseRead;
 end;

end;

{ TpvScene3DPlanet.TRendererInstance.TKey }

constructor TpvScene3DPlanet.TRendererInstance.TKey.Create(const aRendererInstance:TObject);
begin
 fRendererInstance:=aRendererInstance;
end;

{ TpvScene3DPlanet.TRendererInstance }

constructor TpvScene3DPlanet.TRendererInstance.Create(const aPlanet:TpvScene3DPlanet;const aRendererInstance:TObject);
begin
 
 inherited Create;
 
 fPlanet:=aPlanet;
 
 fRendererInstance:=aRendererInstance;
 
 fKey:=TpvScene3DPlanet.TRendererInstance.TKey.Create(fRendererInstance);
 
 fMinimumLODLevel:=0;

end;

destructor TpvScene3DPlanet.TRendererInstance.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DPlanet.TRendererInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fPlanet) and assigned(fPlanet.fRendererInstanceListLock) then begin
  fPlanet.fRendererInstanceListLock.Acquire;
  try
   fPlanet.fRendererInstances.Add(self);
   fPlanet.fRendererInstanceHashMap.Add(fKey,self);
  finally
   fPlanet.fRendererInstanceListLock.Release;
  end;
 end;
end;

procedure TpvScene3DPlanet.TRendererInstance.BeforeDestruction;
begin
 if assigned(fPlanet) and assigned(fPlanet.fRendererInstanceListLock) then begin
  fPlanet.fRendererInstanceListLock.Acquire;
  try
   fPlanet.fRendererInstanceHashMap.Delete(fKey);
   fPlanet.fRendererInstances.RemoveWithoutFree(self);
  finally
   fPlanet.fRendererInstanceListLock.Release;
  end;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3DPlanet.TRendererViewInstance.TKey }

constructor TpvScene3DPlanet.TRendererViewInstance.TKey.Create(const aRendererInstance:TObject;const aRenderPassIndex:TpvSizeInt);
begin
 fRendererInstance:=aRendererInstance;
 fRenderPassIndex:=aRenderPassIndex;
end;

{ TpvScene3DPlanet.TRendererViewInstance }

constructor TpvScene3DPlanet.TRendererViewInstance.Create(const aPlanet:TpvScene3DPlanet;const aRendererInstance:TObject;const aRenderPassIndex:TpvSizeInt;const aMainViewPort:Boolean);
var InFlightFrameIndex,PreviousInFlightFrameIndex,Index:TpvSizeInt;
    GrassMetaData:TGrassMetaData;
begin
 inherited Create;

 fPlanet:=aPlanet;

 fRendererInstance:=aRendererInstance;

 fRenderPassIndex:=aRenderPassIndex;

 fKey:=TpvScene3DPlanet.TRendererViewInstance.TKey.Create(fRendererInstance,fRenderPassIndex);

 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  fVulkanVisiblityBuffers[InFlightFrameIndex]:=nil;
 end;

 for InFlightFrameIndex:=0 to TpvScene3DRendererInstance(fRendererInstance).Scene3D.CountInFlightFrames-1 do begin
  fVulkanVisiblityBuffers[InFlightFrameIndex]:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                                      (((fPlanet.TileMapResolution*fPlanet.TileMapResolution)+31) shr 5)*SizeOf(TpvUInt32),
                                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                      [],
                                                                      0,
                                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                      0,
                                                                      0,
                                                                      0,
                                                                      0,
                                                                      0,
                                                                      0,
                                                                      [],
                                                                      0,
                                                                      pvAllocationGroupIDScene3DPlanetStatic
                                                                     );
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanVisiblityBuffers[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.VisiblityBuffers['+IntToStr(InFlightFrameIndex)+']');
 end;

 fVulkanDrawIndexedIndirectCommandBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                                 ((fPlanet.TileMapResolution*fPlanet.TileMapResolution)+1)*(SizeOf(TpvUInt32)*16)*2,
                                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT) or
                                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                                                 TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                 [],
                                                                 0,
                                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 0,
                                                                 [],
                                                                 0,
                                                                 pvAllocationGroupIDScene3DPlanetStatic
                                                                );
 fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanDrawIndexedIndirectCommandBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.DrawIndexedIndirectCommandBuffer');

 fVulkanVisibleTileListBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                      ((fPlanet.TileMapResolution*fPlanet.TileMapResolution)+3)*SizeOf(TpvUInt32),
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT) or
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                      [],
                                                      0,
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      [],
                                                      0,
                                                      pvAllocationGroupIDScene3DPlanetStatic
                                                     );
 fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanVisibleTileListBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.VisibleTileListBuffer');
 
 if TpvScene3DRendererInstance(fRendererInstance).Scene3D.MeshShaderSupport then begin

  fVulkanGrassTaskIndicesBuffer:=nil;

  fVulkanGrassMetaDataBuffer:=nil;

  fVulkanGrassVerticesBuffer:=nil;

  fVulkanGrassIndicesBuffer:=nil;

 end else begin

  fVulkanGrassTaskIndicesBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                        IfThen(aMainViewPort,((fPlanet.fVisualResolution*fPlanet.fVisualResolution)+3)*SizeOf(TpvUInt32),3*SizeOf(TpvUInt32)),
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT),
                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                        [],
                                                        0,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        [],
                                                        0,
                                                        pvAllocationGroupIDScene3DPlanetStatic
                                                       );
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanGrassTaskIndicesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.GrassTaskIndicesBuffer');

  fVulkanGrassMetaDataBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                     SizeOf(TpvScene3DPlanet.TGrassMetaData),
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT) or
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     [],
                                                     0,
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     [],
                                                     0,
                                                     pvAllocationGroupIDScene3DPlanetStatic
                                                    );
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanGrassMetaDataBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.GrassMetaDataBuffer');

  GrassMetaData.DrawIndexedIndirectCommand.indexCount:=0;
  GrassMetaData.DrawIndexedIndirectCommand.instanceCount:=1;
  GrassMetaData.DrawIndexedIndirectCommand.firstIndex:=0;
  GrassMetaData.DrawIndexedIndirectCommand.vertexOffset:=0;
  GrassMetaData.DrawIndexedIndirectCommand.firstInstance:=0;
  GrassMetaData.CountVertices:=0;

  fPlanet.fVulkanDevice.MemoryStaging.Upload(fPlanet.fVulkanUniversalQueue,
                                             fPlanet.fVulkanUniversalCommandBuffer,
                                             fPlanet.fVulkanUniversalFence,
                                             GrassMetaData,
                                             fVulkanGrassMetaDataBuffer,
                                             0,
                                             SizeOf(TGrassMetaData));


  fVulkanGrassVerticesBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                     IfThen(aMainViewPort,fPlanet.fMaxGrassVertices*SizeOf(TpvScene3DPlanet.TGrassVertex),SizeOf(TpvScene3DPlanet.TGrassVertex)),
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     [],
                                                     0,
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     [],
                                                     0,
                                                     pvAllocationGroupIDScene3DPlanetStatic
                                                    );
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanGrassVerticesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.GrassVerticesBuffer');

  fVulkanGrassIndicesBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                    IfThen(aMainViewPort,fPlanet.fMaxGrassIndices*SizeOf(TpvUInt32),SizeOf(TpvUInt32)),
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                                    TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                    [],
                                                    0,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    [],
                                                    0,
                                                    pvAllocationGroupIDScene3DPlanetStatic
                                                   );
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanGrassIndicesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.GrassIndicesBuffer');

 end;

 fPlanetCullDescriptorPool:=TpvScene3DPlanet.CreatePlanetCullDescriptorPool(fPlanet.fVulkanDevice,
                                                                            TpvScene3DRendererInstance(fRendererInstance).Scene3D.CountInFlightFrames);

 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  fPlanetCullDescriptorSets[InFlightFrameIndex]:=nil;
 end;

 for InFlightFrameIndex:=0 to TpvScene3DRendererInstance(fRendererInstance).Scene3D.CountInFlightFrames-1 do begin

  PreviousInFlightFrameIndex:=InFlightFrameIndex-1;
  if PreviousInFlightFrameIndex<0 then begin
   PreviousInFlightFrameIndex:=TpvScene3DRendererInstance(fRendererInstance).Scene3D.CountInFlightFrames-1;
  end;

  begin

   fPlanetCullDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fPlanetCullDescriptorPool,TpvScene3D(fPlanet.Scene3D).PlanetCullDescriptorSetLayout);

   fPlanetCullDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fPlanet.fData.fTiledMeshBoundingBoxesBuffer.DescriptorBufferInfo],
                                                                      [],
                                                                      false);

   fPlanetCullDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fPlanet.fData.fTiledMeshBoundingSpheresBuffer.DescriptorBufferInfo],
                                                                      [],
                                                                      false);

   fPlanetCullDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(2,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fVulkanVisiblityBuffers[PreviousInFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);

   fPlanetCullDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(3,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fVulkanVisiblityBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                      [],
                                                                      false);

   fPlanetCullDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(4,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fVulkanDrawIndexedIndirectCommandBuffer.DescriptorBufferInfo],
                                                                      [],
                                                                      false);

   fPlanetCullDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(5,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fPlanet.fData.fTiledVisualMeshIndexGroupsBuffer.DescriptorBufferInfo],
                                                                      [],
                                                                      false);

   fPlanetCullDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(6,
                                                                      0,
                                                                      1,
                                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                      [],
                                                                      [fVulkanVisibleTileListBuffer.DescriptorBufferInfo],
                                                                      [],
                                                                      false);

   fPlanetCullDescriptorSets[InFlightFrameIndex].Flush;

   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPlanetCullDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TRendererViewInstance.fPlanetCullDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

  end;

 end;

 fGrassCullDescriptorPool:=TpvScene3DPlanet.CreatePlanetGrassCullAndMeshGenerationDescriptorPool(fPlanet.fVulkanDevice,
                                                                                                 TpvScene3DRendererInstance(fRendererInstance).Scene3D.MeshShaderSupport,
                                                                                                 TpvScene3DRendererInstance(fRendererInstance).Scene3D.CountInFlightFrames);

 for Index:=0 to 1 do begin

  begin

   fGrassCullDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fGrassCullDescriptorPool,TpvScene3D(fPlanet.Scene3D).PlanetGrassCullAndMeshGenerationDescriptorSetLayout);

   fGrassCullDescriptorSets[Index].WriteToDescriptorSet(0,
                                                        0,
                                                        1,
                                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                        [],
                                                        [fVulkanVisibleTileListBuffer.DescriptorBufferInfo],
                                                        [],
                                                        false);

   fGrassCullDescriptorSets[Index].WriteToDescriptorSet(1,
                                                        0,
                                                        1,
                                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                        [],
                                                        [fPlanet.fData.fVisualMeshVertexBuffers[Index].DescriptorBufferInfo],
                                                        [],
                                                        false);

   fGrassCullDescriptorSets[Index].WriteToDescriptorSet(2,
                                                        0,
                                                        1,
                                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                        [],
                                                        [fPlanet.fData.fVisualMeshDistanceBuffers[Index].DescriptorBufferInfo],
                                                        [],
                                                        false);

   if not TpvScene3D(fPlanet.fScene3D).MeshShaderSupport then begin

    fGrassCullDescriptorSets[Index].WriteToDescriptorSet(3,
                                                         0,
                                                         1,
                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                         [],
                                                         [fVulkanGrassTaskIndicesBuffer.DescriptorBufferInfo],
                                                         [],
                                                         false);

    fGrassCullDescriptorSets[Index].WriteToDescriptorSet(4,
                                                         0,
                                                         1,
                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                         [],
                                                         [fVulkanGrassMetaDataBuffer.DescriptorBufferInfo],
                                                         [],
                                                         false);

    fGrassCullDescriptorSets[Index].WriteToDescriptorSet(5,
                                                         0,
                                                         1,
                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                         [],
                                                         [fVulkanGrassVerticesBuffer.DescriptorBufferInfo],
                                                         [],
                                                         false);

    fGrassCullDescriptorSets[Index].WriteToDescriptorSet(6,
                                                         0,
                                                         1,
                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                         [],
                                                         [fVulkanGrassIndicesBuffer.DescriptorBufferInfo],
                                                         [],
                                                         false);

   end;

   fGrassCullDescriptorSets[Index].Flush;

   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fGrassCullDescriptorSets[Index].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TRendererViewInstance.fGrassCullDescriptorSets['+IntToStr(Index)+']');

  end;

 end;

 if aMainViewPort then begin

  fVulkanWaterAccelerationImage:=TpvScene3DRendererArray2DImage.Create(fPlanet.fVulkanDevice,
                                                                       256,
                                                                       256,
                                                                       TpvScene3DRendererInstance(fRendererInstance).CountSurfaceViews,
                                                                       VK_FORMAT_R32_SFLOAT,
                                                                       TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT),
                                                                       VK_IMAGE_LAYOUT_GENERAL,
                                                                       true,
                                                                       pvAllocationGroupIDScene3DPlanetStatic,
                                                                       VK_FORMAT_R32_UINT);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanWaterAccelerationImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.WaterAccelerationImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanWaterAccelerationImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.WaterAccelerationImage.ImageView');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanWaterAccelerationImage.VulkanArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.WaterAccelerationImage.ArrayImageView');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVulkanWaterAccelerationImage.VulkanOtherArrayImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.WaterAccelerationImage.OtherArrayImageView');

  fWaterPrepassDescriptorPool:=TpvScene3DPlanet.CreatePlanetWaterPrepassDescriptorPool(fPlanet.fVulkanDevice,1);

  fWaterPrepassDescriptorSet:=TpvVulkanDescriptorSet.Create(fWaterPrepassDescriptorPool,TpvScene3D(fPlanet.Scene3D).PlanetWaterPrepassDescriptorSetLayout);

  fWaterPrepassDescriptorSet.WriteToDescriptorSet(0,
                                                  0,
                                                  1,
                                                  TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                                  [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                                 fVulkanWaterAccelerationImage.VulkanOtherArrayImageView.Handle,
                                                                                 VK_IMAGE_LAYOUT_GENERAL)],
                                                  [],
                                                  [],
                                                  false);

  fWaterPrepassDescriptorSet.Flush;

  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fWaterPrepassDescriptorSet.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TRendererViewInstance.fWaterPrepassDescriptorSet');

  fWaterRenderDescriptorPool:=TpvScene3DPlanet.CreatePlanetWaterRenderDescriptorPool(fPlanet.fVulkanDevice,TpvScene3D(fPlanet.Scene3D).CountInFlightFrames);

  for InFlightFrameIndex:=0 to TpvScene3DRendererInstance(fRendererInstance).Scene3D.CountInFlightFrames-1 do begin

   fWaterRenderDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fWaterRenderDescriptorPool,TpvScene3D(fPlanet.Scene3D).PlanetWaterRenderDescriptorSetLayout);

   fWaterRenderDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                       [],
                                                                       [fVulkanVisiblityBuffers[InFlightFrameIndex].DescriptorBufferInfo],
                                                                       [],
                                                                       false);

   fWaterRenderDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [TVkDescriptorImageInfo.Create(TpvScene3DRendererInstance(fRendererInstance).Renderer.ClampedNearestSampler.Handle,
                                                                                                      fVulkanWaterAccelerationImage.VulkanArrayImageView.Handle,
                                                                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)],
                                                                       [],
                                                                       [],
                                                                       false);

   fWaterRenderDescriptorSets[InFlightFrameIndex].Flush;

   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fWaterRenderDescriptorSets[InFlightFrameIndex].Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3DPlanet.TRendererViewInstance.fWaterRenderDescriptorSets['+IntToStr(InFlightFrameIndex)+']');

  end;

 end else begin

  fVulkanWaterAccelerationImage:=nil;

  fWaterPrepassDescriptorPool:=nil;

  fWaterPrepassDescriptorSet:=nil;

  fWaterRenderDescriptorPool:=nil;

 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  fWaterRenderDescriptorSets[InFlightFrameIndex]:=nil;
 end;

 end;

end;

destructor TpvScene3DPlanet.TRendererViewInstance.Destroy;
var InFlightFrameIndex,Index:TpvSizeInt;
begin
 FreeAndNil(fWaterPrepassDescriptorSet);
 FreeAndNil(fWaterPrepassDescriptorPool);
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fWaterRenderDescriptorSets[InFlightFrameIndex]);
 end;
 FreeAndNil(fWaterRenderDescriptorPool);
 for Index:=0 to 1 do begin
  FreeAndNil(fGrassCullDescriptorSets[Index]);
 end;
 FreeAndNil(fGrassCullDescriptorPool);
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fPlanetCullDescriptorSets[InFlightFrameIndex]);
 end;
 FreeAndNil(fPlanetCullDescriptorPool);
 for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fVulkanVisiblityBuffers[InFlightFrameIndex]);
 end;
 FreeAndNil(fVulkanDrawIndexedIndirectCommandBuffer);
 FreeAndNil(fVulkanVisibleTileListBuffer);
 FreeAndNil(fVulkanGrassTaskIndicesBuffer);
 FreeAndNil(fVulkanGrassMetaDataBuffer);
 FreeAndNil(fVulkanGrassVerticesBuffer);
 FreeAndNil(fVulkanGrassIndicesBuffer);
 FreeAndNil(fVulkanWaterAccelerationImage);
 inherited Destroy;
end;

procedure TpvScene3DPlanet.TRendererViewInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fPlanet) and assigned(fPlanet.fRendererViewInstanceListLock) then begin
  fPlanet.fRendererViewInstanceListLock.Acquire;
  try
   fPlanet.fRendererViewInstances.Add(self);
   fPlanet.fRendererViewInstanceHashMap.Add(fKey,self);
  finally
   fPlanet.fRendererViewInstanceListLock.Release;
  end;
 end;
end;

procedure TpvScene3DPlanet.TRendererViewInstance.BeforeDestruction;
begin
 if assigned(fPlanet) and assigned(fPlanet.fRendererViewInstanceListLock) then begin
  fPlanet.fRendererViewInstanceListLock.Acquire;
  try
   fPlanet.fRendererViewInstanceHashMap.Delete(fKey);
   fPlanet.fRendererViewInstances.RemoveWithoutFree(self);
  finally
   fPlanet.fRendererViewInstanceListLock.Release;
  end;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3DPlanet.TRendererViewInstanceList }

{ TpvScene3DPlanet }

constructor TpvScene3DPlanet.Create(const aScene3D:TObject;
                                    const aHeightMapResolution:TpvInt32;
                                    const aVisualResolution:TpvSizeInt;
                                    const aPhysicsResolution:TpvSizeInt;
                                    const aBottomRadius:TpvFloat;
                                    const aTopRadius:TpvFloat);
var InFlightFrameIndex,Index,Resolution:TpvSizeInt;
//  ta,tb:TpvHighResolutionTime;
begin

 inherited Create;

 fScene3D:=aScene3D;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 fHeightMapResolution:=RoundUpToPowerOfTwo(Min(Max(aHeightMapResolution,128),8192));

 fGrassMapResolution:=fHeightMapResolution;

 fWaterMapResolution:=fHeightMapResolution;

 fTileMapResolution:=Min(Max(fHeightMapResolution shr 8,32),fHeightMapResolution);

 fTileMapShift:=IntLog2(fHeightMapResolution)-IntLog2(fTileMapResolution);

 fTileMapBits:=IntLog2(fTileMapResolution);

 fVisualTileResolution:=Max(1,RoundUpToPowerOfTwo(Min(aVisualResolution,fHeightMapResolution)) div fTileMapResolution);

 fPhysicsTileResolution:=Max(1,RoundUpToPowerOfTwo(Min(aPhysicsResolution,fHeightMapResolution)) div fTileMapResolution);

 fVisualResolution:=fTileMapResolution*fVisualTileResolution;

 fPhysicsResolution:=fTileMapResolution*fPhysicsTileResolution;

{fMaxGrassVertices:=Max(65536,((fVisualResolution*fVisualResolution)+15) shr 3)*(4*2);

 fMaxGrassIndices:=Max(65536,((fVisualResolution*fVisualResolution)+15) shr 2)*((4*2)-2);}

 fMaxGrassVertices:=Max(65536,(512 shl 20) div SizeOf(TpvScene3DPlanet.TGrassVertex));

 fMaxGrassIndices:=Max(65536,(256 shl 20) div SizeOf(TpvUInt32));

 fTiledVisualMeshIndices:=TMeshIndices.Create;

 fTiledVisualMeshIndexGroups:=TTiledMeshIndexGroups.Create;

 fTiledPhysicsMeshIndices:=TMeshIndices.Create;

 fTiledPhysicsMeshIndexGroups:=TTiledMeshIndexGroups.Create;

//ta:=pvApplication.HighResolutionTimer.GetTime;
 GenerateMeshIndices(fTiledVisualMeshIndices,
                     fTiledVisualMeshIndexGroups,
                     fVisualTileResolution,
                     fTileMapResolution,
                     fCountVisualMeshIndices,
                     fCountVisualMeshLODLevels,
                     fVisualMeshLODOffsets,
                     fVisualMeshLODCounts);
{tb:=pvApplication.HighResolutionTimer.GetTime;
 writeln(pvApplication.HighResolutionTimer.ToFloatSeconds(tb-ta)*1000.0:7:4,'ms');}

//ta:=pvApplication.HighResolutionTimer.GetTime;
 GenerateMeshIndices(fTiledPhysicsMeshIndices,
                     fTiledPhysicsMeshIndexGroups,
                     fPhysicsTileResolution,
                     fTileMapResolution,
                     fCountPhysicsMeshIndices,
                     fCountPhysicsMeshLODLevels,
                     fPhysicsMeshLODOffsets,
                     fPhysicsMeshLODCounts);
{tb:=pvApplication.HighResolutionTimer.GetTime;
 writeln(pvApplication.HighResolutionTimer.ToFloatSeconds(tb-ta)*1000.0:7:4,'ms');}

{fCountVisualMeshIndices:=0;
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
 fCountPhysicsMeshIndices:=fCountPhysicsMeshIndices*((fTileMapResolution*fTileMapResolution)*6); }

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

 fGrassMapInitialization:=TGrassMapInitialization.Create(self);

 fGrassMapModification:=TGrassMapModification.Create(self);

 fWaterMapInitialization:=TWaterMapInitialization.Create(self);

 fWaterMapModification:=TWaterMapModification.Create(self);

 fHeightMapRandomInitialization:=THeightMapRandomInitialization.Create(self);

 fHeightMapModification:=THeightMapModification.Create(self);

 fHeightMapFlatten:=THeightMapFlatten.Create(self);

 fTiledMeshBoundingVolumesGeneration:=TTiledMeshBoundingVolumesGeneration.Create(self);

 fTileDirtyExpansion:=TTileDirtyExpansion.Create(self);

 fTileDirtyQueueGeneration:=TTileDirtyQueueGeneration.Create(self);

 fNormalMapGeneration:=TNormalMapGeneration.Create(self);

 fHeightMapMipMapGeneration:=THeightMapMipMapGeneration.Create(self);

 fNormalMapMipMapGeneration:=TNormalMapMipMapGeneration.Create(self);

{fVisualMeshIndexGeneration:=TMeshIndexGeneration.Create(self,false);

 fPhysicsMeshIndexGeneration:=TMeshIndexGeneration.Create(self,true);}

 fVisualMeshVertexGeneration:=TMeshVertexGeneration.Create(self,false);

 fPhysicsMeshVertexGeneration:=TMeshVertexGeneration.Create(self,true);

 fVisualMeshDistanceGeneration:=TMeshDistanceGeneration.Create(self);

 fWaterCullPass:=TWaterCullPass.Create(self);

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
                                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                                        IfThen(TpvScene3D(fScene3D).UseBufferDeviceAddress,TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),0),
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
                                                                        [TpvVulkanBufferFlag.PersistentMappedIfPossibe],
                                                                        0,
                                                                        pvAllocationGroupIDScene3DPlanetStatic
                                                                       );
  end; 

  fDescriptorPool:=TpvScene3DPlanet.CreatePlanetDescriptorPool(fVulkanDevice,TpvScene3D(fScene3D).CountInFlightFrames);

  for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
   
   fDescriptorSets[InFlightFrameIndex]:=TpvVulkanDescriptorSet.Create(fDescriptorPool,TpvScene3D(fScene3D).PlanetDescriptorSetLayout);
   fDescriptorSets[InFlightFrameIndex].WriteToDescriptorSet(0,
                                                            0,
                                                            5,
                                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                            [TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fHeightMapImage.VulkanImageView.Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                             TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fNormalMapImage.VulkanImageView.Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                             TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fBlendMapImage.VulkanImageView.Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                             TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fGrassMapImage.VulkanImageView.Handle,
                                                                                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                                                             TVkDescriptorImageInfo.Create(TpvScene3D(fScene3D).GeneralComputeSampler.Handle,
                                                                                           fInFlightFrameDataList[InFlightFrameIndex].fWaterMapImage.VulkanImageView.Handle,
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

 fRendererInstanceListLock:=TPasMPCriticalSection.Create;

 fRendererInstances:=TRendererInstances.Create(false);

 fRendererInstanceHashMap:=TRendererInstanceHashMap.Create(nil);

 fRendererViewInstanceListLock:=TPasMPCriticalSection.Create;

 fRendererViewInstances:=TRendererViewInstances.Create(false);

 fRendererViewInstanceHashMap:=TRendererViewInstanceHashMap.Create(nil);

 if assigned(fVulkanDevice) and TpvScene3D(fScene3D).RaytracingActive then begin

  fRaytracingLock:=TPasMPCriticalSection.Create;

  fRaytracingTiles:=TRaytracingTiles.Create(true);

  for Index:=0 to (fTileMapResolution*fTileMapResolution)-1 do begin
   fRaytracingTiles.Add(TRaytracingTile.Create(self,Index));
  end;

  for Index:=0 to 1 do begin
   fRaytracingTileQueues[Index]:=TRaytracingTiles.Create(false);
  end;

  fRaytracingTileQueueUpdateIndex:=0;

  fRaytracingTileQueue:=fRaytracingTileQueues[1];

  fRaytracingTileNextQueue:=fRaytracingTileQueues[1];

 end else begin

  fRaytracingLock:=nil;

  fRaytracingTiles:=nil;

  for Index:=0 to 1 do begin
   fRaytracingTileQueues[Index]:=nil;
  end;

  fRaytracingTileQueueUpdateIndex:=0;

  fRaytracingTileQueue:=nil;

  fRaytracingTileNextQueue:=nil;

 end;

 fReady:=true;

end;

destructor TpvScene3DPlanet.Destroy;
var InFlightFrameIndex,Index:TpvSizeInt;
begin

 if assigned(fRaytracingTiles) then begin
  try
   if assigned(pvApplication) then begin
    pvApplication.WaitForPreviousFrame(true);
   end;
  finally
   FreeAndNil(fRaytracingTiles);
  end;
 end;

 fRaytracingTileQueue:=nil;

 fRaytracingTileNextQueue:=nil;

 for Index:=0 to 1 do begin
  FreeAndNil(fRaytracingTileQueues[Index]);
 end;

 FreeAndNil(fRaytracingLock);

 fRendererViewInstanceListLock.Acquire;
 try
  while fRendererViewInstances.Count>0 do begin
   fRendererViewInstances[fRendererViewInstances.Count-1].Free;
  end;
 finally
  fRendererViewInstanceListLock.Release;
 end;

 FreeAndNil(fRendererViewInstanceListLock);

 FreeAndNil(fRendererViewInstances);

 FreeAndNil(fRendererViewInstanceHashMap);

 fRendererInstanceListLock.Acquire;
 try
  while fRendererInstances.Count>0 do begin
   fRendererInstances[fRendererInstances.Count-1].Free;
  end;
 finally
  fRendererInstanceListLock.Release;
 end;

 FreeAndNil(fRendererInstanceListLock);

 FreeAndNil(fRendererInstances);

 FreeAndNil(fRendererInstanceHashMap);

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fDescriptorSets[InFlightFrameIndex]);
 end;

 FreeAndNil(fDescriptorPool);

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  FreeAndNil(fPlanetDataVulkanBuffers[InFlightFrameIndex]);
 end;
  
 FreeAndNil(fTiledVisualMeshIndices);

 FreeAndNil(fTiledVisualMeshIndexGroups);

 FreeAndNil(fTiledPhysicsMeshIndices);

 FreeAndNil(fTiledPhysicsMeshIndexGroups);

 FreeAndNil(fRayIntersection);

 FreeAndNil(fVulkanMemoryStagingQueue);

 FreeAndNil(fPhysicsMeshVertexGeneration);
 
 FreeAndNil(fVisualMeshVertexGeneration);

{FreeAndNil(fPhysicsMeshIndexGeneration);

 FreeAndNil(fVisualMeshIndexGeneration); }

 FreeAndNil(fVisualMeshDistanceGeneration);

 FreeAndNil(fWaterCullPass);

 FreeAndNil(fNormalMapMipMapGeneration);

 FreeAndNil(fHeightMapMipMapGeneration);
 
 FreeAndNil(fNormalMapGeneration);

 FreeAndNil(fTiledMeshBoundingVolumesGeneration);

 FreeAndNil(fTileDirtyExpansion);

 FreeAndNil(fTileDirtyQueueGeneration);

 FreeAndNil(fHeightMapFlatten); 

 FreeAndNil(fHeightMapModification);

 FreeAndNil(fHeightMapRandomInitialization);

 FreeAndNil(fGrassMapInitialization);

 FreeAndNil(fGrassMapModification);

 FreeAndNil(fWaterMapInitialization);

 FreeAndNil(fWaterMapModification);

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
  TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.AcquireWrite;
  try  
   TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Add(self); 
  finally 
   TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.ReleaseWrite;
  end; 
 end;
end;

procedure TpvScene3DPlanet.BeforeDestruction;
var Index:TpvSizeInt;
begin
 if assigned(fScene3D) then begin
  TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.AcquireWrite;
  try  
   Index:=TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).IndexOf(self);
   if Index>=0 then begin
    TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Extract(Index); // not delete or remove, since we don't want to free ourself here already.
   end;
  finally 
   TpvScene3DPlanets(TpvScene3D(fScene3D).Planets).Lock.ReleaseWrite;
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

procedure TpvScene3DPlanet.GenerateMeshIndices(const aTiledMeshIndices:TpvScene3DPlanet.TMeshIndices;
                                               const aTiledMeshIndexGroups:TpvScene3DPlanet.TTiledMeshIndexGroups;
                                               const aTileResolution:TpvInt32;
                                               const aTileMapResolution:TpvInt32;
                                               out aCountMeshIndices:TpvSizeInt;
                                               out aCountMeshLODLevels:TpvSizeInt;
                                               out aMeshLODOffsets:TpvScene3DPlanet.TSizeIntArray;
                                               out aMeshLODCounts:TpvScene3DPlanet.TSizeIntArray);
var TotalResolution,TotalResolutionMask,TotalResolutionBits,
    {TileResolutionMask,}TileResolutionBits,
    TileVertexSize:TpvInt32;
    CountIndices:TpvUInt32;
    LODIndex:TpvSizeInt;
    TileLODResolution,TileLODResolutionPlusBorder,
    TileMapX,TileMapY,TileLODX,TileLODY,TileX,TileY,GlobalX,GlobalY,Start:TpvInt32;
    v0,v1,v2,v3:TpvUInt32;
    TiledMeshIndexGroup:PTiledMeshIndexGroup;
    TileVertices:TpvUInt32DynamicArray;
    x,y,TileQuadMapX,TileQuadMapY,TileQuadX,TileQuadY:TpvInt32;
begin

 TotalResolution:=aTileResolution*aTileMapResolution;

 TotalResolutionMask:=TotalResolution-1;

 TotalResolutionBits:=IntLog2(TotalResolution);

 //TileResolutionMask:=aTileResolution-1;

 TileResolutionBits:=IntLog2(aTileResolution);

 TileVertexSize:=aTileResolution*aTileResolution;

 aCountMeshIndices:=0;

 aCountMeshLODLevels:=Max(1,IntLog2(aTileMapResolution));

 aMeshLODOffsets:=nil;
 SetLength(aMeshLODOffsets,aCountMeshLODLevels);

 aMeshLODCounts:=nil;
 SetLength(aMeshLODCounts,aCountMeshLODLevels);

 begin
  aTiledMeshIndices.Clear;
  CountIndices:=0;
  for LODIndex:=0 to aCountMeshLODLevels-1 do begin
   TileLODResolution:=aTileResolution shr LODIndex;
   inc(CountIndices,aTileMapResolution*aTileMapResolution*(TileLODResolution+1)*(TileLODResolution+1)*6);
  end;
  aTiledMeshIndices.Reserve(CountIndices);
 end;

 aTiledMeshIndexGroups.Clear;
 aTiledMeshIndexGroups.Reserve(aTileMapResolution*aTileMapResolution*aCountMeshLODLevels);

 CountIndices:=0;

 TileVertices:=nil;
 try

  SetLength(TileVertices,(aTileResolution+2)*(aTileResolution+2));

  for LODIndex:=0 to aCountMeshLODLevels-1 do begin

   TileLODResolution:=aTileResolution shr LODIndex;

   TileLODResolutionPlusBorder:=TileLODResolution+2;

   aMeshLODOffsets[LODIndex]:=CountIndices;

   for TileMapY:=0 to aTileMapResolution-1 do begin

    for TileMapX:=0 to aTileMapResolution-1 do begin

     TiledMeshIndexGroup:=Pointer(aTiledMeshIndexGroups.AddNew);
     TiledMeshIndexGroup^.FirstIndex:=CountIndices;

     if (TileMapX=0) or (TileMapY=0) then begin
      Start:=-1;
     end else begin
      Start:=0;
     end;

     if (TotalResolution and TotalResolutionMask)<>0 then begin
      for TileLODY:=Start to (TileLODResolution+1)-1 do begin
       TileY:=TileLODY shl LODIndex;
       for TileLODX:=Start to (TileLODResolution+1)-1 do begin
        TileX:=TileLODX shl LODIndex;
        GlobalX:=(TileMapX*aTileResolution)+TileX;
        GlobalY:=(TileMapY*aTileResolution)+TileY;
        x:=((GlobalX mod TotalResolution)+TotalResolution) mod TotalResolution;
        y:=((GlobalY mod TotalResolution)+TotalResolution) mod TotalResolution;
        if ((((abs(GlobalX) div TotalResolution)+IfThen(GlobalX<0,1,0)) xor ((abs(GlobalY) div TotalResolution)+IfThen(GlobalY<0,1,0))) and 1)<>0 then begin
         x:=(TotalResolution-(x+1)) mod TotalResolution;
         y:=(TotalResolution-(y+1)) mod TotalResolution;
        end;
        TileQuadMapX:=x div aTileResolution;
        TileQuadMapY:=y div aTileResolution;
        TileQuadX:=x-(TileQuadMapX*aTileResolution);
        TileQuadY:=y-(TileQuadMapY*aTileResolution);
        TileVertices[((TileLODY+1)*TileLODResolutionPlusBorder)+(TileLODX+1)]:=(((TileQuadMapY*fTileMapResolution)+TileQuadMapX)*TileVertexSize)+((TileQuadY*aTileResolution)+TileQuadX);
       end;
      end;
     end else begin
      for TileLODY:=Start to (TileLODResolution+1)-1 do begin
       TileY:=TileLODY shl LODIndex;
       for TileLODX:=Start to (TileLODResolution+1)-1 do begin
        TileX:=TileLODX shl LODIndex;
        GlobalX:=(TileMapX*aTileResolution)+TileX;
        GlobalY:=(TileMapY*aTileResolution)+TileY;
        x:=(GlobalX+TotalResolution) and TotalResolutionMask;
        y:=(GlobalY+TotalResolution) and TotalResolutionMask;
        if ((((abs(GlobalX) shr TotalResolutionBits)+IfThen(GlobalX<0,1,0)) xor ((abs(GlobalY) shr TotalResolutionBits)+IfThen(GlobalY<0,1,0))) and 1)<>0 then begin
         x:=(TotalResolution-(x+1)) and TotalResolutionMask;
         y:=(TotalResolution-(y+1)) and TotalResolutionMask;
        end;
        TileQuadMapX:=x shr TileResolutionBits;
        TileQuadMapY:=y shr TileResolutionBits;
        TileQuadX:=x-(TileQuadMapX*aTileResolution);
        TileQuadY:=y-(TileQuadMapY*aTileResolution);
        TileVertices[((TileLODY+1)*TileLODResolutionPlusBorder)+(TileLODX+1)]:=(((TileQuadMapY*fTileMapResolution)+TileQuadMapX)*TileVertexSize)+((TileQuadY*aTileResolution)+TileQuadX);
       end;
      end;
     end;

     for TileLODY:=0 to TileLODResolution-1 do begin

      TileY:=TileLODY shl LODIndex;

      for TileLODX:=0 to TileLODResolution-1 do begin

       TileX:=TileLODX shl LODIndex;

       GlobalX:=(TileMapX*aTileResolution)+TileX;
       GlobalY:=(TileMapY*aTileResolution)+TileY;

       if (GlobalX=0) or (GlobalY=0) then begin
        v0:=TileVertices[((TileLODY+0)*TileLODResolutionPlusBorder)+(TileLODX+0)]; // -1 -1
        v1:=TileVertices[((TileLODY+0)*TileLODResolutionPlusBorder)+(TileLODX+1)]; //  0 -1
        v2:=TileVertices[((TileLODY+1)*TileLODResolutionPlusBorder)+(TileLODX+1)]; //  0  0
        v3:=TileVertices[((TileLODY+1)*TileLODResolutionPlusBorder)+(TileLODX+0)]; // -1  0
        if (v0<>v1) and (v0<>v2) and (v1<>v2) then begin
         aTiledMeshIndices.ItemArray[CountIndices+0]:=v0;
         aTiledMeshIndices.ItemArray[CountIndices+1]:=v1;
         aTiledMeshIndices.ItemArray[CountIndices+2]:=v2;
         inc(CountIndices,3);
        end;
        if (v0<>v2) and (v0<>v3) and (v2<>v3) then begin
         aTiledMeshIndices.ItemArray[CountIndices+0]:=v0;
         aTiledMeshIndices.ItemArray[CountIndices+1]:=v2;
         aTiledMeshIndices.ItemArray[CountIndices+2]:=v3;
         inc(CountIndices,3);
        end;
       end;//}

       begin
        v0:=TileVertices[((TileLODY+1)*TileLODResolutionPlusBorder)+(TileLODX+1)]; //  0  0
        v1:=TileVertices[((TileLODY+1)*TileLODResolutionPlusBorder)+(TileLODX+2)]; //  1  0
        v2:=TileVertices[((TileLODY+2)*TileLODResolutionPlusBorder)+(TileLODX+2)]; //  1  1
        v3:=TileVertices[((TileLODY+2)*TileLODResolutionPlusBorder)+(TileLODX+1)]; //  0  1
        if (v0<>v1) and (v0<>v2) and (v1<>v2) then begin
         aTiledMeshIndices.ItemArray[CountIndices+0]:=v0;
         aTiledMeshIndices.ItemArray[CountIndices+1]:=v1;
         aTiledMeshIndices.ItemArray[CountIndices+2]:=v2;
         inc(CountIndices,3);
        end;
        if (v0<>v2) and (v0<>v3) and (v2<>v3) then begin
         aTiledMeshIndices.ItemArray[CountIndices+0]:=v0;
         aTiledMeshIndices.ItemArray[CountIndices+1]:=v2;
         aTiledMeshIndices.ItemArray[CountIndices+2]:=v3;
         inc(CountIndices,3);
        end;
       end;

      end;

     end;

     TiledMeshIndexGroup^.CountIndices:=CountIndices-TiledMeshIndexGroup^.FirstIndex;

    end;

   end;

   aMeshLODCounts[LODIndex]:=CountIndices-aMeshLODOffsets[LODIndex];

  end;

 finally
  TileVertices:=nil;
 end;

 aTiledMeshIndices.Count:=CountIndices;

 aTiledMeshIndices.Finish;

 aTiledMeshIndexGroups.Finish;

 aCountMeshIndices:=CountIndices;

end;

class function TpvScene3DPlanet.CreatePlanetDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice;const aMeshShaders:Boolean):TpvVulkanDescriptorSetLayout;
var ShaderStageFlags:TVkShaderStageFlags;
begin

 ShaderStageFlags:=TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT) or
                   IfThen(aMeshShaders and
                          (aVulkanDevice.EnabledExtensionNames.IndexOf(VK_EXT_MESH_SHADER_EXTENSION_NAME)>0) and
                          (aVulkanDevice.PhysicalDevice.MeshShaderFeaturesEXT.meshShader<>VK_FALSE) and
                          (aVulkanDevice.PhysicalDevice.MeshShaderFeaturesEXT.taskShader<>VK_FALSE){and
                          (aVulkanDevice.PhysicalDevice.MeshShaderFeaturesEXT.multiviewMeshShader<>VK_FALSE)},
                          TVkShaderStageFlags(VK_SHADER_STAGE_MESH_BIT_EXT) or
                          TVkShaderStageFlags(VK_SHADER_STAGE_TASK_BIT_EXT),
                          0);

 result:=TpvVulkanDescriptorSetLayout.Create(aVulkanDevice);

 // Height map + normal map + blend map + grass map + water map
 result.AddBinding(0,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                   5,
                   ShaderStageFlags,
                   [],
                   0);

 // Planet data
 result.AddBinding(1,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   ShaderStageFlags,
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
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),5*aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),3*aCountInFlightFrames);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.PlanetDescriptorPool');
end;

class function TpvScene3DPlanet.CreatePlanetCullDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout;
begin
 result:=TpvVulkanDescriptorSetLayout.Create(aVulkanDevice);
 result.AddBinding(0,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(1,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(2,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(3,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(4,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(5,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(6,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DPlanet.PlanetCullDescriptorSetLayout');
end;

class function TpvScene3DPlanet.CreatePlanetCullDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool;
begin
 result:=TpvVulkanDescriptorPool.Create(aVulkanDevice,
                                        TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                        aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),7*aCountInFlightFrames);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.PlanetCullDescriptorPool');
end;

class function TpvScene3DPlanet.CreatePlanetGrassCullAndMeshGenerationDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice;const aMeshShaders:Boolean):TpvVulkanDescriptorSetLayout;
var ShaderStageFlags:TvkShaderStageFlags;
begin
 result:=TpvVulkanDescriptorSetLayout.Create(aVulkanDevice);

 ShaderStageFlags:=IfThen(aMeshShaders and
                          (aVulkanDevice.EnabledExtensionNames.IndexOf(VK_EXT_MESH_SHADER_EXTENSION_NAME)>0) and
                          (aVulkanDevice.PhysicalDevice.MeshShaderFeaturesEXT.meshShader<>VK_FALSE) and
                          (aVulkanDevice.PhysicalDevice.MeshShaderFeaturesEXT.taskShader<>VK_FALSE){and
                          (aVulkanDevice.PhysicalDevice.MeshShaderFeaturesEXT.multiviewMeshShader<>VK_FALSE)},
                          TVkShaderStageFlags(VK_SHADER_STAGE_MESH_BIT_EXT) or
                          TVkShaderStageFlags(VK_SHADER_STAGE_TASK_BIT_EXT),
                          TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT)) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT);

 // VisibleTileList
 result.AddBinding(0,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   ShaderStageFlags,
                   [],
                   0);

 // VisualMeshVertices
 result.AddBinding(1,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   ShaderStageFlags,
                   [],
                   0);

 // VisualMeshDistances
 result.AddBinding(2,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   ShaderStageFlags,
                   [],
                   0);
                   
 if not aMeshShaders then begin

  // GrassTaskIndices
  result.AddBinding(3,
                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                    1,
                    ShaderStageFlags,
                    [],
                    0);

  // GrassMetaData
  result.AddBinding(4,
                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                    1,
                    ShaderStageFlags,
                    [],
                    0);

  // GrassVertices
  result.AddBinding(5,
                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                    1,
                    ShaderStageFlags,
                    [],
                    0);

  // GrassIndices
  result.AddBinding(6,
                    TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                    1,
                    ShaderStageFlags,
                    [],
                    0);

 end;

 result.Initialize;

 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DPlanet.PlanetGrassCullMeshGenerationDescriptorSetLayout');

end;

class function TpvScene3DPlanet.CreatePlanetGrassCullAndMeshGenerationDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aMeshShaders:Boolean;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool;
begin
 result:=TpvVulkanDescriptorPool.Create(aVulkanDevice,
                                        TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                        aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),IfThen(aMeshShaders,7,3)*aCountInFlightFrames);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.PlanetGrassCullMeshGenerationDescriptorPool');
end;
       
class function TpvScene3DPlanet.CreatePlanetWaterCullDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout;
begin
 result:=TpvVulkanDescriptorSetLayout.Create(aVulkanDevice);
 result.AddBinding(0,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(1,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.AddBinding(2,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DPlanet.PlanetWaterCullDescriptorSetLayout');
end;

class function TpvScene3DPlanet.CreatePlanetWaterCullDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool;
begin
 result:=TpvVulkanDescriptorPool.Create(aVulkanDevice,
                                        TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                        aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),2*aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1*aCountInFlightFrames);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.PlanetWaterCullDescriptorPool');
end;

class function TpvScene3DPlanet.CreatePlanetWaterPrepassDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout;
begin
 result:=TpvVulkanDescriptorSetLayout.Create(aVulkanDevice);
 result.AddBinding(0,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                   [],
                   0);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DPlanet.PlanetWaterPrepassDescriptorSetLayout');
end;

class function TpvScene3DPlanet.CreatePlanetWaterPrepassDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool;
begin
 result:=TpvVulkanDescriptorPool.Create(aVulkanDevice,
                                        TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                        aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),aCountInFlightFrames);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.PlanetWaterPrepassDescriptorPool');
end;

class function TpvScene3DPlanet.CreatePlanetWaterRenderDescriptorSetLayout(const aVulkanDevice:TpvVulkanDevice):TpvVulkanDescriptorSetLayout;
begin
 result:=TpvVulkanDescriptorSetLayout.Create(aVulkanDevice);
 result.AddBinding(0,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                   [],
                   0);
 result.AddBinding(1,
                   TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                   1,
                   TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) or
                   TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                   [],
                   0);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3DPlanet.PlanetWaterWaterRenderDescriptorSetLayout');
end;

class function TpvScene3DPlanet.CreatePlanetWaterRenderDescriptorPool(const aVulkanDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt):TpvVulkanDescriptorPool;
begin
 result:=TpvVulkanDescriptorPool.Create(aVulkanDevice,
                                        TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                        aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),1*aCountInFlightFrames);
 result.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),1*aCountInFlightFrames);
 result.Initialize;
 aVulkanDevice.DebugUtils.SetObjectName(result.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3DPlanet.PlanetWaterRenderDescriptorPool');
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

procedure TpvScene3DPlanet.Initialize(const aPasMPInstance:TPasMP;const aData:TStream;const aDataFreeOnDestroy:boolean);
//var ui32:TVkUInt32;
var HeightMapDataInitialization:TpvScene3DPlanet.THeightMapDataInitialization;
begin

 if not fData.fInitialized then begin

  if assigned(fVulkanDevice) then begin

   if assigned(aData) then begin
    HeightMapDataInitialization:=TpvScene3DPlanet.THeightMapDataInitialization.Create(self,aData);
   end else begin
    HeightMapDataInitialization:=nil;
   end;
   try
     
    BeginUpdate;
    try

     if assigned(aData) then begin
      HeightMapDataInitialization.Execute(fVulkanComputeCommandBuffer);
     end else begin 
      fHeightMapRandomInitialization.Execute(fVulkanComputeCommandBuffer);
     end; 

    {fVisualMeshIndexGeneration.Execute(fVulkanComputeCommandBuffer);

     fPhysicsMeshIndexGeneration.Execute(fVulkanComputeCommandBuffer);}

     fTiledMeshBoundingVolumesGeneration.Execute(fVulkanComputeCommandBuffer);

     fGrassMapInitialization.Execute(fVulkanComputeCommandBuffer);

     fWaterMapInitialization.Execute(fVulkanComputeCommandBuffer);

    finally
     EndUpdate;
    end;

   finally
    try
     FreeAndNil(HeightMapDataInitialization);
    finally
     if aDataFreeOnDestroy then begin
      aData.Free;
     end; 
    end; 
   end;

   begin

    fVulkanDevice.MemoryStaging.Upload(fVulkanComputeQueue,
                                       fVulkanComputeCommandBuffer,
                                       fVulkanComputeFence,
                                       fTiledVisualMeshIndices.ItemArray[0],
                                       fData.fVisualMeshIndexBuffer,
                                       0,
                                       fTiledVisualMeshIndices.Count*SizeOf(TpvUInt32));

    fVulkanDevice.MemoryStaging.Upload(fVulkanComputeQueue,
                                       fVulkanComputeCommandBuffer,
                                       fVulkanComputeFence,
                                       fTiledVisualMeshIndexGroups.ItemArray[0],
                                       fData.fTiledVisualMeshIndexGroupsBuffer,
                                       0,
                                       fTiledVisualMeshIndexGroups.Count*SizeOf(TTiledMeshIndexGroup));

{   fVulkanDevice.MemoryStaging.Download(fVulkanComputeQueue,
                                         fVulkanComputeCommandBuffer,
                                         fVulkanComputeFence,
                                         fData.fPhysicsMeshIndexBuffer,
                                         0,
                                         fData.fMeshIndices.ItemArray[0],
                                         fTileMapResolution*fTileMapResolution*fPhysicsTileResolution*fPhysicsTileResolution*6*SizeOf(TpvUInt32));}

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
    UpdateRenderIndex,UpdateWaterVisibility:Boolean;
    RaytracingTile:TRaytracingTile;
    CurrentRaytracingTileQueue:TRaytracingTiles;
begin

 fData.fCountDirtyTiles:=0;

 UpdateRenderIndex:=false;

 UpdateWaterVisibility:=false;

 if fData.fModifyGrassMapActive then begin

  if assigned(fVulkanDevice) then begin

   BeginUpdate;
   try

    if fData.fModifyGrassMapActive then begin
     fGrassMapModification.Execute(fVulkanComputeCommandBuffer);
    end;

   finally
    EndUpdate;
   end;

  end;

 end;

if fData.fModifyWaterMapActive then begin

  if assigned(fVulkanDevice) then begin

   BeginUpdate;
   try

    if fData.fModifyWaterMapActive then begin
     fWaterMapModification.Execute(fVulkanComputeCommandBuffer);
     UpdateWaterVisibility:=true;
    end;

   finally
    EndUpdate;
   end;

  end;

 end;

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

     fVisualMeshDistanceGeneration.Execute(fVulkanComputeCommandBuffer);

     fPhysicsMeshVertexGeneration.Execute(fVulkanComputeCommandBuffer);

     fData.fVisualMeshVertexBufferNextRenderIndex:=fData.fVisualMeshVertexBufferUpdateIndex and 1;

     fData.fVisualMeshVertexBufferUpdateIndex:=(fData.fVisualMeshVertexBufferUpdateIndex+1) and 1;

     UpdateRenderIndex:=true;

     UpdateWaterVisibility:=true;

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

    fData.fVisualMeshVertexBufferCopies.ClearNoFree;

    fData.fVisualMeshDistanceBufferCopies.ClearNoFree;

    CurrentRaytracingTileQueue:=fRaytracingTileQueues[fRaytracingTileQueueUpdateIndex and 1];

    if assigned(fRaytracingTiles) and assigned(CurrentRaytracingTileQueue) then begin
     CurrentRaytracingTileQueue.ClearNoFree;
    end;

    for QueueTileIndex:=0 to TpvSizeInt(fData.fCountDirtyTiles)-1 do begin
     TileIndex:=fData.fTileDirtyQueueItems.ItemArray[QueueTileIndex];
     inc(fData.fTileGenerations[TileIndex]);
     if assigned(fRaytracingTiles) then begin
      RaytracingTile:=fRaytracingTiles[TileIndex];
      RaytracingTile.Generation:=fData.fTileGenerations[TileIndex];
      CurrentRaytracingTileQueue.Add(RaytracingTile);
     end;
     if UpdateRenderIndex and (fData.fCountDirtyTiles<>(fTileMapResolution*fTileMapResolution)) then begin
      fData.fVisualMeshVertexBufferCopies.Add(TVkBufferCopy.Create(TileIndex*fVisualTileResolution*fVisualTileResolution*SizeOf(TMeshVertex),
                                                                   TileIndex*fVisualTileResolution*fVisualTileResolution*SizeOf(TMeshVertex),
                                                                   fVisualTileResolution*fVisualTileResolution*SizeOf(TMeshVertex)));
      fData.fVisualMeshDistanceBufferCopies.Add(TVkBufferCopy.Create(TileIndex*fVisualTileResolution*fVisualTileResolution*SizeOf(TMeshDistance),
                                                                     TileIndex*fVisualTileResolution*fVisualTileResolution*SizeOf(TMeshDistance),
                                                                     fVisualTileResolution*fVisualTileResolution*SizeOf(TMeshDistance)));
     end;
    end;

    fRaytracingTileNextQueue:=CurrentRaytracingTileQueue;

    fRaytracingTileQueueUpdateIndex:=(fRaytracingTileQueueUpdateIndex+1) and 1;

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

   if UpdateWaterVisibility then begin
    BeginUpdate;
    try
     fWaterCullPass.Execute(fVulkanComputeCommandBuffer);
    finally
     EndUpdate;
    end;
   end;

   if UpdateRenderIndex then begin
    TPasMPInterlocked.Write(fData.fVisualMeshVertexBufferRenderIndex,fData.fVisualMeshVertexBufferNextRenderIndex);
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
   if assigned(InFlightFrameData) then begin
    InFlightFrameData.fRaytracingTileQueue:=fRaytracingTileNextQueue;
   end;
  end else begin
   InFlightFrameData:=nil;
  end;

  if ((fVulkanDevice.UniversalQueueFamilyIndex<>fVulkanDevice.ComputeQueueFamilyIndex) and
      (fInFlightFrameSharingMode=TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE))) or
//   true or
//   (fData.fVisualMeshGeneration<>fData.fHeightMapGeneration) or
     (assigned(InFlightFrameData) and
      ((InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration) or
       (InFlightFrameData.fGrassMapGeneration<>fData.fGrassMapGeneration) or
       (InFlightFrameData.fWaterMapGeneration<>fData.fWaterMapGeneration))) then begin

   BeginUpdate;
   try

{   if fData.fVisualMeshGeneration<>fData.fHeightMapGeneration then begin
     fData.fVisualMeshGeneration:=fData.fHeightMapGeneration;
     fVisualMeshVertexGeneration.Execute(fVulkanComputeCommandBuffer);
     fVisualMeshDistanceGeneration.Execute(fVulkanComputeCommandBuffer);
     fData.fVisualMeshVertexBufferNextRenderIndex:=fData.fVisualMeshVertexBufferUpdateIndex and 1;
     fData.fVisualMeshVertexBufferUpdateIndex:=(fData.fVisualMeshVertexBufferUpdateIndex+1) and 1;
    end;}

    if assigned(InFlightFrameData) then begin

     InFlightFrameData.AcquireOnComputeQueue(fVulkanComputeCommandBuffer);

     if (InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration) or
        (InFlightFrameData.fGrassMapGeneration<>fData.fGrassMapGeneration) or
        (InFlightFrameData.fWaterMapGeneration<>fData.fWaterMapGeneration) then begin
      fData.TransferTo(fVulkanComputeCommandBuffer,
                       InFlightFrameData,
                       InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration,
                       InFlightFrameData.fGrassMapGeneration<>fData.fGrassMapGeneration,
                       InFlightFrameData.fWaterMapGeneration<>fData.fWaterMapGeneration
                      );
      InFlightFrameData.fHeightMapGeneration:=fData.fHeightMapGeneration;
      InFlightFrameData.fGrassMapGeneration:=fData.fGrassMapGeneration;
      InFlightFrameData.fWaterMapGeneration:=fData.fWaterMapGeneration;      
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

procedure TpvScene3DPlanet.Prepare(const aInFlightFrameIndex:TpvSizeInt;const aRendererInstance:TObject;const aRenderPassIndex:TpvSizeInt;const aViewPortWidth,aViewPortHeight:TpvInt32;const aMainViewPort:Boolean);
var RendererInstance:TpvScene3DPlanet.TRendererInstance;
    RendererViewInstance:TpvScene3DPlanet.TRendererViewInstance;
    Sphere:TpvSphere;
begin

 if assigned(fVulkanDevice) and (aInFlightFrameIndex>=0) then begin

  if aMainViewPort then begin

   fRendererInstanceListLock.Acquire;
   try

    if not fRendererInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererInstance.TKey.Create(aRendererInstance),
                                           RendererInstance) then begin
     RendererInstance:=TpvScene3DPlanet.TRendererInstance.Create(self,aRendererInstance);
    end;

    if assigned(RendererInstance) then begin

     if aMainViewPort then begin

      Sphere.Center:=(fPlanetData.ModelMatrix*TpvScene3DRendererInstance(aRendererInstance).InFlightFrameStates[aInFlightFrameIndex].MainViewMatrix).MulHomogen(TpvVector3.Origin);
      Sphere.Radius:=fTopRadius;

      if Sphere.Center.Length<Sphere.Radius then begin
       RendererInstance.fMinimumLODLevel:=0;
      end else begin
       RendererInstance.fMinimumLODLevel:=0;//Ceil(Clamp(Log2(Sphere.Center.Length/Sphere.Radius),0.0,Max(0.0,fTileMapBits-1)));
      end;

     end;

    end;

   finally
    fRendererInstanceListLock.Release;
   end;

  end;

  fRendererViewInstanceListLock.Acquire;
  try

   if not fRendererViewInstanceHashMap.TryGet(TpvScene3DPlanet.TRendererViewInstance.TKey.Create(aRendererInstance,aRenderPassIndex),
                                              RendererViewInstance) then begin
    RendererViewInstance:=TpvScene3DPlanet.TRendererViewInstance.Create(self,aRendererInstance,aRenderPassIndex,aMainViewPort);
   end;

   if assigned(RendererViewInstance) then begin

   end;

  finally
   fRendererViewInstanceListLock.Release;
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
   if TpvScene3D(fScene3D).UseBufferDeviceAddress then begin
    fPlanetData.Vertices:=fData.fVisualMeshVertexBuffers[(fData.fVisualMeshVertexBufferUpdateIndex+1) and 1].DeviceAddress;
    fPlanetData.Indices:=fData.fVisualMeshIndexBuffer.DeviceAddress;
   end else begin
    fPlanetData.Vertices:=0;
    fPlanetData.Indices:=0;
   end;
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

  if assigned(fRaytracingTiles) then begin
   fRaytracingTileQueue:=InFlightFrameData.fRaytracingTileQueue;
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
    Normal:TpvVector3;
begin
 WriteLine('# Exported physics mesh from TpvScene3DPlanet');
 WriteLine('o Planet');
 for Index:=0 to fData.fMeshVertices.Count-1 do begin
  Vertex:=@fData.fMeshVertices.ItemArray[Index];  
  WriteLine('v '+ConvertDoubleToString(Vertex^.Position.x)+' '+ConvertDoubleToString(Vertex^.Position.y)+' '+ConvertDoubleToString(Vertex^.Position.z));
  Normal:=OctDecode(Vertex^.OctahedralEncodedNormal);
  WriteLine('vn '+ConvertDoubleToString(Normal.x)+' '+ConvertDoubleToString(Normal.y)+' '+ConvertDoubleToString(Normal.z));
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
 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;
end;

destructor TpvScene3DPlanets.Destroy;
begin
 FreeAndNil(fLock);
 inherited Destroy;
end; 

end.

