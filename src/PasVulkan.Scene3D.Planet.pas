(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2023, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.Image2D;

type TpvScene3DPlanets=class;

     { TpvScene3DPlanet }
     TpvScene3DPlanet=class
      public
       type THeightValue=TpvFloat;
            PHeightValue=^THeightValue;
            THeightMap=array of THeightValue;
            TFibonacciSphereVertex=packed record
             PositionBitangentSign:TpvVector4; // xyz = position, w = bitangent sign
             NormalTangent:TpvVector4; // xy = normal, zw = tangent (both octahedral)
            end;
            PFibonacciSphereVertex=^TFibonacciSphereVertex;
            { TData }
            TData=class // one ground truth instance and one or more in-flight instances for flawlessly parallel rendering
             private    // All 2D maps are octahedral projected maps in this implementation (not equirectangular projected maps or cube maps)
              fPlanet:TpvScene3DPlanet;
              fInFlightFrameIndex:TpvInt32; // -1 is the ground truth instance, >=0 are the in-flight frame instances
              fHeightMap:THeightMap; // only on the ground truth instance, otherwise nil
              fHeightMapImage:TpvScene3DRendererImage2D; // R32_SFLOAT (at least for now, just for the sake of simplicity, later maybe R16_UNORM or R16_SNORM)
              fNormalMapImage:TpvScene3DRendererImage2D; // R16G16_SFLOAT (octahedral)
              fTangentBitangentMapImage:TpvScene3DRendererImage2D; // R16RG16B16A16_SFLOAT (octahedral-wise)   
              fBaseMeshVertexBuffer:TpvVulkanBuffer; // vec4 wise, where only xyz is used, w is unused in the moment
              fBaseMeshTriangleIndexBuffer:TpvVulkanBuffer; // uint32 wise, where the first item is the count of triangle indices and the rest are the triangle indices            
              fBaseMeshQuadIndexBuffer:TpvVulkanBuffer; // uint32 wise, where the first item is the count of quad indices and the rest are the quad indices
              fMeshVertexBuffer:TpvVulkanBuffer; // TFibonacciSphereVertex wise
              fModelMatrix:TpvMatrix4x4;
              fReady:TPasMPBool32;
              fHeightMapInitialized:TPasMPBool32;
              fHeightMapGeneration:TpvUInt64;
              fTangentSpaceGeneration:TpvUInt64;
              fBashMeshInitialized:TPasMPBool32;
              fMeshGeneration:TpvUInt64;
             public 
              constructor Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32); reintroduce;
              destructor Destroy; override; 
              procedure TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aInFlightFrameData:TData;
                                   const aFromSrcQueueFamilyIndex:TpvUInt32=VK_QUEUE_FAMILY_IGNORED;
                                   const aToSrcQueueFamilyIndex:TpvUInt32=VK_QUEUE_FAMILY_IGNORED;
                                   const aDstQueueFamilyIndex:TpvUInt32=VK_QUEUE_FAMILY_IGNORED);
             published
              property Planet:TpvScene3DPlanet read fPlanet;
              property InFlightFrameIndex:TpvInt32 read fInFlightFrameIndex;
              property HeightMap:THeightMap read fHeightMap;              
              property HeightMapImage:TpvScene3DRendererImage2D read fHeightMapImage;
              property NormalMapImage:TpvScene3DRendererImage2D read fNormalMapImage;
              property TangentBitangentMapImage:TpvScene3DRendererImage2D read fTangentBitangentMapImage; 
              property BaseMeshVertexBuffer:TpvVulkanBuffer read fBaseMeshVertexBuffer;
              property BaseMeshTriangleIndexBuffer:TpvVulkanBuffer read fBaseMeshTriangleIndexBuffer; 
              property BaseMeshQuadIndexBuffer:TpvVulkanBuffer read fBaseMeshQuadIndexBuffer;
              property MeshVertexBuffer:TpvVulkanBuffer read fMeshVertexBuffer;
             public
              property ModelMatrix:TpvMatrix4x4 read fModelMatrix write fModelMatrix; 
              property Ready:TPasMPBool32 read fReady write fReady;
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
            { TTangentSpaceGeneration } 
            TTangentSpaceGeneration=class
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
            { TBaseMeshVertexGeneration }
            TBaseMeshVertexGeneration=class
             public
              type TPushConstants=packed record
                    CountPoints:TpvUInt32;
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
            { TBaseMeshIndexGeneration }
            TBaseMeshIndexGeneration=class
             public
              type TPushConstants=packed record
                    CountPoints:TpvUInt32;
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
      private
       fScene3D:TObject;
       fVulkanDevice:TpvVulkanDevice;
       fVulkanComputeQueue:TpvVulkanQueue;
       fVulkanFence:TpvVulkanFence;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanCommandBuffer:TpvVulkanCommandBuffer;
       fHeightMapResolution:TpvInt32;
       fCountSpherePoints:TpvSizeInt;
       fBottomRadius:TpvFloat; // Start of the lowest planet ground
       fTopRadius:TpvFloat; // End of the atmosphere
       fHeightMapScale:TpvFloat; // Scale factor for the height map
       fData:TData;
       fInFlightFrameDataList:TInFlightFrameDataList;
       fReleaseFrameCounter:TpvInt32;
       fReady:TPasMPBool32;
       fInFlightFrameReady:array[0..MaxInFlightFrames-1] of TPasMPBool32;
       fHeightMapRandomInitialization:THeightMapRandomInitialization;
       fHeightMapModification:THeightMapModification;
       fTangentSpaceGeneration:TTangentSpaceGeneration;
       fBaseMeshVertexGeneration:TBaseMeshVertexGeneration;
       fBaseMeshIndexGeneration:TBaseMeshIndexGeneration;
       fMeshVertexGeneration:TMeshVertexGeneration;
      public
      constructor Create(const aScene3D:TObject;     
                          const aHeightMapResolution:TpvInt32=2048;
                          const aCountSpherePoints:TpvSizeInt=65536;
                          const aBottomRadius:TpvFloat=6371000.0;
                          const aTopRadius:TpvFloat=6471000.0;
                          const aHeightMapScale:TpvFloat=1000.0); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release;
       function HandleRelease:boolean;
       procedure Execute(const aInFlightFrameIndex:TpvSizeInt); 
      published
       property Scene3D:TObject read fScene3D;
       property HeightMapResolution:TpvInt32 read fHeightMapResolution;
       property CountSpherePoints:TpvSizeInt read fCountSpherePoints;
       property BottomRadius:TpvFloat read fBottomRadius;
       property TopRadius:TpvFloat read fTopRadius;
       property Data:TData read fData;
       property InFlightFrameDataList:TInFlightFrameDataList read fInFlightFrameDataList;
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
     PasVulkan.VirtualFileSystem;

{ TpvScene3DPlanet.TData }

constructor TpvScene3DPlanet.TData.Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32);
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fInFlightFrameIndex:=aInFlightFrameIndex;

 if fInFlightFrameIndex<0 then begin
  fHeightMap:=nil;
  SetLength(fHeightMap,fPlanet.fHeightMapResolution*fPlanet.fHeightMapResolution);
 end else begin
  fHeightMap:=nil;
 end;

 fHeightMapInitialized:=false;

 fHeightMapGeneration:=0;

 fTangentSpaceGeneration:=High(TpvUInt64);

 fBashMeshInitialized:=false;

 fMeshGeneration:=High(TpvUInt64);

 fModelMatrix:=TpvMatrix4x4.Identity;

 fHeightMapImage:=nil;

 fNormalMapImage:=nil;

 fTangentBitangentMapImage:=nil;

 fBaseMeshVertexBuffer:=nil;

 fBaseMeshTriangleIndexBuffer:=nil;

 fBaseMeshQuadIndexBuffer:=nil;

 fMeshVertexBuffer:=nil;

 if assigned(TpvScene3D(fPlanet.fScene3D).VulkanDevice) then begin

  fHeightMapImage:=TpvScene3DRendererImage2D.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                    fPlanet.fHeightMapResolution,
                                                    fPlanet.fHeightMapResolution,
                                                    VK_FORMAT_R32_SFLOAT,
                                                    VK_SAMPLE_COUNT_1_BIT,
                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

  fNormalMapImage:=TpvScene3DRendererImage2D.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                    fPlanet.fHeightMapResolution,
                                                    fPlanet.fHeightMapResolution,
                                                    VK_FORMAT_R16G16_SFLOAT,
                                                    VK_SAMPLE_COUNT_1_BIT,
                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

  fTangentBitangentMapImage:=TpvScene3DRendererImage2D.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                              fPlanet.fHeightMapResolution,
                                                              fPlanet.fHeightMapResolution,
                                                              VK_FORMAT_R16G16B16A16_SFLOAT,
                                                              VK_SAMPLE_COUNT_1_BIT,
                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

  if fInFlightFrameIndex<0 then begin

   // Don't need to be accessible from the CPU, because it's only used for the initial vertex data without height map modifications
   fBaseMeshVertexBuffer:=TpvVulkanBuffer.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                 fPlanet.fCountSpherePoints*SizeOf(TpvVector4),
                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
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
                                                 []
                                                );
    
   // But this does need to be accessible from the CPU for the download of that data for the physics engine and so on. 
   fBaseMeshTriangleIndexBuffer:=TpvVulkanBuffer.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                ((fPlanet.fCountSpherePoints*12*3)+1)*SizeOf(TpvUInt32), // just for the worst case 
                                                TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                [],
                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                               );

   // But this does need to be accessible not from the CPU, since it's for rendering only
   fBaseMeshQuadIndexBuffer:=TpvVulkanBuffer.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                    ((fPlanet.fCountSpherePoints*12*4)+1)*SizeOf(TpvUInt32), // just for the worst case 
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
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
                                                    []
                                                   );
        
   // And this also does need to be accessible from the CPU for the download of that data for the physics engine and so on. 
   fMeshVertexBuffer:=TpvVulkanBuffer.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                             fPlanet.fCountSpherePoints*SizeOf(TFibonacciSphereVertex),
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                             [],
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                             0,
                                             0,
                                             0,
                                             0,
                                             0,
                                             0,
                                             [TpvVulkanBufferFlag.PersistentMappedIfPossibe]
                                            );

  end else begin

   // TODO: The same for the in-flight frame instances for ray-tracing and so on, but with higher triangle count than for the physics engine

  end;

 end;

end;

destructor TpvScene3DPlanet.TData.Destroy;
begin
 fHeightMap:=nil;
 FreeAndNil(fHeightMapImage);
 FreeAndNil(fNormalMapImage);
 FreeAndNil(fTangentBitangentMapImage);
 FreeAndNil(fBaseMeshVertexBuffer);
 FreeAndNil(fBaseMeshTriangleIndexBuffer);
 FreeAndNil(fBaseMeshQuadIndexBuffer);
 FreeAndNil(fMeshVertexBuffer);
 inherited Destroy;
end;

procedure TpvScene3DPlanet.TData.TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                            const aInFlightFrameData:TData;
                                            const aFromSrcQueueFamilyIndex:TpvUInt32;
                                            const aToSrcQueueFamilyIndex:TpvUInt32;
                                            const aDstQueueFamilyIndex:TpvUInt32);
var Index,CountImageMemoryBarriers:TpvSizeInt;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..5] of TVkImageMemoryBarrier;
    ImageCopy:TVkImageCopy;
begin
  
 if assigned(TpvScene3D(fPlanet.fScene3D).VulkanDevice) then begin

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
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);      

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange); 

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aToSrcQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aToSrcQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aToSrcQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);                                                     

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     6,@ImageMemoryBarriers[0]);                                                 
  end;   
 
  //////////////////////////// 

  begin
    
   FillChar(ImageCopy,SizeOf(TVkImageCopy),#0);
   ImageCopy.srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageCopy.srcSubresource.mipLevel:=0;
   ImageCopy.srcSubresource.baseArrayLayer:=0;
   ImageCopy.srcSubresource.layerCount:=1;
   ImageCopy.srcOffset.x:=0;
   ImageCopy.srcOffset.y:=0;
   ImageCopy.srcOffset.z:=0;
   ImageCopy.dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageCopy.dstSubresource.mipLevel:=0;
   ImageCopy.dstSubresource.baseArrayLayer:=0;
   ImageCopy.dstSubresource.layerCount:=1;
   ImageCopy.dstOffset.x:=0;
   ImageCopy.dstOffset.y:=0;
   ImageCopy.dstOffset.z:=0;
   ImageCopy.extent.width:=fPlanet.fHeightMapResolution;
   ImageCopy.extent.height:=fPlanet.fHeightMapResolution;
   ImageCopy.extent.depth:=1;
   
   aCommandBuffer.CmdCopyImage(fHeightMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               1,@ImageCopy);

   aCommandBuffer.CmdCopyImage(fNormalMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               1,@ImageCopy);

   aCommandBuffer.CmdCopyImage(fTangentBitangentMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               1,@ImageCopy);         

  end;

  //////////////////////////// 

  begin

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);   

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT),
                                     0,
                                     0,
                                     nil,
                                     0,nil,
                                     6,@ImageMemoryBarriers[0]);
      
  end;

  ////////////////////////////

 end;

 aInFlightFrameData.fModelMatrix:=fModelMatrix;

 aInFlightFrameData.fReady:=fReady;

end;

{ TpvScene3DPlanet.THeightMapRandomInitialization }

constructor TpvScene3DPlanet.THeightMapRandomInitialization.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=TpvScene3D(fPlanet.fScene3D).VulkanDevice;

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
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageView.Handle,
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

  fPushConstants.Octaves:=8;
  fPushConstants.Scale:=4.0;
  fPushConstants.Amplitude:=1.0;
  fPushConstants.Lacunarity:=2.0;
  fPushConstants.Gain:=0.5;

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
begin

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

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
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

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier);                                                                                                                                                                                                 

end;

{ TpvScene3DPlanet.THeightMapModification }

constructor TpvScene3DPlanet.THeightMapModification.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=TpvScene3D(fPlanet.fScene3D).VulkanDevice;

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
  fDescriptorSetLayout.Initialize;

  fPipelineLayout:=TpvVulkanPipelineLayout.Create(fVulkanDevice);
  fPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),0,SizeOf(TPushConstants));
  fPipelineLayout.AddDescriptorSetLayout(fDescriptorSetLayout);
  fPipelineLayout.Initialize;

  fDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,
                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                  1);
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),1);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageView.Handle,
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
begin

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

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
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

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   0,nil,
                                   1,@ImageMemoryBarrier); 

end;

{ TpvScene3DPlanet.TTangentSpaceGeneration }

constructor TpvScene3DPlanet.TTangentSpaceGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=TpvScene3D(fPlanet.fScene3D).VulkanDevice;

 if assigned(fVulkanDevice) then begin

  Stream:=pvScene3DShaderVirtualFileSystem.GetFile('planet_tangentspace_generation_comp.spv');
  try
   fComputeShaderModule:=TpvVulkanShaderModule.Create(fVulkanDevice,Stream);
  finally
   FreeAndNil(Stream);
  end;

  fVulkanDevice.DebugUtils.SetObjectName(fComputeShaderModule.Handle,VK_OBJECT_TYPE_SHADER_MODULE,'TpvScene3DPlanet.TTangentSpaceGeneration.fComputeShaderModule');

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
  fDescriptorSetLayout.AddBinding(2,
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
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),3);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fHeightMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fNormalMapImage.VulkanImageView.Handle,
                                                                     VK_IMAGE_LAYOUT_GENERAL)],
                                      [],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(2,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE),
                                      [TVkDescriptorImageInfo.Create(VK_NULL_HANDLE,
                                                                     fPlanet.fData.fTangentBitangentMapImage.VulkanImageView.Handle,
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

  fPushConstants.PlanetGroundRadius:=0.0;
  fPushConstants.HeightMapScale:=0.0;

 end;

end;

destructor TpvScene3DPlanet.TTangentSpaceGeneration.Destroy;
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

procedure TpvScene3DPlanet.TTangentSpaceGeneration.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var ImageMemoryBarriers:array[0..2] of TVkImageMemoryBarrier;
begin

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

 ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                      VK_IMAGE_LAYOUT_GENERAL,
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
                                   0,nil,
                                   3,@ImageMemoryBarriers[0]);   

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

 ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                      VK_IMAGE_LAYOUT_GENERAL,
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
                                   0,nil,
                                   3,@ImageMemoryBarriers[0]);      

end;  

{ TpvScene3DPlanet.TBaseMeshVertexGeneration }

constructor TpvScene3DPlanet.TBaseMeshVertexGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=TpvScene3D(fPlanet.fScene3D).VulkanDevice;

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
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fBaseMeshVertexBuffer.Handle,
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

  fPushConstants.CountPoints:=fPlanet.fCountSpherePoints;

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

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fBaseMeshVertexBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

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

 aCommandBuffer.CmdDispatch((fPlanet.fCountSpherePoints+255) shr 8,
                            1,
                            1);

 BufferMemoryBarrier:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    VK_QUEUE_FAMILY_IGNORED,
                                                    fPlanet.fData.fBaseMeshVertexBuffer.Handle,
                                                    0,
                                                    VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarrier,
                                   0,nil);

end;

{ TpvScene3DPlanet.TBaseMeshIndexGeneration }
                                            
constructor TpvScene3DPlanet.TBaseMeshIndexGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=TpvScene3D(fPlanet.fScene3D).VulkanDevice;

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
  fDescriptorSetLayout.AddBinding(2,
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
  fDescriptorPool.AddDescriptorPoolSize(TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),3);
  fDescriptorPool.Initialize;

  fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fDescriptorSetLayout);
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fBaseMeshVertexBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fBaseMeshTriangleIndexBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false); 
  fDescriptorSet.WriteToDescriptorSet(2,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fBaseMeshQuadIndexBuffer.Handle,
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

  fPushConstants.CountPoints:=fPlanet.fCountSpherePoints;

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
var BufferMemoryBarriers:array[0..2] of TVkBufferMemoryBarrier;
begin

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshVertexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshTriangleIndexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshQuadIndexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);                                                         

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   3,@BufferMemoryBarriers[0],
                                   0,nil);

 aCommandBuffer.CmdFillBuffer(fPlanet.fData.fBaseMeshTriangleIndexBuffer.Handle,
                              0,
                              VK_WHOLE_SIZE,
                              0);

 aCommandBuffer.CmdFillBuffer(fPlanet.fData.fBaseMeshQuadIndexBuffer.Handle,
                              0,
                              VK_WHOLE_SIZE,
                              0);
  
 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshTriangleIndexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshQuadIndexBuffer.Handle,
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

 aCommandBuffer.CmdDispatch((fPlanet.fCountSpherePoints+255) shr 8,
                            1,
                            1);

 // Only fBaseMeshTriangleIndexBuffer and fBaseMeshQuadIndexBuffer are modified, so only these two buffers need to be barriered
 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshTriangleIndexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshQuadIndexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);                                                        

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

end;

{ TpvScene3DPlanet.TMeshVertexGeneration } 

constructor TpvScene3DPlanet.TMeshVertexGeneration.Create(const aPlanet:TpvScene3DPlanet);
var Stream:TStream;
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fVulkanDevice:=TpvScene3D(fPlanet.fScene3D).VulkanDevice;

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
  fDescriptorSet.WriteToDescriptorSet(0,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fBaseMeshVertexBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false);
  fDescriptorSet.WriteToDescriptorSet(1,
                                      0,
                                      1,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                      [],
                                      [TVkDescriptorBufferInfo.Create(fPlanet.fData.fMeshVertexBuffer.Handle,
                                                                      0,
                                                                      VK_WHOLE_SIZE)],
                                      [],
                                      false); 
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

  fPushConstants.CountPoints:=fPlanet.fCountSpherePoints;
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

 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fBaseMeshVertexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fMeshVertexBuffer.Handle,
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

 aCommandBuffer.CmdDispatch((fPlanet.fCountSpherePoints+255) shr 8,
                            1,
                            1);

 // Just one buffer memory barrier is needed here, since only fMeshVertexDataBuffer was written to, here in this compute shader.  
 BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fPlanet.fData.fMeshVertexBuffer.Handle,
                                                        0,
                                                        VK_WHOLE_SIZE);

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   1,@BufferMemoryBarriers[0],
                                   0,nil);

end;

{ TpvScene3DPlanet }

constructor TpvScene3DPlanet.Create(const aScene3D:TObject;
                                    const aHeightMapResolution:TpvInt32;
                                    const aCountSpherePoints:TpvSizeInt;
                                    const aBottomRadius:TpvFloat;
                                    const aTopRadius:TpvFloat;
                                    const aHeightMapScale:TpvFloat);
var InFlightFrameIndex:TpvSizeInt;
begin

 inherited Create;

 fScene3D:=aScene3D;

 fHeightMapResolution:=RoundUpToPowerOfTwo(Min(Max(aHeightMapResolution,128),8192));

 fCountSpherePoints:=Min(Max(aCountSpherePoints,32),16777216);

 fBottomRadius:=aBottomRadius;

 fTopRadius:=aTopRadius;

 fHeightMapScale:=aHeightMapScale;

 fData:=TData.Create(self,-1);

 fInFlightFrameDataList:=TInFlightFrameDataList.Create(true);
 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  fInFlightFrameDataList.Add(TData.Create(self,InFlightFrameIndex));
 end;

 fReleaseFrameCounter:=-1;

 fReady:=true;

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  fInFlightFrameReady[InFlightFrameIndex]:=false;
 end; 
 
 fHeightMapRandomInitialization:=THeightMapRandomInitialization.Create(self);

 fHeightMapModification:=THeightMapModification.Create(self);

 fTangentSpaceGeneration:=TTangentSpaceGeneration.Create(self);

 fBaseMeshVertexGeneration:=TBaseMeshVertexGeneration.Create(self);

 fBaseMeshIndexGeneration:=TBaseMeshIndexGeneration.Create(self);

 fMeshVertexGeneration:=TMeshVertexGeneration.Create(self);

end;

destructor TpvScene3DPlanet.Destroy;
begin
 
 FreeAndNil(fMeshVertexGeneration);

 FreeAndNil(fBaseMeshIndexGeneration);

 FreeAndNil(fBaseMeshVertexGeneration);
 
 FreeAndNil(fTangentSpaceGeneration);

 FreeAndNil(fHeightMapModification);

 FreeAndNil(fHeightMapRandomInitialization);

 FreeAndNil(fInFlightFrameDataList);

 FreeAndNil(fData);
 
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
    TpvScene3D(fScene3D).Planets.Extract(Index); // not delete ir remove, since we don't want to free ourself here already.
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

procedure TpvScene3DPlanet.Execute(const aInFlightFrameIndex:TpvSizeInt);
var Dirty:TPasMPBool32;
    InFlightFrameData:TData;
begin

 Dirty:=false;

 if not fData.fHeightMapInitialized then begin
  fHeightMapRandomInitialization.Execute(fVulkanCommandBuffer);  
  Dirty:=true;
 end;

 if Dirty then begin
  inc(fData.fHeightMapGeneration); // Pump the generation counter, if something has changed. 
 end;

 // Update normals, tangents and bitangents if necessary
 if fData.fTangentSpaceGeneration<>fData.fHeightMapGeneration then begin
  fTangentSpaceGeneration.Execute(fVulkanCommandBuffer);
  fData.fTangentSpaceGeneration:=fData.fHeightMapGeneration;
 end;

 if not fData.fBaseMeshInitialized then begin
  fBaseMeshVertexGeneration.Execute(fVulkanCommandBuffer);
  fBaseMeshIndexGeneration.Execute(fVulkanCommandBuffer);
  fData.fBaseMeshInitialized:=true;
 end;

 if fData.fMeshGeneration<>fData.fTangentSpaceGeneration then begin
  fMeshVertexGeneration.Execute(fVulkanCommandBuffer);
  fData.fMeshGeneration:=fData.fTangentSpaceGeneration;
 end;

 // Update in-flight frame data if necessary
 InFlightFrameData:=fInFlightFrameDataList[aInFlightFrameIndex];
 if InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration then begin
  fData.TransferTo(fVulkanCommandBuffer,
                   InFlightFrameData,
                   VK_QUEUE_FAMILY_IGNORED,
                   VK_QUEUE_FAMILY_IGNORED,
                   VK_QUEUE_FAMILY_IGNORED);
  InFlightFrameData.fHeightMapGeneration:=fData.fHeightMapGeneration;
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

