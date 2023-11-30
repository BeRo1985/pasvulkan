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
              fVisualBaseMeshVertexBuffer:TpvVulkanBuffer; // vec4 wise, where only xyz is used, w is unused in the moment
              fVisualBaseMeshTriangleIndexBuffer:TpvVulkanBuffer; // uint32 wise, where the first item is the count of triangle indices and the rest are the triangle indices
              fVisualBaseMeshQuadIndexBuffer:TpvVulkanBuffer; // uint32 wise, where the first item is the count of quad indices and the rest are the quad indices
              fVisualMeshVertexBuffer:TpvVulkanBuffer; // TFibonacciSphereVertex wise
              fPhysicsBaseMeshVertexBuffer:TpvVulkanBuffer; // vec4 wise, where only xyz is used, w is unused in the moment
              fPhysicsBaseMeshTriangleIndexBuffer:TpvVulkanBuffer; // uint32 wise, where the first item is the count of triangle indices and the rest are the triangle indices
              fPhysicsBaseMeshQuadIndexBuffer:TpvVulkanBuffer; // uint32 wise, where the first item is the count of quad indices and the rest are the quad indices
              fPhysicsMeshVertexBuffer:TpvVulkanBuffer; // TFibonacciSphereVertex wise
              fModelMatrix:TpvMatrix4x4;
              fReady:TPasMPBool32;
              fInitialized:TPasMPBool32;
              fHeightMapGeneration:TpvUInt64;
              fTangentSpaceGeneration:TpvUInt64;
              fVisualMeshGeneration:TpvUInt64;
              fPhysicsMeshGeneration:TpvUInt64;
             public
              constructor Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32); reintroduce;
              destructor Destroy; override; 
              procedure TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aInFlightFrameData:TData);
             published
              property Planet:TpvScene3DPlanet read fPlanet;
              property InFlightFrameIndex:TpvInt32 read fInFlightFrameIndex;
              property HeightMap:THeightMap read fHeightMap;              
              property HeightMapImage:TpvScene3DRendererImage2D read fHeightMapImage;
              property NormalMapImage:TpvScene3DRendererImage2D read fNormalMapImage;
              property TangentBitangentMapImage:TpvScene3DRendererImage2D read fTangentBitangentMapImage; 
              property VisualBaseMeshVertexBuffer:TpvVulkanBuffer read fVisualBaseMeshVertexBuffer;
              property VisualBaseMeshTriangleIndexBuffer:TpvVulkanBuffer read fVisualBaseMeshTriangleIndexBuffer;
              property VisualBaseMeshQuadIndexBuffer:TpvVulkanBuffer read fVisualBaseMeshQuadIndexBuffer;
              property VisualMeshVertexBuffer:TpvVulkanBuffer read fVisualMeshVertexBuffer;
              property PhysicsBaseMeshVertexBuffer:TpvVulkanBuffer read fPhysicsBaseMeshVertexBuffer;
              property PhysicsBaseMeshTriangleIndexBuffer:TpvVulkanBuffer read fPhysicsBaseMeshTriangleIndexBuffer;
              property PhysicsBaseMeshQuadIndexBuffer:TpvVulkanBuffer read fPhysicsBaseMeshQuadIndexBuffer;
              property PhysicsMeshVertexBuffer:TpvVulkanBuffer read fPhysicsMeshVertexBuffer;
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
            end;
      private
       fScene3D:TObject;
       fVulkanDevice:TpvVulkanDevice;
       fVulkanQueue:TpvVulkanQueue;
       fVulkanFence:TpvVulkanFence;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanCommandBuffer:TpvVulkanCommandBuffer;
       fHeightMapResolution:TpvInt32;
       fCountVisualSpherePoints:TpvSizeInt;
       fCountPhysicsSpherePoints:TpvSizeInt;
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
       fVisualBaseMeshVertexGeneration:TBaseMeshVertexGeneration;
       fVisualBaseMeshIndexGeneration:TBaseMeshIndexGeneration;
       fVisualMeshVertexGeneration:TMeshVertexGeneration;
       fPhysicsBaseMeshVertexGeneration:TBaseMeshVertexGeneration;
       fPhysicsBaseMeshIndexGeneration:TBaseMeshIndexGeneration;
       fPhysicsMeshVertexGeneration:TMeshVertexGeneration;
       fCommandBufferLevel:TpvInt32;
       fCommandBufferLock:TPasMPInt32;
      public
      constructor Create(const aScene3D:TObject;     
                          const aHeightMapResolution:TpvInt32=2048;
                          const aCountVisualSpherePoints:TpvSizeInt=65536;
                          const aCountPhysicsSpherePoints:TpvSizeInt=65536;
                          const aBottomRadius:TpvFloat=6371000.0;
                          const aTopRadius:TpvFloat=6471000.0;
                          const aHeightMapScale:TpvFloat=1000.0); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release;
       function HandleRelease:boolean;
       procedure BeginUpdate;
       procedure EndUpdate;
       procedure FlushUpdate;
       procedure Initialize;
       procedure Update;
       procedure FrameUpdate(const aInFlightFrameIndex:TpvSizeInt); 
      published
       property Scene3D:TObject read fScene3D;
       property HeightMapResolution:TpvInt32 read fHeightMapResolution;
       property CountVisualSpherePoints:TpvSizeInt read fCountVisualSpherePoints;
       property CountPhysicsSpherePoints:TpvSizeInt read fCountPhysicsSpherePoints;
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

 fInitialized:=false;

 if fInFlightFrameIndex<0 then begin
  fHeightMapGeneration:=0;
 end else begin
  fHeightMapGeneration:=High(TpvUInt64);
 end;

 fTangentSpaceGeneration:=High(TpvUInt64);

 fVisualMeshGeneration:=High(TpvUInt64);

 fPhysicsMeshGeneration:=High(TpvUInt64);

 fModelMatrix:=TpvMatrix4x4.Identity;

 fHeightMapImage:=nil;

 fNormalMapImage:=nil;

 fTangentBitangentMapImage:=nil;

 fVisualBaseMeshVertexBuffer:=nil;

 fVisualBaseMeshTriangleIndexBuffer:=nil;

 fVisualBaseMeshQuadIndexBuffer:=nil;

 fVisualMeshVertexBuffer:=nil;

 fPhysicsBaseMeshVertexBuffer:=nil;

 fPhysicsBaseMeshTriangleIndexBuffer:=nil;

 fPhysicsBaseMeshQuadIndexBuffer:=nil;

 fPhysicsMeshVertexBuffer:=nil;

 if assigned(fPlanet.fVulkanDevice) then begin

  fHeightMapImage:=TpvScene3DRendererImage2D.Create(fPlanet.fVulkanDevice,
                                                    fPlanet.fHeightMapResolution,
                                                    fPlanet.fHeightMapResolution,
                                                    VK_FORMAT_R32_SFLOAT,
                                                    VK_SAMPLE_COUNT_1_BIT,
                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fHeightMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fHeightMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fHeightMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fHeightMapImage.ImageView');

  fNormalMapImage:=TpvScene3DRendererImage2D.Create(fPlanet.fVulkanDevice,
                                                    fPlanet.fHeightMapResolution,
                                                    fPlanet.fHeightMapResolution,
                                                    VK_FORMAT_R16G16_SFLOAT,
                                                    VK_SAMPLE_COUNT_1_BIT,
                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fNormalMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fNormalMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fNormalMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fNormalMapImage.ImageView');

  fTangentBitangentMapImage:=TpvScene3DRendererImage2D.Create(fPlanet.fVulkanDevice,
                                                              fPlanet.fHeightMapResolution,
                                                              fPlanet.fHeightMapResolution,
                                                              VK_FORMAT_R16G16B16A16_SFLOAT,
                                                              VK_SAMPLE_COUNT_1_BIT,
                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTangentBitangentMapImage.VulkanImage.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTangentBitangentMapImage.Image');
  fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fTangentBitangentMapImage.VulkanImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fTangentBitangentMapImage.ImageView');

  begin

   // All only-visual buffers doesn't need to be accessible from the CPU, just from the GPU itself
    
   if fInFlightFrameIndex<0 then begin

    fVisualBaseMeshVertexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                        fPlanet.fCountVisualSpherePoints*SizeOf(TpvVector4),
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
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualBaseMeshVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fVisualBaseMeshVertexBuffer');                                                   
           
    fVisualBaseMeshTriangleIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                               ((fPlanet.fCountVisualSpherePoints*12*3)+1)*SizeOf(TpvUInt32), // just for the worst case 
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
    fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualBaseMeshTriangleIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fVisualBaseMeshTriangleIndexBuffer');

   end;
   
   fVisualBaseMeshQuadIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                          ((fPlanet.fCountVisualSpherePoints*12*4)+1)*SizeOf(TpvUInt32), // just for the worst case 
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
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualBaseMeshQuadIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fVisualBaseMeshQuadIndexBuffer');
       
   fVisualMeshVertexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                   fPlanet.fCountVisualSpherePoints*SizeOf(TFibonacciSphereVertex),
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
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fVisualMeshVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fVisualMeshVertexBuffer');
        
  end;

  if fInFlightFrameIndex<0 then begin

   // Don't need to be accessible from the CPU, because it's only used for the initial vertex data without height map modifications
   fPhysicsBaseMeshVertexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                        fPlanet.fCountVisualSpherePoints*SizeOf(TpvVector4),
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
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsBaseMeshVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fPhysicsBaseMeshVertexBuffer');
    
   // But this does need to be accessible from the CPU for the download of that data for the physics engine and so on. 
   fPhysicsBaseMeshTriangleIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                               ((fPlanet.fCountVisualSpherePoints*12*3)+1)*SizeOf(TpvUInt32), // just for the worst case 
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
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsBaseMeshTriangleIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fPhysicsBaseMeshTriangleIndexBuffer');

   // But this does need to be accessible not from the CPU, since it's unused in the moment
   fPhysicsBaseMeshQuadIndexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                           ((fPlanet.fCountVisualSpherePoints*12*4)+1)*SizeOf(TpvUInt32), // just for the worst case 
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
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsBaseMeshQuadIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fPhysicsBaseMeshQuadIndexBuffer');
               
   // And this also does need to be accessible from the CPU for the download of that data for the physics engine and so on. 
   fPhysicsMeshVertexBuffer:=TpvVulkanBuffer.Create(fPlanet.fVulkanDevice,
                                                    fPlanet.fCountVisualSpherePoints*SizeOf(TFibonacciSphereVertex),
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
   fPlanet.fVulkanDevice.DebugUtils.SetObjectName(fPhysicsMeshVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3DPlanet.TData['+IntToStr(fInFlightFrameIndex)+'].fPhysicsMeshVertexBuffer');

  end;

 end;

end;

destructor TpvScene3DPlanet.TData.Destroy;
begin
 fHeightMap:=nil;
 FreeAndNil(fHeightMapImage);
 FreeAndNil(fNormalMapImage);
 FreeAndNil(fTangentBitangentMapImage);
 FreeAndNil(fVisualBaseMeshVertexBuffer);
 FreeAndNil(fVisualBaseMeshTriangleIndexBuffer);
 FreeAndNil(fVisualBaseMeshQuadIndexBuffer);
 FreeAndNil(fVisualMeshVertexBuffer);
 FreeAndNil(fPhysicsBaseMeshVertexBuffer);
 FreeAndNil(fPhysicsBaseMeshTriangleIndexBuffer);
 FreeAndNil(fPhysicsBaseMeshQuadIndexBuffer);
 FreeAndNil(fPhysicsMeshVertexBuffer);
 inherited Destroy;
end;

procedure TpvScene3DPlanet.TData.TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                            const aInFlightFrameData:TData);
var Index,CountImageMemoryBarriers:TpvSizeInt;
    SrcQueueFamilyIndex,DstQueueFamilyIndex:TpvUInt32;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..5] of TVkImageMemoryBarrier;
    BufferMemoryBarriers:array[0..3] of TVkBufferMemoryBarrier; 
    ImageCopy:TVkImageCopy;
    BufferCopy:TVkBufferCopy;
begin
  
 if assigned(fPlanet.fVulkanDevice) then begin

  if fPlanet.fVulkanDevice.ComputeQueueFamilyIndex=fPlanet.fVulkanDevice.UniversalQueueFamilyIndex then begin
   SrcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   DstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  end else begin
   SrcQueueFamilyIndex:=fPlanet.fVulkanDevice.UniversalQueueFamilyIndex;
   DstQueueFamilyIndex:=fPlanet.fVulkanDevice.ComputeQueueFamilyIndex;
  end;

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
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);      

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
                                                        SrcQueueFamilyIndex,
                                                        DstQueueFamilyIndex,
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        SrcQueueFamilyIndex,
                                                        DstQueueFamilyIndex,
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        SrcQueueFamilyIndex,
                                                        DstQueueFamilyIndex,
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
                                                          fVisualBaseMeshQuadIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT),
                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          SrcQueueFamilyIndex,
                                                          DstQueueFamilyIndex,
                                                          aInFlightFrameData.fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDEX_READ_BIT),
                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          SrcQueueFamilyIndex,
                                                          DstQueueFamilyIndex,
                                                          aInFlightFrameData.fVisualBaseMeshQuadIndexBuffer.Handle,
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

   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=fVisualBaseMeshVertexBuffer.Size;
   aCommandBuffer.CmdCopyBuffer(fVisualBaseMeshVertexBuffer.Handle,
                                aInFlightFrameData.fVisualMeshVertexBuffer.Handle,
                                1,
                                @BufferCopy);

   BufferCopy.srcOffset:=0;
   BufferCopy.dstOffset:=0;
   BufferCopy.size:=fVisualBaseMeshQuadIndexBuffer.Size;
   aCommandBuffer.CmdCopyBuffer(fVisualBaseMeshQuadIndexBuffer.Handle,
                                aInFlightFrameData.fVisualBaseMeshQuadIndexBuffer.Handle,
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
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        VK_QUEUE_FAMILY_IGNORED,
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

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
                                                        DstQueueFamilyIndex,
                                                        SrcQueueFamilyIndex,
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        DstQueueFamilyIndex,
                                                        SrcQueueFamilyIndex,
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);   

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        DstQueueFamilyIndex,
                                                        SrcQueueFamilyIndex,
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
                                                          fVisualBaseMeshQuadIndexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);

   BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT),
                                                          DstQueueFamilyIndex,
                                                          SrcQueueFamilyIndex,
                                                          aInFlightFrameData.fVisualMeshVertexBuffer.Handle,
                                                          0,
                                                          VK_WHOLE_SIZE);                                                       

   BufferMemoryBarriers[3]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_INDEX_READ_BIT),
                                                          DstQueueFamilyIndex,
                                                          SrcQueueFamilyIndex,
                                                          aInFlightFrameData.fVisualBaseMeshQuadIndexBuffer.Handle,
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

 fVulkanDevice:=fPlanet.fVulkanDevice;

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

  fPushConstants.PlanetGroundRadius:=fPlanet.fBottomRadius;
  fPushConstants.HeightMapScale:=fPlanet.fHeightMapScale;

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
   fDescriptorSet.WriteToDescriptorSet(2,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fPhysicsBaseMeshQuadIndexBuffer.Handle,
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
   fDescriptorSet.WriteToDescriptorSet(2,
                                       0,
                                       1,
                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                       [],
                                       [TVkDescriptorBufferInfo.Create(fPlanet.fData.fVisualBaseMeshQuadIndexBuffer.Handle,
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
var BufferMemoryBarriers:array[0..2] of TVkBufferMemoryBarrier;
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

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshQuadIndexBuffer.Handle,
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

  BufferMemoryBarriers[2]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshQuadIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end;                                                         

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                   0,
                                   0,nil,
                                   3,@BufferMemoryBarriers[0],
                                   0,nil);

 if fPhysics then begin

  aCommandBuffer.CmdFillBuffer(fPlanet.fData.fPhysicsBaseMeshTriangleIndexBuffer.Handle,
                               0,
                               SizeOf(TpvUInt32), // only the first uint32 needs to be cleared, since it is the count of indices
                               0);

  aCommandBuffer.CmdFillBuffer(fPlanet.fData.fPhysicsBaseMeshQuadIndexBuffer.Handle,
                               0,
                               SizeOf(TpvUInt32), // only the first uint32 needs to be cleared, since it is the count of indices
                               0);

 end else begin

  aCommandBuffer.CmdFillBuffer(fPlanet.fData.fVisualBaseMeshTriangleIndexBuffer.Handle,
                               0,
                               SizeOf(TpvUInt32), // only the first uint32 needs to be cleared, since it is the count of indices
                               0);

  aCommandBuffer.CmdFillBuffer(fPlanet.fData.fVisualBaseMeshQuadIndexBuffer.Handle,
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

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fPhysicsBaseMeshQuadIndexBuffer.Handle,
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

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshQuadIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

 end;

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
 end else begin
 
  // Only fBaseMeshTriangleIndexBuffer and fBaseMeshQuadIndexBuffer are modified, so only these two buffers need to be barriered
  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshTriangleIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fPlanet.fData.fVisualBaseMeshQuadIndexBuffer.Handle,
                                                         0,
                                                         VK_WHOLE_SIZE);     

 end;                                                   

 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                   0,
                                   0,nil,
                                   2,@BufferMemoryBarriers[0],
                                   0,nil);

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

{ TpvScene3DPlanet }

constructor TpvScene3DPlanet.Create(const aScene3D:TObject;
                                    const aHeightMapResolution:TpvInt32;
                                    const aCountVisualSpherePoints:TpvSizeInt;
                                    const aCountPhysicsSpherePoints:TpvSizeInt;
                                    const aBottomRadius:TpvFloat;
                                    const aTopRadius:TpvFloat;
                                    const aHeightMapScale:TpvFloat);
var InFlightFrameIndex:TpvSizeInt;
begin

 inherited Create;

 fScene3D:=aScene3D;

 fVulkanDevice:=TpvScene3D(fScene3D).VulkanDevice;

 fHeightMapResolution:=RoundUpToPowerOfTwo(Min(Max(aHeightMapResolution,128),8192));

 fCountVisualSpherePoints:=Min(Max(aCountVisualSpherePoints,32),16777216);

 fCountPhysicsSpherePoints:=Min(Max(aCountPhysicsSpherePoints,32),16777216);

 fBottomRadius:=aBottomRadius;

 fTopRadius:=aTopRadius;

 fHeightMapScale:=aHeightMapScale;

 if assigned(fVulkanDevice) then begin
  
  fVulkanQueue:=fVulkanDevice.ComputeQueue;

  fVulkanCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                                  fVulkanDevice.ComputeQueueFamilyIndex,
                                                  TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

  fVulkanCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool);

  fVulkanFence:=TpvVulkanFence.Create(fVulkanDevice);

 end;

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

 fVisualBaseMeshVertexGeneration:=TBaseMeshVertexGeneration.Create(self,false);

 fVisualBaseMeshIndexGeneration:=TBaseMeshIndexGeneration.Create(self,false);

 fVisualMeshVertexGeneration:=TMeshVertexGeneration.Create(self,false);

 fPhysicsBaseMeshVertexGeneration:=TBaseMeshVertexGeneration.Create(self,true);

 fPhysicsBaseMeshIndexGeneration:=TBaseMeshIndexGeneration.Create(self,true);

 fPhysicsMeshVertexGeneration:=TMeshVertexGeneration.Create(self,true);

 fCommandBufferLevel:=0;

 fCommandBufferLock:=0;

end;

destructor TpvScene3DPlanet.Destroy;
begin
 
 FreeAndNil(fPhysicsMeshVertexGeneration);

 FreeAndNil(fPhysicsBaseMeshIndexGeneration);

 FreeAndNil(fPhysicsBaseMeshVertexGeneration);
 
 FreeAndNil(fVisualMeshVertexGeneration);

 FreeAndNil(fVisualBaseMeshIndexGeneration);

 FreeAndNil(fVisualBaseMeshVertexGeneration);
 
 FreeAndNil(fTangentSpaceGeneration);

 FreeAndNil(fHeightMapModification);

 FreeAndNil(fHeightMapRandomInitialization);

 FreeAndNil(fInFlightFrameDataList);

 FreeAndNil(fData);

 FreeAndNil(fVulkanFence);

 FreeAndNil(fVulkanCommandBuffer);

 FreeAndNil(fVulkanCommandPool);
 
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

procedure TpvScene3DPlanet.BeginUpdate;
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fCommandBufferLock);
 try
  if fCommandBufferLevel=0 then begin
   fVulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
   fVulkanCommandBuffer.BeginRecording;
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
    fVulkanCommandBuffer.EndRecording;
    fVulkanCommandBuffer.Execute(fVulkanQueue,
                                 TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                 nil,
                                 nil,
                                 fVulkanFence,
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
   fVulkanCommandBuffer.EndRecording;
   fVulkanCommandBuffer.Execute(fVulkanQueue,
                                TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                nil,
                                nil,
                                fVulkanFence,
                                true);
   fVulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
   fVulkanCommandBuffer.BeginRecording;
  end;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fCommandBufferLock);
 end;
end;

procedure TpvScene3DPlanet.Initialize;
begin

 if not fData.fInitialized then begin

  BeginUpdate;
  try

   fHeightMapRandomInitialization.Execute(fVulkanCommandBuffer);

   fVisualBaseMeshVertexGeneration.Execute(fVulkanCommandBuffer);
   fVisualBaseMeshIndexGeneration.Execute(fVulkanCommandBuffer);

   fPhysicsBaseMeshVertexGeneration.Execute(fVulkanCommandBuffer);
   fPhysicsBaseMeshIndexGeneration.Execute(fVulkanCommandBuffer);

  finally
   EndUpdate;
  end;

  fData.fInitialized:=true;

 end;

end;

procedure TpvScene3DPlanet.Update;
begin

 if (fData.fTangentSpaceGeneration<>fData.fHeightMapGeneration) or
    (fData.fPhysicsMeshGeneration<>fData.fHeightMapGeneration) then begin

  BeginUpdate;
  try

   if fData.fTangentSpaceGeneration<>fData.fHeightMapGeneration then begin
    fData.fTangentSpaceGeneration:=fData.fHeightMapGeneration;
    fTangentSpaceGeneration.Execute(fVulkanCommandBuffer);
   end;

   if fData.fPhysicsMeshGeneration<>fData.fHeightMapGeneration then begin
    fData.fPhysicsMeshGeneration:=fData.fHeightMapGeneration;
    fPhysicsMeshVertexGeneration.Execute(fVulkanCommandBuffer);
   end;

  finally
   EndUpdate;
  end;

 end;
end;

procedure TpvScene3DPlanet.FrameUpdate(const aInFlightFrameIndex:TpvSizeInt);
var InFlightFrameData:TData;
begin

 if aInFlightFrameIndex>=0 then begin
  InFlightFrameData:=fInFlightFrameDataList[aInFlightFrameIndex];
 end else begin
  InFlightFrameData:=nil;
 end;

 if (fData.fVisualMeshGeneration<>fData.fHeightMapGeneration) or
    (assigned(InFlightFrameData) and (InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration)) then begin

  BeginUpdate;
  try

   if fData.fVisualMeshGeneration<>fData.fHeightMapGeneration then begin
    fData.fVisualMeshGeneration:=fData.fHeightMapGeneration;
    fVisualMeshVertexGeneration.Execute(fVulkanCommandBuffer);
   end;

  if assigned(InFlightFrameData) and (InFlightFrameData.fHeightMapGeneration<>fData.fHeightMapGeneration) then begin
    InFlightFrameData.fHeightMapGeneration:=fData.fHeightMapGeneration;
    fData.TransferTo(fVulkanCommandBuffer,InFlightFrameData);
   end;

  finally
   EndUpdate;
  end;

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

