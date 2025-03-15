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
unit PasVulkan.Raytracing;
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
     PasVulkan.Collections,
     PasVulkan.HighResolutionTimer,
     PasVulkan.Framework,
     PasVulkan.BufferRangeAllocator;

type EpvRaytracing=class(Exception);

     TpvRaytracingCullMask=class
      public
       const Shadows=$01;     // All objects that should cast shadows should have this cull mask set
             CameraView=$02;  // All objects that should be visible in the camera view should have this cull mask set, so for example the player in first person view should not have this cull mask set, but still CULLMASK_REFLECTION for reflections and so on
             Reflection=$04;  // All objects that should be visible in reflections should have this cull mask set
             Occlusion=$08;   // All objects that should be considered for ambient occlusion as occluders should have this cull mask set
             All=$ff;         // Just everything
     end; 

     TpvRaytracingAccelerationStructure=class;

     TpvRaytracingAccelerationStructureList=TpvObjectGenericList<TpvRaytracingAccelerationStructure>;

     TpvRaytracingAccelerationStructureInstanceArrayList=TpvDynamicArrayList<TVkAccelerationStructureInstanceKHR>;

     { TpvRaytracingCompactedSizeQueryPool }
     TpvRaytracingCompactedSizeQueryPool=class
      public
       type TCompactedSizes=TpvDynamicArrayList<TVkDeviceSize>;
            TAccelerationStructureList=TpvDynamicArrayList<TVkAccelerationStructureKHR>;
            TAccelerationStructureIndexHashMap=TpvHashMap<TVkAccelerationStructureKHR,TpvSizeInt>;
      private
       fDevice:TpvVulkanDevice;
       fQueryPool:TVkQueryPool;
       fQueryPoolCreateInfo:TVkQueryPoolCreateInfo;
       fCount:TVkUInt32;
       fAccelerationStructures:TpvRaytracingAccelerationStructureList;
       fAccelerationStructureList:TAccelerationStructureList;
       fAccelerationStructureIndexHashMap:TAccelerationStructureIndexHashMap;
       fResultAccelerationStructureIndexHashMap:TAccelerationStructureIndexHashMap;
       fCompactedSizes:TCompactedSizes;
      public
       constructor Create(const aDevice:TpvVulkanDevice);
       destructor Destroy; override;
       function Empty:boolean;
       function Ready:boolean;
       procedure Reset;
       procedure AddAccelerationStructure(const aAccelerationStructure:TpvRaytracingAccelerationStructure);
       procedure Query(const aCommandBuffer:TpvVulkanCommandBuffer);
       procedure GetResults;
       function GetCompactedSizeByIndex(const aIndex:TpvSizeInt):TVkDeviceSize;
       function GetCompactedSizeByAccelerationStructure(const aAccelerationStructure:TpvRaytracingAccelerationStructure):TVkDeviceSize;
      published
       property Device:TpvVulkanDevice read fDevice;
       property QueryPool:TVkQueryPool read fQueryPool;
       property Count:TVkUInt32 read fCount;
       property CompactedSizes:TCompactedSizes read fCompactedSizes;
       property AccelerationStructures:TpvRaytracingAccelerationStructureList read fAccelerationStructures;
     end;

     { TpvRaytracingBLASGeometryInfoBufferItem } 
     TpvRaytracingBLASGeometryInfoBufferItem=packed record // per gl_InstanceCustomIndexEXT or gl_InstanceID wise, depending on the usage
      public
       const TypeNone=TVkUInt32($ffffffff);
             TypeMesh=0;
             TypeParticle=1;
             TypePlanet=2;
      private

       // uvec4 start
       fType_:TVkUInt32; // Type of object, 0 = mesh, 1 = particle, 2 = planet, and so on
       fObjectIndex:TVkUInt32; // Index of object, especially for planet objects important, because it's the index of the planet in the planet list, and not for the mesh objects, since mesh objects uses the same unique vertex and index buffers.
       fMaterialIndex:TVkUInt32; // Index of material
       fIndexOffset:TVkUInt32; // Offset inside index buffer
       // uvec4 end

      public 
       constructor Create(const aType_:TVkUInt32;
                          const aObjectIndex:TVkUInt32;
                          const aMaterialIndex:TVkUInt32;
                          const aIndexOffset:TVkUInt32);
      public
       property Type_:TVkUInt32 read fType_ write fType_;
       property ObjectIndex:TVkUInt32 read fObjectIndex write fObjectIndex;
       property MaterialIndex:TVkUInt32 read fMaterialIndex write fMaterialIndex;
       property IndexOffset:TVkUInt32 read fIndexOffset write fIndexOffset;
     end;
     PpvRaytracingBLASGeometryInfoBufferItem=^TpvRaytracingBLASGeometryInfoBufferItem;

     TpvRaytracingBLASGeometryInfoBufferItems=array of TpvRaytracingBLASGeometryInfoBufferItem;

     TpvRaytracingBLASGeometryInfoBufferItemList=TpvDynamicArrayList<TpvRaytracingBLASGeometryInfoBufferItem>;

     TpvRaytracingBLASGeometryInfoOffsetBufferItem=TVkUInt32; // Instance offset index for first geometry buffer item per BLAS instance
     PpvRaytracingBLASGeometryInfoOffsetBufferItem=^TpvRaytracingBLASGeometryInfoOffsetBufferItem;

     TpvRaytracingBLASGeometryInfoOffsetBufferItems=array of TpvRaytracingBLASGeometryInfoOffsetBufferItem;

     TpvRaytracingBLASGeometryInfoOffsetBufferItemList=TpvDynamicArrayList<TpvRaytracingBLASGeometryInfoOffsetBufferItem>;

     { TpvRaytracingInstanceShaderBindingTableRecordOffsets }
     TpvRaytracingInstanceShaderBindingTableRecordOffsets=class
      public
       const Mesh=0;
             Planet=1;
     end;

     { TpvRaytracingInstanceCustomIndexManager }
     TpvRaytracingInstanceCustomIndexManager=class
      public
       type TItem=record
             Index:TpvInt32;
             Data:TpvPtrUInt;
            end;
            PItem=^TItem;
            TItemList=TpvDynamicArrayList<TItem>;
            TItemHashMap=TpvHashMap<TpvPtrUInt,TpvInt32>;
            TFreeList=TpvDynamicStack<TpvInt32>;
      private
       fItems:TItemList;
       fItemHashMap:TItemHashMap;
       fFreeList:TFreeList;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       function Add(const aData:TpvPtrUInt):TpvInt32;
       function RemoveData(const aData:TpvPtrUInt):boolean;
       function RemoveIndex(const aIndex:TpvInt32):boolean;
       function GetData(const aIndex:TpvInt32):TpvPtrUInt;
       function GetIndex(const aData:TpvPtrUInt):TpvInt32;
     end;

     { TpvRaytracingAccelerationStructureBuildQueue }
     TpvRaytracingAccelerationStructureBuildQueue=class
      public
       type TBuildGeometryInfos=TpvDynamicArrayList<TVkAccelerationStructureBuildGeometryInfoKHR>;
            TBuildOffsetInfoPtrs=TpvDynamicArrayList<PVkAccelerationStructureBuildRangeInfoKHR>;
      private 
       fDevice:TpvVulkanDevice;
       fBuildGeometryInfos:TBuildGeometryInfos;
       fBuildOffsetInfoPtrs:TBuildOffsetInfoPtrs;
      public
       constructor Create(const aDevice:TpvVulkanDevice); reintroduce;
       destructor Destroy; override;
       procedure Clear;
       function Empty:Boolean;
       procedure Enqueue(const aBuildGeometryInfo:TVkAccelerationStructureBuildGeometryInfoKHR;
                         const aBuildOffsetInfoPtr:PVkAccelerationStructureBuildRangeInfoKHR); 
       procedure Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
      published
       property Device:TpvVulkanDevice read fDevice;
     end;

     { TpvRaytracingAccelerationStructure }
     TpvRaytracingAccelerationStructure=class
      private
       fDevice:TpvVulkanDevice;
       fAccelerationStructure:TVkAccelerationStructureKHR;
       fAccelerationStructureType:TVkAccelerationStructureTypeKHR;
       fBuildGeometryInfo:TVkAccelerationStructureBuildGeometryInfoKHR;
       fBuildSizesInfo:TVkAccelerationStructureBuildSizesInfoKHR;
       fBuildOffsetInfoPtr:PVkAccelerationStructureBuildRangeInfoKHR;
       fGeneration:TpvUInt64;
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aAccelerationStructureType:TVkAccelerationStructureTypeKHR=TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR)); reintroduce; 
       destructor Destroy; override;
       class function Reduce(const aStructures:TpvRaytracingAccelerationStructureList):TVkAccelerationStructureBuildSizesInfoKHR; static;
       function GetMemorySizes(const aCounts:PVkUInt32):TVkAccelerationStructureBuildSizesInfoKHR;
       procedure Initialize(const aResultBuffer:TpvVulkanBuffer;const aResultOffset:TVkDeviceSize);
       procedure Finalize;
       procedure Build(const aCommandBuffer:TpvVulkanCommandBuffer;
                       const aScratchBuffer:TpvVulkanBuffer;
                       const aScratchBufferOffset:TVkDeviceSize;
                       const aUpdate:Boolean=false;   
                       const aSourceAccelerationStructure:TpvRaytracingAccelerationStructure=nil;
                       const aQueue:TpvRaytracingAccelerationStructureBuildQueue=nil);
       procedure CopyFrom(const aCommandBuffer:TpvVulkanCommandBuffer;
                          const aSourceAccelerationStructure:TpvRaytracingAccelerationStructure;
                          const aCompact:Boolean=false);
       class procedure MemoryBarrier(const aCommandBuffer:TpvVulkanCommandBuffer); static;
      published
       property Device:TpvVulkanDevice read fDevice;
       property AccelerationStructure:TVkAccelerationStructureKHR read fAccelerationStructure;
       property AccelerationStructureType:TVkAccelerationStructureTypeKHR read fAccelerationStructureType;
       property Generation:TpvUInt64 read fGeneration;
      public
       property BuildGeometryInfo:TVkAccelerationStructureBuildGeometryInfoKHR read fBuildGeometryInfo;
       property BuildSizesInfo:TVkAccelerationStructureBuildSizesInfoKHR read fBuildSizesInfo;
       property AccelerationStructureSize:TVkDeviceSize read fBuildSizesInfo.accelerationStructureSize;
       property UpdateScratchSize:TVkDeviceSize read fBuildSizesInfo.updateScratchSize;
       property BuildScratchSize:TVkDeviceSize read fBuildSizesInfo.buildScratchSize;
     end;

     { TpvRaytracingBottomLevelAccelerationStructureGeometry }
     TpvRaytracingBottomLevelAccelerationStructureGeometry=class
      public
       type TGeometries=TpvDynamicArrayList<TVkAccelerationStructureGeometryKHR>;
            TBuildOffsets=TpvDynamicArrayList<TVkAccelerationStructureBuildRangeInfoKHR>;
      private
       fDevice:TpvVulkanDevice;
       fGeometries:TGeometries;
       fBuildOffsets:TBuildOffsets;
      public
       constructor Create(const aDevice:TpvVulkanDevice); reintroduce;
       destructor Destroy; override;       
       procedure AddTriangles(const aVertexBuffer:TpvVulkanBuffer;
                              const aVertexOffset:TVkUInt32;
                              const aVertexCount:TVkUInt32;
                              const aVertexStride:TVkDeviceSize;
                              const aIndexBuffer:TpvVulkanBuffer;
                              const aIndexOffset:TVkUInt32;
                              const aIndexCount:TVkUInt32;
                              const aOpaque:Boolean;
                              const aTransformBuffer:TpvVulkanBuffer=nil;
                              const aTransformOffset:TVkDeviceSize=0);
       procedure AddAABBs(const aAABBBuffer:TpvVulkanBuffer;
                          const aOffset:TVkDeviceSize;
                          const aCount:TVkUInt32;
                          const aOpaque:Boolean;
                          const aStride:TVkDeviceSize=SizeOf(TVkAabbPositionsKHR));
      published
       property Geometries:TGeometries read fGeometries;
       property BuildOffsets:TBuildOffsets read fBuildOffsets;
     end;
     
     { TpvRaytracingBottomLevelAccelerationStructure }
     TpvRaytracingBottomLevelAccelerationStructure=class(TpvRaytracingAccelerationStructure)
      private
       fGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry;
       fDynamicGeometry:Boolean;
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry=nil;
                          const aFlags:TVkBuildAccelerationStructureFlagsKHR=0;
                          const aDynamicGeometry:Boolean=false); reintroduce;
       destructor Destroy; override;
       procedure Update(const aGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry;
                        const aFlags:TVkBuildAccelerationStructureFlagsKHR=0;
                        const aDynamicGeometry:Boolean=false); reintroduce;
      published
       property Geometry:TpvRaytracingBottomLevelAccelerationStructureGeometry read fGeometry;
       property DynamicGeometry:Boolean read fDynamicGeometry;
     end;

     { TpvRaytracingBottomLevelAccelerationStructureInstance }
     TpvRaytracingBottomLevelAccelerationStructureInstance=class
      private
       fDevice:TpvVulkanDevice;
       fAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure;
       fAccelerationStructureInstance:TVkAccelerationStructureInstanceKHR;
       fAccelerationStructureInstancePointer:PVkAccelerationStructureInstanceKHR;
       fTag:TpvPtrInt;
       function GetTransform:TpvMatrix4x4;
       procedure SetTransform(const aTransform:TpvMatrix4x4);
       function GetInstanceCustomIndex:TpvUInt32;
       procedure SetInstanceCustomIndex(const aInstanceCustomIndex:TpvUInt32);
       function GetMask:TpvUInt32;
       procedure SetMask(const aMask:TpvUInt32);
       function GetInstanceShaderBindingTableRecordOffset:TpvUInt32;
       procedure SetInstanceShaderBindingTableRecordOffset(const aInstanceShaderBindingTableRecordOffset:TpvUInt32);
       function GetFlags:TVkGeometryInstanceFlagsKHR;
       procedure SetFlags(const aFlags:TVkGeometryInstanceFlagsKHR);
       function GetAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure;
       procedure SetAccelerationStructure(const aAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure);
       function GetAccelerationStructureDeviceAddress:TVkDeviceAddress;
       procedure SetAccelerationStructureDeviceAddress(const aAccelerationStructureDeviceAddress:TVkDeviceAddress); 
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aTransform:TpvMatrix4x4;
                          const aInstanceCustomIndex:TVkUInt32;
                          const aMask:TVkUInt32;
                          const aInstanceShaderBindingTableRecordOffset:TVkUInt32;
                          const aFlags:TVkGeometryInstanceFlagsKHR;
                          const aAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure;
                          const aAccelerationStructureInstancePointer:PVkAccelerationStructureInstanceKHR=nil); reintroduce;
       destructor Destroy; override;
      public
       property Transform:TpvMatrix4x4 read GetTransform write SetTransform;
      published
       property Device:TpvVulkanDevice read fDevice;
       property InstanceCustomIndex:TVkUInt32 read GetInstanceCustomIndex write SetInstanceCustomIndex;
       property Mask:TVkUInt32 read GetMask write SetMask;
       property InstanceShaderBindingTableRecordOffset:TVkUInt32 read GetInstanceShaderBindingTableRecordOffset write SetInstanceShaderBindingTableRecordOffset;
       property Flags:TVkGeometryInstanceFlagsKHR read GetFlags write SetFlags;
       property AccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure read GetAccelerationStructure write SetAccelerationStructure;
       property AccelerationStructureDeviceAddress:TVkDeviceAddress read GetAccelerationStructureDeviceAddress write SetAccelerationStructureDeviceAddress;
       property Tag:TpvPtrInt read fTag write fTag;
      public
       property AccelerationStructureInstance:PVkAccelerationStructureInstanceKHR read fAccelerationStructureInstancePointer write fAccelerationStructureInstancePointer;
     end;

     TpvRaytracingBottomLevelAccelerationStructureInstanceList=TpvObjectGenericList<TpvRaytracingBottomLevelAccelerationStructureInstance>;

     { TpvRaytracingTopLevelAccelerationStructure }
     TpvRaytracingTopLevelAccelerationStructure=class(TpvRaytracingAccelerationStructure)
      private
       fInstances:TVkAccelerationStructureGeometryInstancesDataKHR;
       fCountInstances:TVkUInt32;
       fBuildOffsetInfo:TVkAccelerationStructureBuildRangeInfoKHR;
       fGeometry:TVkAccelerationStructureGeometryKHR;
       fDynamicGeometry:Boolean;
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aInstanceAddress:TVkDeviceAddress=0;
                          const aInstanceCount:TVkUInt32=0;
                          const aFlags:TVkBuildAccelerationStructureFlagsKHR=0;
                          const aDynamicGeometry:Boolean=false); reintroduce;
       destructor Destroy; override;
       procedure Update(const aInstanceAddress:TVkDeviceAddress;
                        const aInstanceCount:TVkUInt32;
                        const aFlags:TVkBuildAccelerationStructureFlagsKHR=0;
                        const aDynamicGeometry:Boolean=false);
       procedure UpdateInstanceAddress(const aInstanceAddress:TVkDeviceAddress);
      public
       property Instances:TVkAccelerationStructureGeometryInstancesDataKHR read fInstances;
       property CountInstances:TVkUInt32 read fCountInstances;
      published
     end;

     { TpvRaytracingGeometryInfoManager }
     TpvRaytracingGeometryInfoManager=class
      public
       type TOnDefragmentMove=procedure(const aSender:TpvRaytracingGeometryInfoManager;const aObject:TObject;const aOldOffset,aNewOffset,aSize:TpvInt64) of object;
            TObjectList=TpvDynamicArrayList<TObject>;
      private
       fLock:TPasMPCriticalSection;
       fObjectList:TObjectList;
       fGeometryInfoList:TpvRaytracingBLASGeometryInfoBufferItemList;
       fBufferRangeAllocator:TpvBufferRangeAllocator;
       fSizeDirty:TPasMPBool32;
       fDirty:TPasMPBool32;
       fOnDefragmentMove:TOnDefragmentMove;
       procedure BufferRangeAllocatorOnResize(const aSender:TpvBufferRangeAllocator;const aNewCapacity:TpvInt64);
       procedure BufferRangeAllocatorOnDefragmentMove(const aSender:TpvBufferRangeAllocator;const aOldOffset,aNewOffset,aSize:TpvInt64);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       function AllocateGeometryInfoRange(const aObject:TObject;const aCount:TpvSizeInt):TpvSizeInt;
       procedure FreeGeometryInfoRange(const aOffset:TpvSizeInt);
       function GetGeometryInfo(const aIndex:TpvSizeInt):PpvRaytracingBLASGeometryInfoBufferItem;
       procedure Defragment;
      published
       property ObjectList:TObjectList read fObjectList;
       property GeometryInfoList:TpvRaytracingBLASGeometryInfoBufferItemList read fGeometryInfoList;       
       property BufferRangeAllocator:TpvBufferRangeAllocator read fBufferRangeAllocator; 
       property OnDefragmentMove:TOnDefragmentMove read fOnDefragmentMove write fOnDefragmentMove;
      public
       property SizeDirty:TPasMPBool32 read fSizeDirty write fSizeDirty; 
       property Dirty:TPasMPBool32 read fDirty write fDirty;
     end;  
     
     { TpvRaytracingBLASManager }
     TpvRaytracingBLASManager=class
      public 
       type { TBLAS }
            TBLAS=class
             public
              type TEnqueueState=(None,Build,Update);
                   { TBLASInstance }
                   TBLASInstance=class
                    private
                     fBLASManager:TpvRaytracingBLASManager;
                     fBLAS:TBLAS;
                     fInBLASManagerIndex:TpvSizeInt;
                     fInBLASIndex:TpvSizeInt;
                     fAccelerationStructureInstance:TpvRaytracingBottomLevelAccelerationStructureInstance;
                    public
                     constructor Create(const aBLAS:TBLAS;
                                        const aTransform:TpvMatrix4x4;
                                        const aMask:TVkUInt32;
                                        const aInstanceShaderBindingTableRecordOffset:TVkUInt32;
                                        const aFlags:TVkGeometryInstanceFlagsKHR); reintroduce;
                     destructor Destroy; override;
                     procedure AfterConstruction; override;
                     procedure BeforeDestruction; override;
                    public
                     property BLASManager:TpvRaytracingBLASManager read fBLASManager;
                     property BLAS:TBLAS read fBLAS;
                     property InBLASManagerIndex:TpvSizeInt read fInBLASManagerIndex;
                     property InBLASIndex:TpvSizeInt read fInBLASIndex;
                     property AccelerationStructureInstance:TpvRaytracingBottomLevelAccelerationStructureInstance read fAccelerationStructureInstance;
                   end; 
                   TBLASInstanceList=TpvObjectGenericList<TBLASInstance>;                   
             private
              fBLASManager:TpvRaytracingBLASManager;
              fInBLASManagerIndex:TpvSizeInt;
              fName:TpvUTF8String;
              fAllocationGroupID:TpvUInt64;
              fFlags:TVkBuildAccelerationStructureFlagsKHR;
              fDynamicGeometry:Boolean;
              fAccelerationStructureGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry;
              fAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure;
              fAccelerationStructureSize:TVkDeviceSize;
              fBuildScratchSize:TVkDeviceSize;
              fUpdateScratchSize:TVkDeviceSize;
              fScratchSize:TVkDeviceSize;
              fAccelerationStructureScratchSize:TVkDeviceSize;
              fAccelerationStructureBuffer:TpvVulkanBuffer;
              fScratchOffset:TVkDeviceSize;
              fScratchPass:TpvUInt64;
              fGeometryInfoBaseIndex:TpvSizeInt;
              fCountGeometries:TpvSizeInt;
              fBLASInstanceList:TBLASInstanceList;
              fEnqueueState:TEnqueueState;
              procedure UpdateBuffer;
             public
              constructor Create(const aBLASManager:TpvRaytracingBLASManager;
                                 const aFlags:TVkBuildAccelerationStructureFlagsKHR=0;
                                 const aDynamicGeometry:Boolean=false;
                                 const aAllocationGroupID:TpvUInt64=0;
                                 const aName:TpvUTF8String=''); reintroduce;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;              
              procedure Initialize;
              procedure Update;
              function GetGeometryInfo(const aIndex:TpvSizeInt):PpvRaytracingBLASGeometryInfoBufferItem;
              function AcquireBLASInstance(const aTransform:TpvMatrix4x4;
                                           const aMask:TVkUInt32;
                                           const aInstanceShaderBindingTableRecordOffset:TVkUInt32;
                                           const aFlags:TVkGeometryInstanceFlagsKHR):TBLASInstance;
              procedure ReleaseBLASInstance(const aBLASInstance:TBLASInstance);
              procedure Enqueue(const aUpdate:Boolean=false); // Enqueue for building or updating acceleration structure
             public
              property BLASManager:TpvRaytracingBLASManager read fBLASManager;
              property InBLASManagerIndex:TpvSizeInt read fInBLASManagerIndex;
              property Flags:TVkBuildAccelerationStructureFlagsKHR read fFlags write fFlags;
              property DynamicGeometry:Boolean read fDynamicGeometry write fDynamicGeometry;
              property AccelerationStructureGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry read fAccelerationStructureGeometry;
              property AccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure read fAccelerationStructure write fAccelerationStructure;
              property AccelerationStructureSize:TVkDeviceSize read fAccelerationStructureSize write fAccelerationStructureSize;
              property BuildScratchSize:TVkDeviceSize read fBuildScratchSize write fBuildScratchSize;
              property UpdateScratchSize:TVkDeviceSize read fUpdateScratchSize write fUpdateScratchSize;
              property ScratchSize:TVkDeviceSize read fScratchSize write fScratchSize;
              property AccelerationStructureScratchSize:TVkDeviceSize read fAccelerationStructureScratchSize write fAccelerationStructureScratchSize;
              property AccelerationStructureBuffer:TpvVulkanBuffer read fAccelerationStructureBuffer write fAccelerationStructureBuffer;
              property BLASInstanceList:TBLASInstanceList read fBLASInstanceList;
              property ScratchOffset:TVkDeviceSize read fScratchOffset write fScratchOffset;
              property ScratchPass:TpvUInt64 read fScratchPass write fScratchPass;
              property GeometryInfoBaseIndex:TpvSizeInt read fGeometryInfoBaseIndex write fGeometryInfoBaseIndex;
              property CountGeometries:TpvSizeInt read fCountGeometries write fCountGeometries;
            end;
            TBLASList=TpvObjectGenericList<TBLAS>;
            TGeometryOffsetArrayList=TpvDynamicArrayList<TVkUInt32>; // Instance offset index for first geometry buffer item per BLAS instance, when >= 24 bits are needed, since instance custom index is only 24 bits
            TOnMustWaitForPreviousFrame=function(aSender:TObject):Boolean of object;
            TOnUpdate=procedure(aSender:TObject) of object;
      private     
       procedure ReassignAccelerationStructureInstancePointers;
      private 
       fVulkanDevice:TpvVulkanDevice;
       fCountInFlightFrames:TpvSizeInt;
       fLock:TPasMPCriticalSection;
       fBLASList:TBLASList;
       fBLASInstanceList:TBLAS.TBLASInstanceList;
       fBLASQueue:TBLASList; // Queue for building or updating acceleration structures
       fBLASQueueLock:TPasMPSlimReaderWriterLock;
       fAccelerationStructureInstanceKHRArrayList:TpvRaytracingAccelerationStructureInstanceArrayList;
       fGeometryInfoManager:TpvRaytracingGeometryInfoManager;
       fGeometryOffsetArrayList:TGeometryOffsetArrayList; // As buffer on the GPU, contains the geometry info offset per BLAS instance, when >= 24 bits are needed, since the instance custom index is only 24 bits, we need to store the offset of the first geometry buffer item per BLAS instance, when >= 24 bits are needed
       fDirty:TPasMPBool32;
       fUpdateRaytracingFrameDoneMask:TPasMPUInt32;
       fRaytracingBLASListChanged:TPasMPBool32;
       fRaytracingMustUpdateTLAS:TPasMPBool32;
       fRaytracingBLASGeometryInfoOffsetBufferItemBuffers:array[0..1] of TpvVulkanBuffer;
       fRaytracingBLASGeometryInfoBufferItemBuffers:array[0..1] of TpvVulkanBuffer;
       fRaytracingBLASGeometryInfoBufferRingIndex:TpvInt32;
       fRaytracingAccelerationStructureBuildQueue:TpvRaytracingAccelerationStructureBuildQueue;
       fRaytracingEmptyInitialized:Boolean;
       fRaytracingEmptyVertexBuffer:TpvVulkanBuffer;
       fRaytracingEmptyIndexBuffer:TpvVulkanBuffer;
       fRaytracingEmptyBLAS:TpvRaytracingBLASManager.TBLAS;
       fRaytracingEmptyBLASInstance:TpvRaytracingBLASManager.TBLAS.TBLASInstance;
       fRaytracingEmptyBLASScratchBuffer:TpvVulkanBuffer;
       fRaytracingBLASScratchBuffer:TpvVulkanBuffer;
       fRaytracingTLASScratchBuffer:TpvVulkanBuffer;
       fRaytracingTLASBLASInstancesBuffer:TpvVulkanBuffer;
       fRaytracingTLASBLASInstancesBuffers:array[-1..MaxInFlightFrames] of TpvVulkanBuffer;
       fRaytracingTLASBLASInstancesBufferSize:TVkDeviceSize;
       fRaytracingTLASBuffer:TpvVulkanBuffer;
       fRaytracingTLAS:TpvRaytracingTopLevelAccelerationStructure;
       fRaytracingTLASAccelerationStructures:array[-1..MaxInFlightFrames-1] of TVkAccelerationStructureKHR;
       fRaytracingTLASGenerations:array[-1..MaxInFlightFrames-1] of TpvUInt64;
      private
       fStagingQueue:TpvVulkanQueue;
       fStagingCommandBuffer:TpvVulkanCommandBuffer;
       fStagingFence:TpvVulkanFence;
       fCommandBuffer:TpvVulkanCommandBuffer;
       fInFlightFrameIndex:TpvSizeInt;
       fMustTLASUpdate:Boolean;
      private
       fScratchSize:TVkDeviceSize;
       fScratchPassSize:TVkDeviceSize;
       fScratchPass:TpvUInt64;
      private
       fOnMustWaitForPreviousFrame:TOnMustWaitForPreviousFrame;
       fOnUpdate:TOnUpdate;
      private
       procedure GeometryInfoManagerOnDefragmentMove(const aSender:TpvRaytracingGeometryInfoManager;const aObject:TObject;const aOldOffset,aNewOffset,aSize:TpvInt64);
      private
       procedure HostMemoryBarrier;
       procedure WaitForPreviousFrame;
       procedure HandleEmptyBLAS;
       procedure ProcessContentUpdate;
       procedure BuildOrUpdateBLASMetaData;
       procedure CollectAndCalculateSizesForAccelerationStructures;
       procedure AllocateOrGrowOrShrinkScratchBuffer;
       procedure BuildOrUpdateAccelerationStructures;
       procedure UpdateBLASInstancesForTLAS;
       procedure CreateOrUpdateTLAS;
       procedure AllocateOrGrowTLASBuffer;
       procedure AllocateOrGrowTLASScratchBuffer;
       procedure BuildOrUpdateTLAS;
      public
       constructor Create(const aDevice:TpvVulkanDevice;
                          const aCountInFlightFrames:TpvSizeInt); reintroduce;
       destructor Destroy; override;
       function AcquireBLAS(const aFlags:TVkBuildAccelerationStructureFlagsKHR=0;
                            const aDynamicGeometry:Boolean=false;
                            const aAllocationGroupID:TpvUInt64=0;
                            const aName:TpvUTF8String=''):TBLAS;
       procedure ReleaseBLAS(const aBLAS:TBLAS);
       procedure Update(const aStagingQueue:TpvVulkanQueue;
                        const aStagingCommandBuffer:TpvVulkanCommandBuffer;
                        const aStagingFence:TpvVulkanFence;
                        const aCommandBuffer:TpvVulkanCommandBuffer;
                        const aInFlightFrameIndex:TpvSizeInt;
                        const aLabels:Boolean);
      public
       property Device:TpvVulkanDevice read fVulkanDevice;
       property BLASList:TBLASList read fBLASList;
       property BLASInstanceList:TBLAS.TBLASInstanceList read fBLASInstanceList;
       property AccelerationStructureInstanceKHRArrayList:TpvRaytracingAccelerationStructureInstanceArrayList read fAccelerationStructureInstanceKHRArrayList;
       property GeometryInfoManager:TpvRaytracingGeometryInfoManager read fGeometryInfoManager;
       property GeometryOffsetArrayList:TGeometryOffsetArrayList read fGeometryOffsetArrayList;
       property Dirty:TPasMPBool32 read fDirty write fDirty;
       property OnMustWaitForPreviousFrame:TOnMustWaitForPreviousFrame read fOnMustWaitForPreviousFrame write fOnMustWaitForPreviousFrame;
       property OnUpdate:TOnUpdate read fOnUpdate write fOnUpdate;
     end;   

     { TpvRaytracingManager }
     TpvRaytracingManager=class
      private
       fVulkanDevice:TpvVulkanDevice;
       fBLASManager:TpvRaytracingBLASManager;
      public
       constructor Create(const aDevice:TpvVulkanDevice); reintroduce;
       destructor Destroy; override;
      published
       property Device:TpvVulkanDevice read fVulkanDevice;
       property BLASManager:TpvRaytracingBLASManager read fBLASManager;
     end; 

implementation

{ TpvRaytracingCompactedSizeQueryPool }

constructor TpvRaytracingCompactedSizeQueryPool.Create(const aDevice:TpvVulkanDevice);
begin

 inherited Create;

 fDevice:=aDevice;

 FillChar(fQueryPoolCreateInfo,SizeOf(TVkQueryPoolCreateInfo),#0);
 fQueryPoolCreateInfo.sType:=VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO;
 fQueryPoolCreateInfo.pNext:=nil;
 fQueryPoolCreateInfo.flags:=0;
 fQueryPoolCreateInfo.queryType:=VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR;
 fQueryPoolCreateInfo.queryCount:=0;

 fQueryPool:=VK_NULL_HANDLE; // Not created yet

 fCount:=0;

 fAccelerationStructures:=TpvRaytracingAccelerationStructureList.Create;

 fAccelerationStructureList:=TAccelerationStructureList.Create;

 fAccelerationStructureIndexHashMap:=TAccelerationStructureIndexHashMap.Create(-1);

 fResultAccelerationStructureIndexHashMap:=TAccelerationStructureIndexHashMap.Create(-1);

 fCompactedSizes:=TCompactedSizes.Create;

end;

destructor TpvRaytracingCompactedSizeQueryPool.Destroy;
begin

 FreeAndNil(fCompactedSizes);

 FreeAndNil(fAccelerationStructureIndexHashMap);

 FreeAndNil(fResultAccelerationStructureIndexHashMap);

 FreeAndNil(fAccelerationStructureList);

 FreeAndNil(fAccelerationStructures);

 if fQueryPool<>VK_NULL_HANDLE then begin
  try
   fDevice.Commands.DestroyQueryPool(fDevice.Handle,fQueryPool,nil);
  finally 
   fQueryPool:=VK_NULL_HANDLE;
  end; 
 end;

 inherited Destroy;

end;

function TpvRaytracingCompactedSizeQueryPool.Empty:boolean;
begin
 result:=fCount=0;
end;

function TpvRaytracingCompactedSizeQueryPool.Ready:boolean;
begin
 result:=(fCount>0) and (fCount=fAccelerationStructures.Count);
end;

procedure TpvRaytracingCompactedSizeQueryPool.Reset;
begin
 
 fCount:=0;

 fAccelerationStructures.ClearNoFree;

 fAccelerationStructureList.ClearNoFree;

 fAccelerationStructureIndexHashMap.Clear;

 fCompactedSizes.ClearNoFree;

end;

procedure TpvRaytracingCompactedSizeQueryPool.AddAccelerationStructure(const aAccelerationStructure:TpvRaytracingAccelerationStructure);
begin
 if not fAccelerationStructureIndexHashMap.ExistKey(aAccelerationStructure.AccelerationStructure) then begin
  fAccelerationStructures.Add(aAccelerationStructure);
  fAccelerationStructureList.Add(aAccelerationStructure.fAccelerationStructure);
  fAccelerationStructureIndexHashMap[aAccelerationStructure.AccelerationStructure]:=fCount;
  inc(fCount);
 end; 
end;

procedure TpvRaytracingCompactedSizeQueryPool.Query(const aCommandBuffer:TpvVulkanCommandBuffer);
var MemoryBarrier:TVkMemoryBarrier;
begin
 
 if fCount>0 then begin 

  // Create acceleration structure compacted size query pool, if it's not created yet or recreate if the count of acceleration 
  // structures has changed, because we need to query the compacted size of each acceleration structure
  if (fQueryPool=VK_NULL_HANDLE) or (fCount>fQueryPoolCreateInfo.queryCount) then begin

   fQueryPoolCreateInfo.queryCount:=fCount;

   // If query pool is already created, destroy it first, in the case that there are more acceleration structures than before 
   if fQueryPool<>VK_NULL_HANDLE then begin
    try
     fDevice.Commands.DestroyQueryPool(fDevice.Handle,fQueryPool,nil);
    finally
     fQueryPool:=VK_NULL_HANDLE;
    end;
   end;

   // Create or re-create query pool
   VulkanCheckResult(fDevice.Commands.CreateQueryPool(fDevice.Handle,@fQueryPoolCreateInfo,nil,@fQueryPool));
    
  end;

  // Memory barrier for acceleration structure compacted size query for to be sure that the acceleration structure is in a valid state beforehand
  FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
  MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
  MemoryBarrier.pNext:=nil;
  MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR) or TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR);
  MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR) or TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR);
  fDevice.Commands.CmdPipelineBarrier(aCommandBuffer.Handle,
                                      TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),
                                      TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),
                                      0,
                                      1,@MemoryBarrier,
                                      0,nil,
                                      0,nil);

  // Reset query pool
  fDevice.Commands.CmdResetQueryPool(aCommandBuffer.Handle,fQueryPool,0,fCount);

  // Write acceleration structure compacted size queries 
  fDevice.Commands.CmdWriteAccelerationStructuresPropertiesKHR(aCommandBuffer.Handle,
                                                               fCount,
                                                               @fAccelerationStructureList.ItemArray[0],
                                                               VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR,
                                                               fQueryPool,
                                                               0);

 end;

end;

procedure TpvRaytracingCompactedSizeQueryPool.GetResults;
var TemporaryAccelerationStructureIndexHashMap:TAccelerationStructureIndexHashMap;
begin
 
 // Get results of acceleration structure compacted size queries
 if fCount>0 then begin

  try 
 
   fCompactedSizes.ClearNoFree;
 
   fCompactedSizes.Resize(fCount);
 
   VulkanCheckResult(fDevice.Commands.GetQueryPoolResults(fDevice.Handle,
                                                          fQueryPool,
                                                          0,
                                                          fCount,
                                                          fCount*SizeOf(TVkDeviceSize),
                                                          @fCompactedSizes.ItemArray[0],
                                                          SizeOf(TVkDeviceSize),
                                                          TVkQueryResultFlags(VK_QUERY_RESULT_64_BIT) or TVkQueryResultFlags(VK_QUERY_RESULT_WAIT_BIT)));

   // Swap acceleration structure index hash maps
   TemporaryAccelerationStructureIndexHashMap:=fAccelerationStructureIndexHashMap;
   fAccelerationStructureIndexHashMap:=fResultAccelerationStructureIndexHashMap;
   fResultAccelerationStructureIndexHashMap:=TemporaryAccelerationStructureIndexHashMap;

   // Clear acceleration structure index hash map
   fAccelerationStructureIndexHashMap.Clear(false);

   // Clear acceleration structures
   fAccelerationStructures.ClearNoFree;
   fAccelerationStructureList.ClearNoFree;

  finally   
   fCount:=0; // Reset count, but don't clear the result list, since these will queried later after this function call
  end;                                                         

 end;

end;

function TpvRaytracingCompactedSizeQueryPool.GetCompactedSizeByIndex(const aIndex:TpvSizeInt):TVkDeviceSize;
begin
 if (aIndex>=0) and (aIndex<fCompactedSizes.Count) then begin
  result:=fCompactedSizes[aIndex];
 end else begin
  result:=0;
 end;
end;

function TpvRaytracingCompactedSizeQueryPool.GetCompactedSizeByAccelerationStructure(const aAccelerationStructure:TpvRaytracingAccelerationStructure):TVkDeviceSize;
var Index:TpvSizeInt;
begin
 Index:=fResultAccelerationStructureIndexHashMap[aAccelerationStructure.AccelerationStructure];
 if (Index>=0) and (Index<fCompactedSizes.Count) then begin
  result:=fCompactedSizes[Index];
 end else begin
  result:=0;
 end;
end;

{ TpvRaytracingBLASGeometryInfoBufferItem }

constructor TpvRaytracingBLASGeometryInfoBufferItem.Create(const aType_:TVkUInt32;
                                                           const aObjectIndex:TVkUInt32;
                                                           const aMaterialIndex:TVkUInt32;
                                                           const aIndexOffset:TVkUInt32);
begin
 fType_:=aType_;
 fObjectIndex:=aObjectIndex;
 fMaterialIndex:=aMaterialIndex;
 fIndexOffset:=aIndexOffset;
end;

{ TpvRaytracingInstanceCustomIndexManager }

constructor TpvRaytracingInstanceCustomIndexManager.Create;
begin
 
 inherited Create;
 
 fItems:=TItemList.Create;

 fItemHashMap:=TItemHashMap.Create(-1);
 
 fFreeList.Initialize;
  
end;

destructor TpvRaytracingInstanceCustomIndexManager.Destroy;
begin

 FreeAndNil(fItems);

 FreeAndNil(fItemHashMap);

 fFreeList.Finalize;

 inherited Destroy;

end;

function TpvRaytracingInstanceCustomIndexManager.Add(const aData:TpvPtrUInt):TpvInt32;
var Index:TpvSizeInt;
    Item:PItem;
begin

 if aData>0 then begin

  if not fFreeList.Pop(result) then begin
   result:=fItems.AddNewIndex;
  end;

  if result>=(1 shl 24) then begin
   raise EpvRaytracing.Create('Instance custom index overflow, Vulkan raytracing instance custom index is limited to 24 bits');
  end;

  Item:=@fItems.ItemArray[result];
  Item^.Index:=result;
  Item^.Data:=aData;  
  
  fItemHashMap.Add(aData,result);

 end else begin
   
  result:=-1;

 end;

end;

function TpvRaytracingInstanceCustomIndexManager.RemoveData(const aData:TpvPtrUInt):boolean;
var Index:TpvSizeInt;
    Item:PItem;
begin

 result:=false;

 if aData>0 then begin

  Index:=fItemHashMap[aData];
  if Index>=0 then begin
   
   Item:=@fItems.ItemArray[Index];
   if Item^.Data=aData then begin
   
    Item^.Data:=0;
    fItemHashMap.Delete(aData);
    fFreeList.Push(Index);
    result:=true;

   end;

  end;

 end;

end;

function TpvRaytracingInstanceCustomIndexManager.RemoveIndex(const aIndex:TpvInt32):boolean;
var Item:PItem;
begin

 result:=false;

 if (aIndex>=0) and (aIndex<fItems.Count) then begin

  Item:=@fItems.ItemArray[aIndex];
  if Item^.Data>0 then begin

   fItemHashMap.Delete(Item^.Data);
   Item^.Data:=0;
   fFreeList.Push(aIndex);
   result:=true;

  end;

 end;

end;

function TpvRaytracingInstanceCustomIndexManager.GetData(const aIndex:TpvInt32):TpvPtrUInt;
begin
 if (aIndex>=0) and (aIndex<fItems.Count) then begin
  result:=fItems.ItemArray[aIndex].Data;
 end else begin
  result:=0;
 end;
end;

function TpvRaytracingInstanceCustomIndexManager.GetIndex(const aData:TpvPtrUInt):TpvInt32;
begin
 result:=fItemHashMap[aData];
end;

{ TpvRaytracingAccelerationStructureBuildQueue }

constructor TpvRaytracingAccelerationStructureBuildQueue.Create(const aDevice:TpvVulkanDevice);
begin
 inherited Create;
 fDevice:=aDevice;
 fBuildGeometryInfos:=TBuildGeometryInfos.Create;
 fBuildOffsetInfoPtrs:=TBuildOffsetInfoPtrs.Create;
end;

destructor TpvRaytracingAccelerationStructureBuildQueue.Destroy;
begin
 FreeAndNil(fBuildGeometryInfos);
 FreeAndNil(fBuildOffsetInfoPtrs);
 inherited Destroy;
end;

procedure TpvRaytracingAccelerationStructureBuildQueue.Clear;
begin
 fBuildGeometryInfos.ClearNoFree;
 fBuildOffsetInfoPtrs.ClearNoFree;
end;

function TpvRaytracingAccelerationStructureBuildQueue.Empty:Boolean;
begin
 result:=fBuildGeometryInfos.Count=0;
end;

procedure TpvRaytracingAccelerationStructureBuildQueue.Enqueue(const aBuildGeometryInfo:TVkAccelerationStructureBuildGeometryInfoKHR;
                                                               const aBuildOffsetInfoPtr:PVkAccelerationStructureBuildRangeInfoKHR);
begin
 fBuildGeometryInfos.Add(aBuildGeometryInfo);
 fBuildOffsetInfoPtrs.Add(aBuildOffsetInfoPtr);
end;

procedure TpvRaytracingAccelerationStructureBuildQueue.Execute(const aCommandBuffer:TpvVulkanCommandBuffer);
var Index:TpvSizeInt;
begin
 Assert(assigned(aCommandBuffer));
 Assert(fDevice=aCommandBuffer.Device);
 if fBuildGeometryInfos.Count>0 then begin
  Assert(fBuildGeometryInfos.Count=fBuildOffsetInfoPtrs.Count);
  try
   if assigned(pvApplication) and pvApplication.VulkanDebugging then begin
    // This is the workaround for newer vulkan validation layer versions > 1.x.275
    for Index:=0 to fBuildGeometryInfos.Count-1 do begin
     fDevice.Commands.Commands.CmdBuildAccelerationStructuresKHR(aCommandBuffer.Handle,
                                                                 1,
                                                                 @fBuildGeometryInfos.ItemArray[Index],
                                                                 @fBuildOffsetInfoPtrs.ItemArray[Index]);
    end;
   end else begin
    // This crashes newer vulkan validation layer versions > 1.x.275
    fDevice.Commands.Commands.CmdBuildAccelerationStructuresKHR(aCommandBuffer.Handle,
                                                                fBuildGeometryInfos.Count,
                                                                @fBuildGeometryInfos.ItemArray[0],
                                                                @fBuildOffsetInfoPtrs.ItemArray[0]);
   end;
  finally
   Clear;
  end; 
 end;
end;

{ TpvRaytracingAccelerationStructure }

constructor TpvRaytracingAccelerationStructure.Create(const aDevice:TpvVulkanDevice;const aAccelerationStructureType:TVkAccelerationStructureTypeKHR=TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR));
begin

 inherited Create;

 fDevice:=aDevice;

 fAccelerationStructure:=VK_NULL_HANDLE;

 fAccelerationStructureType:=aAccelerationStructureType;

 FillChar(fBuildGeometryInfo,SizeOf(TVkAccelerationStructureBuildGeometryInfoKHR),#0);
 fBuildGeometryInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
 fBuildGeometryInfo.pNext:=nil;
 fBuildGeometryInfo.type_:=fAccelerationStructureType;
 fBuildGeometryInfo.flags:=0;
 fBuildGeometryInfo.mode:=TVkBuildAccelerationStructureModeKHR(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR);
 fBuildGeometryInfo.srcAccelerationStructure:=VK_NULL_HANDLE;
 fBuildGeometryInfo.dstAccelerationStructure:=VK_NULL_HANDLE;
 fBuildGeometryInfo.geometryCount:=0;
 fBuildGeometryInfo.pGeometries:=nil;
 fBuildGeometryInfo.ppGeometries:=nil;
 fBuildGeometryInfo.scratchData.deviceAddress:=0;
 fBuildGeometryInfo.scratchData.hostAddress:=nil;

 FillChar(fBuildSizesInfo,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
 fBuildSizesInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
 fBuildSizesInfo.pNext:=nil;
 fBuildSizesInfo.accelerationStructureSize:=0;
 fBuildSizesInfo.updateScratchSize:=0;
 fBuildSizesInfo.buildScratchSize:=0;

 fGeneration:=0;

end;

destructor TpvRaytracingAccelerationStructure.Destroy;
begin
 Finalize;
 inherited Destroy;
end;

class function TpvRaytracingAccelerationStructure.Reduce(const aStructures:TpvRaytracingAccelerationStructureList):TVkAccelerationStructureBuildSizesInfoKHR;
var Index:TpvSizeInt;
    Current:TpvRaytracingAccelerationStructure;
begin
 
 FillChar(result,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
 result.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
 result.pNext:=nil;
 result.accelerationStructureSize:=0;
 result.updateScratchSize:=0;
 result.buildScratchSize:=0;

 for Index:=0 to aStructures.Count-1 do begin
  Current:=aStructures[Index];
  if assigned(Current) then begin
   result.accelerationStructureSize:=result.accelerationStructureSize+Current.fBuildSizesInfo.accelerationStructureSize;
   result.updateScratchSize:=result.updateScratchSize+Current.fBuildSizesInfo.updateScratchSize;
   result.buildScratchSize:=result.buildScratchSize+Current.fBuildSizesInfo.buildScratchSize;
  end;
 end;

end;

function TpvRaytracingAccelerationStructure.GetMemorySizes(const aCounts:PVkUInt32):TVkAccelerationStructureBuildSizesInfoKHR;
begin

 FillChar(result,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
 result.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
 result.pNext:=nil;
 result.accelerationStructureSize:=0;
 result.updateScratchSize:=0;
 result.buildScratchSize:=0;

 fDevice.Commands.Commands.GetAccelerationStructureBuildSizesKHR(fDevice.Handle,
                                                                 VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
                                                                 @fBuildGeometryInfo,
                                                                 aCounts,
                                                                 @result);

 result.accelerationStructureSize:=RoundUp64(result.accelerationStructureSize,256);                                                                
 result.updateScratchSize:=RoundUp64(result.updateScratchSize,TVkDeviceSize(fDevice.PhysicalDevice.AccelerationStructurePropertiesKHR.minAccelerationStructureScratchOffsetAlignment));
 result.buildScratchSize:=RoundUp64(result.buildScratchSize,TVkDeviceSize(fDevice.PhysicalDevice.AccelerationStructurePropertiesKHR.minAccelerationStructureScratchOffsetAlignment));

end;

procedure TpvRaytracingAccelerationStructure.Initialize(const aResultBuffer:TpvVulkanBuffer;const aResultOffset:TVkDeviceSize);
var CreateInfo:TVkAccelerationStructureCreateInfoKHR;
begin

 if fAccelerationStructure=VK_NULL_HANDLE then begin

  FillChar(CreateInfo,SizeOf(TVkAccelerationStructureCreateInfoKHR),#0);
  CreateInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR;
  CreateInfo.pNext:=nil;
  CreateInfo.type_:=fBuildGeometryInfo.type_;
  CreateInfo.size:=fBuildSizesInfo.accelerationStructureSize;
  CreateInfo.buffer:=aResultBuffer.Handle;
  CreateInfo.offset:=aResultOffset;

  VulkanCheckResult(fDevice.Commands.Commands.CreateAccelerationStructureKHR(fDevice.Handle,@CreateInfo,nil,@fAccelerationStructure));

  inc(fGeneration);

 end;

end;

procedure TpvRaytracingAccelerationStructure.Finalize;
begin
 if fAccelerationStructure<>VK_NULL_HANDLE then begin
  try
   fDevice.Commands.Commands.DestroyAccelerationStructureKHR(fDevice.Handle,fAccelerationStructure,nil);
  finally
   fAccelerationStructure:=VK_NULL_HANDLE;
  end;
 end;
end;

procedure TpvRaytracingAccelerationStructure.Build(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                   const aScratchBuffer:TpvVulkanBuffer;
                                                   const aScratchBufferOffset:TVkDeviceSize;
                                                   const aUpdate:Boolean;   
                                                   const aSourceAccelerationStructure:TpvRaytracingAccelerationStructure;
                                                   const aQueue:TpvRaytracingAccelerationStructureBuildQueue);
begin

 Assert(assigned(aCommandBuffer));
 Assert(fDevice=aCommandBuffer.Device);
 Assert(fAccelerationStructure<>VK_NULL_HANDLE);
 Assert(aScratchBuffer.Handle<>VK_NULL_HANDLE);
 Assert((not assigned(aSourceAccelerationStructure)) or (aSourceAccelerationStructure.fDevice=aCommandBuffer.Device));
 Assert((not assigned(aSourceAccelerationStructure)) or (aSourceAccelerationStructure.fAccelerationStructure<>VK_NULL_HANDLE));
 
 if aUpdate then begin

  // Update acceleration structure, either in-place or from another acceleration structure as source

  fBuildGeometryInfo.mode:=TVkBuildAccelerationStructureModeKHR(VK_BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR);
 
  if assigned(aSourceAccelerationStructure) then begin
   fBuildGeometryInfo.srcAccelerationStructure:=aSourceAccelerationStructure.fAccelerationStructure; // Update from another acceleration structure as source
  end else begin
   fBuildGeometryInfo.srcAccelerationStructure:=fAccelerationStructure; // In-place update
  end;

 end else begin

  // Build new acceleration structure
  
  fBuildGeometryInfo.mode:=TVkBuildAccelerationStructureModeKHR(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR);
  
  fBuildGeometryInfo.srcAccelerationStructure:=VK_NULL_HANDLE; // No source acceleration structure for new build

 end;

 fBuildGeometryInfo.dstAccelerationStructure:=fAccelerationStructure;
 
 fBuildGeometryInfo.scratchData.deviceAddress:=aScratchBuffer.DeviceAddress+aScratchBufferOffset;

 if assigned(aQueue) then begin

  // Enqueue build acceleration structure command to queue for parallel building

  aQueue.Enqueue(fBuildGeometryInfo,
                 fBuildOffsetInfoPtr);

 end else begin

  // Build acceleration structure directly as single command

  fDevice.Commands.Commands.CmdBuildAccelerationStructuresKHR(aCommandBuffer.Handle,
                                                              1,@fBuildGeometryInfo,                                                             
                                                              @fBuildOffsetInfoPtr);

 end;

end;

procedure TpvRaytracingAccelerationStructure.CopyFrom(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                      const aSourceAccelerationStructure:TpvRaytracingAccelerationStructure;
                                                      const aCompact:Boolean);
var CopyAccelerationStructureInfo:TVkCopyAccelerationStructureInfoKHR;
begin

 Assert(assigned(aCommandBuffer));
 Assert(assigned(aSourceAccelerationStructure));
 Assert(aSourceAccelerationStructure.fDevice=aCommandBuffer.Device);
 Assert(fDevice=aCommandBuffer.Device);
 Assert(aSourceAccelerationStructure.fAccelerationStructure<>VK_NULL_HANDLE);
 Assert(fAccelerationStructure<>VK_NULL_HANDLE);

 FillChar(CopyAccelerationStructureInfo,SizeOf(TVkCopyAccelerationStructureInfoKHR),#0);
 CopyAccelerationStructureInfo.sType:=VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR;
 CopyAccelerationStructureInfo.pNext:=nil;
 CopyAccelerationStructureInfo.src:=aSourceAccelerationStructure.AccelerationStructure;
 CopyAccelerationStructureInfo.dst:=fAccelerationStructure;
 if aCompact then begin
  CopyAccelerationStructureInfo.mode:=VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR;
 end else begin
  CopyAccelerationStructureInfo.mode:=VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR;
 end; 

 fDevice.Commands.Commands.CmdCopyAccelerationStructureKHR(aCommandBuffer.Handle,@CopyAccelerationStructureInfo);

end;

class procedure TpvRaytracingAccelerationStructure.MemoryBarrier(const aCommandBuffer:TpvVulkanCommandBuffer);
var MemoryBarrier:TVkMemoryBarrier;
begin
 
 FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
 MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 MemoryBarrier.pNext:=nil;
 MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR) or TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR);
 MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR) or TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR);
 
 aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),
                                   0,
                                   1,@MemoryBarrier,
                                   0,nil,
                                   0,nil);

end;

{ TpvRaytracingBottomLevelAccelerationStructureGeometry }

constructor TpvRaytracingBottomLevelAccelerationStructureGeometry.Create(const aDevice:TpvVulkanDevice);
begin
 inherited Create;
 fDevice:=aDevice;
 fGeometries:=TGeometries.Create;
 fBuildOffsets:=TBuildOffsets.Create;
end;

destructor TpvRaytracingBottomLevelAccelerationStructureGeometry.Destroy;
begin
 FreeAndNil(fGeometries);
 FreeAndNil(fBuildOffsets);
 inherited Destroy;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureGeometry.AddTriangles(const aVertexBuffer:TpvVulkanBuffer;
                                                                             const aVertexOffset:TVkUInt32;
                                                                             const aVertexCount:TVkUInt32;
                                                                             const aVertexStride:TVkDeviceSize;
                                                                             const aIndexBuffer:TpvVulkanBuffer;
                                                                             const aIndexOffset:TVkUInt32;
                                                                             const aIndexCount:TVkUInt32;
                                                                             const aOpaque:Boolean;
                                                                             const aTransformBuffer:TpvVulkanBuffer;
                                                                             const aTransformOffset:TVkDeviceSize);
var Geometry:TVkAccelerationStructureGeometryKHR;
    BuildOffsetInfo:TVkAccelerationStructureBuildRangeInfoKHR;
begin

 FillChar(Geometry,SizeOf(TVkAccelerationStructureGeometryKHR),#0);
 Geometry.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
 Geometry.pNext:=nil;
 Geometry.geometryType:=TVkGeometryTypeKHR(VK_GEOMETRY_TYPE_TRIANGLES_KHR);
 Geometry.geometry.triangles.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR;
 Geometry.geometry.triangles.pNext:=nil;
 Geometry.geometry.triangles.vertexData.deviceAddress:=aVertexBuffer.DeviceAddress;
 Geometry.geometry.triangles.vertexStride:=aVertexStride;
 Geometry.geometry.triangles.maxVertex:=aVertexCount;
 Geometry.geometry.triangles.vertexFormat:=VK_FORMAT_R32G32B32_SFLOAT;
 Geometry.geometry.triangles.indexData.deviceAddress:=aIndexBuffer.DeviceAddress;
 Geometry.geometry.triangles.indexType:=TVkIndexType(VK_INDEX_TYPE_UINT32);
 if assigned(aTransformBuffer) then begin
  Geometry.geometry.triangles.transformData.deviceAddress:=aTransformBuffer.DeviceAddress;
 end else begin
  Geometry.geometry.triangles.transformData.deviceAddress:=0;
 end;
 Geometry.flags:=TVkGeometryFlagsKHR(0);
 if aOpaque then begin
  Geometry.flags:=Geometry.flags or TVkGeometryFlagsKHR(VK_GEOMETRY_OPAQUE_BIT_KHR);
 end;

 FillChar(BuildOffsetInfo,SizeOf(TVkAccelerationStructureBuildRangeInfoKHR),#0);
 BuildOffsetInfo.firstVertex:=aVertexOffset;
 BuildOffsetInfo.primitiveOffset:=aIndexOffset;
 BuildOffsetInfo.primitiveCount:=aIndexCount div 3;
 if assigned(aTransformBuffer) then begin
  BuildOffsetInfo.transformOffset:=aTransformOffset;
 end else begin
  BuildOffsetInfo.transformOffset:=0;
 end;

 fGeometries.Add(Geometry);
 fBuildOffsets.Add(BuildOffsetInfo);

end;

procedure TpvRaytracingBottomLevelAccelerationStructureGeometry.AddAABBs(const aAABBBuffer:TpvVulkanBuffer;
                                                                         const aOffset:TVkDeviceSize;
                                                                         const aCount:TVkUInt32;
                                                                         const aOpaque:Boolean;
                                                                         const aStride:TVkDeviceSize);
var Geometry:TVkAccelerationStructureGeometryKHR;
    BuildOffsetInfo:TVkAccelerationStructureBuildRangeInfoKHR;
begin

 FillChar(Geometry,SizeOf(TVkAccelerationStructureGeometryKHR),#0);
 Geometry.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
 Geometry.pNext:=nil;
 Geometry.geometryType:=TVkGeometryTypeKHR(VK_GEOMETRY_TYPE_AABBS_KHR);
 Geometry.geometry.aabbs.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR;
 Geometry.geometry.aabbs.pNext:=nil;
 Geometry.geometry.aabbs.data.deviceAddress:=aAABBBuffer.DeviceAddress;
 Geometry.geometry.aabbs.stride:=aStride;
 Geometry.flags:=TVkGeometryFlagsKHR(0);
 if aOpaque then begin
  Geometry.flags:=Geometry.flags or TVkGeometryFlagsKHR(VK_GEOMETRY_OPAQUE_BIT_KHR);
 end;

 FillChar(BuildOffsetInfo,SizeOf(TVkAccelerationStructureBuildRangeInfoKHR),#0);
 BuildOffsetInfo.firstVertex:=0;
 BuildOffsetInfo.primitiveOffset:=aOffset;
 BuildOffsetInfo.primitiveCount:=aCount;
 BuildOffsetInfo.transformOffset:=0;

 fGeometries.Add(Geometry);
 fBuildOffsets.Add(BuildOffsetInfo);

end;

{ TpvRaytracingBottomLevelAccelerationStructure }

constructor TpvRaytracingBottomLevelAccelerationStructure.Create(const aDevice:TpvVulkanDevice;
                                                                 const aGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry;
                                                                 const aFlags:TVkBuildAccelerationStructureFlagsKHR;
                                                                 const aDynamicGeometry:Boolean);
var Index:TpvSizeInt;
    MaxPrimCount:TpvUInt32DynamicArray;
begin

 inherited Create(aDevice,TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR));

 fGeometry:=aGeometry;

 fDynamicGeometry:=aDynamicGeometry;

 FillChar(fBuildGeometryInfo,SizeOf(TVkAccelerationStructureBuildGeometryInfoKHR),#0);
 fBuildGeometryInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
 fBuildGeometryInfo.pNext:=nil;
 fBuildGeometryInfo.flags:=aFlags;
 if assigned(fGeometry) then begin
  fBuildGeometryInfo.geometryCount:=fGeometry.fGeometries.Count;
  fBuildGeometryInfo.pGeometries:=@fGeometry.fGeometries.ItemArray[0];
 end; 
 fBuildGeometryInfo.mode:=TVkBuildAccelerationStructureModeKHR(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR);
 fBuildGeometryInfo.type_:=TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR);
 fBuildGeometryInfo.srcAccelerationStructure:=VK_NULL_HANDLE; 

 if assigned(fGeometry) then begin

  MaxPrimCount:=nil;
  try
   
   SetLength(MaxPrimCount,fGeometry.fBuildOffsets.Count);

   for Index:=0 to fGeometry.fBuildOffsets.Count-1 do begin
    MaxPrimCount[Index]:=fGeometry.fBuildOffsets.Items[Index].primitiveCount;
   end;

   fBuildSizesInfo:=GetMemorySizes(@MaxPrimCount[0]);

  finally
   MaxPrimCount:=nil;  
  end;

  fBuildOffsetInfoPtr:=@fGeometry.fBuildOffsets.ItemArray[0];

 end else begin

  FillChar(fBuildSizesInfo,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
  fBuildSizesInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
  fBuildSizesInfo.pNext:=nil;
  fBuildSizesInfo.accelerationStructureSize:=0;
  fBuildSizesInfo.updateScratchSize:=0;
  fBuildSizesInfo.buildScratchSize:=0;

  fBuildOffsetInfoPtr:=nil;

 end; 

end;

destructor TpvRaytracingBottomLevelAccelerationStructure.Destroy;
begin
 inherited Destroy;
end;

procedure TpvRaytracingBottomLevelAccelerationStructure.Update(const aGeometry:TpvRaytracingBottomLevelAccelerationStructureGeometry;
                                                               const aFlags:TVkBuildAccelerationStructureFlagsKHR;
                                                               const aDynamicGeometry:Boolean);
var Index:TpvSizeInt;
    MaxPrimCount:TpvUInt32DynamicArray;
begin

 fGeometry:=aGeometry;

 fDynamicGeometry:=aDynamicGeometry;

 FillChar(fBuildGeometryInfo,SizeOf(TVkAccelerationStructureBuildGeometryInfoKHR),#0);
 fBuildGeometryInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
 fBuildGeometryInfo.pNext:=nil;
 fBuildGeometryInfo.flags:=aFlags;
 if assigned(fGeometry) then begin
  fBuildGeometryInfo.geometryCount:=fGeometry.fGeometries.Count;
  fBuildGeometryInfo.pGeometries:=@fGeometry.fGeometries.ItemArray[0];
 end;
 fBuildGeometryInfo.mode:=TVkBuildAccelerationStructureModeKHR(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR);
 fBuildGeometryInfo.type_:=TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR);
 fBuildGeometryInfo.srcAccelerationStructure:=VK_NULL_HANDLE;

 if assigned(fGeometry) then begin

  MaxPrimCount:=nil;
  try
   
   SetLength(MaxPrimCount,fGeometry.fBuildOffsets.Count);

   for Index:=0 to fGeometry.fBuildOffsets.Count-1 do begin
    MaxPrimCount[Index]:=fGeometry.fBuildOffsets.Items[Index].primitiveCount;
   end;

   fBuildSizesInfo:=GetMemorySizes(@MaxPrimCount[0]);

  finally
   MaxPrimCount:=nil;  
  end;

  fBuildOffsetInfoPtr:=@fGeometry.fBuildOffsets.ItemArray[0];

 end else begin

  FillChar(fBuildSizesInfo,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
  fBuildSizesInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
  fBuildSizesInfo.pNext:=nil;
  fBuildSizesInfo.accelerationStructureSize:=0;
  fBuildSizesInfo.updateScratchSize:=0;
  fBuildSizesInfo.buildScratchSize:=0;

  fBuildOffsetInfoPtr:=nil;

 end;

end;

{ TpvRaytracingBottomLevelAccelerationStructureInstance }

constructor TpvRaytracingBottomLevelAccelerationStructureInstance.Create(const aDevice:TpvVulkanDevice;
                                                                         const aTransform:TpvMatrix4x4;
                                                                         const aInstanceCustomIndex:TVkUInt32;
                                                                         const aMask:TVkUInt32;
                                                                         const aInstanceShaderBindingTableRecordOffset:TVkUInt32;
                                                                         const aFlags:TVkGeometryInstanceFlagsKHR;
                                                                         const aAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure;
                                                                         const aAccelerationStructureInstancePointer:PVkAccelerationStructureInstanceKHR);
begin

 inherited Create;

 fDevice:=aDevice;

 if assigned(aAccelerationStructureInstancePointer) then begin
  fAccelerationStructureInstancePointer:=aAccelerationStructureInstancePointer;
 end else begin
  fAccelerationStructureInstancePointer:=@fAccelerationStructureInstance;
 end;

 FillChar(fAccelerationStructureInstancePointer^,SizeOf(TVkAccelerationStructureInstanceKHR),#0);

 SetTransform(aTransform);

 fAccelerationStructureInstancePointer^.instanceCustomIndex:=aInstanceCustomIndex;
 fAccelerationStructureInstancePointer^.mask:=aMask;
 fAccelerationStructureInstancePointer^.instanceShaderBindingTableRecordOffset:=aInstanceShaderBindingTableRecordOffset;
 fAccelerationStructureInstancePointer^.flags:=aFlags;
 fAccelerationStructureInstancePointer^.accelerationStructureReference:=0;

 fAccelerationStructure:=nil;
 
 SetAccelerationStructure(aAccelerationStructure);

 fTag:=0;

end;

destructor TpvRaytracingBottomLevelAccelerationStructureInstance.Destroy;
begin
 inherited Destroy;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetTransform:TpvMatrix4x4;
begin
{PVkTransformMatrixKHR(Pointer(@result))^:=fAccelerationStructureInstancePointer^.transform;
 result.RawComponents[3,0]:=0.0;
 result.RawComponents[3,1]:=0.0;
 result.RawComponents[3,2]:=0.0;
 result.RawComponents[3,3]:=1.0;}
 // Row-order => Column-order
 result.RawComponents[0,0]:=fAccelerationStructureInstancePointer^.transform.matrix[0,0];
 result.RawComponents[0,1]:=fAccelerationStructureInstancePointer^.transform.matrix[1,0];
 result.RawComponents[0,2]:=fAccelerationStructureInstancePointer^.transform.matrix[2,0];
 result.RawComponents[0,3]:=0.0;
 result.RawComponents[1,0]:=fAccelerationStructureInstancePointer^.transform.matrix[0,1];
 result.RawComponents[1,1]:=fAccelerationStructureInstancePointer^.transform.matrix[1,1];
 result.RawComponents[1,2]:=fAccelerationStructureInstancePointer^.transform.matrix[2,1];
 result.RawComponents[1,3]:=0.0;
 result.RawComponents[2,0]:=fAccelerationStructureInstancePointer^.transform.matrix[0,2];
 result.RawComponents[2,1]:=fAccelerationStructureInstancePointer^.transform.matrix[1,2];
 result.RawComponents[2,2]:=fAccelerationStructureInstancePointer^.transform.matrix[2,2];
 result.RawComponents[2,3]:=0.0;
 result.RawComponents[3,0]:=fAccelerationStructureInstancePointer^.transform.matrix[0,3];
 result.RawComponents[3,1]:=fAccelerationStructureInstancePointer^.transform.matrix[1,3];
 result.RawComponents[3,2]:=fAccelerationStructureInstancePointer^.transform.matrix[2,3];
 result.RawComponents[3,3]:=1.0;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetTransform(const aTransform:TpvMatrix4x4);
begin
//fAccelerationStructureInstancePointer^.transform:=PVkTransformMatrixKHR(Pointer(@aTransform))^;
 // Column-order => Row-order
 fAccelerationStructureInstancePointer^.transform.matrix[0,0]:=aTransform.RawComponents[0,0];
 fAccelerationStructureInstancePointer^.transform.matrix[0,1]:=aTransform.RawComponents[1,0];
 fAccelerationStructureInstancePointer^.transform.matrix[0,2]:=aTransform.RawComponents[2,0];
 fAccelerationStructureInstancePointer^.transform.matrix[0,3]:=aTransform.RawComponents[3,0];
 fAccelerationStructureInstancePointer^.transform.matrix[1,0]:=aTransform.RawComponents[0,1];
 fAccelerationStructureInstancePointer^.transform.matrix[1,1]:=aTransform.RawComponents[1,1];
 fAccelerationStructureInstancePointer^.transform.matrix[1,2]:=aTransform.RawComponents[2,1];
 fAccelerationStructureInstancePointer^.transform.matrix[1,3]:=aTransform.RawComponents[3,1];
 fAccelerationStructureInstancePointer^.transform.matrix[2,0]:=aTransform.RawComponents[0,2];
 fAccelerationStructureInstancePointer^.transform.matrix[2,1]:=aTransform.RawComponents[1,2];
 fAccelerationStructureInstancePointer^.transform.matrix[2,2]:=aTransform.RawComponents[2,2];
 fAccelerationStructureInstancePointer^.transform.matrix[2,3]:=aTransform.RawComponents[3,2];
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetInstanceCustomIndex:TVkUInt32;
begin
 result:=fAccelerationStructureInstancePointer^.instanceCustomIndex;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetInstanceCustomIndex(const aInstanceCustomIndex:TVkUInt32);
begin
 fAccelerationStructureInstancePointer^.instanceCustomIndex:=aInstanceCustomIndex;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetMask:TVkUInt32;
begin
 result:=fAccelerationStructureInstancePointer^.mask;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetMask(const aMask:TVkUInt32);
begin
 fAccelerationStructureInstancePointer^.mask:=aMask;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetInstanceShaderBindingTableRecordOffset:TVkUInt32;
begin
 result:=fAccelerationStructureInstancePointer^.instanceShaderBindingTableRecordOffset;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetInstanceShaderBindingTableRecordOffset(const aInstanceShaderBindingTableRecordOffset:TVkUInt32);
begin
 fAccelerationStructureInstancePointer^.instanceShaderBindingTableRecordOffset:=aInstanceShaderBindingTableRecordOffset;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetFlags:TVkGeometryInstanceFlagsKHR;
begin
 result:=fAccelerationStructureInstancePointer^.flags;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetFlags(const aFlags:TVkGeometryInstanceFlagsKHR);
begin
 fAccelerationStructureInstancePointer^.flags:=aFlags;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure;
begin
 result:=fAccelerationStructure;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetAccelerationStructure(const aAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure);
var AddressInfo:TVkAccelerationStructureDeviceAddressInfoKHR;
begin
 
 if fAccelerationStructure<>aAccelerationStructure then begin

  fAccelerationStructure:=aAccelerationStructure;

  if assigned(fAccelerationStructure) then begin
  
   FillChar(AddressInfo,SizeOf(TVkAccelerationStructureDeviceAddressInfoKHR),#0);
   AddressInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR;
   AddressInfo.pNext:=nil;
   AddressInfo.accelerationStructure:=fAccelerationStructure.AccelerationStructure;

   fAccelerationStructureInstancePointer^.accelerationStructureReference:=fDevice.Commands.Commands.GetAccelerationStructureDeviceAddressKHR(fDevice.Handle,@AddressInfo);

  end else begin

   fAccelerationStructureInstancePointer^.accelerationStructureReference:=0;

  end;

 end; 

end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetAccelerationStructureDeviceAddress:TVkDeviceAddress;
begin
 result:=fAccelerationStructureInstancePointer^.accelerationStructureReference;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetAccelerationStructureDeviceAddress(const aAccelerationStructureDeviceAddress:TVkDeviceAddress);
begin
 fAccelerationStructureInstancePointer^.accelerationStructureReference:=aAccelerationStructureDeviceAddress;
end;

{ TpvRaytracingTopLevelAccelerationStructure }

constructor TpvRaytracingTopLevelAccelerationStructure.Create(const aDevice:TpvVulkanDevice;
                                                              const aInstanceAddress:TVkDeviceAddress;
                                                              const aInstanceCount:TVkUInt32;
                                                              const aFlags:TVkBuildAccelerationStructureFlagsKHR;
                                                              const aDynamicGeometry:Boolean);
begin

 inherited Create(aDevice,TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR));

 fDynamicGeometry:=aDynamicGeometry;

 FillChar(fInstances,SizeOf(TVkAccelerationStructureGeometryInstancesDataKHR),#0);
 fInstances.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR; 
 fInstances.pNext:=nil;
 fInstances.arrayOfPointers:=VK_FALSE;
 fInstances.Data.deviceAddress:=aInstanceAddress;

 FillChar(fBuildOffsetInfo,SizeOf(TVkAccelerationStructureBuildRangeInfoKHR),#0);
 fBuildOffsetInfo.firstVertex:=0;
 fBuildOffsetInfo.primitiveOffset:=0;
 fBuildOffsetInfo.primitiveCount:=aInstanceCount;
 
 fBuildOffsetInfoPtr:=@fBuildOffsetInfo;

 FillChar(fGeometry,SizeOf(TVkAccelerationStructureGeometryKHR),#0);
 fGeometry.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
 fGeometry.pNext:=nil;
 fGeometry.geometryType:=TVkGeometryTypeKHR(VK_GEOMETRY_TYPE_INSTANCES_KHR);
 fGeometry.geometry.instances:=fInstances;

 fBuildGeometryInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
 fBuildGeometryInfo.pNext:=nil;
 fBuildGeometryInfo.flags:=aFlags;
 fBuildGeometryInfo.geometryCount:=1;
 fBuildGeometryInfo.pGeometries:=@fGeometry;
 fBuildGeometryInfo.mode:=TVkBuildAccelerationStructureModeKHR(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR);
 fBuildGeometryInfo.type_:=TVkAccelerationStructureTypeKHR(VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR);
 fBuildGeometryInfo.srcAccelerationStructure:=VK_NULL_HANDLE;

 fCountInstances:=aInstanceCount;

 if fCountInstances>0 then begin

  fBuildSizesInfo:=GetMemorySizes(@fCountInstances);

 end else begin
  
  FillChar(fBuildSizesInfo,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
  fBuildSizesInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
  fBuildSizesInfo.pNext:=nil;
  fBuildSizesInfo.accelerationStructureSize:=0;
  fBuildSizesInfo.updateScratchSize:=0;
  fBuildSizesInfo.buildScratchSize:=0;
  
 end; 

end;

destructor TpvRaytracingTopLevelAccelerationStructure.Destroy;
begin
 inherited Destroy;
end;

procedure TpvRaytracingTopLevelAccelerationStructure.Update(const aInstanceAddress:TVkDeviceAddress;
                                                            const aInstanceCount:TVkUInt32;
                                                            const aFlags:TVkBuildAccelerationStructureFlagsKHR;
                                                            const aDynamicGeometry:Boolean);
begin

 fDynamicGeometry:=aDynamicGeometry;

 FillChar(fInstances,SizeOf(TVkAccelerationStructureGeometryInstancesDataKHR),#0);
 fInstances.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR;
 fInstances.pNext:=nil;
 fInstances.arrayOfPointers:=VK_FALSE;
 fInstances.Data.deviceAddress:=aInstanceAddress;

 FillChar(fBuildOffsetInfo,SizeOf(TVkAccelerationStructureBuildRangeInfoKHR),#0);
 fBuildOffsetInfo.firstVertex:=0;
 fBuildOffsetInfo.primitiveOffset:=0;
 fBuildOffsetInfo.primitiveCount:=aInstanceCount;

 fBuildOffsetInfoPtr:=@fBuildOffsetInfo;

 FillChar(fGeometry,SizeOf(TVkAccelerationStructureGeometryKHR),#0);
 fGeometry.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
 fGeometry.pNext:=nil;
 fGeometry.geometryType:=TVkGeometryTypeKHR(VK_GEOMETRY_TYPE_INSTANCES_KHR);
 fGeometry.geometry.instances:=fInstances;

 fBuildGeometryInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR;
 fBuildGeometryInfo.pNext:=nil;
 fBuildGeometryInfo.flags:=aFlags;
 fBuildGeometryInfo.geometryCount:=1;

 fCountInstances:=aInstanceCount;

 if fCountInstances>0 then begin

  fBuildSizesInfo:=GetMemorySizes(@fCountInstances);

 end else begin
  
  FillChar(fBuildSizesInfo,SizeOf(TVkAccelerationStructureBuildSizesInfoKHR),#0);
  fBuildSizesInfo.sType:=VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR;
  fBuildSizesInfo.pNext:=nil;
  fBuildSizesInfo.accelerationStructureSize:=0;
  fBuildSizesInfo.updateScratchSize:=0;
  fBuildSizesInfo.buildScratchSize:=0;
  
 end; 

end;

procedure TpvRaytracingTopLevelAccelerationStructure.UpdateInstanceAddress(const aInstanceAddress:TVkDeviceAddress);
begin
 fInstances.Data.deviceAddress:=aInstanceAddress;
end;

{ TpvRaytracingGeometryInfoManager }

constructor TpvRaytracingGeometryInfoManager.Create;
begin
 inherited Create;
 fLock:=TPasMPCriticalSection.Create;
 fObjectList:=TObjectList.Create;
 fGeometryInfoList:=TpvRaytracingBLASGeometryInfoBufferItemList.Create;
 fBufferRangeAllocator:=TpvBufferRangeAllocator.Create;
 fBufferRangeAllocator.OnResize:=BufferRangeAllocatorOnResize;
 fSizeDirty:=false;
 fDirty:=false;
 fOnDefragmentMove:=nil;
end;

destructor TpvRaytracingGeometryInfoManager.Destroy;
begin
 FreeAndNil(fBufferRangeAllocator);
 FreeAndNil(fGeometryInfoList);
 FreeAndNil(fObjectList);
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvRaytracingGeometryInfoManager.BufferRangeAllocatorOnResize(const aSender:TpvBufferRangeAllocator;const aNewCapacity:TpvInt64);
begin
 fObjectList.Resize(aNewCapacity);
 fGeometryInfoList.Resize(aNewCapacity);
 fSizeDirty:=true;
end;

procedure TpvRaytracingGeometryInfoManager.BufferRangeAllocatorOnDefragmentMove(const aSender:TpvBufferRangeAllocator;const aOldOffset,aNewOffset,aSize:TpvInt64);
var Index:TpvSizeInt;
    Object_:TObject;
begin

 Object_:=fObjectList.Items[aOldOffset];

 // Check for overlapping moves
 if (aOldOffset<aNewOffset) and ((aOldOffset+aSize)>aNewOffset) then begin
  // Copy from front to back or back to front, depending on it is safe for overlapping moves
  if (aOldOffset+aSize)<aNewOffset then begin
   for Index:=0 to aSize-1 do begin
    fObjectList.Items[aOldOffset+Index]:=fObjectList.Items[aNewOffset+Index];
    fGeometryInfoList.Items[aOldOffset+Index]:=fGeometryInfoList.Items[aNewOffset+Index];
   end;
  end else begin
   for Index:=aSize-1 downto 0 do begin
    fObjectList.Items[aOldOffset+Index]:=fObjectList.Items[aNewOffset+Index];
    fGeometryInfoList.Items[aOldOffset+Index]:=fGeometryInfoList.Items[aNewOffset+Index];
   end;
  end;
 end else begin
  for Index:=0 to aSize-1 do begin
   fObjectList.Items[aOldOffset+Index]:=fObjectList.Items[aNewOffset+Index];
   fGeometryInfoList.Items[aOldOffset+Index]:=fGeometryInfoList.Items[aNewOffset+Index];
  end;
 end; 

 if assigned(fOnDefragmentMove) then begin
  fOnDefragmentMove(self,Object_,aOldOffset,aNewOffset,aSize);
 end;

end;

function TpvRaytracingGeometryInfoManager.AllocateGeometryInfoRange(const aObject:TObject;const aCount:TpvSizeInt):TpvSizeInt;
begin
 fLock.Acquire;
 try
  result:=fBufferRangeAllocator.Allocate(aCount);
  if result>=0 then begin
   fObjectList.Items[result]:=aObject;
   fDirty:=true;
  end;
 finally
  fLock.Release;
 end;
end;

procedure TpvRaytracingGeometryInfoManager.FreeGeometryInfoRange(const aOffset:TpvSizeInt);
begin
 fLock.Acquire;
 try
  if (aOffset>=0) and (aOffset<fGeometryInfoList.Count) then begin
   fBufferRangeAllocator.Release(aOffset);
   fObjectList.Items[aOffset]:=nil;
   fDirty:=true;
  end;  
 finally
  fLock.Release;
 end;
end;

function TpvRaytracingGeometryInfoManager.GetGeometryInfo(const aIndex:TpvSizeInt):PpvRaytracingBLASGeometryInfoBufferItem;
begin
 if (aIndex>=0) and (aIndex<fGeometryInfoList.Count) then begin
  result:=@fGeometryInfoList.ItemArray[aIndex];
 end else begin
  result:=nil;
 end;
end;

procedure TpvRaytracingGeometryInfoManager.Defragment;
begin
 fLock.Acquire;
 try
  fBufferRangeAllocator.Defragment(BufferRangeAllocatorOnDefragmentMove);
  fDirty:=true;
 finally
  fLock.Release;
 end;
end;

{ TpvRaytracingBLASManager.TBLAS.TBLASInstance }

constructor TpvRaytracingBLASManager.TBLAS.TBLASInstance.Create(const aBLAS:TBLAS;
                                                                const aTransform:TpvMatrix4x4;
                                                                const aMask:TVkUInt32;
                                                                const aInstanceShaderBindingTableRecordOffset:TVkUInt32;
                                                                const aFlags:TVkGeometryInstanceFlagsKHR);
begin
 inherited Create;

 fBLASManager:=aBLAS.fBLASManager;

 fBLAS:=aBLAS;

 fInBLASManagerIndex:=-1;

 fInBLASIndex:=-1;

 fAccelerationStructureInstance:=TpvRaytracingBottomLevelAccelerationStructureInstance.Create(fBLASManager.fVulkanDevice,
                                                                                              aTransform,
                                                                                              fBLAS.GeometryInfoBaseIndex, // Instance custom index is the base index for accessing the geometry info buffer items for this BLAS
                                                                                              aMask,
                                                                                              aInstanceShaderBindingTableRecordOffset,
                                                                                              aFlags,
                                                                                              fBLAS.fAccelerationStructure,
                                                                                              nil);      

end;

destructor TpvRaytracingBLASManager.TBLAS.TBLASInstance.Destroy;
begin
 FreeAndNil(fAccelerationStructureInstance);
 inherited Destroy;
end;

procedure TpvRaytracingBLASManager.TBLAS.TBLASInstance.AfterConstruction;
var OldPointer,NewPointer:PVkAccelerationStructureInstanceKHR;    
begin
 
 inherited AfterConstruction;

  // Add to BLAS-own BLAS instance list
 if assigned(fBLAS) then begin
  fInBLASIndex:=fBLAS.fBLASInstanceList.Add(self);
 end;

 // Add to manager-global BLAS instance list
 if assigned(fBLASManager) then begin

  TPasMPInterlocked.Write(fBLASManager.fDirty,TPasMPBool32(true));

  fInBLASManagerIndex:=fBLASManager.fBLASInstanceList.Add(self);

  if fInBLASManagerIndex>=0 then begin

   if fBLASManager.fGeometryOffsetArrayList.Count<=fInBLASManagerIndex then begin
    fBLASManager.fGeometryOffsetArrayList.Resize(fInBLASManagerIndex+1);
   end;

   fBLASManager.fGeometryOffsetArrayList[InBLASManagerIndex]:=fBLAS.GeometryInfoBaseIndex;

   // Ensure that the acceleration structure instance list has enough space for the new acceleration structure instance
   if fBLASManager.fAccelerationStructureInstanceKHRArrayList.Count<=fInBLASManagerIndex then begin

    // Save old pointer to the first item of the acceleration structure instance array list
    if fBLASManager.fAccelerationStructureInstanceKHRArrayList.Count>0 then begin
     OldPointer:=@fBLASManager.fAccelerationStructureInstanceKHRArrayList.ItemArray[0];
    end else begin
     OldPointer:=nil;
    end;
    
    fBLASManager.fAccelerationStructureInstanceKHRArrayList.Resize(fInBLASManagerIndex+1);

    if assigned(OldPointer) then begin

     // Get new pointer to the first item of the acceleration structure instance array list 
     NewPointer:=@fBLASManager.fAccelerationStructureInstanceKHRArrayList.ItemArray[0];

     if OldPointer<>NewPointer then begin

      // Full reassign needed, because the list has been resized with possible new memory address and the pointers to the 
      // internal structures can be invalid
      fBLASManager.ReassignAccelerationStructureInstancePointers;

     end; 

    end; 

   end; 
    
   // Copy the TpvRaytracingBottomLevelAccelerationStructureInstance own VKAccelerationStructureInstanceKHR content into 
   // the global VKAccelerationStructureInstanceKHR array list
   fBLASManager.fAccelerationStructureInstanceKHRArrayList.ItemArray[fInBLASManagerIndex]:=fAccelerationStructureInstance.fAccelerationStructureInstance;

   // Set the acceleration structure instance pointer to the global VKAccelerationStructureInstanceKHR array list, so that
   // so that the TpvRaytracingBottomLevelAccelerationStructureInstance own VKAccelerationStructureInstanceKHR instance isn't used anymore
   // from now on. This is needed, because the global VKAccelerationStructureInstanceKHR array list is used as direct memory data source
   // for the GPU-side geometry info buffer.
   fAccelerationStructureInstance.fAccelerationStructureInstancePointer:=@fBLASManager.fAccelerationStructureInstanceKHRArrayList.ItemArray[fInBLASManagerIndex];

  end;

 end;

end;

procedure TpvRaytracingBLASManager.TBLAS.TBLASInstance.BeforeDestruction;
var OtherBLASInstance:TBLASInstance;
begin

 if assigned(fAccelerationStructureInstance) then begin

  // Copy the global VKAccelerationStructureInstanceKHR array list content back into the TpvRaytracingBottomLevelAccelerationStructureInstance 
  // own VKAccelerationStructureInstanceKHR instance, for the case that the instance is destroyed and the acceleration structure instance
  // is still used by the BLAS instance.
  if assigned(fBLASManager) and (fInBLASManagerIndex>=0) then begin
   fAccelerationStructureInstance.fAccelerationStructureInstance:=fBLASManager.fAccelerationStructureInstanceKHRArrayList.ItemArray[fInBLASManagerIndex];
  end;

  // Set the acceleration structure instance pointer back to the own VKAccelerationStructureInstanceKHR instance, so that the
  // TpvRaytracingBottomLevelAccelerationStructureInstance own VKAccelerationStructureInstanceKHR instance is used again, to avoid
  // dangling pointers.
  fAccelerationStructureInstance.fAccelerationStructureInstancePointer:=@fAccelerationStructureInstance.fAccelerationStructureInstance;

 end; 

 // Remove from manager-global BLAS instance list 
 if assigned(fBLASManager) and (fInBLASManagerIndex>=0) then begin
  TPasMPInterlocked.Write(fBLASManager.fDirty,TPasMPBool32(true));
  if (fInBLASManagerIndex+1)<fBLASManager.fBLASInstanceList.Count then begin
   OtherBLASInstance:=fBLASManager.fBLASInstanceList.Items[fBLASManager.fBLASInstanceList.Count-1];
   OtherBLASInstance.fInBLASManagerIndex:=fInBLASManagerIndex;
   fInBLASManagerIndex:=fBLASManager.fBLASInstanceList.Count-1;
   fBLASManager.fBLASInstanceList.Items[OtherBLASInstance.fInBLASManagerIndex]:=OtherBLASInstance;
   fBLASManager.fBLASInstanceList.Items[fInBLASManagerIndex]:=self;
   fBLASManager.fAccelerationStructureInstanceKHRArrayList.Exchange(OtherBLASInstance.fInBLASManagerIndex,fInBLASManagerIndex);
   fBLASManager.fGeometryOffsetArrayList.Exchange(OtherBLASInstance.fInBLASManagerIndex,fInBLASManagerIndex);
  end;
  fBLASManager.fBLASInstanceList.ExtractIndex(fInBLASManagerIndex);
  fBLASManager.fAccelerationStructureInstanceKHRArrayList.Delete(fInBLASManagerIndex);
  fBLASManager.fGeometryOffsetArrayList.Delete(fInBLASManagerIndex);
  fInBLASManagerIndex:=-1;
 end;

 // Remove from BLAS-own BLAS instance list
 if assigned(fBLAS) and (fInBLASIndex>=0) then begin
  if (fInBLASIndex+1)<fBLAS.fBLASInstanceList.Count then begin
   OtherBLASInstance:=fBLAS.fBLASInstanceList.Items[fBLAS.fBLASInstanceList.Count-1];
   OtherBLASInstance.fInBLASIndex:=fInBLASIndex;
   fInBLASIndex:=fBLAS.fBLASInstanceList.Count-1;
   fBLAS.fBLASInstanceList.Items[OtherBLASInstance.fInBLASIndex]:=OtherBLASInstance;
   fBLAS.fBLASInstanceList.Items[fInBLASIndex]:=self;
  end;
  fBLAS.fBLASInstanceList.ExtractIndex(fInBLASIndex); 
  fInBLASIndex:=-1;
 end;

 inherited BeforeDestruction;
end;

 { TpvRaytracingBLASManager.TBLAS }

constructor TpvRaytracingBLASManager.TBLAS.Create(const aBLASManager:TpvRaytracingBLASManager;
                                                  const aFlags:TVkBuildAccelerationStructureFlagsKHR;
                                                  const aDynamicGeometry:Boolean;
                                                  const aAllocationGroupID:TpvUInt64;
                                                  const aName:TpvUTF8String);
begin
 inherited Create;

 fBLASManager:=aBLASManager;
 
 fInBLASManagerIndex:=-1;

 fName:=aName;

 fAllocationGroupId:=aAllocationGroupID;

 fFlags:=aFlags;

 fDynamicGeometry:=aDynamicGeometry;

 fAccelerationStructureGeometry:=TpvRaytracingBottomLevelAccelerationStructureGeometry.Create(fBLASManager.fVulkanDevice);
 
 fAccelerationStructure:=nil;

 fAccelerationStructureSize:=0;

 fBuildScratchSize:=0;

 fUpdateScratchSize:=0;

 fScratchSize:=0;

 fAccelerationStructureScratchSize:=0;
 
 fAccelerationStructureBuffer:=nil;
 
 fScratchOffset:=0;
 
 fScratchPass:=0;
 
 fGeometryInfoBaseIndex:=-1;

 fCountGeometries:=0;
 
 fBLASInstanceList:=TBLASInstanceList.Create(false);

end;

destructor TpvRaytracingBLASManager.TBLAS.Destroy;
begin

 if assigned(fAccelerationStructure) then begin
  fAccelerationStructure.Finalize;
 end;

 while fBLASInstanceList.Count>0 do begin
  fBLASInstanceList[fBLASInstanceList.Count-1].Free;
 end;

 if fGeometryInfoBaseIndex>=0 then begin
  fBLASManager.fGeometryInfoManager.FreeGeometryInfoRange(fGeometryInfoBaseIndex);
 end;

 FreeAndNil(fAccelerationStructureGeometry);

 FreeAndNil(fAccelerationStructure);

 FreeAndNil(fAccelerationStructureBuffer);

 FreeAndNil(fBLASInstanceList);

 inherited Destroy;

end;

procedure TpvRaytracingBLASManager.TBLAS.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fBLASManager) then begin
  TPasMPInterlocked.Write(fBLASManager.fDirty,TPasMPBool32(true));
  fInBLASManagerIndex:=fBLASManager.fBLASList.Add(self);
 end;
end;

procedure TpvRaytracingBLASManager.TBLAS.BeforeDestruction;
var OtherBLAS:TBLAS;
begin
 if assigned(fBLASManager) and (fInBLASManagerIndex>=0) then begin
  TPasMPInterlocked.Write(fBLASManager.fDirty,TPasMPBool32(true));
  if (fInBLASManagerIndex+1)<fBLASManager.fBLASList.Count then begin
   OtherBLAS:=fBLASManager.fBLASList.Items[fBLASManager.fBLASList.Count-1];
   OtherBLAS.fInBLASManagerIndex:=fInBLASManagerIndex;
   fInBLASManagerIndex:=fBLASManager.fBLASList.Count-1;
   fBLASManager.fBLASList.Items[OtherBLAS.fInBLASManagerIndex]:=OtherBLAS;
   fBLASManager.fBLASList.Items[fInBLASManagerIndex]:=self;
  end;
  fBLASManager.fBLASList.ExtractIndex(fInBLASManagerIndex);
  fInBLASManagerIndex:=-1;
 end;
 inherited BeforeDestruction;
end;

procedure TpvRaytracingBLASManager.TBLAS.Initialize;
begin

 if fAccelerationStructureGeometry.Geometries.Count>0 then begin

  if fGeometryInfoBaseIndex<0 then begin

   fCountGeometries:=fAccelerationStructureGeometry.Geometries.Count;

   fGeometryInfoBaseIndex:=fBLASManager.fGeometryInfoManager.AllocateGeometryInfoRange(self,fCountGeometries);

  end;

  if not assigned(fAccelerationStructure) then begin

   fAccelerationStructure:=TpvRaytracingBottomLevelAccelerationStructure.Create(fBLASManager.fVulkanDevice,
                                                                                fAccelerationStructureGeometry,
                                                                                fFlags,
                                                                                fDynamicGeometry);

   fAccelerationStructureSize:=fAccelerationStructure.BuildSizesInfo.accelerationStructureSize;

   fBuildScratchSize:=fAccelerationStructure.BuildSizesInfo.buildScratchSize;

   fUpdateScratchSize:=fAccelerationStructure.BuildSizesInfo.updateScratchSize;

   fScratchSize:=Max(fBuildScratchSize,fUpdateScratchSize);

   UpdateBuffer;

  end;

 end else begin

  if assigned(fAccelerationStructure) then begin
   FreeAndNil(fAccelerationStructure);
  end;

  if assigned(fAccelerationStructureBuffer) then begin
   FreeAndNil(fAccelerationStructureBuffer);
  end;

 end;

end;

procedure TpvRaytracingBLASManager.TBLAS.Update;
begin
 if assigned(fAccelerationStructure) then begin
  fAccelerationStructure.Update(fAccelerationStructureGeometry,
                                fFlags,
                                fDynamicGeometry);
  fAccelerationStructureSize:=fAccelerationStructure.BuildSizesInfo.accelerationStructureSize;
  fBuildScratchSize:=fAccelerationStructure.BuildSizesInfo.buildScratchSize;
  fUpdateScratchSize:=fAccelerationStructure.BuildSizesInfo.updateScratchSize;
  fScratchSize:=Max(fBuildScratchSize,fUpdateScratchSize);
  UpdateBuffer;
 end;
end;

procedure TpvRaytracingBLASManager.TBLAS.UpdateBuffer;
begin

 if ((not assigned(fAccelerationStructureBuffer)) or
     (fAccelerationStructureBuffer.Size<fAccelerationStructureSize)) and
    (fAccelerationStructureSize>0) then begin

  if assigned(fAccelerationStructureBuffer) or (fAccelerationStructure.AccelerationStructure<>VK_NULL_HANDLE) then begin

   if assigned(pvApplication) then begin
    pvApplication.WaitForPreviousFrame(true); // wait on previous frame to avoid destroy still-in-usage buffers.
   end;

   fAccelerationStructure.Finalize;

   FreeAndNil(fAccelerationStructureBuffer);

  end;

  fAccelerationStructureBuffer:=TpvVulkanBuffer.Create(fBLASManager.fVulkanDevice,
                                                       fAccelerationStructureSize,
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
                                                       256,
                                                       fAllocationGroupID,
                                                       'TpvScene3D.fRaytracingVulkanBLASBuffer'
                                                      );
  fBLASManager.fVulkanDevice.DebugUtils.SetObjectName(fAccelerationStructureBuffer.Handle,VK_OBJECT_TYPE_BUFFER,fName+'.BLASBuffer');

  fAccelerationStructure.Initialize(fAccelerationStructureBuffer,0);
  fBLASManager.fVulkanDevice.DebugUtils.SetObjectName(fAccelerationStructure.fAccelerationStructure,VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR,fName+'.BLAS');

 end;

end;

function TpvRaytracingBLASManager.TBLAS.GetGeometryInfo(const aIndex:TpvSizeInt):PpvRaytracingBLASGeometryInfoBufferItem;
begin
 if fGeometryInfoBaseIndex>=0 then begin
  result:=fBLASManager.fGeometryInfoManager.GetGeometryInfo(fGeometryInfoBaseIndex+aIndex);
 end else begin
  result:=nil;
 end; 
end;

function TpvRaytracingBLASManager.TBLAS.AcquireBLASInstance(const aTransform:TpvMatrix4x4;
                                                            const aMask:TVkUInt32;
                                                            const aInstanceShaderBindingTableRecordOffset:TVkUInt32;
                                                            const aFlags:TVkGeometryInstanceFlagsKHR):TBLASInstance;
begin
 result:=TBLASInstance.Create(self,
                              aTransform,
                              aMask,
                              aInstanceShaderBindingTableRecordOffset,
                              aFlags);
end;

procedure TpvRaytracingBLASManager.TBLAS.ReleaseBLASInstance(const aBLASInstance:TBLASInstance);
begin
 aBLASInstance.Free;
end;

procedure TpvRaytracingBLASManager.TBLAS.Enqueue(const aUpdate:Boolean);
begin
 if fEnqueueState=TEnqueueState.None then begin
  if aUpdate then begin
   fEnqueueState:=TEnqueueState.Update;
  end else begin
   fEnqueueState:=TEnqueueState.Build;
  end;
  fBLASManager.fBLASQueueLock.Acquire;
  try
   fBLASManager.fBLASQueue.Add(self);
  finally
   fBLASManager.fBLASQueueLock.Release;
  end;
 end;
end;    

{ TpvRaytracingBLASManager }

constructor TpvRaytracingBLASManager.Create(const aDevice:TpvVulkanDevice;const aCountInFlightFrames:TpvSizeInt);
var Index:TpvSizeInt;
begin

 inherited Create;

 fVulkanDevice:=aDevice;

 fCountInFlightFrames:=aCountInFlightFrames;

 fLock:=TPasMPCriticalSection.Create;

 fBLASList:=TBLASList.Create(false);

 fBLASInstanceList:=TBLAS.TBLASInstanceList.Create(false);

 fBLASQueue:=TBLASList.Create(false);

 fBLASQueueLock:=TPasMPSlimReaderWriterLock.Create;

 fAccelerationStructureInstanceKHRArrayList:=TpvRaytracingAccelerationStructureInstanceArrayList.Create;

 fGeometryInfoManager:=TpvRaytracingGeometryInfoManager.Create;
 fGeometryInfoManager.OnDefragmentMove:=GeometryInfoManagerOnDefragmentMove;

 fGeometryOffsetArrayList:=TGeometryOffsetArrayList.Create;

 fDirty:=false;

 for Index:=0 to 1 do begin
  fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[Index]:=nil;
  fRaytracingBLASGeometryInfoBufferItemBuffers[Index]:=nil;
 end;

 fRaytracingBLASGeometryInfoBufferRingIndex:=0;

 if assigned(fVulkanDevice) then begin
  fRaytracingAccelerationStructureBuildQueue:=TpvRaytracingAccelerationStructureBuildQueue.Create(fVulkanDevice);
 end else begin
  fRaytracingAccelerationStructureBuildQueue:=nil;
 end;

 fOnMustWaitForPreviousFrame:=nil;

 fOnUpdate:=nil;

 fUpdateRaytracingFrameDoneMask:=0;

 fRaytracingEmptyInitialized:=false;

 fRaytracingEmptyVertexBuffer:=nil;

 fRaytracingEmptyIndexBuffer:=nil;

 fRaytracingEmptyBLAS:=nil;

 fRaytracingEmptyBLASInstance:=nil;

 fRaytracingEmptyBLASScratchBuffer:=nil;

 fRaytracingBLASScratchBuffer:=nil;

 fRaytracingTLASScratchBuffer:=nil;

 fRaytracingTLASBLASInstancesBuffer:=nil;

 for Index:=Low(fRaytracingTLASBLASInstancesBuffers) to High(fRaytracingTLASBLASInstancesBuffers) do begin
  fRaytracingTLASBLASInstancesBuffers[Index]:=nil;
 end;

 fRaytracingTLASBuffer:=nil;

 fRaytracingTLAS:=nil;

 for Index:=Low(fRaytracingTLASAccelerationStructures) to High(fRaytracingTLASAccelerationStructures) do begin
  fRaytracingTLASAccelerationStructures[Index]:=VK_NULL_HANDLE;
 end;

 for Index:=Low(fRaytracingTLASGenerations) to High(fRaytracingTLASGenerations) do begin
  fRaytracingTLASGenerations[Index]:=High(TpvUInt64);
 end;

end;

destructor TpvRaytracingBLASManager.Destroy;
var Index:TpvSizeInt;
begin

 while fBLASInstanceList.Count>0 do begin
  fBLASInstanceList[fBLASInstanceList.Count-1].Free;
 end;

 while fBLASList.Count>0 do begin
  fBLASList[fBLASList.Count-1].Free;
 end;

 FreeAndNil(fGeometryOffsetArrayList);
 
 FreeAndNil(fGeometryInfoManager);
 
 FreeAndNil(fAccelerationStructureInstanceKHRArrayList);

 FreeAndNil(fBLASQueueLock);

 FreeAndNil(fBLASQueue);

 FreeAndNil(fBLASList);

 FreeAndNil(fRaytracingTLAS);

 FreeAndNil(fRaytracingTLASScratchBuffer);

 for Index:=Low(fRaytracingTLASBLASInstancesBuffers) to High(fRaytracingTLASBLASInstancesBuffers) do begin
  FreeAndNil(fRaytracingTLASBLASInstancesBuffers[Index]);
 end;

 FreeAndNil(fRaytracingTLASBLASInstancesBuffer);

 FreeAndNil(fRaytracingTLASBuffer);

 FreeAndNil(fRaytracingBLASScratchBuffer);

 FreeAndNil(fRaytracingEmptyBLASInstance);

 FreeAndNil(fRaytracingEmptyBLAS);

 FreeAndNil(fRaytracingEmptyBLASScratchBuffer);

 FreeAndNil(fRaytracingEmptyVertexBuffer);

 FreeAndNil(fRaytracingEmptyIndexBuffer);

 FreeAndNil(fRaytracingAccelerationStructureBuildQueue);

 for Index:=0 to 1 do begin
  FreeAndNil(fRaytracingBLASGeometryInfoBufferItemBuffers[Index]);
  FreeAndNil(fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[Index]);
 end;

 FreeAndNil(fLock);

 inherited Destroy;
end;

procedure TpvRaytracingBLASManager.GeometryInfoManagerOnDefragmentMove(const aSender:TpvRaytracingGeometryInfoManager;const aObject:TObject;const aOldOffset,aNewOffset,aSize:TpvInt64);
var Index,InstanceCustomIndex:TpvSizeInt;
    BLAS:TBLAS;
    BLASInstance:TBLAS.TBLASInstance;
begin

 TPasMPInterlocked.Write(fDirty,TPasMPBool32(true));

 for Index:=0 to fBLASList.Count-1 do begin
  BLAS:=fBLASList.Items[Index];
  if (BLAS.fGeometryInfoBaseIndex>=0) and (BLAS.fGeometryInfoBaseIndex>=aOldOffset) and (BLAS.fGeometryInfoBaseIndex<(aOldOffset+aSize)) then begin
   BLAS.fGeometryInfoBaseIndex:=aNewOffset+(BLAS.fGeometryInfoBaseIndex-aOldOffset);
  end;
 end;

 for Index:=0 to fBLASInstanceList.Count-1 do begin
  BLASInstance:=fBLASInstanceList.Items[Index];
  InstanceCustomIndex:=BLASInstance.AccelerationStructureInstance.InstanceCustomIndex;
  if (InstanceCustomIndex>=0) and (InstanceCustomIndex>=aOldOffset) and (InstanceCustomIndex<(aOldOffset+aSize)) then begin
   BLASInstance.AccelerationStructureInstance.InstanceCustomIndex:=aNewOffset+(InstanceCustomIndex-aOldOffset);
  end;
  if (fGeometryOffsetArrayList[BLASInstance.fInBLASManagerIndex]>=0) and (fGeometryOffsetArrayList[BLASInstance.fInBLASManagerIndex]>=aOldOffset) and (fGeometryOffsetArrayList[BLASInstance.fInBLASManagerIndex]<(aOldOffset+aSize)) then begin
   fGeometryOffsetArrayList[BLASInstance.fInBLASManagerIndex]:=aNewOffset+(fGeometryOffsetArrayList[BLASInstance.fInBLASManagerIndex]-aOldOffset);
  end;
 end;

end;

function TpvRaytracingBLASManager.AcquireBLAS(const aFlags:TVkBuildAccelerationStructureFlagsKHR;
                                              const aDynamicGeometry:Boolean;
                                              const aAllocationGroupID:TpvUInt64;
                                              const aName:TpvUTF8String):TBLAS;
begin
 result:=TBLAS.Create(self,aFlags,aDynamicGeometry,aAllocationGroupID,aName);
end;

procedure TpvRaytracingBLASManager.ReleaseBLAS(const aBLAS:TBLAS);
begin
 aBLAS.Free;
end;

procedure TpvRaytracingBLASManager.ReassignAccelerationStructureInstancePointers;
var Index:TpvSizeInt;    
begin
 if fAccelerationStructureInstanceKHRArrayList.Count>0 then begin
  Assert(fAccelerationStructureInstanceKHRArrayList.Count=fBLASInstanceList.Count,'Different count of acceleration structure instances and BLAS instances');
  for Index:=0 to fAccelerationStructureInstanceKHRArrayList.Count-1 do begin
   fBLASInstanceList.RawItems[Index].AccelerationStructureInstance.AccelerationStructureInstance:=@fAccelerationStructureInstanceKHRArrayList.ItemArray[Index];
  end;
 end; 
end; 

procedure TpvRaytracingBLASManager.HostMemoryBarrier;
var MemoryBarrier:TVkMemoryBarrier;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Host memory barrier                                                     //
 /////////////////////////////////////////////////////////////////////////////

 FillChar(MemoryBarrier,SizeOf(TVkMemoryBarrier),#0);
 MemoryBarrier.sType:=VK_STRUCTURE_TYPE_MEMORY_BARRIER;
 MemoryBarrier.pNext:=nil;
 MemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_MEMORY_WRITE_BIT);
 MemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT);

 fCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                   TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),
                                   0,
                                   1,
                                   @MemoryBarrier,
                                   0,
                                   nil,
                                   0,
                                   nil);

end;

procedure TpvRaytracingBLASManager.WaitForPreviousFrame;
var MustWaitForPreviousFrame:Boolean;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Wait for previous frame, when there are changes in the BLAS list, since //
 // it is necessary at Vulkan, that buffers are not in use, when they are   //
 // destroyed. Therefore we should wait for the previous frame for to be    //
 // sure, that the buffers are not in use anymore.                          //
 /////////////////////////////////////////////////////////////////////////////

 MustWaitForPreviousFrame:=assigned(fOnMustWaitForPreviousFrame) and fOnMustWaitForPreviousFrame(self);

 if not fRaytracingEmptyInitialized then begin
  MustWaitForPreviousFrame:=true;
 end;

 if MustWaitForPreviousFrame and assigned(pvApplication) then begin
  // Wait for previous frame, when there are changes in the BLAS list, since it is necessary at Vulkan, that buffers are not in use,
  // when they are destroyed. Therefore we should wait for the previous frame for to be sure, that the buffers are not in use anymore.
  pvApplication.WaitForPreviousFrame(true);
 end;

end;

procedure TpvRaytracingBLASManager.HandleEmptyBLAS;
const EmptyVertex:array[0..3] of TpvUInt32=($7fc00000,$7fc00000,$7fc00000,$7fc00000); // 4x NaNs
      EmptyIndices:array[0..2] of TpvUInt32=(0,0,0); // Simple as that, only one NaN triangle with three vertices with the same NaN vertex
begin

 //////////////////////////////////////////////////////////////////////////////
 // Create empty blas with invalid geometry for empty TLAS, when there are   //
 // no RaytracingActive group instance nodes.                                //
 //////////////////////////////////////////////////////////////////////////////

 if not fRaytracingEmptyInitialized then begin

  fRaytracingEmptyVertexBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                       SizeOf(EmptyVertex),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR),
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
                                                       pvAllocationGroupIDScene3DRaytracing,
                                                       'TpvScene3D.fRaytracingVulkanEmptyVertexBuffer'
                                                      );
  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingEmptyVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanEmptyVertexBuffer');

  fVulkanDevice.MemoryStaging.Upload(fStagingQueue,
                                     fStagingCommandBuffer,
                                     fStagingFence,
                                     EmptyVertex,
                                     fRaytracingEmptyVertexBuffer,
                                     0,
                                     SizeOf(EmptyVertex));

  fRaytracingEmptyIndexBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                      SizeOf(EmptyIndices),
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR),
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
                                                      pvAllocationGroupIDScene3DRaytracing,
                                                      'TpvScene3D.fRaytracingVulkanEmptyIndexBuffer'
                                                     );
  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingEmptyIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanEmptyIndexBuffer');

  fVulkanDevice.MemoryStaging.Upload(fStagingQueue,
                                     fStagingCommandBuffer,
                                     fStagingFence,
                                     EmptyIndices,
                                     fRaytracingEmptyIndexBuffer,
                                     0,
                                     SizeOf(EmptyIndices));

  fRaytracingEmptyBLAS:=AcquireBLAS(TVkBuildAccelerationStructureFlagsKHR(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR),
                                    false,
                                    pvAllocationGroupIDScene3DRaytracing,
                                    'Empty');

  fRaytracingEmptyBLAS.AccelerationStructureGeometry.AddTriangles(fRaytracingEmptyVertexBuffer,
                                                                  0,
                                                                  3,
                                                                  SizeOf(TpvVector4),
                                                                  fRaytracingEmptyIndexBuffer,
                                                                  0,
                                                                  3,
                                                                  true,
                                                                  nil,
                                                                  0);

  fRaytracingEmptyBLAS.Initialize;

  FreeAndNil(fRaytracingEmptyBLASScratchBuffer);

  fRaytracingEmptyBLASScratchBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                            Max(1,Max(fRaytracingEmptyBLAS.BuildScratchSize,fRaytracingEmptyBLAS.UpdateScratchSize)),
                                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT),
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
                                                            fVulkanDevice.PhysicalDevice.AccelerationStructurePropertiesKHR.minAccelerationStructureScratchOffsetAlignment,
                                                            pvAllocationGroupIDScene3DRaytracing,
                                                            'TpvScene3D.fRaytracingVulkanEmptyBLASScratchBuffer');

  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingEmptyBLASScratchBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanEmptyBLASScratchBuffer');

  fRaytracingEmptyBLAS.AccelerationStructure.Build(fCommandBuffer,
                                                   fRaytracingEmptyBLASScratchBuffer,
                                                   0,
                                                   false,
                                                   nil);

  TpvRaytracingAccelerationStructure.MemoryBarrier(fCommandBuffer);

  fRaytracingEmptyBLASInstance:=fRaytracingEmptyBLAS.AcquireBLASInstance(TpvMatrix4x4.Identity,
                                                                         $ff,
                                                                         0,
                                                                         0);

  fRaytracingBLASListChanged:=true;

  fRaytracingEmptyInitialized:=true;

 end;

end;

procedure TpvRaytracingBLASManager.ProcessContentUpdate;
begin

 //////////////////////////////////////////////////////////////////////////////
 // Call OnUpdate hook                                                       //
 //////////////////////////////////////////////////////////////////////////////

 if assigned(fOnUpdate) then begin
  fOnUpdate(self);
 end;

end;

procedure TpvRaytracingBLASManager.BuildOrUpdateBLASMetaData;
begin

 //////////////////////////////////////////////////////////////////////////////
 // At BLAS list changed, we have to rebuild the BLAS instances and the      //
 // BLAS geometry info buffer items and the BLAS geometry info offset buffer //
 // items.                                                                   //
 //////////////////////////////////////////////////////////////////////////////

 if (not assigned(fRaytracingBLASGeometryInfoBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1])) or
    (not assigned(fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1])) or
    (not assigned(fRaytracingBLASGeometryInfoBufferItemBuffers[(fRaytracingBLASGeometryInfoBufferRingIndex+1) and 1])) or
    (not assigned(fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[(fRaytracingBLASGeometryInfoBufferRingIndex+1) and 1])) then begin
  fRaytracingBLASListChanged:=true;
 end;

 if fRaytracingBLASListChanged then begin

  fRaytracingBLASGeometryInfoBufferRingIndex:=(fRaytracingBLASGeometryInfoBufferRingIndex+1) and 1;

  if (not assigned(fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1])) or
     (fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1].Size<(Max(1,fGeometryOffsetArrayList.Count)*SizeOf(TVkUInt32))) then begin
   if assigned(pvApplication) then begin
    pvApplication.WaitForPreviousFrame(true);
   end;
   FreeAndNil(fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1]);
   fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                                                                                RoundUpToPowerOfTwo64(Max(1,fGeometryOffsetArrayList.Count)*SizeOf(TVkUInt32)*2),
                                                                                                                                TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT),
                                                                                                                                TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                                                                [],
                                                                                                                                0,
                                                                                                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                                                                0,
                                                                                                                                0,
                                                                                                                                0,
                                                                                                                                0,
                                                                                                                                0,
                                                                                                                                0,
                                                                                                                                [TpvVulkanBufferFlag.PersistentMappedIfPossible],
                                                                                                                                0,
                                                                                                                                pvAllocationGroupIDScene3DRaytracing,
                                                                                                                                'TpvScene3D.fRaytracingBLASGeometryInfoOffsetBufferItemBuffer'
                                                                                                                               );
   fVulkanDevice.DebugUtils.SetObjectName(fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingBLASGeometryInfoOffsetBufferItemBuffer');
  end;
  if fGeometryOffsetArrayList.Count>0 then begin
   fVulkanDevice.MemoryStaging.Upload(fStagingQueue,
                                      fStagingCommandBuffer,
                                      fStagingFence,
                                      fGeometryOffsetArrayList.ItemArray[0],
                                      fRaytracingBLASGeometryInfoOffsetBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1],
                                      0,
                                      fGeometryOffsetArrayList.Count*SizeOf(TVkUInt32));
  end;

  if (not assigned(fRaytracingBLASGeometryInfoBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1])) or
     (fRaytracingBLASGeometryInfoBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1].Size<(Max(1,fGeometryInfoManager.GeometryInfoList.Count)*SizeOf(TpvRaytracingBLASGeometryInfoBufferItem))) then begin
   if assigned(pvApplication) then begin
    pvApplication.WaitForPreviousFrame(true);
   end;
   FreeAndNil(fRaytracingBLASGeometryInfoBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1]);
   fRaytracingBLASGeometryInfoBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                                                                          RoundUpToPowerOfTwo64(Max(1,fGeometryInfoManager.GeometryInfoList.Count)*SizeOf(TpvRaytracingBLASGeometryInfoBufferItem)*2),
                                                                                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT),
                                                                                                                          TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                                                          [],
                                                                                                                          0,
                                                                                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                                                          0,
                                                                                                                          0,
                                                                                                                          0,
                                                                                                                          0,
                                                                                                                          0,
                                                                                                                          0,
                                                                                                                          [TpvVulkanBufferFlag.PersistentMappedIfPossible],
                                                                                                                          0,
                                                                                                                          pvAllocationGroupIDScene3DRaytracing,
                                                                                                                          'TpvScene3D.fRaytracingBLASGeometryInfoBufferItemBuffer'
                                                                                                                         );
   fVulkanDevice.DebugUtils.SetObjectName(fRaytracingBLASGeometryInfoBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingBLASGeometryInfoBufferItemBuffer');
  end;
  if fGeometryInfoManager.GeometryInfoList.Count>0 then begin
   fVulkanDevice.MemoryStaging.Upload(fStagingQueue,
                                      fStagingCommandBuffer,
                                      fStagingFence,
                                      fGeometryInfoManager.GeometryInfoList.ItemArray[0],
                                      fRaytracingBLASGeometryInfoBufferItemBuffers[fRaytracingBLASGeometryInfoBufferRingIndex and 1],
                                      0,
                                      fGeometryInfoManager.GeometryInfoList.Count*SizeOf(TpvRaytracingBLASGeometryInfoBufferItem));
  end;

 end;

end;

procedure TpvRaytracingBLASManager.CollectAndCalculateSizesForAccelerationStructures;
var BLASQueueIndex:TpvSizeInt;
    BLAS:TBLAS;
begin

 //////////////////////////////////////////////////////////////////////////////
 // Collect and calculate sizes for acceleration structures                  //
 //////////////////////////////////////////////////////////////////////////////

 fScratchSize:=TpvUInt64(64) shl 20;

 fScratchPassSize:=0;

 fScratchPass:=0;

 for BLASQueueIndex:=0 to fBLASQueue.Count-1 do begin

  BLAS:=fBLASQueue.RawItems[BLASQueueIndex];

  if assigned(BLAS) and (BLAS.CountGeometries>0) then begin

   BLAS.ScratchOffset:=fScratchPassSize;
   BLAS.ScratchPass:=fScratchPass;
   if BLAS.BuildScratchSize<BLAS.UpdateScratchSize then begin
    inc(fScratchPassSize,BLAS.UpdateScratchSize); // Update scratch size is bigger than build scratch size
   end else begin
    inc(fScratchPassSize,BLAS.BuildScratchSize); // Build scratch size is bigger than update scratch size
   end;
   if fScratchSize<fScratchPassSize then begin
    fScratchSize:=fScratchPassSize;
   end;
   if fScratchPassSize>=(TpvUInt64(64) shl 20) then begin
    fScratchPassSize:=0;
    inc(fScratchPass);
   end;

  end;

 end;

end;

procedure TpvRaytracingBLASManager.AllocateOrGrowOrShrinkScratchBuffer;
begin

 //////////////////////////////////////////////////////////////////////////////
 // Allocate or grow or shrink scratch buffer                                //
 //////////////////////////////////////////////////////////////////////////////

 if (not assigned(fRaytracingBLASScratchBuffer)) or // Allocate when there is no allocated scratch buffer then
    (fRaytracingBLASScratchBuffer.Size<fScratchSize) or // Grow when it would be needed
    ((fScratchSize>0) and (fScratchSize<(fRaytracingBLASScratchBuffer.Size shr 1))) then begin // Shrink when it would be useful (when it could be smaller by at least than the half)

  if assigned(pvApplication) then begin
   pvApplication.WaitForPreviousFrame(true); // wait on previous frame to avoid destroy still-in-usage buffers.
  end;

  FreeAndNil(fRaytracingBLASScratchBuffer);

  fRaytracingBLASScratchBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                       RoundUpToPowerOfTwo64(fScratchSize),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT),
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
                                                       fVulkanDevice.PhysicalDevice.AccelerationStructurePropertiesKHR.minAccelerationStructureScratchOffsetAlignment,
                                                       pvAllocationGroupIDScene3DRaytracingScratch,
                                                       'TpvScene3D.fRaytracingVulkanScratchBuffer'
                                                      );
  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingBLASScratchBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanScratchBuffer');

 end;

end;

procedure TpvRaytracingBLASManager.BuildOrUpdateAccelerationStructures;
var BLASQueueIndex:TpvSizeInt;
    BLAS:TBLAS;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Enqueue build acceleration structure commands and execute them in       //
 // batches, so that we can build the acceleration structures in parallel   //
 // but also in a way, that we can avoid that the scratch buffer may be     //
 // too large. Therefore this process is divided into multiple pass splits. //
 /////////////////////////////////////////////////////////////////////////////

 fRaytracingAccelerationStructureBuildQueue.Clear;

 fScratchPass:=0;

 for BLASQueueIndex:=0 to fBLASQueue.Count-1 do begin

  BLAS:=fBLASQueue.RawItems[BLASQueueIndex];

  if assigned(BLAS) and (BLAS.CountGeometries>0) then begin

   if fScratchPass<>BLAS.ScratchPass then begin
    if not fRaytracingAccelerationStructureBuildQueue.Empty then begin
     fRaytracingAccelerationStructureBuildQueue.Execute(fCommandBuffer);
     TpvRaytracingAccelerationStructure.MemoryBarrier(fCommandBuffer);
     fRaytracingAccelerationStructureBuildQueue.Clear;
    end;
    fScratchPass:=BLAS.ScratchPass;
   end;
   BLAS.AccelerationStructure.Build(fCommandBuffer,
                                    fRaytracingBLASScratchBuffer,
                                    BLAS.ScratchOffset,
                                    BLAS.fEnqueueState=TBLAS.TEnqueueState.Update,
                                    nil,
                                    fRaytracingAccelerationStructureBuildQueue);

  end;

 end;

 if not fRaytracingAccelerationStructureBuildQueue.Empty then begin
  fRaytracingAccelerationStructureBuildQueue.Execute(fCommandBuffer);
  TpvRaytracingAccelerationStructure.MemoryBarrier(fCommandBuffer);
  fRaytracingAccelerationStructureBuildQueue.Clear;
 end;

end;

procedure TpvRaytracingBLASManager.UpdateBLASInstancesForTLAS;
var BufferMemoryBarriers:array[0..1] of TVkBufferMemoryBarrier;
    BufferCopy:TVkBufferCopy;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Update BLAS instances for TLAS                                          //
 /////////////////////////////////////////////////////////////////////////////

 fRaytracingTLASBLASInstancesBufferSize:=RoundUpToPowerOfTwo64(Max(1,fAccelerationStructureInstanceKHRArrayList.Count)*SizeOf(TVkAccelerationStructureInstanceKHR));

 if (not assigned(fRaytracingTLASBLASInstancesBuffer)) or
    (fRaytracingTLASBLASInstancesBuffer.Size<fRaytracingTLASBLASInstancesBufferSize) then begin

  if assigned(pvApplication) then begin
   pvApplication.WaitForPreviousFrame(true); // wait on previous frame to avoid destroy still-in-usage buffers.
  end;

  FreeAndNil(fRaytracingTLASBLASInstancesBuffer);

  fRaytracingTLASBLASInstancesBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                             fRaytracingTLASBLASInstancesBufferSize,
                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR) or TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                             [],
                                                             0,
                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                             0,
                                                             0,
                                                             0,
                                                             0,
                                                             0,
                                                             0,
                                                             [TpvVulkanBufferFlag.PersistentMappedIfPossible],
                                                             0,
                                                             pvAllocationGroupIDScene3DRaytracingScratch,
                                                             'TpvScene3D.fRaytracingVulkanTLASBLASInstancesBuffer'
                                                            );
  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingTLASBLASInstancesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanTLASBLASInstancesBuffer');

  fRaytracingMustUpdateTLAS:=true;

 end;

 if (not assigned(fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex])) or
    (fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex].Size<fRaytracingTLASBLASInstancesBufferSize) then begin

  if assigned(pvApplication) then begin
   pvApplication.WaitForPreviousFrame(true); // wait on previous frame to avoid destroy still-in-usage buffers.
  end;

  FreeAndNil(fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex]);

  fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                                   fRaytracingTLASBLASInstancesBufferSize,
                                                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR) or TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT),
                                                                                   TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                   [],
                                                                                   0,
                                                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                                   0,
                                                                                   0,
                                                                                   0,
                                                                                   0,
                                                                                   0,
                                                                                   0,
                                                                                   [TpvVulkanBufferFlag.PersistentMappedIfPossible],
                                                                                   0,
                                                                                   pvAllocationGroupIDScene3DRaytracingScratch,
                                                                                   'TpvScene3D.fRaytracingVulkanTLASBLASInstancesBuffers['+IntToStr(fInFlightFrameIndex)+']'
                                                                                  );
  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex].Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanTLASBLASInstancesBuffers['+IntToStr(fInFlightFrameIndex)+']');

  fRaytracingMustUpdateTLAS:=true;

 end;

 if fAccelerationStructureInstanceKHRArrayList.Count>0 then begin

  fVulkanDevice.MemoryStaging.Upload(fStagingQueue,
                                     fStagingCommandBuffer,
                                     fStagingFence,
                                     fAccelerationStructureInstanceKHRArrayList.ItemArray[0],
                                     fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex],
                                     0,
                                     fAccelerationStructureInstanceKHRArrayList.Count*SizeOf(TVkAccelerationStructureInstanceKHR));

  // Copy in-flight-frame-wise fRaytracingTLASBLASInstancesBuffers to the single GPU-side fRaytracingTLASBLASInstancesBuffer

  // This code ensures synchronization between the CPU and GPU by copying data from the CPU-side buffer to a temporary GPU-side
  // buffer, and then to the final GPU-side buffer. This avoids performance issues caused by waiting for the GPU to finish its
  // work before using the CPU-changed buffer on the GPU.

  BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex].Handle,
                                                         0,
                                                         fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex].Size);

  BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR) or
                                                         TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR) or
                                                         TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or
                                                         TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                         TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         VK_QUEUE_FAMILY_IGNORED,
                                                         fRaytracingTLASBLASInstancesBuffer.Handle,
                                                         0,
                                                         fRaytracingTLASBLASInstancesBuffer.Size);

   fCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     0,
                                     0,nil,
                                     2,@BufferMemoryBarriers[0],
                                     0,nil);

   BufferCopy:=TVkBufferCopy.Create(TVkDeviceSize(0),
                                    TVkDeviceSize(0),
                                    fAccelerationStructureInstanceKHRArrayList.Count*SizeOf(TVkAccelerationStructureInstanceKHR));

   fCommandBuffer.CmdCopyBuffer(fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex].Handle,
                                fRaytracingTLASBLASInstancesBuffer.Handle,
                                1,@BufferCopy);

   BufferMemoryBarriers[0]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR) or
                                                          TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR) or
                                                          TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or
                                                          TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          fRaytracingTLASBLASInstancesBuffer.Handle,
                                                          0,
                                                          fRaytracingTLASBLASInstancesBuffer.Size);

   BufferMemoryBarriers[1]:=TVkBufferMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                          TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or
                                                          TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          VK_QUEUE_FAMILY_IGNORED,
                                                          fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex].Handle,
                                                          0,
                                                          fRaytracingTLASBLASInstancesBuffers[fInFlightFrameIndex].Size);

   fCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR),
                                     0,
                                     0,nil,
                                     2,@BufferMemoryBarriers[0],
                                     0,nil);

 end;

end;

procedure TpvRaytracingBLASManager.CreateOrUpdateTLAS;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Create or update TLAS                                                   //
 /////////////////////////////////////////////////////////////////////////////

 fMustTLASUpdate:=false;

 if assigned(fRaytracingTLAS) then begin

  if fRaytracingBLASListChanged or
     (fRaytracingTLAS.Instances.data.deviceAddress<>fRaytracingTLASBLASInstancesBuffer.DeviceAddress) or
     (fRaytracingTLAS.CountInstances<>fAccelerationStructureInstanceKHRArrayList.Count) then begin

   fRaytracingTLAS.Update(fRaytracingTLASBLASInstancesBuffer.DeviceAddress,
                          fAccelerationStructureInstanceKHRArrayList.Count,
                          TVkBuildAccelerationStructureFlagsKHR(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR),
                          false);

   fMustTLASUpdate:=true;

  end;

 end else begin

  fRaytracingTLAS:=TpvRaytracingTopLevelAccelerationStructure.Create(fVulkanDevice,
                                                                     fRaytracingTLASBLASInstancesBuffer.DeviceAddress,
                                                                     fAccelerationStructureInstanceKHRArrayList.Count,
                                                                     TVkBuildAccelerationStructureFlagsKHR(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR),
                                                                     false);

 end;

//fRaytracingTLAS.UpdateInstanceAddress(fRaytracingTLASBLASInstancesBuffer.DeviceAddress);

end;

procedure TpvRaytracingBLASManager.AllocateOrGrowTLASBuffer;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Allocate or grow TLAS buffer                                            //
 /////////////////////////////////////////////////////////////////////////////

 if (not assigned(fRaytracingTLASBuffer)) or
    (fRaytracingTLASBuffer.Size<Max(1,fRaytracingTLAS.BuildSizesInfo.accelerationStructureSize)) or
    fMustTLASUpdate then begin

  if assigned(pvApplication) then begin
   pvApplication.WaitForPreviousFrame(true); // wait on previous frame to avoid destroy still-in-usage buffers.
  end;

  fRaytracingTLAS.Finalize;

  if (not assigned(fRaytracingTLASBuffer)) or
     (fRaytracingTLASBuffer.Size<Max(1,fRaytracingTLAS.BuildSizesInfo.accelerationStructureSize)) then begin

   FreeAndNil(fRaytracingTLASBuffer);

   fRaytracingTLASBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                 RoundUpToPowerOfTwo64(Max(1,fRaytracingTLAS.BuildSizesInfo.accelerationStructureSize)),
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
                                                 256,
                                                 pvAllocationGroupIDScene3DRaytracingTLAS,
                                                 'TpvScene3D.fRaytracingVulkanTLASBuffer'
                                                );
   fVulkanDevice.DebugUtils.SetObjectName(fRaytracingTLASBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanTLASBuffer');

  end;

  fRaytracingTLAS.Initialize(fRaytracingTLASBuffer,0);
  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingTLAS.AccelerationStructure,VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR,'TpvScene3D.fRaytracingTLAS');

 end;

end;

procedure TpvRaytracingBLASManager.AllocateOrGrowTLASScratchBuffer;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Allocate or grow TLAS scratch buffer                                    //
 /////////////////////////////////////////////////////////////////////////////

 if (not assigned(fRaytracingTLASScratchBuffer)) or
    (fRaytracingTLASScratchBuffer.Size<Max(1,Max(fRaytracingTLAS.BuildSizesInfo.buildScratchSize,fRaytracingTLAS.BuildSizesInfo.updateScratchSize))) then begin

  if assigned(pvApplication) then begin
   pvApplication.WaitForPreviousFrame(true); // wait on previous frame to avoid destroy still-in-usage buffers.
  end;

  FreeAndNil(fRaytracingTLASScratchBuffer);

  fRaytracingTLASScratchBuffer:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                       RoundUpToPowerOfTwo64(Max(1,Max(fRaytracingTLAS.BuildSizesInfo.buildScratchSize,fRaytracingTLAS.BuildSizesInfo.updateScratchSize))),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT),
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
                                                       fVulkanDevice.PhysicalDevice.AccelerationStructurePropertiesKHR.minAccelerationStructureScratchOffsetAlignment,
                                                       pvAllocationGroupIDScene3DRaytracingScratch,
                                                       'TpvScene3D.fRaytracingVulkanTLASScratchBuffer'
                                                      );
  fVulkanDevice.DebugUtils.SetObjectName(fRaytracingTLASScratchBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fRaytracingVulkanTLASScratchBuffer');

 end;

end;

procedure TpvRaytracingBLASManager.BuildOrUpdateTLAS;
begin

 /////////////////////////////////////////////////////////////////////////////
 // Build or update TLAS                                                    //
 /////////////////////////////////////////////////////////////////////////////

 if fRaytracingMustUpdateTLAS or fRaytracingBLASListChanged then begin

  fRaytracingTLAS.Build(fCommandBuffer,
                        fRaytracingTLASScratchBuffer,
                        0,
                        false,
                        nil);

  TpvRaytracingAccelerationStructure.MemoryBarrier(fCommandBuffer);

 end;

end;

procedure TpvRaytracingBLASManager.Update(const aStagingQueue:TpvVulkanQueue;
                                          const aStagingCommandBuffer:TpvVulkanCommandBuffer;
                                          const aStagingFence:TpvVulkanFence;
                                          const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aInFlightFrameIndex:TpvSizeInt;
                                          const aLabels:Boolean);
var FrameDoneMask:TpvUInt32;
begin

 fStagingQueue:=aStagingQueue;

 fStagingCommandBuffer:=aStagingCommandBuffer;

 fStagingFence:=aStagingFence;

 fCommandBuffer:=aCommandBuffer;

 fInFlightFrameIndex:=aInFlightFrameIndex;

 FrameDoneMask:=TpvUInt32(1) shl aInFlightFrameIndex;

 if (TPasMPInterlocked.ExchangeBitwiseOr(fUpdateRaytracingFrameDoneMask,FrameDoneMask) and FrameDoneMask)=0 then begin

  fLock.Acquire;
  try

   fBLASQueue.ClearNoFree;

   if aLabels then begin
    fVulkanDevice.DebugUtils.CmdBufLabelBegin(aCommandBuffer,'TpvRaytracing.Update',[1.0,0.5,0.25,1.0]);
   end;

   fRaytracingBLASListChanged:=false; // Assume, that the BLAS list has not changed yet

   fRaytracingMustUpdateTLAS:=false;

   HostMemoryBarrier;

   WaitForPreviousFrame;

   HandleEmptyBLAS;

   ProcessContentUpdate;

   BuildOrUpdateBLASMetaData;

   CollectAndCalculateSizesForAccelerationStructures;

   AllocateOrGrowOrShrinkScratchBuffer;

   BuildOrUpdateAccelerationStructures;

   UpdateBLASInstancesForTLAS;

   CreateOrUpdateTLAS;

   AllocateOrGrowTLASBuffer;

   AllocateOrGrowTLASScratchBuffer;

   BuildOrUpdateTLAS;

   if aLabels then begin
    fVulkanDevice.DebugUtils.CmdBufLabelEnd(aCommandBuffer);
   end;

  finally
   fLock.Release;
  end;

 end;

end;
{begin

 if TPasMPInterlocked.CompareExchange(fDirty,TPasMPBool32(false),TPasMPBool32(true)) then begin

 end;

end;}

{ TpvRaytracingManager }

constructor TpvRaytracingManager.Create(const aDevice:TpvVulkanDevice);
begin
 
 inherited Create;
 
 fVulkanDevice:=aDevice;
 
 fBLASManager:=TpvRaytracingBLASManager.Create(fVulkanDevice,2);

end;

destructor TpvRaytracingManager.Destroy;
begin

 FreeAndNil(fBLASManager);

 inherited Destroy;

end;

end.
