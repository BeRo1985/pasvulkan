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
     PasVulkan.Collections,
     PasVulkan.Framework;

type EpvRaytracing=class(Exception);

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

     TpvRaytracingAccelerationStructure=class;

     TpvRaytracingAccelerationStructureList=TpvObjectGenericList<TpvRaytracingAccelerationStructure>;

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
                          const aAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure); reintroduce;
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
      public
       property AccelerationStructureInstance:TVkAccelerationStructureInstanceKHR read fAccelerationStructureInstance write fAccelerationStructureInstance; 
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
      public
       property Instances:TVkAccelerationStructureGeometryInstancesDataKHR read fInstances;
       property CountInstances:TVkUInt32 read fCountInstances;
      published
     end;

implementation

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
begin
 Assert(assigned(aCommandBuffer));
 Assert(fDevice=aCommandBuffer.Device);
 if fBuildGeometryInfos.Count>0 then begin
  Assert(fBuildGeometryInfos.Count=fBuildOffsetInfoPtrs.Count);
  try
   fDevice.Commands.Commands.CmdBuildAccelerationStructuresKHR(aCommandBuffer.Handle,
                                                               fBuildGeometryInfos.Count,
                                                               @fBuildGeometryInfos.ItemArray[0],
                                                               @fBuildOffsetInfoPtrs.ItemArray[0]);
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
                                                                         const aAccelerationStructure:TpvRaytracingBottomLevelAccelerationStructure);
begin

 inherited Create;

 fDevice:=aDevice;

 FillChar(fAccelerationStructureInstance,SizeOf(TVkAccelerationStructureInstanceKHR),#0);

 SetTransform(aTransform);

 fAccelerationStructureInstance.instanceCustomIndex:=aInstanceCustomIndex;
 fAccelerationStructureInstance.mask:=aMask;
 fAccelerationStructureInstance.instanceShaderBindingTableRecordOffset:=aInstanceShaderBindingTableRecordOffset;
 fAccelerationStructureInstance.flags:=aFlags;
 fAccelerationStructureInstance.accelerationStructureReference:=0;

 fAccelerationStructure:=nil;
 
 SetAccelerationStructure(aAccelerationStructure);

end;

destructor TpvRaytracingBottomLevelAccelerationStructureInstance.Destroy;
begin
 inherited Destroy;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetTransform:TpvMatrix4x4;
begin
{PVkTransformMatrixKHR(Pointer(@result))^:=fAccelerationStructureInstance.transform;
 result.RawComponents[3,0]:=0.0;
 result.RawComponents[3,1]:=0.0;
 result.RawComponents[3,2]:=0.0;
 result.RawComponents[3,3]:=1.0;}
 // Row-order => Column-order
 result.RawComponents[0,0]:=fAccelerationStructureInstance.transform.matrix[0,0];
 result.RawComponents[0,1]:=fAccelerationStructureInstance.transform.matrix[1,0];
 result.RawComponents[0,2]:=fAccelerationStructureInstance.transform.matrix[2,0];
 result.RawComponents[0,3]:=0.0;
 result.RawComponents[1,0]:=fAccelerationStructureInstance.transform.matrix[0,1];
 result.RawComponents[1,1]:=fAccelerationStructureInstance.transform.matrix[1,1];
 result.RawComponents[1,2]:=fAccelerationStructureInstance.transform.matrix[2,1];
 result.RawComponents[1,3]:=0.0;
 result.RawComponents[2,0]:=fAccelerationStructureInstance.transform.matrix[0,2];
 result.RawComponents[2,1]:=fAccelerationStructureInstance.transform.matrix[1,2];
 result.RawComponents[2,2]:=fAccelerationStructureInstance.transform.matrix[2,2];
 result.RawComponents[2,3]:=0.0;
 result.RawComponents[3,0]:=fAccelerationStructureInstance.transform.matrix[0,3];
 result.RawComponents[3,1]:=fAccelerationStructureInstance.transform.matrix[1,3];
 result.RawComponents[3,2]:=fAccelerationStructureInstance.transform.matrix[2,3];
 result.RawComponents[3,3]:=1.0;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetTransform(const aTransform:TpvMatrix4x4);
begin
//fAccelerationStructureInstance.transform:=PVkTransformMatrixKHR(Pointer(@aTransform))^;
 // Column-order => Row-order
 fAccelerationStructureInstance.transform.matrix[0,0]:=aTransform.RawComponents[0,0];
 fAccelerationStructureInstance.transform.matrix[0,1]:=aTransform.RawComponents[1,0];
 fAccelerationStructureInstance.transform.matrix[0,2]:=aTransform.RawComponents[2,0];
 fAccelerationStructureInstance.transform.matrix[0,3]:=aTransform.RawComponents[3,0];
 fAccelerationStructureInstance.transform.matrix[1,0]:=aTransform.RawComponents[0,1];
 fAccelerationStructureInstance.transform.matrix[1,1]:=aTransform.RawComponents[1,1];
 fAccelerationStructureInstance.transform.matrix[1,2]:=aTransform.RawComponents[2,1];
 fAccelerationStructureInstance.transform.matrix[1,3]:=aTransform.RawComponents[3,1];
 fAccelerationStructureInstance.transform.matrix[2,0]:=aTransform.RawComponents[0,2];
 fAccelerationStructureInstance.transform.matrix[2,1]:=aTransform.RawComponents[1,2];
 fAccelerationStructureInstance.transform.matrix[2,2]:=aTransform.RawComponents[2,2];
 fAccelerationStructureInstance.transform.matrix[2,3]:=aTransform.RawComponents[3,2];
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetInstanceCustomIndex:TVkUInt32;
begin
 result:=fAccelerationStructureInstance.instanceCustomIndex;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetInstanceCustomIndex(const aInstanceCustomIndex:TVkUInt32);
begin
 fAccelerationStructureInstance.instanceCustomIndex:=aInstanceCustomIndex;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetMask:TVkUInt32;
begin
 result:=fAccelerationStructureInstance.mask;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetMask(const aMask:TVkUInt32);
begin
 fAccelerationStructureInstance.mask:=aMask;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetInstanceShaderBindingTableRecordOffset:TVkUInt32;
begin
 result:=fAccelerationStructureInstance.instanceShaderBindingTableRecordOffset;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetInstanceShaderBindingTableRecordOffset(const aInstanceShaderBindingTableRecordOffset:TVkUInt32);
begin
 fAccelerationStructureInstance.instanceShaderBindingTableRecordOffset:=aInstanceShaderBindingTableRecordOffset;
end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetFlags:TVkGeometryInstanceFlagsKHR;
begin
 result:=fAccelerationStructureInstance.flags;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetFlags(const aFlags:TVkGeometryInstanceFlagsKHR);
begin
 fAccelerationStructureInstance.flags:=aFlags;
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

   fAccelerationStructureInstance.accelerationStructureReference:=fDevice.Commands.Commands.GetAccelerationStructureDeviceAddressKHR(fDevice.Handle,@AddressInfo);

  end else begin

   fAccelerationStructureInstance.accelerationStructureReference:=0;

  end;

 end; 

end;

function TpvRaytracingBottomLevelAccelerationStructureInstance.GetAccelerationStructureDeviceAddress:TVkDeviceAddress;
begin
 result:=fAccelerationStructureInstance.accelerationStructureReference;
end;

procedure TpvRaytracingBottomLevelAccelerationStructureInstance.SetAccelerationStructureDeviceAddress(const aAccelerationStructureDeviceAddress:TVkDeviceAddress);
begin
 fAccelerationStructureInstance.accelerationStructureReference:=aAccelerationStructureDeviceAddress;
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


end.
