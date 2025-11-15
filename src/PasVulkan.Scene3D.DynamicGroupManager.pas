(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2025, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.DynamicGroupManager;
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

uses SysUtils,
     Classes,
     Math,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.PasMP,
     PasVulkan.Scene3D;

type { TpvScene3DDynamicGroupManager }

     TpvScene3DDynamicGroupManager=class
      public       
       type // Generic key types for identifying groups
            TGroupKey=TpvUInt64;
            PGroupKey=^TGroupKey;
            
            // Generic key types for identifying states
            TStateKey=record
             public
              procedure Clear;
              function Equals(const aOther:TStateKey):Boolean;
              class function Create(const aHash:TpvUInt64):TStateKey; static;
              class function CreateFromData(const aData:array of TpvUInt64):TStateKey; static;
             public
              class operator Equal(const aLeft,aRight:TStateKey):boolean; {$ifdef CAN_INLINE}inline;{$endif}
              class operator NotEqual(const aLeft,aRight:TStateKey):boolean; {$ifdef CAN_INLINE}inline;{$endif}
             public
              Hash:TpvUInt64;
              Data:array[0..7] of TpvUInt64; // Flexible data storage for state comparison
            end;
            PStateKey=^TStateKey;
            
            // Callback types for custom logic
            TStateKeyCompareFunc=function(const aKeyA,aKeyB:TStateKey):Boolean of object;
            
            // TInstance pool entry
            TInstancePoolEntry=class
             private
              fInstance:TpvScene3D.TGroup.TInstance;
              fStateKey:TStateKey;
              fGroupKey:TGroupKey;
              fReferenceCount:TpvInt32;
              fActive:Boolean;
              fLastUsedFrameIndex:TpvUInt64;
              fIndex:TpvSizeInt; // Position in fInstancePool
             public
              property Instance:TpvScene3D.TGroup.TInstance read fInstance write fInstance;
              property StateKey:TStateKey read fStateKey write fStateKey;
              property GroupKey:TGroupKey read fGroupKey write fGroupKey;
              property ReferenceCount:TpvInt32 read fReferenceCount write fReferenceCount;
              property Active:Boolean read fActive write fActive;
              property LastUsedFrameIndex:TpvUInt64 read fLastUsedFrameIndex write fLastUsedFrameIndex;
              property Index:TpvSizeInt read fIndex write fIndex;
            end;
            TInstancePoolEntries=TpvObjectGenericList<TInstancePoolEntry>;
            
            // Group registration info
            TGroupInfo=class
             private
              fGroup:TpvScene3D.TGroup;
              fMaxInstances:TpvSizeInt; // < 0 means dynamic, no limit
              fCurrentInstanceCount:TpvSizeInt;
             public
              property Group:TpvScene3D.TGroup read fGroup write fGroup;
              property MaxInstances:TpvSizeInt read fMaxInstances write fMaxInstances; // < 0 = dynamic
              property CurrentInstanceCount:TpvSizeInt read fCurrentInstanceCount write fCurrentInstanceCount;
            end;
            TGroupInfos=TpvHashMap<TGroupKey,TGroupInfo>;
            
            // TRenderInstance pool entry
            TRenderInstancePoolEntry=class
             private
              fRenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
              fOwningInstance:TpvScene3D.TGroup.TInstance;
              fActive:Boolean;
              fIndex:TpvSizeInt; // Position in fRenderInstancePool
             public
              property RenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance read fRenderInstance write fRenderInstance;
              property OwningInstance:TpvScene3D.TGroup.TInstance read fOwningInstance write fOwningInstance;
              property Active:Boolean read fActive write fActive;
              property Index:TpvSizeInt read fIndex write fIndex;
            end;
            TRenderInstancePoolEntries=TpvObjectGenericList<TRenderInstancePoolEntry>;
            
            // Type aliases for hash maps
            TGroupRegistry=TpvHashMap<TGroupKey,TpvScene3D.TGroup>;
            TActiveInstances=TpvHashMap<TStateKey,TInstancePoolEntry>;
            TInstanceToPoolEntry=TpvHashMap<TpvScene3D.TGroup.TInstance,TInstancePoolEntry>;
            TRenderInstanceToPoolEntry=TpvHashMap<TpvScene3D.TGroup.TInstance.TRenderInstance,TRenderInstancePoolEntry>;
            
            // Type aliases for free lists
            TInstanceFreeList=TpvDynamicStack<TInstancePoolEntry>;
            TRenderInstanceFreeList=TpvDynamicStack<TRenderInstancePoolEntry>;

      private

       fSceneInstance:TpvScene3D;
       fLock:TPasMPCriticalSection;
       
       // Group registry: GroupKey -> TGroup
       fGroupRegistry:TGroupRegistry;
       
       // Group info: GroupKey -> TGroupInfo (with max instance limits)
       fGroupInfos:TGroupInfos;
       
       // Active instances: StateKey -> Instance pool entry
       fActiveInstances:TActiveInstances;
       fActiveInstancesCount:TpvSizeInt;
       
       // Reverse lookup maps for O(1) access
       fInstanceToPoolEntry:TInstanceToPoolEntry;
       fRenderInstanceToPoolEntry:TRenderInstanceToPoolEntry;
       
       // Instance pool (all allocated entries)
       fInstancePool:TInstancePoolEntries;
       fInstanceFreeList:TInstanceFreeList;
       
       // RenderInstance pool (optional, for lightweight pooling)
       fRenderInstancePool:TRenderInstancePoolEntries;
       fRenderInstanceFreeList:TRenderInstanceFreeList;
       
       // Frame counter for LRU tracking
       fCurrentFrameIndex:TpvUInt64;
       
       // Optional custom state comparison
       fCustomStateCompare:TStateKeyCompareFunc;
       
       function CompareStateKeys(const aKeyA,aKeyB:TStateKey):Boolean;
       function FindOrCreateInstancePoolEntry(const aGroup:TpvScene3D.TGroup;const aStateKey:TStateKey):TInstancePoolEntry;
       function AllocateInstancePoolEntry:TInstancePoolEntry;
       function AllocateRenderInstancePoolEntry:TRenderInstancePoolEntry;

      public

       constructor Create(const aSceneInstance:TpvScene3D);
       destructor Destroy; override;
       
       // Group management
       function RegisterGroup(const aGroupKey:TGroupKey;const aGroup:TpvScene3D.TGroup;const aMaxInstances:TpvSizeInt=1):Boolean;
       function UnregisterGroup(const aGroupKey:TGroupKey):Boolean;
       function GetGroup(const aGroupKey:TGroupKey):TpvScene3D.TGroup;
       function HasGroup(const aGroupKey:TGroupKey):Boolean;
       
       // Instance management - instances are NEVER freed, only recycled
       function AcquireInstance(const aGroupKey:TGroupKey;const aStateKey:TStateKey):TpvScene3D.TGroup.TInstance;
       procedure ReleaseInstance(const aInstance:TpvScene3D.TGroup.TInstance);
       
       // RenderInstance management
       function AcquireRenderInstance(const aInstance:TpvScene3D.TGroup.TInstance):TpvScene3D.TGroup.TInstance.TRenderInstance;
       procedure ReleaseRenderInstance(const aRenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance);
       
       // Frame management
       procedure BeginFrame;
       
       // Statistics
       function GetInstancePoolSize:TpvSizeInt;
       function GetActiveInstanceCount:TpvSizeInt;
       function GetRenderInstancePoolSize:TpvSizeInt;
       
       // Custom state comparison
       property CustomStateCompare:TStateKeyCompareFunc read fCustomStateCompare write fCustomStateCompare;
     end;

implementation

{ TpvScene3DDynamicGroupManager.TStateKey }

procedure TpvScene3DDynamicGroupManager.TStateKey.Clear;
var Index:TpvSizeInt;
begin
 Hash:=0;
 for Index:=Low(Data) to High(Data) do begin
  Data[Index]:=0;
 end;
end;

function TpvScene3DDynamicGroupManager.TStateKey.Equals(const aOther:TStateKey):Boolean;
var Index:TpvSizeInt;
begin
 result:=Hash=aOther.Hash;
 if result then begin
  for Index:=Low(Data) to High(Data) do begin
   if Data[Index]<>aOther.Data[Index] then begin
    result:=false;
    exit;
   end;
  end;
 end;
end;

class function TpvScene3DDynamicGroupManager.TStateKey.Create(const aHash:TpvUInt64):TStateKey;
begin
 result.Clear;
 result.Hash:=aHash;
end;

class function TpvScene3DDynamicGroupManager.TStateKey.CreateFromData(const aData:array of TpvUInt64):TStateKey;
var Index:TpvSizeInt;
    Hasher:TpvUInt64;
begin
 result.Clear;
 Hasher:=14695981039346656037;
 for Index:=Low(aData) to Min(High(aData),High(result.Data)) do begin
  result.Data[Index]:=aData[Index];
  Hasher:=(Hasher xor aData[Index])*1099511628211;
 end;
 result.Hash:=Hasher;
end;

class operator TpvScene3DDynamicGroupManager.TStateKey.Equal(const aLeft,aRight:TStateKey):boolean;
begin
 result:=aLeft.Equals(aRight);
end;

class operator TpvScene3DDynamicGroupManager.TStateKey.NotEqual(const aLeft,aRight:TStateKey):boolean;
begin
 result:=not aLeft.Equals(aRight);
end;

{ TpvScene3DDynamicGroupManager }

constructor TpvScene3DDynamicGroupManager.Create(const aSceneInstance:TpvScene3D);
begin
 inherited Create;
 
 fSceneInstance:=aSceneInstance;
 
 fLock:=TPasMPCriticalSection.Create;
 
 fGroupRegistry:=TGroupRegistry.Create(nil);
 fGroupInfos:=TGroupInfos.Create(nil);
 
 fActiveInstances:=TActiveInstances.Create(nil);
 fActiveInstancesCount:=0;
 
 fInstanceToPoolEntry:=TInstanceToPoolEntry.Create(nil);
 fRenderInstanceToPoolEntry:=TRenderInstanceToPoolEntry.Create(nil);
 
 fInstancePool:=TInstancePoolEntries.Create(true);
 fInstanceFreeList.Initialize;
 
 fRenderInstancePool:=TRenderInstancePoolEntries.Create(true);
 fRenderInstanceFreeList.Initialize;
 
 fCurrentFrameIndex:=0;
 
 fCustomStateCompare:=nil;

end;

destructor TpvScene3DDynamicGroupManager.Destroy;
var Index:TpvSizeInt;
    PoolEntry:TInstancePoolEntry;
    RenderPoolEntry:TRenderInstancePoolEntry;
begin

 // Clean up render instances
{for Index:=0 to fRenderInstancePool.Count-1 do begin
  RenderPoolEntry:=fRenderInstancePool.Items[Index];
  FreeAndNil(RenderPoolEntry.fRenderInstance);
 end;}
 FreeAndNil(fRenderInstancePool);
 fRenderInstanceFreeList.Finalize;
 
 // Clean up instances - they're finally freed on shutdown
{for Index:=0 to fInstancePool.Count-1 do begin
  PoolEntry:=fInstancePool.Items[Index];
  FreeAndNil(PoolEntry.fInstance);
 end;}
 FreeAndNil(fInstancePool);
 fInstanceFreeList.Finalize;
 
 FreeAndNil(fInstanceToPoolEntry);
 FreeAndNil(fRenderInstanceToPoolEntry);
 
 FreeAndNil(fActiveInstances);
 FreeAndNil(fGroupInfos);
 FreeAndNil(fGroupRegistry);
 FreeAndNil(fLock);

 inherited Destroy;
end;

function TpvScene3DDynamicGroupManager.CompareStateKeys(const aKeyA,aKeyB:TStateKey):Boolean;
begin
 if assigned(fCustomStateCompare) then begin
  result:=fCustomStateCompare(aKeyA,aKeyB);
 end else begin
  result:=aKeyA.Equals(aKeyB);
 end;
end;

function TpvScene3DDynamicGroupManager.AllocateInstancePoolEntry:TInstancePoolEntry;
begin
 // Reuse from free list (Pop returns the item in result)
 if not fInstanceFreeList.Pop(result) then begin
  // Otherwise allocate new entry
  result:=TInstancePoolEntry.Create;
  result.fIndex:=fInstancePool.Count;
  fInstancePool.Add(result);
 end;
 result.fReferenceCount:=0;
 result.fActive:=false;
 result.fLastUsedFrameIndex:=fCurrentFrameIndex;
end;

function TpvScene3DDynamicGroupManager.AllocateRenderInstancePoolEntry:TRenderInstancePoolEntry;
begin
 // Reuse from free list (Pop returns the item in result)
 if not fRenderInstanceFreeList.Pop(result) then begin
  // Otherwise allocate new entry
  result:=TRenderInstancePoolEntry.Create;
  result.fIndex:=fRenderInstancePool.Count;
  fRenderInstancePool.Add(result);
 end;
end;

function TpvScene3DDynamicGroupManager.RegisterGroup(const aGroupKey:TGroupKey;const aGroup:TpvScene3D.TGroup;const aMaxInstances:TpvSizeInt):Boolean;
var GroupInfo:TGroupInfo;
    Index:TpvSizeInt;
    PoolEntry:TInstancePoolEntry;
begin
 fLock.Acquire;
 try

  result:=not fGroupRegistry.ExistKey(aGroupKey);

  if result then begin

   fGroupRegistry.Add(aGroupKey,aGroup);

   // Register group info with max instance limit (< 0 = dynamic, no limit)
   GroupInfo:=TGroupInfo.Create;
   GroupInfo.fGroup:=aGroup;
   GroupInfo.fMaxInstances:=aMaxInstances;
   GroupInfo.fCurrentInstanceCount:=0;
   fGroupInfos.Add(aGroupKey,GroupInfo);

   // Preallocate instances only if max is >= 0 (fixed pool)
   if aMaxInstances>=0 then begin

    for Index:=0 to aMaxInstances-1 do begin
     PoolEntry:=AllocateInstancePoolEntry;
     PoolEntry.fInstance:=aGroup.CreateInstance;
     PoolEntry.fGroupKey:=aGroupKey;
     PoolEntry.fActive:=false;
     PoolEntry.fReferenceCount:=0;
     fInstanceFreeList.Push(PoolEntry);
    end;

   end;

  end;

 finally
  fLock.Release;
 end;

end;

function TpvScene3DDynamicGroupManager.UnregisterGroup(const aGroupKey:TGroupKey):Boolean;
begin
 fLock.Acquire;
 try
  result:=fGroupRegistry.Delete(aGroupKey);
 finally
  fLock.Release;
 end;
end;

function TpvScene3DDynamicGroupManager.GetGroup(const aGroupKey:TGroupKey):TpvScene3D.TGroup;
begin
 fLock.Acquire;
 try
  if not fGroupRegistry.TryGet(aGroupKey,result) then begin
   result:=nil;
  end;
 finally
  fLock.Release;
 end;
end;

function TpvScene3DDynamicGroupManager.HasGroup(const aGroupKey:TGroupKey):Boolean;
begin
 fLock.Acquire;
 try
  result:=fGroupRegistry.ExistKey(aGroupKey);
 finally
  fLock.Release;
 end;
end;

function TpvScene3DDynamicGroupManager.FindOrCreateInstancePoolEntry(const aGroup:TpvScene3D.TGroup;const aStateKey:TStateKey):TInstancePoolEntry;
var PoolIndex:TpvSizeInt;
    PoolEntry:TInstancePoolEntry;
begin

 // First check if we have an active instance with this state
 if fActiveInstances.TryGet(aStateKey,result) then begin
  inc(result.fReferenceCount);
  result.fLastUsedFrameIndex:=fCurrentFrameIndex;
  exit;
 end;
 
 // Look for inactive instance with matching state that we can reactivate
 for PoolIndex:=0 to fInstancePool.Count-1 do begin
  PoolEntry:=fInstancePool.Items[PoolIndex];
  if (not PoolEntry.fActive) and assigned(PoolEntry.fInstance) and
     (PoolEntry.fInstance.Group=aGroup) and
     CompareStateKeys(PoolEntry.fStateKey,aStateKey) then begin
   // Found a reusable instance
   result:=PoolEntry;
   result.fActive:=true;
   result.fReferenceCount:=1;
   result.fLastUsedFrameIndex:=fCurrentFrameIndex;
   fActiveInstances.Add(aStateKey,result);
   inc(fActiveInstancesCount);
   exit;
  end;
 end;
 
 // No reusable instance found, create new one
 result:=AllocateInstancePoolEntry;
 result.fInstance:=aGroup.CreateInstance;
 result.fStateKey:=aStateKey;
 result.fReferenceCount:=1;
 result.fActive:=true;
 result.fLastUsedFrameIndex:=fCurrentFrameIndex;
 fActiveInstances.Add(aStateKey,result);
 fInstanceToPoolEntry.Add(result.fInstance,result);
 inc(fActiveInstancesCount);

end;

function TpvScene3DDynamicGroupManager.AcquireInstance(const aGroupKey:TGroupKey;const aStateKey:TStateKey):TpvScene3D.TGroup.TInstance;
var Group:TpvScene3D.TGroup;
    PoolEntry:TInstancePoolEntry;
begin

 result:=nil;

 fLock.Acquire;
 try

  Group:=GetGroup(aGroupKey);

  if assigned(Group) then begin

   PoolEntry:=FindOrCreateInstancePoolEntry(Group,aStateKey);
   result:=PoolEntry.fInstance;

  end;

 finally
  fLock.Release;
 end;
end;

procedure TpvScene3DDynamicGroupManager.ReleaseInstance(const aInstance:TpvScene3D.TGroup.TInstance);
var PoolEntry:TInstancePoolEntry;
    GroupInfo:TGroupInfo;
    FreeListThreshold:TpvSizeInt;
begin

 if not assigned(aInstance) then begin
  exit;
 end;
 
 fLock.Acquire;
 try

  // Use reverse lookup map for O(1) access
  if fInstanceToPoolEntry.TryGet(aInstance,PoolEntry) then begin

   dec(PoolEntry.fReferenceCount);
   if PoolEntry.fReferenceCount<=0 then begin

    // No more references, deactivate but keep in pool
    PoolEntry.fActive:=false;
    PoolEntry.fReferenceCount:=0;
    fActiveInstances.Delete(PoolEntry.fStateKey);
    dec(fActiveInstancesCount);

    // Check if this group is in dynamic mode (MaxInstances < 0)
    if fGroupInfos.TryGet(PoolEntry.fGroupKey,GroupInfo) then begin

     if GroupInfo.fMaxInstances<0 then begin
      // Dynamic mode: free instance if free list is too large
      // Threshold: keep max 25% of current pool size as free
      FreeListThreshold:=Max(16,fInstancePool.Count div 4);

      if fInstanceFreeList.Count>FreeListThreshold then begin

       // Free this instance
       fInstanceToPoolEntry.Delete(PoolEntry.fInstance);
       FreeAndNil(PoolEntry.fInstance);
       // Don't add to free list, instance slot is now empty

      end else begin

       // Keep in free list for reuse
       fInstanceFreeList.Push(PoolEntry);

      end;

     end else begin
      // Fixed pool mode: always keep for reuse
      fInstanceFreeList.Push(PoolEntry);
     end;

    end;

   end;

  end;

 finally
  fLock.Release;
 end;

end;

function TpvScene3DDynamicGroupManager.AcquireRenderInstance(const aInstance:TpvScene3D.TGroup.TInstance):TpvScene3D.TGroup.TInstance.TRenderInstance;
var PoolEntry:TRenderInstancePoolEntry;
begin

 result:=nil;

 if not assigned(aInstance) then begin
  exit;
 end;
 
 fLock.Acquire;
 try

  PoolEntry:=AllocateRenderInstancePoolEntry;
  
  if assigned(PoolEntry.fRenderInstance) then begin

   // Reuse existing render instance
   result:=PoolEntry.fRenderInstance;

  end else begin

   // Create new render instance
   result:=aInstance.CreateRenderInstance;
   PoolEntry.fRenderInstance:=result;

  end;
  
  PoolEntry.fOwningInstance:=aInstance;
  PoolEntry.fActive:=true;
  
  fRenderInstanceToPoolEntry.Add(result,PoolEntry);

 finally
  fLock.Release;
 end;
end;

procedure TpvScene3DDynamicGroupManager.ReleaseRenderInstance(const aRenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance);
var PoolEntry:TRenderInstancePoolEntry;
begin

 if not assigned(aRenderInstance) then begin
  exit;
 end;

 fLock.Acquire;
 try

  // Use reverse lookup map for O(1) access
  if fRenderInstanceToPoolEntry.TryGet(aRenderInstance,PoolEntry) then begin

   PoolEntry.fActive:=false;
   PoolEntry.fOwningInstance:=nil;
   
   fRenderInstanceToPoolEntry.Delete(aRenderInstance);
   fRenderInstanceFreeList.Push(PoolEntry);

  end;

 finally
  fLock.Release;
 end;

end;

procedure TpvScene3DDynamicGroupManager.BeginFrame;
begin
 fLock.Acquire;
 try
  inc(fCurrentFrameIndex);
 finally
  fLock.Release;
 end;
end;

function TpvScene3DDynamicGroupManager.GetInstancePoolSize:TpvSizeInt;
begin
 fLock.Acquire;
 try
  result:=fInstancePool.Count;
 finally
  fLock.Release;
 end;
end;

function TpvScene3DDynamicGroupManager.GetActiveInstanceCount:TpvSizeInt;
begin
 fLock.Acquire;
 try
  result:=fActiveInstancesCount;
 finally
  fLock.Release;
 end;
end;

function TpvScene3DDynamicGroupManager.GetRenderInstancePoolSize:TpvSizeInt;
begin
 fLock.Acquire;
 try
  result:=fRenderInstancePool.Count;
 finally
  fLock.Release;
 end;
end;

end.
