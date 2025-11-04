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
 unit PasVulkan.Collections.TimedQueue;
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
     Math,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math;

type { TpvTimedQueue }
     TpvTimedQueue<T>=class
      public
       const K=4; // 4-ary heap
             StateEmpty=0;
             StateUsed=1;
             StateDeleted=2;
       type THandle=TpvUInt64;
            TData=T;
            TNode=record
             Time:TpvDouble;              // Time in seconds
             Sequence:TpvUInt64;          // Stable tiebreaker
             Handle:THandle;              // Handle
             Data:TData;                  // User payload
             Dead:Boolean;                // Optional for lazy cancel
            end;
            PNode=^TNode;
            TNodeArray=array of TNode;
            TIndexArray=array of TpvSizeInt;
            TMapEntry=record
             Key:THandle;
             Value:TpvSizeInt;
             State:TpvUInt8;
            end;
            PMapEntry=^TMapEntry;
            TMapEntryArray=array of TMapEntry;
            TTraversalMethod=procedure(const aData:TData) of object;
      private

       // Nodes storage (flat array, never moved)
       fNodes:TNodeArray;
       fNodeCount:TpvSizeInt;
       
       // Heap of indices into fNodes (only indices are moved during heap operations)
       fHeap:TIndexArray;
       fHeapPosition:TIndexArray;
       fCount:TpvSizeInt;
       fSequenceCounter:TpvUInt64;
       fNextHandle:THandle;

       // Freelist of reusable node indices (stack)
       fFreeList:TIndexArray;
       fFreeTop:TpvSizeInt;

       // Handle->Index open-addressing map (AoS)
       fMap:TMapEntryArray;
       fMapSize:TpvSizeInt;       // Capacity (power of two)
       fMapUsed:TpvSizeInt;       // Used slots
       fMapTombstones:TpvSizeInt; // Deleted (tombstone) slots since last rehash
       fMapSlotMask:TpvSizeInt;   // Mask (fMapSize-1)

       // Map methods
       class function MapHash(const aHandle:THandle):TpvUInt64; static; inline;
       procedure MapInit(const aCapacityPowerOfTwo:TpvSizeInt); inline;
       procedure MapRehash(const aNewPowerOfTwo:TpvSizeInt);
       procedure MapEnsure; inline;
       procedure MapPut(const aHandle:THandle;const aIndex:TpvSizeInt); inline;
       function MapTryGet(const aHandle:THandle;out aIndex:TpvSizeInt):Boolean; inline;
       procedure MapDelete(const aHandle:THandle); inline;
       
       // Heap methods
       procedure EnsureCapacity(const aNeed:TpvSizeInt); inline;
       function Less(const aIndexA,aIndexB:TpvSizeInt):Boolean; inline;
       procedure Swap(const aIndexA,aIndexB:TpvSizeInt); inline;
       procedure SiftUp(aIndex:TpvSizeInt); inline;
       procedure SiftDown(aIndex:TpvSizeInt); inline;
       procedure RemoveAt(aIndex:TpvSizeInt); inline;

      public

       constructor Create(const aInitialCapacity:TpvSizeInt=16;const aMapCapacityPowerOfTwo:TpvSizeInt=16);
       destructor Destroy; override;
      
       procedure Clear;
      
       function Push(const aTime:TpvDouble;const aData:TData):THandle; inline;
      
       function Cancel(const aHandle:THandle):Boolean; inline;     // eager remove
       function MarkCancel(const aHandle:THandle):Boolean; inline; // lazy mark
      
       function PeekEarliestNode(out aNode:TNode):Boolean; inline;
       function PopEarliestNode(out aNode:TNode):Boolean; inline;

       function PeekEarliest(out aData:TData):Boolean; inline;
       function PopEarliest(out aData:TData):Boolean; inline;

       procedure Traverse(const aTraversalMethod:TTraversalMethod); inline;

       procedure ShiftByTime(const aDeltaTime:TpvDouble); inline;

      published

       property Count:TpvSizeInt read fCount;

     end;

implementation

// === Map ===============================================================

class function TpvTimedQueue<T>.MapHash(const aHandle:THandle):TpvUInt64; 
const Multiplier=TpvUInt64($9e3779b97f4a7c15); // 64-bit golden ratio
var Value:TpvUInt64;
begin
 Value:=aHandle*Multiplier;
 result:=Value xor (Value shr 33);
end;

procedure TpvTimedQueue<T>.MapInit(const aCapacityPowerOfTwo:TpvSizeInt);
var Index,Capacity:TpvSizeInt;
begin
 if aCapacityPowerOfTwo<4 then begin
  Capacity:=1 shl 4;
 end else begin
  Capacity:=1 shl aCapacityPowerOfTwo;
 end;
 SetLength(fMap,Capacity);
 for Index:=0 to length(fMap)-1 do begin
  fMap[Index].State:=StateEmpty;
 end;
 fMapSize:=Capacity;
 fMapSlotMask:=Capacity-1;
 fMapUsed:=0;
 fMapTombstones:=0;
end;

procedure TpvTimedQueue<T>.MapRehash(const aNewPowerOfTwo:TpvSizeInt);
var OldMap:TMapEntryArray;
    Index:TpvSizeInt;
begin
 OldMap:=fMap;
 fMap:=nil; // Dereference old map, so that OldMap keeps the reference without conflict with MapInit and MapPut
 MapInit(aNewPowerOfTwo);
 for Index:=0 to length(OldMap)-1 do begin
  if OldMap[Index].State=StateUsed then begin
   MapPut(OldMap[Index].Key,OldMap[Index].Value);
  end;
 end;
end;

procedure TpvTimedQueue<T>.MapEnsure;
begin
 if (fMapSize>0) and (((fMapUsed+fMapTombstones)*10)>=(fMapSize*7)) then begin
  MapRehash(BSRQWord(RoundUpToPowerOfTwo64(fMapSize))+1);
 end;
end;

procedure TpvTimedQueue<T>.MapPut(const aHandle:THandle;const aIndex:TpvSizeInt);
var Position,FirstDeleted:TpvSizeInt;
    HashKey:TpvUInt64;
    MapEntry:PMapEntry;
begin
 if fMapSize=0 then begin
  MapInit(16);
 end;
 MapEnsure;
 HashKey:=MapHash(aHandle);
 Position:=TpvSizeInt(HashKey and TpvUInt64(fMapSlotMask));
 FirstDeleted:=-1;
 while true do begin
  MapEntry:=@fMap[Position];
  if MapEntry^.State=StateEmpty then begin
   if FirstDeleted>=0 then begin
    Position:=FirstDeleted;
    MapEntry:=@fMap[Position];
    dec(fMapTombstones);
   end else begin
    inc(fMapUsed);
   end;
   MapEntry^.Key:=aHandle;
   MapEntry^.Value:=aIndex;
   MapEntry^.State:=StateUsed;
   exit;
  end else begin
   if MapEntry^.State=StateDeleted then begin
    if FirstDeleted<0 then begin
     FirstDeleted:=Position;
    end;
   end else if MapEntry^.Key=aHandle then begin
    MapEntry^.Value:=aIndex;
    exit;
   end;
   Position:=(Position+1) and fMapSlotMask;
  end;
 end;
end;

function TpvTimedQueue<T>.MapTryGet(const aHandle:THandle;out aIndex:TpvSizeInt):Boolean;
var Position:TpvSizeInt;
    HashKey:TpvUInt64;
    MapEntry:PMapEntry;
begin
 result:=false;
 if fMapSize>0 then begin
  HashKey:=MapHash(aHandle);
  Position:=TpvSizeInt(HashKey and TpvUInt64(fMapSlotMask));
  while true do begin
   MapEntry:=@fMap[Position];
   if MapEntry^.State=StateEmpty then begin
    exit;
   end else begin
    if (MapEntry^.State=StateUsed) and (MapEntry^.Key=aHandle) then begin
     aIndex:=MapEntry^.Value;
     result:=true;
     exit;
    end else begin 
     Position:=(Position+1) and fMapSlotMask;
    end; 
   end; 
  end;
 end;
end;

procedure TpvTimedQueue<T>.MapDelete(const aHandle:THandle); 
var Position:TpvSizeInt;
    HashKey:TpvUInt64;
    MapEntry:PMapEntry;
begin
 if fMapSize>0 then begin
  HashKey:=MapHash(aHandle);
  Position:=TpvSizeInt(HashKey and TpvUInt64(fMapSlotMask));
  while true do begin
   MapEntry:=@fMap[Position];
   if MapEntry^.State=StateEmpty then begin
    exit; // Not found
   end else begin
    if (MapEntry^.State=StateUsed) and (MapEntry^.Key=aHandle) then begin
     MapEntry^.State:=StateDeleted; // Tombstone
     inc(fMapTombstones);
     exit;
    end else begin 
     Position:=(Position+1) and fMapSlotMask;
    end; 
   end; 
  end;
 end;
end; 

// === Heap ============================================================

procedure TpvTimedQueue<T>.EnsureCapacity(const aNeed:TpvSizeInt);
var NewCapacity,OldCapacity:TpvSizeInt;
begin
 if length(fNodes)<aNeed then begin
  OldCapacity:=length(fNodes);
  NewCapacity:=OldCapacity;
  if NewCapacity=0 then begin
   NewCapacity:=16;
  end;
  NewCapacity:=RoundUpToPowerOfTwo64(aNeed);
  SetLength(fNodes,NewCapacity);
  SetLength(fHeap,NewCapacity);
  SetLength(fHeapPosition,NewCapacity);
  SetLength(fFreeList,NewCapacity);
  if NewCapacity>OldCapacity then begin
   FillChar(fHeapPosition[OldCapacity],(NewCapacity-OldCapacity)*SizeOf(TpvSizeInt),$ff); // Initialize to -1
  end;
 end;
end;

function TpvTimedQueue<T>.Less(const aIndexA,aIndexB:TpvSizeInt):Boolean;
var NodeA,NodeB:PNode;
begin
 NodeA:=@fNodes[fHeap[aIndexA]];
 NodeB:=@fNodes[fHeap[aIndexB]];
 if NodeA^.Time<>NodeB^.Time then begin
  result:=NodeA^.Time<NodeB^.Time;
 end else begin
  result:=NodeA^.Sequence<NodeB^.Sequence;
 end;
end;

procedure TpvTimedQueue<T>.Swap(const aIndexA,aIndexB:TpvSizeInt);
var TempIndex:TpvSizeInt;
begin
 TempIndex:=fHeap[aIndexA];
 fHeap[aIndexA]:=fHeap[aIndexB];
 fHeap[aIndexB]:=TempIndex;
 // keep O(1) cancel mapping valid
 fHeapPosition[fHeap[aIndexA]]:=aIndexA;
 fHeapPosition[fHeap[aIndexB]]:=aIndexB;
end;

procedure TpvTimedQueue<T>.SiftUp(aIndex:TpvSizeInt);
var ParentIndex:TpvSizeInt;
begin
 while aIndex>0 do begin
  ParentIndex:=(aIndex-1) div K;
  if Less(aIndex,ParentIndex) then begin
   Swap(aIndex,ParentIndex);
   aIndex:=ParentIndex;
  end else begin 
   break;
  end;
 end;
end;

procedure TpvTimedQueue<T>.SiftDown(aIndex:TpvSizeInt);
var Child1,Child2,Child3,Child4,MinimumIndex:TpvSizeInt;
begin
 while true do begin
  Child1:=(aIndex*K)+1;
  if Child1>=fCount then begin
   break;
  end else begin
   MinimumIndex:=Child1;
   Child2:=Child1+1;
   if (Child2<fCount) and Less(Child2,MinimumIndex) then begin
    MinimumIndex:=Child2;
   end;
   Child3:=Child1+2;
   if (Child3<fCount) and Less(Child3,MinimumIndex) then begin
    MinimumIndex:=Child3;
   end;
   Child4:=Child1+3;
   if (Child4<fCount) and Less(Child4,MinimumIndex) then begin
    MinimumIndex:=Child4;
   end;
   if MinimumIndex=aIndex then begin
    break;
   end else begin
    Swap(aIndex,MinimumIndex);
    aIndex:=MinimumIndex;
   end;
  end; 
 end;
end;

procedure TpvTimedQueue<T>.RemoveAt(aIndex:TpvSizeInt);
var LastIndex,NodeIndex:TpvSizeInt;
begin
 
 LastIndex:=fCount-1;
 NodeIndex:=fHeap[aIndex];
 
 MapDelete(fNodes[NodeIndex].Handle);
 
 if aIndex<>LastIndex then begin
  fHeap[aIndex]:=fHeap[LastIndex];
  fHeapPosition[fHeap[aIndex]]:=aIndex;
  dec(fCount);
  SiftDown(aIndex);
  SiftUp(aIndex);
 end else begin
  dec(fCount);
 end;
 
 // Return node slot to freelist
 fHeapPosition[NodeIndex]:=-1;
 fNodes[NodeIndex].Dead:=false; // Clear tombstone mark for reuse
 if length(fFreeList)<=fFreeTop then begin
  SetLength(fFreeList,length(fFreeList)+((length(fFreeList)+16) shr 1));
 end;
 fFreeList[fFreeTop]:=NodeIndex;
 inc(fFreeTop);

end;

// === Public ==========================================================

constructor TpvTimedQueue<T>.Create(const aInitialCapacity:TpvSizeInt;const aMapCapacityPowerOfTwo:TpvSizeInt);
var InitialCapacity:TpvSizeInt;
begin
 inherited Create;
 InitialCapacity:=aInitialCapacity;
 if InitialCapacity<16 then begin
  InitialCapacity:=16;
 end;
 fNodes:=nil;
 fHeap:=nil;
 fHeapPosition:=nil;
 fFreeList:=nil;
 SetLength(fNodes,InitialCapacity);
 SetLength(fHeap,InitialCapacity);
 SetLength(fHeapPosition,InitialCapacity);
 SetLength(fFreeList,InitialCapacity);
 FillChar(fHeapPosition[0],InitialCapacity*SizeOf(TpvSizeInt),$ff); // Initialize to -1
 fNodeCount:=0;
 fCount:=0;
 fSequenceCounter:=0;
 fNextHandle:=0;
 fFreeTop:=0;
 fMap:=nil;
 MapInit(aMapCapacityPowerOfTwo);
end;

destructor TpvTimedQueue<T>.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TpvTimedQueue<T>.Clear;
begin
 fNodes:=nil;
 fHeap:=nil;
 fHeapPosition:=nil;
 fFreeList:=nil;
 fNodeCount:=0;
 fCount:=0;
 fSequenceCounter:=0;
 fNextHandle:=0;
 fFreeTop:=0;
 fMap:=nil;
 fMapSize:=0;
 fMapUsed:=0;
 fMapTombstones:=0;
 fMapSlotMask:=0; 
end;

function TpvTimedQueue<T>.Push(const aTime:TpvDouble;const aData:TData):THandle;
var HeapIndex,NodeIndex:TpvSizeInt;
    Node:PNode;
begin

 // Reuse a freed node slot if available
 if fFreeTop>0 then begin
  dec(fFreeTop);
  NodeIndex:=fFreeList[fFreeTop];
 end else begin
  EnsureCapacity(fNodeCount+1);
  NodeIndex:=fNodeCount;
  inc(fNodeCount);
 end;

 // Get new handle
 inc(fNextHandle);
 result:=fNextHandle;

 // Initialize node
 Node:=@fNodes[NodeIndex];
 Node^.Time:=aTime;
 Node^.Sequence:=fSequenceCounter;
 inc(fSequenceCounter);
 Node^.Handle:=result;
 Node^.Data:=aData;
 Node^.Dead:=false;

 // Insert into map
 MapPut(result,NodeIndex);

 // Insert into heap
 HeapIndex:=fCount;
 fHeap[HeapIndex]:=NodeIndex;
 fHeapPosition[NodeIndex]:=HeapIndex;
 inc(fCount);
 SiftUp(HeapIndex);

end;

function TpvTimedQueue<T>.Cancel(const aHandle:THandle):Boolean;
var NodeIndex,HeapIndex:TpvSizeInt;
begin
 if MapTryGet(aHandle,NodeIndex) then begin
  HeapIndex:=fHeapPosition[NodeIndex];
  result:=(HeapIndex>=0) and (HeapIndex<fCount) and (fHeap[HeapIndex]=NodeIndex);
  if result then begin
   RemoveAt(HeapIndex);
  end;
 end else begin
  result:=false;
 end;
end;

function TpvTimedQueue<T>.MarkCancel(const aHandle:THandle):Boolean;
var NodeIndex:TpvSizeInt;
begin
 if MapTryGet(aHandle,NodeIndex) then begin
  fNodes[NodeIndex].Dead:=true;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TpvTimedQueue<T>.PeekEarliestNode(out aNode:TNode):Boolean;
var Node:PNode;
begin
 while (fCount>0) and fNodes[fHeap[0]].Dead do begin
  RemoveAt(0);
 end;
 result:=fCount>0;
 if result then begin
  Node:=@fNodes[fHeap[0]];
  aNode:=Node^;
 end;
end;

function TpvTimedQueue<T>.PopEarliestNode(out aNode:TNode):Boolean;
var Node:PNode;
begin
 while fCount>0 do begin
  Node:=@fNodes[fHeap[0]];
  if Node^.Dead then begin
   RemoveAt(0);
  end else begin
   aNode:=Node^;
   RemoveAt(0);
   result:=true;
   exit;
  end;
 end;
 result:=false;
end;

function TpvTimedQueue<T>.PeekEarliest(out aData:TData):Boolean;
var Node:PNode;
begin
 while (fCount>0) and fNodes[fHeap[0]].Dead do begin
  RemoveAt(0);
 end;
 result:=fCount>0;
 if result then begin
  Node:=@fNodes[fHeap[0]];
  aData:=Node^.Data;
 end;
end;

function TpvTimedQueue<T>.PopEarliest(out aData:TData):Boolean;
var Node:PNode;
begin
 while fCount>0 do begin
  Node:=@fNodes[fHeap[0]];
  if Node^.Dead then begin
   RemoveAt(0);
  end else begin
   aData:=Node^.Data;
   RemoveAt(0);
   result:=true;
   exit;
  end;
 end;
 result:=false;
end; 

procedure TpvTimedQueue<T>.Traverse(const aTraversalMethod:TTraversalMethod);
var Index,NodeIndex:TpvSizeInt;
    Node:PNode;
begin
 for Index:=0 to fCount-1 do begin
  NodeIndex:=fHeap[Index];
  Node:=@fNodes[NodeIndex];
  if not Node^.Dead then begin
   aTraversalMethod(Node^.Data);
  end;
 end;
end;  

procedure TpvTimedQueue<T>.ShiftByTime(const aDeltaTime:TpvDouble);
var Index:TpvSizeInt;
    Node:PNode;
begin
 
 if fCount>0 then begin

  // Adjust all times by the delta time 
  for Index:=0 to fCount-1 do begin
   Node:=@fNodes[fHeap[Index]];
   Node^.Time:=Node^.Time-aDeltaTime; 
  end;
 
  // Remove all nodes that are now in the past (time < 0.0) via lazy marking,
  // then clean them from the heap top until the earliest is in the future.
  // We use lazy marking first to avoid O(n log n) heap removals.
  for Index:=0 to fCount-1 do begin
   Node:=@fNodes[fHeap[Index]];
   if (not Node^.Dead) and (Node^.Time<0.0) then begin
    Node^.Dead:=true;
   end;
  end;

  // Now clean the heap by removing all dead nodes at the top
  while (fCount>0) and fNodes[fHeap[0]].Dead do begin
   RemoveAt(0);
  end;

 end;

end; 

end.