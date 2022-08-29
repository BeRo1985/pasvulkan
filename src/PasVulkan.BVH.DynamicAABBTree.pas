(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2021, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.BVH.DynamicAABBTree;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

{$ifdef Delphi2009AndUp}
 {$warn DUPLICATE_CTOR_DTOR off}
{$endif}

{$undef UseDouble}
{$ifdef UseDouble}
 {$define NonSIMD}
{$endif}

{-$define NonSIMD}

{$ifdef NonSIMD}
 {$undef SIMD}
{$else}
 {$ifdef cpu386}
  {$if not (defined(Darwin) or defined(CompileForWithPIC))}
   {$define SIMD}
  {$ifend}
 {$endif}
 {$ifdef cpux64}
  {$define SIMD}
 {$endif}
{$endif}

{$define NonRecursive}

{$ifndef fpc}
 {$scopedenums on}
{$endif}

{$warnings off}

interface

uses SysUtils,
     Classes,
     Math,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections;

type { TpvBVHDynamicAABBTree }
     TpvBVHDynamicAABBTree=class
      public
       const NULLNODE=-1;
             AABBMULTIPLIER=2.0;
             ThresholdAABBVector:TpvVector3=(x:AABBEPSILON;y:AABBEPSILON;z:AABBEPSILON);
       type TTreeNode=record
             public
              AABB:TpvAABB;
              UserData:TpvPtrInt;
              Children:array[0..1] of TpvSizeInt;
              Height:TpvSizeInt;
              case boolean of
               false:(
                Parent:TpvSizeInt;
               );
               true:(
                Next:TpvSizeInt;
               );
            end;
            PTreeNode=^TTreeNode;
            TTreeNodes=array of TTreeNode;
            TState=record
             TreeNodes:TTreeNodes;
             Root:TpvSizeInt;
            end;
            PState=^TState;
            TUserDataArray=array of TpvPtrInt;
            TSizeIntArray=array[0..65535] of TpvSizeInt;
            PSizeIntArray=^TSizeIntArray;
            TSkipListNode=packed record // <= GPU-compatible with 32 bytes per node
             public
              // (u)vec4 aabbMinSkipCount
              AABBMin:TpvVector3;
              SkipCount:TpvUInt32;
              // (u)vec4 aabbMaxUserData
              AABBMax:TpvVector3;
              UserData:TpvUInt32;
            end;
            PSkipListNode=^TSkipListNode;
            TSkipListNodes=array of TSkipListNode;
            TSkipListNodeArray=TpvDynamicArray<TSkipListNode>;
            TSkipListNodeMap=array of TpvSizeUInt;
            TSkipListNodeStackItem=record
             Pass:TpvSizeInt;
             Node:TpvSizeInt;
            end;
            PSkipListNodeStackItem=^TSkipListNodeStackItem;
            TSkipListNodeStack=TpvDynamicStack<TSkipListNodeStackItem>;
            TGetUserDataIndex=function(const aUserData:TpvPtrInt):TpvUInt32 of object;
            TRayCastUserData=function(const aUserData:TpvPtrInt;const aRayOrigin,aRayDirection:TpvVector3;out aTime:TpvFloat;out aStop:boolean):boolean of object;
            { TSkipList }
            TSkipList=class
             private
              fNodeArray:TSkipListNodeArray;
             public
              constructor Create(const aFrom:TpvBVHDynamicAABBTree;const aGetUserDataIndex:TpvBVHDynamicAABBTree.TGetUserDataIndex); reintroduce;
              destructor Destroy; override;
              function IntersectionQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray;
              function ContainQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray; overload;
              function ContainQuery(const aPoint:TpvVector3):TpvBVHDynamicAABBTree.TUserDataArray; overload;
              function RayCast(const aRayOrigin,aRayDirection:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
              function RayCastLine(const aFrom,aTo:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
             public
              property NodeArray:TSkipListNodeArray read fNodeArray;
            end;
      private
       fSkipListNodeLock:TPasMPSpinLock;
       fSkipListNodeMap:TSkipListNodeMap;
       fSkipListNodeStack:TSkipListNodeStack;
      public
       Root:TpvSizeInt;
       Nodes:TTreeNodes;
       NodeCount:TpvSizeInt;
       NodeCapacity:TpvSizeInt;
       FreeList:TpvSizeInt;
       Path:TpvSizeUInt;
       InsertionCount:TpvSizeInt;
       constructor Create;
       destructor Destroy; override;
       function AllocateNode:TpvSizeInt;
       procedure FreeNode(const aNodeID:TpvSizeInt);
       function Balance(const aNodeID:TpvSizeInt):TpvSizeInt;
       procedure InsertLeaf(const aLeaf:TpvSizeInt);
       procedure RemoveLeaf(const aLeaf:TpvSizeInt);
       function CreateProxy(const aAABB:TpvAABB;const aUserData:TpvPtrInt):TpvSizeInt;
       procedure DestroyProxy(const aNodeID:TpvSizeInt);
       function MoveProxy(const aNodeID:TpvSizeInt;const aAABB:TpvAABB;const aDisplacement:TpvVector3):boolean;
       procedure Rebalance(const aIterations:TpvSizeInt);
       procedure RebuildBottomUp;
       procedure RebuildTopDown;
       procedure Rebuild;
       function ComputeHeight:TpvSizeInt;
       function GetHeight:TpvSizeInt;
       function GetAreaRatio:TpvDouble;
       function GetMaxBalance:TpvSizeInt;
       function ValidateStructure:boolean;
       function ValidateMetrics:boolean;
       function Validate:boolean;
       function IntersectionQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray;
       function ContainQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray; overload;
       function ContainQuery(const aPoint:TpvVector3):TpvBVHDynamicAABBTree.TUserDataArray; overload;
       function RayCast(const aRayOrigin,aRayDirection:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
       function RayCastLine(const aFrom,aTo:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
       procedure GetSkipListNodes(var aSkipListNodeArray:TSkipListNodeArray;const aGetUserDataIndex:TpvBVHDynamicAABBTree.TGetUserDataIndex);
     end;

implementation

{ TpvBVHDynamicAABBTree.TSkipList }

constructor TpvBVHDynamicAABBTree.TSkipList.Create(const aFrom:TpvBVHDynamicAABBTree;const aGetUserDataIndex:TpvBVHDynamicAABBTree.TGetUserDataIndex);
begin
 fNodeArray.Initialize;
 aFrom.GetSkipListNodes(fNodeArray,aGetUserDataIndex);
end;

destructor TpvBVHDynamicAABBTree.TSkipList.Destroy;
begin
 fNodeArray.Finalize;
 inherited Destroy;
end;

function TpvBVHDynamicAABBTree.TSkipList.IntersectionQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray;
var Index,Count:TpvSizeInt;
    Node:TpvBVHDynamicAABBTree.PSkipListNode;
begin
 result:=nil;
 Count:=fNodeArray.Count;
 if Count>0 then begin
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvAABB.Intersect(Node^.AABBMin,Node^.AABBMax,aAABB) then begin
    if Node^.UserData<>0 then begin
     result:=result+[Node^.UserData];
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.TSkipList.ContainQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray;
var Index,Count:TpvSizeInt;
    Node:TpvBVHDynamicAABBTree.PSkipListNode;
begin
 result:=nil;
 Count:=fNodeArray.Count;
 if Count>0 then begin
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvAABB.Contains(Node^.AABBMin,Node^.AABBMax,aAABB) then begin
    if Node^.UserData<>0 then begin
     result:=result+[Node^.UserData];
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.TSkipList.ContainQuery(const aPoint:TpvVector3):TpvBVHDynamicAABBTree.TUserDataArray;
var Index,Count:TpvSizeInt;
    Node:TpvBVHDynamicAABBTree.PSkipListNode;
begin
 result:=nil;
 Count:=fNodeArray.Count;
 if Count>0 then begin
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvAABB.Contains(Node^.AABBMin,Node^.AABBMax,aPoint) then begin
    if Node^.UserData<>0 then begin
     result:=result+[Node^.UserData];
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.TSkipList.RayCast(const aRayOrigin,aRayDirection:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
var Index,Count:TpvSizeInt;
    Node:TpvBVHDynamicAABBTree.PSkipListNode;
    RayEnd:TpvVector3;
    Time:TpvScalar;
    Stop:boolean;
begin
 result:=false;
 Count:=fNodeArray.Count;
 if assigned(aRayCastUserData) and (Count>0) then begin
  aTime:=Infinity;
  RayEnd:=aRayOrigin;
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if ((not result) and
       (TpvAABB.Contains(Node^.AABBMin,Node^.AABBMax,aRayOrigin) or
        TpvAABB.FastRayIntersection(Node^.AABBMin,Node^.AABBMax,aRayOrigin,aRayDirection))) or
      (result and TpvAABB.LineIntersection(Node^.AABBMin,Node^.AABBMax,aRayOrigin,RayEnd)) then begin
    if (Node^.UserData<>High(TpvUInt32)) and aRayCastUserData(Node^.UserData,aRayOrigin,aRayDirection,Time,Stop) then begin
     if (not result) or (Time<aTime) then begin
      aTime:=Time;
      aUserData:=Node^.UserData;
      result:=true;
      if aStopAtFirstHit or Stop then begin
       break;
      end else begin
       RayEnd:=aRayOrigin+(aRayDirection*Time);
      end;
     end;
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.TSkipList.RayCastLine(const aFrom,aTo:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
var Index,Count:TpvSizeInt;
    Node:TpvBVHDynamicAABBTree.PSkipListNode;
    Time,RayLength:TpvFloat;
    RayOrigin,RayDirection,RayEnd:TpvVector3;
    Stop:boolean;
begin
 result:=false;
 Count:=fNodeArray.Count;
 if assigned(aRayCastUserData) and (Count>0) then begin
  aTime:=Infinity;
  RayOrigin:=aFrom;
  RayEnd:=aTo;
  RayDirection:=(RayEnd-RayOrigin).Normalize;
  RayLength:=(RayEnd-RayOrigin).Length;
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvAABB.LineIntersection(Node^.AABBMin,Node^.AABBMax,RayOrigin,RayEnd) then begin
    if (Node^.UserData<>High(TpvUInt32)) and aRayCastUserData(Node^.UserData,RayOrigin,RayDirection,Time,Stop) then begin
     if ((Time>=0.0) and (Time<=RayLength)) and ((not result) or (Time<aTime)) then begin
      aTime:=Time;
      aUserData:=Node^.UserData;
      result:=true;
      if aStopAtFirstHit or Stop then begin
       break;
      end else begin
       RayEnd:=RayOrigin+(RayDirection*Time);
       RayLength:=Time;
      end;
     end;
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

{ TpvBVHDynamicAABBTree }

constructor TpvBVHDynamicAABBTree.Create;
var i:TpvSizeInt;
begin
 inherited Create;
 Root:=NULLNODE;
 Nodes:=nil;
 NodeCount:=0;
 NodeCapacity:=16;
 SetLength(Nodes,NodeCapacity);
 FillChar(Nodes[0],NodeCapacity*SizeOf(TTreeNode),#0);
 for i:=0 to NodeCapacity-2 do begin
  Nodes[i].Next:=i+1;
  Nodes[i].Height:=-1;
 end;
 Nodes[NodeCapacity-1].Next:=NULLNODE;
 Nodes[NodeCapacity-1].Height:=-1;
 FreeList:=0;
 Path:=0;
 InsertionCount:=0;
 fSkipListNodeLock:=TPasMPSpinLock.Create;
 fSkipListNodeMap:=nil;
 fSkipListNodeStack.Initialize;
end;

destructor TpvBVHDynamicAABBTree.Destroy;
begin
 fSkipListNodeStack.Finalize;
 fSkipListNodeMap:=nil;
 FreeAndNil(fSkipListNodeLock);
 Nodes:=nil;
 inherited Destroy;
end;

function TpvBVHDynamicAABBTree.AllocateNode:TpvSizeInt;
var Node:PTreeNode;
    i:TpvSizeInt;
begin
 if FreeList=NULLNODE then begin
  inc(NodeCapacity,(NodeCapacity+1) shr 1); // *1.5
  SetLength(Nodes,NodeCapacity);
  FillChar(Nodes[NodeCount],(NodeCapacity-NodeCount)*SizeOf(TTreeNode),#0);
  for i:=NodeCount to NodeCapacity-2 do begin
   Nodes[i].Next:=i+1;
   Nodes[i].Height:=-1;
  end;
  Nodes[NodeCapacity-1].Next:=NULLNODE;
  Nodes[NodeCapacity-1].Height:=-1;
  FreeList:=NodeCount;
 end;
 result:=FreeList;
 FreeList:=Nodes[result].Next;
 Node:=@Nodes[result];
 Node^.Parent:=NULLNODE;
 Node^.Children[0]:=NULLNODE;
 Node^.Children[1]:=NULLNODE;
 Node^.Height:=0;
 Node^.UserData:=0;
 inc(NodeCount);
end;

procedure TpvBVHDynamicAABBTree.FreeNode(const aNodeID:TpvSizeInt);
var Node:PTreeNode;
begin
 Node:=@Nodes[aNodeID];
 Node^.Next:=FreeList;
 Node^.Height:=-1;
 FreeList:=aNodeID;
 dec(NodeCount);
end;

function TpvBVHDynamicAABBTree.Balance(const aNodeID:TpvSizeInt):TpvSizeInt;
var NodeA,NodeB,NodeC,NodeD,NodeE,NodeF,NodeG:PTreeNode;
    NodeBID,NodeCID,NodeDID,NodeEID,NodeFID,NodeGID,NodeBalance:TpvSizeInt;
begin
 NodeA:=@Nodes[aNodeID];
 if (NodeA.Children[0]<0) or (NodeA^.Height<2) then begin
  result:=aNodeID;
 end else begin
  NodeBID:=NodeA.Children[0];
  NodeCID:=NodeA.Children[1];
  NodeB:=@Nodes[NodeBID];
  NodeC:=@Nodes[NodeCID];
  NodeBalance:=NodeC^.Height-NodeB^.Height;
  if NodeBalance>1 then begin
   NodeFID:=NodeC.Children[0];
   NodeGID:=NodeC.Children[1];
   NodeF:=@Nodes[NodeFID];
   NodeG:=@Nodes[NodeGID];
   NodeC^.Children[0]:=aNodeID;
   NodeC^.Parent:=NodeA^.Parent;
   NodeA^.Parent:=NodeCID;
   if NodeC.Parent>=0 then begin
    if Nodes[NodeC^.Parent].Children[0]=aNodeID then begin
     Nodes[NodeC^.Parent].Children[0]:=NodeCID;
    end else begin
     Nodes[NodeC^.Parent].Children[1]:=NodeCID;
    end;
   end else begin
    Root:=NodeCID;
   end;
   if NodeF^.Height>NodeG^.Height then begin
    NodeC^.Children[1]:=NodeFID;
    NodeA^.Children[1]:=NodeGID;
    NodeG^.Parent:=aNodeID;
    NodeA^.AABB:=NodeB^.AABB.Combine(NodeG^.AABB);
    NodeC^.AABB:=NodeA^.AABB.Combine(NodeF^.AABB);
    NodeA^.Height:=1+Max(NodeB^.Height,NodeG^.Height);
    NodeC^.Height:=1+Max(NodeA^.Height,NodeF^.Height);
   end else begin
    NodeC^.Children[1]:=NodeGID;
    NodeA^.Children[1]:=NodeFID;
    NodeF^.Parent:=aNodeID;
    NodeA^.AABB:=NodeB^.AABB.Combine(NodeF^.AABB);
    NodeC^.AABB:=NodeA^.AABB.Combine(NodeG^.AABB);
    NodeA^.Height:=1+Max(NodeB^.Height,NodeF^.Height);
    NodeC^.Height:=1+Max(NodeA^.Height,NodeG^.Height);
   end;
   result:=NodeCID;
  end else if NodeBalance<-1 then begin
   NodeDID:=NodeB^.Children[0];
   NodeEID:=NodeB^.Children[1];
   NodeD:=@Nodes[NodeDID];
   NodeE:=@Nodes[NodeEID];
   NodeB^.Children[0]:=aNodeID;
   NodeB^.Parent:=NodeA^.Parent;
   NodeA^.Parent:=NodeBID;
   if NodeB^.Parent>=0 then begin
    if Nodes[NodeB^.Parent].Children[0]=aNodeID then begin
     Nodes[NodeB^.Parent].Children[0]:=NodeBID;
    end else begin
     Nodes[NodeB^.Parent].Children[1]:=NodeBID;
    end;
   end else begin
    Root:=NodeBID;
   end;
   if NodeD^.Height>NodeE^.Height then begin
    NodeB^.Children[1]:=NodeDID;
    NodeA^.Children[0]:=NodeEID;
    NodeE^.Parent:=aNodeID;
    NodeA^.AABB:=NodeC^.AABB.Combine(NodeE^.AABB);
    NodeB^.AABB:=NodeA^.AABB.Combine(NodeD^.AABB);
    NodeA^.Height:=1+Max(NodeC^.Height,NodeE^.Height);
    NodeB^.Height:=1+Max(NodeA^.Height,NodeD^.Height);
   end else begin
    NodeB^.Children[1]:=NodeEID;
    NodeA^.Children[0]:=NodeDID;
    NodeD^.Parent:=aNodeID;
    NodeA^.AABB:=NodeC^.AABB.Combine(NodeD^.AABB);
    NodeB^.AABB:=NodeA^.AABB.Combine(NodeE^.AABB);
    NodeA^.Height:=1+Max(NodeC^.Height,NodeD^.Height);
    NodeB^.Height:=1+Max(NodeA^.Height,NodeE^.Height);
   end;
   result:=NodeBID;
  end else begin
   result:=aNodeID;
  end;
 end;
end;

procedure TpvBVHDynamicAABBTree.InsertLeaf(const aLeaf:TpvSizeInt);
var Node:PTreeNode;
    LeafAABB,CombinedAABB,AABB:TpvAABB;
    Index,Sibling,OldParent,NewParent:TpvSizeInt;
    Children:array[0..1] of TpvSizeInt;
    CombinedCost,Cost,InheritanceCost:TpvFloat;
    Costs:array[0..1] of TpvFloat;
begin
 inc(InsertionCount);
 if Root<0 then begin
  Root:=aLeaf;
  Nodes[aLeaf].Parent:=NULLNODE;
 end else begin
  LeafAABB:=Nodes[aLeaf].AABB;
  Index:=Root;
  while Nodes[Index].Children[0]>=0 do begin

   Children[0]:=Nodes[Index].Children[0];
   Children[1]:=Nodes[Index].Children[1];

   CombinedAABB:=Nodes[Index].AABB.Combine(LeafAABB);
   CombinedCost:=CombinedAABB.Cost;
   Cost:=CombinedCost*2.0;
   InheritanceCost:=2.0*(CombinedCost-Nodes[Index].AABB.Cost);

   AABB:=LeafAABB.Combine(Nodes[Children[0]].AABB);
   if Nodes[Children[0]].Children[0]<0 then begin
    Costs[0]:=AABB.Cost+InheritanceCost;
   end else begin
    Costs[0]:=(AABB.Cost-Nodes[Children[0]].AABB.Cost)+InheritanceCost;
   end;

   AABB:=LeafAABB.Combine(Nodes[Children[1]].AABB);
   if Nodes[Children[1]].Children[1]<0 then begin
    Costs[1]:=AABB.Cost+InheritanceCost;
   end else begin
    Costs[1]:=(AABB.Cost-Nodes[Children[1]].AABB.Cost)+InheritanceCost;
   end;

   if (Cost<Costs[0]) and (Cost<Costs[1]) then begin
    break;
   end else begin
    if Costs[0]<Costs[1] then begin
     Index:=Children[0];
    end else begin
     Index:=Children[1];
    end;
   end;

  end;

  Sibling:=Index;

  OldParent:=Nodes[Sibling].Parent;
  NewParent:=AllocateNode;
  Nodes[NewParent].Parent:=OldParent;
  Nodes[NewParent].UserData:=0;
  Nodes[NewParent].AABB:=LeafAABB.Combine(Nodes[Sibling].AABB);
  Nodes[NewParent].Height:=Nodes[Sibling].Height+1;

  if OldParent>=0 then begin
   if Nodes[OldParent].Children[0]=Sibling then begin
    Nodes[OldParent].Children[0]:=NewParent;
   end else begin
    Nodes[OldParent].Children[1]:=NewParent;
   end;
   Nodes[NewParent].Children[0]:=Sibling;
   Nodes[NewParent].Children[1]:=aLeaf;
   Nodes[Sibling].Parent:=NewParent;
   Nodes[aLeaf].Parent:=NewParent;
  end else begin
   Nodes[NewParent].Children[0]:=Sibling;
   Nodes[NewParent].Children[1]:=aLeaf;
   Nodes[Sibling].Parent:=NewParent;
   Nodes[aLeaf].Parent:=NewParent;
   Root:=NewParent;
  end;

  Index:=Nodes[aLeaf].Parent;
  while Index>=0 do begin
   Index:=Balance(Index);
   Node:=@Nodes[Index];
   Node^.AABB:=Nodes[Node^.Children[0]].AABB.Combine(Nodes[Node^.Children[1]].AABB);
   Node^.Height:=1+Max(Nodes[Node^.Children[0]].Height,Nodes[Node^.Children[1]].Height);
   Index:=Node^.Parent;
  end;

 end;
end;

procedure TpvBVHDynamicAABBTree.RemoveLeaf(const aLeaf:TpvSizeInt);
var Node:PTreeNode;
    Parent,GrandParent,Sibling,Index:TpvSizeInt;
begin
 if Root=aLeaf then begin
  Root:=NULLNODE;
 end else begin
  Parent:=Nodes[aLeaf].Parent;
  GrandParent:=Nodes[Parent].Parent;
  if Nodes[Parent].Children[0]=aLeaf then begin
   Sibling:=Nodes[Parent].Children[1];
  end else begin
   Sibling:=Nodes[Parent].Children[0];
  end;
  if GrandParent>=0 then begin
   if Nodes[GrandParent].Children[0]=Parent then begin
    Nodes[GrandParent].Children[0]:=Sibling;
   end else begin
    Nodes[GrandParent].Children[1]:=Sibling;
   end;
   Nodes[Sibling].Parent:=GrandParent;
   FreeNode(Parent);
   Index:=GrandParent;
   while Index>=0 do begin
    Index:=Balance(Index);
    Node:=@Nodes[Index];
    Node^.AABB:=Nodes[Node^.Children[0]].AABB.Combine(Nodes[Node^.Children[1]].AABB);
    Node^.Height:=1+Max(Nodes[Node^.Children[0]].Height,Nodes[Node^.Children[1]].Height);
    Index:=Node^.Parent;
   end;
  end else begin
   Root:=Sibling;
   Nodes[Sibling].Parent:=NULLNODE;
   FreeNode(Parent);
  end;
 end;
end;

function TpvBVHDynamicAABBTree.CreateProxy(const aAABB:TpvAABB;const aUserData:TpvPtrInt):TpvSizeInt;
var Node:PTreeNode;
begin
 result:=AllocateNode;
 Node:=@Nodes[result];
 Node^.AABB.Min:=aAABB.Min-ThresholdAABBVector;
 Node^.AABB.Max:=aAABB.Max+ThresholdAABBVector;
 Node^.UserData:=aUserData;
 Node^.Height:=0;
 InsertLeaf(result);
end;

procedure TpvBVHDynamicAABBTree.DestroyProxy(const aNodeID:TpvSizeInt);
begin
 RemoveLeaf(aNodeID);
 FreeNode(aNodeID);
end;

function TpvBVHDynamicAABBTree.MoveProxy(const aNodeID:TpvSizeInt;const aAABB:TpvAABB;const aDisplacement:TpvVector3):boolean;
var Node:PTreeNode;
    b:TpvAABB;
    d:TpvVector3;
begin
 Node:=@Nodes[aNodeID];
 result:=not Node^.AABB.Contains(aAABB);
 if result then begin
  RemoveLeaf(aNodeID);
  b.Min:=aAABB.Min-ThresholdAABBVector;
  b.Max:=aAABB.Max+ThresholdAABBVector;
  d:=aDisplacement*AABBMULTIPLIER;
  if d.x<0.0 then begin
   b.Min.x:=b.Min.x+d.x;
  end else if d.x>0.0 then begin
   b.Max.x:=b.Max.x+d.x;
  end;
  if d.y<0.0 then begin
   b.Min.y:=b.Min.y+d.y;
  end else if d.y>0.0 then begin
   b.Max.y:=b.Max.y+d.y;
  end;
  if d.z<0.0 then begin
   b.Min.z:=b.Min.z+d.z;
  end else if d.z>0.0 then begin
   b.Max.z:=b.Max.z+d.z;
  end;
  Node^.AABB:=b;
  InsertLeaf(aNodeID);
 end;
end;

procedure TpvBVHDynamicAABBTree.Rebalance(const aIterations:TpvSizeInt);
var Counter,Node:TpvSizeInt;
    Bit:TpvSizeUInt;
//  Children:PSizeIntArray;
begin
 if (Root>=0) and (Root<NodeCount) then begin
  for Counter:=1 to aIterations do begin
   Bit:=0;
   Node:=Root;
   while Nodes[Node].Children[0]>=0 do begin
    Node:=Nodes[Node].Children[(Path shr Bit) and 1];
    Bit:=(Bit+1) and 31;
   end;
   inc(Path);
   if ((Node>=0) and (Node<NodeCount)) and (Nodes[Node].Children[0]<0) then begin
    RemoveLeaf(Node);
    InsertLeaf(Node);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure TpvBVHDynamicAABBTree.RebuildBottomUp;
var Count,IndexA,IndexB,IndexAMin,IndexBMin,Index1,Index2,ParentIndex:TpvSizeint;
    NewNodes:array of TpvSizeInt;
    Children:array[0..1] of TpvBVHDynamicAABBTree.PTreeNode;
    Parent:TpvBVHDynamicAABBTree.PTreeNode;
    MinCost,Cost:TpvScalar;
    AABBa,AABBb:PpvAABB;
    AABB:TpvAABB;
    First:boolean;
begin
 if NodeCount>0 then begin
  NewNodes:=nil;
  try
   SetLength(NewNodes,NodeCount);
   FillChar(NewNodes[0],NodeCount*SizeOf(TpvSizeint),#0);
   Count:=0;
   for IndexA:=0 to NodeCapacity-1 do begin
    if Nodes[IndexA].Height>=0 then begin
     if Nodes[IndexA].Children[0]<0 then begin
      Nodes[IndexA].Parent:=TpvBVHDynamicAABBTree.NULLNODE;
      NewNodes[Count]:=IndexA;
      inc(Count);
     end else begin
      FreeNode(IndexA);
     end;
    end;
   end;
   while Count>1 do begin
    First:=true;
    MinCost:=MAX_SCALAR;
    IndexAMin:=-1;
    IndexBMin:=-1;
  {}/////////////////TOOPTIMIZE///////////////////
  {}for IndexA:=0 to Count-1 do begin           //
  {} AABBa:=@Nodes[NewNodes[IndexA]].AABB;      //
  {} for IndexB:=IndexA+1 to Count-1 do begin   //
  {}  AABBb:=@Nodes[NewNodes[IndexB]].AABB;     //
  {}  AABB:=AABBa^.Combine(AABBb^);             //
  {}  Cost:=AABB.Cost;                          //
  {}  if First or (Cost<MinCost) then begin     //
  {}   First:=false;                            //
  {}   MinCost:=Cost;                           //
  {}   IndexAMin:=IndexA;                       //
  {}   IndexBMin:=IndexB;                       //
  {}  end;                                      //
  {} end;                                       //
  {}end;                                        //
  {}/////////////////TOOPTIMIZE///////////////////
    Index1:=NewNodes[IndexAMin];
    Index2:=NewNodes[IndexBMin];
    Children[0]:=@Nodes[Index1];
    Children[1]:=@Nodes[Index2];
    ParentIndex:=AllocateNode;
    Parent:=@Nodes[ParentIndex];
    Parent^.Children[0]:=Index1;
    Parent^.Children[1]:=Index2;
    Parent^.Height:=1+Max(Children[0]^.Height,Children[1]^.Height);
    Parent^.AABB:=Children[0]^.AABB.Combine(Children[1]^.AABB);
    Parent^.Parent:=TpvBVHDynamicAABBTree.NULLNODE;
    Children[0]^.Parent:=ParentIndex;
    Children[1]^.Parent:=ParentIndex;
    NewNodes[IndexBMin]:=NewNodes[Count-1];
    NewNodes[IndexAMin]:=ParentIndex;
    dec(Count);
   end;
   Root:=NewNodes[0];
  finally
   NewNodes:=nil;
  end;
 end;
end;

procedure TpvBVHDynamicAABBTree.RebuildTopDown;
type TLeafNodes=array of TpvSizeInt;
     TFillStackItem=record
      Parent:TpvSizeInt;
      Which:TpvSizeInt;
      LeafNodes:TLeafNodes;
     end;
     TFillStack=TpvDynamicStack<TFillStackItem>;
     THeightStackItem=record
      Node:TpvSizeInt;
      Pass:TpvSizeInt;
     end;
     THeightStack=TpvDynamicStack<THeightStackItem>;
var Count,Index,MinPerSubTree,ParentIndex,NodeIndex,SplitAxis,TempIndex,
    LeftIndex,RightIndex,LeftCount,RightCount:TpvSizeint;
    LeafNodes:TLeafNodes;
    SplitValue:TpvScalar;
    AABB:TpvAABB;
    Center:TpvVector3;
    VarianceX,VarianceY,VarianceZ,MeanX,MeanY,MeanZ:Double;
    FillStack:TFillStack;
    FillStackItem,NewFillStackItem:TFillStackItem;
    HeightStack:THeightStack;
    HeightStackItem,NewHeightStackItem:THeightStackItem;
begin

 if NodeCount>0 then begin

  LeafNodes:=nil;
  try

   SetLength(LeafNodes,NodeCount);
   FillChar(LeafNodes[0],NodeCount*SizeOf(TpvSizeint),#0);

   Count:=0;
   for Index:=0 to NodeCapacity-1 do begin
    if Nodes[Index].Height>=0 then begin
     if Nodes[Index].Children[0]<0 then begin
      Nodes[Index].Parent:=TpvBVHDynamicAABBTree.NULLNODE;
      LeafNodes[Count]:=Index;
      inc(Count);
     end else begin
      FreeNode(Index);
     end;
    end;
   end;

   Root:=TpvBVHDynamicAABBTree.NULLNODE;

   if Count>0 then begin

    FillStack.Initialize;
    try

     NewFillStackItem.Parent:=TpvBVHDynamicAABBTree.NULLNODE;
     NewFillStackItem.Which:=-1;
     NewFillStackItem.LeafNodes:=copy(LeafNodes,0,Count);
     FillStack.Push(NewFillStackItem);

     while FillStack.Pop(FillStackItem) do begin

      case length(FillStackItem.LeafNodes) of

       0:begin
       end;

       1:begin
        NodeIndex:=FillStackItem.LeafNodes[0];
        ParentIndex:=FillStackItem.Parent;
        Nodes[NodeIndex].Parent:=ParentIndex;
        if (FillStackItem.Which>=0) and (ParentIndex>=0) then begin
         Nodes[ParentIndex].Children[FillStackItem.Which]:=NodeIndex;
        end else begin
         Root:=NodeIndex;
        end;
       end;
       else begin

        NodeIndex:=AllocateNode;

        ParentIndex:=FillStackItem.Parent;

        Nodes[NodeIndex].Parent:=ParentIndex;

        if (FillStackItem.Which>=0) and (ParentIndex>=0) then begin
         Nodes[ParentIndex].Children[FillStackItem.Which]:=NodeIndex;
        end else begin
         Root:=NodeIndex;
        end;

        AABB:=Nodes[FillStackItem.LeafNodes[0]].AABB;
        for Index:=1 to length(FillStackItem.LeafNodes)-1 do begin
         AABB:=AABB.Combine(Nodes[FillStackItem.LeafNodes[Index]].AABB);
        end;

        Nodes[NodeIndex].AABB:=AABB;

        MeanX:=0.0;
        MeanY:=0.0;
        MeanZ:=0.0;
        for Index:=0 to length(FillStackItem.LeafNodes)-1 do begin
         Center:=Nodes[FillStackItem.LeafNodes[Index]].AABB.Center;
         MeanX:=MeanX+Center.x;
         MeanY:=MeanY+Center.y;
         MeanZ:=MeanZ+Center.z;
        end;
        MeanX:=MeanX/length(FillStackItem.LeafNodes);
        MeanY:=MeanY/length(FillStackItem.LeafNodes);
        MeanZ:=MeanZ/length(FillStackItem.LeafNodes);

        VarianceX:=0.0;
        VarianceY:=0.0;
        VarianceZ:=0.0;
        for Index:=0 to length(FillStackItem.LeafNodes)-1 do begin
         Center:=Nodes[FillStackItem.LeafNodes[Index]].AABB.Center;
         VarianceX:=VarianceX+sqr(Center.x-MeanX);
         VarianceY:=VarianceY+sqr(Center.y-MeanY);
         VarianceZ:=VarianceZ+sqr(Center.z-MeanZ);
        end;
        VarianceX:=VarianceX/length(FillStackItem.LeafNodes);
        VarianceY:=VarianceY/length(FillStackItem.LeafNodes);
        VarianceZ:=VarianceZ/length(FillStackItem.LeafNodes);

        if VarianceX<VarianceY then begin
         if VarianceY<VarianceZ then begin
          SplitAxis:=2;
          SplitValue:=MeanZ;
         end else begin
          SplitAxis:=1;
          SplitValue:=MeanY;
         end;
        end else begin
         if VarianceX<VarianceZ then begin
          SplitAxis:=2;
          SplitValue:=MeanZ;
         end else begin
          SplitAxis:=0;
          SplitValue:=MeanX;
         end;
        end;

        LeftIndex:=0;
        RightIndex:=length(FillStackItem.LeafNodes);
        LeftCount:=0;
        RightCount:=0;
        while LeftIndex<RightIndex do begin
         Center:=Nodes[FillStackItem.LeafNodes[LeftIndex]].AABB.Center;
         if Center[SplitAxis]<=SplitValue then begin
          inc(LeftIndex);
          inc(LeftCount);
         end else begin
          dec(RightIndex);
          inc(RightCount);
          TempIndex:=FillStackItem.LeafNodes[LeftIndex];
          FillStackItem.LeafNodes[LeftIndex]:=FillStackItem.LeafNodes[RightIndex];
          FillStackItem.LeafNodes[RightIndex]:=TempIndex;
         end;
        end;

        MinPerSubTree:=(TpvInt64(length(FillStackItem.LeafNodes)+1)*341) shr 10;
        if (LeftCount=0) or
           (RightCount=0) or
           (LeftCount<=MinPerSubTree) or
           (RightCount<=MinPerSubTree) then begin
         RightIndex:=(length(FillStackItem.LeafNodes)+1) shr 1;
        end;

        begin
         NewFillStackItem.Parent:=NodeIndex;
         NewFillStackItem.Which:=1;
         NewFillStackItem.LeafNodes:=copy(FillStackItem.LeafNodes,RightIndex,length(FillStackItem.LeafNodes)-RightIndex);
         FillStack.Push(NewFillStackItem);
        end;

        begin
         NewFillStackItem.Parent:=NodeIndex;
         NewFillStackItem.Which:=0;
         NewFillStackItem.LeafNodes:=copy(FillStackItem.LeafNodes,0,RightIndex);
         FillStack.Push(NewFillStackItem);
        end;

        FillStackItem.LeafNodes:=nil;

       end;
      end;
     end;

    finally
     FillStack.Finalize;
    end;

    HeightStack.Initialize;
    try

     NewHeightStackItem.Node:=Root;
     NewHeightStackItem.Pass:=0;
     HeightStack.Push(NewHeightStackItem);

     while HeightStack.Pop(HeightStackItem) do begin
      case HeightStackItem.Pass of
       0:begin
        NewHeightStackItem.Node:=HeightStackItem.Node;
        NewHeightStackItem.Pass:=1;
        HeightStack.Push(NewHeightStackItem);
        if Nodes[HeightStackItem.Node].Children[1]>=0 then begin
         NewHeightStackItem.Node:=Nodes[HeightStackItem.Node].Children[1];
         NewHeightStackItem.Pass:=0;
         HeightStack.Push(NewHeightStackItem);
        end;
        if Nodes[HeightStackItem.Node].Children[0]>=0 then begin
         NewHeightStackItem.Node:=Nodes[HeightStackItem.Node].Children[0];
         NewHeightStackItem.Pass:=0;
         HeightStack.Push(NewHeightStackItem);
        end;
       end;
       1:begin
        if (Nodes[HeightStackItem.Node].Children[0]<0) and (Nodes[HeightStackItem.Node].Children[1]<0) then begin
         Nodes[HeightStackItem.Node].Height:=1;
        end else begin
         Nodes[HeightStackItem.Node].Height:=1+Max(Nodes[Nodes[HeightStackItem.Node].Children[0]].Height,Nodes[Nodes[HeightStackItem.Node].Children[1]].Height);
        end;
       end;
      end;
     end;

    finally
     HeightStack.Finalize;
    end;

   end;

  finally

   LeafNodes:=nil;

  end;

 end;

end;

procedure TpvBVHDynamicAABBTree.Rebuild;
begin
 if NodeCount<128 then begin
  RebuildBottomUp;
 end else begin
  RebuildTopDown;
 end;
end;

function TpvBVHDynamicAABBTree.ComputeHeight:TpvSizeInt;
type TStackItem=record
      NodeID:TpvSizeInt;
      Height:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=0;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   NewStackItem.Height:=1;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if result<StackItem.Height then begin
     result:=StackItem.Height;
    end;
    if Node^.Children[1]>=0 then begin
     NewStackItem.NodeID:=Node^.Children[1];
     NewStackItem.Height:=StackItem.Height+1;
     Stack.Push(NewStackItem);
    end;
    if Node^.Children[0]>=0 then begin
     NewStackItem.NodeID:=Node^.Children[0];
     NewStackItem.Height:=StackItem.Height+1;
     Stack.Push(NewStackItem);
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.GetHeight:TpvSizeInt;
begin
 if Root>=0 then begin
  result:=Nodes[Root].Height;
 end else begin
  result:=0;
 end;
end;

function TpvBVHDynamicAABBTree.GetAreaRatio:TpvDouble;
var NodeID:TpvSizeInt;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=0.0;
 if Root>=0 then begin
  for NodeID:=0 to NodeCount-1 do begin
   Node:=@Nodes[NodeID];
   if Node^.Height>=0 then begin
    result:=result+Node^.AABB.Cost;
   end;
  end;
  result:=result/Nodes[Root].AABB.Cost;
 end;
end;

function TpvBVHDynamicAABBTree.GetMaxBalance:TpvSizeInt;
var NodeID,Balance:TpvSizeInt;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=0;
 if Root>=0 then begin
  for NodeID:=0 to NodeCount-1 do begin
   Node:=@Nodes[NodeID];
   if (Node^.Height>1) and (Node^.Children[0]>=0) and (Node^.Children[1]>=0) then begin
    Balance:=abs(Nodes[Node^.Children[0]].Height-Nodes[Node^.Children[1]].Height);
    if result<Balance then begin
     result:=Balance;
    end;
   end;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.ValidateStructure:boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
      Parent:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=true;
 if (NodeCount>0) and (Root>=0) and (Root<NodeCount) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   NewStackItem.Parent:=TpvBVHDynamicAABBTree.NULLNODE;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.Parent<>StackItem.Parent then begin
     result:=false;
     break;
    end else begin
     if (Node^.Children[1]>=0) and (Node^.Children[1]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      NewStackItem.Parent:=StackItem.NodeID;
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) and (Node^.Children[0]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      NewStackItem.Parent:=StackItem.NodeID;
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.ValidateMetrics:boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=true;
 if (NodeCount>0) and (Root>=0) and (Root<NodeCount) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if (((Node^.Children[0]<0) or (Node^.Children[0]>=NodeCount)) or
        ((Node^.Children[1]<0) or (Node^.Children[1]>=NodeCount))) or
       (Node^.Height<>(1+Max(Nodes[Node^.Children[0]].Height,Nodes[Node^.Children[1]].Height))) then begin
     result:=false;
     break;
    end else begin
     if (Node^.Children[1]>=0) and (Node^.Children[1]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) and (Node^.Children[0]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.Validate:boolean;
var NodeID,FreeCount:TpvSizeInt;
begin
 result:=ValidateStructure;
 if result then begin
  result:=ValidateMetrics;
  if result then begin
   result:=ComputeHeight=GetHeight;
   if result then begin
    NodeID:=FreeList;
    FreeCount:=0;
    while NodeID>=0 do begin
     NodeID:=Nodes[NodeID].Next;
     inc(FreeCount);
    end;
    result:=(NodeCount+FreeCount)=NodeCapacity;
   end;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.IntersectionQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=nil;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.Intersect(aAABB) then begin
     if Node^.UserData<>0 then begin
      result:=result+[Node^.UserData];
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.ContainQuery(const aAABB:TpvAABB):TpvBVHDynamicAABBTree.TUserDataArray;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=nil;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.Contains(aAABB) then begin
     if Node^.UserData<>0 then begin
      result:=result+[Node^.UserData];
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.ContainQuery(const aPoint:TpvVector3):TpvBVHDynamicAABBTree.TUserDataArray;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
begin
 result:=nil;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.Contains(aPoint) then begin
     if Node^.UserData<>0 then begin
      result:=result+[Node^.UserData];
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.RayCast(const aRayOrigin,aRayDirection:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
    RayEnd:TpvVector3;
    Time:TpvFloat;
    Stop:boolean;
begin
 result:=false;
 if assigned(aRayCastUserData) and (NodeCount>0) and (Root>=0) then begin
  aTime:=Infinity;
  RayEnd:=aRayOrigin;
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if ((not result) and
        (Node^.AABB.Contains(aRayOrigin) or Node^.AABB.FastRayIntersection(aRayOrigin,aRayDirection))) or
       (result and Node^.AABB.LineIntersection(aRayOrigin,RayEnd)) then begin
     if (Node^.UserData<>0) and aRayCastUserData(Node^.UserData,aRayOrigin,aRayDirection,Time,Stop) then begin
      if (not result) or (Time<aTime) then begin
       aTime:=Time;
       aUserData:=Node^.UserData;
       result:=true;
       if aStopAtFirstHit or Stop then begin
        break;
       end else begin
        RayEnd:=aRayOrigin+(aRayDirection*Time);
       end;
      end;
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvBVHDynamicAABBTree.RayCastLine(const aFrom,aTo:TpvVector3;out aTime:TpvFloat;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvBVHDynamicAABBTree.TRayCastUserData):boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvBVHDynamicAABBTree.PTreeNode;
    Time,RayLength:TpvFloat;
    RayOrigin,RayDirection,RayEnd:TpvVector3;
    Stop:boolean;
begin
 result:=false;
 if assigned(aRayCastUserData) and (NodeCount>0) and (Root>=0) then begin
  aTime:=Infinity;
  RayOrigin:=aFrom;
  RayEnd:=aTo;
  RayDirection:=(RayEnd-RayOrigin).Normalize;
  RayLength:=(RayEnd-RayOrigin).Length;
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.LineIntersection(RayOrigin,RayEnd) then begin
     if (Node^.UserData<>0) and aRayCastUserData(Node^.UserData,RayOrigin,RayDirection,Time,Stop) then begin
      if ((Time>=0.0) and (Time<=RayLength)) and ((not result) or (Time<aTime)) then begin
       aTime:=Time;
       aUserData:=Node^.UserData;
       result:=true;
       if aStopAtFirstHit or Stop then begin
        break;
       end else begin
        RayEnd:=RayOrigin+(RayDirection*Time);
        RayLength:=Time;
       end;
      end;
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

procedure TpvBVHDynamicAABBTree.GetSkipListNodes(var aSkipListNodeArray:TSkipListNodeArray;const aGetUserDataIndex:TpvBVHDynamicAABBTree.TGetUserDataIndex);
//const ThresholdVector:TpvVector3=(x:1e-7;y:1e-7;z:1e-7);
var StackItem,NewStackItem:TSkipListNodeStackItem;
    Node:PTreeNode;
    SkipListNode:TSkipListNode;
    SkipListNodeIndex:TpvSizeInt;
begin
 fSkipListNodeLock.Acquire;
 try
  if Root>=0 then begin
   if length(fSkipListNodeMap)<length(Nodes) then begin
    SetLength(fSkipListNodeMap,(length(Nodes)*3) shr 1);
   end;
   aSkipListNodeArray.Count:=0;
   NewStackItem.Pass:=0;
   NewStackItem.Node:=Root;
   fSkipListNodeStack.Push(NewStackItem);
   while fSkipListNodeStack.Pop(StackItem) do begin
    case StackItem.Pass of
     0:begin
      if StackItem.Node>=0 then begin
       Node:=@Nodes[StackItem.Node];
       SkipListNode.AABBMin:=Node^.AABB.Min;
       SkipListNode.AABBMax:=Node^.AABB.Max;
       SkipListNode.SkipCount:=0;
       if Node^.UserData<>0 then begin
        if assigned(aGetUserDataIndex) then begin
         SkipListNode.UserData:=aGetUserDataIndex(Node^.UserData);
        end else begin
         SkipListNode.UserData:=Node^.UserData;
        end;
       end else begin
        SkipListNode.UserData:=High(TpvUInt32);
       end;
       SkipListNodeIndex:=aSkipListNodeArray.Add(SkipListNode);
       fSkipListNodeMap[StackItem.Node]:=SkipListNodeIndex;
       NewStackItem.Pass:=1;
       NewStackItem.Node:=StackItem.Node;
       fSkipListNodeStack.Push(NewStackItem);
       if Node^.Children[1]>=0 then begin
        NewStackItem.Pass:=0;
        NewStackItem.Node:=Node^.Children[1];
        fSkipListNodeStack.Push(NewStackItem);
       end;
       if Node^.Children[0]>=0 then begin
        NewStackItem.Pass:=0;
        NewStackItem.Node:=Node^.Children[0];
        fSkipListNodeStack.Push(NewStackItem);
       end;
      end;
     end;
     1:begin
      if StackItem.Node>=0 then begin
       SkipListNodeIndex:=fSkipListNodeMap[StackItem.Node];
       aSkipListNodeArray.Items[SkipListNodeIndex].SkipCount:=aSkipListNodeArray.Count-SkipListNodeIndex;
      end;
     end;
    end;
   end;
  end;
 finally
  fSkipListNodeLock.Release;
 end;
end;

end.

