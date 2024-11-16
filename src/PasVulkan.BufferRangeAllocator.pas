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
unit PasVulkan.BufferRangeAllocator;
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
     PasVulkan.Collections,
     PasVulkan.Types;

type { TpvBufferRangeAllocator }
     TpvBufferRangeAllocator=class
      public
       type TOnResize=procedure(const aSender:TpvBufferRangeAllocator;const aNewCapacity:TpvSizeInt) of object;
            PRange=^TRange;
            TRangeRedBlackTree=TpvUInt64RedBlackTree<PRange>;
            { TRange }
            TRange=record
             public
              Start:TpvSizeInt;
              Len:TpvSizeInt;
              Previous:PRange;
              Next:PRange;
              OffsetNode:TRangeRedBlackTree.TNode;
              SizeNode:TRangeRedBlackTree.TNode;
              procedure SwapWith(var aWith:TRange);
              class function CreateRange(const aStart,aLen:TpvSizeInt):TpvBufferRangeAllocator.PRange; static;
            end;
            { TRangeList }
            TRangeList=record
             public
              First:PRange;
              Last:PRange;
              procedure Initialize;
              procedure Finalize;
              procedure Insert(const aRange:PRange);
              procedure Remove(const aRange:PRange);
              procedure SortByOffsets;
            end;
      private
       fAllocatedRanges:TRangeList;
       fFreeRanges:TRangeList;
       fAllocatedOffsetRangeRedBlackTree:TRangeRedBlackTree;
       fFreeOffsetRangeRedBlackTree:TRangeRedBlackTree;
       fFreeSizeRangeRedBlackTree:TRangeRedBlackTree;
       fCapacity:TpvSizeInt;
       fOnResize:TOnResize;
       procedure MergeFreeRanges(const aRange:PRange);
      public
       constructor Create(const aCapacity:TpvSizeInt=0); reintroduce;
       destructor Destroy; override;
       function Allocate(const aSize:TpvSizeInt):TpvSizeInt;
       procedure Release(const aStart:TpvSizeInt;aSize:TpvSizeInt=-1);
      published
       property Capacity:TpvSizeInt read fCapacity;
       property OnResize:TOnResize read fOnResize write fOnResize; 
     end;


implementation

uses PasVulkan.Utils;

{ TpvBufferRangeAllocator.TRange }

procedure TpvBufferRangeAllocator.TRange.SwapWith(var aWith:TRange);
begin
 TpvSwap<TpvSizeInt>.Swap(Start,aWith.Start);
 TpvSwap<TpvSizeInt>.Swap(Len,aWith.Len);
end;

class function TpvBufferRangeAllocator.TRange.CreateRange(const aStart,aLen:TpvSizeInt):TpvBufferRangeAllocator.PRange;
begin
 GetMem(result,SizeOf(TpvBufferRangeAllocator.TRange));
 result^.Start:=aStart;
 result^.Len:=aLen;
 result^.Previous:=nil;
 result^.Next:=nil;
 result^.OffsetNode:=nil;
 result^.SizeNode:=nil;
end;

{ TpvBufferRangeAllocator.TRangeList }

procedure TpvBufferRangeAllocator.TRangeList.Initialize;
begin
 First:=nil;
 Last:=nil;
end;

procedure TpvBufferRangeAllocator.TRangeList.Finalize;
var Current,Next:PRange;
begin
 Current:=First;
 while assigned(Current) do begin
  Next:=Current^.Next;
  FreeMem(Current);
  Current:=Next;
 end;
 First:=nil;
 Last:=nil;
end;

procedure TpvBufferRangeAllocator.TRangeList.Insert(const aRange:PRange);
var Current,Previous:PRange;
begin
 if assigned(aRange) then begin
  if assigned(First) then begin
   if aRange^.Start<First^.Start then begin
    aRange^.Next:=First;
    aRange^.Next^.Previous:=aRange;
    First:=aRange;
   end else if aRange^.Start>Last^.Start then begin
    aRange^.Previous:=Last;
    aRange^.Previous^.Next:=aRange;
    Last:=aRange;
   end else begin
    Current:=First;
    while assigned(Current^.Next) and (Current^.Next^.Start<aRange^.Start) do begin
     Current:=Current^.Next;
    end;
    aRange^.Next:=Current^.Next;
    if assigned(Current^.Next) then begin
     aRange^.Next^.Previous:=aRange;
    end;
    Current^.Next:=aRange;
    aRange^.Previous:=Current;
   end;
  end else begin
   First:=aRange;
   Last:=aRange;
   aRange^.Previous:=nil;
   aRange^.Next:=nil;
  end;
 end;
end;

procedure TpvBufferRangeAllocator.TRangeList.Remove(const aRange:PRange);
begin
 if assigned(aRange) then begin
  if assigned(aRange^.Previous) then begin
   aRange^.Previous^.Next:=aRange^.Next;
  end else if First=aRange then begin
   First:=aRange^.Next;
  end;
  if assigned(aRange^.Next) then begin
   aRange^.Next^.Previous:=aRange^.Previous;
  end else if Last=aRange then begin
   Last:=aRange^.Previous;
  end;
  aRange^.Previous:=nil;
  aRange^.Next:=nil;
 end;
end;

procedure TpvBufferRangeAllocator.TRangeList.SortByOffsets;
{$define UseMergeSort}
{$ifdef UseMergeSort}
// Merge sort
var PartA,PartB,CurrentRange:PRange;
    InSize,PartASize,PartBSize,Merges:TpvSizeINt;
begin
 if assigned(First) then begin
  InSize:=1;
  while true do begin
   PartA:=First;
   First:=nil;
   Last:=nil;
   Merges:=0;
   while assigned(PartA) do begin
    inc(Merges);
    PartB:=PartA;
    PartASize:=0;
    while PartASize<InSize do begin
     inc(PartASize);
     PartB:=PartB^.Next;
     if not assigned(PartB) then begin
      break;
     end;
    end;
    PartBSize:=InSize;
    while (PartASize>0) or ((PartBSize>0) and assigned(PartB)) do begin
     if PartASize=0 then begin
      CurrentRange:=PartB;
      PartB:=PartB^.Next;
      dec(PartBSize);
     end else if (PartBSize=0) or not assigned(PartB) then begin
      CurrentRange:=PartA;
      PartA:=PartA^.Next;
      dec(PartASize);
     end else if PartA^.Start<=PartB^.Start then begin
      CurrentRange:=PartA;
      PartA:=PartA^.Next;
      dec(PartASize);
     end else begin
      CurrentRange:=PartB;
      PartB:=PartB^.Next;
      dec(PartBSize);
     end;
     if assigned(Last) then begin
      Last^.Next:=CurrentRange;
     end else begin
      First:=CurrentRange;
     end;
     CurrentRange^.Previous:=Last;
     Last:=CurrentRange;
    end;
    PartA:=PartB;
   end;
   Last^.Next:=nil;
   if Merges<=1 then begin
    break;
   end;
   inc(InSize,InSize);
  end;
 end;
end;
{$else}
// Bubble sort
var Current,Next,ToDelete:PRange;
begin
 Current:=First;
 while assigned(Current) and assigned(Current^.Next) do begin
  Next:=Current^.Next;
  if Current^.Start>Current^.Next^.Start then begin
   Current^.SwapWith(Next^);
   if assigned(Current^.Previous) then begin
    Current:=Current^.Previous;
   end else begin
    Current:=Next;
   end;
  end else begin
   Current:=Next;
  end;
 end;
end;
{$endif}

{ TpvBufferRangeAllocator }

constructor TpvBufferRangeAllocator.Create(const aCapacity:TpvSizeInt=0);
var Range:TpvBufferRangeAllocator.PRange;
begin
 inherited Create;
 fAllocatedRanges.Initialize;
 fFreeRanges.Initialize;
 fAllocatedOffsetRangeRedBlackTree:=TRangeRedBlackTree.Create;
 fFreeOffsetRangeRedBlackTree:=TRangeRedBlackTree.Create;
 fFreeSizeRangeRedBlackTree:=TRangeRedBlackTree.Create;
 fOnResize:=nil;
 fCapacity:=aCapacity;
 if fCapacity>0 then begin
  Range:=TpvBufferRangeAllocator.TRange.CreateRange(0,fCapacity);
  fFreeRanges.Insert(Range);
  Range^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(0,Range);
  Range^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(fCapacity,Range);
 end;
end;

destructor TpvBufferRangeAllocator.Destroy;
begin
 fAllocatedRanges.Finalize;
 fFreeRanges.Finalize;
 FreeAndNil(fAllocatedOffsetRangeRedBlackTree);
 FreeAndNil(fFreeOffsetRangeRedBlackTree);
 FreeAndNil(fFreeSizeRangeRedBlackTree);
 inherited Destroy;
end;

procedure TpvBufferRangeAllocator.MergeFreeRanges(const aRange:PRange);
var Node,OtherNode:TRangeRedBlackTree.TNode;
    Current,Next,ToDelete:PRange;
begin

 // If the given range is valid and has a valid offset node, then try to merge the free ranges around the given range
 if assigned(aRange) and assigned(aRange^.OffsetNode) and (aRange^.OffsetNode.Value=aRange) then begin

  // Walk to the beginning of the contiguous area, where the given range is part of it 
  Node:=aRange^.OffsetNode;  
  repeat
   OtherNode:=Node.Predecessor;
   if assigned(OtherNode) and assigned(OtherNode.Value) and ((OtherNode.Value.Start+OtherNode.Value.Len)=Node.Value.Start) then begin
    Node:=OtherNode;
   end else begin
    break;
   end;
  until false;

  // Merge these contiguous areas to one big contiguous area until there is no more contiguous areas to merge which are connected to each other 
  if assigned(Node) then begin
   Current:=Node.Value;
   OtherNode:=Node.Successor;
   while assigned(OtherNode) and assigned(OtherNode.Value) and ((Node.Value.Start+Node.Value.Len)=OtherNode.Value.Start) do begin
    Next:=OtherNode.Value;
    ToDelete:=Next;
    Current^.Len:=Current^.Len+Next^.Len;
    Next:=Next^.Next;
    fFreeRanges.Remove(ToDelete);
    fFreeOffsetRangeRedBlackTree.Remove(ToDelete^.OffsetNode);
    fFreeSizeRangeRedBlackTree.Remove(ToDelete^.SizeNode);
    ToDelete^.OffsetNode:=nil;
    ToDelete^.SizeNode:=nil;
    FreeMem(ToDelete);
 // fFreeOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
    fFreeSizeRangeRedBlackTree.Remove(Current^.SizeNode);
 // Current^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
    Current^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Current^.Len,Current);
    OtherNode:=Node.Successor;
   end;
  end;

 end else begin

  // Otherwise doing a full merge of all free ranges

  // Sort the free ranges by their offsets for the next steps 
  fFreeRanges.SortByOffsets;

  // Walk through all free ranges and merge them if they are contiguous to each other
  Current:=fFreeRanges.First;
  while assigned(Current) and assigned(Current^.Next) do begin
   if (Current^.Start+Current^.Len)=Current^.Next^.Start then begin
    ToDelete:=Current^.Next;
    Current^.Len:=Current^.Len+Current^.Next^.Len;
    Current^.Next:=Current^.Next^.Next;
    fFreeRanges.Remove(ToDelete);
    fFreeOffsetRangeRedBlackTree.Remove(ToDelete^.OffsetNode);
    fFreeSizeRangeRedBlackTree.Remove(ToDelete^.SizeNode);
    ToDelete^.OffsetNode:=nil;
    ToDelete^.SizeNode:=nil;
    FreeMem(ToDelete);
 // fFreeOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
    fFreeSizeRangeRedBlackTree.Remove(Current^.SizeNode);
 // Current^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
    Current^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Current^.Len,Current);
   end else begin
    Current:=Current^.Next;
   end;
  end;

 end;

end;

function TpvBufferRangeAllocator.Allocate(const aSize:TpvSizeInt):TpvSizeInt;
var Current,Next:PRange;
    Node,OtherNode:TRangeRedBlackTree.TNode;
begin

 if aSize>0 then begin

  repeat

   // Best-fit search
   Node:=fFreeSizeRangeRedBlackTree.fRoot;
   while assigned(Node) do begin
    if aSize<Node.fKey then begin
     if assigned(Node.fLeft) then begin
      // If free block is too big, then go to left
      Node:=Node.fLeft;
      continue;
     end else begin
      // If free block is too big and there is no left children node, then try to find suitable smaller but not too small free blocks
      while assigned(Node) and (Node.fKey>aSize) do begin
       OtherNode:=Node.Predecessor;
       if assigned(OtherNode) and (OtherNode.fKey>=aSize) then begin
        Node:=OtherNode;
       end else begin
        break;
       end;
      end;
      break;
     end;
    end else if aSize>Node.fKey then begin
     if assigned(Node.fRight) then begin
      // If free block is too small, go to right
      Node:=Node.fRight;
      continue;
     end else begin
      // If free block is too small and there is no right children node, then try to find suitable bigger but not too small free blocks
      while assigned(Node) and (Node.fKey<aSize) do begin
       OtherNode:=Node.Successor;
       if assigned(OtherNode) then begin
        Node:=OtherNode;
       end else begin
        break;
       end;
      end;
      break;
     end;
    end else begin
     // Perfect match
     break;
    end;
   end;

   // If a suitable free block was found, then allocate it
   if assigned(Node) then begin
    Current:=Node.Value;
    while assigned(Current) do begin
     if Current^.Len=aSize then begin
      result:=Current^.Start;
      fFreeRanges.Remove(Current);
      fFreeOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
      fFreeSizeRangeRedBlackTree.Remove(Current^.SizeNode);
      Current^.Previous:=nil;
      Current^.Next:=nil;
      fAllocatedRanges.Insert(Current);
      Current^.OffsetNode:=fAllocatedOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
      Current^.SizeNode:=nil;
      exit;
     end else if Current^.Len>aSize then begin
      result:=Current^.Start;
      if Current^.Len>aSize then begin
       Next:=Current^.CreateRange(Current^.Start+aSize,Current^.Len-aSize);
       fFreeRanges.Insert(Next);
       Next^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Next^.Start,Next);
       Next^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Next^.Len,Next);
      end;
      fFreeRanges.Remove(Current);
      fFreeOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
      fFreeSizeRangeRedBlackTree.Remove(Current^.SizeNode);
      Current^.Previous:=nil;
      Current^.Next:=nil;
      fAllocatedRanges.Insert(Current);
      Current^.OffsetNode:=fAllocatedOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
      Current^.SizeNode:=nil;
      exit;
     end;
     Current:=Current^.Next;
    end;
   end;

   // Otherwise, try to resize the buffer
   result:=fCapacity;
   inc(fCapacity,aSize);
   if assigned(fOnResize) then begin
    fOnResize(self,fCapacity);
   end;
   Current:=fFreeRanges.Last;
   Next:=Current^.CreateRange(result,aSize);
   fFreeRanges.Insert(Next);
   Next^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Next^.Start,Next);
   Next^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Next^.Len,Next);

  until false;

 end else begin

  result:=-1;

 end;

end;

procedure TpvBufferRangeAllocator.Release(const aStart:TpvSizeInt;aSize:TpvSizeInt);
var Current,Next:PRange;
    Node,OtherNode:TRangeRedBlackTree.TNode;
begin

 if (aStart>=0) and (aSize<>0) then begin

  // Find any node with the same start offset
  Node:=fAllocatedOffsetRangeRedBlackTree.Find(aStart);
  if assigned(Node) then begin

   // Find the first node with the same start offset by going to the left 
   repeat
    OtherNode:=Node.Predecessor;
    if assigned(OtherNode) and (OtherNode.Key=aStart) then begin
     Node:=OtherNode;
    end else begin
     break;
    end;
   until false;

   // When a specific size is given, then try to find the first node with the same start offset and the same size 
   if aSize>0 then begin
    while assigned(Node.Value) and (Node.Value.Len<>aSize) do begin
     OtherNode:=Node.Successor;
     if assigned(OtherNode) and (OtherNode.Key=aStart) then begin
      if assigned(OtherNode.Value) and (OtherNode.Value.Len=aSize) then begin
       break;
      end else begin
       Node:=OtherNode;
      end;
     end else begin
      break;
     end;
    end;
   end;

   // Initialize the current node and update the size variable
   if assigned(Node.Value) then begin
    Current:=Node.Value;
    aSize:=Current^.Len;
   end else begin
    Current:=fAllocatedRanges.First;
   end;

  end else begin

   // If no node with the same start offset was found, then initialize the current node with the first allocated node
   // for a bruteforce search as fallback
   Current:=fAllocatedRanges.First;

  end;

  // If the current node is valid, then search for the best node to release
  if aSize>0 then begin

   while assigned(Current) do begin
    if (Current^.Start=aStart) and (Current^.Len=aSize) then begin
     fAllocatedRanges.Remove(Current);
     fAllocatedOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
     Current^.OffsetNode:=nil;
     Current^.Previous:=nil;
     Current^.Next:=nil;
     fFreeRanges.Insert(Current);
     Current^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
     Current^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Current^.Len,Current);
     MergeFreeRanges(Current);
     exit;
    end else if (Current^.Start=aStart) and (aSize<Current^.Len) then begin
     Next:=Current^.CreateRange(aStart+aSize,Current^.Len-aSize);
     fAllocatedRanges.Remove(Current);
     fAllocatedOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
     Current^.OffsetNode:=nil;
     Current^.Previous:=nil;
     Current^.Next:=nil;
     fAllocatedRanges.Insert(Current);
     Current^.OffsetNode:=fAllocatedOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
     fFreeRanges.Insert(Next);
     Next^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Next^.Start,Next);
     Next^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Next^.Len,Next);
     MergeFreeRanges(Next);
     exit;
    end else if (Current^.Start<aStart) and ((Current^.Start+Current^.Len)>aStart) then begin
     if (Current^.Start+Current^.Len)>(aStart+aSize) then begin
      Next:=Current^.CreateRange(aStart+aSize,(Current^.Start+Current^.Len)-(aStart+aSize));
      fAllocatedRanges.Remove(Current);
      fAllocatedOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
      Current^.OffsetNode:=nil;
      Current^.Previous:=nil;
      Current^.Next:=nil;
      fAllocatedRanges.Insert(Current);
      Current^.OffsetNode:=fAllocatedOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
      Current^.SizeNode:=nil;
      fFreeRanges.Insert(Next);
      Next^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Next^.Start,Next);
      Next^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Next^.Len,Next);
      MergeFreeRanges(Next);
      exit;
     end else begin
      fAllocatedRanges.Remove(Current);
      fAllocatedOffsetRangeRedBlackTree.Remove(Current^.OffsetNode);
      Current^.OffsetNode:=nil;
      Current^.Previous:=nil;
      Current^.Next:=nil;
      fFreeRanges.Insert(Current);
      Current^.OffsetNode:=fFreeOffsetRangeRedBlackTree.Insert(Current^.Start,Current);
      Current^.SizeNode:=fFreeSizeRangeRedBlackTree.Insert(Current^.Len,Current);
      MergeFreeRanges(Current);
      exit;
     end;
    end;
    Current:=Current^.Next;
   end;

  end;

 end;

end;

end.