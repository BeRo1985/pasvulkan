(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
     PasVulkan.Types;

type { TpvBufferRangeAllocator }
     TpvBufferRangeAllocator=class
      public
       type TOnResize=procedure(const aSender:TpvBufferRangeAllocator;const aNewCapacity:TpvSizeInt) of object;
            PBufferRangeAllocatorRange=^TBufferRangeAllocatorRange;
            { TBufferRangeAllocatorRange }
            TBufferRangeAllocatorRange=record
             public
              Start:TpvSizeInt;
              Len:TpvSizeInt;
              Previous:PBufferRangeAllocatorRange;
              Next:PBufferRangeAllocatorRange;
              procedure SwapWith(var aWith:TBufferRangeAllocatorRange);
              class function CreateRange(const aStart,aLen:TpvSizeInt):TpvBufferRangeAllocator.PBufferRangeAllocatorRange; static;
            end;
            { TBufferRangeAllocatorRangeList }
            TBufferRangeAllocatorRangeList=record
             public
              First:PBufferRangeAllocatorRange;
              Last:PBufferRangeAllocatorRange;
              procedure Initialize;
              procedure Finalize;
              procedure Insert(const aRange:PBufferRangeAllocatorRange);
              procedure Remove(const aRange:PBufferRangeAllocatorRange);
              procedure Sort;
              procedure MergeRanges; // only for free ranges 
            end;
      private
       fAllocatedRanges:TBufferRangeAllocatorRangeList;
       fFreeRanges:TBufferRangeAllocatorRangeList;
       fCapacity:TpvSizeInt;
       fOnResize:TOnResize;
       procedure MergeFreeRanges;
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

{ TpvBufferRangeAllocator.TBufferRangeAllocatorRange }

procedure TpvBufferRangeAllocator.TBufferRangeAllocatorRange.SwapWith(var aWith:TBufferRangeAllocatorRange);
begin
 TpvSwap<TpvSizeInt>.Swap(Start,aWith.Start);
 TpvSwap<TpvSizeInt>.Swap(Len,aWith.Len);
end;

class function TpvBufferRangeAllocator.TBufferRangeAllocatorRange.CreateRange(const aStart,aLen:TpvSizeInt):TpvBufferRangeAllocator.PBufferRangeAllocatorRange;
begin
 GetMem(result,SizeOf(TpvBufferRangeAllocator.TBufferRangeAllocatorRange));
 result^.Start:=aStart;
 result^.Len:=aLen;
 result^.Previous:=nil;
 result^.Next:=nil;
end;

{ TpvBufferRangeAllocator.TBufferRangeAllocatorRangeList }

procedure TpvBufferRangeAllocator.TBufferRangeAllocatorRangeList.Initialize;
begin
 First:=nil;
 Last:=nil;
end;

procedure TpvBufferRangeAllocator.TBufferRangeAllocatorRangeList.Finalize;
var Current,Next:PBufferRangeAllocatorRange;
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

procedure TpvBufferRangeAllocator.TBufferRangeAllocatorRangeList.Insert(const aRange:PBufferRangeAllocatorRange); 
var Current,Previous:PBufferRangeAllocatorRange;
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

procedure TpvBufferRangeAllocator.TBufferRangeAllocatorRangeList.Remove(const aRange:PBufferRangeAllocatorRange);
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

procedure TpvBufferRangeAllocator.TBufferRangeAllocatorRangeList.Sort;
var Current,Next,ToDelete:PBufferRangeAllocatorRange;
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

procedure TpvBufferRangeAllocator.TBufferRangeAllocatorRangeList.MergeRanges;
var Current,Next,ToDelete:PBufferRangeAllocatorRange;
begin

 // Sorting per linked list bubble sort, just for safety, should not be needed, when the sorting insert function works correctly
 Sort;

 // Merging 
 Current:=First;
 while assigned(Current) and assigned(Current^.Next) do begin
  if (Current^.Start+Current^.Len)=Current^.Next^.Start then begin
   ToDelete:=Current^.Next;
   Current^.Len:=Current^.Len+Current^.Next^.Len;
   Current^.Next:=Current^.Next^.Next;
   Remove(ToDelete);
   FreeMem(ToDelete);
  end else begin
   Current:=Current^.Next;
  end;
 end;

end;

{ TpvBufferRangeAllocator }

constructor TpvBufferRangeAllocator.Create(const aCapacity:TpvSizeInt=0);
var Range:TpvBufferRangeAllocator.PBufferRangeAllocatorRange;
begin
 inherited Create;
 fAllocatedRanges.Initialize;
 fFreeRanges.Initialize;
 fOnResize:=nil;
 fCapacity:=aCapacity;
 if fCapacity>0 then begin
  Range:=TpvBufferRangeAllocator.TBufferRangeAllocatorRange.CreateRange(0,fCapacity);
  fFreeRanges.Insert(Range);
 end;
end;

destructor TpvBufferRangeAllocator.Destroy;
begin
 fAllocatedRanges.Finalize;
 fFreeRanges.Finalize;
 inherited Destroy;
end;

procedure TpvBufferRangeAllocator.MergeFreeRanges;
begin
 fFreeRanges.MergeRanges;
end;

function TpvBufferRangeAllocator.Allocate(const aSize:TpvSizeInt):TpvSizeInt;
var Current,Next:PBufferRangeAllocatorRange;
begin
 if aSize>0 then begin
  repeat

   Current:=fFreeRanges.First;
   while assigned(Current) do begin
    if Current^.Len=aSize then begin
     result:=Current^.Start;
     fFreeRanges.Remove(Current);
     Current^.Previous:=nil;
     Current^.Next:=nil;
     fAllocatedRanges.Insert(Current);
     exit;
    end else if Current^.Len>aSize then begin
     result:=Current^.Start;
     if Current^.Len>aSize then begin
      Next:=Current^.CreateRange(Current^.Start+aSize,Current^.Len-aSize);
      fFreeRanges.Insert(Next);
     end;
     fFreeRanges.Remove(Current);
     Current^.Previous:=nil;
     Current^.Next:=nil;
     fAllocatedRanges.Insert(Current);
     exit;
    end;
    Current:=Current^.Next;
   end;

   result:=fCapacity;
   inc(fCapacity,aSize);
   if assigned(fOnResize) then begin
    fOnResize(self,fCapacity);
   end;
   Current:=fFreeRanges.Last;
   Next:=Current^.CreateRange(result,aSize);
   fFreeRanges.Insert(Next);

  until false;

 end else begin
  result:=-1;
 end;

end;

procedure TpvBufferRangeAllocator.Release(const aStart:TpvSizeInt;aSize:TpvSizeInt);
var Current,Next:PBufferRangeAllocatorRange;
begin

 if (aStart>=0) and (aSize<>0) then begin

  if aSize<0 then begin
   Current:=fAllocatedRanges.First;
   while assigned(Current) do begin
    if Current^.Start=aStart then begin
     aSize:=Current^.Len;
     break;
    end;
    Current:=Current^.Next;
   end;
  end;

  if aSize>0 then begin

   Current:=fAllocatedRanges.First;
   while assigned(Current) do begin
    if (Current^.Start=aStart) and (Current^.Len=aSize) then begin
     fAllocatedRanges.Remove(Current);
     Current^.Previous:=nil;
     Current^.Next:=nil;
     fFreeRanges.Insert(Current);
     MergeFreeRanges;
     exit;
    end else if (Current^.Start=aStart) and (aSize<Current^.Len) then begin
     Next:=Current^.CreateRange(aStart+aSize,Current^.Len-aSize);
     fAllocatedRanges.Remove(Current);
     Current^.Previous:=nil;
     Current^.Next:=nil;
     fAllocatedRanges.Insert(Current);
     fFreeRanges.Insert(Next);
     MergeFreeRanges;
     exit;
    end else if (Current^.Start<aStart) and ((Current^.Start+Current^.Len)>aStart) then begin
     if (Current^.Start+Current^.Len)>(aStart+aSize) then begin
      Next:=Current^.CreateRange(aStart+aSize,(Current^.Start+Current^.Len)-(aStart+aSize));
      fAllocatedRanges.Remove(Current);
      Current^.Previous:=nil;
      Current^.Next:=nil;
      fAllocatedRanges.Insert(Current);
      fFreeRanges.Insert(Next);
      MergeFreeRanges;
      exit;
     end else begin
      fAllocatedRanges.Remove(Current);
      Current^.Previous:=nil;
      Current^.Next:=nil;
      fFreeRanges.Insert(Current);
      MergeFreeRanges;
      exit;
     end;
    end;
    Current:=Current^.Next;
   end;

  end;

 end;

end;

end.
