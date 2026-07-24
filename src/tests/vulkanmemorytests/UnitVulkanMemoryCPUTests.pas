(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
 * 4. After a pull request, check the status of your pull request at          *
 *    http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                       *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit UnitVulkanMemoryCPUTests;
{$i ../../PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

procedure RunVulkanMemoryCPUTests;

implementation

uses SysUtils,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Framework,
     UnitVulkanMemoryTestUtils;

type TTestAllocation=record
      Block:TpvVulkanDeviceMemoryChunkBlock;
      Offset:TVkDeviceSize;
      Size:TVkDeviceSize;
      Alignment:TVkDeviceSize;
      AllocationType:TpvVulkanDeviceMemoryAllocationType;
     end;

     TTestAllocations=array of TTestAllocation;

const TestAlignments:array[0..10] of TVkDeviceSize=
       (
        1,
        2,
        3,
        4,
        7,
        8,
        16,
        31,
        64,
        192,
        256
       );

function MinimumDeviceSize(const aLeft,aRight:TVkDeviceSize):TVkDeviceSize;
begin
 if aLeft<aRight then begin
  result:=aLeft;
 end else begin
  result:=aRight;
 end;
end;

function MaximumDeviceSize(const aLeft,aRight:TVkDeviceSize):TVkDeviceSize;
begin
 if aLeft>aRight then begin
  result:=aLeft;
 end else begin
  result:=aRight;
 end;
end;

function NextRandom(var aState:TpvUInt64):TpvUInt64;
begin
 if aState=0 then begin
  aState:=(TpvUInt64($9e3779b9) shl 32) or TpvUInt64($7f4a7c15);
 end;
 aState:=aState xor (aState shr 12);
 aState:=aState xor (aState shl 25);
 aState:=aState xor (aState shr 27);
 result:=aState*$2545f4914f6cdd1d;
end;

function RandomBelow(var aState:TpvUInt64;const aLimit:TpvUInt64):TpvUInt64;
begin
 if aLimit>0 then begin
  result:=NextRandom(aState) mod aLimit;
 end else begin
  result:=0;
 end;
end;

function RandomAllocationType(var aState:TpvUInt64):TpvVulkanDeviceMemoryAllocationType;
begin
 case RandomBelow(aState,5) of
  0:begin
   result:=TpvVulkanDeviceMemoryAllocationType.Buffer;
  end;
  1:begin
   result:=TpvVulkanDeviceMemoryAllocationType.ImageLinear;
  end;
  2:begin
   result:=TpvVulkanDeviceMemoryAllocationType.ImageOptimal;
  end;
  3:begin
   result:=TpvVulkanDeviceMemoryAllocationType.Image;
  end;
  else begin
   result:=TpvVulkanDeviceMemoryAllocationType.Unknown;
  end;
 end;
end;

procedure CheckRangesEqual(const aLeft,aRight:TpvVulkanDeviceMemoryChunkTestRanges;
                           const aMessage:TpvUTF8String);
var Index:TpvSizeInt;
begin
 VulkanMemoryTestCheck(length(aLeft)=length(aRight),aMessage);
 for Index:=0 to length(aLeft)-1 do begin
  VulkanMemoryTestCheck((aLeft[Index].Block=aRight[Index].Block) and
                        (aLeft[Index].Offset=aRight[Index].Offset) and
                        (aLeft[Index].Size=aRight[Index].Size) and
                        (aLeft[Index].Alignment=aRight[Index].Alignment) and
                        (aLeft[Index].AllocationType=aRight[Index].AllocationType),
                        aMessage);
 end;
end;

procedure CheckChunk(const aMemoryChunk:TpvVulkanDeviceMemoryChunk;
                     const aAllocations:TTestAllocations;
                     const aCountAllocations:TpvSizeInt);
var RangeIndex,AllocationIndex:TpvSizeInt;
    Ranges:TpvVulkanDeviceMemoryChunkTestRanges;
    Statistics:TpvVulkanDeviceMemoryChunkTestStatistics;
    Error:TpvUTF8String;
    Used,AllocationCount,AllocationBytes,
    AllocationSizeMin,AllocationSizeMax,
    UnusedRangeCount,UnusedRangeSizeMin,UnusedRangeSizeMax:TVkDeviceSize;
    Found:boolean;
begin

 // Validate the live trees and the counters before the full statistics scan can repair them.
 VulkanMemoryTestCheck(aMemoryChunk.ValidateForTests(Error),Error);

 aMemoryChunk.GetRangesForTests(Ranges);

 // Every active allocation must still refer to exactly the range returned by the allocator.
 for AllocationIndex:=0 to aCountAllocations-1 do begin
  Found:=false;
  for RangeIndex:=0 to length(Ranges)-1 do begin
   if Ranges[RangeIndex].Block=aAllocations[AllocationIndex].Block then begin
    Found:=true;
    VulkanMemoryTestCheck((Ranges[RangeIndex].Offset=aAllocations[AllocationIndex].Offset) and
                          (Ranges[RangeIndex].Size=aAllocations[AllocationIndex].Size) and
                          (Ranges[RangeIndex].AllocationType=aAllocations[AllocationIndex].AllocationType),
                          'Active allocation differs from its memory chunk range');
    break;
   end;
  end;
  VulkanMemoryTestCheck(Found,'Active allocation is missing from the memory chunk ranges');
 end;

 // Derive an independent statistics snapshot from the ordered ranges.
 Used:=0;
 AllocationCount:=0;
 AllocationBytes:=0;
 AllocationSizeMin:=TVkDeviceSize(VK_WHOLE_SIZE);
 AllocationSizeMax:=0;
 UnusedRangeCount:=0;
 UnusedRangeSizeMin:=TVkDeviceSize(VK_WHOLE_SIZE);
 UnusedRangeSizeMax:=0;

 for RangeIndex:=0 to length(Ranges)-1 do begin
  if Ranges[RangeIndex].AllocationType=TpvVulkanDeviceMemoryAllocationType.Free then begin
   inc(UnusedRangeCount);
   UnusedRangeSizeMin:=MinimumDeviceSize(UnusedRangeSizeMin,Ranges[RangeIndex].Size);
   UnusedRangeSizeMax:=MaximumDeviceSize(UnusedRangeSizeMax,Ranges[RangeIndex].Size);
  end else begin
   inc(Used,Ranges[RangeIndex].Size);
   inc(AllocationCount);
   inc(AllocationBytes,Ranges[RangeIndex].Size);
   AllocationSizeMin:=MinimumDeviceSize(AllocationSizeMin,Ranges[RangeIndex].Size);
   AllocationSizeMax:=MaximumDeviceSize(AllocationSizeMax,Ranges[RangeIndex].Size);
  end;
 end;

 if AllocationCount=0 then begin
  AllocationSizeMin:=0;
  AllocationSizeMax:=0;
 end;

 if UnusedRangeCount=0 then begin
  UnusedRangeSizeMin:=0;
  UnusedRangeSizeMax:=0;
 end;

 aMemoryChunk.UpdateStatisticsForTests;
 Statistics:=aMemoryChunk.GetStatisticsForTests;

 VulkanMemoryTestCheck((Statistics.Used=Used) and
                       (Statistics.AllocationCount=AllocationCount) and
                       (Statistics.AllocationBytes=AllocationBytes) and
                       (Statistics.AllocationSizeMin=AllocationSizeMin) and
                       (Statistics.AllocationSizeMax=AllocationSizeMax) and
                       (Statistics.UnusedRangeCount=UnusedRangeCount) and
                       (Statistics.UnusedRangeSizeMin=UnusedRangeSizeMin) and
                       (Statistics.UnusedRangeSizeMax=UnusedRangeSizeMax),
                       'Full memory chunk statistics differ from the ordered ranges');

end;

procedure TestExactFit;
var MemoryChunk:TpvVulkanDeviceMemoryChunk;
    MemoryChunkBlock:TpvVulkanDeviceMemoryChunkBlock;
    Offset:TVkDeviceSize;
    Ranges:TpvVulkanDeviceMemoryChunkTestRanges;
    Allocations:TTestAllocations;
begin

 MemoryChunk:=TpvVulkanDeviceMemoryChunk.CreateForTests(1024,1,1);
 try

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(MemoryChunkBlock,
                                                  Offset,
                                                  1024,
                                                  1,
                                                  TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'Exact-fit allocation failed');
  VulkanMemoryTestCheck(Offset=0,'Exact-fit allocation has a wrong offset');

  SetLength(Allocations,1);
  Allocations[0].Block:=MemoryChunkBlock;
  Allocations[0].Offset:=Offset;
  Allocations[0].Size:=1024;
  Allocations[0].Alignment:=1;
  Allocations[0].AllocationType:=TpvVulkanDeviceMemoryAllocationType.Buffer;

  CheckChunk(MemoryChunk,Allocations,1);

  MemoryChunk.GetRangesForTests(Ranges);
  VulkanMemoryTestCheck((length(Ranges)=1) and
                        (Ranges[0].Offset=0) and
                        (Ranges[0].Size=1024) and
                        (Ranges[0].AllocationType=TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'Exact-fit allocation left a spurious split range');

  VulkanMemoryTestCheck(MemoryChunk.FreeMemory(Offset),'Exact-fit allocation could not be freed');
  CheckChunk(MemoryChunk,Allocations,0);

  MemoryChunk.GetRangesForTests(Ranges);
  VulkanMemoryTestCheck((length(Ranges)=1) and
                        (Ranges[0].Offset=0) and
                        (Ranges[0].Size=1024) and
                        (Ranges[0].AllocationType=TpvVulkanDeviceMemoryAllocationType.Free),
                        'Exact-fit free did not restore the original range');

 finally
  FreeAndNil(MemoryChunk);
 end;

 VulkanMemoryTestPassed('CPU exact fit');

end;

procedure TestSplitCoalesceAndAlignment;
var MemoryChunk:TpvVulkanDeviceMemoryChunk;
    FirstBlock,SecondBlock,ThirdBlock:TpvVulkanDeviceMemoryChunkBlock;
    FirstOffset,SecondOffset,ThirdOffset:TVkDeviceSize;
    Ranges:TpvVulkanDeviceMemoryChunkTestRanges;
    Allocations:TTestAllocations;
begin

 MemoryChunk:=TpvVulkanDeviceMemoryChunk.CreateForTests(4096,1,1);
 try

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(FirstBlock,
                                                  FirstOffset,
                                                  1,
                                                  1,
                                                  TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'First split allocation failed');

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(SecondBlock,
                                                  SecondOffset,
                                                  23,
                                                  192,
                                                  TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'Aligned split allocation failed');
  VulkanMemoryTestCheck(SecondOffset=256,'Non-power-of-two alignment was not rounded up');

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(ThirdBlock,
                                                  ThirdOffset,
                                                  100,
                                                  1,
                                                  TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'Third split allocation failed');

  SetLength(Allocations,3);
  Allocations[0].Block:=FirstBlock;
  Allocations[0].Offset:=FirstOffset;
  Allocations[0].Size:=1;
  Allocations[0].Alignment:=1;
  Allocations[0].AllocationType:=TpvVulkanDeviceMemoryAllocationType.Buffer;
  Allocations[1].Block:=SecondBlock;
  Allocations[1].Offset:=SecondOffset;
  Allocations[1].Size:=23;
  Allocations[1].Alignment:=256;
  Allocations[1].AllocationType:=TpvVulkanDeviceMemoryAllocationType.Buffer;
  Allocations[2].Block:=ThirdBlock;
  Allocations[2].Offset:=ThirdOffset;
  Allocations[2].Size:=100;
  Allocations[2].Alignment:=1;
  Allocations[2].AllocationType:=TpvVulkanDeviceMemoryAllocationType.Buffer;

  CheckChunk(MemoryChunk,Allocations,3);

  VulkanMemoryTestCheck(MemoryChunk.FreeMemory(SecondOffset),'Middle allocation could not be freed');
  Allocations[1]:=Allocations[2];
  CheckChunk(MemoryChunk,Allocations,2);

  VulkanMemoryTestCheck(MemoryChunk.FreeMemory(FirstOffset),'First allocation could not be freed');
  Allocations[0]:=Allocations[1];
  CheckChunk(MemoryChunk,Allocations,1);

  VulkanMemoryTestCheck(MemoryChunk.FreeMemory(ThirdOffset),'Third allocation could not be freed');
  CheckChunk(MemoryChunk,Allocations,0);

  MemoryChunk.GetRangesForTests(Ranges);
  VulkanMemoryTestCheck((length(Ranges)=1) and
                        (Ranges[0].Offset=0) and
                        (Ranges[0].Size=4096) and
                        (Ranges[0].AllocationType=TpvVulkanDeviceMemoryAllocationType.Free),
                        'Split ranges were not completely coalesced');

 finally
  FreeAndNil(MemoryChunk);
 end;

 VulkanMemoryTestPassed('CPU split, coalesce and alignment');

end;

procedure TestBufferImageGranularity;
var MemoryChunk:TpvVulkanDeviceMemoryChunk;
    FirstBlock,SecondBlock:TpvVulkanDeviceMemoryChunkBlock;
    FirstOffset,SecondOffset:TVkDeviceSize;
    Allocations:TTestAllocations;
begin

 MemoryChunk:=TpvVulkanDeviceMemoryChunk.CreateForTests(4096,1,256);
 try

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(FirstBlock,
                                                  FirstOffset,
                                                  100,
                                                  1,
                                                  TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'Buffer granularity allocation failed');

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(SecondBlock,
                                                  SecondOffset,
                                                  100,
                                                  1,
                                                  TpvVulkanDeviceMemoryAllocationType.ImageOptimal),
                        'Image granularity allocation failed');
  VulkanMemoryTestCheck(SecondOffset=256,'Buffer-image granularity page was shared');

  SetLength(Allocations,2);
  Allocations[0].Block:=FirstBlock;
  Allocations[0].Offset:=FirstOffset;
  Allocations[0].Size:=100;
  Allocations[0].Alignment:=1;
  Allocations[0].AllocationType:=TpvVulkanDeviceMemoryAllocationType.Buffer;
  Allocations[1].Block:=SecondBlock;
  Allocations[1].Offset:=SecondOffset;
  Allocations[1].Size:=100;
  Allocations[1].Alignment:=256;
  Allocations[1].AllocationType:=TpvVulkanDeviceMemoryAllocationType.ImageOptimal;

  CheckChunk(MemoryChunk,Allocations,2);

  VulkanMemoryTestCheck(MemoryChunk.FreeMemory(FirstOffset),'Granularity buffer could not be freed');
  VulkanMemoryTestCheck(MemoryChunk.FreeMemory(SecondOffset),'Granularity image could not be freed');
  CheckChunk(MemoryChunk,Allocations,0);

 finally
  FreeAndNil(MemoryChunk);
 end;

 VulkanMemoryTestPassed('CPU buffer-image granularity');

end;

procedure TestReallocationFailureIsAtomic;
var MemoryChunk:TpvVulkanDeviceMemoryChunk;
    FirstBlock,SecondBlock:TpvVulkanDeviceMemoryChunkBlock;
    FirstOffset,SecondOffset:TVkDeviceSize;
    BeforeRanges,AfterRanges:TpvVulkanDeviceMemoryChunkTestRanges;
    Allocations:TTestAllocations;
begin

 MemoryChunk:=TpvVulkanDeviceMemoryChunk.CreateForTests(1024,1,1);
 try

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(FirstBlock,
                                                  FirstOffset,
                                                  512,
                                                  1,
                                                  TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'First reallocation allocation failed');

  VulkanMemoryTestCheck(MemoryChunk.AllocateMemory(SecondBlock,
                                                  SecondOffset,
                                                  512,
                                                  1,
                                                  TpvVulkanDeviceMemoryAllocationType.Buffer),
                        'Second reallocation allocation failed');

  SetLength(Allocations,2);
  Allocations[0].Block:=FirstBlock;
  Allocations[0].Offset:=FirstOffset;
  Allocations[0].Size:=512;
  Allocations[0].Alignment:=1;
  Allocations[0].AllocationType:=TpvVulkanDeviceMemoryAllocationType.Buffer;
  Allocations[1].Block:=SecondBlock;
  Allocations[1].Offset:=SecondOffset;
  Allocations[1].Size:=512;
  Allocations[1].Alignment:=1;
  Allocations[1].AllocationType:=TpvVulkanDeviceMemoryAllocationType.Buffer;

  MemoryChunk.GetRangesForTests(BeforeRanges);
  VulkanMemoryTestCheck(not MemoryChunk.ReallocateMemory(FirstOffset,513,1),
                        'Blocked in-place reallocation unexpectedly succeeded');
  MemoryChunk.GetRangesForTests(AfterRanges);

  CheckRangesEqual(BeforeRanges,AfterRanges,'Failed reallocation changed the memory chunk ranges');
  CheckChunk(MemoryChunk,Allocations,2);

 finally
  FreeAndNil(MemoryChunk);
 end;

 VulkanMemoryTestPassed('CPU failed reallocation atomicity');

end;

procedure TestDedicatedBufferDeviceAddressChain;
var MemoryAllocateInfo:TVkMemoryAllocateInfo;
    MemoryAllocateFlagsInfoKHR:TVkMemoryAllocateFlagsInfoKHR;
    MemoryDedicatedAllocateInfoKHR:TVkMemoryDedicatedAllocateInfoKHR;
begin

 FillChar(MemoryDedicatedAllocateInfoKHR,SizeOf(TVkMemoryDedicatedAllocateInfoKHR),#0);
 MemoryDedicatedAllocateInfoKHR.sType:=VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR;

 TpvVulkanDeviceMemoryChunk.InitializeMemoryAllocateInfoForTests(MemoryAllocateInfo,
                                                                 MemoryAllocateFlagsInfoKHR,
                                                                 4096,
                                                                 3,
                                                                 [TpvVulkanDeviceMemoryChunkFlag.DedicatedAllocation,
                                                                  TpvVulkanDeviceMemoryChunkFlag.BufferDeviceAddress],
                                                                 @MemoryDedicatedAllocateInfoKHR);

 VulkanMemoryTestCheck(MemoryAllocateInfo.pNext=@MemoryAllocateFlagsInfoKHR,
                       'Buffer-device-address allocate flags are not first in pNext');
 VulkanMemoryTestCheck(MemoryAllocateFlagsInfoKHR.pNext=@MemoryDedicatedAllocateInfoKHR,
                       'Dedicated allocation info was dropped from the pNext chain');
 VulkanMemoryTestCheck((MemoryAllocateFlagsInfoKHR.flags and
                        TVkMemoryAllocateFlagsKHR(VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR))<>0,
                       'Buffer-device-address allocation flag is missing');

 TpvVulkanDeviceMemoryChunk.InitializeMemoryAllocateInfoForTests(MemoryAllocateInfo,
                                                                 MemoryAllocateFlagsInfoKHR,
                                                                 4096,
                                                                 3,
                                                                 [TpvVulkanDeviceMemoryChunkFlag.DedicatedAllocation],
                                                                 @MemoryDedicatedAllocateInfoKHR);

 VulkanMemoryTestCheck(MemoryAllocateInfo.pNext=@MemoryDedicatedAllocateInfoKHR,
                       'Dedicated allocation info is not the direct pNext without BDA');

 VulkanMemoryTestPassed('CPU dedicated+BDA pNext chain');

end;

procedure TestRandomizedAllocator;
const CountSeeds=32;
      CountOperations=4000;
      ChunkSize=TVkDeviceSize(65536);
var SeedIndex,OperationIndex,AllocationIndex:TpvSizeInt;
    CountAllocations:TpvSizeInt;
    Seed,State:TpvUInt64;
    MemoryChunk:TpvVulkanDeviceMemoryChunk;
    MemoryChunkBlock:TpvVulkanDeviceMemoryChunkBlock;
    Offset,Size,Alignment,OldSize:TVkDeviceSize;
    AllocationType:TpvVulkanDeviceMemoryAllocationType;
    Allocations:TTestAllocations;
    BeforeRanges,AfterRanges:TpvVulkanDeviceMemoryChunkTestRanges;
    Operation:TpvUInt64;
    Succeeded:boolean;
    SeedText:TpvUTF8String;
begin

 SeedText:=TpvUTF8String(GetEnvironmentVariable('PASVULKAN_MEMORY_TEST_SEED'));
 if length(SeedText)>0 then begin
  Seed:=TpvUInt64(StrToInt64(String(SeedText)));
 end else begin
  Seed:=$5eedc0de12345678;
 end;

 WriteLn('[INFO] CPU randomized allocator seed: ',IntToHex(Seed,16));

 SetLength(Allocations,1024);

 for SeedIndex:=0 to CountSeeds-1 do begin

  State:=Seed xor
         (TpvUInt64(SeedIndex)*((TpvUInt64($9e3779b9) shl 32) or TpvUInt64($7f4a7c15)));
  CountAllocations:=0;

  MemoryChunk:=TpvVulkanDeviceMemoryChunk.CreateForTests(ChunkSize,1,256);
  try

   for OperationIndex:=0 to CountOperations-1 do begin

    Operation:=RandomBelow(State,100);

    if (CountAllocations=0) or
       ((Operation<52) and (CountAllocations<length(Allocations))) then begin

     // Allocate with deliberately awkward alignments and all granularity categories.
     Size:=1+RandomBelow(State,2048);
     Alignment:=TestAlignments[TpvSizeInt(RandomBelow(State,length(TestAlignments)))];
     AllocationType:=RandomAllocationType(State);

     Succeeded:=MemoryChunk.AllocateMemory(MemoryChunkBlock,
                                          Offset,
                                          Size,
                                          Alignment,
                                          AllocationType);
     if Succeeded then begin
      Allocations[CountAllocations].Block:=MemoryChunkBlock;
      Allocations[CountAllocations].Offset:=Offset;
      Allocations[CountAllocations].Size:=Size;
      Allocations[CountAllocations].Alignment:=MemoryChunkBlock.Alignment;
      Allocations[CountAllocations].AllocationType:=AllocationType;
      inc(CountAllocations);
     end;

    end else if Operation<78 then begin

     // Free in random order to exercise every coalescing direction.
     AllocationIndex:=TpvSizeInt(RandomBelow(State,CountAllocations));
     VulkanMemoryTestCheck(MemoryChunk.FreeMemory(Allocations[AllocationIndex].Offset),
                           'Randomized free failed');
     dec(CountAllocations);
     if AllocationIndex<CountAllocations then begin
      Allocations[AllocationIndex]:=Allocations[CountAllocations];
     end;

    end else begin

     // Reallocation is in-place only; a failed attempt must leave the complete topology unchanged.
     AllocationIndex:=TpvSizeInt(RandomBelow(State,CountAllocations));
     Size:=1+RandomBelow(State,4096);
     Alignment:=TestAlignments[TpvSizeInt(RandomBelow(State,length(TestAlignments)))];
     OldSize:=Allocations[AllocationIndex].Size;

     MemoryChunk.GetRangesForTests(BeforeRanges);
     Succeeded:=MemoryChunk.ReallocateMemory(Allocations[AllocationIndex].Offset,
                                             Size,
                                             Alignment);
     if Succeeded then begin
      Allocations[AllocationIndex].Size:=Size;
      Allocations[AllocationIndex].Alignment:=Allocations[AllocationIndex].Block.Alignment;
     end else begin
      MemoryChunk.GetRangesForTests(AfterRanges);
      CheckRangesEqual(BeforeRanges,
                       AfterRanges,
                       'Failed randomized reallocation changed the memory chunk ranges');
      VulkanMemoryTestCheck(Allocations[AllocationIndex].Size=OldSize,
                           'Failed randomized reallocation changed the model size');
     end;

    end;

    CheckChunk(MemoryChunk,Allocations,CountAllocations);

   end;

   while CountAllocations>0 do begin
    dec(CountAllocations);
    VulkanMemoryTestCheck(MemoryChunk.FreeMemory(Allocations[CountAllocations].Offset),
                          'Randomized cleanup free failed');
    CheckChunk(MemoryChunk,Allocations,CountAllocations);
   end;

  finally
   FreeAndNil(MemoryChunk);
  end;

 end;

 VulkanMemoryTestPassed('CPU randomized split/coalesce/alignment/granularity/reallocation/statistics');

end;

procedure RunVulkanMemoryCPUTests;
begin
 TestExactFit;
 TestSplitCoalesceAndAlignment;
 TestBufferImageGranularity;
 TestReallocationFailureIsAtomic;
 TestDedicatedBufferDeviceAddressChain;
 TestRandomizedAllocator;
end;

end.
