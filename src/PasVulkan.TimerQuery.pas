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
unit PasVulkan.TimerQuery;
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
     PasVulkan.Types,
     PasVulkan.Framework,
     PasVulkan.Collections;

type { TpvTimerQuery }
     TpvTimerQuery=class
      public
       type TNames=TpvGenericList<TpvUTF8String>;
            TRawResults=TpvDynamicArray<TpvUInt64>;
            TTimeStampMasks=TpvDynamicArray<TpvUInt64>;
            TResults=TpvDynamicArray<TpvDouble>;
      private
       fDevice:TpvVulkanDevice;
       fQueryPool:TVkQueryPool;
       fCount:TpvSizeInt;
       fQueryedCount:TpvSizeInt;
       fNames:TNames;
       fRawResults:TRawResults;
       fTimeStampMasks:TTimeStampMasks;
       fResults:TResults;
       fTickSeconds:TpvDouble;
       fTotal:TpvDouble;
       fValid:boolean;
      public
       constructor Create(const aDevice:TpvVulkanDevice;const aCount:TpvSizeInt); reintroduce;
       destructor Destroy; override;
       procedure Reset;
       function Start(const aQueue:TpvVulkanQueue;const aCommandBuffer:TpvVulkanCommandBuffer;const aName:TpvUTF8String='';const aPipelineStage:TVkPipelineStageFlagBits=TVkPipelineStageFlagBits(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT)):TpvSizeInt;
       procedure Stop(const aQueue:TpvVulkanQueue;const aCommandBuffer:TpvVulkanCommandBuffer;const aPipelineStage:TVkPipelineStageFlagBits=TVkPipelineStageFlagBits(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT));
       function Update:boolean;
      public
       property RawResults:TRawResults read fRawResults;
       property Results:TResults read fResults;
       property Total:Double read fTotal;
      published
       property Count:TpvSizeInt read fCount;
       property QueryedCount:TpvSizeInt read fCount;
       property Names:TNames read fNames;
     end;

implementation

{ TpvTimerQuery }

constructor TpvTimerQuery.Create(const aDevice:TpvVulkanDevice;const aCount:TpvSizeInt);
var QueryPoolCreateInfo:TVkQueryPoolCreateInfo;
begin
 fDevice:=aDevice;
 fQueryPool:=VK_NULL_HANDLE;
 fCount:=aCount;
 if assigned(fDevice.Commands.Commands.CreateQueryPool) and
    assigned(fDevice.Commands.Commands.DestroyQueryPool) and
    (fDevice.PhysicalDevice.HostQueryResetFeaturesEXT.hostQueryReset<>VK_FALSE) and
    (assigned(fDevice.Commands.Commands.ResetQueryPool) or
     assigned(fDevice.Commands.Commands.ResetQueryPoolEXT)) then begin
  QueryPoolCreateInfo:=TVkQueryPoolCreateInfo.Create(0,
                                                     TVkQueryType(VK_QUERY_TYPE_TIMESTAMP),
                                                     aCount,
                                                     0);
  VulkanCheckResult(fDevice.Commands.CreateQueryPool(fDevice.Handle,@QueryPoolCreateInfo,fDevice.AllocationCallbacks,@fQueryPool));
 end;
 fTickSeconds:=fDevice.PhysicalDevice.Properties.limits.timestampPeriod;
 fNames:=TNames.Create;
 fNames.Count:=aCount;
 fTotal:=0.0;
 fRawResults.Initialize;
 fRawResults.Count:=fCount shl 1;
 fTimeStampMasks.Initialize;
 fTimeStampMasks.Count:=fCount;
 fResults.Initialize;
 fResults.Count:=fCount;
 fValid:=false;
end;

destructor TpvTimerQuery.Destroy;
begin
 if fQueryPool<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyQueryPool(fDevice.Handle,fQueryPool,fDevice.AllocationCallbacks);
 end;
 FreeAndNil(fNames);
 fTimeStampMasks.Finalize;
 fRawResults.Finalize;
 fResults.Finalize;
 inherited Destroy;
end;

procedure TpvTimerQuery.Reset;
begin
 fQueryedCount:=0;
 fValid:=false;
end;

function TpvTimerQuery.Start(const aQueue:TpvVulkanQueue;const aCommandBuffer:TpvVulkanCommandBuffer;const aName:TpvUTF8String;const aPipelineStage:TVkPipelineStageFlagBits):TpvSizeInt;
var TimeStampValidBits,TimeStampMask:TpvUInt64;
begin
 if (fQueryPool<>VK_NULL_HANDLE) and (fQueryedCount<fCount) then begin
  TimeStampValidBits:=fDevice.PhysicalDevice.QueueFamilyProperties[aQueue.QueueFamilyIndex].timestampValidBits;
  if TimeStampValidBits<>0 then begin
   if TimeStampValidBits>=64 then begin
    TimeStampMask:=UInt64($ffffffffffffffff);
   end else begin
    TimeStampMask:=(UInt64(1) shl TimeStampValidBits)-1;
   end;
   fTimeStampMasks.Items[fQueryedCount]:=TimeStampMask;
   if assigned(fDevice.Commands.Commands.ResetQueryPool) then begin
    fDevice.Commands.ResetQueryPool(fDevice.Handle,QueryedCount,fQueryedCount shl 1,2);
   end else if assigned(fDevice.Commands.Commands.ResetQueryPoolEXT) then begin
    fDevice.Commands.ResetQueryPoolEXT(fDevice.Handle,QueryedCount,fQueryedCount shl 1,2);
   end;
   aCommandBuffer.CmdWriteTimestamp(aPipelineStage,fQueryPool,fQueryedCount shl 1);
   if fNames[fQueryedCount]<>aName then begin
    fNames[fQueryedCount]:=aName;
   end;
   result:=fQueryedCount;
   fValid:=true;
  end;
 end else begin
  result:=-1;
  fValid:=false;
 end;
end;

procedure TpvTimerQuery.Stop(const aQueue:TpvVulkanQueue;const aCommandBuffer:TpvVulkanCommandBuffer;const aPipelineStage:TVkPipelineStageFlagBits);
begin
 if fValid then begin
  aCommandBuffer.CmdWriteTimestamp(aPipelineStage,fQueryPool,(fQueryedCount shl 1) or 1);
  inc(fQueryedCount);
  fValid:=false;
 end;
end;

function TpvTimerQuery.Update:boolean;
var Index:TpvSizeInt;
begin
 result:=(fQueryedCount>0) and (fDevice.Commands.GetQueryPoolResults(fDevice.Handle,fQueryPool,0,fQueryedCount shl 1,(fQueryedCount shl 1)*SizeOf(TpvUInt64),@fRawResults.Items[0],SizeOf(TpvUInt64),TVkQueryResultFlags(VK_QUERY_RESULT_64_BIT) or TVkQueryResultFlags(VK_QUERY_RESULT_WAIT_BIT))=VK_SUCCESS);
 if result then begin
  for Index:=0 to fQueryedCount-1 do begin
   fResults.Items[Index]:=((fRawResults.Items[(Index shl 1) or 1]-RawResults.Items[Index shl 1]) and fTimeStampMasks.Items[Index])*fTickSeconds;
  end;
  fTotal:=((fRawResults.Items[((fQueryedCount-1) shl 1) or 1] and fTimeStampMasks.Items[fQueryedCount-1])-(fRawResults.Items[0] and fTimeStampMasks.Items[0]))*fTickSeconds;
 end;
end;

end.
