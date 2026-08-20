(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2026, Benjamin Rosseaux (benjamin@rosseaux.de)               *
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
 ******************************************************************************)
unit PasVulkan.FrameTrace;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

// A ring buffer for tracking down timing dependent hangs in the frame loop.
//
// Writing an entry costs one interlocked increment and a few stores into a statically allocated
// array. No allocation, no formatting, no file, no lock - which is the whole point: logging the
// same events through WriteLn perturbs the timing enough to make the race disappear, so the trace
// has to stay out of the way of what it observes.
//
// Reading happens from a debugger once the application sits still, since there is nobody left to
// print it at that moment:
//
//   p PVFRAMETRACESEQUENCE
//   p PVFRAMETRACEENTRIES
//
// The sequence number is written last, so an entry whose sequence does not match its slot was
// caught half written and is to be ignored.

interface

uses SysUtils,
     Classes,
     PasMP,
     PasVulkan.Types;

const // Small on purpose: it covers the last few dozen frames, which is all a hang needs, and it
      // stays small enough to be printed in one go
      pvFrameTraceCount=256;
      pvFrameTraceMask=pvFrameTraceCount-1;

      pvFrameTraceKindAcquire=1;        // an image was asked for, ValueA is the semaphore it signals
      pvFrameTraceKindWaitSemaphore=2;  // the frame's submit chain now hangs on ValueA
      pvFrameTraceKindFrameFence=3;     // the frame fence submit went out, ValueA semaphore, ValueB fence
      pvFrameTraceKindFenceStuck=4;     // that fence did not signal in time, ValueA fence, ValueB semaphore
      pvFrameTraceKindPresent=5;        // a present was queued, ValueA is its identifier

type PpvFrameTraceEntry=^TpvFrameTraceEntry;
     TpvFrameTraceEntry=record
      Kind:TpvUInt32;
      Index:TpvUInt32;
      ValueA:TpvUInt64;
      ValueB:TpvUInt64;
      // Last field on purpose, see the unit comment
      Sequence:TpvUInt64;
     end;

     TpvFrameTraceEntries=array[0..pvFrameTraceCount-1] of TpvFrameTraceEntry;

var pvFrameTraceEntries:TpvFrameTraceEntries;
    pvFrameTraceSequence:TPasMPInt64=0;

procedure pvFrameTraceAdd(const aKind,aIndex:TpvUInt32;const aValueA:TpvUInt64=0;const aValueB:TpvUInt64=0);

implementation

procedure pvFrameTraceAdd(const aKind,aIndex:TpvUInt32;const aValueA:TpvUInt64=0;const aValueB:TpvUInt64=0);
var Sequence:TPasMPInt64;
    Entry:PpvFrameTraceEntry;
begin
 Sequence:=TPasMPInterlocked.Increment(pvFrameTraceSequence);
 Entry:=@pvFrameTraceEntries[Sequence and pvFrameTraceMask];
 Entry^.Kind:=aKind;
 Entry^.Index:=aIndex;
 Entry^.ValueA:=aValueA;
 Entry^.ValueB:=aValueB;
 Entry^.Sequence:=Sequence;
end;

end.
