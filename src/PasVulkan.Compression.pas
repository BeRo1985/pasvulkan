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
unit PasVulkan.Compression;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$rangechecks off}
{$overflowchecks off}

{$scopedenums on}
{$rangechecks off}
{$overflowchecks off}

{$ifdef fpc}
 {$optimization off}
 {$optimization level1}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Compression.Deflate,
     PasVulkan.Compression.LZBRS,
     PasVulkan.Compression.LZBRSF,
     PasVulkan.Compression.LZBRSX,
     PasVulkan.Compression.LZBRRC,
     PasVulkan.Compression.LZMA;

type TpvCompressionMethod=
      (
       None=0,
       Deflate=1,
       LZBRSF=2,
       LZBRRC=3,
       LZMA=4,
       LZBRS=5,
       LZBRSX=6
      );

var pvCompressionPasMPInstance:TPasMP=nil;

// Convert a 32-bit float to a uint32, preserving order.
function MapFloatToUInt32WithOrderPreservation(const aValue:TpvFloat):TpvUInt32; inline;

// Convert a 32-bit uint32 to a float, preserving order.
function UnmapFloatFromUInt32WithOrderPreservation(const aValue:TpvUInt32):TpvFloat; inline;

// This function transforms 32-bit float data to a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure ForwardTransform32BitFloatData(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt); overload;
procedure ForwardTransform32BitFloatData(const aStream:TStream); overload;

// This function transforms 32-bit float data back from a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure BackwardTransform32BitFloatData(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt); overload;
procedure BackwardTransform32BitFloatData(const aStream:TStream); overload;

// This function transforms RGBA8 data to a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure ForwardTransformRGBA8Data(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt); overload;
procedure ForwardTransformRGBA8Data(const aStream:TStream); overload;

// This function transforms RGBA8 data back from a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure BackwardTransformRGBA8Data(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt); overload;
procedure BackwardTransformRGBA8Data(const aStream:TStream); overload;

function CompressStream(const aInStream:TStream;const aOutStream:TStream;const aCompressionMethod:TpvCompressionMethod=TpvCompressionMethod.LZBRRC;const aCompressionLevel:TpvUInt32=5;const aParts:TpvUInt32=0):boolean;

function DecompressStream(const aInStream:TStream;const aOutStream:TStream):boolean;

implementation

//uses PasVulkan.Application;

////////////////////////////////////////////////////////////////////////////////////////

// Convert a 32-bit float to a uint32, preserving order.
function MapFloatToUInt32WithOrderPreservation(const aValue:TpvFloat):TpvUInt32; inline;
var Temporary:TpvUInt32;
begin
 Temporary:=TpvUInt32(Pointer(@aValue)^);
 result:=Temporary xor (TpvUInt32(TpvUInt32(-TpvInt32(TpvUInt32(Temporary shr 31)))) or TpvUInt32($80000000));
end;

// Convert a 32-bit uint32 to a float, preserving order.
function UnmapFloatFromUInt32WithOrderPreservation(const aValue:TpvUInt32):TpvFloat; inline;
var Temporary:TpvUInt32;
begin
 Temporary:=aValue xor TpvUInt32(TpvUInt32(TpvUInt32(aValue shr 31)-1) or TpvUInt32($80000000));
 result:=TpvFloat(Pointer(@Temporary)^);
end;

////////////////////////////////////////////////////////////////////////////////////////

// This function transforms 32-bit float data to a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure ForwardTransform32BitFloatData(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt);
var Index,Count:TpvSizeInt;
    Previous,Value,Delta:TpvUInt32;
begin
 Count:=aDataSize shr 2;
 Previous:=0;
 for Index:=0 to Count-1 do begin
  Value:=PpvUInt32Array(aInData)^[Index];
  Value:=Value xor (TpvUInt32(TpvUInt32(-TpvInt32(TpvUInt32(Value shr 31)))) or TpvUInt32($80000000));
  Delta:=Value-Previous;
  Previous:=Value;
  PpvUInt8Array(aOutData)^[Index]:=(Delta shr 24) and $ff;
  PpvUInt8Array(aOutData)^[Index+Count]:=(Delta shr 16) and $ff;
  PpvUInt8Array(aOutData)^[Index+(Count*2)]:=(Delta shr 8) and $ff;
  PpvUInt8Array(aOutData)^[Index+(Count*3)]:=(Delta shr 0) and $ff;
 end;
end;

procedure ForwardTransform32BitFloatData(const aStream:TStream);
var InData,OutData:Pointer;
    Size:TpvSizeInt;
begin
 if assigned(aStream) then begin
  Size:=aStream.Size;
  if Size>0 then begin
   if aStream is TMemoryStream then begin
    GetMem(OutData,Size);
    try
     ForwardTransform32BitFloatData(TMemoryStream(aStream).Memory,OutData,Size);
     Move(OutData^,TMemoryStream(aStream).Memory^,Size);
    finally
     try
      FreeMem(OutData);
     finally
      OutData:=nil;
     end;
    end;
   end else begin
    GetMem(InData,Size);
    try
     aStream.Seek(0,soBeginning);
     aStream.ReadBuffer(InData^,Size);
     GetMem(OutData,Size);
     try
      ForwardTransform32BitFloatData(InData,OutData,Size);
      aStream.Seek(0,soBeginning);
      aStream.WriteBuffer(OutData^,Size);
     finally
      try
       FreeMem(OutData);
      finally
       OutData:=nil;
      end;
     end;
    finally
     try
      FreeMem(InData);
     finally
      InData:=nil;
     end;
    end;
   end;
  end;
 end;
end;

// This function transforms 32-bit float data back from a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure BackwardTransform32BitFloatData(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt);
var Index,Count:TpvSizeInt;
    Value:TpvUInt32;
begin
 Count:=aDataSize shr 2;
 Value:=0;
 for Index:=0 to Count-1 do begin
  inc(Value,(TpvUInt32(PpvUInt8Array(aInData)^[Index]) shl 24) or
            (TpvUInt32(PpvUInt8Array(aInData)^[Index+Count]) shl 16) or
            (TpvUInt32(PpvUInt8Array(aInData)^[Index+(Count*2)]) shl 8) or
            (TpvUInt32(PpvUInt8Array(aInData)^[Index+(Count*3)]) shl 0));
  PpvUInt32Array(aOutData)^[Index]:=Value xor TpvUInt32(TpvUInt32(TpvUInt32(Value shr 31)-1) or TpvUInt32($80000000));
 end;
end;

procedure BackwardTransform32BitFloatData(const aStream:TStream);
var InData,OutData:Pointer;
    Size:TpvSizeInt;
begin
 if assigned(aStream) then begin
  Size:=aStream.Size;
  if Size>0 then begin
   if aStream is TMemoryStream then begin
    GetMem(OutData,Size);
    try
     BackwardTransform32BitFloatData(TMemoryStream(aStream).Memory,OutData,Size);
     Move(OutData^,TMemoryStream(aStream).Memory^,Size);
    finally
     try
      FreeMem(OutData);
     finally
      OutData:=nil;
     end;
    end;
   end else begin
    GetMem(InData,Size);
    try
     aStream.Seek(0,soBeginning);
     aStream.ReadBuffer(InData^,Size);
     GetMem(OutData,Size);
     try
      BackwardTransform32BitFloatData(InData,OutData,Size);
      aStream.Seek(0,soBeginning);
      aStream.WriteBuffer(OutData^,Size);
     finally
      try
       FreeMem(OutData);
      finally
       OutData:=nil;
      end;
     end;
    finally
     try
      FreeMem(InData);
     finally
      InData:=nil;
     end;
    end;
   end;
  end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////////////

{$ifdef cpuamd64}
procedure ForwardTransformRGBA8DataAMD64(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt); assembler; {$ifdef fpc}nostackframe; ms_abi_default;{$endif}
const Data:array[0..15] of TpvUInt8=(0,4,8,12,0,0,0,0,0,0,0,0,0,0,0,0);
asm
{$ifndef fpc}
 .noframe
{$endif}
 cmp r8,4
 jb @LBB0_15
 push r15
 push r14
 push r13
 push r12
 push rsi
 push rdi
 push rbp
 push rbx
 sub rsp,88
 movdqa xmmword ptr [rsp+64],xmm9
 movdqa xmmword ptr [rsp+48],xmm8
 movdqa xmmword ptr [rsp+32],xmm7
 movdqa xmmword ptr [rsp+16],xmm6
 mov rax,r8
 shr rax,2
 lea r13,[rax+rax]
 lea r10,[rax+rax*2]
 cmp r8,48
 jae @LBB0_3
 xor r8d,r8d
@LBB0_4:
 xor r9d,r9d
 xor edi,edi
 xor ebx,ebx
 xor r11d,r11d
@LBB0_12:
 lea rsi,[rdx+r11]
 add r10,rsi
 mov qword ptr [rsp+8],rsi
 add r13,rsi
 lea r15,[r11+rax]
 sub rax,r11
 add r15,rdx
 lea rcx,[rcx+r11*4]
 xor edx,edx
@LBB0_13:
 mov r11d,dword ptr [rcx+rdx*4]
 mov ebp,r11d
 mov r12d,r11d
 mov r14,rax
 mov rax,r10
 mov r10,r13
 mov r13d,r11d
 mov esi,r11d
 shr esi,8
 shr ebp,16
 shr r12d,24
 sub r13d,r8d
 mov r8d,esi
 sub r8d,r9d
 mov r9d,ebp
 sub r9d,edi
 mov edi,r12d
 sub edi,ebx
 mov rbx,qword ptr [rsp+8]
 mov byte ptr [rbx+rdx],r13b
 mov r13,r10
 mov r10,rax
 mov rax,r14
 mov byte ptr [r15+rdx],r8b
 mov byte ptr [r13+rdx],r9b
 mov byte ptr [r10+rdx],dil
 mov r8d,r11d
 inc rdx
 mov r9d,esi
 mov edi,ebp
 mov ebx,r12d
 cmp r14,rdx
 jne @LBB0_13
@LBB0_14:
 movaps xmm6,xmmword ptr [rsp+16]
 movaps xmm7,xmmword ptr [rsp+32]
 movaps xmm8,xmmword ptr [rsp+48]
 movaps xmm9,xmmword ptr [rsp+64]
 add rsp,88
 pop rbx
 pop rbp
 pop rdi
 pop rsi
 pop r12
 pop r13
 pop r14
 pop r15
@LBB0_15:
 jmp @Exit
@LBB0_3:
 lea rsi,[rdx+r10]
 and r8,-4
 lea r11,[rdx+r8]
 add r8,rcx
 lea rdi,[rdx+r13]
 lea r14,[rdx+rax]
 cmp rsi,r8
 setb r15b
 cmp rcx,r11
 setb r12b
 cmp rdi,r8
 setb r11b
 cmp rcx,rsi
 setb sil
 cmp r14,r8
 setb bl
 cmp rcx,rdi
 setb dil
 cmp rdx,r8
 setb bpl
 cmp rcx,r14
 setb r14b
 xor r8d,r8d
 test r15b,r12b
 jne @LBB0_4
 and r11b,sil
 jne @LBB0_4
 and bl,dil
 jne @LBB0_4
 mov r9d,0
 mov edi,0
 mov ebx,0
 mov r11d,0
 and bpl,r14b
 jne @LBB0_12
 mov r11,rax
 and r11,-4
 pxor xmm0,xmm0
 movd xmm1,dword ptr [rip+Data]
 mov r8,rcx
 mov rsi,rdx
 mov rdi,r11
 pxor xmm2,xmm2
 pxor xmm3,xmm3
 pxor xmm4,xmm4
@LBB0_9:
 movdqa xmm5,xmm4
 movdqa xmm4,xmm3
 movdqa xmm3,xmm2
 movdqa xmm2,xmm0
 movdqu xmm0,xmmword ptr [r8]
 movdqa xmm6,xmm0
 palignr xmm6,xmm2,12
 movdqa xmm2,xmm0
 psrld xmm2,8
 movdqa xmm7,xmm2
 palignr xmm7,xmm3,12
 movdqa xmm3,xmm0
 psrld xmm3,16
 movdqa xmm8,xmm3
 palignr xmm8,xmm4,12
 movdqa xmm4,xmm0
 psrld xmm4,24
 movdqa xmm9,xmm4
 palignr xmm9,xmm5,12
 movdqa xmm5,xmm0
 psubd xmm5,xmm6
 pshufb xmm5,xmm1
 movdqa xmm6,xmm2
 psubd xmm6,xmm7
 pshufb xmm6,xmm1
 movdqa xmm7,xmm3
 psubd xmm7,xmm8
 pshufb xmm7,xmm1
 movdqa xmm8,xmm4
 psubd xmm8,xmm9
 pshufb xmm8,xmm1
 movd dword ptr [rsi],xmm5
 movd dword ptr [rsi+rax],xmm6
 movd dword ptr [rsi+rax*2],xmm7
 movd dword ptr [rsi+r10],xmm8
 add rsi,4
 add r8,16
 add rdi,-4
 jne @LBB0_9
 cmp rax,r11
 je @LBB0_14
 pextrd r8d,xmm0,3
 pextrd r9d,xmm2,3
 pextrd edi,xmm3,3
 pextrd ebx,xmm4,3
 jmp @LBB0_12
 @Exit:
end;
{$endif}

// This function transforms RGBA8 data to a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure ForwardTransformRGBA8Data(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt);
{$ifdef cpuamd64}
begin
 ForwardTransformRGBA8DataAMD64(aInData,aOutData,aDataSize);
end;
{$else}
var Index,Count:TpvSizeInt;
    Value:TpvUInt32;
    PreviousR,ValueR,DeltaR,
    PreviousG,ValueG,DeltaG,
    PreviousB,ValueB,DeltaB,
    PreviousA,ValueA,DeltaA:TpvUInt8;
begin
 Count:=aDataSize shr 2;
 PreviousR:=0;
 PreviousG:=0;
 PreviousB:=0;
 PreviousA:=0;
 for Index:=0 to Count-1 do begin
  Value:=PpvUInt32Array(aInData)^[Index];
  ValueR:=(Value shr 0) and $ff;
  ValueG:=(Value shr 8) and $ff;
  ValueB:=(Value shr 16) and $ff;
  ValueA:=(Value shr 24) and $ff;
  DeltaR:=ValueR-PreviousR;
  DeltaG:=ValueG-PreviousG;
  DeltaB:=ValueB-PreviousB;
  DeltaA:=ValueA-PreviousA;
  PreviousR:=ValueR;
  PreviousG:=ValueG;
  PreviousB:=ValueB;
  PreviousA:=ValueA;
  PpvUInt8Array(aOutData)^[Index]:=DeltaR;
  PpvUInt8Array(aOutData)^[Index+Count]:=DeltaG;
  PpvUInt8Array(aOutData)^[Index+(Count*2)]:=DeltaB;
  PpvUInt8Array(aOutData)^[Index+(Count*3)]:=DeltaA;
 end;
end;
{$endif}

procedure ForwardTransformRGBA8Data(const aStream:TStream);
var InData,OutData:Pointer;
    Size:TpvSizeInt;
begin
 if assigned(aStream) then begin
  Size:=aStream.Size;
  if Size>0 then begin
   if aStream is TMemoryStream then begin
    GetMem(OutData,Size);
    try
     ForwardTransformRGBA8Data(TMemoryStream(aStream).Memory,OutData,Size);
     Move(OutData^,TMemoryStream(aStream).Memory^,Size);
    finally
     try
      FreeMem(OutData);
     finally
      OutData:=nil;
     end;
    end;
   end else begin
    GetMem(InData,Size);
    try
     aStream.Seek(0,soBeginning);
     aStream.ReadBuffer(InData^,Size);
     GetMem(OutData,Size);
     try
      ForwardTransform32BitFloatData(InData,OutData,Size);
      aStream.Seek(0,soBeginning);
      aStream.WriteBuffer(OutData^,Size);
     finally
      try
       FreeMem(OutData);
      finally
       OutData:=nil;
      end;
     end;
    finally
     try
      FreeMem(InData);
     finally
      InData:=nil;
     end;
    end;
   end;
  end;
 end;
end;

{$ifdef cpuamd64}
procedure BackwardTransformRGBA8DataAMD64(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt); assembler; {$ifdef fpc}nostackframe; ms_abi_default;{$endif}
asm
{$ifndef fpc}
 .noframe
{$endif}
 cmp r8,4
 jb @Exit
 push r15
 push r14
 push rsi
 push rdi
 push rbp
 push rbx
 shr r8,2
 lea rax,[r8+r8*2]
 mov r9,r8
 neg r9
 add rax,rcx
 lea r10,[rcx+r8*2]
 add r8,rcx
 xor r11d,r11d
 xor esi,esi
 xor edi,edi
 xor ebx,ebx
 xor ebp,ebp
@Loop:
 movzx r14d,byte ptr [rcx+r11]
 add ebp,r14d
 movzx r14d,byte ptr [r8+r11]
 movzx ebx,bl
 add ebx,r14d
 movzx r14d,byte ptr [r10+r11]
 movzx edi,dil
 add edi,r14d
 movzx r14d,byte ptr [rax+r11]
 add esi,r14d
 movzx r14d,bpl
 mov r15d,ebx
 shl r15d,8
 movzx r15d,r15w
 or r15d,r14d
 movzx r14d,dil
 shl r14d,16
 or r14d,r15d
 mov r15d,esi
 shl r15d,24
 or r15d,r14d
 mov dword ptr [rdx+r11*4],r15d
 inc r11
 mov r14,r9
 add r14,r11
 jne @Loop
 pop rbx
 pop rbp
 pop rdi
 pop rsi
 pop r14
 pop r15
@Exit:
end;
{$endif}

// This function transforms RGBA8 data back from a better compressible format, together with preserving the order
// before and after the transformation for better delta compression
procedure BackwardTransformRGBA8Data(const aInData,aOutData:pointer;const aDataSize:TpvSizeInt);
{$ifdef cpuamd64}
begin
 BackwardTransformRGBA8DataAMD64(aInData,aOutData,aDataSize);
end;
{$else}
var Index,Count:TpvSizeInt;
    ValueR,ValueG,ValueB,ValueA:TpvUInt8;
begin
 Count:=aDataSize shr 2;
 ValueR:=0;
 ValueG:=0;
 ValueB:=0;
 ValueA:=0;
 for Index:=0 to Count-1 do begin
  inc(ValueR,TpvUInt8(PpvUInt8Array(aInData)^[Index]));
  inc(ValueG,TpvUInt8(PpvUInt8Array(aInData)^[Index+Count]));
  inc(ValueB,TpvUInt8(PpvUInt8Array(aInData)^[Index+(Count*2)]));
  inc(ValueA,TpvUInt8(PpvUInt8Array(aInData)^[Index+(Count*3)]));
  PpvUInt32Array(aOutData)^[Index]:=((TpvUInt32(ValueR) and $ff) shl 0) or
                                    ((TpvUInt32(ValueG) and $ff) shl 8) or
                                    ((TpvUInt32(ValueB) and $ff) shl 16) or
                                    ((TpvUInt32(ValueA) and $ff) shl 24);
 end;
end;
{$endif}

procedure BackwardTransformRGBA8Data(const aStream:TStream);
var InData,OutData:Pointer;
    Size:TpvSizeInt;
begin
 if assigned(aStream) then begin
  Size:=aStream.Size;
  if Size>0 then begin
   if aStream is TMemoryStream then begin
    GetMem(OutData,Size);
    try
     BackwardTransformRGBA8Data(TMemoryStream(aStream).Memory,OutData,Size);
     Move(OutData^,TMemoryStream(aStream).Memory^,Size);
    finally
     try
      FreeMem(OutData);
     finally
      OutData:=nil;
     end;
    end;
   end else begin
    GetMem(InData,Size);
    try
     aStream.Seek(0,soBeginning);
     aStream.ReadBuffer(InData^,Size);
     GetMem(OutData,Size);
     try
      BackwardTransform32BitFloatData(InData,OutData,Size);
      aStream.Seek(0,soBeginning);
      aStream.WriteBuffer(OutData^,Size);
     finally
      try
       FreeMem(OutData);
      finally
       OutData:=nil;
      end;
     end;
    finally
     try
      FreeMem(InData);
     finally
      InData:=nil;
     end;
    end;
   end;
  end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////////////

type TCompressedFileSignature=array[0..3] of AnsiChar;

     TCompressedFileHeader=packed record
      Signature:TCompressedFileSignature;
      Version:TpvUInt32;
      Flags:TpvUInt8; // Yet unused, but reserved for future use, better to have it now than to break the file format later 
      Parts:TpvUInt8; // max 255 parts, should be enough
      CompressionMethod:TpvUInt8;
      CompressionLevel:TpvUInt8;
      UncompressedSize:TpvUInt64;
      CompressedSize:TpvUInt64;
     end;
     PCompressedFileHeader=^TCompressedFileHeader;

     TCompressionPartHeader=packed record
      CompressedOffset:TpvUInt64;
      CompressedSize:TpvUInt64;
      UncompressedOffset:TpvUInt64;
      UncompressedSize:TpvUInt64;
     end;
     PCompressionPartHeader=^TCompressionPartHeader;

     TCompressionPartHeaders=array of TCompressionPartHeader;

const CompressedFileSignature:TCompressedFileSignature=('C','O','F','I');

      CompressedFileVersion=2;

type TCompressionPartJob=record
      FileHeader:PCompressedFileHeader;
      CompressionPartHeader:PCompressionPartHeader;
      InData:Pointer;
      InSize:TpvUInt64;
      OutData:Pointer;
      OutSize:TpvUInt64;
      Success:boolean;
     end;
     PCompressionPartJob=^TCompressionPartJob;

     TCompressionPartJobs=array of TCompressionPartJob;

     TDecompressionPartJob=record
      FileHeader:PCompressedFileHeader;
      CompressionPartHeader:PCompressionPartHeader;
      InData:Pointer;
      InSize:TpvUInt64;
      OutData:Pointer;
      OutSize:TpvUInt64;
      Success:boolean;
     end;
     PDecompressionPartJob=^TDecompressionPartJob;

     TDecompressionPartJobs=array of TDecompressionPartJob;

procedure CompressPart(const aJob:PCompressionPartJob);
{var Stream:TFileStream;
    FileName:TpvUTF8String;
    TemporaryOutData:Pointer;
    TemporaryOutDataSize:TpvUInt64;//}
var OutSize:TpvSizeUInt;
begin

 aJob^.OutData:=nil;
 aJob^.OutSize:=0;

 case aJob.FileHeader^.CompressionMethod of
  TpvUInt8(TpvCompressionMethod.None):begin
   if aJob.InSize>0 then begin
    GetMem(aJob.OutData,aJob.InSize);
    aJob.OutSize:=aJob.InSize;
    Move(aJob.InData^,aJob.OutData^,aJob.InSize);
    aJob.Success:=true;
   end else begin
    aJob.Success:=false;
   end;
  end;
  TpvUInt8(TpvCompressionMethod.Deflate):begin
   OutSize:=aJob.OutSize;
   aJob.Success:=DoDeflate(aJob.InData,aJob.InSize,aJob.OutData,OutSize,TpvDeflateMode(aJob.FileHeader^.CompressionLevel),false);
   aJob.OutSize:=OutSize;
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSF):begin
   aJob.Success:=LZBRSFCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRSFLevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRRC):begin
   aJob.Success:=LZBRRCCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRRCLevel(aJob.FileHeader^.CompressionLevel),false);
{  // Safe check by decompressing the compressed data again and compare it with the original data
   if aJob.Success then begin
    GetMem(TemporaryOutData,aJob.CompressionPartHeader^.UncompressedSize);
    try
     TemporaryOutDataSize:=0;
     aJob.Success:=LZBRRCDecompress(aJob.OutData,aJob.OutSize,TemporaryOutData,TemporaryOutDataSize,aJob.CompressionPartHeader^.UncompressedSize,false);
     if aJob.Success then begin
      aJob.Success:=CompareMem(TemporaryOutData,aJob.InData,Min(TemporaryOutDataSize,aJob.CompressionPartHeader^.UncompressedSize));
     end;
     if not aJob.Success then begin
      FileName:='broken_'+IntToHex(TpvPtrUInt(aJob),16)+'_'+IntToHex(random($ffffffff),8);
      Stream:=TFileStream.Create(FileName+'_input.bin',fmCreate);
      try
       Stream.WriteBuffer(aJob.InData^,aJob.InSize);
      finally
       FreeAndNil(Stream);
      end;
      Stream:=TFileStream.Create(FileName+'_output.bin',fmCreate);
      try
       Stream.WriteBuffer(aJob.OutData^,aJob.OutSize);
      finally
       FreeAndNil(Stream);
      end;
      Stream:=TFileStream.Create(FileName+'_decompressed.bin',fmCreate);
      try
       Stream.WriteBuffer(TemporaryOutData^,TemporaryOutDataSize);
      finally
       FreeAndNil(Stream);
      end;
     end;
    finally
     FreeMem(TemporaryOutData);
     TemporaryOutData:=nil;
    end;
   end;//}
  end;
  TpvUInt8(TpvCompressionMethod.LZMA):begin
   aJob.Success:=LZMACompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZMALevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRS):begin
   aJob.Success:=LZBRSCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRSLevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSX):begin
   aJob.Success:=LZBRSXCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRSXLevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  else begin
   aJob.Success:=false;
  end;
 end;

 aJob^.CompressionPartHeader^.CompressedSize:=aJob^.OutSize;

end;     

procedure CompressPartJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:TpvSizeInt;
    JobData:PCompressionPartJob;
begin
 for Index:=FromIndex to ToIndex do begin
  JobData:=Data;
  inc(JobData,Index);
  CompressPart(JobData);
 end;
end;

function CompressStream(const aInStream:TStream;const aOutStream:TStream;const aCompressionMethod:TpvCompressionMethod;const aCompressionLevel:TpvUInt32;const aParts:TpvUInt32):boolean;
var InData:Pointer;
    InSize,Offset,Size,RemainSize,PartSize:TpvUInt64;
    FileHeader:TCompressedFileHeader;
    CompressionPartHeaders:TCompressionPartHeaders;
    CompressionPartHeader:PCompressionPartHeader;
    CompressionPartJobs:TCompressionPartJobs;
    CompressionPartJob:PCompressionPartJob;
    PartIndex:TpvSizeInt;    
begin

 if assigned(aInStream) and (aInStream.Size>0) and assigned(aOutStream) then begin

  GetMem(InData,aInStream.Size);
  try

   aInStream.Seek(0,soBeginning);
   aInStream.ReadBuffer(InData^,aInStream.Size);

   InSize:=aInStream.Size;

   FileHeader.Signature:=CompressedFileSignature;
   FileHeader.Version:=CompressedFileVersion;
   FileHeader.Flags:=0; // Zero for now, because it is reserved for future use
   if (InSize<65536) or (aCompressionMethod=TpvCompressionMethod.None) then begin
    FileHeader.Parts:=1;
   end else begin
    if aParts=0 then begin
     FileHeader.Parts:=Min(Max(IntLog264(InSize)-21,1),255); // minimum 1, maximum 255 parts
    end else begin
     FileHeader.Parts:=Min(Max(aParts,1),255); // minimum 1, maximum 255 parts
    end;
   end;
   FileHeader.CompressionMethod:=TpvUInt8(aCompressionMethod);
   FileHeader.CompressionLevel:=aCompressionLevel;
   FileHeader.UncompressedSize:=InSize;

   CompressionPartHeaders:=nil;
   try

    SetLength(CompressionPartHeaders,FileHeader.Parts);

    Offset:=0;
    RemainSize:=InSize;
    PartSize:=TpvUInt64(Max(TpvUInt64(1),TpvUInt64((InSize+(FileHeader.Parts-1)) div FileHeader.Parts)));
    for PartIndex:=0 to FileHeader.Parts-1 do begin
     if (PartIndex=(FileHeader.Parts-1)) or (RemainSize<PartSize) then begin
      Size:=RemainSize;
     end else begin
      Size:=PartSize;
     end;
     CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
     CompressionPartHeader^.CompressedOffset:=0;
     CompressionPartHeader^.CompressedSize:=0;
     CompressionPartHeader^.UncompressedOffset:=Offset;
     CompressionPartHeader^.UncompressedSize:=Size;
     inc(Offset,Size);
     dec(RemainSize,Size);
    end;

    CompressionPartJobs:=nil;
    try
     
     SetLength(CompressionPartJobs,FileHeader.Parts);
     for PartIndex:=0 to FileHeader.Parts-1 do begin
      CompressionPartJob:=@CompressionPartJobs[PartIndex];
      CompressionPartJob^.FileHeader:=@FileHeader;
      CompressionPartJob^.CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
      CompressionPartJob^.InData:=Pointer(TpvPtrUInt(TpvPtrUInt(InData)+TpvPtrUInt(CompressionPartHeaders[PartIndex].UncompressedOffset)));
      CompressionPartJob^.InSize:=CompressionPartHeaders[PartIndex].UncompressedSize;
      CompressionPartJob^.OutData:=nil;
      CompressionPartJob^.OutSize:=0;
      CompressionPartJob^.Success:=false;
     end;

     if FileHeader.Parts>1 then begin
      if assigned(pvCompressionPasMPInstance) then begin
       // Use multiple threads for multiple parts
       pvCompressionPasMPInstance.Invoke(pvCompressionPasMPInstance.ParallelFor(@CompressionPartJobs[0],0,length(CompressionPartJobs)-1,CompressPartJob,1,PasMPDefaultDepth,nil,0,0));
      end else begin
       for PartIndex:=0 to FileHeader.Parts-1 do begin
        CompressPart(@CompressionPartJobs[PartIndex]);
       end;
      end;
     end else begin
      // No need to use multiple threads for only one part
      CompressPart(@CompressionPartJobs[0]);
     end;

     result:=true;
     Offset:=SizeOf(TCompressedFileHeader)+(FileHeader.Parts*SizeOf(TCompressionPartHeader));
     Size:=0;
     for PartIndex:=0 to FileHeader.Parts-1 do begin
      CompressionPartJob:=@CompressionPartJobs[PartIndex];
      if CompressionPartJob^.Success and assigned(CompressionPartJob^.OutData) and (CompressionPartJob^.OutSize>0) then begin
       CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
       CompressionPartHeader^.CompressedOffset:=Offset;
       CompressionPartHeader^.CompressedSize:=CompressionPartJob^.OutSize;
       inc(Offset,CompressionPartHeader^.CompressedSize);
       inc(Size,CompressionPartHeader^.CompressedSize);
      end else begin
       result:=false;
       break;
      end;
     end;

     FileHeader.CompressedSize:=Size;

     if result then begin
      
      aOutStream.WriteBuffer(FileHeader,SizeOf(TCompressedFileHeader));

      for PartIndex:=0 to FileHeader.Parts-1 do begin
       CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
       aOutStream.WriteBuffer(CompressionPartHeader^,SizeOf(TCompressionPartHeader));
      end;

      for PartIndex:=0 to FileHeader.Parts-1 do begin
       CompressionPartJob:=@CompressionPartJobs[PartIndex];
       if assigned(CompressionPartJob^.OutData) and (CompressionPartJob^.OutSize>0) then begin
        aOutStream.WriteBuffer(CompressionPartJob^.OutData^,CompressionPartJob^.OutSize);
       end;
      end;
      
     end;

     for PartIndex:=0 to FileHeader.Parts-1 do begin
      CompressionPartJob:=@CompressionPartJobs[PartIndex];
      if assigned(CompressionPartJob^.OutData) then begin
       FreeMem(CompressionPartJob^.OutData);
       CompressionPartJob^.OutData:=nil;
      end;
     end;

    finally
     CompressionPartJobs:=nil;
    end;

   finally
    CompressionPartHeaders:=nil;
   end; 

  finally
   FreeMem(InData);
   InData:=nil;
  end;

 end else begin

  result:=false;

 end;

end;

procedure DecompressPart(const aJob:PDecompressionPartJob);
var TemporaryDeflateOutData:Pointer; // Deflate isn't in-place, so we need a temporary buffer
    TemporaryDeflateOutSize:TpvSizeUInt;
begin 

 case aJob.FileHeader^.CompressionMethod of
  TpvUInt8(TpvCompressionMethod.None):begin
   // No compression, just copy the data
   if aJob.InSize>0 then begin
    GetMem(aJob.OutData,aJob.InSize);
    aJob.OutSize:=aJob.InSize;
    Move(aJob.InData^,aJob.OutData^,aJob.InSize);
    aJob.Success:=true;
   end else begin
    aJob.Success:=false;
   end;
  end;
  TpvUInt8(TpvCompressionMethod.Deflate):begin
   // The old good Deflate, it is slow and it doesn't compress very well in addition to that, in comparison to LZBRRC and LZMA
   TemporaryDeflateOutData:=nil;
   TemporaryDeflateOutSize:=0;
   try
    aJob.Success:=DoInflate(aJob.InData,aJob.InSize,TemporaryDeflateOutData,TemporaryDeflateOutSize,false);
    if aJob.Success then begin
     if TemporaryDeflateOutSize=aJob.CompressionPartHeader^.UncompressedSize then begin
      GetMem(aJob.OutData,TemporaryDeflateOutSize);
      aJob.OutSize:=TemporaryDeflateOutSize;
      Move(TemporaryDeflateOutData^,aJob.OutData^,TemporaryDeflateOutSize);
     end else begin
      aJob.Success:=false;
     end;
    end;
   finally
    if assigned(TemporaryDeflateOutData) then begin
     FreeMem(TemporaryDeflateOutData);
     TemporaryDeflateOutData:=nil;
    end;
   end;
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSF):begin
   // LZBRSF is a pure byte-wise compression algorithm, so it is pretty fast, but it doesn't compress very well, but it is still better than 
   // nothing, and it is also very fast at decompression, so it is a pretty good choice for games, where the compression ratio isn't that important, 
   // only the decompression speed.
   aJob.Success:=LZBRSFDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRRC):begin
   // LZBRRC is a LZ77-based compression algorithm but with range coding for the entropy coding, so it is pretty fast and it does also 
   // compress very well, but LZMA is still better at the compression ratio. LZBRRC is also very fast at decompression, so it is a good
   // choice for games, because it is fast at both compression and decompression, and it compresses very well, so it saves a lot of space.
{$if declared(LZBRRCFastDecompress)}
   // Assembler version is faster, but not yet available for all targets, and it has fewer to no sanity checks
   aJob.Success:=LZBRRCFastDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
{$else}
   // The pure pas version is slower, but available for all targets, and it has full sanity checks
   aJob.Success:=LZBRRCDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
{$ifend}
  end;
  TpvUInt8(TpvCompressionMethod.LZMA):begin
   // The old good LZMA, but it is slow, but it compresses very well. It should be used for data, where the compression ratio is more important 
   // than the decompression speed.  
   aJob.Success:=LZMADecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRS):begin
   // LZBRS is a simple LZ77/LZSS-style algorithm like BriefLZ, but with 32-bit tags instead 16-bit tags,
   // and with end tag (match with offset 0)
   // Not to be confused with the old equal-named LRBRS from BeRoEXEPacker, which was 8-bit byte-wise tag-based.
   aJob.Success:=LZBRSDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSX):begin
   // LZBRSX is a simple LZ77/LZSS-style algorithm like apLib, but with 32-bit tags instead 8-bit tags
   aJob.Success:=LZBRSXDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  else begin
   aJob.Success:=false;
  end;
 end;

end; 

procedure DecompressPartJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:TpvSizeInt;
    JobData:PDecompressionPartJob;
begin
 for Index:=FromIndex to ToIndex do begin
  JobData:=Data;
  inc(JobData,Index);
  DecompressPart(JobData);
 end;
end;

function DecompressStream(const aInStream:TStream;const aOutStream:TStream):boolean;
var InData,OutData:Pointer;
    InSize,OutSize,Offset,Size,RemainSize,PartSize:TpvUInt64;
    PartIndex:TpvSizeInt;
    FileHeader:TCompressedFileHeader;
    CompressionPartHeaders:TCompressionPartHeaders;
    CompressionPartHeader:PCompressionPartHeader;
    DecompressionPartJobs:TDecompressionPartJobs;
    DecompressionPartJob:PDecompressionPartJob;
begin

 if assigned(aInStream) and (aInStream.Size>=SizeOf(TCompressedFileHeader)) and assigned(aOutStream) then begin

  InData:=nil;
  OutData:=nil;

  aInStream.ReadBuffer(FileHeader,SizeOf(TCompressedFileHeader));

  Size:=aInStream.Size-(SizeOf(TCompressedFileHeader)+(FileHeader.Parts*SizeOf(TCompressionPartHeader)));

  if (FileHeader.Signature=CompressedFileSignature) and
     (FileHeader.Version=CompressedFileVersion) and
     (FileHeader.CompressedSize=Size) and
     ((FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.None)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.Deflate)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZBRS)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZBRSX)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZBRSF)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZBRRC)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZMA))) then begin

   InSize:=FileHeader.CompressedSize;
   OutSize:=FileHeader.UncompressedSize;

   CompressionPartHeaders:=nil;
   try

    SetLength(CompressionPartHeaders,FileHeader.Parts);
    aInStream.ReadBuffer(CompressionPartHeaders[0],FileHeader.Parts*SizeOf(TCompressionPartHeader));

    if InSize>0 then begin

     GetMem(InData,InSize);
     try

      aInStream.ReadBuffer(InData^,InSize);
      
      if OutSize>0 then begin

       GetMem(OutData,OutSize);
       try

        DecompressionPartJobs:=nil;
        try

         SetLength(DecompressionPartJobs,FileHeader.Parts);

         Offset:=SizeOf(TCompressedFileHeader)+(FileHeader.Parts*SizeOf(TCompressionPartHeader));
         for PartIndex:=0 to FileHeader.Parts-1 do begin
          DecompressionPartJob:=@DecompressionPartJobs[PartIndex];
          DecompressionPartJob^.FileHeader:=@FileHeader;
          DecompressionPartJob^.CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
          DecompressionPartJob^.InData:=Pointer(TpvPtrUInt(TpvPtrUInt(InData)+TpvPtrUInt(DecompressionPartJob^.CompressionPartHeader^.CompressedOffset-Offset)));
          DecompressionPartJob^.InSize:=DecompressionPartJob^.CompressionPartHeader^.CompressedSize;
          DecompressionPartJob^.OutData:=Pointer(TpvPtrUInt(TpvPtrUInt(OutData)+TpvPtrUInt(DecompressionPartJob^.CompressionPartHeader^.UncompressedOffset)));
          DecompressionPartJob^.OutSize:=DecompressionPartJob^.CompressionPartHeader^.UncompressedSize;
          DecompressionPartJob^.Success:=false;
         end;

         if FileHeader.Parts>1 then begin
          if assigned(pvCompressionPasMPInstance) then begin
           // Use multiple threads for multiple parts
           pvCompressionPasMPInstance.Invoke(pvCompressionPasMPInstance.ParallelFor(@DecompressionPartJobs[0],0,length(DecompressionPartJobs)-1,DecompressPartJob,1,PasMPDefaultDepth,nil,0,0));
          end else begin
           for PartIndex:=0 to FileHeader.Parts-1 do begin
            DecompressPart(@DecompressionPartJobs[PartIndex]);
           end;
          end;
         end else begin
          // No need to use multiple threads for only one part
          DecompressPart(@DecompressionPartJobs[0]);
         end;

         result:=true;
         for PartIndex:=0 to FileHeader.Parts-1 do begin
          DecompressionPartJob:=@DecompressionPartJobs[PartIndex];
          if DecompressionPartJob^.Success and assigned(DecompressionPartJob^.OutData) and (DecompressionPartJob^.OutSize>0) then begin
           aOutStream.WriteBuffer(DecompressionPartJob^.OutData^,DecompressionPartJob^.OutSize);
          end else begin
           result:=false;
           break;
          end;
         end;

        finally
         DecompressionPartJobs:=nil;
        end;

       finally
        FreeMem(OutData);
        OutData:=nil;
       end; 

      end;

     finally
      FreeMem(InData);
      InData:=nil;
     end; 

    end; 

   finally
    CompressionPartHeaders:=nil;
   end;   

  end else begin

   result:=false;

  end;

 end else begin

  result:=false;

 end;

end;

end.
