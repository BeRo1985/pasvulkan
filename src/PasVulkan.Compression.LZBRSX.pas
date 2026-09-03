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
unit PasVulkan.Compression.LZBRSX;
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

{$ifdef fpc}
 {$optimization off}
 {$optimization level1}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Math,
     PasVulkan.Types;

// LZBRSX is a simple LZ77/LZSS-style algorithm like apLib, but with 32-bit tags instead 8-bit tags

type TpvLZBRSXLevel=0..9;
     PpvLZBRSXLevel=^TpvLZBRSXLevel;

function LZBRSXCompress(const aInData:TpvPointer;const aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64;const aLevel:TpvLZBRSXLevel=5;const aWithSize:boolean=true):boolean;

function LZBRSXDecompress(const aInData:TpvPointer;aInLen:TpvUInt64;var aDestData:TpvPointer;out aDestLen:TpvUInt64;const aOutputSize:TpvInt64=-1;const aWithSize:boolean=true):boolean;

implementation

// Three variants of the very same bit stream live in this unit and only one of them is ever
// compiled. On x86-64 the assembler cores are used, which are the clang output of the C form of the
// loops below. Otherwise OptimizedVariant selects the restructured Pascal, and without either of
// them the original stays in use. All three produce byte identical output.

// The two lookup tables and the small helpers are shared by the assembler cores and by the
// optimized Pascal, the original variant further down keeps its own local copies untouched.
{$if defined(cpux86_64) or defined(cpux64) or defined(OptimizedVariant)}
const HashBits=16;
      HashSize=1 shl HashBits;
      HashMask=HashSize-1;
      HashShift=32-HashBits;
      WindowSize=32768;
      WindowMask=WindowSize-1;
      MinMatch=3;
      MaxMatch=258;
      MaxOffset=TpvUInt32($7fffffff);
      EmptySlot=TpvUInt32($ffffffff); // The tables hold offsets instead of pointers now, so nil needs a stand-in

      // Source pointer fixups for the pattern expansion at match offsets below eight, so that the
      // copy can go on in eight byte steps afterwards
      SmallOffsetIncTable:array[0..7] of TpvInt32=(0,1,2,1,0,4,4,4);
      SmallOffsetAdjustTable:array[0..7] of TpvInt32=(0,0,0,1,4,-1,-2,-3);

      GammaEncodeLookUpTable:array[0..511,0..1] of TpvUInt32=
       (
        (0,0),
        (0,0),
        ($00,2),($02,2),
        ($04,4),($06,4),($0c,4),($0e,4),
        ($14,6),($16,6),($1c,6),($1e,6),
        ($34,6),($36,6),($3c,6),($3e,6),
        ($54,8),($56,8),($5c,8),($5e,8),
        ($74,8),($76,8),($7c,8),($7e,8),
        ($d4,8),($d6,8),($dc,8),($de,8),
        ($f4,8),($f6,8),($fc,8),($fe,8),
        ($154,10),($156,10),($15c,10),($15e,10),
        ($174,10),($176,10),($17c,10),($17e,10),
        ($1d4,10),($1d6,10),($1dc,10),($1de,10),
        ($1f4,10),($1f6,10),($1fc,10),($1fe,10),
        ($354,10),($356,10),($35c,10),($35e,10),
        ($374,10),($376,10),($37c,10),($37e,10),
        ($3d4,10),($3d6,10),($3dc,10),($3de,10),
        ($3f4,10),($3f6,10),($3fc,10),($3fe,10),
        ($554,12),($556,12),($55c,12),($55e,12),
        ($574,12),($576,12),($57c,12),($57e,12),
        ($5d4,12),($5d6,12),($5dc,12),($5de,12),
        ($5f4,12),($5f6,12),($5fc,12),($5fe,12),
        ($754,12),($756,12),($75c,12),($75e,12),
        ($774,12),($776,12),($77c,12),($77e,12),
        ($7d4,12),($7d6,12),($7dc,12),($7de,12),
        ($7f4,12),($7f6,12),($7fc,12),($7fe,12),
        ($d54,12),($d56,12),($d5c,12),($d5e,12),
        ($d74,12),($d76,12),($d7c,12),($d7e,12),
        ($dd4,12),($dd6,12),($ddc,12),($dde,12),
        ($df4,12),($df6,12),($dfc,12),($dfe,12),
        ($f54,12),($f56,12),($f5c,12),($f5e,12),
        ($f74,12),($f76,12),($f7c,12),($f7e,12),
        ($fd4,12),($fd6,12),($fdc,12),($fde,12),
        ($ff4,12),($ff6,12),($ffc,12),($ffe,12),
        ($1554,14),($1556,14),($155c,14),($155e,14),
        ($1574,14),($1576,14),($157c,14),($157e,14),
        ($15d4,14),($15d6,14),($15dc,14),($15de,14),
        ($15f4,14),($15f6,14),($15fc,14),($15fe,14),
        ($1754,14),($1756,14),($175c,14),($175e,14),
        ($1774,14),($1776,14),($177c,14),($177e,14),
        ($17d4,14),($17d6,14),($17dc,14),($17de,14),
        ($17f4,14),($17f6,14),($17fc,14),($17fe,14),
        ($1d54,14),($1d56,14),($1d5c,14),($1d5e,14),
        ($1d74,14),($1d76,14),($1d7c,14),($1d7e,14),
        ($1dd4,14),($1dd6,14),($1ddc,14),($1dde,14),
        ($1df4,14),($1df6,14),($1dfc,14),($1dfe,14),
        ($1f54,14),($1f56,14),($1f5c,14),($1f5e,14),
        ($1f74,14),($1f76,14),($1f7c,14),($1f7e,14),
        ($1fd4,14),($1fd6,14),($1fdc,14),($1fde,14),
        ($1ff4,14),($1ff6,14),($1ffc,14),($1ffe,14),
        ($3554,14),($3556,14),($355c,14),($355e,14),
        ($3574,14),($3576,14),($357c,14),($357e,14),
        ($35d4,14),($35d6,14),($35dc,14),($35de,14),
        ($35f4,14),($35f6,14),($35fc,14),($35fe,14),
        ($3754,14),($3756,14),($375c,14),($375e,14),
        ($3774,14),($3776,14),($377c,14),($377e,14),
        ($37d4,14),($37d6,14),($37dc,14),($37de,14),
        ($37f4,14),($37f6,14),($37fc,14),($37fe,14),
        ($3d54,14),($3d56,14),($3d5c,14),($3d5e,14),
        ($3d74,14),($3d76,14),($3d7c,14),($3d7e,14),
        ($3dd4,14),($3dd6,14),($3ddc,14),($3dde,14),
        ($3df4,14),($3df6,14),($3dfc,14),($3dfe,14),
        ($3f54,14),($3f56,14),($3f5c,14),($3f5e,14),
        ($3f74,14),($3f76,14),($3f7c,14),($3f7e,14),
        ($3fd4,14),($3fd6,14),($3fdc,14),($3fde,14),
        ($3ff4,14),($3ff6,14),($3ffc,14),($3ffe,14),
        ($5554,16),($5556,16),($555c,16),($555e,16),
        ($5574,16),($5576,16),($557c,16),($557e,16),
        ($55d4,16),($55d6,16),($55dc,16),($55de,16),
        ($55f4,16),($55f6,16),($55fc,16),($55fe,16),
        ($5754,16),($5756,16),($575c,16),($575e,16),
        ($5774,16),($5776,16),($577c,16),($577e,16),
        ($57d4,16),($57d6,16),($57dc,16),($57de,16),
        ($57f4,16),($57f6,16),($57fc,16),($57fe,16),
        ($5d54,16),($5d56,16),($5d5c,16),($5d5e,16),
        ($5d74,16),($5d76,16),($5d7c,16),($5d7e,16),
        ($5dd4,16),($5dd6,16),($5ddc,16),($5dde,16),
        ($5df4,16),($5df6,16),($5dfc,16),($5dfe,16),
        ($5f54,16),($5f56,16),($5f5c,16),($5f5e,16),
        ($5f74,16),($5f76,16),($5f7c,16),($5f7e,16),
        ($5fd4,16),($5fd6,16),($5fdc,16),($5fde,16),
        ($5ff4,16),($5ff6,16),($5ffc,16),($5ffe,16),
        ($7554,16),($7556,16),($755c,16),($755e,16),
        ($7574,16),($7576,16),($757c,16),($757e,16),
        ($75d4,16),($75d6,16),($75dc,16),($75de,16),
        ($75f4,16),($75f6,16),($75fc,16),($75fe,16),
        ($7754,16),($7756,16),($775c,16),($775e,16),
        ($7774,16),($7776,16),($777c,16),($777e,16),
        ($77d4,16),($77d6,16),($77dc,16),($77de,16),
        ($77f4,16),($77f6,16),($77fc,16),($77fe,16),
        ($7d54,16),($7d56,16),($7d5c,16),($7d5e,16),
        ($7d74,16),($7d76,16),($7d7c,16),($7d7e,16),
        ($7dd4,16),($7dd6,16),($7ddc,16),($7dde,16),
        ($7df4,16),($7df6,16),($7dfc,16),($7dfe,16),
        ($7f54,16),($7f56,16),($7f5c,16),($7f5e,16),
        ($7f74,16),($7f76,16),($7f7c,16),($7f7e,16),
        ($7fd4,16),($7fd6,16),($7fdc,16),($7fde,16),
        ($7ff4,16),($7ff6,16),($7ffc,16),($7ffe,16),
        ($d554,16),($d556,16),($d55c,16),($d55e,16),
        ($d574,16),($d576,16),($d57c,16),($d57e,16),
        ($d5d4,16),($d5d6,16),($d5dc,16),($d5de,16),
        ($d5f4,16),($d5f6,16),($d5fc,16),($d5fe,16),
        ($d754,16),($d756,16),($d75c,16),($d75e,16),
        ($d774,16),($d776,16),($d77c,16),($d77e,16),
        ($d7d4,16),($d7d6,16),($d7dc,16),($d7de,16),
        ($d7f4,16),($d7f6,16),($d7fc,16),($d7fe,16),
        ($dd54,16),($dd56,16),($dd5c,16),($dd5e,16),
        ($dd74,16),($dd76,16),($dd7c,16),($dd7e,16),
        ($ddd4,16),($ddd6,16),($dddc,16),($ddde,16),
        ($ddf4,16),($ddf6,16),($ddfc,16),($ddfe,16),
        ($df54,16),($df56,16),($df5c,16),($df5e,16),
        ($df74,16),($df76,16),($df7c,16),($df7e,16),
        ($dfd4,16),($dfd6,16),($dfdc,16),($dfde,16),
        ($dff4,16),($dff6,16),($dffc,16),($dffe,16),
        ($f554,16),($f556,16),($f55c,16),($f55e,16),
        ($f574,16),($f576,16),($f57c,16),($f57e,16),
        ($f5d4,16),($f5d6,16),($f5dc,16),($f5de,16),
        ($f5f4,16),($f5f6,16),($f5fc,16),($f5fe,16),
        ($f754,16),($f756,16),($f75c,16),($f75e,16),
        ($f774,16),($f776,16),($f77c,16),($f77e,16),
        ($f7d4,16),($f7d6,16),($f7dc,16),($f7de,16),
        ($f7f4,16),($f7f6,16),($f7fc,16),($f7fe,16),
        ($fd54,16),($fd56,16),($fd5c,16),($fd5e,16),
        ($fd74,16),($fd76,16),($fd7c,16),($fd7e,16),
        ($fdd4,16),($fdd6,16),($fddc,16),($fdde,16),
        ($fdf4,16),($fdf6,16),($fdfc,16),($fdfe,16),
        ($ff54,16),($ff56,16),($ff5c,16),($ff5e,16),
        ($ff74,16),($ff76,16),($ff7c,16),($ff7e,16),
        ($ffd4,16),($ffd6,16),($ffdc,16),($ffde,16),
        ($fff4,16),($fff6,16),($fffc,16),($fffe,16)
       );

      GammaDecodeLookUpTable:array[0..255,0..1] of TpvUInt8=
       (
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
        (4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),
        (4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),
        (8,6),(8,6),(8,6),(8,6),
        (16,8),(16,0),(17,8),(17,0),
        (9,6),(9,6),(9,6),(9,6),
        (18,8),(18,0),(19,8),(19,0),
        (5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),
        (5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),
        (10,6),(10,6),(10,6),(10,6),
        (20,8),(20,0),(21,8),(21,0),
        (11,6),(11,6),(11,6),(11,6),
        (22,8),(22,0),(23,8),(23,0),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
        (6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),
        (6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),
        (12,6),(12,6),(12,6),(12,6),
        (24,8),(24,0),(25,8),(25,0),
        (13,6),(13,6),(13,6),(13,6),
        (26,8),(26,0),(27,8),(27,0),
        (7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),
        (7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),
        (14,6),(14,6),(14,6),(14,6),
        (28,8),(28,0),(29,8),(29,0),
        (15,6),(15,6),(15,6),(15,6),
        (30,8),(30,0),(31,8),(31,0)
       );

type PBytes=^TBytes;
     TBytes=array[0..$7ffffffe] of TpvUInt8;

     PHashTable=^THashTable;
     THashTable=array[0..HashSize-1] of TpvUInt32;

     PChainTable=^TChainTable;
     TChainTable=array[0..WindowSize-1] of TpvUInt32;
{$ifend}

{$if defined(cpux86_64) or defined(cpux64)}
{$asmmode intel}

// Both cores below are the clang output of the C form of the loops, with the Win64 register
// convention, so that the very same block works under FPC and under Delphi. They call nothing and
// they check no capacity, the caller hands them buffers which are big enough and tables which are
// already filled with EmptySlot.

type PLZBRSXCompressState=^TLZBRSXCompressState;
     TLZBRSXCompressState=record
      InData:TpvPointer;
      DestData:TpvPointer;
      HashTable:TpvPointer;
      ChainTable:TpvPointer;
      InLen:TpvUInt64;
      DestLen:TpvUInt64;
      TagPointer:TpvUInt64;
      Tag:TpvUInt32;
      BitCount:TpvUInt32;
      Level:TpvUInt32;
      Padding:TpvUInt32;
     end;

     PLZBRSXDecompressState=^TLZBRSXDecompressState;
     TLZBRSXDecompressState=record
      InData:TpvPointer;
      DestData:TpvPointer;
      InLen:TpvUInt64;
      OutputSize:TpvUInt64;
      OutputLen:TpvUInt64;
      Status:TpvUInt32;
      Padding:TpvUInt32;
     end;

procedure LZBRSXCompressCore(const aState:PLZBRSXCompressState); assembler; {$ifdef fpc}nostackframe; ms_abi_default;{$endif}
asm
{$ifndef fpc}
 .noframe
{$endif}
  push r15
  push r14
  push r13
  push r12
  push rsi
  push rdi
  push rbp
  push rbx
  sub rsp, 168
  mov rsi, qword ptr [rcx]
  mov r14, qword ptr [rcx + 8]
  mov rbp, qword ptr [rcx + 40]
  mov r13, qword ptr [rcx + 48]
  mov edi, dword ptr [rcx + 56]
  mov edx, dword ptr [rcx + 60]
  mov rax, qword ptr [rcx + 32]
  lea r8, [rsi + rax]
  mov qword ptr [rsp + 56], r8
  cmp rax, 5
  mov qword ptr [rsp + 104], rcx
  jl @LBB0_176
  mov r12, qword ptr [rcx + 16]
  mov r8, qword ptr [rcx + 24]
  mov qword ptr [rsp + 80], r8
  mov ecx, dword ptr [rcx + 64]
  mov qword ptr [rsp + 120], rcx
  add ecx, 23
  lea rax, [rsi + rax - 4]
  mov qword ptr [rsp + 64], rax
  mov eax, 1
  mov r8d, 1
  mov dword ptr [rsp + 76], ecx
  shl r8d, cl
  mov dword ptr [rsp + 24], r8d
  mov ecx, 1
  sub rcx, rsi
  mov qword ptr [rsp + 128], rcx
  sub eax, esi
  mov qword ptr [rsp + 136], rax
  mov qword ptr [rsp + 32], 0
  mov rbx, rsi
  mov dword ptr [rsp + 4], 0
  mov qword ptr [rsp + 112], r14
  mov qword ptr [rsp + 88], rsi
  mov qword ptr [rsp + 40], r12
  jmp @LBB0_4
 @LBB0_2:
  mov r10d, eax
  mov rbx, rcx
 @LBB0_3:
  mov eax, r10d
  add rax, rbx
  mov rbx, rax
  cmp rax, qword ptr [rsp + 64]
  jae @LBB0_177
 @LBB0_4:
  mov rcx, rbx
  sub rcx, rsi
  mov r10d, dword ptr [rbx]
  mov eax, r10d
  and eax, 16777215
  imul r15d, eax, 506832829
  shr r15d, 16
  mov eax, dword ptr [r12 + 4*r15]
  mov qword ptr [rsp + 96], rcx
  mov r9b, 1
  mov dword ptr [rsp + 28], eax
  sub ecx, eax
  jbe @LBB0_56
  cmp ecx, 2147483646
  ja @LBB0_56
  mov qword ptr [rsp + 48], r15
  mov rax, qword ptr [rsp + 56]
  sub rax, rbx
  mov qword ptr [rsp + 160], rax
  lea rax, [rbx + 3]
  mov qword ptr [rsp + 144], rax
  lea rax, [rbx + 6]
  mov qword ptr [rsp + 152], rax
  mov eax, 1
  mov qword ptr [rsp + 8], rax
  mov qword ptr [rsp + 16], 0
  xor r12d, r12d
  mov eax, dword ptr [rsp + 28]
  mov r9d, eax
 @LBB0_7:
  mov r15d, r9d
  mov eax, dword ptr [rsi + r15]
  xor eax, r10d
  test eax, 16777215
  je @LBB0_10
  test al, al
  jne @LBB0_18
  and eax, 65280
  cmp eax, 1
  mov eax, 0
  adc eax, 1
  mov r15, qword ptr [rsp + 8]
  cmp r15d, eax
  setb r8b
  mov r11, r13
  mov r13, rbx
  mov rbx, r14
  mov r14, qword ptr [rsp + 16]
  test r14d, r14d
  sete sil
  or sil, r8b
  mov rsi, qword ptr [rsp + 88]
  cmove eax, r15d
  cmovne r14d, ecx
  mov qword ptr [rsp + 16], r14
  mov r14, rbx
  mov rbx, r13
  mov r13, r11
  mov qword ptr [rsp + 8], rax
  jmp @LBB0_18
 @LBB0_10:
  mov r8, qword ptr [rsp + 8]
  mov eax, r8d
  cmp qword ptr [rsp + 160], rax
  jb @LBB0_18
  add r15, rsi
  lea eax, [r8 - 1]
  movzx r8d, byte ptr [rbx + rax]
  cmp r8b, byte ptr [r15 + rax]
  jne @LBB0_18
  mov r8d, 3
  mov rax, qword ptr [rsp + 152]
  cmp rax, qword ptr [rsp + 56]
  jae @LBB0_17
  mov r8d, 3
  mov eax, 3
  mov rsi, qword ptr [rsp + 144]
 @LBB0_14:
  mov esi, dword ptr [rsi]
  mov eax, dword ptr [r15 + rax]
  cmp esi, eax
  jne @LBB0_16
  add r8d, 4
  lea rsi, [rbx + r8]
  lea r14, [rbx + r8]
  add r14, 3
  mov rax, r8
  cmp r14, qword ptr [rsp + 56]
  jb @LBB0_14
  jmp @LBB0_17
 @LBB0_16:
  xor eax, esi
  bsf eax, eax
  shr eax, 3
  add eax, r8d
  mov r8d, eax
 @LBB0_17:
  mov rax, qword ptr [rsp + 8]
  cmp eax, r8d
  cmova r8d, eax
  mov rax, qword ptr [rsp + 16]
  cmovb eax, ecx
  mov qword ptr [rsp + 16], rax
  mov qword ptr [rsp + 8], r8
  mov r14, qword ptr [rsp + 112]
  mov rsi, qword ptr [rsp + 88]
 @LBB0_18:
  inc r12d
  mov eax, r12d
  mov rcx, qword ptr [rsp + 120]
  shr eax, cl
  test eax, eax
  jne @LBB0_21
  and r9d, 32767
  mov rax, qword ptr [rsp + 80]
  mov r9d, dword ptr [rax + 4*r9]
  mov rax, qword ptr [rsp + 96]
  mov ecx, eax
  sub ecx, r9d
  jbe @LBB0_21
  cmp ecx, 2147483647
  jb @LBB0_7
 @LBB0_21:
  cmp dword ptr [rsp + 8], 1
  sete r9b
  mov rcx, qword ptr [rsp + 16]
  jne @LBB0_27
  lea eax, [rcx - 1]
  cmp eax, 14
  ja @LBB0_27
  or ecx, 112
  mov r10d, 7
  mov r12, qword ptr [rsp + 40]
  mov r15, qword ptr [rsp + 48]
  mov r9, rcx
  jmp @LBB0_25
 @LBB0_24:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, r9d
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_81
 @LBB0_25:
  test edx, edx
  jne @LBB0_24
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_24
 @LBB0_27:
  mov eax, ecx
  xor eax, dword ptr [rsp + 32]
  mov r8, qword ptr [rsp + 8]
  cmp r8d, 2
  setae cl
  or eax, dword ptr [rsp + 4]
  sete al
  and al, cl
  cmp al, 1
  mov r12, qword ptr [rsp + 40]
  mov r15, qword ptr [rsp + 48]
  jne @LBB0_32
  mov r10d, 2
  jmp @LBB0_30
 @LBB0_29:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, 2
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_38
 @LBB0_30:
  test edx, edx
  jne @LBB0_29
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_29
 @LBB0_32:
  mov eax, r8d
  and eax, -2
  cmp eax, 2
  jne @LBB0_47
  cmp dword ptr [rsp + 16], 127
  ja @LBB0_47
  mov r10d, 3
  mov r9, qword ptr [rsp + 16]
  jmp @LBB0_36
 @LBB0_35:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, 6
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_86
 @LBB0_36:
  test edx, edx
  jne @LBB0_35
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_35
 @LBB0_38:
  mov eax, 2
  mov r10, qword ptr [rsp + 8]
  jmp @LBB0_40
 @LBB0_39:
  cmp eax, edx
  mov ecx, edx
  cmovb ecx, eax
  sub edx, ecx
  shl edi, cl
  sub eax, ecx
  je @LBB0_42
 @LBB0_40:
  test edx, edx
  jne @LBB0_39
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_39
 @LBB0_42:
  cmp r10d, 511
  ja @LBB0_83
  mov ecx, r10d
  lea r8, [rip + GammaEncodeLookUpTable]
  mov eax, dword ptr [r8 + 8*rcx]
  mov r10d, dword ptr [r8 + 8*rcx + 4]
  jmp @LBB0_45
 @LBB0_44:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov r8d, edi
  mov ecx, r11d
  shl r8d, cl
  sub edx, r11d
  sub r10d, r11d
  mov r9d, eax
  mov ecx, r10d
  shr r9d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r9d
  or edi, r8d
  test r10d, r10d
  je @LBB0_85
 @LBB0_45:
  test edx, edx
  jne @LBB0_44
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_44
 @LBB0_47:
  mov rax, qword ptr [rsp + 8]
  cmp eax, 3
  ja @LBB0_52
  cmp eax, 2
  jb @LBB0_50
  mov rax, qword ptr [rsp + 16]
  add eax, -128
  cmp eax, 1152
  jb @LBB0_52
 @LBB0_50:
  cmp dword ptr [rsp + 8], 3
  jne @LBB0_56
  mov rax, qword ptr [rsp + 16]
  add eax, -1280
  mov ecx, 3
  mov qword ptr [rsp + 8], rcx
  cmp eax, 30719
  ja @LBB0_56
 @LBB0_52:
  mov r10d, 2
  mov r9, qword ptr [rsp + 16]
  jmp @LBB0_54
 @LBB0_53:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, 2
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_87
 @LBB0_54:
  test edx, edx
  jne @LBB0_53
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_53
 @LBB0_56:
  cmp dword ptr [rsp + 76], 32
  jb @LBB0_61
  test r9b, r9b
  je @LBB0_61
  test r10b, r10b
  je @LBB0_77
  test edx, edx
  je @LBB0_93
  dec edx
  jmp @LBB0_94
 @LBB0_61:
  mov eax, dword ptr [rsp + 24]
  mov ecx, dword ptr [rsp + 76]
  shr eax, cl
  test eax, eax
  je @LBB0_76
  cmp rbx, qword ptr [rsp + 64]
  jae @LBB0_82
  mov qword ptr [rsp + 48], r15
  xor r10d, r10d
  mov rcx, rbx
 @LBB0_64:
  movzx r8d, byte ptr [rcx]
  mov r15, r10
  test r8b, r8b
  je @LBB0_67
  test edx, edx
  je @LBB0_71
  dec edx
  jmp @LBB0_72
 @LBB0_67:
  mov r10d, 7
  jmp @LBB0_69
 @LBB0_68:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov r8d, edi
  mov ecx, r11d
  shl r8d, cl
  sub edx, r11d
  sub r10d, r11d
  mov r9d, 112
  mov ecx, r10d
  shr r9d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r9d
  or edi, r8d
  test r10d, r10d
  je @LBB0_73
 @LBB0_69:
  test edx, edx
  jne @LBB0_68
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_68
 @LBB0_71:
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  movzx r8d, byte ptr [rcx]
  mov edx, 31
 @LBB0_72:
  add edi, edi
  mov byte ptr [r14 + rbp], r8b
  inc rbp
 @LBB0_73:
  mov r10, r15
  inc r10d
  cmp r10d, eax
  je @LBB0_95
  mov ecx, r10d
  add rcx, rbx
  cmp rcx, qword ptr [rsp + 64]
  jb @LBB0_64
  mov dword ptr [rsp + 4], 0
  mov r15, qword ptr [rsp + 48]
  mov eax, 1
  cmp r10d, 1
  je @LBB0_96
  jmp @LBB0_168
 @LBB0_76:
  xor eax, eax
  jmp @LBB0_96
 @LBB0_77:
  mov r10d, 7
  jmp @LBB0_79
 @LBB0_78:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, 112
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_81
 @LBB0_79:
  test edx, edx
  jne @LBB0_78
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_78
 @LBB0_81:
  mov r10d, 1
  mov dword ptr [rsp + 4], 0
  jmp @LBB0_168
 @LBB0_82:
  xor r10d, r10d
  mov eax, 1
  cmp r10d, 1
  je @LBB0_96
  jmp @LBB0_168
 @LBB0_83:
  bsr ecx, r10d
  mov eax, 1
  shl eax, cl
  mov r8d, eax
  shr r8d, 1
  xor ecx, ecx
  test r8d, r10d
  setne r8b
  test edx, edx
  je @LBB0_97
  dec edx
  jmp @LBB0_98
 @LBB0_85:
  mov dword ptr [rsp + 4], 1
  jmp @LBB0_167
 @LBB0_86:
  mov r10, qword ptr [rsp + 8]
  lea eax, [r10 + 2*r9]
  add al, -2
  mov byte ptr [r14 + rbp], al
  inc rbp
  mov dword ptr [rsp + 4], 1
  mov eax, r9d
  mov qword ptr [rsp + 32], rax
  jmp @LBB0_168
 @LBB0_87:
  mov r10d, r9d
  shr r10d, 8
  cmp dword ptr [rsp + 4], 0
  je @LBB0_107
  add r10d, 2
  cmp r9d, 130559
  ja @LBB0_114
  mov eax, r10d
  lea rcx, [rip + GammaEncodeLookUpTable]
  mov r9d, dword ptr [rcx + 8*rax]
  mov r10d, dword ptr [rcx + 8*rax + 4]
  jmp @LBB0_91
 @LBB0_90:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, r9d
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_122
 @LBB0_91:
  test edx, edx
  jne @LBB0_90
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_90
 @LBB0_93:
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  movzx r10d, byte ptr [rbx]
  mov edx, 31
 @LBB0_94:
  add edi, edi
  mov byte ptr [r14 + rbp], r10b
  inc rbp
  mov rcx, qword ptr [rsp + 96]
  mov dword ptr [r12 + 4*r15], ecx
  and ecx, 32767
  mov rax, qword ptr [rsp + 80]
  mov r8d, dword ptr [rsp + 28]
  mov dword ptr [rax + 4*rcx], r8d
  inc rbx
  mov dword ptr [rsp + 4], 0
  xor r10d, r10d
  jmp @LBB0_3
 @LBB0_95:
  mov dword ptr [rsp + 4], 0
  mov r15, qword ptr [rsp + 48]
 @LBB0_96:
  mov ecx, dword ptr [rsp + 24]
  inc ecx
  mov r8d, -1
  cmove ecx, r8d
  mov dword ptr [rsp + 24], ecx
  mov r10d, eax
  jmp @LBB0_168
 @LBB0_97:
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 31
 @LBB0_98:
  mov cl, r8b
  shr eax, 2
  lea edi, [rcx + 2*rdi]
  jmp @LBB0_100
 @LBB0_99:
  mov dword ptr [r14 + r13], edi
  lea rbp, [rcx + 4]
  lea r9d, [2*rdi + 1]
  xor r8d, r8d
  test eax, r10d
  setne r8b
  mov edx, 31
  mov r13, rcx
  dec edx
  lea edi, [r8 + 2*r9]
  shr eax, 1
  je @LBB0_104
 @LBB0_100:
  mov rcx, rbp
  test edx, edx
  je @LBB0_99
  xor r8d, r8d
  test eax, r10d
  setne r8b
  lea r9d, [2*rdi + 1]
  dec edx
  je @LBB0_103
  mov rbp, rcx
  dec edx
  lea edi, [r8 + 2*r9]
  shr eax, 1
  jne @LBB0_100
  jmp @LBB0_104
 @LBB0_103:
  mov dword ptr [r14 + r13], r9d
  lea rbp, [rcx + 4]
  lea edi, [r8 + 2*r9]
  shr eax, 1
  mov edx, 31
  mov r13, rcx
  jne @LBB0_100
  jmp @LBB0_106
 @LBB0_104:
  test edx, edx
  je @LBB0_112
  mov rcx, r13
 @LBB0_106:
  mov rax, rbp
  dec edx
  mov rbp, rcx
  jmp @LBB0_113
 @LBB0_107:
  add r10d, 3
  cmp r9d, 130303
  ja @LBB0_116
  mov eax, r10d
  lea rcx, [rip + GammaEncodeLookUpTable]
  mov r9d, dword ptr [rcx + 8*rax]
  mov r10d, dword ptr [rcx + 8*rax + 4]
  jmp @LBB0_110
 @LBB0_109:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, r9d
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_122
 @LBB0_110:
  test edx, edx
  jne @LBB0_109
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_109
 @LBB0_112:
  mov dword ptr [r14 + r13], edi
  lea rax, [rbp + 4]
  mov edx, 31
 @LBB0_113:
  add edi, edi
  mov dword ptr [rsp + 4], 1
  mov r13, rbp
  mov rbp, rax
  jmp @LBB0_168
 @LBB0_114:
  bsr ecx, r10d
  mov r9d, 1
  shl r9d, cl
  mov eax, r9d
  shr eax, 1
  xor ecx, ecx
  test eax, r10d
  setne r8b
  test edx, edx
  je @LBB0_118
  dec edx
  mov cl, r8b
  lea edi, [rcx + 2*rdi]
  shr r9d, 2
  jne @LBB0_129
  jmp @LBB0_119
 @LBB0_116:
  bsr ecx, r10d
  mov r9d, 1
  shl r9d, cl
  mov eax, r9d
  shr eax, 1
  xor ecx, ecx
  test eax, r10d
  setne r8b
  test edx, edx
  je @LBB0_145
  dec edx
  mov cl, r8b
  lea edi, [rcx + 2*rdi]
  shr r9d, 2
  jne @LBB0_156
  jmp @LBB0_146
 @LBB0_118:
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 31
  mov cl, r8b
  lea edi, [rcx + 2*rdi]
  shr r9d, 2
  jne @LBB0_129
 @LBB0_119:
  mov rcx, rbp
 @LBB0_120:
  test edx, edx
  jne @LBB0_148
  mov dword ptr [r14 + r13], edi
  lea rbp, [rcx + 4]
  mov edx, 31
  add edi, edi
  mov r13, rcx
 @LBB0_122:
  mov rcx, qword ptr [rsp + 16]
  mov byte ptr [r14 + rbp], cl
  cmp ecx, 127
  ja @LBB0_150
 @LBB0_123:
  mov rax, qword ptr [rsp + 8]
  lea r10d, [rax - 2]
  lea r12, [rbp + 1]
  cmp r10d, 511
  jbe @LBB0_151
 @LBB0_124:
  bsr ecx, r10d
  mov r9d, 1
  shl r9d, cl
  mov eax, r9d
  shr eax, 1
  xor ecx, ecx
  test eax, r10d
  setne r8b
  test edx, edx
  je @LBB0_133
  dec edx
  jmp @LBB0_134
 @LBB0_126:
  mov dword ptr [r14 + r13], edi
  lea rcx, [rbp + 4]
  lea edi, [2*rdi + 1]
  xor r11d, r11d
  test r9d, r10d
  setne r11b
  mov edx, 31
 @LBB0_127:
  dec edx
 @LBB0_128:
  mov r13, rbp
  lea edi, [r11 + 2*rdi]
  shr r9d, 1
  mov rbp, rcx
  je @LBB0_120
 @LBB0_129:
  test edx, edx
  je @LBB0_126
  xor r11d, r11d
  test r9d, r10d
  setne r11b
  lea edi, [2*rdi + 1]
  dec edx
  je @LBB0_132
  mov rcx, rbp
  mov rbp, r13
  jmp @LBB0_127
 @LBB0_132:
  mov dword ptr [r14 + r13], edi
  lea rcx, [rbp + 4]
  mov edx, 31
  jmp @LBB0_128
 @LBB0_133:
  mov dword ptr [r14 + r13], edi
  add rbp, 5
  mov edx, 31
  mov r13, r12
  mov r12, rbp
 @LBB0_134:
  mov cl, r8b
  shr r9d, 2
  lea edi, [rcx + 2*rdi]
  jmp @LBB0_136
 @LBB0_135:
  mov dword ptr [r14 + r13], edi
  lea r12, [rcx + 4]
  lea r11d, [2*rdi + 1]
  xor r8d, r8d
  test r9d, r10d
  setne r8b
  mov edx, 31
  mov r13, rcx
  dec edx
  lea edi, [r8 + 2*r11]
  shr r9d, 1
  je @LBB0_140
 @LBB0_136:
  mov rcx, r12
  test edx, edx
  je @LBB0_135
  xor r8d, r8d
  test r9d, r10d
  setne r8b
  lea r11d, [2*rdi + 1]
  dec edx
  je @LBB0_139
  mov r12, rcx
  dec edx
  lea edi, [r8 + 2*r11]
  shr r9d, 1
  jne @LBB0_136
  jmp @LBB0_140
 @LBB0_139:
  mov dword ptr [r14 + r13], r11d
  lea r12, [rcx + 4]
  lea edi, [r8 + 2*r11]
  shr r9d, 1
  mov edx, 31
  mov r13, rcx
  jne @LBB0_136
  jmp @LBB0_142
 @LBB0_140:
  test edx, edx
  je @LBB0_143
  mov rcx, r13
 @LBB0_142:
  mov rbp, r12
  dec edx
  mov r12, rcx
  jmp @LBB0_144
 @LBB0_143:
  mov dword ptr [r14 + r13], edi
  lea rbp, [r12 + 4]
  mov edx, 31
 @LBB0_144:
  add edi, edi
  mov dword ptr [rsp + 4], 1
  mov r13, r12
  jmp @LBB0_165
 @LBB0_145:
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 31
  mov cl, r8b
  lea edi, [rcx + 2*rdi]
  shr r9d, 2
  jne @LBB0_156
 @LBB0_146:
  mov rcx, rbp
 @LBB0_147:
  test edx, edx
  je @LBB0_175
 @LBB0_148:
  dec edx
 @LBB0_149:
  add edi, edi
  mov rbp, rcx
  mov rcx, qword ptr [rsp + 16]
  mov byte ptr [r14 + rbp], cl
  cmp ecx, 127
  jbe @LBB0_123
 @LBB0_150:
  cmp ecx, 1280
  mov rax, qword ptr [rsp + 8]
  mov r10d, eax
  adc r10d, -1
  cmp ecx, 32000
  adc r10d, -1
  lea r12, [rbp + 1]
  cmp r10d, 511
  ja @LBB0_124
 @LBB0_151:
  mov dword ptr [rsp + 4], 1
  cmp r10d, 2
  jae @LBB0_160
  mov rbp, r12
  mov eax, ecx
  jmp @LBB0_166
 @LBB0_153:
  mov dword ptr [r14 + r13], edi
  lea rcx, [rbp + 4]
  lea edi, [2*rdi + 1]
  xor r11d, r11d
  test r9d, r10d
  setne r11b
  mov edx, 31
 @LBB0_154:
  dec edx
 @LBB0_155:
  mov r13, rbp
  lea edi, [r11 + 2*rdi]
  shr r9d, 1
  mov rbp, rcx
  je @LBB0_147
 @LBB0_156:
  test edx, edx
  je @LBB0_153
  xor r11d, r11d
  test r9d, r10d
  setne r11b
  lea edi, [2*rdi + 1]
  dec edx
  je @LBB0_159
  mov rcx, rbp
  mov rbp, r13
  jmp @LBB0_154
 @LBB0_159:
  mov dword ptr [r14 + r13], edi
  lea rcx, [rbp + 4]
  mov edx, 31
  jmp @LBB0_155
 @LBB0_160:
  mov eax, r10d
  lea rcx, [rip + GammaEncodeLookUpTable]
  mov r9d, dword ptr [rcx + 8*rax]
  mov r10d, dword ptr [rcx + 8*rax + 4]
  jmp @LBB0_162
 @LBB0_161:
  cmp r10d, edx
  mov r11d, edx
  cmovb r11d, r10d
  mov eax, edi
  mov ecx, r11d
  shl eax, cl
  sub edx, r11d
  sub r10d, r11d
  mov r8d, r9d
  mov ecx, r10d
  shr r8d, cl
  mov edi, -1
  mov ecx, r11d
  shl edi, cl
  not edi
  and edi, r8d
  or edi, eax
  test r10d, r10d
  je @LBB0_164
 @LBB0_162:
  test edx, edx
  jne @LBB0_161
  mov dword ptr [r14 + r13], edi
  mov r13, r12
  add r12, 4
  mov edx, 32
  jmp @LBB0_161
 @LBB0_164:
  mov rbp, r12
 @LBB0_165:
  mov rax, qword ptr [rsp + 16]
 @LBB0_166:
  mov qword ptr [rsp + 32], rax
  mov r12, qword ptr [rsp + 40]
 @LBB0_167:
  mov r10, qword ptr [rsp + 8]
 @LBB0_168:
  mov rcx, qword ptr [rsp + 96]
  mov eax, ecx
  and eax, 32767
  mov dword ptr [r12 + 4*r15], ecx
  mov rcx, qword ptr [rsp + 80]
  mov r8d, dword ptr [rsp + 28]
  mov dword ptr [rcx + 4*rax], r8d
  cmp dword ptr [rsp + 120], 0
  je @LBB0_3
  lea rcx, [rbx + 1]
  mov eax, r10d
  dec eax
  je @LBB0_2
  cmp rcx, qword ptr [rsp + 64]
  jae @LBB0_2
  mov ecx, r10d
  add rcx, -2
  mov r8, qword ptr [rsp + 128]
  add r8, rbx
  mov r9, qword ptr [rsp + 136]
  add r9d, ebx
  xor r10d, r10d
 @LBB0_172:
  lea r11d, [r8 + r10]
  lea esi, [r9 + r10]
  mov r14d, dword ptr [rbx + r10 + 1]
  mov r15d, 16777215
  and r14d, r15d
  imul r14d, r14d, 506832829
  shr r14d, 16
  mov r15d, dword ptr [r12 + 4*r14]
  mov dword ptr [r12 + 4*r14], esi
  and r11d, 32767
  mov rsi, qword ptr [rsp + 80]
  mov dword ptr [rsi + 4*r11], r15d
  lea r11, [r10 + 1]
  cmp ecx, r10d
  je @LBB0_174
  lea rsi, [rbx + r10]
  add rsi, 2
  mov r10, r11
  cmp rsi, qword ptr [rsp + 64]
  jb @LBB0_172
 @LBB0_174:
  sub eax, r11d
  add rbx, r11
  inc rbx
  mov r10d, eax
  mov r14, qword ptr [rsp + 112]
  mov rsi, qword ptr [rsp + 88]
  jmp @LBB0_3
 @LBB0_175:
  mov dword ptr [r14 + r13], edi
  mov r13, rcx
  add rcx, 4
  mov edx, 31
  jmp @LBB0_149
 @LBB0_176:
  mov rax, rsi
 @LBB0_177:
  cmp rax, qword ptr [rsp + 56]
  jae @LBB0_189
  mov rsi, qword ptr [rsp + 104]
  jmp @LBB0_182
 @LBB0_179:
  dec edx
 @LBB0_180:
  add edi, edi
  mov byte ptr [r14 + rbp], cl
  inc rbp
 @LBB0_181:
  inc rax
  cmp rax, qword ptr [rsp + 56]
  jae @LBB0_190
 @LBB0_182:
  movzx ecx, byte ptr [rax]
  test cl, cl
  je @LBB0_185
  test edx, edx
  jne @LBB0_179
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  movzx ecx, byte ptr [rax]
  mov edx, 31
  jmp @LBB0_180
 @LBB0_185:
  mov r8d, 7
  jmp @LBB0_187
 @LBB0_186:
  cmp r8d, edx
  mov r9d, edx
  cmovb r9d, r8d
  mov r10d, edi
  mov ecx, r9d
  shl r10d, cl
  sub edx, r9d
  sub r8d, r9d
  mov r11d, 112
  mov ecx, r8d
  shr r11d, cl
  mov edi, -1
  mov ecx, r9d
  shl edi, cl
  not edi
  and edi, r11d
  or edi, r10d
  test r8d, r8d
  je @LBB0_181
 @LBB0_187:
  test edx, edx
  jne @LBB0_186
  mov dword ptr [r14 + r13], edi
  mov r13, rbp
  add rbp, 4
  mov edx, 32
  jmp @LBB0_186
 @LBB0_189:
  mov rsi, qword ptr [rsp + 104]
 @LBB0_190:
  test edx, edx
  je @LBB0_193
  lea eax, [2*rdi + 1]
  cmp edx, 1
  jne @LBB0_194
  mov dword ptr [r14 + r13], eax
  lea r8, [rbp + 4]
  lea eax, [2*rax + 1]
  mov edx, 31
  jmp @LBB0_196
 @LBB0_193:
  mov dword ptr [r14 + r13], edi
  lea r8, [rbp + 4]
  lea eax, [4*rdi + 3]
  mov edx, 30
  jmp @LBB0_196
 @LBB0_194:
  lea eax, [2*rax + 1]
  add edx, -2
  je @LBB0_198
  mov r8, rbp
  mov rbp, r13
 @LBB0_196:
  dec edx
 @LBB0_197:
  add eax, eax
  mov byte ptr [r14 + r8], 0
  inc r8
  mov ecx, edx
  shl eax, cl
  mov dword ptr [r14 + rbp], eax
  mov qword ptr [rsi + 40], r8
  mov qword ptr [rsi + 48], rbp
  mov dword ptr [rsi + 56], eax
  mov dword ptr [rsi + 60], edx
  add rsp, 168
  pop rbx
  pop rbp
  pop rdi
  pop rsi
  pop r12
  pop r13
  pop r14
  pop r15
  ret
 @LBB0_198:
  mov dword ptr [r14 + r13], eax
  lea r8, [rbp + 4]
  mov edx, 31
  jmp @LBB0_197
end;

procedure LZBRSXDecompressCore(const aState:PLZBRSXDecompressState); assembler; {$ifdef fpc}nostackframe; ms_abi_default;{$endif}
asm
{$ifndef fpc}
 .noframe
{$endif}
  push r15
  push r14
  push r13
  push r12
  push rsi
  push rdi
  push rbp
  push rbx
  sub rsp, 72
  mov r13, rcx
  mov r14, qword ptr [rcx + 8]
  mov rax, qword ptr [rcx + 16]
  cmp rax, 5
  jb @LBB1_181
  mov r9, qword ptr [r13]
  lea r12, [r9 + rax]
  lea r11, [r9 + rax]
  add r11, -4
  mov r15, qword ptr [r13 + 24]
  add r15, r14
  mov esi, dword ptr [r9]
  add r9, 4
  mov r8d, 32
  mov rax, r14
  mov qword ptr [rsp + 48], 0
  xor edx, edx
  mov qword ptr [rsp + 16], r15
  mov qword ptr [rsp + 8], r12
 @LBB1_2:
  test r8d, r8d
  je @LBB1_9
  dec r8d
  test esi, esi
  lea esi, [rsi + rsi]
  js @LBB1_11
 @LBB1_4:
  xor edi, edi
  cmp r9, r12
  jae @LBB1_183
  cmp rax, r15
  jae @LBB1_183
  movzx ecx, byte ptr [r9]
  inc r9
  mov byte ptr [rax], cl
 @LBB1_7:
  inc rax
  xor edx, edx
 @LBB1_8:
  cmp r9, r12
  jb @LBB1_2
  jmp @LBB1_182
 @LBB1_9:
  cmp r9, r11
  ja @LBB1_186
  mov esi, dword ptr [r9]
  add r9, 4
  mov r8d, 32
  dec r8d
  test esi, esi
  lea esi, [rsi + rsi]
  jns @LBB1_4
 @LBB1_11:
  test r8d, r8d
  je @LBB1_29
  lea r12d, [rsi + rsi]
  lea r15d, [r8 - 1]
  test esi, esi
  js @LBB1_31
 @LBB1_13:
  mov ebx, 1
  cmp r15d, 8
  jb @LBB1_56
  mov ecx, r12d
  shr ecx, 24
  lea r10, [rip + GammaDecodeLookUpTable]
  movzx ebx, byte ptr [r10 + 2*rcx]
  movzx ecx, byte ptr [r10 + 2*rcx + 1]
  test ecx, ecx
  je @LBB1_53
  shl r12d, cl
  sub r15d, ecx
 @LBB1_16:
  test edx, edx
  mov qword ptr [rsp], r14
  jne @LBB1_39
  cmp ebx, 2
  jne @LBB1_39
  mov r14d, 1
  cmp r15d, 8
  jb @LBB1_102
  mov ecx, r12d
  shr ecx, 24
  lea rdx, [rip + GammaDecodeLookUpTable]
  movzx r14d, byte ptr [rdx + 2*rcx]
  movzx ecx, byte ptr [rdx + 2*rcx + 1]
  test ecx, ecx
  je @LBB1_101
  shl r12d, cl
  sub r15d, ecx
  mov r8d, r15d
  mov esi, r12d
  mov r15, qword ptr [rsp + 16]
  mov rdx, qword ptr [rsp + 48]
 @LBB1_21:
  mov ebp, edx
  mov r10, rax
  sub r10, rbp
  xor edi, edi
  cmp r10, qword ptr [rsp]
  jb @LBB1_185
  mov ebx, r14d
  add rbx, rax
  cmp rbx, r15
  ja @LBB1_185
  lea rcx, [rbx + 32]
  mov r12d, r14d
  cmp rcx, r15
  jbe @LBB1_25
  mov r12d, r14d
  sub r12d, 32
  mov ecx, 0
  cmovb r12d, ecx
 @LBB1_25:
  test r12d, r12d
  je @LBB1_78
  mov ecx, r12d
  mov qword ptr [rsp + 56], rcx
  lea rdx, [rax + rcx]
  mov rcx, qword ptr [rsp + 48]
  cmp ecx, 16
  jb @LBB1_80
  mov rcx, rbp
  neg rcx
  mov rdi, rax
 @LBB1_28:
  movups xmm0, dqword ptr [rdi + rcx]
  movups dqword ptr [rdi], xmm0
  add rdi, 16
  cmp rdi, rdx
  jb @LBB1_28
  jmp @LBB1_126
 @LBB1_29:
  cmp r9, r11
  ja @LBB1_186
  mov esi, dword ptr [r9]
  add r9, 4
  mov r8d, 32
  lea r12d, [rsi + rsi]
  lea r15d, [r8 - 1]
  test esi, esi
  jns @LBB1_13
 @LBB1_31:
  test r15d, r15d
  je @LBB1_45
  lea esi, [r12 + r12]
  lea r8d, [r15 - 1]
  test r12d, r12d
  js @LBB1_47
 @LBB1_33:
  xor edi, edi
  mov r12, qword ptr [rsp + 8]
  cmp r9, r12
  jae @LBB1_183
  movzx r10d, byte ptr [r9]
  mov edx, r10d
  shr edx, 1
  je @LBB1_182
  mov rbx, rax
  sub rbx, rdx
  cmp rbx, r14
  jb @LBB1_183
  and r10d, 1
  lea rcx, [rax + r10]
  add rcx, 2
  mov r15, qword ptr [rsp + 16]
  cmp rcx, r15
  ja @LBB1_183
  mov qword ptr [rsp + 48], rdx
  inc r9
  movzx edx, byte ptr [rbx]
  mov byte ptr [rax], dl
  movzx edx, byte ptr [rbx + 1]
  mov byte ptr [rax + 1], dl
  mov edx, 1
  test r10d, r10d
  je @LBB1_72
  movzx r10d, byte ptr [rbx + 2]
  mov byte ptr [rax + 2], r10b
  mov rax, rcx
  jmp @LBB1_8
 @LBB1_39:
  xor edi, edi
  cmp r9, qword ptr [rsp + 8]
  jae @LBB1_183
  cmp edx, 1
  sbb ebx, -1
  movzx edx, byte ptr [r9]
  inc r9
  mov r14d, 1
  cmp r15d, 8
  jb @LBB1_112
  mov ecx, r12d
  shr ecx, 24
  lea r8, [rip + GammaDecodeLookUpTable]
  movzx r14d, byte ptr [r8 + 2*rcx]
  movzx ecx, byte ptr [r8 + 2*rcx + 1]
  test ecx, ecx
  je @LBB1_111
  shl r12d, cl
  sub r15d, ecx
  mov r8d, r15d
  mov esi, r12d
  mov r15, qword ptr [rsp + 16]
 @LBB1_43:
  shl ebx, 8
  add edx, ebx
  add edx, -768
  cmp edx, 127
  ja @LBB1_62
  add r14d, 2
  jmp @LBB1_63
 @LBB1_45:
  cmp r9, r11
  ja @LBB1_186
  mov r12d, dword ptr [r9]
  add r9, 4
  mov r15d, 32
  lea esi, [r12 + r12]
  lea r8d, [r15 - 1]
  test r12d, r12d
  jns @LBB1_33
 @LBB1_47:
  cmp r8d, 3
  jbe @LBB1_73
  mov edi, esi
  shr edi, 28
  shl r12d, 5
  add r15d, -5
  mov r8d, r15d
  mov esi, r12d
  mov r12, qword ptr [rsp + 8]
  mov r15, qword ptr [rsp + 16]
 @LBB1_49:
  test edi, edi
  je @LBB1_99
  mov edx, edi
  mov rcx, rax
  sub rcx, rdx
  xor edi, edi
  cmp rcx, r14
  jb @LBB1_183
  cmp rax, r15
  jae @LBB1_183
  movzx edi, byte ptr [rcx]
  mov byte ptr [rax], dil
  jmp @LBB1_7
 @LBB1_53:
  shl esi, 9
  add r8d, -9
  mov r15d, r8d
  mov r12d, esi
  jmp @LBB1_56
 @LBB1_54:
  add r12d, r12d
  mov ecx, r12d
 @LBB1_55:
  lea r12d, [rcx + rcx]
  dec r15d
  test ecx, ecx
  jns @LBB1_16
 @LBB1_56:
  test r15d, r15d
  je @LBB1_60
  shld ebx, r12d, 1
  dec r15d
  jne @LBB1_54
  cmp r9, r11
  ja @LBB1_186
  mov ecx, dword ptr [r9]
  add r9, 4
  mov r15d, 32
  jmp @LBB1_55
 @LBB1_60:
  cmp r9, r11
  ja @LBB1_186
  mov ecx, dword ptr [r9]
  add r9, 4
  shld ebx, ecx, 1
  add ecx, ecx
  mov r15d, 31
  jmp @LBB1_55
 @LBB1_62:
  add ebx, -768
  xor ecx, ecx
  cmp ebx, 1280
  setae cl
  cmp ebx, 32000
  sbb ecx, -1
  add ecx, r14d
  mov r14d, ecx
 @LBB1_63:
  mov r10d, edx
  mov rbx, rax
  sub rbx, r10
  cmp rbx, qword ptr [rsp]
  jb @LBB1_185
  mov ebp, r14d
  add rbp, rax
  cmp rbp, r15
  ja @LBB1_185
  lea rcx, [rbp + 32]
  mov r12d, r14d
  cmp rcx, r15
  jbe @LBB1_67
  mov r12d, r14d
  sub r12d, 32
  mov ecx, 0
  cmovb r12d, ecx
 @LBB1_67:
  mov qword ptr [rsp + 48], rdx
  test r12d, r12d
  je @LBB1_79
  mov ecx, r12d
  mov qword ptr [rsp + 56], rcx
  lea rdx, [rax + rcx]
  mov rcx, qword ptr [rsp + 48]
  cmp ecx, 16
  jb @LBB1_90
  mov rdi, r10
  mov rcx, r10
  neg rcx
  mov r10, rax
 @LBB1_70:
  movups xmm0, dqword ptr [r10 + rcx]
  movups dqword ptr [r10], xmm0
  add r10, 16
  cmp r10, rdx
  jb @LBB1_70
  mov r10, rdi
  jmp @LBB1_140
 @LBB1_72:
  mov rax, rcx
  jmp @LBB1_8
 @LBB1_73:
  mov ebx, 4
  xor edi, edi
  mov r12, qword ptr [rsp + 8]
  mov r15, qword ptr [rsp + 16]
  jmp @LBB1_75
 @LBB1_74:
  cmp ebx, r8d
  mov edx, r8d
  cmovb edx, ebx
  sub r8d, edx
  mov r10d, edi
  mov ecx, edx
  shl r10d, cl
  neg cl
  mov ebp, esi
  shr ebp, cl
  mov edi, -1
  mov ecx, edx
  shl edi, cl
  not edi
  and edi, ebp
  or edi, r10d
  shl esi, cl
  sub ebx, edx
  je @LBB1_49
 @LBB1_75:
  test r8d, r8d
  jne @LBB1_74
  cmp r9, r11
  ja @LBB1_186
  mov esi, dword ptr [r9]
  add r9, 4
  mov r8d, 32
  jmp @LBB1_74
 @LBB1_78:
  mov qword ptr [rsp + 56], 0
  jmp @LBB1_126
 @LBB1_79:
  mov qword ptr [rsp + 56], 0
  jmp @LBB1_140
 @LBB1_80:
  mov qword ptr [rsp + 24], rbp
  mov qword ptr [rsp + 32], rbx
  mov rbp, rax
  mov rbx, r10
  cmp ecx, 7
  ja @LBB1_82
  movzx ecx, byte ptr [r10]
  mov byte ptr [rax], cl
  movzx ecx, byte ptr [r10 + 1]
  mov byte ptr [rax + 1], cl
  movzx ecx, byte ptr [r10 + 2]
  mov byte ptr [rax + 2], cl
  movzx ecx, byte ptr [r10 + 3]
  mov byte ptr [rax + 3], cl
  lea rcx, [rip + SmallOffsetIncTable]
  mov rbx, qword ptr [rsp + 24]
  movsxd rcx, dword ptr [rcx + 4*rbx]
  lea rdi, [r10 + rcx]
  mov ecx, dword ptr [r10 + rcx]
  mov dword ptr [rax + 4], ecx
  lea rcx, [rip + SmallOffsetAdjustTable]
  movsxd rbx, dword ptr [rcx + 4*rbx]
  add rbx, rdi
  lea rbp, [rax + 8]
 @LBB1_82:
  cmp rbp, rdx
  jae @LBB1_89
  lea rcx, [rbp + 8]
  cmp rdx, rcx
  cmova rcx, rdx
  mov rdi, rbp
  not rdi
  add rdi, rcx
  cmp rdi, 24
  mov qword ptr [rsp + 40], r13
  jb @LBB1_122
  mov rcx, rbp
  sub rcx, rbx
  cmp rcx, 32
  jb @LBB1_122
  shr rdi, 3
  inc rdi
  mov qword ptr [rsp + 64], rdi
  mov r15, rdi
  and r15, -4
  lea r13, [rbx + 8*r15]
  lea rdi, [8*r15]
  add rdi, rbp
  xor ecx, ecx
 @LBB1_86:
  movups xmm0, dqword ptr [rbx + 8*rcx]
  movups xmm1, dqword ptr [rbx + 8*rcx + 16]
  movups dqword ptr [rbp + 8*rcx], xmm0
  movups dqword ptr [rbp + 8*rcx + 16], xmm1
  add rcx, 4
  cmp r15, rcx
  jne @LBB1_86
  cmp qword ptr [rsp + 64], r15
  mov r15, qword ptr [rsp + 16]
  jne @LBB1_123
  mov r13, qword ptr [rsp + 40]
 @LBB1_89:
  mov rbx, qword ptr [rsp + 32]
  mov rbp, qword ptr [rsp + 24]
  jmp @LBB1_126
 @LBB1_90:
  mov qword ptr [rsp + 40], r13
  mov rdi, rax
  mov r13, rbx
  cmp ecx, 7
  ja @LBB1_92
  movzx ecx, byte ptr [rbx]
  mov byte ptr [rax], cl
  movzx ecx, byte ptr [rbx + 1]
  mov byte ptr [rax + 1], cl
  movzx ecx, byte ptr [rbx + 2]
  mov byte ptr [rax + 2], cl
  movzx ecx, byte ptr [rbx + 3]
  mov byte ptr [rax + 3], cl
  lea rcx, [rip + SmallOffsetIncTable]
  movsxd rcx, dword ptr [rcx + 4*r10]
  mov rdi, r10
  lea r10, [rbx + rcx]
  mov ecx, dword ptr [rbx + rcx]
  mov dword ptr [rax + 4], ecx
  lea rcx, [rip + SmallOffsetAdjustTable]
  movsxd r13, dword ptr [rcx + 4*rdi]
  add r13, r10
  mov r10, rdi
  lea rdi, [rax + 8]
 @LBB1_92:
  cmp rdi, rdx
  jae @LBB1_121
  lea rcx, [rdi + 8]
  cmp rdx, rcx
  cmova rcx, rdx
  mov qword ptr [rsp + 24], rcx
  mov rcx, rdi
  not rcx
  add rcx, qword ptr [rsp + 24]
  mov qword ptr [rsp + 24], rcx
  cmp rcx, 24
  mov qword ptr [rsp + 32], rbp
  mov qword ptr [rsp + 64], r10
  jb @LBB1_136
  mov rcx, rdi
  sub rcx, r13
  cmp rcx, 32
  jb @LBB1_136
  mov r15, qword ptr [rsp + 24]
  shr r15, 3
  inc r15
  mov qword ptr [rsp + 24], r15
  and r15, -4
  lea r10, [8*r15]
  add r10, r13
  lea rbp, [rdi + 8*r15]
  xor ecx, ecx
 @LBB1_96:
  movups xmm0, dqword ptr [r13 + 8*rcx]
  movups xmm1, dqword ptr [r13 + 8*rcx + 16]
  movups dqword ptr [rdi + 8*rcx], xmm0
  movups dqword ptr [rdi + 8*rcx + 16], xmm1
  add rcx, 4
  cmp r15, rcx
  jne @LBB1_96
  cmp qword ptr [rsp + 24], r15
  mov r15, qword ptr [rsp + 16]
  jne @LBB1_137
  mov r13, qword ptr [rsp + 40]
  jmp @LBB1_139
 @LBB1_99:
  xor edi, edi
  cmp rax, r15
  jae @LBB1_183
  mov byte ptr [rax], dil
  jmp @LBB1_7
 @LBB1_101:
  shl r12d, 8
  add r15d, -8
 @LBB1_102:
  mov rdx, qword ptr [rsp + 48]
  mov r8d, r15d
  mov esi, r12d
  mov r15, qword ptr [rsp + 16]
  jmp @LBB1_105
 @LBB1_103:
  add esi, esi
  mov ecx, esi
 @LBB1_104:
  lea esi, [rcx + rcx]
  dec r8d
  test ecx, ecx
  jns @LBB1_21
 @LBB1_105:
  test r8d, r8d
  je @LBB1_109
  shld r14d, esi, 1
  dec r8d
  jne @LBB1_103
  cmp r9, r11
  ja @LBB1_184
  mov ecx, dword ptr [r9]
  add r9, 4
  mov r8d, 32
  jmp @LBB1_104
 @LBB1_109:
  cmp r9, r11
  ja @LBB1_184
  mov ecx, dword ptr [r9]
  add r9, 4
  shld r14d, ecx, 1
  add ecx, ecx
  mov r8d, 31
  jmp @LBB1_104
 @LBB1_111:
  shl r12d, 8
  add r15d, -8
 @LBB1_112:
  mov r8d, r15d
  mov esi, r12d
  mov r15, qword ptr [rsp + 16]
  jmp @LBB1_115
 @LBB1_113:
  add esi, esi
  mov ecx, esi
 @LBB1_114:
  lea esi, [rcx + rcx]
  dec r8d
  test ecx, ecx
  jns @LBB1_43
 @LBB1_115:
  test r8d, r8d
  je @LBB1_119
  shld r14d, esi, 1
  dec r8d
  jne @LBB1_113
  cmp r9, r11
  ja @LBB1_185
  mov ecx, dword ptr [r9]
  add r9, 4
  mov r8d, 32
  jmp @LBB1_114
 @LBB1_119:
  cmp r9, r11
  ja @LBB1_185
  mov ecx, dword ptr [r9]
  add r9, 4
  shld r14d, ecx, 1
  add ecx, ecx
  mov r8d, 31
  jmp @LBB1_114
 @LBB1_121:
  mov r13, qword ptr [rsp + 40]
  jmp @LBB1_140
 @LBB1_122:
  mov r13, rbx
  mov rdi, rbp
 @LBB1_123:
  mov rbx, qword ptr [rsp + 32]
  mov rbp, qword ptr [rsp + 24]
 @LBB1_124:
  mov rcx, qword ptr [r13]
  mov qword ptr [rdi], rcx
  add rdi, 8
  add r13, 8
  cmp rdi, rdx
  jb @LBB1_124
  mov r13, qword ptr [rsp + 40]
 @LBB1_126:
  mov edi, r14d
  sub edi, r12d
  setbe cl
  sete dl
  or dl, cl
  mov edx, 1
  je @LBB1_128
  mov rax, rbx
  jmp @LBB1_142
 @LBB1_128:
  mov qword ptr [rsp + 40], r13
  mov rcx, qword ptr [rsp + 56]
  lea r13, [rax + rcx]
  add r10, rcx
  not r12d
  add r14d, r12d
  cmp r14d, 3
  jb @LBB1_162
  cmp dword ptr [rsp + 48], 32
  jb @LBB1_162
  lea rcx, [r14 + 1]
  cmp r14d, 31
  jae @LBB1_132
  xor r12d, r12d
  jmp @LBB1_152
 @LBB1_132:
  mov qword ptr [rsp + 32], rbx
  mov qword ptr [rsp + 24], rcx
  mov r12, rcx
  mov rcx, 8589934560
  and r12, rcx
  mov r14, qword ptr [rsp + 56]
  mov rcx, r14
  sub rcx, rbp
  lea rbx, [rax + rcx]
  add rbx, 16
  add rax, r14
  add rax, 16
  xor ecx, ecx
  mov r14, qword ptr [rsp]
 @LBB1_133:
  movups xmm0, dqword ptr [rbx + rcx - 16]
  movups xmm1, dqword ptr [rbx + rcx]
  movups dqword ptr [rax + rcx - 16], xmm0
  movups dqword ptr [rax + rcx], xmm1
  add rcx, 32
  cmp r12, rcx
  jne @LBB1_133
  mov rcx, qword ptr [rsp + 24]
  cmp rcx, r12
  jne @LBB1_151
  mov rax, qword ptr [rsp + 32]
  mov r13, qword ptr [rsp + 40]
  mov r12, qword ptr [rsp + 8]
  jmp @LBB1_8
 @LBB1_136:
  mov r10, r13
  mov rbp, rdi
 @LBB1_137:
  mov r13, qword ptr [rsp + 40]
 @LBB1_138:
  mov rcx, qword ptr [r10]
  mov qword ptr [rbp], rcx
  add rbp, 8
  add r10, 8
  cmp rbp, rdx
  jb @LBB1_138
 @LBB1_139:
  mov rbp, qword ptr [rsp + 32]
  mov r10, qword ptr [rsp + 64]
 @LBB1_140:
  mov edi, r14d
  sub edi, r12d
  setbe cl
  sete dl
  or dl, cl
  mov edx, 1
  je @LBB1_143
  mov rax, rbp
 @LBB1_142:
  mov r14, qword ptr [rsp]
  mov r12, qword ptr [rsp + 8]
  jmp @LBB1_8
 @LBB1_143:
  mov qword ptr [rsp + 32], rbp
  mov rbp, r13
  mov rcx, qword ptr [rsp + 56]
  lea r13, [rax + rcx]
  add rbx, rcx
  not r12d
  add r14d, r12d
  cmp r14d, 3
  jb @LBB1_171
  cmp dword ptr [rsp + 48], 32
  jb @LBB1_171
  lea rcx, [r14 + 1]
  cmp r14d, 31
  jae @LBB1_147
  xor r12d, r12d
  jmp @LBB1_157
 @LBB1_147:
  mov qword ptr [rsp + 24], rcx
  mov r12, rcx
  mov rcx, 8589934560
  and r12, rcx
  mov r14, qword ptr [rsp + 56]
  mov rcx, r14
  sub rcx, r10
  lea r10, [rax + rcx]
  add r10, 16
  add rax, r14
  add rax, 16
  xor ecx, ecx
  mov r14, qword ptr [rsp]
 @LBB1_148:
  movups xmm0, dqword ptr [r10 + rcx - 16]
  movups xmm1, dqword ptr [r10 + rcx]
  movups dqword ptr [rax + rcx - 16], xmm0
  movups dqword ptr [rax + rcx], xmm1
  add rcx, 32
  cmp r12, rcx
  jne @LBB1_148
  mov rcx, qword ptr [rsp + 24]
  cmp rcx, r12
  jne @LBB1_156
  mov rax, qword ptr [rsp + 32]
  mov r13, rbp
  mov r12, qword ptr [rsp + 8]
  jmp @LBB1_8
 @LBB1_151:
  test cl, 28
  mov rbx, qword ptr [rsp + 32]
  je @LBB1_161
 @LBB1_152:
  mov rax, 8589934560
  add rax, 28
  mov rbp, rcx
  and rax, rcx
  sub edi, eax
  lea r14, [rax + r13]
  lea r15, [r10 + rax]
 @LBB1_153:
  mov ecx, dword ptr [r10 + r12]
  mov dword ptr [r13 + r12], ecx
  add r12, 4
  cmp rax, r12
  jne @LBB1_153
  cmp rbp, rax
  mov r12, qword ptr [rsp + 8]
  jne @LBB1_163
  mov rax, rbx
  mov r13, qword ptr [rsp + 40]
  jmp @LBB1_180
 @LBB1_156:
  test cl, 28
  je @LBB1_170
 @LBB1_157:
  mov rax, 8589934560
  add rax, 28
  mov r10, rcx
  and rax, rcx
  sub edi, eax
  lea r14, [rax + r13]
  lea r15, [rbx + rax]
 @LBB1_158:
  mov ecx, dword ptr [rbx + r12]
  mov dword ptr [r13 + r12], ecx
  add r12, 4
  cmp rax, r12
  jne @LBB1_158
  cmp r10, rax
  mov r12, qword ptr [rsp + 8]
  jne @LBB1_172
  mov rax, qword ptr [rsp + 32]
  mov r13, rbp
  jmp @LBB1_180
 @LBB1_161:
  sub edi, r12d
  add r13, r12
  add r10, r12
 @LBB1_162:
  mov r14, r13
  mov r15, r10
  mov r12, qword ptr [rsp + 8]
 @LBB1_163:
  test dil, 7
  mov r13, qword ptr [rsp + 40]
  je @LBB1_167
  mov eax, edi
  and eax, 7
  xor r10d, r10d
 @LBB1_165:
  movzx ecx, byte ptr [r15 + r10]
  mov byte ptr [r14 + r10], cl
  inc r10
  cmp eax, r10d
  jne @LBB1_165
  mov eax, edi
  sub eax, r10d
  add r14, r10
  add r15, r10
  dec edi
  cmp edi, 7
  jae @LBB1_168
  jmp @LBB1_179
 @LBB1_167:
  mov eax, edi
  dec edi
  cmp edi, 7
  jb @LBB1_179
 @LBB1_168:
  mov eax, eax
  xor r10d, r10d
 @LBB1_169:
  movzx ecx, byte ptr [r15 + r10]
  mov byte ptr [r14 + r10], cl
  movzx ecx, byte ptr [r15 + r10 + 1]
  mov byte ptr [r14 + r10 + 1], cl
  movzx ecx, byte ptr [r15 + r10 + 2]
  mov byte ptr [r14 + r10 + 2], cl
  movzx ecx, byte ptr [r15 + r10 + 3]
  mov byte ptr [r14 + r10 + 3], cl
  movzx ecx, byte ptr [r15 + r10 + 4]
  mov byte ptr [r14 + r10 + 4], cl
  movzx ecx, byte ptr [r15 + r10 + 5]
  mov byte ptr [r14 + r10 + 5], cl
  movzx ecx, byte ptr [r15 + r10 + 6]
  mov byte ptr [r14 + r10 + 6], cl
  movzx ecx, byte ptr [r15 + r10 + 7]
  mov byte ptr [r14 + r10 + 7], cl
  add r10, 8
  cmp eax, r10d
  jne @LBB1_169
  jmp @LBB1_179
 @LBB1_170:
  sub edi, r12d
  add r13, r12
  add rbx, r12
 @LBB1_171:
  mov r14, r13
  mov r15, rbx
  mov r12, qword ptr [rsp + 8]
 @LBB1_172:
  test dil, 7
  mov r13, rbp
  je @LBB1_176
  mov eax, edi
  and eax, 7
  xor r10d, r10d
  mov rbx, qword ptr [rsp + 32]
 @LBB1_174:
  movzx ecx, byte ptr [r15 + r10]
  mov byte ptr [r14 + r10], cl
  inc r10
  cmp eax, r10d
  jne @LBB1_174
  mov eax, edi
  sub eax, r10d
  add r14, r10
  add r15, r10
  dec edi
  cmp edi, 7
  jae @LBB1_177
  jmp @LBB1_179
 @LBB1_176:
  mov eax, edi
  mov rbx, qword ptr [rsp + 32]
  dec edi
  cmp edi, 7
  jb @LBB1_179
 @LBB1_177:
  mov eax, eax
  xor r10d, r10d
 @LBB1_178:
  movzx ecx, byte ptr [r15 + r10]
  mov byte ptr [r14 + r10], cl
  movzx ecx, byte ptr [r15 + r10 + 1]
  mov byte ptr [r14 + r10 + 1], cl
  movzx ecx, byte ptr [r15 + r10 + 2]
  mov byte ptr [r14 + r10 + 2], cl
  movzx ecx, byte ptr [r15 + r10 + 3]
  mov byte ptr [r14 + r10 + 3], cl
  movzx ecx, byte ptr [r15 + r10 + 4]
  mov byte ptr [r14 + r10 + 4], cl
  movzx ecx, byte ptr [r15 + r10 + 5]
  mov byte ptr [r14 + r10 + 5], cl
  movzx ecx, byte ptr [r15 + r10 + 6]
  mov byte ptr [r14 + r10 + 6], cl
  movzx ecx, byte ptr [r15 + r10 + 7]
  mov byte ptr [r14 + r10 + 7], cl
  add r10, 8
  cmp eax, r10d
  jne @LBB1_178
 @LBB1_179:
  mov rax, rbx
 @LBB1_180:
  mov r14, qword ptr [rsp]
  mov r15, qword ptr [rsp + 16]
  jmp @LBB1_8
 @LBB1_181:
  mov rax, r14
 @LBB1_182:
  mov edi, 1
  jmp @LBB1_183
 @LBB1_186:
  xor edi, edi
  jmp @LBB1_183
 @LBB1_184:
  xor edi, edi
 @LBB1_185:
  mov r14, qword ptr [rsp]
 @LBB1_183:
  sub rax, r14
  mov qword ptr [r13 + 32], rax
  mov dword ptr [r13 + 40], edi
  add rsp, 72
  pop rbx
  pop rbp
  pop rdi
  pop rsi
  pop r12
  pop r13
  pop r14
  pop r15
  ret
end;

function LZBRSXCompress(const aInData:TpvPointer;const aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64;const aLevel:TpvLZBRSXLevel;const aWithSize:boolean):boolean;
var State:TLZBRSXCompressState;
    HashTable:PHashTable;
    ChainTable:PChainTable;
begin
 result:=false;
 // The worst case of the format is a match of four bytes at a distance beyond 32000, which spends
 // fourty eight tag bits plus one payload byte on those four input bytes, so 1.75 bytes out per
 // byte in. Twice the input plus slack is therefore always enough, and the core needs no capacity
 // check at all then. The peak is no higher than before, since growing doubled the buffer anyway.
 GetMem(aDestData,(aInLen*2)+4096);
 State.InData:=aInData;
 State.DestData:=aDestData;
 State.InLen:=aInLen;
 if aWithSize then begin
  PpvUInt64(aDestData)^:=aInLen;
  State.DestLen:=SizeOf(TpvUInt64);
 end else begin
  State.DestLen:=0;
 end;
 State.TagPointer:=State.DestLen;
 inc(State.DestLen,SizeOf(TpvUInt32));
 State.Tag:=0;
 State.BitCount:=32;
 State.Level:=aLevel;
 State.Padding:=0;
 GetMem(HashTable,SizeOf(THashTable));
 try
  FillChar(HashTable^,SizeOf(THashTable),#$ff);
  GetMem(ChainTable,SizeOf(TChainTable));
  try
   FillChar(ChainTable^,SizeOf(TChainTable),#$ff);
   State.HashTable:=HashTable;
   State.ChainTable:=ChainTable;
   LZBRSXCompressCore(@State);
  finally
   FreeMem(ChainTable);
  end;
 finally
  FreeMem(HashTable);
 end;
 aDestLen:=State.DestLen;
 if aDestLen>0 then begin
  ReallocMem(aDestData,aDestLen);
  result:=true;
 end else if assigned(aDestData) then begin
  FreeMem(aDestData);
  aDestData:=nil;
 end;
end;

function LZBRSXDecompress(const aInData:TpvPointer;aInLen:TpvUInt64;var aDestData:TpvPointer;out aDestLen:TpvUInt64;const aOutputSize:TpvInt64;const aWithSize:boolean):boolean;
var State:TLZBRSXDecompressState;
    OutputSize:TpvUInt64;
    InputPointer:PpvUInt8;
    Allocated:boolean;
begin

 // If the input size is too small, then exit early
 if (aWithSize and (aInLen<(SizeOf(TpvUInt64)+SizeOf(TpvUInt32)))) or ((not aWithSize) and (aInLen<SizeOf(TpvUInt32))) then begin
  result:=false;
  exit;
 end;

 InputPointer:=aInData;

 if aWithSize then begin
  OutputSize:=PpvUInt64(InputPointer)^;
{$ifdef BIG_ENDIAN}
  OutputSize:=((OutputSize and TpvUInt64($ff00000000000000)) shr 56) or
              ((OutputSize and TpvUInt64($00ff000000000000)) shr 40) or
              ((OutputSize and TpvUInt64($0000ff0000000000)) shr 24) or
              ((OutputSize and TpvUInt64($000000ff00000000)) shr 8) or
              ((OutputSize and TpvUInt64($00000000ff000000)) shl 8) or
              ((OutputSize and TpvUInt64($0000000000ff0000)) shl 24) or
              ((OutputSize and TpvUInt64($000000000000ff00)) shl 40) or
              ((OutputSize and TpvUInt64($00000000000000ff)) shl 56);
{$endif}
  inc(PpvUInt64(InputPointer));
  dec(aInLen,SizeOf(TpvUInt64));
 end else begin
  if aOutputSize>=0 then begin
   OutputSize:=aOutputSize;
  end else begin
   OutputSize:=0;
  end;
 end;

 if OutputSize=0 then begin
  result:=true;
  exit;
 end;

 aDestLen:=OutputSize;

 if (aOutputSize>=0) and (aDestLen<>TpvUInt64(aOutputSize)) then begin
  result:=false;
  aDestLen:=0;
  exit;
 end;

 Allocated:=not assigned(aDestData);
 if Allocated then begin
  if ((not aWithSize) and (aOutputSize<=0)) or (OutputSize=0) then begin
   result:=false;
   aDestLen:=0;
   exit;
  end;
  GetMem(aDestData,OutputSize);
 end;

 State.InData:=InputPointer;
 State.DestData:=aDestData;
 State.InLen:=aInLen;
 State.OutputSize:=OutputSize;
 State.OutputLen:=0;
 State.Status:=0;
 State.Padding:=0;
 LZBRSXDecompressCore(@State);

 result:=State.Status<>0;

 if not (result and (aDestLen=State.OutputLen)) then begin
  result:=false;
  aDestLen:=0;
  if Allocated then begin
   FreeMem(aDestData);
   aDestData:=nil;
  end;
 end;

end;

{$elseif defined(OptimizedVariant)}

// The very same restructuring as in the assembler cores, only in Pascal, for everything which is
// not x86-64. What it does differently to the original variant: no nested procedures anywhere, since
// FreePascal spills every local of a routine which has them, the hash chain walk sits in an own
// call free routine so that the register allocator can hold it, the two tables carry offsets instead
// of pointers, which halves their footprint, and the decompressor got an inlined bit reader plus a
// wide match copy.

{ TLZBRSXOutputState }

type POutputState=^TOutputState;
     TOutputState=record
      DestData:TpvPointer;
      DestLen:TpvUInt64;
      TagPointer:TpvUInt64;
      Tag:TpvUInt32;
      BitCount:TpvUInt32;
     end;

// The tag words and the optional size prefix go out in little endian order
function LZBRSXSwapUInt32(const aValue:TpvUInt32):TpvUInt32; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef BIG_ENDIAN}
 result:=((aValue and TpvUInt32($ff000000)) shr 24) or
         ((aValue and TpvUInt32($00ff0000)) shr 8) or
         ((aValue and TpvUInt32($0000ff00)) shl 8) or
         ((aValue and TpvUInt32($000000ff)) shl 24);
{$else}
 result:=aValue;
{$endif}
end;

function LZBRSXSwapUInt64(const aValue:TpvUInt64):TpvUInt64; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef BIG_ENDIAN}
 result:=((aValue and TpvUInt64($ff00000000000000)) shr 56) or
         ((aValue and TpvUInt64($00ff000000000000)) shr 40) or
         ((aValue and TpvUInt64($0000ff0000000000)) shr 24) or
         ((aValue and TpvUInt64($000000ff00000000)) shr 8) or
         ((aValue and TpvUInt64($00000000ff000000)) shl 8) or
         ((aValue and TpvUInt64($0000000000ff0000)) shl 24) or
         ((aValue and TpvUInt64($000000000000ff00)) shl 40) or
         ((aValue and TpvUInt64($00000000000000ff)) shl 56);
{$else}
 result:=aValue;
{$endif}
end;

procedure DoOutputUInt8(const aState:POutputState;const aValue:TpvUInt8);
begin
 PpvUInt8(Pointer(@PBytes(aState^.DestData)^[aState^.DestLen]))^:=aValue;
 inc(aState^.DestLen,SizeOf(TpvUInt8));
end;

procedure DoOutputBit(const aState:POutputState;const aBit:boolean);
begin
 if aState^.BitCount=0 then begin
  PpvUInt32(Pointer(@PBytes(aState^.DestData)^[aState^.TagPointer]))^:=LZBRSXSwapUInt32(aState^.Tag);
  aState^.TagPointer:=aState^.DestLen;
  inc(aState^.DestLen,SizeOf(TpvUInt32));
  aState^.BitCount:=31;
 end else begin
  dec(aState^.BitCount);
 end;
 aState^.Tag:=(aState^.Tag shl 1) or (ord(aBit) and 1);
end;

procedure DoOutputBits(const aState:POutputState;const aValue,aBits:TpvUInt32);
var ToDo,RemainBits:TpvUInt32;
begin
 RemainBits:=aBits;
 while RemainBits>0 do begin
  if aState^.BitCount=0 then begin
   PpvUInt32(Pointer(@PBytes(aState^.DestData)^[aState^.TagPointer]))^:=LZBRSXSwapUInt32(aState^.Tag);
   aState^.TagPointer:=aState^.DestLen;
   inc(aState^.DestLen,SizeOf(TpvUInt32));
   aState^.BitCount:=32;
  end;
  if RemainBits<aState^.BitCount then begin
   ToDo:=RemainBits;
  end else begin
   ToDo:=aState^.BitCount;
  end;
  dec(aState^.BitCount,ToDo);
  dec(RemainBits,ToDo);
  aState^.Tag:=(aState^.Tag shl ToDo) or ((aValue shr RemainBits) and ((TpvUInt32(1) shl ToDo)-1));
 end;
end;

procedure DoOutputGamma(const aState:POutputState;const aValue:TpvUInt32);
var Mask:TpvUInt32;
begin
 if aValue<=High(GammaEncodeLookUpTable) then begin
  DoOutputBits(aState,GammaEncodeLookUpTable[aValue,0],GammaEncodeLookUpTable[aValue,1]);
 end else begin
{$if declared(BSRDWord)}
  Mask:=TpvUInt32(1) shl (BSRDWord(aValue)-1);
{$else}
  Mask:=aValue shr 1;
  while (Mask and (Mask-1))<>0 do begin
   Mask:=Mask and (Mask-1);
  end;
{$ifend}
  DoOutputBit(aState,(aValue and Mask)<>0);
  Mask:=Mask shr 1;
  while Mask<>0 do begin
   DoOutputBit(aState,true);
   DoOutputBit(aState,(aValue and Mask)<>0);
   Mask:=Mask shr 1;
  end;
  DoOutputBit(aState,false);
 end;
end;

// Turns the lowest set bit into the byte position it sits in. The assembler cores get the same
// thing from a plain bsf instead.
const MultiplyDeBruijnBytePosition:array[0..31] of TpvUInt8=(0,0,3,0,3,1,3,0,3,2,2,1,3,2,0,1,3,3,1,2,2,2,2,0,3,1,2,0,1,0,1,1);

// Returns the best match as (Length shl 32) or Distance. This is an own routine without any call
// inside on purpose, so that the whole hash chain walk can stay in registers.
function FindBestMatch(const aInData,aCurrentPointer,aEndPointer:PpvUInt8;const aCurrentIndex,aHead,aMaxSteps:TpvUInt32;const aChainTable:PChainTable):TpvUInt64;
var CurrentPossibleMatch:PpvUInt8;
    BestMatchDistance,BestMatchLength,MatchLength,Step,Difference,PossibleIndex,
    CurrentValue:TpvUInt32;
begin
 PossibleIndex:=aHead;
 BestMatchDistance:=0;
 BestMatchLength:=1;
 Step:=0;
 CurrentValue:=PpvUInt32(TpvPointer(aCurrentPointer))^;
 // EmptySlot is the largest possible value, so it fails the comparison below all by itself
 while (aCurrentIndex>PossibleIndex) and
       (TpvPtrInt(aCurrentIndex-PossibleIndex)<TpvPtrInt(MaxOffset)) do begin
  CurrentPossibleMatch:={%H-}TpvPointer(TpvPtrUInt(TpvPtrUInt(aInData)+TpvPtrUInt(PossibleIndex)));
  Difference:=CurrentValue xor PpvUInt32(TpvPointer(@PBytes(CurrentPossibleMatch)^[0]))^;
  if (Difference and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffffff00{$else}$00ffffff{$ifend}))=0 then begin
   if (BestMatchLength<=({%H-}TpvPtrUInt(aEndPointer)-{%H-}TpvPtrUInt(aCurrentPointer))) and
      (PBytes(aCurrentPointer)^[BestMatchLength-1]=PBytes(CurrentPossibleMatch)^[BestMatchLength-1]) then begin
    MatchLength:=MinMatch;
    while ({%H-}TpvPtrUInt(@PBytes(aCurrentPointer)^[MatchLength+(SizeOf(TpvUInt32)-1)])<{%H-}TpvPtrUInt(aEndPointer)) do begin
     Difference:=PpvUInt32(TpvPointer(@PBytes(aCurrentPointer)^[MatchLength]))^ xor PpvUInt32(TpvPointer(@PBytes(CurrentPossibleMatch)^[MatchLength]))^;
     if Difference=0 then begin
      inc(MatchLength,SizeOf(TpvUInt32));
     end else begin
{$if defined(FPC_BIG_ENDIAN)}
      if (Difference shr 16)<>0 then begin
       inc(MatchLength,not (Difference shr 24));
      end else begin
       inc(MatchLength,2+(not (Difference shr 8)));
      end;
{$else}
      inc(MatchLength,MultiplyDeBruijnBytePosition[TpvUInt32(TpvUInt32(Difference and (-Difference))*TpvUInt32($077cb531)) shr 27]);
{$ifend}
      break;
     end;
    end;
    if BestMatchLength<MatchLength then begin
     BestMatchDistance:=aCurrentIndex-PossibleIndex;
     BestMatchLength:=MatchLength;
    end;
   end;
  end else if (Difference and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ff000000{$else}$000000ff{$ifend}))=0 then begin
   if (Difference and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffff0000{$else}$0000ffff{$ifend}))=0 then begin
    MatchLength:=2;
   end else begin
    MatchLength:=1;
   end;
   if (BestMatchLength<MatchLength) or (BestMatchDistance=0) then begin
    BestMatchDistance:=aCurrentIndex-PossibleIndex;
    BestMatchLength:=MatchLength;
   end;
  end;
  inc(Step);
  if Step<aMaxSteps then begin
   PossibleIndex:=aChainTable^[PossibleIndex and WindowMask];
  end else begin
   break;
  end;
 end;
 result:=(TpvUInt64(BestMatchLength) shl 32) or TpvUInt64(BestMatchDistance);
end;

procedure CompressCore(const aState:POutputState;const aInData:TpvPointer;const aInLen:TpvUInt64;const aLevel:TpvLZBRSXLevel;const aHashTable:PHashTable;const aChainTable:PChainTable);
var CurrentPointer,EndPointer,EndSearchPointer:PpvUInt8;
    BestMatchDistance,BestMatchLength,Step,MaxSteps,
    Offset,SkipStrength,UnsuccessfulFindMatchAttempts,
    LastMatchDistance,Value,Head,CurrentIndex:TpvUInt32;
    BestMatch:TpvUInt64;
    HashTableItem:PpvUInt32;
    Greedy,LastWasMatch:boolean;
begin

 MaxSteps:=1 shl TpvInt32(aLevel);
 SkipStrength:=(32-9)+TpvInt32(aLevel);
 Greedy:=aLevel>=TpvLZBRSXLevel(1);

 LastMatchDistance:=0;
 LastWasMatch:=false;

 CurrentPointer:=aInData;
 EndPointer:={%H-}TpvPointer(TpvPtrUInt(TpvPtrUInt(CurrentPointer)+TpvPtrUInt(aInLen)));
 EndSearchPointer:={%H-}TpvPointer(TpvPtrUInt((TpvPtrUInt(CurrentPointer)+TpvPtrUInt(aInLen))-TpvPtrUInt(TpvInt64(Max(TpvInt64(MinMatch),TpvInt64(SizeOf(TpvUInt32)))))));
 UnsuccessfulFindMatchAttempts:=TpvUInt32(1) shl SkipStrength;

 while {%H-}TpvPtrUInt(CurrentPointer)<{%H-}TpvPtrUInt(EndSearchPointer) do begin
  CurrentIndex:={%H-}TpvPtrUInt(CurrentPointer)-{%H-}TpvPtrUInt(aInData);
  HashTableItem:=@aHashTable^[((((PpvUInt32(TpvPointer(CurrentPointer))^ and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffffff00{$else}$00ffffff{$ifend}){$if defined(FPC_BIG_ENDIAN)}shr 8{$ifend}))*TpvUInt32($1e35a7bd)) shr HashShift) and HashMask];
  Head:=HashTableItem^;
  BestMatch:=FindBestMatch(aInData,CurrentPointer,EndPointer,CurrentIndex,Head,MaxSteps,aChainTable);
  BestMatchLength:=BestMatch shr 32;
  BestMatchDistance:=BestMatch and TpvUInt32($ffffffff);
  if (BestMatchLength=1) and (BestMatchDistance>0) and (BestMatchDistance<=$f) then begin
   DoOutputBits(aState,(7 shl 4) or BestMatchDistance,7);
   LastWasMatch:=false;
  end else if (BestMatchDistance=LastMatchDistance) and (BestMatchLength>=2) and not LastWasMatch then begin
   DoOutputBits(aState,2,2);
   DoOutputGamma(aState,2);
   DoOutputGamma(aState,BestMatchLength);
   LastMatchDistance:=BestMatchDistance;
   LastWasMatch:=true;
  end else if ((BestMatchLength=2) or (BestMatchLength=3)) and (BestMatchDistance<128) then begin
   DoOutputBits(aState,6,3);
   DoOutputUInt8(aState,(BestMatchDistance shl 1) or (BestMatchLength-2));
   LastMatchDistance:=BestMatchDistance;
   LastWasMatch:=true;
  end else if (BestMatchLength>=4) or
              ((BestMatchDistance>=128) and (BestMatchDistance<1280) and (BestMatchLength>=2)) or
              ((BestMatchDistance>=1280) and (BestMatchDistance<32000) and (BestMatchLength>=3)) then begin
   DoOutputBits(aState,2,2);
   if LastWasMatch then begin
    DoOutputGamma(aState,(BestMatchDistance shr 8)+2);
   end else begin
    DoOutputGamma(aState,(BestMatchDistance shr 8)+3);
   end;
   DoOutputUInt8(aState,BestMatchDistance and $ff);
   Value:=BestMatchLength;
   if BestMatchDistance<128 then begin
    dec(Value,2);
   end else begin
    if BestMatchDistance>=1280 then begin
     dec(Value);
    end;
    if BestMatchDistance>=32000 then begin
     dec(Value);
    end;
   end;
   DoOutputGamma(aState,Value);
   LastMatchDistance:=BestMatchDistance;
   LastWasMatch:=true;
  end else begin
   if (SkipStrength>31) and (BestMatchLength=1) then begin
    if CurrentPointer^=0 then begin
     DoOutputBits(aState,7 shl 4,7);
    end else begin
     DoOutputBit(aState,false);
     DoOutputUInt8(aState,CurrentPointer^);
    end;
    LastWasMatch:=false;
   end else begin
    BestMatchLength:=1;
    if BestMatchLength=1 then begin
     Step:=UnsuccessfulFindMatchAttempts shr SkipStrength;
    end else begin
     Step:=BestMatchLength;
    end;
    Offset:=0;
    while Offset<Step do begin
     if ({%H-}TpvPtrUInt(CurrentPointer)+Offset)<{%H-}TpvPtrUInt(EndSearchPointer) then begin
      if PpvUInt8Array(CurrentPointer)^[Offset]=0 then begin
       DoOutputBits(aState,7 shl 4,7);
      end else begin
       DoOutputBit(aState,false);
       DoOutputUInt8(aState,PpvUInt8Array(CurrentPointer)^[Offset]);
      end;
      LastWasMatch:=false;
      inc(Offset);
     end else begin
      BestMatchLength:=Offset; // Because we reached EndSearchPointer, so that the tail remaining literal stuff is processing the right remaining offset then
      break;
     end;
    end;
    if BestMatchLength=1 then begin
     BestMatchLength:=Offset;
     inc(UnsuccessfulFindMatchAttempts,ord(UnsuccessfulFindMatchAttempts<TpvUInt32($ffffffff)) and 1);
    end;
   end;
  end;
  HashTableItem^:=CurrentIndex;
  aChainTable^[CurrentIndex and WindowMask]:=Head;
  if Greedy then begin
   inc(CurrentPointer);
   dec(BestMatchLength);
   while (BestMatchLength>0) and ({%H-}TpvPtrUInt(CurrentPointer)<{%H-}TpvPtrUInt(EndSearchPointer)) do begin
    CurrentIndex:={%H-}TpvPtrUInt(CurrentPointer)-{%H-}TpvPtrUInt(aInData);
    HashTableItem:=@aHashTable^[((((PpvUInt32(TpvPointer(CurrentPointer))^ and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffffff00{$else}$00ffffff{$ifend}){$if defined(FPC_BIG_ENDIAN)}shr 8{$ifend}))*TpvUInt32($1e35a7bd)) shr HashShift) and HashMask];
    Head:=HashTableItem^;
    HashTableItem^:=CurrentIndex;
    aChainTable^[CurrentIndex and WindowMask]:=Head;
    inc(CurrentPointer);
    dec(BestMatchLength);
   end;
  end;
  inc(CurrentPointer,BestMatchLength);
 end;

 while {%H-}TpvPtrUInt(CurrentPointer)<{%H-}TpvPtrUInt(EndPointer) do begin
  if CurrentPointer^=0 then begin
   DoOutputBits(aState,7 shl 4,7);
  end else begin
   DoOutputBit(aState,false);
   DoOutputUInt8(aState,CurrentPointer^);
  end;
  inc(CurrentPointer);
 end;

end;

function LZBRSXCompress(const aInData:TpvPointer;const aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64;const aLevel:TpvLZBRSXLevel;const aWithSize:boolean):boolean;
var State:TOutputState;
    HashTable:PHashTable;
    ChainTable:PChainTable;
begin
 result:=false;
 // The worst case of the format is a match of four bytes at a distance beyond 32000, which spends
 // fourty eight tag bits plus one payload byte on those four input bytes, so 1.75 bytes out per
 // byte in. Twice the input plus slack is therefore always enough, and the core needs no capacity
 // check at all then. The peak is no higher than before, since growing doubled the buffer anyway.
 GetMem(aDestData,(aInLen*2)+4096);
 State.DestData:=aDestData;
 if aWithSize then begin
  PpvUInt64(aDestData)^:=LZBRSXSwapUInt64(aInLen);
  State.DestLen:=SizeOf(TpvUInt64);
 end else begin
  State.DestLen:=0;
 end;
 State.TagPointer:=State.DestLen;
 inc(State.DestLen,SizeOf(TpvUInt32));
 State.Tag:=0;
 State.BitCount:=32;
 GetMem(HashTable,SizeOf(THashTable));
 try
  FillChar(HashTable^,SizeOf(THashTable),#$ff);
  GetMem(ChainTable,SizeOf(TChainTable));
  try
   FillChar(ChainTable^,SizeOf(TChainTable),#$ff);
   CompressCore(@State,aInData,aInLen,aLevel,HashTable,ChainTable);
  finally
   FreeMem(ChainTable);
  end;
 finally
  FreeMem(HashTable);
 end;
 begin
  // End tag
  DoOutputBit(@State,true);
  DoOutputBit(@State,true);
  DoOutputBit(@State,false);
  DoOutputUInt8(@State,0);
 end;
 begin
  // Flush bits
  State.Tag:=State.Tag shl State.BitCount;
  PpvUInt32(Pointer(@PBytes(State.DestData)^[State.TagPointer]))^:=LZBRSXSwapUInt32(State.Tag);
 end;
 aDestLen:=State.DestLen;
 if aDestLen>0 then begin
  ReallocMem(aDestData,aDestLen);
  result:=true;
 end else if assigned(aDestData) then begin
  FreeMem(aDestData);
  aDestData:=nil;
 end;
end;

// Only used for the tail of a match now, where the wide copy would run past the output end
procedure DoMoveCopy(CopyFromPointer,OutputPointer:PpvUInt8;Len:TpvUInt32);
type TBlock1=TpvUInt8;
     TBlock2=TpvUInt16;
     TBlock3=array[0..2] of TpvUInt8;
     TBlock4=TpvUInt32;
     TBlock5=array[0..4] of TpvUInt8;
     TBlock6=array[0..5] of TpvUInt8;
     TBlock7=array[0..6] of TpvUInt8;
     TBlock8=TpvUInt64;
     TBlock16=array[0..1] of TpvUInt64;
     TBlock32=array[0..3] of TpvUInt64;
     TBlock64=array[0..7] of TpvUInt64;
     PBlock1=^TBlock1;
     PBlock2=^TBlock2;
     PBlock3=^TBlock3;
     PBlock4=^TBlock4;
     PBlock5=^TBlock5;
     PBlock6=^TBlock6;
     PBlock7=^TBlock7;
     PBlock8=^TBlock8;
     PBlock16=^TBlock16;
     PBlock32=^TBlock32;
     PBlock64=^TBlock64;
begin

 if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(OutputPointer)) and (TpvPtrUInt(OutputPointer)<(TpvPtrUInt(CopyFromPointer)+TpvPtrUInt(Len))) then begin

  // Overlapping

  while Len>0 do begin
   OutputPointer^:=CopyFromPointer^;
   inc(OutputPointer);
   inc(CopyFromPointer);
   dec(Len);
  end;

 end else begin

  // Non-overlapping

  if Len>SizeOf(TBlock8) then begin

   while Len>=SizeOf(TBlock64) do begin
    PBlock64(pointer(OutputPointer))^:=PBlock64(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock64));
    inc(CopyFromPointer,SizeOf(TBlock64));
    dec(Len,SizeOf(TBlock64));
   end;

   while Len>=SizeOf(TBlock32) do begin
    PBlock32(pointer(OutputPointer))^:=PBlock32(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock32));
    inc(CopyFromPointer,SizeOf(TBlock32));
    dec(Len,SizeOf(TBlock32));
   end;

   while Len>=SizeOf(TBlock16) do begin
    PBlock16(pointer(OutputPointer))^:=PBlock16(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock16));
    inc(CopyFromPointer,SizeOf(TBlock16));
    dec(Len,SizeOf(TBlock16));
   end;

   while Len>=SizeOf(TBlock8) do begin
    PBlock8(pointer(OutputPointer))^:=PBlock8(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock8));
    inc(CopyFromPointer,SizeOf(TBlock8));
    dec(Len,SizeOf(TBlock8));
   end;

  end;

  case Len of
   0:begin
   end;
   1:begin
    PBlock1(pointer(OutputPointer))^:=PBlock1(pointer(CopyFromPointer))^;
   end;
   2:begin
    PBlock2(pointer(OutputPointer))^:=PBlock2(pointer(CopyFromPointer))^;
   end;
   3:begin
    PBlock3(pointer(OutputPointer))^:=PBlock3(pointer(CopyFromPointer))^;
   end;
   4:begin
    PBlock4(pointer(OutputPointer))^:=PBlock4(pointer(CopyFromPointer))^;
   end;
   5:begin
    PBlock5(pointer(OutputPointer))^:=PBlock5(pointer(CopyFromPointer))^;
   end;
   6:begin
    PBlock6(pointer(OutputPointer))^:=PBlock6(pointer(CopyFromPointer))^;
   end;
   7:begin
    PBlock7(pointer(OutputPointer))^:=PBlock7(pointer(CopyFromPointer))^;
   end;
   8:begin
    PBlock8(pointer(OutputPointer))^:=PBlock8(pointer(CopyFromPointer))^;
   end;
   else begin
    Assert(false);
   end;
  end;

 end;

end;

function LZBRSXDecompress(const aInData:TpvPointer;aInLen:TpvUInt64;var aDestData:TpvPointer;out aDestLen:TpvUInt64;const aOutputSize:TpvInt64;const aWithSize:boolean):boolean;
var InputPointer,InputEnd,InputSafeEnd,OutputPointer,OutputEnd,CopyFromPointer,
    CopyDestPointer,CopySourcePointer,CopyEndPointer:PpvUInt8;
    Len,FastLen,Offset,Tag,BitCount,LastMatchOffset,Value,Bit,GammaIndex,GammaShift,
    RemainBits,ToDo:TpvUInt32;
    OutputSize:TpvUInt64;
    Allocated,LastWasMatch,Failed:boolean;
begin

 // If the input size is too small, then exit early
 if (aWithSize and (aInLen<(SizeOf(TpvUInt64)+SizeOf(TpvUInt32)))) or ((not aWithSize) and (aInLen<SizeOf(TpvUInt32))) then begin
  result:=false;
  exit;
 end;

 // Setup stuff
 InputPointer:=aInData;
 InputEnd:=@PpvUInt8Array(InputPointer)^[aInLen];
 InputSafeEnd:=@PpvUInt8Array(InputPointer)^[aInLen-SizeOf(TpvUInt32)];

 if aWithSize then begin
  OutputSize:=LZBRSXSwapUInt64(PpvUInt64(InputPointer)^);
  inc(PpvUInt64(InputPointer));
 end else begin
  if aOutputSize>=0 then begin
   OutputSize:=aOutputSize;
  end else begin
   OutputSize:=0;
  end;
 end;

 if OutputSize=0 then begin
  result:=true;
  exit;
 end;

 aDestLen:=OutputSize;

 if (aOutputSize>=0) and (aDestLen<>TpvUInt64(aOutputSize)) then begin
  result:=false;
  aDestLen:=0;
  exit;
 end;

 Allocated:=not assigned(aDestData);
 if Allocated then begin
  if ((not aWithSize) and (aOutputSize<=0)) or (OutputSize=0) then begin
   result:=false;
   aDestLen:=0;
   exit;
  end;
  GetMem(aDestData,OutputSize);
 end;

 OutputPointer:=aDestData;
 OutputEnd:=@PpvUInt8Array(OutputPointer)^[OutputSize];

 result:=true;

 Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
 inc(InputPointer,SizeOf(TpvUInt32));
 BitCount:=32;

 LastMatchOffset:=0;
 LastWasMatch:=false;

 while TpvPtrUInt(InputPointer)<TpvPtrUInt(InputEnd) do begin

  // GetBit
  if BitCount=0 then begin
   if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
    result:=false;
    break;
   end;
   Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
   inc(InputPointer,SizeOf(TpvUInt32));
   BitCount:=32;
  end;
  Bit:=Tag shr 31;
  Tag:=Tag shl 1;
  dec(BitCount);

  if Bit<>0 then begin

   // GetBit
   if BitCount=0 then begin
    if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
     result:=false;
     break;
    end;
    Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
    inc(InputPointer,SizeOf(TpvUInt32));
    BitCount:=32;
   end;
   Bit:=Tag shr 31;
   Tag:=Tag shl 1;
   dec(BitCount);

   if Bit<>0 then begin

    // GetBit
    if BitCount=0 then begin
     if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
      result:=false;
      break;
     end;
     Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
     inc(InputPointer,SizeOf(TpvUInt32));
     BitCount:=32;
    end;
    Bit:=Tag shr 31;
    Tag:=Tag shl 1;
    dec(BitCount);

    if Bit<>0 then begin

     // GetBits(4)
     if BitCount>=4 then begin
      Offset:=Tag shr 28;
      Tag:=Tag shl 4;
      dec(BitCount,4);
     end else begin
      Offset:=0;
      RemainBits:=4;
      Failed:=false;
      while RemainBits>0 do begin
       if BitCount=0 then begin
        if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
         Failed:=true;
         break;
        end;
        Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
        inc(InputPointer,SizeOf(TpvUInt32));
        BitCount:=32;
       end;
       if RemainBits<BitCount then begin
        ToDo:=RemainBits;
       end else begin
        ToDo:=BitCount;
       end;
       dec(BitCount,ToDo);
       dec(RemainBits,ToDo);
       Offset:=(Offset shl ToDo) or ((Tag shr (32-ToDo)) and ((TpvUInt32(1) shl ToDo)-1));
       Tag:=Tag shl ToDo;
      end;
      if Failed then begin
       result:=false;
       break;
      end;
     end;

     if Offset<>0 then begin
      CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
      if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
         (TpvPtrUInt(OutputPointer)>=TpvPtrUInt(OutputEnd)) then begin
       result:=false;
       break;
      end;
      OutputPointer^:=CopyFromPointer^;
     end else begin
      if TpvPtrUInt(OutputPointer)>=TpvPtrUInt(OutputEnd) then begin
       result:=false;
       break;
      end;
      OutputPointer^:=0;
     end;
     inc(OutputPointer);
     LastWasMatch:=false;

    end else begin

     if TpvPtrUInt(InputPointer)>=TpvPtrUInt(InputEnd) then begin
      result:=false;
      break;
     end;
     Value:=InputPointer^;
     inc(InputPointer);
     Offset:=Value shr 1;
     Len:=(Value and 1)+2;
     if Offset<>0 then begin
      CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
      if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
         ((TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len))>TpvPtrUInt(OutputEnd)) then begin
       result:=false;
       break;
      end;
      OutputPointer^:=CopyFromPointer^;
      PpvUInt8Array(OutputPointer)^[1]:=PpvUInt8Array(CopyFromPointer)^[1];
      if Len>2 then begin
       PpvUInt8Array(OutputPointer)^[2]:=PpvUInt8Array(CopyFromPointer)^[2];
      end;
      inc(OutputPointer,Len);
      LastMatchOffset:=Offset;
      LastWasMatch:=true;
     end else begin
      break;
     end;

    end;

   end else begin

    // GetGamma
    Offset:=1;
    Failed:=false;
    if BitCount>=8 then begin
     GammaIndex:=Tag shr 24;
     Offset:=GammaDecodeLookUpTable[GammaIndex,0];
     GammaShift:=GammaDecodeLookUpTable[GammaIndex,1];
     if GammaShift<>0 then begin
      Tag:=Tag shl GammaShift;
      dec(BitCount,GammaShift);
     end else begin
      Tag:=Tag shl 8;
      dec(BitCount,8);
      repeat
       if BitCount=0 then begin
        if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
         Failed:=true;
         break;
        end;
        Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
        inc(InputPointer,SizeOf(TpvUInt32));
        BitCount:=32;
       end;
       Offset:=(Offset shl 1) or (Tag shr 31);
       Tag:=Tag shl 1;
       dec(BitCount);
       if BitCount=0 then begin
        if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
         Failed:=true;
         break;
        end;
        Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
        inc(InputPointer,SizeOf(TpvUInt32));
        BitCount:=32;
       end;
       Bit:=Tag shr 31;
       Tag:=Tag shl 1;
       dec(BitCount);
      until Bit=0;
     end;
    end else begin
     repeat
      if BitCount=0 then begin
       if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
        Failed:=true;
        break;
       end;
       Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
       inc(InputPointer,SizeOf(TpvUInt32));
       BitCount:=32;
      end;
      Offset:=(Offset shl 1) or (Tag shr 31);
      Tag:=Tag shl 1;
      dec(BitCount);
      if BitCount=0 then begin
       if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
        Failed:=true;
        break;
       end;
       Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
       inc(InputPointer,SizeOf(TpvUInt32));
       BitCount:=32;
      end;
      Bit:=Tag shr 31;
      Tag:=Tag shl 1;
      dec(BitCount);
     until Bit=0;
    end;
    if Failed then begin
     result:=false;
     break;
    end;

    if (Offset=2) and not LastWasMatch then begin

     Offset:=LastMatchOffset;

     // GetGamma
     Len:=1;
     Failed:=false;
     if BitCount>=8 then begin
      GammaIndex:=Tag shr 24;
      Len:=GammaDecodeLookUpTable[GammaIndex,0];
      GammaShift:=GammaDecodeLookUpTable[GammaIndex,1];
      if GammaShift<>0 then begin
       Tag:=Tag shl GammaShift;
       dec(BitCount,GammaShift);
      end else begin
       Tag:=Tag shl 8;
       dec(BitCount,8);
       repeat
        if BitCount=0 then begin
         if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
          Failed:=true;
          break;
         end;
         Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
         inc(InputPointer,SizeOf(TpvUInt32));
         BitCount:=32;
        end;
        Len:=(Len shl 1) or (Tag shr 31);
        Tag:=Tag shl 1;
        dec(BitCount);
        if BitCount=0 then begin
         if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
          Failed:=true;
          break;
         end;
         Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
         inc(InputPointer,SizeOf(TpvUInt32));
         BitCount:=32;
        end;
        Bit:=Tag shr 31;
        Tag:=Tag shl 1;
        dec(BitCount);
       until Bit=0;
      end;
     end else begin
      repeat
       if BitCount=0 then begin
        if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
         Failed:=true;
         break;
        end;
        Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
        inc(InputPointer,SizeOf(TpvUInt32));
        BitCount:=32;
       end;
       Len:=(Len shl 1) or (Tag shr 31);
       Tag:=Tag shl 1;
       dec(BitCount);
       if BitCount=0 then begin
        if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
         Failed:=true;
         break;
        end;
        Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
        inc(InputPointer,SizeOf(TpvUInt32));
        BitCount:=32;
       end;
       Bit:=Tag shr 31;
       Tag:=Tag shl 1;
       dec(BitCount);
      until Bit=0;
     end;
     if Failed then begin
      result:=false;
      break;
     end;

     CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
     if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
        ((TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len))>TpvPtrUInt(OutputEnd)) then begin
      result:=false;
      break;
     end;

     // The wide copy may write up to fifteen bytes past the match, so it only runs where there is
     // room for that, and the tail goes through the exact routine
     if (TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len)+32)<=TpvPtrUInt(OutputEnd) then begin
      FastLen:=Len;
     end else if Len>=32 then begin
      FastLen:=Len-32;
     end else begin
      FastLen:=0;
     end;
     if FastLen>0 then begin
      CopyDestPointer:=OutputPointer;
      CopySourcePointer:=CopyFromPointer;
      CopyEndPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)+TpvPtrUInt(FastLen)));
      if Offset>=16 then begin
       repeat
        PpvUInt64Array(CopyDestPointer)^[0]:=PpvUInt64Array(CopySourcePointer)^[0];
        PpvUInt64Array(CopyDestPointer)^[1]:=PpvUInt64Array(CopySourcePointer)^[1];
        inc(CopyDestPointer,16);
        inc(CopySourcePointer,16);
       until TpvPtrUInt(CopyDestPointer)>=TpvPtrUInt(CopyEndPointer);
      end else begin
       if Offset<8 then begin
        CopyDestPointer^:=CopySourcePointer^;
        PpvUInt8Array(CopyDestPointer)^[1]:=PpvUInt8Array(CopySourcePointer)^[1];
        PpvUInt8Array(CopyDestPointer)^[2]:=PpvUInt8Array(CopySourcePointer)^[2];
        PpvUInt8Array(CopyDestPointer)^[3]:=PpvUInt8Array(CopySourcePointer)^[3];
        inc(CopySourcePointer,SmallOffsetIncTable[Offset]);
        PpvUInt32(pointer(@PpvUInt8Array(CopyDestPointer)^[4]))^:=PpvUInt32(pointer(CopySourcePointer))^;
        inc(CopySourcePointer,SmallOffsetAdjustTable[Offset]);
        inc(CopyDestPointer,8);
       end;
       while TpvPtrUInt(CopyDestPointer)<TpvPtrUInt(CopyEndPointer) do begin
        PpvUInt64(pointer(CopyDestPointer))^:=PpvUInt64(pointer(CopySourcePointer))^;
        inc(CopyDestPointer,8);
        inc(CopySourcePointer,8);
       end;
      end;
     end;
     if FastLen<Len then begin
      DoMoveCopy(pointer(TpvPtrUInt(TpvPtrUInt(CopyFromPointer)+TpvPtrUInt(FastLen))),
                 pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)+TpvPtrUInt(FastLen))),
                 Len-FastLen);
     end;
     inc(OutputPointer,Len);

    end else begin

     if LastWasMatch then begin
      dec(Offset,2);
     end else begin
      dec(Offset,3);
     end;
     if TpvPtrUInt(InputPointer)>=TpvPtrUInt(InputEnd) then begin
      result:=false;
      break;
     end;
     Offset:=(Offset shl 8) or (InputPointer^);
     inc(InputPointer);

     // GetGamma
     Len:=1;
     Failed:=false;
     if BitCount>=8 then begin
      GammaIndex:=Tag shr 24;
      Len:=GammaDecodeLookUpTable[GammaIndex,0];
      GammaShift:=GammaDecodeLookUpTable[GammaIndex,1];
      if GammaShift<>0 then begin
       Tag:=Tag shl GammaShift;
       dec(BitCount,GammaShift);
      end else begin
       Tag:=Tag shl 8;
       dec(BitCount,8);
       repeat
        if BitCount=0 then begin
         if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
          Failed:=true;
          break;
         end;
         Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
         inc(InputPointer,SizeOf(TpvUInt32));
         BitCount:=32;
        end;
        Len:=(Len shl 1) or (Tag shr 31);
        Tag:=Tag shl 1;
        dec(BitCount);
        if BitCount=0 then begin
         if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
          Failed:=true;
          break;
         end;
         Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
         inc(InputPointer,SizeOf(TpvUInt32));
         BitCount:=32;
        end;
        Bit:=Tag shr 31;
        Tag:=Tag shl 1;
        dec(BitCount);
       until Bit=0;
      end;
     end else begin
      repeat
       if BitCount=0 then begin
        if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
         Failed:=true;
         break;
        end;
        Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
        inc(InputPointer,SizeOf(TpvUInt32));
        BitCount:=32;
       end;
       Len:=(Len shl 1) or (Tag shr 31);
       Tag:=Tag shl 1;
       dec(BitCount);
       if BitCount=0 then begin
        if TpvPtrUInt(InputPointer)>TpvPtrUInt(InputSafeEnd) then begin
         Failed:=true;
         break;
        end;
        Tag:=LZBRSXSwapUInt32(TpvUInt32(pointer(InputPointer)^));
        inc(InputPointer,SizeOf(TpvUInt32));
        BitCount:=32;
       end;
       Bit:=Tag shr 31;
       Tag:=Tag shl 1;
       dec(BitCount);
      until Bit=0;
     end;
     if Failed then begin
      result:=false;
      break;
     end;

     if Offset<128 then begin
      inc(Len,2);
     end else begin
      if Offset>=32000 then begin
       inc(Len);
      end;
      if Offset>=1280 then begin
       inc(Len);
      end;
     end;
     CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
     if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
        ((TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len))>TpvPtrUInt(OutputEnd)) then begin
      result:=false;
      break;
     end;

     if (TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len)+32)<=TpvPtrUInt(OutputEnd) then begin
      FastLen:=Len;
     end else if Len>=32 then begin
      FastLen:=Len-32;
     end else begin
      FastLen:=0;
     end;
     if FastLen>0 then begin
      CopyDestPointer:=OutputPointer;
      CopySourcePointer:=CopyFromPointer;
      CopyEndPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)+TpvPtrUInt(FastLen)));
      if Offset>=16 then begin
       repeat
        PpvUInt64Array(CopyDestPointer)^[0]:=PpvUInt64Array(CopySourcePointer)^[0];
        PpvUInt64Array(CopyDestPointer)^[1]:=PpvUInt64Array(CopySourcePointer)^[1];
        inc(CopyDestPointer,16);
        inc(CopySourcePointer,16);
       until TpvPtrUInt(CopyDestPointer)>=TpvPtrUInt(CopyEndPointer);
      end else begin
       if Offset<8 then begin
        CopyDestPointer^:=CopySourcePointer^;
        PpvUInt8Array(CopyDestPointer)^[1]:=PpvUInt8Array(CopySourcePointer)^[1];
        PpvUInt8Array(CopyDestPointer)^[2]:=PpvUInt8Array(CopySourcePointer)^[2];
        PpvUInt8Array(CopyDestPointer)^[3]:=PpvUInt8Array(CopySourcePointer)^[3];
        inc(CopySourcePointer,SmallOffsetIncTable[Offset]);
        PpvUInt32(pointer(@PpvUInt8Array(CopyDestPointer)^[4]))^:=PpvUInt32(pointer(CopySourcePointer))^;
        inc(CopySourcePointer,SmallOffsetAdjustTable[Offset]);
        inc(CopyDestPointer,8);
       end;
       while TpvPtrUInt(CopyDestPointer)<TpvPtrUInt(CopyEndPointer) do begin
        PpvUInt64(pointer(CopyDestPointer))^:=PpvUInt64(pointer(CopySourcePointer))^;
        inc(CopyDestPointer,8);
        inc(CopySourcePointer,8);
       end;
      end;
     end;
     if FastLen<Len then begin
      DoMoveCopy(pointer(TpvPtrUInt(TpvPtrUInt(CopyFromPointer)+TpvPtrUInt(FastLen))),
                 pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)+TpvPtrUInt(FastLen))),
                 Len-FastLen);
     end;
     inc(OutputPointer,Len);
     LastMatchOffset:=Offset;

    end;
    LastWasMatch:=true;

   end;

  end else begin

   if (TpvPtrUInt(InputPointer)>=TpvPtrUInt(InputEnd)) or
      (TpvPtrUInt(OutputPointer)>=TpvPtrUInt(OutputEnd)) then begin
    result:=false;
    break;
   end;
   OutputPointer^:=InputPointer^;
   inc(InputPointer);
   inc(OutputPointer);
   LastWasMatch:=false;

  end;

 end;

 OutputSize:=TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(aDestData));

 if (not aWithSize) and (aOutputSize<0) then begin
  aDestLen:=OutputSize;
 end;

 if not (result and (aDestLen=OutputSize)) then begin
  result:=false;
  aDestLen:=0;
  if Allocated then begin
   FreeMem(aDestData);
   aDestData:=nil;
  end;
 end;

end;


{$else}

function LZBRSXCompress(const aInData:TpvPointer;const aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64;const aLevel:TpvLZBRSXLevel;const aWithSize:boolean):boolean;
const HashBits=16;
      HashSize=1 shl HashBits;
      HashMask=HashSize-1;
      HashShift=32-HashBits;
      WindowSize=32768;
      WindowMask=WindowSize-1;
      MinMatch=3;
      MaxMatch=258;
      MaxOffset=TpvUInt32($7fffffff);
      MultiplyDeBruijnBytePosition:array[0..31] of TpvUInt8=(0,0,3,0,3,1,3,0,3,2,2,1,3,2,0,1,3,3,1,2,2,2,2,0,3,1,2,0,1,0,1,1);
type PHashTable=^THashTable;
     THashTable=array[0..HashSize-1] of PpvUInt8;
     PChainTable=^TChainTable;
     TChainTable=array[0..WindowSize-1] of TpvPointer;
     PThreeBytes=^TThreeBytes;
     TThreeBytes=array[0..2] of TpvUInt8;
     PBytes=^TBytes;
     TBytes=array[0..$7ffffffe] of TpvUInt8;
var CurrentPointer,EndPointer,EndSearchPointer,Head,CurrentPossibleMatch:PpvUInt8;
    BestMatchDistance,BestMatchLength,MatchLength,Step,MaxSteps,
    Difference,Offset,SkipStrength,UnsuccessfulFindMatchAttempts,
    LastMatchDistance,Value,BitCount:TpvUInt32;
    HashTable:PHashTable;
    ChainTable:PChainTable;
    HashTableItem:PPpvUInt8;
    Greedy,LastWasMatch:boolean;
    Tag:TpvUInt32;
    TagPointer:TpvUInt64;
    AllocatedDestSize:TpvUInt64;
 procedure DoOutputBlock(const aData:Pointer;const aSize:TpvUInt64);
 begin
  if aSize>0 then begin
   if AllocatedDestSize<(aDestLen+aSize) then begin
    AllocatedDestSize:=(aDestLen+aSize) shl 1;
    ReallocMem(aDestData,AllocatedDestSize);
   end;
   Move(aData^,PBytes(aDestData)^[aDestLen],aSize);
   inc(aDestLen,aSize);
  end;
 end;
 function DoOutputUInt8(const aValue:TpvUInt8):TpvUInt64;
 begin
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt8)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt8)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  result:=aDestLen;
  PpvUInt8(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue;
  inc(aDestLen,SizeOf(TpvUInt8));
 end;
 function DoOutputUInt16(const aValue:TpvUInt16):TpvUInt64;
 begin
{$ifdef little_endian}
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt16)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt16)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  result:=aDestLen;
  PpvUInt16(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue;
  inc(aDestLen,SizeOf(TpvUInt16));
{$else}
  result:=DoOutputUInt8((aValue shr 0) and $ff);
  DoOutputUInt8((aValue shr 8) and $ff);
{$endif}
 end;
 procedure DoOutputUInt24(const aValue:TpvUInt32);
 begin
{$ifdef LITTLE_ENDIAN}
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt16)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt16)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  PpvUInt16(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue and $ffff;
  inc(aDestLen,SizeOf(TpvUInt16));
{$else}
  DoOutputUInt8((aValue shr 0) and $ff);
  DoOutputUInt8((aValue shr 8) and $ff);
{$endif}
  DoOutputUInt8((aValue shr 16) and $ff);
 end;
 function DoOutputUInt32(const aValue:TpvUInt32):TpvUInt64;
 begin
{$ifdef LITTLE_ENDIAN}
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt32)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt32)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  result:=aDestLen;
  PpvUInt32(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue;
  inc(aDestLen,SizeOf(TpvUInt32));
{$else}
  result:=DoOutputUInt8((aValue shr 0) and $ff);
  DoOutputUInt8((aValue shr 8) and $ff);
  DoOutputUInt8((aValue shr 16) and $ff);
  DoOutputUInt8((aValue shr 32) and $ff);
{$endif}
 end;
 procedure DoOutputUInt64(const aValue:TpvUInt64);
 begin
{$ifdef LITTLE_ENDIAN}
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt64)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt64)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  PpvUInt64(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue;
  inc(aDestLen,SizeOf(TpvUInt64));
{$else}
  DoOutputUInt8((aValue shr 0) and $ff);
  DoOutputUInt8((aValue shr 8) and $ff);
  DoOutputUInt8((aValue shr 16) and $ff);
  DoOutputUInt8((aValue shr 24) and $ff);
  DoOutputUInt8((aValue shr 32) and $ff);
  DoOutputUInt8((aValue shr 40) and $ff);
  DoOutputUInt8((aValue shr 48) and $ff);
  DoOutputUInt8((aValue shr 56) and $ff);
{$endif}
 end;
 procedure DoOutputBit(Bit:boolean);
 begin
  if BitCount=0 then begin
{$ifdef BIG_ENDIAN}
   Tag:=((Tag and TpvUInt64($ff000000) shr 24) or
        ((Tag and TpvUInt64($00ff0000) shr 8) or
        ((Tag and TpvUInt64($0000ff00) shl 8) or
        ((Tag and TpvUInt64($000000ff) shl 24);
{$endif}
   PpvUInt32(Pointer(@PBytes(aDestData)^[TagPointer]))^:=Tag;
   if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt32)) then begin
    AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt32)) shl 1;
    ReallocMem(aDestData,AllocatedDestSize);
   end;
   TagPointer:=aDestLen;
   inc(aDestLen,SizeOf(TpvUInt32));
   BitCount:=31;
  end else begin
   dec(BitCount);
  end;
  Tag:=(Tag shl 1) or (ord(Bit) and 1);
 end;
 procedure DoOutputBits(Value,Bits:TpvUInt32);
 var ToDo:TpvUInt32;
     RemainBits:TpvUInt32;
 begin
  RemainBits:=Bits;
  while RemainBits>0 do begin
   if BitCount=0 then begin
{$ifdef BIG_ENDIAN}
    Tag:=((Tag and TpvUInt64($ff000000) shr 24) or
         ((Tag and TpvUInt64($00ff0000) shr 8) or
         ((Tag and TpvUInt64($0000ff00) shl 8) or
         ((Tag and TpvUInt64($000000ff) shl 24);
{$endif}
    PpvUInt32(Pointer(@PBytes(aDestData)^[TagPointer]))^:=Tag;
    if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt32)) then begin
     AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt32)) shl 1;
     ReallocMem(aDestData,AllocatedDestSize);
    end;
    TagPointer:=aDestLen;
    inc(aDestLen,SizeOf(TpvUInt32));
    BitCount:=32;
   end;
   if RemainBits<BitCount then begin
    ToDo:=RemainBits;
   end else begin
    ToDo:=BitCount;
   end;
   dec(BitCount,ToDo);
   dec(RemainBits,ToDo);
   Tag:=(Tag shl ToDo) or ((Value shr RemainBits) and ((TpvUInt32(1) shl ToDo)-1));
  end;
 end;
 procedure DoOutputGamma(Value:TpvUInt32);
 const LookUpTable:array[0..511,0..1] of TpvUInt32=
        (
	       (0,0),
	       (0,0),
	       ($00,2),($02,2),
	       ($04,4),($06,4),($0c,4),($0e,4),
	       ($14,6),($16,6),($1c,6),($1e,6),
	       ($34,6),($36,6),($3c,6),($3e,6),
	       ($54,8),($56,8),($5c,8),($5e,8),
	       ($74,8),($76,8),($7c,8),($7e,8),
	       ($d4,8),($d6,8),($dc,8),($de,8),
	       ($f4,8),($f6,8),($fc,8),($fe,8),
	       ($154,10),($156,10),($15c,10),($15e,10),
	       ($174,10),($176,10),($17c,10),($17e,10),
	       ($1d4,10),($1d6,10),($1dc,10),($1de,10),
	       ($1f4,10),($1f6,10),($1fc,10),($1fe,10),
	       ($354,10),($356,10),($35c,10),($35e,10),
	       ($374,10),($376,10),($37c,10),($37e,10),
	       ($3d4,10),($3d6,10),($3dc,10),($3de,10),
	       ($3f4,10),($3f6,10),($3fc,10),($3fe,10),
	       ($554,12),($556,12),($55c,12),($55e,12),
	       ($574,12),($576,12),($57c,12),($57e,12),
	       ($5d4,12),($5d6,12),($5dc,12),($5de,12),
	       ($5f4,12),($5f6,12),($5fc,12),($5fe,12),
	       ($754,12),($756,12),($75c,12),($75e,12),
	       ($774,12),($776,12),($77c,12),($77e,12),
	       ($7d4,12),($7d6,12),($7dc,12),($7de,12),
	       ($7f4,12),($7f6,12),($7fc,12),($7fe,12),
	       ($d54,12),($d56,12),($d5c,12),($d5e,12),
	       ($d74,12),($d76,12),($d7c,12),($d7e,12),
	       ($dd4,12),($dd6,12),($ddc,12),($dde,12),
	       ($df4,12),($df6,12),($dfc,12),($dfe,12),
	       ($f54,12),($f56,12),($f5c,12),($f5e,12),
	       ($f74,12),($f76,12),($f7c,12),($f7e,12),
	       ($fd4,12),($fd6,12),($fdc,12),($fde,12),
	       ($ff4,12),($ff6,12),($ffc,12),($ffe,12),
	       ($1554,14),($1556,14),($155c,14),($155e,14),
	       ($1574,14),($1576,14),($157c,14),($157e,14),
	       ($15d4,14),($15d6,14),($15dc,14),($15de,14),
	       ($15f4,14),($15f6,14),($15fc,14),($15fe,14),
	       ($1754,14),($1756,14),($175c,14),($175e,14),
	       ($1774,14),($1776,14),($177c,14),($177e,14),
	       ($17d4,14),($17d6,14),($17dc,14),($17de,14),
	       ($17f4,14),($17f6,14),($17fc,14),($17fe,14),
	       ($1d54,14),($1d56,14),($1d5c,14),($1d5e,14),
	       ($1d74,14),($1d76,14),($1d7c,14),($1d7e,14),
	       ($1dd4,14),($1dd6,14),($1ddc,14),($1dde,14),
	       ($1df4,14),($1df6,14),($1dfc,14),($1dfe,14),
	       ($1f54,14),($1f56,14),($1f5c,14),($1f5e,14),
	       ($1f74,14),($1f76,14),($1f7c,14),($1f7e,14),
	       ($1fd4,14),($1fd6,14),($1fdc,14),($1fde,14),
	       ($1ff4,14),($1ff6,14),($1ffc,14),($1ffe,14),
	       ($3554,14),($3556,14),($355c,14),($355e,14),
	       ($3574,14),($3576,14),($357c,14),($357e,14),
	       ($35d4,14),($35d6,14),($35dc,14),($35de,14),
	       ($35f4,14),($35f6,14),($35fc,14),($35fe,14),
	       ($3754,14),($3756,14),($375c,14),($375e,14),
	       ($3774,14),($3776,14),($377c,14),($377e,14),
	       ($37d4,14),($37d6,14),($37dc,14),($37de,14),
	       ($37f4,14),($37f6,14),($37fc,14),($37fe,14),
	       ($3d54,14),($3d56,14),($3d5c,14),($3d5e,14),
	       ($3d74,14),($3d76,14),($3d7c,14),($3d7e,14),
	       ($3dd4,14),($3dd6,14),($3ddc,14),($3dde,14),
	       ($3df4,14),($3df6,14),($3dfc,14),($3dfe,14),
	       ($3f54,14),($3f56,14),($3f5c,14),($3f5e,14),
	       ($3f74,14),($3f76,14),($3f7c,14),($3f7e,14),
	       ($3fd4,14),($3fd6,14),($3fdc,14),($3fde,14),
	       ($3ff4,14),($3ff6,14),($3ffc,14),($3ffe,14),
	       ($5554,16),($5556,16),($555c,16),($555e,16),
	       ($5574,16),($5576,16),($557c,16),($557e,16),
	       ($55d4,16),($55d6,16),($55dc,16),($55de,16),
	       ($55f4,16),($55f6,16),($55fc,16),($55fe,16),
	       ($5754,16),($5756,16),($575c,16),($575e,16),
	       ($5774,16),($5776,16),($577c,16),($577e,16),
	       ($57d4,16),($57d6,16),($57dc,16),($57de,16),
	       ($57f4,16),($57f6,16),($57fc,16),($57fe,16),
	       ($5d54,16),($5d56,16),($5d5c,16),($5d5e,16),
	       ($5d74,16),($5d76,16),($5d7c,16),($5d7e,16),
	       ($5dd4,16),($5dd6,16),($5ddc,16),($5dde,16),
	       ($5df4,16),($5df6,16),($5dfc,16),($5dfe,16),
	       ($5f54,16),($5f56,16),($5f5c,16),($5f5e,16),
	       ($5f74,16),($5f76,16),($5f7c,16),($5f7e,16),
	       ($5fd4,16),($5fd6,16),($5fdc,16),($5fde,16),
	       ($5ff4,16),($5ff6,16),($5ffc,16),($5ffe,16),
	       ($7554,16),($7556,16),($755c,16),($755e,16),
	       ($7574,16),($7576,16),($757c,16),($757e,16),
	       ($75d4,16),($75d6,16),($75dc,16),($75de,16),
	       ($75f4,16),($75f6,16),($75fc,16),($75fe,16),
	       ($7754,16),($7756,16),($775c,16),($775e,16),
	       ($7774,16),($7776,16),($777c,16),($777e,16),
	       ($77d4,16),($77d6,16),($77dc,16),($77de,16),
	       ($77f4,16),($77f6,16),($77fc,16),($77fe,16),
	       ($7d54,16),($7d56,16),($7d5c,16),($7d5e,16),
	       ($7d74,16),($7d76,16),($7d7c,16),($7d7e,16),
	       ($7dd4,16),($7dd6,16),($7ddc,16),($7dde,16),
	       ($7df4,16),($7df6,16),($7dfc,16),($7dfe,16),
	       ($7f54,16),($7f56,16),($7f5c,16),($7f5e,16),
	       ($7f74,16),($7f76,16),($7f7c,16),($7f7e,16),
	       ($7fd4,16),($7fd6,16),($7fdc,16),($7fde,16),
	       ($7ff4,16),($7ff6,16),($7ffc,16),($7ffe,16),
	       ($d554,16),($d556,16),($d55c,16),($d55e,16),
	       ($d574,16),($d576,16),($d57c,16),($d57e,16),
	       ($d5d4,16),($d5d6,16),($d5dc,16),($d5de,16),
	       ($d5f4,16),($d5f6,16),($d5fc,16),($d5fe,16),
	       ($d754,16),($d756,16),($d75c,16),($d75e,16),
	       ($d774,16),($d776,16),($d77c,16),($d77e,16),
	       ($d7d4,16),($d7d6,16),($d7dc,16),($d7de,16),
	       ($d7f4,16),($d7f6,16),($d7fc,16),($d7fe,16),
	       ($dd54,16),($dd56,16),($dd5c,16),($dd5e,16),
	       ($dd74,16),($dd76,16),($dd7c,16),($dd7e,16),
	       ($ddd4,16),($ddd6,16),($dddc,16),($ddde,16),
	       ($ddf4,16),($ddf6,16),($ddfc,16),($ddfe,16),
	       ($df54,16),($df56,16),($df5c,16),($df5e,16),
	       ($df74,16),($df76,16),($df7c,16),($df7e,16),
	       ($dfd4,16),($dfd6,16),($dfdc,16),($dfde,16),
	       ($dff4,16),($dff6,16),($dffc,16),($dffe,16),
	       ($f554,16),($f556,16),($f55c,16),($f55e,16),
	       ($f574,16),($f576,16),($f57c,16),($f57e,16),
	       ($f5d4,16),($f5d6,16),($f5dc,16),($f5de,16),
	       ($f5f4,16),($f5f6,16),($f5fc,16),($f5fe,16),
	       ($f754,16),($f756,16),($f75c,16),($f75e,16),
	       ($f774,16),($f776,16),($f77c,16),($f77e,16),
	       ($f7d4,16),($f7d6,16),($f7dc,16),($f7de,16),
	       ($f7f4,16),($f7f6,16),($f7fc,16),($f7fe,16),
	       ($fd54,16),($fd56,16),($fd5c,16),($fd5e,16),
	       ($fd74,16),($fd76,16),($fd7c,16),($fd7e,16),
	       ($fdd4,16),($fdd6,16),($fddc,16),($fdde,16),
	       ($fdf4,16),($fdf6,16),($fdfc,16),($fdfe,16),
	       ($ff54,16),($ff56,16),($ff5c,16),($ff5e,16),
	       ($ff74,16),($ff76,16),($ff7c,16),($ff7e,16),
	       ($ffd4,16),($ffd6,16),($ffdc,16),($ffde,16),
	       ($fff4,16),($fff6,16),($fffc,16),($fffe,16)
        );
 var Mask:TpvUInt32;
 begin
  if Value<=High(LookUpTable) then begin
   DoOutputBits(LookUpTable[Value,0],LookUpTable[Value,1]);
  end else begin
{$if declared(BSRDWord)}
   Mask:=TpvUInt32(1) shl (BSRDWord(Value)-1);
{$else}
   Mask:=Value shr 1;
   while (Mask and (Mask-1))<>0 do begin
    Mask:=Mask and (Mask-1);
   end;
{$ifend}
   DoOutputBit((Value and Mask)<>0);
   Mask:=Mask shr 1;
   while Mask<>0 do begin
    DoOutputBit(true);
    DoOutputBit((Value and Mask)<>0);
    Mask:=Mask shr 1;
   end;
   DoOutputBit(false);
  end;
 end;
begin
 result:=false;
 AllocatedDestSize:=aInLen;
 if AllocatedDestSize<SizeOf(TpvUInt32) then begin
  AllocatedDestSize:=SizeOf(TpvUInt32);
 end;
 GetMem(aDestData,AllocatedDestSize);
 aDestLen:=0;
 try
  MaxSteps:=1 shl TpvInt32(aLevel);
  SkipStrength:=(32-9)+TpvInt32(aLevel);
  Greedy:=aLevel>=TpvLZBRSXLevel(1);
  if aWithSize then begin
   DoOutputUInt64(aInLen);
  end;
  BitCount:=32;
  TagPointer:=DoOutputUInt32(0);
  Tag:=0;
  LastMatchDistance:=0;
  LastWasMatch:=false;
  GetMem(HashTable,SizeOf(THashTable));
  try
   FillChar(HashTable^,SizeOf(THashTable),#0);
   GetMem(ChainTable,SizeOf(TChainTable));
   try
    FillChar(ChainTable^,SizeOf(TChainTable),#0);
    CurrentPointer:=aInData;
    EndPointer:={%H-}TpvPointer(TpvPtrUInt(TpvPtrUInt(CurrentPointer)+TpvPtrUInt(aInLen)));
    EndSearchPointer:={%H-}TpvPointer(TpvPtrUInt((TpvPtrUInt(CurrentPointer)+TpvPtrUInt(aInLen))-TpvPtrUInt(TpvInt64(Max(TpvInt64(MinMatch),TpvInt64(SizeOf(TpvUInt32)))))));
    UnsuccessfulFindMatchAttempts:=TpvUInt32(1) shl SkipStrength;
    while {%H-}TpvPtrUInt(CurrentPointer)<{%H-}TpvPtrUInt(EndSearchPointer) do begin
     HashTableItem:=@HashTable[((((PpvUInt32(TpvPointer(CurrentPointer))^ and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffffff00{$else}$00ffffff{$ifend}){$if defined(FPC_BIG_ENDIAN)}shr 8{$ifend}))*TpvUInt32($1e35a7bd)) shr HashShift) and HashMask];
     Head:=HashTableItem^;
     CurrentPossibleMatch:=Head;
     BestMatchDistance:=0;
     BestMatchLength:=1;
     Step:=0;
     while assigned(CurrentPossibleMatch) and
           ({%H-}TpvPtrUInt(CurrentPointer)>{%H-}TpvPtrUInt(CurrentPossibleMatch)) and
           (TpvPtrInt({%H-}TpvPtrUInt({%H-}TpvPtrUInt(CurrentPointer)-{%H-}TpvPtrUInt(CurrentPossibleMatch)))<TpvPtrInt(MaxOffset)) do begin
      Difference:=PpvUInt32(TpvPointer(@PBytes(CurrentPointer)^[0]))^ xor PpvUInt32(TpvPointer(@PBytes(CurrentPossibleMatch)^[0]))^;
      if (Difference and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffffff00{$else}$00ffffff{$ifend}))=0 then begin
       if (BestMatchLength<=({%H-}TpvPtrUInt(EndPointer)-{%H-}TpvPtrUInt(CurrentPointer))) and
          (PBytes(CurrentPointer)^[BestMatchLength-1]=PBytes(CurrentPossibleMatch)^[BestMatchLength-1]) then begin
        MatchLength:=MinMatch;
        while ({%H-}TpvPtrUInt(@PBytes(CurrentPointer)^[MatchLength+(SizeOf(TpvUInt32)-1)])<{%H-}TpvPtrUInt(EndPointer)) do begin
         Difference:=PpvUInt32(TpvPointer(@PBytes(CurrentPointer)^[MatchLength]))^ xor PpvUInt32(TpvPointer(@PBytes(CurrentPossibleMatch)^[MatchLength]))^;
         if Difference=0 then begin
          inc(MatchLength,SizeOf(TpvUInt32));
         end else begin
{$if defined(FPC_BIG_ENDIAN)}
          if (Difference shr 16)<>0 then begin
           inc(MatchLength,not (Difference shr 24));
          end else begin
           inc(MatchLength,2+(not (Difference shr 8)));
          end;
{$else}
          inc(MatchLength,MultiplyDeBruijnBytePosition[TpvUInt32(TpvUInt32(Difference and (-Difference))*TpvUInt32($077cb531)) shr 27]);
{$ifend}
          break;
         end;
        end;
        if BestMatchLength<MatchLength then begin
         BestMatchDistance:={%H-}TpvPtrUInt({%H-}TpvPtrUInt(CurrentPointer)-{%H-}TpvPtrUInt(CurrentPossibleMatch));
         BestMatchLength:=MatchLength;
        end;
       end;
      end else if (Difference and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ff000000{$else}$000000ff{$ifend}))=0 then begin
       if (Difference and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffff0000{$else}$0000ffff{$ifend}))=0 then begin
        MatchLength:=2;
       end else begin
        MatchLength:=1;
       end;
       if (BestMatchLength<MatchLength) or (BestMatchDistance=0) then begin
        BestMatchDistance:={%H-}TpvPtrUInt({%H-}TpvPtrUInt(CurrentPointer)-{%H-}TpvPtrUInt(CurrentPossibleMatch));
        BestMatchLength:=MatchLength;
       end;
      end;
      inc(Step);
      if Step<MaxSteps then begin
       CurrentPossibleMatch:=ChainTable^[({%H-}TpvPtrUInt(CurrentPossibleMatch)-{%H-}TpvPtrUInt(aInData)) and WindowMask];
      end else begin
       break;
      end;
     end;
     if (BestMatchLength=1) and (BestMatchDistance>0) and (BestMatchDistance<=$f) then begin
      DoOutputBits((7 shl 4) or BestMatchDistance,7);
//    DoOutputBits(7,3);
{     DoOutputBit(true);
      DoOutputBit(true);
      DoOutputBit(true);}
//    DoOutputBits(BestMatchDistance,4);
      LastWasMatch:=false;
     end else if (BestMatchDistance=LastMatchDistance) and (BestMatchLength>=2) and not LastWasMatch then begin
      DoOutputBits(2,2);
{     DoOutputBit(true);
      DoOutputBit(false);}
      DoOutputGamma(2);
      DoOutputGamma(BestMatchLength);
      LastMatchDistance:=BestMatchDistance;
      LastWasMatch:=true;
     end else if ((BestMatchLength=2) or (BestMatchLength=3)) and (BestMatchDistance<128) then begin
      DoOutputBits(6,3);
{     DoOutputBit(true);
      DoOutputBit(true);
      DoOutputBit(false);}
      DoOutputUInt8((BestMatchDistance shl 1) or (BestMatchLength-2));
      LastMatchDistance:=BestMatchDistance;
      LastWasMatch:=true;
     end else if (BestMatchLength>=4) or
                 ((BestMatchDistance>=128) and (BestMatchDistance<1280) and (BestMatchLength>=2)) or
                 ((BestMatchDistance>=1280) and (BestMatchDistance<32000) and (BestMatchLength>=3)) then begin
      DoOutputBits(2,2);
{     DoOutputBit(true);
      DoOutputBit(false);}
      if LastWasMatch then begin
       DoOutputGamma((BestMatchDistance shr 8)+2);
      end else begin
       DoOutputGamma((BestMatchDistance shr 8)+3);
      end;
      DoOutputUInt8(BestMatchDistance and $ff);
      Value:=BestMatchLength;
      if BestMatchDistance<128 then begin
       dec(Value,2);
      end else begin
       if BestMatchDistance>=1280 then begin
        dec(Value);
       end;
       if BestMatchDistance>=32000 then begin
        dec(Value);
       end;
      end;
      DoOutputGamma(Value);
      LastMatchDistance:=BestMatchDistance;
      LastWasMatch:=true;
     end else begin
      if (SkipStrength>31) and (BestMatchLength=1) then begin
       if CurrentPointer^=0 then begin
        DoOutputBits(7 shl 4,7);
//      DoOutputBits(7,3);
{       DoOutputBit(true);
        DoOutputBit(true);
        DoOutputBit(true);}
//      DoOutputBits(0,4);
       end else begin
        DoOutputBit(false);
        DoOutputUInt8(CurrentPointer^);
       end;
       LastWasMatch:=false;
      end else begin
       BestMatchLength:=1;
       if BestMatchLength=1 then begin
        Step:=UnsuccessfulFindMatchAttempts shr SkipStrength;
       end else begin
        Step:=BestMatchLength;
       end;
       Offset:=0;
       while Offset<Step do begin
        if ({%H-}TpvPtrUInt(CurrentPointer)+Offset)<{%H-}TpvPtrUInt(EndSearchPointer) then begin
         if PpvUInt8Array(CurrentPointer)^[Offset]=0 then begin
          DoOutputBits(7 shl 4,7);
//        DoOutputBits(7,3);
{         DoOutputBit(true);
          DoOutputBit(true);
          DoOutputBit(true);}
//        DoOutputBits(0,4);
         end else begin
          DoOutputBit(false);
          DoOutputUInt8(PpvUInt8Array(CurrentPointer)^[Offset]);
         end;
         LastWasMatch:=false;
         inc(Offset);
        end else begin
         BestMatchLength:=Offset; // Because we reached EndSearchPointer, so that the tail remaining literal stuff is processing the right remaining offset then
         break;
        end;
       end;
       if BestMatchLength=1 then begin
        BestMatchLength:=Offset;
        inc(UnsuccessfulFindMatchAttempts,ord(UnsuccessfulFindMatchAttempts<TpvUInt32($ffffffff)) and 1);
       end;
      end;
     end;
     HashTableItem^:=CurrentPointer;
     ChainTable^[({%H-}TpvPtrUInt(CurrentPointer)-{%H-}TpvPtrUInt(aInData)) and WindowMask]:=Head;
     if Greedy then begin
      inc(CurrentPointer);
      dec(BestMatchLength);
      while (BestMatchLength>0) and ({%H-}TpvPtrUInt(CurrentPointer)<{%H-}TpvPtrUInt(EndSearchPointer)) do begin
       HashTableItem:=@HashTable[((((PpvUInt32(TpvPointer(CurrentPointer))^ and TpvUInt32({$if defined(FPC_BIG_ENDIAN)}$ffffff00{$else}$00ffffff{$ifend}){$if defined(FPC_BIG_ENDIAN)}shr 8{$ifend}))*TpvUInt32($1e35a7bd)) shr HashShift) and HashMask];
       Head:=HashTableItem^;
       HashTableItem^:=CurrentPointer;
       ChainTable^[({%H-}TpvPtrUInt(CurrentPointer)-{%H-}TpvPtrUInt(aInData)) and WindowMask]:=Head;
       inc(CurrentPointer);
       dec(BestMatchLength);
      end;
     end;
     inc(CurrentPointer,BestMatchLength);
    end;
    while {%H-}TpvPtrUInt(CurrentPointer)<{%H-}TpvPtrUInt(EndPointer) do begin
     if CurrentPointer^=0 then begin
      DoOutputBits(7 shl 4,7);
//    DoOutputBits(7,3);
{     DoOutputBit(true);
      DoOutputBit(true);
      DoOutputBit(true);}
//    DoOutputBits(0,4);
     end else begin
      DoOutputBit(false);
      DoOutputUInt8(CurrentPointer^);
     end;
     inc(CurrentPointer);
    end;
   finally
    FreeMem(ChainTable);
   end;
  finally
   FreeMem(HashTable);
  end;
  begin
   // End tag
   DoOutputBit(true);
   DoOutputBit(true);
   DoOutputBit(false);
   DoOutputUInt8(0);
  end;
  begin
   // Flush bits
   Tag:=Tag shl BitCount;
{$ifdef BIG_ENDIAN}
   Tag:=((Tag and TpvUInt64($ff000000) shr 24) or
        ((Tag and TpvUInt64($00ff0000) shr 8) or
        ((Tag and TpvUInt64($0000ff00) shl 8) or
        ((Tag and TpvUInt64($000000ff) shl 24);
{$endif}
   PpvUInt32(Pointer(@PBytes(aDestData)^[TagPointer]))^:=Tag;
  end;
 finally
  if aDestLen>0 then begin
   ReallocMem(aDestData,aDestLen);
   result:=true;
  end else if assigned(aDestData) then begin
   FreeMem(aDestData);
   aDestData:=nil;
  end;
 end;
end;

procedure DoMoveCopy(CopyFromPointer,OutputPointer:PpvUInt8;Len:TpvUInt32);
type TBlock1=TpvUInt8;
     TBlock2=TpvUInt16;
     TBlock3=array[0..2] of TpvUInt8;
     TBlock4=TpvUInt32;
     TBlock5=array[0..4] of TpvUInt8;
     TBlock6=array[0..5] of TpvUInt8;
     TBlock7=array[0..6] of TpvUInt8;
     TBlock8=TpvUInt64;
     TBlock16=array[0..1] of TpvUInt64;
     TBlock32=array[0..3] of TpvUInt64;
     TBlock64=array[0..7] of TpvUInt64;
     PBlock1=^TBlock1;
     PBlock2=^TBlock2;
     PBlock3=^TBlock3;
     PBlock4=^TBlock4;
     PBlock5=^TBlock5;
     PBlock6=^TBlock6;
     PBlock7=^TBlock7;
     PBlock8=^TBlock8;
     PBlock16=^TBlock16;
     PBlock32=^TBlock32;
     PBlock64=^TBlock64;
begin

 if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(OutputPointer)) and (TpvPtrUInt(OutputPointer)<(TpvPtrUInt(CopyFromPointer)+TpvPtrUInt(Len))) then begin

  // Overlapping

  while Len>0 do begin
   OutputPointer^:=CopyFromPointer^;
   inc(OutputPointer);
   inc(CopyFromPointer);
   dec(Len);
  end;

 end else begin

  // Non-overlapping

  if Len>SizeOf(TBlock8) then begin

   while Len>=SizeOf(TBlock64) do begin
    PBlock64(pointer(OutputPointer))^:=PBlock64(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock64));
    inc(CopyFromPointer,SizeOf(TBlock64));
    dec(Len,SizeOf(TBlock64));
   end;

   while Len>=SizeOf(TBlock32) do begin
    PBlock32(pointer(OutputPointer))^:=PBlock32(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock32));
    inc(CopyFromPointer,SizeOf(TBlock32));
    dec(Len,SizeOf(TBlock32));
   end;

   while Len>=SizeOf(TBlock16) do begin
    PBlock16(pointer(OutputPointer))^:=PBlock16(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock16));
    inc(CopyFromPointer,SizeOf(TBlock16));
    dec(Len,SizeOf(TBlock16));
   end;

   while Len>=SizeOf(TBlock8) do begin
    PBlock8(pointer(OutputPointer))^:=PBlock8(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock8));
    inc(CopyFromPointer,SizeOf(TBlock8));
    dec(Len,SizeOf(TBlock8));
   end;

  end;

  case Len of

   0:begin

    // Do nothing in this case

   end;

   1:begin

    PBlock1(pointer(OutputPointer))^:=PBlock1(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock1));

   end;

   2:begin

    PBlock2(pointer(OutputPointer))^:=PBlock2(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock2));

   end;

   3:begin

    PBlock3(pointer(OutputPointer))^:=PBlock3(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock3));

   end;

   4:begin

    PBlock4(pointer(OutputPointer))^:=PBlock4(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock4));

   end;

   5:begin

    PBlock5(pointer(OutputPointer))^:=PBlock5(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock5));

   end;

   6:begin

    PBlock6(pointer(OutputPointer))^:=PBlock6(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock6));

   end;

   7:begin

    PBlock7(pointer(OutputPointer))^:=PBlock7(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock7));

   end;

   8:begin

    PBlock8(pointer(OutputPointer))^:=PBlock8(pointer(CopyFromPointer))^;
    inc(OutputPointer,SizeOf(TBlock8));

   end;

   else begin

    Assert(false);

   end;

  end;

 end;

end;

function LZBRSXDecompress(const aInData:TpvPointer;aInLen:TpvUInt64;var aDestData:TpvPointer;out aDestLen:TpvUInt64;const aOutputSize:TpvInt64;const aWithSize:boolean):boolean;
var InputPointer,InputEnd,OutputPointer,OutputEnd,CopyFromPointer:PpvUInt8;
    Len,Offset,Tag,BitCount,LastMatchOffset,Value:TpvUInt32;
    OutputSize:TpvUInt64;
    OK,Allocated,LastWasMatch:boolean;
function GetBit:TpvUInt32;
 begin
  if BitCount=0 then begin
   if (TpvPtrUInt(InputPointer)+SizeOf(TpvUInt32))>TpvPtrUInt(InputEnd) then begin
    OK:=false;
    result:=0;
    exit;
   end;
   Tag:=TpvUInt32(pointer(InputPointer)^);
{$ifdef BIG_ENDIAN}
   Tag:=((Tag and TpvUInt64($ff000000) shr 24) or
        ((Tag and TpvUInt64($00ff0000) shr 8) or
        ((Tag and TpvUInt64($0000ff00) shl 8) or
        ((Tag and TpvUInt64($000000ff) shl 24);
{$endif}
   inc(InputPointer,SizeOf(TpvUInt32));
   BitCount:=31;
  end else begin
   dec(BitCount);
  end;
  result:=Tag shr 31;
  Tag:=Tag shl 1;
 end;
 function GetBits(Bits:TpvUInt32):TpvUInt32;
 var RemainBits,ToDo:TpvUInt32;
 begin
  result:=0;
  RemainBits:=Bits;
  while RemainBits>0 do begin
   if BitCount=0 then begin
    if (TpvPtrUInt(InputPointer)+SizeOf(TpvUInt32))>TpvPtrUInt(InputEnd) then begin
     OK:=false;
     result:=0;
     exit;
    end;
    Tag:=TpvUInt32(pointer(InputPointer)^);
{$ifdef BIG_ENDIAN}
    Tag:=((Tag and TpvUInt64($ff000000) shr 24) or
         ((Tag and TpvUInt64($00ff0000) shr 8) or
         ((Tag and TpvUInt64($0000ff00) shl 8) or
         ((Tag and TpvUInt64($000000ff) shl 24);
{$endif}
    inc(InputPointer,SizeOf(TpvUInt32));
    BitCount:=32;
   end;
   if RemainBits<BitCount then begin
    ToDo:=RemainBits;
   end else begin
    ToDo:=BitCount;
   end;
   dec(BitCount,ToDo);
   dec(RemainBits,ToDo);
   result:=(result shl ToDo) or ((Tag shr (32-ToDo)) and ((TpvUInt32(1) shl ToDo)-1));
   Tag:=Tag shl ToDo;
  end;
 end;
 function GetGamma:TpvUInt32;
 const LookUpTable:array[0..255,0..1] of TpvUInt8=
        (
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),(2,2),
         (4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),
         (4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),(4,4),
         (8,6),(8,6),(8,6),(8,6),
         (16,8),(16,0),(17,8),(17,0),
         (9,6),(9,6),(9,6),(9,6),
         (18,8),(18,0),(19,8),(19,0),
         (5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),
         (5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),(5,4),
         (10,6),(10,6),(10,6),(10,6),
         (20,8),(20,0),(21,8),(21,0),
         (11,6),(11,6),(11,6),(11,6),
         (22,8),(22,0),(23,8),(23,0),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),(3,2),
         (6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),
         (6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),(6,4),
         (12,6),(12,6),(12,6),(12,6),
         (24,8),(24,0),(25,8),(25,0),
         (13,6),(13,6),(13,6),(13,6),
         (26,8),(26,0),(27,8),(27,0),
         (7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),
         (7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),(7,4),
         (14,6),(14,6),(14,6),(14,6),
         (28,8),(28,0),(29,8),(29,0),
         (15,6),(15,6),(15,6),(15,6),
         (30,8),(30,0),(31,8),(31,0)
        );
 var Top,Shift:TpvUInt8;
 begin
  result:=1;
  if BitCount>=8 then begin
   Top:=Tag shr 24;
   result:=LookUpTable[Top,0];
   Shift:=LookUpTable[Top,1];
   if Shift<>0 then begin
    Tag:=Tag shl Shift;
    dec(BitCount,Shift);
    exit;
   end;
   Tag:=Tag shl 8;
   dec(BitCount,8);
  end;
  repeat
   result:=(result shl 1) or GetBit;
  until GetBit=0;
 end;
begin

 // If the input size is too small, then exit early
 if (aWithSize and (aInLen<(SizeOf(TpvUInt64)+SizeOf(TpvUInt32)))) or ((not aWithSize) and (aInLen<SizeOf(TpvUInt32))) then begin
  result:=false;
  exit;
 end;

 // Setup stuff
 InputPointer:=aInData;
 InputEnd:=@PpvUInt8Array(InputPointer)^[aInLen];

 if aWithSize then begin
  OutputSize:=PpvUInt64(InputPointer)^;
{$ifdef BIG_ENDIAN}
  OutputSize:=((OutputSize and TpvUInt64($ff00000000000000)) shr 56) or
              ((OutputSize and TpvUInt64($00ff000000000000)) shr 40) or
              ((OutputSize and TpvUInt64($0000ff0000000000)) shr 24) or
              ((OutputSize and TpvUInt64($000000ff00000000)) shr 8) or
              ((OutputSize and TpvUInt64($00000000ff000000)) shl 8) or
              ((OutputSize and TpvUInt64($0000000000ff0000)) shl 24) or
              ((OutputSize and TpvUInt64($000000000000ff00)) shl 40) or
              ((OutputSize and TpvUInt64($00000000000000ff)) shl 56);
{$endif}
  inc(PpvUInt64(InputPointer));
 end else begin
  if aOutputSize>=0 then begin
   OutputSize:=aOutputSize;
  end else begin
   OutputSize:=0;
  end;
 end;

 if OutputSize=0 then begin
  result:=true;
  exit;
 end;

 aDestLen:=OutputSize;

 if (aOutputSize>=0) and (aDestLen<>TpvUInt64(aOutputSize)) then begin
  result:=false;
  aDestLen:=0;
  exit;
 end;

 Allocated:=not assigned(aDestData);
 if Allocated then begin
  if ((not aWithSize) and (aOutputSize<=0)) or (OutputSize=0) then begin
   result:=false;
   aDestLen:=0;
   exit;
  end;
  GetMem(aDestData,OutputSize);
 end;

 OutputPointer:=aDestData;
 OutputEnd:=@PpvUInt8Array(OutputPointer)^[OutputSize];

 result:=true;

 Tag:=TpvUInt32(pointer(InputPointer)^);
{$ifdef BIG_ENDIAN}
 Tag:=((Tag and TpvUInt64($ff000000) shr 24) or
      ((Tag and TpvUInt64($00ff0000) shr 8) or
      ((Tag and TpvUInt64($0000ff00) shl 8) or
      ((Tag and TpvUInt64($000000ff) shl 24);
{$endif}
 inc(InputPointer,SizeOf(TpvUInt32));
 BitCount:=32;

{Tag:=0;
 BitCount:=0;}

 LastMatchOffset:=0;
 LastWasMatch:=false;

 while TpvPtrUInt(InputPointer)<TpvPtrUInt(InputEnd) do begin

  OK:=true;

  if GetBit<>0 then begin
   if GetBit<>0 then begin
    if GetBit<>0 then begin
     Offset:=GetBits(4);
     if Offset<>0 then begin
      CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
      if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
         (TpvPtrUInt(OutputPointer)>=TpvPtrUInt(OutputEnd)) or not OK then begin
       result:=false;
       break;
      end;
      OutputPointer^:=CopyFromPointer^;
     end else begin
      if (TpvPtrUInt(OutputPointer)>=TpvPtrUInt(OutputEnd)) or not OK then begin
       result:=false;
       break;
      end;
      OutputPointer^:=0;
     end;
     inc(OutputPointer);
     LastWasMatch:=false;
    end else begin
     if (TpvPtrUInt(InputPointer)>=TpvPtrUInt(InputEnd)) or not OK then begin
      result:=false;
      break;
     end;
     Value:=InputPointer^;
     inc(InputPointer);
     Offset:=Value shr 1;
     Len:=(Value and 1)+2;
     if Offset<>0 then begin
      CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
      if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
         ((TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len))>TpvPtrUInt(OutputEnd)) then begin
       result:=false;
       break;
      end;
      DoMoveCopy(CopyFromPointer,OutputPointer,Len);
      inc(OutputPointer,Len);
      LastMatchOffset:=Offset;
      LastWasMatch:=true;
     end else begin
      break;
     end;
    end;
   end else begin
    Offset:=GetGamma;
    if (Offset=2) and not LastWasMatch then begin
     if not OK then begin
      result:=false;
      break;
     end;
     Offset:=LastMatchOffset;
     Len:=GetGamma;
     CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
     if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
        ((TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len))>TpvPtrUInt(OutputEnd)) or not OK then begin
      result:=false;
      break;
     end;
     DoMoveCopy(CopyFromPointer,OutputPointer,Len);
     inc(OutputPointer,Len);
    end else begin
     if LastWasMatch then begin
      dec(Offset,2);
     end else begin
      dec(Offset,3);
     end;
     if (TpvPtrUInt(InputPointer)>=TpvPtrUInt(InputEnd)) or not OK then begin
      result:=false;
      break;
     end;
     Offset:=(Offset shl 8) or (InputPointer^);
     inc(InputPointer);
     Len:=GetGamma;
     if not OK then begin
      result:=false;
      break;
     end;
     if Offset<128 then begin
      inc(Len,2);
     end else begin
      if Offset>=32000 then begin
       inc(Len);
      end;
      if Offset>=1280 then begin
       inc(Len);
      end;
     end;
     CopyFromPointer:=pointer(TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(Offset)));
     if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData)) or
        ((TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len))>TpvPtrUInt(OutputEnd)) then begin
      result:=false;
      break;
     end;
     DoMoveCopy(CopyFromPointer,OutputPointer,Len);
     inc(OutputPointer,Len);
     LastMatchOffset:=Offset;
    end;
    LastWasMatch:=true;
   end;
  end else begin
   if (TpvPtrUInt(InputPointer)>=TpvPtrUInt(InputEnd)) or
      (TpvPtrUInt(OutputPointer)>=TpvPtrUInt(OutputEnd)) or not OK then begin
    result:=false;
    break;
   end;
   OutputPointer^:=InputPointer^;
   inc(InputPointer);
   inc(OutputPointer);
   LastWasMatch:=false;
  end;

 end;

 OutputSize:=TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(aDestData));

 if (not aWithSize) and (aOutputSize<0) then begin
  aDestLen:=OutputSize;
 end;

 if not (result and (aDestLen=OutputSize)) then begin
  result:=false;
  aDestLen:=0;
  if Allocated then begin
   FreeMem(aDestData);
   aDestData:=nil;
  end;
 end;

end;

{$ifend}

initialization
end.
