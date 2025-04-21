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
unit PasVulkan.Hash.RapidHash;
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

// This RapidHash implementation is endian-dependent, so the results are not interchangeable between
// different endian architectures. So use it only for internal purposes, like hash tables or similar, 
// but not for external purposes like file hashes or similar. The RapidHash algorithm is not a 
// cryptographic hash function as well, so don't use it for cryptographic purposes! However, big-endian
// systems are mostly dead, so this is not a big problem, when you're using it for targets with the
// same endian architecture.

type TpvHashRapidHash=class
      public
       const RapidSeed=TpvUInt64($bdd89aa982704029);
             Secret0=TpvUInt64($2d358dccaa6c78a5);
             Secret1=TpvUInt64($8bb84b93962eacc9);
             Secret2=TpvUInt64($4b33a62ed433d4a3);
       type TMessageDigest=TpvUInt64;
            PMessageDigest=^TMessageDigest;
      private
       class procedure MUM(var aA,aB:TpvUInt64); static; {$ifndef cpuamd64}inline;{$endif}
       class function Mix(aA,aB:TpvUInt64):TpvUInt64; static; inline;
{$ifdef BIG_ENDIAN}
       class function Read32(const aData:Pointer):TpvUInt32; static; inline;
       class function Read64(const aData:Pointer):TpvUInt64; static; inline;
       class function ReadSmall(const aData:Pointer;const aDataLength:TpvSizeUInt):TpvUInt64; static; inline;
{$endif}
      public
       class function Process(const aKey:pointer;const aLength:TpvSizeUInt;aSeed:TpvUInt64=RapidSeed):TMessageDigest; static;
     end;

implementation

{ TpvHashRapidHash }

class procedure TpvHashRapidHash.MUM(var aA,aB:TpvUInt64);{$ifdef cpuamd64} assembler; {$ifdef fpc}nostackframe;{$endif} 
asm
{$ifndef fpc}
 .noframe
{$endif}
{$if defined(Windows)}
 // Pointer of aA is in rcx
 // Pointer of aB is in rdx
 // Save the pointers
 mov r8,rcx
 mov r9,rdx
 // Load aA and aB
 mov rax,qword ptr [rcx]
 mov rdx,qword ptr [rdx]
 // Multiply aA and aB
 mul rdx
 // Lo is in rax, Hi is in rdx 
 // Xor aA and aB with the result
 xor rax,qword ptr [r8]
 xor rdx,qword ptr [r9]
 // Store the result back to aA and aB
 mov qword ptr [r8],rax
 mov qword ptr [r9],rdx
{$else}
 // Pointer of aA is in rdi
 // Pointer of aB is in rsi
 // Save the pointers
 mov r8,rdi
 mov r9,rsi
 // Load aA and aB
 mov rax,qword ptr [rdi]
 mov rdx,qword ptr [rsi]
 // Multiply aA and aB
 mul rdx
 // Lo is in rax, Hi is in rdx 
 // Xor aA and aB with the result
 xor rax,qword ptr [r8]
 xor rdx,qword ptr [r9]
 // Store the result back to aA and aB
 mov qword ptr [r8],rax
 mov qword ptr [r9],rdx
{$ifend}
end;
{$else} 
var ha,hb,la,lb,hi,lo:TpvUInt64;
    rh,rm0,rm1,rl:TpvUInt64;
    t:TpvUInt64;
    c:TpvUInt64;
begin
 ha:=aA shr 32;
 hb:=aB shr 32;
 la:=TpvUInt32(aA);
 lb:=TpvUInt32(aB);
 rh:=ha*hb;
 rm0:=ha*lb;
 rm1:=hb*la;
 rl:=la*lb;
 t:=rl+(rm0 shl 32);
 c:=ord(t<rl) and 1;
 lo:=t+(rm1 shl 32);
 inc(c,ord(lo<t) and 1);
 hi:=rh+(rm0 shr 32)+(rm1 shr 32)+c;
 aA:=aA xor lo;
 aB:=aB xor hi;
end;
{$endif}

class function TpvHashRapidHash.Mix(aA,aB:TpvUInt64):TpvUInt64;
begin
 MUM(aA,aB);
 result:=aA xor aB;
end; 

{$ifdef BIG_ENDIAN}
class function TpvHashRapidHash.Read32(const aData:Pointer):TpvUInt32;
begin
 result:=PpvUInt32(aData)^;
 result:=(result shl 24) or ((result and TpvUInt32($00ff0000)) shr 8) or ((result and TpvUInt32($0000ff00)) shl 8) or (result shr 24);
end;

class function TpvHashRapidHash.Read64(const aData:Pointer):TpvUInt64;
begin
 result:=PpvUInt64(aData)^;
 result:=(result shl 56) or 
         ((result and TpvUInt64($00ff000000000000)) shr 8) or 
         ((result and TpvUInt64($0000ff0000000000)) shl 8) or 
         ((result and TpvUInt64($000000ff00000000)) shr 24) or 
         ((result and TpvUInt64($00000000ff000000)) shl 24) or 
         ((result and TpvUInt64($0000000000ff0000)) shr 40) or 
         ((result and TpvUInt64($000000000000ff00)) shl 40) or
         (result shr 56);
end;

class function TpvHashRapidHash.ReadSmall(const aData:Pointer;const aDataLength:TpvSizeUInt):TpvUInt64;
begin
 result:=(PpvUInt8Array(aData)^[0] shl 56) or (PpvUInt8Array(aData)^[TpvPtrUInt(aDataLength) shr 1] shl 32) or PpvUInt8Array(aData)^[TpvPtrUInt(aDataLength)-1];
end;
{$endif}

class function TpvHashRapidHash.Process(const aKey:pointer;const aLength:TpvSizeUInt;aSeed:TpvUInt64):TMessageDigest;
var p,pLast:PpvUInt8;
    i:TpvSizeUInt;
    a,b:TpvUInt64;
    Delta:TpvUInt64;
    See1,See2:TpvUInt64;
begin
 p:=aKey;
 aSeed:=aSeed xor (Mix(aSeed xor Secret0,Secret1) xor aLength);
 if aLength<=16 then begin
  if aLength>=4 then begin
   pLast:=@PpvUInt8Array(aKey)^[aLength-4];
{$ifdef BIG_ENDIAN}   
   a:=(Read32(p) shl 32) or Read32(pLast);
{$else}
   a:=(PpvUInt32(p)^ shl 32) or PpvUInt32(pLast)^;
{$endif}
   Delta:=(aLength and 24) shr (aLength shr 3);
{$ifdef BIG_ENDIAN}
   b:=(Read32(@PpvUInt8Array(aKey)^[Delta]) shl 32) or Read32(@PpvUInt8Array(aKey)^[aLength-Delta]);
{$else}
   b:=(PpvUInt32(@PpvUInt8Array(aKey)^[Delta])^ shl 32) or PpvUInt32(@PpvUInt8Array(aKey)^[aLength-Delta])^;
{$endif}
  end else if aLength>0 then begin
{$ifdef BIG_ENDIAN}  
   a:=ReadSmall(p,aLength);
{$else}
   a:=(TpvUInt64(PpvUInt8Array(p)^[0]) shl 56) or (TpvUInt64(PpvUInt8Array(p)^[TpvPtrUInt(aLength) shr 1]) shl 32) or PpvUInt8Array(p)^[TpvPtrUInt(aLength)-1];
{$endif}
   b:=0;
  end else begin
   a:=0;
   b:=0;
  end;
 end else begin
  i:=aLength;
  if i>48 then begin
   See1:=aSeed;
   See2:=aSeed;
   while i>=96 do begin
{$ifdef BIG_ENDIAN}
    aSeed:=Mix(Read64(p)^ xor Secret0,Read64(@PpvUInt8Array(p)^[8])^ xor aSeed);
    See1:=Mix(Read64(@PpvUInt8Array(p)^[16])^ xor Secret1,Read64(@PpvUInt8Array(p)^[24])^ xor See1);
    See2:=Mix(Read64(@PpvUInt8Array(p)^[32])^ xor Secret2,Read64(@PpvUInt8Array(p)^[40])^ xor See2);
    aSeed:=Mix(Read64(@PpvUInt8Array(p)^[48])^ xor Secret0,Read64(@PpvUInt8Array(p)^[56])^ xor aSeed);
    See1:=Mix(Read64(@PpvUInt8Array(p)^[64])^ xor Secret1,Read64(@PpvUInt8Array(p)^[72])^ xor See1);
    See2:=Mix(Read64(@PpvUInt8Array(p)^[80])^ xor Secret2,Read64(@PpvUInt8Array(p)^[88])^ xor See2);
{$else}
    aSeed:=Mix(PpvUInt64(p)^ xor Secret0,PpvUInt64(@PpvUInt8Array(p)^[8])^ xor aSeed);
    See1:=Mix(PpvUInt64(@PpvUInt8Array(p)^[16])^ xor Secret1,PpvUInt64(@PpvUInt8Array(p)^[24])^ xor See1);
    See2:=Mix(PpvUInt64(@PpvUInt8Array(p)^[32])^ xor Secret2,PpvUInt64(@PpvUInt8Array(p)^[40])^ xor See2);
    aSeed:=Mix(PpvUInt64(@PpvUInt8Array(p)^[48])^ xor Secret0,PpvUInt64(@PpvUInt8Array(p)^[56])^ xor aSeed);
    See1:=Mix(PpvUInt64(@PpvUInt8Array(p)^[64])^ xor Secret1,PpvUInt64(@PpvUInt8Array(p)^[72])^ xor See1);
    See2:=Mix(PpvUInt64(@PpvUInt8Array(p)^[80])^ xor Secret2,PpvUInt64(@PpvUInt8Array(p)^[88])^ xor See2);
{$endif}
    p:=@PpvUInt8Array(p)^[96];
    dec(i,96);
   end;
   if i>=48 then begin
{$ifdef BIG_ENDIAN}
    aSeed:=Mix(Read64(p)^ xor Secret0,Read64(@PpvUInt8Array(p)^[8])^ xor aSeed);
    See1:=Mix(Read64(@PpvUInt8Array(p)^[16])^ xor Secret1,Read64(@PpvUInt8Array(p)^[24])^ xor See1);
    See2:=Mix(Read64(@PpvUInt8Array(p)^[32])^ xor Secret2,Read64(@PpvUInt8Array(p)^[40])^ xor See2);
{$else}
    aSeed:=Mix(PpvUInt64(p)^ xor Secret0,PpvUInt64(@PpvUInt8Array(p)^[8])^ xor aSeed);
    See1:=Mix(PpvUInt64(@PpvUInt8Array(p)^[16])^ xor Secret1,PpvUInt64(@PpvUInt8Array(p)^[24])^ xor See1);
    See2:=Mix(PpvUInt64(@PpvUInt8Array(p)^[32])^ xor Secret2,PpvUInt64(@PpvUInt8Array(p)^[40])^ xor See2);
{$endif}
    p:=@PpvUInt8Array(p)^[48];
    dec(i,48);
   end;
   aSeed:=aSeed xor See1 xor See2;
  end;
  if i>16 then begin
{$ifdef BIG_ENDIAN}
   aSeed:=Mix(Read64(p)^ xor Secret2,Read64(@PpvUInt8Array(p)^[8])^ xor aSeed xor Secret1);
   if i>32 then begin
    aSeed:=Mix(Read64(@PpvUInt8Array(p)^[16])^ xor Secret2,Read64(@PpvUInt8Array(p)^[24])^ xor aSeed);
   end;
{$else}
   aSeed:=Mix(PpvUInt64(p)^ xor Secret2,PpvUInt64(@PpvUInt8Array(p)^[8])^ xor aSeed xor Secret1);
   if i>32 then begin
    aSeed:=Mix(PpvUInt64(@PpvUInt8Array(p)^[16])^ xor Secret2,PpvUInt64(@PpvUInt8Array(p)^[24])^ xor aSeed);
   end;
{$endif}
  end;
{$ifdef BIG_ENDIAN}
  a:=Read64(@PpvUInt8Array(aKey)^[i-16]);
  b:=Read64(@PpvUInt8Array(aKey)^[i-8]);
{$else}
  a:=PpvUInt64(@PpvUInt8Array(aKey)^[i-16])^;
  b:=PpvUInt64(@PpvUInt8Array(aKey)^[i-8])^;
{$endif}
 end;
 a:=a xor Secret1;
 b:=b xor aSeed;
 MUM(a,b);
 result:=Mix(a xor Secret0 xor aLength,b xor Secret1);
end;

end.
