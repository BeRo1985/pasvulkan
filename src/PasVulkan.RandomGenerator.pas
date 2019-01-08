(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2019, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.RandomGenerator;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses {$ifdef windows}Windows,MMSystem,{$endif}
     {$ifdef unix}dl,BaseUnix,Unix,UnixType,{$endif}
     SysUtils,Classes,Math,SyncObjs,
     PasVulkan.Types;

type PpvRandomGeneratorPCG32=^TpvRandomGeneratorPCG32;
     TpvRandomGeneratorPCG32=record
      State:TpvUInt64;
      Increment:TpvUInt64;
     end;

     PpvRandomGeneratorSplitMix64=^TpvRandomGeneratorSplitMix64;
     TpvRandomGeneratorSplitMix64=TpvUInt64;

     PpvRandomGeneratorLCG64=^TpvRandomGeneratorLCG64;
     TpvRandomGeneratorLCG64=TpvUInt64;

     PpvRandomGeneratorMWC=^TpvRandomGeneratorMWC;
     TpvRandomGeneratorMWC=record
      x:TpvUInt32;
      y:TpvUInt32;
      c:TpvUInt32;
     end;

     PpvRandomGeneratorXorShift128=^TpvRandomGeneratorXorShift128;
     TpvRandomGeneratorXorShift128=record
      x,y,z,w:TpvUInt32;
     end;

     PpvRandomGeneratorXorShift128Plus=^TpvRandomGeneratorXorShift128Plus;
     TpvRandomGeneratorXorShift128Plus=record
      s:array[0..1] of TpvUInt64;
     end;

     PpvRandomGeneratorXorShift1024=^TpvRandomGeneratorXorShift1024;
     TpvRandomGeneratorXorShift1024=record
      s:array[0..15] of TpvUInt64;
      p:TpvInt32;
     end;

     PpvRandomGeneratorCMWC4096=^TpvRandomGeneratorCMWC4096;
     TpvRandomGeneratorCMWC4096=record
      Q:array[0..4095] of TpvUInt64;
      QC:TpvUInt64;
      QJ:TpvUInt64;
     end;

     PpvRandomGeneratorState=^TpvRandomGeneratorState;
     TpvRandomGeneratorState=record
      LCG64:TpvRandomGeneratorLCG64;
      XorShift1024:TpvRandomGeneratorXorShift1024;
      CMWC4096:TpvRandomGeneratorCMWC4096;
     end;

     TpvRandomGenerator=class
      private
       fState:TpvRandomGeneratorState;
       fGuassianFloatUseLast:boolean;
       fGuassianFloatLast:single;
       fGuassianDoubleUseLast:boolean;
       fGuassianDoubleLast:double;
       fCriticalSection:TCriticalSection;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Reinitialize(FixedSeed:TpvUInt64=TpvUInt64($ffffffffffffffff));
       function Get32:TpvUInt32;
       function Get64:TpvUInt64;
       function Get(Limit:TpvUInt32):TpvUInt32;
       function GetFloat:single; // -1.0.0 .. 1.0
       function GetFloatAbs:single; // 0.0 .. 1.0
       function GetDouble:double; // -1.0.0 .. 1.0
       function GetDoubleAbs:Double; // 0.0 .. 1.0
       function GetGuassianFloat:single; // -1.0 .. 1.0
       function GetGuassianFloatAbs:single; // 0.0 .. 1.0
       function GetGuassianDouble:double; // -1.0 .. 1.0
       function GetGuassianDoubleAbs:double; // 0.0 .. 1.0
       function GetGuassian(Limit:TpvUInt32):TpvUInt32;
     end;

     TRandomUnique32BitSequence=class
      private
       fIndex:TpvUInt32;
       fIntermediateOffset:TpvUInt32;
       function PermuteQPR(x:TpvUInt32):TpvUInt32;
      public
       constructor Create(const Seed1:TpvUInt32=$b46f23c7;const Seed2:TpvUInt32=$a54c2364);
       destructor Destroy; override;
       function Next:TpvUInt32;
     end;

function PCG32Next(var State:TpvRandomGeneratorPCG32):TpvUInt64; {$ifdef caninline}inline;{$endif}

function SplitMix64Next(var State:TpvRandomGeneratorSplitMix64):TpvUInt64; {$ifdef caninline}inline;{$endif}

function LCG64Next(var State:TpvRandomGeneratorLCG64):TpvUInt64; {$ifdef caninline}inline;{$endif}

function XorShift128Next(var State:TpvRandomGeneratorXorShift128):TpvUInt32; {$ifdef caninline}inline;{$endif}

function XorShift128PlusNext(var State:TpvRandomGeneratorXorShift128Plus):TpvUInt64; {$ifdef caninline}inline;{$endif}
procedure XorShift128PlusJump(var State:TpvRandomGeneratorXorShift128Plus);

function XorShift1024Next(var State:TpvRandomGeneratorXorShift1024):TpvUInt64; {$ifdef caninline}inline;{$endif}
procedure XorShift1024Jump(var State:TpvRandomGeneratorXorShift1024);

function CMWC4096Next(var State:TpvRandomGeneratorCMWC4096):TpvUInt64; {$ifdef caninline}inline;{$endif}

implementation

function PCG32Next(var State:TpvRandomGeneratorPCG32):TpvUInt64; {$ifdef caninline}inline;{$endif}
var OldState:TpvUInt64;
    XorShifted,Rot:TpvUInt32;
begin
 OldState:=State.State;
 State.State:=(OldState*TpvUInt64(6364136223846793005))+(State.Increment or 1);
 XorShifted:=TpvUInt64((OldState shr 18) xor OldState) shr 27;
 Rot:=OldState shr 59;
 result:=(XorShifted shr rot) or (TpvUInt64(XorShifted) shl ((-Rot) and 31));
end;

function SplitMix64Next(var State:TpvRandomGeneratorSplitMix64):TpvUInt64; {$ifdef caninline}inline;{$endif}
var z:TpvUInt64;
begin
 State:=State+{$ifndef fpc}TpvUInt64{$endif}($9e3779b97f4a7c15);
 z:=State;
 z:=(z xor (z shr 30))*{$ifndef fpc}TpvUInt64{$endif}($bf58476d1ce4e5b9);
 z:=(z xor (z shr 27))*{$ifndef fpc}TpvUInt64{$endif}($94d049bb133111eb);
 result:=z xor (z shr 31);
end;

function LCG64Next(var State:TpvRandomGeneratorLCG64):TpvUInt64; {$ifdef caninline}inline;{$endif}
begin
 State:=(State*TpvUInt64(2862933555777941757))+TpvUInt64(3037000493);
 result:=State;
end;

function XorShift128Next(var State:TpvRandomGeneratorXorShift128):TpvUInt32; {$ifdef caninline}inline;{$endif}
var t:TpvUInt32;
begin
 t:=State.x xor (State.x shl 11);
 State.x:=State.y;
 State.y:=State.z;
 State.z:=State.w;
 State.w:=(State.w xor (State.w shr 19)) xor (t xor (t shr 8));
 result:=State.w;
end;

function XorShift128PlusNext(var State:TpvRandomGeneratorXorShift128Plus):TpvUInt64; {$ifdef caninline}inline;{$endif}
var s0,s1:TpvUInt64;
begin
 s1:=State.s[0];
 s0:=State.s[1];
 State.s[0]:=s0;
 s1:=s1 xor (s1 shl 23);
 State.s[1]:=((s1 xor s0) xor (s1 shr 18)) xor (s0 shr 5);
 result:=State.s[1]+s0;
end;

procedure XorShift128PlusJump(var State:TpvRandomGeneratorXorShift128Plus);
const Jump:array[0..1] of TpvUInt64=
       (TpvUInt64($8a5cd789635d2dff),
     		TpvUInt64($121fd2155c472f96));
var i,b:TpvInt32;
    s0,s1:TpvUInt64;
begin
 s0:=0;
 s1:=0;
 for i:=0 to 1 do begin
  for b:=0 to 63 do begin
	 if (Jump[i] and TpvUInt64(TpvUInt64(1) shl b))<>0 then begin
		s0:=s0 xor State.s[0];
		s1:=s1 xor State.s[1];
	 end;
   XorShift128PlusNext(State);
  end;
 end;
 State.s[0]:=s0;
 State.s[1]:=s1;
end;

function XorShift1024Next(var State:TpvRandomGeneratorXorShift1024):TpvUInt64; {$ifdef caninline}inline;{$endif}
var s0,s1:TpvUInt64;
begin
 s0:=State.s[State.p and 15];
 State.p:=(State.p+1) and 15;
 s1:=State.s[State.p];
 s1:=s1 xor (s1 shl 31);
 State.s[State.p]:=((s1 xor s0) xor (s1 shr 11)) xor (s0 shr 30);
 result:=State.s[State.p]*TpvUInt64(1181783497276652981);
end;

procedure XorShift1024Jump(var State:TpvRandomGeneratorXorShift1024);
const Jump:array[0..15] of TpvUInt64=
       (TpvUInt64($84242f96eca9c41d),
     		TpvUInt64($a3c65b8776f96855),
        TpvUInt64($5b34a39f070b5837),
        TpvUInt64($4489affce4f31a1e),
        TpvUInt64($2ffeeb0a48316f40),
        TpvUInt64($dc2d9891fe68c022),
        TpvUInt64($3659132bb12fea70),
        TpvUInt64($aac17d8efa43cab8),
        TpvUInt64($c4cb815590989b13),
        TpvUInt64($5ee975283d71c93b),
        TpvUInt64($691548c86c1bd540),
        TpvUInt64($7910c41d10a1e6a5),
        TpvUInt64($0b5fc64563b3e2a8),
        TpvUInt64($047f7684e9fc949d),
        TpvUInt64($b99181f2d8f685ca),
        TpvUInt64($284600e3f30e38c3));
var i,b,j:TpvInt32;
    t:array[0..15] of TpvUInt64;
begin
 for i:=0 to 15 do begin
  t[i]:=0;
 end;
 for i:=0 to 15 do begin
  for b:=0 to 63 do begin
	 if (Jump[i] and TpvUInt64(TpvUInt64(1) shl b))<>0 then begin
    for j:=0 to 15 do begin
   	 t[j]:=t[j] xor State.s[(j+State.p) and 15];
    end;
   end;
   XorShift1024Next(State);
  end;
 end;
 for i:=0 to 15 do begin
	State.s[(i+State.p) and 15]:=t[i];
 end;
end;

function CMWC4096Next(var State:TpvRandomGeneratorCMWC4096):TpvUInt64; {$ifdef caninline}inline;{$endif}
var x,t:TpvUInt64;
begin
 State.QJ:=(State.QJ+1) and high(State.Q);
 x:=State.Q[State.QJ];
 t:=(x shl 58)+State.QC;
 State.QC:=x shr 6;
 inc(t,x);
 if x<t then begin
  inc(State.QC);
 end;
 State.Q[State.QJ]:=t;
 result:=t;
end;

constructor TpvRandomGenerator.Create;
begin
 inherited Create;
 fCriticalSection:=TCriticalSection.Create;
 Reinitialize;
end;

destructor TpvRandomGenerator.Destroy;
begin
 fCriticalSection.Free;
 inherited Destroy;
end;

{$ifdef win32}
type HCRYPTPROV=TpvUInt32;

const PROV_RSA_FULL=1;
      CRYPT_VERIFYCONTEXT=$f0000000;
      CRYPT_SILENT=$00000040;
      CRYPT_NEWKEYSET=$00000008;

function CryptAcquireContext(var phProv:HCRYPTPROV;pszContainer:PAnsiChar;pszProvider:PAnsiChar;dwProvType:TpvUInt32;dwFlags:TpvUInt32):LONGBOOL; stdcall; external advapi32 name 'CryptAcquireContextA';
function CryptReleaseContext(hProv:HCRYPTPROV;dwFlags:DWORD):BOOL; stdcall; external advapi32 name 'CryptReleaseContext';
function CryptGenRandom(hProv:HCRYPTPROV;dwLen:DWORD;pbBuffer:Pointer):BOOL; stdcall; external advapi32 name 'CryptGenRandom';

function CoCreateGuid(var guid:TGUID):HResult; stdcall; external 'ole32.dll';
{$endif}

{$ifdef fpc}
{$notes off}
{$endif}
procedure TpvRandomGenerator.Reinitialize(FixedSeed:TpvUInt64=TpvUInt64($ffffffffffffffff));
const N=25;
      CountStateQWords=(SizeOf(TpvRandomGeneratorState) div SizeOf(TpvUInt64));
type PStateQWords=^TStateQWords;
     TStateQWords=array[0..CountStateQWords-1] of TpvUInt64;
var i,j:TpvInt32;
    UnixTimeInMilliSeconds:TpvInt64;
    HashState:TpvUInt64;
    LCG64:TpvRandomGeneratorLCG64;
    PCG32:TpvRandomGeneratorPCG32;
    SplitMix64:TpvRandomGeneratorSplitMix64;
{$ifdef unix}
    f:file of TpvUInt32;
    ura,urb:TpvUInt32;
    OLdFileMode:TpvInt32;
{$else}
{$ifdef win32}
    lpc,lpf:TpvInt64;
    pp,p:pwidechar;
    st:ansistring;
{$endif}
{$endif}
{$ifdef win32}
 function GenerateRandomBytes(var Buffer;Bytes:Cardinal):boolean;
 var CryptProv:HCRYPTPROV;
 begin
  try
   if not CryptAcquireContext(CryptProv,nil,nil,PROV_RSA_FULL,CRYPT_VERIFYCONTEXT{ or CRYPT_SILENT}) then begin
    if not CryptAcquireContext(CryptProv,nil,nil,PROV_RSA_FULL,CRYPT_NEWKEYSET) then begin
     result:=false;
     exit;
    end;
   end;
   FillChar(Buffer,Bytes,#0);
   result:=CryptGenRandom(CryptProv,Bytes,@Buffer);
   CryptReleaseContext(CryptProv,0);
  except
   result:=false;
  end;
 end;
 function GetRandomGUIDGarbage:ansistring;
 var g:TGUID;
 begin
  CoCreateGUID(g);
  SetLength(result,sizeof(TGUID));
  Move(g,result[1],sizeof(TGUID));
 end;
{$endif}
begin
 fCriticalSection.Enter;
 try
  if FixedSeed=TpvUInt64($ffffffffffffffff) then begin
   UnixTimeInMilliSeconds:=round((SysUtils.Now-25569.0)*86400000.0);
{$ifdef nunix}
   ura:=0;
   urb:=0;
   OldFileMode:=FileMode;
   FileMode:=0;
   AssignFile(f,'/dev/urandom');
   {$i-}System.Reset(f,1);{$i+}
   if IOResult=0 then begin
    System.Read(f,ura);
    System.Read(f,urb);
    for i:=0 to CountStateDWords-1 do begin
     System.Read(f,PStateDWords(pointer(@fState))^[i]);
    end;
    CloseFile(f);
   end else begin
    AssignFile(f,'/dev/random');
    {$i-}System.Reset(f,1);{$i+}
    if IOResult=0 then begin
     System.Read(f,ura);
     System.Read(f,urb);
     for i:=0 to CountStateDWords-1 do begin
      System.Read(f,PStateDWords(pointer(@fState))^[i]);
     end;
     CloseFile(f);
    end else begin
     LCG64:=((TpvUInt64(UnixTimeInMilliSeconds) shl 31) or (TpvUInt64(UnixTimeInMilliSeconds) shr 33)) xor TpvUInt64($4c2a9d217a5cde81);
     for i:=0 to CountStateQWords-1 do begin
      PStateQWords(pointer(@fState))^[i]:=LCG64Next(LCG64);
     end;
    end;
   end;
   FileMode:=OldFileMode;
   SplitMix64:=TpvUInt64(UnixTimeInMilliSeconds) xor TpvUInt64($7a5cde814c2a9d21);
   for i:=0 to CountStateQWords-1 do begin
    PStateQWords(pointer(@fState))^[i]:=PStateQWords(pointer(@fState))^[i] xor SplitMix64Next(SplitMix64);
   end;
{$else}
{$ifdef win32}
   if not GenerateRandomBytes(fState,SizeOf(TpvRandomGeneratorState)) then begin
{$ifdef fpc}
    LCG64:=GetTickCount64;
{$else}
    LCG64:=GetTickCount;
{$endif}
    LCG64:=LCG64 xor (((TpvUInt64(UnixTimeInMilliSeconds) shl 31) or (TpvUInt64(UnixTimeInMilliSeconds) shr 33)) xor TpvUInt64($4c2a9d217a5cde81));
    for i:=0 to CountStateQWords-1 do begin
     PStateQWords(pointer(@fState))^[i]:=LCG64Next(LCG64);
    end;
   end;
   begin
    QueryPerformanceCounter(lpc);
    QueryPerformanceFrequency(lpf);
    PCG32.State:=lpc;
    PCG32.Increment:=lpf;
    SplitMix64:=(TpvUInt64(GetCurrentProcessId) shl 32) or GetCurrentThreadId;
    for i:=0 to CountStateQWords-1 do begin
     PStateQWords(pointer(@fState))^[i]:=PStateQWords(pointer(@fState))^[i] xor (PCG32Next(PCG32)+SplitMix64Next(SplitMix64));
    end;
   end;
   i:=0;
   HashState:=TpvUInt64(4695981039346656037);
   pp:=GetEnvironmentStringsW;
   if assigned(pp) then begin
    p:=pp;
    while assigned(p) and (p^<>#0) do begin
     while assigned(p) and (p^<>#0) do begin
      HashState:=(HashState xor word(p^))*TpvUInt64(1099511628211);
      PStateQWords(pointer(@fState))^[i]:=PStateQWords(pointer(@fState))^[i] xor HashState;
      inc(i);
      if i>=CountStateQWords then begin
       i:=0;
      end;
      inc(p);
     end;
     inc(p);
    end;
    FreeEnvironmentStringsW(pointer(p));
   end;
   pp:=pointer(GetCommandLineW);
   if assigned(pp) then begin
    p:=pp;
    while assigned(p) and (p^<>#0) do begin
     HashState:=(HashState xor word(p^))*TpvUInt64(1099511628211);
     PStateQWords(pointer(@fState))^[i]:=PStateQWords(pointer(@fState))^[i] xor HashState;
     inc(i);
     if i>=CountStateQWords then begin
      i:=0;
     end;
     inc(p);
    end;
   end;
   st:=GetRandomGUIDGarbage;
   for j:=1 to length(st) do begin
    HashState:=(HashState xor byte(st[j]))*TpvUInt64(1099511628211);
    PStateQWords(pointer(@fState))^[i]:=PStateQWords(pointer(@fState))^[i] xor HashState;
    inc(i);
    if i>=CountStateQWords then begin
     i:=0;
    end;
   end;
   SetLength(st,0);
{$else}
   SplitMix64:=TpvUInt64(UnixTimeInMilliSeconds) xor TpvUInt64($7a5cde814c2a9d21);
   for i:=0 to CountStateQWords-1 do begin
    PStateQWords(pointer(@fState))^[i]:=SplitMix64Next(SplitMix64);
   end;
{$endif}
{$endif}
  end else begin
   SplitMix64:=TpvUInt64(FixedSeed) xor TpvUInt64($7a5cde814c2a9d21);
   for i:=0 to CountStateQWords-1 do begin
    PStateQWords(pointer(@fState))^[i]:=SplitMix64Next(SplitMix64);
   end;
  end;
  XorShift1024Jump(fState.XorShift1024);
  fGuassianFloatUseLast:=false;
  fGuassianFloatLast:=0.0;
  fGuassianDoubleUseLast:=false;
  fGuassianDoubleLast:=0.0;
 finally
  fCriticalSection.Leave;
 end;
end;
{$ifdef fpc}
{$notes on}
{$endif}

function TpvRandomGenerator.Get32:TpvUInt32;
begin
 result:=Get64 shr 32;
end;

function TpvRandomGenerator.Get64:TpvUInt64;
begin
 fCriticalSection.Enter;
 try
  result:=LCG64Next(fState.LCG64)+
          XorShift1024Next(fState.XorShift1024)+
          CMWC4096Next(fState.CMWC4096);
 finally
  fCriticalSection.Leave;
 end;
end;

function TpvRandomGenerator.Get(Limit:TpvUInt32):TpvUInt32;
begin
 if (Limit and $ffff0000)=0 then begin
  result:=((Get32 shr 16)*Limit) shr 16;
 end else begin
  result:=(TpvUInt64(Get32)*Limit) shr 32;
 end;    
end;

function TpvRandomGenerator.GetFloat:single; // -1.0 .. 1.0
var t:TpvUInt32;
begin
 t:=Get32;
 t:=(((t shr 9) and $7fffff)+((t shr 8) and 1)) or $40000000;
 result:=single(pointer(@t)^)-3.0;
end;

function TpvRandomGenerator.GetFloatAbs:single; // 0.0 .. 1.0
var t:TpvUInt32;
begin
 t:=Get32;
 t:=(((t shr 10) and $3fffff)+((t shr 9) and 1)) or $40000000;
 result:=single(pointer(@t)^)-2.0;
end;

function TpvRandomGenerator.GetDouble:double; // -1.0 .. 1.0
var t:TpvUInt64;
begin
 t:=Get64;
 t:=(((t shr 12) and $fffffffffffff)+((t shr 11) and 1)) or $4000000000000000;
 result:=double(pointer(@t)^)-3.0;
end;

function TpvRandomGenerator.GetDoubleAbs:double; // 0.0 .. 1.0
var t:TpvInt64;
begin
 t:=Get64;
 t:=(((t shr 13) and $7ffffffffffff)+((t shr 12) and 1)) or $4000000000000000;
 result:=double(pointer(@t)^)-2.0;
end;

function TpvRandomGenerator.GetGuassianFloat:single; // -1.0 .. 1.0
var x1,x2,w:single;
    i:TpvUInt32;
begin
 if fGuassianFloatUseLast then begin
  fGuassianFloatUseLast:=false;
  result:=fGuassianFloatLast;
 end else begin
  i:=0;
  repeat
   x1:=GetFloat;
   x2:=GetFloat;
   w:=sqr(x1)+sqr(x2);
   inc(i);
  until ((i and $80000000)<>0) or (w<1.0);
  if (i and $80000000)<>0 then begin
   result:=x1;
   fGuassianFloatLast:=x2;
   fGuassianFloatUseLast:=true;
  end else if abs(w)<1e-18 then begin
   result:=0.0;
  end else begin
   w:=sqrt(((-2.0)*ln(w))/w);
   result:=x1*w;
   fGuassianFloatLast:=x2*w;
   fGuassianFloatUseLast:=true;
  end;
 end;
 if result<-1.0 then begin
  result:=-1.0;
 end else if result>1.0 then begin
  result:=1.0;
 end;
end;

function TpvRandomGenerator.GetGuassianFloatAbs:single; // 0.0 .. 1.0
begin
 result:=(GetGuassianFloat+1.0)*0.5;
 if result<0.0 then begin
  result:=0.0;
 end else if result>1.0 then begin
  result:=1.0;
 end;
end;

function TpvRandomGenerator.GetGuassianDouble:double; // -1.0 .. 1.0
var x1,x2,w:double;
    i:TpvUInt32;
begin
 if fGuassianDoubleUseLast then begin
  fGuassianDoubleUseLast:=false;
  result:=fGuassianDoubleLast;
 end else begin
  i:=0;
  repeat
   x1:=GetDouble;
   x2:=GetDouble;
   w:=sqr(x1)+sqr(x2);
   inc(i);
  until ((i and $80000000)<>0) or (w<1.0);
  if (i and $80000000)<>0 then begin
   result:=x1;
   fGuassianDoubleLast:=x2;
   fGuassianDoubleUseLast:=true;
  end else if abs(w)<1e-18 then begin
   result:=0.0;
  end else begin
   w:=sqrt(((-2.0)*ln(w))/w);
   result:=x1*w;
   fGuassianDoubleLast:=x2*w;
   fGuassianDoubleUseLast:=true;
  end;
 end;
 if result<-1.0 then begin
  result:=-1.0;
 end else if result>1.0 then begin
  result:=1.0;
 end;
end;

function TpvRandomGenerator.GetGuassianDoubleAbs:double; // 0.0 .. 1.0
begin
 result:=(GetGuassianDouble+1.0)*0.5;
 if result<0.0 then begin
  result:=0.0;
 end else if result>1.0 then begin
  result:=1.0;
 end;
end;

function TpvRandomGenerator.GetGuassian(Limit:TpvUInt32):TpvUInt32;
begin
 result:=round(GetGuassianDoubleAbs*((Limit-1)+0.25));
end;

constructor TRandomUnique32BitSequence.Create(const Seed1:TpvUInt32=$b46f23c7;const Seed2:TpvUInt32=$a54c2364);
begin
 inherited Create;
 fIndex:=PermuteQPR(PermuteQPR(Seed1)+$682f0161);
 fIntermediateOffset:=PermuteQPR(PermuteQPR(Seed2)+$46790905);
end;

destructor TRandomUnique32BitSequence.Destroy;
begin
 inherited Destroy;
end;

function TRandomUnique32BitSequence.PermuteQPR(x:TpvUInt32):TpvUInt32;
const Prime=TpvUInt32(4294967291);
begin
 if x>=Prime then begin
  result:=x;
 end else begin
  result:={$ifdef fpc}qword{$else}uint64{$endif}(x*x) mod Prime;
  if x>(Prime shr 1) then begin
   result:=Prime-result;
  end;
 end;
end;

function TRandomUnique32BitSequence.Next:TpvUInt32;
begin
 result:=PermuteQPR((PermuteQPR(fIndex)+fIntermediateOffset) xor $5bf03635);
 inc(fIndex);
end;

end.
