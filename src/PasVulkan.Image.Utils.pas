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
unit PasVulkan.Image.Utils;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(fpc) and defined(Android) and defined(cpuarm)}
 {$define UsePNGExternalLibrary}
{$else}
 {$undef UsePNGExternalLibrary}
{$ifend}

interface

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Types;

procedure RGBAAlphaBleeding(const aData:Pointer;const aWidth,aHeight:TpvSizeInt;const a16Bit:Boolean=false);

implementation

uses PasVulkan.Utils;

procedure RGBAAlphaBleeding(const aData:Pointer;const aWidth,aHeight:TpvSizeInt;const a16Bit:Boolean);
const Offsets:array[0..8,0..1] of TpvInt32=((-1,-1),(0,-1),(1,-1),(-1,0),(0,0),(1,0),(-1,1),(0,1),(1,1));
type TpvSizeIntArray=array of TpvSizeInt;
var Size,i,j,k,Index,x,y,s,t,CountPending,CountNextPending,p,Count:TpvSizeInt;
    r,g,b:TpvInt32;
    Opaque:array of TpvUInt8;
    Loose:array of Boolean;
    Pending,NextPending:TpvSizeIntArray;
    IsLoose:boolean;
begin

 Opaque:=nil;
 Loose:=nil;
 Pending:=nil;
 NextPending:=nil;

 try

  Size:=aWidth*aHeight;

  SetLength(Opaque,Size);
  SetLength(Loose,Size);
  SetLength(Pending,Size);
  SetLength(NextPending,Size);

  FillChar(Opaque[0],Size*SizeOf(TpvUInt8),#0);
  FillChar(Loose[0],Size*SizeOf(Boolean),#0);
  FillChar(Pending[0],Size*SizeOf(TpvSizeInt),#0);
  FillChar(NextPending[0],Size*SizeOf(TpvSizeInt),#0);

  CountPending:=0;

  j:=3;
  for i:=0 to Size-1 do begin
   if ((not a16Bit) and (PpvUInt8Array(aData)^[j]=0)) or (a16Bit and (PpvUInt16Array(aData)^[j]=0)) then begin
    IsLoose:=true;
    y:=i div aWidth;
    x:=i-(y*aWidth);
    for k:=Low(Offsets) to High(Offsets) do begin
     s:=Offsets[k,0];
     t:=Offsets[k,1];
     if ((x+s)>=0) and ((x+s)<aWidth) and ((y+t)>=0) and ((y+t)<aHeight) then begin
      Index:=j+((s+(t*aWidth)) shl 2);
      if ((not a16Bit) and (PpvUInt8Array(aData)^[Index]<>0)) or (a16Bit and (PpvUInt16Array(aData)^[Index]<>0)) then begin
       IsLoose:=false;
       break;
      end;
     end;
    end;
    if IsLoose then begin
     Loose[i]:=true;
    end else begin
     Pending[CountPending]:=i;
     inc(CountPending);
    end;
   end else begin
    Opaque[i]:=$ff;
   end;
   inc(j,4);
  end;

  while CountPending>0 do begin

   CountNextPending:=0;

   for p:=0 to CountPending-1 do begin

    j:=Pending[p];
    i:=j shl 2;

    y:=j div aWidth;
    x:=j-(y*aWidth);

    r:=0;
    g:=0;
    b:=0;

    Count:=0;

    for k:=Low(Offsets) to High(Offsets) do begin
     s:=Offsets[k,0];
     t:=Offsets[k,1];
     if ((x+s)>=0) and ((x+s)<aWidth) and ((y+t)>=0) and ((y+t)<aHeight) then begin
      Index:=j+(s+(t*aWidth));
      if (Opaque[Index] and 1)<>0 then begin
       Index:=Index shl 2;
       if a16Bit then begin
        inc(r,PpvUInt16Array(aData)^[Index+0]);
        inc(g,PpvUInt16Array(aData)^[Index+1]);
        inc(b,PpvUInt16Array(aData)^[Index+2]);
       end else begin
        inc(r,PpvUInt8Array(aData)^[Index+0]);
        inc(g,PpvUInt8Array(aData)^[Index+1]);
        inc(b,PpvUInt8Array(aData)^[Index+2]);
       end;
       inc(Count);
      end;
     end;
    end;

    if Count>0 then begin
     if a16Bit then begin
      PpvUInt16Array(aData)^[i+0]:=r div Count;
      PpvUInt16Array(aData)^[i+1]:=g div Count;
      PpvUInt16Array(aData)^[i+2]:=b div Count;
     end else begin
      PpvUInt8Array(aData)^[i+0]:=r div Count;
      PpvUInt8Array(aData)^[i+1]:=g div Count;
      PpvUInt8Array(aData)^[i+2]:=b div Count;
     end;
     Opaque[j]:=$fe;
     for k:=Low(Offsets) to High(Offsets) do begin
      s:=Offsets[k,0];
      t:=Offsets[k,1];
      if ((x+s)>=0) and ((x+s)<aWidth) and ((y+t)>=0) and ((y+t)<aHeight) then begin
       Index:=j+(s+(t*aWidth));
       if Loose[Index] then begin
        Loose[Index]:=false;
        NextPending[CountNextPending]:=Index;
        inc(CountNextPending);
       end;
      end;
     end;
    end else begin
     NextPending[CountNextPending]:=j;
     inc(CountNextPending);
    end;

   end;

   if CountNextPending>0 then begin
    for p:=0 to CountPending-1 do begin
     Opaque[Pending[p]]:=Opaque[Pending[p]] shr 1;
    end;
   end;

   TpvSwap<TpvSizeIntArray>.Swap(Pending,NextPending);
   CountPending:=CountNextPending;

  end;

 finally
  Opaque:=nil;
  Loose:=nil;
  Pending:=nil;
  NextPending:=nil;
 end;

end;

end.
