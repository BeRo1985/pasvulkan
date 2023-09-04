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
unit PasVulkan.Compression.LZBRSF;
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
     PasVulkan.Math,
     PasVulkan.Types;

type PpvLZBRSFMode=^TpvLZBRSFMode;
     TpvLZBRSFMode=
      (
       VeryFast,
       Fast,
       Medium,
       Slow
      );

function LZBRSFCompress(const aInData:TpvPointer;const aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64;const aMode:TpvLZBRSFMode):boolean;

function LZBRSFDecompress(const aInData:TpvPointer;aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64):boolean;

implementation

function LZBRSFCompress(const aInData:TpvPointer;const aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64;const aMode:TpvLZBRSFMode):boolean;
const HashBits=16;
      HashSize=1 shl HashBits;
      HashMask=HashSize-1;
      HashShift=32-HashBits;
      WindowSize=32768;
      WindowMask=WindowSize-1;
      MinMatch=3;
      MaxMatch=258;
      MaxOffset=16777216;
      MultiplyDeBruijnBytePosition:array[0..31] of TpvUInt8=(0,0,3,0,3,1,3,0,3,2,2,1,3,2,0,1,3,3,1,2,2,2,2,0,3,1,2,0,1,0,1,1);
type PHashTable=^THashTable;
     THashTable=array[0..HashSize-1] of PpvUInt8;
     PChainTable=^TChainTable;
     TChainTable=array[0..WindowSize-1] of TpvPointer;
     PThreeBytes=^TThreeBytes;
     TThreeBytes=array[0..2] of TpvUInt8;
     PBytes=^TBytes;
     TBytes=array[0..$7ffffffe] of TpvUInt8;
var LiteralStart:PpvUInt8;
    LiteralLength:TpvUInt64;
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
 procedure DoOutputUInt8(const aValue:TpvUInt8);
 begin
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt8)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt8)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  PpvUInt8(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue;
  inc(aDestLen,SizeOf(TpvUInt8));
 end;
 procedure DoOutputUInt16(const aValue:TpvUInt16);
 begin
{$ifdef little_endian}
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt16)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt16)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  PpvUInt16(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue;
  inc(aDestLen,SizeOf(TpvUInt16));
{$else}
  DoOutputUInt8((aValue shr 0) and $ff);
  DoOutputUInt8((aValue shr 8) and $ff);
{$endif}
 end;
 procedure DoOutputUInt24(const aValue:TpvUInt32);
 begin
{$ifdef little_endian}
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
 procedure DoOutputUInt32(const aValue:TpvUInt32);
 begin
{$ifdef little_endian}
  if AllocatedDestSize<(aDestLen+SizeOf(TpvUInt32)) then begin
   AllocatedDestSize:=(aDestLen+SizeOf(TpvUInt32)) shl 1;
   ReallocMem(aDestData,AllocatedDestSize);
  end;
  PpvUInt32(Pointer(@PBytes(aDestData)^[aDestLen]))^:=aValue and $ffff;
  inc(aDestLen,SizeOf(TpvUInt32));
{$else}
  DoOutputUInt8((aValue shr 0) and $ff);
  DoOutputUInt8((aValue shr 8) and $ff);
  DoOutputUInt8((aValue shr 16) and $ff);
  DoOutputUInt8((aValue shr 32) and $ff);
{$endif}
 end;
 procedure DoOutputUInt64(const aValue:TpvUInt64);
 begin
{$ifdef little_endian}
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
 procedure FlushLiterals;
 begin
  if LiteralLength>0 then begin
   case LiteralLength of
    0..59:begin
     DoOutputUInt8({b00}0 or ((LiteralLength-1) shl 2));
    end;
    60..$00000ff:begin
     DoOutputUInt16(({b00}0 or (60 shl 2)) or ((LiteralLength-1) shl 8));
    end;
    $0000100..$0000ffff:begin
     DoOutputUInt8({b00}0 or (61 shl 2));
     DoOutputUInt16(LiteralLength-1);
    end;
    $0010000..$00ffffff:begin
     DoOutputUInt32(({b00}0 or (62 shl 2)) or ((LiteralLength-1) shl 8));
    end;
    else begin
     DoOutputUInt8({b00}0 or (63 shl 2));
     DoOutputUInt32(LiteralLength-1);
    end;
   end;
   DoOutputBlock(LiteralStart,LiteralLength);
   LiteralStart:=nil;
   LiteralLength:=0;
  end;
 end;
 procedure DoOutputLiteral(const aPosition:PpvUInt8);
 begin
  if (LiteralLength=0) or
     ((LiteralLength>0) and ((TpvPtrUInt(LiteralStart)+LiteralLength)<>TpvPtrUInt(aPosition))) then begin
   if LiteralLength>0 then begin
    FlushLiterals;
   end;
   LiteralStart:=aPosition;
  end;
  inc(LiteralLength);
 end;
 procedure DoOutputCopy(const aDistance,aLength:TpvUInt32);
 begin
  FlushLiterals;
  if ((aLength>3) and (aLength<12)) and (aDistance<2048) then begin
   // Short match
   DoOutputUInt16({b01} 1 or (((aLength-4) shl 2) or (aDistance shl 5)));
  end
  else if (aLength<=64) and (aDistance<65536) then begin
   // Medium match
   DoOutputUInt8({b10}2 or ((aLength-1) shl 2));
   DoOutputUInt16(aDistance);
  end else begin
   case aLength of
    0..8191:begin
     // Long match
     DoOutputUInt32(({b011}3 or ((aLength-1) shl 3)) or (((aDistance+1) and TpvUInt32($0000ffff)) shl 16));
     DoOutputUInt16(((aDistance+1) and TpvUInt32($ffff0000)) shr 16);
    end;
    else begin
     // Huge match
     DoOutputUInt8({b111}7);
     DoOutputUInt32(aLength);
     DoOutputUInt32(aDistance+1);
    end;
   end;
  end;
 end;
 procedure OutputStartBlock;
 begin
 end;
 procedure OutputEndBlock;
 begin
  FlushLiterals;
  DoOutputUInt8({b111}7);
  DoOutputUInt32(0);
 end;
var CurrentPointer,EndPointer,EndSearchPointer,Head,CurrentPossibleMatch:PpvUInt8;
    BestMatchDistance,BestMatchLength,MatchLength,MaximumMatchLength,CheckSum,Step,MaxSteps,
    Difference,Offset,SkipStrength,UnsuccessfulFindMatchAttempts:TpvUInt32;
    HashTable:PHashTable;
    ChainTable:PChainTable;
    HashTableItem:PPpvUInt8;
    Greedy:boolean;
begin
 result:=false;
 AllocatedDestSize:=SizeOf(TpvUInt32);
 GetMem(aDestData,AllocatedDestSize);
 aDestLen:=0;
 try
  Greedy:=aMode in [TpvLZBRSFMode.Medium,TpvLZBRSFMode.Slow];
  case aMode of
   TpvLZBRSFMode.VeryFast:begin
    MaxSteps:=1;
    SkipStrength:=7;
   end;
   TpvLZBRSFMode.Fast:begin
    MaxSteps:=128;
    SkipStrength:=7;
   end;
   TpvLZBRSFMode.Medium:begin
    MaxSteps:=128;
    SkipStrength:=32;
   end;
   TpvLZBRSFMode.Slow:begin
    MaxSteps:=MaxOffset;
    SkipStrength:=32;
   end;
   else begin
    MaxSteps:=128;
    SkipStrength:=32;
   end;
  end;
  LiteralStart:=nil;
  LiteralLength:=0;
  DoOutputUInt64(aInLen);
  OutputStartBlock;
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
      end;
      inc(Step);
      if Step<MaxSteps then begin
       CurrentPossibleMatch:=ChainTable^[({%H-}TpvPtrUInt(CurrentPossibleMatch)-{%H-}TpvPtrUInt(aInData)) and WindowMask];
      end else begin
       break;
      end;
     end;
     if (BestMatchDistance>0) and (BestMatchLength>1) then begin
      DoOutputCopy(BestMatchDistance,BestMatchLength);
     end else begin
      if SkipStrength>31 then begin
       DoOutputLiteral(CurrentPointer);
      end else begin
       Step:=UnsuccessfulFindMatchAttempts shr SkipStrength;
       Offset:=0;
       while (Offset<Step) and (({%H-}TpvPtrUInt(CurrentPointer)+Offset)<{%H-}TpvPtrUInt(EndSearchPointer)) do begin
        DoOutputLiteral(@PpvUInt8Array(CurrentPointer)^[Offset]);
        inc(Offset);
       end;
       BestMatchLength:=Offset;
       inc(UnsuccessfulFindMatchAttempts,ord(UnsuccessfulFindMatchAttempts<TpvUInt32($ffffffff)) and 1);
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
     DoOutputLiteral(CurrentPointer);
     inc(CurrentPointer);
    end;
   finally
    FreeMem(ChainTable);
   end;
  finally
   FreeMem(HashTable);
  end;
  OutputEndBlock;
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

function LZBRSFDecompress(const aInData:TpvPointer;aInLen:TpvUInt64;out aDestData:TpvPointer;out aDestLen:TpvUInt64):boolean;
var InputPointer,InputEnd,OutputPointer,OutputEnd,CopyFromPointer:PpvUInt8;
    Len,Offset,Tag:TpvUInt32;
    OutputSize:TpvUInt64;
begin

 aDestData:=nil;
 aDestLen:=0;

 // If the input OutputSize is too small, then exit early
 if aInLen<SizeOf(TpvUInt64) then begin
  result:=false;
  exit;
 end;

 // Setup stuff
 InputPointer:=aInData;
 InputEnd:=@PpvUInt8Array(InputPointer)^[aInLen];

 OutputSize:=PpvUInt64(InputPointer)^;
{$ifndef litte_endian}
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

 if OutputSize=0 then begin
  result:=true;
  exit;
 end;

 aDestLen:=OutputSize;

 GetMem(aDestData,OutputSize);

 OutputPointer:=aDestData;
 OutputEnd:=@PpvUInt8Array(OutputPointer)^[OutputSize];

 result:=true;

 while TpvPtrUInt(InputPointer)<TpvPtrUInt(InputEnd) do begin

  Tag:=TpvUInt8(pointer(InputPointer)^);
  inc(InputPointer);

  case Tag and 3 of
   {b00}0:begin
    // Literal(s)
    case Tag shr 2 of
     00..59:begin
      Len:=(Tag shr 2)+1;
     end;
     60:begin
      Len:=TpvUInt8(Pointer(InputPointer)^)+1;
      inc(InputPointer);
     end;
     61:begin
{$ifdef little_endian}
      Len:=TpvUInt16(Pointer(InputPointer)^)+1;
      inc(InputPointer,SizeOf(TpvUInt16));
{$else}
      Len:=TpvUInt16(Pointer(InputPointer)^);
      inc(InputPointer,SizeOf(TpvUInt16));
      Len:=((((Len and $ff00) shr 8) or ((Len and $ff) shl 8))+1;
{$endif}
     end;
     62:begin
{$ifdef little_endian}
      Len:=TpvUInt16(Pointer(InputPointer)^);
      inc(InputPointer,SizeOf(TpvUInt16));
      Len:=(Len or (TpvUInt8(pointer(InputPointer)^) shl 16))+1;
      inc(InputPointer);
{$else}
      Len:=TpvUInt8(pointer(InputPointer)^);
      inc(InputPointer);
      Len:=(Len or (TpvUInt8(pointer(InputPointer)^) shl 8));
      inc(InputPointer);
      Len:=(Len or (TpvUInt8(pointer(InputPointer)^) shl 16))+1;
      inc(InputPointer);
{$endif}
     end;
     else {63:}begin
{$ifdef little_endian}
      Len:=TpvUInt32(pointer(InputPointer)^)+1;
      inc(InputPointer,SizeOf(TpvUInt32));
{$else}
      Len:=TpvUInt32(pointer(InputPointer)^);
      inc(InputPointer,SizeOf(TpvUInt32));
      Len:=(((Len and TpvUInt32($ff000000)) shr 24) or
            ((Len and TpvUInt32($00ff0000)) shr 8) or
            ((Len and TpvUInt32($0000ff00)) shl 8) or
            ((Len and TpvUInt32($000000ff)) shl 24))+1;
{$endif}
     end;
    end;
    CopyFromPointer:=InputPointer;
    inc(InputPointer,Len);
   end;
   {b01}1:begin
    // Short match
    if TpvPtrUInt(InputPointer)>=TpvPtrUInt(InputEnd) then begin
     result:=false;
     break;
    end;
    Len:=((Tag shr 2) and 7)+4;
    Offset:=((Tag shr 5) or (TpvUInt8(pointer(InputPointer)^) shl 3))+1;
    inc(InputPointer);
    CopyFromPointer:=pointer(@PpvUInt8Array(OutputPointer)^[-Offset]);
    if TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData) then begin
     result:=false;
     break;
    end;
   end;
   {b10}2:begin
    // Medium match
    if (TpvPtrUInt(InputPointer)+SizeOf(TpvUInt16))>TpvPtrUInt(InputEnd) then begin
     result:=false;
     break;
    end;
    Len:=(Tag shr 2)+1;
{$ifdef little_endian}
    Offset:=TpvUInt16(pointer(InputPointer)^)+1;
    inc(InputPointer,SizeOf(TpvUInt16));
{$else}
    Offset:=TpvUInt16(Pointer(InputPointer)^);
    inc(InputPointer,SizeOf(TpvUInt16));
    Offset:=((((Offset and $ff00) shr 8) or ((Offset and $ff) shl 8))+1;
{$endif}
    CopyFromPointer:=pointer(@PpvUInt8Array(OutputPointer)^[-Offset]);
    if TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData) then begin
     result:=false;
     break;
    end;
   end;
   else {b11}{3:}begin
    if (Tag and 4)<>0 then begin
     // {b111}7 - Huge match
     if (TpvPtrUInt(InputPointer)+(SizeOf(TpvUInt32)*2))>TpvPtrUInt(InputEnd) then begin
      result:=false;
      break;
     end;
     Len:=TpvUInt32(pointer(InputPointer)^);
     inc(InputPointer,SizeOf(TpvUInt32));
{$ifndef little_endian}
     Len:=((Len and TpvUInt32($ff000000)) shr 24) or
          ((Len and TpvUInt32($00ff0000)) shr 8) or
          ((Len and TpvUInt32($0000ff00)) shl 8) or
          ((Len and TpvUInt32($000000ff)) shl 24);
{$endif}
     if Len=0 then begin
      // End code
      break;
     end;
     Offset:=TpvUInt32(pointer(InputPointer)^);
     inc(InputPointer,SizeOf(TpvUInt32));
{$ifndef little_endian}
     Offset:=((Offset and TpvUInt32($ff000000)) shr 24) or
             ((Offset and TpvUInt32($00ff0000)) shr 8) or
             ((Offset and TpvUInt32($0000ff00)) shl 8) or
             ((Offset and TpvUInt32($000000ff)) shl 24);
{$endif}
    end else begin
     // {b011}3 - Long match
     if (TpvPtrUInt(InputPointer)+(SizeOf(TpvUInt8)+SizeOf(TpvUInt32)))>TpvPtrUInt(InputEnd) then begin
      result:=false;
      break;
     end;
     Len:=((Tag shr 3) or (TpvUInt8(pointer(InputPointer)^) shl 5))+1;
     inc(InputPointer,SizeOf(TpvUInt8));
     Offset:=TpvUInt32(pointer(InputPointer)^);
     inc(InputPointer,SizeOf(TpvUInt32));
{$ifndef little_endian}
     Offset:=((Offset and TpvUInt32($ff000000)) shr 24) or
             ((Offset and TpvUInt32($00ff0000)) shr 8) or
             ((Offset and TpvUInt32($0000ff00)) shl 8) or
             ((Offset and TpvUInt32($000000ff)) shl 24);
{$endif}
    end;
    CopyFromPointer:=pointer(@PpvUInt8Array(OutputPointer)^[-Offset]);
    if TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(aDestData) then begin
     result:=false;
     break;
    end;
   end;
  end;

  if (TpvPtrUInt(OutputPointer)+TpvPtrUInt(Len))>TpvPtrUInt(OutputEnd) then begin
   result:=false;
   break;
  end;

  if (TpvPtrUInt(CopyFromPointer)<TpvPtrUInt(OutputPointer)) and (TpvPtrUInt(OutputPointer)<(TpvPtrUInt(CopyFromPointer)+TpvPtrUInt(Len))) then begin
   // Overlapping
   repeat
    OutputPointer^:=CopyFromPointer^;
    inc(OutputPointer);
    inc(CopyFromPointer);
    dec(Len);
   until Len=0;
  end else begin
   // Non-overlapping
   case Len and 7 of
    1:begin
     OutputPointer^:=CopyFromPointer^;
     inc(OutputPointer);
    end;
    2:begin
     TpvUInt16(pointer(OutputPointer)^):=TpvUInt16(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt16));
    end;
    3:begin
     TpvUInt16(pointer(OutputPointer)^):=TpvUInt16(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt16));
     OutputPointer^:=CopyFromPointer^;
     inc(OutputPointer);
    end;
    4:begin
     TpvUInt32(pointer(OutputPointer)^):=TpvUInt32(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt32));
    end;
{$ifdef cpu64}
    5:begin
     TpvUInt32(pointer(OutputPointer)^):=TpvUInt32(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt32));
     OutputPointer^:=CopyFromPointer^;
     inc(OutputPointer);
    end;
    6:begin
     TpvUInt32(pointer(OutputPointer)^):=TpvUInt32(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt32));
     TpvUInt16(pointer(OutputPointer)^):=TpvUInt16(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt16));
    end;
    7:begin
     TpvUInt32(pointer(OutputPointer)^):=TpvUInt32(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt32));
     TpvUInt16(pointer(OutputPointer)^):=TpvUInt16(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt16));
     OutputPointer^:=CopyFromPointer^;
     inc(OutputPointer);
    end;
    8:begin
     TpvUInt64(pointer(OutputPointer)^):=TpvUInt64(pointer(CopyFromPointer)^);
     inc(OutputPointer,SizeOf(TpvUInt64));
    end;
{$endif}
    else begin
{$ifdef fpc}
     Move(CopyFromPointer^,OutputPointer^,Len);
     inc(OutputPointer,Len);
{$else}
{$ifdef cpu64}
     while Len>=SizeOf(TpvUInt64) do begin
      TpvUInt64(pointer(OutputPointer)^):=TpvUInt64(pointer(CopyFromPointer)^);
      inc(OutputPointer,SizeOf(TpvUInt64));
      inc(CopyFromPointer,SizeOf(TpvUInt64));
      dec(Len,SizeOf(TpvUInt64));
     end;
{$else}
     while Len>=SizeOf(TpvUInt32) do begin
      TpvUInt32(pointer(OutputPointer)^):=TpvUInt32(pointer(CopyFromPointer)^);
      inc(OutputPointer,SizeOf(TpvUInt32));
      inc(CopyFromPointer,SizeOf(TpvUInt32));
      dec(Len,SizeOf(TpvUInt32));
     end;
{$endif}
     while Len>0 do begin
      OutputPointer^:=CopyFromPointer^;
      inc(OutputPointer);
      inc(CopyFromPointer);
      dec(Len);
     end;
{$endif}
    end;
   end;
  end;

 end;

 if not (result and (aDestLen=TpvPtrUInt(TpvPtrUInt(OutputPointer)-TpvPtrUInt(aDestData)))) then begin
  result:=false;
  aDestLen:=0;
  FreeMem(aDestData);
  aDestData:=nil;
 end;

end;

initialization
end.
