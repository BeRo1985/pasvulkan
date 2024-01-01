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
unit PasVulkan.Hash.xxHash64;
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

type TpvHashXXHash64=class
      public
       const PRIME64_1=TpvUInt64($9e3779b185ebca87);
             PRIME64_2=TpvUInt64($c2b2ae3d27d4eb4f);
             PRIME64_3=TpvUInt64($165667b19e3779f9);
             PRIME64_4=TpvUInt64($85ebca77c2b2ae63);
             PRIME64_5=TpvUInt64($27d4eb2f165667c5);
       type TMessageDigest=TpvUInt64;
            PMessageDigest=^TMessageDigest;
      private
       fTotalLength:TpvSizeUInt;
       fSeed:TpvUInt64;
       fV1:TpvUInt64;
       fV2:TpvUInt64;
       fV3:TpvUInt64;
       fV4:TpvUInt64;
       fDataSize:TpvSizeUInt;
       fData:array[0..31] of TpvUInt8;
{$if not declared(ROLQWord)}
       class function ROLQWord(const aValue:TpvUInt64;const aBits:TpvSizeUInt):TpvUInt64; static; inline;
{$ifend}
      public
       constructor Create(const aSeed:TpvUInt64=0); reintroduce;
       destructor Destroy; override;
       procedure Update(const aData:pointer;const aDataLength:TpvSizeUInt);
       function Final:TMessageDigest;
       class function Process(const aData:pointer;const aDataLength:TpvSizeUInt;const aSeed:TpvUInt64=0):TMessageDigest; static;
     end;

implementation

{ TpvHashXXHash64 }

{$if not declared(ROLQWord)}
class function TpvHashxxHash64.ROLQWord(const aValue:TpvUInt64;const aBits:TpvSizeUInt):TpvUInt64;
begin
 result:=(aValue shl aBits) or (aValue shr (64-aBits));
end;
{$ifend}

class function TpvHashXXHash64.Process(const aData:pointer;const aDataLength:TpvSizeUInt;const aSeed:TpvUInt64):TpvHashXXHash64.TMessageDigest;
{$if true}
var TotalLength:TpvSizeUInt;
    v1:TpvUInt64;
    v2:TpvUInt64;
    v3:TpvUInt64;
    v4:TpvUInt64;
    DataSize:TpvSizeUInt;
    Data:array[0..31] of TpvUInt8;
    CurrentData,DataEnd,DataStop:Pointer;
begin
 v1:=aSeed+PRIME64_1;
 v1:=v1+PRIME64_2;
 v2:=aSeed+PRIME64_2;
 v3:=aSeed;
 v4:=aSeed-PRIME64_1;
 TotalLength:=0;
 DataSize:=0;
 FillChar(Data,SizeOf(Data),#0);

 CurrentData:=aData;

 inc(TotalLength,aDataLength);

 if (DataSize+aDataLength)<TpvSizeUInt(32) then begin

  Move(CurrentData^,Data[DataSize],aDataLength);
  inc(DataSize,32);

 end else begin

  DataEnd:=@PpvUInt8Array(aData)^[aDataLength];

  if DataSize>0 then begin

   Move(CurrentData^,Data[DataSize],32-DataSize);

   v1:=PRIME64_1*ROLQWord(v1+(PRIME64_2*PpvUInt64(pointer(@Data[0]))^),31);
   v2:=PRIME64_1*ROLQWord(v2+(PRIME64_2*PpvUInt64(pointer(@Data[8]))^),31);
   v3:=PRIME64_1*ROLQWord(v3+(PRIME64_2*PpvUInt64(pointer(@Data[16]))^),31);
   v4:=PRIME64_1*ROLQWord(v4+(PRIME64_2*PpvUInt64(pointer(@Data[24]))^),31);

   CurrentData:=@PpvUInt8Array(CurrentData)^[32-DataSize];

   DataSize:=0;

  end;

  if (TpvPtrUInt(CurrentData)+31)<TpvPtrUInt(DataEnd) then begin

   DataStop:=Pointer(TpvPtrUInt(TpvPtrUInt(DataEnd)-32));
   repeat
    v1:=PRIME64_1*ROLQWord(v1+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
    v2:=PRIME64_1*ROLQWord(v2+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
    v3:=PRIME64_1*ROLQWord(v3+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
    v4:=PRIME64_1*ROLQWord(v4+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
   until TpvPtrUInt(CurrentData)>TpvPtrUInt(DataStop);

  end;

  if TpvPtrUInt(CurrentData)<TpvPtrUInt(DataEnd) then begin
   DataSize:=TpvPtrUInt(DataEnd)-TpvPtrUInt(CurrentData);
   Move(CurrentData^,Data[0],DataSize);
  end;

 end;

 if TotalLength>=TpvSizeUInt(32) then begin
  result:=ROLQWord(v1,1)+ROLQWord(v2,7)+ROLQWord(v3,12)+ROLQWord(v4,18);
  v1:=ROLQWord(v1*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v1)*PRIME64_1)+PRIME64_4;
  v2:=ROLQWord(v2*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v2)*PRIME64_1)+PRIME64_4;
  v3:=ROLQWord(v3*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v3)*PRIME64_1)+PRIME64_4;
  v4:=ROLQWord(v4*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v4)*PRIME64_1)+PRIME64_4;
 end else begin
  result:=aSeed+PRIME64_5;
 end;

 inc(result,TotalLength);

 CurrentData:=@Data[0];
 DataEnd:=@Data[DataSize];

 while (TpvPtrUInt(CurrentData)+7)<TpvPtrUInt(DataEnd) do begin
  result:=result xor (PRIME64_1*ROLQWord(PRIME64_2*PpvUInt64(CurrentData)^,31));
  result:=(ROLQWord(result,27)*PRIME64_1)+PRIME64_4;
  inc(PpvUInt64(CurrentData));
 end;

 while (TpvPtrUInt(CurrentData)+3)<TpvPtrUInt(DataEnd) do begin
  result:=result xor (PpvUInt32(CurrentData)^*PRIME64_1);
  result:=(ROLQWord(result,23)*PRIME64_2)+PRIME64_3;
  inc(PpvUInt32(CurrentData));
 end;

 while TpvPtrUInt(CurrentData)<TpvPtrUInt(DataEnd) do begin
  result:=result xor (PpvUInt8(CurrentData)^*PRIME64_5);
  result:=ROLQWord(result,11)*PRIME64_1;
  inc(PpvUInt8(CurrentData));
 end;

 result:=(result xor (result shr 33))*PRIME64_2;
 result:=(result xor (result shr 29))*PRIME64_3;
 result:=result xor (result shr 32);

end;
{$else}
var Instance:TpvHashXXHash64;
begin
 Instance:=TpvHashXXHash64.Create(aSeed);
 try
  Instance.Update(aData,aDataLength);
  result:=Instance.Final;
 finally
  FreeAndNil(Instance);
 end;
end;
{$ifend}

constructor TpvHashXXHash64.Create(const aSeed:TpvUInt64=0);
begin
 inherited Create;
 fSeed:=aSeed;
 fV1:=aSeed+PRIME64_1;
 fV1:=fV1+PRIME64_2;
 fV2:=aSeed+PRIME64_2;
 fV3:=aSeed;
 fV4:=aSeed-PRIME64_1;
 fTotalLength:=0;
 fDataSize:=0;
 FillChar(fData,SizeOf(fData),#0);
end;

destructor TpvHashXXHash64.Destroy;
begin
 inherited Destroy;
end;

procedure TpvHashXXHash64.Update(const aData:pointer;const aDataLength:TpvSizeUInt);
var v1,v2,v3,v4:TpvUInt64;
    CurrentData,DataEnd,DataStop:Pointer;
begin

 CurrentData:=aData;

 inc(fTotalLength,aDataLength);

 if (fDataSize+aDataLength)<TpvSizeUInt(32) then begin

  Move(CurrentData^,fData[fDataSize],aDataLength);
  inc(fDataSize,32);

 end else begin

  DataEnd:=@PpvUInt8Array(aData)^[aDataLength];

  if fDataSize>0 then begin

   Move(CurrentData^,fData[fDataSize],32-fDataSize);

   fV1:=PRIME64_1*ROLQWord(fV1+(PRIME64_2*PpvUInt64(pointer(@fData[0]))^),31);
   fV2:=PRIME64_1*ROLQWord(fV2+(PRIME64_2*PpvUInt64(pointer(@fData[8]))^),31);
   fV3:=PRIME64_1*ROLQWord(fV3+(PRIME64_2*PpvUInt64(pointer(@fData[16]))^),31);
   fV4:=PRIME64_1*ROLQWord(fV4+(PRIME64_2*PpvUInt64(pointer(@fData[24]))^),31);

   CurrentData:=@PpvUInt8Array(CurrentData)^[32-fDataSize];

   fDataSize:=0;

  end;

  if (TpvPtrUInt(CurrentData)+31)<TpvPtrUInt(DataEnd) then begin
   v1:=fV1;
   v2:=fV2;
   v3:=fV3;
   v4:=fV4;

   DataStop:=Pointer(TpvPtrUInt(TpvPtrUInt(DataEnd)-32));
   repeat
    v1:=PRIME64_1*ROLQWord(v1+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
    v2:=PRIME64_1*ROLQWord(v2+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
    v3:=PRIME64_1*ROLQWord(v3+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
    v4:=PRIME64_1*ROLQWord(v4+(PRIME64_2*PpvUInt64(CurrentData)^),31);
    inc(PpvUInt64(CurrentData));
   until TpvPtrUInt(CurrentData)>TpvPtrUInt(DataStop);

   fV1:=v1;
   fV2:=v2;
   fV3:=v3;
   fV4:=v4;
  end;

  if TpvPtrUInt(CurrentData)<TpvPtrUInt(DataEnd) then begin
   fDataSize:=TpvPtrUInt(DataEnd)-TpvPtrUInt(CurrentData);
   Move(CurrentData^,fData[0],fDataSize);
  end;

 end;

end;

function TpvHashXXHash64.Final:TMessageDigest;
var v1,v2,v3,v4:TpvUInt64;
    CurrentData,DataEnd:Pointer;
begin

 if fTotalLength>=TpvSizeUInt(32) then begin
  v1:=fV1;
  v2:=fV2;
  v3:=fV3;
  v4:=fV4;
  result:=ROLQWord(v1,1)+ROLQWord(v2,7)+ROLQWord(v3,12)+ROLQWord(v4,18);
  v1:=ROLQWord(v1*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v1)*PRIME64_1)+PRIME64_4;
  v2:=ROLQWord(v2*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v2)*PRIME64_1)+PRIME64_4;
  v3:=ROLQWord(v3*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v3)*PRIME64_1)+PRIME64_4;
  v4:=ROLQWord(v4*PRIME64_2,31)*PRIME64_1;
  result:=((result xor v4)*PRIME64_1)+PRIME64_4;
 end else begin
  result:=fSeed+PRIME64_5;
 end;
 inc(result,fTotalLength);

 CurrentData:=@fData[0];
 DataEnd:=@fData[fDataSize];

 while (TpvPtrUInt(CurrentData)+7)<TpvPtrUInt(DataEnd) do begin
  result:=result xor (PRIME64_1*ROLQWord(PRIME64_2*PpvUInt64(CurrentData)^,31));
  result:=(ROLQWord(result,27)*PRIME64_1)+PRIME64_4;
  inc(PpvUInt64(CurrentData));
 end;

 while (TpvPtrUInt(CurrentData)+3)<TpvPtrUInt(DataEnd) do begin
  result:=result xor (PpvUInt32(CurrentData)^*PRIME64_1);
  result:=(ROLQWord(result,23)*PRIME64_2)+PRIME64_3;
  inc(PpvUInt32(CurrentData));
 end;

 while TpvPtrUInt(CurrentData)<TpvPtrUInt(DataEnd) do begin
  result:=result xor (PpvUInt8(CurrentData)^*PRIME64_5);
  result:=ROLQWord(result,11)*PRIME64_1;
  inc(PpvUInt8(CurrentData));
 end;

 result:=(result xor (result shr 33))*PRIME64_2;
 result:=(result xor (result shr 29))*PRIME64_3;
 result:=result xor (result shr 32);

end;

end.
