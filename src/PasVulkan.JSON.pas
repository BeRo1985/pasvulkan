(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.JSON;
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
     PasVulkan.Types;

type EpvJSONSyntaxError=class(Exception);

     EpvJSONMergeError=class(Exception);

     TpvJSONUTF8String=TpvUTF8String;

     TpvJSONRawByteString=TpvRawByteString;

     TpvJSONItem=class
      public
       constructor Create;
       destructor Destroy; override;
       procedure Merge(const AWith:TpvJSONItem); virtual;
     end;

     TpvJSONItems=array of TpvJSONItem;

     TpvJSONItemNull=class(TpvJSONItem)
      public
       constructor Create;
       destructor Destroy; override;
       procedure Merge(const AWith:TpvJSONItem); override;
     end;

     TpvJSONItemBoolean=class(TpvJSONItem)
      private
       fValue:boolean;
      public
       constructor Create(const AValue:boolean);
       destructor Destroy; override;
       procedure Merge(const AWith:TpvJSONItem); override;
      published
       property Value:boolean read fValue write fValue;
     end;

     TpvJSONItemNumber=class(TpvJSONItem)
      private
       fValue:TpvDouble;
      public
       constructor Create(const AValue:TpvDouble);
       destructor Destroy; override;
       procedure Merge(const AWith:TpvJSONItem); override;
      published
       property Value:TpvDouble read fValue write fValue;
     end;

     TpvJSONItemString=class(TpvJSONItem)
      private
       fValue:TpvJSONUTF8String;
      public
       constructor Create(const AValue:TpvJSONUTF8String);
       destructor Destroy; override;
       procedure Merge(const AWith:TpvJSONItem); override;
      published
       property Value:TpvJSONUTF8String read fValue write fValue;
     end;

     PpvJSONItemObjectProperty=^TpvJSONItemObjectProperty;
     TpvJSONItemObjectProperty=record
      Key:TpvJSONUTF8String;
      Value:TpvJSONItem;
     end;

     TpvJSONItemObjectProperties=array of TpvJSONItemObjectProperty;

     TpvJSONItemObject=class(TpvJSONItem)
      private
       fProperties:TpvJSONItemObjectProperties;
       fCount:TpvInt32;
       function GetKeyIndex(const Key:TpvJSONUTF8String):TpvInt32;
       function GetKey(const Index:TpvInt32):TpvJSONUTF8String;
       procedure SetKey(const Index:TpvInt32;const Key:TpvJSONUTF8String);
       function GetValue(const Index:TpvInt32):TpvJSONItem;
       procedure SetValue(const Index:TpvInt32;const Item:TpvJSONItem);
       function GetProperty(const Key:TpvJSONUTF8String):TpvJSONItem;
       procedure SetProperty(const Key:TpvJSONUTF8String;const Item:TpvJSONItem);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Merge(const AWith:TpvJSONItem); override;
       procedure Add(const Key:TpvJSONUTF8String;const Value:TpvJSONItem);
       property Count:TpvInt32 read fCount;
       property Indices[const Key:TpvJSONUTF8String]:TpvInt32 read GetKeyIndex;
       property Keys[const Index:TpvInt32]:TpvJSONUTF8String read GetKey write SetKey;
       property Values[const Index:TpvInt32]:TpvJSONItem read GetValue write SetValue;
       property Properties[const Key:TpvJSONUTF8String]:TpvJSONItem read GetProperty write SetProperty; default;
     end;

     TpvJSONItemArray=class(TpvJSONItem)
      private
       fItems:TpvJSONItems;
       fCount:TpvInt32;
       function GetValue(const Index:TpvInt32):TpvJSONItem;
       procedure SetValue(const Index:TpvInt32;const Item:TpvJSONItem);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Merge(const AWith:TpvJSONItem); override;
       procedure Add(const Value:TpvJSONItem);
       property Count:TpvInt32 read fCount;
       property Items[const Index:TpvInt32]:TpvJSONItem read GetValue write SetValue; default;
     end;

     PpvJSONModeFlag=^TpvJSONModeFlag;
     TpvJSONModeFlag=(
                      UnquotedKeys,
                      Comments,
                      ImplicitRootObject,
                      OptionalCommas,
                      EqualsForColon,
                      MultilineStrings
                     );

     PpvJSONModeFlags=^TpvJSONModeFlag;
     TpvJSONModeFlags=set of TpvJSONModeFlag;

const SimplifiedJSONModeFlags:TpvJSONModeFlags=[TpvJSONModeFlag.UnquotedKeys,
                                                TpvJSONModeFlag.Comments,
                                                TpvJSONModeFlag.ImplicitRootObject,
                                                TpvJSONModeFlag.OptionalCommas,
                                                TpvJSONModeFlag.EqualsForColon,
                                                TpvJSONModeFlag.MultilineStrings];

function JSONStringQuote(const s:TpvJSONUTF8String):TpvJSONRawByteString;

function JSONParse(const pSource:TpvJSONRawByteString;const pModeFlags:TpvJSONModeFlags=[TpvJSONModeFlag.Comments]):TpvJSONItem;
function JSONStringify(const pJSONItem:TpvJSONItem;const pFormatting:boolean=false;const pModeFlags:TpvJSONModeFlags=[];const pLevel:TpvInt32=0):TpvJSONRawByteString;

function JSONGetNumber(const pItem:TpvJSONItem;const pDefault:TpvDouble=0.0):TpvDouble;
function JSONGetString(const pItem:TpvJSONItem;const pDefault:TpvJSONUTF8String=''):TpvJSONUTF8String;
function JSONGetBoolean(const pItem:TpvJSONItem;const pDefault:boolean=false):boolean;

function JSONLoadBinaryFromStream(const pStream:TStream):TpvJSONItem;
procedure JSONSaveBinaryToStream(const pStream:TStream;const pJSONItem:TpvJSONItem);

implementation

uses PasDblStrUtils;

                                           //0 1 2 3 4 5 6 7 8 9 a b c d e f
const{UTF8CharSteps:array[AnsiChar] of byte=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                             1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                             2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                             3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                             4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1); // f
                                           //0 1 2 3 4 5 6 7 8 9 a b c d e f  }

      UTF8DFACharClasses:array[AnsiChar] of byte=(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
                                                  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                                  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                                  8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3,
                                                  11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8);

      UTF8DFATransitions:array[byte] of byte=(0,16,32,48,80,128,112,16,16,16,64,96,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,0,16,16,16,16,16,0,16,0,16,16,16,16,16,16,
                                              16,32,16,16,16,16,16,32,16,32,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,32,16,16,16,16,16,16,16,16,
                                              16,32,16,16,16,16,16,16,16,32,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,48,16,48,16,16,16,16,16,16,
                                              16,48,16,16,16,16,16,48,16,48,16,16,16,16,16,16,
                                              16,48,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                              16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16);

      suDONOTKNOW=-1;
      suNOUTF8=0;
      suPOSSIBLEUTF8=1;
      suISUTF8=2;

      ucACCEPT=0;
      ucERROR=16;

function GetNextUTF8Char(const s:PAnsiChar;const Len:TpvInt32;var CodeUnit:TpvInt32):TpvUInt32;
var StartCodeUnit,Value,CharClass,State:TpvUInt32;
begin
 result:=0;
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  dec(CodeUnit);
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<Len do begin
   Value:=byte(AnsiChar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[AnsiChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=byte(AnsiChar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
  inc(CodeUnit);
 end;
end;

function ConvertUTF16ToUTF8(const s:UnicodeString):TpvJSONUTF8String;
var i,j:TpvInt32;
    w:UInt16;
    u4c:TpvUInt32;
begin
 result:='';
 j:=0;
 i:=1;
 while i<=length(s) do begin
  w:=UInt16(WideChar(s[i]));
  if (w<=$d7ff) or (w>=$e000) then begin
   u4c:=w;
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((UInt16(s[i+1])>=$dc00) and (UInt16(s[i+1])<=$dfff)) then begin
   u4c:=(TpvUInt32(TpvUInt32(w and $3ff) shl 10) or TpvUInt32(UInt16(WideChar(s[i+1])) and $3ff))+$10000;
   inc(i,2);
  end else begin
   u4c:=$fffd;
   inc(i);
  end;
  if u4c<=$7f then begin
   inc(j);
  end else if u4c<=$7ff then begin
   inc(j,2);
  end else if u4c<=$ffff then begin
   inc(j,3);
  end else if u4c<=$1fffff then begin
   inc(j,4);
  end else begin
   inc(j,3);
  end;
 end;
 SetLength(result,j);
 j:=1;
 i:=1;
 while i<=length(s) do begin
  w:=UInt16(WideChar(s[i]));
  if (w<=$d7ff) or (w>=$e000) then begin
   u4c:=w;
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((UInt16(s[i+1])>=$dc00) and (UInt16(s[i+1])<=$dfff)) then begin
   u4c:=(TpvUInt32(TpvUInt32(w and $3ff) shl 10) or TpvUInt32(UInt16(WideChar(s[i+1])) and $3ff))+$10000;
   inc(i,2);
  end else begin
   u4c:=$fffd;
   inc(i);
  end;
  if u4c<=$7f then begin
   result[j]:=ansichar(byte(u4c));
   inc(j);
  end else if u4c<=$7ff then begin
   result[j]:=ansichar(byte($c0 or ((u4c shr 6) and $1f)));
   result[j+1]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,2);
  end else if u4c<=$ffff then begin
   result[j]:=ansichar(byte($e0 or ((u4c shr 12) and $0f)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end else if u4c<=$1fffff then begin
   result[j]:=ansichar(byte($f0 or ((u4c shr 18) and $07)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+3]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,4);
  end else begin
   u4c:=$fffd;
   result[j]:=ansichar(byte($e0 or (u4c shr 12)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end;
 end;
end;

constructor TpvJSONItem.Create;
begin
 inherited Create;
end;

destructor TpvJSONItem.Destroy;
begin
 inherited Destroy;
end;

procedure TpvJSONItem.Merge(const AWith:TpvJSONItem);
begin
 if not (assigned(AWith) and (AWith is TpvJSONItem)) then begin
  raise EpvJSONMergeError.Create('Incompatible data type');
 end;
end;

constructor TpvJSONItemNull.Create;
begin
 inherited Create;
end;

destructor TpvJSONItemNull.Destroy;
begin
 inherited Destroy;
end;

procedure TpvJSONItemNull.Merge(const AWith:TpvJSONItem);
begin
 if not (assigned(AWith) and (AWith is TpvJSONItemNull)) then begin
  raise EpvJSONMergeError.Create('Incompatible data type');
 end;
end;

constructor TpvJSONItemBoolean.Create(const AValue:boolean);
begin
 inherited Create;
 fValue:=AValue;
end;

destructor TpvJSONItemBoolean.Destroy;
begin
 inherited Destroy;
end;

procedure TpvJSONItemBoolean.Merge(const AWith:TpvJSONItem);
begin
 if not (assigned(AWith) and (AWith is TpvJSONItemBoolean)) then begin
  raise EpvJSONMergeError.Create('Incompatible data type');
 end;
 fValue:=TpvJSONItemBoolean(AWith).Value;
end;

constructor TpvJSONItemNumber.Create(const AValue:TpvDouble);
begin
 inherited Create;
 fValue:=AValue;
end;

destructor TpvJSONItemNumber.Destroy;
begin
 inherited Destroy;
end;

procedure TpvJSONItemNumber.Merge(const AWith:TpvJSONItem);
begin
 if not (assigned(AWith) and (AWith is TpvJSONItemNumber)) then begin
  raise EpvJSONMergeError.Create('Incompatible data type');
 end;
 fValue:=TpvJSONItemNumber(AWith).Value;
end;

constructor TpvJSONItemString.Create(const AValue:TpvJSONUTF8String);
begin
 inherited Create;
 fValue:=AValue;
end;

destructor TpvJSONItemString.Destroy;
begin
 fValue:='';
 inherited Destroy;
end;

procedure TpvJSONItemString.Merge(const AWith:TpvJSONItem);
begin
 if not (assigned(AWith) and (AWith is TpvJSONItemString)) then begin
  raise EpvJSONMergeError.Create('Incompatible data type');
 end;
 fValue:=TpvJSONItemString(AWith).Value;
end;

constructor TpvJSONItemObject.Create;
begin
 inherited Create;
 fProperties:=nil;
 fCount:=0;
end;

destructor TpvJSONItemObject.Destroy;
var Index:TpvInt32;
begin
 for Index:=0 to fCount-1 do begin
  fProperties[Index].Key:='';
  FreeAndNil(fProperties[Index].Value);
 end;
 SetLength(fProperties,0);
 inherited Destroy;
end;

function TpvJSONItemObject.GetKeyIndex(const Key:TpvJSONUTF8String):TpvInt32;
var Index:TpvInt32;
begin
 for Index:=0 to fCount-1 do begin
  if fProperties[Index].Key=Key then begin
   result:=Index;
   exit;
  end;
 end;
 result:=-1;
end;

function TpvJSONItemObject.GetKey(const Index:TpvInt32):TpvJSONUTF8String;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=fProperties[Index].Key;
 end else begin
  result:='';
 end;
end;

procedure TpvJSONItemObject.SetKey(const Index:TpvInt32;const Key:TpvJSONUTF8String);
begin
 if (Index>=0) and (Index<fCount) then begin
  fProperties[Index].Key:=Key;
 end;
end;

function TpvJSONItemObject.GetValue(const Index:TpvInt32):TpvJSONItem;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=fProperties[Index].Value;
 end else begin
  result:=nil;
 end;
end;

procedure TpvJSONItemObject.SetValue(const Index:TpvInt32;const Item:TpvJSONItem);
begin
 if (Index>=0) and (Index<fCount) then begin
  fProperties[Index].Value:=Item;
 end;
end;

function TpvJSONItemObject.GetProperty(const Key:TpvJSONUTF8String):TpvJSONItem;
begin
 result:=GetValue(GetKeyIndex(Key));
end;

procedure TpvJSONItemObject.SetProperty(const Key:TpvJSONUTF8String;const Item:TpvJSONItem);
begin
 SetValue(GetKeyIndex(Key),Item);
end;

procedure TpvJSONItemObject.Add(const Key:TpvJSONUTF8String;const Value:TpvJSONItem);
var Index:TpvInt32;
begin
 Index:=fCount;
 inc(fCount);
 if fCount>=length(fProperties) then begin
  SetLength(fProperties,fCount*2);
 end;
 fProperties[Index].Key:=Key;
 fProperties[Index].Value:=Value;
end;

procedure TpvJSONItemObject.Merge(const AWith:TpvJSONItem);
var Index,KeyIndex:TpvInt32;
    SrcProperty,OurProperty:PpvJSONItemObjectProperty;
    NewItem:TpvJSONItem;
begin
 if not (assigned(AWith) and (AWith is TpvJSONItemObject)) then begin
  raise EpvJSONMergeError.Create('Incompatible data type');
 end;
 for Index:=0 to TpvJSONItemObject(AWith).Count do begin
  SrcProperty:=@TpvJSONItemObject(AWith).fProperties[Index];
  if assigned(SrcProperty^.Value) then begin
   KeyIndex:=GetKeyIndex(SrcProperty^.Key);
   if KeyIndex>=0 then begin
    OurProperty:=@fProperties[KeyIndex];
    if assigned(OurProperty^.Value) and (OurProperty^.Value.ClassType=SrcProperty^.Value.ClassType) then begin
     OurProperty^.Value.Merge(SrcProperty^.Value);
    end;
   end else begin
    NewItem:=nil;
    try
     NewItem:=TpvJSONItem(SrcProperty^.Value.ClassType.Create);
     NewItem.Merge(SrcProperty^.Value);
     Add(SrcProperty^.Key,NewItem);
    except
     NewItem.Free;
     raise;
    end;
   end;
  end;
 end;
end;

constructor TpvJSONItemArray.Create;
begin
 inherited Create;
 fItems:=nil;
 fCount:=0;
end;

destructor TpvJSONItemArray.Destroy;
var Index:TpvInt32;
begin
 for Index:=0 to fCount-1 do begin
  FreeAndNil(fItems[Index]);
 end;
 SetLength(fItems,0);
 inherited Destroy;
end;

function TpvJSONItemArray.GetValue(const Index:TpvInt32):TpvJSONItem;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=fItems[Index];
 end else begin
  result:=nil;
 end;
end;

procedure TpvJSONItemArray.SetValue(const Index:TpvInt32;const Item:TpvJSONItem);
begin
 if (Index>=0) and (Index<fCount) then begin
  fItems[Index]:=Item;
 end;
end;

procedure TpvJSONItemArray.Add(const Value:TpvJSONItem);
var Index:TpvInt32;
begin
 Index:=fCount;
 inc(fCount);
 if fCount>=length(fItems) then begin
  SetLength(fItems,fCount*2);
 end;
 fItems[Index]:=Value;
end;

procedure TpvJSONItemArray.Merge(const AWith:TpvJSONItem);
var Index:TpvInt32;
    Item,NewItem:TpvJSONItem;
begin
 if not (assigned(AWith) and (AWith is TpvJSONItemArray)) then begin
  raise EpvJSONMergeError.Create('Incompatible data type');
 end;
 for Index:=0 to TpvJSONItemArray(AWith).Count-1 do begin
  Item:=TpvJSONItemArray(AWith).Items[Index];
  if assigned(Item) then begin
   NewItem:=TpvJSONItem(Item.ClassType.Create);
   Add(NewItem);
   NewItem.Merge(Item);
  end;
 end;
end;

const HexChars:array[boolean,0..15] of ansichar=(('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'),
                                                 ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'));

function JSONStringQuote(const s:TpvJSONUTF8String):TpvJSONRawByteString;
var i,l:TpvInt32;
    c,t:TpvUInt32;
begin
 result:='"';
 i:=1;
 l:=length(s);
 while i<=l do begin
  case s[i] of
   '"','\':begin
    result:=result+'\'+AnsiChar(s[i]);
    inc(i);
   end;
   #$08:begin
    result:=result+'\b';
    inc(i);
   end;
   #$09:begin
    result:=result+'\t';
    inc(i);
   end;
   #$0a:begin
    result:=result+'\n';
    inc(i);
   end;
   #$0b:begin
    result:=result+'\v';
    inc(i);
   end;
   #$0c:begin
    result:=result+'\f';
    inc(i);
   end;
   #$0d:begin
    result:=result+'\r';
    inc(i);
   end;
   #$00..#$07,#$0e..#$1f,#$7e..#$7f:begin
    c:=byte(AnsiChar(s[i]));
    result:=result+'\u00'+HexChars[false,(c shr 4) and $f]+HexChars[false,c and $f];
    inc(i);
   end;
   #$80..#$ff:begin
    c:=GetNextUTF8Char(PAnsiChar(@s[1]),l,i);
    case c of
     $0000..$d7ff,$f000..$fffc:begin
      result:=result+'\u'+HexChars[false,(c shr 12) and $f]+HexChars[false,(c shr 8) and $f]+HexChars[false,(c shr 4) and $f]+HexChars[false,c and $f];
     end;
     $100000..$10ffff:begin
      dec(c,$100000);
      t:=(c shr 10) or $d800;
      result:=result+'\u'+HexChars[false,(t shr 12) and $f]+HexChars[false,(t shr 8) and $f]+HexChars[false,(t shr 4) and $f]+HexChars[false,t and $f];
      t:=(c and $3ff) or $dc00;
      result:=result+'\u'+HexChars[false,(t shr 12) and $f]+HexChars[false,(t shr 8) and $f]+HexChars[false,(t shr 4) and $f]+HexChars[false,t and $f];
     end;
     else {-$d800..$dfff,$fffd..$ffff,$110000..$ffffffff:}begin
      result:=result+'\ufffd';
     end;
    end;
   end;
   else begin
    result:=result+AnsiChar(s[i]);
    inc(i);
   end;
  end;
 end;
 result:=result+'"';
end;

function JSONParse(const pSource:TpvJSONRawByteString;const pModeFlags:TpvJSONModeFlags=[TpvJSONModeFlag.Comments]):TpvJSONItem;
type TEncoding=(Latin1,UTF8,UTF16LE,UTF16BE,UTF32LE,UTF32BE);
var Position:TpvInt32;
    CurrentChar:TpvUInt32;
    CharEOF:boolean;
    Encoding:TEncoding;
 procedure NextChar;
 const UTF16Shifts:array[TEncoding.UTF16LE..TEncoding.UTF16BE,0..1] of TpvInt32=((0,8),(8,0));
 var Temp:TpvUInt32;
 begin
  if Position<=length(pSource) then begin
   case Encoding of
    TEncoding.UTF8:begin
     CurrentChar:=GetNextUTF8Char(PAnsiChar(@pSource[1]),Length(pSource),Position);
    end;
    TEncoding.UTF16LE,TEncoding.UTF16BE:begin
     if (Position+1)<=length(pSource) then begin
      CurrentChar:=(TpvUInt32(byte(AnsiChar(pSource[Position]))) shl UTF16Shifts[Encoding,0]) or (TpvUInt32(byte(AnsiChar(pSource[Position+1]))) shl UTF16Shifts[Encoding,1]);
      inc(Position,2);
      if ((CurrentChar>=$d800) and (CurrentChar<=$dbff)) and ((Position+1)<=length(pSource)) then begin
       Temp:=(TpvUInt32(byte(AnsiChar(pSource[Position]))) shl UTF16Shifts[Encoding,0]) or (TpvUInt32(byte(AnsiChar(pSource[Position+1]))) shl UTF16Shifts[Encoding,1]);
       if (Temp>=$dc00) and (Temp<=$dfff) then begin
        CurrentChar:=(TpvUInt32(TpvUInt32(CurrentChar and $3ff) shl 10) or TpvUInt32(Temp and $3ff))+$10000;
        inc(Position,2);
       end else begin
        CurrentChar:=$fffd;
       end;
      end else if not ((CurrentChar<=$d7ff) or (CurrentChar>=$e000)) then begin
       CurrentChar:=$fffd;
      end;
     end else begin
      inc(Position);
      CurrentChar:=0;
      CharEOF:=true;
     end;
    end;
    TEncoding.UTF32LE:begin
     if (Position+3)<=length(pSource) then begin
      CurrentChar:=(TpvUInt32(byte(AnsiChar(pSource[Position]))) shl 0) or
                   (TpvUInt32(byte(AnsiChar(pSource[Position+1]))) shl 8) or
                   (TpvUInt32(byte(AnsiChar(pSource[Position+2]))) shl 16) or
                   (TpvUInt32(byte(AnsiChar(pSource[Position+3]))) shl 24);
      inc(Position,4);
     end else begin
      inc(Position);
      CurrentChar:=0;
      CharEOF:=true;
     end;
    end;
    TEncoding.UTF32BE:begin
     if (Position+3)<=length(pSource) then begin
      CurrentChar:=(TpvUInt32(byte(AnsiChar(pSource[Position]))) shl 24) or
                   (TpvUInt32(byte(AnsiChar(pSource[Position+1]))) shl 16) or
                   (TpvUInt32(byte(AnsiChar(pSource[Position+2]))) shl 8) or
                   (TpvUInt32(byte(AnsiChar(pSource[Position+3]))) shl 0);
      inc(Position,4);
     end else begin
      inc(Position);
      CurrentChar:=0;
      CharEOF:=true;
     end;
    end;
    else begin
     CurrentChar:=byte(AnsiChar(pSource[Position]));
     inc(Position);
    end;
   end;
  end else begin
   CurrentChar:=0;
   CharEOF:=true;
  end;
 end;
 procedure JSONError;
 begin
  raise EpvJSONSyntaxError.Create('PasVulkan.JSON.parse');
 end;
 procedure SkipWhite;
 var LastChar:TpvUInt32;
 begin
  while not CharEOF do begin
   case CurrentChar of
    $0009,$000a,$000d,$0020:begin
     NextChar;
    end;
    ord('/'):begin
     if TpvJSONModeFlag.Comments in pModeFlags then begin
      NextChar;
      case CurrentChar of
       ord('/'):begin
        NextChar;
        while not (CharEOF or ((CurrentChar=$000a) or (CurrentChar=$000d))) do begin
         NextChar;
        end;
       end;
       ord('*'):begin
        NextChar;
        LastChar:=0;
        while not (CharEOF or ((LastChar=ord('*')) and (CurrentChar=ord('/')))) do begin
         LastChar:=CurrentChar;
         NextChar;
        end;
        if CurrentChar=ord('/') then begin
         NextChar;
        end;
       end;
       else begin
        JSONError;
       end;
      end;
     end else begin
      JSONError;
     end;
    end;
    else begin
     break;
    end;
   end;
  end;
 end;
 function IsChar(c:WideChar):boolean;
 begin
  result:=(not CharEOF) and (CurrentChar=UInt16(c));
 end;
 procedure ExpectChar(c:WideChar);
 begin
  if IsChar(c) then begin
   NextChar;
  end else begin
   JSONError;
  end;
 end;
 function ParseValue(const ImplicitRootObject:boolean):TpvJSONItem;
  function ParseString:TpvJSONItem;
  var w:UnicodeString; // <= because of easy correct handling of UTF16 surrogates (see ECMAScript and JSON specs)
      wl:TpvInt32;
      wc:TpvUInt32;
   procedure AddChar(c:TpvUInt32);
   var cl:TpvInt32;
   begin
    if (c>=$100000) and (c<=$10ffff) then begin
     cl:=2;
    end else begin
     cl:=1;
    end;
    if (wl+cl)>length(w) then begin
     SetLength(w,(wl+cl) shl 1);
    end;
    if c<=$d7ff then begin
     inc(wl);
     w[wl]:=WideChar(UInt16(c));
    end else if c<=$dfff then begin
     inc(wl);
     w[wl]:=WideChar(UInt16($fffd));
    end else if c<=$fffd then begin
     inc(wl);
     w[wl]:=WideChar(UInt16(c));
    end else if c<=$ffff then begin
     inc(wl);
     w[wl]:=WideChar(UInt16($fffd));
    end else if c<=$10ffff then begin
     dec(c,$10000);
     inc(wl);
     w[wl]:=WideChar(UInt16((c shr 10) or $d800));
     inc(wl);
     w[wl]:=WideChar(UInt16((c and $3ff) or $dc00));
    end else begin
     inc(wl);
     w[wl]:=WideChar(UInt16($fffd));
    end;
   end;
  begin
   result:=nil;
   w:='';
   try
    try
     SetLength(w,512);
     wl:=0;
     if IsChar('"') then begin
      NextChar;
      while not (CharEOF or IsChar('"')) do begin
       case CurrentChar of
        $0000..$001f:begin
         if TpvJSONModeFlag.MultiLineStrings in pModeFlags then begin
          AddChar(CurrentChar);
          NextChar;
         end else begin
          JSONError;
         end;
        end;
        ord('\'):begin
         NextChar;
         case CurrentChar of
          ord('"'),ord('\'),ord('/'):begin
           AddChar(CurrentChar);
           NextChar;
          end;
          ord('b'):begin
           AddChar($0008);
           NextChar;
          end;
          ord('f'):begin
           AddChar($000c);
           NextChar;
          end;
          ord('n'):begin
           AddChar($000a);
           NextChar;
          end;
          ord('r'):begin
           AddChar($000d);
           NextChar;
          end;
          ord('t'):begin
           AddChar($0009);
           NextChar;
          end;
          ord('u'):begin
           NextChar;
           if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
            if (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) then begin
             wc:=UInt16(CurrentChar)-ord('0');
            end else if (CurrentChar>=ord('a')) and (CurrentChar<=ord('f')) then begin
             wc:=(UInt16(CurrentChar)-ord('a'))+$a;
            end else if (CurrentChar>=ord('A')) and (CurrentChar<=ord('F')) then begin
             wc:=(UInt16(CurrentChar)-ord('A'))+$a;
            end else begin
             wc:=0;
            end;
            wc:=wc shl 4;
            NextChar;
            if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
             if (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) then begin
              wc:=wc or UInt16(UInt16(CurrentChar)-ord('0'));
             end else if (CurrentChar>=ord('a')) and (CurrentChar<=ord('f')) then begin
              wc:=wc or UInt16((UInt16(CurrentChar)-ord('a'))+$a);
             end else if (CurrentChar>=ord('A')) and (CurrentChar<=ord('F')) then begin
              wc:=wc or UInt16((UInt16(CurrentChar)-ord('A'))+$a);
             end;
             wc:=wc shl 4;
             NextChar;
             if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
              if (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) then begin
               wc:=wc or UInt16(UInt16(CurrentChar)-ord('0'));
              end else if (CurrentChar>=ord('a')) and (CurrentChar<=ord('f')) then begin
               wc:=wc or UInt16((UInt16(CurrentChar)-ord('a'))+$a);
              end else if (CurrentChar>=ord('A')) and (CurrentChar<=ord('F')) then begin
               wc:=wc or UInt16((UInt16(CurrentChar)-ord('A'))+$a);
              end;
              wc:=wc shl 4;
              NextChar;
              if (((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) or ((CurrentChar>=ord('a')) and (CurrentChar<=ord('f'))) or ((CurrentChar>=ord('A')) and (CurrentChar<=ord('F')))) and not CharEOF then begin
               if (CurrentChar>=ord('0')) and (CurrentChar<=ord('9')) then begin
                wc:=wc or UInt16(UInt16(CurrentChar)-ord('0'));
               end else if (CurrentChar>=ord('a')) and (CurrentChar<=ord('f')) then begin
                wc:=wc or UInt16((UInt16(CurrentChar)-ord('a'))+$a);
               end else if (CurrentChar>=ord('A')) and (CurrentChar<=ord('F')) then begin
                wc:=wc or UInt16((UInt16(CurrentChar)-ord('A'))+$a);
               end;
               AddChar(wc);
               NextChar;
              end else begin
               JSONError;
              end;
             end else begin
              JSONError;
             end;
            end else begin
             JSONError;
            end;
           end else begin
            JSONError;
           end;
          end;
          else begin
           JSONError;
          end;
         end;
        end;
        else begin
         AddChar(CurrentChar);
         NextChar;
        end;
       end;
      end;
      ExpectChar('"');
      SetLength(w,wl);
      result:=TpvJSONItemString.Create(ConvertUTF16ToUTF8(w));
     end else begin
      JSONError;
     end;
     SkipWhite;
    except
     FreeAndNil(result);
     raise;
    end;
   finally
    w:='';
   end;
  end;
  function ParseNumber:TpvJSONItem;
  var s:TpvJSONRawByteString;
      OK:TPasDblStrUtilsBoolean;
      Value:TpvDouble;
  begin
   result:=nil;
   s:='';
   try
    try
     if CharEOF then begin
      JSONError;
     end;
     case CurrentChar of
      ord('-'),ord('0')..ord('9'):begin
       if IsChar('-') then begin
        s:=s+ansichar(byte(CurrentChar));
        NextChar;
        if CharEOF or not ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
         JSONError;
        end;
       end;
       if (not CharEOF) and (CurrentChar=ord('0')) then begin
        s:=s+ansichar(byte(CurrentChar));
        NextChar;
        if (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
         JSONError;
        end;
       end else begin
        while (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
         s:=s+ansichar(byte(CurrentChar));
         NextChar;
        end;
       end;
       if IsChar('.') then begin
        s:=s+ansichar(byte(CurrentChar));
        NextChar;
        if CharEOF or not ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
         JSONError;
        end;
        while (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
         s:=s+ansichar(byte(CurrentChar));
         NextChar;
        end;
       end;
       if (not CharEOF) and ((CurrentChar=ord('e')) or (CurrentChar=ord('E'))) then begin
        s:=s+ansichar(byte(CurrentChar));
        NextChar;
        if (not CharEOF) and ((CurrentChar=ord('-')) or (CurrentChar=ord('+'))) then begin
         s:=s+ansichar(byte(CurrentChar));
         NextChar;
        end;
        if CharEOF or not ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) then begin
         JSONError;
        end;
        while (not CharEOF) and ((CurrentChar>=ord('0')) and (CurrentChar<=ord('9'))) do begin
         s:=s+ansichar(byte(CurrentChar));
         NextChar;
        end;
       end;
      end else begin
       JSONError;
      end;
     end;
     if length(s)>0 then begin
      OK:=false;
      Value:=ConvertStringToDouble(s,rmNearest,@OK);
      if not OK then begin
       JSONError;
      end;
      result:=TpvJSONItemNumber.Create(Value);
     end;
    except
     FreeAndNil(result);
     raise;
    end;
   finally
    s:='';
   end;
  end;
  procedure ParseObjectProperty(DestinationObject:TpvJSONItemObject);
  var Key:TpvJSONUTF8String;
      Value:TpvJSONItem;
  begin
   result:=nil;
   try
    if (TpvJSONModeFlag.UnquotedKeys in pModeFlags) and
       (not CharEOF) and
       ((CurrentChar=UInt16(WideChar('_'))) or
        ((CurrentChar>=UInt16(WideChar('a'))) and (CurrentChar<=UInt16(WideChar('z')))) or
        ((CurrentChar>=UInt16(WideChar('A'))) and (CurrentChar<=UInt16(WideChar('Z')))) or
        ((CurrentChar>=UInt16(WideChar('0'))) and (CurrentChar<=UInt16(WideChar('9'))))) then begin
     Key:=TpvJSONUTF8String(AnsiChar(byte(CurrentChar)));
     NextChar;
     while ((CurrentChar=UInt16(WideChar('_'))) or
        ((CurrentChar>=UInt16(WideChar('a'))) and (CurrentChar<=UInt16(WideChar('z')))) or
        ((CurrentChar>=UInt16(WideChar('A'))) and (CurrentChar<=UInt16(WideChar('Z')))) or
        ((CurrentChar>=UInt16(WideChar('0'))) and (CurrentChar<=UInt16(WideChar('9'))))) do begin
      Key:=Key+TpvJSONUTF8String(AnsiChar(byte(CurrentChar)));
      NextChar;
     end;
     SkipWhite;
    end else begin
     Value:=ParseString;
     if assigned(Value) and (Value is TpvJSONItemString) then begin
      Key:=TpvJSONItemString(Value).Value;
      FreeAndNil(Value);
     end else begin
      FreeAndNil(Value);
      JSONError;
      Key:='';
     end;
    end;
    SkipWhite;
    if TpvJSONModeFlag.EqualsForColon in pModeFlags then begin
     ExpectChar('=');
    end else begin
     ExpectChar(':');
    end;
    SkipWhite;
    Value:=ParseValue(false);
    DestinationObject.Add(Key,Value);
   except
    FreeAndNil(result);
    raise;
   end;
  end;
  function ParseObject(const ImplicitRootObject:boolean):TpvJSONItem;
  begin
   result:=TpvJSONItemObject.Create;
   try
    if not ImplicitRootObject then begin
     ExpectChar('{');
    end;
    SkipWhite;
    while not (CharEOF or ((not ImplicitRootObject) and IsChar('}'))) do begin
     ParseObjectProperty(TpvJSONItemObject(result));
     SkipWhite;
     if IsChar(',') then begin
      NextChar;
      SkipWhite;
      if (not (CharEOF or ImplicitRootObject)) and IsChar('}') then begin
       JSONError;
      end;
     end else if not (TpvJSONModeFlag.OptionalCommas in pModeFlags) then begin
      break;
     end;
    end;
    if not ImplicitRootObject then begin
     ExpectChar('}');
    end;
   except
    FreeAndNil(result);
    raise;
   end;
  end;
  function ParseArray:TpvJSONItem;
  begin
   result:=TpvJSONItemArray.Create;
   try
    ExpectChar('[');
    SkipWhite;
    while not (CharEOF or IsChar(']')) do begin
     TpvJSONItemArray(result).Add(ParseValue(false));
     SkipWhite;
     if IsChar(',') then begin
      NextChar;
      SkipWhite;
      if (not CharEOF) and IsChar(']') then begin
       JSONError;
      end;
     end else if not (TpvJSONModeFlag.OptionalCommas in pModeFlags) then begin
      break;
     end;
    end;
    ExpectChar(']');
   except
    FreeAndNil(result);
    raise;
   end;
  end;
  procedure ExpectKeyword(const Keyword:TpvJSONUTF8String);
  var i:TpvInt32;
  begin
   for i:=1 to length(Keyword) do begin
    ExpectChar(WideChar(Keyword[i]));
   end;
  end;
 begin
  result:=nil;
  try
   SkipWhite;
   if CharEOF then begin
    JSONError;
   end;
   if ImplicitRootObject then begin
    result:=ParseObject(true);
   end else begin
    case CurrentChar of
     ord('{'):begin
      result:=ParseObject(false);
     end;
     ord('['):begin
      result:=ParseArray;
     end;
     ord('"'):begin
      result:=ParseString;
     end;
     ord('-'),ord('0')..ord('9'):begin
      result:=ParseNumber;
     end;
     ord('t'):begin
      ExpectKeyword('true');
      result:=TpvJSONItemBoolean.Create(true);
     end;
     ord('f'):begin
      ExpectKeyword('false');
      result:=TpvJSONItemBoolean.Create(false);
     end;
     ord('n'):begin
      ExpectKeyword('null');
      result:=TpvJSONItemNull.Create;
     end;
     else begin
      JSONError;
     end;
    end;
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
begin
 result:=nil;
 try
  CharEOF:=false;
  if (length(pSource)>=3) and ((byte(ansichar(pSource[1]))=$ef) and (byte(ansichar(pSource[2]))=$bb) and (byte(ansichar(pSource[3]))=$bf)) then begin
   Position:=4;
   Encoding:=TEncoding.UTF8;
  end else if (length(pSource)>=4) and ((byte(ansichar(pSource[1]))=$ff) and (byte(ansichar(pSource[2]))=$fe) and (byte(ansichar(pSource[3]))=$00) and (byte(ansichar(pSource[4]))=$00)) then begin
   Position:=5;
   Encoding:=TEncoding.UTF32LE;
  end else if (length(pSource)>=4) and ((byte(ansichar(pSource[1]))=$00) and (byte(ansichar(pSource[2]))=$00) and (byte(ansichar(pSource[3]))=$fe) and (byte(ansichar(pSource[4]))=$ff)) then begin
   Position:=5;
   Encoding:=TEncoding.UTF32BE;
  end else if (length(pSource)>=2) and ((byte(ansichar(pSource[1]))=$ff) and (byte(ansichar(pSource[2]))=$fe)) then begin
   Position:=3;
   Encoding:=TEncoding.UTF16LE;
  end else if (length(pSource)>=2) and ((byte(ansichar(pSource[1]))=$fe) and (byte(ansichar(pSource[2]))=$ff)) then begin
   Position:=3;
   Encoding:=TEncoding.UTF16BE;
  end else begin
   Position:=1;
   Encoding:=TEncoding.Latin1;
  end;
  NextChar;
  result:=ParseValue(TpvJSONModeFlag.ImplicitRootObject in pModeFlags);
  SkipWhite;
  if not CharEOF then begin
   JSONError;
  end;
 except
  on e:EpvJSONSyntaxError do begin
   FreeAndNil(result);
  end;
  else begin
   FreeAndNil(result);
   raise;
  end;
 end;
end;

function JSONStringify(const pJSONItem:TpvJSONItem;const pFormatting:boolean=false;const pModeFlags:TpvJSONModeFlags=[];const pLevel:TpvInt32=0):TpvJSONRawByteString;
 function IsIdentifier(const pKey:TpvJSONUTF8String):boolean;
 var Index:TpvInt32;
 begin
  result:=length(pKey)>0;
  for Index:=1 to length(pKey) do begin
   case UInt16(WideChar(pKey[Index])) of
    UInt16(WideChar('_')),
    UInt16(WideChar('a'))..UInt16(WideChar('z')),
    UInt16(WideChar('A'))..UInt16(WideChar('Z')),
    UInt16(WideChar('0'))..UInt16(WideChar('9')):begin
    end;
    else begin
     result:=false;
     break;
    end;
   end;
  end;
 end;
var Index,Count,Ident,LevelOffset:TpvInt32;
    IntegerValue:Int64;
    Key:TpvJSONUTF8String;
begin
 if assigned(pJSONItem) then begin
  if pJSONItem is TpvJSONItemNull then begin
   result:='null';
  end else if pJSONItem is TpvJSONItemBoolean then begin
   if TpvJSONItemBoolean(pJSONItem).Value then begin
    result:='true';
   end else begin
    result:='false';
   end;
  end else if pJSONItem is TpvJSONItemNumber then begin
   IntegerValue:=trunc(TpvJSONItemNumber(pJSONItem).Value);
   if TpvJSONItemNumber(pJSONItem).Value=IntegerValue then begin
    Str(IntegerValue,result);
   end else begin
    result:=ConvertDoubleToString(TpvJSONItemNumber(pJSONItem).Value,omStandard,0);
   end;
  end else if pJSONItem is TpvJSONItemString then begin
   result:=JSONStringQuote(TpvJSONItemString(pJSONItem).Value);
  end else if pJSONItem is TpvJSONItemArray then begin
   result:='[';
   if pFormatting then begin
    result:=result+#13#10;
   end;
   Count:=TpvJSONItemArray(pJSONItem).Count;
   if TpvJSONModeFlag.ImplicitRootObject in pModeFlags then begin
    LevelOffset:=-1;
   end else begin
    LevelOffset:=0;
   end;
   for Index:=0 to Count-1 do begin
    if pFormatting then begin
     for Ident:=0 to pLevel+LevelOffset do begin
      result:=result+'  ';
     end;
    end;
    result:=result+JSONStringify(TpvJSONItemArray(pJSONItem).Items[Index],pFormatting,pModeFlags,pLevel+1);
    if ((Index+1)<Count) and not (TpvJSONModeFlag.OptionalCommas in pModeFlags) then begin
     result:=result+',';
    end;
    if pFormatting then begin
     result:=result+#13#10;
    end;
   end;
   if pFormatting then begin
    for Ident:=1 to pLevel+LevelOffset do begin
     result:=result+'  ';
    end;
   end;
   result:=result+']';
  end else if pJSONItem is TpvJSONItemObject then begin
   if (not (TpvJSONModeFlag.ImplicitRootObject in pModeFlags)) or (pLevel<>0) then begin
    result:='{';
    if pFormatting then begin
     result:=result+#13#10;
    end;
   end else begin
    result:='';
   end;
   if TpvJSONModeFlag.ImplicitRootObject in pModeFlags then begin
    LevelOffset:=-1;
   end else begin
    LevelOffset:=0;
   end;
   Count:=TpvJSONItemArray(pJSONItem).Count;
   for Index:=0 to Count-1 do begin
    if pFormatting then begin
     for Ident:=0 to pLevel+LevelOffset do begin
      result:=result+'  ';
     end;
    end;
    Key:=TpvJSONItemObject(pJSONItem).Keys[Index];
    if (TpvJSONModeFlag.UnquotedKeys in pModeFlags) and IsIdentifier(Key) then begin
     result:=result+TpvJSONRawByteString(Key);
    end else begin
     result:=result+JSONStringQuote(Key);
    end;
    if TpvJSONModeFlag.EqualsForColon in pModeFlags then begin
     if pFormatting then begin
      result:=result+' ';
     end;
     result:=result+'=';
    end else begin
     result:=result+':';
    end;
    if pFormatting then begin
     result:=result+' ';
    end;
    result:=result+JSONStringify(TpvJSONItemObject(pJSONItem).Values[Index],pFormatting,pModeFlags,pLevel+1);
    if ((Index+1)<Count) and not (TpvJSONModeFlag.OptionalCommas in pModeFlags) then begin
     result:=result+',';
    end;
    if pFormatting then begin
     result:=result+#13#10;
    end;
   end;
   if pFormatting then begin
    for Ident:=1 to pLevel+LevelOffset do begin
     result:=result+'  ';
    end;
   end;
   if (not (TpvJSONModeFlag.ImplicitRootObject in pModeFlags)) or (pLevel<>0) then begin
    result:=result+'}';
   end;
  end else begin
   result:='null';
  end;
 end else begin
  result:='null';
 end;
end;

function JSONGetNumber(const pItem:TpvJSONItem;const pDefault:TpvDouble=0.0):TpvDouble;
begin
 if assigned(pItem) and (pItem is TpvJSONItemNumber) then begin
  result:=TpvJSONItemNumber(pItem).Value;
 end else begin
  result:=pDefault;
 end;
end;

function JSONGetString(const pItem:TpvJSONItem;const pDefault:TpvJSONUTF8String=''):TpvJSONUTF8String;
begin
 if assigned(pItem) and (pItem is TpvJSONItemString) then begin
  result:=TpvJSONItemString(pItem).Value;
 end else begin
  result:=pDefault;
 end;
end;

function JSONGetBoolean(const pItem:TpvJSONItem;const pDefault:boolean=false):boolean;
begin
 if assigned(pItem) and (pItem is TpvJSONItemBoolean) then begin
  result:=TpvJSONItemBoolean(pItem).Value;
 end else begin
  result:=pDefault;
 end;
end;

function JSONLoadBinaryFromStream(const pStream:TStream):TpvJSONItem;
 function LoadJSONItem:TpvJSONItem;
 var ItemType,BooleanValue:TpvUInt8;
     Count,Counter,Len:TpvInt32;
     TempString:TpvJSONUTF8String;
     DoubleValue:TpvDouble;
 begin
  result:=nil;
  if pStream.Read(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
   raise EInOutError.Create('Stream read error');
  end;
  case ItemType of
   0:begin
    result:=TpvJSONItemNull.Create;
   end;
   1:begin
    result:=TpvJSONItemArray.Create;
    if pStream.Read(Count,SizeOf(TpvUInt32))<>SizeOf(TpvInt32) then begin
     raise EInOutError.Create('Stream read error');
    end;
    for Counter:=0 to Count-1 do begin
     TpvJSONItemArray(result).Add(LoadJSONItem);
    end;
   end;
   2:begin
    TempString:='';
    try
     result:=TpvJSONItemObject.Create;
     if pStream.Read(Count,SizeOf(TpvUInt32))<>SizeOf(TpvInt32) then begin
      raise EInOutError.Create('Stream read error');
     end;
     for Counter:=0 to Count-1 do begin
      if pStream.Read(Len,SizeOf(TpvUInt32))<>SizeOf(TpvInt32) then begin
       raise EInOutError.Create('Stream read error');
      end;
      SetLength(TempString,Len);
      if Len>0 then begin
       if TpvInt64(pStream.Read(TempString[1],Len*SizeOf(AnsiChar)))<>TpvInt64(Len*SizeOf(AnsiChar)) then begin
        raise EInOutError.Create('Stream read error');
       end;
      end;
      TpvJSONItemObject(result).Add(TempString,LoadJSONItem);
     end;
    finally
     TempString:='';
    end;
   end;
   3:begin
    if pStream.Read(BooleanValue,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream read error');
    end;
    result:=TpvJSONItemBoolean.Create(BooleanValue<>0);
   end;
   4:begin
    if pStream.Read(DoubleValue,SizeOf(TpvDouble))<>SizeOf(TpvDouble) then begin
     raise EInOutError.Create('Stream read error');
    end;
    result:=TpvJSONItemNumber.Create(DoubleValue);
   end;
   5:begin
    TempString:='';
    try
     if pStream.Read(Len,SizeOf(TpvInt32))<>SizeOf(TpvInt32) then begin
      raise EInOutError.Create('Stream read error');
     end;
     SetLength(TempString,Len);
     if Len>0 then begin
      if TpvInt64(pStream.Read(TempString[1],Len*SizeOf(AnsiChar)))<>TpvInt64(Len*SizeOf(AnsiChar)) then begin
       raise EInOutError.Create('Stream read error');
      end;
     end;
     result:=TpvJSONItemString.Create(TempString);
    finally
     TempString:='';
    end;
   end;
   else begin
    raise EInOutError.Create('Stream read error');
   end;
  end;
 end;
begin
 if assigned(pStream) and (pStream.Position<pStream.Size) then begin
  result:=LoadJSONItem;
 end else begin
  result:=nil;
 end;
end;

procedure JSONSaveBinaryToStream(const pStream:TStream;const pJSONItem:TpvJSONItem);
 procedure SaveJSONItem(const pCurrentJSONItem:TpvJSONItem);
 var ItemType,BooleanValue:TpvUInt8;
     Count,Counter,Len:TpvInt32;
     TempString:TpvJSONUTF8String;
     DoubleValue:TpvDouble;
 begin
  if assigned(pCurrentJSONItem) then begin
   if pCurrentJSONItem is TpvJSONItemNull then begin
    ItemType:=0;
    if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
   end else if pCurrentJSONItem is TpvJSONItemArray then begin
    ItemType:=1;
    if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
    Count:=TpvJSONItemArray(pCurrentJSONItem).Count;
    if pStream.Write(Count,SizeOf(TpvInt32))<>SizeOf(TpvInt32) then begin
     raise EInOutError.Create('Stream write error');
    end;
    for Counter:=0 to Count-1 do begin
     SaveJSONItem(TpvJSONItemArray(pCurrentJSONItem).Items[Counter]);
    end;
   end else if pCurrentJSONItem is TpvJSONItemObject then begin
    ItemType:=2;
    if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
    Count:=TpvJSONItemObject(pCurrentJSONItem).Count;
    if pStream.Write(Count,SizeOf(TpvInt32))<>SizeOf(TpvInt32) then begin
     raise EInOutError.Create('Stream write error');
    end;
    try
     for Counter:=0 to Count-1 do begin
      TempString:=TpvJSONItemObject(pCurrentJSONItem).Keys[Counter];
      Len:=length(TempString);
      if pStream.Write(Len,SizeOf(TpvInt32))<>SizeOf(TpvInt32) then begin
       raise EInOutError.Create('Stream write error');
      end;
      if Len>0 then begin
       if TpvInt64(pStream.Write(TempString[1],Len*SizeOf(AnsiChar)))<>TpvInt64(Len*SizeOf(AnsiChar)) then begin
        raise EInOutError.Create('Stream write error');
       end;
      end;
      SaveJSONItem(TpvJSONItemObject(pCurrentJSONItem).Values[Counter]);
     end;
    finally
     TempString:='';
    end;
   end else if pCurrentJSONItem is TpvJSONItemBoolean then begin
    ItemType:=3;
    if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
    if TpvJSONItemBoolean(pCurrentJSONItem).fValue then begin
     BooleanValue:=$ff;
    end else begin
     BooleanValue:=$00;
    end;
    if pStream.Write(BooleanValue,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
   end else if pCurrentJSONItem is TpvJSONItemNumber then begin
    ItemType:=4;
    if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
    DoubleValue:=TpvJSONItemNumber(pCurrentJSONItem).fValue;
    if pStream.Write(DoubleValue,SizeOf(TpvDouble))<>SizeOf(TpvDouble) then begin
     raise EInOutError.Create('Stream write error');
    end;
   end else if pCurrentJSONItem is TpvJSONItemString then begin
    ItemType:=5;
    if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
    TempString:=TpvJSONItemString(pCurrentJSONItem).fValue;
    Len:=length(TempString);
    if pStream.Write(Len,SizeOf(TpvInt32))<>SizeOf(TpvInt32) then begin
     raise EInOutError.Create('Stream write error');
    end;
    if Len>0 then begin
     if TpvInt64(pStream.Write(TempString[1],Len*SizeOf(AnsiChar)))<>TpvInt64(Len*SizeOf(AnsiChar)) then begin
      raise EInOutError.Create('Stream write error');
     end;
    end;
   end else begin
    ItemType:=0;
    if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
   end;
  end else begin
   ItemType:=0;
   if pStream.Write(ItemType,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
    raise EInOutError.Create('Stream write error');
   end;
  end;
 end;
begin
 if assigned(pJSONItem) and assigned(pStream) then begin
  SaveJSONItem(pJSONItem);
 end;
end;

initialization
end.

