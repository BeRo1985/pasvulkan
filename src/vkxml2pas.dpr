(******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016, Benjamin Rosseaux (benjamin@rosseaux.de)               *
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
program vkxml2pas;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {-$pic off}
 {$define CAN_INLINE}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi} 
 {$undef HasSAR}
 {$define UseDIV}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef cpu386}
 {$define cpux86}
{$endif}
{$ifdef cpuamd64}
 {$define cpux86}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$ifdef Windows}
 {$define Win}
{$endif}
{$ifdef sdl20}
 {$define sdl}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$ifdef fpc}
 {$define CAN_INLINE}
{$else}
 {$undef CAN_INLINE}
 {$ifdef ver180}
  {$define CAN_INLINE}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define CAN_INLINE}
   {$ifend}
  {$endif}
 {$endif}
{$endif}
{$ifdef windows}
 {$apptype console}
{$endif}

uses SysUtils,Classes,Contnrs;

// On Windows, Vulkan commands use the stdcall convention
// On Android/ARMv7a, Vulkan functions use the armeabi-v7a-hard calling convention, even if the application's native code is compiled with the armeabi-v7a calling convention.
// On other platforms, use the default calling convention
const CallingConventions='{$ifdef Windows}stdcall;{$else}{$ifdef Android}cdecl;{TODO-for-FPC-Devs:armeabi-v7a-hard-calling-convention}{$else}cdecl;{$endif}{$endif}';

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
type qword=int64;
{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

function UTF32CharToUTF8(CharValue:longword):ansistring;
var Data:array[0..{$ifdef strictutf8}3{$else}5{$endif}] of ansichar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=ansichar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef strictutf8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef strictutf8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$endif}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,pansichar(@Data[0]),ResultLen);
 end;
end;

type TXMLClass=class
      public
       Previous,Next:TXMLClass;
       Core:pointer;
       constructor Create; overload; virtual;
       destructor Destroy; override;
     end;

const MaxListSize=2147483647 div sizeof(TXMLClass);

type PEngineListClasses=^TXMLClasses;
     TXMLClasses=array[0..MaxListSize-1] of TXMLClass;

     TXMLClassList=class(TXMLClass)
      private
       InternalList:PEngineListClasses;
       InternalCount,InternalCapacity:longint;
       function GetItem(Index:longint):TXMLClass;
       procedure SetItem(Index:longint;Value:TXMLClass);
       function GetItemPointer(Index:longint):TXMLClass;
      public
       ClearWithContentDestroying:boolean;
       CapacityMinimium:longint;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear;
       procedure ClearNoFree;
       procedure ClearWithFree;
       function Add(Item:TXMLClass):longint;
       function Append(Item:TXMLClass):longint;
       function AddList(List:TXMLClassList):longint;
       function AppendList(List:TXMLClassList):longint;
       function NewClass:TXMLClass;
       procedure Insert(Index:longint;Item:TXMLClass);
       procedure Delete(Index:longint);
       procedure DeleteClass(Index:longint);
       function Remove(Item:TXMLClass):longint;
       function RemoveClass(Item:TXMLClass):longint;
       function Find(Item:TXMLClass):longint;
       function IndexOf(Item:TXMLClass):longint;
       procedure Exchange(Index1,Index2:longint);
       procedure SetCapacity(NewCapacity:longint);
       procedure SetOptimalCapacity(TargetCapacity:longint);
       procedure SetCount(NewCount:longint);
       function Push(Item:TXMLClass):longint;
       function Pop(var Item:TXMLClass):boolean; overload;
       function Pop:TXMLClass; overload;
       function Last:TXMLClass;
       property Count:longint read InternalCount; 
       property Capacity:longint read InternalCapacity write SetCapacity;
       property Item[Index:longint]:TXMLClass read GetItem write SetItem; default;
       property Items[Index:longint]:TXMLClass read GetItem write SetItem;
       property PItems[Index:longint]:TXMLClass read GetItemPointer;
     end;

     TXMLClassLinkedList=class(TXMLClass)
      public
       ClearWithContentDestroying:boolean;
       First,Last:TXMLClass;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear;
       procedure ClearNoFree;
       procedure ClearWithFree;
       procedure Add(Item:TXMLClass);
       procedure Append(Item:TXMLClass);
       procedure AddLinkedList(List:TXMLClassLinkedList);
       procedure AppendLinkedList(List:TXMLClassLinkedList);
       procedure Remove(Item:TXMLClass);
       procedure RemoveClass(Item:TXMLClass);
       procedure Push(Item:TXMLClass);
       function Pop(var Item:TXMLClass):boolean; overload;
       function Pop:TXMLClass; overload;
       function Count:longint;
     end;

     TXMLStringTreeData=ptrint;

     PXMLStringTreeNode=^TXMLStringTreeNode;
     TXMLStringTreeNode=record
      TheChar:ansichar;
      Data:TXMLStringTreeData;
      DataExist:boolean;
      Prevoius,Next,Up,Down:PXMLStringTreeNode;
     end;

     TXMLStringTree=class
      private
       Root:PXMLStringTreeNode;
       function CreateStringTreeNode(AChar:ansichar):PXMLStringTreeNode;
       procedure DestroyStringTreeNode(Node:PXMLStringTreeNode);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure DumpTree;
       procedure DumpList;
       procedure AppendTo(DestStringTree:TXMLStringTree);
       procedure Optimize(DestStringTree:TXMLStringTree);
       function Add(Content:ansistring;Data:TXMLStringTreeData;Replace:boolean=false):boolean;
       function Delete(Content:ansistring):boolean;
       function Find(Content:ansistring;var Data:TXMLStringTreeData):boolean;
       function FindEx(Content:ansistring;var Data:TXMLStringTreeData;var Len:longint):boolean;
     end;

     TXMLString={$ifdef UNICODE}widestring{$else}ansistring{$endif};
     TXMLChar={$ifdef UNICODE}widechar{$else}ansichar{$endif};

     TXMLParameter=class(TXMLClass)
      public
       Name:ansistring;
       Value:TXMLString;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLParameter); virtual;
     end;

     TXMLItemList=class;

     TXMLTag=class;

     TXMLItem=class(TXMLClass)
      public
       Items:TXMLItemList;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear; virtual;
       procedure Add(Item:TXMLItem);
       procedure Assign(From:TXMLItem); virtual;
       function FindTag(const TagName:ansistring):TXMLTag;
     end;

     TXMLItemList=class(TXMLClassList)
      private
       function GetItem(Index:longint):TXMLItem;
       procedure SetItem(Index:longint;Value:TXMLItem);
      public
       constructor Create; override;
       destructor Destroy; override;
       function NewClass:TXMLItem;
       function FindTag(const TagName:ansistring):TXMLTag;
       property Item[Index:longint]:TXMLItem read GetItem write SetItem; default;
       property Items[Index:longint]:TXMLItem read GetItem write SetItem;
     end;

     TXMLText=class(TXMLItem)
      public
       Text:TXMLString;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLCommentTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLTag=class(TXMLItem)
      private
       StringTree:TXMLStringTree;
      public
       Name:ansistring;
       Parameter:array of TXMLParameter;
       IsAloneTag:boolean;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear; override;
       procedure Assign(From:TXMLItem); override;
       function FindParameter(ParameterName:ansistring):TXMLParameter;
       function GetParameter(ParameterName:ansistring;default:ansistring=''):ansistring;
       function AddParameter(AParameter:TXMLParameter):boolean; overload;
       function AddParameter(Name:ansistring;Value:TXMLString):boolean; overload;
       function RemoveParameter(AParameter:TXMLParameter):boolean; overload;
       function RemoveParameter(ParameterName:ansistring):boolean; overload;
     end;

     TXMLProcessTag=class(TXMLTag)
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
     end;

     TXMLScriptTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLCDataTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLDOCTYPETag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLExtraTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXML=class(TXMLClass)
      private
       function ReadXMLText:ansistring;
       procedure WriteXMLText(Text:ansistring);
      public
       Root:TXMLItem;
       AutomaticAloneTagDetection:boolean;
       FormatIndent:boolean;
       FormatIndentText:boolean;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXML);
       function Parse(Stream:TStream):boolean;
       function Read(Stream:TStream):boolean;
       function Write(Stream:TStream;IdentSize:longint=2):boolean;
       property Text:ansistring read ReadXMLText write WriteXMLText;
     end;

function NextPowerOfTwo(Value:longint;const MinThreshold:longint=0):longint;
begin
 result:=(Value or MinThreshold)-1;
 result:=result or (result shr 1);
 result:=result or (result shr 2);
 result:=result or (result shr 4);
 result:=result or (result shr 8);
 result:=result or (result shr 16);
 inc(result);
end;

const EntityChars:array[1..102,1..2] of TXMLString=(('&quot;',#34),('&amp;',#38),('&apos;',''''),
                                                    ('&lt;',#60),('&gt;',#62),('&euro;',#128),('&nbsp;',#160),('&iexcl;',#161),
                                                    ('&cent;',#162),('&pound;',#163),('&curren;',#164),('&yen;',#165),
                                                    ('&brvbar;',#166),('&sect;',#167),('&uml;',#168),('&copy;',#169),
                                                    ('&ordf;',#170),('&laquo;',#171),('&not;',#172),('&shy;',#173),
                                                    ('&reg;',#174),('&macr;',#175),('&deg;',#176),('&plusmn;',#177),
                                                    ('&sup2;',#178),('&sup3;',#179),('&acute;',#180),('&micro;',#181),
                                                    ('&para;',#182),('&middot;',#183),('&cedil;',#184),('&sup1;',#185),
                                                    ('&ordm;',#186),('&raquo;',#187),('&frac14;',#188),('&frac12;',#189),
                                                    ('&frac34;',#190),('&iquest;',#191),('&Agrave;',#192),('&Aacute;',#193),
                                                    ('&Acirc;',#194),('&Atilde;',#195),('&Auml;',#196),('&Aring;',#197),
                                                    ('&AElig;',#198),('&Ccedil;',#199),('&Egrave;',#200),('&Eacute;',#201),
                                                    ('&Ecirc;',#202),('&Euml;',#203),('&Igrave;',#204),('&Iacute;',#205),
                                                    ('&Icirc;',#206),('&Iuml;',#207),('&ETH;',#208),('&Ntilde;',#209),
                                                    ('&Ograve;',#210),('&Oacute;',#211),('&Ocirc;',#212),('&Otilde;',#213),
                                                    ('&Ouml;',#214),('&times;',#215),('&Oslash;',#216),('&Ugrave;',#217),
                                                    ('&Uacute;',#218),('&Ucirc;',#219),('&Uuml;',#220),('&Yacute;',#221),
                                                    ('&THORN;',#222),('&szlig;',#223),('&agrave;',#224),('&aacute;',#225),
                                                    ('&acirc;',#226),('&atilde;',#227),('&auml;',#228),('&aring;',#229),
                                                    ('&aelig;',#230),('&ccedil;',#231),('&egrave;',#232),('&eacute;',#233),
                                                    ('&ecirc;',#234),('&euml;',#235),('&igrave;',#236),('&iacute;',#237),
                                                    ('&icirc;',#238),('&iuml;',#239),('&eth;',#240),('&ntilde;',#241),
                                                    ('&ograve;',#242),('&oacute;',#243),('&ocirc;',#244),('&otilde;',#245),
                                                    ('&ouml;',#246),('&divide;',#247),('&oslash;',#248),('&ugrave;',#249),
                                                    ('&uacute;',#250),('&ucirc;',#251),('&uuml;',#252),('&yacute;',#253),
                                                    ('&thorn;',#254),('&yuml;',#255));

type TEntitiesCharLookUpItem=record
      IsEntity:boolean;
      Entity:ansistring;
     end;

     TEntitiesCharLookUpTable=array[0..{$ifdef UNICODE}65535{$else}255{$endif}] of TEntitiesCharLookUpItem;

var EntitiesCharLookUp:TEntitiesCharLookUpTable;
    EntityStringTree:TXMLStringTree;

const EntityInitialized:boolean=false;

procedure InitializeEntites;
var EntityCounter:longint;
begin
 if not EntityInitialized then begin
  EntityInitialized:=true;
  EntityStringTree:=TXMLStringTree.Create;
  FillChar(EntitiesCharLookUp,sizeof(TEntitiesCharLookUpTable),#0);
  for EntityCounter:=low(EntityChars) to high(EntityChars) do begin
   EntityStringTree.Add(EntityChars[EntityCounter,1],EntityCounter,true);
   with EntitiesCharLookUp[ord(EntityChars[EntityCounter,2][1])] do begin
    IsEntity:=true;
    Entity:=EntityChars[EntityCounter,1];
   end;
  end;
 end;
end;

procedure FinalizeEntites;
begin
 if assigned(EntityStringTree) then begin
  EntityStringTree.Destroy;
  EntityStringTree:=nil;
 end;
 EntityInitialized:=false;
end;

function ConvertToEntities(AString:TXMLString;IdentLevel:longint=0):ansistring;
var Counter,IdentCounter:longint;
    c:TXMLChar;
begin
 result:='';
 for Counter:=1 to length(AString) do begin
  c:=AString[Counter];
  if c=#13 then begin
   if ((Counter+1)<=length(AString)) and (AString[Counter+1]=#10) then begin
    continue;
   end;
   c:=#10;
  end;
  if EntitiesCharLookUp[ord(c)].IsEntity then begin
   result:=result+EntitiesCharLookUp[ord(c)].Entity;
  end else if (c=#9) or (c=#10) or (c=#13) or ((c>=#32) and (c<=#127)) then begin
   result:=result+c;
   if c=#10 then begin
    for IdentCounter:=1 to IdentLevel do begin
     result:=result+' ';
    end;
   end;
  end else begin
{$ifdef UNICODE}
   if c<#255 then begin
    result:=result+'&#'+INTTOSTR(ord(c))+';';
   end else begin
    result:=result+'&#x'+IntToHex(ord(c),4)+';';
   end;
{$else}
   result:=result+'&#'+INTTOSTR(byte(c))+';';
{$endif}
  end;
 end;
end;

constructor TXMLClass.Create;
begin
 inherited Create;
 Previous:=nil;
 Next:=nil;
 Core:=nil;
end;

destructor TXMLClass.Destroy;
begin
 inherited Destroy;
end;

constructor TXMLClassList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=false;
 InternalCount:=0;
 InternalCapacity:=0;
 InternalList:=nil;
 CapacityMinimium:=0;
 Clear;
end;

destructor TXMLClassList.Destroy;
begin
 Clear;
 if assigned(InternalList) and (InternalCapacity<>0) then begin
  FREEMEM(InternalList);
 end;
 inherited Destroy;
end;

procedure TXMLClassList.Clear;
begin
 if ClearWithContentDestroying then begin
  ClearWithFree;
 end else begin
  ClearNoFree;
 end;
end;

procedure TXMLClassList.ClearNoFree;
begin
 SetCount(0);
end;

procedure TXMLClassList.ClearWithFree;
var Counter:longint;
begin
 for Counter:=0 to InternalCount-1 do begin
  if assigned(InternalList^[Counter]) then begin
   try
    InternalList^[Counter].Destroy;
   except
   end;
  end;
 end;
 SetCount(0);
end;

procedure TXMLClassList.SetCapacity(NewCapacity:longint);
begin
 if (InternalCapacity<>NewCapacity) and
    ((NewCapacity>=0) and (NewCapacity<MaxListSize)) then begin
  REALLOCMEM(InternalList,NewCapacity*sizeof(TXMLClass));
  if InternalCapacity<NewCapacity then begin
   FillChar(InternalList^[InternalCapacity],(NewCapacity-InternalCapacity)*sizeof(TXMLClass),#0);
  end;
  InternalCapacity:=NewCapacity;
 end;
end;

procedure TXMLClassList.SetOptimalCapacity(TargetCapacity:longint);
var CapacityMask:longint;
begin
 if (TargetCapacity>=0) and (TargetCapacity<MaxListSize) then begin
  if TargetCapacity<256 then begin
   CapacityMask:=15;
  end else if TargetCapacity<1024 then begin
   CapacityMask:=255;
  end else if TargetCapacity<4096 then begin
   CapacityMask:=1023;
  end else if TargetCapacity<16384 then begin
   CapacityMask:=4095;
  end else if TargetCapacity<65536 then begin
   CapacityMask:=16383;
  end else begin
   CapacityMask:=65535;
  end;
  SetCapacity((TargetCapacity+CapacityMask+CapacityMinimium) and not CapacityMask);
 end;
end;

procedure TXMLClassList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  SetOptimalCapacity(NewCount);
  if InternalCount<NewCount then begin
   FillChar(InternalList^[InternalCount],(NewCount-InternalCount)*sizeof(TXMLClass),#0);
  end;
  InternalCount:=NewCount;
 end;
end;

function TXMLClassList.Add(Item:TXMLClass):longint;
begin
 result:=InternalCount;
 SetCount(result+1);
 InternalList^[result]:=Item;
end;

function TXMLClassList.Append(Item:TXMLClass):longint;
begin
 result:=Add(Item);
end;

function TXMLClassList.AddList(List:TXMLClassList):longint;
var Counter,Index:longint;
begin
 result:=-1;
 for Counter:=0 to List.Count-1 do begin
  Index:=Add(List[Counter]);
  if Counter=0 then begin
   result:=Index;
  end;
 end;
end;

function TXMLClassList.AppendList(List:TXMLClassList):longint;
begin
 result:=AddList(List);
end;

function TXMLClassList.NewClass:TXMLClass;
var Item:TXMLClass;
begin
 Item:=TXMLClass.Create;
 Add(Item);
 result:=Item;
end;

procedure TXMLClassList.Insert(Index:longint;Item:TXMLClass);
var Counter:longint;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  SetCount(InternalCount+1);
  for Counter:=InternalCount-1 downto Index do begin
   InternalList^[Counter+1]:=InternalList^[Counter];
  end;
  InternalList^[Index]:=Item;
 end else if Index=InternalCount then begin
  Add(Item);
 end else if Index>InternalCount then begin
  SetCount(Index);
  Add(Item);
 end;
end;

procedure TXMLClassList.Delete(Index:longint);
var i,j:longint;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  j:=InternalCount-1;
  i:=Index;
  MOVE(InternalList^[i+1],InternalList^[i],(j-i)*sizeof(TXMLClass));
  SetCount(j);
 end;
end;

procedure TXMLClassList.DeleteClass(Index:longint);
var i,j:longint;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  j:=InternalCount-1;
  i:=Index;
  if assigned(InternalList^[i]) then begin
   try
    InternalList^[i].Destroy;
   except
   end;
   InternalList^[i]:=nil;
  end;
  MOVE(InternalList^[i+1],InternalList^[i],(j-i)*sizeof(TXMLClass));
  SetCount(j);
 end;
end;

function TXMLClassList.Remove(Item:TXMLClass):longint;
var i,j,k:longint;
begin
 result:=-1;
 k:=InternalCount;
 j:=-1;
 for i:=0 to k-1 do begin
  if InternalList^[i]=Item then begin
   j:=i;
   break;
  end;
 end;
 if j>=0 then begin
  dec(k);
  MOVE(InternalList^[j+1],InternalList^[j],(k-j)*sizeof(TXMLClass));
  SetCount(k);
  result:=j;
 end;
end;

function TXMLClassList.RemoveClass(Item:TXMLClass):longint;
var i,j,k:longint;
begin
 result:=-1;
 k:=InternalCount;
 j:=-1;
 for i:=0 to k-1 do begin
  if InternalList^[i]=Item then begin
   j:=i;
   break;
  end;
 end;
 if j>=0 then begin
  dec(k);
  MOVE(InternalList^[j+1],InternalList^[j],(k-j)*sizeof(TXMLClass));
  SetCount(k);
  if assigned(Item) then begin
   try
    Item.Destroy;
   except
   end;
  end;
  result:=j;
 end;
end;

function TXMLClassList.Find(Item:TXMLClass):longint;
var i:longint;
begin
 result:=-1;
 for i:=0 to InternalCount-1 do begin
  if InternalList^[i]=Item then begin
   result:=i;
   exit;
  end;
 end;
end;

function TXMLClassList.IndexOf(Item:TXMLClass):longint;
var i:longint;
begin
 result:=-1;
 for i:=0 to InternalCount-1 do begin
  if InternalList^[i]=Item then begin
   result:=i;
   exit;
  end;
 end;
end;

procedure TXMLClassList.Exchange(Index1,Index2:longint);
var TempPointer:TXMLClass;
begin
 if (Index1>=0) and (Index1<InternalCount) and (Index2>=0) and (Index2<InternalCount) then begin
  TempPointer:=InternalList^[Index1];
  InternalList^[Index1]:=InternalList^[Index2];
  InternalList^[Index2]:=TempPointer;
 end;
end;

function TXMLClassList.Push(Item:TXMLClass):longint;
begin
 result:=Add(Item);
end;

function TXMLClassList.Pop(var Item:TXMLClass):boolean;
begin
 result:=InternalCount>0;
 if result then begin
  Item:=InternalList^[InternalCount-1];
  Delete(InternalCount-1);
 end;
end;

function TXMLClassList.Pop:TXMLClass;
begin
 if InternalCount>0 then begin
  result:=InternalList^[InternalCount-1];
  Delete(InternalCount-1);
 end else begin
  result:=nil;
 end;
end;

function TXMLClassList.Last:TXMLClass;
begin
 if InternalCount>0 then begin
  result:=InternalList^[InternalCount-1];
 end else begin
  result:=nil;
 end;
end;

function TXMLClassList.GetItem(Index:longint):TXMLClass;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  result:=InternalList^[Index];
 end else begin
  result:=nil;
 end;
end;

procedure TXMLClassList.SetItem(Index:longint;Value:TXMLClass);
begin
 if (Index>=0) and (Index<InternalCount) then begin
  InternalList^[Index]:=Value;
 end;
end;

function TXMLClassList.GetItemPointer(Index:longint):TXMLClass;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  result:=@InternalList^[Index];
 end else begin
  result:=nil;
 end;
end;

constructor TXMLClassLinkedList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=false;
 ClearNoFree;
end;

destructor TXMLClassLinkedList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TXMLClassLinkedList.Clear;
begin
 if ClearWithContentDestroying then begin
  ClearWithFree;
 end else begin
  ClearNoFree;
 end;
end;

procedure TXMLClassLinkedList.ClearNoFree;
var Current,Next:TXMLClass;
begin
 Current:=First;
 while assigned(Current) do begin
  Next:=Current.Next;
  Remove(Current);
  Current:=Next;
 end;
 First:=nil;
 Last:=nil;
end;

procedure TXMLClassLinkedList.ClearWithFree;
var Current,Next:TXMLClass;
begin
 Current:=First;
 while assigned(Current) do begin
  Next:=Current.Next;
  RemoveClass(Current);
  Current:=Next;
 end;
 First:=nil;
 Last:=nil;
end;

procedure TXMLClassLinkedList.Add(Item:TXMLClass);
begin
 Item.Next:=nil;
 if assigned(Last) then begin
  Last.Next:=Item;
  Item.Previous:=Last;
 end else begin
  Item.Previous:=nil;
  First:=Item;
 end;
 Last:=Item;
end;

procedure TXMLClassLinkedList.Append(Item:TXMLClass);
begin
 Add(Item);
end;

procedure TXMLClassLinkedList.AddLinkedList(List:TXMLClassLinkedList);
begin
 Last.Next:=List.First;
 if assigned(List.First) then begin
  List.First.Previous:=Last;
 end;
 Last:=List.Last;
 List.First:=nil;
 List.Last:=nil;
end;

procedure TXMLClassLinkedList.AppendLinkedList(List:TXMLClassLinkedList);
begin
 AddLinkedList(List);
end;

procedure TXMLClassLinkedList.Remove(Item:TXMLClass);
begin
 if assigned(Item) then begin
  if assigned(Item.Next) then begin
   Item.Next.Previous:=Item.Previous;
  end else if Last=Item then begin
   Last:=Item.Previous;
  end;
  if assigned(Item.Previous) then begin
   Item.Previous.Next:=Item.Next;
  end else if First=Item then begin
   First:=Item.Next;
  end;
  Item.Previous:=nil;
  Item.Next:=nil;
 end;
end;

procedure TXMLClassLinkedList.RemoveClass(Item:TXMLClass);
begin
 if assigned(Item) then begin
  Remove(Item);
  Item.Destroy;
 end;
end;

procedure TXMLClassLinkedList.Push(Item:TXMLClass);
begin
 Add(Item);
end;

function TXMLClassLinkedList.Pop(var Item:TXMLClass):boolean;
begin
 result:=assigned(Last);
 if result then begin
  Item:=Last;
  Remove(Last);
 end;
end;

function TXMLClassLinkedList.Pop:TXMLClass;
begin
 result:=Last;
 if assigned(Last) then begin
  Remove(Last);
 end;
end;

function TXMLClassLinkedList.Count:longint;
var Current:TXMLClass;
begin
 result:=0;
 Current:=First;
 while assigned(Current) do begin
  inc(result);
  Current:=Current.Next;
 end;
end;

constructor TXMLStringTree.Create;
begin
 inherited Create;
 Root:=nil;
 Clear;
end;

destructor TXMLStringTree.Destroy;
begin
 Clear;
 inherited Destroy;
end;

function TXMLStringTree.CreateStringTreeNode(AChar:ansichar):PXMLStringTreeNode;
begin
 GetMem(result,SizeOf(TXMLStringTreeNode));
 result^.TheChar:=AChar;
 result^.Data:=0;
 result^.DataExist:=false;
 result^.Prevoius:=nil;
 result^.Next:=nil;
 result^.Up:=nil;
 result^.Down:=nil;
end;

procedure TXMLStringTree.DestroyStringTreeNode(Node:PXMLStringTreeNode);
begin
 if assigned(Node) then begin
  DestroyStringTreeNode(Node^.Next);
  DestroyStringTreeNode(Node^.Down);
  FreeMem(Node);
 end;
end;

procedure TXMLStringTree.Clear;
begin
 DestroyStringTreeNode(Root);
 Root:=nil;
end;

procedure TXMLStringTree.DumpTree;
var Ident:longint;
 procedure DumpNode(Node:PXMLStringTreeNode);
 var SubNode:PXMLStringTreeNode;
     IdentCounter,IdentOld:longint;
 begin
  for IdentCounter:=1 to Ident do begin
   write(' ');
  end;
  write(Node^.TheChar);
  IdentOld:=Ident;
  SubNode:=Node^.Next;
  while assigned(SubNode) do begin
   write(SubNode.TheChar);
   if not assigned(SubNode^.Next) then begin
    break;
   end;
   inc(Ident);
   SubNode:=SubNode^.Next;
  end;
  writeln;
  inc(Ident);
  while assigned(SubNode) and (SubNode<>Node) do begin
   if assigned(SubNode^.Down) then begin
    DumpNode(SubNode^.Down);
   end;
   SubNode:=SubNode^.Prevoius;
   dec(Ident);
  end;
  Ident:=IdentOld;
  if assigned(Node^.Down) then begin
   DumpNode(Node^.Down);
  end;
 end;
begin
 Ident:=0;
 DumpNode(Root);
end;

procedure TXMLStringTree.DumpList;
 procedure DumpNode(Node:PXMLStringTreeNode;ParentStr:ansistring);
 begin
  if assigned(Node) then begin
   ParentStr:=ParentStr;
   if Node^.DataExist then begin
    writeln(ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Next) then begin
    DumpNode(Node^.Next,ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Down) then begin
    DumpNode(Node^.Down,ParentStr);
   end;
  end;
 end;
begin
 if assigned(Root) then begin
  DumpNode(Root,'');
 end;
end;

procedure TXMLStringTree.AppendTo(DestStringTree:TXMLStringTree);
 procedure DumpNode(Node:PXMLStringTreeNode;ParentStr:ansistring);
 begin
  if assigned(Node) then begin
   ParentStr:=ParentStr;
   if Node^.DataExist then begin
    DestStringTree.Add(ParentStr+Node^.TheChar,Node^.Data);
   end;
   if assigned(Node^.Next) then begin
    DumpNode(Node^.Next,ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Down) then begin
    DumpNode(Node^.Down,ParentStr);
   end;
  end;
 end;
begin
 if assigned(DestStringTree) and assigned(Root) then begin
  DumpNode(Root,'');
 end;
end;

procedure TXMLStringTree.Optimize(DestStringTree:TXMLStringTree);
 procedure DumpNode(Node:PXMLStringTreeNode;ParentStr:ansistring);
 begin
  if assigned(Node) then begin
   ParentStr:=ParentStr;
   if Node^.DataExist then begin
    DestStringTree.Add(ParentStr+Node^.TheChar,Node^.Data);
   end;
   if assigned(Node^.Next) then begin
    DumpNode(Node^.Next,ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Down) then begin
    DumpNode(Node^.Down,ParentStr);
   end;
  end;
 end;
begin
 if assigned(DestStringTree) then begin
  DestStringTree.Clear;
  if assigned(Root) then begin
   DumpNode(Root,'');
  end;
 end;
end;

function TXMLStringTree.Add(Content:ansistring;Data:TXMLStringTreeData;Replace:boolean=false):boolean;
var StringLength,Position,PositionCounter:longint;
    NewNode,LastNode,Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  LastNode:=nil;
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    if NodeChar=StringChar then begin
     LastNode:=Node;
     Node:=Node^.Next;
   end else begin
     while (NodeChar<StringChar) and assigned(Node^.Down) do begin
      Node:=Node^.Down;
      NodeChar:=Node^.TheChar;
     end;
     if NodeChar=StringChar then begin
      LastNode:=Node;
      Node:=Node^.Next;
     end else begin
      NewNode:=CreateStringTreeNode(StringChar);
      if NodeChar<StringChar then begin
       NewNode^.Down:=Node^.Down;
       NewNode^.Up:=Node;
       if assigned(NewNode^.Down) then begin
        NewNode^.Down^.Up:=NewNode;
       end;
       NewNode^.Prevoius:=Node^.Prevoius;
       Node^.Down:=NewNode;
      end else if NodeChar>StringChar then begin
       NewNode^.Down:=Node;
       NewNode^.Up:=Node^.Up;
       if assigned(NewNode^.Up) then begin
        NewNode^.Up^.Down:=NewNode;
       end;
       NewNode^.Prevoius:=Node^.Prevoius;
       if not assigned(NewNode^.Up) then begin
        if assigned(NewNode^.Prevoius) then begin
         NewNode^.Prevoius^.Next:=NewNode;
        end else begin
         Root:=NewNode;
        end;
       end;
       Node^.Up:=NewNode;
      end;
      LastNode:=NewNode;
      Node:=LastNode^.Next;
     end;
    end;
   end else begin
    for PositionCounter:=Position to StringLength do begin
     NewNode:=CreateStringTreeNode(Content[PositionCounter]);
     if assigned(LastNode) then begin
      NewNode^.Prevoius:=LastNode;
      LastNode^.Next:=NewNode;
      LastNode:=LastNode^.Next;
     end else begin
      if not assigned(Root) then begin
       Root:=NewNode;
       LastNode:=Root;
      end;
     end;
    end;
    break;
   end;
  end;
  if assigned(LastNode) then begin
   if Replace or not LastNode^.DataExist then begin
    LastNode^.Data:=Data;
    LastNode^.DataExist:=true;
    result:=true;
   end;
  end;
 end;
end;

function TXMLStringTree.Delete(Content:ansistring):boolean;
var StringLength,Position:longint;
    Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.DataExist then begin
      Node^.DataExist:=false;
      result:=true;
      exit;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TXMLStringTree.Find(Content:ansistring;var Data:TXMLStringTreeData):boolean;
var StringLength,Position:longint;
    Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.DataExist then begin
      Data:=Node^.Data;
      result:=true;
      exit;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TXMLStringTree.FindEx(Content:ansistring;var Data:TXMLStringTreeData;var Len:longint):boolean;
var StringLength,Position:longint;
    Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 Len:=0;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if Node^.DataExist then begin
      Len:=Position;
      Data:=Node^.Data;
      result:=true;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

constructor TXMLItem.Create;
begin
 inherited Create;
 Items:=TXMLItemList.Create;
end;

destructor TXMLItem.Destroy;
begin
 Items.Destroy;
 inherited Destroy;
end;

procedure TXMLItem.Clear;
begin
 Items.Clear;
end;

procedure TXMLItem.Add(Item:TXMLItem);
begin
 Items.Add(Item);
end;

procedure TXMLItem.Assign(From:TXMLItem);
var i:longint;
    NewItem:TXMLItem;
begin
 Items.ClearWithFree;
 NewItem:=nil;
 for i:=0 to Items.Count-1 do begin
  if Items[i] is TXMLTag then begin
   NewItem:=TXMLTag.Create;
  end else if Items[i] is TXMLCommentTag then begin
   NewItem:=TXMLCommentTag.Create;
  end else if Items[i] is TXMLScriptTag then begin
   NewItem:=TXMLScriptTag.Create;
  end else if Items[i] is TXMLProcessTag then begin
   NewItem:=TXMLProcessTag.Create;
  end else if Items[i] is TXMLCDATATag then begin
   NewItem:=TXMLCDATATag.Create;
  end else if Items[i] is TXMLDOCTYPETag then begin
   NewItem:=TXMLDOCTYPETag.Create;
  end else if Items[i] is TXMLExtraTag then begin
   NewItem:=TXMLExtraTag.Create;
  end else if Items[i] is TXMLText then begin
   NewItem:=TXMLText.Create;
  end else if Items[i] is TXMLItem then begin
   NewItem:=Items[i].Create;
  end else begin
   continue;
  end;
  NewItem.Assign(Items[i]);
  Items.Add(NewItem);
 end;
end;

function TXMLItem.FindTag(const TagName:ansistring):TXMLTag;
begin
 result:=Items.FindTag(TagName);
end;

constructor TXMLItemList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=true;
//CapacityMask:=$f;
 CapacityMinimium:=0;
end;

destructor TXMLItemList.Destroy;
begin
 ClearWithFree;
 inherited Destroy;
end;

function TXMLItemList.NewClass:TXMLItem;
begin
 result:=TXMLItem.Create;
 Add(result);
end;

function TXMLItemList.GetItem(Index:longint):TXMLItem;
begin
 result:=TXMLItem(inherited Items[Index]);
end;

procedure TXMLItemList.SetItem(Index:longint;Value:TXMLItem);
begin
 inherited Items[Index]:=Value;
end;

function TXMLItemList.FindTag(const TagName:ansistring):TXMLTag;
var i:longint;
    Item:TXMLItem;
begin
 result:=nil;
 for i:=0 to Count-1 do begin
  Item:=TXMLItem(inherited Items[i]);
  if (assigned(Item) and (Item is TXMLTag)) and (TXMLTag(Item).Name=TagName) then begin
   result:=TXMLTag(Item);
   break;
  end;
 end;
end;

constructor TXMLParameter.Create;
begin
 inherited Create;
 Name:='';
 Value:='';
end;

destructor TXMLParameter.Destroy;
begin
 Name:='';
 Value:='';
 inherited Destroy;
end;

procedure TXMLParameter.Assign(From:TXMLParameter);
begin
 Name:=From.Name;
 Value:=From.Value;
end;

constructor TXMLText.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLText.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLText.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLText then begin
  Text:=TXMLText(From).Text;
 end;
end;

procedure TXMLText.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLCommentTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLCommentTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLCommentTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLCommentTag then begin
  Text:=TXMLCommentTag(From).Text;
 end;
end;

procedure TXMLCommentTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLTag.Create;
begin
 inherited Create;
 StringTree:=TXMLStringTree.Create;
 Name:='';
 Parameter:=nil;
end;

destructor TXMLTag.Destroy;
begin
 Clear;
 StringTree.Destroy;
 StringTree:=nil;
 inherited Destroy;
end;

procedure TXMLTag.Clear;
var Counter:longint;
begin
 inherited Clear;
 if assigned(StringTree) then begin
  StringTree.Clear;
 end;
 for Counter:=0 to length(Parameter)-1 do begin
  Parameter[Counter].Free;
 end;
 SetLength(Parameter,0);
 Name:='';
end;

procedure TXMLTag.Assign(From:TXMLItem);
var Counter:longint;
begin
 inherited Assign(From);
 if From is TXMLTag then begin
  for Counter:=0 to length(Parameter)-1 do begin
   Parameter[Counter].Free;
  end;
  SetLength(Parameter,0);
  Name:=TXMLTag(From).Name;
  for Counter:=0 to length(TXMLTag(From).Parameter)-1 do begin
   AddParameter(TXMLTag(From).Parameter[Counter].Name,TXMLTag(From).Parameter[Counter].Value);
  end;
 end;
end;

function TXMLTag.FindParameter(ParameterName:ansistring):TXMLParameter;
var Link:TXMLStringTreeData;
    LinkClass:TXMLParameter absolute Link;
begin
 if StringTree.Find(ParameterName,Link) then begin
  result:=LinkClass;
 end else begin
  result:=nil;
 end;
end;

function TXMLTag.GetParameter(ParameterName:ansistring;default:ansistring=''):ansistring;
var Link:TXMLStringTreeData;
    LinkClass:TXMLParameter absolute Link;
begin
 if StringTree.Find(ParameterName,Link) then begin
  result:=LinkClass.Value;
 end else begin
  result:=default;
 end;
end;

function TXMLTag.AddParameter(AParameter:TXMLParameter):boolean;
var Index:longint;
begin
 try
  Index:=length(Parameter);
  SetLength(Parameter,Index+1);
  Parameter[Index]:=AParameter;
  StringTree.Add(AParameter.Name,longword(AParameter),true);
  result:=true;
 except
  result:=false;
 end;
end;

function TXMLTag.AddParameter(Name:ansistring;Value:TXMLString):boolean;
var AParameter:TXMLParameter;
begin
 AParameter:=TXMLParameter.Create;
 AParameter.Name:=Name;
 AParameter.Value:=Value;
 result:=AddParameter(AParameter);
end;

function TXMLTag.RemoveParameter(AParameter:TXMLParameter):boolean;
var Found,Counter:longint;
begin
 result:=false;
 try
  Found:=-1;
  for Counter:=0 to length(Parameter)-1 do begin
   if Parameter[Counter]=AParameter then begin
    Found:=Counter;
    break;
   end;
  end;
  if Found>=0 then begin
   for Counter:=Found to length(Parameter)-2 do begin
    Parameter[Counter]:=Parameter[Counter+1];
   end;
   SetLength(Parameter,length(Parameter)-1);
   if Found>=length(Parameter) then begin
    StringTree.Delete(AParameter.Name);
   end else begin
    StringTree.Clear;
    for Counter:=0 to length(Parameter)-1 do begin
     StringTree.Add(Parameter[Counter].Name,Counter);
    end;
   end;
   AParameter.Destroy;
   result:=true;
  end;
 except
 end;
end;

function TXMLTag.RemoveParameter(ParameterName:ansistring):boolean;
begin
 result:=RemoveParameter(FindParameter(ParameterName));
end;

constructor TXMLProcessTag.Create;
begin
 inherited Create;
end;

destructor TXMLProcessTag.Destroy;
begin
 inherited Destroy;
end;

procedure TXMLProcessTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
end;

constructor TXMLScriptTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLScriptTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLScriptTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLScriptTag then begin
  Text:=TXMLScriptTag(From).Text;
 end;
end;

procedure TXMLScriptTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLCDataTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLCDataTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLCDataTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLCDataTag then begin
  Text:=TXMLCDataTag(From).Text;
 end;
end;

procedure TXMLCDataTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLDOCTYPETag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLDOCTYPETag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLDOCTYPETag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLDOCTYPETag then begin
  Text:=TXMLDOCTYPETag(From).Text;
 end;
end;

procedure TXMLDOCTYPETag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLExtraTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLExtraTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLExtraTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLExtraTag then begin
  Text:=TXMLExtraTag(From).Text;
 end;
end;

procedure TXMLExtraTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXML.Create;
begin
 inherited Create;
 InitializeEntites;
 Root:=TXMLItem.Create;
 AutomaticAloneTagDetection:=true;
 FormatIndent:=true;
 FormatIndentText:=false;
end;

destructor TXML.Destroy;
begin
 Root.Free;
 inherited Destroy;
end;

procedure TXML.Assign(From:TXML);
begin
 Root.Assign(From.Root);
 AutomaticAloneTagDetection:=From.AutomaticAloneTagDetection;
 FormatIndent:=From.FormatIndent;
 FormatIndentText:=From.FormatIndentText;
end;

function TXML.Parse(Stream:TStream):boolean;
const NameCanBeginWithCharSet:set of ansichar=['A'..'Z','a'..'z','_'];
      NameCanContainCharSet:set of ansichar=['A'..'Z','a'..'z','0'..'9','.',':','_','-'];
      BlankCharSet:set of ansichar=[#0..#$20];//[#$9,#$A,#$D,#$20];
type TEncoding=(etASCII,etUTF8,etUTF16);
var Errors:boolean;
    CurrentChar:ansichar;
    StreamEOF:boolean;
    Encoding:TEncoding;

 function IsEOF:boolean;
 begin
  result:=StreamEOF or (Stream.Position>Stream.Size);
 end;

 function IsEOFOrErrors:boolean;
 begin
  result:=IsEOF or Errors;
 end;

 function NextChar:ansichar;
 begin
  if Stream.Read(CurrentChar,sizeof(ansichar))<>sizeof(ansichar) then begin
   StreamEOF:=true;
   CurrentChar:=#0;
  end;
  result:=CurrentChar;
//system.Write(result);
 end;

 procedure SkipBlank;
 begin
  while (CurrentChar in BlankCharSet) and not IsEOFOrErrors do begin
   NextChar;
  end;
 end;

 function GetName:ansistring;
 var i:longint;
 begin
  result:='';
  i:=0;
  if (CurrentChar in NameCanBeginWithCharSet) and not IsEOFOrErrors then begin
   while (CurrentChar in NameCanContainCharSet) and not IsEOFOrErrors do begin
    inc(i);
    if (i+1)>length(result) then begin
     SetLength(result,NextPowerOfTwo(i+1));
    end;
    result[i]:=CurrentChar;
    NextChar;
   end;
  end;
  SetLength(result,i);
 end;

 function ExpectToken(const S:ansistring):boolean; overload;
 var i:longint;
 begin
  result:=true;
  for i:=1 to length(S) do begin
   if S[i]<>CurrentChar then begin
    result:=false;
    break;
   end;
   NextChar;
  end;
 end;

 function ExpectToken(const c:ansichar):boolean; overload;
 begin
  result:=false;
  if c=CurrentChar then begin
   result:=true;
   NextChar;
  end;
 end;

 function GetUntil(var Content:ansistring;const TerminateToken:ansistring):boolean;
 var i,j,OldPosition:longint;
     OldEOF:boolean;
     OldChar:ansichar;
 begin
  result:=false;
  j:=0;
  Content:='';
  while not IsEOFOrErrors do begin
   if (length(TerminateToken)>0) and (TerminateToken[1]=CurrentChar) and ((Stream.Size-Stream.Position+1)>=length(TerminateToken)) then begin
    OldPosition:=Stream.Position;
    OldEOF:=StreamEOF;
    OldChar:=CurrentChar;
    for i:=1 to length(TerminateToken) do begin
     if TerminateToken[i]=CurrentChar then begin
      if i=length(TerminateToken) then begin
       NextChar;
       result:=true;
       exit;
      end;
     end else begin
      break;
     end;
     NextChar;
    end;
    Stream.Seek(OldPosition,soFromBeginning);
    StreamEOF:=OldEOF;
    CurrentChar:=OldChar;
   end;
   inc(j);
   if (j+1)>length(Content) then begin
    SetLength(Content,NextPowerOfTwo(j+1));
   end;
   Content[j]:=CurrentChar;
   NextChar;
  end;
  SetLength(Content,j);
 end;

 function GetDecimalValue:longint;
 var Negitive:boolean;
 begin
  Negitive:=CurrentChar='-';
  if Negitive then begin
   NextChar;
  end else if CurrentChar='+' then begin
   NextChar;
  end;
  result:=0;
  while (CurrentChar in ['0'..'9']) and not IsEOFOrErrors do begin
   result:=(result*10)+(ord(CurrentChar)-ord('0'));
   NextChar;
  end;
  if Negitive then begin
   result:=-result;
  end;
 end;

 function GetHeximalValue:longint;
 var Negitive:boolean;
     Value:longint;
 begin
  Negitive:=CurrentChar='-';
  if Negitive then begin
   NextChar;
  end else if CurrentChar='+' then begin
   NextChar;
  end;
  result:=0;
  Value:=0;
  while not IsEOFOrErrors do begin
   case CurrentChar of
    '0'..'9':begin
     Value:=byte(CurrentChar)-ord('0');
    end;
    'A'..'F':begin
     Value:=byte(CurrentChar)-ord('A')+$a;
    end;
    'a'..'f':begin
     Value:=byte(CurrentChar)-ord('a')+$a;
    end;
    else begin
     break;
    end;
   end;
   result:=(result*16)+Value;
   NextChar;
  end;
  if Negitive then begin
   result:=-result;
  end;
 end;

 function GetEntity:TXMLString;
 var Value:longint;
     Entity:ansistring;
     c:TXMLChar;
     EntityLink:TXMLStringTreeData;
 begin
  result:='';
  if CurrentChar='&' then begin
   NextChar;
   if not IsEOF then begin
    if CurrentChar='#' then begin
     NextChar;
     if IsEOF then begin
      Errors:=true;
     end else begin
      if CurrentChar='x' then begin
       NextChar;
       Value:=GetHeximalValue;
      end else begin
       Value:=GetDecimalValue;
      end;
      if CurrentChar=';' then begin
       NextChar;
{$ifdef UNICODE}
       c:=widechar(word(Value));
{$else}
       c:=ansichar(byte(Value));
{$endif}
       result:=c;
      end else begin
       Errors:=true;
      end;
     end;
    end else begin
     Entity:='&';
     while (CurrentChar in ['a'..'z','A'..'Z','0'..'9','_']) and not IsEOFOrErrors do begin
      Entity:=Entity+CurrentChar;
      NextChar;
     end;
     if CurrentChar=';' then begin
      Entity:=Entity+CurrentChar;
      NextChar;
      if EntityStringTree.Find(Entity,EntityLink) then begin
       result:=EntityChars[EntityLink,2];
      end else begin
       result:=Entity;
      end;
     end else begin
      Errors:=true;
     end;
    end;
   end;
  end;
 end;

 function ParseTagParameterValue(TerminateChar:ansichar):TXMLString;
 var i,wc,c:longint;
 begin
  result:='';
  SkipBlank;
  i:=0;
  while (CurrentChar<>TerminateChar) and not IsEOFOrErrors do begin
   if (Encoding=etUTF8) and (ord(CurrentChar)>=$80) then begin
    wc:=ord(CurrentChar) and $3f;
    if (wc and $20)<>0 then begin
     NextChar;
     c:=ord(CurrentChar);
     if (c and $c0)<>$80 then begin
      break;
     end;
     wc:=(wc shl 6) or (c and $3f);
    end;
    NextChar;
    c:=ord(CurrentChar);
    if (c and $c0)<>$80 then begin
     break;
    end;
    wc:=(wc shl 6) or (c and $3f);
    NextChar;
    inc(i);
    if (i+1)>length(result) then begin
     SetLength(result,NextPowerOfTwo(i+1));
    end;
{$ifdef UNICODE}
    result[i]:=widechar(wc);
{$else}
    result[i]:=ansichar(wc);
{$endif}
   end else if CurrentChar='&' then begin
    SetLength(result,i);
    result:=result+GetEntity;
    i:=length(result);
   end else begin
    inc(i);
    if (i+1)>length(result) then begin
     SetLength(result,NextPowerOfTwo(i+1));
    end;
{$ifdef UNICODE}
    result[i]:=widechar(word(byte(CurrentChar)+0));
{$else}
    result[i]:=CurrentChar;
{$endif}
    NextChar;
   end;
  end;
  SetLength(result,i);
  NextChar;
 end;

 procedure ParseTagParameter(XMLTag:TXMLTag);
 var ParameterName,ParameterValue:ansistring;
     TerminateChar:ansichar;
 begin
  SkipBlank;
  while (CurrentChar in NameCanBeginWithCharSet) and not IsEOFOrErrors do begin
   ParameterName:=GetName;
   SkipBlank;
   if CurrentChar='=' then begin
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     break;
    end;
   end else begin
    Errors:=true;
    break;
   end;
   SkipBlank;
   if CurrentChar in ['''','"'] then begin
    TerminateChar:=CurrentChar;
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     break;
    end;
    ParameterValue:=ParseTagParameterValue(TerminateChar);
    if Errors then begin
     break;
    end else begin
     XMLTag.AddParameter(ParameterName,ParameterValue);
     SkipBlank;
    end;
   end else begin
    Errors:=true;
    break;
   end;
  end;
 end;

 procedure Process(ParentItem:TXMLItem;Closed:boolean);
 var FinishLevel:boolean;

  procedure ParseText;
  var Text:TXMLString;
      XMLText:TXMLText;
      i,wc,c:longint;
{$ifndef UNICODE}
      w:ansistring;
{$endif}
  begin
   SkipBlank;
   if CurrentChar='<' then begin
    exit;
   end;
   i:=0;
   Text:='';
   SetLength(Text,16);
   while (CurrentChar<>'<') and not IsEOFOrErrors do begin
    if (Encoding=etUTF8) and (ord(CurrentChar)>=$80) then begin
     wc:=ord(CurrentChar) and $3f;
     if (wc and $20)<>0 then begin
      NextChar;
      c:=ord(CurrentChar);
      if (c and $c0)<>$80 then begin
       break;
      end;
      wc:=(wc shl 6) or (c and $3f);
     end;
     NextChar;
     c:=ord(CurrentChar);
     if (c and $c0)<>$80 then begin
      break;
     end;
     wc:=(wc shl 6) or (c and $3f);
     NextChar;
{$ifdef UNICODE}
     if wc<=$d7ff then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word(wc));
     end else if wc<=$dfff then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=#$fffd;
     end else if wc<=$fffd then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word(wc));
     end else if wc<=$ffff then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=#$fffd;
     end else if wc<=$10ffff then begin
      dec(wc,$10000);
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word((wc shr 10) or $d800));
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word((wc and $3ff) or $dc00));
     end else begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=#$fffd;
     end;
{$else}
     if wc<$80 then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=ansichar(byte(wc));
     end else begin
      w:=UTF32CharToUTF8(wc);
      if length(w)>0 then begin
       inc(i);
       if (i+length(w)+1)>length(Text) then begin
        SetLength(Text,NextPowerOfTwo(i+length(w)+1));
       end;
       Move(w[1],Text[i],length(w));
       inc(i,length(w)-1);
      end;
     end;
{$endif}
    end else if CurrentChar='&' then begin
     SetLength(Text,i);
     Text:=Text+GetEntity;
     i:=length(Text);
    end else if CurrentChar in BlankCharSet then begin
{$ifdef UNICODE}
     inc(i);
     if (i+1)>length(Text) then begin
      SetLength(Text,NextPowerOfTwo(i+1));
     end;
     Text[i]:=widechar(word(byte(CurrentChar)+0));
{$else}
     wc:=ord(CurrentChar);
     if wc<$80 then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=ansichar(byte(wc));
     end else begin
      w:=UTF32CharToUTF8(wc);
      if length(w)>0 then begin
       inc(i);
       if (i+length(w)+1)>length(Text) then begin
        SetLength(Text,NextPowerOfTwo(i+length(w)+1));
       end;
       Move(w[1],Text[i],length(w));
       inc(i,length(w)-1);
      end;
     end;
{$endif}
     SkipBlank;
    end else begin
{$ifdef UNICODE}
     inc(i);
     if (i+1)>length(Text) then begin
      SetLength(Text,NextPowerOfTwo(i+1));
     end;
     Text[i]:=widechar(word(byte(CurrentChar)+0));
{$else}
     wc:=ord(CurrentChar);
     if wc<$80 then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=ansichar(byte(wc));
     end else begin
      w:=UTF32CharToUTF8(wc);
      if length(w)>0 then begin
       inc(i);
       if (i+length(w)+1)>length(Text) then begin
        SetLength(Text,NextPowerOfTwo(i+length(w)+1));
       end;
       Move(w[1],Text[i],length(w));
       inc(i,length(w)-1);
      end;
     end;
{$endif}
     NextChar;
    end;
   end;
   SetLength(Text,i);
   if length(Text)<>0 then begin
    XMLText:=TXMLText.Create;
    XMLText.Text:=Text;
    ParentItem.Add(XMLText);
   end;
  end;

  procedure ParseProcessTag;
  var TagName,EncodingName:ansistring;
      XMLProcessTag:TXMLProcessTag;
  begin
   if not ExpectToken('?') then begin
    Errors:=true;
    exit;
   end;
   TagName:=GetName;
   if IsEOF or Errors then begin
    Errors:=true;
    exit;
   end;
   XMLProcessTag:=TXMLProcessTag.Create;
   XMLProcessTag.Name:=TagName;
   ParentItem.Add(XMLProcessTag);
   ParseTagParameter(XMLProcessTag);
   if not ExpectToken('?>') then begin
    Errors:=true;
    exit;
   end;
   if XMLProcessTag.Name='xml' then begin
    EncodingName:=UPPERCASE(XMLProcessTag.GetParameter('encoding','ascii'));
    if EncodingName='UTF-8' then begin
     Encoding:=etUTF8;
    end else if EncodingName='UTF-16' then begin
     Encoding:=etUTF16;
    end else begin
     Encoding:=etASCII;
    end;
   end;
  end;

  procedure ParseScriptTag;
  var XMLScriptTag:TXMLScriptTag;
  begin
   if not ExpectToken('%') then begin
    Errors:=true;
    exit;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;
   XMLScriptTag:=TXMLScriptTag.Create;
   ParentItem.Add(XMLScriptTag);
   if not GetUntil(XMLScriptTag.Text,'%>') then begin
    Errors:=true;
   end;
  end;

  procedure ParseCommentTag;
  var XMLCommentTag:TXMLCommentTag;
  begin
   if not ExpectToken('--') then begin
    Errors:=true;
    exit;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;
   XMLCommentTag:=TXMLCommentTag.Create;
   ParentItem.Add(XMLCommentTag);
   if not GetUntil(XMLCommentTag.Text,'-->') then begin
    Errors:=true;
   end;
  end;

  procedure ParseCDATATag;
  var XMLCDataTag:TXMLCDataTag;
  begin
   if not ExpectToken('[CDATA[') then begin
    Errors:=true;
    exit;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;
   XMLCDataTag:=TXMLCDataTag.Create;
   ParentItem.Add(XMLCDataTag);
   if not GetUntil(XMLCDataTag.Text,']]>') then begin
    Errors:=true;
   end;
  end;

  procedure ParseDOCTYPEOrExtraTag;
  var Content:ansistring;
      XMLDOCTYPETag:TXMLDOCTYPETag;
      XMLExtraTag:TXMLExtraTag;
  begin
   Content:='';
   if not GetUntil(Content,'>') then begin
    Errors:=true;
    exit;
   end;
   if POS('DOCTYPE',Content)=1 then begin
    XMLDOCTYPETag:=TXMLDOCTYPETag.Create;
    ParentItem.Add(XMLDOCTYPETag);
    XMLDOCTYPETag.Text:=TRIMLEFT(COPY(Content,8,length(Content)-7));
   end else begin
    XMLExtraTag:=TXMLExtraTag.Create;
    ParentItem.Add(XMLExtraTag);
    XMLExtraTag.Text:=Content;
   end;
  end;

  procedure ParseTag;
  var TagName:ansistring;
      XMLTag:TXMLTag;
      IsAloneTag:boolean;
  begin
   if CurrentChar='/' then begin
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     exit;
    end;
    TagName:='/'+GetName;
   end else begin
    TagName:=GetName;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;

   XMLTag:=TXMLTag.Create;
   XMLTag.Name:=TagName;
   ParseTagParameter(XMLTag);

   IsAloneTag:=CurrentChar='/';
   if IsAloneTag then begin
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     exit;
    end;
   end;

   if CurrentChar<>'>' then begin
    Errors:=true;
    exit;
   end;
   NextChar;

   if (ParentItem<>Root) and (ParentItem is TXMLTag) and (XMLTag.Name='/'+TXMLTag(ParentItem).Name) then begin
    XMLTag.Destroy;
    FinishLevel:=true;
    Closed:=true;
   end else begin
    ParentItem.Add(XMLTag);
    if not IsAloneTag then begin
     Process(XMLTag,false);
    end;
   end;
// IsAloneTag:=false;
  end;

 begin
  FinishLevel:=false;
  while not (IsEOFOrErrors or FinishLevel) do begin
   ParseText;
   if CurrentChar='<' then begin
    NextChar;
    if not IsEOFOrErrors then begin
     if CurrentChar='/' then begin
      ParseTag;
     end else if CurrentChar='?' then begin
      ParseProcessTag;
     end else if CurrentChar='%' then begin
      ParseScriptTag;
     end else if CurrentChar='!' then begin
      NextChar;
      if not IsEOFOrErrors then begin
       if CurrentChar='-' then begin
        ParseCommentTag;
       end else if CurrentChar='[' then begin
        ParseCDATATag;
       end else begin
        ParseDOCTYPEOrExtraTag;
       end;
      end;
     end else begin
      ParseTag;
     end;
    end;
   end;
  end;
  if not Closed then begin
   Errors:=true;
  end;
 end;
begin
 Encoding:=etASCII;
 Errors:=false;
 CurrentChar:=#0;
 Root.Clear;
 StreamEOF:=false;
 Stream.Seek(0,soFromBeginning);
 NextChar;
 Process(Root,true);
 if Errors then begin
  Root.Clear;
 end;
 result:=not Errors;
end;

function TXML.Read(Stream:TStream):boolean;
begin
 result:=Parse(Stream);
end;

function TXML.Write(Stream:TStream;IdentSize:longint=2):boolean;
var IdentLevel:longint;
    Errors:boolean;
 procedure Process(Item:TXMLItem;DoIndent:boolean);
 var Line:ansistring;
     Counter:longint;
     TagWithSingleLineText,ItemsText:boolean;
  procedure WriteLineEx(Line:ansistring);
  begin
   if length(Line)>0 then begin
    if Stream.Write(Line[1],length(Line))<>length(Line) then begin
     Errors:=true;
    end;
   end;
  end;
  procedure WriteLine(Line:ansistring);
  begin
   if FormatIndent and DoIndent then begin
    Line:=Line+#10;
   end;
   if length(Line)>0 then begin
    if Stream.Write(Line[1],length(Line))<>length(Line) then begin
     Errors:=true;
    end;
   end;
  end;
 begin
  if not Errors then begin
   if assigned(Item) then begin
    inc(IdentLevel,IdentSize);
    Line:='';
    if FormatIndent and DoIndent then begin
     for Counter:=1 to IdentLevel do begin
      Line:=Line+' ';
     end;
    end;
    if Item is TXMLText then begin
     if FormatIndentText then begin
      Line:=Line+ConvertToEntities(TXMLText(Item).Text,IdentLevel);
     end else begin
      Line:=ConvertToEntities(TXMLText(Item).Text);
     end;
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLCommentTag then begin
     Line:=Line+'<!--'+TXMLCommentTag(Item).Text+'-->';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLProcessTag then begin
     Line:=Line+'<?'+TXMLProcessTag(Item).Name;
     for Counter:=0 to length(TXMLProcessTag(Item).Parameter)-1 do begin
      if assigned(TXMLProcessTag(Item).Parameter[Counter]) then begin
       Line:=Line+' '+TXMLProcessTag(Item).Parameter[Counter].Name+'="'+ConvertToEntities(TXMLProcessTag(Item).Parameter[Counter].Value)+'"';
      end;
     end;
     Line:=Line+'?>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLScriptTag then begin
     Line:=Line+'<%'+TXMLScriptTag(Item).Text+'%>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLCDataTag then begin
     Line:=Line+'<![CDATA['+TXMLCDataTag(Item).Text+']]>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLDOCTYPETag then begin
     Line:=Line+'<!DOCTYPE '+TXMLDOCTYPETag(Item).Text+'>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLExtraTag then begin
     Line:=Line+'<!'+TXMLExtraTag(Item).Text+'>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLTag then begin
     if AutomaticAloneTagDetection then begin
      TXMLTag(Item).IsAloneTag:=TXMLTag(Item).Items.Count=0;
     end;
     Line:=Line+'<'+TXMLTag(Item).Name;
     for Counter:=0 to length(TXMLTag(Item).Parameter)-1 do begin
      if assigned(TXMLTag(Item).Parameter[Counter]) then begin
       Line:=Line+' '+TXMLTag(Item).Parameter[Counter].Name+'="'+ConvertToEntities(TXMLTag(Item).Parameter[Counter].Value)+'"';
      end;
     end;
     if TXMLTag(Item).IsAloneTag then begin
      Line:=Line+' />';
      WriteLine(Line);
     end else begin
      TagWithSingleLineText:=false;
      if Item.Items.Count=1 then begin
       if assigned(Item.Items[0]) then begin
        if Item.Items[0] is TXMLText then begin
         if ((POS(#13,TXMLText(Item.Items[0]).Text)=0) and
             (POS(#10,TXMLText(Item.Items[0]).Text)=0)) or not FormatIndentText then begin
          TagWithSingleLineText:=true;
         end;
        end;
       end;
      end;
      ItemsText:=false;
      for Counter:=0 to Item.Items.Count-1 do begin
       if assigned(Item.Items[Counter]) then begin
        if Item.Items[Counter] is TXMLText then begin
         ItemsText:=true;
        end;
       end;
      end;
      if TagWithSingleLineText then begin
       Line:=Line+'>'+ConvertToEntities(TXMLText(Item.Items[0]).Text)+'</'+TXMLTag(Item).Name+'>';
       WriteLine(Line);
      end else if Item.Items.Count<>0 then begin
       Line:=Line+'>';
       if assigned(Item.Items[0]) and (Item.Items[0] is TXMLText) and not FormatIndentText then begin
        WriteLineEx(Line);
       end else begin
        WriteLine(Line);
       end;
       for Counter:=0 to Item.Items.Count-1 do begin
        Process(Item.Items[Counter],DoIndent and ((not ItemsText) or (FormatIndent and FormatIndentText)));
       end;
       Line:='';
       if DoIndent and ((not ItemsText) or (FormatIndent and FormatIndentText)) then begin
        for Counter:=1 to IdentLevel do begin
         Line:=Line+' ';
        end;
       end;
       Line:=Line+'</'+TXMLTag(Item).Name+'>';
       WriteLine(Line);
      end else begin
       Line:=Line+'></'+TXMLTag(Item).Name+'>';
       WriteLine(Line);
      end;
     end;
    end else begin
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end;
    dec(IdentLevel,IdentSize);
   end;
  end;
 end;
begin
 IdentLevel:=-(2*IdentSize);
 if Stream is TMemoryStream then begin
  TMemoryStream(Stream).Clear;
 end;
 Errors:=false;
 Process(Root,FormatIndent);
 result:=not Errors;
end;

function TXML.ReadXMLText:ansistring;
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 Write(Stream);
 if Stream.Size>0 then begin
  SetLength(result,Stream.Size);
  Stream.Seek(0,soFromBeginning);
  Stream.Read(result[1],Stream.Size);
 end else begin
  result:='';
 end;
 Stream.Destroy;
end;

procedure TXML.WriteXMLText(Text:ansistring);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 if length(Text)>0 then begin
  Stream.Write(Text[1],length(Text));
  Stream.Seek(0,soFromBeginning);
 end;
 Parse(Stream);
 Stream.Destroy;
end;

function ParseText(ParentItem:TXMLItem):ansistring;
var XMLItemIndex:longint;
    XMLItem:TXMLItem;
begin
 result:='';
 if assigned(ParentItem) then begin
  for XMLItemIndex:=0 to ParentItem.Items.Count-1 do begin
   XMLItem:=ParentItem.Items[XMLItemIndex];
   if assigned(XMLItem) then begin
    if XMLItem is TXMLText then begin
     result:=result+TXMLText(XMLItem).Text;
    end else if XMLItem is TXMLTag then begin
     if TXMLTag(XMLItem).Name='br' then begin
      result:=result+#13#10;
     end;
     result:=result+ParseText(XMLItem)+' ';
    end;
   end;
  end;
 end;
end;

type TVendorID=class
      public
       Name:ansistring;
       ID:longint;
       Comment:ansistring;
     end;

     TTag=class
      public
       Name:ansistring;
       Author:ansistring;
       Contact:ansistring;
     end;

     TExtension=class;

     TExtensionEnum=class
      public
       Extension:TExtension;
       Name:ansistring;
       Value:ansistring;
       Offset:longint;
       Dir:ansistring;
       Extends:ansistring;
     end;

     TExtensionType=class
      public
       Name:ansistring;
     end;

     TExtensionCommand=class
      public
       Name:ansistring;
     end;

     TExtension=class
      public
       Name:ansistring;
       Number:longint;
       Protect:ansistring;
       Author:ansistring;
       Contact:ansistring;
       Supported:ansistring;
       Enums:TObjectList;
       Types:TObjectList;
       Commands:TObjectList;
       constructor Create;
       destructor Destroy;
     end;

constructor TExtension.Create;
begin
 inherited Create;
 Name:='';
 Number:=0;
 Protect:='';
 Author:='';
 Contact:='';
 Supported:='';
 Enums:=TObjectList.Create(true);
 Types:=TObjectList.Create(true);
 Commands:=TObjectList.Create(true);
end;

destructor TExtension.Destroy;
begin
 Enums.Free;
 Types.Free;
 Commands.Free;
 inherited Destroy;
end;

var Comment:ansistring;
    VendorIDList:TObjectList;
    TagList:TObjectList;
    Extensions:TObjectList;
    ExtensionEnums:TStringList;
    ExtensionTypes:TStringList;
    ExtensionCommands:TStringList;
    BaseTypes:TStringList;
    BitMaskTypes:TStringList;
    HandleTypes:TStringList;
    ENumTypes:TStringList;
    ENumConstants:TStringList;
    TypeDefinitionTypes:TStringList;
    CommandTypes:TStringList;
    CommandVariables:TStringList;
    AllCommandType:TStringList;
    VersionMajor,VersionMinor,VersionPatch:longint;

function TranslateType(Type_:ansistring;const Ptr:boolean):ansistring;
begin
 if Ptr then begin
  if Type_='void' then begin
   result:='TVkPointer';
  end else if Type_='char' then begin
   result:='PVkChar';
  end else if Type_='float' then begin
   result:='PVkFloat';
  end else if Type_='double' then begin
   result:='PVkDouble';
  end else if Type_='int8_t' then begin
   result:='PVkInt8';
  end else if Type_='uint8_t' then begin
   result:='PVkUInt8';
  end else if Type_='int16_t' then begin
   result:='PVkInt16';
  end else if Type_='uint16_t' then begin
   result:='PVkUInt16';
  end else if Type_='int32_t' then begin
   result:='PVkInt32';
  end else if Type_='uint32_t' then begin
   result:='PVkUInt32';
  end else if Type_='int64_t' then begin
   result:='PVkInt64';
  end else if Type_='uint64_t' then begin
   result:='PVkUInt64';
  end else if Type_='size_t' then begin
   result:='PVkPtrInt';
  end else if Type_='VK_DEFINE_HANDLE' then begin
   result:='PVkDispatchableHandle';
  end else if Type_='VK_DEFINE_NON_DISPATCHABLE_HANDLE' then begin
   result:='PVkNonDispatchableHandle';
  end else if Type_='HINSTANCE' then begin
   result:='PVkHINSTANCE';
  end else if Type_='HWND' then begin
   result:='PVkHWND';
  end else if Type_='Display' then begin
   result:='PDisplay';
  end else if Type_='VisualID' then begin
   result:='PVisualID';
  end else if Type_='Window' then begin
   result:='PWindow';
  end else if Type_='xcb_connection_t' then begin
   result:='Pxcb_connection';
  end else if Type_='xcb_visualid_t' then begin
   result:='Pxcb_visualid';
  end else if Type_='xcb_window_t' then begin
   result:='Pxcb_window';
  end else if Type_='wl_display' then begin
   result:='Pwl_display';
  end else if Type_='wl_surface' then begin
   result:='Pwl_surface';
  end else if Type_='MirConnection' then begin
   result:='PMirConnection';
  end else if Type_='MirSurface' then begin
   result:='PMirSurface';
  end else if Type_='ANativeWindow' then begin
   result:='PANativeWindow';
  end else begin
   result:='P'+Type_;
  end;
 end else begin
  if Type_='void' then begin
   result:='TVkPointer';
  end else if Type_='char' then begin
   result:='TVkChar';
  end else if Type_='float' then begin
   result:='TVkFloat';
  end else if Type_='double' then begin
   result:='TVkDouble';
  end else if Type_='int8_t' then begin
   result:='TVkInt8';
  end else if Type_='uint8_t' then begin
   result:='TVkUInt8';
  end else if Type_='int16_t' then begin
   result:='TVkInt16';
  end else if Type_='uint16_t' then begin
   result:='TVkUInt16';
  end else if Type_='int32_t' then begin
   result:='TVkInt32';
  end else if Type_='uint32_t' then begin
   result:='TVkUInt32';
  end else if Type_='int64_t' then begin
   result:='TVkInt64';
  end else if Type_='uint64_t' then begin
   result:='TVkUInt64';
  end else if Type_='size_t' then begin
   result:='TVkPtrInt';
  end else if Type_='VK_DEFINE_HANDLE' then begin
   result:='TVkDispatchableHandle';
  end else if Type_='VK_DEFINE_NON_DISPATCHABLE_HANDLE' then begin
   result:='TVkNonDispatchableHandle';
  end else if Type_='HINSTANCE' then begin
   result:='TVkHINSTANCE';
  end else if Type_='HWND' then begin
   result:='TVkHWND';
  end else if Type_='Display' then begin
   result:='TDisplay';
  end else if Type_='VisualID' then begin
   result:='TVisualID';
  end else if Type_='Window' then begin
   result:='TWindow';
  end else if Type_='xcb_connection_t' then begin
   result:='Txcb_connection';
  end else if Type_='xcb_visualid_t' then begin
   result:='Txcb_visualid';
  end else if Type_='xcb_window_t' then begin
   result:='Txcb_window';
  end else if Type_='wl_display' then begin
   result:='Twl_display';
  end else if Type_='wl_surface' then begin
   result:='Twl_surface';
  end else if Type_='MirConnection' then begin
   result:='TMirConnection';
  end else if Type_='MirSurface' then begin
   result:='TMirSurface';
  end else if Type_='ANativeWindow' then begin
   result:='TANativeWindow';
  end else begin
   result:='T'+Type_;
  end;
 end;
end;

procedure ParseCommentTag(Tag:TXMLTag);
begin
 Comment:=ParseText(Tag);
end;

procedure ParseExtensionsTag(Tag:TXMLTag);
var i,j:longint;
    ChildItem,ChildChildItem,ChildChildChildItem:TXMLItem;
    ChildTag,ChildChildTag,ChildChildChildTag:TXMLTag;
    Extension:TExtension;
    ExtensionEnum:TExtensionEnum;
    ExtensionType:TExtensionType;
    ExtensionCommand:TExtensionCommand;
begin
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='extension' then begin
    Extension:=TExtension.Create;
    Extensions.Add(Extension);
    Extension.Name:=ChildTag.GetParameter('name','');
    Extension.Number:=StrToIntDef(ChildTag.GetParameter('number','0'),0);
    Extension.Protect:=ChildTag.GetParameter('protect','');
    Extension.Author:=ChildTag.GetParameter('author','');
    Extension.Contact:=ChildTag.GetParameter('contact','');
    Extension.Supported:=ChildTag.GetParameter('supported','');
    ChildChildTag:=ChildTag.FindTag('require');
    if assigned(ChildChildTag) then begin
     for j:=0 to ChildChildTag.Items.Count-1 do begin
      ChildChildChildItem:=ChildChildTag.Items[j];
      if ChildChildChildItem is TXMLTag then begin
       ChildChildChildTag:=TXMLTag(ChildChildChildItem);
       if ChildChildChildTag.Name='enum' then begin
        ExtensionEnum:=TExtensionEnum.Create;
        Extension.Enums.Add(ExtensionEnum);
        ExtensionEnum.Extension:=Extension;
        ExtensionEnum.Name:=ChildChildChildTag.GetParameter('name','');
        ExtensionEnum.Value:=ChildChildChildTag.GetParameter('value','');
        ExtensionEnum.Offset:=StrToIntDef(ChildChildChildTag.GetParameter('offset','-1'),-1);
        ExtensionEnum.Dir:=ChildChildChildTag.GetParameter('dir','');
        ExtensionEnum.Extends:=ChildChildChildTag.GetParameter('extends','');
        ExtensionEnums.AddObject(ExtensionEnum.Name,ExtensionEnum);
       end else if ChildChildChildTag.Name='type' then begin
        ExtensionType:=TExtensionType.Create;
        Extension.Types.Add(ExtensionType);
        ExtensionType.Name:=ChildChildChildTag.GetParameter('name','');
        ExtensionTypes.AddObject(ExtensionType.Name,ExtensionType);
       end else if ChildChildChildTag.Name='command' then begin
        ExtensionCommand:=TExtensionCommand.Create;
        Extension.Commands.Add(ExtensionCommand);
        ExtensionCommand.Name:=ChildChildChildTag.GetParameter('name','');
        ExtensionCommands.AddObject(ExtensionCommand.Name,ExtensionCommand);
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 for i:=0 to ExtensionEnums.Count-1 do begin
  ExtensionEnum:=TExtensionEnum(ExtensionEnums.Objects[i]);
  Extension:=ExtensionEnum.Extension;
  if length(ExtensionEnum.Value)<>0 then begin
   if pos('"',ExtensionEnum.Value)=0 then begin
    ENumConstants.Add('      '+ExtensionEnum.Name+'='+ExtensionEnum.Value+';');
   end else begin
    ENumConstants.Add('      '+ExtensionEnum.Name+'='+StringReplace(ExtensionEnum.Value,'"','''',[rfReplaceAll])+';');
   end;
  end else if ExtensionEnum.Offset>=0 then begin
   ENumConstants.Add('      '+ExtensionEnum.Name+'='+ExtensionEnum.Dir+IntToStr(1000000000+((Extension.Number-1)*1000)+ExtensionEnum.Offset)+';');
  end;
 end;
end;

procedure ParseVendorIDsTag(Tag:TXMLTag);
var i:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
    VendorID:TVendorID;
begin
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='vendorid' then begin
    VendorID:=TVendorID.Create;
    VendorIDList.Add(VendorID);
    VendorID.Name:=ChildTag.GetParameter('name','');
    VendorID.ID:=StrToIntDef(StringReplace(ChildTag.GetParameter('id','0'),'0x','$',[rfReplaceAll]),0);
    VendorID.Comment:=ChildTag.GetParameter('comment','');
   end;
  end;
 end;
 writeln(VendorIDList.Count,' vendor IDs found ! ');
end;

procedure ParseTagsTag(Tag:TXMLTag);
var i:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
    ATag:TTag;
begin
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='tag' then begin
    ATag:=TTag.Create;
    TagList.Add(ATag);
    ATag.Name:=ChildTag.GetParameter('name','');
    ATag.Author:=ChildTag.GetParameter('author','');
    ATag.Contact:=ChildTag.GetParameter('contact','');
   end;
  end;
 end;
 writeln(TagList.Count,' tags found ! ');
end;

procedure ParseTypesTag(Tag:TXMLTag);
type PTypeDefinitionKind=^TTypeDefinitionKind;
     TTypeDefinitionKind=(tdkUNKNOWN,tdkSTRUCT,tdkUNION,tdkFUNCPOINTER);
     PTypeDefinitionMember=^TTypeDefinitionMember;
     TTypeDefinitionMember=record
      Name:ansistring;
      ArraySize:longint;
      Type_:ansistring;
      Enum:ansistring;
      Comment:ansistring;
      TypeDefinitionIndex:longint;
      Ptr:longbool;
      Constant:longbool;
     end;
     TTypeDefinitionMembers=array of TTypeDefinitionMember;
     PTypeDefinition=^TTypeDefinition;
     TTypeDefinition=record
      Kind:TTypeDefinitionKind;
      Name:ansistring;
      Comment:ansistring;
      Members:TTypeDefinitionMembers;
      CountMembers:longint;
      Define:ansistring;
      Type_:ansistring;
      Ptr:longbool;
     end;
     TTypeDefinitions=array of TTypeDefinition;
     TPTypeDefinitions=array of PTypeDefinition;
var i,j,k,ArraySize,CountTypeDefinitions:longint;
    ChildItem,ChildChildItem:TXMLItem;
    ChildTag,ChildChildTag:TXMLTag;
    Category,Type_,Name,Text,NextText:ansistring;
    TypeDefinitions:TTypeDefinitions;
    SortedTypeDefinitions:TPTypeDefinitions;
    TypeDefinition:PTypeDefinition;
    TypeDefinitionMember:PTypeDefinitionMember;
    TypeDefinitionList:TStringList;
    Ptr,Constant:boolean;
 procedure ResolveTypeDefinitionDependencies;
  function HaveDependencyOnTypeDefinition(const TypeDefinition,OtherTypeDefinition:PTypeDefinition):boolean;
  var i:longint;
  begin
   result:=false;
   for i:=0 to TypeDefinition^.CountMembers-1 do begin
    if (TypeDefinition^.Members[i].Type_=OtherTypeDefinition^.Name) and not TypeDefinition^.Members[i].Ptr then begin
     result:=true;
     break;
    end;
   end;
  end;
  function HaveCircularDependencyWithTypeDefinition(const TypeDefinition,OtherTypeDefinition:PTypeDefinition):boolean;
  var VisitedList,StackList:TList;
      Index,TypeDefinitionIndex:longint;
      CurrentTypeDefinition,RequiredTypeDefinition:PTypeDefinition;
  begin
   result:=false;
   if assigned(OtherTypeDefinition) then begin
    VisitedList:=TList.Create;
    try
     StackList:=TList.Create;
     try
      StackList.Add(OtherTypeDefinition);
      while (StackList.Count>0) and not result do begin
       CurrentTypeDefinition:=StackList.Items[StackList.Count-1];
       StackList.Delete(StackList.Count-1);
       VisitedList.Add(CurrentTypeDefinition);
       for Index:=0 to CurrentTypeDefinition.CountMembers-1 do begin
        RequiredTypeDefinition:=nil;
        if not CurrentTypeDefinition^.Members[Index].Ptr then begin
         TypeDefinitionIndex:=TypeDefinitionList.IndexOf(CurrentTypeDefinition^.Members[Index].Type_);
         if TypeDefinitionIndex>=0 then begin
          RequiredTypeDefinition:=pointer(TypeDefinitionList.Objects[TypeDefinitionIndex]);
         end;
        end;
        if assigned(RequiredTypeDefinition) then begin
         if RequiredTypeDefinition=TypeDefinition then begin
          result:=true;
          break;
         end else if VisitedList.IndexOf(RequiredTypeDefinition)<0 then begin
          StackList.Add(RequiredTypeDefinition);
         end;
        end;
       end;
      end;
     finally
      StackList.Free;
     end;
    finally
     VisitedList.Free;
    end;
   end;
  end;
 var Index,OtherIndex,TypeDefinitionIndex:longint;
     Done,Stop:boolean;
     TypeDefinition,OtherTypeDefinition:PTypeDefinition;
 begin
  // Resolve dependencies with "stable" topological sorting a la naive bubble sort with a bad
  // execution time (but that fact does not matter at so few SortedTypeDefinitions), and not with Kahn's or
  // Tarjan's algorithms, because the result must be in a stable sort order
  repeat
   Done:=true;
   for Index:=0 to CountTypeDefinitions-1 do begin
    TypeDefinition:=SortedTypeDefinitions[Index];
    for OtherIndex:=0 to Index-1 do begin
     OtherTypeDefinition:=SortedTypeDefinitions[OtherIndex];
     if HaveDependencyOnTypeDefinition(OtherTypeDefinition,TypeDefinition) then begin
      if HaveCircularDependencyWithTypeDefinition(OtherTypeDefinition,TypeDefinition) then begin
       raise Exception.Create(TypeDefinition^.Name+' have circular dependency with '+OtherTypeDefinition^.Name);
      end else begin
       SortedTypeDefinitions[OtherIndex]:=TypeDefinition;
       SortedTypeDefinitions[Index]:=OtherTypeDefinition;
       Done:=false;
       break;
      end;
     end;
    end;
    if not Done then begin
     break;
    end;
   end;
  until Done;
 end;
begin
 TypeDefinitions:=nil;
 CountTypeDefinitions:=0;
 SortedTypeDefinitions:=nil;
 TypeDefinitionList:=TStringList.Create;
 try
  SetLength(TypeDefinitions,Tag.Items.Count);
  for i:=0 to Tag.Items.Count-1 do begin
   ChildItem:=Tag.Items[i];
   if ChildItem is TXMLTag then begin
    ChildTag:=TXMLTag(ChildItem);
    if ChildTag.Name='type' then begin
     Category:=ChildTag.GetParameter('category');
     if Category='include' then begin
     end else if Category='define' then begin
      Name:=ParseText(ChildTag.FindTag('name'));
      if Name='VK_API_VERSION' then begin
       Text:=ParseText(ChildTag);
       if length(Text)>0 then begin
        j:=pos('(',Text);
        if j>0 then begin
         delete(Text,1,j);
         j:=pos(')',Text);
         if j>0 then begin
          Text:=copy(Text,1,j-1);
          j:=pos(',',Text);
          if j>0 then begin
           VersionMajor:=StrToIntDef(trim(copy(Text,1,j-1)),0);
           delete(Text,1,j);
           Text:=trim(Text);
           j:=pos(',',Text);
           if j>0 then begin
            VersionMinor:=StrToIntDef(trim(copy(Text,1,j-1)),0);
            delete(Text,1,j);
            Text:=trim(Text);
            VersionPatch:=StrToIntDef(Text,0);
           end;
          end;
         end;
        end;
       end;
      end;
     end else if Category='basetype' then begin
      Type_:=ParseText(ChildTag.FindTag('type'));
      Name:=ParseText(ChildTag.FindTag('name'));
      BaseTypes.Add('     P'+Name+'=^T'+Name+';');
      BaseTypes.Add('     T'+Name+'='+TranslateType(Type_,false)+';');
     end else if Category='bitmask' then begin
      Type_:=ParseText(ChildTag.FindTag('type'));
      Name:=ParseText(ChildTag.FindTag('name'));
      BitMaskTypes.Add('     P'+Name+'=^T'+Name+';');
      BitMaskTypes.Add('     T'+Name+'='+TranslateType(Type_,false)+';');
     end else if Category='handle' then begin
      Type_:=ParseText(ChildTag.FindTag('type'));
      Name:=ParseText(ChildTag.FindTag('name'));
      HandleTypes.Add('     P'+Name+'=^T'+Name+';');
      HandleTypes.Add('     T'+Name+'='+TranslateType(Type_,false)+';');
     end else if Category='enum' then begin
      Name:=ChildTag.GetParameter('name');
     end else if Category='funcpointer' then begin
      TypeDefinition:=@TypeDefinitions[CountTypeDefinitions];
      inc(CountTypeDefinitions);
      TypeDefinition^.Kind:=tdkFUNCPOINTER;
      TypeDefinition^.Name:='';
      TypeDefinition^.Comment:=ChildTag.GetParameter('comment');
      TypeDefinition^.Members:=nil;
      TypeDefinition^.Define:='';
      TypeDefinition^.Type_:='';
      TypeDefinition^.Ptr:=false;
      Name:='';
      Type_:='';
      Text:='';
      Ptr:=false;
      Constant:=false;
      for j:=0 to ChildTag.Items.Count-1 do begin
       ChildChildItem:=ChildTag.Items[j];
       if ChildChildItem is TXMLTag then begin
        ChildChildTag:=TXMLTag(ChildChildItem);
        if ChildChildTag.Name='name' then begin
         TypeDefinition^.Name:=ParseText(ChildChildTag);
         if pos('void*',Text)>0 then begin
          TypeDefinition^.Ptr:=true;
          TypeDefinition^.Type_:='void';
         end else begin
          Text:=trim(StringReplace(Text,'typedef','',[rfReplaceAll]));
          Text:=trim(StringReplace(Text,'VKAPI_PTR','',[rfReplaceAll]));
          Text:=trim(StringReplace(Text,'(','',[rfReplaceAll]));
          Text:=trim(StringReplace(Text,'*','',[rfReplaceAll]));
          TypeDefinition^.Type_:=Text;
         end;
        end else if ChildChildTag.Name='type' then begin
         Type_:=ParseText(ChildChildTag);
        end;
       end else if ChildChildItem is TXMLText then begin
        Text:=TXMLText(ChildChildItem).Text;
        if length(TypeDefinition^.Name)>0 then begin
         while length(Text)>0 do begin
          NextText:='';
          if trim(Text)='const' then begin
           Constant:=true;
          end else if length(Type_)>0 then begin
           if length(TypeDefinition^.Members)<TypeDefinition^.CountMembers+1 then begin
            SetLength(TypeDefinition^.Members,(TypeDefinition^.CountMembers+1)*2);
           end;
           TypeDefinitionMember:=@TypeDefinition^.Members[TypeDefinition^.CountMembers];
           inc(TypeDefinition^.CountMembers);
           TypeDefinitionMember^.Type_:=Type_;
           k:=pos(',',Text);
           if k>0 then begin
            NextText:=trim(copy(Text,k+1,(length(Text)-k)+1));
            Text:=trim(copy(Text,1,k-1));
           end;
           Text:=trim(StringReplace(Text,');','',[rfReplaceAll]));
           if pos('*',Text)>0 then begin
            TypeDefinitionMember^.Ptr:=true;
            Text:=trim(StringReplace(Text,'*','',[rfReplaceAll]));
           end else begin
            TypeDefinitionMember^.Ptr:=false;
           end;
           TypeDefinitionMember^.Constant:=Constant;
           Constant:=false;
           if Text='object' then begin
            Text:='object_';
           end;
           TypeDefinitionMember^.Name:=Text;
           TypeDefinitionMember^.ArraySize:=0;
           TypeDefinitionMember^.Enum:='';
           TypeDefinitionMember^.Comment:='';
           Type_:='';
          end;
          Text:=NextText;
         end;
        end;
       end;
      end;
      SetLength(TypeDefinition^.Members,TypeDefinition^.CountMembers);
     end else if (Category='struct') or (Category='union') then begin
      Name:=ChildTag.GetParameter('name');
      TypeDefinition:=@TypeDefinitions[CountTypeDefinitions];
      inc(CountTypeDefinitions);
      if Category='union' then begin
       TypeDefinition^.Kind:=tdkUNION;
      end else begin
       TypeDefinition^.Kind:=tdkSTRUCT;
      end;
      TypeDefinition^.Name:=Name;
      TypeDefinition^.Comment:=ChildTag.GetParameter('comment');
      TypeDefinition^.Members:=nil;
      TypeDefinition^.Define:='';
      SetLength(TypeDefinition^.Members,ChildTag.Items.Count);
      TypeDefinition^.CountMembers:=0;
      for j:=0 to ChildTag.Items.Count-1 do begin
       ChildChildItem:=ChildTag.Items[j];
       if ChildChildItem is TXMLTag then begin
        ChildChildTag:=TXMLTag(ChildChildItem);
        if ChildChildTag.Name='member' then begin
         Name:=ParseText(ChildChildTag.FindTag('name'));
         k:=pos('[',Name);
         if k>0 then begin
          ArraySize:=StrToIntDef(copy(Name,k+1,length(Name)-(k+1)),1);
          Name:=copy(Name,1,k-1);
         end else begin
          ArraySize:=-1;
         end;
         if Name='type' then begin
          Name:='type_';
         end else if Name='hinstance' then begin
          Name:='hinstance_';
         end else if Name='hwnd' then begin
          Name:='hwnd_';
         end;
         TypeDefinitionMember:=@TypeDefinition^.Members[TypeDefinition^.CountMembers];
         inc(TypeDefinition^.CountMembers);
         TypeDefinitionMember^.Name:=Name;
         TypeDefinitionMember^.ArraySize:=ArraySize;
         Type_:=ParseText(ChildChildTag.FindTag('type'));
         TypeDefinitionMember^.Type_:=Type_;
         TypeDefinitionMember^.Enum:=ParseText(ChildChildTag.FindTag('enum'));
         TypeDefinitionMember^.Comment:=ChildChildTag.GetParameter('comment');
         TypeDefinitionMember^.TypeDefinitionIndex:=-1;
         TypeDefinitionMember^.Ptr:=pos('*',ParseText(ChildChildTag))>0;
         if (Type_='HWND') or (Type_='HINSTANCE') then begin
          TypeDefinition^.Define:='Windows';
         end else if (Type_='Display') or (Type_='VisualID') or (Type_='Window') then begin
          TypeDefinition^.Define:='X11';
         end else if (Type_='xcb_connection_t') or (Type_='xcb_visualid_t') or (Type_='xcb_window_t') then begin
          TypeDefinition^.Define:='XCB';
         end else if (Type_='wl_display') or (Type_='wl_surface') then begin
          TypeDefinition^.Define:='Wayland';
         end else if (Type_='MirConnection') or (Type_='MirSurface') then begin
          TypeDefinition^.Define:='Mir';
         end else if Type_='ANativeWindow' then begin
          TypeDefinition^.Define:='Android';
         end;
        end;
       end;
      end;
      SetLength(TypeDefinition^.Members,TypeDefinition^.CountMembers);
     end else begin
     end;
    end;
   end;
  end;
  SetLength(TypeDefinitions,CountTypeDefinitions);
  SetLength(SortedTypeDefinitions,CountTypeDefinitions);
  for i:=0 to CountTypeDefinitions-1 do begin
   SortedTypeDefinitions[i]:=@TypeDefinitions[i];
   TypeDefinitionList.AddObject(TypeDefinitions[i].Name,pointer(SortedTypeDefinitions[i]));
  end;
  ResolveTypeDefinitionDependencies;
  for i:=0 to CountTypeDefinitions-1 do begin
   TypeDefinition:=SortedTypeDefinitions[i];
   if length(TypeDefinition^.Define)>0 then begin
    TypeDefinitionTypes.Add('{$ifdef '+TypeDefinition^.Define+'}');
   end;
   TypeDefinitionTypes.Add('     P'+TypeDefinition^.Name+'=^T'+TypeDefinition^.Name+';');
   if length(TypeDefinition^.Define)>0 then begin
    TypeDefinitionTypes.Add('{$endif}');
   end;
  end;
  for i:=0 to CountTypeDefinitions-1 do begin
   TypeDefinition:=SortedTypeDefinitions[i];
   if length(TypeDefinition^.Define)>0 then begin
    TypeDefinitionTypes.Add('{$ifdef '+TypeDefinition^.Define+'}');
   end;
   case TypeDefinition^.Kind of
    tdkSTRUCT:begin
     TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=record');
     for j:=0 to TypeDefinition^.CountMembers-1 do begin
      if length(TypeDefinition^.Members[j].Enum)>0 then begin
       TypeDefinitionTypes.Add('      '+TypeDefinition^.Members[j].Name+':array[0..'+TypeDefinition^.Members[j].Enum+'-1] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';');
      end else if TypeDefinition^.Members[j].ArraySize>=0 then begin
       TypeDefinitionTypes.Add('      '+TypeDefinition^.Members[j].Name+':array[0..'+IntToStr(TypeDefinition^.Members[j].ArraySize-1)+'] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';');
      end else begin
       TypeDefinitionTypes.Add('      '+TypeDefinition^.Members[j].Name+':'+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';');
      end;
     end;
     TypeDefinitionTypes.Add('     end;');
    end;
    tdkUNION:begin
     TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=record');
     TypeDefinitionTypes.Add('      case longint of');
     for j:=0 to TypeDefinition^.CountMembers-1 do begin
      TypeDefinitionTypes.Add('       '+IntToStr(j)+':(');
      if length(TypeDefinition^.Members[j].Enum)>0 then begin
       TypeDefinitionTypes.Add('        '+TypeDefinition^.Members[j].Name+':array[0..'+TypeDefinition^.Members[j].Enum+'-1] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';');
      end else if TypeDefinition^.Members[j].ArraySize>=0 then begin
       TypeDefinitionTypes.Add('        '+TypeDefinition^.Members[j].Name+':array[0..'+IntToStr(TypeDefinition^.Members[j].ArraySize-1)+'] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';');
      end else begin
       TypeDefinitionTypes.Add('        '+TypeDefinition^.Members[j].Name+':'+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';');
      end;
      TypeDefinitionTypes.Add('       );');
     end;
     TypeDefinitionTypes.Add('     end;');
    end;
    tdkFUNCPOINTER:begin
     Text:='';
     for j:=0 to TypeDefinition^.CountMembers-1 do begin
      TypeDefinitionMember:=@TypeDefinition^.Members[j];
      if j>0 then begin
       Text:=Text+';';
      end;
      if TypeDefinitionMember^.Constant then begin
       Text:=Text+'const ';
      end;
      Text:=Text+TypeDefinitionMember^.Name+':'+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr);
     end;
     if (TypeDefinition^.Type_='void') and not TypeDefinition^.Ptr then begin
      TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=procedure('+Text+'); '+CallingConventions);
     end else begin
      TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=function('+Text+'):'+TranslateType(TypeDefinition^.Type_,TypeDefinition^.Ptr)+'; '+CallingConventions);
     end;
    end;
   end;
   if length(TypeDefinition^.Define)>0 then begin
    TypeDefinitionTypes.Add('{$endif}');
   end;
  end;
 finally
  SetLength(TypeDefinitions,0);
  SetLength(SortedTypeDefinitions,0);
  TypeDefinitionList.Free;
 end;
end;

procedure ParseEnumsTag(Tag:TXMLTag);
type PValueItem=^TValueItem;
     TValueItem=record
      Name:ansistring;
      ValueStr:ansistring;
      ValueInt64:int64;
      Comment:ansistring;
     end;
     TValueItems=array of TValueItem;
var i,j,lv,hv,CountValueItems:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
    Type_,Name,NameEnum,Line,Comment,Expand,Value,LowValue,HighValue:ansistring;
    Values:TStringList;
    v:int64;
    ValueItems:TValueItems;
    ValueItem:PValueItem;
    TempValueItem:TValueItem;
begin
 ValueItems:=nil;
 try
  Name:=Tag.GetParameter('name','');
  Type_:=Tag.GetParameter('type','');
  Expand:=Tag.GetParameter('expand','');
  LowValue:='unknown';
  HighValue:='unknown';
  Values:=TStringList.Create;
  try
   j:=-1;
   lv:=$7fffffffffffffff;
   hv:=-$7fffffffffffffff;
   SetLength(ValueItems,Tag.Items.Count);
   CountValueItems:=0;
   for i:=0 to Tag.Items.Count-1 do begin
    ChildItem:=Tag.Items[i];
    if ChildItem is TXMLTag then begin
     ChildTag:=TXMLTag(ChildItem);
     if (ChildTag.Name='enum') or (ChildTag.Name='unused') then begin
      ValueItem:=@ValueItems[CountValueItems];
      inc(CountValueItems);
      if ChildTag.Name='unused' then begin
       ValueItem^.Name:=Expand+'_UNUSED_START';
      end else begin
       ValueItem^.Name:=ChildTag.GetParameter('name');
      end;
      if ChildTag.Name='unused' then begin
       ValueItem^.ValueStr:=IntToStr(StrToIntDef(ChildTag.GetParameter('start','0'),0));
      end else if Type_='bitmask' then begin
       ValueItem^.ValueStr:='$'+IntToHex(longword(1) shl StrToIntDef(ChildTag.GetParameter('bitpos','0'),0),8);
      end else if Type_='enum' then begin
       ValueItem^.ValueStr:=IntToStr(StrToIntDef(ChildTag.GetParameter('value','0'),0));
      end else begin
       Value:=ChildTag.GetParameter('value','0');
       if Value='(~0U)' then begin
        ValueItem^.ValueStr:='TVkUInt32($ffffffff)';
       end else if Value='(~0ULL)' then begin
        ValueItem^.ValueStr:='TVkUInt64($ffffffffffffffff)';
       end else if (pos('.',Value)>0) or ((pos('f',Value)=length(Value)) and (pos('x',Value)=0)) then begin
        ValueItem^.ValueStr:=StringReplace(Value,'f','',[]);
       end else begin
        ValueItem^.ValueStr:=IntToStr(StrToIntDef(ChildTag.GetParameter('value','0'),0));
       end;
      end;
      ValueItem^.ValueInt64:=StrToIntDef(ValueItem^.ValueStr,0);
      ValueItem^.Comment:=ChildTag.GetParameter('comment','');
     end;
    end;
   end;
   SetLength(ValueItems,CountValueItems);
   if length(Expand)>0 then begin
    v:=$7fffffffffffffff;
    lv:=0;
    for i:=0 to CountValueItems-1 do begin
     ValueItem:=@ValueItems[i];
     if ValueItem^.ValueInt64<v then begin
      v:=ValueItem^.ValueInt64;
      lv:=i;
     end;
    end;
    v:=-$7fffffffffffffff;
    hv:=0;
    for i:=0 to CountValueItems-1 do begin
     ValueItem:=@ValueItems[i];
     if ValueItem^.ValueInt64>v then begin
      v:=ValueItem^.ValueInt64;
      hv:=i;
     end;
    end;
    SetLength(ValueItems,CountValueItems+4);
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_BEGIN_RANGE';
     ValueItem^.ValueStr:=ValueItems[lv].ValueStr;
     ValueItem^.ValueInt64:=ValueItems[lv].ValueInt64;
     ValueItem^.Comment:=ValueItems[lv].Name;
    end;
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_END_RANGE';
     ValueItem^.ValueStr:=ValueItems[hv].ValueStr;
     ValueItem^.ValueInt64:=ValueItems[hv].ValueInt64;
     ValueItem^.Comment:=ValueItems[hv].Name;
    end;
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_RANGE_SIZE';
     ValueItem^.ValueStr:=IntToStr((ValueItems[hv].ValueInt64-ValueItems[lv].ValueInt64)+1);
     ValueItem^.ValueInt64:=(ValueItems[hv].ValueInt64-ValueItems[lv].ValueInt64)+1;
     ValueItem^.Comment:='('+ValueItems[hv].Name+'-'+ValueItems[lv].Name+')+1';
    end;
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_MAX_ENUM';
     ValueItem^.ValueStr:='$7fffffff';
     ValueItem^.ValueInt64:=$7fffffff;
     ValueItem^.Comment:='';
    end;
   end;
   if (Type_='enum') or (Type_='bitmask') then begin
    ENumTypes.Add('     P'+Name+'=^T'+Name+';');
    ENumTypes.Add('     T'+Name+'=TVkEnum;');
   end;
   for i:=0 to CountValueItems-1 do begin
    ValueItem:=@ValueItems[i];
    if length(ValueItem^.Comment)>0 then begin
     ENumConstants.Add('      '+ValueItem^.Name+'='+ValueItem^.ValueStr+'; // '+ValueItem^.Comment);
    end else begin
     ENumConstants.Add('      '+ValueItem^.Name+'='+ValueItem^.ValueStr+';');
    end;
   end;
{  if Type_='bitmask' then begin
    ENumTypes.Add('     P'+Name+'=^T'+Name+';');
    ENumTypes.Add('     T'+Name+'=');
    ENumTypes.Add('      (');
    i:=0;
    while i<(CountValueItems-1) do begin
     if ValueItems[i].ValueInt64>ValueItems[i+1].ValueInt64 then begin
      TempValueItem:=ValueItems[i];
      ValueItems[i]:=ValueItems[i+1];
      ValueItems[i+1]:=TempValueItem;
      if i>0 then begin
       dec(i);
      end else begin
       inc(i);
      end;
     end else begin
      inc(i);
     end;
    end;
    for i:=0 to CountValueItems-1 do begin
     ValueItem:=@ValueItems[i];
     if length(ValueItem^.Comment)>0 then begin
      if (i+1)<CountValueItems then begin
       ENumTypes.Add('       '+ValueItem^.Name+'='+ValueItem^.ValueStr+', // '+ValueItem^.Comment);
      end else begin
       ENumTypes.Add('       '+ValueItem^.Name+'='+ValueItem^.ValueStr+' // '+ValueItem^.Comment);
      end;
     end else begin
      if (i+1)<CountValueItems then begin
       ENumTypes.Add('       '+ValueItem^.Name+'='+ValueItem^.ValueStr+',');
      end else begin
       ENumTypes.Add('       '+ValueItem^.Name+'='+ValueItem^.ValueStr);
      end;
     end;
    end;
    ENumTypes.Add('      );');
   end;}
  finally
   Values.Free;
  end;
 finally
  SetLength(ValueItems,0);
 end;
end;

procedure ParseCommandsTag(Tag:TXMLTag);
var i,j,k,ArraySize,CountTypeDefinitions:longint;
    ChildItem,ChildChildItem:TXMLItem;
    ChildTag,ChildChildTag:TXMLTag;
    ProtoName,ProtoType,ParamName,ParamType,Text,Line,Define:ansistring;
    ProtoPtr,ParamPtr:boolean;
begin
 AllCommandType.Add('     PVulkan=^TVulkan;');
 AllCommandType.Add('     TVulkan=record');
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='command' then begin
    ProtoName:='';
    ProtoType:='';
    ProtoPtr:=false;
    ParamName:='';
    ParamType:='';
    ParamPtr:=false;
    Line:='';
    Define:='';
    for j:=0 to ChildTag.Items.Count-1 do begin
     ChildChildItem:=ChildTag.Items[j];
     if ChildChildItem is TXMLTag then begin
      ChildChildTag:=TXMLTag(ChildChildItem);
      if ChildChildTag.Name='proto' then begin
       ProtoName:=ParseText(ChildChildTag.FindTag('name'));
       ProtoType:=ParseText(ChildChildTag.FindTag('type'));
       ProtoPtr:=Pos('*',ParseText(ChildChildTag))>0;
      end else if ChildChildTag.Name='param' then begin
       ParamName:=ParseText(ChildChildTag.FindTag('name'));
       ParamType:=ParseText(ChildChildTag.FindTag('type'));
       Text:=ParseText(ChildChildTag);
       ParamPtr:=Pos('*',Text)>0;
       if length(Line)>0 then begin
        Line:=Line+';'
       end;
       if ParamName='type' then begin
        ParamName:='type_';
       end;
       if ParamName='object' then begin
        ParamName:='object_';
       end;
       if pos('const ',trim(Text))=1 then begin
        Line:=Line+'const ';
       end;
       Line:=Line+ParamName+':'+TranslateType(ParamType,ParamPtr);
       if (ParamType='HWND') or (ParamType='HINSTANCE') then begin
        Define:='Windows';
       end else if (ParamType='Display') or (ParamType='VisualID') or (ParamType='Window') or (pos('Xlib',ParamType)>0) then begin
        Define:='X11';
       end else if (ParamType='xcb_connection_t') or (ParamType='xcb_visualid_t') or (ParamType='xcb_window_t') or (pos('Xcb',ParamType)>0) then begin
        Define:='XCB';
       end else if (ParamType='wl_display') or (ParamType='wl_surface') or (pos('Wayland',ParamType)>0) then begin
        Define:='Wayland';
       end else if (ParamType='MirConnection') or (ParamType='MirSurface') or (pos('Mir',ParamType)>0) then begin
        Define:='Mir';
       end else if (ParamType='ANativeWindow') or (pos('Android',ParamType)>0) then begin
        Define:='Android';
       end;
      end;
     end;
    end;
    if length(Define)>0 then begin
     CommandTypes.Add('{$ifdef '+Define+'}');
     CommandVariables.Add('{$ifdef '+Define+'}');
     AllCommandType.Add('{$ifdef '+Define+'}');
    end;
    if (ProtoType='void') and not ProtoPtr then begin
     CommandTypes.Add('     T'+ProtoName+'=procedure('+Line+'); '+CallingConventions);
    end else begin
     CommandTypes.Add('     T'+ProtoName+'=function('+Line+'):'+TranslateType(ProtoType,ProtoPtr)+'; '+CallingConventions);
    end;
    CommandVariables.Add('    '+ProtoName+':T'+ProtoName+'=nil;');
    AllCommandType.Add('      '+ProtoName+':T'+ProtoName+';');
    if length(Define)>0 then begin
     CommandTypes.Add('{$endif}');
     CommandVariables.Add('{$endif}');
     AllCommandType.Add('{$endif}');
    end;
   end;
  end;
 end;
 AllCommandType.Add('     end;');
end;

procedure ParseRegistryTag(Tag:TXMLTag);
var i:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
begin

 write('Searching for registry/comment tag . . . ');
 ChildTag:=Tag.FindTag('comment');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseCommentTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/extensions tag . . . ');
 ChildTag:=Tag.FindTag('extensions');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseExtensionsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/vendorids tag . . . ');
 ChildTag:=Tag.FindTag('vendorids');
 if assigned(ChildTag) then begin
  ParseVendorIDsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/tags tag . . . ');
 ChildTag:=Tag.FindTag('tags');
 if assigned(ChildTag) then begin
  ParseTagsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/types tag . . . ');
 ChildTag:=Tag.FindTag('types');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseTypesTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='enums' then begin
    ParseEnumsTag(ChildTag);
   end;
  end;
 end;

 write('Searching for registry/commands tag . . . ');
 ChildTag:=Tag.FindTag('commands');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseCommandsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

end;

var VKXMLFileStream:TMemoryStream;
    VKXML:TXML;
    RegistryTag:TXMLTag;
    OutputPAS:TStringList;

var i:longint;
    ExtensionEnum:TExtensionEnum;
begin
 Comment:='';
 VendorIDList:=TObjectList.Create(true);
 TagList:=TObjectList.Create(true);
 Extensions:=TObjectList.Create(true);
 ExtensionEnums:=TStringList.Create;
 ExtensionTypes:=TStringList.Create;
 ExtensionCommands:=TStringList.Create;
 BaseTypes:=TStringList.Create;
 BitMaskTypes:=TStringList.Create;
 HandleTypes:=TStringList.Create;
 ENumTypes:=TStringList.Create;
 ENumConstants:=TStringList.Create;
 TypeDefinitionTypes:=TStringList.Create;
 CommandTypes:=TStringList.Create;
 CommandVariables:=TStringList.Create;
 AllCommandType:=TStringList.Create;

 VersionMajor:=1;
 VersionMinor:=0;
 VersionPatch:=0;

 InitializeEntites;
 try

  VKXMLFileStream:=TMemoryStream.Create;
  try

   write('Loading "vk.xml" . . . ');

   try
    VKXMLFileStream.LoadFromFile('vk.xml');
   except
    writeln('Error!');
    raise;
   end;

   if VKXMLFileStream.Seek(0,soBeginning)=0 then begin

    writeln('OK!');

    VKXML:=TXML.Create;
    try

     write('Parsing "vk.xml" . . . ');
     if VKXML.Parse(VKXMLFileStream) then begin

      writeln('OK!');

      write('Searching for registry tag . . . ');
      RegistryTag:=VKXML.Root.FindTag('registry');
      if assigned(RegistryTag) then begin
       writeln('found!');
       ParseRegistryTag(RegistryTag);
      end else begin
       writeln('not found!');
      end;

     end else begin
      writeln('Error!');
     end;

    finally
     VKXML.Free;
    end;

   end else begin
    writeln('Error!');
   end;

  finally
   VKXMLFileStream.Free;
  end;

  write('Generating "vulkan.pas" . . . ');
  OutputPAS:=TStringList.Create;
  try
   OutputPAS.Add('(*');
   OutputPAS.Add('** Copyright (c) 2015-2016 The Khronos Group Inc.');
   OutputPAS.Add('** Copyright (c) 2016, Benjamin Rosseaux (benjamin@rosseaux.de, the pascal headers)');
   OutputPAS.Add('**');
   OutputPAS.Add('** Permission is hereby granted, free of charge, to any person obtaining a');
   OutputPAS.Add('** copy of this software and/or associated documentation files (the');
   OutputPAS.Add('** "Materials"), to deal in the Materials without restriction, including');
   OutputPAS.Add('** without limitation the rights to use, copy, modify, merge, publish,');
   OutputPAS.Add('** distribute, sublicense, and/or sell copies of the Materials, and to');
   OutputPAS.Add('** permit persons to whom the Materials are furnished to do so, subject to');
   OutputPAS.Add('** the following conditions:');
   OutputPAS.Add('**');
   OutputPAS.Add('** The above copyright notice and this permission notice shall be included');
   OutputPAS.Add('** in all copies or substantial portions of the Materials.');
   OutputPAS.Add('**');
   OutputPAS.Add('** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,');
   OutputPAS.Add('** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF');
   OutputPAS.Add('** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.');
   OutputPAS.Add('** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY');
   OutputPAS.Add('** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,');
   OutputPAS.Add('** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE');
   OutputPAS.Add('** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.');
   OutputPAS.Add('*)');
   OutputPAS.Add('(*');
   OutputPAS.Add('** This header is generated from the Khronos Vulkan XML API Registry.');
   OutputPAS.Add('**');
   OutputPAS.Add('*)');
   OutputPAS.Add('unit vulkan;');
   OutputPAS.Add('{$ifdef fpc}');
   OutputPAS.Add(' {$mode delphi}');
   OutputPAS.Add(' {$z4}');
   OutputPAS.Add(' {$packrecords c}');
   OutputPAS.Add(' {$define CAN_INLINE}');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' {$z4}');
   OutputPAS.Add(' {$undef CAN_INLINE}');
   OutputPAS.Add(' {$ifdef ver180}');
   OutputPAS.Add('  {$define CAN_INLINE}');
   OutputPAS.Add(' {$else}');
   OutputPAS.Add('  {$ifdef conditionalexpressions}');
   OutputPAS.Add('   {$if compilerversion>=18}');
   OutputPAS.Add('    {$define CAN_INLINE}');
   OutputPAS.Add('   {$ifend}');
   OutputPAS.Add('  {$endif}');
   OutputPAS.Add(' {$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef Win32}');
   OutputPAS.Add(' {$define Windows}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef Win64}');
   OutputPAS.Add(' {$define Windows}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef WinCE}');
   OutputPAS.Add(' {$define Windows}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add(' {$define VK_USE_PLATFORM_WIN32_KHR}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('interface');
   OutputPAS.Add('uses {$ifdef Windows}Windows,{$endif}{$ifdef Unix}BaseUnix,UnixType,dl,{$endif}{$ifdef X11}x,xlib,{$endif}{$ifdef XCB}xcb,{$endif}{$ifdef Mir}Mir,{$endif}{$ifdef Wayland}Wayland,{$endif}{$ifdef Android}Android,{$endif}SysUtils;');
   OutputPAS.Add('type PVkInt8=^TVkInt8;');
   OutputPAS.Add('     TVkInt8=shortint;');
   OutputPAS.Add('     PVkUInt8=^TVkUInt8;');
   OutputPAS.Add('     TVkUInt8=byte;');
   OutputPAS.Add('     PVkInt16=^TVkInt16;');
   OutputPAS.Add('     TVkInt16=smallint;');
   OutputPAS.Add('     PVkUInt16=^TVkUInt16;');
   OutputPAS.Add('     TVkUInt16=word;');
   OutputPAS.Add('     PVkInt32=^TVkInt32;');
   OutputPAS.Add('     TVkInt32=longint;');
   OutputPAS.Add('     PVkUInt32=^TVkUInt32;');
   OutputPAS.Add('     TVkUInt32=longword;');
   OutputPAS.Add('     PVkInt64=^TVkInt64;');
   OutputPAS.Add('     TVkInt64=int64;');
   OutputPAS.Add('     PVkUInt64=^TVkUInt64;');
   OutputPAS.Add('     TVkUInt64=uint64;');
   OutputPAS.Add('     PVkChar=^TVkChar;');
   OutputPAS.Add('     TVkChar=ansichar;');
   OutputPAS.Add('     PVkPointer=^TVkPointer;');
   OutputPAS.Add('     TVkPointer=pointer;');
   OutputPAS.Add('     PVkFloat=^TVkFloat;');
   OutputPAS.Add('     TVkFloat=single;');
   OutputPAS.Add('     PVkDouble=^TVkDouble;');
   OutputPAS.Add('     TVkDouble=double;');
   OutputPAS.Add('     PVkPtrUInt=^TVkPtrUInt;');
   OutputPAS.Add('     PVkPtrInt=^TVkPtrInt;');
   OutputPAS.Add('{$ifdef fpc}');
   OutputPAS.Add('     TVkPtrUInt=PtrUInt;');
   OutputPAS.Add('     TVkPtrInt=PtrInt;');
   OutputPAS.Add(' {$undef OldDelphi}');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' {$ifdef conditionalexpressions}');
   OutputPAS.Add('  {$if CompilerVersion>=23.0}');
   OutputPAS.Add('   {$undef OldDelphi}');
   OutputPAS.Add('     TVkPtrUInt=NativeUInt;');
   OutputPAS.Add('     TVkPtrInt=NativeInt;');
   OutputPAS.Add('  {$else}');
   OutputPAS.Add('   {$define OldDelphi}');
   OutputPAS.Add('  {$ifend}');
   OutputPAS.Add(' {$else}');
   OutputPAS.Add('  {$define OldDelphi}');
   OutputPAS.Add(' {$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef OldDelphi}');
   OutputPAS.Add('{$ifdef cpu64}');
   OutputPAS.Add('     TVkPtrUInt=uint64;');
   OutputPAS.Add('     TVkPtrInt=int64;');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('     TVkPtrUInt=longword;');
   OutputPAS.Add('     TVkPtrInt=longint;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('const VK_API_VERSION=('+IntToStr(VersionMajor)+' shl 22) or ('+IntToStr(VersionMinor)+' shl 12) or ('+IntToStr(VersionPatch)+' shl 0);');
   OutputPAS.Add('      VK_NULL_HANDLE=0;');
   OutputPAS.AddStrings(ENumConstants);
   OutputPAS.Add('type PVkDispatchableHandle=^TVkDispatchableHandle;');
   OutputPAS.Add('     TVkDispatchableHandle=TVkPtrInt;');
   OutputPAS.Add('     PVkNonDispatchableHandle=^TVkNonDispatchableHandle;');
   OutputPAS.Add('     TVkNonDispatchableHandle=TVkUInt64;');
   OutputPAS.Add('     PVkEnum=^TVkEnum;');
   OutputPAS.Add('     TVkEnum=TVkInt32;');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add('     PVkHINSTANCE=^TVkHINSTANCE;');
   OutputPAS.Add('     TVkHINSTANCE=TVkPtrUInt;');
   OutputPAS.Add('     PVkHWND=^TVkHWND;');
   OutputPAS.Add('     TVkHWND=HWND;');
   OutputPAS.Add('{$endif}');
   OutputPAS.AddStrings(BaseTypes);
   OutputPAS.AddStrings(BitMaskTypes);
   OutputPAS.AddStrings(HandleTypes);
   OutputPAS.AddStrings(EnumTypes);
   OutputPAS.AddStrings(TypeDefinitionTypes);
   OutputPAS.AddStrings(CommandTypes);
   OutputPAS.AddStrings(AllCommandType);
   OutputPAS.Add('var LibVulkan:pointer=nil;');
   OutputPAS.AddStrings(CommandVariables);
   OutputPAS.Add('function VK_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function VK_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function VK_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function VK_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function vkLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function vkFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function vkGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function LoadVulkanLibrary:boolean;');
   OutputPAS.Add('implementation');
   OutputPAS.Add('function VK_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=(VersionMajor shl 22) or (VersionMinor shl 12) or (VersionPatch shl 0);');
   OutputPAS.Add('end;');
   OutputPAS.Add('function VK_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=Version shr 22;');
   OutputPAS.Add('end;');
   OutputPAS.Add('function VK_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=(Version shr 12) and $3ff;');
   OutputPAS.Add('end;');
   OutputPAS.Add('function VK_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=(Version shr 0) and $fff;');
   OutputPAS.Add('end;');
   OutputPAS.Add('function vkLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add(' result:=pointer(LoadLibrary(PChar(LibraryName)));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('{$ifdef Linux}');
   OutputPAS.Add(' result:=dlopen(PChar(LibraryName),RTLD_NOW or RTLD_LAZY);');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' result:=nil;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('end;');
   OutputPAS.Add('function vkFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=assigned(LibraryHandle);');
   OutputPAS.Add(' if result then begin');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add('  result:=FreeLibrary(HMODULE(LibraryHandle));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('{$ifdef Unix}');
   OutputPAS.Add('  result:=dlclose(LibraryHandle)=0;');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('  result:=false;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add(' end;');
   OutputPAS.Add('end;');
   OutputPAS.Add('function vkGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add(' result:=GetProcAddress(HMODULE(LibraryHandle),PChar(ProcName));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('{$ifdef Unix}');
   OutputPAS.Add(' result:=dlsym(LibraryHandle,PChar(ProcName));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' result:=nil;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('end;');
   OutputPAS.Add('function LoadVulkanLibrary:boolean;');
   OutputPAS.Add('begin');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add(' LibVulkan:=vkLoadLibrary(''vulkan.dll'');');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('{$ifdef Unix}');
   OutputPAS.Add(' LibVulkan:=vkLoadLibrary(''libvulkan.so'');');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' LibVulkan:=nil;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add(' result:=assigned(LibVulkan);');
   OutputPAS.Add(' if result then begin');
   OutputPAS.Add('  vkEnumerateInstanceExtensionProperties:=vkGetProcAddress(LibVulkan,''vkEnumerateInstanceExtensionProperties'');');
   OutputPAS.Add('  vkEnumerateInstanceLayerProperties:=vkGetProcAddress(LibVulkan,''vkEnumerateInstanceLayerProperties'');');
   OutputPAS.Add('  vkCreateInstance:=vkGetProcAddress(LibVulkan,''vkCreateInstance'');');
   OutputPAS.Add('  vkGetInstanceProcAddr:=vkGetProcAddress(LibVulkan,''vkGetInstanceProcAddr'');');
   OutputPAS.Add('  vkGetDeviceProcAddr:=vkGetProcAddress(LibVulkan,''vkGetDeviceProcAddr'');');
   OutputPAS.Add('  result:=assigned(vkEnumerateInstanceExtensionProperties) and');
   OutputPAS.Add('          assigned(vkEnumerateInstanceLayerProperties) and');
   OutputPAS.Add('          assigned(vkCreateInstance) and');
   OutputPAS.Add('          assigned(vkGetInstanceProcAddr) and');
   OutputPAS.Add('          assigned(vkGetDeviceProcAddr);');
   OutputPAS.Add(' end;');
   OutputPAS.Add('end;');
   OutputPAS.Add('end.');
   OutputPAS.SaveToFile('vulkan.pas');
  finally
   OutputPAS.Free;
  end;
  writeln('done!');

 finally
  FinalizeEntites;
  VendorIDList.Free;
  TagList.Free;
  Extensions.Free;
  ExtensionEnums.Free;
  ExtensionTypes.Free;
  ExtensionCommands.Free;
  BitMaskTypes.Free;
  BaseTypes.Free;
  HandleTypes.Free;
  ENumTypes.Free;
  ENumConstants.Free;
  TypeDefinitionTypes.Free;
  CommandTypes.Free;
  CommandVariables.Free;
  AllCommandType.Free;
 end;

 readln;
end.


