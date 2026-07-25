(******************************************************************************
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

// Generator for PasVulkan.Steamworks.pas out of the Steamworks SDK's own machine readable API
// description file steam_api.json. Every interface method and every struct method described in that
// file is exported as a plain cdecl C symbol by the Steamworks redistributable libraries, so the
// generated bindings never need to touch a C++ vtable.
//
// Usage: steamapi2pas [steam_api.json] [PasVulkan.Steamworks.pas]

program steamapi2pas;
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
 {$define CAN_INLINE}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifdef windows}
 {$apptype console}
{$endif}
{$undef UNICODE}

uses {$ifdef Unix}cthreads,{$endif}
     SysUtils,
     Classes,
     Math,
     PasDblStrUtils in '../externals/pasdblstrutils/src/PasDblStrUtils.pas',
     PasJSON in '../externals/pasjson/src/PasJSON.pas';

type TText=TPasJSONUTF8String;

     TTexts=array of TText;

     // How a record has to be packed. The Steamworks headers use three different packing regimes,
     // and getting these wrong silently shifts every field behind the mistake.
     TPackingMode=
      (
       // Whatever the platform's C compiler does by default, so natural alignment.
       PackingModeCDefault,
       // #pragma pack(1), platform independent. Emitted as a packed record rather than as a
       // packrecords directive, so that the packing sits on the type itself and needs no Delphi
       // counterpart directive.
       PackingModeOne,
       // The VALVE_CALLBACK_PACK_SMALL / VALVE_CALLBACK_PACK_LARGE regime: 4 bytes on Linux, macOS
       // and FreeBSD, 8 bytes on Windows. This is what nearly every callback struct uses.
       PackingModePlatform
      );

     TFieldItem=record
      Name:TText;
      TypeName:TText;
     end;

     TFieldItems=array of TFieldItem;

     TParameterItem=record
      Name:TText;
      TypeName:TText;
     end;

     TParameterItems=array of TParameterItem;

     TMethodItem=record
      Name:TText;
      FlatName:TText;
      ReturnTypeName:TText;
      Parameters:TParameterItems;
     end;

     TMethodItems=array of TMethodItem;

     TStructItem=record
      Name:TText;
      Fields:TFieldItems;
      Methods:TMethodItems;
      CallbackID:TPasJSONInt64;
      IsCallback:boolean;
      PackingMode:TPackingMode;
      IsHandWritten:boolean;
      IsEmitted:boolean;
      IsInProgress:boolean;
     end;

     TStructItems=array of TStructItem;

     TEnumerationValueItem=record
      Name:TText;
      Value:TText;
     end;

     TEnumerationValueItems=array of TEnumerationValueItem;

     TEnumerationItem=record
      Name:TText;
      Values:TEnumerationValueItems;
     end;

     TEnumerationItems=array of TEnumerationItem;

     TTypeDefinitionItem=record
      Name:TText;
      TypeName:TText;
     end;

     TTypeDefinitionItems=array of TTypeDefinitionItem;

     TConstantItem=record
      Name:TText;
      TypeName:TText;
      Value:TText;
     end;

     TConstantItems=array of TConstantItem;

     TAccessorItem=record
      Name:TText;
      FlatName:TText;
      Kind:TText;
     end;

     TAccessorItems=array of TAccessorItem;

     TInterfaceItem=record
      Name:TText;
      VersionString:TText;
      Accessors:TAccessorItems;
      Methods:TMethodItems;
     end;

     TInterfaceItems=array of TInterfaceItem;

const UnitName='PasVulkan.Steamworks';

      // Records whose C definition uses #pragma pack(1). Determined by walking the pack pragmas of
      // the public headers with the VALVE_CALLBACK_PACK_SMALL branch taken.
      PackOneStructNames:array[0..8] of TText=
       (
        'InputAnalogActionData_t',
        'InputDigitalActionData_t',
        'InputMotionData_t',
        'SteamIPAddress_t',
        'SteamInputActionEvent_t',
        'SteamNetworkingIPAddr',
        'SteamNetworkingIdentity',
        'SteamNetworkingMessagesSessionFailed_t',
        'SteamNetworkingMessagesSessionRequest_t'
       );

      // Records that sit outside any pack pragma and therefore use the compiler's natural alignment.
      PackCDefaultStructNames:array[0..7] of TText=
       (
        'MatchMakingKeyValuePair_t',
        'SteamNetworkPingLocation_t',
        'SteamNetworkingConfigValue_t',
        'SteamNetworkingMessage_t',
        'SteamParentalSettingsChanged_t',
        'SteamRelayNetworkStatus_t',
        'gameserveritem_t',
        'servernetadr_t'
       );

      // Records that steam_api.json describes incorrectly, because it flattens C unions down to a
      // single member and cannot express bit fields or nested structs at all. These are emitted from
      // the hand written blocks further below instead of from the JSON field list.
      HandWrittenStructNames:array[0..5] of TText=
       (
        'RemotePlayInput_t',
        'SteamIPAddress_t',
        'SteamInputActionEvent_t',
        'SteamNetworkingConfigValue_t',
        'SteamNetworkingIPAddr',
        'SteamNetworkingIdentity'
       );

      // Type definitions that the base type block already covers, so they must not be emitted a
      // second time under their C name. intp and uintp are listed here because steam_api.json was
      // generated on a 64 bit machine and hardcodes them as long long, which would be wrong for a
      // 32 bit target.
      BaseTypeDefinitionNames:array[0..11] of TText=
       (
        'int16',
        'int32',
        'int64',
        'int8',
        'intp',
        'lint64',
        'uint16',
        'uint32',
        'uint64',
        'uint8',
        'uintp',
        'ulint64'
       );

      // Parameter names from the C headers that collide with Pascal keywords or with the implicit
      // result identifier.
      ReservedParameterNames:array[0..2] of TText=
       (
        'index',
        'result',
        'type'
       );

var OutputStream:TMemoryStream;
    StructItems:TStructItems;
    EnumerationItems:TEnumerationItems;
    TypeDefinitionItems:TTypeDefinitionItems;
    ConstantItems:TConstantItems;
    InterfaceItems:TInterfaceItems;
    CurrentPackingMode:TPackingMode;
    EmittedFunctionNames:TTexts;
    CountEmittedFunctionNames:TPasJSONSizeInt;

procedure Emit(const aLine:TText);
var Line:TText;
begin
 // Line feed only, matching the existing generated unit src/Vulkan.pas and the rest of src/.
 Line:=aLine+#10;
 OutputStream.Write(Line[1],Length(Line));
end;

procedure EmitEmptyLine;
begin
 Emit('');
end;

function TrimText(const aText:TText):TText;
var StartIndex,EndIndex:TPasJSONSizeInt;
begin
 StartIndex:=1;
 EndIndex:=Length(aText);
 while (StartIndex<=EndIndex) and (aText[StartIndex]<=' ') do begin
  inc(StartIndex);
 end;
 while (EndIndex>=StartIndex) and (aText[EndIndex]<=' ') do begin
  dec(EndIndex);
 end;
 result:=Copy(aText,StartIndex,(EndIndex-StartIndex)+1);
end;

function StartsWithText(const aText,aPrefix:TText):boolean;
begin
 result:=(Length(aText)>=Length(aPrefix)) and (Copy(aText,1,Length(aPrefix))=aPrefix);
end;

function EndsWithText(const aText,aSuffix:TText):boolean;
begin
 result:=(Length(aText)>=Length(aSuffix)) and (Copy(aText,(Length(aText)-Length(aSuffix))+1,Length(aSuffix))=aSuffix);
end;

function ReplaceText(const aText,aFrom,aTo:TText):TText;
var Position:TPasJSONSizeInt;
begin
 result:=aText;
 repeat
  Position:=Pos(aFrom,result);
  if Position>0 then begin
   result:=Copy(result,1,Position-1)+aTo+Copy(result,Position+Length(aFrom),Length(result));
  end;
 until Position=0;
end;

function IsInTextArray(const aText:TText;const aTexts:array of TText):boolean;
var Index:TPasJSONSizeInt;
begin
 result:=false;
 for Index:=Low(aTexts) to High(aTexts) do begin
  if aTexts[Index]=aText then begin
   result:=true;
   exit;
  end;
 end;
end;

// The generator emits a couple of accessor and struct method entry points from more than one place,
// so it keeps track of which loader entries already exist.
function RegisterFunctionName(const aName:TText):boolean;
var Index:TPasJSONSizeInt;
begin
 for Index:=0 to CountEmittedFunctionNames-1 do begin
  if EmittedFunctionNames[Index]=aName then begin
   result:=false;
   exit;
  end;
 end;
 if CountEmittedFunctionNames>=Length(EmittedFunctionNames) then begin
  SetLength(EmittedFunctionNames,(CountEmittedFunctionNames+1)*2);
 end;
 EmittedFunctionNames[CountEmittedFunctionNames]:=aName;
 inc(CountEmittedFunctionNames);
 result:=true;
end;

{ Name and type translation }

function FindStructIndex(const aName:TText):TPasJSONSizeInt;
var Index:TPasJSONSizeInt;
begin
 result:=-1;
 for Index:=0 to length(StructItems)-1 do begin
  if StructItems[Index].Name=aName then begin
   result:=Index;
   exit;
  end;
 end;
end;

function IsEnumerationName(const aName:TText):boolean;
var Index:TPasJSONSizeInt;
begin
 result:=false;
 for Index:=0 to length(EnumerationItems)-1 do begin
  if EnumerationItems[Index].Name=aName then begin
   result:=true;
   exit;
  end;
 end;
end;

function IsTypeDefinitionName(const aName:TText):boolean;
var Index:TPasJSONSizeInt;
begin
 result:=false;
 for Index:=0 to length(TypeDefinitionItems)-1 do begin
  if TypeDefinitionItems[Index].Name=aName then begin
   result:=true;
   exit;
  end;
 end;
end;

// Turns pchName into pchName and type into aType, so that no C parameter name collides with a
// Pascal keyword.
function TranslateParameterName(const aName:TText):TText;
begin
 if IsInTextArray(LowerCase(aName),ReservedParameterNames) then begin
  result:='a'+UpperCase(Copy(aName,1,1))+Copy(aName,2,Length(aName));
 end else begin
  result:=aName;
 end;
end;

// Splits an array type such as "char [1024]" or "uint8 [16]" into its element type and its element
// count. Returns false for anything that is not an array.
function SplitArrayType(const aTypeName:TText;out aElementTypeName:TText;out aElementCount:TText):boolean;
var OpeningPosition,ClosingPosition:TPasJSONSizeInt;
begin
 result:=false;
 aElementTypeName:='';
 aElementCount:='';
 OpeningPosition:=Pos('[',aTypeName);
 ClosingPosition:=Pos(']',aTypeName);
 if (OpeningPosition>0) and (ClosingPosition>OpeningPosition) then begin
  aElementTypeName:=TrimText(Copy(aTypeName,1,OpeningPosition-1));
  aElementCount:=TrimText(Copy(aTypeName,OpeningPosition+1,(ClosingPosition-OpeningPosition)-1));
  result:=(Length(aElementTypeName)>0) and (Length(aElementCount)>0);
 end;
end;

// Maps one C type name onto its Pascal spelling. Pointers, references and const qualifiers are
// stripped here, because a C++ reference has the same ABI as a pointer.
function TranslateTypeName(const aTypeName:TText):TText;
var TypeName:TText;
    PointerLevel:TPasJSONSizeInt;
    Index:TPasJSONSizeInt;
begin

 TypeName:=TrimText(aTypeName);

 // Strip the const qualifiers, they carry no layout information.
 while StartsWithText(TypeName,'const ') do begin
  TypeName:=TrimText(Copy(TypeName,7,Length(TypeName)));
 end;
 if EndsWithText(TypeName,' const') then begin
  TypeName:=TrimText(Copy(TypeName,1,Length(TypeName)-6));
 end;

 // Count the pointer and reference indirections and strip them as well.
 PointerLevel:=0;
 while (Length(TypeName)>0) and ((TypeName[Length(TypeName)]='*') or (TypeName[Length(TypeName)]='&')) do begin
  inc(PointerLevel);
  TypeName:=TrimText(Copy(TypeName,1,Length(TypeName)-1));
  while StartsWithText(TypeName,'const ') do begin
   TypeName:=TrimText(Copy(TypeName,7,Length(TypeName)));
  end;
  if EndsWithText(TypeName,'const') then begin
   TypeName:=TrimText(Copy(TypeName,1,Length(TypeName)-5));
  end;
 end;

 // A nested C++ enumeration such as ISteamHTMLSurface::EHTMLMouseButton keeps only its own name,
 // since none of them collides with a global one.
 Index:=Pos('::',TypeName);
 while Index>0 do begin
  TypeName:=TrimText(Copy(TypeName,Index+2,Length(TypeName)));
  Index:=Pos('::',TypeName);
 end;

 // void* and char* have their own Pascal spellings and must not go through the generic pointer
 // prefixing below.
 if TypeName='void' then begin
  if PointerLevel=0 then begin
   result:='';
  end else if PointerLevel=1 then begin
   result:='TSteamPointer';
  end else begin
   result:='SteamPointer';
   for Index:=2 to PointerLevel do begin
    result:='P'+result;
   end;
  end;
  exit;
 end;
 if TypeName='char' then begin
  if PointerLevel=0 then begin
   result:='TSteamChar';
  end else begin
   result:='SteamChar';
   for Index:=1 to PointerLevel do begin
    result:='P'+result;
   end;
  end;
  exit;
 end;

 // The scalar base types.
 if (TypeName='bool') or (TypeName='_Bool') then begin
  result:='TSteamBool';
 end else if (TypeName='signed char') or (TypeName='int8') or (TypeName='int8_t') then begin
  result:='TSteamInt8';
 end else if (TypeName='unsigned char') or (TypeName='uint8') or (TypeName='uint8_t') then begin
  result:='TSteamUInt8';
 end else if (TypeName='short') or (TypeName='short int') or (TypeName='int16') or (TypeName='int16_t') then begin
  result:='TSteamInt16';
 end else if (TypeName='unsigned short') or (TypeName='unsigned short int') or (TypeName='uint16') or (TypeName='uint16_t') then begin
  result:='TSteamUInt16';
 end else if (TypeName='int') or (TypeName='signed int') or (TypeName='int32') or (TypeName='int32_t') then begin
  result:='TSteamInt32';
 end else if (TypeName='unsigned int') or (TypeName='unsigned') or (TypeName='uint32') or (TypeName='uint32_t') then begin
  result:='TSteamUInt32';
 end else if (TypeName='long long') or (TypeName='long long int') or (TypeName='long int') or (TypeName='int64') or (TypeName='int64_t') or (TypeName='lint64') then begin
  result:='TSteamInt64';
 end else if (TypeName='unsigned long long') or (TypeName='unsigned long long int') or (TypeName='unsigned long int') or (TypeName='uint64') or (TypeName='uint64_t') or (TypeName='ulint64') then begin
  result:='TSteamUInt64';
 end else if (TypeName='intp') or (TypeName='intptr_t') or (TypeName='ptrdiff_t') then begin
  result:='TSteamPtrInt';
 end else if (TypeName='uintp') or (TypeName='uintptr_t') then begin
  result:='TSteamPtrUInt';
 end else if TypeName='float' then begin
  result:='TSteamFloat';
 end else if TypeName='double' then begin
  result:='TSteamDouble';
 end else if (TypeName='size_t') or (TypeName='unsigned long') then begin
  result:='TSteamPtrUInt';
 end else if TypeName='uint64_steamid' then begin
  result:='TSteamUInt64SteamID';
 end else if TypeName='uint64_gameid' then begin
  result:='TSteamUInt64GameID';
 end else begin
  // Everything else is an enumeration, a type definition, a struct or an interface, all of which
  // keep their C name behind the Pascal T prefix.
  result:='T'+TypeName;
 end;

 // Apply the indirections. TFoo becomes PFoo, PPFoo and so on.
 if PointerLevel>0 then begin
  result:=Copy(result,2,Length(result));
  for Index:=1 to PointerLevel do begin
   result:='P'+result;
  end;
 end;

end;

// Translates a field type, which unlike a parameter type can be a fixed size array.
function TranslateFieldTypeName(const aTypeName:TText):TText;
var ElementTypeName,ElementCount:TText;
begin
 if SplitArrayType(aTypeName,ElementTypeName,ElementCount) then begin
  result:='array[0..'+ElementCount+'-1] of '+TranslateTypeName(ElementTypeName);
 end else begin
  result:=TranslateTypeName(aTypeName);
 end;
end;

// Translates the C expressions that appear as constant values, for example "100 * 1024 * 1024",
// "( SteamItemInstanceID_t ) ~ 0", "600.f" and "a | b".
function TranslateConstantValue(const aValue,aTypeName:TText):TText;
var Value,PascalTypeName:TText;
    Position,ClosingPosition,ScanPosition:TPasJSONSizeInt;
    IsHexadecimalLiteral:boolean;
begin

 Value:=TrimText(aValue);

 // Drop the C casts, wherever in the expression they sit. A cast is a parenthesised type name with
 // nothing else inside, and dropping it is safe here because none of these constants relies on the
 // cast to widen or to truncate.
 Position:=1;
 while Position<Length(Value) do begin
  if Value[Position]='(' then begin
   ClosingPosition:=Position+1;
   while (ClosingPosition<=Length(Value)) and (Value[ClosingPosition]<>')') and (Value[ClosingPosition]<>'(') do begin
    inc(ClosingPosition);
   end;
   if (ClosingPosition<=Length(Value)) and (Value[ClosingPosition]=')') then begin
    PascalTypeName:=TrimText(Copy(Value,Position+1,(ClosingPosition-Position)-1));
    if IsTypeDefinitionName(PascalTypeName) or IsEnumerationName(PascalTypeName) then begin
     Value:=Copy(Value,1,Position-1)+' '+Copy(Value,ClosingPosition+1,Length(Value));
     continue;
    end;
   end;
  end;
  inc(Position);
 end;

 // Character literals only mean something as an ordinal value in a Pascal constant expression.
 Position:=1;
 while (Position+2)<=Length(Value) do begin
  if (Value[Position]='''') and (Value[Position+2]='''') then begin
   Value:=Copy(Value,1,Position-1)+'ord('''+Value[Position+1]+''')'+Copy(Value,Position+3,Length(Value));
   inc(Position,8);
  end else begin
   inc(Position);
  end;
 end;

 // Drop the C integer literal suffixes, so 0xffffffffffffffffull becomes 0xffffffffffffffff and 16U
 // becomes 16.
 Position:=1;
 while Position<=Length(Value) do begin
  if (Value[Position]>='0') and (Value[Position]<='9') then begin
   IsHexadecimalLiteral:=((Position+1)<=Length(Value)) and (Value[Position]='0') and
                         ((Value[Position+1]='x') or (Value[Position+1]='X'));
   ScanPosition:=Position;
   if IsHexadecimalLiteral then begin
    inc(ScanPosition,2);
   end;
   while (ScanPosition<=Length(Value)) and
         (((Value[ScanPosition]>='0') and (Value[ScanPosition]<='9')) or
          (IsHexadecimalLiteral and (((Value[ScanPosition]>='a') and (Value[ScanPosition]<='f')) or
                                     ((Value[ScanPosition]>='A') and (Value[ScanPosition]<='F'))))) do begin
    inc(ScanPosition);
   end;
   Position:=ScanPosition;
   while (ScanPosition<=Length(Value)) and
         ((Value[ScanPosition]='u') or (Value[ScanPosition]='U') or
          (Value[ScanPosition]='l') or (Value[ScanPosition]='L')) do begin
    inc(ScanPosition);
   end;
   if ScanPosition>Position then begin
    Value:=Copy(Value,1,Position-1)+Copy(Value,ScanPosition,Length(Value));
   end;
  end else begin
   inc(Position);
  end;
 end;

 // The bitwise operators.
 Value:=ReplaceText(Value,'|',' or ');
 Value:=ReplaceText(Value,'&',' and ');
 Value:=ReplaceText(Value,'~','not ');
 Value:=ReplaceText(Value,'<<',' shl ');
 Value:=ReplaceText(Value,'>>',' shr ');

 // Float literals such as 600.f and 1.5f.
 if EndsWithText(Value,'.f') then begin
  Value:=Copy(Value,1,Length(Value)-2)+'.0';
 end else if EndsWithText(Value,'f') and (Pos('.',Value)>0) then begin
  Value:=Copy(Value,1,Length(Value)-1);
 end;

 // Hexadecimal literals in lower case Pascal spelling. Only the digits of the literal itself are
 // lower cased, so that any identifier behind it keeps its spelling.
 Position:=Pos('0x',LowerCase(Value));
 while Position>0 do begin
  ClosingPosition:=Position+2;
  while (ClosingPosition<=Length(Value)) and
        (((Value[ClosingPosition]>='0') and (Value[ClosingPosition]<='9')) or
         ((Value[ClosingPosition]>='a') and (Value[ClosingPosition]<='f')) or
         ((Value[ClosingPosition]>='A') and (Value[ClosingPosition]<='F'))) do begin
   inc(ClosingPosition);
  end;
  Value:=Copy(Value,1,Position-1)+
         '$'+LowerCase(Copy(Value,Position+2,(ClosingPosition-Position)-2))+
         Copy(Value,ClosingPosition,Length(Value));
  Position:=Pos('0x',LowerCase(Value));
 end;

 // Collapse the whitespace that the substitutions above may have doubled up.
 while Pos('  ',Value)>0 do begin
  Value:=ReplaceText(Value,'  ',' ');
 end;

 result:=TrimText(Value);

 // A negated literal reads better without the space that the C spelling carries.
 if StartsWithText(result,'- ') then begin
  result:='-'+TrimText(Copy(result,3,Length(result)));
 end;

 // An unsigned all-ones constant needs its width, because Pascal's not 0 is signed.
 if result='not 0' then begin
  if (aTypeName='SteamItemInstanceID_t') or (aTypeName='uint64') or (aTypeName='SteamAPICall_t') then begin
   result:='TSteamUInt64($ffffffffffffffff)';
  end else begin
   result:='TSteamUInt32($ffffffff)';
  end;
 end;

 // A hexadecimal literal wider than 32 bits gets an explicit cast, because Delphi would otherwise
 // read it as a signed Int64.
 if StartsWithText(result,'$') and (Length(result)>9) then begin
  ScanPosition:=2;
  while (ScanPosition<=Length(result)) and
        (((result[ScanPosition]>='0') and (result[ScanPosition]<='9')) or
         ((result[ScanPosition]>='a') and (result[ScanPosition]<='f'))) do begin
   inc(ScanPosition);
  end;
  if ScanPosition>Length(result) then begin
   result:='TSteamUInt64('+result+')';
  end;
 end;

end;

{ Packing mode handling }

function GetPackingMode(const aName:TText):TPackingMode;
begin
 if IsInTextArray(aName,PackOneStructNames) then begin
  result:=PackingModeOne;
 end else if IsInTextArray(aName,PackCDefaultStructNames) then begin
  result:=PackingModeCDefault;
 end else begin
  result:=PackingModePlatform;
 end;
end;

// Emits the compiler directives for a packing mode, but only when the mode actually changes. Both
// spellings are emitted because Delphi does not know packrecords. PackingModeOne needs no directive
// at all, since those records carry the packed keyword and therefore ignore the surrounding state.
procedure SetPackingMode(const aPackingMode:TPackingMode);
begin
 if (aPackingMode<>PackingModeOne) and (CurrentPackingMode<>aPackingMode) then begin
  CurrentPackingMode:=aPackingMode;
  case aPackingMode of
   PackingModeCDefault:begin
    Emit('{$ifdef fpc}{$packrecords c}{$else}{$A8}{$endif}');
   end;
   PackingModePlatform:begin
    Emit('{$ifdef Windows}{$ifdef fpc}{$packrecords 8}{$else}{$A8}{$endif}{$else}{$ifdef fpc}{$packrecords 4}{$else}{$A4}{$endif}{$endif}');
   end;
   else begin
   end;
  end;
 end;
end;

// The record keyword for a packing mode, so that the platform independent pack(1) records carry
// their packing in the declaration instead of in a directive.
function GetRecordKeyword(const aPackingMode:TPackingMode):TText;
begin
 if aPackingMode=PackingModeOne then begin
  result:='packed record';
 end else begin
  result:='record';
 end;
end;

{ Hand written type blocks }

// CSteamID and CGameID are C++ classes built out of bit fields inside a union. Pascal has no bit
// fields, so both are emitted as their 64 bit storage plus the shift and mask constants that
// describe the packing. The bit order below is the little endian one, which covers every platform
// Steam runs on.
//
// The storage is a byte array rather than a plain 64 bit scalar on purpose. Both classes are declared
// inside a #pragma pack(1) block, so in C they have an alignment of one byte and get embedded into
// other records without any padding in front of them. A Pascal record holding a 64 bit scalar is
// given that scalar's alignment instead, no matter what packrecords says, which shifted CSteamID
// fields that sit behind an odd number of bytes. A byte array keeps the alignment at one.
procedure EmitHandWrittenSteamIDAndGameID;
begin

 Emit('type PPCSteamID=^PCSteamID;');
 Emit('     PCSteamID=^TCSteamID;');
 Emit('     TCSteamID=packed record');
 Emit('      m_rgubSteamID:array[0..8-1] of TSteamUInt8; // Account ID in bit 0..31, account instance in bit 32..51, account type in bit 52..55, universe in bit 56..63');
 Emit('     end;');
 EmitEmptyLine;

 Emit('const k_nCSteamIDAccountIDShift=0;');
 Emit('      k_nCSteamIDAccountIDMask=TSteamUInt64($00000000ffffffff);');
 Emit('      k_nCSteamIDAccountInstanceShift=32;');
 Emit('      k_nCSteamIDAccountInstanceMask=TSteamUInt64($00000000000fffff);');
 Emit('      k_nCSteamIDAccountTypeShift=52;');
 Emit('      k_nCSteamIDAccountTypeMask=TSteamUInt64($000000000000000f);');
 Emit('      k_nCSteamIDUniverseShift=56;');
 Emit('      k_nCSteamIDUniverseMask=TSteamUInt64($00000000000000ff);');
 EmitEmptyLine;

 Emit('type PPCGameID=^PCGameID;');
 Emit('     PCGameID=^TCGameID;');
 Emit('     TCGameID=packed record');
 Emit('      m_rgubGameID:array[0..8-1] of TSteamUInt8; // Application ID in bit 0..23, type in bit 24..31, mod ID in bit 32..63');
 Emit('     end;');
 EmitEmptyLine;

 Emit('const k_nCGameIDAppIDShift=0;');
 Emit('      k_nCGameIDAppIDMask=TSteamUInt64($0000000000ffffff);');
 Emit('      k_nCGameIDTypeShift=24;');
 Emit('      k_nCGameIDTypeMask=TSteamUInt64($00000000000000ff);');
 Emit('      k_nCGameIDModIDShift=32;');
 Emit('      k_nCGameIDModIDMask=TSteamUInt64($00000000ffffffff);');
 EmitEmptyLine;

end;

// In C the union is followed by m_eType. A Pascal variant part always has to be the last thing in a
// record, so the trailing field is carried inside one of the variants behind a padding array. Every
// variant field stays directly reachable, and the record keeps the C size of 20 bytes.
procedure EmitHandWrittenSteamIPAddress;
begin

 Emit('type { TSteamIPAddress_t }');
 Emit('     PPSteamIPAddress_t=^PSteamIPAddress_t;');
 Emit('     PSteamIPAddress_t=^TSteamIPAddress_t;');
 Emit('     TSteamIPAddress_t=packed record');
 Emit('      case TSteamInt32 of');
 Emit('       0:(');
 Emit('        m_unIPv4:TSteamUInt32; // Host order');
 Emit('       );');
 Emit('       1:(');
 Emit('        m_rgubIPv6:array[0..16-1] of TSteamUInt8; // Network order, same as inaddr_in6');
 Emit('       );');
 Emit('       2:(');
 Emit('        m_ipv6Qword:array[0..2-1] of TSteamUInt64; // Big endian, for internal use only');
 Emit('       );');
 Emit('       3:(');
 Emit('        m_Padding:array[0..16-1] of TSteamUInt8;');
 Emit('        m_eType:TESteamIPType;');
 Emit('       );');
 Emit('     end;');
 EmitEmptyLine;

end;

// Same trailing field situation as TSteamIPAddress_t, here m_port rides along inside the IPv6
// variant. The record keeps the C size of 18 bytes.
procedure EmitHandWrittenSteamNetworkingIPAddr;
begin

 Emit('type { TSteamNetworkingIPAddrIPv4MappedAddress }');
 Emit('     PPSteamNetworkingIPAddrIPv4MappedAddress=^PSteamNetworkingIPAddrIPv4MappedAddress;');
 Emit('     PSteamNetworkingIPAddrIPv4MappedAddress=^TSteamNetworkingIPAddrIPv4MappedAddress;');
 Emit('     TSteamNetworkingIPAddrIPv4MappedAddress=packed record');
 Emit('      m_8zeros:TSteamUInt64;');
 Emit('      m_0000:TSteamUInt16;');
 Emit('      m_ffff:TSteamUInt16;');
 Emit('      m_ip:array[0..4-1] of TSteamUInt8; // Network byte order');
 Emit('     end;');
 EmitEmptyLine;

 Emit('     { TSteamNetworkingIPAddr }');
 Emit('     PPSteamNetworkingIPAddr=^PSteamNetworkingIPAddr;');
 Emit('     PSteamNetworkingIPAddr=^TSteamNetworkingIPAddr;');
 Emit('     TSteamNetworkingIPAddr=packed record');
 Emit('      case TSteamInt32 of');
 Emit('       0:(');
 Emit('        m_ipv6:array[0..16-1] of TSteamUInt8;');
 Emit('        m_port:TSteamUInt16; // Host byte order');
 Emit('       );');
 Emit('       1:(');
 Emit('        m_ipv4:TSteamNetworkingIPAddrIPv4MappedAddress;');
 Emit('       );');
 Emit('     end;');
 EmitEmptyLine;

 Emit('const k_cchSteamNetworkingIPAddrMaxString=48;');
 EmitEmptyLine;

end;

procedure EmitHandWrittenSteamNetworkingIdentity;
begin

 Emit('type { TSteamNetworkingIdentity }');
 Emit('     PPSteamNetworkingIdentity=^PSteamNetworkingIdentity;');
 Emit('     PSteamNetworkingIdentity=^TSteamNetworkingIdentity;');
 Emit('     TSteamNetworkingIdentity=packed record');
 Emit('      m_eType:TESteamNetworkingIdentityType;');
 Emit('      m_cbSize:TSteamInt32;');
 Emit('      case TSteamInt32 of');
 Emit('       0:(');
 Emit('        m_steamID64:TSteamUInt64;');
 Emit('       );');
 Emit('       1:(');
 Emit('        m_PSNID:TSteamUInt64;');
 Emit('       );');
 Emit('       2:(');
 Emit('        m_szGenericString:array[0..32-1] of TSteamChar;');
 Emit('       );');
 Emit('       3:(');
 Emit('        m_szXboxPairwiseID:array[0..33-1] of TSteamChar;');
 Emit('       );');
 Emit('       4:(');
 Emit('        m_genericBytes:array[0..32-1] of TSteamUInt8;');
 Emit('       );');
 Emit('       5:(');
 Emit('        m_szUnknownRawString:array[0..128-1] of TSteamChar;');
 Emit('       );');
 Emit('       6:(');
 Emit('        m_ip:TSteamNetworkingIPAddr;');
 Emit('       );');
 Emit('       7:(');
 Emit('        m_reserved:array[0..32-1] of TSteamUInt32; // Pads the record out to leave room for future expansion');
 Emit('       );');
 Emit('     end;');
 EmitEmptyLine;

 Emit('const k_cchSteamNetworkingIdentityMaxString=128;');
 Emit('      k_cchSteamNetworkingIdentityMaxGenericString=32;');
 Emit('      k_cchSteamNetworkingIdentityMaxXboxPairwiseID=33;');
 Emit('      k_cbSteamNetworkingIdentityMaxGenericBytes=32;');
 EmitEmptyLine;

end;

procedure EmitHandWrittenSteamNetworkingConfigValue;
begin

 SetPackingMode(PackingModeCDefault);

 Emit('type { TSteamNetworkingConfigValue_t }');
 Emit('     PPSteamNetworkingConfigValue_t=^PSteamNetworkingConfigValue_t;');
 Emit('     PSteamNetworkingConfigValue_t=^TSteamNetworkingConfigValue_t;');
 Emit('     TSteamNetworkingConfigValue_t=record');
 Emit('      m_eValue:TESteamNetworkingConfigValue;');
 Emit('      m_eDataType:TESteamNetworkingConfigDataType;');
 Emit('      case TSteamInt32 of');
 Emit('       0:(');
 Emit('        m_int32:TSteamInt32;');
 Emit('       );');
 Emit('       1:(');
 Emit('        m_int64:TSteamInt64;');
 Emit('       );');
 Emit('       2:(');
 Emit('        m_float:TSteamFloat;');
 Emit('       );');
 Emit('       3:(');
 Emit('        m_string:PSteamChar; // Points at a caller owned zero terminated buffer');
 Emit('       );');
 Emit('       4:(');
 Emit('        m_ptr:TSteamPointer;');
 Emit('       );');
 Emit('     end;');
 EmitEmptyLine;

end;

procedure EmitHandWrittenSteamInputActionEvent;
begin

 Emit('type { TSteamInputActionEventAnalogAction_t }');
 Emit('     PPSteamInputActionEventAnalogAction_t=^PSteamInputActionEventAnalogAction_t;');
 Emit('     PSteamInputActionEventAnalogAction_t=^TSteamInputActionEventAnalogAction_t;');
 Emit('     TSteamInputActionEventAnalogAction_t=packed record');
 Emit('      actionHandle:TInputAnalogActionHandle_t;');
 Emit('      analogActionData:TInputAnalogActionData_t;');
 Emit('     end;');
 EmitEmptyLine;

 Emit('     { TSteamInputActionEventDigitalAction_t }');
 Emit('     PPSteamInputActionEventDigitalAction_t=^PSteamInputActionEventDigitalAction_t;');
 Emit('     PSteamInputActionEventDigitalAction_t=^TSteamInputActionEventDigitalAction_t;');
 Emit('     TSteamInputActionEventDigitalAction_t=packed record');
 Emit('      actionHandle:TInputDigitalActionHandle_t;');
 Emit('      digitalActionData:TInputDigitalActionData_t;');
 Emit('     end;');
 EmitEmptyLine;

 Emit('     { TSteamInputActionEvent_t }');
 Emit('     PPSteamInputActionEvent_t=^PSteamInputActionEvent_t;');
 Emit('     PSteamInputActionEvent_t=^TSteamInputActionEvent_t;');
 Emit('     TSteamInputActionEvent_t=packed record');
 Emit('      controllerHandle:TInputHandle_t;');
 Emit('      eEventType:TESteamInputActionEventType;');
 Emit('      case TSteamInt32 of');
 Emit('       0:(');
 Emit('        analogAction:TSteamInputActionEventAnalogAction_t;');
 Emit('       );');
 Emit('       1:(');
 Emit('        digitalAction:TSteamInputActionEventDigitalAction_t;');
 Emit('       );');
 Emit('     end;');
 EmitEmptyLine;

end;

procedure EmitHandWrittenRemotePlayInput;
begin

 SetPackingMode(PackingModePlatform);

 Emit('type { TRemotePlayInput_t }');
 Emit('     PPRemotePlayInput_t=^PRemotePlayInput_t;');
 Emit('     PRemotePlayInput_t=^TRemotePlayInput_t;');
 Emit('     TRemotePlayInput_t=record');
 Emit('      m_unSessionID:TRemotePlaySessionID_t;');
 Emit('      m_eType:TERemotePlayInputType;');
 Emit('      case TSteamInt32 of');
 Emit('       0:(');
 Emit('        m_MouseMotion:TRemotePlayInputMouseMotion_t; // Valid when m_eType is k_ERemotePlayInputMouseMotion');
 Emit('       );');
 Emit('       1:(');
 Emit('        m_eMouseButton:TERemotePlayMouseButton; // Valid when m_eType is k_ERemotePlayInputMouseButtonDown or k_ERemotePlayInputMouseButtonUp');
 Emit('       );');
 Emit('       2:(');
 Emit('        m_MouseWheel:TRemotePlayInputMouseWheel_t; // Valid when m_eType is k_ERemotePlayInputMouseWheel');
 Emit('       );');
 Emit('       3:(');
 Emit('        m_Key:TRemotePlayInputKey_t; // Valid when m_eType is k_ERemotePlayInputKeyDown or k_ERemotePlayInputKeyUp');
 Emit('       );');
 Emit('       4:(');
 Emit('        padding:array[0..56-1] of TSteamChar; // Unused space for future use');
 Emit('       );');
 Emit('     end;');
 EmitEmptyLine;

end;

// Returns the struct names a hand written block depends on, so that the topological sort still
// places it after everything it references.
function GetHandWrittenStructDependencies(const aName:TText):TTexts;
begin
 result:=nil;
 if aName='SteamNetworkingIdentity' then begin
  SetLength(result,1);
  result[0]:='SteamNetworkingIPAddr';
 end else if aName='SteamInputActionEvent_t' then begin
  SetLength(result,2);
  result[0]:='InputAnalogActionData_t';
  result[1]:='InputDigitalActionData_t';
 end else if aName='RemotePlayInput_t' then begin
  SetLength(result,3);
  result[0]:='RemotePlayInputMouseMotion_t';
  result[1]:='RemotePlayInputMouseWheel_t';
  result[2]:='RemotePlayInputKey_t';
 end else begin
 end;
end;

function EmitHandWrittenStruct(const aName:TText):boolean;
begin
 result:=true;
 if aName='SteamIPAddress_t' then begin
  EmitHandWrittenSteamIPAddress;
 end else if aName='SteamNetworkingIPAddr' then begin
  EmitHandWrittenSteamNetworkingIPAddr;
 end else if aName='SteamNetworkingIdentity' then begin
  EmitHandWrittenSteamNetworkingIdentity;
 end else if aName='SteamNetworkingConfigValue_t' then begin
  EmitHandWrittenSteamNetworkingConfigValue;
 end else if aName='SteamInputActionEvent_t' then begin
  EmitHandWrittenSteamInputActionEvent;
 end else if aName='RemotePlayInput_t' then begin
  EmitHandWrittenRemotePlayInput;
 end else begin
  result:=false;
 end;
end;

{ JSON reading }

procedure ReadMethods(const aArray:TPasJSONItemArray;out aMethods:TMethodItems);
var MethodIndex,ParameterIndex:TPasJSONSizeInt;
    MethodObject,ParameterObject:TPasJSONItemObject;
    ParameterArray:TPasJSONItemArray;
    FlatTypeName:TText;
begin

 aMethods:=nil;
 if not assigned(aArray) then begin
  exit;
 end;

 SetLength(aMethods,aArray.Count);
 for MethodIndex:=0 to aArray.Count-1 do begin

  MethodObject:=aArray.Items[MethodIndex] as TPasJSONItemObject;
  aMethods[MethodIndex].Name:=TPasJSON.GetString(MethodObject.Properties['methodname'],'');
  aMethods[MethodIndex].FlatName:=TPasJSON.GetString(MethodObject.Properties['methodname_flat'],'');

  // The flat spellings win wherever they exist, because the exported C entry point takes a plain
  // uint64 where the C++ method takes a CSteamID or a CGameID.
  FlatTypeName:=TPasJSON.GetString(MethodObject.Properties['returntype_flat'],'');
  if Length(FlatTypeName)>0 then begin
   aMethods[MethodIndex].ReturnTypeName:=FlatTypeName;
  end else begin
   aMethods[MethodIndex].ReturnTypeName:=TPasJSON.GetString(MethodObject.Properties['returntype'],'void');
  end;

  aMethods[MethodIndex].Parameters:=nil;
  ParameterArray:=MethodObject.Properties['params'] as TPasJSONItemArray;
  if assigned(ParameterArray) then begin
   SetLength(aMethods[MethodIndex].Parameters,ParameterArray.Count);
   for ParameterIndex:=0 to ParameterArray.Count-1 do begin
    ParameterObject:=ParameterArray.Items[ParameterIndex] as TPasJSONItemObject;
    aMethods[MethodIndex].Parameters[ParameterIndex].Name:=TPasJSON.GetString(ParameterObject.Properties['paramname'],'');
    FlatTypeName:=TPasJSON.GetString(ParameterObject.Properties['paramtype_flat'],'');
    if Length(FlatTypeName)>0 then begin
     aMethods[MethodIndex].Parameters[ParameterIndex].TypeName:=FlatTypeName;
    end else begin
     aMethods[MethodIndex].Parameters[ParameterIndex].TypeName:=TPasJSON.GetString(ParameterObject.Properties['paramtype'],'');
    end;
   end;
  end;

 end;

end;

procedure ReadFields(const aArray:TPasJSONItemArray;out aFields:TFieldItems);
var FieldIndex:TPasJSONSizeInt;
    FieldObject:TPasJSONItemObject;
begin
 aFields:=nil;
 if not assigned(aArray) then begin
  exit;
 end;
 SetLength(aFields,aArray.Count);
 for FieldIndex:=0 to aArray.Count-1 do begin
  FieldObject:=aArray.Items[FieldIndex] as TPasJSONItemObject;
  aFields[FieldIndex].Name:=TPasJSON.GetString(FieldObject.Properties['fieldname'],'');
  aFields[FieldIndex].TypeName:=TPasJSON.GetString(FieldObject.Properties['fieldtype'],'');
 end;
end;

procedure ReadStructs(const aRootObject:TPasJSONItemObject);
var StructIndex,CountStructs,SourceIndex:TPasJSONSizeInt;
    StructArray,CallbackArray:TPasJSONItemArray;
    StructObject:TPasJSONItemObject;
begin

 StructArray:=aRootObject.Properties['structs'] as TPasJSONItemArray;
 CallbackArray:=aRootObject.Properties['callback_structs'] as TPasJSONItemArray;

 CountStructs:=0;
 if assigned(StructArray) then begin
  inc(CountStructs,StructArray.Count);
 end;
 if assigned(CallbackArray) then begin
  inc(CountStructs,CallbackArray.Count);
 end;
 SetLength(StructItems,CountStructs);

 StructIndex:=0;

 if assigned(StructArray) then begin
  for SourceIndex:=0 to StructArray.Count-1 do begin
   StructObject:=StructArray.Items[SourceIndex] as TPasJSONItemObject;
   StructItems[StructIndex].Name:=TPasJSON.GetString(StructObject.Properties['struct'],'');
   ReadFields(StructObject.Properties['fields'] as TPasJSONItemArray,StructItems[StructIndex].Fields);
   ReadMethods(StructObject.Properties['methods'] as TPasJSONItemArray,StructItems[StructIndex].Methods);
   StructItems[StructIndex].CallbackID:=-1;
   StructItems[StructIndex].IsCallback:=false;
   inc(StructIndex);
  end;
 end;

 if assigned(CallbackArray) then begin
  for SourceIndex:=0 to CallbackArray.Count-1 do begin
   StructObject:=CallbackArray.Items[SourceIndex] as TPasJSONItemObject;
   StructItems[StructIndex].Name:=TPasJSON.GetString(StructObject.Properties['struct'],'');
   ReadFields(StructObject.Properties['fields'] as TPasJSONItemArray,StructItems[StructIndex].Fields);
   ReadMethods(StructObject.Properties['methods'] as TPasJSONItemArray,StructItems[StructIndex].Methods);
   StructItems[StructIndex].CallbackID:=TPasJSON.GetInt64(StructObject.Properties['callback_id'],-1);
   StructItems[StructIndex].IsCallback:=true;
   inc(StructIndex);
  end;
 end;

 for StructIndex:=0 to length(StructItems)-1 do begin
  StructItems[StructIndex].PackingMode:=GetPackingMode(StructItems[StructIndex].Name);
  StructItems[StructIndex].IsHandWritten:=IsInTextArray(StructItems[StructIndex].Name,HandWrittenStructNames);
  StructItems[StructIndex].IsEmitted:=false;
  StructItems[StructIndex].IsInProgress:=false;
 end;

end;

procedure ReadEnumerations(const aRootObject:TPasJSONItemObject);
var EnumerationIndex,ValueIndex:TPasJSONSizeInt;
    EnumerationArray,ValueArray:TPasJSONItemArray;
    EnumerationObject,ValueObject:TPasJSONItemObject;
begin

 EnumerationItems:=nil;
 EnumerationArray:=aRootObject.Properties['enums'] as TPasJSONItemArray;
 if not assigned(EnumerationArray) then begin
  exit;
 end;

 SetLength(EnumerationItems,EnumerationArray.Count);
 for EnumerationIndex:=0 to EnumerationArray.Count-1 do begin

  EnumerationObject:=EnumerationArray.Items[EnumerationIndex] as TPasJSONItemObject;
  EnumerationItems[EnumerationIndex].Name:=TPasJSON.GetString(EnumerationObject.Properties['enumname'],'');

  EnumerationItems[EnumerationIndex].Values:=nil;
  ValueArray:=EnumerationObject.Properties['values'] as TPasJSONItemArray;
  if assigned(ValueArray) then begin
   SetLength(EnumerationItems[EnumerationIndex].Values,ValueArray.Count);
   for ValueIndex:=0 to ValueArray.Count-1 do begin
    ValueObject:=ValueArray.Items[ValueIndex] as TPasJSONItemObject;
    EnumerationItems[EnumerationIndex].Values[ValueIndex].Name:=TPasJSON.GetString(ValueObject.Properties['name'],'');
    EnumerationItems[EnumerationIndex].Values[ValueIndex].Value:=TPasJSON.GetString(ValueObject.Properties['value'],'0');
   end;
  end;

 end;

end;

procedure ReadTypeDefinitions(const aRootObject:TPasJSONItemObject);
var Index:TPasJSONSizeInt;
    TypeDefinitionArray:TPasJSONItemArray;
    TypeDefinitionObject:TPasJSONItemObject;
begin
 TypeDefinitionItems:=nil;
 TypeDefinitionArray:=aRootObject.Properties['typedefs'] as TPasJSONItemArray;
 if not assigned(TypeDefinitionArray) then begin
  exit;
 end;
 SetLength(TypeDefinitionItems,TypeDefinitionArray.Count);
 for Index:=0 to TypeDefinitionArray.Count-1 do begin
  TypeDefinitionObject:=TypeDefinitionArray.Items[Index] as TPasJSONItemObject;
  TypeDefinitionItems[Index].Name:=TPasJSON.GetString(TypeDefinitionObject.Properties['typedef'],'');
  TypeDefinitionItems[Index].TypeName:=TPasJSON.GetString(TypeDefinitionObject.Properties['type'],'');
 end;
end;

procedure ReadConstants(const aRootObject:TPasJSONItemObject);
var Index:TPasJSONSizeInt;
    ConstantArray:TPasJSONItemArray;
    ConstantObject:TPasJSONItemObject;
begin
 ConstantItems:=nil;
 ConstantArray:=aRootObject.Properties['consts'] as TPasJSONItemArray;
 if not assigned(ConstantArray) then begin
  exit;
 end;
 SetLength(ConstantItems,ConstantArray.Count);
 for Index:=0 to ConstantArray.Count-1 do begin
  ConstantObject:=ConstantArray.Items[Index] as TPasJSONItemObject;
  ConstantItems[Index].Name:=TPasJSON.GetString(ConstantObject.Properties['constname'],'');
  ConstantItems[Index].TypeName:=TPasJSON.GetString(ConstantObject.Properties['consttype'],'');
  ConstantItems[Index].Value:=TPasJSON.GetString(ConstantObject.Properties['constval'],'0');
 end;
end;

procedure ReadInterfaces(const aRootObject:TPasJSONItemObject);
var InterfaceIndex,AccessorIndex:TPasJSONSizeInt;
    InterfaceArray,AccessorArray:TPasJSONItemArray;
    InterfaceObject,AccessorObject:TPasJSONItemObject;
begin

 InterfaceItems:=nil;
 InterfaceArray:=aRootObject.Properties['interfaces'] as TPasJSONItemArray;
 if not assigned(InterfaceArray) then begin
  exit;
 end;

 SetLength(InterfaceItems,InterfaceArray.Count);
 for InterfaceIndex:=0 to InterfaceArray.Count-1 do begin

  InterfaceObject:=InterfaceArray.Items[InterfaceIndex] as TPasJSONItemObject;
  InterfaceItems[InterfaceIndex].Name:=TPasJSON.GetString(InterfaceObject.Properties['classname'],'');
  InterfaceItems[InterfaceIndex].VersionString:=TPasJSON.GetString(InterfaceObject.Properties['version_string'],'');
  ReadMethods(InterfaceObject.Properties['methods'] as TPasJSONItemArray,InterfaceItems[InterfaceIndex].Methods);

  InterfaceItems[InterfaceIndex].Accessors:=nil;
  AccessorArray:=InterfaceObject.Properties['accessors'] as TPasJSONItemArray;
  if assigned(AccessorArray) then begin
   SetLength(InterfaceItems[InterfaceIndex].Accessors,AccessorArray.Count);
   for AccessorIndex:=0 to AccessorArray.Count-1 do begin
    AccessorObject:=AccessorArray.Items[AccessorIndex] as TPasJSONItemObject;
    InterfaceItems[InterfaceIndex].Accessors[AccessorIndex].Name:=TPasJSON.GetString(AccessorObject.Properties['name'],'');
    InterfaceItems[InterfaceIndex].Accessors[AccessorIndex].FlatName:=TPasJSON.GetString(AccessorObject.Properties['name_flat'],'');
    InterfaceItems[InterfaceIndex].Accessors[AccessorIndex].Kind:=TPasJSON.GetString(AccessorObject.Properties['kind'],'');
   end;
  end;

 end;

end;

{ Emitters }

procedure EmitFileHeader;
begin
 Emit('(*');
 Emit('** Copyright (c) Valve Corporation, All rights reserved. (the Steamworks SDK)');
 Emit('** Copyright (c) 2026, Benjamin Rosseaux (benjamin@rosseaux.de, the pascal headers)');
 Emit('**');
 Emit('** Permission is hereby granted, free of charge, to any person obtaining a');
 Emit('** copy of this software and/or associated documentation files (the');
 Emit('** "Materials"), to deal in the Materials without restriction, including');
 Emit('** without limitation the rights to use, copy, modify, merge, publish,');
 Emit('** distribute, sublicense, and/or sell copies of the Materials, and to');
 Emit('** permit persons to whom the Materials are furnished to do so, subject to');
 Emit('** the following conditions:');
 Emit('**');
 Emit('** The above copyright notice and this permission notice shall be included');
 Emit('** in all copies or substantial portions of the Materials.');
 Emit('**');
 Emit('** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,');
 Emit('** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF');
 Emit('** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.');
 Emit('** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY');
 Emit('** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,');
 Emit('** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE');
 Emit('** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.');
 Emit('*)');
 Emit('(*');
 Emit('** This header is generated from the Steamworks SDK API description file steam_api.json by');
 Emit('** the steamapi2pas tool. Do not edit it, edit the generator instead.');
 Emit('**');
 Emit('** The bindings target the flat C API of the Steamworks redistributable libraries, where every');
 Emit('** interface method is exported as a plain cdecl symbol taking the interface pointer as its');
 Emit('** first argument, so no C++ vtable is ever involved.');
 Emit('*)');
end;

procedure EmitUnitPrologue;
begin

 Emit('unit '+UnitName+';');
 Emit('{$ifdef fpc}');
 Emit(' {$mode delphi}');
 Emit(' {$define CAN_INLINE}');
 Emit(' {$notes off}');
 Emit('{$else}');
 Emit(' {$undef CAN_INLINE}');
 Emit(' {$ifdef conditionalexpressions}');
 Emit('  {$if CompilerVersion>=24.0}');
 Emit('   {$legacyifend on}');
 Emit('  {$ifend}');
 Emit('  {$if CompilerVersion>=18.0}');
 Emit('   {$define CAN_INLINE}');
 Emit('  {$ifend}');
 Emit(' {$endif}');
 Emit('{$endif}');
 Emit('{$ifdef Win32}');
 Emit(' {$define Windows}');
 Emit('{$endif}');
 Emit('{$ifdef Win64}');
 Emit(' {$define Windows}');
 Emit('{$endif}');
 Emit('{$rangechecks off}');
 Emit('{$hints off}');
 Emit('{$scopedenums off}');
 EmitEmptyLine;

 Emit('interface');
 EmitEmptyLine;

 Emit('uses {$if defined(Windows)}');
 Emit('      Windows,');
 Emit('     {$elseif defined(Unix)}');
 Emit('      dl,');
 Emit('     {$ifend}');
 Emit('     SysUtils;');
 EmitEmptyLine;

 // The Steam client library ships under a different name per platform, and the 32 bit Windows build
 // keeps the historic unsuffixed name.
 Emit('const STEAMWORKS_DEFAULT_LIB_NAME={$ifdef Windows}{$ifdef cpu64}''steam_api64.dll''{$else}''steam_api.dll''{$endif}{$else}{$ifdef Darwin}''libsteam_api.dylib''{$else}''libsteam_api.so''{$endif}{$endif};');
 EmitEmptyLine;

 // Everything below the platform packing directive belongs to the VALVE_CALLBACK_PACK regime unless
 // a record explicitly switches away from it.
 Emit('// The Steamworks headers pack their callback records with 4 bytes on Linux, macOS and FreeBSD');
 Emit('// (VALVE_CALLBACK_PACK_SMALL) and with 8 bytes on Windows (VALVE_CALLBACK_PACK_LARGE).');
 Emit('{$ifdef Windows}{$ifdef fpc}{$packrecords 8}{$else}{$A8}{$endif}{$else}{$ifdef fpc}{$packrecords 4}{$else}{$A4}{$endif}{$endif}');
 CurrentPackingMode:=PackingModePlatform;
 EmitEmptyLine;

end;

procedure EmitPointerTypes(const aName:TText;const aIsFirstInBlock:boolean);
var Prefix:TText;
begin
 if aIsFirstInBlock then begin
  Prefix:='type ';
 end else begin
  Prefix:='     ';
 end;
 Emit(Prefix+'PP'+aName+'=^P'+aName+';');
 Emit('     P'+aName+'=^T'+aName+';');
end;

procedure EmitBaseTypes;
begin

 Emit('type PPSteamInt8=^PSteamInt8;');
 Emit('     PSteamInt8=^TSteamInt8;');
 Emit('     TSteamInt8={$ifdef fpc}Int8{$else}ShortInt{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamUInt8=^PSteamUInt8;');
 Emit('     PSteamUInt8=^TSteamUInt8;');
 Emit('     TSteamUInt8={$ifdef fpc}UInt8{$else}Byte{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamInt16=^PSteamInt16;');
 Emit('     PSteamInt16=^TSteamInt16;');
 Emit('     TSteamInt16={$ifdef fpc}Int16{$else}SmallInt{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamUInt16=^PSteamUInt16;');
 Emit('     PSteamUInt16=^TSteamUInt16;');
 Emit('     TSteamUInt16={$ifdef fpc}UInt16{$else}Word{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamInt32=^PSteamInt32;');
 Emit('     PSteamInt32=^TSteamInt32;');
 Emit('     TSteamInt32={$ifdef fpc}Int32{$else}LongInt{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamUInt32=^PSteamUInt32;');
 Emit('     PSteamUInt32=^TSteamUInt32;');
 Emit('     TSteamUInt32={$ifdef fpc}UInt32{$else}LongWord{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamInt64=^PSteamInt64;');
 Emit('     PSteamInt64=^TSteamInt64;');
 Emit('     TSteamInt64=Int64;');
 EmitEmptyLine;

 Emit('     PPSteamUInt64=^PSteamUInt64;');
 Emit('     PSteamUInt64=^TSteamUInt64;');
 Emit('     TSteamUInt64=UInt64;');
 EmitEmptyLine;

 Emit('     PPSteamPtrInt=^PSteamPtrInt;');
 Emit('     PSteamPtrInt=^TSteamPtrInt;');
 Emit('     TSteamPtrInt={$ifdef fpc}PtrInt{$else}NativeInt{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamPtrUInt=^PSteamPtrUInt;');
 Emit('     PSteamPtrUInt=^TSteamPtrUInt;');
 Emit('     TSteamPtrUInt={$ifdef fpc}PtrUInt{$else}NativeUInt{$endif};');
 EmitEmptyLine;

 Emit('     PPSteamFloat=^PSteamFloat;');
 Emit('     PSteamFloat=^TSteamFloat;');
 Emit('     TSteamFloat=Single;');
 EmitEmptyLine;

 Emit('     PPSteamDouble=^PSteamDouble;');
 Emit('     PSteamDouble=^TSteamDouble;');
 Emit('     TSteamDouble=Double;');
 EmitEmptyLine;

 Emit('     PPSteamChar=^PSteamChar;');
 Emit('     PSteamChar=PAnsiChar;');
 Emit('     TSteamChar=AnsiChar;');
 EmitEmptyLine;

 Emit('     PPSteamPointer=^PSteamPointer;');
 Emit('     PSteamPointer=^TSteamPointer;');
 Emit('     TSteamPointer=Pointer;');
 EmitEmptyLine;

 // A C++ bool is one byte wide and only ever holds 0 or 1, but ByteBool treats every non zero value
 // as true, which is the safer reading direction for values that come out of the library.
 Emit('     PPSteamBool=^PSteamBool;');
 Emit('     PSteamBool=^TSteamBool;');
 Emit('     TSteamBool=ByteBool;');
 EmitEmptyLine;

 // The flat API passes a CSteamID and a CGameID as a plain 64 bit value.
 Emit('     PPSteamUInt64SteamID=^PSteamUInt64SteamID;');
 Emit('     PSteamUInt64SteamID=^TSteamUInt64SteamID;');
 Emit('     TSteamUInt64SteamID=TSteamUInt64;');
 EmitEmptyLine;

 Emit('     PPSteamUInt64GameID=^PSteamUInt64GameID;');
 Emit('     PSteamUInt64GameID=^TSteamUInt64GameID;');
 Emit('     TSteamUInt64GameID=TSteamUInt64;');
 EmitEmptyLine;

end;

// Types that the interface methods refer to but that steam_api.json does not declare. The four
// opaque records have no definition in the public headers either, so a pointer is all the type
// information that exists for them. ScePadTriggerEffectParam does have a full definition, but it
// lives in isteamdualsense.h, which is outside the JSON and therefore part of the hand written
// companion include.
procedure EmitExtraTypes;
begin

 Emit('type PPSteamAPIWarningMessageHook_t=^PSteamAPIWarningMessageHook_t;');
 Emit('     PSteamAPIWarningMessageHook_t=^TSteamAPIWarningMessageHook_t;');
 Emit('     TSteamAPIWarningMessageHook_t=procedure(const aSeverity:TSteamInt32;const aDebugText:PSteamChar); cdecl;');
 EmitEmptyLine;

 Emit('     { TISteamNetworkingConnectionSignaling }');
 Emit('     PPISteamNetworkingConnectionSignaling=^PISteamNetworkingConnectionSignaling;');
 Emit('     PISteamNetworkingConnectionSignaling=^TISteamNetworkingConnectionSignaling;');
 Emit('     TISteamNetworkingConnectionSignaling=record');
 Emit('     end;');
 EmitEmptyLine;

 Emit('     { TISteamNetworkingSignalingRecvContext }');
 Emit('     PPISteamNetworkingSignalingRecvContext=^PISteamNetworkingSignalingRecvContext;');
 Emit('     PISteamNetworkingSignalingRecvContext=^TISteamNetworkingSignalingRecvContext;');
 Emit('     TISteamNetworkingSignalingRecvContext=record');
 Emit('     end;');
 EmitEmptyLine;

 Emit('     { TSteamDatagramRelayAuthTicket }');
 Emit('     PPSteamDatagramRelayAuthTicket=^PSteamDatagramRelayAuthTicket;');
 Emit('     PSteamDatagramRelayAuthTicket=^TSteamDatagramRelayAuthTicket;');
 Emit('     TSteamDatagramRelayAuthTicket=record');
 Emit('     end;');
 EmitEmptyLine;

 Emit('     { TScePadTriggerEffectParam }');
 Emit('     PPScePadTriggerEffectParam=^PScePadTriggerEffectParam;');
 Emit('     PScePadTriggerEffectParam=^TScePadTriggerEffectParam;');
 Emit('     TScePadTriggerEffectParam=record');
 Emit('     end;');
 EmitEmptyLine;

 // The two enumerations that ISteamHTMLSurface nests inside its own class scope.
 Emit('type { TEHTMLMouseButton }');
 Emit('     PPEHTMLMouseButton=^PEHTMLMouseButton;');
 Emit('     PEHTMLMouseButton=^TEHTMLMouseButton;');
 Emit('     TEHTMLMouseButton=TSteamInt32;');
 EmitEmptyLine;

 Emit('const eHTMLMouseButton_Left=0;');
 Emit('      eHTMLMouseButton_Right=1;');
 Emit('      eHTMLMouseButton_Middle=2;');
 EmitEmptyLine;

 Emit('type { TEHTMLKeyModifiers }');
 Emit('     PPEHTMLKeyModifiers=^PEHTMLKeyModifiers;');
 Emit('     PEHTMLKeyModifiers=^TEHTMLKeyModifiers;');
 Emit('     TEHTMLKeyModifiers=TSteamInt32;');
 EmitEmptyLine;

 Emit('const k_eHTMLKeyModifier_None=0;');
 Emit('      k_eHTMLKeyModifier_AltDown=1 shl 0;');
 Emit('      k_eHTMLKeyModifier_CtrlDown=1 shl 1;');
 Emit('      k_eHTMLKeyModifier_ShiftDown=1 shl 2;');
 EmitEmptyLine;

end;

// Emits the scalar type definitions. Arrays and function pointers are held back, because the latter
// reference records that are only declared further down.
procedure EmitScalarTypeDefinitions;
var Index:TPasJSONSizeInt;
    Item:TTypeDefinitionItem;
    ElementTypeName,ElementCount:TText;
begin
 for Index:=0 to length(TypeDefinitionItems)-1 do begin
  Item:=TypeDefinitionItems[Index];
  if not IsInTextArray(Item.Name,BaseTypeDefinitionNames) then begin
   if (Pos('(*)',Item.TypeName)=0) and not SplitArrayType(Item.TypeName,ElementTypeName,ElementCount) then begin
    EmitPointerTypes(Item.Name,true);
    Emit('     T'+Item.Name+'='+TranslateTypeName(Item.TypeName)+';');
    EmitEmptyLine;
   end;
  end;
 end;
end;

procedure EmitArrayTypeDefinitions;
var Index:TPasJSONSizeInt;
    Item:TTypeDefinitionItem;
    ElementTypeName,ElementCount:TText;
begin
 for Index:=0 to length(TypeDefinitionItems)-1 do begin
  Item:=TypeDefinitionItems[Index];
  if not IsInTextArray(Item.Name,BaseTypeDefinitionNames) then begin
   if SplitArrayType(Item.TypeName,ElementTypeName,ElementCount) then begin
    EmitPointerTypes(Item.Name,true);
    Emit('     T'+Item.Name+'=array[0..'+ElementCount+'-1] of '+TranslateTypeName(ElementTypeName)+';');
    EmitEmptyLine;
   end;
  end;
 end;
end;

// Turns a C function pointer spelling such as "void (*)(SteamNetworkingMessage_t *)" into the body
// of a Pascal procedural type, so "procedure(const aParameter1:PSteamNetworkingMessage_t); cdecl;".
function BuildFunctionPointerTypeBody(const aTypeName:TText):TText;
var CountParameters:TPasJSONSizeInt;
    OpeningPosition,ClosingPosition,CommaPosition,MarkerPosition:TPasJSONSizeInt;
    Signature,ParameterList,ParameterTypeName,ReturnTypeName:TText;
begin

 MarkerPosition:=Pos('(*)',aTypeName);
 ReturnTypeName:=TrimText(Copy(aTypeName,1,MarkerPosition-1));

 // The parameter list sits between the parentheses that follow the (*) marker.
 Signature:=Copy(aTypeName,MarkerPosition+3,Length(aTypeName));
 OpeningPosition:=Pos('(',Signature);
 ClosingPosition:=0;
 if OpeningPosition>0 then begin
  ClosingPosition:=Length(Signature);
  while (ClosingPosition>OpeningPosition) and (Signature[ClosingPosition]<>')') do begin
   dec(ClosingPosition);
  end;
 end;
 if ClosingPosition>OpeningPosition then begin
  Signature:=TrimText(Copy(Signature,OpeningPosition+1,(ClosingPosition-OpeningPosition)-1));
 end else begin
  Signature:='';
 end;

 // The C headers give these parameters no names, so they are numbered.
 ParameterList:='';
 CountParameters:=0;
 while Length(Signature)>0 do begin
  CommaPosition:=Pos(',',Signature);
  if CommaPosition>0 then begin
   ParameterTypeName:=TrimText(Copy(Signature,1,CommaPosition-1));
   Signature:=TrimText(Copy(Signature,CommaPosition+1,Length(Signature)));
  end else begin
   ParameterTypeName:=TrimText(Signature);
   Signature:='';
  end;
  if (Length(ParameterTypeName)>0) and (ParameterTypeName<>'void') then begin
   inc(CountParameters);
   if CountParameters>1 then begin
    ParameterList:=ParameterList+';';
   end;
   ParameterList:=ParameterList+'const aParameter'+IntToStr(CountParameters)+':'+TranslateTypeName(ParameterTypeName);
  end;
 end;

 if ReturnTypeName='void' then begin
  if CountParameters>0 then begin
   result:='procedure('+ParameterList+'); cdecl;';
  end else begin
   result:='procedure; cdecl;';
  end;
 end else begin
  if CountParameters>0 then begin
   result:='function('+ParameterList+'):'+TranslateTypeName(ReturnTypeName)+'; cdecl;';
  end else begin
   result:='function:'+TranslateTypeName(ReturnTypeName)+'; cdecl;';
  end;
 end;

end;

// Emits the function pointer type definitions. They have to come after the records, because their
// parameters point at callback records.
procedure EmitFunctionPointerTypeDefinitions;
var Index:TPasJSONSizeInt;
    Item:TTypeDefinitionItem;
begin
 for Index:=0 to length(TypeDefinitionItems)-1 do begin
  Item:=TypeDefinitionItems[Index];
  if not (IsInTextArray(Item.Name,BaseTypeDefinitionNames) or (Pos('(*)',Item.TypeName)=0)) then begin
   EmitPointerTypes(Item.Name,true);
   Emit('     T'+Item.Name+'='+BuildFunctionPointerTypeBody(Item.TypeName));
   EmitEmptyLine;
  end;
 end;
end;

// C enumerations are int sized. Pascal enumerations cannot hold the 16 negative values or the three
// duplicate values that the Steamworks enumerations use, and Delphi rejects negative ordinals
// outright, so each enumeration becomes an integer alias plus a block of untyped constants.
procedure EmitEnumerations;
var EnumerationIndex,ValueIndex:TPasJSONSizeInt;
    Item:TEnumerationItem;
begin
 for EnumerationIndex:=0 to length(EnumerationItems)-1 do begin

  Item:=EnumerationItems[EnumerationIndex];

  Emit('type { T'+Item.Name+' }');
  Emit('     PP'+Item.Name+'=^P'+Item.Name+';');
  Emit('     P'+Item.Name+'=^T'+Item.Name+';');
  Emit('     T'+Item.Name+'=TSteamInt32;');
  EmitEmptyLine;

  if length(Item.Values)>0 then begin
   for ValueIndex:=0 to length(Item.Values)-1 do begin
    if ValueIndex=0 then begin
     Emit('const '+Item.Values[ValueIndex].Name+'='+TranslateConstantValue(Item.Values[ValueIndex].Value,'')+';');
    end else begin
     Emit('      '+Item.Values[ValueIndex].Name+'='+TranslateConstantValue(Item.Values[ValueIndex].Value,'')+';');
    end;
   end;
   EmitEmptyLine;
  end;

 end;
end;

// The interfaces themselves are opaque C++ objects. Only pointers to them ever cross the boundary.
procedure EmitInterfaceHandleTypes;
var Index:TPasJSONSizeInt;
begin
 for Index:=0 to length(InterfaceItems)-1 do begin
  Emit('type { T'+InterfaceItems[Index].Name+' }');
  Emit('     PP'+InterfaceItems[Index].Name+'=^P'+InterfaceItems[Index].Name+';');
  Emit('     P'+InterfaceItems[Index].Name+'=^T'+InterfaceItems[Index].Name+';');
  Emit('     T'+InterfaceItems[Index].Name+'=record');
  Emit('     end;');
  EmitEmptyLine;
 end;
end;

procedure EmitStruct(const aName:TText); forward;

// Walks the field types of a record and emits everything it references first, so that the generated
// type block never forward references a record.
procedure EmitStructDependencies(const aStructIndex:TPasJSONSizeInt);
var FieldIndex,DependencyIndex:TPasJSONSizeInt;
    TypeName,ElementTypeName,ElementCount:TText;
    Dependencies:TTexts;
begin

 if StructItems[aStructIndex].IsHandWritten then begin
  Dependencies:=GetHandWrittenStructDependencies(StructItems[aStructIndex].Name);
  for DependencyIndex:=0 to length(Dependencies)-1 do begin
   EmitStruct(Dependencies[DependencyIndex]);
  end;
 end;

 for FieldIndex:=0 to length(StructItems[aStructIndex].Fields)-1 do begin
  TypeName:=StructItems[aStructIndex].Fields[FieldIndex].TypeName;
  if SplitArrayType(TypeName,ElementTypeName,ElementCount) then begin
   TypeName:=ElementTypeName;
  end;
  // Only a by value field creates an ordering requirement, a pointer field does not.
  if (Pos('*',TypeName)=0) and (Pos('&',TypeName)=0) then begin
   if FindStructIndex(TypeName)>=0 then begin
    EmitStruct(TypeName);
   end;
  end;
 end;

end;

// A record field can be an inline function pointer, which needs a named procedural type of its own.
// That type is emitted between the record's pointer types and the record itself, because it may well
// point back at the very record it belongs to.
function BuildFieldFunctionPointerTypeName(const aStructName,aFieldName:TText):TText;
begin
 result:='T'+aStructName+'_'+aFieldName;
end;

procedure EmitStruct(const aName:TText);
var StructIndex,FieldIndex:TPasJSONSizeInt;
    FieldTypeName:TText;
begin

 StructIndex:=FindStructIndex(aName);
 if StructIndex<0 then begin
  exit;
 end;
 if StructItems[StructIndex].IsEmitted or StructItems[StructIndex].IsInProgress then begin
  exit;
 end;

 StructItems[StructIndex].IsInProgress:=true;
 EmitStructDependencies(StructIndex);
 StructItems[StructIndex].IsInProgress:=false;
 StructItems[StructIndex].IsEmitted:=true;

 if StructItems[StructIndex].IsHandWritten then begin
  EmitHandWrittenStruct(aName);
  exit;
 end;

 SetPackingMode(StructItems[StructIndex].PackingMode);

 Emit('type { T'+aName+' }');
 Emit('     PP'+aName+'=^P'+aName+';');
 Emit('     P'+aName+'=^T'+aName+';');

 for FieldIndex:=0 to length(StructItems[StructIndex].Fields)-1 do begin
  if Pos('(*)',StructItems[StructIndex].Fields[FieldIndex].TypeName)>0 then begin
   Emit('     '+BuildFieldFunctionPointerTypeName(aName,StructItems[StructIndex].Fields[FieldIndex].Name)+'='+
        BuildFunctionPointerTypeBody(StructItems[StructIndex].Fields[FieldIndex].TypeName));
  end;
 end;

 Emit('     T'+aName+'='+GetRecordKeyword(StructItems[StructIndex].PackingMode));
 if length(StructItems[StructIndex].Fields)>0 then begin
  for FieldIndex:=0 to length(StructItems[StructIndex].Fields)-1 do begin
   if Pos('(*)',StructItems[StructIndex].Fields[FieldIndex].TypeName)>0 then begin
    FieldTypeName:=BuildFieldFunctionPointerTypeName(aName,StructItems[StructIndex].Fields[FieldIndex].Name);
   end else begin
    FieldTypeName:=TranslateFieldTypeName(StructItems[StructIndex].Fields[FieldIndex].TypeName);
   end;
   Emit('      '+StructItems[StructIndex].Fields[FieldIndex].Name+':'+FieldTypeName+';');
  end;
 end else begin
  Emit('      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size');
 end;
 Emit('     end;');
 EmitEmptyLine;

 // Callback records carry the identifier that the dispatcher matches against.
 if StructItems[StructIndex].IsCallback and (StructItems[StructIndex].CallbackID>=0) then begin
  Emit('const '+aName+'_k_iCallback='+IntToStr(StructItems[StructIndex].CallbackID)+';');
  EmitEmptyLine;
 end;

end;

procedure EmitStructs;
var Index:TPasJSONSizeInt;
begin

 // CSteamID and CGameID are not part of steam_api.json at all, but records and methods refer to
 // them, so they go first.
 EmitHandWrittenSteamIDAndGameID;

 for Index:=0 to length(StructItems)-1 do begin
  if not StructItems[Index].IsCallback then begin
   EmitStruct(StructItems[Index].Name);
  end;
 end;

 for Index:=0 to length(StructItems)-1 do begin
  EmitStruct(StructItems[Index].Name);
 end;

 SetPackingMode(PackingModePlatform);

end;

procedure EmitConstants;
var Index:TPasJSONSizeInt;
    IsFirst:boolean;
begin
 IsFirst:=true;
 for Index:=0 to length(ConstantItems)-1 do begin
  if IsFirst then begin
   IsFirst:=false;
   Emit('const '+ConstantItems[Index].Name+'='+TranslateConstantValue(ConstantItems[Index].Value,ConstantItems[Index].TypeName)+';');
  end else begin
   Emit('      '+ConstantItems[Index].Name+'='+TranslateConstantValue(ConstantItems[Index].Value,ConstantItems[Index].TypeName)+';');
  end;
 end;
 if not IsFirst then begin
  EmitEmptyLine;
 end;
end;

// The version strings are what SteamInternal_SteamAPI_Init checks the loaded library against.
procedure EmitInterfaceVersionConstants;
var Index:TPasJSONSizeInt;
    IsFirst:boolean;
begin
 IsFirst:=true;
 for Index:=0 to length(InterfaceItems)-1 do begin
  if Length(InterfaceItems[Index].VersionString)>0 then begin
   if IsFirst then begin
    IsFirst:=false;
    Emit('const '+InterfaceItems[Index].Name+'_INTERFACE_VERSION='''+InterfaceItems[Index].VersionString+''';');
   end else begin
    Emit('      '+InterfaceItems[Index].Name+'_INTERFACE_VERSION='''+InterfaceItems[Index].VersionString+''';');
   end;
  end;
 end;
 if not IsFirst then begin
  EmitEmptyLine;
 end;
end;

// Builds the Pascal declaration of one flat entry point. aSelfTypeName is empty for the accessors,
// which take no interface pointer.
function BuildFunctionPointerDeclaration(const aMethod:TMethodItem;const aSelfTypeName:TText):TText;
var ParameterIndex:TPasJSONSizeInt;
    ParameterList,ReturnTypeName:TText;
begin

 ParameterList:='';
 if Length(aSelfTypeName)>0 then begin
  ParameterList:='const aSelf:'+aSelfTypeName;
 end;

 for ParameterIndex:=0 to length(aMethod.Parameters)-1 do begin
  if Length(ParameterList)>0 then begin
   ParameterList:=ParameterList+';';
  end;
  ParameterList:=ParameterList+'const '+TranslateParameterName(aMethod.Parameters[ParameterIndex].Name)+':'+
                 TranslateTypeName(aMethod.Parameters[ParameterIndex].TypeName);
 end;

 ReturnTypeName:=TranslateTypeName(aMethod.ReturnTypeName);
 if Length(ReturnTypeName)=0 then begin
  if Length(ParameterList)>0 then begin
   result:='procedure('+ParameterList+'); cdecl;';
  end else begin
   result:='procedure; cdecl;';
  end;
 end else begin
  if Length(ParameterList)>0 then begin
   result:='function('+ParameterList+'):'+ReturnTypeName+'; cdecl;';
  end else begin
   result:='function:'+ReturnTypeName+'; cdecl;';
  end;
 end;

end;

procedure EmitFunctionPointers;
var InterfaceIndex,MethodIndex,StructIndex,AccessorIndex:TPasJSONSizeInt;
    IsFirst:boolean;
    Prefix,Declaration:TText;

 procedure EmitOne(const aName,aDeclaration:TText);
 begin
  if not RegisterFunctionName(aName) then begin
   exit;
  end;
  if IsFirst then begin
   IsFirst:=false;
   Prefix:='var ';
  end else begin
   Prefix:='    ';
  end;
  Emit(Prefix+aName+':'+aDeclaration);
 end;

begin

 IsFirst:=true;

 for InterfaceIndex:=0 to length(InterfaceItems)-1 do begin

  Emit('// '+InterfaceItems[InterfaceIndex].Name);

  for AccessorIndex:=0 to length(InterfaceItems[InterfaceIndex].Accessors)-1 do begin
   Declaration:='function:P'+InterfaceItems[InterfaceIndex].Name+'; cdecl;';
   EmitOne(InterfaceItems[InterfaceIndex].Accessors[AccessorIndex].FlatName,Declaration);
  end;

  for MethodIndex:=0 to length(InterfaceItems[InterfaceIndex].Methods)-1 do begin
   Declaration:=BuildFunctionPointerDeclaration(InterfaceItems[InterfaceIndex].Methods[MethodIndex],
                                                'P'+InterfaceItems[InterfaceIndex].Name);
   EmitOne(InterfaceItems[InterfaceIndex].Methods[MethodIndex].FlatName,Declaration);
  end;

  EmitEmptyLine;

 end;

 for StructIndex:=0 to length(StructItems)-1 do begin
  if length(StructItems[StructIndex].Methods)>0 then begin
   Emit('// '+StructItems[StructIndex].Name);
   for MethodIndex:=0 to length(StructItems[StructIndex].Methods)-1 do begin
    Declaration:=BuildFunctionPointerDeclaration(StructItems[StructIndex].Methods[MethodIndex],
                                                 'P'+StructItems[StructIndex].Name);
    EmitOne(StructItems[StructIndex].Methods[MethodIndex].FlatName,Declaration);
   end;
   EmitEmptyLine;
  end;
 end;

end;

procedure EmitLoaderDeclarations;
begin
 // The byte array storage of TCSteamID and TCGameID keeps their alignment at one byte, so these
 // convert between that storage and the 64 bit value that the flat API passes around.
 Emit('function SteamIDToUInt64(const aSteamID:TCSteamID):TSteamUInt64;');
 Emit('function UInt64ToSteamID(const aValue:TSteamUInt64):TCSteamID;');
 Emit('function GameIDToUInt64(const aGameID:TCGameID):TSteamUInt64;');
 Emit('function UInt64ToGameID(const aValue:TSteamUInt64):TCGameID;');
 EmitEmptyLine;
 Emit('var SteamworksLibraryHandle:TSteamPointer=nil;');
 EmitEmptyLine;
 Emit('function SteamworksLoadLibrary(const aLibraryName:string):TSteamPointer;');
 Emit('function SteamworksFreeLibrary(const aLibraryHandle:TSteamPointer):boolean;');
 Emit('function SteamworksGetProcAddress(const aLibraryHandle:TSteamPointer;const aProcName:string):TSteamPointer;');
 EmitEmptyLine;
 Emit('function LoadSteamworksLibrary(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):boolean;');
 Emit('procedure UnloadSteamworksLibrary;');
 EmitEmptyLine;
 Emit('implementation');
 EmitEmptyLine;
end;

procedure EmitLoaderImplementation;
var InterfaceIndex,MethodIndex,StructIndex,AccessorIndex:TPasJSONSizeInt;

 procedure EmitLoadEntry(const aName:TText);
 begin
  Emit(' LoadEntryPoint('+aName+','''+aName+''');');
 end;

begin

 // Move rather than a pointer cast, because the byte array carries no alignment guarantee.
 Emit('function SteamIDToUInt64(const aSteamID:TCSteamID):TSteamUInt64;');
 Emit('begin');
 Emit(' Move(aSteamID.m_rgubSteamID[0],result,SizeOf(TSteamUInt64));');
 Emit('end;');
 EmitEmptyLine;

 Emit('function UInt64ToSteamID(const aValue:TSteamUInt64):TCSteamID;');
 Emit('begin');
 Emit(' Move(aValue,result.m_rgubSteamID[0],SizeOf(TSteamUInt64));');
 Emit('end;');
 EmitEmptyLine;

 Emit('function GameIDToUInt64(const aGameID:TCGameID):TSteamUInt64;');
 Emit('begin');
 Emit(' Move(aGameID.m_rgubGameID[0],result,SizeOf(TSteamUInt64));');
 Emit('end;');
 EmitEmptyLine;

 Emit('function UInt64ToGameID(const aValue:TSteamUInt64):TCGameID;');
 Emit('begin');
 Emit(' Move(aValue,result.m_rgubGameID[0],SizeOf(TSteamUInt64));');
 Emit('end;');
 EmitEmptyLine;

 Emit('function SteamworksLoadLibrary(const aLibraryName:string):TSteamPointer;');
 Emit('begin');
 Emit('{$ifdef Windows}');
 Emit(' result:={%H-}TSteamPointer(LoadLibrary(PChar(aLibraryName)));');
 Emit('{$else}');
 Emit('{$ifdef Unix}');
 Emit(' result:=dlopen(PChar(aLibraryName),RTLD_NOW or RTLD_GLOBAL);');
 Emit('{$else}');
 Emit(' result:=nil;');
 Emit('{$endif}');
 Emit('{$endif}');
 Emit('end;');
 EmitEmptyLine;

 Emit('function SteamworksFreeLibrary(const aLibraryHandle:TSteamPointer):boolean;');
 Emit('begin');
 Emit(' result:=assigned(aLibraryHandle);');
 Emit(' if result then begin');
 Emit('{$ifdef Windows}');
 Emit('  result:=FreeLibrary({%H-}HMODULE(aLibraryHandle));');
 Emit('{$else}');
 Emit('{$ifdef Unix}');
 Emit('  result:=dlclose(aLibraryHandle)=0;');
 Emit('{$else}');
 Emit('  result:=false;');
 Emit('{$endif}');
 Emit('{$endif}');
 Emit(' end;');
 Emit('end;');
 EmitEmptyLine;

 Emit('function SteamworksGetProcAddress(const aLibraryHandle:TSteamPointer;const aProcName:string):TSteamPointer;');
 Emit('begin');
 Emit('{$ifdef Windows}');
 Emit(' result:=GetProcAddress({%H-}HMODULE(aLibraryHandle),PChar(aProcName));');
 Emit('{$else}');
 Emit('{$ifdef Unix}');
 Emit(' result:=dlsym(aLibraryHandle,PChar(aProcName));');
 Emit('{$else}');
 Emit(' result:=nil;');
 Emit('{$endif}');
 Emit('{$endif}');
 Emit('end;');
 EmitEmptyLine;

 // A single untyped helper keeps the loader body one line per entry point instead of one cast per
 // entry point.
 Emit('function LoadSteamworksLibrary(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):boolean;');
 Emit('var CountMissingEntryPoints:TSteamInt32;');
 Emit(' // The target is untyped so that one helper can fill in every differently typed entry point.');
 Emit(' procedure LoadEntryPoint(out aTarget;const aName:string);');
 Emit(' begin');
 Emit('  TSteamPointer(aTarget):=SteamworksGetProcAddress(SteamworksLibraryHandle,aName);');
 Emit('  if not assigned(TSteamPointer(aTarget)) then begin');
 Emit('   inc(CountMissingEntryPoints);');
 Emit('  end;');
 Emit(' end;');
 Emit('begin');
 EmitEmptyLine;
 Emit(' if assigned(SteamworksLibraryHandle) then begin');
 Emit('  result:=true;');
 Emit('  exit;');
 Emit(' end;');
 EmitEmptyLine;
 Emit(' SteamworksLibraryHandle:=SteamworksLoadLibrary(aLibraryName);');
 Emit(' if not assigned(SteamworksLibraryHandle) then begin');
 Emit('  result:=false;');
 Emit('  exit;');
 Emit(' end;');
 EmitEmptyLine;
 Emit(' CountMissingEntryPoints:=0;');
 EmitEmptyLine;

 for InterfaceIndex:=0 to length(InterfaceItems)-1 do begin
  Emit(' // '+InterfaceItems[InterfaceIndex].Name);
  for AccessorIndex:=0 to length(InterfaceItems[InterfaceIndex].Accessors)-1 do begin
   EmitLoadEntry(InterfaceItems[InterfaceIndex].Accessors[AccessorIndex].FlatName);
  end;
  for MethodIndex:=0 to length(InterfaceItems[InterfaceIndex].Methods)-1 do begin
   EmitLoadEntry(InterfaceItems[InterfaceIndex].Methods[MethodIndex].FlatName);
  end;
  EmitEmptyLine;
 end;

 for StructIndex:=0 to length(StructItems)-1 do begin
  if length(StructItems[StructIndex].Methods)>0 then begin
   Emit(' // '+StructItems[StructIndex].Name);
   for MethodIndex:=0 to length(StructItems[StructIndex].Methods)-1 do begin
    EmitLoadEntry(StructItems[StructIndex].Methods[MethodIndex].FlatName);
   end;
   EmitEmptyLine;
  end;
 end;

 Emit(' // A missing entry point means the loaded library is older than these bindings.');
 Emit(' result:=CountMissingEntryPoints=0;');
 Emit(' if not result then begin');
 Emit('  UnloadSteamworksLibrary;');
 Emit(' end;');
 EmitEmptyLine;
 Emit('end;');
 EmitEmptyLine;

 Emit('procedure UnloadSteamworksLibrary;');
 Emit('begin');
 Emit(' if assigned(SteamworksLibraryHandle) then begin');
 Emit('  SteamworksFreeLibrary(SteamworksLibraryHandle);');
 Emit('  SteamworksLibraryHandle:=nil;');
 Emit(' end;');
 Emit('end;');
 EmitEmptyLine;

end;

procedure EmitUnitEpilogue;
begin
 Emit('end.');
end;

{ Main }

var InputFileName,OutputFileName:string;
    InputStream:TMemoryStream;
    JSONRootItem:TPasJSONItem;
    JSONRootObject:TPasJSONItemObject;
    CountMethods,CountAccessors,Index:TPasJSONSizeInt;
begin

 if ParamCount>=1 then begin
  InputFileName:=ParamStr(1);
 end else begin
  InputFileName:='../ref/steamworks/sdk/public/steam/steam_api.json';
 end;

 if ParamCount>=2 then begin
  OutputFileName:=ParamStr(2);
 end else begin
  OutputFileName:=UnitName+'.pas';
 end;

 if not FileExists(InputFileName) then begin
  WriteLn('Error: "',InputFileName,'" not found');
  WriteLn('Usage: steamapi2pas [steam_api.json] [',UnitName,'.pas]');
  Halt(1);
 end;

 WriteLn('Reading "',InputFileName,'" . . .');

 JSONRootItem:=nil;
 InputStream:=TMemoryStream.Create;
 try

  InputStream.LoadFromFile(InputFileName);
  InputStream.Seek(0,soBeginning);
  JSONRootItem:=TPasJSON.Parse(InputStream);

  if not (assigned(JSONRootItem) and (JSONRootItem is TPasJSONItemObject)) then begin
   WriteLn('Error: "',InputFileName,'" is not a JSON object');
   Halt(1);
  end;

  JSONRootObject:=JSONRootItem as TPasJSONItemObject;

  ReadEnumerations(JSONRootObject);
  ReadTypeDefinitions(JSONRootObject);
  ReadConstants(JSONRootObject);
  ReadInterfaces(JSONRootObject);
  ReadStructs(JSONRootObject);

  CountMethods:=0;
  CountAccessors:=0;
  for Index:=0 to length(InterfaceItems)-1 do begin
   inc(CountMethods,length(InterfaceItems[Index].Methods));
   inc(CountAccessors,length(InterfaceItems[Index].Accessors));
  end;
  for Index:=0 to length(StructItems)-1 do begin
   inc(CountMethods,length(StructItems[Index].Methods));
  end;

  WriteLn(length(EnumerationItems),' enumerations, ',
          length(TypeDefinitionItems),' type definitions, ',
          length(ConstantItems),' constants, ',
          length(StructItems),' records, ',
          length(InterfaceItems),' interfaces, ',
          CountMethods,' methods, ',
          CountAccessors,' versioned accessors');

  WriteLn('Generating "',OutputFileName,'" . . .');

  EmittedFunctionNames:=nil;
  CountEmittedFunctionNames:=0;
  OutputStream:=TMemoryStream.Create;
  try

   EmitFileHeader;
   EmitUnitPrologue;
   EmitBaseTypes;
   EmitExtraTypes;
   EmitScalarTypeDefinitions;
   EmitArrayTypeDefinitions;
   EmitEnumerations;
   EmitInterfaceHandleTypes;
   EmitStructs;
   EmitFunctionPointerTypeDefinitions;
   EmitConstants;
   EmitInterfaceVersionConstants;
   EmitFunctionPointers;
   EmitLoaderDeclarations;
   EmitLoaderImplementation;
   EmitUnitEpilogue;

   OutputStream.Seek(0,soBeginning);
   OutputStream.SaveToFile(OutputFileName);

   WriteLn(OutputStream.Size,' bytes, ',CountEmittedFunctionNames,' entry points');

  finally
   FreeAndNil(OutputStream);
  end;

 finally
  if assigned(JSONRootItem) then begin
   FreeAndNil(JSONRootItem);
  end;
  FreeAndNil(InputStream);
 end;

 WriteLn('Done.');

end.
