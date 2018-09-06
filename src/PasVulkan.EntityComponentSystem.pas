(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2018, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.EntityComponentSystem;
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

uses {$ifdef Windows}Windows,{$endif}SysUtils,Classes,Math,Variants,TypInfo,
     PasMP,
     PUCU,
     PasDblStrUtils,
     PasJSON,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Base64,
     PasVulkan.Collections;

type TpvEntityComponentSystem=class
      public
       type ESystemCircularDependency=class(Exception);

            ESystemSerialization=class(Exception);

            ESystemUnserialization=class(Exception);

            EDuplicateComponentInEntity=class(Exception);

            PComponentID=^TComponentID;
            TComponentID=type TpvSizeUInt;

            TComponentIDDynamicArray=array of TComponentID;

            PEntityID=^TEntityID;
            TEntityID=type TpvUInt32;

            TEntityIDDynamicArray=array of TEntityID;

            TEntityIDHelper=record helper for TEntityID
             private
              const IndexBits=24; // when all these bits set, then => -1
                    GenerationBits=8;
                    InverseIndexBits=32-IndexBits;
                    IndexBitsMinusOne=TpvUInt32(IndexBits-1);
                    IndexSignBitMask=TpvUInt32(1 shl IndexBitsMinusOne);
                    IndexMask=TpvUInt32((TpvUInt32(1) shl IndexBits)-1);
                    GenerationMask=TpvUInt32(not IndexMask);
              function GetIndex:TpvInt32; inline;
              procedure SetIndex(const aIndex:TpvInt32); inline;
              function GetGeneration:TpvUInt8; inline;
              procedure SetGeneration(const aGeneration:TpvUInt8); inline;
             public
              property Index:TpvInt32 read GetIndex write SetIndex;
              property Generation:TpvUInt8 read GetGeneration write SetGeneration;
            end;

            PWorldID=^TWorldID;
            TWorldID=type TpvInt32;

            PEventID=^TEventID;
            TEventID=type TpvInt32;

            ERegisteredComponentType=class(Exception);

            TRegisteredComponentType=class
             public
              type TPath=array of TpvUTF8String;
                   TField=record
                    public
                     type TElementType=
                           (
                            EntityID,
                            Enumeration,
                            Flags,
                            Boolean,
                            SignedInteger,
                            UnsignedInteger,
                            FloatingPoint,
                            LengthPrefixedString,
                            ZeroTerminatedString,
                            Blob
                           );
                          PElementType=^TElementType;
                          TEnumerationOrFlag=record
                           Value:TpvUInt64;
                           Name:TpvUTF8String;
                           DisplayName:TpvUTF8String;
                           constructor Create(const aValue:TpvUInt64;
                                              const aName:TpvUTF8String;
                                              const aDisplayName:TpvUTF8String);
                          end;
                          TEnumerationsOrFlags=array of TEnumerationOrFlag;
                    public
                     Name:TpvUTF8String;
                     DisplayName:TpvUTF8String;
                     ElementType:TElementType;
                     ElementSize:TpvSizeInt;
                     ElementCount:TpvSizeInt;
                     Offset:TpvSizeInt;
                     Size:TpvSizeInt;
                     EnumerationsOrFlags:TEnumerationsOrFlags;
                   end;
                   PField=^TField;
                   TFields=array of TField;
             private
              fID:TpvSizeInt;
              fName:TpvUTF8String;
              fDisplayName:TpvUTF8String;
              fPath:TPath;
              fSize:TpvSizeInt;
              fFields:TFields;
              fCountFields:TpvSizeInt;
              fDefault:TpvUInt8DynamicArray;
              fEditorWidget:TpvPointer;
             public
              constructor Create(const aName:TpvUTF8String;
                                 const aDisplayName:TpvUTF8String;
                                 const aPath:array of TpvUTF8String;
                                 const aSize:TpvSizeInt;
                                 const aDefault:TpvPointer); reintroduce;
              destructor Destroy; override;
              class function GetSetOrdValue(const Info:PTypeInfo;const SetParam):TpvUInt64; static;
              procedure Add(const aName:TpvUTF8String;
                            const aDisplayName:TpvUTF8String;
                            const aElementType:TField.TElementType;
                            const aElementSize:TpvSizeInt;
                            const aElementCount:TpvSizeInt;
                            const aOffset:TpvSizeInt;
                            const aSize:TpvSizeInt;
                            const aEnumerationsOrFlags:array of TField.TEnumerationOrFlag);
              procedure Finish;
              function SerializeToJSON(const aData:TpvPointer):TPasJSONItemObject;
              procedure UnserializeFromJSON(const aJSON:TPasJSONItem;const aData:TpvPointer);
              property Fields:TFields read fFields;
              property EditorWidget:TpvPointer read fEditorWidget write fEditorWidget;
              property Path:TPath read fPath;
             published
              property ID:TpvSizeInt read fID;
              property Size:TpvSizeInt read fSize;
              property Default:TpvUInt8DynamicArray read fDefault;
            end;

            TRegisteredComponentTypeList=TpvObjectGenericList<TRegisteredComponentType>;

            TComponent=class
             public
              type TIndexMapArray=array of TpvSizeInt;
                   TUsedBitmap=array of TpvUInt32;
                   TPointers=array of TpvPointer;
             private
              fRegisteredComponentType:TRegisteredComponentType;
              fComponentPoolIndexToEntityIndex:TIndexMapArray;
              fEntityIndexToComponentPoolIndex:TIndexMapArray;
              fUsedBitmap:TUsedBitmap;
              fSize:TpvSizeInt;
              fPoolUnaligned:TpvPointer;
              fPool:TpvPointer;
              fPoolSize:TpvSizeInt;
              fCountPoolItems:TpvSizeInt;
              fCapacity:TpvSizeInt;
              fPoolIndexCounter:TpvSizeInt;
              fMaxEntityIndex:TpvSizeInt;
              fCountFrees:TpvSizeInt;
              fNeedToDefragment:boolean;
              fPointers:TPointers;
              fDataPointer:TpvPointer;
              procedure FinalizeComponentByPoolIndex(const aPoolIndex:TpvSizeInt);
              function GetEntityIndexByPoolIndex(const aPoolIndex:TpvSizeInt):TpvSizeInt; inline;
              function GetComponentByPoolIndex(const aPoolIndex:TpvSizeInt):TpvPointer; inline;
              function GetComponentByEntityIndex(const aEntityIndex:TpvSizeInt):TpvPointer; inline;
              procedure SetMaxEntities(const aCount:TpvSizeInt);
             public
              constructor Create(const aRegisteredComponentType:TRegisteredComponentType); reintroduce;
              destructor Destroy; override;
              procedure Defragment;
              procedure DefragmentIfNeeded;
              function IsComponentInEntityIndex(const aEntityIndex:TpvSizeInt):boolean; inline;
              function GetComponentPoolIndexForEntityIndex(const aEntityIndex:TpvSizeInt):TpvSizeInt; inline;
              function AllocateComponentForEntityIndex(const aEntityIndex:TpvSizeInt):boolean;
              function FreeComponentFromEntityIndex(const aEntityIndex:TpvSizeInt):boolean;
             public
              property Pool:TpvPointer read fPool;
              property PoolSize:TpvSizeInt read fPoolSize;
              property CountPoolItems:TpvSizeInt read fCountPoolItems;
              property EntityIndexByPoolIndex[const aPoolIndex:TpvSizeInt]:TpvSizeInt read GetEntityIndexByPoolIndex;
              property ComponentByPoolIndex[const aPoolIndex:TpvSizeInt]:pointer read GetComponentByPoolIndex;
              property ComponentByEntityIndex[const aEntityIndex:TpvSizeInt]:pointer read GetComponentByEntityIndex;
              property Pointers:TPointers read fPointers;
              property DataPointer:pointer read fDataPointer;
             published
              property RegisteredComponentType:TRegisteredComponentType read fRegisteredComponentType;
            end;

            PEntity=^TEntity;
            TEntity=record
             public
              type TFlag=
                    (
                     Used,
                     Active,
                     Killed
                    );
                    TFlags=set of TFlag;
             private
              fID:TEntityID;
              fFlags:TFlags;
              function GetActive:boolean; inline;
              procedure SetActive(const aActive:boolean); inline;
             public
              property ID:TEntityID read fID write fID;
              property Flags:TFlags read fFlags write fFlags;
              property Active:boolean read GetActive write SetActive;
            end;

            TpvEntities=array of TEntity;

            TpvWorld=class
             public
              type TEntityIndexFreeList=TpvGenericList<TpvSizeInt>;
                   TEntityGenerationList=TpvDynamicArray<TpvUInt8>;
                   TUsedBitmap=array of TpvUInt32;
             private
              fEntities:TpvEntities;
              fEntityIndexFreeList:TEntityIndexFreeList;
              fEntityGenerationList:TEntityGenerationList;
              fEntityUsedBitmap:TUsedBitmap;
              fEntityIndexCounter:TpvSizeInt;
              fMaxEntityIndex:TpvSizeInt;
             public

            end;

     end;

var RegisteredComponentTypeList:TpvEntityComponentSystem.TRegisteredComponentTypeList=nil;

procedure InitializeEntityComponentSystemGlobals;

implementation

uses PasVulkan.Components.Name,
     PasVulkan.Components.Parent,
     PasVulkan.Components.Renderer,
     PasVulkan.Components.SortKey,
     PasVulkan.Components.Transform;

{ TpvEntityComponentSystem.TpvEntityIDHelper }

function TpvEntityComponentSystem.TEntityIDHelper.GetIndex:TpvInt32;
begin
 result:=self and IndexMask;
 result:=result or (-(ord(result=IndexMask) and 1));
end;

procedure TpvEntityComponentSystem.TEntityIDHelper.SetIndex(const aIndex:TpvInt32);
begin
 self:=(self and GenerationMask) or (TpvUInt32(aIndex) and IndexMask);
end;

function TpvEntityComponentSystem.TEntityIDHelper.GetGeneration:TpvUInt8;
begin
 result:=self shr IndexBits;
end;

procedure TpvEntityComponentSystem.TEntityIDHelper.SetGeneration(const aGeneration:TpvUInt8);
begin
 self:=(self and IndexMask) or (aGeneration shl IndexBits);
end;

{ TpvEntityComponentSystem.TpvRegisteredComponentType.TField.TEnumerationOrFlag }

constructor TpvEntityComponentSystem.TRegisteredComponentType.TField.TEnumerationOrFlag.Create(const aValue:TpvUInt64;
                                                                                               const aName:TpvUTF8String;
                                                                                               const aDisplayName:TpvUTF8String);
begin
 Value:=aValue;
 Name:=aName;
 DisplayName:=aDisplayName;
end;

{ TpvRegisteredComponentType }

constructor TpvEntityComponentSystem.TRegisteredComponentType.Create(const aName:TpvUTF8String;
                                                                     const aDisplayName:TpvUTF8String;
                                                                     const aPath:array of TpvUTF8String;
                                                                     const aSize:TpvSizeInt;
                                                                     const aDefault:TpvPointer);
var Index:TpvSizeInt;
begin
 inherited Create;
 InitializeEntityComponentSystemGlobals;
 fID:=RegisteredComponentTypeList.Add(self);
 fName:=aName;
 fDisplayName:=aDisplayName;
 SetLength(fPath,length(aPath));
 for Index:=0 to length(aPath)-1 do begin
  fPath[Index]:=aPath[Index];
 end;
 fSize:=aSize;
 fFields:=nil;
 fCountFields:=0;
 fEditorWidget:=nil;
 SetLength(fDefault,fSize);
 if assigned(aDefault) then begin
  Move(aDefault^,fDefault[0],fSize);
 end else begin
  FillChar(fDefault[0],fSize,#0);
 end;
end;

destructor TpvEntityComponentSystem.TRegisteredComponentType.Destroy;
begin
 fFields:=nil;
 fDefault:=nil;
 inherited Destroy;
end;

class function TpvEntityComponentSystem.TRegisteredComponentType.GetSetOrdValue(const Info:PTypeInfo;const SetParam):TpvUInt64;
begin
 result:=0;
 case GetTypeData(Info)^.OrdType of
  otSByte,otUByte:begin
   result:=TpvUInt8(SetParam);
  end;
  otSWord,otUWord:begin
   result:=TpvUInt16(SetParam);
  end;
  otSLong,otULong:begin
   result:=TpvUInt32(SetParam);
  end;
 end;
end;

procedure TpvEntityComponentSystem.TRegisteredComponentType.Add(const aName:TpvUTF8String;
                                                                const aDisplayName:TpvUTF8String;
                                                                const aElementType:TField.TElementType;
                                                                const aElementSize:TpvSizeInt;
                                                                const aElementCount:TpvSizeInt;
                                                                const aOffset:TpvSizeInt;
                                                                const aSize:TpvSizeInt;
                                                                const aEnumerationsOrFlags:array of TField.TEnumerationOrFlag);
var Index:TpvSizeInt;
    Field:TRegisteredComponentType.PField;
begin
 Index:=fCountFields;
 inc(fCountFields);
 if length(fFields)<fCountFields then begin
  SetLength(fFields,fCountFields*2);
 end;
 Field:=@fFields[Index];
 Field^.Name:=aName;
 Field^.DisplayName:=aDisplayName;
 Field^.ElementType:=aElementType;
 Field^.Offset:=aOffset;
 Field^.ElementSize:=aElementSize;
 Field^.ElementCount:=aElementCount;
 Field^.Size:=aSize;
 SetLength(Field^.EnumerationsOrFlags,length(aEnumerationsOrFlags));
 for Index:=0 to length(aEnumerationsOrFlags)-1 do begin
  Field^.EnumerationsOrFlags[Index]:=aEnumerationsOrFlags[Index];
 end;
end;

procedure TpvEntityComponentSystem.TRegisteredComponentType.Finish;
begin
 SetLength(fFields,fCountFields);
end;

function TpvEntityComponentSystem.TRegisteredComponentType.SerializeToJSON(const aData:TpvPointer):TPasJSONItemObject;
 function GetElementValue(const aField:PField;
                          const aValueData:TpvPointer):TPasJSONItem;
 var Data:TpvPointer;
     EnumerationFlagIndex:TpvSizeInt;
     SignedInteger:TpvInt64;
     UnsignedInteger:TpvUInt64;
     FloatValue:TpvDouble;
     StringValue:TpvUTF8String;
 begin
  result:=nil;
  Data:=aValueData;
  case aField^.ElementType of
   TRegisteredComponentType.TField.TElementType.EntityID:begin
    case aField^.ElementSize of
     1:begin
      UnsignedInteger:=PpvUInt8(Data)^;
     end;
     2:begin
      UnsignedInteger:=PpvUInt16(Data)^;
     end;
     4:begin
      UnsignedInteger:=PpvUInt32(Data)^;
     end;
     8:begin
      UnsignedInteger:=PpvUInt64(Data)^;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-04-23-58-0000');
     end;
    end;
    if abs(SignedInteger)<TpvInt64($0010000000000000) then begin
     result:=TPasJSONItemNumber.Create(UnsignedInteger);
    end else begin
     result:=TPasJSONItemString.Create(IntToStr(UnsignedInteger));
    end;
   end;
   TRegisteredComponentType.TField.TElementType.Enumeration:begin
    case aField^.ElementSize of
     1:begin
      UnsignedInteger:=PpvUInt8(Data)^;
     end;
     2:begin
      UnsignedInteger:=PpvUInt16(Data)^;
     end;
     4:begin
      UnsignedInteger:=PpvUInt32(Data)^;
     end;
     8:begin
      UnsignedInteger:=PpvUInt64(Data)^;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-04-23-36-0000');
     end;
    end;
    EnumerationFlagIndex:=0;
    while EnumerationFlagIndex<length(aField^.EnumerationsOrFlags) do begin
     if aField^.EnumerationsOrFlags[EnumerationFlagIndex].Value=UnsignedInteger then begin
      result:=TPasJSONItemString.Create(aField^.EnumerationsOrFlags[EnumerationFlagIndex].Name);
      break;
     end;
     inc(EnumerationFlagIndex);
    end;
    if EnumerationFlagIndex>=length(aField^.EnumerationsOrFlags) then begin
     result:=TPasJSONItemString.Create('');
    end;
   end;
   TRegisteredComponentType.TField.TElementType.Flags:begin
    case aField^.ElementSize of
     1:begin
      UnsignedInteger:=PpvUInt8(Data)^;
     end;
     2:begin
      UnsignedInteger:=PpvUInt16(Data)^;
     end;
     4:begin
      UnsignedInteger:=PpvUInt32(Data)^;
     end;
     8:begin
      UnsignedInteger:=PpvUInt64(Data)^;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-04-23-36-0000');
     end;
    end;
    result:=TPasJSONItemArray.Create;
    for EnumerationFlagIndex:=0 to length(aField^.EnumerationsOrFlags)-1 do begin
     if (aField^.EnumerationsOrFlags[EnumerationFlagIndex].Value and UnsignedInteger)<>0 then begin
      TPasJSONItemArray(result).Add(TPasJSONItemString.Create(aField^.EnumerationsOrFlags[EnumerationFlagIndex].Name));
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.Boolean:begin
    case aField^.ElementSize of
     1:begin
      UnsignedInteger:=PpvUInt8(Data)^;
     end;
     2:begin
      UnsignedInteger:=PpvUInt16(Data)^;
     end;
     4:begin
      UnsignedInteger:=PpvUInt32(Data)^;
     end;
     8:begin
      UnsignedInteger:=PpvUInt64(Data)^;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-00-25-0000');
     end;
    end;
    result:=TPasJSONItemBoolean.Create(UnsignedInteger<>0);
   end;
   TRegisteredComponentType.TField.TElementType.SignedInteger:begin
    case aField^.ElementSize of
     1:begin
      SignedInteger:=PpvInt8(Data)^;
     end;
     2:begin
      SignedInteger:=PpvInt16(Data)^;
     end;
     4:begin
      SignedInteger:=PpvInt32(Data)^;
     end;
     8:begin
      SignedInteger:=PpvInt64(Data)^;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-00-15-0001');
     end;
    end;
    if abs(SignedInteger)<TpvUInt64($0010000000000000) then begin
     result:=TPasJSONItemNumber.Create(SignedInteger);
    end else begin
     result:=TPasJSONItemString.Create(IntToStr(SignedInteger));
    end;
   end;
   TRegisteredComponentType.TField.TElementType.UnsignedInteger:begin
    case aField^.ElementSize of
     1:begin
      UnsignedInteger:=PpvUInt8(Data)^;
     end;
     2:begin
      UnsignedInteger:=PpvUInt16(Data)^;
     end;
     4:begin
      UnsignedInteger:=PpvUInt32(Data)^;
     end;
     8:begin
      UnsignedInteger:=PpvUInt64(Data)^;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-00-15-0000');
     end;
    end;
    if UnsignedInteger<TpvUInt64($0010000000000000) then begin
     result:=TPasJSONItemNumber.Create(UnsignedInteger);
    end else begin
     result:=TPasJSONItemString.Create(IntToStr(UnsignedInteger));
    end;
   end;
   TRegisteredComponentType.TField.TElementType.FloatingPoint:begin
    case aField^.ElementSize of
     2:begin
      FloatValue:=PpvHalfFloat(Data)^;
     end;
     4:begin
      FloatValue:=PpvFloat(Data)^;
     end;
     8:begin
      FloatValue:=PpvDouble(Data)^;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-00-22-0000');
     end;
    end;
    result:=TPasJSONItemNumber.Create(FloatValue);
   end;
   TRegisteredComponentType.TField.TElementType.LengthPrefixedString:begin
    UnsignedInteger:=PpvUInt8(Data)^;
    StringValue:='';
    if UnsignedInteger>0 then begin
     SetLength(StringValue,UnsignedInteger);
     Move(PAnsiChar(Data)[1],StringValue[1],UnsignedInteger);
    end;
    result:=TPasJSONItemString.Create(StringValue);
   end;
   TRegisteredComponentType.TField.TElementType.ZeroTerminatedString:begin
    StringValue:=PAnsiChar(Data);
    result:=TPasJSONItemString.Create(StringValue);
   end;
   TRegisteredComponentType.TField.TElementType.Blob:begin
    result:=TPasJSONItemString.Create(TpvBase64.Encode(Data^,aField^.ElementSize));
   end;
   else begin
    raise ERegisteredComponentType.Create('Internal error 2018-09-05-00-11-0000');
   end;
  end;
 end;
 function GetFieldValue(const aField:PField;
                        const aData:TpvPointer):TPasJSONItem;
 var ElementIndex:TpvSizeInt;
 begin
  if aField^.ElementCount>1 then begin
   result:=TPasJSONItemArray.Create;
   for ElementIndex:=0 to aField^.ElementCount-1 do begin
    TPasJSONItemArray(result).Add(GetElementValue(aField,@PpvUInt8Array(aData)^[aField^.Offset+(ElementIndex*aField^.ElementSize)]));
   end;
  end else begin
   result:=GetElementValue(aField,@PpvUInt8Array(aData)^[aField^.Offset]);
  end;
 end;
var FieldIndex:TpvSizeInt;
    Field:PField;
begin
 result:=TPasJSONItemObject.Create;
 try
  for FieldIndex:=0 to fCountFields-1 do begin
   Field:=@fFields[FieldIndex];
   result.Add(Field^.Name,GetFieldValue(Field,aData));
  end;
 except
  FreeAndNil(result);
  raise;
 end;
end;

procedure TpvEntityComponentSystem.TRegisteredComponentType.UnserializeFromJSON(const aJSON:TPasJSONItem;const aData:TpvPointer);
 procedure SetField(const aField:PField;
                    const aData:TpvPointer;
                    const aJSONItemValue:TPasJSONItem);
 var EnumerationFlagIndex,ArrayItemIndex:TpvSizeInt;
     Code:TpvInt32;
     ArrayJSONItemValue:TPasJSONItem;
     SignedInteger:TpvInt64;
     UnsignedInteger:TpvUInt64;
     FloatValue:TpvDouble;
     StringValue:TpvUTF8String;
     Stream:TMemoryStream;
 begin
  case aField^.ElementType of
   TRegisteredComponentType.TField.TElementType.EntityID:begin
    if aJSONItemValue is TPasJSONItemNumber then begin
     UnsignedInteger:=trunc(TPasJSONItemNumber(aJSONItemValue).Value);
    end else if aJSONItemValue is TPasJSONItemString then begin
     UnsignedInteger:=StrToIntDef(TPasJSONItemString(aJSONItemValue).Value,0);
    end else if aJSONItemValue is TPasJSONItemBoolean then begin
     UnsignedInteger:=ord(TPasJSONItemBoolean(aJSONItemValue).Value) and 1;
    end else begin
     UnsignedInteger:=$ffffffff;
    end;
    case aField^.ElementSize of
     1:begin
      PpvUInt8(aData)^:=UnsignedInteger;
     end;
     2:begin
      PpvUInt16(aData)^:=UnsignedInteger;
     end;
     4:begin
      PpvUInt32(aData)^:=UnsignedInteger;
     end;
     8:begin
      PpvUInt64(aData)^:=UnsignedInteger;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-24-0000');
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.Enumeration:begin
    if aJSONItemValue is TPasJSONItemString then begin
     StringValue:=TPasJSONItemString(aJSONItemValue).Value;
    end else begin
     StringValue:='';
    end;
    UnsignedInteger:=0;
    for EnumerationFlagIndex:=0 to length(aField^.EnumerationsOrFlags)-1 do begin
     if aField^.EnumerationsOrFlags[EnumerationFlagIndex].Name=StringValue then begin
      UnsignedInteger:=aField^.EnumerationsOrFlags[EnumerationFlagIndex].Value;
      break;
     end;
    end;
    case aField^.ElementSize of
     1:begin
      PpvUInt8(aData)^:=UnsignedInteger;
     end;
     2:begin
      PpvUInt16(aData)^:=UnsignedInteger;
     end;
     4:begin
      PpvUInt32(aData)^:=UnsignedInteger;
     end;
     8:begin
      PpvUInt64(aData)^:=UnsignedInteger;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-29-0000');
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.Flags:begin
    UnsignedInteger:=0;
    if aJSONItemValue is TPasJSONItemArray then begin
     for ArrayItemIndex:=0 to TPasJSONItemArray(aJSONItemValue).Count-1 do begin
      ArrayJSONItemValue:=TPasJSONItemArray(aJSONItemValue).Items[ArrayItemIndex];
      if assigned(ArrayJSONItemValue) then begin
       if ArrayJSONItemValue is TPasJSONItemString then begin
        StringValue:=TPasJSONItemString(ArrayJSONItemValue).Value;
       end else begin
        StringValue:='';
       end;
       for EnumerationFlagIndex:=0 to length(aField^.EnumerationsOrFlags)-1 do begin
        if aField^.EnumerationsOrFlags[EnumerationFlagIndex].Name=StringValue then begin
         UnsignedInteger:=UnsignedInteger or aField^.EnumerationsOrFlags[EnumerationFlagIndex].Value;
        end;
       end;
      end;
     end;
    end;
    case aField^.ElementSize of
     1:begin
      PpvUInt8(aData)^:=UnsignedInteger;
     end;
     2:begin
      PpvUInt16(aData)^:=UnsignedInteger;
     end;
     4:begin
      PpvUInt32(aData)^:=UnsignedInteger;
     end;
     8:begin
      PpvUInt64(aData)^:=UnsignedInteger;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-33-0000');
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.Boolean:begin
    if aJSONItemValue is TPasJSONItemNumber then begin
     UnsignedInteger:=trunc(TPasJSONItemNumber(aJSONItemValue).Value) and 1;
    end else if aJSONItemValue is TPasJSONItemString then begin
     UnsignedInteger:=StrToIntDef(TPasJSONItemString(aJSONItemValue).Value,0) and 1;
    end else if aJSONItemValue is TPasJSONItemBoolean then begin
     UnsignedInteger:=ord(TPasJSONItemBoolean(aJSONItemValue).Value) and 1;
    end else begin
     UnsignedInteger:=0;
    end;
    case aField^.ElementSize of
     1:begin
      PpvUInt8(aData)^:=UnsignedInteger;
     end;
     2:begin
      PpvUInt16(aData)^:=UnsignedInteger;
     end;
     4:begin
      PpvUInt32(aData)^:=UnsignedInteger;
     end;
     8:begin
      PpvUInt64(aData)^:=UnsignedInteger;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-37-0000');
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.SignedInteger:begin
    if aJSONItemValue is TPasJSONItemNumber then begin
     SignedInteger:=trunc(TPasJSONItemNumber(aJSONItemValue).Value);
    end else if aJSONItemValue is TPasJSONItemString then begin
     SignedInteger:=StrToIntDef(TPasJSONItemString(aJSONItemValue).Value,0);
    end else if aJSONItemValue is TPasJSONItemBoolean then begin
     SignedInteger:=ord(TPasJSONItemBoolean(aJSONItemValue).Value) and 1;
    end else begin
     SignedInteger:=0;
    end;
    case aField^.ElementSize of
     1:begin
      PpvInt8(aData)^:=SignedInteger;
     end;
     2:begin
      PpvInt16(aData)^:=SignedInteger;
     end;
     4:begin
      PpvInt32(aData)^:=SignedInteger;
     end;
     8:begin
      PpvInt64(aData)^:=SignedInteger;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-38-0000');
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.UnsignedInteger:begin
    if aJSONItemValue is TPasJSONItemNumber then begin
     UnsignedInteger:=trunc(TPasJSONItemNumber(aJSONItemValue).Value);
    end else if aJSONItemValue is TPasJSONItemString then begin
     UnsignedInteger:=StrToIntDef(TPasJSONItemString(aJSONItemValue).Value,0);
    end else if aJSONItemValue is TPasJSONItemBoolean then begin
     UnsignedInteger:=ord(TPasJSONItemBoolean(aJSONItemValue).Value) and 1;
    end else begin
     UnsignedInteger:=0;
    end;
    case aField^.ElementSize of
     1:begin
      PpvUInt8(aData)^:=UnsignedInteger;
     end;
     2:begin
      PpvUInt16(aData)^:=UnsignedInteger;
     end;
     4:begin
      PpvUInt32(aData)^:=UnsignedInteger;
     end;
     8:begin
      PpvUInt64(aData)^:=UnsignedInteger;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-38-0001');
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.FloatingPoint:begin
    if aJSONItemValue is TPasJSONItemNumber then begin
     FloatValue:=TPasJSONItemNumber(aJSONItemValue).Value;
    end else if aJSONItemValue is TPasJSONItemString then begin
     FloatValue:=0.0;
     Val(TPasJSONItemString(aJSONItemValue).Value,FloatValue,Code);
    end else if aJSONItemValue is TPasJSONItemBoolean then begin
     FloatValue:=ord(TPasJSONItemBoolean(aJSONItemValue).Value) and 1;
    end else begin
     FloatValue:=0.0;
    end;
    case aField^.ElementSize of
     2:begin
      PpvHalfFloat(aData)^:=FloatValue;
     end;
     4:begin
      PpvFloat(aData)^:=FloatValue;
     end;
     8:begin
      PpvDouble(aData)^:=FloatValue;
     end;
     else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-38-0002');
     end;
    end;
   end;
   TRegisteredComponentType.TField.TElementType.LengthPrefixedString:begin
    if aJSONItemValue is TPasJSONItemString then begin
     StringValue:=TPasJSONItemString(aJSONItemValue).Value;
    end else begin
     StringValue:='';
    end;
    if length(StringValue)>(aField^.ElementSize-1) then begin
     SetLength(StringValue,aField^.ElementSize-1);
    end;
    PpvUInt8(aData)^:=length(StringValue);
    if length(StringValue)>0 then begin
     Move(StringValue[1],PAnsiChar(aData)[1],length(StringValue));
    end;
   end;
   TRegisteredComponentType.TField.TElementType.ZeroTerminatedString:begin
    if aJSONItemValue is TPasJSONItemString then begin
     StringValue:=TPasJSONItemString(aJSONItemValue).Value;
    end else begin
     StringValue:='';
    end;
    if length(StringValue)>(aField^.ElementSize-1) then begin
     SetLength(StringValue,aField^.ElementSize-1);
    end;
    if length(StringValue)>0 then begin
     Move(StringValue[1],PAnsiChar(aData)[0],length(StringValue));
    end;
    PAnsiChar(aData)[length(StringValue)]:=#0;
   end;
   TRegisteredComponentType.TField.TElementType.Blob:begin
    if aJSONItemValue is TPasJSONItemString then begin
     StringValue:=TPasJSONItemString(aJSONItemValue).Value;
    end else begin
     StringValue:='';
    end;
    Stream:=TMemoryStream.Create;
    try
     if TpvBase64.Decode(TpvRawByteString(StringValue),Stream) then begin
      FillChar(aData^,Min(Stream.Size,aField^.ElementSize),#0);
      if Stream.Size>0 then begin
       Move(Stream.Memory^,aData^,Min(Stream.Size,aField^.ElementSize));
      end;
     end else begin
      raise ERegisteredComponentType.Create('Internal error 2018-09-05-00-53-0001');
     end;
    finally
     FreeAndNil(Stream);
    end;
   end;
   else begin
    raise ERegisteredComponentType.Create('Internal error 2018-09-05-00-53-0000');
   end;
  end;
 end;
var FieldIndex,ElementIndex:TpvSizeInt;
    Field:PField;
    Data:TpvPointer;
    JSONItemObject:TPasJSONItemObject;
    ValueJSONItem:TPasJSONItem;
    ValueJSONItemArray:TPasJSONItemArray;
begin
 if assigned(aJSON) and (aJSON is TPasJSONItemObject) then begin
  JSONItemObject:=TPasJSONItemObject(aJSON);
  for FieldIndex:=0 to fCountFields-1 do begin
   Field:=@fFields[FieldIndex];
   Data:=@PpvUInt8Array(aData)^[Field^.Offset];
   ValueJSONItem:=JSONItemObject.Properties[Field^.Name];
   if assigned(ValueJSONItem) then begin
    if Field^.ElementCount>1 then begin
     if ValueJSONItem is TPasJSONItemArray then begin
      ValueJSONItemArray:=TPasJSONItemArray(ValueJSONItem);
     end else begin
      ValueJSONItemArray:=TPasJSONItemArray.Create;
      ValueJSONItemArray.Add(ValueJSONItem);
     end;
     try
      for ElementIndex:=0 to Min(Field^.ElementCount,ValueJSONItemArray.Count)-1 do begin
       SetField(Field,@PpvUInt8Array(Data)^[ElementIndex*Field^.ElementSize],ValueJSONItemArray.Items[ElementIndex]);
      end;
      for ElementIndex:=ValueJSONItemArray.Count to Field^.ElementCount-1 do begin
       FillChar(PpvUInt8Array(Data)^[ElementIndex*Field^.ElementSize],Field^.ElementSize,#0);
      end;
     finally
      if ValueJSONItemArray<>ValueJSONItem then begin
       FreeAndNil(ValueJSONItemArray);
      end;
     end;
    end else begin
     SetField(Field,Data,ValueJSONItem);
    end;
   end else begin
    if Field^.ElementType=TRegisteredComponentType.TField.TElementType.EntityID then begin
     FillChar(Data^,Field^.Size,#$ff);
    end else begin
     FillChar(Data^,Field^.Size,#0);
    end;
   end;
  end;
 end else begin
  raise ERegisteredComponentType.Create('Internal error 2018-09-05-01-08-0000');
 end;
end;

{ TpvComponentType }

constructor TpvEntityComponentSystem.TComponent.Create(const aRegisteredComponentType:TRegisteredComponentType);
begin
 inherited Create;

 fRegisteredComponentType:=aRegisteredComponentType;

 fSize:=fRegisteredComponentType.fSize;

 fPoolUnaligned:=nil;

 fPool:=nil;

 fPoolSize:=0;

 fCountPoolItems:=0;

 fCapacity:=0;

 fPoolIndexCounter:=0;

 fMaxEntityIndex:=-1;

 fCountFrees:=0;

 fNeedToDefragment:=false;

 fEntityIndexToComponentPoolIndex:=nil;

 fComponentPoolIndexToEntityIndex:=nil;

 fPointers:=nil;

 fDataPointer:=nil;

 fUsedBitmap:=nil;

end;

destructor TpvEntityComponentSystem.TComponent.Destroy;
begin

 if assigned(fPoolUnaligned) then begin
  FreeMem(fPoolUnaligned);
 end;

 fPointers:=nil;

 fEntityIndexToComponentPoolIndex:=nil;

 fComponentPoolIndexToEntityIndex:=nil;

 fPointers:=nil;

 fUsedBitmap:=nil;

 inherited Destroy;

end;

procedure TpvEntityComponentSystem.TComponent.FinalizeComponentByPoolIndex(const aPoolIndex:TpvSizeInt);
begin
end;

procedure TpvEntityComponentSystem.TComponent.SetMaxEntities(const aCount:TpvSizeInt);
var OldCount:TpvSizeInt;
begin
 OldCount:=length(fPointers);
 if OldCount<aCount then begin
  SetLength(fPointers,aCount*2);
  FillChar(fPointers[OldCount],(length(fPointers)-OldCount)*SizeOf(TpvPointer),#0);
  fDataPointer:=@fPointers[0];
 end;
end;

procedure TpvEntityComponentSystem.TComponent.Defragment;
 function CompareFunction(const a,b:TpvSizeInt):TpvSizeInt;
 begin
  if (a>=0) and (b>=0) then begin
   result:=a-b;
  end else if a<>b then begin
   if a<0 then begin
    result:=1;
   end else begin
    result:=-1;
   end;
  end else begin
   result:=0;
  end;
 end;
 procedure IntroSort(Left,Right:TpvSizeInt);
 type PByteArray=^TByteArray;
      TByteArray=array[0..$3fffffff] of byte;
      PStackItem=^TStackItem;
      TStackItem=record
       Left,Right,Depth:TpvSizeInt;
      end;
 var Depth,i,j,Middle,Size,Parent,Child,TempPoolIndex,PivotPoolIndex:TpvSizeInt;
     Items,Pivot,Temp:TpvPointer;
     StackItem:PStackItem;
     Stack:array[0..31] of TStackItem;
 begin
  if Left<Right then begin
   GetMem(Temp,fSize);
   GetMem(Pivot,fSize);
   try
    Items:=fPool;
    StackItem:=@Stack[0];
    StackItem^.Left:=Left;
    StackItem^.Right:=Right;
    StackItem^.Depth:=IntLog2((Right-Left)+1) shl 1;
    inc(StackItem);
    while TpvPtrUInt(TpvPointer(StackItem))>TpvPtrUInt(TpvPointer(@Stack[0])) do begin
     dec(StackItem);
     Left:=StackItem^.Left;
     Right:=StackItem^.Right;
     Depth:=StackItem^.Depth;
     if (Right-Left)<16 then begin
      // Insertion sort
      for i:=Left+1 to Right do begin
       j:=i-1;
       if (j>=Left) and (CompareFunction(fComponentPoolIndexToEntityIndex[j],fComponentPoolIndexToEntityIndex[i])>0) then begin
        Move(PByteArray(Items)^[i*fSize],Temp^,fSize);
        TempPoolIndex:=fComponentPoolIndexToEntityIndex[i];
        repeat
         Move(PByteArray(Items)^[j*fSize],PByteArray(Items)^[(j+1)*fSize],fSize);
         fComponentPoolIndexToEntityIndex[j+1]:=fComponentPoolIndexToEntityIndex[j];
         dec(j);
        until not ((j>=Left) and (CompareFunction(fComponentPoolIndexToEntityIndex[j],TempPoolIndex)>0));
        Move(Temp^,PByteArray(Items)^[(j+1)*fSize],fSize);
        fComponentPoolIndexToEntityIndex[j+1]:=TempPoolIndex;
       end;
      end;
     end else begin
      if (Depth=0) or (TpvPtrUInt(TpvPointer(StackItem))>=TpvPtrUInt(TpvPointer(@Stack[high(Stack)-1]))) then begin
       // Heap sort
       Size:=(Right-Left)+1;
       i:=Size div 2;
       TempPoolIndex:=0;
       repeat
        if i>Left then begin
         dec(i);
         Move(PByteArray(Items)^[(Left+i)*fSize],Temp^,fSize);
         TempPoolIndex:=fComponentPoolIndexToEntityIndex[Left+i];
        end else begin
         if Size=0 then begin
          break;
         end else begin
          dec(Size);
          Move(PByteArray(Items)^[(Left+Size)*fSize],Temp^,fSize);
          Move(PByteArray(Items)^[Left*fSize],PByteArray(Items)^[(Left+Size)*fSize],fSize);
          TempPoolIndex:=fComponentPoolIndexToEntityIndex[Left+Size];
          fComponentPoolIndexToEntityIndex[Left+Size]:=fComponentPoolIndexToEntityIndex[Left];
         end;
        end;
        Parent:=i;
        Child:=(i*2)+1;
        while Child<Size do begin
         if ((Child+1)<Size) and (CompareFunction(fComponentPoolIndexToEntityIndex[(Left+Child)+1],fComponentPoolIndexToEntityIndex[Left+Child])>0) then begin
          inc(Child);
         end;
         if CompareFunction(fComponentPoolIndexToEntityIndex[Left+Child],TempPoolIndex)>0 then begin
          Move(PByteArray(Items)^[(Left+Child)*fSize],PByteArray(Items)^[(Left+Parent)*fSize],fSize);
          fComponentPoolIndexToEntityIndex[Left+Parent]:=fComponentPoolIndexToEntityIndex[Left+Child];
          Parent:=Child;
          Child:=(Parent*2)+1;
         end else begin
          break;
         end;
        end;
        Move(Temp^,PByteArray(fPool)^[(Left+Parent)*fSize],fSize);
        fComponentPoolIndexToEntityIndex[Left+Parent]:=TempPoolIndex;
       until false;
      end else begin
       // Quick sort width median-of-three optimization
       Middle:=Left+((Right-Left) shr 1);
       if (Right-Left)>3 then begin
        if CompareFunction(fComponentPoolIndexToEntityIndex[Left],fComponentPoolIndexToEntityIndex[Middle])>0 then begin
         Move(PByteArray(Items)^[Left*fSize],Temp^,fSize);
         Move(PByteArray(Items)^[Middle*fSize],PByteArray(Items)^[Left*fSize],fSize);
         Move(Temp^,PByteArray(Items)^[Middle*fSize],fSize);
         TempPoolIndex:=fComponentPoolIndexToEntityIndex[Left];
         fComponentPoolIndexToEntityIndex[Left]:=fComponentPoolIndexToEntityIndex[Middle];
         fComponentPoolIndexToEntityIndex[Middle]:=TempPoolIndex;
        end;
        if CompareFunction(fComponentPoolIndexToEntityIndex[Left],fComponentPoolIndexToEntityIndex[Right])>0 then begin
         Move(PByteArray(Items)^[Left*fSize],Temp^,fSize);
         Move(PByteArray(Items)^[Right*fSize],PByteArray(Items)^[Left*fSize],fSize);
         Move(Temp^,PByteArray(Items)^[Right*fSize],fSize);
         TempPoolIndex:=fComponentPoolIndexToEntityIndex[Left];
         fComponentPoolIndexToEntityIndex[Left]:=fComponentPoolIndexToEntityIndex[Right];
         fComponentPoolIndexToEntityIndex[Right]:=TempPoolIndex;
        end;
        if CompareFunction(fComponentPoolIndexToEntityIndex[Middle],fComponentPoolIndexToEntityIndex[Right])>0 then begin
         Move(PByteArray(Items)^[Middle*fSize],Temp^,fSize);
         Move(PByteArray(Items)^[Right*fSize],PByteArray(Items)^[Middle*fSize],fSize);
         Move(Temp^,PByteArray(Items)^[Right*fSize],fSize);
         TempPoolIndex:=fComponentPoolIndexToEntityIndex[Middle];
         fComponentPoolIndexToEntityIndex[Middle]:=fComponentPoolIndexToEntityIndex[Right];
         fComponentPoolIndexToEntityIndex[Right]:=TempPoolIndex;
        end;
       end;
       Move(PByteArray(Items)^[Middle*fSize],Pivot^,fSize);
       PivotPoolIndex:=fComponentPoolIndexToEntityIndex[Middle];
       i:=Left;
       j:=Right;
       repeat
        while (i<Right) and (CompareFunction(fComponentPoolIndexToEntityIndex[i],PivotPoolIndex)<0) do begin
         inc(i);
        end;
        while (j>=i) and (CompareFunction(fComponentPoolIndexToEntityIndex[j],PivotPoolIndex)>0) do begin
         dec(j);
        end;
        if i>j then begin
         break;
        end else begin
         if i<>j then begin
          Move(PByteArray(Items)^[i*fSize],Temp^,fSize);
          Move(PByteArray(Items)^[j*fSize],PByteArray(Items)^[i*fSize],fSize);
          Move(Temp^,PByteArray(Items)^[j*fSize],fSize);
          TempPoolIndex:=fComponentPoolIndexToEntityIndex[i];
          fComponentPoolIndexToEntityIndex[i]:=fComponentPoolIndexToEntityIndex[j];
          fComponentPoolIndexToEntityIndex[j]:=TempPoolIndex;
         end;
         inc(i);
         dec(j);
        end;
       until false;
       if i<Right then begin
        StackItem^.Left:=i;
        StackItem^.Right:=Right;
        StackItem^.Depth:=Depth-1;
        inc(StackItem);
       end;
       if Left<j then begin
        StackItem^.Left:=Left;
        StackItem^.Right:=j;
        StackItem^.Depth:=Depth-1;
        inc(StackItem);
       end;
      end;
     end;
    end;
   finally
    FreeMem(Pivot);
    FreeMem(Temp);
   end;
  end;
 end;
var Index,OtherIndex:TpvSizeInt;
    NeedToSort:boolean;
begin
 NeedToSort:=false;
 for Index:=0 to fPoolIndexCounter-2 do begin
  if fComponentPoolIndexToEntityIndex[Index]>fComponentPoolIndexToEntityIndex[Index+1] then begin
   NeedToSort:=true;
   break;
  end;
 end;
 if NeedToSort then begin
  IntroSort(0,fPoolIndexCounter-1);
  for Index:=0 to fMaxEntityIndex do begin
   fEntityIndexToComponentPoolIndex[Index]:=-1;
  end;
  for Index:=0 to fPoolIndexCounter-1 do begin
   OtherIndex:=fComponentPoolIndexToEntityIndex[Index];
   if OtherIndex>=0 then begin
    fEntityIndexToComponentPoolIndex[OtherIndex]:=Index;
   end;
  end;
  for Index:=0 to fMaxEntityIndex do begin
   OtherIndex:=fEntityIndexToComponentPoolIndex[Index];
   if OtherIndex>=0 then begin
    fPointers[Index]:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(OtherIndex)*TpvPtrUInt(fSize))));
   end else begin
    fPointers[Index]:=nil;
   end;
  end;
  fCountFrees:=0;
  fNeedToDefragment:=false;
 end;
end;

procedure TpvEntityComponentSystem.TComponent.DefragmentIfNeeded;
begin
 if fNeedToDefragment then begin
  fNeedToDefragment:=false;
  Defragment;
 end;
end;

function TpvEntityComponentSystem.TComponent.GetComponentPoolIndexForEntityIndex(const aEntityIndex:TpvSizeInt):TpvSizeInt;
begin
 if (aEntityIndex>=0) and
    (aEntityIndex<=fMaxEntityIndex) and
    ((fUsedBitmap[aEntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(aEntityIndex and 31)))<>0) then begin
  result:=fEntityIndexToComponentPoolIndex[aEntityIndex];
 end else begin
  result:=-1;
 end;
end;

function TpvEntityComponentSystem.TComponent.IsComponentInEntityIndex(const aEntityIndex:TpvSizeInt):boolean;
begin
 result:=(aEntityIndex>=0) and
         (aEntityIndex<=fMaxEntityIndex) and
         ((fUsedBitmap[aEntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(aEntityIndex and 31)))<>0) and
         (fEntityIndexToComponentPoolIndex[aEntityIndex]>=0);
end;

function TpvEntityComponentSystem.TComponent.GetEntityIndexByPoolIndex(const aPoolIndex:TpvSizeInt):TpvSizeInt;
begin
 if (aPoolIndex>=0) and
    (aPoolIndex<fPoolIndexCounter) then begin
  result:=fComponentPoolIndexToEntityIndex[aPoolIndex];
 end else begin
  result:=-1;
 end;
end;

function TpvEntityComponentSystem.TComponent.GetComponentByPoolIndex(const aPoolIndex:TpvSizeInt):TpvPointer;
begin
 if (aPoolIndex>=0) and
    (aPoolIndex<fPoolIndexCounter) then begin
  result:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(aPoolIndex)*TpvPtrUInt(fSize))));
 end else begin
  result:=nil;
 end;
end;

function TpvEntityComponentSystem.TComponent.GetComponentByEntityIndex(const aEntityIndex:TpvSizeInt):TpvPointer;
var PoolIndex:TpvSizeInt;
begin
 result:=nil;
 if (aEntityIndex>=0) and
    (aEntityIndex<=fMaxEntityIndex) and
    ((fUsedBitmap[aEntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(aEntityIndex and 31)))<>0) then begin
  PoolIndex:=fComponentPoolIndexToEntityIndex[aEntityIndex];
  if (PoolIndex>=0) and
     (PoolIndex<fPoolIndexCounter) then begin
   result:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(PoolIndex)*TpvPtrUInt(fSize))));
  end;
 end;
end;

function TpvEntityComponentSystem.TComponent.AllocateComponentForEntityIndex(const aEntityIndex:TpvSizeInt):boolean;
var Index,PoolIndex,NewMaxEntityIndex,OldCapacity,OldCount,Count,OtherIndex,
    OldPoolSize,NewPoolSize:TpvSizeInt;
    Bitmap:PpvUInt32;
    OldPoolAlignmentOffset:TpvPtrUInt;
begin

 result:=false;

 if (aEntityIndex>=0) and
    not ((aEntityIndex<=fMaxEntityIndex) and
         (fEntityIndexToComponentPoolIndex[aEntityIndex]>=0)) then begin

  if fMaxEntityIndex<aEntityIndex then begin
   NewMaxEntityIndex:=(aEntityIndex+1)*2;
   SetLength(fEntityIndexToComponentPoolIndex,NewMaxEntityIndex+1);
   for Index:=fMaxEntityIndex+1 to NewMaxEntityIndex do begin
    fEntityIndexToComponentPoolIndex[Index]:=-1;
   end;
   fMaxEntityIndex:=NewMaxEntityIndex;
  end;

  PoolIndex:=fPoolIndexCounter;
  inc(fPoolIndexCounter);

  if fCapacity<fPoolIndexCounter then begin
   OldCapacity:=fCapacity;
   fCapacity:=fPoolIndexCounter*2;
   SetLength(fComponentPoolIndexToEntityIndex,fCapacity);
   for Index:=OldCapacity to fCapacity-1 do begin
    fComponentPoolIndexToEntityIndex[Index]:=-1;
   end;
  end;

  NewPoolSize:=TpvSizeInt(fCapacity)*TpvSizeInt(fSize);
  if fPoolSize<NewPoolSize then begin
   OldPoolSize:=fPoolSize;
   fPoolSize:=NewPoolSize*2;
   if assigned(fPoolUnaligned) then begin
    OldPoolAlignmentOffset:=TpvPtrUInt(TpvPtrUInt(fPool)-TpvPtrUInt(fPoolUnaligned));
    ReallocMem(fPoolUnaligned,fPoolSize+(4096*2));
    fPool:=TpvPointer(TpvPtrUInt(TpvPtrUInt(TpvPtrUInt(fPoolUnaligned)+4095) and not 4095));
    if OldPoolAlignmentOffset<>TpvPtrUInt(TpvPtrUInt(fPool)-TpvPtrUInt(fPoolUnaligned)) then begin
     // Move the old existent data to the new alignment offset
     Move(TpvPointer(TpvPtrUInt(TpvPtrUInt(fPoolUnaligned)+TpvPtrUInt(OldPoolAlignmentOffset)))^,fPool^,fPoolSize);
    end;
    if OldPoolSize<fPoolSize then begin
     FillChar(TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(OldPoolSize)))^,fPoolSize-OldPoolSize,#0);
    end;
    for Index:=0 to fMaxEntityIndex do begin
     OtherIndex:=fEntityIndexToComponentPoolIndex[Index];
     if OtherIndex>=0 then begin
      fPointers[Index]:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(OtherIndex)*TpvPtrUInt(fSize))));
     end else begin
      fPointers[Index]:=nil;
     end;
    end;
   end else begin
    GetMem(fPoolUnaligned,fPoolSize+(4096*2));
    fPool:=TpvPointer(TpvPtrUInt(TpvPtrUInt(TpvPtrUInt(fPoolUnaligned)+4095) and not 4095));
    FillChar(fPool^,fPoolSize,#0);
   end;
  end;

  OldCount:=length(fUsedBitmap);
  Count:=((fMaxEntityIndex+1)+31) shr 5;
  if OldCount<Count then begin
   SetLength(fUsedBitmap,Count*2);
   for Index:=OldCount to length(fUsedBitmap)-1 do begin
    fUsedBitmap[Index]:=0;
   end;
  end;

  OldCount:=length(fPointers);
  Count:=fMaxEntityIndex+1;
  if OldCount<Count then begin
   SetLength(fPointers,Count*2);
   for Index:=OldCount to length(fPointers)-1 do begin
    fPointers[Index]:=nil;
   end;
   fDataPointer:=@fPointers[0];
  end;

  fEntityIndexToComponentPoolIndex[aEntityIndex]:=PoolIndex;
  fComponentPoolIndexToEntityIndex[PoolIndex]:=aEntityIndex;

  fPointers[aEntityIndex]:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(PoolIndex)*TpvPtrUInt(fSize))));

//FillChar(TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(PoolIndex)*TpvPtrUInt(fSize))))^,fSize,#0);

  Bitmap:=@fUsedBitmap[aEntityIndex shr 5];
  Bitmap^:=Bitmap^ or TpvUInt32(TpvUInt32(1) shl TpvUInt32(aEntityIndex and 31));

  result:=true;

 end;

end;

function TpvEntityComponentSystem.TComponent.FreeComponentFromEntityIndex(const aEntityIndex:TpvSizeInt):boolean;
var PoolIndex,OtherPoolIndex,OtherEntityID:longint;
    Mask:TpvUInt32;
    Bitmap:PpvUInt32;
begin
 result:=false;
 Bitmap:=@fUsedBitmap[aEntityIndex shr 5];
 Mask:=TpvUInt32(TpvUInt32(1) shl TpvUInt32(aEntityIndex and 31));
 if (aEntityIndex>=0) and
    (aEntityIndex<=fMaxEntityIndex) and
    ((Bitmap^ and Mask)<>0) and
    (fEntityIndexToComponentPoolIndex[aEntityIndex]>=0) then begin
  Bitmap^:=Bitmap^ and not Mask;
  PoolIndex:=fEntityIndexToComponentPoolIndex[aEntityIndex];
  FinalizeComponentByPoolIndex(PoolIndex);
  fPointers[aEntityIndex]:=nil;
  dec(fPoolIndexCounter);
  if fPoolIndexCounter>0 then begin
   OtherPoolIndex:=fPoolIndexCounter;
   OtherEntityID:=fComponentPoolIndexToEntityIndex[OtherPoolIndex];
   fEntityIndexToComponentPoolIndex[OtherEntityID]:=PoolIndex;
   fComponentPoolIndexToEntityIndex[PoolIndex]:=OtherEntityID;
   fComponentPoolIndexToEntityIndex[OtherPoolIndex]:=-1;
   Move(TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(OtherPoolIndex)*TpvPtrUInt(fSize))))^,
        TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(PoolIndex)*TpvPtrUInt(fSize))))^,
        fSize);
   fPointers[OtherEntityID]:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fPool)+TpvPtrUInt(TpvPtrUInt(PoolIndex)*TpvPtrUInt(fSize))));
  end else begin
   fComponentPoolIndexToEntityIndex[PoolIndex]:=-1;
  end;
  fEntityIndexToComponentPoolIndex[aEntityIndex]:=-1;
  inc(fCountFrees);
  if fCountFrees>(fPoolIndexCounter shr 2) then begin
   fNeedToDefragment:=true;
  end;
 end;
end;

{ TpvEntityComponentSystem.TEntity }

function TpvEntityComponentSystem.TEntity.GetActive:boolean;
begin
 result:=TFlag.Active in fFlags;
end;

procedure TpvEntityComponentSystem.TEntity.SetActive(const aActive:boolean);
begin
 if aActive<>(TFlag.Active in fFlags) then begin
  if aActive then begin
   Include(fFlags,TFlag.Active);
  end else begin
   Exclude(fFlags,TFlag.Active);
  end;
 end;
end;

procedure InitializeEntityComponentSystemGlobals;
begin
 if not assigned(RegisteredComponentTypeList) then begin
  RegisteredComponentTypeList:=TpvEntityComponentSystem.TRegisteredComponentTypeList.Create;
  RegisteredComponentTypeList.OwnsObjects:=true;
 end;
end;

initialization
 InitializeEntityComponentSystemGlobals;
finalization
 FreeAndNil(RegisteredComponentTypeList);
end.

