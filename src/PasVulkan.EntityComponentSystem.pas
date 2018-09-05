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

type EpvSystemCircularDependency=class(Exception);

     EpvSystemSerialization=class(Exception);

     EpvSystemUnserialization=class(Exception);

     EpvDuplicateComponentInEntity=class(Exception);

     PpvComponentTypeID=^TpvComponentTypeID;
     TpvComponentTypeID=type TpvSizeUInt;

     TpvComponentTypeIDDynamicArray=array of TpvComponentTypeID;

     PpvEntityID=^TpvEntityID;
     TpvEntityID=type TpvInt32;

     TpvEntityIDs=array of TpvEntityID;

     PpvWorldID=^TpvWorldID;
     TpvWorldID=type TpvInt32;

     PpvEventID=^TpvEventID;
     TpvEventID=type TpvInt32;

     EpvRegisteredComponentType=class(Exception);

     TpvRegisteredComponentType=class
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
       fID:TpvSizeUInt;
       fName:TpvUTF8String;
       fDisplayName:TpvUTF8String;
       fPath:TPath;
       fSize:TpvSizeInt;
       fUUID:TpvUUID;
       fFields:TFields;
       fCountFields:TpvSizeInt;
       fDefault:TpvUInt8DynamicArray;
       fEditorWidget:pointer;
      public
       constructor Create(const aName:TpvUTF8String;
                          const aDisplayName:TpvUTF8String;
                          const aPath:array of TpvUTF8String;
                          const aSize:TpvSizeInt;
                          const aDefault:pointer); reintroduce;
       destructor Destroy; override;
       procedure Add(const aName:TpvUTF8String;
                     const aDisplayName:TpvUTF8String;
                     const aElementType:TField.TElementType;
                     const aElementSize:TpvSizeInt;
                     const aElementCount:TpvSizeInt;
                     const aOffset:TpvSizeInt;
                     const aSize:TpvSizeInt;
                     const aEnumerationsOrFlags:array of TField.TEnumerationOrFlag);
       procedure Finish;
       function SerializeToJSON(const aData:pointer):TPasJSONItemObject;
       procedure UnserializeFromJSON(const aJSON:TPasJSONItem;const aData:pointer);
       property Fields:TFields read fFields;
       property EditorWidget:pointer read fEditorWidget write fEditorWidget;
       class function GetSetOrdValue(const Info:PTypeInfo;const SetParam):TpvUInt64; static;
      published
       property ID:TpvSizeUInt read fID;
       property Default:TpvUInt8DynamicArray read fDefault;
     end;

     TpvRegisteredComponentTypeList=TpvObjectGenericList<TpvRegisteredComponentType>;

var RegisteredComponentTypeList:TpvRegisteredComponentTypeList=nil;

implementation

{ TpvRegisteredComponentType.TField.TEnumerationOrFlag }

constructor TpvRegisteredComponentType.TField.TEnumerationOrFlag.Create(const aValue:TpvUInt64;
                                                                        const aName:TpvUTF8String;
                                                                        const aDisplayName:TpvUTF8String);
begin
 Value:=aValue;
 Name:=aName;
 DisplayName:=aDisplayName;
end;

{ TpvRegisteredComponentType }

constructor TpvRegisteredComponentType.Create(const aName:TpvUTF8String;
                                              const aDisplayName:TpvUTF8String;
                                              const aPath:array of TpvUTF8String;
                                              const aSize:TpvSizeInt;
                                              const aDefault:pointer);
var Index:TpvSizeInt;
begin
 inherited Create;
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

destructor TpvRegisteredComponentType.Destroy;
begin
 fFields:=nil;
 fDefault:=nil;
 inherited Destroy;
end;

class function TpvRegisteredComponentType.GetSetOrdValue(const Info:PTypeInfo;const SetParam):TpvUInt64;
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

procedure TpvRegisteredComponentType.Add(const aName:TpvUTF8String;
                                         const aDisplayName:TpvUTF8String;
                                         const aElementType:TField.TElementType;
                                         const aElementSize:TpvSizeInt;
                                         const aElementCount:TpvSizeInt;
                                         const aOffset:TpvSizeInt;
                                         const aSize:TpvSizeInt;
                                         const aEnumerationsOrFlags:array of TField.TEnumerationOrFlag);
var Index:TpvSizeInt;
    Field:TpvRegisteredComponentType.PField;
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

procedure TpvRegisteredComponentType.Finish;
begin
 SetLength(fFields,fCountFields);
end;

function TpvRegisteredComponentType.SerializeToJSON(const aData:pointer):TPasJSONItemObject;
 function GetElementValue(const aField:PField;
                          const aValueData:pointer):TPasJSONItem;
 var Data:pointer;
     EnumerationFlagIndex:TpvSizeInt;
     SignedInteger:TpvInt64;
     UnsignedInteger:TpvUInt64;
     FloatValue:TpvDouble;
     StringValue:TpvUTF8String;
 begin
  result:=nil;
  Data:=aValueData;
  case aField^.ElementType of
   TpvRegisteredComponentType.TField.TElementType.EntityID:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-04-23-58-0000');
     end;
    end;
    if abs(SignedInteger)<TpvInt64($0010000000000000) then begin
     result:=TPasJSONItemNumber.Create(SignedInteger);
    end else begin
     result:=TPasJSONItemString.Create(IntToStr(SignedInteger));
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.Enumeration:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-04-23-36-0000');
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
   TpvRegisteredComponentType.TField.TElementType.Flags:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-04-23-36-0000');
     end;
    end;
    result:=TPasJSONItemArray.Create;
    for EnumerationFlagIndex:=0 to length(aField^.EnumerationsOrFlags)-1 do begin
     if (aField^.EnumerationsOrFlags[EnumerationFlagIndex].Value and UnsignedInteger)<>0 then begin
      TPasJSONItemArray(result).Add(TPasJSONItemString.Create(aField^.EnumerationsOrFlags[EnumerationFlagIndex].Name));
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.Boolean:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-00-25-0000');
     end;
    end;
    result:=TPasJSONItemBoolean.Create(UnsignedInteger<>0);
   end;
   TpvRegisteredComponentType.TField.TElementType.SignedInteger:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-00-15-0001');
     end;
    end;
    if abs(SignedInteger)<TpvUInt64($0010000000000000) then begin
     result:=TPasJSONItemNumber.Create(SignedInteger);
    end else begin
     result:=TPasJSONItemString.Create(IntToStr(SignedInteger));
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.UnsignedInteger:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-00-15-0000');
     end;
    end;
    if UnsignedInteger<TpvUInt64($0010000000000000) then begin
     result:=TPasJSONItemNumber.Create(UnsignedInteger);
    end else begin
     result:=TPasJSONItemString.Create(IntToStr(UnsignedInteger));
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.FloatingPoint:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-00-22-0000');
     end;
    end;
    result:=TPasJSONItemNumber.Create(FloatValue);
   end;
   TpvRegisteredComponentType.TField.TElementType.LengthPrefixedString:begin
    UnsignedInteger:=PpvUInt8(Data)^;
    StringValue:='';
    if UnsignedInteger>0 then begin
     SetLength(StringValue,UnsignedInteger);
     Move(PAnsiChar(Data)[1],StringValue[1],UnsignedInteger);
    end;
    result:=TPasJSONItemString.Create(StringValue);
   end;
   TpvRegisteredComponentType.TField.TElementType.ZeroTerminatedString:begin
    StringValue:=PAnsiChar(Data);
    result:=TPasJSONItemString.Create(StringValue);
   end;
   TpvRegisteredComponentType.TField.TElementType.Blob:begin
    result:=TPasJSONItemString.Create(TpvBase64.Encode(Data^,aField^.ElementSize));
   end;
   else begin
    raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-00-11-0000');
   end;
  end;
 end;
 function GetFieldValue(const aField:PField;
                        const aData:pointer):TPasJSONItem;
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

procedure TpvRegisteredComponentType.UnserializeFromJSON(const aJSON:TPasJSONItem;const aData:pointer);
 procedure SetField(const aField:PField;
                    const aData:pointer;
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
   TpvRegisteredComponentType.TField.TElementType.EntityID:begin
    if aJSONItemValue is TPasJSONItemNumber then begin
     SignedInteger:=trunc(TPasJSONItemNumber(aJSONItemValue).Value);
    end else if aJSONItemValue is TPasJSONItemString then begin
     SignedInteger:=StrToIntDef(TPasJSONItemString(aJSONItemValue).Value,0);
    end else if aJSONItemValue is TPasJSONItemBoolean then begin
     SignedInteger:=ord(TPasJSONItemBoolean(aJSONItemValue).Value) and 1;
    end else begin
     SignedInteger:=-1;
    end;
    case aField^.ElementSize of
     1:begin
      PpvUInt8(aData)^:=SignedInteger;
     end;
     2:begin
      PpvUInt16(aData)^:=SignedInteger;
     end;
     4:begin
      PpvUInt32(aData)^:=SignedInteger;
     end;
     8:begin
      PpvUInt64(aData)^:=SignedInteger;
     end;
     else begin
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-24-0000');
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.Enumeration:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-29-0000');
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.Flags:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-33-0000');
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.Boolean:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-37-0000');
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.SignedInteger:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-38-0000');
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.UnsignedInteger:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-38-0001');
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.FloatingPoint:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-38-0002');
     end;
    end;
   end;
   TpvRegisteredComponentType.TField.TElementType.LengthPrefixedString:begin
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
   TpvRegisteredComponentType.TField.TElementType.ZeroTerminatedString:begin
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
   TpvRegisteredComponentType.TField.TElementType.Blob:begin
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
      raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-00-53-0001');
     end;
    finally
     FreeAndNil(Stream);
    end;
   end;
   else begin
    raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-00-53-0000');
   end;
  end;
 end;
var FieldIndex,ElementIndex:TpvSizeInt;
    Field:PField;
    Data:pointer;
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
    if Field^.ElementType=TpvRegisteredComponentType.TField.TElementType.EntityID then begin
     FillChar(Data^,Field^.Size,#$ff);
    end else begin
     FillChar(Data^,Field^.Size,#0);
    end;
   end;
  end;
 end else begin
  raise EpvRegisteredComponentType.Create('Internal error 2018-09-05-01-08-0000');
 end;
end;

procedure TestCase;
type TTest=record
      public
       Position:TpvVector3;
       TestString:shortstring;
      end;
      PTest=^TTest;
var RegisteredComponentType:TpvRegisteredComponentType;
    Test:TTest;
    k:TPasJSONItem;
begin
 RegisteredComponentType:=TpvRegisteredComponentType.Create('test',
                                                            'Test',
                                                            ['Base','Test'],
                                                            sizeof(TTest),
                                                            @Test);
 try
  RegisteredComponentType.Add('position',
                              'Position',
                              TpvRegisteredComponentType.TField.TElementType.FloatingPoint,
                              SizeOf(TpvFloat),
                              3,
                              TpvPtrUInt(@PTest(nil)^.Position),
                              SizeOf(TpvVector3),
                              []);
  RegisteredComponentType.Add('teststring',
                              'Test string',
                              TpvRegisteredComponentType.TField.TElementType.LengthPrefixedString,
                              SizeOf(ShortString),
                              1,
                              TpvPtrUInt(@PTest(nil)^.TestString),
                              SizeOf(ShortString),
                              []);
  RegisteredComponentType.Add('testblob',
                              'Test blob',
                              TpvRegisteredComponentType.TField.TElementType.Blob,
                              SizeOf(ShortString),
                              1,
                              TpvPtrUInt(@PTest(nil)^.TestString),
                              SizeOf(ShortString),
                              []);
  Test.Position.x:=1.0;
  Test.Position.y:=2.0;
  Test.Position.z:=3.0;
  Test.TestString:='bla';
  k:=RegisteredComponentType.SerializeToJSON(@Test);
  writeln(TPasJSON.Stringify(k,true,TPasJSON.SimplifiedJSONModeFlags));
  Test.Position.x:=0.0;
  Test.Position.y:=0.0;
  Test.Position.z:=0.0;
  Test.TestString:='tzrterrt';
  writeln(TPasJSON.Stringify(RegisteredComponentType.SerializeToJSON(@Test),true,TPasJSON.SimplifiedJSONModeFlags));
  RegisteredComponentType.UnserializeFromJSON(k,@Test);
  writeln(TPasJSON.Stringify(RegisteredComponentType.SerializeToJSON(@Test),true,TPasJSON.SimplifiedJSONModeFlags));
  readln;
 finally
  RegisteredComponentType.Free;
 end;
 halt(0);
end;

initialization
 RegisteredComponentTypeList:=TpvRegisteredComponentTypeList.Create;
 RegisteredComponentTypeList.OwnsObjects:=true;
finalization
 FreeAndNil(RegisteredComponentTypeList);
end.

