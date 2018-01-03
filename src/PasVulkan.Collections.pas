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
unit PasVulkan.Collections;
{$i PasVulkan.inc}

{$define ExtraStringHashMap}

interface

uses SysUtils,
     Classes,
     SyncObjs,
     TypInfo,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     Generics.Collections;

type TpvDynamicArray<T>=class
      private
       type TValueEnumerator=record
             private
              fDynamicArray:TpvDynamicArray<T>;
              fIndex:TpvSizeInt;
              function GetCurrent:T; inline;
             public
              constructor Create(const aDynamicArray:TpvDynamicArray<T>);
              function MoveNext:boolean; inline;
              property Current:T read GetCurrent;
            end;
      private
       fItems:array of T;
       fCount:TpvSizeInt;
       fAllocated:TpvSizeInt;
       procedure SetCount(const pNewCount:TpvSizeInt);
       function GetItem(const pIndex:TpvSizeInt):T; inline;
       procedure SetItem(const pIndex:TpvSizeInt;const pItem:T); inline;
      protected
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const pItem:T):TpvSizeInt;
       procedure Insert(const pIndex:TpvSizeInt;const pItem:T);
       procedure Delete(const pIndex:TpvSizeInt);
       procedure Exchange(const pIndex,pWithIndex:TpvSizeInt); inline;
       function GetEnumerator:TValueEnumerator;
       function Memory:pointer; inline;
       property Count:TpvSizeInt read fCount write SetCount;
       property Allocated:TpvSizeInt read fAllocated;
       property Items[const pIndex:TpvSizeInt]:T read GetItem write SetItem; default;
     end;

     TpvBaseList=class
      private
       procedure SetCount(const pNewCount:TpvSizeInt);
       function GetItem(const pIndex:TpvSizeInt):pointer;
      protected
       fItemSize:TpvSizeInt;
       fCount:TpvSizeInt;
       fAllocated:TpvSizeInt;
       fMemory:pointer;
       fSorted:boolean;
       procedure InitializeItem(var pItem); virtual;
       procedure FinalizeItem(var pItem); virtual;
       procedure CopyItem(const pSource;var pDestination); virtual;
       procedure ExchangeItem(var pSource,pDestination); virtual;
       function CompareItem(const pSource,pDestination):TpvInt32; virtual;
      public
       constructor Create(const pItemSize:TpvSizeInt);
       destructor Destroy; override;
       procedure Clear; virtual;
       procedure FillWith(const pSourceData;const pSourceCount:TpvSizeInt); virtual;
       function IndexOf(const pItem):TpvSizeInt; virtual;
       function Add(const pItem):TpvSizeInt; virtual;
       procedure Insert(const pIndex:TpvSizeInt;const pItem); virtual;
       procedure Delete(const pIndex:TpvSizeInt); virtual;
       procedure Remove(const pItem); virtual;
       procedure Exchange(const pIndex,pWithIndex:TpvSizeInt); virtual;
       procedure Sort; virtual;
       property Count:TpvSizeInt read fCount write SetCount;
       property Allocated:TpvSizeInt read fAllocated;
       property Memory:pointer read fMemory;
       property ItemPointers[const pIndex:TpvSizeInt]:pointer read GetItem; default;
       property Sorted:boolean read fSorted;
     end;

     TpvObjectGenericList<T:class>=class
      private
       type TValueEnumerator=record
             private
              fObjectList:TpvObjectGenericList<T>;
              fIndex:TpvSizeInt;
              function GetCurrent:T; inline;
             public
              constructor Create(const aObjectList:TpvObjectGenericList<T>);
              function MoveNext:boolean; inline;
              property Current:T read GetCurrent;
            end;
      private
       fItems:array of T;
       fCount:TpvSizeInt;
       fAllocated:TpvSizeInt;
       fOwnObjects:boolean;
       procedure SetCount(const pNewCount:TpvSizeInt);
       function GetItem(const pIndex:TpvSizeInt):T;
       procedure SetItem(const pIndex:TpvSizeInt;const pItem:T);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function IndexOf(const pItem:T):TpvSizeInt;
       function Add(const pItem:T):TpvSizeInt;
       procedure Insert(const pIndex:TpvSizeInt;const pItem:T);
       procedure Delete(const pIndex:TpvSizeInt);
       procedure Remove(const pItem:T);
       procedure Exchange(const pIndex,pWithIndex:TpvSizeInt);
       function GetEnumerator:TValueEnumerator;
       property Count:TpvSizeInt read fCount write SetCount;
       property Allocated:TpvSizeInt read fAllocated;
       property Items[const pIndex:TpvSizeInt]:T read GetItem write SetItem; default;
       property OwnObjects:boolean read fOwnObjects write fOwnObjects;
     end;

     TpvObjectList=TpvObjectGenericList<TObject>;

     TpvGenericList<T>=class
      private
       type TValueEnumerator=record
             private
              fGenericList:TpvGenericList<T>;
              fIndex:TpvSizeInt;
              function GetCurrent:T; inline;
             public
              constructor Create(const aGenericList:TpvGenericList<T>);
              function MoveNext:boolean; inline;
              property Current:T read GetCurrent;
            end;
      private
       fItems:array of T;
       fCount:TpvSizeInt;
       fAllocated:TpvSizeInt;
       fSorted:boolean;
       procedure SetCount(const pNewCount:TpvSizeInt);
       function GetItem(const pIndex:TpvSizeInt):T;
       procedure SetItem(const pIndex:TpvSizeInt;const pItem:T);
      protected
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure Assign(const pFrom:TpvGenericList<T>);
       function IndexOf(const pItem:T):TpvSizeInt;
       function Add(const pItem:T):TpvSizeInt;
       procedure Insert(const pIndex:TpvSizeInt;const pItem:T);
       procedure Delete(const pIndex:TpvSizeInt);
       procedure Remove(const pItem:T);
       procedure Exchange(const pIndex,pWithIndex:TpvSizeInt);
       function GetEnumerator:TValueEnumerator;
       procedure Sort;
       property Count:TpvSizeInt read fCount write SetCount;
       property Allocated:TpvSizeInt read fAllocated;
       property Items[const pIndex:TpvSizeInt]:T read GetItem write SetItem; default;
       property Sorted:boolean read fSorted;
     end;

     EpvHandleMap=class(Exception);

     TpvCustomHandleMap=class
      public
       type TpvUInt8Array=array of TpvUInt8;
            TpvUInt32Array=array of TpvUInt32;
      private
       fMultipleReaderSingleWriterLock:TPasMPMultipleReaderSingleWriterLock;
       fDataSize:TpvSizeUInt;
       fSize:TpvSizeUInt;
       fIndexCounter:TpvUInt32;
       fDenseIndex:TpvUInt32;
       fFreeIndex:TpvUInt32;
       fFreeArray:TpvUInt32Array;
{$ifdef Debug}
       fGenerationArray:TpvUInt32Array;
{$endif}
       fSparseToDenseArray:TpvUInt32Array;
       fDenseToSparseArray:TpvUInt32Array;
       fDataArray:TpvUInt8Array;
      protected
       procedure InitializeHandleData(var pData); virtual;
       procedure FinalizeHandleData(var pData); virtual;
       procedure CopyHandleData(const pSource;out pDestination); virtual;
      public
       constructor Create(const pDataSize:TpvSizeUInt); reintroduce;
       destructor Destroy; override;
       procedure Lock; inline;
       procedure Unlock; inline;
       procedure Clear;
       procedure Defragment;
       function AllocateHandle:TpvHandle;
       procedure FreeHandle(const ppvHandle:TpvHandle);
       procedure GetHandleData(const ppvHandle:TpvHandle;out pData);
       procedure SetHandleData(const ppvHandle:TpvHandle;const pData);
       function GetHandleDataPointer(const ppvHandle:TpvHandle):pointer; inline;
       property DataSize:TpvSizeUInt read fDataSize;
       property Size:TpvSizeUInt read fSize;
       property IndexCounter:TpvUInt32 read fIndexCounter;
     end;

     TpvHashMapEntityIndices=array of TpvInt32;

     TpvHashMapUInt128=array[0..1] of TpvUInt64;

     TpvHashMap<TpvHashMapKey,TpvHashMapValue>=class
      public
       const CELL_EMPTY=-1;
             CELL_DELETED=-2;
             ENT_EMPTY=-1;
             ENT_DELETED=-2;
       type PpvHashMapEntity=^TpvHashMapEntity;
            TpvHashMapEntity=record
             Key:TpvHashMapKey;
             Value:TpvHashMapValue;
            end;
            TpvHashMapEntities=array of TpvHashMapEntity;
      private
       type TpvHashMapEntityEnumerator=record
             private
              fHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>;
              fIndex:TpvSizeInt;
              function GetCurrent:TpvHashMapEntity; inline;
             public
              constructor Create(const aHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
              function MoveNext:boolean; inline;
              property Current:TpvHashMapEntity read GetCurrent;
            end;
            TpvHashMapKeyEnumerator=record
             private
              fHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>;
              fIndex:TpvSizeInt;
              function GetCurrent:TpvHashMapKey; inline;
             public
              constructor Create(const aHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
              function MoveNext:boolean; inline;
              property Current:TpvHashMapKey read GetCurrent;
            end;
            TpvHashMapValueEnumerator=record
             private
              fHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>;
              fIndex:TpvSizeInt;
              function GetCurrent:TpvHashMapValue; inline;
             public
              constructor Create(const aHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
              function MoveNext:boolean; inline;
              property Current:TpvHashMapValue read GetCurrent;
            end;
            TpvHashMapEntitiesObject=class
             private
              fOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>;
             public
              constructor Create(const aOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
              function GetEnumerator:TpvHashMapEntityEnumerator;
            end;
            TpvHashMapKeysObject=class
             private
              fOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>;
             public
              constructor Create(const aOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
              function GetEnumerator:TpvHashMapKeyEnumerator;
            end;
            TpvHashMapValuesObject=class
             private
              fOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>;
              function GetValue(const Key:TpvHashMapKey):TpvHashMapValue; inline;
              procedure SetValue(const Key:TpvHashMapKey;const aValue:TpvHashMapValue); inline;
             public
              constructor Create(const aOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
              function GetEnumerator:TpvHashMapValueEnumerator;
              property Values[const Key:TpvHashMapKey]:TpvHashMapValue read GetValue write SetValue; default;
            end;
      private
       fRealSize:TpvInt32;
       fLogSize:TpvInt32;
       fSize:TpvInt32;
       fEntities:TpvHashMapEntities;
       fEntityToCellIndex:TpvHashMapEntityIndices;
       fCellToEntityIndex:TpvHashMapEntityIndices;
       fDefaultValue:TpvHashMapValue;
       fCanShrink:boolean;
       fEntitiesObject:TpvHashMapEntitiesObject;
       fKeysObject:TpvHashMapKeysObject;
       fValuesObject:TpvHashMapValuesObject;
       function HashData(const Data:pointer;const DataLength:TpvUInt32):TpvUInt32;
       function HashKey(const Key:TpvHashMapKey):TpvUInt32;
       function CompareKey(const KeyA,KeyB:TpvHashMapKey):boolean;
       function FindCell(const Key:TpvHashMapKey):TpvUInt32;
       procedure Resize;
      protected
       function GetValue(const Key:TpvHashMapKey):TpvHashMapValue;
       procedure SetValue(const Key:TpvHashMapKey;const Value:TpvHashMapValue);
      public
       constructor Create(const DefaultValue:TpvHashMapValue);
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:TpvHashMapKey;const Value:TpvHashMapValue):PpvHashMapEntity;
       function Get(const Key:TpvHashMapKey;const CreateIfNotExist:boolean=false):PpvHashMapEntity;
       function TryGet(const Key:TpvHashMapKey;out Value:TpvHashMapValue):boolean;
       function ExistKey(const Key:TpvHashMapKey):boolean;
       function Delete(const Key:TpvHashMapKey):boolean;
       property EntityValues[const Key:TpvHashMapKey]:TpvHashMapValue read GetValue write SetValue; default;
       property Entities:TpvHashMapEntitiesObject read fEntitiesObject;
       property Keys:TpvHashMapKeysObject read fKeysObject;
       property Values:TpvHashMapValuesObject read fValuesObject;
       property CanShrink:boolean read fCanShrink write fCanShrink;
     end;

{$ifdef ExtraStringHashMap}
     TpvStringHashMap<TpvHashMapValue>=class
      private
       const CELL_EMPTY=-1;
             CELL_DELETED=-2;
             ENT_EMPTY=-1;
             ENT_DELETED=-2;
       type TpvHashMapKey=RawByteString;
            PHashMapEntity=^THashMapEntity;
            THashMapEntity=record
             Key:TpvHashMapKey;
             Value:TpvHashMapValue;
            end;
            THashMapEntities=array of THashMapEntity;
      private
       fRealSize:TpvInt32;
       fLogSize:TpvInt32;
       fSize:TpvInt32;
       fEntities:THashMapEntities;
       fEntityToCellIndex:TpvHashMapEntityIndices;
       fCellToEntityIndex:TpvHashMapEntityIndices;
       fDefaultValue:TpvHashMapValue;
       fCanShrink:boolean;
      private
       function HashKey(const Key:TpvHashMapKey):TpvUInt32;
       function FindCell(const Key:TpvHashMapKey):TpvUInt32;
       procedure Resize;
      protected
       function GetValue(const Key:TpvHashMapKey):TpvHashMapValue;
       procedure SetValue(const Key:TpvHashMapKey;const Value:TpvHashMapValue);
      public
       constructor Create(const DefaultValue:TpvHashMapValue);
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:TpvHashMapKey;const Value:TpvHashMapValue):PHashMapEntity;
       function Get(const Key:TpvHashMapKey;const CreateIfNotExist:boolean=false):PHashMapEntity;
       function TryGet(const Key:TpvHashMapKey;out Value:TpvHashMapValue):boolean;
       function ExistKey(const Key:TpvHashMapKey):boolean;
       function Delete(const Key:TpvHashMapKey):boolean;
       property Values[const Key:TpvHashMapKey]:TpvHashMapValue read GetValue write SetValue; default;
       property CanShrink:boolean read fCanShrink write fCanShrink;
     end;
{$else}
     TpvStringHashMap<TpvHashMapValue>=class(TpvHashMap<RawByteString,TpvHashMapValue>);
{$endif}

implementation

uses Generics.Defaults,
     PasVulkan.Utils;

constructor TpvDynamicArray<T>.TValueEnumerator.Create(const aDynamicArray:TpvDynamicArray<T>);
begin
 fDynamicArray:=aDynamicArray;
 fIndex:=-1;
end;

function TpvDynamicArray<T>.TValueEnumerator.MoveNext:boolean;
begin
 inc(fIndex);
 result:=fIndex<fDynamicArray.fCount;
end;

function TpvDynamicArray<T>.TValueEnumerator.GetCurrent:T;
begin
 result:=fDynamicArray.fItems[fIndex];
end;

constructor TpvDynamicArray<T>.Create;
begin
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
 inherited Create;
end;

destructor TpvDynamicArray<T>.Destroy;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
 inherited Destroy;
end;

procedure TpvDynamicArray<T>.Clear;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
end;

procedure TpvDynamicArray<T>.SetCount(const pNewCount:TpvSizeInt);
begin
 if pNewCount<=0 then begin
  SetLength(fItems,0);
  fCount:=0;
  fAllocated:=0;
 end else begin
  if pNewCount<fCount then begin
   fCount:=pNewCount;
   if (fCount+fCount)<fAllocated then begin
    fAllocated:=fCount+fCount;
    SetLength(fItems,fAllocated);
   end;
  end else begin
   fCount:=pNewCount;
   if fAllocated<fCount then begin
    fAllocated:=fCount+fCount;
    SetLength(fItems,fAllocated);
   end;
  end;
 end;
end;

function TpvDynamicArray<T>.GetItem(const pIndex:TpvSizeInt):T;
begin
 result:=fItems[pIndex];
end;

procedure TpvDynamicArray<T>.SetItem(const pIndex:TpvSizeInt;const pItem:T);
begin
 fItems[pIndex]:=pItem;
end;

function TpvDynamicArray<T>.Add(const pItem:T):TpvSizeInt;
begin
 result:=fCount;
 inc(fCount);
 if fAllocated<fCount then begin
  fAllocated:=fCount+fCount;
  SetLength(fItems,fAllocated);
 end;
 fItems[result]:=pItem;
end;

procedure TpvDynamicArray<T>.Insert(const pIndex:TpvSizeInt;const pItem:T);
begin
 if pIndex>=0 then begin
  if pIndex<fCount then begin
   inc(fCount);
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
   Move(fItems[pIndex],fItems[pIndex+1],(fCount-pIndex)*SizeOf(T));
   FillChar(fItems[pIndex],SizeOf(T),#0);
  end else begin
   fCount:=pIndex+1;
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
  end;
  fItems[pIndex]:=pItem;
 end;
end;

procedure TpvDynamicArray<T>.Delete(const pIndex:TpvSizeInt);
begin
 Finalize(fItems[pIndex]);
 Move(fItems[pIndex+1],fItems[pIndex],(fCount-pIndex)*SizeOf(T));
 dec(fCount);
 FillChar(fItems[fCount],SizeOf(T),#0);
 if fCount<(fAllocated shr 1) then begin
  fAllocated:=fAllocated shr 1;
  SetLength(fItems,fAllocated);
 end;
end;

procedure TpvDynamicArray<T>.Exchange(const pIndex,pWithIndex:TpvSizeInt);
var Temporary:T;
begin
 Temporary:=fItems[pIndex];
 fItems[pIndex]:=fItems[pWithIndex];
 fItems[pWithIndex]:=Temporary;
end;

function TpvDynamicArray<T>.Memory:pointer;
begin
 result:=@fItems[0];
end;

function TpvDynamicArray<T>.GetEnumerator:TpvDynamicArray<T>.TValueEnumerator;
begin
 result:=TValueEnumerator.Create(self);
end;

constructor TpvBaseList.Create(const pItemSize:TpvSizeInt);
begin
 inherited Create;
 fItemSize:=pItemSize;
 fCount:=0;
 fAllocated:=0;
 fMemory:=nil;
 fSorted:=false;
end;

destructor TpvBaseList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TpvBaseList.SetCount(const pNewCount:TpvSizeInt);
var Index,NewAllocated:TpvSizeInt;
    Item:pointer;
begin
 if fCount<pNewCount then begin
  NewAllocated:=RoundUpToPowerOfTwoSizeUInt(pNewCount);
  if fAllocated<NewAllocated then begin
   if assigned(fMemory) then begin
    ReallocMem(fMemory,NewAllocated*fItemSize);
   end else begin
    GetMem(fMemory,NewAllocated*fItemSize);
   end;
   FillChar(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(fAllocated)*TpvPtrUInt(fItemSize))))^,(NewAllocated-fAllocated)*fItemSize,#0);
   fAllocated:=NewAllocated;
  end;
  Item:=fMemory;
  Index:=fCount;
  inc(TpvPtrUInt(Item),Index*fItemSize);
  while Index<pNewCount do begin
   FillChar(Item^,fItemSize,#0);
   InitializeItem(Item^);
   inc(TpvPtrUInt(Item),fItemSize);
   inc(Index);
  end;
  fCount:=pNewCount;
 end else if fCount>pNewCount then begin
  Item:=fMemory;
  Index:=pNewCount;
  inc(TpvPtrUInt(Item),Index*fItemSize);
  while Index<fCount do begin
   FinalizeItem(Item^);
   FillChar(Item^,fItemSize,#0);
   inc(TpvPtrUInt(Item),fItemSize);
   inc(Index);
  end;
  fCount:=pNewCount;
  if pNewCount<(fAllocated shr 2) then begin
   if pNewCount=0 then begin
    if assigned(fMemory) then begin
     FreeMem(fMemory);
     fMemory:=nil;
    end;
    fAllocated:=0;
   end else begin
    NewAllocated:=fAllocated shr 1;
    if assigned(fMemory) then begin
     ReallocMem(fMemory,NewAllocated*fItemSize);
    end else begin
     GetMem(fMemory,NewAllocated*fItemSize);
    end;
    fAllocated:=NewAllocated;
   end;
  end;
 end;
 fSorted:=false;
end;

function TpvBaseList.GetItem(const pIndex:TpvSizeInt):pointer;
begin
 if (pIndex>=0) and (pIndex<fCount) then begin
  result:=pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))));
 end else begin
  result:=nil;
 end;
end;

procedure TpvBaseList.InitializeItem(var pItem);
begin
end;

procedure TpvBaseList.FinalizeItem(var pItem);
begin
end;

procedure TpvBaseList.CopyItem(const pSource;var pDestination);
begin
 Move(pSource,pDestination,fItemSize);
end;

procedure TpvBaseList.ExchangeItem(var pSource,pDestination);
var a,b:PpvUInt8;
    c8:TpvUInt8;
    c32:TpvUInt32;
    Index:TpvInt32;
begin
 a:=@pSource;
 b:=@pDestination;
 for Index:=1 to fItemSize shr 2 do begin
  c32:=PpvUInt32(a)^;
  PpvUInt32(a)^:=PpvUInt32(b)^;
  PpvUInt32(b)^:=c32;
  inc(PpvUInt32(a));
  inc(PpvUInt32(b));
 end;
 for Index:=1 to fItemSize and 3 do begin
  c8:=a^;
  a^:=b^;
  b^:=c8;
  inc(a);
  inc(b);
 end;
end;

function TpvBaseList.CompareItem(const pSource,pDestination):TpvInt32;
var a,b:PpvUInt8;
    Index:TpvInt32;
begin
 result:=0;
 a:=@pSource;
 b:=@pDestination;
 for Index:=1 to fItemSize do begin
  result:=a^-b^;
  if result<>0 then begin
   exit;
  end;
  inc(a);
  inc(b);
 end;
end;

procedure TpvBaseList.Clear;
var Index:TpvSizeInt;
    Item:pointer;
begin
 Item:=fMemory;
 Index:=0;
 while Index<fCount do begin
  FinalizeItem(Item^);
  inc(TpvPtrInt(Item),fItemSize);
  inc(Index);
 end;
 if assigned(fMemory) then begin
  FreeMem(fMemory);
  fMemory:=nil;
 end;
 fCount:=0;
 fAllocated:=0;
 fSorted:=false;
end;

procedure TpvBaseList.FillWith(const pSourceData;const pSourceCount:TpvSizeInt);
var Index:TpvSizeInt;
    SourceItem,Item:pointer;
begin
 SourceItem:=@pSourceData;
 if assigned(SourceItem) and (pSourceCount>0) then begin
  SetCount(pSourceCount);
  Item:=fMemory;
  Index:=0;
  while Index<fCount do begin
   CopyItem(SourceItem^,Item^);
   inc(TpvPtrInt(SourceItem),fItemSize);
   inc(TpvPtrInt(Item),fItemSize);
   inc(Index);
  end;
 end else begin
  SetCount(0);
 end;
 fSorted:=false;
end;

function TpvBaseList.IndexOf(const pItem):TpvSizeInt;
var Index,LowerIndexBound,UpperIndexBound,Difference:TpvInt32;
begin
 result:=-1;
 if fSorted then begin
  LowerIndexBound:=0;
  UpperIndexBound:=fCount-1;
  while LowerIndexBound<=UpperIndexBound do begin
   Index:=LowerIndexBound+((UpperIndexBound-LowerIndexBound) shr 1);
   Difference:=CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Index)*TpvPtrUInt(fItemSize))))^,pItem);
   if Difference=0 then begin
    result:=Index;
    exit;
   end else if Difference<0 then begin
    LowerIndexBound:=Index+1;
   end else begin
    UpperIndexBound:=Index-1;
   end;
  end;
 end else begin
  Index:=0;
  while Index<fCount do begin
   if CompareItem(pItem,pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Index)*TpvPtrUInt(fItemSize))))^)=0 then begin
    result:=Index;
    break;
   end;
   inc(Index);
  end;
 end;
end;

function TpvBaseList.Add(const pItem):TpvSizeInt;
var Index,LowerIndexBound,UpperIndexBound,Difference:TpvInt32;
begin
 if fSorted and (fCount>0) then begin
  if CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(fCount-1)*TpvPtrUInt(fItemSize))))^,pItem)<0 then begin
   result:=fCount;
  end else if CompareItem(pItem,pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(0)*TpvPtrUInt(fItemSize))))^)<0 then begin
   result:=0;
  end else begin
   LowerIndexBound:=0;
   UpperIndexBound:=fCount-1;
   while LowerIndexBound<=UpperIndexBound do begin
    Index:=LowerIndexBound+((UpperIndexBound-LowerIndexBound) shr 1);
    Difference:=CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Index)*TpvPtrUInt(fItemSize))))^,pItem);
    if Difference=0 then begin
     LowerIndexBound:=Index;
     break;
    end else if Difference<0 then begin
     LowerIndexBound:=Index+1;
    end else begin
     UpperIndexBound:=Index-1;
    end;
   end;
   result:=LowerIndexBound;
  end;
  if result>=0 then begin
   Insert(result,pItem);
   fSorted:=true;
  end;
 end else begin
  result:=fCount;
  SetCount(result+1);
  CopyItem(pItem,pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(result)*TpvPtrUInt(fItemSize))))^);
  fSorted:=false;
 end;
end;

procedure TpvBaseList.Insert(const pIndex:TpvSizeInt;const pItem);
begin
 if pIndex>=0 then begin
  if pIndex<fCount then begin
   SetCount(fCount+1);
   Move(pointer(TpvPtrInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex+1)*TpvPtrUInt(fItemSize))))^,(fCount-(pIndex+1))*fItemSize);
   FillChar(pointer(TpvPtrInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,fItemSize,#0);
  end else begin
   SetCount(pIndex+1);
  end;
  CopyItem(pItem,pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^);
 end;
 fSorted:=false;
end;

procedure TpvBaseList.Delete(const pIndex:TpvSizeInt);
var OldSorted:boolean;
begin
 if (pIndex>=0) and (pIndex<fCount) then begin
  OldSorted:=fSorted;
  FinalizeItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^);
  Move(pointer(TpvPtrUInt(TpvPtruInt(fMemory)+(TpvPtrUInt(pIndex+1)*TpvPtrUInt(fItemSize))))^,pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,(fCount-pIndex)*fItemSize);
  FillChar(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(fCount-1)*TpvPtrUInt(fItemSize))))^,fItemSize,#0);
  SetCount(fCount-1);
  fSorted:=OldSorted;
 end;
end;

procedure TpvBaseList.Remove(const pItem);
var Index:TpvSizeInt;
begin
 repeat
  Index:=IndexOf(pItem);
  if Index>=0 then begin
   Delete(Index);
  end else begin
   break;
  end;
 until false;
end;

procedure TpvBaseList.Exchange(const pIndex,pWithIndex:TpvSizeInt);
begin
 if (pIndex>=0) and (pIndex<fCount) and (pWithIndex>=0) and (pWithIndex<fCount) then begin
  ExchangeItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pWithIndex)*TpvPtrUInt(fItemSize))))^);
  fSorted:=false;
 end;
end;

procedure TpvBaseList.Sort;
type PByteArray=^TByteArray;
     TByteArray=array[0..$3fffffff] of TpvUInt8;
     PStackItem=^TStackItem;
     TStackItem=record
      Left,Right,Depth:TpvInt32;
     end;
var Left,Right,Depth,i,j,Middle,Size,Parent,Child,Pivot,iA,iB,iC:TpvInt32;
    StackItem:PStackItem;
    Stack:array[0..31] of TStackItem;
begin
 if not fSorted then begin
  if fCount>1 then begin
   StackItem:=@Stack[0];
   StackItem^.Left:=0;
   StackItem^.Right:=fCount-1;
   StackItem^.Depth:=IntLog2(fCount) shl 1;
   inc(StackItem);
   while TpvPtrUInt(pointer(StackItem))>TpvPtrUInt(pointer(@Stack[0])) do begin
    dec(StackItem);
    Left:=StackItem^.Left;
    Right:=StackItem^.Right;
    Depth:=StackItem^.Depth;
    Size:=(Right-Left)+1;
    if Size<16 then begin
     // Insertion sort
     iA:=Left;
     iB:=iA+1;
     while iB<=Right do begin
      iC:=iB;
      while (iA>=Left) and
            (iC>=Left) and
            (CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(iA)*TpvPtrUInt(fItemSize))))^,
                         pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(iC)*TpvPtrUInt(fItemSize))))^)>0) do begin
       Exchange(iA,iC);
       dec(iA);
       dec(iC);
      end;
      iA:=iB;
      inc(iB);
     end;
    end else begin
     if (Depth=0) or (TpvPtrUInt(pointer(StackItem))>=TpvPtrUInt(pointer(@Stack[high(Stack)-1]))) then begin
      // Heap sort
      i:=Size div 2;
      repeat
       if i>0 then begin
        dec(i);
       end else begin
        dec(Size);
        if Size>0 then begin
         Exchange(Left+Size,Left);
        end else begin
         break;
        end;
       end;
       Parent:=i;
       repeat
        Child:=(Parent*2)+1;
        if Child<Size then begin
         if (Child<(Size-1)) and (CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Child)*TpvPtrUInt(fItemSize))))^,
                                              pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Child+1)*TpvPtrUInt(fItemSize))))^)<0) then begin
          inc(Child);
         end;
         if CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Parent)*TpvPtrUInt(fItemSize))))^,
                        pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Child)*TpvPtrUInt(fItemSize))))^)<0 then begin
          Exchange(Left+Parent,Left+Child);
          Parent:=Child;
          continue;
         end;
        end;
        break;
       until false;
      until false;
     end else begin
      // Quick sort width median-of-three optimization
      Middle:=Left+((Right-Left) shr 1);
      if (Right-Left)>3 then begin
       if CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left)*TpvPtrUInt(fItemSize))))^,
                      pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Middle)*TpvPtrUInt(fItemSize))))^)>0 then begin
        Exchange(Left,Middle);
       end;
       if CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left)*TpvPtrUInt(fItemSize))))^,
                      pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Right)*TpvPtrUInt(fItemSize))))^)>0 then begin
        Exchange(Left,Right);
       end;
       if CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Middle)*TpvPtrUInt(fItemSize))))^,
                      pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Right)*TpvPtrUInt(fItemSize))))^)>0 then begin
        Exchange(Middle,Right);
       end;
      end;
      Pivot:=Middle;
      i:=Left;
      j:=Right;
      repeat
       while (i<Right) and (CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(i)*TpvPtrUInt(fItemSize))))^,
                                        pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Pivot)*TpvPtrUInt(fItemSize))))^)<0) do begin
        inc(i);
       end;
       while (j>=i) and (CompareItem(pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(j)*TpvPtrUInt(fItemSize))))^,
                                     pointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Pivot)*TpvPtrUInt(fItemSize))))^)>0) do begin
        dec(j);
       end;
       if i>j then begin
        break;
       end else begin
        if i<>j then begin
         Exchange(i,j);
         if Pivot=i then begin
          Pivot:=j;
         end else if Pivot=j then begin
          Pivot:=i;
         end;
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
  end;
  fSorted:=true;
 end;
end;

constructor TpvObjectGenericList<T>.TValueEnumerator.Create(const aObjectList:TpvObjectGenericList<T>);
begin
 fObjectList:=aObjectList;
 fIndex:=-1;
end;

function TpvObjectGenericList<T>.TValueEnumerator.MoveNext:boolean;
begin
 inc(fIndex);
 result:=fIndex<fObjectList.fCount;
end;

function TpvObjectGenericList<T>.TValueEnumerator.GetCurrent:T;
begin
 result:=fObjectList.fItems[fIndex];
end;

constructor TpvObjectGenericList<T>.Create;
begin
 inherited Create;
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
 fOwnObjects:=true;
end;

destructor TpvObjectGenericList<T>.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TpvObjectGenericList<T>.Clear;
var Index:TpvSizeInt;
begin
 if fOwnObjects then begin
  for Index:=fCount-1 downto 0 do begin
   FreeAndNil(fItems[Index]);
  end;
 end;
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
end;

procedure TpvObjectGenericList<T>.SetCount(const pNewCount:TpvSizeInt);
var Index,NewAllocated:TpvSizeInt;
    Item:pointer;
begin
 if fCount<pNewCount then begin
  NewAllocated:=RoundUpToPowerOfTwoSizeUInt(pNewCount);
  if fAllocated<NewAllocated then begin
   SetLength(fItems,NewAllocated);
   FillChar(fItems[fAllocated],(NewAllocated-fAllocated)*SizeOf(T),#0);
   fAllocated:=NewAllocated;
  end;
  FillChar(fItems[fCount],(pNewCount-fCount)*SizeOf(T),#0);
  fCount:=pNewCount;
 end else if fCount>pNewCount then begin
  if fOwnObjects then begin
   for Index:=fCount-1 downto pNewCount do begin
    FreeAndNil(fItems[Index]);
   end;
  end;
  fCount:=pNewCount;
  if pNewCount<(fAllocated shr 2) then begin
   if pNewCount=0 then begin
    fItems:=nil;
    fAllocated:=0;
   end else begin
    NewAllocated:=fAllocated shr 1;
    SetLength(fItems,NewAllocated);
    fAllocated:=NewAllocated;
   end;
  end;
 end;
end;

function TpvObjectGenericList<T>.GetItem(const pIndex:TpvSizeInt):T;
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 result:=fItems[pIndex];
end;

procedure TpvObjectGenericList<T>.SetItem(const pIndex:TpvSizeInt;const pItem:T);
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 fItems[pIndex]:=pItem;
end;

function TpvObjectGenericList<T>.IndexOf(const pItem:T):TpvSizeInt;
var Index:TpvInt32;
begin
 for Index:=0 to fCount-1 do begin
  if fItems[Index]=pItem then begin
   result:=Index;
   exit;
  end;
 end;
 result:=-1;
end;

function TpvObjectGenericList<T>.Add(const pItem:T):TpvSizeInt;
begin
 result:=fCount;
 inc(fCount);
 if fAllocated<fCount then begin
  fAllocated:=fCount+fCount;
  SetLength(fItems,fAllocated);
 end;
 fItems[result]:=pItem;
end;

procedure TpvObjectGenericList<T>.Insert(const pIndex:TpvSizeInt;const pItem:T);
begin
 if pIndex>=0 then begin
  if pIndex<fCount then begin
   inc(fCount);
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
   Move(fItems[pIndex],fItems[pIndex+1],(fCount-(pIndex+1))*SizeOf(T));
   FillChar(fItems[pIndex],SizeOf(T),#0);
  end else begin
   fCount:=pIndex+1;
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
  end;
  fItems[pIndex]:=pItem;
 end;
end;

procedure TpvObjectGenericList<T>.Delete(const pIndex:TpvSizeInt);
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 FreeANdNil(fItems[pIndex]);
 Move(fItems[pIndex+1],fItems[pIndex],(fCount-pIndex)*SizeOf(T));
 dec(fCount);
 FillChar(fItems[fCount],SizeOf(T),#0);
 if fCount<(fAllocated shr 1) then begin
  fAllocated:=fAllocated shr 1;
  SetLength(fItems,fAllocated);
 end;
end;

procedure TpvObjectGenericList<T>.Remove(const pItem:T);
var Index:TpvSizeInt;
begin
 Index:=IndexOf(pItem);
 if Index>=0 then begin
  Delete(Index);
 end;
end;

procedure TpvObjectGenericList<T>.Exchange(const pIndex,pWithIndex:TpvSizeInt);
var Temporary:T;
begin
 if ((pIndex<0) or (pIndex>=fCount)) or ((pWithIndex<0) or (pWithIndex>=fCount)) then begin
  raise ERangeError.Create('Out of index range');
 end;
 Temporary:=fItems[pIndex];
 fItems[pIndex]:=fItems[pWithIndex];
 fItems[pWithIndex]:=Temporary;
end;

function TpvObjectGenericList<T>.GetEnumerator:TpvObjectGenericList<T>.TValueEnumerator;
begin
 result:=TValueEnumerator.Create(self);
end;

constructor TpvGenericList<T>.TValueEnumerator.Create(const aGenericList:TpvGenericList<T>);
begin
 fGenericList:=aGenericList;
 fIndex:=-1;
end;

function TpvGenericList<T>.TValueEnumerator.MoveNext:boolean;
begin
 inc(fIndex);
 result:=fIndex<fGenericList.fCount;
end;

function TpvGenericList<T>.TValueEnumerator.GetCurrent:T;
begin
 result:=fGenericList.fItems[fIndex];
end;

constructor TpvGenericList<T>.Create;
begin
 inherited Create;
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
 fSorted:=false;
end;

destructor TpvGenericList<T>.Destroy;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
 fSorted:=false;
 inherited Destroy;
end;

procedure TpvGenericList<T>.Clear;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
 fSorted:=false;
end;

procedure TpvGenericList<T>.SetCount(const pNewCount:TpvSizeInt);
var Index,NewAllocated:TpvSizeInt;
    Item:pointer;
begin
 if fCount<pNewCount then begin
  NewAllocated:=RoundUpToPowerOfTwoSizeUInt(pNewCount);
  if fAllocated<NewAllocated then begin
   SetLength(fItems,NewAllocated);
   FillChar(fItems[fAllocated],(NewAllocated-fAllocated)*SizeOf(T),#0);
   fAllocated:=NewAllocated;
  end;
  for Index:=fCount to pNewCount-1 do begin
   FillChar(fItems[Index],SizeOf(T),#0);
   Initialize(fItems[Index]);
  end;
  fCount:=pNewCount;
 end else if fCount>pNewCount then begin
  for Index:=pNewCount to fCount-1 do begin
   Finalize(fItems[Index]);
   FillChar(fItems[Index],SizeOf(T),#0);
  end;
  fCount:=pNewCount;
  if pNewCount<(fAllocated shr 2) then begin
   if pNewCount=0 then begin
    fItems:=nil;
    fAllocated:=0;
   end else begin
    NewAllocated:=fAllocated shr 1;
    SetLength(fItems,NewAllocated);
    fAllocated:=NewAllocated;
   end;
  end;
 end;
 fSorted:=false;
end;

function TpvGenericList<T>.GetItem(const pIndex:TpvSizeInt):T;
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 result:=fItems[pIndex];
end;

procedure TpvGenericList<T>.SetItem(const pIndex:TpvSizeInt;const pItem:T);
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 fItems[pIndex]:=pItem;
end;

procedure TpvGenericList<T>.Assign(const pFrom:TpvGenericList<T>);
begin
 fItems:=pFrom.fItems;
 fCount:=pFrom.Count;
 fAllocated:=pFrom.fAllocated;
 fSorted:=pFrom.fSorted;
end;

function TpvGenericList<T>.IndexOf(const pItem:T):TpvSizeInt;
var Index,LowerIndexBound,UpperIndexBound,Difference:TpvInt32;
    Comparer:IComparer<T>;
begin
 Comparer:=TComparer<T>.Default;
 result:=-1;
 if fSorted then begin
  LowerIndexBound:=0;
  UpperIndexBound:=fCount-1;
  while LowerIndexBound<=UpperIndexBound do begin
   Index:=LowerIndexBound+((UpperIndexBound-LowerIndexBound) shr 1);
   Difference:=Comparer.Compare(fItems[Index],pItem);
   if Difference=0 then begin
    result:=Index;
    exit;
   end else if Difference<0 then begin
    LowerIndexBound:=Index+1;
   end else begin
    UpperIndexBound:=Index-1;
   end;
  end;
 end else begin
  for Index:=0 to fCount-1 do begin
   if Comparer.Compare(fItems[Index],pItem)=0 then begin
    result:=Index;
    exit;
   end;
  end;
 end;
end;

function TpvGenericList<T>.Add(const pItem:T):TpvSizeInt;
var Index,LowerIndexBound,UpperIndexBound,Difference:TpvInt32;
    Comparer:IComparer<T>;
begin
 Comparer:=TComparer<T>.Default;
 if fSorted and (fCount>0) then begin
  if Comparer.Compare(fItems[fCount-1],pItem)<0 then begin
   result:=fCount;
  end else if Comparer.Compare(pItem,fItems[0])<0 then begin
   result:=0;
  end else begin
   LowerIndexBound:=0;
   UpperIndexBound:=fCount-1;
   while LowerIndexBound<=UpperIndexBound do begin
    Index:=LowerIndexBound+((UpperIndexBound-LowerIndexBound) shr 1);
    Difference:=Comparer.Compare(fItems[Index],pItem);
    if Difference=0 then begin
     LowerIndexBound:=Index;
     break;
    end else if Difference<0 then begin
     LowerIndexBound:=Index+1;
    end else begin
     UpperIndexBound:=Index-1;
    end;
   end;
   result:=LowerIndexBound;
  end;
  if result>=0 then begin
   if result<fCount then begin
    inc(fCount);
    if length(fItems)<fCount then begin
     SetLength(fItems,fCount*2);
    end;
    Move(fItems[result],fItems[result+1],(fCount-(result+1))*SizeOf(T));
    FillChar(fItems[result],SizeOf(T),#0);
   end else begin
    fCount:=result+1;
    if length(fItems)<fCount then begin
     SetLength(fItems,fCount*2);
    end;
   end;
   fItems[result]:=pItem;
  end;
 end else begin
  result:=fCount;
  inc(fCount);
  if fAllocated<fCount then begin
   fAllocated:=fCount+fCount;
   SetLength(fItems,fAllocated);
  end;
  fItems[result]:=pItem;
 end;
end;

procedure TpvGenericList<T>.Insert(const pIndex:TpvSizeInt;const pItem:T);
begin
 if pIndex>=0 then begin
  if pIndex<fCount then begin
   inc(fCount);
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
   Move(fItems[pIndex],fItems[pIndex+1],(fCount-(pIndex+1))*SizeOf(T));
   FillChar(fItems[pIndex],SizeOf(T),#0);
  end else begin
   fCount:=pIndex+1;
   if fCount<fAllocated then begin
    fAllocated:=fCount shl 1;
    SetLength(fItems,fAllocated);
   end;
  end;
  fItems[pIndex]:=pItem;
 end;
 fSorted:=false;
end;

procedure TpvGenericList<T>.Delete(const pIndex:TpvSizeInt);
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 Finalize(fItems[pIndex]);
 Move(fItems[pIndex+1],fItems[pIndex],(fCount-pIndex)*SizeOf(T));
 dec(fCount);
 FillChar(fItems[fCount],SizeOf(T),#0);
 if fCount<(fAllocated shr 1) then begin
  fAllocated:=fAllocated shr 1;
  SetLength(fItems,fAllocated);
 end;
end;

procedure TpvGenericList<T>.Remove(const pItem:T);
var Index:TpvSizeInt;
begin
 Index:=IndexOf(pItem);
 if Index>=0 then begin
  Delete(Index);
 end;
end;

procedure TpvGenericList<T>.Exchange(const pIndex,pWithIndex:TpvSizeInt);
var Temporary:T;
begin
 if ((pIndex<0) or (pIndex>=fCount)) or ((pWithIndex<0) or (pWithIndex>=fCount)) then begin
  raise ERangeError.Create('Out of index range');
 end;
 Temporary:=fItems[pIndex];
 fItems[pIndex]:=fItems[pWithIndex];
 fItems[pWithIndex]:=Temporary;
 fSorted:=false;
end;

function TpvGenericList<T>.GetEnumerator:TpvGenericList<T>.TValueEnumerator;
begin
 result:=TValueEnumerator.Create(self);
end;

procedure TpvGenericList<T>.Sort;
begin
 if not fSorted then begin
  if fCount>1 then begin
   TpvTypedSort<T>.IntroSort(@fItems[0],0,fCount-1);
  end;
  fSorted:=true;
 end;
end;

constructor TpvCustomHandleMap.Create(const pDataSize:TpvSizeUInt);
begin
 inherited Create;
 fMultipleReaderSingleWriterLock:=TPasMPMultipleReaderSingleWriterLock.Create;
 fDataSize:=pDataSize;
 fSize:=0;
 fIndexCounter:=0;
 fDenseIndex:=0;
 fFreeIndex:=0;
 fFreeArray:=nil;
{$ifdef Debug}
 fGenerationArray:=nil;
{$endif}
 fSparseToDenseArray:=nil;
 fDenseToSparseArray:=nil;
 fDataArray:=nil;
end;

destructor TpvCustomHandleMap.Destroy;
begin
 Clear;
 fMultipleReaderSingleWriterLock.Free;
 inherited Destroy;
end;

procedure TpvCustomHandleMap.Lock;
begin
 fMultipleReaderSingleWriterLock.AcquireWrite;
end;

procedure TpvCustomHandleMap.Unlock;
begin
 fMultipleReaderSingleWriterLock.ReleaseWrite;
end;

procedure TpvCustomHandleMap.Clear;
var Index:TpvSizeUInt;
begin
 fMultipleReaderSingleWriterLock.AcquireWrite;
 try
  Index:=0;
  while Index<fSize do begin
   FinalizeHandleData(pointer(@fDataArray[Index*TpvSizeUInt(fDataSize)])^);
   inc(Index);
  end;
  fSize:=0;
  fIndexCounter:=0;
  fDenseIndex:=0;
  fFreeIndex:=0;
  fFreeArray:=nil;
{$ifdef Debug}
  fGenerationArray:=nil;
{$endif}
  fSparseToDenseArray:=nil;
  fDenseToSparseArray:=nil;
  fDataArray:=nil;
 finally
  fMultipleReaderSingleWriterLock.ReleaseWrite;
 end;
end;

procedure TpvCustomHandleMap.InitializeHandleData(var pData);
begin
end;

procedure TpvCustomHandleMap.FinalizeHandleData(var pData);
begin
 FillChar(pData,fDataSize,#$00);
end;

procedure TpvCustomHandleMap.CopyHandleData(const pSource;out pDestination);
begin
 Move(pSource,pDestination,fDataSize);
end;

procedure TpvCustomHandleMap.Defragment;
begin

end;

function TpvCustomHandleMap.AllocateHandle:TpvHandle;
var OldSize,NewSize:TpvSizeUInt;
begin
 fMultipleReaderSingleWriterLock.AcquireWrite;
 try
  if fFreeIndex>0 then begin
   dec(fFreeIndex);
   result.Index:=fFreeArray[fFreeIndex];
{$ifdef Debug}
   result.Generation:=fGenerationArray[result.Index] and $7fffffff;
{$endif}
  end else begin
   result.Index:=TPasMPInterlocked.Increment(fIndexCounter);
   result.Generation:=0;
   NewSize:=TpvSizeUInt(result.Index)+1;
{$ifdef Debug}
   begin
    OldSize:=length(fGenerationArray);
    if OldSize<NewSize then begin
     SetLength(fGenerationArray,NewSize*2);
     FillChar(fGenerationArray[OldSize],TpvSizeUInt(NewSize-OldSize)*TpvSizeUInt(SizeOf(TpvUInt32)),#$ff);
    end;
   end;
{$endif}
   begin
    OldSize:=fSize;
    if OldSize<NewSize then begin
     fSize:=NewSize*2;
     SetLength(fSparseToDenseArray,fSize);
     SetLength(fDenseToSparseArray,fSize);
     SetLength(fDataArray,fSize*fDataSize);
     FillChar(fSparseToDenseArray[OldSize],TpvSizeUInt(NewSize-OldSize)*TpvSizeUInt(SizeOf(TpvUInt32)),#$ff);
     FillChar(fDenseToSparseArray[OldSize],TpvSizeUInt(NewSize-OldSize)*TpvSizeUInt(SizeOf(TpvUInt32)),#$ff);
     FillChar(fDataArray[OldSize*fDataSize],TpvSizeUInt(NewSize-OldSize)*TpvSizeUInt(fDataSize),#$00);
    end;
   end;
  end;
{$ifdef Debug}
  fGenerationArray[result.Index]:=result.Generation;
{$endif}
  fSparseToDenseArray[result.Index]:=fDenseIndex;
  fDenseToSparseArray[fDenseIndex]:=result.Index;
  InitializeHandleData(pointer(@fDataArray[fDenseIndex*TpvSizeUInt(fDataSize)])^);
  inc(fDenseIndex);
 finally
  fMultipleReaderSingleWriterLock.ReleaseWrite;
 end;
end;

procedure TpvCustomHandleMap.FreeHandle(const ppvHandle:TpvHandle);
var DenseIndex:TpvUInt32;
begin
 fMultipleReaderSingleWriterLock.AcquireWrite;
 try
{$ifdef Debug}
  if (ppvHandle.Index<TpvInt64(length(fGenerationArray))) and
     (ppvHandle.Generation=fGenerationArray[ppvHandle.Index]) then begin
   fGenerationArray[ppvHandle.Index]:=(fGenerationArray[ppvHandle.Index]+1) or $80000000;
{$endif}
   if TpvSizeUInt(length(fFreeArray))<=TpvSizeUInt(fFreeIndex) then begin
    SetLength(fFreeArray,(TpvSizeUInt(fFreeIndex)+1)*2);
   end;
   fFreeArray[fFreeIndex]:=ppvHandle.Index;
   inc(fFreeIndex);
   dec(fDenseIndex);
   DenseIndex:=fSparseToDenseArray[ppvHandle.Index];
   if fDenseIndex<>DenseIndex then begin
    Move(fDataArray[fDenseIndex*TpvSizeUInt(fDataSize)],fDataArray[DenseIndex*TpvSizeUInt(fDataSize)],fDataSize);
   end;
   FinalizeHandleData(pointer(@fDataArray[fDenseIndex*TpvSizeUInt(fDataSize)])^);
   fSparseToDenseArray[fDenseToSparseArray[DenseIndex]]:=DenseIndex;
{$ifdef Debug}
  end else begin
   raise EpvHandleMap.Create('Freeing non-used or already-freed handle');
  end;
{$endif}
 finally
  fMultipleReaderSingleWriterLock.ReleaseWrite;
 end;
end;

procedure TpvCustomHandleMap.GetHandleData(const ppvHandle:TpvHandle;out pData);
begin
 fMultipleReaderSingleWriterLock.AcquireRead;
 try
{$ifdef Debug}
  if (ppvHandle.Index<TpvInt64(length(fGenerationArray))) and
     (ppvHandle.Generation=fGenerationArray[ppvHandle.Index]) then begin
{$endif}
   CopyHandleData(fDataArray[fSparseToDenseArray[ppvHandle.Index]*TpvSizeUInt(fDataSize)],pData);
{$ifdef Debug}
  end else begin
   raise EpvHandleMap.Create('Accessing non-used or already-freed handle');
  end;
{$endif}
 finally
  fMultipleReaderSingleWriterLock.ReleaseRead;
 end;
end;

procedure TpvCustomHandleMap.SetHandleData(const ppvHandle:TpvHandle;const pData);
begin
 fMultipleReaderSingleWriterLock.AcquireRead;
 try
{$ifdef Debug}
  if (ppvHandle.Index<TpvInt64(length(fGenerationArray))) and
     (ppvHandle.Generation=fGenerationArray[ppvHandle.Index]) then begin
{$endif}
   CopyHandleData(pData,fDataArray[fSparseToDenseArray[ppvHandle.Index]*TpvSizeUInt(fDataSize)]);
{$ifdef Debug}
  end else begin
   raise EpvHandleMap.Create('Accessing non-used or already-freed handle');
  end;
{$endif}
 finally
  fMultipleReaderSingleWriterLock.ReleaseRead;
 end;
end;

function TpvCustomHandleMap.GetHandleDataPointer(const ppvHandle:TpvHandle):pointer;
begin
{$ifdef Debug}
 if (ppvHandle.Index<TpvInt64(length(fGenerationArray))) and
    (ppvHandle.Generation=fGenerationArray[ppvHandle.Index]) then begin
{$endif}
  result:=@fDataArray[fSparseToDenseArray[ppvHandle.Index]*TpvSizeUInt(fDataSize)];
{$ifdef Debug}
 end else begin
  result:=nil;
  raise EpvHandleMap.Create('Accessing non-used or already-freed handle');
 end;
{$endif}
end;

{$warnings off}
{$hints off}

constructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapEntityEnumerator.Create(const aHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
begin
 fHashMap:=aHashMap;
 fIndex:=-1;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapEntityEnumerator.GetCurrent:TpvHashMapEntity;
begin
 result:=fHashMap.fEntities[fIndex];
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapEntityEnumerator.MoveNext:boolean;
begin
 repeat
  inc(fIndex);
  if fIndex<fHashMap.fSize then begin
   if fHashMap.fEntityToCellIndex[fIndex]<>CELL_EMPTY then begin
    result:=true;
    exit;
   end;
  end else begin
   break;
  end;
 until false;
 result:=false;
end;

constructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapKeyEnumerator.Create(const aHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
begin
 fHashMap:=aHashMap;
 fIndex:=-1;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapKeyEnumerator.GetCurrent:TpvHashMapKey;
begin
 result:=fHashMap.fEntities[fIndex].Key;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapKeyEnumerator.MoveNext:boolean;
begin
 repeat
  inc(fIndex);
  if fIndex<fHashMap.fSize then begin
   if fHashMap.fEntityToCellIndex[fIndex]<>CELL_EMPTY then begin
    result:=true;
    exit;
   end;
  end else begin
   break;
  end;
 until false;
 result:=false;
end;

constructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapValueEnumerator.Create(const aHashMap:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
begin
 fHashMap:=aHashMap;
 fIndex:=-1;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapValueEnumerator.GetCurrent:TpvHashMapValue;
begin
 result:=fHashMap.fEntities[fIndex].Value;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapValueEnumerator.MoveNext:boolean;
begin
 repeat
  inc(fIndex);
  if fIndex<fHashMap.fSize then begin
   if fHashMap.fEntityToCellIndex[fIndex]<>CELL_EMPTY then begin
    result:=true;
    exit;
   end;
  end else begin
   break;
  end;
 until false;
 result:=false;
end;

constructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapEntitiesObject.Create(const aOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
begin
 inherited Create;
 fOwner:=aOwner;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapEntitiesObject.GetEnumerator:TpvHashMapEntityEnumerator;
begin
 result:=TpvHashMapEntityEnumerator.Create(fOwner);
end;

constructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapKeysObject.Create(const aOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
begin
 inherited Create;
 fOwner:=aOwner;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapKeysObject.GetEnumerator:TpvHashMapKeyEnumerator;
begin
 result:=TpvHashMapKeyEnumerator.Create(fOwner);
end;

constructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapValuesObject.Create(const aOwner:TpvHashMap<TpvHashMapKey,TpvHashMapValue>);
begin
 inherited Create;
 fOwner:=aOwner;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapValuesObject.GetEnumerator:TpvHashMapValueEnumerator;
begin
 result:=TpvHashMapValueEnumerator.Create(fOwner);
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapValuesObject.GetValue(const Key:TpvHashMapKey):TpvHashMapValue;
begin
 result:=fOwner.GetValue(Key);
end;

procedure TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TpvHashMapValuesObject.SetValue(const Key:TpvHashMapKey;const aValue:TpvHashMapValue);
begin
 fOwner.SetValue(Key,aValue);
end;

constructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.Create(const DefaultValue:TpvHashMapValue);
begin
 inherited Create;
 fRealSize:=0;
 fLogSize:=0;
 fSize:=0;
 fEntities:=nil;
 fEntityToCellIndex:=nil;
 fCellToEntityIndex:=nil;
 fDefaultValue:=DefaultValue;
 fCanShrink:=true;
 fEntitiesObject:=TpvHashMapEntitiesObject.Create(self);
 fKeysObject:=TpvHashMapKeysObject.Create(self);
 fValuesObject:=TpvHashMapValuesObject.Create(self);
 Resize;
end;

destructor TpvHashMap<TpvHashMapKey,TpvHashMapValue>.Destroy;
var Counter:TpvInt32;
begin
 Clear;
 for Counter:=0 to length(fEntities)-1 do begin
  Finalize(fEntities[Counter].Key);
  Finalize(fEntities[Counter].Value);
 end;
 SetLength(fEntities,0);
 SetLength(fEntityToCellIndex,0);
 SetLength(fCellToEntityIndex,0);
 FreeAndNil(fEntitiesObject);
 FreeAndNil(fKeysObject);
 FreeAndNil(fValuesObject);
 inherited Destroy;
end;

procedure TpvHashMap<TpvHashMapKey,TpvHashMapValue>.Clear;
var Counter:TpvInt32;
begin
 for Counter:=0 to length(fEntities)-1 do begin
  Finalize(fEntities[Counter].Key);
  Finalize(fEntities[Counter].Value);
 end;
 if fCanShrink then begin
  fRealSize:=0;
  fLogSize:=0;
  fSize:=0;
  SetLength(fEntities,0);
  SetLength(fEntityToCellIndex,0);
  SetLength(fCellToEntityIndex,0);
  Resize;
 end else begin
  for Counter:=0 to length(fCellToEntityIndex)-1 do begin
   fCellToEntityIndex[Counter]:=ENT_EMPTY;
  end;
  for Counter:=0 to length(fEntityToCellIndex)-1 do begin
   fEntityToCellIndex[Counter]:=CELL_EMPTY;
  end;
 end;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.HashData(const Data:pointer;const DataLength:TpvUInt32):TpvUInt32;
const m=TpvUInt32($57559429);
      n=TpvUInt32($5052acdb);
var b:PByte;
    h,k,len:TpvUInt32;
    p:TpvUInt64;
begin
 Len:=DataLength;
 h:=len;
 k:=h+n+1;
 if len>0 then begin
  b:=Data;
  while len>7 do begin
   begin
    p:=TpvUInt32(pointer(b)^)*TpvUInt64(n);
    h:=h xor TpvUInt32(p and $ffffffff);
    k:=k xor TpvUInt32(p shr 32);
    inc(b,4);
   end;
   begin
    p:=TpvUInt32(pointer(b)^)*TpvUInt64(m);
    k:=k xor TpvUInt32(p and $ffffffff);
    h:=h xor TpvUInt32(p shr 32);
    inc(b,4);
   end;
   dec(len,8);
  end;
  if len>3 then begin
   p:=TpvUInt32(pointer(b)^)*TpvUInt64(n);
   h:=h xor TpvUInt32(p and $ffffffff);
   k:=k xor TpvUInt32(p shr 32);
   inc(b,4);
   dec(len,4);
  end;
  if len>0 then begin
   if len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(len,2);
   end else begin
    p:=0;
   end;
   if len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*TpvUInt64(m);
   k:=k xor TpvUInt32(p and $ffffffff);
   h:=h xor TpvUInt32(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*TpvUInt64(n);
  h:=h xor TpvUInt32(p and $ffffffff);
  k:=k xor TpvUInt32(p shr 32);
 end;
 result:=k xor h;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.HashKey(const Key:TpvHashMapKey):TpvUInt32;
var p:TpvUInt64;
begin
 // We're hoping here that the compiler is here so smart, so that the compiler optimizes the
 // unused if-branches away
{$ifndef ExtraStringHashMap}
 if (SizeOf(TpvHashMapKey)=SizeOf(AnsiString)) and
    (TypeInfo(TpvHashMapKey)=TypeInfo(AnsiString)) then begin
  result:=HashData(PByte(@AnsiString(pointer(@Key)^)[1]),length(AnsiString(pointer(@Key)^))*SizeOf(AnsiChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UTF8String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UTF8String)) then begin
  result:=HashData(PByte(@UTF8String(pointer(@Key)^)[1]),length(UTF8String(pointer(@Key)^))*SizeOf(AnsiChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(RawByteString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(RawByteString)) then begin
  result:=HashData(PByte(@RawByteString(pointer(@Key)^)[1]),length(RawByteString(pointer(@Key)^))*SizeOf(AnsiChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(WideString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(WideString)) then begin
  result:=HashData(PByte(@WideString(pointer(@Key)^)[1]),length(WideString(pointer(@Key)^))*SizeOf(WideChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UnicodeString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UnicodeString)) then begin
  result:=HashData(PByte(@UnicodeString(pointer(@Key)^)[1]),length(UnicodeString(pointer(@Key)^))*SizeOf(UnicodeChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(String)) then begin
  result:=HashData(PByte(@String(pointer(@Key)^)[1]),length(String(pointer(@Key)^))*SizeOf(Char));
 end else if TypeInfo(TpvHashMapKey)=TypeInfo(TpvUUID) then begin
  result:=(TpvUUID(pointer(@Key)^).DoubleWords[0]*73856093) xor
          (TpvUUID(pointer(@Key)^).DoubleWords[1]*19349669) xor
          (TpvUUID(pointer(@Key)^).DoubleWords[2]*83492791) xor
          (TpvUUID(pointer(@Key)^).DoubleWords[3]*50331653);
 end else{$endif}begin
  case SizeOf(TpvHashMapKey) of
   SizeOf(UInt16):begin
    // 16-bit big => use 16-bit integer-rehashing
    result:=TpvUInt16(pointer(@Key)^);
    result:=(result or (((not result) and $ffff) shl 16));
    dec(result,result shl 6);
    result:=result xor (result shr 17);
    dec(result,result shl 9);
    result:=result xor (result shl 4);
    dec(result,result shl 3);
    result:=result xor (result shl 10);
    result:=result xor (result shr 15);
   end;
   SizeOf(TpvUInt32):begin
    // 32-bit big => use 32-bit integer-rehashing
    result:=TpvUInt32(pointer(@Key)^);
    dec(result,result shl 6);
    result:=result xor (result shr 17);
    dec(result,result shl 9);
    result:=result xor (result shl 4);
    dec(result,result shl 3);
    result:=result xor (result shl 10);
    result:=result xor (result shr 15);
   end;
   SizeOf(TpvUInt64):begin
    // 64-bit big => use 64-bit to 32-bit integer-rehashing
    p:=TpvUInt64(pointer(@Key)^);
    p:=(not p)+(p shl 18); // p:=((p shl 18)-p-)1;
    p:=p xor (p shr 31);
    p:=p*21; // p:=(p+(p shl 2))+(p shl 4);
    p:=p xor (p shr 11);
    p:=p+(p shl 6);
    result:=TpvUInt32(TpvPtrUInt(p xor (p shr 22)));
   end;
   else begin
    result:=HashData(PByte(pointer(@Key)),SizeOf(TpvHashMapKey));
   end;
  end;
 end;
{$if defined(CPU386) or defined(CPUAMD64)}
 // Special case: The hash value may be never zero
 result:=result or (-TpvUInt32(ord(result=0) and 1));
{$else}
 if result=0 then begin
  // Special case: The hash value may be never zero
  result:=$ffffffff;
 end;
{$ifend}
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.CompareKey(const KeyA,KeyB:TpvHashMapKey):boolean;
var Index:TpvInt32;
    pA,pB:PByte;
begin
{$ifdef ReleaseBuild}
 // We're hoping also here that the compiler is here so smart, so that the compiler optimizes the
 // unused if-branches away
{$ifndef ExtraStringHashMap}
 if (SizeOf(TpvHashMapKey)=SizeOf(AnsiString)) and
    (TypeInfo(TpvHashMapKey)=TypeInfo(AnsiString)) then begin
  result:=AnsiString(pointer(@KeyA)^)=AnsiString(pointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UTF8String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UTF8String)) then begin
  result:=UTF8String(pointer(@KeyA)^)=UTF8String(pointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(RawByteString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(RawByteString)) then begin
  result:=RawByteString(pointer(@KeyA)^)=RawByteString(pointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(WideString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(WideString)) then begin
  result:=WideString(pointer(@KeyA)^)=WideString(pointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UnicodeString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UnicodeString)) then begin
  result:=UnicodeString(pointer(@KeyA)^)=UnicodeString(pointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(String)) then begin
  result:=String(pointer(@KeyA)^)=String(pointer(@KeyB)^);
 end else{$endif}if SizeOf(TpvHashMapKey)=SizeOf(UInt8) then begin
  result:=UInt8(pointer(@KeyA)^)=UInt8(pointer(@KeyB)^);
 end else if SizeOf(TpvHashMapKey)=SizeOf(UInt16) then begin
  result:=UInt16(pointer(@KeyA)^)=UInt16(pointer(@KeyB)^);
 end else if SizeOf(TpvHashMapKey)=SizeOf(TpvUInt32) then begin
  result:=TpvUInt32(pointer(@KeyA)^)=TpvUInt32(pointer(@KeyB)^);
 end else if SizeOf(TpvHashMapKey)=SizeOf(TpvUInt64) then begin
  result:=TpvUInt64(pointer(@KeyA)^)=TpvUInt64(pointer(@KeyB)^);
{$ifdef fpc}
 end else if SizeOf(TpvHashMapKey)=SizeOf(TpvHashMapUInt128) then begin
  result:=(TpvHashMapUInt128(pointer(@KeyA)^)[0]=TpvHashMapUInt128(pointer(@KeyB)^)[0]) and
          (TpvHashMapUInt128(pointer(@KeyA)^)[1]=TpvHashMapUInt128(pointer(@KeyB)^)[1]);
{$endif}
 end else{$else}begin
  Index:=0;
  pA:=@KeyA;
  pB:=@KeyB;
  while (Index+SizeOf(TpvUInt32))<SizeOf(TpvHashMapKey) do begin
   if TpvUInt32(pointer(@pA[Index])^)<>TpvUInt32(pointer(@pB[Index])^) then begin
    result:=false;
    exit;
   end;
   inc(Index,SizeOf(TpvUInt32));
  end;
  while (Index+SizeOf(UInt8))<SizeOf(TpvHashMapKey) do begin
   if UInt8(pointer(@pA[Index])^)<>UInt8(pointer(@pB[Index])^) then begin
    result:=false;
    exit;
   end;
   inc(Index,SizeOf(UInt8));
  end;
  result:=true;
 end;
{$endif}
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.FindCell(const Key:TpvHashMapKey):TpvUInt32;
var HashCode,Mask,Step:TpvUInt32;
    Entity:TpvInt32;
begin
 HashCode:=HashKey(Key);
 Mask:=(2 shl fLogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if fLogSize<>0 then begin
  result:=HashCode shr (32-fLogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=fCellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and CompareKey(fEntities[Entity].Key,Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TpvHashMap<TpvHashMapKey,TpvHashMapValue>.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:TpvInt32;
    OldEntities:TpvHashMapEntities;
    OldCellToEntityIndex:TpvHashMapEntityIndices;
    OldEntityToCellIndex:TpvHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=fRealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 fSize:=0;
 fRealSize:=0;
 fLogSize:=NewLogSize;
 OldEntities:=fEntities;
 OldCellToEntityIndex:=fCellToEntityIndex;
 OldEntityToCellIndex:=fEntityToCellIndex;
 fEntities:=nil;
 fCellToEntityIndex:=nil;
 fEntityToCellIndex:=nil;
 SetLength(fEntities,2 shl fLogSize);
 SetLength(fCellToEntityIndex,2 shl fLogSize);
 SetLength(fEntityToCellIndex,2 shl fLogSize);
 for Counter:=0 to length(fCellToEntityIndex)-1 do begin
  fCellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(fEntityToCellIndex)-1 do begin
  fEntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
 for Counter:=0 to length(OldEntities)-1 do begin
  Finalize(OldEntities[Counter].Key);
  Finalize(OldEntities[Counter].Value);
 end;
 SetLength(OldEntities,0);
 SetLength(OldCellToEntityIndex,0);
 SetLength(OldEntityToCellIndex,0);
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.Add(const Key:TpvHashMapKey;const Value:TpvHashMapValue):PpvHashMapEntity;
var Entity:TpvInt32;
    Cell:TpvUInt32;
begin
 result:=nil;
 while fRealSize>=(1 shl fLogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@fEntities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=fSize;
 inc(fSize);
 if Entity<(2 shl fLogSize) then begin
  fCellToEntityIndex[Cell]:=Entity;
  fEntityToCellIndex[Entity]:=Cell;
  inc(fRealSize);
  result:=@fEntities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.Get(const Key:TpvHashMapKey;const CreateIfNotExist:boolean=false):PpvHashMapEntity;
var Entity:TpvInt32;
    Cell:TpvUInt32;
    Value:TpvHashMapValue;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@fEntities[Entity];
 end else if CreateIfNotExist then begin
  Initialize(Value);
  result:=Add(Key,Value);
 end;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.TryGet(const Key:TpvHashMapKey;out Value:TpvHashMapValue):boolean;
var Entity:TpvInt32;
begin
 Entity:=fCellToEntityIndex[FindCell(Key)];
 result:=Entity>=0;
 if result then begin
  Value:=fEntities[Entity].Value;
 end else begin
  Initialize(Value);
 end;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.ExistKey(const Key:TpvHashMapKey):boolean;
begin
 result:=fCellToEntityIndex[FindCell(Key)]>=0;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.Delete(const Key:TpvHashMapKey):boolean;
var Entity:TpvInt32;
    Cell:TpvUInt32;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  Finalize(fEntities[Entity].Key);
  Finalize(fEntities[Entity].Value);
  fEntityToCellIndex[Entity]:=CELL_DELETED;
  fCellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end;
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.GetValue(const Key:TpvHashMapKey):TpvHashMapValue;
var Entity:TpvInt32;
    Cell:TpvUInt32;
begin
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=fEntities[Entity].Value;
 end else begin
  result:=fDefaultValue;
 end;
end;

procedure TpvHashMap<TpvHashMapKey,TpvHashMapValue>.SetValue(const Key:TpvHashMapKey;const Value:TpvHashMapValue);
begin
 Add(Key,Value);
end;

{$ifdef ExtraStringHashMap}
constructor TpvStringHashMap<TpvHashMapValue>.Create(const DefaultValue:TpvHashMapValue);
begin
 inherited Create;
 fRealSize:=0;
 fLogSize:=0;
 fSize:=0;
 fEntities:=nil;
 fEntityToCellIndex:=nil;
 fCellToEntityIndex:=nil;
 fDefaultValue:=DefaultValue;
 fCanShrink:=true;
 Resize;
end;

destructor TpvStringHashMap<TpvHashMapValue>.Destroy;
var Counter:TpvInt32;
begin
 Clear;
 for Counter:=0 to length(fEntities)-1 do begin
  Finalize(fEntities[Counter].Key);
  Finalize(fEntities[Counter].Value);
 end;
 SetLength(fEntities,0);
 SetLength(fEntityToCellIndex,0);
 SetLength(fCellToEntityIndex,0);
 inherited Destroy;
end;

procedure TpvStringHashMap<TpvHashMapValue>.Clear;
var Counter:TpvInt32;
begin
 for Counter:=0 to length(fEntities)-1 do begin
  Finalize(fEntities[Counter].Key);
  Finalize(fEntities[Counter].Value);
 end;
 if fCanShrink then begin
  fRealSize:=0;
  fLogSize:=0;
  fSize:=0;
  SetLength(fEntities,0);
  SetLength(fEntityToCellIndex,0);
  SetLength(fCellToEntityIndex,0);
  Resize;
 end else begin
  for Counter:=0 to length(fCellToEntityIndex)-1 do begin
   fCellToEntityIndex[Counter]:=ENT_EMPTY;
  end;
  for Counter:=0 to length(fEntityToCellIndex)-1 do begin
   fEntityToCellIndex[Counter]:=CELL_EMPTY;
  end;
 end;
end;

function TpvStringHashMap<TpvHashMapValue>.HashKey(const Key:TpvHashMapKey):TpvUInt32;
const m=TpvUInt32($57559429);
      n=TpvUInt32($5052acdb);
var b:PByte;
    h,k,len:TpvUInt32;
    p:TpvUInt64;
begin
 len:=length(Key)*SizeOf(Key[1]);
 h:=len;
 k:=h+n+1;
 if len>0 then begin
  b:=PByte(@Key[1]);
  while len>7 do begin
   begin
    p:=TpvUInt32(pointer(b)^)*TpvUInt64(n);
    h:=h xor TpvUInt32(p and $ffffffff);
    k:=k xor TpvUInt32(p shr 32);
    inc(b,4);
   end;
   begin
    p:=TpvUInt32(pointer(b)^)*TpvUInt64(m);
    k:=k xor TpvUInt32(p and $ffffffff);
    h:=h xor TpvUInt32(p shr 32);
    inc(b,4);
   end;
   dec(len,8);
  end;
  if len>3 then begin
   p:=TpvUInt32(pointer(b)^)*TpvUInt64(n);
   h:=h xor TpvUInt32(p and $ffffffff);
   k:=k xor TpvUInt32(p shr 32);
   inc(b,4);
   dec(len,4);
  end;
  if len>0 then begin
   if len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(len,2);
   end else begin
    p:=0;
   end;
   if len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*TpvUInt64(m);
   k:=k xor TpvUInt32(p and $ffffffff);
   h:=h xor TpvUInt32(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*TpvUInt64(n);
  h:=h xor TpvUInt32(p and $ffffffff);
  k:=k xor TpvUInt32(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;

function TpvStringHashMap<TpvHashMapValue>.FindCell(const Key:TpvHashMapKey):TpvUInt32;
var HashCode,Mask,Step:TpvUInt32;
    Entity:TpvInt32;
begin
 HashCode:=HashKey(Key);
 Mask:=(2 shl fLogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if fLogSize<>0 then begin
  result:=HashCode shr (32-fLogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=fCellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (fEntities[Entity].Key=Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TpvStringHashMap<TpvHashMapValue>.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:TpvInt32;
    OldEntities:THashMapEntities;
    OldCellToEntityIndex:TpvHashMapEntityIndices;
    OldEntityToCellIndex:TpvHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=fRealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 fSize:=0;
 fRealSize:=0;
 fLogSize:=NewLogSize;
 OldEntities:=fEntities;
 OldCellToEntityIndex:=fCellToEntityIndex;
 OldEntityToCellIndex:=fEntityToCellIndex;
 fEntities:=nil;
 fCellToEntityIndex:=nil;
 fEntityToCellIndex:=nil;
 SetLength(fEntities,2 shl fLogSize);
 SetLength(fCellToEntityIndex,2 shl fLogSize);
 SetLength(fEntityToCellIndex,2 shl fLogSize);
 for Counter:=0 to length(fCellToEntityIndex)-1 do begin
  fCellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(fEntityToCellIndex)-1 do begin
  fEntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
 for Counter:=0 to length(OldEntities)-1 do begin
  Finalize(OldEntities[Counter].Key);
  Finalize(OldEntities[Counter].Value);
 end;
 SetLength(OldEntities,0);
 SetLength(OldCellToEntityIndex,0);
 SetLength(OldEntityToCellIndex,0);
end;

function TpvStringHashMap<TpvHashMapValue>.Add(const Key:TpvHashMapKey;const Value:TpvHashMapValue):PHashMapEntity;
var Entity:TpvInt32;
    Cell:TpvUInt32;
begin
 result:=nil;
 while fRealSize>=(1 shl fLogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@fEntities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=fSize;
 inc(fSize);
 if Entity<(2 shl fLogSize) then begin
  fCellToEntityIndex[Cell]:=Entity;
  fEntityToCellIndex[Entity]:=Cell;
  inc(fRealSize);
  result:=@fEntities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TpvStringHashMap<TpvHashMapValue>.Get(const Key:TpvHashMapKey;const CreateIfNotExist:boolean=false):PHashMapEntity;
var Entity:TpvInt32;
    Cell:TpvUInt32;
    Value:TpvHashMapValue;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@fEntities[Entity];
 end else if CreateIfNotExist then begin
  Initialize(Value);
  result:=Add(Key,Value);
 end;
end;

function TpvStringHashMap<TpvHashMapValue>.TryGet(const Key:TpvHashMapKey;out Value:TpvHashMapValue):boolean;
var Entity:TpvInt32;
begin
 Entity:=fCellToEntityIndex[FindCell(Key)];
 result:=Entity>=0;
 if result then begin
  Value:=fEntities[Entity].Value;
 end else begin
  Initialize(Value);
 end;
end;

function TpvStringHashMap<TpvHashMapValue>.ExistKey(const Key:TpvHashMapKey):boolean;
begin
 result:=fCellToEntityIndex[FindCell(Key)]>=0;
end;

function TpvStringHashMap<TpvHashMapValue>.Delete(const Key:TpvHashMapKey):boolean;
var Entity:TpvInt32;
    Cell:TpvUInt32;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  Finalize(fEntities[Entity].Key);
  Finalize(fEntities[Entity].Value);
  fEntityToCellIndex[Entity]:=CELL_DELETED;
  fCellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end;
end;

function TpvStringHashMap<TpvHashMapValue>.GetValue(const Key:TpvHashMapKey):TpvHashMapValue;
var Entity:TpvInt32;
    Cell:TpvUInt32;
begin
 Cell:=FindCell(Key);
 Entity:=fCellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=fEntities[Entity].Value;
 end else begin
  result:=fDefaultValue;
 end;
end;

procedure TpvStringHashMap<TpvHashMapValue>.SetValue(const Key:TpvHashMapKey;const Value:TpvHashMapValue);
begin
 Add(Key,Value);
end;
{$endif}

end.

