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
unit PasVulkan.Collections;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$legacyifend on}
{$endif}

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

type TpvDynamicArray<T>=record
      public
       Items:array of T;
       Count:TpvSizeInt;
       procedure Initialize;
       procedure Finalize;
       procedure Clear;
       procedure Resize(const aCount:TpvSizeInt);
       procedure Finish;
       procedure Assign(const aFrom:{$ifdef fpc}{$endif}TpvDynamicArray<T>); overload;
       procedure Assign(const aItems:array of T); overload;
       function AddNew:TpvSizeInt; overload;
       function Insert(const aIndex:TpvSizeInt;const aItem:T):TpvSizeInt; overload;
       function Add(const aItem:T):TpvSizeInt; overload;
       function Add(const aItems:array of T):TpvSizeInt; overload;
       function Add(const aFrom:{$ifdef fpc}{$endif}TpvDynamicArray<T>):TpvSizeInt; overload;
       function AddRangeFrom(const aFrom:{$ifdef fpc}{$endif}TpvDynamicArray<T>;const aStartIndex,aCount:TpvSizeInt):TpvSizeInt; overload;
       function AssignRangeFrom(const aFrom:{$ifdef fpc}{$endif}TpvDynamicArray<T>;const aStartIndex,aCount:TpvSizeInt):TpvSizeInt; overload;
       procedure Exchange(const aIndexA,aIndexB:TpvSizeInt); inline;
       procedure Delete(const aIndex:TpvSizeInt);
     end;

     TpvDynamicStack<T>=record
      public
       Items:array of T;
       Count:TpvSizeInt;
       procedure Initialize;
       procedure Finalize;
       procedure Push(const aItem:T);
       function Pop(out aItem:T):boolean;
     end;

     TpvDynamicQueue<T>=record
      public
       type TQueueItems=array of T;
      public
       Items:TQueueItems;
       Head:TpvSizeInt;
       Tail:TpvSizeInt;
       Count:TpvSizeInt;
       Size:TpvSizeInt;
       procedure Initialize;
       procedure Finalize;
       procedure GrowResize(const aSize:TpvSizeInt);
       procedure Clear;
       function IsEmpty:boolean;
       procedure EnqueueAtFront(const aItem:T);
       procedure Enqueue(const aItem:T);
       function Dequeue(out aItem:T):boolean; overload;
       function Dequeue:boolean; overload;
       function Peek(out aItem:T):boolean;
     end;

     TpvDynamicArrayList<T>=class
      public
       type TItemArray=array of T;
      private
       type TValueEnumerator=record
             private
              fDynamicArray:TpvDynamicArrayList<T>;
              fIndex:TpvSizeInt;
              function GetCurrent:T; inline;
             public
              constructor Create(const aDynamicArray:TpvDynamicArrayList<T>);
              function MoveNext:boolean; inline;
              property Current:T read GetCurrent;
            end;
      private
       fItems:TItemArray;
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
       function Memory:TpvPointer; inline;
       property Count:TpvSizeInt read fCount write SetCount;
       property Allocated:TpvSizeInt read fAllocated;
       property ItemArray:TItemArray read fItems;
       property Items[const pIndex:TpvSizeInt]:T read GetItem write SetItem; default;
     end;

     TpvBaseList=class
      private
       procedure SetCount(const pNewCount:TpvSizeInt);
       function GetItem(const pIndex:TpvSizeInt):TpvPointer;
      protected
       fItemSize:TpvSizeInt;
       fCount:TpvSizeInt;
       fAllocated:TpvSizeInt;
       fMemory:TpvPointer;
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
       property Memory:TpvPointer read fMemory;
       property ItemPointers[const pIndex:TpvSizeInt]:TpvPointer read GetItem; default;
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
       fOwnsObjects:boolean;
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
       function Extract(const pIndex:TpvSizeInt):T;
       procedure Remove(const pItem:T);
       procedure Exchange(const pIndex,pWithIndex:TpvSizeInt);
       function GetEnumerator:TValueEnumerator;
       property Count:TpvSizeInt read fCount write SetCount;
       property Allocated:TpvSizeInt read fAllocated;
       property Items[const pIndex:TpvSizeInt]:T read GetItem write SetItem; default;
       property OwnsObjects:boolean read fOwnsObjects write fOwnsObjects;
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
       function GetHandleDataPointer(const ppvHandle:TpvHandle):TpvPointer; inline;
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
       function HashData(const Data:TpvPointer;const DataLength:TpvUInt32):TpvUInt32;
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

     TpvGenericSkipList<TKey,TValue>=class
      public
       type TPair=class
             private
              fSkipList:TpvGenericSkipList<TKey,TValue>;
              fPrevious:TPair;
              fNext:TPair;
              fKey:TKey;
              fValue:TValue;
              function GetPrevious:TPair;
              function GetNext:TPair;
             public
              constructor Create(const aSkipList:TpvGenericSkipList<TKey,TValue>;const aKey:TKey;const aValue:TValue); reintroduce;
              constructor CreateEmpty(const aSkipList:TpvGenericSkipList<TKey,TValue>); reintroduce;
              destructor Destroy; override;
              property Previous:TPair read GetPrevious;
              property Next:TPair read GetNext;
              property Key:TKey read fKey write fKey;
              property Value:TValue read fValue write fValue;
            end;
            TNode=class
             private
              fPrevious:TNode;
              fNext:TNode;
              fChildren:TNode;
              fPair:TPair;
             public
              constructor Create(const aPrevious:TNode=nil;
                                 const aNext:TNode=nil;
                                 const aChildren:TNode=nil;
                                 const aPair:TPair=nil); reintroduce;
              destructor Destroy; override;
             published
              property Previous:TNode read fPrevious write fPrevious;
              property Next:TNode read fNext write fNext;
              property Children:TNode read fChildren write fChildren;
              property Pair:TPair read fPair write fPair;
            end;
            TNodeArray=array of TNode;
            TRandomGeneratorState=record
             State:TpvUInt64;
             Increment:TpvUInt64;
            end;
      private
       type TValueEnumerator=record
             private
              fSkipList:TpvGenericSkipList<TKey,TValue>;
              fPair:TPair;
              function GetCurrent:TValue; inline;
             public
              constructor Create(const aSkipList:TpvGenericSkipList<TKey,TValue>);
              function MoveNext:boolean; inline;
              property Current:TValue read GetCurrent;
            end;
      private
       fRandomGeneratorState:TRandomGeneratorState;
       fDefaultValue:TValue;
       fHead:TNode;
       fPairs:TPair;
       function GetRandomValue:TpvUInt32;
       function GetFirstPair:TPair;
       function GetLastPair:TPair;
       function FindPreviousNode(const aNode:TNode;const aKey:TKey):TNode;
      public
       constructor Create(const aDefaultValue:TValue); reintroduce;
       destructor Destroy; override;
       function GetNearestPair(const aKey:TKey):TPair;
       function GetNearestKey(const aKey:TKey):TKey;
       function GetNearestValue(const aKey:TKey):TValue;
       function Get(const aKey:TKey;out aValue:TValue):boolean;
       function GetPair(const aKey:TKey):TPair;
       function GetValue(const aKey:TKey):TValue;
       procedure SetValue(const aKey:TKey;const aValue:TValue);
       procedure Delete(const aKey:TKey);
       function GetEnumerator:TValueEnumerator;
       property FirstPair:TPair read GetFirstPair;
       property LastPair:TPair read GetLastPair;
       property Values[const aKey:TKey]:TValue read GetValue write SetValue; default;
     end;

     TpvInt64SkipList<TValue>=class
      public
       type TKey=TpvInt64;
            TPair=class
             private
              fSkipList:TpvInt64SkipList<TValue>;
              fPrevious:TPair;
              fNext:TPair;
              fKey:TKey;
              fValue:TValue;
              function GetPrevious:TPair;
              function GetNext:TPair;
             public
              constructor Create(const aSkipList:TpvInt64SkipList<TValue>;const aKey:TKey;const aValue:TValue); reintroduce;
              constructor CreateEmpty(const aSkipList:TpvInt64SkipList<TValue>); reintroduce;
              destructor Destroy; override;
              property Previous:TPair read GetPrevious;
              property Next:TPair read GetNext;
              property Key:TKey read fKey write fKey;
              property Value:TValue read fValue write fValue;
            end;
            TNode=class
             private
              fPrevious:TNode;
              fNext:TNode;
              fChildren:TNode;
              fPair:TPair;
             public
              constructor Create(const aPrevious:TNode=nil;
                                 const aNext:TNode=nil;
                                 const aChildren:TNode=nil;
                                 const aPair:TPair=nil); reintroduce;
              destructor Destroy; override;
             published
              property Previous:TNode read fPrevious write fPrevious;
              property Next:TNode read fNext write fNext;
              property Children:TNode read fChildren write fChildren;
              property Pair:TPair read fPair write fPair;
            end;
            TNodeArray=array of TNode;
            TRandomGeneratorState=record
             State:TpvUInt64;
             Increment:TpvUInt64;
            end;
      private
       type TValueEnumerator=record
             private
              fSkipList:TpvInt64SkipList<TValue>;
              fPair:TPair;
              function GetCurrent:TValue; inline;
             public
              constructor Create(const aSkipList:TpvInt64SkipList<TValue>);
              function MoveNext:boolean; inline;
              property Current:TValue read GetCurrent;
            end;
      private
       fRandomGeneratorState:TRandomGeneratorState;
       fDefaultValue:TValue;
       fHead:TNode;
       fPairs:TPair;
       function GetRandomValue:TpvUInt32;
       function GetFirstPair:TPair;
       function GetLastPair:TPair;
       function FindPreviousNode(const aNode:TNode;const aKey:TKey):TNode;
      public
       constructor Create(const aDefaultValue:TValue); reintroduce;
       destructor Destroy; override;
       function GetNearestPair(const aKey:TKey):TPair;
       function GetNearestKey(const aKey:TKey):TKey;
       function GetNearestValue(const aKey:TKey):TValue;
       function Get(const aKey:TKey;out aValue:TValue):boolean;
       function GetPair(const aKey:TKey):TPair;
       function GetValue(const aKey:TKey):TValue;
       procedure SetValue(const aKey:TKey;const aValue:TValue);
       procedure Delete(const aKey:TKey);
       function GetEnumerator:TValueEnumerator;
       property FirstPair:TPair read GetFirstPair;
       property LastPair:TPair read GetLastPair;
       property Values[const aKey:TKey]:TValue read GetValue write SetValue; default;
     end;

implementation

uses Generics.Defaults,
     PasVulkan.Utils;

{ TpvDynamicArray<T> }

procedure TpvDynamicArray<T>.Initialize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvDynamicArray<T>.Finalize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvDynamicArray<T>.Clear;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvDynamicArray<T>.Resize(const aCount:TpvSizeInt);
begin
 if Count<>aCount then begin
  Count:=aCount;
  SetLength(Items,Count);
 end;
end;

procedure TpvDynamicArray<T>.Finish;
begin
 SetLength(Items,Count);
end;

procedure TpvDynamicArray<T>.Assign(const aFrom:TpvDynamicArray<T>);
begin
 Items:=copy(aFrom.Items);
 Count:=aFrom.Count;
end;

procedure TpvDynamicArray<T>.Assign(const aItems:array of T);
var Index:TpvSizeInt;
begin
 Count:=length(aItems);
 SetLength(Items,Count);
 for Index:=0 to Count-1 do begin
  Items[Index]:=aItems[Index];
 end;
end;

function TpvDynamicArray<T>.Insert(const aIndex:TpvSizeInt;const aItem:T):TpvSizeInt;
begin
 result:=aIndex;
 if aIndex>=0 then begin
  if aIndex<Count then begin
   inc(Count);
   if length(Items)<Count then begin
    SetLength(Items,Count*2);
   end;
   Move(Items[aIndex],Items[aIndex+1],(Count-(aIndex+1))*SizeOf(T));
   FillChar(Items[aIndex],SizeOf(T),#0);
  end else begin
   Count:=aIndex+1;
   if length(Items)<Count then begin
    SetLength(Items,Count*2);
   end;
  end;
  Items[aIndex]:=aItem;
 end;
end;

function TpvDynamicArray<T>.AddNew:TpvSizeInt;
begin
 result:=Count;
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) shr 1));
 end;
 System.Initialize(Items[Count]);
 inc(Count);
end;

function TpvDynamicArray<T>.Add(const aItem:T):TpvSizeInt;
begin
 result:=Count;
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) shr 1));
 end;
 Items[Count]:=aItem;
 inc(Count);
end;

function TpvDynamicArray<T>.Add(const aItems:array of T):TpvSizeInt;
var Index,FromCount:TpvSizeInt;
begin
 result:=Count;
 FromCount:=length(aItems);
 if FromCount>0 then begin
  if length(Items)<(Count+FromCount) then begin
   SetLength(Items,(Count+FromCount)+((Count+FromCount) shr 1));
  end;
  for Index:=0 to FromCount-1 do begin
   Items[Count]:=aItems[Index];
   inc(Count);
  end;
 end;
end;

function TpvDynamicArray<T>.Add(const aFrom:TpvDynamicArray<T>):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=Count;
 if aFrom.Count>0 then begin
  if length(Items)<(Count+aFrom.Count) then begin
   SetLength(Items,(Count+aFrom.Count)+((Count+aFrom.Count) shr 1));
  end;
  for Index:=0 to aFrom.Count-1 do begin
   Items[Count]:=aFrom.Items[Index];
   inc(Count);
  end;
 end;
end;

function TpvDynamicArray<T>.AddRangeFrom(const aFrom:{$ifdef fpc}{$endif}TpvDynamicArray<T>;const aStartIndex,aCount:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=Count;
 if aCount>0 then begin
  if length(Items)<(Count+aCount) then begin
   SetLength(Items,(Count+aCount)+((Count+aCount) shr 1));
  end;
  for Index:=0 to aCount-1 do begin
   Items[Count]:=aFrom.Items[aStartIndex+Index];
   inc(Count);
  end;
 end;
end;

function TpvDynamicArray<T>.AssignRangeFrom(const aFrom:{$ifdef fpc}{$endif}TpvDynamicArray<T>;const aStartIndex,aCount:TpvSizeInt):TpvSizeInt;
begin
 Clear;
 result:=AddRangeFrom(aFrom,aStartIndex,aCount);
end;

procedure TpvDynamicArray<T>.Exchange(const aIndexA,aIndexB:TpvSizeInt);
var Temp:T;
begin
 Temp:=Items[aIndexA];
 Items[aIndexA]:=Items[aIndexB];
 Items[aIndexB]:=Temp;
end;

procedure TpvDynamicArray<T>.Delete(const aIndex:TpvSizeInt);
begin
 if (Count>0) and (aIndex<Count) then begin
  dec(Count);
  System.Finalize(Items[aIndex]);
  Move(Items[aIndex+1],Items[aIndex],SizeOf(T)*(Count-aIndex));
  FillChar(Items[Count],SizeOf(T),#0);
 end;
end;

{ TpvDynamicStack<T> }

procedure TpvDynamicStack<T>.Initialize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvDynamicStack<T>.Finalize;
begin
 Items:=nil;
 Count:=0;
end;

procedure TpvDynamicStack<T>.Push(const aItem:T);
begin
 if length(Items)<(Count+1) then begin
  SetLength(Items,(Count+1)+((Count+1) shr 1));
 end;
 Items[Count]:=aItem;
 inc(Count);
end;

function TpvDynamicStack<T>.Pop(out aItem:T):boolean;
begin
 result:=Count>0;
 if result then begin
  dec(Count);
  aItem:=Items[Count];
 end;
end;

{ TpvDynamicQueue<T> }

procedure TpvDynamicQueue<T>.Initialize;
begin
 Items:=nil;
 Head:=0;
 Tail:=0;
 Count:=0;
 Size:=0;
end;

procedure TpvDynamicQueue<T>.Finalize;
begin
 Clear;
end;

procedure TpvDynamicQueue<T>.GrowResize(const aSize:TpvSizeInt);
var Index,OtherIndex:TpvSizeInt;
    NewItems:TQueueItems;
begin
 SetLength(NewItems,aSize);
 OtherIndex:=Head;
 for Index:=0 to Count-1 do begin
  NewItems[Index]:=Items[OtherIndex];
  inc(OtherIndex);
  if OtherIndex>=Size then begin
   OtherIndex:=0;
  end;
 end;
 Items:=NewItems;
 Head:=0;
 Tail:=Count;
 Size:=aSize;
end;

procedure TpvDynamicQueue<T>.Clear;
begin
 while Count>0 do begin
  dec(Count);
  System.Finalize(Items[Head]);
  inc(Head);
  if Head>=Size then begin
   Head:=0;
  end;
 end;
 Items:=nil;
 Head:=0;
 Tail:=0;
 Count:=0;
 Size:=0;
end;

function TpvDynamicQueue<T>.IsEmpty:boolean;
begin
 result:=Count=0;
end;

procedure TpvDynamicQueue<T>.EnqueueAtFront(const aItem:T);
var Index:TpvSizeInt;
begin
 if Size<=Count then begin
  GrowResize(Count+1);
 end;
 dec(Head);
 if Head<0 then begin
  inc(Head,Size);
 end;
 Index:=Head;
 Items[Index]:=aItem;
 inc(Count);
end;

procedure TpvDynamicQueue<T>.Enqueue(const aItem:T);
var Index:TpvSizeInt;
begin
 if Size<=Count then begin
  GrowResize(Count+1);
 end;
 Index:=Tail;
 inc(Tail);
 if Tail>=Size then begin
  Tail:=0;
 end;
 Items[Index]:=aItem;
 inc(Count);
end;

function TpvDynamicQueue<T>.Dequeue(out aItem:T):boolean;
begin
 result:=Count>0;
 if result then begin
  dec(Count);
  aItem:=Items[Head];
  System.Finalize(Items[Head]);
  FillChar(Items[Head],SizeOf(T),#0);
  if Count=0 then begin
   Head:=0;
   Tail:=0;
  end else begin
   inc(Head);
   if Head>=Size then begin
    Head:=0;
   end;
  end;
 end;
end;

function TpvDynamicQueue<T>.Dequeue:boolean;
begin
 result:=Count>0;
 if result then begin
  dec(Count);
  System.Finalize(Items[Head]);
  FillChar(Items[Head],SizeOf(T),#0);
  if Count=0 then begin
   Head:=0;
   Tail:=0;
  end else begin
   inc(Head);
   if Head>=Size then begin
    Head:=0;
   end;
  end;
 end;
end;

function TpvDynamicQueue<T>.Peek(out aItem:T):boolean;
begin
 result:=Count>0;
 if result then begin
  aItem:=Items[Head];
 end;
end;

constructor TpvDynamicArrayList<T>.TValueEnumerator.Create(const aDynamicArray:TpvDynamicArrayList<T>);
begin
 fDynamicArray:=aDynamicArray;
 fIndex:=-1;
end;

function TpvDynamicArrayList<T>.TValueEnumerator.MoveNext:boolean;
begin
 inc(fIndex);
 result:=fIndex<fDynamicArray.fCount;
end;

function TpvDynamicArrayList<T>.TValueEnumerator.GetCurrent:T;
begin
 result:=fDynamicArray.fItems[fIndex];
end;

constructor TpvDynamicArrayList<T>.Create;
begin
 fItems:=nil;
 fCount:=0;
 fAllocated:=0;
 inherited Create;
end;

destructor TpvDynamicArrayList<T>.Destroy;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
 inherited Destroy;
end;

procedure TpvDynamicArrayList<T>.Clear;
begin
 SetLength(fItems,0);
 fCount:=0;
 fAllocated:=0;
end;

procedure TpvDynamicArrayList<T>.SetCount(const pNewCount:TpvSizeInt);
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

function TpvDynamicArrayList<T>.GetItem(const pIndex:TpvSizeInt):T;
begin
 result:=fItems[pIndex];
end;

procedure TpvDynamicArrayList<T>.SetItem(const pIndex:TpvSizeInt;const pItem:T);
begin
 fItems[pIndex]:=pItem;
end;

function TpvDynamicArrayList<T>.Add(const pItem:T):TpvSizeInt;
begin
 result:=fCount;
 inc(fCount);
 if fAllocated<fCount then begin
  fAllocated:=fCount+fCount;
  SetLength(fItems,fAllocated);
 end;
 fItems[result]:=pItem;
end;

procedure TpvDynamicArrayList<T>.Insert(const pIndex:TpvSizeInt;const pItem:T);
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

procedure TpvDynamicArrayList<T>.Delete(const pIndex:TpvSizeInt);
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

procedure TpvDynamicArrayList<T>.Exchange(const pIndex,pWithIndex:TpvSizeInt);
var Temporary:T;
begin
 Temporary:=fItems[pIndex];
 fItems[pIndex]:=fItems[pWithIndex];
 fItems[pWithIndex]:=Temporary;
end;

function TpvDynamicArrayList<T>.Memory:TpvPointer;
begin
 result:=@fItems[0];
end;

function TpvDynamicArrayList<T>.GetEnumerator:TpvDynamicArrayList<T>.TValueEnumerator;
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
    Item:TpvPointer;
begin
 if fCount<pNewCount then begin
  NewAllocated:=RoundUpToPowerOfTwoSizeUInt(pNewCount);
  if fAllocated<NewAllocated then begin
   if assigned(fMemory) then begin
    ReallocMem(fMemory,NewAllocated*fItemSize);
   end else begin
    GetMem(fMemory,NewAllocated*fItemSize);
   end;
   FillChar(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(fAllocated)*TpvPtrUInt(fItemSize))))^,(NewAllocated-fAllocated)*fItemSize,#0);
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

function TpvBaseList.GetItem(const pIndex:TpvSizeInt):TpvPointer;
begin
 if (pIndex>=0) and (pIndex<fCount) then begin
  result:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))));
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
    Item:TpvPointer;
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
    SourceItem,Item:TpvPointer;
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
   Difference:=CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Index)*TpvPtrUInt(fItemSize))))^,pItem);
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
   if CompareItem(pItem,TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Index)*TpvPtrUInt(fItemSize))))^)=0 then begin
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
  if CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(fCount-1)*TpvPtrUInt(fItemSize))))^,pItem)<0 then begin
   result:=fCount;
  end else if CompareItem(pItem,TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(0)*TpvPtrUInt(fItemSize))))^)<0 then begin
   result:=0;
  end else begin
   LowerIndexBound:=0;
   UpperIndexBound:=fCount-1;
   while LowerIndexBound<=UpperIndexBound do begin
    Index:=LowerIndexBound+((UpperIndexBound-LowerIndexBound) shr 1);
    Difference:=CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Index)*TpvPtrUInt(fItemSize))))^,pItem);
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
  CopyItem(pItem,TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(result)*TpvPtrUInt(fItemSize))))^);
  fSorted:=false;
 end;
end;

procedure TpvBaseList.Insert(const pIndex:TpvSizeInt;const pItem);
begin
 if pIndex>=0 then begin
  if pIndex<fCount then begin
   SetCount(fCount+1);
   Move(TpvPointer(TpvPtrInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex+1)*TpvPtrUInt(fItemSize))))^,(fCount-(pIndex+1))*fItemSize);
   FillChar(TpvPointer(TpvPtrInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,fItemSize,#0);
  end else begin
   SetCount(pIndex+1);
  end;
  CopyItem(pItem,TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^);
 end;
 fSorted:=false;
end;

procedure TpvBaseList.Delete(const pIndex:TpvSizeInt);
var OldSorted:boolean;
begin
 if (pIndex>=0) and (pIndex<fCount) then begin
  OldSorted:=fSorted;
  FinalizeItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^);
  Move(TpvPointer(TpvPtrUInt(TpvPtruInt(fMemory)+(TpvPtrUInt(pIndex+1)*TpvPtrUInt(fItemSize))))^,TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,(fCount-pIndex)*fItemSize);
  FillChar(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(fCount-1)*TpvPtrUInt(fItemSize))))^,fItemSize,#0);
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
  ExchangeItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pIndex)*TpvPtrUInt(fItemSize))))^,TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(pWithIndex)*TpvPtrUInt(fItemSize))))^);
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
   while TpvPtrUInt(TpvPointer(StackItem))>TpvPtrUInt(TpvPointer(@Stack[0])) do begin
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
            (CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(iA)*TpvPtrUInt(fItemSize))))^,
                         TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(iC)*TpvPtrUInt(fItemSize))))^)>0) do begin
       Exchange(iA,iC);
       dec(iA);
       dec(iC);
      end;
      iA:=iB;
      inc(iB);
     end;
    end else begin
     if (Depth=0) or (TpvPtrUInt(TpvPointer(StackItem))>=TpvPtrUInt(TpvPointer(@Stack[high(Stack)-1]))) then begin
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
         if (Child<(Size-1)) and (CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Child)*TpvPtrUInt(fItemSize))))^,
                                              TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Child+1)*TpvPtrUInt(fItemSize))))^)<0) then begin
          inc(Child);
         end;
         if CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Parent)*TpvPtrUInt(fItemSize))))^,
                        TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left+Child)*TpvPtrUInt(fItemSize))))^)<0 then begin
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
       if CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left)*TpvPtrUInt(fItemSize))))^,
                      TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Middle)*TpvPtrUInt(fItemSize))))^)>0 then begin
        Exchange(Left,Middle);
       end;
       if CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Left)*TpvPtrUInt(fItemSize))))^,
                      TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Right)*TpvPtrUInt(fItemSize))))^)>0 then begin
        Exchange(Left,Right);
       end;
       if CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Middle)*TpvPtrUInt(fItemSize))))^,
                      TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Right)*TpvPtrUInt(fItemSize))))^)>0 then begin
        Exchange(Middle,Right);
       end;
      end;
      Pivot:=Middle;
      i:=Left;
      j:=Right;
      repeat
       while (i<Right) and (CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(i)*TpvPtrUInt(fItemSize))))^,
                                        TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Pivot)*TpvPtrUInt(fItemSize))))^)<0) do begin
        inc(i);
       end;
       while (j>=i) and (CompareItem(TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(j)*TpvPtrUInt(fItemSize))))^,
                                     TpvPointer(TpvPtrUInt(TpvPtrUInt(fMemory)+(TpvPtrUInt(Pivot)*TpvPtrUInt(fItemSize))))^)>0) do begin
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
 fOwnsObjects:=true;
end;

destructor TpvObjectGenericList<T>.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TpvObjectGenericList<T>.Clear;
var Index:TpvSizeInt;
begin
 if fOwnsObjects then begin
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
    Item:TpvPointer;
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
  if fOwnsObjects then begin
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
var OldCount:TpvSizeInt;
begin
 if pIndex>=0 then begin
  OldCount:=fCount;
  if fCount<pIndex then begin
   fCount:=pIndex+1;
  end else begin
   inc(fCount);
  end;
  if fAllocated<fCount then begin
   fAllocated:=fCount shl 1;
   SetLength(fItems,fAllocated);
  end;
  if OldCount<fCount then begin
   FillChar(fItems[OldCount],(fCount-OldCount)*SizeOf(T),#0);
  end;
  if pIndex<OldCount then begin
   System.Move(fItems[pIndex],fItems[pIndex+1],(OldCount-pIndex)*SizeOf(T));
   FillChar(fItems[pIndex],SizeOf(T),#0);
  end;
  fItems[pIndex]:=pItem;
 end;
end;

procedure TpvObjectGenericList<T>.Delete(const pIndex:TpvSizeInt);
var Old:T;
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 Old:=fItems[pIndex];
 dec(fCount);
 FillChar(fItems[pIndex],SizeOf(T),#0);
 if pIndex<>fCount then begin
  System.Move(fItems[pIndex+1],fItems[pIndex],(fCount-pIndex)*SizeOf(T));
  FillChar(fItems[fCount],SizeOf(T),#0);
 end;
 if fCount<(fAllocated shr 1) then begin
  fAllocated:=fAllocated shr 1;
  SetLength(fItems,fAllocated);
 end;
 if fOwnsObjects then begin
  FreeAndNil(Old);
 end;
end;

function TpvObjectGenericList<T>.Extract(const pIndex:TpvSizeInt):T;
var Old:T;
begin
 if (pIndex<0) or (pIndex>=fCount) then begin
  raise ERangeError.Create('Out of index range');
 end;
 Old:=fItems[pIndex];
 dec(fCount);
 FillChar(fItems[pIndex],SizeOf(T),#0);
 if pIndex<>fCount then begin
  System.Move(fItems[pIndex+1],fItems[pIndex],(fCount-pIndex)*SizeOf(T));
  FillChar(fItems[fCount],SizeOf(T),#0);
 end;
 if fCount<(fAllocated shr 1) then begin
  fAllocated:=fAllocated shr 1;
  SetLength(fItems,fAllocated);
 end;
 result:=Old;
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
    Item:TpvPointer;
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
var OldCount:TpvSizeInt;
begin
 if pIndex>=0 then begin
  OldCount:=fCount;
  if fCount<pIndex then begin
   fCount:=pIndex+1;
  end else begin
   inc(fCount);
  end;
  if fAllocated<fCount then begin
   fAllocated:=fCount shl 1;
   SetLength(fItems,fAllocated);
  end;
  if OldCount<fCount then begin
   FillChar(fItems[OldCount],(fCount-OldCount)*SizeOf(T),#0);
  end;
  if pIndex<OldCount then begin
   System.Move(fItems[pIndex],fItems[pIndex+1],(OldCount-pIndex)*SizeOf(T));
   FillChar(fItems[pIndex],SizeOf(T),#0);
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
 dec(fCount);
 FillChar(fItems[pIndex],SizeOf(T),#0);
 if pIndex<>fCount then begin
  System.Move(fItems[pIndex+1],fItems[pIndex],(fCount-pIndex)*SizeOf(T));
  FillChar(fItems[fCount],SizeOf(T),#0);
 end;
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
   FinalizeHandleData(TpvPointer(@fDataArray[Index*TpvSizeUInt(fDataSize)])^);
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
  InitializeHandleData(TpvPointer(@fDataArray[fDenseIndex*TpvSizeUInt(fDataSize)])^);
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
   FinalizeHandleData(TpvPointer(@fDataArray[fDenseIndex*TpvSizeUInt(fDataSize)])^);
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

function TpvCustomHandleMap.GetHandleDataPointer(const ppvHandle:TpvHandle):TpvPointer;
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
   if (fHashMap.fEntityToCellIndex[fIndex]>=0) and (fHashMap.fCellToEntityIndex[fHashMap.fEntityToCellIndex[fIndex]]>=0) then begin
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
   if (fHashMap.fEntityToCellIndex[fIndex]>=0) and (fHashMap.fCellToEntityIndex[fHashMap.fEntityToCellIndex[fIndex]]>=0) then begin
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
   if (fHashMap.fEntityToCellIndex[fIndex]>=0) and (fHashMap.fCellToEntityIndex[fHashMap.fEntityToCellIndex[fIndex]]>=0) then begin
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

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.HashData(const Data:TpvPointer;const DataLength:TpvUInt32):TpvUInt32;
// xxHash32
const PRIME32_1=TpvUInt32(2654435761);
      PRIME32_2=TpvUInt32(2246822519);
      PRIME32_3=TpvUInt32(3266489917);
      PRIME32_4=TpvUInt32(668265263);
      PRIME32_5=TpvUInt32(374761393);
      Seed=TpvUInt32($1337c0d3);
      v1Initialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(PRIME32_1)+TpvUInt64(PRIME32_2)));
      v2Initialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(PRIME32_2)));
      v3Initialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(0)));
      v4Initialization=TpvUInt32(TpvUInt64(TpvInt64(TpvInt64(Seed)-TpvInt64(PRIME32_1))));
      HashInitialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(PRIME32_5)));
var v1,v2,v3,v4:TpvUInt32;
    p,e,Limit:PpvUInt8;
begin
 p:=Data;
 if DataLength>=16 then begin
  v1:=v1Initialization;
  v2:=v2Initialization;
  v3:=v3Initialization;
  v4:=v4Initialization;
  e:=@PpvUInt8Array(Data)^[DataLength-16];
  repeat
{$if defined(fpc) or declared(ROLDWord)}
   v1:=ROLDWord(v1+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v1,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v1:=((v1 shl 13) or (v1 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v2:=ROLDWord(v2+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v2,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v2:=((v2 shl 13) or (v2 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v3:=ROLDWord(v3+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v3,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v3:=((v3 shl 13) or (v3 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v4:=ROLDWord(v4+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v4,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v4:=((v4 shl 13) or (v4 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
  until {%H-}TpvPtrUInt(p)>{%H-}TpvPtrUInt(e);
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(v1,1)+ROLDWord(v2,7)+ROLDWord(v3,12)+ROLDWord(v4,18);
{$else}
  result:=((v1 shl 1) or (v1 shr 31))+
          ((v2 shl 7) or (v2 shr 25))+
          ((v3 shl 12) or (v3 shr 20))+
          ((v4 shl 18) or (v4 shr 14));
{$ifend}
 end else begin
  result:=HashInitialization;
 end;
 inc(result,DataLength);
 e:=@PpvUInt8Array(Data)^[DataLength];
 while ({%H-}TpvPtrUInt(p)+SizeOf(TpvUInt32))<={%H-}TpvPtrUInt(e) do begin
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(result+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_3)),17)*TpvUInt32(PRIME32_4);
{$else}
  inc(result,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_3));
  result:=((result shl 17) or (result shr 15))*TpvUInt32(PRIME32_4);
{$ifend}
  inc(p,SizeOf(TpvUInt32));
 end;
 while {%H-}TpvPtrUInt(p)<{%H-}TpvPtrUInt(e) do begin
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(result+(TpvUInt8(TpvPointer(p)^)*TpvUInt32(PRIME32_5)),11)*TpvUInt32(PRIME32_1);
{$else}
  inc(result,TpvUInt8(TpvPointer(p)^)*TpvUInt32(PRIME32_5));
  result:=((result shl 11) or (result shr 21))*TpvUInt32(PRIME32_1);
{$ifend}
  inc(p,SizeOf(TpvUInt8));
 end;
 result:=(result xor (result shr 15))*TpvUInt32(PRIME32_2);
 result:=(result xor (result shr 13))*TpvUInt32(PRIME32_3);
 result:=result xor (result shr 16);
end;

function TpvHashMap<TpvHashMapKey,TpvHashMapValue>.HashKey(const Key:TpvHashMapKey):TpvUInt32;
var p:TpvUInt64;
begin
 // We're hoping here that the compiler is here so smart, so that the compiler optimizes the
 // unused if-branches away
{$ifndef ExtraStringHashMap}
 if (SizeOf(TpvHashMapKey)=SizeOf(AnsiString)) and
    (TypeInfo(TpvHashMapKey)=TypeInfo(AnsiString)) then begin
  result:=HashData(PpvUInt8(@AnsiString(TpvPointer(@Key)^)[1]),length(AnsiString(TpvPointer(@Key)^))*SizeOf(AnsiChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UTF8String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UTF8String)) then begin
  result:=HashData(PpvUInt8(@UTF8String(TpvPointer(@Key)^)[1]),length(UTF8String(TpvPointer(@Key)^))*SizeOf(AnsiChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(RawByteString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(RawByteString)) then begin
  result:=HashData(PpvUInt8(@RawByteString(TpvPointer(@Key)^)[1]),length(RawByteString(TpvPointer(@Key)^))*SizeOf(AnsiChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(WideString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(WideString)) then begin
  result:=HashData(PpvUInt8(@WideString(TpvPointer(@Key)^)[1]),length(WideString(TpvPointer(@Key)^))*SizeOf(WideChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UnicodeString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UnicodeString)) then begin
  result:=HashData(PpvUInt8(@UnicodeString(TpvPointer(@Key)^)[1]),length(UnicodeString(TpvPointer(@Key)^))*SizeOf(UnicodeChar));
 end else if (SizeOf(TpvHashMapKey)=SizeOf(String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(String)) then begin
  result:=HashData(PpvUInt8(@String(TpvPointer(@Key)^)[1]),length(String(TpvPointer(@Key)^))*SizeOf(Char));
 end else if TypeInfo(TpvHashMapKey)=TypeInfo(TpvUUID) then begin
  result:=(TpvUUID(TpvPointer(@Key)^).DoubleWords[0]*73856093) xor
          (TpvUUID(TpvPointer(@Key)^).DoubleWords[1]*19349669) xor
          (TpvUUID(TpvPointer(@Key)^).DoubleWords[2]*83492791) xor
          (TpvUUID(TpvPointer(@Key)^).DoubleWords[3]*50331653);
 end else{$endif}begin
  case SizeOf(TpvHashMapKey) of
   SizeOf(UInt16):begin
    // 16-bit big => use 16-bit integer-rehashing
    result:=TpvUInt16(TpvPointer(@Key)^);
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
    result:=TpvUInt32(TpvPointer(@Key)^);
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
    p:=TpvUInt64(TpvPointer(@Key)^);
    p:=(not p)+(p shl 18); // p:=((p shl 18)-p-)1;
    p:=p xor (p shr 31);
    p:=p*21; // p:=(p+(p shl 2))+(p shl 4);
    p:=p xor (p shr 11);
    p:=p+(p shl 6);
    result:=TpvUInt32(TpvPtrUInt(p xor (p shr 22)));
   end;
   else begin
    result:=HashData(PpvUInt8(TpvPointer(@Key)),SizeOf(TpvHashMapKey));
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
    pA,pB:PpvUInt8Array;
begin
 // We're hoping also here that the compiler is here so smart, so that the compiler optimizes the
 // unused if-branches away
{$ifndef ExtraStringHashMap}
 if (SizeOf(TpvHashMapKey)=SizeOf(AnsiString)) and
    (TypeInfo(TpvHashMapKey)=TypeInfo(AnsiString)) then begin
  result:=AnsiString(TpvPointer(@KeyA)^)=AnsiString(TpvPointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UTF8String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UTF8String)) then begin
  result:=UTF8String(TpvPointer(@KeyA)^)=UTF8String(TpvPointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(RawByteString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(RawByteString)) then begin
  result:=RawByteString(TpvPointer(@KeyA)^)=RawByteString(TpvPointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(WideString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(WideString)) then begin
  result:=WideString(TpvPointer(@KeyA)^)=WideString(TpvPointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(UnicodeString)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(UnicodeString)) then begin
  result:=UnicodeString(TpvPointer(@KeyA)^)=UnicodeString(TpvPointer(@KeyB)^);
 end else if (SizeOf(TpvHashMapKey)=SizeOf(String)) and
             (TypeInfo(TpvHashMapKey)=TypeInfo(String)) then begin
  result:=String(TpvPointer(@KeyA)^)=String(TpvPointer(@KeyB)^);
 end else{$endif}begin
  case SizeOf(TpvHashMapKey) of
   SizeOf(TpvUInt8):begin
    result:=UInt8(TpvPointer(@KeyA)^)=UInt8(TpvPointer(@KeyB)^);
   end;
   SizeOf(TpvUInt16):begin
    result:=UInt16(TpvPointer(@KeyA)^)=UInt16(TpvPointer(@KeyB)^);
   end;
   SizeOf(TpvUInt32):begin
    result:=TpvUInt32(TpvPointer(@KeyA)^)=TpvUInt32(TpvPointer(@KeyB)^);
   end;
   SizeOf(TpvUInt64):begin
    result:=TpvUInt64(TpvPointer(@KeyA)^)=TpvUInt64(TpvPointer(@KeyB)^);
   end;
{$ifdef fpc}
   SizeOf(TpvHashMapUInt128):begin
    result:=(TpvHashMapUInt128(TpvPointer(@KeyA)^)[0]=TpvHashMapUInt128(TpvPointer(@KeyB)^)[0]) and
           (TpvHashMapUInt128(TpvPointer(@KeyA)^)[1]=TpvHashMapUInt128(TpvPointer(@KeyB)^)[1]);
   end;
{$endif}
   else begin
    Index:=0;
    pA:=@KeyA;
    pB:=@KeyB;
    while (Index+SizeOf(TpvUInt32))<SizeOf(TpvHashMapKey) do begin
     if TpvUInt32(TpvPointer(@pA^[Index])^)<>TpvUInt32(TpvPointer(@pB^[Index])^) then begin
      result:=false;
      exit;
     end;
     inc(Index,SizeOf(TpvUInt32));
    end;
    while (Index+SizeOf(UInt8))<SizeOf(TpvHashMapKey) do begin
     if UInt8(TpvPointer(@pA^[Index])^)<>UInt8(TpvPointer(@pB^[Index])^) then begin
      result:=false;
      exit;
     end;
     inc(Index,SizeOf(UInt8));
    end;
    result:=true;
   end;
  end;
 end;
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
// xxHash32
const PRIME32_1=TpvUInt32(2654435761);
      PRIME32_2=TpvUInt32(2246822519);
      PRIME32_3=TpvUInt32(3266489917);
      PRIME32_4=TpvUInt32(668265263);
      PRIME32_5=TpvUInt32(374761393);
      Seed=TpvUInt32($1337c0d3);
      v1Initialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(PRIME32_1)+TpvUInt64(PRIME32_2)));
      v2Initialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(PRIME32_2)));
      v3Initialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(0)));
      v4Initialization=TpvUInt32(TpvUInt64(TpvInt64(TpvInt64(Seed)-TpvInt64(PRIME32_1))));
      HashInitialization=TpvUInt32(TpvUInt64(TpvUInt64(Seed)+TpvUInt64(PRIME32_5)));
var v1,v2,v3,v4,DataLength:TpvUInt32;
    p,e,Limit:PpvUInt8;
begin
 p:=TpvPointer(@Key[1]);
 DataLength:=length(Key)*SizeOf(Key[1]);
 if DataLength>=16 then begin
  v1:=v1Initialization;
  v2:=v2Initialization;
  v3:=v3Initialization;
  v4:=v4Initialization;
  e:=@PpvUInt8Array(TpvPointer(@Key[1]))^[DataLength-16];
  repeat
{$if defined(fpc) or declared(ROLDWord)}
   v1:=ROLDWord(v1+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v1,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v1:=((v1 shl 13) or (v1 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v2:=ROLDWord(v2+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v2,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v2:=((v2 shl 13) or (v2 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v3:=ROLDWord(v3+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v3,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v3:=((v3 shl 13) or (v3 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
{$if defined(fpc) or declared(ROLDWord)}
   v4:=ROLDWord(v4+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2)),13)*TpvUInt32(PRIME32_1);
{$else}
   inc(v4,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_2));
   v4:=((v4 shl 13) or (v4 shr 19))*TpvUInt32(PRIME32_1);
{$ifend}
   inc(p,SizeOf(TpvUInt32));
  until {%H-}TpvPtrUInt(p)>{%H-}TpvPtrUInt(e);
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(v1,1)+ROLDWord(v2,7)+ROLDWord(v3,12)+ROLDWord(v4,18);
{$else}
  result:=((v1 shl 1) or (v1 shr 31))+
          ((v2 shl 7) or (v2 shr 25))+
          ((v3 shl 12) or (v3 shr 20))+
          ((v4 shl 18) or (v4 shr 14));
{$ifend}
 end else begin
  result:=HashInitialization;
 end;
 inc(result,DataLength);
 e:=@PpvUInt8Array(TpvPointer(@Key[1]))^[DataLength];
 while ({%H-}TpvPtrUInt(p)+SizeOf(TpvUInt32))<={%H-}TpvPtrUInt(e) do begin
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(result+(TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_3)),17)*TpvUInt32(PRIME32_4);
{$else}
  inc(result,TpvUInt32(TpvPointer(p)^)*TpvUInt32(PRIME32_3));
  result:=((result shl 17) or (result shr 15))*TpvUInt32(PRIME32_4);
{$ifend}
  inc(p,SizeOf(TpvUInt32));
 end;
 while {%H-}TpvPtrUInt(p)<{%H-}TpvPtrUInt(e) do begin
{$if defined(fpc) or declared(ROLDWord)}
  result:=ROLDWord(result+(TpvUInt8(TpvPointer(p)^)*TpvUInt32(PRIME32_5)),11)*TpvUInt32(PRIME32_1);
{$else}
  inc(result,TpvUInt8(TpvPointer(p)^)*TpvUInt32(PRIME32_5));
  result:=((result shl 11) or (result shr 21))*TpvUInt32(PRIME32_1);
{$ifend}
  inc(p,SizeOf(TpvUInt8));
 end;
 result:=(result xor (result shr 15))*TpvUInt32(PRIME32_2);
 result:=(result xor (result shr 13))*TpvUInt32(PRIME32_3);
 result:=result xor (result shr 16);
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

constructor TpvGenericSkipList<TKey,TValue>.TValueEnumerator.Create(const aSkipList:TpvGenericSkipList<TKey,TValue>);
begin
 fSkipList:=aSkipList;
 fPair:=fSkipList.fPairs;
end;

function TpvGenericSkipList<TKey,TValue>.TValueEnumerator.GetCurrent:TValue;
begin
 result:=fPair.fValue;
end;

function TpvGenericSkipList<TKey,TValue>.TValueEnumerator.MoveNext:boolean;
begin
 fPair:=fPair.fNext;
 result:=fPair<>fSkipList.fPairs;
end;

constructor TpvGenericSkipList<TKey,TValue>.TPair.Create(const aSkipList:TpvGenericSkipList<TKey,TValue>;const aKey:TKey;const aValue:TValue);
begin
 inherited Create;
 fSkipList:=aSkipList;
 fPrevious:=self;
 fNext:=self;
 fKey:=aKey;
 fValue:=aValue;
end;

constructor TpvGenericSkipList<TKey,TValue>.TPair.CreateEmpty(const aSkipList:TpvGenericSkipList<TKey,TValue>);
begin
 inherited Create;
 fSkipList:=aSkipList;
 fPrevious:=self;
 fNext:=self;
 Initialize(fKey);
 Initialize(fValue);
end;

destructor TpvGenericSkipList<TKey,TValue>.TPair.Destroy;
begin
 fPrevious.fNext:=fNext;
 fNext.fPrevious:=fPrevious;
 fPrevious:=self;
 fNext:=self;
 Finalize(fKey);
 Finalize(fValue);
 inherited Destroy;
end;

function TpvGenericSkipList<TKey,TValue>.TPair.GetPrevious:TPair;
begin
 if fPrevious<>fSkipList.fPairs then begin
  result:=fPrevious;
 end else begin
  result:=nil;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.TPair.GetNext:TPair;
begin
 if fNext<>fSkipList.fPairs then begin
  result:=fNext;
 end else begin
  result:=nil;
 end;
end;

constructor TpvGenericSkipList<TKey,TValue>.TNode.Create(const aPrevious:TNode=nil;
                                                         const aNext:TNode=nil;
                                                         const aChildren:TNode=nil;
                                                         const aPair:TPair=nil);
begin
 inherited Create;
 fPrevious:=aPrevious;
 fNext:=aNext;
 fChildren:=aChildren;
 fPair:=aPair;
end;

destructor TpvGenericSkipList<TKey,TValue>.TNode.Destroy;
begin
 if assigned(fPair) and not assigned(fChildren) then begin
  fPair.Free;
 end;
 inherited Destroy;
end;

constructor TpvGenericSkipList<TKey,TValue>.Create(const aDefaultValue:TValue);
begin
 inherited Create;
 fRandomGeneratorState.State:=0;
 fRandomGeneratorState.Increment:=(TpvPtrUInt(TpvPointer(self)) shl 1) or 1;
 GetRandomValue;
 inc(fRandomGeneratorState.State,(TpvPtrUInt(TpvPointer(self)) shr 19) or (TpvPtrUInt(Pointer(self)) shl 13));
 GetRandomValue;
 fDefaultValue:=aDefaultValue;
 fHead:=TNode.Create;
 fPairs:=TPair.CreateEmpty(self);
end;

destructor TpvGenericSkipList<TKey,TValue>.Destroy;
var Node,TemporaryHead,CurrentNode,NextNode:TNode;
begin
 Node:=fHead;
 while assigned(Node) do begin
  TemporaryHead:=Node;
  Node:=Node.fChildren;
  CurrentNode:=TemporaryHead;
  while assigned(CurrentNode) do begin
   NextNode:=CurrentNode.fNext;
   CurrentNode.Free;
   CurrentNode:=NextNode;
  end;
 end;
 while fPairs.fNext<>fPairs do begin
  fPairs.fNext.Free;
 end;
 fPairs.Free;
 inherited Destroy;
end;

function TpvGenericSkipList<TKey,TValue>.GetRandomValue:TpvUInt32;
var RandomGeneratorState:TpvUInt64;
{$ifndef fpc}
    RandomGeneratorXorShifted,RandomGeneratorRotation:TpvUInt32;
{$endif}
begin
 RandomGeneratorState:=fRandomGeneratorState.State;
 fRandomGeneratorState.State:=(RandomGeneratorState*TpvUInt64(6364136223846793005))+fRandomGeneratorState.Increment;
{$ifdef fpc}
 result:=RORDWord(TpvUInt32(((RandomGeneratorState shr 18) xor RandomGeneratorState) shr 27),RandomGeneratorState shr 59);
{$else}
 RandomGeneratorXorShifted:=((RandomGeneratorState shr 18) xor RandomGeneratorState) shr 27;
 RandomGeneratorRotation:=RandomGeneratorState shr 59;
 result:=(RandomGeneratorXorShifted shr RandomGeneratorRotation) or (RandomGeneratorXorShifted shl (32-RandomGeneratorRotation));
{$endif}
end;

function TpvGenericSkipList<TKey,TValue>.GetFirstPair:TPair;
begin
 result:=fPairs.fNext;
 if result=fPairs then begin
  result:=nil;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.GetLastPair:TPair;
begin
 result:=fPairs.fPrevious;
 if result=fPairs then begin
  result:=nil;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.FindPreviousNode(const aNode:TNode;const aKey:TKey):TNode;
var NextNode:TNode;
    Comparer:IComparer<TKey>;
begin
 Comparer:=TComparer<TKey>.Default;
 result:=aNode;
 while assigned(result) do begin
  NextNode:=result.fNext;
  if assigned(NextNode) and (Comparer.Compare(NextNode.fPair.fKey,aKey)<=0) then begin
   result:=NextNode;
  end else begin
   break;
  end;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.GetNearestPair(const aKey:TKey):TPair;
var CurrentNode,PreviousNode:TNode;
    Pair,BestPair:TPair;
begin
 BestPair:=fPairs.Next;
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) then begin
   BestPair:=Pair;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 if assigned(BestPair) and (BestPair<>fPairs) then begin
  result:=BestPair;
 end else begin
  result:=nil;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.GetNearestKey(const aKey:TKey):TKey;
var Pair:TPair;
begin
 Pair:=GetNearestPair(aKey);
 if assigned(Pair) then begin
  result:=Pair.fKey;
 end else begin
  result:=aKey;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.GetNearestValue(const aKey:TKey):TValue;
var Pair:TPair;
begin
 Pair:=GetNearestPair(aKey);
 if assigned(Pair) then begin
  result:=Pair.fValue;
 end else begin
  result:=fDefaultValue;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.Get(const aKey:TKey;out aValue:TValue):boolean;
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
    Comparer:IComparer<TKey>;
begin
 Comparer:=TComparer<TKey>.Default;
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) and (Comparer.Compare(Pair.fKey,aKey)=0) then begin
   aValue:=Pair.fValue;
   result:=true;
   exit;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 result:=false;
end;

function TpvGenericSkipList<TKey,TValue>.GetPair(const aKey:TKey):TPair;
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
    Comparer:IComparer<TKey>;
begin
 Comparer:=TComparer<TKey>.Default;
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) and (Comparer.Compare(Pair.fKey,aKey)=0) then begin
   result:=Pair;
   exit;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 result:=nil;
end;

function TpvGenericSkipList<TKey,TValue>.GetValue(const aKey:TKey):TValue;
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
    Comparer:IComparer<TKey>;
begin
 Comparer:=TComparer<TKey>.Default;
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) and (Comparer.Compare(Pair.fKey,aKey)=0) then begin
   result:=Pair.fValue;
   exit;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 result:=fDefaultValue;
end;

procedure TpvGenericSkipList<TKey,TValue>.SetValue(const aKey:TKey;const aValue:TValue);
var AllocatedPreviousNodes,CountPreviousNodes:TpvInt32;
    RandomGeneratorValue:TpvUInt32;
    CurrentNode,PreviousNode,NewNode:TNode;
    Pair,OtherPair:TPair;
    PreviousNodes:TNodeArray;
    Comparer:IComparer<TKey>;
begin
 Comparer:=TComparer<TKey>.Default;
 PreviousNodes:=nil;
 try
  CurrentNode:=fHead;
  AllocatedPreviousNodes:=0;
  CountPreviousNodes:=0;
  while assigned(CurrentNode) do begin
   PreviousNode:=FindPreviousNode(CurrentNode,aKey);
   if AllocatedPreviousNodes<=CountPreviousNodes then begin
    AllocatedPreviousNodes:=(CountPreviousNodes+1) shl 1;
    SetLength(PreviousNodes,AllocatedPreviousNodes);
   end;
   PreviousNodes[CountPreviousNodes]:=PreviousNode;
   inc(CountPreviousNodes);
   CurrentNode:=PreviousNode.fChildren;
  end;
  dec(CountPreviousNodes);
  PreviousNode:=PreviousNodes[CountPreviousNodes];
  if assigned(PreviousNode.fPair) and (Comparer.Compare(PreviousNode.fPair.fKey,aKey)=0) then begin
   PreviousNode.fPair.fValue:=aValue;
  end else begin
   if assigned(PreviousNode.fPair) and (PreviousNode.fPair.fNext<>fPairs) then begin
    OtherPair:=PreviousNode.fPair.fNext;
   end else begin
    if Comparer.Compare(fPairs.fPrevious.fKey,aKey)<0 then begin
     OtherPair:=fPairs;
    end else begin
     OtherPair:=fPairs.fNext;
    end;
   end;
   Pair:=TPair.Create(self,aKey,aValue);
   Pair.fPrevious:=OtherPair.fPrevious;
   Pair.fNext:=OtherPair;
   Pair.fPrevious.fNext:=Pair;
   OtherPair.fPrevious:=Pair;
   NewNode:=TNode.Create(PreviousNode,PreviousNode.fNext,nil,Pair);
   if assigned(PreviousNode.fNext) then begin
    PreviousNode.fNext.fPrevious:=NewNode;
   end;
   PreviousNode.fNext:=NewNode;
   RandomGeneratorValue:=0;
   repeat
    if RandomGeneratorValue=0 then begin
     RandomGeneratorValue:=GetRandomValue;
    end;
    if (RandomGeneratorValue and 1)=0 then begin
     break;
    end else begin
     RandomGeneratorValue:=RandomGeneratorValue shr 1;
     if CountPreviousNodes>0 then begin
      dec(CountPreviousNodes);
      PreviousNode:=PreviousNodes[CountPreviousNodes];
     end else begin
      fHead:=TNode.Create(nil,nil,fHead);
      PreviousNode:=fHead;
     end;
     NewNode:=TNode.Create(PreviousNode,PreviousNode.fNext,NewNode,Pair);
     if assigned(PreviousNode.fNext) then begin
      PreviousNode.fNext.fPrevious:=NewNode;
     end;
     PreviousNode.fNext:=NewNode;
    end;
   until false;
  end;
 finally
  PreviousNodes:=nil;
 end;
end;

procedure TpvGenericSkipList<TKey,TValue>.Delete(const aKey:TKey);
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
    Comparer:IComparer<TKey>;
begin
 Comparer:=TComparer<TKey>.Default;
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  if assigned(PreviousNode) then begin
   Pair:=PreviousNode.fPair;
   if assigned(Pair) and (Comparer.Compare(Pair.fKey,aKey)=0) then begin
    CurrentNode:=PreviousNode;
    repeat
     CurrentNode.fPrevious.fNext:=CurrentNode.fNext;
     if assigned(CurrentNode.fNext) then begin
      CurrentNode.fNext.fPrevious:=CurrentNode.fPrevious;
     end;
     PreviousNode:=CurrentNode;
     CurrentNode:=CurrentNode.fChildren;
     PreviousNode.Free;
    until not assigned(CurrentNode);
    break;
   end;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
end;

function TpvGenericSkipList<TKey,TValue>.GetEnumerator:TValueEnumerator;
begin
 result:=TValueEnumerator.Create(self);
end;

constructor TpvInt64SkipList<TValue>.TValueEnumerator.Create(const aSkipList:TpvInt64SkipList<TValue>);
begin
 fSkipList:=aSkipList;
 fPair:=fSkipList.fPairs;
end;

function TpvInt64SkipList<TValue>.TValueEnumerator.GetCurrent:TValue;
begin
 result:=fPair.fValue;
end;

function TpvInt64SkipList<TValue>.TValueEnumerator.MoveNext:boolean;
begin
 fPair:=fPair.fNext;
 result:=fPair<>fSkipList.fPairs;
end;

constructor TpvInt64SkipList<TValue>.TPair.Create(const aSkipList:TpvInt64SkipList<TValue>;const aKey:TKey;const aValue:TValue);
begin
 inherited Create;
 fSkipList:=aSkipList;
 fPrevious:=self;
 fNext:=self;
 fKey:=aKey;
 fValue:=aValue;
end;

constructor TpvInt64SkipList<TValue>.TPair.CreateEmpty(const aSkipList:TpvInt64SkipList<TValue>);
begin
 inherited Create;
 fSkipList:=aSkipList;
 fPrevious:=self;
 fNext:=self;
 Initialize(fKey);
 Initialize(fValue);
end;

destructor TpvInt64SkipList<TValue>.TPair.Destroy;
begin
 fPrevious.fNext:=fNext;
 fNext.fPrevious:=fPrevious;
 fPrevious:=self;
 fNext:=self;
 Finalize(fKey);
 Finalize(fValue);
 inherited Destroy;
end;

function TpvInt64SkipList<TValue>.TPair.GetPrevious:TPair;
begin
 if fPrevious<>fSkipList.fPairs then begin
  result:=fPrevious;
 end else begin
  result:=nil;
 end;
end;

function TpvInt64SkipList<TValue>.TPair.GetNext:TPair;
begin
 if fNext<>fSkipList.fPairs then begin
  result:=fNext;
 end else begin
  result:=nil;
 end;
end;

constructor TpvInt64SkipList<TValue>.TNode.Create(const aPrevious:TNode=nil;
                                                    const aNext:TNode=nil;
                                                    const aChildren:TNode=nil;
                                                    const aPair:TPair=nil);
begin
 inherited Create;
 fPrevious:=aPrevious;
 fNext:=aNext;
 fChildren:=aChildren;
 fPair:=aPair;
end;

destructor TpvInt64SkipList<TValue>.TNode.Destroy;
begin
 if assigned(fPair) and not assigned(fChildren) then begin
  fPair.Free;
 end;
 inherited Destroy;
end;

constructor TpvInt64SkipList<TValue>.Create(const aDefaultValue:TValue);
begin
 inherited Create;
 fRandomGeneratorState.State:=0;
 fRandomGeneratorState.Increment:=(TpvPtrUInt(TpvPointer(self)) shl 1) or 1;
 GetRandomValue;
 inc(fRandomGeneratorState.State,(TpvPtrUInt(TpvPointer(self)) shr 19) or (TpvPtrUInt(Pointer(self)) shl 13));
 GetRandomValue;
 fDefaultValue:=aDefaultValue;
 fHead:=TNode.Create;
 fPairs:=TPair.CreateEmpty(self);
end;

destructor TpvInt64SkipList<TValue>.Destroy;
var Node,TemporaryHead,CurrentNode,NextNode:TNode;
begin
 Node:=fHead;
 while assigned(Node) do begin
  TemporaryHead:=Node;
  Node:=Node.fChildren;
  CurrentNode:=TemporaryHead;
  while assigned(CurrentNode) do begin
   NextNode:=CurrentNode.fNext;
   CurrentNode.Free;
   CurrentNode:=NextNode;
  end;
 end;
 while fPairs.fNext<>fPairs do begin
  fPairs.fNext.Free;
 end;
 fPairs.Free;
 inherited Destroy;
end;

function TpvInt64SkipList<TValue>.GetRandomValue:TpvUInt32;
var RandomGeneratorState:TpvUInt64;
{$ifndef fpc}
    RandomGeneratorXorShifted,RandomGeneratorRotation:TpvUInt32;
{$endif}
begin
 RandomGeneratorState:=fRandomGeneratorState.State;
 fRandomGeneratorState.State:=(RandomGeneratorState*TpvUInt64(6364136223846793005))+fRandomGeneratorState.Increment;
{$ifdef fpc}
 result:=RORDWord(TpvUInt32(((RandomGeneratorState shr 18) xor RandomGeneratorState) shr 27),RandomGeneratorState shr 59);
{$else}
 RandomGeneratorXorShifted:=((RandomGeneratorState shr 18) xor RandomGeneratorState) shr 27;
 RandomGeneratorRotation:=RandomGeneratorState shr 59;
 result:=(RandomGeneratorXorShifted shr RandomGeneratorRotation) or (RandomGeneratorXorShifted shl (32-RandomGeneratorRotation));
{$endif}
end;

function TpvInt64SkipList<TValue>.GetFirstPair:TPair;
begin
 result:=fPairs.fNext;
 if result=fPairs then begin
  result:=nil;
 end;
end;

function TpvInt64SkipList<TValue>.GetLastPair:TPair;
begin
 result:=fPairs.fPrevious;
 if result=fPairs then begin
  result:=nil;
 end;
end;

function TpvInt64SkipList<TValue>.FindPreviousNode(const aNode:TNode;const aKey:TKey):TNode;
var NextNode:TNode;
begin
 result:=aNode;
 while assigned(result) do begin
  NextNode:=result.fNext;
  if assigned(NextNode) and (NextNode.fPair.fKey<=aKey) then begin
   result:=NextNode;
  end else begin
   break;
  end;
 end;
end;

function TpvInt64SkipList<TValue>.GetNearestPair(const aKey:TKey):TPair;
var CurrentNode,PreviousNode:TNode;
    Pair,BestPair:TPair;
begin
 BestPair:=fPairs.Next;
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) then begin
   BestPair:=Pair;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 if assigned(BestPair) and (BestPair<>fPairs) then begin
  result:=BestPair;
 end else begin
  result:=nil;
 end;
end;

function TpvInt64SkipList<TValue>.GetNearestKey(const aKey:TKey):TKey;
var Pair:TPair;
begin
 Pair:=GetNearestPair(aKey);
 if assigned(Pair) then begin
  result:=Pair.fKey;
 end else begin
  result:=aKey;
 end;
end;

function TpvInt64SkipList<TValue>.GetNearestValue(const aKey:TKey):TValue;
var Pair:TPair;
begin
 Pair:=GetNearestPair(aKey);
 if assigned(Pair) then begin
  result:=Pair.fValue;
 end else begin
  result:=fDefaultValue;
 end;
end;

function TpvInt64SkipList<TValue>.Get(const aKey:TKey;out aValue:TValue):boolean;
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
begin
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) and (Pair.fKey=aKey) then begin
   aValue:=Pair.fValue;
   result:=true;
   exit;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 result:=false;
end;

function TpvInt64SkipList<TValue>.GetPair(const aKey:TKey):TPair;
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
begin
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) and (Pair.fKey=aKey) then begin
   result:=Pair;
   exit;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 result:=nil;
end;

function TpvInt64SkipList<TValue>.GetValue(const aKey:TKey):TValue;
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
begin
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  Pair:=PreviousNode.fPair;
  if assigned(Pair) and (Pair.fKey=aKey) then begin
   result:=Pair.fValue;
   exit;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
 result:=fDefaultValue;
end;

procedure TpvInt64SkipList<TValue>.SetValue(const aKey:TKey;const aValue:TValue);
var AllocatedPreviousNodes,CountPreviousNodes:TpvInt32;
    RandomGeneratorValue:TpvUInt32;
    CurrentNode,PreviousNode,NewNode:TNode;
    Pair,OtherPair:TPair;
    PreviousNodes:TNodeArray;
begin
 PreviousNodes:=nil;
 try
  CurrentNode:=fHead;
  AllocatedPreviousNodes:=0;
  CountPreviousNodes:=0;
  while assigned(CurrentNode) do begin
   PreviousNode:=FindPreviousNode(CurrentNode,aKey);
   if AllocatedPreviousNodes<=CountPreviousNodes then begin
    AllocatedPreviousNodes:=(CountPreviousNodes+1) shl 1;
    SetLength(PreviousNodes,AllocatedPreviousNodes);
   end;
   PreviousNodes[CountPreviousNodes]:=PreviousNode;
   inc(CountPreviousNodes);
   CurrentNode:=PreviousNode.fChildren;
  end;
  dec(CountPreviousNodes);
  PreviousNode:=PreviousNodes[CountPreviousNodes];
  if assigned(PreviousNode.fPair) and (PreviousNode.fPair.fKey=aKey) then begin
   PreviousNode.fPair.fValue:=aValue;
  end else begin
   if assigned(PreviousNode.fPair) and (PreviousNode.fPair.fNext<>fPairs) then begin
    OtherPair:=PreviousNode.fPair.fNext;
   end else begin
    if fPairs.fPrevious.fKey<aKey then begin
     OtherPair:=fPairs;
    end else begin
     OtherPair:=fPairs.fNext;
    end;
   end;
   Pair:=TPair.Create(self,aKey,aValue);
   Pair.fPrevious:=OtherPair.fPrevious;
   Pair.fNext:=OtherPair;
   Pair.fPrevious.fNext:=Pair;
   OtherPair.fPrevious:=Pair;
   NewNode:=TNode.Create(PreviousNode,PreviousNode.fNext,nil,Pair);
   if assigned(PreviousNode.fNext) then begin
    PreviousNode.fNext.fPrevious:=NewNode;
   end;
   PreviousNode.fNext:=NewNode;
   RandomGeneratorValue:=0;
   repeat
    if RandomGeneratorValue=0 then begin
     RandomGeneratorValue:=GetRandomValue;
    end;
    if (RandomGeneratorValue and 1)=0 then begin
     break;
    end else begin
     RandomGeneratorValue:=RandomGeneratorValue shr 1;
     if CountPreviousNodes>0 then begin
      dec(CountPreviousNodes);
      PreviousNode:=PreviousNodes[CountPreviousNodes];
     end else begin
      fHead:=TNode.Create(nil,nil,fHead);
      PreviousNode:=fHead;
     end;
     NewNode:=TNode.Create(PreviousNode,PreviousNode.fNext,NewNode,Pair);
     if assigned(PreviousNode.fNext) then begin
      PreviousNode.fNext.fPrevious:=NewNode;
     end;
     PreviousNode.fNext:=NewNode;
    end;
   until false;
  end;
 finally
  PreviousNodes:=nil;
 end;
end;

procedure TpvInt64SkipList<TValue>.Delete(const aKey:TKey);
var CurrentNode,PreviousNode:TNode;
    Pair:TPair;
begin
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  PreviousNode:=FindPreviousNode(CurrentNode,aKey);
  if assigned(PreviousNode) then begin
   Pair:=PreviousNode.fPair;
   if assigned(Pair) and (Pair.fKey=aKey) then begin
    CurrentNode:=PreviousNode;
    repeat
     CurrentNode.fPrevious.fNext:=CurrentNode.fNext;
     if assigned(CurrentNode.fNext) then begin
      CurrentNode.fNext.fPrevious:=CurrentNode.fPrevious;
     end;
     PreviousNode:=CurrentNode;
     CurrentNode:=CurrentNode.fChildren;
     PreviousNode.Free;
    until not assigned(CurrentNode);
    break;
   end;
  end;
  CurrentNode:=PreviousNode.fChildren;
 end;
end;

function TpvInt64SkipList<TValue>.GetEnumerator:TValueEnumerator;
begin
 result:=TValueEnumerator.Create(self);
end;

end.

