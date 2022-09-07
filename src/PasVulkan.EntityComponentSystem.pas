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
     PasVulkan.Collections,
     PasVulkan.DataStructures.LinkedList,
     PasVulkan.Value;

type TpvEntityComponentSystem=class
      public
       type ESystemCircularDependency=class(Exception);

            ESystemSerialization=class(Exception);

            ESystemUnserialization=class(Exception);

            EDuplicateComponentInEntity=class(Exception);

            PComponentID=^TComponentID;
            TComponentID=type TpvSizeUInt;

            TComponentIDDynamicArray=array of TComponentID;

            TWorld=class;

            TSystem=class;

            TSystemList=class(TpvObjectGenericList<TSystem>)
            end;

            PEntityID=^TEntityID;
            TEntityID=type TpvUInt32;

            TEntityIDDynamicArray=array of TEntityID;

            TEntityIDList=class(TpvGenericList<TEntityID>)
            end;

            TEntityIDHelper=record helper for TEntityID
             private // first 24 bits then 8 bits, so that it is still sortable by entity index
              const IndexBits=24; // when all these bits set, then => -1
                    GenerationBits=8;
                    IndexBitsMinusOne=TpvUInt32(IndexBits-1);
                    IndexSignBitMask=TpvUInt32(1 shl IndexBitsMinusOne);
                    IndexMask=TpvUInt32((TpvUInt32(1) shl IndexBits)-1);
                    GenerationMask=TpvUInt32((TpvUInt32(1) shl GenerationBits)-1);
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
              fID:TComponentID;
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
              property ID:TComponentID read fID;
              property Size:TpvSizeInt read fSize;
              property Default:TpvUInt8DynamicArray read fDefault;
            end;

            TRegisteredComponentTypeList=class(TpvObjectGenericList<TRegisteredComponentType>)
            end;

            TRegisteredComponentTypeNameHashMap=class(TpvStringHashMap<TRegisteredComponentType>)
            end;

            TComponentIDBitmap=array of TpvUInt32;

            TComponent=class
             public
              type TIndexMapArray=array of TpvSizeInt;
                   TUsedBitmap=array of TpvUInt32;
                   TPointers=array of TpvPointer;
             private
              fWorld:TWorld;
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
              constructor Create(const aWorld:TWorld;const aRegisteredComponentType:TRegisteredComponentType); reintroduce;
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

            TComponentList=TpvObjectGenericList<TpvEntityComponentSystem.TComponent>;

            TComponentIDList=class(TpvGenericList<TComponentID>)
            end;

            TEventParameter=TpvValue;
            PEventParameter=^TEventParameter;

            TEventParameters=array of TEventParameter;
            PEventParameters=^TEventParameters;

            TEvent=record
             LinkedListHead:TpvLinkedListHead;
             TimeStamp:TpvTime;
             RemainingTime:TpvTime;
             EventID:TEventID;
             EntityID:TEntityID;
             CountParameters:TpvInt32;
             Parameters:TEventParameters;
            end;
            PEvent=^TEvent;

            TEventHandler=procedure(const Event:TEvent) of object;

            TEventHandlers=array of TEventHandler;

            TEventRegistration=class
             private
              fEventID:TEventID;
              fName:TpvUTF8String;
              fActive:longbool;
              fLock:TPasMPMultipleReaderSingleWriterLock;
              fSystems:TSystemList;
              fEventHandlers:TEventHandlers;
              fCountEventHandlers:TpvInt32;
             public
              constructor Create(const aEventID:TEventID;const aName:TpvUTF8String);
              destructor Destroy; override;
              procedure Clear;
              procedure AddSystem(const aSystem:TSystem);
              procedure RemoveSystem(const aSystem:TSystem);
              procedure AddEventHandler(const aEventHandler:TEventHandler);
              procedure RemoveEventHandler(const aEventHandler:TEventHandler);
              property EventID:TEventID read fEventID;
              property Name:TpvUTF8String read fName;
              property Active:longbool read fActive;
              property Lock:TPasMPMultipleReaderSingleWriterLock read fLock;
              property SystemList:TSystemList read fSystems;
              property EventHandlers:TEventHandlers read fEventHandlers;
              property CountEventHandlers:TpvInt32 read fCountEventHandlers;
            end;

            TEventRegistrationList=class(TpvObjectGenericList<TEventRegistration>)
            end;

            TSystemEvents=array of PEvent;

            { TEntity }

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
              fWorld:TWorld;
              fID:TEntityID;
              fUUID:TpvUUID;
              fFlags:TFlags;
              fCountComponents:TpvInt32;
              fComponentsBitmap:TComponentIDBitmap;
              fUnknownData:TObject;
              function GetActive:boolean; inline;
              procedure SetActive(const aActive:boolean); inline;
              procedure AddComponentToEntity(const aComponentID:TComponentID);
              procedure RemoveComponentFromEntity(const aComponentID:TComponentID);
             public
              procedure SynchronizeToPrefab;
              procedure Activate; inline;
              procedure Deactivate; inline;
              procedure Kill; inline;
              procedure AddComponent(const aComponentID:TComponentID); inline;
              procedure RemoveComponent(const aComponentID:TComponentID); inline;
              function HasComponent(const aComponentID:TComponentID):boolean; inline;
              function GetComponent(const aComponentID:TComponentID):TpvEntityComponentSystem.TComponent; inline;
             public
              property World:TWorld read fWorld write fWorld;
              property ID:TEntityID read fID write fID;
              property UUID:TpvUUID read fUUID write fUUID;
              property Flags:TFlags read fFlags write fFlags;
              property Active:boolean read GetActive write SetActive;
              property Components[const aComponentID:TComponentID]:TpvEntityComponentSystem.TComponent read GetComponent;
            end;

            PEntity=^TEntity;

            TEntities=array of TEntity;

            { TSystemChoreography }

            TSystemChoreography=class
             public
              type TSystemChoreographyStepSystems=array of TSystem;
                   TSystemChoreographyStepJobs=array of PPasMPJob;
                   PSystemChoreographyStep=^TSystemChoreographyStep;
                   TSystemChoreographyStep=record
                    Systems:TSystemChoreographyStepSystems;
                    Jobs:TSystemChoreographyStepJobs;
                    Count:TpvInt32;
                   end;
                   TSystemChoreographySteps=array of TSystemChoreographyStep;
                   PSystemChoreographyStepProcessEventsJobData=^TSystemChoreographyStepProcessEventsJobData;
                   TSystemChoreographyStepProcessEventsJobData=record
                    ChoreographyStep:PSystemChoreographyStep;
                   end;
                   PSystemChoreographyStepUpdateEntitiesJobData=^TSystemChoreographyStepUpdateEntitiesJobData;
                   TSystemChoreographyStepUpdateEntitiesJobData=record
                    ChoreographyStep:PSystemChoreographyStep;
                   end;
             private
              fWorld:TWorld;
              fPasMPInstance:TPasMP;
              fChoreographySteps:TSystemChoreographySteps;
              fChoreographyStepJobs:TSystemChoreographyStepJobs;
              fCountChoreographySteps:TpvInt32;
              fSortedSystemList:TSystemList;
              function CreateProcessEventsJob(const aSystem:TSystem;const aFirstEventIndex,aLastEventIndex:TPasMPSizeInt;const aParentJob:PPasMPJob):PPasMPJob;
              procedure ProcessEventsJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
              procedure ChoreographyStepProcessEventsJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
              procedure ChoreographyProcessEventsJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
              function CreateUpdateEntitiesJob(const aSystem:TSystem;const aFirstEntityIndex,aLastEntityIndex:TPasMPSizeInt;const aParentJob:PPasMPJob):PPasMPJob;
              procedure UpdateEntitiesJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
              procedure ChoreographyStepUpdateEntitiesJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
              procedure ChoreographyUpdateEntitiesJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
             public
              constructor Create(const aWorld:TWorld);
              destructor Destroy; override;
              procedure Build;
              procedure ProcessEvents;
              procedure InitializeUpdate;
              procedure Update;
              procedure FinalizeUpdate;
            end;

            { TSystem }

            TSystem=class
             public
              type TFlag=
                    (
                     ParallelProcessing,
                     Secluded,
                     OwnUpdate
                    );
                   TFlags=set of TFlag;
             private
              fWorld:TWorld;
              fFlags:TFlags;
              fEntities:TEntityIDList;
              fRequiredComponents:TComponentIDList;
              fExcludedComponents:TComponentIDList;
              fRequiresSystems:TSystemList;
              fConflictsWithSystems:TSystemList;
              fNeedToSort:boolean;
              fEventsCanBeParallelProcessed:boolean;
              fEventGranularity:TpvInt32;
              fEntityGranularity:TpvInt32;
              fCountEntities:TpvInt32;
              fEvents:TSystemEvents;
              fCountEvents:TpvInt32;
              fDeltaTime:TpvTime;
             protected
              function HaveDependencyOnSystem(const aOtherSystem:TSystem):boolean;
              function HaveDependencyOnSystemOrViceVersa(const aOtherSystem:TSystem):boolean;
              function HaveCircularDependencyWithSystem(const aOtherSystem:TSystem):boolean;
              function HaveConflictWithSystem(const aOtherSystem:TSystem):boolean;
              function HaveConflictWithSystemOrViceVersa(const aOtherSystem:TSystem):boolean;
             public
              constructor Create(const aWorld:TWorld); virtual;
              destructor Destroy; override;
              procedure Added; virtual;
              procedure Removed; virtual;
              procedure SubscribeToEvent(const aEventID:TEventID);
              procedure UnsubscribeFromEvent(const aEventID:TEventID);
              procedure RequiresSystem(const aSystem:TSystem);
              procedure ConflictsWithSystem(const aSystem:TSystem);
              procedure AddRequiredComponent(const aComponentID:TComponentID);
              procedure AddExcludedComponent(const aComponentID:TComponentID);
              function FitsEntityToSystem(const aEntityID:TEntityID):boolean; virtual;
              function AddEntityToSystem(const aEntityID:TEntityID):boolean; virtual;
              function RemoveEntityFromSystem(const aEntityID:TEntityID):boolean; virtual;
              procedure SortEntities; virtual;
              procedure Finish; virtual;
              procedure ProcessEvent(const aEvent:TEvent); virtual;
              procedure ProcessEvents(const aFirstEventIndex,aLastEventIndex:TpvSizeInt); virtual;
              procedure InitializeUpdate; virtual;
              procedure Update; virtual;
              procedure UpdateEntities(const aFirstEntityIndex,aLastEntityIndex:TpvSizeInt); virtual;
              procedure FinalizeUpdate; virtual;
              property World:TWorld read fWorld;
              property Flags:TFlags read fFlags write fFlags;
              property Entities:TEntityIDList read fEntities;
              property CountEntities:TpvInt32 read fCountEntities;
              property EventsCanBeParallelProcessed:boolean read fEventsCanBeParallelProcessed write fEventsCanBeParallelProcessed;
              property EventGranularity:TpvInt32 read fEventGranularity write fEventGranularity;
              property EntityGranularity:TpvInt32 read fEntityGranularity write fEntityGranularity;
              property Events:TSystemEvents read fEvents;
              property CountEvents:TpvInt32 read fCountEvents;
              property DeltaTime:TpvTime read fDeltaTime;
            end;

            TDelayedManagementEventType=(
             None,
             CreateEntity,
             ActivateEntity,
             DeactivateEntity,
             KillEntity,
             AddComponentToEntity,
             RemoveComponentFromEntity,
             AddSystem,
             RemoveSystem,
             SortSystem
            );
            PDelayedManagementEventType=^TDelayedManagementEventType;

            TDelayedManagementEventData=array of TpvUInt8;

            TDelayedManagementEvent=record
             EventType:TDelayedManagementEventType;
             EntityID:TEntityID;
             ComponentID:TComponentID;
             System:TSystem;
             UUID:TpvUUID;
             Data:TDelayedManagementEventData;
             DataSize:TpvInt32;
             DataString:TpvRawByteString;
            end;
            PDelayedManagementEvent=^TDelayedManagementEvent;

            TDelayedManagementEvents=array of TDelayedManagementEvent;

            TDelayedManagementEventQueue=TpvDynamicQueue<TDelayedManagementEvent>;

            TWorld=class
             public
              type TEntityIndexFreeList=TpvGenericList<TpvSizeInt>;
                   TEntityGenerationList=array of TpvUInt8;
                   TUsedBitmap=array of TpvUInt32;
                   TOnEvent=procedure(const aWorld:TWorld;const aEvent:TEvent) of object;
                   TEventRegistrationStringIntegerPairHashMap=class(TpvStringHashMap<TpvSizeInt>);
                   TUUIDEntityIDPairHashMap=class(TpvHashMap<TpvUUID,TpvEntityComponentSystem.TEntityID>);
                   TSystemBooleanPairHashMap=class(TpvHashMap<TpvEntityComponentSystem.TSystem,longbool>);
             private
              fUUID:TpvUUID;
              fActive:longbool;
              fPasMPInstance:TPasMP;
              fLock:TPasMPMultipleReaderSingleWriterLock;
              fComponents:TComponentList;
              fEntities:TEntities;
              fSystems:TSystemList;
              fSystemUsedMap:TSystemBooleanPairHashMap;
              fSystemChoreography:TSystemChoreography;
              fSystemChoreographyNeedToRebuild:TPasMPInt32;
              fEntityLock:TPasMPMultipleReaderSingleWriterLock;
              fEntityIndexFreeList:TEntityIndexFreeList;
              fEntityGenerationList:TEntityGenerationList;
              fEntityUsedBitmap:TUsedBitmap;
              fEntityIndexCounter:TpvSizeInt;
              fMaxEntityIndex:TpvSizeInt;
              fEntityUUIDHashMap:TUUIDEntityIDPairHashMap;
              fReservedEntityHashMapLock:TPasMPMultipleReaderSingleWriterLock;
              fReservedEntityUUIDHashMap:TUUIDEntityIDPairHashMap;
              fEventInProcessing:longbool;
              fEventRegistrationLock:TPasMPMultipleReaderSingleWriterLock;
              fEventRegistrationList:TEventRegistrationList;
              fFreeEventRegistrationList:TEventRegistrationList;
              fEventRegistrationStringIntegerPairHashMap:TEventRegistrationStringIntegerPairHashMap;
              fOnEvent:TOnEvent;
              fEventListLock:TPasMPMultipleReaderSingleWriterLock;
              fEventList:TList;
              fDelayedEventQueueLock:TPasMPMultipleReaderSingleWriterLock;
              fDelayedEventQueue:TpvLinkedListHead;
              fEventQueueLock:TPasMPMultipleReaderSingleWriterLock;
              fEventQueue:TpvLinkedListHead;
              fDelayedFreeEventQueue:TpvLinkedListHead;
              fFreeEventQueueLock:TPasMPMultipleReaderSingleWriterLock;
              fFreeEventQueue:TpvLinkedListHead;
              fCurrentTime:TpvTime;
              fDelayedManagementEventLock:TPasMPMultipleReaderSingleWriterLock;
              fDelayedManagementEvents:TDelayedManagementEvents;
              fCountDelayedManagementEvents:TpvSizeInt;
              procedure AddDelayedManagementEvent(const aDelayedManagementEvent:TDelayedManagementEvent);
              function GetEntityByID(const aEntityID:TEntityID):PEntity;
              function GetEntityByUUID(const aEntityUUID:TpvUUID):PEntity;
              function DoCreateEntity(const aEntityID:TEntityID;const aEntityUUID:TpvUUID):boolean;
              function DoDestroyEntity(const aEntityID:TEntityID):boolean;
              procedure ProcessEvent(const aEvent:PEvent);
              procedure ProcessEvents;
              procedure ProcessDelayedEvents(const aDeltaTime:TTime);
              function CreateEntity(const aEntityID:TEntityID;const aEntityUUID:TpvUUID):TEntityID; overload;
             public
              constructor Create(const aPasMPInstance:TPasMP=nil); reintroduce;
              destructor Destroy; override;
              procedure Kill;
              function CreateEvent(const aName:TpvUTF8String):TEventID;
              procedure DestroyEvent(const aEventID:TEventID);
              function FindEvent(const aName:TpvUTF8String):TEventID;
              procedure SubscribeToEvent(const aEventID:TEventID;const aEventHandler:TEventHandler);
              procedure UnsubscribeFromEvent(const aEventID:TEventID;const aEventHandler:TEventHandler);
              function CreateEntity(const aEntityUUID:TpvUUID):TEntityID; overload;
              function CreateEntity:TEntityID; overload;
              function HasEntity(const aEntityID:TEntityID):boolean; {$ifdef caninline}inline;{$endif}
              function IsEntityActive(const aEntityID:TEntityID):boolean; {$ifdef caninline}inline;{$endif}
              procedure ActivateEntity(const aEntityID:TEntityID); {$ifdef caninline}inline;{$endif}
              procedure DeactivateEntity(const aEntityID:TEntityID); {$ifdef caninline}inline;{$endif}
              procedure KillEntity(const aEntityID:TEntityID); {$ifdef caninline}inline;{$endif}
              procedure AddComponentToEntity(const aEntityID:TEntityID;const aComponentID:TComponentID);
              procedure RemoveComponentFromEntity(const aEntityID:TEntityID;const aComponentID:TComponentID);
              function HasEntityComponent(const aEntityID:TEntityID;const aComponentID:TComponentID):boolean;
              procedure AddSystem(const aSystem:TSystem);
              procedure RemoveSystem(const aSystem:TSystem);
              procedure SortSystem(const aSystem:TSystem);
              procedure Defragment;
              procedure Refresh;
              procedure QueueEvent(const aEventToQueue:TEvent;const aDeltaTime:TpvTime); overload;
              procedure QueueEvent(const aEventToQueue:TEvent); overload;
              procedure Update(const aDeltaTime:TpvTime);
              procedure Clear;
              procedure ClearEntities;
              procedure Activate;
              procedure Deactivate;
              procedure MementoSerialize(const aStream:TStream);
              procedure MementoUnserialize(const aStream:TStream);
             public
              property UUID:TpvUUID read fUUID write fUUID;
              property Active:longbool read fActive write fActive;
              property Components:TComponentList read fComponents;
              property CurrentTime:TpvTime read fCurrentTime;
              property OnEvent:TOnEvent read fOnEvent write fOnEvent;
            end;

     end;

var RegisteredComponentTypeList:TpvEntityComponentSystem.TRegisteredComponentTypeList=nil;
    RegisteredComponentTypeNameHashMap:TpvEntityComponentSystem.TRegisteredComponentTypeNameHashMap=nil;

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
 result:=(self shr GenerationBits) and IndexMask;
 result:=result or (-(ord(result=IndexMask) and 1));
end;

procedure TpvEntityComponentSystem.TEntityIDHelper.SetIndex(const aIndex:TpvInt32);
begin
 self:=(self and GenerationMask) or ((TpvUInt32(aIndex) and IndexMask) shl GenerationBits);
end;

function TpvEntityComponentSystem.TEntityIDHelper.GetGeneration:TpvUInt8;
begin
 result:=self and GenerationMask;
end;

procedure TpvEntityComponentSystem.TEntityIDHelper.SetGeneration(const aGeneration:TpvUInt8);
begin
 self:=(self and not GenerationMask) or (aGeneration and GenerationMask);
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

{ TpvEntityComponentSystem.TRegisteredComponent }

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
 RegisteredComponentTypeNameHashMap.Add(aName,self);
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
  SetLength(fFields,fCountFields+((fCountFields+1) shr 1));
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
    if UnsignedInteger<TpvUInt64($0010000000000000) then begin
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
      FloatValue:=PpvHalfFloat(Data)^.ToFloat;
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
     if Code<>0 then begin
     end;
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

{ TpvEntityComponentSystem.TComponent }

constructor TpvEntityComponentSystem.TComponent.Create(const aWorld:TWorld;const aRegisteredComponentType:TRegisteredComponentType);
begin
 inherited Create;

 fWorld:=aWorld;

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
  SetLength(fPointers,aCount+((aCount+1) shr 1));
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
   SetLength(fUsedBitmap,Count+((Count+1) shr 1));
   for Index:=OldCount to length(fUsedBitmap)-1 do begin
    fUsedBitmap[Index]:=0;
   end;
  end;

  OldCount:=length(fPointers);
  Count:=fMaxEntityIndex+1;
  if OldCount<Count then begin
   SetLength(fPointers,Count+((Count+1) shr 1));
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
  result:=true;
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

procedure TpvEntityComponentSystem.TEntity.AddComponentToEntity(const aComponentID:TComponentID);
var Index,BitmapIndex,BitIndex,OldCount:TpvSizeInt;
begin
 BitmapIndex:=aComponentID shr 5;
 BitIndex:=aComponentID and 31;
 fCountComponents:=Max(fCountComponents,aComponentID+1);
 if length(fComponentsBitmap)<=BitmapIndex then begin
  OldCount:=length(fComponentsBitmap);
  SetLength(fComponentsBitmap,(BitmapIndex+1)+((BitmapIndex+2) shr 1));
  FillChar(fComponentsBitmap[OldCount],(length(fComponentsBitmap)-OldCount)*SizeOf(UInt32),#0);
 end;
 fComponentsBitmap[BitIndex]:=fComponentsBitmap[BitIndex] or (TpvUInt32(1) shl BitIndex);
end;

procedure TpvEntityComponentSystem.TEntity.RemoveComponentFromEntity(const aComponentID:TComponentID);
var Index,BitmapIndex,BitIndex,OldCount:TpvSizeInt;
begin
 BitmapIndex:=aComponentID shr 5;
 BitIndex:=aComponentID and 31;
 fCountComponents:=Max(fCountComponents,aComponentID+1);
 if length(fComponentsBitmap)<=BitmapIndex then begin
  OldCount:=length(fComponentsBitmap);
  SetLength(fComponentsBitmap,(BitmapIndex+1)+((BitmapIndex+2) shr 1));
  FillChar(fComponentsBitmap[OldCount],(length(fComponentsBitmap)-OldCount)*SizeOf(UInt32),#0);
 end;
 fComponentsBitmap[BitIndex]:=fComponentsBitmap[BitIndex] and not (TpvUInt32(1) shl BitIndex);
end;

procedure TpvEntityComponentSystem.TEntity.SynchronizeToPrefab;
begin
end;

procedure TpvEntityComponentSystem.TEntity.Activate;
begin
 if assigned(fWorld) then begin
  fWorld.ActivateEntity(fID);
 end;
end;

procedure TpvEntityComponentSystem.TEntity.Deactivate;
begin
 if assigned(fWorld) then begin
  fWorld.DeactivateEntity(fID);
 end;
end;

procedure TpvEntityComponentSystem.TEntity.Kill;
begin
 if assigned(fWorld) then begin
  fWorld.KillEntity(fID);
 end;
end;

procedure TpvEntityComponentSystem.TEntity.AddComponent(const aComponentID:TComponentID);
begin
 if assigned(fWorld) then begin
  fWorld.AddComponentToEntity(fID,aComponentID);
 end;
end;

procedure TpvEntityComponentSystem.TEntity.RemoveComponent(const aComponentID:TComponentID);
begin
 if assigned(fWorld) then begin
  fWorld.RemoveComponentFromEntity(fID,aComponentID);
 end;
end;

function TpvEntityComponentSystem.TEntity.HasComponent(const aComponentID:TComponentID):boolean;
begin
 result:=assigned(fWorld) and World.HasEntityComponent(fID,aComponentID);
end;

function TpvEntityComponentSystem.TEntity.GetComponent(const aComponentID:TComponentID):TpvEntityComponentSystem.TComponent;
begin
 if assigned(fWorld) and World.HasEntityComponent(fID,aComponentID) then begin
  result:=fWorld.fComponents[aComponentID];
 end else begin
  result:=nil;
 end;
end;

{ TpvEntityComponentSystem.TEventRegistration }

constructor TpvEntityComponentSystem.TEventRegistration.Create(const aEventID:TpvEntityComponentSystem.TEventID;const aName:TpvUTF8String);
begin
 inherited Create;
 fEventID:=aEventID;
 fName:=aName;
 fActive:=false;
 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;
 fSystems:=TSystemList.Create;
 fSystems.OwnsObjects:=false;
 fEventHandlers:=nil;
 fCountEventHandlers:=0;
end;

destructor TpvEntityComponentSystem.TEventRegistration.Destroy;
begin
 fName:='';
 fEventHandlers:=nil;
 FreeAndNil(fSystems);
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvEntityComponentSystem.TEventRegistration.Clear;
begin
 fLock.AcquireWrite;
 try
  fName:='';
  fActive:=false;
  fSystems.Clear;
  fEventHandlers:=nil;
  fCountEventHandlers:=0;
 finally
  fLock.ReleaseWrite;
 end;
end;

procedure TpvEntityComponentSystem.TEventRegistration.AddSystem(const aSystem:TSystem);
begin
 fLock.AcquireRead;
 try
  if fSystems.IndexOf(aSystem)<0 then begin
   fLock.ReadToWrite;
   try
    fSystems.Add(aSystem);
   finally
    fLock.WriteToRead;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

procedure TpvEntityComponentSystem.TEventRegistration.RemoveSystem(const aSystem:TSystem);
var Index:TpvSizeInt;
begin
 fLock.AcquireRead;
 try
  Index:=fSystems.IndexOf(aSystem);
  if Index>=0 then begin
   fLock.ReadToWrite;
   try
    fSystems.Delete(Index);
   finally
    fLock.WriteToRead;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

procedure TpvEntityComponentSystem.TEventRegistration.AddEventHandler(const aEventHandler:TEventHandler);
var Index:TpvSizeInt;
    Found:boolean;
begin
 Found:=false;
 fLock.AcquireRead;
 try
  for Index:=0 to fCountEventHandlers-1 do begin
   if (TMethod(fEventHandlers[Index]).Code=TMethod(aEventHandler).Code) and
      (TMethod(fEventHandlers[Index]).Data=TMethod(aEventHandler).Data) then begin
    Found:=true;
    break;
   end;
  end;
  if not Found then begin
   fLock.ReadToWrite;
   try
    Index:=fCountEventHandlers;
    inc(fCountEventHandlers);
    if length(fEventHandlers)<fCountEventHandlers then begin
     SetLength(fEventHandlers,fCountEventHandlers+((fCountEventHandlers+1) shr 1));
    end;
    fEventHandlers[Index]:=aEventHandler;
   finally
    fLock.WriteToRead;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

procedure TpvEntityComponentSystem.TEventRegistration.RemoveEventHandler(const aEventHandler:TEventHandler);
var Index:TpvSizeInt;
begin
 fLock.AcquireRead;
 try
  for Index:=0 to fCountEventHandlers-1 do begin
   if (TMethod(fEventHandlers[Index]).Code=TMethod(aEventHandler).Code) and
      (TMethod(fEventHandlers[Index]).Data=TMethod(aEventHandler).Data) then begin
    fLock.ReadToWrite;
    try
     dec(fCountEventHandlers);
     if fCountEventHandlers>0 then begin
      Move(fEventHandlers[Index+1],fEventHandlers[Index],fCountEventHandlers*SizeOf(TEventHandler)); // for to be keep the ordering
//    fEventHandlers[Index]:=fEventHandlers[fCountEventHandlers]; // for to be faster, but with changing the ordering of the last item to the deleted item position
     end;
    finally
     fLock.WriteToRead;
    end;
    break;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

{ TpvEntityComponentSystem.TSystemChoreography }

constructor TpvEntityComponentSystem.TSystemChoreography.Create(const aWorld:TWorld);
begin
 inherited Create;
 fWorld:=aWorld;
 fPasMPInstance:=fWorld.fPasMPInstance;
 fChoreographySteps:=nil;
 fChoreographyStepJobs:=nil;
 fCountChoreographySteps:=0;
 fSortedSystemList:=TSystemList.Create;
 fSortedSystemList.OwnsObjects:=false;
end;

destructor TpvEntityComponentSystem.TSystemChoreography.Destroy;
begin
 fChoreographySteps:=nil;
 fChoreographyStepJobs:=nil;
 FreeAndNil(fSortedSystemList);
 inherited Destroy;
end;

procedure TpvEntityComponentSystem.TSystemChoreography.Build;
var Systems:TSystemList;
    Index,OtherIndex,SystemIndex:TpvInt32;
    Done,Stop:boolean;
    System,OtherSystem:TSystem;
    ChoreographyStep:PSystemChoreographyStep;
begin
 Systems:=fSortedSystemList;

 Systems.Clear;

 // Fill in
 for Index:=0 to fWorld.fSystems.Count-1 do begin
  Systems.Add(fWorld.fSystems.Items[Index]);
 end;

 // Resolve dependencies with "stable" topological sorting a la naive bubble sort with a bad
 // execution time (but that fact does not matter at so few systems), and not with Kahn's or
 // Tarjan's algorithms, because the result must be in a stable sort order
 repeat
  Done:=true;
  for Index:=0 to Systems.Count-1 do begin
   System:=Systems.Items[Index];
   for OtherIndex:=0 to Index-1 do begin
    OtherSystem:=Systems.Items[OtherIndex];
    if OtherSystem.HaveDependencyOnSystem(System) then begin
     if OtherSystem.HaveCircularDependencyWithSystem(System) then begin
      raise ESystemCircularDependency.Create(System.ClassName+' have circular dependency with '+OtherSystem.ClassName);
     end else begin
      Systems.Exchange(Index,OtherIndex);
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

 // Construct dependency conflict-free choreography
 fCountChoreographySteps:=0;
 Index:=0;
 while Index<Systems.Count do begin
  System:=Systems.Items[Index];
  inc(Index);
  inc(fCountChoreographySteps);
  if fCountChoreographySteps>length(fChoreographySteps) then begin
   SetLength(fChoreographySteps,fCountChoreographySteps*2);
  end;
  ChoreographyStep:=@fChoreographySteps[fCountChoreographySteps-1];
  ChoreographyStep^.Count:=1;
  SetLength(ChoreographyStep^.Systems,ChoreographyStep^.Count);
  SetLength(ChoreographyStep^.Jobs,ChoreographyStep^.Count);
  ChoreographyStep^.Systems[0]:=System;
  while Index<Systems.Count do begin
   OtherSystem:=Systems.Items[Index];
   Stop:=TpvEntityComponentSystem.TSystem.TFlag.Secluded in OtherSystem.fFlags;
   if not Stop then begin
    for SystemIndex:=0 to ChoreographyStep^.Count-1 do begin
     System:=ChoreographyStep^.Systems[SystemIndex];
     if System.HaveDependencyOnSystemOrViceVersa(OtherSystem) or
        System.HaveConflictWithSystemOrViceVersa(OtherSystem) then begin
      Stop:=true;
      break;
     end;
    end;
   end;
   if Stop then begin
    break;
   end else begin
    inc(Index);
    inc(ChoreographyStep^.Count);
    if ChoreographyStep^.Count>length(ChoreographyStep^.Systems) then begin
     SetLength(ChoreographyStep^.Systems,ChoreographyStep^.Count*2);
    end;
    ChoreographyStep^.Systems[ChoreographyStep^.Count-1]:=OtherSystem;
   end;
  end;
 end;
 SetLength(fChoreographyStepJobs,fCountChoreographySteps);

end;

type PSystemChoreographyProcessEventsJobData=^TSystemChoreographyProcessEventsJobData;
     TSystemChoreographyProcessEventsJobData=record
      System:TpvEntityComponentSystem.TSystem;
      FirstEventIndex:TPasMPSizeInt;
      LastEventIndex:TPasMPSizeInt;
     end;

function TpvEntityComponentSystem.TSystemChoreography.CreateProcessEventsJob(const aSystem:TSystem;const aFirstEventIndex,aLastEventIndex:TPasMPSizeInt;const aParentJob:PPasMPJob):PPasMPJob;
var Data:PSystemChoreographyProcessEventsJobData;
begin
 result:=fPasMPInstance.Acquire(ProcessEventsJobFunction,nil,nil);
 Data:=PSystemChoreographyProcessEventsJobData(pointer(@result^.Data));
 Data^.System:=aSystem;
 Data^.FirstEventIndex:=aFirstEventIndex;
 Data^.LastEventIndex:=aFirstEventIndex;
end;

procedure TpvEntityComponentSystem.TSystemChoreography.ProcessEventsJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
var Data:PSystemChoreographyProcessEventsJobData;
    MidEventIndex,Count:TpvSizeInt;
begin
 Data:=@aJob^.Data;
 if Data^.FirstEventIndex<=Data^.LastEventIndex then begin
  Count:=Data^.LastEventIndex-Data^.FirstEventIndex;
  if (fPasMPInstance.CountJobWorkerThreads<2) or
     (not (TpvEntityComponentSystem.TSystem.TFlag.ParallelProcessing in Data.System.fFlags)) or
     ((Count<=Data^.System.fEventGranularity) or (Count<4)) then begin
   Data^.System.ProcessEvents(Data^.FirstEventIndex,Data^.LastEventIndex);
  end else begin
   MidEventIndex:=Data^.FirstEventIndex+((Data^.LastEventIndex-Data^.FirstEventIndex) shr 1);
   fPasMPInstance.Invoke([CreateProcessEventsJob(Data^.System,Data^.FirstEventIndex,MidEventIndex-1,aJob),
                  CreateProcessEventsJob(Data^.System,MidEventIndex,Data^.LastEventIndex,aJob)]);
  end;
 end;
end;

procedure TpvEntityComponentSystem.TSystemChoreography.ChoreographyStepProcessEventsJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
var Data:PSystemChoreographyStepProcessEventsJobData;
    ChoreographyStep:PSystemChoreographyStep;
    SystemIndex:TpvSizeInt;
    System:TpvEntityComponentSystem.TSystem;
begin
 Data:=@aJob^.Data;
 ChoreographyStep:=Data^.ChoreographyStep;
 for SystemIndex:=0 to ChoreographyStep^.Count-1 do begin
  System:=ChoreographyStep^.Systems[SystemIndex];
  if System.fEventsCanBeParallelProcessed then begin
   ChoreographyStep^.Jobs[SystemIndex]:=CreateProcessEventsJob(System,0,System.fCountEvents-1,aJob);
  end else begin
   ChoreographyStep^.Jobs[SystemIndex]:=nil;
  end;
 end;
 fPasMPInstance.Invoke(ChoreographyStep^.Jobs);
end;

procedure TpvEntityComponentSystem.TSystemChoreography.ChoreographyProcessEventsJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
var ChoreographyStepJob:PPasMPJob;
    ChoreographyStepJobData:PSystemChoreographyStepProcessEventsJobData;
    StepIndex:TpvSizeInt;
    ChoreographyStep:PSystemChoreographyStep;
begin
 for StepIndex:=0 to fCountChoreographySteps-1 do begin
  ChoreographyStep:=@fChoreographySteps[StepIndex];
  ChoreographyStepJob:=fPasMPInstance.Acquire(ChoreographyStepProcessEventsJobFunction,nil,nil);
  ChoreographyStepJobData:=PSystemChoreographyStepProcessEventsJobData(pointer(@ChoreographyStepJob^.Data));
  ChoreographyStepJobData^.ChoreographyStep:=ChoreographyStep;
  fChoreographyStepJobs[StepIndex]:=ChoreographyStepJob;
  fPasMPInstance.Invoke(fChoreographyStepJobs[StepIndex]);
 end;
end;

procedure TpvEntityComponentSystem.TSystemChoreography.ProcessEvents;
begin
 fPasMPInstance.Invoke(fPasMPInstance.Acquire(ChoreographyProcessEventsJobFunction,nil,nil));
end;

procedure TpvEntityComponentSystem.TSystemChoreography.InitializeUpdate;
var StepIndex,SystemIndex:TpvSizeInt;
    ChoreographyStep:PSystemChoreographyStep;
    System:TSystem;
begin
 for StepIndex:=0 to fCountChoreographySteps-1 do begin
  ChoreographyStep:=@fChoreographySteps[StepIndex];
  for SystemIndex:=0 to ChoreographyStep^.Count-1 do begin
   System:=ChoreographyStep^.Systems[SystemIndex];
   System.InitializeUpdate;
  end;
 end;
end;

type PSystemChoreographyUpdateEntitiesJobData=^TSystemChoreographyUpdateEntitiesJobData;
     TSystemChoreographyUpdateEntitiesJobData=record
      System:TpvEntityComponentSystem.TSystem;
      FirstEntityIndex:TpvSizeInt;
      LastEntityIndex:TpvSizeInt;
     end;

function TpvEntityComponentSystem.TSystemChoreography.CreateUpdateEntitiesJob(const aSystem:TSystem;const aFirstEntityIndex,aLastEntityIndex:TPasMPSizeInt;const aParentJob:PPasMPJob):PPasMPJob;
var Data:PSystemChoreographyUpdateEntitiesJobData;
begin
 result:=fPasMPInstance.Acquire(UpdateEntitiesJobFunction,nil,nil);
 Data:=PSystemChoreographyUpdateEntitiesJobData(pointer(@result^.Data));
 Data^.System:=aSystem;
 Data^.FirstEntityIndex:=aFirstEntityIndex;
 Data^.LastEntityIndex:=aLastEntityIndex;
end;

procedure TpvEntityComponentSystem.TSystemChoreography.UpdateEntitiesJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
var Data:PSystemChoreographyUpdateEntitiesJobData;
    MidEntityIndex,Count:TpvSizeInt;
begin
 Data:=@aJob^.Data;
 if Data^.FirstEntityIndex<=Data^.LastEntityIndex then begin
  Count:=Data^.LastEntityIndex-Data^.FirstEntityIndex;
  if (TpvEntityComponentSystem.TSystem.TFlag.OwnUpdate in Data.System.fFlags) or
     (not (TpvEntityComponentSystem.TSystem.TFlag.ParallelProcessing in Data.System.fFlags)) or
     (fPasMPInstance.CountJobWorkerThreads<2) or
     ((Count<=Data^.System.fEntityGranularity) or (Count<4)) then begin
   if TpvEntityComponentSystem.TSystem.TFlag.OwnUpdate in Data.System.fFlags then begin
    Data^.System.Update;
   end else begin
    Data^.System.UpdateEntities(Data^.FirstEntityIndex,Data^.LastEntityIndex);
   end;
  end else begin
   MidEntityIndex:=Data^.FirstEntityIndex+((Data^.LastEntityIndex-Data^.FirstEntityIndex) shr 1);
   fPasMPInstance.Invoke([CreateUpdateEntitiesJob(Data^.System,Data^.FirstEntityIndex,MidEntityIndex-1,aJob),
                  CreateUpdateEntitiesJob(Data^.System,MidEntityIndex,Data^.LastEntityIndex,aJob)]);
  end;
 end;
end;

procedure TpvEntityComponentSystem.TSystemChoreography.ChoreographyStepUpdateEntitiesJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
var Data:PSystemChoreographyStepUpdateEntitiesJobData;
    ChoreographyStep:PSystemChoreographyStep;
    SystemIndex:TpvSizeInt;
    System:TSystem;
begin
 Data:=@aJob^.Data;
 ChoreographyStep:=Data^.ChoreographyStep;
 for SystemIndex:=0 to ChoreographyStep^.Count-1 do begin
  System:=ChoreographyStep^.Systems[SystemIndex];
  ChoreographyStep^.Jobs[SystemIndex]:=CreateUpdateEntitiesJob(System,0,System.fCountEntities-1,aJob);
 end;
 fPasMPInstance.Invoke(ChoreographyStep^.Jobs);
end;

procedure TpvEntityComponentSystem.TSystemChoreography.ChoreographyUpdateEntitiesJobFunction(const aJob:PPasMPJob;const aThreadIndex:TpvInt32);
var ChoreographyStepJob:PPasMPJob;
    ChoreographyStepJobData:PSystemChoreographyStepUpdateEntitiesJobData;
    StepIndex:TpvSizeInt;
    ChoreographyStep:PSystemChoreographyStep;
begin
 for StepIndex:=0 to fCountChoreographySteps-1 do begin
  ChoreographyStep:=@fChoreographySteps[StepIndex];
  ChoreographyStepJob:=fPasMPInstance.Acquire(ChoreographyStepUpdateEntitiesJobFunction,nil,nil);
  ChoreographyStepJobData:=PSystemChoreographyStepUpdateEntitiesJobData(pointer(@ChoreographyStepJob^.Data));
  ChoreographyStepJobData^.ChoreographyStep:=ChoreographyStep;
  fChoreographyStepJobs[StepIndex]:=ChoreographyStepJob;
  fPasMPInstance.Invoke(fChoreographyStepJobs[StepIndex]);
 end;
end;

procedure TpvEntityComponentSystem.TSystemChoreography.Update;
begin
 fPasMPInstance.Invoke(fPasMPInstance.Acquire(ChoreographyUpdateEntitiesJobFunction,nil,nil));
end;

procedure TpvEntityComponentSystem.TSystemChoreography.FinalizeUpdate;
var StepIndex,SystemIndex:TpvSizeInt;
    ChoreographyStep:PSystemChoreographyStep;
    System:TSystem;
begin
 for StepIndex:=0 to fCountChoreographySteps-1 do begin
  ChoreographyStep:=@fChoreographySteps[StepIndex];
  for SystemIndex:=0 to ChoreographyStep^.Count-1 do begin
   System:=ChoreographyStep^.Systems[SystemIndex];
   System.FinalizeUpdate;
  end;
 end;
end;

{ TpvEntityComponentSystem.TSystem }

constructor TpvEntityComponentSystem.TSystem.Create(const aWorld:TWorld);
begin
 inherited Create;
 fWorld:=aWorld;
 fFlags:=[];
 fEntities:=TEntityIDList.Create;
 fRequiredComponents:=TComponentIDList.Create;
 fExcludedComponents:=TComponentIDList.Create;
 fRequiresSystems:=TSystemList.Create;
 fRequiresSystems.OwnsObjects:=false;
 fConflictsWithSystems:=TSystemList.Create;
 fConflictsWithSystems.OwnsObjects:=false;
 fNeedToSort:=true;
 fEventsCanBeParallelProcessed:=false;
 fEventGranularity:=256;
 fEntityGranularity:=256;
 fCountEntities:=0;
 fEvents:=nil;
 fCountEvents:=0;
end;

destructor TpvEntityComponentSystem.TSystem.Destroy;
begin
 FreeAndNil(fExcludedComponents);
 FreeAndNil(fRequiredComponents);
 FreeAndNil(fRequiresSystems);
 FreeAndNil(fConflictsWithSystems);
 FreeAndNil(fEntities);
 fEvents:=nil;
 inherited Destroy;
end;

procedure TpvEntityComponentSystem.TSystem.Added;
begin
end;

procedure TpvEntityComponentSystem.TSystem.Removed;
begin
end;

procedure TpvEntityComponentSystem.TSystem.SubscribeToEvent(const aEventID:TEventID);
var EventRegistration:TEventRegistration;
begin
 fWorld.fEventRegistrationLock.AcquireWrite;
 try
  if (aEventID>=0) and (aEventID<fWorld.fEventRegistrationList.Count) then begin
   EventRegistration:=fWorld.fEventRegistrationList.Items[aEventID];
   if EventRegistration.fActive then begin
    EventRegistration.AddSystem(self);
   end;
  end;
 finally
  fWorld.fEventRegistrationLock.ReleaseWrite;
 end;
end;

procedure TpvEntityComponentSystem.TSystem.UnsubscribeFromEvent(const aEventID:TEventID);
var EventRegistration:TEventRegistration;
begin
 fWorld.fEventRegistrationLock.AcquireWrite;
 try
  if (aEventID>=0) and (aEventID<fWorld.fEventRegistrationList.Count) then begin
   EventRegistration:=fWorld.fEventRegistrationList.Items[aEventID];
   if EventRegistration.fActive then begin
    EventRegistration.RemoveSystem(self);
   end;
  end;
 finally
  fWorld.fEventRegistrationLock.ReleaseWrite;
 end;
end;

function TpvEntityComponentSystem.TSystem.HaveDependencyOnSystem(const aOtherSystem:TSystem):boolean;
begin
 result:=assigned(aOtherSystem) and (fRequiresSystems.IndexOf(aOtherSystem)>=0);
end;

function TpvEntityComponentSystem.TSystem.HaveDependencyOnSystemOrViceVersa(const aOtherSystem:TSystem):boolean;
begin
 result:=assigned(aOtherSystem) and ((fRequiresSystems.IndexOf(aOtherSystem)>=0) or (aOtherSystem.fRequiresSystems.IndexOf(self)>=0));
end;

function TpvEntityComponentSystem.TSystem.HaveCircularDependencyWithSystem(const aOtherSystem:TSystem):boolean;
var VisitedList,StackList:TList;
    Index:TpvSizeInt;
    System,RequiredSystem:TSystem;
begin
 result:=false;
 if assigned(aOtherSystem) then begin
  VisitedList:=TList.Create;
  try
   StackList:=TList.Create;
   try
    StackList.Add(aOtherSystem);
    while (StackList.Count>0) and not result do begin
     System:=StackList.Items[StackList.Count-1];
     StackList.Delete(StackList.Count-1);
     VisitedList.Add(System);
     for Index:=0 to System.fRequiresSystems.Count-1 do begin
      RequiredSystem:=System.fRequiresSystems.Items[Index];
      if RequiredSystem=self then begin
       result:=true;
       break;
      end else if VisitedList.IndexOf(RequiredSystem)<0 then begin
       StackList.Add(RequiredSystem);
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

function TpvEntityComponentSystem.TSystem.HaveConflictWithSystem(const aOtherSystem:TSystem):boolean;
begin
 result:=assigned(aOtherSystem) and (fConflictsWithSystems.IndexOf(aOtherSystem)>=0);
end;

function TpvEntityComponentSystem.TSystem.HaveConflictWithSystemOrViceVersa(const aOtherSystem:TSystem):boolean;
begin
 result:=assigned(aOtherSystem) and ((fConflictsWithSystems.IndexOf(aOtherSystem)>=0) or (aOtherSystem.fConflictsWithSystems.IndexOf(self)>=0));
end;

procedure TpvEntityComponentSystem.TSystem.RequiresSystem(const aSystem:TSystem);
begin
 if fRequiresSystems.IndexOf(aSystem)<0 then begin
  fRequiresSystems.Add(aSystem);
 end;
end;

procedure TpvEntityComponentSystem.TSystem.ConflictsWithSystem(const aSystem:TSystem);
begin
 if fConflictsWithSystems.IndexOf(aSystem)<0 then begin
  fConflictsWithSystems.Add(aSystem);
 end;
end;

procedure TpvEntityComponentSystem.TSystem.AddRequiredComponent(const aComponentID:TComponentID);
begin
 if fRequiredComponents.IndexOf(aComponentID)<0 then begin
  fRequiredComponents.Add(aComponentID);
 end;
end;

procedure TpvEntityComponentSystem.TSystem.AddExcludedComponent(const aComponentID:TComponentID);
begin
 if fExcludedComponents.IndexOf(aComponentID)<0 then begin
  fExcludedComponents.Add(aComponentID);
 end;
end;

function TpvEntityComponentSystem.TSystem.FitsEntityToSystem(const aEntityID:TEntityID):boolean;
var Index:TpvSizeInt;
begin
 result:=fWorld.HasEntity(aEntityID);
 if result then begin
  for Index:=0 to fExcludedComponents.Count-1 do begin
   if fWorld.HasEntityComponent(aEntityID,fExcludedComponents.Items[Index]) then begin
    result:=false;
    exit;
   end;
  end;
  for Index:=0 to fRequiredComponents.Count-1 do begin
   if not fWorld.HasEntityComponent(aEntityID,fRequiredComponents.Items[Index]) then begin
    result:=false;
    exit;
   end;
  end;
 end;
end;

function TpvEntityComponentSystem.TSystem.AddEntityToSystem(const aEntityID:TEntityID):boolean;
begin
 if fEntities.IndexOf(aEntityID)<0 then begin
  fEntities.Add(aEntityID);
  inc(fCountEntities);
  fNeedToSort:=true;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TpvEntityComponentSystem.TSystem.RemoveEntityFromSystem(const aEntityID:TEntityID):boolean;
var Index:TpvSizeInt;
begin
 Index:=fEntities.IndexOf(aEntityID);
 if Index>=0 then begin
  fEntities.Delete(Index);
  dec(fCountEntities);
  result:=true;
 end else begin
  result:=false;
 end;
end;

procedure TpvEntityComponentSystem.TSystem.SortEntities;
begin
 if fNeedToSort then begin
  fNeedToSort:=false;
  fEntities.Sort;
 end;
end;

procedure TpvEntityComponentSystem.TSystem.Finish;
begin
end;

procedure TpvEntityComponentSystem.TSystem.ProcessEvent(const aEvent:TpvEntityComponentSystem.TEvent);
begin
end;

procedure TpvEntityComponentSystem.TSystem.ProcessEvents(const aFirstEventIndex,aLastEventIndex:TpvSizeInt);
var EntityIndex:TpvSizeInt;
    Event:TpvEntityComponentSystem.PEvent;
begin
 for EntityIndex:=aFirstEventIndex to aLastEventIndex do begin
  Event:=fEvents[EntityIndex];
  if assigned(Event) then begin
   ProcessEvent(Event^);
  end;
 end;
end;

procedure TpvEntityComponentSystem.TSystem.InitializeUpdate;
begin
end;

procedure TpvEntityComponentSystem.TSystem.Update;
begin
end;

procedure TpvEntityComponentSystem.TSystem.UpdateEntities(const aFirstEntityIndex,aLastEntityIndex:TpvSizeInt);
begin
end;

procedure TpvEntityComponentSystem.TSystem.FinalizeUpdate;
begin
end;

{ TpvEntityComponentSystem.TWorld }

constructor TpvEntityComponentSystem.TWorld.Create(const aPasMPInstance:TPasMP);
var Index:TpvSizeInt;
begin

 inherited Create;

 fUUID:=TpvUUID.Create;

 if assigned(aPasMPInstance) then begin
  fPasMPInstance:=aPasMPInstance;
 end else begin
  fPasMPInstance:=TPasMP.GetGlobalInstance;
 end;

 fActive:=false;

 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fEntityUUIDHashMap:=TpvEntityComponentSystem.TWorld.TUUIDEntityIDPairHashMap.Create(0);

 fReservedEntityHashMapLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fReservedEntityUUIDHashMap:=TpvEntityComponentSystem.TWorld.TUUIDEntityIDPairHashMap.Create(0);

 fComponents:=TComponentList.Create;
 fComponents.OwnsObjects:=true;

 for Index:=0 to RegisteredComponentTypeList.Count-1 do begin
  fComponents.Add(TpvEntityComponentSystem.TComponent.Create(self,RegisteredComponentTypeList.Items[Index]));
 end;

 fEntities:=nil;

 fEntityLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fEntityIndexFreeList:=TEntityIndexFreeList.Create;

 fEntityGenerationList:=nil;

 fEntityUsedBitmap:=nil;

 fEntityIndexCounter:=1;

 fMaxEntityIndex:=-1;

 fEventListLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fEventList:=TList.Create;

 fDelayedEventQueueLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 LinkedListInitialize(@fDelayedEventQueue);

 fEventQueueLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 LinkedListInitialize(@fEventQueue);

 LinkedListInitialize(@fDelayedFreeEventQueue);

 fFreeEventQueueLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 LinkedListInitialize(@fFreeEventQueue);

 fCurrentTime:=0.0;

 fSystems:=TSystemList.Create;
 fSystems.OwnsObjects:=false;

 fSystemUsedMap:=TSystemBooleanPairHashMap.Create(false);

 fSystemChoreography:=TSystemChoreography.Create(self);

 fSystemChoreographyNeedToRebuild:=0;

 fEventInProcessing:=false;

 fEventRegistrationLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fEventRegistrationList:=TEventRegistrationList.Create;
 fEventRegistrationList.OwnsObjects:=false;

 fFreeEventRegistrationList:=TEventRegistrationList.Create;
 fFreeEventRegistrationList.OwnsObjects:=false;

 fEventRegistrationStringIntegerPairHashMap:=TEventRegistrationStringIntegerPairHashMap.Create(-1);

 fOnEvent:=nil;

 fDelayedManagementEventLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fDelayedManagementEvents:=nil;

 fCountDelayedManagementEvents:=0;

end;

destructor TpvEntityComponentSystem.TWorld.Destroy;
var Index:TpvSizeInt;
    Event:TpvEntityComponentSystem.PEvent;
begin

 fEventRegistrationList.OwnsObjects:=true;
 FreeAndNil(fEventRegistrationList);

 fFreeEventRegistrationList.OwnsObjects:=true;
 FreeAndNil(fFreeEventRegistrationList);

 FreeAndNil(fEventRegistrationLock);

 FreeAndNil(fEventRegistrationStringIntegerPairHashMap);

 FreeAndNil(fSystemChoreography);

 FreeAndNil(fSystemUsedMap);

 FreeAndNil(fSystems);

 FreeAndNil(fComponents);

 fEntities:=nil;

 FreeAndNil(fEntityIndexFreeList);

 fEntityGenerationList:=nil;

 fEntityUsedBitmap:=nil;

 FreeAndNil(fDelayedManagementEventLock);

 fDelayedManagementEvents:=nil;

 for Index:=0 to fEventList.Count-1 do begin
  Event:=fEventList.Items[Index];
  if assigned(Event) then begin
   Finalize(Event^);
   FreeMem(Event);
  end;
 end;
 fEventList.Free;

 fEventListLock.Free;

 fDelayedEventQueueLock.Free;

 fEventQueueLock.Free;

 fFreeEventQueueLock.Free;

 FreeAndNil(fEntityUUIDHashMap);

 FreeAndNil(fReservedEntityUUIDHashMap);

 FreeAndNil(fReservedEntityHashMapLock);

 FreeAndNil(fEntityLock);

 FreeAndNil(fLock);

 inherited Destroy;

end;

procedure TpvEntityComponentSystem.TWorld.AddDelayedManagementEvent(const aDelayedManagementEvent:TDelayedManagementEvent);
var DelayedManagementEventIndex:TpvSizeInt;
begin
 fDelayedManagementEventLock.AcquireWrite;
 try
  DelayedManagementEventIndex:=fCountDelayedManagementEvents;
  inc(fCountDelayedManagementEvents);
  if length(fDelayedManagementEvents)<fCountDelayedManagementEvents then begin
   SetLength(fDelayedManagementEvents,fCountDelayedManagementEvents+((fCountDelayedManagementEvents+1) shr 1));
  end;
  fDelayedManagementEvents[DelayedManagementEventIndex]:=aDelayedManagementEvent;
 finally
  fDelayedManagementEventLock.ReleaseWrite;
 end;
end;

function TpvEntityComponentSystem.TWorld.GetEntityByID(const aEntityID:TpvEntityComponentSystem.TEntityID):TpvEntityComponentSystem.PEntity;
var EntityIndex:TpvInt32;
begin
 EntityIndex:=aEntityID.Index;
 if (EntityIndex>=0) and
    (EntityIndex<=fMaxEntityIndex) and
    ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) then begin
  result:=@fEntities[EntityIndex];
  if result^.fID<>aEntityID then begin
   result:=nil;
  end;
 end else begin
  result:=nil;
 end;
end;

function TpvEntityComponentSystem.TWorld.GetEntityByUUID(const aEntityUUID:TpvUUID):TpvEntityComponentSystem.PEntity;
begin
 result:=nil;
end;

function TpvEntityComponentSystem.TWorld.DoCreateEntity(const aEntityID:TEntityID;const aEntityUUID:TpvUUID):boolean;
var EntityIndex,Index,OldCount,Count:TpvInt32;
    Bitmap:PpvInt32;
    Entity:PEntity;
begin

 result:=false;

 fLock.AcquireRead;
 try

  fLock.ReadToWrite;
  try

   EntityIndex:=aEntityID.Index;

   if fMaxEntityIndex<EntityIndex then begin

    fMaxEntityIndex:=EntityIndex;

    for Index:=0 to fComponents.Count-1 do begin
     fComponents[Index].SetMaxEntities(fMaxEntityIndex);
    end;

    OldCount:=length(fEntities);
    Count:=fEntityIndexCounter;
    if OldCount<Count then begin
     SetLength(fEntities,Count+((Count+1) shr 1));
     for Index:=OldCount to length(fEntities)-1 do begin
      Entity:=@fEntities[Index];
      Entity^.fWorld:=self;
      Entity^.fID:=0;
      Entity^.fFlags:=[];
      Entity^.fUUID:=TpvUUID.Null;
      Entity^.fUnknownData:=nil;
      Entity^.fCountComponents:=0;
      Entity^.fComponentsBitmap:=nil;
     end;
    end;

    OldCount:=length(fEntityGenerationList);
    Count:=fEntityIndexCounter;
    if OldCount<Count then begin
     SetLength(fEntityGenerationList,Count+((Count+1) shr 1));
     for Index:=OldCount to length(fEntityGenerationList)-1 do begin
      fEntityGenerationList[Index]:=0;
     end;
    end;

    OldCount:=length(fEntityUsedBitmap);
    Count:=(fEntityIndexCounter+31) shr 5;
    if OldCount<Count then begin
     SetLength(fEntityUsedBitmap,Count+((Count+1) shr 1));
     for Index:=OldCount to length(fEntityUsedBitmap)-1 do begin
      fEntityUsedBitmap[Index]:=0;
     end;
    end;

   end;

   Bitmap:=@fEntityUsedBitmap[EntityIndex shr 5];
   Bitmap^:=Bitmap^ or TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31));

   Entity:=@fEntities[EntityIndex];
   Entity^.fWorld:=self;
   Entity^.fID:=aEntityID;
   Entity^.fFlags:=[TEntity.TFlag.Used];
   Entity^.fUUID:=aEntityUUID;
   Entity^.fUnknownData:=nil;
   Entity^.fCountComponents:=0;
   Entity^.fComponentsBitmap:=nil;

   fEntityUUIDHashMap.Add(aEntityUUID,aEntityID);

   result:=true;

  finally
   fLock.WriteToRead;
  end;

 finally
  fLock.ReleaseRead;
 end;

end;

function TpvEntityComponentSystem.TWorld.DoDestroyEntity(const aEntityID:TEntityID):boolean;
var EntityIndex:TpvInt32;
    Index:TpvSizeInt;
    Bitmap:PpvUInt32;
    Mask:TpvUInt32;
    Entity:PEntity;
begin
 fLock.AcquireRead;
 try
  EntityIndex:=aEntityID.Index;
  Bitmap:=@fEntityUsedBitmap[EntityIndex shr 5];
  Mask:=TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31));
  if (EntityIndex>=0) and (EntityIndex<fEntityIndexCounter) and ((Bitmap^ and Mask)<>0) then begin
   fLock.ReadToWrite;
   try
    Bitmap:=@fEntityUsedBitmap[EntityIndex shr 5]; // the pointer of fEntityIDUsedBitmap could be changed already here by an another CPU thread, so reload it
    Bitmap^:=Bitmap^ and not Mask;
    for Index:=0 to fComponents.Count-1 do begin
     fComponents[Index].FreeComponentFromEntityIndex(EntityIndex);
    end;
    Entity:=@fEntities[EntityIndex];
    fEntityUUIDHashMap.Delete(Entity^.UUID);
    fReservedEntityHashMapLock.AcquireWrite;
    try
     fReservedEntityUUIDHashMap.Delete(Entity^.UUID);
    finally
     fReservedEntityHashMapLock.ReleaseWrite;
    end;
    Entity^.Flags:=[];
    FreeAndNil(Entity^.fUnknownData);
    Entity^.fCountComponents:=0;
    Entity^.fComponentsBitmap:=nil;
    fEntityLock.AcquireWrite;
    try
     fEntityIndexFreeList.Add(EntityIndex);
     inc(fEntityGenerationList[EntityIndex]);
    finally
     fEntityLock.ReleaseWrite;
    end;
   finally
    fLock.WriteToRead;
   end;
   result:=true;
  end else begin
   result:=false;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

procedure TpvEntityComponentSystem.TWorld.ProcessEvent(const aEvent:PEvent);
begin
end;

procedure TpvEntityComponentSystem.TWorld.ProcessEvents;
begin
end;

procedure TpvEntityComponentSystem.TWorld.ProcessDelayedEvents(const aDeltaTime:TTime);
begin
end;

procedure TpvEntityComponentSystem.TWorld.Kill;
begin
end;

function TpvEntityComponentSystem.TWorld.CreateEvent(const aName:TpvUTF8String):TEventID;
var EventRegistration:TEventRegistration;
begin
 fEventRegistrationLock.AcquireWrite;
 try
  result:=fEventRegistrationStringIntegerPairHashMap.Values[aName];
  if result<0 then begin
   if fFreeEventRegistrationList.Count>0 then begin
    EventRegistration:=fFreeEventRegistrationList[fFreeEventRegistrationList.Count-1];
    fFreeEventRegistrationList.Delete(fFreeEventRegistrationList.Count-1);
    EventRegistration.Clear;
    EventRegistration.fName:=aName;
   end else begin
    EventRegistration:=TEventRegistration.Create(fEventRegistrationList.Count,aName);
    fEventRegistrationList.Add(EventRegistration);
   end;
   EventRegistration.fActive:=true;
   result:=EventRegistration.fEventID;
   fEventRegistrationStringIntegerPairHashMap.Add(aName,result);
  end;
 finally
  fEventRegistrationLock.ReleaseWrite;
 end;
end;

procedure TpvEntityComponentSystem.TWorld.DestroyEvent(const aEventID:TEventID);
var EventRegistration:TEventRegistration;
begin
 fEventRegistrationLock.AcquireWrite;
 try
  if (aEventID>=0) and (aEventID<fEventRegistrationList.Count) then begin
   EventRegistration:=fEventRegistrationList.Items[aEventID];
   if EventRegistration.fActive then begin
    fEventRegistrationStringIntegerPairHashMap.Delete(EventRegistration.fName);
    EventRegistration.fActive:=false;
    EventRegistration.fName:='';
    EventRegistration.Clear;
    fFreeEventRegistrationList.Add(EventRegistration);
   end;
  end;
 finally
  fEventRegistrationLock.ReleaseWrite;
 end;
end;

function TpvEntityComponentSystem.TWorld.FindEvent(const aName:TpvUTF8String):TEventID;
begin
 fEventRegistrationLock.AcquireRead;
 try
  result:=fEventRegistrationStringIntegerPairHashMap.Values[aName];
 finally
  fEventRegistrationLock.ReleaseRead;
 end;
end;

procedure TpvEntityComponentSystem.TWorld.SubscribeToEvent(const aEventID:TEventID;const aEventHandler:TEventHandler);
var EventRegistration:TEventRegistration;
begin
 fEventRegistrationLock.AcquireWrite;
 try
  if (aEventID>=0) and (aEventID<fEventRegistrationList.Count) then begin
   EventRegistration:=fEventRegistrationList.Items[aEventID];
   if EventRegistration.fActive then begin
    EventRegistration.AddEventHandler(aEventHandler);
   end;
  end;
 finally
  fEventRegistrationLock.ReleaseWrite;
 end;
end;

procedure TpvEntityComponentSystem.TWorld.UnsubscribeFromEvent(const aEventID:TEventID;const aEventHandler:TEventHandler);
var EventRegistration:TEventRegistration;
begin
 fEventRegistrationLock.AcquireWrite;
 try
  if (aEventID>=0) and (aEventID<fEventRegistrationList.Count) then begin
   EventRegistration:=fEventRegistrationList.Items[aEventID];
   if EventRegistration.fActive then begin
    EventRegistration.RemoveEventHandler(aEventHandler);
   end;
  end;
 finally
  fEventRegistrationLock.ReleaseWrite;
 end;
end;

function TpvEntityComponentSystem.TWorld.CreateEntity(const aEntityID:TpvEntityComponentSystem.TEntityID;const aEntityUUID:TpvUUID):TpvEntityComponentSystem.TEntityID;
var DelayedManagementEvent:TDelayedManagementEvent;
    Index,OldCount,Count:TpvSizeInt;
    EntityUUID:PpvUUID;
    AutoGeneratedUUID:TpvUUID;
    UUIDIsUnused:boolean;
begin
 result:=0;
 fReservedEntityHashMapLock.AcquireRead;
 try
  if UUID=TpvUUID.Null then begin
   repeat
    AutoGeneratedUUID:=TpvUUID.Create;
   until not fReservedEntityUUIDHashMap.ExistKey(AutoGeneratedUUID);
   EntityUUID:=@AutoGeneratedUUID;
   UUIDIsUnused:=true;
  end else begin
   EntityUUID:=@UUID;
   UUIDIsUnused:=not fReservedEntityUUIDHashMap.ExistKey(EntityUUID^);
  end;
  if UUIDIsUnused then begin
   result:=aEntityID;
   if aEntityID.Index<=0 then begin
    fReservedEntityHashMapLock.ReadToWrite;
    try
     fEntityLock.AcquireWrite;
     try
      if fEntityIndexFreeList.Count>0 then begin
       result.Index:=fEntityIndexFreeList.Items[fEntityIndexFreeList.Count-1];
       fEntityIndexFreeList.Delete(fEntityIndexFreeList.Count-1);
      end else begin
       result.Index:=fEntityIndexCounter;
       inc(fEntityIndexCounter);
      end;
      OldCount:=length(fEntityGenerationList);
      Count:=fEntityIndexCounter;
      if OldCount<Count then begin
       SetLength(fEntityGenerationList,Count+((Count+1) shr 1));
       for Index:=OldCount to length(fEntityGenerationList)-1 do begin
        fEntityGenerationList[Index]:=0;
       end;
      end;
      result.Generation:=fEntityGenerationList[Index] and $ff;
     finally
      fEntityLock.ReleaseWrite;
     end;
     fReservedEntityUUIDHashMap.Add(EntityUUID^,result);
    finally
     fReservedEntityHashMapLock.WriteToRead;
    end;
   end else begin
    fReservedEntityHashMapLock.ReadToWrite;
    try
     fEntityLock.AcquireWrite;
     try
      if fEntityIndexFreeList.IndexOf(result.Index)>=0 then begin
       fEntityIndexFreeList.Remove(result.Index);
      end;
      fEntityIndexCounter:=Max(fEntityIndexCounter,result.Index+1);
     finally
      fEntityLock.ReleaseWrite;
     end;
     fReservedEntityUUIDHashMap.Add(EntityUUID^,result);
    finally
     fReservedEntityHashMapLock.WriteToRead;
    end;
   end;
  end;
 finally
  fReservedEntityHashMapLock.ReleaseRead;
 end;
 if result.Index>=0 then begin
  DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.CreateEntity;
  DelayedManagementEvent.EntityID:=result;
  DelayedManagementEvent.UUID:=EntityUUID^;
  AddDelayedManagementEvent(DelayedManagementEvent);
 end;
end;

function TpvEntityComponentSystem.TWorld.CreateEntity(const aEntityUUID:TpvUUID):TpvEntityComponentSystem.TEntityID;
var DelayedManagementEvent:TDelayedManagementEvent;
    Index,OldCount,Count:TpvSizeInt;
    EntityUUID:PpvUUID;
    UUID,AutoGeneratedUUID:TpvUUID;
    UUIDIsUnused:boolean;
begin
 result:=0;
 fReservedEntityHashMapLock.AcquireRead;
 try
  UUID:=aEntityUUID;
  if UUID=TpvUUID.Null then begin
   repeat
    AutoGeneratedUUID:=TpvUUID.Create;
   until not fReservedEntityUUIDHashMap.ExistKey(AutoGeneratedUUID);
   EntityUUID:=@AutoGeneratedUUID;
   UUIDIsUnused:=true;
  end else begin
   EntityUUID:=@UUID;
   UUIDIsUnused:=not fReservedEntityUUIDHashMap.ExistKey(EntityUUID^);
  end;
  if UUIDIsUnused then begin
   fReservedEntityHashMapLock.ReadToWrite;
   try
    fEntityLock.AcquireWrite;
    try
     if fEntityIndexFreeList.Count>0 then begin
      result.Index:=fEntityIndexFreeList.Items[fEntityIndexFreeList.Count-1];
      fEntityIndexFreeList.Delete(fEntityIndexFreeList.Count-1);
     end else begin
      result.Index:=fEntityIndexCounter;
      inc(fEntityIndexCounter);
     end;
     OldCount:=length(fEntityGenerationList);
     Count:=fEntityIndexCounter;
     if OldCount<Count then begin
      SetLength(fEntityGenerationList,Count+((Count+1) shr 1));
      for Index:=OldCount to length(fEntityGenerationList)-1 do begin
       fEntityGenerationList[Index]:=0;
      end;
     end;
     result.Generation:=fEntityGenerationList[Index] and $ff;
    finally
     fEntityLock.ReleaseWrite;
    end;
    fReservedEntityUUIDHashMap.Add(EntityUUID^,result);
   finally
    fReservedEntityHashMapLock.WriteToRead;
   end;
  end;
 finally
  fReservedEntityHashMapLock.ReleaseRead;
 end;
 if result<>0 then begin
  DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.CreateEntity;
  DelayedManagementEvent.EntityID:=result;
  DelayedManagementEvent.UUID:=EntityUUID^;
  AddDelayedManagementEvent(DelayedManagementEvent);
 end;
end;

function TpvEntityComponentSystem.TWorld.CreateEntity:TEntityID; overload;
begin
 result:=CreateEntity(TpvUUID.Null);
end;

function TpvEntityComponentSystem.TWorld.HasEntity(const aEntityID:TpvEntityComponentSystem.TEntityID):boolean;
var EntityIndex:TpvInt32;
begin
 EntityIndex:=aEntityID.Index;
 fLock.AcquireRead;
 try
  result:=(EntityIndex>=0) and
          (EntityIndex<=fMaxEntityIndex) and
          ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) and
          (fEntities[EntityIndex].fID=aEntityID);
 finally
  fLock.ReleaseRead;
 end;
end;

function TpvEntityComponentSystem.TWorld.IsEntityActive(const aEntityID:TpvEntityComponentSystem.TEntityID):boolean;
var EntityIndex:TpvInt32;
begin
 EntityIndex:=aEntityID.Index;
 fLock.AcquireRead;
 try
  result:=(EntityIndex>=0) and
          (EntityIndex<=fMaxEntityIndex) and
          ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) and
          (fEntities[EntityIndex].fID=aEntityID) and
          (TpvEntityComponentSystem.TEntity.TFlag.Active in fEntities[EntityIndex].fFlags);
 finally
  fLock.ReleaseRead;
 end;
end;

procedure TpvEntityComponentSystem.TWorld.ActivateEntity(const aEntityID:TEntityID);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.ActivateEntity;
 DelayedManagementEvent.EntityID:=aEntityID;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

procedure TpvEntityComponentSystem.TWorld.DeactivateEntity(const aEntityID:TEntityID);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.DeactivateEntity;
 DelayedManagementEvent.EntityID:=aEntityID;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

procedure TpvEntityComponentSystem.TWorld.KillEntity(const aEntityID:TEntityID);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.DeactivateEntity;
 DelayedManagementEvent.EntityID:=aEntityID;
 AddDelayedManagementEvent(DelayedManagementEvent);
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.KillEntity;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

procedure TpvEntityComponentSystem.TWorld.AddComponentToEntity(const aEntityID:TEntityID;const aComponentID:TComponentID);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.AddComponentToEntity;
 DelayedManagementEvent.EntityID:=aEntityID;
 DelayedManagementEvent.ComponentID:=aComponentID;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

procedure TpvEntityComponentSystem.TWorld.RemoveComponentFromEntity(const aEntityID:TEntityID;const aComponentID:TComponentID);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.RemoveComponentFromEntity;
 DelayedManagementEvent.EntityID:=aEntityID;
 DelayedManagementEvent.ComponentID:=aComponentID;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

function TpvEntityComponentSystem.TWorld.HasEntityComponent(const aEntityID:TEntityID;const aComponentID:TComponentID):boolean;
var EntityIndex:TpvInt32;
begin
 EntityIndex:=aEntityID.Index;
 fLock.AcquireRead;
 try
  result:=(EntityIndex>=0) and
          (EntityIndex<=fMaxEntityIndex) and
          ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) and
          (fEntities[EntityIndex].fID=aEntityID) and
          fComponents[aComponentID].IsComponentInEntityIndex(EntityIndex);
 finally
  fLock.ReleaseRead;
 end;
end;

procedure TpvEntityComponentSystem.TWorld.AddSystem(const aSystem:TSystem);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.AddSystem;
 DelayedManagementEvent.System:=aSystem;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

procedure TpvEntityComponentSystem.TWorld.RemoveSystem(const aSystem:TSystem);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.RemoveSystem;
 DelayedManagementEvent.System:=aSystem;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

procedure TpvEntityComponentSystem.TWorld.SortSystem(const aSystem:TSystem);
var DelayedManagementEvent:TDelayedManagementEvent;
begin
 DelayedManagementEvent.EventType:=TpvEntityComponentSystem.TDelayedManagementEventType.SortSystem;
 DelayedManagementEvent.System:=aSystem;
 AddDelayedManagementEvent(DelayedManagementEvent);
end;

procedure TpvEntityComponentSystem.TWorld.Defragment;
begin
end;

procedure TpvEntityComponentSystem.TWorld.Refresh;
var DelayedManagementEventIndex,Index,EntityIndex:TpvSizeInt;
    DelayedManagementEvent:PDelayedManagementEvent;
    EntityID:TEntityID;
    Entity:PEntity;
    Component:TpvEntityComponentSystem.TComponent;
    System:TpvEntityComponentSystem.TSystem;
    EntitiesWereAdded,EntitiesWereRemoved,WasActive:boolean;
begin

 EntitiesWereAdded:=false;
 EntitiesWereRemoved:=false;

 fDelayedManagementEventLock.AcquireRead;
 try

  DelayedManagementEventIndex:=0;
  while DelayedManagementEventIndex<fCountDelayedManagementEvents do begin
   DelayedManagementEvent:=@fDelayedManagementEvents[DelayedManagementEventIndex];
   inc(DelayedManagementEventIndex);
   case DelayedManagementEvent^.EventType of
    TpvEntityComponentSystem.TDelayedManagementEventType.CreateEntity:begin
     EntityID:=DelayedManagementEvent^.EntityID;
     EntityIndex:=EntityID.Index;
     if (EntityIndex>=0) and (EntityIndex<fEntityIndexCounter) then begin
      DoCreateEntity(EntityID,DelayedManagementEvent^.UUID);
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.ActivateEntity:begin
     EntityID:=DelayedManagementEvent^.EntityID;
     EntityIndex:=EntityID.Index;
     if (EntityIndex>=0) and
        (EntityIndex<fEntityIndexCounter) and
        ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) and
        not (TpvEntityComponentSystem.TEntity.TFlag.Active in fEntities[EntityIndex].Flags) then begin
      for Index:=0 to fSystems.Count-1 do begin
       System:=fSystems.Items[Index];
       if System.FitsEntityToSystem(EntityID) then begin
        System.AddEntityToSystem(EntityID);
        EntitiesWereAdded:=true;
       end;
      end;
      Include(fEntities[EntityID].fFlags,TpvEntityComponentSystem.TEntity.TFlag.Active);
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.DeactivateEntity:begin
     EntityID:=DelayedManagementEvent^.EntityID;
     EntityIndex:=EntityID.Index;
     if (EntityIndex>=0) and
        (EntityIndex<fEntityIndexCounter) and
        ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) and
        (TpvEntityComponentSystem.TEntity.TFlag.Active in fEntities[EntityIndex].Flags) then begin
      Exclude(fEntities[EntityID].fFlags,TpvEntityComponentSystem.TEntity.TFlag.Active);
      for Index:=0 to fSystems.Count-1 do begin
       System:=fSystems.Items[Index];
       System.RemoveEntityFromSystem(EntityID);
       EntitiesWereRemoved:=true;
      end;
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.KillEntity:begin
     EntityID:=DelayedManagementEvent^.EntityID;
     EntityIndex:=EntityID.Index;
     if (EntityIndex>=0) and
        (EntityIndex<fEntityIndexCounter) and
        ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) then begin
      for Index:=0 to fSystems.Count-1 do begin
       System:=fSystems.Items[Index];
       System.RemoveEntityFromSystem(EntityID);
       EntitiesWereRemoved:=true;
      end;
      fDelayedManagementEventLock.ReleaseRead;
      try
       DoDestroyEntity(EntityID);
      finally
       fDelayedManagementEventLock.AcquireRead;
      end;
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.AddComponentToEntity:begin
     EntityID:=DelayedManagementEvent^.EntityID;
     EntityIndex:=EntityID.Index;
     if (EntityIndex>=0) and
        (EntityIndex<fEntityIndexCounter) and
        ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) then begin
      if{(DelayedManagementEvent^.ComponentID>=0) and}(DelayedManagementEvent^.ComponentID<fComponents.Count) then begin
       Component:=fComponents[DelayedManagementEvent^.ComponentID];
      end else begin
       Component:=nil;
      end;
      if assigned(Component) and (not Component.IsComponentInEntityIndex(EntityIndex)) and Component.AllocateComponentForEntityIndex(EntityIndex) then begin
       WasActive:=TpvEntityComponentSystem.TEntity.TFlag.Active in fEntities[EntityIndex].Flags;
       if WasActive then begin
        for Index:=0 to fSystems.Count-1 do begin
         System:=fSystems.Items[Index];
         System.RemoveEntityFromSystem(EntityID);
         EntitiesWereRemoved:=true;
        end;
       end;
       Entity:=@fEntities[EntityIndex];
       Entity^.AddComponentToEntity(DelayedManagementEvent^.ComponentID);
       if WasActive then begin
        for Index:=0 to fSystems.Count-1 do begin
         System:=TSystem(fSystems.Items[Index]);
         if System.FitsEntityToSystem(EntityID) then begin
          System.AddEntityToSystem(EntityID);
          EntitiesWereAdded:=true;
         end;
        end;
       end;
      end;
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.RemoveComponentFromEntity:begin
     EntityID:=DelayedManagementEvent^.EntityID;
     EntityIndex:=EntityID.Index;
     if (EntityIndex>=0) and
        (EntityIndex<fEntityIndexCounter) and
        ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) then begin
      if{(DelayedManagementEvent^.ComponentID>=0) and}(DelayedManagementEvent^.ComponentID<fComponents.Count) then begin
       Component:=fComponents[DelayedManagementEvent^.ComponentID];
      end else begin
       Component:=nil;
      end;
      if assigned(Component) and Component.IsComponentInEntityIndex(EntityIndex) and Component.FreeComponentFromEntityIndex(EntityIndex) then begin
       WasActive:=TpvEntityComponentSystem.TEntity.TFlag.Active in fEntities[EntityIndex].Flags;
       if WasActive then begin
        for Index:=0 to fSystems.Count-1 do begin
         System:=fSystems.Items[Index];
         System.RemoveEntityFromSystem(EntityID);
         EntitiesWereRemoved:=true;
        end;
       end;
       Entity:=@fEntities[EntityIndex];
       Entity^.RemoveComponentFromEntity(DelayedManagementEvent^.ComponentID);
       if WasActive then begin
        for Index:=0 to fSystems.Count-1 do begin
         System:=TSystem(fSystems.Items[Index]);
         if System.FitsEntityToSystem(EntityID) then begin
          System.AddEntityToSystem(EntityID);
          EntitiesWereAdded:=true;
         end;
        end;
       end;
      end;
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.AddSystem:begin
     System:=DelayedManagementEvent^.System;
     if not fSystemUsedMap.Values[System] then begin
      fSystemUsedMap.Add(System,true);
      fSystems.Add(System);
      InterlockedExchange(fSystemChoreographyNeedToRebuild,-1);
      for EntityIndex:=0 to fEntityIndexCounter-1 do begin
       if (EntityIndex>=0) and
           (EntityIndex<fEntityIndexCounter) and
           ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl longword(EntityIndex and 31)))<>0) and
           (TpvEntityComponentSystem.TEntity.TFlag.Active in fEntities[EntityIndex].Flags) and
           System.FitsEntityToSystem(fEntities[EntityIndex].fID) then begin
        System.AddEntityToSystem(fEntities[EntityIndex].fID);
        EntitiesWereAdded:=true;
       end;
      end;
      System.Added;
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.RemoveSystem:begin
     System:=DelayedManagementEvent^.System;
     if fSystemUsedMap.Values[System] then begin
      System.Removed;
      fSystemUsedMap.Delete(System);
      fSystems.Remove(System);
      InterlockedExchange(fSystemChoreographyNeedToRebuild,-1);
     end;
    end;
    TpvEntityComponentSystem.TDelayedManagementEventType.SortSystem:begin
     System:=DelayedManagementEvent^.System;
     if fSystemUsedMap.Values[System] then begin
      System.SortEntities;
     end;
    end;
   end;
  end;
  fDelayedManagementEventLock.ReadToWrite;
  try
   fCountDelayedManagementEvents:=0;
  finally
   fDelayedManagementEventLock.WriteToRead;
  end;
 finally
  fDelayedManagementEventLock.ReleaseRead;
 end;

 if EntitiesWereAdded then begin
 end;

 if EntitiesWereRemoved then begin
 end;

end;

procedure TpvEntityComponentSystem.TWorld.QueueEvent(const aEventToQueue:TpvEntityComponentSystem.TEvent;const aDeltaTime:TpvTime);
var ParameterIndex:TpvSizeInt;
    Event:TpvEntityComponentSystem.PEvent;
begin
 fFreeEventQueueLock.AcquireWrite;
 try
  Event:=LinkedListPopFront(@fFreeEventQueue);
 finally
  fFreeEventQueueLock.ReleaseWrite;
 end;
 if not assigned(Event) then begin
  GetMem(Event,SizeOf(TEvent));
  FillChar(Event^,SizeOf(TEvent),#0);
  fEventListLock.AcquireWrite;
  try
   fEventList.Add(Event);
  finally
   fEventListLock.ReleaseWrite;
  end;
 end;
 LinkedListInitialize(pointer(Event));
 if aDeltaTime>0 then begin
  fDelayedEventQueueLock.AcquireWrite;
  try
   LinkedListPushBack(@fDelayedEventQueue,pointer(Event));
  finally
   fDelayedEventQueueLock.ReleaseWrite;
  end;
 end else begin
  fEventQueueLock.AcquireWrite;
  try
   LinkedListPushBack(@fEventQueue,pointer(Event));
  finally
   fEventQueueLock.ReleaseWrite;
  end;
 end;
 Event^.TimeStamp:=fCurrentTime+aDeltaTime;
 Event^.RemainingTime:=aDeltaTime;
 Event^.EventID:=aEventToQueue.EventID;
 Event^.EntityID:=aEventToQueue.EntityID;
 Event^.CountParameters:=aEventToQueue.CountParameters;
 if length(Event^.Parameters)<Event^.CountParameters then begin
  SetLength(Event^.Parameters,Event^.CountParameters*2);
 end;
 for ParameterIndex:=0 to Event^.CountParameters-1 do begin
  Event^.Parameters[ParameterIndex]:=aEventToQueue.Parameters[ParameterIndex];
 end;
 if assigned(fOnEvent) then begin
  fOnEvent(self,Event^);
 end;
end;

procedure TpvEntityComponentSystem.TWorld.QueueEvent(const aEventToQueue:TpvEntityComponentSystem.TEvent);
begin
 QueueEvent(aEventToQueue,1.0e-18); // one femtosecond
end;

procedure TpvEntityComponentSystem.TWorld.Update(const aDeltaTime:TpvTime);
var SystemIndex:TpvSizeInt;
    System:TSystem;
begin
 for SystemIndex:=0 to fSystems.Count-1 do begin
  System:=fSystems.Items[SystemIndex];
  System.fCountEvents:=0;
  System.fDeltaTime:=aDeltaTime;
 end;
 ProcessEvents;
 if InterlockedCompareExchange(fSystemChoreographyNeedToRebuild,0,-1)<0 then begin
  fSystemChoreography.Build;
 end;
 fSystemChoreography.ProcessEvents;
 fSystemChoreography.InitializeUpdate;
 fSystemChoreography.Update;
 fSystemChoreography.FinalizeUpdate;
 fCurrentTime:=fCurrentTime+aDeltaTime;
 ProcessDelayedEvents(aDeltaTime);
 Refresh;
end;

procedure TpvEntityComponentSystem.TWorld.Clear;
var EntityIndex,SystemIndex:TpvSizeInt;
begin
 fLock.AcquireRead;
 try
  if fEntityIndexCounter>0 then begin
   fLock.ReleaseRead;
   try
    for EntityIndex:=0 to fEntityIndexCounter-1 do begin
     if (fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0 then begin
      KillEntity(fEntities[EntityIndex].fID);
     end;
    end;
    Refresh;
   finally
    fLock.AcquireRead;
   end;
  end;
  if fSystems.Count>0 then begin
   fLock.ReleaseRead;
   try
    for SystemIndex:=0 to fSystems.Count-1 do begin
     RemoveSystem(fSystems.Items[SystemIndex]);
    end;
    Refresh;
   finally
    fLock.AcquireRead;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
 fEntityIndexCounter:=0;
 fEntityUUIDHashMap.Clear;
 fReservedEntityUUIDHashMap.Clear;
 fEntityIndexFreeList.Clear;
end;

procedure TpvEntityComponentSystem.TWorld.ClearEntities;
var EntityIndex:TpvSizeInt;
begin
 fLock.AcquireRead;
 try
  if fEntityIndexCounter>0 then begin
   fLock.ReleaseRead;
   try
    for EntityIndex:=0 to fEntityIndexCounter-1 do begin
     if (fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0 then begin
      KillEntity(fEntities[EntityIndex].fID);
     end;
    end;
    Refresh;
   finally
    fLock.AcquireRead;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
 fEntityIndexCounter:=0;
 fEntityUUIDHashMap.Clear;
 fReservedEntityUUIDHashMap.Clear;
 fEntityIndexFreeList.Clear;
end;

procedure TpvEntityComponentSystem.TWorld.Activate;
begin
 fActive:=true;
end;

procedure TpvEntityComponentSystem.TWorld.Deactivate;
begin
 fActive:=false;
end;

procedure TpvEntityComponentSystem.TWorld.MementoSerialize(const aStream:TStream);
var EntityIndex,ComponentIndex:TpvInt32;
    ComponentID:TComponentID;
    BitCounter:TpvInt32;
    BitTag:TpvUInt8;
    BitPosition:TpvInt64;
    EntityID:TEntityID;
    Entity:PEntity;
    Component:TpvEntityComponentSystem.TComponent;
    BufferedStream:TStream;
 procedure WriteBit(const aValue:boolean);
 var OldPosition:TpvInt64;
 begin
  if BitCounter>=8 then begin
   if BitPosition>=0 then begin
    OldPosition:=BufferedStream.Position;
    if BufferedStream.Seek(BitPosition,soBeginning)<>BitPosition then begin
     raise EInOutError.Create('Stream seek error');
    end;
    if BufferedStream.Write(BitTag,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
     raise EInOutError.Create('Stream write error');
    end;
    if BufferedStream.Seek(OldPosition,soBeginning)<>OldPosition then begin
     raise EInOutError.Create('Stream seek error');
    end;
   end;
   BitTag:=0;
   BitCounter:=0;
   BitPosition:=BufferedStream.Position;
   if BufferedStream.Write(BitTag,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
    raise EInOutError.Create('Stream write error');
   end;
  end;
  if aValue then begin
   BitTag:=BitTag or (TpvUInt8(1) shl BitCounter);
  end else begin
   BitTag:=BitTag and not (TpvUInt8(1) shl BitCounter);
  end;
  inc(BitCounter);
 end;
 procedure FlushBits;
 var OldPosition:TpvInt64;
 begin
  if BitPosition>=0 then begin
   OldPosition:=BufferedStream.Position;
   if BufferedStream.Seek(BitPosition,soBeginning)<>BitPosition then begin
    raise EInOutError.Create('Stream seek error');
   end;
   if BufferedStream.Write(BitTag,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
    raise EInOutError.Create('Stream write error');
   end;
   if BufferedStream.Seek(OldPosition,soBeginning)<>OldPosition then begin
    raise EInOutError.Create('Stream seek error');
   end;
  end;
 end;
 procedure WriteUInt8(const aValue:TpvUInt8);
 begin
  if BufferedStream.Write(aValue,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
   raise EInOutError.Create('Stream write error');
  end;
 end;
 procedure WriteInt32(const aValue:TpvInt32);
 begin
  if BufferedStream.Write(aValue,SizeOf(TpvInt32))<>SizeOf(TpvInt32) then begin
   raise EInOutError.Create('Stream write error');
  end;
 end;
begin
 Refresh;
 BufferedStream:=TMemoryStream.Create;
 try
  BitTag:=0;
  BitCounter:=8;
  BitPosition:=-1;
  if BufferedStream.Write(fUUID,SizeOf(TpvUUID))<>SizeOf(TpvUUID) then begin
   raise EInOutError.Create('Stream write error');
  end;
  WriteInt32(fEntityIndexCounter);
  for EntityIndex:=0 to fEntityIndexCounter-1 do begin
   if (EntityIndex>=0) and
      (EntityIndex<=fMaxEntityIndex) and
      ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) then begin
    EntityID:=fEntities[EntityIndex].fID;
    WriteBit(true);
    Entity:=@fEntities[EntityID];
    WriteUInt8(EntityID.Generation);
    WriteBit(Entity^.Active);
    BufferedStream.Write(Entity^.fUUID,SizeOf(TpvUUID));
    WriteInt32(Entity^.fCountComponents);
    for ComponentIndex:=0 to Entity^.fCountComponents-1 do begin
     ComponentID:=ComponentIndex;
     if{(DelayedManagementEvent^.ComponentID>=0) and}(ComponentID<fComponents.Count) then begin
      Component:=fComponents[ComponentID];
     end else begin
      Component:=nil;
     end;
     if assigned(Component) then begin
      WriteBit(true);
      BufferedStream.Write(Component.ComponentByEntityIndex[EntityIndex]^,Component.fSize);
     end else begin
      WriteBit(false);
     end;
    end;
   end else begin
    WriteBit(false);
   end;
  end;
  FlushBits;
  BufferedStream.Seek(0,soBeginning);
  aStream.CopyFrom(BufferedStream,BufferedStream.Size);
 finally
  BufferedStream.Free;
 end;
end;

procedure TpvEntityComponentSystem.TWorld.MementoUnserialize(const aStream:TStream);
var ComponentID:TComponentID;
    EntityIndex,LocalEntityCounter,CountEntityComponents,
    EntityComponentIndex,BitCounter:TpvInt32;
    BitTag,Generation:TpvUInt8;
    EntityID:TEntityID;
    Entity:PEntity;
    Component:TpvEntityComponentSystem.TComponent;
    BufferedStream:TStream;
    TempUUID:TpvUUID;
    HasNewEntity,IsActive,HasNewComponent:boolean;
 function ReadBit:boolean;
 begin
  if BitCounter>=8 then begin
   BitTag:=0;
   BitCounter:=0;
   if BufferedStream.Read(BitTag,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
    raise EInOutError.Create('Stream write error');
   end;
  end;
  result:=(BitTag and (TpvUInt8(1) shl BitCounter))<>0;
  inc(BitCounter);
 end;
 function ReadUInt8:TpvUInt8;
 begin
  if BufferedStream.Read(result,SizeOf(TpvUInt8))<>SizeOf(TpvUInt8) then begin
   raise EInOutError.Create('Stream read error');
  end;
 end;
 function ReadInt32:TpvInt32;
 begin
  if BufferedStream.Read(result,SizeOf(TpvInt32))<>SizeOf(TpvInt32) then begin
   raise EInOutError.Create('Stream read error');
  end;
 end;
begin
 Refresh;
 aStream.Seek(0,soBeginning);
 BufferedStream:=TMemoryStream.Create;
 try
  BufferedStream.CopyFrom(aStream,aStream.Size);
  BufferedStream.Seek(0,soBeginning);
  BitTag:=0;
  BitCounter:=8;
  if BufferedStream.Read(TempUUID,SizeOf(TpvUUID))<>SizeOf(TpvUUID) then begin
   raise EInOutError.Create('Stream read error');
  end;
  LocalEntityCounter:=ReadInt32;
  for EntityIndex:=0 to Max(LocalEntityCounter,fEntityIndexCounter)-1 do begin
   if EntityIndex<LocalEntityCounter then begin
    HasNewEntity:=ReadBit;
   end else begin
    HasNewEntity:=false;
   end;
   if HasNewEntity then begin
    Generation:=ReadUInt8;
    IsActive:=ReadBit;
    if BufferedStream.Read(TempUUID,SizeOf(TpvUUID))<>SizeOf(TpvUUID) then begin
     raise EInOutError.Create('Stream read error');
    end;
    if (EntityIndex>=0) and
       (EntityIndex<=fMaxEntityIndex) and
       ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) then begin
     Entity:=@fEntities[EntityIndex];
     EntityID:=Entity^.ID;
     if Generation<>EntityID.Generation then begin
      fEntityUUIDHashMap.Delete(Entity.fUUID);
      Entity.fUUID:=TempUUID;
      fEntityUUIDHashMap.Add(Entity.fUUID,EntityID);
      Entity^.ID.Generation:=Generation;
      EntityID:=Entity^.ID;
     end;
     if TempUUID<>Entity.fUUID then begin
      fEntityUUIDHashMap.Delete(Entity.fUUID);
      Entity.fUUID:=TempUUID;
      fEntityUUIDHashMap.Add(Entity.fUUID,EntityID);
     end;
    end else begin
     EntityID.Index:=EntityIndex;
     EntityID.Generation:=Generation;
     EntityID:=CreateEntity(EntityID,TempUUID);
     Refresh;
     Entity:=@fEntities[EntityIndex];
    end;
    CountEntityComponents:=ReadInt32;
    for EntityComponentIndex:=0 to Max(CountEntityComponents,Entity.fCountComponents)-1 do begin
     if EntityComponentIndex<CountEntityComponents then begin
      HasNewComponent:=ReadBit;
     end else begin
      HasNewComponent:=false;
     end;
     if HasNewComponent then begin
      ComponentID:=EntityComponentIndex;
      if{(ComponentID>=0) and}(ComponentID<fComponents.Count) then begin
       Component:=fComponents[ComponentID];
      end else begin
       Component:=nil;
      end;
      if assigned(Component) then begin
       if not Component.IsComponentInEntityIndex(EntityIndex) then begin
        AddComponentToEntity(EntityID,ComponentID);
        Refresh;
       end;
       BufferedStream.Read(Component.ComponentByEntityIndex[EntityIndex]^,Component.fSize);
      end;
     end else if EntityComponentIndex<Entity.fCountComponents then begin
      ComponentID:=EntityComponentIndex;
      if{(ComponentID>=0) and}(ComponentID<fComponents.Count) then begin
       Component:=fComponents[ComponentID];
      end else begin
       Component:=nil;
      end;
      if assigned(Component) then begin
       if Component.IsComponentInEntityIndex(EntityIndex) then begin
        RemoveComponentFromEntity(EntityID,ComponentID);
        Refresh;
       end;
      end;
     end;
    end;
    if IsActive then begin
     ActivateEntity(EntityID);
    end else begin
     DeactivateEntity(EntityID);
    end;
    Refresh;
   end else begin
    if (EntityIndex>=0) and
       (EntityIndex<=fMaxEntityIndex) and
       ((fEntityUsedBitmap[EntityIndex shr 5] and TpvUInt32(TpvUInt32(1) shl TpvUInt32(EntityIndex and 31)))<>0) then begin
     Entity:=@fEntities[EntityIndex];
     EntityID:=Entity^.ID;
     KillEntity(EntityID);
     Refresh;
    end;
   end;
  end;
  Refresh;
 finally
  BufferedStream.Free;
 end;
end;

procedure InitializeEntityComponentSystemGlobals;
begin
 if not assigned(RegisteredComponentTypeList) then begin
  RegisteredComponentTypeList:=TpvEntityComponentSystem.TRegisteredComponentTypeList.Create;
  RegisteredComponentTypeList.OwnsObjects:=true;
 end;
 if not assigned(RegisteredComponentTypeNameHashMap) then begin
  RegisteredComponentTypeNameHashMap:=TpvEntityComponentSystem.TRegisteredComponentTypeNameHashMap.Create(nil);
 end;
end;

initialization
 InitializeEntityComponentSystemGlobals;
finalization
 FreeAndNil(RegisteredComponentTypeList);
 FreeAndNil(RegisteredComponentTypeNameHashMap);
end.

