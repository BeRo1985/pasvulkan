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
unit PasVulkan.Resources;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

{$if defined(cpu386) or defined(cpuamd64) or defined(cpux86_64) or defined(cpux64)}
 {$define WordReadsAndWritesAreAtomic}
{$else}
 {$undef WordReadsAndWritesAreAtomic}
{$ifend}

interface

uses {$ifdef Windows}
      Windows,
     {$endif}
     SysUtils,
     Classes,
     PasMP,
     PasJSON,
     PasVulkan.Types,
     PasVulkan.Collections,
     PasVulkan.HighResolutionTimer;

type EpvResource=class(Exception);

     EpvResourceClass=class(EpvResource);

     EpvResourceClassNull=class(EpvResourceClass);

     EpvResourceClassMismatch=class(EpvResourceClass);

     TpvResourceManager=class;

     TpvResource=class;

     TpvResourceClass=class of TpvResource;

     TpvResourceClassType=class;

     IpvResource=interface(IpvReferenceCountedObject)['{AD2C0315-C8AF-4D79-876E-1FA42FB869F9}']
      function GetResource:TpvResource;
      function GetResourceClass:TpvResourceClass;
      function WaitFor:boolean;
     end;

     TpvResourceOnFinish=procedure(const aResource:TpvResource;const aSuccess:boolean) of object;

     TpvResource=class(TpvReferenceCountedObject,IpvResource)
      public
       type TAsyncLoadState=
             (
              None,
              Done,
              Queued,
              Loading,
              Success,
              Fail
             );
            PAsyncLoadState=^TAsyncLoadState;
       const VirtualFileNamePrefix:TpvUTF8String='virtual://';
      private
       fResourceManager:TpvResourceManager;
       fParent:TpvResource;
       fResourceClassType:TpvResourceClassType;
       fFileName:TpvUTF8String;
       fAsyncLoadState:TAsyncLoadState;
       fLoaded:boolean;
       fIsOnDelayedToFreeResourcesList:TPasMPBool32;
       fMemoryUsage:TpvUInt64;
       fMetaData:TPasJSONItem;
       fInstanceInterface:IpvResource;
       fOnFinish:TpvResourceOnFinish;
       fReleaseFrameDelay:TPasMPInt32; // for resources with frame-wise in-flight data stuff
       procedure SetFileName(const aFileName:TpvUTF8String);
      protected
       function _AddRef:TpvInt32; override; {$ifdef Windows}stdcall{$else}cdecl{$endif};
       function _Release:TpvInt32; override; {$ifdef Windows}stdcall{$else}cdecl{$endif};
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure PrepareDeferredFree; virtual;
       procedure DeferredFree; virtual;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       function GetResource:TpvResource;
       function GetResourceClass:TpvResourceClass;
       function WaitFor:boolean;
       function CreateNewFileStreamFromFileName(const aFileName:TpvUTF8String):TStream; virtual;
       function GetStreamFromFileName(const aFileName:TpvUTF8String):TStream; virtual;
       function LoadMetaData(const aStream:TStream):boolean; overload; virtual;
       function SaveMetaData(const aStream:TStream):boolean; overload; virtual;
       function LoadMetaData:boolean; overload; virtual;
       function SaveMetaData:boolean; overload; virtual;
       function BeginLoad(const aStream:TStream):boolean; virtual;
       function EndLoad:boolean; virtual;
       function Load(const aStream:TStream):boolean; virtual;
       function Save:boolean; virtual;
       function LoadFromFileName(const aFileName:TpvUTF8String):boolean; virtual;
       function SaveToFileName(const aFileName:TpvUTF8String):boolean; virtual;
      public
       property InstanceInterface:IpvResource read fInstanceInterface;
       property MemoryUsage:TpvUInt64 read fMemoryUsage write fMemoryUsage;
      published
       property ResourceManager:TpvResourceManager read fResourceManager;
       property Parent:TpvResource read fParent;
       property ResourceClassType:TpvResourceClassType read fResourceClassType;
       property FileName:TpvUTF8String read fFileName write SetFileName;
       property AsyncLoadState:TAsyncLoadState read fAsyncLoadState write fAsyncLoadState;
       property Loaded:boolean read fLoaded write fLoaded;
       property MetaData:TPasJSONItem read fMetaData write fMetaData;
       property OnFinish:TpvResourceOnFinish read fOnFinish write fOnFinish;
       property ReleaseFrameDelay:TPasMPInt32 read fReleaseFrameDelay write fReleaseFrameDelay;
     end;

     TpvResourceBackgroundLoader=class(TPasMPThread)
      public
       type TQueueItem=class
             public
              type TResourcArray=TpvDynamicArray<TpvResource>;
             private
              fResourceBackgroundLoader:TpvResourceBackgroundLoader;
              fResource:TpvResource;
              fDependencies:TResourcArray;
              fDependents:TResourcArray;
             public
              constructor Create(const aResourceBackgroundLoader:TpvResourceBackgroundLoader;const aResource:TpvResource); reintroduce;
              destructor Destroy; override;
            end;
           TQueueItems=TpvDynamicArray<TQueueItem>;
           TQueueItemResourceMap=class(TpvHashMap<TpvResource,TQueueItem>);
           TQueueItemStringMap=class(TpvStringHashMap<TQueueItem>);
      private
       fResourceManager:TpvResourceManager;
       fEvent:TPasMPEvent;
       fLock:TPasMPSlimReaderWriterLock;
       fQueueItems:TQueueItems;
       fQueueItemResourceMap:TQueueItemResourceMap;
       function QueueResource(const aResource:TpvResource;const aParent:TpvResource):boolean;
       procedure FinalizeQueueItem(const aQueueItem:TQueueItem);
       procedure WaitForResource(const aResource:TpvResource);
       function ProcessIteration(const aStartTime:TpvHighResolutionTime;const aTimeout:TpvInt64):boolean;
       function Process(const aTimeout:TpvInt64=5):boolean;
       function WaitForResources(const aTimeout:TpvInt64=-1):boolean;
       function GetCountOfQueuedResources:TpvSizeInt;
      protected
       procedure Execute; override;
      public
       constructor Create(const aResourceManager:TpvResourceManager); reintroduce;
       destructor Destroy; override;
     end;

     TpvResourceClassType=class
      private
       type TResourceList=class(TpvObjectGenericList<TpvResource>);
            TResourceStringMap=class(TpvStringHashMap<TpvResource>);
      private
       fResourceManager:TpvResourceManager;
       fResourceClass:TpvResourceClass;
       fResourceList:TResourceList;
       fResourceFileNameMap:TResourceStringMap;
       fMemoryBudget:TpvSizeInt;
       fMemoryUsage:TpvSizeInt;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aResourceClass:TpvResourceClass); reintroduce;
       destructor Destroy; override;
       procedure Shutdown;
      published
       property MemoryBudget:TpvSizeInt read fMemoryBudget write fMemoryBudget;
       property MemoryUsage:TpvSizeInt read fMemoryUsage;
     end;

     { TpvResourceManager }

     TpvResourceManager=class
      private
       type TResourceClassTypeList=class(TpvObjectGenericList<TpvResourceClassType>);
            TResourceClassTypeMap=class(TpvHashMap<TpvResourceClass,TpvResourceClassType>);
            TResourceList=class(TpvObjectGenericList<TpvResource>);
      private
       fLock:TPasMPMultipleReaderSingleWriterLock;
       fLocked:TPasMPBool32;
       fActive:TPasMPBool32;
       fResourceClassTypeList:TResourceClassTypeList;
       fResourceClassTypeListLock:TPasMPMultipleReaderSingleWriterLock;
       fResourceClassTypeMap:TResourceClassTypeMap;
       fDelayedToFreeResources:TResourceList;
       fBackgroundLoader:TpvResourceBackgroundLoader;
       fBaseDataPath:TpvUTF8String;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Shutdown;
       class function SanitizeFileName(aFileName:TpvUTF8String):TpvUTF8String; static;
       function GetResourceClassType(const aResourceClass:TpvResourceClass):TpvResourceClassType;
       function FindResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String):TpvResource;
       function LoadResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil;const aLoadInBackground:boolean=false;const aParent:TpvResource=nil):TpvResource;
       function GetResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil):TpvResource;
       function BackgroundLoadResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil;const aParent:TpvResource=nil):TpvResource;
       function FinishResources(const aTimeout:TpvInt64=5):boolean;
       function WaitForResources(const aTimeout:TpvInt64=-1):boolean;
       property ResourceClassTypes[const aResourceClass:TpvResourceClass]:TpvResourceClassType read GetResourceClassType;
       property Resources[const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String]:TpvResource read FindResource;
       property BaseDataPath:TpvUTF8String read fBaseDataPath write fBaseDataPath;
     end;

var AllowExternalResources:boolean=false;

implementation

uses PasVulkan.Application;

{ TpvResource }

constructor TpvResource.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
var OldReferenceCounter:TpvInt32;
begin
 inherited Create;

 fResourceManager:=aResourceManager;

 fParent:=aParent;

 fFileName:='';

 fAsyncLoadState:=TAsyncLoadState.None;

 fLoaded:=false;

 fIsOnDelayedToFreeResourcesList:=false;

 fMemoryUsage:=0;

 fMetaData:=nil;

 fOnFinish:=nil;

 fReleaseFrameDelay:=0;

 OldReferenceCounter:=fReferenceCounter;
 try
  fInstanceInterface:=self;
 finally
  fReferenceCounter:=OldReferenceCounter;
 end;

 fResourceClassType:=fResourceManager.GetResourceClassType(TpvResourceClass(ClassType));

 if assigned(fResourceManager) and assigned(fResourceClassType) then begin
  if not fResourceManager.fLocked then begin
   fResourceManager.fLock.AcquireWrite;
  end;
  try
   fResourceClassType.fResourceList.Add(self);
  finally
   if not fResourceManager.fLocked then begin
    fResourceManager.fLock.ReleaseWrite;
   end;
  end;
 end;

end;

destructor TpvResource.Destroy;
var Index:TpvSizeInt;
begin

 if assigned(fResourceManager) and
    (assigned(fResourceClassType) or
     (fIsOnDelayedToFreeResourcesList and assigned(fResourceManager.fDelayedToFreeResources))) then begin

  fResourceManager.fLock.AcquireWrite;
  try

   if assigned(fResourceClassType) then begin
    fResourceClassType.fResourceList.Remove(self);
   end;

   if fIsOnDelayedToFreeResourcesList and assigned(fResourceManager.fDelayedToFreeResources) then begin
    try
     Index:=fResourceManager.fDelayedToFreeResources.IndexOf(self);
     if Index>=0 then begin
      fResourceManager.fDelayedToFreeResources.Extract(Index);
     end;
    finally
     fIsOnDelayedToFreeResourcesList:=false;
    end;
   end;

  finally
   fResourceManager.fLock.ReleaseWrite;
  end;

 end;

 SetFileName('');

 FreeAndNil(fMetaData);

 FillChar(fInstanceInterface,SizeOf(IpvResource),0);

 inherited Destroy;
end;

procedure TpvResource.PrepareDeferredFree;
begin
end;

procedure TpvResource.DeferredFree;
begin
 if assigned(self) then begin
  if (fReleaseFrameDelay>0) and
     assigned(fResourceManager) and
     fResourceManager.fActive and
     assigned(fResourceManager.fDelayedToFreeResources) and not fIsOnDelayedToFreeResourcesList then begin
   try
    PrepareDeferredFree;
   finally
    try
     fResourceManager.fDelayedToFreeResources.Add(self);
    finally
     fIsOnDelayedToFreeResourcesList:=true;
    end;
   end;
  end else begin
   Destroy;
  end;
 end;
end;

procedure TpvResource.SetFileName(const aFileName:TpvUTF8String);
var NewFileName:TpvUTF8String;
    OldReferenceCounter:TpvInt32;
begin
 NewFileName:=TpvResourceManager.SanitizeFileName(aFileName);
 if fFileName<>NewFileName then begin
  if assigned(fResourceManager) and not fResourceManager.fLocked then begin
   fResourceManager.fLock.AcquireWrite;
  end;
  try
   OldReferenceCounter:=fReferenceCounter;
   try
    inc(fReferenceCounter,2); // For to avoid false-positive frees in this situation
    if assigned(fResourceClassType) and (length(fFileName)>0) then begin
     fResourceClassType.fResourceFileNameMap.Delete(TpvUTF8String(LowerCase(String(fFileName))));
    end;
    fFileName:=NewFileName;
    if assigned(fResourceClassType) and (length(fFileName)>0) then begin
     fResourceClassType.fResourceFileNameMap.Add(TpvUTF8String(LowerCase(String(fFileName))),self);
    end;
   finally
    fReferenceCounter:=OldReferenceCounter;
   end;
  finally
   if assigned(fResourceManager) and not fResourceManager.fLocked then begin
    fResourceManager.fLock.ReleaseWrite;
   end;
  end;
 end;
end;

procedure TpvResource.AfterConstruction;
begin
 inherited AfterConstruction;
end;

procedure TpvResource.BeforeDestruction;
begin
 inherited BeforeDestruction;
end;

function TpvResource._AddRef:TpvInt32;
begin
 result:=inherited _AddRef;
end;

function TpvResource._Release:TpvInt32;
begin
 if (fReleaseFrameDelay>0) and assigned(fResourceManager) and fResourceManager.fActive and assigned(fResourceManager.fDelayedToFreeResources) and not fIsOnDelayedToFreeResourcesList then begin
  result:=TPasMPInterlocked.Decrement(fReferenceCounter);
  if result=0 then begin
   try
    PrepareDeferredFree;
   finally
    try
     fResourceManager.fDelayedToFreeResources.Add(self);
    finally
     fIsOnDelayedToFreeResourcesList:=true;
    end;
   end;
  end;
 end else begin
  result:=inherited _Release;
 end;
end;

function TpvResource.GetResource:TpvResource;
begin
 result:=self;
end;

function TpvResource.GetResourceClass:TpvResourceClass;
begin
 result:=TpvResourceClass(ClassType);
end;

function TpvResource.WaitFor:boolean;
begin
 result:=fLoaded;
 if (not result) and
    assigned(fResourceManager) and
    assigned(fResourceManager.fBackgroundLoader) then begin
  fResourceManager.fBackgroundLoader.WaitForResource(self);
  result:=fLoaded;
 end;
end;

function TpvResource.CreateNewFileStreamFromFileName(const aFileName:TpvUTF8String):TStream;
begin
 result:=TFileStream.Create(IncludeTrailingPathDelimiter(String(fResourceManager.fBaseDataPath))+String(TpvResourceManager.SanitizeFileName(aFileName)),fmCreate);
end;

function TpvResource.GetStreamFromFileName(const aFileName:TpvUTF8String):TStream;
var SanitizedFileName:TpvUTF8String;
begin
 SanitizedFileName:=TpvResourceManager.SanitizeFileName(aFileName);
 if pvApplication.Assets.ExistAsset(String(SanitizedFileName)) then begin
  result:=pvApplication.Assets.GetAssetStream(String(SanitizedFileName));
 end else begin
  if FileExists(String(SanitizedFileName)) then begin
   result:=TFileStream.Create(String(SanitizedFileName),fmOpenRead);
  end else begin
   result:=nil;
  end;
 end;
end;

function TpvResource.LoadMetaData(const aStream:TStream):boolean;
begin
 FreeAndNil(fMetaData);
 if assigned(aStream) and (aStream.Size>0) then begin
  fMetaData:=TPasJSON.Parse(aStream);
  result:=assigned(fMetaData);
 end else begin
  result:=false;
 end;
end;

function TpvResource.SaveMetaData(const aStream:TStream):boolean;
var Data:TpvRawByteString;
begin
 if assigned(aStream) and assigned(fMetaData) then begin
  Data:=TPasJSON.Stringify(fMetaData);
  if length(Data)>0 then begin
   aStream.WriteBuffer(Data[1],length(Data));
   result:=true;
  end else begin
   result:=false;
  end;
 end else begin
  result:=false;
 end;
end;

function TpvResource.LoadMetaData:boolean;
var MetaFileName:TpvUTF8String;
    Stream:TStream;
begin
 MetaFileName:=TpvResourceManager.SanitizeFileName(TpvUTF8String(ChangeFileExt(String(fFileName),'.meta')));
 Stream:=GetStreamFromFileName(MetaFileName);
 if assigned(Stream) then begin
  try
   result:=LoadMetaData(Stream);
  finally
   FreeAndNil(Stream);
  end;
 end else begin
  result:=false;
 end;
end;

function TpvResource.SaveMetaData:boolean;
var MetaFileName:TpvUTF8String;
    Stream:TStream;
begin
 MetaFileName:=TpvResourceManager.SanitizeFileName(TpvUTF8String(ChangeFileExt(String(fFileName),'.meta')));
 if assigned(fMetaData) then begin
  Stream:=CreateNewFileStreamFromFileName(MetaFileName);
  try
   result:=SaveMetaData(Stream);
  finally
   FreeAndNil(Stream);
  end;
 end else begin
  result:=false;
 end;
end;

function TpvResource.BeginLoad(const aStream:TStream):boolean;
begin
 result:=false;
end;

function TpvResource.EndLoad:boolean;
begin
 result:=true;
end;

function TpvResource.Load(const aStream:TStream):boolean;
begin
 result:=fLoaded;
 if not result then begin
  if GetCurrentThreadID=MainThreadID then begin
   fAsyncLoadState:=TAsyncLoadState.Done;
  end else begin
   fAsyncLoadState:=TAsyncLoadState.Loading;
  end;
  LoadMetaData;
  result:=BeginLoad(aStream);
  if result then begin
   result:=EndLoad;
   fAsyncLoadState:=TAsyncLoadState.Done;
   if result then begin
    fLoaded:=true;
   end;
  end;
 end;
end;

function TpvResource.Save:boolean;
begin
 result:=false;
end;

function TpvResource.LoadFromFileName(const aFileName:TpvUTF8String):boolean;
var SanitizedFileName:TpvUTF8String;
    Stream:TStream;
begin
 SanitizedFileName:=TpvResourceManager.SanitizeFileName(aFileName);
 Stream:=GetStreamFromFileName(SanitizedFileName);
 if assigned(Stream) then begin
  try
   result:=Load(Stream);
   if result then begin
    SetFileName(SanitizedFileName);
   end;
  finally
   FreeAndNil(Stream);
  end;
 end else begin
  result:=false;
 end;
end;

function TpvResource.SaveToFileName(const aFileName:TpvUTF8String):boolean;
begin
 SetFileName(TpvResourceManager.SanitizeFileName(aFileName));
 result:=Save;
end;

{ TpvResourceBackgroundLoader.TQueueItem }

constructor TpvResourceBackgroundLoader.TQueueItem.Create(const aResourceBackgroundLoader:TpvResourceBackgroundLoader;const aResource:TpvResource);
begin
 inherited Create;
 fResourceBackgroundLoader:=aResourceBackgroundLoader;
 fResource:=aResource;
 fDependencies.Initialize;
 fDependents.Initialize;
 fResourceBackgroundLoader.fQueueItems.Add(self);
 if assigned(fResourceBackgroundLoader) and assigned(fResource) then begin
  fResourceBackgroundLoader.fQueueItemResourceMap.Add(fResource.GetResource,self);
 end;
end;

destructor TpvResourceBackgroundLoader.TQueueItem.Destroy;
var Index:TpvSizeInt;
begin
 if assigned(fResourceBackgroundLoader) then begin
  try
   if assigned(fResource) then begin
    fResourceBackgroundLoader.fQueueItemResourceMap.Delete(fResource.GetResource);
   end;
  finally
   for Index:=0 to fResourceBackgroundLoader.fQueueItems.Count-1 do begin
    if fResourceBackgroundLoader.fQueueItems.Items[Index]=self then begin
     fResourceBackgroundLoader.fQueueItems.Delete(Index);
     break;
    end;
   end;
  end;
 end;
 fDependencies.Finalize;
 fDependents.Finalize;
 fResource:=nil;
 inherited Destroy;
end;

{ TpvResourceBackgroundLoader }

constructor TpvResourceBackgroundLoader.Create(const aResourceManager:TpvResourceManager);
begin
 fResourceManager:=aResourceManager;
 fEvent:=TPasMPEvent.Create(nil,false,false,'');
 fLock:=TPasMPSlimReaderWriterLock.Create;
 fQueueItems.Initialize;
 fQueueItemResourceMap:=TQueueItemResourceMap.Create(nil);
 inherited Create(false);
end;

destructor TpvResourceBackgroundLoader.Destroy;
begin
 while fQueueItems.Count>0 do begin
  fQueueItems.Items[0].Free;
 end;
 fQueueItems.Finalize;
 FreeAndNil(fQueueItemResourceMap);
 FreeAndNil(fEvent);
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvResourceBackgroundLoader.Execute;
var Index,
    OtherIndex,
    OtherOtherIndex,
    OtherOtherOtherIndex:TpvSizeInt;
    QueueItem,
    TemporaryQueueItem:TQueueItem;
    Resource:TpvResource;
    Stream:TStream;
    Success,
    DoWait:boolean;
begin

 DoWait:=true;

 while not Terminated do begin

  if DoWait then begin
   fEvent.WaitFor(60000);
  end else begin
   TPasMP.Relax;
  end;

  if Terminated then begin

   break;

  end else begin

   DoWait:=true;

   fLock.Acquire;
   try

    QueueItem:=nil;
    for Index:=0 to fQueueItems.Count-1 do begin
     TemporaryQueueItem:=fQueueItems.Items[Index];
     if (TemporaryQueueItem.fResource.GetResource.fAsyncLoadState=TpvResource.TAsyncLoadState.Queued) and
        (TemporaryQueueItem.fDependencies.Count=0) then begin
      QueueItem:=TemporaryQueueItem;
      break;
     end;
    end;

    if assigned(QueueItem) then begin

     Success:=false;

     Resource:=QueueItem.fResource.GetResource;
     Resource.fAsyncLoadState:=TpvResource.TAsyncLoadState.Loading;

     fLock.Release;
     try

      Stream:=Resource.GetStreamFromFileName(Resource.fFileName);

      if assigned(Stream) then begin

       try
        try
         Resource.LoadMetaData;
        finally
         Success:=Resource.BeginLoad(Stream);
        end;
       finally
        FreeAndNil(Stream);
       end;

      end;

     finally
      fLock.Acquire;
     end;

     if QueueItem.fDependents.Count>0 then begin
      try
       for OtherIndex:=0 to QueueItem.fDependents.Count-1 do begin
        for OtherOtherIndex:=0 to fQueueItems.Count-1 do begin
         TemporaryQueueItem:=fQueueItems.Items[OtherOtherIndex];
         for OtherOtherOtherIndex:=0 to TemporaryQueueItem.fDependencies.Count-1 do begin
          if TemporaryQueueItem.fDependencies.Items[OtherOtherOtherIndex]=QueueItem.fDependents.Items[OtherIndex] then begin
           TemporaryQueueItem.fDependencies.Delete(OtherOtherOtherIndex);
           break;
          end;
         end;
        end;
       end;
      finally
       QueueItem.fDependents.Clear;
      end;
     end;

     if Success then begin
      Resource.fAsyncLoadState:=TpvResource.TAsyncLoadState.Success;
     end else begin
      Resource.fAsyncLoadState:=TpvResource.TAsyncLoadState.Fail;
     end;

     DoWait:=false;

    end;

   finally
    fLock.Release;
   end;

  end;
 end;
end;

function TpvResourceBackgroundLoader.QueueResource(const aResource:TpvResource;const aParent:TpvResource):boolean;
var Index:TpvSizeInt;
    QueueItem,
    TemporaryQueueItem:TQueueItem;
    Resource:TpvResource;
    Found:boolean;
begin

 result:=false;

 fLock.Acquire;
 try

  Resource:=aResource.GetResource;

  QueueItem:=fQueueItemResourceMap.Values[Resource];

  if not assigned(QueueItem) then begin

   Resource.fAsyncLoadState:=TpvResource.TAsyncLoadState.Queued;

   QueueItem:=TQueueItem.Create(self,aResource);

   if assigned(aParent) then begin

    TemporaryQueueItem:=fQueueItemResourceMap.Values[aParent.GetResource];

    if assigned(TemporaryQueueItem) then begin

     Found:=false;
     for Index:=0 to QueueItem.fDependents.Count-1 do begin
      if QueueItem.fDependents.Items[Index]=aParent then begin
       Found:=true;
       break;
      end;
     end;
     if not Found then begin
      QueueItem.fDependents.Add(aParent);
     end;

     Found:=false;
     for Index:=0 to TemporaryQueueItem.fDependencies.Count-1 do begin
      if TemporaryQueueItem.fDependencies.Items[Index]=aResource then begin
       Found:=true;
       break;
      end;
     end;
     if not Found then begin
      TemporaryQueueItem.fDependencies.Add(aResource);
     end;

    end;

   end;

   result:=true;

  end;

 finally
  fLock.Release;
 end;

 if result then begin
  fEvent.SetEvent;
 end;

end;

procedure TpvResourceBackgroundLoader.FinalizeQueueItem(const aQueueItem:TQueueItem);
var Resource:TpvResource;
    Success:boolean;
begin

 fLock.Acquire;
 try

  Resource:=aQueueItem.fResource;

  Success:=Resource.fAsyncLoadState=TpvResource.TAsyncLoadState.Success;

  if Success then begin
   fLock.Release;
   try
    Success:=Resource.EndLoad;
   finally
    fLock.Acquire;
   end;
  end;

  Resource.fAsyncLoadState:=TpvResource.TAsyncLoadState.Done;

  if Success then begin
   Resource.fLoaded:=true;
  end;

 finally
  fLock.Release;
 end;

 if assigned(Resource.fOnFinish) then begin
  Resource.fOnFinish(Resource,Success);
 end;

end;

procedure TpvResourceBackgroundLoader.WaitForResource(const aResource:TpvResource);
var QueueItem:TQueueItem;
begin

 fLock.Acquire;
 try

  QueueItem:=fQueueItemResourceMap.Values[aResource];
  if assigned(QueueItem) then begin

   while (QueueItem.fDependencies.Count>0) or
         (aResource.fAsyncLoadState in [TpvResource.TAsyncLoadState.Queued,
                                        TpvResource.TAsyncLoadState.Loading]) do begin
    fLock.Release;
    try
     TPasMP.Yield;
     Sleep(1);
    finally
     fLock.Acquire;
    end;
   end;

   fLock.Release;
   try
    FinalizeQueueItem(QueueItem);
   finally
    fLock.Acquire;
   end;

   FreeAndNil(QueueItem);

  end;

 finally
  fLock.Release;
 end;

end;

function TpvResourceBackgroundLoader.ProcessIteration(const aStartTime:TpvHighResolutionTime;const aTimeout:TpvInt64):boolean;
var Index:TpvSizeInt;
    QueueItem:TQueueItem;
    Resource:TpvResource;
begin

 result:=true;

 Index:=0;
 while Index<fQueueItems.Count do begin

  QueueItem:=fQueueItems.Items[Index];

  Resource:=QueueItem.fResource;
  try

   if (QueueItem.fDependencies.Count>0) or
      (Resource.fAsyncLoadState in [TpvResource.TAsyncLoadState.Queued,
                                    TpvResource.TAsyncLoadState.Loading]) then begin

    inc(Index);

   end else begin

    fLock.Release;
    try
     FinalizeQueueItem(QueueItem);
    finally
     fLock.Acquire;
    end;

    FreeAndNil(QueueItem);

   end;

  finally
  end;

  if (aTimeout>=0) and
     (pvApplication.HighResolutionTimer.ToMilliseconds(pvApplication.HighResolutionTimer.GetTime-aStartTime)>=aTimeout) then begin
   result:=false;
   break;
  end;

 end;

end;

function TpvResourceBackgroundLoader.Process(const aTimeout:TpvInt64=5):boolean;
begin
 fLock.Acquire;
 try
  ProcessIteration(pvApplication.HighResolutionTimer.GetTime,aTimeOut);
  result:=fQueueItems.Count=0;
 finally
  fLock.Release;
 end;
end;

function TpvResourceBackgroundLoader.WaitForResources(const aTimeout:TpvInt64=-1):boolean;
var Start:TpvHighResolutionTime;
begin
 Start:=pvApplication.HighResolutionTimer.GetTime;
 fLock.Acquire;
 try
  while (fQueueItems.Count>0) and ProcessIteration(Start,aTimeout) do begin
   TPasMP.Relax;
  end;
  result:=fQueueItems.Count=0;
 finally
  fLock.Release;
 end;
end;

function TpvResourceBackgroundLoader.GetCountOfQueuedResources:TpvSizeInt;
begin
 fLock.Acquire;
 try
  result:=fQueueItems.Count;
 finally
  fLock.Release;
 end;
end;

{ TpvResourceClassType }

constructor TpvResourceClassType.Create(const aResourceManager:TpvResourceManager;const aResourceClass:TpvResourceClass);
begin

 inherited Create;

 fResourceManager:=aResourceManager;

 fResourceClass:=aResourceClass;

 fResourceList:=TResourceList.Create;
 fResourceList.OwnsObjects:=false;

 fResourceFileNameMap:=TResourceStringMap.Create(nil);

 fMemoryBudget:=0;

 fMemoryUsage:=0;

end;

destructor TpvResourceClassType.Destroy;
begin

 Shutdown;

 fResourceList.Free;

 fResourceFileNameMap.Free;

 inherited Destroy;
end;

procedure TpvResourceClassType.Shutdown;
var Resource:TpvResource;
begin
 while fResourceList.Count>0 do begin
  Resource:=fResourceList.Items[fResourceList.Count-1];
  fResourceList.Items[fResourceList.Count-1]:=nil;
  fResourceList.Delete(fResourceList.Count-1);
  Resource.Free;
 end;
end;

{ TpvResourceManager }

constructor TpvResourceManager.Create;
begin
 inherited Create;

 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fLocked:=false;

 fResourceClassTypeList:=TResourceClassTypeList.Create;
 fResourceClassTypeList.OwnsObjects:=true;

 fResourceClassTypeListLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 fResourceClassTypeMap:=TResourceClassTypeMap.Create(nil);

 if assigned(pvApplication) then begin
  fBaseDataPath:=TpvUTF8String(pvApplication.Assets.BasePath);
 end;

 fDelayedToFreeResources:=TResourceList.Create;
 fDelayedToFreeResources.OwnsObjects:=true;

 fBackgroundLoader:=TpvResourceBackgroundLoader.Create(self);

 fActive:=true;

end;

destructor TpvResourceManager.Destroy;
begin

 Shutdown;

 FreeAndNil(fBackgroundLoader);

 FreeAndNil(fDelayedToFreeResources);

 FreeAndNil(fResourceClassTypeList);

 FreeAndNil(fResourceClassTypeListLock);

 FreeAndNil(fResourceClassTypeMap);

 FreeAndNil(fLock);

 inherited Destroy;

end;

procedure TpvResourceManager.Shutdown;
var Index:TpvSizeInt;
    ResourceClassType:TpvResourceClassType;
begin

 if fActive then begin

  fActive:=false;

  if not fBackgroundLoader.Finished then begin
   fBackgroundLoader.Terminate;
   fBackgroundLoader.fEvent.SetEvent;
   fBackgroundLoader.WaitFor;
  end;

  while fDelayedToFreeResources.Count>0 do begin
   fDelayedToFreeResources.Delete(fDelayedToFreeResources.Count-1);
  end;
  FreeAndNil(fDelayedToFreeResources);

  Index:=0;
  while Index<fResourceClassTypeList.Count do begin
   ResourceClassType:=fResourceClassTypeList[Index];
   ResourceClassType.Shutdown;
   inc(Index);
  end;

  fResourceClassTypeList.Clear;

  fResourceClassTypeMap.Clear;

 end;

end;

class function TpvResourceManager.SanitizeFileName(aFileName:TpvUTF8String):TpvUTF8String;
var Index,LastDirectoryNameBeginIndex,Len:TpvSizeInt;
    Temporary:TpvUTF8String;
begin

 result:=aFileName;

 if AllowExternalResources then begin
  if FileExists(result) then begin
   result:=ExpandFileName(result);
   exit;
  end else begin
   for Index:=1 to length(result) do begin
    if result[Index] in ['\','/'] then begin
     Temporary:=ExpandFileName(result);
     if FileExists(Temporary) then begin
      result:=Temporary;
      exit;
     end;
    end;
   end;
  end;
 end;

 Index:=1;
 LastDirectoryNameBeginIndex:=1;
 Len:=length(result);
 while Index<=Len do begin
  case result[Index] of
   'A'..'Z':begin
    inc(result[Index],Ord('a')-Ord('A'));
    inc(Index);
   end;
   '\','/':begin
    if (LastDirectoryNameBeginIndex<Index) and
       ((result[LastDirectoryNameBeginIndex]='.') and
        (((Index-LastDirectoryNameBeginIndex)=1) or
         (((Index-LastDirectoryNameBeginIndex)=2) and
          (result[LastDirectoryNameBeginIndex+1]='.')))) then begin
     // Remove "./" and "../" from string
     Delete(result,LastDirectoryNameBeginIndex,(Index-LastDirectoryNameBeginIndex)+1);
     dec(Len,(Index-LastDirectoryNameBeginIndex)+1);
    end else if Index=1 then begin
     // Remove beginning "/" from string
     Delete(result,1,1);
     dec(Len);
    end else begin
     if result[Index]='\' then begin
      result[Index]:='/';
     end;
     inc(Index);
     LastDirectoryNameBeginIndex:=Index;
    end;
   end;
   else begin
    inc(Index);
   end;
  end;
 end;

end;

function TpvResourceManager.GetResourceClassType(const aResourceClass:TpvResourceClass):TpvResourceClassType;
begin
 if not assigned(aResourceClass) then begin
  raise EpvResourceClassNull.Create('Resource class is null');
 end;
 fResourceClassTypeListLock.AcquireRead;
 try
  result:=fResourceClassTypeMap[aResourceClass];
  if not assigned(result) then begin
   fResourceClassTypeListLock.ReadToWrite;
   try
    result:=TpvResourceClassType.Create(self,aResourceClass);
    try
     fResourceClassTypeMap[aResourceClass]:=result;
    finally
     fResourceClassTypeList.Add(result);
    end;
   finally
    fResourceClassTypeListLock.WriteToRead;
   end;
  end;
 finally
  fResourceClassTypeListLock.ReleaseRead;
 end;
end;

function TpvResourceManager.FindResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String):TpvResource;
var ResourceClassType:TpvResourceClassType;
begin
 ResourceClassType:=GetResourceClassType(aResourceClass);
 fLock.AcquireRead;
 try
  result:=ResourceClassType.fResourceFileNameMap[SanitizeFileName(aFileName)];
 finally
  fLock.ReleaseRead;
 end;
end;

function TpvResourceManager.LoadResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil;const aLoadInBackground:boolean=false;const aParent:TpvResource=nil):TpvResource;
var ResourceClassType:TpvResourceClassType;
    Resource:TpvResource;
    FileName:TpvUTF8String;
begin
 FileName:=SanitizeFileName(aFileName);
 ResourceClassType:=GetResourceClassType(aResourceClass);
 fLock.AcquireRead;
 try
  result:=ResourceClassType.fResourceFileNameMap[FileName];
  if assigned(result) then begin
   Resource:=result.GetResource;
   if not (Resource is aResourceClass) then begin
    raise EpvResourceClassMismatch.Create('Resource class mismatch');
   end;
   if aLoadInBackground then begin
    if not Resource.fLoaded then begin
     fLock.ReadToWrite;
     try
      fBackgroundLoader.QueueResource(result,aParent);
     finally
      fLock.WriteToRead;
     end;
    end;
   end else begin
    fLock.ReleaseRead;
    try
     fBackgroundLoader.WaitForResource(Resource);
    finally
     fLock.AcquireRead;
    end;
   end;
  end else begin
   fLock.ReadToWrite;
   try
    fLocked:=true;
    try
     Resource:=aResourceClass.Create(self,aParent);
     Resource.SetFileName(FileName);
     Resource.fOnFinish:=aOnFinish;
    finally
     fLocked:=false;
    end;
    if aLoadInBackground then begin
     result:=Resource;
     fBackgroundLoader.QueueResource(result,aParent);
    end;
   finally
    fLock.WriteToRead;
   end;
   if not aLoadInBackground then begin
    fLock.ReleaseRead;
    try
     if Resource.LoadFromFileName(FileName) then begin
      result:=Resource;
     end else begin
      FreeAndNil(Resource);
     end;
    finally
     fLock.AcquireRead;
    end;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

function TpvResourceManager.GetResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil):TpvResource;
begin
 result:=LoadResource(aResourceClass,aFileName,aOnFinish,false,nil);
end;

function TpvResourceManager.BackgroundLoadResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil;const aParent:TpvResource=nil):TpvResource;
begin
 result:=LoadResource(aResourceClass,aFileName,aOnFinish,true,aParent);
end;

function TpvResourceManager.FinishResources(const aTimeout:TpvInt64=5):boolean;
var Index:TpvSizeInt;
begin
 for Index:=fDelayedToFreeResources.Count-1 downto 0 do begin
  if TPasMPInterlocked.Decrement(fDelayedToFreeResources[Index].fReleaseFrameDelay)=0 then begin
   fDelayedToFreeResources.Delete(Index);
  end;
 end;
 result:=fBackgroundLoader.Process(aTimeout);
end;

function TpvResourceManager.WaitForResources(const aTimeout:TpvInt64=-1):boolean;
begin
 result:=fBackgroundLoader.WaitForResources(aTimeout);
end;

initialization
end.

