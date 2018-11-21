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

     EpvResourceClassMismatch=class(EpvResource);

     TpvResourceManager=class;

     TpvResource=class;

     TpvResourceClass=class of TpvResource;

     IpvResource=interface(IpvReferenceCountedObject)['{AD2C0315-C8AF-4D79-876E-1FA42FB869F9}']
      function GetResource:TpvResource;
      function GetResourceClass:TpvResourceClass;
     end;

     TpvResourceOnFinish=procedure(aResource:IpvResource;const aSuccess:boolean) of object;

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
      private
       fResourceManager:TpvResourceManager;
       fFileName:TpvUTF8String;
       fAsyncLoadState:TAsyncLoadState;
       fLoaded:boolean;
       fMemoryUsage:TpvUInt64;
       fMetaData:TPasJSONItem;
       fInstanceInterface:IpvResource;
       fOnFinish:TpvResourceOnFinish;
       procedure SetFileName(const aFileName:TpvUTF8String);
      protected
       function _AddRef:TpvInt32; override; {$ifdef Windows}stdcall{$else}cdecl{$endif};
       function _Release:TpvInt32; override; {$ifdef Windows}stdcall{$else}cdecl{$endif};
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aFileName:TpvUTF8String='';const aOnFinish:TpvResourceOnFinish=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       function GetResource:TpvResource;
       function GetResourceClass:TpvResourceClass;
       function CreateNewFileStreamFromFileName(const aFileName:TpvUTF8String):TStream; virtual;
       function GetStreamFromFileName(const aFileName:TpvUTF8String):TStream; virtual;
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
       property FileName:TpvUTF8String read fFileName write SetFileName;
       property AsyncLoadState:TAsyncLoadState read fAsyncLoadState write fAsyncLoadState;
       property Loaded:boolean read fLoaded write fLoaded;
       property MetaData:TPasJSONItem read fMetaData write fMetaData;
       property OnFinish:TpvResourceOnFinish read fOnFinish write fOnFinish;
     end;

     TpvResourceBackgroundLoader=class(TPasMPThread)
      public
       type TQueueItem=class
             private
              fResource:IpvResource;
              fDependencies:TpvDynamicArray<IpvResource>;
              fDependents:TpvDynamicArray<IpvResource>;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
            end;
           TQueueItems=TpvDynamicArray<TQueueItem>;
           TQueueItemStringMap=class(TpvStringHashMap<TQueueItem>);
      private
       fResourceManager:TpvResourceManager;
       fEvent:TPasMPEvent;
       fLock:TPasMPSlimReaderWriterLock;
       fQueueItems:TQueueItems;
       fQueueItemFileNameMap:TQueueItemStringMap;
       procedure FinishBackgroundLoading(const aQueueItem:TQueueItem);
      protected
       procedure Execute; override;
      public
       constructor Create(const aResourceManager:TpvResourceManager); reintroduce;
       destructor Destroy; override;
       function QueueResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aParent:IpvResource=nil;const aOnFinish:TpvResourceOnFinish=nil):boolean;
       procedure WaitForResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String);
       procedure FinishResources(const aTimeout:TpvInt64=5);
       function GetCountOfQueuedResources:TpvSizeInt;
     end;

     TpvResourceManager=class
      private
       type TResourceList=class(TpvObjectGenericList<TpvResource>);
            TResourceStringMap=class(TpvStringHashMap<IpvResource>);
      private
       fResourceLock:TPasMPMultipleReaderSingleWriterLock;
       fResourceList:TResourceList;
       fResourceFileNameMap:TResourceStringMap;
       fBackgroundLoader:TpvResourceBackgroundLoader;
       fBaseDataPath:TpvUTF8String;
       function GetResourceByFileName(const aFileName:TpvUTF8String):IpvResource; inline;
      public
       constructor Create;
       destructor Destroy; override;
       class function SanitizeFileName(const aFileName:TpvUTF8String):TpvUTF8String; static;
       function GetResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil):IpvResource;
       function BackgroundLoadResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aParent:IpvResource=nil;const aOnFinish:TpvResourceOnFinish=nil):boolean;
       property ResourceByFileName[const aFileName:TpvUTF8String]:IpvResource read GetResourceByFileName;
       property BaseDataPath:TpvUTF8String read fBaseDataPath write fBaseDataPath;
       property BackgroundLoader:TpvResourceBackgroundLoader read fBackgroundLoader;
     end;

implementation

uses PasVulkan.Application;

{ TpvResource }

constructor TpvResource.Create(const aResourceManager:TpvResourceManager;const aFileName:TpvUTF8String='';const aOnFinish:TpvResourceOnFinish=nil);
var OldReferenceCounter:TpvInt32;
begin
 inherited Create;
 fResourceManager:=aResourceManager;
 fFileName:='';
 fAsyncLoadState:=TAsyncLoadState.None;
 fLoaded:=false;
 fMemoryUsage:=0;
 fMetaData:=nil;
 fOnFinish:=aOnFinish;
 OldReferenceCounter:=fReferenceCounter;
 try
  fInstanceInterface:=self;
 finally
  fReferenceCounter:=OldReferenceCounter;
 end;
 if assigned(fResourceManager) then begin
  fResourceManager.fResourceLock.AcquireWrite;
  try
   OldReferenceCounter:=fReferenceCounter;
   try
    fResourceManager.fResourceList.Add(self);
   finally
    fReferenceCounter:=OldReferenceCounter;
   end;
  finally
   fResourceManager.fResourceLock.ReleaseWrite;
  end;
 end;
 SetFileName(TpvResourceManager.SanitizeFileName(aFileName));
end;

destructor TpvResource.Destroy;
var OldReferenceCounter:TpvInt32;
begin
 if assigned(fResourceManager) then begin
  fResourceManager.fResourceLock.AcquireWrite;
  try
   OldReferenceCounter:=fReferenceCounter;
   try
    fResourceManager.fResourceList.Remove(self);
   finally
    fReferenceCounter:=OldReferenceCounter;
   end;
  finally
   fResourceManager.fResourceLock.ReleaseWrite;
  end;
 end;
 SetFileName('');
 FreeAndNil(fMetaData);
 inherited Destroy;
end;

procedure TpvResource.SetFileName(const aFileName:TpvUTF8String);
var NewFileName:TpvUTF8String;
    OldReferenceCounter:TpvInt32;
begin
 NewFileName:=TpvResourceManager.SanitizeFileName(aFileName);
 if fFileName<>NewFileName then begin
  if assigned(fResourceManager) then begin
   fResourceManager.fResourceLock.AcquireWrite;
  end;
  try
   OldReferenceCounter:=fReferenceCounter;
   try
    if assigned(fResourceManager) and (length(fFileName)>0) then begin
     fResourceManager.fResourceFileNameMap.Delete(TpvUTF8String(LowerCase(String(fFileName))));
    end;
    fFileName:=NewFileName;
    if assigned(fResourceManager) and (length(fFileName)>0) then begin
     fResourceManager.fResourceFileNameMap.Add(TpvUTF8String(LowerCase(String(fFileName))),fInstanceInterface);
    end;
   finally
    fReferenceCounter:=OldReferenceCounter;
   end;
  finally
   if assigned(fResourceManager) then begin
    fResourceManager.fResourceLock.ReleaseWrite;
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
 result:=inherited _Release;
end;

function TpvResource.GetResource:TpvResource;
begin
 result:=self;
end;

function TpvResource.GetResourceClass:TpvResourceClass;
begin
 result:=TpvResourceClass(ClassType);
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
  result:=nil;
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
  result:=BeginLoad(aStream);
  if result then begin
   result:=EndLoad;
   fAsyncLoadState:=TAsyncLoadState.Done;
   fLoaded:=true;
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

constructor TpvResourceBackgroundLoader.TQueueItem.Create;
begin
 inherited Create;
 fResource:=nil;
 fDependencies.Initialize;
 fDependents.Initialize;
end;

destructor TpvResourceBackgroundLoader.TQueueItem.Destroy;
begin
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
 fQueueItemFileNameMap:=TQueueItemStringMap.Create(nil);
 inherited Create(false);
end;

destructor TpvResourceBackgroundLoader.Destroy;
var Index:TpvSizeInt;
begin
 for Index:=0 to fQueueItems.Count-1 do begin
  FreeAndNil(fQueueItems.Items[Index]);
 end;
 fQueueItems.Finalize;
 FreeAndNil(fQueueItemFileNameMap);
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
    HasMoreItems:boolean;
begin
 HasMoreItems:=false;
 while not Terminated do begin

  if HasMoreItems then begin
   HasMoreItems:=false;
   TPasMP.Yield;
  end else begin
   fEvent.WaitFor(1000);
  end;

  if Terminated then begin
   break;
  end else begin

   fLock.Acquire;
   try

    QueueItem:=nil;
    for Index:=0 to fQueueItems.Count-1 do begin
     TemporaryQueueItem:=fQueueItems.Items[Index];
     if TemporaryQueueItem.fResource.GetResource.fAsyncLoadState=TpvResource.TAsyncLoadState.Queued then begin
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
        Success:=Resource.BeginLoad(Stream);
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

     for Index:=0 to fQueueItems.Count-1 do begin
      TemporaryQueueItem:=fQueueItems.Items[Index];
      if (TemporaryQueueItem<>QueueItem) and
         (TemporaryQueueItem.fResource.GetResource.fAsyncLoadState=TpvResource.TAsyncLoadState.Queued) then begin
       HasMoreItems:=true;
       break;
      end;
     end;

    end;

   finally
    fLock.Release;
   end;

  end;
 end;
end;

function TpvResourceBackgroundLoader.QueueResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aParent:IpvResource=nil;const aOnFinish:TpvResourceOnFinish=nil):boolean;
var Index:TpvSizeInt;
    QueueItem,
    TemporaryQueueItem:TQueueItem;
    Resource:TpvResource;
    IResource:IpvResource;
    Found:boolean;
begin

 result:=false;

 fLock.Acquire;
 try

  if not fQueueItemFileNameMap.ExistKey(aFileName) then begin

   QueueItem:=nil;

   try

    IResource:=fResourceManager.GetResourceByFileName(aFileName);

    if assigned(IResource) then begin

     Resource:=IResource.GetResource;

     if not (Resource is aResourceClass) then begin
      raise EpvResourceClassMismatch.Create('Resource class mismatch');
     end;

    end else begin

     Resource:=aResourceClass.Create(fResourceManager,aFileName,aOnFinish);
     Resource.fAsyncLoadState:=TpvResource.TAsyncLoadState.Queued;

     IResource:=Resource.fInstanceInterface;

    end;

    QueueItem:=TQueueItem.Create;
    QueueItem.fResource:=IResource;

    fQueueItems.Add(QueueItem);

    if assigned(aParent) then begin

     TemporaryQueueItem:=fQueueItemFileNameMap.Values[aParent.GetResource.fFileName];

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
       if TemporaryQueueItem.fDependencies.Items[Index]=IResource then begin
        Found:=true;
        break;
       end;
      end;
      if not Found then begin
       TemporaryQueueItem.fDependencies.Add(IResource);
      end;

     end;

    end;

    result:=true;

   finally
    if assigned(QueueItem) then begin
     fQueueItemFileNameMap.Add(aFileName,QueueItem);
    end;
   end;

  end;

 finally
  fLock.Release;
 end;

 if result then begin
  fEvent.SetEvent;
 end;

end;

procedure TpvResourceBackgroundLoader.FinishBackgroundLoading(const aQueueItem:TQueueItem);
var IResource:IpvResource;
    Resource:TpvResource;
    Success:boolean;
begin

 IResource:=aQueueItem.fResource;

 Resource:=IResource.GetResource;

 Success:=Resource.fAsyncLoadState=TpvResource.TAsyncLoadState.Success;

 if Success then begin
  Success:=Resource.EndLoad;
  if Success then begin
   Resource.fLoaded:=true;
  end;
 end;

 Resource.fAsyncLoadState:=TpvResource.TAsyncLoadState.Done;

 if assigned(Resource.fOnFinish) then begin
  Resource.fOnFinish(IResource,Success);
 end;

end;

procedure TpvResourceBackgroundLoader.WaitForResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String);
var Index:TpvSizeInt;
    QueueItem:TQueueItem;
    IResource:IpvResource;
    Resource:TpvResource;
begin

 fLock.Acquire;
 try

  QueueItem:=fQueueItemFileNameMap.Values[aFileName];
  if assigned(QueueItem) then begin

   IResource:=fResourceManager.GetResourceByFileName(aFileName);

   if assigned(IResource) then begin

    Resource:=IResource.GetResource;

    if not (Resource is aResourceClass) then begin
     raise EpvResourceClassMismatch.Create('Resource class mismatch');
    end;

    while (QueueItem.fDependencies.Count>0) or
          (Resource.fAsyncLoadState in [TpvResource.TAsyncLoadState.Queued,
                                        TpvResource.TAsyncLoadState.Loading]) do begin
     fLock.Release;
     try
      TPasMP.Yield;
      Sleep(1);
     finally
      fLock.Acquire;
     end;
    end;

    FinishBackgroundLoading(QueueItem);

    try
     for Index:=0 to fQueueItems.Count-1 do begin
      if fQueueItems.Items[Index]=QueueItem then begin
       fQueueItems.Delete(Index);
       break;
      end;
     end;
    finally
     try
      fQueueItemFileNameMap.Delete(Resource.fFileName);
     finally
      FreeAndNil(QueueItem);
     end;
    end;

   end;

  end;

 finally
  fLock.Release;
 end;

end;

procedure TpvResourceBackgroundLoader.FinishResources(const aTimeout:TpvInt64=5);
var Index:TpvSizeInt;
    QueueItem:TQueueItem;
    IResource:IpvResource;
    Resource:TpvResource;
    Start:TpvHighResolutionTime;
begin

 Start:=pvApplication.HighResolutionTimer.GetTime;

 fLock.Acquire;
 try

  try

   Index:=0;
   while Index<fQueueItems.Count do begin

    QueueItem:=fQueueItems.Items[Index];

    IResource:=QueueItem.fResource;

    Resource:=IResource.GetResource;

    if (QueueItem.fDependencies.Count>0) or
          (Resource.fAsyncLoadState in [TpvResource.TAsyncLoadState.Queued,
                                        TpvResource.TAsyncLoadState.Loading]) then begin
     inc(Index);
    end else begin

     fLock.Release;
     try
      FinishBackgroundLoading(QueueItem);
     finally
      fLock.Acquire;
     end;

     try
      fQueueItemFileNameMap.Delete(Resource.fFileName);
     finally
      try
       FreeAndNil(QueueItem);
      finally
       fQueueItems.Delete(Index);
      end;
     end;

    end;

    if (aTimeout>=0) and
       (pvApplication.HighResolutionTimer.ToMilliseconds(pvApplication.HighResolutionTimer.GetTime-Start)>=aTimeout) then begin
     break;
    end;

   end;

  finally
   IResource:=nil;
  end;

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

{ TpvResourceManager }

constructor TpvResourceManager.Create;
begin
 inherited Create;
 fResourceLock:=TPasMPMultipleReaderSingleWriterLock.Create;
 fResourceList:=TResourceList.Create;
 fResourceList.OwnsObjects:=false;
 fResourceFileNameMap:=TResourceStringMap.Create(nil);
 if assigned(pvApplication) then begin
  fBaseDataPath:=TpvUTF8String(pvApplication.Assets.BasePath);
 end;
 fBackgroundLoader:=TpvResourceBackgroundLoader.Create(self);
end;

destructor TpvResourceManager.Destroy;
var Resource:TpvResource;
begin

 if not fBackgroundLoader.Finished then begin
  fBackgroundLoader.Terminate;
  fBackgroundLoader.fEvent.SetEvent;
  fBackgroundLoader.WaitFor;
 end;
 FreeAndNil(fBackgroundLoader);

 while fResourceList.Count>0 do begin
  Resource:=fResourceList.Items[fResourceList.Count-1];
  fResourceList.Items[fResourceList.Count-1]:=nil;
  fResourceList.Delete(fResourceList.Count-1);
  Resource.Free;
 end;
 fResourceList.Free;

 fResourceFileNameMap.Free;

 fResourceLock.Free;

 inherited Destroy;

end;

class function TpvResourceManager.SanitizeFileName(const aFileName:TpvUTF8String):TpvUTF8String;
begin
 result:=TpvUTF8String(StringReplace(LowerCase(String(aFileName)),'\','/',[rfReplaceAll]));
end;

function TpvResourceManager.GetResourceByFileName(const aFileName:TpvUTF8String):IpvResource;
begin
 fResourceLock.AcquireRead;
 try
  result:=fResourceFileNameMap.Values[SanitizeFileName(aFileName)];
 finally
  fResourceLock.ReleaseRead;
 end;
end;

function TpvResourceManager.GetResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aOnFinish:TpvResourceOnFinish=nil):IpvResource;
var Resource:TpvResource;
    FileName:TpvUTF8String;
begin
 FileName:=SanitizeFileName(aFileName);
 result:=GetResourceByFileName(FileName);
 if assigned(result) then begin
  Resource:=result.GetResource;
  if not (Resource is aResourceClass) then begin
   raise EpvResourceClassMismatch.Create('Resource class mismatch');
  end;
  fBackgroundLoader.WaitForResource(aResourceClass,FileName);
 end else begin
  Resource:=aResourceClass.Create(self,FileName,aOnFinish);
  if Resource.LoadFromFileName(FileName) then begin
   result:=Resource.InstanceInterface;
  end else begin
   FreeAndNil(result);
  end;
 end;
end;

function TpvResourceManager.BackgroundLoadResource(const aResourceClass:TpvResourceClass;const aFileName:TpvUTF8String;const aParent:IpvResource=nil;const aOnFinish:TpvResourceOnFinish=nil):boolean;
var Exist:boolean;
    FileName:TpvUTF8String;
begin
 FileName:=SanitizeFileName(aFileName);
 fResourceLock.AcquireRead;
 try
  Exist:=fResourceFileNameMap.ExistKey(FileName);
 finally
  fResourceLock.ReleaseRead;
 end;
 if Exist then begin
  result:=false;
 end else begin
  result:=fBackgroundLoader.QueueResource(aResourceClass,FileName,aParent,aOnFinish);
 end;
end;

initialization
end.

