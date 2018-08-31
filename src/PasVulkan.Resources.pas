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

interface

uses SysUtils,
     Classes,
     PasMP,
     PasVulkan.Types,
     PasVulkan.PooledObject,
     PasVulkan.Collections,
     PasVulkan.IDManager;

type TpvResourceManager=class;

     TpvResourceHandle=TpvInt32;

     TpvResource=class;

     IpvResource=interface(IpvReferenceCountedObject)['{AD2C0315-C8AF-4D79-876E-1FA42FB869F9}']
     end;

     TpvMetaResourceClass=class of TpvMetaResource;

     TpvMetaResource=class(TpvPooledObject)
      private
      protected
       fUUID:TpvUUID;
       fResourceLock:TPasMPSlimReaderWriterLock;
       fResource:TpvResource;
       fFileName:TpvUTF8String;
       fAssetName:TpvUTF8String;
       fName:TpvUTF8String;
       fTemporary:boolean;
       constructor CreateTemporary; reintroduce; virtual;
       procedure SetUUID(const aUUID:TpvUUID);
       procedure SetFileName(const aFileName:TpvUTF8String);
       procedure SetAssetName(const aAssetName:TpvUTF8String);
       function GetResource:IpvResource; virtual;
      public
       constructor Create; reintroduce; virtual;
       constructor CreateNew(const aFileName:TpvUTF8String); reintroduce; virtual;
       destructor Destroy; override;
       function HasResourceInstance:boolean; virtual;
       procedure LoadFromStream(const aStream:TStream); virtual;
       procedure LoadFromFile(const aFileName:TpvUTF8String); virtual;
       function Clone(const aFileName:TpvUTF8String):TpvMetaResource; virtual;
       procedure Rename(const aFileName:TpvUTF8String); virtual;
       procedure Delete; virtual;
       property UUID:TpvUUID read fUUID write SetUUID;
       property Resource:IpvResource read GetResource;
       property FileName:TpvUTF8String read fFileName write SetFileName;
       property AssetName:TpvUTF8String read fAssetName write SetAssetName;
       property Name:TpvUTF8String read fName write fName;
       property Temporary:boolean read fTemporary write fTemporary;
     end;

     TpvResource=class(TpvReferenceCountedObject,IpvResource)
      private
       fMetaResource:TpvMetaResource;
       fHandle:TpvResourceHandle;
      protected
       function _AddRef:TpvInt32; override; {$ifdef Windows}stdcall{$else}cdecl{$endif};
       function _Release:TpvInt32; override; {$ifdef Windows}stdcall{$else}cdecl{$endif};
      public
       constructor Create(const aMetaResource:TpvMetaResource=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       class function GetMetaResourceClass:TpvMetaResourceClass; virtual;
      published
       property MetaResource:TpvMetaResource read fMetaResource;
       property Handle:TpvResourceHandle read fHandle;
     end;

     TpvResourceManager=class(TpvPooledObject)
      private
       type TResourceHandleManager=class(TpvIDManager<TpvResourceHandle>);
            TResourceHandleMap=array of TpvResource;
            TpvMetaResourceList=class(TpvObjectGenericList<TpvMetaResource>);
            TpvMetaResourceUUIDMap=class(TpvHashMap<TpvUUID,TpvMetaResource>);
            TpvMetaResourceFileNameMap=class(TpvStringHashMap<TpvMetaResource>);
            TpvMetaResourceAssetNameMap=class(TpvStringHashMap<TpvMetaResource>);
      private
       fMetaResourceLock:TPasMPMultipleReaderSingleWriterLock;
       fMetaResourceList:TpvMetaResourceList;
       fMetaResourceUUIDMap:TpvMetaResourceUUIDMap;
       fMetaResourceFileNameMap:TpvMetaResourceFileNameMap;
       fMetaResourceAssetNameMap:TpvMetaResourceAssetNameMap;
       fResourceHandleLock:TPasMPMultipleReaderSingleWriterLock;
       fResourceHandleManager:TResourceHandleManager;
       fResourceHandleMap:TResourceHandleMap;
       fBaseDataPath:TpvUTF8String;
       function GetMetaResourceByUUID(const aUUID:TpvUUID):TpvMetaResource; inline;
       function GetMetaResourceByFileName(const aFileName:TpvUTF8String):TpvMetaResource; inline;
       function GetMetaResourceByAssetName(const aAssetName:TpvUTF8String):TpvMetaResource; inline;
      protected
       function AllocateHandle(const aResource:TpvResource):TpvResourceHandle;
       procedure FreeHandle(const aHandle:TpvResourceHandle);
       function GetResource(const aHandle:TpvResourceHandle):IpvResource; inline;
      public
       constructor Create;
       destructor Destroy; override;
       function GetNewUUID:TpvUUID;
       property Resources[const aHandle:TpvResourceHandle]:IpvResource read GetResource; default;
       property MetaResourceByUUID[const aUUID:TpvUUID]:TpvMetaResource read GetMetaResourceByUUID;
       property MetaResourceByFileName[const aFileName:TpvUTF8String]:TpvMetaResource read GetMetaResourceByFileName;
       property MetaResourceByAssetName[const aAssetName:TpvUTF8String]:TpvMetaResource read GetMetaResourceByAssetName;
       property BaseDataPath:TpvUTF8String read fBaseDataPath write fBaseDataPath;
     end;

var ResourceManager:TpvResourceManager=nil;

implementation

constructor TpvMetaResource.Create;
begin

 inherited Create;

 fUUID.UInt64s[0]:=0;
 fUUID.UInt64s[1]:=0;

 fResource:=nil;

 fResourceLock:=TPasMPSlimReaderWriterLock.Create;

 fFileName:='';

 fName:='';

 fTemporary:=false;

 ResourceManager.fMetaResourceLock.AcquireWrite;
 try
  ResourceManager.fMetaResourceList.Add(self);
 finally
  ResourceManager.fMetaResourceLock.ReleaseWrite;
 end;

end;

constructor TpvMetaResource.CreateTemporary;
begin
 Create;
 fTemporary:=true;
 SetUUID(ResourceManager.GetNewUUID);
end;

constructor TpvMetaResource.CreateNew(const aFileName:TpvUTF8String);
begin
 Create;
 SetUUID(ResourceManager.GetNewUUID);
 SetFileName(aFileName);
end;

destructor TpvMetaResource.Destroy;
begin

 FreeAndNil(fResource);

 ResourceManager.fMetaResourceLock.AcquireWrite;
 try
  if length(fFileName)>0 then begin
   ResourceManager.fMetaResourceFileNameMap.Delete(LowerCase(fFileName));
   fFileName:='';
  end;
  if length(fAssetName)>0 then begin
   ResourceManager.fMetaResourceAssetNameMap.Delete(LowerCase(fAssetName));
   fAssetName:='';
  end;
  if (fUUID.UInt64s[0]<>0) or
     (fUUID.UInt64s[1]<>0) then begin
   ResourceManager.fMetaResourceUUIDMap.Delete(fUUID);
   fUUID.UInt64s[0]:=0;
   fUUID.UInt64s[1]:=0;
  end;
  ResourceManager.fMetaResourceList.Remove(self);
 finally
  ResourceManager.fMetaResourceLock.ReleaseWrite;
 end;

 fResourceLock.Free;

 inherited Destroy;
end;

procedure TpvMetaResource.SetUUID(const aUUID:TpvUUID);
begin
 if (fUUID.UInt64s[0]<>aUUID.UInt64s[0]) or
    (fUUID.UInt64s[1]<>aUUID.UInt64s[1]) then begin
  ResourceManager.fMetaResourceLock.AcquireWrite;
  try
   if (fUUID.UInt64s[0]<>0) or
      (fUUID.UInt64s[1]<>0) then begin
    ResourceManager.fMetaResourceUUIDMap.Delete(fUUID);
   end;
   if (aUUID.UInt64s[0]<>0) or
      (aUUID.UInt64s[1]<>0) then begin
    fUUID:=aUUID;
    ResourceManager.fMetaResourceUUIDMap.Add(aUUID,self);
   end;
  finally
   ResourceManager.fMetaResourceLock.ReleaseWrite;
  end;
 end;
end;

procedure TpvMetaResource.SetFileName(const aFileName:TpvUTF8String);
begin
 if fFileName<>aFileName then begin
  ResourceManager.fMetaResourceLock.AcquireWrite;
  try
   if length(fFileName)>0 then begin
    ResourceManager.fMetaResourceFileNameMap.Delete(LowerCase(fFileName));
   end;
   if length(aFileName)>0 then begin
    fFileName:=aFileName;
    ResourceManager.fMetaResourceFileNameMap.Add(LowerCase(aFileName),self);
   end;
  finally
   ResourceManager.fMetaResourceLock.ReleaseWrite;
  end;
 end;
 if length(aFileName)>0 then begin
  if copy(LowerCase(aFileName),1,length(ResourceManager.fBaseDataPath))=LowerCase(ResourceManager.fBaseDataPath) then begin
   SetAssetName(StringReplace(copy(aFileName,length(ResourceManager.fBaseDataPath)+1,(length(aFileName)-length(ResourceManager.fBaseDataPath))+1),'\','/',[rfReplaceAll]));
  end;
 end;
end;

procedure TpvMetaResource.SetAssetName(const aAssetName:TpvUTF8String);
begin
 if fAssetName<>aAssetName then begin
  ResourceManager.fMetaResourceLock.AcquireWrite;
  try
   if length(fAssetName)>0 then begin
    ResourceManager.fMetaResourceAssetNameMap.Delete(LowerCase(fAssetName));
   end;
   if length(aAssetName)>0 then begin
    fAssetName:=aAssetName;
    ResourceManager.fMetaResourceAssetNameMap.Add(LowerCase(aAssetName),self);
   end;
  finally
   ResourceManager.fMetaResourceLock.ReleaseWrite;
  end;
 end;
end;

function TpvMetaResource.GetResource:IpvResource;
begin
 fResourceLock.Acquire;
 try
  result:=fResource;
 finally
  fResourceLock.Release;
 end;
end;

function TpvMetaResource.HasResourceInstance:boolean;
begin
 result:=assigned(fResource);
end;

procedure TpvMetaResource.LoadFromStream(const aStream:TStream);
begin
end;

procedure TpvMetaResource.LoadFromFile(const aFileName:TpvUTF8String);
var FileStream:TFileStream;
begin
 FileStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(FileStream);
  SetFileName(aFileName);
 finally
  FileStream.Free;
 end;
end;

function TpvMetaResource.Clone(const aFileName:TpvUTF8String):TpvMetaResource;
var MetaResourceClass:TpvMetaResourceClass;
begin
 MetaResourceClass:=TpvMetaResourceClass(ClassType);
 result:=MetaResourceClass.Create;
 result.SetUUID(ResourceManager.GetNewUUID);
 result.SetFileName(aFileName);
end;

procedure TpvMetaResource.Rename(const aFileName:TpvUTF8String);
begin
 if length(fFileName)>0 then begin
  RenameFile(fFileName,aFileName);
  SetFileName(aFileName);
 end;
end;

procedure TpvMetaResource.Delete;
begin
 if length(fFileName)>0 then begin
  DeleteFile(fFileName);
 end;
 Free;
end;

constructor TpvResource.Create(const aMetaResource:TpvMetaResource);
begin
 fMetaResource:=aMetaResource;
 if not assigned(fMetaResource) then begin
  fMetaResource:=GetMetaResourceClass.CreateTemporary;
 end;
 TPasMPInterlocked.Write(TObject(fMetaResource.fResource),TObject(self));
 fHandle:=ResourceManager.AllocateHandle(self);
 inherited Create;
end;

destructor TpvResource.Destroy;
begin
 inherited Destroy;
 if assigned(fMetaResource) then begin
  TPasMPInterlocked.Write(TObject(fMetaResource.fResource),nil);
 end;
 ResourceManager.FreeHandle(fHandle);
 if assigned(fMetaResource) and fMetaResource.fTemporary then begin
  FreeAndNil(fMetaResource);
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
 if assigned(fMetaResource) then begin
  result:=TPasMPInterlocked.Decrement(fReferenceCounter);
  if result=0 then begin
   fMetaResource.fResourceLock.Acquire;
   try
    if TPasMPInterlocked.Read(fReferenceCounter)=0 then begin
     Free;
    end;
   finally
    if assigned(fMetaResource) and assigned(fMetaResource.fResourceLock) then begin
     fMetaResource.fResourceLock.Release;
    end;
   end;
  end;
 end else begin
  result:=inherited _Release;
 end;
end;

class function TpvResource.GetMetaResourceClass:TpvMetaResourceClass;
begin
 result:=TpvMetaResource;
end;

constructor TpvResourceManager.Create;
begin
 inherited Create;
 ResourceManager:=self;
 fMetaResourceLock:=TPasMPMultipleReaderSingleWriterLock.Create;
 fMetaResourceList:=TpvMetaResourceList.Create;
 fMetaResourceList.OwnsObjects:=false;
 fMetaResourceUUIDMap:=TpvMetaResourceUUIDMap.Create(nil);
 fMetaResourceFileNameMap:=TpvMetaResourceFileNameMap.Create(nil);
 fMetaResourceAssetNameMap:=TpvMetaResourceAssetNameMap.Create(nil);
 fResourceHandleLock:=TPasMPMultipleReaderSingleWriterLock.Create;
 fResourceHandleManager:=TResourceHandleManager.Create;
 fResourceHandleMap:=nil;
end;

destructor TpvResourceManager.Destroy;
begin

 while fMetaResourceList.Count>0 do begin
  fMetaResourceList.Items[fMetaResourceList.Count-1].Free;
 end;
 fMetaResourceList.Free;

 fMetaResourceUUIDMap.Free;

 fMetaResourceFileNameMap.Free;

 fMetaResourceAssetNameMap.Free;

 fMetaResourceLock.Free;

 fResourceHandleManager.Free;

 SetLength(fResourceHandleMap,0);

 fResourceHandleLock.Free;

 ResourceManager:=nil;

 inherited Destroy;
end;

function TpvResourceManager.GetMetaResourceByUUID(const aUUID:TpvUUID):TpvMetaResource;
begin
 fMetaResourceLock.AcquireRead;
 try
  result:=fMetaResourceUUIDMap.Values[aUUID];
 finally
  fMetaResourceLock.ReleaseRead;
 end;
end;

function TpvResourceManager.GetMetaResourceByFileName(const aFileName:TpvUTF8String):TpvMetaResource;
begin
 fMetaResourceLock.AcquireRead;
 try
  result:=fMetaResourceFileNameMap.Values[LowerCase(aFileName)];
 finally
  fMetaResourceLock.ReleaseRead;
 end;
end;

function TpvResourceManager.GetMetaResourceByAssetName(const aAssetName:TpvUTF8String):TpvMetaResource;
begin
 fMetaResourceLock.AcquireRead;
 try
  result:=fMetaResourceAssetNameMap.Values[LowerCase(aAssetName)];
 finally
  fMetaResourceLock.ReleaseRead;
 end;
end;

function TpvResourceManager.AllocateHandle(const aResource:TpvResource):TpvResourceHandle;
var OldCount:TpvInt32;
begin
 result:=fResourceHandleManager.AllocateID;
 fResourceHandleLock.AcquireWrite;
 try
  OldCount:=length(fResourceHandleMap);
  if OldCount<=result then begin
   SetLength(fResourceHandleMap,(result+1)*2);
   FillChar(fResourceHandleMap[OldCount],(length(fResourceHandleMap)-OldCount)*SizeOf(TpvResource),#0);
  end;
  fResourceHandleMap[result]:=aResource;
 finally
  fResourceHandleLock.ReleaseWrite;
 end;
end;

procedure TpvResourceManager.FreeHandle(const aHandle:TpvResourceHandle);
begin
 fResourceHandleLock.AcquireWrite;
 try
  fResourceHandleMap[aHandle]:=nil;
 finally
  fResourceHandleLock.ReleaseWrite;
 end;
 fResourceHandleManager.FreeID(aHandle);
end;

function TpvResourceManager.GetResource(const aHandle:TpvResourceHandle):IpvResource;
begin
 if aHandle>=0 then begin
  fResourceHandleLock.AcquireRead;
  try
   if aHandle<length(fResourceHandleMap) then begin
    result:=fResourceHandleMap[aHandle];
   end else begin
    result:=nil;
   end;
  finally
   fResourceHandleLock.ReleaseRead;
  end;
 end else begin
  result:=nil;
 end;
end;

function TpvResourceManager.GetNewUUID:TpvUUID;
begin
 repeat
  result:=TpvUUID.Create;
 until not assigned(GetMetaResourceByUUID(result));
end;

initialization
end.

