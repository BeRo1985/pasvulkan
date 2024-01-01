(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses Classes,
     SysUtils,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Scene3D;

{

A scene node can be an entity or even a component for an entity node as well, here is no distinction for simplicity, for the contrast to 
the entity-component-system pattern, which is also implemented in the PasVulkan framework, see the PasVulkan.EntityComponentSystem.pas unit.
So it's your choice, if you want to use the entity-component-system pattern or the scene graph pattern or both.

The scene graph pattern is a tree structure, where each node can have zero or more child nodes, but only one parent node. The root node
has no parent node. Each node can have zero or more data objects, which can be used for any purpose, even as components for an entity
node. The scene graph pattern is very useful for rendering, physics, audio, AI, etc. and is very flexible and easy to use. 

GetNodeListOf returns a list of all child nodes of the specified node class.

GetNodeOf returns the child node of the specified node class at the specified index, which is zero by default, and nil if there is out of bounds.

GetNodeCountOf returns the count of child nodes of the specified node class.

StartLoad, BackgroundLoad and FinishLoad are used for loading of data, which can be done in parallel, like loading of textures, meshes, etc. 
Or to be more precise, StartLoad is called before the background loading of the scene graph, BackgroundLoad is called in a background thread
and should be used for loading of data, which can be done in parallel and FinishLoad is called after the background loading of the scene graph. 

StartLoad is called before the background loading of the scene graph. It's called in the main thread.

BackgroundLoad is called in a background thread and should be used for loading of data, which can be done in parallel.

FinishLoad is called after the background loading of the scene graph. It's called in the main thread.

WaitForLoaded waits until the scene graph or node is loaded.

IsLoaded returns true, if the scene graph or node is loaded.

These loading functions should be called just once before the beginning of a level or game together with a loading screen, etc. For other
resources, which are loaded during the game, like textures, meshes, etc. should be loaded in an other way, for example, with the
resource manager of the PasVulkan framework, see the PasVulkan.Resources.pas unit. These loading functions here are just for to simplify 
the initial loading of a level or game without the actual mess of loading of resources during the game with a resource manager, etc.  

Store and Interpolate are used for interpolation of the scene graph for the "Fix your timestep" pattern, which means, that the scene graph
is updated with a fixed timestep, but rendered with a variable timestep, which is interpolated between the last and the current scene graph
state for smooth rendering. Where Store is called for storing the scene graph state, Interpolate is called for interpolating the scene graph
with a fixed timestep with aDeltaTime as parameter, Interpolate is called for interpolating the scene graph with a variable timestep with 
aAlpha as parameter. And FrameUpdate is called after Interpolate for updating some stuff just frame-wise, like audio, etc. and is called
in the main thread.

Render is called for rendering the scene graph and can be called in the main "or" in a render thread, depending on the settings of the
PasVulkan main loop, so be careful with thread-safety.

UpdateAudio is called for updating audio and is called in the audio thread, so be careful with thread-safety. So use it in combination with
FrameUpdate, which is called in the main thread, with a thread safe data ring buffer oder queue for audio data, which is filled in FrameUpdate
and read in UpdateAudio. You can use the constructs from PasMP for that, see the PasMP.pas unit.

}

type TpvScene=class;

     TpvSceneNode=class;

     TpvSceneNodeClass=class of TpvSceneNode;

     TpvSceneNodes=TpvObjectGenericList<TpvSceneNode>;

     TpvSceneNodeHashMap=TpvHashMap<TpvSceneNodeClass,TpvSceneNodes>;

     TpvSceneNodeState=TPasMPInt32;
     PpvSceneNodeState=^TpvSceneNodeState;

     TpvSceneNodeStateHelper=record helper for TpvSceneNodeState
      public
       const Unloaded=TpvSceneNodeState(0);
             StartingLoading=TpvSceneNodeState(1);
             Loading=TpvSceneNodeState(2);
             Loaded=TpvSceneNodeState(3);
             Failed=TpvSceneNodeState(4);
             Unloading=TpvSceneNodeState(5);
     end;

     { TpvSceneNode }
     TpvSceneNode=class      
      public
      private
       fScene:TpvScene;
       fParent:TpvSceneNode;
       fData:TObject;
       fChildren:TpvSceneNodes;
       fNodeHashMap:TpvSceneNodeHashMap;
       fLock:TpvInt32;
       fState:TpvSceneNodeState;
       fDestroying:boolean;
      public
       constructor Create(const aParent:TpvSceneNode;const aData:TObject=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure Add(const aNode:TpvSceneNode);
       procedure Remove(const aNode:TpvSceneNode);
       function GetNodeListOf(const aNodeClass:TpvSceneNodeClass):TpvSceneNodes;
       function GetNodeOf(const aNodeClass:TpvSceneNodeClass;const aIndex:TpvSizeInt=0):TpvSceneNode;
       function GetNodeCountOf(const aNodeClass:TpvSceneNodeClass):TpvSizeInt;
       procedure StartLoad; virtual;
       procedure BackgroundLoad; virtual                                      ;
       procedure FinishLoad; virtual;
       procedure WaitForLoaded; virtual;
       function IsLoaded:boolean; virtual;
       procedure Store; virtual;
       procedure BeginUpdate(const aDeltaTime:TpvDouble); virtual;
       procedure Update(const aDeltaTime:TpvDouble); virtual;
       procedure EndUpdate(const aDeltaTime:TpvDouble); virtual;
       procedure Interpolate(const aAlpha:TpvDouble); virtual;
       procedure FrameUpdate; virtual;
       procedure Render; virtual;
       procedure UpdateAudio; virtual;
      public
       property State:TpvSceneNodeState read fState;
      published
       property Scene:TpvScene read fScene;
       property Parent:TpvSceneNode read fParent;
       property Data:TObject read fData;
       property Children:TpvSceneNodes read fChildren;
     end;

     { TpvScene }
     TpvScene=class
      private
       fRootNode:TpvSceneNode;
       fData:TObject;
      public
       constructor Create(const aData:TObject=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure StartLoad; virtual;
       procedure BackgroundLoad; virtual;
       procedure FinishLoad; virtual;
       procedure WaitForLoaded; virtual;
       function IsLoaded:boolean; virtual;
       procedure Store; virtual;
       procedure BeginUpdate(const aDeltaTime:TpvDouble); virtual;
       procedure Update(const aDeltaTime:TpvDouble); virtual;
       procedure EndUpdate(const aDeltaTime:TpvDouble); virtual;
       procedure Interpolate(const aAlpha:TpvDouble); virtual;
       procedure FrameUpdate; virtual;
       procedure Render; virtual;
       procedure UpdateAudio; virtual;
      published
       property RootNode:TpvSceneNode read fRootNode;
       property Data:TObject read fData;
     end;

     { TpvSceneNode3D }
     TpvSceneNode3D=class(TpvSceneNode)
      private
       fLastNode3DParent:TpvSceneNode3D;
       fTransform:TpvMatrix4x4;
       fCachedWorldTransform:TpvMatrix4x4;
       fLastCachedWorldTransform:TpvMatrix4x4;
       fInterpolatedCachedWorldTransform:TpvMatrix4x4;
       fBounds:TpvAABB;
      protected
       procedure UpdateCachedWorldTransform; virtual;
       procedure RecursiveUpdateCachedWorldTransform; virtual;
       procedure SetTransform(const aValue:TpvMatrix4x4); virtual;
       function GetWorldTransform:TpvMatrix4x4; virtual;
       procedure SetWorldTransform(const aWorldTransform:TpvMatrix4x4); virtual;
       procedure UpdateBounds; virtual;
      public
       constructor Create(const aParent:TpvSceneNode;const aData:TObject=nil); override;
       destructor Destroy; override;
       procedure Store; override;
       procedure BeginUpdate(const aDeltaTime:TpvDouble); override;
       procedure Update(const aDeltaTime:TpvDouble); override;
       procedure EndUpdate(const aDeltaTime:TpvDouble); override;
       procedure Interpolate(const aAlpha:TpvDouble); override;
      public
       property Transform:TpvMatrix4x4 read fTransform write SetTransform;
       property WorldTransform:TpvMatrix4x4 read GetWorldTransform write SetWorldTransform;
       property CachedWorldTransform:TpvMatrix4x4 read fCachedWorldTransform;
       property LastCachedWorldTransform:TpvMatrix4x4 read fLastCachedWorldTransform;
       property InterpolatedCachedWorldTransform:TpvMatrix4x4 read fInterpolatedCachedWorldTransform;
       property Bounds:TpvAABB read fBounds write fBounds;
     end;

implementation

uses PasVulkan.Application;

{ TpvSceneNode }

constructor TpvSceneNode.Create(const aParent:TpvSceneNode;const aData:TObject);
begin
 inherited Create;

 fLock:=0;

 fParent:=aParent;

 if assigned(fScene) then begin
  fScene:=fParent.fScene;
 end else begin
  fScene:=nil;
 end;

 fData:=aData;

 fChildren:=TpvSceneNodes.Create;
 fChildren.OwnsObjects:=true;

 fDestroying:=false;

 TPasMPInterlocked.Write(fState,TpvSceneNodeState.Unloaded);

 fNodeHashMap:=TpvSceneNodeHashMap.Create(nil);

 if assigned(fParent) then begin
  fParent.Add(self);
 end;

end;

destructor TpvSceneNode.Destroy;
var ChildNodeIndex:TpvSizeInt;
    ChildNode,ParentNode:TpvSceneNode;
    NodeClass:TpvSceneNodeClass;
    Nodes:TpvSceneNodes;
begin

 if assigned(fParent) and not fDestroying then begin
  ParentNode:=fParent;
  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(ParentNode.fLock);
  try
   fParent:=nil;
   ChildNodeIndex:=ParentNode.fChildren.IndexOf(self);
   if ChildNodeIndex>=0 then begin
    NodeClass:=TpvSceneNodeClass(ClassType);
    Nodes:=ParentNode.fNodeHashMap[NodeClass];
    if assigned(Nodes) then begin
     Nodes.Remove(self);
    end;
    ParentNode.fChildren.Extract(ChildNodeIndex);
   end;
  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(ParentNode.fLock);
  end;
 end;

 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.fDestroying:=true;
 end;
 FreeAndNil(fChildren);

 for Nodes in fNodeHashMap.Values do begin
  Nodes.Free;
 end;
 FreeAndNil(fNodeHashMap);

 inherited Destroy;
end;

procedure TpvSceneNode.Add(const aNode:TpvSceneNode);
var NodeClass:TpvSceneNodeClass;
    Nodes:TpvSceneNodes;
begin
 if assigned(aNode) then begin

  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fLock);
  try

   NodeClass:=TpvSceneNodeClass(aNode.ClassType);

   Nodes:=fNodeHashMap[NodeClass];
   if not assigned(Nodes) then begin
    Nodes:=TpvSceneNodes.Create;
    Nodes.OwnsObjects:=false;
    fNodeHashMap[NodeClass]:=Nodes;
   end;
   Nodes.Add(aNode);

   fChildren.Add(aNode);

   aNode.fParent:=self;

  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fLock);
  end;

 end;
end;

procedure TpvSceneNode.Remove(const aNode:TpvSceneNode);
var Index:TpvSizeInt;
    NodeClass:TpvSceneNodeClass;
    Nodes:TpvSceneNodes;
begin
 if assigned(aNode) and (aNode.fParent=self) and not aNode.fDestroying then begin

  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fLock);
  try

   Index:=fChildren.IndexOf(aNode);
   if Index>=0 then begin

    aNode.fDestroying:=true;

    NodeClass:=TpvSceneNodeClass(aNode.ClassType);

    Nodes:=fNodeHashMap[NodeClass];
    if assigned(Nodes) then begin
     Nodes.Remove(aNode);
    end;

    fChildren.Extract(Index);

    aNode.Free;

   end;

  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fLock);
  end;

 end;
end;

function TpvSceneNode.GetNodeListOf(const aNodeClass:TpvSceneNodeClass):TpvSceneNodes;
begin
 result:=fNodeHashMap[aNodeClass];
end;

function TpvSceneNode.GetNodeOf(const aNodeClass:TpvSceneNodeClass;const aIndex:TpvSizeInt=0):TpvSceneNode;
var Nodes:TpvSceneNodes;
begin
 Nodes:=fNodeHashMap[aNodeClass];
 if assigned(Nodes) and (aIndex>=0) and (aIndex<Nodes.Count) then begin
  result:=Nodes[aIndex];
 end else begin
  result:=nil;
 end;
end;

function TpvSceneNode.GetNodeCountOf(const aNodeClass:TpvSceneNodeClass):TpvSizeInt;
var Nodes:TpvSceneNodes;
begin
 Nodes:=fNodeHashMap[aNodeClass];
 if assigned(Nodes) then begin
  result:=Nodes.Count;
 end else begin
  result:=0;
 end;
end;

procedure TpvSceneNode.StartLoad;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.StartLoad;
 end;
 TPasMPInterlocked.Write(fState,TpvSceneNodeState.StartingLoading);
end;

procedure TpvSceneNode.BackgroundLoad;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.BackgroundLoad;
 end;
 TPasMPInterlocked.Write(fState,TpvSceneNodeState.Loading);
end;

procedure TpvSceneNode.FinishLoad;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.FinishLoad;
 end;
 pvApplication.Log(LOG_DEBUG,ClassName+'.FinishLoad.WaitForLoaded','Entering...');
 try
  while TPasMPInterlocked.Read(fState)<TpvSceneNodeState.Loading do begin
   Sleep(1);
  end;
  TPasMPInterlocked.Write(fState,TpvSceneNodeState.Loaded);
 finally
  pvApplication.Log(LOG_DEBUG,ClassName+'.FinishLoad.WaitForLoaded','Leaving...');
 end;
end;

procedure TpvSceneNode.WaitForLoaded;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 pvApplication.Log(LOG_DEBUG,ClassName+'.WaitForLoaded','Entering...');
 try
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   ChildNode.WaitForLoaded;
  end;
  while TPasMPInterlocked.Read(fState)<TpvSceneNodeState.Loaded do begin
   Sleep(1);
  end;
 finally
  pvApplication.Log(LOG_DEBUG,ClassName+'.WaitForLoaded','Leaving...');
 end;
end;

function TpvSceneNode.IsLoaded:boolean;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  result:=ChildNode.IsLoaded;
  if not result then begin
   exit;
  end;
 end;
 result:=TPasMPInterlocked.Read(fState)>=TpvSceneNodeState.Loaded;
end;

procedure TpvSceneNode.Store;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.Store;
   end;
  end;
 end;
end;

procedure TpvSceneNode.BeginUpdate(const aDeltaTime:TpvDouble);
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.BeginUpdate(aDeltaTime);
   end;
  end;
 end;
end;

procedure TpvSceneNode.Update(const aDeltaTime:TpvDouble);
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.Update(aDeltaTime);
   end;
  end;
 end;
end;

procedure TpvSceneNode.EndUpdate(const aDeltaTime:TpvDouble);
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.EndUpdate(aDeltaTime);
   end;
  end;
 end;
end;

procedure TpvSceneNode.Interpolate(const aAlpha:TpvDouble);
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.Interpolate(aAlpha);
   end;
  end;
 end;
end;

procedure TpvSceneNode.FrameUpdate;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.FrameUpdate;
   end;
  end;
 end;
end;

procedure TpvSceneNode.Render;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.Render;
   end;
  end;
 end;
end;

procedure TpvSceneNode.UpdateAudio;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.UpdateAudio;
   end;
  end;
 end;
end;

{ TpvScene }

constructor TpvScene.Create(const aData:TObject=nil);
begin
 inherited Create;
 fRootNode:=TpvSceneNode.Create(nil);
 fRootNode.fScene:=self;
 fData:=aData;
end;

destructor TpvScene.Destroy;
begin
 FreeAndNil(fRootNode);
 inherited Destroy;
end;

procedure TpvScene.StartLoad;
begin
 fRootNode.StartLoad;
end;

procedure TpvScene.BackgroundLoad;
begin
 fRootNode.BackgroundLoad;
end;

procedure TpvScene.FinishLoad;
begin
 fRootNode.FinishLoad;
end;

procedure TpvScene.WaitForLoaded;
begin
 fRootNode.WaitForLoaded;
end;

function TpvScene.IsLoaded:boolean;
begin
 result:=fRootNode.IsLoaded;
end;

procedure TpvScene.Store;
begin
 fRootNode.Store;
end;

procedure TpvScene.BeginUpdate(const aDeltaTime:TpvDouble);
begin
 fRootNode.BeginUpdate(aDeltaTime);
end;

procedure TpvScene.Update(const aDeltaTime:TpvDouble);
begin
 fRootNode.Update(aDeltaTime);
end;

procedure TpvScene.EndUpdate(const aDeltaTime:TpvDouble);
begin
 fRootNode.EndUpdate(aDeltaTime);
end;

procedure TpvScene.Interpolate(const aAlpha:TpvDouble);
begin
 fRootNode.Interpolate(aAlpha);
end;

procedure TpvScene.FrameUpdate;
begin
 fRootNode.FrameUpdate;
end;

procedure TpvScene.Render;
begin
 fRootNode.Render;
end;

procedure TpvScene.UpdateAudio;
begin
 fRootNode.UpdateAudio;
end;

{ TpvSceneNode3D }

constructor TpvSceneNode3D.Create(const aParent:TpvSceneNode;const aData:TObject=nil);
var LastNode3D:TpvSceneNode; 
begin

 inherited Create(aParent,aData);

 LastNode3D:=fParent;
 while assigned(LastNode3D) and not (LastNode3D is TpvSceneNode3D) do begin
  LastNode3D:=LastNode3D.fParent;
 end;
 if not (assigned(LastNode3D) and (LastNode3D is TpvSceneNode3D)) then begin
  LastNode3D:=nil; // No parent TpvSceneNode3D found
 end;

 fLastNode3DParent:=TpvSceneNode3D(LastNode3D);

 fTransform:=TpvMatrix4x4.Identity;

 fCachedWorldTransform:=TpvMatrix4x4.Identity;

end;

destructor TpvSceneNode3D.Destroy;
begin
 inherited Destroy;
end;

procedure TpvSceneNode3D.UpdateCachedWorldTransform;
begin
 if assigned(fLastNode3DParent) then begin
  fCachedWorldTransform:=fLastNode3DParent.fCachedWorldTransform*fTransform;
 end else begin
  fCachedWorldTransform:=fTransform;
 end;
end;

procedure TpvSceneNode3D.RecursiveUpdateCachedWorldTransform;
var Index:TpvSizeInt;
    Node:TpvSceneNode;
begin
 UpdateCachedWorldTransform;
 for Index:=0 to fChildren.Count-1 do begin
  Node:=fChildren[Index];
  if Node is TpvSceneNode3D then begin
   TpvSceneNode3D(Node).RecursiveUpdateCachedWorldTransform;
  end;
 end;
end;

procedure TpvSceneNode3D.SetTransform(const aValue:TpvMatrix4x4);
begin
 fTransform:=aValue;
 RecursiveUpdateCachedWorldTransform;
end;

function TpvSceneNode3D.GetWorldTransform:TpvMatrix4x4;
begin
 if assigned(fLastNode3DParent) then begin
  result:=fLastNode3DParent.GetWorldTransform*fTransform;
 end else begin
  result:=fTransform;
 end;
end;

procedure TpvSceneNode3D.SetWorldTransform(const aWorldTransform:TpvMatrix4x4);
begin
 if assigned(fLastNode3DParent) then begin
  fTransform:=fLastNode3DParent.GetWorldTransform.Inverse*aWorldTransform;
 end else begin
  fTransform:=aWorldTransform;
 end;
 RecursiveUpdateCachedWorldTransform;
end;

procedure TpvSceneNode3D.UpdateBounds;
begin
end;

procedure TpvSceneNode3D.Store;
begin
 inherited Store;
 fLastCachedWorldTransform:=fCachedWorldTransform;
end;

procedure TpvSceneNode3D.BeginUpdate(const aDeltaTime:TpvDouble);
begin
 inherited BeginUpdate(aDeltaTime);
end;

procedure TpvSceneNode3D.Update(const aDeltaTime:TpvDouble);
begin
 inherited Update(aDeltaTime);
end;

procedure TpvSceneNode3D.EndUpdate(const aDeltaTime:TpvDouble);
begin
 inherited EndUpdate(aDeltaTime);
end;

procedure TpvSceneNode3D.Interpolate(const aAlpha:TpvDouble);
begin
 fInterpolatedCachedWorldTransform:=fLastCachedWorldTransform.Slerp(fCachedWorldTransform,aAlpha);
 inherited Interpolate(aAlpha);
end;

end.
