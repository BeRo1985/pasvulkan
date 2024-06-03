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
     PasVulkan.Scene3D,
     PasVulkan.Utils;

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

LoadSynchronizationPoint should be called every frame outside of Update and Render functions to have a synchronization point for the loading
mechanism of the scene graph.

WaitForLoaded waits until the scene graph or node is loaded, and should be only used with awareness, because it can block the main thread.

IsLoaded returns true, if the scene graph or node is loaded.

Check is called for checking outside and before the Update and Render functions in parallel lock-step, for doing stuff which needs to be 
done sequentially in serial order, like creating or destroying of objects, or checking stuff, etc.

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

Serialize and Deserialize are used for serialization and deserialization of the scene graph and can be used for saving and loading of the
scene graph, for example for saving and loading of a game level, etc.

And very important, avoid acyclic and circular dependencies as much as possible, because it can lead to deadlocks, etc. and can be very
difficult to debug. If you have to use them, use them with awareness and be careful with them.

}

type TpvScene=class;

     TpvSceneNode=class;

     TpvSceneNodeClass=class of TpvSceneNode;

     TpvSceneNodes=TpvObjectGenericList<TpvSceneNode>;

     TpvSceneNodeStack=TpvDynamicFastStack<TpvSceneNode>;

     TpvSceneNodeHashMap=TpvHashMap<TpvSceneNodeClass,TpvSceneNodes>;

     TpvSceneNodeState=TPasMPInt32;
     PpvSceneNodeState=^TpvSceneNodeState;

     TpvSceneNodeStateHelper=record helper for TpvSceneNodeState
      public
       const Unused=TpvSceneNodeState(0);
             Unloaded=TpvSceneNodeState(1);
             StartLoading=TpvSceneNodeState(2);
             StartLoaded=TpvSceneNodeState(3);
             BackgroundLoading=TpvSceneNodeState(4);
             BackgroundLoaded=TpvSceneNodeState(5);
             Loading=TpvSceneNodeState(6);
             Loaded=TpvSceneNodeState(7);
             Failed=TpvSceneNodeState(8);
             Unloading=TpvSceneNodeState(9);
     end;

     { TpvSceneNode }
     TpvSceneNode=class      
      public
      private
       fScene:TpvScene;
       fParent:TpvSceneNode;
       fData:TObject;
       fIndex:TpvSizeInt;
       fChildren:TpvSceneNodes;
       fIncomingNodeDependencies:TpvSceneNodes;
       fOutgoingNodeDependencies:TpvSceneNodes;
       fNodeHashMap:TpvSceneNodeHashMap;
       fLock:TpvInt32;
       fState:TpvSceneNodeState;
       fDestroying:boolean;
      public 
       fStartLoadVisitGeneration:TpvUInt32;
       fBackgroundLoadVisitGeneration:TpvUInt32;
       fFinishLoadVisitGeneration:TpvUInt32;
      public
       
       constructor Create(const aParent:TpvSceneNode;const aData:TObject=nil); reintroduce; virtual;
       destructor Destroy; override;
       
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       
       procedure AddDependency(const aNode:TpvSceneNode);
       procedure RemoveDependency(const aNode:TpvSceneNode);
       
       procedure Add(const aNode:TpvSceneNode);
       procedure Remove(const aNode:TpvSceneNode);
       
       function GetNodeListOf(const aNodeClass:TpvSceneNodeClass):TpvSceneNodes;
       function GetNodeOf(const aNodeClass:TpvSceneNodeClass;const aIndex:TpvSizeInt=0):TpvSceneNode;
       function GetNodeCountOf(const aNodeClass:TpvSceneNodeClass):TpvSizeInt;
       
       procedure BeforeStartLoad; virtual;
       procedure StartLoad; virtual;
       procedure AfterStartLoad; virtual;
       
       procedure BeforeBackgroundLoad; virtual;
       procedure BackgroundLoad; virtual;
       procedure AfterBackgroundLoad; virtual;
       
       procedure BeforeFinishLoad; virtual;
       procedure FinishLoad; virtual;
       procedure AfterFinishLoad; virtual;
       
       procedure WaitForLoaded; virtual;
       
       function IsLoaded:boolean; virtual;
              
       procedure Check; virtual;

       procedure Store; virtual;

       procedure BeginUpdate(const aDeltaTime:TpvDouble); virtual;
       procedure Update(const aDeltaTime:TpvDouble); virtual;
       procedure EndUpdate(const aDeltaTime:TpvDouble); virtual;
       
       procedure Interpolate(const aAlpha:TpvDouble); virtual;
       
       procedure FrameUpdate; virtual;
       
       procedure Render; virtual;
       
       procedure UpdateAudio; virtual;
       
       function Serialize:TObject; virtual;       
       procedure Deserialize(const aData:TObject); virtual;

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
      public
       type TBackgroundLoadThread=class(TPasMPThread)
             private
              fScene:TpvScene;
              fEvent:TPasMPEvent;
             protected
              procedure Execute; override;
             public
              constructor Create(const aScene:TpvScene); reintroduce;
              destructor Destroy; override;
              procedure Shutdown;
              procedure WakeUp;
            end;
      private
       fRootNode:TpvSceneNode;
       fAllNodesLock:TPasMPSlimReaderWriterLock;
       fAllNodes:TpvSceneNodes;
       fCountToLoadNodes:TPasMPInt32;
       fBackgroundLoadThread:TBackgroundLoadThread;
       fData:TObject;
      public 
       fStartLoadVisitGeneration:TpvUInt32;
       fBackgroundLoadVisitGeneration:TpvUInt32;
       fFinishLoadVisitGeneration:TpvUInt32;
      public
       constructor Create(const aData:TObject=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure StartLoad; virtual;
       procedure BackgroundLoad; virtual;
       procedure FinishLoad; virtual;
       procedure WaitForLoaded; virtual;
       function IsLoaded:boolean; virtual;
       procedure LoadSynchronizationPoint; virtual;
       procedure Check; virtual;
       procedure Store; virtual;
       procedure BeginUpdate(const aDeltaTime:TpvDouble); virtual;
       procedure Update(const aDeltaTime:TpvDouble); virtual;
       procedure EndUpdate(const aDeltaTime:TpvDouble); virtual;
       procedure Interpolate(const aAlpha:TpvDouble); virtual;
       procedure FrameUpdate; virtual;
       procedure Render; virtual;
       procedure UpdateAudio; virtual;
       function Serialize:TObject; virtual;
       procedure Deserialize(const aData:TObject); virtual;
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

 if assigned(fParent) then begin
  fScene:=fParent.fScene;
 end else begin
  fScene:=nil;
 end;

 fData:=aData;

 fIndex:=-1;

 fChildren:=TpvSceneNodes.Create;
 fChildren.OwnsObjects:=true;

 fIncomingNodeDependencies:=TpvSceneNodes.Create;
 fIncomingNodeDependencies.OwnsObjects:=false;

 fOutgoingNodeDependencies:=TpvSceneNodes.Create;
 fOutgoingNodeDependencies.OwnsObjects:=false;

 fDestroying:=false;

 fStartLoadVisitGeneration:=0;
 fBackgroundLoadVisitGeneration:=0;
 fFinishLoadVisitGeneration:=0;

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

 FreeAndNil(fOutgoingNodeDependencies);

 FreeAndNil(fIncomingNodeDependencies);

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

procedure TpvSceneNode.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fScene) then begin
  fScene.fAllNodesLock.Acquire;
  try
   fIndex:=fScene.fAllNodes.Add(self);
  finally
   fScene.fAllNodesLock.Release;
  end; 
  TPasMPInterlocked.Increment(fScene.fCountToLoadNodes);
 end;
end;

procedure TpvSceneNode.BeforeDestruction;
var Index:TpvSizeInt;
begin
 if assigned(fScene) then begin
  if fIndex>=0 then begin
   try
    fScene.fAllNodesLock.Acquire;
    try
     Index:=fIndex;
     if Index=(fScene.fAllNodes.Count-1) then begin
      fScene.fAllNodes.Delete(Index);
     end else begin
      fScene.fAllNodes.Exchange(Index,fScene.fAllNodes.Count-1);
      fScene.fAllNodes.Delete(fScene.fAllNodes.Count-1);
      fScene.fAllNodes[Index].fIndex:=Index;
     end;
    finally
     fScene.fAllNodesLock.Release;
    end;
   finally
    fIndex:=-1;
   end; 
  end;
 end;
 if fOutgoingNodeDependencies.Count>0 then begin
  for Index:=fOutgoingNodeDependencies.Count-1 downto 0 do begin
   fOutgoingNodeDependencies[Index].RemoveDependency(self);
  end;
 end;
 if fIncomingNodeDependencies.Count>0 then begin
  for Index:=fIncomingNodeDependencies.Count-1 downto 0 do begin
   RemoveDependency(fIncomingNodeDependencies[Index]);
  end;
 end;
 inherited BeforeDestruction;
end;

procedure TpvSceneNode.AddDependency(const aNode:TpvSceneNode);
begin

 if assigned(aNode) then begin

  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fLock);
  try
   if assigned(fIncomingNodeDependencies) and not fIncomingNodeDependencies.Contains(aNode) then begin
    fIncomingNodeDependencies.Add(aNode);
   end;
  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fLock);
  end;

  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(aNode.fLock);
  try
   if assigned(aNode.fOutgoingNodeDependencies) and not aNode.fOutgoingNodeDependencies.Contains(self) then begin
    aNode.fOutgoingNodeDependencies.Add(self);
   end;
  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(aNode.fLock);
  end;

 end;

end;

procedure TpvSceneNode.RemoveDependency(const aNode:TpvSceneNode);
var Index:TpvSizeInt;
begin

 if assigned(aNode) then begin

  if assigned(fIncomingNodeDependencies) then begin
   TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fLock);
   try
    Index:=fIncomingNodeDependencies.IndexOf(aNode);
    if Index>=0 then begin
     fIncomingNodeDependencies.Delete(Index);
    end;
   finally
    TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fLock);
   end;
  end;

  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(aNode.fLock);
  try
   if assigned(aNode.fOutgoingNodeDependencies) then begin
    Index:=aNode.fOutgoingNodeDependencies.IndexOf(self);
    if Index>=0 then begin
     aNode.fOutgoingNodeDependencies.Delete(Index);
    end;
   end;
  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(aNode.fLock);
  end;

 end;

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

procedure TpvSceneNode.BeforeStartLoad;
begin
end;

procedure TpvSceneNode.StartLoad;
begin
end;

procedure TpvSceneNode.AfterStartLoad;
var OldState:TpvSceneNodeState;
begin
 OldState:=TPasMPInterlocked.Read(fState);
 if (OldState=TpvSceneNodeState.Unloaded) or (OldState=TpvSceneNodeState.StartLoading) then begin
  TPasMPInterlocked.CompareExchange(fState,TpvSceneNodeState.StartLoaded,OldState);
 end;  
end;

procedure TpvSceneNode.BeforeBackgroundLoad;
begin  
end;

procedure TpvSceneNode.BackgroundLoad;
begin
end;

procedure TpvSceneNode.AfterBackgroundLoad;
var OldState:TpvSceneNodeState;
begin
 OldState:=TPasMPInterlocked.Read(fState);
 if (OldState=TpvSceneNodeState.StartLoaded) or (OldState=TpvSceneNodeState.BackgroundLoading) then begin
  TPasMPInterlocked.CompareExchange(fState,TpvSceneNodeState.BackgroundLoaded,OldState);
 end;  
end;

procedure TpvSceneNode.BeforeFinishLoad;
begin
end;

procedure TpvSceneNode.FinishLoad;
begin
end;

procedure TpvSceneNode.AfterFinishLoad;
var OldState:TpvSceneNodeState;
begin
 OldState:=TPasMPInterlocked.Read(fState);
 if ((OldState=TpvSceneNodeState.BackgroundLoaded) or (OldState=TpvSceneNodeState.Loading)) and
    (TPasMPInterlocked.CompareExchange(fState,TpvSceneNodeState.Loaded,OldState)=OldState) then begin
  if assigned(fScene) then begin
   TPasMPInterlocked.Decrement(fScene.fCountToLoadNodes);
  end;
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

procedure TpvSceneNode.Check;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 if fState=TpvSceneNodeState.Loaded then begin
  for ChildNodeIndex:=0 to fChildren.Count-1 do begin
   ChildNode:=fChildren[ChildNodeIndex];
   if assigned(ChildNode) and (ChildNode.fState=TpvSceneNodeState.Loaded) then begin
    ChildNode.Check;
   end;
  end;
 end;
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

function TpvSceneNode.Serialize:TObject;
begin
 result:=nil;
end;

procedure TpvSceneNode.Deserialize(const aData:TObject);
begin
end;

{ TpvScene.TBackgroundLoadThread }

constructor TpvScene.TBackgroundLoadThread.Create(const aScene:TpvScene);
begin
 fScene:=aScene;
 fEvent:=TPasMPEvent.Create(nil,false,false,'');
 inherited Create(false);
end;

destructor TpvScene.TBackgroundLoadThread.Destroy;
begin
 Shutdown;
 FreeAndNil(fEvent);
 inherited Destroy;
end;

procedure TpvScene.TBackgroundLoadThread.Shutdown;
begin
 if not Finished then begin
  Terminate;
  fEvent.SetEvent;
  WaitFor;
 end;
end;

procedure TpvScene.TBackgroundLoadThread.WakeUp;
begin
 fEvent.SetEvent;
end;

procedure TpvScene.TBackgroundLoadThread.Execute;
begin
 while not Terminated do begin
  fEvent.WaitFor;
  if Terminated then begin
   break;
  end else begin 
   if TPasMPInterlocked.Read(fScene.fCountToLoadNodes)>0 then begin
    fScene.BackgroundLoad;
   end else begin
    Sleep(0);
   end;
  end;
 end;
end;

{ TpvScene }

constructor TpvScene.Create(const aData:TObject=nil);
begin
 inherited Create;

 fAllNodesLock:=TPasMPSlimReaderWriterLock.Create;

 fAllNodes:=TpvSceneNodes.Create(false);

 fRootNode:=TpvSceneNode.Create(nil);
 fRootNode.fScene:=self;

 fCountToLoadNodes:=1;

 fData:=aData;

 fStartLoadVisitGeneration:=1;
 fBackgroundLoadVisitGeneration:=1;
 fFinishLoadVisitGeneration:=1;

 fBackgroundLoadThread:=TBackgroundLoadThread.Create(self);

end;

destructor TpvScene.Destroy;
begin
 fBackgroundLoadThread.Shutdown;
 FreeAndNil(fBackgroundLoadThread);
 FreeAndNil(fRootNode);
 FreeAndNil(fAllNodes);
 FreeAndNil(fAllNodesLock);
 inherited Destroy;
end;

procedure TpvScene.StartLoad;
type TStackItem=record
      Node:TpvSceneNode;
      Pass:TpvSizeInt;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicFastStack<TStackItem>;
var Index,Pass:TpvSizeInt;
    Stack:TStack;
    NewStackItem:PStackItem;
    CurrentStackItem:TStackItem;
    Node:TpvSceneNode;
begin
 Stack.Initialize;
 try
  NewStackItem:=Pointer(Stack.PushIndirect);
  NewStackItem^.Node:=fRootNode;
  NewStackItem^.Pass:=0;
  while Stack.Pop(CurrentStackItem) do begin
   Node:=CurrentStackItem.Node;
   Pass:=CurrentStackItem.Pass;
   repeat
    case Pass of
     0:begin
      if Node.fStartLoadVisitGeneration<>fStartLoadVisitGeneration then begin
       Node.fStartLoadVisitGeneration:=fStartLoadVisitGeneration;
       if Node.fIncomingNodeDependencies.Count>0 then begin
        NewStackItem:=Pointer(Stack.PushIndirect);
        NewStackItem^.Node:=Node;
        NewStackItem^.Pass:=1;
        for Index:=Node.fIncomingNodeDependencies.Count-1 downto 0 do begin
         NewStackItem:=Pointer(Stack.PushIndirect);
         NewStackItem^.Node:=Node.fIncomingNodeDependencies[Index];
         NewStackItem^.Pass:=0;
        end;       
       end else begin
        Pass:=1;
        continue;
       end;
      end; 
     end;
     1:begin     
      if TPasMPInterlocked.Read(Node.fState)=TpvSceneNodeState.Unloaded then begin
       NewStackItem:=Pointer(Stack.PushIndirect);
       NewStackItem^.Node:=Node;
       NewStackItem^.Pass:=2;
       try
        Node.BeforeStartLoad;
       except
        on e:Exception do begin
         pvApplication.Log(LOG_ERROR,ClassName+'.StartLoad',DumpExceptionCallStack(e));
         if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.Failed,TpvSceneNodeState.Unloaded)=TpvSceneNodeState.Unloaded then begin
          TPasMPInterlocked.Decrement(fCountToLoadNodes);
         end;
        end;
       end;
      end;
      if Node.Children.Count>0 then begin
       for Index:=Node.Children.Count-1 downto 0 do begin
        NewStackItem:=Pointer(Stack.PushIndirect);
        NewStackItem^.Node:=Node.Children[Index];
        NewStackItem^.Pass:=0;
       end;
      end;
     end;
     2:begin
      if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.StartLoading,TpvSceneNodeState.Unloaded)=TpvSceneNodeState.Unloaded then begin
       try
        Node.StartLoad;
        Node.AfterStartLoad;
       except
        on e:Exception do begin
         pvApplication.Log(LOG_ERROR,ClassName+'.StartLoad',DumpExceptionCallStack(e));
         if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.Failed,TpvSceneNodeState.StartLoading)=TpvSceneNodeState.StartLoading then begin
          TPasMPInterlocked.Decrement(fCountToLoadNodes);
         end;
        end;
       end;
      end;
     end;    
    end;
    break;
   until false;
  end;
 finally
  Stack.Finalize;
 end;
 inc(fStartLoadVisitGeneration);
end;

procedure TpvScene.BackgroundLoad;
type TStackItem=record
      Node:TpvSceneNode;
      Pass:TpvSizeInt;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicFastStack<TStackItem>;
var Index,Pass:TpvSizeInt;
    Stack:TStack;
    NewStackItem:PStackItem;
    CurrentStackItem:TStackItem;
    Node:TpvSceneNode;
begin
 Stack.Initialize;
 try
  NewStackItem:=Pointer(Stack.PushIndirect);
  NewStackItem^.Node:=fRootNode;
  NewStackItem^.Pass:=0;
  while Stack.Pop(CurrentStackItem) do begin
   Node:=CurrentStackItem.Node;
   Pass:=CurrentStackItem.Pass;
   repeat
    case Pass of
     0:begin
      if Node.fBackgroundLoadVisitGeneration<>fBackgroundLoadVisitGeneration then begin
       Node.fBackgroundLoadVisitGeneration:=fBackgroundLoadVisitGeneration;
       if Node.fIncomingNodeDependencies.Count>0 then begin
        NewStackItem:=Pointer(Stack.PushIndirect);
        NewStackItem^.Node:=Node;
        NewStackItem^.Pass:=1;
        for Index:=Node.fIncomingNodeDependencies.Count-1 downto 0 do begin
         NewStackItem:=Pointer(Stack.PushIndirect);
         NewStackItem^.Node:=Node.fIncomingNodeDependencies[Index];
         NewStackItem^.Pass:=0;
        end;       
       end else begin
        Pass:=1;
        continue;
       end;
      end; 
     end;
     1:begin     
      if TPasMPInterlocked.Read(Node.fState)=TpvSceneNodeState.StartLoaded then begin
       NewStackItem:=Pointer(Stack.PushIndirect);
       NewStackItem^.Node:=Node;
       NewStackItem^.Pass:=2;
       try
        Node.BeforeBackgroundLoad;
       except
        on e:Exception do begin
         pvApplication.Log(LOG_ERROR,ClassName+'.BackgroundLoad',DumpExceptionCallStack(e));
         if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.Failed,TpvSceneNodeState.StartLoaded)=TpvSceneNodeState.StartLoaded then begin
          TPasMPInterlocked.Decrement(fCountToLoadNodes);
         end;
        end;
       end;
      end;
      if Node.Children.Count>0 then begin
       for Index:=Node.Children.Count-1 downto 0 do begin
        NewStackItem:=Pointer(Stack.PushIndirect);
        NewStackItem^.Node:=Node.Children[Index];
        NewStackItem^.Pass:=0;
       end;
      end;
     end;
     2:begin
      if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.BackgroundLoading,TpvSceneNodeState.StartLoaded)=TpvSceneNodeState.StartLoaded then begin
       try
        Node.BackgroundLoad;
        Node.AfterBackgroundLoad;
       except
        on e:Exception do begin
         pvApplication.Log(LOG_ERROR,ClassName+'.BackgroundLoad',DumpExceptionCallStack(e));
         if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.Failed,TpvSceneNodeState.BackgroundLoading)=TpvSceneNodeState.BackgroundLoading then begin
          TPasMPInterlocked.Decrement(fCountToLoadNodes);
         end;
        end;
       end;
      end;
     end;
    end;
    break;
   until false;
  end;
 finally
  Stack.Finalize;
 end;
 inc(fBackgroundLoadVisitGeneration);
end;

procedure TpvScene.FinishLoad;
type TStackItem=record
      Node:TpvSceneNode;
      Pass:TpvSizeInt;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicFastStack<TStackItem>;
var Index,Pass:TpvSizeInt;
    Stack:TStack;
    NewStackItem:PStackItem;
    CurrentStackItem:TStackItem;
    Node:TpvSceneNode;
begin
 Stack.Initialize;
 try
  NewStackItem:=Pointer(Stack.PushIndirect);
  NewStackItem^.Node:=fRootNode;
  NewStackItem^.Pass:=0;
  while Stack.Pop(CurrentStackItem) do begin
   Node:=CurrentStackItem.Node;
   Pass:=CurrentStackItem.Pass;
   repeat
    case Pass of
     0:begin
      if Node.fFinishLoadVisitGeneration<>fFinishLoadVisitGeneration then begin
       Node.fFinishLoadVisitGeneration:=fFinishLoadVisitGeneration;
       if Node.fIncomingNodeDependencies.Count>0 then begin
        NewStackItem:=Pointer(Stack.PushIndirect);
        NewStackItem^.Node:=Node;
        NewStackItem^.Pass:=1;
        for Index:=Node.fIncomingNodeDependencies.Count-1 downto 0 do begin
         NewStackItem:=Pointer(Stack.PushIndirect);
         NewStackItem^.Node:=Node.fIncomingNodeDependencies[Index];
         NewStackItem^.Pass:=0;
        end;       
       end else begin
        Pass:=1;
        continue;
       end;
      end;
     end;
     1:begin     
      if TPasMPInterlocked.Read(Node.fState)=TpvSceneNodeState.BackgroundLoaded then begin
       NewStackItem:=Pointer(Stack.PushIndirect);
       NewStackItem^.Node:=Node;
       NewStackItem^.Pass:=2;
       try
        Node.BeforeFinishLoad;
       except
        on e:Exception do begin
         pvApplication.Log(LOG_ERROR,ClassName+'.FinishLoad',DumpExceptionCallStack(e));
         if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.Failed,TpvSceneNodeState.BackgroundLoaded)=TpvSceneNodeState.BackgroundLoaded then begin
          TPasMPInterlocked.Decrement(fCountToLoadNodes);
         end;
        end;
       end;
      end;
      if Node.Children.Count>0 then begin
       for Index:=Node.Children.Count-1 downto 0 do begin
        NewStackItem:=Pointer(Stack.PushIndirect);
        NewStackItem^.Node:=Node.Children[Index];
        NewStackItem^.Pass:=0;
       end;
      end;
     end;
     2:begin
      if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.Loading,TpvSceneNodeState.BackgroundLoaded)=TpvSceneNodeState.BackgroundLoaded then begin
       try
        Node.FinishLoad;
        Node.AfterFinishLoad;
       except
        on e:Exception do begin
         pvApplication.Log(LOG_ERROR,ClassName+'.FinishLoad',DumpExceptionCallStack(e));
         if TPasMPInterlocked.CompareExchange(Node.fState,TpvSceneNodeState.Failed,TpvSceneNodeState.Loading)=TpvSceneNodeState.Loading then begin
          TPasMPInterlocked.Decrement(fCountToLoadNodes);
         end;
        end;
       end;
      end;
     end;
    end;
    break;
   until false;
  end;
 finally
  Stack.Finalize;
 end;
 inc(fFinishLoadVisitGeneration);
end;

procedure TpvScene.WaitForLoaded;
begin
 fRootNode.WaitForLoaded;
end;

function TpvScene.IsLoaded:boolean;
begin
 result:=fRootNode.IsLoaded;
end;

procedure TpvScene.LoadSynchronizationPoint;
begin
 if TPasMPInterlocked.Read(fCountToLoadNodes)>0 then begin
  StartLoad;
  fBackgroundLoadThread.WakeUp;
  FinishLoad;
 end;
end;

procedure TpvScene.Check;
begin
 fRootNode.Check;
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

function TpvScene.Serialize:TObject;
begin
 result:=nil;
end;

procedure TpvScene.Deserialize(const aData:TObject);
begin
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
