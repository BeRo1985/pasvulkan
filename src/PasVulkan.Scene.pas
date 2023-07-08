(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2023, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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

uses Classes,SysUtils,PasMP,PasVulkan.Types,PasVulkan.Collections;

type TpvScene=class;

     TpvSceneNode=class;

     TpvSceneNodeClass=class of TpvSceneNode;

     TpvSceneNodes=TpvObjectGenericList<TpvSceneNode>;

     TpvSceneNodeHashMap=TpvHashMap<TpvSceneNodeClass,TpvSceneNodes>;

     // A scene node can be an entity or even a component for an entity as well, here is no distinction for simplicity, for the contrast to 
     // the entity-component-system pattern, which is also implemented in the PasVulkan framework, see the PasVulkan.EntityComponentSystem.pas unit.
     // So it's your choice, if you want to use the entity-component-system pattern or the scene graph pattern or both.

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
       fLoaded:boolean;
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
       procedure BackgroundLoad; virtual;
       procedure FinishLoad; virtual;
       procedure WaitForLoaded; virtual;
       function IsLoaded:boolean; virtual;
       procedure Store; virtual;
       procedure Update(const aDeltaTime:TpvDouble); virtual;
       procedure Interpolate(const aAlpha:TpvDouble); virtual;
       procedure Render; virtual;
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
      published
       property RootNode:TpvSceneNode read fRootNode;
       property Data:TObject read fData;
     end;

implementation

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

 fLoaded:=false;

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
begin
end;

procedure TpvSceneNode.BackgroundLoad;
begin
end;

procedure TpvSceneNode.FinishLoad;
begin
end;

procedure TpvSceneNode.WaitForLoaded;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.WaitForLoaded;
 end;
 while not fLoaded do begin
  Sleep(1);
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
 result:=fLoaded;
end;

procedure TpvSceneNode.Store;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.Store;
 end;
end;

procedure TpvSceneNode.Update(const aDeltaTime:TpvDouble);
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.Update(aDeltaTime);
 end;
end;

procedure TpvSceneNode.Interpolate(const aAlpha:TpvDouble);
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.Interpolate(aAlpha);
 end;
end;

procedure TpvSceneNode.Render;
var ChildNodeIndex:TpvSizeInt;
    ChildNode:TpvSceneNode;
begin
 for ChildNodeIndex:=0 to fChildren.Count-1 do begin
  ChildNode:=fChildren[ChildNodeIndex];
  ChildNode.Render;
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

end.

