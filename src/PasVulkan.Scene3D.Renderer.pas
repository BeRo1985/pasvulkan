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
unit PasVulkan.Scene3D.Renderer;
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

uses Classes,
     SysUtils,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.TimerQuery,
     PasVulkan.Collections,
     PasVulkan.CircularDoublyLinkedList,
     PasVulkan.Scene3D;

type TpvScene3DRenderer=class;

     TpvScene3DRendererBaseObject=class;

     TpvScene3DRendererBaseObjects=class(TpvObjectGenericList<TpvScene3DRendererBaseObject>);

     TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode=class(TpvCircularDoublyLinkedListNode<TpvScene3DRendererBaseObject>);

     { TpvScene3DRendererBaseObject }
     TpvScene3DRendererBaseObject=class
      private
       fParent:TpvScene3DRendererBaseObject;
       fRenderer:TpvScene3DRenderer;
       fChildrenLock:TPasMPCriticalSection;
       fChildren:TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode;
       fOwnCircularDoublyLinkedListNode:TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode;
      public
       constructor Create(const aParent:TpvScene3DRendererBaseObject); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
     end;

     { TpvScene3DRenderer }
     TpvScene3DRenderer=class(TpvScene3DRendererBaseObject)
      public
       type { TInstance }
            TInstance=class(TpvScene3DRendererBaseObject)
             private
             public
              constructor Create(const aParent:TpvScene3DRendererBaseObject); reintroduce;
              destructor Destroy; override;
            end;
      private
       fScene3D:TpvScene3D;
      public
       constructor Create(const aScene3D:TpvScene3D); reintroduce;
       destructor Destroy; override;
      published
       property Scene3D:TpvScene3D read fScene3D;
     end;


implementation

{ TpvScene3DRendererBaseObject }

constructor TpvScene3DRendererBaseObject.Create(const aParent:TpvScene3DRendererBaseObject);
begin
 inherited Create;

 fParent:=aParent;
 if assigned(fParent) then begin
  if fParent is TpvScene3DRenderer then begin
   fRenderer:=TpvScene3DRenderer(fParent);
  end else begin
   fRenderer:=fParent.fRenderer;
  end;
 end else begin
  fRenderer:=nil;
 end;

 fOwnCircularDoublyLinkedListNode:=TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode.Create;
 fOwnCircularDoublyLinkedListNode.Value:=self;

 fChildrenLock:=TPasMPCriticalSection.Create;
 fChildren:=TpvScene3DRendererBaseObjectCircularDoublyLinkedListNode.Create;

end;

destructor TpvScene3DRendererBaseObject.Destroy;
var Child:TpvScene3DRendererBaseObject;
begin
 fChildrenLock.Acquire;
 try
  while fChildren.PopFromBack(Child) do begin
   FreeAndNil(Child);
  end;
 finally
  fChildrenLock.Release;
 end;
 FreeAndNil(fChildren);
 FreeAndNil(fChildrenLock);
 FreeAndNil(fOwnCircularDoublyLinkedListNode);
 inherited Destroy;
end;

procedure TpvScene3DRendererBaseObject.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fParent) then begin
  fParent.fChildrenLock.Acquire;
  try
   fParent.fChildren.Add(fOwnCircularDoublyLinkedListNode);
  finally
   fParent.fChildrenLock.Release;
  end;
 end;
end;

procedure TpvScene3DRendererBaseObject.BeforeDestruction;
begin
 if assigned(fParent) and not fOwnCircularDoublyLinkedListNode.IsEmpty then begin
  try
   fParent.fChildrenLock.Acquire;
   try
    if not fOwnCircularDoublyLinkedListNode.IsEmpty then begin
     fOwnCircularDoublyLinkedListNode.Remove;
    end;
   finally
    fParent.fChildrenLock.Release;
   end;
  finally
   fParent:=nil;
  end;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3DRenderer.TInstance }

constructor TpvScene3DRenderer.TInstance.Create(const aParent:TpvScene3DRendererBaseObject);
begin
 inherited Create(aParent);
end;

destructor TpvScene3DRenderer.TInstance.Destroy;
begin
 inherited Destroy;
end;

{ TpvScene3DRenderer }

constructor TpvScene3DRenderer.Create(const aScene3D:TpvScene3D);
begin
 inherited Create(nil);
 fScene3D:=aScene3D;
end;

destructor TpvScene3DRenderer.Destroy;
begin
 inherited Destroy;
end;

end.

