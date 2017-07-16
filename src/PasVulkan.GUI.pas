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
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >= 3.0 *
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
unit PasVulkan.GUI;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,Classes,Vulkan,PasVulkan.Framework,PasVulkan.Application;

type TPasVulkanGUIObject=class;

     TPasVulkanGUIObjectList=class(TList)
      private
       function GetItem(const aIndex:TVkInt32):TPasVulkanGUIObject;
       procedure SetItem(const aIndex:TVkInt32;const aItem:TPasVulkanGUIObject);
      public
       procedure ClearObjects;
       property Items[const aIndex:TVkInt32]:TPasVulkanGUIObject read GetItem write SetItem; default;
     end;

     TPasVulkanGUIObject=class
      private
       fParent:TPasVulkanGUIObject;
       fChildren:TPasVulkanGUIObjectList;
      public
       constructor Create(const aParent:TPasVulkanGUIObject=nil); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
      published
       property Parent:TPasVulkanGUIObject read fParent write fParent;
       property Children:TPasVulkanGUIObjectList read fChildren;
     end;

     TPasVulkanGUIInstance=class(TPasVulkanGUIObject)
      public
     end;

implementation

function TPasVulkanGUIObjectList.GetItem(const aIndex:TVkInt32):TPasVulkanGUIObject;
begin
 result:=inherited Items[aIndex];
end;

procedure TPasVulkanGUIObjectList.SetItem(const aIndex:TVkInt32;const aItem:TPasVulkanGUIObject);
begin
 inherited Items[aIndex]:=aItem;
end;

procedure TPasVulkanGUIObjectList.ClearObjects;
var CurrentObject:TPasVulkanGUIObject;
begin
 while Count>0 do begin
  CurrentObject:=Items[Count-1];
  Delete(Count-1);
  CurrentObject.Free;
 end;
end;

constructor TPasVulkanGUIObject.Create(const aParent:TPasVulkanGUIObject=nil);
begin

 inherited Create;

 fParent:=aParent;

 fChildren:=TPasVulkanGUIObjectList.Create;

end;

destructor TPasVulkanGUIObject.Destroy;
begin

 fChildren.ClearObjects;
 FreeAndNil(fChildren);

 inherited Destroy;

end;

procedure TPasVulkanGUIObject.AfterConstruction;
begin

 inherited AfterConstruction;

 if assigned(fParent) then begin
  fParent.fChildren.Add(self);
 end;

end;

procedure TPasVulkanGUIObject.BeforeDestruction;
begin

 if assigned(fParent) then begin
  fParent.fChildren.Remove(self);
 end;

 inherited BeforeDestruction;

end;

end.
