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
unit PasVulkan.GUI;
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
     Math,
     Generics.Collections,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Collections,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Sprites,
     PasVulkan.Canvas;

type TPasVulkanGUIInstance=class;

     TPasVulkanGUIObject=class;

     TPasVulkanGUIWidget=class;

     TPasVulkanGUIObjectList=class(TObjectList<TPasVulkanGUIObject>);

     TPasVulkanGUIObject=class(TPersistent)
      private
       fInstance:TPasVulkanGUIInstance;
       fParent:TPasVulkanGUIObject;
       fChildren:TPasVulkanGUIObjectList;
      public
       constructor Create(const aParent:TPasVulkanGUIObject=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
      published
       property Instance:TPasVulkanGUIInstance read fInstance;
       property Parent:TPasVulkanGUIObject read fParent write fParent;
       property Children:TPasVulkanGUIObjectList read fChildren;
     end;

     TPasVulkanGUILayout=class(TPasVulkanGUIObject)
      protected
       function GetPreferredSize(const aWidget:TPasVulkanGUIWidget):TpvVector2; virtual;
      public
     end;

     TPasVulkanGUIWidget=class(TPasVulkanGUIObject)
      private
       fLayout:TPasVulkanGUILayout;
       fPosition:TpvVector2;
       fSize:TpvVector2;
       fPositionProperty:TpvVector2Property;
       fSizeProperty:TpvVector2Property;
       fVisible:boolean;
       fEnabled:boolean;
       function GetAbsolutePosition:TpvVector2; {$ifdef CAN_INLINE}inline;{$endif}
      protected
       procedure Paint; virtual;
      public
       constructor Create(const aParent:TPasVulkanGUIObject=nil); override;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
      public
       property AbsolutePosition:TpvVector2 read GetAbsolutePosition;
      published
       property Position:TpvVector2Property read fPositionProperty;
       property Size:TpvVector2Property read fSizeProperty;
       property Visible:boolean read fVisible write fVisible;
       property Enabled:boolean read fEnabled write fEnabled;
     end;

     TPasVulkanGUIInstance=class(TPasVulkanGUIWidget)
      private
       fCanvas:TpvCanvas;
      public
       constructor Create(const aCanvas:TpvCanvas); reintroduce;
       destructor Destroy; override;
      published
       property Canvas:TpvCanvas read fCanvas;
     end;

implementation

function TPasVulkanGUILayout.GetPreferredSize(const aWidget:TPasVulkanGUIWidget):TpvVector2;
begin
 result:=TpvVector2.Create(0.0,0.0);
end;

constructor TPasVulkanGUIObject.Create(const aParent:TPasVulkanGUIObject=nil);
begin

 inherited Create;

 if assigned(aParent) then begin
  fInstance:=aParent.fInstance;
 end else if self is TPasVulkanGUIInstance then begin
  fInstance:=TPasVulkanGUIInstance(self);
 end else begin
  fInstance:=nil;
 end;

 fParent:=aParent;

 fChildren:=TPasVulkanGUIObjectList.Create(true);

end;

destructor TPasVulkanGUIObject.Destroy;
begin
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
  fParent.fChildren.Extract(self);
 end;
 inherited BeforeDestruction;
end;

constructor TPasVulkanGUIWidget.Create(const aParent:TPasVulkanGUIObject=nil);
begin
 inherited Create(aParent);
 fLayout:=nil;
 fPosition:=TpvVector2.Create(0.0,0.0);
 fSize:=TpvVector2.Create(1.0,1.0);
 fVisible:=true;
 fEnabled:=true;
end;

destructor TPasVulkanGUIWidget.Destroy;
begin
 FreeAndNil(fPositionProperty);
 FreeAndNil(fSizeProperty);
 inherited Destroy;
end;

procedure TPasVulkanGUIWidget.AfterConstruction;
begin
 inherited AfterConstruction;
end;

procedure TPasVulkanGUIWidget.BeforeDestruction;
begin
 inherited BeforeDestruction;
end;

function TPasVulkanGUIWidget.GetAbsolutePosition:TpvVector2;
begin
 if assigned(fParent) and (fParent is TPasVulkanGUIWidget) then begin
  result:=(fParent as TPasVulkanGUIWidget).AbsolutePosition+fPosition;
 end else begin
  result:=fPosition;
 end;
end;

procedure TPasVulkanGUIWidget.Paint;
begin

end;

constructor TPasVulkanGUIInstance.Create(const aCanvas:TpvCanvas);
begin

 inherited Create(nil);

 fInstance:=self;

 fCanvas:=aCanvas;

end;

destructor TPasVulkanGUIInstance.Destroy;
begin

 inherited Destroy;

end;

end.
