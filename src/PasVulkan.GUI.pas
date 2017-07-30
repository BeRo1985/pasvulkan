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

     TPasVulkanGUIObjectList=class(TObjectList<TPasVulkanGUIObject>);

     PPasVulkanGUIAlignment=^TPasVulkanGUIAlignment;
     TPasVulkanGUIAlignment=
      (
       pvgaTop,
       pvgaBottom,
       pvgaLeft,
       pvgaRight,
       pvgaClient,
       pvgaNone
      );

     TPasVulkanGUIObject=class(TPersistent)
      private
       fInstance:TPasVulkanGUIInstance;
       fParent:TPasVulkanGUIObject;
       fChildren:TPasVulkanGUIObjectList;
       fAlignment:TPasVulkanGUIAlignment;
       fAutoSize:boolean;
       fLastPosition:TpvVector2;
       fLastSize:TpvVector2;
       fLastTotalSize:TpvVector2;
       fPosition:TpvVector2;
       fSize:TpvVector2;
       fTotalSize:TpvVector2;
       fMargin:TpvVector4;
       fPadding:TpvVector4;
       fPositionProperty:TpvVector2Property;
       fSizeProperty:TpvVector2Property;
       fTotalSizeProperty:TpvVector2Property;
       fMarginProperty:TpvVector4Property;
       fPaddingProperty:TpvVector4Property;
      protected
       procedure Prepare; virtual;
      public
       constructor Create(const aParent:TPasVulkanGUIObject=nil); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Paint; virtual;
      published
       property Instance:TPasVulkanGUIInstance read fInstance;
       property Parent:TPasVulkanGUIObject read fParent write fParent;
       property Children:TPasVulkanGUIObjectList read fChildren;
       property Alignment:TPasVulkanGUIAlignment read fAlignment write fAlignment;
       property AutoSize:boolean read fAutoSize write fAutoSize;
       property Position:TpvVector2Property read fPositionProperty;
       property Size:TpvVector2Property read fSizeProperty;
       property TotalSize:TpvVector2Property read fTotalSizeProperty;
       property Margin:TpvVector4Property read fMarginProperty;
       property Padding:TpvVector4Property read fPaddingProperty;
     end;

     TPasVulkanGUIInstance=class(TPasVulkanGUIObject)
      private
       fCanvas:TpvCanvas;
      public
       constructor Create(const aCanvas:TpvCanvas); reintroduce;
       destructor Destroy; override;
      published
       property Canvas:TpvCanvas read fCanvas;
     end;

implementation

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

 fLastPosition:=TpvVector2.Create(Infinity,Infinity);

 fLastSize:=TpvVector2.Create(-Infinity,-Infinity);

 fLastTotalSize:=TpvVector2.Create(Infinity,-Infinity);

 fPosition:=TpvVector2.Create(0.0,0.0);

 fSize:=TpvVector2.Create(1.0,1.0);

 fTotalSize:=TpvVector2.Create(-Infinity,Infinity);

 fMargin:=TpvVector4.Create(0.0,0.0,0.0,0.0);

 fPadding:=TpvVector4.Create(0.0,0.0,0.0,0.0);

 fPositionProperty:=TpvVector2Property.Create(@fPosition);

 fSizeProperty:=TpvVector2Property.Create(@fSize);

 fTotalSizeProperty:=TpvVector2Property.Create(@fTotalSize);

 fMarginProperty:=TpvVector4Property.Create(@fMargin);

 fPaddingProperty:=TpvVector4Property.Create(@fPadding);

 fAlignment:=pvgaNone;

 fAutoSize:=false;

end;

destructor TPasVulkanGUIObject.Destroy;
begin

 fChildren.Clear;

 FreeAndNil(fChildren);

 FreeAndNil(fPositionProperty);

 FreeAndNil(fSizeProperty);

 FreeAndNil(fTotalSizeProperty);

 FreeAndNil(fMarginProperty);

 FreeAndNil(fPaddingProperty);

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

procedure TPasVulkanGUIObject.Prepare;
var TryIteration:TpvInt32;
    Child:TPasVulkanGUIObject;
    AABB:TpvAABB2D;
    AlignmentIndex:TPasVulkanGUIAlignment;
begin

 for TryIteration:=1 to 10 do begin

  if (fLastPosition<>fPosition) or (fLastSize<>fSize) then begin
   fLastPosition:=fPosition;
   fLastSize:=fSize;
  end;

  fTotalSize:=fSize+
              fMargin.xy+
              fMargin.zw+
              fPadding.xy+
              fPadding.zw;

  if fLastTotalSize<>fTotalSize then begin
   fLastTotalSize:=fTotalSize;
  end;

  for AlignmentIndex:=low(TPasVulkanGUIAlignment) to high(TPasVulkanGUIAlignment) do begin
   for Child in fChildren do begin
    if Child.fAlignment=AlignmentIndex then begin
     case AlignmentIndex of
      pvgaTop:begin

      end;
      pvgaBottom:begin

      end;
      pvgaLeft:begin

      end;
      pvgaRight:begin

      end;
      pvgaClient:begin

      end;
      pvgaNone:begin

      end;
     end;
    end;
   end;
  end;

  for Child in fChildren do begin
   Child.Prepare;
  end;

  if fAutoSize then begin

   AABB:=TpvAABB2D.Create(Infinity,Infinity,-Infinity,-Infinity);

   for Child in fChildren do begin
    AABB.Min.x:=Min(AABB.Min.x,Child.fPosition.x);
    AABB.Min.y:=Min(AABB.Min.y,Child.fPosition.y);
    AABB.Max.x:=Max(AABB.Max.x,Child.fPosition.x+Child.fTotalSize.x);
    AABB.Max.y:=Max(AABB.Max.y,Child.fPosition.y+Child.fTotalSize.y);
   end;

   if (AABB.Min.x>=AABB.Max.x) or
      (AABB.Min.y>=AABB.Max.y) then begin
    AABB:=TpvAABB2D.Create(0.0,0.0,1.0,1.0);
   end;

   if (not SameValue(fSize.x,AABB.Max.x)) or
      (not SameValue(fSize.y,AABB.Max.y)) then begin
    fSize.x:=AABB.Max.x;
    fSize.y:=AABB.Max.y;
    continue;
   end;

  end;

  break;

 end;

end;

procedure TPasVulkanGUIObject.Paint;
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
