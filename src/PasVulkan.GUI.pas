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

type TPasVulkanGUIObject=class;

     TPasVulkanGUIWidget=class;

     TPasVulkanGUIInstance=class;

     TPasVulkanGUIWindow=class;

     TPasVulkanGUIObjectList=class(TObjectList<TPasVulkanGUIObject>);

     TPasVulkanGUIObject=class(TPersistent)
      private
       fInstance:TPasVulkanGUIInstance;
       fParent:TPasVulkanGUIObject;
       fChildren:TPasVulkanGUIObjectList;
       fID:TpvUTF8String;
       fTag:TpvPtrInt;
      public
       constructor Create(const aParent:TPasVulkanGUIObject=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
      published
       property Instance:TPasVulkanGUIInstance read fInstance;
       property Parent:TPasVulkanGUIObject read fParent write fParent;
       property Children:TPasVulkanGUIObjectList read fChildren;
       property ID:TpvUTF8String read fID write fID;
       property Tag:TpvPtrInt read fTag write fTag;
     end;

     TPasVulkanGUILayout=class(TPasVulkanGUIObject)
      protected
       function GetPreferredSize(const aWidget:TPasVulkanGUIWidget):TpvVector2; virtual;
       procedure PerformLayout(const aWidget:TPasVulkanGUIWidget); virtual;
      public
     end;

     TPasVulkanGUITheme=class(TPasVulkanGUIObject)
      private
       fFontSize:TpvFloat;
      protected
      public
     end;

     TPasVulkanGUICursor=class(TPasVulkanGUIObject)
      protected
      public
     end;

     TPasVulkanGUIWidgetEnumerator=class(TEnumerator<TPasVulkanGUIWidget>)
      private
       fWidget:TPasVulkanGUIWidget;
       fIndex:TpvSizeInt;
      protected
       function DoMoveNext:boolean; override;
       function DoGetCurrent:TPasVulkanGUIWidget; override;
      public
       constructor Create(const aWidget:TPasVulkanGUIWidget); reintroduce;
     end;

     TPasVulkanGUIWidget=class(TPasVulkanGUIObject)
      private
       fWindow:TPasVulkanGUIWindow;
       fLayout:TPasVulkanGUILayout;
       fTheme:TPasVulkanGUITheme;
       fCursor:TPasVulkanGUICursor;
       fPosition:TpvVector2;
       fSize:TpvVector2;
       fFixedSize:TpvVector2;
       fPositionProperty:TpvVector2Property;
       fSizeProperty:TpvVector2Property;
       fFixedSizeProperty:TpvVector2Property;
       fVisible:boolean;
       fEnabled:boolean;
       fFocused:boolean;
       fHint:TpvUTF8String;
       fFontSize:TpvFloat;
       function GetLeft:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetLeft(const aLeft:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetTop:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetTop(const aTop:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetWidth:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetWidth(const aWidth:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetHeight:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetHeight(const aHeight:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetFixedWidth:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetFixedWidth(const aFixedWidth:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetFixedHeight:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetFixedHeight(const aFixedHeight:TpvFloat); {$ifdef CAN_INLINE}inline;{$endif}
       function GetAbsolutePosition:TpvVector2; {$ifdef CAN_INLINE}inline;{$endif}
       function GetRecursiveVisible:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function GetPreferredSize:TpvVector2; {$ifdef CAN_INLINE}inline;{$endif}
       function GetFontSize:TpvFloat; {$ifdef CAN_INLINE}inline;{$endif}
      protected
       procedure SetTheme(const aTheme:TPasVulkanGUITheme); virtual;
       procedure PerformLayout; virtual;
      public
       constructor Create(const aParent:TPasVulkanGUIObject=nil); override;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       function GetEnumerator:TPasVulkanGUIWidgetEnumerator;
       function Contains(const aPosition:TpvVector2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function FindWidget(const aPosition:TpvVector2):TPasVulkanGUIWidget;
       procedure RequestFocus; virtual;
       procedure Draw; virtual;
       function KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean; virtual;
       function KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean; virtual;
       function KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean; virtual;
       function PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; virtual;
       function PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; virtual;
       function PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; virtual;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; virtual;
      public
       property AbsolutePosition:TpvVector2 read GetAbsolutePosition;
       property PreferredSize:TpvVector2 read GetPreferredSize;
      published
       property Window:TPasVulkanGUIWindow read fWindow write fWindow;
       property Layout:TPasVulkanGUILayout read fLayout write fLayout;
       property Theme:TPasVulkanGUITheme read fTheme write SetTheme;
       property Cursor:TPasVulkanGUICursor read fCursor write fCursor;
       property Position:TpvVector2Property read fPositionProperty;
       property Size:TpvVector2Property read fSizeProperty;
       property FixedSize:TpvVector2Property read fFixedSizeProperty;
       property Visible:boolean read fVisible write fVisible;
       property RecursiveVisible:boolean read GetRecursiveVisible;
       property Enabled:boolean read fEnabled write fEnabled;
       property Focused:boolean read fFocused write fFocused;
       property Left:TpvFloat read GetLeft write SetLeft;
       property Top:TpvFloat read GetTop write SetTop;
       property Width:TpvFloat read GetWidth write SetWidth;
       property Height:TpvFloat read GetHeight write SetHeight;
       property FixedWidth:TpvFloat read GetFixedWidth write SetFixedWidth;
       property FixedHeight:TpvFloat read GetFixedHeight write SetFixedHeight;
       property Hint:TpvUTF8String read fHint write fHint;
       property FontSize:TpvFloat read GetFontSize write fFontSize;
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

     TPasVulkanGUIWindow=class(TPasVulkanGUIWidget)
      public
     end;

implementation

function TPasVulkanGUILayout.GetPreferredSize(const aWidget:TPasVulkanGUIWidget):TpvVector2;
begin
 result:=aWidget.fSize;
end;

procedure TPasVulkanGUILayout.PerformLayout(const aWidget:TPasVulkanGUIWidget);
begin

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

 fID:='';

 fTag:=0;

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

constructor TPasVulkanGUIWidgetEnumerator.Create(const aWidget:TPasVulkanGUIWidget);
begin
 inherited Create;
 fWidget:=aWidget;
 fIndex:=-1;
end;

function TPasVulkanGUIWidgetEnumerator.DoMoveNext:boolean;
begin
 inc(fIndex);
 while (fIndex<fWidget.fChildren.Count) and not (fWidget.fChildren[fIndex] is TPasVulkanGUIWidget) do begin
  inc(fIndex);
 end;
 result:=(fWidget.fChildren.Count<>0) and (fIndex<fWidget.fChildren.Count);
end;

function TPasVulkanGUIWidgetEnumerator.DoGetCurrent:TPasVulkanGUIWidget;
begin
 result:=fWidget.fChildren[fIndex] as TPasVulkanGUIWidget;
end;

constructor TPasVulkanGUIWidget.Create(const aParent:TPasVulkanGUIObject=nil);
begin

 inherited Create(aParent);

 fWindow:=nil;

 fLayout:=nil;

 fTheme:=nil;

 fCursor:=nil;

 fPosition:=TpvVector2.Create(0.0,0.0);

 fSize:=TpvVector2.Create(1.0,1.0);

 fFixedSize:=TpvVector2.Create(-1.0,-1.0);

 fPositionProperty:=TpvVector2Property.Create(@fPosition);

 fSizeProperty:=TpvVector2Property(@fSize);

 fFixedSizeProperty:=TpvVector2Property(@fFixedSize);

 fVisible:=true;

 fEnabled:=true;

 fFocused:=false;

 fHint:='';

 fFontSize:=0.0;

end;

destructor TPasVulkanGUIWidget.Destroy;
begin

 FreeAndNil(fPositionProperty);

 FreeAndNil(fSizeProperty);

 FreeAndNil(fFixedSizeProperty);

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

procedure TPasVulkanGUIWidget.SetTheme(const aTheme:TPasVulkanGUITheme);
var ChildIndex:TpvInt32;
    Child:TPasVulkanGUIObject;
    ChildWidget:TPasVulkanGUIWidget;
begin
 fTheme:=aTheme;
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TPasVulkanGUIWidget then begin
   ChildWidget:=Child as TPasVulkanGUIWidget;
   ChildWidget.SetTheme(aTheme);
  end;
 end;
end;

function TPasVulkanGUIWidget.GetLeft:TpvFloat;
begin
 result:=fPosition.x;
end;

procedure TPasVulkanGUIWidget.SetLeft(const aLeft:TpvFloat);
begin
 fPosition.x:=aLeft;
end;

function TPasVulkanGUIWidget.GetTop:TpvFloat;
begin
 result:=fPosition.y;
end;

procedure TPasVulkanGUIWidget.SetTop(const aTop:TpvFloat);
begin
 fPosition.y:=aTop;
end;

function TPasVulkanGUIWidget.GetWidth:TpvFloat;
begin
 result:=fSize.x;
end;

procedure TPasVulkanGUIWidget.SetWidth(const aWidth:TpvFloat);
begin
 fSize.x:=aWidth;
end;

function TPasVulkanGUIWidget.GetHeight:TpvFloat;
begin
 result:=fSize.y;
end;

procedure TPasVulkanGUIWidget.SetHeight(const aHeight:TpvFloat);
begin
 fSize.y:=aHeight;
end;

function TPasVulkanGUIWidget.GetFixedWidth:TpvFloat;
begin
 result:=fFixedSize.x;
end;

procedure TPasVulkanGUIWidget.SetFixedWidth(const aFixedWidth:TpvFloat);
begin
 fFixedSize.x:=aFixedWidth;
end;

function TPasVulkanGUIWidget.GetFixedHeight:TpvFloat;
begin
 result:=fFixedSize.y;
end;

procedure TPasVulkanGUIWidget.SetFixedHeight(const aFixedHeight:TpvFloat);
begin
 fFixedSize.y:=aFixedHeight;
end;

function TPasVulkanGUIWidget.GetAbsolutePosition:TpvVector2;
begin
 if assigned(fParent) and (fParent is TPasVulkanGUIWidget) then begin
  result:=(fParent as TPasVulkanGUIWidget).AbsolutePosition+fPosition;
 end else begin
  result:=fPosition;
 end;
end;

function TPasVulkanGUIWidget.GetRecursiveVisible:boolean;
var CurrentWidget:TPasVulkanGUIWidget;
begin
 CurrentWidget:=self;
 repeat
  result:=CurrentWidget.Visible;
  if result and assigned(fParent) and (fParent is TPasVulkanGUIWidget) then begin
   CurrentWidget:=fParent as TPasVulkanGUIWidget;
  end else begin
   break;
  end;
 until false;
end;

function TPasVulkanGUIWidget.GetPreferredSize:TpvVector2;
begin
 if assigned(fLayout) then begin
  result:=fLayout.GetPreferredSize(self);
 end else begin
  result:=fSize;
 end;
end;

function TPasVulkanGUIWidget.GetFontSize:TpvFloat;
begin
 if assigned(fTheme) and IsZero(fFontSize) then begin
  result:=fTheme.fFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TPasVulkanGUIWidget.GetEnumerator:TPasVulkanGUIWidgetEnumerator;
begin
 result:=TPasVulkanGUIWidgetEnumerator.Create(self);
end;

function TPasVulkanGUIWidget.Contains(const aPosition:TpvVector2):boolean;
begin
 result:=(aPosition.x>=fPosition.x) and
         (aPosition.y>=fPosition.y) and
         (aPosition.x<(fPosition.x+fSize.x)) and
         (aPosition.y<(fPosition.y+fSize.y));
end;

function TPasVulkanGUIWidget.FindWidget(const aPosition:TpvVector2):TPasVulkanGUIWidget;
var ChildIndex:TpvInt32;
    Child:TPasVulkanGUIObject;
    ChildWidget:TPasVulkanGUIWidget;
begin
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TPasVulkanGUIWidget then begin
   ChildWidget:=Child as TPasVulkanGUIWidget;
   if ChildWidget.Visible and ChildWidget.Contains(aPosition-fPosition) then begin
    result:=ChildWidget.FindWidget(aPosition-fPosition);
    exit;
   end;
  end;
 end;
 if Contains(aPosition) then begin
  result:=self;
 end else begin
  result:=nil;
 end;
end;

procedure TPasVulkanGUIWidget.RequestFocus;
begin
end;

procedure TPasVulkanGUIWidget.PerformLayout;
var ChildIndex:TpvInt32;
    Child:TPasVulkanGUIObject;
    ChildWidget:TPasVulkanGUIWidget;
    ChildWidgetPreferredSize,ChildWidgetFixedSize,ChildWidgetSize:TpvVector2;
begin
 if assigned(fLayout) then begin
  fLayout.PerformLayout(self);
 end else begin
  for ChildIndex:=0 to fChildren.Count-1 do begin
   Child:=fChildren.Items[ChildIndex];
   if Child is TPasVulkanGUIWidget then begin
    ChildWidget:=Child as TPasVulkanGUIWidget;
    ChildWidgetPreferredSize:=ChildWidget.GetPreferredSize;
    ChildWidgetFixedSize:=ChildWidget.fFixedSize;
    if ChildWidgetFixedSize.x>0.0 then begin
     ChildWidgetSize.x:=ChildWidgetFixedSize.x;
    end else begin
     ChildWidgetSize.x:=ChildWidgetPreferredSize.x;
    end;
    if ChildWidgetFixedSize.y>0.0 then begin
     ChildWidgetSize.y:=ChildWidgetFixedSize.y;
    end else begin
     ChildWidgetSize.y:=ChildWidgetPreferredSize.y;
    end;
    ChildWidget.fSize:=ChildWidgetSize;
    ChildWidget.PerformLayout;
   end;
  end;
 end;
end;

procedure TPasVulkanGUIWidget.Draw;
begin

end;

function TPasVulkanGUIWidget.KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TPasVulkanGUIWidget.KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TPasVulkanGUIWidget.KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TPasVulkanGUIWidget.PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var ChildIndex:TpvInt32;
    Child:TPasVulkanGUIObject;
    ChildWidget:TPasVulkanGUIWidget;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TPasVulkanGUIWidget then begin
   ChildWidget:=Child as TPasVulkanGUIWidget;
   if ChildWidget.Visible and ChildWidget.Contains(aPosition-fPosition) then begin
    result:=ChildWidget.PointerDown(aPosition-fPosition,aPressure,aPointerID,aButton);
    if result then begin
     exit;
    end;
   end;
  end;
 end;
 if (aButton=BUTTON_LEFT) and not fFocused then begin
  RequestFocus;
 end;
 result:=false;
end;

function TPasVulkanGUIWidget.PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var ChildIndex:TpvInt32;
    Child:TPasVulkanGUIObject;
    ChildWidget:TPasVulkanGUIWidget;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TPasVulkanGUIWidget then begin
   ChildWidget:=Child as TPasVulkanGUIWidget;
   if ChildWidget.Visible and ChildWidget.Contains(aPosition-fPosition) then begin
    result:=ChildWidget.PointerUp(aPosition-fPosition,aPressure,aPointerID,aButton);
    if result then begin
     exit;
    end;
   end;
  end;
 end;
 result:=false;
end;

function TPasVulkanGUIWidget.PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var ChildIndex:TpvInt32;
    Child:TPasVulkanGUIObject;
    ChildWidget:TPasVulkanGUIWidget;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TPasVulkanGUIWidget then begin
   ChildWidget:=Child as TPasVulkanGUIWidget;
   if ChildWidget.Visible and ChildWidget.Contains(aPosition-fPosition) then begin
    result:=ChildWidget.PointerMotion(aPosition-fPosition,aRelativePosition,aPressure,aPointerID,aButton);
    if result then begin
     exit;
    end;
   end;
  end;
 end;
 result:=false;
end;

function TPasVulkanGUIWidget.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var ChildIndex:TpvInt32;
    Child:TPasVulkanGUIObject;
    ChildWidget:TPasVulkanGUIWidget;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TPasVulkanGUIWidget then begin
   ChildWidget:=Child as TPasVulkanGUIWidget;
   if ChildWidget.Visible and ChildWidget.Contains(aPosition-fPosition) then begin
    result:=ChildWidget.Scrolled(aPosition-fPosition,aRelativeAmount);
    if result then begin
     exit;
    end;
   end;
  end;
 end;
 result:=false;
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
