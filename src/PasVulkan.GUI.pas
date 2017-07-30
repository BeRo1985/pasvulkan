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

type TpvGUIObject=class;

     TpvGUIWidget=class;

     TpvGUIInstance=class;

     TpvGUIWindow=class;

     EpvGUIWidget=class(Exception);

     PpvGUIDelayedDeleteQueueItem=^TpvGUIDelayedDeleteQueueItem;
     TpvGUIDelayedDeleteQueueItem=record
      private
       fObject:TObject;
       fCounter:TpvInt32;
      public
       constructor Create(const aObject:TObject;const aCounter:TpvInt32);
       property TheObject:TObject read fObject write fObject;
       property Counter:TpvInt32 read fCounter write fCounter;
     end;

     TpvGUIDelayedDeleteQueue=class(TList<TpvGUIDelayedDeleteQueueItem>);

     TpvGUIObjectList=class(TObjectList<TpvGUIObject>)
      protected
       procedure Notify({$ifdef fpc}constref{$else}const{$endif} Value:TpvGUIObject;Action:TCollectionNotification); override;
      public
     end;

     TpvGUIObject=class(TPersistent)
      private
       fInstance:TpvGUIInstance;
       fParent:TpvGUIObject;
       fChildren:TpvGUIObjectList;
       fID:TpvUTF8String;
       fTag:TpvPtrInt;
      public
       constructor Create(const aParent:TpvGUIObject=nil); reintroduce; virtual;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release;
      published
       property Instance:TpvGUIInstance read fInstance;
       property Parent:TpvGUIObject read fParent write fParent;
       property Children:TpvGUIObjectList read fChildren;
       property ID:TpvUTF8String read fID write fID;
       property Tag:TpvPtrInt read fTag write fTag;
     end;

     TpvGUILayout=class(TpvGUIObject)
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; virtual;
       procedure PerformLayout(const aWidget:TpvGUIWidget); virtual;
      public
     end;

     TpvGUITheme=class(TpvGUIObject)
      private
       fFontSize:TpvFloat;
      protected
      public
     end;

     TpvGUICursor=class(TpvGUIObject)
      protected
      public
     end;

     TpvGUIWidgetEnumerator=class(TEnumerator<TpvGUIWidget>)
      private
       fWidget:TpvGUIWidget;
       fIndex:TpvSizeInt;
      protected
       function DoMoveNext:boolean; override;
       function DoGetCurrent:TpvGUIWidget; override;
      public
       constructor Create(const aWidget:TpvGUIWidget); reintroduce;
     end;

     TpvGUIWidget=class(TpvGUIObject)
      private
       fLayout:TpvGUILayout;
       fTheme:TpvGUITheme;
       fCursor:TpvGUICursor;
       fPosition:TpvVector2;
       fSize:TpvVector2;
       fFixedSize:TpvVector2;
       fPositionProperty:TpvVector2Property;
       fSizeProperty:TpvVector2Property;
       fFixedSizeProperty:TpvVector2Property;
       fVisible:boolean;
       fEnabled:boolean;
       fFocused:boolean;
       fPointerFocused:boolean;
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
       function GetWindow:TpvGUIWindow;
      protected
       procedure SetTheme(const aTheme:TpvGUITheme); virtual;
       procedure PerformLayout; virtual;
      public
       constructor Create(const aParent:TpvGUIObject=nil); override;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       function GetEnumerator:TpvGUIWidgetEnumerator;
       function Contains(const aPosition:TpvVector2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function FindWidget(const aPosition:TpvVector2):TpvGUIWidget;
       procedure RequestFocus; virtual;
       function Enter:boolean; virtual;
       function Leave:boolean; virtual;
       function PointerEnter:boolean; virtual;
       function PointerLeave:boolean; virtual;
       function KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean; virtual;
       function KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean; virtual;
       function KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean; virtual;
       function PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; virtual;
       function PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; virtual;
       function PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; virtual;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; virtual;
       procedure Update(const aDeltaTime:TpvDouble); virtual;
       procedure Draw; virtual;
      public
       property AbsolutePosition:TpvVector2 read GetAbsolutePosition;
       property PreferredSize:TpvVector2 read GetPreferredSize;
      published
       property Window:TpvGUIWindow read GetWindow;
       property Layout:TpvGUILayout read fLayout write fLayout;
       property Theme:TpvGUITheme read fTheme write SetTheme;
       property Cursor:TpvGUICursor read fCursor write fCursor;
       property Position:TpvVector2Property read fPositionProperty;
       property Size:TpvVector2Property read fSizeProperty;
       property FixedSize:TpvVector2Property read fFixedSizeProperty;
       property Visible:boolean read fVisible write fVisible;
       property RecursiveVisible:boolean read GetRecursiveVisible;
       property Enabled:boolean read fEnabled write fEnabled;
       property Focused:boolean read fFocused write fFocused;
       property PointerFocused:boolean read fPointerFocused write fPointerFocused;
       property Left:TpvFloat read GetLeft write SetLeft;
       property Top:TpvFloat read GetTop write SetTop;
       property Width:TpvFloat read GetWidth write SetWidth;
       property Height:TpvFloat read GetHeight write SetHeight;
       property FixedWidth:TpvFloat read GetFixedWidth write SetFixedWidth;
       property FixedHeight:TpvFloat read GetFixedHeight write SetFixedHeight;
       property Hint:TpvUTF8String read fHint write fHint;
       property FontSize:TpvFloat read GetFontSize write fFontSize;
     end;

     TpvGUIInstance=class(TpvGUIWidget)
      private
       fDelayedDeleteQueue:TpvGUIDelayedDeleteQueue;
       fCanvas:TpvCanvas;
      public
       constructor Create(const aCanvas:TpvCanvas); reintroduce;
       destructor Destroy; override;
       procedure ProcessDelayedDeleteQueue;
       procedure UpdateFocus(const aWidget:TpvGUIWidget);
       procedure Update(const aDeltaTime:TpvDouble); override;
       procedure Draw; override;
      published
       property Canvas:TpvCanvas read fCanvas;
     end;

     TpvGUIWindow=class(TpvGUIWidget)
      public
     end;

implementation

constructor TpvGUIDelayedDeleteQueueItem.Create(const aObject:TObject;const aCounter:TpvInt32);
begin
 fObject:=aObject;
 fCounter:=aCounter;
end;

procedure TpvGUIObjectList.Notify({$ifdef fpc}constref{$else}const{$endif} Value:TpvGUIObject;Action:TCollectionNotification);
begin
 if (Action=cnRemoved) and assigned(Value) and assigned(Value.fInstance) then begin
  Value.fInstance.fDelayedDeleteQueue.Add(TpvGUIDelayedDeleteQueueItem.Create(Value,0));
 end else begin
  inherited Notify(Value,Action);
 end;
end;

constructor TpvGUIObject.Create(const aParent:TpvGUIObject=nil);
begin

 inherited Create;

 if assigned(aParent) then begin
  fInstance:=aParent.fInstance;
 end else if self is TpvGUIInstance then begin
  fInstance:=TpvGUIInstance(self);
 end else begin
  fInstance:=nil;
 end;

 fParent:=aParent;

 fChildren:=TpvGUIObjectList.Create(true);

 fID:='';

 fTag:=0;

end;

destructor TpvGUIObject.Destroy;
begin
 FreeAndNil(fChildren);
 inherited Destroy;
end;

procedure TpvGUIObject.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fParent) then begin
  fParent.fChildren.Add(self);
 end;
end;

procedure TpvGUIObject.BeforeDestruction;
begin
 if assigned(fParent) then begin
  fParent.fChildren.Extract(self);
 end;
 inherited BeforeDestruction;
end;

procedure TpvGUIObject.Release;
begin
 if assigned(self) then begin
  if assigned(fInstance) and (fInstance<>self) then begin
   fInstance.fDelayedDeleteQueue.Add(TpvGUIDelayedDeleteQueueItem.Create(self,0));
  end else begin
   Free;
  end;
 end;
end;

function TpvGUILayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
begin
 result:=aWidget.fSize;
end;

procedure TpvGUILayout.PerformLayout(const aWidget:TpvGUIWidget);
begin

end;

constructor TpvGUIWidgetEnumerator.Create(const aWidget:TpvGUIWidget);
begin
 inherited Create;
 fWidget:=aWidget;
 fIndex:=-1;
end;

function TpvGUIWidgetEnumerator.DoMoveNext:boolean;
begin
 inc(fIndex);
 while (fIndex<fWidget.fChildren.Count) and not (fWidget.fChildren[fIndex] is TpvGUIWidget) do begin
  inc(fIndex);
 end;
 result:=(fWidget.fChildren.Count<>0) and (fIndex<fWidget.fChildren.Count);
end;

function TpvGUIWidgetEnumerator.DoGetCurrent:TpvGUIWidget;
begin
 result:=fWidget.fChildren[fIndex] as TpvGUIWidget;
end;

constructor TpvGUIWidget.Create(const aParent:TpvGUIObject=nil);
begin

 inherited Create(aParent);

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

 fPointerFocused:=false;

 fHint:='';

 fFontSize:=0.0;

end;

destructor TpvGUIWidget.Destroy;
begin

 FreeAndNil(fPositionProperty);

 FreeAndNil(fSizeProperty);

 FreeAndNil(fFixedSizeProperty);

 inherited Destroy;

end;

procedure TpvGUIWidget.AfterConstruction;
begin
 inherited AfterConstruction;
end;

procedure TpvGUIWidget.BeforeDestruction;
begin
 inherited BeforeDestruction;
end;

procedure TpvGUIWidget.SetTheme(const aTheme:TpvGUITheme);
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 fTheme:=aTheme;
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   ChildWidget.SetTheme(aTheme);
  end;
 end;
end;

function TpvGUIWidget.GetLeft:TpvFloat;
begin
 result:=fPosition.x;
end;

procedure TpvGUIWidget.SetLeft(const aLeft:TpvFloat);
begin
 fPosition.x:=aLeft;
end;

function TpvGUIWidget.GetTop:TpvFloat;
begin
 result:=fPosition.y;
end;

procedure TpvGUIWidget.SetTop(const aTop:TpvFloat);
begin
 fPosition.y:=aTop;
end;

function TpvGUIWidget.GetWidth:TpvFloat;
begin
 result:=fSize.x;
end;

procedure TpvGUIWidget.SetWidth(const aWidth:TpvFloat);
begin
 fSize.x:=aWidth;
end;

function TpvGUIWidget.GetHeight:TpvFloat;
begin
 result:=fSize.y;
end;

procedure TpvGUIWidget.SetHeight(const aHeight:TpvFloat);
begin
 fSize.y:=aHeight;
end;

function TpvGUIWidget.GetFixedWidth:TpvFloat;
begin
 result:=fFixedSize.x;
end;

procedure TpvGUIWidget.SetFixedWidth(const aFixedWidth:TpvFloat);
begin
 fFixedSize.x:=aFixedWidth;
end;

function TpvGUIWidget.GetFixedHeight:TpvFloat;
begin
 result:=fFixedSize.y;
end;

procedure TpvGUIWidget.SetFixedHeight(const aFixedHeight:TpvFloat);
begin
 fFixedSize.y:=aFixedHeight;
end;

function TpvGUIWidget.GetAbsolutePosition:TpvVector2;
begin
 if assigned(fParent) and (fParent is TpvGUIWidget) then begin
  result:=(fParent as TpvGUIWidget).AbsolutePosition+fPosition;
 end else begin
  result:=fPosition;
 end;
end;

function TpvGUIWidget.GetRecursiveVisible:boolean;
var CurrentWidget:TpvGUIWidget;
begin
 CurrentWidget:=self;
 repeat
  result:=CurrentWidget.Visible;
  if result and assigned(CurrentWidget.fParent) and (CurrentWidget.fParent is TpvGUIWidget) then begin
   CurrentWidget:=CurrentWidget.fParent as TpvGUIWidget;
  end else begin
   break;
  end;
 until false;
end;

function TpvGUIWidget.GetPreferredSize:TpvVector2;
begin
 if assigned(fLayout) then begin
  result:=fLayout.GetPreferredSize(self);
 end else begin
  result:=fSize;
 end;
end;

function TpvGUIWidget.GetFontSize:TpvFloat;
begin
 if assigned(fTheme) and IsZero(fFontSize) then begin
  result:=fTheme.fFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TpvGUIWidget.GetEnumerator:TpvGUIWidgetEnumerator;
begin
 result:=TpvGUIWidgetEnumerator.Create(self);
end;

function TpvGUIWidget.Contains(const aPosition:TpvVector2):boolean;
begin
 result:=(aPosition.x>=fPosition.x) and
         (aPosition.y>=fPosition.y) and
         (aPosition.x<(fPosition.x+fSize.x)) and
         (aPosition.y<(fPosition.y+fSize.y));
end;

function TpvGUIWidget.FindWidget(const aPosition:TpvVector2):TpvGUIWidget;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
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

function TpvGUIWidget.GetWindow:TpvGUIWindow;
var CurrentWidget:TpvGUIWidget;
begin
 result:=nil;
 CurrentWidget:=self;
 while assigned(CurrentWidget) do begin
  if CurrentWidget is TpvGUIWindow then begin
   result:=CurrentWidget as TpvGUIWindow;
   exit;
  end else begin
   if assigned(CurrentWidget.Parent) and (CurrentWidget.Parent is TpvGUIWidget) then begin
    CurrentWidget:=CurrentWidget.fParent as TpvGUIWidget;
   end else begin
    break;
   end;
  end;
 end;
 raise EpvGUIWidget.Create('Could not find parent window');
end;

procedure TpvGUIWidget.RequestFocus;
var CurrentWidget:TpvGUIWidget;
begin
 if assigned(fInstance) then begin
  fInstance.UpdateFocus(self);
 end else begin
  CurrentWidget:=self;
  while assigned(CurrentWidget) do begin
   if CurrentWidget is TpvGUIInstance then begin
    (CurrentWidget as TpvGUIInstance).UpdateFocus(self);
    break;
   end else begin
    if assigned(CurrentWidget.Parent) and (CurrentWidget.Parent is TpvGUIWidget) then begin
     CurrentWidget:=CurrentWidget.fParent as TpvGUIWidget;
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

procedure TpvGUIWidget.PerformLayout;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildWidgetPreferredSize,ChildWidgetFixedSize,ChildWidgetSize:TpvVector2;
begin
 if assigned(fLayout) then begin
  fLayout.PerformLayout(self);
 end else begin
  for ChildIndex:=0 to fChildren.Count-1 do begin
   Child:=fChildren.Items[ChildIndex];
   if Child is TpvGUIWidget then begin
    ChildWidget:=Child as TpvGUIWidget;
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

function TpvGUIWidget.Enter:boolean;
begin
 fFocused:=true;
 result:=false;
end;

function TpvGUIWidget.Leave:boolean;
begin
 fFocused:=false;
 result:=false;
end;

function TpvGUIWidget.PointerEnter:boolean;
begin
 fPointerFocused:=true;
 result:=false;
end;

function TpvGUIWidget.PointerLeave:boolean;
begin
 fPointerFocused:=false;
 result:=false;
end;

function TpvGUIWidget.KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TpvGUIWidget.KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TpvGUIWidget.KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TpvGUIWidget.PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
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

function TpvGUIWidget.PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
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

function TpvGUIWidget.PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    PreviousContained,CurrentContained:boolean;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    PreviousContained:=ChildWidget.Contains((aPosition-fPosition)-aRelativePosition);
    CurrentContained:=ChildWidget.Contains(aPosition-fPosition);
    if CurrentContained and not PreviousContained then begin
     ChildWidget.PointerEnter;
    end else if PreviousContained and not CurrentContained then begin
     ChildWidget.PointerLeave;
    end;
    if PreviousContained or CurrentContained then begin
     result:=ChildWidget.PointerMotion(aPosition-fPosition,aRelativePosition,aPressure,aPointerID,aButton);
     if result then begin
      exit;
     end;
    end;
   end;
  end;
 end;
 result:=false;
end;

function TpvGUIWidget.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
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

procedure TpvGUIWidget.Update(const aDeltaTime:TpvDouble);
begin

end;

procedure TpvGUIWidget.Draw;
begin

end;

constructor TpvGUIInstance.Create(const aCanvas:TpvCanvas);
begin

 inherited Create(nil);

 fInstance:=self;

 fDelayedDeleteQueue:=TpvGUIDelayedDeleteQueue.Create;

 fCanvas:=aCanvas;

end;

destructor TpvGUIInstance.Destroy;
var DelayedDeleteQueueItem:TpvGUIDelayedDeleteQueueItem;
begin
 for DelayedDeleteQueueItem in fDelayedDeleteQueue do begin
  DelayedDeleteQueueItem.fObject.Free;
 end;
 FreeAndNil(fDelayedDeleteQueue);
 inherited Destroy;
end;

procedure TpvGUIInstance.ProcessDelayedDeleteQueue;
var DelayedDeleteQueueItemIndex,CounterThreshold:TpvInt32;
    DelayedDeleteQueueItem:TpvGUIDelayedDeleteQueueItem;
begin
 CounterThreshold:=-((pvApplication.VulkanSwapChain.CountImages shl 1) or 1);
 DelayedDeleteQueueItemIndex:=0;
 while DelayedDeleteQueueItemIndex<fDelayedDeleteQueue.Count do begin
  DelayedDeleteQueueItem:=fDelayedDeleteQueue.Items[DelayedDeleteQueueItemIndex];
  if DelayedDeleteQueueItem.fCounter>CounterThreshold then begin
   dec(DelayedDeleteQueueItem.fCounter);
   if DelayedDeleteQueueItem.fCounter=CounterThreshold then begin
    DelayedDeleteQueueItem.fObject.Free;
    fDelayedDeleteQueue.Delete(DelayedDeleteQueueItemIndex);
   end else begin
    fDelayedDeleteQueue.Items[DelayedDeleteQueueItemIndex]:=DelayedDeleteQueueItem;
    inc(DelayedDeleteQueueItemIndex);
   end;
  end;
 end;
end;

procedure TpvGUIInstance.UpdateFocus(const aWidget:TpvGUIWidget);
begin

end;

procedure TpvGUIInstance.Update(const aDeltaTime:TpvDouble);
begin
 ProcessDelayedDeleteQueue;
 inherited Update(aDeltaTime);
end;

procedure TpvGUIInstance.Draw;
begin
 inherited Draw;
end;

end.
