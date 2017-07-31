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
     PasMP,
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

     TpvGUIObjectList=class(TObjectList<TpvGUIObject>)
      protected
       procedure Notify({$ifdef fpc}constref{$else}const{$endif} Value:TpvGUIObject;Action:TCollectionNotification); override;
      public
     end;

     TpvGUIObject=class(TpvReferenceCountedObject)
      private
       fInstance:TpvGUIInstance;
       fParent:TpvGUIObject;
       fChildren:TpvGUIObjectList;
       fID:TpvUTF8String;
       fTag:TpvPtrInt;
       fReferenceCounter:TpvInt32;
      public
       constructor Create(const aParent:TpvGUIObject); reintroduce; virtual;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
      published
       property Instance:TpvGUIInstance read fInstance;
       property Parent:TpvGUIObject read fParent write fParent;
       property Children:TpvGUIObjectList read fChildren;
       property ID:TpvUTF8String read fID write fID;
       property Tag:TpvPtrInt read fTag write fTag;
       property ReferenceCounter:TpvInt32 read fReferenceCounter write fReferenceCounter;
     end;

     TpvGUILayout=class(TpvGUIObject)
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; virtual;
       procedure PerformLayout(const aWidget:TpvGUIWidget); virtual;
      public
     end;

     TpvGUITheme=class(TpvGUIObject)
      private
      protected
       fFontSize:TpvFloat;
       fSpriteAtlas:TpvSpriteAtlas;
       fSpriteUnfocusedWindowFill:TpvSprite;
       fSpriteUnfocusedWindowFillNinePatch:TpvSpriteNinePatch;
       fSpriteFocusedWindowFill:TpvSprite;
       fSpriteFocusedWindowFillNinePatch:TpvSpriteNinePatch;
       fSpriteUnfocusedWindowHeader:TpvSprite;
       fSpriteUnfocusedWindowHeaderNinePatch:TpvSpriteNinePatch;
       fSpriteFocusedWindowHeader:TpvSprite;
       fSpriteFocusedWindowHeaderNinePatch:TpvSpriteNinePatch;
       fSpriteUnfocusedWindowGrip:TpvSprite;
       fSpriteUnfocusedWindowGripNinePatch:TpvSpriteNinePatch;
       fSpriteFocusedWindowGrip:TpvSprite;
       fSpriteFocusedWindowGripNinePatch:TpvSpriteNinePatch;
       fWindowHeaderHeight:TpvFloat;
       fWindowGripPaddingRight:TpvFloat;
       fWindowGripPaddingBottom:TpvFloat;
       fWindowGripWidth:TpvFloat;
       fWindowGripHeight:TpvFloat;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure Setup; virtual;
      public
       property SpriteUnfocusedWindowFillNinePatch:TpvSpriteNinePatch read fSpriteUnfocusedWindowFillNinePatch write fSpriteUnfocusedWindowFillNinePatch;
       property SpriteFocusedWindowFillNinePatch:TpvSpriteNinePatch read fSpriteFocusedWindowFillNinePatch write fSpriteFocusedWindowFillNinePatch;
       property SpriteUnfocusedWindowHeaderNinePatch:TpvSpriteNinePatch read fSpriteUnfocusedWindowHeaderNinePatch write fSpriteUnfocusedWindowHeaderNinePatch;
       property SpriteFocusedWindowHeaderNinePatch:TpvSpriteNinePatch read fSpriteFocusedWindowHeaderNinePatch write fSpriteFocusedWindowHeaderNinePatch;
       property SpriteUnfocusedWindowGripNinePatch:TpvSpriteNinePatch read fSpriteUnfocusedWindowGripNinePatch write fSpriteUnfocusedWindowGripNinePatch;
       property SpriteFocusedWindowGripNinePatch:TpvSpriteNinePatch read fSpriteFocusedWindowGripNinePatch write fSpriteFocusedWindowGripNinePatch;
      published
       property FontSize:TpvFloat read fFontSize write fFontSize;
       property SpriteAtlas:TpvSpriteAtlas read fSpriteAtlas;
       property SpriteUnfocusedWindowFill:TpvSprite read fSpriteUnfocusedWindowFill write fSpriteUnfocusedWindowFill;
       property SpriteFocusedWindowFill:TpvSprite read fSpriteFocusedWindowFill write fSpriteFocusedWindowFill;
       property SpriteUnfocusedWindowHeader:TpvSprite read fSpriteUnfocusedWindowHeader write fSpriteUnfocusedWindowHeader;
       property SpriteFocusedWindowHeader:TpvSprite read fSpriteFocusedWindowHeader write fSpriteFocusedWindowHeader;
       property SpriteUnfocusedWindowGrip:TpvSprite read fSpriteUnfocusedWindowGrip write fSpriteUnfocusedWindowGrip;
       property SpriteFocusedWindowGrip:TpvSprite read fSpriteFocusedWindowGrip write fSpriteFocusedWindowGrip;
       property WindowHeaderHeight:TpvFloat read fWindowHeaderHeight write fWindowHeaderHeight;
       property WindowGripPaddingRight:TpvFloat read fWindowGripPaddingRight write fWindowGripPaddingRight;
       property WindowGripPaddingBottom:TpvFloat read fWindowGripPaddingBottom write fWindowGripPaddingBottom;
       property WindowGripWidth:TpvFloat read fWindowGripWidth write fWindowGripWidth;
       property WindowGripHeight:TpvFloat read fWindowGripHeight write fWindowGripHeight;
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
       fCanvas:TpvCanvas;
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
       function GetWindow:TpvGUIWindow;
      protected
       procedure SetCanvas(const aCanvas:TpvCanvas); virtual;
       function GetTheme:TpvGUITheme; virtual;
       procedure SetTheme(const aTheme:TpvGUITheme); virtual;
       function GetPreferredSize:TpvVector2; virtual;
       function GetFontSize:TpvFloat; virtual;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       function GetEnumerator:TpvGUIWidgetEnumerator;
       function Contains(const aPosition:TpvVector2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function FindWidget(const aPosition:TpvVector2):TpvGUIWidget;
       procedure PerformLayout; virtual;
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
       procedure Update; virtual;
       procedure Draw; virtual;
      public
       property AbsolutePosition:TpvVector2 read GetAbsolutePosition;
       property PreferredSize:TpvVector2 read GetPreferredSize;
      published
       property Window:TpvGUIWindow read GetWindow;
       property Canvas:TpvCanvas read fCanvas write SetCanvas;
       property Layout:TpvGUILayout read fLayout write fLayout;
       property Theme:TpvGUITheme read GetTheme write SetTheme;
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

     TpvGUIInstanceBufferReferenceCountedObjects=array of TpvReferenceCountedObject;

     PpvGUIInstanceBuffer=^TpvGUIInstanceBuffer;
     TpvGUIInstanceBuffer=record
      ReferenceCountedObjects:TpvGUIInstanceBufferReferenceCountedObjects;
      CountReferenceCountedObjects:TpvInt32;
     end;

     TpvGUIInstanceBuffers=array of TpvGUIInstanceBuffer;

     TpvGUIInstance=class(TpvGUIWidget)
      private
       fVulkanDevice:TpvVulkanDevice;
       fStandardTheme:TpvGUITheme;
       fDrawWidgetBounds:boolean;
       fBuffers:TpvGUIInstanceBuffers;
       fCountBuffers:TpvInt32;
       fUpdateBufferIndex:TpvInt32;
       fDrawBufferIndex:TpvInt32;
       fDeltaTime:TpvDouble;
       fFocusPath:TpvGUIObjectList;
       fDragWidget:TpvGUIWidget;
       fWindow:TpvGUIWindow;
       procedure SetCountBuffers(const aCountBuffers:TpvInt32);
       procedure SetUpdateBufferIndex(const aUpdateBufferIndex:TpvInt32);
       procedure SetDrawBufferIndex(const aDrawBufferIndex:TpvInt32);
       procedure DisposeWindow(const aWindow:TpvGUIWindow);
       procedure CenterWindow(const aWindow:TpvGUIWindow);
       procedure MoveWindowToFront(const aWindow:TpvGUIWindow);
      public
       constructor Create(const aVulkanDevice:TpvVulkanDevice); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure ClearReferenceCountedObjectList;
       procedure AddReferenceCountedObjectForNextDraw(const aObject:TpvReferenceCountedObject);
       procedure UpdateFocus(const aWidget:TpvGUIWidget);
       function PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;
       function PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;
       function PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;
       property StandardTheme:TpvGUITheme read fStandardTheme;
       property DrawWidgetBounds:boolean read fDrawWidgetBounds write fDrawWidgetBounds;
       property CountBuffers:TpvInt32 read fCountBuffers write SetCountBuffers;
       property UpdateBufferIndex:TpvInt32 read fUpdateBufferIndex write fUpdateBufferIndex;
       property DrawBufferIndex:TpvInt32 read fDrawBufferIndex write fDrawBufferIndex;
       property DeltaTime:TpvDouble read fDeltaTime write fDeltaTime;
     end;

     PpvGUIWindowMouseAction=^TpvGUIWindowMouseAction;
     TpvGUIWindowMouseAction=
      (
       pvgwmaNone,
       pvgwmaMove,
       pvgwmaSize
      );

     TpvGUIWindow=class(TpvGUIWidget)
      private
       fTitle:TpvRawByteString;
       fMouseAction:TpvGUIWindowMouseAction;
       fModal:boolean;
       fResizable:boolean;
       fButtonPanel:TpvGUIWidget;
       function GetButtonPanel:TpvGUIWidget;
      protected
       function GetPreferredSize:TpvVector2; override;
       procedure RefreshRelativePlacement; virtual;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure DisposeWindow;
       procedure Center;
       procedure PerformLayout; override;
       function PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;
       function PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;
       function PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Title:TpvRawByteString read fTitle write fTitle;
       property Modal:boolean read fModal write fModal;
       property Resizable:boolean read fResizable write fResizable;
       property ButtonPanel:TpvGUIWidget read GetButtonPanel;
     end;

implementation

procedure TpvGUIObjectList.Notify({$ifdef fpc}constref{$else}const{$endif} Value:TpvGUIObject;Action:TCollectionNotification);
begin
 if assigned(Value) then begin
  case Action of
   cnAdded:begin
    Value.IncRef;
   end;
   cnRemoved:begin
    Value.DecRef;
   end;
   cnExtracted:begin
   end;
  end;
 end else begin
  inherited Notify(Value,Action);
 end;
end;

constructor TpvGUIObject.Create(const aParent:TpvGUIObject);
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

 fChildren:=TpvGUIObjectList.Create(false);

 fID:='';

 fTag:=0;

 fReferenceCounter:=0;

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
 if assigned(fParent) and assigned(fParent.fChildren) then begin
  fParent.fChildren.Extract(self);
 end;
 inherited BeforeDestruction;
end;

function TpvGUILayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
begin
 result:=aWidget.fSize;
end;

procedure TpvGUILayout.PerformLayout(const aWidget:TpvGUIWidget);
begin

end;

constructor TpvGUITheme.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fSpriteAtlas:=nil;
 Setup;
end;

destructor TpvGUITheme.Destroy;
begin
 FreeAndNil(fSpriteAtlas);
 inherited Destroy;
end;

procedure TpvGUITheme.Setup;
 function sdRoundBox(const p,b:TpvVector2;const r:TpvFloat):TpvFloat;
 var d:TpvVector2;
 begin
  d.x:=abs(p.x)-b.x;
  d.y:=abs(p.y)-b.y;
  result:=(Min(Max(d.x,d.y),0.0)+sqrt(sqr(Max(0.0,d.x))+sqr(Max(0.0,d.y))))-r;
 end;
 procedure CreateWindowFillNinePatchSprite(var aSprite:TpvSprite;
                                           var aSpriteNinePatch:TpvSpriteNinePatch;
                                           const aWidth:TpvInt32;
                                           const aHeight:TpvInt32;
                                           const aRadius:TpvInt32;
                                           const aFillColor:TpvVector4;
                                           const aBorderColor:TpvVector4;
                                           const aTransparentColor:TpvVector4);
 var x,y,Index,r:TpvInt32;
     ImageData:array of TpvSpriteTextureTexel;
     FillColor,BorderColor,TransparentColor,Color:TpvSpriteTextureTexel;
     c:TpvVector4;
     f:TpvFloat;
 begin
  ImageData:=nil;
  try
   SetLength(ImageData,aWidth*aHeight);
   Index:=0;
   for y:=0 to aHeight-1 do begin
    for x:=0 to aWidth-1 do begin
     f:=sdRoundBox(TpvVector2.Create((x+0.5)-(aWidth*0.5),(y+0.5)-(aHeight*0.5)),
                   TpvVector2.Create((aWidth*0.5)-(aRadius*2.0),(aHeight*0.5)-(aRadius*2.0)),
                   aRadius);
     c:=Mix(Mix(aTransparentColor,
                aBorderColor,
                LinearStep(2.0,1.0,f)),
            aFillColor,
            LinearStep(1.0,0.0,f));
     Color.r:=Min(Max(round(c.r*255.0),0),255);
     Color.g:=Min(Max(round(c.g*255.0),0),255);
     Color.b:=Min(Max(round(c.b*255.0),0),255);
     Color.a:=Min(Max(round(c.a*255.0),0),255);
     ImageData[Index]:=Color;
     inc(Index);
    end;
   end;
   aSprite:=fSpriteAtlas.LoadRawSprite('',@ImageData[0],aWidth,aHeight,false);
   r:=aRadius+1;
   aSpriteNinePatch.Regions[0,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,0,r,r);
   aSpriteNinePatch.Regions[0,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,0,aWidth-(r*2),r);
   aSpriteNinePatch.Regions[0,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,0,r,r);
   aSpriteNinePatch.Regions[1,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,r,r,aHeight-(r*2));
   aSpriteNinePatch.Regions[1,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,r,aWidth-(r*2),aHeight-(r*2));
   aSpriteNinePatch.Regions[1,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,r,r,aHeight-(r*2));
   aSpriteNinePatch.Regions[2,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,aHeight-r,r,r);
   aSpriteNinePatch.Regions[2,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,aHeight-r,aWidth-(r*2),r);
   aSpriteNinePatch.Regions[2,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,aHeight-r,r,r);
  finally
   ImageData:=nil;
  end;
 end;
 procedure CreateWindowHeaderNinePatchSprite(var aSprite:TpvSprite;
                                             var aSpriteNinePatch:TpvSpriteNinePatch;
                                             const aWidth:TpvInt32;
                                             const aHeight:TpvInt32;
                                             const aRadius:TpvInt32;
                                             const aFillStartColor:TpvVector4;
                                             const aFillStopColor:TpvVector4;
                                             const aBorderStartColor:TpvVector4;
                                             const aBorderStopColor:TpvVector4;
                                             const aTransparentStartColor:TpvVector4;
                                             const aTransparentStopColor:TpvVector4);
 var x,y,Index,r:TpvInt32;
     ImageData:array of TpvSpriteTextureTexel;
     FillColor,BorderColor,TransparentColor,Color:TpvSpriteTextureTexel;
     c:TpvVector4;
     f:TpvFloat;
 begin
  ImageData:=nil;
  try
   SetLength(ImageData,aWidth*aHeight);
   Index:=0;
   for y:=0 to aHeight-1 do begin
    for x:=0 to aWidth-1 do begin
     f:=sdRoundBox(TpvVector2.Create((x+0.5)-(aWidth*0.5),(y+0.5)-(aHeight*0.5)),
                   TpvVector2.Create((aWidth*0.5)-(aRadius*2.0),(aHeight*0.5)-(aRadius*2.0)),
                   aRadius);
     c:=Mix(Mix(Mix(aTransparentStartColor,aTransparentStopColor,LinearStep(0.0,aHeight-1,y)),
                Mix(aBorderStartColor,aBorderStopColor,LinearStep(0.0,aHeight-1,y)),
                LinearStep(2.0,1.0,f)),
            Mix(aFillStartColor,aFillStopColor,LinearStep(0.0,aHeight-1,y)),
            LinearStep(1.0,0.0,f));
     Color.r:=Min(Max(round(c.r*255.0),0),255);
     Color.g:=Min(Max(round(c.g*255.0),0),255);
     Color.b:=Min(Max(round(c.b*255.0),0),255);
     Color.a:=Min(Max(round(c.a*255.0),0),255);
     ImageData[Index]:=Color;
     inc(Index);
    end;
   end;
   aSprite:=fSpriteAtlas.LoadRawSprite('',@ImageData[0],aWidth,aHeight,false);
   r:=aRadius+1;
   aSpriteNinePatch.Regions[0,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,0,r,r);
   aSpriteNinePatch.Regions[0,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,0,aWidth-(r*2),r);
   aSpriteNinePatch.Regions[0,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,0,r,r);
   aSpriteNinePatch.Regions[1,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,r,r,aHeight-(r*2));
   aSpriteNinePatch.Regions[1,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,r,aWidth-(r*2),aHeight-(r*2));
   aSpriteNinePatch.Regions[1,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,r,r,aHeight-(r*2));
   aSpriteNinePatch.Regions[2,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,aHeight-r,r,r);
   aSpriteNinePatch.Regions[2,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,aHeight-r,aWidth-(r*2),r);
   aSpriteNinePatch.Regions[2,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,aHeight-r,r,r);
  finally
   ImageData:=nil;
  end;
 end;
 procedure CreateWindowGripNinePatchSprite(var aSprite:TpvSprite;
                                           var aSpriteNinePatch:TpvSpriteNinePatch;
                                           const aWidth:TpvInt32;
                                           const aHeight:TpvInt32;
                                           const aRadius:TpvInt32;
                                           const aOuterFillStartColor:TpvVector4;
                                           const aOuterFillStopColor:TpvVector4;
                                           const aInnerFillStartColor:TpvVector4;
                                           const aInnerFillStopColor:TpvVector4;
                                           const aBorderStartColor:TpvVector4;
                                           const aBorderStopColor:TpvVector4;
                                           const aTransparentStartColor:TpvVector4;
                                           const aTransparentStopColor:TpvVector4);
 var x,y,Index,r:TpvInt32;
     ImageData:array of TpvSpriteTextureTexel;
     FillColor,BorderColor,TransparentColor,Color:TpvSpriteTextureTexel;
     c:TpvVector4;
     f:TpvFloat;
 begin
  ImageData:=nil;
  try
   SetLength(ImageData,aWidth*aHeight);
   Index:=0;
   for y:=0 to aHeight-1 do begin
    for x:=0 to aWidth-1 do begin
     f:=sdRoundBox(TpvVector2.Create((x+0.5)-(aWidth*0.5),(y+0.5)-(aHeight*0.5)),
                   TpvVector2.Create((aWidth*0.5)-(aRadius*2.0),(aHeight*0.5)-(aRadius*2.0)),
                   aRadius);
     c:=Mix(Mix(Mix(aTransparentStartColor,aTransparentStopColor,LinearStep(0.0,aHeight-1,y)),
                Mix(aBorderStartColor,aBorderStopColor,LinearStep(0.0,aHeight-1,y)),
                LinearStep(2.0,1.0,f)),
            Mix(Mix(aOuterFillStartColor,aOuterFillStopColor,LinearStep(0.0,aHeight-1,y)),
                Mix(aInnerFillStartColor,aInnerFillStopColor,LinearStep(0.0,aHeight-1,y)),
                LinearStep(0.0,-Min(aWidth*0.5,aHeight*0.5),f)),
            LinearStep(1.0,0.0,f));
     Color.r:=Min(Max(round(c.r*255.0),0),255);
     Color.g:=Min(Max(round(c.g*255.0),0),255);
     Color.b:=Min(Max(round(c.b*255.0),0),255);
     Color.a:=Min(Max(round(c.a*255.0),0),255);
     ImageData[Index]:=Color;
     inc(Index);
    end;
   end;
   aSprite:=fSpriteAtlas.LoadRawSprite('',@ImageData[0],aWidth,aHeight,false);
   r:=aRadius+1;
   aSpriteNinePatch.Regions[0,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,0,r,r);
   aSpriteNinePatch.Regions[0,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,0,aWidth-(r*2),r);
   aSpriteNinePatch.Regions[0,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,0,r,r);
   aSpriteNinePatch.Regions[1,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,r,r,aHeight-(r*2));
   aSpriteNinePatch.Regions[1,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,r,aWidth-(r*2),aHeight-(r*2));
   aSpriteNinePatch.Regions[1,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,r,r,aHeight-(r*2));
   aSpriteNinePatch.Regions[2,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,aHeight-r,r,r);
   aSpriteNinePatch.Regions[2,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,r,aHeight-r,aWidth-(r*2),r);
   aSpriteNinePatch.Regions[2,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aWidth-r,aHeight-r,r,r);
  finally
   ImageData:=nil;
  end;
 end;
begin

 fFontSize:=-12;

 fWindowHeaderHeight:=32;

 fWindowGripPaddingRight:=4;
 fWindowGripPaddingBottom:=4;

 fWindowGripWidth:=16;
 fWindowGripHeight:=8;

 fSpriteAtlas:=TpvSpriteAtlas.Create(fInstance.fVulkanDevice);

 CreateWindowFillNinePatchSprite(fSpriteUnfocusedWindowFill,
                                 fSpriteUnfocusedWindowFillNinePatch,
                                 32,
                                 32,
                                 2,
                                 TpvVector4.Create(43.0,43.0,43.0,230.0)/255.0,
                                 TpvVector4.Create(21.5,21.5,21.5,230.0)/255.0,
                                 TpvVector4.Create(21.5,21.5,21.5,0.0)/255.0);

 CreateWindowFillNinePatchSprite(fSpriteFocusedWindowFill,
                                 fSpriteFocusedWindowFillNinePatch,
                                 32,
                                 32,
                                 2,
                                 TpvVector4.Create(45.0,45.0,45.0,230.0)/255.0,
                                 TpvVector4.Create(22.5,22.5,22.5,230.0)/255.0,
                                 TpvVector4.Create(22.5,22.5,22.5,0.0)/255.0);

 CreateWindowHeaderNinePatchSprite(fSpriteUnfocusedWindowHeader,
                                   fSpriteUnfocusedWindowHeaderNinePatch,
                                   32,
                                   32,
                                   2,
                                   TpvVector4.Create(64.0,64.0,64.0,255.0)/255.0,
                                   TpvVector4.Create(48.0,48.0,48.0,255.0)/255.0,
                                   TpvVector4.Create(92.0,92.0,92.0,255.0)/255.0,
                                   TpvVector4.Create(29.0,29.0,29.0,255.0)/255.0,
                                   TpvVector4.Create(92.0,92.0,92.0,0.0)/255.0,
                                   TpvVector4.Create(29.0,29.0,29.0,0.0)/255.0);

 CreateWindowHeaderNinePatchSprite(fSpriteFocusedWindowHeader,
                                   fSpriteFocusedWindowHeaderNinePatch,
                                   32,
                                   32,
                                   2,
                                   TpvVector4.Create(74.0,74.0,74.0,255.0)/255.0,
                                   TpvVector4.Create(58.0,58.0,58.0,255.0)/255.0,
                                   TpvVector4.Create(92.0,92.0,92.0,255.0)/255.0,
                                   TpvVector4.Create(29.0,29.0,29.0,255.0)/255.0,
                                   TpvVector4.Create(92.0,92.0,92.0,0.0)/255.0,
                                   TpvVector4.Create(29.0,29.0,29.0,0.0)/255.0);

 CreateWindowGripNinePatchSprite(fSpriteUnfocusedWindowGrip,
                                 fSpriteUnfocusedWindowGripNinePatch,
                                 16,
                                 8,
                                 2,
                                 TpvVector4.Create(48.0,48.0,48.0,255.0)/255.0,
                                 TpvVector4.Create(64.0,64.0,64.0,255.0)/255.0,
                                 TpvVector4.Create(32.0,32.0,32.0,255.0)/255.0,
                                 TpvVector4.Create(48.0,48.0,48.0,255.0)/255.0,
                                 TpvVector4.Create(29.0,29.0,29.0,255.0)/255.0,
                                 TpvVector4.Create(92.0,92.0,92.0,255.0)/255.0,
                                 TpvVector4.Create(29.0,29.0,29.0,0.0)/255.0,
                                 TpvVector4.Create(92.0,92.0,92.0,0.0)/255.0);

 CreateWindowGripNinePatchSprite(fSpriteFocusedWindowGrip,
                                 fSpriteFocusedWindowGripNinePatch,
                                 16,
                                 8,
                                 2,
                                 TpvVector4.Create(58.0,58.0,58.0,255.0)/255.0,
                                 TpvVector4.Create(74.0,74.0,74.0,255.0)/255.0,
                                 TpvVector4.Create(37.0,37.0,37.0,255.0)/255.0,
                                 TpvVector4.Create(58.0,58.0,58.0,255.0)/255.0,
                                 TpvVector4.Create(29.0,29.0,29.0,255.0)/255.0,
                                 TpvVector4.Create(92.0,92.0,92.0,255.0)/255.0,
                                 TpvVector4.Create(29.0,29.0,29.0,0.0)/255.0,
                                 TpvVector4.Create(92.0,92.0,92.0,0.0)/255.0);

 fSpriteAtlas.MipMaps:=false;

 fSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                     pvApplication.VulkanGraphicsCommandBuffers[0,0],
                     pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                     pvApplication.VulkanDevice.TransferQueue,
                     pvApplication.VulkanTransferCommandBuffers[0,0],
                     pvApplication.VulkanTransferCommandBufferFences[0,0]);

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

constructor TpvGUIWidget.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fCanvas:=nil;

 fLayout:=nil;

 fTheme:=nil;

 fCursor:=nil;

 fPosition:=TpvVector2.Create(0.0,0.0);

 fSize:=TpvVector2.Create(1.0,1.0);

 fFixedSize:=TpvVector2.Create(-1.0,-1.0);

 fPositionProperty:=TpvVector2Property.Create(@fPosition);

 fSizeProperty:=TpvVector2Property.Create(@fSize);

 fFixedSizeProperty:=TpvVector2Property.Create(@fFixedSize);

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

procedure TpvGUIWidget.SetCanvas(const aCanvas:TpvCanvas);
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 fCanvas:=aCanvas;
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   ChildWidget.SetCanvas(aCanvas);
  end;
 end;
end;

function TpvGUIWidget.GetTheme:TpvGUITheme;
begin
 if assigned(fTheme) then begin
  result:=fTheme;
 end else if assigned(fInstance) then begin
  result:=fInstance.fStandardTheme;
 end else begin
  result:=nil;
 end;
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

procedure TpvGUIWidget.Update;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    BaseClipRect:TpvRect;
    BaseModelMatrix:TpvMatrix4x4;
begin
 BaseClipRect:=fCanvas.State.ClipRect;
 BaseModelMatrix:=fCanvas.ModelMatrix;
 try
  if fInstance.fDrawWidgetBounds then begin
   fCanvas.Push;
   try
    fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
    fCanvas.LineWidth:=4.0;
    fCanvas.LineJoin:=pvcljRound;
    fCanvas.LineCap:=pvclcRound;
    fCanvas.BeginPath;
    fCanvas.MoveTo(0.0,0.0);
    fCanvas.LineTo(Width,0.0);
    fCanvas.LineTo(Width,Height);
    fCanvas.LineTo(0.0,Height);
    fCanvas.ClosePath;
    fCanvas.Stroke;
    fCanvas.EndPath;
   finally
    fCanvas.Pop;
   end;
  end;
  for ChildIndex:=0 to fChildren.Count-1 do begin
   Child:=fChildren.Items[ChildIndex];
   if Child is TpvGUIWidget then begin
    ChildWidget:=Child as TpvGUIWidget;
    fInstance.AddReferenceCountedObjectForNextDraw(ChildWidget);
    if ChildWidget.Visible then begin
     fCanvas.ClipRect:=BaseClipRect.GetIntersection(TpvRect.CreateAbsolute(ChildWidget.Left,
                                                                           ChildWidget.Top,
                                                                           ChildWidget.Left+ChildWidget.Width,
                                                                           ChildWidget.Top+ChildWidget.Height));
     fCanvas.ModelMatrix:=TpvMatrix4x4.CreateTranslation(ChildWidget.Left,ChildWidget.Top)*BaseModelMatrix;
     ChildWidget.fCanvas:=fCanvas;
     ChildWidget.Update;
    end;
   end;
  end;
 finally
  fCanvas.ClipRect:=BaseClipRect;
  fCanvas.ModelMatrix:=BaseModelMatrix;
 end;
end;

procedure TpvGUIWidget.Draw;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildWidget.fCanvas:=fCanvas;
    ChildWidget.Draw;
   end;
  end;
 end;
end;

constructor TpvGUIInstance.Create(const aVulkanDevice:TpvVulkanDevice);
begin

 inherited Create(nil);

 fInstance:=self;

 fVulkanDevice:=aVulkanDevice;

 fStandardTheme:=TpvGUITheme.Create(self);

 fDrawWidgetBounds:=false;

 fBuffers:=nil;

 fCountBuffers:=0;

 fUpdateBufferIndex:=0;

 fDrawBufferIndex:=0;

 fDeltaTime:=0.0;

 fFocusPath:=TpvGUIObjectList.Create(false);

 fDragWidget:=nil;

 fWindow:=nil;

 SetCountBuffers(1);

end;

destructor TpvGUIInstance.Destroy;
begin

 TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);

 FreeAndNil(fFocusPath);

 SetCountBuffers(0);

 fBuffers:=nil;

 inherited Destroy;

end;

procedure TpvGUIInstance.SetCountBuffers(const aCountBuffers:TpvInt32);
var Index,SubIndex:TpvInt32;
    Buffer:PpvGUIInstanceBuffer;
begin

 if fCountBuffers<>aCountBuffers then begin

  for Index:=aCountBuffers to fCountBuffers-1 do begin
   Buffer:=@fBuffers[Index];
   for SubIndex:=0 to Buffer^.CountReferenceCountedObjects-1 do begin
    Buffer^.ReferenceCountedObjects[SubIndex].DecRef;
   end;
   Buffer^.CountReferenceCountedObjects:=0;
  end;

  if length(fBuffers)<aCountBuffers then begin
   SetLength(fBuffers,aCountBuffers*2);
   for Index:=Max(0,fCountBuffers) to length(fBuffers)-1 do begin
    fBuffers[Index].CountReferenceCountedObjects:=0;
   end;
  end;

  for Index:=fCountBuffers to aCountBuffers-1 do begin
   fBuffers[Index].CountReferenceCountedObjects:=0;
  end;

  fCountBuffers:=aCountBuffers;

 end;

end;

procedure TpvGUIInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 IncRef;
end;

procedure TpvGUIInstance.BeforeDestruction;
begin
 TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
 TpvReferenceCountedObject.DecRefOrFreeAndNil(fWindow);
 fFocusPath.Clear;
 DecRefWithoutFree;
 inherited BeforeDestruction;
end;

procedure TpvGUIInstance.SetUpdateBufferIndex(const aUpdateBufferIndex:TpvInt32);
begin
 fUpdateBufferIndex:=aUpdateBufferIndex;
end;

procedure TpvGUIInstance.SetDrawBufferIndex(const aDrawBufferIndex:TpvInt32);
begin
 fDrawBufferIndex:=aDrawBufferIndex;
end;

procedure TpvGUIInstance.ClearReferenceCountedObjectList;
var Index:TpvInt32;
    Buffer:PpvGUIInstanceBuffer;
begin
 if (fUpdateBufferIndex>=0) and (fUpdateBufferIndex<fCountBuffers) then begin
  Buffer:=@fBuffers[fUpdateBufferIndex];
  for Index:=0 to Buffer^.CountReferenceCountedObjects-1 do begin
   Buffer^.ReferenceCountedObjects[Index].DecRef;
  end;
  Buffer^.CountReferenceCountedObjects:=0;
 end;
end;

procedure TpvGUIInstance.AddReferenceCountedObjectForNextDraw(const aObject:TpvReferenceCountedObject);
var Index:TpvInt32;
    Buffer:PpvGUIInstanceBuffer;
begin
 if assigned(aObject) and ((fUpdateBufferIndex>=0) and (fUpdateBufferIndex<fCountBuffers)) then begin
  Buffer:=@fBuffers[fUpdateBufferIndex];
  Index:=Buffer^.CountReferenceCountedObjects;
  inc(Buffer^.CountReferenceCountedObjects);
  if length(Buffer^.ReferenceCountedObjects)<Buffer^.CountReferenceCountedObjects then begin
   SetLength(Buffer^.ReferenceCountedObjects,Buffer^.CountReferenceCountedObjects*2);
  end;
  Buffer^.ReferenceCountedObjects[Index]:=aObject;
  aObject.IncRef;
 end;
end;

procedure TpvGUIInstance.UpdateFocus(const aWidget:TpvGUIWidget);
var CurrentIndex:TpvInt32;
    Current:TpvGUIObject;
    CurrentWidget:TpvGUIWidget;
begin
 for CurrentIndex:=0 to fFocusPath.Count-1 do begin
  Current:=fFocusPath.Items[CurrentIndex];
  if Current is TpvGUIWidget then begin
   CurrentWidget:=Current as TpvGUIWidget;
   if CurrentWidget.Focused then begin
    CurrentWidget.Leave;
   end;
  end;
 end;
 fFocusPath.Clear;
 TpvReferenceCountedObject.DecRefOrFreeAndNil(fWindow);
 CurrentWidget:=aWidget;
 while assigned(CurrentWidget) do begin
  fFocusPath.Add(CurrentWidget);
  if CurrentWidget is TpvGUIWindow then begin
   TpvReferenceCountedObject.DecRefOrFreeAndNil(fWindow);
   fWindow:=CurrentWidget as TpvGUIWindow;
   fWindow.IncRef;
  end;
  if assigned(CurrentWidget.fParent) and (CurrentWidget.fParent is TpvGUIWidget) then begin
   CurrentWidget:=CurrentWidget.fParent as TpvGUIWidget;
  end else begin
   break;
  end;
 end;
 for CurrentIndex:=0 to fFocusPath.Count-1 do begin
  Current:=fFocusPath.Items[CurrentIndex];
  if Current is TpvGUIWidget then begin
   CurrentWidget:=Current as TpvGUIWidget;
   CurrentWidget.Enter;
  end;
 end;
 if assigned(fWindow) then begin
  MoveWindowToFront(fWindow);
 end;
end;

procedure TpvGUIInstance.DisposeWindow(const aWindow:TpvGUIWindow);
begin
 if assigned(aWindow) then begin
  if assigned(fFocusPath) and fFocusPath.Contains(aWindow) then begin
   fFocusPath.Clear;
  end;
  if fDragWidget=aWindow then begin
   TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
  end;
  if assigned(fChildren) and fChildren.Contains(aWindow) then begin
   fChildren.Remove(aWindow);
  end;
 end;
end;

procedure TpvGUIInstance.CenterWindow(const aWindow:TpvGUIWindow);
begin
 if assigned(aWindow) then begin
  if aWindow.fSize=TpvVector2.Null then begin
   aWindow.fSize:=aWindow.PreferredSize;
   aWindow.PerformLayout;
  end;
  aWindow.fPosition:=(fSize-aWindow.fSize)*0.5;
 end;
end;

procedure TpvGUIInstance.MoveWindowToFront(const aWindow:TpvGUIWindow);
var Index,BaseIndex:TpvInt32;
    Changed:boolean;
    Current:TpvGUIObject;
//  PopupWidget:TpvGUIPopup;
begin
 if assigned(aWindow) then begin
  Index:=fChildren.IndexOf(aWindow);
  if Index>=0 then begin
   if Index<>(fChildren.Count-1) then begin
    fChildren.Move(Index,fChildren.Count-1);
   end;
   repeat
    Changed:=false;
    BaseIndex:=0;
    for Index:=0 to fChildren.Count-1 do begin
     if fChildren[Index]=aWindow then begin
      BaseIndex:=Index;
      break;
     end;
    end;
    for Index:=0 to fChildren.Count-1 do begin
     Current:=fChildren[Index];
     if assigned(Current) then begin
{     if Current is TpvGUIPopup then begin
       PopupWidget:=Current as TpvGUIPopup;
       if (PopupWidget.ParentWindow=aWindow) and (Index<BaseIndex) then begin
        MoveWindowToFront(PopupWidget);
        Changed:=true;
        break;
       end;
      end;}
     end;
    end;
   until not Changed;
  end;
 end;
end;

function TpvGUIInstance.PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 result:=inherited PointerDown(aPosition,aPressure,aPointerID,aButton);
 if not result then begin
  RequestFocus;
 end;
end;

function TpvGUIInstance.PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 result:=inherited PointerUp(aPosition,aPressure,aPointerID,aButton);
end;

function TpvGUIInstance.PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 result:=inherited PointerMotion(aPosition,aRelativePosition,aPressure,aPointerID,aButton);
end;

function TpvGUIInstance.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=inherited Scrolled(aPosition,aRelativeAmount);
end;

procedure TpvGUIInstance.Update;
begin
 ClearReferenceCountedObjectList;
 inherited Update;
end;

procedure TpvGUIInstance.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIWindow.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fTitle:='Window';
 fMouseAction:=pvgwmaNone;
 fFocused:=false;
 fModal:=false;
 fResizable:=true;
 fButtonPanel:=nil;
end;

destructor TpvGUIWindow.Destroy;
begin
 inherited Destroy;
end;

procedure TpvGUIWindow.AfterConstruction;
begin
 inherited AfterConstruction;
end;

procedure TpvGUIWindow.BeforeDestruction;
begin
 if assigned(fInstance) then begin
  fInstance.DisposeWindow(self);
 end;
 inherited BeforeDestruction;
end;

procedure TpvGUIWindow.DisposeWindow;
begin
 if assigned(fInstance) then begin
  fInstance.DisposeWindow(self);
 end;
end;

function TpvGUIWindow.GetButtonPanel:TpvGUIWidget;
begin
 result:=fButtonPanel;
end;

function TpvGUIWindow.GetPreferredSize:TpvVector2;
begin
 if assigned(fButtonPanel) then begin
  fButtonPanel.Visible:=false;
 end;
 result:=inherited GetPreferredSize;
 if assigned(fButtonPanel) then begin
  fButtonPanel.Visible:=true;
 end;
end;

procedure TpvGUIWindow.PerformLayout;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 if assigned(fButtonPanel) then begin
  fButtonPanel.Visible:=false;
  inherited PerformLayout;
  fButtonPanel.Visible:=true;
  for ChildIndex:=0 to fButtonPanel.fChildren.Count-1 do begin
   Child:=fButtonPanel.fChildren.Items[ChildIndex];
   if Child is TpvGUIWidget then begin
    ChildWidget:=Child as TpvGUIWidget;
    ChildWidget.FixedWidth:=22;
    ChildWidget.FixedHeight:=22;
    ChildWidget.FontSize:=-15;
   end;
  end;
  fButtonPanel.Width:=Width;
  fButtonPanel.Height:=22;
  fButtonPanel.Left:=Width-(fButtonPanel.PreferredSize.x+5);
  fButtonPanel.Top:=3;
  fButtonPanel.PerformLayout;
 end else begin
  inherited PerformLayout;
 end;
end;

procedure TpvGUIWindow.RefreshRelativePlacement;
begin

end;

procedure TpvGUIWindow.Center;
begin
 if assigned(fInstance) then begin
  fInstance.CenterWindow(self);
 end;
end;

function TpvGUIWindow.PointerDown(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 result:=inherited PointerDown(aPosition,aPressure,aPointerID,aButton);
 fMouseAction:=pvgwmaNone;
 if (not result) and ((aPosition.y-fPosition.y)<Theme.fWindowHeaderHeight) then begin
  fMouseAction:=pvgwmaMove;
 end;
 if (not result) and
    ((aPosition.x-fPosition.x)>(fSize.x-(Theme.fWindowGripWidth+Theme.fWindowGripPaddingRight))) and
    ((aPosition.y-fPosition.y)>(fSize.y-(Theme.fWindowGripHeight+Theme.fWindowGripPaddingBottom))) and
    fResizable then begin
  fMouseAction:=pvgwmaSize;
 end;
 if not fFocused then begin
  RequestFocus;
 end;
 result:=true;
end;

function TpvGUIWindow.PointerUp(const aPosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 result:=inherited PointerUp(aPosition,aPressure,aPointerID,aButton);
 fMouseAction:=pvgwmaNone;
 result:=true;
end;

function TpvGUIWindow.PointerMotion(const aPosition,aRelativePosition:TpvVector2;const aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 if (fMouseAction=pvgwmaMove) and ((aButton and BUTTON_LEFT)<>0) then begin
  fPosition:=fPosition+aRelativePosition;
 end;
 if (fMouseAction=pvgwmaSize) and ((aButton and BUTTON_LEFT)<>0) then begin
  fSize.x:=Max(32.0++Theme.fWindowGripWidth+Theme.fWindowGripPaddingRight,fSize.x+aRelativePosition.x);
  fSize.y:=Max(Max(32.0,Theme.fWindowHeaderHeight+Theme.fWindowGripHeight+Theme.fWindowGripPaddingBottom),fSize.y+aRelativePosition.y);
 end;
 result:=inherited PointerMotion(aPosition,aRelativePosition,aPressure,aPointerID,aButton);
end;

function TpvGUIWindow.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 inherited Scrolled(aPosition,aRelativeAmount);
 result:=true;
end;

procedure TpvGUIWindow.Update;
begin
 fCanvas.Push;
 try
  fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
  if fFocused then begin
   fCanvas.DrawNinePatchSprite(Theme.fSpriteFocusedWindowFill,
                               Theme.fSpriteFocusedWindowFillNinePatch,
                               TpvVector2.Null,
                               fSize);
   fCanvas.DrawNinePatchSprite(Theme.fSpriteFocusedWindowHeader,
                               Theme.fSpriteFocusedWindowHeaderNinePatch,
                               TpvVector2.Null,
                               TpvVector2.Create(fSize.x,Theme.fSpriteFocusedWindowHeader.Height));
   if Resizable then begin
    fCanvas.DrawNinePatchSprite(Theme.fSpriteFocusedWindowGrip,
                                Theme.fSpriteFocusedWindowGripNinePatch,
                                TpvVector2.Create(fSize.x-(Theme.fSpriteFocusedWindowGrip.Width+Theme.fWindowGripPaddingRight),fSize.y-(Theme.fSpriteFocusedWindowGrip.Height+Theme.fWindowGripPaddingBottom)),
                                TpvVector2.Create(Theme.fSpriteFocusedWindowGrip.Width,Theme.fSpriteFocusedWindowGrip.Height));
   end;
  end else begin
   fCanvas.DrawNinePatchSprite(Theme.fSpriteUnfocusedWindowFill,
                               Theme.fSpriteUnfocusedWindowFillNinePatch,
                               TpvVector2.Null,
                               fSize);
   fCanvas.DrawNinePatchSprite(Theme.fSpriteUnfocusedWindowHeader,
                               Theme.fSpriteUnfocusedWindowHeaderNinePatch,
                               TpvVector2.Null,
                               TpvVector2.Create(fSize.x,Theme.fSpriteUnfocusedWindowHeader.Height));
   if Resizable then begin
    fCanvas.DrawNinePatchSprite(Theme.fSpriteUnfocusedWindowGrip,
                                Theme.fSpriteUnfocusedWindowGripNinePatch,
                                TpvVector2.Create(fSize.x-(Theme.fSpriteUnfocusedWindowGrip.Width+Theme.fWindowGripPaddingRight),fSize.y-(Theme.fSpriteUnfocusedWindowGrip.Height+Theme.fWindowGripPaddingBottom)),
                                TpvVector2.Create(Theme.fSpriteUnfocusedWindowGrip.Width,Theme.fSpriteUnfocusedWindowGrip.Height));
   end;
  end;
 finally
  fCanvas.Pop;
 end;
 inherited Update;
end;

procedure TpvGUIWindow.Draw;
begin
 inherited Draw;
end;

end.
