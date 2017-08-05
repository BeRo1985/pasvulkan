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
     PasVulkan.Utils,
     PasVulkan.Collections,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Streams,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.TrueTypeFont,
     PasVulkan.Font;

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

     PpvGUILayoutAlignment=^TpvGUILayoutAlignment;
     TpvGUILayoutAlignment=
      (
       pvglaLeading,
       pvglaMiddle,
       pvglaTailing,
       pvglaFill
      );

     PpvGUILayoutOrientation=^TpvGUILayoutOrientation;
     TpvGUILayoutOrientation=
      (
       pvgloHorizontal,
       pvgloVertical
      );

     TpvGUILayout=class(TpvGUIObject)
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; virtual;
       procedure PerformLayout(const aWidget:TpvGUIWidget); virtual;
      public
     end;

     TpvGUIBoxLayout=class(TpvGUILayout)
      private
       fAlignment:TpvGUILayoutAlignment;
       fOrientation:TpvGUILayoutOrientation;
       fMargin:TpvFloat;
       fSpacing:TpvFloat;
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       procedure PerformLayout(const aWidget:TpvGUIWidget); override;
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aAlignment:TpvGUILayoutAlignment=pvglaMiddle;
                          const aOrientation:TpvGUILayoutOrientation=pvgloHorizontal;
                          const aMargin:TpvFloat=0.0;
                          const aSpacing:TpvFloat=0.0); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Alignment:TpvGUILayoutAlignment read fAlignment write fAlignment;
       property Orientation:TpvGUILayoutOrientation read fOrientation write fOrientation;
       property Margin:TpvFloat read fMargin write fMargin;
       property Spacing:TpvFloat read fSpacing write fSpacing;
     end;

     TpvGUISkin=class(TpvGUIObject)
      private
      protected
       fFontSize:TpvFloat;
       fUnfocusedWindowHeaderFontSize:TpvFloat;
       fFocusedWindowHeaderFontSize:tpvFloat;
       fUnfocusedWindowHeaderFontShadow:boolean;
       fFocusedWindowHeaderFontShadow:boolean;
       fUnfocusedWindowHeaderFontShadowOffset:TpvVector2;
       fFocusedWindowHeaderFontShadowOffset:TpvVector2;
       fUnfocusedWindowHeaderFontShadowColor:TpvVector4;
       fFocusedWindowHeaderFontShadowColor:TpvVector4;
       fUnfocusedWindowHeaderFontColor:TpvVector4;
       fFocusedWindowHeaderFontColor:TpvVector4;
       fMipmappedSpriteAtlas:TpvSpriteAtlas;
       fSpriteAtlas:TpvSpriteAtlas;
       fSansFont:TpvFont;
       fMonoFont:TpvFont;
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
       fSpriteUnfocusedWindowShadow:TpvSprite;
       fSpriteUnfocusedWindowShadowNinePatch:TpvSpriteNinePatch;
       fSpriteFocusedWindowShadow:TpvSprite;
       fSpriteFocusedWindowShadowNinePatch:TpvSpriteNinePatch;
       fSpriteMouseCursorArrow:TpvSprite;
       fSpriteMouseCursorBeam:TpvSprite;
       fSpriteMouseCursorBusy:TpvSprite;
       fSpriteMouseCursorCross:TpvSprite;
       fSpriteMouseCursorEW:TpvSprite;
       fSpriteMouseCursorHelp:TpvSprite;
       fSpriteMouseCursorLink:TpvSprite;
       fSpriteMouseCursorMove:TpvSprite;
       fSpriteMouseCursorNESW:TpvSprite;
       fSpriteMouseCursorNS:TpvSprite;
       fSpriteMouseCursorNWSE:TpvSprite;
       fSpriteMouseCursorPen:TpvSprite;
       fSpriteMouseCursorUnavailable:TpvSprite;
       fSpriteMouseCursorUp:TpvSprite;
       fWindowHeaderHeight:TpvFloat;
       fWindowResizeGripSize:TpvFloat;
       fWindowMinimumWidth:TpvFloat;
       fWindowMinimumHeight:TpvFloat;
       fWindowShadowWidth:TpvFloat;
       fWindowShadowHeight:TpvFloat;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure Setup; virtual;
      public
       property UnfocusedWindowHeaderFontShadowOffset:TpvVector2 read fUnfocusedWindowHeaderFontShadowOffset write fUnfocusedWindowHeaderFontShadowOffset;
       property FocusedWindowHeaderFontShadowOffset:TpvVector2 read fFocusedWindowHeaderFontShadowOffset write fFocusedWindowHeaderFontShadowOffset;
       property UnfocusedWindowHeaderFontShadowColor:TpvVector4 read fUnfocusedWindowHeaderFontShadowColor write fUnfocusedWindowHeaderFontShadowColor;
       property FocusedWindowHeaderFontShadowColor:TpvVector4 read fFocusedWindowHeaderFontShadowColor write fFocusedWindowHeaderFontShadowColor;
       property UnfocusedWindowHeaderFontColor:TpvVector4 read fUnfocusedWindowHeaderFontColor write fUnfocusedWindowHeaderFontColor;
       property FocusedWindowHeaderFontColor:TpvVector4 read fFocusedWindowHeaderFontColor write fFocusedWindowHeaderFontColor;
       property SpriteUnfocusedWindowFillNinePatch:TpvSpriteNinePatch read fSpriteUnfocusedWindowFillNinePatch write fSpriteUnfocusedWindowFillNinePatch;
       property SpriteFocusedWindowFillNinePatch:TpvSpriteNinePatch read fSpriteFocusedWindowFillNinePatch write fSpriteFocusedWindowFillNinePatch;
       property SpriteUnfocusedWindowHeaderNinePatch:TpvSpriteNinePatch read fSpriteUnfocusedWindowHeaderNinePatch write fSpriteUnfocusedWindowHeaderNinePatch;
       property SpriteFocusedWindowHeaderNinePatch:TpvSpriteNinePatch read fSpriteFocusedWindowHeaderNinePatch write fSpriteFocusedWindowHeaderNinePatch;
       property SpriteUnfocusedWindowGripNinePatch:TpvSpriteNinePatch read fSpriteUnfocusedWindowGripNinePatch write fSpriteUnfocusedWindowGripNinePatch;
       property SpriteFocusedWindowGripNinePatch:TpvSpriteNinePatch read fSpriteFocusedWindowGripNinePatch write fSpriteFocusedWindowGripNinePatch;
       property SpriteUnfocusedWindowShadowNinePatch:TpvSpriteNinePatch read fSpriteUnfocusedWindowShadowNinePatch write fSpriteUnfocusedWindowShadowNinePatch;
       property SpriteFocusedWindowShadowNinePatch:TpvSpriteNinePatch read fSpriteFocusedWindowShadowNinePatch write fSpriteFocusedWindowShadowNinePatch;
       property SpriteMouseCursorArrow:TpvSprite read fSpriteMouseCursorArrow write fSpriteMouseCursorArrow;
       property SpriteMouseCursorBeam:TpvSprite read fSpriteMouseCursorBeam write fSpriteMouseCursorBeam;
       property SpriteMouseCursorBusy:TpvSprite read fSpriteMouseCursorBusy write fSpriteMouseCursorBusy;
       property SpriteMouseCursorCross:TpvSprite read fSpriteMouseCursorCross write fSpriteMouseCursorCross;
       property SpriteMouseCursorEW:TpvSprite read fSpriteMouseCursorEW write fSpriteMouseCursorEW;
       property SpriteMouseCursorHelp:TpvSprite read fSpriteMouseCursorHelp write fSpriteMouseCursorHelp;
       property SpriteMouseCursorLink:TpvSprite read fSpriteMouseCursorLink write fSpriteMouseCursorLink;
       property SpriteMouseCursorMove:TpvSprite read fSpriteMouseCursorMove write fSpriteMouseCursorMove;
       property SpriteMouseCursorNESW:TpvSprite read fSpriteMouseCursorNESW write fSpriteMouseCursorNESW;
       property SpriteMouseCursorNS:TpvSprite read fSpriteMouseCursorNS write fSpriteMouseCursorNS;
       property SpriteMouseCursorNWSE:TpvSprite read fSpriteMouseCursorNWSE write fSpriteMouseCursorNWSE;
       property SpriteMouseCursorPen:TpvSprite read fSpriteMouseCursorPen write fSpriteMouseCursorPen;
       property SpriteMouseCursorUnavailable:TpvSprite read fSpriteMouseCursorUnavailable write fSpriteMouseCursorUnavailable;
       property SpriteMouseCursorUp:TpvSprite read fSpriteMouseCursorUp write fSpriteMouseCursorUp;
      published
       property FontSize:TpvFloat read fFontSize write fFontSize;
       property UnfocusedWindowHeaderFontSize:TpvFloat read fUnfocusedWindowHeaderFontSize write fUnfocusedWindowHeaderFontSize;
       property FocusedWindowHeaderFontSize:TpvFloat read fFocusedWindowHeaderFontSize write fFocusedWindowHeaderFontSize;
       property UnfocusedWindowHeaderFontShadow:boolean read fUnfocusedWindowHeaderFontShadow write fUnfocusedWindowHeaderFontShadow;
       property FocusedWindowHeaderFontShadow:boolean read fFocusedWindowHeaderFontShadow write fFocusedWindowHeaderFontShadow;
       property MipmappedSpriteAtlas:TpvSpriteAtlas read fMipmappedSpriteAtlas;
       property SpriteAtlas:TpvSpriteAtlas read fSpriteAtlas;
       property SpriteUnfocusedWindowFill:TpvSprite read fSpriteUnfocusedWindowFill write fSpriteUnfocusedWindowFill;
       property SpriteFocusedWindowFill:TpvSprite read fSpriteFocusedWindowFill write fSpriteFocusedWindowFill;
       property SpriteUnfocusedWindowHeader:TpvSprite read fSpriteUnfocusedWindowHeader write fSpriteUnfocusedWindowHeader;
       property SpriteFocusedWindowHeader:TpvSprite read fSpriteFocusedWindowHeader write fSpriteFocusedWindowHeader;
       property SpriteUnfocusedWindowGrip:TpvSprite read fSpriteUnfocusedWindowGrip write fSpriteUnfocusedWindowGrip;
       property SpriteFocusedWindowGrip:TpvSprite read fSpriteFocusedWindowGrip write fSpriteFocusedWindowGrip;
       property SpriteUnfocusedWindowShadow:TpvSprite read fSpriteUnfocusedWindowShadow write fSpriteUnfocusedWindowShadow;
       property SpriteFocusedWindowShadow:TpvSprite read fSpriteFocusedWindowShadow write fSpriteFocusedWindowShadow;
       property WindowHeaderHeight:TpvFloat read fWindowHeaderHeight write fWindowHeaderHeight;
       property WindowGripPaddingRight:TpvFloat read fWindowResizeGripSize write fWindowResizeGripSize;
       property WindowMinimumWidth:TpvFloat read fWindowMinimumWidth write fWindowMinimumWidth;
       property WindowMinimumHeight:TpvFloat read fWindowMinimumHeight write fWindowMinimumHeight;
       property WindowShadowWidth:TpvFloat read fWindowShadowWidth write fWindowShadowWidth;
       property WindowShadowHeight:TpvFloat read fWindowShadowHeight write fWindowShadowHeight;
     end;

     PpvGUICursor=^TpvGUICursor;
     TpvGUICursor=
      (
       pvgcNone,
       pvgcArrow,
       pvgcBeam,
       pvgcBusy,
       pvgcCross,
       pvgcEW,
       pvgcHelp,
       pvgcLink,
       pvgcMove,
       pvgcNESW,
       pvgcNS,
       pvgcNWSE,
       pvgcPen,
       pvgcUnavailable,
       pvgcUp
      );

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

     PpvGUIWidgetFlag=^TpvGUIWidgetFlag;
     TpvGUIWidgetFlag=
      (
       pvgwfEnabled,
       pvgwfVisible,
       pvgwfFocused,
       pvgwfPointerFocused
      );

     PpvGUIWidgetFlags=^TpvGUIWidgetFlags;
     TpvGUIWidgetFlags=set of TpvGUIWidgetFlag;

     TpvGUIWidget=class(TpvGUIObject)
      public
       const DefaultFlags=[pvgwfEnabled,
                           pvgwfVisible];
      private
      protected
       fCanvas:TpvCanvas;
       fLayout:TpvGUILayout;
       fSkin:TpvGUISkin;
       fCursor:TpvGUICursor;
       fPosition:TpvVector2;
       fSize:TpvVector2;
       fFixedSize:TpvVector2;
       fPositionProperty:TpvVector2Property;
       fSizeProperty:TpvVector2Property;
       fFixedSizeProperty:TpvVector2Property;
       fWidgetFlags:TpvGUIWidgetFlags;
       fHint:TpvUTF8String;
       fFontSize:TpvFloat;
       function GetEnabled:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetEnabled(const aEnabled:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetVisible:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetVisible(const aVisible:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetFocused:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetFocused(const aFocused:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetPointerFocused:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetPointerFocused(const aPointerFocused:boolean); {$ifdef CAN_INLINE}inline;{$endif}
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
       procedure SetCanvas(const aCanvas:TpvCanvas); virtual;
       function GetSkin:TpvGUISkin; virtual;
       procedure SetSkin(const aSkin:TpvGUISkin); virtual;
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
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; virtual;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; virtual;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; virtual;
       procedure AfterCreateSwapChain; virtual;
       procedure BeforeDestroySwapChain; virtual;
       procedure Update; virtual;
       procedure Draw; virtual;
      public
       property AbsolutePosition:TpvVector2 read GetAbsolutePosition;
       property PreferredSize:TpvVector2 read GetPreferredSize;
      published
       property Window:TpvGUIWindow read GetWindow;
       property Canvas:TpvCanvas read fCanvas write SetCanvas;
       property Layout:TpvGUILayout read fLayout write fLayout;
       property Skin:TpvGUISkin read GetSkin write SetSkin;
       property Cursor:TpvGUICursor read fCursor write fCursor;
       property Position:TpvVector2Property read fPositionProperty;
       property Size:TpvVector2Property read fSizeProperty;
       property FixedSize:TpvVector2Property read fFixedSizeProperty;
       property WidgetFlags:TpvGUIWidgetFlags read fWidgetFlags write fWidgetFlags;
       property Enabled:boolean read GetEnabled write SetEnabled;
       property Visible:boolean read GetVisible write SetVisible;
       property RecursiveVisible:boolean read GetRecursiveVisible;
       property Focused:boolean read GetFocused write SetFocused;
       property PointerFocused:boolean read GetPointerFocused write SetPointerFocused;
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
       fStandardSkin:TpvGUISkin;
       fDrawWidgetBounds:boolean;
       fBuffers:TpvGUIInstanceBuffers;
       fCountBuffers:TpvInt32;
       fUpdateBufferIndex:TpvInt32;
       fDrawBufferIndex:TpvInt32;
       fDeltaTime:TpvDouble;
       fTime:TpvDouble;
       fLastFocusPath:TpvGUIObjectList;
       fCurrentFocusPath:TpvGUIObjectList;
       fDragWidget:TpvGUIWidget;
       fWindow:TpvGUIWindow;
       fMousePosition:TpvVector2;
       fVisibleCursor:TpvGUICursor;
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
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      public
       property MousePosition:TpvVector2 read fMousePosition write fMousePosition;
      published
       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;
       property StandardSkin:TpvGUISkin read fStandardSkin;
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
       pvgwmaSizeNW,
       pvgwmaSizeNE,
       pvgwmaSizeSW,
       pvgwmaSizeSE,
       pvgwmaSizeN,
       pvgwmaSizeS,
       pvgwmaSizeW,
       pvgwmaSizeE
      );

     PpvGUIWindowFlag=^TpvGUIWindowFlag;
     TpvGUIWindowFlag=
      (
       pvgwfModal,
       pvgwfHeader,
       pvgwfMovable,
       pvgwfResizableNW,
       pvgwfResizableNE,
       pvgwfResizableSW,
       pvgwfResizableSE,
       pvgwfResizableN,
       pvgwfResizableS,
       pvgwfResizableW,
       pvgwfResizableE
      );

     PpvGUIWindowFlags=^TpvGUIWindowFlags;
     TpvGUIWindowFlags=set of TpvGUIWindowFlag;

     TpvGUIWindow=class(TpvGUIWidget)
      public
       const DefaultFlags=[pvgwfHeader,
                           pvgwfMovable,
                           pvgwfResizableNW,
                           pvgwfResizableNE,
                           pvgwfResizableSW,
                           pvgwfResizableSE,
                           pvgwfResizableN,
                           pvgwfResizableS,
                           pvgwfResizableW,
                           pvgwfResizableE];
      private
      protected
       fTitle:TpvUTF8String;
       fMouseAction:TpvGUIWindowMouseAction;
       fWindowFlags:TpvGUIWindowFlags;
       fButtonPanel:TpvGUIWidget;
       function GetModal:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetModal(const aModal:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetButtonPanel:TpvGUIWidget;
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
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Title:TpvUTF8String read fTitle write fTitle;
       property WindowFlags:TpvGUIWindowFlags read fWindowFlags write fWindowFlags;
       property Modal:boolean read GetModal write SetModal;
       property ButtonPanel:TpvGUIWidget read GetButtonPanel;
     end;

     TpvGUILabel=class(TpvGUIWidget)
      private
       fCaption:TpvUTF8String;
      protected
       function GetPreferredSize:TpvVector2; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Caption:TpvUTF8String read fCaption write fCaption;
     end;

implementation

uses PasVulkan.Assets,
     PasVulkan.VectorPath,
     PasVulkan.Image.PNG;

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

constructor TpvGUIBoxLayout.Create(const aParent:TpvGUIObject;
                                   const aAlignment:TpvGUILayoutAlignment=pvglaMiddle;
                                   const aOrientation:TpvGUILayoutOrientation=pvgloHorizontal;
                                   const aMargin:TpvFloat=0.0;
                                   const aSpacing:TpvFloat=0.0);
begin
 inherited Create(aParent);
 fAlignment:=aAlignment;
 fOrientation:=aOrientation;
 fMargin:=aMargin;
 fSpacing:=aSpacing;
end;

destructor TpvGUIBoxLayout.Destroy;
begin
 inherited Destroy;
end;

function TpvGUIBoxLayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
var Axis0,Axis1,ChildIndex:TpvInt32;
    YOffset:TpvFloat;
    Size,ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
    First:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 Size:=TpvVector2.Create(fMargin*2.0,fMargin*2.0);
 YOffset:=0;
 if (aWidget is TpvGUIWindow) and
    (pvgwfHeader in (aWidget as TpvGUIWindow).fWindowFlags) then begin
  case fOrientation of
   pvgloHorizontal:begin
    YOffset:=aWidget.Skin.WindowHeaderHeight;
   end;
   pvgloVertical:begin
    Size.y:=Size.y+(aWidget.Skin.WindowHeaderHeight-(fMargin*0.5));
   end;
  end;
 end;
 case fOrientation of
  pvgloHorizontal:begin
   Axis0:=0;
   Axis1:=1;
  end;
  else begin
   Axis0:=1;
   Axis1:=0;
  end;
 end;
 First:=true;
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    if not First then begin
     Size[Axis0]:=Size[Axis0]+fSpacing;
    end;
    ChildPreferredSize:=ChildWidget.PreferredSize;
    ChildFixedSize:=ChildWidget.fFixedSize;
    if ChildFixedSize.x>0.0 then begin
     ChildTargetSize.x:=ChildFixedSize.x;
    end else begin
     ChildTargetSize.x:=ChildPreferredSize.x;
    end;
    if ChildFixedSize.y>0.0 then begin
     ChildTargetSize.y:=ChildFixedSize.y;
    end else begin
     ChildTargetSize.y:=ChildPreferredSize.y;
    end;
    Size[Axis0]:=Size[Axis0]+ChildTargetSize[Axis0];
    Size[Axis1]:=Max(Size[Axis1],ChildTargetSize[Axis1]+(fMargin*2.0));
    First:=false;
   end;
  end;
 end;
 result:=Size+TpvVector2.Create(0.0,YOffset);
end;

procedure TpvGUIBoxLayout.PerformLayout(const aWidget:TpvGUIWidget);
var Axis0,Axis1,ChildIndex:TpvInt32;
    Offset,YOffset:TpvFloat;
    FixedSize,ContainerSize,ChildPreferredSize,ChildFixedSize,ChildTargetSize,
    Position:TpvVector2;
    First:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 FixedSize:=aWidget.fFixedSize;
 if FixedSize.x>0.0 then begin
  ContainerSize.x:=FixedSize.x;
 end else begin
  ContainerSize.x:=aWidget.Width;
 end;
 if FixedSize.y>0.0 then begin
  ContainerSize.y:=FixedSize.y;
 end else begin
  ContainerSize.y:=aWidget.Height;
 end;
 case fOrientation of
  pvgloHorizontal:begin
   Axis0:=0;
   Axis1:=1;
  end;
  else begin
   Axis0:=1;
   Axis1:=0;
  end;
 end;
 Offset:=fMargin;
 YOffset:=0;
 if (aWidget is TpvGUIWindow) and
    (pvgwfHeader in (aWidget as TpvGUIWindow).fWindowFlags) then begin
  case fOrientation of
   pvgloHorizontal:begin
    YOffset:=aWidget.Skin.WindowHeaderHeight;
    ContainerSize.y:=ContainerSize.y-YOffset;
   end;
   pvgloVertical:begin
    Offset:=Offset+(aWidget.Skin.WindowHeaderHeight-(fMargin*0.5));
   end;
  end;
 end;
 First:=true;
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    if not First then begin
     Offset:=Offset+fSpacing;
    end;
    ChildPreferredSize:=ChildWidget.PreferredSize;
    ChildFixedSize:=ChildWidget.fFixedSize;
    if ChildFixedSize.x>0.0 then begin
     ChildTargetSize.x:=ChildFixedSize.x;
    end else begin
     ChildTargetSize.x:=ChildPreferredSize.x;
    end;
    if ChildFixedSize.y>0.0 then begin
     ChildTargetSize.y:=ChildFixedSize.y;
    end else begin
     ChildTargetSize.y:=ChildPreferredSize.y;
    end;
    Position:=TpvVector2.Create(0,YOffset);
    Position[Axis0]:=Offset;
    case fAlignment of
     pvglaLeading:begin
      Position[Axis1]:=Position[Axis1]+fMargin;
     end;
     pvglaMiddle:begin
      Position[Axis1]:=Position[Axis1]+((ContainerSize[Axis1]-ChildTargetSize[Axis1])*0.5);
     end;
     pvglaTailing:begin
      Position[Axis1]:=Position[Axis1]+((ContainerSize[Axis1]-ChildTargetSize[Axis1])-(fMargin*2.0));
     end;
     else {pvglaFill:}begin
      Position[Axis1]:=Position[Axis1]+fMargin;
      if ChildFixedSize[Axis1]>0.0 then begin
       ChildTargetSize[Axis1]:=ChildFixedSize[Axis1];
      end else begin
       ChildTargetSize[Axis1]:=ContainerSize[Axis1]-(fMargin*2.0);
      end;
     end;
    end;
    ChildWidget.fPosition:=Position;
    ChildWidget.fSize:=ChildTargetSize;
    ChildWidget.PerformLayout;
    Offset:=Offset+ChildTargetSize[Axis0];
    First:=false;
   end;
  end;
 end;
end;

constructor TpvGUISkin.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fMipmappedSpriteAtlas:=nil;
 fSpriteAtlas:=nil;
 fSansFont:=nil;
 fMonoFont:=nil;
 Setup;
end;

destructor TpvGUISkin.Destroy;
begin
 FreeAndNil(fSansFont);
 FreeAndNil(fMonoFont);
 FreeAndNil(fSpriteAtlas);
 FreeAndNil(fMipmappedSpriteAtlas);
 inherited Destroy;
end;

procedure TpvGUISkin.Setup;
 function sdRoundBox(const p,b:TpvVector2;const r:TpvFloat):TpvFloat;
 var d:TpvVector2;
 begin
  d.x:=abs(p.x)-b.x;
  d.y:=abs(p.y)-b.y;
  result:=(Min(Max(d.x,d.y),0.0)+sqrt(sqr(Max(0.0,d.x))+sqr(Max(0.0,d.y))))-r;
 end;
 procedure CreateMouseCursorSprite(out aSprite:TpvSprite;
                                   const aDrawScale:TpvFloat;
                                   const aOffsetX:TpvFloat;
                                   const aOffsetY:TpvFloat;
                                   const aShadow:TpvFloat;
                                   const aShadowX:TpvFloat;
                                   const aShadowY:TpvFloat;
                                   const aDataPointer:TpvPointer;
                                   const aDataSize:TpvUInt32); overload;
 var Index,ImageWidth,ImageHeight:TpvInt32;
     ImageData:TpvPointer;
     p8:PpvUInt8;
     p16:PpvUInt16;
     PNGPixelFormat:TpvPNGPixelFormat;
 begin
  ImageData:=nil;
  try
   PNGPixelFormat:=pvppfUnknown;
   if LoadPNGImage(aDataPointer,aDataSize,ImageData,ImageWidth,ImageHeight,false,PNGPixelFormat) then begin
    if PNGPixelFormat=pvppfR16G16B16A16 then begin
     // Convert to R8G8B8A8 in-placve
     p8:=ImageData;
     p16:=ImageData;
     for Index:=1 to ImageWidth*ImageHeight*4 do begin
      p8^:=p16^ shr 8;
      inc(p8);
      inc(p16);
     end;
    end;
    aSprite:=fMipmappedSpriteAtlas.LoadRawSprite('',ImageData,ImageWidth,ImageHeight,true);
    aSprite.OffsetX:=aOffsetX;
    aSprite.OffsetY:=aOffsetY;
    aSprite.ScaleX:=aDrawScale;
    aSprite.ScaleY:=aDrawScale;
   end;
  finally
   if assigned(ImageData) then begin
    FreeMem(ImageData);
   end;
  end;
 end;
 procedure CreateMouseCursorSprite(out aSprite:TpvSprite;
                                   const aWidth:TpvInt32;
                                   const aHeight:TpvInt32;
                                   const aDrawScale:TpvFloat;
                                   const aScale:TpvFloat;
                                   const aRootX:TpvFloat;
                                   const aRootY:TpvFloat;
                                   const aOffsetX:TpvFloat;
                                   const aOffsetY:TpvFloat;
                                   const aShadow:TpvFloat;
                                   const aShadowX:TpvFloat;
                                   const aShadowY:TpvFloat;
                                   const aSVGPath:TpvRawByteString); overload;
 var x,y,Index,InsideOutsideSign:TpvInt32;
     ImageData:array of TpvSpriteTextureTexel;
     Color:TpvSpriteTextureTexel;
     c,a:TpvVector4;
     f:TpvFloat;
     InverseScale:TpvDouble;
     VectorPath:TpvVectorPath;
 begin
  VectorPath:=TpvVectorPath.CreateFromSVGPath(aSVGPath);
  try
   ImageData:=nil;
   try
    SetLength(ImageData,aWidth*aHeight);
    Index:=0;
    for y:=0 to aHeight-1 do begin
     for x:=0 to aWidth-1 do begin
      f:=VectorPath.GetSignedDistance(x+(aRootX*aScale),
                                      y+(aRootY*aScale),
                                      aScale,
                                      InsideOutsideSign)*aDrawScale;
      c:=Mix(TpvVector4.Create(0.0,0.0,0.0,0.0),
             Mix(TpvVector4.Create(0.0,0.0,0.0,1.0),
                 TpvVector4.Create(1.0,1.0,1.0,1.0),
                 LinearStep(1.0,-1.0,f*InsideOutsideSign)),
             LinearStep(2.0,0.0,f*InsideOutsideSign));
      if aShadow>0.0 then begin
       if not (IsZero(aShadowX) and IsZero(aShadowY)) then begin
        f:=VectorPath.GetSignedDistance(x+((aRootX-aShadowX)*aScale),
                                        y+((aRootY-aShadowY)*aScale),
                                        aScale,
                                        InsideOutsideSign)*aDrawScale;
       end;
       c:=Mix(Mix(TpvVector4.Create(0.0,0.0,0.0,0.0),
                  TpvVector4.Create(0.0,0.0,0.0,0.5),
                  LinearStep(aShadow,0.0,f*InsideOutsideSign)),
              c,
              c.a);
      end;
      Color.r:=Min(Max(round(c.r*255.0),0),255);
      Color.g:=Min(Max(round(c.g*255.0),0),255);
      Color.b:=Min(Max(round(c.b*255.0),0),255);
      Color.a:=Min(Max(round(c.a*255.0),0),255);
      ImageData[Index]:=Color;
      inc(Index);
     end;
    end;
    aSprite:=fMipmappedSpriteAtlas.LoadRawSprite('',@ImageData[0],aWidth,aHeight,true);
    aSprite.OffsetX:=aOffsetX*aScale;
    aSprite.OffsetY:=aOffsetY*aScale;
    aSprite.ScaleX:=aDrawScale;
    aSprite.ScaleY:=aDrawScale;
   finally
    ImageData:=nil;
   end;
  finally
   VectorPath.Free;
  end;
 end;
 procedure CreateWindowFillNinePatchSprite(out aSprite:TpvSprite;
                                           out aSpriteNinePatch:TpvSpriteNinePatch;
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
 procedure CreateWindowHeaderNinePatchSprite(out aSprite:TpvSprite;
                                             out aSpriteNinePatch:TpvSpriteNinePatch;
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
 procedure CreateWindowGripNinePatchSprite(out aSprite:TpvSprite;
                                           out aSpriteNinePatch:TpvSpriteNinePatch;
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
 procedure CreateWindowShadowNinePatchSprite(out aSprite:TpvSprite;
                                             out aSpriteNinePatch:TpvSpriteNinePatch;
                                             const aWidth:TpvInt32;
                                             const aHeight:TpvInt32;
                                             const aRadius:TpvInt32;
                                             const aFillStartColor:TpvVector4;
                                             const aFillStopColor:TpvVector4);
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
     c:=Mix(aFillStartColor,aFillStopColor,LinearStep(0.0,aRadius,f));
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
 procedure CreateNinePatchSprite(out aSprite:TpvSprite;
                                 out aSpriteNinePatch:TpvSpriteNinePatch;
                                 const aRadius:TpvInt32;
                                 const aDataPointer:TpvPointer;
                                 const aDataSize:TpvUInt32);
 var Index,ImageWidth,ImageHeight:TpvInt32;
     ImageData:TpvPointer;
     p8:PpvUInt8;
     p16:PpvUInt16;
     PNGPixelFormat:TpvPNGPixelFormat;
 begin
  ImageData:=nil;
  try
   PNGPixelFormat:=pvppfUnknown;
   if LoadPNGImage(aDataPointer,aDataSize,ImageData,ImageWidth,ImageHeight,false,PNGPixelFormat) then begin
    if PNGPixelFormat=pvppfR16G16B16A16 then begin
     // Convert to R8G8B8A8 in-placve
     p8:=ImageData;
     p16:=ImageData;
     for Index:=1 to ImageWidth*ImageHeight*4 do begin
      p8^:=p16^ shr 8;
      inc(p8);
      inc(p16);
     end;
    end;
    aSprite:=fSpriteAtlas.LoadRawSprite('',ImageData,ImageWidth,ImageHeight,true);
    aSpriteNinePatch.Regions[0,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,0,aRadius,aRadius);
    aSpriteNinePatch.Regions[0,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aRadius,0,ImageWidth-(aRadius*2),aRadius);
    aSpriteNinePatch.Regions[0,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,ImageWidth-aRadius,0,aRadius,aRadius);
    aSpriteNinePatch.Regions[1,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,aRadius,aRadius,ImageHeight-(aRadius*2));
    aSpriteNinePatch.Regions[1,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aRadius,aRadius,ImageWidth-(aRadius*2),ImageHeight-(aRadius*2));
    aSpriteNinePatch.Regions[1,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,ImageWidth-aRadius,aRadius,aRadius,ImageHeight-(aRadius*2));
    aSpriteNinePatch.Regions[2,0]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,0,ImageHeight-aRadius,aRadius,aRadius);
    aSpriteNinePatch.Regions[2,1]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,aRadius,ImageHeight-aRadius,ImageWidth-(aRadius*2),aRadius);
    aSpriteNinePatch.Regions[2,2]:=TpvSpriteNinePatchRegion.Create(pvsnprmStretch,ImageWidth-aRadius,ImageHeight-aRadius,aRadius,aRadius);
   end;
  finally
   if assigned(ImageData) then begin
    FreeMem(ImageData);
   end;
  end;
 end;
const CursorScale=0.18275;
var Stream:TStream;
    TrueTypeFont:TpvTrueTypeFont;
begin

 fFontSize:=-12;

 fUnfocusedWindowHeaderFontSize:=-16;
 fFocusedWindowHeaderFontSize:=-16;

 fUnfocusedWindowHeaderFontShadow:=true;
 fFocusedWindowHeaderFontShadow:=true;

 fUnfocusedWindowHeaderFontShadowOffset:=TpvVector2.Create(2.0,2.0);
 fFocusedWindowHeaderFontShadowOffset:=TpvVector2.Create(2.0,2.0);

 fUnfocusedWindowHeaderFontShadowColor:=TpvVector4.Create(0.0,0.0,0.0,0.3275);
 fFocusedWindowHeaderFontShadowColor:=TpvVector4.Create(0.0,0.0,0.0,0.5);

 fUnfocusedWindowHeaderFontColor:=TpvVector4.Create(0.75,0.75,0.75,1.0);
 fFocusedWindowHeaderFontColor:=TpvVector4.Create(1.0,1.0,1.0,1.0);

 fWindowHeaderHeight:=32;

 fWindowResizeGripSize:=8;

 fWindowMinimumWidth:=Max(fWindowHeaderHeight+8,fWindowResizeGripSize*2);
 fWindowMinimumHeight:=Max(fWindowHeaderHeight+8,fWindowResizeGripSize*2);

 fWindowShadowWidth:=7;
 fWindowShadowHeight:=7;

 fMipmappedSpriteAtlas:=TpvSpriteAtlas.Create(fInstance.fVulkanDevice);

 fSpriteAtlas:=TpvSpriteAtlas.Create(fInstance.fVulkanDevice);

 CreateNinePatchSprite(fSpriteUnfocusedWindowFill,
                       fSpriteUnfocusedWindowFillNinePatch,
                       4,
                       @GUISkinUnfocusedWindowFillData,
                       GUISkinUnfocusedWindowFillDataSize);

 CreateNinePatchSprite(fSpriteFocusedWindowFill,
                       fSpriteFocusedWindowFillNinePatch,
                       4,
                       @GUISkinFocusedWindowFillData,
                       GUISkinFocusedWindowFillDataSize);

 CreateNinePatchSprite(fSpriteUnfocusedWindowHeader,
                       fSpriteUnfocusedWindowHeaderNinePatch,
                       4,
                       @GUISkinUnfocusedWindowHeaderData,
                       GUISkinUnfocusedWindowHeaderDataSize);

 CreateNinePatchSprite(fSpriteFocusedWindowHeader,
                       fSpriteFocusedWindowHeaderNinePatch,
                       4,
                       @GUISkinFocusedWindowHeaderData,
                       GUISkinFocusedWindowHeaderDataSize);

 CreateNinePatchSprite(fSpriteUnfocusedWindowShadow,
                       fSpriteUnfocusedWindowShadowNinePatch,
                       7,
                       @GUISkinUnfocusedWindowShadowData,
                       GUISkinUnfocusedWindowShadowDataSize);

 CreateNinePatchSprite(fSpriteFocusedWindowShadow,
                       fSpriteFocusedWindowShadowNinePatch,
                       7,
                       @GUISkinFocusedWindowShadowData,
                       GUISkinFocusedWindowShadowDataSize);

{CreateWindowFillNinePatchSprite(fSpriteUnfocusedWindowFill,
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


 CreateWindowShadowNinePatchSprite(fSpriteUnfocusedWindowShadow,
                                   fSpriteUnfocusedWindowShadowNinePatch,
                                   64,
                                   64,
                                   16,
                                   TpvVector4.Create(0.0,0.0,0.0,16.0)/255.0,
                                   TpvVector4.Create(0.0,0.0,0.0,0.0)/255.0);

 CreateWindowShadowNinePatchSprite(fSpriteFocusedWindowShadow,
                                   fSpriteFocusedWindowShadowNinePatch,
                                   64,
                                   64,
                                   16,
                                   TpvVector4.Create(0.0,0.0,0.0,64.0)/255.0,
                                   TpvVector4.Create(0.0,0.0,0.0,0.0)/255.0);
 }

 Stream:=TpvDataStream.Create(@GUIStandardTrueTypeFontSansFontData,GUIStandardTrueTypeFontSansFontDataSize);
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fSansFont:=TpvFont.CreateFromTrueTypeFont(pvApplication.VulkanDevice,
                                             fSpriteAtlas,
                                             TrueTypeFont,
                                             [TpvFontCodePointRange.Create(0,255)]);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@GUIStandardTrueTypeFontMonoFontData,GUIStandardTrueTypeFontMonoFontDataSize);
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fMonoFont:=TpvFont.CreateFromTrueTypeFont(pvApplication.VulkanDevice,
                                             fSpriteAtlas,
                                             TrueTypeFont,
                                             [TpvFontCodePointRange.Create(0,255)]);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 CreateMouseCursorSprite(fSpriteMouseCursorArrow,
                         CursorScale,
                         0.0,0.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorArrowData,
                         GUICursorArrowDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorBeam,
                         CursorScale,
                         63.0,62.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorBeamData,
                         GUICursorBeamDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorBusy,
                         0.5,
                         32.0,32.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorBusyData,
                         GUICursorBusyDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorCross,
                         CursorScale,
                         63.0,63.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorCrossData,
                         GUICursorCrossDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorEW,
                         CursorScale,
                         53.0,21.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorEWData,
                         GUICursorEWDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorHelp,
                         CursorScale,
                         0.0,0.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorHelpData,
                         GUICursorHelpDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorLink,
                         CursorScale,
                         40.0,0.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorLinkData,
                         GUICursorLinkDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorMove,
                         CursorScale,
                         54.0,53.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorMoveData,
                         GUICursorMoveDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorNESW,
                         CursorScale,
                         43.0,30.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorNESWData,
                         GUICursorNESWDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorNS,
                         CursorScale,
                         21.0,51.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorNSData,
                         GUICursorNSDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorNWSE,
                         CursorScale,
                         40.0,40.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorNWSEData,
                         GUICursorNWSEDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorPen,
                         CursorScale,
                         0.0,0.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorPenData,
                         GUICursorPenDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorUnavailable,
                         CursorScale,
                         48.0,48.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorUnavailableData,
                         GUICursorUnavailableDataSize);

 CreateMouseCursorSprite(fSpriteMouseCursorUp,
                         CursorScale,
                         21.0,0.0,
                         0.0,
                         0.0,0.0,
                         @GUICursorUpData,
                         GUICursorUpDataSize);

 fMipmappedSpriteAtlas.MipMaps:=true;
 fMipmappedSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                              pvApplication.VulkanGraphicsCommandBuffers[0,0],
                              pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                              pvApplication.VulkanDevice.TransferQueue,
                              pvApplication.VulkanTransferCommandBuffers[0,0],
                              pvApplication.VulkanTransferCommandBufferFences[0,0]);


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

 fSkin:=nil;

 fCursor:=pvgcArrow;

 fPosition:=TpvVector2.Create(0.0,0.0);

 fSize:=TpvVector2.Create(1.0,1.0);

 fFixedSize:=TpvVector2.Create(-1.0,-1.0);

 fPositionProperty:=TpvVector2Property.Create(@fPosition);

 fSizeProperty:=TpvVector2Property.Create(@fSize);

 fFixedSizeProperty:=TpvVector2Property.Create(@fFixedSize);

 fWidgetFlags:=TpvGUIWidget.DefaultFlags;

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

function TpvGUIWidget.GetSkin:TpvGUISkin;
begin
 if assigned(fSkin) then begin
  result:=fSkin;
 end else if assigned(fInstance) then begin
  result:=fInstance.fStandardSkin;
 end else begin
  result:=nil;
 end;
end;

procedure TpvGUIWidget.SetSkin(const aSkin:TpvGUISkin);
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 fSkin:=aSkin;
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   ChildWidget.SetSkin(aSkin);
  end;
 end;
end;

function TpvGUIWidget.GetEnabled:boolean;
begin
 result:=pvgwfEnabled in fWidgetFlags;
end;

procedure TpvGUIWidget.SetEnabled(const aEnabled:boolean);
begin
 if aEnabled then begin
  Include(fWidgetFlags,pvgwfEnabled);
 end else begin
  Exclude(fWidgetFlags,pvgwfEnabled);
 end;
end;

function TpvGUIWidget.GetVisible:boolean;
begin
 result:=pvgwfVisible in fWidgetFlags;
end;

procedure TpvGUIWidget.SetVisible(const aVisible:boolean);
begin
 if aVisible then begin
  Include(fWidgetFlags,pvgwfVisible);
 end else begin
  Exclude(fWidgetFlags,pvgwfVisible);
 end;
end;

function TpvGUIWidget.GetFocused:boolean;
begin
 result:=pvgwfFocused in fWidgetFlags;
end;

procedure TpvGUIWidget.SetFocused(const aFocused:boolean);
begin
 if aFocused then begin
  Include(fWidgetFlags,pvgwfFocused);
 end else begin
  Exclude(fWidgetFlags,pvgwfFocused);
 end;
end;

function TpvGUIWidget.GetPointerFocused:boolean;
begin
 result:=pvgwfPointerFocused in fWidgetFlags;
end;

procedure TpvGUIWidget.SetPointerFocused(const aPointerFocused:boolean);
begin
 if aPointerFocused then begin
  Include(fWidgetFlags,pvgwfPointerFocused);
 end else begin
  Exclude(fWidgetFlags,pvgwfPointerFocused);
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
 if assigned(Skin) and IsZero(fFontSize) then begin
  result:=Skin.fFontSize;
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
 result:=(aPosition.x>=0.0) and
         (aPosition.y>=0.0) and
         (aPosition.x<fSize.x) and
         (aPosition.y<fSize.y);
end;

function TpvGUIWidget.FindWidget(const aPosition:TpvVector2):TpvGUIWidget;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildPosition:TpvVector2;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildPosition:=aPosition-ChildWidget.fPosition;
    if ChildWidget.Contains(ChildPosition) then begin
     result:=ChildWidget.FindWidget(ChildPosition);
     exit;
    end;
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
 Include(fWidgetFlags,pvgwfFocused);
 result:=false;
end;

function TpvGUIWidget.Leave:boolean;
begin
 Exclude(fWidgetFlags,pvgwfFocused);
 result:=false;
end;

function TpvGUIWidget.PointerEnter:boolean;
begin
 Include(fWidgetFlags,pvgwfPointerFocused);
 result:=false;
end;

function TpvGUIWidget.PointerLeave:boolean;
begin
 Exclude(fWidgetFlags,pvgwfPointerFocused);
 result:=false;
end;

function TpvGUIWidget.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
end;

function TpvGUIWidget.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildPointerEvent:TpvApplicationInputPointerEvent;
    PreviousContained,CurrentContained:boolean;
begin
 ChildPointerEvent:=aPointerEvent;
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    case aPointerEvent.PointerEventType of
     POINTEREVENT_MOTION,POINTEREVENT_DRAG:begin
      ChildPointerEvent.Position:=aPointerEvent.Position-ChildWidget.fPosition;
      PreviousContained:=ChildWidget.Contains(ChildPointerEvent.Position-ChildPointerEvent.RelativePosition);
      CurrentContained:=ChildWidget.Contains(ChildPointerEvent.Position);
      if CurrentContained and not PreviousContained then begin
       ChildWidget.PointerEnter;
      end else if PreviousContained and not CurrentContained then begin
       ChildWidget.PointerLeave;
      end;
      if PreviousContained or CurrentContained then begin
       result:=ChildWidget.PointerEvent(ChildPointerEvent);
       if result then begin
        exit;
       end;
      end;
     end;
     else begin
      ChildPointerEvent.Position:=aPointerEvent.Position-ChildWidget.fPosition;
      if ChildWidget.Contains(ChildPointerEvent.Position) then begin
       result:=ChildWidget.PointerEvent(ChildPointerEvent);
       if result then begin
        exit;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 if (aPointerEvent.PointerEventType=POINTEREVENT_DOWN) and
    (aPointerEvent.Button=BUTTON_LEFT) and not
    (pvgwfFocused in fWidgetFlags) then begin
  RequestFocus;
 end;
 result:=false;
end;

function TpvGUIWidget.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildPosition:TpvVector2;
begin
 for ChildIndex:=fChildren.Count-1 downto 0 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildPosition:=aPosition-ChildWidget.fPosition;
    if ChildWidget.Contains(ChildPosition) then begin
     result:=ChildWidget.Scrolled(ChildPosition,aRelativeAmount);
     if result then begin
      exit;
     end;
    end;
   end;
  end;
 end;
 result:=false;
end;

procedure TpvGUIWidget.AfterCreateSwapChain;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   ChildWidget.AfterCreateSwapChain;
  end;
 end;
end;

procedure TpvGUIWidget.BeforeDestroySwapChain;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   ChildWidget.BeforeDestroySwapChain;
  end;
 end;
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
     fCanvas.ClipRect:=BaseClipRect.GetIntersection(TpvRect.CreateRelative(BaseModelMatrix*ChildWidget.fPosition,
                                                                           ChildWidget.fSize));
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

 fStandardSkin:=TpvGUISkin.Create(self);

 fDrawWidgetBounds:=false;

 fBuffers:=nil;

 fCountBuffers:=0;

 fUpdateBufferIndex:=0;

 fDrawBufferIndex:=0;

 fDeltaTime:=0.0;

 fTime:=0.0;

 fLastFocusPath:=TpvGUIObjectList.Create(false);

 fCurrentFocusPath:=TpvGUIObjectList.Create(false);

 fDragWidget:=nil;

 fWindow:=nil;

 fVisibleCursor:=pvgcArrow;

 SetCountBuffers(1);

end;

destructor TpvGUIInstance.Destroy;
begin

 TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);

 FreeAndNil(fLastFocusPath);

 FreeAndNil(fCurrentFocusPath);

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
 fLastFocusPath.Clear;
 fCurrentFocusPath.Clear;
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

 TpvSwap<TpvGUIObjectList>.Swap(fCurrentFocusPath,fLastFocusPath);

 fCurrentFocusPath.Clear;

 TpvReferenceCountedObject.DecRefOrFreeAndNil(fWindow);

 CurrentWidget:=aWidget;
 while assigned(CurrentWidget) do begin
  fCurrentFocusPath.Add(CurrentWidget);
  if CurrentWidget is TpvGUIWindow then begin
   TpvReferenceCountedObject.DecRefOrFreeAndNil(fWindow);
   fWindow:=CurrentWidget as TpvGUIWindow;
   fWindow.IncRef;
   break;
  end;
  if assigned(CurrentWidget.fParent) and (CurrentWidget.fParent is TpvGUIWidget) then begin
   CurrentWidget:=CurrentWidget.fParent as TpvGUIWidget;
  end else begin
   break;
  end;
 end;

 try
  for CurrentIndex:=0 to fLastFocusPath.Count-1 do begin
   Current:=fLastFocusPath.Items[CurrentIndex];
   if Current is TpvGUIWidget then begin
    CurrentWidget:=Current as TpvGUIWidget;
    if CurrentWidget.Focused and not fCurrentFocusPath.Contains(Current) then begin
     CurrentWidget.Leave;
    end;
   end;
  end;
 finally
  fLastFocusPath.Clear;
 end;

 for CurrentIndex:=0 to fCurrentFocusPath.Count-1 do begin
  Current:=fCurrentFocusPath.Items[CurrentIndex];
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
  if assigned(fLastFocusPath) and fLastFocusPath.Contains(aWindow) then begin
   fLastFocusPath.Clear;
  end;
  if assigned(fCurrentFocusPath) and fCurrentFocusPath.Contains(aWindow) then begin
   fCurrentFocusPath.Clear;
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

function TpvGUIInstance.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var Index:TpvInt32;
    Current:TpvGUIObject;
    CurrentWidget:TpvGUIWidget;
begin
 result:=false;
 for Index:=0 to fCurrentFocusPath.Count-1 do begin
  Current:=fCurrentFocusPath.Items[Index];
  if (Current<>self) and (Current is TpvGUIWidget) then begin
   CurrentWidget:=Current as TpvGUIWidget;
   if CurrentWidget.Focused then begin
    result:=CurrentWidget.KeyEvent(aKeyEvent);
    if result then begin
     exit;
    end;
   end;
  end;
 end;
end;

function TpvGUIInstance.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Index:TpvInt32;
    Current:TpvGUIObject;
    CurrentWindow:TpvGUIWindow;
    CurrentWidget:TpvGUIWidget;
    LocalPointerEvent:TpvApplicationInputPointerEvent;
    DoUpdateCursor:boolean;
begin
 result:=false;
 DoUpdateCursor:=false;
 fMousePosition:=aPointerEvent.Position;
 case aPointerEvent.PointerEventType of
  POINTEREVENT_DOWN,POINTEREVENT_UP:begin
   for Index:=0 to fCurrentFocusPath.Count-1 do begin
    Current:=fCurrentFocusPath.Items[Index];
    if (Current<>self) and (Current is TpvGUIWindow) then begin
     CurrentWindow:=Current as TpvGUIWindow;
     if (pvgwfModal in CurrentWindow.fWindowFlags) and not CurrentWindow.Contains(aPointerEvent.Position-CurrentWindow.AbsolutePosition) then begin
      exit;
     end;
    end;
   end;
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     case aPointerEvent.Button of
      BUTTON_LEFT,BUTTON_RIGHT:begin
       TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
       CurrentWidget:=FindWidget(aPointerEvent.Position);
       if assigned(CurrentWidget) and (CurrentWidget<>self) then begin
        fDragWidget:=CurrentWidget;
        fDragWidget.IncRef;
       end else begin
        TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
        UpdateFocus(nil);
       end;
      end;
      else begin
       TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
      end;
     end;
    end;
    POINTEREVENT_UP:begin
     CurrentWidget:=FindWidget(aPointerEvent.Position);
     if assigned(fDragWidget) and (fDragWidget<>CurrentWidget) then begin
      LocalPointerEvent.PointerEventType:=POINTEREVENT_UP;
      LocalPointerEvent.Button:=BUTTON_LEFT;
      fDragWidget.PointerEvent(LocalPointerEvent);
     end;
     TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
    end;
   end;
   result:=inherited PointerEvent(aPointerEvent);
   DoUpdateCursor:=true;
  end;
  POINTEREVENT_MOTION:begin
   if assigned(fDragWidget) then begin
    LocalPointerEvent:=aPointerEvent;
    LocalPointerEvent.PointerEventType:=POINTEREVENT_DRAG;
    result:=fDragWidget.PointerEvent(LocalPointerEvent);
   end else begin
    result:=inherited PointerEvent(aPointerEvent);
   end;
   DoUpdateCursor:=true;
  end;
  POINTEREVENT_DRAG:begin
   result:=inherited PointerEvent(aPointerEvent);
  end;
 end;
 if DoUpdateCursor then begin
  if assigned(fDragWidget) then begin
   fVisibleCursor:=fDragWidget.fCursor;
  end else begin
   CurrentWidget:=FindWidget(aPointerEvent.Position);
   if assigned(CurrentWidget) then begin
    fVisibleCursor:=CurrentWidget.fCursor;
   end else begin
    fVisibleCursor:=fCursor;
   end;
  end;
 end;
end;

function TpvGUIInstance.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=inherited Scrolled(aPosition,aRelativeAmount);
end;

procedure TpvGUIInstance.Update;
begin
 ClearReferenceCountedObjectList;
 inherited Update;
 fCanvas.BlendingMode:=pvcbmAlphaBlending;
 case fVisibleCursor of
  pvgcArrow:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorArrow,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorArrow,fMousePosition);
  end;
  pvgcBeam:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorBeam,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorBeam,fMousePosition);
  end;
  pvgcBusy:begin
   fCanvas.Push;
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.ModelMatrix:=((TpvMatrix4x4.CreateTranslation(-(fMousePosition+TpvVector2.Create(2.0,2.0)))*
                          TpvMatrix4x4.CreateRotateZ(frac(fTime)*TwoPI))*
                         TpvMatrix4x4.CreateTranslation(fMousePosition+TpvVector2.Create(2.0,2.0)))*
                         fCanvas.ModelMatrix;
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorBusy,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Pop;
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.Push;
   fCanvas.ModelMatrix:=((TpvMatrix4x4.CreateTranslation(-fMousePosition)*
                          TpvMatrix4x4.CreateRotateZ(frac(fTime)*TwoPI))*
                         TpvMatrix4x4.CreateTranslation(fMousePosition))*
                         fCanvas.ModelMatrix;
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorBusy,fMousePosition);
   fCanvas.Pop;
  end;
  pvgcCross:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorCross,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorCross,fMousePosition);
  end;
  pvgcEW:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorEW,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorEW,fMousePosition);
  end;
  pvgcHelp:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorHelp,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorHelp,fMousePosition);
  end;
  pvgcLink:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorLink,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorLink,fMousePosition);
  end;
  pvgcMove:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorMove,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorMove,fMousePosition);
  end;
  pvgcNESW:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorNESW,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorNESW,fMousePosition);
  end;
  pvgcNS:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorNS,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorNS,fMousePosition);
  end;
  pvgcNWSE:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorNWSE,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorNWSE,fMousePosition);
  end;
  pvgcPen:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorPen,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorPen,fMousePosition);
  end;
  pvgcUnavailable:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorUnavailable,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorUnavailable,fMousePosition);
  end;
  pvgcUp:begin
   fCanvas.Color:=TpvVector4.Create(0.0,0.0,0.0,0.25);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorUp,fMousePosition+TpvVector2.Create(2.0,2.0));
   fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
   fCanvas.DrawSprite(Skin.fSpriteMouseCursorUp,fMousePosition);
  end;
 end;
 fTime:=fTime+fDeltaTime;
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
 fWindowFlags:=TpvGUIWindow.DefaultFlags;
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

function TpvGUIWindow.GetModal:boolean;
begin
 result:=pvgwfModal in fWindowFlags;
end;

procedure TpvGUIWindow.SetModal(const aModal:boolean);
begin
 if aModal then begin
  Include(fWindowFlags,pvgwfModal);
 end else begin
  Exclude(fWindowFlags,pvgwfModal);
 end;
end;

function TpvGUIWindow.GetButtonPanel:TpvGUIWidget;
begin
 if not assigned(fButtonPanel) then begin
  fButtonPanel:=TpvGUIWidget.Create(self);
  fButtonPanel.fLayout:=TpvGUIBoxLayout.Create(fButtonPanel,pvglaMiddle,pvgloHorizontal,0.0,4.0);
 end;
 result:=fButtonPanel;
end;

function TpvGUIWindow.GetPreferredSize:TpvVector2;
begin
 if assigned(fButtonPanel) then begin
  fButtonPanel.Visible:=false;
 end;
 result:=Maximum(inherited GetPreferredSize,
                 Skin.fSansFont.TextSize(fTitle,
                                          Max(Skin.fUnfocusedWindowHeaderFontSize,
                                              Skin.fFocusedWindowHeaderFontSize))+
                 TpvVector2.Create(Skin.fSansFont.TextWidth('====',
                                                             Max(Skin.fUnfocusedWindowHeaderFontSize,
                                                             Skin.fFocusedWindowHeaderFontSize)),
                                   0.0));
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

function TpvGUIWindow.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
end;

function TpvGUIWindow.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var ClampedRelativePosition,MinimumSize,NewSize,NewPosition:TpvVector2;
begin
 result:=inherited PointerEvent(aPointerEvent);
 if not result then begin
  case aPointerEvent.PointerEventType of
   POINTEREVENT_DOWN:begin
    fMouseAction:=pvgwmaNone;
    fCursor:=pvgcArrow;
    if (pvgwfResizableNW in fWindowFlags) and
       (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
       (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
     fMouseAction:=pvgwmaSizeNW;
     fCursor:=pvgcNWSE;
    end else if (pvgwfResizableNE in fWindowFlags) and
                (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
     fMouseAction:=pvgwmaSizeNE;
     fCursor:=pvgcNESW;
    end else if (pvgwfResizableSW in fWindowFlags) and
                (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
                (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
     fMouseAction:=pvgwmaSizeSW;
     fCursor:=pvgcNESW;
    end else if (pvgwfResizableSE in fWindowFlags) and
                (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
     fMouseAction:=pvgwmaSizeSE;
     fCursor:=pvgcNWSE;
    end else if (pvgwfResizableN in fWindowFlags) and
                (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
     fMouseAction:=pvgwmaSizeN;
     fCursor:=pvgcNS;
    end else if (pvgwfResizableS in fWindowFlags) and
                (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
     fMouseAction:=pvgwmaSizeS;
     fCursor:=pvgcNS;
    end else if (pvgwfResizableW in fWindowFlags) and
                (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) then begin
     fMouseAction:=pvgwmaSizeW;
     fCursor:=pvgcEW;
    end else if (pvgwfResizableE in fWindowFlags) and
                (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) then begin
     fMouseAction:=pvgwmaSizeE;
     fCursor:=pvgcEW;
    end else if (pvgwfMovable in fWindowFlags) and
                (aPointerEvent.Position.y<Skin.fWindowHeaderHeight) then begin
     fMouseAction:=pvgwmaMove;
     fCursor:=pvgcMove;
    end;
    if not (pvgwfFocused in fWidgetFlags) then begin
     RequestFocus;
    end;
   end;
   POINTEREVENT_UP:begin
    fMouseAction:=pvgwmaNone;
    fCursor:=pvgcArrow;
   end;
   POINTEREVENT_MOTION:begin
    if fMouseAction=pvgwmaNone then begin
     fCursor:=pvgcArrow;
     if (pvgwfResizableNW in fWindowFlags) and
        (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
        (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
      fCursor:=pvgcNWSE;
     end else if (pvgwfResizableNE in fWindowFlags) and
                 (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                 (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
      fCursor:=pvgcNESW;
     end else if (pvgwfResizableSW in fWindowFlags) and
                 (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
                 (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
      fCursor:=pvgcNESW;
     end else if (pvgwfResizableSE in fWindowFlags) and
                 (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                 (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
      fCursor:=pvgcNWSE;
     end else if (pvgwfResizableN in fWindowFlags) and
                 (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
      fCursor:=pvgcNS;
     end else if (pvgwfResizableS in fWindowFlags) and
                 (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
      fCursor:=pvgcNS;
     end else if (pvgwfResizableW in fWindowFlags) and
                 (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) then begin
      fCursor:=pvgcEW;
     end else if (pvgwfResizableE in fWindowFlags) and
                 (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) then begin
      fCursor:=pvgcEW;
     end;
    end;
   end;
   POINTEREVENT_DRAG:begin
    MinimumSize:=TpvVector2.Create(Skin.fWindowMinimumWidth,Skin.fWindowMinimumHeight);
    case fMouseAction of
     pvgwmaMove:begin
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       ClampedRelativePosition:=Clamp(aPointerEvent.RelativePosition,-fPosition,(fParent as TpvGUIWidget).fSize-(fPosition+fSize));
      end else begin
       ClampedRelativePosition:=Maximum(aPointerEvent.RelativePosition,-fPosition);
      end;
      fPosition:=fPosition+ClampedRelativePosition;
      fCursor:=pvgcMove;
     end;
     pvgwmaSizeNW:begin
      NewSize:=Maximum(fSize-aPointerEvent.RelativePosition,MinimumSize);
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       ClampedRelativePosition:=Clamp(fPosition+(fSize-NewSize),TpvVector2.Null,(fParent as TpvGUIWidget).fSize-NewSize)-fPosition;
      end else begin
       ClampedRelativePosition:=Maximum(fPosition+(fSize-NewSize),TpvVector2.Null)-fPosition;
      end;
      fPosition:=fPosition+ClampedRelativePosition;
      fSize:=fSize-ClampedRelativePosition;
      fCursor:=pvgcNWSE;
     end;
     pvgwmaSizeNE:begin
      NewSize:=Maximum(fSize+TpvVector2.Create(aPointerEvent.RelativePosition.x,
                                               -aPointerEvent.RelativePosition.y),
                       MinimumSize);
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       ClampedRelativePosition.x:=Minimum(NewSize.x,(fParent as TpvGUIWidget).fSize.x-fPosition.x)-fSize.x;
       ClampedRelativePosition.y:=Clamp(fPosition.y+(fSize.y-NewSize.y),0.0,(fParent as TpvGUIWidget).fSize.y-NewSize.y)-fPosition.y;
      end else begin
       ClampedRelativePosition.x:=NewSize.x-fSize.x;
       ClampedRelativePosition.y:=Maximum(fPosition.y+(fSize.y-NewSize.y),0.0)-fPosition.y;
      end;
      fPosition.y:=fPosition.y+ClampedRelativePosition.y;
      fSize.x:=fSize.x+ClampedRelativePosition.x;
      fSize.y:=fSize.y-ClampedRelativePosition.y;
      fCursor:=pvgcNESW;
     end;
     pvgwmaSizeSW:begin
      NewSize:=Maximum(fSize+TpvVector2.Create(-aPointerEvent.RelativePosition.x,
                                               aPointerEvent.RelativePosition.y),
                       MinimumSize);
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       ClampedRelativePosition.x:=Clamp(fPosition.x+(fSize.x-NewSize.x),0.0,(fParent as TpvGUIWidget).fSize.x-NewSize.x)-fPosition.x;
       ClampedRelativePosition.y:=Minimum(NewSize.y,(fParent as TpvGUIWidget).fSize.y-fPosition.y)-fSize.y;
      end else begin
       ClampedRelativePosition.x:=Maximum(fPosition.x+(fSize.x-NewSize.x),0.0)-fPosition.x;
       ClampedRelativePosition.y:=NewSize.y-fSize.y;
      end;
      fPosition.x:=fPosition.x+ClampedRelativePosition.x;
      fSize.x:=fSize.x-ClampedRelativePosition.x;
      fSize.y:=fSize.y+ClampedRelativePosition.y;
      fCursor:=pvgcNESW;
     end;
     pvgwmaSizeSE:begin
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       fSize:=Clamp(fSize+aPointerEvent.RelativePosition,MinimumSize,(fParent as TpvGUIWidget).fSize-fPosition);
      end else begin
       fSize:=Maximum(fSize+aPointerEvent.RelativePosition,MinimumSize);
      end;
      fCursor:=pvgcNWSE;
     end;
     pvgwmaSizeN:begin
      NewSize.y:=Maximum(fSize.y-aPointerEvent.RelativePosition.y,MinimumSize.y);
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       ClampedRelativePosition.y:=Clamp(fPosition.y+(fSize.y-NewSize.y),0.0,(fParent as TpvGUIWidget).fSize.y-NewSize.y)-fPosition.y;
      end else begin
       ClampedRelativePosition.y:=Maximum(fPosition.y+(fSize.y-NewSize.y),0.0)-fPosition.y;
      end;
      fPosition.y:=fPosition.y+ClampedRelativePosition.y;
      fSize.y:=fSize.y-ClampedRelativePosition.y;
      fCursor:=pvgcNS;
     end;
     pvgwmaSizeS:begin
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       fSize.y:=Clamp(fSize.y+aPointerEvent.RelativePosition.y,MinimumSize.y,(fParent as TpvGUIWidget).fSize.y-fPosition.y);
      end else begin
       fSize.y:=Maximum(fSize.y+aPointerEvent.RelativePosition.y,MinimumSize.y);
      end;
      fCursor:=pvgcNS;
     end;
     pvgwmaSizeW:begin
      NewSize.x:=Maximum(fSize.x-aPointerEvent.RelativePosition.x,MinimumSize.x);
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       ClampedRelativePosition.x:=Clamp(fPosition.x+(fSize.x-NewSize.x),0.0,(fParent as TpvGUIWidget).fSize.x-NewSize.x)-fPosition.x;
      end else begin
       ClampedRelativePosition.x:=Maximum(fPosition.x+(fSize.x-NewSize.x),0.0)-fPosition.x;
      end;
      fPosition.x:=fPosition.x+ClampedRelativePosition.x;
      fSize.x:=fSize.x-ClampedRelativePosition.x;
      fCursor:=pvgcEW;
     end;
     pvgwmaSizeE:begin
      if assigned(fParent) and (fParent is TpvGUIWidget) then begin
       fSize.x:=Clamp(fSize.x+aPointerEvent.RelativePosition.x,MinimumSize.x,(fParent as TpvGUIWidget).fSize.x-fPosition.x);
      end else begin
       fSize.x:=Maximum(fSize.x+aPointerEvent.RelativePosition.x,MinimumSize.x);
      end;
      fCursor:=pvgcEW;
     end;
     else begin
      fCursor:=pvgcArrow;
     end;
    end;
    if assigned(fParent) and (fParent is TpvGUIWidget) then begin
     fSize:=Clamp(fSize,MinimumSize,(fParent as TpvGUIWidget).fSize-fPosition);
     fPosition:=Clamp(fPosition,TpvVector2.Null,(fParent as TpvGUIWidget).fSize-fSize);
    end else begin
     fSize:=Maximum(fSize,MinimumSize);
     fPosition:=Maximum(fPosition,TpvVector2.Null);
    end;
   end;
  end;
 end;
 result:=true;
end;

function TpvGUIWindow.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 inherited Scrolled(aPosition,aRelativeAmount);
 result:=true;
end;

procedure TpvGUIWindow.Update;
var LastClipRect,NewClipRect:TpvRect;
    LastModelMatrix,NewModelMatrix:TpvMatrix4x4;
begin
 fCanvas.Push;
 try
  fCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
  begin
   LastClipRect:=fCanvas.ClipRect;
   NewClipRect:=TpvRect.CreateAbsolute(LastClipRect.Left-Skin.fWindowShadowWidth,
                                       LastClipRect.Top-Skin.fWindowShadowHeight,
                                       LastClipRect.Right+Skin.fWindowShadowWidth,
                                       LastClipRect.Bottom+Skin.fWindowShadowHeight);
   if assigned(fParent) and
      (fParent is TpvGUIWidget) then begin
    NewClipRect:=TpvRect.CreateRelative((fParent as TpvGUIWidget).fPosition,
                                        (fParent as TpvGUIWidget).fSize).GetIntersection(NewClipRect);
   end;
   fCanvas.ClipRect:=NewClipRect;
   fCanvas.DrawNinePatchSprite(Skin.fSpriteFocusedWindowShadow,
                               Skin.fSpriteFocusedWindowShadowNinePatch,
                               TpvVector2.Create(-Skin.fWindowShadowWidth,-Skin.fWindowShadowHeight),
                               fSize+TpvVector2.Create(Skin.fWindowShadowWidth*2,Skin.fWindowShadowHeight*2));
   fCanvas.ClipRect:=LastClipRect;
  end;

  if pvgwfFocused in fWidgetFlags then begin
   fCanvas.DrawNinePatchSprite(Skin.fSpriteFocusedWindowFill,
                               Skin.fSpriteFocusedWindowFillNinePatch,
                               TpvVector2.Create(0.0,Skin.fSpriteFocusedWindowHeader.Height),
                               TpvVector2.Create(fSize.x,fSize.y-Skin.fSpriteFocusedWindowHeader.Height));
   fCanvas.DrawNinePatchSprite(Skin.fSpriteFocusedWindowHeader,
                               Skin.fSpriteFocusedWindowHeaderNinePatch,
                               TpvVector2.Null,
                               TpvVector2.Create(fSize.x,Skin.fSpriteFocusedWindowHeader.Height));
{  if Resizable then begin
    fCanvas.DrawNinePatchSprite(Skin.fSpriteFocusedWindowGrip,
                                Skin.fSpriteFocusedWindowGripNinePatch,
                                TpvVector2.Create(fSize.x-(Skin.fSpriteFocusedWindowGrip.Width+Skin.fWindowResizeGripSize),fSize.y-(Skin.fSpriteFocusedWindowGrip.Height+Skin.fWindowGripPaddingBottom)),
                                TpvVector2.Create(Skin.fSpriteFocusedWindowGrip.Width,Skin.fSpriteFocusedWindowGrip.Height));
   end;}
  end else begin
   fCanvas.DrawNinePatchSprite(Skin.fSpriteUnfocusedWindowFill,
                               Skin.fSpriteUnfocusedWindowFillNinePatch,
                               TpvVector2.Create(0.0,Skin.fSpriteUnfocusedWindowHeader.Height),
                               TpvVector2.Create(fSize.x,fSize.y-Skin.fSpriteUnfocusedWindowHeader.Height));
   fCanvas.DrawNinePatchSprite(Skin.fSpriteUnfocusedWindowHeader,
                               Skin.fSpriteUnfocusedWindowHeaderNinePatch,
                               TpvVector2.Null,
                               TpvVector2.Create(fSize.x,Skin.fSpriteUnfocusedWindowHeader.Height));
{  if Resizable then begin
    fCanvas.DrawNinePatchSprite(Skin.fSpriteUnfocusedWindowGrip,
                                Skin.fSpriteUnfocusedWindowGripNinePatch,
                                TpvVector2.Create(fSize.x-(Skin.fSpriteUnfocusedWindowGrip.Width+Skin.fWindowResizeGripSize),fSize.y-(Skin.fSpriteUnfocusedWindowGrip.Height+Skin.fWindowGripPaddingBottom)),
                                TpvVector2.Create(Skin.fSpriteUnfocusedWindowGrip.Width,Skin.fSpriteUnfocusedWindowGrip.Height));
   end;}
  end;

  if pvgwfHeader in fWindowFlags then begin
   LastModelMatrix:=fCanvas.ModelMatrix;
   try
    fCanvas.Font:=Skin.fSansFont;
    fCanvas.FontSize:=IfThen(pvgwfFocused in fWidgetFlags,Skin.fFocusedWindowHeaderFontSize,Skin.fUnfocusedWindowHeaderFontSize);
    fCanvas.TextHorizontalAlignment:=pvcthaCenter;
    fCanvas.TextVerticalAlignment:=pvctvaMiddle;
    NewModelMatrix:=TpvMatrix4x4.CreateTranslation(fSize.x*0.5,
                                                   Skin.fSpriteUnfocusedWindowHeader.Height*0.5)*
                    LastModelMatrix;
    if ((pvgwfFocused in fWidgetFlags) and Skin.fFocusedWindowHeaderFontShadow) or
       ((not (pvgwfFocused in fWidgetFlags)) and Skin.fUnfocusedWindowHeaderFontShadow) then begin
     if pvgwfFocused in fWidgetFlags then begin
      fCanvas.ModelMatrix:=TpvMatrix4x4.CreateTranslation(Skin.fFocusedWindowHeaderFontShadowOffset)*NewModelMatrix;
      fCanvas.Color:=Skin.fFocusedWindowHeaderFontShadowColor;
     end else begin
      fCanvas.ModelMatrix:=TpvMatrix4x4.CreateTranslation(Skin.fUnfocusedWindowHeaderFontShadowOffset)*NewModelMatrix;
      fCanvas.Color:=Skin.fUnfocusedWindowHeaderFontShadowColor;
     end;
     fCanvas.DrawText(fTitle);
    end;
    fCanvas.ModelMatrix:=NewModelMatrix;
    if pvgwfFocused in fWidgetFlags then begin
     fCanvas.Color:=Skin.fFocusedWindowHeaderFontColor;
    end else begin
     fCanvas.Color:=Skin.fUnfocusedWindowHeaderFontColor;
    end;
    fCanvas.DrawText(fTitle);
   finally
    fCanvas.ModelMatrix:=LastModelMatrix;
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

constructor TpvGUILabel.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fCaption:='Label';
end;

destructor TpvGUILabel.Destroy;
begin
 inherited Destroy;
end;

function TpvGUILabel.GetPreferredSize:TpvVector2;
begin
 result:=Maximum(inherited GetPreferredSize,
                 Skin.fSansFont.TextSize(fCaption,FontSize)+TpvVector2.Create(0.0,0.0));
end;

function TpvGUILabel.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
end;

function TpvGUILabel.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=inherited PointerEvent(aPointerEvent);
end;

function TpvGUILabel.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=inherited Scrolled(aPosition,aRelativeAmount);
end;

procedure TpvGUILabel.Update;
begin
 fCanvas.Push;
 try
  fCanvas.Font:=Skin.fSansFont;
  fCanvas.FontSize:=FontSize;
  fCanvas.TextHorizontalAlignment:=pvcthaLeft;
  fCanvas.TextVerticalAlignment:=pvctvaTop;
  fCanvas.Color:=Skin.fFocusedWindowHeaderFontColor;
  fCanvas.DrawText(fCaption);
 finally
  fCanvas.Pop;
 end;
 inherited Update;
end;

procedure TpvGUILabel.Draw;
begin
 inherited Draw;
end;


end.
