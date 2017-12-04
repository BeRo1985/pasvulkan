{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR ON}
{$WARN CVT_WIDENING_STRING_LOST ON}
{$WARN NON_PORTABLE_TYPECAST ON}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}
{$WARN IMMUTABLE_STRINGS OFF}
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
     PUCU,
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

     TpvGUIMessageDialog=class;

     TpvGUIPopup=class;

     TpvGUIImage=class;

     TpvGUILabel=class;

     TpvGUIButton=class;

     TpvGUICheckBox=class;

     TpvGUITextEdit=class;

     TpvGUIMenuItem=class;

     TpvGUIPopupMenu=class;

     TpvGUIWindowMenu=class;

     TpvGUIScrollBar=class;

     TpvGUISlider=class;

     TpvGUIProgressBar=class;

     TpvGUIPanel=class;

     EpvGUIWidget=class(Exception);

     TpvGUIOnEvent=procedure(const aSender:TpvGUIObject) of object;

     TpvGUIOnChange=procedure(const aSender:TpvGUIObject;const aChanged:boolean) of object;

     TpvGUIOnKeyEvent=function(const aSender:TpvGUIObject;const aKeyEvent:TpvApplicationInputKeyEvent):boolean of object;

     TpvGUIOnPointerEvent=function(const aSender:TpvGUIObject;const aPointerEvent:TpvApplicationInputPointerEvent):boolean of object;

     TpvGUIOnScrolled=function(const aSender:TpvGUIObject;const aPosition,aRelativeAmount:TpvVector2):boolean of object;

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
       function HasParent(const aParent:TpvGUIObject):boolean; virtual;
      published
       property Instance:TpvGUIInstance read fInstance;
       property Parent:TpvGUIObject read fParent write fParent;
       property Children:TpvGUIObjectList read fChildren;
       property ID:TpvUTF8String read fID write fID;
       property Tag:TpvPtrInt read fTag write fTag;
       property ReferenceCounter:TpvInt32 read fReferenceCounter write fReferenceCounter;
     end;

     TpvGUIObjectHolder=class(TpvGUIObject)
      private
       fHoldedObject:TObject;
      public
       constructor Create(const aParent:TpvGUIObject;const aHoldedObject:TObject=nil); reintroduce; virtual;
       destructor Destroy; override;
      published
       property HoldedObject:TObject read fHoldedObject write fHoldedObject;
     end;

     PpvGUITextAlignment=^TpvGUITextAlignment;
     TpvGUITextAlignment=
      (
       pvgtaLeading,
       pvgtaMiddle,
       pvgtaCenter=pvgtaMiddle,
       pvgtaTailing
      );

     PpvGUITextTruncation=^TpvGUITextTruncation;
     TpvGUITextTruncation=
      (
       pvgttNone,
       pvgttHead,
       pvgttMiddle,
       pvgttTail
      );

     TpvGUITextUtils=class
      public
       class function TextTruncation(const aText:TpvUTF8String;
                                     const aTextTruncation:TpvGUITextTruncation;
                                     const aFont:TpvFont;
                                     const aFontSize:TVkFloat;
                                     const aAvailableWidth:TVkFloat):TpvUTF8String; static;
     end;

     PpvGUILayoutAlignment=^TpvGUILayoutAlignment;
     TpvGUILayoutAlignment=
      (
       pvglaLeading,
       pvglaMiddle,
       pvglaTailing,
       pvglaFill
      );

     TpvGUILayoutAlignments=array of TpvGUILayoutAlignment;

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

     TpvGUIRootLayout=class(TpvGUILayout)
      private
       fMargin:TpvFloat;
       fSpacing:TpvFloat;
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       procedure PerformLayout(const aWidget:TpvGUIWidget); override;
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aMargin:TpvFloat=0.0;
                          const aSpacing:TpvFloat=0.0); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Margin:TpvFloat read fMargin write fMargin;
       property Spacing:TpvFloat read fSpacing write fSpacing;
     end;

     TpvGUIFillLayout=class(TpvGUILayout)
      private
       fMargin:TpvFloat;
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       procedure PerformLayout(const aWidget:TpvGUIWidget); override;
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aMargin:TpvFloat=0.0); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Margin:TpvFloat read fMargin write fMargin;
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

     TpvGUIGroupLayout=class(TpvGUILayout)
      private
       fMargin:TpvFloat;
       fSpacing:TpvFloat;
       fGroupSpacing:TpvFloat;
       fGroupIdent:TpvFloat;
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       procedure PerformLayout(const aWidget:TpvGUIWidget); override;
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aMargin:TpvFloat=15.0;
                          const aSpacing:TpvFloat=6.0;
                          const aGroupSpacing:TpvFloat=14.0;
                          const aGroupIndent:TpvFloat=20.0); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Margin:TpvFloat read fMargin write fMargin;
       property Spacing:TpvFloat read fSpacing write fSpacing;
       property GroupSpacing:TpvFloat read fGroupSpacing write fGroupSpacing;
       property GroupIdent:TpvFloat read fGroupIdent write fGroupIdent;
     end;

     TpvGUIGridLayoutAlignments=class(TpvGenericList<TpvGUILayoutAlignment>)
      public
       procedure SetAlignments(const aAlignments:array of TpvGUILayoutAlignment);
     end;

     PpvGUIGridLayoutFlag=^TpvGUIGridLayoutFlag;
     TpvGUIGridLayoutFlag=
      (
       pvgglfStretchHorizontal,
       pvgglfStretchVertical
      );

     PpvGUIGridLayoutFlags=^TpvGUIGridLayoutFlags;
     TpvGUIGridLayoutFlags=set of TpvGUIGridLayoutFlag;

     TpvGUIGridLayout=class(TpvGUILayout)
      private
       const AxisStretchFlags:array[0..1] of TpvGUIGridLayoutFlag=
              (
               pvgglfStretchHorizontal,
               pvgglfStretchVertical
              );
             AxisOrientationAxes:array[TpvGUILayoutOrientation,0..1] of TpvInt32=
              (
               (0,1), // pvgloHorizontal
               (1,0)  // pvgloVertical
              );
      private
       fFlags:TpvGUIGridLayoutFlags;
       fResolution:TpvInt32;
       fDefaultAlignments:array[0..1] of TpvGUILayoutAlignment;
       fAlignments:array[0..1] of TpvGUIGridLayoutAlignments;
       fOrientation:TpvGUILayoutOrientation;
       fMargin:TpvFloat;
       fSpacing:TpvVector2;
       fSpacingProperty:TpvVector2Property;
       fGrid:array[0..1] of TpvFloats;
       fGridDimensions:array[0..1] of TpvInt32;
       function GetColumnAlignments:TpvGUIGridLayoutAlignments; inline;
       function GetRowAlignments:TpvGUIGridLayoutAlignments; inline;
       function GetColumnAlignment:TpvGUILayoutAlignment; inline;
       procedure SetColumnAlignment(const aAlignment:TpvGUILayoutAlignment); inline;
       function GetRowAlignment:TpvGUILayoutAlignment; inline;
       procedure SetRowAlignment(const aAlignment:TpvGUILayoutAlignment); inline;
       procedure SetResolution(const aResolution:TpvInt32);
       function GetAlignment(const aAxisIndex,aItemIndex:TpvInt32):TpvGUILayoutAlignment;
       procedure ComputeLayout(const aWidget:TpvGUIWidget);
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       procedure PerformLayout(const aWidget:TpvGUIWidget); override;
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aResolution:TpvInt32=2;
                          const aColumnAlignment:TpvGUILayoutAlignment=pvglaMiddle;
                          const aRowAlignment:TpvGUILayoutAlignment=pvglaMiddle;
                          const aOrientation:TpvGUILayoutOrientation=pvgloHorizontal;
                          const aMargin:TpvFloat=0.0;
                          const aHorizontalSpacing:TpvFloat=0.0;
                          const aVerticalSpacing:TpvFloat=0.0); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Flags:TpvGUIGridLayoutFlags read fFlags write fFlags;
       property Resolution:TpvInt32 read fResolution write SetResolution;
       property ColumnAlignments:TpvGUIGridLayoutAlignments read GetColumnAlignments;
       property RowAlignments:TpvGUIGridLayoutAlignments read GetRowAlignments;
       property ColumnAlignment:TpvGUILayoutAlignment read GetColumnAlignment write SetColumnAlignment;
       property RowAlignment:TpvGUILayoutAlignment read GetRowAlignment write SetRowAlignment;
       property Orientation:TpvGUILayoutOrientation read fOrientation write fOrientation;
       property Margin:TpvFloat read fMargin write fMargin;
       property Spacing:TpvVector2Property read fSpacingProperty;
     end;

     PpvGUIAdvancedGridLayoutAnchorVector=^TpvGUIAdvancedGridLayoutAnchorVector;
     TpvGUIAdvancedGridLayoutAnchorVector=record
      case boolean of
       false:(
        x,y:TpvUInt8;
       );
       true:(
        Axis:array[0..1] of TpvUInt8;
       );
     end;

     PpvGUIAdvancedGridLayoutAnchorAlignmentVector=^TpvGUIAdvancedGridLayoutAnchorAlignmentVector;
     TpvGUIAdvancedGridLayoutAnchorAlignmentVector=record
      case boolean of
       false:(
        x,y:TpvGUILayoutAlignment;
       );
       true:(
        Axis:array[0..1] of TpvGUILayoutAlignment;
       );
     end;

     PpvGUIAdvancedGridLayoutAnchor=^TpvGUIAdvancedGridLayoutAnchor;
     TpvGUIAdvancedGridLayoutAnchor=record
      public
       class function CreateNull:TpvGUIAdvancedGridLayoutAnchor; static; inline;
       constructor Create(const aX,aY:TpvUInt8;const aWidth,aHeight:TpvUInt8;const aPaddingLeft:TpvFloat=0.0;const aPaddingTop:TpvFloat=0.0;const aPaddingRight:TpvFloat=0.0;const aPaddingBottom:TpvFloat=0.0;const aHorizontalAlignment:TpvGUILayoutAlignment=pvglaFill;const aVerticalAlignment:TpvGUILayoutAlignment=pvglaFill); overload;
       constructor Create(const aX,aY:TpvUInt8;const aHorizontalAlignment,aVerticalAlignment:TpvGUILayoutAlignment); overload;
       constructor Create(const aX,aY:TpvUInt8); overload;
       case boolean of
        false:(
         Position:TpvGUIAdvancedGridLayoutAnchorVector;
         Size:TpvGUIAdvancedGridLayoutAnchorVector;
         Alignment:TpvGUIAdvancedGridLayoutAnchorAlignmentVector;
         Padding:array[0..1,0..1] of TVkFloat;
        );
     end;

     TpvGUIAdvancedGridLayoutAnchors=class(TpvHashMap<TpvGUIWidget,TpvGUIAdvancedGridLayoutAnchor>);

     TpvGUIAdvancedGridLayoutColumnRow=class
      private
       fSize:TpvFloat;
       fStretch:TpvFloat;
      public
       constructor Create(const aSize:TpvFloat;const aStretch:TpvFloat=0.0);
      published
       property Size:TpvFloat read fSize write fSize;
       property Stretch:TpvFloat read fStretch write fStretch;
     end;

     TpvGUIAdvancedGridLayoutColumnRows=class(TpvObjectGenericList<TpvGUIAdvancedGridLayoutColumnRow>)
      public
       function Add(const aSize:TpvFloat;const aStretch:TpvFloat=0.0):TpvSizeInt; reintroduce;
     end;

     EpvGUIAdvancedGridLayout=class(Exception);

     TpvGUIAdvancedGridLayout=class(TpvGUILayout)
      private
       fMargin:TpvFloat;
       fAnchors:TpvGUIAdvancedGridLayoutAnchors;
       fRows:TpvGUIAdvancedGridLayoutColumnRows;
       fColumns:TpvGUIAdvancedGridLayoutColumnRows;
       fGrid:array[0..1] of TpvFloats;
       fGridDimensions:array[0..1] of TpvInt32;
       fPositions:TpvVector2Array;
       fSizes:TpvVector2Array;
       fFixedSizes:TpvVector2Array;
       fTargetSizes:TpvVector2Array;
       procedure ComputeLayout(const aWidget:TpvGUIWidget);
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       procedure PerformLayout(const aWidget:TpvGUIWidget); override;
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aMargin:TpvFloat=0.0); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Margin:TpvFloat read fMargin write fMargin;
       property Anchors:TpvGUIAdvancedGridLayoutAnchors read fAnchors;
       property Rows:TpvGUIAdvancedGridLayoutColumnRows read fRows;
       property Columns:TpvGUIAdvancedGridLayoutColumnRows read fColumns;
     end;

     PpvGUIFlowLayoutDirection=^TpvGUIFlowLayoutDirection;
     TpvGUIFlowLayoutDirection=
      (
       pgfldLeftToRight,
       pgfldRightToLeft
      );

     PpvGUIFlowLayoutAlignment=^TpvGUIFlowLayoutAlignment;
     TpvGUIFlowLayoutAlignment=
      (
       pgflaLeading,
       pgflaMiddle,
       pgflaTailing
      );

     PpvGUIFlowLayoutAlignments=^TpvGUIFlowLayoutAlignments;
     TpvGUIFlowLayoutAlignments=array[0..1] of TpvGUIFlowLayoutAlignment;

     TpvGUIFlowLayout=class(TpvGUILayout)
      private
       const AxisOrientationAxes:array[TpvGUILayoutOrientation,0..1] of TpvInt32=
              (
               (0,1), // pvgloHorizontal
               (1,0)  // pvgloVertical
              );
      private
       fOrientation:TpvGUILayoutOrientation;
       fMargin:TpvFloat;
       fDesignedSize:TpvVector2;
       fDesignedSizeProperty:TpvVector2Property;
       fSpacing:TpvVector2;
       fSpacingProperty:TpvVector2Property;
       fDirection:TpvGUIFlowLayoutDirection;
       fAlignments:TpvGUIFlowLayoutAlignments;
       fAlignmentOnBaseLine:boolean;
       fPositions:TpvVector2Array;
       fSizes:TpvVector2Array;
       function GetHorizontalAlignment:TpvGUIFlowLayoutAlignment; inline;
       procedure SetHorizontalAlignment(const aHorizontalAlignment:TpvGUIFlowLayoutAlignment); inline;
       function GetVerticalAlignment:TpvGUIFlowLayoutAlignment; inline;
       procedure SetVerticalAlignment(const aVerticalAlignment:TpvGUIFlowLayoutAlignment); inline;
      protected
       function GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       procedure PerformLayout(const aWidget:TpvGUIWidget); override;
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aOrientation:TpvGUILayoutOrientation=pvgloHorizontal;
                          const aMargin:TpvFloat=0.0;
                          const aDesignedWidth:TpvFloat=0.0;
                          const aDesignedHeight:TpvFloat=0.0;
                          const aHorizontalSpacing:TpvFloat=4.0;
                          const aVerticalSpacing:TpvFloat=4.0;
                          const aDirection:TpvGUIFlowLayoutDirection=pgfldLeftToRight;
                          const aHorizontalAlignment:TpvGUIFlowLayoutAlignment=pgflaLeading;
                          const aVerticalAlignment:TpvGUIFlowLayoutAlignment=pgflaLeading;
                          const aAlignmentOnBaseLine:boolean=false); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Orientation:TpvGUILayoutOrientation read fOrientation write fOrientation;
       property Margin:TpvFloat read fMargin write fMargin;
       property DesignedSize:TpvVector2Property read fDesignedSizeProperty;
       property Spacing:TpvVector2Property read fSpacingProperty;
       property Direction:TpvGUIFlowLayoutDirection read fDirection write fDirection;
       property HorizontalAlignment:TpvGUIFlowLayoutAlignment read GetHorizontalAlignment write SetHorizontalAlignment;
       property VerticalAlignment:TpvGUIFlowLayoutAlignment read GetVerticalAlignment write SetVerticalAlignment;
       property AlignmentOnBaseLine:boolean read fAlignmentOnBaseLine write fAlignmentOnBaseLine;
     end;

     TpvGUISkin=class(TpvGUIObject)
      private
      protected
       fSpacing:TpvFloat;
       fFontSize:TpvFloat;
       fWindowHeaderFontSize:tpvFloat;
       fButtonFontSize:TpvFloat;
       fTextEditFontSize:TpvFloat;
       fLabelFontSize:TpvFloat;
       fPopupMenuFontSize:TpvFloat;
       fWindowMenuFontSize:TpvFloat;
       fCheckBoxFontSize:TpvFloat;
       fCheckBoxSize:TpvVector2;
       fFontColor:TpvVector4;
       fWindowFontColor:TpvVector4;
       fButtonFontColor:TpvVector4;
       fTextEditFontColor:TpvVector4;
       fLabelFontColor:TpvVector4;
       fPopupMenuFontColor:TpvVector4;
       fWindowMenuFontColor:TpvVector4;
       fCheckBoxFontColor:TpvVector4;
       fImageSignedDistanceFieldColor:TpvVector4;
       fSignedDistanceFieldSpriteAtlas:TpvSpriteAtlas;
       fSansFont:TpvFont;
       fSansBoldFont:TpvFont;
       fSansBoldItalicFont:TpvFont;
       fSansItalicFont:TpvFont;
       fMonoFont:TpvFont;
       fWindowMenuHeight:TpvFloat;
       fWindowHeaderHeight:TpvFloat;
       fWindowResizeGripSize:TpvFloat;
       fMinimizedWindowMinimumWidth:TpvFloat;
       fMinimizedWindowMinimumHeight:TpvFloat;
       fWindowMinimumWidth:TpvFloat;
       fWindowMinimumHeight:TpvFloat;
       fWindowButtonIconHeight:TpvFloat;
       fPopupAnchorHeight:TpvFloat;
       fIconWindowClose:TObject;
       fIconWindowRestore:TObject;
       fIconWindowMinimize:TObject;
       fIconWindowMaximize:TObject;
       fIconMenuRight:TObject;
       fIconContentCut:TObject;
       fIconContentCopy:TObject;
       fIconContentPaste:TObject;
       fIconContentDelete:TObject;
       fIconSelectAll:TObject;
       fIconSelectNone:TObject;
       fIconChevronLeft:TObject;
       fIconChevronRight:TObject;
       fIconChevronUp:TObject;
       fIconChevronDown:TObject;
       fIconDirectionArrowLeft:TObject;
       fIconDirectionArrowRight:TObject;
       fIconDirectionArrowUp:TObject;
       fIconDirectionArrowDown:TObject;
       fIconCheck:TObject;
       fIconRoundCheck:TObject;
       fIconThumbUp:TObject;
       fIconThumbDown:TObject;
       fIconDialogAlert:TObject;
       fIconDialogError:TObject;
       fIconDialogInformation:TObject;
       fIconDialogQuestion:TObject;
       fIconDialogStop:TObject;
       fIconDialogWarning:TObject;
       fIconArrowUpDown:TObject;
       fIconChevronHeight:TpvFloat;
       fIconPopupMenuHeight:TpvFloat;
       fIconMenuRightHeight:TpvFloat;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure Setup; virtual;
      public
       procedure DrawFocus(const aCanvas:TpvCanvas;const aWidget:TpvGUIWidget); virtual;
      public
       procedure DrawMouse(const aCanvas:TpvCanvas;const aInstance:TpvGUIInstance); virtual;
      public
       function GetWidgetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; virtual;
       function GetWidgetLayoutPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; virtual;
      public
       function GetWindowPreferredSize(const aWindow:TpvGUIWindow):TpvVector2; virtual;
       procedure DrawWindow(const aCanvas:TpvCanvas;const aWindow:TpvGUIWindow); virtual;
      public
       function GetImagePreferredSize(const aImage:TpvGUIImage):TpvVector2; virtual;
       procedure DrawImage(const aCanvas:TpvCanvas;const aImage:TpvGUIImage); virtual;
      public
       function GetLabelPreferredSize(const aLabel:TpvGUILabel):TpvVector2; virtual;
       procedure DrawLabel(const aCanvas:TpvCanvas;const aLabel:TpvGUILabel); virtual;
      public
       function GetButtonPreferredSize(const aButton:TpvGUIButton):TpvVector2; virtual;
       procedure DrawButton(const aCanvas:TpvCanvas;const aButton:TpvGUIButton); virtual;
      public
       function GetCheckBoxPreferredSize(const aCheckBox:TpvGUICheckBox):TpvVector2; virtual;
       procedure DrawCheckBox(const aCanvas:TpvCanvas;const aCheckBox:TpvGUICheckBox); virtual;
      public
       function GetTextEditPreferredSize(const aTextEdit:TpvGUITextEdit):TpvVector2; virtual;
       procedure DrawTextEdit(const aCanvas:TpvCanvas;const aTextEdit:TpvGUITextEdit); virtual;
      public
       function GetPopupMenuPreferredSize(const aPopupMenu:TpvGUIPopupMenu):TpvVector2; virtual;
       procedure DrawPopupMenu(const aCanvas:TpvCanvas;const aPopupMenu:TpvGUIPopupMenu); virtual;
      public
       function GetWindowMenuPreferredSize(const aWindowMenu:TpvGUIWindowMenu):TpvVector2; virtual;
       procedure DrawWindowMenu(const aCanvas:TpvCanvas;const aWindowMenu:TpvGUIWindowMenu); virtual;
      public
       function GetScrollBarPreferredSize(const aScrollBar:TpvGUIScrollBar):TpvVector2; virtual;
       procedure DrawScrollBar(const aCanvas:TpvCanvas;const aScrollBar:TpvGUIScrollBar); virtual;
      public
       function GetSliderPreferredSize(const aSlider:TpvGUISlider):TpvVector2; virtual;
       procedure DrawSlider(const aCanvas:TpvCanvas;const aSlider:TpvGUISlider); virtual;
      public
       function GetProgressBarPreferredSize(const aProgressBar:TpvGUIProgressBar):TpvVector2; virtual;
       procedure DrawProgressBar(const aCanvas:TpvCanvas;const aProgressBar:TpvGUIProgressBar); virtual;
      public
       property FontColor:TpvVector4 read fFontColor write fFontColor;
       property WindowFontColor:TpvVector4 read fWindowFontColor write fWindowFontColor;
       property ButtonFontColor:TpvVector4 read fButtonFontColor write fButtonFontColor;
       property TextEditFontColor:TpvVector4 read fTextEditFontColor write fTextEditFontColor;
       property LabelFontColor:TpvVector4 read fLabelFontColor write fLabelFontColor;
       property PopupMenuFontColor:TpvVector4 read fPopupMenuFontColor write fPopupMenuFontColor;
       property WindowMenuFontColor:TpvVector4 read fWindowMenuFontColor write fWindowMenuFontColor;
       property CheckBoxFontColor:TpvVector4 read fCheckBoxFontColor write fCheckBoxFontColor;
       property ImageSignedDistanceFieldColor:TpvVector4 read fImageSignedDistanceFieldColor write fImageSignedDistanceFieldColor;
      published
       property SansFont:TpvFont read fSansFont write fSansFont;
       property SansBoldFont:TpvFont read fSansBoldFont write fSansBoldFont;
       property SansBoldItalicFont:TpvFont read fSansBoldItalicFont write fSansBoldItalicFont;
       property SansItalicFont:TpvFont read fSansItalicFont write fSansItalicFont;
       property MonoFont:TpvFont read fMonoFont write fMonoFont;
       property Spacing:TpvFloat read fSpacing write fSpacing;
       property FontSize:TpvFloat read fFontSize write fFontSize;
       property WindowHeaderFontSize:TpvFloat read fWindowHeaderFontSize write fWindowHeaderFontSize;
       property ButtonFontSize:TpvFloat read fButtonFontSize write fButtonFontSize;
       property TextEditFontSize:TpvFloat read fTextEditFontSize write fTextEditFontSize;
       property LabelFontSize:TpvFloat read fLabelFontSize write fLabelFontSize;
       property PopupMenuFontSize:TpvFloat read fPopupMenuFontSize write fPopupMenuFontSize;
       property WindowMenuFontSize:TpvFloat read fWindowMenuFontSize write fWindowMenuFontSize;
       property CheckBoxFontSize:TpvFloat read fCheckBoxFontSize write fCheckBoxFontSize;
       property SignedDistanceFieldSpriteAtlas:TpvSpriteAtlas read fSignedDistanceFieldSpriteAtlas;
       property WindowMenuHeight:TpvFloat read fWindowMenuHeight write fWindowMenuHeight;
       property WindowHeaderHeight:TpvFloat read fWindowHeaderHeight write fWindowHeaderHeight;
       property WindowResizeGripSize:TpvFloat read fWindowResizeGripSize write fWindowResizeGripSize;
       property MinimizedWindowMinimumWidth:TpvFloat read fMinimizedWindowMinimumWidth write fMinimizedWindowMinimumWidth;
       property MinimizedWindowMinimumHeight:TpvFloat read fMinimizedWindowMinimumHeight write fMinimizedWindowMinimumHeight;
       property WindowMinimumWidth:TpvFloat read fWindowMinimumWidth write fWindowMinimumWidth;
       property WindowMinimumHeight:TpvFloat read fWindowMinimumHeight write fWindowMinimumHeight;
       property PopupAnchorHeight:TpvFloat read fPopupAnchorHeight write fPopupAnchorHeight;
       property IconWindowClose:TObject read fIconWindowClose write fIconWindowClose;
       property IconWindowRestore:TObject read fIconWindowRestore write fIconWindowRestore;
       property IconWindowMinimize:TObject read fIconWindowMinimize write fIconWindowMinimize;
       property IconWindowMaximize:TObject read fIconWindowMaximize write fIconWindowMaximize;
       property IconMenuRight:TObject read fIconMenuRight write fIconMenuRight;
       property IconContentCut:TObject read fIconContentCut write fIconContentCut;
       property IconContentCopy:TObject read fIconContentCopy write fIconContentCopy;
       property IconContentPaste:TObject read fIconContentPaste write fIconContentPaste;
       property IconContentDelete:TObject read fIconContentDelete write fIconContentDelete;
       property IconSelectAll:TObject read fIconSelectAll write fIconSelectAll;
       property IconSelectNone:TObject read fIconSelectNone write fIconSelectNone;
       property IconChevronLeft:TObject read fIconChevronLeft write fIconChevronLeft;
       property IconChevronRight:TObject read fIconChevronRight write fIconChevronRight;
       property IconChevronUp:TObject read fIconChevronUp write fIconChevronUp;
       property IconChevronDown:TObject read fIconChevronDown write fIconChevronDown;
       property IconDirectionArrowLeft:TObject read fIconDirectionArrowLeft write fIconDirectionArrowLeft;
       property IconDirectionArrowRight:TObject read fIconDirectionArrowRight write fIconDirectionArrowRight;
       property IconDirectionArrowUp:TObject read fIconDirectionArrowUp write fIconDirectionArrowUp;
       property IconDirectionArrowDown:TObject read fIconDirectionArrowDown write fIconDirectionArrowDown;
       property IconCheck:TObject read fIconCheck write fIconCheck;
       property IconRoundCheck:TObject read fIconRoundCheck write fIconRoundCheck;
       property IconThumbUp:TObject read fIconThumbUp write fIconThumbUp;
       property IconThumbDown:TObject read fIconThumbDown write fIconThumbDown;
       property IconDialogAlert:TObject read fIconDialogAlert write fIconDialogAlert;
       property IconDialogError:TObject read fIconDialogError write fIconDialogError;
       property IconDialogInformation:TObject read fIconDialogInformation write fIconDialogInformation;
       property IconDialogQuestion:TObject read fIconDialogQuestion write fIconDialogQuestion;
       property IconDialogStop:TObject read fIconDialogStop write fIconDialogStop;
       property IconDialogWarning:TObject read fIconDialogWarning write fIconDialogWarning;
       property IconArrowUpDown:TObject read fIconArrowUpDown write fIconArrowUpDown;
       property IconChevronHeight:TpvFloat read fIconChevronHeight write fIconChevronHeight;
       property IconPopupMenuHeight:TpvFloat read fIconPopupMenuHeight write fIconPopupMenuHeight;
       property IconMenuRightHeight:TpvFloat read fIconMenuRightHeight write fIconMenuRightHeight;
     end;

     TpvGUIDefaultVectorBasedSkin=class(TpvGUISkin)
      private
       const ButtonHorizontalBorderSpacing=10.0;
             ButtonIconSpacing=8.0;
      protected
       fUnfocusedWindowHeaderFontShadow:boolean;
       fFocusedWindowHeaderFontShadow:boolean;
       fUnfocusedWindowHeaderFontShadowOffset:TpvVector2;
       fFocusedWindowHeaderFontShadowOffset:TpvVector2;
       fUnfocusedWindowHeaderFontShadowColor:TpvVector4;
       fFocusedWindowHeaderFontShadowColor:TpvVector4;
       fUnfocusedWindowHeaderFontColor:TpvVector4;
       fFocusedWindowHeaderFontColor:TpvVector4;
       fWindowShadowWidth:TpvFloat;
       fWindowShadowHeight:TpvFloat;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure Setup; override;
      public
       procedure DrawFocus(const aCanvas:TpvCanvas;const aWidget:TpvGUIWidget); override;
      public
       procedure DrawMouse(const aCanvas:TpvCanvas;const aInstance:TpvGUIInstance); override;
      public
       function GetWidgetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
       function GetWidgetLayoutPreferredSize(const aWidget:TpvGUIWidget):TpvVector2; override;
      public
       function GetWindowPreferredSize(const aWindow:TpvGUIWindow):TpvVector2; override;
       procedure DrawWindow(const aCanvas:TpvCanvas;const aWindow:TpvGUIWindow); override;
      public
       function GetImagePreferredSize(const aImage:TpvGUIImage):TpvVector2; override;
       procedure DrawImage(const aCanvas:TpvCanvas;const aImage:TpvGUIImage); override;
      public
       function GetLabelPreferredSize(const aLabel:TpvGUILabel):TpvVector2; override;
       procedure DrawLabel(const aCanvas:TpvCanvas;const aLabel:TpvGUILabel); override;
      public
       function GetButtonPreferredSize(const aButton:TpvGUIButton):TpvVector2; override;
       procedure DrawButton(const aCanvas:TpvCanvas;const aButton:TpvGUIButton); override;
      public
       function GetCheckBoxPreferredSize(const aCheckBox:TpvGUICheckBox):TpvVector2; override;
       procedure DrawCheckBox(const aCanvas:TpvCanvas;const aCheckBox:TpvGUICheckBox); override;
      public
       function GetTextEditPreferredSize(const aTextEdit:TpvGUITextEdit):TpvVector2; override;
       procedure DrawTextEdit(const aCanvas:TpvCanvas;const aTextEdit:TpvGUITextEdit); override;
      private
       function ProcessPopupMenuItems(const aPopupMenu:TpvGUIPopupMenu):TpvVector2;
      public
       function GetPopupMenuPreferredSize(const aPopupMenu:TpvGUIPopupMenu):TpvVector2; override;
       procedure DrawPopupMenu(const aCanvas:TpvCanvas;const aPopupMenu:TpvGUIPopupMenu); override;
      private
       procedure ProcessWindowMenuItems(const aWindowMenu:TpvGUIWindowMenu);
      public
       function GetWindowMenuPreferredSize(const aWindowMenu:TpvGUIWindowMenu):TpvVector2; override;
       procedure DrawWindowMenu(const aCanvas:TpvCanvas;const aWindowMenu:TpvGUIWindowMenu); override;
      public
       function GetScrollBarPreferredSize(const aScrollBar:TpvGUIScrollBar):TpvVector2; override;
       procedure DrawScrollBar(const aCanvas:TpvCanvas;const aScrollBar:TpvGUIScrollBar); override;
      public
       function GetSliderPreferredSize(const aSlider:TpvGUISlider):TpvVector2; override;
       procedure DrawSlider(const aCanvas:TpvCanvas;const aSlider:TpvGUISlider); override;
      public
       function GetProgressBarPreferredSize(const aProgressBar:TpvGUIProgressBar):TpvVector2; override;
       procedure DrawProgressBar(const aCanvas:TpvCanvas;const aProgressBar:TpvGUIProgressBar); override;
      public
       property UnfocusedWindowHeaderFontShadowOffset:TpvVector2 read fUnfocusedWindowHeaderFontShadowOffset write fUnfocusedWindowHeaderFontShadowOffset;
       property FocusedWindowHeaderFontShadowOffset:TpvVector2 read fFocusedWindowHeaderFontShadowOffset write fFocusedWindowHeaderFontShadowOffset;
       property UnfocusedWindowHeaderFontShadowColor:TpvVector4 read fUnfocusedWindowHeaderFontShadowColor write fUnfocusedWindowHeaderFontShadowColor;
       property FocusedWindowHeaderFontShadowColor:TpvVector4 read fFocusedWindowHeaderFontShadowColor write fFocusedWindowHeaderFontShadowColor;
       property UnfocusedWindowHeaderFontColor:TpvVector4 read fUnfocusedWindowHeaderFontColor write fUnfocusedWindowHeaderFontColor;
       property FocusedWindowHeaderFontColor:TpvVector4 read fFocusedWindowHeaderFontColor write fFocusedWindowHeaderFontColor;
      published
       property UnfocusedWindowHeaderFontShadow:boolean read fUnfocusedWindowHeaderFontShadow write fUnfocusedWindowHeaderFontShadow;
       property FocusedWindowHeaderFontShadow:boolean read fFocusedWindowHeaderFontShadow write fFocusedWindowHeaderFontShadow;
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
       pvgwfDraggable,
       pvgwfFocused,
       pvgwfPointerFocused,
       pvgwfKeyPreview,
       pvgwfWantAllKeys,
       pvgwfTabStop,
       pvgwfScissor,
       pvgwfDrawFocus
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
       fPopups:TpvGUIObjectList;
       fCursor:TpvGUICursor;
       fPosition:TpvVector2;
       fSize:TpvVector2;
       fFixedSize:TpvVector2;
       fPositionProperty:TpvVector2Property;
       fSizeProperty:TpvVector2Property;
       fFixedSizeProperty:TpvVector2Property;
       fWidgetFlags:TpvGUIWidgetFlags;
       fHint:TpvUTF8String;
       fFont:TpvFont;
       fFontSize:TpvFloat;
       fFontColor:TpvVector4;
       fTextHorizontalAlignment:TpvGUITextAlignment;
       fTextVerticalAlignment:TpvGUITextAlignment;
       fTextTruncation:TpvGUITextTruncation;
       fOnKeyEvent:TpvGUIOnKeyEvent;
       fOnPointerEvent:TpvGUIOnPointerEvent;
       fOnScrolled:TpvGUIOnScrolled;
       fParentClipRect:TpvRect;
       fClipRect:TpvRect;
       fModelMatrix:TpvMatrix4x4;
       function GetEnabled:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetEnabled(const aEnabled:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetVisible:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetVisible(const aVisible:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetDraggable:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetDraggable(const aDraggable:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetFocused:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetFocused(const aFocused:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetPointerFocused:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetPointerFocused(const aPointerFocused:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetTabStop:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetTabStop(const aTabStop:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetKeyPreview:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetKeyPreview(const aKeyPreview:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetWantAllKeys:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetWantAllKeys(const aWantAllKeys:boolean); {$ifdef CAN_INLINE}inline;{$endif}
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
       function GetLastParentWindow:TpvGUIWindow;
       function GetScissorParent:TpvGUIWidget;
       procedure SetCanvas(const aCanvas:TpvCanvas); virtual;
       function GetSkin:TpvGUISkin; virtual;
       procedure SetSkin(const aSkin:TpvGUISkin); virtual;
       function GetWidgetPreferredSize:TpvVector2; virtual;
       function GetWidgetLayoutPreferredSize:TpvVector2; virtual;
       function GetPreferredSize:TpvVector2; virtual;
       function GetFixedSize:TpvVector2; virtual;
       function GetHighlightRect:TpvRect; virtual;
       function GetFont:TpvFont; virtual;
       function GetFontSize:TpvFloat; virtual;
       function GetFontColor:TpvVector4; virtual;
      protected
       property TextHorizontalAlignment:TpvGUITextAlignment read fTextHorizontalAlignment write fTextHorizontalAlignment;
       property TextVerticalAlignment:TpvGUITextAlignment read fTextVerticalAlignment write fTextVerticalAlignment;
       property TextTruncation:TpvGUITextTruncation read fTextTruncation write fTextTruncation;
       property Font:TpvFont read GetFont write fFont;
       property FontColor:TpvVector4 read GetFontColor write fFontColor;
       property FontSize:TpvFloat read GetFontSize write fFontSize;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release; virtual;
       function GetEnumerator:TpvGUIWidgetEnumerator;
       function Contains(const aPosition:TpvVector2):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure GetTabList(const aList:Classes.TList);
       function FindWidget(const aPosition:TpvVector2):TpvGUIWidget; virtual;
       function FindNextWidget(const aCurrentWidget:TpvGUIWidget;const aForward,aCheckTabStop,aCheckParent:boolean):TpvGUIWidget; virtual;
       function ProcessTab(const aFromWidget:TpvGUIWidget;const aToPrevious:boolean):boolean; virtual;
       procedure SetSizeToPreferredSize; virtual;
       procedure PerformLayout; virtual;
       procedure RequestFocus; virtual;
       procedure Show; virtual;
       procedure Hide; virtual;
       function Enter:boolean; virtual;
       function Leave:boolean; virtual;
       function PointerEnter:boolean; virtual;
       function PointerLeave:boolean; virtual;
       function DragEvent(const aPosition:TpvVector2):boolean; virtual;
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
       property HighlightRect:TpvRect read GetHighlightRect;
       property ParentClipRect:TpvRect read fParentClipRect write fParentClipRect;
       property ClipRect:TpvRect read fClipRect write fClipRect;
       property ModelMatrix:TpvMatrix4x4 read fModelMatrix write fModelMatrix;
      published
       property Window:TpvGUIWindow read GetWindow;
       property ScissorParent:TpvGUIWidget read GetScissorParent;
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
       property Draggable:boolean read GetDraggable write SetDraggable;
       property RecursiveVisible:boolean read GetRecursiveVisible;
       property Focused:boolean read GetFocused write SetFocused;
       property PointerFocused:boolean read GetPointerFocused write SetPointerFocused;
       property TabStop:boolean read GetTabStop write SetTabStop;
       property KeyPreview:boolean read GetKeyPreview write SetKeyPreview;
       property WantAllKeys:boolean read GetWantAllKeys write SetWantAllKeys;
       property Left:TpvFloat read GetLeft write SetLeft;
       property Top:TpvFloat read GetTop write SetTop;
       property Width:TpvFloat read GetWidth write SetWidth;
       property Height:TpvFloat read GetHeight write SetHeight;
       property FixedWidth:TpvFloat read GetFixedWidth write SetFixedWidth;
       property FixedHeight:TpvFloat read GetFixedHeight write SetFixedHeight;
       property Hint:TpvUTF8String read fHint write fHint;
       property OnKeyEvent:TpvGUIOnKeyEvent read fOnKeyEvent write fOnKeyEvent;
       property OnPointerEvent:TpvGUIOnPointerEvent read fOnPointerEvent write fOnPointerEvent;
       property OnScrolled:TpvGUIOnScrolled read fOnScrolled write fOnScrolled;
     end;

     TpvGUIInstanceBufferReferenceCountedObjects=array of TpvReferenceCountedObject;

     PpvGUIInstanceBuffer=^TpvGUIInstanceBuffer;
     TpvGUIInstanceBuffer=record
      ReferenceCountedObjects:TpvGUIInstanceBufferReferenceCountedObjects;
      CountReferenceCountedObjects:TpvInt32;
     end;

     TpvGUIInstanceBuffers=array of TpvGUIInstanceBuffer;

     TpvGUIHolder=class(TpvGUIWidget)
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
     end;

     TpvGUIInstance=class(TpvGUIHolder)
      private
       fVulkanDevice:TpvVulkanDevice;
       fFontCodePointRanges:TpvFontCodePointRanges;
       fStandardSkin:TpvGUISkin;
       fDrawWidgetBounds:boolean;
       fBuffers:TpvGUIInstanceBuffers;
       fCountBuffers:TpvInt32;
       fUpdateBufferIndex:TpvInt32;
       fDrawBufferIndex:TpvInt32;
       fDeltaTime:TpvDouble;
       fTime:TpvDouble;
       fPopupMenuStack:TpvGUIObjectList;
       fModalWindowStack:TpvGUIObjectList;
       fLastFocusPath:TpvGUIObjectList;
       fCurrentFocusPath:TpvGUIObjectList;
       fDragWidget:TpvGUIWidget;
       fWindow:TpvGUIWindow;
       fContent:TpvGUIPanel;
       fMenu:TpvGUIWindowMenu;
       fFocusedWidget:TpvGUIWidget;
       fHoveredWidget:TpvGUIWidget;
       fMousePosition:TpvVector2;
       fVisibleCursor:TpvGUICursor;
       procedure SetCountBuffers(const aCountBuffers:TpvInt32);
       procedure SetUpdateBufferIndex(const aUpdateBufferIndex:TpvInt32);
       procedure SetDrawBufferIndex(const aDrawBufferIndex:TpvInt32);
       procedure DisposeWindow(const aWindow:TpvGUIWindow);
       procedure CenterWindow(const aWindow:TpvGUIWindow);
       procedure MoveWindowToFront(const aWindow:TpvGUIWindow);
       procedure FindHoveredWidget;
      public
       constructor Create(const aVulkanDevice:TpvVulkanDevice;
                          const aFontCodePointRanges:TpvFontCodePointRanges=nil); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure ReleaseObject(const aGUIObject:TpvGUIObject);
       procedure ClearReferenceCountedObjectList;
       procedure AddReferenceCountedObjectForNextDraw(const aObject:TpvReferenceCountedObject);
       procedure UpdateFocus(const aWidget:TpvGUIWidget);
       function AddMenu:TpvGUIWindowMenu;
       procedure PerformLayout; override;
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
       property FocusedWidget:TpvGUIWidget read fFocusedWidget write UpdateFocus;
       property HoveredWidget:TpvGUIWidget read fHoveredWidget;
       property Content:TpvGUIPanel read fContent;
       property Menu:TpvGUIWindowMenu read fMenu write fMenu;
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

     PpvGUIWindowState=^TpvGUIWindowState;
     TpvGUIWindowState=
      (
       pvgwsNormal,
       pvgwsMinimized,
       pvgwsMaximized
      );

     TpvGUIWindow=class(TpvGUIHolder)
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
       fLastWindowState:TpvGUIWindowState;
       fWindowState:TpvGUIWindowState;
       fMenu:TpvGUIWindowMenu;
       fButtonPanel:TpvGUIPanel;
       fContent:TpvGUIPanel;
       fSavedPosition:TpvVector2;
       fSavedSize:TpvVector2;
       fMinimizationButton:TpvGUIButton;
       fMaximizationButton:TpvGUIButton;
       fCloseButton:TpvGUIButton;
       function GetFixedSize:TpvVector2; override;
       procedure SetWindowFlags(const aWindowFlags:TpvGUIWindowFlags);
       procedure SetWindowState(const aWindowState:TpvGUIWindowState);
       function GetModal:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure SetModal(const aModal:boolean); {$ifdef CAN_INLINE}inline;{$endif}
       function GetButtonPanel:TpvGUIPanel;
       function GetFontColor:TpvVector4; override;
       function GetPreferredSize:TpvVector2; override;
       procedure OnWindowHeaderButtonClick(const aSender:TpvGUIObject); virtual;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure AddMinimizationButton;
       procedure AddMaximizationButton;
       procedure AddCloseButton;
       function AddMenu:TpvGUIWindowMenu;
       procedure DisposeWindow;
       procedure Center;
       function FindWidget(const aPosition:TpvVector2):TpvGUIWidget; override;
       procedure PerformLayout; override;
       function DragEvent(const aPosition:TpvVector2):boolean; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      public
       property FontColor;
       property SavedPosition:TpvVector2 read fSavedPosition write fSavedPosition;
       property SavedSize:TpvVector2 read fSavedSize write fSavedSize;
      published
       property Title:TpvUTF8String read fTitle write fTitle;
       property WindowFlags:TpvGUIWindowFlags read fWindowFlags write SetWindowFlags;
       property WindowState:TpvGUIWindowState read fWindowState write SetWindowState;
       property Modal:boolean read GetModal write SetModal;
       property ButtonPanel:TpvGUIPanel read GetButtonPanel;
       property Content:TpvGUIPanel read fContent;
       property Menu:TpvGUIWindowMenu read fMenu;
       property Font;
       property TextHorizontalAlignment;
       property TextTruncation;
     end;

     PpvGUIMessageDialogButton=^TpvGUIMessageDialogButton;
     TpvGUIMessageDialogButton=record
      private
       fID:TpvInt32;
       fCaption:TpvUTF8String;
       fIcon:TObject;
       fIconHeight:TpvFloat;
       fKeyCode:TpvInt32;
       fButton:TpvGUIButton;
      public
       constructor Create(const aID:TpvInt32;
                          const aCaption:TpvUTF8String;
                          const aKeyCode:TpvInt32=KEYCODE_UNKNOWN;
                          const aIcon:TObject=nil;
                          const aIconHeight:TpvFloat=24.0);
       property ID:TpvInt32 read fID write fID;
       property Caption:TpvUTF8String read fCaption write fCaption;
       property Icon:TObject read fIcon write fIcon;
     end;

     TpvGUIMessageDialogButtons=array of TpvGUIMessageDialogButton;

     TpvGUIMessageDialogOnButtonClick=procedure(const aSender:TpvGUIObject;const aID:TpvInt32) of object;

     TpvGUIMessageDialog=class(TpvGUIWindow)
      private
       fMessagePanel:TpvGUIPanel;
       fMessageImage:TpvGUIImage;
       fMessageLabel:TpvGUILabel;
       fMessageDialogButtonPanel:TpvGUIPanel;
       fButtons:TpvGUIMessageDialogButtons;
       fOnButtonClick:TpvGUIMessageDialogOnButtonClick;
       procedure MessageDialogOnButtonClick(const aSender:TpvGUIObject);
      public
       constructor Create(const aParent:TpvGUIObject;
                          const aTitle:TpvUTF8String;
                          const aMessage:TpvUTF8String;
                          const aButtons:array of TpvGUIMessageDialogButton;
                          const aIcon:TObject=nil;
                          const aIconHeight:TpvFloat=36.0); reintroduce; overload;
       constructor Create(const aParent:TpvGUIObject;
                          const aTitle:TpvUTF8String;
                          const aMessage:TpvUTF8String;
                          const aButtons:array of TpvUTF8String;
                          const aIcon:TObject=nil;
                          const aIconHeight:TpvFloat=36.0); reintroduce; overload;
       constructor Create(const aParent:TpvGUIObject;
                          const aTitle:TpvUTF8String;
                          const aMessage:TpvUTF8String;
                          const aIcon:TObject=nil;
                          const aIconHeight:TpvFloat=36.0); reintroduce; overload;
       destructor Destroy; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
      published
       property OnButtonClick:TpvGUIMessageDialogOnButtonClick read fOnButtonClick write fOnButtonClick;
     end;

     PpvGUIPopupSide=^TpvGUIPopupAnchorSide;
     TpvGUIPopupAnchorSide=
      (
       pvgpasNone,
       pvgpasLeft,
       pvgpasRight,
       pvgpasTop,
       pvgpasBottom
      );

     TpvGUIPopup=class(TpvGUIWindow)
      private
       fParentWidget:TpvGUIWidget;
       fParentHolder:TpvGUIHolder;
       fAnchorSide:TpvGUIPopupAnchorSide;
       fAnchorPosition:TpvVector2;
       fAnchorPositionProperty:TpvVector2Property;
       fAnchorOffset:TpvVector2;
       fAnchorOffsetProperty:TpvVector2Property;
       fAnchorSideOffset:TpvVector2;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure UpdatePosition;
       procedure PerformLayout; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property ParentWidget:TpvGUIWidget read fParentWidget;
       property ParentHolder:TpvGUIHolder read fParentHolder;
       property AnchorSide:TpvGUIPopupAnchorSide read fAnchorSide write fAnchorSide;
       property AnchorPosition:TpvVector2Property read fAnchorPositionProperty;
       property AnchorOffset:TpvVector2Property read fAnchorOffsetProperty;
     end;

     TpvGUIPanel=class(TpvGUIWidget);

     TpvGUIImage=class(TpvGUIWidget)
      private
       fImage:TObject;
      protected
       function GetPreferredSize:TpvVector2; override;
      public
       constructor Create(const aParent:TpvGUIObject;const aImage:TObject); reintroduce;
       destructor Destroy; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Image:TObject read fImage write fImage;
     end;

     TpvGUILabel=class(TpvGUIWidget)
      private
       fCaption:TpvUTF8String;
      protected
       function GetFontSize:TpvFloat; override;
       function GetFontColor:TpvVector4; override;
       function GetPreferredSize:TpvVector2; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      public
       property FontColor;
      published
       property Font;
       property FontSize;
       property Caption:TpvUTF8String read fCaption write fCaption;
       property TextHorizontalAlignment;
       property TextVerticalAlignment;
       property TextTruncation;
     end;

     PpvGUIButtonFlag=^TpvGUIButtonFlag;
     TpvGUIButtonFlag=
      (
       pvgbfNormalButton,
       pvgbfRadioButton,
       pvgbfToggleButton,
       pvgbfPopupButton,
       pvgbfDown
      );

     PpvGUIButtonFlags=^TpvGUIButtonFlags;
     TpvGUIButtonFlags=set of TpvGUIButtonFlag;

     TpvGUIButtonGroup=class(TObjectList<TpvGUIButton>);

     PpvGUIButtonIconPosition=^TpvGUIButtonIconPosition;
     TpvGUIButtonIconPosition=
      (
       pvgbipLeft,
       pvgbipLeftCentered,
       pvgbipRightCentered,
       pvgbipRight
      );

     TpvGUIButton=class(TpvGUIWidget)
      private
       fButtonFlags:TpvGUIButtonFlags;
       fButtonGroup:TpvGUIButtonGroup;
       fCaption:TpvUTF8String;
       fIconPosition:TpvGUIButtonIconPosition;
       fIcon:TObject;
       fIconHeight:TpvFloat;
       fOnClick:TpvGUIOnEvent;
       fOnChange:TpvGUIOnChange;
       procedure ProcessDown(const aPosition:TpvVector2);
       procedure ProcessUp(const aPosition:TpvVector2);
      protected
       function GetDown:boolean; inline;
       procedure SetDown(const aDown:boolean); virtual;
       function GetFontSize:TpvFloat; override;
       function GetFontColor:TpvVector4; override;
       function GetPreferredSize:TpvVector2; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      public
       property FontColor;
      published
       property Font;
       property FontSize;
       property ButtonFlags:TpvGUIButtonFlags read fButtonFlags write fButtonFlags;
       property ButtonGroup:TpvGUIButtonGroup read fButtonGroup;
       property Down:boolean read GetDown write SetDown;
       property Caption:TpvUTF8String read fCaption write fCaption;
       property IconPosition:TpvGUIButtonIconPosition read fIconPosition write fIconPosition;
       property Icon:TObject read fIcon write fIcon;
       property IconHeight:TpvFloat read fIconHeight write fIconHeight;
       property OnClick:TpvGUIOnEvent read fOnClick write fOnClick;
       property OnChange:TpvGUIOnChange read fOnChange write fOnChange;
       property TextHorizontalAlignment;
       property TextVerticalAlignment;
       property TextTruncation;
     end;

     TpvGUIRadioButton=class(TpvGUIButton)
      public
       constructor Create(const aParent:TpvGUIObject); override;
     end;

     TpvGUIToggleButton=class(TpvGUIButton)
      public
       constructor Create(const aParent:TpvGUIObject); override;
     end;

     TpvGUIPopupButton=class(TpvGUIButton)
      private
       fPopup:TpvGUIPopup;
      protected
       procedure SetDown(const aDown:boolean); override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       procedure PerformLayout; override;
      published
       property Popup:TpvGUIPopup read fPopup;
     end;

     TpvGUIPopupMenuButton=class(TpvGUIButton)
      private
       fPopupMenu:TpvGUIPopupMenu;
      protected
       procedure SetDown(const aDown:boolean); override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       procedure Update; override;
      published
       property PopupMenu:TpvGUIPopupMenu read fPopupMenu;
     end;

     TpvGUIToolButton=class(TpvGUIButton)
      public
       constructor Create(const aParent:TpvGUIObject); override;
     end;

     PpvGUICheckBoxFlag=^TpvGUICheckBoxFlag;
     TpvGUICheckBoxFlag=
      (
       pvgcbfRadioCheckBox,
       pvgcbfPushed,
       pvgcbfChecked
      );

     PpvGUICheckBoxFlags=^TpvGUICheckBoxFlags;
     TpvGUICheckBoxFlags=set of TpvGUICheckBoxFlag;

     TpvGUICheckBoxGroup=class(TObjectList<TpvGUICheckBox>);

     TpvGUICheckBox=class(TpvGUIWidget)
      private
       fCheckBoxFlags:TpvGUICheckBoxFlags;
       fCheckBoxGroup:TpvGUICheckBoxGroup;
       fCaption:TpvUTF8String;
       fOnChange:TpvGUIOnChange;
       function GetPushed:boolean; inline;
       procedure SetPushed(const aPushed:boolean); inline;
       function GetChecked:boolean; inline;
       procedure SetChecked(const aChecked:boolean);
      protected
       function GetHighlightRect:TpvRect; override;
       function GetFontSize:TpvFloat; override;
       function GetFontColor:TpvVector4; override;
       function GetPreferredSize:TpvVector2; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      public
       property FontColor;
      published
       property Font;
       property FontSize;
       property TextHorizontalAlignment;
       property TextVerticalAlignment;
       property TextTruncation;
       property Pushed:boolean read GetPushed write SetPushed;
       property Checked:boolean read GetChecked write SetChecked;
       property Caption:TpvUTF8String read fCaption write fCaption;
       property OnChange:TpvGUIOnChange read fOnChange write fOnChange;
     end;

     TpvGUIRadioCheckBox=class(TpvGUICheckBox)
      public
       constructor Create(const aParent:TpvGUIObject); override;
     end;

     TpvGUITextEditOnCheckText=function(const aText:TpvUTF8String):boolean of object;

     TpvGUITextEdit=class(TpvGUIWidget)
      private
       fEditable:boolean;
       fSpinnable:boolean;
       fText:TpvUTF8String;
       fTextGlyphRects:TpvCanvasTextGlyphRects;
       fCountTextGlyphRects:TpvInt32;
       fTextOffset:TpvFloat;
       fTextCursorPositionOffset:TpvInt32;
       fTextCursorPositionIndex:TpvInt32;
       fTextSelectionStart:TpvInt32;
       fTextSelectionEnd:TpvInt32;
       fMinimumWidth:TpvFloat;
       fMinimumHeight:TpvFloat;
       fDragRect:TpvRect;
       fPopupMenu:TpvGUIPopupMenu;
       fOnClick:TpvGUIOnEvent;
       fOnChange:TpvGUIOnEvent;
       fOnCheckText:TpvGUITextEditOnCheckText;
       procedure PopupMenuOnCutClick(const aSender:TpvGUIObject);
       procedure PopupMenuOnCopyClick(const aSender:TpvGUIObject);
       procedure PopupMenuOnPasteClick(const aSender:TpvGUIObject);
       procedure PopupMenuOnDeleteClick(const aSender:TpvGUIObject);
       procedure PopupMenuOnSelectAllClick(const aSender:TpvGUIObject);
       procedure PopupMenuOnSelectNoneClick(const aSender:TpvGUIObject);
       property Spinnable:boolean read fSpinnable write fSpinnable;
      protected
       function GetFontSize:TpvFloat; override;
       function GetFontColor:TpvVector4; override;
       function GetPreferredSize:TpvVector2; override;
       function GetEditable:boolean;
       procedure SetEditable(const aEditable:boolean);
       procedure UpdateText; virtual;
       function CheckText(const aText:TpvUTF8String):boolean; virtual;
       function GetText:TpvUTF8String; virtual;
       procedure SetText(const aText:TpvUTF8String); virtual;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function Enter:boolean; override;
       function Leave:boolean; override;
       procedure CutSelectedText;
       procedure CopySelectedText;
       procedure PasteText;
       procedure DeleteSelectedText;
       procedure SelectAll;
       procedure SelectNone;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Font;
       property FontSize;
       property Editable:boolean read GetEditable write SetEditable;
       property Text:TpvUTF8String read GetText write SetText;
       property MinimumWidth:TpvFloat read fMinimumWidth write fMinimumWidth;
       property MinimumHeight:TpvFloat read fMinimumHeight write fMinimumHeight;
       property OnClick:TpvGUIOnEvent read fOnClick write fOnClick;
       property OnChange:TpvGUIOnEvent read fOnChange write fOnChange;
       property OnCheckText:TpvGUITextEditOnCheckText read fOnCheckText write fOnCheckText;
       property TextHorizontalAlignment;
       property TextVerticalAlignment;
       property TextTruncation;
     end;

     TpvGUIIntegerEdit=class(TpvGUITextEdit)
      private
       fMinimumValue:TpvInt64;
       fMaximumValue:TpvInt64;
       fSmallStep:TpvInt64;
       fLargeStep:TpvInt64;
       procedure UpdateText; override;
       procedure ApplyMinMaxValueBounds;
       procedure SetMinimumValue(const aMinimumValue:TpvInt64);
       procedure SetMaximumValue(const aMaximumValue:TpvInt64);
       function GetValue:TpvInt64;
       procedure SetValue(const aValue:TpvInt64);
       function CheckText(const aText:TpvUTF8String):boolean; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function DragEvent(const aPosition:TpvVector2):boolean; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
      published
       property MinimumValue:TpvInt64 read fMinimumValue write SetMinimumValue;
       property MaximumValue:TpvInt64 read fMaximumValue write SetMaximumValue;
       property SmallStep:TpvInt64 read fSmallStep write fSmallStep;
       property LargeStep:TpvInt64 read fLargeStep write fLargeStep;
       property Value:TpvInt64 read GetValue write SetValue;
       property Spinnable;
     end;

     TpvGUIFloatEdit=class(TpvGUITextEdit)
      private
       fMinimumValue:TpvDouble;
       fMaximumValue:TpvDouble;
       fSmallStep:TpvDouble;
       fLargeStep:TpvDouble;
       fDigits:TpvInt32;
       procedure UpdateText; override;
       procedure ApplyMinMaxValueBounds;
       procedure SetMinimumValue(const aMinimumValue:TpvDouble);
       procedure SetMaximumValue(const aMaximumValue:TpvDouble);
       function GetValue:TpvDouble;
       procedure SetValue(const aValue:TpvDouble);
       function CheckText(const aText:TpvUTF8String):boolean; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function DragEvent(const aPosition:TpvVector2):boolean; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
      published
       property MinimumValue:TpvDouble read fMinimumValue write SetMinimumValue;
       property MaximumValue:TpvDouble read fMaximumValue write SetMaximumValue;
       property SmallStep:TpvDouble read fSmallStep write fSmallStep;
       property LargeStep:TpvDouble read fLargeStep write fLargeStep;
       property Digits:TpvInt32 read fDigits write fDigits;
       property Value:TpvDouble read GetValue write SetValue;
       property Spinnable;
     end;

     PpvGUIMenuItemFlag=^TpvGUIMenuItemFlag;
     TpvGUIMenuItemFlag=
      (
       pvgmifEnabled
      );

     PpvGUIMenuItemFlags=^TpvGUIMenuItemFlags;
     TpvGUIMenuItemFlags=set of TpvGUIMenuItemFlag;

     TpvGUIMenuItem=class(TpvGUIObject)
      private
       fFlags:TpvGUIMenuItemFlags;
       fCaption:TpvUTF8String;
       fShortcutHint:TpvUTF8String;
       fIcon:TObject;
       fIconHeight:TpvFloat;
       fRect:TpvRect;
       fOpenRect:TpvRect;
       fOnClick:TpvGUIOnEvent;
       function GetEnabled:boolean; inline;
       procedure SetEnabled(const aEnabled:boolean); inline;
       function GetSelectable:boolean; inline;
       function GetMenu:TpvGUIPopupMenu;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
      published
       property Enabled:boolean read GetEnabled write SetEnabled;
       property Selectable:boolean read GetSelectable;
       property Caption:TpvUTF8String read fCaption write fCaption;
       property ShortcutHint:TpvUTF8String read fShortcutHint write fShortcutHint;
       property Icon:TObject read fIcon write fIcon;
       property IconHeight:TpvFloat read fIconHeight write fIconHeight;
       property Menu:TpvGUIPopupMenu read GetMenu;
       property OnClick:TpvGUIOnEvent read fOnClick write fOnClick;
     end;

     TpvGUIPopupMenu=class(TpvGUIObject)
      private
       fSkin:TpvGUISkin;
       fFont:TpvFont;
       fFontSize:TpvFloat;
       fFontColor:TpvVector4;
       fPosition:TpvVector2;
       fPositionProperty:TpvVector2Property;
       fSize:TpvVector2;
       fHasSubMenus:boolean;
       fReleaseOnDeactivation:boolean;
       fSelectedMenuItem:TpvGUIMenuItem;
       fFocusedMenuItem:TpvGUIMenuItem;
       fHoveredMenuItem:TpvGUIMenuItem;
       function GetActivated:boolean;
      protected
       function GetSkin:TpvGUISkin; virtual;
       procedure SetSkin(const aSkin:TpvGUISkin); virtual;
       function GetFont:TpvFont; virtual;
       function GetFontSize:TpvFloat; virtual;
       function GetFontColor:TpvVector4; virtual;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure Activate(const aPosition:TpvVector2);
       procedure FocusFirstMenuItem;
       procedure Deactivate;
       procedure DeactivateWindowMenu;
       procedure DeactivateSubmenus;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
       procedure Draw(const aCanvas:TpvCanvas);
      public
       property FontColor:TpvVector4 read GetFontColor write fFontColor;
      published
       property Activated:boolean read GetActivated;
       property Skin:TpvGUISkin read GetSkin write SetSkin;
       property Font:TpvFont read GetFont write fFont;
       property FontSize:TpvFloat read GetFontSize write fFontSize;
       property Position:TpvVector2Property read fPositionProperty;
       property ReleaseOnDeactivation:boolean read fReleaseOnDeactivation write fReleaseOnDeactivation;
     end;

     TpvGUIWindowMenu=class(TpvGUIWidget)
      private
       fSelectedMenuItem:TpvGUIMenuItem;
       fFocusedMenuItem:TpvGUIMenuItem;
       fHoveredMenuItem:TpvGUIMenuItem;
       function GetFontSize:TpvFloat; override;
       function GetFontColor:TpvVector4; override;
       function GetPreferredSize:TpvVector2; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function Enter:boolean; override;
       function Leave:boolean; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      published
     end;

     PpvGUIScrollBarOrientation=^TpvGUIScrollBarOrientation;
     TpvGUIScrollBarOrientation=
      (
       pvgsboHorizontal,
       pvgsboVertical
      );

     PpvGUIScrollBarSubWidget=^TpvGUIScrollBarSubWidget;
     TpvGUIScrollBarSubWidget=
      (
       pvgsbswNone,
       pvgsbswDecButton,
       pvgsbswIncButton,
       pvgsbswSliderButton
      );

     TpvGUIScrollBar=class(TpvGUIWidget)
      private
       fOrientation:TpvGUIScrollBarOrientation;
       fMinimumValue:TpvInt64;
       fMaximumValue:TpvInt64;
       fValue:TpvInt64;
       fSmallStep:TpvInt64;
       fLargeStep:TpvInt64;
       fButtonSize:TpvFloat;
       fSliderButtonSize:TpvFloat;
       fSliderPushed:boolean;
       fOnChange:TpvGUIOnEvent;
       fFocusedSubWidget:TpvGUIScrollBarSubWidget;
       fPushedSubWidget:TpvGUIScrollBarSubWidget;
       fStepSize:TpvInt64;
       fTimeAccumulator:TpvDouble;
       fCachedSliderButtonSize:TpvFloat;
       procedure SetOrientation(const aOrientation:TpvGUIScrollBarOrientation);
       procedure SetMinimumValue(const aMinimumValue:TpvInt64);
       procedure SetMaximumValue(const aMaximumValue:TpvInt64);
       procedure SetValue(const aValue:TpvInt64);
       procedure SetButtonSize(const aButtonSize:TpvFloat);
       function GetSliderButtonSize:TpvFloat;
       procedure SetSliderButtonSize(const aSliderButtonSize:TpvFloat);
       function GetPreferredSize:TpvVector2; override;
       function GetSliderButtonRect:TpvRect;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function Enter:boolean; override;
       function Leave:boolean; override;
       function PointerEnter:boolean; override;
       function PointerLeave:boolean; override;
       function DragEvent(const aPosition:TpvVector2):boolean; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure PerformLayout; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Orientation:TpvGUIScrollBarOrientation read fOrientation write SetOrientation;
       property MinimumValue:TpvInt64 read fMinimumValue write SetMinimumValue;
       property MaximumValue:TpvInt64 read fMaximumValue write SetMaximumValue;
       property Value:TpvInt64 read fValue write SetValue;
       property SmallStep:TpvInt64 read fSmallStep write fSmallStep;
       property LargeStep:TpvInt64 read fLargeStep write fLargeStep;
       property ButtonSize:TpvFloat read fButtonSize write SetButtonSize;
       property SliderButtonSize:TpvFloat read GetSliderButtonSize write SetSliderButtonSize;
       property OnChange:TpvGUIOnEvent read fOnChange write fOnChange;
       property FocusedSubWidget:TpvGUIScrollBarSubWidget read fFocusedSubWidget;
       property PushedSubWidget:TpvGUIScrollBarSubWidget read fPushedSubWidget;
     end;

     PpvGUISliderOrientation=^TpvGUISliderOrientation;
     TpvGUISliderOrientation=
      (
       pvgsoHorizontal,
       pvgsoVertical
      );

     PpvGUISliderSubWidget=^TpvGUISliderSubWidget;
     TpvGUISliderSubWidget=
      (
       pvgsswNone,
       pvgsswSliderButton
      );

     TpvGUISlider=class(TpvGUIWidget)
      private
       fOrientation:TpvGUISliderOrientation;
       fMinimumValue:TpvInt64;
       fMaximumValue:TpvInt64;
       fValue:TpvInt64;
       fSmallStep:TpvInt64;
       fLargeStep:TpvInt64;
       fButtonSize:TpvFloat;
       fSliderButtonSize:TpvFloat;
       fSliderPushed:boolean;
       fOnChange:TpvGUIOnEvent;
       fFocusedSubWidget:TpvGUISliderSubWidget;
       fPushedSubWidget:TpvGUISliderSubWidget;
       fStepSize:TpvInt64;
       fTimeAccumulator:TpvDouble;
       procedure SetOrientation(const aOrientation:TpvGUISliderOrientation);
       procedure SetMinimumValue(const aMinimumValue:TpvInt64);
       procedure SetMaximumValue(const aMaximumValue:TpvInt64);
       procedure SetValue(const aValue:TpvInt64);
       procedure SetButtonSize(const aButtonSize:TpvFloat);
       procedure SetSliderButtonSize(const aSliderButtonSize:TpvFloat);
       function GetPreferredSize:TpvVector2; override;
       function GetSliderButtonRect:TpvRect;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       function Enter:boolean; override;
       function Leave:boolean; override;
       function PointerEnter:boolean; override;
       function PointerLeave:boolean; override;
       function DragEvent(const aPosition:TpvVector2):boolean; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Orientation:TpvGUISliderOrientation read fOrientation write SetOrientation;
       property MinimumValue:TpvInt64 read fMinimumValue write SetMinimumValue;
       property MaximumValue:TpvInt64 read fMaximumValue write SetMaximumValue;
       property Value:TpvInt64 read fValue write SetValue;
       property SmallStep:TpvInt64 read fSmallStep write fSmallStep;
       property LargeStep:TpvInt64 read fLargeStep write fLargeStep;
       property ButtonSize:TpvFloat read fButtonSize write SetButtonSize;
       property SliderButtonSize:TpvFloat read fSliderButtonSize write SetSliderButtonSize;
       property OnChange:TpvGUIOnEvent read fOnChange write fOnChange;
       property FocusedSubWidget:TpvGUISliderSubWidget read fFocusedSubWidget;
       property PushedSubWidget:TpvGUISliderSubWidget read fPushedSubWidget;
     end;

     PpvGUIProgressBarOrientation=^TpvGUIProgressBarOrientation;
     TpvGUIProgressBarOrientation=
      (
       pvgpboHorizontal,
       pvgpboVertical
      );

     TpvGUIProgressBar=class(TpvGUIWidget)
      private
       fOrientation:TpvGUIProgressBarOrientation;
       fMinimumValue:TpvInt64;
       fMaximumValue:TpvInt64;
       fValue:TpvInt64;
       fOnChange:TpvGUIOnEvent;
       procedure SetOrientation(const aOrientation:TpvGUIProgressBarOrientation);
       procedure SetMinimumValue(const aMinimumValue:TpvInt64);
       procedure SetMaximumValue(const aMaximumValue:TpvInt64);
       procedure SetValue(const aValue:TpvInt64);
       function GetPreferredSize:TpvVector2; override;
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property Orientation:TpvGUIProgressBarOrientation read fOrientation write SetOrientation;
       property MinimumValue:TpvInt64 read fMinimumValue write SetMinimumValue;
       property MaximumValue:TpvInt64 read fMaximumValue write SetMaximumValue;
       property Value:TpvInt64 read fValue write SetValue;
       property OnChange:TpvGUIOnEvent read fOnChange write fOnChange;
     end;

     PpvGUIScrollPanelScrollDirection=^TpvGUIScrollPanelScrollDirection;
     TpvGUIScrollPanelScrollDirection=
      (
       pvgspsdfOff,
       pvgspsdfOn,
       pvgspsdfAuto
      );

     TpvGUIScrollPanel=class(TpvGUIWidget)
      private
       fHorizontalScrollDirection:TpvGUIScrollPanelScrollDirection;
       fVerticalScrollDirection:TpvGUIScrollPanelScrollDirection;
       fHorizontalScrollBar:TpvGUIScrollBar;
       fVerticalScrollBar:TpvGUIScrollBar;
       fClipContentPanel:TpvGUIPanel;
       fContent:TpvGUIPanel;
       procedure SetHorizontalScrollDirection(const aHorizontalScrollDirection:TpvGUIScrollPanelScrollDirection);
       procedure SetVerticalScrollDirection(const aVerticalScrollDirection:TpvGUIScrollPanelScrollDirection);
       function GetPreferredSize:TpvVector2; override;
       procedure HorizontalScrollBarOnChange(const aSender:TpvGUIObject);
       procedure VerticalScrollBarOnChange(const aSender:TpvGUIObject);
      public
       constructor Create(const aParent:TpvGUIObject); override;
       destructor Destroy; override;
       procedure PerformLayout; override;
       procedure Update; override;
       procedure Draw; override;
      published
       property HorizontalScrollDirection:TpvGUIScrollPanelScrollDirection read fHorizontalScrollDirection write SetHorizontalScrollDirection;
       property VerticalScrollDirection:TpvGUIScrollPanelScrollDirection read fVerticalScrollDirection write SetVerticalScrollDirection;
       property HorizontalScrollBar:TpvGUIScrollBar read fHorizontalScrollBar;
       property VerticalScrollBar:TpvGUIScrollBar read fVerticalScrollBar;
       property Content:TpvGUIPanel read fContent;
     end;

implementation

uses PasDblStrUtils,
     PasVulkan.Assets,
     PasVulkan.VectorPath,
     PasVulkan.Image.PNG;

const GUI_ELEMENT_WINDOW_HEADER=1;
      GUI_ELEMENT_WINDOW_FILL=2;
      GUI_ELEMENT_WINDOW_DROPSHADOW=3;
      GUI_ELEMENT_BUTTON_UNFOCUSED=4;
      GUI_ELEMENT_BUTTON_FOCUSED=5;
      GUI_ELEMENT_BUTTON_PUSHED=6;
      GUI_ELEMENT_BUTTON_DISABLED=7;
      GUI_ELEMENT_FOCUSED=8;
      GUI_ELEMENT_HOVERED=9;
      GUI_ELEMENT_BOX_UNFOCUSED=10;
      GUI_ELEMENT_BOX_FOCUSED=11;
      GUI_ELEMENT_BOX_DISABLED=12;
      GUI_ELEMENT_BOX_DARK_UNFOCUSED=13;
      GUI_ELEMENT_BOX_DARK_FOCUSED=14;
      GUI_ELEMENT_BOX_DARK_DISABLED=15;
      GUI_ELEMENT_PANEL_ENABLED=16;
      GUI_ELEMENT_PANEL_DISABLED=17;
      GUI_ELEMENT_MOUSE_CURSOR_ARROW=64;
      GUI_ELEMENT_MOUSE_CURSOR_BEAM=65;
      GUI_ELEMENT_MOUSE_CURSOR_BUSY=66;
      GUI_ELEMENT_MOUSE_CURSOR_CROSS=67;
      GUI_ELEMENT_MOUSE_CURSOR_EW=68;
      GUI_ELEMENT_MOUSE_CURSOR_HELP=69;
      GUI_ELEMENT_MOUSE_CURSOR_LINK=70;
      GUI_ELEMENT_MOUSE_CURSOR_MOVE=71;
      GUI_ELEMENT_MOUSE_CURSOR_NESW=72;
      GUI_ELEMENT_MOUSE_CURSOR_NS=73;
      GUI_ELEMENT_MOUSE_CURSOR_NWSE=74;
      GUI_ELEMENT_MOUSE_CURSOR_PEN=75;
      GUI_ELEMENT_MOUSE_CURSOR_UNAVAILABLE=76;
      GUI_ELEMENT_MOUSE_CURSOR_UP=77;

class function TpvGUITextUtils.TextTruncation(const aText:TpvUTF8String;
                                              const aTextTruncation:TpvGUITextTruncation;
                                              const aFont:TpvFont;
                                              const aFontSize:TVkFloat;
                                              const aAvailableWidth:TVkFloat):TpvUTF8String;
const Ellipsis:TpvRawByteString=TpvRawByteString(#$e2#$80#$a6);
var ForwardIndex,BackwardIndex,Len:TpvInt32;
    TextWidth:TVkFloat;
    Text,ForwardTemporary,BackwardTemporary,Current:TpvUTF8String;
begin
 if aTextTruncation=pvgttNone then begin
  result:=aText;
 end else begin
  TextWidth:=aFont.TextWidth(aText,aFontSize);
  if TextWidth<=aAvailableWidth then begin
   result:=aText;
  end else begin
   result:=Ellipsis;
   Text:=PUCUUTF8Trim(aText);
   Len:=length(Text);
   case aTextTruncation of
    pvgttHead:begin
     BackwardIndex:=Len+1;
     BackwardTemporary:='';
     repeat
      PUCUUTF8Dec(Text,BackwardIndex);
      if BackwardIndex>=1 then begin
       BackwardTemporary:=PUCUUTF32CharToUTF8(PUCUUTF8CodeUnitGetCharFallback(Text,BackwardIndex))+BackwardTemporary;
       Current:=Ellipsis+PUCUUTF8TrimLeft(BackwardTemporary);
       if aFont.TextWidth(Current,aFontSize)<=aAvailableWidth then begin
        result:=Current;
       end else begin
        break;
       end;
      end else begin
       break;
      end;
     until false;
    end;
    pvgttMiddle:begin
     ForwardIndex:=1;
     BackwardIndex:=Len+1;
     ForwardTemporary:='';
     BackwardTemporary:='';
     repeat
      PUCUUTF8Dec(Text,BackwardIndex);
      if (ForwardIndex<=Len) and (BackwardIndex>=1) then begin
       ForwardTemporary:=ForwardTemporary+PUCUUTF32CharToUTF8(PUCUUTF8CodeUnitGetCharAndIncFallback(Text,ForwardIndex));
       BackwardTemporary:=PUCUUTF32CharToUTF8(PUCUUTF8CodeUnitGetCharFallback(Text,BackwardIndex))+BackwardTemporary;
       Current:=PUCUUTF8TrimRight(ForwardTemporary)+Ellipsis+PUCUUTF8TrimLeft(BackwardTemporary);
       if aFont.TextWidth(Current,aFontSize)<=aAvailableWidth then begin
        result:=Current;
       end else begin
        break;
       end;
      end else begin
       break;
      end;
     until false;
    end;
    pvgttTail:begin
     ForwardIndex:=1;
     ForwardTemporary:='';
     while ForwardIndex<=Len do begin
      ForwardTemporary:=ForwardTemporary+PUCUUTF32CharToUTF8(PUCUUTF8CodeUnitGetCharAndIncFallback(Text,ForwardIndex));
      Current:=PUCUUTF8TrimRight(ForwardTemporary)+Ellipsis;
      if aFont.TextWidth(Current,aFontSize)<=aAvailableWidth then begin
       result:=Current;
      end else begin
       break;
      end;
     end;
    end;
   end;
  end;
 end;
end;

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

function TpvGUIObject.HasParent(const aParent:TpvGUIObject):boolean;
var CurrentParent:TpvGUIObject;
begin
 CurrentParent:=fParent;
 while assigned(CurrentParent) do begin
  if CurrentParent=aParent then begin
   result:=true;
   exit;
  end;
  CurrentParent:=CurrentParent.Parent;
 end;
 result:=false;
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

constructor TpvGUIObjectHolder.Create(const aParent:TpvGUIObject;const aHoldedObject:TObject=nil);
begin
 inherited Create(aParent);
 fHoldedObject:=aHoldedObject;
end;

destructor TpvGUIObjectHolder.Destroy;
begin
 try
  inherited Destroy;
 finally
  FreeAndNil(fHoldedObject);
 end;
end;

function TpvGUILayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
begin
 result:=aWidget.fSize;
end;

procedure TpvGUILayout.PerformLayout(const aWidget:TpvGUIWidget);
begin

end;

constructor TpvGUIRootLayout.Create(const aParent:TpvGUIObject;
                                    const aMargin:TpvFloat=0.0;
                                    const aSpacing:TpvFloat=0.0);
begin
 inherited Create(aParent);
 fMargin:=aMargin;
 fSpacing:=aSpacing;
end;

destructor TpvGUIRootLayout.Destroy;
begin
 inherited Destroy;
end;

function TpvGUIRootLayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
const Axis0=1;
      Axis1=0;
var ChildIndex:TpvInt32;
    YOffset:TpvFloat;
    Size,ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
    First:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 Size:=TpvVector2.Create(fMargin*2.0,fMargin*2.0);
 YOffset:=0;
 if aWidget is TpvGUIInstance then begin
  if assigned((aWidget as TpvGUIInstance).fMenu) then begin
   Size.y:=Size.y+aWidget.Skin.WindowMenuHeight;
  end;
 end else if aWidget is TpvGUIWindow then begin
  if pvgwfHeader in (aWidget as TpvGUIWindow).fWindowFlags then begin
   Size.y:=Size.y+(aWidget.Skin.WindowHeaderHeight-(fMargin*0.5));
  end;
  if assigned((aWidget as TpvGUIWindow).fMenu) then begin
   Size.y:=Size.y+aWidget.Skin.WindowMenuHeight;
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
    ChildFixedSize:=ChildWidget.GetFixedSize;
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

procedure TpvGUIRootLayout.PerformLayout(const aWidget:TpvGUIWidget);
var ChildIndex:TpvInt32;
    Offset,YOffset:TpvFloat;
    FixedSize,ContainerSize,ChildPreferredSize,ChildFixedSize,ChildTargetSize,
    Position:TpvVector2;
    IsInstance,First:boolean;
    Child:TpvGUIObject;
    ChildWidget,LastVisibleChildWidget:TpvGUIWidget;
begin
 FixedSize:=aWidget.GetFixedSize;
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
 Offset:=fMargin;
 YOffset:=0;
 IsInstance:=aWidget is TpvGUIInstance;
 if IsInstance then begin
  if assigned((aWidget as TpvGUIInstance).fMenu) then begin
   Offset:=Offset+aWidget.Skin.WindowMenuHeight;
  end;
 end else if aWidget is TpvGUIWindow then begin
  if pvgwfHeader in (aWidget as TpvGUIWindow).fWindowFlags then begin
   Offset:=Offset+(aWidget.Skin.WindowHeaderHeight-(fMargin*0.5));
  end;
  if assigned((aWidget as TpvGUIWindow).fMenu) then begin
   Offset:=Offset+aWidget.Skin.WindowMenuHeight;
  end;
 end;
 ContainerSize.y:=ContainerSize.y-YOffset;
 LastVisibleChildWidget:=nil;
 First:=true;
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildPreferredSize:=ChildWidget.PreferredSize;
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    if IsInstance and (ChildWidget is TpvGUIWindow) then begin
     ChildWidget.fSize:=ChildTargetSize;
    end else begin
     if not First then begin
      Offset:=Offset+fSpacing;
     end;
     Position:=TpvVector2.Create(0,YOffset);
     Position.x:=Position.x+fMargin;
     Position.y:=Offset;
     if ChildFixedSize.y>0.0 then begin
      ChildTargetSize.x:=ChildFixedSize.x;
     end else begin
      ChildTargetSize.x:=ContainerSize.x-(fMargin*2.0);
     end;
     if not ((ChildWidget is TpvGUIWindow) and ((ChildWidget as TpvGUIWindow).WindowState=pvgwsMaximized)) then begin
      ChildWidget.fPosition:=Position;
     end;
     ChildWidget.fSize:=ChildTargetSize;
     Offset:=Offset+ChildTargetSize.y;
     First:=false;
     LastVisibleChildWidget:=ChildWidget;
    end;
   end;
  end;
 end;
 if assigned(LastVisibleChildWidget) then begin
  LastVisibleChildWidget.Height:=ContainerSize.y-(LastVisibleChildWidget.Top+(fMargin*2.0));
 end;
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildWidget.PerformLayout;
   end;
  end;
 end;
end;

constructor TpvGUIFillLayout.Create(const aParent:TpvGUIObject;
                                    const aMargin:TpvFloat=0.0);
begin
 inherited Create(aParent);
 fMargin:=aMargin;
end;

destructor TpvGUIFillLayout.Destroy;
begin
 inherited Destroy;
end;

function TpvGUIFillLayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
var ChildIndex:TpvInt32;
    Size,ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 Size:=TpvVector2.Null;
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildPreferredSize:=ChildWidget.PreferredSize;
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    Size:=Maximum(Size,ChildTargetSize);
   end;
  end;
 end;
 result:=Size+TpvVector2.Create(fMargin*2.0,fMargin*2.0);
end;

procedure TpvGUIFillLayout.PerformLayout(const aWidget:TpvGUIWidget);
var ChildIndex:TpvInt32;
    FixedSize,ContainerSize,ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
    First:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 FixedSize:=aWidget.GetFixedSize;
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
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildPreferredSize:=ChildWidget.PreferredSize;
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    if not ((ChildWidget is TpvGUIWindow) and ((ChildWidget as TpvGUIWindow).WindowState=pvgwsMaximized)) then begin
     ChildWidget.fPosition:=TpvVector2.Create(fMargin,fMargin);
    end;
    ChildWidget.fSize:=ContainerSize-(TpvVector2.Create(fMargin,fMargin)*2.0);
    ChildWidget.PerformLayout;
   end;
  end;
 end;
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
    Size,ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
    First:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 Size:=TpvVector2.Create(fMargin*2.0,fMargin*2.0);
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
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
 result:=Size;
end;

procedure TpvGUIBoxLayout.PerformLayout(const aWidget:TpvGUIWidget);
var Axis0,Axis1,ChildIndex:TpvInt32;
    Offset:TpvFloat;
    FixedSize,ContainerSize,ChildPreferredSize,ChildFixedSize,ChildTargetSize,
    Position:TpvVector2;
    First:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 FixedSize:=aWidget.GetFixedSize;
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
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    Position:=TpvVector2.Null;
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
    if not ((ChildWidget is TpvGUIWindow) and ((ChildWidget as TpvGUIWindow).WindowState=pvgwsMaximized)) then begin
     ChildWidget.fPosition:=Position;
    end;
    ChildWidget.fSize:=ChildTargetSize;
    ChildWidget.PerformLayout;
    Offset:=Offset+ChildTargetSize[Axis0];
    First:=false;
   end;
  end;
 end;
end;

constructor TpvGUIGroupLayout.Create(const aParent:TpvGUIObject;
                                     const aMargin:TpvFloat=15.0;
                                     const aSpacing:TpvFloat=6.0;
                                     const aGroupSpacing:TpvFloat=14.0;
                                     const aGroupIndent:TpvFloat=20.0);
begin

 inherited Create(aParent);

 fMargin:=aMargin;

 fSpacing:=aSpacing;

 fGroupSpacing:=aGroupSpacing;

 fGroupIdent:=aGroupIndent;

end;

destructor TpvGUIGroupLayout.Destroy;
begin
 inherited Destroy;
end;

function TpvGUIGroupLayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
var ChildIndex:TpvInt32;
    ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
    First,Indent,IndentCurrent:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildLabel:TpVGUILabel;
begin
 result:=TpvVector2.Create(fMargin*2.0,fMargin);
 First:=true;
 Indent:=false;
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    if ChildWidget is TpvGUILabel then begin
     ChildLabel:=ChildWidget as TpvGUILabel;
    end else begin
     ChildLabel:=nil;
    end;
    if First then begin
     First:=false;
    end else begin
     if assigned(ChildLabel) then begin
      result.y:=result.y+fGroupSpacing;
     end else begin
      result.y:=result.y+fSpacing;
     end;
    end;
    ChildPreferredSize:=ChildWidget.GetPreferredSize;
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    IndentCurrent:=Indent and not assigned(ChildLabel);
    result.x:=Max(result.x,ChildTargetSize.x+(2.0*fMargin)+((ord(IndentCurrent) and 1)*fGroupIdent));
    result.y:=result.y+ChildTargetSize.y;
    if assigned(ChildLabel) then begin
     Indent:=length(ChildLabel.Caption)>0;
    end
   end;
  end;
 end;
 result.y:=result.y+fMargin;
end;

procedure TpvGUIGroupLayout.PerformLayout(const aWidget:TpvGUIWidget);
var ChildIndex:TpvInt32;
    Size,ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
    AvailableWidth:TpvFloat;
    First,Indent,IndentCurrent:boolean;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildLabel:TpVGUILabel;
begin
 Size:=TpvVector2.Create(fMargin*2.0,fMargin);
 if aWidget.GetFixedWidth>0.0 then begin
  AvailableWidth:=aWidget.GetFixedWidth-(fMargin*2.0);
 end else begin
  AvailableWidth:=aWidget.Width-(fMargin*2.0);
 end;
 First:=true;
 Indent:=false;
 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    if ChildWidget is TpvGUILabel then begin
     ChildLabel:=ChildWidget as TpvGUILabel;
    end else begin
     ChildLabel:=nil;
    end;
    if First then begin
     First:=false;
    end else begin
     if assigned(ChildLabel) then begin
      Size.y:=Size.y+fGroupSpacing;
     end else begin
      Size.y:=Size.y+fSpacing;
     end;
    end;
    IndentCurrent:=Indent and not assigned(ChildLabel);
    ChildPreferredSize:=TpvVector2.Create(AvailableWidth-((ord(IndentCurrent) and 1)*fGroupIdent),ChildWidget.GetPreferredSize.y);
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    ChildWidget.fPosition:=TpvVector2.Create(fMargin+((ord(IndentCurrent) and 1)*fGroupIdent),Size.y);
    ChildWidget.fSize:=ChildTargetSize;
    ChildWidget.PerformLayout;
    Size.y:=Size.y+ChildTargetSize.y;
    if assigned(ChildLabel) then begin
     Indent:=length(ChildLabel.Caption)>0;
    end
   end;
  end;
 end;
 Size.y:=Size.y+fMargin;
end;

procedure TpvGUIGridLayoutAlignments.SetAlignments(const aAlignments:array of TpvGUILayoutAlignment);
var Index:TpvSizeInt;
begin
 Count:=length(aAlignments);
 for Index:=0 to length(aAlignments)-1 do begin
  Items[Index]:=aAlignments[Index];
 end;
end;

constructor TpvGUIGridLayout.Create(const aParent:TpvGUIObject;
                                    const aResolution:TpvInt32=2;
                                    const aColumnAlignment:TpvGUILayoutAlignment=pvglaMiddle;
                                    const aRowAlignment:TpvGUILayoutAlignment=pvglaMiddle;
                                    const aOrientation:TpvGUILayoutOrientation=pvgloHorizontal;
                                    const aMargin:TpvFloat=0.0;
                                    const aHorizontalSpacing:TpvFloat=0.0;
                                    const aVerticalSpacing:TpvFloat=0.0);
begin

 inherited Create(aParent);

 fFlags:=[pvgglfStretchHorizontal,pvgglfStretchVertical];

 fSpacingProperty:=TpvVector2Property.Create(@fSpacing);

 SetColumnAlignment(aColumnAlignment);

 SetRowAlignment(aRowAlignment);

 fOrientation:=aOrientation;

 SetResolution(aResolution);

 fMargin:=aMargin;

 fSpacing:=TpvVector2.Create(aHorizontalSpacing,aVerticalSpacing);

 fAlignments[0]:=TpvGUIGridLayoutAlignments.Create;
 fAlignments[1]:=TpvGUIGridLayoutAlignments.Create;

end;

destructor TpvGUIGridLayout.Destroy;
begin

 FreeAndNil(fAlignments[0]);
 FreeAndNil(fAlignments[1]);

 FreeAndNil(fSpacingProperty);

 inherited Destroy;

end;

function TpvGUIGridLayout.GetColumnAlignments:TpvGUIGridLayoutAlignments;
begin
 result:=fAlignments[0];
end;

function TpvGUIGridLayout.GetRowAlignments:TpvGUIGridLayoutAlignments;
begin
 result:=fAlignments[1];
end;

function TpvGUIGridLayout.GetColumnAlignment:TpvGUILayoutAlignment;
begin
 result:=fDefaultAlignments[0];
end;

procedure TpvGUIGridLayout.SetColumnAlignment(const aAlignment:TpvGUILayoutAlignment);
begin
 fDefaultAlignments[0]:=aAlignment;
end;

function TpvGUIGridLayout.GetRowAlignment:TpvGUILayoutAlignment;
begin
 result:=fDefaultAlignments[1];
end;

procedure TpvGUIGridLayout.SetRowAlignment(const aAlignment:TpvGUILayoutAlignment);
begin
 fDefaultAlignments[1]:=aAlignment;
end;

procedure TpvGUIGridLayout.SetResolution(const aResolution:TpvInt32);
begin
 fResolution:=aResolution;
end;

function TpvGUIGridLayout.GetAlignment(const aAxisIndex,aItemIndex:TpvInt32):TpvGUILayoutAlignment;
var AxisIndex:TpvInt32;
begin
 AxisIndex:=aAxisIndex and 1;
 if (aItemIndex>=0) and (aItemIndex<fAlignments[AxisIndex].Count) then begin
  result:=fAlignments[AxisIndex][aItemIndex];
 end else begin
  result:=fDefaultAlignments[AxisIndex];
 end;
end;

procedure TpvGUIGridLayout.ComputeLayout(const aWidget:TpvGUIWidget);
var Axis0,Axis1,VisibleChildren,ChildIndex,i0,i1:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
begin

 Axis0:=TpvGUIGridLayout.AxisOrientationAxes[fOrientation,0];
 Axis1:=TpvGUIGridLayout.AxisOrientationAxes[fOrientation,1];

 VisibleChildren:=0;

 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    inc(VisibleChildren);
   end;
  end;
 end;

 fGridDimensions[Axis0]:=fResolution;
 fGridDimensions[Axis1]:=(VisibleChildren+(fResolution-1)) div fResolution;

 i0:=RoundUpToPowerOfTwo(fGridDimensions[Axis0]);
 if length(fGrid[Axis0])<>i0 then begin
  SetLength(fGrid[Axis0],i0);
 end;

 i1:=RoundUpToPowerOfTwo(fGridDimensions[Axis1]);
 if length(fGrid[Axis1])<>i1 then begin
  SetLength(fGrid[Axis1],i1);
 end;

 for i0:=0 to fGridDimensions[Axis0]-1 do begin
  fGrid[Axis0,i0]:=0.0;
 end;

 for i1:=0 to fGridDimensions[Axis1]-1 do begin
  fGrid[Axis1,i1]:=0.0;
 end;

 ChildIndex:=0;

 for i1:=0 to fGridDimensions[Axis1]-1 do begin

  ChildWidget:=nil;

  for i0:=0 to fGridDimensions[Axis0]-1 do begin

   repeat
    Child:=nil;
    ChildWidget:=nil;
    if ChildIndex>=aWidget.fChildren.Count then begin
     break;
    end;
    Child:=aWidget.fChildren.Items[ChildIndex];
    inc(ChildIndex);
    if Child is TpvGUIWidget then begin
     ChildWidget:=Child as TpvGUIWidget;
     if ChildWidget.Visible then begin
      break;
     end;
    end;
   until false;

   if assigned(ChildWidget) then begin

    ChildPreferredSize:=ChildWidget.GetPreferredSize;

    ChildFixedSize:=ChildWidget.GetFixedSize;

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

    fGrid[Axis0,i0]:=Max(fGrid[Axis0,i0],ChildTargetSize[Axis0]);
    fGrid[Axis1,i1]:=Max(fGrid[Axis1,i1],ChildTargetSize[Axis1]);

   end else begin
    break;
   end;

  end;

  if not assigned(ChildWidget) then begin
   break;
  end;

 end;

end;

function TpvGUIGridLayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
var Index:TpvInt32;
begin
 ComputeLayout(aWidget);
 result:=TpvVector2.Create((fMargin*2.0)+(Max(fGridDimensions[0]-1,0)*fSpacing.x),
                           (fMargin*2.0)+(Max(fGridDimensions[1]-1,0)*fSpacing.y));
 for Index:=0 to fGridDimensions[0]-1 do begin
  result.x:=result.x+fGrid[0,Index];
 end;
 for Index:=0 to fGridDimensions[1]-1 do begin
  result.y:=result.y+fGrid[1,Index];
 end;
end;

procedure TpvGUIGridLayout.PerformLayout(const aWidget:TpvGUIWidget);
var Index0,Index1,Index2,Axis0,Axis1,ChildIndex,
    AxisIndex,ItemIndex:TpvInt32;
    FixedSize,ContainerSize,GridSize,Start,Position,
    ChildPreferredSize,ChildFixedSize,ChildTargetSize,ChildPosition:TpvVector2;
    Gap,SpreadedGap:TpvFloat;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    Alignment:TpvGUILayoutAlignment;
begin

 FixedSize:=aWidget.GetFixedSize;
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

 ComputeLayout(aWidget);

 for AxisIndex:=0 to 1 do begin
  if TpvGUIGridLayout.AxisStretchFlags[AxisIndex] in fFlags then begin
   GridSize[AxisIndex]:=2.0*fMargin;
   for ItemIndex:=0 to fGridDimensions[AxisIndex]-1 do begin
    GridSize[AxisIndex]:=GridSize[AxisIndex]+fGrid[AxisIndex,ItemIndex];
   end;
   GridSize[AxisIndex]:=GridSize[AxisIndex]+(Max(fGridDimensions[AxisIndex]-1,0)*fSpacing[AxisIndex]);
   if (fGridDimensions[AxisIndex]>0) and (GridSize[AxisIndex]<ContainerSize[AxisIndex]) then begin
    Gap:=ContainerSize[AxisIndex]-GridSize[AxisIndex];
    SpreadedGap:=Gap/fGridDimensions[AxisIndex];
    for ItemIndex:=0 to fGridDimensions[AxisIndex]-1 do begin
     fGrid[AxisIndex,ItemIndex]:=fGrid[AxisIndex,ItemIndex]+SpreadedGap;
    end;
   end;
  end;
 end;

 Axis0:=TpvGUIGridLayout.AxisOrientationAxes[fOrientation,0];
 Axis1:=TpvGUIGridLayout.AxisOrientationAxes[fOrientation,1];

 Start:=TpvVector2.Create(fMargin,fMargin);

 Position:=Start;

 ChildIndex:=0;

 for Index1:=0 to fGridDimensions[Axis1]-1 do begin

  Position[Axis0]:=Start[Axis0];

  ChildWidget:=nil;

  for Index0:=0 to fGridDimensions[Axis0]-1 do begin

   repeat
    Child:=nil;
    ChildWidget:=nil;
    if ChildIndex>=aWidget.fChildren.Count then begin
     break;
    end;
    Child:=aWidget.fChildren.Items[ChildIndex];
    inc(ChildIndex);
    if Child is TpvGUIWidget then begin
     ChildWidget:=Child as TpvGUIWidget;
     if ChildWidget.Visible then begin
      break;
     end;
    end;
   until false;

   if assigned(ChildWidget) then begin

    ChildPreferredSize:=ChildWidget.GetPreferredSize;

    ChildFixedSize:=ChildWidget.GetFixedSize;

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

    ChildPosition:=Position;

    for Index2:=0 to 1 do begin

     AxisIndex:=(Axis0+Index2) and 1;

     if AxisIndex=0 then begin
      ItemIndex:=Index0;
     end else begin
      ItemIndex:=Index1;
     end;

     Alignment:=GetAlignment(AxisIndex,ItemIndex);

     case Alignment of
      pvglaLeading:begin
      end;
      pvglaMiddle:begin
       ChildPosition[AxisIndex]:=ChildPosition[AxisIndex]+((fGrid[AxisIndex,ItemIndex]-ChildTargetSize[AxisIndex])*0.5);
      end;
      pvglaTailing:begin
       ChildPosition[AxisIndex]:=ChildPosition[AxisIndex]+(fGrid[AxisIndex,ItemIndex]-ChildTargetSize[AxisIndex]);
      end;
      else {pvglaFill:}begin
       if ChildFixedSize[AxisIndex]>0.0 then begin
        ChildTargetSize[AxisIndex]:=ChildFixedSize[AxisIndex];
       end else begin
        ChildTargetSize[AxisIndex]:=fGrid[AxisIndex,ItemIndex];
       end;
      end;
     end;

    end;

    ChildWidget.fPosition:=ChildPosition;

    ChildWidget.fSize:=ChildTargetSize;

    ChildWidget.PerformLayout;

    Position[Axis0]:=Position[Axis0]+(fGrid[Axis0,Index0]+fSpacing[Axis0]);

   end else begin

    break;

   end;

  end;

  if assigned(ChildWidget) then begin
   Position[Axis1]:=Position[Axis1]+(fGrid[Axis1,Index1]+fSpacing[Axis1]);
  end else begin
   break;
  end;

 end;

end;

class function TpvGUIAdvancedGridLayoutAnchor.CreateNull:TpvGUIAdvancedGridLayoutAnchor;
begin
{$if false}
 result.Position.x:=0;
 result.Position.y:=0;
 result.Size.x:=0;
 result.Size.y:=0;
 result.Alignment.x:=pvglaLeading;
 result.Alignment.y:=pvglaLeading;
 result.Padding:=0.0;
{$else}
 FillChar(result,SizeOf(TpvGUIAdvancedGridLayoutAnchor),#0);
{$ifend}
end;

constructor TpvGUIAdvancedGridLayoutAnchor.Create(const aX,aY:TpvUInt8;const aWidth,aHeight:TpvUInt8;const aPaddingLeft:TpvFloat=0.0;const aPaddingTop:TpvFloat=0.0;const aPaddingRight:TpvFloat=0.0;const aPaddingBottom:TpvFloat=0.0;const aHorizontalAlignment:TpvGUILayoutAlignment=pvglaFill;const aVerticalAlignment:TpvGUILayoutAlignment=pvglaFill);
begin
 Position.x:=aX;
 Position.y:=aY;
 Size.x:=aWidth;
 Size.y:=aHeight;
 Alignment.x:=aHorizontalAlignment;
 Alignment.y:=aVerticalAlignment;
 Padding[0,0]:=aPaddingLeft;
 Padding[0,1]:=aPaddingRight;
 Padding[1,0]:=aPaddingTop;
 Padding[1,1]:=aPaddingBottom;
end;

constructor TpvGUIAdvancedGridLayoutAnchor.Create(const aX,aY:TpvUInt8;const aHorizontalAlignment,aVerticalAlignment:TpvGUILayoutAlignment);
begin
 Position.x:=aX;
 Position.y:=aY;
 Size.x:=1;
 Size.y:=1;
 Alignment.x:=aHorizontalAlignment;
 Alignment.y:=aVerticalAlignment;
 Padding[0,0]:=0.0;
 Padding[0,1]:=0.0;
 Padding[1,0]:=0.0;
 Padding[1,1]:=0.0;
end;

constructor TpvGUIAdvancedGridLayoutAnchor.Create(const aX,aY:TpvUInt8);
begin
 Position.x:=aX;
 Position.y:=aY;
 Size.x:=1;
 Size.y:=1;
 Alignment.x:=pvglaFill;
 Alignment.y:=pvglaFill;
 Padding[0,0]:=0.0;
 Padding[0,1]:=0.0;
 Padding[1,0]:=0.0;
 Padding[1,1]:=0.0;
end;

constructor TpvGUIAdvancedGridLayoutColumnRow.Create(const aSize:TpvFloat;const aStretch:TpvFloat=0.0);
begin
 inherited Create;
 fSize:=aSize;
 fStretch:=aStretch;
end;

function TpvGUIAdvancedGridLayoutColumnRows.Add(const aSize:TpvFloat;const aStretch:TpvFloat=0.0):TpvSizeInt;
begin
 result:=inherited Add(TpvGUIAdvancedGridLayoutColumnRow.Create(aSize,aStretch));
end;

constructor TpvGUIAdvancedGridLayout.Create(const aParent:TpvGUIObject;const aMargin:TpvFloat);
begin

 inherited Create(aParent);

 fAnchors:=TpvGUIAdvancedGridLayoutAnchors.Create(TpvGUIAdvancedGridLayoutAnchor.CreateNull);

 fRows:=TpvGUIAdvancedGridLayoutColumnRows.Create;
 fRows.OwnObjects:=true;

 fColumns:=TpvGUIAdvancedGridLayoutColumnRows.Create;
 fColumns.OwnObjects:=true;

 fMargin:=aMargin;

 fPositions:=nil;

 fSizes:=nil;

 fFixedSizes:=nil;

 fTargetSizes:=nil;

end;

destructor TpvGUIAdvancedGridLayout.Destroy;
begin

 fPositions:=nil;

 fSizes:=nil;

 fFixedSizes:=nil;

 fTargetSizes:=nil;

 FreeAndNil(fAnchors);

 FreeAndNil(fRows);

 FreeAndNil(fColumns);

 inherited Destroy;

end;

procedure TpvGUIAdvancedGridLayout.ComputeLayout(const aWidget:TpvGUIWidget);
var AxisIndex,PhaseIndex,ChildIndex,Index:TpvInt32;
    FixedSize,ContainerSize:TpvVector2;
    ColumnRows:TpvGUIAdvancedGridLayoutColumnRows;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    Anchor:TpvGUIAdvancedGridLayoutAnchor;
    AnchorEntity:TpvGUIAdvancedGridLayoutAnchors.PpvHashMapEntity;
    ChildPreferredSize,ChildFixedSize,ChildTargetSize,
    CurrentSize,TotalStretch,Factor:TpvFloat;
begin

 FixedSize:=aWidget.GetFixedSize;

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

 ContainerSize:=ContainerSize-TpvVector2.Create(fMargin*2.0,fMargin*2.0);

 for AxisIndex:=0 to 1 do begin

  if AxisIndex=0 then begin
   ColumnRows:=fColumns;
  end else begin
   ColumnRows:=fRows;
  end;

  fGridDimensions[AxisIndex]:=ColumnRows.Count;

  SetLength(fGrid[AxisIndex],fGridDimensions[AxisIndex]);

  for Index:=0 to fGridDimensions[AxisIndex]-1 do begin
   fGrid[AxisIndex,Index]:=ColumnRows[Index].fSize;
  end;

  for PhaseIndex:=0 to 1 do begin

   for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
    Child:=aWidget.fChildren.Items[ChildIndex];
    if Child is TpvGUIWidget then begin
     ChildWidget:=Child as TpvGUIWidget;
     if ChildWidget.Visible then begin
      AnchorEntity:=fAnchors.Get(ChildWidget,false);
      if assigned(AnchorEntity) then begin
       Anchor:=AnchorEntity^.Value;
       if (Anchor.Size.Axis[AxisIndex]=1)=(PhaseIndex=0) then begin
        ChildPreferredSize:=ChildWidget.GetPreferredSize[AxisIndex];
        ChildFixedSize:=ChildWidget.GetFixedSize[AxisIndex];
        if ChildFixedSize>0.0 then begin
         ChildTargetSize:=ChildFixedSize;
        end else begin
         ChildTargetSize:=ChildPreferredSize;
        end;
        ChildTargetSize:=ChildTargetSize+(Anchor.Padding[AxisIndex,0]+Anchor.Padding[AxisIndex,1]);
        if (Anchor.Position.Axis[AxisIndex]+Anchor.Size.Axis[AxisIndex])>ColumnRows.Count then begin
         raise EpvGUIAdvancedGridLayout.Create('A widget is out of bounds');
        end;
        CurrentSize:=0.0;
        TotalStretch:=0.0;
        for Index:=Anchor.Position.Axis[AxisIndex] to (Anchor.Position.Axis[AxisIndex]+Anchor.Size.Axis[AxisIndex])-1 do begin
         if SameValue(ColumnRows[Index].Size,0.0) and (Anchor.Size.Axis[AxisIndex]=1) then begin
          fGrid[AxisIndex,Index]:=Max(fGrid[AxisIndex,Index],ChildTargetSize);
         end;
         CurrentSize:=CurrentSize+fGrid[AxisIndex,Index];
         TotalStretch:=TotalStretch+ColumnRows[Index].fStretch;
        end;
        if ChildTargetSize>CurrentSize then begin
         if SameValue(TotalStretch,0.0) then begin
          raise EpvGUIAdvancedGridLayout.Create('No space left to place widget');
         end;
         Factor:=(ChildTargetSize-CurrentSize)/TotalStretch;
         for Index:=Anchor.Position.Axis[AxisIndex] to (Anchor.Position.Axis[AxisIndex]+Anchor.Size.Axis[AxisIndex])-1 do begin
          fGrid[AxisIndex,Index]:=fGrid[AxisIndex,Index]+(ColumnRows[Index].fStretch*Factor);
         end;
        end;
       end;
      end else begin
       raise EpvGUIAdvancedGridLayout.Create('Unregistered widget');
      end;
     end;
    end;
   end;

  end;

  CurrentSize:=0.0;
  TotalStretch:=0.0;
  for Index:=0 to ColumnRows.Count-1 do begin
   CurrentSize:=CurrentSize+fGrid[AxisIndex,Index];
   TotalStretch:=TotalStretch+ColumnRows[Index].fStretch;
  end;

  if (CurrentSize<ContainerSize[AxisIndex]) and (TotalStretch>0.0) then begin

   Factor:=(ContainerSize[AxisIndex]-CurrentSize)/TotalStretch;
   for Index:=0 to ColumnRows.Count-1 do begin
    fGrid[AxisIndex,Index]:=fGrid[AxisIndex,Index]+(ColumnRows[Index].fStretch*Factor);
   end;

  end;

 end;

end;

function TpvGUIAdvancedGridLayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
var AxisIndex,Index:TpvInt32;
begin
 ComputeLayout(aWidget);
 result:=TpvVector2.Create(fMargin*2.0,fMargin*2.0);
 for AxisIndex:=0 to 1 do begin
  for Index:=0 to fGridDimensions[AxisIndex]-1 do begin
   result[AxisIndex]:=result[AxisIndex]+fGrid[AxisIndex,Index];
  end;
 end;
end;

procedure TpvGUIAdvancedGridLayout.PerformLayout(const aWidget:TpvGUIWidget);
 function GetGrid(const aAxisIndex,aItemIndex:TpvInt32):TpvFloat;
 begin
  if aItemIndex>0 then begin
   result:=fGrid[aAxisIndex,aItemIndex-1];
  end else begin
   result:=fMargin;
  end;
 end;
var AxisIndex,Index,ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    Anchor:TpvGUIAdvancedGridLayoutAnchor;
    AnchorEntity:TpvGUIAdvancedGridLayoutAnchors.PpvHashMapEntity;
    CurrentSize,TotalStretch,Factor,ChildPosition,CellSize,
    TargetSize:TpvFloat;
    ChildPreferredSize,ChildFixedSize,ChildTargetSize:TpvVector2;
begin

 ComputeLayout(aWidget);

 if length(fPositions)<>aWidget.fChildren.Count then begin
  SetLength(fPositions,aWidget.fChildren.Count);
 end;

 if length(fSizes)<>aWidget.fChildren.Count then begin
  SetLength(fSizes,aWidget.fChildren.Count);
 end;

 if length(fFixedSizes)<>aWidget.fChildren.Count then begin
  SetLength(fFixedSizes,aWidget.fChildren.Count);
 end;

 if length(fTargetSizes)<>aWidget.fChildren.Count then begin
  SetLength(fTargetSizes,aWidget.fChildren.Count);
 end;

 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildPreferredSize:=ChildWidget.GetPreferredSize;
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    fFixedSizes[ChildIndex]:=ChildFixedSize;
    fTargetSizes[ChildIndex]:=ChildTargetSize;
   end;
  end;
 end;

 for AxisIndex:=0 to 1 do begin

  if fGridDimensions[AxisIndex]>0 then begin
   fGrid[AxisIndex,0]:=fGrid[AxisIndex,0]+fMargin;
   for Index:=1 to fGridDimensions[AxisIndex]-1 do begin
    fGrid[AxisIndex,Index]:=fGrid[AxisIndex,Index]+fGrid[AxisIndex,Index-1];
   end;
  end;

  for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
   Child:=aWidget.fChildren.Items[ChildIndex];
   if Child is TpvGUIWidget then begin
    ChildWidget:=Child as TpvGUIWidget;
    if ChildWidget.Visible then begin
     AnchorEntity:=fAnchors.Get(ChildWidget,false);
     if assigned(AnchorEntity) then begin
      Anchor:=AnchorEntity^.Value;
      ChildPosition:=GetGrid(AxisIndex,Anchor.Position.Axis[AxisIndex])+Anchor.Padding[AxisIndex,0];
      CellSize:=(GetGrid(AxisIndex,Anchor.Position.Axis[AxisIndex]+Anchor.Size.Axis[AxisIndex])-Anchor.Padding[AxisIndex,1])-ChildPosition;
      TargetSize:=fTargetSizes[ChildIndex][AxisIndex];
      case Anchor.Alignment.Axis[AxisIndex] of
       pvglaLeading:begin
       end;
       pvglaMiddle:begin
        ChildPosition:=ChildPosition+((ChildPosition-TargetSize)*0.5);
       end;
       pvglaTailing:begin
        ChildPosition:=ChildPosition+(ChildPosition-TargetSize);
       end;
       pvglaFill:begin
        if fFixedSizes[ChildIndex][AxisIndex]>0.0 then begin
         TargetSize:=fFixedSizes[ChildIndex][AxisIndex];
        end else begin
         TargetSize:=CellSize;
        end;
       end;
      end;
      fPositions[ChildIndex][AxisIndex]:=ChildPosition;
      fSizes[ChildIndex][AxisIndex]:=TargetSize;
     end else begin
      raise EpvGUIAdvancedGridLayout.Create('Unregistered widget');
     end;
    end;
   end;
  end;

 end;

 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildWidget.fPosition:=fPositions[ChildIndex];
    ChildWidget.fSize:=fSizes[ChildIndex];
    ChildWidget.PerformLayout;
   end;
  end;
 end;

end;

constructor TpvGUIFlowLayout.Create(const aParent:TpvGUIObject;
                                    const aOrientation:TpvGUILayoutOrientation=pvgloHorizontal;
                                    const aMargin:TpvFloat=0.0;
                                    const aDesignedWidth:TpvFloat=0.0;
                                    const aDesignedHeight:TpvFloat=0.0;
                                    const aHorizontalSpacing:TpvFloat=4.0;
                                    const aVerticalSpacing:TpvFloat=4.0;
                                    const aDirection:TpvGUIFlowLayoutDirection=pgfldLeftToRight;
                                    const aHorizontalAlignment:TpvGUIFlowLayoutAlignment=pgflaLeading;
                                    const aVerticalAlignment:TpvGUIFlowLayoutAlignment=pgflaLeading;
                                    const aAlignmentOnBaseLine:boolean=false);
begin

 inherited Create(aParent);

 fDesignedSizeProperty:=TpvVector2Property.Create(@fDesignedSize);

 fSpacingProperty:=TpvVector2Property.Create(@fSpacing);

 fOrientation:=aOrientation;

 fMargin:=aMargin;

 fDesignedSize:=TpvVector2.Create(aDesignedWidth,aDesignedHeight);

 fSpacing:=TpvVector2.Create(aHorizontalSpacing,aVerticalSpacing);

 fDirection:=aDirection;

 fAlignments[0]:=aHorizontalAlignment;

 fAlignments[1]:=aVerticalAlignment;

 fAlignmentOnBaseLine:=aAlignmentOnBaseLine;

 fPositions:=nil;

 fSizes:=nil;

end;

destructor TpvGUIFlowLayout.Destroy;
begin

 fPositions:=nil;

 fSizes:=nil;

 FreeAndNil(fSpacingProperty);

 FreeAndNil(fDesignedSizeProperty);

 inherited Destroy;

end;

function TpvGUIFlowLayout.GetHorizontalAlignment:TpvGUIFlowLayoutAlignment;
begin
 result:=fAlignments[0];
end;

procedure TpvGUIFlowLayout.SetHorizontalAlignment(const aHorizontalAlignment:TpvGUIFlowLayoutAlignment);
begin
 fAlignments[0]:=aHorizontalAlignment;
end;

function TpvGUIFlowLayout.GetVerticalAlignment:TpvGUIFlowLayoutAlignment;
begin
 result:=fAlignments[1];
end;

procedure TpvGUIFlowLayout.SetVerticalAlignment(const aVerticalAlignment:TpvGUIFlowLayoutAlignment);
begin
 fAlignments[1]:=aVerticalAlignment;
end;

function TpvGUIFlowLayout.GetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
var Axis0,Axis1,ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    First:boolean;
    FixedSize,ContainerSize,ChildPreferredSize,ChildFixedSize,ChildTargetSize,
    Position:TpvVector2;
    MaxAxis1:TpvFloat;
begin

 Axis0:=AxisOrientationAxes[fOrientation,0];
 Axis1:=AxisOrientationAxes[fOrientation,1];

 FixedSize:=aWidget.GetFixedSize;

 if FixedSize.x>0.0 then begin
  ContainerSize.x:=FixedSize.x;
 end else if fDesignedSize.x>0.0 then begin
  ContainerSize.x:=fDesignedSize.x;
 end else begin
  ContainerSize.x:=aWidget.Width;
 end;

 if FixedSize.y>0.0 then begin
  ContainerSize.y:=FixedSize.y;
 end else if fDesignedSize.y>0.0 then begin
  ContainerSize.y:=fDesignedSize.y;
 end else begin
  ContainerSize.y:=aWidget.Height;
 end;

 result:=TpvVector2.Null;

 Position:=TpvVector2.Create(fMargin,fMargin);

 MaxAxis1:=0.0;

 First:=true;

 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    if not First then begin
     Position[Axis0]:=Position[Axis0]+fSpacing[Axis0];
    end;
    ChildPreferredSize:=ChildWidget.PreferredSize;
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    if (not First) and
       ((Position[Axis0]+ChildTargetSize[Axis0])>=(ContainerSize[Axis0]-fMargin)) then begin
     Position[Axis0]:=fMargin;
     Position[Axis1]:=Position[Axis1]+fSpacing[Axis1]+MaxAxis1;
     MaxAxis1:=0.0;
    end;
    Position[Axis0]:=Position[Axis0]+ChildTargetSize[Axis0];
    MaxAxis1:=Max(MaxAxis1,ChildTargetSize[Axis1]);
    result[Axis0]:=Max(result[Axis0],Position[Axis0]+ChildTargetSize[Axis0]);
    result[Axis1]:=Max(result[Axis1],Position[Axis1]+ChildTargetSize[Axis1]);
    First:=false;
   end;
  end;
 end;

 result:=result+TpvVector2.Create(fMargin*2.0,fMargin*2.0);

 if FixedSize.x>0.0 then begin
  result.x:=FixedSize.x;
 end else if fDesignedSize.x>0.0 then begin
  result.x:=fDesignedSize.x;
 end;

 if FixedSize.y>0.0 then begin
  result.y:=FixedSize.y;
 end else if fDesignedSize.y>0.0 then begin
  result.y:=fDesignedSize.y;
 end;

end;

procedure TpvGUIFlowLayout.PerformLayout(const aWidget:TpvGUIWidget);
var Axis0,Axis1,RowFromChildIndex,RowToChildIndex:TpvInt32;
    ContainerSize:TpvVector2;
 procedure FlushRow;
 var ChildIndex:TpvInt32;
     Child:TpvGUIObject;
     ChildWidget:TpvGUIWidget;
     MinPosition,MaxPosition,MaxHeight,Difference:TpvFloat;
 begin
  if RowFromChildIndex<=RowToChildIndex then begin

   MinPosition:=MaxSingle;
   MaxPosition:=-MaxSingle;

   MaxHeight:=0.0;

   for ChildIndex:=RowFromChildIndex to RowToChildIndex do begin
    Child:=aWidget.fChildren.Items[ChildIndex];
    if Child is TpvGUIWidget then begin
     ChildWidget:=Child as TpvGUIWidget;
     if ChildWidget.Visible then begin
      MinPosition:=Min(MinPosition,fPositions[ChildIndex][Axis0]);
      MaxPosition:=Max(MaxPosition,fPositions[ChildIndex][Axis0]+fSizes[ChildIndex][Axis0]);
      MaxHeight:=Max(MaxHeight,fSizes[ChildIndex][Axis1]);
     end;
    end;
   end;

   if fAlignmentOnBaseLine then begin
    for ChildIndex:=RowFromChildIndex to RowToChildIndex do begin
     Child:=aWidget.fChildren.Items[ChildIndex];
     if Child is TpvGUIWidget then begin
      ChildWidget:=Child as TpvGUIWidget;
      if ChildWidget.Visible then begin
       fPositions[ChildIndex][Axis1]:=fPositions[ChildIndex][Axis1]+((MaxHeight-fSizes[ChildIndex][Axis1])*0.5);
      end;
     end;
    end;
   end;

   case fAlignments[Axis0] of
    pgflaLeading:begin
     Difference:=fMargin-MinPosition;
     for ChildIndex:=RowFromChildIndex to RowToChildIndex do begin
      Child:=aWidget.fChildren.Items[ChildIndex];
      if Child is TpvGUIWidget then begin
       ChildWidget:=Child as TpvGUIWidget;
       if ChildWidget.Visible then begin
        fPositions[ChildIndex][Axis0]:=fPositions[ChildIndex][Axis0]+Difference;
       end;
      end;
     end;
    end;
    pgflaMiddle:begin
     Difference:=((ContainerSize[Axis0]-(MaxPosition-MinPosition))*0.5)-MinPosition;
     for ChildIndex:=RowFromChildIndex to RowToChildIndex do begin
      Child:=aWidget.fChildren.Items[ChildIndex];
      if Child is TpvGUIWidget then begin
       ChildWidget:=Child as TpvGUIWidget;
       if ChildWidget.Visible then begin
        fPositions[ChildIndex][Axis0]:=fPositions[ChildIndex][Axis0]+Difference;
       end;
      end;
     end;
    end;
    pgflaTailing:begin
     Difference:=((ContainerSize[Axis0]-(fMargin+(MaxPosition-MinPosition))))-MinPosition;
     for ChildIndex:=RowFromChildIndex to RowToChildIndex do begin
      Child:=aWidget.fChildren.Items[ChildIndex];
      if Child is TpvGUIWidget then begin
       ChildWidget:=Child as TpvGUIWidget;
       if ChildWidget.Visible then begin
        fPositions[ChildIndex][Axis0]:=fPositions[ChildIndex][Axis0]+Difference;
       end;
      end;
     end;
    end;
   end;

  end;
 end;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    First:boolean;
    FixedSize,ChildPreferredSize,ChildFixedSize,ChildTargetSize,
    Position:TpvVector2;
    MaxAxis1,MinPosition,MaxPosition,MaxHeight,Difference:TpvFloat;
begin

 Axis0:=AxisOrientationAxes[fOrientation,0];
 Axis1:=AxisOrientationAxes[fOrientation,1];

 FixedSize:=aWidget.GetFixedSize;

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

 if length(fPositions)<>aWidget.fChildren.Count then begin
  SetLength(fPositions,aWidget.fChildren.Count);
 end;

 if length(fSizes)<>aWidget.fChildren.Count then begin
  SetLength(fSizes,aWidget.fChildren.Count);
 end;

 RowFromChildIndex:=0;
 RowToChildIndex:=-1;

 MaxAxis1:=0.0;

 Position:=TpvVector2.Create(fMargin,fMargin);

 if fDirection<>pgfldLeftToRight then begin
  Position[Axis0]:=ContainerSize[Axis0]-fMargin;
 end;

 First:=true;

 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildPreferredSize:=ChildWidget.PreferredSize;
    ChildFixedSize:=ChildWidget.GetFixedSize;
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
    if fDirection=pgfldLeftToRight then begin
     if not First then begin
      Position[Axis0]:=Position[Axis0]+fSpacing[Axis0];
      if (Position[Axis0]+ChildTargetSize[Axis0])>=(ContainerSize[Axis0]-fMargin) then begin
       RowToChildIndex:=ChildIndex-1;
       FlushRow;
       RowFromChildIndex:=ChildIndex;
       Position[Axis0]:=fMargin;
       Position[Axis1]:=Position[Axis1]+fSpacing[Axis1]+MaxAxis1;
       MaxAxis1:=0.0;
      end;
     end;
     fPositions[ChildIndex]:=Position;
     fSizes[ChildIndex]:=ChildTargetSize;
     Position[Axis0]:=Position[Axis0]+ChildTargetSize[Axis0];
    end else begin
     if not First then begin
      Position[Axis0]:=Position[Axis0]-fSpacing[Axis0];
     end;
     Position[Axis0]:=Position[Axis0]-ChildTargetSize[Axis0];
     if (not First) and
        (Position[Axis0]<fMargin) then begin
      RowToChildIndex:=ChildIndex-1;
      FlushRow;
      RowFromChildIndex:=ChildIndex;
      Position[Axis0]:=(ContainerSize[Axis0]-fMargin)-ChildTargetSize[Axis0];
      Position[Axis1]:=Position[Axis1]+fSpacing[Axis1]+MaxAxis1;
      MaxAxis1:=0.0;
     end;
     fPositions[ChildIndex]:=Position;
     fSizes[ChildIndex]:=ChildTargetSize;
    end;
    MaxAxis1:=Max(MaxAxis1,ChildTargetSize[Axis1]);
    RowToChildIndex:=ChildIndex;
    First:=false;
   end;
  end;
 end;

 FlushRow;

 MinPosition:=MaxSingle;
 MaxPosition:=-MaxSingle;

 MaxHeight:=0.0;

 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    MinPosition:=Min(MinPosition,fPositions[ChildIndex][Axis1]);
    MaxPosition:=Max(MaxPosition,fPositions[ChildIndex][Axis1]+fSizes[ChildIndex][Axis1]);
    MaxHeight:=Max(MaxHeight,fSizes[ChildIndex][Axis0]);
   end;
  end;
 end;

 case fAlignments[Axis1] of
  pgflaLeading:begin
   Difference:=fMargin-MinPosition;
   for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
    Child:=aWidget.fChildren.Items[ChildIndex];
    if Child is TpvGUIWidget then begin
     ChildWidget:=Child as TpvGUIWidget;
     if ChildWidget.Visible then begin
      fPositions[ChildIndex][Axis1]:=fPositions[ChildIndex][Axis1]+Difference;
     end;
    end;
   end;
  end;
  pgflaMiddle:begin
   Difference:=((ContainerSize[Axis1]-(MaxPosition-MinPosition))*0.5)-MinPosition;
   for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
    Child:=aWidget.fChildren.Items[ChildIndex];
    if Child is TpvGUIWidget then begin
     ChildWidget:=Child as TpvGUIWidget;
     if ChildWidget.Visible then begin
      fPositions[ChildIndex][Axis1]:=fPositions[ChildIndex][Axis1]+Difference;
     end;
    end;
   end;
  end;
  pgflaTailing:begin
   Difference:=((ContainerSize[Axis1]-(fMargin+(MaxPosition-MinPosition))))-MinPosition;
   for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
    Child:=aWidget.fChildren.Items[ChildIndex];
    if Child is TpvGUIWidget then begin
     ChildWidget:=Child as TpvGUIWidget;
     if ChildWidget.Visible then begin
      fPositions[ChildIndex][Axis1]:=fPositions[ChildIndex][Axis1]+Difference;
     end;
    end;
   end;
  end;
 end;

 for ChildIndex:=0 to aWidget.fChildren.Count-1 do begin
  Child:=aWidget.fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   if ChildWidget.Visible then begin
    ChildWidget.fPosition:=fPositions[ChildIndex];
    ChildWidget.fSize:=fSizes[ChildIndex];
    ChildWidget.PerformLayout;
   end;
  end;
 end;

end;

constructor TpvGUISkin.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fSignedDistanceFieldSpriteAtlas:=nil;
 fSansFont:=nil;
 fSansBoldFont:=nil;
 fSansBoldItalicFont:=nil;
 fSansItalicFont:=nil;
 fMonoFont:=nil;
 fIconWindowClose:=nil;
 fIconWindowRestore:=nil;
 fIconWindowMinimize:=nil;
 fIconWindowMaximize:=nil;
 fIconMenuRight:=nil;
 fIconContentCut:=nil;
 fIconContentCopy:=nil;
 fIconContentPaste:=nil;
 fIconContentDelete:=nil;
 fIconSelectAll:=nil;
 fIconSelectNone:=nil;
 fIconChevronLeft:=nil;
 fIconChevronRight:=nil;
 fIconChevronUp:=nil;
 fIconChevronDown:=nil;
 fIconDirectionArrowLeft:=nil;
 fIconDirectionArrowRight:=nil;
 fIconDirectionArrowUp:=nil;
 fIconDirectionArrowDown:=nil;
 fIconCheck:=nil;
 fIconRoundCheck:=nil;
 fIconThumbUp:=nil;
 fIconThumbDown:=nil;
 fIconDialogAlert:=nil;
 fIconDialogError:=nil;
 fIconDialogInformation:=nil;
 fIconDialogQuestion:=nil;
 fIconDialogStop:=nil;
 fIconDialogWarning:=nil;
 fIconArrowUpDown:=nil;
 Setup;
end;

destructor TpvGUISkin.Destroy;
begin
 FreeAndNil(fSansFont);
 FreeAndNil(fSansBoldFont);
 FreeAndNil(fSansBoldItalicFont);
 FreeAndNil(fSansItalicFont);
 FreeAndNil(fMonoFont);
 FreeAndNil(fSignedDistanceFieldSpriteAtlas);
 inherited Destroy;
end;

procedure TpvGUISkin.Setup;
begin

end;

procedure TpvGUISkin.DrawFocus(const aCanvas:TpvCanvas;const aWidget:TpvGUIWidget);
begin
end;

procedure TpvGUISkin.DrawMouse(const aCanvas:TpvCanvas;const aInstance:TpvGUIInstance);
begin
end;

function TpvGUISkin.GetWidgetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
begin
 result:=aWidget.GetWidgetPreferredSize;
end;

function TpvGUISkin.GetWidgetLayoutPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
begin
 result:=aWidget.GetWidgetLayoutPreferredSize;
end;

function TpvGUISkin.GetWindowPreferredSize(const aWindow:TpvGUIWindow):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aWindow);
end;

procedure TpvGUISkin.DrawWindow(const aCanvas:TpvCanvas;const aWindow:TpvGUIWindow);
begin
end;

function TpvGUISkin.GetImagePreferredSize(const aImage:TpvGUIImage):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aImage);
end;

procedure TpvGUISkin.DrawImage(const aCanvas:TpvCanvas;const aImage:TpvGUIImage);
begin
end;

function TpvGUISkin.GetLabelPreferredSize(const aLabel:TpvGUILabel):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aLabel);
end;

procedure TpvGUISkin.DrawLabel(const aCanvas:TpvCanvas;const aLabel:TpvGUILabel);
begin
end;

function TpvGUISkin.GetButtonPreferredSize(const aButton:TpvGUIButton):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aButton);
end;

procedure TpvGUISkin.DrawButton(const aCanvas:TpvCanvas;const aButton:TpvGUIButton);
begin
end;

function TpvGUISkin.GetCheckBoxPreferredSize(const aCheckBox:TpvGUICheckBox):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aCheckBox);
end;

procedure TpvGUISkin.DrawCheckBox(const aCanvas:TpvCanvas;const aCheckBox:TpvGUICheckBox);
begin
end;

function TpvGUISkin.GetTextEditPreferredSize(const aTextEdit:TpvGUITextEdit):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aTextEdit);
end;

procedure TpvGUISkin.DrawTextEdit(const aCanvas:TpvCanvas;const aTextEdit:TpvGUITextEdit);
begin
end;

function TpvGUISkin.GetPopupMenuPreferredSize(const aPopupMenu:TpvGUIPopupMenu):TpvVector2;
begin
 result:=TpvVector2.Null;
end;

procedure TpvGUISkin.DrawPopupMenu(const aCanvas:TpvCanvas;const aPopupMenu:TpvGUIPopupMenu);
begin
end;

function TpvGUISkin.GetWindowMenuPreferredSize(const aWindowMenu:TpvGUIWindowMenu):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aWindowMenu);
end;

procedure TpvGUISkin.DrawWindowMenu(const aCanvas:TpvCanvas;const aWindowMenu:TpvGUIWindowMenu);
begin
end;

function TpvGUISkin.GetScrollBarPreferredSize(const aScrollBar:TpvGUIScrollBar):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aScrollBar);
end;

procedure TpvGUISkin.DrawScrollBar(const aCanvas:TpvCanvas;const aScrollBar:TpvGUIScrollBar);
begin

end;

function TpvGUISkin.GetSliderPreferredSize(const aSlider:TpvGUISlider):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aSlider);
end;

procedure TpvGUISkin.DrawSlider(const aCanvas:TpvCanvas;const aSlider:TpvGUISlider);
begin

end;

function TpvGUISkin.GetProgressBarPreferredSize(const aProgressBar:TpvGUIProgressBar):TpvVector2;
begin
 result:=GetWidgetPreferredSize(aProgressBar);
end;

procedure TpvGUISkin.DrawProgressBar(const aCanvas:TpvCanvas;const aProgressBar:TpvGUIProgressBar);
begin

end;

constructor TpvGUIDefaultVectorBasedSkin.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
end;

destructor TpvGUIDefaultVectorBasedSkin.Destroy;
begin
 inherited Destroy;
end;

procedure TpvGUIDefaultVectorBasedSkin.Setup;
var Stream:TStream;
    TrueTypeFont:TpvTrueTypeFont;
begin

 fSpacing:=4.0;

 fFontSize:=-12;

 fWindowHeaderFontSize:=-16;

 fButtonFontSize:=-12;

 fTextEditFontSize:=-12;

 fLabelFontSize:=-12;

 fPopupMenuFontSize:=-12;

 fWindowMenuFontSize:=-12;

 fCheckBoxFontSize:=-12;

 fCheckBoxSize:=TpvVector2.Create(20.0,20.0);

 fFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fWindowFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fButtonFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fTextEditFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fLabelFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fPopupMenuFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fWindowMenuFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fCheckBoxFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fImageSignedDistanceFieldColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.5));

 fUnfocusedWindowHeaderFontShadow:=true;
 fFocusedWindowHeaderFontShadow:=true;

 fUnfocusedWindowHeaderFontShadowOffset:=TpvVector2.Create(2.0,2.0);
 fFocusedWindowHeaderFontShadowOffset:=TpvVector2.Create(2.0,2.0);

 fUnfocusedWindowHeaderFontShadowColor:=ConvertSRGBToLinear(TpvVector4.Create(0.0,0.0,0.0,0.3275));
 fFocusedWindowHeaderFontShadowColor:=ConvertSRGBToLinear(TpvVector4.Create(0.0,0.0,0.0,0.5));

 fUnfocusedWindowHeaderFontColor:=ConvertSRGBToLinear(TpvVector4.Create(0.86,0.86,0.86,0.62));
 fFocusedWindowHeaderFontColor:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,0.75));

 fWindowMenuHeight:=36;

 fWindowHeaderHeight:=32;

 fWindowResizeGripSize:=8;

 fMinimizedWindowMinimumWidth:=Max(fSpacing*2.0,fWindowResizeGripSize*2);
 fMinimizedWindowMinimumHeight:=Max(fWindowHeaderHeight,fWindowResizeGripSize*2);

 fWindowMinimumWidth:=Max(fWindowHeaderHeight+(fSpacing*2.0),fWindowResizeGripSize*2);
 fWindowMinimumHeight:=Max(fWindowHeaderHeight+(fSpacing*2.0),fWindowResizeGripSize*2);

 fWindowShadowWidth:=16;
 fWindowShadowHeight:=16;

 fPopupAnchorHeight:=0.0;

 fSignedDistanceFieldSpriteAtlas:=TpvSpriteAtlas.Create(fInstance.fVulkanDevice,false);
 fSignedDistanceFieldSpriteAtlas.UseConvexHullTrimming:=false;

 Stream:=TpvDataStream.Create(@GUIStandardTrueTypeFontSansFontData,GUIStandardTrueTypeFontSansFontDataSize);
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fSansFont:=TpvFont.CreateFromTrueTypeFont(pvApplication.VulkanDevice,
                                             fSignedDistanceFieldSpriteAtlas,
                                             TrueTypeFont,
                                             fInstance.fFontCodePointRanges,
                                             true,
                                             2,
                                             1);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@GUIStandardTrueTypeFontSansBoldFontData,GUIStandardTrueTypeFontSansBoldFontDataSize);
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fSansBoldFont:=TpvFont.CreateFromTrueTypeFont(pvApplication.VulkanDevice,
                                                 fSignedDistanceFieldSpriteAtlas,
                                                 TrueTypeFont,
                                                 fInstance.fFontCodePointRanges,
                                                 true,
                                                 2,
                                                 1);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@GUIStandardTrueTypeFontSansBoldItalicFontData,GUIStandardTrueTypeFontSansBoldItalicFontDataSize);
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fSansBoldItalicFont:=TpvFont.CreateFromTrueTypeFont(pvApplication.VulkanDevice,
                                                       fSignedDistanceFieldSpriteAtlas,
                                                       TrueTypeFont,
                                                       fInstance.fFontCodePointRanges,
                                                       true,
                                                       2,
                                                       1);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 Stream:=TpvDataStream.Create(@GUIStandardTrueTypeFontSansItalicFontData,GUIStandardTrueTypeFontSansItalicFontDataSize);
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fSansItalicFont:=TpvFont.CreateFromTrueTypeFont(pvApplication.VulkanDevice,
                                                   fSignedDistanceFieldSpriteAtlas,
                                                   TrueTypeFont,
                                                   fInstance.fFontCodePointRanges,
                                                   true,
                                                   2,
                                                   1);
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
                                             fSignedDistanceFieldSpriteAtlas,
                                             TrueTypeFont,
                                             fInstance.fFontCodePointRanges,
                                             true,
                                             2,
                                             1);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 fWindowButtonIconHeight:=14.0;

 fIconWindowClose:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconWindowClose',
                                                                                 'M461.029 419.2l-164.571-163.2 164.571-163.2-41.143-41.143-164.571 163.2-163.2-163.2-41.143 41.143 163.2 163.2-163.2 163.2 41.143 41.143 163.2-163.2 164.571 163.2z',
                                                                                 48,
                                                                                 48,
                                                                                 48.0/512.0,
                                                                                 0.0,
                                                                                 0.0,
                                                                                 pvvpfrNonZero,
                                                                                 true,
                                                                                 2,
                                                                                 1);

 fIconWindowRestore:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconWindowRestore',
                                                                                   'M61.714 353.143h97.143v98.286h292.571v-292.571h-97.143v-98.286h-292.571v292.571zm292.571 0v-146.286h49.143v195.429h-195.429v-49.143h146.286zm-243.429-97.143v-146.286h194.286v146.286h-194.286z',
                                                                                   48,
                                                                                   48,
                                                                                   48.0/512.0,
                                                                                   0.0,
                                                                                   0.0,
                                                                                   pvvpfrNonZero,
                                                                                   true,
                                                                                   2,
                                                                                   1);

 fIconWindowMinimize:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconWindowMinimize',
                                                                                    'M450.286 321.143h-389.714v98.286h389.714v-98.286z',
                                                                                    48,
                                                                                    48,
                                                                                    48.0/512.0,
                                                                                    0.0,
                                                                                    0.0,
                                                                                    pvvpfrNonZero,
                                                                                    true,
                                                                                    2,
                                                                                    1);

 fIconWindowMaximize:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconWindowMaximize',
                                                                                    'M61.714 451.429h389.714v-390.857h-389.714v390.857zm49.143-98.286v-243.429h292.571v243.429h-292.571z',
                                                                                    48,
                                                                                    48,
                                                                                    48.0/512.0,
                                                                                    0.0,
                                                                                    0.0,
                                                                                    pvvpfrNonZero,
                                                                                    true,
                                                                                    2,
                                                                                    1);

 fIconMenuRight:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconMenuRight',
                                                                               'M6,2 l12,10 l-12,10 v-20 z',
                                                                               96,
                                                                               96,
                                                                               96.0/24.0,
                                                                               0.0,
                                                                               0.0,
                                                                               pvvpfrNonZero,
                                                                               true,
                                                                               2,
                                                                               1);

 fIconContentCut:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconContentCut',
                                                                                'M19,3L13,9L15,11L22,4V3M12,12.5A0.5,0.5 0 0,1 11.5,12A0.5,0.5 0 0,1 12,11.5A0.5,0.5 0 0,1 12.5,12A0.5,0.5'+' 0 0,1 12,12.5M6,20A2,2 0 0,1 4,18C4,16.89 4.9,16 6,16A2,2 0 0,1 8,18C8,19.11 7.1,20 6,20M6,8A2,2 0 0,1 4,6C4,4.89 4.9,4 6,4A2,2 0 0,1 8,6C8,7.11 7.1,8 6,8M9.64,'+'7.64C9.87,7.14 10,6.59 10,6A4,4 0 0,0 6,2A4,4 0 0,0 2,6A4,4 0 0,0 6,10C6.59,10 7.14,9.87'+' 7.64,9.64L10,12L7.64,14.36C7.14,14.13 6.59,14 6,14A4,4 0 0,0 2,18A4,4 0 0,0 6,22A4,4 0 0,0 10,18C10,17.41'+' 9.87,16.86 9.64,16.36L12,14L19,21H22V20L9.64,7.64Z',
                                                                                96,
                                                                                96,
                                                                                96.0/24.0,
                                                                                0.0,
                                                                                0.0,
                                                                                pvvpfrNonZero,
                                                                                true,
                                                                                2,
                                                                                1);

 fIconContentCopy:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconContentCopy',
                                                                                 'M19,21H8V7H19M19,5H8A2,2 0 0,0 6,7V21A2,2 0 0,0 8,23H19A2,2 0 0,0 21,21V7A2,2 0 0,0 19,5M16,1H4A2,2 0 0,0 2,3V17H4V3H16V1Z',
                                                                                 96,
                                                                                 96,
                                                                                 96.0/24.0,
                                                                                 0.0,
                                                                                 0.0,
                                                                                 pvvpfrNonZero,
                                                                                 true,
                                                                                 2,
                                                                                 1);

 fIconContentPaste:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconContentPaste',
                                                                                  'M19,20H5V4H7V7H17V4H19M12,2A1,1 0 0,1 13,3A1,1 0 0,1 12,4A1,1 0 0,1 11,3A1,1 0 0,1 12,2M19,2H14.82C14.4,0.84 13.3,0 12,0C10.7,0 9.6,0.84 9.18,2H5A2,2 0 0,0 3,4V20'+'A2,2 0 0,0 5,22H19A2,2 0 0,0 21,20V4A2,2 0 0,0 19,2Z',
                                                                                  96,
                                                                                  96,
                                                                                  96.0/24.0,
                                                                                  0.0,
                                                                                  0.0,
                                                                                  pvvpfrNonZero,
                                                                                  true,
                                                                                  2,
                                                                                  1);

 fIconContentDelete:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconContentDelete',
                                                                                   'M19,4H15.5L14.5,3H9.5L8.5,4H5V6H19M6,19A2,2 0 0,0 8,21H16A2,2 0 0,0 18,19V7H6V19Z',
                                                                                   96,
                                                                                   96,
                                                                                   96.0/24.0,
                                                                                   0.0,
                                                                                   0.0,
                                                                                   pvvpfrNonZero,
                                                                                   true,
                                                                                   2,
                                                                                   1);

 fIconSelectAll:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconSelectAll',
                                                                               'M9,9H15V15H9M7,17H17V7H7M15,5H17V3H15M15,21H17V19H15M19,17H21V15H19M19,9H21V7H19M19,21A2,2 0 0,0 21,19H19M19,13H21V11H19M11,21H13V19H11M9,3H7V5H9M3,17H5V15H3M5,21V19H3A2,2 0 0,0 5,21'+'M19,3V5H21A2,2 0 0,0 19,3M13,3H11V5H13M3,9H5V7H3M7,21H9V19H7M3,13H5V11H3M3,5H5V3A2,2 0 0,0 3,5Z',
                                                                               96,
                                                                               96,
                                                                               96.0/24.0,
                                                                               0.0,
                                                                               0.0,
                                                                               pvvpfrNonZero,
                                                                               true,
                                                                               2,
                                                                               1);

 fIconSelectNone:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconSelectNone',
                                                                                'M1,4.27L2.28,3L21,21.72L19.73,23L17,20.27V21H15V19H15.73L5,8.27V9H3V7H3.73L1,4.27M20,3A1,1 0 0,1 21,4V5H19V3H20M15,5V3H17V5H15M11,5V3H13V5H11M7,5V3H9V5H7M11,21V19H13V21H11M7,21V19H9V21H7'+'M4,21A1,1 0 0,1 3,20V19H5V21H4M3,15H5V17H3V15M21,15V17H19V15H21M3,11H5V13H3V11M21,11V13H19V11H21M21,7V9H19V7H21Z',
                                                                                96,
                                                                                96,
                                                                                96.0/24.0,
                                                                                0.0,
                                                                                0.0,
                                                                                pvvpfrNonZero,
                                                                                true,
                                                                                2,
                                                                                1);

 fIconChevronLeft:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconChevronLeft',
                                                                                 'M17.558 18.87 10.688 12 17.558 5.115 15.442 3 6.442 12 15.442 21 17.558 18.87Z',
                                                                                 48,
                                                                                 48,
                                                                                 48.0/24.0,
                                                                                 0.0,
                                                                                 0.0,
                                                                                 pvvpfrNonZero,
                                                                                 true,
                                                                                 2,
                                                                                 1);

 fIconChevronRight:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconChevronRight',
                                                                                  'M6.443 18.87 13.313 12 6.443 5.115 8.558 3 17.558 12 8.558 21 6.443 18.87Z',
                                                                                  48,
                                                                                  48,
                                                                                  48.0/24.0,
                                                                                  0.0,
                                                                                  0.0,
                                                                                  pvvpfrNonZero,
                                                                                  true,
                                                                                  2,
                                                                                  1);

 fIconChevronUp:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconChevronUp',
                                                                               'M5.115 17.558 12 10.688 18.885 17.558 21 15.442 12 6.442 3 15.442 5.115 17.558Z',
                                                                               48,
                                                                               48,
                                                                               48.0/24.0,
                                                                               0.0,
                                                                               0.0,
                                                                               pvvpfrNonZero,
                                                                               true,
                                                                               2,
                                                                               1);

 fIconChevronDown:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconChevronDown',
                                                                                 'M5.115 6.435 12 13.32 18.885 6.435 21 8.565 12 17.565 3 8.565 5.115 6.435Z',
                                                                                 48,
                                                                                 48,
                                                                                 48.0/24.0,
                                                                                 0.0,
                                                                                 0.0,
                                                                                 pvvpfrNonZero,
                                                                                 true,
                                                                                 2,
                                                                                 1);

 fIconCheck:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconCheck',
                                                                           'M21,7L9,19L3.5,13.5L4.91,12.09L9,16.17L19.59,5.59L21,7Z',
                                                                           48,
                                                                           48,
                                                                           48.0/24.0,
                                                                           0.0,
                                                                           0.0,
                                                                           pvvpfrNonZero,
                                                                           true,
                                                                           2,
                                                                           1);

 fIconDirectionArrowLeft:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDirectionArrowLeft',
                                                                                        'M14,7L9,12L14,17V7Z',
                                                                                        48,
                                                                                        48,
                                                                                        48.0/24.0,
                                                                                        0.0,
                                                                                        0.0,
                                                                                        pvvpfrNonZero,
                                                                                        true,
                                                                                        2,
                                                                                        1);

 fIconDirectionArrowRight:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDirectionArrowRight',
                                                                                         'M10,17L15,12L10,7V17Z',
                                                                                         48,
                                                                                         48,
                                                                                         48.0/24.0,
                                                                                         0.0,
                                                                                         0.0,
                                                                                         pvvpfrNonZero,
                                                                                         true,
                                                                                         2,
                                                                                         1);

 fIconDirectionArrowUp:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDirectionArrowUp',
                                                                                      'M7,15L12,10L17,15H7Z',
                                                                                      48,
                                                                                      48,
                                                                                      48.0/24.0,
                                                                                      0.0,
                                                                                      0.0,
                                                                                      pvvpfrNonZero,
                                                                                      true,
                                                                                      2,
                                                                                      1);

 fIconDirectionArrowDown:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDirectionArrowDown',
                                                                                        'M7,10L12,15L17,10H7Z',
                                                                                        48,
                                                                                        48,
                                                                                        48.0/24.0,
                                                                                        0.0,
                                                                                        0.0,
                                                                                        pvvpfrNonZero,
                                                                                        true,
                                                                                        2,
                                                                                        1);

 fIconCheck:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconCheck',
                                                                           'M21,7L9,19L3.5,13.5L4.91,12.09L9,16.17L19.59,5.59L21,7Z',
                                                                           48,
                                                                           48,
                                                                           48.0/24.0,
                                                                           0.0,
                                                                           0.0,
                                                                           pvvpfrNonZero,
                                                                           true,
                                                                           2,
                                                                           1);

 fIconRoundCheck:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconRoundCheck',
                                                                                'M12 4A8 8 0 0 0 4 12 8 8 0 0 0 12 20 8 8 0 0 0 20 12 8 8 0 0 0 12 4Z',
                                                                                48,
                                                                                48,
                                                                                48.0/24.0,
                                                                                0.0,
                                                                                0.0,
                                                                                pvvpfrNonZero,
                                                                                true,
                                                                                2,
                                                                                1);

 fIconThumbUp:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconThumbUp',
                                                                             'M23,10C23,8.89 22.1,8 21,8H14.68L15.64,3.43C15.66,3.33 15.67,3.22 15.67,3.11C15.67,2.7 15.5,2.32 15.23,2.05L14.17,1L7.59,7.58C7.22,7.95 7,8.45 7,9V19A2,2'+' 0 0,0 9,21H18C18.83,21 19.54,20.5 19.84,19.78L22.86,12.73C22.95,12.5 23,12.26 23,12V10.08L23,10M1,21H5V9H1V21Z',
                                                                             48,
                                                                             48,
                                                                             48.0/24.0,
                                                                             0.0,
                                                                             0.0,
                                                                             pvvpfrNonZero,
                                                                             true,
                                                                             2,
                                                                             1);

 fIconThumbDown:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconThumbDown',
                                                                               'M19,15H23V3H19M15,3H6C5.17,3 4.46,3.5 4.16,4.22L1.14,11.27C1.05,11.5 1,11.74 1,12V13.91L1,14A2,2 0 0,0 3,16H9.31L8.36,20.57C8.34,20.67 8.33,20.77 8.33,20.88C8.33,21.3 8.5,21.67'+' 8.77,21.94L9.83,23L16.41,16.41C16.78,16.05 17,15.55 17,15V5C17,3.89 16.1,3 15,3Z',
                                                                               48,
                                                                               48,
                                                                               48.0/24.0,
                                                                               0.0,
                                                                               0.0,
                                                                               pvvpfrNonZero,
                                                                               true,
                                                                               2,
                                                                               1);

 fIconDialogAlert:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDialogAlert',
                                                                                 'M13,14H11V10H13M13,18H11V16H13M1,21H23L12,2L1,21Z',
                                                                                 48,
                                                                                 48,
                                                                                 48.0/24.0,
                                                                                 0.0,
                                                                                 0.0,
                                                                                 pvvpfrNonZero,
                                                                                 true,
                                                                                 2,
                                                                                 1);

 fIconDialogError:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDialogError',
                                                                                 'M13,13H11V7H13M13,17H11V15H13M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2Z',
                                                                                 48,
                                                                                 48,
                                                                                 48.0/24.0,
                                                                                 0.0,
                                                                                 0.0,
                                                                                 pvvpfrNonZero,
                                                                                 true,
                                                                                 2,
                                                                                 1);

 fIconDialogInformation:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDialogInformation',
                                                                                       'M13,9H11V7H13M13,17H11V11H13M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2Z',
                                                                                       48,
                                                                                       48,
                                                                                       48.0/24.0,
                                                                                       0.0,
                                                                                       0.0,
                                                                                       pvvpfrNonZero,
                                                                                       true,
                                                                                       2,
                                                                                       1);

 fIconDialogQuestion:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDialogQuestion',
                                                                                    'M15.07,11.25L14.17,12.17C13.45,12.89 13,13.5 13,15H11V14.5C11,13.39 11.45,12.39 12.17,11.67L13.41,10.41C13.78,10.05'+' 14,9.55 14,9C14,7.89 13.1,7 12,7A2,2 0 0,0 10,9H8A4,4 0 0,1 12,5A4,4 0 0,1 16,9C16,9.88 15.64,10.67 15.07,11.25M13,19H11V17H13M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12C22,6.47 17.5,2 12,2Z',
                                                                                    48,
                                                                                    48,
                                                                                    48.0/24.0,
                                                                                    0.0,
                                                                                    0.0,
                                                                                    pvvpfrNonZero,
                                                                                    true,
                                                                                    2,
                                                                                    1);

 fIconDialogStop:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDialogStop',
                                                                                'M12 1.2A10.8 10.8 0 0 1 22.8 12 10.8 10.8 0 0 1 12 22.8 10.8 10.8 0 0 1 1.2 12 10.8 10.8 0 0 1 12 1.2M12 3A9 9 0 0 0 3 12'+'C3 14.16 3.765 16.14 5.034 17.697L17.697 5.034C16.14 3.765 14.16 3 12 3M12 21A9 9 0 0 0 21 12C21 9.84 20.235 7.86 18.966 6.303L6.303 18.966C7.86 20.235 9.84 21 12 21Z',
                                                                                48,
                                                                                48,
                                                                                48.0/24.0,
                                                                                0.0,
                                                                                0.0,
                                                                                pvvpfrNonZero,
                                                                                true,
                                                                                2,
                                                                                1);

 fIconDialogWarning:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconDialogWarning',
                                                                                   'M13,14H11V10H13M13,18H11V16H13M1,21H23L12,2L1,21Z',
                                                                                   48,
                                                                                   48,
                                                                                   48.0/24.0,
                                                                                   0.0,
                                                                                   0.0,
                                                                                   pvvpfrNonZero,
                                                                                   true,
                                                                                   2,
                                                                                   1);

 fIconArrowUpDown:=fSignedDistanceFieldSpriteAtlas.LoadSignedDistanceFieldSprite('IconArrowUpDown',
                                                                                 'M4 10l8-8 8 8h-16zM4 14l8 8 8-8h-16z',
                                                                                 48,
                                                                                 48,
                                                                                 48.0/24.0,
                                                                                 0.0,
                                                                                 0.0,
                                                                                 pvvpfrNonZero,
                                                                                 true,
                                                                                 2,
                                                                                 1);

 fIconChevronHeight:=14.0;

 fIconPopupMenuHeight:=14.0;

 fIconMenuRightHeight:=12.0;

 fSignedDistanceFieldSpriteAtlas.MipMaps:=false;
 fSignedDistanceFieldSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                                        pvApplication.VulkanGraphicsCommandBuffers[0,0],
                                        pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                                        pvApplication.VulkanDevice.TransferQueue,
                                        pvApplication.VulkanTransferCommandBuffers[0,0],
                                        pvApplication.VulkanTransferCommandBufferFences[0,0]);

// SignedDistanceFieldSpriteAtlas.SaveToFile('testbla.zip');

 //fSansFont.SaveToFile('testfont.xml');

end;

procedure TpvGUIDefaultVectorBasedSkin.DrawFocus(const aCanvas:TpvCanvas;const aWidget:TpvGUIWidget);
var Rect:TpvRect;
begin
 if assigned(fInstance) then begin
  Rect:=aWidget.HighlightRect;
  if fInstance.fHoveredWidget=aWidget then begin
   aCanvas.ClipRect:=aWidget.fParentClipRect;
   aCanvas.ModelMatrix:=aWidget.fModelMatrix;
   aCanvas.DrawGUIElement(GUI_ELEMENT_HOVERED,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          aWidget.fSize+TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(0.0,0.0),
                          Rect.Size);
  end else if fInstance.fFocusedWidget=aWidget then begin
   aCanvas.ClipRect:=aWidget.fParentClipRect;
   aCanvas.ModelMatrix:=aWidget.fModelMatrix;
   aCanvas.DrawGUIElement(GUI_ELEMENT_FOCUSED,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          aWidget.fSize+TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(0.0,0.0),
                          Rect.Size);
  end;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawMouse(const aCanvas:TpvCanvas;const aInstance:TpvGUIInstance);
begin
 aCanvas.ModelMatrix:=TpvMatrix4x4.CreateTranslation(aInstance.fMousePosition)*aInstance.fModelMatrix;
 aCanvas.ClipRect:=aInstance.fClipRect;
 case aInstance.fVisibleCursor of
  pvgcArrow:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_ARROW,
                          false,
                          TpvVector2.Create(2.0,2.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(2.0,2.0),
                          TpvVector2.Create(34.0,34.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_ARROW,
                          true,
                          TpvVector2.Null,
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Null,
                          TpvVector2.Create(32.0,32.0));
  end;
  pvgcBeam:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_BEAM,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_BEAM,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcBusy:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_BUSY,
                          false,
                          TpvVector2.Create(-18.0,-18.0),
                          TpvVector2.Create(22.0,22.0),
                          TpvVector2.Create(-8.0,-8.0),
                          TpvVector2.Create(12.0,12.0),
                          frac(aInstance.fTime)*TwoPI);
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_BUSY,
                          true,
                          TpvVector2.Create(-20.0,-20.0),
                          TpvVector2.Create(20.0,20.0),
                          TpvVector2.Create(-10.0,-10.0),
                          TpvVector2.Create(10.0,10.0),
                          frac(aInstance.fTime)*TwoPI);
  end;
  pvgcCross:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_CROSS,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_CROSS,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcEW:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_EW,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_EW,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcHelp:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_HELP,
                          false,
                          TpvVector2.Create(2.0,2.0),
                          TpvVector2.Create(64.0,64.0),
                          TpvVector2.Create(2.0,2.0),
                          TpvVector2.Create(34.0,34.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_HELP,
                          true,
                          TpvVector2.Null,
                          TpvVector2.Create(64.0,64.0),
                          TpvVector2.Null,
                          TpvVector2.Create(32.0,32.0));
  end;
  pvgcLink:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_LINK,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(2.0,2.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_LINK,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Null,
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcMove:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_MOVE,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_MOVE,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcNESW:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_NESW,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_NESW,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcNS:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_NS,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_NS,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcNWSE:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_NWSE,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_NWSE,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcPen:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_PEN,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_PEN,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
  pvgcUnavailable:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_UNAVAILABLE,
                          false,
                          TpvVector2.Create(-18.0,-18.0),
                          TpvVector2.Create(22.0,22.0),
                          TpvVector2.Create(-8.0,-8.0),
                          TpvVector2.Create(12.0,12.0),
                          frac(aInstance.fTime)*TwoPI);
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_UNAVAILABLE,
                          true,
                          TpvVector2.Create(-20.0,-20.0),
                          TpvVector2.Create(20.0,20.0),
                          TpvVector2.Create(-10.0,-10.0),
                          TpvVector2.Create(10.0,10.0));
  end;
  pvgcUp:begin
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_UP,
                          false,
                          TpvVector2.Create(-30.0,-30.0),
                          TpvVector2.Create(34.0,34.0),
                          TpvVector2.Create(-14.0,-14.0),
                          TpvVector2.Create(18.0,18.0));
   aCanvas.DrawGUIElement(GUI_ELEMENT_MOUSE_CURSOR_UP,
                          true,
                          TpvVector2.Create(-32.0,-32.0),
                          TpvVector2.Create(32.0,32.0),
                          TpvVector2.Create(-16.0,-16.0),
                          TpvVector2.Create(16.0,16.0));
  end;
 end;
end;

function TpvGUIDefaultVectorBasedSkin.GetWidgetPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
begin
 result:=inherited GetWidgetPreferredSize(aWidget);
end;

function TpvGUIDefaultVectorBasedSkin.GetWidgetLayoutPreferredSize(const aWidget:TpvGUIWidget):TpvVector2;
begin
 result:=inherited GetWidgetLayoutPreferredSize(aWidget);
end;

function TpvGUIDefaultVectorBasedSkin.GetWindowPreferredSize(const aWindow:TpvGUIWindow):TpvVector2;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    TextSize:TpvVector2;
begin
 if assigned(aWindow.fButtonPanel) then begin
  aWindow.fButtonPanel.Visible:=false;
 end;
 if assigned(aWindow.fMenu) then begin
  aWindow.fMenu.Visible:=false;
 end;
 TextSize:=aWindow.Font.TextSize(aWindow.fTitle,
                                 fWindowHeaderFontSize)+
           TpvVector2.Create(fSpacing*2.0,0.0);
 result:=Maximum(GetWidgetLayoutPreferredSize(aWindow),
                 TextSize);
 if pvgwfHeader in aWindow.fWindowFlags then begin
  result.y:=Maximum(result.y,fWindowHeaderHeight);
 end;
 if assigned(aWindow.fButtonPanel) then begin
  aWindow.fButtonPanel.Visible:=true;
  for ChildIndex:=0 to aWindow.fButtonPanel.fChildren.Count-1 do begin
   Child:=aWindow.fButtonPanel.fChildren.Items[ChildIndex];
   if Child is TpvGUIWidget then begin
    ChildWidget:=Child as TpvGUIWidget;
    ChildWidget.FixedWidth:=22;
    ChildWidget.FixedHeight:=22;
    ChildWidget.FontSize:=-15;
   end;
  end;
  result:=Maximum(result,aWindow.fButtonPanel.PreferredSize+
                         TpvVector2.Create(TextSize.x+(fSpacing*2.0),0.0));
 end;
 if assigned(aWindow.fMenu) then begin
  aWindow.fMenu.Visible:=true;
 end;
 case aWindow.fWindowState of
  pvgwsMinimized:begin
   result.y:=fWindowHeaderHeight;
  end;
  pvgwsMaximized:begin
   if assigned(fParent) then begin
    if fParent is TpvGUIInstance then begin
     result:=(fParent as TpvGUIInstance).fContent.fSize;
    end else if fParent is TpvGUIWindow then begin
     result:=(fParent as TpvGUIWindow).fContent.fSize;
    end else if fParent is TpvGUIWidget then begin
     result:=(fParent as TpvGUIWidget).fSize;
    end;
   end;
  end;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawWindow(const aCanvas:TpvCanvas;const aWindow:TpvGUIWindow);
var LastClipRect,NewClipRect:TpvRect;
    LastModelMatrix,NewModelMatrix:TpvMatrix4x4;
    LastColor:TpvVector4;
    Title:TpvRawByteString;
    Offset:TpvVector2;
begin
 LastColor:=aCanvas.Color;
 try

  aCanvas.ModelMatrix:=aWindow.fModelMatrix;

  aCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);

  aCanvas.ClipRect:=aWindow.fParentClipRect;
  aCanvas.DrawGUIElement(GUI_ELEMENT_WINDOW_DROPSHADOW,
                        aWindow.Focused,
                        TpvVector2.Create(-fWindowShadowWidth,-fWindowShadowHeight),
                        aWindow.fSize+TpvVector2.Create(fWindowShadowWidth*2,fWindowShadowHeight*2),
                        TpvVector2.Create(0.0,0.0),
                        aWindow.fSize);

  aCanvas.ClipRect:=aWindow.fClipRect;

  if pvgwfHeader in aWindow.fWindowFlags then begin

   aCanvas.DrawGUIElement(GUI_ELEMENT_WINDOW_FILL,
                          aWindow.Focused,
                          TpvVector2.Create(0.0,fWindowHeaderHeight-fSpacing),
                          TpvVector2.Create(aWindow.fSize.x,aWindow.fSize.y),
                          TpvVector2.Create(0.0,fWindowHeaderHeight-fSpacing),
                          TpvVector2.Create(aWindow.fSize.x,aWindow.fSize.y));

   aCanvas.DrawGUIElement(GUI_ELEMENT_WINDOW_HEADER,
                          aWindow.Focused,
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aWindow.fSize.x,fWindowHeaderHeight),
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aWindow.fSize.x,fWindowHeaderHeight));

   LastClipRect:=aCanvas.ClipRect;
   LastClipRect.LeftTop:=LastClipRect.LeftTop+TpvVector2.Create(1.0,1.0);
   LastClipRect.RightBottom:=LastClipRect.RightBottom-TpvVector2.Create(1.0,1.0);
   aCanvas.ClipRect:=LastClipRect;

   LastModelMatrix:=aCanvas.ModelMatrix;
   try
    aCanvas.Font:=aWindow.Font;
    aCanvas.FontSize:=fWindowHeaderFontSize;
    case aWindow.TextHorizontalAlignment of
     pvgtaLeading:begin
      aCanvas.TextHorizontalAlignment:=pvcthaLeading;
      Offset.x:=fSpacing;
     end;
     pvgtaCenter:begin
      aCanvas.TextHorizontalAlignment:=pvcthaCenter;
      if assigned(aWindow.fButtonPanel) and (aWindow.fButtonPanel.Children.Count>0) then begin
       Offset.x:=aWindow.fButtonPanel.Left*0.5;
      end else begin
       Offset.x:=aWindow.fSize.x*0.5;
      end;
     end;
     else {pvgtaTailing:}begin
      aCanvas.TextHorizontalAlignment:=pvcthaTailing;
      if assigned(aWindow.fButtonPanel) and (aWindow.fButtonPanel.Children.Count>0) then begin
       Offset.x:=aWindow.fButtonPanel.Left-fSpacing;
      end else begin
       Offset.x:=aWindow.fSize.x-fSpacing;
      end;
     end;
    end;
    Offset.y:=fWindowHeaderHeight*0.5;
    aCanvas.TextVerticalAlignment:=pvctvaMiddle;
    NewModelMatrix:=TpvMatrix4x4.CreateTranslation(Offset.x,Offset.y)*LastModelMatrix;
    if assigned(aWindow.fButtonPanel) and (aWindow.fButtonPanel.Children.Count>0) then begin
     Title:=TpvGUITextUtils.TextTruncation(aWindow.fTitle,
                                           aWindow.fTextTruncation,
                                           aCanvas.Font,
                                           aCanvas.FontSize,
                                           aWindow.fButtonPanel.Left-(fSpacing*2.0));
    end else begin
     Title:=TpvGUITextUtils.TextTruncation(aWindow.fTitle,
                                           aWindow.fTextTruncation,
                                           aCanvas.Font,
                                           aCanvas.FontSize,
                                           aWindow.fSize.x-(fSpacing*2.0));
    end;
    if ((pvgwfFocused in aWindow.fWidgetFlags) and fFocusedWindowHeaderFontShadow) or
       ((not (pvgwfFocused in aWindow.fWidgetFlags)) and fUnfocusedWindowHeaderFontShadow) then begin
     if pvgwfFocused in aWindow.fWidgetFlags then begin
      aCanvas.ModelMatrix:=TpvMatrix4x4.CreateTranslation(fFocusedWindowHeaderFontShadowOffset)*NewModelMatrix;
      aCanvas.Color:=fFocusedWindowHeaderFontShadowColor;
     end else begin
      aCanvas.ModelMatrix:=TpvMatrix4x4.CreateTranslation(fUnfocusedWindowHeaderFontShadowOffset)*NewModelMatrix;
      aCanvas.Color:=fUnfocusedWindowHeaderFontShadowColor;
     end;
     aCanvas.DrawText(Title);
    end;
    aCanvas.ModelMatrix:=NewModelMatrix;
    if pvgwfFocused in aWindow.fWidgetFlags then begin
     aCanvas.Color:=fFocusedWindowHeaderFontColor;
    end else begin
     aCanvas.Color:=fUnfocusedWindowHeaderFontColor;
    end;
    aCanvas.DrawText(Title);
   finally
    aCanvas.ModelMatrix:=LastModelMatrix;
   end;

  end else begin

   aCanvas.DrawGUIElement(GUI_ELEMENT_WINDOW_FILL,
                          aWindow.Focused,
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aWindow.fSize.x,aWindow.fSize.y),
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aWindow.fSize.x,aWindow.fSize.y));

   LastClipRect:=aCanvas.ClipRect;
   LastClipRect.LeftTop:=LastClipRect.LeftTop+TpvVector2.Create(1.0,1.0);
   LastClipRect.RightBottom:=LastClipRect.RightBottom-TpvVector2.Create(1.0,1.0);
   aCanvas.ClipRect:=LastClipRect;

  end;

 finally
  aCanvas.Color:=LastColor;
 end;

end;

function TpvGUIDefaultVectorBasedSkin.GetImagePreferredSize(const aImage:TpvGUIImage):TpvVector2;
var ImageSize:TpvVector2;
begin
 if assigned(aImage.fImage) then begin
  if aImage.fImage is TpvSprite then begin
   ImageSize:=TpvVector2.Create(TpvSprite(aImage.fImage).Width,TpvSprite(aImage.fImage).Height);
  end else if aImage.fImage is TpvVulkanTexture then begin
   ImageSize:=TpvVector2.Create(TpvVulkanTexture(aImage.fImage).Width,TpvVulkanTexture(aImage.fImage).Height);
  end else begin
   ImageSize:=TpvVector2.Null;
  end;
 end else begin
  ImageSize:=TpvVector2.Null;
 end;
 if aImage.fFixedSize.x>0.0 then begin
  if aImage.fFixedSize.y>0.0 then begin
   result.x:=aImage.fFixedSize.x;
   result.y:=aImage.fFixedSize.y;
  end else begin
   result.x:=ImageSize.x;
   result.y:=(ImageSize.y*aImage.fFixedSize.x)/ImageSize.x;
  end;
 end else begin
  if aImage.fFixedSize.y>0.0 then begin
   result.x:=(ImageSize.x*aImage.fFixedSize.y)/ImageSize.y;
   result.y:=aImage.fFixedSize.y;
  end else begin
   result:=ImageSize;
  end;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawImage(const aCanvas:TpvCanvas;const aImage:TpvGUIImage);
begin
 aCanvas.ModelMatrix:=aImage.fModelMatrix;
 aCanvas.ClipRect:=aImage.fClipRect;
 if assigned(aImage.fImage) then begin
  if aImage.fImage is TpvSprite then begin
   aCanvas.Color:=fImageSignedDistanceFieldColor;
   aCanvas.DrawSprite(TpvSprite(aImage.fImage),
                      TpvRect.CreateRelative(TpvVector2.Null,
                                             TpvVector2.Create(TpvSprite(aImage.fImage).Width,TpvSprite(aImage.fImage).Height)),
                      TpvRect.CreateRelative(TpvVector2.Null,aImage.fSize));
  end else if aImage.fImage is TpvVulkanTexture then begin
   aCanvas.DrawTexturedRectangle(TpvVulkanTexture(aImage.fImage),
                                 TpvRect.CreateRelative(TpvVector2.Null,aImage.fSize));
  end;
 end;
end;

function TpvGUIDefaultVectorBasedSkin.GetLabelPreferredSize(const aLabel:TpvGUILabel):TpvVector2;
begin
 result:=Maximum(GetWidgetLayoutPreferredSize(aLabel),
                 aLabel.Font.TextSize(aLabel.fCaption,aLabel.FontSize)+TpvVector2.Create(0.0,0.0));
 if aLabel.fFixedSize.x>0.0 then begin
  result.x:=aLabel.fFixedSize.x;
 end;
 if aLabel.fFixedSize.y>0.0 then begin
  result.y:=aLabel.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawLabel(const aCanvas:TpvCanvas;const aLabel:TpvGUILabel);
var Offset:TpvVector2;
begin
 aCanvas.ModelMatrix:=aLabel.fModelMatrix;
 aCanvas.ClipRect:=aLabel.fClipRect;
 aCanvas.Font:=aLabel.Font;
 aCanvas.FontSize:=aLabel.FontSize;
 case aLabel.TextHorizontalAlignment of
  pvgtaLeading:begin
   aCanvas.TextHorizontalAlignment:=pvcthaLeading;
   Offset.x:=0.0;
  end;
  pvgtaCenter:begin
   aCanvas.TextHorizontalAlignment:=pvcthaCenter;
   Offset.x:=aLabel.fSize.x*0.5;
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextHorizontalAlignment:=pvcthaTailing;
   Offset.x:=aLabel.fSize.x;
  end;
 end;
 case aLabel.TextVerticalAlignment of
  pvgtaLeading:begin
   aCanvas.TextVerticalAlignment:=pvctvaLeading;
   Offset.y:=0.0;
  end;
  pvgtaCenter:begin
   aCanvas.TextVerticalAlignment:=pvctvaMiddle;
   Offset.y:=aLabel.fSize.y*0.5;
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextVerticalAlignment:=pvctvaTailing;
   Offset.y:=aLabel.fSize.y;
  end;
 end;
 if aLabel.Enabled then begin
  aCanvas.Color:=aLabel.FontColor;
 end else begin
  aCanvas.Color:=TpvVector4.Create(aLabel.FontColor.rgb,aLabel.FontColor.a*0.25);
 end;
 aCanvas.DrawText(TpvGUITextUtils.TextTruncation(aLabel.fCaption,
                                                 aLabel.fTextTruncation,
                                                 aCanvas.Font,
                                                 aCanvas.FontSize,
                                                 aLabel.fSize.x),
                  Offset);
end;

function TpvGUIDefaultVectorBasedSkin.GetButtonPreferredSize(const aButton:TpvGUIButton):TpvVector2;
var TextSize,IconSize,TemporarySize,ChevronIconSize:TpvVector2;
    ChevronIcon:TpvSprite;
begin
 TextSize:=aButton.Font.TextSize(aButton.fCaption,FontSize);
 if aButton is TpvGUIPopupButton then begin
  case TpvGUIPopupButton(aButton).fPopup.fAnchorSide of
   pvgpasLeft:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronRight);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronLeft);
    end;
   end;
   pvgpasRight:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronLeft);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronRight);
    end;
   end;
   pvgpasTop:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronDown);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronUp);
    end;
   end;
   pvgpasBottom:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronUp);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronDown);
    end;
   end;
   else {pvgpasNone:}begin
    ChevronIcon:=nil;
   end;
  end;
 end else if aButton is TpvGUIPopupMenuButton then begin
  if aButton.Down then begin
   ChevronIcon:=TpvSprite(fIconChevronUp);
  end else begin
   ChevronIcon:=TpvSprite(fIconChevronDown);
  end;
 end else begin
  ChevronIcon:=nil;
 end;
 if assigned(aButton.fIcon) then begin
  if aButton.fIcon is TpvSprite then begin
   IconSize:=TpvVector2.Create(TpvSprite(aButton.fIcon).Width,TpvSprite(aButton.fIcon).Height);
  end else if aButton.fIcon is TpvVulkanTexture then begin
   IconSize:=TpvVector2.Create(TpvVulkanTexture(aButton.fIcon).Width,TpvVulkanTexture(aButton.fIcon).Height);
  end else begin
   IconSize:=TpvVector2.Null;
  end;
  if aButton.fIconHeight>0.0 then begin
   IconSize.x:=(IconSize.x*aButton.fIconHeight)/IconSize.y;
   IconSize.y:=aButton.fIconHeight;
  end;
 end else begin
  IconSize:=TpvVector2.Null;
 end;
 if assigned(ChevronIcon) then begin
  ChevronIconSize.x:=(ChevronIcon.Width*fIconChevronHeight)/ChevronIcon.Height;
  if (length(aButton.fCaption)>0) or (IconSize.x>0.0) then begin
   ChevronIconSize.x:=ChevronIconSize.x+ButtonIconSpacing;
  end;
  ChevronIconSize.y:=fIconChevronHeight;
 end else begin
  ChevronIconSize:=TpvVector2.Null;
 end;
 if (length(aButton.fCaption)>0) and (IconSize.x>0.0) then begin
  TextSize.x:=TextSize.x+ButtonIconSpacing;
 end;
 TemporarySize.x:=TextSize.x+IconSize.x+ChevronIconSize.x;
 TemporarySize.y:=Max(TextSize.y,Maximum(IconSize.y,ChevronIconSize.y));
 result:=Maximum(GetWidgetLayoutPreferredSize(aButton),
                 TemporarySize+TpvVector2.Create(ButtonHorizontalBorderSpacing*2.0,10.0));
 if aButton.fFixedSize.x>0.0 then begin
  result.x:=aButton.fFixedSize.x;
 end;
 if aButton.fFixedSize.y>0.0 then begin
  result.y:=aButton.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawButton(const aCanvas:TpvCanvas;const aButton:TpvGUIButton);
var Offset,TextOffset:TpvVector2;
    TextSize,IconSize,TemporarySize,ChevronIconSize:TpvVector2;
    ButtonRect,TextRect,IconRect,ChevronIconRect:TpvRect;
    SpriteWidth:TpvFloat;
    ChevronIcon:TpvSprite;
begin

 if aButton.Down then begin
  Offset:=TpvVector2.Create(-0.5,-0.5);
 end else begin
  Offset:=TpvVector2.Null;
 end;

 ButtonRect:=TpvRect.CreateRelative(TpvVector2.Create(ButtonHorizontalBorderSpacing,0.0),
                                    aButton.fSize-TpvVector2.Create(ButtonHorizontalBorderSpacing*2.0,0.0));

 if aButton is TpvGUIPopupButton then begin
  case TpvGUIPopupButton(aButton).fPopup.fAnchorSide of
   pvgpasLeft:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronRight);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronLeft);
    end;
   end;
   pvgpasRight:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronLeft);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronRight);
    end;
   end;
   pvgpasTop:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronDown);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronUp);
    end;
   end;
   pvgpasBottom:begin
    if aButton.Down then begin
     ChevronIcon:=TpvSprite(fIconChevronUp);
    end else begin
     ChevronIcon:=TpvSprite(fIconChevronDown);
    end;
   end;
   else {pvgpasNone:}begin
    ChevronIcon:=nil;
   end;
  end;
 end else if aButton is TpvGUIPopupMenuButton then begin
  if aButton.Down then begin
   ChevronIcon:=TpvSprite(fIconChevronUp);
  end else begin
   ChevronIcon:=TpvSprite(fIconChevronDown);
  end;
 end else begin
  ChevronIcon:=nil;
 end;

 if assigned(ChevronIcon) then begin
  ChevronIconSize.x:=(ChevronIcon.Width*fIconChevronHeight)/ChevronIcon.Height;
  ChevronIconSize.y:=fIconChevronHeight;
  if (aButton is TpvGUIPopupButton) and
     (TpvGUIPopupButton(aButton).fPopup.fAnchorSide=pvgpasLeft) then begin
   ChevronIconRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+TpvVector2.Create(0.0,
                                                                                (((ButtonRect.Height-ChevronIconSize.y)*0.5)))+Offset,
                                           ChevronIconSize);
   ButtonRect.Left:=ButtonRect.Left+(ChevronIconSize.x+ButtonIconSpacing);
  end else begin
   ChevronIconRect:=TpvRect.CreateRelative(TpvVector2.Create(ButtonRect.Right-ChevronIconSize.x,
                                                             ButtonRect.Top+(((ButtonRect.Height-ChevronIconSize.y)*0.5)))+Offset,
                                           ChevronIconSize);
   ButtonRect.Right:=ButtonRect.Right-(ChevronIconSize.x+ButtonIconSpacing);
  end;
 end else begin
  ChevronIconSize:=TpvVector2.Null;
  ChevronIconRect:=TpvRect.CreateRelative(TpvVector2.Null,
                                          TpvVector2.Null);
 end;

 aCanvas.ModelMatrix:=aButton.fModelMatrix;
 aCanvas.ClipRect:=aButton.fClipRect;

 if not aButton.Enabled then begin

  aCanvas.DrawGUIElement(GUI_ELEMENT_BUTTON_DISABLED,
                         true,
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y),
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y));

 end else if aButton.Down then begin

  aCanvas.DrawGUIElement(GUI_ELEMENT_BUTTON_PUSHED,
                         true,
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y),
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y));

 end else if aButton.Focused then begin

  aCanvas.DrawGUIElement(GUI_ELEMENT_BUTTON_FOCUSED,
                         true,
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y),
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y));

 end else begin

  aCanvas.DrawGUIElement(GUI_ELEMENT_BUTTON_UNFOCUSED,
                         true,
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y),
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aButton.fSize.x,aButton.fSize.y));

 end;

 TextSize:=aButton.Font.TextSize(aButton.fCaption,aButton.FontSize);

 if assigned(aButton.fIcon) then begin
  if aButton.fIcon is TpvSprite then begin
   IconSize:=TpvVector2.Create(TpvSprite(aButton.fIcon).Width,TpvSprite(aButton.fIcon).Height);
  end else if aButton.fIcon is TpvVulkanTexture then begin
   IconSize:=TpvVector2.Create(TpvVulkanTexture(aButton.fIcon).Width,TpvVulkanTexture(aButton.fIcon).Height);
  end else begin
   IconSize:=TpvVector2.Null;
  end;
  if aButton.fIconHeight>0.0 then begin
   IconSize.x:=(IconSize.x*aButton.fIconHeight)/IconSize.y;
   IconSize.y:=aButton.fIconHeight;
  end;
 end else begin
  IconSize:=TpvVector2.Null;
 end;

 if IconSize.x>0.0 then begin

  if length(aButton.fCaption)>0 then begin

   TemporarySize.x:=TextSize.x+IconSize.x+ButtonIconSpacing;
   TemporarySize.y:=Max(TextSize.y,Max(IconSize.y,ChevronIconSize.y));

   case aButton.fIconPosition of
    pvgbipLeft:begin
     IconRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+TpvVector2.Create(0.0,
                                                                           (ButtonRect.Height-IconSize.y)*0.5),
                                      IconSize);
     TextRect:=TpvRect.CreateAbsolute(ButtonRect.LeftTop+TpvVector2.Create(IconSize.x+ButtonIconSpacing,0.0),
                                      ButtonRect.RightBottom);
    end;
    pvgbipLeftCentered:begin
     IconRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+TpvVector2.Create((ButtonRect.Width-TemporarySize.x)*0.5,
                                                                           (ButtonRect.Height-IconSize.y)*0.5),
                                      IconSize);
     TextRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+TpvVector2.Create(((ButtonRect.Width-TemporarySize.x)*0.5)+IconSize.x+ButtonIconSpacing,
                                                                           0.0),
                                      TpvVector2.Create(TextSize.x,ButtonRect.Height));
    end;
    pvgbipRightCentered:begin
     IconRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+TpvVector2.Create(((ButtonRect.Width-TemporarySize.x)*0.5)+TextSize.x+ButtonIconSpacing,
                                                                           (ButtonRect.Height-IconSize.y)*0.5),
                                      IconSize);
     TextRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+TpvVector2.Create((ButtonRect.Width-TemporarySize.x)*0.5,0.0),
                                      TpvVector2.Create(TextSize.x,ButtonRect.Height));
    end;
    else {pvgbipRight:}begin
     IconRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+TpvVector2.Create(ButtonRect.Width-IconSize.x,
                                                                           (ButtonRect.Height-IconSize.y)*0.5),
                                      IconSize);
     TextRect:=TpvRect.CreateRelative(ButtonRect.LeftTop,
                                      ButtonRect.Size-TpvVector2.Create(IconSize.x+ButtonIconSpacing,0.0));
    end;
   end;

  end else begin

   IconRect:=TpvRect.CreateRelative(ButtonRect.LeftTop+((ButtonRect.Size-IconSize)*0.5),IconSize);

   TextRect:=TpvRect.CreateRelative(TpvVector2.Null,TpvVector2.Null);

  end;

 end else begin

  TextRect:=ButtonRect;

  IconRect:=TpvRect.CreateRelative(TpvVector2.Null,TpvVector2.Null);

 end;

 aCanvas.Font:=aButton.Font;
 aCanvas.FontSize:=aButton.FontSize;
 case aButton.TextHorizontalAlignment of
  pvgtaLeading:begin
   aCanvas.TextHorizontalAlignment:=pvcthaLeading;
   TextOffset.x:=fSpacing;
  end;
  pvgtaCenter:begin
   aCanvas.TextHorizontalAlignment:=pvcthaCenter;
   TextOffset.x:=TextRect.Size.x*0.5;
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextHorizontalAlignment:=pvcthaTailing;
   TextOffset.x:=TextRect.Size.x-fSpacing;
  end;
 end;
 case aButton.TextVerticalAlignment of
  pvgtaLeading:begin
   aCanvas.TextVerticalAlignment:=pvctvaLeading;
   TextOffset.y:=fSpacing;
  end;
  pvgtaCenter:begin
   aCanvas.TextVerticalAlignment:=pvctvaMiddle;
   TextOffset.y:=TextRect.Size.y*0.5;
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextVerticalAlignment:=pvctvaTailing;
   TextOffset.y:=TextRect.Size.y-fSpacing;
  end;
 end;

 if assigned(aButton.fIcon) then begin
  if aButton.Enabled then begin
   aCanvas.Color:=aButton.FontColor;
  end else begin
   aCanvas.Color:=TpvVector4.Create(aButton.FontColor.rgb,aButton.FontColor.a*0.25);
  end;
  if aButton.fIcon is TpvSprite then begin
   aCanvas.DrawSprite(TpvSprite(aButton.fIcon),
                      TpvRect.CreateRelative(TpvVector2.Null,
                                             TpvVector2.Create(TpvSprite(aButton.fIcon).Width,TpvSprite(aButton.fIcon).Height)),
                      TpvRect.CreateRelative(Offset+IconRect.LeftTop,IconRect.Size));
  end else if aButton.fIcon is TpvVulkanTexture then begin
   aCanvas.DrawTexturedRectangle(TpvVulkanTexture(aButton.fIcon),
                                 Offset+IconRect.LeftTop+((IconRect.RightBottom-IconRect.LeftTop)*0.5),
                                 (IconRect.RightBottom-IconRect.LeftTop)*0.5);
  end;
 end;
 if aButton.Enabled then begin
  aCanvas.Color:=aButton.FontColor;
 end else begin
  aCanvas.Color:=TpvVector4.Create(aButton.FontColor.rgb,aButton.FontColor.a*0.25);
 end;
 aCanvas.Font:=aButton.Font;
 aCanvas.FontSize:=aButton.FontSize;
 aCanvas.DrawText(TpvGUITextUtils.TextTruncation(aButton.fCaption,
                                                 aButton.fTextTruncation,
                                                 aCanvas.Font,
                                                 aCanvas.FontSize,
                                                 ButtonRect.Width+(ButtonHorizontalBorderSpacing*2.0)),
                  Offset+TextRect.LeftTop+TextOffset);

 if assigned(ChevronIcon) then begin
  aCanvas.DrawSprite(ChevronIcon,
                     TpvRect.CreateRelative(TpvVector2.Null,
                                            TpvVector2.Create(ChevronIcon.Width,ChevronIcon.Height)),
                     ChevronIconRect);
 end;


end;

function TpvGUIDefaultVectorBasedSkin.GetCheckBoxPreferredSize(const aCheckBox:TpvGUICheckBox):TpvVector2;
begin
 result:=Maximum(Maximum(GetWidgetLayoutPreferredSize(aCheckBox),
                         aCheckBox.Font.TextSize(aCheckBox.fCaption,aCheckBox.FontSize)+
                         TpvVector2.Create(fCheckBoxSize.x+fSpacing,0.0)),
                 fCheckBoxSize);
 if aCheckBox.fFixedSize.x>0.0 then begin
  result.x:=aCheckBox.fFixedSize.x;
 end;
 if aCheckBox.fFixedSize.y>0.0 then begin
  result.y:=aCheckBox.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawCheckBox(const aCanvas:TpvCanvas;const aCheckBox:TpvGUICheckBox);
var Element:TpvInt32;
    Offset:TpvVector2;
    Icon:TpvSprite;
begin

 aCanvas.ModelMatrix:=aCheckBox.fModelMatrix;
 aCanvas.ClipRect:=aCheckBox.fClipRect;

 if aCheckBox.Enabled then begin
  aCanvas.Color:=aCheckBox.FontColor;
 end else begin
  aCanvas.Color:=TpvVector4.Create(aCheckBox.FontColor.rgb,aCheckBox.FontColor.a*0.25);
 end;

 if not aCheckBox.Enabled then begin

  Element:=GUI_ELEMENT_BOX_DARK_DISABLED;

 end else if aCheckBox.Focused then begin

  Element:=GUI_ELEMENT_BOX_DARK_FOCUSED;

 end else begin

  Element:=GUI_ELEMENT_BOX_DARK_UNFOCUSED;

 end;

 Offset:=TpvVector2.Create(0.0,(aCheckBox.fSize.y-fCheckBoxSize.y)*0.5);

 aCanvas.DrawGUIElement(Element,
                        aCheckBox.Focused,
                        Offset,
                        Offset+fCheckBoxSize,
                        Offset,
                        Offset+fCheckBoxSize);

 if aCheckBox.Checked then begin

  if pvgcbfRadioCheckBox in aCheckBox.fCheckBoxFlags then begin
   Icon:=TpvSprite(fIconRoundCheck);
  end else begin
   Icon:=TpvSprite(fIconCheck);
  end;

  aCanvas.DrawSprite(Icon,
                     TpvRect.CreateRelative(0.0,0.0,Icon.Width,Icon.Height),
                     TpvRect.CreateRelative(Offset,fCheckBoxSize));

 end;

 aCanvas.Font:=aCheckBox.Font;
 aCanvas.FontSize:=aCheckBox.FontSize;
 case aCheckBox.TextHorizontalAlignment of
  pvgtaLeading:begin
   aCanvas.TextHorizontalAlignment:=pvcthaLeading;
   Offset.x:=fCheckBoxSize.x+fSpacing;
  end;
  pvgtaCenter:begin
   aCanvas.TextHorizontalAlignment:=pvcthaCenter;
   Offset.x:=(fCheckBoxSize.x+fSpacing)+((aCheckBox.fSize.x-(fCheckBoxSize.x+fSpacing))*0.5);
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextHorizontalAlignment:=pvcthaTailing;
   Offset.x:=(fCheckBoxSize.x+fSpacing)+(aCheckBox.fSize.x-(fCheckBoxSize.x+fSpacing));
  end;
 end;
 case aCheckBox.TextVerticalAlignment of
  pvgtaLeading:begin
   aCanvas.TextVerticalAlignment:=pvctvaLeading;
   Offset.y:=0.0;
  end;
  pvgtaCenter:begin
   aCanvas.TextVerticalAlignment:=pvctvaMiddle;
   Offset.y:=aCheckBox.fSize.y*0.5;
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextVerticalAlignment:=pvctvaTailing;
   Offset.y:=aCheckBox.fSize.y;
  end;
 end;
 aCanvas.DrawText(TpvGUITextUtils.TextTruncation(aCheckBox.fCaption,
                                                 aCheckBox.fTextTruncation,
                                                 aCanvas.Font,
                                                 aCanvas.FontSize,
                                                 aCheckBox.fSize.x-(fCheckBoxSize.x+fSpacing)),
                  Offset);

end;

function TpvGUIDefaultVectorBasedSkin.GetTextEditPreferredSize(const aTextEdit:TpvGUITextEdit):TpvVector2;
var TextSize:TpvVector2;
begin
 TextSize.x:=4*2;
 TextSize.y:=(aTextEdit.Font.RowHeight(100,aTextEdit.GetFontSize))+(4*2);
 result:=Maximum(GetWidgetLayoutPreferredSize(aTextEdit),
                 Maximum(TextSize,
                         TpvVector2.Create(aTextEdit.fMinimumWidth,aTextEdit.fMinimumHeight)));
 if aTextEdit.fFixedSize.x>0.0 then begin
  result.x:=aTextEdit.fFixedSize.x;
 end;
 if aTextEdit.fFixedSize.y>0.0 then begin
  result.y:=aTextEdit.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawTextEdit(const aCanvas:TpvCanvas;const aTextEdit:TpvGUITextEdit);
var Offset,TextOffset:TpvVector2;
    TextSize,IconSize,TemporarySize:TpvVector2;
    TextRect,IconRect,TextClipRect,SelectionRect:TpvRect;
    TextCursorPositionIndex,
    PreviousCursorPosition,NextCursorPosition,StartIndex,EndIndex:TpvInt32;
    PreviousCursorX,NextCursorX:TpvFloat;
    IconSprite:TpvSprite;
begin

 Offset:=TpvVector2.Null;

 aCanvas.ModelMatrix:=aTextEdit.fModelMatrix;
 aCanvas.ClipRect:=aTextEdit.fClipRect;

 if not aTextEdit.Enabled then begin

  aCanvas.DrawGUIElement(GUI_ELEMENT_BOX_DISABLED,
                         true,
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aTextEdit.fSize.x,aTextEdit.fSize.y),
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aTextEdit.fSize.x,aTextEdit.fSize.y));

 end else if aTextEdit.Focused then begin

  aCanvas.DrawGUIElement(GUI_ELEMENT_BOX_FOCUSED,
                         true,
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aTextEdit.fSize.x,aTextEdit.fSize.y),
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aTextEdit.fSize.x,aTextEdit.fSize.y));

 end else begin

  aCanvas.DrawGUIElement(GUI_ELEMENT_BOX_UNFOCUSED,
                         true,
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aTextEdit.fSize.x,aTextEdit.fSize.y),
                         TpvVector2.Create(0.0,0.0),
                         TpvVector2.Create(aTextEdit.fSize.x,aTextEdit.fSize.y));

 end;

 if ((aTextEdit is TpvGUIIntegerEdit) or
     (aTextEdit is TpvGUIFloatEdit)) and
     aTextEdit.fSpinnable then begin

  IconSprite:=TpvSprite(fIconArrowUpDown);

  IconSize:=TpvVector2.Create(IconSprite.Width*(aTextEdit.fSize.y-16.0)/IconSprite.Height,
                              aTextEdit.fSize.y-16.0);

  IconRect:=TpvRect.CreateRelative(TpvVector2.Create(aTextEdit.fSize.x-(IconSize.x+4.0),
                                                     (aTextEdit.fSize.y-IconSize.y)*0.5),
                                   IconSize);

  TextRect:=TpvRect.CreateRelative(TpvVector2.Create(2.0,2.0),
                                   aTextEdit.fSize-TpvVector2.Create(IconSize.x+8.0,4.0));

 end else begin

  IconSize:=TpvVector2.Null;

  IconSprite:=nil;

  IconRect:=TpvRect.CreateRelative(TpvVector2.Null,TpvVector2.Null);

  TextRect:=TpvRect.CreateRelative(TpvVector2.Create(2.0,2.0),
                                   aTextEdit.fSize-TpvVector2.Create(4.0,4.0));

 end;

 aTextEdit.fDragRect:=IconRect;

 aCanvas.Font:=aTextEdit.Font;
 aCanvas.FontSize:=aTextEdit.FontSize;
 case aTextEdit.TextHorizontalAlignment of
  pvgtaLeading:begin
   aCanvas.TextHorizontalAlignment:=pvcthaLeading;
   TextOffset.x:=fSpacing;
  end;
  pvgtaCenter:begin
   aCanvas.TextHorizontalAlignment:=pvcthaCenter;
   TextOffset.x:=TextRect.Size.x*0.5;
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextHorizontalAlignment:=pvcthaTailing;
   TextOffset.x:=TextRect.Size.x-fSpacing;
  end;
 end;
 case aTextEdit.TextVerticalAlignment of
  pvgtaLeading:begin
   aCanvas.TextVerticalAlignment:=pvctvaLeading;
   TextOffset.y:=fSpacing;
  end;
  pvgtaCenter:begin
   aCanvas.TextVerticalAlignment:=pvctvaMiddle;
   TextOffset.y:=TextRect.Size.y*0.5;
  end;
  else {pvgtaTailing:}begin
   aCanvas.TextVerticalAlignment:=pvctvaTailing;
   TextOffset.y:=TextRect.Size.y-fSpacing;
  end;
 end;

{if (length(aTextEdit.fIconText)>0) and assigned(aButton.fIconFont) then begin
  if aButton.Enabled then begin
   aCanvas.Color:=aButton.FontColor;
  end else begin
   aCanvas.Color:=TpvVector4.Create(aButton.FontColor.rgb,aButton.FontColor.a*0.25);
  end;
  aCanvas.Font:=aButton.fIconFont;
  aCanvas.FontSize:=aButton.fIconFontSize;
  aCanvas.DrawText(aButton.fIconText,IconRect.LeftTop);
 end else if assigned(aButton.fIcon) then begin
  if aButton.fIcon is TpvSprite then begin
   aCanvas.DrawSprite(TpvSprite(aButton.fIcon),
                      TpvRect.CreateRelative(TpvVector2.Null,
                                             TpvVector2.Create(TpvSprite(aButton.fIcon).Width,TpvSprite(aButton.fIcon).Height)),
                      TpvRect.CreateRelative(Offset+IconRect.LeftTop,
                                             TpvVector2.Create(TpvSprite(aButton.fIcon).Width,TpvSprite(aButton.fIcon).Height)));
  end else if aButton.fIcon is TpvVulkanTexture then begin
   aCanvas.DrawTexturedRectangle(TpvVulkanTexture(aButton.fIcon),
                                 Offset+IconRect.LeftTop+(TpvVector2.Create(TpvVulkanTexture(aButton.fIcon).Width,TpvVulkanTexture(aButton.fIcon).Height)*0.5),
                                 TpvVector2.Create(TpvVulkanTexture(aButton.fIcon).Width,TpvVulkanTexture(aButton.fIcon).Height)*0.5);
  end;
 end;  }

 TextClipRect:=TpvRect.CreateAbsolute(aTextEdit.fClipRect.Left+2,
                                      aTextEdit.fClipRect.Top+2,
                                      aTextEdit.fClipRect.Right-2,
                                      aTextEdit.fClipRect.Bottom-2);

 aCanvas.ClipRect:=TextClipRect;

 TextClipRect.LeftTop:=TextClipRect.LeftTop-aTextEdit.fClipRect.LeftTop;
 TextClipRect.RightBottom:=TextClipRect.RightBottom-aTextEdit.fClipRect.LeftTop;

 aCanvas.Font:=aTextEdit.Font;
 aCanvas.FontSize:=aTextEdit.FontSize;

 TextOffset.x:=TextOffset.x+aTextEdit.fTextOffset;

 aCanvas.TextGlyphRects(aTextEdit.fText,Offset+TextRect.LeftTop+TextOffset,aTextEdit.fTextGlyphRects,aTextEdit.fCountTextGlyphRects);

 if aTextEdit.fCountTextGlyphRects>0 then begin

  PreviousCursorPosition:=Min(Max(aTextEdit.fTextCursorPositionIndex-1,1),aTextEdit.fCountTextGlyphRects+1);
  NextCursorPosition:=Min(Max(aTextEdit.fTextCursorPositionIndex+1,1),aTextEdit.fCountTextGlyphRects+1);
  if PreviousCursorPosition>aTextEdit.fCountTextGlyphRects then begin
   PreviousCursorX:=aTextEdit.fTextGlyphRects[aTextEdit.fCountTextGlyphRects-1].Right;
  end else begin
   PreviousCursorX:=aTextEdit.fTextGlyphRects[PreviousCursorPosition-1].Left;
  end;
  if NextCursorPosition>aTextEdit.fCountTextGlyphRects then begin
   NextCursorX:=aTextEdit.fTextGlyphRects[aTextEdit.fCountTextGlyphRects-1].Right;
  end else begin
   NextCursorX:=aTextEdit.fTextGlyphRects[NextCursorPosition-1].Left;
  end;

  if NextCursorX>(TextRect.Right-2.0) then begin
   aTextEdit.fTextOffset:=aTextEdit.fTextOffset-((NextCursorX-(TextRect.Right-2.0))+1.0);
   TextOffset.x:=TextOffset.x-((NextCursorX-(TextRect.Right-2.0))+1.0);
  end;
  if PreviousCursorX<TextRect.Left then begin
   aTextEdit.fTextOffset:=aTextEdit.fTextOffset+((TextRect.Left-PreviousCursorX)+1.0);
   TextOffset.x:=TextOffset.x+((TextRect.Left-PreviousCursorX)+1.0);
  end;

 end;

 aCanvas.TextGlyphRects(aTextEdit.fText,Offset+TextRect.LeftTop+TextOffset,aTextEdit.fTextGlyphRects,aTextEdit.fCountTextGlyphRects);

 if (aTextEdit.fCountTextGlyphRects>16) and
    ((length(aTextEdit.fTextGlyphRects) shl 1)>=aTextEdit.fCountTextGlyphRects) then begin
  SetLength(aTextEdit.fTextGlyphRects,aTextEdit.fCountTextGlyphRects);
 end;

 if (aTextEdit.fTextSelectionStart>0) and
    (aTextEdit.fTextSelectionStart<=(aTextEdit.fCountTextGlyphRects+1)) and
    (aTextEdit.fTextSelectionEnd>0) and
    (aTextEdit.fTextSelectionEnd<=(aTextEdit.fCountTextGlyphRects+1)) then begin
  aCanvas.Color:=TpvVector4.Create(0.016275,0.016275,0.016275,1.0);
  StartIndex:=Min(aTextEdit.fTextSelectionStart,aTextEdit.fTextSelectionEnd)-1;
  EndIndex:=Max(aTextEdit.fTextSelectionStart,aTextEdit.fTextSelectionEnd)-1;
  if StartIndex>=aTextEdit.fCountTextGlyphRects then begin
   SelectionRect.Left:=Maximum(aTextEdit.fTextGlyphRects[aTextEdit.fCountTextGlyphRects-1].Right,
                               Offset.x+TextRect.Left+TextOffset.x+aCanvas.TextWidth(aTextEdit.fText))+1.0;
  end else begin
   SelectionRect.Left:=aTextEdit.fTextGlyphRects[StartIndex].Left+1.0;
  end;
  if EndIndex>=aTextEdit.fCountTextGlyphRects then begin
   SelectionRect.Right:=Maximum(aTextEdit.fTextGlyphRects[aTextEdit.fCountTextGlyphRects-1].Right,
                                Offset.x+TextRect.Left+TextOffset.x+aCanvas.TextWidth(aTextEdit.fText))+1.0;
  end else begin
   SelectionRect.Right:=aTextEdit.fTextGlyphRects[EndIndex].Left+1.0;
  end;
  SelectionRect.Top:=(Offset.y+TextRect.Top)+2;
  SelectionRect.Bottom:=(Offset.y+TextRect.Bottom)-2;
  aCanvas.DrawFilledRectangle((SelectionRect.LeftTop+SelectionRect.RightBottom)*0.5,
                              (SelectionRect.RightBottom-SelectionRect.LeftTop)*0.5);
 end;

 if aTextEdit.Enabled then begin
  aCanvas.Color:=aTextEdit.FontColor;
 end else begin
  aCanvas.Color:=TpvVector4.Create(aTextEdit.FontColor.rgb,aTextEdit.FontColor.a*0.25);
 end;

 aCanvas.DrawText(aTextEdit.fText,Offset+TextRect.LeftTop+TextOffset);

 if aTextEdit.Enabled and
    aTextEdit.Focused and
    (frac(fInstance.fTime)<0.5) then begin
  if aTextEdit.fCountTextGlyphRects>0 then begin
   TextCursorPositionIndex:=Min(Max(aTextEdit.fTextCursorPositionIndex,1),aTextEdit.fCountTextGlyphRects+1);
   if TextCursorPositionIndex>aTextEdit.fCountTextGlyphRects then begin
    aCanvas.DrawFilledRectangle(TpvVector2.Create(Max(aTextEdit.fTextGlyphRects[aTextEdit.fCountTextGlyphRects-1].Right,
                                                      Offset.x+TextRect.Left+TextOffset.x+aCanvas.TextWidth(aTextEdit.fText))+0.5,
                                                  Offset.y+TextRect.Top+(TextRect.Size.y*0.5)),
                                TpvVector2.Create(1.0,
                                                  (aCanvas.TextRowHeight(100.0)*0.5)));
   end else begin
    aCanvas.DrawFilledRectangle(TpvVector2.Create(aTextEdit.fTextGlyphRects[TextCursorPositionIndex-1].Left,
                                                  Offset.y+TextRect.Top+(TextRect.Size.y*0.5)),
                                TpvVector2.Create(1.0,
                                                  (aCanvas.TextRowHeight(100.0)*0.5)));
   end;
  end else begin
   aCanvas.DrawFilledRectangle(TpvVector2.Create(Offset.x+TextRect.Left+TextOffset.x,
                                                 Offset.y+TextRect.Top+(TextRect.Size.y*0.5)),
                               TpvVector2.Create(1.0,
                                                 (aCanvas.TextRowHeight(100.0)*0.5)));
  end;
 end;

 if assigned(IconSprite) then begin
  aCanvas.DrawSprite(IconSprite,
                     TpvRect.CreateRelative(TpvVector2.Null,
                                            TpvVector2.Create(IconSprite.Width,IconSprite.Height)),
                     IconRect);
 end;

end;

function TpvGUIDefaultVectorBasedSkin.ProcessPopupMenuItems(const aPopupMenu:TpvGUIPopupMenu):TpvVector2;
var Index,Element:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
    YOffset,MenuItemWidth,MenuItemHeight:TpvFloat;
    IconSize:TpvVector2;
begin

 result:=TpvVector2.Null;

 aPopupMenu.fHasSubMenus:=false;

 YOffset:=2.0+fSpacing;

 for Index:=0 to aPopupMenu.Children.Count-1 do begin

  Child:=aPopupMenu.Children[Index];
  if Child is TpvGUIMenuItem then begin

   MenuItem:=TpvGUIMenuItem(Child);

   MenuItemWidth:=aPopupMenu.Font.TextWidth(MenuItem.fCaption,aPopupMenu.FontSize);

   if length(MenuItem.fShortcutHint)>0 then begin
    MenuItemWidth:=MenuItemWidth+(fSpacing*4.0)+aPopupMenu.Font.TextWidth(MenuItem.fShortcutHint,aPopupMenu.FontSize);
   end;

   if assigned(MenuItem.fIcon) then begin
    if MenuItem.fIcon is TpvSprite then begin
     IconSize:=TpvVector2.Create(TpvSprite(MenuItem.fIcon).Width,TpvSprite(MenuItem.fIcon).Height);
    end else if MenuItem.fIcon is TpvVulkanTexture then begin
     IconSize:=TpvVector2.Create(TpvVulkanTexture(MenuItem.fIcon).Width,TpvVulkanTexture(MenuItem.fIcon).Height);
    end else begin
     IconSize:=TpvVector2.Null;
    end;
    if MenuItem.fIconHeight>0.0 then begin
     IconSize.x:=(IconSize.x*MenuItem.fIconHeight)/IconSize.y;
     IconSize.y:=MenuItem.fIconHeight;
    end;
    MenuItemWidth:=MenuItemWidth+fSpacing+IconSize.x;
   end;

   if assigned(MenuItem.Menu) then begin
    MenuItemWidth:=MenuItemWidth+fSpacing+((TpvSprite(fIconMenuRight).Width*fIconMenuRightHeight)/TpvSprite(fIconMenuRight).Height);
    aPopupMenu.fHasSubMenus:=true;
   end;

   MenuItemWidth:=MenuItemWidth+((10.0+fSpacing)*2.0);

   result.x:=Maximum(result.x,MenuItemWidth);

  end;

 end;

 for Index:=0 to aPopupMenu.Children.Count-1 do begin

  Child:=aPopupMenu.Children[Index];
  if Child is TpvGUIMenuItem then begin

   MenuItem:=TpvGUIMenuItem(Child);

   if MenuItem.fCaption='-' then begin

    MenuItemHeight:=fSpacing;

    MenuItem.fRect:=TpvRect.CreateAbsolute(TpvVector2.Create(2.0+fSpacing,YOffset),
                                           TpvVector2.Create(result.x-(2.0+fSpacing),YOffset+MenuItemHeight));

   end else begin

    MenuItemHeight:=aPopupMenu.Font.TextHeight(MenuItem.fCaption,aPopupMenu.FontSize);

    if length(MenuItem.fShortcutHint)>0 then begin
     MenuItemHeight:=Maximum(MenuItemHeight,aPopupMenu.Font.TextHeight(MenuItem.fShortcutHint,aPopupMenu.FontSize));
    end;

    if assigned(MenuItem.Menu) then begin
     MenuItemHeight:=Maximum(MenuItemHeight,fIconMenuRightHeight);
    end;

    if assigned(MenuItem.fIcon) then begin
     if MenuItem.fIcon is TpvSprite then begin
      IconSize:=TpvVector2.Create(TpvSprite(MenuItem.fIcon).Width,TpvSprite(MenuItem.fIcon).Height);
     end else if MenuItem.fIcon is TpvVulkanTexture then begin
      IconSize:=TpvVector2.Create(TpvVulkanTexture(MenuItem.fIcon).Width,TpvVulkanTexture(MenuItem.fIcon).Height);
     end else begin
      IconSize:=TpvVector2.Null;
     end;
     if MenuItem.fIconHeight>0.0 then begin
      IconSize.x:=(IconSize.x*MenuItem.fIconHeight)/IconSize.y;
      IconSize.y:=MenuItem.fIconHeight;
     end;
     MenuItemHeight:=Maximum(MenuItemHeight,fSpacing+IconSize.y);
    end;

    MenuItemHeight:=MenuItemHeight+10.0;

    MenuItem.fRect:=TpvRect.CreateAbsolute(TpvVector2.Create(2.0+fSpacing,YOffset),
                                           TpvVector2.Create(result.x-(2.0+fSpacing),YOffset+MenuItemHeight));

   end;

   MenuItem.fOpenRect:=MenuItem.fRect;
   MenuItem.fOpenRect.Top:=MenuItem.fOpenRect.Top-fSpacing;
   MenuItem.fOpenRect.Bottom:=MenuItem.fOpenRect.Bottom-fSpacing;

   YOffset:=YOffset+MenuItemHeight+fSpacing;

   result.y:=Maximum(result.y,YOffset+2.0);

  end;

 end;

 aPopupMenu.fSize:=result;

end;

function TpvGUIDefaultVectorBasedSkin.GetPopupMenuPreferredSize(const aPopupMenu:TpvGUIPopupMenu):TpvVector2;
begin
 result:=ProcessPopupMenuItems(aPopupMenu);
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawPopupMenu(const aCanvas:TpvCanvas;const aPopupMenu:TpvGUIPopupMenu);
var Index,Element:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
    Offset,IconSize:TpvVector2;
    XOffset,SpriteWidth:TpvFloat;
begin

 ProcessPopupMenuItems(aPopupMenu);

 aCanvas.ModelMatrix:=TpvMatrix4x4.CreateTranslation(aPopupMenu.fPosition);

 aCanvas.ClipRect:=aPopupMenu.fInstance.fClipRect;
 aCanvas.DrawGUIElement(GUI_ELEMENT_WINDOW_DROPSHADOW,
                        true,
                        TpvVector2.Create(-fWindowShadowWidth,-fWindowShadowHeight),
                        aPopupMenu.fSize+TpvVector2.Create(fWindowShadowWidth*2,fWindowShadowHeight*2),
                        TpvVector2.Create(0.0,0.0),
                        aPopupMenu.fSize);

 aCanvas.ClipRect:=TpvRect.CreateRelative(aPopupMenu.fPosition,aPopupMenu.fSize);

 aCanvas.DrawGUIElement(GUI_ELEMENT_WINDOW_FILL,
                        true,
                        TpvVector2.Create(0.0,0.0),
                        TpvVector2.Create(aPopupMenu.fSize.x,aPopupMenu.fSize.y),
                        TpvVector2.Create(0.0,0.0),
                        TpvVector2.Create(aPopupMenu.fSize.x,aPopupMenu.fSize.y));

 aCanvas.Font:=aPopupMenu.Font;
 aCanvas.FontSize:=aPopupMenu.FontSize;

 for Index:=0 to aPopupMenu.Children.Count-1 do begin

  Child:=aPopupMenu.Children[Index];
  if Child is TpvGUIMenuItem then begin

   MenuItem:=TpvGUIMenuItem(Child);

   Offset:=TpvVector2.Null;

   if MenuItem.Enabled then begin

    aCanvas.Color:=aPopupMenu.FontColor;

    if MenuItem.fCaption='-' then begin
     Element:=GUI_ELEMENT_BUTTON_UNFOCUSED;
    end else if aPopupMenu.fSelectedMenuItem=MenuItem then begin
     Element:=GUI_ELEMENT_BUTTON_PUSHED;
     Offset:=TpvVector2.Create(-0.5,-0.5);
    end else if aPopupMenu.fFocusedMenuItem=MenuItem then begin
     Element:=GUI_ELEMENT_BUTTON_FOCUSED;
    end else begin
     Element:=GUI_ELEMENT_BUTTON_UNFOCUSED;
    end;

   end else begin

    aCanvas.Color:=TpvVector4.Create(aPopupMenu.FontColor.rgb,aPopupMenu.FontColor.a*0.25);

    Element:=GUI_ELEMENT_BUTTON_DISABLED;

   end;

   aCanvas.DrawGUIElement(Element,
                          true,
                          MenuItem.fRect.LeftTop,
                          MenuItem.fRect.RightBottom,
                          MenuItem.fRect.LeftTop,
                          MenuItem.fRect.RightBottom);

   if MenuItem.fCaption<>'-' then begin

    aCanvas.TextHorizontalAlignment:=pvcthaLeading;

    XOffset:=MenuItem.fRect.Left+(4.0+fSpacing);

    if assigned(MenuItem.fIcon) then begin

     if MenuItem.fIcon is TpvSprite then begin
      IconSize:=TpvVector2.Create(TpvSprite(MenuItem.fIcon).Width,TpvSprite(MenuItem.fIcon).Height);
     end else if MenuItem.fIcon is TpvVulkanTexture then begin
      IconSize:=TpvVector2.Create(TpvVulkanTexture(MenuItem.fIcon).Width,TpvVulkanTexture(MenuItem.fIcon).Height);
     end else begin
      IconSize:=TpvVector2.Null;
     end;
     if MenuItem.fIconHeight>0.0 then begin
      IconSize.x:=(IconSize.x*MenuItem.fIconHeight)/IconSize.y;
      IconSize.y:=MenuItem.fIconHeight;
     end;

     if MenuItem.fIcon is TpvSprite then begin
      aCanvas.DrawSprite(TpvSprite(MenuItem.fIcon),
                         TpvRect.CreateRelative(TpvVector2.Null,
                                                TpvVector2.Create(TpvSprite(MenuItem.fIcon).Width,TpvSprite(MenuItem.fIcon).Height)),
                         TpvRect.CreateRelative(TpvVector2.Create(XOffset,((((MenuItem.fRect.Top+MenuItem.fRect.Bottom)-IconSize.y)*0.5)))+Offset,
                                                IconSize));
     end else if MenuItem.fIcon is TpvVulkanTexture then begin
      aCanvas.DrawTexturedRectangle(TpvVulkanTexture(MenuItem.fIcon),
                                    TpvVector2.Create(XOffset,((((MenuItem.fRect.Top+MenuItem.fRect.Bottom)-IconSize.y)*0.5)))+Offset,
                                    IconSize);
     end;

     XOffset:=XOffset+IconSize.x+fSpacing;

    end;

    aCanvas.TextVerticalAlignment:=pvctvaMiddle;
    aCanvas.DrawText(MenuItem.fCaption,TpvVector2.Create(XOffset,((MenuItem.fRect.Top+MenuItem.fRect.Bottom)*0.5))+Offset);

    XOffset:=MenuItem.fRect.Right-(4.0+fSpacing);

    if aPopupMenu.fHasSubMenus then begin
     SpriteWidth:=(TpvSprite(fIconMenuRight).Width*fIconMenuRightHeight)/TpvSprite(fIconMenuRight).Height;
     XOffset:=XOffset-SpriteWidth;
     if assigned(MenuItem.Menu) then begin
      aCanvas.DrawSprite(TpvSprite(fIconMenuRight),
                         TpvRect.CreateRelative(TpvVector2.Null,
                                                TpvVector2.Create(TpvSprite(fIconMenuRight).Width,TpvSprite(fIconMenuRight).Height)),
                         TpvRect.CreateRelative(TpvVector2.Create(XOffset,((((MenuItem.fRect.Top+MenuItem.fRect.Bottom)-fIconMenuRightHeight)*0.5)))+Offset,
                                                TpvVector2.Create(SpriteWidth,fIconMenuRightHeight)));
     end;
     XOffset:=XOffset-fSpacing;
    end;

    if length(MenuItem.fShortcutHint)>0 then begin
     aCanvas.TextHorizontalAlignment:=pvcthaTailing;
     aCanvas.DrawText(MenuItem.fShortcutHint,TpvVector2.Create(XOffset,((MenuItem.fRect.Top+MenuItem.fRect.Bottom)*0.5))+Offset);
    end;

    if aPopupMenu.fHoveredMenuItem=MenuItem then begin

     aCanvas.DrawGUIElement(GUI_ELEMENT_HOVERED,
                            true,
                            MenuItem.fRect.LeftTop,
                            MenuItem.fRect.RightBottom,
                            MenuItem.fRect.LeftTop,
                            MenuItem.fRect.RightBottom);

    end else if aPopupMenu.fFocusedMenuItem=MenuItem then begin

     aCanvas.DrawGUIElement(GUI_ELEMENT_FOCUSED,
                            true,
                            MenuItem.fRect.LeftTop,
                            MenuItem.fRect.RightBottom,
                            MenuItem.fRect.LeftTop,
                            MenuItem.fRect.RightBottom);

    end;

   end;

  end;

 end;

end;

procedure TpvGUIDefaultVectorBasedSkin.ProcessWindowMenuItems(const aWindowMenu:TpvGUIWindowMenu);
var Index,Element:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
    XOffset,MenuItemWidth:TpvFloat;
begin

 XOffset:=2.0+fSpacing;

 for Index:=0 to aWindowMenu.Children.Count-1 do begin

  Child:=aWindowMenu.Children[Index];
  if Child is TpvGUIMenuItem then begin

   MenuItem:=TpvGUIMenuItem(Child);

   MenuItemWidth:=((4.0+fSpacing)*2)+aWindowMenu.Font.TextWidth(MenuItem.fCaption,aWindowMenu.FontSize);

   MenuItem.fRect:=TpvRect.CreateAbsolute(TpvVector2.Create(XOffset,4.0),
                                          TpvVector2.Create(XOffset+MenuItemWidth,aWindowMenu.fSize.y-4.0));

   MenuItem.fOpenRect:=MenuItem.fRect;

   XOffset:=XOffset+MenuItemWidth+fSpacing;

  end;

 end;

end;

function TpvGUIDefaultVectorBasedSkin.GetWindowMenuPreferredSize(const aWindowMenu:TpvGUIWindowMenu):TpvVector2;
begin
 ProcessWindowMenuItems(aWindowMenu);
 result:=Maximum(GetWidgetLayoutPreferredSize(aWindowMenu),
                 TpvVector2.Create(0.0,fWindowMenuHeight));
 if aWindowMenu.fFixedSize.x>0.0 then begin
  result.x:=aWindowMenu.fFixedSize.x;
 end;
 if aWindowMenu.fFixedSize.y>0.0 then begin
  result.y:=aWindowMenu.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawWindowMenu(const aCanvas:TpvCanvas;const aWindowMenu:TpvGUIWindowMenu);
var Index,Element:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
    Offset:TpvVector2;
begin

 ProcessWindowMenuItems(aWindowMenu);

 aCanvas.ModelMatrix:=aWindowMenu.fModelMatrix;
 aCanvas.ClipRect:=aWindowMenu.fClipRect;

 if aWindowMenu.Enabled then begin
  Element:=GUI_ELEMENT_PANEL_ENABLED;
 end else begin
  Element:=GUI_ELEMENT_PANEL_DISABLED;
 end;

 aCanvas.DrawGUIElement(Element,
                        true,
                        TpvVector2.Create(0.0,0.0),
                        TpvVector2.Create(aWindowMenu.fSize.x,aWindowMenu.fSize.y),
                        TpvVector2.Create(0.0,0.0),
                        TpvVector2.Create(aWindowMenu.fSize.x,aWindowMenu.fSize.y));

 aCanvas.Font:=aWindowMenu.Font;
 aCanvas.FontSize:=aWindowMenu.FontSize;

 aCanvas.TextHorizontalAlignment:=pvcthaCenter;
 aCanvas.TextVerticalAlignment:=pvctvaMiddle;

 for Index:=0 to aWindowMenu.Children.Count-1 do begin

  Child:=aWindowMenu.Children[Index];
  if Child is TpvGUIMenuItem then begin

   MenuItem:=TpvGUIMenuItem(Child);

   Offset:=TpvVector2.Null;

   if aWindowMenu.Enabled and MenuItem.Enabled then begin

    aCanvas.Color:=aWindowMenu.FontColor;

    if aWindowMenu.fSelectedMenuItem=MenuItem then begin
     Element:=GUI_ELEMENT_BUTTON_PUSHED;
     Offset:=TpvVector2.Create(-0.5,-0.5);
    end else if aWindowMenu.Focused and (aWindowMenu.fFocusedMenuItem=MenuItem) then begin
     Element:=GUI_ELEMENT_BUTTON_FOCUSED;
    end else begin
     Element:=GUI_ELEMENT_BUTTON_UNFOCUSED;
    end;

   end else begin

    aCanvas.Color:=TpvVector4.Create(aWindowMenu.FontColor.rgb,aWindowMenu.FontColor.a*0.25);

    Element:=GUI_ELEMENT_BUTTON_DISABLED;

   end;

   aCanvas.DrawGUIElement(Element,
                          true,
                          MenuItem.fRect.LeftTop,
                          MenuItem.fRect.RightBottom,
                          MenuItem.fRect.LeftTop,
                          MenuItem.fRect.RightBottom);

   aCanvas.DrawText(MenuItem.fCaption,((MenuItem.fRect.LeftTop+MenuItem.fRect.RightBottom)*0.5)+Offset);

   if aWindowMenu.PointerFocused and (aWindowMenu.fHoveredMenuItem=MenuItem) then begin

    aCanvas.DrawGUIElement(GUI_ELEMENT_HOVERED,
                           true,
                           MenuItem.fRect.LeftTop,
                           MenuItem.fRect.RightBottom,
                           MenuItem.fRect.LeftTop,
                           MenuItem.fRect.RightBottom);

   end else if aWindowMenu.Focused and (aWindowMenu.fFocusedMenuItem=MenuItem) then begin

    aCanvas.DrawGUIElement(GUI_ELEMENT_FOCUSED,
                           true,
                           MenuItem.fRect.LeftTop,
                           MenuItem.fRect.RightBottom,
                           MenuItem.fRect.LeftTop,
                           MenuItem.fRect.RightBottom);

   end;

  end;

 end;

end;

function TpvGUIDefaultVectorBasedSkin.GetScrollBarPreferredSize(const aScrollBar:TpvGUIScrollBar):TpvVector2;
begin
 case aScrollBar.fOrientation of
  pvgsboHorizontal:begin
   result:=TpvVector2.Create((aScrollBar.ButtonSize*2.0)+128,aScrollBar.ButtonSize);
  end;
  else {pvgsboVertical:}begin
   result:=TpvVector2.Create(aScrollBar.ButtonSize,(aScrollBar.ButtonSize*2.0)+128);
  end;
 end;
 result:=Maximum(GetWidgetLayoutPreferredSize(aScrollBar),
                 result);
 if aScrollBar.fFixedSize.x>0.0 then begin
  result.x:=aScrollBar.fFixedSize.x;
 end;
 if aScrollBar.fFixedSize.y>0.0 then begin
  result.y:=aScrollBar.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawScrollBar(const aCanvas:TpvCanvas;const aScrollBar:TpvGUIScrollBar);
const IconSpacer=0.0;
var Element:TpvInt32;
    Offset:TpvVector2;
    Sprite:TpvSprite;
    Rect:TpvRect;
begin

 aCanvas.ModelMatrix:=aScrollBar.fModelMatrix;
 aCanvas.ClipRect:=aScrollBar.fClipRect;

 if aScrollBar.Enabled then begin
  if aScrollBar.Focused xor (aScrollBar.fPushedSubWidget=pvgsbswSliderButton) then begin
   Element:=GUI_ELEMENT_BOX_DARK_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BOX_DARK_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BOX_DARK_DISABLED;
 end;
 case aScrollBar.fOrientation of
  pvgsboHorizontal:begin
   Offset:=TpvVector2.Create(aScrollBar.fButtonSize-2.0,0.0);
  end;
  else {pvgsboVertical:}begin
   Offset:=TpvVector2.Create(0.0,aScrollBar.fButtonSize-2.0);
  end;
 end;
 aCanvas.DrawGUIElement(Element,
                        true,
                        Offset,
                        TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fSize.y)-Offset,
                        Offset,
                        TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fSize.y)-Offset);

 if aScrollBar.Enabled then begin
  aCanvas.Color:=aScrollBar.FontColor;
  if aScrollBar.fPushedSubWidget=pvgsbswDecButton then begin
   Element:=GUI_ELEMENT_BUTTON_PUSHED;
  end else if aScrollBar.Focused and (aScrollBar.fFocusedSubWidget=pvgsbswDecButton) then begin
   Element:=GUI_ELEMENT_BUTTON_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BUTTON_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BUTTON_DISABLED;
  aCanvas.Color:=TpvVector4.Create(aScrollBar.FontColor.rgb,aScrollBar.FontColor.a*0.25);
 end;
 case aScrollBar.fOrientation of
  pvgsboHorizontal:begin
   aCanvas.DrawGUIElement(Element,
                          true,
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aScrollBar.fButtonSize,aScrollBar.fSize.y),
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aScrollBar.fButtonSize,aScrollBar.fSize.y));
   Sprite:=TpvSprite(fIconDirectionArrowLeft);
   aCanvas.DrawSprite(Sprite,
                      TpvRect.CreateRelative(0.0,0.0,Sprite.Width,Sprite.Height),
                      TpvRect.CreateAbsolute(TpvVector2.Create(IconSpacer,IconSpacer),
                                             TpvVector2.Create(aScrollBar.fButtonSize-IconSpacer,aScrollBar.fSize.y-IconSpacer)));
  end;
  else {pvgsboVertical:}begin
   aCanvas.DrawGUIElement(Element,
                          true,
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fButtonSize),
                          TpvVector2.Create(0.0,0.0),
                          TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fButtonSize));
   Sprite:=TpvSprite(fIconDirectionArrowUp);
   aCanvas.DrawSprite(Sprite,
                      TpvRect.CreateRelative(0.0,0.0,Sprite.Width,Sprite.Height),
                      TpvRect.CreateAbsolute(TpvVector2.Create(IconSpacer,IconSpacer),
                                             TpvVector2.Create(aScrollBar.fSize.x-IconSpacer,aScrollBar.fButtonSize-IconSpacer)));
  end;
 end;

 if aScrollBar.Enabled then begin
  aCanvas.Color:=aScrollBar.FontColor;
  if aScrollBar.fPushedSubWidget=pvgsbswIncButton then begin
   Element:=GUI_ELEMENT_BUTTON_PUSHED;
  end else if aScrollBar.Focused and (aScrollBar.fFocusedSubWidget=pvgsbswIncButton) then begin
   Element:=GUI_ELEMENT_BUTTON_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BUTTON_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BUTTON_DISABLED;
  aCanvas.Color:=TpvVector4.Create(aScrollBar.FontColor.rgb,aScrollBar.FontColor.a*0.25);
 end;
 case aScrollBar.fOrientation of
  pvgsboHorizontal:begin
   aCanvas.DrawGUIElement(Element,
                          true,
                          TpvVector2.Create(aScrollBar.fSize.x-aScrollBar.fButtonSize,0.0),
                          TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fSize.y),
                          TpvVector2.Create(aScrollBar.fSize.x-aScrollBar.fButtonSize,0.0),
                          TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fSize.y));
   Sprite:=TpvSprite(fIconDirectionArrowRight);
   aCanvas.DrawSprite(Sprite,
                      TpvRect.CreateRelative(0.0,0.0,Sprite.Width,Sprite.Height),
                      TpvRect.CreateAbsolute(TpvVector2.Create((aScrollBar.fSize.x-aScrollBar.fButtonSize)+IconSpacer,IconSpacer),
                                             TpvVector2.Create(aScrollBar.fSize.x-IconSpacer,aScrollBar.fSize.y-IconSpacer)));
  end;
  else {pvgsboVertical:}begin
   aCanvas.DrawGUIElement(Element,
                          true,
                          TpvVector2.Create(0.0,aScrollBar.fSize.y-aScrollBar.fButtonSize),
                          TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fSize.y),
                          TpvVector2.Create(0.0,aScrollBar.fSize.y-aScrollBar.fButtonSize),
                          TpvVector2.Create(aScrollBar.fSize.x,aScrollBar.fSize.y));
   Sprite:=TpvSprite(fIconDirectionArrowDown);
   aCanvas.DrawSprite(Sprite,
                      TpvRect.CreateRelative(0.0,0.0,Sprite.Width,Sprite.Height),
                      TpvRect.CreateAbsolute(TpvVector2.Create(IconSpacer,(aScrollBar.fSize.y-aScrollBar.fButtonSize)+IconSpacer),
                                             TpvVector2.Create(aScrollBar.fSize.x-IconSpacer,aScrollBar.fSize.y-IconSpacer)));
  end;
 end;

 Rect:=aScrollBar.GetSliderButtonRect;
 if aScrollBar.Enabled then begin
  if aScrollBar.fPushedSubWidget=pvgsbswSliderButton then begin
   Element:=GUI_ELEMENT_BUTTON_PUSHED;
  end else if aScrollBar.Focused and (aScrollBar.fFocusedSubWidget=pvgsbswSliderButton) then begin
   Element:=GUI_ELEMENT_BUTTON_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BUTTON_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BUTTON_DISABLED;
 end;
 aCanvas.DrawGUIElement(Element,
                        true,
                        Rect.LeftTop,
                        Rect.RightBottom,
                        Rect.LeftTop,
                        Rect.RightBottom);

end;

function TpvGUIDefaultVectorBasedSkin.GetSliderPreferredSize(const aSlider:TpvGUISlider):TpvVector2;
begin
 case aSlider.fOrientation of
  pvgsoHorizontal:begin
   result:=TpvVector2.Create((aSlider.ButtonSize*2.0)+128,aSlider.ButtonSize);
  end;
  else {pvgsoVertical:}begin
   result:=TpvVector2.Create(aSlider.ButtonSize,(aSlider.ButtonSize*2.0)+128);
  end;
 end;
 result:=Maximum(GetWidgetLayoutPreferredSize(aSlider),
                 result);
 if aSlider.fFixedSize.x>0.0 then begin
  result.x:=aSlider.fFixedSize.x;
 end;
 if aSlider.fFixedSize.y>0.0 then begin
  result.y:=aSlider.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawSlider(const aCanvas:TpvCanvas;const aSlider:TpvGUISlider);
const IconSpacer=0.0;
var Element:TpvInt32;
    Offset:TpvVector2;
    Sprite:TpvSprite;
    Rect:TpvRect;
begin

 aCanvas.ModelMatrix:=aSlider.fModelMatrix;
 aCanvas.ClipRect:=aSlider.fClipRect;

 if aSlider.Enabled then begin
  if aSlider.Focused xor (aSlider.fPushedSubWidget=pvgsswSliderButton) then begin
   Element:=GUI_ELEMENT_BOX_DARK_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BOX_DARK_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BOX_DARK_DISABLED;
 end;
 case aSlider.fOrientation of
  pvgsoHorizontal:begin
   Offset:=TpvVector2.Create(0.0,6.0);
  end;
  else {pvgsoVertical:}begin
   Offset:=TpvVector2.Create(6.0,0.0);
  end;
 end;
 aCanvas.DrawGUIElement(Element,
                        true,
                        Offset,
                        TpvVector2.Create(aSlider.fSize.x,aSlider.fSize.y)-Offset,
                        Offset,
                        TpvVector2.Create(aSlider.fSize.x,aSlider.fSize.y)-Offset);

 Rect:=aSlider.GetSliderButtonRect;
 if aSlider.Enabled then begin
  if aSlider.fPushedSubWidget=pvgsswSliderButton then begin
   Element:=GUI_ELEMENT_BUTTON_PUSHED;
  end else if aSlider.Focused and (aSlider.fFocusedSubWidget=pvgsswSliderButton) then begin
   Element:=GUI_ELEMENT_BUTTON_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BUTTON_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BUTTON_DISABLED;
 end;
 aCanvas.DrawGUIElement(Element,
                        true,
                        Rect.LeftTop,
                        Rect.RightBottom,
                        Rect.LeftTop,
                        Rect.RightBottom);

end;

function TpvGUIDefaultVectorBasedSkin.GetProgressBarPreferredSize(const aProgressBar:TpvGUIProgressBar):TpvVector2;
begin
 case aProgressBar.fOrientation of
  pvgpboHorizontal:begin
   result:=TpvVector2.Create(100,24);
  end;
  else {pvgpboVertical:}begin
   result:=TpvVector2.Create(24,100);
  end;
 end;
 result:=Maximum(GetWidgetLayoutPreferredSize(aProgressBar),
                 result);
 if aProgressBar.fFixedSize.x>0.0 then begin
  result.x:=aProgressBar.fFixedSize.x;
 end;
 if aProgressBar.fFixedSize.y>0.0 then begin
  result.y:=aProgressBar.fFixedSize.y;
 end;
end;

procedure TpvGUIDefaultVectorBasedSkin.DrawProgressBar(const aCanvas:TpvCanvas;const aProgressBar:TpvGUIProgressBar);
const IconSpacer=0.0;
var Element:TpvInt32;
    Offset,Scale:TpvVector2;
    Sprite:TpvSprite;
    Rect:TpvRect;
begin

 aCanvas.ModelMatrix:=aProgressBar.fModelMatrix;
 aCanvas.ClipRect:=aProgressBar.fClipRect;

 if aProgressBar.Enabled then begin
  if aProgressBar.Focused then begin
   Element:=GUI_ELEMENT_BOX_DARK_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BOX_DARK_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BOX_DARK_DISABLED;
 end;
 case aProgressBar.fOrientation of
  pvgpboHorizontal:begin
   Offset:=TpvVector2.Create(0.0,0.0);
  end;
  else {pvgpboVertical:}begin
   Offset:=TpvVector2.Create(0.0,0.0);
  end;
 end;
 aCanvas.DrawGUIElement(Element,
                        true,
                        Offset,
                        TpvVector2.Create(aProgressBar.fSize.x,aProgressBar.fSize.y)-Offset,
                        Offset,
                        TpvVector2.Create(aProgressBar.fSize.x,aProgressBar.fSize.y)-Offset);

 if aProgressBar.Enabled then begin
  if aProgressBar.Focused then begin
   Element:=GUI_ELEMENT_BOX_FOCUSED;
  end else begin
   Element:=GUI_ELEMENT_BOX_UNFOCUSED;
  end;
 end else begin
  Element:=GUI_ELEMENT_BOX_DISABLED;
 end;
 case aProgressBar.fOrientation of
  pvgpboHorizontal:begin
   Offset:=TpvVector2.Create(2.0,2.0);
   Scale:=TpvVector2.Create((aProgressBar.fValue-aProgressBar.fMinimumValue)/(aProgressBar.fMaximumValue-aProgressBar.fMinimumValue),1.0);
   aCanvas.DrawGUIElement(Element,
                          true,
                          Offset,
                          Offset+((TpvVector2.Create(aProgressBar.fSize.x,aProgressBar.fSize.y)-(Offset*2.0))*Scale),
                          Offset,
                          Offset+((TpvVector2.Create(aProgressBar.fSize.x,aProgressBar.fSize.y)-(Offset*2.0))*Scale));
  end;
  else {pvgpboVertical:}begin
   Offset:=TpvVector2.Create(2.0,2.0);
   Scale:=TpvVector2.Create(1.0,1.0-((aProgressBar.fValue-aProgressBar.fMinimumValue)/(aProgressBar.fMaximumValue-aProgressBar.fMinimumValue)));
   aCanvas.DrawGUIElement(Element,
                          true,
                          Offset+(TpvVector2.Create(0.0,aProgressBar.fSize.y-(Offset.y*2.0))*Scale),
                          TpvVector2.Create(aProgressBar.fSize.x,aProgressBar.fSize.y)-Offset,
                          Offset+(TpvVector2.Create(0.0,aProgressBar.fSize.y-(Offset.y*2.0))*Scale),
                          TpvVector2.Create(aProgressBar.fSize.x,aProgressBar.fSize.y)-Offset);
  end;
 end;

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

 fSize:=TpvVector2.Create(0.0,0.0);

 fFixedSize:=TpvVector2.Create(-1.0,-1.0);

 fPositionProperty:=TpvVector2Property.Create(@fPosition);

 fSizeProperty:=TpvVector2Property.Create(@fSize);

 fFixedSizeProperty:=TpvVector2Property.Create(@fFixedSize);

 fWidgetFlags:=TpvGUIWidget.DefaultFlags;

 fHint:='';

 fFont:=nil;

 fFontSize:=0.0;

 fFontColor:=TpvVector4.Null;

 fTextHorizontalAlignment:=pvgtaCenter;

 fTextVerticalAlignment:=pvgtaMiddle;

 fTextTruncation:=pvgttNone;

 fOnKeyEvent:=nil;

 fOnPointerEvent:=nil;

 fOnScrolled:=nil;

 fPopups:=TpvGUIObjectList.Create(false);

end;

destructor TpvGUIWidget.Destroy;
var Index:TpvInt32;
    Popup:TpvGUIPopup;
begin

 try
  for Index:=fPopups.Count-1 downto 0 do begin
   Popup:=fPopups[Index] as TpvGUIPopup;
   try
    Popup.fParentWidget:=nil;
    Popup.fParentHolder:=nil;
    fInstance.ReleaseObject(Popup);
   finally
    fPopups.Delete(Index);
   end;
  end;
 finally
  FreeAndNil(fPopups);
 end;

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
  end else if Child is TpvGUIPopupMenu then begin
   (Child as TpvGUIPopupMenu).SetSkin(aSkin);
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

function TpvGUIWidget.GetDraggable:boolean;
begin
 result:=pvgwfDraggable in fWidgetFlags;
end;

procedure TpvGUIWidget.SetDraggable(const aDraggable:boolean);
begin
 if aDraggable then begin
  Include(fWidgetFlags,pvgwfDraggable);
 end else begin
  Exclude(fWidgetFlags,pvgwfDraggable);
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

function TpvGUIWidget.GetTabStop:boolean;
begin
 result:=pvgwfTabStop in fWidgetFlags;
end;

procedure TpvGUIWidget.SetTabStop(const aTabStop:boolean);
begin
 if aTabStop then begin
  Include(fWidgetFlags,pvgwfTabStop);
 end else begin
  Exclude(fWidgetFlags,pvgwfTabStop);
 end;
end;

function TpvGUIWidget.GetKeyPreview:boolean;
begin
 result:=pvgwfKeyPreview in fWidgetFlags;
end;

procedure TpvGUIWidget.SetKeyPreview(const aKeyPreview:boolean);
begin
 if aKeyPreview then begin
  Include(fWidgetFlags,pvgwfKeyPreview);
 end else begin
  Exclude(fWidgetFlags,pvgwfKeyPreview);
 end;
end;

function TpvGUIWidget.GetWantAllKeys:boolean;
begin
 result:=pvgwfWantAllKeys in fWidgetFlags;
end;

procedure TpvGUIWidget.SetWantAllKeys(const aWantAllKeys:boolean);
begin
 if aWantAllKeys then begin
  Include(fWidgetFlags,pvgwfWantAllKeys);
 end else begin
  Exclude(fWidgetFlags,pvgwfWantAllKeys);
 end;
end;

function TpvGUIWidget.GetLeft:TpvFloat;
begin
 result:=fPosition.x;
end;

procedure TpvGUIWidget.SetLeft(const aLeft:TpvFloat);
begin
 Position.x:=aLeft;
end;

function TpvGUIWidget.GetTop:TpvFloat;
begin
 result:=fPosition.y;
end;

procedure TpvGUIWidget.SetTop(const aTop:TpvFloat);
begin
 Position.y:=aTop;
end;

function TpvGUIWidget.GetWidth:TpvFloat;
begin
 result:=fSize.x;
end;

procedure TpvGUIWidget.SetWidth(const aWidth:TpvFloat);
begin
 Size.x:=aWidth;
end;

function TpvGUIWidget.GetHeight:TpvFloat;
begin
 result:=fSize.y;
end;

procedure TpvGUIWidget.SetHeight(const aHeight:TpvFloat);
begin
 Size.y:=aHeight;
end;

function TpvGUIWidget.GetFixedWidth:TpvFloat;
begin
 result:=fFixedSize.x;
end;

procedure TpvGUIWidget.SetFixedWidth(const aFixedWidth:TpvFloat);
begin
 FixedSize.x:=aFixedWidth;
end;

function TpvGUIWidget.GetFixedHeight:TpvFloat;
begin
 result:=fFixedSize.y;
end;

procedure TpvGUIWidget.SetFixedHeight(const aFixedHeight:TpvFloat);
begin
 FixedSize.y:=aFixedHeight;
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

function TpvGUIWidget.GetWidgetPreferredSize:TpvVector2;
begin
 if assigned(fLayout) then begin
  result:=fLayout.GetPreferredSize(self);
 end else begin
  result:=fSize;
 end;
end;

function TpvGUIWidget.GetWidgetLayoutPreferredSize:TpvVector2;
begin
 if assigned(fLayout) then begin
  result:=fLayout.GetPreferredSize(self);
 end else begin
  result:=TpvVector2.Null;
 end;
end;

function TpvGUIWidget.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetWidgetPreferredSize(self);
end;

function TpvGUIWidget.GetFixedSize:TpvVector2;
begin
 result:=fFixedSize;
end;

function TpvGUIWidget.GetHighlightRect:TpvRect;
begin
 result:=TpvRect.CreateRelative(TpvVector2.Null,fSize);
end;

function TpvGUIWidget.GetFont:TpvFont;
begin
 if assigned(Skin) and not assigned(fFont) then begin
  result:=Skin.fSansFont;
 end else begin
  result:=fFont;
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

function TpvGUIWidget.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

procedure TpvGUIWidget.Release;
begin
 if assigned(fInstance) then begin
  fInstance.ReleaseObject(self);
 end else begin
  DecRef;
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

procedure TpvGUIWidget.GetTabList(const aList:Classes.TList);
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
begin
 if (fWidgetFlags*[pvgwfVisible,pvgwfEnabled])=[pvgwfVisible,pvgwfEnabled] then begin
  for ChildIndex:=0 to fChildren.Count-1 do begin
   Child:=fChildren.Items[ChildIndex];
   if assigned(Child) and (Child is TpvGUIWidget) then begin
    ChildWidget:=Child as TpvGUIWidget;
    if not ((self is TpvGUIWindow) and
            (ChildWidget=(self as TpvGUIWindow).fButtonPanel)) then begin
     if (ChildWidget.fWidgetFlags*[pvgwfVisible,pvgwfEnabled])=[pvgwfVisible,pvgwfEnabled] then begin
      if ChildWidget.TabStop then begin
       aList.Add(ChildWidget);
      end;
      ChildWidget.GetTabList(aList);
     end;
    end;
   end;
  end;
 end;
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

function TpvGUIWidget.FindNextWidget(const aCurrentWidget:TpvGUIWidget;const aForward,aCheckTabStop,aCheckParent:boolean):TpvGUIWidget;
const Directions:array[boolean] of TpvInt32=(-1,1);
var Count,Index,StartIndex:TpvInt32;
    Widget:TpvGUIWidget;
    List:Classes.TList;
begin
 result:=nil;
 List:=Classes.TList.Create;
 try
  GetTabList(List);
  Count:=List.Count;
  if Count>0 then begin
   Index:=List.IndexOf(aCurrentWidget);
   if Index<0 then begin
    if aForward then begin
     Index:=0;
    end else begin
     Index:=Count-1;
    end;
   end;
   StartIndex:=Index;
   repeat
    inc(Index,Directions[aForward]);
    if Index<0 then begin
     inc(Index,Count);
    end else if Index>=Count then begin
     dec(Index,Count);
    end;
    Widget:=List.Items[Index];
    if (Widget<>aCurrentWidget) and
       ((Widget.fWidgetFlags*[pvgwfVisible,pvgwfEnabled])=[pvgwfVisible,pvgwfEnabled]) and
       (Widget.TabStop or not aCheckTabStop) and
       ((Widget.fParent=self) or not aCheckParent) then begin
     result:=Widget;
     break;
    end;
   until Index=StartIndex;
  end;
 finally
  List.Free;
 end;
end;

function TpvGUIWidget.ProcessTab(const aFromWidget:TpvGUIWidget;const aToPrevious:boolean):boolean;
var CurrentWidget,ParentWidget:TpvGUIWidget;
begin
 result:=false;
 if assigned(fInstance) then begin
  ParentWidget:=aFromWidget.Window;
  if assigned(ParentWidget) then begin
   CurrentWidget:=ParentWidget.FindNextWidget(aFromWidget,not aToPrevious,true,false);
   if assigned(CurrentWidget) and
     (pvgwfTabStop in CurrentWidget.fWidgetFlags) then begin
    fInstance.UpdateFocus(CurrentWidget);
    result:=true;
   end;
  end;
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
end;

function TpvGUIWidget.GetLastParentWindow:TpvGUIWindow;
var CurrentWidget:TpvGUIWidget;
begin
 result:=nil;
 CurrentWidget:=self;
 while assigned(CurrentWidget) do begin
  if CurrentWidget is TpvGUIWindow then begin
   result:=CurrentWidget as TpvGUIWindow;
  end;
  if assigned(CurrentWidget.Parent) and (CurrentWidget.Parent is TpvGUIWidget) then begin
   CurrentWidget:=CurrentWidget.fParent as TpvGUIWidget;
  end else begin
   break;
  end;
 end;
end;

function TpvGUIWidget.GetScissorParent:TpvGUIWidget;
var CurrentWidget:TpvGUIWidget;
begin
 result:=nil;
 CurrentWidget:=self;
 while assigned(CurrentWidget) do begin
  if (CurrentWidget<>self) and
     (pvgwfScissor in CurrentWidget.fWidgetFlags) then begin
   result:=CurrentWidget;
   exit;
  end else begin
   if assigned(CurrentWidget.Parent) and (CurrentWidget.Parent is TpvGUIWidget) then begin
    CurrentWidget:=CurrentWidget.fParent as TpvGUIWidget;
   end else begin
    break;
   end;
  end;
 end;
 raise EpvGUIWidget.Create('Could not find scissor parent');
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

procedure TpvGUIWidget.SetSizeToPreferredSize;
var NewSize:TpvVector2;
begin
 NewSize:=GetPreferredSize;
 if fSize<>NewSize then begin
  fSize:=NewSize;
  PerformLayout;
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
    ChildWidgetFixedSize:=ChildWidget.GetFixedSize;
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

procedure TpvGUIWidget.Show;
begin
 if not (pvgwfVisible in fWidgetFlags) then begin
  SetVisible(true);
 end;
end;

procedure TpvGUIWidget.Hide;
begin
 if pvgwfVisible in fWidgetFlags then begin
  SetVisible(false);
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

function TpvGUIWidget.DragEvent(const aPosition:TpvVector2):boolean;
begin
 result:=false;
end;

function TpvGUIWidget.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
end;

function TpvGUIWidget.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildPointerEvent:TpvApplicationInputPointerEvent;
    PreviousContained,CurrentContained:boolean;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
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
end;

function TpvGUIWidget.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildPosition:TpvVector2;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
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
 end;
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
begin
 if fInstance.fDrawWidgetBounds then begin
  fCanvas.Push;
  try
   fCanvas.ModelMatrix:=fModelMatrix;
   fCanvas.ClipRect:=fParentClipRect;
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
 if pvgwfScissor in fWidgetFlags then begin
  fParentClipRect:=fClipRect;
 end;
 for ChildIndex:=0 to fChildren.Count-1 do begin
  Child:=fChildren.Items[ChildIndex];
  if Child is TpvGUIWidget then begin
   ChildWidget:=Child as TpvGUIWidget;
   fInstance.AddReferenceCountedObjectForNextDraw(ChildWidget);
   if ChildWidget.Visible then begin
    ChildWidget.fParentClipRect:=fParentClipRect;
    ChildWidget.fClipRect:=fClipRect.GetIntersection(TpvRect.CreateRelative(fModelMatrix*ChildWidget.fPosition,
                                                                            ChildWidget.fSize));
    ChildWidget.fModelMatrix:=TpvMatrix4x4.CreateTranslation(ChildWidget.Left,ChildWidget.Top)*fModelMatrix;
    ChildWidget.fCanvas:=fCanvas;
    ChildWidget.Update;
   end;
  end;
 end;
 if pvgwfDrawFocus in fWidgetFlags then begin
  Skin.DrawFocus(fCanvas,self);
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

constructor TpvGUIHolder.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
end;

destructor TpvGUIHolder.Destroy;
begin
 inherited Destroy;
end;

constructor TpvGUIInstance.Create(const aVulkanDevice:TpvVulkanDevice;
                                  const aFontCodePointRanges:TpvFontCodePointRanges=nil);
begin

 inherited Create(nil);

 fInstance:=self;

 fVulkanDevice:=aVulkanDevice;

 fFontCodePointRanges:=aFontCodePointRanges;

 if length(fFontCodePointRanges)=0 then begin
  SetLength(fFontCodePointRanges,2);
  fFontCodePointRanges[0]:=TpvFontCodePointRange.Create(0,255);
  fFontCodePointRanges[1]:=TpvFontCodePointRange.Create($2026,$2026);
 end;

 fStandardSkin:=TpvGUIDefaultVectorBasedSkin.Create(self);

 fDrawWidgetBounds:=false;

 fBuffers:=nil;

 fCountBuffers:=0;

 fUpdateBufferIndex:=0;

 fDrawBufferIndex:=0;

 fDeltaTime:=0.0;

 fTime:=0.0;

 fPopupMenuStack:=TpvGUIObjectList.Create(false);

 fModalWindowStack:=TpvGUIObjectList.Create(false);

 fLastFocusPath:=TpvGUIObjectList.Create(false);

 fCurrentFocusPath:=TpvGUIObjectList.Create(false);

 fDragWidget:=nil;

 fWindow:=nil;

 fLayout:=TpvGUIRootLayout.Create(self);

 fContent:=TpvGUIPanel.Create(self);

 fMenu:=nil;

 fFocusedWidget:=nil;

 fHoveredWidget:=nil;

 fVisibleCursor:=pvgcArrow;

 Include(fWidgetFlags,pvgwfScissor);

 SetCountBuffers(1);

end;

destructor TpvGUIInstance.Destroy;
begin

 TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);

 FreeAndNil(fPopupMenuStack);

 FreeAndNil(fLastFocusPath);

 FreeAndNil(fCurrentFocusPath);

 FreeAndNil(fModalWindowStack);

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
 TpvReferenceCountedObject.DecRefOrFreeAndNil(fFocusedWidget);
 TpvReferenceCountedObject.DecRefOrFreeAndNil(fHoveredWidget);
 fPopupMenuStack.Clear;
 fModalWindowStack.Clear;
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

procedure TpvGUIInstance.ReleaseObject(const aGUIObject:TpvGUIObject);
begin
 if assigned(aGUIObject) then begin
  if assigned(fPopupMenuStack) and fPopupMenuStack.Contains(aGUIObject) then begin
   fPopupMenuStack.Clear;
  end;
  if assigned(fModalWindowStack) and fModalWindowStack.Contains(aGUIObject) then begin
   fModalWindowStack.Clear;
  end;
  if assigned(fLastFocusPath) and fLastFocusPath.Contains(aGUIObject) then begin
   fLastFocusPath.Clear;
  end;
  if assigned(fCurrentFocusPath) and fCurrentFocusPath.Contains(aGUIObject) then begin
   fCurrentFocusPath.Clear;
  end;
  if fDragWidget=aGUIObject then begin
   TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
  end;
  if fWindow=aGUIObject then begin
   TpvReferenceCountedObject.DecRefOrFreeAndNil(fWindow);
  end;
  if fFocusedWidget=aGUIObject then begin
   TpvReferenceCountedObject.DecRefOrFreeAndNil(fFocusedWidget);
  end;
  if fHoveredWidget=aGUIObject then begin
   TpvReferenceCountedObject.DecRefOrFreeAndNil(fHoveredWidget);
  end;
  if assigned(aGUIObject.fParent) and
     assigned(aGUIObject.fParent.fChildren) and
     aGUIObject.fParent.fChildren.Contains(aGUIObject) then begin
   aGUIObject.fParent.fChildren.Remove(aGUIObject);
  end;
  if assigned(fChildren) and fChildren.Contains(aGUIObject) then begin
   fChildren.Remove(aGUIObject);
  end;
 end;
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

 if fPopupMenuStack.Count>0 then begin
  if (fPopupMenuStack[0].fParent<>aWidget) and
     ((not assigned(fPopupMenuStack[0].fParent)) or
      (not (fPopupMenuStack[0].fParent is TpvGUIMenuItem)) or
      ((fPopupMenuStack[0].fParent.fParent<>aWidget))) then begin
   (fPopupMenuStack[0] as TpvGUIPopupMenu).Deactivate;
  end;
 end;

 if (fModalWindowStack.Count>0) and
    (aWidget.GetWindow<>fModalWindowStack[fModalWindowStack.Count-1]) then begin
  exit;
 end;

 TpvSwap<TpvGUIObjectList>.Swap(fCurrentFocusPath,fLastFocusPath);

 fCurrentFocusPath.Clear;

 TpvReferenceCountedObject.DecRefOrFreeAndNil(fWindow);

 TpvReferenceCountedObject.DecRefOrFreeAndNil(fFocusedWidget);
 fFocusedWidget:=aWidget;
 if assigned(fFocusedWidget) then begin
  fFocusedWidget.IncRef;
 end;

 CurrentWidget:=aWidget;
 while assigned(CurrentWidget) do begin
  if fCurrentFocusPath.Count=0 then begin
   fCurrentFocusPath.Add(CurrentWidget);
  end else begin
   fCurrentFocusPath.Insert(0,CurrentWidget);
  end;
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
 ReleaseObject(aWindow);
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
    PopupWidget:TpvGUIPopup;
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
      if Current is TpvGUIPopup then begin
       PopupWidget:=Current as TpvGUIPopup;
       if (PopupWidget.ParentHolder.GetLastParentWindow=aWindow) and (Index<BaseIndex) then begin
        MoveWindowToFront(PopupWidget);
        Changed:=true;
        break;
       end;
      end;
     end;
    end;
   until not Changed;
  end;
 end;
end;

function TpvGUIInstance.AddMenu:TpvGUIWindowMenu;
begin
 if not assigned(fMenu) then begin
  fMenu:=TpvGUIWindowMenu.Create(self);
 end;
 result:=fMenu;
end;

procedure TpvGUIInstance.PerformLayout;
var ChildPreferredSize:TpvVector2;
begin

 if assigned(fMenu) then begin
  fMenu.Visible:=false;
 end;

 inherited PerformLayout;

 if assigned(fMenu) then begin
  fMenu.Visible:=true;
  ChildPreferredSize:=fMenu.PreferredSize;
  fMenu.Width:=Width;
  fMenu.Height:=ChildPreferredSize.y;
  fMenu.Left:=0.0;
  fMenu.Top:=0.0;
  fMenu.PerformLayout;
 end;

end;

function TpvGUIInstance.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var Index:TpvInt32;
    Current:TpvGUIObject;
    CurrentWidget:TpvGUIWidget;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if not result then begin
  if fPopupMenuStack.Count>0 then begin
   result:=(fPopupMenuStack[fPopupMenuStack.Count-1] as TpvGUIPopupMenu).KeyEvent(aKeyEvent);
  end;
  if not result then begin
   if (aKeyEvent.KeyEventType=KEYEVENT_TYPED) and (aKeyEvent.KeyCode=KEYCODE_TAB) then begin
    if fCurrentFocusPath.Count>0 then begin
     Current:=fCurrentFocusPath.Items[fCurrentFocusPath.Count-1];
     if (Current<>self) and (Current is TpvGUIWidget) then begin
      CurrentWidget:=Current as TpvGUIWidget;
      if CurrentWidget.Focused then begin
       result:=ProcessTab(CurrentWidget,KEYMODIFIER_SHIFT IN aKeyEvent.KeyModifiers);
       if result then begin
        exit;
       end;
      end;
     end;
    end;
   end;
   begin
    // KeyPreview pass
    Index:=0;
    while Index<fCurrentFocusPath.Count do begin
     // must be a while-loop, not an for-loop, because fCurrentFocusPath can be changed while going through this list
     Current:=fCurrentFocusPath.Items[Index];
     if (Current<>self) and (Current is TpvGUIWidget) then begin
      CurrentWidget:=Current as TpvGUIWidget;
      if CurrentWidget.KeyPreview then begin
       result:=CurrentWidget.KeyEvent(aKeyEvent);
       if result then begin
        exit;
       end;
      end;
     end;
     inc(Index);
    end;
   end;
   begin
    // Normal pass
    Index:=fCurrentFocusPath.Count-1;
    while (Index>=0) and (Index<fCurrentFocusPath.Count) do begin
     // must be a while-loop, not an for-loop, because fCurrentFocusPath can be changed while going through this list
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
     dec(Index);
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
    DoUpdateCursor,IsCursorOnMenu:boolean;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  DoUpdateCursor:=false;
  IsCursorOnMenu:=false;
  fMousePosition:=aPointerEvent.Position;
  if not assigned(fDragWidget) then begin
   for Index:=fPopupMenuStack.Count-1 downto 0 do begin
    result:=(fPopupMenuStack[Index] as TpvGUIPopupMenu).PointerEvent(aPointerEvent);
    if result then begin
     IsCursorOnMenu:=true;
     DoUpdateCursor:=true;
     break;
    end;
   end;
  end;
  if not result then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN,POINTEREVENT_UP:begin
     if (fModalWindowStack.Count>0) and
        (fModalWindowStack[fModalWindowStack.Count-1] is TpvGUIWindow) then begin
      CurrentWindow:=fModalWindowStack[fModalWindowStack.Count-1] as TpvGUIWindow;
      if not CurrentWindow.Contains(aPointerEvent.Position-CurrentWindow.AbsolutePosition) then begin
       exit;
      end;
     end;
     case aPointerEvent.PointerEventType of
      POINTEREVENT_DOWN:begin
       case aPointerEvent.Button of
        BUTTON_LEFT,BUTTON_RIGHT:begin
         TpvReferenceCountedObject.DecRefOrFreeAndNil(fDragWidget);
         CurrentWidget:=FindWidget(aPointerEvent.Position);
         if assigned(CurrentWidget) and
            (CurrentWidget<>self) and
            CurrentWidget.Draggable and
            CurrentWidget.DragEvent(aPointerEvent.Position-CurrentWidget.AbsolutePosition) then begin
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
       if assigned(CurrentWidget) and CurrentWidget.HasParent(fDragWidget) then begin
        CurrentWidget:=fDragWidget;
       end;
       if assigned(fDragWidget) and (fDragWidget<>CurrentWidget) then begin
        LocalPointerEvent:=aPointerEvent;
        LocalPointerEvent.PointerEventType:=POINTEREVENT_UP;
        LocalPointerEvent.Button:=BUTTON_LEFT;
        LocalPointerEvent.Position:=LocalPointerEvent.Position-fDragWidget.AbsolutePosition;
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
      LocalPointerEvent.Position:=LocalPointerEvent.Position-fDragWidget.AbsolutePosition;
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
  end;
  if DoUpdateCursor then begin
   if assigned(fDragWidget) then begin
    fVisibleCursor:=fDragWidget.fCursor;
   end else if IsCursorOnMenu then begin
    fVisibleCursor:=pvgcArrow;
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
end;

function TpvGUIInstance.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var Index:TpvInt32;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
  for Index:=fPopupMenuStack.Count-1 downto 0 do begin
   result:=(fPopupMenuStack[Index] as TpvGUIPopupMenu).Scrolled(aPosition,aRelativeAmount);
   if result then begin
    break;
   end;
  end;
  if not result then begin
   result:=inherited Scrolled(aPosition,aRelativeAmount);
  end;
 end;
end;

procedure TpvGUIInstance.FindHoveredWidget;
var Index:TpvInt32;
    CurrentWidget:TpvGUIWidget;
    IsOnPopupMenu:boolean;
begin
 IsOnPopupMenu:=false;
 if fPopupMenuStack.Count>0 then begin
  for Index:=fPopupMenuStack.Count-1 downto 0 do begin
   if TpvRect.CreateRelative((fPopupMenuStack[Index] as TpvGUIPopupMenu).fPosition,
                             (fPopupMenuStack[Index] as TpvGUIPopupMenu).fSize).Touched(fMousePosition) then begin
    IsOnPopupMenu:=true;
    break;
   end;
  end;
 end;
 if IsOnPopupMenu then begin
  CurrentWidget:=nil;
 end else begin
  CurrentWidget:=FindWidget(fMousePosition);
  if assigned(CurrentWidget) and
     (fModalWindowStack.Count>0) and
     (fModalWindowStack[fModalWindowStack.Count-1] is TpvGUIWindow) and
     (CurrentWidget.GetWindow<>(fModalWindowStack[fModalWindowStack.Count-1] as TpvGUIWindow)) then begin
   CurrentWidget:=nil;
  end;
 end;
 if fHoveredWidget<>CurrentWidget then begin
  TpvReferenceCountedObject.DecRefOrFreeAndNil(fHoveredWidget);
  fHoveredWidget:=CurrentWidget;
  if assigned(fHoveredWidget) then begin
   fHoveredWidget.IncRef;
  end;
 end;
end;

procedure TpvGUIInstance.Update;
var Index:TpvInt32;
    LastModelMatrix:TpvMatrix4x4;
    LastClipRect:TpvRect;
    Popup:TpvGUIPopup;
    PopupMenu:TpvGUIPopupMenu;
begin
 ClearReferenceCountedObjectList;
 for Index:=0 to fChildren.Count-1 do begin
  if fChildren[Index] is TpvGUIPopup then begin
   Popup:=fChildren[Index] as TpvGUIPopup;
   if Popup.RecursiveVisible then begin
    Popup.UpdatePosition;
   end;
  end;
 end;
 LastModelMatrix:=fCanvas.ModelMatrix;
 LastClipRect:=fCanvas.ClipRect;
 try
  fModelMatrix:=LastModelMatrix;
  fClipRect:=LastClipRect.GetIntersection(TpvRect.CreateRelative(fPosition,fSize));
  FindHoveredWidget;
  inherited Update;
  for Index:=0 to fPopupMenuStack.Count-1 do begin
   PopupMenu:=fPopupMenuStack[Index] as TpvGUIPopupMenu;
   fInstance.AddReferenceCountedObjectForNextDraw(PopupMenu);
   PopupMenu.Draw(fCanvas);
  end;
  Skin.DrawMouse(fCanvas,self);
 finally
  fCanvas.ModelMatrix:=LastModelMatrix;
  fCanvas.ClipRect:=LastClipRect;
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

 Include(fWidgetFlags,pvgwfScissor);
//Include(fWidgetFlags,pvgwfDrawFocus);
 Include(fWidgetFlags,pvgwfDraggable);

 fLastWindowState:=TpvGUIWindowState.pvgwsNormal;

 fWindowState:=TpvGUIWindowState.pvgwsNormal;

 fLayout:=TpvGUIRootLayout.Create(self);

 fMenu:=nil;

 fButtonPanel:=nil;

 fContent:=TpvGUIPanel.Create(self);

 fMinimizationButton:=nil;

 fMaximizationButton:=nil;

 fCloseButton:=nil;

 fTextHorizontalAlignment:=pvgtaCenter;

 fTextTruncation:=pvgttMiddle;

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

procedure TpvGUIWindow.OnWindowHeaderButtonClick(const aSender:TpvGUIObject);
begin
 if aSender=fMinimizationButton then begin
  if fWindowState=pvgwsMinimized then begin
   WindowState:=pvgwsNormal;
  end else begin
   WindowState:=pvgwsMinimized;
  end;
 end else if aSender=fMaximizationButton then begin
  if fWindowState=pvgwsMaximized then begin
   WindowState:=pvgwsNormal;
  end else begin
   WindowState:=pvgwsMaximized;
  end;
 end else if aSender=fCloseButton then begin
  DisposeWindow;
 end;
 if assigned(fInstance) and
    (aSender is TpvGUIWidget) and
    ((aSender as TpvGUIWidget).Focused) then begin
  fInstance.UpdateFocus(nil);
 end;
end;

procedure TpvGUIWindow.AddMinimizationButton;
begin
 if not assigned(fMinimizationButton) then begin
  fMinimizationButton:=TpvGUIButton.Create(ButtonPanel);
  fMinimizationButton.fIcon:=Skin.fIconWindowMinimize;
  fMinimizationButton.fIconHeight:=Skin.fWindowButtonIconHeight;
  fMinimizationButton.fCaption:='';
  fMinimizationButton.OnClick:=OnWindowHeaderButtonClick;
  fMinimizationButton.fWidgetFlags:=fMinimizationButton.fWidgetFlags-[pvgwfTabStop];
 end;
end;

procedure TpvGUIWindow.AddMaximizationButton;
begin
 if not assigned(fMaximizationButton) then begin
  fMaximizationButton:=TpvGUIButton.Create(ButtonPanel);
  fMaximizationButton.fIcon:=Skin.fIconWindowMaximize;
  fMaximizationButton.fIconHeight:=Skin.fWindowButtonIconHeight;
  fMaximizationButton.fCaption:='';
  fMaximizationButton.OnClick:=OnWindowHeaderButtonClick;
  fMaximizationButton.fWidgetFlags:=fMaximizationButton.fWidgetFlags-[pvgwfTabStop];
 end;
end;

procedure TpvGUIWindow.AddCloseButton;
begin
 if not assigned(fCloseButton) then begin
  fCloseButton:=TpvGUIButton.Create(ButtonPanel);
  fCloseButton.fIcon:=Skin.fIconWindowClose;
  fCloseButton.fIconHeight:=Skin.fWindowButtonIconHeight;
  fCloseButton.fCaption:='';
  fCloseButton.OnClick:=OnWindowHeaderButtonClick;
  fCloseButton.fWidgetFlags:=fCloseButton.fWidgetFlags-[pvgwfTabStop];
 end;
end;

function TpvGUIWindow.AddMenu:TpvGUIWindowMenu;
begin
 if not assigned(fMenu) then begin
  fMenu:=TpvGUIWindowMenu.Create(self);
 end;
 result:=fMenu;
end;

procedure TpvGUIWindow.DisposeWindow;
begin
 if assigned(fInstance) then begin
  fInstance.DisposeWindow(self);
 end;
end;

procedure TpvGUIWindow.SetWindowFlags(const aWindowFlags:TpvGUIWindowFlags);
begin
 if ((pvgwfModal in fWindowFlags)<>(pvgwfModal in aWindowFlags)) and
    assigned(fInstance) and
    assigned(fInstance.fModalWindowStack) then begin
  if pvgwfModal in aWindowFlags then begin
   if fInstance.fModalWindowStack.IndexOf(self)<0 then begin
    fInstance.fModalWindowStack.Add(self);
   end;
  end else begin
   fInstance.fModalWindowStack.Remove(self);
  end;
 end;
 fWindowFlags:=aWindowFlags;
end;

procedure TpvGUIWindow.SetWindowState(const aWindowState:TpvGUIWindowState);
var MinimumPosition,MinimumSize:TpvVector2;
begin
 if fWindowState<>aWindowState then begin
  if fWindowState=pvgwsNormal then begin
   fSavedPosition:=fPosition;
   fSavedSize:=fSize;
  end;
  MinimumPosition:=TpvVector2.Null;
  if assigned(fParent) and (fParent is TpvGUIWindow) then begin
   if pvgwfHeader in (fParent as TpvGUIWindow).fWindowFlags then begin
    MinimumPosition.y:=MinimumPosition.y+Skin.fWindowHeaderHeight;
   end;
   if assigned((fParent as TpvGUIWindow).fMenu) then begin
    MinimumPosition.y:=MinimumPosition.y+Skin.fWindowMenuHeight;
   end;
  end else if assigned(fParent) and (fParent is TpvGUIInstance) then begin
   if assigned((fParent as TpvGUIInstance).fMenu) then begin
    MinimumPosition.y:=MinimumPosition.y+Skin.fWindowMenuHeight;
   end;
  end;
  case aWindowState of
   pvgwsNormal:begin
    if fWindowState=pvgwsMaximized then begin
     fPosition:=fSavedPosition;
    end;
    fSize:=fSavedSize;
   end;
   pvgwsMinimized:begin
    fSize.y:=Skin.fWindowHeaderHeight;
   end;
   pvgwsMaximized:begin
    fPosition:=MinimumPosition;
    if assigned(fParent) then begin
     if fParent is TpvGUIInstance then begin
      fSize:=(fParent as TpvGUIInstance).fContent.fSize;
     end else if fParent is TpvGUIWindow then begin
      fSize:=(fParent as TpvGUIWindow).fContent.fSize;
     end else if fParent is TpvGUIWidget then begin
      fSize:=(fParent as TpvGUIWidget).fSize;
     end;
    end;
   end;
  end;
  if aWindowState=pvgwsMinimized then begin
   MinimumSize:=TpvVector2.Create(Skin.fMinimizedWindowMinimumWidth,Skin.fMinimizedWindowMinimumHeight);
  end else begin
   MinimumSize:=TpvVector2.Create(Skin.fWindowMinimumWidth,Skin.fWindowMinimumHeight);
  end;
  if assigned(fButtonPanel) then begin
   MinimumSize.x:=Max(MinimumSize.x,fButtonPanel.Size.x+(Skin.fSpacing*2.0));
  end;
  if assigned(fParent) and (fParent is TpvGUIWidget) then begin
   fSize:=Clamp(fSize,MinimumSize,(fParent as TpvGUIWidget).fSize-fPosition);
   fPosition:=Clamp(fPosition,MinimumPosition,(fParent as TpvGUIWidget).fSize-fSize);
  end else begin
   fSize:=Maximum(fSize,MinimumSize);
   fPosition:=Maximum(fPosition,MinimumPosition);
  end;
  fLastWindowState:=fWindowState;
  fWindowState:=aWindowState;
  case fWindowState of
   pvgwsNormal:begin
    if assigned(fMinimizationButton) then begin
     fMinimizationButton.fIcon:=Skin.fIconWindowMinimize;
    end;
    if assigned(fMaximizationButton) then begin
     fMaximizationButton.fIcon:=Skin.fIconWindowMaximize;
    end;
    if assigned(fCloseButton) then begin
     fCloseButton.fIcon:=Skin.fIconWindowClose;
    end;
   end;
   pvgwsMinimized:begin
    if assigned(fMinimizationButton) then begin
     fMinimizationButton.fIcon:=Skin.fIconWindowRestore;
    end;
    if assigned(fMaximizationButton) then begin
     fMaximizationButton.fIcon:=Skin.fIconWindowMaximize;
    end;
    if assigned(fCloseButton) then begin
     fCloseButton.fIcon:=Skin.fIconWindowClose;
    end;
   end;
   pvgwsMaximized:begin
    if assigned(fMinimizationButton) then begin
     fMinimizationButton.fIcon:=Skin.fIconWindowMinimize;
    end;
    if assigned(fMaximizationButton) then begin
     fMaximizationButton.fIcon:=Skin.fIconWindowRestore;
    end;
    if assigned(fCloseButton) then begin
     fCloseButton.fIcon:=Skin.fIconWindowClose;
    end;
   end;
  end;
  PerformLayout;
 end;
end;

function TpvGUIWindow.GetModal:boolean;
begin
 result:=pvgwfModal in fWindowFlags;
end;

procedure TpvGUIWindow.SetModal(const aModal:boolean);
begin
 if aModal then begin
  SetWindowFlags(fWindowFlags+[pvgwfModal]);
 end else begin
  SetWindowFlags(fWindowFlags-[pvgwfModal]);
 end;
end;

function TpvGUIWindow.GetButtonPanel:TpvGUIPanel;
begin
 if not assigned(fButtonPanel) then begin
  fButtonPanel:=TpvGUIPanel.Create(self);
  fButtonPanel.fLayout:=TpvGUIBoxLayout.Create(fButtonPanel,pvglaMiddle,pvgloHorizontal,0.0,4.0);
 end;
 result:=fButtonPanel;
end;

function TpvGUIWindow.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fWindowFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

function TpvGUIWindow.GetFixedSize:TpvVector2;
begin
 case fWindowState of
  pvgwsMinimized:begin
   result:=inherited GetFixedSize;
   result.y:=Skin.fWindowHeaderHeight;
  end;
  pvgwsMaximized:begin
   if assigned(fParent) then begin
    if fParent is TpvGUIInstance then begin
     result:=(fParent as TpvGUIInstance).fContent.fSize;
    end else if fParent is TpvGUIWindow then begin
     result:=(fParent as TpvGUIWindow).fContent.fSize;
    end else if fParent is TpvGUIWidget then begin
     result:=(fParent as TpvGUIWidget).fSize;
    end else begin
     result:=inherited GetFixedSize;
    end;
   end else begin
    result:=inherited GetFixedSize;
   end;
  end;
  else begin
   result:=inherited GetFixedSize;
  end;
 end;
end;

function TpvGUIWindow.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetWindowPreferredSize(self);
end;

function TpvGUIWindow.FindWidget(const aPosition:TpvVector2):TpvGUIWidget;
begin
 if fWindowState=pvgwsNormal then begin
  if (fWindowState in [pvgwsNormal]) and
     (pvgwfResizableNW in fWindowFlags) and
     (aPosition.x<Skin.fWindowResizeGripSize) and
     (aPosition.y<Skin.fWindowResizeGripSize) then begin
   result:=self;
   exit;
  end else if (fWindowState in [pvgwsNormal]) and
              (pvgwfResizableNE in fWindowFlags) and
              (aPosition.x>(fSize.x-Skin.fWindowResizeGripSize)) and
              (aPosition.y<Skin.fWindowResizeGripSize) then begin
   result:=self;
   exit;
  end else if (fWindowState in [pvgwsNormal]) and
              (pvgwfResizableSW in fWindowFlags) and
              (aPosition.x<Skin.fWindowResizeGripSize) and
              (aPosition.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
   result:=self;
   exit;
  end else if (fWindowState in [pvgwsNormal]) and
              (pvgwfResizableSE in fWindowFlags) and
              (aPosition.x>(fSize.x-Skin.fWindowResizeGripSize)) and
              (aPosition.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
   result:=self;
   exit;
  end else if (fWindowState in [pvgwsNormal]) and
              (pvgwfResizableN in fWindowFlags) and
              (aPosition.y<Skin.fWindowResizeGripSize) then begin
   result:=self;
   exit;
  end else if (fWindowState in [pvgwsNormal]) and
              (pvgwfResizableS in fWindowFlags) and
              (aPosition.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
   result:=self;
   exit;
  end else if (fWindowState in [pvgwsNormal]) and
              (pvgwfResizableW in fWindowFlags) and
              (aPosition.x<Skin.fWindowResizeGripSize) then begin
   result:=self;
   exit;
  end else if (fWindowState in [pvgwsNormal]) and
              (pvgwfResizableE in fWindowFlags) and
              (aPosition.x>(fSize.x-Skin.fWindowResizeGripSize)) then begin
   result:=self;
   exit;
  end;
 end;
 result:=inherited FindWidget(aPosition);
end;

procedure TpvGUIWindow.PerformLayout;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildWidget:TpvGUIWidget;
    ChildPreferredSize:TpvVector2;
begin

 if assigned(fButtonPanel) then begin
  fButtonPanel.Visible:=false;
 end;

 if assigned(fMenu) then begin
  fMenu.Visible:=false;
 end;

 inherited PerformLayout;

 if assigned(fButtonPanel) then begin
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
  ChildPreferredSize:=fButtonPanel.PreferredSize;
  fButtonPanel.Width:=ChildPreferredSize.x;
  fButtonPanel.Height:=ChildPreferredSize.y;
  fButtonPanel.Left:=Width-(ChildPreferredSize.x+5);
  fButtonPanel.Top:=(Skin.WindowHeaderHeight-ChildPreferredSize.y)*0.5;
  fButtonPanel.PerformLayout;
 end;

 if assigned(fMenu) then begin
  fMenu.Visible:=true;
  ChildPreferredSize:=fMenu.PreferredSize;
  fMenu.Width:=Width;
  fMenu.Height:=ChildPreferredSize.y;
  fMenu.Left:=0.0;
  fMenu.Top:=Skin.WindowHeaderHeight;
  fMenu.PerformLayout;
 end;

end;

procedure TpvGUIWindow.Center;
begin
 if assigned(fInstance) then begin
  fInstance.CenterWindow(self);
 end;
end;

function TpvGUIWindow.DragEvent(const aPosition:TpvVector2):boolean;
begin
 result:=true;
end;

function TpvGUIWindow.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
end;

function TpvGUIWindow.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var ClampedRelativePosition,MinimumPosition,MinimumSize,NewSize,NewPosition,OldSize:TpvVector2;
    OK:boolean;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  OK:=false;
  if (aPointerEvent.PointerEventType=POINTEREVENT_DRAG) or
     (fMouseAction<>pvgwmaNone) then begin
   OK:=true;
  end else if (fWindowState=pvgwsNormal) and
              (aPointerEvent.Position.x>=0) and
              (aPointerEvent.Position.y>=0) and
              (aPointerEvent.Position.x<fSize.x) and
              (aPointerEvent.Position.y<fSize.y) then begin
   if (fWindowState in [pvgwsNormal]) and
      (pvgwfResizableNW in fWindowFlags) and
      (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
      (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
    OK:=true;
   end else if (fWindowState in [pvgwsNormal]) and
               (pvgwfResizableNE in fWindowFlags) and
               (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
               (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
    OK:=true;
   end else if (fWindowState in [pvgwsNormal]) and
               (pvgwfResizableSW in fWindowFlags) and
               (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
               (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
    OK:=true;
   end else if (fWindowState in [pvgwsNormal]) and
               (pvgwfResizableSE in fWindowFlags) and
               (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
               (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
    OK:=true;
   end else if (fWindowState in [pvgwsNormal]) and
               (pvgwfResizableN in fWindowFlags) and
               (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
    OK:=true;
   end else if (fWindowState in [pvgwsNormal]) and
               (pvgwfResizableS in fWindowFlags) and
               (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
    OK:=true;
   end else if (fWindowState in [pvgwsNormal]) and
               (pvgwfResizableW in fWindowFlags) and
               (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) then begin
    OK:=true;
   end else if (fWindowState in [pvgwsNormal]) and
               (pvgwfResizableE in fWindowFlags) and
               (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) then begin
    OK:=true;
   end;
  end;
  if OK then begin
   result:=false;
  end else begin
   result:=inherited PointerEvent(aPointerEvent);
  end;
  if not result then begin
   OldSize:=fSize;
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     fMouseAction:=pvgwmaNone;
     fCursor:=pvgcArrow;
     if (aPointerEvent.Position.x>=0) and
        (aPointerEvent.Position.y>=0) and
        (aPointerEvent.Position.x<fSize.x) and
        (aPointerEvent.Position.y<fSize.y) then begin
      if (fWindowState in [pvgwsNormal]) and
         (pvgwfResizableNW in fWindowFlags) and
         (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
         (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
       fMouseAction:=pvgwmaSizeNW;
       fCursor:=pvgcNWSE;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableNE in fWindowFlags) and
                  (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                  (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
       fMouseAction:=pvgwmaSizeNE;
       fCursor:=pvgcNESW;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableSW in fWindowFlags) and
                  (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
                  (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
       fMouseAction:=pvgwmaSizeSW;
       fCursor:=pvgcNESW;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableSE in fWindowFlags) and
                  (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                  (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
       fMouseAction:=pvgwmaSizeSE;
       fCursor:=pvgcNWSE;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableN in fWindowFlags) and
                  (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
       fMouseAction:=pvgwmaSizeN;
       fCursor:=pvgcNS;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableS in fWindowFlags) and
                  (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
       fMouseAction:=pvgwmaSizeS;
       fCursor:=pvgcNS;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableW in fWindowFlags) and
                  (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) then begin
       fMouseAction:=pvgwmaSizeW;
       fCursor:=pvgcEW;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableE in fWindowFlags) and
                  (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) then begin
       fMouseAction:=pvgwmaSizeE;
       fCursor:=pvgcEW;
      end else if (pvgwfMovable in fWindowFlags) and
                  (aPointerEvent.Position.y<Skin.fWindowHeaderHeight) then begin
       if fWindowState=pvgwsMaximized then begin
        fSavedPosition.x:=Max(0.0,aPointerEvent.Position.x-(fSavedSize.x*0.5));
        fSavedPosition.y:=fPosition.y;
        WindowState:=pvgwsNormal;
       end;
       fMouseAction:=pvgwmaMove;
       fCursor:=pvgcMove;
      end;
      RequestFocus;
     end;
{    if not (pvgwfFocused in fWidgetFlags) then begin
      RequestFocus;
     end;}
    end;
    POINTEREVENT_UP:begin
     fMouseAction:=pvgwmaNone;
     fCursor:=pvgcArrow;
    end;
    POINTEREVENT_MOTION:begin
     if fMouseAction=pvgwmaNone then begin
      fCursor:=pvgcArrow;
      if (fWindowState in [pvgwsNormal]) and
         (pvgwfResizableNW in fWindowFlags) and
         (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
         (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
       fCursor:=pvgcNWSE;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableNE in fWindowFlags) and
                  (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                  (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
       fCursor:=pvgcNESW;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableSW in fWindowFlags) and
                  (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) and
                  (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
       fCursor:=pvgcNESW;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableSE in fWindowFlags) and
                  (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) and
                  (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
       fCursor:=pvgcNWSE;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableN in fWindowFlags) and
                  (aPointerEvent.Position.y<Skin.fWindowResizeGripSize) then begin
       fCursor:=pvgcNS;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableS in fWindowFlags) and
                  (aPointerEvent.Position.y>(fSize.y-Skin.fWindowResizeGripSize)) then begin
       fCursor:=pvgcNS;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableW in fWindowFlags) and
                  (aPointerEvent.Position.x<Skin.fWindowResizeGripSize) then begin
       fCursor:=pvgcEW;
      end else if (fWindowState in [pvgwsNormal]) and
                  (pvgwfResizableE in fWindowFlags) and
                  (aPointerEvent.Position.x>(fSize.x-Skin.fWindowResizeGripSize)) then begin
       fCursor:=pvgcEW;
      end;
     end;
    end;
    POINTEREVENT_DRAG:begin
     if assigned(fParent) and (fParent is TpvGUIWindow) and (pvgwfHeader in (fParent as TpvGUIWindow).fWindowFlags) then begin
      MinimumPosition:=TpvVector2.Create(0.0,Skin.fWindowHeaderHeight);
     end else begin
      MinimumPosition:=TpvVector2.Null;
     end;
     if WindowState=pvgwsMinimized then begin
      MinimumSize:=TpvVector2.Create(Skin.fMinimizedWindowMinimumWidth,Skin.fMinimizedWindowMinimumHeight);
     end else begin
      MinimumSize:=TpvVector2.Create(Skin.fWindowMinimumWidth,Skin.fWindowMinimumHeight);
     end;
     if assigned(fButtonPanel) then begin
      MinimumSize.x:=Max(MinimumSize.x,fButtonPanel.Size.x+(Skin.fSpacing*2.0));
     end;
     //writeln(aPointerEvent.RelativePosition.x:1:8,' ',aPointerEvent.RelativePosition.y:1:8,' ',int32(fMouseAction),' ',TpvPtrUInt(self));
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
      Size.Vector:=Clamp(fSize,MinimumSize,(fParent as TpvGUIWidget).fSize-fPosition);
      Position.Vector:=Clamp(fPosition,MinimumPosition,(fParent as TpvGUIWidget).fSize-fSize);
     end else begin
      Size.Vector:=Maximum(fSize,MinimumSize);
      Position.Vector:=Maximum(fPosition,MinimumPosition);
     end;
     if fSize<>OldSize then begin
      PerformLayout;
     end;
    end;
   end;
  end;
  result:=true;
 end;
end;

function TpvGUIWindow.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
  inherited Scrolled(aPosition,aRelativeAmount);
  result:=true;
 end;
end;

procedure TpvGUIWindow.Update;
begin
 Skin.DrawWindow(fCanvas,self);
 inherited Update;
end;

procedure TpvGUIWindow.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIMessageDialogButton.Create(const aID:TpvInt32;
                                             const aCaption:TpvUTF8String;
                                             const aKeyCode:TpvInt32=KEYCODE_UNKNOWN;
                                             const aIcon:TObject=nil;
                                             const aIconHeight:TpvFloat=24.0);
begin
 fID:=aID;
 fCaption:=aCaption;
 fIcon:=aIcon;
 fIconHeight:=aIconHeight;
 fKeyCode:=aKeyCode;
 fButton:=nil;
end;

constructor TpvGUIMessageDialog.Create(const aParent:TpvGUIObject;
                                       const aTitle:TpvUTF8String;
                                       const aMessage:TpvUTF8String;
                                       const aButtons:array of TpvGUIMessageDialogButton;
                                       const aIcon:TObject=nil;
                                       const aIconHeight:TpvFloat=36.0);
var Index:TpvSizeInt;
    MessageDialogButton:PpvGUIMessageDialogButton;
begin

 inherited Create(aParent);

 fOnButtonClick:=nil;

 SetWindowFlags((fWindowFlags-[pvgwfResizableNW,
                               pvgwfResizableNE,
                               pvgwfResizableSW,
                               pvgwfResizableSE,
                               pvgwfResizableN,
                               pvgwfResizableS,
                               pvgwfResizableW,
                               pvgwfResizableE])+[pvgwfModal]);

 fTitle:=aTitle;

 fContent.fLayout:=TpvGUIBoxLayout.Create(fContent,
                                          pvglaMiddle,
                                          pvgloVertical,
                                          10,
                                          10);

 fMessagePanel:=TpvGUIPanel.Create(fContent);
 fMessagePanel.fLayout:=TpvGUIBoxLayout.Create(fMessagePanel,
                                               pvglaMiddle,
                                               pvgloHorizontal,
                                               15,
                                               10);

 if assigned(aIcon) then begin
  fMessageImage:=TpvGUIImage.Create(fMessagePanel,aIcon);
  fMessageImage.FixedHeight:=aIconHeight;
 end;

 fMessageLabel:=TpvGUILabel.Create(fMessagePanel);
 fMessageLabel.fCaption:=aMessage;

 fMessageDialogButtonPanel:=TpvGUIPanel.Create(fContent);
 fMessageDialogButtonPanel.fLayout:=TpvGUIBoxLayout.Create(fMessageDialogButtonPanel,
                                                           pvglaMiddle,
                                                           pvgloHorizontal,
                                                           0,
                                                           10);

 if length(aButtons)=0 then begin

  SetLength(fButtons,1);
  fButtons[0]:=TpvGUIMessageDialogButton.Create(0,'OK',KEYCODE_RETURN);

 end else begin

  SetLength(fButtons,length(aButtons));

 end;

 for Index:=0 to length(fButtons)-1 do begin

  fButtons[Index]:=aButtons[Index];

  MessageDialogButton:=@fButtons[Index];

  MessageDialogButton^.fButton:=TpvGUIButton.Create(fMessageDialogButtonPanel);
  MessageDialogButton^.fButton.fCaption:=MessageDialogButton^.fCaption;
  MessageDialogButton^.fButton.fIconPosition:=pvgbipLeftCentered;
  MessageDialogButton^.fButton.fIcon:=MessageDialogButton^.fIcon;
  MessageDialogButton^.fButton.fIconHeight:=MessageDialogButton^.fIconHeight;
  MessageDialogButton^.fButton.OnClick:=MessageDialogOnButtonClick;

 end;

 fSize:=TpvVector2.Null;

 Center;

{if length(fButtons)>0 then begin
  fButtons[0].fButton.RequestFocus;
 end else begin
  RequestFocus;
 end;}

 RequestFocus;

end;

constructor TpvGUIMessageDialog.Create(const aParent:TpvGUIObject;
                                       const aTitle:TpvUTF8String;
                                       const aMessage:TpvUTF8String;
                                       const aButtons:array of TpvUTF8String;
                                       const aIcon:TObject=nil;
                                       const aIconHeight:TpvFloat=36.0);
var Index:TpvSizeInt;
    MessageDialogButtons:TpvGUIMessageDialogButtons;
begin
 MessageDialogButtons:=nil;
 try
  SetLength(MessageDialogButtons,length(aButtons));
  for Index:=0 to length(aButtons)-1 do begin
   MessageDialogButtons[Index]:=TpvGUIMessageDialogButton.Create(Index,aButtons[Index]);
  end;
  Create(aParent,
         aTitle,
         aMessage,
         MessageDialogButtons,
         aIcon,
         aIconHeight);
 finally
  MessageDialogButtons:=nil;
 end;
end;

constructor TpvGUIMessageDialog.Create(const aParent:TpvGUIObject;
                                       const aTitle:TpvUTF8String;
                                       const aMessage:TpvUTF8String;
                                       const aIcon:TObject=nil;
                                       const aIconHeight:TpvFloat=36.0);
var MessageDialogButtons:TpvGUIMessageDialogButtons;
begin
 MessageDialogButtons:=nil;
 Create(aParent,
        aTitle,
        aMessage,
        MessageDialogButtons,
        aIcon,
        aIconHeight);
end;

destructor TpvGUIMessageDialog.Destroy;
begin
 inherited Destroy;
end;

function TpvGUIMessageDialog.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var Index:TpvSizeInt;
    MessageDialogButton:PpvGUIMessageDialogButton;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if (aKeyEvent.KeyEventType=KEYEVENT_TYPED) and not result then begin
  for Index:=0 to length(fButtons) do begin
   MessageDialogButton:=@fButtons[Index];
   if MessageDialogButton^.fKeyCode=aKeyEvent.KeyCode then begin
    if assigned(fOnButtonClick) then begin
     fOnButtonClick(self,MessageDialogButton^.fID);
    end;
    result:=true;
    DisposeWindow;
    break;
   end;
  end;
 end;
end;

procedure TpvGUIMessageDialog.MessageDialogOnButtonClick(const aSender:TpvGUIObject);
var Index:TpvSizeInt;
    MessageDialogButton:PpvGUIMessageDialogButton;
begin
 for Index:=0 to length(fButtons) do begin
  MessageDialogButton:=@fButtons[Index];
  if MessageDialogButton^.fButton=aSender then begin
   if assigned(fOnButtonClick) then begin
    fOnButtonClick(self,MessageDialogButton^.fID);
   end;
   break;
  end;
 end;
 DisposeWindow;
end;

constructor TpvGUIPopup.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent.fInstance);

 fWindowFlags:=fWindowFlags-[pvgwfHeader,
                             pvgwfMovable,
                             pvgwfResizableNW,
                             pvgwfResizableNE,
                             pvgwfResizableSW,
                             pvgwfResizableSE,
                             pvgwfResizableN,
                             pvgwfResizableS,
                             pvgwfResizableW,
                             pvgwfResizableE];

 if aParent is TpvGUIWidget then begin
  fParentWidget:=aParent as TpvGUIWidget;
 end else begin
  fParentWidget:=nil;
 end;

 if aParent is TpvGUIHolder then begin
  fParentHolder:=aParent as TpvGUIHolder;
 end else begin
  fParentHolder:=nil;
 end;

 if not assigned(fParentHolder) then begin
  if assigned(fParentWidget) then begin
   fParentHolder:=fParentWidget.GetWindow;
  end;
  if not assigned(fParentHolder) then begin
   fParentHolder:=fInstance;
  end;
 end;

 if assigned(fParentWidget) then begin
  fParentWidget.fPopups.Add(self);
 end;

 fAnchorSide:=pvgpasNone;

 fAnchorPosition:=TpvVector2.Null;

 fAnchorPositionProperty:=TpvVector2Property.Create(@fAnchorPosition);

 fAnchorOffset:=TpvVector2.Null;

 fAnchorOffsetProperty:=TpvVector2Property.Create(@fAnchorOffset);

 fAnchorSideOffset:=TpvVector2.Null;

end;

destructor TpvGUIPopup.Destroy;
var Widget:TpvGUIWidget;
begin
 Widget:=fParentWidget;
 fParentHolder:=nil;
 fParentWidget:=nil;
 if assigned(Widget) then begin
  Widget.fPopups.Remove(self);
 end;
 FreeAndNil(fAnchorPositionProperty);
 FreeAndNil(fAnchorOffsetProperty);
 inherited Destroy;
end;

procedure TpvGUIPopup.PerformLayout;
begin
 inherited PerformLayout;
end;

procedure TpvGUIPopup.UpdatePosition;
begin
 if assigned(fParentWidget) then begin
  case fAnchorSide of
   pvgpasLeft:begin
    fAnchorSideOffset:=TpvVector2.Create(-fSize.x,0.0);
   end;
   pvgpasRight:begin
    fAnchorSideOffset:=TpvVector2.Create(fParentWidget.fSize.x,0.0);
   end;
   pvgpasTop:begin
    fAnchorSideOffset:=TpvVector2.Create(0.0,-fSize.y);
   end;
   pvgpasBottom:begin
    fAnchorSideOffset:=TpvVector2.Create(0.0,fParentWidget.fSize.y);
   end;
   else {pvgpasNone:}begin
    fAnchorSideOffset:=TpvVector2.Null;
   end;
  end;
  Position.Vector:=fParentWidget.AbsolutePosition+fAnchorPosition+fAnchorOffset+fAnchorSideOffset;
 end;
end;

procedure TpvGUIPopup.Update;
begin

 if not Visible then begin
  exit;
 end;

 if not RecursiveVisible then begin
  Visible:=false;
  exit;
 end;

 UpdatePosition;

 inherited Update;

end;

procedure TpvGUIPopup.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIImage.Create(const aParent:TpvGUIObject;const aImage:TObject);
begin
 inherited Create(aParent);
 fImage:=aImage;
end;

destructor TpvGUIImage.Destroy;
begin
 inherited Destroy;
end;

function TpvGUIImage.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetImagePreferredSize(self);
end;

procedure TpvGUIImage.Update;
begin
 Skin.DrawImage(fCanvas,self);
 inherited Update;
end;

procedure TpvGUIImage.Draw;
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

function TpvGUILabel.GetFontSize:TpvFloat;
begin
 if assigned(Skin) and IsZero(fFontSize) then begin
  result:=Skin.fLabelFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TpvGUILabel.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fLabelFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

function TpvGUILabel.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetLabelPreferredSize(self);
end;

function TpvGUILabel.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
end;

function TpvGUILabel.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  result:=inherited PointerEvent(aPointerEvent);
 end;
end;

function TpvGUILabel.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
  result:=inherited Scrolled(aPosition,aRelativeAmount);
 end;
end;

procedure TpvGUILabel.Update;
begin
 Skin.DrawLabel(fCanvas,self);
 inherited Update;
end;

procedure TpvGUILabel.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIButton.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 Include(fWidgetFlags,pvgwfTabStop);
 Include(fWidgetFlags,pvgwfDrawFocus);

 fButtonFlags:=[pvgbfNormalButton];

 fButtonGroup:=TpvGUIButtonGroup.Create(false);

 fCaption:='Button';

 fIconPosition:=pvgbipLeftCentered;

 fIcon:=nil;

 fIconHeight:=0.0;

 fOnClick:=nil;

 fOnChange:=nil;

end;

destructor TpvGUIButton.Destroy;
begin
 FreeAndNil(fButtonGroup);
 inherited Destroy;
end;

function TpvGUIButton.GetDown:boolean;
begin
 result:=pvgbfDown in fButtonFlags;
end;

procedure TpvGUIButton.SetDown(const aDown:boolean);
begin
 if aDown then begin
  Include(fButtonFlags,pvgbfDown);
 end else begin
  Exclude(fButtonFlags,pvgbfDown);
 end;
end;

function TpvGUIButton.GetFontSize:TpvFloat;
begin
 if assigned(Skin) and IsZero(fFontSize) then begin
  result:=Skin.fButtonFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TpvGUIButton.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fButtonFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

function TpvGUIButton.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetButtonPreferredSize(self);
end;

procedure TpvGUIButton.ProcessDown(const aPosition:TpvVector2);
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildButton:TpvGUIButton;
    OldDown:boolean;
begin
 OldDown:=Down;
 if pvgbfRadioButton in fButtonFlags then begin
  if assigned(fButtonGroup) and (fButtonGroup.Count>0) then begin
   for ChildIndex:=0 to fButtonGroup.Count-1 do begin
    ChildButton:=fButtonGroup.Items[ChildIndex];
    if (ChildButton<>self) and
       ((ChildButton.fButtonFlags*[pvgbfRadioButton,pvgbfDown])=[pvgbfRadioButton,pvgbfDown]) then begin
     ChildButton.Down:=false;
     if assigned(ChildButton.fOnChange) then begin
      ChildButton.fOnChange(ChildButton,false);
     end;
    end;
   end;
  end else if assigned(fParent) then begin
   for ChildIndex:=0 to fParent.fChildren.Count-1 do begin
    Child:=fParent.fChildren.Items[ChildIndex];
    if assigned(Child) and
       (Child<>self) and
       (Child is TpvGUIButton) and
       (((Child as TpvGUIButton).fButtonFlags*[pvgbfRadioButton,pvgbfDown])=[pvgbfRadioButton,pvgbfDown]) then begin
     ChildButton:=Child as TpvGUIButton;
     ChildButton.Down:=false;
     if assigned(ChildButton.fOnChange) then begin
      ChildButton.fOnChange(ChildButton,false);
     end;
    end;
   end;
  end;
 end;
 if pvgbfPopupButton in fButtonFlags then begin
  if assigned(fParent) then begin
   for ChildIndex:=0 to fParent.fChildren.Count-1 do begin
    Child:=fParent.fChildren.Items[ChildIndex];
    if assigned(Child) and
       (Child<>self) and
       (Child is TpvGUIButton) and
       (((Child as TpvGUIButton).fButtonFlags*[pvgbfPopupButton,pvgbfDown])=[pvgbfPopupButton,pvgbfDown]) then begin
     ChildButton:=Child as TpvGUIButton;
     ChildButton.Down:=false;
     if assigned(ChildButton.fOnChange) then begin
      ChildButton.fOnChange(ChildButton,false);
     end;
    end;
   end;
  end;
 end;
 if pvgbfToggleButton in fButtonFlags then begin
  Down:=not Down;
 end else begin
  Down:=true;
 end;
 if (OldDown<>Down) and assigned(OnChange) then begin
  OnChange(self,Down);
 end;
end;

procedure TpvGUIButton.ProcessUp(const aPosition:TpvVector2);
var OldDown:boolean;
begin
 OldDown:=Down;
 if assigned(fOnClick) and Contains(aPosition) then begin
  fOnClick(self);
 end;
 if pvgbfNormalButton in fButtonFlags then begin
  Down:=false;
 end;
 if (OldDown<>Down) and assigned(OnChange) then begin
  OnChange(self,Down);
 end;
end;

function TpvGUIButton.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if Enabled and not result then begin
  case aKeyEvent.KeyCode of
   KEYCODE_SPACE,KEYCODE_RETURN:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_DOWN:begin
      ProcessDown(fSize*0.5);
     end;
     KEYEVENT_UP:begin
      ProcessUp(fSize*0.5);
     end;
     KEYEVENT_TYPED:begin
     end;
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUIButton.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildButton:TpvGUIButton;
    OldDown:boolean;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  result:=inherited PointerEvent(aPointerEvent);
  if Enabled and (aPointerEvent.Button=BUTTON_LEFT) and not result then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     ProcessDown(aPointerEvent.Position);
     result:=true;
    end;
    POINTEREVENT_UP:begin
     ProcessUp(aPointerEvent.Position);
     result:=true;
    end;
    POINTEREVENT_MOTION:begin
    end;
    POINTEREVENT_DRAG:begin
    end;
   end;
  end;
 end;
end;

function TpvGUIButton.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
  result:=inherited Scrolled(aPosition,aRelativeAmount);
 end;
end;

procedure TpvGUIButton.Update;
begin
 Skin.DrawButton(fCanvas,self);
 inherited Update;
end;

procedure TpvGUIButton.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIRadioButton.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fButtonFlags:=(fButtonFlags-[pvgbfNormalButton])+[pvgbfRadioButton];
end;

constructor TpvGUIToggleButton.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fButtonFlags:=(fButtonFlags-[pvgbfNormalButton])+[pvgbfToggleButton];
end;

constructor TpvGUIPopupButton.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fButtonFlags:=(fButtonFlags-[pvgbfNormalButton])+[pvgbfToggleButton,pvgbfPopupButton];

 fPopup:=TpvGUIPopup.Create(self);
 fPopup.Visible:=false;
 fPopup.AnchorSide:=pvgpasBottom;
 fPopup.fSize:=TpvVector2.Create(160,80);
 fPopup.fFixedSize:=TpvVector2.Create(160,80);

end;

procedure TpvGUIPopupButton.PerformLayout;
begin
 inherited PerformLayout;
 if assigned(fPopup) and fPopup.Visible then begin
  fPopup.UpdatePosition;
  fPopup.PerformLayout;
 end;
end;

procedure TpvGUIPopupButton.SetDown(const aDown:boolean);
begin
 inherited SetDown(aDown);
 fPopup.Visible:=aDown;
 if aDown then begin
  fPopup.UpdatePosition;
  fPopup.PerformLayout;
  fPopup.RequestFocus;
 end;
end;

constructor TpvGUIPopupMenuButton.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fButtonFlags:=(fButtonFlags-[pvgbfNormalButton])+[pvgbfToggleButton,pvgbfPopupButton];

 fPopupMenu:=TpvGUIPopupMenu.Create(self);

end;

procedure TpvGUIPopupMenuButton.SetDown(const aDown:boolean);
begin
 inherited SetDown(aDown);
 if aDown then begin
  fPopupMenu.Activate(AbsolutePosition+TpvVector2.Create(0.0,fSize.y));
 end else begin
  fPopupMenu.Deactivate;
 end;
end;

procedure TpvGUIPopupMenuButton.Update;
begin
 if not fPopupMenu.Activated then begin
  inherited SetDown(false);
 end;
 inherited Update;
end;

constructor TpvGUIToolButton.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 fButtonFlags:=(fButtonFlags-[pvgbfNormalButton])+[pvgbfRadioButton,pvgbfToggleButton];
end;

constructor TpvGUICheckBox.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 Include(fWidgetFlags,pvgwfTabStop);
 Include(fWidgetFlags,pvgwfDrawFocus);

 fCheckBoxFlags:=[];

 fCheckBoxGroup:=TpvGUICheckBoxGroup.Create(false);

 fCaption:='';

 fOnChange:=nil;

end;

destructor TpvGUICheckBox.Destroy;
begin
 FreeAndNil(fCheckBoxGroup);
 inherited Destroy;
end;

function TpvGUICheckBox.GetPushed:boolean;
begin
 result:=pvgcbfPushed in fCheckBoxFlags;
end;

procedure TpvGUICheckBox.SetPushed(const aPushed:boolean);
begin
 if aPushed then begin
  Include(fCheckBoxFlags,pvgcbfPushed);
 end else begin
  Exclude(fCheckBoxFlags,pvgcbfPushed);
 end;
end;

function TpvGUICheckBox.GetChecked:boolean;
begin
 result:=pvgcbfChecked in fCheckBoxFlags;
end;

procedure TpvGUICheckBox.SetChecked(const aChecked:boolean);
var ChildIndex:TpvInt32;
    Child:TpvGUIObject;
    ChildCheckBox:TpvGUICheckBox;
begin
 if (pvgcbfChecked in fCheckBoxFlags)<>aChecked then begin
  if aChecked then begin
   Include(fCheckBoxFlags,pvgcbfChecked);
   if pvgcbfRadioCheckBox in fCheckBoxFlags then begin
    if assigned(fCheckBoxGroup) and (fCheckBoxGroup.Count>0) then begin
     for ChildIndex:=0 to fCheckBoxGroup.Count-1 do begin
      ChildCheckBox:=fCheckBoxGroup.Items[ChildIndex];
      if (ChildCheckBox<>self) and
         ((ChildCheckBox.fCheckBoxFlags*[pvgcbfRadioCheckBox,pvgcbfChecked])=[pvgcbfRadioCheckBox,pvgcbfChecked]) then begin
       Exclude(ChildCheckBox.fCheckBoxFlags,pvgcbfChecked);
       if assigned(ChildCheckBox.fOnChange) then begin
        ChildCheckBox.fOnChange(ChildCheckBox,false);
       end;
      end;
     end;
    end else if assigned(fParent) then begin
     for ChildIndex:=0 to fParent.fChildren.Count-1 do begin
      Child:=fParent.fChildren.Items[ChildIndex];
      if assigned(Child) and
         (Child<>self) and
         (Child is TpvGUICheckBox) and
         (((Child as TpvGUICheckBox).fCheckBoxFlags*[pvgcbfRadioCheckBox,pvgcbfChecked])=[pvgcbfRadioCheckBox,pvgcbfChecked]) then begin
       ChildCheckBox:=Child as TpvGUICheckBox;
       Exclude(ChildCheckBox.fCheckBoxFlags,pvgcbfChecked);
       if assigned(ChildCheckBox.fOnChange) then begin
        ChildCheckBox.fOnChange(ChildCheckBox,false);
       end;
      end;
     end;
    end;
   end;
   if assigned(fOnChange) then begin
    fOnChange(self,true);
   end;
  end else begin
   Exclude(fCheckBoxFlags,pvgcbfChecked);
   if assigned(fOnChange) then begin
    fOnChange(self,false);
   end;
  end;
 end;
end;

function TpvGUICheckBox.GetHighlightRect:TpvRect;
begin
 result:=inherited GetHighlightRect;
 result.Size:=Skin.fCheckBoxSize;
end;

function TpvGUICheckBox.GetFontSize:TpvFloat;
begin
 if assigned(Skin) and IsZero(fFontSize) then begin
  result:=Skin.fCheckBoxFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TpvGUICheckBox.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fCheckBoxFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

function TpvGUICheckBox.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetCheckBoxPreferredSize(self);
end;

function TpvGUICheckBox.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if Enabled and not result then begin
  case aKeyEvent.KeyCode of
   KEYCODE_SPACE,KEYCODE_RETURN:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if pvgcbfRadioCheckBox in fCheckBoxFlags then begin
       SetChecked(true);
      end else begin
       SetChecked(not GetChecked);
      end;
     end;
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUICheckBox.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  result:=inherited PointerEvent(aPointerEvent);
  if Enabled and not result then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     case aPointerEvent.Button of
      BUTTON_LEFT:begin
       if pvgcbfRadioCheckBox in fCheckBoxFlags then begin
        SetChecked(true);
       end else begin
        SetChecked(not GetChecked);
       end;
      end;
      BUTTON_MIDDLE:begin
      end;
      BUTTON_RIGHT:begin
      end;
     end;
    end;
    POINTEREVENT_UP:begin
    end;
    POINTEREVENT_MOTION:begin
    end;
    POINTEREVENT_DRAG:begin
    end;
   end;
   result:=true;
  end;
 end;
end;

function TpvGUICheckBox.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
  result:=inherited Scrolled(aPosition,aRelativeAmount);
 end;
end;

procedure TpvGUICheckBox.Update;
begin
 Skin.DrawCheckBox(fCanvas,self);
 inherited Update;
end;

procedure TpvGUICheckBox.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIRadioCheckBox.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);
 Include(fCheckBoxFlags,pvgcbfRadioCheckBox);
end;

constructor TpvGUITextEdit.Create(const aParent:TpvGUIObject);
var MenuItem:TpvGUIMenuItem;
begin

 inherited Create(aParent);

 Include(fWidgetFlags,pvgwfTabStop);
 Include(fWidgetFlags,pvgwfDrawFocus);

 fPopupMenu:=TpvGUIPopupMenu.Create(self);

 MenuItem:=TpvGUIMenuItem.Create(fPopupMenu);
 MenuItem.Caption:='Cut';
 MenuItem.ShortcutHint:='Ctrl-X';
 MenuItem.fIcon:=Skin.fIconContentCut;
 MenuItem.fIconHeight:=Skin.fIconPopupMenuHeight;
 MenuItem.OnClick:=PopupMenuOnCutClick;

 MenuItem:=TpvGUIMenuItem.Create(fPopupMenu);
 MenuItem.Caption:='Copy';
 MenuItem.ShortcutHint:='Ctrl-C';
 MenuItem.fIcon:=Skin.fIconContentCopy;
 MenuItem.fIconHeight:=Skin.fIconPopupMenuHeight;
 MenuItem.OnClick:=PopupMenuOnCopyClick;

 MenuItem:=TpvGUIMenuItem.Create(fPopupMenu);
 MenuItem.Caption:='Paste';
 MenuItem.ShortcutHint:='Ctrl-V';
 MenuItem.fIcon:=Skin.fIconContentPaste;
 MenuItem.fIconHeight:=Skin.fIconPopupMenuHeight;
 MenuItem.OnClick:=PopupMenuOnPasteClick;

 MenuItem:=TpvGUIMenuItem.Create(fPopupMenu);
 MenuItem.Caption:='Delete';
 MenuItem.ShortcutHint:='Del';
 MenuItem.fIcon:=Skin.fIconContentDelete;
 MenuItem.fIconHeight:=Skin.fIconPopupMenuHeight;
 MenuItem.OnClick:=PopupMenuOnDeleteClick;

 MenuItem:=TpvGUIMenuItem.Create(fPopupMenu);
 MenuItem.Caption:='-';

 MenuItem:=TpvGUIMenuItem.Create(fPopupMenu);
 MenuItem.Caption:='Select all';
 MenuItem.ShortcutHint:='Ctrl+A';
 MenuItem.fIcon:=Skin.fIconSelectAll;
 MenuItem.fIconHeight:=Skin.fIconPopupMenuHeight;
 MenuItem.OnClick:=PopupMenuOnSelectAllClick;

 MenuItem:=TpvGUIMenuItem.Create(fPopupMenu);
 MenuItem.Caption:='Select none';
 MenuItem.fIcon:=Skin.fIconSelectNone;
 MenuItem.fIconHeight:=Skin.fIconPopupMenuHeight;
 MenuItem.OnClick:=PopupMenuOnSelectNoneClick;

 fTextHorizontalAlignment:=pvgtaLeading;

 fTextVerticalAlignment:=pvgtaCenter;

 SetEditable(true);

 fSpinnable:=false;

 fText:='';

 fTextGlyphRects:=nil;

 fCountTextGlyphRects:=0;

 fTextOffset:=0.0;

 fTextCursorPositionOffset:=0;

 fTextCursorPositionIndex:=1;

 fTextSelectionStart:=0;

 fTextSelectionEnd:=0;

 fMinimumWidth:=0.0;

 fMinimumHeight:=0.0;

 fDragRect:=TpvRect.CreateRelative(TpvVector2.Null,TpvVector2.Null);

 fOnClick:=nil;

 fOnChange:=nil;

 fOnCheckText:=nil;

end;

destructor TpvGUITextEdit.Destroy;
begin

 fTextGlyphRects:=nil;

 inherited Destroy;

end;

function TpvGUITextEdit.GetFontSize:TpvFloat;
begin
 if assigned(Skin) and IsZero(fFontSize) then begin
  result:=Skin.fTextEditFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TpvGUITextEdit.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fTextEditFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

function TpvGUITextEdit.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetTextEditPreferredSize(self);
end;

function TpvGUITextEdit.GetEditable:boolean;
begin
 result:=fEditable;
end;

procedure TpvGUITextEdit.SetEditable(const aEditable:boolean);
begin
 fEditable:=aEditable;
 if fEditable then begin
  fCursor:=pvgcBeam;
 end else begin
  fCursor:=pvgcArrow;
 end;
end;

procedure TpvGUITextEdit.UpdateText;
begin
end;

function TpvGUITextEdit.CheckText(const aText:TpvUTF8String):boolean;
begin
 if assigned(fOnCheckText) then begin
  result:=fOnCheckText(aText);
 end else begin
  result:=true;
 end;
end;

function TpvGUITextEdit.GetText:TpvUTF8String;
begin
 result:=fText;
end;

procedure TpvGUITextEdit.SetText(const aText:TpvUTF8String);
begin

 if CheckText(aText) then begin

  if fText<>aText then begin

   fText:=aText;

   UpdateText;

  end else begin

   fText:=aText;

  end;

  fCountTextGlyphRects:=0;

  fTextOffset:=0.0;

  fTextCursorPositionOffset:=0;

  fTextCursorPositionIndex:=PUCUUTF8Length(fText)+1;

  fTextSelectionStart:=0;

  fTextSelectionEnd:=0;

 end;

end;

function TpvGUITextEdit.Enter:boolean;
begin
 result:=inherited Enter;
 pvApplication.Input.StartTextInput;
end;

function TpvGUITextEdit.Leave:boolean;
begin
 pvApplication.Input.StopTextInput;
 result:=inherited Leave;
end;

procedure TpvGUITextEdit.CutSelectedText;
var CurrentPosition,OtherPosition:TpvInt32;
    TemporaryUncheckedText:TpvUTF8String;
begin
 if (fTextSelectionStart>0) and
    (fTextSelectionEnd>0) then begin
  CurrentPosition:=PUCUUTF8GetCodeUnit(fText,Min(fTextSelectionStart,fTextSelectionEnd)-1);
  OtherPosition:=PUCUUTF8GetCodeUnit(fText,Max(fTextSelectionStart,fTextSelectionEnd)-1);
  pvApplication.Clipboard.SetText(Copy(fText,CurrentPosition,OtherPosition-CurrentPosition));
  TemporaryUncheckedText:=fText;
  Delete(TemporaryUncheckedText,CurrentPosition,OtherPosition-CurrentPosition);
  if CheckText(TemporaryUncheckedText) then begin
   fTextCursorPositionIndex:=CurrentPosition;
   fTextSelectionStart:=0;
   fTextSelectionEnd:=0;
   if fText<>TemporaryUncheckedText then begin
    fText:=TemporaryUncheckedText;
    UpdateText;
    if assigned(fOnChange) then begin
     fOnChange(self);
    end;
   end;
  end;
 end;
end;

procedure TpvGUITextEdit.CopySelectedText;
var CurrentPosition,OtherPosition:TpvInt32;
begin
 if (fTextSelectionStart>0) and
    (fTextSelectionEnd>0) then begin
  CurrentPosition:=PUCUUTF8GetCodeUnit(fText,Min(fTextSelectionStart,fTextSelectionEnd)-1);
  OtherPosition:=PUCUUTF8GetCodeUnit(fText,Max(fTextSelectionStart,fTextSelectionEnd)-1);
  pvApplication.Clipboard.SetText(Copy(fText,CurrentPosition,OtherPosition-CurrentPosition));
 end;
end;

procedure TpvGUITextEdit.PasteText;
var CurrentPosition,OtherPosition,TemporaryUncheckedTextCursorPositionIndex,
    TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd:TpvInt32;
    TemporaryUncheckedText,TemporaryText:TpvUTF8String;
begin
 TemporaryUncheckedText:=fText;
 TemporaryUncheckedTextCursorPositionIndex:=fTextCursorPositionIndex;
 TemporaryUncheckedTextSelectionStart:=fTextSelectionStart;
 TemporaryUncheckedTextSelectionEnd:=fTextSelectionEnd;
 if (TemporaryUncheckedTextSelectionStart>0) and
    (TemporaryUncheckedTextSelectionEnd>0) then begin
  CurrentPosition:=PUCUUTF8GetCodeUnit(TemporaryUncheckedText,Min(TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd)-1);
  OtherPosition:=PUCUUTF8GetCodeUnit(TemporaryUncheckedText,Max(TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd)-1);
  Delete(TemporaryUncheckedText,CurrentPosition,OtherPosition-CurrentPosition);
  TemporaryUncheckedTextCursorPositionIndex:=CurrentPosition;
  TemporaryUncheckedTextSelectionStart:=0;
  TemporaryUncheckedTextSelectionEnd:=0;
 end;
 if pvApplication.Clipboard.HasText then begin
  TemporaryText:=pvApplication.Clipboard.GetText;
  if length(TemporaryText)>0 then begin
   Insert(TemporaryText,
          TemporaryUncheckedText,
          PUCUUTF8GetCodeUnit(TemporaryUncheckedText,TemporaryUncheckedTextCursorPositionIndex-1));
   inc(TemporaryUncheckedTextCursorPositionIndex,PUCUUTF8Length(TemporaryText));
  end;
 end;
 if CheckText(TemporaryUncheckedText) then begin
  fTextCursorPositionIndex:=TemporaryUncheckedTextCursorPositionIndex;
  fTextSelectionStart:=TemporaryUncheckedTextSelectionStart;
  fTextSelectionEnd:=TemporaryUncheckedTextSelectionEnd;
  if fText<>TemporaryUncheckedText then begin
   fText:=TemporaryUncheckedText;
   UpdateText;
   if assigned(fOnChange) then begin
    fOnChange(self);
   end;
  end;
 end;
end;

procedure TpvGUITextEdit.DeleteSelectedText;
var CurrentPosition,OtherPosition:TpvInt32;
    TemporaryUncheckedText:TpvUTF8String;
begin
 if (fTextSelectionStart>0) and
    (fTextSelectionEnd>0) then begin
  CurrentPosition:=PUCUUTF8GetCodeUnit(fText,Min(fTextSelectionStart,fTextSelectionEnd)-1);
  OtherPosition:=PUCUUTF8GetCodeUnit(fText,Max(fTextSelectionStart,fTextSelectionEnd)-1);
  TemporaryUncheckedText:=fText;
  Delete(TemporaryUncheckedText,CurrentPosition,OtherPosition-CurrentPosition);
  if CheckText(TemporaryUncheckedText) then begin
   fTextCursorPositionIndex:=CurrentPosition;
   fTextSelectionStart:=0;
   fTextSelectionEnd:=0;
   if fText<>TemporaryUncheckedText then begin
    fText:=TemporaryUncheckedText;
    UpdateText;
    if assigned(fOnChange) then begin
     fOnChange(self);
    end;
   end;
  end;
 end;
end;

procedure TpvGUITextEdit.SelectAll;
begin
 fTextSelectionStart:=1;
 fTextSelectionEnd:=PUCUUTF8Length(Text)+1;
end;

procedure TpvGUITextEdit.SelectNone;
begin
 fTextSelectionStart:=0;
 fTextSelectionEnd:=0;
end;

procedure TpvGUITextEdit.PopupMenuOnCutClick(const aSender:TpvGUIObject);
begin
 CutSelectedText;
end;

procedure TpvGUITextEdit.PopupMenuOnCopyClick(const aSender:TpvGUIObject);
begin
 CopySelectedText;
end;

procedure TpvGUITextEdit.PopupMenuOnPasteClick(const aSender:TpvGUIObject);
begin
 PasteText;
end;

procedure TpvGUITextEdit.PopupMenuOnDeleteClick(const aSender:TpvGUIObject);
begin
 DeleteSelectedText;
end;

procedure TpvGUITextEdit.PopupMenuOnSelectAllClick(const aSender:TpvGUIObject);
begin
 SelectAll;
end;

procedure TpvGUITextEdit.PopupMenuOnSelectNoneClick(const aSender:TpvGUIObject);
begin
 SelectNone;
end;

function TpvGUITextEdit.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var CurrentPosition,OtherPosition,TemporaryUncheckedTextCursorPositionIndex,
    TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd:TpvInt32;
    TemporaryText,TemporaryUncheckedText:TpvUTF8String;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if Enabled and not result then begin
  case aKeyEvent.KeyEventType of
   KEYEVENT_DOWN:begin
    case aKeyEvent.KeyCode of
     KEYCODE_APPLICATION:begin
      result:=true;
     end;
    end;
   end;
   KEYEVENT_UP:begin
    case aKeyEvent.KeyCode of
     KEYCODE_APPLICATION:begin
      if assigned(fPopupMenu) then begin
       fPopupMenu.Activate(AbsolutePosition+(fSize*0.5));
      end;
      result:=true;
     end;
    end;
   end;
   KEYEVENT_TYPED:begin
    case aKeyEvent.KeyCode of
     KEYCODE_LEFT:begin
      if KEYMODIFIER_SHIFT in aKeyEvent.KeyModifiers then begin
       if fTextSelectionStart<1 then begin
        fTextSelectionStart:=fTextCursorPositionIndex;
       end;
       fTextCursorPositionIndex:=Min(Max(fTextCursorPositionIndex-1,1),PUCUUTF8Length(fText)+1);
       fTextSelectionEnd:=fTextCursorPositionIndex;
      end else begin
       fTextSelectionStart:=0;
       fTextSelectionEnd:=0;
       fTextCursorPositionIndex:=Min(Max(fTextCursorPositionIndex-1,1),PUCUUTF8Length(fText)+1);
      end;
      result:=true;
     end;
     KEYCODE_RIGHT:begin
      if KEYMODIFIER_SHIFT in aKeyEvent.KeyModifiers then begin
       if fTextSelectionStart<1 then begin
        fTextSelectionStart:=fTextCursorPositionIndex;
       end;
       fTextCursorPositionIndex:=Min(Max(fTextCursorPositionIndex+1,1),PUCUUTF8Length(fText)+1);
       fTextSelectionEnd:=fTextCursorPositionIndex;
      end else begin
       fTextSelectionStart:=0;
       fTextSelectionEnd:=0;
       fTextCursorPositionIndex:=Min(Max(fTextCursorPositionIndex+1,1),PUCUUTF8Length(fText)+1);
      end;
      result:=true;
     end;
     KEYCODE_HOME:begin
      if KEYMODIFIER_SHIFT in aKeyEvent.KeyModifiers then begin
       if fTextSelectionStart<1 then begin
        fTextSelectionStart:=fTextCursorPositionIndex;
       end;
       fTextCursorPositionIndex:=1;
       fTextSelectionEnd:=fTextCursorPositionIndex;
      end else begin
       fTextSelectionStart:=0;
       fTextSelectionEnd:=0;
       fTextCursorPositionIndex:=1;
      end;
      result:=true;
     end;
     KEYCODE_END:begin
      if KEYMODIFIER_SHIFT in aKeyEvent.KeyModifiers then begin
       if fTextSelectionStart<1 then begin
        fTextSelectionStart:=fTextCursorPositionIndex;
       end;
       fTextCursorPositionIndex:=PUCUUTF8Length(fText)+1;
       fTextSelectionEnd:=fTextCursorPositionIndex;
      end else begin
       fTextSelectionStart:=0;
       fTextSelectionEnd:=0;
       fTextCursorPositionIndex:=PUCUUTF8Length(fText)+1;
      end;
      result:=true;
     end;
     KEYCODE_BACKSPACE:begin
      if (fTextSelectionStart>0) and
         (fTextSelectionEnd>0) then begin
       CurrentPosition:=PUCUUTF8GetCodeUnit(fText,Min(fTextSelectionStart,fTextSelectionEnd)-1);
       OtherPosition:=PUCUUTF8GetCodeUnit(fText,Max(fTextSelectionStart,fTextSelectionEnd)-1);
       TemporaryUncheckedText:=fText;
       Delete(TemporaryUncheckedText,CurrentPosition,OtherPosition-CurrentPosition);
       if CheckText(TemporaryUncheckedText) then begin
        fTextCursorPositionIndex:=CurrentPosition;
        fTextSelectionStart:=0;
        fTextSelectionEnd:=0;
        if fText<>TemporaryUncheckedText then begin
         fText:=TemporaryUncheckedText;
         UpdateText;
         if assigned(fOnChange) then begin
          fOnChange(self);
         end;
        end;
       end;
      end else begin
       CurrentPosition:=PUCUUTF8GetCodeUnit(fText,fTextCursorPositionIndex-1);
       if (CurrentPosition>1) and (CurrentPosition<=(length(fText)+1)) then begin
        OtherPosition:=CurrentPosition;
        PUCUUTF8Dec(fText,OtherPosition);
        if (OtherPosition>0) and (OtherPosition<=length(fText)) and (OtherPosition<CurrentPosition) then begin
         TemporaryUncheckedText:=fText;
         Delete(TemporaryUncheckedText,OtherPosition,CurrentPosition-OtherPosition);
         if CheckText(TemporaryUncheckedText) then begin
          dec(fTextCursorPositionIndex);
          if fText<>TemporaryUncheckedText then begin
           fText:=TemporaryUncheckedText;
           UpdateText;
           if assigned(fOnChange) then begin
            fOnChange(self);
           end;
          end;
         end;
        end;
       end;
      end;
      result:=true;
     end;
     KEYCODE_INSERT:begin
      TemporaryUncheckedText:=fText;
      TemporaryUncheckedTextCursorPositionIndex:=fTextCursorPositionIndex;
      TemporaryUncheckedTextSelectionStart:=fTextSelectionStart;
      TemporaryUncheckedTextSelectionEnd:=fTextSelectionEnd;
      if (TemporaryUncheckedTextSelectionStart>0) and
         (TemporaryUncheckedTextSelectionEnd>0) then begin
       CurrentPosition:=PUCUUTF8GetCodeUnit(TemporaryUncheckedText,Min(TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd)-1);
       OtherPosition:=PUCUUTF8GetCodeUnit(TemporaryUncheckedText,Max(TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd)-1);
       Delete(TemporaryUncheckedText,CurrentPosition,OtherPosition-CurrentPosition);
       TemporaryUncheckedTextCursorPositionIndex:=CurrentPosition;
       TemporaryUncheckedTextSelectionStart:=0;
       TemporaryUncheckedTextSelectionEnd:=0;
      end;
      if KEYMODIFIER_SHIFT in aKeyEvent.KeyModifiers then begin
       if pvApplication.Clipboard.HasText then begin
        TemporaryText:=pvApplication.Clipboard.GetText;
        if length(TemporaryText)>0 then begin
         Insert(TemporaryText,
                TemporaryUncheckedText,
                PUCUUTF8GetCodeUnit(TemporaryUncheckedText,TemporaryUncheckedTextCursorPositionIndex-1));
         inc(TemporaryUncheckedTextCursorPositionIndex,PUCUUTF8Length(TemporaryText));
        end;
       end;
      end else begin
       Insert(#32,
              TemporaryUncheckedText,
              PUCUUTF8GetCodeUnit(TemporaryUncheckedText,TemporaryUncheckedTextCursorPositionIndex-1));
      end;
      if CheckText(TemporaryUncheckedText) then begin
       fTextCursorPositionIndex:=TemporaryUncheckedTextCursorPositionIndex;
       fTextSelectionStart:=TemporaryUncheckedTextSelectionStart;
       fTextSelectionEnd:=TemporaryUncheckedTextSelectionEnd;
       if fText<>TemporaryUncheckedText then begin
        fText:=TemporaryUncheckedText;
        UpdateText;
        if assigned(fOnChange) then begin
         fOnChange(self);
        end;
       end;
      end;
      result:=true;
     end;
     KEYCODE_DELETE:begin
      if (fTextSelectionStart>0) and
         (fTextSelectionEnd>0) then begin
       if KEYMODIFIER_SHIFT in aKeyEvent.KeyModifiers then begin
        CutSelectedText;
       end else begin
        DeleteSelectedText;
       end;
      end else begin
       CurrentPosition:=PUCUUTF8GetCodeUnit(fText,fTextCursorPositionIndex-1);
       if (CurrentPosition>0) and (CurrentPosition<=length(fText)) then begin
        OtherPosition:=CurrentPosition;
        PUCUUTF8Inc(fText,OtherPosition);
        if (OtherPosition>1) and (OtherPosition<=(length(fText)+1)) and (CurrentPosition<OtherPosition) then begin
         TemporaryUncheckedText:=fText;
         Delete(TemporaryUncheckedText,CurrentPosition,OtherPosition-CurrentPosition);
         if (fText<>TemporaryUncheckedText) and CheckText(TemporaryUncheckedText) then begin
          fText:=TemporaryUncheckedText;
          UpdateText;
          if assigned(fOnChange) then begin
           fOnChange(self);
          end;
         end;
        end;
       end;
      end;
      result:=true;
     end;
     KEYCODE_A:begin
      if KEYMODIFIER_CTRL in aKeyEvent.KeyModifiers then begin
       fTextSelectionStart:=1;
       fTextSelectionEnd:=PUCUUTF8Length(fText)+1;
       result:=true;
      end;
     end;
     KEYCODE_C:begin
      if KEYMODIFIER_CTRL in aKeyEvent.KeyModifiers then begin
       CopySelectedText;
       result:=true;
      end;
     end;
     KEYCODE_V:begin
      if KEYMODIFIER_CTRL in aKeyEvent.KeyModifiers then begin
       PasteText;
       result:=true;
      end;
     end;
     KEYCODE_X:begin
      if KEYMODIFIER_CTRL in aKeyEvent.KeyModifiers then begin
       CutSelectedText;
       result:=true;
      end;
     end;
    end;
   end;
   KEYEVENT_UNICODE:begin
    TemporaryUncheckedText:=fText;
    TemporaryUncheckedTextCursorPositionIndex:=fTextCursorPositionIndex;
    TemporaryUncheckedTextSelectionStart:=fTextSelectionStart;
    TemporaryUncheckedTextSelectionEnd:=fTextSelectionEnd;
    if (TemporaryUncheckedTextSelectionStart>0) and
       (TemporaryUncheckedTextSelectionEnd>0) then begin
     CurrentPosition:=PUCUUTF8GetCodeUnit(TemporaryUncheckedText,Min(TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd)-1);
     OtherPosition:=PUCUUTF8GetCodeUnit(TemporaryUncheckedText,Max(TemporaryUncheckedTextSelectionStart,TemporaryUncheckedTextSelectionEnd)-1);
     Delete(TemporaryUncheckedText,CurrentPosition,OtherPosition-CurrentPosition);
     TemporaryUncheckedTextCursorPositionIndex:=CurrentPosition;
     TemporaryUncheckedTextSelectionStart:=0;
     TemporaryUncheckedTextSelectionEnd:=0;
    end;
    Insert(PUCUUTF32CharToUTF8(aKeyEvent.KeyCode),
           TemporaryUncheckedText,
           PUCUUTF8GetCodeUnit(TemporaryUncheckedText,TemporaryUncheckedTextCursorPositionIndex-1));
    inc(TemporaryUncheckedTextCursorPositionIndex);
    if CheckText(TemporaryUncheckedText) then begin
     fTextCursorPositionIndex:=TemporaryUncheckedTextCursorPositionIndex;
     fTextSelectionStart:=TemporaryUncheckedTextSelectionStart;
     fTextSelectionEnd:=TemporaryUncheckedTextSelectionEnd;
     if fText<>TemporaryUncheckedText then begin
      fText:=TemporaryUncheckedText;
      UpdateText;
      if assigned(fOnChange) then begin
       fOnChange(self);
      end;
     end;
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUITextEdit.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Index:TpvInt32;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  result:=inherited PointerEvent(aPointerEvent);
  if not result then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     case aPointerEvent.Button of
      BUTTON_LEFT:begin
       fTextSelectionStart:=0;
       fTextSelectionEnd:=0;
       fTextCursorPositionIndex:=1;
       if fCountTextGlyphRects>0 then begin
        if aPointerEvent.Position.x>=fTextGlyphRects[fCountTextGlyphRects-1].Right then begin
         fTextCursorPositionIndex:=fCountTextGlyphRects+1;
        end else begin
         for Index:=fCountTextGlyphRects-1 downto 0 do begin
          if aPointerEvent.Position.x>=fTextGlyphRects[Index].Left then begin
           fTextCursorPositionIndex:=Index+1;
           break;
          end;
         end;
        end;
       end;
       RequestFocus;
      end;
      BUTTON_MIDDLE:begin
       RequestFocus;
      end;
      BUTTON_RIGHT:begin
       RequestFocus;
      end;
     end;
     result:=true;
    end;
    POINTEREVENT_UP:begin
     case aPointerEvent.Button of
      BUTTON_LEFT:begin
       if assigned(fOnClick) and Contains(aPointerEvent.Position) then begin
        fOnClick(self);
       end;
       RequestFocus;
      end;
      BUTTON_MIDDLE:begin
       RequestFocus;
      end;
      BUTTON_RIGHT:begin
       RequestFocus;
       if assigned(fPopupMenu) then begin
        fPopupMenu.Activate(AbsolutePosition+aPointerEvent.Position);
       end;
      end;
     end;
    end;
    POINTEREVENT_MOTION:begin
     if BUTTON_LEFT in aPointerEvent.Buttons then begin
      if fTextSelectionStart<1 then begin
       fTextSelectionStart:=fTextCursorPositionIndex;
      end;
      fTextCursorPositionIndex:=1;
      if fCountTextGlyphRects>0 then begin
       if aPointerEvent.Position.x>=fTextGlyphRects[fCountTextGlyphRects-1].Right then begin
        fTextCursorPositionIndex:=fCountTextGlyphRects+1;
       end else begin
        for Index:=fCountTextGlyphRects-1 downto 0 do begin
         if aPointerEvent.Position.x>=fTextGlyphRects[Index].Left then begin
          fTextCursorPositionIndex:=Index+1;
          break;
         end;
        end;
       end;
      end;
      fTextSelectionEnd:=fTextCursorPositionIndex;
     end;
     if not fEditable then begin
      fCursor:=pvgcArrow;
     end else begin
      if fSpinnable and fDragRect.Touched(aPointerEvent.Position) then begin
       fCursor:=pvgcNS;
      end else begin
       fCursor:=pvgcBeam;
      end;
     end;
     result:=true;
    end;
   end;
  end;
 end;
end;

function TpvGUITextEdit.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
  result:=inherited Scrolled(aPosition,aRelativeAmount);
 end;
end;

procedure TpvGUITextEdit.Update;
begin
 Skin.DrawTextEdit(fCanvas,self);
 inherited Update;
end;

procedure TpvGUITextEdit.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIIntegerEdit.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fWidgetFlags:=fWidgetFlags+[pvgwfDraggable];

 fMinimumValue:=Low(TpvInt64);

 fMaximumValue:=High(TpvInt64);

 fSmallStep:=1;

 fLargeStep:=10;

 SetValue(0);

 Spinnable:=true;

end;

destructor TpvGUIIntegerEdit.Destroy;
begin
 inherited Destroy;
end;

procedure TpvGUIIntegerEdit.UpdateText;
begin
 inherited UpdateText;
 ApplyMinMaxValueBounds;
end;

procedure TpvGUIIntegerEdit.ApplyMinMaxValueBounds;
var OldValue,TemporaryValue:TpvInt64;
begin
 OldValue:=GetValue;
 TemporaryValue:=Min(Max(OldValue,fMinimumValue),fMaximumValue);
 if OldValue<>TemporaryValue then begin
  SetValue(TemporaryValue);
 end;
end;

procedure TpvGUIIntegerEdit.SetMinimumValue(const aMinimumValue:TpvInt64);
begin
 fMinimumValue:=aMinimumValue;
 ApplyMinMaxValueBounds;
end;

procedure TpvGUIIntegerEdit.SetMaximumValue(const aMaximumValue:TpvInt64);
begin
 fMaximumValue:=aMaximumValue;
 ApplyMinMaxValueBounds;
end;

function TpvGUIIntegerEdit.GetValue:TpvInt64;
type TCharSet=set of AnsiChar;
var Index,Len,Base,Sign:TpvSizeInt;
    DigitCharSet:TCharSet;
begin
 result:=0;
 if length(fText)>0 then begin
  Len:=length(fText);
  if Len=0 then begin
   exit;
  end;
  Index:=1;
  Sign:=1;
  if (Index<=Len) and (fText[Index] in ['-','+']) then begin
   if fText[Index]='-' then begin
    Sign:=-1;
   end;
   inc(Index);
   if Index>Len then begin
    exit;
   end;
  end;
  if (Index<=Len) and (fText[Index]='$') then begin
   inc(Index);
   if Index>Len then begin
    exit;
   end;
   DigitCharSet:=['0'..'9','a'..'f','A'..'F'];
   Base:=16;
  end else if ((Index+1)<=Len) and (fText[Index]='0') and (fText[Index+1] in ['b','B','o','O','x','X']) then begin
   case fText[Index+1] of
    'b','B':begin
     DigitCharSet:=['0'..'1'];
     Base:=2;
    end;
    'o','O':begin
     DigitCharSet:=['0'..'7'];
     Base:=8;
    end;
    else {'x','X':}begin
     DigitCharSet:=['0'..'9','a'..'f','A'..'F'];
     Base:=16;
    end;
   end;
   inc(Index,2);
   if Index>Len then begin
    exit;
   end;
  end else begin
   DigitCharSet:=['0'..'9'];
   Base:=10;
  end;
  if fText[Index] in DigitCharSet then begin
   repeat
    case fText[Index] of
     '0'..'9':begin
      result:=(result*Base)+((ord(fText[Index])-ord('0'))*Sign);
     end;
     'a'..'f':begin
      result:=(result*Base)+(((ord(fText[Index])-ord('a'))+$a)*Sign);
     end;
     'A'..'F':begin
      result:=(result*Base)+(((ord(fText[Index])-ord('A'))+$a)*Sign);
     end;
     else begin
      break;
     end;
    end;
    inc(Index);
   until (Index>Len) or not (fText[Index] in DigitCharSet);
   if Index<=Len then begin
    result:=0;
    exit;
   end;
  end;
 end;
end;

procedure TpvGUIIntegerEdit.SetValue(const aValue:TpvInt64);
var OldText:TpvUTF8String;
begin
 OldText:=fText;
 fText:=TpvUTF8String(IntToStr(aValue));
 if OldText<>fText then begin
  ApplyMinMaxValueBounds;
  if OldText<>fText then begin
   if assigned(fOnChange) then begin
    fOnChange(self);
   end;
  end;
 end;
end;

function TpvGUIIntegerEdit.CheckText(const aText:TpvUTF8String):boolean;
type TCharSet=set of AnsiChar;
var Index,Len:TpvSizeInt;
    DigitCharSet:TCharSet;
begin
 result:=true;
 if length(aText)>0 then begin
  result:=false;
  Len:=length(aText);
  if Len=0 then begin
   result:=true;
   exit;
  end;
  Index:=1;
  if (Index<=Len) and (aText[Index] in ['-','+']) then begin
   inc(Index);
   if Index>Len then begin
    result:=true;
    exit;
   end;
  end;
  if (Index<=Len) and (aText[Index]='$') then begin
   inc(Index);
   if Index>Len then begin
    result:=true;
    exit;
   end;
   DigitCharSet:=['0'..'9','a'..'f','A'..'F'];
  end else if ((Index+1)<=Len) and (aText[Index]='0') and (aText[Index+1] in ['b','B','o','O','x','X']) then begin
   case aText[Index+1] of
    'b','B':begin
     DigitCharSet:=['0'..'1'];
    end;
    'o','O':begin
     DigitCharSet:=['0'..'7'];
    end;
    else {'x','X':}begin
     DigitCharSet:=['0'..'9','a'..'f','A'..'F'];
    end;
   end;
   inc(Index,2);
   if Index>Len then begin
    result:=true;
    exit;
   end;
  end else begin
   DigitCharSet:=['0'..'9'];
  end;
  if aText[Index] in DigitCharSet then begin
   repeat
    inc(Index);
   until (Index>Len) or not (aText[Index] in DigitCharSet);
   if Index<=Len then begin
    exit;
   end;
   result:=true;
  end;
 end;
end;

function TpvGUIIntegerEdit.DragEvent(const aPosition:TpvVector2):boolean;
begin
 result:=fSpinnable and fDragRect.Touched(aPosition);
end;

function TpvGUIIntegerEdit.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var TemporaryValue:TpvInt64;
begin
 result:=inherited KeyEvent(aKeyEvent);
 if not result then begin
  case aKeyEvent.KeyEventType of
   KEYEVENT_TYPED:begin
    case aKeyEvent.KeyCode of
     KEYCODE_UP:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue+fSmallStep)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+fSmallStep)) then begin
       SetValue(TemporaryValue+fSmallStep);
      end else begin
       SetValue(fMaximumValue);
      end;
      result:=true;
     end;
     KEYCODE_DOWN:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue-fSmallStep)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue-fSmallStep)) then begin
       SetValue(TemporaryValue-fSmallStep);
      end else begin
       SetValue(fMinimumValue);
      end;
      result:=true;
     end;
     KEYCODE_PAGEUP:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue+fLargeStep)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+fLargeStep)) then begin
       SetValue(TemporaryValue+fLargeStep);
      end else begin
       SetValue(fMaximumValue);
      end;
      result:=true;
     end;
     KEYCODE_PAGEDOWN:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue-fLargeStep)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue-fLargeStep)) then begin
       SetValue(TemporaryValue-fLargeStep);
      end else begin
       SetValue(fMinimumValue);
      end;
      result:=true;
     end;
    end;
   end;
  end;
 end;
end;

function TpvGUIIntegerEdit.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var TemporaryValue,Step:TpvInt64;
    v:TpvFloat;
begin
 result:=inherited PointerEvent(aPointerEvent);
 if not result then begin
  case aPointerEvent.PointerEventType of
   POINTEREVENT_DRAG:begin
    TemporaryValue:=GetValue;
    v:=aPointerEvent.RelativePosition.x-aPointerEvent.RelativePosition.y;
    if v<0.0 then begin
     Step:=floor(v);
    end else begin
     Step:=ceil(v);
    end;
    if ((Step>0) and ((TemporaryValue+Step)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+Step))) or
       ((Step<0) and ((TemporaryValue+Step)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue+Step))) then begin
     SetValue(TemporaryValue+Step);
    end else if Step<0 then begin
     SetValue(fMinimumValue);
    end else if Step>0 then begin
     SetValue(fMaximumValue);
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUIIntegerEdit.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var TemporaryValue,Step:TpvInt64;
    v:TpvFloat;
begin
 result:=inherited Scrolled(aPosition,aRelativeAmount);
 if not result then begin
  TemporaryValue:=GetValue;
  v:=aRelativeAmount.x+aRelativeAmount.y;
  if v<0.0 then begin
   Step:=floor(v);
  end else begin
   Step:=ceil(v);
  end;
  if ((Step>0) and ((TemporaryValue+Step)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+Step))) or
     ((Step<0) and ((TemporaryValue+Step)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue+Step))) then begin
   SetValue(TemporaryValue+Step);
  end else if Step<0 then begin
   SetValue(fMinimumValue);
  end else if Step>0 then begin
   SetValue(fMaximumValue);
  end;
  result:=true;
 end;
end;

constructor TpvGUIFloatEdit.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fWidgetFlags:=fWidgetFlags+[pvgwfDraggable];

 fMinimumValue:=-MaxDouble;

 fMaximumValue:=MaxDouble;

 fSmallStep:=1.0;

 fLargeStep:=10.0;

 SetValue(0.0);

 fDigits:=-1;

 Spinnable:=true;

end;

destructor TpvGUIFloatEdit.Destroy;
begin
 inherited Destroy;
end;

procedure TpvGUIFloatEdit.UpdateText;
begin
 inherited UpdateText;
 ApplyMinMaxValueBounds;
end;

procedure TpvGUIFloatEdit.ApplyMinMaxValueBounds;
var OldValue,TemporaryValue:TpvDouble;
begin
 OldValue:=GetValue;
 TemporaryValue:=Min(Max(OldValue,fMinimumValue),fMaximumValue);
 if OldValue<>TemporaryValue then begin
  SetValue(TemporaryValue);
 end;
end;

procedure TpvGUIFloatEdit.SetMinimumValue(const aMinimumValue:TpvDouble);
begin
 fMinimumValue:=aMinimumValue;
 ApplyMinMaxValueBounds;
end;

procedure TpvGUIFloatEdit.SetMaximumValue(const aMaximumValue:TpvDouble);
begin
 fMaximumValue:=aMaximumValue;
 ApplyMinMaxValueBounds;
end;

function TpvGUIFloatEdit.GetValue:TpvDouble;
var OK:TPasDblStrUtilsBoolean;
begin
 if length(fText)=0 then begin
  result:=0.0;
 end else begin
  OK:=false;
  result:=PasDblStrUtils.ConvertStringToDouble(fText,rmNearest,@OK,fDigits);
  if not OK then begin
   result:=0.0;
  end;
 end;
end;

procedure TpvGUIFloatEdit.SetValue(const aValue:TpvDouble);
var OldText:TpvUTF8String;
begin
 OldText:=fText;
 fText:=PasDblStrUtils.ConvertDoubleToString(aValue,omStandard,fDigits);
 if OldText<>fText then begin
  ApplyMinMaxValueBounds;
  if OldText<>fText then begin
   if assigned(fOnChange) then begin
    fOnChange(self);
   end;
  end;
 end;
end;

function TpvGUIFloatEdit.CheckText(const aText:TpvUTF8String):boolean;
type TCharSet=set of AnsiChar;
var Index,Len:TpvSizeInt;
    DigitCharSet:TCharSet;
begin
 result:=true;
 if length(aText)>0 then begin
  result:=false;
  Len:=length(aText);
  if Len=0 then begin
   result:=true;
   exit;
  end;
  Index:=1;
  if (Index<=Len) and (aText[Index] in ['-','+']) then begin
   inc(Index);
   if Index>Len then begin
    result:=true;
    exit;
   end;
  end;
  if ((Index+1)<=Len) and (aText[Index]='0') and (aText[Index+1] in ['b','B','o','O','x','X']) then begin
   case aText[Index+1] of
    'b','B':begin
     DigitCharSet:=['0'..'1'];
    end;
    'o','O':begin
     DigitCharSet:=['0'..'7'];
    end;
    else {'x','X':}begin
     DigitCharSet:=['0'..'9','a'..'f','A'..'F'];
    end;
   end;
   inc(Index,2);
   if Index>Len then begin
    result:=true;
    exit;
   end;
  end else begin
   DigitCharSet:=['0'..'9'];
  end;
  if aText[Index] in DigitCharSet then begin
   repeat
    inc(Index);
   until (Index>Len) or not (aText[Index] in DigitCharSet);
   if (Index<=Len) and (aText[Index]='.') then begin
    inc(Index);
    if (Index<=Len) and (aText[Index] in DigitCharSet) then begin
     repeat
      inc(Index);
     until (Index>Len) or not (aText[Index] in DigitCharSet);
    end else if Index<=Len then begin
     exit;
    end;
   end;
   if (Index<=Len) and (aText[Index] in ['e','E','p','P']) then begin
    inc(Index);
    if (Index<=Len) and (aText[Index] in ['-','+']) then begin
     inc(Index);
    end;
    if (Index<=Len) and (aText[Index] in ['0'..'9']) then begin
     repeat
      inc(Index);
     until (Index>Len) or not (aText[Index] in ['0'..'9']);
    end;
   end;
   if Index<=Len then begin
    exit;
   end;
   result:=true;
  end;
 end;
end;

function TpvGUIFloatEdit.DragEvent(const aPosition:TpvVector2):boolean;
begin
 result:=fSpinnable and fDragRect.Touched(aPosition);
end;

function TpvGUIFloatEdit.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var TemporaryValue:TpvDouble;
begin
 result:=inherited KeyEvent(aKeyEvent);
 if not result then begin
  case aKeyEvent.KeyEventType of
   KEYEVENT_TYPED:begin
    case aKeyEvent.KeyCode of
     KEYCODE_UP:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue+fSmallStep)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+fSmallStep)) then begin
       SetValue(TemporaryValue+fSmallStep);
      end else begin
       SetValue(fMaximumValue);
      end;
      result:=true;
     end;
     KEYCODE_DOWN:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue-fSmallStep)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue-fSmallStep)) then begin
       SetValue(TemporaryValue-fSmallStep);
      end else begin
       SetValue(fMinimumValue);
      end;
      result:=true;
     end;
     KEYCODE_PAGEUP:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue+fLargeStep)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+fLargeStep)) then begin
       SetValue(TemporaryValue+fLargeStep);
      end else begin
       SetValue(fMaximumValue);
      end;
      result:=true;
     end;
     KEYCODE_PAGEDOWN:begin
      TemporaryValue:=GetValue;
      if ((TemporaryValue-fLargeStep)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue-fLargeStep)) then begin
       SetValue(TemporaryValue-fLargeStep);
      end else begin
       SetValue(fMinimumValue);
      end;
      result:=true;
     end;
    end;
   end;
  end;
 end;
end;

function TpvGUIFloatEdit.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var TemporaryValue,Step:TpvDouble;
    v:TpvFloat;
begin
 result:=inherited PointerEvent(aPointerEvent);
 if not result then begin
  case aPointerEvent.PointerEventType of
   POINTEREVENT_DRAG:begin
    TemporaryValue:=GetValue;
    v:=aPointerEvent.RelativePosition.x-aPointerEvent.RelativePosition.y;
    if v<0.0 then begin
     Step:=floor(v);
    end else begin
     Step:=ceil(v);
    end;
    if ((Step>0) and ((TemporaryValue+Step)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+Step))) or
       ((Step<0) and ((TemporaryValue+Step)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue+Step))) then begin
     SetValue(TemporaryValue+Step);
    end else if Step<0 then begin
     SetValue(fMinimumValue);
    end else if Step>0 then begin
     SetValue(fMaximumValue);
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUIFloatEdit.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var TemporaryValue,Step:TpvDouble;
    v:TpvFloat;
begin
 result:=inherited Scrolled(aPosition,aRelativeAmount);
 if not result then begin
  TemporaryValue:=GetValue;
  v:=aRelativeAmount.x+aRelativeAmount.y;
  if v<0.0 then begin
   Step:=floor(v);
  end else begin
   Step:=ceil(v);
  end;
  if ((Step>0) and ((TemporaryValue+Step)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+Step))) or
     ((Step<0) and ((TemporaryValue+Step)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue+Step))) then begin
   SetValue(TemporaryValue+Step);
  end else if Step<0 then begin
   SetValue(fMinimumValue);
  end else if Step>0 then begin
   SetValue(fMaximumValue);
  end;
  result:=true;
 end;
end;

constructor TpvGUIMenuItem.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fFlags:=[pvgmifEnabled];

 fCaption:='';

 fShortcutHint:='';

 fIcon:=nil;

 fIconHeight:=0.0;

 fOnClick:=nil;

end;

destructor TpvGUIMenuItem.Destroy;
begin

 inherited Destroy;

end;

function TpvGUIMenuItem.GetEnabled:boolean;
begin
 result:=pvgmifEnabled in fFlags;
end;

procedure TpvGUIMenuItem.SetEnabled(const aEnabled:boolean);
begin
 if aEnabled then begin
  Include(fFlags,pvgmifEnabled);
 end else begin
  Exclude(fFlags,pvgmifEnabled);
 end;
end;

function TpvGUIMenuItem.GetSelectable:boolean;
begin
 result:=fCaption<>'-';
end;

function TpvGUIMenuItem.GetMenu:TpvGUIPopupMenu;
var Index:TpvInt32;
    Child:TpvGUIObject;
begin
 for Index:=0 to fChildren.Count-1 do begin
  Child:=fChildren[Index];
  if Child is TpvGUIPopupMenu then begin
   result:=TpvGUIPopupMenu(Child);
   exit;
  end;
 end;
 result:=nil;
end;

constructor TpvGUIPopupMenu.Create(const aParent:TpvGUIObject);
begin
 inherited Create(aParent);

 fSelectedMenuItem:=nil;

 fFocusedMenuItem:=nil;

 fHoveredMenuItem:=nil;

 fSkin:=nil;

 fFont:=nil;

 fFontSize:=0.0;

 fFontColor:=TpvVector4.Null;

 fPosition:=TpvVector2.Null;

 fPositionProperty:=TpvVector2Property.Create(@fPosition);

 fReleaseOnDeactivation:=false;

end;

destructor TpvGUIPopupMenu.Destroy;
begin
 FreeAndNil(fPositionProperty);
 inherited Destroy;
end;

function TpvGUIPopupMenu.GetSkin:TpvGUISkin;
begin
 if assigned(fSkin) then begin
  result:=fSkin;
 end else if assigned(fInstance) then begin
  result:=fInstance.fStandardSkin;
 end else begin
  result:=nil;
 end;
end;

procedure TpvGUIPopupMenu.SetSkin(const aSkin:TpvGUISkin);
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
  end else if Child is TpvGUIPopupMenu then begin
   (Child as TpvGUIPopupMenu).SetSkin(aSkin);
  end;
 end;
end;

function TpvGUIPopupMenu.GetFont:TpvFont;
begin
 if assigned(Skin) and not assigned(fFont) then begin
  result:=Skin.fSansFont;
 end else begin
  result:=fFont;
 end;
end;

function TpvGUIPopupMenu.GetFontSize:TpvFloat;
begin
 if assigned(Skin) and IsZero(fFontSize) then begin
  result:=Skin.fPopupMenuFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TpvGUIPopupMenu.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fPopupMenuFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

function TpvGUIPopupMenu.GetActivated:boolean;
begin
 result:=fInstance.fPopupMenuStack.Contains(self);
end;

procedure TpvGUIPopupMenu.Activate(const aPosition:TpvVector2);
var Index:TpvInt32;
    ParentPopupMenu:TpvGUIPopupMenu;
begin

 fPosition:=aPosition;

 if not fInstance.fPopupMenuStack.Contains(self) then begin

  if not assigned(fParent) then begin
   fInstance.fPopupMenuStack.Clear;
  end else if fInstance.fPopupMenuStack.Count>0 then begin
   if fParent is TpvGUIPopupMenu then begin
    ParentPopupMenu:=fParent as TpvGUIPopupMenu;
   end else if (fParent is TpvGUIMenuItem) and
               assigned((fParent as TpvGUIMenuItem).fParent) and
               ((fParent as TpvGUIMenuItem).fParent is TpvGUIPopupMenu) then begin
    ParentPopupMenu:=(fParent as TpvGUIMenuItem).fParent as TpvGUIPopupMenu;
   end else begin
    ParentPopupMenu:=nil;
   end;
   if assigned(ParentPopupMenu) then begin
    Index:=fInstance.fPopupMenuStack.IndexOf(ParentPopupMenu);
   end else begin
    Index:=-1;
   end;
   if Index>=0 then begin
    while (Index+1)<fInstance.fPopupMenuStack.Count do begin
     fInstance.fPopupMenuStack.Delete(fInstance.fPopupMenuStack.Count-1);
    end;
   end else begin
    fInstance.fPopupMenuStack.Clear;
   end;
  end;

  fSelectedMenuItem:=nil;
  fFocusedMenuItem:=nil;
  fHoveredMenuItem:=nil;

  fInstance.fPopupMenuStack.Add(self);

 end;

end;

procedure TpvGUIPopupMenu.FocusFirstMenuItem;
var Index:TpvInt32;
    ParentPopupMenu:TpvGUIPopupMenu;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
begin
 fSelectedMenuItem:=nil;
 for Index:=0 to fChildren.Count-1 do begin
  Child:=fChildren[Index];
  if Child is TpvGUIMenuItem then begin
   MenuItem:=TpvGUIMenuItem(Child);
   fFocusedMenuItem:=MenuItem;
   fHoveredMenuItem:=MenuItem;
   break;
  end;
 end;
end;

procedure TpvGUIPopupMenu.Deactivate;
var Index:TpvInt32;
begin
 if fInstance.fPopupMenuStack.Contains(self) then begin
  IncRef;
  try
   Index:=fInstance.fPopupMenuStack.IndexOf(self);
   if Index>=0 then begin
    while Index<fInstance.fPopupMenuStack.Count do begin
     fInstance.fPopupMenuStack.Delete(fInstance.fPopupMenuStack.Count-1);
    end;
   end else begin
    fInstance.fPopupMenuStack.Clear;
   end;
  finally
   DecRef;
  end;
  if fReleaseOnDeactivation then begin
   fInstance.ReleaseObject(self);
  end;
 end;
end;

procedure TpvGUIPopupMenu.DeactivateWindowMenu;
begin
 if assigned(fParent) and
    (fParent is TpvGUIMenuItem) and
    assigned(fParent.fParent) and
    (fParent.fParent is TpvGUIWindowMenu) then begin
  (fParent.fParent as TpvGUIWindowMenu).fSelectedMenuItem:=nil;
  (fParent.fParent as TpvGUIWindowMenu).fFocusedMenuItem:=nil;
  (fParent.fParent as TpvGUIWindowMenu).fHoveredMenuItem:=nil;
 end;
end;

procedure TpvGUIPopupMenu.DeactivateSubmenus;
var Index:TpvInt32;
begin
 if fInstance.fPopupMenuStack.Contains(self) then begin
  IncRef;
  try
   Index:=fInstance.fPopupMenuStack.IndexOf(self);
   if Index>=0 then begin
    while (Index+1)<fInstance.fPopupMenuStack.Count do begin
     fInstance.fPopupMenuStack.Delete(fInstance.fPopupMenuStack.Count-1);
    end;
   end else begin
    fInstance.fPopupMenuStack.Clear;
   end;
  finally
   DecRef;
  end;
 end;
end;

function TpvGUIPopupMenu.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var Index,OtherIndex:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
begin
 case aKeyEvent.KeyEventType of
  KEYEVENT_DOWN:begin
   result:=true;
  end;
  KEYEVENT_UP:begin
   result:=true;
  end;
  KEYEVENT_TYPED:begin
   case aKeyEvent.KeyCode of
    KEYCODE_RIGHT:begin
     fSelectedMenuItem:=nil;
     if assigned(fFocusedMenuItem) then begin
      if fFocusedMenuItem.Enabled and fFocusedMenuItem.Selectable then begin
       if assigned(fFocusedMenuItem.Menu) then begin
        fSelectedMenuItem:=fFocusedMenuItem;
        fFocusedMenuItem.Menu.Activate(fPosition+TpvVector2.Create(fSize.x,fFocusedMenuItem.fOpenRect.Top));
        fFocusedMenuItem.Menu.FocusFirstMenuItem;
        if assigned(fFocusedMenuItem.OnClick) then begin
         fFocusedMenuItem.OnClick(fFocusedMenuItem);
        end;
       end;
      end;
     end;
    end;
    KEYCODE_SPACE,KEYCODE_RETURN:begin
     DeactivateSubmenus;
     fSelectedMenuItem:=nil;
     if assigned(fFocusedMenuItem) then begin
      fSelectedMenuItem:=fFocusedMenuItem;
      if fFocusedMenuItem.Enabled and fFocusedMenuItem.Selectable then begin
       if assigned(fFocusedMenuItem.Menu) then begin
        fFocusedMenuItem.Menu.Activate(fPosition+TpvVector2.Create(fSize.x,fFocusedMenuItem.fOpenRect.Top));
        fFocusedMenuItem.Menu.FocusFirstMenuItem;
       end;
       if assigned(fFocusedMenuItem.OnClick) then begin
        fFocusedMenuItem.OnClick(fFocusedMenuItem);
       end;
       if not assigned(fSelectedMenuItem.Menu) then begin
        if fInstance.fPopupMenuStack.Count>0 then begin
         (fInstance.fPopupMenuStack[0] as TpvGUIPopupMenu).DeactivateWindowMenu;
         (fInstance.fPopupMenuStack[0] as TpvGUIPopupMenu).Deactivate;
        end;
       end;
      end;
     end;
    end;
    KEYCODE_LEFT:begin
     if assigned(fParent) and
        (fParent is TpvGUIMenuItem) and
         assigned(fParent.fParent) and
        (fParent.fParent is TpvGUIPopupMenu) then begin
      Deactivate;
     end;
    end;
    KEYCODE_ESCAPE:begin
     fSelectedMenuItem:=nil;
     for Index:=0 to fChildren.Count-1 do begin
      Child:=fChildren[Index];
      if Child is TpvGUIMenuItem then begin
       MenuItem:=TpvGUIMenuItem(Child);
       fFocusedMenuItem:=MenuItem;
       fHoveredMenuItem:=MenuItem;
       break;
      end;
     end;
     Deactivate;
    end;
    KEYCODE_UP:begin
     if assigned(fFocusedMenuItem) then begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if (Child is TpvGUIMenuItem) and (Child=fFocusedMenuItem) then begin
        if Index=0 then begin
         if assigned(fParent) and
            (fParent is TpvGUIMenuItem) and
            assigned(fParent.fParent) and
            (fParent.fParent is TpvGUIWindowMenu) then begin
          Deactivate;
         end;
        end else begin
         for OtherIndex:=Index-1 downto 0 do begin
          Child:=fChildren[OtherIndex];
          if Child is TpvGUIMenuItem then begin
           MenuItem:=TpvGUIMenuItem(Child);
           if MenuItem.Enabled and MenuItem.Selectable then begin
            fSelectedMenuItem:=nil;
            fFocusedMenuItem:=MenuItem;
            fHoveredMenuItem:=MenuItem;
            break;
           end;
          end;
         end;
        end;
        break;
       end;
      end;
     end else begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if Child is TpvGUIMenuItem then begin
        MenuItem:=TpvGUIMenuItem(Child);
        fFocusedMenuItem:=MenuItem;
        fHoveredMenuItem:=MenuItem;
        break;
       end;
      end;
     end;
    end;
    KEYCODE_DOWN:begin
     if assigned(fFocusedMenuItem) then begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if (Child is TpvGUIMenuItem) and (Child=fFocusedMenuItem) then begin
        for OtherIndex:=Index+1 to fChildren.Count-1 do begin
         Child:=fChildren[OtherIndex];
         if Child is TpvGUIMenuItem then begin
          MenuItem:=TpvGUIMenuItem(Child);
          if MenuItem.Enabled and MenuItem.Selectable then begin
           fSelectedMenuItem:=nil;
           fFocusedMenuItem:=MenuItem;
           fHoveredMenuItem:=MenuItem;
           break;
          end;
         end;
        end;
        break;
       end;
      end;
     end else begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if Child is TpvGUIMenuItem then begin
        MenuItem:=TpvGUIMenuItem(Child);
        fFocusedMenuItem:=MenuItem;
        fHoveredMenuItem:=MenuItem;
        break;
       end;
      end;
     end;
    end;
    KEYCODE_HOME:begin
     for Index:=0 to fChildren.Count-1 do begin
      Child:=fChildren[Index];
      if Child is TpvGUIMenuItem then begin
       MenuItem:=TpvGUIMenuItem(Child);
       fSelectedMenuItem:=nil;
       fFocusedMenuItem:=MenuItem;
       fHoveredMenuItem:=MenuItem;
       break;
      end;
     end;
    end;
    KEYCODE_END:begin
     for Index:=fChildren.Count-1 downto 0 do begin
      Child:=fChildren[Index];
      if Child is TpvGUIMenuItem then begin
       MenuItem:=TpvGUIMenuItem(Child);
       fSelectedMenuItem:=nil;
       fFocusedMenuItem:=MenuItem;
       fHoveredMenuItem:=MenuItem;
       break;
      end;
     end;
    end;
   end;
   result:=true;
  end;
  KEYEVENT_UNICODE:begin
   result:=true;
  end;
  else begin
   result:=false;
  end;
 end;
end;

function TpvGUIPopupMenu.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Index:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
begin
 fHoveredMenuItem:=nil;
 result:=TpvRect.CreateRelative(fPosition,fSize).Touched(aPointerEvent.Position);
 if result then begin
  case aPointerEvent.PointerEventType of
   POINTEREVENT_DOWN:begin
    DeactivateSubmenus;
    fSelectedMenuItem:=nil;
    fFocusedMenuItem:=nil;
    fHoveredMenuItem:=nil;
    for Index:=0 to fChildren.Count-1 do begin
     Child:=fChildren[Index];
     if Child is TpvGUIMenuItem then begin
      MenuItem:=TpvGUIMenuItem(Child);
      if MenuItem.Enabled and
         MenuItem.Selectable and
         MenuItem.fRect.Touched(aPointerEvent.Position-fPosition) then begin
       fSelectedMenuItem:=MenuItem;
       fFocusedMenuItem:=MenuItem;
       fHoveredMenuItem:=MenuItem;
       if fSelectedMenuItem.Enabled and fSelectedMenuItem.Selectable and assigned(fSelectedMenuItem.Menu) then begin
        fSelectedMenuItem.Menu.Activate(fPosition+TpvVector2.Create(fSize.x,fFocusedMenuItem.fOpenRect.Top));
       end;
       break;
      end;
     end;
    end;
    if not assigned(fFocusedMenuItem) then begin
     for Index:=0 to fChildren.Count-1 do begin
      Child:=fChildren[Index];
      if Child is TpvGUIMenuItem then begin
       fFocusedMenuItem:=TpvGUIMenuItem(Child);
       break;
      end;
     end;
    end;
    result:=true;
   end;
   POINTEREVENT_UP:begin
    if assigned(fSelectedMenuItem) then begin
     if fSelectedMenuItem.Enabled and fSelectedMenuItem.Selectable and assigned(fSelectedMenuItem.fOnClick) then begin
      fSelectedMenuItem.fOnClick(fSelectedMenuItem);
     end;
     if fSelectedMenuItem.Enabled and fSelectedMenuItem.Selectable and not assigned(fSelectedMenuItem.Menu) then begin
      if fInstance.fPopupMenuStack.Count>0 then begin
       (fInstance.fPopupMenuStack[0] as TpvGUIPopupMenu).DeactivateWindowMenu;
       (fInstance.fPopupMenuStack[0] as TpvGUIPopupMenu).Deactivate;
      end;
     end;
     fSelectedMenuItem:=nil;
    end;
    result:=true;
   end;
   POINTEREVENT_MOTION:begin
    fHoveredMenuItem:=nil;
    for Index:=0 to fChildren.Count-1 do begin
     Child:=fChildren[Index];
     if Child is TpvGUIMenuItem then begin
      MenuItem:=TpvGUIMenuItem(Child);
      if MenuItem.Enabled and
         MenuItem.Selectable and
         MenuItem.fRect.Touched(aPointerEvent.Position-fPosition) then begin
       fHoveredMenuItem:=MenuItem;
       break;
      end;
     end;
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUIPopupMenu.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=TpvRect.CreateRelative(fPosition,fSize).Touched(aPosition);
 if result then begin
 end;
end;

procedure TpvGUIPopupMenu.Draw(const aCanvas:TpvCanvas);
begin
 Skin.DrawPopupMenu(aCanvas,self);
end;

constructor TpvGUIWindowMenu.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fSelectedMenuItem:=nil;

 fFocusedMenuItem:=nil;

 fHoveredMenuItem:=nil;

 Exclude(fWidgetFlags,pvgwfVisible);

 Include(fWidgetFlags,pvgwfTabStop);

 fLayout:=TpvGUIBoxLayout.Create(self,pvglaMiddle,pvgloVertical,0.0,4.0);

end;

destructor TpvGUIWindowMenu.Destroy;
begin

 inherited Destroy;

end;

function TpvGUIWindowMenu.Enter:boolean;
var Index:TpvInt32;
    Child:TpvGUIObject;
begin
 fSelectedMenuItem:=nil;
 fFocusedMenuItem:=nil;
 fHoveredMenuItem:=nil;
 for Index:=0 to fChildren.Count-1 do begin
  Child:=fChildren[Index];
  if Child is TpvGUIMenuItem then begin
   fFocusedMenuItem:=TpvGUIMenuItem(Child);
   break;
  end;
 end;
 result:=inherited Enter;
end;

function TpvGUIWindowMenu.Leave:boolean;
begin
 result:=inherited Leave;
 fFocusedMenuItem:=nil;
 fHoveredMenuItem:=nil;
end;

function TpvGUIWindowMenu.GetFontSize:TpvFloat;
begin
 if assigned(Skin) and IsZero(fFontSize) then begin
  result:=Skin.fWindowMenuFontSize;
 end else begin
  result:=fFontSize;
 end;
end;

function TpvGUIWindowMenu.GetFontColor:TpvVector4;
begin
 if assigned(Skin) and IsZero(fFontColor.a) then begin
  result:=Skin.fWindowMenuFontColor;
 end else begin
  result:=fFontColor;
 end;
end;

function TpvGUIWindowMenu.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetWindowMenuPreferredSize(self);
end;

function TpvGUIWindowMenu.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var Index,OtherIndex:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if Enabled and not result then begin
  case aKeyEvent.KeyEventType of
   KEYEVENT_DOWN:begin
    result:=true;
   end;
   KEYEVENT_UP:begin
    result:=true;
   end;
   KEYEVENT_TYPED:begin
    case aKeyEvent.KeyCode of
     KEYCODE_DOWN,KEYCODE_SPACE,KEYCODE_RETURN:begin
      fSelectedMenuItem:=nil;
      if assigned(fFocusedMenuItem) then begin
       fSelectedMenuItem:=fFocusedMenuItem;
       if fFocusedMenuItem.Enabled and fFocusedMenuItem.Selectable then begin
        if assigned(fFocusedMenuItem.Menu) then begin
         fFocusedMenuItem.Menu.Activate(AbsolutePosition+TpvVector2.Create(fFocusedMenuItem.fOpenRect.Left,fSize.y));
         fFocusedMenuItem.Menu.FocusFirstMenuItem;
        end;
        if assigned(fFocusedMenuItem.OnClick) then begin
         fFocusedMenuItem.OnClick(fFocusedMenuItem);
        end;
       end;
      end;
     end;
     KEYCODE_ESCAPE:begin
      fSelectedMenuItem:=nil;
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if Child is TpvGUIMenuItem then begin
        MenuItem:=TpvGUIMenuItem(Child);
        fFocusedMenuItem:=MenuItem;
        fHoveredMenuItem:=MenuItem;
        break;
       end;
      end;
     end;
     KEYCODE_LEFT:begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if (Child is TpvGUIMenuItem) and (Child=fFocusedMenuItem) then begin
        for OtherIndex:=Index-1 downto 0 do begin
         Child:=fChildren[OtherIndex];
         if Child is TpvGUIMenuItem then begin
          MenuItem:=TpvGUIMenuItem(Child);
          if MenuItem.Enabled and MenuItem.Selectable then begin
           fSelectedMenuItem:=nil;
           fFocusedMenuItem:=MenuItem;
           fHoveredMenuItem:=MenuItem;
           break;
          end;
         end;
        end;
        break;
       end;
      end;
     end;
     KEYCODE_RIGHT:begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if (Child is TpvGUIMenuItem) and (Child=fFocusedMenuItem) then begin
        for OtherIndex:=Index+1 to fChildren.Count-1 do begin
         Child:=fChildren[OtherIndex];
         if Child is TpvGUIMenuItem then begin
          MenuItem:=TpvGUIMenuItem(Child);
          if MenuItem.Enabled and MenuItem.Selectable then begin
           fSelectedMenuItem:=nil;
           fFocusedMenuItem:=MenuItem;
           fHoveredMenuItem:=MenuItem;
           break;
          end;
         end;
        end;
        break;
       end;
      end;
     end;
     KEYCODE_HOME:begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if Child is TpvGUIMenuItem then begin
        MenuItem:=TpvGUIMenuItem(Child);
        fSelectedMenuItem:=nil;
        fFocusedMenuItem:=MenuItem;
        fHoveredMenuItem:=MenuItem;
        break;
       end;
      end;
     end;
     KEYCODE_END:begin
      for Index:=fChildren.Count-1 downto 0 do begin
       Child:=fChildren[Index];
       if Child is TpvGUIMenuItem then begin
        MenuItem:=TpvGUIMenuItem(Child);
        fSelectedMenuItem:=nil;
        fFocusedMenuItem:=MenuItem;
        fHoveredMenuItem:=MenuItem;
        break;
       end;
      end;
     end;
    end;
    result:=true;
   end;
   KEYEVENT_UNICODE:begin
    result:=true;
   end;
  end;
 end;
end;

function TpvGUIWindowMenu.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Index:TpvInt32;
    Child:TpvGUIObject;
    MenuItem:TpvGUIMenuItem;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  result:=inherited PointerEvent(aPointerEvent);
  if not result then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     if not Focused then begin
      RequestFocus;
     end;
     fSelectedMenuItem:=nil;
     fFocusedMenuItem:=nil;
     fHoveredMenuItem:=nil;
     for Index:=0 to fChildren.Count-1 do begin
      Child:=fChildren[Index];
      if Child is TpvGUIMenuItem then begin
       MenuItem:=TpvGUIMenuItem(Child);
       if MenuItem.Enabled and
          MenuItem.Selectable and
          MenuItem.fRect.Touched(aPointerEvent.Position) then begin
        fSelectedMenuItem:=MenuItem;
        fFocusedMenuItem:=MenuItem;
        fHoveredMenuItem:=MenuItem;
        if fSelectedMenuItem.Enabled and fSelectedMenuItem.Selectable and assigned(fSelectedMenuItem.Menu) then begin
         fSelectedMenuItem.Menu.Activate(AbsolutePosition+TpvVector2.Create(fSelectedMenuItem.fOpenRect.Left,fSize.y));
        end;
        break;
       end;
      end;
     end;
     if not assigned(fFocusedMenuItem) then begin
      for Index:=0 to fChildren.Count-1 do begin
       Child:=fChildren[Index];
       if Child is TpvGUIMenuItem then begin
        fFocusedMenuItem:=TpvGUIMenuItem(Child);
        break;
       end;
      end;
     end;
     result:=true;
    end;
    POINTEREVENT_UP:begin
     if assigned(fSelectedMenuItem) then begin
      if fSelectedMenuItem.Enabled and fSelectedMenuItem.Selectable and assigned(fSelectedMenuItem.fOnClick) then begin
       fSelectedMenuItem.fOnClick(fSelectedMenuItem);
      end;
      fSelectedMenuItem:=nil;
     end;
     result:=true;
    end;
    POINTEREVENT_MOTION:begin
     fHoveredMenuItem:=nil;
     for Index:=0 to fChildren.Count-1 do begin
      Child:=fChildren[Index];
      if Child is TpvGUIMenuItem then begin
       MenuItem:=TpvGUIMenuItem(Child);
       if MenuItem.Enabled and
          MenuItem.Selectable and
          MenuItem.fRect.Touched(aPointerEvent.Position) then begin
        fHoveredMenuItem:=MenuItem;
        break;
       end;
      end;
     end;
     result:=true;
    end;
   end;
  end;
 end;
end;

function TpvGUIWindowMenu.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
begin
 result:=assigned(fOnScrolled) and fOnScrolled(self,aPosition,aRelativeAmount);
 if not result then begin
  result:=inherited Scrolled(aPosition,aRelativeAmount);
 end;
end;

procedure TpvGUIWindowMenu.Update;
begin
 Skin.DrawWindowMenu(fCanvas,self);
 inherited Update;
end;

procedure TpvGUIWindowMenu.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIScrollBar.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 Include(fWidgetFlags,pvgwfTabStop);
 Include(fWidgetFlags,pvgwfDrawFocus);
 Include(fWidgetFlags,pvgwfDraggable);

 fOrientation:=pvgsboHorizontal;

 fMinimumValue:=0;

 fMaximumValue:=100;

 fValue:=0;

 fSmallStep:=1;

 fLargeStep:=10;

 fButtonSize:=24;

 fSliderButtonSize:=0;

 fSliderPushed:=false;

 fOnChange:=nil;

 fFocusedSubWidget:=pvgsbswNone;

 fPushedSubWidget:=pvgsbswNone;

 fStepSize:=0;

 fTimeAccumulator:=MaxDouble;

 fCachedSliderButtonSize:=-1.0;

end;

destructor TpvGUIScrollBar.Destroy;
begin

 inherited Destroy;

end;

procedure TpvGUIScrollBar.SetOrientation(const aOrientation:TpvGUIScrollBarOrientation);
begin
 if fOrientation<>aOrientation then begin
  fOrientation:=aOrientation;
  fCachedSliderButtonSize:=-1.0;
 end;
end;

procedure TpvGUIScrollBar.SetMinimumValue(const aMinimumValue:TpvInt64);
begin
 if fMinimumValue<>aMinimumValue then begin
  fMinimumValue:=aMinimumValue;
  fCachedSliderButtonSize:=-1.0;
  SetValue(fValue);
 end;
end;

procedure TpvGUIScrollBar.SetMaximumValue(const aMaximumValue:TpvInt64);
begin
 if fMaximumValue<>aMaximumValue then begin
  fMaximumValue:=aMaximumValue;
  fCachedSliderButtonSize:=-1.0;
  SetValue(fValue);
 end;
end;

procedure TpvGUIScrollBar.SetValue(const aValue:TpvInt64);
var NewValue:TpvInt64;
begin
 NewValue:=Min(Max(aValue,fMinimumValue),fMaximumValue);
 if fValue<>NewValue then begin
  fValue:=NewValue;
  if assigned(fOnChange) then begin
   fOnChange(self);
  end;
 end;
end;

procedure TpvGUIScrollBar.SetButtonSize(const aButtonSize:TpvFloat);
begin
 if fButtonSize<>aButtonSize then begin
  fButtonSize:=aButtonSize;
  fCachedSliderButtonSize:=-1.0;
 end;
end;

function TpvGUIScrollBar.GetSliderButtonSize:TpvFloat;
begin
 if fSliderButtonSize>0.0 then begin
  result:=fSliderButtonSize;
 end else begin
  if fCachedSliderButtonSize>0.0 then begin
   result:=fCachedSliderButtonSize;
  end else begin
   case fOrientation of
    pvgsboHorizontal:begin
     result:=Width;
    end;
    else {pvgsbVertical:}begin
     result:=Height;
    end;
   end;
   result:=Max(Max(24.0,fButtonSize),result-(fButtonSize*2.0));
   if fMinimumValue<fMaximumValue then begin
    result:=Min(Max(result/Max(fMaximumValue-fMinimumValue,1),Max(24.0,fButtonSize)),result);
   end;
   fCachedSliderButtonSize:=result;
  end;
 end;
end;

procedure TpvGUIScrollBar.SetSliderButtonSize(const aSliderButtonSize:TpvFloat);
begin
 if fSliderButtonSize<>aSliderButtonSize then begin
  fSliderButtonSize:=aSliderButtonSize;
  fCachedSliderButtonSize:=-1.0;
 end;
end;

function TpvGUIScrollBar.GetPreferredSize:TpvVector2;
begin
 fCachedSliderButtonSize:=-1.0;
 result:=Skin.GetScrollBarPreferredSize(self);
end;

function TpvGUIScrollBar.GetSliderButtonRect:TpvRect;
begin
 case fOrientation of
  pvgsboHorizontal:begin
   if fMinimumValue<fMaximumValue then begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(fButtonSize+
                                                     ((fSize.x-((fButtonSize*2.0)+SliderButtonSize))*
                                                      ((fValue-fMinimumValue)/Max(1,fMaximumValue-fMinimumValue))),
                                                     0.0),
                                   TpvVector2.Create(SliderButtonSize,fSize.y));
   end else begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(fButtonSize,0.0),
                                   TpvVector2.Create(SliderButtonSize,fSize.y));
   end;
  end;
  else {pvgsboVertical:}begin
   if fMinimumValue<fMaximumValue then begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(0.0,
                                                     fButtonSize+
                                                      ((fSize.y-((fButtonSize*2.0)+SliderButtonSize))*
                                                       ((fValue-fMinimumValue)/Max(1,fMaximumValue-fMinimumValue)))),
                                   TpvVector2.Create(fSize.x,SliderButtonSize));
   end else begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(0.0,fButtonSize),
                                   TpvVector2.Create(fSize.x,SliderButtonSize));
   end;
  end;
 end;
 result.LeftTop:=result.LeftTop+TpvVector2.Create(1.0,1.0);
 result.RightBottom:=result.RightBottom-TpvVector2.Create(1.0,1.0);
end;

function TpvGUIScrollBar.Enter:boolean;
begin
 result:=inherited Enter;
end;

function TpvGUIScrollBar.Leave:boolean;
begin
 fPushedSubWidget:=pvgsbswNone;
 fStepSize:=0;
 fSliderPushed:=false;
 result:=inherited Leave;
end;

function TpvGUIScrollBar.PointerEnter:boolean;
begin
 result:=inherited PointerEnter;
end;

function TpvGUIScrollBar.PointerLeave:boolean;
begin
 if assigned(fInstance) and (fInstance.fDragWidget<>self) then begin
  fPushedSubWidget:=pvgsbswNone;
 end;
 result:=inherited PointerLeave;
end;

function TpvGUIScrollBar.DragEvent(const aPosition:TpvVector2):boolean;
begin
 result:=GetSliderButtonRect.Touched(aPosition);
end;

function TpvGUIScrollBar.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if Enabled and not result then begin
  case aKeyEvent.KeyCode of
   KEYCODE_LEFT,KEYCODE_UP,KEYCODE_MINUS,KEYCODE_KP_MINUS:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue-fSmallStep)>=fMinimumValue) and not (fValue<(fValue-fSmallStep)) then begin
       SetValue(fValue-fSmallStep);
      end else begin
       SetValue(fMinimumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_RIGHT,KEYCODE_DOWN,KEYCODE_PLUS,KEYCODE_KP_PLUS:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue+fSmallStep)<=fMaximumValue) and not (fValue>(fValue+fSmallStep)) then begin
       SetValue(fValue+fSmallStep);
      end else begin
       SetValue(fMaximumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_PAGEDOWN:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue+fLargeStep)<=fMaximumValue) and not (fValue>(fValue+fLargeStep)) then begin
       SetValue(fValue+fLargeStep);
      end else begin
       SetValue(fMaximumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_PAGEUP:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue-fLargeStep)>=fMinimumValue) and not (fValue<(fValue-fLargeStep)) then begin
       SetValue(fValue-fLargeStep);
      end else begin
       SetValue(fMinimumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_HOME:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      SetValue(fMinimumValue);
     end;
    end;
    result:=true;
   end;
   KEYCODE_END:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      SetValue(fMaximumValue);
     end;
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUIScrollBar.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Step:TpvInt64;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  result:=inherited PointerEvent(aPointerEvent);
  if not result then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     if not Focused then begin
      RequestFocus;
     end;
     fFocusedSubWidget:=pvgsbswNone;
     fPushedSubWidget:=pvgsbswNone;
     fSliderPushed:=false;
     fStepSize:=0;
     fTimeAccumulator:=MaxDouble;
     case fOrientation of
      pvgsboHorizontal:begin
       if aPointerEvent.Position.x<fButtonSize then begin
        fFocusedSubWidget:=pvgsbswDecButton;
        fPushedSubWidget:=pvgsbswDecButton;
       end else if aPointerEvent.Position.x>=(Width-fButtonSize) then begin
        fFocusedSubWidget:=pvgsbswIncButton;
        fPushedSubWidget:=pvgsbswIncButton;
       end else if GetSliderButtonRect.Touched(aPointerEvent.Position) then begin
        fFocusedSubWidget:=pvgsbswSliderButton;
        fPushedSubWidget:=pvgsbswSliderButton;
       end else begin
        fSliderPushed:=true;
       end;
      end;
      else {pvgsboVertical:}begin
       if aPointerEvent.Position.y<fButtonSize then begin
        fFocusedSubWidget:=pvgsbswDecButton;
        fPushedSubWidget:=pvgsbswDecButton;
       end else if aPointerEvent.Position.y>=(Height-fButtonSize) then begin
        fFocusedSubWidget:=pvgsbswIncButton;
        fPushedSubWidget:=pvgsbswIncButton;
       end else if GetSliderButtonRect.Touched(aPointerEvent.Position) then begin
        fFocusedSubWidget:=pvgsbswSliderButton;
        fPushedSubWidget:=pvgsbswSliderButton;
       end else begin
        fSliderPushed:=true;
       end;
      end;
     end;
     if fPushedSubWidget=pvgsbswSliderButton then begin
(*    case fOrientation of
       pvgsboHorizontal:begin
        SetValue(round(fMinimumValue+((aPointerEvent.Position.x-(fButtonSize+(SliderButtonSize*0.5)))*(Max(1,fMaximumValue-fMinimumValue)/(Width-((fButtonSize*2.0)+(SliderButtonSize*1.0)))))));
       end;
       else {pvgsboVertical:}begin
        SetValue(round(fMinimumValue+((aPointerEvent.Position.y-(fButtonSize+(SliderButtonSize*0.5)))*(Max(1,fMaximumValue-fMinimumValue)/(Height-((fButtonSize*2.0)+(SliderButtonSize*1.0)))))));
       end;
      end;*)
     end else if fSliderPushed then begin
      fFocusedSubWidget:=pvgsbswSliderButton;
      fPushedSubWidget:=pvgsbswSliderButton;
      case fOrientation of
       pvgsboHorizontal:begin
        if aPointerEvent.Position.x<GetSliderButtonRect.Left then begin
         fStepSize:=-fLargeStep;
        end else begin
         fStepSize:=fLargeStep;
        end;
       end;
       else {pvgsboVertical:}begin
        if aPointerEvent.Position.y<GetSliderButtonRect.Top then begin
         fStepSize:=-fLargeStep;
        end else begin
         fStepSize:=fLargeStep;
        end;
       end;
      end;
      fTimeAccumulator:=0.5;
      if ((fStepSize>0) and ((fValue+fStepSize)<=fMaximumValue) and not (fValue>(fValue+fStepSize))) or
         ((fStepSize<0) and ((fValue+fStepSize)>=fMinimumValue) and not (fValue<(fValue+fStepSize))) then begin
       SetValue(fValue+fStepSize);
      end else if fStepSize<0 then begin
       SetValue(fMinimumValue);
      end else if fStepSize>0 then begin
       SetValue(fMaximumValue);
      end;
     end else begin
      case fPushedSubWidget of
       pvgsbswDecButton:begin
        if ((fValue-fSmallStep)>=fMinimumValue) and not (fValue<(fValue-fSmallStep)) then begin
         SetValue(fValue-fSmallStep);
        end else begin
         SetValue(fMinimumValue);
        end;
        fStepSize:=-fSmallStep;
        fTimeAccumulator:=0.5;
       end;
       pvgsbswIncButton:begin
        if ((fValue+fSmallStep)<=fMaximumValue) and not (fValue>(fValue+fSmallStep)) then begin
         SetValue(fValue+fSmallStep);
        end else begin
         SetValue(fMaximumValue);
        end;
        fStepSize:=fSmallStep;
        fTimeAccumulator:=0.5;
       end;
      end;
     end;
     result:=true;
    end;
    POINTEREVENT_UP:begin
     fPushedSubWidget:=pvgsbswNone;
     fSliderPushed:=false;
     fStepSize:=0;
     result:=true;
    end;
    POINTEREVENT_MOTION:begin
     result:=true;
    end;
    POINTEREVENT_DRAG:begin
{$if true}
     case fOrientation of
      pvgsboHorizontal:begin
       SetValue(round(fMinimumValue+((aPointerEvent.Position.x-(fButtonSize+(SliderButtonSize*0.5)))*((fMaximumValue-fMinimumValue)/Max(1,Width-((fButtonSize*2.0)+(SliderButtonSize*1.0)))))));
      end;
      else {pvgsboVertical:}begin
       SetValue(round(fMinimumValue+((aPointerEvent.Position.y-(fButtonSize+(SliderButtonSize*0.5)))*((fMaximumValue-fMinimumValue)/Max(1,Height-((fButtonSize*2.0)+(SliderButtonSize*1.0)))))));
      end;
     end;
{$else}
     case fOrientation of
      pvgsboHorizontal:begin
       if (aPointerEvent.Position.x>=GetSliderButtonRect.Left) and
          (aPointerEvent.Position.x<=GetSliderButtonRect.Right) then begin
        Step:=round(aPointerEvent.RelativePosition.x*((fMaximumValue-fMinimumValue)/Max(1,Width-((fButtonSize*2.0)+SliderButtonSize))));
       end else begin
        Step:=0;
       end;
      end;
      else {pvgsboVertical:}begin
       if (aPointerEvent.Position.y>=GetSliderButtonRect.Top) and
          (aPointerEvent.Position.y<=GetSliderButtonRect.Bottom) then begin
        Step:=round(aPointerEvent.RelativePosition.y*((fMaximumValue-fMinimumValue)/Max(1,Height-((fButtonSize*2.0)+SliderButtonSize))));
       end else begin
        Step:=0;
       end;
      end;
     end;
     if ((Step>0) and ((fValue+Step)<=fMaximumValue) and not (fValue>(fValue+Step))) or
        ((Step<0) and ((fValue+Step)>=fMinimumValue) and not (fValue<(fValue+Step))) then begin
      SetValue(fValue+Step);
     end else if Step<0 then begin
      SetValue(fMinimumValue);
     end else if Step>0 then begin
      SetValue(fMaximumValue);
     end;
{$ifend}
     result:=true;
    end;
   end;
  end;
 end;
end;

function TpvGUIScrollBar.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var TemporaryValue,Step:TpvInt64;
    v:TpvFloat;
begin
 result:=inherited Scrolled(aPosition,aRelativeAmount);
 if not result then begin
  TemporaryValue:=Value;
  v:=aRelativeAmount.x-aRelativeAmount.y;
  if v<0.0 then begin
   Step:=floor(v);
  end else begin
   Step:=ceil(v);
  end;
  if ((Step>0) and ((TemporaryValue+Step)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+Step))) or
     ((Step<0) and ((TemporaryValue+Step)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue+Step))) then begin
   SetValue(TemporaryValue+Step);
  end else if Step<0 then begin
   SetValue(fMinimumValue);
  end else if Step>0 then begin
   SetValue(fMaximumValue);
  end;
  result:=true;
 end;
end;

procedure TpvGUIScrollBar.PerformLayout;
begin
 fCachedSliderButtonSize:=-1.0;
 inherited PerformLayout;
end;

procedure TpvGUIScrollBar.Update;
begin
 if fStepSize<>0 then begin
  fTimeAccumulator:=fTimeAccumulator-fInstance.fDeltaTime;
  if fTimeAccumulator<0.0 then begin
   fTimeAccumulator:=fTimeAccumulator+0.1;
   if ((fStepSize>0) and ((fValue+fStepSize)<=fMaximumValue) and not (fValue>(fValue+fStepSize))) or
      ((fStepSize<0) and ((fValue+fStepSize)>=fMinimumValue) and not (fValue<(fValue+fStepSize))) then begin
    SetValue(fValue+fStepSize);
   end else if fStepSize<0 then begin
    SetValue(fMinimumValue);
   end else if fStepSize>0 then begin
    SetValue(fMaximumValue);
   end;
  end;
 end;
 Skin.DrawScrollBar(fCanvas,self);
 inherited Update;
end;

procedure TpvGUIScrollBar.Draw;
begin
 inherited Draw;
end;

constructor TpvGUISlider.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 Include(fWidgetFlags,pvgwfTabStop);
 Include(fWidgetFlags,pvgwfDrawFocus);
 Include(fWidgetFlags,pvgwfDraggable);

 fOrientation:=pvgsoHorizontal;

 fMinimumValue:=0;

 fMaximumValue:=100;

 fValue:=0;

 fSmallStep:=1;

 fLargeStep:=10;

 fButtonSize:=24;

 fSliderButtonSize:=16;

 fSliderPushed:=false;

 fOnChange:=nil;

 fFocusedSubWidget:=pvgsswNone;

 fPushedSubWidget:=pvgsswNone;

 fStepSize:=0;

 fTimeAccumulator:=MaxDouble;

end;

destructor TpvGUISlider.Destroy;
begin

 inherited Destroy;

end;

procedure TpvGUISlider.SetOrientation(const aOrientation:TpvGUISliderOrientation);
begin
 if fOrientation<>aOrientation then begin
  fOrientation:=aOrientation;
 end;
end;

procedure TpvGUISlider.SetMinimumValue(const aMinimumValue:TpvInt64);
begin
 if fMinimumValue<>aMinimumValue then begin
  fMinimumValue:=aMinimumValue;
  SetValue(fValue);
 end;
end;

procedure TpvGUISlider.SetMaximumValue(const aMaximumValue:TpvInt64);
begin
 if fMaximumValue<>aMaximumValue then begin
  fMaximumValue:=aMaximumValue;
  SetValue(fValue);
 end;
end;

procedure TpvGUISlider.SetValue(const aValue:TpvInt64);
var NewValue:TpvInt64;
begin
 NewValue:=Min(Max(aValue,fMinimumValue),fMaximumValue);
 if fValue<>NewValue then begin
  fValue:=NewValue;
  if assigned(fOnChange) then begin
   fOnChange(self);
  end;
 end;
end;

procedure TpvGUISlider.SetButtonSize(const aButtonSize:TpvFloat);
begin
 if fButtonSize<>aButtonSize then begin
  fButtonSize:=aButtonSize;
 end;
end;

procedure TpvGUISlider.SetSliderButtonSize(const aSliderButtonSize:TpvFloat);
begin
 if fSliderButtonSize<>aSliderButtonSize then begin
  fSliderButtonSize:=aSliderButtonSize;
 end;
end;

function TpvGUISlider.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetSliderPreferredSize(self);
end;

function TpvGUISlider.GetSliderButtonRect:TpvRect;
begin
 case fOrientation of
  pvgsoHorizontal:begin
   if fMinimumValue<fMaximumValue then begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(((fSize.x-fSliderButtonSize)*
                                                      ((fValue-fMinimumValue)/Max(1,fMaximumValue-fMinimumValue))),
                                                     0.0),
                                   TpvVector2.Create(fSliderButtonSize,fSize.y));
   end else begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(fButtonSize,0.0),
                                   TpvVector2.Create(fSliderButtonSize,fSize.y));
   end;
  end;
  else {pvgsoVertical:}begin
   if fMinimumValue<fMaximumValue then begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(0.0,
                                                     ((fSize.y-fSliderButtonSize)*
                                                       ((fValue-fMinimumValue)/Max(1,fMaximumValue-fMinimumValue)))),
                                   TpvVector2.Create(fSize.x,fSliderButtonSize));
   end else begin
    result:=TpvRect.CreateRelative(TpvVector2.Create(0.0,fButtonSize),
                                   TpvVector2.Create(fSize.x,fSliderButtonSize));
   end;
  end;
 end;
{result.LeftTop:=result.LeftTop+TpvVector2.Create(1.0,1.0);
 result.RightBottom:=result.RightBottom-TpvVector2.Create(1.0,1.0);}
end;

function TpvGUISlider.Enter:boolean;
begin
 result:=inherited Enter;
end;

function TpvGUISlider.Leave:boolean;
begin
 fPushedSubWidget:=pvgsswNone;
 fStepSize:=0;
 fSliderPushed:=false;
 result:=inherited Leave;
end;

function TpvGUISlider.PointerEnter:boolean;
begin
 result:=inherited PointerEnter;
end;

function TpvGUISlider.PointerLeave:boolean;
begin
 if assigned(fInstance) and (fInstance.fDragWidget<>self) then begin
  fPushedSubWidget:=pvgsswNone;
 end;
 result:=inherited PointerLeave;
end;

function TpvGUISlider.DragEvent(const aPosition:TpvVector2):boolean;
begin
 result:=GetSliderButtonRect.Touched(aPosition);
end;

function TpvGUISlider.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=assigned(fOnKeyEvent) and fOnKeyEvent(self,aKeyEvent);
 if Enabled and not result then begin
  case aKeyEvent.KeyCode of
   KEYCODE_LEFT,KEYCODE_UP,KEYCODE_MINUS,KEYCODE_KP_MINUS:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue-fSmallStep)>=fMinimumValue) and not (fValue<(fValue-fSmallStep)) then begin
       SetValue(fValue-fSmallStep);
      end else begin
       SetValue(fMinimumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_RIGHT,KEYCODE_DOWN,KEYCODE_PLUS,KEYCODE_KP_PLUS:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue+fSmallStep)<=fMaximumValue) and not (fValue>(fValue+fSmallStep)) then begin
       SetValue(fValue+fSmallStep);
      end else begin
       SetValue(fMaximumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_PAGEDOWN:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue+fLargeStep)<=fMaximumValue) and not (fValue>(fValue+fLargeStep)) then begin
       SetValue(fValue+fLargeStep);
      end else begin
       SetValue(fMaximumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_PAGEUP:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      if ((fValue-fLargeStep)>=fMinimumValue) and not (fValue<(fValue-fLargeStep)) then begin
       SetValue(fValue-fLargeStep);
      end else begin
       SetValue(fMinimumValue);
      end;
     end;
    end;
    result:=true;
   end;
   KEYCODE_HOME:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      SetValue(fMinimumValue);
     end;
    end;
    result:=true;
   end;
   KEYCODE_END:begin
    case aKeyEvent.KeyEventType of
     KEYEVENT_TYPED:begin
      SetValue(fMaximumValue);
     end;
    end;
    result:=true;
   end;
  end;
 end;
end;

function TpvGUISlider.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Step:TpvInt64;
begin
 result:=assigned(fOnPointerEvent) and fOnPointerEvent(self,aPointerEvent);
 if not result then begin
  result:=inherited PointerEvent(aPointerEvent);
  if not result then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     if not Focused then begin
      RequestFocus;
     end;
     fFocusedSubWidget:=pvgsswNone;
     fPushedSubWidget:=pvgsswNone;
     fSliderPushed:=false;
     fStepSize:=0;
     fTimeAccumulator:=MaxDouble;
     case fOrientation of
      pvgsoHorizontal:begin
       if GetSliderButtonRect.Touched(aPointerEvent.Position) then begin
        fFocusedSubWidget:=pvgsswSliderButton;
        fPushedSubWidget:=pvgsswSliderButton;
       end else begin
        fSliderPushed:=true;
       end;
      end;
      else {pvgsoVertical:}begin
       if GetSliderButtonRect.Touched(aPointerEvent.Position) then begin
        fFocusedSubWidget:=pvgsswSliderButton;
        fPushedSubWidget:=pvgsswSliderButton;
       end else begin
        fSliderPushed:=true;
       end;
      end;
     end;
     if fPushedSubWidget=pvgsswSliderButton then begin
(*    case fOrientation of
       pvgsoHorizontal:begin
        SetValue(round(fMinimumValue+((aPointerEvent.Position.x-(fButtonSize+(fSliderButtonSize*0.5)))*((fMaximumValue-fMinimumValue)/Max(1,Width-((fButtonSize*2.0)+(fSliderButtonSize*1.0)))))));
       end;
       else {pvgsoVertical:}begin
        SetValue(round(fMinimumValue+((aPointerEvent.Position.y-(fButtonSize+(fSliderButtonSize*0.5)))*((fMaximumValue-fMinimumValue)/Max(1,Height-((fButtonSize*2.0)+(fSliderButtonSize*1.0)))))));
       end;
      end;*)
     end else if fSliderPushed then begin
      fFocusedSubWidget:=pvgsswSliderButton;
      fPushedSubWidget:=pvgsswSliderButton;
      case fOrientation of
       pvgsoHorizontal:begin
        if aPointerEvent.Position.x<GetSliderButtonRect.Left then begin
         fStepSize:=-fLargeStep;
        end else begin
         fStepSize:=fLargeStep;
        end;
       end;
       else {pvgsoVertical:}begin
        if aPointerEvent.Position.y<GetSliderButtonRect.Top then begin
         fStepSize:=-fLargeStep;
        end else begin
         fStepSize:=fLargeStep;
        end;
       end;
      end;
      if ((fStepSize>0) and ((fValue+fStepSize)<=fMaximumValue) and not (fValue>(fValue+fStepSize))) or
         ((fStepSize<0) and ((fValue+fStepSize)>=fMinimumValue) and not (fValue<(fValue+fStepSize))) then begin
       SetValue(fValue+fStepSize);
      end else if fStepSize<0 then begin
       SetValue(fMinimumValue);
      end else if fStepSize>0 then begin
       SetValue(fMaximumValue);
      end;
      fTimeAccumulator:=0.5;
     end;
     result:=true;
    end;
    POINTEREVENT_UP:begin
     fPushedSubWidget:=pvgsswNone;
     fSliderPushed:=false;
     fStepSize:=0;
     result:=true;
    end;
    POINTEREVENT_MOTION:begin
     result:=true;
    end;
    POINTEREVENT_DRAG:begin
{$if true}
     case fOrientation of
      pvgsoHorizontal:begin
       SetValue(round(fMinimumValue+((aPointerEvent.Position.x-(fSliderButtonSize*0.5))*((fMaximumValue-fMinimumValue)/Max(1,Width-(fSliderButtonSize*1.0))))));
      end;
      else {pvgsoVertical:}begin
       SetValue(round(fMinimumValue+((aPointerEvent.Position.y-(fSliderButtonSize*0.5))*((fMaximumValue-fMinimumValue)/Max(1,Height-(fSliderButtonSize*1.0))))));
      end;
     end;
{$else}
     case fOrientation of
      pvgsoHorizontal:begin
       Step:=round(aPointerEvent.RelativePosition.x*((fMaximumValue-fMinimumValue)/Max(1,Width-SliderButtonSize)));
      end;
      else {pvgsoVertical:}begin
       Step:=round(aPointerEvent.RelativePosition.y*((fMaximumValue-fMinimumValue)/Max(1,Height-SliderButtonSize)));
      end;
     end;
     if ((Step>0) and ((fValue+Step)<=fMaximumValue) and not (fValue>(fValue+Step))) or
        ((Step<0) and ((fValue+Step)>=fMinimumValue) and not (fValue<(fValue+Step))) then begin
      SetValue(fValue+Step);
     end else if Step<0 then begin
      SetValue(fMinimumValue);
     end else if Step>0 then begin
      SetValue(fMaximumValue);
     end;
{$ifend}
     result:=true;
    end;
   end;
  end;
 end;
end;

function TpvGUISlider.Scrolled(const aPosition,aRelativeAmount:TpvVector2):boolean;
var TemporaryValue,Step:TpvInt64;
    v:TpvFloat;
begin
 result:=inherited Scrolled(aPosition,aRelativeAmount);
 if not result then begin
  TemporaryValue:=Value;
  v:=aRelativeAmount.x-aRelativeAmount.y;
  if v<0.0 then begin
   Step:=floor(v);
  end else begin
   Step:=ceil(v);
  end;
  if ((Step>0) and ((TemporaryValue+Step)<=fMaximumValue) and not (TemporaryValue>(TemporaryValue+Step))) or
     ((Step<0) and ((TemporaryValue+Step)>=fMinimumValue) and not (TemporaryValue<(TemporaryValue+Step))) then begin
   SetValue(TemporaryValue+Step);
  end else if Step<0 then begin
   SetValue(fMinimumValue);
  end else if Step>0 then begin
   SetValue(fMaximumValue);
  end;
  result:=true;
 end;
end;

procedure TpvGUISlider.Update;
begin
 if fStepSize<>0 then begin
  fTimeAccumulator:=fTimeAccumulator-fInstance.fDeltaTime;
  if fTimeAccumulator<0.0 then begin
   fTimeAccumulator:=fTimeAccumulator+0.1;
   if ((fStepSize>0) and ((fValue+fStepSize)<=fMaximumValue) and not (fValue>(fValue+fStepSize))) or
      ((fStepSize<0) and ((fValue+fStepSize)>=fMinimumValue) and not (fValue<(fValue+fStepSize))) then begin
    SetValue(fValue+fStepSize);
   end else if fStepSize<0 then begin
    SetValue(fMinimumValue);
   end else if fStepSize>0 then begin
    SetValue(fMaximumValue);
   end;
  end;
 end;
 Skin.DrawSlider(fCanvas,self);
 inherited Update;
end;

procedure TpvGUISlider.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIProgressBar.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fOrientation:=pvgpboHorizontal;

 fMinimumValue:=0;

 fMaximumValue:=100;

 fValue:=0;

 fOnChange:=nil;

end;

destructor TpvGUIProgressBar.Destroy;
begin

 inherited Destroy;

end;

procedure TpvGUIProgressBar.SetOrientation(const aOrientation:TpvGUIProgressBarOrientation);
begin
 if fOrientation<>aOrientation then begin
  fOrientation:=aOrientation;
 end;
end;

procedure TpvGUIProgressBar.SetMinimumValue(const aMinimumValue:TpvInt64);
begin
 if fMinimumValue<>aMinimumValue then begin
  fMinimumValue:=aMinimumValue;
  SetValue(fValue);
 end;
end;

procedure TpvGUIProgressBar.SetMaximumValue(const aMaximumValue:TpvInt64);
begin
 if fMaximumValue<>aMaximumValue then begin
  fMaximumValue:=aMaximumValue;
  SetValue(fValue);
 end;
end;

procedure TpvGUIProgressBar.SetValue(const aValue:TpvInt64);
var NewValue:TpvInt64;
begin
 NewValue:=Min(Max(aValue,fMinimumValue),fMaximumValue);
 if fValue<>NewValue then begin
  fValue:=NewValue;
  if assigned(fOnChange) then begin
   fOnChange(self);
  end;
 end;
end;

function TpvGUIProgressBar.GetPreferredSize:TpvVector2;
begin
 result:=Skin.GetProgressBarPreferredSize(self);
end;

procedure TpvGUIProgressBar.Update;
begin
 Skin.DrawProgressBar(fCanvas,self);
 inherited Update;
end;

procedure TpvGUIProgressBar.Draw;
begin
 inherited Draw;
end;

constructor TpvGUIScrollPanel.Create(const aParent:TpvGUIObject);
begin

 inherited Create(aParent);

 fHorizontalScrollDirection:=TpvGUIScrollPanelScrollDirection.pvgspsdfAuto;

 fVerticalScrollDirection:=TpvGUIScrollPanelScrollDirection.pvgspsdfAuto;

 fClipContentPanel:=TpvGUIPanel.Create(self);
 Include(fClipContentPanel.fWidgetFlags,pvgwfScissor);

 fContent:=TpvGUIPanel.Create(fClipContentPanel);

 fContent.Layout:=TpvGUIBoxLayout.Create(fContent,pvglaMiddle,pvgloHorizontal,0.0,4.0);

 fHorizontalScrollBar:=TpvGUIScrollBar.Create(self);
 fHorizontalScrollBar.Orientation:=pvgsboHorizontal;
 fHorizontalScrollBar.OnChange:=HorizontalScrollBarOnChange;

 fVerticalScrollBar:=TpvGUIScrollBar.Create(self);
 fVerticalScrollBar.Orientation:=pvgsboVertical;
 fVerticalScrollBar.OnChange:=VerticalScrollBarOnChange;

end;

destructor TpvGUIScrollPanel.Destroy;
begin
 inherited Destroy;
end;

procedure TpvGUIScrollPanel.SetHorizontalScrollDirection(const aHorizontalScrollDirection:TpvGUIScrollPanelScrollDirection);
begin
 if fHorizontalScrollDirection<>aHorizontalScrollDirection then begin
  fHorizontalScrollDirection:=aHorizontalScrollDirection;
  PerformLayout;
 end;
end;

procedure TpvGUIScrollPanel.SetVerticalScrollDirection(const aVerticalScrollDirection:TpvGUIScrollPanelScrollDirection);
begin
 if fVerticalScrollDirection<>aVerticalScrollDirection then begin
  fVerticalScrollDirection:=aVerticalScrollDirection;
  PerformLayout;
 end;
end;

function TpvGUIScrollPanel.GetPreferredSize:TpvVector2;
var ContentPreferredSize,
    HorizontalScrollBarPreferredSize,
    VerticalScrollBarPreferredSize:TpvVector2;
begin

 ContentPreferredSize:=fContent.GetPreferredSize;

 HorizontalScrollBarPreferredSize:=fHorizontalScrollBar.GetPreferredSize;

 VerticalScrollBarPreferredSize:=fVerticalScrollBar.GetPreferredSize;

 result:=ContentPreferredSize;

 if fHorizontalScrollDirection=pvgspsdfOn then begin
  //result.x:=Max(result.x,HorizontalScrollBarPreferredSize.x);
  result.y:=result.y+HorizontalScrollBarPreferredSize.y;
 end;

 if fVerticalScrollDirection=pvgspsdfOn then begin
  result.x:=result.x+VerticalScrollBarPreferredSize.x;
  //result.y:=Max(result.y,VerticalScrollBarPreferredSize.y);
 end;

end;

procedure TpvGUIScrollPanel.PerformLayout;
var Index,OldState,NewState:TpvInt32;
    ContentPreferredSize,
    HorizontalScrollBarPreferredSize,
    VerticalScrollBarPreferredSize,
    AvailiableSize:TpvVector2;
begin

 AvailiableSize:=fSize;

 ContentPreferredSize:=fContent.GetPreferredSize;

 HorizontalScrollBarPreferredSize:=fHorizontalScrollBar.GetPreferredSize;

 VerticalScrollBarPreferredSize:=fVerticalScrollBar.GetPreferredSize;

 NewState:=0;

 for Index:=0 to 2 do begin

  OldState:=NewState;

  if (fHorizontalScrollDirection=pvgspsdfOn) or
     ((fHorizontalScrollDirection=pvgspsdfAuto) and (ContentPreferredSize.x>AvailiableSize.x)) then begin
   fHorizontalScrollBar.Visible:=true;
   NewState:=NewState or 1;
  end else begin
   fHorizontalScrollBar.Visible:=false;
  end;

  if (fVerticalScrollDirection=pvgspsdfOn) or
     ((fVerticalScrollDirection=pvgspsdfAuto) and (ContentPreferredSize.y>AvailiableSize.y)) then begin
   fVerticalScrollBar.Visible:=true;
   NewState:=NewState or 2;
  end else begin
   fVerticalScrollBar.Visible:=false;
  end;

  if fHorizontalScrollBar.Visible then begin
   fHorizontalScrollBar.fPosition:=TpvVector2.Create(0.0,fSize.y-HorizontalScrollBarPreferredSize.y);
   if fVerticalScrollBar.Visible then begin
    fHorizontalScrollBar.fSize:=TpvVector2.Create(fSize.x-VerticalScrollBarPreferredSize.x,HorizontalScrollBarPreferredSize.y);
   end else begin
    fHorizontalScrollBar.fSize:=TpvVector2.Create(fSize.x,HorizontalScrollBarPreferredSize.y);
   end;
   fHorizontalScrollBar.PerformLayout;
  end;

  if fVerticalScrollBar.Visible then begin
   fVerticalScrollBar.fPosition:=TpvVector2.Create(fSize.x-VerticalScrollBarPreferredSize.x,0.0);
   if fHorizontalScrollBar.Visible then begin
    fVerticalScrollBar.fSize:=TpvVector2.Create(VerticalScrollBarPreferredSize.x,fSize.y-HorizontalScrollBarPreferredSize.y);
   end else begin
    fVerticalScrollBar.fSize:=TpvVector2.Create(VerticalScrollBarPreferredSize.x,fSize.y);
   end;
   fVerticalScrollBar.PerformLayout;
  end;

  if ((NewState and 1)<>0) and ((OldState and 1)=0) then begin
   AvailiableSize.y:=AvailiableSize.y-HorizontalScrollBarPreferredSize.y;
  end;

  if ((NewState and 2)<>0) and ((OldState and 2)=0) then begin
   AvailiableSize.x:=AvailiableSize.x-VerticalScrollBarPreferredSize.x;
  end;

  if NewState=OldState then begin
   break;
  end;

 end;

 fHorizontalScrollBar.MinimumValue:=0;
 fHorizontalScrollBar.MaximumValue:=Max(0,ceil(ContentPreferredSize.x-AvailiableSize.x));

 fVerticalScrollBar.MinimumValue:=0;
 fVerticalScrollBar.MaximumValue:=Max(0,ceil(ContentPreferredSize.y-AvailiableSize.y));

 fClipContentPanel.fPosition:=TpvVector2.Null;
 fClipContentPanel.fSize:=AvailiableSize;

 fContent.fPosition:=TpvVector2.Create(-fHorizontalScrollBar.Value,
                                       -fVerticalScrollBar.Value);
 fContent.fSize:=ContentPreferredSize;
 fContent.PerformLayout;

end;

procedure TpvGUIScrollPanel.HorizontalScrollBarOnChange(const aSender:TpvGUIObject);
begin
 fContent.fPosition:=TpvVector2.Create(-fHorizontalScrollBar.Value,
                                       -fVerticalScrollBar.Value);
end;

procedure TpvGUIScrollPanel.VerticalScrollBarOnChange(const aSender:TpvGUIObject);
begin
 fContent.fPosition:=TpvVector2.Create(-fHorizontalScrollBar.Value,
                                       -fVerticalScrollBar.Value);
end;

procedure TpvGUIScrollPanel.Update;
begin
 inherited Update;
end;

procedure TpvGUIScrollPanel.Draw;
begin
 inherited Draw;
end;

end.
