(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2018, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.TextEditor;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Contnrs,
     Math,
     PasVulkan.Types;

type TpvUTF8DFA=class
      public                                            //0 1 2 3 4 5 6 7 8 9 a b c d e f
        const CodePointSizes:array[AnsiChar] of TpvUInt8=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                                          1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                                          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                                          3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                                          4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1); // f
              StateCharClasses:array[AnsiChar] of TpvUInt8=($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,
                                                            $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,
                                                            $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,
                                                            $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,
                                                            $08,$08,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,
                                                            $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,
                                                            $0a,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$04,$03,$03,
                                                            $0b,$06,$06,$06,$05,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08);
              StateTransitions:array[TpvUInt8] of TpvUInt8=($00,$10,$20,$30,$50,$80,$70,$10,$10,$10,$40,$60,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$00,$10,$10,$10,$10,$10,$00,$10,$00,$10,$10,$10,$10,$10,$10,
                                                            $10,$20,$10,$10,$10,$10,$10,$20,$10,$20,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$20,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$20,$10,$10,$10,$10,$10,$10,$10,$20,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$30,$10,$30,$10,$10,$10,$10,$10,$10,
                                                            $10,$30,$10,$10,$10,$10,$10,$30,$10,$30,$10,$10,$10,$10,$10,$10,
                                                            $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10);
               StateAccept=0;
               StateError=16;
               StateCharClassSingleByte=0;
     end;

     TpvUTF8Utils=class
      public
       const cpLATIN1=28591;
             cpISO_8859_1=28591;
             cpUTF16LE=1200;
             cpUTF16BE=1201;
             cpUTF7=65000;
             cpUTF8=65001;
      private
       const UTF16LittleEndianBigEndianShifts:array[0..1,0..1] of TpvInt32=((0,8),(8,0));
             UTF32LittleEndianBigEndianShifts:array[0..1,0..3] of TpvInt32=((0,8,16,24),(24,16,8,0));
      public
       class function UTF32CharToUTF8(const aCodePoint:TpvUInt32):TpVUTF8String; static;
       class function UTF8Validate(const aString:TpvUTF8String):boolean; static;
       class function UTF8Correct(const aString:TpvUTF8String):TpvUTF8String; static;
       class function RawDataToUTF8String(const aData;const aDataLength:TpvInt32;const aCodePage:TpvInt32=-1):TpvUTF8String; static;
       class function RawByteStringToUTF8String(const aString:TpvRawByteString;const aCodePage:TpvInt32=-1):TpvUTF8String; static;
       class function RawStreamToUTF8String(const aStream:TStream;const aCodePage:TpvInt32=-1):TpvUTF8String; static;
     end;

     EpvUTF8StringRope=class(Exception);

     TpvUTF8StringRope=class
      public
       type TNode=class
             public
              const StringSize=128;
                    MaximumHeight=60;
                    MaximumHeightMinusOne=MaximumHeight-1;
                    MaximumHeightMinusTwoBitMask=TpvUInt64((TpvUInt64(1) shl (MaximumHeight-2))-1);
              type TNodeLink=record
                    private
                     fNode:TNode;
                     fSkipSize:TpvSizeInt;
                   end;
                   PNodeLink=^TNodeLink;
                   TNodeLinks=array of TNodeLink; // index 0 is linked-list-next
                   TNodePositionLinks=array[0..MaximumHeight-1] of TNodeLink;
                   TData=array[0..StringSize-1] of AnsiChar;
             private
              fData:TData;
              fCountCodeUnits:TpvSizeInt;
              fHeight:TpvInt32;
              fLinks:TNodeLinks;
              function GetData:TpvUTF8String;
             public
              constructor Create(const aHeight:TpvInt32); reintroduce;
              destructor Destroy; override;
              property Data:TpvUTF8String read GetData;
            end;
            TNodeEnumerator=record
             private
              fStringRope:TpvUTF8StringRope;
              fFirst:boolean;
              fNode:TNode;
              function GetCurrent:TNode; inline;
             public
              constructor Create(const aCodeUnitsRope:TpvUTF8StringRope);
              function MoveNext:boolean; inline;
              property Current:TNode read GetCurrent;
            end;
            TCodePointEnumerator=record
             private
              fStringRope:TpvUTF8StringRope;
              fFirst:boolean;
              fNode:TNode;
              fNodeCodeUnitIndex:TpvSizeInt;
              fCodePointIndex:TpvSizeInt;
              fStopCodePointIndex:TpvSizeInt;
              fCodePoint:TpvUInt32;
              fUTF8DFACharClass:TpvUInt8;
              fUTF8DFAState:TpvUInt8;
              function GetCurrent:TpvUInt32;
             public
              constructor Create(const aCodeUnitsRope:TpvUTF8StringRope;const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1);
              function MoveNext:boolean; inline;
              property Current:TpvUInt32 read GetCurrent;
            end;
            TCodePointEnumeratorSource=record
             private
              fStringRope:TpvUTF8StringRope;
              fStartCodePointIndex:TpvSizeInt;
              fStopCodePointIndex:TpvSizeInt;
             public
              constructor Create(const aCodeUnitsRope:TpvUTF8StringRope;const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1);
              function GetEnumerator:TCodePointEnumerator;
            end;
            TRandomGenerator=record
             private
              fState:TpvUInt64;
              fIncrement:TpvUInt64;
             public
              constructor Create(const aSeed:TpvUInt64);
              function GetUInt32:TpvUInt32;
              function GetUInt64:TpvUInt64;
            end;
      private
       fCountCodePoints:TpvSizeInt;
       fCountCodeUnits:TpvSizeInt;
       fHead:TNode;
       fRandomGenerator:TRandomGenerator;
       function GetText:TpvUTF8String;
       procedure SetText(const aCodeUnits:TpvUTF8String);
       class function FindFirstSetBit(aValue:TpvUInt64):TpvUInt32; {$ifndef fpc}{$ifdef cpu386}stdcall;{$else}register;{$endif}{$endif} static;
       function GetRandomHeight:TpvInt32;
       class function GetCountCodeUnits(const aCodeUnits:PAnsiChar;const aCountCodePoints:TpvSizeInt):TpvSizeInt; static;
       class function GetCountCodePoints(const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt):TpvSizeInt; static;
       class procedure UTF8Check(const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt); static;
       function FindNodePositionAtCodePoint(const aCodePointIndex:TpvSizeInt;out aNodePositionLinks:TNode.TNodePositionLinks):TNode;
       procedure UpdateOffsetList(var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
       procedure InsertAt(var aNodePositionLinks:TNode.TNodePositionLinks;const aCodeUnits:PAnsiChar;const aCountCodeUnits,aCountCodePoints:TpvSizeInt);
       procedure InsertAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt);
       procedure DeleteAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
       function ExtractAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt):TpvUTF8String;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aCodeUnits:TpvUTF8String); reintroduce; overload;
       constructor Create(const aFrom:TpvUTF8StringRope); reintroduce; overload;
       destructor Destroy; override;
       procedure Clear;
       function GetNodeAndOffsetFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aNode:TNode;out aNodeCodeUnitIndex:TpvSizeInt):boolean;
       procedure Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt); overload;
       procedure Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:TpvUTF8String); overload;
       procedure Delete(const aCodePointIndex,aCountCodePoints:TpvSizeInt);
       function Extract(const aCodePointIndex,aCountCodePoints:TpvSizeInt):TpvUTF8String;
       function GetCodePoint(const aCodePointIndex:TpvSizeInt):TpvUInt32;
       function GetEnumerator:TNodeEnumerator;
       function GetCodePointEnumeratorSource(const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1):TpvUTF8StringRope.TCodePointEnumeratorSource;
       procedure Check;
       procedure Dump;
       property CountCodePoints:TpvSizeInt read fCountCodePoints;
       property CountCodeUnits:TpvSizeInt read fCountCodeUnits;
       property Text:TpvUTF8String read GetText write SetText;
     end;

     TpvUTF8StringRopeLineMap=class
      public
       type TLine=TpvSizeInt;
            PLine=^TLine;
            TLines=array of TLine;
      private
       fStringRope:TpvUTF8StringRope;
       fLines:TLines;
       fCountLines:TpvSizeInt;
       fLineWrap:TpvSizeInt;
       fTabWidth:TpvSizeInt;
       fCountVisibleVisualCodePointsSinceNewLine:TpvSizeInt;
       fCodePointIndex:TpvSizeInt;
       fLastWasPossibleNewLineTwoCharSequence:boolean;
       fLastCodePoint:TpvUInt32;
       procedure SetLineWrap(const aLineWrap:TpvSizeInt);
       procedure SetTabWidth(const aTabWidth:TpvSizeInt);
       procedure AddLine(const aCodePointIndex:TpvSizeInt);
      public
       constructor Create(const aRope:TpvUTF8StringRope); reintroduce;
       destructor Destroy; override;
       procedure Reset;
       procedure Truncate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
       procedure Update(const aUntilCodePoint,aUntilLine:TpvSizeInt);
       function GetLineIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt):TpvSizeInt;
       function GetLineIndexAndColumnIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aLineIndex,aColumnIndex:TpvSizeInt):boolean;
       function GetCodePointIndexFromLineIndex(const aLineIndex:TpvSizeInt):TpvSizeInt;
       function GetCodePointIndexFromNextLineIndexOrTextEnd(const aLineIndex:TpvSizeInt):TpvSizeInt;
       function GetCodePointIndexFromLineIndexAndColumnIndex(const aLineIndex,aColumnIndex:TpvSizeInt):TpvSizeInt;
      published
       property CountLines:TpvSizeInt read fCountLines;
       property LineWrap:TpvSizeInt read fLineWrap write SetLineWrap;
       property TabWidth:TpvSizeInt read fTabWidth write SetTabWidth;
     end;

     TpvTextEditor=class
      public
       type TCoordinate=record
             public
              x:TpvSizeInt;
              y:TpvSizeInt;
            end;
            PCoordinate=^TCoordinate;
            TLineColumn=record
             public
              Line:TpvSizeInt;
              Column:TpvSizeInt;
            end;
            PLineColumn=^TLineColumn;
            TView=class;
            TUndoRedoCommand=class
             private
              fParent:TpvTextEditor;
              fCursorCodePointIndex:TpvSizeInt;
             public
              constructor Create(const aParent:TpvTextEditor;const aCursorCodePointIndex:TpvSizeInt); reintroduce; virtual;
              destructor Destroy; override;
              procedure Undo(const aView:TpvTextEditor.TView=nil); virtual;
              procedure Redo(const aView:TpvTextEditor.TView=nil); virtual;
            end;
            TUndoRedoCommandInsert=class(TUndoRedoCommand)
             private
              fCodePointIndex:TpvSizeInt;
              fCountCodePoints:TpvSizeInt;
              fCodeUnits:TpvUTF8String;
             public
              constructor Create(const aParent:TpvTextEditor;const aCursorCodePointIndex,aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String); reintroduce;
              destructor Destroy; override;
              procedure Undo(const aView:TpvTextEditor.TView=nil); override;
              procedure Redo(const aView:TpvTextEditor.TView=nil); override;
            end;
            TUndoRedoCommandOverwrite=class(TUndoRedoCommand)
             private
              fCodePointIndex:TpvSizeInt;
              fCountCodePoints:TpvSizeInt;
              fCodeUnits:TpvUTF8String;
              fPreviousCodeUnits:TpvUTF8String;
             public
              constructor Create(const aParent:TpvTextEditor;const aCursorCodePointIndex,aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits,aPreviousCodeUnits:TpvUTF8String); reintroduce;
              destructor Destroy; override;
              procedure Undo(const aView:TpvTextEditor.TView=nil); override;
              procedure Redo(const aView:TpvTextEditor.TView=nil); override;
            end;
            TUndoRedoCommandDelete=class(TUndoRedoCommand)
             private
              fCodePointIndex:TpvSizeInt;
              fCountCodePoints:TpvSizeInt;
              fCodeUnits:TpvUTF8String;
             public
              constructor Create(const aParent:TpvTextEditor;const aCursorCodePointIndex,aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String); reintroduce;
              destructor Destroy; override;
              procedure Undo(const aView:TpvTextEditor.TView=nil); override;
              procedure Redo(const aView:TpvTextEditor.TView=nil); override;
            end;
            TView=class
             public
              type TBufferItem=record
                    BackgroundColor:TpvUInt8;
                    ForegroundColor:TpvUInt8;
                    CodePoint:TpvUInt32;
                   end;
                   PBufferItem=^TBufferItem;
                   TBufferItems=array of TBufferItem;
             private
              fParent:TpvTextEditor;
              fPrevious:TView;
              fNext:TView;
              fVisibleAreaDirty:boolean;
              fVisibleAreaWidth:TpvSizeInt;
              fVisibleAreaHeight:TpvSizeInt;
              fNonScrollVisibleAreaWidth:TpvSizeInt;
              fNonScrollVisibleAreaHeight:TpvSizeInt;
              fCodePointIndex:TpvSizeInt;
              fCursorOffset:TCoordinate;
              fCursor:TCoordinate;
              fLineColumn:TLineColumn;
              fLineWrap:TpvSizeInt;
              fStringRopeVisualLineMap:TpvUTF8StringRopeLineMap;
              fBuffer:TBufferItems;
              procedure SetVisibleAreaWidth(const aVisibleAreaWidth:TpvSizeInt);
              procedure SetVisibleAreaHeight(const aVisibleAreaHeight:TpvSizeInt);
              procedure SetNonScrollVisibleAreaWidth(const aNonScrollVisibleAreaWidth:TpvSizeInt);
              procedure SetNonScrollVisibleAreaHeight(const aNonScrollVisibleAreaHeight:TpvSizeInt);
              procedure SetLineWrap(const aLineWrap:TpvSizeInt);
              procedure SetLineColumn(const aLineColumn:TLineColumn);
             public
              constructor Create(const aParent:TpvTextEditor); reintroduce;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure EnsureCodePointIndexIsInRange;
              procedure EnsureCursorIsVisible(const aUpdateCursor:boolean=true;const aForceVisibleLines:TpvSizeInt=1);
              procedure UpdateCursor;
              procedure UpdateBuffer;
              procedure InsertCodePoint(const aCodePoint:TpvUInt32;const aOverwrite:boolean);
              procedure InsertString(const aCodeUnits:TpvUTF8String;const aOverwrite:boolean);
              procedure Backspace;
              procedure Delete;
              procedure Enter(const aOverwrite:boolean);
              procedure MoveUp;
              procedure MoveDown;
              procedure MoveLeft;
              procedure MoveRight;
              procedure MoveToLineBegin;
              procedure MoveToLineEnd;
              procedure MovePageUp;
              procedure MovePageDown;
              procedure InsertLine;
              procedure DeleteLine;
              procedure Undo;
              procedure Redo;
              property Buffer:TBufferItems read fBuffer;
              property Cursor:TCoordinate read fCursor;
              property LineColumn:TLineColumn read fLineColumn write SetLineColumn;
             published
              property VisibleAreaWidth:TpvSizeInt read fVisibleAreaWidth write SetVisibleAreaWidth;
              property VisibleAreaHeight:TpvSizeInt read fVisibleAreaHeight write SetVisibleAreaHeight;
              property NonScrollVisibleAreaWidth:TpvSizeInt read fNonScrollVisibleAreaWidth write SetNonScrollVisibleAreaWidth;
              property NonScrollVisibleAreaHeight:TpvSizeInt read fNonScrollVisibleAreaHeight write SetNonScrollVisibleAreaHeight;
              property LineWrap:TpvSizeInt read fLineWrap write SetLineWrap;
            end;
      private
       fStringRope:TpvUTF8StringRope;
       fStringRopeLineMap:TpvUTF8StringRopeLineMap;
       fFirstView:TView;
       fLastView:TView;
       fUndoStack:TObjectList;
       fRedoStack:TObjectList;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       function IsTwoCodePointNewLine(const aCodePointIndex:TpvSizeInt):boolean;
       procedure LoadFromStream(const aStream:TStream);
       procedure LoadFromFile(const aFileName:string);
       procedure LoadFromString(const aString:TpvRawByteString);
       procedure SaveToStream(const aStream:TStream);
       procedure SaveToFile(const aFileName:string);
       function SaveToString:TpvUTF8String;
       function CreateView:TpvTextEditor.TView;
       procedure LineMapTruncate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
       procedure LineMapUpdate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
       procedure ResetLineMaps;
       procedure ResetViewCodePointIndices;
       procedure UpdateViewCodePointIndices(const aCodePointIndex,aDelta:TpvSizeInt);
       procedure EnsureViewCodePointIndicesAreInRange;
       procedure EnsureViewCursorsAreVisible(const aUpdateCursors:boolean=true;const aForceVisibleLines:TpvSizeInt=1);
       procedure UpdateViewCursors;
       procedure Undo(const aView:TView=nil);
       procedure Redo(const aView:TView=nil);
      published
     end;

implementation

uses PUCU;

class function TpvUTF8Utils.UTF32CharToUTF8(const aCodePoint:TpvUInt32):TpVUTF8String;
var Data:array[0..3] of AnsiChar;
    ResultLen:TpvInt32;
begin
 if aCodePoint=0 then begin
  result:=#0;
 end else begin
  if aCodePoint<=$7f then begin
   Data[0]:=AnsiChar(TpvUInt8(aCodePoint));
   ResultLen:=1;
  end else if aCodePoint<=$7ff then begin
   Data[0]:=AnsiChar(TpvUInt8($c0 or ((aCodePoint shr 6) and $1f)));
   Data[1]:=AnsiChar(TpvUInt8($80 or (aCodePoint and $3f)));
   ResultLen:=2;
  end else if aCodePoint<=$d7ff then begin
   Data[0]:=AnsiChar(TpvUInt8($e0 or ((aCodePoint shr 12) and $0f)));
   Data[1]:=AnsiChar(TpvUInt8($80 or ((aCodePoint shr 6) and $3f)));
   Data[2]:=AnsiChar(TpvUInt8($80 or (aCodePoint and $3f)));
   ResultLen:=3;
  end else if aCodePoint<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end else if aCodePoint<=$ffff then begin
   Data[0]:=AnsiChar(TpvUInt8($e0 or ((aCodePoint shr 12) and $0f)));
   Data[1]:=AnsiChar(TpvUInt8($80 or ((aCodePoint shr 6) and $3f)));
   Data[2]:=AnsiChar(TpvUInt8($80 or (aCodePoint and $3f)));
   ResultLen:=3;
  end else if aCodePoint<=$1fffff then begin
   Data[0]:=AnsiChar(TpvUInt8($f0 or ((aCodePoint shr 18) and $07)));
   Data[1]:=AnsiChar(TpvUInt8($80 or ((aCodePoint shr 12) and $3f)));
   Data[2]:=AnsiChar(TpvUInt8($80 or ((aCodePoint shr 6) and $3f)));
   Data[3]:=AnsiChar(TpvUInt8($80 or (aCodePoint and $3f)));
   ResultLen:=4;
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,PAnsiChar(@Data[0]),ResultLen);
 end;
end;

class function TpvUTF8Utils.UTF8Validate(const aString:TpvUTF8String):boolean;
var Index:TpvSizeInt;
    State:TpvUInt32;
begin
 State:=TpvUTF8DFA.StateAccept;
 for Index:=1 to length(aString) do begin
  State:=TpvUTF8DFA.StateTransitions[State+TpvUTF8DFA.StateCharClasses[aString[Index]]];
  if State=TpvUTF8DFA.StateError then begin
   break;
  end;
 end;
 result:=State=TpvUTF8DFA.StateAccept;
end;

class function TpvUTF8Utils.UTF8Correct(const aString:TpvUTF8String):TpvUTF8String;
var CodeUnit,Len,ResultLen:TpvSizeInt;
    StartCodeUnit,Value,CharClass,State,CharValue:TpvUInt32;
    Data:PAnsiChar;
begin
 if (length(aString)=0) or UTF8Validate(aString) then begin
  result:=aString;
 end else begin
  result:='';
  CodeUnit:=1;
  Len:=length(aString);
  SetLength(result,Len*4);
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=TpvUTF8DFA.StateAccept;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=ord(aString[CodeUnit]);
    inc(CodeUnit);
    CharClass:=TpvUTF8DFA.StateCharClasses[AnsiChar(UInt8(Value))];
    if State=TpvUTF8DFA.StateAccept then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=TpvUTF8DFA.StateTransitions[State+CharClass];
    if State<=TpvUTF8DFA.StateError then begin
     break;
    end;
   end;
   if State<>TpvUTF8DFA.StateAccept then begin
    CharValue:=ord(aString[StartCodeUnit]);
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=AnsiChar(TpvUInt8(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=AnsiChar(TpvUInt8($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=AnsiChar(TpvUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,2);
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=AnsiChar(TpvUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=AnsiChar(TpvUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=AnsiChar(TpvUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=AnsiChar(TpvUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=AnsiChar(TpvUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=AnsiChar(TpvUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=AnsiChar(TpvUInt8($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=AnsiChar(TpvUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=AnsiChar(TpvUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=AnsiChar(TpvUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,4);
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

class function TpvUTF8Utils.RawDataToUTF8String(const aData;const aDataLength:TpvInt32;const aCodePage:TpvInt32=-1):TpvUTF8String;
type TBytes=array[0..65535] of TpvUInt8;
     PBytes=^TBytes;
var Bytes:PBytes;
    BytesPerCodeUnit,BytesPerCodeUnitMask,StartCodeUnit,CodeUnit,
    InputLen,OutputLen:TpvSizeInt;
    LittleEndianBigEndian,PassIndex,CodePoint,Temp:TpvUInt32;
    State,CharClass,Value:TpvUInt8;
    CodePage:PPUCUCharSetCodePage;
    SubCodePages:PPUCUCharSetSubCodePages;
    SubSubCodePages:PPUCUCharSetSubSubCodePages;
begin
 begin
  CodePage:=nil;
  if (aCodePage>=0) and (aCodePage<=65535) then begin
   SubCodePages:=PUCUCharSetCodePages[(aCodePage shr 8) and $ff];
   if assigned(SubCodePages) then begin
    SubSubCodePages:=SubCodePages^[(aCodePage shr 4) and $f];
    if assigned(SubSubCodePages) then begin
     CodePage:=SubSubCodePages^[(aCodePage shr 0) and $f];
    end;
   end;
  end;
 end;
 result:='';
 Bytes:=@aData;
 if aCodePage=cpUTF16LE then begin
  // UTF16 little endian (per code page)
  BytesPerCodeUnit:=2;
  BytesPerCodeUnitMask:=1;
  LittleEndianBigEndian:=0;
  if (aDataLength>=2) and
     ((Bytes^[0]=$ff) and (Bytes^[1]=$fe)) then begin
   Bytes:=@Bytes^[2];
   InputLen:=aDataLength-2;
  end else begin
   Bytes:=@Bytes^[0];
   InputLen:=aDataLength;
  end;
 end else if aCodePage=cpUTF16BE then begin
  // UTF16 big endian (per code page)
  BytesPerCodeUnit:=2;
  BytesPerCodeUnitMask:=1;
  LittleEndianBigEndian:=1;
  if (aDataLength>=2) and
     ((Bytes^[0]=$fe) and (Bytes^[1]=$ff)) then begin
   Bytes:=@Bytes^[2];
   InputLen:=aDataLength-2;
  end else begin
   Bytes:=@Bytes^[0];
   InputLen:=aDataLength;
  end;
 end else if aCodePage=cpUTF7 then begin
  // UTF7 (per code page)
  raise Exception.Create('UTF-7 not supported');
 end else if aCodePage=cpUTF8 then begin
  // UTF8 (per code page)
  BytesPerCodeUnit:=1;
  BytesPerCodeUnitMask:=0;
  LittleEndianBigEndian:=0;
  if (aDataLength>=3) and (Bytes^[0]=$ef) and (Bytes^[1]=$bb) and (Bytes^[2]=$bf) then begin
   Bytes:=@Bytes^[3];
   InputLen:=aDataLength-3;
  end else begin
   Bytes:=@Bytes^[0];
   InputLen:=aDataLength;
  end;
 end else if assigned(CodePage) then begin
  // Code page
  BytesPerCodeUnit:=0;
  BytesPerCodeUnitMask:=0;
  LittleEndianBigEndian:=0;
  Bytes:=@Bytes^[0];
  InputLen:=aDataLength;
 end else if (aDataLength>=3) and (Bytes^[0]=$ef) and (Bytes^[1]=$bb) and (Bytes^[2]=$bf) then begin
  // UTF8
  BytesPerCodeUnit:=1;
  BytesPerCodeUnitMask:=0;
  LittleEndianBigEndian:=0;
  Bytes:=@Bytes^[3];
  InputLen:=aDataLength-3;
 end else if (aDataLength>=4) and
             (((Bytes^[0]=$00) and (Bytes^[1]=$00) and (Bytes^[2]=$fe) and (Bytes^[3]=$ff)) or
              ((Bytes^[0]=$ff) and (Bytes^[1]=$fe) and (Bytes^[2]=$00) and (Bytes^[3]=$00))) then begin
  // UTF32
  BytesPerCodeUnit:=4;
  BytesPerCodeUnitMask:=3;
  if Bytes^[0]=$00 then begin
   // Big endian
   LittleEndianBigEndian:=1;
  end else begin
   // Little endian
   LittleEndianBigEndian:=0;
  end;
  Bytes:=@Bytes^[4];
  InputLen:=aDataLength-4;
 end else if (aDataLength>=2) and
             (((Bytes^[0]=$fe) and (Bytes^[1]=$ff)) or
              ((Bytes^[0]=$ff) and (Bytes^[1]=$fe))) then begin
  // UTF16
  BytesPerCodeUnit:=2;
  BytesPerCodeUnitMask:=1;
  if Bytes^[0]=$fe then begin
   // Big endian
   LittleEndianBigEndian:=1;
  end else begin
   // Little endian
   LittleEndianBigEndian:=0;
  end;
  Bytes:=@Bytes^[2];
  InputLen:=aDataLength-2;
 end else begin
  // Latin1
  BytesPerCodeUnit:=0;
  BytesPerCodeUnitMask:=0;
  LittleEndianBigEndian:=0;
  Bytes:=@Bytes^[0];
  InputLen:=aDataLength;
 end;
 for PassIndex:=0 to 1 do begin
  CodeUnit:=0;
  OutputLen:=0;
  while (CodeUnit+BytesPerCodeUnitMask)<InputLen do begin
   case BytesPerCodeUnit of
    1:begin
     // UTF8
     CodePoint:=0;
     if (CodeUnit>=0) and (CodeUnit<InputLen) then begin
      StartCodeUnit:=CodeUnit;
      State:=TpvUTF8DFA.StateAccept;
      repeat
       Value:=ord(Bytes^[CodeUnit]);
       inc(CodeUnit);
       CharClass:=TpvUTF8DFA.StateCharClasses[AnsiChar(TpvUInt8(Value))];
       if State=TpvUTF8DFA.StateAccept then begin
        CodePoint:=Value and ($ff shr CharClass);
       end else begin
        CodePoint:=(CodePoint shl 6) or (Value and $3f);
       end;
       State:=TpvUTF8DFA.StateTransitions[State+CharClass];
      until (State<=TpvUTF8DFA.StateError) or (CodeUnit>=InputLen);
      if State<>TpvUTF8DFA.StateAccept then begin
       CodePoint:=ord(Bytes^[StartCodeUnit]);
       CodeUnit:=StartCodeUnit+1;
      end;
     end;
    end;
    2:begin
     // UTF16
     CodePoint:=(TpvUInt32(Bytes^[CodeUnit+0]) shl UTF16LittleEndianBigEndianShifts[LittleEndianBigEndian,0]) or
                (TpvUInt32(Bytes^[CodeUnit+1]) shl UTF16LittleEndianBigEndianShifts[LittleEndianBigEndian,1]);
     inc(CodeUnit,2);
     if ((CodeUnit+1)<InputLen) and ((CodePoint and $fc00)=$d800) then begin
      Temp:=(TpvUInt32(Bytes^[CodeUnit+0]) shl UTF16LittleEndianBigEndianShifts[LittleEndianBigEndian,0]) or
            (TpvUInt32(Bytes^[CodeUnit+1]) shl UTF16LittleEndianBigEndianShifts[LittleEndianBigEndian,1]);
      if (Temp and $fc00)=$dc00 then begin
       CodePoint:=(TpvUInt32(TpvUInt32(CodePoint and $3ff) shl 10) or TpvUInt32(Temp and $3ff))+$10000;
       inc(CodeUnit,2);
      end;
     end;
    end;
    4:begin
     // UTF32
     CodePoint:=(TpvUInt32(Bytes^[CodeUnit+0]) shl UTF32LittleEndianBigEndianShifts[LittleEndianBigEndian,0]) or
                (TpvUInt32(Bytes^[CodeUnit+1]) shl UTF32LittleEndianBigEndianShifts[LittleEndianBigEndian,1]) or
                (TpvUInt32(Bytes^[CodeUnit+2]) shl UTF32LittleEndianBigEndianShifts[LittleEndianBigEndian,2]) or
                (TpvUInt32(Bytes^[CodeUnit+3]) shl UTF32LittleEndianBigEndianShifts[LittleEndianBigEndian,3]);
     inc(CodeUnit,4);
    end;
    else begin
     // Latin1 or custom code page
     CodePoint:=Bytes^[CodeUnit];
     inc(CodeUnit);
     if assigned(CodePage) then begin
      CodePoint:=CodePage^[CodePoint and $ff];
     end;
    end;
   end;
   if PassIndex=0 then begin
    if CodePoint<=$7f then begin
     inc(OutputLen);
    end else if CodePoint<=$7ff then begin
     inc(OutputLen,2);
    end else if CodePoint<=$ffff then begin
     inc(OutputLen,3);
    end else if CodePoint<=$1fffff then begin
     inc(OutputLen,4);
    end else begin
     inc(OutputLen,3);
    end;
   end else begin
    if CodePoint<=$7f then begin
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8(CodePoint));
    end else if CodePoint<=$7ff then begin
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($c0 or ((CodePoint shr 6) and $1f)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or (CodePoint and $3f)));
    end else if CodePoint<=$d7ff then begin
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($e0 or ((CodePoint shr 12) and $0f)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or ((CodePoint shr 6) and $3f)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or (CodePoint and $3f)));
    end else if CodePoint<=$dfff then begin
     inc(OutputLen);
     result[OutputLen]:=#$ef; // $fffd
     inc(OutputLen);
     result[OutputLen]:=#$bf;
     inc(OutputLen);
     result[OutputLen]:=#$bd;
    end else if CodePoint<=$ffff then begin
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($e0 or ((CodePoint shr 12) and $0f)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or ((CodePoint shr 6) and $3f)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or (CodePoint and $3f)));
    end else if CodePoint<=$1fffff then begin
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($f0 or ((CodePoint shr 18) and $07)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or ((CodePoint shr 12) and $3f)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or ((CodePoint shr 6) and $3f)));
     inc(OutputLen);
     result[OutputLen]:=AnsiChar(TpvUInt8($80 or (CodePoint and $3f)));
    end else begin
     inc(OutputLen);
     result[OutputLen]:=#$ef; // $fffd
     inc(OutputLen);
     result[OutputLen]:=#$bf;
     inc(OutputLen);
     result[OutputLen]:=#$bd;
    end;
   end;
  end;
  if PassIndex=0 then begin
   SetLength(result,OutputLen);
  end;
 end;
end;

class function TpvUTF8Utils.RawByteStringToUTF8String(const aString:TpvRawByteString;const aCodePage:TpvInt32=-1):TpvUTF8String;
var p:PAnsiChar;
begin
 if length(aString)>0 then begin
  p:=PAnsiChar(@aString[1]);
  result:=RawDataToUTF8String(p^,length(aString),aCodePage);
 end else begin
  result:='';
 end;
end;

class function TpvUTF8Utils.RawStreamToUTF8String(const aStream:TStream;const aCodePage:TpvInt32=-1):TpvUTF8String;
var Memory:pointer;
    Size:TpvSizeInt;
begin
 result:='';
 if assigned(aStream) and (aStream.Seek(0,soBeginning)=0) then begin
  Size:=aStream.Size;
  GetMem(Memory,Size);
  try
   if aStream.Read(Memory^,Size)=Size then begin
    result:=PUCURawDataToUTF8String(Memory^,Size,aCodePage);
   end;
  finally
   FreeMem(Memory);
  end;
 end;
end;

constructor TpvUTF8StringRope.TNode.Create(const aHeight:TpvInt32);
begin
 inherited Create;
 fHeight:=aHeight;
 fCountCodeUnits:=0;
 SetLength(fLinks,MaximumHeight+1);
 fLinks[0].fNode:=nil;
 fLinks[0].fSkipSize:=0;
end;

destructor TpvUTF8StringRope.TNode.Destroy;
begin
 inherited Destroy;
end;

function TpvUTF8StringRope.TNode.GetData:TpvUTF8String;
begin
 SetString(result,PAnsiChar(@fData[0]),fCountCodeUnits);
end;

constructor TpvUTF8StringRope.TNodeEnumerator.Create(const aCodeUnitsRope:TpvUTF8StringRope);
begin
 fStringRope:=aCodeUnitsRope;
 fFirst:=true;
 fNode:=fStringRope.fHead;
end;

function TpvUTF8StringRope.TNodeEnumerator.GetCurrent:TNode;
begin
 result:=fNode;
end;

function TpvUTF8StringRope.TNodeEnumerator.MoveNext:boolean;
begin
 result:=assigned(fNode);
 if result then begin
  if fFirst then begin
   fFirst:=false;
  end else begin
   fNode:=fNode.fLinks[0].fNode;
   result:=assigned(fNode);
  end;
 end;
end;

constructor TpvUTF8StringRope.TCodePointEnumerator.Create(const aCodeUnitsRope:TpvUTF8StringRope;const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1);
begin
 fStringRope:=aCodeUnitsRope;
 fFirst:=true;
 fStringRope.GetNodeAndOffsetFromCodePointIndex(aStartCodePointIndex,fNode,fNodeCodeUnitIndex);
 fCodePointIndex:=aStartCodePointIndex;
 fStopCodePointIndex:=aStopCodePointIndex;
 fUTF8DFAState:=TpvUTF8DFA.StateAccept;
 fCodePoint:=0;
end;

function TpvUTF8StringRope.TCodePointEnumerator.GetCurrent:TpvUInt32;
begin
 result:=fCodePoint;
end;

function TpvUTF8StringRope.TCodePointEnumerator.MoveNext:boolean;
var CodeUnit:AnsiChar;
begin
 result:=false;
 if assigned(fNode) and
    ((fStopCodePointIndex<0) or
     (fCodePointIndex<fStopCodePointIndex)) then begin
  repeat
   if fNodeCodeUnitIndex>=fNode.fCountCodeUnits then begin
    fNode:=fNode.fLinks[0].fNode;
    fNodeCodeUnitIndex:=0;
    if assigned(fNode) then begin
     continue;
    end else begin
     break;
    end;
   end else begin
    CodeUnit:=fNode.fData[fNodeCodeUnitIndex];
    inc(fNodeCodeUnitIndex);
    fUTF8DFACharClass:=TpvUTF8DFA.StateCharClasses[CodeUnit];
    case fUTF8DFAState of
     TpvUTF8DFA.StateAccept..TpvUTF8DFA.StateError:begin
      fCodePoint:=ord(CodeUnit) and ($ff shr fUTF8DFACharClass);
     end;
     else begin
      fCodePoint:=(fCodePoint shl 6) or (ord(CodeUnit) and $3f);
     end;
    end;
    fUTF8DFAState:=TpvUTF8DFA.StateTransitions[fUTF8DFAState+fUTF8DFACharClass];
    if fUTF8DFAState<=TpvUTF8DFA.StateError then begin
     if fUTF8DFAState<>TpvUTF8DFA.StateAccept then begin
      fCodePoint:=$fffd;
     end;
     inc(fCodePointIndex);
     result:=true;
     break;
    end;
   end;
  until false;
 end;
end;

constructor TpvUTF8StringRope.TCodePointEnumeratorSource.Create(const aCodeUnitsRope:TpvUTF8StringRope;const aStartCodePointIndex,aStopCodePointIndex:TpvSizeInt);
begin
 fStringRope:=aCodeUnitsRope;
 fStartCodePointIndex:=aStartCodePointIndex;
 fStopCodePointIndex:=aStopCodePointIndex;
end;

function TpvUTF8StringRope.TCodePointEnumeratorSource.GetEnumerator:TpvUTF8StringRope.TCodePointEnumerator;
begin
 result:=TpvUTF8StringRope.TCodePointEnumerator.Create(fStringRope,fStartCodePointIndex,fStopCodePointIndex);
end;

constructor TpvUTF8StringRope.TRandomGenerator.Create(const aSeed:TpvUInt64);
begin
 fState:=TpVUInt64($853c49e6748fea9b);
 fIncrement:=TpVUInt64($da3e39cb94b95bdb);
 if aSeed<>0 then begin
  fIncrement:=((fIncrement xor aSeed) shl 1) or 1; // must be odd
  GetUInt32;
  inc(fState,{$ifdef fpc}RORQWord(aSeed,23){$else}(aSeed shr 23) or (aSeed shl 41){$endif});
  GetUInt32;
 end;
end;

function TpvUTF8StringRope.TRandomGenerator.GetUInt32:TpvUInt32;
var OldState:TpvUInt64;
{$ifndef fpc}
    XorShifted,Rotation:TpvUInt32;
{$endif}
begin
 OldState:=fState;
 fState:=(OldState*TpvUInt64(6364136223846793005))+fIncrement;
{$ifdef fpc}
 result:=RORDWord(TpvUInt32(((OldState shr 18) xor OldState) shr 27),OldState shr 59);
{$else}
 XorShifted:=((OldState shr 18) xor OldState) shr 27;
 Rotation:=OldState shr 59;
 result:=(XorShifted shr Rotation) or (XorShifted shl (32-Rotation));
{$endif}
end;

function TpvUTF8StringRope.TRandomGenerator.GetUInt64:TpvUInt64;
begin
 result:=(TpvUInt64(GetUInt32) shl 32) or
         (TpvUInt64(GetUInt32) shl 0);
end;

constructor TpvUTF8StringRope.Create;
begin
 inherited Create;
 fCountCodePoints:=0;
 fCountCodeUnits:=0;
 fHead:=TNode.Create(TNode.MaximumHeight);
 fHead.fHeight:=1;
 fRandomGenerator:=TRandomGenerator.Create(TpvPtrUInt(self));
end;

constructor TpvUTF8StringRope.Create(const aCodeUnits:TpvUTF8String);
begin
 Create;
 SetText(aCodeUnits);
end;

constructor TpvUTF8StringRope.Create(const aFrom:TpvUTF8StringRope);
begin
 Create(aFrom.GetText);
end;

destructor TpvUTF8StringRope.Destroy;
var Node,NextNode:TNode;
begin
 Node:=fHead.fLinks[0].fNode;
 while assigned(Node) do begin
  NextNode:=Node.fLinks[0].fNode;
  Node.Free;
  Node:=NextNode;
 end;
 fHead.Free;
 inherited Destroy;
end;

procedure TpvUTF8StringRope.Clear;
var Node,NextNode:TNode;
begin
 Node:=fHead.fLinks[0].fNode;
 while assigned(Node) do begin
  NextNode:=Node.fLinks[0].fNode;
  Node.Free;
  Node:=NextNode;
 end;
 fHead.Free;
 fCountCodePoints:=0;
 fCountCodeUnits:=0;
 fHead:=TNode.Create(TNode.MaximumHeight);
 fHead.fHeight:=1;
end;

function TpvUTF8StringRope.GetText:TpvUTF8String;
var Position:TpvSizeInt;
    Node:TNode;
begin
 SetLength(result,fCountCodeUnits);
 if fCountCodeUnits>0 then begin
  Position:=1;
  Node:=fHead;
  while assigned(Node) do begin
   Move(Node.fData[0],result[Position],Node.fCountCodeUnits);
   inc(Position,Node.fCountCodeUnits);
   Node:=Node.fLinks[0].fNode;
  end;
{$if defined(DebugTpvUTF8StringRope)}
  Assert(Position=(fCountCodeUnits+1));
{$ifend}
 end;
end;

procedure TpvUTF8StringRope.SetText(const aCodeUnits:TpvUTF8String);
begin
 Clear;
 Insert(0,aCodeUnits);
end;

{$ifdef fpc}
class function TpvUTF8StringRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32;
begin
 if aValue=0 then begin
  result:=255;
 end else begin
  result:=BSFQWord(aValue);
 end;
end;
{$else}
{$if defined(cpu386)}
class function TpvUTF8StringRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32; assembler; stdcall; {$ifdef fpc}nostackframe;{$endif}
asm
 bsf eax,dword ptr [aValue+0]
 jnz @Done
 bsf eax,dword ptr [aValue+4]
 jz @Fail
 add eax,32
 jmp @Done
@Fail:
 mov eax,255
@Done:
end;
{$elseif defined(cpuamd64) or defined(cpux64)}
class function TpvUTF8StringRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32; assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
{$ifndef fpc}
 .NOFRAME
{$endif}
{$if defined(Win32) or defined(Win64) or defined(Windows)}
 bsf rax,rcx
{$else}
 bsf rax,rdi
{$ifend}
 jnz @Done
 mov eax,255
@Done:
end;
{$else}
class function TpvUTF8StringRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32;
const DebruijnMultiplicator:TpvUInt64=TpvUInt64($03f79d71b4cb0a89);
      DebruijnShift=58;
      DebruijnMask=63;
      DebruijnTable:array[0..63] of TpvUInt32=(0,1,48,2,57,49,28,3,61,58,50,42,38,29,17,4,62,55,59,36,53,51,43,22,45,39,33,30,24,18,12,5,
                                               63,47,56,27,60,41,37,16,54,35,52,21,44,32,23,11,46,26,40,15,34,20,31,10,25,14,19,9,13,8,7,6);
begin
 if aValue=0 then begin
  result:=255;
 end else begin
  result:=DebruijnTable[(((aValue and not (aValue-1))*DebruijnMultiplicator) shr DebruijnShift) and DebruijnMask];
 end;
end;
{$ifend}
{$endif}

function TpvUTF8StringRope.GetRandomHeight:TpvInt32;
begin
 result:=FindFirstSetBit(not (fRandomGenerator.GetUInt64 and TNode.MaximumHeightMinusTwoBitMask))+1;
 if result>TNode.MaximumHeightMinusOne then begin
  result:=TNode.MaximumHeightMinusOne;
 end;
end;

class function TpvUTF8StringRope.GetCountCodeUnits(const aCodeUnits:PAnsiChar;const aCountCodePoints:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=0;
 Index:=0;
 while Index<aCountCodePoints do begin
  inc(result,TpvUTF8DFA.CodePointSizes[aCodeUnits[result]]);
  inc(Index);
 end;
end;

class function TpvUTF8StringRope.GetCountCodePoints(const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=0;
 Index:=0;
 while Index<aCountCodeUnits do begin
  inc(Index,TpvUTF8DFA.CodePointSizes[aCodeUnits[Index]]);
  inc(result);
 end;
end;

class procedure TpvUTF8StringRope.UTF8Check(const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt);
var Index:TpvSizeInt;
    State:TpvUInt32;
begin
 State:=TpvUTF8DFA.StateAccept;
 for Index:=0 to aCountCodeUnits-1 do begin
  State:=TpvUTF8DFA.StateTransitions[State+TpvUTF8DFA.StateCharClasses[aCodeUnits[Index]]];
  if State=TpvUTF8DFA.StateError then begin
   break;
  end;
 end;
 if State<>TpvUTF8DFA.StateAccept then begin
  raise EpvUTF8StringRope.Create('Invalid UTF8');
 end;
end;

function TpvUTF8StringRope.FindNodePositionAtCodePoint(const aCodePointIndex:TpvSizeInt;out aNodePositionLinks:TNode.TNodePositionLinks):TNode;
var Height:TpvInt32;
    Offset,Skip:TpvSizeInt;
begin
{$if defined(DebugTpvUTF8StringRope)}
 Assert(aCodePointIndex<=fCountCodePoints);
{$ifend}
 FillChar(aNodePositionLinks,SizeOf(TNode.TNodePositionLinks),#0);
 result:=fHead;
 Height:=result.fHeight-1;
 Offset:=aCodePointIndex;
 repeat
  Skip:=result.fLinks[Height].fSkipSize;
  if Skip<Offset then begin
{$if defined(DebugTpvUTF8StringRope)}
   Assert((result=fHead) or (result.fCountCodeUnits>0));
{$ifend}
   dec(Offset,Skip);
   result:=result.fLinks[Height].fNode;
  end else begin
   aNodePositionLinks[Height].fSkipSize:=Offset;
   aNodePositionLinks[Height].fNode:=result;
   if Height=0 then begin
    break;
   end else begin
    dec(Height);
   end;
  end;
 until false;
{$if defined(DebugTpvUTF8StringRope)}
 Assert(Offset<=TNode.StringSize);
 Assert(aNodePositionLinks[0].fNode=result);
{$ifend}
end;

function TpvUTF8StringRope.GetNodeAndOffsetFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aNode:TNode;out aNodeCodeUnitIndex:TpvSizeInt):boolean;
var NodePositionLinks:TpvUTF8StringRope.TNode.TNodePositionLinks;
begin
 if (aCodePointIndex>=0) and
    (aCodePointIndex<fCountCodePoints) then begin
  if aCodePointIndex=0 then begin
   aNode:=fHead;
   aNodeCodeUnitIndex:=0;
   result:=true;
  end else begin
   aNode:=FindNodePositionAtCodePoint(aCodePointIndex,NodePositionLinks);
   if assigned(aNode) then begin
    aNodeCodeUnitIndex:=TpvUTF8StringRope.GetCountCodeUnits(@aNode.fData[0],NodePositionLinks[0].fSkipSize);
    result:=true;
   end else begin
    aNodeCodeUnitIndex:=0;
    result:=false;
   end;
  end;
 end else begin
  aNode:=nil;
  aNodeCodeUnitIndex:=0;
  result:=false;
 end;
end;

procedure TpvUTF8StringRope.UpdateOffsetList(var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
var Index:TpvInt32;
begin
 for Index:=0 to fHead.fHeight-1 do begin
  inc(aNodePositionLinks[Index].fNode.fLinks[Index].fSkipSize,aCountCodePoints);
 end;
end;

procedure TpvUTF8StringRope.InsertAt(var aNodePositionLinks:TNode.TNodePositionLinks;const aCodeUnits:PAnsiChar;const aCountCodeUnits,aCountCodePoints:TpvSizeInt);
var MaximumHeight,NewHeight,Index:TpvInt32;
    NewNode:TNode;
    PreviousNodeLink:TNode.PNodeLink;
begin

 MaximumHeight:=fHead.fHeight;

 NewHeight:=GetRandomHeight;

 NewNode:=TNode.Create(NewHeight);
 NewNode.fCountCodeUnits:=aCountCodeUnits;
 Move(aCodeUnits[0],NewNode.fData[0],aCountCodeUnits);

{$if defined(DebugTpvUTF8StringRope)}
 Assert(NewHeight<TNode.MaximumHeight);
{$ifend}

 while MaximumHeight<=NewHeight do begin
  inc(fHead.fHeight);
  fHead.fLinks[MaximumHeight]:=fHead.fLinks[MaximumHeight-1];
  aNodePositionLinks[MaximumHeight]:=aNodePositionLinks[MaximumHeight-1];
  inc(MaximumHeight);
 end;

 for Index:=0 to NewHeight-1 do begin
  PreviousNodeLink:=@aNodePositionLinks[Index].fNode.fLinks[Index];
  NewNode.fLinks[Index].fNode:=PreviousNodeLink^.fNode;
  NewNode.fLinks[Index].fSkipSize:=(aCountCodePoints+PreviousNodeLink^.fSkipSize)-aNodePositionLinks[Index].fSkipSize;
  PreviousNodeLink^.fNode:=NewNode;
  PreviousNodeLink^.fSkipSize:=aNodePositionLinks[Index].fSkipSize;
  aNodePositionLinks[Index].fNode:=NewNode;
  aNodePositionLinks[Index].fSkipSize:=aCountCodePoints;
 end;

 for Index:=NewHeight to MaximumHeight-1 do begin
  inc(aNodePositionLinks[Index].fNode.fLinks[Index].fSkipSize,aCountCodePoints);
  inc(aNodePositionLinks[Index].fSkipSize,aCountCodePoints);
 end;

 inc(fCountCodeUnits,aCountCodeUnits);
 inc(fCountCodePoints,aCountCodePoints);

end;

procedure TpvUTF8StringRope.InsertAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt);
var OffsetCodeUnits,Offset,CountInsertedCodePoints,CountEndCodeUnits,
    CountEndCodePoints,StringOffset,CountNewNodeCodeUnits,CountNewNodeCodePoints,
    CodePointSize,CountInsertedCodeUnits:TpvSizeInt;
    InsertHere:boolean;
    Node,NextNode:TNode;
    Index:TpvInt32;
begin

 Offset:=aNodePositionLinks[0].fSkipSize;

 if Offset<>0 then begin
{$if defined(DebugTpvUTF8StringRope)}
  Assert(Offset<=aNode.fLinks[0].fSkipSize);
{$ifend}
  OffsetCodeUnits:=GetCountCodeUnits(@aNode.fData[0],Offset);
 end else begin
  OffsetCodeUnits:=0;
 end;

 CountInsertedCodeUnits:=aCountCodeUnits;

 UTF8Check(aCodeUnits,CountInsertedCodeUnits);

 InsertHere:=(aNode.fCountCodeUnits+CountInsertedCodeUnits)<=TNode.StringSize;

 Node:=aNode;

 if (OffsetCodeUnits=Node.fCountCodeUnits) and not InsertHere then begin
  NextNode:=Node.fLinks[0].fNode;
  if assigned(NextNode) and ((NextNode.fCountCodeUnits+CountInsertedCodeUnits)<=TNode.StringSize) then begin
   Offset:=0;
   OffsetCodeUnits:=0;
   for Index:=0 to NextNode.fHeight-1 do begin
    aNodePositionLinks[Index].fNode:=NextNode;
   end;
   Node:=NextNode;
   InsertHere:=true;
  end;
 end;

 if InsertHere then begin

  if OffsetCodeUnits<Node.fCountCodeUnits then begin
   Move(Node.fData[OffsetCodeUnits],
        Node.fData[OffsetCodeUnits+CountInsertedCodeUnits],
        Node.fCountCodeUnits-OffsetCodeUnits);
  end;

  Move(aCodeUnits[0],
       Node.fData[OffsetCodeUnits],
       CountInsertedCodeUnits);
  inc(Node.fCountCodeUnits,CountInsertedCodeUnits);

  inc(fCountCodeUnits,CountInsertedCodeUnits);

  CountInsertedCodePoints:=GetCountCodePoints(@aCodeUnits[0],aCountCodeUnits);

  inc(fCountCodePoints,CountInsertedCodePoints);

  UpdateOffsetList(aNodePositionLinks,CountInsertedCodePoints);

 end else begin

  CountEndCodeUnits:=Node.fCountCodeUnits-OffsetCodeUnits;

  if CountEndCodeUnits<>0 then begin
   Node.fCountCodeUnits:=OffsetCodeUnits;
   CountEndCodePoints:=Node.fLinks[0].fSkipSize-Offset;
   UpdateOffsetList(aNodePositionLinks,-CountEndCodePoints);
   dec(fCountCodePoints,CountEndCodePoints);
   dec(fCountCodeUnits,CountEndCodeUnits);
  end else begin
   CountEndCodePoints:=0;
  end;

  StringOffset:=0;
  while StringOffset<CountInsertedCodeUnits do begin
   CountNewNodeCodeUnits:=0;
   CountNewNodeCodePoints:=0;
   while (StringOffset+CountNewNodeCodeUnits)<CountInsertedCodeUnits do begin
    CodePointSize:=TpvUTF8DFA.CodePointSizes[aCodeUnits[StringOffset+CountNewNodeCodeUnits]];
    if (CodePointSize+CountNewNodeCodeUnits)<=TNode.StringSize then begin
     inc(CountNewNodeCodeUnits,CodePointSize);
     inc(CountNewNodeCodePoints);
    end else begin
     break;
    end;
   end;
   InsertAt(aNodePositionLinks,@aCodeUnits[StringOffset],CountNewNodeCodeUnits,CountNewNodeCodePoints);
   inc(StringOffset,CountNewNodeCodeUnits);
  end;

  if CountEndCodeUnits>0 then begin
   InsertAt(aNodePositionLinks,@Node.fData[OffsetCodeUnits],CountEndCodeUnits,CountEndCodePoints);
  end;

 end;

end;

procedure TpvUTF8StringRope.DeleteAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
var Offset,RemainingCodePoints,CodePointsToDo,CodePointsRemoved,
    LeadingCodeUnits,RemovedCodeUnits,TrailingCodeUnits:TpvSizeInt;
    Node,NextNode:TNode;
    Index:TpvInt32;
begin
 dec(fCountCodePoints,aCountCodePoints);
 Offset:=aNodePositionLinks[0].fSkipSize;
 RemainingCodePoints:=aCountCodePoints;
 Node:=aNode;
 while RemainingCodePoints>0 do begin
  if Offset=Node.fLinks[0].fSkipSize then begin
   Node:=aNodePositionLinks[0].fNode.fLinks[0].fNode;
   Offset:=0;
  end;
  CodePointsToDo:=Node.fLinks[0].fSkipSize;
  CodePointsRemoved:=CodePointsToDo-Offset;
  if CodePointsRemoved>RemainingCodePoints then begin
   CodePointsRemoved:=RemainingCodePoints;
  end;
  if (CodePointsRemoved<CodePointsToDo) or (Node=fHead) then begin
   LeadingCodeUnits:=GetCountCodeUnits(@Node.fData[0],Offset);
   RemovedCodeUnits:=GetCountCodeUnits(@Node.fData[LeadingCodeUnits],CodePointsRemoved);
   TrailingCodeUnits:=Node.fCountCodeUnits-(LeadingCodeUnits+RemovedCodeUnits);
   if TrailingCodeUnits>0 then begin
    Move(Node.fData[LeadingCodeUnits+RemovedCodeUnits],
         Node.fData[LeadingCodeUnits],
         TrailingCodeUnits);
   end;
   dec(Node.fCountCodeUnits,RemovedCodeUnits);
   dec(fCountCodeUnits,RemovedCodeUnits);
   Index:=0;
   while Index<Node.fHeight do begin
    dec(Node.fLinks[Index].fSkipSize,CodePointsRemoved);
    inc(Index);
   end;
  end else begin
   Index:=0;
   while Index<Node.fHeight do begin
    aNodePositionLinks[Index].fNode.fLinks[Index].fNode:=Node.fLinks[Index].fNode;
    inc(aNodePositionLinks[Index].fNode.fLinks[Index].fSkipSize,Node.fLinks[Index].fSkipSize-CodePointsRemoved);
    inc(Index);
   end;
   dec(fCountCodeUnits,Node.fCountCodeUnits);
   NextNode:=Node.fLinks[0].fNode;
   Node.Free;
   Node:=NextNode;
  end;
  while Index<fHead.fHeight do begin
   dec(aNodePositionLinks[Index].fNode.fLinks[Index].fSkipSize,CodePointsRemoved);
   inc(Index);
  end;
  dec(RemainingCodePoints,CodePointsRemoved);
 end;
end;

function TpvUTF8StringRope.ExtractAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt):TpvUTF8String;
var Offset,RemainingCodePoints,CodePointsToDo,CodePointsExtracted,
    LeadingCodeUnits,ExtractedCodeUnits:TpvSizeInt;
    Node:TNode;
    TemporaryString:TpvUTF8String;
begin
 result:='';
 Offset:=aNodePositionLinks[0].fSkipSize;
 RemainingCodePoints:=aCountCodePoints;
 Node:=aNode;
 while RemainingCodePoints>0 do begin
  if Offset=Node.fLinks[0].fSkipSize then begin
   Node:=aNodePositionLinks[0].fNode.fLinks[0].fNode;
   Offset:=0;
  end;
  CodePointsToDo:=Node.fLinks[0].fSkipSize;
  CodePointsExtracted:=CodePointsToDo-Offset;
  if CodePointsExtracted>RemainingCodePoints then begin
   CodePointsExtracted:=RemainingCodePoints;
  end;
  if (CodePointsExtracted<CodePointsToDo) or (Node=fHead) then begin
   LeadingCodeUnits:=GetCountCodeUnits(@Node.fData[0],Offset);
   ExtractedCodeUnits:=GetCountCodeUnits(@Node.fData[LeadingCodeUnits],CodePointsExtracted);
   SetString(TemporaryString,PAnsiChar(@Node.fData[LeadingCodeUnits]),ExtractedCodeUnits);
  end else begin
   SetString(TemporaryString,PAnsiChar(@Node.fData[0]),Node.fCountCodeUnits);
  end;
  result:=result+TemporaryString;
  Offset:=0;
  Node:=Node.fLinks[0].fNode;
  dec(RemainingCodePoints,CodePointsExtracted);
 end;
end;

procedure TpvUTF8StringRope.Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt);
var Node:TNode;
    NodePositionLinks:TNode.TNodePositionLinks;
    CodePointIndex:TpvSizeInt;
begin
{$if defined(DebugTpvUTF8StringRope)}
 Check;
{$ifend}
 if aCodePointIndex<fCountCodePoints then begin
  CodePointIndex:=aCodePointIndex;
 end else begin
  CodePointIndex:=fCountCodePoints;
 end;
 Node:=FindNodePositionAtCodePoint(CodePointIndex,NodePositionLinks);
 InsertAtNodePosition(Node,NodePositionLinks,aCodeUnits,aCountCodeUnits);
{$if defined(DebugTpvUTF8StringRope)}
 Check;
{$ifend}
end;

procedure TpvUTF8StringRope.Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:TpvUTF8String);
begin
 Insert(aCodePointIndex,PAnsiChar(aCodeUnits),length(aCodeUnits));
end;

procedure TpvUTF8StringRope.Delete(const aCodePointIndex,aCountCodePoints:TpvSizeInt);
var Node:TNode;
    NodePositionLinks:TNode.TNodePositionLinks;
    CodePointIndex,CountCodePoints:TpvSizeInt;
begin
{$if defined(DebugTpvUTF8StringRope)}
 Check;
{$ifend}
 if aCodePointIndex<fCountCodePoints then begin
  CodePointIndex:=aCodePointIndex;
 end else begin
  CodePointIndex:=fCountCodePoints;
 end;
 CountCodePoints:=fCountCodePoints-CodePointIndex;
 if CountCodePoints>aCountCodePoints then begin
  CountCodePoints:=aCountCodePoints;
 end;
 Node:=FindNodePositionAtCodePoint(CodePointIndex,NodePositionLinks);
 DeleteAtNodePosition(Node,NodePositionLinks,CountCodePoints);
{$if defined(DebugTpvUTF8StringRope)}
 Check;
{$ifend}
end;

function TpvUTF8StringRope.Extract(const aCodePointIndex,aCountCodePoints:TpvSizeInt):TpvUTF8String;
var Node:TNode;
    NodePositionLinks:TNode.TNodePositionLinks;
    CodePointIndex,CountCodePoints:TpvSizeInt;
begin
{$if defined(DebugTpvUTF8StringRope)}
 Check;
{$ifend}
 if aCodePointIndex<fCountCodePoints then begin
  CodePointIndex:=aCodePointIndex;
 end else begin
  CodePointIndex:=fCountCodePoints;
 end;
 CountCodePoints:=fCountCodePoints-CodePointIndex;
 if CountCodePoints>aCountCodePoints then begin
  CountCodePoints:=aCountCodePoints;
 end;
 Node:=FindNodePositionAtCodePoint(CodePointIndex,NodePositionLinks);
 result:=ExtractAtNodePosition(Node,NodePositionLinks,CountCodePoints);
end;

function TpvUTF8StringRope.GetCodePoint(const aCodePointIndex:TpvSizeInt):TpvUInt32;
var Node:TNode;
    NodePositionLinks:TNode.TNodePositionLinks;
    NodeCodeUnitIndex,CodePointIndex:TpvSizeInt;
    CodeUnit:AnsiChar;
    First:boolean;
    UTF8DFAState,UTF8DFACharClass:TpvUInt8;
begin
{$if defined(DebugTpvUTF8StringRope)}
 Check;
{$ifend}
 result:=32;
 if aCodePointIndex<fCountCodePoints then begin
  CodePointIndex:=aCodePointIndex;
 end else begin
  CodePointIndex:=fCountCodePoints;
 end;
 Node:=FindNodePositionAtCodePoint(CodePointIndex,NodePositionLinks);
 if assigned(Node) then begin
  NodeCodeUnitIndex:=GetCountCodeUnits(@Node.fData[0],NodePositionLinks[0].fSkipSize);
  UTF8DFAState:=TpvUTF8DFA.StateAccept;
  First:=true;
  repeat
   if NodeCodeUnitIndex>=Node.fCountCodeUnits then begin
    Node:=Node.fLinks[0].fNode;
    NodeCodeUnitIndex:=0;
    if assigned(Node) then begin
     continue;
    end else begin
     break;
    end;
   end else begin
    CodeUnit:=Node.fData[NodeCodeUnitIndex];
    inc(NodeCodeUnitIndex);
    UTF8DFACharClass:=TpvUTF8DFA.StateCharClasses[CodeUnit];
    case UTF8DFAState of
     TpvUTF8DFA.StateAccept..TpvUTF8DFA.StateError:begin
      if First then begin
       First:=false;
       result:=ord(CodeUnit) and ($ff shr UTF8DFACharClass);
      end else begin
       break;
      end;
     end;
     else begin
      result:=(result shl 6) or (ord(CodeUnit) and $3f);
     end;
    end;
    UTF8DFAState:=TpvUTF8DFA.StateTransitions[UTF8DFAState+UTF8DFACharClass];
   end;
  until false;
  if UTF8DFAState<>TpvUTF8DFA.StateAccept then begin
   result:=$fffd;
  end;
 end else begin
  result:=32;
 end;
end;

function TpvUTF8StringRope.GetEnumerator:TNodeEnumerator;
begin
 result:=TNodeEnumerator.Create(self);
end;

function TpvUTF8StringRope.GetCodePointEnumeratorSource(const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1):TpvUTF8StringRope.TCodePointEnumeratorSource;
begin
 result:=TpvUTF8StringRope.TCodePointEnumeratorSource.Create(self,aStartCodePointIndex,aStopCodePointIndex);
end;

procedure TpvUTF8StringRope.Check;
{$if defined(DebugTpvUTF8StringRope)}
var Index:TpvInt32;
    Node:TNode;
    CurrentCountCodeUnits,CurrentCountCodePoints:TpvSizeInt;
    SkipOverLink:TNode.PNodeLink;
    NodePositionLinks:TNode.TNodePositionLinks;
begin
 Assert(fHead.fHeight>0);
 Assert(fCountCodeUnits>=fCountCodePoints);

 SkipOverLink:=@fHead.fLinks[fHead.fHeight-1];
 Assert(SkipOverLink^.fSkipSize=fCountCodePoints);
 Assert(not assigned(SkipOverLink^.fNode));

 FillChar(NodePositionLinks,SizeOf(TNode.TNodePositionLinks),#0);
 for Index:=0 to fHead.fHeight-1 do begin
  NodePositionLinks[Index].fNode:=fHead;
 end;

 CurrentCountCodeUnits:=0;
 CurrentCountCodePoints:=0;

 Node:=fHead;
 while assigned(Node) do begin
  Assert((Node=fHead) or (Node.fCountCodeUnits>0));
  Assert(Node.fHeight<=TNode.MaximumHeight);
  Assert(GetCountCodeUnits(@Node.fData[0],Node.fLinks[0].fSkipSize)=Node.fCountCodeUnits);
  for Index:=0 to Node.fHeight-1 do begin
   Assert(NodePositionLinks[Index].fNode=Node);
   Assert(NodePositionLinks[Index].fSkipSize=CurrentCountCodePoints);
   NodePositionLinks[Index].fNode:=Node.fLinks[Index].fNode;
   inc(NodePositionLinks[Index].fSkipSize,Node.fLinks[Index].fSkipSize);
  end;
  inc(CurrentCountCodeUnits,Node.fCountCodeUnits);
  inc(CurrentCountCodePoints,Node.fLinks[0].fSkipSize);
  Node:=Node.fLinks[0].fNode;
 end;

 for Index:=0 to fHead.fHeight-1 do begin
  Assert(not assigned(NodePositionLinks[Index].fNode));
  Assert(NodePositionLinks[Index].fSkipSize=CurrentCountCodePoints);
 end;

 Assert(fCountCodeUnits=CurrentCountCodeUnits);
 Assert(fCountCodePoints=CurrentCountCodePoints);

end;
{$else}
begin
end;
{$ifend}

procedure TpvUTF8StringRope.Dump;
var Index,Counter:TpvInt32;
    Node:TNode;
begin
 WriteLn('Code points: ',fCountCodePoints, '    Code units: ',fCountCodeUnits,'    Height: ',fHead.fHeight);

 Write('HEAD');
 for Index:=0 to fHead.fHeight-1 do begin
  Write(' |',fHead.fLinks[Index].fSkipSize:3);
 end;
 WriteLn;

 Counter:=0;
 Node:=fHead;
 while assigned(Node) do begin
  Write(Counter:3,':');
  for Index:=0 to Node.fHeight-1 do begin
   Write(' |',Node.fLinks[Index].fSkipSize:3);
  end;
  WriteLn(' ':8,': "',Node.Data,'" (',Node.fCountCodeUnits,')');
  Node:=Node.fLinks[0].fNode;
  inc(Counter);
 end;

 WriteLn;

end;

constructor TpvUTF8StringRopeLineMap.Create(const aRope:TpvUTF8StringRope);
begin
 inherited Create;
 fStringRope:=aRope;
 fLines:=nil;
 fCountLines:=0;
 fLineWrap:=0;
 fTabWidth:=8;
 Reset;
 Update(-1,-1);
end;

destructor TpvUTF8StringRopeLineMap.Destroy;
begin
 fLines:=nil;
 inherited Destroy;
end;

procedure TpvUTF8StringRopeLineMap.SetLineWrap(const aLineWrap:TpvSizeInt);
begin
 if fLineWrap<>aLineWrap then begin
  fLineWrap:=aLineWrap;
  Reset;
  Update(-1,-1);
 end;
end;

procedure TpvUTF8StringRopeLineMap.SetTabWidth(const aTabWidth:TpvSizeInt);
begin
 if fTabWidth<>aTabWidth then begin
  fTabWidth:=aTabWidth;
  if fLineWrap>0 then begin
   Reset;
   Update(-1,-1);
  end;
 end;
end;

procedure TpvUTF8StringRopeLineMap.AddLine(const aCodePointIndex:TpvSizeInt);
begin
 if length(fLines)<(fCountLines+1) then begin
  SetLength(fLines,(fCountLines+1)*2);
 end;
 fLines[fCountLines]:=aCodePointIndex;
 inc(fCountLines);
end;

procedure TpvUTF8StringRopeLineMap.Reset;
begin
 fCountLines:=0;
 AddLine(0);
 fCodePointIndex:=0;
 fCountVisibleVisualCodePointsSinceNewLine:=0;
 fLastWasPossibleNewLineTwoCharSequence:=false;
 fLastCodePoint:=0;
end;

procedure TpvUTF8StringRopeLineMap.Truncate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var UntilCodePointCountLines,UntilLineCountLines,NewCountLines,LineIndex:TpvSizeInt;
begin

 if aUntilCodePoint>=0 then begin
  if aUntilCodePoint>0 then begin
   LineIndex:=GetLineIndexFromCodePointIndex(aUntilCodePoint-1);
   if (LineIndex>0) and (fCountLines>(LineIndex-1)) then begin
    UntilCodePointCountLines:=LineIndex-1;
    while (UntilCodePointCountLines>0) and
          (fLines[UntilCodePointCountLines-1]>=aUntilCodePoint) do begin
     dec(UntilCodePointCountLines);
    end;
   end else begin
    UntilCodePointCountLines:=0;
   end;
  end else begin
   UntilCodePointCountLines:=0;
  end;
 end else begin
  UntilCodePointCountLines:=fCountLines;
 end;

 if (aUntilLine>=0) and
    (fCountLines>aUntilLine) then begin
  UntilLineCountLines:=aUntilLine;
 end else begin
  UntilLineCountLines:=fCountLines;
 end;

 if UntilCodePointCountLines<UntilLineCountLines then begin
  NewCountLines:=UntilCodePointCountLines;
 end else begin
  NewCountLines:=UntilLineCountLines;
 end;

 if fCountLines<>NewCountLines then begin
  while (NewCountLines>0) and ((NewCountLines+1)>=fCountLines) do begin
   dec(NewCountLines);
  end;
  if fCountLines<>NewCountLines then begin
   if (NewCountLines>0) and ((NewCountLines+1)<fCountLines) then begin
    fCodePointIndex:=fLines[NewCountLines-1];
    fCountLines:=NewCountLines;
    fCountVisibleVisualCodePointsSinceNewLine:=0;
    fLastWasPossibleNewLineTwoCharSequence:=false;
    fLastCodePoint:=0;
   end else begin
    Reset;
   end;
  end;
 end;

end;

procedure TpvUTF8StringRopeLineMap.Update(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var CodePoint:TpvUInt32;
    DoStop:TpvInt32;
    DoNewLine,DoTab:boolean;
begin

 if (fCodePointIndex<fStringRope.fCountCodePoints) and
    ((aUntilCodePoint<0) or (fCodePointIndex<aUntilCodePoint)) and
    ((aUntilLine<0) or (fCountLines<aUntilLine)) then begin

  DoStop:=0;

  for CodePoint in fStringRope.GetCodePointEnumeratorSource(fCodePointIndex,-1) do begin

   inc(fCodePointIndex);

   DoTab:=false;

   DoNewLine:=false;

   case CodePoint of
    $09:begin
     DoTab:=true;
     fLastWasPossibleNewLineTwoCharSequence:=false;
    end;
    $0a,$0d:begin
     if fLastWasPossibleNewLineTwoCharSequence and
        (((CodePoint=$0a) and (fLastCodePoint=$0d)) or
         ((CodePoint=$0d) and (fLastCodePoint=$0a))) then begin
      if fCountLines>0 then begin
       fLines[fCountLines-1]:=fCodePointIndex;
      end;
      fLastWasPossibleNewLineTwoCharSequence:=false;
     end else begin
      DoNewLine:=true;
      fLastWasPossibleNewLineTwoCharSequence:=true;
     end;
    end;
    else begin
     fLastWasPossibleNewLineTwoCharSequence:=false;
    end;
   end;

   if fLineWrap>0 then begin
    if (CodePoint<>10) and (CodePoint<>13) then begin
     if DoTab and (fTabWidth>0) then begin
      inc(fCountVisibleVisualCodePointsSinceNewLine,fTabWidth-(fCountVisibleVisualCodePointsSinceNewLine mod fTabWidth));
     end else begin
      inc(fCountVisibleVisualCodePointsSinceNewLine);
     end;
    end;
    if fCountVisibleVisualCodePointsSinceNewLine>=fLineWrap then begin
     fCountVisibleVisualCodePointsSinceNewLine:=0;
     DoNewLine:=true;
    end;
   end;

   if DoNewLine then begin
    AddLine(fCodePointIndex);
    fCountVisibleVisualCodePointsSinceNewLine:=0;
    if ((aUntilCodePoint>=0) and (fCodePointIndex>=aUntilCodePoint)) or
       ((aUntilLine>=0) and (fCountLines>=aUntilLine)) then begin
     DoStop:=2; // for as fallback for possible two-single-char-class-codepoint-width-sized newline sequences
    end;
   end;

   fLastCodePoint:=CodePoint;

   if DoStop>0 then begin
    dec(DoStop);
    if DoStop=0 then begin
     break;
    end;
   end;

  end;

 end;

end;

function TpvUTF8StringRopeLineMap.GetLineIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt):TpvSizeInt;
var MinIndex,MaxIndex,MidIndex:TpvSizeInt;
begin
 if aCodePointIndex<=fStringRope.CountCodePoints then begin
  Update(aCodePointIndex+1,-1);
  MinIndex:=0;
  MaxIndex:=fCountLines-1;
  while MinIndex<MaxIndex do begin
   MidIndex:=MinIndex+((MaxIndex-MinIndex) shr 1);
   if aCodePointIndex<fLines[MidIndex] then begin
    MaxIndex:=MidIndex-1;
   end else if aCodePointIndex>=fLines[MidIndex+1] then begin
    MinIndex:=MidIndex+1;
   end else begin
    MinIndex:=MidIndex;
    break;
   end;
  end;
  result:=MinIndex;
 end else begin
  result:=-1;
 end;
end;

function TpvUTF8StringRopeLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aLineIndex,aColumnIndex:TpvSizeInt):boolean;
var StartCodePointIndex,StopCodePointIndex,CurrentCodePointIndex,
    StepWidth,CurrentColumn:TpvSizeInt;
    CodePoint,LastCodePoint:TpvUInt32;
    LastWasPossibleNewLineTwoCharSequence:boolean;
begin

 result:=false;

 Update(aCodePointIndex+1,-1);

 aLineIndex:=GetLineIndexFromCodePointIndex(aCodePointIndex);

 if aLineIndex<0 then begin

  aColumnIndex:=-1;

 end else begin

  StartCodePointIndex:=GetCodePointIndexFromLineIndex(aLineIndex);

  if StartCodePointIndex<0 then begin

   aLineIndex:=-1;
   aColumnIndex:=-1;

  end else begin

   StopCodePointIndex:=GetCodePointIndexFromNextLineIndexOrTextEnd(aLineIndex);

   if StartCodePointIndex<StopCodePointIndex then begin

    CodePoint:=0;

    CurrentColumn:=0;

    CurrentCodePointIndex:=StartCodePointIndex;

    LastCodePoint:=0;

    LastWasPossibleNewLineTwoCharSequence:=false;

    for CodePoint in fStringRope.GetCodePointEnumeratorSource(StartCodePointIndex,StopCodePointIndex) do begin

     StepWidth:=1;

     case CodePoint of
      9:begin
       StepWidth:=Max(1,(fTabWidth-(CurrentColumn mod fTabWidth)));
       LastWasPossibleNewLineTwoCharSequence:=false;
      end;
      $0a,$0d:begin
       if LastWasPossibleNewLineTwoCharSequence and
          (((CodePoint=$0a) and (LastCodePoint=$0d)) or
           ((CodePoint=$0d) and (LastCodePoint=$0a))) then begin
        StepWidth:=0;
        LastWasPossibleNewLineTwoCharSequence:=false;
       end else begin
        LastWasPossibleNewLineTwoCharSequence:=true;
       end;
      end;
      else begin
       LastWasPossibleNewLineTwoCharSequence:=false;
      end;
     end;

     LastCodePoint:=CodePoint;

     aColumnIndex:=CurrentColumn;

     if LastWasPossibleNewLineTwoCharSequence or
        (CurrentCodePointIndex>=aCodePointIndex) then begin
      break;
     end;

     inc(CurrentColumn,StepWidth);

     inc(CurrentCodePointIndex);

    end;

    if CurrentCodePointIndex=fStringRope.CountCodePoints then begin
     inc(aColumnIndex);
    end;

   end else begin

    aColumnIndex:=0;

   end;

   result:=true;

  end;

 end;

end;

function TpvUTF8StringRopeLineMap.GetCodePointIndexFromLineIndex(const aLineIndex:TpvSizeInt):TpvSizeInt;
begin
 Update(-1,aLineIndex+1);
 if (aLineIndex>=0) and (aLineIndex<fCountLines) then begin
  result:=fLines[aLineIndex];
 end else begin
  result:=-1;
 end;
end;

function TpvUTF8StringRopeLineMap.GetCodePointIndexFromNextLineIndexOrTextEnd(const aLineIndex:TpvSizeInt):TpvSizeInt;
begin
 Update(-1,aLineIndex+2);
 if (aLineIndex>=0) and (aLineIndex<fCountLines) then begin
  if (aLineIndex+1)<fCountLines then begin
   result:=fLines[aLineIndex+1];
  end else begin
   result:=fStringRope.CountCodePoints;
  end;
 end else begin
  result:=-1;
 end;
end;

function TpvUTF8StringRopeLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(const aLineIndex,aColumnIndex:TpvSizeInt):TpvSizeInt;
var StartCodePointIndex,StopCodePointIndex,CurrentCodePointIndex,
    StepWidth,CurrentColumn:TpvSizeInt;
    CodePoint,LastCodePoint:TpvUInt32;
    LastWasPossibleNewLineTwoCharSequence:boolean;
begin

 Update(-1,aLineIndex+2);

 if (aLineIndex>=0) and (aLineIndex<fCountLines) then begin

  result:=fLines[aLineIndex];

  StartCodePointIndex:=result;

  if (aLineIndex+1)<fCountLines then begin
   StopCodePointIndex:=fLines[aLineIndex+1];
  end else begin
   StopCodePointIndex:=fStringRope.CountCodePoints;
  end;

  CurrentColumn:=0;

  CurrentCodePointIndex:=StartCodePointIndex;

  LastCodePoint:=0;

  LastWasPossibleNewLineTwoCharSequence:=false;

  if StartCodePointIndex<StopCodePointIndex then begin

   for CodePoint in fStringRope.GetCodePointEnumeratorSource(StartCodePointIndex,StopCodePointIndex) do begin

    StepWidth:=1;

    case CodePoint of
     9:begin
      StepWidth:=Max(1,(fTabWidth-(CurrentColumn mod fTabWidth)));
      LastWasPossibleNewLineTwoCharSequence:=false;
     end;
     $0a,$0d:begin
      if LastWasPossibleNewLineTwoCharSequence and
         (((CodePoint=$0a) and (LastCodePoint=$0d)) or
          ((CodePoint=$0d) and (LastCodePoint=$0a))) then begin
       StepWidth:=0;
       LastWasPossibleNewLineTwoCharSequence:=false;
      end else begin
       LastWasPossibleNewLineTwoCharSequence:=true;
      end;
     end;
     else begin
      LastWasPossibleNewLineTwoCharSequence:=false;
     end;
    end;

    LastCodePoint:=CodePoint;

    result:=CurrentCodePointIndex;

    if LastWasPossibleNewLineTwoCharSequence or
       (CurrentColumn>=aColumnIndex) then begin
     break;
    end;

    inc(CurrentColumn,StepWidth);

    inc(CurrentCodePointIndex);

   end;

   if (CurrentColumn<=aColumnIndex) and (CurrentCodePointIndex=fStringRope.CountCodePoints) then begin
    result:=fStringRope.CountCodePoints;
   end;

  end;

 end else begin

  result:=-1;

 end;

end;

constructor TpvTextEditor.TUndoRedoCommand.Create(const aParent:TpvTextEditor;const aCursorCodePointIndex:TpvSizeInt);
begin
 inherited Create;
 fParent:=aParent;
 fCursorCodePointIndex:=aCursorCodePointIndex;
end;

destructor TpvTextEditor.TUndoRedoCommand.Destroy;
begin
 inherited Destroy;
end;

procedure TpvTextEditor.TUndoRedoCommand.Redo(const aView:TpvTextEditor.TView=nil);
begin
end;

procedure TpvTextEditor.TUndoRedoCommand.Undo(const aView:TpvTextEditor.TView=nil);
begin
end;

constructor TpvTextEditor.TUndoRedoCommandInsert.Create(const aParent:TpvTextEditor;const aCursorCodePointIndex,aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String);
begin
 inherited Create(aParent,aCursorCodePointIndex);
 fCodePointIndex:=aCodePointIndex;
 fCountCodePoints:=aCountCodePoints;
 fCodeUnits:=aCodeUnits;
end;

destructor TpvTextEditor.TUndoRedoCommandInsert.Destroy;
begin
 inherited Destroy;
end;

procedure TpvTextEditor.TUndoRedoCommandInsert.Undo(const aView:TpvTextEditor.TView=nil);
begin
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fStringRope.Delete(fCodePointIndex,fCountCodePoints);
 fParent.UpdateViewCodePointIndices(fCodePointIndex,-fCountCodePoints);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fCursorCodePointIndex;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TUndoRedoCommandInsert.Redo(const aView:TpvTextEditor.TView=nil);
begin
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fStringRope.Insert(fCodePointIndex,fCodeUnits);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fCursorCodePointIndex;
 end;
 fParent.UpdateViewCodePointIndices(fCodePointIndex,fCountCodePoints);
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

constructor TpvTextEditor.TUndoRedoCommandOverwrite.Create(const aParent:TpvTextEditor;const aCursorCodePointIndex,aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits,aPreviousCodeUnits:TpvUTF8String);
begin
 inherited Create(aParent,aCursorCodePointIndex);
 fCodePointIndex:=aCodePointIndex;
 fCountCodePoints:=aCountCodePoints;
 fCodeUnits:=aCodeUnits;
 fPreviousCodeUnits:=aPreviousCodeUnits;
end;

destructor TpvTextEditor.TUndoRedoCommandOverwrite.Destroy;
begin
 inherited Destroy;
end;

procedure TpvTextEditor.TUndoRedoCommandOverwrite.Undo(const aView:TpvTextEditor.TView=nil);
begin
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fStringRope.Delete(fCodePointIndex,fCountCodePoints);
 fParent.fStringRope.Insert(fCodePointIndex,fPreviousCodeUnits);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fCursorCodePointIndex;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TUndoRedoCommandOverwrite.Redo(const aView:TpvTextEditor.TView=nil);
begin
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fStringRope.Delete(fCodePointIndex,fCountCodePoints);
 fParent.fStringRope.Insert(fCodePointIndex,fCodeUnits);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fCursorCodePointIndex+fCountCodePoints;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

constructor TpvTextEditor.TUndoRedoCommandDelete.Create(const aParent:TpvTextEditor;const aCursorCodePointIndex,aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String);
begin
 inherited Create(aParent,aCursorCodePointIndex);
 fCodePointIndex:=aCodePointIndex;
 fCountCodePoints:=aCountCodePoints;
 fCodeUnits:=aCodeUnits;
end;

destructor TpvTextEditor.TUndoRedoCommandDelete.Destroy;
begin
 inherited Destroy;
end;

procedure TpvTextEditor.TUndoRedoCommandDelete.Undo(const aView:TpvTextEditor.TView=nil);
begin
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fStringRope.Insert(fCodePointIndex,fCodeUnits);
 fParent.UpdateViewCodePointIndices(fCodePointIndex,fCountCodePoints);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fCursorCodePointIndex;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TUndoRedoCommandDelete.Redo(const aView:TpvTextEditor.TView=nil);
begin
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fStringRope.Delete(fCodePointIndex,fCountCodePoints);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fCursorCodePointIndex+fCountCodePoints;
 end;
 fParent.UpdateViewCodePointIndices(fCodePointIndex,-fCountCodePoints);
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

constructor TpvTextEditor.Create;
begin
 inherited Create;
 fStringRope:=TpvUTF8StringRope.Create;
 fStringRopeLineMap:=TpvUTF8StringRopeLineMap.Create(fStringRope);
 fFirstView:=nil;
 fLastView:=nil;
 fUndoStack:=TObjectList.Create;
 fUndoStack.OwnsObjects:=true;
 fRedoStack:=TObjectList.Create;
 fRedoStack.OwnsObjects:=true;
end;

destructor TpvTextEditor.Destroy;
begin
 while assigned(fLastView) do begin
  fLastView.Free;
 end;
 fStringRopeLineMap.Free;
 fStringRope.Free;
 fUndoStack.Clear;
 fRedoStack.Clear;
 fUndoStack.Free;
 fRedoStack.Free;
 inherited Destroy;
end;

function TpvTextEditor.IsTwoCodePointNewLine(const aCodePointIndex:TpvSizeInt):boolean;
var CodePoint,LastCodePoint:TpvUInt32;
    LastWasPossibleNewLineTwoCharSequence:boolean;
begin
 result:=false;
 LastCodePoint:=0;
 LastWasPossibleNewLineTwoCharSequence:=false;
 for CodePoint in fStringRope.GetCodePointEnumeratorSource(aCodePointIndex,aCodePointIndex+2) do begin
  case CodePoint of
   $0a,$0d:begin
    if LastWasPossibleNewLineTwoCharSequence and
       (((CodePoint=$0a) and (LastCodePoint=$0d)) or
        ((CodePoint=$0d) and (LastCodePoint=$0a))) then begin
     result:=true;
     break;
    end else begin
     LastWasPossibleNewLineTwoCharSequence:=true;
    end;
   end;
   else begin
    break;
   end;
  end;
  LastCodePoint:=CodePoint;
 end;
end;

procedure TpvTextEditor.LoadFromStream(const aStream:TStream);
begin
 fUndoStack.Clear;
 fRedoStack.Clear;
 if assigned(aStream) then begin
  fStringRope.Text:=TpvUTF8Utils.RawStreamToUTF8String(aStream);
 end else begin
  fStringRope.Text:='';
 end;
 ResetLineMaps;
 ResetViewCodePointIndices;
end;

procedure TpvTextEditor.LoadFromFile(const aFileName:string);
var FileStream:TFileStream;
begin
 FileStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(FileStream);
 finally
  FileStream.Free;
 end;
end;

procedure TpvTextEditor.LoadFromString(const aString:TpvRawByteString);
begin
 fUndoStack.Clear;
 fRedoStack.Clear;
 fStringRope.Text:=TpvUTF8Utils.RawByteStringToUTF8String(aString);
 ResetLineMaps;
 ResetViewCodePointIndices;
end;

procedure TpvTextEditor.SaveToStream(const aStream:TStream);
var TemporaryString:TpvUTF8String;
begin
 if assigned(aStream) then begin
  TemporaryString:=fStringRope.GetText;
  aStream.Seek(0,soBeginning);
  aStream.Size:=length(TemporaryString);
  if length(TemporaryString)>0 then begin
   aStream.Seek(0,soBeginning);
   aStream.WriteBuffer(TemporaryString[1],aStream.Size);
  end;
 end;
end;

procedure TpvTextEditor.SaveToFile(const aFileName:string);
var FileStream:TFileStream;
begin
 FileStream:=TFileStream.Create(aFileName,fmCreate);
 try
  SaveToStream(FileStream);
 finally
  FileStream.Free;
 end;
end;

function TpvTextEditor.SaveToString:TpvUTF8String;
begin
 result:=fStringRope.GetText;
end;

function TpvTextEditor.CreateView:TpvTextEditor.TView;
begin
 result:=TpvTextEditor.TView.Create(self);
end;

procedure TpvTextEditor.LineMapTruncate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var View:TView;
begin
 fStringRopeLineMap.Truncate(aUntilCodePoint,aUntilLine);
 View:=fFirstView;
 while assigned(View) do begin
  View.fStringRopeVisualLineMap.Truncate(aUntilCodePoint,aUntilLine);
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.LineMapUpdate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var View:TView;
begin
 fStringRopeLineMap.Update(aUntilCodePoint,aUntilLine);
 View:=fFirstView;
 while assigned(View) do begin
  View.fStringRopeVisualLineMap.Update(aUntilCodePoint,aUntilLine);
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.ResetLineMaps;
var View:TView;
begin
 fStringRopeLineMap.Truncate(0,0);
 fStringRopeLineMap.Update(-1,-1);
 View:=fFirstView;
 while assigned(View) do begin
  View.fStringRopeVisualLineMap.Truncate(0,0);
  View.fStringRopeVisualLineMap.Update(-1,-1);
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.ResetViewCodePointIndices;
var View:TView;
begin
 View:=fFirstView;
 while assigned(View) do begin
  View.fCodePointIndex:=0;
  View.EnsureCodePointIndexIsInRange;
  View.EnsureCursorIsVisible(true);
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.UpdateViewCodePointIndices(const aCodePointIndex,aDelta:TpvSizeInt);
var View:TView;
begin
 View:=fFirstView;
 while assigned(View) do begin
  if View.fCodePointIndex>=aCodePointIndex then begin
   inc(View.fCodePointIndex,aDelta);
  end;
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.EnsureViewCodePointIndicesAreInRange;
var View:TView;
begin
 View:=fFirstView;
 while assigned(View) do begin
  View.EnsureCodePointIndexIsInRange;
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.EnsureViewCursorsAreVisible(const aUpdateCursors:boolean=true;const aForceVisibleLines:TpvSizeInt=1);
var View:TView;
begin
 View:=fFirstView;
 while assigned(View) do begin
  View.EnsureCursorIsVisible(aUpdateCursors,aForceVisibleLines);
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.UpdateViewCursors;
var View:TView;
begin
 View:=fFirstView;
 while assigned(View) do begin
  View.UpdateCursor;
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.Undo(const aView:TView=nil);
var UndoRedoCommand:TUndoRedoCommand;
begin
 if fUndoStack.Count>0 then begin
  UndoRedoCommand:=TUndoRedoCommand(fUndoStack.Extract(fUndoStack.Items[fUndoStack.Count-1]));
  try
   UndoRedoCommand.Undo(aView);
  finally
   fRedoStack.Add(UndoRedoCommand);
  end;
 end;
end;

procedure TpvTextEditor.Redo(const aView:TView=nil);
var UndoRedoCommand:TUndoRedoCommand;
begin
 if fRedoStack.Count>0 then begin
  UndoRedoCommand:=TUndoRedoCommand(fRedoStack.Extract(fRedoStack.Items[fRedoStack.Count-1]));
  try
   UndoRedoCommand.Redo(aView);
  finally
   fUndoStack.Add(UndoRedoCommand);
  end;
 end;
end;

constructor TpvTextEditor.TView.Create(const aParent:TpvTextEditor);
begin
 inherited Create;
 fParent:=aParent;
 fVisibleAreaWidth:=0;
 fVisibleAreaHeight:=0;
 fNonScrollVisibleAreaWidth:=0;
 fNonScrollVisibleAreaHeight:=0;
 fVisibleAreaDirty:=false;
 fCodePointIndex:=0;
 fCursorOffset.x:=0;
 fCursorOffset.y:=0;
 fCursor.x:=0;
 fCursor.y:=0;
 fLineWrap:=0;
 fStringRopeVisualLineMap:=TpvUTF8StringRopeLineMap.Create(fParent.fStringRope);
 fBuffer:=nil;
end;

destructor TpvTextEditor.TView.Destroy;
begin
 FreeAndNil(fStringRopeVisualLineMap);
 fBuffer:=nil;
 inherited Destroy;
end;

procedure TpvTextEditor.TView.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fParent) then begin
  if assigned(fParent.fFirstView) then begin
   fParent.fFirstView.fNext:=self;
   fPrevious:=fParent.fFirstView;
  end else begin
   fParent.fFirstView:=self;
   fPrevious:=nil;
  end;
  fParent.fLastView:=self;
  fNext:=nil;
 end;
end;

procedure TpvTextEditor.TView.BeforeDestruction;
begin
 if assigned(fParent) then begin
  if assigned(fNext) then begin
   fNext.fPrevious:=fPrevious;
  end else if fParent.fLastView=self then begin
   fParent.fLastView:=fPrevious;
  end;
  if assigned(fPrevious) then begin
   fPrevious.fNext:=fNext;
  end else if fParent.fFirstView=self then begin
   fParent.fFirstView:=fNext;
  end;
  fPrevious:=nil;
  fNext:=nil;
 end;
 inherited BeforeDestruction;
end;

procedure TpvTextEditor.TView.SetVisibleAreaWidth(const aVisibleAreaWidth:TpvSizeInt);
begin
 if fVisibleAreaWidth<>aVisibleAreaWidth then begin
  fVisibleAreaWidth:=aVisibleAreaWidth;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvTextEditor.TView.SetVisibleAreaHeight(const aVisibleAreaHeight:TpvSizeInt);
begin
 if fVisibleAreaHeight<>aVisibleAreaHeight then begin
  fVisibleAreaHeight:=aVisibleAreaHeight;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvTextEditor.TView.SetNonScrollVisibleAreaWidth(const aNonScrollVisibleAreaWidth:TpvSizeInt);
begin
 if fNonScrollVisibleAreaWidth<>aNonScrollVisibleAreaWidth then begin
  fNonScrollVisibleAreaWidth:=aNonScrollVisibleAreaWidth;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvTextEditor.TView.SetNonScrollVisibleAreaHeight(const aNonScrollVisibleAreaHeight:TpvSizeInt);
begin
 if fNonScrollVisibleAreaHeight<>aNonScrollVisibleAreaHeight then begin
  fNonScrollVisibleAreaHeight:=aNonScrollVisibleAreaHeight;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvTextEditor.TView.SetLineWrap(const aLineWrap:TpvSizeInt);
begin
 if fLineWrap<>aLineWrap then begin
  fLineWrap:=aLineWrap;
  fStringRopeVisualLineMap.LineWrap:=aLineWrap;
  fStringRopeVisualLineMap.Update(-1,-1);
  if aLineWrap>0 then begin
   fCursorOffset.x:=0;
  end;
  EnsureCodePointIndexIsInRange;
  EnsureCursorIsVisible(true);
 end;
end;

procedure TpvTextEditor.TView.SetLineColumn(const aLineColumn:TLineColumn);
begin
 fCodePointIndex:=fParent.fStringRopeLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(aLineColumn.Line,aLineColumn.Column);
 EnsureCodePointIndexIsInRange;
 EnsureCursorIsVisible(true);
end;

procedure TpvTextEditor.TView.EnsureCodePointIndexIsInRange;
begin
 fCodePointIndex:=Min(Max(fCodePointIndex,0),fParent.fStringRope.CountCodePoints);
end;

procedure TpvTextEditor.TView.EnsureCursorIsVisible(const aUpdateCursor:boolean=true;const aForceVisibleLines:TpvSizeInt=1);
var CurrentLineIndex,CurrentColumnIndex:TpvSizeInt;
begin

 if fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin

  if CurrentLineIndex<fCursorOffset.y then begin
   fCursorOffset.y:=CurrentLineIndex;
  end else if (fCursorOffset.y+NonScrollVisibleAreaHeight)<(CurrentLineIndex+aForceVisibleLines) then begin
   fCursorOffset.y:=(CurrentLineIndex+aForceVisibleLines)-NonScrollVisibleAreaHeight;
  end;

  if CurrentColumnIndex<fCursorOffset.x then begin
   fCursorOffset.x:=CurrentColumnIndex;
  end else if (fCursorOffset.x+NonScrollVisibleAreaWidth)<=CurrentColumnIndex then begin
   fCursorOffset.x:=(CurrentColumnIndex-NonScrollVisibleAreaWidth)+1;
  end;

  if aUpdateCursor then begin
   fCursor.x:=CurrentColumnIndex-fCursorOffset.x;
   fCursor.y:=CurrentLineIndex-fCursorOffset.y;
  end;

 end;

 if aUpdateCursor and fParent.fStringRopeLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin
  fLineColumn.Line:=CurrentLineIndex;
  fLineColumn.Column:=CurrentColumnIndex;
 end;

end;

procedure TpvTextEditor.TView.UpdateCursor;
var CurrentLineIndex,CurrentColumnIndex:TpvSizeInt;
begin
 if fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin
  fCursor.x:=CurrentColumnIndex-fCursorOffset.x;
  fCursor.y:=CurrentLineIndex-fCursorOffset.y;
 end;
 if fParent.fStringRopeLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin
  fLineColumn.Line:=CurrentLineIndex;
  fLineColumn.Column:=CurrentColumnIndex;
 end;
end;

procedure TpvTextEditor.TView.UpdateBuffer;
const EmptyBufferItem:TBufferItem=
       (
        BackgroundColor:0;
        ForegroundColor:0;
        CodePoint:32;
       );
var BufferSize,BufferBaseIndex,BufferBaseEndIndex,BufferIndex,
    CurrentLineIndex,StartCodePointIndex,StopCodePointIndex,
    CurrentCodePointIndex,StepWidth:TpvSizeInt;
    CodePoint,IncomingCodePoint:TpvUInt32;
    RelativeCursor:TCoordinate;
    CodePointEnumerator:TpvUTF8StringRope.TCodePointEnumerator;
begin

 EnsureCodePointIndexIsInRange;

 EnsureCursorIsVisible(true);

 BufferSize:=VisibleAreaWidth*VisibleAreaHeight;

 CodePointEnumerator.fFirst:=true; // for to suppress compiler-warning

 if BufferSize>0 then begin

  if length(fBuffer)<>BufferSize then begin
   SetLength(fBuffer,BufferSize);
  end;

  for BufferIndex:=0 to BufferSize-1 do begin
   fBuffer[BufferIndex]:=EmptyBufferItem;
  end;

  BufferBaseIndex:=0;

  RelativeCursor.y:=-fCursorOffset.y;

  CurrentCodePointIndex:=-1;

  for CurrentLineIndex:=fCursorOffset.y to fCursorOffset.y+(VisibleAreaHeight-1) do begin

   StartCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndex(CurrentLineIndex);
   if (StartCodePointIndex<0) or
      (StartCodePointIndex>=fParent.fStringRope.fCountCodePoints) then begin
    break;
   end;

   StopCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromNextLineIndexOrTextEnd(CurrentLineIndex);

   BufferBaseEndIndex:=BufferBaseIndex+VisibleAreaWidth;

   if BufferBaseEndIndex>BufferSize then begin
    BufferBaseEndIndex:=BufferSize;
   end;

   BufferIndex:=BufferBaseIndex;

   RelativeCursor.x:=-fCursorOffset.x;

   if CurrentCodePointIndex<>StartCodePointIndex then begin
    CurrentCodePointIndex:=StartCodePointIndex;

    CodePointEnumerator:=TpvUTF8StringRope.TCodePointEnumerator.Create(fParent.fStringRope,StartCodePointIndex,-1);

   end;

   while (CurrentCodePointIndex<StopCodePointIndex) and
         CodePointEnumerator.MoveNext do begin

    IncomingCodePoint:=CodePointEnumerator.GetCurrent;

    case IncomingCodePoint of
     $09:begin
      CodePoint:=32;
      StepWidth:=Max(1,(fStringRopeVisualLineMap.fTabWidth-(RelativeCursor.x mod fStringRopeVisualLineMap.fTabWidth)));
     end;
     $0a,$0d:begin
      CodePoint:=32;
      StepWidth:=0;
     end;
     else begin
      CodePoint:=IncomingCodePoint;
      StepWidth:=1;
     end;
    end;

    if StepWidth>0 then begin

     if RelativeCursor.x>=0 then begin

      BufferIndex:=BufferBaseIndex+RelativeCursor.x;

      if (BufferIndex>=BufferBaseIndex) and
         (BufferIndex<BufferBaseEndIndex) then begin
       fBuffer[BufferIndex].CodePoint:=CodePoint;
      end;

     end;

     inc(RelativeCursor.x,StepWidth);

    end;

    inc(CurrentCodePointIndex);

   end;

   inc(BufferBaseIndex,VisibleAreaWidth);

   inc(RelativeCursor.y);

  end;

 end;

end;

procedure TpvTextEditor.TView.InsertCodePoint(const aCodePoint:TpvUInt32;const aOverwrite:boolean);
var Count:TpvSizeInt;
    CodeUnits:TpvUTF8String;
begin
 CodeUnits:=TpvUTF8Utils.UTF32CharToUTF8(aCodePoint);
 fParent.LineMapTruncate(fCodePointIndex,-1);
 if aOverwrite and (fCodePointIndex<fParent.fStringRope.fCountCodePoints) then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
   fParent.fRedoStack.Clear;
   fParent.fUndoStack.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex,2,fParent.fStringRope.Extract(fCodePointIndex,2)));
   fParent.fStringRope.Delete(fCodePointIndex,Count);
   fParent.fStringRope.Insert(fCodePointIndex,CodeUnits);
  end else begin
   Count:=1;
   fParent.fRedoStack.Clear;
   fParent.fUndoStack.Add(TUndoRedoCommandOverwrite.Create(fParent,fCodePointIndex,fCodePointIndex,1,CodeUnits,fParent.fStringRope.Extract(fCodePointIndex,1)));
   fParent.fStringRope.Delete(fCodePointIndex,Count);
   fParent.fStringRope.Insert(fCodePointIndex,CodeUnits);
  end;
 end else begin
  fParent.fRedoStack.Clear;
  fParent.fUndoStack.Add(TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex,1,CodeUnits));
  fParent.fStringRope.Insert(fCodePointIndex,CodeUnits);
 end;
 fParent.UpdateViewCodePointIndices(fCodePointIndex,1);
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TView.InsertString(const aCodeUnits:TpvUTF8String;const aOverwrite:boolean);
var CountCodePoints,Count:TpvSizeInt;
begin
 CountCodePoints:=TpvUTF8StringRope.GetCountCodePoints(@aCodeUnits[1],length(aCodeUnits));
 fParent.LineMapTruncate(fCodePointIndex,-1);
 if aOverwrite and (fCodePointIndex<fParent.fStringRope.fCountCodePoints) then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  fParent.fRedoStack.Clear;
  fParent.fUndoStack.Add(TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex,CountCodePoints,aCodeUnits));
  fParent.fStringRope.Delete(fCodePointIndex,(CountCodePoints+Count)-1);
  fParent.fStringRope.Insert(fCodePointIndex,aCodeUnits);
 end else begin
  fParent.fRedoStack.Clear;
  fParent.fUndoStack.Add(TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex,CountCodePoints,aCodeUnits));
  fParent.fStringRope.Insert(fCodePointIndex,aCodeUnits);
 end;
 fParent.UpdateViewCodePointIndices(fCodePointIndex,CountCodePoints);
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TView.Backspace;
var Count:TpvSizeInt;
begin
 if (fCodePointIndex>0) and (fCodePointIndex<=fParent.fStringRope.fCountCodePoints) then begin
  if fparent.IsTwoCodePointNewLine(fCodePointIndex-2) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  fParent.fRedoStack.Clear;
  fParent.fUndoStack.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex-Count,Count,fParent.fStringRope.Extract(fCodePointIndex-Count,Count)));
  fParent.UpdateViewCodePointIndices(fCodePointIndex,-Count);
  fParent.fStringRope.Delete(fCodePointIndex,Count);
  if fCodePointIndex>0 then begin
   fParent.LineMapTruncate(fCodePointIndex-1,-1);
  end else begin
   fParent.LineMapTruncate(fCodePointIndex,-1);
  end;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TView.Delete;
var Count:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fStringRope.fCountCodePoints then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  fParent.fRedoStack.Clear;
  fParent.fUndoStack.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex,Count,fParent.fStringRope.Extract(fCodePointIndex,Count)));
  fParent.fStringRope.Delete(fCodePointIndex,Count);
  if fCodePointIndex>0 then begin
   fParent.LineMapTruncate(fCodePointIndex-1,-1);
  end else begin
   fParent.LineMapTruncate(fCodePointIndex,-1);
  end;
 end;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TView.Enter(const aOverwrite:boolean);
begin
 if aOverwrite then begin
  MoveDown;
  MoveToLineBegin;
 end else begin
{$ifdef Windows}
  InsertString(TpvUTF8String(#13#10),aOverwrite);
{$else}
  InsertCodePoint(10,aOverwrite);
{$endif}
 end;
 fParent.UpdateViewCursors;
end;

procedure TpvTextEditor.TView.MoveUp;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fStringRope.CountCodePoints then begin
  fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex-1,ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
end;

procedure TpvTextEditor.TView.MoveDown;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fStringRope.CountCodePoints then begin
  fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex+1,ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
end;

procedure TpvTextEditor.TView.MoveLeft;
var Count:TpvSizeInt;
begin
 if fCodePointIndex>0 then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex-2) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  dec(fCodePointIndex,Count);
 end;
end;

procedure TpvTextEditor.TView.MoveRight;
var Count:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fStringRope.CountCodePoints then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  inc(fCodePointIndex,Count);
 end;
end;

procedure TpvTextEditor.TView.MoveToLineBegin;
var LineIndex:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fStringRope.CountCodePoints then begin
  LineIndex:=fParent.fStringRopeLineMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
  fCodePointIndex:=fParent.fStringRopeLineMap.GetCodePointIndexFromLineIndex(LineIndex);
 end else if (fCodePointIndex>0) and (fCodePointIndex>=fParent.fStringRope.CountCodePoints) then begin
  LineIndex:=fParent.fStringRopeLineMap.GetLineIndexFromCodePointIndex(fParent.fStringRope.CountCodePoints);
  fCodePointIndex:=fParent.fStringRopeLineMap.GetCodePointIndexFromLineIndex(LineIndex);
 end;
end;

procedure TpvTextEditor.TView.MoveToLineEnd;
var LineIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fStringRope.CountCodePoints then begin
  LineIndex:=fParent.fStringRopeLineMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fParent.fStringRopeLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex,High(TpvSizeInt));
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
end;

procedure TpvTextEditor.TView.MovePageUp;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fStringRope.CountCodePoints then begin
  fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(Max(0,LineIndex-fNonScrollVisibleAreaHeight),ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
  EnsureCodePointIndexIsInRange;
  EnsureCursorIsVisible(true,fNonScrollVisibleAreaHeight);
 end;
end;

procedure TpvTextEditor.TView.MovePageDown;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fStringRope.CountCodePoints then begin
  fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   fStringRopeVisualLineMap.Update(-1,LineIndex+fNonScrollVisibleAreaHeight+1);
   NewCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(Max(0,Min(LineIndex+fNonScrollVisibleAreaHeight,fStringRopeVisualLineMap.fCountLines-1)),ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
  EnsureCodePointIndexIsInRange;
  EnsureCursorIsVisible(true,fNonScrollVisibleAreaHeight);
 end;
end;

procedure TpvTextEditor.TView.InsertLine;
var LineIndex,LineCodePointIndex:TpvSizeInt;
begin
 LineIndex:=fParent.fStringRopeLineMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
 if LineIndex>=0 then begin
  LineCodePointIndex:=fParent.fStringRopeLineMap.GetCodePointIndexFromLineIndex(LineIndex);
  fParent.LineMapTruncate(LineCodePointIndex,-1);
{$ifdef Windows}
  fParent.fRedoStack.Clear;
  fParent.fUndoStack.Add(TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,LineCodePointIndex,2,#13#10));
  fParent.fStringRope.Insert(LineCodePointIndex,TpvUTF8String(#13#10));
  fParent.UpdateViewCodePointIndices(LineCodePointIndex,2);
{$else}
  fParent.fRedoStack.Clear;
  fParent.fUndoStack.Add(TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,LineCodePointIndex,1,#10));
  fParent.fStringRope.Insert(LineCodePointIndex,TpvUTF8String(#10));
  fParent.UpdateViewCodePointIndices(LineCodePointIndex,1);
{$endif}
  fParent.EnsureViewCodePointIndicesAreInRange;
  fParent.EnsureViewCursorsAreVisible(true);
 end;
end;

procedure TpvTextEditor.TView.DeleteLine;
var LineIndex,StartCodePointIndex,StopCodePointIndex:TpvSizeInt;
begin
 LineIndex:=fParent.fStringRopeLineMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
 if LineIndex>=0 then begin
  StartCodePointIndex:=fParent.fStringRopeLineMap.GetCodePointIndexFromLineIndex(LineIndex);
  StopCodePointIndex:=fParent.fStringRopeLineMap.GetCodePointIndexFromNextLineIndexOrTextEnd(LineIndex);
  if (StartCodePointIndex>=0) and
     (StartCodePointIndex<StopCodePointIndex) then begin
   fParent.fRedoStack.Clear;
   fParent.fUndoStack.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,StartCodePointIndex,StopCodePointIndex-StartCodePointIndex,fParent.fStringRope.Extract(StartCodePointIndex,StopCodePointIndex-StartCodePointIndex)));
   fParent.fStringRope.Delete(StartCodePointIndex,StopCodePointIndex-StartCodePointIndex);
   fParent.LineMapTruncate(Max(0,StartCodePointIndex)-1,-1);
   fParent.UpdateViewCodePointIndices(fCodePointIndex,StartCodePointIndex-fCodePointIndex);
   fParent.EnsureViewCodePointIndicesAreInRange;
   fParent.EnsureViewCursorsAreVisible(true);
  end;
 end;
end;

procedure TpvTextEditor.TView.Undo;
begin
 fParent.Undo(self);
end;

procedure TpvTextEditor.TView.Redo;
begin
 fParent.Redo(self);
end;

end.
