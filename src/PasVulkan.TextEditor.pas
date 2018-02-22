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

{-$define TpvTextEditorUsePUCU}

type TpvTextEditor=class
      public
       const NewLineCodePointSequence={$ifdef Windows}#13#10{$else}#10{$endif};
       type TUTF8DFA=class
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
            TUTF8Utils=class
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
              class function UTF8GetCodePointAndIncFallback(const aString:TpvUTF8String;var aCodeUnit:TpvSizeInt):TpvUInt32; static;
              class function UTF8Correct(const aString:TpvUTF8String):TpvUTF8String; static;
              class function RawDataToUTF8String(const aData;const aDataLength:TpvInt32;const aCodePage:TpvInt32=-1):TpvUTF8String; static;
              class function RawByteStringToUTF8String(const aString:TpvRawByteString;const aCodePage:TpvInt32=-1):TpvUTF8String; static;
              class function RawStreamToUTF8String(const aStream:TStream;const aCodePage:TpvInt32=-1):TpvUTF8String; static;
            end;
            ERope=class(Exception);
            TRope=class
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
                     fRope:TRope;
                     fFirst:boolean;
                     fNode:TNode;
                     function GetCurrent:TNode; inline;
                    public
                     constructor Create(const aCodeUnitsRope:TRope);
                     function MoveNext:boolean; inline;
                     property Current:TNode read GetCurrent;
                   end;
                   TCodePointEnumerator=record
                    private
                     fRope:TRope;
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
                     constructor Create(const aCodeUnitsRope:TRope;const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1);
                     function CanMoveNext:boolean; inline;
                     function MoveNext:boolean;
                     property Current:TpvUInt32 read GetCurrent;
                   end;
                   TCodePointEnumeratorSource=record
                    private
                     fRope:TRope;
                     fStartCodePointIndex:TpvSizeInt;
                     fStopCodePointIndex:TpvSizeInt;
                    public
                     constructor Create(const aCodeUnitsRope:TRope;const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1);
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
              constructor Create(const aFrom:TRope); reintroduce; overload;
              destructor Destroy; override;
              procedure Clear;
              function GetNodeAndOffsetFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aNode:TNode;out aNodeCodeUnitIndex:TpvSizeInt):boolean;
              procedure Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt); overload;
              procedure Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:TpvUTF8String); overload;
              procedure Delete(const aCodePointIndex,aCountCodePoints:TpvSizeInt);
              function Extract(const aCodePointIndex,aCountCodePoints:TpvSizeInt):TpvUTF8String;
              function GetCodePoint(const aCodePointIndex:TpvSizeInt):TpvUInt32;
              function GetEnumerator:TNodeEnumerator;
              function GetCodePointEnumeratorSource(const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1):TRope.TCodePointEnumeratorSource;
              procedure Check;
              procedure Dump;
              property CountCodePoints:TpvSizeInt read fCountCodePoints;
              property CountCodeUnits:TpvSizeInt read fCountCodeUnits;
              property Text:TpvUTF8String read GetText write SetText;
            end;
            TLineCacheMap=class
             public
              type TLine=TpvSizeInt;
                   PLine=^TLine;
                   TLines=array of TLine;
             private
              fRope:TRope;
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
              constructor Create(const aRope:TRope); reintroduce;
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
            TCoordinate=record
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
            TMarkState=record
             public
              StartCodePointIndex:TpvSizeInt;
              EndCodePointIndex:TpvSizeInt;
            end;
            PMarkState=^TMarkState;
            TView=class;
            TUndoRedoCommand=class;
            TUndoRedoCommandClass=class of TUndoRedoCommand;
            TUndoRedoCommand=class
             private
              fParent:TpvTextEditor;
              fUndoCursorCodePointIndex:TpvSizeInt;
              fRedoCursorCodePointIndex:TpvSizeInt;
              fUndoMarkState:TMarkState;
              fRedoMarkState:TMarkState;
              fSealed:boolean;
              fActionID:TpvUInt64;
             public
              constructor Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState); reintroduce; virtual;
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
              constructor Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState;const aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String); reintroduce;
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
              constructor Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState;const aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits,aPreviousCodeUnits:TpvUTF8String); reintroduce;
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
              constructor Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState;const aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String); reintroduce;
              destructor Destroy; override;
              procedure Undo(const aView:TpvTextEditor.TView=nil); override;
              procedure Redo(const aView:TpvTextEditor.TView=nil); override;
            end;
            TUndoRedoCommandGroup=class(TUndoRedoCommand)
             private
              fClass:TUndoRedoCommandClass;
              fList:TObjectList;
             public
              constructor Create(const aParent:TpvTextEditor;const aClass:TUndoRedoCommandClass); reintroduce;
              destructor Destroy; override;
              procedure Undo(const aView:TpvTextEditor.TView=nil); override;
              procedure Redo(const aView:TpvTextEditor.TView=nil); override;
            end;
            TUndoRedoManager=class(TObjectList)
             private
              fParent:TpvTextEditor;
              fHistoryIndex:TpvSizeInt;
              fMaxUndoSteps:TpvSizeInt;
              fMaxRedoSteps:TpvSizeInt;
              fActionID:TpvUInt64;
             public
              constructor Create(const aParent:TpvTextEditor); reintroduce;
              destructor Destroy; override;
              procedure Clear; reintroduce;
              procedure IncreaseActionID;
              procedure Add(const aUndoRedoCommand:TpvTextEditor.TUndoRedoCommand); reintroduce;
              procedure GroupUndoRedoCommands(const aFromIndex,aToIndex:TpvSizeInt);
              procedure Undo(const aView:TpvTextEditor.TView=nil);
              procedure Redo(const aView:TpvTextEditor.TView=nil);
             published
              property HistoryIndex:TpvSizeInt read fHistoryIndex write fHistoryIndex;
              property MaxUndoSteps:TpvSizeInt read fMaxUndoSteps write fMaxUndoSteps;
              property MaxRedoSteps:TpvSizeInt read fMaxRedoSteps write fMaxRedoSteps;
            end;
            TSyntaxHighlighting=class;
            TSyntaxHighlightingClass=class of TSyntaxHighlighting;
            TSyntaxHighlighting=class
             public
              type TAttributes=class
                    public
                     const Unknown=0;
                           WhiteSpace=1;
                           Preprocessor=2;
                           Comment=3;
                           Keyword=4;
                           Type_=5;
                           Builtin=6;
                           Identifier=7;
                           Number=8;
                           Symbol=9;
                           String_=10;
                           Delimiter=11;
                           Operator=12;
                           Highlight=TpvUInt32($80000000);
                           Mask=TpvUInt32($7fffffff);
                   end;
                   TState=class
                    private
                     fCodePointIndex:TpvSizeInt;
                     fLevel:TpvUInt32;
                     fAttribute:TpvUInt32;
                    public
                     property CodePointIndex:TpvSizeInt read fCodePointIndex write fCodePointIndex;
                     property Level:TpvUInt32 read fLevel write fLevel;
                     property Attribute:TpvUInt32 read fAttribute write fAttribute;
                   end;
                   TStates=array of TState;
                   TFileExtensions=array of TpvUTF8String;
             private
              fParent:TpvTextEditor;
             protected
              fStates:TStates;
              fCountStates:TpvSizeInt;
              fCodePointIndex:TpvSizeInt;
              fLevel:TpvUInt32;
              function GetStateIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt):TpvSizeInt;
             public
              constructor Create(const aParent:TpvTextEditor); reintroduce; virtual;
              destructor Destroy; override;
              class function GetName:TpvUTF8String; virtual;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; virtual;
              procedure Reset; virtual;
              procedure Truncate(const aUntilCodePoint:TpvSizeInt); virtual;
              procedure Update(const aUntilCodePoint:TpvSizeInt); virtual;
              class function GetSyntaxHighlightingClassByFileExtension(const aFileExtension:TpvUTF8String):TSyntaxHighlightingClass; static;
             published
              property Parent:TpvTextEditor read fParent;
            end;
            TGenericSyntaxHighlighting=class(TSyntaxHighlighting)
             public
              type TState=class(TSyntaxHighlighting.TState);
             public
              class function GetName:TpvUTF8String; override;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; override;
              procedure Update(const aUntilCodePoint:TpvSizeInt); override;
            end;
            TDFASyntaxHighlighting=class(TSyntaxHighlighting)
             public
              const KeywordCharSet=[#32..#127];
              type TCharSet=set of AnsiChar;
                   PCharSet=^TCharSet;
                   TNFA=class
                    private
                     fNext:TNFA;
                     fFrom:TpvUInt32;
                     fTo:TpvUInt32;
                     fSet:TCharSet;
                   end;
                   TNFAArray=array of TNFA;
                   TNFASetArray=array of TpvUInt32;
                   TNFASet=record
                    private
                     fSet:TNFASetArray;
                    public
                     constructor Create(const aValues:array of TpvUInt32);
                     class operator Add(const aSet:TNFASet;const aValue:TpvUInt32):TNFASet;
                     class operator Add(const aSet,aOtherSet:TNFASet):TNFASet;
                     class operator Subtract(const aSet:TNFASet;const aValue:TpvUInt32):TNFASet;
                     class operator Subtract(const aSet,aOtherSet:TNFASet):TNFASet;
                     class operator Multiply(const aSet,aOtherSet:TNFASet):TNFASet;
                     class operator BitwiseAnd(const aSet,aOtherSet:TNFASet):TNFASet;
                     class operator BitwiseOr(const aSet,aOtherSet:TNFASet):TNFASet;
                     class operator BitwiseXor(const aSet,aOtherSet:TNFASet):TNFASet;
                     class operator In(const aValue:TpvUInt32;const aSet:TNFASet):boolean;
                     class operator Equal(const aSet,aOtherSet:TNFASet):boolean;
                     class operator NotEqual(const aSet,aOtherSet:TNFASet):boolean;
                   end;
                   TAccept=class
                    public
                     type TFlag=
                           (
                            IsQuick,
                            IsEnd,
                            IsPreprocessorLine,
                            IsMaybeCPreprocessorMultiLine,
                            IsKeyword,
                            IncreaseLevel,
                            DecreaseLevel
                           );
                          PFlag=^TFlag;
                          TFlags=set of TFlag;
                          PFlags=^TFlags;
                    private
                     fNext:TAccept;
                     fFlags:TFlags;
                     fState:TpvUInt32;
                     fAttribute:TpvUInt32;
                   end;
                   TDFA=class
                    public
                     type TDFASet=array[AnsiChar] of TDFA;
                          PDFASet=^TDFASet;
                    private
                     fNext:TDFA;
                     fNumber:TpvSizeInt;
                     fNFASet:TNFASet;
                     fAccept:TAccept;
                     fAcceptEnd:TAccept;
                     fWhereTo:TDFASet;
                   end;
                   TDFAArray=array of TDFA;
                   TEquivalence=array[AnsiChar] of AnsiChar;
                   PEquivalence=^TEquivalence;
                   EParserError=class(Exception);
                   EParserErrorExpectedEndOfText=class(EParserError);
                   EParserErrorUnexpectedEndOfText=class(EParserError);
                   EParserErrorExpectedRightParen=class(EParserError);
                   EParserErrorExpectedRightBracket=class(EParserError);
                   EParserErrorEmptySet=class(EParserError);
                   EParserErrorInvalidMetaChar=class(EParserError);
                   EParserErrorInvalidCount=class(EParserError);
                   TKeywordCharSet=#32..#127;
                   TKeywordCharTreeNode=class
                    public
                     type TKeywordCharTreeNodes=array[TKeywordCharSet] of TKeywordCharTreeNode;
                    private
                     fChildren:TKeywordCharTreeNodes;
                     fHasChildren:boolean;
                     fKeyword:boolean;
                     fFlags:TAccept.TFlags;
                     fAttribute:TpvUInt32;
                    public
                     constructor Create; reintroduce;
                     destructor Destroy; override;
                   end;
                   TState=class(TSyntaxHighlighting.TState)
                    private
                     fAccept:TAccept;
                   end;
             private
              fNFAStates:TpvSizeInt;
              fDFAStates:TpvSizeInt;
              fNFA:TNFA;
              fDFA:TDFA;
              fAccept:TAccept;
              fEquivalence:TEquivalence;
              fKeywordCharRootTreeNode:TKeywordCharTreeNode;
              fCaseInsensitive:boolean;
              procedure Clear;
              procedure BuildDFA;
             protected
              procedure Setup; virtual;
             public
              constructor Create(const aParent:TpvTextEditor); override;
              destructor Destroy; override;
              class function GetName:TpvUTF8String; override;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; override;
              procedure AddKeyword(const aKeyword:TpvUTF8String;const aFlags:TAccept.TFlags;const aAttribute:TpvUInt32);
              procedure AddKeywords(const aKeywords:array of TpvUTF8String;const aFlags:TAccept.TFlags;const aAttribute:TpvUInt32);
              procedure AddRule(const aRule:TpvUTF8String;const aFlags:TAccept.TFlags;const aAttribute:TpvUInt32);
              procedure Update(const aUntilCodePoint:TpvSizeInt); override;
            end;
            TPascalSyntaxHighlighting=class(TDFASyntaxHighlighting)
             protected
              procedure Setup; override;
             public
              class function GetName:TpvUTF8String; override;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; override;
            end;
            TCSyntaxHighlighting=class(TDFASyntaxHighlighting)
             protected
              procedure Setup; override;
             public
              class function GetName:TpvUTF8String; override;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; override;
            end;
            TCPPSyntaxHighlighting=class(TDFASyntaxHighlighting)
             protected
              procedure Setup; override;
             public
              class function GetName:TpvUTF8String; override;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; override;
            end;
            TJavaSyntaxHighlighting=class(TDFASyntaxHighlighting)
             protected
              procedure Setup; override;
             public
              class function GetName:TpvUTF8String; override;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; override;
            end;
            TGLSLSyntaxHighlighting=class(TDFASyntaxHighlighting)
             protected
              procedure Setup; override;
             public
              class function GetName:TpvUTF8String; override;
              class function GetFileExtensions:TSyntaxHighlighting.TFileExtensions; override;
            end;
            TCharSet=set of AnsiChar;
            TCodePointSet=record
             public
              type TCodePointRange=record
                    private
                     fFromCodePoint:TpvUInt32;
                     fToCodePoint:TpvUInt32;
                    public
                     constructor Create(const aCodePoint:TpvUInt32); overload;
                     constructor Create(const aFromCodePoint,aToCodePoint:TpvUInt32); overload;
                     property FromCodePoint:TpvUInt32 read fFromCodePoint write fFromCodePoint;
                     property ToCodePoint:TpvUInt32 read fToCodePoint write fToCodePoint;
                   end;
                   PCodePointRange=^TCodePointRange;
                   TCodePointRanges=array of TCodePointRange;
             private
              fRanges:TCodePointRanges;
             public
              class function CreateEmpty:TCodePointSet; static;
              constructor Create(const aCodePointRanges:array of TCodePointRange); overload;
              constructor Create(const aCodePoints:array of TpvUInt32); overload;
              constructor Create(const aCharSet:TCharSet); overload;
              procedure Sort;
              procedure Optimize;
              class operator Add(const aCodePointSet,aOtherCodePointSet:TCodePointSet):TCodePointSet;
              class operator Add(const aCodePointSet:TCodePointSet;const aOtherCodePointSets:array of TCodePointSet):TCodePointSet;
              class operator Add(const aCodePointSet:TCodePointSet;const aCodePointRange:TCodePointRange):TCodePointSet;
              class operator Add(const aCodePointSet:TCodePointSet;const aCodePointRanges:array of TCodePointRange):TCodePointSet;
              class operator Add(const aCodePointSet:TCodePointSet;const aCodePoint:TpvUInt32):TCodePointSet;
              class operator Add(const aCodePointSet:TCodePointSet;const aCodePoints:array of TpvUInt32):TCodePointSet;
              class operator Subtract(const aCodePointSet,aOtherCodePointSet:TCodePointSet):TCodePointSet;
              class operator Subtract(const aCodePointSet:TCodePointSet;const aOtherCodePointSets:array of TCodePointSet):TCodePointSet;
              class operator Subtract(const aCodePointSet:TCodePointSet;const aCodePointRange:TCodePointRange):TCodePointSet;
              class operator Subtract(const aCodePointSet:TCodePointSet;const aCodePointRanges:array of TCodePointRange):TCodePointSet;
              class operator Subtract(const aCodePointSet:TCodePointSet;const aCodePoint:TpvUInt32):TCodePointSet;
              class operator Subtract(const aCodePointSet:TCodePointSet;const aCodePoints:array of TpvUInt32):TCodePointSet;
              class operator Multiply(const aCodePointSet,aOtherCodePointSet:TCodePointSet):TCodePointSet;
              class operator Multiply(const aCodePointSet:TCodePointSet;const aOtherCodePointSets:array of TCodePointSet):TCodePointSet;
              class operator Multiply(const aCodePointSet:TCodePointSet;const aCodePointRange:TCodePointRange):TCodePointSet;
              class operator Multiply(const aCodePointSet:TCodePointSet;const aCodePointRanges:array of TCodePointRange):TCodePointSet;
              class operator Multiply(const aCodePointSet:TCodePointSet;const aCodePoint:TpvUInt32):TCodePointSet;
              class operator Multiply(const aCodePointSet:TCodePointSet;const aCodePoints:array of TpvUInt32):TCodePointSet;
              class operator Equal(const aCodePointSet,aOtherCodePointSet:TCodePointSet):boolean;
              class operator NotEqual(const aCodePointSet,aOtherCodePointSet:TCodePointSet):boolean;
              class operator LogicalNot(const aCodePointSet:TCodePointSet):TCodePointSet;
              class operator Negative(const aCodePointSet:TCodePointSet):TCodePointSet;
              class operator In(const aCodePoint:TpvUInt32;const aCodePointSet:TCodePointSet):boolean;
              function ToCaseInsensitive:TCodePointSet;
              function ToLowerCase:TCodePointSet;
              function ToUpperCase:TCodePointSet;
            end;
            PCodePointSet=^TCodePointSet;
            ERegularExpression=class(Exception);
            PRegularExpressionCharClass=^TRegularExpressionCharClass;
            TRegularExpressionCharClass=TpvTextEditor.TCodePointSet;
            TPRegularExpressionCharClasses=array of PRegularExpressionCharClass;
            PRegularExpressionFlag=^TRegularExpressionFlag;
            TRegularExpressionFlag=
             (
              CaseInsensitive,
              MultiLine,
              Longest,
              Lazy,
              Greedy
             );
            TRegularExpressionFlags=set of TRegularExpressionFlag;
            PPRegularExpressionNode=^PRegularExpressionNode;
            PRegularExpressionNode=^TRegularExpressionNode;
            TRegularExpressionNode=record
             NodeType:TpvInt32;
             CharClass:TRegularExpressionCharClass;
             Value:TpvInt32;
             MinCount:TpvInt32;
             MaxCount:TpvInt32;
             Left:PRegularExpressionNode;
             Right:PRegularExpressionNode;
             Index:TpvInt32;
            end;
            PRegularExpressionInstruction=^TRegularExpressionInstruction;
            TRegularExpressionInstruction=record
             IndexAndOpcode:TpvPtrInt;
             Value:TpvPtrInt;
             Generation:TpvPtrUInt;
             Next:PRegularExpressionInstruction;
             OtherNext:PRegularExpressionInstruction;
            end;
            TRegularExpressionInstructions=array of TRegularExpressionInstruction;
            TPRegularExpressionInstructions=array of PRegularExpressionInstruction;
            PRegularExpressionSubMatchesItem=^TRegularExpressionSubMatchesItem;
            TRegularExpressionSubMatchesItem=TpvInt32;
            TRegularExpressionSubMatchesItems=array of TRegularExpressionSubMatchesItem;
            PRegularExpressionSubMatches=^TRegularExpressionSubMatches;
            TRegularExpressionSubMatches=record
             Next:PRegularExpressionSubMatches;
             ReferenceCounter:TpvInt32;
             Count:TpvInt32;
             BitState:TpvUInt32;
             SubMatches:TRegularExpressionSubMatchesItems;
            end;
            PRegularExpressionThread=^TRegularExpressionThread;
            TRegularExpressionThread=record
             Instruction:PRegularExpressionInstruction;
             SubMatches:PRegularExpressionSubMatches;
            end;
            TRegularExpressionThreads=array of TRegularExpressionThread;
            PRegularExpressionThreadList=^TRegularExpressionThreadList;
            TRegularExpressionThreadList=record
             Threads:TRegularExpressionThreads;
             Count:TpvInt32;
            end;
            TRegularExpressionThreadLists=array[0..1] of TRegularExpressionThreadList;
            PRegularExpressionCapture=^TRegularExpressionCapture;
            TRegularExpressionCapture=record
             Start:TpvInt32;
             Length:TpvInt32;
            end;
            TRegularExpressionCaptures=array of TRegularExpressionCapture;
            TRegularExpressionMultiCaptures=array of TRegularExpressionCaptures;
            TRegularExpression=class
             public
              const // Node types
                    ntALT=0;
                    ntCAT=1;
                    ntCHAR=2;
                    ntDOT=3;
                    ntPAREN=4;
                    ntQUEST=5;
                    ntSTAR=6;
                    ntPLUS=7;
                    ntEXACT=8;
                    ntBOL=9;
                    ntEOL=10;
                    ntBRK=11;
                    ntNBRK=12;
                    ntZEROWIDTH=13;

                    // Opcodes
                    opSINGLECHAR=0;
                    opCHAR=1;
                    opANY=2;
                    opMATCH=3;
                    opJMP=4;
                    opSPLIT=5;
                    opSAVE=6;
                    opBOL=7;
                    opEOL=8;
                    opBRK=9;
                    opNBRK=10;

                    // Qualifier kind
                    qkGREEDY=0;
                    qkLAZY=1;

                    MaxGeneration=$40000000;

              type TCodePointWindow=record
                    private
                     fCodePoints:array[0..3] of TpvUInt32;
                     fOffset:TpvSizeInt;
                     function GetCodePoint(const aOffset:TpvSizeInt):TpvUInt32;
                     procedure SetCodePoint(const aOffset:TpvSizeInt;const aCodePoint:TpvUInt32);
                    public
                     procedure Reset;
                     procedure Advance;
                    public
                     property CodePoints[const aOffset:TpvSizeInt]:TpvUInt32 read GetCodePoint write SetCodePoint; default;
                   end;
             private

              fParent:TpvTextEditor;

              fRegularExpression:TpvUTF8String;

              fFlags:TRegularExpressionFlags;

              fAnchoredRootNode:PRegularExpressionNode;

              fUnanchoredRootNode:PRegularExpressionNode;

              fNodes:TList;

              fGeneration:TpvUInt32;

              fCountParens:TpvInt32;

              fCountSubMatches:TpvInt32;

              fInstructions:TRegularExpressionInstructions;
              fCountInstructions:TpvInt32;

              fAnchoredStartInstruction:PRegularExpressionInstruction;
              fUnanchoredStartInstruction:PRegularExpressionInstruction;
              fReversedStartInstruction:PRegularExpressionInstruction;

              fCharClasses:TPRegularExpressionCharClasses;
              fCountCharClasses:TpvInt32;

              fThreadLists:TRegularExpressionThreadLists;

              fFreeSubMatches:PRegularExpressionSubMatches;
              fAllSubMatches:TList;

              fCodePointWindow:TCodePointWindow;

              fBeginningJump:longbool;
              fBeginningSplit:longbool;
              fBeginningWildCard:longbool;
              fBeginningAnchor:longbool;
              fBeginningWildcardLoop:longbool;

              fDoUnanchoredStart:longbool;

              fNamedGroupStringList:TStringList;

              function NewNode(const aNodeType:TpvInt32;const aLeft,aRight:PRegularExpressionNode;const aValue:TpvInt32):PRegularExpressionNode;
              procedure FreeNode(var aNode:PRegularExpressionNode);
              function AreNodesEqual(aNodeA,aNodeB:PRegularExpressionNode):boolean;
              function AreNodesEqualSafe(aNodeA,aNodeB:PRegularExpressionNode):boolean;
              function Concat(aNodeLeft,aNodeRight:PRegularExpressionNode):PRegularExpressionNode;
              function NewAlt(aNodeLeft,aNodeRight:PRegularExpressionNode):PRegularExpressionNode;
              function NewPlus(aNode:PRegularExpressionNode;aKind:TpvInt32):PRegularExpressionNode;
              function NewStar(aNode:PRegularExpressionNode;aKind:TpvInt32):PRegularExpressionNode;
              function NewQuest(aNode:PRegularExpressionNode;aKind:TpvInt32):PRegularExpressionNode;

              function IsStarNullable(aNode:PRegularExpressionNode):boolean;
              function StarDenull(aNode:PRegularExpressionNode):PRegularExpressionNode;

              procedure Parse;
              procedure Compile;
              function NewSubMatches(const aCount:TpvInt32;const aBitState:TpvUInt32):PRegularExpressionSubMatches; {$ifdef caninline}inline;{$endif}
              procedure DecRef(const aSubMatches:PRegularExpressionSubMatches); {$ifdef caninline}inline;{$endif}
              function IncRef(const aSubMatches:PRegularExpressionSubMatches):PRegularExpressionSubMatches; {$ifdef caninline}inline;{$endif}
              function Update(const aSubMatches:PRegularExpressionSubMatches;const aIndex,aPosition:TpvSizeInt):PRegularExpressionSubMatches; {$ifdef caninline}inline;{$endif}
              function NewThread(const aInstruction:PRegularExpressionInstruction;const aSubMatches:PRegularExpressionSubMatches):TRegularExpressionThread; {$ifdef caninline}inline;{$endif}
              function IsWordChar(const aPosition:TpvInt32):boolean; {$ifdef caninline}inline;{$endif}
              procedure AddThread(const aThreadList:PRegularExpressionThreadList;aInstruction:PRegularExpressionInstruction;aSubMatches:PRegularExpressionSubMatches;const aPosition,aWindowOffset:TpvSizeInt);
             protected
              function SearchMatch(out aCaptures:TRegularExpressionCaptures;const aStartPosition,aUntilExcludingPosition:TpvSizeInt;const aUnanchoredStart:boolean):boolean;
             public
              constructor Create(const aParent:TpvTextEditor;const aRegularExpression:TpvUTF8String;const aFlags:TRegularExpressionFlags=[]);
              destructor Destroy; override;
              class function Escape(const aString:TpvUTF8String):TpvUTF8String; static;
              function MatchNext(out aCaptures:TRegularExpressionCaptures;out aPosition,aLength:TpvSizeInt;const aStartPosition:TpvSizeInt=0;const aUntilExcludingPosition:TpvSizeInt=-1;const aCountTries:TpvSizeInt=-1):boolean;
              function FindNext(out aPosition,aLength:TpvSizeInt;const aStartPosition:TpvSizeInt=0;const aUntilExcludingPosition:TpvSizeInt=-1;const aCountTries:TpvSizeInt=-1):boolean;
              property NamedGroups:TStringList read fNamedGroupStringList;
            end;
            TView=class
             public
              type TAutoIdentOnEnterMode=
                    (
                     None,
                     Copy,
                     Tabs,
                     Spaces
                    );
                   PAutoIdentOnEnterMode=^TAutoIdentOnEnterMode;
                   TBufferItem=record
                    Attribute:TpvUInt32;
                    CodePoint:TpvUInt32;
                   end;
                   PBufferItem=^TBufferItem;
                   TBufferItems=array of TBufferItem;
             private
              fParent:TpvTextEditor;
              fPrevious:TView;
              fNext:TView;
              fAutoIdentOnEnterMode:TAutoIdentOnEnterMode;
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
              fVisualLineCacheMap:TLineCacheMap;
              fBuffer:TBufferItems;
              fMarkState:TMarkState;
              procedure SetVisibleAreaWidth(const aVisibleAreaWidth:TpvSizeInt);
              procedure SetVisibleAreaHeight(const aVisibleAreaHeight:TpvSizeInt);
              procedure SetNonScrollVisibleAreaWidth(const aNonScrollVisibleAreaWidth:TpvSizeInt);
              procedure SetNonScrollVisibleAreaHeight(const aNonScrollVisibleAreaHeight:TpvSizeInt);
              procedure SetLineWrap(const aLineWrap:TpvSizeInt);
              procedure SetLineColumn(const aLineColumn:TLineColumn);
              procedure SetCodePointIndex(const aCodePointIndex:TpvSizeInt);
              function GetMarkStartCodePointIndex:TpvSizeInt;
              procedure SetMarkStartCodePointIndex(const aMarkStartCodePointIndex:TpvSizeInt);
              function GetMarkEndCodePointIndex:TpvSizeInt;
              procedure SetMarkEndCodePointIndex(const aMarkEndCodePointIndex:TpvSizeInt);
             public
              constructor Create(const aParent:TpvTextEditor); reintroduce;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure ClampMarkCodePointIndices;
              procedure EnsureCodePointIndexIsInRange;
              procedure EnsureCursorIsVisible(const aUpdateCursor:boolean=true;const aForceVisibleLines:TpvSizeInt=1);
              procedure UpdateCursor;
              procedure UpdateBuffer;
              procedure MarkAll;
              procedure UnmarkAll;
              procedure SetMarkStart;
              procedure SetMarkEndToHere;
              procedure SetMarkEndUntilHere;
              function HasMarkedRange:boolean;
              function GetMarkedRangeText:TpvUTF8String;
              function DeleteMarkedRange:boolean;
              function CutMarkedRangeText:TpvUTF8String;
              procedure InsertCodePoint(const aCodePoint:TpvUInt32;const aOverwrite:boolean;const aStealIt:boolean=false);
              procedure InsertString(const aCodeUnits:TpvUTF8String;const aOverwrite:boolean;const aStealIt:boolean=false);
              procedure Backspace;
              procedure Paste(const aText:TpvUTF8String);
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
              property AutoIdentOnEnterMode:TAutoIdentOnEnterMode read fAutoIdentOnEnterMode write fAutoIdentOnEnterMode;
              property VisibleAreaWidth:TpvSizeInt read fVisibleAreaWidth write SetVisibleAreaWidth;
              property VisibleAreaHeight:TpvSizeInt read fVisibleAreaHeight write SetVisibleAreaHeight;
              property NonScrollVisibleAreaWidth:TpvSizeInt read fNonScrollVisibleAreaWidth write SetNonScrollVisibleAreaWidth;
              property NonScrollVisibleAreaHeight:TpvSizeInt read fNonScrollVisibleAreaHeight write SetNonScrollVisibleAreaHeight;
              property LineWrap:TpvSizeInt read fLineWrap write SetLineWrap;
              property CodePointIndex:TpvSizeInt read fCodePointIndex write SetCodePointIndex;
              property MarkStartCodePointIndex:TpvSizeInt read GetMarkStartCodePointIndex write SetMarkStartCodePointIndex;
              property MarkEndCodePointIndex:TpvSizeInt read GetMarkEndCodePointIndex write SetMarkEndCodePointIndex;
            end;
       const EmptyMarkState:TMarkState=(
              StartCodePointIndex:-1;
              EndCodePointIndex:-1;
             );
      private
       fRope:TRope;
       fLineCacheMap:TLineCacheMap;
       fFirstView:TView;
       fLastView:TView;
       fUndoRedoManager:TUndoRedoManager;
       fSyntaxHighlighting:TSyntaxHighlighting;
       fCountLines:TpvSizeInt;
       function GetCountLines:TpvSizeInt;
       function GetText:TpvUTF8String;
       procedure SetText(const aText:TpvUTF8String);
       function GetLine(const aLineIndex:TpvSizeInt):TpvUTF8String;
       procedure SetLine(const aLineIndex:TpvSizeInt;const aLine:TpvUTF8String);
       procedure SetSyntaxHighlighting(const aSyntaxHighlighting:TpvTextEditor.TSyntaxHighlighting);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       function IsCodePointNewLine(const aCodePointIndex:TpvSizeInt):boolean;
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
       procedure ResetLineCacheMaps;
       procedure ResetViewCodePointIndices;
       procedure ResetViewMarkCodePointIndices;
       procedure ClampViewMarkCodePointIndices;
       procedure UpdateViewCodePointIndices(const aCodePointIndex,aDelta:TpvSizeInt);
       procedure EnsureViewCodePointIndicesAreInRange;
       procedure EnsureViewCursorsAreVisible(const aUpdateCursors:boolean=true;const aForceVisibleLines:TpvSizeInt=1);
       procedure UpdateViewCursors;
       procedure Undo(const aView:TView=nil);
       procedure Redo(const aView:TView=nil);
      public
       property Lines[const aLineIndex:TpvSizeInt]:TpvUTF8String read GetLine write SetLine; default;
      published
       property Text:TpvUTF8String read GetText write SetText;
       property CountLines:TpvSizeInt read GetCountLines;
       property UndoRedoManager:TUndoRedoManager read fUndoRedoManager;
       property SyntaxHighlighting:TSyntaxHighlighting read fSyntaxHighlighting write SetSyntaxHighlighting;
     end;

implementation

{$ifdef TpvTextEditorUsePUCU}
uses PUCU;
{$endif}

function PopFirstOneBit(var Value:TpvUInt32):TpvUInt32;{$ifdef cpu386}assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
 push esi
 mov esi,Value
 xor eax,eax
 bsf ecx,dword ptr [esi]
 jz @Found
 xor eax,ecx
 xor edx,edx
 inc edx
 shl edx,cl
 xor dword ptr [esi],edx
 @Found:
 pop esi
end;
{$else}
{$ifdef cpuamd64}assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
{$ifdef win64}
 mov eax,dword ptr [rcx]
{$else}
 mov eax,dword ptr [rdi]
{$endif}
 lea edx,[eax-1]
 bsf eax,eax
{$ifdef win64}
 and dword ptr [rcx],edx
{$else}
 and dword ptr [rdi],edx
{$endif}
end;
{$else}
begin
{$ifdef fpc}
 result:=BsfByte(Value);
{$else}
 result:=(Value and (-Value))-1;
 result:=result-((result shr 1) and $55555555);
 result:=(result and $33333333)+((result shr 2) and $33333333);
 result:=(result+(result shr 4)) and $0f0f0f0f;
 inc(result,result shr 8);
 inc(result,result shr 16);
 result:=result and $1f;
{$endif}
 Value:=Value and (Value-1);
end;
{$endif}
{$endif}

class function TpvTextEditor.TUTF8Utils.UTF32CharToUTF8(const aCodePoint:TpvUInt32):TpVUTF8String;
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

class function TpvTextEditor.TUTF8Utils.UTF8Validate(const aString:TpvUTF8String):boolean;
var Index:TpvSizeInt;
    State:TpvUInt32;
begin
 State:=TUTF8DFA.StateAccept;
 for Index:=1 to length(aString) do begin
  State:=TUTF8DFA.StateTransitions[State+TUTF8DFA.StateCharClasses[aString[Index]]];
  if State=TUTF8DFA.StateError then begin
   break;
  end;
 end;
 result:=State=TUTF8DFA.StateAccept;
end;

class function TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(const aString:TpvUTF8String;var aCodeUnit:TpvSizeInt):TpvUInt32;
var Len,StartCodeUnit:TpvSizeInt;
    Value,CharClass,State:TpvUInt32;
begin
 result:=0;
 Len:=length(aString);
 if (aCodeUnit>0) and (aCodeUnit<=Len) then begin
  StartCodeUnit:=aCodeUnit;
  State:=TpvTextEditor.TUTF8DFA.StateAccept;
  repeat
   Value:=TpvUInt8(TpvRawByteChar(aString[aCodeUnit]));
   inc(aCodeUnit);
   CharClass:=TpvTextEditor.TUTF8DFA.StateCharClasses[TpvRawByteChar(Value)];
   if State=TpvTextEditor.TUTF8DFA.StateAccept then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=TpvTextEditor.TUTF8DFA.StateTransitions[State+CharClass];
  until (State<=TpvTextEditor.TUTF8DFA.StateError) or (aCodeUnit>Len);
  if State<>TpvTextEditor.TUTF8DFA.StateAccept then begin
   result:=TpvUInt8(TpvRawByteChar(aString[StartCodeUnit]));
   aCodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

class function TpvTextEditor.TUTF8Utils.UTF8Correct(const aString:TpvUTF8String):TpvUTF8String;
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
   State:=TUTF8DFA.StateAccept;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=ord(aString[CodeUnit]);
    inc(CodeUnit);
    CharClass:=TUTF8DFA.StateCharClasses[AnsiChar(UInt8(Value))];
    if State=TUTF8DFA.StateAccept then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=TUTF8DFA.StateTransitions[State+CharClass];
    if State<=TUTF8DFA.StateError then begin
     break;
    end;
   end;
   if State<>TUTF8DFA.StateAccept then begin
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

class function TpvTextEditor.TUTF8Utils.RawDataToUTF8String(const aData;const aDataLength:TpvInt32;const aCodePage:TpvInt32=-1):TpvUTF8String;
type TBytes=array[0..65535] of TpvUInt8;
     PBytes=^TBytes;
var Bytes:PBytes;
    BytesPerCodeUnit,BytesPerCodeUnitMask,StartCodeUnit,CodeUnit,
    InputLen,OutputLen:TpvSizeInt;
    LittleEndianBigEndian,PassIndex,CodePoint,Temp:TpvUInt32;
    State,CharClass,Value:TpvUInt8;
{$ifdef TpvTextEditorUsePUCU}
    CodePage:PPUCUCharSetCodePage;
    SubCodePages:PPUCUCharSetSubCodePages;
    SubSubCodePages:PPUCUCharSetSubSubCodePages;
{$endif}
begin
{$ifdef TpvTextEditorUsePUCU}
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
{$endif}
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
{$ifdef TpvTextEditorUsePUCU}
 end else if assigned(CodePage) then begin
  // Code page
  BytesPerCodeUnit:=0;
  BytesPerCodeUnitMask:=0;
  LittleEndianBigEndian:=0;
  Bytes:=@Bytes^[0];
  InputLen:=aDataLength;
{$endif}
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
      State:=TUTF8DFA.StateAccept;
      repeat
       Value:=ord(Bytes^[CodeUnit]);
       inc(CodeUnit);
       CharClass:=TUTF8DFA.StateCharClasses[AnsiChar(TpvUInt8(Value))];
       if State=TUTF8DFA.StateAccept then begin
        CodePoint:=Value and ($ff shr CharClass);
       end else begin
        CodePoint:=(CodePoint shl 6) or (Value and $3f);
       end;
       State:=TUTF8DFA.StateTransitions[State+CharClass];
      until (State<=TUTF8DFA.StateError) or (CodeUnit>=InputLen);
      if State<>TUTF8DFA.StateAccept then begin
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
{$ifdef TpvTextEditorUsePUCU}
     if assigned(CodePage) then begin
      CodePoint:=CodePage^[CodePoint and $ff];
     end;
{$endif}
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

class function TpvTextEditor.TUTF8Utils.RawByteStringToUTF8String(const aString:TpvRawByteString;const aCodePage:TpvInt32=-1):TpvUTF8String;
var p:PAnsiChar;
begin
 if length(aString)>0 then begin
  p:=PAnsiChar(@aString[1]);
  result:=RawDataToUTF8String(p^,length(aString),aCodePage);
 end else begin
  result:='';
 end;
end;

class function TpvTextEditor.TUTF8Utils.RawStreamToUTF8String(const aStream:TStream;const aCodePage:TpvInt32=-1):TpvUTF8String;
var Memory:pointer;
    Size:TpvSizeInt;
begin
 result:='';
 if assigned(aStream) and (aStream.Seek(0,soBeginning)=0) then begin
  Size:=aStream.Size;
  GetMem(Memory,Size);
  try
   if aStream.Read(Memory^,Size)=Size then begin
    result:=RawDataToUTF8String(Memory^,Size,aCodePage);
   end;
  finally
   FreeMem(Memory);
  end;
 end;
end;

constructor TpvTextEditor.TRope.TNode.Create(const aHeight:TpvInt32);
begin
 inherited Create;
 fHeight:=aHeight;
 fCountCodeUnits:=0;
 SetLength(fLinks,MaximumHeight+1);
 fLinks[0].fNode:=nil;
 fLinks[0].fSkipSize:=0;
end;

destructor TpvTextEditor.TRope.TNode.Destroy;
begin
 inherited Destroy;
end;

function TpvTextEditor.TRope.TNode.GetData:TpvUTF8String;
begin
 SetString(result,PAnsiChar(@fData[0]),fCountCodeUnits);
end;

constructor TpvTextEditor.TRope.TNodeEnumerator.Create(const aCodeUnitsRope:TRope);
begin
 fRope:=aCodeUnitsRope;
 fFirst:=true;
 fNode:=fRope.fHead;
end;

function TpvTextEditor.TRope.TNodeEnumerator.GetCurrent:TNode;
begin
 result:=fNode;
end;

function TpvTextEditor.TRope.TNodeEnumerator.MoveNext:boolean;
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

constructor TpvTextEditor.TRope.TCodePointEnumerator.Create(const aCodeUnitsRope:TRope;const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1);
begin
 fRope:=aCodeUnitsRope;
 fFirst:=true;
 fRope.GetNodeAndOffsetFromCodePointIndex(aStartCodePointIndex,fNode,fNodeCodeUnitIndex);
 fCodePointIndex:=aStartCodePointIndex;
 fStopCodePointIndex:=aStopCodePointIndex;
 fUTF8DFAState:=TUTF8DFA.StateAccept;
 fCodePoint:=0;
end;

function TpvTextEditor.TRope.TCodePointEnumerator.GetCurrent:TpvUInt32;
begin
 result:=fCodePoint;
end;

function TpvTextEditor.TRope.TCodePointEnumerator.CanMoveNext:boolean;
begin
 result:=assigned(fNode) and
         ((fStopCodePointIndex<0) or
          (fCodePointIndex<fStopCodePointIndex));
end;

function TpvTextEditor.TRope.TCodePointEnumerator.MoveNext:boolean;
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
    fUTF8DFACharClass:=TUTF8DFA.StateCharClasses[CodeUnit];
    case fUTF8DFAState of
     TUTF8DFA.StateAccept..TUTF8DFA.StateError:begin
      fCodePoint:=ord(CodeUnit) and ($ff shr fUTF8DFACharClass);
     end;
     else begin
      fCodePoint:=(fCodePoint shl 6) or (ord(CodeUnit) and $3f);
     end;
    end;
    fUTF8DFAState:=TUTF8DFA.StateTransitions[fUTF8DFAState+fUTF8DFACharClass];
    if fUTF8DFAState<=TUTF8DFA.StateError then begin
     if fUTF8DFAState<>TUTF8DFA.StateAccept then begin
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

constructor TpvTextEditor.TRope.TCodePointEnumeratorSource.Create(const aCodeUnitsRope:TRope;const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1);
begin
 fRope:=aCodeUnitsRope;
 fStartCodePointIndex:=aStartCodePointIndex;
 fStopCodePointIndex:=aStopCodePointIndex;
end;

function TpvTextEditor.TRope.TCodePointEnumeratorSource.GetEnumerator:TRope.TCodePointEnumerator;
begin
 result:=TRope.TCodePointEnumerator.Create(fRope,fStartCodePointIndex,fStopCodePointIndex);
end;

constructor TpvTextEditor.TRope.TRandomGenerator.Create(const aSeed:TpvUInt64);
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

function TpvTextEditor.TRope.TRandomGenerator.GetUInt32:TpvUInt32;
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

function TpvTextEditor.TRope.TRandomGenerator.GetUInt64:TpvUInt64;
begin
 result:=(TpvUInt64(GetUInt32) shl 32) or
         (TpvUInt64(GetUInt32) shl 0);
end;

constructor TpvTextEditor.TRope.Create;
begin
 inherited Create;
 fCountCodePoints:=0;
 fCountCodeUnits:=0;
 fHead:=TNode.Create(TNode.MaximumHeight);
 fHead.fHeight:=1;
 fRandomGenerator:=TRandomGenerator.Create(TpvPtrUInt(self));
end;

constructor TpvTextEditor.TRope.Create(const aCodeUnits:TpvUTF8String);
begin
 Create;
 SetText(aCodeUnits);
end;

constructor TpvTextEditor.TRope.Create(const aFrom:TRope);
begin
 Create(aFrom.GetText);
end;

destructor TpvTextEditor.TRope.Destroy;
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

procedure TpvTextEditor.TRope.Clear;
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

function TpvTextEditor.TRope.GetText:TpvUTF8String;
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

procedure TpvTextEditor.TRope.SetText(const aCodeUnits:TpvUTF8String);
begin
 Clear;
 Insert(0,aCodeUnits);
end;

{$ifdef fpc}
class function TpvTextEditor.TRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32;
begin
 if aValue=0 then begin
  result:=255;
 end else begin
  result:=BSFQWord(aValue);
 end;
end;
{$else}
{$if defined(cpu386)}
class function TpvTextEditor.TRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32; assembler; stdcall; {$ifdef fpc}nostackframe;{$endif}
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
class function TpvTextEditor.TRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32; assembler; register; {$ifdef fpc}nostackframe;{$endif}
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
class function TpvTextEditor.TRope.FindFirstSetBit(aValue:TpvUInt64):TpvUInt32;
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

function TpvTextEditor.TRope.GetRandomHeight:TpvInt32;
begin
 result:=FindFirstSetBit(not (fRandomGenerator.GetUInt64 and TNode.MaximumHeightMinusTwoBitMask))+1;
 if result>TNode.MaximumHeightMinusOne then begin
  result:=TNode.MaximumHeightMinusOne;
 end;
end;

class function TpvTextEditor.TRope.GetCountCodeUnits(const aCodeUnits:PAnsiChar;const aCountCodePoints:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=0;
 Index:=0;
 while Index<aCountCodePoints do begin
  inc(result,TUTF8DFA.CodePointSizes[aCodeUnits[result]]);
  inc(Index);
 end;
end;

class function TpvTextEditor.TRope.GetCountCodePoints(const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=0;
 Index:=0;
 while Index<aCountCodeUnits do begin
  inc(Index,TUTF8DFA.CodePointSizes[aCodeUnits[Index]]);
  inc(result);
 end;
end;

class procedure TpvTextEditor.TRope.UTF8Check(const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt);
var Index:TpvSizeInt;
    State:TpvUInt32;
begin
 State:=TUTF8DFA.StateAccept;
 for Index:=0 to aCountCodeUnits-1 do begin
  State:=TUTF8DFA.StateTransitions[State+TUTF8DFA.StateCharClasses[aCodeUnits[Index]]];
  if State=TUTF8DFA.StateError then begin
   break;
  end;
 end;
 if State<>TUTF8DFA.StateAccept then begin
  raise ERope.Create('Invalid UTF8');
 end;
end;

function TpvTextEditor.TRope.FindNodePositionAtCodePoint(const aCodePointIndex:TpvSizeInt;out aNodePositionLinks:TNode.TNodePositionLinks):TNode;
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

function TpvTextEditor.TRope.GetNodeAndOffsetFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aNode:TNode;out aNodeCodeUnitIndex:TpvSizeInt):boolean;
var NodePositionLinks:TRope.TNode.TNodePositionLinks;
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
    aNodeCodeUnitIndex:=TRope.GetCountCodeUnits(@aNode.fData[0],NodePositionLinks[0].fSkipSize);
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

procedure TpvTextEditor.TRope.UpdateOffsetList(var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
var Index:TpvInt32;
begin
 for Index:=0 to fHead.fHeight-1 do begin
  inc(aNodePositionLinks[Index].fNode.fLinks[Index].fSkipSize,aCountCodePoints);
 end;
end;

procedure TpvTextEditor.TRope.InsertAt(var aNodePositionLinks:TNode.TNodePositionLinks;const aCodeUnits:PAnsiChar;const aCountCodeUnits,aCountCodePoints:TpvSizeInt);
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

procedure TpvTextEditor.TRope.InsertAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt);
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
    CodePointSize:=TUTF8DFA.CodePointSizes[aCodeUnits[StringOffset+CountNewNodeCodeUnits]];
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

procedure TpvTextEditor.TRope.DeleteAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
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

function TpvTextEditor.TRope.ExtractAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt):TpvUTF8String;
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

procedure TpvTextEditor.TRope.Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:PAnsiChar;const aCountCodeUnits:TpvSizeInt);
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

procedure TpvTextEditor.TRope.Insert(const aCodePointIndex:TpvSizeInt;const aCodeUnits:TpvUTF8String);
begin
 Insert(aCodePointIndex,PAnsiChar(aCodeUnits),length(aCodeUnits));
end;

procedure TpvTextEditor.TRope.Delete(const aCodePointIndex,aCountCodePoints:TpvSizeInt);
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

function TpvTextEditor.TRope.Extract(const aCodePointIndex,aCountCodePoints:TpvSizeInt):TpvUTF8String;
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

function TpvTextEditor.TRope.GetCodePoint(const aCodePointIndex:TpvSizeInt):TpvUInt32;
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
  UTF8DFAState:=TUTF8DFA.StateAccept;
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
    UTF8DFACharClass:=TUTF8DFA.StateCharClasses[CodeUnit];
    case UTF8DFAState of
     TUTF8DFA.StateAccept..TUTF8DFA.StateError:begin
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
    UTF8DFAState:=TUTF8DFA.StateTransitions[UTF8DFAState+UTF8DFACharClass];
   end;
  until false;
  if UTF8DFAState<>TUTF8DFA.StateAccept then begin
   result:=$fffd;
  end;
 end else begin
  result:=32;
 end;
end;

function TpvTextEditor.TRope.GetEnumerator:TNodeEnumerator;
begin
 result:=TNodeEnumerator.Create(self);
end;

function TpvTextEditor.TRope.GetCodePointEnumeratorSource(const aStartCodePointIndex:TpvSizeInt=0;const aStopCodePointIndex:TpvSizeInt=-1):TRope.TCodePointEnumeratorSource;
begin
 result:=TRope.TCodePointEnumeratorSource.Create(self,aStartCodePointIndex,aStopCodePointIndex);
end;

procedure TpvTextEditor.TRope.Check;
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

procedure TpvTextEditor.TRope.Dump;
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

constructor TpvTextEditor.TLineCacheMap.Create(const aRope:TRope);
begin
 inherited Create;
 fRope:=aRope;
 fLines:=nil;
 fCountLines:=0;
 fLineWrap:=0;
 fTabWidth:=8;
 Reset;
 Update(-1,-1);
end;

destructor TpvTextEditor.TLineCacheMap.Destroy;
begin
 fLines:=nil;
 inherited Destroy;
end;

procedure TpvTextEditor.TLineCacheMap.SetLineWrap(const aLineWrap:TpvSizeInt);
begin
 if fLineWrap<>aLineWrap then begin
  fLineWrap:=aLineWrap;
  Reset;
  Update(-1,-1);
 end;
end;

procedure TpvTextEditor.TLineCacheMap.SetTabWidth(const aTabWidth:TpvSizeInt);
begin
 if fTabWidth<>aTabWidth then begin
  fTabWidth:=aTabWidth;
  if fLineWrap>0 then begin
   Reset;
   Update(-1,-1);
  end;
 end;
end;

procedure TpvTextEditor.TLineCacheMap.AddLine(const aCodePointIndex:TpvSizeInt);
begin
 if length(fLines)<(fCountLines+1) then begin
  SetLength(fLines,(fCountLines+1)*2);
 end;
 fLines[fCountLines]:=aCodePointIndex;
 inc(fCountLines);
end;

procedure TpvTextEditor.TLineCacheMap.Reset;
begin
 fCountLines:=0;
 AddLine(0);
 fCodePointIndex:=0;
 fCountVisibleVisualCodePointsSinceNewLine:=0;
 fLastWasPossibleNewLineTwoCharSequence:=false;
 fLastCodePoint:=0;
end;

procedure TpvTextEditor.TLineCacheMap.Truncate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
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

procedure TpvTextEditor.TLineCacheMap.Update(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var CodePoint:TpvUInt32;
    DoStop:TpvInt32;
    DoNewLine,DoTab:boolean;
begin

 if (fCodePointIndex<fRope.fCountCodePoints) and
    ((aUntilCodePoint<0) or (fCodePointIndex<aUntilCodePoint)) and
    ((aUntilLine<0) or (fCountLines<aUntilLine)) then begin

  DoStop:=0;

  for CodePoint in fRope.GetCodePointEnumeratorSource(fCodePointIndex,-1) do begin

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

function TpvTextEditor.TLineCacheMap.GetLineIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt):TpvSizeInt;
var MinIndex,MaxIndex,MidIndex:TpvSizeInt;
begin
 if aCodePointIndex<=fRope.CountCodePoints then begin
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

function TpvTextEditor.TLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aLineIndex,aColumnIndex:TpvSizeInt):boolean;
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

    for CodePoint in fRope.GetCodePointEnumeratorSource(StartCodePointIndex,StopCodePointIndex) do begin

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

    if CurrentCodePointIndex=fRope.CountCodePoints then begin
     inc(aColumnIndex);
    end;

   end else begin

    aColumnIndex:=0;

   end;

   result:=true;

  end;

 end;

end;

function TpvTextEditor.TLineCacheMap.GetCodePointIndexFromLineIndex(const aLineIndex:TpvSizeInt):TpvSizeInt;
begin
 Update(-1,aLineIndex+1);
 if (aLineIndex>=0) and (aLineIndex<fCountLines) then begin
  result:=fLines[aLineIndex];
 end else begin
  result:=-1;
 end;
end;

function TpvTextEditor.TLineCacheMap.GetCodePointIndexFromNextLineIndexOrTextEnd(const aLineIndex:TpvSizeInt):TpvSizeInt;
begin
 Update(-1,aLineIndex+2);
 if (aLineIndex>=0) and (aLineIndex<fCountLines) then begin
  if (aLineIndex+1)<fCountLines then begin
   result:=fLines[aLineIndex+1];
  end else begin
   result:=fRope.CountCodePoints;
  end;
 end else begin
  result:=-1;
 end;
end;

function TpvTextEditor.TLineCacheMap.GetCodePointIndexFromLineIndexAndColumnIndex(const aLineIndex,aColumnIndex:TpvSizeInt):TpvSizeInt;
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
   StopCodePointIndex:=fRope.CountCodePoints;
  end;

  CurrentColumn:=0;

  CurrentCodePointIndex:=StartCodePointIndex;

  LastCodePoint:=0;

  LastWasPossibleNewLineTwoCharSequence:=false;

  if StartCodePointIndex<StopCodePointIndex then begin

   for CodePoint in fRope.GetCodePointEnumeratorSource(StartCodePointIndex,StopCodePointIndex) do begin

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

   if (CurrentColumn<=aColumnIndex) and (CurrentCodePointIndex=fRope.CountCodePoints) then begin
    result:=fRope.CountCodePoints;
   end;

  end;

 end else begin

  result:=-1;

 end;

end;

constructor TpvTextEditor.TUndoRedoCommand.Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState);
begin
 inherited Create;
 fParent:=aParent;
 fUndoCursorCodePointIndex:=aUndoCursorCodePointIndex;
 fRedoCursorCodePointIndex:=aRedoCursorCodePointIndex;
 fUndoMarkState:=aUndoMarkState;
 fRedoMarkstate:=aRedoMarkState;
 fSealed:=false;
 fActionID:=aParent.fUndoRedoManager.fActionID;
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

constructor TpvTextEditor.TUndoRedoCommandInsert.Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState;const aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String);
begin
 inherited Create(aParent,aUndoCursorCodePointIndex,aRedoCursorCodePointIndex,aUndoMarkState,aRedoMarkState);
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
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fRope.Delete(fCodePointIndex,fCountCodePoints);
 fParent.UpdateViewCodePointIndices(fCodePointIndex,-fCountCodePoints);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fUndoCursorCodePointIndex;
  aView.fMarkState:=fUndoMarkState;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TUndoRedoCommandInsert.Redo(const aView:TpvTextEditor.TView=nil);
begin
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fRope.Insert(fCodePointIndex,fCodeUnits);
 fParent.UpdateViewCodePointIndices(fCodePointIndex,fCountCodePoints);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fRedoCursorCodePointIndex;
  aView.fMarkState:=fRedoMarkState;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

constructor TpvTextEditor.TUndoRedoCommandOverwrite.Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState;const aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits,aPreviousCodeUnits:TpvUTF8String);
begin
 inherited Create(aParent,aUndoCursorCodePointIndex,aRedoCursorCodePointIndex,aUndoMarkState,aRedoMarkState);
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
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fRope.Delete(fCodePointIndex,fCountCodePoints);
 fParent.fRope.Insert(fCodePointIndex,fPreviousCodeUnits);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fUndoCursorCodePointIndex;
  aView.fMarkState:=fUndoMarkState;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TUndoRedoCommandOverwrite.Redo(const aView:TpvTextEditor.TView=nil);
begin
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fRope.Delete(fCodePointIndex,fCountCodePoints);
 fParent.fRope.Insert(fCodePointIndex,fCodeUnits);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fRedoCursorCodePointIndex;
  aView.fMarkState:=fRedoMarkState;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

constructor TpvTextEditor.TUndoRedoCommandDelete.Create(const aParent:TpvTextEditor;const aUndoCursorCodePointIndex,aRedoCursorCodePointIndex:TpvSizeInt;const aUndoMarkState,aRedoMarkState:TMarkState;const aCodePointIndex,aCountCodePoints:TpvSizeInt;const aCodeUnits:TpvUTF8String);
begin
 inherited Create(aParent,aUndoCursorCodePointIndex,aRedoCursorCodePointIndex,aUndoMarkState,aRedoMarkState);
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
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fRope.Insert(fCodePointIndex,fCodeUnits);
 fParent.UpdateViewCodePointIndices(fCodePointIndex,fCountCodePoints);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fUndoCursorCodePointIndex;
  aView.fMarkState:=fUndoMarkState;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

procedure TpvTextEditor.TUndoRedoCommandDelete.Redo(const aView:TpvTextEditor.TView=nil);
begin
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 fParent.LineMapTruncate(fCodePointIndex,-1);
 fParent.fRope.Delete(fCodePointIndex,fCountCodePoints);
 fParent.UpdateViewCodePointIndices(fCodePointIndex,-fCountCodePoints);
 if assigned(aView) then begin
  aView.fCodePointIndex:=fRedoCursorCodePointIndex;
  aView.fMarkState:=fRedoMarkState;
 end;
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
end;

constructor TpvTextEditor.TUndoRedoCommandGroup.Create(const aParent:TpvTextEditor;const aClass:TUndoRedoCommandClass);
begin
 inherited Create(aParent,0,0,EmptyMarkState,EmptyMarkState);
 fClass:=aClass;
 fList:=TObjectList.Create;
 fList.OwnsObjects:=true;
end;

destructor TpvTextEditor.TUndoRedoCommandGroup.Destroy;
begin
 fList.Free;
 inherited Destroy;
end;

procedure TpvTextEditor.TUndoRedoCommandGroup.Undo(const aView:TpvTextEditor.TView=nil);
var Index:TpvSizeInt;
begin
 for Index:=fList.Count-1 downto 0 do begin
  TUndoRedoCommand(fList[Index]).Undo(aView);
 end;
end;

procedure TpvTextEditor.TUndoRedoCommandGroup.Redo(const aView:TpvTextEditor.TView=nil);
var Index:TpvSizeInt;
begin
 for Index:=0 to fList.Count-1 do begin
  TUndoRedoCommand(fList[Index]).Redo(aView);
 end;
end;

constructor TpvTextEditor.TUndoRedoManager.Create(const aParent:TpvTextEditor);
begin
 inherited Create;
 OwnsObjects:=true;
 fParent:=aParent;
 fHistoryIndex:=-1;
 fMaxUndoSteps:=-1;
 fMaxRedoSteps:=-1;
 fActionID:=0;
end;

destructor TpvTextEditor.TUndoRedoManager.Destroy;
begin
 inherited Destroy;
end;

procedure TpvTextEditor.TUndoRedoManager.Clear;
begin
 inherited Clear;
 fHistoryIndex:=-1;
 fActionID:=0;
end;

procedure TpvTextEditor.TUndoRedoManager.IncreaseActionID;
begin
 inc(fActionID);
end;

procedure TpvTextEditor.TUndoRedoManager.Add(const aUndoRedoCommand:TpvTextEditor.TUndoRedoCommand);
var Index:TpvSizeInt;
    UndoRedoCommand:TpvTextEditor.TUndoRedoCommand;
    UndoRedoCommandGroup:TpvTextEditor.TUndoRedoCommandGroup;
begin
 if (fHistoryIndex>=0) and (fHistoryIndex<Count) then begin
  UndoRedoCommand:=TpvTextEditor.TUndoRedoCommand(Items[fHistoryIndex]);
  if (UndoRedoCommand.fActionID=fActionID) and not UndoRedoCommand.fSealed then begin
   if UndoRedoCommand is TpvTextEditor.TUndoRedoCommandGroup then begin
    UndoRedoCommandGroup:=TpvTextEditor.TUndoRedoCommandGroup(UndoRedoCommand);
    if aUndoRedoCommand is UndoRedoCommandGroup.fClass then begin
     UndoRedoCommandGroup.fList.Add(aUndoRedoCommand);
     exit;
    end;
   end else if UndoRedoCommand is aUndoRedoCommand.ClassType then begin
    if UndoRedoCommand is TpvTextEditor.TUndoRedoCommandInsert then begin
     if (TpvTextEditor.TUndoRedoCommandInsert(UndoRedoCommand).fCodePointIndex+TpvTextEditor.TUndoRedoCommandInsert(UndoRedoCommand).fCountCodePoints)=TpvTextEditor.TUndoRedoCommandInsert(aUndoRedoCommand).fCodePointIndex then begin
      inc(TpvTextEditor.TUndoRedoCommandInsert(UndoRedoCommand).fCountCodePoints,TpvTextEditor.TUndoRedoCommandInsert(aUndoRedoCommand).fCountCodePoints);
      TpvTextEditor.TUndoRedoCommandInsert(UndoRedoCommand).fCodeUnits:=TpvTextEditor.TUndoRedoCommandInsert(UndoRedoCommand).fCodeUnits+TpvTextEditor.TUndoRedoCommandInsert(aUndoRedoCommand).fCodeUnits;
      TpvTextEditor.TUndoRedoCommandInsert(UndoRedoCommand).fRedoCursorCodePointIndex:=TpvTextEditor.TUndoRedoCommandInsert(aUndoRedoCommand).fRedoCursorCodePointIndex;
      exit;
     end;
    end else if UndoRedoCommand is TpvTextEditor.TUndoRedoCommandDelete then begin
     if (TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCodePointIndex-TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCountCodePoints)=TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCodePointIndex then begin
      dec(TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCodePointIndex,TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCountCodePoints);
      inc(TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCountCodePoints,TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCountCodePoints);
      TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCodeUnits:=TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCodeUnits+TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCodeUnits;
      TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fRedoCursorCodePointIndex:=TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fRedoCursorCodePointIndex;
      exit;
     end else if TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCodePointIndex=TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCodePointIndex then begin
      inc(TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCountCodePoints,TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCountCodePoints);
      TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCodeUnits:=TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fCodeUnits+TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fCodeUnits;
      TpvTextEditor.TUndoRedoCommandDelete(UndoRedoCommand).fRedoCursorCodePointIndex:=TpvTextEditor.TUndoRedoCommandDelete(aUndoRedoCommand).fRedoCursorCodePointIndex;
      exit;
     end;
    end else begin
{$ifdef fpc}
     Extract(UndoRedoCommand);
{$else}
     ExtractItem(UndoRedoCommand,TDirection.FromEnd);
{$endif}
     UndoRedoCommandGroup:=TpvTextEditor.TUndoRedoCommandGroup.Create(fParent,TpvTextEditor.TUndoRedoCommandClass(aUndoRedoCommand.ClassType));
     Insert(fHistoryIndex,UndoRedoCommandGroup);
     UndoRedoCommandGroup.fList.Add(UndoRedoCommand);
     UndoRedoCommandGroup.fList.Add(aUndoRedoCommand);
     exit;
    end;
   end;
  end;
 end;
 for Index:=Count-1 downto fHistoryIndex+1 do begin
  Delete(Index);
 end;
 if fMaxUndoSteps>0 then begin
  while Count>=fMaxUndoSteps do begin
   Delete(0);
  end;
 end;
 fHistoryIndex:=inherited Add(aUndoRedoCommand);
end;

procedure TpvTextEditor.TUndoRedoManager.GroupUndoRedoCommands(const aFromIndex,aToIndex:TpvSizeInt);
var Index:TpvSizeInt;
    UndoRedoCommand:TpvTextEditor.TUndoRedoCommand;
    UndoRedoCommandGroup:TpvTextEditor.TUndoRedoCommandGroup;
begin
 if ((aFromIndex>=0) and (aFromIndex<Count)) and
    ((aToIndex>=0) and (aToIndex<Count)) then begin
  UndoRedoCommandGroup:=TpvTextEditor.TUndoRedoCommandGroup.Create(fParent,TUndoRedoCommand);
  UndoRedoCommandGroup.fSealed:=true;
  Insert(aFromIndex,UndoRedoCommandGroup);
  for Index:=aFromIndex to aToIndex do begin
   UndoRedoCommand:=TpvTextEditor.TUndoRedoCommand(Items[aFromIndex+1]);
{$ifdef fpc}
   Extract(UndoRedoCommand);
{$else}
   ExtractItem(UndoRedoCommand,TDirection.FromEnd);
{$endif}
   UndoRedoCommandGroup.fList.Add(UndoRedoCommand);
  end;
 end;
end;

procedure TpvTextEditor.TUndoRedoManager.Undo(const aView:TpvTextEditor.TView=nil);
var UndoRedoCommand:TpvTextEditor.TUndoRedoCommand;
begin
 if (fHistoryIndex>=0) and (fHistoryIndex<Count) then begin
  UndoRedoCommand:=TpvTextEditor.TUndoRedoCommand(Items[fHistoryIndex]);
  UndoRedoCommand.fSealed:=true;
  UndoRedoCommand.Undo(aView);
  dec(fHistoryIndex);
  if fMaxRedoSteps>0 then begin
   while (fHistoryIndex+fMaxRedoSteps)<Count do begin
    Delete(Count-1);
   end;
  end;
  IncreaseActionID;
 end;
end;

procedure TpvTextEditor.TUndoRedoManager.Redo(const aView:TpvTextEditor.TView=nil);
var UndoRedoCommand:TpvTextEditor.TUndoRedoCommand;
begin
 if (fHistoryIndex>=(-1)) and ((fHistoryIndex+1)<Count) then begin
  inc(fHistoryIndex);
  UndoRedoCommand:=TpvTextEditor.TUndoRedoCommand(Items[fHistoryIndex]);
  UndoRedoCommand.fSealed:=true;
  UndoRedoCommand.Redo(aView);
  if fMaxUndoSteps>0 then begin
   while fHistoryIndex>fMaxUndoSteps do begin
    dec(fHistoryIndex);
    Delete(0);
   end;
  end;
  IncreaseActionID;
 end;
end;

constructor TpvTextEditor.Create;
begin
 inherited Create;
 fRope:=TRope.Create;
 fLineCacheMap:=TLineCacheMap.Create(fRope);
 fFirstView:=nil;
 fLastView:=nil;
 fUndoRedoManager:=TUndoRedoManager.Create(self);
 fSyntaxHighlighting:=nil;
 fCountLines:=-1;
end;

destructor TpvTextEditor.Destroy;
begin
 while assigned(fLastView) do begin
  fLastView.Free;
 end;
 fLineCacheMap.Free;
 fRope.Free;
 fUndoRedoManager.Free;
 fSyntaxHighlighting.Free;
 inherited Destroy;
end;

function TpvTextEditor.GetCountLines:TpvSizeInt;
begin
 if fCountLines<0 then begin
  fLineCacheMap.Update(-1,-1);
  fCountLines:=fLineCacheMap.fCountLines;
 end;
 result:=fCountLines;
end;

function TpvTextEditor.IsCodePointNewLine(const aCodePointIndex:TpvSizeInt):boolean;
var CodePoint:TpvUInt32;
begin
 result:=false;
 for CodePoint in fRope.GetCodePointEnumeratorSource(aCodePointIndex,aCodePointIndex+1) do begin
  case CodePoint of
   $0a,$0d:begin
    result:=true;
   end;
   else begin
    break;
   end;
  end;
 end;
end;

function TpvTextEditor.IsTwoCodePointNewLine(const aCodePointIndex:TpvSizeInt):boolean;
var CodePoint,LastCodePoint:TpvUInt32;
    LastWasPossibleNewLineTwoCharSequence:boolean;
begin
 result:=false;
 LastCodePoint:=0;
 LastWasPossibleNewLineTwoCharSequence:=false;
 for CodePoint in fRope.GetCodePointEnumeratorSource(aCodePointIndex,aCodePointIndex+2) do begin
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
 fUndoRedoManager.Clear;
 if assigned(aStream) then begin
  fRope.Text:=TUTF8Utils.RawStreamToUTF8String(aStream);
 end else begin
  fRope.Text:='';
 end;
 if assigned(fSyntaxHighlighting) then begin
  fSyntaxHighlighting.Reset;
 end;
 ResetLineCacheMaps;
 ResetViewCodePointIndices;
 ResetViewMarkCodePointIndices;
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
 fUndoRedoManager.Clear;
 fRope.SetText(TUTF8Utils.RawByteStringToUTF8String(aString));
 if assigned(fSyntaxHighlighting) then begin
  fSyntaxHighlighting.Reset;
 end;
 ResetLineCacheMaps;
 ResetViewCodePointIndices;
 ResetViewMarkCodePointIndices;
end;

procedure TpvTextEditor.SaveToStream(const aStream:TStream);
var TemporaryString:TpvUTF8String;
begin
 if assigned(aStream) then begin
  TemporaryString:=fRope.GetText;
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
 result:=fRope.GetText;
end;

function TpvTextEditor.GetText:TpvUTF8String;
begin
 result:=fRope.GetText;
end;

procedure TpvTextEditor.SetText(const aText:TpvUTF8String);
begin
 fUndoRedoManager.Clear;
 fRope.SetText(aText);
 if assigned(fSyntaxHighlighting) then begin
  fSyntaxHighlighting.Reset;
 end;
 ResetLineCacheMaps;
 ResetViewCodePointIndices;
 ResetViewMarkCodePointIndices;
end;

function TpvTextEditor.GetLine(const aLineIndex:TpvSizeInt):TpvUTF8String;
var StartCodePointIndex,StopCodePointIndex,CodeUnitIndex:TpvSizeInt;
begin
 result:='';
 fLineCacheMap.Update(-1,aLineIndex+2);
 if (aLineIndex>=0) and (aLineIndex<fLineCacheMap.fCountLines) then begin
  StartCodePointIndex:=fLineCacheMap.GetCodePointIndexFromLineIndex(aLineIndex);
  StopCodePointIndex:=fLineCacheMap.GetCodePointIndexFromNextLineIndexOrTextEnd(aLineIndex);
  if (StartCodePointIndex>=0) and
     (StartCodePointIndex<StopCodePointIndex) then begin
   result:=fRope.Extract(StartCodePointIndex,StopCodePointIndex-StartCodePointIndex);
   for CodeUnitIndex:=length(result) downto 1 do begin
    if not (result[CodeUnitIndex] in [AnsiChar(#10),AnsiChar(#13)]) then begin
     result:=Copy(result,1,CodeUnitIndex+1);
     break;
    end;
   end;
   exit;
  end;
 end;
 raise ERangeError.Create('Line index out of bounds');
end;

procedure TpvTextEditor.SetLine(const aLineIndex:TpvSizeInt;const aLine:TpvUTF8String);
var StartCodePointIndex,StopCodePointIndex:TpvSizeInt;
begin
 fLineCacheMap.Update(-1,aLineIndex+2);
 if (aLineIndex>=0) and (aLineIndex<=fLineCacheMap.fCountLines) then begin
  StartCodePointIndex:=fLineCacheMap.GetCodePointIndexFromLineIndex(aLineIndex);
  if StartCodePointIndex>=0 then begin
   fUndoRedoManager.Clear;
   StopCodePointIndex:=fLineCacheMap.GetCodePointIndexFromNextLineIndexOrTextEnd(aLineIndex);
   if StartCodePointIndex<StopCodePointIndex then begin
    fRope.Delete(StartCodePointIndex,StopCodePointIndex-StartCodePointIndex);
   end;
   fRope.Insert(StartCodePointIndex,aLine+NewLineCodePointSequence);
   if assigned(fSyntaxHighlighting) then begin
    fSyntaxHighlighting.Truncate(StartCodePointIndex);
   end;
   LineMapTruncate(-1,Max(0,aLineIndex-1));
   exit;
  end;
 end;
 raise ERangeError.Create('Line index out of bounds');
end;

procedure TpvTextEditor.SetSyntaxHighlighting(const aSyntaxHighlighting:TpvTextEditor.TSyntaxHighlighting);
begin
 if fSyntaxHighlighting<>aSyntaxHighlighting then begin
  fSyntaxHighlighting.Free;
  fSyntaxHighlighting:=aSyntaxHighlighting;
 end;
end;

function TpvTextEditor.CreateView:TpvTextEditor.TView;
begin
 result:=TpvTextEditor.TView.Create(self);
end;

procedure TpvTextEditor.LineMapTruncate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var View:TView;
begin
 fLineCacheMap.Truncate(aUntilCodePoint,aUntilLine);
 View:=fFirstView;
 while assigned(View) do begin
  View.fVisualLineCacheMap.Truncate(aUntilCodePoint,aUntilLine);
  View:=View.fNext;
 end;
 fCountLines:=-1;
end;

procedure TpvTextEditor.LineMapUpdate(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var View:TView;
begin
 fLineCacheMap.Update(aUntilCodePoint,aUntilLine);
 View:=fFirstView;
 while assigned(View) do begin
  View.fVisualLineCacheMap.Update(aUntilCodePoint,aUntilLine);
  View:=View.fNext;
 end;
 fCountLines:=-1;
end;

procedure TpvTextEditor.ResetLineCacheMaps;
var View:TView;
begin
 fLineCacheMap.Truncate(0,0);
 fLineCacheMap.Update(-1,-1);
 View:=fFirstView;
 while assigned(View) do begin
  View.fVisualLineCacheMap.Truncate(0,0);
  View.fVisualLineCacheMap.Update(-1,-1);
  View:=View.fNext;
 end;
 fCountLines:=-1;
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

procedure TpvTextEditor.ResetViewMarkCodePointIndices;
var View:TView;
begin
 View:=fFirstView;
 while assigned(View) do begin
  View.fMarkState.StartCodePointIndex:=-1;
  View.fMarkState.EndCodePointIndex:=-1;
  View:=View.fNext;
 end;
end;

procedure TpvTextEditor.ClampViewMarkCodePointIndices;
var View:TView;
begin
 View:=fFirstView;
 while assigned(View) do begin
  View.ClampMarkCodePointIndices;
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
begin
 fUndoRedoManager.Undo(aView);
end;

procedure TpvTextEditor.Redo(const aView:TView=nil);
begin
 fUndoRedoManager.Redo(aView);
end;

constructor TpvTextEditor.TSyntaxHighlighting.Create(const aParent:TpvTextEditor);
begin
 inherited Create;
 fParent:=aParent;
 fStates:=nil;
 fCountStates:=0;
 fCodePointIndex:=0;
 fLevel:=0;
end;

destructor TpvTextEditor.TSyntaxHighlighting.Destroy;
var Index:TpvSizeInt;
begin
 for Index:=0 to length(fStates)-1 do begin
  FreeAndNil(fStates[Index]);
 end;
 fStates:=nil;
 inherited Destroy;
end;

class function TpvTextEditor.TSyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='';
end;

class function TpvTextEditor.TSyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
end;

function TpvTextEditor.TSyntaxHighlighting.GetStateIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt):TpvSizeInt;
var MinIndex,MaxIndex,MidIndex:TpvSizeInt;
begin
 if aCodePointIndex<=fParent.fRope.CountCodePoints then begin
  Update(aCodePointIndex+1);
  MinIndex:=0;
  MaxIndex:=fCountStates-1;
  while MinIndex<MaxIndex do begin
   MidIndex:=MinIndex+((MaxIndex-MinIndex) shr 1);
   if aCodePointIndex<fStates[MidIndex].fCodePointIndex then begin
    MaxIndex:=MidIndex-1;
   end else if aCodePointIndex>=fStates[MidIndex+1].fCodePointIndex then begin
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

procedure TpvTextEditor.TSyntaxHighlighting.Reset;
var Index:TpvSizeInt;
begin
 for Index:=0 to length(fStates)-1 do begin
  FreeAndNil(fStates[Index]);
 end;
 fStates:=nil;
end;

procedure TpvTextEditor.TSyntaxHighlighting.Truncate(const aUntilCodePoint:TpvSizeInt);
var LineIndex,UntilCodePoint:TpvSizeInt;
begin
 if aUntilCodePoint>=0 then begin
  LineIndex:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(aUntilCodePoint);
  if LineIndex>=0 then begin
   UntilCodePoint:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(LineIndex);
  end else begin
   UntilCodePoint:=aUntilCodePoint;
  end;
 end else begin
  UntilCodePoint:=aUntilCodePoint;
 end;
 if UntilCodePoint<0 then begin
  while fCountStates>0 do begin
   dec(fCountStates);
   FreeAndNil(fStates[fCountStates]);
  end;
  fStates:=nil;
  fCodePointIndex:=0;
  fLevel:=0;
 end else begin
  while (fCountStates>0) and
        (fStates[fCountStates-1].fCodePointIndex>=UntilCodePoint) do begin
   dec(fCountStates);
   fCodePointIndex:=fStates[fCountStates].fCodePointIndex;
   fLevel:=fStates[fCountStates].fLevel and $3fffffff;
   FreeAndNil(fStates[fCountStates]);
  end;
 if (fCountStates>0) and
    (fStates[fCountStates-1].fCodePointIndex<UntilCodePoint) then begin
   dec(fCountStates);
   fCodePointIndex:=fStates[fCountStates].fCodePointIndex;
   fLevel:=fStates[fCountStates].fLevel and $3fffffff;
   FreeAndNil(fStates[fCountStates]);
  end;
  if ((fCountStates*8)<length(fStates)) and (fCountStates<(fCountStates*8)) then begin
   SetLength(fStates,fCountStates);
  end;
 end;
end;

procedure TpvTextEditor.TSyntaxHighlighting.Update(const aUntilCodePoint:TpvSizeInt);
begin
end;

class function TpvTextEditor.TSyntaxHighlighting.GetSyntaxHighlightingClassByFileExtension(const aFileExtension:TpvUTF8String):TSyntaxHighlightingClass;
const SyntaxHighlightingClasses:array[0..4] of TSyntaxHighlightingClass=
       (
        TPascalSyntaxHighlighting,
        TCSyntaxHighlighting,
        TCPPSyntaxHighlighting,
        TJavaSyntaxHighlighting,
        TGLSLSyntaxHighlighting
       );
var SyntaxHighlightingClass:TSyntaxHighlightingClass;
    FileExtension:TpvUTF8String;
begin
 for SyntaxHighlightingClass in SyntaxHighlightingClasses do begin
  for FileExtension in SyntaxHighlightingClass.GetFileExtensions do begin
   if FileExtension=aFileExtension then begin
    result:=SyntaxHighlightingClass;
    exit;
   end;
  end;
 end;
 result:=TGenericSyntaxHighlighting;
end;

class function TpvTextEditor.TGenericSyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='Generic';
end;

class function TpvTextEditor.TGenericSyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
end;

procedure TpvTextEditor.TGenericSyntaxHighlighting.Update(const aUntilCodePoint:TpvSizeInt);
var CodePointEnumeratorSource:TpvTextEditor.TRope.TCodePointEnumeratorSource;
    CodePoint,LastLevel,Level,LastAttribute,Attribute:TpvUInt32;
    State:TSyntaxHighlighting.TState;
    OldCount:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fRope.fCountCodePoints then begin
  if fCountStates>0 then begin
   State:=fStates[fCountStates-1];
   LastLevel:=TGenericSyntaxHighlighting.TState(State).fLevel;
   LastAttribute:=TGenericSyntaxHighlighting.TState(State).fAttribute;
  end else begin
   State:=nil;
   LastLevel:=0;
   LastAttribute:=TSyntaxHighlighting.TAttributes.Unknown;
  end;
  CodePointEnumeratorSource:=fParent.fRope.GetCodePointEnumeratorSource(fCodePointIndex,IfThen(aUntilCodePoint<0,aUntilCodePoint,aUntilCodePoint+1));
  for CodePoint in CodePointEnumeratorSource do begin
   case CodePoint of
    0..32:begin
     Attribute:=TSyntaxHighlighting.TAttributes.WhiteSpace;
    end;
    ord('a')..ord('z'),ord('A')..ord('Z'),ord('_'):begin
     case LastAttribute of
      TSyntaxHighlighting.TAttributes.Number:begin
       Attribute:=TSyntaxHighlighting.TAttributes.Number;
      end;
      else begin
       Attribute:=TSyntaxHighlighting.TAttributes.Identifier;
      end;
     end;
    end;
    ord('0')..ord('9'):begin
     case LastAttribute of
      TSyntaxHighlighting.TAttributes.Identifier:begin
       Attribute:=TSyntaxHighlighting.TAttributes.Identifier;
      end;
      else begin
       Attribute:=TSyntaxHighlighting.TAttributes.Number;
      end;
     end;
    end;
    else begin
     Attribute:=TSyntaxHighlighting.TAttributes.Symbol;
    end;
   end;
   case CodePoint of
    ord('('),ord('['),ord('{'):begin
     inc(fLevel);
     Level:=fLevel or $40000000;
    end;
    ord(')'),ord(']'),ord('}'):begin
     Level:=fLevel or $80000000;
    end;
    else begin
     Level:=fLevel;
    end;
   end;
   if (LastLevel<>Level) or
      (LastAttribute<>Attribute) then begin
    LastLevel:=Level;
    LastAttribute:=Attribute;
    OldCount:=length(fStates);
    if OldCount<(fCountStates+1) then begin
     SetLength(fStates,(fCountStates+1)*2);
     FillChar(fStates[OldCount],(length(fStates)-OldCount)*SizeOf(TSyntaxHighlighting.TState),#0);
    end;
    State:=TGenericSyntaxHighlighting.TState.Create;
    fStates[fCountStates]:=State;
    inc(fCountStates);
    TGenericSyntaxHighlighting.TState(State).fCodePointIndex:=fCodePointIndex;
    TGenericSyntaxHighlighting.TState(State).fLevel:=Level;
    TGenericSyntaxHighlighting.TState(State).fAttribute:=Attribute;
   end;
   case CodePoint of
    ord(')'),ord(']'),ord('}'):begin
     if fLevel>0 then begin
      dec(fLevel);
     end;
    end;
   end;
   inc(fCodePointIndex);
  end;
 end;
end;

class function TpvTextEditor.TDFASyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='DFA';
end;

class function TpvTextEditor.TDFASyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
end;

constructor TpvTextEditor.TDFASyntaxHighlighting.TNFASet.Create(const aValues:array of TpvUInt32);
var Value,MaxValue:TpvUInt32;
    WordIndex:TpvSizeInt;
begin
 MaxValue:=0;
 for Value in aValues do begin
  if MaxValue<Value then begin
   MaxValue:=Value;
  end;
 end;
 SetLength(fSet,(MaxValue+32) shr 5);
 if length(fSet)>0 then begin
  FillChar(fSet[0],length(fSet)*SizeOf(TpvUInt32),#0);
 end;
 for Value in aValues do begin
  WordIndex:=Value shr 5;
  fSet[WordIndex]:=fSet[WordIndex] or (TpvUInt32(1) shl (Value and 31));
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.Add(const aSet:TNFASet;const aValue:TpvUInt32):TNFASet;
var WordIndex,OldCount:TpvSizeInt;
begin
 result.fSet:=copy(aSet.fSet);
 WordIndex:=aValue shr 5;
 OldCount:=length(result.fSet);
 if OldCount<=WordIndex then begin
  SetLength(result.fSet,(WordIndex+1)*2);
  FillChar(result.fSet[OldCount],(length(result.fSet)-OldCount)*SizeOf(TpvUInt32),#0);
 end;
 result.fSet[WordIndex]:=result.fSet[WordIndex] or (TpvUInt32(1) shl (aValue and 31));
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.Add(const aSet,aOtherSet:TNFASet):TNFASet;
var Index:TpvSizeInt;
    WordValue:TpvUInt32;
begin
 SetLength(result.fSet,Max(length(aSet.fSet),length(aOtherSet.fSet)));
 for Index:=0 to length(result.fSet)-1 do begin
  if Index<length(aSet.fSet) then begin
   WordValue:=aSet.fSet[Index];
  end else begin
   WordValue:=0;
  end;
  if Index<length(aOtherSet.fSet) then begin
   WordValue:=WordValue or aOtherSet.fSet[Index];
  end;
  result.fSet[Index]:=WordValue;
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.Subtract(const aSet:TNFASet;const aValue:TpvUInt32):TNFASet;
var WordIndex:TpvSizeInt;
begin
 result.fSet:=copy(aSet.fSet);
 WordIndex:=aValue shr 5;
 if WordIndex<length(result.fSet) then begin
  result.fSet[WordIndex]:=result.fSet[WordIndex] and not (TpvUInt32(1) shl (aValue and 31));
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.Subtract(const aSet,aOtherSet:TNFASet):TNFASet;
var Index:TpvSizeInt;
begin
 SetLength(result.fSet,length(aSet.fSet));
 for Index:=0 to length(result.fSet)-1 do begin
  result.fSet[Index]:=aSet.fSet[Index] and not aOtherSet.fSet[Index];
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.Multiply(const aSet,aOtherSet:TNFASet):TNFASet;
var Index:TpvSizeInt;
begin
 SetLength(result.fSet,length(aSet.fSet));
 for Index:=0 to length(result.fSet)-1 do begin
  result.fSet[Index]:=aSet.fSet[Index] and aOtherSet.fSet[Index];
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.BitwiseAnd(const aSet,aOtherSet:TNFASet):TNFASet;
var Index:TpvSizeInt;
begin
 SetLength(result.fSet,length(aSet.fSet));
 for Index:=0 to length(result.fSet)-1 do begin
  result.fSet[Index]:=aSet.fSet[Index] and aOtherSet.fSet[Index];
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.BitwiseOr(const aSet,aOtherSet:TNFASet):TNFASet;
var Index:TpvSizeInt;
    WordValue:TpvUInt32;
begin
 SetLength(result.fSet,Max(length(aSet.fSet),length(aOtherSet.fSet)));
 for Index:=0 to length(result.fSet)-1 do begin
  if Index<length(aSet.fSet) then begin
   WordValue:=aSet.fSet[Index];
  end else begin
   WordValue:=0;
  end;
  if Index<length(aOtherSet.fSet) then begin
   WordValue:=WordValue or aOtherSet.fSet[Index];
  end;
  result.fSet[Index]:=WordValue;
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.BitwiseXor(const aSet,aOtherSet:TNFASet):TNFASet;
var Index:TpvSizeInt;
    WordValue:TpvUInt32;
begin
 SetLength(result.fSet,Max(length(aSet.fSet),length(aOtherSet.fSet)));
 for Index:=0 to length(result.fSet)-1 do begin
  if Index<length(aSet.fSet) then begin
   WordValue:=aSet.fSet[Index];
  end else begin
   WordValue:=0;
  end;
  if Index<length(aOtherSet.fSet) then begin
   WordValue:=WordValue xor aOtherSet.fSet[Index];
  end;
  result.fSet[Index]:=WordValue;
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.In(const aValue:TpvUInt32;const aSet:TNFASet):boolean;
var WordIndex:TpvSizeInt;
begin
 WordIndex:=aValue shr 5;
 result:=(WordIndex<length(aSet.fSet)) and
         ((aSet.fSet[WordIndex] and (TpvUInt32(1) shl (aValue and 31)))<>0);
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.Equal(const aSet,aOtherSet:TNFASet):boolean;
var Index:TpvSizeInt;
    WordValue,OtherWordValue:TpvUInt32;
begin
 result:=true;
 for Index:=0 to Max(length(aSet.fSet),length(aOtherSet.fSet))-1 do begin
  if Index<length(aSet.fSet) then begin
   WordValue:=aSet.fSet[Index];
  end else begin
   WordValue:=0;
  end;
  if Index<length(aOtherSet.fSet) then begin
   OtherWordValue:=aOtherSet.fSet[Index];
  end else begin
   OtherWordValue:=0;
  end;
  if WordValue<>OtherWordValue then begin
   result:=false;
   exit;
  end;
 end;
end;

class operator TpvTextEditor.TDFASyntaxHighlighting.TNFASet.NotEqual(const aSet,aOtherSet:TNFASet):boolean;
var Index:TpvSizeInt;
    WordValue,OtherWordValue:TpvUInt32;
begin
 result:=false;
 for Index:=0 to Max(length(aSet.fSet),length(aOtherSet.fSet))-1 do begin
  if Index<length(aSet.fSet) then begin
   WordValue:=aSet.fSet[Index];
  end else begin
   WordValue:=0;
  end;
  if Index<length(aOtherSet.fSet) then begin
   OtherWordValue:=aOtherSet.fSet[Index];
  end else begin
   OtherWordValue:=0;
  end;
  if WordValue<>OtherWordValue then begin
   result:=true;
   exit;
  end;
 end;
end;

constructor TpvTextEditor.TDFASyntaxHighlighting.TKeywordCharTreeNode.Create;
begin
 inherited Create;
 FillChar(fChildren,SizeOf(TKeywordCharTreeNodes),#0);
 fHasChildren:=false;
 fKeyword:=false;
 fAttribute:=TpvTextEditor.TDFASyntaxHighlighting.TAttributes.Unknown;
end;

destructor TpvTextEditor.TDFASyntaxHighlighting.TKeywordCharTreeNode.Destroy;
var CurrentChar:AnsiChar;
begin
 for CurrentChar:=Low(TKeywordCharTreeNodes) to High(TKeywordCharTreeNodes) do begin
  FreeAndNil(fChildren[CurrentChar]);
 end;
 inherited Destroy;
end;

constructor TpvTextEditor.TDFASyntaxHighlighting.Create(const aParent:TpvTextEditor);
var NFA:TNFA;
begin
 inherited Create(aParent);

 fNFAStates:=2;

 fNFA:=nil;

 fDFA:=nil;

 fAccept:=nil;

 fCaseInsensitive:=false;

 FillChar(fEquivalence,SizeOf(TEquivalence),#0);

 fKeywordCharRootTreeNode:=TKeywordCharTreeNode.Create;

 try

  Setup;

 finally

  try

   BuildDFA;

  finally

   // We don't need the original NFA states anymore, after we have built the DFA states
   fNFAStates:=0;
   while assigned(fNFA) do begin
    NFA:=fNFA.fNext;
    fNFA.Free;
    fNFA:=NFA;
   end;

  end;

 end;

end;

destructor TpvTextEditor.TDFASyntaxHighlighting.Destroy;
begin
 Clear;
 FreeAndNil(fKeywordCharRootTreeNode);
 inherited Destroy;
end;

procedure TpvTextEditor.TDFASyntaxHighlighting.Clear;
var NFA:TNFA;
    DFA:TDFA;
    Accept:TAccept;
begin
 fDFAStates:=0;
 fNFAStates:=2;
 while assigned(fNFA) do begin
  NFA:=fNFA.fNext;
  fNFA.Free;
  fNFA:=NFA;
 end;
 while assigned(fDFA) do begin
  DFA:=fDFA.fNext;
  fDFA.Free;
  fDFA:=DFA;
 end;
 while assigned(fAccept) do begin
  Accept:=fAccept.fNext;
  fAccept.Free;
  fAccept:=Accept;
 end;
end;

procedure TpvTextEditor.TDFASyntaxHighlighting.AddKeyword(const aKeyword:TpvUTF8String;const aFlags:TAccept.TFlags;const aAttribute:TpvUInt32);
var Node:TKeywordCharTreeNode;
    Index:TpvSizeInt;
    CurrentChar:ansichar;
begin
 Node:=fKeywordCharRootTreeNode;
 for Index:=1 to length(aKeyword) do begin
  CurrentChar:=aKeyword[Index];
  if CurrentChar in KeywordCharSet then begin
   if fCaseInsensitive and (CurrentChar in ['A'..'Z']) then begin
    inc(CurrentChar,ord('a')-ord('A'));
   end;
   if Node.fHasChildren and assigned(Node.fChildren[CurrentChar]) then begin
    Node:=Node.fChildren[CurrentChar];
   end else begin
    Node.fChildren[CurrentChar]:=TKeywordCharTreeNode.Create;
    Node.fHasChildren:=true;
    Node:=Node.fChildren[CurrentChar];
    Node.fHasChildren:=false;
    Node.fKeyword:=false;
    Node.fAttribute:=TpvTextEditor.TDFASyntaxHighlighting.TAttributes.Unknown;
   end;
  end else begin
   break;
  end;
 end;
 if assigned(Node) and (Node<>fKeywordCharRootTreeNode) then begin
  Node.fKeyword:=true;
  Node.fFlags:=aFlags;
  Node.fAttribute:=aAttribute;
 end;
end;

procedure TpvTextEditor.TDFASyntaxHighlighting.AddKeywords(const aKeywords:array of TpvUTF8String;const aFlags:TAccept.TFlags;const aAttribute:TpvUInt32);
var Keyword:TpvRawByteString;
begin
 for Keyword in aKeywords do begin
  AddKeyword(Keyword,aFlags,aAttribute);
 end;
end;

procedure TpvTextEditor.TDFASyntaxHighlighting.AddRule(const aRule:TpvUTF8String;const aFlags:TAccept.TFlags;const aAttribute:TpvUInt32);
var IsBegin,IsEnd:boolean;
 procedure AddNFATransition(const aFrom,aTo:TpvSizeInt;const aSet:TCharSet);
 var NFA:TNFA;
     CurrentChar,EquivalenceChar:AnsiChar;
     Other:array[AnsiChar] of AnsiChar;
     InSet:TCharSet;
 begin
  NFA:=TNFA.Create;
  NFA.fNext:=fNFA;
  fNFA:=NFA;
  NFA.fSet:=aSet;
  NFA.fFrom:=aFrom;
  NFA.fTo:=aTo;
  if aSet<>[] then begin
   FillChar(Other,SizeOf(Other),#0);
   InSet:=[];
   for CurrentChar:=#0 to #255 do begin
    EquivalenceChar:=fEquivalence[CurrentChar];
    if CurrentChar=EquivalenceChar then begin
     Other[CurrentChar]:=CurrentChar;
     if CurrentChar in aSet then begin
      Include(InSet,CurrentChar);
     end;
    end else if (not (CurrentChar in aSet)) xor not (EquivalenceChar in InSet) then begin
     if Other[EquivalenceChar]=EquivalenceChar then begin
      Other[EquivalenceChar]:=CurrentChar;
     end;
     fEquivalence[CurrentChar]:=Other[EquivalenceChar];
    end;
   end;
  end;
 end;
 procedure Parse(var aStart,aEnd:TpvSizeInt);
 const LexSymbolChars=['^','$','|','*','+','?','[',']','-','.','(',')','{','}'];
 var InputText:TpvRawByteString;
     InputPosition:TpvSizeInt;
     InputLength:TpvSizeInt;
  procedure ParseDisjunction(var aStart,aEnd:TpvSizeInt); forward;
  procedure ParseAtom(var aStart,aEnd:TpvSizeInt);
  var CharSet:TCharSet;
      Complement:boolean;
      CurrentChar,OneEndChar,OtherEndChar:AnsiChar;
  begin
   if (InputPosition<=InputLength) and (InputText[InputPosition]='(') then begin
    inc(InputPosition);
    ParseDisjunction(aStart,aEnd);
    if (InputPosition<=InputLength) and (InputText[InputPosition]=')') then begin
     inc(InputPosition);
    end else begin
     raise EParserErrorExpectedRightParen.Create('Expected right paren');
    end;
   end else if InputPosition<=InputLength then begin
    CharSet:=[];
    CurrentChar:=InputText[InputPosition];
    case CurrentChar of
     '.':begin
      inc(InputPosition);
      CharSet:=[#0..#255];
     end;
     '[':begin
      inc(InputPosition);
      Complement:=(InputPosition<=InputLength) and (InputText[InputPosition]='^');
      if Complement then begin
       inc(InputPosition);
       CharSet:=[#0..#255];
      end;
      if (InputPosition<=InputLength) and (InputText[InputPosition]=']') then begin
       inc(InputPosition);
       raise EParserErrorEmptySet.Create('Empty set');
      end else begin
       while InputPosition<=InputLength do begin
        CurrentChar:=InputText[InputPosition];
        case CurrentChar of
         ']':begin
          break;
         end;
         else begin
          if CurrentChar in ([#0..#255]-LexSymbolChars) then begin
           inc(InputPosition);
           if (CurrentChar='\') and (InputPosition<=InputLength) then begin
            CurrentChar:=InputText[InputPosition];
            inc(InputPosition);
           end;
           OneEndChar:=CurrentChar;
           OtherEndChar:=CurrentChar;
           if (InputPosition<=InputLength) and (InputText[InputPosition]='-') then begin
            inc(InputPosition);
            if InputPosition<=InputLength then begin
             CurrentChar:=InputText[InputPosition];
             if CurrentChar in ([#0..#255]-LexSymbolChars) then begin
              inc(InputPosition);
              if (CurrentChar='\') and (InputPosition<=InputLength) then begin
               CurrentChar:=InputText[InputPosition];
               inc(InputPosition);
              end;
              OtherEndChar:=CurrentChar;
             end else begin
              raise EParserErrorInvalidMetaChar.Create('Invalid meta-char');
             end;
            end else begin
             raise EParserErrorUnexpectedEndOfText.Create('Unexpected end of text');
            end;
           end;
           if OneEndChar=OtherEndChar then begin
            if Complement then begin
             Exclude(CharSet,OneEndChar);
            end else begin
             Include(CharSet,OneEndChar);
            end;
           end else if OtherEndChar<OneEndChar then begin
            if Complement then begin
             CharSet:=CharSet-[OtherEndChar..OneEndChar];
            end else begin
             CharSet:=CharSet+[OtherEndChar..OneEndChar];
            end;
           end else begin
            if Complement then begin
             CharSet:=CharSet-[OneEndChar..OtherEndChar];
            end else begin
             CharSet:=CharSet+[OneEndChar..OtherEndChar];
            end;
           end;
          end else begin
           raise EParserErrorInvalidMetaChar.Create('Invalid meta-char');
          end;
         end;
        end;
       end;
       if (InputPosition<=InputLength) and (InputText[InputPosition]=']') then begin
        inc(InputPosition);
       end else begin
        raise EParserErrorExpectedRightBracket.Create('Expected right bracket');
       end;
      end;
     end;
     else begin
      if CurrentChar in (([#0..#255]-LexSymbolChars)+['-']) then begin
       inc(InputPosition);
       if (CurrentChar='\') and (InputPosition<=InputLength) then begin
        CurrentChar:=InputText[InputPosition];
        inc(InputPosition);
       end;
       Include(CharSet,CurrentChar);
      end else begin
       raise EParserErrorInvalidMetaChar.Create('Invalid meta-char');
      end;
     end;
    end;
    if aStart=0 then begin
     aStart:=fNFAStates;
     inc(fNFAStates);
    end;
    if aEnd=0 then begin
     aEnd:=fNFAStates;
     inc(fNFAStates);
    end;
    AddNFATransition(aStart,aEnd,CharSet);
   end;
  end;
  procedure ParseTerm(var aStart,aEnd:TpvSizeInt);
  var LocalStart,LocalEnd:TpvSizeInt;
  begin
   LocalStart:=0;
   LocalEnd:=0;
   ParseAtom(LocalStart,LocalEnd);
   if InputPosition<=InputLength then begin
    case InputText[InputPosition] of
     '*':begin
      inc(InputPosition);
      AddNFATransition(LocalStart,LocalEnd,[]);
      AddNFATransition(LocalEnd,LocalStart,[]);
     end;
     '+':begin
      inc(InputPosition);
      AddNFATransition(LocalEnd,LocalStart,[]);
     end;
     '?':begin
      inc(InputPosition);
      AddNFATransition(LocalStart,LocalEnd,[]);
     end;
    end;
   end;
   if aEnd=0 then begin
    aEnd:=fNFAStates;
    inc(fNFAStates);
   end;
   AddNFATransition(LocalEnd,aEnd,[]);
   if aStart=0 then begin
    aStart:=fNFAStates;
    inc(fNFAStates);
   end;
   AddNFATransition(aStart,LocalStart,[]);
  end;
  procedure ParseAlternative(var aStart,aEnd:TpvSizeInt);
  const AllowedChars=([#0..#255]-LexSymbolChars)+['(','[','-','.'];
  var LocalStart,LocalEnd:TpvSizeInt;
  begin
   LocalEnd:=0;
   ParseTerm(aStart,LocalEnd);
   while (InputPosition<=InputLength) and
         (InputText[InputPosition] in AllowedChars) do begin
    LocalStart:=LocalEnd;
    LocalEnd:=0;
    ParseTerm(LocalStart,LocalEnd);
   end;
   if aEnd<>0 then begin
    AddNFATransition(LocalEnd,aEnd,[]);
   end else begin
    aEnd:=LocalEnd;
   end;
  end;
  procedure ParseDisjunction(var aStart,aEnd:TpvSizeInt);
  begin
   ParseAlternative(aStart,aEnd);
   while (InputPosition<=InputLength) and (InputText[InputPosition]='|') do begin
    inc(InputPosition);
    ParseAlternative(aStart,aEnd);
   end;
  end;
 begin

  InputText:=aRule;
  InputPosition:=1;
  InputLength:=length(InputText);

  if (InputPosition<=InputLength) and (InputText[InputPosition]='^') then begin
   IsBegin:=true;
   inc(InputPosition);
  end else begin
   IsBegin:=false;
  end;

  ParseDisjunction(aStart,aEnd);

  if (InputPosition<=InputLength) and (InputText[InputPosition]='$') then begin
   IsEnd:=true;
   inc(InputPosition);
  end else begin
   IsEnd:=false;
  end;

  if InputPosition<=InputLength then begin
   raise EParserErrorExpectedEndOfText.Create('Expected end of text');
  end;

 end;
var LocalStart,LocalEnd,OldNFAStates:TpvSizeInt;
    Accept:TAccept;
    OldNFA,NFA:TNFA;
begin
 LocalStart:=0;
 LocalEnd:=0;
 OldNFAStates:=fNFAStates;
 OldNFA:=fNFA;
 try
  Parse(LocalStart,LocalEnd);
  AddNFATransition(ord(IsBegin) and 1,LocalStart,[]);
  Accept:=TAccept.Create;
  Accept.fNext:=fAccept;
  fAccept:=Accept;
  Accept.fState:=LocalEnd;
  Accept.fFlags:=aFlags;
  if IsEnd then begin
   Include(Accept.fFlags,TAccept.TFlag.IsEnd);
  end;
  Accept.fAttribute:=aAttribute;
 except
  fNFAStates:=OldNFAStates;
  while assigned(fNFA) and (fNFA<>OldNFA) do begin
   NFA:=fNFA.fNext;
   fNFA.Free;
   fNFA:=NFA;
  end;
  raise;
 end;
end;

procedure TpvTextEditor.TDFASyntaxHighlighting.BuildDFA;
 procedure ComputeClosure(var aNFASet:TNFASet);
 var NFA:TNFA;
     Changed:boolean;
 begin
  repeat
   Changed:=false;
   NFA:=fNFA;
   while assigned(NFA) do begin
    if (NFA.fSet=[]) and (NFA.fFrom in aNFASet) and not (NFA.fTo in aNFASet) then begin
     Changed:=true;
     aNFASet:=aNFASet+NFA.fTo;
    end;
    NFA:=NFA.fNext;
   end;
  until not Changed;
 end;
var Tail,Next,Search:TDFA;
    Destination:TNFASet;
    CurrentChar:AnsiChar;
    DestinationEmpty:boolean;
    NFA:TNFA;
    Accept:TAccept;
begin

 Destination:=TNFASet.Create([]);

 fDFAStates:=0;

 Tail:=TDFA.Create;
 fDFA:=Tail;
 Tail.fNext:=nil;
 Tail.fNFASet:=TNFASet.Create([0]);
 Tail.fNumber:=fDFAStates;
 inc(fDFAStates);
 ComputeClosure(Tail.fNFASet);

 Tail.fNext:=TDFA.Create;
 Tail:=Tail.fNext;
 Tail.fNext:=nil;
 Tail.fNFASet:=TNFASet.Create([0,1]);
 Tail.fNumber:=fDFAStates;
 inc(fDFAStates);
 ComputeClosure(Tail.fNFASet);

 Next:=fDFA;
 while assigned(Next) do begin

  for CurrentChar:=#0 to #255 do begin

   if fEquivalence[CurrentChar]=CurrentChar then begin

    Destination:=TNFASet.Create([]);
    DestinationEmpty:=true;

    NFA:=fNFA;
    while assigned(NFA) do begin
     if (NFA.fFrom in Next.fNFASet) and
        (CurrentChar in NFA.fSet) then begin
      Destination:=Destination+NFA.fTo;
      DestinationEmpty:=false;
     end;
     NFA:=NFA.fNext;
    end;

    ComputeClosure(Destination);

    if DestinationEmpty then begin
     Search:=nil;
    end else begin
     Search:=fDFA;
     while assigned(Search) and (Search.fNFASet<>Destination) do begin
      Search:=Search.fNext;
     end;
     if not assigned(Search) then begin
      Tail.fNext:=TDFA.Create;
      Tail:=Tail.fNext;
      Search:=Tail;
      Tail.fNext:=nil;
      Tail.fNumber:=fDFAStates;
      inc(fDFAStates);
      Tail.fNFASet.fSet:=copy(Destination.fSet);
     end;
    end;

    Next.fWhereTo[CurrentChar]:=Search;

   end else begin

    Next.fWhereTo[CurrentChar]:=Next.fWhereTo[fEquivalence[CurrentChar]];

   end;

  end;

  Next.fAccept:=nil;
  Next.fAcceptEnd:=nil;

  Accept:=fAccept;
  while assigned(Accept) do begin
   if Accept.fState in Next.fNFASet then begin
    Next.fAcceptEnd:=Accept;
    if not (TAccept.TFlag.IsEnd in Accept.fFlags) then begin
     Next.fAccept:=Accept;
    end;
   end;
   Accept:=Accept.fNext;
  end;

  Next:=Next.fNext;

 end;

end;

procedure TpvTextEditor.TDFASyntaxHighlighting.Setup;
begin

end;

procedure TpvTextEditor.TDFASyntaxHighlighting.Update(const aUntilCodePoint:TpvSizeInt);
type TMultiLineCPreprocessorState=
      (
       None,
       Maybe,
       Skip,
       LastCRSkipLF,
       LastLFSkipCR
      );
     TParserState=record
      CodePointEnumerator:TpvTextEditor.TRope.TCodePointEnumerator;
      CodePointIndex:TpvSizeInt;
      NewLine:TpvUInt8;
      MultiLineCPreprocessorState:TMultiLineCPreprocessorState;
      Valid:boolean;
     end;
     TParserStates=array[0..3] of TParserState;
var CodePointEnumeratorSource:TpvTextEditor.TRope.TCodePointEnumeratorSource;
    CodePoint,Level,Attribute,Preprocessor:TpvUInt32;
    LastState,State:TDFASyntaxHighlighting.TState;
    DFA:TDFA;
    OldCount:TpvSizeInt;
    Accept,LastAccept:TAccept;
    ParserStates:TParserStates;
    KeywordCharTreeNode:TKeywordCharTreeNode;
    CodeUnit:TpvRawByteChar;
    DoBreak,MaybePreprocessorMultiLine:boolean;
    Flags:TAccept.TFlags;
 procedure UpdateMultiLineCPreprocessorState(var aMultiLineCPreprocessorState:TMultiLineCPreprocessorState;const aCodePoint:TpvUInt32);
 begin
  case aCodePoint of
   ord('\'):begin
    case aMultiLineCPreprocessorState of
     TMultiLineCPreprocessorState.None:begin
      if MaybePreprocessorMultiLine then begin
       aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.Maybe;
      end;
     end;
    end;
   end;
   10:begin
    case aMultiLineCPreprocessorState of
     TMultiLineCPreprocessorState.Maybe:begin
      aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.LastLFSkipCR;
     end;
     TMultiLineCPreprocessorState.LastCRSkipLF:begin
      aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.Skip;
     end;
     else begin
      aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.None;
     end;
    end;
   end;
   13:begin
    case aMultiLineCPreprocessorState of
     TMultiLineCPreprocessorState.Maybe:begin
      aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.LastCRSkipLF;
     end;
     TMultiLineCPreprocessorState.LastLFSkipCR:begin
      aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.Skip;
     end;
     else begin
      aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.None;
     end;
    end;
   end;
   else begin
    aMultiLineCPreprocessorState:=TMultiLineCPreprocessorState.None;
   end;
  end;
 end;
 function ProcessCodeUnit(const aCodeUnit:TpvRawByteChar):boolean;
 begin

  DFA:=DFA.fWhereTo[aCodeUnit];

  if not assigned(DFA) then begin
   if not assigned(LastAccept) then begin
    ParserStates[1]:=ParserStates[0];
    CodePoint:=ParserStates[1].CodePointEnumerator.GetCurrent;
    ParserStates[1].Valid:=ParserStates[1].CodePointEnumerator.MoveNext;
    inc(ParserStates[1].CodePointIndex);
    if (ParserStates[1].MultiLineCPreprocessorState=TMultiLineCPreprocessorState.None) and
       ((CodePoint in [10,13]) or
        (ParserStates[1].CodePointIndex=fParent.fRope.fCountCodePoints)) then begin
     ParserStates[1].NewLine:=ParserStates[1].NewLine or 2;
    end;
    UpdateMultiLineCPreprocessorState(ParserStates[1].MultiLineCPreprocessorState,CodePoint);
    ParserStates[1].NewLine:=ParserStates[1].NewLine shr 1;
   end;
   result:=false;
   exit;
  end;

  if (ParserStates[1].NewLine and 2)<>0 then begin
   Accept:=DFA.fAcceptEnd;
  end else begin
   Accept:=DFA.fAccept;
  end;

  if assigned(Accept) then begin
   LastAccept:=Accept;
   ParserStates[2]:=ParserStates[1];
   if (TAccept.TFlag.IsQuick in Accept.fFlags) or
      (((ParserStates[1].NewLine and 2)<>0) and
       (TAccept.TFlag.IsEnd in Accept.fFlags)) then begin
    result:=false;
    exit;
   end;
  end;

  result:=true;

 end;
begin

 if (fCodePointIndex<fParent.fRope.fCountCodePoints) and
    ((aUntilCodePoint<0) or (fCodePointIndex<aUntilCodePoint)) then begin

  if fCountStates>0 then begin
   LastState:=TDFASyntaxHighlighting.TState(fStates[fCountStates-1]);
   fLevel:=LastState.fLevel and $3fffffff;
  end else begin
   LastState:=nil;
  end;

  CodePointEnumeratorSource:=fParent.fRope.GetCodePointEnumeratorSource(fCodePointIndex,-1);

  Preprocessor:=TpvUInt32($ffffffff);

  MaybePreprocessorMultiLine:=false;

  ParserStates[0].CodePointIndex:=fCodePointIndex;

  ParserStates[0].CodePointEnumerator:=CodePointEnumeratorSource.GetEnumerator;

  ParserStates[0].Valid:=ParserStates[0].CodePointEnumerator.MoveNext;

  if (ParserStates[0].CodePointIndex=0) or
     ((ParserStates[0].CodePointIndex>0) and
      fParent.IsCodePointNewLine(ParserStates[0].CodePointIndex-1)) then begin
   ParserStates[0].NewLine:=1;
  end else begin
   ParserStates[0].NewLine:=0;
  end;

  ParserStates[0].MultiLineCPreprocessorState:=TMultiLineCPreprocessorState.None;

  while ParserStates[0].Valid and
        (MaybePreprocessorMultiLine or
         (aUntilCodePoint<0) or
         (ParserStates[0].CodePointIndex<aUntilCodePoint)) do begin

   if (ParserStates[0].NewLine and 1)<>0 then begin
    Preprocessor:=TpvUInt32($ffffffff);
    MaybePreprocessorMultiLine:=false;
    DFA:=fDFA.fNext;
   end else begin
    DFA:=fDFA;
   end;

   ParserStates[1]:=ParserStates[0];

   LastAccept:=nil;

   while ParserStates[1].Valid do begin

    CodePoint:=ParserStates[1].CodePointEnumerator.GetCurrent;

    ParserStates[1].Valid:=ParserStates[1].CodePointEnumerator.MoveNext;

    inc(ParserStates[1].CodePointIndex);

    ParserStates[1].NewLine:=ParserStates[1].NewLine shr 1;

    if (ParserStates[1].MultiLineCPreprocessorState=TMultiLineCPreprocessorState.None) and
       ((CodePoint in [10,13]) or
        (ParserStates[1].CodePointIndex=fParent.fRope.fCountCodePoints)) then begin
     ParserStates[1].NewLine:=ParserStates[1].NewLine or 2;
    end;

    UpdateMultiLineCPreprocessorState(ParserStates[1].MultiLineCPreprocessorState,CodePoint);

    if CodePoint<128 then begin
     if not ProcessCodeUnit(AnsiChar(TpvUInt8(CodePoint))) then begin
      break;
     end;
    end else begin
     DoBreak:=false;
     for CodeUnit in TpvTextEditor.TUTF8Utils.UTF32CharToUTF8(CodePoint) do begin
      if not ProcessCodeUnit(AnsiChar(TpvUInt8(CodePoint))) then begin
       DoBreak:=true;
       break;
      end;
     end;
     if DoBreak then begin
      break;
     end;
    end;

   end;

   if assigned(LastAccept) then begin
    Attribute:=LastAccept.fAttribute;
    Flags:=LastAccept.fFlags;
    if TAccept.TFlag.IsKeyword in Flags then begin
     ParserStates[3]:=ParserStates[0];
     KeywordCharTreeNode:=fKeywordCharRootTreeNode;
     while ParserStates[3].Valid and
           (ParserStates[3].CodePointIndex<ParserStates[2].CodePointIndex) do begin
      CodePoint:=ParserStates[3].CodePointEnumerator.Current;
      if (CodePoint>=TpvUInt32(ord(Low(TKeywordCharSet)))) and (CodePoint<=TpvUInt32(ord(High(TKeywordCharSet)))) then begin
       if fCaseInsensitive and ((CodePoint>=TpvUInt32(ord('A'))) and (CodePoint<=TpvUInt32(ord('Z')))) then begin
        inc(CodePoint,ord('a')-ord('A'));
       end;
       if KeywordCharTreeNode.fHasChildren and
          assigned(KeywordCharTreeNode.fChildren[AnsiChar(TpvUInt8(CodePoint))]) then begin
        KeywordCharTreeNode:=KeywordCharTreeNode.fChildren[AnsiChar(TpvUInt8(CodePoint))];
        ParserStates[3].Valid:=ParserStates[3].CodePointEnumerator.MoveNext;
        inc(ParserStates[3].CodePointIndex);
        continue;
       end;
      end;
      break;
     end;
     if (assigned(KeywordCharTreeNode) and
         KeywordCharTreeNode.fKeyword) and not
        (ParserStates[3].Valid and
         (ParserStates[3].CodePointIndex<ParserStates[2].CodePointIndex)) then begin
      Attribute:=KeywordCharTreeNode.fAttribute;
      Flags:=Flags+KeywordCharTreeNode.fFlags;
     end;
    end;
    if TAccept.TFlag.IsPreprocessorLine in Flags then begin
     Preprocessor:=LastAccept.fAttribute;
     if TAccept.TFlag.IsMaybeCPreprocessorMultiLine in Flags then begin
      MaybePreprocessorMultiLine:=true;
     end;
    end else if Preprocessor<>TpvUInt32($ffffffff) then begin
     if Attribute=TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment then begin
      if TAccept.TFlag.IsEnd in Flags then begin
       MaybePreprocessorMultiLine:=false;
       ParserStates[2].MultiLineCPreprocessorState:=TMultiLineCPreprocessorState.None;
      end else begin
       case ParserStates[2].MultiLineCPreprocessorState of
        TMultiLineCPreprocessorState.Maybe:begin
         ParserStates[2].MultiLineCPreprocessorState:=TMultiLineCPreprocessorState.None;
        end;
        TMultiLineCPreprocessorState.LastCRSkipLF,
        TMultiLineCPreprocessorState.LastLFSkipCR:begin
         ParserStates[2].NewLine:=1;
         ParserStates[2].MultiLineCPreprocessorState:=TMultiLineCPreprocessorState.None;
        end;
       end;
      end;
     end else begin
      Attribute:=Preprocessor;
     end;
    end;
   end else begin
    if Preprocessor<>TpvUInt32($ffffffff) then begin
     Attribute:=Preprocessor;
    end else begin
     Attribute:=TpvTextEditor.TSyntaxHighlighting.TAttributes.Unknown;
    end;
    Flags:=[];
   end;

   if TAccept.TFlag.IncreaseLevel in Flags then begin
    inc(fLevel);
    Level:=fLevel or $40000000;
   end else if TAccept.TFlag.DecreaseLevel in Flags then begin
    Level:=fLevel or $80000000;
   end else begin
    Level:=fLevel;
   end;

   if (not assigned(LastState)) or
      ((LastState.fAccept<>LastAccept) or
       (LastState.fLevel<>Level) or
       (LastState.fAttribute<>Attribute)) then begin
    OldCount:=length(fStates);
    if OldCount<(fCountStates+1) then begin
     SetLength(fStates,(fCountStates+1)*2);
     FillChar(fStates[OldCount],(length(fStates)-OldCount)*SizeOf(TSyntaxHighlighting.TState),#0);
    end;
    State:=TDFASyntaxHighlighting.TState.Create;
    fStates[fCountStates]:=State;
    inc(fCountStates);
    State.fCodePointIndex:=ParserStates[0].CodePointIndex;
    State.fAttribute:=Attribute;
    State.fLevel:=Level;
    State.fAccept:=LastAccept;
    LastState:=State;
   end;

   if TAccept.TFlag.DecreaseLevel in Flags then begin
    if fLevel>0 then begin
     dec(fLevel);
    end;
   end;

   if assigned(LastAccept) then begin
    ParserStates[0]:=ParserStates[2];
   end else begin
    ParserStates[0]:=ParserStates[1];
   end;

   if ParserStates[0].CodePointIndex>=fParent.fRope.fCountCodePoints then begin
    fCodePointIndex:=fParent.fRope.fCountCodePoints;
   end else begin
    fCodePointIndex:=ParserStates[0].CodePointIndex;
   end;

  end;

 end;

end;

class function TpvTextEditor.TPascalSyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='Pascal';
end;

class function TpvTextEditor.TPascalSyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
 SetLength(result,3);
 result[0]:='.pas';
 result[1]:='.pp';
 result[2]:='.p';
end;

procedure TpvTextEditor.TPascalSyntaxHighlighting.Setup;
begin
 fCaseInsensitive:=true;
 AddKeywords(['absolute','abstract','and','array','as','asm','assembler',
              'automated','case','cdecl','class','const','constructor',
              'contains','default','deprecated','destructor','dispid',
              'dispinterface','div','do','downto','dynamic','else','except',
              'export','exports','external','far','file','final','finalization',
              'finally','for','forward','function','goto','helper','if',
              'implementation','implements','in','index','interface','inherited',
              'initialization','inline','is','label','library',
              'message','mod','name','near','nil','nodefault','not','of',
              'object','on','operator','or','out','overload','override',
              'package','packed','pascal','platform','private','procedure',
              'program','property','protected','public','published','raise','read',
              'readonly','register','reintroduce','repeat','requires','resourcestring',
              'safecall','sealed','set','shl','shr','stdcall','stored','string',
              'stringresource','then','threadvar','to','try','type','unit','until',
              'uses','var','virtual','while','with','write','writeonly','xor'],
             [],
             TpvTextEditor.TSyntaxHighlighting.TAttributes.Keyword);
 AddKeywords(['begin','record'],
             [TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IncreaseLevel],
             TpvTextEditor.TSyntaxHighlighting.TAttributes.Keyword);
 AddKeywords(['end'],
             [TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.DecreaseLevel],
             TpvTextEditor.TSyntaxHighlighting.TAttributes.Keyword);
 AddRule('['#32#9']+',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.WhiteSpace);
 AddRule('\(\*\$.*\*\)|\{\$.*\}',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.Preprocessor);
 AddRule('\(\*[^\$].*\*\)|\{[^\$].*\}',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('\(\*\*\)|\{\}',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('\(\*\$.*|\{\$.*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Preprocessor);
 AddRule('\(\*.*|\{.*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('//.*$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment); // or alternatively '//[^'#10#13']*['#10#13']?'
 AddRule('\#(\$[0-9A-Fa-f]*|[0-9]*)',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\$[0-9A-Fa-f]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('[0-9]+(\.[0-9]+)?([Ee][\+\-]?[0-9]*)?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule(TpvRawByteString('[A-Za-z\_'#128'-'#255'][A-Za-z0-9\_'#128'-'#255']*'),[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsKeyword],TpvTextEditor.TSyntaxHighlighting.TAttributes.Identifier);
 AddRule('\@|\-|\+|\/|\*|\=|\<|\>|\<\>|\<\=|\>\=|\:\=|\^',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Operator);
 AddRule('\}|\*\)|\,|\.|\.\.|\:|\;|\?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\[|\(',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IncreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\]|\)',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.DecreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\''[^\'']*\''',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''[^\'']*$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
end;

class function TpvTextEditor.TCSyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='C';
end;

class function TpvTextEditor.TCSyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
 SetLength(result,2);
 result[0]:='.c';
 result[1]:='.h';
end;

procedure TpvTextEditor.TCSyntaxHighlighting.Setup;
begin
 fCaseInsensitive:=false;
 AddKeywords(['_Alignas','_Alignof','_Atomic','_Bool','_Complex','_Generic','_Imaginary',
              '_Noreturn','_Static_assert','_Thread_local','auto','break','case','char',
              'const','continue','default','do','double','else','enum','extern','float',
              'for','goto','if','inline','int','long','register','restrict','return',
              'short','signed','sizeof','static','struct','switch','typedef','union',
              'unsigned','void','volatile','while'],
             [],
             TpvTextEditor.TSyntaxHighlighting.TAttributes.Keyword);
 AddRule('^['#32#9']*\#',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick,TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsPreprocessorLine,TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsMaybeCPreprocessorMultiLine],TpvTextEditor.TSyntaxHighlighting.TAttributes.Preprocessor);
 AddRule('['#32#9']+',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.WhiteSpace);
 AddRule('\/\*.*\*\/',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('\/\*.*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('//[^'#10#13']*['#10#13']?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule(TpvRawByteString('[A-Za-z\_\$'#128'-'#255'][A-Za-z0-9\_\$'#128'-'#255']*'),[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsKeyword],TpvTextEditor.TSyntaxHighlighting.TAttributes.Identifier);
 AddRule('[0-9]+(\.[0-9]+)?([Ee][\+\-]?[0-9]*)?([DdFf]|[Ll][Dd])?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('0[xX][0-9A-Fa-f]*[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('[0-9A-Fa-f]+[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('\"([^\"\\]|\\.)*\"',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\"([^\"\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\''',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('[\%\-\+\/\&\*\=\<\>\|\!\~\^]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Operator);
 AddRule('[\,\;\.\?\:\\]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\[|\(|\{',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IncreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\]|\)|\}',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.DecreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
end;

class function TpvTextEditor.TCPPSyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='C++';
end;

class function TpvTextEditor.TCPPSyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
 SetLength(result,4);
 result[0]:='.cpp';
 result[1]:='.cxx';
 result[2]:='.hpp';
 result[3]:='.hxx';
end;

procedure TpvTextEditor.TCPPSyntaxHighlighting.Setup;
begin
 fCaseInsensitive:=false;
 AddKeywords(['__asm','__automated','__cdecl','__classid','__closure','__declspec',
              '__dispid','__except','__export','__fastcall','__finally','__import',
              '__int16','__int32','__int64','__int8','__pascal','__property',
              '__published','__rtti','__stdcall','__thread','__try','_asm','_cdecl',
              '_export','_fastcall','_import','_pascal','_stdcall',
              'asm','auto','bool','break','case','catch','cdecl','char','class',
              'const','const_cast','continue','default','delete','do','double',
              'dynamic_cast','else','enum','explicit','extern','false','float',
              'for','friend','goto','if','inline','int','interface','long',
              'mutable','namespace','new','operator','pascal','private','protected',
              'public','register','reinterpret_cast','return','short','signed',
              'sizeof','static','static_cast','struct','switch','template','this',
              'throw','true','try','typedef','typeid','typename','union',
              'unsigned','using','virtual','void','volatile','wchar_t','while'],
             [],
             TpvTextEditor.TSyntaxHighlighting.TAttributes.Keyword);
 AddRule('^['#32#9']*\#',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick,TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsPreprocessorLine,TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsMaybeCPreprocessorMultiLine],TpvTextEditor.TSyntaxHighlighting.TAttributes.Preprocessor);
 AddRule('['#32#9']+',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.WhiteSpace);
 AddRule('\/\*.*\*\/',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('\/\*.*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('//[^'#10#13']*['#10#13']?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule(TpvRawByteString('[A-Za-z\_\$'#128'-'#255'][A-Za-z0-9\_\$'#128'-'#255']*'),[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsKeyword],TpvTextEditor.TSyntaxHighlighting.TAttributes.Identifier);
 AddRule('[0-9]+(\.[0-9]+)?([Ee][\+\-]?[0-9]*)?([DdFf]|[Ll][Dd])?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('0[xX][0-9A-Fa-f]*[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('[0-9A-Fa-f]+[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('\"([^\"\\]|\\.)*\"',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\"([^\"\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\''',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('[\%\-\+\/\&\*\=\<\>\|\!\~\^]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Operator);
 AddRule('[\,\;\.\?\:\\]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\[|\(|\{',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IncreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\]|\)|\}',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.DecreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
end;

class function TpvTextEditor.TJavaSyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='Java';
end;

class function TpvTextEditor.TJavaSyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
 SetLength(result,1);
 result[0]:='.java';
end;

procedure TpvTextEditor.TJavaSyntaxHighlighting.Setup;
begin
 fCaseInsensitive:=false;
 AddKeywords(['abstract','assert','boolean','break','TpvUInt8','case','catch','char',
              'class','const','continue','default','do','double','else','enum',
              'extends','false','final','finally','float','for','goto','if',
              'implements','import','instanceof','int','interface','long','native',
              'new','null','package','private','protected','public','return',
              'short','static','strictfp','super','switch','synchronized','this',
              'throw','throws','transient','true','try','void','volatile','while'],
             [],
             TpvTextEditor.TSyntaxHighlighting.TAttributes.Keyword);
 AddRule('['#32#9']+',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.WhiteSpace);
 AddRule('\/\*.*\*\/',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('\/\*.*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('//.*$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment); // or alternatively '//[^'#10#13']*['#10#13']?'
 AddRule(TpvRawByteString('[A-Za-z\_\$'#128'-'#255'][A-Za-z0-9\_\$'#128'-'#255']*'),[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsKeyword],TpvTextEditor.TSyntaxHighlighting.TAttributes.Identifier);
 AddRule('[0-9]+(\.[0-9]+)?([Ee][\+\-]?[0-9]*)?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('0[xX][0-9A-Fa-f]*[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('[0-9A-Fa-f]+[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('\"([^\"\\]|\\.)*\"',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\"([^\"\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\''',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('[\%\-\+\/\&\*\=\<\>\|\!\~\^]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Operator);
 AddRule('[\@\,\;\.\?\:\\]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('[\,\;\.\?\:\\]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\[|\(|\{',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IncreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\]|\)|\}',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.DecreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
end;

class function TpvTextEditor.TGLSLSyntaxHighlighting.GetName:TpvUTF8String;
begin
 result:='GLSL';
end;

class function TpvTextEditor.TGLSLSyntaxHighlighting.GetFileExtensions:TpvTextEditor.TSyntaxHighlighting.TFileExtensions;
begin
 result:=nil;
 SetLength(result,1);
 result[0]:='.glsl';
end;

procedure TpvTextEditor.TGLSLSyntaxHighlighting.Setup;
begin
 fCaseInsensitive:=false;
 AddKeywords(['and_op','or_op','xor_op','mul_assign','div_assign','add_assign',
              'atomic_uint','break','continue','do','else','for','if','discard',
              'return','switch','case','default','subroutine','bvec2','bvec3',
              'bvec4','ivec2','ivec3','ivec4','uvec2','uvec3','uvec4','vec2',
              'vec3','vec4','coherent','volatile','restrict','readonly',
              'writeonly','comma','colon','equal','semicolon','bang','dash',
              'tilde','plus','star','slash','percent','const','bool','float',
              'double','int','uint','dmat2x2','dmat2x3','dmat2x4','dmat3x2',
              'dmat3x3','dmat3x4','dmat4x2','dmat4x3','dmat4x4','dvec2','dvec3',
              'dvec4','dmat2','dmat3','dmat4','field_selection','floatconstant',
              'doubleconstant','intconstant','uintconstant','boolconstant',
              'high_precision','medium_precision','low_precision','precision',
              'identifier','type_name','image1d','iimage1d','uimage1d','image2d',
              'iimage2d','image1darray','iimage1darray','uimage1darray',
              'image2darray','iimage2darray','uimage2darray','image2dms',
              'iimage2dms','uimage2dms','image2dmsarray','iimage2dmsarray',
              'uimage2dmsarray','image2drect','iimage2drect','uimage2drect',
              'imagebuffer','iimagebuffer','uimagebuffer','imagecube','iimagecube',
              'uimagecube','imagecubearray','iimagecubearray','uimagecubearray',
              'inc_op','dec_op','le_op','ge_op','eq_op','ne_op','invariant','precise',
              'isampler1darray','isampler2darray','usampler1d','usampler2d','usampler3d',
              'isamplercubearray','usamplercubearray','left_angle','right_angle',
              'vertical_bar','caret','ampersand','question','left_op','right_op',
              'left_paren','right_paren','left_bracket','right_bracket','left_brace',
              'right_brace','dot','mat2','mat3','mat4','centroid','in','out','inout',
              'mat2x2','mat2x3','mat2x4','mat3x2','mat3x3','mat3x4','mat4x2','mat4x3',
              'mat4x4','mod_assign','left_assign','right_assign','and_assign','xor_assign',
              'or_assign','noperspective','flat','smooth','layout','sampler1d','sampler2d',
              'sampler3d','samplercube','sampler1dshadow','sampler2dshadow',
              'sampler2darrayshadow','isampler1d','isampler2d','isampler3d','isamplercube',
              'sampler2dms','isampler2dms','usampler2dms','sampler2dmsarray',
              'isampler2dmsarray','usampler2dmsarray','sampler2drect','sampler2drectshadow',
              'isampler2drect','usampler2drect','samplerbuffer','isamplerbuffer','usamplerbuffer',
              'samplercubearray','samplercubearrayshadow','samplercubeshadow','sampler1darray',
              'sampler2darray','sampler1darrayshadow','struct','void','while','sub_assign',
              'uimage2d','image3d','iimage3d','uimage3d','uniform','patch','sample','buffer',
              'shared','usamplercube','usampler1darray','usampler2darray'],
             [],
             TpvTextEditor.TSyntaxHighlighting.TAttributes.Keyword);
 AddRule('^['#32#9']*\#',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick,TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsPreprocessorLine,TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsMaybeCPreprocessorMultiLine],TpvTextEditor.TSyntaxHighlighting.TAttributes.Preprocessor);
 AddRule('['#32#9']+',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.WhiteSpace);
 AddRule('\/\*.*\*\/',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('\/\*.*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule('//[^'#10#13']*['#10#13']?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Comment);
 AddRule(TpvRawByteString('[A-Za-z\_\$'#128'-'#255'][A-Za-z0-9\_\$'#128'-'#255']*'),[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsKeyword],TpvTextEditor.TSyntaxHighlighting.TAttributes.Identifier);
 AddRule('[0-9]+(\.[0-9]+)?([Ee][\+\-]?[0-9]*)?([DdFf]|[Ll][Dd])?',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('0[xX][0-9A-Fa-f]*[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('[0-9A-Fa-f]+[LlUu]*',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Number);
 AddRule('\"([^\"\\]|\\.)*\"',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\"([^\"\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\''',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IsQuick],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('\''([^\''\\]|\\.)*\\?$',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.String_);
 AddRule('[\%\-\+\/\&\*\=\<\>\|\!\~\^]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Operator);
 AddRule('[\,\;\.\?\:\\]',[],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\[|\(|\{',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.IncreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
 AddRule('\]|\)|\}',[TpvTextEditor.TDFASyntaxHighlighting.TAccept.TFlag.DecreaseLevel],TpvTextEditor.TSyntaxHighlighting.TAttributes.Delimiter);
end;

constructor TpvTextEditor.TCodePointSet.TCodePointRange.Create(const aCodePoint:TpvUInt32);
begin
 fFromCodePoint:=aCodePoint;
 fToCodePoint:=aCodePoint;
end;

constructor TpvTextEditor.TCodePointSet.TCodePointRange.Create(const aFromCodePoint,aToCodePoint:TpvUInt32);
begin
 fFromCodePoint:=aFromCodePoint;
 fToCodePoint:=aToCodePoint;
end;

class function TpvTextEditor.TCodePointSet.CreateEmpty:TCodePointSet;
begin
 result.fRanges:=nil;
end;

constructor TpvTextEditor.TCodePointSet.Create(const aCodePointRanges:array of TCodePointRange);
begin
 SetLength(fRanges,length(aCodePointRanges));
 if length(fRanges)>0 then begin
  Move(aCodePointRanges[0],fRanges[0],length(fRanges)*SizeOf(TCodePointRange));
 end;
 Optimize;
end;

constructor TpvTextEditor.TCodePointSet.Create(const aCodePoints:array of TpvUInt32);
var Index:TpvSizeInt;
    CodePointRanges:TCodePointRanges;
begin
 SetLength(CodePointRanges,length(aCodePoints));
 for Index:=0 to length(aCodePoints)-1 do begin
  CodePointRanges[Index]:=TCodePointRange.Create(aCodePoints[Index]);
 end;
 Create(CodePointRanges);
end;

constructor TpvTextEditor.TCodePointSet.Create(const aCharSet:TCharSet);
var Count:TpvSizeInt;
    CurrentChar:AnsiChar;
begin
 Count:=0;
 for CurrentChar in aCharSet do begin
  inc(Count);
 end;
 SetLength(fRanges,Count);
 Count:=0;
 for CurrentChar in aCharSet do begin
  fRanges[Count]:=TpvTextEditor.TCodePointSet.TCodePointRange.Create(ord(CurrentChar));
  inc(Count);
 end;
 Optimize;
end;

procedure TpvTextEditor.TCodePointSet.Sort;
var Index,Count:TpvSizeInt;
    CodePointRange:TCodePointRange;
begin
 Index:=0;
 Count:=length(fRanges);
 while (Index+1)<Count do begin
  if (fRanges[Index].fFromCodePoint>fRanges[Index+1].fFromCodePoint) or
     ((fRanges[Index].fFromCodePoint=fRanges[Index+1].fFromCodePoint) and
      (fRanges[Index].fToCodePoint>fRanges[Index+1].fToCodePoint)) then begin
   CodePointRange:=fRanges[Index];
   fRanges[Index]:=fRanges[Index+1];
   fRanges[Index+1]:=CodePointRange;
   if Index>0 then begin
    dec(Index);
   end else begin
    inc(Index);
   end;
  end else begin
   inc(Index);
  end;
 end;
end;

procedure TpvTextEditor.TCodePointSet.Optimize;
var Index,Count:TpvSizeInt;
    NewCodePointRanges:TCodePointRanges;
    NewRange,Range:PCodePointRange;
begin
 Sort;
 SetLength(NewCodePointRanges,length(fRanges));
 Count:=0;
 try
  NewRange:=nil;
  for Index:=0 to length(fRanges)-1 do begin
   Range:=@fRanges[Index];
   if assigned(NewRange) and
      (((Range^.fFromCodePoint<=NewRange^.fToCodePoint) and
        (NewRange^.fFromCodePoint<=Range^.fToCodePoint)) or
       ((NewRange^.fToCodePoint+1)=Range^.fFromCodePoint)) then begin
    if NewRange^.fFromCodePoint>Range^.fFromCodePoint then begin
     NewRange^.fFromCodePoint:=Range^.fFromCodePoint;
    end;
    if NewRange^.fToCodePoint<Range^.fToCodePoint then begin
     NewRange^.fToCodePoint:=Range^.fToCodePoint;
    end;
   end else begin
    NewRange:=@NewCodePointRanges[Count];
    inc(Count);
    NewRange^:=Range^;
   end;
  end;
 finally
  try
   fRanges:=copy(NewCodePointRanges,0,Count);
  finally
   NewCodePointRanges:=nil;
  end;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Add(const aCodePointSet,aOtherCodePointSet:TCodePointSet):TCodePointSet;
begin
 result:=aCodePointSet+aOtherCodePointSet.fRanges;
end;

class operator TpvTextEditor.TCodePointSet.Add(const aCodePointSet:TCodePointSet;const aOtherCodePointSets:array of TCodePointSet):TCodePointSet;
var CodePointSet:TCodePointSet;
begin
 result.fRanges:=copy(aCodePointSet.fRanges);
 for CodePointSet in aOtherCodePointSets do begin
  result:=result+CodePointSet;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Add(const aCodePointSet:TCodePointSet;const aCodePointRange:TCodePointRange):TCodePointSet;
var Index,InsertIndex:TpvSizeInt;
begin
 SetLength(result.fRanges,length(aCodePointSet.fRanges)+1);
 InsertIndex:=0;
 for Index:=length(aCodePointSet.fRanges)-1 downto 0 do begin
  if (aCodePointRange.fFromCodePoint>aCodePointSet.fRanges[Index].fFromCodePoint) or
      ((aCodePointRange.fFromCodePoint=aCodePointSet.fRanges[Index].fFromCodePoint) and
       (aCodePointRange.fToCodePoint>aCodePointSet.fRanges[Index].fToCodePoint)) then begin
   InsertIndex:=Index;
   break;
  end;
 end;
 for Index:=0 to InsertIndex-1 do begin
  result.fRanges[Index]:=aCodePointSet.fRanges[Index];
 end;
 result.fRanges[InsertIndex]:=aCodePointRange;
 for Index:=InsertIndex+1 to length(result.fRanges)-1 do begin
  result.fRanges[Index]:=aCodePointSet.fRanges[Index-1];
 end;
 result.Optimize;
end;

class operator TpvTextEditor.TCodePointSet.Add(const aCodePointSet:TCodePointSet;const aCodePointRanges:array of TCodePointRange):TCodePointSet;
var CodePointRange:TCodePointRange;
begin
 result.fRanges:=copy(aCodePointSet.fRanges);
 for CodePointRange in aCodePointRanges do begin
  result:=result+CodePointRange;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Add(const aCodePointSet:TCodePointSet;const aCodePoint:TpvUInt32):TCodePointSet;
begin
 result:=aCodePointSet+TCodePointRange.Create(aCodePoint);
end;

class operator TpvTextEditor.TCodePointSet.Add(const aCodePointSet:TCodePointSet;const aCodePoints:array of TpvUInt32):TCodePointSet;
var CodePoint:TpvUInt32;
begin
 result.fRanges:=copy(aCodePointSet.fRanges);
 for CodePoint in aCodePoints do begin
  result:=result+CodePoint;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Subtract(const aCodePointSet,aOtherCodePointSet:TCodePointSet):TCodePointSet;
var Count:TpvSizeInt;
    First,Min,Max:TpvUInt32;
    RangeA,RangeB:TCodePointRange;
begin
 result.fRanges:=nil;
 Count:=0;
 try
  for RangeA in aCodePointSet.fRanges do begin
   First:=RangeA.fFromCodePoint;
   for RangeB in aOtherCodePointSet.fRanges do begin
    if (First<=RangeB.fToCodePoint) and (RangeB.fFromCodePoint<=RangeA.fToCodePoint) then begin
     if First>RangeB.fFromCodePoint then begin
      Min:=First;
     end else begin
      Min:=RangeB.fFromCodePoint;
     end;
     if RangeA.fToCodePoint<RangeB.fToCodePoint then begin
      Max:=RangeA.fToCodePoint;
     end else begin
      Max:=RangeB.fToCodePoint;
     end;
     if First<Min then begin
      if length(result.fRanges)<=Count then begin
       SetLength(result.fRanges,(Count+1)*2);
      end;
      result.fRanges[Count]:=TCodePointRange.Create(First,Min-1);
      inc(Count);
     end;
     First:=Max+1;
    end;
   end;
   if First<=RangeA.fToCodePoint then begin
    if length(result.fRanges)<=Count then begin
     SetLength(result.fRanges,(Count+1)*2);
    end;
    result.fRanges[Count]:=TCodePointRange.Create(First,RangeA.fToCodePoint);
    inc(Count);
   end;
  end;
 finally
  try
   SetLength(result.fRanges,Count);
  finally
   result.Optimize;
  end;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Subtract(const aCodePointSet:TCodePointSet;const aOtherCodePointSets:array of TCodePointSet):TCodePointSet;
var CodePointSet:TCodePointSet;
begin
 result.fRanges:=copy(aCodePointSet.fRanges);
 for CodePointSet in aOtherCodePointSets do begin
  result:=result-CodePointSet;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Subtract(const aCodePointSet:TCodePointSet;const aCodePointRange:TCodePointRange):TCodePointSet;
var CodePointRanges:TCodePointRanges;
begin
 SetLength(CodePointRanges,1);
 CodePointRanges[0]:=aCodePointRange;
 result:=aCodePointSet-TCodePointSet.Create(CodePointRanges);
end;

class operator TpvTextEditor.TCodePointSet.Subtract(const aCodePointSet:TCodePointSet;const aCodePointRanges:array of TCodePointRange):TCodePointSet;
begin
 result:=aCodePointSet-TCodePointSet.Create(aCodePointRanges);
end;

class operator TpvTextEditor.TCodePointSet.Subtract(const aCodePointSet:TCodePointSet;const aCodePoint:TpvUInt32):TCodePointSet;
begin
 result:=aCodePointSet-TCodePointSet.Create([aCodePoint]);
end;

class operator TpvTextEditor.TCodePointSet.Subtract(const aCodePointSet:TCodePointSet;const aCodePoints:array of TpvUInt32):TCodePointSet;
begin
 result:=aCodePointSet-TCodePointSet.Create(aCodePoints);
end;

class operator TpvTextEditor.TCodePointSet.Multiply(const aCodePointSet,aOtherCodePointSet:TCodePointSet):TCodePointSet;
var Count:TpvSizeInt;
    Min,Max:TpvUInt32;
    RangeA,RangeB:TCodePointRange;
begin
 result.fRanges:=nil;
 Count:=0;
 try
  if (length(aCodePointSet.fRanges)>0) and
     (length(aOtherCodePointSet.fRanges)>0) and
     ((aCodePointSet.fRanges[0].fFromCodePoint<=aOtherCodePointSet.fRanges[length(aOtherCodePointSet.fRanges)-1].fToCodePoint) and
      (aOtherCodePointSet.fRanges[0].fFromCodePoint<=aCodePointSet.fRanges[length(aCodePointSet.fRanges)-1].fToCodePoint)) then begin
   for RangeA in aCodePointSet.fRanges do begin
    for RangeB in aOtherCodePointSet.fRanges do begin
     if (RangeA.fFromCodePoint<=RangeB.fToCodePoint) and (RangeB.fFromCodePoint<=RangeA.fToCodePoint) then begin
      if RangeA.fFromCodePoint>RangeB.fFromCodePoint then begin
       Min:=RangeA.fFromCodePoint;
      end else begin
       Min:=RangeB.fFromCodePoint;
      end;
      if RangeA.fToCodePoint<RangeB.fToCodePoint then begin
       Max:=RangeA.fToCodePoint;
      end else begin
       Max:=RangeB.fToCodePoint;
      end;
      if Min<=Max then begin
       if length(result.fRanges)<=Count then begin
        SetLength(result.fRanges,(Count+1)*2);
       end;
       result.fRanges[Count]:=TCodePointRange.Create(Min,Max);
       inc(Count);
      end;
     end;
    end;
   end;
  end;
 finally
  try
   SetLength(result.fRanges,Count);
  finally
   result.Optimize;
  end;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Multiply(const aCodePointSet:TCodePointSet;const aOtherCodePointSets:array of TCodePointSet):TCodePointSet;
var CodePointSet:TCodePointSet;
begin
 result.fRanges:=copy(aCodePointSet.fRanges);
 for CodePointSet in aOtherCodePointSets do begin
  result:=result*CodePointSet;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Multiply(const aCodePointSet:TCodePointSet;const aCodePointRange:TCodePointRange):TCodePointSet;
var CodePointRanges:TCodePointRanges;
begin
 SetLength(CodePointRanges,1);
 CodePointRanges[0]:=aCodePointRange;
 result:=aCodePointSet*TCodePointSet.Create(CodePointRanges);
end;

class operator TpvTextEditor.TCodePointSet.Multiply(const aCodePointSet:TCodePointSet;const aCodePointRanges:array of TCodePointRange):TCodePointSet;
begin
 result:=aCodePointSet*TCodePointSet.Create(aCodePointRanges);
end;

class operator TpvTextEditor.TCodePointSet.Multiply(const aCodePointSet:TCodePointSet;const aCodePoint:TpvUInt32):TCodePointSet;
begin
 result:=aCodePointSet*TCodePointSet.Create([aCodePoint]);
end;

class operator TpvTextEditor.TCodePointSet.Multiply(const aCodePointSet:TCodePointSet;const aCodePoints:array of TpvUInt32):TCodePointSet;
begin
 result:=aCodePointSet*TCodePointSet.Create(aCodePoints);
end;

class operator TpvTextEditor.TCodePointSet.Equal(const aCodePointSet,aOtherCodePointSet:TCodePointSet):boolean;
var Index:TpvSizeInt;
begin
 result:=length(aCodePointSet.fRanges)=length(aOtherCodePointSet.fRanges);
 if result then begin
  for Index:=0 to length(aCodePointSet.fRanges)-1 do begin
   if (aCodePointSet.fRanges[Index].fFromCodePoint<>aOtherCodePointSet.fRanges[Index].fFromCodePoint) or
      (aCodePointSet.fRanges[Index].fToCodePoint<>aOtherCodePointSet.fRanges[Index].fToCodePoint) then begin
    result:=false;
    break;
   end;
  end;
 end;
end;

class operator TpvTextEditor.TCodePointSet.NotEqual(const aCodePointSet,aOtherCodePointSet:TCodePointSet):boolean;
var Index:TpvSizeInt;
begin
 result:=length(aCodePointSet.fRanges)<>length(aOtherCodePointSet.fRanges);
 if not result then begin
  for Index:=0 to length(aCodePointSet.fRanges)-1 do begin
   if (aCodePointSet.fRanges[Index].fFromCodePoint<>aOtherCodePointSet.fRanges[Index].fFromCodePoint) or
      (aCodePointSet.fRanges[Index].fToCodePoint<>aOtherCodePointSet.fRanges[Index].fToCodePoint) then begin
    result:=true;
    break;
   end;
  end;
 end;
end;

class operator TpvTextEditor.TCodePointSet.LogicalNot(const aCodePointSet:TCodePointSet):TCodePointSet;
var Count:TpvSizeInt;
    First,Min,Max:TpvUInt32;
    RangeA,RangeB:TCodePointRange;
begin
 if length(aCodePointSet.fRanges)=0 then begin
  result:=TCodePointSet.Create(TCodePointRange.Create(0,$ffffffff));
 end else if (length(aCodePointSet.fRanges)=1) and
             (aCodePointSet.fRanges[0].fFromCodePoint=0) and
             (aCodePointSet.fRanges[0].fToCodePoint=$ffffffff) then begin
  result.fRanges:=nil;
 end else begin
  // inlined: result:=TCodePointSet.Create(TCodePointRange.Create(0,$ffffffff))-aCodePointSet;
  result.fRanges:=nil;
  Count:=0;
  try
   RangeA:=TCodePointRange.Create(0,$ffffffff);
   First:=RangeA.fFromCodePoint;
   for RangeB in aCodePointSet.fRanges do begin
    if (First<=RangeB.fToCodePoint) and (RangeB.fFromCodePoint<=RangeA.fToCodePoint) then begin
     if First>RangeB.fFromCodePoint then begin
      Min:=First;
     end else begin
      Min:=RangeB.fFromCodePoint;
     end;
     if RangeA.fToCodePoint<RangeB.fToCodePoint then begin
      Max:=RangeA.fToCodePoint;
     end else begin
      Max:=RangeB.fToCodePoint;
     end;
     if First<Min then begin
      if length(result.fRanges)<=Count then begin
       SetLength(result.fRanges,(Count+1)*2);
      end;
      result.fRanges[Count]:=TCodePointRange.Create(First,Min-1);
      inc(Count);
     end;
     First:=Max+1;
    end;
   end;
   if First<=RangeA.fToCodePoint then begin
    if length(result.fRanges)<=Count then begin
     SetLength(result.fRanges,(Count+1)*2);
    end;
    result.fRanges[Count]:=TCodePointRange.Create(First,RangeA.fToCodePoint);
    inc(Count);
   end;
  finally
   try
    SetLength(result.fRanges,Count);
   finally
    result.Optimize;
   end;
  end;
 end;
end;

class operator TpvTextEditor.TCodePointSet.Negative(const aCodePointSet:TCodePointSet):TCodePointSet;
begin
 result:=not aCodePointSet;
end;

class operator TpvTextEditor.TCodePointSet.In(const aCodePoint:TpvUInt32;const aCodePointSet:TCodePointSet):boolean;
var Range:TCodePointRange;
begin
 for Range in aCodePointSet.fRanges do begin
  if (Range.fFromCodePoint<=aCodePoint) and (aCodePoint<=Range.fToCodePoint) then begin
   result:=true;
   exit;
  end;
 end;
 result:=false;
end;

function TpvTextEditor.TCodePointSet.ToCaseInsensitive:TCodePointSet;
var UsedCodePointSet,CodePointSetToAdd:TCodePointSet;
    Range:TCodePointRange;
    CodePoint:TpvUInt32;
var CodePointRanges:TCodePointRanges;
begin
 SetLength(CodePointRanges,2);
 CodePointRanges[0]:=TCodePointRange.Create(ord('A'),ord('Z'));
 CodePointRanges[1]:=TCodePointRange.Create(ord('a'),ord('z'));
 UsedCodePointSet:=TCodePointSet.Create(CodePointRanges);
 CodePointSetToAdd:=TCodePointSet.CreateEmpty;
 for Range in UsedCodePointSet.fRanges do begin
  for CodePoint:=Range.fFromCodePoint to Range.fToCodePoint do begin
   case CodePoint of
    ord('A')..ord('Z'):Begin
     CodePointSetToAdd:=CodePointSetToAdd+TpvUInt32(CodePoint+(ord('a')-ord('A')));
    end;
    ord('a')..ord('z'):Begin
     CodePointSetToAdd:=CodePointSetToAdd+TpvUInt32(CodePoint-(ord('a')-ord('A')));
    end;
   end;
  end;
 end;
 result:=Self+CodePointSetToAdd;
end;

function TpvTextEditor.TCodePointSet.ToLowerCase:TCodePointSet;
var UsedCodePointSet,CodePointSetToAdd,CodePointSetToSubtract:TCodePointSet;
    Range:TCodePointRange;
    CodePoint:TpvUInt32;
begin
 UsedCodePointSet:=TCodePointSet.Create(TCodePointRange.Create(ord('A'),ord('Z')));
 CodePointSetToAdd:=TCodePointSet.CreateEmpty;
 CodePointSetToSubtract:=TCodePointSet.CreateEmpty;
 for Range in UsedCodePointSet.fRanges do begin
  for CodePoint:=Range.fFromCodePoint to Range.fToCodePoint do begin
   CodePointSetToSubtract:=CodePointSetToSubtract+CodePoint;
   CodePointSetToAdd:=CodePointSetToAdd+TpvUInt32(CodePoint+(ord('a')-ord('A')));
  end;
 end;
 result:=(Self-CodePointSetToSubtract)+CodePointSetToAdd;
end;

function TpvTextEditor.TCodePointSet.ToUpperCase:TCodePointSet;
var UsedCodePointSet,CodePointSetToAdd,CodePointSetToSubtract:TCodePointSet;
    Range:TCodePointRange;
    CodePoint:TpvUInt32;
begin
 UsedCodePointSet:=TCodePointSet.Create(TCodePointRange.Create(ord('a'),ord('z')));
 CodePointSetToAdd:=TCodePointSet.CreateEmpty;
 CodePointSetToSubtract:=TCodePointSet.CreateEmpty;
 for Range in UsedCodePointSet.fRanges do begin
  for CodePoint:=Range.fFromCodePoint to Range.fToCodePoint do begin
   CodePointSetToSubtract:=CodePointSetToSubtract+CodePoint;
   CodePointSetToAdd:=CodePointSetToAdd+TpvUInt32(CodePoint-(ord('a')-ord('A')));
  end;
 end;
 result:=(Self-CodePointSetToSubtract)+CodePointSetToAdd;
end;

function TpvTextEditor.TRegularExpression.TCodePointWindow.GetCodePoint(const aOffset:TpvSizeInt):TpvUInt32;
begin
 result:=fCodePoints[(fOffset+aOffset) and 3];
end;

procedure TpvTextEditor.TRegularExpression.TCodePointWindow.SetCodePoint(const aOffset:TpvSizeInt;const aCodePoint:TpvUInt32);
begin
 fCodePoints[(fOffset+aOffset) and 3]:=aCodePoint;
end;

procedure TpvTextEditor.TRegularExpression.TCodePointWindow.Reset;
begin
 fCodePoints[0]:=$ffffffff;
 fCodePoints[1]:=$ffffffff;
 fCodePoints[2]:=$ffffffff;
 fCodePoints[3]:=$ffffffff;
 fOffset:=1;
end;

procedure TpvTextEditor.TRegularExpression.TCodePointWindow.Advance;
begin
 fOffset:=(fOffset+1) and 3;
end;

constructor TpvTextEditor.TRegularExpression.Create(const aParent:TpvTextEditor;const aRegularExpression:TpvUTF8String;const aFlags:TRegularExpressionFlags=[]);
begin
 inherited Create;

 fParent:=aParent;

 fGeneration:=0;
 fCountParens:=0;

 fAnchoredRootNode:=nil;

 fUnanchoredRootNode:=nil;

 fNodes:=TList.Create;

 fInstructions:=nil;
 fCountInstructions:=0;

 fAnchoredStartInstruction:=nil;
 fUnanchoredStartInstruction:=nil;
 fReversedStartInstruction:=nil;

 fCharClasses:=nil;
 fCountCharClasses:=0;

 fThreadLists[0].Threads:=nil;
 fThreadLists[1].Threads:=nil;

 fFreeSubMatches:=nil;

 fAllSubMatches:=TList.Create;

 fRegularExpression:=aRegularExpression;

 fFlags:=aFlags;

 fBeginningJump:=false;
 fBeginningSplit:=false;
 fBeginningWildCard:=false;
 fBeginningAnchor:=false;

 fNamedGroupStringList:=TStringList.Create;

 try

  try

   Parse;

   Compile;

  finally
   SetLength(fCharClasses,fCountCharClasses);
  end;

  fCountSubMatches:=(fCountParens+1)*2;

  SetLength(fThreadLists[0].Threads,(fCountInstructions+1)*4);
  SetLength(fThreadLists[1].Threads,(fCountInstructions+1)*4);

  fBeginningWildcardLoop:=fBeginningJump and fBeginningSplit and fBeginningWildCard;
  if TRegularExpressionFlag.Lazy in fFlags then begin
   fDoUnanchoredStart:=false;
  end else if TRegularExpressionFlag.Greedy in fFlags then begin
   fDoUnanchoredStart:=not fBeginningAnchor;
  end else begin
   fDoUnanchoredStart:=not fBeginningAnchor;
  end;

 finally
 end;

end;

destructor TpvTextEditor.TRegularExpression.Destroy;
var Index:TpvInt32;
    SubMatches:PRegularExpressionSubMatches;
begin

 for Index:=0 to fNodes.Count-1 do begin
  FreeMem(fNodes[Index]);
 end;
 FreeAndNil(fNodes);

 SetLength(fInstructions,0);

 for Index:=0 to fCountCharClasses-1 do begin
  FreeMem(fCharClasses[Index]);
 end;
 SetLength(fCharClasses,0);
 fCountCharClasses:=0;

 SetLength(fThreadLists[0].Threads,0);
 SetLength(fThreadLists[1].Threads,0);

 while assigned(fFreeSubMatches) do begin
  SubMatches:=fFreeSubMatches;
  fFreeSubMatches:=fFreeSubMatches^.Next;
  SetLength(SubMatches^.SubMatches,0);
  Finalize(SubMatches^);
  FreeMem(SubMatches);
 end;
 fFreeSubMatches:=nil;

 FreeAndNil(fAllSubMatches);

 fNamedGroupStringList.Free;

 inherited Destroy;
end;

class function TpvTextEditor.TRegularExpression.Escape(const aString:TpvUTF8String):TpvUTF8String;
var Index,Len:TpvSizeInt;
    CodePoint:TpvUInt32;
begin
 result:='';
 Index:=1;
 Len:=length(aString);
 while Index<=Len do begin
  CodePoint:=TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(aString,Index);
  case CodePoint of
   33..47,58..64,91..96,123..126:begin
    result:=result+'\'+TpvTextEditor.TUTF8Utils.UTF32CharToUTF8(CodePoint);
   end;
   else begin
    result:=result+TpvTextEditor.TUTF8Utils.UTF32CharToUTF8(CodePoint);
   end;
  end;
 end;
end;

function TpvTextEditor.TRegularExpression.NewNode(const aNodeType:TpvInt32;const aLeft,aRight:PRegularExpressionNode;const aValue:TpvInt32):PRegularExpressionNode;
begin
 GetMem(result,SizeOf(TRegularExpressionNode));
 FillChar(result^,SizeOf(TRegularExpressionNode),#0);
 result^.NodeType:=aNodeType;
 result^.Left:=aLeft;
 result^.Right:=aRight;
 result^.Value:=aValue;
 result^.Index:=fNodes.Add(result);
end;

procedure TpvTextEditor.TRegularExpression.FreeNode(var aNode:PRegularExpressionNode);
begin
 if assigned(aNode) then begin
  if assigned(aNode^.Left) then begin
   FreeNode(aNode^.Left);
  end;
  if assigned(aNode^.Right) then begin
   FreeNode(aNode^.Right);
  end;
  FreeMem(aNode);
  aNode:=nil;
 end;
end;

function TpvTextEditor.TRegularExpression.AreNodesEqual(aNodeA,aNodeB:PRegularExpressionNode):boolean;
begin
 result:=(aNodeA=aNodeB) or
         ((((assigned(aNodeA) and assigned(aNodeB))) and
          ((aNodeA^.NodeType=aNodeB^.NodeType) and
           ((aNodeA^.Value=aNodeB^.Value) and
            (AreNodesEqual(aNodeA^.Left,aNodeB^.Left) and AreNodesEqual(aNodeA^.Right,aNodeB^.Right))))) or
          not (assigned(aNodeA) or assigned(aNodeB)));
end;

function TpvTextEditor.TRegularExpression.AreNodesEqualSafe(aNodeA,aNodeB:PRegularExpressionNode):boolean;
begin
 result:=(aNodeA=aNodeB) or
         ((((assigned(aNodeA) and assigned(aNodeB))) and
           (((aNodeA^.NodeType=aNodeB^.NodeType) and not (aNodeB^.NodeType in [ntPAREN])) and
            ((aNodeA^.Value=aNodeB^.Value) and
             (AreNodesEqualSafe(aNodeA^.Left,aNodeB^.Left) and
              AreNodesEqualSafe(aNodeA^.Right,aNodeB^.Right))))) or
          not (assigned(aNodeA) or assigned(aNodeB)));
end;

function TpvTextEditor.TRegularExpression.Concat(aNodeLeft,aNodeRight:PRegularExpressionNode):PRegularExpressionNode;
begin
 if assigned(aNodeLeft) and assigned(aNodeRight) then begin
  if (aNodeLeft^.NodeType=ntZEROWIDTH) and (aNodeRight^.NodeType=ntZEROWIDTH) then begin
   aNodeLeft^.Value:=aNodeLeft^.Value or aNodeRight^.Value;
   result:=aNodeLeft;
  end else if ((aNodeLeft^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (aNodeRight^.NodeType=ntPLUS)) and AreNodesEqualSafe(aNodeLeft^.Left,aNodeRight^.Left) and (aNodeLeft^.Value=0) and (aNodeRight^.Value=0) then begin
   result:=aNodeRight;
  end else if ((aNodeLeft^.NodeType in [ntSTAR,ntPLUS]) and (aNodeRight^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(aNodeLeft^.Left,aNodeRight^.Left) and (aNodeLeft^.Value=0) and (aNodeRight^.Value=0) then begin
   result:=aNodeLeft;
  end else if (aNodeLeft^.NodeType=ntCAT) and assigned(aNodeLeft^.Left) and assigned(aNodeLeft^.Right) then begin
   if (aNodeLeft^.Right^.NodeType=ntZEROWIDTH) and (aNodeRight^.NodeType=ntZEROWIDTH) then begin
    aNodeLeft^.Right^.Value:=aNodeLeft^.Right^.Value or aNodeRight^.Value;
    result:=aNodeLeft;
   end else if ((aNodeLeft^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (aNodeRight^.NodeType=ntPLUS)) and AreNodesEqualSafe(aNodeLeft^.Right^.Left,aNodeRight^.Left) and (aNodeLeft^.Right^.Value=0) and (aNodeRight^.Value=0) then begin
    aNodeLeft^.Right:=aNodeRight;
    result:=aNodeLeft;
   end else if ((aNodeLeft^.Right^.NodeType in [ntSTAR,ntPLUS]) and (aNodeRight^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(aNodeLeft^.Right^.Left,aNodeRight^.Left) and (aNodeLeft^.Right^.Value=0) and (aNodeRight^.Value=0) then begin
    result:=aNodeLeft;
   end else begin
    result:=NewNode(ntCAT,aNodeLeft,aNodeRight,0);
   end;
  end else begin
   result:=NewNode(ntCAT,aNodeLeft,aNodeRight,0);
  end;
 end else begin
  if assigned(aNodeLeft) then begin
   result:=aNodeLeft;
  end else if assigned(aNodeRight) then begin
   result:=aNodeRight;
  end else begin
   result:=nil;
  end;
 end;
{ if not (rfONLYFASTOPTIMIZATIONS in Flags) then}begin
  while (assigned(result) and (result^.NodeType=ntCAT)) and (assigned(result^.Left) and assigned(result^.Right)) do begin
   if (result^.Left^.NodeType=ntCAT) and (result^.Right^.NodeType=ntZEROWIDTH) and assigned(result^.Left^.Right) and (result^.Left^.Right^.NodeType=ntZEROWIDTH) then begin
    result^.Left^.Right^.Value:=result^.Left^.Right^.Value or result^.Right^.Value;
    result:=result^.Left;
    continue;
   end else if (result^.Left^.NodeType=ntZEROWIDTH) and (result^.Right^.NodeType=ntCAT) and assigned(result^.Right^.Left) and (result^.Right^.Left^.NodeType=ntZEROWIDTH) then begin
    result^.Right^.Left^.Value:=result^.Right^.Left^.Value or result^.Left^.Value;
    result:=result^.Right;
    continue;
   end else if (result^.Left^.NodeType=ntCAT) and (result^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and assigned(result^.Left^.Right) and (result^.Right^.Value=0) then begin
    if ((result^.Left^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Right^.Left,result^.Right^.Left) then begin
     result^.Left^.Right:=result^.Right;
     result:=result^.Left;
     continue;
    end else if ((result^.Left^.Right^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Right^.Left,result^.Right^.Left) then begin
     result:=result^.Left;
     continue;
    end;
   end else if (result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntCAT) and assigned(result^.Right^.Left) and (result^.Left^.Value=0) then begin
    if ((result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.Left^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left^.Left) and (result^.Right^.Left^.Value=0) then begin
     result:=result^.Right;
     continue;
    end else if ((result^.Left^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.Left^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left^.Left) and (result^.Right^.Left^.Value=0) then begin
     result^.Right^.Left:=result^.Left;
     result:=result^.Right;
     continue;
    end;
   end else if (result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Left^.Value=0) and (result^.Right^.Value=0) then begin
    if ((result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left) then begin
     result:=result^.Right;
     continue;
    end else if ((result^.Left^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left) then begin
     result:=result^.Left;
     continue;
    end;
   end;
   break;
  end;
 end;
end;

function TpvTextEditor.TRegularExpression.NewAlt(aNodeLeft,aNodeRight:PRegularExpressionNode):PRegularExpressionNode;
var NodeEx,pl,pr:PPRegularExpressionNode;
    Node,l,r:PRegularExpressionNode;
begin
 if assigned(aNodeLeft) and assigned(aNodeRight) then begin
  if (aNodeLeft^.NodeType=ntCAT) and (aNodeRight^.NodeType=ntCAT) {and not (rfONLYFASTOPTIMIZATIONS in Flags)} then begin
   result:=NewNode(ntALT,aNodeLeft,aNodeRight,0);
   NodeEx:=@result;
   while (((assigned(NodeEx) and assigned(NodeEx^)) and (NodeEx^^.NodeType=ntALT)) and (assigned(NodeEx^^.Left) and assigned(NodeEx^^.Right))) and ((NodeEx^^.Left^.NodeType=ntCAT) and (NodeEx^^.Right^.NodeType=ntCAT)) do begin
    Node:=NodeEx^;
    pl:=@Node^.Left;
    l:=Node^.Left;
    while (assigned(l) and assigned(l^.Left)) and (l^.Left^.NodeType=ntCAT) do begin
     pl:=@l^.Left;
     l:=l^.Left;
    end;
    pr:=@Node^.Right;
    r:=Node^.Right;
    while (assigned(r) and assigned(r^.Left)) and (r^.Left^.NodeType=ntCAT) do begin
     pr:=@r^.Left;
     r:=r^.Left;
    end;
    if ((assigned(l^.Left) and assigned(l^.Right)) and (assigned(r^.Left) and assigned(r^.Right))) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
     NodeEx^:=l;
     pl^:=l^.Right;
     pr^:=r^.Right;
     l^.Right:=Node;
     NodeEx:=@l^.Right;
     continue;
    end;
    break;
   end;
  end else if AreNodesEqualSafe(aNodeLeft,aNodeRight) then begin
   result:=aNodeLeft;
  end else if (aNodeLeft^.NodeType=ntCHAR) and (aNodeRight^.NodeType=ntCHAR) then begin
   result:=NewNode(ntCHAR,nil,nil,0);
   result^.CharClass:=aNodeLeft^.CharClass+aNodeRight^.CharClass;
  end else begin
   result:=NewNode(ntALT,aNodeLeft,aNodeRight,0);
  end;
 end else begin
  if assigned(aNodeLeft) then begin
   result:=aNodeLeft;
  end else if assigned(aNodeRight) then begin
   result:=aNodeRight;
  end else begin
   result:=nil;
  end;
 end;
end;

{ * (a*)* equals a*
  * (a+)+ equals a+
  * (a?)? equals a?
  * (a*)+ equals (a+)* equals a*
  * (a*)? equals (a?)* equals a*
  * (a+)? equals (a?)+ equals a*
}
function TpvTextEditor.TRegularExpression.NewPlus(aNode:PRegularExpressionNode;aKind:TpvInt32):PRegularExpressionNode;
begin
 if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType=ntPAREN)) and ((assigned(aNode^.Left) and (aNode^.Left^.NodeType in [ntPLUS,ntSTAR])) and (aNode^.Left^.Value=qkGREEDY))) then begin
  result:=aNode;
 end else if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType=ntPLUS)) and (aNode^.Value=qkGREEDY)) then begin
  result:=aNode;
 end else if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType in [ntSTAR,ntQUEST])) and (aNode^.Value=qkGREEDY)) then begin
  result:=aNode;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntPLUS,aNode,nil,aKind);
 end;
end;

function TpvTextEditor.TRegularExpression.NewStar(aNode:PRegularExpressionNode;aKind:TpvInt32):PRegularExpressionNode;
begin
  if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType=ntPAREN)) and ((assigned(aNode^.Left) and (aNode^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and (aNode^.Left^.Value=qkGREEDY))) then begin
  result:=aNode;
  result^.Left^.NodeType:=ntSTAR;
 end else if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType in [ntPLUS,ntQUEST,ntSTAR])) and (aNode^.Value=qkGREEDY)) then begin
  result:=aNode;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntSTAR,StarDenull(aNode),nil,aKind);
 end;
end;

function TpvTextEditor.TRegularExpression.NewQuest(aNode:PRegularExpressionNode;aKind:TpvInt32):PRegularExpressionNode;
begin
 if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType=ntPAREN)) and ((assigned(aNode^.Left) and (aNode^.Left^.NodeType=ntQUEST)) and (aNode^.Left^.Value=qkGREEDY))) then begin
  result:=aNode;
 end else if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType=ntPAREN)) and ((assigned(aNode^.Left) and (aNode^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and (aNode^.Left^.Value=qkGREEDY))) then begin
  result:=aNode;
  result^.Left^.NodeType:=ntSTAR;
 end else if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType=ntQUEST)) and (aNode^.Value=qkGREEDY)) then begin
  result:=aNode;
 end else if (aKind=qkGREEDY) and ((assigned(aNode) and (aNode^.NodeType in [ntPLUS,ntSTAR])) and (aNode^.Value=qkGREEDY)) then begin
  result:=aNode;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntQUEST,aNode,nil,aKind);
 end;
end;

function TpvTextEditor.TRegularExpression.IsStarNullable(aNode:PRegularExpressionNode):boolean;
begin
 if assigned(aNode) then begin
  case aNode^.NodeType of
   ntSTAR:begin
    result:=aNode^.Value=qkGREEDY;
   end;
   ntALT:begin
    result:=(IsStarNullable(aNode^.Left) or IsStarNullable(aNode^.Right)) or not (assigned(aNode^.Left) and assigned(aNode^.Right));
   end;
   ntCAT:begin
    result:=IsStarNullable(aNode^.Left) and IsStarNullable(aNode^.Right);
   end;
   else begin
    result:=false;
   end;
  end;
 end else begin
  result:=false;
 end;
end;

function TpvTextEditor.TRegularExpression.StarDenull(aNode:PRegularExpressionNode):PRegularExpressionNode;
begin
 result:=aNode;
 if IsStarNullable(result) then begin
  case result^.NodeType of
   ntSTAR:begin
    result:=result^.Left;
   end;
   ntCAT:begin
    result^.NodeType:=ntALT;
    result^.Left:=StarDenull(result^.Left);
    result^.Right:=StarDenull(result^.Right);
   end;
   ntALT:begin
    result^.Left:=StarDenull(result^.Left);
    result^.Right:=StarDenull(result^.Right);
   end;
  end;
 end;
end;

procedure TpvTextEditor.TRegularExpression.Parse;
var SourcePosition,SourceLength:TpvSizeInt;
    Source:TpvUTF8String;
 function Hex2Value(const c:ansichar):TpvUInt32;
 begin
  case c of
   '0'..'9':begin
    result:=TpvUInt8(ansichar(c))-TpvUInt8(ansichar('0'))
   end;
   'a'..'f':begin
    result:=(TpvUInt8(ansichar(c))-TpvUInt8(ansichar('a')))+$a;
   end;
   'A'..'F':begin
    result:=(TpvUInt8(ansichar(c))-TpvUInt8(ansichar('A')))+$a;
   end;
   else begin
    result:=0;
   end;
  end;
 end;
 function ParseDisjunction:PRegularExpressionNode; forward;
 type TCharSet=set of ansichar;
 function GetCharClass(const CurrentCodePoint:TpvUInt32;out IsSingle:boolean;out SingleCodePoint:TpvUInt32):TRegularExpressionCharClass;
 begin
  case CurrentCodePoint of
   ord('0'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#0]);
    IsSingle:=true;
    SingleCodePoint:=0;
   end;
   ord('t'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#9]);
    IsSingle:=true;
    SingleCodePoint:=9;
   end;
   ord('n'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#10]);
    IsSingle:=true;
    SingleCodePoint:=10;
   end;
   ord('v'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#11]);
    IsSingle:=true;
    SingleCodePoint:=11;
   end;
   ord('f'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#12]);
    IsSingle:=true;
    SingleCodePoint:=12;
   end;
   ord('r'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#13]);
    IsSingle:=true;
    SingleCodePoint:=13;
   end;
   ord('a'):begin
    result:=TpvTextEditor.TCodePointSet.Create(['a'..'z','A'..'Z']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('A'):begin
    result:=not TpvTextEditor.TCodePointSet.Create(['a'..'z','A'..'Z']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('w'):begin
    result:=TpvTextEditor.TCodePointSet.Create(['a'..'z','A'..'Z','0'..'9','_']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('W'):begin
    result:=not TpvTextEditor.TCodePointSet.Create(['a'..'z','A'..'Z','0'..'9','_']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('s'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#9,#10,#13,#32]);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('S'):begin
    result:=not TpvTextEditor.TCodePointSet.Create([#9,#10,#13,#32]);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('d'):begin
    result:=TpvTextEditor.TCodePointSet.Create(['0'..'9']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('D'):begin
    result:=not TpvTextEditor.TCodePointSet.Create(['0'..'9']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('h'):begin
    result:=TpvTextEditor.TCodePointSet.Create(['0'..'9','a'..'f','A'..'F']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('H'):begin
    result:=not TpvTextEditor.TCodePointSet.Create(['0'..'9','a'..'f','A'..'F']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('c'):begin
    result:=TpvTextEditor.TCodePointSet.Create([#0..#31,#127]);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('C'):begin
    result:=not TpvTextEditor.TCodePointSet.Create([#0..#31,#127]);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('p'):begin
    result:=TpvTextEditor.TCodePointSet.Create(['!','"','#','%','&','''','(',')',';','<','=','>','?','[','\',']','*','+',',','-','.','/',':','^','_','{','|','}','~']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('P'):begin
    result:=not TpvTextEditor.TCodePointSet.Create(['!','"','#','%','&','''','(',')',';','<','=','>','?','[','\',']','*','+',',','-','.','/',':','^','_','{','|','}','~']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('l'):begin
    result:=TpvTextEditor.TCodePointSet.Create(['a'..'z']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('L'):begin
    result:=not TpvTextEditor.TCodePointSet.Create(['a'..'z']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('u'):begin
    result:=TpvTextEditor.TCodePointSet.Create(['A'..'Z']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   ord('U'):begin
    result:=not TpvTextEditor.TCodePointSet.Create(['A'..'Z']);
    IsSingle:=false;
    SingleCodePoint:=0;
   end;
   else begin
    result:=TpvTextEditor.TCodePointSet.Create([CurrentCodePoint]);
    IsSingle:=true;
    SingleCodePoint:=CurrentCodePoint;
   end;
  end;
 end;
 function GetCharClassPerName(const Name:TpvUTF8String):TRegularExpressionCharClass;
 begin
  if Name='alnum' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['a'..'z','A'..'Z','0'..'9']);
  end else if Name='alpha' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['a'..'z','A'..'Z']);
  end else if Name='ascii' then begin
   result:=TpvTextEditor.TCodePointSet.Create([#$00..#$7f]);
  end else if Name='blank' then begin
   result:=TpvTextEditor.TCodePointSet.Create([#9,#32]);
  end else if Name='cntrl' then begin
   result:=TpvTextEditor.TCodePointSet.Create([#$00..#$1f,#$7f]);
  end else if Name='digits' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['0'..'9']);
  end else if Name='graph' then begin
   result:=TpvTextEditor.TCodePointSet.Create([#$21..#$7e]);
  end else if Name='lower' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['a'..'z']);
  end else if Name='print' then begin
   result:=TpvTextEditor.TCodePointSet.Create([#$20..#$7e]);
  end else if Name='punct' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['!','"','#','$','%','&','''','(',')','*','+',
                                               ',','\','-','.','/',':',';','<','=','>','?',
                                               '@','[',']','^','_','`','{','|','}','~']);
  end else if Name='space' then begin
   result:=TpvTextEditor.TCodePointSet.Create([#9,#10..#13,#32]);
  end else if Name='upper' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['A'..'Z']);
  end else if Name='word' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['a'..'z','A'..'Z','0'..'9','_']);
  end else if Name='xdigit' then begin
   result:=TpvTextEditor.TCodePointSet.Create(['a'..'f','A'..'F','0'..'9']);
  end else begin
   result:=TpvTextEditor.TCodePointSet.CreateEmpty;
   raise ERegularExpression.Create('Syntax error');
  end;
 end;
 function ParseAtom:PRegularExpressionNode;
 var Value,Position:TpvSizeInt;
     Negate,IsSingle:boolean;
     StartCodePoint,EndCodePoint,CodePoint:TpvUInt32;
     Name:TpvUTF8String;
 begin
  result:=nil;
  try
   if SourcePosition<=SourceLength then begin
    case Source[SourcePosition] of
     '*','+','?',')',']','{','}','|':begin
      raise ERegularExpression.Create('Syntax error');
     end;
     '(':begin
      inc(SourcePosition);
      if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
       inc(SourcePosition);
       if SourcePosition<=SourceLength then begin
        case Source[SourcePosition] of
         ':':begin
          inc(SourcePosition);
          result:=ParseDisjunction;
         end;
         '''':begin
          inc(SourcePosition);
          Position:=SourcePosition;
          while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
           inc(SourcePosition);
          end;
          if Position<SourcePosition then begin
           Name:=copy(Source,Position,SourcePosition-Position);
          end else begin
           Name:='';
          end;
          if (SourcePosition<=SourceLength) and (Source[SourcePosition]='''') then begin
           inc(SourcePosition);
          end else begin
           raise ERegularExpression.Create('Syntax error');
          end;
          Value:=fCountParens;
          inc(fCountParens);
          if fNamedGroupStringList.IndexOfName(String(Name))<0 then begin
           fNamedGroupStringList.Add(String(Name)+fNamedGroupStringList.NameValueSeparator+IntToStr(Value));
          end;
          result:=NewNode(ntPAREN,ParseDisjunction,nil,0);
          result^.Value:=Value;
         end;
         '<':begin
          inc(SourcePosition);
          if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['!','=']) then begin
           raise ERegularExpression.Create('Syntax error');
          end else begin
           Position:=SourcePosition;
           while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
            inc(SourcePosition);
           end;
           if Position<SourcePosition then begin
            Name:=copy(Source,Position,SourcePosition-Position);
           end else begin
            Name:='';
           end;
           if (SourcePosition<=SourceLength) and (Source[SourcePosition]='>') then begin
            inc(SourcePosition);
           end else begin
            raise ERegularExpression.Create('Syntax error');
           end;
           Value:=fCountParens;
           inc(fCountParens);
           if fNamedGroupStringList.IndexOfName(String(Name))<0 then begin
            fNamedGroupStringList.Add(String(Name)+fNamedGroupStringList.NameValueSeparator+IntToStr(Value));
           end;
           result:=NewNode(ntPAREN,ParseDisjunction,nil,0);
           result^.Value:=Value;
          end;
         end;
         'P':begin
          inc(SourcePosition);
          if (SourcePosition<=SourceLength) and (Source[SourcePosition]='<') then begin
           inc(SourcePosition);
           Position:=SourcePosition;
           while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
            inc(SourcePosition);
           end;
           if Position<SourcePosition then begin
            Name:=copy(Source,Position,SourcePosition-Position);
           end else begin
            Name:='';
           end;
           if (SourcePosition<=SourceLength) and (Source[SourcePosition]='>') then begin
            inc(SourcePosition);
           end else begin
            raise ERegularExpression.Create('Syntax error');
           end;
           Value:=fCountParens;
           inc(fCountParens);
           if fNamedGroupStringList.IndexOfName(String(Name))<0 then begin
            fNamedGroupStringList.Add(String(Name)+fNamedGroupStringList.NameValueSeparator+IntToStr(Value));
           end;
           result:=NewNode(ntPAREN,ParseDisjunction,nil,0);
           result^.Value:=Value;
          end else begin
           raise ERegularExpression.Create('Syntax error');
          end;
         end;
         else begin
          raise ERegularExpression.Create('Syntax error');
         end;
        end;
       end else begin
        raise ERegularExpression.Create('Syntax error');
       end;
      end else begin
       Value:=fCountParens;
       inc(fCountParens);
       result:=NewNode(ntPAREN,ParseDisjunction,nil,0);
       result^.Value:=Value;
      end;
      if (SourcePosition<=SourceLength) and (Source[SourcePosition]=')') then begin
       inc(SourcePosition);
      end else begin
       raise ERegularExpression.Create('Syntax error');
      end;
     end;
     '\':begin
      inc(SourcePosition);
      if SourcePosition<=SourceLength then begin
       case Source[SourcePosition] of
        'b':begin
         result:=NewNode(ntBRK,nil,nil,0);
         inc(SourcePosition);
        end;
        'B':begin
         result:=NewNode(ntNBRK,nil,nil,0);
         inc(SourcePosition);
        end;
        'x','X':begin
         inc(SourcePosition);
         if (SourcePosition<=SourceLength) and
            (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']) then begin
          CodePoint:=0;
          repeat
           CodePoint:=(CodePoint shl 4) or Hex2Value(Source[SourcePosition]);
           inc(SourcePosition);
          until (SourcePosition>SourceLength) or not
                (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']);
          result:=NewNode(ntCHAR,nil,nil,0);
          result^.CharClass:=TpvTextEditor.TCodePointSet.Create([CodePoint]);
         end else begin
          raise ERegularExpression.Create('Syntax error');
         end;
        end;
        else begin
         result:=NewNode(ntCHAR,nil,nil,0);
         result^.CharClass:=GetCharClass(TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(Source,SourcePosition),IsSingle,StartCodePoint);
        end;
       end;
      end else begin
       raise ERegularExpression.Create('Syntax error');
      end;
     end;
     '.':begin
      result:=NewNode(ntDOT,nil,nil,0);
      inc(SourcePosition);
     end;
     '^':begin
      result:=NewNode(ntBOL,nil,nil,0);
      inc(SourcePosition);
     end;
     '$':begin
      result:=NewNode(ntEOL,nil,nil,0);
      inc(SourcePosition);
     end;
     '[':begin
      inc(SourcePosition);
      if (SourcePosition<=SourceLength) and (Source[SourcePosition]=':') then begin
       inc(SourcePosition);
       Position:=SourcePosition;
       while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
        inc(SourcePosition);
       end;
       if Position<SourcePosition then begin
        Name:=copy(Source,Position,SourcePosition-Position);
       end else begin
        Name:='';
       end;
       if ((SourcePosition+1)<=SourceLength) and ((Source[SourcePosition]=':') and (Source[SourcePosition+1]=']')) then begin
        inc(SourcePosition,2);
        result:=NewNode(ntCHAR,nil,nil,0);
        result^.CharClass:=GetCharClassPerName(Name);
       end else begin
        raise ERegularExpression.Create('Syntax error');
       end;
      end else begin
       result:=NewNode(ntCHAR,nil,nil,0);
       result^.CharClass:=TpvTextEditor.TCodePointSet.CreateEmpty;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='^') then begin
        inc(SourcePosition);
        Negate:=true;
       end else begin
        Negate:=false;
       end;
       StartCodePoint:=0;
       EndCodePoint:=0;
       while SourcePosition<=SourceLength do begin
        case Source[SourcePosition] of
         ']':begin
          break;
         end;
         '^':begin
          raise ERegularExpression.Create('Syntax error');
         end;
         '\':begin
          inc(SourcePosition);
          if SourcePosition<=SourceLength then begin
           case Source[SourcePosition] of
            'x','X':begin
             inc(SourcePosition);
             if (SourcePosition<=SourceLength) and
                (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']) then begin
              CodePoint:=0;
              repeat
               CodePoint:=(CodePoint shl 4) or Hex2Value(Source[SourcePosition]);
               inc(SourcePosition);
              until (SourcePosition>SourceLength) or not
                    (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']);
              StartCodePoint:=CodePoint;
              result^.CharClass:=result^.CharClass+TpvTextEditor.TCodePointSet.Create([StartCodePoint]);
             end else begin
              raise ERegularExpression.Create('Syntax error');
             end;
            end;
            else begin
             IsSingle:=false;
             result^.CharClass:=result^.CharClass+GetCharClass(TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(Source,SourcePosition),IsSingle,StartCodePoint);
             if not IsSingle then begin
              continue;
             end;
            end;
           end;
          end else begin
           raise ERegularExpression.Create('Syntax error');
          end;
         end;
         '[':begin
          inc(SourcePosition);
          if (SourcePosition<=SourceLength) and (Source[SourcePosition]=':') then begin
           inc(SourcePosition);
           Position:=SourcePosition;
           while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
            inc(SourcePosition);
           end;
           if Position<SourcePosition then begin
            Name:=copy(Source,Position,SourcePosition-Position);
           end else begin
            Name:='';
           end;
           if ((SourcePosition+1)<=SourceLength) and ((Source[SourcePosition]=':') and (Source[SourcePosition+1]=']')) then begin
            inc(SourcePosition,2);
            result^.CharClass:=result^.CharClass+GetCharClassPerName(Name);
            continue;
           end else begin
            raise ERegularExpression.Create('Syntax error');
           end;
          end else begin
           raise ERegularExpression.Create('Syntax error');
          end;
         end;
         '|','*','+','?','(',')','{','}',':','$':begin
          raise ERegularExpression.Create('Syntax error');
         end;
         else begin
          StartCodePoint:=TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(Source,SourcePosition);
         end;
        end;
        if (SourcePosition<=SourceLength) and (Source[SourcePosition]='-') then begin
         inc(SourcePosition);
         case Source[SourcePosition] of
          '\':begin
           inc(SourcePosition);
           if SourcePosition<=SourceLength then begin
            case Source[SourcePosition] of
             'x','X':begin
              inc(SourcePosition);
              if (SourcePosition<=SourceLength) and
                 (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']) then begin
               CodePoint:=0;
               repeat
                CodePoint:=(CodePoint shl 4) or Hex2Value(Source[SourcePosition]);
                inc(SourcePosition);
               until (SourcePosition>SourceLength) or not
                     (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']);
               EndCodePoint:=CodePoint;
              end else begin
               raise ERegularExpression.Create('Syntax error');
              end;
             end;
             else begin
              IsSingle:=false;
              result^.CharClass:=result^.CharClass+GetCharClass(TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(Source,SourcePosition),IsSingle,EndCodePoint);
              if not IsSingle then begin
               raise ERegularExpression.Create('Syntax error');
              end;
             end;
            end;
           end;
          end;
          '|','*','+','?','(',')','[',']','{','}',':','.','^','$':begin
           raise ERegularExpression.Create('Syntax error');
          end;
          else begin
           EndCodePoint:=TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(Source,SourcePosition);
          end;
         end;
         if EndCodePoint<StartCodePoint then begin
          raise ERegularExpression.Create('Syntax error');
         end else begin
          result^.CharClass:=result^.CharClass+TpvTextEditor.TCodePointSet.TCodePointRange.Create(StartCodePoint,EndCodePoint);
         end;
        end else begin
         result^.CharClass:=result^.CharClass+StartCodePoint;
        end;
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]=']') then begin
        inc(SourcePosition);
        if Negate then begin
         result^.CharClass:=not result^.CharClass;
        end;
        if length(result^.CharClass.fRanges)=0 then begin
         raise ERegularExpression.Create('Syntax error');
        end;
       end else begin
        raise ERegularExpression.Create('Syntax error');
       end;
      end;
     end;
     else begin
      result:=NewNode(ntCHAR,nil,nil,0);
      result^.CharClass:=TpvTextEditor.TCodePointSet.Create([TpvTextEditor.TUTF8Utils.UTF8GetCodePointAndIncFallback(Source,SourcePosition)]);
     end;
    end;
   end;
  except
   FreeNode(result);
   raise;
  end;
 end;
 function ParseTerm:PRegularExpressionNode;
 var MinCount,MaxCount:TpvInt32;
 begin
  result:=nil;
  try
   if SourcePosition<=SourceLength then begin
    result:=ParseAtom;
    if SourcePosition<=SourceLength then begin
     case Source[SourcePosition] of
      '*':begin
       inc(SourcePosition);
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
        inc(SourcePosition);
        result:=NewStar(result,qkLAZY);
        if TRegularExpressionFlag.Longest in fFlags then begin
         raise ERegularExpression.Create('Syntax error');
        end;
       end else begin
        if TRegularExpressionFlag.Longest in fFlags then begin
         result:=NewStar(result,qkLAZY);
        end else begin
         result:=NewStar(result,qkGREEDY);
        end;
       end;
      end;
      '+':begin
       inc(SourcePosition);
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
        inc(SourcePosition);
        result:=NewPlus(result,qkLAZY);
        if TRegularExpressionFlag.Longest in fFlags then begin
         raise ERegularExpression.Create('Syntax error');
        end;
       end else begin
        if TRegularExpressionFlag.Longest in fFlags then begin
         result:=NewPlus(result,qkLAZY);
        end else begin
         result:=NewPlus(result,qkGREEDY);
        end;
       end;
      end;
      '?':begin
       inc(SourcePosition);
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
        inc(SourcePosition);
        result:=NewQuest(result,qkLAZY);
        if TRegularExpressionFlag.Longest in fFlags then begin
         raise ERegularExpression.Create('Syntax error');
        end;
       end else begin
        if TRegularExpressionFlag.Longest in fFlags then begin
         result:=NewQuest(result,qkLAZY);
        end else begin
         result:=NewQuest(result,qkGREEDY);
        end;
       end;
      end;
      '{':begin
       inc(SourcePosition);
       if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) then begin
        MinCount:=0;
        while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) do begin
         MinCount:=(MinCount*10)+(TpvUInt8(ansichar(Source[SourcePosition]))-TpvUInt8(ansichar('0')));
         inc(SourcePosition);
        end;
       end else begin
        MinCount:=-1;
        raise ERegularExpression.Create('Syntax error');
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]=',') then begin
        inc(SourcePosition);
        if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) then begin
         MaxCount:=0;
         while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) do begin
          MaxCount:=(MaxCount*10)+(TpvUInt8(ansichar(Source[SourcePosition]))-TpvUInt8(ansichar('0')));
          inc(SourcePosition);
         end;
         if MinCount>MaxCount then begin
          raise ERegularExpression.Create('Syntax error');
         end;
        end else begin
         MaxCount:=-1;
        end;
       end else begin
        MaxCount:=MinCount;
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='}') then begin
        inc(SourcePosition);
        result:=NewNode(ntEXACT,result,nil,0);
        if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
         inc(SourcePosition);
         result^.Value:=1;
         if TRegularExpressionFlag.Longest in fFlags then begin
          raise ERegularExpression.Create('Syntax error');
         end;
        end else begin
         if TRegularExpressionFlag.Longest in fFlags then begin
          result^.Value:=1;
         end else begin
          result^.Value:=0;
         end;
        end;
        result^.MinCount:=MinCount;
        result^.MaxCount:=MaxCount;
       end else begin
        raise ERegularExpression.Create('Syntax error');
       end;
      end;
     end;
    end;
   end;
  except
   FreeNode(result);
   raise;
  end;
 end;
 function ParseAlternative:PRegularExpressionNode;
 var Node:PRegularExpressionNode;
 begin
  result:=nil;
  try
   while SourcePosition<=SourceLength do begin
    Node:=ParseTerm;
    if assigned(result) then begin
     result:=Concat(result,Node);
    end else begin
     result:=Node;
    end;
    if SourcePosition<=SourceLength then begin
     case Source[SourcePosition] of
      '|',')':begin
       break;
      end;
     end;
    end else begin
     break;
    end;
   end;
  except
   FreeNode(result);
   raise;
  end;
 end;
 function ParseDisjunction:PRegularExpressionNode;
 var Node:PRegularExpressionNode;
 begin
  result:=nil;
  try
   while SourcePosition<=SourceLength do begin
    Node:=ParseAlternative;
    if assigned(result) then begin
     result:=NewAlt(result,Node);
    end else begin
     result:=Node;
    end;
    if SourcePosition<=SourceLength then begin
     case Source[SourcePosition] of
      '|':begin
       inc(SourcePosition);
      end;
      ')':begin
       break;
      end;
     end;
    end else begin
     break;
    end;
   end;
  except
   FreeNode(result);
   raise;
  end;
 end;
var Counter:TpvInt32;
    Node:PRegularExpressionNode;
begin
 Source:=fRegularExpression;
 SourcePosition:=1;
 SourceLength:=length(Source);
 if SourcePosition<=SourceLength then begin
  fCountParens:=1;
  fAnchoredRootNode:=NewNode(ntPAREN,ParseDisjunction,nil,0);
  if TRegularExpressionFlag.Longest in fFlags then begin
   fUnanchoredRootNode:=NewNode(ntCAT,NewNode(ntSTAR,NewNode(ntDOT,nil,nil,0),nil,qkGREEDY),fAnchoredRootNode,0);
  end else begin
   fUnanchoredRootNode:=NewNode(ntCAT,NewNode(ntSTAR,NewNode(ntDOT,nil,nil,0),nil,qkLAZY),fAnchoredRootNode,0);
  end;
  fBeginningAnchor:=false;
  for Counter:=0 to fNodes.Count-1 do begin
   Node:=fNodes[Counter];
   if Node^.NodeType in [ntBOL,ntEOL,ntBRK,ntNBRK] then begin
    if (Node^.NodeType=ntBOL) and not (TRegularExpressionFlag.MultiLine in fFlags) then begin
     fBeginningAnchor:=true;
    end;
    break;
   end;
  end;
  if TRegularExpressionFlag.CaseInsensitive in fFlags then begin
   for Counter:=0 to fNodes.Count-1 do begin
    Node:=fNodes[Counter];
    if Node^.NodeType=ntCHAR then begin
     Node^.CharClass:=Node^.CharClass.ToCaseInsensitive;
    end;
   end;
  end;
  if SourcePosition<=SourceLength then begin
   raise ERegularExpression.Create('Syntax error');
  end;
 end;
end;

procedure TpvTextEditor.TRegularExpression.Compile;
 procedure GenerateInstructions(var Instructions:TRegularExpressionInstructions;var CountInstructions:TpvInt32);
  function NewInstruction(Opcode:TpvUInt32):TpvInt32;
  begin
   result:=CountInstructions;
   inc(CountInstructions);
   if CountInstructions>length(Instructions) then begin
    SetLength(Instructions,CountInstructions*2);
   end;
   Instructions[result].IndexAndOpcode:=(TpvUInt32(result) shl 8) or (Opcode and $ff);
   Instructions[result].Next:=pointer(TpvPtrInt(-1));
   Instructions[result].OtherNext:=pointer(TpvPtrInt(-1));
  end;
  procedure Emit(Node:PRegularExpressionNode);
  var i0,i1,Counter,Index:TpvInt32;
      Last:array of TpvInt32;
  begin
   while assigned(Node) do begin
    case Node^.NodeType of
     ntALT:begin
      i0:=NewInstruction(opSPLIT);
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
      Emit(Node^.Left);
      i1:=NewInstruction(opJMP);
      Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
      Emit(Node^.Right);
      Instructions[i1].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     ntCAT:begin
      Emit(Node^.Left);
      Node:=Node^.Right;
      continue;
     end;
     ntCHAR:begin
      if (length(Node^.CharClass.fRanges)=1) and
         (Node^.CharClass.fRanges[0].fFromCodePoint=Node^.CharClass.fRanges[0].fToCodePoint) then begin
       i0:=NewInstruction(opSINGLECHAR);
       Instructions[i0].Value:=Node^.CharClass.fRanges[0].fFromCodePoint;
      end else begin
       i0:=NewInstruction(opCHAR);
       Index:=-1;
       for Counter:=0 to fCountCharClasses-1 do begin
        if fCharClasses[Counter]^=Node^.CharClass then begin
         Index:=Counter;
         break;
        end;
       end;
       if Index<0 then begin
        Index:=fCountCharClasses;
        inc(fCountCharClasses);
        if fCountCharClasses>length(fCharClasses) then begin
         SetLength(fCharClasses,fCountCharClasses*2);
        end;
        GetMem(fCharClasses[Index],SizeOf(TRegularExpressionCharClass));
        fCharClasses[Index]^:=Node^.CharClass;
       end;
       Instructions[i0].Value:=TpvPtrUInt(pointer(fCharClasses[Index]));
      end;
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     ntDOT:begin
      i0:=NewInstruction(opANY);
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     ntPAREN:begin
      i0:=NewInstruction(opSAVE);
      Instructions[i0].Value:=Node^.Value shl 1;
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
      Emit(Node^.Left);
      i0:=NewInstruction(opSAVE);
      Instructions[i0].Value:=(Node^.Value shl 1) or 1;
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     ntQUEST:begin
      i0:=NewInstruction(opSPLIT);
      if Node^.Value<>0 then begin
       // Non-greedy
       Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
       Emit(Node^.Left);
       Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
      end else begin
       // Greedy
       Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
       Emit(Node^.Left);
       Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
      end;
     end;
     ntSTAR:begin
      i0:=NewInstruction(opSPLIT);
      if Node^.Value<>0 then begin
       // Non-greedy
       Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
       Emit(Node^.Left);
       i1:=NewInstruction(opJMP);
       Instructions[i1].Next:=pointer(TpvPtrInt(i0));
       Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
      end else begin
       // Greedy
       Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
       Emit(Node^.Left);
       i1:=NewInstruction(opJMP);
       Instructions[i1].Next:=pointer(TpvPtrInt(i0));
       Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
      end;
     end;
     ntPLUS:begin
      i0:=CountInstructions;
      Emit(Node^.Left);
      i1:=NewInstruction(opSPLIT);
      if Node^.Value<>0 then begin
       // Non-greedy
       Instructions[i1].OtherNext:=pointer(TpvPtrInt(i0));
       Instructions[i1].Next:=pointer(TpvPtrInt(CountInstructions));
      end else begin
       // Greedy
       Instructions[i1].Next:=pointer(TpvPtrInt(i0));
       Instructions[i1].OtherNext:=pointer(TpvPtrInt(CountInstructions));
      end;
     end;
     ntEXACT:begin
      if (Node^.MinCount=0) and (Node^.MaxCount=0) then begin
       // nothing
      end else if (Node^.MinCount=0) and (Node^.MaxCount=1) then begin
       i0:=NewInstruction(opSPLIT);
       if Node^.Value<>0 then begin
        // Non-greedy
        Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
        Emit(Node^.Left);
        Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
       end else begin
        // Greedy
        Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
        Emit(Node^.Left);
        Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
       end;
      end else if Node^.MaxCount<0 then begin
       if Node^.MinCount>0 then begin
        // Infinity with minimum count
        for Counter:=1 to Node^.MinCount-1 do begin
         Emit(Node^.Left);
        end;
        i0:=CountInstructions;
        Emit(Node^.Left);
        i1:=NewInstruction(opSPLIT);
        if Node^.Value<>0 then begin
         // Non-greedy
         Instructions[i1].OtherNext:=pointer(TpvPtrInt(i0));
         Instructions[i1].Next:=pointer(TpvPtrInt(CountInstructions));
        end else begin
         // Greedy
         Instructions[i1].Next:=pointer(TpvPtrInt(i0));
         Instructions[i1].OtherNext:=pointer(TpvPtrInt(CountInstructions));
        end;
       end else begin
        // Infinity without minimum count
        i0:=NewInstruction(opSPLIT);
        if Node^.Value<>0 then begin
         // Non-greedy
         Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
         Emit(Node^.Left);
         i1:=NewInstruction(opJMP);
         Instructions[i1].Next:=pointer(TpvPtrInt(i0));
         Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
        end else begin
         // Greedy
         Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
         Emit(Node^.Left);
         i1:=NewInstruction(opJMP);
         Instructions[i1].Next:=pointer(TpvPtrInt(i0));
         Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
        end;
       end;
      end else begin
       for Counter:=1 to Node^.MinCount do begin
        Emit(Node^.Left);
       end;
       if Node^.MinCount<Node^.MaxCount then begin
        if (Node^.MaxCount-Node^.MinCount)<1024 then begin
         SetLength(Last,Node^.MaxCount-Node^.MinCount);
         try
          for Counter:=Node^.MinCount to Node^.MaxCount-1 do begin
           i0:=NewInstruction(opSPLIT);
           Last[Counter-Node^.MinCount]:=i0;
           if Node^.Value<>0 then begin
            // Non-greedy
            Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
           end else begin
            // Greedy
            Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
           end;
           Emit(Node^.Left);
          end;
          for Counter:=Node^.MaxCount-1 downto Node^.MinCount do begin
           i0:=Last[Counter-Node^.MinCount];
           if Node^.Value<>0 then begin
            // Non-greedy
            Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
           end else begin
            // Greedy
            Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
           end;
          end;
         finally
          SetLength(Last,0);
         end;
        end else begin
         for Counter:=Node^.MinCount to Node^.MaxCount-1 do begin
          i0:=NewInstruction(opSPLIT);
          if Node^.Value<>0 then begin
           // Non-greedy
           Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
           Emit(Node^.Left);
           Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
          end else begin
           // Greedy
           Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
           Emit(Node^.Left);
           Instructions[i0].OtherNext:=pointer(TpvPtrInt(CountInstructions));
          end;
         end;
        end;
       end;
      end;
     end;
     ntBOL:begin
      i0:=NewInstruction(opBOL);
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     ntEOL:begin
      i0:=NewInstruction(opEOL);
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     ntBRK:begin
      i0:=NewInstruction(opBRK);
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     ntNBRK:begin
      i0:=NewInstruction(opNBRK);
      Instructions[i0].Next:=pointer(TpvPtrInt(CountInstructions));
     end;
     else begin
      raise ERegularExpression.Create('Internal error');
     end;
    end;
    break;
   end;
  end;
 var Counter:TpvInt32;
     Instruction:PRegularExpressionInstruction;
 begin
  SetLength(Instructions,4096);
  CountInstructions:=0;
  try
   try
    Emit(fUnanchoredRootNode);
   except
    CountInstructions:=0;
    raise;
   end;
   NewInstruction(opMATCH);
  finally
   SetLength(Instructions,CountInstructions);
   for Counter:=0 to CountInstructions-1 do begin
    Instruction:=@Instructions[Counter];
    if Instruction^.Next<>pointer(TpvPtrInt(-1)) then begin
     Instruction^.Next:=@Instructions[TpvPtrInt(TpvPtrUInt(Instruction^.Next))];
    end else begin
     Instruction^.Next:=nil;
    end;
    if Instruction^.OtherNext<>pointer(TpvPtrInt(-1)) then begin
     Instruction^.OtherNext:=@Instructions[TpvPtrInt(TpvPtrUInt(Instruction^.OtherNext))];
    end else begin
     Instruction^.OtherNext:=nil;
    end;
   end;
   fAnchoredStartInstruction:=@Instructions[0];
   fUnanchoredStartInstruction:=@Instructions[0];
   for Counter:=0 to CountInstructions-1 do begin
    Instruction:=@Instructions[Counter];
    if ((Instruction^.IndexAndOpcode and $ff)=opSAVE) and (Instruction^.Value=0) then begin
     fAnchoredStartInstruction:=Instruction;
     break;
    end;
   end;
  end;
 end;
begin
 GenerateInstructions(fInstructions,fCountInstructions);
end;

function TpvTextEditor.TRegularExpression.NewSubMatches(const aCount:TpvInt32;const aBitState:TpvUInt32):PRegularExpressionSubMatches; {$ifdef caninline}inline;{$endif}
begin
 if assigned(fFreeSubMatches) then begin
  result:=fFreeSubMatches;
  fFreeSubMatches:=result^.Next;
 end else begin
  GetMem(result,SizeOf(TRegularExpressionSubMatches));
  FillChar(result^,SizeOf(TRegularExpressionSubMatches),#0);
  SetLength(result^.SubMatches,fCountSubMatches);
  fAllSubMatches.Add(result);
 end;
 result^.ReferenceCounter:=1;
 result^.Count:=aCount;
 result^.BitState:=aBitState;
end;

procedure TpvTextEditor.TRegularExpression.DecRef(const aSubMatches:PRegularExpressionSubMatches); {$ifdef caninline}inline;{$endif}
begin
 dec(aSubMatches^.ReferenceCounter);
 if aSubMatches^.ReferenceCounter=0 then begin
  aSubMatches^.Next:=fFreeSubMatches;
  fFreeSubMatches:=aSubMatches;
 end;
end;

function TpvTextEditor.TRegularExpression.IncRef(const aSubMatches:PRegularExpressionSubMatches):PRegularExpressionSubMatches; {$ifdef caninline}inline;{$endif}
begin
 inc(aSubMatches^.ReferenceCounter);
 result:=aSubMatches;
end;

function TpvTextEditor.TRegularExpression.Update(const aSubMatches:PRegularExpressionSubMatches;const aIndex,aPosition:TpvSizeInt):PRegularExpressionSubMatches; {$ifdef caninline}inline;{$endif}
var Counter:TpvInt32;
    BitState:TpvUInt32;
begin
 result:=aSubMatches;
 if result^.ReferenceCounter>1 then begin
  result:=NewSubMatches(aSubMatches^.Count,aSubMatches^.BitState);
  if (result^.BitState and TpvUInt32($80000000))=0 then begin
   BitState:=result^.BitState;
   while BitState<>0 do begin
    Counter:=PopFirstOneBit(BitState);
    result^.SubMatches[Counter]:=aSubMatches^.SubMatches[Counter];
   end;
  end else begin
   Move(aSubMatches^.SubMatches[0],result^.SubMatches[0],aSubMatches^.Count*SizeOf(TRegularExpressionSubMatchesItem));
  end;
  dec(aSubMatches^.ReferenceCounter);
 end;
{$ifdef cpu386}
 result^.BitState:=result^.BitState or ((TpvUInt32(1) shl aIndex) or TpvUInt32(-TpvUInt32(TpvUInt32(-(aIndex-30)) shr 31)));
{$else}
 if (result^.BitState and TpvUInt32($80000000))=0 then begin
  if Index>30 then begin
   result^.BitState:=$ffffffff;
  end else begin
   result^.BitState:=result^.BitState or (TpvUInt32(1) shl Index);
  end;
 end;
{$endif}
 result^.SubMatches[aIndex]:=aPosition;
end;

function TpvTextEditor.TRegularExpression.NewThread(const aInstruction:PRegularExpressionInstruction;const aSubMatches:PRegularExpressionSubMatches):TRegularExpressionThread; {$ifdef caninline}inline;{$endif}
begin
 result.Instruction:=aInstruction;
 result.SubMatches:=aSubMatches;
end;

function TpvTextEditor.TRegularExpression.IsWordChar(const aPosition:TpvInt32):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=fCodePointWindow[aPosition] in [ord('A')..ord('Z'),ord('a')..ord('z')];
end;

procedure TpvTextEditor.TRegularExpression.AddThread(const aThreadList:PRegularExpressionThreadList;aInstruction:PRegularExpressionInstruction;aSubMatches:PRegularExpressionSubMatches;const aPosition,aWindowOffset:TpvSizeInt);
var Thread:PRegularExpressionThread;
begin
 while assigned(aInstruction) do begin
  if aInstruction^.Generation=fGeneration then begin
   DecRef(aSubMatches);
   break;
  end else begin
   aInstruction^.Generation:=fGeneration;
   case aInstruction^.IndexAndOpcode and $ff of
    opJMP:begin
     aInstruction:=aInstruction^.Next;
     continue;
    end;
    opSPLIT:begin
     AddThread(aThreadList,aInstruction^.Next,IncRef(aSubMatches),aPosition,aWindowOffset);
     aInstruction:=aInstruction^.OtherNext;
     continue;
    end;
    opSAVE:begin
     aSubMatches:=Update(aSubMatches,aInstruction^.Value,aPosition);
     aInstruction:=aInstruction^.Next;
     continue;
    end;
    opBOL:begin
     if (aPosition=0) or
        ((TRegularExpressionFlag.MultiLine in fFlags) and
         ((aPosition>0) and (fCodePointWindow[aWindowOffset-1] in [10,13]))) then begin
      aInstruction:=aInstruction^.Next;
      continue;
     end else begin
      DecRef(aSubMatches);
      break;
     end;
    end;
    opEOL:begin
     if ((aPosition+1)>=fParent.fRope.CountCodePoints) or
        ((TRegularExpressionFlag.MultiLine in fFlags) and
         (((aPosition+1)<fParent.fRope.CountCodePoints) and
          (fCodePointWindow[aWindowOffset+1] in [10,13]))) then begin
      aInstruction:=aInstruction^.Next;
      continue;
     end else begin
      DecRef(aSubMatches);
      break;
     end;
    end;
    opBRK:begin
     if IsWordChar(aWindowOffset-1)<>IsWordChar(aWindowOffset) then begin
      aInstruction:=aInstruction^.Next;
      continue;
     end else begin
      DecRef(aSubMatches);
      break;
     end;
    end;
    opNBRK:begin
     if IsWordChar(aWindowOffset-1)=IsWordChar(aWindowOffset) then begin
      aInstruction:=aInstruction^.Next;
      continue;
     end else begin
      DecRef(aSubMatches);
      break;
     end;
    end;
    else begin
     Thread:=@aThreadList^.Threads[aThreadList^.Count];
     inc(aThreadList^.Count);
     Thread^.Instruction:=aInstruction;
     Thread^.SubMatches:=aSubMatches;
     break;
    end;
   end;
  end;
 end;
end;

function TpvTextEditor.TRegularExpression.SearchMatch(out aCaptures:TRegularExpressionCaptures;const aStartPosition,aUntilExcludingPosition:TpvSizeInt;const aUnanchoredStart:boolean):boolean;
var CurrentPosition,Counter,ThreadIndex,CurrentLength,LastPosition,WindowStartOffset,
    CountCodePoints:TpvSizeInt;
    CurrentThreadList,NewThreadList,TemporaryThreadList:PRegularExpressionThreadList;
    SubMatches,Matched,BestSubMatches:PRegularExpressionSubMatches;
    CurrentThread:PRegularExpressionThread;
    Instruction:PRegularExpressionInstruction;
    BitState:TpvUInt32;
    Capture:PRegularExpressionCapture;
    CodePointEnumeratorSource:TpvTextEditor.TRope.TCodePointEnumeratorSource;
    CodePointEnumerator:TpvTextEditor.TRope.TCodePointEnumerator;
begin
 result:=false;

 CountCodePoints:=fParent.fRope.CountCodePoints;

 CurrentThreadList:=@fThreadLists[0];
 NewThreadList:=@fThreadLists[1];

 CurrentThreadList^.Count:=0;
 NewThreadList^.Count:=0;

 SubMatches:=NewSubMatches(fCountSubMatches,0);

 fCodePointWindow.Reset;

 if aStartPosition>0 then begin
  WindowStartOffset:=-1;
 end else begin
  WindowStartOffset:=0;
 end;
 CodePointEnumeratorSource:=TpvTextEditor.TRope.TCodePointEnumeratorSource.Create(fParent.fRope,aStartPosition+WindowStartOffset,CountCodePoints);
 CodePointEnumerator:=CodePointEnumeratorSource.GetEnumerator;
 for Counter:=-1 to WindowStartOffset-1 do begin
  fCodePointWindow[Counter]:=$ffffffff;
 end;
 for Counter:=WindowStartOffset to 2 do begin
  if CodePointEnumerator.MoveNext then begin
   fCodePointWindow[Counter]:=CodePointEnumerator.GetCurrent;
  end else begin
   fCodePointWindow[Counter]:=$ffffffff;
  end;
 end;

 inc(fGeneration);

 if aUnanchoredStart then begin
  AddThread(CurrentThreadList,fUnanchoredStartInstruction,SubMatches,aStartPosition,0);
 end else begin
  AddThread(CurrentThreadList,fAnchoredStartInstruction,SubMatches,aStartPosition,0);
 end;

 Matched:=nil;

 BestSubMatches:=nil;

 LastPosition:=-1;

 for CurrentPosition:=aStartPosition to aUntilExcludingPosition-1 do begin
  if CurrentThreadList^.Count=0 then begin
   break;
  end;
  if CurrentPosition<>aStartPosition then begin
   fCodePointWindow.Advance;
   if CodePointEnumerator.MoveNext then begin
    fCodePointWindow[2]:=CodePointEnumerator.GetCurrent;
   end else begin
    fCodePointWindow[2]:=$ffffffff;
   end;
  end;
  inc(fGeneration);
  for ThreadIndex:=0 to CurrentThreadList^.Count-1 do begin
   CurrentThread:=@CurrentThreadList^.Threads[ThreadIndex];
   Instruction:=CurrentThread^.Instruction;
   SubMatches:=CurrentThread^.SubMatches;
   case Instruction^.IndexAndOpcode and $ff of
    opSINGLECHAR:begin
     if (CurrentPosition>=CountCodePoints) or
        (fCodePointWindow[0]<>TpvUInt32(Instruction^.Value)) then begin
      DecRef(SubMatches);
     end else begin
      AddThread(NewThreadList,Instruction^.Next,SubMatches,CurrentPosition+1,1);
     end;
    end;
    opCHAR:begin
     if (CurrentPosition>=CountCodePoints) or not
        (fCodePointWindow[0] in PRegularExpressionCharClass(pointer(TpvPtrUInt(Instruction^.Value)))^) then begin
      DecRef(SubMatches);
     end else begin
      AddThread(NewThreadList,Instruction^.Next,SubMatches,CurrentPosition+1,1);
     end;
    end;
    opANY:begin
     if CurrentPosition>=CountCodePoints then begin
      DecRef(SubMatches);
     end else begin
      AddThread(NewThreadList,Instruction^.Next,SubMatches,CurrentPosition+1,1);
     end;
    end;
    opMATCH:begin
     if TRegularExpressionFlag.Longest in fFlags then begin
      if not assigned(BestSubMatches) then begin
       BestSubMatches:=NewSubMatches(fCountSubMatches,SubMatches^.BitState);
      end;
      if SubMatches^.BitState<>0 then begin
       if LastPosition<CurrentPosition then begin
        LastPosition:=CurrentPosition;
        BestSubMatches^.BitState:=SubMatches^.BitState;
        Move(SubMatches^.SubMatches[0],BestSubMatches^.SubMatches[0],SubMatches^.Count*SizeOf(TRegularExpressionSubMatchesItem));
       end;
      end;
     end else begin
      if assigned(Matched) then begin
       DecRef(Matched);
      end;
      Matched:=SubMatches;
      for Counter:=ThreadIndex+1 to CurrentThreadList^.Count-1 do begin
       DecRef(CurrentThreadList^.Threads[Counter].SubMatches);
      end;
      break;
     end;
    end;
   end;
  end;
  TemporaryThreadList:=CurrentThreadList;
  CurrentThreadList:=NewThreadList;
  NewThreadList:=TemporaryThreadList;
  NewThreadList^.Count:=0;
 end;

 if CurrentThreadList^.Count<>0 then begin
  inc(fGeneration);
  for ThreadIndex:=0 to CurrentThreadList^.Count-1 do begin
   CurrentThread:=@CurrentThreadList^.Threads[ThreadIndex];
   Instruction:=CurrentThread^.Instruction;
   SubMatches:=CurrentThread^.SubMatches;
   case Instruction^.IndexAndOpcode and $ff of
    opSINGLECHAR,opCHAR,opANY:begin
     DecRef(SubMatches);
    end;
    opMATCH:begin
     if TRegularExpressionFlag.Longest in fFlags then begin
      if not assigned(BestSubMatches) then begin
       BestSubMatches:=NewSubMatches(fCountSubMatches,SubMatches^.BitState);
      end;
      if SubMatches^.BitState<>0 then begin
       if LastPosition<aUntilExcludingPosition then begin
        LastPosition:=aUntilExcludingPosition;
        BestSubMatches^.BitState:=SubMatches^.BitState;
        Move(SubMatches^.SubMatches[0],BestSubMatches^.SubMatches[0],SubMatches^.Count*SizeOf(TRegularExpressionSubMatchesItem));
       end;
      end;
     end else begin
      if assigned(Matched) then begin
       DecRef(Matched);
      end;
      Matched:=SubMatches;
      for Counter:=ThreadIndex+1 to CurrentThreadList^.Count-1 do begin
       DecRef(CurrentThreadList^.Threads[Counter].SubMatches);
      end;
      break;
     end;
    end;
   end;
  end;
 end;

 if assigned(BestSubMatches) then begin
  if assigned(Matched) then begin
   DecRef(Matched);
  end;
  Matched:=BestSubMatches;
 end;

 if assigned(Matched) then begin
  SetLength(aCaptures,fCountParens);
  BitState:=Matched^.BitState;
  for Counter:=0 to fCountParens-1 do begin
   Capture:=@aCaptures[Counter];
   if (BitState and TpvUInt32($80000000))<>0 then begin
    CurrentPosition:=Matched^.SubMatches[Counter shl 1];
    CurrentLength:=Matched^.SubMatches[(Counter shl 1) or 1]-CurrentPosition;
   end else begin
    if (BitState and (TpvUInt32(1) shl (Counter shl 1)))<>0 then begin
     CurrentPosition:=Matched^.SubMatches[Counter shl 1];
    end else begin
     CurrentPosition:=0;
    end;
    if (BitState and (TpvUInt32(1) shl ((Counter shl 1) or 1)))<>0 then begin
     CurrentLength:=Matched^.SubMatches[(Counter shl 1) or 1]-CurrentPosition;
    end else begin
     CurrentLength:=0;
    end;
   end;
   if CurrentLength<1 then begin
    Capture^.Start:=0;
    Capture^.Length:=0;
   end else begin
    Capture^.Start:=CurrentPosition;
    Capture^.Length:=CurrentLength;
   end;
  end;
  DecRef(Matched);
  result:=true;
 end;

 if fGeneration>MaxGeneration then begin
  fGeneration:=0;
  for Counter:=0 to fCountInstructions-1 do begin
   fInstructions[Counter].Generation:=$ffffffff;
  end;
 end;

end;

function TpvTextEditor.TRegularExpression.MatchNext(out aCaptures:TRegularExpressionCaptures;out aPosition,aLength:TpvSizeInt;const aStartPosition:TpvSizeInt=0;const aUntilExcludingPosition:TpvSizeInt=-1;const aCountTries:TpvSizeInt=-1):boolean;
var Position,UntilExcludingPosition,CountTries:TpvSizeInt;
begin
 if aUntilExcludingPosition>=0 then begin
  UntilExcludingPosition:=aUntilExcludingPosition;
 end else begin
  UntilExcludingPosition:=fParent.fRope.fCountCodePoints;
 end;
 CountTries:=aCountTries;
 for Position:=aStartPosition to UntilExcludingPosition-1 do begin
  if CountTries>0 then begin
   dec(CountTries);
   if CountTries=0 then begin
    break;
   end;
  end;
  if SearchMatch(aCaptures,Position,UntilExcludingPosition,fDoUnanchoredStart) then begin
   aPosition:=aCaptures[0].Start;
   aLength:=aCaptures[0].Length;
   result:=true;
   exit;
  end else if fDoUnanchoredStart or fBeginningWildcardLoop or fBeginningAnchor then begin
   break;
  end;
 end;
 result:=false;
end;

function TpvTextEditor.TRegularExpression.FindNext(out aPosition,aLength:TpvSizeInt;const aStartPosition:TpvSizeInt=0;const aUntilExcludingPosition:TpvSizeInt=-1;const aCountTries:TpvSizeInt=-1):boolean;
var Captures:TRegularExpressionCaptures;
begin
 result:=MatchNext(Captures,aPosition,aLength,aStartPosition,aUntilExcludingPosition,aCountTries);
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
 fAutoIdentOnEnterMode:=TAutoIdentOnEnterMode.Copy;
 fVisualLineCacheMap:=TLineCacheMap.Create(fParent.fRope);
 fBuffer:=nil;
 fMarkState.StartCodePointIndex:=-1;
 fMarkState.EndCodePointIndex:=-1;
end;

destructor TpvTextEditor.TView.Destroy;
begin
 FreeAndNil(fVisualLineCacheMap);
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
  fVisualLineCacheMap.LineWrap:=aLineWrap;
  fVisualLineCacheMap.Update(-1,-1);
  if aLineWrap>0 then begin
   fCursorOffset.x:=0;
  end;
  EnsureCodePointIndexIsInRange;
  EnsureCursorIsVisible(true);
 end;
end;

procedure TpvTextEditor.TView.SetLineColumn(const aLineColumn:TLineColumn);
begin
 fCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromLineIndexAndColumnIndex(aLineColumn.Line,aLineColumn.Column);
 EnsureCodePointIndexIsInRange;
 EnsureCursorIsVisible(true);
end;

procedure TpvTextEditor.TView.SetCodePointIndex(const aCodePointIndex:TpvSizeInt);
begin
 fCodePointIndex:=aCodePointIndex;
 EnsureCodePointIndexIsInRange;
 EnsureCursorIsVisible(true);
end;

function TpvTextEditor.TView.GetMarkStartCodePointIndex:TpvSizeInt;
begin
 result:=fMarkState.StartCodePointIndex;
end;

procedure TpvTextEditor.TView.SetMarkStartCodePointIndex(const aMarkStartCodePointIndex:TpvSizeInt);
begin
 if fMarkState.StartCodePointIndex<>aMarkStartCodePointIndex then begin
  fMarkState.StartCodePointIndex:=Min(Max(aMarkStartCodePointIndex,-1),fParent.fRope.fCountCodePoints-1);
 end;
end;

function TpvTextEditor.TView.GetMarkEndCodePointIndex:TpvSizeInt;
begin
 result:=fMarkState.EndCodePointIndex;
end;

procedure TpvTextEditor.TView.SetMarkEndCodePointIndex(const aMarkEndCodePointIndex:TpvSizeInt);
begin
 if fMarkState.EndCodePointIndex<>aMarkEndCodePointIndex then begin
  fMarkState.EndCodePointIndex:=Min(Max(aMarkEndCodePointIndex,-1),fParent.fRope.fCountCodePoints-1);
 end;
end;

procedure TpvTextEditor.TView.ClampMarkCodePointIndices;
begin
 if (fMarkState.StartCodePointIndex>=0) and (fMarkState.EndCodePointIndex>=0) then begin
  fMarkState.StartCodePointIndex:=Min(Max(fMarkState.StartCodePointIndex,-1),fParent.fRope.fCountCodePoints-1);
  fMarkState.EndCodePointIndex:=Min(Max(fMarkState.EndCodePointIndex,-1),fParent.fRope.fCountCodePoints-1);
 end else begin
  fMarkState.StartCodePointIndex:=-1;
  fMarkState.EndCodePointIndex:=-1;
 end;
end;

procedure TpvTextEditor.TView.EnsureCodePointIndexIsInRange;
begin
 fCodePointIndex:=Min(Max(fCodePointIndex,0),fParent.fRope.CountCodePoints);
end;

procedure TpvTextEditor.TView.EnsureCursorIsVisible(const aUpdateCursor:boolean=true;const aForceVisibleLines:TpvSizeInt=1);
var CurrentLineIndex,CurrentColumnIndex:TpvSizeInt;
begin

 if fVisualLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin

  if CurrentLineIndex<fCursorOffset.y then begin
   fCursorOffset.y:=CurrentLineIndex;
  end else if (fCursorOffset.y+NonScrollVisibleAreaHeight)<(CurrentLineIndex+aForceVisibleLines) then begin
   fCursorOffset.y:=(CurrentLineIndex+aForceVisibleLines)-NonScrollVisibleAreaHeight;
  end;

  fVisualLineCacheMap.Update(-1,fVisualLineCacheMap.fCountLines+(fNonScrollVisibleAreaHeight*2));

  if fCursorOffset.y>=(fVisualLineCacheMap.fCountLines-NonScrollVisibleAreaHeight) then begin
   fCursorOffset.y:=fVisualLineCacheMap.fCountLines-fNonScrollVisibleAreaHeight;
  end;

  if fCursorOffset.y<0 then begin
   fCursorOffset.y:=0;
  end;

  if CurrentColumnIndex<fCursorOffset.x then begin
   fCursorOffset.x:=CurrentColumnIndex;
  end else if (fCursorOffset.x+NonScrollVisibleAreaWidth)<=CurrentColumnIndex then begin
   fCursorOffset.x:=(CurrentColumnIndex-NonScrollVisibleAreaWidth)+1;
  end;

  if fCursorOffset.x<0 then begin
   fCursorOffset.x:=0;
  end;

  if aUpdateCursor then begin
   fCursor.x:=CurrentColumnIndex-fCursorOffset.x;
   fCursor.y:=CurrentLineIndex-fCursorOffset.y;
  end;

 end;

 if aUpdateCursor and fParent.fLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin
  fLineColumn.Line:=CurrentLineIndex;
  fLineColumn.Column:=CurrentColumnIndex;
 end;

end;

procedure TpvTextEditor.TView.UpdateCursor;
var CurrentLineIndex,CurrentColumnIndex:TpvSizeInt;
begin
 if fVisualLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin
  fCursor.x:=CurrentColumnIndex-fCursorOffset.x;
  fCursor.y:=CurrentLineIndex-fCursorOffset.y;
 end;
 if fParent.fLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,CurrentLineIndex,CurrentColumnIndex) then begin
  fLineColumn.Line:=CurrentLineIndex;
  fLineColumn.Column:=CurrentColumnIndex;
 end;
end;

procedure TpvTextEditor.TView.UpdateBuffer;
const EmptyBufferItem:TBufferItem=
       (
        Attribute:0;
        CodePoint:32;
       );
var BufferSize,BufferBaseIndex,BufferBaseEndIndex,BufferIndex,
    CurrentLineIndex,StartCodePointIndex,StopCodePointIndex,
    CurrentCodePointIndex,StepWidth,StateIndex,
    LevelStateIndex:TpvSizeInt;
    CodePoint,IncomingCodePoint,CurrentAttribute,
    Level,CurrentLevel,TargetLevel:TpvUInt32;
    RelativeCursor:TCoordinate;
    CodePointEnumerator:TRope.TCodePointEnumerator;
    State,StartLevelState,EndLevelState:TpvTextEditor.TSyntaxHighlighting.TState;
    BufferItem:PBufferItem;
    CurrentHighlight:boolean;
begin

 ClampMarkCodePointIndices;

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

  StateIndex:=0;

  State:=nil;

  CurrentAttribute:=0;

  for CurrentLineIndex:=fCursorOffset.y to fCursorOffset.y+(VisibleAreaHeight-1) do begin

   StartCodePointIndex:=fVisualLineCacheMap.GetCodePointIndexFromLineIndex(CurrentLineIndex);
   if (StartCodePointIndex<0) or
      (StartCodePointIndex>=fParent.fRope.fCountCodePoints) then begin
    break;
   end;

   StopCodePointIndex:=fVisualLineCacheMap.GetCodePointIndexFromNextLineIndexOrTextEnd(CurrentLineIndex);

   BufferBaseEndIndex:=BufferBaseIndex+VisibleAreaWidth;

   if BufferBaseEndIndex>BufferSize then begin
    BufferBaseEndIndex:=BufferSize;
   end;

   BufferIndex:=BufferBaseIndex;

   RelativeCursor.x:=-fCursorOffset.x;

   if CurrentCodePointIndex<>StartCodePointIndex then begin

    CurrentCodePointIndex:=StartCodePointIndex;

    CodePointEnumerator:=TRope.TCodePointEnumerator.Create(fParent.fRope,StartCodePointIndex,-1);

   end;

   StartLevelState:=nil;

   EndLevelState:=nil;

   if assigned(fParent.fSyntaxHighlighting) then begin

    fParent.fSyntaxHighlighting.Update(StopCodePointIndex);

    if not (((StateIndex+1)<fParent.fSyntaxHighlighting.fCountStates) and
            (fParent.fSyntaxHighlighting.fStates[StateIndex].fCodePointIndex<=StartCodePointIndex) and
            (StartCodePointIndex<fParent.fSyntaxHighlighting.fStates[StateIndex+1].fCodePointIndex)) then begin
     StateIndex:=fParent.fSyntaxHighlighting.GetStateIndexFromCodePointIndex(StartCodePointIndex);
    end;

    if StateIndex<fParent.fSyntaxHighlighting.fCountStates then begin
     State:=fParent.fSyntaxHighlighting.fStates[StateIndex];
     CurrentAttribute:=State.fAttribute;
    end;

{   LevelStateIndex:=StateIndex;
    while ((LevelStateIndex+1)<fParent.fSyntaxHighlighting.fCountStates) and
          (fParent.fSyntaxHighlighting.fStates[LevelStateIndex+1].fCodePointIndex<=fCodePointIndex) do begin
     inc(LevelStateIndex);
     Level:=fParent.fSyntaxHighlighting.fStates[LevelStateIndex].fLevel;
     LevelID:=fParent.fSyntaxHighlighting.fStates[LevelStateIndex].fLevelID;
    end;}
    LevelStateIndex:=fParent.fSyntaxHighlighting.GetStateIndexFromCodePointIndex(fCodePointIndex);

    if LevelStateIndex>=0 then begin

     StartLevelState:=fParent.fSyntaxHighlighting.fStates[LevelStateIndex];

     EndLevelState:=StartLevelState;

     case StartLevelState.fLevel and $c0000000 of
      $40000000:begin
       inc(LevelStateIndex);
       CurrentLevel:=StartLevelState.fLevel and $3fffffff;
       TargetLevel:=CurrentLevel or $80000000;
       while LevelStateIndex<fParent.fSyntaxHighlighting.fCountStates do begin
        Level:=fParent.fSyntaxHighlighting.fStates[LevelStateIndex].fLevel;
        if Level=TargetLevel then begin
         EndLevelState:=fParent.fSyntaxHighlighting.fStates[LevelStateIndex];
         break;
        end else if (Level and $3fffffff)<CurrentLevel then begin
         break;
        end else begin
         inc(LevelStateIndex);
        end;
       end;
      end;
      $80000000:begin
       dec(LevelStateIndex);
       CurrentLevel:=StartLevelState.fLevel and $3fffffff;
       TargetLevel:=CurrentLevel or $40000000;
       while LevelStateIndex>0 do begin
        Level:=fParent.fSyntaxHighlighting.fStates[LevelStateIndex].fLevel;
        if Level=TargetLevel then begin
         EndLevelState:=fParent.fSyntaxHighlighting.fStates[LevelStateIndex];
         break;
        end else if (Level and $3fffffff)<CurrentLevel then begin
         break;
        end else begin
         dec(LevelStateIndex);
        end;
       end;
      end;
     end;

    end;

   end;

   CurrentHighlight:=false;

   while (CurrentCodePointIndex<StopCodePointIndex) and
         CodePointEnumerator.MoveNext do begin

    if assigned(fParent.fSyntaxHighlighting) then begin

     while ((StateIndex+1)<fParent.fSyntaxHighlighting.fCountStates) and
           (fParent.fSyntaxHighlighting.fStates[StateIndex+1].fCodePointIndex<=CurrentCodePointIndex) do begin
      inc(StateIndex);
      State:=fParent.fSyntaxHighlighting.fStates[StateIndex];
     end;

     if assigned(State) then begin

      CurrentAttribute:=State.fAttribute;

      CurrentHighlight:=((State.fLevel and $c0000000)<>0) and
                        ((State=StartLevelState) or
                         (State=EndLevelState));

     end;

    end;

    IncomingCodePoint:=CodePointEnumerator.GetCurrent;

    case IncomingCodePoint of
     $09:begin
      CodePoint:=32;
      StepWidth:=Max(1,(fVisualLineCacheMap.fTabWidth-(RelativeCursor.x mod fVisualLineCacheMap.fTabWidth)));
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

    while StepWidth>0 do begin

     if RelativeCursor.x>=0 then begin

      BufferIndex:=BufferBaseIndex+RelativeCursor.x;

      if (BufferIndex>=BufferBaseIndex) and
         (BufferIndex<BufferBaseEndIndex) then begin
       BufferItem:=@fBuffer[BufferIndex];
       if CurrentHighlight then begin
        BufferItem^.Attribute:=CurrentAttribute or TpvTextEditor.TSyntaxHighlighting.TAttributes.Highlight;
       end else begin
        BufferItem^.Attribute:=CurrentAttribute;
       end;
       BufferItem^.CodePoint:=CodePoint;
      end;

     end;

     CodePoint:=32;

     inc(RelativeCursor.x);
     dec(StepWidth);

    end;

    inc(CurrentCodePointIndex);

   end;

   inc(BufferBaseIndex,VisibleAreaWidth);

   inc(RelativeCursor.y);

  end;

 end;

end;

procedure TpvTextEditor.TView.MarkAll;
begin
 if fParent.fRope.fCountCodePoints>0 then begin
  fMarkState.StartCodePointIndex:=0;
  fMarkState.EndCodePointIndex:=fParent.fRope.fCountCodePoints-1;
 end else begin
  fMarkState.StartCodePointIndex:=-1;
  fMarkState.EndCodePointIndex:=-1;
 end;
end;

procedure TpvTextEditor.TView.UnmarkAll;
begin
 fMarkState.StartCodePointIndex:=-1;
 fMarkState.EndCodePointIndex:=-1;
end;

procedure TpvTextEditor.TView.SetMarkStart;
begin
 fMarkState.StartCodePointIndex:=fCodePointIndex;
 fMarkState.EndCodePointIndex:=fCodePointIndex;
end;

procedure TpvTextEditor.TView.SetMarkEndToHere;
begin
 fMarkState.EndCodePointIndex:=fCodePointIndex;
end;

procedure TpvTextEditor.TView.SetMarkEndUntilHere;
begin
 fMarkState.EndCodePointIndex:=fCodePointIndex-1;
end;

function TpvTextEditor.TView.HasMarkedRange:boolean;
begin
 result:=((fMarkState.StartCodePointIndex>=0) and
          (fMarkState.StartCodePointIndex<fParent.fRope.fCountCodePoints)) and
         ((fMarkState.EndCodePointIndex>=0) and
          (fMarkState.EndCodePointIndex<fParent.fRope.fCountCodePoints));
end;

function TpvTextEditor.TView.GetMarkedRangeText:TpvUTF8String;
var StartCodePointIndex,EndCodePointIndex:TpvSizeInt;
begin
 if HasMarkedRange then begin
  StartCodePointIndex:=Min(fMarkState.StartCodePointIndex,fMarkState.EndCodePointIndex);
  EndCodePointIndex:=Max(fMarkState.StartCodePointIndex,fMarkState.EndCodePointIndex);
  result:=fParent.fRope.Extract(StartCodePointIndex,(EndCodePointIndex-StartCodePointIndex)+1);
 end else begin
  result:='';
 end;
end;

function TpvTextEditor.TView.DeleteMarkedRange:boolean;
var StartCodePointIndex,EndCodePointIndex,Count:TpvSizeInt;
begin
 result:=HasMarkedRange;
 if result then begin
  StartCodePointIndex:=Min(fMarkState.StartCodePointIndex,fMarkState.EndCodePointIndex);
  EndCodePointIndex:=Max(fMarkState.StartCodePointIndex,fMarkState.EndCodePointIndex);
  fCodePointIndex:=StartCodePointIndex;
  Count:=(EndCodePointIndex-StartCodePointIndex)+1;
  fParent.fUndoRedoManager.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex,TpvTextEditor.EmptyMarkState,fMarkState,fCodePointIndex,Count,fParent.fRope.Extract(fCodePointIndex,Count)));
  fParent.fRope.Delete(fCodePointIndex,Count);
  if fCodePointIndex>0 then begin
   fParent.LineMapTruncate(fCodePointIndex-1,-1);
   if assigned(fParent.fSyntaxHighlighting) then begin
    fParent.fSyntaxHighlighting.Truncate(fCodePointIndex-1);
   end;
  end else begin
   fParent.LineMapTruncate(fCodePointIndex,-1);
   if assigned(fParent.fSyntaxHighlighting) then begin
    fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
   end;
  end;
  fParent.EnsureViewCursorsAreVisible(true);
  fParent.ResetViewMarkCodePointIndices;
 end;
end;

function TpvTextEditor.TView.CutMarkedRangeText:TpvUTF8String;
var StartCodePointIndex,EndCodePointIndex,Count:TpvSizeInt;
begin
 if HasMarkedRange then begin
  StartCodePointIndex:=Min(fMarkState.StartCodePointIndex,fMarkState.EndCodePointIndex);
  EndCodePointIndex:=Max(fMarkState.StartCodePointIndex,fMarkState.EndCodePointIndex);
  fCodePointIndex:=StartCodePointIndex;
  Count:=(EndCodePointIndex-StartCodePointIndex)+1;
  fParent.fUndoRedoManager.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex,TpvTextEditor.EmptyMarkState,fMarkState,fCodePointIndex,Count,fParent.fRope.Extract(fCodePointIndex,Count)));
  result:=fParent.fRope.Extract(fCodePointIndex,Count);
  fParent.fRope.Delete(fCodePointIndex,Count);
  if fCodePointIndex>0 then begin
   fParent.LineMapTruncate(fCodePointIndex-1,-1);
   if assigned(fParent.fSyntaxHighlighting) then begin
    fParent.fSyntaxHighlighting.Truncate(fCodePointIndex-1);
   end;
  end else begin
   fParent.LineMapTruncate(fCodePointIndex,-1);
   if assigned(fParent.fSyntaxHighlighting) then begin
    fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
   end;
  end;
  fParent.EnsureViewCursorsAreVisible(true);
  fParent.ResetViewMarkCodePointIndices;
 end else begin
  result:='';
 end;
end;

procedure TpvTextEditor.TView.InsertCodePoint(const aCodePoint:TpvUInt32;const aOverwrite:boolean;const aStealIt:boolean=false);
var Count,UndoRedoHistoryIndex:TpvSizeInt;
    CodeUnits:TpvUTF8String;
    HasDeletedMarkedRange:boolean;
    UndoRedoCommand:TpvTextEditor.TUndoRedoCommand;
begin
 UndoRedoHistoryIndex:=fParent.fUndoRedoManager.fHistoryIndex;
 HasDeletedMarkedRange:=DeleteMarkedRange;
 CodeUnits:=TUTF8Utils.UTF32CharToUTF8(aCodePoint);
 fParent.LineMapTruncate(fCodePointIndex,-1);
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 if aOverwrite and (fCodePointIndex<fParent.fRope.fCountCodePoints) then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  UndoRedoCommand:=TUndoRedoCommandOverwrite.Create(fParent,fCodePointIndex,fCodePointIndex+Count,TpvTextEditor.EmptyMarkState,fMarkState,fCodePointIndex,Count,CodeUnits,fParent.fRope.Extract(fCodePointIndex,Count));
  UndoRedoCommand.fSealed:=aStealIt;
  fParent.fUndoRedoManager.Add(UndoRedoCommand);
  fParent.fRope.Delete(fCodePointIndex,Count);
  fParent.fRope.Insert(fCodePointIndex,CodeUnits);
 end else begin
  UndoRedoCommand:=TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex+1,TpvTextEditor.EmptyMarkState,fMarkState,fCodePointIndex,1,CodeUnits);
  UndoRedoCommand.fSealed:=aStealIt;
  fParent.fUndoRedoManager.Add(UndoRedoCommand);
  fParent.fRope.Insert(fCodePointIndex,CodeUnits);
 end;
 if HasDeletedMarkedRange then begin
  fParent.fUndoRedoManager.GroupUndoRedoCommands(UndoRedoHistoryIndex,fParent.fUndoRedoManager.fHistoryIndex);
 end;
 fParent.UpdateViewCodePointIndices(fCodePointIndex,1);
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
 fParent.ResetViewMarkCodePointIndices;
end;

procedure TpvTextEditor.TView.InsertString(const aCodeUnits:TpvUTF8String;const aOverwrite:boolean;const aStealIt:boolean=false);
var CountCodePoints,Count,UndoRedoHistoryIndex:TpvSizeInt;
    HasDeletedMarkedRange:boolean;
    UndoRedoCommand:TpvTextEditor.TUndoRedoCommand;
begin
 UndoRedoHistoryIndex:=fParent.fUndoRedoManager.fHistoryIndex;
 HasDeletedMarkedRange:=DeleteMarkedRange;
 CountCodePoints:=TRope.GetCountCodePoints(@aCodeUnits[1],length(aCodeUnits));
 fParent.LineMapTruncate(fCodePointIndex,-1);
 if assigned(fParent.fSyntaxHighlighting) then begin
  fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
 end;
 if aOverwrite and (fCodePointIndex<fParent.fRope.fCountCodePoints) then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  UndoRedoCommand:=TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex,TpvTextEditor.EmptyMarkState,fMarkState,CountCodePoints,(CountCodePoints+Count)-1,fParent.fRope.Extract(fCodePointIndex,(CountCodePoints+Count)-1));
  UndoRedoCommand.fSealed:=aStealIt;
  fParent.fUndoRedoManager.Add(UndoRedoCommand);
  fParent.fRope.Delete(fCodePointIndex,(CountCodePoints+Count)-1);
  UndoRedoCommand:=TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex+(CountCodePoints+Count)-1,TpvTextEditor.EmptyMarkState,fMarkState,CountCodePoints,(CountCodePoints+Count)-1,aCodeUnits);
  UndoRedoCommand.fSealed:=aStealIt;
  fParent.fUndoRedoManager.Add(UndoRedoCommand);
  fParent.fRope.Insert(fCodePointIndex,aCodeUnits);
 end else begin
  UndoRedoCommand:=TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex+CountCodePoints,TpvTextEditor.EmptyMarkState,fMarkState,fCodePointIndex,CountCodePoints,aCodeUnits);
  UndoRedoCommand.fSealed:=aStealIt;
  fParent.fUndoRedoManager.Add(UndoRedoCommand);
  fParent.fRope.Insert(fCodePointIndex,aCodeUnits);
 end;
 if HasDeletedMarkedRange then begin
  fParent.fUndoRedoManager.GroupUndoRedoCommands(UndoRedoHistoryIndex,fParent.fUndoRedoManager.fHistoryIndex);
 end;
 fParent.UpdateViewCodePointIndices(fCodePointIndex,CountCodePoints);
 fParent.EnsureViewCodePointIndicesAreInRange;
 fParent.EnsureViewCursorsAreVisible(true);
 fParent.ResetViewMarkCodePointIndices;
end;

procedure TpvTextEditor.TView.Backspace;
var Count:TpvSizeInt;
begin
 if not DeleteMarkedRange then begin
  if (fCodePointIndex>0) and (fCodePointIndex<=fParent.fRope.fCountCodePoints) then begin
   if fparent.IsTwoCodePointNewLine(fCodePointIndex-2) then begin
    Count:=2;
   end else begin
    Count:=1;
   end;
   fParent.fUndoRedoManager.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex-Count,TpvTextEditor.EmptyMarkState,fMarkState,fCodePointIndex-Count,Count,fParent.fRope.Extract(fCodePointIndex-Count,Count)));
   fParent.UpdateViewCodePointIndices(fCodePointIndex,-Count);
   fParent.fRope.Delete(fCodePointIndex,Count);
   if fCodePointIndex>0 then begin
    fParent.LineMapTruncate(fCodePointIndex-1,-1);
    if assigned(fParent.fSyntaxHighlighting) then begin
     fParent.fSyntaxHighlighting.Truncate(fCodePointIndex-1);
    end;
   end else begin
    fParent.LineMapTruncate(fCodePointIndex,-1);
    if assigned(fParent.fSyntaxHighlighting) then begin
     fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
    end;
   end;
  end;
  fParent.EnsureViewCodePointIndicesAreInRange;
  fParent.EnsureViewCursorsAreVisible(true);
  fParent.ResetViewMarkCodePointIndices;
 end;
end;

procedure TpvTextEditor.TView.Paste(const aText:TpvUTF8String);
begin
 InsertString(aText,false,true);
end;

procedure TpvTextEditor.TView.Delete;
var Count:TpvSizeInt;
begin
 if not DeleteMarkedRange then begin
  if fCodePointIndex<fParent.fRope.fCountCodePoints then begin
   if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
    Count:=2;
   end else begin
    Count:=1;
   end;
   fParent.fUndoRedoManager.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex,TpvTextEditor.EmptyMarkState,fMarkState,fCodePointIndex,Count,fParent.fRope.Extract(fCodePointIndex,Count)));
   fParent.fRope.Delete(fCodePointIndex,Count);
   if fCodePointIndex>0 then begin
    fParent.LineMapTruncate(fCodePointIndex-1,-1);
    if assigned(fParent.fSyntaxHighlighting) then begin
     fParent.fSyntaxHighlighting.Truncate(fCodePointIndex-1);
    end;
   end else begin
    fParent.LineMapTruncate(fCodePointIndex,-1);
    if assigned(fParent.fSyntaxHighlighting) then begin
     fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
    end;
   end;
  end;
  fParent.EnsureViewCursorsAreVisible(true);
  fParent.ResetViewMarkCodePointIndices;
 end;
end;

procedure TpvTextEditor.TView.Enter(const aOverwrite:boolean);
var LineIndex,StartCodePointIndex,StopCodePointIndex,WhiteSpaceSteps:TpvSizeInt;
    CodePoint:TpvUInt32;
    PrependedWhiteSpace:TpvUTF8String;
begin
 if aOverwrite then begin
  MoveDown;
  MoveToLineBegin;
 end else begin
  PrependedWhiteSpace:='';
  if fAutoIdentOnEnterMode<>TAutoIdentOnEnterMode.None then begin
   LineIndex:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
   if LineIndex>=0 then begin
    StartCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromLineIndex(LineIndex);
    StopCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromNextLineIndexOrTextEnd(LineIndex);
    case fAutoIdentOnEnterMode of
     TAutoIdentOnEnterMode.Copy:begin
      for CodePoint in fParent.fRope.GetCodePointEnumeratorSource(StartCodePointIndex,StopCodePointIndex) do begin
       case CodePoint of
        9,32:begin
         PrependedWhiteSpace:=PrependedWhiteSpace+TpvTextEditor.TUTF8Utils.UTF32CharToUTF8(CodePoint);
        end;
        else begin
         break;
        end;
       end;
      end;
     end;
     TAutoIdentOnEnterMode.Tabs,
     TAutoIdentOnEnterMode.Spaces:begin
      WhiteSpaceSteps:=0;
      for CodePoint in fParent.fRope.GetCodePointEnumeratorSource(StartCodePointIndex,StopCodePointIndex) do begin
       case CodePoint of
        9:begin
         if fVisualLineCacheMap.fTabWidth>0 then begin
          inc(WhiteSpaceSteps,fVisualLineCacheMap.fTabWidth-(WhiteSpaceSteps mod fVisualLineCacheMap.fTabWidth));
         end;
        end;
        32:begin
         inc(WhiteSpaceSteps);
        end;
        else begin
         break;
        end;
       end;
      end;
      case fAutoIdentOnEnterMode of
       TAutoIdentOnEnterMode.Tabs:begin
        if fVisualLineCacheMap.fTabWidth>0 then begin
         PrependedWhiteSpace:=TpvUTF8String(StringOfChar(#9,WhiteSpaceSteps div fVisualLineCacheMap.fTabWidth))+
                              TpvUTF8String(StringOfChar(#32,WhiteSpaceSteps mod fVisualLineCacheMap.fTabWidth));
        end else begin
         PrependedWhiteSpace:=TpvUTF8String(StringOfChar(#32,WhiteSpaceSteps));
        end;
       end;
       else {TAutoIdentOnEnterMode.Spaces:}begin
        PrependedWhiteSpace:=TpvUTF8String(StringOfChar(#32,WhiteSpaceSteps));
       end;
      end;
     end;
    end;
   end;
  end;
  if length(PrependedWhiteSpace)>0 then begin
   InsertString({$ifdef Windows}TpvUTF8String(#13#10){$else}TpvUTF8String(#10){$endif}+TpvUTF8String(PrependedWhiteSpace),aOverwrite,false);
  end else begin
{$ifdef Windows}
   InsertString(TpvUTF8String(#13#10),aOverwrite,false);
{$else}
   InsertCodePoint(10,aOverwrite,false);
{$endif}
  end;
 end;
 fParent.UpdateViewCursors;
end;

procedure TpvTextEditor.TView.MoveUp;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fRope.CountCodePoints then begin
  fVisualLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fVisualLineCacheMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex-1,ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
 fParent.fUndoRedoManager.IncreaseActionID;
end;

procedure TpvTextEditor.TView.MoveDown;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fRope.CountCodePoints then begin
  fVisualLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fVisualLineCacheMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex+1,ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
 fParent.fUndoRedoManager.IncreaseActionID;
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
 fParent.fUndoRedoManager.IncreaseActionID;
end;

procedure TpvTextEditor.TView.MoveRight;
var Count:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fRope.CountCodePoints then begin
  if fParent.IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  inc(fCodePointIndex,Count);
 end;
 fParent.fUndoRedoManager.IncreaseActionID;
end;

procedure TpvTextEditor.TView.MoveToLineBegin;
var LineIndex:TpvSizeInt;
begin
 if fCodePointIndex<fParent.fRope.CountCodePoints then begin
  LineIndex:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
  fCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromLineIndex(LineIndex);
 end else if (fCodePointIndex>0) and (fCodePointIndex>=fParent.fRope.CountCodePoints) then begin
  LineIndex:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(fParent.fRope.CountCodePoints);
  fCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromLineIndex(LineIndex);
 end;
 fParent.fUndoRedoManager.IncreaseActionID;
end;

procedure TpvTextEditor.TView.MoveToLineEnd;
var LineIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fRope.CountCodePoints then begin
  LineIndex:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex,High(TpvSizeInt));
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
 fParent.fUndoRedoManager.IncreaseActionID;
end;

procedure TpvTextEditor.TView.MovePageUp;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fRope.CountCodePoints then begin
  fVisualLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fVisualLineCacheMap.GetCodePointIndexFromLineIndexAndColumnIndex(Max(0,LineIndex-fNonScrollVisibleAreaHeight),ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
   if fCursorOffset.y<fNonScrollVisibleAreaHeight then begin
    fCursorOffset.y:=0;
   end else begin
    dec(fCursorOffset.y,fNonScrollVisibleAreaHeight);
   end;
  end;
  EnsureCodePointIndexIsInRange;
  EnsureCursorIsVisible(true);
 end;
 fParent.fUndoRedoManager.IncreaseActionID;
end;

procedure TpvTextEditor.TView.MovePageDown;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fParent.fRope.CountCodePoints then begin
  fVisualLineCacheMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   fVisualLineCacheMap.Update(-1,LineIndex+fNonScrollVisibleAreaHeight+1);
   NewCodePointIndex:=fVisualLineCacheMap.GetCodePointIndexFromLineIndexAndColumnIndex(Max(0,Min(LineIndex+fNonScrollVisibleAreaHeight,fVisualLineCacheMap.fCountLines-1)),ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
   if (fCursorOffset.y+fNonScrollVisibleAreaHeight)>=fVisualLineCacheMap.fCountLines then begin
    fCursorOffset.y:=fVisualLineCacheMap.fCountLines-1;
   end else begin
    inc(fCursorOffset.y,fNonScrollVisibleAreaHeight);
   end;
  end;
  EnsureCodePointIndexIsInRange;
  EnsureCursorIsVisible(true);
 end;
 fParent.fUndoRedoManager.IncreaseActionID;
end;

procedure TpvTextEditor.TView.InsertLine;
var LineIndex,LineCodePointIndex:TpvSizeInt;
begin
 LineIndex:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
 if LineIndex>=0 then begin
  LineCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromLineIndex(LineIndex);
  fParent.LineMapTruncate(LineCodePointIndex,-1);
  if assigned(fParent.fSyntaxHighlighting) then begin
   fParent.fSyntaxHighlighting.Truncate(fCodePointIndex);
  end;
{$ifdef Windows}
  fParent.fUndoRedoManager.Add(TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex,TpvTextEditor.EmptyMarkState,fMarkState,LineCodePointIndex,2,#13#10));
  fParent.fRope.Insert(LineCodePointIndex,TpvUTF8String(#13#10));
  fParent.UpdateViewCodePointIndices(LineCodePointIndex,2);
{$else}
  fParent.fUndoRedoManager.Add(TUndoRedoCommandInsert.Create(fParent,fCodePointIndex,fCodePointIndex,TpvTextEditor.EmptyMarkState,fMarkState,LineCodePointIndex,1,#10));
  fParent.fRope.Insert(LineCodePointIndex,TpvUTF8String(#10));
  fParent.UpdateViewCodePointIndices(LineCodePointIndex,1);
{$endif}
  fParent.EnsureViewCodePointIndicesAreInRange;
  fParent.EnsureViewCursorsAreVisible(true);
  fParent.ResetViewMarkCodePointIndices;
 end;
end;

procedure TpvTextEditor.TView.DeleteLine;
var LineIndex,StartCodePointIndex,StopCodePointIndex:TpvSizeInt;
begin
 LineIndex:=fParent.fLineCacheMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
 if LineIndex>=0 then begin
  StartCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromLineIndex(LineIndex);
  StopCodePointIndex:=fParent.fLineCacheMap.GetCodePointIndexFromNextLineIndexOrTextEnd(LineIndex);
  if (StartCodePointIndex>=0) and
     (StartCodePointIndex<StopCodePointIndex) then begin
   fParent.fUndoRedoManager.Add(TUndoRedoCommandDelete.Create(fParent,fCodePointIndex,fCodePointIndex,TpvTextEditor.EmptyMarkState,fMarkState,StartCodePointIndex,StopCodePointIndex-StartCodePointIndex,fParent.fRope.Extract(StartCodePointIndex,StopCodePointIndex-StartCodePointIndex)));
   fParent.fRope.Delete(StartCodePointIndex,StopCodePointIndex-StartCodePointIndex);
   fParent.LineMapTruncate(Max(0,StartCodePointIndex)-1,-1);
   if assigned(fParent.fSyntaxHighlighting) then begin
    fParent.fSyntaxHighlighting.Truncate(Max(0,StartCodePointIndex)-1);
   end;
   fParent.UpdateViewCodePointIndices(fCodePointIndex,StartCodePointIndex-fCodePointIndex);
   fParent.EnsureViewCodePointIndicesAreInRange;
   fParent.EnsureViewCursorsAreVisible(true);
   fParent.ResetViewMarkCodePointIndices;
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

