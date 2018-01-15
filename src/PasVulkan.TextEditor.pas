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
                                                          4,4,4,4,4,4,4,4,5,5,5,5,6,6,6,6); // f
              StateCharClasses:array[AnsiChar] of TpvUInt8=($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                                                            $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,
                                                            $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,
                                                            $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,
                                                            $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,
                                                            $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,
                                                            $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,
                                                            $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,
                                                            $05,$05,$05,$05,$05,$05,$05,$05,$06,$06,$06,$06,$07,$07,$08,$08);
              StateTransitions:array[TpvUInt8] of TpvUInt8=($00,$10,$10,$20,$30,$40,$50,$60,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$00,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$20,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$40,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$50,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
                                                            $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,
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
      private
       type TNodeEnumerator=record
             private
              fStringRope:TpvUTF8StringRope;
              fFirst:boolean;
              fNode:TNode;
              function GetCurrent:TNode; inline;
             public
              constructor Create(const aStringRope:TpvUTF8StringRope);
              function MoveNext:boolean; inline;
              property Current:TNode read GetCurrent;
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
       procedure SetText(const aString:TpvUTF8String);
       class function FindFirstSetBit(aValue:TpvUInt64):TpvUInt32; {$ifndef fpc}{$ifdef cpu386}stdcall;{$else}register;{$endif}{$endif} static;
       function GetRandomHeight:TpvInt32;
       class function GetCountCodeUnits(const aString:PAnsiChar;const aCountCodePoints:TpvSizeInt):TpvSizeInt; static;
       class function GetCountCodePoints(const aString:PAnsiChar;const aCountCodeUnits:TpvSizeInt):TpvSizeInt; static;
       class function GetCountCodeUnitsAndCheck(const aString:TpvUTF8String):TpvSizeInt; static;
       function FindNodePositionAtCodePoint(const aCodePointIndex:TpvSizeInt;out aNodePositionLinks:TNode.TNodePositionLinks):TNode;
       procedure UpdateOffsetList(var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
       procedure InsertAt(var aNodePositionLinks:TNode.TNodePositionLinks;const aString:PAnsiChar;const aCountCodeUnits,aCountCodePoints:TpvSizeInt);
       procedure InsertAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aString:TpvUTF8String);
       procedure DeleteAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
       function ExtractAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt):TpvUTF8String;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aString:TpvUTF8String); reintroduce; overload;
       constructor Create(const aFrom:TpvUTF8StringRope); reintroduce; overload;
       destructor Destroy; override;
       procedure Clear;
       procedure Insert(const aCodePointIndex:TpvSizeInt;const aString:TpvUTF8String);
       procedure Delete(const aCodePointIndex,aCountCodePoints:TpvSizeInt);
       function Extract(const aCodePointIndex,aCountCodePoints:TpvSizeInt):TpvUTF8String;
       function GetCodePoint(const aCodePointIndex:TpvSizeInt):TpvUInt32;
       function GetEnumerator:TNodeEnumerator;
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
       fRope:TpvUTF8StringRope;
       fLines:TLines;
       fCountLines:TpvSizeInt;
       fLineWrap:TpvSizeInt;
       fTabWidth:TpvSizeInt;
       fCountVisibleVisualCodePointsSinceNewLine:TpvSizeInt;
       fCodePointIndex:TpvSizeInt;
       fLastWasPossibleNewLineTwoCharSequence:boolean;
       fCodeUnit:AnsiChar;
       fCodePoint:TpvUInt32;
       fLastCodePoint:TpvUInt32;
       fNode:TpvUTF8StringRope.TNode;
       fNodeCodeUnitIndex:TpvSizeInt;
       fUTF8DFACharClass:TpvUInt8;
       fUTF8DFAState:TpvUInt8;
       fNodePositionLinks:TpvUTF8StringRope.TNode.TNodePositionLinks;
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
       procedure GetLineIndexAndColumnIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aLineIndex,aColumnIndex:TpvSizeInt);
       function GetCodePointIndexFromLineIndex(const aLineIndex:TpvSizeInt):TpvSizeInt;
       function GetCodePointIndexFromNextLineIndexOrTextEnd(const aLineIndex:TpvSizeInt):TpvSizeInt;
       function GetCodePointIndexFromLineIndexAndColumnIndex(const aLineIndex,aColumnIndex:TpvSizeInt):TpvSizeInt;
      published
       property CountLines:TpvSizeInt read fCountLines;
       property LineWrap:TpvSizeInt read fLineWrap write SetLineWrap;
       property TabWidth:TpvSizeInt read fTabWidth write SetTabWidth;
     end;

     TpvAbstractTextEditor=class
      public
       type TDrawBufferItem=record
             BackgroundColor:TpvUInt8;
             ForegroundColor:TpvUInt8;
             CodePoint:TpvUInt32;
            end;
            PDrawBufferItem=^TDrawBufferItem;
            TDrawBufferItems=array of TDrawBufferItem;
      private
       fVisibleAreaWidth:TpvSizeInt;
       fVisibleAreaHeight:TpvSizeInt;
       fNonScrollVisibleAreaWidth:TpvSizeInt;
       fNonScrollVisibleAreaHeight:TpvSizeInt;
       fVisibleAreaDirty:boolean;
       fStringRope:TpvUTF8StringRope;
       fStringRopeLineMap:TpvUTF8StringRopeLineMap;
       fStringRopeVisualLineMap:TpvUTF8StringRopeLineMap;
       fCodePointIndex:TpvSizeInt;
       fFirstVisualLineIndex:TpvSizeInt;
       fCursorX:TpvSizeInt;
       fCursorY:TpvSizeInt;
       procedure SetVisibleAreaWidth(const aVisibleAreaWidth:TpvSizeInt);
       procedure SetVisibleAreaHeight(const aVisibleAreaHeight:TpvSizeInt);
       procedure SetNonScrollVisibleAreaWidth(const aNonScrollVisibleAreaWidth:TpvSizeInt);
       procedure SetNonScrollVisibleAreaHeight(const aNonScrollVisibleAreaHeight:TpvSizeInt);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Update;
       procedure FillDrawBuffer(var aDrawBufferItems:TDrawBufferItems);
       function IsTwoCodePointNewLine(const aCodePoint:TpvUInt32):boolean;
       procedure InsertCodePoint(const aCodePoint:TpvUInt32;const aOverwrite:boolean);
       procedure InsertString(const aString:TpvUTF8String;const aOverwrite:boolean);
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
      published
       property VisibleAreaWidth:TpvSizeInt read fVisibleAreaWidth write SetVisibleAreaWidth;
       property VisibleAreaHeight:TpvSizeInt read fVisibleAreaHeight write SetVisibleAreaHeight;
       property NonScrollVisibleAreaWidth:TpvSizeInt read fNonScrollVisibleAreaWidth write SetNonScrollVisibleAreaWidth;
       property NonScrollVisibleAreaHeight:TpvSizeInt read fNonScrollVisibleAreaHeight write SetNonScrollVisibleAreaHeight;
       property CursorX:TpvSizeInt read fCursorX;
       property CursorY:TpvSizeInt read fCursorY;
     end;


implementation

uses PUCU;

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

constructor TpvUTF8StringRope.TNodeEnumerator.Create(const aStringRope:TpvUTF8StringRope);
begin
 fStringRope:=aStringRope;
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

constructor TpvUTF8StringRope.Create(const aString:TpvUTF8String);
begin
 Create;
 SetText(aString);
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

procedure TpvUTF8StringRope.SetText(const aString:TpvUTF8String);
begin
 Clear;
 Insert(0,aString);
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

class function TpvUTF8StringRope.GetCountCodeUnits(const aString:PAnsiChar;const aCountCodePoints:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=0;
 Index:=0;
 while Index<aCountCodePoints do begin
  inc(result,TpvUTF8DFA.CodePointSizes[aString[result]]);
  inc(Index);
 end;
end;

class function TpvUTF8StringRope.GetCountCodePoints(const aString:PAnsiChar;const aCountCodeUnits:TpvSizeInt):TpvSizeInt;
var Index:TpvSizeInt;
begin
 result:=0;
 Index:=0;
 while Index<aCountCodeUnits do begin
  inc(Index,TpvUTF8DFA.CodePointSizes[aString[Index]]);
  inc(result);
 end;
end;

class function TpvUTF8StringRope.GetCountCodeUnitsAndCheck(const aString:TpvUTF8String):TpvSizeInt;
var Index:TpvSizeInt;
    State:TpvUInt32;
begin
 State:=TpvUTF8DFA.StateAccept;
 result:=length(aString);
 for Index:=1 to result do begin
  State:=TpvUTF8DFA.StateTransitions[State+TpvUTF8DFA.StateCharClasses[aString[Index]]];
  if State<=TpvUTF8DFA.StateError then begin
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

procedure TpvUTF8StringRope.UpdateOffsetList(var aNodePositionLinks:TNode.TNodePositionLinks;const aCountCodePoints:TpvSizeInt);
var Index:TpvInt32;
begin
 for Index:=0 to fHead.fHeight-1 do begin
  inc(aNodePositionLinks[Index].fNode.fLinks[Index].fSkipSize,aCountCodePoints);
 end;
end;

procedure TpvUTF8StringRope.InsertAt(var aNodePositionLinks:TNode.TNodePositionLinks;const aString:PAnsiChar;const aCountCodeUnits,aCountCodePoints:TpvSizeInt);
var MaximumHeight,NewHeight,Index:TpvInt32;
    NewNode:TNode;
    PreviousNodeLink:TNode.PNodeLink;
begin

 MaximumHeight:=fHead.fHeight;

 NewHeight:=GetRandomHeight;

 NewNode:=TNode.Create(NewHeight);
 NewNode.fCountCodeUnits:=aCountCodeUnits;
 Move(aString[0],NewNode.fData[0],aCountCodeUnits);

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

procedure TpvUTF8StringRope.InsertAtNodePosition(const aNode:TNode;var aNodePositionLinks:TNode.TNodePositionLinks;const aString:TpvUTF8String);
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

 CountInsertedCodeUnits:=GetCountCodeUnitsAndCheck(aString);

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

  Move(aString[1],
       Node.fData[OffsetCodeUnits],
       CountInsertedCodeUnits);
  inc(Node.fCountCodeUnits,CountInsertedCodeUnits);

  inc(fCountCodeUnits,CountInsertedCodeUnits);

  CountInsertedCodePoints:=GetCountCodePoints(@aString[1],length(aString));

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
    CodePointSize:=TpvUTF8DFA.CodePointSizes[aString[StringOffset+CountNewNodeCodeUnits+1]];
    if (CodePointSize+CountNewNodeCodeUnits)<=TNode.StringSize then begin
     inc(CountNewNodeCodeUnits,CodePointSize);
     inc(CountNewNodeCodePoints);
    end else begin
     break;
    end;
   end;
   InsertAt(aNodePositionLinks,@aString[StringOffset+1],CountNewNodeCodeUnits,CountNewNodeCodePoints);
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

procedure TpvUTF8StringRope.Insert(const aCodePointIndex:TpvSizeInt;const aString:TpvUTF8String);
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
 InsertAtNodePosition(Node,NodePositionLinks,aString);
{$if defined(DebugTpvUTF8StringRope)}
 Check;
{$ifend}
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
 fRope:=aRope;
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
 fCodePoint:=0;
 fLastCodePoint:=0;
 fUTF8DFAState:=TpvUTF8DFA.StateAccept;
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
    fCodePoint:=0;
    fLastCodePoint:=0;
    fUTF8DFAState:=TpvUTF8DFA.StateAccept;
   end else begin
    Reset;
   end;
  end;
 end;

end;

procedure TpvUTF8StringRopeLineMap.Update(const aUntilCodePoint,aUntilLine:TpvSizeInt);
var DoStop:TpvInt32;
    DoNewLine,DoTab:boolean;
begin
 if (fCodePointIndex<fRope.fCountCodePoints) and
    ((aUntilCodePoint<0) or (fCodePointIndex<aUntilCodePoint)) and
    ((aUntilLine<0) or (fCountLines<aUntilLine)) then begin

  if fCodePointIndex=0 then begin
   fNode:=fRope.fHead;
   fNodeCodeUnitIndex:=0;
  end else begin
   fNode:=fRope.FindNodePositionAtCodePoint(fCodePointIndex,fNodePositionLinks);
   if assigned(fNode) then begin
    fNodeCodeUnitIndex:=TpvUTF8StringRope.GetCountCodeUnits(@fNode.fData[0],fNodePositionLinks[0].fSkipSize);
   end else begin
    fNodeCodeUnitIndex:=0;
   end;
  end;

  DoStop:=0;
  if assigned(fNode) then begin

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

     fCodeUnit:=fNode.fData[fNodeCodeUnitIndex];
     inc(fNodeCodeUnitIndex);

     fUTF8DFACharClass:=TpvUTF8DFA.StateCharClasses[fCodeUnit];

     case fUTF8DFAState of
      TpvUTF8DFA.StateAccept..TpvUTF8DFA.StateError:begin
       fCodePoint:=ord(fCodeUnit) and ($ff shr fUTF8DFACharClass);
      end;
      else begin
       fCodePoint:=(fCodePoint shl 6) or (ord(fCodeUnit) and $3f);
      end;
     end;

     fUTF8DFAState:=TpvUTF8DFA.StateTransitions[fUTF8DFAState+fUTF8DFACharClass];

     if fUTF8DFAState<=TpvUTF8DFA.StateError then begin

      if fUTF8DFAState<>TpvUTF8DFA.StateAccept then begin
       fCodePoint:=$fffd;
      end;

      inc(fCodePointIndex);

      DoTab:=false;

      DoNewLine:=false;

      case fCodePoint of
       $09:begin
        DoTab:=true;
        fLastWasPossibleNewLineTwoCharSequence:=false;
       end;
       $0a,$0d:begin
        if fLastWasPossibleNewLineTwoCharSequence and
           (((fCodePoint=$0a) and (fLastCodePoint=$0d)) or
            ((fCodePoint=$0d) and (fLastCodePoint=$0a))) then begin
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
       if DoTab and (fTabWidth>0) then begin
        inc(fCountVisibleVisualCodePointsSinceNewLine,fTabWidth-(fCountVisibleVisualCodePointsSinceNewLine mod fTabWidth));
       end else begin
        inc(fCountVisibleVisualCodePointsSinceNewLine);
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

      fLastCodePoint:=fCodePoint;

      if DoStop>0 then begin
       dec(DoStop);
       if DoStop=0 then begin
        DoStop:=-1;
       end;
      end;

     end;

    end;

    if DoStop<0 then begin
     break;
    end;

   until false;

  end;

 end;

end;

function TpvUTF8StringRopeLineMap.GetLineIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt):TpvSizeInt;
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

procedure TpvUTF8StringRopeLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(const aCodePointIndex:TpvSizeInt;out aLineIndex,aColumnIndex:TpvSizeInt);
var StartCodePointIndex,StopCodePointIndex,CurrentCodePointIndex,
    NodeCodeUnitIndex,StepWidth,CurrentColumn:TpvSizeInt;
    CodeUnit:AnsiChar;
    UTF8DFAState,UTF8DFACharClass:TpvUInt8;
    CodePoint,LastCodePoint:TpvUInt32;
    Node:TpvUTF8StringRope.TNode;
    LastWasPossibleNewLineTwoCharSequence:boolean;
    NodePositionLinks:TpvUTF8StringRope.TNode.TNodePositionLinks;
begin

 Update(-1,-1);//aLineIndex+2);

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

   if StartCodePointIndex=0 then begin
    Node:=fRope.fHead;
    NodeCodeUnitIndex:=0;
   end else begin
    Node:=fRope.FindNodePositionAtCodePoint(StartCodePointIndex,NodePositionLinks);
    if assigned(Node) then begin
     NodeCodeUnitIndex:=TpvUTF8StringRope.GetCountCodeUnits(@Node.fData[0],NodePositionLinks[0].fSkipSize);
    end else begin
     NodeCodeUnitIndex:=0;
    end;
   end;

   if assigned(Node) then begin

    CodePoint:=0;

    CurrentColumn:=0;

    CurrentCodePointIndex:=StartCodePointIndex;

    LastCodePoint:=0;

    LastWasPossibleNewLineTwoCharSequence:=false;

    UTF8DFAState:=TpvUTF8DFA.StateAccept;

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
        CodePoint:=ord(CodeUnit) and ($ff shr UTF8DFACharClass);
       end;
       else begin
        CodePoint:=(CodePoint shl 6) or (ord(CodeUnit) and $3f);
       end;
      end;

      UTF8DFAState:=TpvUTF8DFA.StateTransitions[UTF8DFAState+UTF8DFACharClass];

      if UTF8DFAState<=TpvUTF8DFA.StateError then begin

       if UTF8DFAState<>TpvUTF8DFA.StateAccept then begin
        CodePoint:=$fffd;
       end;

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

       if CurrentCodePointIndex>=StopCodePointIndex then begin
        break;
       end;

      end;

     end;

    until false;

    if CurrentCodePointIndex=fRope.CountCodePoints then begin
     inc(aColumnIndex);
    end;

   end;

  end;

 end;

end;

function TpvUTF8StringRopeLineMap.GetCodePointIndexFromLineIndex(const aLineIndex:TpvSizeInt):TpvSizeInt;
begin
 Update(-1,-1);//aLineIndex+1);
 if (aLineIndex>=0) and (aLineIndex<fCountLines) then begin
  result:=fLines[aLineIndex];
 end else begin
  result:=-1;
 end;
end;

function TpvUTF8StringRopeLineMap.GetCodePointIndexFromNextLineIndexOrTextEnd(const aLineIndex:TpvSizeInt):TpvSizeInt;
begin
 Update(-1,-1);//aLineIndex+2);
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

function TpvUTF8StringRopeLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(const aLineIndex,aColumnIndex:TpvSizeInt):TpvSizeInt;
var StartCodePointIndex,StopCodePointIndex,CurrentCodePointIndex,
    NodeCodeUnitIndex,StepWidth,CurrentColumn:TpvSizeInt;
    CodeUnit:AnsiChar;
    UTF8DFAState,UTF8DFACharClass:TpvUInt8;
    CodePoint,LastCodePoint:TpvUInt32;
    Node:TpvUTF8StringRope.TNode;
    LastWasPossibleNewLineTwoCharSequence:boolean;
    NodePositionLinks:TpvUTF8StringRope.TNode.TNodePositionLinks;
begin
 Update(-1,-1);//aLineIndex+2);
 if (aLineIndex>=0) and (aLineIndex<fCountLines) then begin

  result:=fLines[aLineIndex];

  StartCodePointIndex:=result;

  if (aLineIndex+1)<fCountLines then begin
   StopCodePointIndex:=fLines[aLineIndex+1];
  end else begin
   StopCodePointIndex:=fRope.CountCodePoints;
  end;

  if StartCodePointIndex=0 then begin
   Node:=fRope.fHead;
   NodeCodeUnitIndex:=0;
  end else begin
   Node:=fRope.FindNodePositionAtCodePoint(StartCodePointIndex,NodePositionLinks);
   if assigned(Node) then begin
    NodeCodeUnitIndex:=TpvUTF8StringRope.GetCountCodeUnits(@Node.fData[0],NodePositionLinks[0].fSkipSize);
   end else begin
    NodeCodeUnitIndex:=0;
   end;
  end;

  if assigned(Node) then begin

   CodePoint:=0;

   CurrentColumn:=0;

   CurrentCodePointIndex:=StartCodePointIndex;

   UTF8DFAState:=TpvUTF8DFA.StateAccept;

   LastCodePoint:=0;

   LastWasPossibleNewLineTwoCharSequence:=false;

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
       CodePoint:=ord(CodeUnit) and ($ff shr UTF8DFACharClass);
      end;
      else begin
       CodePoint:=(CodePoint shl 6) or (ord(CodeUnit) and $3f);
      end;
     end;

     UTF8DFAState:=TpvUTF8DFA.StateTransitions[UTF8DFAState+UTF8DFACharClass];

     if UTF8DFAState<=TpvUTF8DFA.StateError then begin

      if UTF8DFAState<>TpvUTF8DFA.StateAccept then begin
       CodePoint:=$fffd;
      end;

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

      if (CurrentCodePointIndex>=StopCodePointIndex) then begin
       break;
      end;

     end;

    end;

   until false;

   if (CurrentColumn<=aColumnIndex) and (CurrentCodePointIndex=fRope.CountCodePoints) then begin
    result:=fRope.CountCodePoints;
   end;

  end;

 end else begin

  result:=-1;

 end;

end;

constructor TpvAbstractTextEditor.Create;
begin
 inherited Create;
 fVisibleAreaDirty:=false;
 fStringRope:=TpvUTF8StringRope.Create;
//fStringRope.Text:=UTF8Encode('Hello world'#10'Hello world'#10'Hello world'#10'Hello world'#10'Hello ä'#10#10);
 fStringRope.Text:='Hello world'#13#10'Hello world'#13#10'Hello world'#13#10'Hello world'#13#10'Hello'#13#10#13#10;
// fStringRope.Text:='Hello world'#10'Hello world'#10'Hello world'#10'Hello world'#10'Hello'#10#10;
 fStringRopeLineMap:=TpvUTF8StringRopeLineMap.Create(fStringRope);
 fStringRopeVisualLineMap:=TpvUTF8StringRopeLineMap.Create(fStringRope);
//fStringRopeVisualLineMap.LineWrap:=80;
 fCodePointIndex:=0;
 fFirstVisualLineIndex:=0;
end;

destructor TpvAbstractTextEditor.Destroy;
begin
 fStringRopeLineMap.Free;
 fStringRopeVisualLineMap.Free;
 fStringRope.Free;
 inherited Destroy;
end;

procedure TpvAbstractTextEditor.SetVisibleAreaWidth(const aVisibleAreaWidth:TpvSizeInt);
begin
 if fVisibleAreaWidth<>aVisibleAreaWidth then begin
  fVisibleAreaWidth:=aVisibleAreaWidth;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvAbstractTextEditor.SetVisibleAreaHeight(const aVisibleAreaHeight:TpvSizeInt);
begin
 if fVisibleAreaHeight<>aVisibleAreaHeight then begin
  fVisibleAreaHeight:=aVisibleAreaHeight;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvAbstractTextEditor.SetNonScrollVisibleAreaWidth(const aNonScrollVisibleAreaWidth:TpvSizeInt);
begin
 if fNonScrollVisibleAreaWidth<>aNonScrollVisibleAreaWidth then begin
  fNonScrollVisibleAreaWidth:=aNonScrollVisibleAreaWidth;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvAbstractTextEditor.SetNonScrollVisibleAreaHeight(const aNonScrollVisibleAreaHeight:TpvSizeInt);
begin
 if fNonScrollVisibleAreaHeight<>aNonScrollVisibleAreaHeight then begin
  fNonScrollVisibleAreaHeight:=aNonScrollVisibleAreaHeight;
  fVisibleAreaDirty:=true;
 end;
end;

procedure TpvAbstractTextEditor.Update;
begin

end;

procedure TpvAbstractTextEditor.FillDrawBuffer(var aDrawBufferItems:TDrawBufferItems);
var BufferSize,BufferBaseIndex,BufferBaseEndIndex,BufferIndex,
    VisualLineIndex,CountRemainingLines,
    VisualLineStartCodePointIndex,VisualLineStopCodePointIndex,
    CurrentCodePointIndex,LocalCursorX,LocalCursorY,StepWidth:TpvSizeInt;
    CodePoint:TpvUInt32;
    LastWasNewLine:boolean;
    CodeUnit:AnsiChar;
    Node:TpvUTF8StringRope.TNode;
    NodeCodeUnitIndex:TpvSizeInt;
    UTF8DFACharClass:TpvUInt8;
    UTF8DFAState:TpvUInt8;
    NodePositionLinks:TpvUTF8StringRope.TNode.TNodePositionLinks;
begin

 fCursorX:=0;
 fCursorY:=0;

 LocalCursorX:=0;
 LocalCursorY:=0;

 BufferSize:=VisibleAreaWidth*VisibleAreaHeight;

 if BufferSize>0 then begin

  if length(aDrawBufferItems)<>BufferSize then begin
   SetLength(aDrawBufferItems,BufferSize);
  end;

  FillChar(aDrawBufferItems[0],BufferSize*SizeOf(TDrawBufferItem),#0);

  BufferIndex:=0;
  while BufferIndex<BufferSize do begin
   aDrawBufferItems[BufferIndex].CodePoint:=32;
   inc(BufferIndex);
  end;

  VisualLineIndex:=fFirstVisualLineIndex;

  CountRemainingLines:=VisibleAreaHeight;

  BufferBaseIndex:=0;

  Node:=nil;

  NodeCodeUnitIndex:=0;

  CurrentCodePointIndex:=-1;

  while CountRemainingLines>0 do begin

   VisualLineStartCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndex(VisualLineIndex);
   if (VisualLineStartCodePointIndex<0) or
      (VisualLineStartCodePointIndex>=fStringRope.fCountCodePoints) then begin
    break;
   end;

   VisualLineStopCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromNextLineIndexOrTextEnd(VisualLineIndex);

   BufferBaseEndIndex:=BufferBaseIndex+VisibleAreaWidth;

   BufferIndex:=BufferBaseIndex;

   LocalCursorX:=0;

   LastWasNewLine:=false;

   if VisualLineStartCodePointIndex=0 then begin
    Node:=fStringRope.fHead;
    NodeCodeUnitIndex:=0;
   end else begin
    if not (assigned(Node) and (CurrentCodePointIndex=VisualLineStartCodePointIndex)) then begin
     Node:=fStringRope.FindNodePositionAtCodePoint(VisualLineStartCodePointIndex,NodePositionLinks);
     if assigned(Node) then begin
      NodeCodeUnitIndex:=TpvUTF8StringRope.GetCountCodeUnits(@Node.fData[0],NodePositionLinks[0].fSkipSize);
     end else begin
      NodeCodeUnitIndex:=0;
     end;
    end;
   end;

   CurrentCodePointIndex:=VisualLineStartCodePointIndex;

   UTF8DFAState:=TpvUTF8DFA.StateAccept;

   CodePoint:=0;

   if (CurrentCodePointIndex<VisualLineStopCodePointIndex) and
      assigned(Node) then begin

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
        CodePoint:=ord(CodeUnit) and ($ff shr UTF8DFACharClass);
       end;
       else begin
        CodePoint:=(CodePoint shl 6) or (ord(CodeUnit) and $3f);
       end;
      end;

      UTF8DFAState:=TpvUTF8DFA.StateTransitions[UTF8DFAState+UTF8DFACharClass];

      if UTF8DFAState<=TpvUTF8DFA.StateError then begin

       if UTF8DFAState<>TpvUTF8DFA.StateAccept then begin
        CodePoint:=$fffd;
       end;

       if fCodePointIndex=CurrentCodePointIndex then begin
        fCursorX:=LocalCursorX;
        fCursorY:=LocalCursorY;
       end;

       StepWidth:=1;

       LastWasNewLine:=false;

       case CodePoint of
        9:begin
         CodePoint:=32;
         StepWidth:=Max(1,(fStringRopeVisualLineMap.fTabWidth-(LocalCursorX mod fStringRopeVisualLineMap.fTabWidth)));
        end;
        10:begin
         CodePoint:=48;
         LastWasNewLine:=true;
         StepWidth:=1;
        end;
        13:begin
         CodePoint:=49;
         StepWidth:=1;
        end;
       end;

       if (BufferIndex<BufferSize) and (StepWidth>0) then begin
        aDrawBufferItems[BufferIndex].CodePoint:=CodePoint;
       end;

       inc(BufferIndex,StepWidth);

       inc(LocalCursorX,StepWidth);

       inc(CurrentCodePointIndex);

       if CurrentCodePointIndex>=VisualLineStopCodePointIndex then begin
        break;
       end;

      end;

     end;

    until false;

   end;

   if fCodePointIndex>=fStringRope.CountCodePoints then begin
    if LastWasNewLine then begin
     fCursorX:=0;
     fCursorY:=LocalCursorY+1;
    end else begin
     fCursorX:=LocalCursorX;
     fCursorY:=LocalCursorY;
    end;
   end;

   inc(BufferBaseIndex,VisibleAreaWidth);

   inc(LocalCursorY);

   dec(CountRemainingLines);

   inc(VisualLineIndex);

  end;

 end;

end;

function TpvAbstractTextEditor.IsTwoCodePointNewLine(const aCodePoint:TpvUInt32):boolean;
var Temporary:TpvUTF8String;
begin
 result:=false;
 if (aCodePoint>=0) and
    ((aCodePoint+1)<fStringRope.fCountCodePoints) then begin
  Temporary:=fStringRope.Extract(aCodePoint,2);
  result:=(Temporary=TpvUTF8String(#13#10)) or
          (Temporary=TpvUTF8String(#10#13));
 end;
end;

procedure TpvAbstractTextEditor.InsertCodePoint(const aCodePoint:TpvUInt32;const aOverwrite:boolean);
begin
 fStringRopeLineMap.Truncate(fCodePointIndex,-1);
 fStringRopeVisualLineMap.Truncate(fCodePointIndex,-1);
 if aOverwrite and (fCodePointIndex<fStringRope.fCountCodePoints) then begin
  fStringRope.Delete(fCodePointIndex,1);
 end;
 fStringRope.Insert(fCodePointIndex,PUCUUTF32CharToUTF8(aCodePoint));
 inc(fCodePointIndex);
{fStringRopeLineMap.Update(-1,-1);
 fStringRopeVisualLineMap.Update(-1,-1);}
end;

procedure TpvAbstractTextEditor.InsertString(const aString:TpvUTF8String;const aOverwrite:boolean);
var CountCodePoints:TpvSizeInt;
begin
 CountCodePoints:=TpvUTF8StringRope.GetCountCodePoints(@aString[1],length(aString));
 fStringRopeLineMap.Truncate(fCodePointIndex,-1);
 fStringRopeVisualLineMap.Truncate(fCodePointIndex,-1);
 if aOverwrite and (fCodePointIndex<fStringRope.fCountCodePoints) then begin
  fStringRope.Delete(fCodePointIndex,CountCodePoints);
 end;
 fStringRope.Insert(fCodePointIndex,aString);
 inc(fCodePointIndex,CountCodePoints);
{fStringRopeLineMap.Update(-1,-1);
 fStringRopeVisualLineMap.Update(-1,-1);}
end;

procedure TpvAbstractTextEditor.Backspace;
var Count:TpvSizeInt;
begin
 if (fCodePointIndex>0) and (fCodePointIndex<=fStringRope.fCountCodePoints) then begin
  if IsTwoCodePointNewLine(fCodePointIndex-2) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  dec(fCodePointIndex,Count);
  fStringRope.Delete(fCodePointIndex,Count);
  if fCodePointIndex>0 then begin
   fStringRopeLineMap.Truncate(fCodePointIndex-1,-1);
   fStringRopeVisualLineMap.Truncate(fCodePointIndex-1,-1);
  end else begin
   fStringRopeLineMap.Truncate(fCodePointIndex,-1);
   fStringRopeVisualLineMap.Truncate(fCodePointIndex,-1);
  end;
{ fStringRopeLineMap.Update(-1,-1);
  fStringRopeVisualLineMap.Update(-1,-1);}
 end;
end;

procedure TpvAbstractTextEditor.Delete;
var Count:TpvSizeInt;
    Temporary:TpvUTF8String;
begin
 if fCodePointIndex<fStringRope.fCountCodePoints then begin
  if IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  fStringRope.Delete(fCodePointIndex,Count);
  if fCodePointIndex>0 then begin
   fStringRopeLineMap.Truncate(fCodePointIndex-1,-1);
   fStringRopeVisualLineMap.Truncate(fCodePointIndex-1,-1);
  end else begin
   fStringRopeLineMap.Truncate(fCodePointIndex,-1);
   fStringRopeVisualLineMap.Truncate(fCodePointIndex,-1);
  end;
{ fStringRopeLineMap.Update(-1,-1);
  fStringRopeVisualLineMap.Update(-1,-1);}
 end;
end;

procedure TpvAbstractTextEditor.Enter(const aOverwrite:boolean);
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
end;

procedure TpvAbstractTextEditor.MoveUp;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fStringRope.CountCodePoints then begin
  fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex-1,ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
end;

procedure TpvAbstractTextEditor.MoveDown;
var LineIndex,ColumnIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<fStringRope.CountCodePoints then begin
  fStringRopeVisualLineMap.GetLineIndexAndColumnIndexFromCodePointIndex(fCodePointIndex,LineIndex,ColumnIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex+1,ColumnIndex);
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
end;

procedure TpvAbstractTextEditor.MoveLeft;
var Count:TpvSizeInt;
begin
 if fCodePointIndex>0 then begin
  if IsTwoCodePointNewLine(fCodePointIndex-2) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  dec(fCodePointIndex,Count);
 end;
end;

procedure TpvAbstractTextEditor.MoveRight;
var Count:TpvSizeInt;
begin
 if fCodePointIndex<fStringRope.CountCodePoints then begin
  if IsTwoCodePointNewLine(fCodePointIndex) then begin
   Count:=2;
  end else begin
   Count:=1;
  end;
  inc(fCodePointIndex,Count);
 end;
end;

procedure TpvAbstractTextEditor.MoveToLineBegin;
var LineIndex:TpvSizeInt;
begin
 if fCodePointIndex<fStringRope.CountCodePoints then begin
  LineIndex:=fStringRopeLineMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
  fCodePointIndex:=fStringRopeLineMap.GetCodePointIndexFromLineIndex(LineIndex);
 end else if (fCodePointIndex>0) and (fCodePointIndex>=fStringRope.CountCodePoints) then begin
  LineIndex:=fStringRopeLineMap.GetLineIndexFromCodePointIndex(fStringRope.CountCodePoints);
  fCodePointIndex:=fStringRopeLineMap.GetCodePointIndexFromLineIndex(LineIndex);
 end;
end;

procedure TpvAbstractTextEditor.MoveToLineEnd;
var LineIndex,NewCodePointIndex:TpvSizeInt;
begin
 if fCodePointIndex<=fStringRope.CountCodePoints then begin
  LineIndex:=fStringRopeVisualLineMap.GetLineIndexFromCodePointIndex(fCodePointIndex);
  if LineIndex>=0 then begin
   NewCodePointIndex:=fStringRopeVisualLineMap.GetCodePointIndexFromLineIndexAndColumnIndex(LineIndex,High(TpvSizeInt));
   if NewCodePointIndex>=0 then begin
    fCodePointIndex:=NewCodePointIndex;
   end;
  end;
 end;
end;

procedure TpvAbstractTextEditor.MovePageUp;
begin
end;

procedure TpvAbstractTextEditor.MovePageDown;
begin
end;

end.
