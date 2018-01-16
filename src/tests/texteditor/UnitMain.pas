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
 unit UnitMain;
{$ifdef fpc}
 {$mode delphi}
{$else}
 {$legacyifend on}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}
{$scopedenums on}

interface

uses SysUtils,Classes,PasVulkan.Types,PasVulkan.TextEditor,UnitConsole;

var AbstractTextEditor:TpvAbstractTextEditor=nil;
    AbstractTextEditorBuffer:TpvAbstractTextEditor.TDrawBufferItems=nil;
    OverwriteMode:boolean=false;

procedure Main;

implementation

function RepChar(const aChar:Char;const aCount:Int32):string;
var Index:Int32;
begin
 SetLength(result,aCount);
 for Index:=1 to aCount do begin
  result[Index]:=aChar;
 end;
end;

procedure DisplayKeys;
 procedure AddFKey(const aFKeyNr:byte;const aName:string);
 begin
  Console.TextBackground(TConsole.TColor.Black);
  Console.TextColor(TConsole.TColor.LightGray);
  Console.Write(UTF8String(IntToStr(aFKeyNr)));
  Console.TextBackground(TConsole.TColor.Cyan);
  Console.TextColor(TConsole.TColor.Black);
  Console.Write(UTF8String(aName));
  Console.TextBackground(TConsole.TColor.Black);
  Console.TextColor(TConsole.TColor.LightGray);
  Console.Write(UTF8String(' '));
 end;
begin
 Console.GotoXY(1,Console.Height);
 AddFKey(1,'Help');
 AddFKey(2,'Save');
 AddFKey(3,'Find');
 AddFKey(4,'Repl');
 AddFKey(5,'Refr');
 if AbstractTextEditor.LineWrap=0 then begin
  AddFKey(6,'Wrap');
 end else begin
  AddFKey(6,'UnWr');
 end;
 AddFKey(7,'InsL');
 AddFKey(8,'DelL');
 AddFKey(9,'Load');
 AddFKey(10,'Quit');
 AddFKey(11,'Undo');
 AddFKey(12,'Redo');
 while Console.WhereX<Console.Width do begin
  Console.Write(' ');
 end;
 Console.Write(' ');
end;

procedure ClearEditScreen;
var Index:Int32;
begin
 Console.TextBackground(TConsole.TColor.Blue);
 Console.TextColor(TConsole.TColor.LightGray);
 for Index:=2 to Console.Height-1 do begin
  Console.GotoXY(1,Index);
  Console.Write(UTF8String(RepChar(#32,Console.Width)));
 end;
 Console.TextBackground(TConsole.TColor.Black);
 Console.TextColor(TConsole.TColor.LightGray);
end;

procedure ResetScreen;
begin

 Console.TextBackground(TConsole.TColor.Blue);
 Console.TextColor(TConsole.TColor.LightGray);
 Console.ClrScr;
 Console.TextBackground(TConsole.TColor.Black);
 Console.TextColor(TConsole.TColor.LightGray);

 Console.CursorOff;

 Console.TextBackground(TConsole.TColor.Cyan);
 Console.TextColor(TConsole.TColor.Black);
 Console.GotoXY(1,1);
 Console.Write(UTF8String(RepChar(#32,Console.Width)));

 Console.GotoXY(2,1);
 Console.Write('PasVulkan Test Text Editor');

 Console.GotoXY(Console.Width-6,1);
 Console.Write('[');
 Console.Write('-');
 Console.Write('-');
 Console.Write('-');
 if OverwriteMode then begin
  Console.Write('O');
 end else begin
  Console.Write('-');
 end;
 Console.Write(']');

 DisplayKeys;

 Console.TextBackground(TConsole.TColor.Black);
 Console.TextColor(TConsole.TColor.LightGray);

end;

procedure UpdateScreen;
var x,y,i:Int32;
begin

 ResetScreen;

 Console.GotoXY(1,2);

 Console.TextBackground(TConsole.TColor.Blue);
 Console.TextColor(TConsole.TColor.White);
//Console.TextColor(TConsole.TColor.LightCyan);

 AbstractTextEditor.VisibleAreaWidth:=Console.Width;
 AbstractTextEditor.VisibleAreaHeight:=Console.Height-2;
 AbstractTextEditor.NonScrollVisibleAreaWidth:=Console.Width;
 AbstractTextEditor.NonScrollVisibleAreaHeight:=Console.Height-2;

 AbstractTextEditor.FillDrawBuffer(AbstractTextEditorBuffer);

 i:=0;
 for y:=0 to AbstractTextEditor.VisibleAreaHeight-1 do begin
  for x:=0 to AbstractTextEditor.VisibleAreaWidth-1 do begin
   if i<length(AbstractTextEditorBuffer) then begin
    Console.WriteCodePointToBuffer(x+1,y+2,AbstractTextEditorBuffer[i].CodePoint);
   end;
   inc(i);
  end;
 end;

 Console.GotoXY(AbstractTextEditor.CursorX+1,AbstractTextEditor.CursorY+2);

 if OverwriteMode then begin
  Console.CursorBig;
 end else begin
  Console.CursorOn;
 end;

 Console.Flush;

end;

procedure Main;
var c:Int32;
begin

 AbstractTextEditor:=TpvAbstractTextEditor.Create;
 try

  repeat

   UpdateScreen;

   if {Console.KeyPressed}true then begin
    c:=Console.ReadKey;
    case c of
     0:begin
      // Code escape
      if Console.KeyPressed then begin
       c:=Console.ReadKey;
       case c of
        82:begin
         // Insert
         OverwriteMode:=not OverwriteMode;
        end;
        83:begin
         // Delete
         AbstractTextEditor.Delete;
        end;
        71:begin
         // Home
         AbstractTextEditor.MoveToLineBegin;
        end;
        79:begin
         // End
         AbstractTextEditor.MoveToLineEnd;
        end;
        73:begin
         // Page up
         AbstractTextEditor.MovePageUp;
        end;
        81:begin
         // Page down
         AbstractTextEditor.MovePageDown;
        end;
        72:begin
         // Up
         AbstractTextEditor.MoveUp;
        end;
        80:begin
         // Down
         AbstractTextEditor.MoveDown;
        end;
        75:begin
         // Left
         AbstractTextEditor.MoveLeft;
        end;
        77:begin
         // Right
         AbstractTextEditor.MoveRight;
        end;
        64:begin
         // F6
         if AbstractTextEditor.LineWrap=0 then begin
          AbstractTextEditor.LineWrap:=AbstractTextEditor.NonScrollVisibleAreaWidth;
         end else begin
          AbstractTextEditor.LineWrap:=0;
         end;
        end;
        65:begin
         // F7
         AbstractTextEditor.InsertLine;
        end;
        66:begin
         // F8
         AbstractTextEditor.DeleteLine;
        end;
        68:begin
         // F10
         break;
        end;
       end;
      end;
     end;
     8:begin
      // Backspace
      AbstractTextEditor.Backspace;
     end;
     13:begin
      AbstractTextEditor.Enter(OverwriteMode);
     end;
     27:begin
      // Escape
     end;
     else begin
      AbstractTextEditor.InsertCodePoint(c,OverwriteMode);
     end;
    end;
{  end else begin
    Sleep(10);}
   end;

  until false;

  AbstractTextEditorBuffer:=nil;

 finally
  AbstractTextEditor.Free;
 end;

 Console.TextBackground(TConsole.TColor.Black);
 Console.TextColor(TConsole.TColor.LightGray);
 Console.ClrScr;
 Console.GotoXY(1,1);
 Console.Flush;

end;

end.
