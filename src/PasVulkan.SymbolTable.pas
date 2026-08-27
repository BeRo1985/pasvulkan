(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.SymbolTable;
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
     PasVulkan.Types;

// A compact address to source location table, together with the reader for it.
//
// The motivation is that Delphi has no equivalent of the lnfodwrf unit which
// FreePascal offers, so a Delphi built crash log can only ever show module plus
// offset. Resolving that afterwards needs the matching .map file, which nobody
// has when a crash log arrives from a player.
//
// So the mapsymbols tool under src/tools/mapsymbols converts a .map into this
// format and appends it to the executable, and the reader below picks it up at
// crash time. The result is that a shipped build symbolicates its own crash
// logs, which no separate symbol file, PDB included, can do.
//
// The blob is appended behind the end of the image and located through a footer
// at the very end of the file. That works for PE and for ELF alike, which keeps
// one implementation for both of the operating systems PasVulkan targets, and
// it keeps the tool free of any executable format specific writing code.
//
// Everything is stored as an RVA, so relative to the image base rather than as
// an absolute address. The reader turns a runtime address into an RVA using the
// actual load address of the module, which makes the whole thing indifferent to
// address space layout randomization.

const pvSymbolTableMagic:array[0..7] of AnsiChar=('P','V','S','Y','M','T','A','B');

      pvSymbolTableVersion=TpvUInt32(1);

type PpvSymbolTableHeader=^TpvSymbolTableHeader;
     TpvSymbolTableHeader=packed record
      Magic:array[0..7] of AnsiChar;
      Version:TpvUInt32;
      Flags:TpvUInt32;
      UnitCount:TpvUInt32;
      SymbolCount:TpvUInt32;
      LineCount:TpvUInt32;
      StringSize:TpvUInt32;
     end;

     PpvSymbolTableUnitEntry=^TpvSymbolTableUnitEntry;
     TpvSymbolTableUnitEntry=packed record
      StartRVA:TpvUInt64;
      Size:TpvUInt64;
      NameOffset:TpvUInt32;
      FileNameOffset:TpvUInt32;
     end;

     PpvSymbolTableSymbolEntry=^TpvSymbolTableSymbolEntry;
     TpvSymbolTableSymbolEntry=packed record
      RVA:TpvUInt64;
      NameOffset:TpvUInt32;
      Reserved:TpvUInt32;
     end;

     PpvSymbolTableLineEntry=^TpvSymbolTableLineEntry;
     TpvSymbolTableLineEntry=packed record
      RVA:TpvUInt64;
      LineNumber:TpvUInt32;
      UnitIndex:TpvUInt32;
     end;

     PpvSymbolTableFooter=^TpvSymbolTableFooter;
     TpvSymbolTableFooter=packed record
      Magic:array[0..7] of AnsiChar;
      Offset:TpvUInt64;
     end;

     TpvSymbolTableLocation=record
      UnitName:TpvUTF8String;
      SymbolName:TpvUTF8String;
      FileName:TpvUTF8String;
      LineNumber:TpvUInt32;
      SymbolRVA:TpvUInt64;
     end;

     TpvSymbolTable=class
      private
       fData:TpvPointer;
       fSize:TpvSizeInt;
       fHeader:PpvSymbolTableHeader;
       fUnits:PpvSymbolTableUnitEntry;
       fSymbols:PpvSymbolTableSymbolEntry;
       fLines:PpvSymbolTableLineEntry;
       fStrings:PAnsiChar;
       function GetString(const aOffset:TpvUInt32):TpvUTF8String;
       function FindSymbol(const aRVA:TpvUInt64):TpvSizeInt;
       function FindLine(const aRVA:TpvUInt64):TpvSizeInt;
       function FindUnit(const aRVA:TpvUInt64):TpvSizeInt;
      public
       constructor Create;
       destructor Destroy; override;
       // Reads an appended table from the given file. Returns false when the
       // file carries none, which is the normal case for a build the mapsymbols
       // tool has not been run on.
       function LoadFromFile(const aFileName:String):Boolean;
       // Resolves an image relative address. Returns false when nothing covers
       // it, for example an address inside a system library.
       function Resolve(const aRVA:TpvUInt64;out aLocation:TpvSymbolTableLocation):Boolean;
       function Loaded:Boolean;
     end;

implementation

constructor TpvSymbolTable.Create;
begin
 inherited Create;
 fData:=nil;
 fSize:=0;
 fHeader:=nil;
 fUnits:=nil;
 fSymbols:=nil;
 fLines:=nil;
 fStrings:=nil;
end;

destructor TpvSymbolTable.Destroy;
begin
 if assigned(fData) then begin
  FreeMem(fData);
  fData:=nil;
 end;
 inherited Destroy;
end;

function TpvSymbolTable.Loaded:Boolean;
begin
 result:=assigned(fHeader);
end;

function TpvSymbolTable.LoadFromFile(const aFileName:String):Boolean;
var Stream:TFileStream;
    Footer:TpvSymbolTableFooter;
    Header:TpvSymbolTableHeader;
    Expected:TpvUInt64;
    Index:TpvSizeInt;
begin

 result:=false;

 if assigned(fData) then begin
  FreeMem(fData);
  fData:=nil;
 end;
 fSize:=0;
 fHeader:=nil;

 if not FileExists(aFileName) then begin
  exit;
 end;

 try

  Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  try

   if Stream.Size<TpvInt64(SizeOf(TpvSymbolTableFooter)+SizeOf(TpvSymbolTableHeader)) then begin
    exit;
   end;

   Stream.Seek(-TpvInt64(SizeOf(TpvSymbolTableFooter)),soEnd);
   Stream.ReadBuffer(Footer,SizeOf(TpvSymbolTableFooter));

   for Index:=0 to 7 do begin
    if Footer.Magic[Index]<>pvSymbolTableMagic[Index] then begin
     exit;
    end;
   end;

   if (Footer.Offset=0) or
      (Footer.Offset>=TpvUInt64(Stream.Size-TpvInt64(SizeOf(TpvSymbolTableFooter)))) then begin
    exit;
   end;

   Stream.Seek(TpvInt64(Footer.Offset),soBeginning);
   Stream.ReadBuffer(Header,SizeOf(TpvSymbolTableHeader));

   for Index:=0 to 7 do begin
    if Header.Magic[Index]<>pvSymbolTableMagic[Index] then begin
     exit;
    end;
   end;

   if Header.Version<>pvSymbolTableVersion then begin
    exit;
   end;

   // Check that the announced contents actually fit into what is there, so that
   // a truncated or otherwise damaged file cannot send the reader off into
   // arbitrary memory later on.
   Expected:=TpvUInt64(SizeOf(TpvSymbolTableHeader))+
             (TpvUInt64(Header.UnitCount)*TpvUInt64(SizeOf(TpvSymbolTableUnitEntry)))+
             (TpvUInt64(Header.SymbolCount)*TpvUInt64(SizeOf(TpvSymbolTableSymbolEntry)))+
             (TpvUInt64(Header.LineCount)*TpvUInt64(SizeOf(TpvSymbolTableLineEntry)))+
             TpvUInt64(Header.StringSize);
   if (Header.StringSize=0) or
      (Expected>TpvUInt64(TpvInt64(Stream.Size)-TpvInt64(Footer.Offset)-TpvInt64(SizeOf(TpvSymbolTableFooter)))) then begin
    exit;
   end;

   fSize:=TpvSizeInt(Expected);
   GetMem(fData,fSize);
   Stream.Seek(TpvInt64(Footer.Offset),soBeginning);
   Stream.ReadBuffer(fData^,fSize);

  finally
   FreeAndNil(Stream);
  end;

  fHeader:=PpvSymbolTableHeader(fData);
  fUnits:=PpvSymbolTableUnitEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fData)+TpvPtrUInt(SizeOf(TpvSymbolTableHeader)))));
  fSymbols:=PpvSymbolTableSymbolEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fUnits)+(TpvPtrUInt(fHeader^.UnitCount)*TpvPtrUInt(SizeOf(TpvSymbolTableUnitEntry))))));
  fLines:=PpvSymbolTableLineEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fSymbols)+(TpvPtrUInt(fHeader^.SymbolCount)*TpvPtrUInt(SizeOf(TpvSymbolTableSymbolEntry))))));
  fStrings:=PAnsiChar(TpvPointer(TpvPtrUInt(TpvPtrUInt(fLines)+(TpvPtrUInt(fHeader^.LineCount)*TpvPtrUInt(SizeOf(TpvSymbolTableLineEntry))))));

  // The string block must end in a terminator, otherwise GetString could run
  // past the end of the buffer.
  if fStrings[fHeader^.StringSize-1]<>#0 then begin
   FreeMem(fData);
   fData:=nil;
   fHeader:=nil;
   fSize:=0;
   exit;
  end;

  result:=true;

 except
  if assigned(fData) then begin
   FreeMem(fData);
   fData:=nil;
  end;
  fHeader:=nil;
  fSize:=0;
  result:=false;
 end;

end;

function TpvSymbolTable.GetString(const aOffset:TpvUInt32):TpvUTF8String;
begin
 if assigned(fHeader) and (aOffset<fHeader^.StringSize) then begin
  result:=TpvUTF8String(PAnsiChar(TpvPointer(TpvPtrUInt(TpvPtrUInt(fStrings)+TpvPtrUInt(aOffset)))));
 end else begin
  result:='';
 end;
end;

function TpvSymbolTable.FindSymbol(const aRVA:TpvUInt64):TpvSizeInt;
var Low,High,Middle:TpvSizeInt;
begin
 // Greatest entry with an RVA which is not past the wanted address.
 result:=-1;
 Low:=0;
 High:=TpvSizeInt(fHeader^.SymbolCount)-1;
 while Low<=High do begin
  Middle:=Low+((High-Low) shr 1);
  if PpvSymbolTableSymbolEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fSymbols)+(TpvPtrUInt(Middle)*TpvPtrUInt(SizeOf(TpvSymbolTableSymbolEntry))))))^.RVA<=aRVA then begin
   result:=Middle;
   Low:=Middle+1;
  end else begin
   High:=Middle-1;
  end;
 end;
end;

function TpvSymbolTable.FindLine(const aRVA:TpvUInt64):TpvSizeInt;
var Low,High,Middle:TpvSizeInt;
begin
 result:=-1;
 Low:=0;
 High:=TpvSizeInt(fHeader^.LineCount)-1;
 while Low<=High do begin
  Middle:=Low+((High-Low) shr 1);
  if PpvSymbolTableLineEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fLines)+(TpvPtrUInt(Middle)*TpvPtrUInt(SizeOf(TpvSymbolTableLineEntry))))))^.RVA<=aRVA then begin
   result:=Middle;
   Low:=Middle+1;
  end else begin
   High:=Middle-1;
  end;
 end;
end;

function TpvSymbolTable.FindUnit(const aRVA:TpvUInt64):TpvSizeInt;
var Low,High,Middle:TpvSizeInt;
    Entry:PpvSymbolTableUnitEntry;
begin
 result:=-1;
 Low:=0;
 High:=TpvSizeInt(fHeader^.UnitCount)-1;
 while Low<=High do begin
  Middle:=Low+((High-Low) shr 1);
  Entry:=PpvSymbolTableUnitEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fUnits)+(TpvPtrUInt(Middle)*TpvPtrUInt(SizeOf(TpvSymbolTableUnitEntry))))));
  if Entry^.StartRVA>aRVA then begin
   High:=Middle-1;
  end else if (Entry^.StartRVA+Entry^.Size)<=aRVA then begin
   Low:=Middle+1;
  end else begin
   result:=Middle;
   exit;
  end;
 end;
end;

function TpvSymbolTable.Resolve(const aRVA:TpvUInt64;out aLocation:TpvSymbolTableLocation):Boolean;
var SymbolIndex,LineIndex,UnitIndex:TpvSizeInt;
    SymbolEntry:PpvSymbolTableSymbolEntry;
    LineEntry:PpvSymbolTableLineEntry;
    UnitEntry:PpvSymbolTableUnitEntry;
begin

 aLocation.UnitName:='';
 aLocation.SymbolName:='';
 aLocation.FileName:='';
 aLocation.LineNumber:=0;
 aLocation.SymbolRVA:=0;

 result:=false;

 if not assigned(fHeader) then begin
  exit;
 end;

 UnitIndex:=FindUnit(aRVA);
 if UnitIndex<0 then begin
  // Outside of every known unit, so this address does not belong to the image
  // the table was built for.
  exit;
 end;

 UnitEntry:=PpvSymbolTableUnitEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fUnits)+(TpvPtrUInt(UnitIndex)*TpvPtrUInt(SizeOf(TpvSymbolTableUnitEntry))))));
 aLocation.UnitName:=GetString(UnitEntry^.NameOffset);
 aLocation.FileName:=GetString(UnitEntry^.FileNameOffset);

 SymbolIndex:=FindSymbol(aRVA);
 if SymbolIndex>=0 then begin
  SymbolEntry:=PpvSymbolTableSymbolEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fSymbols)+(TpvPtrUInt(SymbolIndex)*TpvPtrUInt(SizeOf(TpvSymbolTableSymbolEntry))))));
  // Only trust the symbol when it lies inside the same unit, otherwise it is
  // just the last one before a gap and would name the wrong routine.
  if (SymbolEntry^.RVA>=UnitEntry^.StartRVA) and
     (SymbolEntry^.RVA<(UnitEntry^.StartRVA+UnitEntry^.Size)) then begin
   aLocation.SymbolName:=GetString(SymbolEntry^.NameOffset);
   aLocation.SymbolRVA:=SymbolEntry^.RVA;
  end;
 end;

 LineIndex:=FindLine(aRVA);
 if LineIndex>=0 then begin
  LineEntry:=PpvSymbolTableLineEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fLines)+(TpvPtrUInt(LineIndex)*TpvPtrUInt(SizeOf(TpvSymbolTableLineEntry))))));
  // Same reasoning as for the symbol above, a line record only counts when it
  // belongs to the unit the address is in, since not every unit necessarily
  // carries line information.
  if LineEntry^.UnitIndex=TpvUInt32(UnitIndex) then begin
   aLocation.LineNumber:=LineEntry^.LineNumber;
  end;
 end;

 result:=true;

end;

end.
