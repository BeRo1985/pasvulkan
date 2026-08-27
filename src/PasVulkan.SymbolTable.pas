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
     {$ifdef PasVulkanSymbolTableCompression}
      // Only for reading a packed table back. It is behind the define because
      // unpacking means asking for a block of memory at the one moment where
      // that request is the least likely to succeed, namely while a report is
      // being written after the heap has been damaged.
      PasVulkan.Compression.LZBRSF,
     {$endif}
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
//
// Every number in the format is little endian, whatever the machine which reads
// it. The tool which writes it runs on a desktop and would otherwise have to
// know the byte order of the target, and a table written on one machine would
// not be readable on another. A reader on a big endian machine turns the block
// around once after loading it, see SwapLoadedBlock, and everything behind that
// point works on native numbers.

const pvSymbolTableMagic:array[0..7] of AnsiChar=('P','V','S','Y','M','T','A','B');

      pvSymbolTableVersion=TpvUInt32(2);

      // Everything behind the header is compressed. The counts in the header
      // still describe the contents as they are once unpacked, so they say how
      // much room the unpacking needs.
      //
      // The flag is known whether or not this was built with the unpacking
      // side, so that a build without it turns such a table down instead of
      // reading compressed bytes as if they were entries.
      pvSymbolTableFlagCompressed=TpvUInt32(1) shl 0;

      // How far an address may sit behind a symbol before that symbol is no
      // longer believed to name it. Only used when no unit range covers the
      // address, since a unit range is the better bound where it exists.
      pvSymbolTableMaximalSymbolDistance=TpvUInt64(65536);

      // How far back from the end of the file the footer is looked for. It only
      // has to cover whatever a signing tool or an installer appended behind the
      // table, which is a few kilobytes of certificate at most, so this is
      // generous rather than tight.
      pvSymbolTableFooterScanSize=TpvInt64(1) shl 20;

      // How much of that window is read at a time by the block wise scan. Only
      // reached when the footer is not where it normally is, and kept small so
      // that even then nothing large has to be allocated. Not used when
      // PasVulkanSymbolTableFooterWholeWindowScan reads the window in one go.
      pvSymbolTableFooterBlockSize=TpvInt64(64) shl 10;

type PpvSymbolTableHeader=^TpvSymbolTableHeader;
     TpvSymbolTableHeader=packed record
      Magic:array[0..7] of AnsiChar;
      Version:TpvUInt32;
      Flags:TpvUInt32;
      // Link time base of the image the table was built from. On Windows the
      // loader reports the actual base directly, so this is informational
      // there, but on Linux it is what turns the load bias into a base.
      ImageBase:TpvUInt64;
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
       // Locates the footer, which is normally at the very end of the file but
       // is not guaranteed to be, see the implementation.
       function FindFooter(const aStream:TStream;out aFooter:TpvSymbolTableFooter;out aFooterPosition:TpvInt64):Boolean;
       // Turns the loaded block from the little endian of the format into the
       // byte order of this machine. Does nothing where the two agree, which is
       // every desktop and every phone in use.
       procedure SwapLoadedBlock;
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
       function ImageBase:TpvUInt64;
     end;

implementation

// Whether this machine reads numbers in the other order than the format stores
// them in. A compile time answer, so the swapping below costs nothing at all
// where it is not needed, which is everywhere PasVulkan currently runs.
{$if defined(ENDIAN_BIG) or defined(FPC_BIG_ENDIAN) or defined(BIG_ENDIAN)}
 {$define PasVulkanSymbolTableSwap}
{$ifend}

// Turns one number from the little endian of the format into the order of this
// machine, and back, which is the same operation. Plain pass through where the
// two already agree, so nothing is paid for this anywhere it is not needed.
function FixUInt32(const aValue:TpvUInt32):TpvUInt32;
begin
{$ifdef PasVulkanSymbolTableSwap}
 result:=((aValue and TpvUInt32($000000ff)) shl 24) or
         ((aValue and TpvUInt32($0000ff00)) shl 8) or
         ((aValue and TpvUInt32($00ff0000)) shr 8) or
         ((aValue and TpvUInt32($ff000000)) shr 24);
{$else}
 result:=aValue;
{$endif}
end;

function FixUInt64(const aValue:TpvUInt64):TpvUInt64;
begin
{$ifdef PasVulkanSymbolTableSwap}
 result:=(TpvUInt64(FixUInt32(TpvUInt32(aValue and TpvUInt64($ffffffff)))) shl 32) or
         TpvUInt64(FixUInt32(TpvUInt32(aValue shr 32)));
{$else}
 result:=aValue;
{$endif}
end;

// Applied immediately after either record is read out of the file, so that
// everything which looks at them afterwards works on native numbers and does
// not have to know that the format has an order of its own.
procedure FixFooterRecord(var aFooter:TpvSymbolTableFooter);
begin
 aFooter.Offset:=FixUInt64(aFooter.Offset);
end;

procedure FixHeaderRecord(var aHeader:TpvSymbolTableHeader);
begin
 aHeader.Version:=FixUInt32(aHeader.Version);
 aHeader.Flags:=FixUInt32(aHeader.Flags);
 aHeader.ImageBase:=FixUInt64(aHeader.ImageBase);
 aHeader.UnitCount:=FixUInt32(aHeader.UnitCount);
 aHeader.SymbolCount:=FixUInt32(aHeader.SymbolCount);
 aHeader.LineCount:=FixUInt32(aHeader.LineCount);
 aHeader.StringSize:=FixUInt32(aHeader.StringSize);
end;

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

function TpvSymbolTable.ImageBase:TpvUInt64;
begin
 if assigned(fHeader) then begin
  result:=fHeader^.ImageBase;
 end else begin
  result:=0;
 end;
end;

// Code signing appends a certificate table behind the end of the image, and an
// installer or an archiver may append a trailer of its own, so the footer is
// only the last thing in the file for as long as nobody touches the file after
// the table was written. Reading a fixed position would therefore find nothing
// at all on a signed executable, and would do so without a word.
//
// The search walks backwards from the end instead. The magic on its own is not
// taken as proof, since the same eight bytes can occur inside appended data by
// chance, so a candidate only counts once its offset leads to a readable header.
function TpvSymbolTable.FindFooter(const aStream:TStream;out aFooter:TpvSymbolTableFooter;out aFooterPosition:TpvInt64):Boolean;
var Buffer:TpvRawByteString;
    BlockSize,BlockStart,Limit,CandidatePosition:TpvInt64;
    Position:TpvSizeInt;

 // Checks whether a footer sits at the given position. The magic alone is not
 // taken as proof, since the same eight bytes can occur inside appended data by
 // chance, so it only counts once its offset leads to a readable header.
 function Accept(const aPosition:TpvInt64):Boolean;
 var Candidate:TpvSymbolTableFooter;
     Header:TpvSymbolTableHeader;
     Index:TpvSizeInt;
 begin

  result:=false;

  if (aPosition<0) or ((aPosition+TpvInt64(SizeOf(TpvSymbolTableFooter)))>aStream.Size) then begin
   exit;
  end;

  aStream.Seek(aPosition,soBeginning);
  aStream.ReadBuffer(Candidate,SizeOf(TpvSymbolTableFooter));

  for Index:=0 to 7 do begin
   if Candidate.Magic[Index]<>pvSymbolTableMagic[Index] then begin
    exit;
   end;
  end;

  FixFooterRecord(Candidate);

  if (Candidate.Offset=0) or
     (Candidate.Offset>TpvUInt64(aPosition)) or
     ((TpvUInt64(aPosition)-Candidate.Offset)<TpvUInt64(SizeOf(TpvSymbolTableHeader))) then begin
   exit;
  end;

  aStream.Seek(TpvInt64(Candidate.Offset),soBeginning);
  aStream.ReadBuffer(Header,SizeOf(TpvSymbolTableHeader));
  FixHeaderRecord(Header);

  for Index:=0 to 7 do begin
   if Header.Magic[Index]<>pvSymbolTableMagic[Index] then begin
    exit;
   end;
  end;

  if Header.Version<>pvSymbolTableVersion then begin
   exit;
  end;

  aFooter:=Candidate;
  aFooterPosition:=aPosition;
  result:=true;

 end;

begin

 result:=false;
 FillChar(aFooter,SizeOf(TpvSymbolTableFooter),#0);
 aFooterPosition:=0;

 if aStream.Size<TpvInt64(SizeOf(TpvSymbolTableFooter)) then begin
  exit;
 end;

 // The ordinary case is that the footer is the last thing in the file, which
 // costs one seek and sixteen bytes to establish. This is read while a report
 // is being written, which can be after a heap corruption, so an allocation
 // which is not needed is one which cannot fail.
 if Accept(aStream.Size-TpvInt64(SizeOf(TpvSymbolTableFooter))) then begin
  result:=true;
  exit;
 end;

 // It is not, so something was appended behind it. Code signing does exactly
 // that with its certificate table, and an installer may add a trailer of its
 // own, so the window behind the footer is searched backwards.
 Limit:=aStream.Size-pvSymbolTableFooterScanSize;
 if Limit<0 then begin
  Limit:=0;
 end;

 Buffer:='';

{$ifdef PasVulkanSymbolTableFooterWholeWindowScan}

 // The whole window in one piece, which is how this was first written. Kept
 // because it is the plainer of the two, but it asks for a megabyte in one go,
 // and this runs while a crash is being written up, where a damaged heap makes
 // exactly that request the one which fails.
 BlockSize:=aStream.Size-Limit;
 SetLength(Buffer,BlockSize);
 aStream.Seek(Limit,soBeginning);
 aStream.ReadBuffer(Buffer[1],BlockSize);

 for Position:=TpvSizeInt(BlockSize)-TpvSizeInt(SizeOf(TpvSymbolTableFooter)) downto 0 do begin
  if Buffer[Position+1]=pvSymbolTableMagic[0] then begin
   CandidatePosition:=Limit+Position;
   if Accept(CandidatePosition) then begin
    result:=true;
    exit;
   end;
  end;
 end;

{$else}

 // The same window over the same bytes, read backwards in blocks, so that
 // nothing large has to be had at once. Same answer, smaller footprint, which
 // is why it is the one which is on by default.
 BlockStart:=aStream.Size;

 while BlockStart>Limit do begin

  BlockSize:=pvSymbolTableFooterBlockSize;
  if (BlockStart-BlockSize)<Limit then begin
   BlockSize:=BlockStart-Limit;
  end;
  dec(BlockStart,BlockSize);

  // Overlapping the block behind it, so that a footer straddling the boundary
  // is not missed.
  if (BlockStart+BlockSize+TpvInt64(SizeOf(TpvSymbolTableFooter)))<=aStream.Size then begin
   inc(BlockSize,TpvInt64(SizeOf(TpvSymbolTableFooter))-1);
  end;

  if TpvInt64(length(Buffer))<>BlockSize then begin
   SetLength(Buffer,BlockSize);
  end;
  aStream.Seek(BlockStart,soBeginning);
  aStream.ReadBuffer(Buffer[1],BlockSize);

  for Position:=TpvSizeInt(BlockSize)-TpvSizeInt(SizeOf(TpvSymbolTableFooter)) downto 0 do begin
   if Buffer[Position+1]=pvSymbolTableMagic[0] then begin
    CandidatePosition:=BlockStart+Position;
    if Accept(CandidatePosition) then begin
     result:=true;
     exit;
    end;
   end;
  end;

 end;

{$endif}

end;

procedure TpvSymbolTable.SwapLoadedBlock;
{$ifdef PasVulkanSymbolTableSwap}
var Index:TpvSizeInt;
    Header:PpvSymbolTableHeader;
    Units:PpvSymbolTableUnitEntry;
    Symbols:PpvSymbolTableSymbolEntry;
    Lines:PpvSymbolTableLineEntry;
    UnitEntry:PpvSymbolTableUnitEntry;
    SymbolEntry:PpvSymbolTableSymbolEntry;
    LineEntry:PpvSymbolTableLineEntry;
begin

 // Only the entries. The header in the block was put there already turned
 // around, by whichever of the two paths in LoadFromFile got here, so doing it
 // again would put it back the way it came.
 //
 // Runs before the caller lays out its pointers, since the counts it needs for
 // that are in that header, so the walk works out its own pointers as it goes.
 // The string block is bytes and stays as it is.
 Header:=PpvSymbolTableHeader(fData);

 Units:=PpvSymbolTableUnitEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fData)+TpvPtrUInt(SizeOf(TpvSymbolTableHeader)))));
 Symbols:=PpvSymbolTableSymbolEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(Units)+(TpvPtrUInt(Header^.UnitCount)*TpvPtrUInt(SizeOf(TpvSymbolTableUnitEntry))))));
 Lines:=PpvSymbolTableLineEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(Symbols)+(TpvPtrUInt(Header^.SymbolCount)*TpvPtrUInt(SizeOf(TpvSymbolTableSymbolEntry))))));

 for Index:=0 to TpvSizeInt(Header^.UnitCount)-1 do begin
  UnitEntry:=PpvSymbolTableUnitEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(Units)+(TpvPtrUInt(Index)*TpvPtrUInt(SizeOf(TpvSymbolTableUnitEntry))))));
  UnitEntry^.StartRVA:=FixUInt64(UnitEntry^.StartRVA);
  UnitEntry^.Size:=FixUInt64(UnitEntry^.Size);
  UnitEntry^.NameOffset:=FixUInt32(UnitEntry^.NameOffset);
  UnitEntry^.FileNameOffset:=FixUInt32(UnitEntry^.FileNameOffset);
 end;

 for Index:=0 to TpvSizeInt(Header^.SymbolCount)-1 do begin
  SymbolEntry:=PpvSymbolTableSymbolEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(Symbols)+(TpvPtrUInt(Index)*TpvPtrUInt(SizeOf(TpvSymbolTableSymbolEntry))))));
  SymbolEntry^.RVA:=FixUInt64(SymbolEntry^.RVA);
  SymbolEntry^.NameOffset:=FixUInt32(SymbolEntry^.NameOffset);
 end;

 for Index:=0 to TpvSizeInt(Header^.LineCount)-1 do begin
  LineEntry:=PpvSymbolTableLineEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(Lines)+(TpvPtrUInt(Index)*TpvPtrUInt(SizeOf(TpvSymbolTableLineEntry))))));
  LineEntry^.RVA:=FixUInt64(LineEntry^.RVA);
  LineEntry^.LineNumber:=FixUInt32(LineEntry^.LineNumber);
  LineEntry^.UnitIndex:=FixUInt32(LineEntry^.UnitIndex);
 end;

end;
{$else}
begin
 // Same order on both sides, so there is nothing to turn around.
end;
{$endif}

function TpvSymbolTable.LoadFromFile(const aFileName:String):Boolean;
var Stream:TFileStream;
    Footer:TpvSymbolTableFooter;
    Header:TpvSymbolTableHeader;
    Expected,Stored:TpvUInt64;
    FooterPosition:TpvInt64;
    Index:TpvSizeInt;
{$ifdef PasVulkanSymbolTableCompression}
    Packed_,Unpacked:TpvPointer;
    UnpackedSize,EmbeddedSize:TpvUInt64;
{$endif}
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

   if not FindFooter(Stream,Footer,FooterPosition) then begin
    exit;
   end;

   Stream.Seek(TpvInt64(Footer.Offset),soBeginning);
   Stream.ReadBuffer(Header,SizeOf(TpvSymbolTableHeader));
   FixHeaderRecord(Header);

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
   // The counts are four unchecked thirty two bit numbers, so what they
   // multiply out to can be larger than this build can address at all. Checked
   // before anything is cast down to a size, since on a thirty two bit runtime
   // that cast would silently wrap and the allocation which follows would then
   // be far too small for what is read into it.
   if (Header.StringSize=0) or
      (Expected>TpvUInt64(High(TpvSizeInt))) then begin
    exit;
   end;

   // Everything between the header and the footer, which is what the contents
   // were stored as, packed or not.
   Stored:=TpvUInt64(FooterPosition-TpvInt64(Footer.Offset))-TpvUInt64(SizeOf(TpvSymbolTableHeader));

   if (Header.Flags and pvSymbolTableFlagCompressed)<>0 then begin

{$ifdef PasVulkanSymbolTableCompression}
    if (Stored<=TpvUInt64(SizeOf(TpvUInt64))) or
       (Stored>TpvUInt64(High(TpvSizeInt))) then begin
     exit;
    end;

    // The counts in the header are four unchecked thirty two bit numbers, and
    // Expected is what they multiply out to, which can be hundreds of gigabytes
    // for a damaged or hostile one. The unpacked size is also written into the
    // first eight bytes of the packed data, so the two are held against each
    // other here, before anything is asked for. The unpacker checks the same
    // thing, but only once the room has already been taken, which in the middle
    // of a crash is exactly too late.
    Stream.Seek(TpvInt64(Footer.Offset)+TpvInt64(SizeOf(TpvSymbolTableHeader)),soBeginning);
    Stream.ReadBuffer(EmbeddedSize,SizeOf(TpvUInt64));
    if EmbeddedSize<>(Expected-TpvUInt64(SizeOf(TpvSymbolTableHeader))) then begin
     exit;
    end;

    fSize:=TpvSizeInt(Expected);
    GetMem(fData,fSize);
    Move(Header,fData^,SizeOf(TpvSymbolTableHeader));
    Packed_:=nil;
    try
     GetMem(Packed_,TpvSizeInt(Stored));
     Stream.Seek(TpvInt64(Footer.Offset)+TpvInt64(SizeOf(TpvSymbolTableHeader)),soBeginning);
     Stream.ReadBuffer(Packed_^,TpvSizeInt(Stored));
     Unpacked:=TpvPointer(TpvPtrUInt(TpvPtrUInt(fData)+TpvPtrUInt(SizeOf(TpvSymbolTableHeader))));
     UnpackedSize:=0;
     // A destination which is already there is written into rather than
     // replaced, so the block stays the one which was just laid out.
     if not LZBRSFDecompress(Packed_,Stored,Unpacked,UnpackedSize,TpvInt64(Expected)-TpvInt64(SizeOf(TpvSymbolTableHeader))) then begin
      FreeMem(fData);
      fData:=nil;
      fSize:=0;
      exit;
     end;
    finally
     if assigned(Packed_) then begin
      FreeMem(Packed_);
     end;
    end;
{$else}
    // Built without the unpacking side, so this is turned down rather than
    // read as if the packed bytes were entries.
    exit;
{$endif}

   end else begin

    // Check that the announced contents actually fit into what is there, so
    // that a truncated or otherwise damaged file cannot send the reader off
    // into arbitrary memory later on. Measured against the footer rather than
    // against the end of the file, since anything appended behind the footer
    // does not belong to the table.
    if (Expected-TpvUInt64(SizeOf(TpvSymbolTableHeader)))>Stored then begin
     exit;
    end;

    fSize:=TpvSizeInt(Expected);
    GetMem(fData,fSize);
    Stream.Seek(TpvInt64(Footer.Offset),soBeginning);
    Stream.ReadBuffer(fData^,fSize);
    // The header of the block as it was read is still in the order of the
    // format. The one read out separately above has already been turned
    // around, so putting that one in its place leaves the whole block with a
    // header which the rest of this can use, exactly as in the packed path.
    Move(Header,fData^,SizeOf(TpvSymbolTableHeader));

   end;

  finally
   FreeAndNil(Stream);
  end;

  // Before anything looks at the entries, and before the pointers below are
  // worked out, since both need native numbers.
  SwapLoadedBlock;

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

 // A unit range is optional. Code compiled without line information, which is
 // common for parts of the runtime library, belongs to no unit here but can
 // still be named by a symbol.
 UnitIndex:=FindUnit(aRVA);
 if UnitIndex>=0 then begin
  UnitEntry:=PpvSymbolTableUnitEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fUnits)+(TpvPtrUInt(UnitIndex)*TpvPtrUInt(SizeOf(TpvSymbolTableUnitEntry))))));
  aLocation.UnitName:=GetString(UnitEntry^.NameOffset);
  aLocation.FileName:=GetString(UnitEntry^.FileNameOffset);
 end else begin
  UnitEntry:=nil;
 end;

 SymbolIndex:=FindSymbol(aRVA);
 if SymbolIndex>=0 then begin
  SymbolEntry:=PpvSymbolTableSymbolEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fSymbols)+(TpvPtrUInt(SymbolIndex)*TpvPtrUInt(SizeOf(TpvSymbolTableSymbolEntry))))));
  if assigned(UnitEntry) then begin
   // Only trust the symbol when it lies inside the same unit, otherwise it is
   // just the last one before a gap and would name the wrong routine.
   if (SymbolEntry^.RVA>=UnitEntry^.StartRVA) and
      (SymbolEntry^.RVA<(UnitEntry^.StartRVA+UnitEntry^.Size)) then begin
    aLocation.SymbolName:=GetString(SymbolEntry^.NameOffset);
    aLocation.SymbolRVA:=SymbolEntry^.RVA;
   end;
  end else if (aRVA-SymbolEntry^.RVA)<pvSymbolTableMaximalSymbolDistance then begin
   // Without a unit range to bound it, a plausibility distance has to do, so
   // that an address in a gap does not borrow the name of something far away.
   aLocation.SymbolName:=GetString(SymbolEntry^.NameOffset);
   aLocation.SymbolRVA:=SymbolEntry^.RVA;
  end;
 end;

 if assigned(UnitEntry) then begin
  LineIndex:=FindLine(aRVA);
  if LineIndex>=0 then begin
   LineEntry:=PpvSymbolTableLineEntry(TpvPointer(TpvPtrUInt(TpvPtrUInt(fLines)+(TpvPtrUInt(LineIndex)*TpvPtrUInt(SizeOf(TpvSymbolTableLineEntry))))));
   // Same reasoning as for the symbol above, a line record only counts when it
   // belongs to the unit the address is in, since not every unit necessarily
   // carries line information.
   if LineEntry^.UnitIndex=TpvUInt32(UnitIndex) then begin
    // A line number of zero is an end of sequence marker, which says that the
    // code described by the records before it has stopped here. Carrying it
    // over is exactly right: it leaves the line unknown for an address in a
    // hole inside a unit, rather than naming the last line before the hole.
    aLocation.LineNumber:=LineEntry^.LineNumber;
   end;
  end;
 end;

 result:=assigned(UnitEntry) or (length(aLocation.SymbolName)>0);

end;

end.
