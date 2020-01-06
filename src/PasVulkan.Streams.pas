(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Streams;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses {$if defined(Windows)}
      Windows,
     {$elseif defined(Unix)}
      BaseUnix,
      Unix,
      UnixType,
     {$ifend}
     SysUtils,
     Classes,
     Math,
     PasVulkan.Types,
     PasVulkan.Math;

const DefaultBufferSize=4096;
      DefaultBufferBucketCount=4096;

type EpvDataStream=class(Exception);

     TpvDataStream=class(TStream)
      private
       fData:TpvPointer;
       fSize:TpvInt64;
       fPosition:TpvInt64;
      public
       constructor Create(const AData:TpvPointer;const ASize:TpvInt64);
       destructor Destroy; override;
       function Read(var Buffer;Count:TpvInt32):TpvInt32; override;
       function Write(const Buffer;Count:TpvInt32):TpvInt32; override;
       function Seek(Offset:TpvInt32;Origin:TpvUInt16):TpvInt32; overload; override;
       function Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64; overload; override;
       procedure SetSize(NewSize:TpvInt32); overload; override;
       procedure SetSize(const NewSize:TpvInt64); overload; override;
     end;

     TpvSimpleBufferedStream=class(TStream)
      private
       fWrappedStream:TStream;
       fWrappedStreamSize:TpvInt64;
       fFreeWrappedStream:boolean;
       fStreamBuffer:PpvRawByteChar;
       fStreamBufferSize:TpvInt64;
       fStreamBufferPosition:TpvInt64;
       fStreamBufferPointer:PpvRawByteChar;
       fStreamBufferEnd:PpvRawByteChar;
       fStreamBufferDirty:boolean;
       procedure ReadBufferFromFile;
      protected
       procedure SetSize(NewSize:TpvInt32); overload; override;
       procedure SetSize(const NewSize:TpvInt64); overload; override;
      public
       constructor Create(Stream:TStream;FreeStream:boolean=false;BufferSize:TpvInt32=DefaultBufferSize);
       destructor Destroy; override;
       function Read(var Buffer;Count:TpvInt32):TpvInt32; override;
       function Write(const Buffer;Count:TpvInt32):TpvInt32; override;
       function Seek(Offset:TpvInt32;Origin:TpvUInt16):TpvInt32; overload; override;
       function Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64; overload; override;
       procedure Flush;
     end;

     PpvBufferedStreamBucket=^TpvBufferedStreamBucket;
     TpvBufferedStreamBucket=record
      Previous,Next:PpvBufferedStreamBucket;
      Buffer:TpvPointer;
      BufferSize,BasePosition:TpvInt64;
      Dirty:boolean;
     end;

     TpvBufferedStreamBuckets=array of TpvBufferedStreamBucket;

     TpvBufferedStreamBucketBitmap=array[0..1048575] of TpvUInt8;

     TpvBufferedStream=class(TStream)
      private
       fWrappedStream:TStream;
       fWrappedStreamSize:TpvInt64;
       fFreeWrappedStream:boolean;
       fBuckets:TpvBufferedStreamBuckets;
       fBucketBitmap:TpvBufferedStreamBucketBitmap;
       fFirstBucket:PpvBufferedStreamBucket;
       fLastBucket:PpvBufferedStreamBucket;
       fStreamBuffer:PpvRawByteChar;
       fStreamPosition:TpvInt64;
       fStreamBufferSize:TpvInt64;
       fStreamBufferSizeMask:TpvInt64;
       fStreamBufferSizeInvMask:TpvInt64;
       fStreamBufferSizeShift:TpvInt32;
       fBucketCount:TpvInt32;
       function GetBucket(fStreamPosition:TpvInt64):PpvBufferedStreamBucket;
      protected
       procedure SetSize(NewSize:TpvInt32); overload; override;
       procedure SetSize(const NewSize:TpvInt64); overload; override;
      public
       constructor Create(Stream:TStream;FreeStream:boolean=false;BufferSize:TpvInt32=DefaultBufferSize;BufferBucketCount:TpvInt32=DefaultBufferBucketCount);
       destructor Destroy; override;
       function Read(var Buffer;Count:TpvInt32):TpvInt32; override;
       function Write(const Buffer;Count:TpvInt32):TpvInt32; override;
       function Seek(Offset:TpvInt32;Origin:TpvUInt16):TpvInt32; overload; override;
       function Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64; overload; override;
       procedure Flush;
     end;

     PpvChunkSignature=^TpvChunkSignature;
     TpvChunkSignature=array[0..3] of ansichar;

     PpvChunk=^TpvChunk;
     TpvChunk=packed record
      Signature:TpvChunkSignature;
      Offset:TpvInt32;
      Size:TpvInt32;
      Reserved:TpvUInt32;
     end;

     TpvChunks=array of TpvChunk;

     EpvChunkStream=class(Exception);

     TpvChunkStream=class(TStream)
      private
       fStream:TStream;
       fOffset:TpvInt64;
       fSize:TpvInt64;
       fPosition:TpvInt64;
       fMemory:boolean;
      public
       constructor Create(const AStream:TStream;const AOffset,ASize:TpvInt64;const AMemory:boolean=true);
       destructor Destroy; override;
       function Read(var Buffer;Count:TpvInt32):TpvInt32; override;
       function Write(const Buffer;Count:TpvInt32):TpvInt32; override;
       function Seek(Offset:TpvInt32;Origin:word):TpvInt32; override;
       function Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64; override;
       procedure SetSize(NewSize:TpvInt32); override;
       procedure SetSize(const NewSize:TpvInt64); override;
       function ReadWithCheck(var Buffer;Count:TpvInt32):TpvInt32;
       function ReadString:TpvUTF8String;
       function ReadUInt8:TpvUInt8;
       function ReadInt32:TpvInt32;
       function ReadUInt32:TpvUInt32;
       function ReadFloat:TpvFloat;
       function ReadVector2:TpvVector2;
       function ReadVector3:TpvVector3;
       function ReadVector4:TpvVector4;
       function ReadPlane:TpvPlane;
       function ReadMatrix4x4:TpvMatrix4x4;
     end;

implementation

type PpvBytes=^TpvBytes;
     TpvBytes=array[0..0] of TpvUInt8;

constructor TpvDataStream.Create(const AData:TpvPointer;const ASize:TpvInt64);
begin
 inherited Create;
 fData:=AData;
 fSize:=ASize;
 fPosition:=0;
end;

destructor TpvDataStream.Destroy;
begin
 inherited Destroy;
end;

function TpvDataStream.Read(var Buffer;Count:TpvInt32):TpvInt32;
begin
 if (fPosition+Count)>fSize then begin
  Count:=fSize-fPosition;
 end;
 if Count>0 then begin
  Move(PpvBytes(fData)^[fPosition],Buffer,Count);
  inc(fPosition,Count);
  result:=Count;
 end else begin
  result:=0;
 end;
end;

function TpvDataStream.Write(const Buffer;Count:TpvInt32):TpvInt32;
begin
 if (fPosition+Count)>fSize then begin
  Count:=fSize-fPosition;
 end;
 if Count>0 then begin
  Move(Buffer,PpvBytes(fData)^[fPosition],Count);
  inc(fPosition,Count);
  result:=Count;
 end else begin
  result:=0;
 end;
end;

function TpvDataStream.Seek(Offset:TpvInt32;Origin:TpvUInt16):TpvInt32;
begin
 case Origin of
  soFromBeginning:begin
   fPosition:=Offset;
  end;
  soFromCurrent:begin
   inc(fPosition,Offset);
  end;
  soFromEnd:begin
   fPosition:=fSize+Offset;
  end;
 end;
 if (fPosition<0) or (fPosition>fSize) then begin
  raise EpvDataStream.Create('Stream seek error');
 end;
 result:=fPosition;
end;

function TpvDataStream.Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64;
begin
 case Origin of
  soBeginning:begin
   fPosition:=Offset;
  end;
  soCurrent:begin
   inc(fPosition,Offset);
  end;
  soEnd:begin
   fPosition:=fSize+Offset;
  end;
 end;
 if (fPosition<0) or (fPosition>fSize) then begin
  raise EpvDataStream.Create('Stream seek error');
 end;
 result:=fPosition;
end;

procedure TpvDataStream.SetSize(NewSize:TpvInt32);
begin
 if fSize<>NewSize then begin
  raise EpvDataStream.Create('Stream set size error');
 end;
end;

procedure TpvDataStream.SetSize(const NewSize:TpvInt64);
begin
 if fSize<>NewSize then begin
  raise EpvDataStream.Create('Stream set size error');
 end;
end;

constructor TpvSimpleBufferedStream.Create(Stream:TStream;FreeStream:boolean=false;BufferSize:TpvInt32=DefaultBufferSize);
begin
 inherited Create;
 fWrappedStream:=Stream;
 fWrappedStreamSize:=fWrappedStream.Size;
 fFreeWrappedStream:=FreeStream;
 fStreamBufferPosition:=0;
 fStreamBufferSize:=BufferSize;
 GetMem(fStreamBuffer,fStreamBufferSize);
 fStreamBufferPointer:=fStreamBuffer;
 fStreamBufferEnd:=fStreamBufferPointer;
 fStreamBufferDirty:=false;
end;

destructor TpvSimpleBufferedStream.Destroy;
begin
 Flush;
 FreeMem(fStreamBuffer,fStreamBufferSize);
 if fFreeWrappedStream then begin
  FreeAndNil(fWrappedStream);
 end;
 inherited Destroy;
end;

procedure TpvSimpleBufferedStream.ReadBufferFromFile;
var BytesRead:TpvInt64;
begin
 if fWrappedStream.Seek(fStreamBufferPosition,soBeginning)<>fStreamBufferPosition then begin
  raise EStreamError.Create('Seek error');
 end;
 BytesRead:=fWrappedStream.Read(fStreamBuffer^,fStreamBufferSize);
 if (BytesRead<0) or (BytesRead>fStreamBufferSize) then begin
  raise EReadError.Create('Read error');
 end;
 fStreamBufferPointer:=fStreamBuffer;
 fStreamBufferEnd:=@fStreamBufferPointer[BytesRead];
end;

function TpvSimpleBufferedStream.Read(var Buffer;Count:TpvInt32):TpvInt32;
var Destination,OldfStreamBufferPointer:PpvRawByteChar;
    BytesToRead,BytesInBuffer:TpvPtrUInt;
begin
 result:=0;
 Destination:=@Buffer;
 if assigned(Destination) then begin
  while Count>0 do begin
   BytesToRead:=TpvPtrUInt(fStreamBufferEnd)-TpvPtrUInt(fStreamBufferPointer);
   if BytesToRead<=0 then begin
    BytesInBuffer:=TpvPtrUInt(fStreamBufferEnd)-TpvPtrUInt(fStreamBuffer);
    OldfStreamBufferPointer:=fStreamBufferPointer;
    Assert(BytesInBuffer<=fStreamBufferSize,'Buffer overflow');
    if BytesInBuffer>=fStreamBufferSize then begin
     inc(fStreamBufferPosition,fStreamBufferSize);
     dec(OldfStreamBufferPointer,fStreamBufferSize);
    end;
    ReadBufferFromFile;
    fStreamBufferPointer:=OldfStreamBufferPointer;
    BytesToRead:=TpvPtrUInt(fStreamBufferEnd)-TpvPtrUInt(fStreamBufferPointer);
   end;
   if TpvPtrInt(BytesToRead)>Count then begin
    BytesToRead:=Count;
   end;
   if BytesToRead>0 then begin
    Move(fStreamBufferPointer^,Destination^,BytesToRead);
    dec(Count,BytesToRead);
    inc(fStreamBufferPointer,BytesToRead);
    inc(Destination,BytesToRead);
    inc(result,BytesToRead);
   end else begin
    break;
   end;
  end;
 end;
end;

function TpvSimpleBufferedStream.Write(const Buffer;Count:TpvInt32):TpvInt32;
var Source:PpvRawByteChar;
    BytesToWrite,CurrentPosition:TpvInt64;
begin
 result:=0;
 Source:=@Buffer;
 if assigned(Source) then begin
  while Count>0 do begin
   BytesToWrite:=fStreamBufferSize-TpvPtrInt(TpvPtrUInt(fStreamBufferPointer)-TpvPtrUInt(fStreamBuffer));
   if BytesToWrite<=0 then begin
    Flush;
    inc(fStreamBufferPosition,fStreamBufferSize);
    ReadBufferFromFile;
    BytesToWrite:=fStreamBufferSize-TpvPtrInt(TpvPtrUInt(fStreamBufferPointer)-TpvPtrUInt(fStreamBuffer));
   end;
   if BytesToWrite>Count then begin
    BytesToWrite:=Count;
   end;
   if BytesToWrite>(TpvPtrUInt(fStreamBufferEnd)-TpvPtrUInt(fStreamBufferPointer)) then begin
    fStreamBufferEnd:=@fStreamBufferPointer[BytesToWrite];
   end;
   if BytesToWrite>0 then begin
    fStreamBufferDirty:=true;
    Move(Source^,fStreamBufferPointer^,BytesToWrite);
    dec(Count,BytesToWrite);
    inc(Source,BytesToWrite);
    inc(fStreamBufferPointer,BytesToWrite);
    inc(result,BytesToWrite);
    CurrentPosition:=fStreamBufferPosition+TpvPtrInt(TpvPtrUInt(fStreamBufferPointer)-TpvPtrUInt(fStreamBuffer));
    if fWrappedStreamSize<CurrentPosition then begin
     fWrappedStreamSize:=CurrentPosition;
    end;
   end;
  end;
 end;
end;

function TpvSimpleBufferedStream.Seek(Offset:TpvInt32;Origin:TpvUInt16):TpvInt32;
var CurrentPosition,Delta:TpvInt64;
begin
 CurrentPosition:=fStreamBufferPosition+TpvPtrInt(TpvPtrUInt(fStreamBufferPointer)-TpvPtrUInt(fStreamBuffer));
 if (Origin=soFromCurrent) and (Offset=0) then begin
  result:=CurrentPosition;
 end else begin
  case Origin of
   soFromBeginning:begin
    result:=Offset;
   end;
   soFromCurrent:begin
    result:=CurrentPosition+Offset;
   end;
   soFromEnd:begin
    result:=fWrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end;
  if (fStreamBufferPosition<=result) and (result<(fStreamBufferPosition+fStreamBufferSize)) then begin
   fStreamBufferPointer:=@fStreamBuffer[result-fStreamBufferPosition];
  end else begin
   Flush;
   Delta:=result mod fStreamBufferSize;
   fStreamBufferPosition:=result-Delta;
   ReadBufferFromFile;
   fStreamBufferPointer:=@fStreamBuffer[Delta];
  end;
  result:=fStreamBufferPosition+TpvPtrInt(TpvPtrUInt(fStreamBufferPointer)-TpvPtrUInt(fStreamBuffer));
  if fWrappedStreamSize<result then begin
   fWrappedStreamSize:=result;
  end;
 end;
end;

function TpvSimpleBufferedStream.Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64;
var CurrentPosition,Delta:TpvInt64;
begin
 CurrentPosition:=fStreamBufferPosition+TpvPtrInt(TpvPtrUInt(fStreamBufferPointer)-TpvPtrUInt(fStreamBuffer));
 if (Origin=soCurrent) and (Offset=0) then begin
  result:=CurrentPosition;
 end else begin
  case Origin of
   soBeginning:begin
    result:=Offset;
   end;
   soCurrent:begin
    result:=CurrentPosition+Offset;
   end;
   soEnd:begin
    result:=fWrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end;
  if (fStreamBufferPosition<=result) and (result<(fStreamBufferPosition+fStreamBufferSize)) then begin
   fStreamBufferPointer:=@fStreamBuffer[result-fStreamBufferPosition];
  end else begin
   Flush;
   Delta:=result mod fStreamBufferSize;
   fStreamBufferPosition:=result-Delta;
   ReadBufferFromFile;
   fStreamBufferPointer:=@fStreamBuffer[Delta];
  end;
  result:=fStreamBufferPosition+TpvPtrInt(TpvPtrUInt(fStreamBufferPointer)-TpvPtrUInt(fStreamBuffer));
  if fWrappedStreamSize<result then begin
   fWrappedStreamSize:=result;
  end;
 end;
end;

procedure TpvSimpleBufferedStream.Flush;
var BytesToWrite:TpvInt64;
begin
 if fStreamBufferDirty then begin
  fStreamBufferDirty:=false;
  if fWrappedStream.Seek(fStreamBufferPosition,soBeginning)<>fStreamBufferPosition then begin
   raise EStreamError.Create('Seek error');
  end;
  BytesToWrite:=TpvPtrUInt(fStreamBufferEnd)-TpvPtrUInt(fStreamBuffer);
  if fWrappedStream.Write(fStreamBuffer^,BytesToWrite)<>BytesToWrite then begin
   raise EWriteError.Create('Write error');
  end;
  fWrappedStreamSize:=fWrappedStream.Size;
  if fWrappedStream is TFileStream then begin
{$ifdef Windows}
  Windows.FlushFileBuffers(TFileStream(fWrappedStream).Handle);
{$else}
  fpfsync(TFileStream(fWrappedStream).Handle);
{$endif}
  end;
 end;
end;

procedure TpvSimpleBufferedStream.SetSize(NewSize:TpvInt32);
begin
 if NewSize<0 then begin
  NewSize:=0;
 end;
 if NewSize>fStreamBufferPosition then begin
  Flush;
 end;
 fWrappedStream.Size:=NewSize;
 if fWrappedStream.Seek(NewSize,soBeginning)<>NewSize then begin
  raise EStreamError.Create('Seek error');
 end;
 fWrappedStreamSize:=NewSize;
 if Position>NewSize then begin
  Position:=NewSize;
 end;
end;

procedure TpvSimpleBufferedStream.SetSize(const NewSize:TpvInt64);
var NewSizeEx:TpvInt64;
begin
 NewSizeEx:=NewSize;
 if NewSizeEx<0 then begin
  NewSizeEx:=0;
 end;
 if NewSizeEx>fStreamBufferPosition then begin
  Flush;
 end;
 fWrappedStream.Size:=NewSizeEx;
 if fWrappedStream.Seek(NewSizeEx,soBeginning)<>NewSizeEx then begin
  raise EStreamError.Create('Seek error');
 end;
 fWrappedStreamSize:=NewSize;
 if Position>NewSizeEx then begin
  Position:=NewSizeEx;
 end;
end;

constructor TpvBufferedStream.Create(Stream:TStream;FreeStream:boolean=false;BufferSize:TpvInt32=DefaultBufferSize;BufferBucketCount:TpvInt32=DefaultBufferBucketCount);
var i:TpvInt32;
begin
 inherited Create;
 fBuckets:=nil;
 fWrappedStream:=Stream;
 fWrappedStreamSize:=fWrappedStream.Size;
 fFreeWrappedStream:=FreeStream;
 fBucketCount:=BufferBucketCount;
 begin
  dec(BufferSize);
  BufferSize:=BufferSize or (BufferSize shr 1);
  BufferSize:=BufferSize or (BufferSize shr 2);
  BufferSize:=BufferSize or (BufferSize shr 4);
  BufferSize:=BufferSize or (BufferSize shr 8);
  BufferSize:=BufferSize or (BufferSize shr 16);
 {$ifdef cpu64}
  BufferSize:=BufferSize or (BufferSize shr 32);
 {$endif}
  fStreamBufferSize:=BufferSize+1;
 end;
 fStreamBufferSizeMask:=fStreamBufferSize-1;
 fStreamBufferSizeInvMask:=not fStreamBufferSizeMask;
 fStreamBufferSizeShift:=1;
 while (TpvInt64(1) shl fStreamBufferSizeShift)<fStreamBufferSize do begin
  inc(fStreamBufferSizeShift);
 end;
 GetMem(fStreamBuffer,fStreamBufferSize*fBucketCount);
 SetLength(fBuckets,fBucketCount);
 for i:=0 to fBucketCount-1 do begin
  FillChar(fBuckets[i],sizeof(TpvBufferedStreamBucket),#0);
  fBuckets[i].Previous:=nil;
  fBuckets[i].Next:=nil;
  fBuckets[i].BasePosition:=-1;
  fBuckets[i].Dirty:=false;
  fBuckets[i].Buffer:=@fStreamBuffer[i*fStreamBufferSize];
 end;
 for i:=1 to fBucketCount-1 do begin
  fBuckets[i].Previous:=@fBuckets[i-1];
 end;
 for i:=0 to fBucketCount-2 do begin
  fBuckets[i].Next:=@fBuckets[i+1];
 end;
 fFirstBucket:=@fBuckets[0];
 fLastBucket:=@fBuckets[fBucketCount-1];
 fStreamPosition:=0;
 FillChar(fBucketBitmap,sizeof(TpvBufferedStreamBucketBitmap),#0);
end;

destructor TpvBufferedStream.Destroy;
begin
 Flush;
 FreeMem(fStreamBuffer);
 SetLength(fBuckets,0);
 if fFreeWrappedStream then begin
  FreeAndNil(fWrappedStream);
 end;
 inherited Destroy;
end;

function TpvBufferedStream.GetBucket(fStreamPosition:TpvInt64):PpvBufferedStreamBucket;
var MaskedStreamPosition,BucketBitmapPosition:TpvInt64;
begin
 MaskedStreamPosition:=fStreamPosition and fStreamBufferSizeInvMask;
 BucketBitmapPosition:=MaskedStreamPosition shr fStreamBufferSizeShift;
 if ((BucketBitmapPosition shr 3)>=length(fBucketBitmap)) or ((fBucketBitmap[BucketBitmapPosition shr 3] and (1 shl (BucketBitmapPosition and 7)))<>0) then begin
  result:=fFirstBucket;
  while assigned(result) and (result^.BasePosition<>MaskedStreamPosition) do begin
   result:=result^.Next;
  end;
 end else begin
  result:=nil;
 end;
 if not assigned(result) then begin
  result:=fLastBucket;
  if result^.Dirty then begin
   if fWrappedStream.Seek(result^.BasePosition,soBeginning)<>result^.BasePosition then begin
    raise EStreamError.Create('Seek error');
   end;
   if fWrappedStream.Write(result^.Buffer^,result^.BufferSize)<>result^.BufferSize then begin
    raise EWriteError.Create('Write error');
   end;
   result^.Dirty:=false;
  end;
  if result^.BasePosition>=0 then begin
   BucketBitmapPosition:=result^.BasePosition shr fStreamBufferSizeShift;
   if (BucketBitmapPosition shr 3)<length(fBucketBitmap) then begin
    fBucketBitmap[BucketBitmapPosition shr 3]:=fBucketBitmap[BucketBitmapPosition shr 3] and not (1 shl (BucketBitmapPosition and 7));
   end;
  end;
  BucketBitmapPosition:=MaskedStreamPosition shr fStreamBufferSizeShift;
  if (BucketBitmapPosition shr 3)<length(fBucketBitmap) then begin
   fBucketBitmap[BucketBitmapPosition shr 3]:=fBucketBitmap[BucketBitmapPosition shr 3] or (1 shl (BucketBitmapPosition and 7));
  end;
  result^.BasePosition:=MaskedStreamPosition;
  if fWrappedStream.Seek(result^.BasePosition,soBeginning)<>result^.BasePosition then begin
   raise EStreamError.Create('Seek error');
  end;
  result^.BufferSize:=fWrappedStream.Read(result^.Buffer^,fStreamBufferSize);
  if (result^.BufferSize<0) or (result^.BufferSize>fStreamBufferSize) then begin
   raise EReadError.Create('Read error');
  end;
 end;
 if assigned(result) and (fFirstBucket<>result) then begin
  if assigned(result^.Previous) then begin
   result^.Previous^.Next:=result^.Next;
  end;
  if assigned(result^.Next) then begin
   result^.Next^.Previous:=result^.Previous;
  end else if fLastBucket=result then begin
   fLastBucket:=result^.Previous;
  end;
  fFirstBucket^.Previous:=result;
  result^.Next:=fFirstBucket;
  result^.Previous:=nil;
  fFirstBucket:=result;
 end;
end;

function TpvBufferedStream.Read(var Buffer;Count:TpvInt32):TpvInt32;
var Bucket:PpvBufferedStreamBucket;
    Destination:PpvRawByteChar;
    BufferPosition,BytesToRead:TpvInt64;
begin
 result:=0;
 Destination:=@Buffer;
 if assigned(Destination) then begin
  while Count>0 do begin
   Bucket:=GetBucket(fStreamPosition);
   if assigned(Bucket) then begin
    BufferPosition:=fStreamPosition-Bucket^.BasePosition;
    BytesToRead:=Bucket^.BufferSize-BufferPosition;
    if BytesToRead>Count then begin
     BytesToRead:=Count;
    end;
    if BytesToRead>0 then begin
     Move(PpvRawByteChar(Bucket^.Buffer)[BufferPosition],Destination^,BytesToRead);
     inc(fStreamPosition,BytesToRead);
     inc(Destination,BytesToRead);
     inc(result,BytesToRead);
     dec(Count,BytesToRead);
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TpvBufferedStream.Write(const Buffer;Count:TpvInt32):TpvInt32;
var Bucket:PpvBufferedStreamBucket;
    Source:PpvRawByteChar;
    BufferPosition,BytesToWrite:TpvInt64;
    DoNeedResize:boolean;
begin
 result:=0;
 Source:=@Buffer;
 if assigned(Source) then begin
  while Count>0 do begin
   Bucket:=GetBucket(fStreamPosition);
   if assigned(Bucket) then begin
    DoNeedResize:=false;
    BufferPosition:=fStreamPosition-Bucket^.BasePosition;
    BytesToWrite:=Bucket^.BufferSize-BufferPosition;
    if BytesToWrite<=0 then begin
     BytesToWrite:=fStreamBufferSize-BufferPosition;
     DoNeedResize:=true;
    end;
    if BytesToWrite>Count then begin
     BytesToWrite:=Count;
    end;
    if BytesToWrite>0 then begin
     Bucket^.Dirty:=true;
     Move(Source^,PpvRawByteChar(Bucket^.Buffer)[BufferPosition],BytesToWrite);
     inc(fStreamPosition,BytesToWrite);
     inc(Source,BytesToWrite);
     inc(result,BytesToWrite);
     dec(Count,BytesToWrite);
     if fWrappedStreamSize<fStreamPosition then begin
      fWrappedStreamSize:=fStreamPosition;
     end;
     if DoNeedResize and (Bucket^.BufferSize<(BufferPosition+BytesToWrite)) then begin
      Bucket^.BufferSize:=BufferPosition+BytesToWrite;
     end;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TpvBufferedStream.Seek(Offset:TpvInt32;Origin:TpvUInt16):TpvInt32;
begin
 if (Origin=soFromCurrent) and (Offset=0) then begin
  result:=fStreamPosition;
 end else begin
  case Origin of
   soFromBeginning:begin
    result:=Offset;
   end;
   soFromCurrent:begin
    result:=fStreamPosition+Offset;
   end;
   soFromEnd:begin
    result:=fWrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end else if result>fWrappedStreamSize then begin
   result:=fWrappedStreamSize;
  end;
  fStreamPosition:=result;
 end;
end;

function TpvBufferedStream.Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64;
begin
 if (Origin=soCurrent) and (Offset=0) then begin
  result:=fStreamPosition;
 end else begin
  case Origin of
   soBeginning:begin
    result:=Offset;
   end;
   soCurrent:begin
    result:=fStreamPosition+Offset;
   end;
   soEnd:begin
    result:=fWrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end else if result>fWrappedStreamSize then begin
   result:=fWrappedStreamSize;
  end;
  fStreamPosition:=result;
 end;
end;

procedure TpvBufferedStream.Flush;
var i:TpvInt32;
    Dirty:boolean;
begin
 Dirty:=false;
 for i:=0 to fBucketCount-1 do begin
  if fBuckets[i].Dirty then begin
   if fWrappedStream.Seek(fBuckets[i].BasePosition,soBeginning)<>fBuckets[i].BasePosition then begin
    raise EStreamError.Create('Seek error');
   end;
   if fWrappedStream.Write(fBuckets[i].Buffer^,fBuckets[i].BufferSize)<>fBuckets[i].BufferSize then begin
    raise EWriteError.Create('Write error');
   end;
   Dirty:=true;
   fBuckets[i].Dirty:=false;
  end;
 end;
 if Dirty and (fWrappedStream is TFileStream) then begin
{$ifdef windows}
  Windows.FlushFileBuffers(TFileStream(fWrappedStream).Handle);
{$else}
  fpfsync(TFileStream(fWrappedStream).Handle);
{$endif}
 end;
end;

procedure TpvBufferedStream.SetSize(NewSize:TpvInt32);
begin
 if NewSize<0 then begin
  NewSize:=0;
 end;
 if NewSize>fStreamPosition then begin
  Flush;
 end;
 fWrappedStream.Size:=NewSize;
 if fWrappedStream.Seek(NewSize,soBeginning)<>NewSize then begin
  raise EStreamError.Create('Seek error');
 end;
 fWrappedStreamSize:=NewSize;
 if Position>NewSize then begin
  Position:=NewSize;
 end;
end;

procedure TpvBufferedStream.SetSize(const NewSize:TpvInt64);
var NewSizeEx:TpvInt64;
begin
 NewSizeEx:=NewSize;
 if NewSizeEx<0 then begin
  NewSizeEx:=0;
 end;
 if NewSizeEx>fStreamPosition then begin
  Flush;
 end;
 fWrappedStream.Size:=NewSizeEx;
 if fWrappedStream.Seek(NewSizeEx,soBeginning)<>NewSizeEx then begin
  raise EStreamError.Create('Seek error');
 end;
 fWrappedStreamSize:=NewSize;
 if Position>NewSizeEx then begin
  Position:=NewSizeEx;
 end;
end;

constructor TpvChunkStream.Create(const AStream:TStream;const AOffset,ASize:TpvInt64;const AMemory:boolean=true);
begin
 inherited Create;
 if (not assigned(AStream)) or ((AOffset<0) or ((AOffset+ASize)>AStream.Size)) then begin
  raise EpvChunkStream.Create('Stream slice error');
 end;
 fPosition:=0;
 fMemory:=AMemory;
 if fMemory then begin
  fStream:=TMemoryStream.Create;
  fOffset:=0;
  fSize:=ASize;
  if AStream.Seek(AOffset,soBeginning)<>AOffset then begin
   raise EpvChunkStream.Create('Stream seek error');
  end;
  if fStream.CopyFrom(AStream,ASize)<>ASize then begin
   raise EpvChunkStream.Create('Stream copy error');
  end;
  if fStream.Seek(0,soBeginning)<>fOffset then begin
   raise EpvChunkStream.Create('Stream seek error');
  end;
 end else begin
  fStream:=AStream;
  fOffset:=AOffset;
  fSize:=ASize;
 end;
end;

destructor TpvChunkStream.Destroy;
begin
 if fMemory then begin
  fStream.Free;
 end;
 inherited Destroy;
end;

function TpvChunkStream.Read(var Buffer;Count:TpvInt32):TpvInt32;
begin
 if (fPosition+Count)>fSize then begin
  Count:=fSize-fPosition;
 end;
 if Count>0 then begin
  if fStream.Position<>(fOffset+fPosition) then begin
   if fStream.Seek(fOffset+fPosition,soBeginning)<>(fOffset+fPosition) then begin
    raise EpvChunkStream.Create('Stream seek error');
   end;
  end;
  result:=fStream.Read(Buffer,Count);
  inc(fPosition,result);
 end else begin
  result:=0;
 end;
end;

function TpvChunkStream.Write(const Buffer;Count:TpvInt32):TpvInt32;
begin
 if (fPosition+Count)>fSize then begin
  Count:=fSize-fPosition;
 end;
 if Count>0 then begin
  if fStream.Position<>(fOffset+fPosition) then begin
   if fStream.Seek(fOffset+fPosition,soBeginning)<>(fOffset+fPosition) then begin
    raise EpvChunkStream.Create('Stream seek error');
   end;
  end;
  result:=fStream.Write(Buffer,Count);
  inc(fPosition,result);
 end else begin
  result:=0;
 end;
end;

function TpvChunkStream.Seek(Offset:TpvInt32;Origin:word):TpvInt32;
begin
 case Origin of
  soFromBeginning:begin
   fPosition:=Offset;
  end;
  soFromCurrent:begin
   inc(fPosition,Offset);
  end;
  soFromEnd:begin
   fPosition:=fSize+Offset;
  end;
 end;
 if (fPosition<0) or (fPosition>fSize) then begin
  raise EpvChunkStream.Create('Stream seek error');
 end;
 result:=fPosition;
end;

function TpvChunkStream.Seek(const Offset:TpvInt64;Origin:TSeekOrigin):TpvInt64;
begin
 case Origin of
  soBeginning:begin
   fPosition:=Offset;
  end;
  soCurrent:begin
   inc(fPosition,Offset);
  end;
  soEnd:begin
   fPosition:=fSize+Offset;
  end;
 end;
 if (fPosition<0) or (fPosition>fSize) then begin
  raise EpvChunkStream.Create('Stream seek error');
 end;
 result:=fPosition;
end;

procedure TpvChunkStream.SetSize(NewSize:TpvInt32);
begin
 if fSize<>NewSize then begin
  raise EpvChunkStream.Create('Stream set size error');
 end;
end;

procedure TpvChunkStream.SetSize(const NewSize:TpvInt64);
begin
 if fSize<>NewSize then begin
  raise EpvChunkStream.Create('Stream set size error');
 end;
end;

function TpvChunkStream.ReadWithCheck(var Buffer;Count:TpvInt32):TpvInt32;
begin
 result:=Read(Buffer,Count);
 if result<>Count then begin
  raise EpvChunkStream.Create('Stream read error');
 end;
end;

function TpvChunkStream.ReadString:TpvUTF8String;
var Len:TpvInt32;
begin
 ReadWithCheck(Len,SizeOf(TpvInt32));
 SetLength(result,Len);
 if Len>0 then begin
  ReadWithCheck(result[1],Len*SizeOf(AnsiChar));
 end;
end;

function TpvChunkStream.ReadUInt8:TpvUInt8;
begin
 ReadWithCheck(result,SizeOf(TpvUInt8));
end;

function TpvChunkStream.ReadInt32:TpvInt32;
begin
 ReadWithCheck(result,SizeOf(TpvInt32));
end;

function TpvChunkStream.ReadUInt32:TpvUInt32;
begin
 ReadWithCheck(result,SizeOf(TpvUInt32));
end;

function TpvChunkStream.ReadFloat:TpvFloat;
begin
 ReadWithCheck(result,SizeOf(TpvFloat));
end;

function TpvChunkStream.ReadVector2:TpvVector2;
begin
 ReadWithCheck(result.x,SizeOf(TpvFloat));
 ReadWithCheck(result.y,SizeOf(TpvFloat));
end;

function TpvChunkStream.ReadVector3:TpvVector3;
begin
 ReadWithCheck(result.x,SizeOf(TpvFloat));
 ReadWithCheck(result.y,SizeOf(TpvFloat));
 ReadWithCheck(result.z,SizeOf(TpvFloat));
end;

function TpvChunkStream.ReadVector4:TpvVector4;
begin
 ReadWithCheck(result.x,SizeOf(TpvFloat));
 ReadWithCheck(result.y,SizeOf(TpvFloat));
 ReadWithCheck(result.z,SizeOf(TpvFloat));
 ReadWithCheck(result.w,SizeOf(TpvFloat));
end;

function TpvChunkStream.ReadPlane:TpvPlane;
begin
 ReadWithCheck(result.RawComponents[0],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[1],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[2],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[3],SizeOf(TpvFloat));
end;

function TpvChunkStream.ReadMatrix4x4:TpvMatrix4x4;
begin
 ReadWithCheck(result.RawComponents[0,0],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[0,1],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[0,2],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[0,3],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[1,0],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[1,1],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[1,2],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[1,3],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[2,0],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[2,1],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[2,2],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[2,3],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[3,0],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[3,1],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[3,2],SizeOf(TpvFloat));
 ReadWithCheck(result.RawComponents[3,3],SizeOf(TpvFloat));
end;

end.
