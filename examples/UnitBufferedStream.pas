unit UnitBufferedStream; // Copyright (C) 2006-2017, Benjamin Rosseaux - License: zlib
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {-$pic off}
 {$define caninline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi} 
 {$undef HasSAR}
 {$define UseDIV}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef cpu386}
 {$define cpux86}
{$endif}
{$ifdef cpuamd64}
 {$define cpux86}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$ifdef windows}
 {$define win}
{$endif}
{$ifdef sdl20}
 {$define sdl}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$ifdef fpc}
 {$define caninline}
{$else}
 {$undef caninline}
 {$ifdef ver180}
  {$define caninline}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define caninline}
   {$ifend}
  {$endif}
 {$endif}
{$endif}

interface

uses {$ifdef unix}BaseUnix,Unix,UnixType,UnixUtil,{$else}Windows,{$endif}Classes,SysUtils;

const DefaultBufferSize=4096;
      DefaultBufferBucketCount=4096;
      
type TSimpleBufferedStream=class(TStream)
      private
       WrappedStream:TStream;
       WrappedStreamSize:int64;
       FreeWrappedStream:boolean;
       StreamBuffer:pansichar;
       StreamBufferSize:int64;
       StreamBufferPosition:int64;
       StreamBufferPointer:pansichar;
       StreamBufferEnd:pansichar;
       StreamBufferDirty:boolean;
       procedure ReadBufferFromFile;
      protected
       procedure SetSize(NewSize:longint); overload; override;
       procedure SetSize(const NewSize:int64); overload; override;
      public
       constructor Create(Stream:TStream;FreeStream:boolean=false;BufferSize:integer=DefaultBufferSize);
       destructor Destroy; override;
       function Read(var Buffer;Count:longint):longint; override;
       function Write(const Buffer;Count:longint):longint; override;
       function Seek(Offset:longint;Origin:word):longint; overload; override;
       function Seek(const Offset:int64;Origin:TSeekOrigin):int64; overload; override;
       procedure Flush;
     end;

     PBufferedStreamBucket=^TBufferedStreamBucket;
     TBufferedStreamBucket=record
      Previous,Next:PBufferedStreamBucket;
      Buffer:pointer;
      BufferSize,BasePosition:int64;
      Dirty:boolean;
     end;

     TBufferedStreamBuckets=array of TBufferedStreamBucket;

     TBufferedStreamBucketBitmap=array[0..1048575] of byte;

     TBufferedStream=class(TStream)
      private
       WrappedStream:TStream;
       WrappedStreamSize:int64;
       FreeWrappedStream:boolean;
       Buckets:TBufferedStreamBuckets;
       BucketBitmap:TBufferedStreamBucketBitmap;
       FirstBucket,LastBucket:PBufferedStreamBucket;
       StreamBuffer:pansichar;
       StreamPosition:int64;
       StreamBufferSize:int64;
       StreamBufferSizeMask:int64;
       StreamBufferSizeInvMask:int64;
       StreamBufferSizeShift:integer;
       BucketCount:integer;
       function GetBucket(StreamPosition:int64):PBufferedStreamBucket;
      protected
       procedure SetSize(NewSize:longint); overload; override;
       procedure SetSize(const NewSize:int64); overload; override;
      public
       constructor Create(Stream:TStream;FreeStream:boolean=false;BufferSize:integer=DefaultBufferSize;BufferBucketCount:integer=DefaultBufferBucketCount);
       destructor Destroy; override;
       function Read(var Buffer;Count:longint):longint; override;
       function Write(const Buffer;Count:longint):longint; override;
       function Seek(Offset:longint;Origin:word):longint; overload; override;
       function Seek(const Offset:int64;Origin:TSeekOrigin):int64; overload; override;
       procedure Flush;
     end;

implementation

{$ifndef fpc}
type PtrInt=longint;
     PtrUInt=longword;
{$endif}

constructor TSimpleBufferedStream.Create(Stream:TStream;FreeStream:boolean=false;BufferSize:integer=DefaultBufferSize);
begin
 inherited Create;
 WrappedStream:=Stream;
 WrappedStreamSize:=WrappedStream.Size;
 FreeWrappedStream:=FreeStream;
 StreamBufferPosition:=0;
 StreamBufferSize:=BufferSize;
 GetMem(StreamBuffer,StreamBufferSize);
 StreamBufferPointer:=StreamBuffer;
 StreamBufferEnd:=StreamBufferPointer;
 StreamBufferDirty:=false;
end;

destructor TSimpleBufferedStream.Destroy;
begin
 Flush;
 FreeMem(StreamBuffer,StreamBufferSize);
 if FreeWrappedStream then begin
  FreeAndNil(WrappedStream);
 end;
 inherited Destroy;
end;

procedure TSimpleBufferedStream.ReadBufferFromFile;
var BytesRead:int64;
begin
 if WrappedStream.Seek(StreamBufferPosition,soBeginning)<>StreamBufferPosition then begin
  raise EStreamError.Create('Seek error');
 end;
 BytesRead:=WrappedStream.Read(StreamBuffer^,StreamBufferSize);
 if (BytesRead<0) or (BytesRead>StreamBufferSize) then begin
  raise EReadError.Create('Read error');
 end;
 StreamBufferPointer:=StreamBuffer;
 StreamBufferEnd:=@StreamBufferPointer[BytesRead];
end;

function TSimpleBufferedStream.Read(var Buffer;Count:longint):longint;
var Destination,OldStreamBufferPointer:pansichar;
    BytesToRead,BytesInBuffer:ptruint;
begin
 result:=0;
 Destination:=@Buffer;
 if assigned(Destination) then begin
  while Count>0 do begin
   BytesToRead:=ptruint(StreamBufferEnd)-ptruint(StreamBufferPointer);
   if BytesToRead<=0 then begin
    BytesInBuffer:=ptruint(StreamBufferEnd)-ptruint(StreamBuffer);
    OldStreamBufferPointer:=StreamBufferPointer;
    Assert(BytesInBuffer<=StreamBufferSize,'Buffer overflow');
    if BytesInBuffer>=StreamBufferSize then begin
     inc(StreamBufferPosition,StreamBufferSize);
     dec(OldStreamBufferPointer,StreamBufferSize);
    end;
    ReadBufferFromFile;
    StreamBufferPointer:=OldStreamBufferPointer;
    BytesToRead:=ptruint(StreamBufferEnd)-ptruint(StreamBufferPointer);
   end;
   if ptrint(BytesToRead)>Count then begin
    BytesToRead:=Count;
   end;
   if BytesToRead>0 then begin
    Move(StreamBufferPointer^,Destination^,BytesToRead);
    dec(Count,BytesToRead);
    inc(StreamBufferPointer,BytesToRead);
    inc(Destination,BytesToRead);
    inc(result,BytesToRead);
   end else begin
    break;
   end;
  end;
 end;
end;

function TSimpleBufferedStream.Write(const Buffer;Count:longint):longint;
var Source:pansichar;
    BytesToWrite,CurrentPosition:int64;
begin
 result:=0;
 Source:=@Buffer;
 if assigned(Source) then begin
  while Count>0 do begin
   BytesToWrite:=StreamBufferSize-(ptruint(StreamBufferPointer)-ptruint(StreamBuffer));
   if BytesToWrite<=0 then begin
    Flush;
    inc(StreamBufferPosition,StreamBufferSize);
    ReadBufferFromFile;
    BytesToWrite:=StreamBufferSize-(ptruint(StreamBufferPointer)-ptruint(StreamBuffer));
   end;
   if BytesToWrite>Count then begin
    BytesToWrite:=Count;
   end;
   if BytesToWrite>(ptruint(StreamBufferEnd)-ptruint(StreamBufferPointer)) then begin
    StreamBufferEnd:=@StreamBufferPointer[BytesToWrite];
   end;
   if BytesToWrite>0 then begin
    StreamBufferDirty:=true;
    Move(Source^,StreamBufferPointer^,BytesToWrite);
    dec(Count,BytesToWrite);
    inc(Source,BytesToWrite);
    inc(StreamBufferPointer,BytesToWrite);
    inc(result,BytesToWrite);
    CurrentPosition:=StreamBufferPosition+(ptruint(StreamBufferPointer)-ptruint(StreamBuffer));
    if WrappedStreamSize<CurrentPosition then begin
     WrappedStreamSize:=CurrentPosition;
    end;
   end;
  end;
 end;
end;

function TSimpleBufferedStream.Seek(Offset:longint;Origin:word):longint;
var CurrentPosition,Delta:int64;
begin
 CurrentPosition:=StreamBufferPosition+(ptruint(StreamBufferPointer)-ptruint(StreamBuffer));
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
    result:=WrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end;
  if (StreamBufferPosition<=result) and (result<(StreamBufferPosition+StreamBufferSize)) then begin
   StreamBufferPointer:=@StreamBuffer[result-StreamBufferPosition];
  end else begin
   Flush;
   Delta:=result mod StreamBufferSize;
   StreamBufferPosition:=result-Delta;
   ReadBufferFromFile;
   StreamBufferPointer:=@StreamBuffer[Delta];
  end;
  result:=StreamBufferPosition+(ptruint(StreamBufferPointer)-ptruint(StreamBuffer));
  if WrappedStreamSize<result then begin
   WrappedStreamSize:=result;
  end;
 end;
end;

function TSimpleBufferedStream.Seek(const Offset:int64;Origin:TSeekOrigin):int64;
var CurrentPosition,Delta:int64;
begin
 CurrentPosition:=StreamBufferPosition+(ptruint(StreamBufferPointer)-ptruint(StreamBuffer));
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
    result:=WrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end;
  if (StreamBufferPosition<=result) and (result<(StreamBufferPosition+StreamBufferSize)) then begin
   StreamBufferPointer:=@StreamBuffer[result-StreamBufferPosition];
  end else begin
   Flush;
   Delta:=result mod StreamBufferSize;
   StreamBufferPosition:=result-Delta;
   ReadBufferFromFile;
   StreamBufferPointer:=@StreamBuffer[Delta];
  end;
  result:=StreamBufferPosition+(ptruint(StreamBufferPointer)-ptruint(StreamBuffer));
  if WrappedStreamSize<result then begin
   WrappedStreamSize:=result;
  end;
 end;
end;

procedure TSimpleBufferedStream.Flush;
var BytesToWrite:int64;
begin
 if StreamBufferDirty then begin
  StreamBufferDirty:=false;
  if WrappedStream.Seek(StreamBufferPosition,soBeginning)<>StreamBufferPosition then begin
   raise EStreamError.Create('Seek error');
  end;
  BytesToWrite:=ptruint(StreamBufferEnd)-ptruint(StreamBuffer);
  if WrappedStream.Write(StreamBuffer^,BytesToWrite)<>BytesToWrite then begin
   raise EWriteError.Create('Write error');
  end;
  WrappedStreamSize:=WrappedStream.Size;
  if WrappedStream is TFileStream then begin
{$ifdef windows}
  Windows.FlushFileBuffers(TFileStream(WrappedStream).Handle);
{$else}
  fpfsync(TFileStream(WrappedStream).Handle);
{$endif}
  end;
 end;
end;

procedure TSimpleBufferedStream.SetSize(NewSize:longint);
begin
 if NewSize<0 then begin
  NewSize:=0;
 end;
 if NewSize>StreamBufferPosition then begin
  Flush;
 end;
 WrappedStream.Size:=NewSize;
 if WrappedStream.Seek(NewSize,soBeginning)<>NewSize then begin
  raise EStreamError.Create('Seek error');
 end;
 WrappedStreamSize:=NewSize;
 if Position>NewSize then begin
  Position:=NewSize;
 end;
end;

procedure TSimpleBufferedStream.SetSize(const NewSize:int64);
var NewSizeEx:int64;
begin
 NewSizeEx:=NewSize;
 if NewSizeEx<0 then begin
  NewSizeEx:=0;
 end;
 if NewSizeEx>StreamBufferPosition then begin
  Flush;
 end;
 WrappedStream.Size:=NewSizeEx;
 if WrappedStream.Seek(NewSizeEx,soBeginning)<>NewSizeEx then begin
  raise EStreamError.Create('Seek error');
 end;
 WrappedStreamSize:=NewSize;
 if Position>NewSizeEx then begin
  Position:=NewSizeEx;
 end;
end;

constructor TBufferedStream.Create(Stream:TStream;FreeStream:boolean=false;BufferSize:integer=DefaultBufferSize;BufferBucketCount:integer=DefaultBufferBucketCount);
var i:integer;
begin
 inherited Create;
 Buckets:=nil;
 WrappedStream:=Stream;
 WrappedStreamSize:=WrappedStream.Size;
 FreeWrappedStream:=FreeStream;
 BucketCount:=BufferBucketCount;
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
  StreamBufferSize:=BufferSize+1;
 end;
 StreamBufferSizeMask:=StreamBufferSize-1;
 StreamBufferSizeInvMask:=not StreamBufferSizeMask;
 StreamBufferSizeShift:=1;
 while (int64(1) shl StreamBufferSizeShift)<StreamBufferSize do begin
  inc(StreamBufferSizeShift);
 end;
 GetMem(StreamBuffer,StreamBufferSize*BucketCount);
 SetLength(Buckets,BucketCount);
 for i:=0 to BucketCount-1 do begin
  FillChar(Buckets[i],sizeof(TBufferedStreamBucket),#0);
  Buckets[i].Previous:=nil;
  Buckets[i].Next:=nil;
  Buckets[i].BasePosition:=-1;
  Buckets[i].Dirty:=false;
  Buckets[i].Buffer:=@StreamBuffer[i*StreamBufferSize];
 end;
 for i:=1 to BucketCount-1 do begin
  Buckets[i].Previous:=@Buckets[i-1];
 end;
 for i:=0 to BucketCount-2 do begin
  Buckets[i].Next:=@Buckets[i+1];
 end;
 FirstBucket:=@Buckets[0];
 LastBucket:=@Buckets[BucketCount-1];
 StreamPosition:=0;
 FillChar(BucketBitmap,sizeof(TBufferedStreamBucketBitmap),#0);
end;

destructor TBufferedStream.Destroy;
begin
 Flush;
 FreeMem(StreamBuffer);
 SetLength(Buckets,0);
 if FreeWrappedStream then begin
  FreeAndNil(WrappedStream);
 end;
 inherited Destroy;
end;

function TBufferedStream.GetBucket(StreamPosition:int64):PBufferedStreamBucket;
var MaskedStreamPosition,BucketBitmapPosition:int64;
begin
 MaskedStreamPosition:=StreamPosition and StreamBufferSizeInvMask;
 BucketBitmapPosition:=MaskedStreamPosition shr StreamBufferSizeShift;
 if ((BucketBitmapPosition shr 3)>=length(BucketBitmap)) or ((BucketBitmap[BucketBitmapPosition shr 3] and (1 shl (BucketBitmapPosition and 7)))<>0) then begin
  result:=FirstBucket;
  while assigned(result) and (result^.BasePosition<>MaskedStreamPosition) do begin
   result:=result^.Next;
  end;
 end else begin
  result:=nil;
 end;
 if not assigned(result) then begin
  result:=LastBucket;
  if result^.Dirty then begin
   if WrappedStream.Seek(result^.BasePosition,soBeginning)<>result^.BasePosition then begin
    raise EStreamError.Create('Seek error');
   end;
   if WrappedStream.Write(result^.Buffer^,result^.BufferSize)<>result^.BufferSize then begin
    raise EWriteError.Create('Write error');
   end;
   result^.Dirty:=false;
  end;
  if result^.BasePosition>=0 then begin
   BucketBitmapPosition:=result^.BasePosition shr StreamBufferSizeShift;
   if (BucketBitmapPosition shr 3)<length(BucketBitmap) then begin
    BucketBitmap[BucketBitmapPosition shr 3]:=BucketBitmap[BucketBitmapPosition shr 3] and not (1 shl (BucketBitmapPosition and 7));
   end;
  end;
  BucketBitmapPosition:=MaskedStreamPosition shr StreamBufferSizeShift;
  if (BucketBitmapPosition shr 3)<length(BucketBitmap) then begin
   BucketBitmap[BucketBitmapPosition shr 3]:=BucketBitmap[BucketBitmapPosition shr 3] or (1 shl (BucketBitmapPosition and 7));
  end;
  result^.BasePosition:=MaskedStreamPosition;
  if WrappedStream.Seek(result^.BasePosition,soBeginning)<>result^.BasePosition then begin
   raise EStreamError.Create('Seek error');
  end;
  result^.BufferSize:=WrappedStream.Read(result^.Buffer^,StreamBufferSize);
  if (result^.BufferSize<0) or (result^.BufferSize>StreamBufferSize) then begin
   raise EReadError.Create('Read error');
  end;
 end;
 if assigned(result) and (FirstBucket<>result) then begin
  if assigned(result^.Previous) then begin
   result^.Previous^.Next:=result^.Next;
  end;
  if assigned(result^.Next) then begin
   result^.Next^.Previous:=result^.Previous;
  end else if LastBucket=result then begin
   LastBucket:=result^.Previous;
  end;
  FirstBucket^.Previous:=result;
  result^.Next:=FirstBucket;
  result^.Previous:=nil;
  FirstBucket:=result;
 end;
end;

function TBufferedStream.Read(var Buffer;Count:longint):longint;
var Bucket:PBufferedStreamBucket;
    Destination:pansichar;
    BufferPosition,BytesToRead:int64;
begin
 result:=0;
 Destination:=@Buffer;
 if assigned(Destination) then begin
  while Count>0 do begin
   Bucket:=GetBucket(StreamPosition);
   if assigned(Bucket) then begin
    BufferPosition:=StreamPosition-Bucket^.BasePosition;
    BytesToRead:=Bucket^.BufferSize-BufferPosition;
    if BytesToRead>Count then begin
     BytesToRead:=Count;
    end;
    if BytesToRead>0 then begin
     Move(pansichar(Bucket^.Buffer)[BufferPosition],Destination^,BytesToRead);
     inc(StreamPosition,BytesToRead);
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

function TBufferedStream.Write(const Buffer;Count:longint):longint;
var Bucket:PBufferedStreamBucket;
    Source:pansichar;
    BufferPosition,BytesToWrite:int64;
    DoNeedResize:boolean;
begin
 result:=0;
 Source:=@Buffer;
 if assigned(Source) then begin
  while Count>0 do begin
   Bucket:=GetBucket(StreamPosition);
   if assigned(Bucket) then begin
    DoNeedResize:=false;
    BufferPosition:=StreamPosition-Bucket^.BasePosition;
    BytesToWrite:=Bucket^.BufferSize-BufferPosition;
    if BytesToWrite<=0 then begin
     BytesToWrite:=StreamBufferSize-BufferPosition;
     DoNeedResize:=true;
    end;
    if BytesToWrite>Count then begin
     BytesToWrite:=Count;
    end;
    if BytesToWrite>0 then begin
     Bucket^.Dirty:=true;
     Move(Source^,pansichar(Bucket^.Buffer)[BufferPosition],BytesToWrite);
     inc(StreamPosition,BytesToWrite);
     inc(Source,BytesToWrite);
     inc(result,BytesToWrite);
     dec(Count,BytesToWrite);
     if WrappedStreamSize<StreamPosition then begin
      WrappedStreamSize:=StreamPosition;
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

function TBufferedStream.Seek(Offset:longint;Origin:word):longint;
begin
 if (Origin=soFromCurrent) and (Offset=0) then begin
  result:=StreamPosition;
 end else begin
  case Origin of
   soFromBeginning:begin
    result:=Offset;
   end;
   soFromCurrent:begin
    result:=StreamPosition+Offset;
   end;
   soFromEnd:begin
    result:=WrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end else if result>WrappedStreamSize then begin
   result:=WrappedStreamSize;
  end;
  StreamPosition:=result;
 end;
end;

function TBufferedStream.Seek(const Offset:int64;Origin:TSeekOrigin):int64;
begin
 if (Origin=soCurrent) and (Offset=0) then begin
  result:=StreamPosition;
 end else begin
  case Origin of
   soBeginning:begin
    result:=Offset;
   end;
   soCurrent:begin
    result:=StreamPosition+Offset;
   end;
   soEnd:begin
    result:=WrappedStreamSize+Offset;
   end;
   else begin
    raise EStreamError.Create('Unknown seek origin');
   end;
  end;
  if result<0 then begin
   result:=0;
  end else if result>WrappedStreamSize then begin
   result:=WrappedStreamSize;
  end;
  StreamPosition:=result;
 end;
end;

procedure TBufferedStream.Flush;
var i:integer;
    Dirty:boolean;
begin
 Dirty:=false;
 for i:=0 to BucketCount-1 do begin
  if Buckets[i].Dirty then begin
   if WrappedStream.Seek(Buckets[i].BasePosition,soBeginning)<>Buckets[i].BasePosition then begin
    raise EStreamError.Create('Seek error');
   end;
   if WrappedStream.Write(Buckets[i].Buffer^,Buckets[i].BufferSize)<>Buckets[i].BufferSize then begin
    raise EWriteError.Create('Write error');
   end;
   Dirty:=true;
   Buckets[i].Dirty:=false;
  end;
 end;
 if Dirty and (WrappedStream is TFileStream) then begin
{$ifdef windows}
  Windows.FlushFileBuffers(TFileStream(WrappedStream).Handle);
{$else}
  fpfsync(TFileStream(WrappedStream).Handle);
{$endif}
 end;
end;

procedure TBufferedStream.SetSize(NewSize:longint);
begin
 if NewSize<0 then begin
  NewSize:=0;
 end;
 if NewSize>StreamPosition then begin
  Flush;
 end;
 WrappedStream.Size:=NewSize;
 if WrappedStream.Seek(NewSize,soBeginning)<>NewSize then begin
  raise EStreamError.Create('Seek error');
 end;
 WrappedStreamSize:=NewSize;
 if Position>NewSize then begin
  Position:=NewSize;
 end;
end;

procedure TBufferedStream.SetSize(const NewSize:int64);
var NewSizeEx:int64;
begin
 NewSizeEx:=NewSize;
 if NewSizeEx<0 then begin
  NewSizeEx:=0;
 end;
 if NewSizeEx>StreamPosition then begin
  Flush;
 end;
 WrappedStream.Size:=NewSizeEx;
 if WrappedStream.Seek(NewSizeEx,soBeginning)<>NewSizeEx then begin
  raise EStreamError.Create('Seek error');
 end;
 WrappedStreamSize:=NewSize;
 if Position>NewSizeEx then begin
  Position:=NewSizeEx;
 end;
end;

end.
