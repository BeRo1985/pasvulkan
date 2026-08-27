// Writes the MSF container which a PDB file is built on.
//
// MSF is a block based file holding a numbered set of streams, comparable to a
// very small file system. A PDB is nothing but an MSF whose streams happen to
// have agreed meanings: stream one is the information header, stream two the
// types, stream three the debug information, and so on.
//
// The layout is:
//
//   block 0        the super block, naming block size, block count and where
//                  the block map lives
//   block 1 and 2  two free block maps, one bit per block, a set bit meaning
//                  the block is free. They repeat every block size blocks, so
//                  for anything under sixteen megabytes these two are all
//                  there is.
//   block 3 on     stream data, then the stream directory, then the block map
//
// A stream is not stored contiguously. Its blocks are listed in the stream
// directory, and the directory itself is scattered too, which is why the block
// map exists: it lists the blocks the directory occupies.
unit UnitMSFWriter;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,
     Classes,
     PasVulkan.Types;

const pvMSFBlockSize=4096;

type TMSFWriter=class
      private
       type TStreamRecord=record
             Data:TMemoryStream;
             Owned:Boolean;
            end;
            TStreamRecords=array of TStreamRecord;
      private
       fStreams:TStreamRecords;
       fStreamCount:TpvSizeInt;
      public
       constructor Create;
       destructor Destroy; override;
       // Adds an empty stream and returns it to be filled by the caller. The
       // returned stream stays owned by the writer.
       function AddStream:TMemoryStream;
       // Adds a stream whose content is already there. Ownership stays with the
       // caller.
       function AddExistingStream(const aData:TMemoryStream):TpvSizeInt;
       function GetStream(const aIndex:TpvSizeInt):TMemoryStream;
       procedure SaveToFile(const aFileName:String);
       property StreamCount:TpvSizeInt read fStreamCount;
     end;

implementation

const MSFMagic:array[0..31] of AnsiChar=('M','i','c','r','o','s','o','f','t',' ','C','/','C','+','+',' ',
                                         'M','S','F',' ','7','.','0','0',#13,#10,#26,'D','S',#0,#0,#0);

constructor TMSFWriter.Create;
begin
 inherited Create;
 fStreams:=nil;
 fStreamCount:=0;
end;

destructor TMSFWriter.Destroy;
var Index:TpvSizeInt;
begin
 for Index:=0 to fStreamCount-1 do begin
  if fStreams[Index].Owned then begin
   FreeAndNil(fStreams[Index].Data);
  end;
 end;
 fStreams:=nil;
 inherited Destroy;
end;

function TMSFWriter.AddStream:TMemoryStream;
begin
 if fStreamCount>=length(fStreams) then begin
  SetLength(fStreams,(fStreamCount+1)*2);
 end;
 result:=TMemoryStream.Create;
 fStreams[fStreamCount].Data:=result;
 fStreams[fStreamCount].Owned:=true;
 inc(fStreamCount);
end;

function TMSFWriter.AddExistingStream(const aData:TMemoryStream):TpvSizeInt;
begin
 if fStreamCount>=length(fStreams) then begin
  SetLength(fStreams,(fStreamCount+1)*2);
 end;
 result:=fStreamCount;
 fStreams[fStreamCount].Data:=aData;
 fStreams[fStreamCount].Owned:=false;
 inc(fStreamCount);
end;

function TMSFWriter.GetStream(const aIndex:TpvSizeInt):TMemoryStream;
begin
 result:=fStreams[aIndex].Data;
end;

procedure TMSFWriter.SaveToFile(const aFileName:String);
var Stream:TFileStream;
    Directory:TMemoryStream;
    StreamBlocks:array of array of TpvUInt32;
    DirectoryBlocks:array of TpvUInt32;
    BlockMapBlock:TpvUInt32;
    NextBlock:TpvUInt32;
    Index,BlockIndex,Count:TpvSizeInt;
    Value32:TpvUInt32;
    DirectoryBlockCount:TpvSizeInt;
    TotalBlocks:TpvUInt32;
    FreeBlockMap:array[0..pvMSFBlockSize-1] of TpvUInt8;
    Padding:array[0..pvMSFBlockSize-1] of TpvUInt8;
    Written:TpvInt64;

 // Hands out the next block which is not one of the free block maps. Those sit
 // at one and two of every block size sized interval.
 function AllocateBlock:TpvUInt32;
 begin
  while (NextBlock mod pvMSFBlockSize)=1 do begin
   inc(NextBlock,2);
  end;
  result:=NextBlock;
  inc(NextBlock);
 end;

 function BlocksFor(const aSize:TpvInt64):TpvSizeInt;
 begin
  result:=TpvSizeInt((aSize+(pvMSFBlockSize-1)) div pvMSFBlockSize);
 end;

begin

 Directory:=TMemoryStream.Create;
 try

  // Blocks zero to two are the super block and the two free block maps.
  NextBlock:=3;

  SetLength(StreamBlocks,fStreamCount);
  for Index:=0 to fStreamCount-1 do begin
   Count:=BlocksFor(fStreams[Index].Data.Size);
   SetLength(StreamBlocks[Index],Count);
   for BlockIndex:=0 to Count-1 do begin
    StreamBlocks[Index][BlockIndex]:=AllocateBlock;
   end;
  end;

  // The directory lists the stream count, then every stream size, then the
  // block numbers of each stream in turn.
  Value32:=TpvUInt32(fStreamCount);
  Directory.WriteBuffer(Value32,SizeOf(TpvUInt32));
  for Index:=0 to fStreamCount-1 do begin
   Value32:=TpvUInt32(fStreams[Index].Data.Size);
   Directory.WriteBuffer(Value32,SizeOf(TpvUInt32));
  end;
  for Index:=0 to fStreamCount-1 do begin
   for BlockIndex:=0 to length(StreamBlocks[Index])-1 do begin
    Value32:=StreamBlocks[Index][BlockIndex];
    Directory.WriteBuffer(Value32,SizeOf(TpvUInt32));
   end;
  end;

  DirectoryBlockCount:=BlocksFor(Directory.Size);
  SetLength(DirectoryBlocks,DirectoryBlockCount);
  for Index:=0 to DirectoryBlockCount-1 do begin
   DirectoryBlocks[Index]:=AllocateBlock;
  end;

  BlockMapBlock:=AllocateBlock;
  TotalBlocks:=NextBlock;

  Stream:=TFileStream.Create(aFileName,fmCreate);
  try

   FillChar(Padding,SizeOf(Padding),#0);

   // Super block
   Stream.WriteBuffer(MSFMagic,SizeOf(MSFMagic));
   Value32:=pvMSFBlockSize;
   Stream.WriteBuffer(Value32,SizeOf(TpvUInt32));
   Value32:=1; // the free block map in use
   Stream.WriteBuffer(Value32,SizeOf(TpvUInt32));
   Stream.WriteBuffer(TotalBlocks,SizeOf(TpvUInt32));
   Value32:=TpvUInt32(Directory.Size);
   Stream.WriteBuffer(Value32,SizeOf(TpvUInt32));
   Value32:=0; // unknown, zero in every file produced by the toolchain
   Stream.WriteBuffer(Value32,SizeOf(TpvUInt32));
   Stream.WriteBuffer(BlockMapBlock,SizeOf(TpvUInt32));
   Stream.WriteBuffer(Padding,pvMSFBlockSize-Stream.Position);

   // The two free block maps. A set bit means free, so everything handed out
   // above is cleared and the rest of the last block stays free.
   FillChar(FreeBlockMap,SizeOf(FreeBlockMap),$ff);
   for Index:=0 to TpvSizeInt(TotalBlocks)-1 do begin
    FreeBlockMap[Index shr 3]:=FreeBlockMap[Index shr 3] and not (1 shl (Index and 7));
   end;
   Stream.WriteBuffer(FreeBlockMap,SizeOf(FreeBlockMap));
   FillChar(FreeBlockMap,SizeOf(FreeBlockMap),$ff);
   Stream.WriteBuffer(FreeBlockMap,SizeOf(FreeBlockMap));

   // Stream data, block by block, in the order the blocks were handed out.
   for Index:=0 to fStreamCount-1 do begin
    fStreams[Index].Data.Position:=0;
    for BlockIndex:=0 to length(StreamBlocks[Index])-1 do begin
     Stream.Position:=TpvInt64(StreamBlocks[Index][BlockIndex])*pvMSFBlockSize;
     Written:=fStreams[Index].Data.Size-fStreams[Index].Data.Position;
     if Written>pvMSFBlockSize then begin
      Written:=pvMSFBlockSize;
     end;
     if Written>0 then begin
      Stream.CopyFrom(fStreams[Index].Data,Written);
     end;
     if Written<pvMSFBlockSize then begin
      Stream.WriteBuffer(Padding,pvMSFBlockSize-Written);
     end;
    end;
   end;

   // The directory itself.
   Directory.Position:=0;
   for Index:=0 to DirectoryBlockCount-1 do begin
    Stream.Position:=TpvInt64(DirectoryBlocks[Index])*pvMSFBlockSize;
    Written:=Directory.Size-Directory.Position;
    if Written>pvMSFBlockSize then begin
     Written:=pvMSFBlockSize;
    end;
    if Written>0 then begin
     Stream.CopyFrom(Directory,Written);
    end;
    if Written<pvMSFBlockSize then begin
     Stream.WriteBuffer(Padding,pvMSFBlockSize-Written);
    end;
   end;

   // And the block map, which says where the directory blocks are.
   Stream.Position:=TpvInt64(BlockMapBlock)*pvMSFBlockSize;
   for Index:=0 to DirectoryBlockCount-1 do begin
    Stream.WriteBuffer(DirectoryBlocks[Index],SizeOf(TpvUInt32));
   end;
   Stream.WriteBuffer(Padding,pvMSFBlockSize-(DirectoryBlockCount*SizeOf(TpvUInt32)));

   Stream.Size:=TpvInt64(TotalBlocks)*pvMSFBlockSize;

  finally
   FreeAndNil(Stream);
  end;

 finally
  FreeAndNil(Directory);
 end;

end;

end.
