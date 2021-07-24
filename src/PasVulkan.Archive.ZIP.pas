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
unit PasVulkan.Archive.ZIP;
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
     PasVulkan.Types,
     PasVulkan.Collections,
     PasVulkan.Compression.Deflate;

type EpvArchiveZIP=class(Exception);

     PpvArchiveZIPHeaderSignature=^TpvArchiveZIPHeaderSignature;
     TpvArchiveZIPHeaderSignature=packed record
      case TpvUInt8 of
       0:(
        Chars:array[0..3] of TpvRawByteChar;
       );
       1:(
        Value:TpvUInt32;
       );
     end;

     TpvArchiveZIPHeaderSignatures=class
      public
       const LocalFileHeaderSignature:TpvArchiveZIPHeaderSignature=(Chars:('P','K',#3,#4));
             CentralFileHeaderSignature:TpvArchiveZIPHeaderSignature=(Chars:('P','K',#1,#2));
             EndCentralFileHeaderSignature:TpvArchiveZIPHeaderSignature=(Chars:('P','K',#5,#6));
     end;

     TpvArchiveZIPCompressionLevel=0..5;

     TpvArchiveZIPDateTimeUtils=class
      public
       class procedure ConvertDateTimeToZIPDateTime(const aDateTime:TDateTime;out aZIPDate,aZIPTime:TpvUInt16); static;
       class function ConvertZIPDateTimeToDateTime(const aZIPDate,aZIPTime:TpvUInt16):TDateTime; static;
     end;

     PpvArchiveZIPOS=^TpvArchiveZIPOS;
     TpvArchiveZIPOS=
      (
       FAT=0,
       UNIX=3,
       OS2=6,
       NTFS=10,
       VFAT=14,
       OSX=19
      );

     PpvArchiveZIPCRC32=^TpvArchiveZIPCRC32;
     TpvArchiveZIPCRC32=record
      private
       const CRC32Table:array[0..255] of TpvUInt32=($00000000,$77073096,$ee0e612c,$990951ba,$076dc419,$706af48f,$e963a535,$9e6495a3,
                                                    $0edb8832,$79dcb8a4,$e0d5e91e,$97d2d988,$09b64c2b,$7eb17cbd,$e7b82d07,$90bf1d91,
                                                    $1db71064,$6ab020f2,$f3b97148,$84be41de,$1adad47d,$6ddde4eb,$f4d4b551,$83d385c7,
                                                    $136c9856,$646ba8c0,$fd62f97a,$8a65c9ec,$14015c4f,$63066cd9,$fa0f3d63,$8d080df5,
                                                    $3b6e20c8,$4c69105e,$d56041e4,$a2677172,$3c03e4d1,$4b04d447,$d20d85fd,$a50ab56b,
                                                    $35b5a8fa,$42b2986c,$dbbbc9d6,$acbcf940,$32d86ce3,$45df5c75,$dcd60dcf,$abd13d59,
                                                    $26d930ac,$51de003a,$c8d75180,$bfd06116,$21b4f4b5,$56b3c423,$cfba9599,$b8bda50f,
                                                    $2802b89e,$5f058808,$c60cd9b2,$b10be924,$2f6f7c87,$58684c11,$c1611dab,$b6662d3d,
                                                    $76dc4190,$01db7106,$98d220bc,$efd5102a,$71b18589,$06b6b51f,$9fbfe4a5,$e8b8d433,
                                                    $7807c9a2,$0f00f934,$9609a88e,$e10e9818,$7f6a0dbb,$086d3d2d,$91646c97,$e6635c01,
                                                    $6b6b51f4,$1c6c6162,$856530d8,$f262004e,$6c0695ed,$1b01a57b,$8208f4c1,$f50fc457,
                                                    $65b0d9c6,$12b7e950,$8bbeb8ea,$fcb9887c,$62dd1ddf,$15da2d49,$8cd37cf3,$fbd44c65,
                                                    $4db26158,$3ab551ce,$a3bc0074,$d4bb30e2,$4adfa541,$3dd895d7,$a4d1c46d,$d3d6f4fb,
                                                    $4369e96a,$346ed9fc,$ad678846,$da60b8d0,$44042d73,$33031de5,$aa0a4c5f,$dd0d7cc9,
                                                    $5005713c,$270241aa,$be0b1010,$c90c2086,$5768b525,$206f85b3,$b966d409,$ce61e49f,
                                                    $5edef90e,$29d9c998,$b0d09822,$c7d7a8b4,$59b33d17,$2eb40d81,$b7bd5c3b,$c0ba6cad,
                                                    $edb88320,$9abfb3b6,$03b6e20c,$74b1d29a,$ead54739,$9dd277af,$04db2615,$73dc1683,
                                                    $e3630b12,$94643b84,$0d6d6a3e,$7a6a5aa8,$e40ecf0b,$9309ff9d,$0a00ae27,$7d079eb1,
                                                    $f00f9344,$8708a3d2,$1e01f268,$6906c2fe,$f762575d,$806567cb,$196c3671,$6e6b06e7,
                                                    $fed41b76,$89d32be0,$10da7a5a,$67dd4acc,$f9b9df6f,$8ebeeff9,$17b7be43,$60b08ed5,
                                                    $d6d6a3e8,$a1d1937e,$38d8c2c4,$4fdff252,$d1bb67f1,$a6bc5767,$3fb506dd,$48b2364b,
                                                    $d80d2bda,$af0a1b4c,$36034af6,$41047a60,$df60efc3,$a867df55,$316e8eef,$4669be79,
                                                    $cb61b38c,$bc66831a,$256fd2a0,$5268e236,$cc0c7795,$bb0b4703,$220216b9,$5505262f,
                                                    $c5ba3bbe,$b2bd0b28,$2bb45a92,$5cb36a04,$c2d7ffa7,$b5d0cf31,$2cd99e8b,$5bdeae1d,
                                                    $9b64c2b0,$ec63f226,$756aa39c,$026d930a,$9c0906a9,$eb0e363f,$72076785,$05005713,
                                                    $95bf4a82,$e2b87a14,$7bb12bae,$0cb61b38,$92d28e9b,$e5d5be0d,$7cdcefb7,$0bdbdf21,
                                                    $86d3d2d4,$f1d4e242,$68ddb3f8,$1fda836e,$81be16cd,$f6b9265b,$6fb077e1,$18b74777,
                                                    $88085ae6,$ff0f6a70,$66063bca,$11010b5c,$8f659eff,$f862ae69,$616bffd3,$166ccf45,
                                                    $a00ae278,$d70dd2ee,$4e048354,$3903b3c2,$a7672661,$d06016f7,$4969474d,$3e6e77db,
                                                    $aed16a4a,$d9d65adc,$40df0b66,$37d83bf0,$a9bcae53,$debb9ec5,$47b2cf7f,$30b5ffe9,
                                                    $bdbdf21c,$cabac28a,$53b39330,$24b4a3a6,$bad03605,$cdd70693,$54de5729,$23d967bf,
                                                    $b3667a2e,$c4614ab8,$5d681b02,$2a6f2b94,$b40bbe37,$c30c8ea1,$5a05df1b,$2d02ef8d);
      private
       fState:TpvUInt32;
      public
       procedure Initialize;
       procedure Update(const aData;const aDataLength:TpvSizeUInt); overload;
       procedure Update(const aStream:TStream); overload;
       function Finalize:TpvUInt32;
       property State:TpvUInt32 read fState write fState;
     end;

     PpvArchiveZIPLocalFileHeader=^TpvArchiveZIPLocalFileHeader;
     TpvArchiveZIPLocalFileHeader=packed record
      public
       Signature:TpvArchiveZIPHeaderSignature;
       ExtractVersion:TpvUInt16;
       BitFlags:TpvUInt16;
       CompressMethod:TpvUInt16;
       Time:TpvUInt16;
       Date:TpvUInt16;
       CRC32:TpvUInt32;
       CompressedSize:TpvUInt32;
       UncompressedSize:TpvUInt32;
       FileNameLength:TpvUInt16;
       ExtraFieldLength:TpvUInt16;
       procedure SwapEndiannessIfNeeded;
     end;

     PpvArchiveZIPExtensibleDataFieldHeader=^TpvArchiveZIPExtensibleDataFieldHeader;
     TpvArchiveZIPExtensibleDataFieldHeader=packed record
      public
       HeaderID:TpvUInt16;
       DataSize:TpvUInt16;
       procedure SwapEndiannessIfNeeded;
     end;

     PpvArchiveZIP64ExtensibleInfoFieldHeader=^TpvArchiveZIP64ExtensibleInfoFieldHeader;
     TpvArchiveZIP64ExtensibleInfoFieldHeader=packed record
      public
       OriginalSize:TpvUInt64;
       CompressedSize:TpvUInt64;
       RelativeHeaderOffset:TpvUInt64;
       DiskStartNumber:TpvUInt32;
       procedure SwapEndiannessIfNeeded;
     end;

     PpvArchiveZIPCentralFileHeader=^TpvArchiveZIPCentralFileHeader;
     TpvArchiveZIPCentralFileHeader=packed record
      public
       Signature:TpvArchiveZIPHeaderSignature;
       CreatorVersion:TpvUInt16;
       ExtractVersion:TpvUInt16;
       BitFlags:TpvUInt16;
       CompressMethod:TpvUInt16;
       Time:TpvUInt16;
       Date:TpvUInt16;
       CRC32:TpvUInt32;
       CompressedSize:TpvUInt32;
       UncompressedSize:TpvUInt32;
       FileNameLength:TpvUInt16;
       ExtraFieldLength:TpvUInt16;
       FileCommentLength:TpvUInt16;
       StartDiskNumber:TpvUInt16;
       InternalAttrributes:TpvUInt16;
       ExternalAttrributes:TpvUInt32;
       LocalFileHeaderOffset:TpvUInt32;
       procedure SwapEndiannessIfNeeded;
     end;

     PpvArchiveZIPEndCentralFileHeader=^TpvArchiveZIPEndCentralFileHeader;
     TpvArchiveZIPEndCentralFileHeader=packed record
      public
       Signature:TpvArchiveZIPHeaderSignature;
       DiskNumber:TpvUInt16;
       CentralDirectoryStartDisk:TpvUInt16;
       EntriesThisDisk:TpvUInt16;
       TotalEntries:TpvUInt16;
       CentralDirectorySize:TpvUInt32;
       StartDiskOffset:TpvUInt32;
       CommentLength:TpvUInt16;
       procedure SwapEndiannessIfNeeded;
     end;

     PpvArchiveZIP64EndCentralFileHeader=^TpvArchiveZIP64EndCentralFileHeader;
     TpvArchiveZIP64EndCentralFileHeader=packed record
      public
       Signature:TpvArchiveZIPHeaderSignature;
       RecordSize:TpvUInt64;
       VersionMadeBy:TpvUInt16;
       ExtractVersionRequired:TpvUInt16;
       DiskNumber:TpvUInt32;
       CentralDirectoryStartDisk:TpvUInt32;
       EntriesThisDisk:TpvUInt64;
       TotalEntries:TpvUInt64;
       CentralDirectorySize:TpvUInt64;
       StartDiskOffset:TpvUInt64;
       procedure SwapEndiannessIfNeeded;
     end;

     PpvArchiveZIP64EndCentralLocator=^TpvArchiveZIP64EndCentralLocator;
     TpvArchiveZIP64EndCentralLocator=packed record
      public
       Signature:TpvArchiveZIPHeaderSignature;
       EndOfCentralDirectoryStartDisk:TpvUInt32;
       CentralDirectoryOffset:TpvUInt64;
       TotalDisks:TpvUInt32;
       procedure SwapEndiannessIfNeeded;
     end;

     TpvArchiveZIP=class;

     TpvArchiveZIPEntry=class(TCollectionItem)
      private
       fFileName:TpvRawByteString;
       fAttributes:TpvUInt32;
       fDateTime:TDateTime;
       fCentralHeaderPosition:TpvInt64;
       fHeaderPosition:TpvInt64;
       fRequiresZIP64:boolean;
       fOS:TpvArchiveZIPOS;
       fSize:TpvInt64;
       fStream:TStream;
       fSourceArchive:TpvArchiveZIP;
       fCompressionLevel:TpvArchiveZIPCompressionLevel;
       fLocalFileHeader:TpvArchiveZIPLocalFileHeader;
       procedure SetFileName(const aFileName:TpvRawByteString);
       function GetDirectory:boolean;
       function GetLink:boolean;
      protected
       property CentralHeaderPosition:TpvInt64 read fCentralHeaderPosition write fCentralHeaderPosition;
       property HeaderPosition:TpvInt64 read fHeaderPosition write fHeaderPosition;
       property RequiresZIP64:boolean read fRequiresZIP64 write fRequiresZIP64;
      public
       constructor Create(aCollection:TCollection); override;
       destructor Destroy; override;
       procedure Assign(aSource:TPersistent); override;
       procedure LoadFromStream(const aStream:TStream);
       procedure LoadFromFile(const aFileName:string);
       procedure SaveToStream(const aStream:TStream);
       procedure SaveToFile(const aFileName:string);
       property Stream:TStream read fStream write fStream;
       property SourceArchive:TpvArchiveZIP read fSourceArchive write fSourceArchive;
       property Directory:boolean read GetDirectory;
       property Link:boolean read GetLink;
      published
       property FileName:TpvRawByteString read fFileName write SetFileName;
       property Attributes:TpvUInt32 read fAttributes write fAttributes;
       property DateTime:TDateTime read fDateTime write fDateTime;
       property OS:TpvArchiveZIPOS read fOS write fOS;
       property Size:TpvInt64 read fSize write fSize;
       property CompressionLevel:TpvArchiveZIPCompressionLevel read fCompressionLevel write fCompressionLevel;
     end;

     TpvArchiveZIPEntriesFileNameHashMap=TpvStringHashMap<TpvArchiveZIPEntry>;

     TpvArchiveZIPEntries=class(TCollection)
      private
       fFileNameHashMap:TpvArchiveZIPEntriesFileNameHashMap;
       function GetEntry(const aIndex:TpvSizeInt):TpvArchiveZIPEntry;
       procedure SetEntry(const aIndex:TpvSizeInt;const aEntry:TpvArchiveZIPEntry);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       function Add(const aFileName:TpvRawByteString):TpvArchiveZIPEntry; reintroduce;
       function Find(const aFileName:TpvRawByteString):TpvArchiveZIPEntry; reintroduce;
       property Entries[const aIndex:TpvSizeInt]:TpvArchiveZIPEntry read GetEntry write SetEntry; default;
     end;

     TpvArchiveZIP=class
      private
       fEntries:TpvArchiveZIPEntries;
       fStream:TStream;
      public
       constructor Create;
       destructor Destroy; override;
       class function CorrectPath(const aFileName:TpvRawByteString):TpvRawByteString; static;
       procedure Clear;
       procedure LoadFromStream(const aStream:TStream);
       procedure LoadFromFile(const aFileName:string);
       procedure SaveToStream(const aStream:TStream);
       procedure SaveToFile(const aFileName:string);
      published
       property Entries:TpvArchiveZIPEntries read fEntries;
     end;

implementation

procedure TpvArchiveZIPLocalFileHeader.SwapEndiannessIfNeeded;
begin
end;

procedure TpvArchiveZIPExtensibleDataFieldHeader.SwapEndiannessIfNeeded;
begin
end;

procedure TpvArchiveZIP64ExtensibleInfoFieldHeader.SwapEndiannessIfNeeded;
begin
end;

procedure TpvArchiveZIPCentralFileHeader.SwapEndiannessIfNeeded;
begin
end;

procedure TpvArchiveZIPEndCentralFileHeader.SwapEndiannessIfNeeded;
begin
end;

procedure TpvArchiveZIP64EndCentralFileHeader.SwapEndiannessIfNeeded;
begin
end;

procedure TpvArchiveZIP64EndCentralLocator.SwapEndiannessIfNeeded;
begin
end;

class procedure TpvArchiveZIPDateTimeUtils.ConvertDateTimeToZIPDateTime(const aDateTime:TDateTime;out aZIPDate,aZIPTime:TpvUInt16);
var Year,Month,Day,Hour,Minute,Second,Millisecond:TpvUInt16;
begin
 DecodeDate(aDateTime,Year,Month,Day);
 DecodeTime(aDateTime,Hour,Minute,Second,Millisecond);
 if Year<1980 then begin
  Year:=0;
  Month:=1;
  Day:=1;
  Hour:=0;
  Minute:=0;
  Second:=0;
  Millisecond:=0;
 end else begin
  dec(Year,1980);
 end;
 aZIPDate:=Day+(32*Month)+(512*Year);
 aZIPTime:=(Second div 2)+(32*Minute)+(2048*Hour);
end;

class function TpvArchiveZIPDateTimeUtils.ConvertZIPDateTimeToDateTime(const aZIPDate,aZIPTime:TpvUInt16):TDateTime;
begin
 result:=EncodeDate(((aZIPDate shr 9) and 127)+1980,
                    Max(1,(aZIPDate shr 5) and 15),
                    Max(1,aZIPDate and 31))+
         EncodeTime(aZIPTime shr 11,
                    (aZIPTime shr 5) and 63,
                    (aZIPTime and 31) shl 1,
                    0);
end;

procedure TpvArchiveZIPCRC32.Initialize;
begin
 fState:=$ffffffff;
end;

procedure TpvArchiveZIPCRC32.Update(const aData;const aDataLength:TpvSizeUInt);
var b:PpvUInt8;
    Index:TpvSizeUInt;
begin
 b:=@aData;
 for Index:=1 to aDataLength do begin
  fState:=CRC32Table[(fState and $ff) xor b^] xor ((fState shr 8) and $00ffffff);
  inc(b);
 end;
end;

procedure TpvArchiveZIPCRC32.Update(const aStream:TStream);
var b:TpvUInt8;
    Index:TpvSizeUInt;
begin
 if aStream is TMemoryStream then begin
  Update(TMemoryStream(aStream).Memory^,aStream.Size);
 end else begin
  aStream.Seek(0,soBeginning);
  for Index:=1 to aStream.Size do begin
   aStream.ReadBuffer(b,SizeOf(TpvUInt8));
   fState:=CRC32Table[(fState and $ff) xor b] xor ((fState shr 8) and $00ffffff);
  end;
 end;
end;

function TpvArchiveZIPCRC32.Finalize:TpvUInt32;
begin
 result:=not fState;
end;

constructor TpvArchiveZIPEntry.Create(aCollection:TCollection);
begin
 inherited Create(aCollection);
 fFileName:='';
 fOS:=TpvArchiveZIPOS.FAT;
//fOS:={$if defined(Unix) or defined(Posix)}TpvArchiveZIPOS.UNIX{$else}TpvArchiveZIPOS.FAT{$ifend};
 fCompressionLevel:=0;
 fDateTime:=Now;
 fRequiresZIP64:=false;
 fAttributes:=0;
 fStream:=nil;
end;

destructor TpvArchiveZIPEntry.Destroy;
begin
 if (Collection is TpvArchiveZIPEntries) and
    (length(fFileName)>0) then begin
  (Collection as TpvArchiveZIPEntries).fFileNameHashMap.Delete(fFileName);
 end;
 FreeAndNil(fStream);
 inherited Destroy;
end;

procedure TpvArchiveZIPEntry.SetFileName(const aFileName:TpvRawByteString);
var NewFileName:TpvRawByteString;
begin
 NewFileName:=TpvArchiveZIP.CorrectPath(aFileName);
 if fFileName<>NewFileName then begin
  if (Collection is TpvArchiveZIPEntries) and
     (length(fFileName)>0) then begin
   (Collection as TpvArchiveZIPEntries).fFileNameHashMap.Delete(fFileName);
  end;
  fFileName:=NewFileName;
  if (Collection is TpvArchiveZIPEntries) and
     (length(fFileName)>0) then begin
   (Collection as TpvArchiveZIPEntries).fFileNameHashMap.Add(fFileName,self);
  end;
 end;
end;

procedure TpvArchiveZIPEntry.Assign(aSource:TPersistent);
var Source:TpvArchiveZIPEntry;
begin
 if assigned(aSource) and (aSource is TpvArchiveZIPEntry) then begin
  Source:=aSource as TpvArchiveZIPEntry;
  SetFileName(Source.fFileName);
  fAttributes:=Source.fAttributes;
  fDateTime:=Source.fDateTime;
  fCentralHeaderPosition:=Source.fCentralHeaderPosition;
  fHeaderPosition:=Source.fHeaderPosition;
  fRequiresZIP64:=Source.fRequiresZIP64;
  fOS:=Source.fOS;
  fSize:=Source.fSize;
  if assigned(Source.fStream) then begin
   fStream:=TMemoryStream.Create;
   Source.fStream.Seek(0,soBeginning);
   fStream.CopyFrom(Source.fStream,Source.fStream.Size);
   fStream.Seek(0,soBeginning);
  end else begin
   FreeAndNil(fStream);
  end;
  fSourceArchive:=Source.fSourceArchive;
  fCompressionLevel:=Source.fCompressionLevel;
 end else begin
  inherited Assign(aSource);
 end;
end;

function TpvArchiveZIPEntry.GetDirectory:boolean;
begin
 result:=((Attributes=0) and (length(fFileName)>0) and (fFileName[length(fFileName)-1]='/')) or
         ((Attributes<>0) and
          ((((fOS=TpvArchiveZIPOS.FAT) and ((fAttributes and faDirectory)<>0))) or
            ((fOS=TpvArchiveZIPOS.UNIX) and ((fAttributes and $f000)=$4000))));
end;

function TpvArchiveZIPEntry.GetLink:boolean;
begin
 result:=(Attributes<>0) and
         ((((fOS=TpvArchiveZIPOS.FAT) and ((fAttributes and faSymLink)<>0))) or
           ((fOS=TpvArchiveZIPOS.UNIX) and ((fAttributes and $f000)=$a000)));
end;

procedure TpvArchiveZIPEntry.LoadFromStream(const aStream:TStream);
begin
 FreeAndNil(fStream);
 fStream:=TMemoryStream.Create;
 aStream.Seek(0,soBeginning);
 fStream.CopyFrom(aStream,aStream.Size);
 aStream.Seek(0,soBeginning);
 fSourceArchive:=nil;
end;

procedure TpvArchiveZIPEntry.LoadFromFile(const aFileName:string);
var Stream:TStream;
begin
 Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(Stream);
 finally
  Stream.Free;
 end;
end;

procedure TpvArchiveZIPEntry.SaveToStream(const aStream:TStream);
var LocalFileHeader:TpvArchiveZIPLocalFileHeader;
    BitBuffer,OriginalCRC:TpvUInt32;
    BitsInBitBuffer,FilePosition,InputBufferPosition,SlideWindowPosition:TpvSizeInt;
    ReachedSize,CompressedSize,UncompressedSize:TpvInt64;
    Offset,StartPosition,From:TpvInt64;
    CRC32:TpvArchiveZIPCRC32;
    ItIsAtEnd:boolean;
    ExtensibleDataFieldHeader:TpvArchiveZIPExtensibleDataFieldHeader;
    ExtensibleInfoFieldHeader:TpvArchiveZIP64ExtensibleInfoFieldHeader;
 function Decompress(const InStream,OutStream:TStream):boolean;
 const StatusOk=0;
       StatusCRCErr=-1;
       StatusWriteErr=-2;
       StatusReadErr=-3;
       StatusZipFileErr=-4;
       StatusUserAbort=-5;
       StatusNotSupported=-6;
       StatusEncrypted=-7;
       StatusInUse=-8;
       StatusInternalError=-9;
       StatusNoMoreItems=-10;
       StatusFileError=-11;
       StatusNoTpvArchiveZIPfile=-12;
       StatusHeaderTooLarge=-13;
       StatusZipFileOpenError=-14;
       StatusSeriousError=-100;
       StatusMissingParameter=-500;

       HuffmanTreeComplete=0;
       HuffmanTreeIncomplete=1;
       HuffmanTreeError=2;
       HuffmanTreeOutOfMemory=3;

       MaxMax=31*1024;
       SlidingDictionaryWindowSize=$8000;
       InBufferSize=1024*4;
       DefaultLiteralBits=9;
       DefaultDistanceBits=6;
       BitLengthCountMax=16;
       OrderOfBitLengthMax=288;
       HuffManTreeBuildMaxValue=16;

       MaxCode=8192;
       MaxStack=8192;
       InitialCodeSize=9;
       FinalCodeSize=13;

       TFileBufferSize=high(TpvInt32)-16;
       TFileNameSize=259;

       SupportedMethods=1 or (1 shl 1) or (1 shl 6) or (1 shl 8);

       MaskBits:array[0..16] of TpvUInt16=($0000,$0001,$0003,$0007,$000f,$001f,$003f,$007f,$00ff,$01ff,$03ff,$07ff,$0fff,$1fff,$3fff,$7fff,$ffff);
       Border:array[0..18] of TpvUInt8=(16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15);
       CopyLengthLiteralCodes:array[0..30] of TpvUInt16=(3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258,0,0);
       ExtraBitsLiteralCodes:array[0..30] of TpvUInt16=(0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0,99,99);
       CopyOffsetDistanceCodes:array[0..29] of TpvUInt16=(1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577);
       ExtraBitsDistanceCodes:array[0..29] of TpvUInt16=(0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13);
       CopyLength2:array[0..63] of TpvUInt16=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65);
       CopyLength3:array[0..63] of TpvUInt16=(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66);
       ExtraBitsTable:array[0..63] of TpvUInt16=(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8);
       CopyOffserDistanceCodes4:array[0..63] of TpvUInt16 =(1,65,129,193,257,321,385,449,513,577,641,705,769,833,897,961,1025,1089,1153,1217,1281,1345,1409,1473,1537,1601,1665,1729,1793,1857,1921,1985,2049,2113,2177,2241,2305,2369,2433,2497,2561,2625,2689,2753,2817,2881,2945,3009,3073,3137,3201,3265,3329,3393,3457,3521,3585,3649,3713,3777,3841,3905,3969,4033);
       CopyOffserDistanceCodes8:array[0..63] of TpvUInt16=(1,129,257,385,513,641,769,897,1025,1153,1281,1409,1537,1665,1793,1921,2049,2177,2305,2433,2561,2689,2817,2945,3073,3201,3329,3457,3585,3713,3841,3969,4097,4225,4353,4481,4609,4737,4865,4993,5121,5249,5377,5505,5633,5761,5889,6017,6145,6273,6401,6529,6657,6785,6913,7041,7169,7297,7425,7553,7681,7809,7937,8065);

 type PUSBList=^TUSBList;
      TUSBList=array[0..MaxMax] of TpvUInt16;

      PIOBuffer=^TIOBuffer;
      TIOBuffer=array[0..InBufferSize-1] of TpvUInt8;

      PPHuffManTree=^PHuffManTree;
      PHuffManTree=^THuffManTree;
      PHuffManTreeList=^THuffManTreeList;
      THuffManTree=record
       ExtraBits,CodeBits:TpvUInt8;
       ByteSize:TpvUInt16;
       LinkList:PHuffManTreeList;
      end;
      THuffManTreeList=array[0..8190] of THuffManTree;

      PPreviousCodeTrie=^TPreviousCodeTrie;
      TPreviousCodeTrie=array[257..MaxCode] of TpvInt32;
      PActualCodeTrie=^TActualCodeTrie;
      TActualCodeTrie=array[257..MaxCode] of TpvUInt8;
      PStack=^TStack;
      TStack=array[0..MaxStack] of TpvUInt8;

      PSlide=^TSlide;
      TSlide=array[0..SlidingDictionaryWindowSize-1] of TpvUInt8;

 var Slide:PSlide;
     InputBuffer:TIOBuffer;
     InputBufferPosition:TpvInt32;
     FilePosition:TpvInt32;
     SlideWindowPosition:TpvUInt16;
     BitBuffer:TpvUInt32;
     BitsInBitBuffer:TpvUInt8;
     ReachedSize:int64;
     BitsFlagsType:TpvUInt16;
     UserAbort,ItIsAtEnd:boolean;
     PreviousCode:PPreviousCodeTrie;
     ActualCode:PActualCodeTrie;
     Stack:PStack;
     NextFreeCodeInTrie:TpvInt32;

  procedure UpdateCRC(const IOBuffer:TIOBuffer;InLen:TpvInt32);
  begin
   CRC32.Update(IOBuffer,InLen);
  end;

  procedure Idle;
  begin
  end;

  procedure ReadBuffer;
  begin
   if ReachedSize>(CompressedSize+2) then begin
    FilePosition:=SizeOf(TIOBuffer);
    ItIsAtEnd:=true;
   end else begin
    Idle;
    FilePosition:=InStream.Read(InputBuffer,SizeOf(TIOBuffer));
    if FilePosition<=0 then begin
     FilePosition:=SizeOf(TIOBuffer);
     ItIsAtEnd:=true;
    end;
    inc(ReachedSize,FilePosition);
    dec(FilePosition);
   end;
   InputBufferPosition:=0;
  end;

  procedure ReadByte(var B:TpvUInt8);
  begin
   if InputBufferPosition>FilePosition then begin
    ReadBuffer;
   end;
   B:=InputBuffer[InputBufferPosition];
   inc(InputBufferPosition);
  end;

  procedure NeedBits(Count:TpvUInt8);
  var Value:TpvUInt32;
  begin
   while BitsInBitBuffer<Count do begin
    if InputBufferPosition>FilePosition then begin
     ReadBuffer;
    end;
    Value:=InputBuffer[InputBufferPosition];
    inc(InputBufferPosition);
    BitBuffer:=BitBuffer or (Value shl BitsInBitBuffer);
    inc(BitsInBitBuffer,8);
   end;
  end;

  procedure DumpBits(Count:TpvUInt8);
  begin
   BitBuffer:=BitBuffer shr Count;
   dec(BitsInBitBuffer,Count);
  end;

  function ReadBits(Count:TpvInt32):TpvInt32;
  begin
   if Count>0 then begin
    NeedBits(Count);
    result:=BitBuffer and ((1 shl Count)-1);
    DumpBits(Count);
   end else begin
    result:=0;
   end;
  end;

  function Flush(Bytes:TpvUInt32):boolean;
  begin
   result:=OutStream.Write(Slide^[0],Bytes)=TpvInt32(Bytes);
   CRC32.Update(Slide^[0],Bytes);
  end;

  procedure HuffManTreeFree(T:PHuffManTreeList);
  var P,Q:PHuffManTreeList;
      Z:TpvInt32;
  begin
   P:=T;
   while assigned(P) do begin
    dec(TpvPtrUInt(P),SizeOf(THuffManTree));
    Q:=P^[0].LinkList;
    Z:=P^[0].ByteSize;
    FreeMem(P,(Z+1)*SizeOf(THuffManTree));
    P:=Q;
   end;
  end;

  function HuffManTreeBuild(B:pword;N:TpvUInt16;S:TpvUInt16;D,E:PUSBList;T:PPHuffManTree;var M:TpvInt32):TpvInt32;
  type TBitLengthCountTable=array[0..BitLengthCountMax+1] of TpvUInt16;
  var CodeLengthKCount:TpvUInt16;
      BitLengthCountTable:TBitLengthCountTable;
      CurrentCodeCounterRepeatsEveryFEntries:TpvUInt16;
      MaxCodeLength:TpvInt32;
      TableLevel:TpvInt32;
      CurrentCodeCounter:TpvUInt16;
      Counter:TpvUInt16;
      NumberOfBitsInCurrentCode:TpvInt32;
      P:pword;
      CurrentTable:PHuffManTreeList;
      TableEntry:THuffManTree;
      TableStack:array[0..BitLengthCountMax] of PHuffManTreeList;
      ValuesInOrderOfBitsLength:array[0..OrderOfBitLengthMax] of TpvUInt16;
      BitsBeforeThisTable:TpvInt32;
      BitOffsets:array[0..BitLengthCountMax+1] of TpvUInt16;
      LLevelBitsInTableOfLevel:array[-1..BitLengthCountMax+1] of TpvUInt16;
      BitOffsetPointer:pword;
      NumberOfDummyCodesAdded:TpvInt32;
      NumberOfEntriesInCurrentTable:TpvUInt16;
      PT:PHuffManTree;
      EOBCodeLength:TpvUInt16;
  begin
   if N>256 then begin
    EOBCodeLength:=pword(TpvPtrUInt(TpvPtrUInt(B)+(256*SizeOf(TpvUInt16))))^;
   end else begin
    EOBCodeLength:=HuffManTreeBuildMaxValue;
   end;
   FillChar(BitLengthCountTable,SizeOf(TBitLengthCountTable),#0);

   P:=B;
   CurrentCodeCounter:=N;
   repeat
    if P^>BitLengthCountMax then begin
     T^:=nil;
     M:=0;
     HuffManTreeBuild:=HuffmanTreeError;
     exit;
    end;
    inc(BitLengthCountTable[P^]);
    inc(TpvPtrUInt(P),SizeOf(TpvUInt16));
    dec(CurrentCodeCounter);
   until CurrentCodeCounter=0;
   if BitLengthCountTable[0]=N then begin
    T^:=nil;
    M:=0;
    HuffManTreeBuild:=HuffmanTreeComplete;
    exit;
   end;

   Counter:=1;
   while (Counter<=BitLengthCountMax) and (BitLengthCountTable[Counter]=0) do inc(Counter);
   NumberOfBitsInCurrentCode:=Counter;
   if M<Counter then M:=Counter;
   CurrentCodeCounter:=BitLengthCountMax;
   while (CurrentCodeCounter>0) and (BitLengthCountTable[CurrentCodeCounter]=0) do dec(CurrentCodeCounter);
   MaxCodeLength:=CurrentCodeCounter;
   if M>CurrentCodeCounter then M:=CurrentCodeCounter;

   NumberOfDummyCodesAdded:=1 shl Counter;
   while Counter<CurrentCodeCounter do begin
    dec(NumberOfDummyCodesAdded,BitLengthCountTable[Counter]);
    if NumberOfDummyCodesAdded<0 then begin
     HuffManTreeBuild:=HuffmanTreeError;
     exit;
    end;
    NumberOfDummyCodesAdded:=NumberOfDummyCodesAdded shl 1;
    inc(Counter);
   end;
   dec(NumberOfDummyCodesAdded,BitLengthCountTable[CurrentCodeCounter]);
   if NumberOfDummyCodesAdded<0 then begin
    HuffManTreeBuild:=HuffmanTreeError;
    exit;
   end;
   inc(BitLengthCountTable[CurrentCodeCounter],NumberOfDummyCodesAdded);

   BitOffsets[1]:=0;
   Counter:=0;
   P:=pword(@BitLengthCountTable);
   inc(TpvPtrUInt(P),SizeOf(TpvUInt16));
   BitOffsetPointer:=pword(@BitOffsets);
   inc(TpvPtrUInt(BitOffsetPointer),2*SizeOf(TpvUInt16));
   dec(CurrentCodeCounter);
   while CurrentCodeCounter<>0 do begin
    inc(Counter,P^);
    BitOffsetPointer^:=Counter;
    inc(TpvPtrUInt(P),SizeOf(TpvUInt16));
    inc(TpvPtrUInt(BitOffsetPointer),SizeOf(TpvUInt16));
    dec(CurrentCodeCounter);
   end;

   P:=B;
   CurrentCodeCounter:=0;
   repeat
    Counter:=P^;
    inc(TpvPtrUInt(P),SizeOf(TpvUInt16));
    if Counter<>0 then begin
     ValuesInOrderOfBitsLength[BitOffsets[Counter]]:=CurrentCodeCounter;
     inc(BitOffsets[Counter]);
    end;
    inc(CurrentCodeCounter);
   until CurrentCodeCounter>=N;

   BitOffsets[0]:=0;
   CurrentCodeCounter:=0;
   P:=pword(@ValuesInOrderOfBitsLength);
   TableLevel:=-1;
   LLevelBitsInTableOfLevel[-1]:=0;
   BitsBeforeThisTable:=0;
   TableStack[0]:=nil;
   CurrentTable:=nil;
   NumberOfEntriesInCurrentTable:=0;

   for NumberOfBitsInCurrentCode:=NumberOfBitsInCurrentCode to MaxCodeLength do begin
    for CodeLengthKCount:=BitLengthCountTable[NumberOfBitsInCurrentCode] downto 1 do begin
     while NumberOfBitsInCurrentCode>(BitsBeforeThisTable+LLevelBitsInTableOfLevel[TableLevel]) do begin
      inc(BitsBeforeThisTable,LLevelBitsInTableOfLevel[TableLevel]);
      inc(TableLevel);
      NumberOfEntriesInCurrentTable:=MaxCodeLength-BitsBeforeThisTable;
      if NumberOfEntriesInCurrentTable>M then NumberOfEntriesInCurrentTable:=M;
      Counter:=NumberOfBitsInCurrentCode-BitsBeforeThisTable;
      CurrentCodeCounterRepeatsEveryFEntries:=1 shl Counter;
      if CurrentCodeCounterRepeatsEveryFEntries>(CodeLengthKCount+1) then begin
       dec(CurrentCodeCounterRepeatsEveryFEntries,CodeLengthKCount+1);
       BitOffsetPointer:=@BitLengthCountTable[NumberOfBitsInCurrentCode];
       inc(Counter);
       while Counter<NumberOfEntriesInCurrentTable do begin
         CurrentCodeCounterRepeatsEveryFEntries:=CurrentCodeCounterRepeatsEveryFEntries shl 1;
         inc(TpvPtrUInt(BitOffsetPointer),SizeOf(TpvUInt16));
         if CurrentCodeCounterRepeatsEveryFEntries<=BitOffsetPointer^ then begin
          break;
         end else begin
          dec(CurrentCodeCounterRepeatsEveryFEntries,BitOffsetPointer^);
          inc(Counter);
         end;
        end;
       end;
       if (BitsBeforeThisTable+Counter>EOBCodeLength) and (BitsBeforeThisTable<EOBCodeLength) then Counter:=EOBCodeLength-BitsBeforeThisTable;
       if BitsBeforeThisTable=0 then Counter:=M;
       NumberOfEntriesInCurrentTable:=1 shl Counter;
       LLevelBitsInTableOfLevel[TableLevel]:=Counter;

       GETMEM(CurrentTable,(NumberOfEntriesInCurrentTable+1)*SizeOf(THuffManTree));
       if not assigned(CurrentTable) then begin
        if TableLevel<>0 then HuffManTreeFree(TableStack[0]);
        HuffManTreeBuild:=HuffmanTreeOutOfMemory;
        exit;
       end;
       FillChar(CurrentTable^,(NumberOfEntriesInCurrentTable+1)*SizeOf(THuffManTree),#0);
       CurrentTable^[0].ByteSize:=NumberOfEntriesInCurrentTable;
       T^:=@CurrentTable^[1];
       T:=PPHuffManTree(@CurrentTable^[0].LinkList);
       T^:=nil;
       CurrentTable:=PHuffManTreeList(@CurrentTable^[1]);
       TableStack[TableLevel]:=CurrentTable;
       if TableLevel<>0 then begin
        BitOffsets[TableLevel]:=CurrentCodeCounter;
        TableEntry.CodeBits:=LLevelBitsInTableOfLevel[TableLevel-1];
        TableEntry.ExtraBits:=16+Counter;
        TableEntry.LinkList:=CurrentTable;
        Counter:=(CurrentCodeCounter and ((1 shl BitsBeforeThisTable)-1)) shr (BitsBeforeThisTable-LLevelBitsInTableOfLevel[TableLevel-1]);
        PT:=PHuffManTree(TpvPtrUInt(TpvPtrUInt(TableStack[TableLevel-1])-SizeOf(THuffManTree)));
        if Counter>PT^.ByteSize then begin
         HuffManTreeFree(TableStack[0]);
         HuffManTreeBuild:=HuffmanTreeError;
         exit;
        end;
        PT:=@TableStack[TableLevel-1]^[Counter];
        PT^:=TableEntry;
       end;
      end;

      TableEntry.CodeBits:=TpvUInt16(NumberOfBitsInCurrentCode-BitsBeforeThisTable);
      TableEntry.LinkList:=nil;
      if TpvPtrUInt(P)>=TpvPtrUInt(@ValuesInOrderOfBitsLength[N]) then begin
       TableEntry.ExtraBits:=99;
      end else if P^<S then begin
       if P^<256 then begin
        TableEntry.ExtraBits:=16;
       end else begin
        TableEntry.ExtraBits:=15;
       end;
       TableEntry.ByteSize:=P^;
       inc(TpvPtrUInt(P),SizeOf(TpvUInt16));
      end else begin
       if not (assigned(D) and assigned(E)) then begin
        HuffManTreeFree(TableStack[0]);
        HuffManTreeBuild:=HuffmanTreeError;
        exit;
       end;
       TableEntry.ExtraBits:=TpvUInt16(E^[P^-S]);
       TableEntry.ByteSize:=D^[P^-S];
       inc(TpvPtrUInt(P),SizeOf(TpvUInt16));
      end;

      CurrentCodeCounterRepeatsEveryFEntries:=1 shl(NumberOfBitsInCurrentCode-BitsBeforeThisTable);
      Counter:=CurrentCodeCounter shr BitsBeforeThisTable;
      while Counter<NumberOfEntriesInCurrentTable do begin
       CurrentTable^[Counter]:=TableEntry;
       inc(Counter,CurrentCodeCounterRepeatsEveryFEntries);
      end;

      Counter:=1 shl(NumberOfBitsInCurrentCode-1);
      while(CurrentCodeCounter and Counter)<> 0 do begin
       CurrentCodeCounter:=CurrentCodeCounter xor Counter;
       Counter:=Counter shr 1;
      end;
      CurrentCodeCounter:=CurrentCodeCounter xor Counter;

      while ((CurrentCodeCounter and ((1 shl BitsBeforeThisTable)-1))<>BitOffsets[TableLevel]) do begin
       dec(TableLevel);
       dec(BitsBeforeThisTable,LLevelBitsInTableOfLevel[TableLevel]);
      end;
    end;
   end;
   if (NumberOfDummyCodesAdded<>0) and (MaxCodeLength<>1) then begin
    result:=HuffmanTreeIncomplete;
   end else begin
    result:=HuffmanTreeComplete;
   end;
  end;

  function InflateCodes(LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32):TpvInt32;
  var N,D,ElementLength:TpvUInt16;
      LMask,DMask:TpvUInt16;
      T:PHuffManTree;
      TableEntry:TpvUInt8;
  begin
   LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
   DMask:=MaskBits[DistanceCodeTableLookupBits];
   while not (UserAbort or ItIsAtEnd) do begin
    NeedBits(LiteralLengthCodeTableLookupBits);
    T:=@LiteralLengthCodeTable^[BitBuffer and LMask];
    TableEntry:=T^.ExtraBits;
    if TableEntry>16 then begin
     repeat
      if TableEntry=99 then begin
       result:=StatusZipFileErr;
       exit;
      end;
      DumpBits(T^.CodeBits);
      dec(TableEntry,16);
      NeedBits(TableEntry);
      T:=@T^.LinkList^[BitBuffer and MaskBits[TableEntry]];
      TableEntry:=T^.ExtraBits;
     until TableEntry<=16;
    end;
    DumpBits(T^.CodeBits);
    if TableEntry=16 then begin
     Slide^[SlideWindowPosition]:=T^.ByteSize;
     inc(SlideWindowPosition);
     if SlideWindowPosition=SlidingDictionaryWindowSize then begin
      if not Flush(SlideWindowPosition) then begin
       InflateCodes:=StatusWriteErr;
       exit;
      end;
      SlideWindowPosition:=0;
     end;
    end else begin
     if TableEntry=15 then begin
      InflateCodes:=StatusOk;
      exit;
     end;
     NeedBits(TableEntry);
     N:=T^.ByteSize+(BitBuffer and MaskBits[TableEntry]);
     DumpBits(TableEntry);
     NeedBits(DistanceCodeTableLookupBits);
     T:=@DistanceCodeTable^[BitBuffer and DMask];
     TableEntry:=T^.ExtraBits;
     if TableEntry>16 then begin
      repeat
       if TableEntry=99 then begin
        InflateCodes:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(TableEntry,16);
       NeedBits(TableEntry);
       T:=@T^.LinkList^[BitBuffer and MaskBits[TableEntry]];
       TableEntry:=T^.ExtraBits;
      until TableEntry<=16;
     end;
     DumpBits(T^.CodeBits);
     NeedBits(TableEntry);
     D:=SlideWindowPosition-T^.ByteSize-TpvUInt16(BitBuffer and MaskBits[TableEntry]);
     DumpBits(TableEntry);
     repeat
      D:=D and (SlidingDictionaryWindowSize-1);
      if D>SlideWindowPosition then begin
       ElementLength:=SlidingDictionaryWindowSize-D;
      end else begin
       ElementLength:=SlidingDictionaryWindowSize-SlideWindowPosition;
      end;
      if ElementLength>N then ElementLength:=N;
      dec(N,ElementLength);
      if (SlideWindowPosition-D)>=ElementLength then begin
       Move(Slide^[D],Slide^[SlideWindowPosition],ElementLength);
       inc(SlideWindowPosition,ElementLength);
       inc(D,ElementLength);
      end else begin
       repeat
        Slide^[SlideWindowPosition]:=Slide^[D];
        inc(SlideWindowPosition);
        inc(D);
        dec(ElementLength);
       until ElementLength=0;
      end;
      if SlideWindowPosition=SlidingDictionaryWindowSize then begin
       if not Flush(SlideWindowPosition) then begin
        result:=StatusWriteErr;
        exit;
       end;
       SlideWindowPosition:=0;
      end;
     until N=0;
    end;
   end;
   if UserAbort then begin
    result:=Statususerabort
   end else begin
    result:=StatusreadErr;
   end;
  end;

  function InflateStored:TpvInt32;
  var NumberOfBlockInBlock:TpvUInt16;
  begin
   NumberOfBlockInBlock:=BitsInBitBuffer and 7;
   DumpBits(NumberOfBlockInBlock);

   NeedBits(16);
   NumberOfBlockInBlock:=BitBuffer and $ffff;
   DumpBits(16);
   NeedBits(16);
   if NumberOfBlockInBlock<>((not BitBuffer) and $ffff) then begin
    result:=StatuszipFileErr;
    exit;
   end;
   DumpBits(16);
   while (NumberOfBlockInBlock>0) and not (UserAbort or ItIsAtEnd) do begin
    dec(NumberOfBlockInBlock);
    NeedBits(8);
    Slide^[SlideWindowPosition]:=BitBuffer;
    inc(SlideWindowPosition);
    if SlideWindowPosition=SlidingDictionaryWindowSize then begin
     if not Flush(SlideWindowPosition)then begin
      result:=StatusWriteErr;
      exit;
     end;
     SlideWindowPosition:=0;
    end;
    DumpBits(8);
   end;
   if UserAbort then begin
    result:=StatusUserAbort;
   end else if ItIsAtEnd then begin
    result:=StatusreadErr;
   end else begin
    result:=StatusOk;
   end;
  end;

  function InflateFixed:TpvInt32;
  var Counter,Value:TpvInt32;
      LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;
      LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32;
      LengthList:array[0..287] of TpvUInt16;
  begin
   for Counter:=0 to 143 do LengthList[Counter]:=8;
   for Counter:=144 to 255 do LengthList[Counter]:=9;
   for Counter:=256 to 279 do LengthList[Counter]:=7;
   for Counter:=280 to 287 do LengthList[Counter]:=8;
   LiteralLengthCodeTableLookupBits:=7;
   Value:=HuffManTreeBuild(pword(@LengthList),288,257,PUSBList(@CopyLengthLiteralCodes),PUSBList(@ExtraBitsLiteralCodes),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits); {@@}
   if Value<>HuffmanTreeComplete then begin
    result:=Value;
    exit;
   end;
   for Counter:=0 to 29 do LengthList[Counter]:=5;
   DistanceCodeTableLookupBits:=5;
   if HuffManTreeBuild(pword(@LengthList),30,0,PUSBList(@CopyOffsetDistanceCodes),PUSBList(@ExtraBitsDistanceCodes),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits)>HuffmanTreeIncomplete then begin
    HuffManTreeFree(LiteralLengthCodeTable);
    result:=StatusZipFileErr;
    exit;
   end;
   result:=InflateCodes(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
   HuffManTreeFree(LiteralLengthCodeTable);
   HuffManTreeFree(DistanceCodeTable);
  end;

  function InflateDynamic:TpvInt32;
  var I:TpvInt32;
      J:TpvUInt16;
      LastLength:TpvUInt16;
      BitLengthTableMask:TpvUInt16;
      NumberOfLengthsToGet:TpvUInt16;
      LiteralLengthCodeTable,
      DistanceCodeTable:PHuffManTreeList;
      LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32;
      NumberOfBitLengthCodes,NumberOfLiteralLengthCodes,NumberOfDistanceCodes:TpvUInt16;
      LiteralLengthDistanceCodeLengths:array[0..288+32-1] of TpvUInt16;
  begin
   NeedBits(5);
   NumberOfLiteralLengthCodes:=257+TpvUInt16(BitBuffer) and $1f;
   DumpBits(5);
   NeedBits(5);
   NumberOfDistanceCodes:=1+TpvUInt16(BitBuffer) and $1f;
   DumpBits(5);
   NeedBits(4);
   NumberOfBitLengthCodes:=4+TpvUInt16(BitBuffer) and $f;
   DumpBits(4);
   if (NumberOfLiteralLengthCodes>288) or (NumberOfDistanceCodes>32) then begin
    result:=1;
    exit;
   end;

   FillChar(LiteralLengthDistanceCodeLengths,SizeOf(LiteralLengthDistanceCodeLengths),#0);
   for J:=0 to NumberOfBitLengthCodes-1 do begin
    NeedBits(3);
    LiteralLengthDistanceCodeLengths[Border[J]]:=BitBuffer and 7;
    DumpBits(3);
   end;
   for J:=NumberOfBitLengthCodes to 18 do LiteralLengthDistanceCodeLengths[Border[J]]:=0;

   LiteralLengthCodeTableLookupBits:=7;
   I:=HuffManTreeBuild(pword(@LiteralLengthDistanceCodeLengths),19,19,nil,nil,PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits); {@@}
   if I<>HuffmanTreeComplete then begin
    if I=HuffmanTreeIncomplete then HuffManTreeFree(LiteralLengthCodeTable);
    result:=StatusZipFileErr;
    exit;
   end;

   NumberOfLengthsToGet:=NumberOfLiteralLengthCodes+NumberOfDistanceCodes;
   BitLengthTableMask:=MaskBits[LiteralLengthCodeTableLookupBits];
   I:=0;
   LastLength:=0;
   while TpvUInt16(I)<NumberOfLengthsToGet do begin
    NeedBits(LiteralLengthCodeTableLookupBits);
    DistanceCodeTable:=PHuffManTreeList(@LiteralLengthCodeTable^[BitBuffer and BitLengthTableMask]);
    J:=PHuffManTree(DistanceCodeTable)^.CodeBits;
    DumpBits(J);
    J:=PHuffManTree(DistanceCodeTable)^.ByteSize;
    if J<16 then begin
     LastLength:=J;
     LiteralLengthDistanceCodeLengths[I]:=LastLength;
     inc(I)
    end else if J=16 then begin
     NeedBits(2);
     J:=3+(BitBuffer and 3);
     DumpBits(2);
     if (I+J)>NumberOfLengthsToGet then begin
      result:=1;
      exit;
     end;
     while J>0 do begin
      LiteralLengthDistanceCodeLengths[I]:=LastLength;
      dec(J);
      inc(I);
     end;
    end else if J=17 then begin
     NeedBits(3);
     J:=3+(BitBuffer and 7);
     DumpBits(3);
     if (I+J)>NumberOfLengthsToGet then begin
      result:=1;
      exit;
     end;
     while J>0 do begin
      LiteralLengthDistanceCodeLengths[I]:=0;
      inc(I);
      dec(J);
     end;
     LastLength:=0;
    end else begin
     NeedBits(7);
     J:=11+(BitBuffer and $7f);
     DumpBits(7);
     if (I+J)>NumberOfLengthsToGet then begin
      result:=StatuszipfileErr;
      exit;
     end;
     while J>0 do begin
      LiteralLengthDistanceCodeLengths[I]:=0;
      dec(J);
      inc(I);
     end;
     LastLength:=0;
    end;
   end;
   HuffManTreeFree(LiteralLengthCodeTable);

   LiteralLengthCodeTableLookupBits:=DefaultLiteralBits;
   I:=HuffManTreeBuild(pword(@LiteralLengthDistanceCodeLengths),NumberOfLiteralLengthCodes,257,PUSBList(@CopyLengthLiteralCodes),PUSBList(@ExtraBitsLiteralCodes),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits);
   if I<>HuffmanTreeComplete then begin
    if I=HuffmanTreeIncomplete then HuffManTreeFree(LiteralLengthCodeTable);
    result:=StatusZipFileErr;
    exit;
   end;
   DistanceCodeTableLookupBits:=DefaultDistanceBits;
   I:=HuffManTreeBuild(pword(@LiteralLengthDistanceCodeLengths[NumberOfLiteralLengthCodes]),NumberOfDistanceCodes,0,PUSBList(@CopyOffsetDistanceCodes),PUSBList(@ExtraBitsDistanceCodes),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
   if I>HuffmanTreeIncomplete then begin
    if I=HuffmanTreeIncomplete then HuffManTreeFree(DistanceCodeTable);
    HuffManTreeFree(LiteralLengthCodeTable);
    result:=StatusZipFileErr;
    exit;
   end;
   InflateDynamic:=InflateCodes(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
   HuffManTreeFree(LiteralLengthCodeTable);
   HuffManTreeFree(DistanceCodeTable);
  end;

  function InflateBlock(var E:TpvInt32):TpvInt32;
  var T:TpvUInt16;
  begin
   NeedBits(1);
   E:=BitBuffer and 1;
   DumpBits(1);

   NeedBits(2);
   T:=BitBuffer and 3;
   DumpBits(2);

   case T of
    0:result:=InflateStored;
    1:result:=InflateFixed;
    2:result:=InflateDynamic;
    else result:=StatusZipFileErr;
   end;
  end;

  function Inflate:TpvInt32;
  var LastBlockFlag:TpvInt32;
  begin
   InputBufferPosition:=0;
   FilePosition:=-1;
   SlideWindowPosition:=0;
   BitsInBitBuffer:=0;
   BitBuffer:=0;
   repeat
    result:=InflateBlock(LastBlockFlag);
    if result<>0 then begin
     exit;
    end;
   until LastBlockFlag<>0;
   if not Flush(SlideWindowPosition) then begin
    result:=StatusWriteErr;
   end else begin
    result:=StatusOk;
   end;
  end;

  function CopyStored:TpvInt32;
  var DoReadInBytes,ReadInBytes:TpvInt32;
  begin
   while (ReachedSize<CompressedSize) and not UserAbort do begin
    DoReadInBytes:=CompressedSize-ReachedSize;
    if DoReadInBytes>SlidingDictionaryWindowSize then begin
     DoReadInBytes:=SlidingDictionaryWindowSize;
    end;
    ReadInBytes:=InStream.Read(Slide^[0],DoReadInBytes);
    if ReadInBytes<>DoReadInBytes then begin
     result:=StatusReadErr;
     exit;
    end;
    if not Flush(ReadInBytes) then begin
     result:=StatusWriteErr;
     exit;
    end;
    inc(ReachedSize,ReadInBytes);
    Idle;
   end;
   if UserAbort then begin
    result:=StatusUserabort;
   end else begin
    result:=StatusOk;
   end;
  end;

  function GetTree(l:pword;N:TpvUInt16):TpvInt32;
  var I,K,J,B:TpvUInt16;
      ByteBuffer:TpvUInt8;
  begin
   ReadByte(ByteBuffer);
   I:=ByteBuffer;
   inc(I);
   K:=0;
   repeat
    ReadByte(ByteBuffer);
    J:=ByteBuffer;
    B:=(J and $f)+1;
    J:=((J and $f0) shr 4)+1;
    if (K+J)>N then begin
     result:=4;
     exit;
    end;
    repeat
     l^:=B;
     inc(TpvPtrUInt(l),SizeOf(TpvUInt16));
     inc(K);
     dec(J);
    until J=0;
    dec(I);
   until I=0;
   if K<>N then begin
    result:=4;
   end else begin
    result:=0;
   end;
  end;

  function ExplodeLiteral8k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32): TpvInt32;
  var S:TpvInt32;
      E:TpvUInt16;
      N,D:TpvUInt16;
      W:TpvUInt16;
      T:PHuffManTree;
      BMask,LMask,DMask:TpvUInt16;
      U:TpvUInt16;
  begin
   BitBuffer:=0;
   BitsInBitBuffer:=0;
   W:=0;
   U:=1;
   BMask:=MaskBits[BitLengthCodeTableLookupBits];
   LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
   DMask:=MaskBits[DistanceCodeTableLookupBits];
   S:=UncompressedSize;
   while (S>0) and not (UserAbort or ItIsAtEnd) do begin
    NeedBits(1);
    if(BitBuffer and 1)<>0 then begin
     DumpBits(1);
     dec(S);
     NeedBits(BitLengthCodeTableLookupBits);
     T:=@BitLengthCodeTable^[BMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.ExtraBits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     Slide^[W]:=T^.ByteSize;
     inc(W);
     if W=SlidingDictionaryWindowSize then begin
      if not Flush(W) then begin
       result:=StatusWriteErr;
       exit;
      end;
      W:=0;
      U:=0;
     end;
    end else begin
     DumpBits(1);
     NeedBits(7);
     D:=BitBuffer and $7f;
     DumpBits(7);
     NeedBits(DistanceCodeTableLookupBits);
     T:=@DistanceCodeTable^[DMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.Extrabits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     D:=W-D-T^.ByteSize;
     NeedBits(LiteralLengthCodeTableLookupBits);
     T:=@LiteralLengthCodeTable^[LMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.ExtraBits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     N:=T^.ByteSize;
     if E<>0 then begin
      NeedBits(8);
      inc(N,TpvUInt8(BitBuffer) and $ff);
      DumpBits(8);
     end;
     dec(S,N);
     repeat
      D:=D and (SlidingDictionaryWindowSize-1);
      if D>W then begin
       E:=SlidingDictionaryWindowSize-D;
      end else begin
       E:=SlidingDictionaryWindowSize-W;
      end;
      if E>N then E:=N;
      dec(N,E);
      if (U<>0) and (W<=D) then begin
       FillChar(Slide^[W],E,#0);
       inc(W,E);
       inc(D,E);
      end else if(W-D>=E)then begin
       Move(Slide^[D],Slide^[W],E);
       inc(W,E);
       inc(D,E);
      end else begin
       repeat
        Slide^[W]:=Slide^[D];
        inc(W);
        inc(D);
        dec(E);
       until E=0;
      end;
      if W=SlidingDictionaryWindowSize then begin
       if not Flush(W)then begin
        result:=StatusWriteErr;
        exit;
       end;
       W:=0;
       U:=0;
      end;
     until N=0;
    end;
   end;
   if UserAbort then begin
    result:=StatusUserAbort;
   end else if not Flush(W) then begin
    result:=StatusWriteErr;
   end else if ItIsAtEnd then begin
    result:=StatusReadErr;
   end else begin
    result:=StatusOk;
   end;
  end;

  function ExplodeLiteral4k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32): TpvInt32;
  var S:TpvInt32;
      E:TpvUInt16;
      N,D:TpvUInt16;
      W:TpvUInt16;
      T:PHuffManTree;
      BMask,LMask,DMask:TpvUInt16;
      U:TpvUInt16;
  begin
   BitBuffer:=0;
   BitsInBitBuffer:=0;
   W:=0;
   U:=1;
   BMask:=MaskBits[BitLengthCodeTableLookupBits];
   LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
   DMask:=MaskBits[DistanceCodeTableLookupBits];
   S:=UncompressedSize;
   while (S>0) and not (UserAbort or ItIsAtEnd) do begin
    NeedBits(1);
    if (BitBuffer and 1)<>0 then begin
     DumpBits(1);
     dec(S);
     NeedBits(BitLengthCodeTableLookupBits);
     T:=@BitLengthCodeTable^[BMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.ExtraBits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     Slide^[W]:=T^.ByteSize;
     inc(W);
     if W=SlidingDictionaryWindowSize then begin
      if not Flush(W) then begin
       result:=StatusWriteErr;
       exit;
      end;
      W:=0;
      U:=0;
     end;
    end else begin
     DumpBits(1);
     NeedBits(6);
     D:=BitBuffer and $3f;
     DumpBits(6);
     NeedBits(DistanceCodeTableLookupBits);
     T:=@DistanceCodeTable^[DMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.ExtraBits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     D:=W-D-T^.ByteSize;
     NeedBits(LiteralLengthCodeTableLookupBits);
     T:=@LiteralLengthCodeTable^[LMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.ExtraBits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     N:=T^.ByteSize;
     if E<>0 then begin
      NeedBits(8);
      inc(N,BitBuffer and $ff);
      DumpBits(8);
     end;
     dec(S,N);
     repeat
      D:=D and (SlidingDictionaryWindowSize-1);
      if D>W then begin
       E:=SlidingDictionaryWindowSize-D;
      end else begin
       E:=SlidingDictionaryWindowSize-W;
      end;
      if E>N then E:=N;
      dec(N,E);
      if (U<>0) and (W<=D) then begin
       FillChar(Slide^[W],E,#0);
       inc(W,E);
       inc(D,E);
      end else if (W-D)>=E then begin
       Move(Slide^[D],Slide^[W],E);
       inc(W,E);
       inc(D,E);
      end else begin
       repeat
        Slide^[W]:=Slide^[D];
        inc(W);
        inc(D);
        dec(E);
       until E=0;
      end;
      if W=SlidingDictionaryWindowSize then begin
       if not Flush(W) then begin
        result:=StatusWriteErr;
        exit;
       end;
       W:=0;
       U:=0;
      end;
     until N=0;
    end;
   end;
   if UserAbort then begin
    result:=StatusUserAbort;
   end else if not Flush(W) then begin
    result:=StatusWriteErr;
   end else if ItIsAtEnd then begin
    result:=StatusReadErr;
   end else begin
    result:=StatusOk;
   end;
  end;

  function ExplodeNoLiteral8k(LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32): TpvInt32;
  var S:TpvInt32;
      E:TpvUInt16;
      N,D:TpvUInt16;
      W:TpvUInt16;
      T:PHuffManTree;
      LMask,DMask:TpvUInt16;
      U:TpvUInt16;
  begin
   BitBuffer:=0;
   BitsInBitBuffer:=0;
   W:=0;
   U:=1;
   LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
   DMask:=MaskBits[DistanceCodeTableLookupBits];
   S:=UncompressedSize;
   while (S>0) and not (UserAbort or ItIsAtEnd) do begin
    NeedBits(1);
    if(BitBuffer and 1)<>0 then begin
     DumpBits(1);
     dec(S);
     NeedBits(8);
     Slide^[W]:=BitBuffer;
     inc(W);
     if W=SlidingDictionaryWindowSize then begin
      if not Flush(W)then begin
       result:=StatusWriteErr;
       exit;
      end;
      W:=0;
      U:=0;
     end;
     DumpBits(8);
    end else begin
     DumpBits(1);
     NeedBits(7);
     D:=BitBuffer and $7f;
     DumpBits(7);
     NeedBits(DistanceCodeTableLookupBits);
     T:=@DistanceCodeTable^[DMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.ExtraBits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     D:=W-D-T^.ByteSize;
     NeedBits(LiteralLengthCodeTableLookupBits);
     T:=@LiteralLengthCodeTable^[LMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then begin
      repeat
       if E=99 then begin
        result:=StatusZipFileErr;
        exit;
       end;
       DumpBits(T^.CodeBits);
       dec(E,16);
       NeedBits(E);
       T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
       E:=T^.ExtraBits;
      until E<=16;
     end;
     DumpBits(T^.CodeBits);
     N:=T^.ByteSize;
     if E<>0 then begin
      NeedBits(8);
      inc(N,BitBuffer and $ff);
      DumpBits(8);
     end;
     dec(S,N);
     repeat
      D:=D and (SlidingDictionaryWindowSize-1);
      if D>W then begin
       E:=SlidingDictionaryWindowSize-D;
      end else begin
       E:=SlidingDictionaryWindowSize-W;
      end;
      if E>N then E:=N;
      dec(N,E);
      if (U<>0) and (W<=D) then begin
       FillChar(Slide^[W],E,#0);
       inc(W,E);
       inc(D,E);
      end else if(W-D>=E)then begin
       Move(Slide^[D],Slide^[W],E);
       inc(W,E);
       inc(D,E);
      end else begin
       repeat
        Slide^[W]:=Slide^[D];
        inc(W);
        inc(D);
        dec(E);
       until E=0;
      end;
      if W=SlidingDictionaryWindowSize then begin
       if not Flush(W)then begin
        result:=StatusWriteErr;
        exit;
       end;
       W:=0;
       U:=0;
      end;
     until N=0;
    end;
   end;
   if UserAbort then begin
    result:=StatusUserAbort;
   end else if not Flush(W) then begin
    result:=StatusWriteErr;
   end else if ItIsAtEnd then begin
    result:=StatusReadErr;
   end else begin
    result:=StatusOk;
   end;
  end;

  function ExplodeNoLiteral4k(LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32): TpvInt32;
  var S:TpvInt32;
      E:TpvUInt16;
      N,D:TpvUInt16;
      W:TpvUInt16;
      T:PHuffManTree;
      LMask,DMask:TpvUInt16;
      U:TpvUInt16;
  begin
   BitBuffer:=0;
   BitsInBitBuffer:=0;
   W:=0;
   U:=1;
   LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
   DMask:=MaskBits[DistanceCodeTableLookupBits];
   S:=UncompressedSize;
   while (S>0) and not (UserAbort or ItIsAtEnd) do begin
    NeedBits(1);
    if(BitBuffer and 1)<>0 then begin
     DumpBits(1);
     dec(S);
     NeedBits(8);
     Slide^[W]:=BitBuffer;
     inc(W);
     if W=SlidingDictionaryWindowSize then begin
      if not Flush(W) then begin
       result:=StatusWriteErr;
       exit;
      end;
      W:=0;
      U:=0;
     end;
     DumpBits(8);
    end else begin
     DumpBits(1);
     NeedBits(6);
     D:=BitBuffer and $3f;
     DumpBits(6);
     NeedBits(DistanceCodeTableLookupBits);
     T:=@DistanceCodeTable^[DMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then repeat
      if E=99 then begin
       result:=StatusZipFileErr;
       exit;
      end;
      DumpBits(T^.CodeBits);
      dec(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
      E:=T^.ExtraBits;
     until E<=16;
     DumpBits(T^.CodeBits);
     D:=W-D-T^.ByteSize;
     NeedBits(LiteralLengthCodeTableLookupBits);
     T:=@LiteralLengthCodeTable^[LMask and not BitBuffer];
     E:=T^.ExtraBits;
     if E>16 then repeat
      if E=99 then begin
       result:=StatusZipFileErr;
       exit;
      end;
      DumpBits(T^.CodeBits);
      dec(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] and not BitBuffer];
      E:=T^.ExtraBits;
     until E<=16;
     DumpBits(T^.CodeBits);
     N:=T^.ByteSize;
     if E<>0 then begin
      NeedBits(8);
      inc(N,BitBuffer and $ff);
      DumpBits(8);
     end;
     dec(S,N);
     repeat
      D:=D and (SlidingDictionaryWindowSize-1);
      if D>W then begin
       E:=SlidingDictionaryWindowSize-D;
      end else begin
       E:=SlidingDictionaryWindowSize-W;
      end;
      if E>N then E:=N;
      dec(N,E);
      if (U<>0) and (W<=D) then begin
       FillChar(Slide^[W],E,#0);
       inc(W,E);
       inc(D,E);
      end else if (W-D>=E) then begin
       Move(Slide^[D],Slide^[W],E);
       inc(W,E);
       inc(D,E);
      end else begin
       repeat
        Slide^[W]:=Slide^[D];
        inc(W);
        inc(D);
        dec(E);
       until E=0;
      end;
      if W=SlidingDictionaryWindowSize then begin
       if not Flush(W) then begin
        result:=StatusWriteErr;
        exit;
       end;
       W:=0;
       U:=0;
      end;
     until N=0;
    end;
   end;
   if UserAbort then begin
    result:=StatusUserAbort;
   end else if not Flush(W) then begin
    result:=StatusWriteErr;
   end else if ItIsAtEnd then begin
    result:=StatusReadErr;
   end else begin
    result:=StatusOk;
   end;
  end;

  function Explode:TpvInt32;
  var BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;
      BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:TpvInt32;
      TreeTable:array[0..255] of TpvUInt16;
  begin
   InputBufferPosition:=0;
   FilePosition:=-1;
   LiteralLengthCodeTableLookupBits:=7;
   if CompressedSize>200000 then begin
    DistanceCodeTableLookupBits:=8;
   end else begin
    DistanceCodeTableLookupBits:=7;
   end;
   if (BitsFlagsType and 4)<>0 then begin
    BitLengthCodeTableLookupBits:=9;
    result:=GetTree(@TreeTable[0],256);
    if result<>0 then begin
     result:=StatusZipFileErr;
     exit;
    end;
    result:=HuffManTreeBuild(pword(@TreeTable),256,256,nil,nil,PPHuffManTree(@BitLengthCodeTable),BitLengthCodeTableLookupBits);
    if result<>0 then begin
     if result=HuffmanTreeIncomplete then begin
      HuffManTreeFree(BitLengthCodeTable);
     end;
     result:=StatusZipFileErr;
     exit;
    end;
    result:=GetTree(@TreeTable[0],64);
    if result<>0 then begin
     HuffManTreeFree(BitLengthCodeTable);
     result:=StatusZipFileErr;
     exit;
    end;
    result:=HuffManTreeBuild(pword(@TreeTable),64,0,PUSBList(@CopyLength3),PUSBList(@ExtraBitsTable),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits);
    if result<>0 then begin
     if result=HuffmanTreeIncomplete then begin
      HuffManTreeFree(LiteralLengthCodeTable);
     end;
     HuffManTreeFree(BitLengthCodeTable);
     result:=StatusZipFileErr;
     exit;
    end;
    result:=GetTree(@TreeTable[0],64);
    if result<>0 then begin
     HuffManTreeFree(BitLengthCodeTable);
     HuffManTreeFree(LiteralLengthCodeTable);
     result:=StatusZipFileErr;
     exit;
    end;
    if (BitsFlagsType and 2)<>0 then begin
     result:=HuffManTreeBuild(pword(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes8),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
     if result<>0 then begin
      if result=HuffmanTreeIncomplete then begin
       HuffManTreeFree(DistanceCodeTable);
      end;
      HuffManTreeFree(BitLengthCodeTable);
      HuffManTreeFree(LiteralLengthCodeTable);
      result:=StatusZipFileErr;
      exit;
     end;
     result:=ExplodeLiteral8k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable,BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
    end else begin
     result:=HuffManTreeBuild(pword(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes4),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
     if result<>0 then begin
      if result=HuffmanTreeIncomplete then begin
       HuffManTreeFree(DistanceCodeTable);
      end;
      HuffManTreeFree(BitLengthCodeTable);
      HuffManTreeFree(LiteralLengthCodeTable);
      result:=StatusZipFileErr;
      exit;
     end;
     result:=ExplodeLiteral4k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable,BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
    end;
    HuffManTreeFree(DistanceCodeTable);
    HuffManTreeFree(LiteralLengthCodeTable);
    HuffManTreeFree(BitLengthCodeTable);
   end else begin
    result:=GetTree(@TreeTable[0],64);
    if result<>0 then begin
     result:=StatusZipFileErr;
     exit;
    end;
    result:=HuffManTreeBuild(pword(@TreeTable),64,0,PUSBList(@CopyLength2),PUSBList(@ExtraBitsTable),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits);
    if result<>0 then begin
     if result=HuffmanTreeIncomplete then begin
      HuffManTreeFree(LiteralLengthCodeTable);
     end;
     result:=StatusZipFileErr;
     exit;
    end;
    result:=GetTree(@TreeTable[0],64);
    if result<>0 then begin
     HuffManTreeFree(LiteralLengthCodeTable);
     result:=StatusZipFileErr;
     exit;
    end;
    if (BitsFlagsType and 2)<>0 then begin
     result:=HuffManTreeBuild(pword(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes8),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
     if result<>0 then begin
      if result=HuffmanTreeIncomplete then begin
       HuffManTreeFree(DistanceCodeTable);
      end;
      HuffManTreeFree(LiteralLengthCodeTable);
      result:=StatusZipFileErr;
      exit;
     end;
     result:=ExplodeNoLiteral8k(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
    end else begin
     result:=HuffManTreeBuild(pword(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes4),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
     if result<>0 then begin
      if result=HuffmanTreeIncomplete then begin
       HuffManTreeFree(DistanceCodeTable);
      end;
      HuffManTreeFree(LiteralLengthCodeTable);
      result:=StatusZipFileErr;
      exit;
     end;
     result:=ExplodeNoLiteral4k(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
    end;
    HuffManTreeFree(DistanceCodeTable);
    HuffManTreeFree(LiteralLengthCodeTable);
   end;
  end;

  function WriteChar(C:TpvUInt8):boolean;
  begin
   result:=OutStream.Write(C,SizeOf(TpvUInt8))=SizeOf(TpvUInt8);
   CRC32.Update(C,SizeOf(TpvUInt8));
  end;

  procedure ClearLeafNodes;
  var PreviousCodeValue:TpvInt32;
      index:TpvInt32;
      MaxActualCode:TpvInt32;
      CurrentPreviousCodeTrie:PPreviousCodeTrie;
  begin
   CurrentPreviousCodeTrie:=PreviousCode;
   MaxActualCode:=NextFreeCodeInTrie-1;
   for index:=257 to MaxActualCode do begin
    CurrentPreviousCodeTrie^[index]:=CurrentPreviousCodeTrie^[index] or $8000;
   end;
   for index:=257 to MaxActualCode do begin
    PreviousCodeValue:=CurrentPreviousCodeTrie^[index] and not $8000;
    if PreviousCodeValue>256 then begin
     CurrentPreviousCodeTrie^[PreviousCodeValue]:=CurrentPreviousCodeTrie^[PreviousCodeValue] and not $8000;
    end;
   end;
   PreviousCodeValue:=-1;
   NextFreeCodeInTrie:=-1;
   for index:=257 to MaxActualCode do begin
    if (CurrentPreviousCodeTrie^[index] and $c000)<>0 then begin
     if PreviousCodeValue<>-1 then begin
      CurrentPreviousCodeTrie^[PreviousCodeValue]:=-index;
     end else begin
      NextFreeCodeInTrie:=index;
     end;
     PreviousCodeValue:=index;
    end;
   end;
   if PreviousCodeValue<>-1 then begin
    CurrentPreviousCodeTrie^[PreviousCodeValue]:=-MaxActualCode-1;
   end;
  end;

  function Unshrink:TpvInt32;
   function DoIt:TpvInt32;
   var InputCode:TpvInt32;
       LastInputCode:TpvInt32;
       LastOutputCode:TpvUInt8;
       ActualCodeSize:TpvUInt8;
       StackPtr:TpvInt32;
       NewCode:TpvInt32;
       CodeMask:TpvInt32;
       index:TpvInt32;
       BitsToRead:TpvInt32;
   begin
    InputBufferPosition:=0;
    FilePosition:=-1;

    SlideWindowPosition:=0;
    BitsInBitBuffer:=0;
    BitBuffer:=0;

    FillChar(PreviousCode^,SizeOf(TPreviousCodeTrie),#0);
    FillChar(ActualCode^,SizeOf(TActualCodeTrie),#0);
    FillChar(Stack^,SizeOf(TStack),#0);

    for index:=257 to MaxCode do begin
     PreviousCode^[index]:=-(index+1);
    end;
    NextFreeCodeInTrie:=257;
    StackPtr:=MaxStack;
    ActualCodeSize:=InitialCodeSize;
    CodeMask:=MaskBits[ActualCodeSize];

    NeedBits(ActualCodeSize);
    InputCode:=BitBuffer and CodeMask;
    DumpBits(ActualCodeSize);

    LastInputCode:=InputCode;
    LastOutputCode:=InputCode and $ff;

    if not WriteChar(LastOutputCode) then begin
     result:=StatuswriteErr;
     exit;
    end;

    BitsToRead:=(8*CompressedSize)-ActualCodeSize;

    while (BitsToRead>=ActualCodeSize) and not UserAbort do begin
     NeedBits(ActualCodeSize);
     InputCode:=BitBuffer and CodeMask;
     DumpBits(ActualCodeSize);
     dec(BitsToRead,ActualCodeSize);
     if InputCode=256 then begin
      NeedBits(ActualCodeSize);
      InputCode:=BitBuffer and CodeMask;
      DumpBits(ActualCodeSize);
      dec(BitsToRead,ActualCodeSize);
      case InputCode of
       1:begin
        inc(ActualCodeSize);
        if ActualCodeSize>FinalCodeSize then begin
         result:=StatusZipFileErr;
         exit;
        end;
        CodeMask:=MaskBits[ActualCodeSize];
       end;
       2:begin
        ClearLeafNodes;
       end;
       else begin
        result:=StatusZipFileErr;
        exit;
       end;
      end;
     end else begin
      NewCode:=InputCode;
      if InputCode<256 then begin
       LastOutputCode:=InputCode and $ff;
       if not WriteChar(LastOutputCode) then begin
        result:=StatusWriteErr;
        exit;
       end;
      end else begin
       if PreviousCode^[InputCode]<0 then begin
        Stack^[StackPtr]:=LastOutputCode;
        dec(StackPtr);
        InputCode:=LastInputCode;
       end;
       while InputCode>256 do begin
        Stack^[StackPtr]:=ActualCode^[InputCode];
        dec(StackPtr);
        InputCode:=PreviousCode^[InputCode];
       end;
       LastOutputCode:=InputCode and $ff;
       if not WriteChar(LastOutputCode) then begin
        result:=StatusWriteErr;
        exit;
       end;
       for index:=StackPtr+1 to MaxStack do begin
        if not WriteChar(Stack^[index]) then begin
         result:=StatusWriteErr;
         exit;
        end;
       end;
       StackPtr:=MaxStack;
      end;
      InputCode:=NextFreeCodeInTrie;
      if InputCode<=MaxCode then begin
       NextFreeCodeInTrie:=-PreviousCode^[InputCode];
       PreviousCode^[InputCode]:=LastInputCode;
       ActualCode^[InputCode]:=LastOutputCode;
      end;
      LastInputCode:=NewCode;
     end;
    end;
    if UserAbort then begin
     result:=StatusUserAbort;
    end else begin
     result:=StatusOk;
    end;
   end;
  begin
   if CompressedSize=MAXLONGINT then begin
    result:=StatusNotSupported;
    exit;
   end;
   New(PreviousCode);
   New(ActualCode);
   New(Stack);
   try
    result:=DoIt;
   finally
    Dispose(PreviousCode);
    Dispose(ActualCode);
    Dispose(Stack);
   end;
  end;

  function Unreduce(InputFactor:TpvInt32):TpvInt32;
  const DLE=144;
  type PFArray=^TFArray;
       TFArray=array[0..63] of TpvUInt8;
       PFollowers=^TFollowers;
       TFollowers=array[0..255] of TFArray;
       PSlen=^TSlen;
       TSlen=array[0..255] of TpvUInt8;
  const L_table:array[0..4] of TpvInt32=($00,$7f,$3f,$1f,$0f);
        D_shift:array[0..4] of TpvInt32=($00,$07,$06,$05,$04);
        D_mask:array[0..4] of TpvInt32=($00,$01,$03,$07,$0f);
        B_table:array[0..255] of TpvInt32=(8,1,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,
                                          5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,
                                          6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                          6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,
                                          7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                          7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                          7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                          7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                          8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                          8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                          8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                          8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                          8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                          8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                          8,8,8,8);
  var Slen:PSLen;
      Followers:PFollowers;
   function DoIt:TpvInt32;
   var lchar,nchar,ExState,v,Len,s,u,Factor,Follower,BitsNeeded,x,i:TpvInt32;
       e,n,d:TpvUInt32;
   begin
    u:=1;
    v:=0;
    lchar:=0;
    Len:=0;
    ExState:=0;
    Factor:=InputFactor;
    for x:=255 downto 0 do begin
     Slen^[x]:=ReadBits(6);
     for i:=0 to Slen^[x]-1 do begin
      Followers^[x,i]:=ReadBits(8);
     end;
    end;
    SlideWindowPosition:=0;
    S:=UncompressedSize;
    while (S>0) and not (UserAbort or ItIsAtEnd) do begin
     if Slen^[lchar]=0 then begin
      nchar:=ReadBits(8);
     end else begin
      nchar:=ReadBits(1);
      if nchar<>0 then begin
       nchar:=ReadBits(8);
      end else begin
       BitsNeeded:=B_table[Slen^[lchar]];
       Follower:=ReadBits(BitsNeeded);
       nchar:=Followers[lchar,follower];
      end;
     end;
     case ExState of
      0:begin
       if nchar<>DLE then begin
        dec(s);
        Slide^[SlideWindowPosition]:=TpvUInt8(nchar);
        inc(SlideWindowPosition);
        if SlideWindowPosition=$4000 then begin
         if not Flush(SlideWindowPosition) then begin
          result:=StatusWriteErr;
          exit;
         end;
         SlideWindowPosition:=0;
         u:=0;
        end;
       end else begin
        ExState:=1;
       end;
      end;
      1:begin
       if nchar<>0 then begin
        v:=nchar;
        Len:=v and L_table[Factor];
        if Len=L_table[Factor] then begin
         ExState:=2;
        end else begin
         ExState:=3;
        end;
       end else begin
        dec(s);
        Slide^[SlideWindowPosition]:=TpvUInt8(DLE);
        inc(SlideWindowPosition);
        if SlideWindowPosition=$4000 then begin
         if not Flush(SlideWindowPosition) then begin
          result:=StatusWriteErr;
          exit;
         end;
         SlideWindowPosition:=0;
         u:=0;
        end;
        ExState:=0;
       end;
      end;
      2:begin
       inc(Len,nchar);
       ExState:=3;
      end;
      3:begin
       n:=Len+3;
       d:=SlideWindowPosition-(((((v shr D_shift[Factor]) and D_mask[Factor]) shl 8)+nchar)+1);
       dec(s,n);
       repeat
        d:=d and $3fff;
        if d>TpvUInt32(SlideWindowPosition) then begin
         e:=d;
        end else begin
         e:=SlideWindowPosition;
        end;
        e:=$4000-e;
        if e>n then begin
         e:=n;
        end;
        dec(n,e);
        if (u<>0) and (SlideWindowPosition<=d) then begin
         FillChar(Slide^[SlideWindowPosition],e,#0);
         inc(SlideWindowPosition,e);
         inc(d,e);
        end else if (TpvUInt32(SlideWindowPosition)-d)<e then begin
         repeat
          Slide^[SlideWindowPosition]:=Slide^[d];
          inc(SlideWindowPosition);
          inc(d);
          dec(e);
         until e=0;
        end else begin
         Move(Slide^[d],Slide^[SlideWindowPosition],e);
         inc(SlideWindowPosition,e);
         inc(d,e);
        end;
        if SlideWindowPosition=$4000 then begin
         if not Flush(SlideWindowPosition) then begin
          result:=StatusWriteErr;
          exit;
         end;
         SlideWindowPosition:=0;
         u:=0;
        end;
       until n=0;
       ExState:=0;
      end;
     end;
     lchar:=nchar;
    end;
    if UserAbort then begin
     result:=StatusUserAbort;
    end else if s=0 then begin
     result:=StatusOk;
    end else begin
     result:=StatusReadErr;
    end;
   end;
  begin
   if CompressedSize=MAXLONGINT then begin
    result:=StatusNotSupported;
    exit;
   end;
   New(Slen);
   New(Followers);
   try
    result:=DoIt;
   finally
    Dispose(Slen);
    Dispose(Followers);
   end;
  end;

  function DoDecompress(InStream,OutStream:TStream):TpvInt32;
  begin

   GetMem(Slide,SlidingDictionaryWindowSize);
   FillChar(Slide^,SlidingDictionaryWindowSize,#0);
   try

    ReachedSize:=0;

    UserAbort:=false;
    ItIsAtEnd:=false;

    BitsFlagsType:=LocalFileHeader.BitFlags;

    case LocalFileHeader.CompressMethod of
     0:begin
      result:=CopyStored;
     end;
     1:begin
      result:=Unshrink;
     end;
     2..5:begin
      result:=Unreduce(LocalFileHeader.CompressMethod-1);
     end;
     6:begin
      result:=Explode;
     end;
     8:begin
      result:=Inflate;
     end;
     else begin
      result:=StatusNotSupported;
     end;
    end;

    if (result=Statusok) and ((BitsFlagsType and 8)<>0) then begin
     DumpBits(BitsInBitBuffer and 7);
     NeedBits(16);

     DumpBits(16);
     NeedBits(16);
     OriginalCRC:=(BitBuffer and $ffff) shl 16;
     DumpBits(16);
    end;

   finally
    FreeMem(Slide);
   end;

  end;

 begin
  result:=DoDecompress(InStream,OutStream)=StatusOk;
 end;
 procedure Inflate;
 var InData:TpvPointer;
     DestData:TpvPointer;
     DestLen:TpvSizeUInt;
 begin
  if CompressedSize>0 then begin
   DestData:=nil;
   DestLen:=0;
   try
    GetMem(InData,CompressedSize);
    try
     fSourceArchive.fStream.ReadBuffer(InData^,CompressedSize);
     DoInflate(InData,CompressedSize,DestData,DestLen,false);
    finally
     FreeMem(InData);
    end;
    if DestLen>0 then begin
     CRC32.Update(DestData^,DestLen);
     aStream.WriteBuffer(DestData^,DestLen);
    end;
   finally
    if assigned(DestData) then begin
     FreeMem(DestData);
    end;
   end;
  end;
 end;
begin
 if assigned(aStream) then begin

  if aStream is TMemoryStream then begin
   (aStream as TMemoryStream).Clear;
  end else if (aStream is TFileStream) and ((aStream as TFileStream).Size>0) then begin
   (aStream as TFileStream).Size:=0;
  end;

  if assigned(fSourceArchive) then begin

   if fSourceArchive.fStream.Seek(fHeaderPosition,soBeginning)<>fHeaderPosition then begin
    raise EpvArchiveZIP.Create('Seek error');
   end;

   fSourceArchive.fStream.ReadBuffer(LocalFileHeader,SizeOf(TpvArchiveZIPLocalFileHeader));

   if LocalFileHeader.Signature.Value<>TpvArchiveZIPHeaderSignatures.LocalFileHeaderSignature.Value then begin
    raise EpvArchiveZIP.Create('Invalid or corrupt ZIP archive');
   end;

   LocalFileHeader.SwapEndiannessIfNeeded;

   Offset:=fSourceArchive.fStream.Position+LocalFileHeader.FileNameLength+LocalFileHeader.ExtraFieldLength;

   if (LocalFileHeader.BitFlags and 8)=0 then begin
    CompressedSize:=LocalFileHeader.CompressedSize;
    UncompressedSize:=LocalFileHeader.UncompressedSize;
    OriginalCRC:=LocalFileHeader.CRC32;
   end else begin
    CompressedSize:=High(TpvInt64);
    UncompressedSize:=High(TpvInt64);
    OriginalCRC:=0;
   end;

   if LocalFileHeader.ExtraFieldLength>=SizeOf(TpvArchiveZIPExtensibleDataFieldHeader) then begin
    StartPosition:=fSourceArchive.fStream.Position;
    while (fSourceArchive.fStream.Position+(SizeOf(ExtensibleDataFieldHeader)-1))<(StartPosition+LocalFileHeader.ExtraFieldLength) do begin
     fSourceArchive.fStream.ReadBuffer(ExtensibleDataFieldHeader,SizeOf(TpvArchiveZIPExtensibleDataFieldHeader));
     ExtensibleDataFieldHeader.SwapEndiannessIfNeeded;
     From:=fSourceArchive.fStream.Position;
     case ExtensibleDataFieldHeader.HeaderID of
      $0001:begin
       fSourceArchive.fStream.ReadBuffer(ExtensibleInfoFieldHeader,SizeOf(TpvArchiveZIP64ExtensibleInfoFieldHeader));
       ExtensibleInfoFieldHeader.SwapEndiannessIfNeeded;
       if (LocalFileHeader.BitFlags and 8)=0 then begin
        CompressedSize:=ExtensibleInfoFieldHeader.CompressedSize;
        UncompressedSize:=ExtensibleInfoFieldHeader.OriginalSize;
       end;
      end;
      $7075:begin
      end;
     end;
     if fSourceArchive.fStream.Seek(From+ExtensibleDataFieldHeader.DataSize,soBeginning)<>From+ExtensibleDataFieldHeader.DataSize then begin
      raise EpvArchiveZIP.Create('Seek error');
     end;
    end;
   end;

   if (LocalFileHeader.BitFlags and 1)<>0 then begin
    raise EpvArchiveZIP.Create('Encrypted ZIP archive');
   end;

   if (LocalFileHeader.BitFlags and 32)<>0 then begin
    raise EpvArchiveZIP.Create('Patched ZIP archive');
   end;

   if fSourceArchive.fStream.Seek(Offset,soBeginning)<>Offset then begin
    raise EpvArchiveZIP.Create('Seek error');
   end;

   if CompressedSize>0 then begin

    CRC32.Initialize;

    case LocalFileHeader.CompressMethod of
     0:begin
      if aStream.CopyFrom(fSourceArchive.fStream,CompressedSize)<>CompressedSize then begin
       raise EpvArchiveZIP.Create('Read error');
      end;
      CRC32.Update(aStream);
     end;
     1..6:begin
      if not Decompress(fSourceArchive.fStream,aStream) then begin
       raise EpvArchiveZIP.Create('Decompression failed');
      end;
     end;
     8:begin
      if (CompressedSize<high(TpvUInt16)) and
         (UncompressedSize<high(TpvUInt16)) then begin
       Inflate;
      end else begin
       if not Decompress(fSourceArchive.fStream,aStream) then begin
        raise EpvArchiveZIP.Create('Decompression failed');
       end;
      end;
     end;
     else
     begin
      raise EpvArchiveZIP.Create('Non-supported compression method');
     end;
    end;

    if (aStream.Size<>0) and (CRC32.Finalize<>OriginalCRC) then begin
     raise EpvArchiveZIP.Create('Checksum mismatch');
    end;

   end;

  end else if assigned(fStream) then begin
   fStream.Seek(0,soBeginning);
   aStream.CopyFrom(fStream,fStream.Size);
  end;

 end;

end;

procedure TpvArchiveZIPEntry.SaveToFile(const aFileName:string);
var Stream:TStream;
begin
 Stream:=TFileStream.Create(aFileName,fmCreate);
 try
  SaveToStream(Stream);
 finally
  Stream.Free;
 end;
end;

constructor TpvArchiveZIPEntries.Create;
begin
 inherited Create(TpvArchiveZIPEntry);
 fFileNameHashMap:=TpvArchiveZIPEntriesFileNameHashMap.Create(nil);
end;

destructor TpvArchiveZIPEntries.Destroy;
begin
 Clear;
 FreeAndNil(fFileNameHashMap);
 inherited Destroy;
end;

function TpvArchiveZIPEntries.GetEntry(const aIndex:TpvSizeInt):TpvArchiveZIPEntry;
begin
 result:=inherited Items[aIndex] as TpvArchiveZIPEntry;
end;

procedure TpvArchiveZIPEntries.SetEntry(const aIndex:TpvSizeInt;const aEntry:TpvArchiveZIPEntry);
begin
 inherited Items[aIndex]:=aEntry;
end;

function TpvArchiveZIPEntries.Add(const aFileName:TpvRawByteString):TpvArchiveZIPEntry;
begin
 result:=TpvArchiveZIPEntry.Create(self);
 result.SetFileName(aFileName);
end;

function TpvArchiveZIPEntries.Find(const aFileName:TpvRawByteString):TpvArchiveZIPEntry;
begin
 result:=fFileNameHashMap[TpvArchiveZIP.CorrectPath(aFileName)];
end;

constructor TpvArchiveZIP.Create;
begin
 inherited Create;
 fEntries:=TpvArchiveZIPEntries.Create;
 fStream:=nil;
end;

destructor TpvArchiveZIP.Destroy;
begin
 FreeAndNil(fEntries);
 //FreeAndNil(fStream);
 inherited Destroy;
end;

class function TpvArchiveZIP.CorrectPath(const aFileName:TpvRawByteString):TpvRawByteString;
var Index:TpvSizeInt;
begin
 result:=aFileName;
 for Index:=1 to length(result) do begin
  case result[Index] of
   '\':begin
    result[Index]:='/';
   end;
  end;
 end;
end;

procedure TpvArchiveZIP.Clear;
begin
 FreeAndNil(fStream);
 fEntries.Clear;
end;

procedure TpvArchiveZIP.LoadFromStream(const aStream:TStream);
var LocalFileHeader:TpvArchiveZIPLocalFileHeader;
    EndCentralFileHeader:TpvArchiveZIPEndCentralFileHeader;
    CentralFileHeader:TpvArchiveZIPCentralFileHeader;
    Size,CentralFileHeaderOffset:TpvInt64;
    Index,Count,FileCount:TpvSizeInt;
    FileName:TpvRawByteString;
    OK:boolean;
    FileEntry:TpvArchiveZIPEntry;
begin

 Clear;
 fStream:=aStream;

 Size:=fStream.Size;

 OK:=false;

 fStream.Seek(0,soFromBeginning);

 fStream.ReadBuffer(LocalFileHeader,SizeOf(TpvArchiveZIPLocalFileHeader));

 if LocalFileHeader.Signature.Value=TpvArchiveZIPHeaderSignatures.LocalFileHeaderSignature.Value then begin
  for Index:=Size-SizeOf(TpvArchiveZipEndCentralFileHeader) downto Size-(65535+SizeOf(TpvArchiveZipEndCentralFileHeader)) do begin
   if Index<0 then begin
    break;
   end else begin
    fStream.Seek(Index,soFromBeginning);
    if fStream.Read(EndCentralFileHeader,SizeOf(TpvArchiveZipEndCentralFileHeader))=SizeOf(TpvArchiveZipEndCentralFileHeader) then begin
     if EndCentralFileHeader.Signature.Value=TpvArchiveZIPHeaderSignatures.EndCentralFileHeaderSignature.Value then begin
      EndCentralFileHeader.SwapEndiannessIfNeeded;
      OK:=true;
      break;
     end;
    end;
   end;
  end;
 end;

 if not OK then begin
  raise EpvArchiveZIP.Create('Invalid ZIP archive');
 end;

 Count:=0;

 fStream.Seek(EndCentralFileHeader.StartDiskOffset,soFromBeginning);

 repeat

  CentralFileHeaderOffset:=fStream.Position;

  if fStream.Read(CentralFileHeader,SizeOf(TpvArchiveZIPCentralFileHeader))<>SizeOf(TpvArchiveZIPCentralFileHeader) then begin
   break;
  end;

  if CentralFileHeader.Signature.Value<>TpvArchiveZIPHeaderSignatures.CentralFileHeaderSignature.Value then begin
   break;
  end;

  CentralFileHeader.SwapEndiannessIfNeeded;

  SetLength(FileName,CentralFileHeader.FileNameLength);
  if CentralFileHeader.FileNameLength>0 then begin
   if fStream.Read(FileName[1],CentralFileHeader.FileNameLength)<>CentralFileHeader.FileNameLength then begin
    break;
   end;
  end;

  if Count<EndCentralFileHeader.EntriesThisDisk then begin

   fStream.Seek(CentralFileHeader.ExtraFieldLength+CentralFileHeader.FileCommentLength,soFromCurrent);

   if length(FileName)>0 then begin
    FileEntry:=fEntries.Add(FileName);
    FileEntry.fCentralHeaderPosition:=CentralFileHeaderOffset;
    FileEntry.fHeaderPosition:=CentralFileHeader.LocalFileHeaderOffset;
    FileEntry.Size:=CentralFileHeader.UncompressedSize;
    FileEntry.fSourceArchive:=self;
   end;

   inc(Count);

  end else begin

   Count:=not EndCentralFileHeader.EntriesThisDisk;
   break;

  end;

 until false;

 if EndCentralFileHeader.EntriesThisDisk<>Count then begin
  raise EpvArchiveZIP.Create('Invalid ZIP archive');
 end;

end;

procedure TpvArchiveZIP.LoadFromFile(const aFileName:string);
var Stream:TStream;
begin
 Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(Stream);
 finally
 end;
end;

procedure TpvArchiveZIP.SaveToStream(const aStream:TStream);
 function CompressShrink(InStream,OutStream:TStream):boolean;
 const BufSize=10240;
       MINBITS=9;
       MAXBITS=13;
       TABLESIZE=8191;
       SPECIAL=256;
       INCSIZE=1;
       CLEARCODE=2;
       FIRSTENTRY=257;
       UNUSED=-1;
       STDATTR=$23;
 type TCodeTableItem=record
       Child:TpvInt32;
       Sibling:TpvInt32;
       Suffix:TpvUInt8;
      end;

      PCodeTable=^TCodeTable;
      TCodeTable=array[0..TABLESIZE] of TCodeTableItem;

      PFreeList=^TFreeList;
      TFreeList=array[FIRSTENTRY..TABLESIZE] of longword;

      PClearList=^TClearList;
      TClearList=array[0..1023] of TpvUInt8;

 var FirstByte:boolean;
     TableFull:boolean;
     SaveByte:TpvUInt8;
     BitsUsed:TpvUInt8;
     CodeSize:TpvUInt8;
     MaxCode:TpvUInt16;
     CodeTable:PCodeTable;
     FreeList:PFreeList;
     ClearList:PClearList;
     NextFree:TpvUInt16;
     LastCode:TpvInt32;

  procedure Prune(Parent:TpvUInt16);
  var CurrentChild,NextSibling:TpvInt32;
  begin
   CurrentChild:=CodeTable^[Parent].Child;
   while (CurrentChild>=0) and (CodeTable^[CurrentChild].Child<0) do begin
    CodeTable^[Parent].Child:=CodeTable^[CurrentChild].Sibling;
    CodeTable^[CurrentChild].Sibling:=-1;
    ClearList^[CurrentChild shr 3]:=(ClearList^[CurrentChild shr 3] or (1 shl (CurrentChild and 7)));
    CurrentChild:=CodeTable^[Parent].Child;
   end;
   if CurrentChild>=0 then begin
    Prune(CurrentChild);
    NextSibling:=CodeTable^[CurrentChild].Sibling;
    while NextSibling>=0 do begin
     if CodeTable^[NextSibling].Child<0 then begin
      CodeTable^[CurrentChild].Sibling:=CodeTable^[NextSibling].Sibling;
      CodeTable^[NextSibling].Sibling:=-1;
      ClearList^[NextSibling shr 3]:=(ClearList^[NextSibling shr 3] or (1 shl (NextSibling and 7)));
      NextSibling:=CodeTable^[CurrentChild].Sibling;
     end else begin
      CurrentChild:=NextSibling;
      Prune(CurrentChild);
      NextSibling:=CodeTable^[CurrentChild].Sibling;
     end;
    end;
   end;
  end;

  procedure TableClear;
  var Node:TpvUInt16;
  begin
   FillChar(ClearList^,sizeof(TClearList),#0);
   for Node:=0 to 255 do begin
    Prune(Node);
   end;
   NextFree:=TABLESIZE+1;
   for Node:=TABLESIZE downto FIRSTENTRY do begin
    if (ClearList^[Node shr 3] and (1 shl (Node and 7)))<>0 then begin
     dec(NextFree);
     FreeList^[NextFree]:=Node;
    end;
   end;
   if NextFree<=TABLESIZE then begin
    TableFull:=false;
   end;
  end;

  procedure TableAdd(Prefix:TpvUInt16;Suffix:TpvUInt8);
  var FreeNode:TpvUInt16;
  begin
   if NextFree<=TABLESIZE then begin
    FreeNode:=FreeList^[NextFree];
    inc(NextFree);
    CodeTable^[FreeNode].Child:=-1;
    CodeTable^[FreeNode].Sibling:=-1;
    CodeTable^[FreeNode].Suffix:=Suffix;
    if CodeTable^[Prefix].Child=-1 then begin
     CodeTable^[Prefix].Child:=FreeNode;
    end else begin
     Prefix:=CodeTable^[Prefix].Child;
     while CodeTable^[Prefix].Sibling<>-1 do begin
      Prefix:=CodeTable^[Prefix].Sibling;
     end;
     CodeTable^[Prefix].Sibling:=FreeNode;
    end;
   end;
   if NextFree>TABLESIZE then begin
    TableFull:=true;
   end;
  end;

  function TableLookup(TargetPrefix:TpvInt32;TargetSuffix:TpvUInt8;var FoundAt:TpvInt32):boolean;
  var TempChild:TpvInt32;
  begin
   result:=false;
   FoundAt:=-1;
   if CodeTable^[TargetPrefix].Child=-1 then begin
    exit;
   end;
   TempChild:=CodeTable^[TargetPrefix].Child;
   while true do begin
    with CodeTable^[TempChild] do begin
     if Suffix=TargetSuffix then begin
      FoundAt:=TempChild;
      result:=true;
      break;
     end;
     if Sibling=-1 then begin
      break;
     end;
     TempChild:=Sibling;
    end;
   end;
  end;

  procedure PutByte(Value:TpvUInt8);
  begin
   OutStream.WriteBuffer(Value,sizeof(TpvUInt8));
  end;

  procedure PutCode(Code:smallint);
  var Mask:TpvUInt16;
      Agent,LocalSaveByte,LocalBitsUsed,LocalCodeSize:TpvUInt8;
  begin
   LocalSaveByte:=SaveByte;
   LocalBitsUsed:=BitsUsed;
   LocalCodeSize:=CodeSize;
   if Code=-1 then begin
    if LocalBitsUsed<>0 then begin
     PutByte(LocalSaveByte);
    end;
   end else begin
    Mask:=$0001;
    repeat
     Agent:=0;
     if (Code and Mask)<>0 then begin
      inc(Agent);
     end;
     Mask:=Mask shl 1;
     Agent:=Agent shl LocalBitsUsed;
     inc(LocalBitsUsed);
     LocalSaveByte:=LocalSaveByte or Agent;
     if LocalBitsUsed=8 then begin
      PutByte(LocalSaveByte);
      LocalSaveByte:=0;
      LocalBitsUsed:=0;
     end;
     dec(LocalCodeSize);
    until LocalCodeSize=0;
    SaveByte:=LocalSaveByte;
    BitsUsed:=LocalBitsUsed;
   end;
  end;

  procedure ProcessSuffix(Suffix:TpvInt32);
  var WhereFound:TpvInt32;
  begin
   if FirstByte then begin
    SaveByte:=0;
    BitsUsed:=0;
    CodeSize:=MINBITS;
    MaxCode:=(1 shl CodeSize)-1;
    LastCode:=Suffix;
    FirstByte:=false;
   end else begin
    if Suffix<>-1 then begin
     if TableFull then begin
      PutCode(LastCode);
      PutCode(SPECIAL);
      PutCode(CLEARCODE);
      TableClear;
      TableAdd(LastCode,Suffix);
      LastCode:=Suffix;
     end else begin
      if TableLookup(LastCode,Suffix,WhereFound) then begin
       LastCode:=WhereFound;
      end else begin
       PutCode(LastCode);
       TableAdd(LastCode,Suffix);
       LastCode:=Suffix;
       if (FreeList^[NextFree]>MaxCode) and (CodeSize<MaxBits) then begin
        PutCode(SPECIAL);
        PutCode(INCSIZE);
        inc(CodeSize);
        MaxCode:=(1 shl CodeSize)-1;
       end;
      end;
     end;
    end else begin
     PutCode(LastCode);
     PutCode(-1);
    end;
   end;
  end;

 var Counter:TpvInt32;
     b:TpvUInt8;
 begin
  result:=false;
  New(CodeTable);
  New(FreeList);
  New(ClearList);
  try
   for Counter:=0 to TABLESIZE do begin
    with CodeTable^[Counter] do begin
     Child:=-1;
     Sibling:=-1;
     if Counter<=255 then begin
      Suffix:=Counter;
     end;
    end;
    if Counter>=FIRSTENTRY then begin
     FreeList^[Counter]:=Counter;
    end;
   end;
   NextFree:=FIRSTENTRY;
   TableFull:=false;
   FirstByte:=true;
   LastCode:=0;
   InStream.Seek(0,soBeginning);
   for Counter:=1 to InStream.Size do begin
    InStream.ReadBuffer(b,SizeOf(TpvUInt8));
    ProcessSuffix(b);
   end;
   ProcessSuffix(-1);
   result:=true;
  finally
   Dispose(CodeTable);
   Dispose(FreeList);
   Dispose(ClearList);
  end;
 end;
var LocalFileHeader:TpvArchiveZIPLocalFileHeader;
    CentralFileHeader:TpvArchiveZIPCentralFileHeader;
    EndCentralFileHeader:TpvArchiveZIPEndCentralFileHeader;
    Counter:TpvSizeInt;
    CompressedStream:TStream;
    LocalFile:TpvArchiveZIPEntry;
    Entries:TpvSizeInt;
    StartDiskOffset:TpvInt64;
    CentralFileDirectorySize:TpvInt64;
    CRC32:TpvArchiveZIPCRC32;
    InData:TpvPointer;
    DestData:TpvPointer;
    DestLen:TpvSizeUInt;
begin

 if fEntries.Count=0 then begin
  exit;
 end;

 if aStream is TMemoryStream then begin
  (aStream as TMemoryStream).Clear;
 end else if (aStream is TFileStream) and ((aStream as TFileStream).Size>0) then begin
  (aStream as TFileStream).Size:=0;
 end;

 for Counter:=0 to fEntries.Count-1 do begin
  LocalFile:=fEntries[Counter];
  if assigned(LocalFile) then begin
   CompressedStream:=TMemoryStream.Create;
   try
    FillChar(LocalFileHeader,SizeOf(TpvArchiveZIPLocalFileHeader),#0);
    LocalFileHeader.Signature:=TpvArchiveZIPHeaderSignatures.LocalFileHeaderSignature;
    LocalFileHeader.ExtractVersion:=10;
    LocalFileHeader.BitFlags:=0;
    if LocalFile.Stream.Size>0 then begin
     if LocalFile.CompressionLevel>1 then begin
      LocalFileHeader.BitFlags:=2;
      LocalFileHeader.CompressMethod:=8;
      if LocalFile.Stream.Size>0 then begin
       DestData:=nil;
       DestLen:=0;
       try
        GetMem(InData,LocalFile.Stream.Size);
        try
         LocalFile.Stream.Seek(0,soBeginning);
         LocalFile.Stream.ReadBuffer(InData^,LocalFile.Stream.Size);
         case LocalFile.CompressionLevel of
          2:begin
           DoDeflate(InData,LocalFile.Stream.Size,DestData,DestLen,TpvDeflateMode.VeryFast,false);
          end;
          3:begin
           DoDeflate(InData,LocalFile.Stream.Size,DestData,DestLen,TpvDeflateMode.Fast,false);
          end;
          4:begin
           DoDeflate(InData,LocalFile.Stream.Size,DestData,DestLen,TpvDeflateMode.Medium,false);
          end;
          else begin
           DoDeflate(InData,LocalFile.Stream.Size,DestData,DestLen,TpvDeflateMode.Slow,false);
          end;
         end;
        finally
         FreeMem(InData);
        end;
        if DestLen>0 then begin
         CompressedStream.WriteBuffer(DestData^,DestLen);
        end;
       finally
        if assigned(DestData) then begin
         FreeMem(DestData);
        end;
       end;
      end;
     end else if (LocalFile.CompressionLevel=1) and CompressShrink(LocalFile.Stream,CompressedStream) then begin
      LocalFileHeader.CompressMethod:=1;
     end else begin
      LocalFileHeader.CompressMethod:=0;
      LocalFile.Stream.Seek(0,soBeginning);
      CompressedStream.CopyFrom(LocalFile.Stream,LocalFile.Stream.Size);
     end;
     CompressedStream.Seek(0,soBeginning);
    end else begin
     LocalFileHeader.CompressMethod:=0;
    end;
    TpvArchiveZIPDateTimeUtils.ConvertDateTimeToZIPDateTime(LocalFile.DateTime,
                                                            LocalFileHeader.Date,
                                                            LocalFileHeader.Time);
    LocalFileHeader.FileNameLength:=length(LocalFile.FileName);
    LocalFileHeader.CompressedSize:=CompressedStream.Size;
    LocalFile.Stream.Seek(0,soBeginning);
    CRC32.Initialize;
    CRC32.Update(LocalFile.Stream);
    LocalFileHeader.CRC32:=CRC32.Finalize;
    LocalFileHeader.UncompressedSize:=LocalFile.Stream.Size;
    LocalFile.fHeaderPosition:=aStream.Position;
    LocalFile.fLocalFileHeader:=LocalFileHeader;
    LocalFileHeader.SwapEndiannessIfNeeded;
    aStream.WriteBuffer(LocalFileHeader,SizeOf(TpvArchiveZIPLocalFileHeader));
    LocalFileHeader.SwapEndiannessIfNeeded;
    if LocalFileHeader.FileNameLength>0 then begin
     aStream.WriteBuffer(LocalFile.FileName[1],LocalFileHeader.FileNameLength);
    end;
    if LocalFileHeader.CompressedSize>0 then begin
     if aStream.CopyFrom(CompressedStream,LocalFileHeader.CompressedSize)<>TpvInt32(LocalFileHeader.CompressedSize) then begin
      raise EpvArchiveZIP.Create('Copy error');
     end;
    end;
   finally
    CompressedStream.Free;
   end;
  end;
 end;

 Entries:=0;
 StartDiskOffset:=aStream.Position;
 CentralFileDirectorySize:=0;
 for Counter:=0 to fEntries.Count-1 do begin
  LocalFile:=fEntries[Counter];
  if assigned(LocalFile) then begin
   FillChar(CentralFileHeader,SizeOf(TpvArchiveZIPCentralFileHeader),#0);
   CentralFileHeader.Signature:=TpvArchiveZIPHeaderSignatures.CentralFileHeaderSignature;
   CentralFileHeader.CreatorVersion:=LocalFile.fLocalFileHeader.ExtractVersion;
   CentralFileHeader.ExtractVersion:=LocalFile.fLocalFileHeader.ExtractVersion;
   CentralFileHeader.BitFlags:=LocalFile.fLocalFileHeader.BitFlags;
   CentralFileHeader.CompressMethod:=LocalFile.fLocalFileHeader.CompressMethod;
   CentralFileHeader.Time:=LocalFile.fLocalFileHeader.Time;
   CentralFileHeader.Date:=LocalFile.fLocalFileHeader.Date;
   CentralFileHeader.CRC32:=LocalFile.fLocalFileHeader.CRC32;
   CentralFileHeader.CompressedSize:=LocalFile.fLocalFileHeader.CompressedSize;
   CentralFileHeader.UncompressedSize:=LocalFile.fLocalFileHeader.UncompressedSize;
   CentralFileHeader.FileNameLength:=LocalFile.fLocalFileHeader.FileNameLength;
   CentralFileHeader.ExtraFieldLength:=LocalFile.fLocalFileHeader.ExtraFieldLength;
   CentralFileHeader.ExternalAttrributes:=$20;
   CentralFileHeader.LocalFileHeaderOffset:=LocalFile.fHeaderPosition;
   CentralFileHeader.SwapEndiannessIfNeeded;
   LocalFile.fCentralHeaderPosition:=aStream.Position;
   aStream.WriteBuffer(CentralFileHeader,SizeOf(TpvArchiveZIPCentralFileHeader));
   if CentralFileHeader.FileNameLength>0 then begin
    aStream.WriteBuffer(LocalFile.FileName[1],CentralFileHeader.FileNameLength);
   end;
   inc(Entries);
   inc(CentralFileDirectorySize,SizeOf(TpvArchiveZIPCentralFileHeader)+CentralFileHeader.FileNameLength);
  end;
 end;

 FillChar(EndCentralFileHeader,SizeOf(TpvArchiveZIPEndCentralFileHeader),#0);

 EndCentralFileHeader.Signature:=TpvArchiveZIPHeaderSignatures.EndCentralFileHeaderSignature;
 EndCentralFileHeader.EntriesThisDisk:=Entries;
 EndCentralFileHeader.TotalEntries:=Entries;
 EndCentralFileHeader.StartDiskOffset:=StartDiskOffset;
 EndCentralFileHeader.CentralDirectorySize:=CentralFileDirectorySize;
 aStream.WriteBuffer(EndCentralFileHeader,SizeOf(TpvArchiveZIPEndCentralFileHeader));

end;

procedure TpvArchiveZIP.SaveToFile(const aFileName:string);
var Stream:TStream;
begin
 Stream:=TFileStream.Create(aFileName,fmCreate);
 try
  SaveToStream(Stream);
 finally
  Stream.Free;
 end;
end;

end.
