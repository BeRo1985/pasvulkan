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
unit PasVulkan.Compression;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$rangechecks off}
{$overflowchecks off}

{$scopedenums on}
{$rangechecks off}
{$overflowchecks off}

interface

uses SysUtils,
     Classes,
     Math,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Compression.Deflate,
     PasVulkan.Compression.LZBRS,
     PasVulkan.Compression.LZBRSF,
     PasVulkan.Compression.LZBRSX,
     PasVulkan.Compression.LZBRRC,
     PasVulkan.Compression.LZMA;

type TpvCompressionMethod=
      (
       None=0,
       Deflate=1,
       LZBRSF=2,
       LZBRRC=3,
       LZMA=4,
       LZBRS=5,
       LZBRSX=6
      );

function CompressStream(const aInStream:TStream;const aOutStream:TStream;const aCompressionMethod:TpvCompressionMethod=TpvCompressionMethod.LZBRRC;const aCompressionLevel:TpvUInt32=5;const aParts:TpvUInt32=0):boolean;

function DecompressStream(const aInStream:TStream;const aOutStream:TStream):boolean;

implementation

uses PasVulkan.Application;

type TCompressedFileSignature=array[0..3] of AnsiChar;

     TCompressedFileHeader=packed record
      Signature:TCompressedFileSignature;
      Version:TpvUInt32;
      Flags:TpvUInt8; // Yet unused, but reserved for future use, better to have it now than to break the file format later 
      Parts:TpvUInt8; // max 255 parts, should be enough
      CompressionMethod:TpvUInt8;
      CompressionLevel:TpvUInt8;
      UncompressedSize:TpvUInt64;
      CompressedSize:TpvUInt64;
     end;
     PCompressedFileHeader=^TCompressedFileHeader;

     TCompressionPartHeader=packed record
      CompressedOffset:TpvUInt64;
      CompressedSize:TpvUInt64;
      UncompressedOffset:TpvUInt64;
      UncompressedSize:TpvUInt64;
     end;
     PCompressionPartHeader=^TCompressionPartHeader;

     TCompressionPartHeaders=array of TCompressionPartHeader;

const CompressedFileSignature:TCompressedFileSignature=('C','O','F','I');

      CompressedFileVersion=2;

type TCompressionPartJob=record
      FileHeader:PCompressedFileHeader;
      CompressionPartHeader:PCompressionPartHeader;
      InData:Pointer;
      InSize:TpvUInt64;
      OutData:Pointer;
      OutSize:TpvUInt64;
      Success:boolean;
     end;
     PCompressionPartJob=^TCompressionPartJob;

     TCompressionPartJobs=array of TCompressionPartJob;

     TDecompressionPartJob=record
      FileHeader:PCompressedFileHeader;
      CompressionPartHeader:PCompressionPartHeader;
      InData:Pointer;
      InSize:TpvUInt64;
      OutData:Pointer;
      OutSize:TpvUInt64;
      Success:boolean;
     end;
     PDecompressionPartJob=^TDecompressionPartJob;

     TDecompressionPartJobs=array of TDecompressionPartJob;

procedure CompressPart(const aJob:PCompressionPartJob);
{var Stream:TFileStream;
    FileName:TpvUTF8String;
    TemporaryOutData:Pointer;
    TemporaryOutDataSize:TpvUInt64;//}
var OutSize:TpvSizeUInt;
begin

 aJob^.OutData:=nil;
 aJob^.OutSize:=0;

 case aJob.FileHeader^.CompressionMethod of
  TpvUInt8(TpvCompressionMethod.None):begin
   if aJob.InSize>0 then begin
    GetMem(aJob.OutData,aJob.InSize);
    aJob.OutSize:=aJob.InSize;
    Move(aJob.InData^,aJob.OutData^,aJob.InSize);
    aJob.Success:=true;
   end else begin
    aJob.Success:=false;
   end;
  end;
  TpvUInt8(TpvCompressionMethod.Deflate):begin
   OutSize:=aJob.OutSize;
   aJob.Success:=DoDeflate(aJob.InData,aJob.InSize,aJob.OutData,OutSize,TpvDeflateMode(aJob.FileHeader^.CompressionLevel),false);
   aJob.OutSize:=OutSize;
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSF):begin
   aJob.Success:=LZBRSFCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRSFLevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRRC):begin
   aJob.Success:=LZBRRCCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRRCLevel(aJob.FileHeader^.CompressionLevel),false);
{  // Safe check by decompressing the compressed data again and compare it with the original data
   if aJob.Success then begin
    GetMem(TemporaryOutData,aJob.CompressionPartHeader^.UncompressedSize);
    try
     TemporaryOutDataSize:=0;
     aJob.Success:=LZBRRCDecompress(aJob.OutData,aJob.OutSize,TemporaryOutData,TemporaryOutDataSize,aJob.CompressionPartHeader^.UncompressedSize,false);
     if aJob.Success then begin
      aJob.Success:=CompareMem(TemporaryOutData,aJob.InData,Min(TemporaryOutDataSize,aJob.CompressionPartHeader^.UncompressedSize));
     end;
     if not aJob.Success then begin
      FileName:='broken_'+IntToHex(TpvPtrUInt(aJob),16)+'_'+IntToHex(random($ffffffff),8);
      Stream:=TFileStream.Create(FileName+'_input.bin',fmCreate);
      try
       Stream.WriteBuffer(aJob.InData^,aJob.InSize);
      finally
       FreeAndNil(Stream);
      end;
      Stream:=TFileStream.Create(FileName+'_output.bin',fmCreate);
      try
       Stream.WriteBuffer(aJob.OutData^,aJob.OutSize);
      finally
       FreeAndNil(Stream);
      end;
      Stream:=TFileStream.Create(FileName+'_decompressed.bin',fmCreate);
      try
       Stream.WriteBuffer(TemporaryOutData^,TemporaryOutDataSize);
      finally
       FreeAndNil(Stream);
      end;
     end;
    finally
     FreeMem(TemporaryOutData);
     TemporaryOutData:=nil;
    end;
   end;//}
  end;
  TpvUInt8(TpvCompressionMethod.LZMA):begin
   aJob.Success:=LZMACompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZMALevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRS):begin
   aJob.Success:=LZBRSCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRSLevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSX):begin
   aJob.Success:=LZBRSXCompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,TpvLZBRSXLevel(aJob.FileHeader^.CompressionLevel),false);
  end;
  else begin
   aJob.Success:=false;
  end;
 end;

 aJob^.CompressionPartHeader^.CompressedSize:=aJob^.OutSize;

end;     

procedure CompressPartJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:TpvSizeInt;
    JobData:PCompressionPartJob;
begin
 for Index:=FromIndex to ToIndex do begin
  JobData:=Data;
  inc(JobData,Index);
  CompressPart(JobData);
 end;
end;

function CompressStream(const aInStream:TStream;const aOutStream:TStream;const aCompressionMethod:TpvCompressionMethod;const aCompressionLevel:TpvUInt32;const aParts:TpvUInt32):boolean;
var InData:Pointer;
    InSize,Offset,Size,RemainSize,PartSize:TpvUInt64;
    FileHeader:TCompressedFileHeader;
    CompressionPartHeaders:TCompressionPartHeaders;
    CompressionPartHeader:PCompressionPartHeader;
    CompressionPartJobs:TCompressionPartJobs;
    CompressionPartJob:PCompressionPartJob;
    PartIndex:TpvSizeInt;    
begin

 if assigned(aInStream) and (aInStream.Size>0) and assigned(aOutStream) then begin

  GetMem(InData,aInStream.Size);
  try

   aInStream.Seek(0,soBeginning);
   aInStream.ReadBuffer(InData^,aInStream.Size);

   InSize:=aInStream.Size;

   FileHeader.Signature:=CompressedFileSignature;
   FileHeader.Version:=CompressedFileVersion;
   FileHeader.Flags:=0; // Zero for now, because it is reserved for future use
   if InSize<65536 then begin
    FileHeader.Parts:=1;
   end else begin
    if aParts=0 then begin
     FileHeader.Parts:=Min(Max(IntLog264(InSize)-21,1),255); // minimum 1, maximum 255 parts
    end else begin
     FileHeader.Parts:=Min(Max(aParts,1),255); // minimum 1, maximum 255 parts
    end;
   end;
   FileHeader.CompressionMethod:=TpvUInt8(aCompressionMethod);
   FileHeader.CompressionLevel:=aCompressionLevel;
   FileHeader.UncompressedSize:=InSize;

   CompressionPartHeaders:=nil;
   try

    SetLength(CompressionPartHeaders,FileHeader.Parts);

    Offset:=0;
    RemainSize:=InSize;
    PartSize:=TpvUInt64(Max(TpvUInt64(1),TpvUInt64((InSize+(FileHeader.Parts-1)) div FileHeader.Parts)));
    for PartIndex:=0 to FileHeader.Parts-1 do begin
     if (PartIndex=(FileHeader.Parts-1)) or (RemainSize<PartSize) then begin
      Size:=RemainSize;
     end else begin
      Size:=PartSize;
     end;
     CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
     CompressionPartHeader^.CompressedOffset:=0;
     CompressionPartHeader^.CompressedSize:=0;
     CompressionPartHeader^.UncompressedOffset:=Offset;
     CompressionPartHeader^.UncompressedSize:=Size;
     inc(Offset,Size);
     dec(RemainSize,Size);
    end;

    CompressionPartJobs:=nil;
    try
     
     SetLength(CompressionPartJobs,FileHeader.Parts);
     for PartIndex:=0 to FileHeader.Parts-1 do begin
      CompressionPartJob:=@CompressionPartJobs[PartIndex];
      CompressionPartJob^.FileHeader:=@FileHeader;
      CompressionPartJob^.CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
      CompressionPartJob^.InData:=Pointer(TpvPtrUInt(TpvPtrUInt(InData)+TpvPtrUInt(CompressionPartHeaders[PartIndex].UncompressedOffset)));
      CompressionPartJob^.InSize:=CompressionPartHeaders[PartIndex].UncompressedSize;
      CompressionPartJob^.OutData:=nil;
      CompressionPartJob^.OutSize:=0;
      CompressionPartJob^.Success:=false;
     end;

     if FileHeader.Parts>1 then begin
      if assigned(pvApplication) and assigned(pvApplication.PasMPInstance) then begin
       // Use multiple threads for multiple parts
       pvApplication.PasMPInstance.Invoke(pvApplication.PasMPInstance.ParallelFor(@CompressionPartJobs[0],0,length(CompressionPartJobs)-1,CompressPartJob,1,PasMPDefaultDepth,nil,0,0));
      end else begin
       for PartIndex:=0 to FileHeader.Parts-1 do begin
        CompressPart(@CompressionPartJobs[PartIndex]);
       end;
      end;
     end else begin
      // No need to use multiple threads for only one part
      CompressPart(@CompressionPartJobs[0]);
     end;

     result:=true;
     Offset:=SizeOf(TCompressedFileHeader)+(FileHeader.Parts*SizeOf(TCompressionPartHeader));
     Size:=0;
     for PartIndex:=0 to FileHeader.Parts-1 do begin
      CompressionPartJob:=@CompressionPartJobs[PartIndex];
      if CompressionPartJob^.Success and assigned(CompressionPartJob^.OutData) and (CompressionPartJob^.OutSize>0) then begin
       CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
       CompressionPartHeader^.CompressedOffset:=Offset;
       CompressionPartHeader^.CompressedSize:=CompressionPartJob^.OutSize;
       inc(Offset,CompressionPartHeader^.CompressedSize);
       inc(Size,CompressionPartHeader^.CompressedSize);
      end else begin
       result:=false;
       break;
      end;
     end;

     FileHeader.CompressedSize:=Size;

     if result then begin
      
      aOutStream.WriteBuffer(FileHeader,SizeOf(TCompressedFileHeader));

      for PartIndex:=0 to FileHeader.Parts-1 do begin
       CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
       aOutStream.WriteBuffer(CompressionPartHeader^,SizeOf(TCompressionPartHeader));
      end;

      for PartIndex:=0 to FileHeader.Parts-1 do begin
       CompressionPartJob:=@CompressionPartJobs[PartIndex];
       if assigned(CompressionPartJob^.OutData) and (CompressionPartJob^.OutSize>0) then begin
        aOutStream.WriteBuffer(CompressionPartJob^.OutData^,CompressionPartJob^.OutSize);
       end;
      end;
      
     end;

     for PartIndex:=0 to FileHeader.Parts-1 do begin
      CompressionPartJob:=@CompressionPartJobs[PartIndex];
      if assigned(CompressionPartJob^.OutData) then begin
       FreeMem(CompressionPartJob^.OutData);
       CompressionPartJob^.OutData:=nil;
      end;
     end;

    finally
     CompressionPartJobs:=nil;
    end;

   finally
    CompressionPartHeaders:=nil;
   end; 

  finally
   FreeMem(InData);
   InData:=nil;
  end;

 end else begin

  result:=false;

 end;

end;

procedure DecompressPart(const aJob:PDecompressionPartJob);
var TemporaryDeflateOutData:Pointer; // Deflate isn't in-place, so we need a temporary buffer
    TemporaryDeflateOutSize:TpvSizeUInt;
begin 

 case aJob.FileHeader^.CompressionMethod of
  TpvUInt8(TpvCompressionMethod.None):begin
   // No compression, just copy the data
   if aJob.InSize>0 then begin
    GetMem(aJob.OutData,aJob.InSize);
    aJob.OutSize:=aJob.InSize;
    Move(aJob.InData^,aJob.OutData^,aJob.InSize);
    aJob.Success:=true;
   end else begin
    aJob.Success:=false;
   end;
  end;
  TpvUInt8(TpvCompressionMethod.Deflate):begin
   // The old good Deflate, it is slow and it doesn't compress very well in addition to that, in comparison to LZBRRC and LZMA
   TemporaryDeflateOutData:=nil;
   TemporaryDeflateOutSize:=0;
   try
    aJob.Success:=DoInflate(aJob.InData,aJob.InSize,TemporaryDeflateOutData,TemporaryDeflateOutSize,false);
    if aJob.Success then begin
     if TemporaryDeflateOutSize=aJob.CompressionPartHeader^.UncompressedSize then begin
      GetMem(aJob.OutData,TemporaryDeflateOutSize);
      aJob.OutSize:=TemporaryDeflateOutSize;
      Move(TemporaryDeflateOutData^,aJob.OutData^,TemporaryDeflateOutSize);
     end else begin
      aJob.Success:=false;
     end;
    end;
   finally
    if assigned(TemporaryDeflateOutData) then begin
     FreeMem(TemporaryDeflateOutData);
     TemporaryDeflateOutData:=nil;
    end;
   end;
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSF):begin
   // LZBRSF is a pure byte-wise compression algorithm, so it is pretty fast, but it doesn't compress very well, but it is still better than 
   // nothing, and it is also very fast at decompression, so it is a pretty good choice for games, where the compression ratio isn't that important, 
   // only the decompression speed.
   aJob.Success:=LZBRSFDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRRC):begin
   // LZBRRC is a LZ77-based compression algorithm but with range coding for the entropy coding, so it is pretty fast and it does also 
   // compress very well, but LZMA is still better at the compression ratio. LZBRRC is also very fast at decompression, so it is a good
   // choice for games, because it is fast at both compression and decompression, and it compresses very well, so it saves a lot of space.
{$if declared(LZBRRCFastDecompress)}
   // Assembler version is faster, but not yet available for all targets, and it has fewer to no sanity checks
   aJob.Success:=LZBRRCFastDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
{$else}
   // The pure pas version is slower, but available for all targets, and it has full sanity checks
   aJob.Success:=LZBRRCDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
{$ifend}
  end;
  TpvUInt8(TpvCompressionMethod.LZMA):begin
   // The old good LZMA, but it is slow, but it compresses very well. It should be used for data, where the compression ratio is more important 
   // than the decompression speed.  
   aJob.Success:=LZMADecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRS):begin
   // LZBRS is a simple LZ77/LZSS-style algorithm like BriefLZ, but with 32-bit tags instead 16-bit tags,
   // and with end tag (match with offset 0)
   // Not to be confused with the old equal-named LRBRS from BeRoEXEPacker, which was 8-bit byte-wise tag-based.
   aJob.Success:=LZBRSDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  TpvUInt8(TpvCompressionMethod.LZBRSX):begin
   // LZBRSX is a simple LZ77/LZSS-style algorithm like apLib, but with 32-bit tags instead 8-bit tags
   aJob.Success:=LZBRSXDecompress(aJob.InData,aJob.InSize,aJob.OutData,aJob.OutSize,aJob.CompressionPartHeader^.UncompressedSize,false);
  end;
  else begin
   aJob.Success:=false;
  end;
 end;

end; 

procedure DecompressPartJob(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:TpvSizeInt;
    JobData:PDecompressionPartJob;
begin
 for Index:=FromIndex to ToIndex do begin
  JobData:=Data;
  inc(JobData,Index);
  DecompressPart(JobData);
 end;
end;

function DecompressStream(const aInStream:TStream;const aOutStream:TStream):boolean;
var InData,OutData:Pointer;
    InSize,OutSize,Offset,Size,RemainSize,PartSize:TpvUInt64;
    PartIndex:TpvSizeInt;
    FileHeader:TCompressedFileHeader;
    CompressionPartHeaders:TCompressionPartHeaders;
    CompressionPartHeader:PCompressionPartHeader;
    DecompressionPartJobs:TDecompressionPartJobs;
    DecompressionPartJob:PDecompressionPartJob;
begin

 if assigned(aInStream) and (aInStream.Size>=SizeOf(TCompressedFileHeader)) and assigned(aOutStream) then begin

  InData:=nil;
  OutData:=nil;

  aInStream.ReadBuffer(FileHeader,SizeOf(TCompressedFileHeader));

  Size:=aInStream.Size-(SizeOf(TCompressedFileHeader)+(FileHeader.Parts*SizeOf(TCompressionPartHeader)));

  if (FileHeader.Signature=CompressedFileSignature) and
     (FileHeader.Version=CompressedFileVersion) and
     (FileHeader.CompressedSize=Size) and
     ((FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.None)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.Deflate)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZBRSF)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZBRRC)) or
      (FileHeader.CompressionMethod=TpvUInt8(TpvCompressionMethod.LZMA))) then begin

   InSize:=FileHeader.CompressedSize;
   OutSize:=FileHeader.UncompressedSize;

   CompressionPartHeaders:=nil;
   try

    SetLength(CompressionPartHeaders,FileHeader.Parts);
    aInStream.ReadBuffer(CompressionPartHeaders[0],FileHeader.Parts*SizeOf(TCompressionPartHeader));

    if InSize>0 then begin

     GetMem(InData,InSize);
     try

      aInStream.ReadBuffer(InData^,InSize);
      
      if OutSize>0 then begin

       GetMem(OutData,OutSize);
       try

        DecompressionPartJobs:=nil;
        try

         SetLength(DecompressionPartJobs,FileHeader.Parts);

         Offset:=SizeOf(TCompressedFileHeader)+(FileHeader.Parts*SizeOf(TCompressionPartHeader));
         for PartIndex:=0 to FileHeader.Parts-1 do begin
          DecompressionPartJob:=@DecompressionPartJobs[PartIndex];
          DecompressionPartJob^.FileHeader:=@FileHeader;
          DecompressionPartJob^.CompressionPartHeader:=@CompressionPartHeaders[PartIndex];
          DecompressionPartJob^.InData:=Pointer(TpvPtrUInt(TpvPtrUInt(InData)+TpvPtrUInt(DecompressionPartJob^.CompressionPartHeader^.CompressedOffset-Offset)));
          DecompressionPartJob^.InSize:=DecompressionPartJob^.CompressionPartHeader^.CompressedSize;
          DecompressionPartJob^.OutData:=Pointer(TpvPtrUInt(TpvPtrUInt(OutData)+TpvPtrUInt(DecompressionPartJob^.CompressionPartHeader^.UncompressedOffset)));
          DecompressionPartJob^.OutSize:=DecompressionPartJob^.CompressionPartHeader^.UncompressedSize;
          DecompressionPartJob^.Success:=false;
         end;

         if FileHeader.Parts>1 then begin
          if assigned(pvApplication) and assigned(pvApplication.PasMPInstance) then begin
           // Use multiple threads for multiple parts
           pvApplication.PasMPInstance.Invoke(pvApplication.PasMPInstance.ParallelFor(@DecompressionPartJobs[0],0,length(DecompressionPartJobs)-1,DecompressPartJob,1,PasMPDefaultDepth,nil,0,0));
          end else begin
           for PartIndex:=0 to FileHeader.Parts-1 do begin
            DecompressPart(@DecompressionPartJobs[PartIndex]);
           end;
          end;
         end else begin
          // No need to use multiple threads for only one part
          DecompressPart(@DecompressionPartJobs[0]);
         end;

         result:=true;
         for PartIndex:=0 to FileHeader.Parts-1 do begin
          DecompressionPartJob:=@DecompressionPartJobs[PartIndex];
          if DecompressionPartJob^.Success and assigned(DecompressionPartJob^.OutData) and (DecompressionPartJob^.OutSize>0) then begin
           aOutStream.WriteBuffer(DecompressionPartJob^.OutData^,DecompressionPartJob^.OutSize);
          end else begin
           result:=false;
           break;
          end;
         end;

        finally
         DecompressionPartJobs:=nil;
        end;

       finally
        FreeMem(OutData);
        OutData:=nil;
       end; 

      end;

     finally
      FreeMem(InData);
      InData:=nil;
     end; 

    end; 

   finally
    CompressionPartHeaders:=nil;
   end;   

  end else begin

   result:=false;

  end;

 end else begin

  result:=false;

 end;

end;

end.
