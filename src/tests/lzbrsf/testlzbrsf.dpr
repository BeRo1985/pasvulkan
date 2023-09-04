program testlzbrsf;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
 {$apptype console}
{$endif}

(*{$ifdef Unix}
      cthreads,
     {$endif}*)

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Types,
     PasVulkan.Compression.LZBRSF;

procedure TestLZBRSFCompress;
var InputFileStream:TFileStream;
    OutputFileStream:TFileStream;
    CompressedSize:TpvUInt64;
    UncompressedSize:TpvUInt64;
    CompressedData:Pointer;
    UncompressedData:Pointer;
begin

 InputFileStream:=TFileStream.Create('input.dat',fmOpenRead {or fmShareDenyNone});
 try
  UncompressedSize:=InputFileStream.Size;
  GetMem(UncompressedData,InputFileStream.Size);
  InputFileStream.ReadBuffer(UncompressedData^,InputFileStream.Size);
 finally
  FreeAndNil(InputFileStream);
 end;

 if LZBRSFCompress(UncompressedData,UncompressedSize,CompressedData,CompressedSize,TpvLZBRSFMode.Slow) then begin
 
  OutputFileStream:=TFileStream.Create('output.dat',fmCreate);
  try
   OutputFileStream.WriteBuffer(CompressedData^,CompressedSize);
  finally
   FreeAndNil(OutputFileStream);
  end;

  FreeMem(CompressedData);
  CompressedData:=nil;
  
 end; 

 FreeMem(UncompressedData);
 UncompressedData:=nil;

end;

procedure TestLZBRSFDecompress;
var InputFileStream:TFileStream;
    OutputFileStream:TFileStream;
    CompressedSize:TpvUInt64;
    UncompressedSize:TpvUInt64;
    CompressedData:Pointer;
    UncompressedData:Pointer;
begin

 InputFileStream:=TFileStream.Create('output.dat',fmOpenRead {or fmShareDenyNone});
 try
  CompressedSize:=InputFileStream.Size;
  GetMem(CompressedData,CompressedSize);
  InputFileStream.ReadBuffer(CompressedData^,CompressedSize);
 finally
  FreeAndNil(InputFileStream);
 end;

 if LZBRSFDecompress(CompressedData,CompressedSize,UncompressedData,UncompressedSize) then begin
 
  OutputFileStream:=TFileStream.Create('output2.dat',fmCreate);
  try
   OutputFileStream.WriteBuffer(UncompressedData^,UncompressedSize);
  finally
   FreeAndNil(OutputFileStream);
  end;

  FreeMem(UncompressedData);
  UncompressedData:=nil;
  
 end; 

 FreeMem(CompressedData);
 CompressedData:=nil;

end;

procedure TestCompare;
var OriginalFileStream:TFileStream;
    UncompressedFileStream:TFileStream;
    OriginalSize:TpvUInt64;
    UncompressedSize:TpvUInt64;
    OriginalData:Pointer;
    UncompressedData:Pointer;
begin

 OriginalFileStream:=TFileStream.Create('input.dat',fmOpenRead {or fmShareDenyNone});
 try
  OriginalSize:=OriginalFileStream.Size;
  GetMem(OriginalData,OriginalFileStream.Size);
  OriginalFileStream.ReadBuffer(OriginalData^,OriginalFileStream.Size);
 finally
  FreeAndNil(OriginalFileStream);
 end;

 UncompressedFileStream:=TFileStream.Create('output2.dat',fmOpenRead {or fmShareDenyNone});
 try
  UncompressedSize:=UncompressedFileStream.Size;
  GetMem(UncompressedData,UncompressedFileStream.Size);
  UncompressedFileStream.ReadBuffer(UncompressedData^,UncompressedFileStream.Size);
 finally
  FreeAndNil(UncompressedFileStream);
 end;

 if OriginalSize=UncompressedSize then begin
  if CompareMem(OriginalData,UncompressedData,OriginalSize) then begin
   WriteLn('Compare OK');
  end else begin
   WriteLn('Compare failed');
  end;
 end else begin
  WriteLn('Compare failed');
 end;

 FreeMem(UncompressedData);
 UncompressedData:=nil;

 FreeMem(OriginalData);
 OriginalData:=nil;
  
end;

begin
 
 try

  TestLZBRSFCompress;
  TestLZBRSFDecompress;
  TestCompare;

 except
  on e:Exception do begin
   WriteLn('Exception: ',e.Message);
  end;
 end;

 WriteLn('Press enter to continue...');
 ReadLn;

end.

