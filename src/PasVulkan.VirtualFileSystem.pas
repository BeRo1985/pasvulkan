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
unit PasVulkan.VirtualFileSystem;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$scopedenums on}
{$m+}

interface

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Types,
     PasVulkan.Assets,
     PasVulkan.Streams,
     PasVulkan.Math,
     PasVulkan.Archive.ZIP;

type { TpvVirtualFileSystem }

     TpvVirtualFileSystem=class
      private
       fArchiveZIP:TpvArchiveZIP;
       fStream:TStream;
      public
       constructor Create(const aData:pointer;const aDataSize:TpvSizeInt;const aFileName:string=''); reintroduce; virtual;
       destructor Destroy; override;
       function ExistFile(const aFileName:string):boolean;
       function GetFile(const aFileName:string):TStream;
      published
       property ArchiveZIP:TpvArchiveZIP read fArchiveZIP;
     end;

implementation

{ TpvVirtualFileSystem }

constructor TpvVirtualFileSystem.Create(const aData:pointer;const aDataSize:TpvSizeInt;const aFileName:string);
var Stream:TStream;
begin
 inherited Create;
 fStream:=TMemoryStream.Create;
 fArchiveZIP:=TpvArchiveZIP.Create;
 if (length(aFileName)>0) and FileExists(aFileName) then begin
  Stream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
 end else begin
  Stream:=TMemoryStream.Create;
  if assigned(aData) and (aDataSize>0) then begin
   Stream.Write(aData^,aDataSize);
   Stream.Seek(0,soBeginning);
  end;
 end;
 try
  fStream.CopyFrom(Stream,Stream.Size);
  fStream.Seek(0,soBeginning);
  fArchiveZIP.LoadFromStream(fStream);
 finally
  FreeAndNil(Stream);
 end;
end;

destructor TpvVirtualFileSystem.Destroy;
begin
 FreeAndNil(fArchiveZIP);
 FreeAndNil(fStream);
 inherited Destroy;
end;

function TpvVirtualFileSystem.ExistFile(const aFileName:string):boolean;
begin
 result:=assigned(fArchiveZIP.Entries.Find(aFileName));
end;

function TpvVirtualFileSystem.GetFile(const aFileName:string):TStream;
var ZIPEntry:TpvArchiveZIPEntry;
begin
 result:=nil;
 ZIPEntry:=fArchiveZIP.Entries.Find(aFileName);
 if assigned(ZIPEntry) then begin
  result:=TMemoryStream.Create;
  ZIPEntry.SaveToStream(result);
  result.Seek(0,soBeginning);
 end;
end;

end.
