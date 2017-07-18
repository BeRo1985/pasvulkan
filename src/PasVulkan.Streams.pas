(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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

uses SysUtils,
     Classes,
     Math,
     PasVulkan.Types;

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

end.
