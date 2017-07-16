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
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >= 3.0 *
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
unit PasVulkan.Types.Standard;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

type PPpvInt8=^PpvInt8;
     PpvInt8=^TpvInt8;
     TpvInt8={$ifdef fpc}Int8{$else}shortint{$endif};

     PPpvUInt8=^PpvUInt8;
     PpvUInt8=^TpvUInt8;
     TpvUInt8={$ifdef fpc}UInt8{$else}byte{$endif};

     PPpvInt16=^PpvInt16;
     PpvInt16=^TpvInt16;
     TpvInt16={$ifdef fpc}Int16{$else}smallint{$endif};

     PPpvUInt16=^PpvUInt16;
     PpvUInt16=^TpvUInt16;
     TpvUInt16={$ifdef fpc}UInt16{$else}word{$endif};

     PPpvInt32=^PpvInt32;
     PpvInt32=^TpvInt32;
     TpvInt32={$ifdef fpc}Int32{$else}longint{$endif};

     PPpvUInt32=^PpvUInt32;
     PpvUInt32=^TpvUInt32;
     TpvUInt32={$ifdef fpc}UInt32{$else}longword{$endif};

     PPpvInt64=^PpvInt64;
     PpvInt64=^TpvInt64;
     TpvInt64=Int64;

     PPpvUInt64=^PpvUInt64;
     PpvUInt64=^TpvUInt64;
     TpvUInt64=UInt64;

     PPChar=^PChar;
     PChar=PAnsiChar;
     TChar=AnsiChar;

     PPpvPointer=^PpvPointer;
     PpvPointer=^TpvPointer;
     TpvPointer=Pointer;

     PPPointers=^PPointers;
     PPointers=^TPointers;
     TPointers=array[0..65535] of pointer;

     PPpvVoid=^PpvVoid;
     PpvVoid=Pointer;

     PPpvFloat=^PpvFloat;
     PpvFloat=^TpvFloat;
     TpvFloat=Single;

     PPpvDouble=^PpvDouble;
     PpvDouble=^TpvDouble;
     TpvDouble=Double;

{    PPTime=^PTime;
     PTime=^TTime;
     TTime=TpvInt64;}

     PPpvPtrUInt=^PpvPtrUInt;
     PPpvPtrInt=^PpvPtrInt;
     PpvPtrUInt=^TpvPtrUInt;
     PpvPtrInt=^TpvPtrInt;
{$ifdef fpc}
     TpvPtrUInt=PtrUInt;
     TpvPtrInt=PtrInt;
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TpvPtrUInt=NativeUInt;
     TpvPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
     TpvPtrUInt=uint64;
     TpvPtrInt=int64;
{$else}
     TpvPtrUInt=longword;
     TpvPtrInt=longint;
{$endif}
{$endif}

     PPpvSizeUInt=^PpvSizeUInt;
     PpvSizeUInt=^TpvSizeUInt;
     TpvSizeUInt=TpvPtrUInt;

     PPpvSizeInt=^PpvSizeInt;
     PpvSizeInt=^TpvSizeInt;
     TpvSizeInt=TpvPtrInt;

     PPpvSize=^PpvSizeUInt;
     PpvSize=^TpvSizeUInt;
     TpvSize=TpvPtrUInt;

     PPpvPtrDiff=^PpvPtrDiff;
     PpvPtrDiff=^TpvPtrDiff;
     TpvPtrDiff=TpvPtrInt;

     PPpvRawByteString=^PpvRawByteString;
     PpvRawByteString=^TpvRawByteString;
     TpvRawByteString=RawByteString;

     PPpvUTF8String=^PpvUTF8String;
     PpvUTF8String=^TpvUTF8String;
     TpvUTF8String=UTF8String;

     PPpvUnicodeString=^PpvUnicodeString;
     PpvUnicodeString=^TpvUnicodeString;
     TpvUnicodeString=UnicodeString;

     PPpvFileName=^PpvFileName;
     PpvFileName=^TpvFileName;
     TpvFileName=String;

     PHandle=^THandle;
     THandle={$ifdef fpc}bitpacked{$endif} record
      Index:TpvUInt32;
      Generation:TpvUInt32;
     end;

     PID=^TID;
     TID={$ifdef fpc}bitpacked{$endif} record
      Index:TpvUInt32;
      Generation:TpvUInt32;
     end;

{const TimeToSeconds=1.0/TTime($100000000);
      TimeFromSeconds=TTime($100000000);
      TimeSeconds=TTime($100000000);}

implementation

uses SysUtils,
     Classes;

initialization
end.
