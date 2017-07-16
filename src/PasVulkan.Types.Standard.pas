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
 * 5. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
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

type PPInt8=^PInt8;
     PInt8=^TInt8;
     TInt8={$ifdef fpc}Int8{$else}shortint{$endif};

     PPUInt8=^PUInt8;
     PUInt8=^TUInt8;
     TUInt8={$ifdef fpc}UInt8{$else}byte{$endif};

     PPInt16=^PInt16;
     PInt16=^TInt16;
     TInt16={$ifdef fpc}Int16{$else}smallint{$endif};

     PPUInt16=^PUInt16;
     PUInt16=^TUInt16;
     TUInt16={$ifdef fpc}UInt16{$else}word{$endif};

     PPInt32=^PInt32;
     PInt32=^TInt32;
     TInt32={$ifdef fpc}Int32{$else}longint{$endif};

     PPUInt32=^PUInt32;
     PUInt32=^TUInt32;
     TUInt32={$ifdef fpc}UInt32{$else}longword{$endif};

     PPInt64=^PInt64;
     PInt64=^TInt64;
     TInt64=Int64;

     PPUInt64=^PUInt64;
     PUInt64=^TUInt64;
     TUInt64=UInt64;

     PPChar=^PChar;
     PChar=PAnsiChar;
     TChar=AnsiChar;

     PPPointer=^PPointer;
     PPointer=^TPointer;
     TPointer=Pointer;

     PPPointers=^PPointers;
     PPointers=^TPointers;
     TPointers=array[0..65535] of pointer;

     PPVoid=^PVoid;
     PVoid=Pointer;

     PPFloat=^PFloat;
     PFloat=^TFloat;
     TFloat=Single;

     PPDouble=^PDouble;
     PDouble=^TDouble;
     TDouble=Double;

{    PPTime=^PTime;
     PTime=^TTime;
     TTime=TInt64;}

     PPPtrUInt=^PPtrUInt;
     PPPtrInt=^PPtrInt;
     PPtrUInt=^TPtrUInt;
     PPtrInt=^TPtrInt;
{$ifdef fpc}
     TPtrUInt=PtrUInt;
     TPtrInt=PtrInt;
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TPtrUInt=NativeUInt;
     TPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
     TPtrUInt=uint64;
     TPtrInt=int64;
{$else}
     TPtrUInt=longword;
     TPtrInt=longint;
{$endif}
{$endif}

     PPSizeUInt=^PSizeUInt;
     PSizeUInt=^TSizeUInt;
     TSizeUInt=TPtrUInt;

     PPSizeInt=^PSizeInt;
     PSizeInt=^TSizeInt;
     TSizeInt=TPtrInt;

     PPSize=^PSizeUInt;
     PSize=^TSizeUInt;
     TSize=TPtrUInt;

     PPPtrDiff=^PPtrDiff;
     PPtrDiff=^TPtrDiff;
     TPtrDiff=TPtrInt;

     PPRawByteString=^PRawByteString;
     PRawByteString=^TRawByteString;
     TRawByteString=RawByteString;

     PPUTF8String=^PUTF8String;
     PUTF8String=^TUTF8String;
     TUTF8String=UTF8String;

     PPFileName=^PFileName;
     PFileName=^TFileName;
     TFileName=String;

     PHandle=^THandle;
     THandle={$ifdef fpc}bitpacked{$endif} record
      Index:TUInt32;
      Generation:TUInt32;
     end;

     PID=^TID;
     TID={$ifdef fpc}bitpacked{$endif} record
      Index:TUInt32;
      Generation:TUInt32;
     end;

{const TimeToSeconds=1.0/TTime($100000000);
      TimeFromSeconds=TTime($100000000);
      TimeSeconds=TTime($100000000);}

implementation

uses SysUtils,
     Classes;

initialization
end.
