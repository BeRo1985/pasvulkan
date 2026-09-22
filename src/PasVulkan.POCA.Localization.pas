(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.POCA.Localization;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

// Exposes the localization of PasVulkan.Localization to POCA scripts, so that a script drawn user interface can
// be translated by the same catalogs and by the same keys as the Pascal side. The host decides under which hash
// the functions do land, in the same way as the Scene3D bindings do it, so that this unit does not need to know
// anything about the namespace layout of the application which does use it.
//
// Every function does stay usable when no localization instance does exist at all, in which case the inline
// default of the call is returned, so that a script can never break just because the localization was not set up.

interface

uses SysUtils,
     Classes,
     POCA,
     PasVulkan.Types,
     PasVulkan.Localization;

procedure InitializePOCALocalizationContext(const aContext:PPOCAContext;const aEngineHash:TPOCAValue);

implementation

// engine.localization.tr(key,default) -> the translated text, or the default when no catalog does know the key
function POCALocalizationTranslate(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Key,Default:TpvUTF8String;
begin
 result:=POCAValueNull;
 if aCountArguments<1 then begin
  exit;
 end;
 Key:=POCAGetStringValue(aContext,aArguments^[0]);
 if aCountArguments>1 then begin
  Default:=POCAGetStringValue(aContext,aArguments^[1]);
 end else begin
  Default:=Key;
 end;
 result:=POCANewString(aContext,Tr(Key,Default));
end;

// engine.localization.trPlural(key,singular,plural,count) -> the plural form which does fit the count in the
// current language, which can be more than just two forms
function POCALocalizationTranslatePlural(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Key,DefaultSingular,DefaultPlural:TpvUTF8String;
    Count:TpvInt64;
begin
 result:=POCAValueNull;
 if aCountArguments<4 then begin
  exit;
 end;
 Key:=POCAGetStringValue(aContext,aArguments^[0]);
 DefaultSingular:=POCAGetStringValue(aContext,aArguments^[1]);
 DefaultPlural:=POCAGetStringValue(aContext,aArguments^[2]);
 Count:=trunc(POCAGetNumberValue(aContext,aArguments^[3]));
 result:=POCANewString(aContext,TrPlural(Key,DefaultSingular,DefaultPlural,Count));
end;

// Fills the named placeholders of an already translated text from the name and value pairs which do start at
// aFirstArgument, which is what both trFormat and formatText do need
function POCALocalizationFillPlaceholders(const aContext:PPOCAContext;const aText:TpvUTF8String;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aFirstArgument:TpvSizeInt):TpvUTF8String;
var Index,CountValues:TpvSizeInt;
    Value:TPOCAValue;
    VarRecs:array of TVarRec;
    Strings:array of TpvUTF8String;
    Numbers:array of Extended;
begin
 VarRecs:=nil;
 Strings:=nil;
 Numbers:=nil;
 try
  // A trailing name without a value is dropped, since it could not be filled in anyway
  CountValues:=((aCountArguments-aFirstArgument) shr 1) shl 1;
  SetLength(VarRecs,CountValues);
  SetLength(Strings,CountValues);
  SetLength(Numbers,CountValues);
  for Index:=0 to CountValues-1 do begin
   Value:=aArguments^[Index+aFirstArgument];
   if POCAGetValueType(Value)=pvtNUMBER then begin
    // A number does stay a number, so that a spec like {total:n2} can do the grouping and the rounding with the
    // separators of the current language
    Numbers[Index]:=POCAGetNumberValue(aContext,Value);
    VarRecs[Index].VType:=vtExtended;
    VarRecs[Index].VExtended:=@Numbers[Index];
   end else begin
    Strings[Index]:=POCAGetStringValue(aContext,Value);
    VarRecs[Index].VType:=vtAnsiString;
    VarRecs[Index].VAnsiString:=Pointer(Strings[Index]);
   end;
  end;
  result:=pvLocalization.FormatText(aText,VarRecs);
 finally
  VarRecs:=nil;
  Strings:=nil;
  Numbers:=nil;
 end;
end;

// engine.localization.trFormat(key,default,name,value,name,value,...) -> the translated text with its named
// placeholders filled in, for example trFormat("hud.progress","{done} of {total}","done",3,"total",7). The names
// and values are given in pairs, in the same way as the Pascal side does it with an array of const, so that a
// translator can reorder the placeholders freely.
function POCALocalizationFormat(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Key,Default:TpvUTF8String;
begin
 result:=POCAValueNull;
 if aCountArguments<2 then begin
  exit;
 end;
 Key:=POCAGetStringValue(aContext,aArguments^[0]);
 Default:=POCAGetStringValue(aContext,aArguments^[1]);
 if not assigned(pvLocalization) then begin
  result:=POCANewString(aContext,Default);
  exit;
 end;
 result:=POCANewString(aContext,POCALocalizationFillPlaceholders(aContext,pvLocalization.Translate(Key,Default),aArguments,aCountArguments,2));
end;

// engine.localization.formatText(text,name,value,...) -> the text with its named placeholders filled in, without
// translating it first. It is what a text needs which is already translated, above all the result of trPlural,
// where trFormat would wrongly look the text up as a key again.
function POCALocalizationFormatTextOnly(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Text:TpvUTF8String;
begin
 result:=POCAValueNull;
 if aCountArguments<1 then begin
  exit;
 end;
 Text:=POCAGetStringValue(aContext,aArguments^[0]);
 if not assigned(pvLocalization) then begin
  result:=POCANewString(aContext,Text);
  exit;
 end;
 result:=POCANewString(aContext,POCALocalizationFillPlaceholders(aContext,Text,aArguments,aCountArguments,1));
end;

// engine.localization.formatNumber(value,fractionDigits,grouping) -> the number with the decimal and thousands
// separators of the current language. A negative digit count does keep the digits which the value does need.
function POCALocalizationFormatNumber(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var FractionDigits:TpvInt32;
    Grouping:boolean;
begin
 result:=POCAValueNull;
 if (aCountArguments<1) or not assigned(pvLocalization) then begin
  exit;
 end;
 if aCountArguments>1 then begin
  FractionDigits:=trunc(POCAGetNumberValue(aContext,aArguments^[1]));
 end else begin
  FractionDigits:=-1;
 end;
 if aCountArguments>2 then begin
  Grouping:=POCAGetBooleanValue(aContext,aArguments^[2]);
 end else begin
  Grouping:=true;
 end;
 result:=POCANewString(aContext,pvLocalization.FormatNumber(POCAGetNumberValue(aContext,aArguments^[0]),FractionDigits,Grouping));
end;

// engine.localization.formatInteger(value,grouping) -> the integer with the thousands separator of the current
// language
function POCALocalizationFormatInteger(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Grouping:boolean;
begin
 result:=POCAValueNull;
 if (aCountArguments<1) or not assigned(pvLocalization) then begin
  exit;
 end;
 if aCountArguments>1 then begin
  Grouping:=POCAGetBooleanValue(aContext,aArguments^[1]);
 end else begin
  Grouping:=true;
 end;
 result:=POCANewString(aContext,pvLocalization.FormatInteger(trunc(POCAGetNumberValue(aContext,aArguments^[0])),Grouping));
end;

// engine.localization.getLanguage() -> the tag of the current language, for example "de" or "pt-BR"
function POCALocalizationGetLanguage(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
begin
 if assigned(pvLocalization) then begin
  result:=POCANewString(aContext,pvLocalization.LanguageTag);
 end else begin
  result:=POCAValueNull;
 end;
end;

// engine.localization.setLanguage(tag) -> true when the tag is one of the available languages. An unknown tag
// does fall back to the source language, in the same way as the Pascal side does it.
function POCALocalizationSetLanguage(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var LanguageTag:TpvUTF8String;
begin
 result:=POCAValueNull;
 if (aCountArguments<1) or not assigned(pvLocalization) then begin
  exit;
 end;
 LanguageTag:=POCAGetStringValue(aContext,aArguments^[0]);
 pvLocalization.SetLanguage(LanguageTag);
 result:=POCANewNumber(aContext,ord(pvLocalization.LanguageTag=LanguageTag));
end;

// engine.localization.getLanguages() -> an array of hashes with the tag, the English name, the native name and
// the fallback tag of every available language, for a language selection in the user interface
function POCALocalizationGetLanguages(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Index:TpvSizeInt;
    Language:TpvLocalizationLanguage;
    LanguageHash:TPOCAValue;
begin
 result:=POCANewArray(aContext);
 if assigned(pvLocalization) then begin
  for Index:=0 to pvLocalization.Languages.Count-1 do begin
   Language:=pvLocalization.Languages[Index];
   LanguageHash:=POCANewHash(aContext);
   POCAHashSetString(aContext,LanguageHash,'tag',POCANewString(aContext,Language.Tag),false);
   POCAHashSetString(aContext,LanguageHash,'name',POCANewString(aContext,Language.Name),false);
   POCAHashSetString(aContext,LanguageHash,'nativeName',POCANewString(aContext,Language.NativeName),false);
   POCAHashSetString(aContext,LanguageHash,'fallback',POCANewString(aContext,Language.FallbackTag),false);
   POCAArrayPush(result,LanguageHash);
  end;
 end;
end;

// engine.localization.getGeneration() -> a counter which does increment on every language change, so that a
// script can throw away its cached texts and its cached text layouts exactly when it does need to
function POCALocalizationGetGeneration(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
begin
 if assigned(pvLocalization) then begin
  result:=POCANewNumber(aContext,pvLocalization.Generation);
 end else begin
  result:=POCANewNumber(aContext,0);
 end;
end;

// engine.localization.getSystemLanguage() -> the language which the system of the user does suggest, as a tag,
// or an empty string when it could not be determined
function POCALocalizationGetSystemLanguage(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
begin
 result:=POCANewString(aContext,TpvLocalization.GetSystemLanguageTag);
end;

// engine.localization.matchLanguage(["pt-BR","pt","en"]) -> the best matching available language for the wish
// list, or an empty string when nothing does match. The tags can be given as an array or as single arguments.
function POCALocalizationMatchLanguage(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Index,Count:TpvSizeInt;
    WishedLanguageTags:array of TpvUTF8String;
begin
 result:=POCAValueNull;
 if (aCountArguments<1) or not assigned(pvLocalization) then begin
  exit;
 end;
 WishedLanguageTags:=nil;
 try
  if POCAGetValueType(aArguments^[0])=pvtARRAY then begin
   Count:=POCAArraySize(aArguments^[0]);
   SetLength(WishedLanguageTags,Count);
   for Index:=0 to Count-1 do begin
    WishedLanguageTags[Index]:=POCAGetStringValue(aContext,POCAArrayGet(aArguments^[0],Index));
   end;
  end else begin
   SetLength(WishedLanguageTags,aCountArguments);
   for Index:=0 to aCountArguments-1 do begin
    WishedLanguageTags[Index]:=POCAGetStringValue(aContext,aArguments^[Index]);
   end;
  end;
  result:=POCANewString(aContext,pvLocalization.MatchLanguage(WishedLanguageTags));
 finally
  WishedLanguageTags:=nil;
 end;
end;

procedure InitializePOCALocalizationContext(const aContext:PPOCAContext;const aEngineHash:TPOCAValue);
var LocalizationHash:TPOCAValue;
begin

 LocalizationHash:=POCANewHash(aContext);
 POCAHashSetString(aContext,aEngineHash,'localization',LocalizationHash);

 POCAAddNativeFunction(aContext,LocalizationHash,'tr',@POCALocalizationTranslate,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'trPlural',@POCALocalizationTranslatePlural,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'trFormat',@POCALocalizationFormat,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'formatText',@POCALocalizationFormatTextOnly,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'formatNumber',@POCALocalizationFormatNumber,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'formatInteger',@POCALocalizationFormatInteger,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'getLanguage',@POCALocalizationGetLanguage,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'setLanguage',@POCALocalizationSetLanguage,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'getLanguages',@POCALocalizationGetLanguages,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'getGeneration',@POCALocalizationGetGeneration,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'getSystemLanguage',@POCALocalizationGetSystemLanguage,nil,nil);
 POCAAddNativeFunction(aContext,LocalizationHash,'matchLanguage',@POCALocalizationMatchLanguage,nil,nil);

end;

end.
