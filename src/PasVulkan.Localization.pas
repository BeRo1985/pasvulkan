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
unit PasVulkan.Localization;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

// The translation catalogs themselves are kept by PasMultiLang, which does the PO, MO and own binary format
// loading, the plural form rules of a catalog and the thread safe lookup. What this unit does add on top of it
// is everything which is about a running application rather than about a single catalog: the list of available
// languages, the fallback chain, the placeholder and number formatting, a cheap cache for text which is drawn
// every frame, a change notification for rebuilding cached layouts, the pseudo language for finding layout
// overflows and hard coded strings, and the collecting of missing keys.
//
// A translated string is addressed by a symbolic key plus an inline default, for example
//
//   Tr('menu.settings.audio.globalvolume','Global volume')
//
// The key is the msgid inside the catalogs, and the inline default is what is drawn when no catalog knows the
// key, so a missing or incomplete catalog can never break the application. Keeping the key rather than the
// English text as the msgid does mean that fixing an English wording does not invalidate any translation.
// A msgctxt is therefore not needed here and is not used, since the key disambiguates already.

interface

uses {$ifdef Windows}Windows,{$endif}
     SysUtils,
     Classes,
     Math,
     PasMP,
     PasJSON,
     PasMultiLang,
     PasVulkan.Types,
     PasVulkan.Collections;

type EpvLocalization=class(Exception);

     TpvLocalization=class;

     // Replaces every translated string by a visually distinct but still readable variant, for finding layout
     // overflows and strings which are not going through the translation at all
     TpvLocalizationPseudoMode=
      (
       None,      // Normal operation
       Accented,  // "Global volume" becomes "[Ğłöƀáł vöłűmé ~~~~]"
       Keys       // The key itself is drawn instead of the text, for finding out which key a string does use
      );

     TpvLocalizationLanguageChangedEvent=procedure(const aSender:TpvLocalization) of object;

     // The application decides where catalog files do come from, so that this unit does not need to know
     // anything about virtual file systems, asset archives or plain files. When it is not assigned, plain file
     // streams relative to the base path are used.
     TpvLocalizationOpenStreamEvent=function(const aFileName:TpvUTF8String;out aStream:TStream):boolean of object;

     { TpvLocalizationLanguage }
     TpvLocalizationLanguage=class
      private
       fTag:TpvUTF8String;
       fName:TpvUTF8String;
       fNativeName:TpvUTF8String;
       fFallbackTag:TpvUTF8String;
       fDecimalSeparator:TpvUTF8String;
       fThousandsSeparator:TpvUTF8String;
       fMetaData:TPasJSONItemObject;
      public
       constructor Create(const aTag:TpvUTF8String); reintroduce;
       destructor Destroy; override;
       procedure LoadFromJSON(const aJSONItem:TPasJSONItem);
      published
       property Tag:TpvUTF8String read fTag;
       property Name:TpvUTF8String read fName;
       property NativeName:TpvUTF8String read fNativeName;
       property FallbackTag:TpvUTF8String read fFallbackTag;
       property DecimalSeparator:TpvUTF8String read fDecimalSeparator;
       property ThousandsSeparator:TpvUTF8String read fThousandsSeparator;
       // The whole language entry of the manifest, so that an application can read its own additional fields
       // out of it, for example which font file and which code point ranges a language does need
       property MetaData:TPasJSONItemObject read fMetaData;
     end;

     TpvLocalizationLanguages=TpvObjectGenericList<TpvLocalizationLanguage>;

     TpvLocalizationLanguageHashMap=TpvStringHashMap<TpvLocalizationLanguage>;

     { TpvLocalizationCatalog }
     TpvLocalizationCatalog=class
      private
       fLanguageTag:TpvUTF8String;
       fDomain:TpvUTF8String;
       fFileName:TpvUTF8String;
       fMultiLang:TPasMultiLang;
      public
       constructor Create(const aLanguageTag,aDomain,aFileName:TpvUTF8String;const aMultiLang:TPasMultiLang); reintroduce;
       destructor Destroy; override;
      published
       property LanguageTag:TpvUTF8String read fLanguageTag;
       property Domain:TpvUTF8String read fDomain;
       property FileName:TpvUTF8String read fFileName;
       property MultiLang:TPasMultiLang read fMultiLang;
     end;

     TpvLocalizationCatalogs=TpvObjectGenericList<TpvLocalizationCatalog>;

     TpvLocalizationLanguageChangedEvents=array of TpvLocalizationLanguageChangedEvent;

     TpvLocalizationStrings=array of TpvUTF8String;

     { TpvLocalization }
     TpvLocalization=class
      private
       fLock:TPasMPMultipleReaderSingleWriterLock;
       fBasePath:TpvUTF8String;
       fSourceLanguageTag:TpvUTF8String;
       fLanguageTag:TpvUTF8String;
       fLanguages:TpvLocalizationLanguages;
       fLanguageHashMap:TpvLocalizationLanguageHashMap;
       fDomains:TpvLocalizationStrings;
       fCatalogs:TpvLocalizationCatalogs;
       fCurrentLanguage:TpvLocalizationLanguage;
       fGeneration:TpvUInt32;
       fPseudoMode:TpvLocalizationPseudoMode;
       fDecimalSeparator:TpvUTF8String;
       fThousandsSeparator:TpvUTF8String;
       fCollectMissingKeys:boolean;
       fMissingKeys:TPasMultiLang;
       fLanguageChangedEvents:TpvLocalizationLanguageChangedEvents;
       fOnOpenStream:TpvLocalizationOpenStreamEvent;
       function OpenStream(const aFileName:TpvUTF8String;out aStream:TStream):boolean;
       function BuildLanguageChain(const aLanguageTag:TpvUTF8String):TpvLocalizationStrings;
       procedure LoadCatalogsUnlocked(const aLanguageTag:TpvUTF8String);
       function LookUpUnlocked(const aKey:TpvUTF8String;const aCount:TpvInt64;const aHasCount:boolean;out aText:TpvUTF8String):boolean;
       procedure NoteMissingKey(const aKey,aDefault:TpvUTF8String);
       function PostProcess(const aKey,aText:TpvUTF8String):TpvUTF8String;
      protected
       class function VarRecToText(const aVarRec:TVarRec):TpvUTF8String; static;
       class function VarRecToNumber(const aVarRec:TVarRec;out aValue:TpvDouble):boolean; static;
       class function Pseudoize(const aText:TpvUTF8String):TpvUTF8String; static;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;

       // The manifest lists the available languages and the catalog domains, see the example at the end of this
       // unit. Loading it does not load any catalog yet, that happens with SetLanguage.
       procedure LoadManifestFromJSON(const aJSONItem:TPasJSONItem);
       procedure LoadManifestFromStream(const aStream:TStream);
       function LoadManifest(const aFileName:TpvUTF8String='locales/manifest.json'):boolean;

       // Switches the language, loads the catalogs of the whole fallback chain, increments the generation
       // counter and calls the registered language changed handlers, so that cached layouts can be rebuilt.
       // An unknown tag does fall back to the source language instead of raising an exception.
       procedure SetLanguage(const aLanguageTag:TpvUTF8String);

       function ExistLanguage(const aLanguageTag:TpvUTF8String):boolean;
       function GetLanguage(const aLanguageTag:TpvUTF8String):TpvLocalizationLanguage;

       // The language which the user's system does suggest, as a tag like "de" or "pt-BR", or an empty string
       // when it could not be determined. An application should prefer, in this order, an explicit setting of
       // its own, the platform store language, for example the Steam one, and this one.
       class function GetSystemLanguageTag:TpvUTF8String; static;

       // Returns the best matching available language for a wish list, for example
       // ['pt-BR','pt','en'], or an empty string when nothing does match
       function MatchLanguage(const aWishedLanguageTags:array of TpvUTF8String):TpvUTF8String;

       procedure AddLanguageChangedHandler(const aHandler:TpvLocalizationLanguageChangedEvent);
       procedure RemoveLanguageChangedHandler(const aHandler:TpvLocalizationLanguageChangedEvent);

       // The actual translation, where aDefault is the source language text as it is written in the code
       function Translate(const aKey,aDefault:TpvUTF8String):TpvUTF8String;

       // The plural form is chosen by the plural rule of the catalog which does contain the key, and by the
       // source language rule when no catalog does
       function TranslatePlural(const aKey,aDefaultSingular,aDefaultPlural:TpvUTF8String;const aCount:TpvInt64):TpvUTF8String;

       // Named placeholders in the form of {name} or {name:spec}, where the arguments are given as name and
       // value pairs, for example Format('hud.progress','{done} of {total}',['done',Done,'total',Total]).
       // The names are what a translator does see, so they can be reordered freely, which positional
       // placeholders would not allow. A literal brace is written as {{ or }}.
       // The optional spec is n for a grouped number, n<digits> for a grouped number with that many fraction
       // digits, and f<digits> for an ungrouped one.
       function Format(const aKey,aDefault:TpvUTF8String;const aArguments:array of const):TpvUTF8String;
       function FormatText(const aText:TpvUTF8String;const aArguments:array of const):TpvUTF8String;

       // Number formatting with the decimal and thousands separators of the current language
       function FormatNumber(const aValue:TpvDouble;const aFractionDigits:TpvInt32=-1;const aGrouping:boolean=true):TpvUTF8String;
       function FormatInteger(const aValue:TpvInt64;const aGrouping:boolean=true):TpvUTF8String;

       // Everything which was asked for but is not in any catalog, as a PO file, so that it can be fed into
       // the translation workflow. Only filled when CollectMissingKeys is true.
       procedure SaveMissingKeysToPOFile(const aFileName:TpvUTF8String);
       procedure ClearMissingKeys;
       function CountMissingKeys:TpvSizeInt;

       property Languages:TpvLocalizationLanguages read fLanguages;
       property Catalogs:TpvLocalizationCatalogs read fCatalogs;
       property CurrentLanguage:TpvLocalizationLanguage read fCurrentLanguage;
      published
       // Where the catalog files do live, the default is "locales/"
       property BasePath:TpvUTF8String read fBasePath write fBasePath;
       property SourceLanguageTag:TpvUTF8String read fSourceLanguageTag write fSourceLanguageTag;
       property LanguageTag:TpvUTF8String read fLanguageTag;
       // Increments on every language change, for invalidating cached texts and cached text layouts
       property Generation:TpvUInt32 read fGeneration;
       property PseudoMode:TpvLocalizationPseudoMode read fPseudoMode write fPseudoMode;
       property CollectMissingKeys:boolean read fCollectMissingKeys write fCollectMissingKeys;
       property OnOpenStream:TpvLocalizationOpenStreamEvent read fOnOpenStream write fOnOpenStream;
     end;

     { TpvLocalizedText }
     // A text which is drawn again and again, for example inside the HUD, where a hash map look-up per frame
     // and per string would be a waste. It does resolve itself once and again after every language change.
     TpvLocalizedText=record
      private
       fKey:TpvUTF8String;
       fDefault:TpvUTF8String;
       fText:TpvUTF8String;
       fGeneration:TpvUInt32;
       fResolved:boolean;
       function GetText:TpvUTF8String;
      public
       class function Create(const aKey,aDefault:TpvUTF8String):TpvLocalizedText; static;
       procedure Invalidate;
       property Key:TpvUTF8String read fKey;
       property Text:TpvUTF8String read GetText;
     end;

     PpvLocalizedText=^TpvLocalizedText;

var pvLocalization:TpvLocalization=nil;

// Short forms for the everyday use, which are safe to call before pvLocalization does even exist, so that unit
// initialization code can use them as well
function Tr(const aKey,aDefault:TpvUTF8String):TpvUTF8String;
function TrPlural(const aKey,aDefaultSingular,aDefaultPlural:TpvUTF8String;const aCount:TpvInt64):TpvUTF8String;
function TrFormat(const aKey,aDefault:TpvUTF8String;const aArguments:array of const):TpvUTF8String;

implementation

const CatalogFileExtensions:array[0..2] of TpvUTF8String=('.pml','.mo','.po');

{ TpvLocalizationLanguage }

constructor TpvLocalizationLanguage.Create(const aTag:TpvUTF8String);
begin
 inherited Create;
 fTag:=aTag;
 fName:=aTag;
 fNativeName:=aTag;
 fFallbackTag:='';
 fDecimalSeparator:='.';
 fThousandsSeparator:=',';
 fMetaData:=nil;
end;

destructor TpvLocalizationLanguage.Destroy;
begin
 FreeAndNil(fMetaData);
 inherited Destroy;
end;

procedure TpvLocalizationLanguage.LoadFromJSON(const aJSONItem:TPasJSONItem);
begin
 if assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject) then begin
  fTag:=TPasJSON.GetString(TPasJSONItemObject(aJSONItem).Properties['tag'],fTag);
  fName:=TPasJSON.GetString(TPasJSONItemObject(aJSONItem).Properties['name'],fTag);
  fNativeName:=TPasJSON.GetString(TPasJSONItemObject(aJSONItem).Properties['nativename'],fName);
  fFallbackTag:=TPasJSON.GetString(TPasJSONItemObject(aJSONItem).Properties['fallback'],'');
  fDecimalSeparator:=TPasJSON.GetString(TPasJSONItemObject(aJSONItem).Properties['decimalseparator'],'.');
  fThousandsSeparator:=TPasJSON.GetString(TPasJSONItemObject(aJSONItem).Properties['thousandsseparator'],',');
  FreeAndNil(fMetaData);
  fMetaData:=TPasJSONItemObject(aJSONItem.Clone);
 end;
end;

{ TpvLocalizationCatalog }

constructor TpvLocalizationCatalog.Create(const aLanguageTag,aDomain,aFileName:TpvUTF8String;const aMultiLang:TPasMultiLang);
begin
 inherited Create;
 fLanguageTag:=aLanguageTag;
 fDomain:=aDomain;
 fFileName:=aFileName;
 fMultiLang:=aMultiLang;
end;

destructor TpvLocalizationCatalog.Destroy;
begin
 FreeAndNil(fMultiLang);
 inherited Destroy;
end;

{ TpvLocalization }

constructor TpvLocalization.Create;
begin
 inherited Create;
 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;
 fBasePath:='locales/';
 fSourceLanguageTag:='en';
 fLanguageTag:='en';
 fLanguages:=TpvLocalizationLanguages.Create;
 fLanguages.OwnsObjects:=true;
 fLanguageHashMap:=TpvLocalizationLanguageHashMap.Create(nil);
 fDomains:=nil;
 fCatalogs:=TpvLocalizationCatalogs.Create;
 fCatalogs.OwnsObjects:=true;
 fCurrentLanguage:=nil;
 fGeneration:=1;
 fPseudoMode:=TpvLocalizationPseudoMode.None;
 fDecimalSeparator:='.';
 fThousandsSeparator:=',';
 fCollectMissingKeys:=false;
 fMissingKeys:=TPasMultiLang.Create;
 fLanguageChangedEvents:=nil;
 fOnOpenStream:=nil;
end;

destructor TpvLocalization.Destroy;
begin
 FreeAndNil(fCatalogs);
 FreeAndNil(fLanguageHashMap);
 FreeAndNil(fLanguages);
 FreeAndNil(fMissingKeys);
 FreeAndNil(fLock);
 fDomains:=nil;
 fLanguageChangedEvents:=nil;
 inherited Destroy;
end;

function TpvLocalization.OpenStream(const aFileName:TpvUTF8String;out aStream:TStream):boolean;
begin
 aStream:=nil;
 if assigned(fOnOpenStream) then begin
  result:=fOnOpenStream(aFileName,aStream) and assigned(aStream);
 end else begin
  result:=FileExists(String(aFileName));
  if result then begin
   try
    aStream:=TFileStream.Create(String(aFileName),fmOpenRead or fmShareDenyWrite);
   except
    aStream:=nil;
    result:=false;
   end;
  end;
 end;
end;

procedure TpvLocalization.LoadManifestFromJSON(const aJSONItem:TPasJSONItem);
var Index:TpvSizeInt;
    JSONItemObject:TPasJSONItemObject;
    JSONItemArray:TPasJSONItemArray;
    Language:TpvLocalizationLanguage;
begin
 if not (assigned(aJSONItem) and (aJSONItem is TPasJSONItemObject)) then begin
  raise EpvLocalization.Create('Localization manifest must be a JSON object');
 end;
 fLock.AcquireWrite;
 try
  JSONItemObject:=TPasJSONItemObject(aJSONItem);
  fSourceLanguageTag:=TPasJSON.GetString(JSONItemObject.Properties['sourcelanguage'],fSourceLanguageTag);
  fBasePath:=TPasJSON.GetString(JSONItemObject.Properties['basepath'],fBasePath);
  fDomains:=nil;
  if JSONItemObject.Properties['domains'] is TPasJSONItemArray then begin
   JSONItemArray:=TPasJSONItemArray(JSONItemObject.Properties['domains']);
   SetLength(fDomains,JSONItemArray.Count);
   for Index:=0 to JSONItemArray.Count-1 do begin
    fDomains[Index]:=TPasJSON.GetString(JSONItemArray.Items[Index],'');
   end;
  end;
  fLanguages.Clear;
  fLanguageHashMap.Clear;
  if JSONItemObject.Properties['languages'] is TPasJSONItemArray then begin
   JSONItemArray:=TPasJSONItemArray(JSONItemObject.Properties['languages']);
   for Index:=0 to JSONItemArray.Count-1 do begin
    Language:=TpvLocalizationLanguage.Create('');
    try
     Language.LoadFromJSON(JSONItemArray.Items[Index]);
    finally
     if length(Language.fTag)>0 then begin
      fLanguages.Add(Language);
      fLanguageHashMap[Language.fTag]:=Language;
     end else begin
      FreeAndNil(Language);
     end;
    end;
   end;
  end;
 finally
  fLock.ReleaseWrite;
 end;
end;

procedure TpvLocalization.LoadManifestFromStream(const aStream:TStream);
var JSONItem:TPasJSONItem;
begin
 // Standard JSON plus comments, so that a manifest can carry translator and maintainer notes. The encoding is
 // stated explicitly, since the automatic detection of PasJSON does fall back to Latin-1 without a byte order
 // mark, which would turn every native language name with non-ASCII characters into mojibake.
 JSONItem:=TPasJSON.Parse(aStream,[TPasJSONModeFlag.Comments],TPasJSONEncoding.UTF8);
 try
  LoadManifestFromJSON(JSONItem);
 finally
  FreeAndNil(JSONItem);
 end;
end;

function TpvLocalization.LoadManifest(const aFileName:TpvUTF8String='locales/manifest.json'):boolean;
var Stream:TStream;
begin
 result:=OpenStream(aFileName,Stream);
 if result then begin
  try
   LoadManifestFromStream(Stream);
  finally
   FreeAndNil(Stream);
  end;
 end;
end;

// The chain does go from the most specific language over its fallbacks to the source language, for example
// "pt-BR" -> "pt" -> "en", where a fallback which is named in the manifest does win over the derived one
function TpvLocalization.BuildLanguageChain(const aLanguageTag:TpvUTF8String):TpvLocalizationStrings;
var Count,Index,Position:TpvSizeInt;
    CurrentTag:TpvUTF8String;
    Language:TpvLocalizationLanguage;
    Found:boolean;
 procedure AddTag(const aTag:TpvUTF8String);
 var SubIndex:TpvSizeInt;
 begin
  if length(aTag)>0 then begin
   for SubIndex:=0 to Count-1 do begin
    if result[SubIndex]=aTag then begin
     exit;
    end;
   end;
   if Count>=length(result) then begin
    SetLength(result,(Count+1)*2);
   end;
   result[Count]:=aTag;
   inc(Count);
  end;
 end;
begin
 result:=nil;
 Count:=0;
 CurrentTag:=aLanguageTag;
 Index:=0;
 while (length(CurrentTag)>0) and (Index<16) do begin // The counter is just a cycle guard for broken manifests
  inc(Index);
  AddTag(CurrentTag);
  Found:=false;
  if fLanguageHashMap.TryGet(CurrentTag,Language) and assigned(Language) and (length(Language.fFallbackTag)>0) then begin
   CurrentTag:=Language.fFallbackTag;
   Found:=true;
  end;
  if not Found then begin
   // Without an explicit fallback, "pt-BR" does fall back to "pt"
   Position:=Pos(TpvUTF8String('-'),CurrentTag);
   if Position>0 then begin
    CurrentTag:=Copy(CurrentTag,1,Position-1);
   end else begin
    CurrentTag:='';
   end;
  end;
 end;
 AddTag(fSourceLanguageTag);
 SetLength(result,Count);
end;

procedure TpvLocalization.LoadCatalogsUnlocked(const aLanguageTag:TpvUTF8String);
var ChainIndex,DomainIndex,ExtensionIndex:TpvSizeInt;
    Chain:TpvLocalizationStrings;
    FileName,Domain,Tag:TpvUTF8String;
    Stream:TStream;
    MultiLang:TPasMultiLang;
    Loaded:boolean;
begin
 fCatalogs.Clear;
 Chain:=BuildLanguageChain(aLanguageTag);
 for ChainIndex:=0 to length(Chain)-1 do begin
  Tag:=Chain[ChainIndex];
  for DomainIndex:=0 to length(fDomains)-1 do begin
   Domain:=fDomains[DomainIndex];
   if length(Domain)=0 then begin
    continue;
   end;
   for ExtensionIndex:=0 to length(CatalogFileExtensions)-1 do begin
    FileName:=fBasePath+Tag+'/'+Domain+CatalogFileExtensions[ExtensionIndex];
    if OpenStream(FileName,Stream) then begin
     try
      MultiLang:=TPasMultiLang.Create;
      Loaded:=false;
      try
       case ExtensionIndex of
        0:begin
         MultiLang.LoadFromStream(Stream);
        end;
        1:begin
         MultiLang.LoadMOFromStream(Stream);
        end;
        else begin
         MultiLang.LoadPOFromStream(Stream);
        end;
       end;
       Loaded:=true;
      finally
       if Loaded then begin
        fCatalogs.Add(TpvLocalizationCatalog.Create(Tag,Domain,FileName,MultiLang));
       end else begin
        FreeAndNil(MultiLang);
       end;
      end;
     finally
      FreeAndNil(Stream);
     end;
     break; // Since the first existing file format of a domain does win
    end;
   end;
  end;
 end;
end;

procedure TpvLocalization.SetLanguage(const aLanguageTag:TpvUTF8String);
var Index:TpvSizeInt;
    Tag:TpvUTF8String;
    Language:TpvLocalizationLanguage;
begin
 fLock.AcquireWrite;
 try
  Tag:=aLanguageTag;
  if not (fLanguageHashMap.TryGet(Tag,Language) and assigned(Language)) then begin
   Tag:=fSourceLanguageTag;
   if not (fLanguageHashMap.TryGet(Tag,Language) and assigned(Language)) then begin
    Language:=nil;
   end;
  end;
  fLanguageTag:=Tag;
  fCurrentLanguage:=Language;
  if assigned(Language) then begin
   fDecimalSeparator:=Language.fDecimalSeparator;
   fThousandsSeparator:=Language.fThousandsSeparator;
  end else begin
   fDecimalSeparator:='.';
   fThousandsSeparator:=',';
  end;
  LoadCatalogsUnlocked(Tag);
  inc(fGeneration);
 finally
  fLock.ReleaseWrite;
 end;
 // The handlers are called outside of the lock on purpose, since they do usually translate strings themselves
 // while they are rebuilding their cached layouts
 for Index:=0 to length(fLanguageChangedEvents)-1 do begin
  if assigned(fLanguageChangedEvents[Index]) then begin
   fLanguageChangedEvents[Index](self);
  end;
 end;
end;

function TpvLocalization.ExistLanguage(const aLanguageTag:TpvUTF8String):boolean;
var Language:TpvLocalizationLanguage;
begin
 fLock.AcquireRead;
 try
  result:=fLanguageHashMap.TryGet(aLanguageTag,Language) and assigned(Language);
 finally
  fLock.ReleaseRead;
 end;
end;

function TpvLocalization.GetLanguage(const aLanguageTag:TpvUTF8String):TpvLocalizationLanguage;
begin
 fLock.AcquireRead;
 try
  if not fLanguageHashMap.TryGet(aLanguageTag,result) then begin
   result:=nil;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

function TpvLocalization.MatchLanguage(const aWishedLanguageTags:array of TpvUTF8String):TpvUTF8String;
var Index,SubIndex,Position:TpvSizeInt;
    Tag:TpvUTF8String;
    Language:TpvLocalizationLanguage;
begin
 result:='';
 fLock.AcquireRead;
 try
  for Index:=0 to length(aWishedLanguageTags)-1 do begin
   Tag:=aWishedLanguageTags[Index];
   while length(Tag)>0 do begin
    if fLanguageHashMap.TryGet(Tag,Language) and assigned(Language) then begin
     result:=Tag;
     exit;
    end;
    Position:=Pos(TpvUTF8String('-'),Tag);
    if Position>0 then begin
     Tag:=Copy(Tag,1,Position-1);
    end else begin
     Tag:='';
    end;
   end;
   // A wish for "pt" is also served by "pt-BR" when there is no plain "pt" at all
   Tag:=aWishedLanguageTags[Index];
   if length(Tag)>0 then begin
    for SubIndex:=0 to fLanguages.Count-1 do begin
     Language:=fLanguages[SubIndex];
     if (length(Language.fTag)>length(Tag)) and
        (Copy(Language.fTag,1,length(Tag)+1)=(Tag+'-')) then begin
      result:=Language.fTag;
      exit;
     end;
    end;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;
end;

procedure TpvLocalization.AddLanguageChangedHandler(const aHandler:TpvLocalizationLanguageChangedEvent);
var Index:TpvSizeInt;
begin
 for Index:=0 to length(fLanguageChangedEvents)-1 do begin
  if (TMethod(fLanguageChangedEvents[Index]).Code=TMethod(aHandler).Code) and
     (TMethod(fLanguageChangedEvents[Index]).Data=TMethod(aHandler).Data) then begin
   exit;
  end;
 end;
 SetLength(fLanguageChangedEvents,length(fLanguageChangedEvents)+1);
 fLanguageChangedEvents[length(fLanguageChangedEvents)-1]:=aHandler;
end;

procedure TpvLocalization.RemoveLanguageChangedHandler(const aHandler:TpvLocalizationLanguageChangedEvent);
var Index,SubIndex:TpvSizeInt;
begin
 for Index:=length(fLanguageChangedEvents)-1 downto 0 do begin
  if (TMethod(fLanguageChangedEvents[Index]).Code=TMethod(aHandler).Code) and
     (TMethod(fLanguageChangedEvents[Index]).Data=TMethod(aHandler).Data) then begin
   for SubIndex:=Index to length(fLanguageChangedEvents)-2 do begin
    fLanguageChangedEvents[SubIndex]:=fLanguageChangedEvents[SubIndex+1];
   end;
   SetLength(fLanguageChangedEvents,length(fLanguageChangedEvents)-1);
  end;
 end;
end;

// Walks the catalogs in fallback order and returns the first real hit. PasMultiLang does return the original
// string, which is the key here, when it does not know it or when the entry is not translated yet, so exactly
// that case is what makes the look-up fall through to the next catalog.
function TpvLocalization.LookUpUnlocked(const aKey:TpvUTF8String;const aCount:TpvInt64;const aHasCount:boolean;out aText:TpvUTF8String):boolean;
var Index:TpvSizeInt;
    Catalog:TpvLocalizationCatalog;
    Text:TpvUTF8String;
begin
 result:=false;
 aText:='';
 for Index:=0 to fCatalogs.Count-1 do begin
  Catalog:=fCatalogs[Index];
  if aHasCount then begin
   Text:=Catalog.fMultiLang.TranslatePlural(aKey,aKey,TPasMultiLangUInt64(aCount));
  end else begin
   Text:=Catalog.fMultiLang.Translate(aKey);
  end;
  if (length(Text)>0) and (Text<>aKey) then begin
   aText:=Text;
   result:=true;
   exit;
  end;
 end;
end;

procedure TpvLocalization.NoteMissingKey(const aKey,aDefault:TpvUTF8String);
begin
 if fCollectMissingKeys then begin
  fMissingKeys.Translate(aKey,0,true);
 end;
end;

function TpvLocalization.PostProcess(const aKey,aText:TpvUTF8String):TpvUTF8String;
begin
 case fPseudoMode of
  TpvLocalizationPseudoMode.Accented:begin
   result:=Pseudoize(aText);
  end;
  TpvLocalizationPseudoMode.Keys:begin
   result:='«'+aKey+'»';
  end;
  else begin
   result:=aText;
  end;
 end;
end;

function TpvLocalization.Translate(const aKey,aDefault:TpvUTF8String):TpvUTF8String;
var Found:boolean;
begin
 fLock.AcquireRead;
 try
  Found:=LookUpUnlocked(aKey,0,false,result);
 finally
  fLock.ReleaseRead;
 end;
 if not Found then begin
  NoteMissingKey(aKey,aDefault);
  if length(aDefault)>0 then begin
   result:=aDefault;
  end else begin
   result:=aKey;
  end;
 end;
 result:=PostProcess(aKey,result);
end;

function TpvLocalization.TranslatePlural(const aKey,aDefaultSingular,aDefaultPlural:TpvUTF8String;const aCount:TpvInt64):TpvUTF8String;
var Found:boolean;
begin
 fLock.AcquireRead;
 try
  Found:=LookUpUnlocked(aKey,aCount,true,result);
 finally
  fLock.ReleaseRead;
 end;
 if not Found then begin
  NoteMissingKey(aKey,aDefaultSingular);
  // Without a catalog, the plural rule of the source language does apply, which is the English one, just like
  // the GNU gettext runtime does it as well
  if aCount=1 then begin
   result:=aDefaultSingular;
  end else begin
   result:=aDefaultPlural;
  end;
  if length(result)=0 then begin
   result:=aKey;
  end;
 end;
 result:=PostProcess(aKey,result);
end;

class function TpvLocalization.VarRecToText(const aVarRec:TVarRec):TpvUTF8String;
begin
 case aVarRec.VType of
  vtInteger:begin
   result:=TpvUTF8String(IntToStr(aVarRec.VInteger));
  end;
  vtInt64:begin
   result:=TpvUTF8String(IntToStr(aVarRec.VInt64^));
  end;
{$ifdef fpc}
  vtQWord:begin
   result:=TpvUTF8String(IntToStr(aVarRec.VQWord^));
  end;
{$endif}
  vtBoolean:begin
   if aVarRec.VBoolean then begin
    result:='true';
   end else begin
    result:='false';
   end;
  end;
  vtChar:begin
   result:=TpvUTF8String(aVarRec.VChar);
  end;
  vtWideChar:begin
   result:=TpvUTF8String(UTF8Encode(WideString(aVarRec.VWideChar)));
  end;
  vtString:begin
   result:=TpvUTF8String(aVarRec.VString^);
  end;
  vtAnsiString:begin
   result:=TpvUTF8String(AnsiString(aVarRec.VAnsiString));
  end;
  vtWideString:begin
   result:=TpvUTF8String(UTF8Encode(WideString(aVarRec.VWideString)));
  end;
{$ifdef fpc}
  vtUnicodeString:begin
   result:=TpvUTF8String(UTF8Encode(UnicodeString(aVarRec.VUnicodeString)));
  end;
{$endif}
  vtPChar:begin
   result:=TpvUTF8String(AnsiString(aVarRec.VPChar));
  end;
  vtExtended:begin
   result:=TpvUTF8String(FloatToStr(aVarRec.VExtended^));
  end;
  vtCurrency:begin
   result:=TpvUTF8String(CurrToStr(aVarRec.VCurrency^));
  end;
  else begin
   result:='';
  end;
 end;
end;

class function TpvLocalization.VarRecToNumber(const aVarRec:TVarRec;out aValue:TpvDouble):boolean;
begin
 result:=true;
 case aVarRec.VType of
  vtInteger:begin
   aValue:=aVarRec.VInteger;
  end;
  vtInt64:begin
   aValue:=aVarRec.VInt64^;
  end;
{$ifdef fpc}
  vtQWord:begin
   aValue:=aVarRec.VQWord^;
  end;
{$endif}
  vtExtended:begin
   aValue:=aVarRec.VExtended^;
  end;
  vtCurrency:begin
   aValue:=aVarRec.VCurrency^;
  end;
  else begin
   aValue:=0.0;
   result:=false;
  end;
 end;
end;

function TpvLocalization.FormatNumber(const aValue:TpvDouble;const aFractionDigits:TpvInt32=-1;const aGrouping:boolean=true):TpvUTF8String;
var Digits,Index,Count:TpvSizeInt;
    Text,IntegerPart,FractionPart:TpvUTF8String;
    Negative:boolean;
    Position:TpvSizeInt;
begin
 Digits:=aFractionDigits;
 if Digits<0 then begin
  if SameValue(aValue,Round(aValue)) then begin
   Digits:=0;
  end else begin
   Digits:=2;
  end;
 end;
 Negative:=aValue<0.0;
 Text:=TpvUTF8String(FloatToStrF(abs(aValue),ffFixed,15,Digits));
 // FloatToStrF does use the separators of the current locale of the process, so the parts are split apart here
 // and put back together with the separators of the current language instead
 Position:=0;
 for Index:=1 to length(Text) do begin
  if not (Text[Index] in ['0'..'9']) then begin
   Position:=Index;
   break;
  end;
 end;
 if Position>0 then begin
  IntegerPart:=Copy(Text,1,Position-1);
  FractionPart:=Copy(Text,Position+1,length(Text)-Position);
 end else begin
  IntegerPart:=Text;
  FractionPart:='';
 end;
 if aGrouping and (length(fThousandsSeparator)>0) and (length(IntegerPart)>3) then begin
  Text:='';
  Count:=0;
  for Index:=length(IntegerPart) downto 1 do begin
   Text:=IntegerPart[Index]+Text;
   inc(Count);
   if ((Count mod 3)=0) and (Index>1) then begin
    Text:=fThousandsSeparator+Text;
   end;
  end;
  IntegerPart:=Text;
 end;
 result:=IntegerPart;
 if length(FractionPart)>0 then begin
  result:=result+fDecimalSeparator+FractionPart;
 end;
 if Negative then begin
  result:='-'+result;
 end;
end;

function TpvLocalization.FormatInteger(const aValue:TpvInt64;const aGrouping:boolean=true):TpvUTF8String;
begin
 result:=FormatNumber(aValue,0,aGrouping);
end;

function TpvLocalization.FormatText(const aText:TpvUTF8String;const aArguments:array of const):TpvUTF8String;
var Index,Len,Start,ArgumentIndex,FractionDigits:TpvSizeInt;
    Name,Spec,Replacement:TpvUTF8String;
    Value:TpvDouble;
    Found:boolean;
begin
 result:='';
 Index:=1;
 Len:=length(aText);
 while Index<=Len do begin
  case aText[Index] of
   '{':begin
    if (Index<Len) and (aText[Index+1]='{') then begin
     result:=result+'{';
     inc(Index,2);
    end else begin
     Start:=Index+1;
     Index:=Start;
     while (Index<=Len) and (aText[Index]<>'}') do begin
      inc(Index);
     end;
     if Index>Len then begin
      result:=result+Copy(aText,Start-1,(Len-Start)+2); // An unterminated placeholder is passed through as is
      break;
     end;
     Name:=Copy(aText,Start,Index-Start);
     inc(Index);
     Spec:='';
     Start:=Pos(TpvUTF8String(':'),Name);
     if Start>0 then begin
      Spec:=Copy(Name,Start+1,length(Name)-Start);
      Name:=Copy(Name,1,Start-1);
     end;
     Found:=false;
     Replacement:='';
     ArgumentIndex:=0;
     while (ArgumentIndex+1)<=High(aArguments) do begin
      if VarRecToText(aArguments[ArgumentIndex])=Name then begin
       if (length(Spec)>0) and (Spec[1] in ['n','N','f','F']) and VarRecToNumber(aArguments[ArgumentIndex+1],Value) then begin
        if length(Spec)>1 then begin
         FractionDigits:=StrToIntDef(String(Copy(Spec,2,length(Spec)-1)),-1);
        end else begin
         FractionDigits:=-1;
        end;
        Replacement:=FormatNumber(Value,FractionDigits,Spec[1] in ['n','N']);
       end else begin
        Replacement:=VarRecToText(aArguments[ArgumentIndex+1]);
       end;
       Found:=true;
       break;
      end;
      inc(ArgumentIndex,2);
     end;
     if Found then begin
      result:=result+Replacement;
     end else begin
      result:=result+'{'+Name+'}'; // An unknown name stays visible on purpose, since that is a bug to be seen
     end;
    end;
   end;
   '}':begin
    if (Index<Len) and (aText[Index+1]='}') then begin
     result:=result+'}';
     inc(Index,2);
    end else begin
     result:=result+'}';
     inc(Index);
    end;
   end;
   else begin
    result:=result+aText[Index];
    inc(Index);
   end;
  end;
 end;
end;

function TpvLocalization.Format(const aKey,aDefault:TpvUTF8String;const aArguments:array of const):TpvUTF8String;
begin
 result:=FormatText(Translate(aKey,aDefault),aArguments);
end;

// Maps ASCII letters to similar looking accented ones and does pad the text, so that a text which is not going
// through the translation does stay readable while everything else does look obviously foreign, and so that a
// layout which cannot take the roughly thirty percent of length which German or Russian do need does break
// here already. Placeholders are left alone, since breaking them would break the formatting afterwards.
class function TpvLocalization.Pseudoize(const aText:TpvUTF8String):TpvUTF8String;
const LowerCaseReplacements:array[0..25] of TpvUTF8String=
       ('á','ƀ','ć','đ','é','ƒ','ğ','ĥ','í','ĵ','ķ','ł','ɱ','ń','ö','ƥ','ɋ','ŕ','ś','ţ','ű','ṽ','ŵ','ẋ','ý','ź');
      UpperCaseReplacements:array[0..25] of TpvUTF8String=
       ('Á','Ɓ','Ć','Đ','É','Ƒ','Ğ','Ĥ','Í','Ĵ','Ķ','Ł','Ṁ','Ń','Ö','Ƥ','Ɋ','Ŕ','Ś','Ţ','Ű','Ṽ','Ŵ','Ẋ','Ý','Ź');
var Index,Len,PaddingCount:TpvSizeInt;
    Inside:boolean;
begin
 result:='«';
 Index:=1;
 Len:=length(aText);
 Inside:=false;
 while Index<=Len do begin
  case aText[Index] of
   '{':begin
    Inside:=true;
    result:=result+aText[Index];
   end;
   '}':begin
    Inside:=false;
    result:=result+aText[Index];
   end;
   'a'..'z':begin
    if Inside then begin
     result:=result+aText[Index];
    end else begin
     result:=result+LowerCaseReplacements[ord(aText[Index])-ord('a')];
    end;
   end;
   'A'..'Z':begin
    if Inside then begin
     result:=result+aText[Index];
    end else begin
     result:=result+UpperCaseReplacements[ord(aText[Index])-ord('A')];
    end;
   end;
   else begin
    result:=result+aText[Index];
   end;
  end;
  inc(Index);
 end;
 PaddingCount:=Max(1,(Len+2) div 3);
 for Index:=1 to PaddingCount do begin
  result:=result+'~';
 end;
 result:=result+'»';
end;

procedure TpvLocalization.SaveMissingKeysToPOFile(const aFileName:TpvUTF8String);
begin
 fMissingKeys.SavePOToFile(String(aFileName));
end;

procedure TpvLocalization.ClearMissingKeys;
begin
 fMissingKeys.Clear;
end;

function TpvLocalization.CountMissingKeys:TpvSizeInt;
var Stream:TMemoryStream;
begin
 // TPasMultiLang does not expose a count, so the cheap way without touching the library is used here
 result:=0;
 Stream:=TMemoryStream.Create;
 try
  fMissingKeys.SaveToStream(Stream);
  if Stream.Size>=12 then begin
   Stream.Seek(8,soBeginning);
   Stream.ReadBuffer(result,SizeOf(TpvUInt32));
   result:=TpvUInt32(result);
  end;
 finally
  FreeAndNil(Stream);
 end;
end;

class function TpvLocalization.GetSystemLanguageTag:TpvUTF8String;
{$ifdef Windows}
var Buffer:array[0..85] of AnsiChar;
    LanguageName,CountryName:TpvUTF8String;
begin
 result:='';
 FillChar(Buffer,SizeOf(Buffer),#0);
 if GetLocaleInfoA(LOCALE_USER_DEFAULT,LOCALE_SISO639LANGNAME,@Buffer[0],length(Buffer))>0 then begin
  LanguageName:=TpvUTF8String(PAnsiChar(@Buffer[0]));
  FillChar(Buffer,SizeOf(Buffer),#0);
  if GetLocaleInfoA(LOCALE_USER_DEFAULT,LOCALE_SISO3166CTRYNAME,@Buffer[0],length(Buffer))>0 then begin
   CountryName:=TpvUTF8String(PAnsiChar(@Buffer[0]));
  end else begin
   CountryName:='';
  end;
  if length(CountryName)>0 then begin
   result:=LanguageName+'-'+CountryName;
  end else begin
   result:=LanguageName;
  end;
 end;
end;
{$else}
var Index:TpvSizeInt;
    Value:TpvUTF8String;
begin
 result:='';
 Value:=TpvUTF8String(GetEnvironmentVariable('LC_ALL'));
 if length(Value)=0 then begin
  Value:=TpvUTF8String(GetEnvironmentVariable('LC_MESSAGES'));
 end;
 if length(Value)=0 then begin
  Value:=TpvUTF8String(GetEnvironmentVariable('LANG'));
 end;
 // "de_DE.UTF-8" and "de_DE@euro" both do become "de-DE" here
 for Index:=1 to length(Value) do begin
  case Value[Index] of
   '.',
   '@',
   ':':begin
    Value:=Copy(Value,1,Index-1);
    break;
   end;
   '_':begin
    Value[Index]:='-';
   end;
   else begin
   end;
  end;
 end;
 if (Value='C') or (Value='POSIX') then begin
  Value:='';
 end;
 result:=Value;
end;
{$endif}

{ TpvLocalizedText }

class function TpvLocalizedText.Create(const aKey,aDefault:TpvUTF8String):TpvLocalizedText;
begin
 result.fKey:=aKey;
 result.fDefault:=aDefault;
 result.fText:=aDefault;
 result.fGeneration:=0;
 result.fResolved:=false;
end;

procedure TpvLocalizedText.Invalidate;
begin
 fResolved:=false;
end;

function TpvLocalizedText.GetText:TpvUTF8String;
begin
 if assigned(pvLocalization) then begin
  if (not fResolved) or (fGeneration<>pvLocalization.Generation) then begin
   fText:=pvLocalization.Translate(fKey,fDefault);
   fGeneration:=pvLocalization.Generation;
   fResolved:=true;
  end;
  result:=fText;
 end else begin
  result:=fDefault;
 end;
end;

function Tr(const aKey,aDefault:TpvUTF8String):TpvUTF8String;
begin
 if assigned(pvLocalization) then begin
  result:=pvLocalization.Translate(aKey,aDefault);
 end else begin
  result:=aDefault;
 end;
end;

function TrPlural(const aKey,aDefaultSingular,aDefaultPlural:TpvUTF8String;const aCount:TpvInt64):TpvUTF8String;
begin
 if assigned(pvLocalization) then begin
  result:=pvLocalization.TranslatePlural(aKey,aDefaultSingular,aDefaultPlural,aCount);
 end else if aCount=1 then begin
  result:=aDefaultSingular;
 end else begin
  result:=aDefaultPlural;
 end;
end;

function TrFormat(const aKey,aDefault:TpvUTF8String;const aArguments:array of const):TpvUTF8String;
begin
 if assigned(pvLocalization) then begin
  result:=pvLocalization.Format(aKey,aDefault,aArguments);
 end else begin
  result:=aDefault;
 end;
end;

initialization
 pvLocalization:=TpvLocalization.Create;
finalization
 FreeAndNil(pvLocalization);
end.
