program i18ntest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$scopedenums on}

uses {$ifdef fpc}
     {$ifdef unix}
     cthreads,
     {$endif}
     {$endif}
     SysUtils,
     Classes,
     PasMP,
     PUCU,
     PasDblStrUtils,
     PasJSON,
     PasMultiLang,
     POCA,
     PasVulkan.Types,
     PasVulkan.Localization,
     PasVulkan.POCA.Localization;

const TestScript:TPOCARawByteString=
       'puts("  language        = " ~ engine.localization.getLanguage());'+#10+
       'puts("  tr              = " ~ engine.localization.tr("hud.button.inventory","Inventory"));'+#10+
       'puts("  trPlural(1)     = " ~ engine.localization.trPlural("hud.items","one item","many items",1));'+#10+
       'puts("  trPlural(3)     = " ~ engine.localization.trPlural("hud.items","one item","many items",3));'+#10+
       'puts("  trFormat        = " ~ engine.localization.trFormat("hud.progress","{done} of {total}","done",3,"total",7));'+#10+
       'puts("  trFormat spec   = " ~ engine.localization.trFormat("hud.credits","{have:n2} of {total:n} credits","have",1234.5,"total",2000));'+#10+
       'puts("  trFormat utf8   = " ~ engine.localization.trFormat("hud.pickup","Collected {item}","item","Prügelkäfer"));'+#10+
       'puts("  unknown name    = " ~ engine.localization.trFormat("hud.none","{a} and {b}","a",1));'+#10+
       'puts("  formatNumber    = " ~ engine.localization.formatNumber(1234.5,2,true));'+#10+
       'puts("  formatInteger   = " ~ engine.localization.formatInteger(1234567));'+#10+
       'puts("  generation      = " ~ engine.localization.getGeneration());'+#10+
       'puts("  systemLanguage  = " ~ engine.localization.getSystemLanguage());'+#10+
       'puts("  matchLanguage   = " ~ engine.localization.matchLanguage(["pt-BR","de","en"]));'+#10+
       'let languages = engine.localization.getLanguages();'+#10+
       'puts("  languages       = " ~ languages.length);'+#10+
       'for(let i = 0; i < languages.length; i++) {'+#10+
       ' puts("    " ~ languages[i].tag ~ " / " ~ languages[i].name ~ " / " ~ languages[i].nativeName);'+#10+
       '}'+#10;

var Instance:PPOCAInstance;
    Context:PPOCAContext;
    EngineHash:TPOCAValue;

procedure RunTestScript(const aTitle:string);
var Code:TPOCAValue;
begin
 writeln;
 writeln('=== ',aTitle,' ===');
 Code:=POCACompile(Instance,Context,TestScript,'<i18ntest>');
 POCACall(Context,Code,[],POCAValueNull,Instance^.Globals.Namespace);
end;

begin

 Instance:=POCAInstanceCreate;
 try

  Context:=POCAContextCreate(Instance);
  try

   EngineHash:=POCANewHash(Context);
   POCAHashSetString(Context,Instance^.Globals.Namespace,'engine',EngineHash);

   InitializePOCALocalizationContext(Context,EngineHash);

   RunTestScript('Phase 1 - no manifest, no catalogs, inline defaults only');

   if pvLocalization.LoadManifest('locales/manifest.json') then begin
    writeln;
    writeln('Manifest loaded, ',pvLocalization.Languages.Count,' languages');
   end else begin
    writeln;
    writeln('Manifest NOT loaded');
   end;

   pvLocalization.SetLanguage('de');
   writeln('Catalogs after SetLanguage("de"): ',pvLocalization.Catalogs.Count);

   RunTestScript('Phase 2 - German catalog');

   pvLocalization.PseudoMode:=TpvLocalizationPseudoMode.Accented;
   RunTestScript('Phase 3 - pseudo localization for layout overflow testing');
   pvLocalization.PseudoMode:=TpvLocalizationPseudoMode.None;

   pvLocalization.SetLanguage('en');
   RunTestScript('Phase 4 - back to English');

  finally
   POCAContextDestroy(Context);
  end;

 finally
  POCAInstanceDestroy(Instance);
 end;

end.
