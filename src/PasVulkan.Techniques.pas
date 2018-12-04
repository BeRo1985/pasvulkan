(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2018, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Techniques;
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
     Vulkan,
     PasJSON,
     PasVulkan.Types,
     PasVulkan.Collections,
     PasVulkan.JSON;

type TpvTechniques=class
      public
       type TTechnique=class;
            TTechniqueList=TpvObjectGenericList<TTechnique>;
            TTechniqueNameMap=TpvStringHashMap<TTechnique>;
            TTechnique=class
             public
              type TPass=class;
                   TPassList=TpvObjectGenericList<TPass>;
                   TPass=class
                    private
                     fTechnique:TTechnique;
                     fIndex:TpvSizeInt;
                     fName:TpvUTF8String;
                     procedure LoadFromJSONObject(const aRootJSONObject:TPasJSONItemObject);
                    public
                     constructor Create(const aTechnique:TTechnique); reintroduce;
                     destructor Destroy; override;
                   end;
             private
              fParent:TpvTechniques;
              fName:TpvUTF8String;
              fVariantTechniqueNameMap:TTechniqueNameMap;
              fPasses:TPassList;
              procedure LoadFromJSONObject(const aRootJSONObject:TPasJSONItemObject);
             public
              constructor Create(const aParent:TpvTechniques); reintroduce;
              destructor Destroy; override;
             published
              property VariantTechniqueByName:TTechniqueNameMap read fVariantTechniqueNameMap;
              property Passes:TPassList read fPasses;
            end;
      private
       fPath:TpvUTF8String;
       fTechniques:TTechniqueList;
       fTechniqueNameMap:TTechniqueNameMap;
      public
       constructor Create(const aPath:TpvUTF8String='techniques');
       destructor Destroy; override;
      published
       property TechniqueByName:TTechniqueNameMap read fTechniqueNameMap;
     end;

implementation

uses PasVulkan.Application;

{ TpvTechniques.TTechnique.TPass }

constructor TpvTechniques.TTechnique.TPass.Create(const aTechnique:TTechnique);
begin

 inherited Create;

 fTechnique:=aTechnique;

 fIndex:=-1;

 fName:='';

end;

destructor TpvTechniques.TTechnique.TPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvTechniques.TTechnique.TPass.LoadFromJSONObject(const aRootJSONObject:TPasJSONItemObject);
begin

end;

{ TpvTechniques.TTechnique }

constructor TpvTechniques.TTechnique.Create(const aParent:TpvTechniques);
begin

 inherited Create;

 fParent:=aParent;

 fVariantTechniqueNameMap:=TTechniqueNameMap.Create(nil);

 fPasses:=TPassList.Create;

 end;

destructor TpvTechniques.TTechnique.Destroy;
begin

 FreeAndNil(fPasses);

 FreeAndNil(fVariantTechniqueNameMap);

 inherited Destroy;
end;

procedure TpvTechniques.TTechnique.LoadFromJSONObject(const aRootJSONObject:TPasJSONItemObject);
var SectionJSONItem,JSONItem:TPasJSONItem;
    SectionJSONItemObject:TPasJSONItemObject;
    JSONItemObjectProperty:TPasJSONItemObjectProperty;
    VariantTechniqueName:TpvUTF8String;
    VariantTechnique:TTechnique;
    Pass:TPass;
begin

 begin
  SectionJSONItem:=aRootJSONObject.Properties['variants'];
  if assigned(SectionJSONItem) and (SectionJSONItem is TPasJSONItemObject) then begin
   SectionJSONItemObject:=TPasJSONItemObject(SectionJSONItem);
   for JSONItemObjectProperty in SectionJSONItemObject do begin
    if length(JSONItemObjectProperty.Key)>0 then begin
     VariantTechniqueName:=TPasJSON.GetString(JSONItemObjectProperty.Value,'');
     if length(VariantTechniqueName)>0 then begin
      VariantTechnique:=fParent.fTechniqueNameMap[VariantTechniqueName];
      if assigned(VariantTechnique) then begin
       fVariantTechniqueNameMap.Add(JSONItemObjectProperty.Key,VariantTechnique);
      end;
     end;
    end;
   end;
  end;
 end;

 begin
  SectionJSONItem:=aRootJSONObject.Properties['passes'];
  if assigned(SectionJSONItem) and (SectionJSONItem is TPasJSONItemObject) then begin
   SectionJSONItemObject:=TPasJSONItemObject(SectionJSONItem);
   for JSONItemObjectProperty in SectionJSONItemObject do begin
    if length(JSONItemObjectProperty.Key)>0 then begin
     if assigned(JSONItemObjectProperty.Value) and
        (JSONItemObjectProperty.Value is TPasJSONItemObject) then begin
      Pass:=TPass.Create(self);
      try
       Pass.fName:=JSONItemObjectProperty.Key;
      finally
       Pass.fIndex:=fPasses.Add(Pass);
      end;
      Pass.LoadFromJSONObject(TPasJSONItemObject(JSONItemObjectProperty.Value));
     end;
    end;
   end;
  end;
 end;

end;

{ TpvTechniques }

constructor TpvTechniques.Create(const aPath:TpvUTF8String='techniques');
var FileNameList:TpvApplicationAssets.TFileNameList;
    FileName:TpvUTF8String;
    Stream:TStream;
    JSONTechniques:TPasJSONItemObject;
    CurrentJSON:TPasJSONItem;
    BaseJSONItem,JSONItem:TPasJSONItem;
    BaseJSONItemObject:TPasJSONItemObject;
    JSONItemObjectProperty:TPasJSONItemObjectProperty;
    Technique:TTechnique;
begin

 inherited Create;

 fTechniques:=TTechniqueList.Create;
 fTechniques.OwnsObjects:=true;

 fTechniqueNameMap:=TTechniqueNameMap.Create(nil);

 fPath:=aPath;
 if (length(fPath)>0) and (fPath[length(fPath)] in ['/','\']) then begin
  Delete(fPath,length(fPath),1);
 end;

 JSONTechniques:=TPasJSONItemObject.Create;
 try

  FileNameList:=pvApplication.Assets.GetDirectoryFileList(fPath);
  for FileName in FileNameList do begin
   if ExtractFileExt(String(FileName))='.techniques' then begin
    Stream:=pvApplication.Assets.GetAssetStream(fPath+'/'+FileName);
    if assigned(Stream) then begin
     try
      CurrentJSON:=TPasJSON.Parse(Stream,TPasJSON.SimplifiedJSONModeFlags+[TPasJSONModeFlag.HexadecimalNumbers]);
      if assigned(CurrentJSON) then begin
       try
        if CurrentJSON is TPasJSONItemObject then begin
         JSONTechniques.Merge(CurrentJSON,[TPasJSONMergeFlag.ForceObjectPropertyValueDestinationType]);
        end;
       finally
        FreeAndNil(CurrentJSON);
       end;
      end;
     finally
      FreeAndNil(Stream);
     end;
    end;
   end;
  end;

  TpvJSONUtils.ResolveTemplates(JSONTechniques);

  BaseJSONItem:=JSONTechniques.Properties['techniques'];

  if assigned(BaseJSONItem) then begin

   TpvJSONUtils.ResolveInheritances(BaseJSONItem);

   if BaseJSONItem is TPasJSONItemObject then begin

    BaseJSONItemObject:=TPasJSONItemObject(BaseJSONItem);

    for JSONItemObjectProperty in BaseJSONItemObject do begin
     if (length(JSONItemObjectProperty.Key)>0) and
        assigned(JSONItemObjectProperty.Value) and
        (JSONItemObjectProperty.Value is TPasJSONItemObject) then begin
      Technique:=TTechnique.Create(self);
      try
       Technique.fName:=JSONItemObjectProperty.Key;
      finally
       fTechniques.Add(Technique);
      end;
      fTechniqueNameMap.Add(Technique.fName,Technique);
     end;
    end;

    for JSONItemObjectProperty in BaseJSONItemObject do begin
     if (length(JSONItemObjectProperty.Key)>0) and
        assigned(JSONItemObjectProperty.Value) and
        (JSONItemObjectProperty.Value is TPasJSONItemObject) then begin
      Technique:=fTechniqueNameMap[JSONItemObjectProperty.Key];
      if assigned(Technique) then begin
       Technique.LoadFromJSONObject(TPasJSONItemObject(JSONItemObjectProperty.Value));
      end;
     end;
    end;

   end;

  end;


 finally
  FreeAndNil(JSONTechniques);
 end;

end;

destructor TpvTechniques.Destroy;
begin
 FreeAndNil(fTechniqueNameMap);
 FreeAndNil(fTechniques);
 inherited Destroy;
end;

end.
