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
unit PasVulkan.Scene3D;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses {$ifdef Windows}
      Windows,
     {$endif}
     SysUtils,
     Classes,
     PasMP,
     PasJSON,
     PasGLTF,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.HighResolutionTimer,
     PasVulkan.Resources;

type EpvScene3D=class(Exception);

     IpvScene3D=interface(IpvResource)['{82D8BA65-626A-4A20-829E-51480B151116}']
     end;

     TpvScene3D=class(TpvResource,IpvScene3D)
      public
       type ITexture=interface(IpvResource)['{910CB49F-5700-49AD-8C48-49DF517E7850}']
            end;
            TTexture=class(TpvResource,ITexture)
             private
              fSceneInstance:TpvScene3D;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            TTextures=TpvObjectGenericList<TTexture>;
            IMaterial=interface(IpvResource)['{AC0AB88D-7E4A-42BF-B888-D198DD561895}']
            end;
            TMaterial=class(TpvResource,IMaterial)
             public
              type TAlphaMode=
                    (
                     Opaque=0,
                     Mask=1,
                     Blend=2
                    );
                   PAlphaMode=^TAlphaMode;
                   TAlphaModes=set of TAlphaMode;
                   TShadingModel=
                    (
                     PBRMetallicRoughness=0,
                     PBRSpecularGlossiness=1,
                     Unlit=2
                    );
                   PShadingModel=^TShadingModel;
                   TTextureReference=record
                    Texture:TpvScene3D.ITexture;
                    TexCoord:TpvSizeInt;
                   end;
                   PTextureReference=^TTextureReference;
                   TPBRMetallicRoughness=record
                    BaseColorFactor:TpvVector4;
                    BaseColorTexture:TTextureReference;
                    RoughnessFactor:TpvFloat;
                    MetallicFactor:TpvFloat;
                    MetallicRoughnessTexture:TTextureReference;
                   end;
                   PPBRMetallicRoughness=^TPBRMetallicRoughness;
                   TPBRSpecularGlossiness=record
                    DiffuseFactor:TpvVector4;
                    DiffuseTexture:TTextureReference;
                    GlossinessFactor:TpvFloat;
                    SpecularFactor:TpvVector3;
                    SpecularGlossinessTexture:TTextureReference;
                   end;
                   PPBRSpecularGlossiness=^TPBRSpecularGlossiness;
                   TUnlit=record
                    Dummy:TpvInt32;
                   end;
                   PUnlit=^TUnlit;
                   TShaderData=packed record // 128 bytes
                    BaseColorFactor:TpvVector4;
                    SpecularFactor:TpvVector4; // actually TVector3, but for easier and more convenient alignment reasons a TVector4
                    EmissiveFactor:TpvVector4; // actually TVector3, but for easier and more convenient alignment reasons a TVector4
                    MetallicRoughnessNormalScaleOcclusionStrengthFactor:TpvVector4;
                    // uvec4 AlphaCutOffFlags begin
                     AlphaCutOff:TpvFloat; // for with uintBitsToFloat on GLSL code side
                     Flags:TpvUInt32;
                     Reversed0:TpvUInt32;
                     Reversed1:TpvUInt32;
                    // uvec4 uAlphaCutOffFlags end
                   end;
                   PShaderData=^TShaderData;
                   TData=record
                    Name:TpvUTF8String;
                    ShadingModel:TShadingModel;
                    AlphaCutOff:TpvFloat;
                    AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
                    DoubleSided:boolean;
                    NormalTexture:TTextureReference;
                    NormalTextureScale:TpvFloat;
                    OcclusionTexture:TTextureReference;
                    OcclusionTextureStrength:TpvFloat;
                    EmissiveFactor:TpvVector3;
                    EmissiveTexture:TTextureReference;
                    PBRMetallicRoughness:TPBRMetallicRoughness;
                    PBRSpecularGlossiness:TPBRSpecularGlossiness;
                    Unlit:TUnlit;
                   end;
              const DefaultData:TData=(
                     Name:'';
                     ShadingModel:TpvScene3D.TMaterial.TShadingModel.PBRMetallicRoughness;
                     AlphaCutOff:1.0;
                     AlphaMode:TpvScene3D.TMaterial.TAlphaMode.Opaque;
                     DoubleSided:false;
                     NormalTexture:(Texture:nil;TexCoord:0);
                     NormalTextureScale:1.0;
                     OcclusionTexture:(Texture:nil;TexCoord:0);
                     OcclusionTextureStrength:1.0;
                     EmissiveFactor:(x:1.0;y:1.0;z:1.0);
                     PBRMetallicRoughness:(
                      BaseColorFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                      BaseColorTexture:(Texture:nil;TexCoord:0);
                      RoughnessFactor:1.0;
                      MetallicFactor:1.0;
                      MetallicRoughnessTexture:(Texture:nil;TexCoord:0);
                     );
                     PBRSpecularGlossiness:(
                      DiffuseFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                      DiffuseTexture:(Texture:nil;TexCoord:0);
                      GlossinessFactor:1.0;
                      SpecularFactor:(x:1.0;y:1.0;z:1.0);
                      SpecularGlossinessTexture:(Texture:nil;TexCoord:0);
                     );
                     Unlit:(
                      Dummy:0;
                     );
                    );
             private
              fSceneInstance:TpvScene3D;
              fData:TData;
              fShaderData:TShaderData;
              fUniformBufferObjectIndex:TpvSizeInt;
              fUniformBufferObjectOffset:TpvSizeInt;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure AssignFromGLTF(const aSourceMaterial:TPasGLTF.TMaterial;const aTextureReindexMap:array of TpvSizeInt);
              procedure FillShaderData;
            end;
            TMaterials=TpvObjectGenericList<TMaterial>;
      private
       fTextureListLock:TPasMPSlimReaderWriterLock;
       fTextures:TTextures;
       fMaterialListLock:TPasMPSlimReaderWriterLock;
       fMaterials:TMaterials;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
       destructor Destroy; override;
     end;

implementation

uses PasVulkan.Application;

{ TpvScene3D.TTexture }

constructor TpvScene3D.TTexture.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fSceneInstance:=aParent as TpvScene3D;

 //fData:=DefaultData;

end;

destructor TpvScene3D.TTexture.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TTexture.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fTextureListLock.Acquire;
 try
  fSceneInstance.fTextures.Add(self);
 finally
  fSceneInstance.fTextureListLock.Release;
 end;
end;

procedure TpvScene3D.TTexture.BeforeDestruction;
begin
 fSceneInstance.fTextureListLock.Acquire;
 try
  fSceneInstance.fTextures.Remove(self);
 finally
  fSceneInstance.fTextureListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D.TMaterial }

constructor TpvScene3D.TMaterial.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fSceneInstance:=aParent as TpvScene3D;

 fData:=DefaultData;

end;

destructor TpvScene3D.TMaterial.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TMaterial.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fMaterialListLock.Acquire;
 try
  fSceneInstance.fMaterials.Add(self);
 finally
  fSceneInstance.fMaterialListLock.Release;
 end;
end;

procedure TpvScene3D.TMaterial.BeforeDestruction;
begin
 fSceneInstance.fMaterialListLock.Acquire;
 try
  fSceneInstance.fMaterials.Remove(self);
 finally
  fSceneInstance.fMaterialListLock.Release;
 end;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TMaterial.AssignFromGLTF(const aSourceMaterial:TPasGLTF.TMaterial;const aTextureReindexMap:array of TpvSizeInt);
var Index:TpvSizeInt;
    JSONItem:TPasJSONItem;
    JSONObject:TPasJSONItemObject;
begin

 begin
  fData.Name:=aSourceMaterial.Name;
  fData.AlphaCutOff:=aSourceMaterial.AlphaCutOff;
  case aSourceMaterial.AlphaMode of
   TPasGLTF.TMaterial.TAlphaMode.Opaque:begin
    fData.AlphaMode:=TpvScene3D.TMaterial.TAlphaMode.Opaque;
   end;
   TPasGLTF.TMaterial.TAlphaMode.Mask:begin
    fData.AlphaMode:=TpvScene3D.TMaterial.TAlphaMode.Mask;
   end;
   TPasGLTF.TMaterial.TAlphaMode.Blend:begin
    fData.AlphaMode:=TpvScene3D.TMaterial.TAlphaMode.Blend;
   end;
  end;
  fData.DoubleSided:=aSourceMaterial.DoubleSided;
  fData.EmissiveFactor:=TpvVector3.InlineableCreate(aSourceMaterial.EmissiveFactor[0],aSourceMaterial.EmissiveFactor[1],aSourceMaterial.EmissiveFactor[2]);
  if (aSourceMaterial.EmissiveTexture.Index>=0) and (aSourceMaterial.EmissiveTexture.Index<length(aTextureReindexMap)) then begin
   fData.EmissiveTexture.Texture:=fSceneInstance.fTextures[aTextureReindexMap[aSourceMaterial.EmissiveTexture.Index]].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.EmissiveTexture.Texture:=nil;
  end;
  fData.EmissiveTexture.TexCoord:=aSourceMaterial.EmissiveTexture.TexCoord;
  if (aSourceMaterial.NormalTexture.Index>=0) and (aSourceMaterial.NormalTexture.Index<length(aTextureReindexMap)) then begin
   fData.NormalTexture.Texture:=fSceneInstance.fTextures[aTextureReindexMap[aSourceMaterial.NormalTexture.Index]].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.NormalTexture.Texture:=nil;
  end;
  fData.NormalTexture.TexCoord:=aSourceMaterial.NormalTexture.TexCoord;
  fData.NormalTextureScale:=aSourceMaterial.NormalTexture.Scale;
  if (aSourceMaterial.OcclusionTexture.Index>=0) and (aSourceMaterial.OcclusionTexture.Index<length(aTextureReindexMap)) then begin
   fData.OcclusionTexture.Texture:=fSceneInstance.fTextures[aTextureReindexMap[aSourceMaterial.OcclusionTexture.Index]].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.OcclusionTexture.Texture:=nil;
  end;
  fData.OcclusionTexture.TexCoord:=aSourceMaterial.OcclusionTexture.TexCoord;
  fData.OcclusionTextureStrength:=aSourceMaterial.OcclusionTexture.Strength;
 end;

 begin
  fData.PBRMetallicRoughness.BaseColorFactor:=TpvVector4.InlineableCreate(aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[0],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[1],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[2],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[3]);
  if (aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index>=0) and (aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index<length(aTextureReindexMap)) then begin
   fData.PBRMetallicRoughness.BaseColorTexture.Texture:=fSceneInstance.fTextures[aTextureReindexMap[aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index]].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.PBRMetallicRoughness.BaseColorTexture.Texture:=nil;
  end;
  fData.PBRMetallicRoughness.BaseColorTexture.TexCoord:=aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.TexCoord;
  fData.PBRMetallicRoughness.RoughnessFactor:=aSourceMaterial.PBRMetallicRoughness.RoughnessFactor;
  fData.PBRMetallicRoughness.MetallicFactor:=aSourceMaterial.PBRMetallicRoughness.MetallicFactor;
  if (aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index>=0) and (aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index<length(aTextureReindexMap)) then begin
   fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture:=fSceneInstance.fTextures[aTextureReindexMap[aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index]].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture:=nil;
  end;
  fData.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord:=aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord;
 end;

 JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_unlit'];
 if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
  fData.ShadingModel:=TMaterial.TShadingModel.Unlit;
 end else begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_pbrSpecularGlossiness'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fData.ShadingModel:=TMaterial.TShadingModel.PBRSpecularGlossiness;
   fData.PBRSpecularGlossiness.DiffuseFactor:=TpvVector4.InlineableCreate(TPasGLTF.TDefaults.IdentityVector4[0],TPasGLTF.TDefaults.IdentityVector4[1],TPasGLTF.TDefaults.IdentityVector4[2],TPasGLTF.TDefaults.IdentityVector4[3]);
   fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=nil;
   fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord:=0;
   fData.PBRSpecularGlossiness.GlossinessFactor:=TPasGLTF.TDefaults.IdentityScalar;
   fData.PBRSpecularGlossiness.SpecularFactor:=TpvVector3.InlineableCreate(TPasGLTF.TDefaults.IdentityVector3[0],TPasGLTF.TDefaults.IdentityVector3[1],TPasGLTF.TDefaults.IdentityVector3[2]);
   fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=nil;
   fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord:=0;
   begin
    JSONItem:=JSONObject.Properties['diffuseFactor'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=4) then begin
     fData.PBRSpecularGlossiness.DiffuseFactor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],fData.PBRSpecularGlossiness.DiffuseFactor[0]);
     fData.PBRSpecularGlossiness.DiffuseFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],fData.PBRSpecularGlossiness.DiffuseFactor[1]);
     fData.PBRSpecularGlossiness.DiffuseFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],fData.PBRSpecularGlossiness.DiffuseFactor[2]);
     fData.PBRSpecularGlossiness.DiffuseFactor[3]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[3],fData.PBRSpecularGlossiness.DiffuseFactor[3]);
    end;
    JSONItem:=JSONObject.Properties['diffuseTexture'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
     Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
     if (Index>=0) and (Index<length(aTextureReindexMap)) then begin
      fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=fSceneInstance.fTextures[aTextureReindexMap[Index]].InstanceInterface as TpvScene3D.ITexture;
     end else begin
      fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=nil;
     end;
     fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord);
    end;
    fData.PBRSpecularGlossiness.GlossinessFactor:=TPasJSON.GetNumber(JSONObject.Properties['glossinessFactor'],fData.PBRSpecularGlossiness.GlossinessFactor);
    JSONItem:=JSONObject.Properties['specularFactor'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3) then begin
     fData.PBRSpecularGlossiness.SpecularFactor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],fData.PBRSpecularGlossiness.SpecularFactor[0]);
     fData.PBRSpecularGlossiness.SpecularFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],fData.PBRSpecularGlossiness.SpecularFactor[1]);
     fData.PBRSpecularGlossiness.SpecularFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],fData.PBRSpecularGlossiness.SpecularFactor[2]);
    end;
    JSONItem:=JSONObject.Properties['specularGlossinessTexture'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
     Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
     if (Index>=0) and (Index<length(aTextureReindexMap)) then begin
      fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=fSceneInstance.fTextures[aTextureReindexMap[Index]].InstanceInterface as TpvScene3D.ITexture;
     end else begin
      fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=nil;
     end;
     fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord);
    end;
   end;
  end else begin
   fData.ShadingModel:=TMaterial.TShadingModel.PBRMetallicRoughness;
  end;
 end;

 FillShaderData;

end;

procedure TpvScene3D.TMaterial.FillShaderData;
begin

 fShaderData.Flags:=0;
 case fData.AlphaMode of
  TpvScene3D.TMaterial.TAlphaMode.Opaque:begin
   fShaderData.AlphaCutOff:=0.0;
  end;
  TpvScene3D.TMaterial.TAlphaMode.Mask:begin
   fShaderData.AlphaCutOff:=fData.AlphaCutOff;
  end;
  TpvScene3D.TMaterial.TAlphaMode.Blend:begin
   fShaderData.AlphaCutOff:=0.0;
   fShaderData.Flags:=fShaderData.Flags or (1 shl 4);
  end;
  else begin
   Assert(false);
  end;
 end;
 if fData.DoubleSided then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 5);
 end;
 fShaderData.Reversed0:=$ffffffff;
 fShaderData.Reversed1:=$ffffffff;
 case fData.ShadingModel of
  TMaterial.TShadingModel.PBRMetallicRoughness:begin
   fShaderData.Flags:=fShaderData.Flags or ((0 and $f) shl 0);
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (0 shl 2))) or ((fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
   end;
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (1 shl 2))) or ((fData.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord and $f) shl (1 shl 2));
   end;
   fShaderData.BaseColorFactor:=TpvVector4.InlineableCreate(fData.PBRMetallicRoughness.BaseColorFactor[0],fData.PBRMetallicRoughness.BaseColorFactor[1],fData.PBRMetallicRoughness.BaseColorFactor[2],fData.PBRMetallicRoughness.BaseColorFactor[3]);
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[0]:=fData.PBRMetallicRoughness.MetallicFactor;
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[1]:=fData.PBRMetallicRoughness.RoughnessFactor;
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[2]:=fData.NormalTextureScale;
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[3]:=fData.OcclusionTextureStrength;
  end;
  TMaterial.TShadingModel.PBRSpecularGlossiness:begin
   fShaderData.Flags:=fShaderData.Flags or ((1 and $f) shl 0);
   if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
    fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (0 shl 2))) or ((fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord and $f) shl (0 shl 2));
   end;
   if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
    fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (1 shl 2))) or ((fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord and $f) shl (1 shl 2));
   end;
   fShaderData.BaseColorFactor:=fData.PBRSpecularGlossiness.DiffuseFactor;
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[0]:=1.0;
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[1]:=fData.PBRSpecularGlossiness.GlossinessFactor;
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[2]:=fData.NormalTextureScale;
   fShaderData.MetallicRoughnessNormalScaleOcclusionStrengthFactor[3]:=fData.OcclusionTextureStrength;
   fShaderData.SpecularFactor[0]:=fData.PBRSpecularGlossiness.SpecularFactor[0];
   fShaderData.SpecularFactor[1]:=fData.PBRSpecularGlossiness.SpecularFactor[1];
   fShaderData.SpecularFactor[2]:=fData.PBRSpecularGlossiness.SpecularFactor[2];
   fShaderData.SpecularFactor[3]:=0.0;
  end;
  TMaterial.TShadingModel.Unlit:begin
   fShaderData.Flags:=fShaderData.Flags or ((2 and $f) shl 0);
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (0 shl 2))) or ((fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
   end;
   fShaderData.BaseColorFactor:=TpvVector4.InlineableCreate(fData.PBRMetallicRoughness.BaseColorFactor[0],fData.PBRMetallicRoughness.BaseColorFactor[1],fData.PBRMetallicRoughness.BaseColorFactor[2],fData.PBRMetallicRoughness.BaseColorFactor[3]);
  end;
  else begin
   Assert(false);
  end;
 end;
 if assigned(fData.NormalTexture.Texture) then begin
  fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (2 shl 2))) or ((fData.NormalTexture.TexCoord and $f) shl (2 shl 2));
 end;
 if assigned(fData.OcclusionTexture.Texture) then begin
  fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (3 shl 2))) or ((fData.OcclusionTexture.TexCoord and $f) shl (3 shl 2));
 end;
 if assigned(fData.EmissiveTexture.Texture) then begin
  fShaderData.Reversed0:=(fShaderData.Reversed0 and not ($f shl (4 shl 2))) or ((fData.EmissiveTexture.TexCoord and $f) shl (4 shl 2));
 end;
 fShaderData.EmissiveFactor[0]:=fData.EmissiveFactor[0];
 fShaderData.EmissiveFactor[1]:=fData.EmissiveFactor[1];
 fShaderData.EmissiveFactor[2]:=fData.EmissiveFactor[2];
 fShaderData.EmissiveFactor[3]:=0.0;

end;

{ TpvScene3D }

constructor TpvScene3D.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin

 inherited Create(aResourceManager,aParent);

 fTextureListLock:=TPasMPSlimReaderWriterLock.Create;

 fTextures:=TTextures.Create;
 fTextures.OwnsObjects:=false;

 fMaterialListLock:=TPasMPSlimReaderWriterLock.Create;

 fMaterials:=TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 ReleaseFrameDelay:=MaxSwapChainImages+1;

end;

destructor TpvScene3D.Destroy;
begin

 while fMaterials.Count>0 do begin
  fMaterials[fMaterials.Count-1].Free;
 end;
 FreeAndNil(fMaterials);

 FreeAndNil(fMaterialListLock);

 while fTextures.Count>0 do begin
  fTextures[fTextures.Count-1].Free;
 end;
 FreeAndNil(fTextures);

 FreeAndNil(fTextureListLock);

 inherited Destroy;
end;

initialization
end.

