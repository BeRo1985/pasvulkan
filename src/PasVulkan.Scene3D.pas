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
     Math,
     Vulkan,
     PasMP,
     PasJSON,
     PasGLTF,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.HighResolutionTimer,
     PasVulkan.Resources,
     PasVulkan.Techniques;

type EpvScene3D=class(Exception);

     IpvScene3D=interface(IpvResource)['{82D8BA65-626A-4A20-829E-51480B151116}']
     end;

     TpvScene3D=class(TpvResource,IpvScene3D)
      public
       type TVertexAttributeBindingLocations=class
             public
              const Position=0;
                    VertexIndex=1;
                    TangentSpace=2;
                    TexCoord0=3;
                    TexCoord1=4;
                    Color0=5;
                    Joints=6;
                    Weights=7;
            end;
            TUInt32Vector4=array[0..3] of TpvUInt32;
            TUInt16Vector4=array[0..3] of TpvUInt16;
            TInt16Vector4=array[0..3] of TpvInt16;
            PUInt32Vector4=^TUInt32Vector4;
            TMatrix4x4DynamicArray=TpvDynamicArray<TpvMatrix4x4>;
            TSizeIntDynamicArray=TpvDynamicArray<TpvSizeInt>;
            TVertex=packed record                 // Minimum required vertex structure for to be GLTF 2.0 conformant
             case boolean of
              false:(
               Position:TpvVector3;               //  12    0 (32-bit float 3D vector)
               VertexIndex:TpvUInt32;             // + 4 = 16 (unsigned 32-bit vertex index)
               Normal:TpvHalfFloatVector3;        // + 6 = 22 (16-bit float 3D vector)
               Tangent:TpvHalfFloatVector4;       // + 8 = 30 (16-bit float 4D vector)
               TexCoord0:TpvVector2;              // + 8 = 38 (must be full 32-bit float)
               TexCoord1:TpvVector2;              // + 8 = 46 (must be full 32-bit float)
               Color0:TpvHalfFloatVector4;        // + 8 = 54 (must be at least half-float for HDR)
               Joints:TUInt16Vector4;             // + 8 = 62 (node-wise indirect indices for a 16-bit index to 32-bit index lookup table)
               Weights:TpvHalfFloatVector4;       // + 8 = 70 (half-float should be enough for it)
              );                                  //  ==   ==
              true:(                              //  70   70 per vertex
               Padding:array[0..63] of TpvUInt8;
              );
            end;
            PVertex=^TVertex;
            TVertices=array of TVertex;
            IBaseObject=interface(IpvResource)['{9B6429EC-861D-4266-A7CB-408724C6AD27}']
            end;
            TBaseObject=class(TpvResource,IBaseObject)
             private
              fSceneInstance:TpvScene3D;
              fName:TpvUTF8String;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
             public
              property SceneInstance:TpvScene3D read fSceneInstance;
             published
              property Name:TpvUTF8String read fName write fName;
            end;
            TIBaseObjects=TpvGenericList<IBaseObject>;
            TBaseObjects=TpvObjectGenericList<TBaseObject>;
            IGroup=interface(IBaseObject)['{66A53CBE-F7A9-433A-B995-BC1D3882D03B}']
            end;
            TGroup=class;
            IBaseGroupObject=interface(IBaseObject)['{90FE8CF3-849E-476B-A504-A015B6840DEB}']
            end;
            TBaseGroupObject=class(TBaseObject,IBaseGroupObject)
             private
              fGroup:TGroup;
            end;
            IImage=interface(IBaseObject)['{B9D41155-5F92-49E8-9D1C-BFBEA2607149}']
            end;
            TImage=class(TBaseObject,IImage)
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            TIImage=TpvGenericList<IImage>;
            TImages=TpvObjectGenericList<TImage>;
            ISampler=interface(IBaseObject)['{BD753AB1-76A3-43F4-ADD9-4EF41DD280B4}']
            end;
            TSampler=class(TBaseObject,ISampler)
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            TISampler=TpvGenericList<ISampler>;
            TSamplers=TpvObjectGenericList<TSampler>;
            ITexture=interface(IBaseObject)['{910CB49F-5700-49AD-8C48-49DF517E7850}']
            end;
            TTexture=class(TBaseObject,ITexture)
             private
              fImage:IImage;
              fSampler:ISampler;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              property Image:IImage read fImage write fImage;
              property Sampler:ISampler read fSampler write fSampler;
            end;
            TITexture=TpvGenericList<ITexture>;
            TTextures=TpvObjectGenericList<TTexture>;
            IMaterial=interface(IBaseObject)['{AC0AB88D-7E4A-42BF-B888-D198DD561895}']
            end;
            TMaterial=class(TBaseObject,IMaterial)
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
                     TextureCoords:TpvUInt32;
                     Reversed:TpvUInt32;
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
            TIMaterials=TpvGenericList<IMaterial>;
            TMaterials=TpvObjectGenericList<TMaterial>;
            TGroup=class(TBaseObject,IGroup) // A group is a GLTF scene in a uber-scene
             public
              type TGroupObject=class
                    private
                     fName:TpvUTF8String;
                     fGroup:TGroup;
                    public
                     constructor Create(const aGroup:TGroup); reintroduce; virtual;
                     destructor Destroy; override;
                    published
                     property Group:TGroup read fGroup write fGroup;
                   end;
                   TAnimation=class(TGroupObject)
                    public
                     type TChannel=record
                                public
                                 type TTarget=
                                       (
                                        Translation,
                                        Rotation,
                                        Scale,
                                        Weights
                                       );
                                      TInterpolation=
                                       (
                                        Linear,
                                        Step,
                                        CubicSpline
                                       );
                                public
                                 Name:TpvUTF8String;
                                 Node:TpvSizeInt;
                                 Target:TTarget;
                                 Interpolation:TInterpolation;
                                 InputTimeArray:TpvFloatDynamicArray;
                                 OutputScalarArray:TpvFloatDynamicArray;
                                 OutputVector3Array:TpvVector3Array;
                                 OutputVector4Array:TpvVector4Array;
                                 Last:TPasGLTFSizeInt;
                               end;
                               PChannel=^TChannel;
                               TChannels=array of TChannel;
                    private
                     fChannels:TChannels;
                    public
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceAnimation:TPasGLTF.TAnimation);
                   end;
                   TAnimations=TpvObjectGenericList<TAnimation>;
                   TMesh=class(TGroupObject)
                    public
                     type TPrimitive=record
                           public
                            type TTarget=record
                                  public
                                   type TTargetVertex=packed record // 24 byte per target vertex
                                         Position:TpvVector3;
                                         Normal:TpvHalfFloatVector3;
                                         Tangent:TpvHalfFloatVector3;
                                        end;
                                        PTargetVertex=^TTargetVertex;
                                        TTargetVertices=array of TTargetVertex;
                                  public
                                   Vertices:TTargetVertices;
                                 end;
                                 PTarget=^TTarget;
                                 TTargets=array of TTarget;
                           public
                            PrimitiveMode:TVkPrimitiveTopology;
                            Material:TpvSizeInt;
                            Vertices:TVertices;
                            Indices:TpvUInt32DynamicArray;
                            Targets:TTargets;
                            StartBufferVertexOffset:TpvSizeUInt;
                            StartBufferIndexOffset:TpvSizeUInt;
                            CountVertices:TpvSizeUInt;
                            CountIndices:TpvSizeUInt;
                            MorphTargetVertexShaderStorageBufferObjectIndex:TpvSizeInt;
                            MorphTargetVertexShaderStorageBufferObjectOffset:TpvSizeUInt;
                            MorphTargetVertexShaderStorageBufferObjectByteOffset:TpvSizeUInt;
                            MorphTargetVertexShaderStorageBufferObjectByteSize:TpvSizeUInt;
                          end;
                          PPrimitive=^TPrimitive;
                          TPrimitives=array of TPrimitive;
                    private
                     fPrimitives:TPrimitives;
                     fBoundingBox:TpvAABB;
                     fWeights:TpvFloatDynamicArray;
                    public
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMesh:TPasGLTF.TMesh);
                   end;
                   TMeshes=TpvObjectGenericList<TMesh>;
                   TSkin=class(TGroupObject)
                    private
                     fSkeleton:TpvSizeInt;
                     fInverseBindMatrices:TMatrix4x4DynamicArray;
                     fMatrices:TMatrix4x4DynamicArray;
                     fJoints:TSizeIntDynamicArray;
                     fSkinShaderStorageBufferObjectIndex:TpvSizeInt;
                     fSkinShaderStorageBufferObjectOffset:TpvSizeUInt;
                     fSkinShaderStorageBufferObjectByteOffset:TpvSizeUInt;
                     fSkinShaderStorageBufferObjectByteSize:TpvSizeUInt;
                   public
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSkin:TPasGLTF.TSkin);
                   end;
                   TSkins=TpvObjectGenericList<TSkin>;
                   TNode=class;
                   TNodes=TpvObjectGenericList<TNode>;
                   TNode=class(TGroupObject)
                    private
                     fChildren:TNodes;
                     fMesh:TMesh;
                     fSkin:TSkin;
                    public
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     property Children:TNodes read fChildren;
                     property Mesh:TMesh read fMesh write fMesh;
                     property Skin:TSkin read fSkin write fSkin;
                   end;
             private
              fObjects:TIBaseObjects;
              fAnimations:TAnimations;
              fMeshes:TMeshes;
              fSkins:TSkins;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
             published
              property Objects:TIBaseObjects read fObjects;
              property Animations:TAnimations read fAnimations;
              property Meshes:TMeshes read fMeshes;
              property Skins:TSkins read fSkins;
            end;
            TIGroups=TpvGenericList<IGroup>;
            TGroups=TpvObjectGenericList<TGroup>;
            IGroupInstance=interface(IBaseObject)['{B4360C7F-7C60-4676-B301-A68D67FB401F}']
            end;
            TGroupInstance=class(TBaseObject,IGroupInstance)
             private
              fGroup:IGroup;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              property Group:IGroup read fGroup write fGroup;
            end;
            TIGroupInstances=TpvGenericList<IGroupInstance>;
            TGroupInstances=TpvObjectGenericList<TGroupInstance>;
      private
       fTechniques:TpvTechniques;
       fImageListLock:TPasMPSlimReaderWriterLock;
       fImages:TImages;
       fSamplerListLock:TPasMPSlimReaderWriterLock;
       fSamplers:TSamplers;
       fTextureListLock:TPasMPSlimReaderWriterLock;
       fTextures:TTextures;
       fMaterialListLock:TPasMPSlimReaderWriterLock;
       fMaterials:TMaterials;
       fGroupListLock:TPasMPSlimReaderWriterLock;
       fGroups:TGroups;
       fGroupInstanceListLock:TPasMPSlimReaderWriterLock;
       fGroupInstances:TGroupInstances;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
       destructor Destroy; override;
     end;

implementation

uses PasVulkan.Application;

{ TpvScene3D.TBaseObject }

constructor TpvScene3D.TBaseObject.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fSceneInstance:=aParent as TpvScene3D;

end;

destructor TpvScene3D.TBaseObject.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TBaseObject.AfterConstruction;
begin
 inherited AfterConstruction;
end;

procedure TpvScene3D.TBaseObject.BeforeDestruction;
begin
 inherited BeforeDestruction;
end;

{ TpvScene3D.TImage }

constructor TpvScene3D.TImage.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
end;

destructor TpvScene3D.TImage.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TImage.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fImageListLock.Acquire;
 try
  fSceneInstance.fImages.Add(self);
 finally
  fSceneInstance.fImageListLock.Release;
 end;
end;

procedure TpvScene3D.TImage.BeforeDestruction;
begin
 fSceneInstance.fImageListLock.Acquire;
 try
  fSceneInstance.fImages.Remove(self);
 finally
  fSceneInstance.fImageListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D.TSampler }

constructor TpvScene3D.TSampler.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
end;

destructor TpvScene3D.TSampler.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TSampler.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fSamplerListLock.Acquire;
 try
  fSceneInstance.fSamplers.Add(self);
 finally
  fSceneInstance.fSamplerListLock.Release;
 end;
end;

procedure TpvScene3D.TSampler.BeforeDestruction;
begin
 fSceneInstance.fSamplerListLock.Acquire;
 try
  fSceneInstance.fSamplers.Remove(self);
 finally
  fSceneInstance.fSamplerListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D.TTexture }

constructor TpvScene3D.TTexture.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fImage:=nil;

 fSampler:=nil;

end;

destructor TpvScene3D.TTexture.Destroy;
begin

 fImage:=nil;

 fSampler:=nil;

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
 fImage:=nil;
 fSampler:=nil;
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
   fShaderData.Flags:=fShaderData.Flags or (1 shl 4);
  end;
  TpvScene3D.TMaterial.TAlphaMode.Blend:begin
   fShaderData.AlphaCutOff:=0.0;
   fShaderData.Flags:=fShaderData.Flags or (1 shl 5);
  end;
  else begin
   Assert(false);
  end;
 end;
 if fData.DoubleSided then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 6);
 end;
 fShaderData.TextureCoords:=$ffffffff;
 fShaderData.Reversed:=$ffffffff;
 case fData.ShadingModel of
  TMaterial.TShadingModel.PBRMetallicRoughness:begin
   fShaderData.Flags:=fShaderData.Flags or ((0 and $f) shl 0);
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (0 shl 2))) or ((fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
   end;
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (1 shl 2))) or ((fData.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord and $f) shl (1 shl 2));
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
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (0 shl 2))) or ((fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord and $f) shl (0 shl 2));
   end;
   if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (1 shl 2))) or ((fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord and $f) shl (1 shl 2));
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
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (0 shl 2))) or ((fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
   end;
   fShaderData.BaseColorFactor:=TpvVector4.InlineableCreate(fData.PBRMetallicRoughness.BaseColorFactor[0],fData.PBRMetallicRoughness.BaseColorFactor[1],fData.PBRMetallicRoughness.BaseColorFactor[2],fData.PBRMetallicRoughness.BaseColorFactor[3]);
  end;
  else begin
   Assert(false);
  end;
 end;
 if assigned(fData.NormalTexture.Texture) then begin
  fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (2 shl 2))) or ((fData.NormalTexture.TexCoord and $f) shl (2 shl 2));
 end;
 if assigned(fData.OcclusionTexture.Texture) then begin
  fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (3 shl 2))) or ((fData.OcclusionTexture.TexCoord and $f) shl (3 shl 2));
 end;
 if assigned(fData.EmissiveTexture.Texture) then begin
  fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (4 shl 2))) or ((fData.EmissiveTexture.TexCoord and $f) shl (4 shl 2));
 end;
 fShaderData.EmissiveFactor[0]:=fData.EmissiveFactor[0];
 fShaderData.EmissiveFactor[1]:=fData.EmissiveFactor[1];
 fShaderData.EmissiveFactor[2]:=fData.EmissiveFactor[2];
 fShaderData.EmissiveFactor[3]:=0.0;

end;

{ TpvScene3D.TGroup.TGroupObject }

constructor TpvScene3D.TGroup.TGroupObject.Create(const aGroup:TGroup);
begin
 inherited Create;
 fGroup:=aGroup;
end;

destructor TpvScene3D.TGroup.TGroupObject.Destroy;
begin
 inherited Destroy;
end;

{ TpvScene3D.TGroup.TAnimation }

constructor TpvScene3D.TGroup.TAnimation.Create(const aGroup:TGroup);
begin
 inherited Create(aGroup);
end;

destructor TpvScene3D.TGroup.TAnimation.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TAnimation.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceAnimation:TPasGLTF.TAnimation);
var Index,ChannelIndex,ValueIndex:TPasGLTFSizeInt;
    SourceAnimation:TPasGLTF.TAnimation;
    DestinationAnimation:TAnimation;
    SourceAnimationChannel:TPasGLTF.TAnimation.TChannel;
    SourceAnimationSampler:TPasGLTF.TAnimation.TSampler;
    DestinationAnimationChannel:TAnimation.PChannel;
    OutputVector3Array:TPasGLTF.TVector3DynamicArray;
    OutputVector4Array:TPasGLTF.TVector4DynamicArray;
    OutputScalarArray:TPasGLTFFloatDynamicArray;
begin

 SourceAnimation:=aSourceAnimation;

 DestinationAnimation:=self;

 DestinationAnimation.fName:=SourceAnimation.Name;

 SetLength(DestinationAnimation.fChannels,SourceAnimation.Channels.Count);

 for ChannelIndex:=0 to SourceAnimation.Channels.Count-1 do begin

  SourceAnimationChannel:=SourceAnimation.Channels[ChannelIndex];

  DestinationAnimationChannel:=@DestinationAnimation.fChannels[ChannelIndex];

  DestinationAnimationChannel^.Last:=-1;

  DestinationAnimationChannel^.Node:=SourceAnimationChannel.Target.Node;

  if SourceAnimationChannel.Target.Path='translation' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Translation;
  end else if SourceAnimationChannel.Target.Path='rotation' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Rotation;
  end else if SourceAnimationChannel.Target.Path='scale' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Scale;
  end else if SourceAnimationChannel.Target.Path='weights' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Weights;
  end else begin
   raise EPasGLTF.Create('Non-supported animation channel target path "'+String(SourceAnimationChannel.Target.Path)+'"');
  end;

  if (SourceAnimationChannel.Sampler>=0) and (SourceAnimationChannel.Sampler<SourceAnimation.Samplers.Count) then begin
   SourceAnimationSampler:=SourceAnimation.Samplers[SourceAnimationChannel.Sampler];
   case SourceAnimationSampler.Interpolation of
    TPasGLTF.TAnimation.TSampler.TType.Linear:begin
     DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.Linear;
    end;
    TPasGLTF.TAnimation.TSampler.TType.Step:begin
     DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.Step;
    end;
    TPasGLTF.TAnimation.TSampler.TType.CubicSpline:begin
     DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.CubicSpline;
    end;
    else begin
     raise EPasGLTF.Create('Non-supported animation sampler interpolation method type');
    end;
   end;
   begin
    OutputScalarArray:=aSourceDocument.Accessors[SourceAnimationSampler.Input].DecodeAsFloatArray(false);
    try
     SetLength(DestinationAnimationChannel^.InputTimeArray,length(OutputScalarArray));
     if length(OutputScalarArray)>0 then begin
      Move(OutputScalarArray[0],DestinationAnimationChannel^.InputTimeArray[0],length(OutputScalarArray)*SizeOf(TpvFloat));
     end;
    finally
     OutputScalarArray:=nil;
    end;
   end;
   case DestinationAnimationChannel^.Target of
    TAnimation.TChannel.TTarget.Translation,
    TAnimation.TChannel.TTarget.Scale:begin
     OutputVector3Array:=aSourceDocument.Accessors[SourceAnimationSampler.Output].DecodeAsVector3Array(false);
     try
      SetLength(DestinationAnimationChannel^.OutputVector3Array,length(OutputVector3Array));
      if length(OutputVector3Array)>0 then begin
       Move(OutputVector3Array[0],DestinationAnimationChannel^.OutputVector3Array[0],length(OutputVector3Array)*SizeOf(TpvVector3));
      end;
     finally
      OutputVector3Array:=nil;
     end;
    end;
    TAnimation.TChannel.TTarget.Rotation:begin
     OutputVector4Array:=aSourceDocument.Accessors[SourceAnimationSampler.Output].DecodeAsVector4Array(false);
     try
      for ValueIndex:=0 to length(DestinationAnimationChannel^.OutputVector4Array)-1 do begin
       TpvVector4(pointer(@OutputVector4Array[ValueIndex])^):=TpvVector4(pointer(@DestinationAnimationChannel^.OutputVector4Array[ValueIndex])^).Normalize;
      end;
      SetLength(DestinationAnimationChannel^.OutputVector4Array,length(OutputVector4Array));
      if length(OutputVector4Array)>0 then begin
       Move(OutputVector4Array[0],DestinationAnimationChannel^.OutputVector4Array[0],length(OutputVector4Array)*SizeOf(TpvVector4));
      end;
     finally
      OutputVector4Array:=nil;
     end;
    end;
    TAnimation.TChannel.TTarget.Weights:begin
     OutputScalarArray:=aSourceDocument.Accessors[SourceAnimationSampler.Output].DecodeAsFloatArray(false);
     try
      SetLength(DestinationAnimationChannel^.OutputScalarArray,length(OutputScalarArray));
      if length(OutputScalarArray)>0 then begin
       Move(OutputScalarArray[0],DestinationAnimationChannel^.OutputScalarArray[0],length(OutputScalarArray)*SizeOf(TpvFloat));
      end;
     finally
      OutputScalarArray:=nil;
     end;
    end;
   end;
  end else begin
   raise EPasGLTF.Create('Non-existent sampler');
  end;

 end;

end;

{ TpvScene3D.TGroup.TMesh }

constructor TpvScene3D.TGroup.TMesh.Create(const aGroup:TGroup);
begin
 inherited Create(aGroup);
end;

destructor TpvScene3D.TGroup.TMesh.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TMesh.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMesh:TPasGLTF.TMesh);
var Index,
    PrimitiveIndex,
    AccessorIndex,
    IndexIndex,
    VertexIndex,
    TargetIndex,
    WeightIndex,
    JointIndex,
    OtherJointIndex,
    OldCount,
    MaxCountTargets:TPasGLTFSizeInt;
    SourceMesh:TPasGLTF.TMesh;
    SourceMeshPrimitive:TPasGLTF.TMesh.TPrimitive;
    SourceMeshPrimitiveTarget:TPasGLTF.TAttributes;
    DestinationMesh:TMesh;
    DestinationMeshPrimitive:TMesh.PPrimitive;
    DestinationMeshPrimitiveTarget:TMesh.TPrimitive.PTarget;
    DestinationMeshPrimitiveTargetVertex:TMesh.TPrimitive.TTarget.PTargetVertex;
    TemporaryPositions,
    TemporaryNormals,
    TemporaryBitangents,
    TemporaryTargetTangents:TPasGLTF.TVector3DynamicArray;
    TemporaryTangents,
    TemporaryColor0,
    TemporaryWeights0:TPasGLTF.TVector4DynamicArray;
    TemporaryJoints0:TPasGLTF.TUInt32Vector4DynamicArray;
    TemporaryTexCoord0,
    TemporaryTexCoord1:TPasGLTF.TVector2DynamicArray;
    TemporaryIndices,
    TemporaryTriangleIndices:TPasGLTFUInt32DynamicArray;
    Normal,Tangent,Bitangent,p1p0,p2p0:TpvVector3;
    p0,p1,p2:PpvVector3;
    t1t0,t2t0:TpvVector2;
    t0,t1,t2:PpvVector2;
    TangentSpaceMatrix:TpvMatrix3x3;
    TangentSpaceQuaternion:TpvQuaternion;
    Vertex:PVertex;
    Area:TPasGLTFFloat;
    DoNeedCalculateTangents:boolean;
begin

 SourceMesh:=aSourceMesh;

 DestinationMesh:=self;

 DestinationMesh.fName:=SourceMesh.Name;

 SetLength(DestinationMesh.fPrimitives,SourceMesh.Primitives.Count);

 DestinationMesh.fBoundingBox:=TpvAABB.Create(TpvVector3.InlineableCreate(Infinity,Infinity,Infinity),
                                              TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity));

//DestinationMesh^.JointWeights:=nil;

 MaxCountTargets:=0;

 for PrimitiveIndex:=0 to SourceMesh.Primitives.Count-1 do begin

  SourceMeshPrimitive:=SourceMesh.Primitives.Items[PrimitiveIndex];

  DestinationMeshPrimitive:=@DestinationMesh.fPrimitives[PrimitiveIndex];

  DestinationMeshPrimitive^.Material:=SourceMeshPrimitive.Material;

  begin
   // Load accessor data
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['POSITION'];
    if AccessorIndex>=0 then begin
     TemporaryPositions:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
     for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
      DestinationMesh.fBoundingBox.Min[0]:=Min(DestinationMesh.fBoundingBox.Min[0],TemporaryPositions[VertexIndex,0]);
      DestinationMesh.fBoundingBox.Min[1]:=Min(DestinationMesh.fBoundingBox.Min[1],TemporaryPositions[VertexIndex,1]);
      DestinationMesh.fBoundingBox.Min[2]:=Min(DestinationMesh.fBoundingBox.Min[2],TemporaryPositions[VertexIndex,2]);
      DestinationMesh.fBoundingBox.Max[0]:=Max(DestinationMesh.fBoundingBox.Max[0],TemporaryPositions[VertexIndex,0]);
      DestinationMesh.fBoundingBox.Max[1]:=Max(DestinationMesh.fBoundingBox.Max[1],TemporaryPositions[VertexIndex,1]);
      DestinationMesh.fBoundingBox.Max[2]:=Max(DestinationMesh.fBoundingBox.Max[2],TemporaryPositions[VertexIndex,2]);
     end;
    end else begin
     raise EPasGLTF.Create('Missing position data');
    end;
   end;
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['NORMAL'];
    if AccessorIndex>=0 then begin
     TemporaryNormals:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
    end else begin
     TemporaryNormals:=nil;
    end;
   end;
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['TANGENT'];
    if AccessorIndex>=0 then begin
     TemporaryTangents:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
    end else begin
     TemporaryTangents:=nil;
    end;
   end;
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['TEXCOORD_0'];
    if AccessorIndex>=0 then begin
     TemporaryTexCoord0:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector2Array(true);
    end else begin
     TemporaryTexCoord0:=nil;
    end;
   end;
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['TEXCOORD_1'];
    if AccessorIndex>=0 then begin
     TemporaryTexCoord1:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector2Array(true);
    end else begin
     TemporaryTexCoord1:=nil;
    end;
   end;
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['COLOR_0'];
    if AccessorIndex>=0 then begin
     TemporaryColor0:=aSourceDocument.Accessors[AccessorIndex].DecodeAsColorArray(true);
    end else begin
     TemporaryColor0:=nil;
    end;
   end;
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['JOINTS_0'];
    if AccessorIndex>=0 then begin
     TemporaryJoints0:=aSourceDocument.Accessors[AccessorIndex].DecodeAsUInt32Vector4Array(true);
    end else begin
     TemporaryJoints0:=nil;
    end;
   end;
   begin
    AccessorIndex:=SourceMeshPrimitive.Attributes['WEIGHTS_0'];
    if AccessorIndex>=0 then begin
     TemporaryWeights0:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
    end else begin
     TemporaryWeights0:=nil;
    end;
   end;
  end;

  begin
   // load or generate vertex indices
   if SourceMeshPrimitive.Indices>=0 then begin
    TemporaryIndices:=aSourceDocument.Accessors[SourceMeshPrimitive.Indices].DecodeAsUInt32Array(false);
   end else begin
    SetLength(TemporaryIndices,length(TemporaryPositions));
    for IndexIndex:=0 to length(TemporaryIndices)-1 do begin
     TemporaryIndices[IndexIndex]:=IndexIndex;
    end;
   end;
   case SourceMeshPrimitive.Mode of
    TPasGLTF.TMesh.TPrimitive.TMode.Triangles:begin
     TemporaryTriangleIndices:=TemporaryIndices;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip:begin
     TemporaryTriangleIndices:=nil;
     SetLength(TemporaryTriangleIndices,(length(TemporaryIndices)-2)*3);
     for IndexIndex:=0 to length(TemporaryIndices)-3 do begin
      if (IndexIndex and 1)<>0 then begin
       TemporaryTriangleIndices[(IndexIndex*3)+0]:=TemporaryIndices[IndexIndex+0];
       TemporaryTriangleIndices[(IndexIndex*3)+1]:=TemporaryIndices[IndexIndex+1];
       TemporaryTriangleIndices[(IndexIndex*3)+2]:=TemporaryIndices[IndexIndex+2];
      end else begin
       TemporaryTriangleIndices[(IndexIndex*3)+0]:=TemporaryIndices[IndexIndex+0];
       TemporaryTriangleIndices[(IndexIndex*3)+1]:=TemporaryIndices[IndexIndex+2];
       TemporaryTriangleIndices[(IndexIndex*3)+2]:=TemporaryIndices[IndexIndex+1];
      end;
     end;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan:begin
     TemporaryTriangleIndices:=nil;
     SetLength(TemporaryTriangleIndices,(length(TemporaryIndices)-2)*3);
     for IndexIndex:=2 to length(TemporaryIndices)-1 do begin
      TemporaryTriangleIndices[((IndexIndex-1)*3)+0]:=TemporaryIndices[0];
      TemporaryTriangleIndices[((IndexIndex-1)*3)+1]:=TemporaryIndices[IndexIndex-1];
      TemporaryTriangleIndices[((IndexIndex-1)*3)+2]:=TemporaryIndices[IndexIndex];
     end;
    end;
    else begin
     TemporaryTriangleIndices:=nil;
    end;
   end;
  end;

  begin
   // Generate missing data
   if length(TemporaryNormals)<>length(TemporaryPositions) then begin
    SetLength(TemporaryNormals,length(TemporaryPositions));
    for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
     TemporaryNormals[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
    end;
    if length(TemporaryTriangleIndices)>0 then begin
     IndexIndex:=0;
     while (IndexIndex+2)<length(TemporaryTriangleIndices) do begin
      p0:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+0]];
      p1:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+1]];
      p2:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+2]];
      Normal:=(p1^-p0^).Cross(p2^-p0^); // non-normalized weighted normal
      PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+0]]))^:=PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+0]]))^+Normal;
      PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+1]]))^:=PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+1]]))^+Normal;
      PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+2]]))^:=PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+2]]))^+Normal;
      inc(IndexIndex,3);
     end;
     for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
      PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^:=PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^.Normalize;
     end;
    end;
   end;
   if length(TemporaryTexCoord0)<>length(TemporaryPositions) then begin
    SetLength(TemporaryTexCoord0,length(TemporaryPositions));
    for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
     PpvVector2(pointer(@TemporaryTexCoord0[VertexIndex]))^:=PpvVector2(pointer(@TPasGLTF.TDefaults.NullVector3))^;
    end;
   end;
   if length(TemporaryTangents)<>length(TemporaryPositions) then begin
    SetLength(TemporaryTangents,length(TemporaryPositions));
    SetLength(TemporaryBitangents,length(TemporaryPositions));
    for VertexIndex:=0 to length(TemporaryTangents)-1 do begin
     PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^:=PpvVector3(pointer(@TPasGLTF.TDefaults.NullVector3))^;
     TemporaryBitangents[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
    end;
    if length(TemporaryTriangleIndices)>0 then begin
     IndexIndex:=0;
     while (IndexIndex+2)<length(TemporaryTriangleIndices) do begin
      p0:=pointer(@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+0]]);
      p1:=pointer(@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+1]]);
      p2:=pointer(@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+2]]);
      t0:=pointer(@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+0]]);
      t1:=pointer(@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+1]]);
      t2:=pointer(@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+2]]);
      p1p0:=p1^-p0^;
      p2p0:=p2^-p0^;
      t1t0:=t1^-t0^;
      t2t0:=t2^-t0^;
      Normal:=(p1p0.Cross(p2p0)).Normalize;
      if PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+0]]))^.Dot(Normal)<0.0 then begin
       Normal:=-Normal;
      end;
{$if true}
      Area:=(t2t0[0]*t1t0[1])-(t1t0[0]*t2t0[1]);
      if IsZero(Area) then begin
       Tangent[0]:=0.0;
       Tangent[1]:=1.0;
       Tangent[2]:=0.0;
       Bitangent[0]:=1.0;
       Bitangent[1]:=0.0;
       Bitangent[2]:=0.0;
      end else begin
       Tangent[0]:=((t1t0[1]*p2p0[0])-(t2t0[1]*p1p0[0]))/Area;
       Tangent[1]:=((t1t0[1]*p2p0[1])-(t2t0[1]*p1p0[1]))/Area;
       Tangent[2]:=((t1t0[1]*p2p0[2])-(t2t0[1]*p1p0[2]))/Area;
       Bitangent[0]:=((t1t0[0]*p2p0[0])-(t2t0[0]*p1p0[0]))/Area;
       Bitangent[1]:=((t1t0[0]*p2p0[1])-(t2t0[0]*p1p0[1]))/Area;
       Bitangent[2]:=((t1t0[0]*p2p0[2])-(t2t0[0]*p1p0[2]))/Area;
      end;
      if (Tangent.Cross(Bitangent)).Dot(Normal)<0.0 then begin
       Tangent:=-Tangent;
       Bitangent:=-Bitangent;
      end;
{$else}
      Tangent[0]:=(t1t0[1]*p2p0[0])-(t2t0[1]*p1p0[0]);
      Tangent[1]:=(t1t0[1]*p2p0[1])-(t2t0[1]*p1p0[1]);
      Tangent[2]:=(t1t0[1]*p2p0[2])-(t2t0[1]*p1p0[2]);
      Bitangent[0]:=(t1t0[0]*p2p0[0])-(t2t0[0]*p1p0[0]);
      Bitangent[1]:=(t1t0[0]*p2p0[1])-(t2t0[0]*p1p0[1]);
      Bitangent[2]:=(t1t0[0]*p2p0[2])-(t2t0[0]*p1p0[2]);
      if (Tangent.Cross(Bitangent)).Dot(Normal)<0.0 then begin
       Tangent:=-Tangent;
       Bitangent:=-Bitangent;
      end;
{$ifend}
      PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+0]]))^:=PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+0]]))^+Tangent;
      PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+1]]))^:=PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+1]]))^+Tangent;
      PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+2]]))^:=PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+2]]))^+Tangent;
      PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+0]]))^:=PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+0]]))^+Bitangent;
      PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+1]]))^:=PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+1]]))^+Bitangent;
      PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+2]]))^:=PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+2]]))^+Bitangent;
      inc(IndexIndex,3);
     end;
     for VertexIndex:=0 to length(TemporaryTangents)-1 do begin
      Normal:=PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^;
      Tangent:=PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^.Normalize;
      Tangent:=(Tangent-(Normal*Tangent.Dot(Normal))).Normalize;
      Bitangent:=PpvVector3(pointer(@TemporaryBitangents[VertexIndex]))^.Normalize;
      Bitangent:=(Bitangent-(Normal*Bitangent.Dot(Normal))).Normalize;
      PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^:=Tangent;
      if (PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^.Cross(Tangent)).Dot(Bitangent)<0.0 then begin
       TemporaryTangents[VertexIndex,3]:=-1.0;
      end else begin
       TemporaryTangents[VertexIndex,3]:=1.0;
      end;
     end;
    end;
   end;
  end;

  begin
   // Primitive mode
   case SourceMeshPrimitive.Mode of
    TPasGLTF.TMesh.TPrimitive.TMode.Points:begin
     DestinationMeshPrimitive^.PrimitiveMode:=VK_PRIMITIVE_TOPOLOGY_POINT_LIST;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.Lines:begin
     DestinationMeshPrimitive^.PrimitiveMode:=VK_PRIMITIVE_TOPOLOGY_LINE_LIST;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.LineLoop:begin
     DestinationMeshPrimitive^.PrimitiveMode:=VK_PRIMITIVE_TOPOLOGY_LINE_LIST;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.LineStrip:begin
     DestinationMeshPrimitive^.PrimitiveMode:=VK_PRIMITIVE_TOPOLOGY_LINE_STRIP;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.Triangles:begin
     DestinationMeshPrimitive^.PrimitiveMode:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip:begin
     DestinationMeshPrimitive^.PrimitiveMode:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP;
    end;
    TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan:begin
     DestinationMeshPrimitive^.PrimitiveMode:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN;
    end;
    else begin
     raise EPasGLTF.Create('Invalid primitive mode');
    end;
   end;
  end;

  begin
   // Generate vertex array buffer
   SetLength(DestinationMeshPrimitive^.Vertices,length(TemporaryPositions));
   for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
    Vertex:=@DestinationMeshPrimitive^.Vertices[VertexIndex];
    FillChar(Vertex^,SizeOf(TVertex),#0);
    Vertex^.Position:=TpvVector3(pointer(@TemporaryPositions[VertexIndex])^);
    Vertex^.VertexIndex:=VertexIndex;
    if VertexIndex<length(TemporaryNormals) then begin
     Vertex^.Normal.x:=TpvVector3(pointer(@TemporaryNormals[VertexIndex])^).x;
     Vertex^.Normal.y:=TpvVector3(pointer(@TemporaryNormals[VertexIndex])^).y;
     Vertex^.Normal.z:=TpvVector3(pointer(@TemporaryNormals[VertexIndex])^).z;
    end;
    if VertexIndex<length(TemporaryTangents) then begin
     Vertex^.Tangent.x:=TpvVector4(pointer(@TemporaryTangents[VertexIndex])^).x;
     Vertex^.Tangent.y:=TpvVector4(pointer(@TemporaryTangents[VertexIndex])^).y;
     Vertex^.Tangent.z:=TpvVector4(pointer(@TemporaryTangents[VertexIndex])^).z;
     Vertex^.Tangent.w:=TpvVector4(pointer(@TemporaryTangents[VertexIndex])^).w;
    end;
    if VertexIndex<length(TemporaryTexCoord0) then begin
     Vertex^.TexCoord0:=TpvVector2(pointer(@TemporaryTexCoord0[VertexIndex])^);
    end;
    if VertexIndex<length(TemporaryTexCoord1) then begin
     Vertex^.TexCoord1:=TpvVector2(pointer(@TemporaryTexCoord1[VertexIndex])^);
    end;
    if VertexIndex<length(TemporaryColor0) then begin
     Vertex^.Color0.x:=TemporaryColor0[VertexIndex][0];
     Vertex^.Color0.y:=TemporaryColor0[VertexIndex][1];
     Vertex^.Color0.z:=TemporaryColor0[VertexIndex][2];
     Vertex^.Color0.w:=TemporaryColor0[VertexIndex][3];
    end else begin
     Vertex^.Color0.x:=TPasGLTF.TDefaults.IdentityVector4[0];
     Vertex^.Color0.y:=TPasGLTF.TDefaults.IdentityVector4[1];
     Vertex^.Color0.z:=TPasGLTF.TDefaults.IdentityVector4[2];
     Vertex^.Color0.w:=TPasGLTF.TDefaults.IdentityVector4[3];
    end;
    if VertexIndex<length(TemporaryJoints0) then begin
     Vertex^.Joints[0]:=TemporaryJoints0[VertexIndex][0];
     Vertex^.Joints[1]:=TemporaryJoints0[VertexIndex][1];
     Vertex^.Joints[2]:=TemporaryJoints0[VertexIndex][2];
     Vertex^.Joints[3]:=TemporaryJoints0[VertexIndex][3];
    end;
    if VertexIndex<length(TemporaryWeights0) then begin
     Vertex^.Weights.x:=TemporaryWeights0[VertexIndex][0];
     Vertex^.Weights.y:=TemporaryWeights0[VertexIndex][1];
     Vertex^.Weights.z:=TemporaryWeights0[VertexIndex][2];
     Vertex^.Weights.w:=TemporaryWeights0[VertexIndex][3];
    end;
{   for WeightIndex:=0 to 3 do begin
     if Vertex^.Weights0[WeightIndex]>0 then begin
      JointIndex:=Vertex^.Joints0[WeightIndex];
      OldCount:=length(DestinationMesh^.JointWeights);
      if OldCount<=JointIndex then begin
       SetLength(DestinationMesh^.JointWeights,(JointIndex+1)*2);
       for OtherJointIndex:=OldCount to length(DestinationMesh^.JointWeights)-1 do begin
        DestinationMesh^.JointWeights[OtherJointIndex]:=0.0;
       end;
      end;
      DestinationMesh^.JointWeights[JointIndex]:=Max(DestinationMesh^.JointWeights[JointIndex],Vertex^.Weights0[WeightIndex]);
     end;
     if Vertex^.Weights1[WeightIndex]>0 then begin
      JointIndex:=Vertex^.Joints1[WeightIndex];
      OldCount:=length(DestinationMesh^.JointWeights);
      if OldCount<=JointIndex then begin
       SetLength(DestinationMesh^.JointWeights,(JointIndex+1)*2);
       for OtherJointIndex:=OldCount to length(DestinationMesh^.JointWeights)-1 do begin
        DestinationMesh^.JointWeights[OtherJointIndex]:=0.0;
       end;
      end;
      DestinationMesh^.JointWeights[JointIndex]:=Max(DestinationMesh^.JointWeights[JointIndex],Vertex^.Weights1[WeightIndex]);
     end;
    end;}
   end;
  end;

  begin
   // Generate vertex index array buffer
   SetLength(DestinationMeshPrimitive^.Indices,length(TemporaryIndices));
   if length(TemporaryIndices)>0 then begin
    Move(TemporaryIndices[0],DestinationMeshPrimitive^.Indices[0],length(TemporaryIndices)*SizeOf(TpvUInt32));
   end;
  end;

  begin

   // Load morph target data

   SetLength(DestinationMeshPrimitive^.Targets,SourceMeshPrimitive.Targets.Count);

   MaxCountTargets:=Max(MaxCountTargets,length(DestinationMeshPrimitive^.Targets));

   for TargetIndex:=0 to length(DestinationMeshPrimitive^.Targets)-1 do begin

    SourceMeshPrimitiveTarget:=SourceMeshPrimitive.Targets[TargetIndex];

    DestinationMeshPrimitiveTarget:=@DestinationMeshPrimitive^.Targets[TargetIndex];

    AccessorIndex:=SourceMeshPrimitiveTarget['POSITION'];
    if AccessorIndex>=0 then begin
     TemporaryPositions:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
     if length(TemporaryPositions)<>length(DestinationMeshPrimitive^.Vertices) then begin
      raise EPasGLTF.Create('Vertex count mismatch');
     end;
    end else begin
     SetLength(TemporaryPositions,length(DestinationMeshPrimitive^.Vertices));
     for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
      TemporaryPositions[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
     end;
    end;

    AccessorIndex:=SourceMeshPrimitiveTarget['NORMAL'];
    if AccessorIndex>=0 then begin
     TemporaryNormals:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
     if length(TemporaryNormals)<>length(DestinationMeshPrimitive^.Vertices) then begin
      raise EPasGLTF.Create('Vertex count mismatch');
     end;
    end else begin
     SetLength(TemporaryNormals,length(DestinationMeshPrimitive^.Vertices));
     for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
      TemporaryNormals[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
     end;
    end;

    AccessorIndex:=SourceMeshPrimitiveTarget['TANGENT'];
    if AccessorIndex>=0 then begin
     TemporaryTargetTangents:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
     if length(TemporaryTargetTangents)<>length(DestinationMeshPrimitive^.Vertices) then begin
      raise EPasGLTF.Create('Vertex count mismatch');
     end;
     DoNeedCalculateTangents:=false;
    end else begin
     SetLength(TemporaryTargetTangents,length(DestinationMeshPrimitive^.Vertices));
     for VertexIndex:=0 to length(TemporaryTargetTangents)-1 do begin
      TemporaryTargetTangents[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
     end;
     DoNeedCalculateTangents:=true;
    end;

    // Construct morph target vertex array
    SetLength(DestinationMeshPrimitiveTarget^.Vertices,length(DestinationMeshPrimitive^.Vertices));
    for VertexIndex:=0 to length(DestinationMeshPrimitiveTarget^.Vertices)-1 do begin
     DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex];
     DestinationMeshPrimitiveTargetVertex^.Position:=TpvVector3(pointer(@TemporaryPositions[VertexIndex])^);
     DestinationMeshPrimitiveTargetVertex^.Normal.x:=TemporaryNormals[VertexIndex][0];
     DestinationMeshPrimitiveTargetVertex^.Normal.y:=TemporaryNormals[VertexIndex][1];
     DestinationMeshPrimitiveTargetVertex^.Normal.z:=TemporaryNormals[VertexIndex][2];
     DestinationMeshPrimitiveTargetVertex^.Tangent.x:=TemporaryTargetTangents[VertexIndex][0];
     DestinationMeshPrimitiveTargetVertex^.Tangent.y:=TemporaryTargetTangents[VertexIndex][1];
     DestinationMeshPrimitiveTargetVertex^.Tangent.z:=TemporaryTargetTangents[VertexIndex][2];
    end;

    if DoNeedCalculateTangents then begin
     SetLength(TemporaryTangents,length(TemporaryPositions));
     SetLength(TemporaryBitangents,length(TemporaryPositions));
     for VertexIndex:=0 to length(TemporaryTangents)-1 do begin
      PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^:=PpvVector3(pointer(@TPasGLTF.TDefaults.NullVector3))^;
      PpvVector3(pointer(@TemporaryBitangents[VertexIndex]))^:=PpvVector3(pointer(@TPasGLTF.TDefaults.NullVector3))^;
     end;
     if length(TemporaryTriangleIndices)>0 then begin
      for VertexIndex:=0 to length(TemporaryTangents)-1 do begin
       DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex];
       TemporaryPositions[VertexIndex,0]:=DestinationMeshPrimitive^.Vertices[VertexIndex].Position[0]+DestinationMeshPrimitiveTargetVertex^.Position[0];
       TemporaryPositions[VertexIndex,1]:=DestinationMeshPrimitive^.Vertices[VertexIndex].Position[1]+DestinationMeshPrimitiveTargetVertex^.Position[1];
       TemporaryPositions[VertexIndex,2]:=DestinationMeshPrimitive^.Vertices[VertexIndex].Position[2]+DestinationMeshPrimitiveTargetVertex^.Position[2];
       TemporaryNormals[VertexIndex,0]:=DestinationMeshPrimitive^.Vertices[VertexIndex].Normal.x+DestinationMeshPrimitiveTargetVertex^.Normal.x;
       TemporaryNormals[VertexIndex,1]:=DestinationMeshPrimitive^.Vertices[VertexIndex].Normal.y+DestinationMeshPrimitiveTargetVertex^.Normal.y;
       TemporaryNormals[VertexIndex,2]:=DestinationMeshPrimitive^.Vertices[VertexIndex].Normal.z+DestinationMeshPrimitiveTargetVertex^.Normal.z;
      end;
      IndexIndex:=0;
      while (IndexIndex+2)<length(TemporaryTriangleIndices) do begin
       p0:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+0]];
       p1:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+1]];
       p2:=@TemporaryPositions[TemporaryTriangleIndices[IndexIndex+2]];
       t0:=@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+0]];
       t1:=@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+1]];
       t2:=@TemporaryTexCoord0[TemporaryTriangleIndices[IndexIndex+2]];
       p1p0:=p1^-p0^;
       p2p0:=p2^-p0^;
       t1t0:=t1^-t0^;
       t2t0:=t2^-t0^;
       Normal:=(p1p0.Cross(p2p0)).Normalize;
       if PpvVector3(pointer(@TemporaryNormals[TemporaryTriangleIndices[IndexIndex+0]]))^.Dot(Normal)<0.0 then begin
        Normal:=-Normal;
       end;
{$if true}
       Area:=(t2t0[0]*t1t0[1])-(t1t0[0]*t2t0[1]);
       if IsZero(Area) then begin
        Tangent[0]:=0.0;
        Tangent[1]:=1.0;
        Tangent[2]:=0.0;
        Bitangent[0]:=1.0;
        Bitangent[1]:=0.0;
        Bitangent[2]:=0.0;
       end else begin
        Tangent[0]:=((t1t0[1]*p2p0[0])-(t2t0[1]*p1p0[0]))/Area;
        Tangent[1]:=((t1t0[1]*p2p0[1])-(t2t0[1]*p1p0[1]))/Area;
        Tangent[2]:=((t1t0[1]*p2p0[2])-(t2t0[1]*p1p0[2]))/Area;
        Bitangent[0]:=((t1t0[0]*p2p0[0])-(t2t0[0]*p1p0[0]))/Area;
        Bitangent[1]:=((t1t0[0]*p2p0[1])-(t2t0[0]*p1p0[1]))/Area;
        Bitangent[2]:=((t1t0[0]*p2p0[2])-(t2t0[0]*p1p0[2]))/Area;
       end;
       if (Tangent.Cross(Bitangent)).Dot(Normal)<0.0 then begin
        Tangent:=-Tangent;
        Bitangent:=-Bitangent;
       end;
{$else}
       Tangent[0]:=(t1t0[1]*p2p0[0])-(t2t0[1]*p1p0[0]);
       Tangent[1]:=(t1t0[1]*p2p0[1])-(t2t0[1]*p1p0[1]);
       Tangent[2]:=(t1t0[1]*p2p0[2])-(t2t0[1]*p1p0[2]);
       Bitangent[0]:=(t1t0[0]*p2p0[0])-(t2t0[0]*p1p0[0]);
       Bitangent[1]:=(t1t0[0]*p2p0[1])-(t2t0[0]*p1p0[1]);
       Bitangent[2]:=(t1t0[0]*p2p0[2])-(t2t0[0]*p1p0[2]);
       if (Tangent.Cross(Bitangent)).Dot(Normal)<0.0 then begin
        Tangent:=-Tangent;
        Bitangent:=-Bitangent;
       end;
{$ifend}
       PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+0]]))^:=PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+0]]))^+Tangent;
       PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+1]]))^:=PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+1]]))^+Tangent;
       PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+2]]))^:=PpvVector3(pointer(@TemporaryTangents[TemporaryTriangleIndices[IndexIndex+2]]))^+Tangent;
       PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+0]]))^:=PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+0]]))^+Bitangent;
       PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+1]]))^:=PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+1]]))^+Bitangent;
       PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+2]]))^:=PpvVector3(pointer(@TemporaryBitangents[TemporaryTriangleIndices[IndexIndex+2]]))^+Bitangent;
       inc(IndexIndex,3);
      end;
      for VertexIndex:=0 to length(TemporaryTangents)-1 do begin
       Normal:=PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^;
       Tangent:=PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^.Normalize;
       Tangent:=(Tangent-(Normal*Tangent.Dot(Normal))).Normalize;
       Bitangent:=PpvVector3(pointer(@TemporaryBitangents[VertexIndex]))^.Normalize;
       Bitangent:=(Bitangent-(Normal*Bitangent.Dot(Normal))).Normalize;
       PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^:=Tangent;
       if (PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^.Cross(Tangent)).Dot(Bitangent)<0.0 then begin
        TemporaryTangents[VertexIndex,3]:=-1.0;
       end else begin
        TemporaryTangents[VertexIndex,3]:=1.0;
       end;
      end;
     end;
     SetLength(DestinationMeshPrimitiveTarget^.Vertices,length(DestinationMeshPrimitive^.Vertices));
     for VertexIndex:=0 to length(DestinationMeshPrimitiveTarget^.Vertices)-1 do begin
      DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex];
      if trunc(TemporaryTangents[VertexIndex,3])<>trunc(DestinationMeshPrimitive^.Vertices[VertexIndex].Tangent.w.ToFloat) then begin
       DestinationMeshPrimitiveTargetVertex^.Tangent.x:=DestinationMeshPrimitive^.Vertices[VertexIndex].Tangent.x-TemporaryTangents[VertexIndex,0];
       DestinationMeshPrimitiveTargetVertex^.Tangent.y:=DestinationMeshPrimitive^.Vertices[VertexIndex].Tangent.y-TemporaryTangents[VertexIndex,1];
       DestinationMeshPrimitiveTargetVertex^.Tangent.z:=DestinationMeshPrimitive^.Vertices[VertexIndex].Tangent.z-TemporaryTangents[VertexIndex,2];
      end else begin
       DestinationMeshPrimitiveTargetVertex^.Tangent.x:=TemporaryTangents[VertexIndex,0]-DestinationMeshPrimitive^.Vertices[VertexIndex].Tangent.x;
       DestinationMeshPrimitiveTargetVertex^.Tangent.y:=TemporaryTangents[VertexIndex,1]-DestinationMeshPrimitive^.Vertices[VertexIndex].Tangent.y;
       DestinationMeshPrimitiveTargetVertex^.Tangent.z:=TemporaryTangents[VertexIndex,2]-DestinationMeshPrimitive^.Vertices[VertexIndex].Tangent.z;
      end;
     end;
    end;

   end;

  end;

 end;

 begin
  // Process morph target weights
  SetLength(DestinationMesh.fWeights,SourceMesh.Weights.Count);
  for WeightIndex:=0 to length(DestinationMesh.fWeights)-1 do begin
   DestinationMesh.fWeights[WeightIndex]:=SourceMesh.Weights[WeightIndex];
  end;
  OldCount:=length(DestinationMesh.fWeights);
  if OldCount<MaxCountTargets then begin
   SetLength(DestinationMesh.fWeights,MaxCountTargets);
   for WeightIndex:=OldCount to length(DestinationMesh.fWeights)-1 do begin
    DestinationMesh.fWeights[WeightIndex]:=0.0;
   end;
  end;
 end;

end;

{ TpvScene3D.TSkin }

constructor TpvScene3D.TGroup.TSkin.Create(const aGroup:TGroup);
begin
 inherited Create(aGroup);
end;

destructor TpvScene3D.TGroup.TSkin.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TSkin.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSkin:TPasGLTF.TSkin);
var Index,JointIndex,OldCount:TPasGLTFSizeInt;
    SourceSkin:TPasGLTF.TSkin;
    DestinationSkin:TSkin;
    JSONItem:TPasJSONItem;
    JSONObject:TPasJSONItemObject;
    InverseBindMatrices:TPasGLTF.TMatrix4x4DynamicArray;
begin

 SourceSkin:=aSourceSkin;

 DestinationSkin:=self;

 DestinationSkin.fName:=SourceSkin.Name;

 DestinationSkin.fSkeleton:=SourceSkin.Skeleton;

 DestinationSkin.fSkinShaderStorageBufferObjectIndex:=-1;

 DestinationSkin.fInverseBindMatrices.Initialize;

 if SourceSkin.InverseBindMatrices>=0 then begin
  InverseBindMatrices:=aSourceDocument.Accessors[SourceSkin.InverseBindMatrices].DecodeAsMatrix4x4Array(false);
  try
   DestinationSkin.fInverseBindMatrices.Count:=length(InverseBindMatrices);
   SetLength(DestinationSkin.fInverseBindMatrices.Items,DestinationSkin.fInverseBindMatrices.Count);
   if DestinationSkin.fInverseBindMatrices.Count>0 then begin
    Move(InverseBindMatrices[0],DestinationSkin.fInverseBindMatrices.Items[0],length(InverseBindMatrices)*SizeOf(TpvMatrix4x4));
   end;
  finally
   InverseBindMatrices:=nil;
  end;
 end;

 DestinationSkin.fMatrices.Initialize;
 DestinationSkin.fMatrices.Resize(SourceSkin.Joints.Count);

 DestinationSkin.fJoints.Initialize;
 DestinationSkin.fJoints.Resize(SourceSkin.Joints.Count);
 for JointIndex:=0 to DestinationSkin.fJoints.Count-1 do begin
  DestinationSkin.fJoints.Items[JointIndex]:=SourceSkin.Joints.Items[JointIndex];
 end;

 OldCount:=DestinationSkin.fInverseBindMatrices.Count;
 if OldCount<SourceSkin.Joints.Count then begin
  DestinationSkin.fInverseBindMatrices.Resize(SourceSkin.Joints.Count);
  for JointIndex:=0 to DestinationSkin.fInverseBindMatrices.Count-1 do begin
   DestinationSkin.fInverseBindMatrices.Items[JointIndex]:=TpvMatrix4x4(pointer(@TPasGLTF.TDefaults.IdentityMatrix4x4)^);
  end;
 end;

 DestinationSkin.fInverseBindMatrices.Finish;
 DestinationSkin.fMatrices.Finish;
 DestinationSkin.fJoints.Finish;

end;

{ TpvScene3D.TNode }

constructor TpvScene3D.TGroup.TNode.Create(const aGroup:TGroup);
begin

 inherited Create(aGroup);

 fChildren:=TNodes.Create;
 fChildren.OwnsObjects:=false;

 fMesh:=nil;

 fSkin:=nil;

end;

destructor TpvScene3D.TGroup.TNode.Destroy;
begin

 fMesh:=nil;

 fSkin:=nil;

 FreeAndNil(fChildren);

 inherited Destroy;
end;

{ TpvScene3D.TGroup }

constructor TpvScene3D.TGroup.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fObjects:=TIBaseObjects.Create;

end;

destructor TpvScene3D.TGroup.Destroy;
begin
 FreeAndNil(fObjects);
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fGroupListLock.Acquire;
 try
  fSceneInstance.fGroups.Add(self);
 finally
  fSceneInstance.fGroupListLock.Release;
 end;
end;

procedure TpvScene3D.TGroup.BeforeDestruction;
begin
 fObjects.Clear;
 fSceneInstance.fGroupListLock.Acquire;
 try
  fSceneInstance.fGroups.Remove(self);
 finally
  fSceneInstance.fGroupListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D.TGroupInstance }

constructor TpvScene3D.TGroupInstance.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fGroup:=nil;

end;

destructor TpvScene3D.TGroupInstance.Destroy;
begin
 fGroup:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TGroupInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fGroupInstanceListLock.Acquire;
 try
  fSceneInstance.fGroupInstances.Add(self);
 finally
  fSceneInstance.fGroupInstanceListLock.Release;
 end;
end;

procedure TpvScene3D.TGroupInstance.BeforeDestruction;
begin
 fGroup:=nil;
 fSceneInstance.fGroupInstanceListLock.Acquire;
 try
  fSceneInstance.fGroupInstances.Remove(self);
 finally
  fSceneInstance.fGroupInstanceListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D }

constructor TpvScene3D.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin

 inherited Create(aResourceManager,aParent);

 fTechniques:=TpvTechniques.Create;

 fImageListLock:=TPasMPSlimReaderWriterLock.Create;
 fImages:=TImages.Create;
 fImages.OwnsObjects:=false;

 fSamplerListLock:=TPasMPSlimReaderWriterLock.Create;
 fSamplers:=TSamplers.Create;
 fSamplers.OwnsObjects:=false;

 fTextureListLock:=TPasMPSlimReaderWriterLock.Create;
 fTextures:=TTextures.Create;
 fTextures.OwnsObjects:=false;

 fMaterialListLock:=TPasMPSlimReaderWriterLock.Create;
 fMaterials:=TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 fGroupListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroups:=TGroups.Create;
 fGroups.OwnsObjects:=false;

 fGroupInstanceListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroupInstances:=TGroupInstances.Create;
 fGroupInstances.OwnsObjects:=false;

 ReleaseFrameDelay:=MaxSwapChainImages+1;

end;

destructor TpvScene3D.Destroy;
begin

 while fGroupInstances.Count>0 do begin
  fGroupInstances[fGroupInstances.Count-1].Free;
 end;
 FreeAndNil(fGroupInstances);
 FreeAndNil(fGroupInstanceListLock);

 while fGroups.Count>0 do begin
  fGroups[fGroups.Count-1].Free;
 end;
 FreeAndNil(fGroups);
 FreeAndNil(fGroupListLock);

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

 while fSamplers.Count>0 do begin
  fSamplers[fSamplers.Count-1].Free;
 end;
 FreeAndNil(fSamplers);
 FreeAndNil(fSamplerListLock);

 while fImages.Count>0 do begin
  fImages[fImages.Count-1].Free;
 end;
 FreeAndNil(fImages);
 FreeAndNil(fImageListLock);

 FreeAndNil(fTechniques);

 inherited Destroy;
end;

initialization
end.

