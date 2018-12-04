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
                    Normal=1;
                    Tangent=2;
                    TexCoord0=3;
                    TexCoord1=4;
                    Color0=5;
                    Joints0=6;
                    Joints1=7;
                    Weights0=8;
                    Weights1=9;
                    VertexIndex=10;
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
               Position:TpvVector3;               //  12    0
               VertexIndex:TpvUInt32;             // + 4 = 12
               TangentSpace:TInt16Vector4;        // + 8 = 16 (signed 16-bit QTangent)
               TexCoord0:TpvVector2;              // + 8 = 24 (must be full 32-bit float)
               TexCoord1:TpvVector2;              // + 8 = 32 (must be full 32-bit float)
               Color0:TpvHalfFloatVector4;        // + 8 = 40 (must be at least half-float for HDR)
               Joints:TUInt16Vector4;             // + 8 = 48 (node-wise indirect indices for a 16-bit index to 32-bit index lookup table)
               Weights:TpvHalfFloatVector4;       // + 8 = 56 (half-float should be enough for it)
              );                                  //  ==   ==
              true:(                              //  64   64 per vertex
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
            IAnimation=interface(IBaseObject)['{CCD6AAF2-0B61-4831-AD09-1B78936AACA5}']
            end;
            TAnimation=class(TBaseObject,IAnimation)
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
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            TIAnimation=TpvGenericList<IAnimation>;
            TAnimations=TpvObjectGenericList<TAnimation>;
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

            IMesh=interface(IBaseObject)['{8AB4D1D1-5BF5-45BB-8E4B-DECA806AFD58}']
            end;
            TMesh=class(TBaseObject,IMesh)
             public
              type TPrimitive=record
                    public
                     type TTarget=record
                           public
                            type TTargetVertex=record
                                  Position:TPasGLTF.TVector3;
                                  Normal:TPasGLTF.TVector3;
                                  Tangent:TPasGLTF.TVector3;
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
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            TIMeshes=TpvGenericList<IMesh>;
            TMeshes=TpvObjectGenericList<TMesh>;
            INode=interface(IBaseObject)['{1C6E11BB-823E-4BE8-9C03-B93DB0B8CCDD}']
            end;
            TINodes=TpvGenericList<INode>;
            ISkin=interface(IBaseObject)['{942BE66B-6FD1-460B-BAA6-4F34C5FB44E4}']
            end;
            TSkin=class(TBaseObject,ISkin)
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
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            TISkins=TpvGenericList<ISkin>;
            TSkins=TpvObjectGenericList<TSkin>;
            IJoint=interface(IBaseObject)['{7D989671-0771-4CD2-BA84-4B9293B28E87}']
            end;
            TJoint=class(TBaseObject,IJoint)
             private
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
            end;
            TIJoints=TpvGenericList<IJoint>;
            TJoints=TpvObjectGenericList<TJoint>;
            TNode=class(TBaseObject,INode)
             private
              fChildren:TINodes;
              fMesh:IMesh;
              fSkin:ISkin;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              property Children:TINodes read fChildren;
              property Mesh:IMesh read fMesh write fMesh;
              property Skin:ISkin read fSkin write fSkin;
            end;
            TNodes=TpvObjectGenericList<TNode>;
            IGroup=interface(IBaseObject)['{66A53CBE-F7A9-433A-B995-BC1D3882D03B}']
            end;
            TGroup=class(TBaseObject,IGroup)
             private
              fObjects:TIBaseObjects;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              property Objects:TIBaseObjects read fObjects;
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
       fAnimationListLock:TPasMPSlimReaderWriterLock;
       fAnimations:TAnimations;
       fMaterialListLock:TPasMPSlimReaderWriterLock;
       fMaterials:TMaterials;
       fMeshListLock:TPasMPSlimReaderWriterLock;
       fMeshes:TMeshes;
       fSkinListLock:TPasMPSlimReaderWriterLock;
       fSkins:TSkins;
       fJointListLock:TPasMPSlimReaderWriterLock;
       fJoints:TJoints;
       fNodeListLock:TPasMPSlimReaderWriterLock;
       fNodes:TNodes;
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

{ TpvScene3D.TAnimation }

constructor TpvScene3D.TAnimation.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
end;

destructor TpvScene3D.TAnimation.Destroy;
begin

 inherited Destroy;
end;

procedure TpvScene3D.TAnimation.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fAnimationListLock.Acquire;
 try
  fSceneInstance.fAnimations.Add(self);
 finally
  fSceneInstance.fAnimationListLock.Release;
 end;
end;

procedure TpvScene3D.TAnimation.BeforeDestruction;
begin
 fSceneInstance.fAnimationListLock.Acquire;
 try
  fSceneInstance.fAnimations.Remove(self);
 finally
  fSceneInstance.fAnimationListLock.Release;
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

{ TpvScene3D.TMesh }

constructor TpvScene3D.TMesh.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
end;

destructor TpvScene3D.TMesh.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TMesh.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fMeshListLock.Acquire;
 try
  fSceneInstance.fMeshes.Add(self);
 finally
  fSceneInstance.fMeshListLock.Release;
 end;
end;

procedure TpvScene3D.TMesh.BeforeDestruction;
begin
 fSceneInstance.fMeshListLock.Acquire;
 try
  fSceneInstance.fMeshes.Remove(self);
 finally
  fSceneInstance.fMeshListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D.TSkin }

constructor TpvScene3D.TSkin.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
end;

destructor TpvScene3D.TSkin.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TSkin.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fSkinListLock.Acquire;
 try
  fSceneInstance.fSkins.Add(self);
 finally
  fSceneInstance.fSkinListLock.Release;
 end;
end;

procedure TpvScene3D.TSkin.BeforeDestruction;
begin
 fSceneInstance.fSkinListLock.Acquire;
 try
  fSceneInstance.fSkins.Remove(self);
 finally
  fSceneInstance.fSkinListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D.TJoint }

constructor TpvScene3D.TJoint.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
end;

destructor TpvScene3D.TJoint.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TJoint.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fJointListLock.Acquire;
 try
  fSceneInstance.fJoints.Add(self);
 finally
  fSceneInstance.fJointListLock.Release;
 end;
end;

procedure TpvScene3D.TJoint.BeforeDestruction;
begin
 fSceneInstance.fJointListLock.Acquire;
 try
  fSceneInstance.fJoints.Remove(self);
 finally
  fSceneInstance.fJointListLock.Release;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3D.TNode }

constructor TpvScene3D.TNode.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin

 inherited Create(aResourceManager,aParent);

 fChildren:=TINodes.Create;

 fMesh:=nil;

 fSkin:=nil;

end;

destructor TpvScene3D.TNode.Destroy;
begin

 fMesh:=nil;

 fSkin:=nil;

 FreeAndNil(fChildren);

 inherited Destroy;
end;

procedure TpvScene3D.TNode.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fNodeListLock.Acquire;
 try
  fSceneInstance.fNodes.Add(self);
 finally
  fSceneInstance.fNodeListLock.Release;
 end;
end;

procedure TpvScene3D.TNode.BeforeDestruction;
begin

 fChildren.Clear;

 fMesh:=nil;

 fSkin:=nil;

 fSceneInstance.fNodeListLock.Acquire;
 try
  fSceneInstance.fNodes.Remove(self);
 finally
  fSceneInstance.fNodeListLock.Release;
 end;

 inherited BeforeDestruction;

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

 fAnimationListLock:=TPasMPSlimReaderWriterLock.Create;
 fAnimations:=TAnimations.Create;
 fAnimations.OwnsObjects:=false;

 fMaterialListLock:=TPasMPSlimReaderWriterLock.Create;
 fMaterials:=TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 fMeshListLock:=TPasMPSlimReaderWriterLock.Create;
 fMeshes:=TMeshes.Create;
 fMeshes.OwnsObjects:=false;

 fSkinListLock:=TPasMPSlimReaderWriterLock.Create;
 fSkins:=TSkins.Create;
 fSkins.OwnsObjects:=false;

 fJointListLock:=TPasMPSlimReaderWriterLock.Create;
 fJoints:=TJoints.Create;
 fJoints.OwnsObjects:=false;

 fNodeListLock:=TPasMPSlimReaderWriterLock.Create;
 fNodes:=TNodes.Create;
 fNodes.OwnsObjects:=false;

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

 while fNodes.Count>0 do begin
  fNodes[fNodes.Count-1].Free;
 end;
 FreeAndNil(fNodes);
 FreeAndNil(fNodeListLock);

 while fJoints.Count>0 do begin
  fJoints[fJoints.Count-1].Free;
 end;
 FreeAndNil(fJoints);
 FreeAndNil(fJointListLock);

 while fSkins.Count>0 do begin
  fSkins[fSkins.Count-1].Free;
 end;
 FreeAndNil(fSkins);
 FreeAndNil(fSkinListLock);

 while fMeshes.Count>0 do begin
  fMeshes[fMeshes.Count-1].Free;
 end;
 FreeAndNil(fMeshes);
 FreeAndNil(fMeshListLock);

 while fMaterials.Count>0 do begin
  fMaterials[fMaterials.Count-1].Free;
 end;
 FreeAndNil(fMaterials);
 FreeAndNil(fMaterialListLock);

 while fAnimations.Count>0 do begin
  fAnimations[fAnimations.Count-1].Free;
 end;
 FreeAndNil(fAnimations);
 FreeAndNil(fAnimationListLock);

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

