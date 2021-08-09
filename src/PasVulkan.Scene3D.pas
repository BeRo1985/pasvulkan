(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
     PasVulkan.Hash.SHA3,
     PasVulkan.Collections,
     PasVulkan.HighResolutionTimer,
     PasVulkan.Resources,
     PasVulkan.Techniques,
     PasVulkan.Framework,
     PasVulkan.Application;

type EpvScene3D=class(Exception);

     TpvScene3D=class(TpvResource)
      public
       type TVertexAttributeBindingLocations=class
             public
              const Position=0;
                    NodeIndex=1;
                    TangentSpace=2;
                    TexCoord0=3;
                    TexCoord1=4;
                    Color0=5;
                    MorphTargetVertexBaseIndex=6;
                    CountMorphTargetVertices=7;
                    JointBlockBaseIndex=8;
                    CountJointBlocks=9;
            end;
            TUInt32Vector4=array[0..3] of TpvUInt32;
            TUInt16Vector4=array[0..3] of TpvUInt16;
            TInt16Vector4=array[0..3] of TpvInt16;
            PUInt32Vector4=^TUInt32Vector4;
            TMatrix4x4DynamicArray=TpvDynamicArray<TpvMatrix4x4>;
            TSizeIntDynamicArray=TpvDynamicArray<TpvSizeInt>;
            TVertex=packed record                    // Minimum required vertex structure for to be GLTF 2.0 conformant
             case boolean of
              false:(
               Position:TpvVector3;                  //  12   12 (32-bit float 3D vector)
               NodeIndex:TpvUInt32;                  // + 4 = 16 (unsigned 32-bit node index)
               TangentSpace:TInt16Vector4;           // + 8 = 24 (signed 16-bit integer QTangent)
               TexCoord0:TpvVector2;                 // + 8 = 32 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               TexCoord1:TpvVector2;                 // + 8 = 40 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               Color0:TpvHalfFloatVector4;           // + 8 = 48 (must be at least half-float for HDR)
               MorphTargetVertexBaseIndex:TpvUInt32; // + 4 = 52 (unsigned 32-bit morph target vertex base index)
               CountMorphTargetVertices:TpvUInt32;   // + 4 = 56 (unsigned 32-bit count of morph target vertices)
               JointBlockBaseIndex:TpvUInt32;        // + 4 = 60 (unsigned 32-bit joint block base index)
               CountJointBlocks:TpvUInt32;           // + 4 = 64 (unsigned 32-bit count of joint blocks)
              );                                     //  ==   ==
              true:(                                 //  64   64 per vertex
               Padding:array[0..63] of TpvUInt8;
              );
            end;
            PVertex=^TVertex;
            TVertices=array of TVertex;
            TJointBlock=packed record
             case boolean of
              false:(
               Joints:TUInt32Vector4;                //  16 = 16
               Weights:TpvVector4;                   // +16 = 32
              );                                     //  ==   ==
              true:(                                 //  32   32
               Padding:array[0..31] of TpvUInt8;
              );
            end;
            PJointBlock=^TJointBlock;
            TJointBlocks=array of TJointBlock;
            TMaxJointBlocks=array[0..9] of TJointBlock;
            PMaxJointBlocks=^TMaxJointBlocks;
            { TBaseObject }
            TBaseObject=class(TpvResource)
             private
              fSceneInstance:TpvScene3D;
              fName:TpvUTF8String;
              fReferenceCounter:Int32;
              fUploaded:TPasMPBool32;
              fAdded:TPasMPBool32;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; virtual;
              procedure Upload; virtual;
              procedure Unload; virtual;
              procedure IncRef; virtual;
              procedure DecRef; virtual;
             public
              property SceneInstance:TpvScene3D read fSceneInstance;
             published
              property Name:TpvUTF8String read fName write fName;
              property Uploaded:TPasMPBool32 read fUploaded;
            end;
            TBaseObjects=TpvObjectGenericList<TBaseObject>;
            TGroup=class;
            TBaseGroupObject=class(TBaseObject)
             private
              fGroup:TGroup;
            end;
            { TImage }
            TImage=class(TBaseObject)
             public
              type THashData=packed record
                    MessageDigest:TpvHashSHA3.TMessageDigest;
                   end;
                   PHashData=^THashData;
             private
              fResourceDataStream:TMemoryStream;
              fHashData:THashData;
              fTexture:TpvVulkanTexture;
              fLock:TPasMPSpinLock;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure Upload; override;
              procedure Unload; override;
              function GetHashData:THashData;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceImage:TPasGLTF.TImage);
            end;
            TImages=TpvObjectGenericList<TImage>;
            { TSampler }
            TSampler=class(TBaseObject)
             public
              type THashData=packed record
                    MinFilter:TVkFilter;
                    MagFilter:TVkFilter;
                    MipmapMode:TVkSamplerMipmapMode;
                    AddressModeS:TVkSamplerAddressMode;
                    AddressModeT:TVkSamplerAddressMode;
                   end;
                   PHashData=^THashData;
             private
              fMinFilter:TVkFilter;
              fMagFilter:TVkFilter;
              fMipmapMode:TVkSamplerMipmapMode;
              fAddressModeS:TVkSamplerAddressMode;
              fAddressModeT:TVkSamplerAddressMode;
              fLock:TPasMPSpinLock;
              fSampler:TpvVulkanSampler;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure Upload; override;
              procedure Unload; override;
              function GetHashData:THashData;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSampler:TPasGLTF.TSampler);
             published
              property Sampler:TpvVulkanSampler read fSampler;
            end;
            TSamplers=TpvObjectGenericList<TSampler>;
            { TTexture }
            TTexture=class(TBaseObject)
             public
              type THashData=packed record
                    Image:TImage;
                    Sampler:TSampler;
                   end;
                   PHashData=^THashData;
             private
              fImage:TImage;
              fSampler:TSampler;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure Upload; override;
              procedure Unload; override;
              function GetHashData:THashData;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceTexture:TPasGLTF.TTexture;const aImageMap:TImages;const aSamplerMap:TSamplers);
              function GetDescriptorImageInfo:TVkDescriptorImageInfo;
             published
              property Image:TImage read fImage write fImage;
              property Sampler:TSampler read fSampler write fSampler;
            end;
            TTextures=TpvObjectGenericList<TTexture>;
            TMaterial=class(TBaseObject)
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
                   { TTextureReference }
                   TTextureReference=record
                    public
                     type { TTransform }
                          TTransform=record
                           public
                            Active:boolean;
                            Offset:TpvVector2;
                            Rotation:TpvFloat;
                            Scale:TpvVector2;
                           public
                            procedure AssignFromGLTF(var aTextureReference:TTextureReference;const aExtensionsItem:TPasJSONItem);
                            function ToMatrix4x4:TpvMatrix4x4;
                          end;
                          PTransform=^TTransform;
                    public
                     Texture:TpvScene3D.TTexture;
                     TexCoord:TpvSizeInt;
                     Transform:TTransform;
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
                   TPBRSheen=record
                    Active:boolean;
                    IntensityFactor:TpvFloat;
                    ColorFactor:TpvVector3;
                    ColorIntensityTexture:TTextureReference;
                   end;
                   PPBRSheen=^TPBRSheen;
                   TPBRClearCoat=record
                    Active:boolean;
                    Factor:TpvFloat;
                    Texture:TTextureReference;
                    RoughnessFactor:TpvFloat;
                    RoughnessTexture:TTextureReference;
                    NormalTexture:TTextureReference;
                   end;
                   PPBRClearCoat=^TPBRClearCoat;
                   TUnlit=record
                    Dummy:TpvInt32;
                   end;
                   PUnlit=^TUnlit;
                   TShaderData=packed record // 2048 bytes
                    case boolean of
                     false:(
                      BaseColorFactor:TpvVector4;
                      SpecularFactor:TpvVector4; // actually TpvVector3, but for easier and more convenient alignment reasons a TpvVector4
                      EmissiveFactor:TpvVector4; // actually TpvVector3, but for easier and more convenient alignment reasons a TpvVector4
                      MetallicRoughnessNormalScaleOcclusionStrengthFactor:TpvVector4;
                      SheenColorFactorSheenIntensityFactor:TpvVector4;
                      ClearcoatFactorClearcoatRoughnessFactor:TpvVector4;
                      // uvec4 AlphaCutOffFlags begin
                       AlphaCutOff:TpvFloat; // for with uintBitsToFloat on GLSL code side
                       Flags:TpvUInt32;
                       Textures0:TPasGLTFUInt32;
                       Textures1:TPasGLTFUInt32;
                       // uvec4 uAlphaCutOffFlags end
                       TextureTransforms:array[0..15] of TpvMatrix4x4;
                     );
                     true:(
                      Padding:array[0..2047] of TpvUInt8;
                     );
                   end;
                   PShaderData=^TShaderData;
                   TData=record
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
                    PBRSheen:TPBRSheen;
                    PBRClearCoat:TPBRClearCoat;
                    Unlit:TUnlit;
                   end;
                   PData=^TData;
                   THashData=TData;
                   PHashData=^THashData;
              const DefaultData:TData=(
                     ShadingModel:TpvScene3D.TMaterial.TShadingModel.PBRMetallicRoughness;
                     AlphaCutOff:1.0;
                     AlphaMode:TpvScene3D.TMaterial.TAlphaMode.Opaque;
                     DoubleSided:false;
                     NormalTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     NormalTextureScale:1.0;
                     OcclusionTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     OcclusionTextureStrength:1.0;
                     EmissiveFactor:(x:1.0;y:1.0;z:1.0);
                     EmissiveTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     PBRMetallicRoughness:(
                      BaseColorFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                      BaseColorTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      RoughnessFactor:1.0;
                      MetallicFactor:1.0;
                      MetallicRoughnessTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     );
                     PBRSpecularGlossiness:(
                      DiffuseFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                      DiffuseTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      GlossinessFactor:1.0;
                      SpecularFactor:(x:1.0;y:1.0;z:1.0);
                      SpecularGlossinessTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     );
                     PBRSheen:(
                      Active:false;
                      IntensityFactor:1.0;
                      ColorFactor:(x:1.0;y:1.0;z:1.0);
                      ColorIntensityTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     );
                     PBRClearCoat:(
                      Active:false;
                      Factor:1.0;
                      Texture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      RoughnessFactor:1.0;
                      RoughnessTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      NormalTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     );
                     Unlit:(
                      Dummy:0;
                     );
                    );
                   DefaultShaderData:TShaderData=
                    (
                     BaseColorFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SpecularFactor:(x:1.0;y:1.0;z:1.0;w:0.0);
                     EmissiveFactor:(x:0.0;y:0.0;z:0.0;w:0.0);
                     MetallicRoughnessNormalScaleOcclusionStrengthFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SheenColorFactorSheenIntensityFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     ClearcoatFactorClearcoatRoughnessFactor:(x:0.0;y:0.0;z:1.0;w:1.0);
                     AlphaCutOff:1.0;
                     Flags:0;
                     Textures0:$ffffffff;
                     Textures1:$ffffffff;
                     TextureTransforms:(
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0))),
                      (RawComponents:((1.0,0.0,0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0,1.0)))
                     );
                    );
             private
              fData:TData;
              fShaderData:TShaderData;
              fUniformBufferObjectIndex:TpvSizeInt;
              fUniformBufferObjectOffset:TpvSizeInt;
              fLock:TPasMPSpinLock;
              fShaderDataUniformBlockBuffer:TpvVulkanBuffer;
              fVulkanDescriptorPool:TpvVulkanDescriptorPool;
              fVulkanDescriptorSet:TpvVulkanDescriptorSet;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMaterial:TPasGLTF.TMaterial;const aTextureMap:TTextures);
              procedure FillShaderData;
            end;
            TMaterials=TpvObjectGenericList<TMaterial>;
            { TGroup }
            TGroup=class(TBaseObject) // A group is a GLTF scene in a uber-scene
             public
              type TNode=class;
                   TMesh=class;
                   TGroupVertices=TpvDynamicArray<TVertex>;
                   TGroupIndices=TpvDynamicArray<TVkUInt32>;
                   TGroupJointBlocks=TpvDynamicArray<TJointBlock>;
                   TMorphTargetVertex=packed record
                    case boolean of
                     false:(
                      Position:TpvVector3;               //  12   12
                      Index:TpvUInt32;                   // + 4   16
                      Normal:TpvHalfFloatVector3;        // + 6   22
                      Tangent:TpvHalfFloatVector3;       // + 6   24
                      Next:TpvUInt32;                    // + 4   32
                     );                                  //  ==   ==
                     true:(                              //  32   32 per vertex
                      Padding:array[0..31] of TpvUInt8;
                     );
                   end;
                   PMorphTargetVertex=^TMorphTargetVertex;
                   TMorphTargetVertexDynamicArray=TpvDynamicArray<TMorphTargetVertex>;
                   TMorphTargetShaderStorageBufferObject=record
                    Count:TpvSizeInt;
                    Size:TpvSizeInt;
                    Data:TpvFloatDynamicArray;
                   end;
                   PMorphTargetShaderStorageBufferObject=^TMorphTargetShaderStorageBufferObject;
                   TMorphTargetVertexShaderStorageBufferObject=record
                    Count:TpvSizeInt;
                    Size:TpvSizeInt;
                    Data:TBytes;
                   end;
                   PMorphTargetVertexShaderStorageBufferObject=^TMorphTargetVertexShaderStorageBufferObject;
                   TNodeShaderStorageBufferObjectDataItem=packed record
                    Matrices:array[0..0] of TpvMatrix4x4;
                   end;
                   PNodeMeshPrimitiveShaderStorageBufferObjectDataItem=^TNodeShaderStorageBufferObjectDataItem;
                   TNodeShaderStorageBufferObjectDataItems=array of TNodeShaderStorageBufferObjectDataItem;
                   TNodeShaderStorageBufferObject=record
                    Size:TpvSizeInt;
                    Count:TpvSizeInt;
                   end;
                   PNodeMeshPrimitiveShaderStorageBufferObject=^TNodeShaderStorageBufferObject;
                   TGroupObject=class
                    private
                     fName:TpvUTF8String;
                     fGroup:TGroup;
                    public
                     constructor Create(const aGroup:TGroup); reintroduce; virtual;
                     destructor Destroy; override;
                    published
                     property Group:TGroup read fGroup write fGroup;
                   end;
                   { TAnimation }
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
                     fIndex:TpvSizeInt;
                     fChannels:TChannels;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceAnimation:TPasGLTF.TAnimation);
                     function GetAnimationBeginTime:TpvFloat;
                     function GetAnimationEndTime:TpvFloat;
                    published
                     property Index:TpvSizeInt read fIndex;
                   end;
                   TAnimations=TpvObjectGenericList<TAnimation>;
                   TCamera=class(TGroupObject)
                    public
                     type TType=
                           (
                            None=0,
                            Orthographic=1,
                            Perspective=2
                           );
                          TOrthographic=record
                           XMag:TpvFloat;
                           YMag:TpvFloat;
                           ZNear:TpvFloat;
                           ZFar:TpvFloat;
                          end;
                          TPerspective=record
                           AspectRatio:TpvFloat;
                           YFoV:TpvFloat;
                           ZNear:TpvFloat;
                           ZFar:TpvFloat;
                          end;
                    private
                     fIndex:TpvSizeInt;
                     fType:TType;
                     fOrthographic:TOrthographic;
                     fPerspective:TPerspective;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceCamera:TPasGLTF.TCamera);
                    published
                     property Index:TpvSizeInt read fIndex;
                   end;
                   TCameras=TpvObjectGenericList<TCamera>;
                   TMesh=class(TGroupObject)
                    public
                     type TPrimitive=record
                           public
                            type TTarget=record
                                  public
                                   type TTargetVertex=packed record
                                         Position:TpvVector3;
                                         Normal:TpvHalfFloatVector3;
                                         Tangent:TpvHalfFloatVector3;
                                         Count:TpvUInt32;
                                        end;
                                        PTargetVertex=^TTargetVertex;
                                        TTargetVertices=array of TTargetVertex;
                                  public
                                   Vertices:TTargetVertices;
                                 end;
                                 PTarget=^TTarget;
                                 TTargets=array of TTarget;
                                 TNodeMeshPrimitiveInstance=record
                                  MorphTargetBaseIndex:TpvSizeUInt;
                                  StartBufferVertexOffset:TpvSizeUInt;
                                  StartBufferIndexOffset:TpvSizeUInt;
                                 end;
                                 PNodeMeshPrimitiveInstance=^TNodeMeshPrimitiveInstance;
                                 TNodeMeshPrimitiveInstances=TpvDynamicArray<TNodeMeshPrimitiveInstance>;
                           public
                            PrimitiveMode:TVkPrimitiveTopology;
                            Material:TMaterial;
                            Targets:TTargets;
                            MorphTargetBaseIndex:TpvSizeUInt;
                            StartBufferVertexOffset:TpvSizeUInt;
                            StartBufferIndexOffset:TpvSizeUInt;
                            CountVertices:TpvSizeUInt;
                            CountIndices:TpvSizeUInt;
                            NodeMeshPrimitiveInstances:TNodeMeshPrimitiveInstances;
                          end;
                          PPrimitive=^TPrimitive;
                          TPrimitives=array of TPrimitive;
                    private
                     fIndex:TpvSizeInt;
                     fPrimitives:TPrimitives;
                     fBoundingBox:TpvAABB;
                     fWeights:TpvFloatDynamicArray;
                     fNodeMeshInstances:TpvSizeInt;
                     function CreateNodeMeshInstance(const aNodeIndex:TpvUInt32):TpvSizeInt;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMesh:TPasGLTF.TMesh;const aMaterialMap:TpvScene3D.TMaterials);
                    published
                     property Index:TpvSizeInt read fIndex;
                   end;
                   TMeshes=TpvObjectGenericList<TMesh>;
                   TSkin=class(TGroupObject)
                    private
                     fIndex:TpvSizeInt;
                     fSkeleton:TpvSizeInt;
                     fInverseBindMatrices:TMatrix4x4DynamicArray;
                     fMatrices:TMatrix4x4DynamicArray;
                     fJoints:TSizeIntDynamicArray;
                     fStorageBufferObjectOffset:TpvSizeUInt;
                     fStorageBufferObjectSize:TpvSizeUInt;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSkin:TPasGLTF.TSkin);
                    published
                     property Index:TpvSizeInt read fIndex;
                   end;
                   TSkins=TpvObjectGenericList<TSkin>;
                   TSkinDynamicArray=TpvDynamicArray<TSkin>;
                   TNodes=TpvObjectGenericList<TNode>;
                   TLight=class(TGroupObject)
                    public
                     type TType=
                           (
                            None=0,
                            Directional=1,
                            Point=2,
                            Spot=3
                           );
                    private
                     fIndex:TpvSizeInt;
                     fType_:TType;
                     fNodes:TNodes;
                     fShadowMapIndex:TpvInt32;
                     fIntensity:TpvFloat;
                     fRange:TpvFloat;
                     fInnerConeAngle:TpvFloat;
                     fOuterConeAngle:TpvFloat;
                     fDirection:TpvVector3;
                     fColor:TpvVector3;
                     fCastShadows:boolean;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceLight:TPasJSONItemObject);
                    published
                     property Index:TpvSizeInt read fIndex;
                   end;
                   TLights=TpvObjectGenericList<TLight>;
                   TNode=class(TGroupObject)
                    public
                     type TChildNodeIndices=TpvDynamicArray<TpvSizeInt>;
                    private
                     fIndex:TpvSizeInt;
                     fChildNodeIndices:TChildNodeIndices;
                     fChildren:TNodes;
                     fMesh:TMesh;
                     fCamera:TCamera;
                     fSkin:TSkin;
                     fLight:TLight;
                     fWeights:TpvFloatDynamicArray;
                     fJoint:TPasGLTFSizeInt;
                     fMatrix:TpvMatrix4x4;
                     fTranslation:TpvVector3;
                     fRotation:TpvVector4;
                     fScale:TpvVector3;
                     fShaderStorageBufferObjectOffset:TpvSizeInt;
                     fShaderStorageBufferObjectSize:TpvSizeInt;
                     procedure Finish;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceNode:TPasGLTF.TNode;const aLightMap:TLights);
                    published
                     property Index:TpvSizeInt read fIndex;
                     property Children:TNodes read fChildren;
                     property Camera:TCamera read fCamera;
                     property Mesh:TMesh read fMesh;
                     property Skin:TSkin read fSkin;
                   end;
                   TScene=class(TGroupObject)
                    private
                     fIndex:TpvSizeInt;
                     fNodes:TNodes;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceScene:TPasGLTF.TScene);
                    published
                     property Index:TpvSizeInt read fIndex;
                     property Nodes:TNodes read fNodes;
                   end;
                   TScenes=TpvObjectGenericList<TScene>;
                   { TInstance }
                   TInstance=class(TBaseObject)
                    public
                     type { TAnimation }
                          TAnimation=class
                           private
                            fFactor:TPasGLTFFloat;
                            fTime:TPasGLTFFloat;
                           public
                            constructor Create; reintroduce;
                            destructor Destroy; override;
                           published
                            property Factor:TPasGLTFFloat read fFactor write fFactor;
                            property Time:TPasGLTFFloat read fTime write fTime;
                          end;
                          TAnimations=array of TAnimation;
                          TNode=record
                           public
                            type TOverwriteFlag=
                                  (
                                   Defaults,
                                   Translation,
                                   Rotation,
                                   Scale,
                                   Weights
                                  );
                                 TOverwriteFlags=set of TOverwriteFlag;
                                 TOverwrite=record
                                  public
                                   Flags:TOverwriteFlags;
                                   Translation:TpvVector3;
                                   Rotation:TpvVector4;
                                   Scale:TpvVector3;
                                   Weights:TpvFloatDynamicArray;
                                   Factor:TpvFloat;
                                 end;
                                 POverwrite=^TOverwrite;
                                 TOverwrites=array of TOverwrite;
                           public
                            Overwrites:TOverwrites;
                            CountOverwrites:TpvSizeInt;
                            OverwriteFlags:TOverwriteFlags;
                            OverwriteTranslation:TpvVector3;
                            OverwriteRotation:TpvVector4;
                            OverwriteScale:TpvVector3;
                            OverwriteWeights:TpvFloatDynamicArray;
                            OverwriteWeightsSum:TpvDoubleDynamicArray;
                            WorkWeights:TpvFloatDynamicArray;
                            WorkMatrix:TpvMatrix4x4;
                          end;
                          PNode=^TNode;
                          TNodes=array of TNode;
                          TSkin=record
                           Used:boolean;
                          end;
                          PSkin=^TSkin;
                          TSkins=array of TSkin;
                          TNodeIndices=array of TpvSizeInt;
                          TOnNodeMatrix=procedure(const aInstance:TInstance;aNode,InstanceNode:pointer;var Matrix:TpvMatrix4x4) of object;
                          TNodeMatrices=array of TpvMatrix4x4;
                          TMorphTargetVertexWeights=array of TpvFloat;
                          { TVulkanData }
                          TVulkanData=class
                           private
                            fUploaded:boolean;
                            fInstance:TInstance;
                            fNodeMatricesBuffer:TpvVulkanBuffer;
                            fMorphTargetVertexWeightsBuffer:TpvVulkanBuffer;
                           public
                            constructor Create(const aInstance:TInstance); reintroduce;
                            destructor Destroy; override;
                            procedure Upload;
                            procedure Unload;
                            procedure Update;
                           published
                            property NodeMatricesBuffer:TpvVulkanBuffer read fNodeMatricesBuffer;
                            property MorphTargetVertexWeightsBuffer:TpvVulkanBuffer read fMorphTargetVertexWeightsBuffer;
                          end;
                          TVulkanDatas=array[0..MaxSwapChainImages+1] of TVulkanData;
                    private
                     fGroup:TGroup;
                     fScene:TPasGLTFSizeInt;
                     fAnimations:TAnimations;
                     fNodes:TNodes;
                     fSkins:TSkins;
                     fLightNodes:TNodeIndices;
                     fLightShadowMapMatrices:TPasGLTF.TMatrix4x4DynamicArray;
                     fLightShadowMapZFarValues:TPasGLTFFloatDynamicArray;
                     fDynamicBoundingBox:TpvAABB;
                     fWorstCaseStaticBoundingBox:TpvAABB;
                     fUserData:pointer;
                     fOnNodeMatrixPre:TOnNodeMatrix;
                     fOnNodeMatrixPost:TOnNodeMatrix;
                     fUploaded:boolean;
                     fNodeMatrices:TNodeMatrices;
                     fMorphTargetVertexWeights:TMorphTargetVertexWeights;
                     fVulkanDatas:TVulkanDatas;
                     fVulkanData:TVulkanData;
                     function GetAutomation(const aIndex:TPasGLTFSizeInt):TAnimation;
                     procedure SetScene(const aScene:TpvSizeInt);
                     function GetScene:TpvScene3D.TGroup.TScene;
                    public
                     constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
                     destructor Destroy; override;
                     procedure AfterConstruction; override;
                     procedure BeforeDestruction; override;
                     procedure Upload; override;
                     procedure Unload; override;
                     procedure Update;
                    published
                     property Group:TGroup read fGroup write fGroup;
                     property Scene:TpvSizeInt read fScene write SetScene;
                    public
                     property Nodes:TNodes read fNodes;
                     property Skins:TSkins read fSkins;
                     property UserData:pointer read fUserData write fUserData;
                    published
                     property VulkanData:TVulkanData read fVulkanData;
                     property Automations[const aIndex:TPasGLTFSizeInt]:TAnimation read GetAutomation;
                     property OnNodeMatrixPre:TOnNodeMatrix read fOnNodeMatrixPre write fOnNodeMatrixPre;
                     property OnNodeMatrixPost:TOnNodeMatrix read fOnNodeMatrixPost write fOnNodeMatrixPost;
                   end;
                   TInstances=TpvObjectGenericList<TInstance>;
             private
              fMaximalCountInstances:TpvSizeInt;
              fObjects:TBaseObjects;
              fAnimations:TAnimations;
              fCameras:TCameras;
              fMeshes:TMeshes;
              fSkins:TSkins;
              fLights:TLights;
              fNodes:TNodes;
              fScenes:TScenes;
              fScene:TScene;
              fVertices:TGroupVertices;
              fIndices:TGroupIndices;
              fJointBlocks:TGroupJointBlocks;
              fMorphTargetVertices:TMorphTargetVertexDynamicArray;
              fSkinStorageBufferSize:TpvSizeInt;
              fMorphTargetCount:TpvSizeInt;
              fNodeShaderStorageBufferObject:TNodeShaderStorageBufferObject;
              fLock:TPasMPSpinLock;
              fVulkanVertexBuffer:TpvVulkanBuffer;
              fVulkanIndexBuffer:TpvVulkanBuffer;
              fVulkanMorphTargetVertexBuffer:TpvVulkanBuffer;
              fVulkanInstancesNodeMatricesBuffer:TpvVulkanBuffer;
              fVulkanInstancesMorphTargetVertexWeightsBuffer:TpvVulkanBuffer;
              fInstanceListLock:TPasMPSlimReaderWriterLock;
              fInstances:TInstances;
              fBoundingBox:TpvAABB;
              procedure ConstructBuffers;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aMaximalCountInstances:TpvSizeInt=1);
              function CreateInstance:TpvScene3D.TGroup.TInstance;
             public
              property BoundingBox:TpvAABB read fBoundingBox;
             published
              property Objects:TBaseObjects read fObjects;
              property Animations:TAnimations read fAnimations;
              property Cameras:TCameras read fCameras;
              property Meshes:TMeshes read fMeshes;
              property Skins:TSkins read fSkins;
              property Lights:TLights read fLights;
              property Nodes:TNodes read fNodes;
              property Scenes:TScenes read fScenes;
              property Scene:TScene read fScene;
            end;
            TGroups=TpvObjectGenericList<TGroup>;
            TImageHashMap=TpvHashMap<TImage.THashData,TImage>;
            TSamplerHashMap=TpvHashMap<TSampler.THashData,TSampler>;
            TTextureHashMap=TpvHashMap<TTexture.THashData,TTexture>;
            TMaterialHashMap=TpvHashMap<TMaterial.THashData,TMaterial>;
      private
       fLock:TPasMPSpinLock;
       fUploaded:TPasMPBool32;
       fWhiteTexture:TpvVulkanTexture;
       fWhiteTextureLock:TPasMPSlimReaderWriterLock;
       fDefaultNormalMapTexture:TpvVulkanTexture;
       fDefaultNormalMapTextureLock:TPasMPSlimReaderWriterLock;
       fMaterialVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fTechniques:TpvTechniques;
       fImageListLock:TPasMPSlimReaderWriterLock;
       fImages:TImages;
       fImageHashMap:TImageHashMap;
       fSamplerListLock:TPasMPSlimReaderWriterLock;
       fSamplers:TSamplers;
       fSamplerHashMap:TSamplerHashMap;
       fTextureListLock:TPasMPSlimReaderWriterLock;
       fTextures:TTextures;
       fTextureHashMap:TTextureHashMap;
       fMaterialListLock:TPasMPSlimReaderWriterLock;
       fMaterials:TMaterials;
       fMaterialHashMap:TMaterialHashMap;
       fGroupListLock:TPasMPSlimReaderWriterLock;
       fGroups:TGroups;
       fGroupInstanceListLock:TPasMPSlimReaderWriterLock;
       fGroupInstances:TGroup.TInstances;
       procedure UploadWhiteTexture;
       procedure UploadDefaultNormalMapTexture;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
       destructor Destroy; override;
       procedure Upload;
       procedure Unload;
       procedure Draw(const aViewMatrix,aProjectionMatrix:TpvMatrix4x4;const aViewRect:TpvRect);
     end;

implementation

{ TpvScene3D.TBaseObject }

constructor TpvScene3D.TBaseObject.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
var Current:TpvResource;
begin
 inherited Create(aResourceManager,aParent);

 if assigned(Parent) then begin
  Current:=Parent;
  while assigned(Current) and not (Current is TpvScene3D) do begin
   Current:=Current.Parent;
  end;
  if assigned(Current) and (Current is TpvScene3D) then begin
   fSceneInstance:=TpvScene3D(Current);
  end else begin
   fSceneInstance:=nil;
  end;
 end else begin
  fSceneInstance:=nil;
 end;

 ReleaseFrameDelay:=MaxSwapChainImages+1;

 fUploaded:=false;

 fAdded:=false;

 fReferenceCounter:=0;

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

procedure TpvScene3D.TBaseObject.Remove;
begin
end;

procedure TpvScene3D.TBaseObject.Upload;
begin
end;

procedure TpvScene3D.TBaseObject.Unload;
begin
end;

procedure TpvScene3D.TBaseObject.IncRef;
begin
 TPasMPInterlocked.Increment(fReferenceCounter);
end;

procedure TpvScene3D.TBaseObject.DecRef;
begin
 if assigned(self) and (TPasMPInterlocked.Decrement(fReferenceCounter)=0) then begin
  try
   Remove;
  finally
   DeferredFree;
  end;
 end;
end;

{ TpvScene3D.TImage }

constructor TpvScene3D.TImage.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
 fResourceDataStream:=TMemoryStream.Create;
 fLock:=TPasMPSpinLock.Create;
end;

destructor TpvScene3D.TImage.Destroy;
begin
 Unload;
 FreeAndNil(fLock);
 FreeAndNil(fResourceDataStream);
 inherited Destroy;
end;

procedure TpvScene3D.TImage.AfterConstruction;
begin
 inherited AfterConstruction;
 try
  fSceneInstance.fImageListLock.Acquire;
  try
   fSceneInstance.fImages.Add(self);
  finally
   fSceneInstance.fImageListLock.Release;
  end;
 finally
  fAdded:=true;
 end;
end;

procedure TpvScene3D.TImage.BeforeDestruction;
begin
 Remove;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TImage.Remove;
begin
 if fAdded then begin
  try
   fSceneInstance.fImageListLock.Acquire;
   try
    fSceneInstance.fImages.Remove(self);
    if fSceneInstance.fImageHashMap[fHashData]=self then begin
     fSceneInstance.fImageHashMap.Delete(fHashData);
    end;
   finally
    fSceneInstance.fImageListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

procedure TpvScene3D.TImage.Upload;
var GraphicsQueue:TpvVulkanQueue;
    GraphicsCommandPool:TpvVulkanCommandPool;
    GraphicsCommandBuffer:TpvVulkanCommandBuffer;
    GraphicsFence:TpvVulkanFence;
begin
 if (fReferenceCounter>0) and not fUploaded then begin
  fLock.Acquire;
  try
   if (fReferenceCounter>0) and not fUploaded then begin
    try
     GraphicsQueue:=pvApplication.VulkanDevice.GraphicsQueue;
     try
      GraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                       pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                       TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
      try
       GraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(GraphicsCommandPool,
                                                            VK_COMMAND_BUFFER_LEVEL_PRIMARY);
       try
        GraphicsFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
        try
         fTexture:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                    GraphicsQueue,
                                                    GraphicsCommandBuffer,
                                                    GraphicsFence,
                                                    GraphicsQueue,
                                                    GraphicsCommandBuffer,
                                                    GraphicsFence,
                                                    fResourceDataStream,
                                                    true,
                                                    false);
        finally
         FreeAndNil(GraphicsFence);
        end;
       finally
        FreeAndNil(GraphicsCommandBuffer);
       end;
      finally
       FreeAndNil(GraphicsCommandPool);
      end;
     finally
      GraphicsQueue:=nil;
     end;
    finally
     fUploaded:=true;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.TImage.Unload;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     FreeAndNil(fTexture);
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

function TpvScene3D.TImage.GetHashData:THashData;
begin
 FillChar(result.MessageDigest,SizeOf(TpvHashSHA3.TMessageDigest),#0);
 if fResourceDataStream.Size>0 then begin
  TpvHashSHA3.Process(fResourceDataStream.Memory,fResourceDataStream.Size,@result.MessageDigest,SizeOf(TpvHashSHA3.TMessageDigest));
 end;
end;

procedure TpvScene3D.TImage.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceImage:TPasGLTF.TImage);
begin
 fName:=aSourceImage.Name;
 fResourceDataStream.Clear;
 aSourceImage.GetResourceData(fResourceDataStream);
end;

{ TpvScene3D.TSampler }

constructor TpvScene3D.TSampler.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
 fLock:=TPasMPSpinLock.Create;
end;

destructor TpvScene3D.TSampler.Destroy;
begin
 Unload;
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvScene3D.TSampler.AfterConstruction;
begin
 inherited AfterConstruction;
 try
  fSceneInstance.fSamplerListLock.Acquire;
  try
   fSceneInstance.fSamplers.Add(self);
  finally
   fSceneInstance.fSamplerListLock.Release;
  end;
 finally
  fAdded:=true;
 end;
end;

procedure TpvScene3D.TSampler.BeforeDestruction;
begin
 Remove;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TSampler.Remove;
var HashData:THashData;
begin
 if fAdded then begin
  try
   HashData:=GetHashData;
   fSceneInstance.fSamplerListLock.Acquire;
   try
    fSceneInstance.fSamplers.Remove(self);
    if fSceneInstance.fSamplerHashMap[HashData]=self then begin
     fSceneInstance.fSamplerHashMap.Delete(HashData);
    end;
   finally
    fSceneInstance.fSamplerListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

function TpvScene3D.TSampler.GetHashData:THashData;
begin
 result.MinFilter:=fMinFilter;
 result.MagFilter:=fMagFilter;
 result.MipmapMode:=fMipmapMode;
 result.AddressModeS:=fAddressModeS;
 result.AddressModeT:=fAddressModeT;
end;

procedure TpvScene3D.TSampler.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSampler:TPasGLTF.TSampler);
begin
 fName:=aSourceSampler.Name;
 case aSourceSampler.MinFilter of
  TPasGLTF.TSampler.TMinFilter.None:begin
   fMinFilter:=VK_FILTER_NEAREST;
   fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  end;
  TPasGLTF.TSampler.TMinFilter.Nearest:begin
   fMinFilter:=VK_FILTER_NEAREST;
   fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  end;
  TPasGLTF.TSampler.TMinFilter.Linear:begin
   fMinFilter:=VK_FILTER_LINEAR;
   fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  end;
  TPasGLTF.TSampler.TMinFilter.NearestMipMapNearest:begin
   fMinFilter:=VK_FILTER_NEAREST;
   fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  end;
  TPasGLTF.TSampler.TMinFilter.LinearMipMapNearest:begin
   fMinFilter:=VK_FILTER_LINEAR;
   fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
  end;
  TPasGLTF.TSampler.TMinFilter.NearestMipMapLinear:begin
   fMinFilter:=VK_FILTER_NEAREST;
   fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_LINEAR;
  end;
  TPasGLTF.TSampler.TMinFilter.LinearMipMapLinear:begin
   fMinFilter:=VK_FILTER_LINEAR;
   fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_LINEAR;
  end;
  else begin
   Assert(false);
  end;
 end;
 case aSourceSampler.MagFilter of
  TPasGLTF.TSampler.TMagFilter.None:begin
   fMagFilter:=VK_FILTER_NEAREST;
  end;
  TPasGLTF.TSampler.TMagFilter.Nearest:begin
   fMagFilter:=VK_FILTER_NEAREST;
  end;
  TPasGLTF.TSampler.TMagFilter.Linear:begin
   fMagFilter:=VK_FILTER_LINEAR;
  end;
  else begin
   Assert(false);
  end;
 end;
 case aSourceSampler.WrapS of
  TPasGLTF.TSampler.TWrappingMode.Repeat_:begin
   fAddressModeS:=VK_SAMPLER_ADDRESS_MODE_REPEAT;
  end;
  TPasGLTF.TSampler.TWrappingMode.ClampToEdge:begin
   fAddressModeS:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
  end;
  TPasGLTF.TSampler.TWrappingMode.MirroredRepeat:begin
   fAddressModeS:=VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
  end;
  else begin
   Assert(false);
  end;
 end;
 case aSourceSampler.WrapT of
  TPasGLTF.TSampler.TWrappingMode.Repeat_:begin
   fAddressModeT:=VK_SAMPLER_ADDRESS_MODE_REPEAT;
  end;
  TPasGLTF.TSampler.TWrappingMode.ClampToEdge:begin
   fAddressModeT:=VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
  end;
  TPasGLTF.TSampler.TWrappingMode.MirroredRepeat:begin
   fAddressModeT:=VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
  end;
  else begin
   Assert(false);
  end;
 end;
end;

procedure TpvScene3D.TSampler.Upload;
begin
 if (fReferenceCounter>0) and not fUploaded then begin
  fLock.Acquire;
  try
   if (fReferenceCounter>0) and not fUploaded then begin
    try
     fSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                       fMagFilter,
                                       fMinFilter,
                                       fMipmapMode,
                                       fAddressModeS,
                                       fAddressModeT,
                                       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                       0.0,
                                       false,
                                       1.0,
                                       false,
                                       VK_COMPARE_OP_ALWAYS,
                                       0.0,
                                       65535.0,
                                       VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
                                       false);
    finally
     fUploaded:=true;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.TSampler.Unload;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     FreeAndNil(fSampler);
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
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

 Unload;

 if assigned(fImage) then begin
  try
   fImage.DecRef;
  finally
   fImage:=nil;
  end;
 end;

 if assigned(fSampler) then begin
  try
   fSampler.DecRef;
  finally
   fSampler:=nil;
  end;
 end;

 inherited Destroy;
end;

procedure TpvScene3D.TTexture.AfterConstruction;
begin
 inherited AfterConstruction;
 try
  fSceneInstance.fTextureListLock.Acquire;
  try
   fSceneInstance.fTextures.Add(self);
  finally
   fSceneInstance.fTextureListLock.Release;
  end;
 finally
  fAdded:=true;
 end;
end;

procedure TpvScene3D.TTexture.BeforeDestruction;
begin
 Remove;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TTexture.Remove;
var HashData:THashData;
begin
 if fAdded then begin
  try
   HashData:=GetHashData;
   if assigned(fImage) then begin
    try
     fImage.DecRef;
    finally
     fImage:=nil;
    end;
   end;
   if assigned(fSampler) then begin
    try
     fSampler.DecRef;
    finally
     fSampler:=nil;
    end;
   end;
   fSceneInstance.fTextureListLock.Acquire;
   try
    fSceneInstance.fTextures.Remove(self);
    if fSceneInstance.fTextureHashMap[HashData]=self then begin
     fSceneInstance.fTextureHashMap.Delete(HashData);
    end;
   finally
    fSceneInstance.fTextureListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

procedure TpvScene3D.TTexture.Upload;
begin
 if fReferenceCounter>0 then begin
  if assigned(fImage) then begin
   fImage.Upload;
  end;
  if assigned(fSampler) then begin
   fSampler.Upload;
  end;
 end;
end;

procedure TpvScene3D.TTexture.Unload;
begin
end;

function TpvScene3D.TTexture.GetDescriptorImageInfo:TVkDescriptorImageInfo;
begin
 if assigned(fSampler) and assigned(TSampler(fSampler.GetResource).fSampler) then begin
  result.Sampler:=TSampler(fSampler.GetResource).fSampler.Handle;
 end else begin
  result.Sampler:=VK_NULL_HANDLE;
 end;
 if assigned(fImage) and assigned(TImage(fImage.GetResource).fTexture.ImageView) then begin
  result.ImageView:=TImage(fImage.GetResource).fTexture.ImageView.Handle;
 end else begin
  result.ImageView:=VK_NULL_HANDLE;
 end;
 result.ImageLayout:=TImage(fImage.GetResource).fTexture.ImageLayout;
end;

function TpvScene3D.TTexture.GetHashData:THashData;
begin
 if assigned(fImage) then begin
  result.Image:=TImage(fImage.GetResource);
 end else begin
  result.Image:=nil;
 end;
 if assigned(fSampler) then begin
  result.Sampler:=TSampler(fSampler.GetResource);
 end else begin
  result.Sampler:=nil;
 end;
end;

procedure TpvScene3D.TTexture.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceTexture:TPasGLTF.TTexture;const aImageMap:TImages;const aSamplerMap:TSamplers);
begin
 fName:=aSourceTexture.Name;
 if (aSourceTexture.Source>=0) and (aSourceTexture.Source<aImageMap.Count) then begin
  fImage:=aImageMap[aSourceTexture.Source];
  if assigned(fImage) then begin
   fImage.IncRef;
  end;
 end else begin
  raise EPasGLTFInvalidDocument.Create('Image index out of range');
 end;
 if (aSourceTexture.Sampler>=0) and (aSourceTexture.Sampler<aSamplerMap.Count) then begin
  fSampler:=aSamplerMap[aSourceTexture.Sampler];
  if assigned(fSampler) then begin
   fSampler.IncRef;
  end;
 end else begin
  raise EPasGLTFInvalidDocument.Create('Sampler index out of range');
 end;
end;

{ TpvScene3D.TMaterial.TTextureReference }

procedure TpvScene3D.TMaterial.TTextureReference.TTransform.AssignFromGLTF(var aTextureReference:TTextureReference;const aExtensionsItem:TPasJSONItem);
var JSONItem:TPasJSONItem;
    JSONObject:TPasJSONItemObject;
begin
 Active:=false;
 Offset[0]:=0.0;
 Offset[1]:=0.0;
 Rotation:=0.0;
 Scale[0]:=1.0;
 Scale[1]:=1.0;
 if assigned(aExtensionsItem) and (aExtensionsItem is TPasJSONItemObject) then begin
  JSONItem:=TPasJSONItemObject(aExtensionsItem).Properties['KHR_texture_transform'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   Active:=true;
   aTextureReference.TexCoord:=TPasJSON.GetInt64(JSONObject.Properties['texCoord'],aTextureReference.TexCoord);
   JSONItem:=JSONObject.Properties['offset'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=2) then begin
    Offset[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],Offset[0]);
    Offset[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],Offset[1]);
   end;
   Rotation:=TPasJSON.GetNumber(JSONObject.Properties['rotation'],Rotation);
   JSONItem:=JSONObject.Properties['scale'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=2) then begin
    Scale[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],Scale[0]);
    Scale[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],Scale[1]);
   end;
  end;
 end;
end;

function TpvScene3D.TMaterial.TTextureReference.TTransform.ToMatrix4x4:TpvMatrix4x4;
begin
 if Active then begin
  result:=((TpvMatrix4x4.CreateRotateZ(-Rotation)*
            TpvMatrix4x4.CreateScale(Scale[0],Scale[1],1.0)))*
          TpvMatrix4x4.CreateTranslation(Offset[0],Offset[1],0.0);
 end else begin
  result:=TpvMatrix4x4.Identity;
 end;
end;

{ TpvScene3D.TMaterial }

constructor TpvScene3D.TMaterial.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fData:=DefaultData;

 fLock:=TPasMPSpinLock.Create;

end;

destructor TpvScene3D.TMaterial.Destroy;
begin
 if assigned(fData.EmissiveTexture.Texture) then begin
  try
   fData.EmissiveTexture.Texture.DecRef;
  finally
   fData.EmissiveTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.NormalTexture.Texture) then begin
  try
   fData.NormalTexture.Texture.DecRef;
  finally
   fData.NormalTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.OcclusionTexture.Texture) then begin
  try
   fData.OcclusionTexture.Texture.DecRef;
  finally
   fData.OcclusionTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
  try
   fData.PBRMetallicRoughness.BaseColorTexture.Texture.DecRef;
  finally
   fData.PBRMetallicRoughness.BaseColorTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
  try
   fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.DecRef;
  finally
   fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
  try
   fData.PBRSpecularGlossiness.DiffuseTexture.Texture.DecRef;
  finally
   fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
  try
   fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.DecRef;
  finally
   fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRSheen.ColorIntensityTexture.Texture) then begin
  try
   fData.PBRSheen.ColorIntensityTexture.Texture.DecRef;
  finally
   fData.PBRSheen.ColorIntensityTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRClearCoat.Texture.Texture) then begin
  try
   fData.PBRClearCoat.Texture.Texture.DecRef;
  finally
   fData.PBRClearCoat.Texture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
  try
   fData.PBRClearCoat.RoughnessTexture.Texture.DecRef;
  finally
   fData.PBRClearCoat.RoughnessTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
  try
   fData.PBRClearCoat.NormalTexture.Texture.DecRef;
  finally
   fData.PBRClearCoat.NormalTexture.Texture:=nil;
  end;
 end;
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvScene3D.TMaterial.AfterConstruction;
begin
 inherited AfterConstruction;
 try
  fSceneInstance.fMaterialListLock.Acquire;
  try
   fSceneInstance.fMaterials.Add(self);
  finally
   fSceneInstance.fMaterialListLock.Release;
  end;
 finally
  fAdded:=true;
 end;
end;

procedure TpvScene3D.TMaterial.BeforeDestruction;
begin
 Remove;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TMaterial.Remove;
begin
 if fAdded then begin
  try
   if assigned(fData.EmissiveTexture.Texture) then begin
    try
     fData.EmissiveTexture.Texture.DecRef;
    finally
     fData.EmissiveTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.NormalTexture.Texture) then begin
    try
     fData.NormalTexture.Texture.DecRef;
    finally
     fData.NormalTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.OcclusionTexture.Texture) then begin
    try
     fData.OcclusionTexture.Texture.DecRef;
    finally
     fData.OcclusionTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    try
     fData.PBRMetallicRoughness.BaseColorTexture.Texture.DecRef;
    finally
     fData.PBRMetallicRoughness.BaseColorTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    try
     fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.DecRef;
    finally
     fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
    try
     fData.PBRSpecularGlossiness.DiffuseTexture.Texture.DecRef;
    finally
     fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
    try
     fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.DecRef;
    finally
     fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRSheen.ColorIntensityTexture.Texture) then begin
    try
     fData.PBRSheen.ColorIntensityTexture.Texture.DecRef;
    finally
     fData.PBRSheen.ColorIntensityTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRClearCoat.Texture.Texture) then begin
    try
     fData.PBRClearCoat.Texture.Texture.DecRef;
    finally
     fData.PBRClearCoat.Texture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
    try
     fData.PBRClearCoat.RoughnessTexture.Texture.DecRef;
    finally
     fData.PBRClearCoat.RoughnessTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
    try
     fData.PBRClearCoat.NormalTexture.Texture.DecRef;
    finally
     fData.PBRClearCoat.NormalTexture.Texture:=nil;
    end;
   end;
   fSceneInstance.fMaterialListLock.Acquire;
   try
    fSceneInstance.fMaterials.Remove(self);
    if fSceneInstance.fMaterialHashMap[fData]=self then begin
     fSceneInstance.fMaterialHashMap.Delete(fData);
    end;
   finally
    fSceneInstance.fMaterialListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

procedure TpvScene3D.TMaterial.Upload;
var UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
    NormalTextureDescriptorImageInfo,
    OcclusionTextureDescriptorImageInfo,
    EmissiveTextureDescriptorImageInfo,
    BaseColorOrDiffuseTextureDescriptorImageInfo,
    MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo,
    SheenColorIntensityTextureDescriptorImageInfo,
    ClearCoatNormalTextureDescriptorImageInfo,
    ClearCoatRoughnessTextureDescriptorImageInfo,
    ClearCoatTextureDescriptorImageInfo:TVkDescriptorImageInfo;
begin

 if (fReferenceCounter>0) and not fUploaded then begin

  fLock.Acquire;
  try

   if (fReferenceCounter>0) and not fUploaded then begin

    try

     fSceneInstance.UploadWhiteTexture;
     fSceneInstance.UploadDefaultNormalMapTexture;

     if assigned(fData.NormalTexture.Texture) then begin
      fData.NormalTexture.Texture.Upload;
      NormalTextureDescriptorImageInfo:=fData.NormalTexture.Texture.GetDescriptorImageInfo;
     end else begin
      NormalTextureDescriptorImageInfo:=fSceneInstance.fDefaultNormalMapTexture.DescriptorImageInfo;
     end;

     if assigned(fData.OcclusionTexture.Texture) then begin
      fData.OcclusionTexture.Texture.Upload;
      OcclusionTextureDescriptorImageInfo:=fData.OcclusionTexture.Texture.GetDescriptorImageInfo;
     end else begin
      OcclusionTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
     end;

     if assigned(fData.EmissiveTexture.Texture) then begin
      fData.EmissiveTexture.Texture.Upload;
      EmissiveTextureDescriptorImageInfo:=fData.EmissiveTexture.Texture.GetDescriptorImageInfo;
     end else begin
      EmissiveTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
     end;

     case fData.ShadingModel of
      TpvScene3D.TMaterial.TShadingModel.PBRMetallicRoughness:begin
       if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
        fData.PBRMetallicRoughness.BaseColorTexture.Texture.Upload;
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fData.PBRMetallicRoughness.BaseColorTexture.Texture.GetDescriptorImageInfo;
       end else begin
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       end;
       if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
        fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.Upload;
        MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.GetDescriptorImageInfo;
       end else begin
        MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       end;
      end;
      TpvScene3D.TMaterial.TShadingModel.PBRSpecularGlossiness:begin
       if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
        fData.PBRSpecularGlossiness.DiffuseTexture.Texture.Upload;
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fData.PBRSpecularGlossiness.DiffuseTexture.Texture.GetDescriptorImageInfo;
       end else begin
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       end;
       if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
        fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.Upload;
        MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.GetDescriptorImageInfo;
       end else begin
        MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       end;
      end;
      TpvScene3D.TMaterial.TShadingModel.Unlit:begin
       if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
        fData.PBRMetallicRoughness.BaseColorTexture.Texture.Upload;
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fData.PBRMetallicRoughness.BaseColorTexture.Texture.GetDescriptorImageInfo;
       end else begin
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       end;
       MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
      end;
      else begin
       BaseColorOrDiffuseTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
      end;
     end;

     if assigned(fData.PBRSheen.ColorIntensityTexture.Texture) then begin
      fData.PBRSheen.ColorIntensityTexture.Texture.Upload;
      SheenColorIntensityTextureDescriptorImageInfo:=fData.PBRSheen.ColorIntensityTexture.Texture.GetDescriptorImageInfo;
     end else begin
      SheenColorIntensityTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
     end;

     if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
      fData.PBRClearCoat.NormalTexture.Texture.Upload;
      ClearCoatNormalTextureDescriptorImageInfo:=fData.PBRClearCoat.NormalTexture.Texture.GetDescriptorImageInfo;
     end else begin
      ClearCoatNormalTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
     end;

     if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
      fData.PBRClearCoat.RoughnessTexture.Texture.Upload;
      ClearCoatRoughnessTextureDescriptorImageInfo:=fData.PBRClearCoat.RoughnessTexture.Texture.GetDescriptorImageInfo;
     end else begin
      ClearCoatRoughnessTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
     end;

     if assigned(fData.PBRClearCoat.Texture.Texture) then begin
      fData.PBRClearCoat.Texture.Texture.Upload;
      ClearCoatTextureDescriptorImageInfo:=fData.PBRClearCoat.Texture.Texture.GetDescriptorImageInfo;
     end else begin
      ClearCoatTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
     end;

     fShaderDataUniformBlockBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                           SizeOf(TShaderData),
                                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                           [],
                                                           0,
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                           0,
                                                           0,
                                                           0,
                                                           0,
                                                           []);
     UniversalQueue:=pvApplication.VulkanDevice.UniversalQueue;
     try
      UniversalCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                        pvApplication.VulkanDevice.UniversalQueueFamilyIndex,
                                                        TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
      try
       UniversalCommandBuffer:=TpvVulkanCommandBuffer.Create(UniversalCommandPool,
                                                             VK_COMMAND_BUFFER_LEVEL_PRIMARY);
       try
        UniversalFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
        try
         FillShaderData;
         fShaderDataUniformBlockBuffer.UploadData(UniversalQueue,
                                                  UniversalCommandBuffer,
                                                  UniversalFence,
                                                  fShaderData,
                                                  0,
                                                  SizeOf(TShaderData),
                                                  TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);
        finally
         FreeAndNil(UniversalFence);
        end;
       finally
        FreeAndNil(UniversalCommandBuffer);
       end;
      finally
       FreeAndNil(UniversalCommandPool);
      end;
     finally
      UniversalQueue:=nil;
     end;

     fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),2);
     fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,1);
     fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,9);
     fVulkanDescriptorPool.Initialize;
     fVulkanDescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                         fSceneInstance.fMaterialVulkanDescriptorSetLayout);
     fVulkanDescriptorSet.WriteToDescriptorSet(0,
                                               0,
                                               1,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                               [],
                                               [fShaderDataUniformBlockBuffer.DescriptorBufferInfo],
                                               [],
                                               false);
     fVulkanDescriptorSet.WriteToDescriptorSet(1,
                                               0,
                                               9,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                               [NormalTextureDescriptorImageInfo,
                                                OcclusionTextureDescriptorImageInfo,
                                                EmissiveTextureDescriptorImageInfo,
                                                BaseColorOrDiffuseTextureDescriptorImageInfo,
                                                MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo,
                                                SheenColorIntensityTextureDescriptorImageInfo,
                                                ClearCoatNormalTextureDescriptorImageInfo,
                                                ClearCoatRoughnessTextureDescriptorImageInfo,
                                                ClearCoatTextureDescriptorImageInfo],
                                               [],
                                               [],
                                               false);
     fVulkanDescriptorSet.Flush;

    finally
     fUploaded:=true;
    end;

   end;

  finally
   fLock.Release;
  end;

 end;
end;

procedure TpvScene3D.TMaterial.Unload;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     FreeAndNil(fVulkanDescriptorSet);
     FreeAndNil(fVulkanDescriptorPool);
     FreeAndNil(fShaderDataUniformBlockBuffer);
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.TMaterial.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMaterial:TPasGLTF.TMaterial;const aTextureMap:TTextures);
var Index:TpvSizeInt;
    JSONItem:TPasJSONItem;
    JSONObject:TPasJSONItemObject;
begin

 fName:=aSourceMaterial.Name;

 begin
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
  if (aSourceMaterial.EmissiveTexture.Index>=0) and (aSourceMaterial.EmissiveTexture.Index<aTextureMap.Count) then begin
   fData.EmissiveTexture.Texture:=aTextureMap[aSourceMaterial.EmissiveTexture.Index];
   if assigned(fData.EmissiveTexture.Texture) then begin
    fData.EmissiveTexture.Texture.IncRef;
   end;
  end else begin
   fData.EmissiveTexture.Texture:=nil;
  end;
  fData.EmissiveTexture.TexCoord:=aSourceMaterial.EmissiveTexture.TexCoord;
  fData.EmissiveTexture.Transform.AssignFromGLTF(fData.EmissiveTexture,aSourceMaterial.EmissiveTexture.Extensions);
  if (aSourceMaterial.NormalTexture.Index>=0) and (aSourceMaterial.NormalTexture.Index<aTextureMap.Count) then begin
   fData.NormalTexture.Texture:=aTextureMap[aSourceMaterial.NormalTexture.Index];
   if assigned(fData.NormalTexture.Texture) then begin
    fData.NormalTexture.Texture.IncRef;
   end;
  end else begin
   fData.NormalTexture.Texture:=nil;
  end;
  fData.NormalTexture.TexCoord:=aSourceMaterial.NormalTexture.TexCoord;
  fData.NormalTexture.Transform.AssignFromGLTF(fData.NormalTexture,aSourceMaterial.NormalTexture.Extensions);
  fData.NormalTextureScale:=aSourceMaterial.NormalTexture.Scale;
  if (aSourceMaterial.OcclusionTexture.Index>=0) and (aSourceMaterial.OcclusionTexture.Index<aTextureMap.Count) then begin
   fData.OcclusionTexture.Texture:=aTextureMap[aSourceMaterial.OcclusionTexture.Index];
   if assigned(fData.OcclusionTexture.Texture) then begin
    fData.OcclusionTexture.Texture.IncRef;
   end;
  end else begin
   fData.OcclusionTexture.Texture:=nil;
  end;
  fData.OcclusionTexture.TexCoord:=aSourceMaterial.OcclusionTexture.TexCoord;
  fData.OcclusionTexture.Transform.AssignFromGLTF(fData.OcclusionTexture,aSourceMaterial.OcclusionTexture.Extensions);
  fData.OcclusionTextureStrength:=aSourceMaterial.OcclusionTexture.Strength;
 end;

 begin
  fData.PBRMetallicRoughness.BaseColorFactor:=TpvVector4.InlineableCreate(aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[0],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[1],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[2],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[3]);
  if (aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index>=0) and (aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index<aTextureMap.Count) then begin
   fData.PBRMetallicRoughness.BaseColorTexture.Texture:=aTextureMap[aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index];
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    fData.PBRMetallicRoughness.BaseColorTexture.Texture.IncRef;
   end;
  end else begin
   fData.PBRMetallicRoughness.BaseColorTexture.Texture:=nil;
  end;
  fData.PBRMetallicRoughness.BaseColorTexture.TexCoord:=aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.TexCoord;
  fData.PBRMetallicRoughness.BaseColorTexture.Transform.AssignFromGLTF(fData.PBRMetallicRoughness.BaseColorTexture,aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Extensions);
  fData.PBRMetallicRoughness.RoughnessFactor:=aSourceMaterial.PBRMetallicRoughness.RoughnessFactor;
  fData.PBRMetallicRoughness.MetallicFactor:=aSourceMaterial.PBRMetallicRoughness.MetallicFactor;
  if (aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index>=0) and (aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index<aTextureMap.Count) then begin
   fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture:=aTextureMap[aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index];
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.IncRef;
   end;
  end else begin
   fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture:=nil;
  end;
  fData.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord:=aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord;
  fData.PBRMetallicRoughness.MetallicRoughnessTexture.Transform.AssignFromGLTF(fData.PBRMetallicRoughness.MetallicRoughnessTexture,aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Extensions);
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
     if (Index>=0) and (Index<aTextureMap.Count) then begin
      fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=aTextureMap[Index];
      if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
       fData.PBRSpecularGlossiness.DiffuseTexture.Texture.IncRef;
      end;
     end else begin
      fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=nil;
     end;
     fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord);
     fData.PBRSpecularGlossiness.DiffuseTexture.Transform.AssignFromGLTF(fData.PBRSpecularGlossiness.DiffuseTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
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
     if (Index>=0) and (Index<aTextureMap.Count) then begin
      fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=aTextureMap[Index];
      if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
       fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.IncRef;
      end;
     end else begin
      fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=nil;
     end;
     fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord);
     fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Transform.AssignFromGLTF(fData.PBRSpecularGlossiness.SpecularGlossinessTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
    end;
   end;
  end else begin
   fData.ShadingModel:=TMaterial.TShadingModel.PBRMetallicRoughness;
  end;
 end;

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_sheen'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fData.PBRSheen.Active:=true;
   fData.PBRSheen.IntensityFactor:=TPasJSON.GetNumber(JSONObject.Properties['intensityFactor'],TPasJSON.GetNumber(JSONObject.Properties['sheenFactor'],1.0));
   JSONItem:=JSONObject.Properties['colorFactor'];
   if not assigned(JSONItem) then begin
    JSONItem:=JSONObject.Properties['sheenColor'];
   end;
   if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3) then begin
    fData.PBRSheen.ColorFactor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],1.0);
    fData.PBRSheen.ColorFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],1.0);
    fData.PBRSheen.ColorFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],1.0);
   end;
   JSONItem:=JSONObject.Properties['colorIntensityTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.PBRSheen.ColorIntensityTexture.Texture:=aTextureMap[Index];
     if assigned(fData.PBRSheen.ColorIntensityTexture.Texture) then begin
      fData.PBRSheen.ColorIntensityTexture.Texture.IncRef;
     end;
    end else begin
     fData.PBRSheen.ColorIntensityTexture.Texture:=nil;
    end;
    fData.PBRSheen.ColorIntensityTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.PBRSheen.ColorIntensityTexture.Transform.AssignFromGLTF(fData.PBRSheen.ColorIntensityTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
  end;
 end;

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_clearcoat'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fData.PBRClearCoat.Active:=true;
   fData.PBRClearCoat.Factor:=TPasJSON.GetNumber(JSONObject.Properties['intensityFactor'],TPasJSON.GetNumber(JSONObject.Properties['clearcoatFactor'],fData.PBRClearCoat.Factor));
   JSONItem:=JSONObject.Properties['clearcoatTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.PBRClearCoat.Texture.Texture:=aTextureMap[Index];
     if assigned(fData.PBRClearCoat.Texture.Texture) then begin
      fData.PBRClearCoat.Texture.Texture.IncRef;
     end;
    end else begin
     fData.PBRClearCoat.Texture.Texture:=nil;
    end;
    fData.PBRClearCoat.Texture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.PBRClearCoat.Texture.Transform.AssignFromGLTF(fData.PBRClearCoat.Texture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
   fData.PBRClearCoat.RoughnessFactor:=TPasJSON.GetNumber(JSONObject.Properties['intensityFactor'],TPasJSON.GetNumber(JSONObject.Properties['clearcoatRoughnessFactor'],fData.PBRClearCoat.RoughnessFactor));
   JSONItem:=JSONObject.Properties['clearcoatRoughnessTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.PBRClearCoat.RoughnessTexture.Texture:=aTextureMap[Index];
     if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
      fData.PBRClearCoat.RoughnessTexture.Texture.IncRef;
     end;
    end else begin
     fData.PBRClearCoat.RoughnessTexture.Texture:=nil;
    end;
    fData.PBRClearCoat.RoughnessTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.PBRClearCoat.RoughnessTexture.Transform.AssignFromGLTF(fData.PBRClearCoat.RoughnessTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
   JSONItem:=JSONObject.Properties['clearcoatNormalTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.PBRClearCoat.NormalTexture.Texture:=aTextureMap[Index];
     if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
      fData.PBRClearCoat.NormalTexture.Texture.IncRef;
     end;
    end else begin
     fData.PBRClearCoat.NormalTexture.Texture:=nil;
    end;
    fData.PBRClearCoat.NormalTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.PBRClearCoat.NormalTexture.Transform.AssignFromGLTF(fData.PBRClearCoat.NormalTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
  end;
 end;

 FillShaderData;

end;

procedure TpvScene3D.TMaterial.FillShaderData;
begin

 fShaderData:=DefaultShaderData;

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
 fShaderData.Textures0:=$ffffffff;
 fShaderData.Textures1:=$ffffffff;
 case fData.ShadingModel of
  TMaterial.TShadingModel.PBRMetallicRoughness:begin
   fShaderData.Flags:=fShaderData.Flags or ((0 and $f) shl 0);
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (0 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
    fShaderData.TextureTransforms[0]:=fData.PBRMetallicRoughness.BaseColorTexture.Transform.ToMatrix4x4;
   end;
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (1 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord and $f) shl (1 shl 2));
    fShaderData.TextureTransforms[1]:=fData.PBRMetallicRoughness.MetallicRoughnessTexture.Transform.ToMatrix4x4;
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
    fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (0 shl 2))) or (TpvUInt32(fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord and $f) shl (0 shl 2));
    fShaderData.TextureTransforms[0]:=fData.PBRSpecularGlossiness.DiffuseTexture.Transform.ToMatrix4x4;
   end;
   if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
    fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (1 shl 2))) or (TpvUInt32(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord and $f) shl (1 shl 2));
    fShaderData.TextureTransforms[1]:=fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Transform.ToMatrix4x4;
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
    fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (0 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
    fShaderData.TextureTransforms[0]:=fData.PBRMetallicRoughness.BaseColorTexture.Transform.ToMatrix4x4;
   end;
   fShaderData.BaseColorFactor:=TpvVector4.InlineableCreate(fData.PBRMetallicRoughness.BaseColorFactor[0],fData.PBRMetallicRoughness.BaseColorFactor[1],fData.PBRMetallicRoughness.BaseColorFactor[2],fData.PBRMetallicRoughness.BaseColorFactor[3]);
  end;
  else begin
   Assert(false);
  end;
 end;
 if assigned(fData.NormalTexture.Texture) then begin
  fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (2 shl 2))) or (TpvUInt32(fData.NormalTexture.TexCoord and $f) shl (2 shl 2));
  fShaderData.TextureTransforms[2]:=fData.NormalTexture.Transform.ToMatrix4x4;
 end;
 if assigned(fData.OcclusionTexture.Texture) then begin
  fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (3 shl 2))) or (TpvUInt32(fData.OcclusionTexture.TexCoord and $f) shl (3 shl 2));
  fShaderData.TextureTransforms[3]:=fData.OcclusionTexture.Transform.ToMatrix4x4;
 end;
 if assigned(fData.EmissiveTexture.Texture) then begin
  fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (4 shl 2))) or (TpvUInt32(fData.EmissiveTexture.TexCoord and $f) shl (4 shl 2));
  fShaderData.TextureTransforms[4]:=fData.EmissiveTexture.Transform.ToMatrix4x4;
 end;
 fShaderData.EmissiveFactor[0]:=fData.EmissiveFactor[0];
 fShaderData.EmissiveFactor[1]:=fData.EmissiveFactor[1];
 fShaderData.EmissiveFactor[2]:=fData.EmissiveFactor[2];
 fShaderData.EmissiveFactor[3]:=0.0;

 if fData.PBRSheen.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 7);
  fShaderData.SheenColorFactorSheenIntensityFactor[0]:=fData.PBRSheen.ColorFactor[0];
  fShaderData.SheenColorFactorSheenIntensityFactor[1]:=fData.PBRSheen.ColorFactor[1];
  fShaderData.SheenColorFactorSheenIntensityFactor[2]:=fData.PBRSheen.ColorFactor[2];
  fShaderData.SheenColorFactorSheenIntensityFactor[3]:=fData.PBRSheen.IntensityFactor;
  if assigned(fData.PBRSheen.ColorIntensityTexture.Texture) then begin
   fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (5 shl 2))) or ((fData.PBRSheen.ColorIntensityTexture.TexCoord and $f) shl (5 shl 2));
   fShaderData.TextureTransforms[5]:=fData.PBRSheen.ColorIntensityTexture.Transform.ToMatrix4x4;
  end;
 end;

 if fData.PBRClearCoat.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 8);
  fShaderData.ClearcoatFactorClearcoatRoughnessFactor[0]:=fData.PBRClearCoat.Factor;
  fShaderData.ClearcoatFactorClearcoatRoughnessFactor[1]:=fData.PBRClearCoat.RoughnessFactor;
  if assigned(fData.PBRClearCoat.Texture.Texture) then begin
   fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (6 shl 2))) or ((fData.PBRClearCoat.Texture.TexCoord and $f) shl (6 shl 2));
   fShaderData.TextureTransforms[6]:=fData.PBRClearCoat.Texture.Transform.ToMatrix4x4;
  end;
  if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
   fShaderData.Textures0:=(fShaderData.Textures0 and not ($f shl (7 shl 2))) or ((fData.PBRClearCoat.RoughnessTexture.TexCoord and $f) shl (7 shl 2));
   fShaderData.TextureTransforms[7]:=fData.PBRClearCoat.RoughnessTexture.Transform.ToMatrix4x4;
  end;
  if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
   fShaderData.Textures1:=(fShaderData.Textures1 and not ($f shl (0 shl 2))) or ((fData.PBRClearCoat.NormalTexture.TexCoord and $f) shl (0 shl 2));
   fShaderData.TextureTransforms[8]:=fData.PBRClearCoat.NormalTexture.Transform.ToMatrix4x4;
  end;
 end;

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

constructor TpvScene3D.TGroup.TAnimation.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
end;

destructor TpvScene3D.TGroup.TAnimation.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TAnimation.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceAnimation:TPasGLTF.TAnimation);
var Index,ChannelIndex,ValueIndex:TPasGLTFSizeInt;
    SourceAnimationChannel:TPasGLTF.TAnimation.TChannel;
    SourceAnimationSampler:TPasGLTF.TAnimation.TSampler;
    DestinationAnimationChannel:TAnimation.PChannel;
    OutputVector3Array:TPasGLTF.TVector3DynamicArray;
    OutputVector4Array:TPasGLTF.TVector4DynamicArray;
    OutputScalarArray:TPasGLTFFloatDynamicArray;
begin

 fName:=aSourceAnimation.Name;

 SetLength(fChannels,aSourceAnimation.Channels.Count);

 for ChannelIndex:=0 to aSourceAnimation.Channels.Count-1 do begin

  SourceAnimationChannel:=aSourceAnimation.Channels[ChannelIndex];

  DestinationAnimationChannel:=@fChannels[ChannelIndex];

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

  if (SourceAnimationChannel.Sampler>=0) and (SourceAnimationChannel.Sampler<aSourceAnimation.Samplers.Count) then begin
   SourceAnimationSampler:=aSourceAnimation.Samplers[SourceAnimationChannel.Sampler];
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

function TpvScene3D.TGroup.TAnimation.GetAnimationBeginTime:TpvFloat;
var Index:TpvSizeInt;
    Channel:TAnimation.PChannel;
begin
 result:=0.0;
 for Index:=0 to length(fChannels)-1 do begin
  Channel:=@fChannels[Index];
  if length(Channel^.InputTimeArray)>0 then begin
   if Index=0 then begin
    result:=Channel^.InputTimeArray[0];
   end else begin
    result:=Min(result,Channel^.InputTimeArray[0]);
   end;
  end;
 end;
end;

function TpvScene3D.TGroup.TAnimation.GetAnimationEndTime:TpvFloat;
var Index:TpvSizeInt;
    Channel:TAnimation.PChannel;
begin
 result:=1.0;
 for Index:=0 to length(fChannels)-1 do begin
  Channel:=@fChannels[Index];
  if length(Channel^.InputTimeArray)>0 then begin
   if Index=0 then begin
    result:=Channel^.InputTimeArray[length(Channel^.InputTimeArray)-1];
   end else begin
    result:=Max(result,Channel^.InputTimeArray[0]);
   end;
  end;
 end;
end;

{ TpvScene3D.TGroup.TCamera }

constructor TpvScene3D.TGroup.TCamera.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
end;

destructor TpvScene3D.TGroup.TCamera.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TCamera.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceCamera:TPasGLTF.TCamera);
begin

 fName:=aSourceCamera.Name;

 case aSourceCamera.Type_ of
  TPasGLTF.TCamera.TType.None:begin
   fType:=TType.None;
  end;
  TPasGLTF.TCamera.TType.Orthographic:begin
   fType:=TType.Orthographic;
   fOrthographic.XMag:=aSourceCamera.Orthographic.XMag;
   fOrthographic.YMag:=aSourceCamera.Orthographic.YMag;
   fOrthographic.ZNear:=aSourceCamera.Orthographic.ZNear;
   fOrthographic.ZFar:=aSourceCamera.Orthographic.ZFar;
  end;
  TPasGLTF.TCamera.TType.Perspective:begin
   fType:=TType.Perspective;
   fPerspective.AspectRatio:=aSourceCamera.Perspective.AspectRatio;
   fPerspective.YFoV:=aSourceCamera.Perspective.YFoV;
   fPerspective.ZNear:=aSourceCamera.Perspective.ZNear;
   fPerspective.ZFar:=aSourceCamera.Perspective.ZFar;
  end;
  else begin
   Assert(false);
  end;
 end;

end;

{ TpvScene3D.TGroup.TMesh }

constructor TpvScene3D.TGroup.TMesh.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
 fPrimitives:=nil;
 fNodeMeshInstances:=0;
end;

destructor TpvScene3D.TGroup.TMesh.Destroy;
var Index:TpvSizeInt;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
begin
 for Index:=0 to length(fPrimitives)-1 do begin
  Primitive:=@fPrimitives[Index];
  if assigned(Primitive^.Material) then begin
   try
    Primitive^.Material.DecRef;
   finally
    Primitive^.Material:=nil;
   end;
  end;
 end;
 fPrimitives:=nil;
 inherited Destroy;
end;

function TpvScene3D.TGroup.TMesh.CreateNodeMeshInstance(const aNodeIndex:TpvUInt32):TpvSizeInt;
var PrimitiveIndex,
    NodeMeshPrimitiveInstanceIndex,
    VertexIndex,
    NewVertexIndex,
    IndexIndex,
    NewMorphTargetVertexIndex,
    JointBlockIndex,
    NewJointBlockIndex:TpvSizeInt;
    Primitive:TMesh.PPrimitive;
    NodeMeshPrimitiveInstance:TMesh.TPrimitive.PNodeMeshPrimitiveInstance;
    Vertex:PVertex;
    MorphTargetVertex:PMorphTargetVertex;
    MorphTargetVertexIndex:TpvUInt32;
begin

 result:=fNodeMeshInstances;
 inc(fNodeMeshInstances);

 if result=0 then begin

  for PrimitiveIndex:=0 to length(fPrimitives)-1 do begin
   Primitive:=@fPrimitives[PrimitiveIndex];
   NodeMeshPrimitiveInstanceIndex:=Primitive^.NodeMeshPrimitiveInstances.AddNew;
   NodeMeshPrimitiveInstance:=@Primitive^.NodeMeshPrimitiveInstances.Items[NodeMeshPrimitiveInstanceIndex];
   NodeMeshPrimitiveInstance^.MorphTargetBaseIndex:=Primitive^.MorphTargetBaseIndex;
   NodeMeshPrimitiveInstance^.StartBufferVertexOffset:=Primitive^.StartBufferVertexOffset;
   NodeMeshPrimitiveInstance^.StartBufferIndexOffset:=Primitive^.StartBufferIndexOffset;
   for VertexIndex:=TpvSizeInt(Primitive^.StartBufferVertexOffset) to TpvSizeInt(Primitive^.StartBufferVertexOffset+Primitive^.CountVertices)-1 do begin
    Vertex:=@fGroup.fVertices.Items[VertexIndex];
    Vertex^.NodeIndex:=aNodeIndex;
   end;
  end;

 end else begin

  for PrimitiveIndex:=0 to length(fPrimitives)-1 do begin

   Primitive:=@fPrimitives[PrimitiveIndex];

   NodeMeshPrimitiveInstanceIndex:=Primitive^.NodeMeshPrimitiveInstances.AddNew;
   NodeMeshPrimitiveInstance:=@Primitive^.NodeMeshPrimitiveInstances.Items[NodeMeshPrimitiveInstanceIndex];

   NodeMeshPrimitiveInstance^.MorphTargetBaseIndex:=fGroup.fMorphTargetCount;
   inc(fGroup.fMorphTargetCount,length(Primitive^.Targets));

   NodeMeshPrimitiveInstance^.StartBufferVertexOffset:=fGroup.fVertices.Count;
   for VertexIndex:=TpvSizeInt(Primitive^.StartBufferVertexOffset) to TpvSizeInt(Primitive^.StartBufferVertexOffset+Primitive^.CountVertices)-1 do begin
    NewVertexIndex:=fGroup.fVertices.Add(fGroup.fVertices.Items[VertexIndex]);
    Vertex:=@fGroup.fVertices.Items[NewVertexIndex];
    Vertex^.NodeIndex:=aNodeIndex;
    if (Vertex^.MorphTargetVertexBaseIndex<>TpvUInt32($ffffffff)) and (Vertex^.CountMorphTargetVertices>0) then begin
     Vertex^.MorphTargetVertexBaseIndex:=fGroup.fMorphTargetVertices.Count;
     MorphTargetVertexIndex:=Vertex^.MorphTargetVertexBaseIndex;
     while MorphTargetVertexIndex<>TpvUInt32($ffffffff) do begin
      NewMorphTargetVertexIndex:=fGroup.fMorphTargetVertices.AddNew;
      fGroup.fMorphTargetVertices.Items[NewMorphTargetVertexIndex]:=fGroup.fMorphTargetVertices.Items[MorphTargetVertexIndex];
      MorphTargetVertex:=@fGroup.fMorphTargetVertices.Items[NewMorphTargetVertexIndex];
      MorphTargetVertex^.Index:=(MorphTargetVertex^.Index-Primitive^.MorphTargetBaseIndex)+NodeMeshPrimitiveInstance^.MorphTargetBaseIndex;
      if MorphTargetVertex^.Next=TpvUInt32($ffffffff) then begin
       break;
      end else begin
       MorphTargetVertexIndex:=MorphTargetVertex^.Next;
       MorphTargetVertex^.Next:=fGroup.fMorphTargetVertices.Count+1;
      end;
     end;
    end else begin
     Vertex^.MorphTargetVertexBaseIndex:=TpvUInt32($ffffffff);
     Vertex^.CountMorphTargetVertices:=0;
    end;
    if (Vertex^.JointBlockBaseIndex<>TpvUInt32($ffffffff)) and (Vertex^.CountJointBlocks>0) then begin
     Vertex^.JointBlockBaseIndex:=fGroup.fJointBlocks.Count;
     for JointBlockIndex:=0 to TpvSizeInt(Vertex^.CountJointBlocks)-1 do begin
      NewJointBlockIndex:=fGroup.fJointBlocks.AddNew;
      fGroup.fJointBlocks.Items[NewJointBlockIndex]:=fGroup.fJointBlocks.Items[Vertex^.JointBlockBaseIndex+JointBlockIndex];
     end;
    end else begin
     Vertex^.JointBlockBaseIndex:=TpvUInt32($ffffffff);
     Vertex^.CountJointBlocks:=0;
    end;

   end;

   NodeMeshPrimitiveInstance^.StartBufferIndexOffset:=fGroup.fIndices.Count;
   for IndexIndex:=TpvSizeInt(Primitive^.StartBufferIndexOffset) to TpvSizeInt(Primitive^.StartBufferIndexOffset+Primitive^.CountIndices)-1 do begin
    fGroup.fIndices.Add((fGroup.fIndices.Items[IndexIndex]-Primitive^.StartBufferVertexOffset)+NodeMeshPrimitiveInstance^.StartBufferVertexOffset);
   end;

  end;

 end;
end;

procedure TpvScene3D.TGroup.TMesh.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMesh:TPasGLTF.TMesh;const aMaterialMap:TpvScene3D.TMaterials);
type TMaxJointBlocksHashMap=TpvHashMap<TMaxJointBlocks,TpvUInt32>;
var Index,
    PrimitiveIndex,
    AccessorIndex,
    IndexIndex,
    VertexIndex,
    TargetIndex,
    WeightIndex,
    JointIndex,
    OtherJointIndex,
    JointBlockIndex,
    MorphTargetVertexIndex,
    CountJointBlocks,
    OldCount,
    MaxCountTargets:TpvSizeInt;
    SourceMeshPrimitive:TPasGLTF.TMesh.TPrimitive;
    SourceMeshPrimitiveTarget:TPasGLTF.TAttributes;
    DestinationMeshPrimitive:TMesh.PPrimitive;
    DestinationMeshPrimitiveTarget:TMesh.TPrimitive.PTarget;
    DestinationMeshPrimitiveTargetVertex:TMesh.TPrimitive.TTarget.PTargetVertex;
    MorphTargetVertex:PMorphTargetVertex;
    TemporaryPositions,
    TemporaryNormals,
    TemporaryBitangents,
    TemporaryTargetTangents:TPasGLTF.TVector3DynamicArray;
    TemporaryTangents,
    TemporaryColor0:TPasGLTF.TVector4DynamicArray;
    TemporaryWeights:array[0..9] of TPasGLTF.TVector4DynamicArray;
    TemporaryJoints:array[0..9] of TPasGLTF.TUInt32Vector4DynamicArray;
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
    HasMorphVertexTargets,
    HasJoints,
    DoNeedCalculateTangents:boolean;
    DestinationMeshPrimitiveVertices:TVertices;
    DestinationMeshPrimitiveIndices:TpvUInt32DynamicArray;
    MaxJointBlocks:PMaxJointBlocks;
    MaxJointBlocksHashMap:TMaxJointBlocksHashMap;
begin

 GetMem(MaxJointBlocks,SizeOf(TMaxJointBlocks));
 try

  FillChar(MaxJointBlocks^,SizeOf(TMaxJointBlocks),#0);

  fName:=aSourceMesh.Name;

  SetLength(fPrimitives,aSourceMesh.Primitives.Count);

  fBoundingBox:=TpvAABB.Create(TpvVector3.InlineableCreate(Infinity,Infinity,Infinity),
                               TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity));

 //DestinationMesh^.JointBlocks:=nil;

  MaxCountTargets:=0;

  for PrimitiveIndex:=0 to aSourceMesh.Primitives.Count-1 do begin

   MaxJointBlocksHashMap:=TMaxJointBlocksHashMap.Create(TpvUInt32($ffffffff));
   try

    DestinationMeshPrimitiveVertices:=nil;
    try

     DestinationMeshPrimitiveIndices:=nil;
     try

      SourceMeshPrimitive:=aSourceMesh.Primitives.Items[PrimitiveIndex];

      DestinationMeshPrimitive:=@fPrimitives[PrimitiveIndex];

      if (SourceMeshPrimitive.Material>=0) and (SourceMeshPrimitive.Material<aMaterialMap.Count) then begin
       DestinationMeshPrimitive^.Material:=aMaterialMap[SourceMeshPrimitive.Material];
       if assigned(DestinationMeshPrimitive^.Material) then begin
        DestinationMeshPrimitive^.Material.IncRef;
       end;
      end else begin
       DestinationMeshPrimitive^.Material:=nil;
      end;

      HasJoints:=false;

      CountJointBlocks:=0;

      begin
       // Load accessor data
       begin
        AccessorIndex:=SourceMeshPrimitive.Attributes['POSITION'];
        if AccessorIndex>=0 then begin
         TemporaryPositions:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
         for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
          fBoundingBox.Min[0]:=Min(fBoundingBox.Min[0],TemporaryPositions[VertexIndex,0]);
          fBoundingBox.Min[1]:=Min(fBoundingBox.Min[1],TemporaryPositions[VertexIndex,1]);
          fBoundingBox.Min[2]:=Min(fBoundingBox.Min[2],TemporaryPositions[VertexIndex,2]);
          fBoundingBox.Max[0]:=Max(fBoundingBox.Max[0],TemporaryPositions[VertexIndex,0]);
          fBoundingBox.Max[1]:=Max(fBoundingBox.Max[1],TemporaryPositions[VertexIndex,1]);
          fBoundingBox.Max[2]:=Max(fBoundingBox.Max[2],TemporaryPositions[VertexIndex,2]);
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
       for JointBlockIndex:=0 to 9 do begin
        begin
         AccessorIndex:=SourceMeshPrimitive.Attributes['JOINTS_'+IntToStr(JointBlockIndex)];
         if AccessorIndex>=0 then begin
          TemporaryJoints[JointBlockIndex]:=aSourceDocument.Accessors[AccessorIndex].DecodeAsUInt32Vector4Array(true);
          HasJoints:=true;
          CountJointBlocks:=Max(CountJointBlocks,JointBlockIndex+1);
         end else begin
          TemporaryJoints[JointBlockIndex]:=nil;
         end;
        end;
        begin
         AccessorIndex:=SourceMeshPrimitive.Attributes['WEIGHTS_'+IntToStr(JointBlockIndex)];
         if AccessorIndex>=0 then begin
          TemporaryWeights[JointBlockIndex]:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector4Array(true);
          HasJoints:=true;
          CountJointBlocks:=Max(CountJointBlocks,JointBlockIndex+1);
         end else begin
          TemporaryWeights[JointBlockIndex]:=nil;
         end;
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
          PpvVector3(pointer(@TemporaryBitangents[VertexIndex]))^:=Bitangent;
          if (PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^.Cross(Tangent)).Dot(Bitangent)<0.0 then begin
           TemporaryTangents[VertexIndex,3]:=-1.0;
          end else begin
           TemporaryTangents[VertexIndex,3]:=1.0;
          end;
         end;
        end;
       end else begin
        SetLength(TemporaryBitangents,length(TemporaryPositions));
        for VertexIndex:=0 to length(TemporaryBitangents)-1 do begin
         PpvVector3(pointer(@TemporaryBitangents[VertexIndex]))^:=(PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^.Normalize.Cross(PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^.Normalize)).Normalize*TemporaryTangents[VertexIndex,3];
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

      HasMorphVertexTargets:=SourceMeshPrimitive.Targets.Count>0;

      begin
       // Generate vertex array buffer
       SetLength(DestinationMeshPrimitiveVertices,length(TemporaryPositions));
       for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
        Vertex:=@DestinationMeshPrimitiveVertices[VertexIndex];
        FillChar(Vertex^,SizeOf(TVertex),#0);
        Vertex^.Position:=TpvVector3(pointer(@TemporaryPositions[VertexIndex])^);
        Vertex^.NodeIndex:=TpvUInt32($ffffffff);
        if VertexIndex<length(TemporaryNormals) then begin
         TangentSpaceMatrix.Normal:=TpvVector3(pointer(@TemporaryNormals[VertexIndex])^);
        end else begin
         TangentSpaceMatrix.Normal:=TpvVector3.ZAxis;
        end;
        if VertexIndex<length(TemporaryTangents) then begin
         TangentSpaceMatrix.Tangent:=TpvVector3(pointer(@TemporaryTangents[VertexIndex])^);
        end else begin
         TangentSpaceMatrix.Tangent:=TpvVector3.XAxis;
        end;
        if VertexIndex<length(TemporaryBitangents) then begin
         TangentSpaceMatrix.Bitangent:=TpvVector3(pointer(@TemporaryBitangents[VertexIndex])^);
        end else begin
         TangentSpaceMatrix.Bitangent:=TpvVector3.YAxis;
        end;
        TangentSpaceQuaternion:=TangentSpaceMatrix.ToQTangent;
        Vertex^.TangentSpace[0]:=Min(Max(round(TangentSpaceQuaternion.x*32767.0),-32768),32767);
        Vertex^.TangentSpace[1]:=Min(Max(round(TangentSpaceQuaternion.y*32767.0),-32768),32767);
        Vertex^.TangentSpace[2]:=Min(Max(round(TangentSpaceQuaternion.z*32767.0),-32768),32767);
        Vertex^.TangentSpace[3]:=Min(Max(round(TangentSpaceQuaternion.w*32767.0),-32768),32767);
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
        if HasMorphVertexTargets then begin
         Vertex^.MorphTargetVertexBaseIndex:=fGroup.fMorphTargetVertices.Count+(VertexIndex*SourceMeshPrimitive.Targets.Count);
         Vertex^.CountMorphTargetVertices:=SourceMeshPrimitive.Targets.Count;
        end else begin
         Vertex^.MorphTargetVertexBaseIndex:=TpvUInt32($ffffffff);
         Vertex^.CountMorphTargetVertices:=0;
        end;
        if CountJointBlocks>0 then begin
         FillChar(MaxJointBlocks^,SizeOf(TMaxJointBlocks),#0);
         for JointBlockIndex:=0 to CountJointBlocks-1 do begin
          if VertexIndex<length(TemporaryJoints[JointBlockIndex]) then begin
           MaxJointBlocks^[JointBlockIndex].Joints[0]:=TemporaryJoints[JointBlockIndex][VertexIndex][0];
           MaxJointBlocks^[JointBlockIndex].Joints[1]:=TemporaryJoints[JointBlockIndex][VertexIndex][1];
           MaxJointBlocks^[JointBlockIndex].Joints[2]:=TemporaryJoints[JointBlockIndex][VertexIndex][2];
           MaxJointBlocks^[JointBlockIndex].Joints[3]:=TemporaryJoints[JointBlockIndex][VertexIndex][3];
          end;
          if VertexIndex<length(TemporaryWeights[JointBlockIndex]) then begin
           MaxJointBlocks^[JointBlockIndex].Weights.x:=TemporaryWeights[JointBlockIndex][VertexIndex][0];
           MaxJointBlocks^[JointBlockIndex].Weights.y:=TemporaryWeights[JointBlockIndex][VertexIndex][1];
           MaxJointBlocks^[JointBlockIndex].Weights.z:=TemporaryWeights[JointBlockIndex][VertexIndex][2];
           MaxJointBlocks^[JointBlockIndex].Weights.w:=TemporaryWeights[JointBlockIndex][VertexIndex][3];
          end;
         end;
         if not MaxJointBlocksHashMap.TryGet(MaxJointBlocks^,Vertex^.JointBlockBaseIndex) then begin
          Vertex^.JointBlockBaseIndex:=fGroup.fJointBlocks.Count;
          for JointBlockIndex:=0 to CountJointBlocks-1 do begin
           fGroup.fJointBlocks.Add(MaxJointBlocks^[JointBlockIndex]);
          end;
          MaxJointBlocksHashMap.Add(MaxJointBlocks^,Vertex^.JointBlockBaseIndex);
         end;
         Vertex^.CountJointBlocks:=CountJointBlocks;
        end else begin
         Vertex^.JointBlockBaseIndex:=TpvUInt32($ffffffff);
         Vertex^.CountJointBlocks:=0;
        end;
       end;
      end;

      begin
       // Generate vertex index array buffer
       SetLength(DestinationMeshPrimitiveIndices,length(TemporaryIndices));
       if length(TemporaryIndices)>0 then begin
        Move(TemporaryIndices[0],DestinationMeshPrimitiveIndices[0],length(TemporaryIndices)*SizeOf(TpvUInt32));
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
         if length(TemporaryPositions)<>length(DestinationMeshPrimitiveVertices) then begin
          raise EPasGLTF.Create('Vertex count mismatch');
         end;
        end else begin
         SetLength(TemporaryPositions,length(DestinationMeshPrimitiveVertices));
         for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
          TemporaryPositions[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
         end;
        end;

        AccessorIndex:=SourceMeshPrimitiveTarget['NORMAL'];
        if AccessorIndex>=0 then begin
         TemporaryNormals:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
         if length(TemporaryNormals)<>length(DestinationMeshPrimitiveVertices) then begin
          raise EPasGLTF.Create('Vertex count mismatch');
         end;
        end else begin
         SetLength(TemporaryNormals,length(DestinationMeshPrimitiveVertices));
         for VertexIndex:=0 to length(TemporaryNormals)-1 do begin
          TemporaryNormals[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
         end;
        end;

        AccessorIndex:=SourceMeshPrimitiveTarget['TANGENT'];
        if AccessorIndex>=0 then begin
         TemporaryTargetTangents:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
         if length(TemporaryTargetTangents)<>length(DestinationMeshPrimitiveVertices) then begin
          raise EPasGLTF.Create('Vertex count mismatch');
         end;
         DoNeedCalculateTangents:=false;
        end else begin
         SetLength(TemporaryTargetTangents,length(DestinationMeshPrimitiveVertices));
         for VertexIndex:=0 to length(TemporaryTargetTangents)-1 do begin
          TemporaryTargetTangents[VertexIndex]:=TPasGLTF.TDefaults.NullVector3;
         end;
         DoNeedCalculateTangents:=true;
        end;

        // Construct morph target vertex array
        SetLength(DestinationMeshPrimitiveTarget^.Vertices,length(DestinationMeshPrimitiveVertices));
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
           Vertex:=@DestinationMeshPrimitiveVertices[VertexIndex];
           TemporaryPositions[VertexIndex,0]:=Vertex^.Position[0]+DestinationMeshPrimitiveTargetVertex^.Position[0];
           TemporaryPositions[VertexIndex,1]:=Vertex^.Position[1]+DestinationMeshPrimitiveTargetVertex^.Position[1];
           TemporaryPositions[VertexIndex,2]:=Vertex^.Position[2]+DestinationMeshPrimitiveTargetVertex^.Position[2];
           TangentSpaceMatrix:=TpvMatrix3x3.CreateFromQTangent(TpvQuaternion.Create(Min(Max(Vertex^.TangentSpace[0]/32767.0,-1.0),1.0),
                                                                                    Min(Max(Vertex^.TangentSpace[1]/32767.0,-1.0),1.0),
                                                                                    Min(Max(Vertex^.TangentSpace[2]/32767.0,-1.0),1.0),
                                                                                    Min(Max(Vertex^.TangentSpace[3]/32767.0,-1.0),1.0)));
           TemporaryNormals[VertexIndex,0]:=TangentSpaceMatrix.Normal.x+DestinationMeshPrimitiveTargetVertex^.Normal.x;
           TemporaryNormals[VertexIndex,1]:=TangentSpaceMatrix.Normal.y+DestinationMeshPrimitiveTargetVertex^.Normal.y;
           TemporaryNormals[VertexIndex,2]:=TangentSpaceMatrix.Normal.z+DestinationMeshPrimitiveTargetVertex^.Normal.z;
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
         SetLength(DestinationMeshPrimitiveTarget^.Vertices,length(DestinationMeshPrimitiveVertices));
         for VertexIndex:=0 to length(DestinationMeshPrimitiveTarget^.Vertices)-1 do begin
          DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex];
          Vertex:=@DestinationMeshPrimitiveVertices[VertexIndex];
          TangentSpaceMatrix:=TpvMatrix3x3.CreateFromQTangent(TpvQuaternion.Create(Min(Max(Vertex^.TangentSpace[0]/32767.0,-1.0),1.0),
                                                                                   Min(Max(Vertex^.TangentSpace[1]/32767.0,-1.0),1.0),
                                                                                   Min(Max(Vertex^.TangentSpace[2]/32767.0,-1.0),1.0),
                                                                                   Min(Max(Vertex^.TangentSpace[3]/32767.0,-1.0),1.0)));
          DestinationMeshPrimitiveTargetVertex^.Tangent.x:=TemporaryTangents[VertexIndex,0]-TangentSpaceMatrix.Tangent.x;
          DestinationMeshPrimitiveTargetVertex^.Tangent.y:=TemporaryTangents[VertexIndex,1]-TangentSpaceMatrix.Tangent.y;
          DestinationMeshPrimitiveTargetVertex^.Tangent.z:=TemporaryTangents[VertexIndex,2]-TangentSpaceMatrix.Tangent.z;
          DestinationMeshPrimitiveTargetVertex^.Count:=length(DestinationMeshPrimitive^.Targets);
         end;
        end;

       end;

      end;

      DestinationMeshPrimitive^.StartBufferVertexOffset:=fGroup.fVertices.Count;
      fGroup.fVertices.Add(DestinationMeshPrimitiveVertices);
      DestinationMeshPrimitive^.CountVertices:=TpvSizeUInt(fGroup.fVertices.Count)-DestinationMeshPrimitive^.StartBufferVertexOffset;

      DestinationMeshPrimitive^.StartBufferIndexOffset:=fGroup.fIndices.Count;
      fGroup.fIndices.Add(DestinationMeshPrimitiveIndices);
      for IndexIndex:=TpvSizeInt(DestinationMeshPrimitive^.StartBufferIndexOffset) to fGroup.fIndices.Count-1 do begin
       inc(fGroup.fIndices.Items[IndexIndex],DestinationMeshPrimitive^.StartBufferVertexOffset);
      end;
      DestinationMeshPrimitive^.CountIndices:=TpvSizeUInt(fGroup.fIndices.Count)-DestinationMeshPrimitive^.StartBufferIndexOffset;

      DestinationMeshPrimitive^.MorphTargetBaseIndex:=fGroup.fMorphTargetCount;

      if length(DestinationMeshPrimitive^.Targets)>0 then begin

       inc(fGroup.fMorphTargetCount,length(DestinationMeshPrimitive^.Targets));

       for VertexIndex:=TpvSizeInt(DestinationMeshPrimitive^.StartBufferVertexOffset) to TpvSizeInt(DestinationMeshPrimitive^.StartBufferVertexOffset+DestinationMeshPrimitive^.CountVertices)-1 do begin
        Vertex:=@fGroup.fVertices.Items[VertexIndex];
        Vertex^.MorphTargetVertexBaseIndex:=fGroup.fMorphTargetVertices.Count;
        Vertex^.CountMorphTargetVertices:=length(DestinationMeshPrimitive^.Targets);
        for TargetIndex:=0 to length(DestinationMeshPrimitive^.Targets)-1 do begin
         DestinationMeshPrimitiveTarget:=@DestinationMeshPrimitive^.Targets[TargetIndex];
         DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex];
         MorphTargetVertexIndex:=fGroup.fMorphTargetVertices.AddNew;
         MorphTargetVertex:=@fGroup.fMorphTargetVertices.Items[MorphTargetVertexIndex];
         MorphTargetVertex^.Position:=DestinationMeshPrimitiveTargetVertex^.Position;
         MorphTargetVertex^.Normal:=DestinationMeshPrimitiveTargetVertex^.Normal;
         MorphTargetVertex^.Tangent:=DestinationMeshPrimitiveTargetVertex^.Tangent;
         MorphTargetVertex^.Index:=DestinationMeshPrimitive^.MorphTargetBaseIndex+TargetIndex;
         if (TargetIndex+1)<length(DestinationMeshPrimitive^.Targets) then begin
          MorphTargetVertex^.Next:=MorphTargetVertexIndex+1;
         end else begin
          MorphTargetVertex^.Next:=TpvUInt32($ffffffff);
         end;
        end;
       end;
      end;

      DestinationMeshPrimitive^.NodeMeshPrimitiveInstances.Initialize;

      DestinationMeshPrimitive^.NodeMeshPrimitiveInstances.Clear;

     finally
      DestinationMeshPrimitiveIndices:=nil;
     end;

    finally
     DestinationMeshPrimitiveVertices:=nil;
    end;

   finally
    FreeAndNil(MaxJointBlocksHashMap);

   end;

  end;

  begin
   // Process morph target weights
   SetLength(fWeights,aSourceMesh.Weights.Count);
   for WeightIndex:=0 to length(fWeights)-1 do begin
    fWeights[WeightIndex]:=aSourceMesh.Weights[WeightIndex];
   end;
   OldCount:=length(fWeights);
   if OldCount<MaxCountTargets then begin
    SetLength(fWeights,MaxCountTargets);
    for WeightIndex:=OldCount to length(fWeights)-1 do begin
     fWeights[WeightIndex]:=0.0;
    end;
   end;
  end;

 finally
  FreeMem(MaxJointBlocks);
 end;

end;

{ TpvScene3D.TSkin }

constructor TpvScene3D.TGroup.TSkin.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
end;

destructor TpvScene3D.TGroup.TSkin.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TSkin.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSkin:TPasGLTF.TSkin);
var JointIndex,OldCount:TPasGLTFSizeInt;
    JSONItem:TPasJSONItem;
    JSONObject:TPasJSONItemObject;
    InverseBindMatrices:TPasGLTF.TMatrix4x4DynamicArray;
begin

 fName:=aSourceSkin.Name;

 fSkeleton:=aSourceSkin.Skeleton;

 fInverseBindMatrices.Initialize;

 if aSourceSkin.InverseBindMatrices>=0 then begin
  InverseBindMatrices:=aSourceDocument.Accessors[aSourceSkin.InverseBindMatrices].DecodeAsMatrix4x4Array(false);
  try
   fInverseBindMatrices.Count:=length(InverseBindMatrices);
   SetLength(fInverseBindMatrices.Items,fInverseBindMatrices.Count);
   if fInverseBindMatrices.Count>0 then begin
    Move(InverseBindMatrices[0],fInverseBindMatrices.Items[0],length(InverseBindMatrices)*SizeOf(TpvMatrix4x4));
   end;
  finally
   InverseBindMatrices:=nil;
  end;
 end;

 fMatrices.Initialize;
 fMatrices.Resize(aSourceSkin.Joints.Count);

 fJoints.Initialize;
 fJoints.Resize(aSourceSkin.Joints.Count);
 for JointIndex:=0 to fJoints.Count-1 do begin
  fJoints.Items[JointIndex]:=aSourceSkin.Joints.Items[JointIndex];
 end;

 OldCount:=fInverseBindMatrices.Count;
 if OldCount<aSourceSkin.Joints.Count then begin
  fInverseBindMatrices.Resize(aSourceSkin.Joints.Count);
  for JointIndex:=0 to fInverseBindMatrices.Count-1 do begin
   fInverseBindMatrices.Items[JointIndex]:=TpvMatrix4x4(pointer(@TPasGLTF.TDefaults.IdentityMatrix4x4)^);
  end;
 end;

 fInverseBindMatrices.Finish;
 fMatrices.Finish;
 fJoints.Finish;

end;

{ TpvScene3D.TGroup.TLight }

constructor TpvScene3D.TGroup.TLight.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
 fNodes:=TNodes.Create;
 fNodes.OwnsObjects:=false;
end;

destructor TpvScene3D.TGroup.TLight.Destroy;
begin
 FreeAndNil(fNodes);
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TLight.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceLight:TPasJSONItemObject);
var TypeString:TPasJSONUTF8String;
    ColorItem,SpotItem:TPasJSONItem;
    ColorArray:TPasJSONItemArray;
    SpotObject:TPasJSONItemObject;
begin
 fName:='';
 fNodes.Clear;
 fType_:=TType.None;
 fShadowMapIndex:=-1;
 fIntensity:=1.0;
 fRange:=0.0;
 fInnerConeAngle:=0.0;
 fOuterConeAngle:=pi*0.25;
 fColor.x:=1.0;
 fColor.y:=1.0;
 fColor.z:=1.0;
 fCastShadows:=false;
 if assigned(aSourceLight) then begin
  fName:=TPasJSON.GetString(aSourceLight.Properties['name'],'');
  TypeString:=TPasJSON.GetString(aSourceLight.Properties['type'],'');
  if pos('_noshadows',String(fName))>0 then begin
   fCastShadows:=false;
  end else begin
   fCastShadows:=TPasJSON.GetBoolean(aSourceLight.Properties['castShadows'],true);
  end;
  if TypeString='directional' then begin
   fType_:=TType.Directional;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end else if TypeString='point' then begin
   fType_:=TType.Point;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountCubeMapShadowMaps;
    inc(fCountCubeMapShadowMaps);
   end;}
  end else if TypeString='spot' then begin
   fType_:=TType.Spot;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end else begin
   fType_:=TType.None;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end;
  fIntensity:=TPasJSON.GetNumber(aSourceLight.Properties['intensity'],fIntensity);
  fRange:=TPasJSON.GetNumber(aSourceLight.Properties['range'],fRange);
  SpotItem:=aSourceLight.Properties['spot'];
  if assigned(SpotItem) and (SpotItem is TPasJSONItemObject) then begin
   SpotObject:=TPasJSONItemObject(SpotItem);
   fInnerConeAngle:=TPasJSON.GetNumber(SpotObject.Properties['innerConeAngle'],fInnerConeAngle);
   fOuterConeAngle:=TPasJSON.GetNumber(SpotObject.Properties['outerConeAngle'],fOuterConeAngle);
  end;
  ColorItem:=aSourceLight.Properties['color'];
  if assigned(ColorItem) and (ColorItem is TPasJSONItemArray) then begin
   ColorArray:=TPasJSONItemArray(ColorItem);
   if ColorArray.Count>0 then begin
    fColor.x:=TPasJSON.GetNumber(ColorArray.Items[0],fColor.x);
   end;
   if ColorArray.Count>1 then begin
    fColor.y:=TPasJSON.GetNumber(ColorArray.Items[1],fColor.y);
   end;
   if ColorArray.Count>2 then begin
    fColor.z:=TPasJSON.GetNumber(ColorArray.Items[2],fColor.z);
   end;
  end;
 end;
end;

{ TpvScene3D.TGroup.TNode }

constructor TpvScene3D.TGroup.TNode.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin

 inherited Create(aGroup);

 fIndex:=aIndex;

 fChildNodeIndices.Initialize;

 fChildren:=TNodes.Create;
 fChildren.OwnsObjects:=false;

 fMesh:=nil;

 fSkin:=nil;

 fLight:=nil;

 fShaderStorageBufferObjectOffset:=0;

 fShaderStorageBufferObjectSize:=0;

end;

destructor TpvScene3D.TGroup.TNode.Destroy;
begin

 fMesh:=nil;

 fSkin:=nil;

 fLight:=nil;

 FreeAndNil(fChildren);

 fChildNodeIndices.Finalize;

 inherited Destroy;

end;

procedure TpvScene3D.TGroup.TNode.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceNode:TPasGLTF.TNode;const aLightMap:TLights);
var WeightIndex,ChildrenIndex,Count,LightIndex:TPasGLTFSizeInt;
    Mesh:TMesh;
    ExtensionObject:TPasJSONItemObject;
    KHRLightsPunctualItem:TPasJSONItem;
    KHRLightsPunctualObject:TPasJSONItemObject;
begin

 fName:=aSourceNode.Name;

 if (aSourceNode.Mesh>=0) and (aSourceNode.Mesh<fGroup.fMeshes.Count) then begin
  fMesh:=fGroup.fMeshes[aSourceNode.Mesh];
 end else begin
  fMesh:=nil;
 end;

 if (aSourceNode.Camera>=0) and (aSourceNode.Camera<fGroup.fCameras.Count) then begin
  fCamera:=fGroup.fCameras[aSourceNode.Camera];
 end else begin
  fCamera:=nil;
 end;

 if (aSourceNode.Skin>=0) and (aSourceNode.Skin<fGroup.fSkins.Count) then begin
  fSkin:=fGroup.fSkins[aSourceNode.Skin];
 end else begin
  fSkin:=nil;
 end;

 fJoint:=-1;

 fMatrix:=TpvMatrix4x4(pointer(@aSourceNode.Matrix)^);

 fTranslation:=TpvVector3(pointer(@aSourceNode.Translation)^);

 fRotation:=TpvVector4(pointer(@aSourceNode.Rotation)^);

 fScale:=TpvVector3(pointer(@aSourceNode.Scale)^);

 SetLength(fWeights,aSourceNode.Weights.Count);
 for WeightIndex:=0 to length(fWeights)-1 do begin
  fWeights[WeightIndex]:=aSourceNode.Weights[WeightIndex];
 end;

 if assigned(fMesh) then begin
  Mesh:=fMesh;
  Count:=length(fWeights);
  if Count<length(Mesh.fWeights) then begin
   SetLength(fWeights,length(Mesh.fWeights));
   for WeightIndex:=Count to length(Mesh.fWeights)-1 do begin
    fWeights[WeightIndex]:=Mesh.fWeights[WeightIndex];
   end;
  end;
 end;

 fChildNodeIndices.Initialize;
 fChildNodeIndices.Resize(aSourceNode.Children.Count);
 for ChildrenIndex:=0 to fChildNodeIndices.Count-1 do begin
  fChildNodeIndices.Items[ChildrenIndex]:=aSourceNode.Children[ChildrenIndex];
 end;
 fChildNodeIndices.Finish;

 if aLightMap.Count>0 then begin
  ExtensionObject:=aSourceNode.Extensions;
  if assigned(ExtensionObject) then begin
   KHRLightsPunctualItem:=ExtensionObject.Properties['KHR_lights_punctual'];
   if assigned(KHRLightsPunctualItem) and (KHRLightsPunctualItem is TPasJSONItemObject) then begin
    KHRLightsPunctualObject:=TPasJSONItemObject(KHRLightsPunctualItem);
    LightIndex:=TPasJSON.GetInt64(KHRLightsPunctualObject.Properties['light'],-1);
    if (LightIndex>=0) and (LightIndex<aLightMap.Count) then begin
     fLight:=aLightMap[LightIndex];
     fLight.fNodes.Add(self);
    end;
   end;
  end;
 end;

end;

procedure TpvScene3D.TGroup.TNode.Finish;
var ChildrenIndex,NodeIndex:TpvSizeInt;
begin
 for ChildrenIndex:=0 to fChildNodeIndices.Count-1 do begin
  NodeIndex:=fChildNodeIndices.Items[ChildrenIndex];
  if (NodeIndex>=0) and (NodeIndex<fGroup.fNodes.Count) then begin
   fChildren.Add(fGroup.fNodes[NodeIndex]);
  end else begin
   raise EPasGLTFInvalidDocument.Create('Node index out of range');
  end;
 end;
end;

{ TpvScene3D.TGroup.TScene }

constructor TpvScene3D.TGroup.TScene.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
 fNodes:=TNodes.Create;
 fNodes.OwnsObjects:=false;
end;

destructor TpvScene3D.TGroup.TScene.Destroy;
begin
 FreeAndNil(fNodes);
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TScene.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceScene:TPasGLTF.TScene);
var Index,NodeIndex:TpvSizeInt;
begin
 fName:=aSourceScene.Name;
 fNodes.Clear;
 for Index:=0 to aSourceScene.Nodes.Count-1 do begin
  NodeIndex:=aSourceScene.Nodes[Index];
  if (NodeIndex>=0) and (NodeIndex<fGroup.fNodes.Count) then begin
   fNodes.Add(fGroup.fNodes[NodeIndex]);
  end else begin
   raise EPasGLTFInvalidDocument.Create('Node index out of range');
  end;
 end;
end;

{ TpvScene3D.TGroup }

constructor TpvScene3D.TGroup.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fLock:=TPasMPSpinLock.Create;

 fObjects:=TBaseObjects.Create;
 fObjects.OwnsObjects:=false;

 fAnimations:=TAnimations.Create;
 fAnimations.OwnsObjects:=true;

 fCameras:=TCameras.Create;
 fCameras.OwnsObjects:=true;

 fMeshes:=TMeshes.Create;
 fMeshes.OwnsObjects:=true;

 fSkins:=TSkins.Create;
 fSkins.OwnsObjects:=true;

 fNodes:=TNodes.Create;
 fNodes.OwnsObjects:=true;

 fLights:=TLights.Create;
 fLights.OwnsObjects:=true;

 fScenes:=TScenes.Create;
 fScenes.OwnsObjects:=true;

 fVertices.Initialize;

 fIndices.Initialize;

 fMorphTargetVertices.Initialize;

 fJointBlocks.Initialize;

 fSkinStorageBufferSize:=0;

 fMorphTargetCount:=0;

 fMaximalCountInstances:=1;

 fInstanceListLock:=TPasMPSlimReaderWriterLock.Create;

 fInstances:=TInstances.Create;
 fInstances.OwnsObjects:=false;

end;

destructor TpvScene3D.TGroup.Destroy;
begin

 while fInstances.Count>0 do begin
  fInstances[fInstances.Count-1].Free;
 end;
 FreeAndNil(fInstances);
 FreeAndNil(fInstanceListLock);

 FreeAndNil(fScenes);

 FreeAndNil(fLights);

 FreeAndNil(fNodes);

 FreeAndNil(fSkins);

 FreeAndNil(fMeshes);

 FreeAndNil(fCameras);

 FreeAndNil(fAnimations);

 FreeAndNil(fObjects);

 fIndices.Finalize;

 fVertices.Finalize;

 fMorphTargetVertices.Finalize;

 fJointBlocks.Finalize;

 FreeAndNil(fLock);

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

procedure TpvScene3D.TGroup.Upload;
var Index:TpvSizeInt;
    UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
    Node:TpvScene3D.TGroup.TNode;
    Mesh:TpvScene3D.TGroup.TMesh;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
    Material:TpvScene3D.TMaterial;
 procedure ProcessPrimitives;
 begin

  if fVertices.Count=0 then begin
   fVertices.AddNew;
  end;
  if fIndices.Count=0 then begin
   fIndices.Add(0);
  end;
  if fMorphTargetVertices.Count=0 then begin
   fMorphTargetVertices.AddNew;
  end;
  if fJointBlocks.Count=0 then begin
   fJointBlocks.AddNew;
  end;

  fVertices.Finish;
  fIndices.Finish;
  fMorphTargetVertices.Finish;
  fJointBlocks.Finish;

  fVulkanVertexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                              fVertices.Count*SizeOf(TVertex),
                                              TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                              TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                              [],
                                              TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                             );
  fVulkanVertexBuffer.UploadData(UniversalQueue,
                                 UniversalCommandBuffer,
                                 UniversalFence,
                                 fVertices.Items[0],
                                 0,
                                 fVertices.Count*SizeOf(TVertex),
                                 TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);

  fVulkanIndexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                             fIndices.Count*SizeOf(TVkUInt32),
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                             [],
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                            );
  fVulkanIndexBuffer.UploadData(UniversalQueue,
                                UniversalCommandBuffer,
                                UniversalFence,
                                fIndices.Items[0],
                                0,
                                fIndices.Count*SizeOf(TVkUInt32),
                                TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);

  fVulkanMorphTargetVertexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                         fMorphTargetVertices.Count*SizeOf(TMorphTargetVertex),
                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                         [],
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                                        );
  fVulkanMorphTargetVertexBuffer.UploadData(UniversalQueue,
                                            UniversalCommandBuffer,
                                            UniversalFence,
                                            fMorphTargetVertices.Items[0],
                                            0,
                                            fMorphTargetVertices.Count*SizeOf(TMorphTargetVertex),
                                            TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);

 end;
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try
     UniversalQueue:=pvApplication.VulkanDevice.UniversalQueue;
     try
      UniversalCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                        pvApplication.VulkanDevice.UniversalQueueFamilyIndex,
                                                        TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
      try
       UniversalCommandBuffer:=TpvVulkanCommandBuffer.Create(UniversalCommandPool,
                                                             VK_COMMAND_BUFFER_LEVEL_PRIMARY);
       try
        UniversalFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
        try
         ProcessPrimitives;
        finally
         FreeAndNil(UniversalFence);
        end;
       finally
        FreeAndNil(UniversalCommandBuffer);
       end;
      finally
       FreeAndNil(UniversalCommandPool);
      end;
     finally
      UniversalQueue:=nil;
     end;
     for Node in fNodes do begin
      Mesh:=Node.Mesh;
      if assigned(Mesh) then begin
       for Index:=0 to length(Mesh.fPrimitives)-1 do begin
        Primitive:=@Mesh.fPrimitives[Index];
        Material:=Primitive.Material;
        if assigned(Primitive.Material) then begin
         Material.Upload;
        end;
       end;
      end;
     end;
    finally
     fUploaded:=true;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.TGroup.Unload;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     FreeAndNil(fVulkanVertexBuffer);
     FreeAndNil(fVulkanIndexBuffer);
     FreeAndNil(fVulkanMorphTargetVertexBuffer);
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.TGroup.ConstructBuffers;
 procedure InitializeNodeMeshPrimitiveShaderStorageBufferObject;
 var NodeIndex:TpvSizeInt;
     Node:TNode;
 begin
  fNodeShaderStorageBufferObject.Count:=0;
  fNodeShaderStorageBufferObject.Size:=0;
  for NodeIndex:=0 to fNodes.Count-1 do begin
   Node:=fNodes[NodeIndex];
   if assigned(Node.fMesh) then begin
    Node.fShaderStorageBufferObjectOffset:=fNodeShaderStorageBufferObject.Size;
    Node.fShaderStorageBufferObjectSize:=SizeOf(TNodeShaderStorageBufferObjectDataItem);
    if assigned(Node.fSkin) then begin
     inc(Node.fShaderStorageBufferObjectSize,SizeOf(TpvMatrix4x4)*Node.fSkin.fJoints.Count);
    end;
    Node.fShaderStorageBufferObjectSize:=(Node.fShaderStorageBufferObjectSize+TpvSizeInt(127)) and not TpvSizeInt(127);
    inc(fNodeShaderStorageBufferObject.Size,Node.fShaderStorageBufferObjectSize);
   end;
  end;
 end;
begin
 InitializeNodeMeshPrimitiveShaderStorageBufferObject;
end;

procedure TpvScene3D.TGroup.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aMaximalCountInstances:TpvSizeInt=1);
var LightMap:TpvScene3D.TGroup.TLights;
    ImageMap:TpvScene3D.TImages;
    SamplerMap:TpvScene3D.TSamplers;
    TextureMap:TpvScene3D.TTextures;
    MaterialMap:TpvScene3D.TMaterials;
    HasLights:boolean;
 procedure ProcessImages;
 var Index:TpvSizeInt;
     SourceImage:TPasGLTF.TImage;
     Image,
     HashedImage:TImage;
     HashData:TImage.THashData;
 begin
  for Index:=0 to aSourceDocument.Images.Count-1 do begin
   SourceImage:=aSourceDocument.Images[Index];
   Image:=TImage.Create(pvApplication.ResourceManager,self);
   try
    fSceneInstance.fImageListLock.Acquire;
    try
     Image.AssignFromGLTF(aSourceDocument,SourceImage);
     HashData:=Image.GetHashData;
     HashedImage:=fSceneInstance.fImageHashMap[HashData];
     if assigned(HashedImage) then begin
      ImageMap.Add(HashedImage);
     end else begin
      Image.fHashData:=HashData;
      fSceneInstance.fImageHashMap[HashData]:=Image;
      ImageMap.Add(Image);
      Image:=nil;
     end;
    finally
     fSceneInstance.fImageListLock.Release;
    end;
   finally
    FreeAndNil(Image);
   end;
  end;
 end;
 procedure ProcessSamplers;
 var Index:TpvSizeInt;
     SourceSampler:TPasGLTF.TSampler;
     Sampler,
     HashedSampler:TSampler;
     HashData:TSampler.THashData;
 begin
  for Index:=0 to aSourceDocument.Samplers.Count-1 do begin
   SourceSampler:=aSourceDocument.Samplers[Index];
   Sampler:=TSampler.Create(pvApplication.ResourceManager,self);
   try
    fSceneInstance.fSamplerListLock.Acquire;
    try
     Sampler.AssignFromGLTF(aSourceDocument,SourceSampler);
     HashData:=Sampler.GetHashData;
     HashedSampler:=fSceneInstance.fSamplerHashMap[HashData];
     if assigned(HashedSampler) then begin
      SamplerMap.Add(HashedSampler);
     end else begin
      fSceneInstance.fSamplerHashMap[HashData]:=Sampler;
      SamplerMap.Add(Sampler);
      Sampler:=nil;
     end;
    finally
     fSceneInstance.fSamplerListLock.Release;
    end;
   finally
    FreeAndNil(Sampler);
   end;
  end;
 end;
 procedure ProcessTextures;
 var Index:TpvSizeInt;
     SourceTexture:TPasGLTF.TTexture;
     Texture,
     HashedTexture:TTexture;
     HashData:TTexture.THashData;
 begin
  for Index:=0 to aSourceDocument.Textures.Count-1 do begin
   SourceTexture:=aSourceDocument.Textures[Index];
   Texture:=TTexture.Create(pvApplication.ResourceManager,self);
   try
    fSceneInstance.fTextureListLock.Acquire;
    try
     Texture.AssignFromGLTF(aSourceDocument,SourceTexture,ImageMap,SamplerMap);
     HashData:=Texture.GetHashData;
     HashedTexture:=fSceneInstance.fTextureHashMap[HashData];
     if assigned(HashedTexture) then begin
      TextureMap.Add(HashedTexture);
     end else begin
      fSceneInstance.fTextureHashMap[HashData]:=Texture;
      TextureMap.Add(Texture);
      Texture:=nil;
     end;
    finally
     fSceneInstance.fTextureListLock.Release;
    end;
   finally
    FreeAndNil(Texture);
   end;
  end;
 end;
 procedure ProcessMaterials;
 var Index:TpvSizeInt;
     SourceMaterial:TPasGLTF.TMaterial;
     Material,
     HashedMaterial:TMaterial;
     HashData:TMaterial.THashData;
 begin
  for Index:=0 to aSourceDocument.Materials.Count-1 do begin
   SourceMaterial:=aSourceDocument.Materials[Index];
   Material:=TMaterial.Create(pvApplication.ResourceManager,self);
   try
    fSceneInstance.fMaterialListLock.Acquire;
    try
     Material.AssignFromGLTF(aSourceDocument,SourceMaterial,TextureMap);
     HashData:=Material.fData;
     HashedMaterial:=fSceneInstance.fMaterialHashMap[HashData];
     if assigned(HashedMaterial) then begin
      MaterialMap.Add(HashedMaterial);
     end else begin
      fSceneInstance.fMaterialHashMap[HashData]:=Material;
      MaterialMap.Add(Material);
      Material:=nil;
     end;
    finally
     fSceneInstance.fMaterialListLock.Release;
    end;
   finally
    FreeAndNil(Material);
   end;
  end;
 end;
 procedure ProcessAnimations;
 var Index:TpvSizeInt;
     SourceAnimation:TPasGLTF.TAnimation;
     Animation:TAnimation;
 begin
  for Index:=0 to aSourceDocument.Animations.Count-1 do begin
   SourceAnimation:=aSourceDocument.Animations[Index];
   Animation:=TAnimation.Create(self,Index);
   try
    Animation.AssignFromGLTF(aSourceDocument,SourceAnimation);
   finally
    fAnimations.Add(Animation);
   end;
  end;
 end;
 procedure ProcessCameras;
 var Index:TpvSizeInt;
     SourceCamera:TPasGLTF.TCamera;
     Camera:TCamera;
 begin
  for Index:=0 to aSourceDocument.Cameras.Count-1 do begin
   SourceCamera:=aSourceDocument.Cameras[Index];
   Camera:=TCamera.Create(self,Index);
   try
    Camera.AssignFromGLTF(aSourceDocument,SourceCamera);
   finally
    fCameras.Add(Camera);
   end;
  end;
 end;
 procedure ProcessMeshes;
 var Index:TpvSizeInt;
     SourceMesh:TPasGLTF.TMesh;
     Mesh:TMesh;
 begin
  fMorphTargetVertices.Clear;
  fMorphTargetCount:=0;
  for Index:=0 to aSourceDocument.Meshes.Count-1 do begin
   SourceMesh:=aSourceDocument.Meshes[Index];
   Mesh:=TMesh.Create(self,Index);
   try
    Mesh.AssignFromGLTF(aSourceDocument,SourceMesh,MaterialMap);
   finally
    fMeshes.Add(Mesh);
   end;
  end;
 end;
 procedure ProcessSkins;
 var Index:TpvSizeInt;
     SourceSkin:TPasGLTF.TSkin;
     Skin:TSkin;
 begin
  fSkinStorageBufferSize:=0;
  for Index:=0 to aSourceDocument.Skins.Count-1 do begin
   SourceSkin:=aSourceDocument.Skins[Index];
   Skin:=TSkin.Create(self,Index);
   try
    Skin.AssignFromGLTF(aSourceDocument,SourceSkin);
   finally
    fSkins.Add(Skin);
   end;
   Skin.fStorageBufferObjectOffset:=fSkinStorageBufferSize;
   if Skin.fJoints.Count>0 then begin
    Skin.fStorageBufferObjectSize:=Skin.fJoints.Count*SizeOf(TpvMatrix4x4);
    inc(fSkinStorageBufferSize,Skin.fStorageBufferObjectSize);
   end else begin
    Skin.fStorageBufferObjectSize:=0;
   end;
  end;
 end;
 procedure ProcessLights;
 var Index:TpvSizeInt;
     Light:TLight;
     ExtensionObject:TPasJSONItemObject;
     KHRLightsPunctualItem,LightsItem,LightItem:TPasJSONItem;
     KHRLightsPunctualObject:TPasJSONItemObject;
     LightsArray:TPasJSONItemArray;
 begin
  if HasLights then begin
   ExtensionObject:=aSourceDocument.Extensions;
   if assigned(ExtensionObject) then begin
    KHRLightsPunctualItem:=ExtensionObject.Properties['KHR_lights_punctual'];
    if assigned(KHRLightsPunctualItem) and (KHRLightsPunctualItem is TPasJSONItemObject) then begin
     KHRLightsPunctualObject:=TPasJSONItemObject(KHRLightsPunctualItem);
     LightsItem:=KHRLightsPunctualObject.Properties['lights'];
     if assigned(LightsItem) and (LightsItem is TPasJSONItemArray) then begin
      LightsArray:=TPasJSONItemArray(LightsItem);
      for Index:=0 to LightsArray.Count-1 do begin
       LightItem:=LightsArray.Items[Index];
       if assigned(LightItem) and (LightItem is TPasJSONItemObject) then begin
        Light:=TLight.Create(self,Index);
        try
         Light.AssignFromGLTF(aSourceDocument,TPasJSONItemObject(LightItem));
        finally
         fLights.Add(Light);
        end;
       end else begin
        Light:=nil;
       end;
       LightMap.Add(Light);
      end;
     end;
    end;
   end;
  end;
 end;
 procedure ProcessNodes;
 var Index:TpvSizeInt;
     SourceNode:TPasGLTF.TNode;
     Node:TNode;
 begin
  for Index:=0 to aSourceDocument.Nodes.Count-1 do begin
   SourceNode:=aSourceDocument.Nodes[Index];
   Node:=TNode.Create(self,Index);
   try
    Node.AssignFromGLTF(aSourceDocument,SourceNode,LightMap);
   finally
    fNodes.Add(Node);
   end;
  end;
  for Index:=0 to fNodes.Count-1 do begin
   fNodes[Index].Finish;
  end;
 end;
 procedure ProcessScenes;
 var Index:TpvSizeInt;
     SourceScene:TPasGLTF.TScene;
     Scene:TScene;
 begin
  for Index:=0 to aSourceDocument.Scenes.Count-1 do begin
   SourceScene:=aSourceDocument.Scenes[Index];
   Scene:=TScene.Create(self,Index);
   try
    Scene.AssignFromGLTF(aSourceDocument,SourceScene);
   finally
    fScenes.Add(Scene);
   end;
  end;
 end;
 procedure CalculateBoundingBox;
  procedure ProcessNode(const aNodeIndex:TpvSizeInt;const aMatrix:TpvMatrix4x4);
  var Index:TpvSizeInt;
      Matrix:TpvMatrix4x4;
      Node:TpvScene3D.TGroup.TNode;
  begin
   Node:=fNodes[aNodeIndex];
   Matrix:=((TpvMatrix4x4.CreateScale(Node.fScale)*
             (TpvMatrix4x4.CreateFromQuaternion(TpvQuaternion.Create(Node.fRotation))*
              TpvMatrix4x4.CreateTranslation(Node.fTranslation)))*Node.fMatrix)*aMatrix;
   if assigned(Node.fMesh) then begin
    fBoundingBox:=fBoundingBox.Combine(Node.fMesh.fBoundingBox.Transform(Matrix));
   end;
   for Index:=0 to Node.Children.Count-1 do begin
    ProcessNode(Node.Children[Index].Index,Matrix);
   end;
  end;
 var Scene:TpvScene3D.TGroup.TScene;
     Node:TpvScene3D.TGroup.TNode;
 begin
  fBoundingBox.Min:=TpvVector3.Origin;
  fBoundingBox.Max:=TpvVector3.Origin;
  for Scene in fScenes do begin
   for Node in Scene.fNodes do begin
    ProcessNode(Node.Index,TpvMatrix4x4.Identity);
   end;
  end;
 end;
begin

 fMaximalCountInstances:=aMaximalCountInstances;

 HasLights:=aSourceDocument.ExtensionsUsed.IndexOf('KHR_lights_punctual')>=0;

 LightMap:=TpvScene3D.TGroup.TLights.Create;
 LightMap.OwnsObjects:=false;
 try

  ProcessLights;

  ImageMap:=TpvScene3D.TImages.Create;
  ImageMap.OwnsObjects:=false;
  try

   ProcessImages;

   SamplerMap:=TpvScene3D.TSamplers.Create;
   SamplerMap.OwnsObjects:=false;
   try

    ProcessSamplers;

    TextureMap:=TpvScene3D.TTextures.Create;
    TextureMap.OwnsObjects:=false;
    try

     ProcessTextures;

     MaterialMap:=TpvScene3D.TMaterials.Create;
     MaterialMap.OwnsObjects:=false;
     try

      ProcessMaterials;

      ProcessAnimations;

      ProcessCameras;

      ProcessMeshes;

      ProcessSkins;

      ProcessNodes;

      ProcessScenes;

      if (aSourceDocument.Scene>=0) and (aSourceDocument.Scene<fScenes.Count) then begin
       fScene:=fScenes[aSourceDocument.Scene];
      end else if fScenes.Count>0 then begin
       fScene:=fScenes[0];
      end else begin
       fScene:=nil;
      end;

      CalculateBoundingBox;

     finally
      FreeAndNil(MaterialMap);
     end;

    finally
     FreeAndNil(TextureMap);
    end;

   finally
    FreeAndNil(SamplerMap);
   end;

  finally
   FreeAndNil(ImageMap);
  end;

 finally
  FreeAndNil(LightMap);
 end;

 ConstructBuffers;

end;

function TpvScene3D.TGroup.CreateInstance:TpvScene3D.TGroup.TInstance;
begin
 result:=TpvScene3D.TGroup.TInstance.Create(ResourceManager,self);
end;

{ TpvScene3D.TGroup.TInstance.TVulkanData }

constructor TpvScene3D.TGroup.TInstance.TVulkanData.Create(const aInstance:TGroup.TInstance);
begin
 inherited Create;
 fUploaded:=false;
 fInstance:=aInstance;
 fNodeMatricesBuffer:=nil;
 fMorphTargetVertexWeightsBuffer:=nil;
end;

destructor TpvScene3D.TGroup.TInstance.TVulkanData.Destroy;
begin
 Unload;
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TInstance.TVulkanData.Upload;
begin
 if not fUploaded then begin
  try
   fNodeMatricesBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                               length(fInstance.fNodeMatrices)*SizeOf(TpvMatrix4x4),
                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                               [],
                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                                              );
   fMorphTargetVertexWeightsBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                           length(fInstance.fMorphTargetVertexWeights)*SizeOf(TpvFloat),
                                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                           [],
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                                                          );
  finally
   fUploaded:=true;
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.TVulkanData.Unload;
begin
 if fUploaded then begin
  try
   FreeAndNil(fNodeMatricesBuffer);
   FreeAndNil(fMorphTargetVertexWeightsBuffer);
  finally
   fUploaded:=false;
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.TVulkanData.Update;
begin
 Upload;
 if fUploaded then begin
  fNodeMatricesBuffer.UpdateData(fInstance.fNodeMatrices[0],0,length(fInstance.fNodeMatrices)*SizeOf(TpvMatrix4x4));
  fMorphTargetVertexWeightsBuffer.UpdateData(fInstance.fMorphTargetVertexWeights[0],0,length(fInstance.fMorphTargetVertexWeights)*SizeOf(TpvFloat));
 end;
end;

{ TpvScene3D.TGroup.TInstance.TAnimation }

constructor TpvScene3D.TGroup.TInstance.TAnimation.Create;
begin
 inherited Create;
end;

destructor TpvScene3D.TGroup.TInstance.TAnimation.Destroy;
begin
 inherited Destroy;
end;

{ TpvScene3D.TGroup.TInstance }

constructor TpvScene3D.TGroup.TInstance.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
var Index,OtherIndex:TpvSizeInt;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    Node:TpvScene3D.TGroup.TNode;
begin
 inherited Create(aResourceManager,aParent);
 if aParent is TGroup then begin
  fGroup:=TpvScene3D.TGroup(aParent);
 end else begin
  fGroup:=nil;
 end;
 fUploaded:=false;
 fScene:=-1;
 fNodes:=nil;
 fSkins:=nil;
 fAnimations:=nil;
 SetLength(fNodes,fGroup.fNodes.Count);
 SetLength(fSkins,fGroup.fSkins.Count);
{SetLength(fLightNodes,fGroup.fLights.Count);
 SetLength(fLightShadowMapMatrices,fParent.fLights.Count);
 SetLength(fLightShadowMapZFarValues,fParent.fLights.Count);
 for Index:=0 to length(fLightNodes)-1 do begin
  fLightNodes[Index]:=-1;
 end;}
 for Index:=0 to fGroup.fNodes.Count-1 do begin
  InstanceNode:=@fNodes[Index];
  Node:=fGroup.fNodes[Index];
  SetLength(InstanceNode^.WorkWeights,length(Node.fWeights));
  SetLength(InstanceNode^.OverwriteWeights,length(Node.fWeights));
  SetLength(InstanceNode^.OverwriteWeightsSum,length(Node.fWeights));
  SetLength(InstanceNode^.Overwrites,fGroup.fAnimations.Count+1);
  for OtherIndex:=0 to fGroup.fAnimations.Count do begin
   SetLength(InstanceNode^.Overwrites[OtherIndex].Weights,length(Node.fWeights));
  end;
 end;
 SetLength(fAnimations,fGroup.fAnimations.Count+1);
 for Index:=0 to length(fAnimations)-1 do begin
  fAnimations[Index]:=TpvScene3D.TGroup.TInstance.TAnimation.Create;
 end;
 fNodeMatrices:=nil;
 fMorphTargetVertexWeights:=nil;
 for Index:=0 to length(fVulkanDatas)-1 do begin
  fVulkanDatas[Index]:=TpvScene3D.TGroup.TInstance.TVulkanData.Create(self);
 end;
end;

destructor TpvScene3D.TGroup.TInstance.Destroy;
var Index:TPasGLTFSizeInt;
begin
 Unload;
 for Index:=0 to length(fVulkanDatas)-1 do begin
  FreeAndNil(fVulkanDatas[Index]);
 end;
 for Index:=0 to length(fAnimations)-1 do begin
  FreeAndNil(fAnimations[Index]);
 end;
 fNodes:=nil;
 fSkins:=nil;
 fAnimations:=nil;
 fNodeMatrices:=nil;
 fMorphTargetVertexWeights:=nil;
 fGroup:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 fSceneInstance.fGroupInstanceListLock.Acquire;
 try
  fSceneInstance.fGroupInstances.Add(self);
 finally
  fSceneInstance.fGroupInstanceListLock.Release;
 end;
 fGroup.fInstanceListLock.Acquire;
 try
  fGroup.fInstances.Add(self);
 finally
  fGroup.fInstanceListLock.Release;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.BeforeDestruction;
begin
 try
  fSceneInstance.fGroupInstanceListLock.Acquire;
  try
   fSceneInstance.fGroupInstances.Remove(self);
  finally
   fSceneInstance.fGroupInstanceListLock.Release;
  end;
  fGroup.fInstanceListLock.Acquire;
  try
   fGroup.fInstances.Remove(self);
  finally
   fGroup.fInstanceListLock.Release;
  end;
 finally
  fGroup:=nil;
 end;
 inherited BeforeDestruction;
end;

function TpvScene3D.TGroup.TInstance.GetAutomation(const aIndex:TPasGLTFSizeInt):TAnimation;
begin
 result:=fAnimations[aIndex+1];
end;

procedure TpvScene3D.TGroup.TInstance.SetScene(const aScene:TPasGLTFSizeInt);
begin
 fScene:=Min(Max(aScene,-1),fGroup.fScenes.Count-1);
end;

function TpvScene3D.TGroup.TInstance.GetScene:TpvScene3D.TGroup.TScene;
begin
 if fGroup.fUploaded then begin
  if fScene<0 then begin
   result:=fGroup.fScene;
  end else if fScene<fGroup.fScenes.Count then begin
   result:=fGroup.fScenes[fScene];
  end else begin
   result:=nil;
  end;
 end else begin
  result:=nil;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.Upload;
var Index:TpvSizeInt;
begin
 inherited Upload;
 if not fUploaded then begin
  try
   SetLength(fNodeMatrices,Max(fGroup.fNodes.Count,1));
   SetLength(fMorphTargetVertexWeights,Max(fGroup.fMorphTargetCount,1));
   for Index:=0 to length(fVulkanDatas)-1 do begin
    fVulkanDatas[Index].Upload;
   end;
  finally
   fUploaded:=true;
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.Unload;
var Index:TpvSizeInt;
begin
 if fUploaded then begin
  try
   for Index:=0 to length(fVulkanDatas)-1 do begin
    fVulkanDatas[Index].Unload;
   end;
   fNodeMatrices:=nil;
   fMorphTargetVertexWeights:=nil;
  finally
   fUploaded:=false;
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.Update;
var {NonSkinnedShadingShader,SkinnedShadingShader:TShadingShader;
    CurrentShader:TShader;
    CurrentSkinShaderStorageBufferObjectHandle:glUInt;}
    CullFace,Blend:TPasGLTFInt32;
 procedure ResetNode(const aNodeIndex:TPasGLTFSizeInt);
 var Index:TPasGLTFSizeInt;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Node:TpvScene3D.TGroup.TNode;
 begin
  InstanceNode:=@fNodes[aNodeIndex];
  Node:=fGroup.fNodes[aNodeIndex];
  InstanceNode^.CountOverwrites:=0;
  InstanceNode^.OverwriteFlags:=[];
  for Index:=0 to Node.Children.Count-1 do begin
   ResetNode(Node.Children[Index].Index);
  end;
 end;
 procedure ProcessBaseOverwrite(const aFactor:TPasGLTFFloat);
 var Index:TPasGLTFSizeInt;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Overwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
 begin
  if aFactor>=-0.5 then begin
   for Index:=0 to fGroup.fNodes.Count-1 do begin
    InstanceNode:=@fNodes[Index];
    if InstanceNode^.CountOverwrites<length(InstanceNode^.Overwrites) then begin
     Overwrite:=@InstanceNode^.Overwrites[InstanceNode^.CountOverwrites];
     Overwrite^.Flags:=[TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Defaults];
     Overwrite^.Factor:=Max(aFactor,0.0);
     inc(InstanceNode^.CountOverwrites);
    end;
   end;
  end;
 end;
 procedure ProcessAnimation(const aAnimationIndex:TPasGLTFSizeInt;const aAnimationTime:TPasGLTFFloat;const aFactor:TPasGLTFFloat);
 var ChannelIndex,
     InputTimeArrayIndex,
     WeightIndex,
     CountWeights,
     ElementIndex,
     l,r,m:TpvSizeInt;
     Animation:TpvScene3D.TGroup.TAnimation;
     AnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
     //Node:TpvScene3D.TGroup.TNode;
     Node:TpvScene3D.TGroup.TInstance.PNode;
     Time,Factor,Scalar,Value,SqrFactor,CubeFactor,KeyDelta,v0,v1,a,b:TpvFloat;
     Vector3:TpvVector3;
     Vector4:TpvVector4;
     Vector3s:array[0..1] of PpvVector3;
     Vector4s:array[0..1] of PpvVector4;
     TimeIndices:array[0..1] of TpvSizeInt;
     Overwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
 begin

  Animation:=fGroup.fAnimations[aAnimationIndex];

  for ChannelIndex:=0 to length(Animation.fChannels)-1 do begin

   AnimationChannel:=@Animation.fChannels[ChannelIndex];

   if (AnimationChannel.Node>=0) and (length(AnimationChannel.InputTimeArray)>0) then begin

    TimeIndices[1]:=length(AnimationChannel^.InputTimeArray)-1;

    Time:=Min(Max(aAnimationTime,AnimationChannel^.InputTimeArray[0]),AnimationChannel^.InputTimeArray[TimeIndices[1]]);

    if (AnimationChannel^.Last<=0) or (Time<AnimationChannel^.InputTimeArray[AnimationChannel.Last-1]) then begin
     l:=0;
    end else begin
     l:=AnimationChannel^.Last-1;
    end;

    for InputTimeArrayIndex:=Min(Max(l,0),length(AnimationChannel^.InputTimeArray)-1) to Min(Max(l+3,0),length(AnimationChannel^.InputTimeArray)-1) do begin
     if AnimationChannel^.InputTimeArray[InputTimeArrayIndex]>Time then begin
      l:=InputTimeArrayIndex-1;
      break;
     end;
    end;

    r:=length(AnimationChannel^.InputTimeArray);
    if ((l+1)<r) and (Time<AnimationChannel^.InputTimeArray[l+1]) then begin
     inc(l);
    end else begin
     while l<r do begin
      m:=l+((r-l) shr 1);
      Value:=AnimationChannel^.InputTimeArray[m];
      if Value<=Time then begin
       l:=m+1;
       if Time<AnimationChannel^.InputTimeArray[l] then begin
        break;
       end;
      end else begin
       r:=m;
      end;
     end;
    end;

    for InputTimeArrayIndex:=Min(Max(l,0),length(AnimationChannel^.InputTimeArray)-1) to length(AnimationChannel^.InputTimeArray)-1 do begin
     if AnimationChannel^.InputTimeArray[InputTimeArrayIndex]>Time then begin
      TimeIndices[1]:=InputTimeArrayIndex;
      break;
     end;
    end;

    AnimationChannel^.Last:=TimeIndices[1];

    if TimeIndices[1]>=0 then begin

     TimeIndices[0]:=Max(0,TimeIndices[1]-1);

     KeyDelta:=AnimationChannel^.InputTimeArray[TimeIndices[1]]-AnimationChannel^.InputTimeArray[TimeIndices[0]];

     if SameValue(TimeIndices[0],TimeIndices[1]) then begin
      Factor:=0.0;
     end else begin
      Factor:=(Time-AnimationChannel^.InputTimeArray[TimeIndices[0]])/KeyDelta;
      if Factor<0.0 then begin
       Factor:=0.0;
      end else if Factor>1.0 then begin
       Factor:=1.0;
      end;
     end;

     Node:=@fNodes[AnimationChannel^.Node];

     if (aFactor>=-0.5) and (Node.CountOverwrites<length(Node.Overwrites)) then begin
      Overwrite:=@Node.Overwrites[Node.CountOverwrites];
      Overwrite^.Flags:=[];
      Overwrite^.Factor:=Max(aFactor,0.0);
      inc(Node^.CountOverwrites);
     end else begin
      Overwrite:=nil;
     end;

     case AnimationChannel^.Target of
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale:begin
       case AnimationChannel^.Interpolation of
        TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
         Vector3s[0]:=@AnimationChannel^.OutputVector3Array[TimeIndices[0]];
         Vector3s[1]:=@AnimationChannel^.OutputVector3Array[TimeIndices[1]];
         Vector3[0]:=(Vector3s[0]^[0]*(1.0-Factor))+(Vector3s[1]^[0]*Factor);
         Vector3[1]:=(Vector3s[0]^[1]*(1.0-Factor))+(Vector3s[1]^[1]*Factor);
         Vector3[2]:=(Vector3s[0]^[2]*(1.0-Factor))+(Vector3s[1]^[2]*Factor);
        end;
        TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
         Vector3:=AnimationChannel^.OutputVector3Array[TimeIndices[0]];
        end;
        TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
         SqrFactor:=sqr(Factor);
         CubeFactor:=SqrFactor*Factor;
         Vector3:=(((AnimationChannel^.OutputVector3Array[(TimeIndices[0]*3)+1]*(((2.0*CubeFactor)-(3.0*SqrFactor))+1.0))+
                   (AnimationChannel^.OutputVector3Array[(TimeIndices[1]*3)+0]*(KeyDelta*((CubeFactor-(2.0*SqrFactor))+Factor))))+
                    (AnimationChannel^.OutputVector3Array[(TimeIndices[1]*3)+1]*((3.0*SqrFactor)-(2.0*CubeFactor))))+
                     (AnimationChannel^.OutputVector3Array[(TimeIndices[1]*3)+0]*(KeyDelta*(CubeFactor-SqrFactor)));
        end;
        else begin
         Assert(false);
        end;
       end;
       case AnimationChannel^.Target of
        TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation:begin
         if assigned(Overwrite) then begin
          Include(Overwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Translation);
          Overwrite^.Translation:=Vector3;
         end else begin
          Include(Node^.OverwriteFlags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Translation);
          Node^.OverwriteTranslation:=Vector3;
         end;
        end;
        TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale:begin
         if assigned(Overwrite) then begin
          Include(Overwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Scale);
          Overwrite^.Scale:=Vector3;
         end else begin
          Include(Node^.OverwriteFlags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Scale);
          Node^.OverwriteScale:=Vector3;
         end;
        end;
       end;
      end;
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation:begin
       case AnimationChannel^.Interpolation of
        TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
         Vector4:=TpvQuaternion.Create(AnimationChannel^.OutputVector4Array[TimeIndices[0]]).Slerp(TpvQuaternion.Create(AnimationChannel^.OutputVector4Array[TimeIndices[1]]),Factor).Vector;
        end;
        TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
         Vector4:=AnimationChannel^.OutputVector4Array[TimeIndices[0]];
        end;
        TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
         SqrFactor:=sqr(Factor);
         CubeFactor:=SqrFactor*Factor;
         Vector4:=((((AnimationChannel^.OutputVector4Array[(TimeIndices[0]*3)+1]*(((2.0*CubeFactor)-(3.0*SqrFactor))+1.0))+
                    (AnimationChannel^.OutputVector4Array[(TimeIndices[1]*3)+0]*(KeyDelta*((CubeFactor-(2.0*SqrFactor))+Factor))))+
                     (AnimationChannel^.OutputVector4Array[(TimeIndices[1]*3)+1]*((3.0*SqrFactor)-(2.0*CubeFactor))))+
                      (AnimationChannel^.OutputVector4Array[(TimeIndices[1]*3)+0]*(KeyDelta*(CubeFactor-SqrFactor)))).Normalize;
        end;
        else begin
         Assert(false);
        end;
       end;
       if assigned(Overwrite) then begin
        Include(Overwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation);
        Overwrite^.Rotation:=Vector4;
       end else begin
        Include(Node^.OverwriteFlags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation);
        Node^.OverwriteRotation:=Vector4;
       end;
      end;
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights:begin
       CountWeights:=length(Node^.WorkWeights);
       if assigned(Overwrite) then begin
        Include(Overwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Weights);
        case AnimationChannel^.Interpolation of
         TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
          for WeightIndex:=0 to CountWeights-1 do begin
           Overwrite^.Weights[WeightIndex]:=(AnimationChannel^.OutputScalarArray[(TimeIndices[0]*CountWeights)+WeightIndex]*(1.0-Factor))+
                                            (AnimationChannel^.OutputScalarArray[(TimeIndices[1]*CountWeights)+WeightIndex]*Factor);
          end;
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
          for WeightIndex:=0 to CountWeights-1 do begin
           Overwrite^.Weights[WeightIndex]:=AnimationChannel^.OutputScalarArray[(TimeIndices[0]*CountWeights)+WeightIndex];
          end;
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
          SqrFactor:=sqr(Factor);
          CubeFactor:=SqrFactor*Factor;
          for WeightIndex:=0 to CountWeights-1 do begin
           Overwrite^. Weights[WeightIndex]:=((((2.0*CubeFactor)-(3.0*SqrFactor))+1.0)*AnimationChannel^.OutputScalarArray[(((TimeIndices[0]*3)+1)*CountWeights)+WeightIndex])+
                                             (((CubeFactor-(2.0*SqrFactor))+Factor)*KeyDelta*AnimationChannel^.OutputScalarArray[(((TimeIndices[0]*3)+2)*CountWeights)+WeightIndex])+
                                             (((3.0*SqrFactor)-(2.0*CubeFactor))*AnimationChannel^.OutputScalarArray[(((TimeIndices[1]*3)+1)*CountWeights)+WeightIndex])+
                                             ((CubeFactor-SqrFactor)*KeyDelta*AnimationChannel^.OutputScalarArray[(((TimeIndices[1]*3)+0)*CountWeights)+WeightIndex]);
          end;
         end;
         else begin
          Assert(false);
         end;
        end;
       end else begin
        Include(Node^.OverwriteFlags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Weights);
        case AnimationChannel^.Interpolation of
         TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
          for WeightIndex:=0 to CountWeights-1 do begin
           Node^.OverwriteWeights[WeightIndex]:=(AnimationChannel^.OutputScalarArray[(TimeIndices[0]*CountWeights)+WeightIndex]*(1.0-Factor))+
                                                (AnimationChannel^.OutputScalarArray[(TimeIndices[1]*CountWeights)+WeightIndex]*Factor);
          end;
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
          for WeightIndex:=0 to CountWeights-1 do begin
           Node^.OverwriteWeights[WeightIndex]:=AnimationChannel^.OutputScalarArray[(TimeIndices[0]*CountWeights)+WeightIndex];
          end;
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
          SqrFactor:=sqr(Factor);
          CubeFactor:=SqrFactor*Factor;
          for WeightIndex:=0 to CountWeights-1 do begin
           Node^.OverwriteWeights[WeightIndex]:=((((2.0*CubeFactor)-(3.0*SqrFactor))+1.0)*AnimationChannel^.OutputScalarArray[(((TimeIndices[0]*3)+1)*CountWeights)+WeightIndex])+
                                                (((CubeFactor-(2.0*SqrFactor))+Factor)*KeyDelta*AnimationChannel^.OutputScalarArray[(((TimeIndices[0]*3)+2)*CountWeights)+WeightIndex])+
                                                (((3.0*SqrFactor)-(2.0*CubeFactor))*AnimationChannel^.OutputScalarArray[(((TimeIndices[1]*3)+1)*CountWeights)+WeightIndex])+
                                                ((CubeFactor-SqrFactor)*KeyDelta*AnimationChannel^.OutputScalarArray[(((TimeIndices[1]*3)+0)*CountWeights)+WeightIndex]);
          end;
         end;
         else begin
          Assert(false);
         end;
        end;
       end;
      end;
     end;


    end;

   end;

  end;

 end;
 procedure ProcessNode(const aNodeIndex:TpvSizeInt;const aMatrix:TpvMatrix4x4);
 type TVector3Sum=record
       x,y,z,FactorSum:Double;
      end;
      TVector4Sum=record
       x,y,z,w,FactorSum:Double;
      end;
 var Index,OtherIndex:TpvSizeInt;
     Matrix:TpvMatrix4x4;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Node:TpvScene3D.TGroup.TNode;
     Translation,Scale:TpvVector3;
     Rotation:TpvVector4;
     TranslationSum,ScaleSum:TVector3Sum;
     RotationSum:TVector4Sum;
     Factor,
     WeightsFactorSum:TpvDouble;
     Overwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
     FirstWeights,SkinUsed:boolean;
 begin
  SkinUsed:=false;
  InstanceNode:=@fNodes[aNodeIndex];
  Node:=fGroup.fNodes[aNodeIndex];
  if InstanceNode^.CountOverwrites>0 then begin
   SkinUsed:=true;
   TranslationSum.x:=0.0;
   TranslationSum.y:=0.0;
   TranslationSum.z:=0.0;
   TranslationSum.FactorSum:=0.0;
   ScaleSum.x:=0.0;
   ScaleSum.y:=0.0;
   ScaleSum.z:=0.0;
   ScaleSum.FactorSum:=0.0;
   RotationSum.x:=0.0;
   RotationSum.y:=0.0;
   RotationSum.z:=0.0;
   RotationSum.w:=0.0;
   RotationSum.FactorSum:=0.0;
   WeightsFactorSum:=0.0;
   FirstWeights:=true;
   for Index:=0 to InstanceNode^.CountOverwrites-1 do begin
    Overwrite:=@InstanceNode^.Overwrites[Index];
    Factor:=Overwrite^.Factor;
    if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Defaults in Overwrite^.Flags then begin
     TranslationSum.x:=TranslationSum.x+(Node.fTranslation.x*Factor);
     TranslationSum.y:=TranslationSum.y+(Node.fTranslation.y*Factor);
     TranslationSum.z:=TranslationSum.z+(Node.fTranslation.z*Factor);
     TranslationSum.FactorSum:=TranslationSum.FactorSum+Factor;
     ScaleSum.x:=ScaleSum.x+(Node.fScale.x*Factor);
     ScaleSum.y:=ScaleSum.y+(Node.fScale.y*Factor);
     ScaleSum.z:=ScaleSum.z+(Node.fScale.z*Factor);
     ScaleSum.FactorSum:=ScaleSum.FactorSum+Factor;
     RotationSum.x:=RotationSum.x+(Node.fRotation.x*Factor);
     RotationSum.y:=RotationSum.y+(Node.fRotation.y*Factor);
     RotationSum.z:=RotationSum.z+(Node.fRotation.z*Factor);
     RotationSum.w:=RotationSum.w+(Node.fRotation.w*Factor);
     RotationSum.FactorSum:=RotationSum.FactorSum+Factor;
     if length(Node.fWeights)>0 then begin
      if FirstWeights then begin
       FirstWeights:=false;
       for OtherIndex:=0 to length(InstanceNode^.OverwriteWeightsSum)-1 do begin
        InstanceNode^.OverwriteWeightsSum[OtherIndex]:=0.0;
       end;
      end;
      for OtherIndex:=0 to Min(length(InstanceNode^.OverwriteWeightsSum),length(Node.fWeights))-1 do begin
       InstanceNode^.OverwriteWeightsSum[OtherIndex]:=InstanceNode^.OverwriteWeightsSum[OtherIndex]+(Node.fWeights[OtherIndex]*Factor);
      end;
      WeightsFactorSum:=WeightsFactorSum+Factor;
     end;
    end else begin
     if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Translation in Overwrite^.Flags then begin
      TranslationSum.x:=TranslationSum.x+(Overwrite^.Translation.x*Factor);
      TranslationSum.y:=TranslationSum.y+(Overwrite^.Translation.y*Factor);
      TranslationSum.z:=TranslationSum.z+(Overwrite^.Translation.z*Factor);
      TranslationSum.FactorSum:=TranslationSum.FactorSum+Factor;
     end;
     if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Scale in Overwrite^.Flags then begin
      ScaleSum.x:=ScaleSum.x+(Overwrite^.Scale.x*Factor);
      ScaleSum.y:=ScaleSum.y+(Overwrite^.Scale.y*Factor);
      ScaleSum.z:=ScaleSum.z+(Overwrite^.Scale.z*Factor);
      ScaleSum.FactorSum:=ScaleSum.FactorSum+Factor;
     end;
     if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation in Overwrite^.Flags then begin
      RotationSum.x:=RotationSum.x+(Overwrite^.Rotation.x*Factor);
      RotationSum.y:=RotationSum.y+(Overwrite^.Rotation.y*Factor);
      RotationSum.z:=RotationSum.z+(Overwrite^.Rotation.z*Factor);
      RotationSum.w:=RotationSum.w+(Overwrite^.Rotation.w*Factor);
      RotationSum.FactorSum:=RotationSum.FactorSum+Factor;
     end;
     if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Weights in Overwrite^.Flags then begin
      if FirstWeights then begin
       FirstWeights:=false;
       for OtherIndex:=0 to length(InstanceNode^.OverwriteWeightsSum)-1 do begin
        InstanceNode^.OverwriteWeightsSum[OtherIndex]:=0.0;
       end;
      end;
      for OtherIndex:=0 to Min(length(InstanceNode^.OverwriteWeightsSum),length(Overwrite^.Weights))-1 do begin
       InstanceNode^.OverwriteWeightsSum[OtherIndex]:=InstanceNode^.OverwriteWeightsSum[OtherIndex]+(Overwrite^.Weights[OtherIndex]*Factor);
      end;
      WeightsFactorSum:=WeightsFactorSum+Factor;
     end;
    end;
   end;
   if TranslationSum.FactorSum>0.0 then begin
    Factor:=1.0/TranslationSum.FactorSum;
    Translation.x:=TranslationSum.x*Factor;
    Translation.y:=TranslationSum.y*Factor;
    Translation.z:=TranslationSum.z*Factor;
   end else begin
    Translation:=Node.fTranslation;
   end;
   if ScaleSum.FactorSum>0.0 then begin
    Factor:=1.0/ScaleSum.FactorSum;
    Scale.x:=ScaleSum.x*Factor;
    Scale.y:=ScaleSum.y*Factor;
    Scale.z:=ScaleSum.z*Factor;
   end else begin
    Scale:=Node.fScale;
   end;
   if RotationSum.FactorSum>0.0 then begin
    Factor:=1.0/RotationSum.FactorSum;
    Rotation.x:=RotationSum.x*Factor;
    Rotation.y:=RotationSum.y*Factor;
    Rotation.z:=RotationSum.z*Factor;
    Rotation.w:=RotationSum.w*Factor;
    Rotation:=Rotation.Normalize;
   end else begin
    Rotation:=Node.fRotation;
   end;
   if WeightsFactorSum>0.0 then begin
    Factor:=1.0/WeightsFactorSum;
    for Index:=0 to Min(length(InstanceNode^.WorkWeights),length(Node.fWeights))-1 do begin
     InstanceNode^.WorkWeights[Index]:=InstanceNode^.OverwriteWeightsSum[Index]*Factor;
    end;
   end else begin
    for Index:=0 to Min(length(InstanceNode^.WorkWeights),length(Node.fWeights))-1 do begin
     InstanceNode^.WorkWeights[Index]:=Node.fWeights[Index];
    end;
   end;
  end else begin
   if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Translation in InstanceNode^.OverwriteFlags then begin
    Translation:=InstanceNode^.OverwriteTranslation;
    SkinUsed:=true;
   end else begin
    Translation:=Node.fTranslation;
   end;
   if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Scale in InstanceNode^.OverwriteFlags then begin
    Scale:=InstanceNode^.OverwriteScale;
    SkinUsed:=true;
   end else begin
    Scale:=Node.fScale;
   end;
   if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation in InstanceNode^.OverwriteFlags then begin
    Rotation:=InstanceNode^.OverwriteRotation;
    SkinUsed:=true;
   end else begin
    Rotation:=Node.fRotation;
   end;
   if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Weights in InstanceNode^.OverwriteFlags then begin
    SkinUsed:=true;
    for Index:=0 to Min(length(InstanceNode^.WorkWeights),length(InstanceNode^.OverwriteWeights))-1 do begin
     InstanceNode^.WorkWeights[Index]:=InstanceNode^.OverwriteWeights[Index];
    end;
   end else begin
    for Index:=0 to Min(length(InstanceNode^.WorkWeights),length(Node.fWeights))-1 do begin
     InstanceNode^.WorkWeights[Index]:=Node.fWeights[Index];
    end;
   end;
  end;
  Matrix:=TpvMatrix4x4.CreateScale(Scale)*
          (TpvMatrix4x4.CreateFromQuaternion(TpvQuaternion.Create(Rotation))*
           TpvMatrix4x4.CreateTranslation(Translation));
  if assigned(fOnNodeMatrixPre) then begin
   fOnNodeMatrixPre(self,Node,InstanceNode,Matrix);
  end;
  Matrix:=Matrix*Node.fMatrix;
  if assigned(fOnNodeMatrixPost) then begin
   fOnNodeMatrixPost(self,Node,InstanceNode,Matrix);
  end;
  Matrix:=Matrix*aMatrix;
  InstanceNode^.WorkMatrix:=Matrix;
  if assigned(Node.fMesh) then begin
   if SkinUsed then begin
    fSkins[Node.fSkin.Index].Used:=true;
   end;
  end;
{ if (Node^.Light>=0) and (Node^.Light<=length(fLightNodes)) then begin
   fLightNodes[Node^.Light]:=aNodeIndex;
  end;}
  for Index:=0 to Node.Children.Count-1 do begin
   ProcessNode(Node.Children[Index].Index,Matrix);
  end;
 end;
var Index:TPasGLTFSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    Animation:TpvScene3D.TGroup.TInstance.TAnimation;
begin
 Scene:=GetScene;
 if assigned(Scene) then begin
  //CurrentSkinShaderStorageBufferObjectHandle:=0;
  for Index:=0 to length(fLightNodes)-1 do begin
   fLightNodes[Index]:=-1;
  end;
  for Index:=0 to Scene.Nodes.Count-1 do begin
   ResetNode(Scene.Nodes[Index].Index);
  end;
  for Index:=0 to length(fSkins)-1 do begin
   fSkins[Index].Used:=false;
  end;
  for Index:=-1 to length(fAnimations)-2 do begin
   Animation:=fAnimations[Index+1];
   if Animation.fFactor>=-0.5 then begin
    if Index<0 then begin
     ProcessBaseOverwrite(Animation.fFactor);
    end else begin
     ProcessAnimation(Index,Animation.fTime,Animation.fFactor);
    end;
   end;
  end;
  for Index:=0 to Scene.fNodes.Count-1 do begin
   ProcessNode(Scene.fNodes[Index].Index,TpvMatrix4x4.Identity);
  end;
 end;
 fVulkanData:=fVulkanDatas[pvApplication.DrawFrameCounter mod MaxSwapChainImages];
 if assigned(fVulkanData) then begin
  fVulkanData.Update;
 end;
end;

{ TpvScene3D }

constructor TpvScene3D.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin

 inherited Create(aResourceManager,aParent);

 fLock:=TPasMPSpinLock.Create;

 fUploaded:=false;

 fTechniques:=TpvTechniques.Create;

 fImageListLock:=TPasMPSlimReaderWriterLock.Create;
 fImages:=TImages.Create;
 fImages.OwnsObjects:=false;

 fImageHashMap:=TImageHashMap.Create(nil);

 fSamplerListLock:=TPasMPSlimReaderWriterLock.Create;
 fSamplers:=TSamplers.Create;
 fSamplers.OwnsObjects:=false;

 fSamplerHashMap:=TSamplerHashMap.Create(nil);

 fTextureListLock:=TPasMPSlimReaderWriterLock.Create;
 fTextures:=TTextures.Create;
 fTextures.OwnsObjects:=false;

 fTextureHashMap:=TTextureHashMap.Create(nil);

 fMaterialListLock:=TPasMPSlimReaderWriterLock.Create;
 fMaterials:=TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 fMaterialHashMap:=TMaterialHashMap.Create(nil);

 fGroupListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroups:=TGroups.Create;
 fGroups.OwnsObjects:=false;

 fGroupInstanceListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroupInstances:=TGroup.TInstances.Create;
 fGroupInstances.OwnsObjects:=false;

 fWhiteTexture:=nil;

 fWhiteTextureLock:=TPasMPSlimReaderWriterLock.Create;

 fDefaultNormalMapTexture:=nil;

 fDefaultNormalMapTextureLock:=TPasMPSlimReaderWriterLock.Create;

 ReleaseFrameDelay:=MaxSwapChainImages+1;

 fMaterialVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fMaterialVulkanDescriptorSetLayout.AddBinding(0,
                                               VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                               1,
                                               TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                               []);
 fMaterialVulkanDescriptorSetLayout.AddBinding(1,
                                               VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                               9,
                                               TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                               []);
 fMaterialVulkanDescriptorSetLayout.Initialize;

end;

destructor TpvScene3D.Destroy;
begin

 Unload;

 FreeAndNil(fMaterialVulkanDescriptorSetLayout);

 FreeAndNil(fWhiteTexture);

 FreeAndNil(fDefaultNormalMapTexture);

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
 FreeAndNil(fMaterialHashMap);
 FreeAndNil(fMaterialListLock);

 while fTextures.Count>0 do begin
  fTextures[fTextures.Count-1].Free;
 end;
 FreeAndNil(fTextures);
 FreeAndNil(fTextureHashMap);
 FreeAndNil(fTextureListLock);

 while fSamplers.Count>0 do begin
  fSamplers[fSamplers.Count-1].Free;
 end;
 FreeAndNil(fSamplers);
 FreeAndNil(fSamplerHashMap);
 FreeAndNil(fSamplerListLock);

 while fImages.Count>0 do begin
  fImages[fImages.Count-1].Free;
 end;
 FreeAndNil(fImages);
 FreeAndNil(fImageHashMap);
 FreeAndNil(fImageListLock);

 FreeAndNil(fTechniques);

 FreeAndNil(fWhiteTextureLock);

 FreeAndNil(fDefaultNormalMapTextureLock);

 FreeAndNil(fLock);

 inherited Destroy;
end;

procedure TpvScene3D.UploadWhiteTexture;
const Pixel:TpvUInt32=TpvUInt32($ffffffff);
var GraphicsQueue:TpvVulkanQueue;
    GraphicsCommandPool:TpvVulkanCommandPool;
    GraphicsCommandBuffer:TpvVulkanCommandBuffer;
    GraphicsFence:TpvVulkanFence;
begin
 if not assigned(fWhiteTexture) then begin
  fWhiteTextureLock.Acquire;
  try
   if not assigned(fWhiteTexture) then begin
    GraphicsQueue:=pvApplication.VulkanDevice.GraphicsQueue;
    try
     GraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                      pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                      TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
     try
      GraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(GraphicsCommandPool,
                                                           VK_COMMAND_BUFFER_LEVEL_PRIMARY);
      try
       GraphicsFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
       try
        fWhiteTexture:=TpvVulkanTexture.CreateFromMemory(pvApplication.VulkanDevice,
                                                         GraphicsQueue,
                                                         GraphicsCommandBuffer,
                                                         GraphicsFence,
                                                         GraphicsQueue,
                                                         GraphicsCommandBuffer,
                                                         GraphicsFence,
                                                         VK_FORMAT_R8G8B8A8_UNORM,
                                                         VK_SAMPLE_COUNT_1_BIT,
                                                         1,
                                                         1,
                                                         1,
                                                         0,
                                                         1,
                                                         0,
                                                         [TpvVulkanTextureUsageFlag.General,
                                                          TpvVulkanTextureUsageFlag.TransferDst,
                                                          TpvVulkanTextureUsageFlag.TransferSrc,
                                                          TpvVulkanTextureUsageFlag.Sampled],
                                                         @Pixel,
                                                         SizeOf(TpvUInt32),
                                                         false,
                                                         false,
                                                         0,
                                                         true);
        fWhiteTexture.UpdateSampler;
       finally
        FreeAndNil(GraphicsFence);
       end;
      finally
       FreeAndNil(GraphicsCommandBuffer);
      end;
     finally
      FreeAndNil(GraphicsCommandPool);
     end;
    finally
     GraphicsQueue:=nil;
    end;
   end;
  finally
   fWhiteTextureLock.Release;
  end;
 end;
end;

procedure TpvScene3D.UploadDefaultNormalMapTexture;
const Pixel:TpvUInt32=TpvUInt32($80808080);
var GraphicsQueue:TpvVulkanQueue;
    GraphicsCommandPool:TpvVulkanCommandPool;
    GraphicsCommandBuffer:TpvVulkanCommandBuffer;
    GraphicsFence:TpvVulkanFence;
begin
 if not assigned(fDefaultNormalMapTexture) then begin
  fDefaultNormalMapTextureLock.Acquire;
  try
   if not assigned(fDefaultNormalMapTexture) then begin
    GraphicsQueue:=pvApplication.VulkanDevice.GraphicsQueue;
    try
     GraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                      pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                      TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
     try
      GraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(GraphicsCommandPool,
                                                           VK_COMMAND_BUFFER_LEVEL_PRIMARY);
      try
       GraphicsFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);
       try
        fDefaultNormalMapTexture:=TpvVulkanTexture.CreateFromMemory(pvApplication.VulkanDevice,
                                                                    GraphicsQueue,
                                                                    GraphicsCommandBuffer,
                                                                    GraphicsFence,
                                                                    GraphicsQueue,
                                                                    GraphicsCommandBuffer,
                                                                    GraphicsFence,
                                                                    VK_FORMAT_R8G8B8A8_UNORM,
                                                                    VK_SAMPLE_COUNT_1_BIT,
                                                                    1,
                                                                    1,
                                                                    1,
                                                                    0,
                                                                    1,
                                                                    0,
                                                                    [TpvVulkanTextureUsageFlag.General,
                                                                     TpvVulkanTextureUsageFlag.TransferDst,
                                                                     TpvVulkanTextureUsageFlag.TransferSrc,
                                                                     TpvVulkanTextureUsageFlag.Sampled],
                                                                    @Pixel,
                                                                    SizeOf(TpvUInt32),
                                                                    false,
                                                                    false,
                                                                    0,
                                                                    true);
       finally
        FreeAndNil(GraphicsFence);
       end;
      finally
       FreeAndNil(GraphicsCommandBuffer);
      end;
     finally
      FreeAndNil(GraphicsCommandPool);
     end;
    finally
     GraphicsQueue:=nil;
    end;
   end;
  finally
   fDefaultNormalMapTextureLock.Release;
  end;
 end;
end;

procedure TpvScene3D.Upload;
var Group:TGroup;
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try
     UploadWhiteTexture;
     for Group in fGroups do begin
      Group.Upload;
     end;
    finally
     fUploaded:=true;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.Unload;
var Group:TGroup;
    Material:TMaterial;
    Texture:TTexture;
    Sampler:TSampler;
    Image:TImage;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     for Group in fGroups do begin
      Group.Unload;
     end;
     for Material in fMaterials do begin
      Material.Unload;
     end;
     for Texture in fTextures do begin
      Texture.Unload;
     end;
     for Sampler in fSamplers do begin
      Sampler.Unload;
     end;
     for Image in fImages do begin
      Image.Unload;
     end;
     FreeAndNil(fWhiteTexture);
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.Draw(const aViewMatrix,aProjectionMatrix:TpvMatrix4x4;const aViewRect:TpvRect);
begin

end;

initialization
end.

