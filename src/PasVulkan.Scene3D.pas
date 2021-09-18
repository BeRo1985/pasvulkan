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
     PasVulkan.Utils,
     PasVulkan.Math,
     PasVulkan.Hash.SHA3,
     PasVulkan.Collections,
     PasVulkan.HighResolutionTimer,
     PasVulkan.Resources,
     PasVulkan.Techniques,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.BVH.DynamicAABBTree,
     PasVulkan.BVH.StaticAABBTree,
     PasVulkan.Frustum;

type EpvScene3D=class(Exception);

     { TpvScene3D }

     TpvScene3D=class(TpvResource)
      public
       const MaxVisibleLights=65536;
             // Light cluster index 3D grid size = ceil(CanvasWidth/64) x ceil(CanvasHeight/64) x 16
             LightClusterTileWidthBits=6;
             LightClusterTileHeightBits=6;
             LightClusterTileWidth=1 shl LightClusterTileWidthBits;
             LightClusterTileHeight=1 shl LightClusterTileHeightBits;
             LightClusterTileWidthMask=LightClusterTileWidth-1;
             LightClusterTileHeightMask=LightClusterTileHeight-1;
             // LightClusterGridWidth:=(CanvasWidth+LightClusterTileWidthMask) shr LightClusterTileWidthBits;
             // LightClusterGridHeight:=(CanvasHeight+LightClusterTileHeightMask) shr LightClusterTileHeightBits;
             LightClusterGridDepth=16;
             LightClusterGridHashBits=16;
             LightClusterGridHashSize=1 shl LightClusterGridHashBits;
             LightClusterGridHashMask=LightClusterGridHashSize-1;
       type TPrimitiveTopology=VK_PRIMITIVE_TOPOLOGY_POINT_LIST..VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN;
            TDoubleSided=boolean;
            TGraphicsPipelines=array[TPrimitiveTopology,TDoubleSided] of TpvVulkanPipeline;
            TVertexAttributeBindingLocations=class
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
            TVkPrimitiveTopologySet=set of TVkPrimitiveTopology;
            TUInt32Vector4=array[0..3] of TpvUInt32;
            TUInt16Vector4=array[0..3] of TpvUInt16;
            TInt16Vector2=array[0..1] of TpvInt16;
            TInt16Vector3=array[0..2] of TpvInt16;
            TInt16Vector4=array[0..3] of TpvInt16;
            PUInt32Vector4=^TUInt32Vector4;
            TMatrix4x4DynamicArray=TpvDynamicArray<TpvMatrix4x4>;
            TSizeIntDynamicArray=TpvDynamicArray<TpvSizeInt>;
            TSizeIntDynamicArrayEx=array of TpvSizeInt;
            TViewPortGlobalsUniformBuffer=record
             ViewMatrix:TpvMatrix4x4;
             ProjectionMatrix:TpvMatrix4x4;
            end;
            PViewPortGlobalsUniformBuffer=^TViewPortGlobalsUniformBuffer;
            TVertexStagePushConstants=record
             ViewMatrix:TpvMatrix4x4;
             ProjectionMatrix:TpvMatrix4x4;
            end;
            PVertexStagePushConstants=^TVertexStagePushConstants;
            TVertex=packed record                    // Minimum required vertex structure for to be GLTF 2.0 conformant
             case boolean of
              false:(
               Position:TpvVector3;                  //  12   12 (32-bit float 3D vector)
               NodeIndex:TpvUInt32;                  // + 4 = 16 (unsigned 32-bit node index)
               Normal:TInt16Vector2;                 // + 4 = 20 (signed 16-bit oct-encoded normal)
               Tangent:TInt16Vector2;                // + 4 = 24 (signed 16-bit oct-encoded tangent)
               TexCoord0:TpvVector2;                 // + 8 = 32 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               TexCoord1:TpvVector2;                 // + 8 = 40 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               Color0:TpvHalfFloatVector4;           // + 8 = 48 (must be at least half-float for HDR)
               MorphTargetVertexBaseIndex:TpvUInt32; // + 4 = 52 (unsigned 32-bit morph target vertex base index)
               JointBlockBaseIndex:TpvUInt32;        // + 4 = 56 (unsigned 32-bit joint block base index)
               CountJointBlocks:TpvUInt32;           // + 4 = 60 (unsigned 32-bit count of joint blocks)
               Flags:TpvUInt32;                      // + 4 = 64 (unsigned 32-bit dflags)
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
                    SpecularFactor:TpvFloat;
                    SpecularTexture:TTextureReference;
                    SpecularColorFactor:TpvVector3;
                    SpecularColorTexture:TTextureReference;
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
                      IOR:TpvVector4;
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
                    IOR:TpvFloat;
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
                      SpecularFactor:1.0;
                      SpecularTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      SpecularColorFactor:(x:1.0;y:1.0;z:1.0);
                      SpecularColorTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
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
                     IOR:1.5;
                    );
                   DefaultShaderData:TShaderData=
                    (
                     BaseColorFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SpecularFactor:(x:1.0;y:1.0;z:1.0;w:0.0);
                     EmissiveFactor:(x:0.0;y:0.0;z:0.0;w:0.0);
                     MetallicRoughnessNormalScaleOcclusionStrengthFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SheenColorFactorSheenIntensityFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     ClearcoatFactorClearcoatRoughnessFactor:(x:0.0;y:0.0;z:1.0;w:1.0);
                     ior:(x:1.5;y:0.0;z:0.0;w:0.0);
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
            TLightData=record
             public
              type TType=
                    (
                     None=0,
                     Directional=1,
                     Point=2,
                     Spot=3
                    );
             private
              fType_:TType;
              fIntensity:TpvFloat;
              fRange:TpvFloat;
              fInnerConeAngle:TpvFloat;
              fOuterConeAngle:TpvFloat;
              fColor:TpvVector3;
              fCastShadows:boolean;
              fVisible:boolean;
              fAlways:boolean;
             public
              property Type_:TType read fType_ write fType_;
              property Intensity:TpvFloat read fIntensity write fIntensity;
              property Range:TpvFloat read fRange write fRange;
              property InnerConeAngle:TpvFloat read fInnerConeAngle write fInnerConeAngle;
              property OuterConeAngle:TpvFloat read fOuterConeAngle write fOuterConeAngle;
              property Color:TpvVector3 read fColor write fColor;
              property CastShadows:boolean read fCastShadows write fCastShadows;
              property Visible:boolean read fVisible write fVisible;
              property Always:boolean read fAlways write fAlways;
            end;
            TLightItem=packed record
             // uvec4 MetaData; begin
              Type_:TpvUInt32;
              ShadowMapIndex:TpvUInt32;
{             InnerConeCosinus:TpvFloat;
              OuterConeCosinus:TpvFloat;}
              LightAngleScale:TpvFloat;
              LightAngleOffset:TpvFloat;
             // uvec4 MetaData; end
             ColorIntensity:TpvVector4; // XYZ = Color RGB, W = Intensity
             PositionRange:TpvVector4; // XYZ = Position, W = Range
             DirectionZFar:TpvVector4; // XYZ = Direction, W = Unused
             ShadowMapMatrix:TpvMatrix4x4;
            end;
            PLightItem=^TLightItem;
            TLightItems=TpvDynamicArray<TLightItem>;
            { TLightBuffer }
            TLightBuffer=class
             private
              fSceneInstance:TpvScene3D;
              fUploaded:TPasMPBool32;
              fLightItems:TLightItems;
              fLightAABBTreeGeneration:TpvUInt32;
              fLightTree:TpvBVHDynamicAABBTree.TGPUSkipListNodeArray;
              fLightItemsVulkanBuffer:TpvVulkanBuffer;
              fLightTreeVulkanBuffer:TpvVulkanBuffer;
             public
              constructor Create(const aSceneInstance:TpvScene3D); reintroduce;
              destructor Destroy; override;
              procedure Upload;
              procedure Unload;
              procedure Update(const aSwapChainImageIndex:TpvSizeInt);
            end;
            TLightBuffers=array[0..MaxSwapChainImages+1] of TLightBuffer;
            { TLight }
            TLight=class
             public
              type TType=
                    (
                     None=0,
                     Directional=1,
                     Point=2,
                     Spot=3
                    );
             private
              fSceneInstance:TpvScene3D;
              fVisible:boolean;
              fData:TpvScene3D.TLightData;
              fShadowMapIndex:TpvInt32;
              fPosition:TpvVector3;
              fDirection:TpvVector3;
              fMatrix:TpvMatrix4x4;
              fViewSpacePosition:TpvVector3;
              fBoundingBox:TpvAABB;
              fBoundingSphere:TpvSphere;
              fScissorRect:TpvFloatClipRect;
              fAABBTreeProxy:TpvSizeInt;
              fLightItemIndex:TpvSizeInt;
             public
              constructor Create(const aSceneInstance:TpvScene3D); reintroduce;
              destructor Destroy; override;
              procedure Assign(const aFrom:TpvScene3D.TLightData);
              procedure Update;
             public
              property Data:TpvScene3D.TLightData read fData write fData;
              property Type_:TpvScene3D.TLightData.TType read fData.fType_ write fData.fType_;
              property Intensity:TpvFloat read fData.fIntensity write fData.fIntensity;
              property Range:TpvFloat read fData.fRange write fData.fRange;
              property InnerConeAngle:TpvFloat read fData.fInnerConeAngle write fData.fInnerConeAngle;
              property OuterConeAngle:TpvFloat read fData.fOuterConeAngle write fData.fOuterConeAngle;
              property Color:TpvVector3 read fData.fColor write fData.fColor;
              property CastShadows:boolean read fData.fCastShadows write fData.fCastShadows;
             published
              property ShadowMapIndex:TpvInt32 read fShadowMapIndex write fShadowMapIndex;
             public
              property Visible:boolean read fVisible write fVisible;
              property Matrix:TpvMatrix4x4 read fMatrix write fMatrix;
              property ViewSpacePosition:TpvVector3 read fViewSpacePosition write fViewSpacePosition;
            end;
            TLights=TpvObjectGenericList<TpvScene3D.TLight>;
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
                      Position:TpvVector4;               //  16   16
                      Normal:TpvVector4;                 // +16   32
                      Tangent:TpvVector4;                // +16   48
                      // uvec4 metaData begin
                       Index:TpvUInt32;                  // + 4   52
                       Next:TpvUInt32;                   // + 4   56
                       Reserved0:TpvUInt32;              // + 4   60
                       Reserved1:TpvUInt32;              // + 4   64
                      // uvec4 metaData end
                     );                                  //  ==   ==
                     true:(                              //  64   64 per vertex
                      Padding:array[0..63] of TpvUInt8;
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
                                 InputTimeArray:TpvDoubleDynamicArray;
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
                     function GetAnimationBeginTime:TpvDouble;
                     function GetAnimationEndTime:TpvDouble;
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
                                         Normal:TpvVector3;
                                         Tangent:TpvVector3;
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
                     function CreateNodeMeshInstance(const aNodeIndex,aWeightsOffset,aJointNodeOffset:TpvUInt32):TpvSizeInt;
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
                     fJointMatrixOffset:TPasGLTFSizeInt;
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
                    private
                     fData:TLightData;
                     fIndex:TpvSizeInt;
                     fNodes:TNodes;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceLight:TPasJSONItemObject);
                    public
                     property Data:TLightData read fData write fData;
                    published
                     property Group:TGroup read fGroup write fGroup;
                     property Index:TpvSizeInt read fIndex;
                     property Nodes:TNodes read fNodes write fNodes;
                   end;
                   TLights=TpvObjectGenericList<TpvScene3D.TGroup.TLight>;
                   TNode=class(TGroupObject)
                    public
                     type TChildNodeIndices=TpvDynamicArray<TpvSizeInt>;
                    private
                     fIndex:TpvSizeInt;
                     fChildNodeIndices:TChildNodeIndices;
                     fChildren:TNodes;
                     fMesh:TMesh;
                     fNodeMeshInstanceIndex:TPasGLTFSizeInt;
                     fCamera:TCamera;
                     fSkin:TSkin;
                     fLight:TpvScene3D.TGroup.TLight;
                     fWeights:TpvFloatDynamicArray;
                     fWeightsOffset:TPasGLTFSizeInt;
                     fJoint:TPasGLTFSizeInt;
                     fMatrix:TpvMatrix4x4;
                     fTranslation:TpvVector3;
                     fRotation:TpvQuaternion;
                     fScale:TpvVector3;
                     fShaderStorageBufferObjectOffset:TpvSizeInt;
                     fShaderStorageBufferObjectSize:TpvSizeInt;
                     procedure Finish;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceNode:TPasGLTF.TNode;const aLightMap:TpvScene3D.TGroup.TLights);
                    published
                     property Index:TpvSizeInt read fIndex;
                     property Children:TNodes read fChildren;
                     property Camera:TCamera read fCamera;
                     property Mesh:TMesh read fMesh;
                     property NodeMeshInstanceIndex:TPasGLTFSizeInt read fNodeMeshInstanceIndex;
                     property Skin:TSkin read fSkin;
                   end;
                   TScene=class(TGroupObject)
                    public
                     type { TMaterial }
                          TMaterial=class
                           public
                            type TPrimitiveIndexRange=record
                                  PrimitiveTopology:TPrimitiveTopology;
                                  Index:TpvSizeInt;
                                  Count:TpvSizeInt;
                                  Node:TpvSizeInt;
                                 end;
                                 PPrimitiveIndexRange=^TPrimitiveIndexRange;
                                 TPrimitiveIndexRanges=TpvDynamicArray<TpvScene3D.TGroup.TScene.TMaterial.TPrimitiveIndexRange>;
                                 PPrimitiveIndexRanges=^TPrimitiveIndexRanges;
                           private
                            fMaterial:TpvScene3D.TMaterial;
                            fPrimitiveIndexRanges:TPrimitiveIndexRanges;
                            fCombinedPrimitiveIndexRanges:TPrimitiveIndexRanges;
                            fStartIndex:TpvSizeInt;
                            fCountIndices:TpvSizeInt;
                           public
                            constructor Create; reintroduce;
                            destructor Destroy; override;
                          end;
                          TMaterials=class(TpvObjectGenericList<TpvScene3D.TGroup.TScene.TMaterial>);
                          TMaterialHashMap=class(TpvHashMap<TpvScene3D.TMaterial,TpvScene3D.TGroup.TScene.TMaterial>);
                    private
                     fIndex:TpvSizeInt;
                     fNodes:TNodes;
                     fMaterials:TpvScene3D.TGroup.TScene.TMaterials;
                     fMaterialHashMap:TpvScene3D.TGroup.TScene.TMaterialHashMap;
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
                           public
                            type TChannel=class
                                  private
                                   fNode:Pointer;
                                   fOverwrite:TpvSizeInt;
                                 end;
                                 TChannels=TpvObjectGenericList<TChannel>;
                           private
                            fFactor:TPasGLTFFloat;
                            fTime:TPasGLTFDouble;
                            fChannels:TChannels;
                           public
                            constructor Create; reintroduce;
                            destructor Destroy; override;
                           published
                            property Factor:TPasGLTFFloat read fFactor write fFactor;
                            property Time:TPasGLTFDouble read fTime write fTime;
                          end;
                          TAnimations=array of TpvScene3D.TGroup.TInstance.TAnimation;
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
                                   Rotation:TpvQuaternion;
                                   Scale:TpvVector3;
                                   Weights:TpvFloatDynamicArray;
                                   Factor:TpvFloat;
                                 end;
                                 POverwrite=^TOverwrite;
                                 TOverwrites=array of TOverwrite;
                           public
                            Processed:LongBool;
                            Overwrites:TOverwrites;
                            CountOverwrites:TpvSizeInt;
                            OverwriteFlags:TOverwriteFlags;
                            OverwriteTranslation:TpvVector3;
                            OverwriteRotation:TpvQuaternion;
                            OverwriteScale:TpvVector3;
                            OverwriteWeights:TpvFloatDynamicArray;
                            OverwriteWeightsSum:TpvDoubleDynamicArray;
                            WorkWeights:TpvFloatDynamicArray;
                            WorkMatrix:TpvMatrix4x4;
                            VisibleBitmap:TpvUInt32;
                            Light:TpvScene3D.TLight;
                            BoundingBoxes:array[0..MaxSwapChainImages+1] of TpvAABB;
                            BoundingBoxFilled:array[0..MaxSwapChainImages+1] of boolean;
                          end;
                          TInstanceNode=TpvScene3D.TGroup.TInstance.TNode;
                          PNode=^TInstanceNode;
                          TNodes=array of TpvScene3D.TGroup.TInstance.TNode;
                          TSkin=record
                           Used:boolean;
                          end;
                          TInstanceSkin=TpvScene3D.TGroup.TInstance.TSkin;
                          PSkin=^TInstanceSkin;
                          TSkins=array of TpvScene3D.TGroup.TInstance.TSkin;
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
                            procedure Update(const aSwapChainImageIndex:TpvSizeInt);
                           published
                            property NodeMatricesBuffer:TpvVulkanBuffer read fNodeMatricesBuffer;
                            property MorphTargetVertexWeightsBuffer:TpvVulkanBuffer read fMorphTargetVertexWeightsBuffer;
                          end;
                          TVulkanDatas=array[0..MaxSwapChainImages+1] of TVulkanData;
                    private
                     fGroup:TGroup;
                     fActive:boolean;
                     fPreviousActive:boolean;
                     fScene:TPasGLTFSizeInt;
                     fAnimations:TpvScene3D.TGroup.TInstance.TAnimations;
                     fNodes:TpvScene3D.TGroup.TInstance.TNodes;
                     fSkins:TpvScene3D.TGroup.TInstance.TSkins;
                     fLightNodes:TNodeIndices;
                     fLightShadowMapMatrices:TPasGLTF.TMatrix4x4DynamicArray;
                     fLightShadowMapZFarValues:TPasGLTFFloatDynamicArray;
                     fBoundingBox:TpvAABB;
                     fWorstCaseStaticBoundingBox:TpvAABB;
                     fUserData:pointer;
                     fOnNodeMatrixPre:TOnNodeMatrix;
                     fOnNodeMatrixPost:TOnNodeMatrix;
                     fUploaded:boolean;
                     fModelMatrix:TpvMatrix4x4;
                     fNodeMatrices:TNodeMatrices;
                     fMorphTargetVertexWeights:TMorphTargetVertexWeights;
                     fVulkanDatas:TVulkanDatas;
                     fVulkanData:TVulkanData;
                     fVulkanDescriptorPool:TpvVulkanDescriptorPool;
                     fVulkanDescriptorSets:array[0..MaxSwapChainImages+1] of TpvVulkanDescriptorSet;
                     fScenes:array[0..MaxSwapChainImages+1] of TpvScene3D.TGroup.TScene;
                     fActives:array[0..MaxSwapChainImages+1] of boolean;
                     fAABBTreeProxy:TpvSizeInt;
                     fVisibleBitmap:TPasMPUInt32;
                     function GetAutomation(const aIndex:TPasGLTFSizeInt):TpvScene3D.TGroup.TInstance.TAnimation;
                     procedure SetScene(const aScene:TpvSizeInt);
                     function GetScene:TpvScene3D.TGroup.TScene;
                     procedure Prepare(const aSwapChainImageIndex:TpvSizeInt;
                                       const aRenderPassIndex:TpvSizeInt;
                                       const aFrustum:TpvFrustum;
                                       const aFrustumCulling:boolean);
                     procedure Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                                    const aSwapChainImageIndex:TpvSizeInt;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aCommandBuffer:TpvVulkanCommandBuffer;
                                    var aPipeline:TpvVulkanPipeline;
                                    const aPipelineLayout:TpvVulkanPipelineLayout;
                                    const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
                    public
                     constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
                     destructor Destroy; override;
                     procedure AfterConstruction; override;
                     procedure BeforeDestruction; override;
                     procedure Upload; override;
                     procedure Unload; override;
                     procedure UpdateInvisible;
                     procedure Update(const aSwapChainImageIndex:TpvSizeInt);
                    published
                     property Group:TGroup read fGroup write fGroup;
                     property Active:boolean read fActive write fActive;
                     property Scene:TpvSizeInt read fScene write SetScene;
                    public
                     property Nodes:TpvScene3D.TGroup.TInstance.TNodes read fNodes;
                     property Skins:TpvScene3D.TGroup.TInstance.TSkins read fSkins;
                     property UserData:pointer read fUserData write fUserData;
                     property ModelMatrix:TpvMatrix4x4 read fModelMatrix write fModelMatrix;
                    published
                     property VulkanData:TVulkanData read fVulkanData;
                    public
                     property Automations[const aIndex:TPasGLTFSizeInt]:TpvScene3D.TGroup.TInstance.TAnimation read GetAutomation;
                    published
                     property OnNodeMatrixPre:TOnNodeMatrix read fOnNodeMatrixPre write fOnNodeMatrixPre;
                     property OnNodeMatrixPost:TOnNodeMatrix read fOnNodeMatrixPost write fOnNodeMatrixPost;
                   end;
                   TInstances=TpvObjectGenericList<TInstance>;
             private
              fCulling:boolean;
              fObjects:TBaseObjects;
              fAnimations:TAnimations;
              fCameras:TCameras;
              fMeshes:TMeshes;
              fSkins:TSkins;
              fLights:TpvScene3D.TGroup.TLights;
              fNodes:TNodes;
              fScenes:TScenes;
              fScene:TScene;
              fVertices:TGroupVertices;
              fIndices:TGroupIndices;
              fPerMaterialCondensedIndices:TGroupIndices;
              fJointBlocks:TGroupJointBlocks;
              fJointBlockOffsets:TSizeIntDynamicArrayEx;
              fMorphTargetVertices:TMorphTargetVertexDynamicArray;
              fSkinStorageBufferSize:TpvSizeInt;
              fMorphTargetCount:TpvSizeInt;
              fCountNodeWeights:TpvSizeInt;
              fCountJointNodeMatrices:TpvSizeInt;
              fNodeShaderStorageBufferObject:TNodeShaderStorageBufferObject;
              fLock:TPasMPSpinLock;
              fVulkanVertexBuffer:TpvVulkanBuffer;
              //fVulkanIndexBuffer:TpvVulkanBuffer;
              fVulkanMaterialIndexBuffer:TpvVulkanBuffer;
              fVulkanMorphTargetVertexBuffer:TpvVulkanBuffer;
              fVulkanJointBlockBuffer:TpvVulkanBuffer;
              fInstanceListLock:TPasMPSlimReaderWriterLock;
              fInstances:TInstances;
              fBoundingBox:TpvAABB;
              procedure ConstructBuffers;
              procedure CollectMaterialPrimitives;
              procedure Prepare(const aSwapChainImageIndex:TpvSizeInt;
                                const aRenderPassIndex:TpvSizeInt;
                                const aFrustum:TpvFrustum;
                                const aFrustumCulling:boolean);
              procedure Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                             const aSwapChainImageIndex:TpvSizeInt;
                             const aRenderPassIndex:TpvSizeInt;
                             const aCommandBuffer:TpvVulkanCommandBuffer;
                             var aPipeline:TpvVulkanPipeline;
                             const aPipelineLayout:TpvVulkanPipelineLayout;
                             const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure Update(const aSwapChainImageIndex:TpvSizeInt);
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument);
              function CreateInstance:TpvScene3D.TGroup.TInstance;
             public
              property BoundingBox:TpvAABB read fBoundingBox;
             published
              property Culling:boolean read fCulling write fCulling;
              property Objects:TBaseObjects read fObjects;
              property Animations:TAnimations read fAnimations;
              property Cameras:TCameras read fCameras;
              property Meshes:TMeshes read fMeshes;
              property Skins:TSkins read fSkins;
              property Lights:TpvScene3D.TGroup.TLights read fLights;
              property Nodes:TNodes read fNodes;
              property Scenes:TScenes read fScenes;
              property Scene:TScene read fScene;
            end;
            TGroups=TpvObjectGenericList<TGroup>;
            TImageHashMap=TpvHashMap<TImage.THashData,TImage>;
            TSamplerHashMap=TpvHashMap<TSampler.THashData,TSampler>;
            TTextureHashMap=TpvHashMap<TTexture.THashData,TTexture>;
            TMaterialHashMap=TpvHashMap<TMaterial.THashData,TMaterial>;
            TBufferMemoryBarriers=TpvDynamicArray<TVkBufferMemoryBarrier>;
            TSwapChainImageBufferMemoryBarriers=array[0..MaxSwapChainImages+1] of TBufferMemoryBarriers;
      private
       fLock:TPasMPSpinLock;
       fUploaded:TPasMPBool32;
       fWhiteTexture:TpvVulkanTexture;
       fWhiteTextureLock:TPasMPSlimReaderWriterLock;
       fDefaultNormalMapTexture:TpvVulkanTexture;
       fDefaultNormalMapTextureLock:TPasMPSlimReaderWriterLock;
       fMeshVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fMaterialVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fGlobalVulkanDescriptorSets:array[0..MaxSwapChainImages+1] of TpvVulkanDescriptorSet;
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
       fLights:array[0..MaxSwapChainImages+1] of TpvScene3D.TLights;
       fCountLights:array[0..MaxSwapChainImages+1] of TpvSizeInt;
       fIndirectLights:array[0..MaxSwapChainImages+1,0..MaxVisibleLights-1] of TpvScene3D.TLight;
       fCountIndirectLights:array[0..MaxSwapChainImages+1] of TpvSizeInt;
       fGroupListLock:TPasMPSlimReaderWriterLock;
       fGroups:TGroups;
       fGroupInstanceListLock:TPasMPSlimReaderWriterLock;
       fGroupInstances:TGroup.TInstances;
       fLightAABBTree:TpvBVHDynamicAABBTree;
       fLightAABBTreeGeneration:TpvUInt32;
       fLightAABBTreeStates:array[0..MaxSwapChainImages+1] of TpvBVHDynamicAABBTree.TState;
       fLightAABBTreeStateGenerations:array[0..MaxSwapChainImages+1] of TpvUInt32;
       fLightBuffers:TpvScene3D.TLightBuffers;
       fAABBTree:TpvBVHDynamicAABBTree;
       fAABBTreeStates:array[0..MaxSwapChainImages+1] of TpvBVHDynamicAABBTree.TState;
       fBoundingBox:TpvAABB;
       fSwapChainImageBufferMemoryBarriers:TSwapChainImageBufferMemoryBarriers;
       procedure AddSwapChainImageBufferMemoryBarrier(const aSwapChainImageIndex:TpvSizeInt;
                                                      const aBuffer:TpvVulkanBuffer);
       procedure UploadWhiteTexture;
       procedure UploadDefaultNormalMapTexture;
       procedure CullAABBTreeWithFrustum(const aFrustum:TpvFrustum;
                                         const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                         const aRoot:TpvSizeInt;
                                         const aVisibleBit:TPasMPUInt32);
       procedure CullLightAABBTreeWithFrustum(const aSwapChainImageIndex:TpvSizeInt;
                                              const aFrustum:PpvFrustum;
                                              const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                              const aRoot:TpvSizeInt);
       procedure CollectLightAABBTreeLights(const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                            const aRoot:TpvSizeInt;
                                            var aLightItemArray:TpvScene3D.TLightItems);
       function GetLightUserDataIndex(const aUserData:TpvPtrInt):TpvUInt32;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
       destructor Destroy; override;
       procedure Upload;
       procedure Unload;
       procedure Update(const aSwapChainImageIndex:TpvSizeInt);
       procedure PrepareLights(const aSwapChainImageIndex:TpvSizeInt;
                               const aViewMatrix:TpvMatrix4x4;
                               const aProjectionMatrix:TpvMatrix4x4;
                               const aViewPortWidth:TpvInt32;
                               const aViewPortHeight:TpvInt32;
                               const aFrustum:TpvFrustum;
                               const aFrustumCulling:boolean=true);
       procedure Prepare(const aSwapChainImageIndex:TpvSizeInt;
                         const aRenderPassIndex:TpvSizeInt;
                         const aViewMatrix:TpvMatrix4x4;
                         const aProjectionMatrix:TpvMatrix4x4;
                         const aViewPortWidth:TpvInt32;
                         const aViewPortHeight:TpvInt32;
                         const aFrustumCulling:boolean=true);
       procedure Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                      const aSwapChainImageIndex:TpvSizeInt;
                      const aRenderPassIndex:TpvSizeInt;
                      const aViewMatrix:TpvMatrix4x4;
                      const aProjectionMatrix:TpvMatrix4x4;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aPipelineLayout:TpvVulkanPipelineLayout;
                      const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
       procedure GetZNearZFar(const aViewMatrix:TpvMatrix4x4;
                              const aAspectRatio:TpvScalar;
                              out aZNear:TpvScalar;
                              out aZFar:TpvScalar);
      public
       property BoundingBox:TpvAABB read fBoundingBox;
      published
       property MeshVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fMeshVulkanDescriptorSetLayout;
       property MaterialVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fMaterialVulkanDescriptorSetLayout;
       property GlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fGlobalVulkanDescriptorSetLayout;
     end;

implementation

const FlushUpdateData=false;

function OctEncode(const aVector:TpvVector3;const aFloorX,aFloorY:Boolean):TpvScene3D.TInt16Vector2; overload;
var Vector:TpvVector3;
    x,y,s,tx,ty:TpvScalar;
begin
 Vector:=aVector.Normalize;
 s:=abs(Vector.x)+abs(Vector.y)+abs(Vector.z);
 x:=Vector.x/s;
 y:=Vector.y/s;
 if Vector.z<0.0 then begin
  tx:=1.0-abs(y);
  if x<0.0 then begin
   tx:=-tx;
  end;
  ty:=1.0-abs(x);
  if y<0.0 then begin
   ty:=-ty;
  end;
  x:=tx;
  y:=ty;
 end;
 if aFloorX then begin
  result[0]:=Min(Max(trunc(Floor(x*32767.5)),-32768),32767);
 end else begin
  result[0]:=Min(Max(trunc(Ceil(x*32767.5)),-32768),32767);
 end;
 if aFloorY then begin
  result[1]:=Min(Max(trunc(Floor(y*32767.5)),-32768),32767);
 end else begin
  result[1]:=Min(Max(trunc(Ceil(y*32767.5)),-32768),32767);
 end;
end;

function OctDecode(const aOct:TpvScene3D.TInt16Vector2):TpvVector3;
var x,y,z,s,tx,ty:TpvScalar;
begin
 x:=aOct[0];
 y:=aOct[1];
 if x<0.0 then begin
  x:=x/32768.0;
 end else begin
  x:=x/32767.0;
 end;
 if y<0.0 then begin
  y:=y/32768.0;
 end else begin
  y:=y/32767.0;
 end;
 z:=(1.0-abs(x))-abs(y);
 if z<0 then begin
  tx:=1.0-abs(y);
  if x<0.0 then begin
   tx:=-tx;
  end;
  ty:=1.0-abs(x);
  if y<0.0 then begin
   ty:=-ty;
  end;
  x:=tx;
  y:=ty;
 end;
 result:=TpvVector3.Create(x,y,z).Normalize;
end;

function OctEncode(const aVector:TpvVector3):TpvScene3D.TInt16Vector2; overload;
var Vector:TpvVector3;
    Oct:TpvScene3D.TInt16Vector2;
    BestDot,Dot:TpvScalar;
begin

 Vector:=aVector.Normalize;

 result:=OctEncode(Vector,false,false);
 BestDot:=Vector.Dot(OctDecode(result));

 Oct:=OctEncode(Vector,false,true);
 Dot:=Vector.Dot(OctDecode(Oct));
 if BestDot>Dot then begin
  result:=Oct;
  BestDot:=Dot;
 end;

 Oct:=OctEncode(Vector,true,true);
 Dot:=Vector.Dot(OctDecode(Oct));
 if BestDot>Dot then begin
  result:=Oct;
  BestDot:=Dot;
 end;

 Oct:=OctEncode(Vector,true,false);
 Dot:=Vector.Dot(OctDecode(Oct));
 if BestDot>Dot then begin
  result:=Oct;
  BestDot:=Dot;
 end;

end;

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
                                       pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.maxSamplerAnisotropy>1.0,
                                       Max(1.0,pvApplication.VulkanDevice.PhysicalDevice.Properties.limits.maxSamplerAnisotropy),
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
  fImage:=nil;
//raise EPasGLTFInvalidDocument.Create('Image index out of range');
 end;
 if (aSourceTexture.Sampler>=0) and (aSourceTexture.Sampler<aSamplerMap.Count) then begin
  fSampler:=aSamplerMap[aSourceTexture.Sampler];
  if assigned(fSampler) then begin
   fSampler.IncRef;
  end;
 end else begin
  fSampler:=nil;
//raise EPasGLTFInvalidDocument.Create('Sampler index out of range');
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
 if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) then begin
  try
   fData.PBRMetallicRoughness.SpecularTexture.Texture.DecRef;
  finally
   fData.PBRMetallicRoughness.SpecularTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
  try
   fData.PBRMetallicRoughness.SpecularColorTexture.Texture.DecRef;
  finally
   fData.PBRMetallicRoughness.SpecularColorTexture.Texture:=nil;
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
   if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) then begin
    try
     fData.PBRMetallicRoughness.SpecularTexture.Texture.DecRef;
    finally
     fData.PBRMetallicRoughness.SpecularTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
    try
     fData.PBRMetallicRoughness.SpecularColorTexture.Texture.DecRef;
    finally
     fData.PBRMetallicRoughness.SpecularColorTexture.Texture:=nil;
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
    SpecularFactorTextureDescriptorImageInfo,
    SpecularColorFactorTextureDescriptorImageInfo,
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
       if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) then begin
        fData.PBRMetallicRoughness.SpecularTexture.Texture.Upload;
        SpecularFactorTextureDescriptorImageInfo:=fData.PBRMetallicRoughness.SpecularTexture.Texture.GetDescriptorImageInfo;
       end else begin
        SpecularFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       end;
       if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
        fData.PBRMetallicRoughness.SpecularColorTexture.Texture.Upload;
        SpecularColorFactorTextureDescriptorImageInfo:=fData.PBRMetallicRoughness.SpecularColorTexture.Texture.GetDescriptorImageInfo;
       end else begin
        SpecularColorFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
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
       SpecularFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       SpecularColorFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
      end;
      TpvScene3D.TMaterial.TShadingModel.Unlit:begin
       if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
        fData.PBRMetallicRoughness.BaseColorTexture.Texture.Upload;
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fData.PBRMetallicRoughness.BaseColorTexture.Texture.GetDescriptorImageInfo;
       end else begin
        BaseColorOrDiffuseTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       end;
       MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       SpecularFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       SpecularColorFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
      end;
      else begin
       BaseColorOrDiffuseTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       SpecularFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
       SpecularColorFactorTextureDescriptorImageInfo:=fSceneInstance.fWhiteTexture.DescriptorImageInfo;
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
     fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,11);
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
                                               11,
                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                               [BaseColorOrDiffuseTextureDescriptorImageInfo,                    // 0
                                                MetallicRoughnessOrSpecularGlossinessTextureDescriptorImageInfo, // 1
                                                NormalTextureDescriptorImageInfo,                                // 2
                                                OcclusionTextureDescriptorImageInfo,                             // 3
                                                EmissiveTextureDescriptorImageInfo,                              // 4
                                                SheenColorIntensityTextureDescriptorImageInfo,                   // 5
                                                ClearCoatNormalTextureDescriptorImageInfo,                       // 6
                                                ClearCoatRoughnessTextureDescriptorImageInfo,                    // 7
                                                ClearCoatTextureDescriptorImageInfo,                             // 8
                                                SpecularFactorTextureDescriptorImageInfo,                        // 9
                                                SpecularColorFactorTextureDescriptorImageInfo],                  // 10
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
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_specular'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fData.PBRMetallicRoughness.SpecularFactor:=TPasJSON.GetNumber(JSONObject.Properties['specularFactor'],1.0);
   JSONItem:=JSONObject.Properties['specularTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.PBRMetallicRoughness.SpecularTexture.Texture:=aTextureMap[Index];
     if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) then begin
      fData.PBRMetallicRoughness.SpecularTexture.Texture.IncRef;
     end;
    end else begin
     fData.PBRMetallicRoughness.SpecularTexture.Texture:=nil;
    end;
    fData.PBRMetallicRoughness.SpecularTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],fData.PBRMetallicRoughness.SpecularTexture.TexCoord);
    fData.PBRMetallicRoughness.SpecularTexture.Transform.AssignFromGLTF(fData.PBRMetallicRoughness.SpecularTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
   JSONItem:=JSONObject.Properties['specularColorFactor'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3) then begin
    fData.PBRMetallicRoughness.SpecularColorFactor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],fData.PBRSpecularGlossiness.DiffuseFactor[0]);
    fData.PBRMetallicRoughness.SpecularColorFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],fData.PBRSpecularGlossiness.DiffuseFactor[1]);
    fData.PBRMetallicRoughness.SpecularColorFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],fData.PBRSpecularGlossiness.DiffuseFactor[2]);
   end;
   JSONItem:=JSONObject.Properties['specularColorTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.PBRMetallicRoughness.SpecularColorTexture.Texture:=aTextureMap[Index];
     if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
      fData.PBRMetallicRoughness.SpecularColorTexture.Texture.IncRef;
     end;
    end else begin
     fData.PBRMetallicRoughness.SpecularColorTexture.Texture:=nil;
    end;
    fData.PBRMetallicRoughness.SpecularColorTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],fData.PBRMetallicRoughness.SpecularColorTexture.TexCoord);
    fData.PBRMetallicRoughness.SpecularColorTexture.Transform.AssignFromGLTF(fData.PBRMetallicRoughness.SpecularColorTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
  end;
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

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_ior'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   fData.IOR:=TPasJSON.GetNumber(JSONObject.Properties['ior'],1.5);
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
   if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) or
      assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
    fShaderData.Flags:=fShaderData.Flags or (1 shl 9);
   end;
   fShaderData.SpecularFactor:=TpvVector4.InlineableCreate(fData.PBRMetallicRoughness.SpecularColorFactor[0],fData.PBRMetallicRoughness.SpecularColorFactor[1],fData.PBRMetallicRoughness.SpecularColorFactor[2],fData.PBRMetallicRoughness.SpecularFactor);
   if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) then begin
    fShaderData.Textures1:=(fShaderData.Textures1 and not ($f shl (1 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.SpecularTexture.TexCoord and $f) shl (1 shl 2));
    fShaderData.TextureTransforms[9]:=fData.PBRMetallicRoughness.SpecularTexture.Transform.ToMatrix4x4;
   end;
   if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
    fShaderData.Textures1:=(fShaderData.Textures1 and not ($f shl (2 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.SpecularColorTexture.TexCoord and $f) shl (2 shl 2));
    fShaderData.TextureTransforms[10]:=fData.PBRMetallicRoughness.SpecularColorTexture.Transform.ToMatrix4x4;
   end;
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
   fShaderData.IOR[0]:=fData.IOR;
   fShaderData.IOR[1]:=0.0;
   fShaderData.IOR[2]:=0.0;
   fShaderData.IOR[3]:=0.0;
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

{ TpvScene3D.TLight }

constructor TpvScene3D.TLight.Create(const aSceneInstance:TpvScene3D);
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 fAABBTreeProxy:=-1;
end;

destructor TpvScene3D.TLight.Destroy;
begin
 if fAABBTreeProxy>=0 then begin
  try
   if assigned(fSceneInstance) then begin
    if assigned(fSceneInstance.fLightAABBTree) then begin
     fSceneInstance.fLightAABBTree.DestroyProxy(fAABBTreeProxy);
    end;
    TPasMPInterlocked.Increment(fSceneInstance.fLightAABBTreeGeneration);
   end;
  finally
   fAABBTreeProxy:=-1;
  end;
 end;
 inherited Destroy;
end;

procedure TpvScene3D.TLight.Assign(const aFrom:TpvScene3D.TLightData);
begin
 fData:=aFrom;
end;

procedure TpvScene3D.TLight.Update;
const DownZ:TpvVector3=(x:0.0;y:0.0;z:-1.0);
      LinearRGBLuminance:TpvVector3=(x:0.2126;y:0.7152;z:0.0722);
      MinLuminance=1e-3;
      Constant=0.0;
      Linear=0.0;
      Threshold=1e-3;
      Infinity=16777216.0;
var Position,Direction:TpvVector3;
    OBB:TpvOBB;
    AABB:TpvAABB;
    Radius,Luminance,OppositeLength:TpvScalar;
begin
 if fData.fVisible then begin
  Position:=(fMatrix*TpvVector3.Origin).xyz;
  Direction:=(((fMatrix*DownZ).xyz)-Position).Normalize;
  fPosition:=Position;
  fDirection:=Direction;
  case fData.Type_ of
   TpvScene3D.TLightData.TType.Point,
   TpvScene3D.TLightData.TType.Spot:begin
    if fData.fRange>1e-7 then begin
     // float distanceByRange = currentDistance / light.positionRange.w;
     // lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange * distanceByRange * distanceByRange), 0.0, 1.0) / (currentDistance * currentDistance);
     Radius:=fData.fRange;
    end else begin
     // lightAttenuation *= 1.0 / (currentDistance * currentDistance);
     Luminance:=fData.Color.Dot(LinearRGBLuminance);
     if Luminance>1e-7 then begin
      Radius:=Threshold/Luminance;
      if Radius>1e-7 then begin
       Radius:=1.0/sqrt(Radius);
      end else begin
       Radius:=EPSILON;
      end;
     end else begin
      Radius:=1.0;
     end;
    end;
   end;
   else begin
    Radius:=Infinity;
   end;
  end;
  case fData.Type_ of
   TpvScene3D.TLightData.TType.Directional:begin
    AABB.Min:=TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity);
    AABB.Max:=TpvVector3.InlineableCreate(Infinity,Infinity,Infinity);
   end;
   TpvScene3D.TLightData.TType.Point:begin
    AABB.Min:=Position-TpvVector3.InlineableCreate(Radius,Radius,Radius);
    AABB.Max:=Position+TpvVector3.InlineableCreate(Radius,Radius,Radius);
   end;
   TpvScene3D.TLightData.TType.Spot:begin
    OppositeLength:=Tan(fData.fOuterConeAngle{*0.5})*Radius;
    OBB.Center:=fMatrix*TpvVector3.InlineableCreate(0.0,0.0,-Radius*0.5);
    OBB.Extents:=TpvVector3.InlineableCreate(OppositeLength,OppositeLength,Radius*0.5);
    OBB.Matrix:=fMatrix.ToMatrix3x3;
    AABB:=TpvAABB.CreateFromOBB(OBB);
   end;
   else {TpvScene3D.TLightData.TType.None:}begin
    AABB.Min:=TpvVector3.InlineableCreate(Infinity,Infinity,Infinity);
    AABB.Max:=TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity);
   end;
  end;
  fBoundingBox:=AABB;
  fBoundingSphere:=TpvSphere.CreateFromAABB(AABB);
  if fAABBTreeProxy<0 then begin
   fAABBTreeProxy:=fSceneInstance.fLIghtAABBTree.CreateProxy(fBoundingBox,TpvPtrInt(Pointer(self)));
  end else begin
   fSceneInstance.fLightAABBTree.MoveProxy(fAABBTreeProxy,fBoundingBox,TpvVector3.Create(1.0,1.0,1.0));
  end;
  TPasMPInterlocked.Increment(fSceneInstance.fLightAABBTreeGeneration);
 end else begin
  if fAABBTreeProxy>=0 then begin
   try
    if assigned(fSceneInstance) and assigned(fSceneInstance.fLightAABBTree) then begin
     fSceneInstance.fLightAABBTree.DestroyProxy(fAABBTreeProxy);
     TPasMPInterlocked.Increment(fSceneInstance.fLightAABBTreeGeneration);
    end;
   finally
    fAABBTreeProxy:=-1;
   end;
  end;
 end;
end;

{ TpvScene3D.TLightBuffer }

constructor TpvScene3D.TLightBuffer.Create(const aSceneInstance:TpvScene3D);
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 fUploaded:=false;
 fLightTree.Initialize;
 fLightAABBTreeGeneration:=fSceneInstance.fLightAABBTreeGeneration-2;
end;

destructor TpvScene3D.TLightBuffer.Destroy;
begin
 Unload;
 fLightTree.Finalize;
 inherited Destroy;
end;

procedure TpvScene3D.TLightBuffer.Upload;
begin
 if not fUploaded then begin
  try
   FreeAndNil(fLightItemsVulkanBuffer);
   FreeAndNil(fLightTreeVulkanBuffer);
   fLightItemsVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                   MaxVisibleLights*SizeOf(TLightItem),
                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                   TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                   [],
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   [TpvVulkanBufferFlag.PersistentMapped]
                                                  );
   fLightTreeVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                  (MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TGPUSkipListNode),
                                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                  TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                  [],
                                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  [TpvVulkanBufferFlag.PersistentMapped]
                                                 );
  finally
   fUploaded:=true;
  end;
 end;
end;

procedure TpvScene3D.TLightBuffer.Unload;
begin
 if fUploaded then begin
  try
   FreeAndNil(fLightItemsVulkanBuffer);
   FreeAndNil(fLightTreeVulkanBuffer);
  finally
   fUploaded:=false;
  end;
 end;
end;

procedure TpvScene3D.TLightBuffer.Update(const aSwapChainImageIndex:TpvSizeInt);
const EmptyGPUSkipListNode:TpvBVHDynamicAABBTree.TGPUSkipListNode=
       (AABBMin:(x:0.0;y:0.0;z:0.0);
        SkipCount:0;
        AABBMax:(x:0.0;y:0.0;z:0.0);
        UserData:TpvUInt32($ffffffff)
       );
begin
 if fUploaded then begin
  if fLightAABBTreeGeneration<>fSceneInstance.fLightAABBTreeGeneration then begin
   fLightAABBTreeGeneration:=fSceneInstance.fLightAABBTreeGeneration;
   if fLightItems.Count>0 then begin
    fLightItemsVulkanBuffer.UpdateData(fLightItems.Items[0],0,Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TLightItem),FlushUpdateData);
   end;
   if fLightTree.Count>0 then begin
    fLightTreeVulkanBuffer.UpdateData(fLightTree.Items[0],0,Min(fLightTree.Count,MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TGPUSkipListNode),FlushUpdateData);
   end else begin
    fLightTreeVulkanBuffer.UpdateData(EmptyGPUSkipListNode,0,SizeOf(TpvBVHDynamicAABBTree.TGPUSkipListNode),FlushUpdateData);
   end;
{  fSceneInstance.AddSwapChainImageBufferMemoryBarrier(aSwapChainImageIndex,fLightItemsVulkanBuffer);
   fSceneInstance.AddSwapChainImageBufferMemoryBarrier(aSwapChainImageIndex,fLightTreeVulkanBuffer);}
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
    OutputScalar64Array:TPasGLTFDoubleDynamicArray;
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
    OutputScalar64Array:=aSourceDocument.Accessors[SourceAnimationSampler.Input].DecodeAsDoubleArray(false);
    try
     SetLength(DestinationAnimationChannel^.InputTimeArray,length(OutputScalar64Array));
     if length(OutputScalar64Array)>0 then begin
      Move(OutputScalar64Array[0],DestinationAnimationChannel^.InputTimeArray[0],length(OutputScalar64Array)*SizeOf(TpvDouble));
     end;
    finally
     OutputScalar64Array:=nil;
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

function TpvScene3D.TGroup.TAnimation.GetAnimationBeginTime:TpvDouble;
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

function TpvScene3D.TGroup.TAnimation.GetAnimationEndTime:TpvDouble;
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

function TpvScene3D.TGroup.TMesh.CreateNodeMeshInstance(const aNodeIndex,aWeightsOffset,aJointNodeOffset:TpvUInt32):TpvSizeInt;
var PrimitiveIndex,
    NodeMeshPrimitiveInstanceIndex,
    VertexIndex,
    NewVertexIndex,
    IndexIndex,
    NewMorphTargetVertexIndex,
    WeightIndex,
    JointBlockIndex,
    NewJointBlockIndex,
    Old:TpvSizeInt;
    Primitive:TMesh.PPrimitive;
    NodeMeshPrimitiveInstance:TMesh.TPrimitive.PNodeMeshPrimitiveInstance;
    Vertex,OldVertex:PVertex;
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
    Vertex^.NodeIndex:=aNodeIndex+1;
    if Vertex^.MorphTargetVertexBaseIndex<>TpvUInt32($ffffffff) then begin
     WeightIndex:=0;
     MorphTargetVertexIndex:=Vertex^.MorphTargetVertexBaseIndex;
     while MorphTargetVertexIndex<>TpvUInt32($ffffffff) do begin
      MorphTargetVertex:=@fGroup.fMorphTargetVertices.Items[MorphTargetVertexIndex];
      MorphTargetVertex^.Index:=aWeightsOffset+WeightIndex;
      inc(WeightIndex);
      if MorphTargetVertex^.Next=TpvUInt32($ffffffff) then begin
       break;
      end else begin
       MorphTargetVertexIndex:=MorphTargetVertex^.Next;
      end;
     end;
    end;
    if (Vertex^.JointBlockBaseIndex<>TpvUInt32($ffffffff)) and (Vertex^.CountJointBlocks>0) then begin
     for JointBlockIndex:=0 to TpvSizeInt(Vertex^.CountJointBlocks)-1 do begin
      NewJointBlockIndex:=Vertex^.JointBlockBaseIndex+JointBlockIndex;
      if length(fGroup.fJointBlockOffsets)<=NewJointBlockIndex then begin
       Old:=length(fGroup.fJointBlockOffsets);
       SetLength(fGroup.fJointBlockOffsets,(NewJointBlockIndex+1)*2);
       FillChar(fGroup.fJointBlockOffsets[Old],((length(fGroup.fJointBlockOffsets)-Old)+1)*SizeOf(TpvSizeInt),#0);
      end;
      fGroup.fJointBlockOffsets[NewJointBlockIndex]:=aJointNodeOffset;
     end;
    end else begin
     Vertex^.JointBlockBaseIndex:=TpvUInt32($ffffffff);
     Vertex^.CountJointBlocks:=0;
    end;
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
    OldVertex:=@fGroup.fVertices.Items[VertexIndex];
    NewVertexIndex:=fGroup.fVertices.Add(OldVertex^);
    Vertex:=@fGroup.fVertices.Items[NewVertexIndex];
    Vertex^.NodeIndex:=aNodeIndex+1;
    if Vertex^.MorphTargetVertexBaseIndex<>TpvUInt32($ffffffff) then begin
     WeightIndex:=0;
     MorphTargetVertexIndex:=Vertex^.MorphTargetVertexBaseIndex;
     Vertex^.MorphTargetVertexBaseIndex:=fGroup.fMorphTargetVertices.Count;
     while MorphTargetVertexIndex<>TpvUInt32($ffffffff) do begin
      NewMorphTargetVertexIndex:=fGroup.fMorphTargetVertices.AddNew;
      fGroup.fMorphTargetVertices.Items[NewMorphTargetVertexIndex]:=fGroup.fMorphTargetVertices.Items[MorphTargetVertexIndex];
      MorphTargetVertex:=@fGroup.fMorphTargetVertices.Items[NewMorphTargetVertexIndex];
//    MorphTargetVertex^.Index:=(MorphTargetVertex^.Index-Primitive^.MorphTargetBaseIndex)+NodeMeshPrimitiveInstance^.MorphTargetBaseIndex;
      MorphTargetVertex^.Index:=aWeightsOffset+WeightIndex;
      inc(WeightIndex);
      if MorphTargetVertex^.Next=TpvUInt32($ffffffff) then begin
       break;
      end else begin
       MorphTargetVertexIndex:=MorphTargetVertex^.Next;
       MorphTargetVertex^.Next:=fGroup.fMorphTargetVertices.Count;
      end;
     end;
    end else begin
     Vertex^.MorphTargetVertexBaseIndex:=TpvUInt32($ffffffff);
    end;

    if (Vertex^.JointBlockBaseIndex<>TpvUInt32($ffffffff)) and (Vertex^.CountJointBlocks>0) then begin
     Vertex^.JointBlockBaseIndex:=fGroup.fJointBlocks.Count;
     for JointBlockIndex:=0 to TpvSizeInt(Vertex^.CountJointBlocks)-1 do begin
      NewJointBlockIndex:=fGroup.fJointBlocks.AddNew;
      fGroup.fJointBlocks.Items[NewJointBlockIndex]:=fGroup.fJointBlocks.Items[OldVertex^.JointBlockBaseIndex+JointBlockIndex];
      if length(fGroup.fJointBlockOffsets)<=NewJointBlockIndex then begin
       Old:=length(fGroup.fJointBlockOffsets);
       SetLength(fGroup.fJointBlockOffsets,(NewJointBlockIndex+1)*2);
       FillChar(fGroup.fJointBlockOffsets[Old],((length(fGroup.fJointBlockOffsets)-Old)+1)*SizeOf(TpvSizeInt),#0);
      end;
      fGroup.fJointBlockOffsets[NewJointBlockIndex]:=aJointNodeOffset;
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
         Normal:=PpvVector3(pointer(@TemporaryNormals[VertexIndex]))^.Normalize;
         Tangent:=PpvVector3(pointer(@TemporaryTangents[VertexIndex]))^.Normalize;
         Bitangent:=Normal.Cross(Tangent).Normalize;
         PpvVector3(pointer(@TemporaryBitangents[VertexIndex]))^:=Bitangent*TemporaryTangents[VertexIndex,3];
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
        Vertex^.Flags:=0;
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
        Vertex^.Normal:=OctEncode(TangentSpaceMatrix.Normal);
        Vertex^.Tangent:=OctEncode(TangentSpaceMatrix.Tangent);
{$if true}
        if (OctDecode(Vertex^.Normal).Cross(OctDecode(Vertex^.Tangent))).Dot(TangentSpaceMatrix.Bitangent)<0.0 then begin
         Vertex^.Flags:=Vertex^.Flags or (1 shl 0);
        end;
{$else}
        if (VertexIndex<length(TemporaryTangents)) and (TpvVector4(pointer(@TemporaryTangents[VertexIndex])^).w<0) then begin
         Vertex^.Flags:=Vertex^.Flags or (1 shl 0);
        end;
{$ifend}
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
        end else begin
         Vertex^.MorphTargetVertexBaseIndex:=TpvUInt32($ffffffff);
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
         {if not MaxJointBlocksHashMap.TryGet(MaxJointBlocks^,Vertex^.JointBlockBaseIndex) then }begin
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
           TangentSpaceMatrix.Normal:=OctDecode(Vertex^.Normal);
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
          TangentSpaceMatrix.Tangent:=OctDecode(Vertex^.Tangent);
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
        for TargetIndex:=0 to length(DestinationMeshPrimitive^.Targets)-1 do begin
         DestinationMeshPrimitiveTarget:=@DestinationMeshPrimitive^.Targets[TargetIndex];
         DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex-DestinationMeshPrimitive^.StartBufferVertexOffset];
         MorphTargetVertexIndex:=fGroup.fMorphTargetVertices.AddNew;
         MorphTargetVertex:=@fGroup.fMorphTargetVertices.Items[MorphTargetVertexIndex];
         MorphTargetVertex^.Position:=TpvVector4.Create(DestinationMeshPrimitiveTargetVertex^.Position,0.0);
         MorphTargetVertex^.Normal:=TpvVector4.Create(DestinationMeshPrimitiveTargetVertex^.Normal,0.0);
         MorphTargetVertex^.Tangent:=TpvVector4.Create(DestinationMeshPrimitiveTargetVertex^.Tangent,0.0);
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

{ TpvScene3D.TGroup.TSkin }

constructor TpvScene3D.TGroup.TSkin.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
 fJointMatrixOffset:=0;
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
 fName:='';
 fIndex:=aIndex;
 fNodes:=TNodes.Create;
 fNodes.OwnsObjects:=false;
 fData.fVisible:=true;
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
 fData.fType_:=TpvScene3D.TLightData.TType.None;
 fData.fIntensity:=1.0;
 fData.fRange:=0.0;
 fData.fInnerConeAngle:=0.0;
 fData.fOuterConeAngle:=pi*0.25;
 fData.fColor.x:=1.0;
 fData.fColor.y:=1.0;
 fData.fColor.z:=1.0;
 fData.fVisible:=true;
 fData.fCastShadows:=false;
 if assigned(aSourceLight) then begin
  fName:=TPasJSON.GetString(aSourceLight.Properties['name'],'');
  TypeString:=TPasJSON.GetString(aSourceLight.Properties['type'],'');
  if pos('_noshadows',String(fName))>0 then begin
   fData.fCastShadows:=false;
  end else begin
   fData.fCastShadows:=TPasJSON.GetBoolean(aSourceLight.Properties['castShadows'],true);
  end;
  if TypeString='directional' then begin
   fData.fType_:=TpvScene3D.TLightData.TType.Directional;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end else if TypeString='point' then begin
   fData.fType_:=TpvScene3D.TLightData.TType.Point;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountCubeMapShadowMaps;
    inc(fCountCubeMapShadowMaps);
   end;}
  end else if TypeString='spot' then begin
   fData.fType_:=TpvScene3D.TLightData.TType.Spot;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end else begin
   fData.fType_:=TpvScene3D.TLightData.TType.None;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end;
  fData.fIntensity:=TPasJSON.GetNumber(aSourceLight.Properties['intensity'],fData.fIntensity);
  fData.fRange:=TPasJSON.GetNumber(aSourceLight.Properties['range'],fData.fRange);
  SpotItem:=aSourceLight.Properties['spot'];
  if assigned(SpotItem) and (SpotItem is TPasJSONItemObject) then begin
   SpotObject:=TPasJSONItemObject(SpotItem);
   fData.fInnerConeAngle:=TPasJSON.GetNumber(SpotObject.Properties['innerConeAngle'],fData.fInnerConeAngle);
   fData.fOuterConeAngle:=TPasJSON.GetNumber(SpotObject.Properties['outerConeAngle'],fData.fOuterConeAngle);
  end;
  ColorItem:=aSourceLight.Properties['color'];
  if assigned(ColorItem) and (ColorItem is TPasJSONItemArray) then begin
   ColorArray:=TPasJSONItemArray(ColorItem);
   if ColorArray.Count>0 then begin
    fData.fColor.x:=TPasJSON.GetNumber(ColorArray.Items[0],fData.fColor.x);
   end;
   if ColorArray.Count>1 then begin
    fData.fColor.y:=TPasJSON.GetNumber(ColorArray.Items[1],fData.fColor.y);
   end;
   if ColorArray.Count>2 then begin
    fData.fColor.z:=TPasJSON.GetNumber(ColorArray.Items[2],fData.fColor.z);
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

 fNodeMeshInstanceIndex:=-1;

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

procedure TpvScene3D.TGroup.TNode.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceNode:TPasGLTF.TNode;const aLightMap:TpvScene3D.TGroup.TLights);
var WeightIndex,ChildrenIndex,Count,LightIndex,JointMatrixOffset:TPasGLTFSizeInt;
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

 fRotation:=TpvQuaternion(pointer(@aSourceNode.Rotation)^);

 fScale:=TpvVector3(pointer(@aSourceNode.Scale)^);

 SetLength(fWeights,aSourceNode.Weights.Count);
 for WeightIndex:=0 to length(fWeights)-1 do begin
  fWeights[WeightIndex]:=aSourceNode.Weights[WeightIndex];
 end;

 fWeightsOffset:=Group.fCountNodeWeights;

 if assigned(fSkin) then begin
  JointMatrixOffset:=fSkin.fJointMatrixOffset;
 end else begin
  JointMatrixOffset:=0;
 end;

 if assigned(fMesh) then begin
  Mesh:=fMesh;
  fNodeMeshInstanceIndex:=fMesh.CreateNodeMeshInstance(fIndex,fWeightsOffset,JointMatrixOffset);
  Count:=length(fWeights);
  if Count<length(Mesh.fWeights) then begin
   SetLength(fWeights,length(Mesh.fWeights));
   for WeightIndex:=Count to length(Mesh.fWeights)-1 do begin
    fWeights[WeightIndex]:=Mesh.fWeights[WeightIndex];
   end;
  end;
 end else begin
  fNodeMeshInstanceIndex:=-1;
 end;

 inc(fGroup.fCountNodeWeights,length(fWeights));

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

{ TpvScene3D.TGroup.TMaterial }

constructor TpvScene3D.TGroup.TScene.TMaterial.Create;
begin
 inherited Create;
 fPrimitiveIndexRanges.Initialize;
 fCombinedPrimitiveIndexRanges.Initialize;
end;

destructor TpvScene3D.TGroup.TScene.TMaterial.Destroy;
begin
 fPrimitiveIndexRanges.Finalize;
 fCombinedPrimitiveIndexRanges.Finalize;
 inherited Destroy;
end;

{ TpvScene3D.TGroup.TScene }

constructor TpvScene3D.TGroup.TScene.Create(const aGroup:TGroup;const aIndex:TpvSizeInt);
begin
 inherited Create(aGroup);
 fIndex:=aIndex;
 fNodes:=TNodes.Create;
 fNodes.OwnsObjects:=false;
 fMaterials:=TpvScene3D.TGroup.TScene.TMaterials.Create;
 fMaterials.OwnsObjects:=true;
 fMaterialHashMap:=TpvScene3D.TGroup.TScene.TMaterialHashMap.Create(nil);
end;

destructor TpvScene3D.TGroup.TScene.Destroy;
begin
 FreeAndNil(fMaterialHashMap);
 FreeAndNil(fMaterials);
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

 fLights:=TpvScene3D.TGroup.TLights.Create;
 fLights.OwnsObjects:=true;

 fScenes:=TScenes.Create;
 fScenes.OwnsObjects:=true;

 fVertices.Initialize;

 fIndices.Initialize;

 fPerMaterialCondensedIndices.Initialize;

 fMorphTargetVertices.Initialize;

 fJointBlocks.Initialize;

 fJointBlockOffsets:=nil;

 fSkinStorageBufferSize:=0;

 fMorphTargetCount:=0;

 fCulling:=false;

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

 fPerMaterialCondensedIndices.Finalize;

 fIndices.Finalize;

 fVertices.Finalize;

 fMorphTargetVertices.Finalize;

 fJointBlocks.Finalize;

 fJointBlockOffsets:=nil;

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
  if fPerMaterialCondensedIndices.Count=0 then begin
   fPerMaterialCondensedIndices.Add(0);
  end;
  if fMorphTargetVertices.Count=0 then begin
   fMorphTargetVertices.AddNew;
  end;
  if fJointBlocks.Count=0 then begin
   fJointBlocks.AddNew;
  end;

  fVertices.Finish;
  fIndices.Finish;
  fPerMaterialCondensedIndices.Finish;
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

{ fVulkanIndexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
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
                                TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);//}

  fVulkanMaterialIndexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                     fPerMaterialCondensedIndices.Count*SizeOf(TVkUInt32),
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     [],
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                                    );
  fVulkanMaterialIndexBuffer.UploadData(UniversalQueue,
                                        UniversalCommandBuffer,
                                        UniversalFence,
                                        fPerMaterialCondensedIndices.Items[0],
                                        0,
                                        fPerMaterialCondensedIndices.Count*SizeOf(TVkUInt32),
                                        TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);

  fVulkanMorphTargetVertexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                         fMorphTargetVertices.Count*SizeOf(TMorphTargetVertex),
                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
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

  fVulkanJointBlockBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                  fJointBlocks.Count*SizeOf(TJointBlock),
                                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                  TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                  [],
                                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                                 );
  fVulkanJointBlockBuffer.UploadData(UniversalQueue,
                                     UniversalCommandBuffer,
                                     UniversalFence,
                                     fJointBlocks.Items[0],
                                     0,
                                     fJointBlocks.Count*SizeOf(TJointBlock),
                                     TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);

 end;
var Instance:TpvScene3D.TGroup.TInstance;
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
 for Instance in fInstances do begin
  Instance.Upload;
 end;
end;

procedure TpvScene3D.TGroup.Unload;
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Unload;
 end;
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     FreeAndNil(fVulkanVertexBuffer);
//   FreeAndNil(fVulkanIndexBuffer);
     FreeAndNil(fVulkanMaterialIndexBuffer);
     FreeAndNil(fVulkanMorphTargetVertexBuffer);
     FreeAndNil(fVulkanJointBlockBuffer);
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

procedure TpvScene3D.TGroup.CollectMaterialPrimitives;
 procedure ProcessNode(const aScene:TpvScene3D.TGroup.TScene;const aNode:TpvScene3D.TGroup.TNode);
 var PrimitiveIndex:TpvSizeInt;
     Mesh:TpvScene3D.TGroup.TMesh;
     Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
     Node:TpvScene3D.TGroup.TNode;
     Material:TpvScene3D.TMaterial;
     SceneMaterial:TpvScene3D.TGroup.TScene.TMaterial;
     NodeMeshPrimitiveInstance:TpvScene3D.TGroup.TMesh.TPrimitive.PNodeMeshPrimitiveInstance;
     PrimitiveIndexRange:TpvScene3D.TGroup.TScene.TMaterial.TPrimitiveIndexRange;
 begin
  Mesh:=aNode.fMesh;
  if assigned(Mesh) then begin
   if aNode.fNodeMeshInstanceIndex>=0 then begin
    for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin
     Primitive:=@Mesh.fPrimitives[PrimitiveIndex];
     if assigned(Primitive) then begin
      Material:=Primitive^.Material;
      if assigned(Material) then begin
       if aNode.fNodeMeshInstanceIndex<Primitive^.NodeMeshPrimitiveInstances.Count then begin
        NodeMeshPrimitiveInstance:=@Primitive^.NodeMeshPrimitiveInstances.Items[aNode.fNodeMeshInstanceIndex];
        SceneMaterial:=aScene.fMaterialHashMap[Material];
        if not assigned(SceneMaterial) then begin
         SceneMaterial:=TpvScene3D.TGroup.TScene.TMaterial.Create;
         try
          SceneMaterial.fMaterial:=Material;
          aScene.fMaterialHashMap[Material]:=SceneMaterial;
         finally
          aScene.fMaterials.Add(SceneMaterial);
         end;
        end;
        PrimitiveIndexRange.PrimitiveTopology:=TpvScene3D.TPrimitiveTopology(Primitive^.PrimitiveMode);
        PrimitiveIndexRange.Index:=NodeMeshPrimitiveInstance^.StartBufferIndexOffset;
        PrimitiveIndexRange.Count:=Primitive^.CountIndices;
        PrimitiveIndexRange.Node:=aNode.fIndex;
        SceneMaterial.fPrimitiveIndexRanges.Add(PrimitiveIndexRange);
       end;
      end;
     end;
    end;
   end;
  end;
  for Node in aNode.Children do begin
   ProcessNode(aScene,Node);
  end;
 end;
var Scene:TpvScene3D.TGroup.TScene;
    Node:TpvScene3D.TGroup.TNode;
    SceneMaterial:TpvScene3D.TGroup.TScene.TMaterial;
    PrimitiveIndexRange:TpvScene3D.TGroup.TScene.TMaterial.PPrimitiveIndexRange;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    Index,FoundIndex,PrimitiveIndexRangeIndex,IndexValue:TpvSizeInt;
begin
 for Scene in fScenes do begin
  for Node in Scene.fNodes do begin
   ProcessNode(Scene,Node);
  end;
  for SceneMaterial in Scene.fMaterials do begin
   SceneMaterial.fStartIndex:=fPerMaterialCondensedIndices.Count;
   SceneMaterial.fCountIndices:=0;
   SceneMaterial.fPrimitiveIndexRanges.Finish;
   for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin
    for PrimitiveIndexRangeIndex:=0 to SceneMaterial.fPrimitiveIndexRanges.Count-1 do begin
     PrimitiveIndexRange:=@SceneMaterial.fPrimitiveIndexRanges.Items[PrimitiveIndexRangeIndex];
     if (PrimitiveIndexRange^.Count>0) and (PrimitiveIndexRange^.PrimitiveTopology=PrimitiveTopology) then begin
      FoundIndex:=-1;
      for Index:=0 to SceneMaterial.fCombinedPrimitiveIndexRanges.Count-1 do begin
       if SceneMaterial.fCombinedPrimitiveIndexRanges.Items[Index].PrimitiveTopology=PrimitiveTopology then begin
        FoundIndex:=Index;
        break;
       end;
      end;
      if FoundIndex<0 then begin
       FoundIndex:=SceneMaterial.fCombinedPrimitiveIndexRanges.AddNew;
       SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].PrimitiveTopology:=PrimitiveTopology;
       SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].Index:=fPerMaterialCondensedIndices.Count;
       SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].Count:=0;
      end;
      IndexValue:=fPerMaterialCondensedIndices.Count;
      fPerMaterialCondensedIndices.Add(copy(fIndices.Items,PrimitiveIndexRange^.Index,PrimitiveIndexRange^.Count));
      PrimitiveIndexRange^.Index:=IndexValue;
      inc(SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].Count,PrimitiveIndexRange^.Count);
     end;
    end;
   end;
   SceneMaterial.fCountIndices:=fPerMaterialCondensedIndices.Count-SceneMaterial.fStartIndex;
  end;
 end;
 fPerMaterialCondensedIndices.Finish;
end;

procedure TpvScene3D.TGroup.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument);
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
     HashedMaterial:TpvScene3D.TMaterial;
     HashData:TpvScene3D.TMaterial.THashData;
 begin
  for Index:=0 to aSourceDocument.Materials.Count-1 do begin
   SourceMaterial:=aSourceDocument.Materials[Index];
   Material:=TpvScene3D.TMaterial.Create(pvApplication.ResourceManager,self);
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
  fCountJointNodeMatrices:=0;
  fSkinStorageBufferSize:=0;
  for Index:=0 to aSourceDocument.Skins.Count-1 do begin
   SourceSkin:=aSourceDocument.Skins[Index];
   Skin:=TSkin.Create(self,Index);
   try
    Skin.AssignFromGLTF(aSourceDocument,SourceSkin);
   finally
    fSkins.Add(Skin);
   end;
   Skin.fJointMatrixOffset:=fCountJointNodeMatrices;
   inc(fCountJointNodeMatrices,Skin.fJoints.Count);
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
     Light:TpvScene3D.TGroup.TLight;
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
        Light:=TpvScene3D.TGroup.TLight.Create(self,Index);
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
 var Index,Offset:TpvSizeInt;
     SourceNode:TPasGLTF.TNode;
     Node:TNode;
 begin
  fCountNodeWeights:=0;
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

  begin
   Offset:=fNodes.Count+1;
   for Index:=0 to length(fJointBlockOffsets)-1 do begin
    inc(fJointBlockOffsets[Index],Offset);
   end;
   for Index:=0 to fSkins.Count-1 do begin
    inc(fSkins[Index].fJointMatrixOffset,Offset);
   end;
   for Index:=0 to Min(fJointBlocks.Count,length(fJointBlockOffsets))-1 do begin
    if IsZero(fJointBlocks.Items[Index].Weights.x) then begin
     fJointBlocks.Items[Index].Joints[0]:=0;
    end else begin
     inc(fJointBlocks.Items[Index].Joints[0],fJointBlockOffsets[Index]);
    end;
    if IsZero(fJointBlocks.Items[Index].Weights.y) then begin
     fJointBlocks.Items[Index].Joints[1]:=0;
    end else begin
     inc(fJointBlocks.Items[Index].Joints[1],fJointBlockOffsets[Index]);
    end;
    if IsZero(fJointBlocks.Items[Index].Weights.z) then begin
     fJointBlocks.Items[Index].Joints[2]:=0;
    end else begin
     inc(fJointBlocks.Items[Index].Joints[2],fJointBlockOffsets[Index]);
    end;
    if IsZero(fJointBlocks.Items[Index].Weights.w) then begin
     fJointBlocks.Items[Index].Joints[3]:=0;
    end else begin
     inc(fJointBlocks.Items[Index].Joints[3],fJointBlockOffsets[Index]);
    end;
   end;
  end;

{ if fCountNodeWeights=fMorphTargetCount then begin
  end;}
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
             (TpvMatrix4x4.CreateFromQuaternion(Node.fRotation)*
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

 CollectMaterialPrimitives;

end;

procedure TpvScene3D.TGroup.Update(const aSwapChainImageIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Update(aSwapChainImageIndex);
 end;
end;

procedure TpvScene3D.TGroup.Prepare(const aSwapChainImageIndex:TpvSizeInt;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aFrustum:TpvFrustum;
                                    const aFrustumCulling:boolean);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Prepare(aSwapChainImageIndex,
                   aRenderPassIndex,
                   aFrustum,
                   aFrustumCulling);
 end;
end;

procedure TpvScene3D.TGroup.Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                                 const aSwapChainImageIndex:TpvSizeInt;
                                 const aRenderPassIndex:TpvSizeInt;
                                 const aCommandBuffer:TpvVulkanCommandBuffer;
                                 var aPipeline:TpvVulkanPipeline;
                                 const aPipelineLayout:TpvVulkanPipelineLayout;
                                 const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes);
const Offsets:TVkDeviceSize=0;
var Instance:TpvScene3D.TGroup.TInstance;
begin
 aCommandBuffer.CmdBindVertexBuffers(0,1,@fVulkanVertexBuffer.Handle,@Offsets);
 aCommandBuffer.CmdBindIndexBuffer(fVulkanMaterialIndexBuffer.Handle,0,TVkIndexType.VK_INDEX_TYPE_UINT32);
 for Instance in fInstances do begin
  Instance.Draw(aGraphicsPipelines,
                aSwapChainImageIndex,
                aRenderPassIndex,
                aCommandBuffer,
                aPipeline,
                aPipelineLayout,
                aMaterialAlphaModes);
 end;
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
                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                               TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                               [],
                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                               TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                               0,
                                               0,
                                               0,
                                               0,
                                               [TpvVulkanBufferFlag.PersistentMapped]
                                              );
   fMorphTargetVertexWeightsBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                           length(fInstance.fMorphTargetVertexWeights)*SizeOf(TpvFloat),
                                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                           [],
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                           0,
                                                           0,
                                                           0,
                                                           0,
                                                           [TpvVulkanBufferFlag.PersistentMapped]
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

procedure TpvScene3D.TGroup.TInstance.TVulkanData.Update(const aSwapChainImageIndex:TpvSizeInt);
begin
 Upload;
 if fUploaded then begin
  fNodeMatricesBuffer.UpdateData(fInstance.fNodeMatrices[0],0,length(fInstance.fNodeMatrices)*SizeOf(TpvMatrix4x4),FlushUpdateData);
  fMorphTargetVertexWeightsBuffer.UpdateData(fInstance.fMorphTargetVertexWeights[0],0,length(fInstance.fMorphTargetVertexWeights)*SizeOf(TpvFloat),FlushUpdateData);
{ fInstance.fSceneInstance.AddSwapChainImageBufferMemoryBarrier(aSwapChainImageIndex,fNodeMatricesBuffer);
  fInstance.fSceneInstance.AddSwapChainImageBufferMemoryBarrier(aSwapChainImageIndex,fMorphTargetVertexWeightsBuffer);}
 end;
end;

{ TpvScene3D.TGroup.TInstance.TAnimation }

constructor TpvScene3D.TGroup.TInstance.TAnimation.Create;
begin
 inherited Create;
 fChannels:=TChannels.Create;
 fChannels.OwnsObjects:=true;
end;

destructor TpvScene3D.TGroup.TInstance.TAnimation.Destroy;
begin
 FreeAndNil(fChannels);
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
 fActive:=true;
 fPreviousActive:=false;
 fUploaded:=false;
 fModelMatrix:=TpvMatrix4x4.Identity;
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
  InstanceNode^.Processed:=false;
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
  if Index>0 then begin
   for OtherIndex:=0 to length(fGroup.fAnimations[Index-1].fChannels)-1 do begin
    fAnimations[Index].fChannels.Add(TpvScene3D.TGroup.TInstance.TAnimation.TChannel.Create);
   end;
  end;
 end;
 fAnimations[0].Factor:=1.0;
 fNodeMatrices:=nil;
 fMorphTargetVertexWeights:=nil;
 for Index:=0 to length(fVulkanDatas)-1 do begin
  fVulkanDatas[Index]:=TpvScene3D.TGroup.TInstance.TVulkanData.Create(self);
 end;
 fAABBTreeProxy:=-1;
end;

destructor TpvScene3D.TGroup.TInstance.Destroy;
var Index:TPasGLTFSizeInt;
begin
 Unload;
 if fAABBTreeProxy>=0 then begin
  try
   if assigned(fGroup) and
      assigned(fGroup.fSceneInstance) and
      assigned(fGroup.fSceneInstance.fAABBTree) then begin
    fGroup.fSceneInstance.fAABBTree.DestroyProxy(fAABBTreeProxy);
   end;
  finally
   fAABBTreeProxy:=-1;
  end;
 end;
 for Index:=0 to length(fNodes)-1 do begin
  if assigned(fNodes[Index].Light) then begin
   FreeAndNil(fNodes[Index].Light);
  end;
 end;
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
  if fAABBTreeProxy>=0 then begin
   try
    if assigned(fGroup) and
       assigned(fGroup.fSceneInstance) and
       assigned(fGroup.fSceneInstance.fAABBTree) then begin
     fGroup.fSceneInstance.fAABBTree.DestroyProxy(fAABBTreeProxy);
    end;
   finally
    fAABBTreeProxy:=-1;
   end;
  end;
 finally
  fGroup:=nil;
 end;
 inherited BeforeDestruction;
end;

function TpvScene3D.TGroup.TInstance.GetAutomation(const aIndex:TPasGLTFSizeInt):TpvScene3D.TGroup.TInstance.TAnimation;
begin
 result:=fAnimations[aIndex+1];
end;

procedure TpvScene3D.TGroup.TInstance.SetScene(const aScene: TpvSizeInt);
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
    DescriptorSet:TpvVulkanDescriptorSet;
begin

 inherited Upload;

 if not fUploaded then begin
  try

   SetLength(fNodeMatrices,fGroup.fNodes.Count+fGroup.fCountJointNodeMatrices+1);

   SetLength(fMorphTargetVertexWeights,Max(Max(fGroup.fMorphTargetCount,fGroup.fCountNodeWeights),1));

   for Index:=0 to length(fVulkanDatas)-1 do begin
    fVulkanDatas[Index].Upload;
   end;

   fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                         TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                         length(fVulkanDescriptorSets));
   fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,length(fVulkanDescriptorSets)*4);
   fVulkanDescriptorPool.Initialize;

   for Index:=0 to length(fVulkanDescriptorSets)-1 do begin

    DescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                 fSceneInstance.fMeshVulkanDescriptorSetLayout);
    try
     DescriptorSet.WriteToDescriptorSet(0,
                                        0,
                                        1,
                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                        [],
                                        [fGroup.fVulkanMorphTargetVertexBuffer.DescriptorBufferInfo],
                                        [],
                                        false);
     DescriptorSet.WriteToDescriptorSet(1,
                                        0,
                                        1,
                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                        [],
                                        [fGroup.fVulkanJointBlockBuffer.DescriptorBufferInfo],
                                        [],
                                        false);
     DescriptorSet.WriteToDescriptorSet(2,
                                        0,
                                        1,
                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                        [],
                                        [fVulkanDatas[Index].fNodeMatricesBuffer.DescriptorBufferInfo],
                                        [],
                                        false);
     DescriptorSet.WriteToDescriptorSet(3,
                                        0,
                                        1,
                                        TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                        [],
                                        [fVulkanDatas[Index].fMorphTargetVertexWeightsBuffer.DescriptorBufferInfo],
                                        [],
                                        false);
     DescriptorSet.Flush;
    finally
     fVulkanDescriptorSets[Index]:=DescriptorSet;
    end;

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
   for Index:=0 to length(fVulkanDescriptorSets)-1 do begin
    FreeAndNil(fVulkanDescriptorSets[Index]);
   end;
   FreeAndNil(fVulkanDescriptorPool);
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

procedure TpvScene3D.TGroup.TInstance.UpdateInvisible;
var Index:TPasGLTFSizeInt;
begin
 for Index:=0 to length(fNodes)-1 do begin
  if assigned(fNodes[Index].Light) then begin
   FreeAndNil(fNodes[Index].Light);
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.Update(const aSwapChainImageIndex:TpvSizeInt);
var CullFace,Blend:TPasGLTFInt32;
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
 procedure ProcessAnimation(const aAnimationIndex:TpvSizeInt;const aAnimationTime:TpvDouble;const aFactor:TpvFloat);
 var ChannelIndex,
     InstanceChannelIndex,
     CountInstanceChannels,
     InputTimeArrayIndex,
     WeightIndex,
     CountWeights,
     ElementIndex,
     l,r,m:TpvSizeInt;
     Animation:TpvScene3D.TGroup.TAnimation;
     AnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
     InstanceAnimation:TpvScene3D.TGroup.TInstance.TAnimation;
     InstanceAnimationChannel:TpvScene3D.TGroup.TInstance.TAnimation.TChannel;
     //Node:TpvScene3D.TGroup.TNode;
     Node:TpvScene3D.TGroup.TInstance.PNode;
     Time,Factor,Scalar,Value,SqrFactor,CubeFactor,KeyDelta,v0,v1,a,b:TpvDouble;
     Vector3:TpvVector3;
     Vector4:TpvVector4;
     Vector3s:array[0..1] of PpvVector3;
     Vector4s:array[0..1] of PpvVector4;
     TimeIndices:array[0..1] of TpvSizeInt;
     Overwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
 begin

  InstanceAnimation:=fAnimations[aAnimationIndex+1];

  for InstanceChannelIndex:=0 to InstanceAnimation.fChannels.Count-1 do begin
   InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
   InstanceAnimationChannel.fNode:=nil;
   InstanceAnimationChannel.fOverwrite:=-1;
  end;

  CountInstanceChannels:=0;

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

     Overwrite:=nil;

     if aFactor>=-0.5 then begin
      InstanceAnimationChannel:=nil;
      for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
       if InstanceAnimation.fChannels[InstanceChannelIndex].fNode=Node then begin
        InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
        break;
       end;
      end;
      if assigned(InstanceAnimationChannel) then begin
       Overwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
      end else if (Node.CountOverwrites<length(Node.Overwrites)) and
                  (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
       InstanceChannelIndex:=CountInstanceChannels;
       inc(CountInstanceChannels);
       InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
       InstanceAnimationChannel.fNode:=Node;
       InstanceAnimationChannel.fOverwrite:=Node.CountOverwrites;
       inc(Node.CountOverwrites);
       Overwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
       Overwrite^.Flags:=[];
       Overwrite^.Factor:=Max(aFactor,0.0);
      end;
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
        Overwrite^.Rotation.Vector:=Vector4;
       end else begin
        Include(Node^.OverwriteFlags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation);
        Node^.OverwriteRotation.Vector:=Vector4;
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
 var Index,OtherIndex,RotationCounter:TpvSizeInt;
     Matrix:TpvMatrix4x4;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Node:TpvScene3D.TGroup.TNode;
     Translation,Scale:TpvVector3;
     Rotation:TpvQuaternion;
     TranslationSum,ScaleSum:TVector3Sum;
     Factor,
     RotationFactorSum,
     WeightsFactorSum:TpvDouble;
     Overwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
     FirstWeights,SkinUsed:boolean;
     Light:TpvScene3D.TLight;
  procedure AddRotation(const aRotation:TpvQuaternion;const aFactor:TpvDouble);
  begin
   if not IsZero(aFactor) then begin
    if RotationCounter=0 then begin
     Rotation:=aRotation;
    end else begin
     // Informal rolling weighted average proof as javascript/ecmascript:
     // var data = [[1, 0.5], [2, 0.25], [3, 0.125], [4, 0.0625]]; // <= [[value, weight], ... ]
     // var weightedAverage = 0, weightSum = 0;
     // for(var i = 0; i < data.length; i++){
     //   weightSum += data[i][1];
     // }
     // for(var i = 0; i < data.length; i++){
     //    weightedAverage += data[i][0] * data[i][1];
     // };
     // weightedAverage /= weightSum;
     // var rollingAverage = 0, rollingWeightSum = 0;
     // for(var i = 0; i < data.length; i++){
     //   //-------------------- THIS -----------------\\ should be replaced with the actual blend operation, for example slerping
     //   rollingAverage += (data[i][0] - rollingAverage) * (data[i][1] / (rollingWeightSum + data[i][1]));
     //   rollingWeightSum += data[i][1];
     // }
     // var output = [weightedAverage, rollingAverage, weightedAverage * weightSum, rollingAverage * weightSum];
     // output should be [1.7333333333333334, 1.7333333333333334, 1.625, 1.625] then
     // Slerp: Commutative =  No, Constant velocity = Yes, Torque minimal = Yes (no artefact-jumps)
     // Nlerp: Commutative = Yes, Constant velocity = No,  Torque minimal = Yes (no artefact-jumps)
     // Elerp: Commutative = Yes, Constant velocity = Yes, Torque minimal = No  (can produce artefact-jumps on too distinct to blending automation rotation frames)
     Rotation:=Rotation.Slerp(aRotation,aFactor/(RotationFactorSum+aFactor)); // Rolling weighted average
    end;
    inc(RotationCounter);
    RotationFactorSum:=RotationFactorSum+aFactor;
   end;
  end;
 begin
  SkinUsed:=false;
  InstanceNode:=@fNodes[aNodeIndex];
  Node:=fGroup.fNodes[aNodeIndex];
  InstanceNode^.Processed:=true;
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
   RotationFactorSum:=0.0;
   WeightsFactorSum:=0.0;
   FirstWeights:=true;
   Rotation:=TpvQuaternion.Identity;
   RotationCounter:=0;
   for Index:=0 to InstanceNode^.CountOverwrites-1 do begin
    Overwrite:=@InstanceNode^.Overwrites[Index];
    Factor:=Overwrite^.Factor;
    if not IsZero(Factor) then begin
     if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Defaults in Overwrite^.Flags then begin
      TranslationSum.x:=TranslationSum.x+(Node.fTranslation.x*Factor);
      TranslationSum.y:=TranslationSum.y+(Node.fTranslation.y*Factor);
      TranslationSum.z:=TranslationSum.z+(Node.fTranslation.z*Factor);
      TranslationSum.FactorSum:=TranslationSum.FactorSum+Factor;
      ScaleSum.x:=ScaleSum.x+(Node.fScale.x*Factor);
      ScaleSum.y:=ScaleSum.y+(Node.fScale.y*Factor);
      ScaleSum.z:=ScaleSum.z+(Node.fScale.z*Factor);
      ScaleSum.FactorSum:=ScaleSum.FactorSum+Factor;
      AddRotation(Node.fRotation,Factor);
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
       AddRotation(Overwrite^.Rotation,Factor);
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
   if RotationFactorSum>0.0 then begin
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
          (TpvMatrix4x4.CreateFromQuaternion(Rotation)*
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
   if {SkinUsed and} assigned(Node.fSkin) then begin
    fSkins[Node.fSkin.Index].Used:=true;
   end;
  end;
  if assigned(Node.fLight) then begin
   if assigned(InstanceNode^.Light) then begin
    Light:=InstanceNode^.Light;
    if Light.fMatrix<>Matrix then begin
     Light.fMatrix:=Matrix;
     Light.Update;
    end;
   end else begin
    Light:=TpvScene3D.TLight.Create(fSceneInstance);
    try
     Light.fData:=Node.fLight.fData;
     Light.fMatrix:=Matrix;
     Light.Update;
    finally
     InstanceNode^.Light:=Light;
    end;
   end;
  end;
  for Index:=0 to Node.Children.Count-1 do begin
   ProcessNode(Node.Children[Index].Index,Matrix);
  end;
 end;
 procedure ProcessSkins;
 var SkinIndex,Index:TpvSizeInt;
     Skin:TpvScene3D.TGroup.TSkin;
     InstanceSkin:TpvScene3D.TGroup.TInstance.PSkin;
 begin
  for SkinIndex:=0 to fGroup.fSkins.Count-1 do begin
   Skin:=fGroup.fSkins[SkinIndex];
   InstanceSkin:=@fSkins[SkinIndex];
   if InstanceSkin^.Used and (Skin.fJoints.Count>0) then begin
    for Index:=0 to Skin.fJoints.Count-1 do begin
     Assert(fGroup.fNodes[Skin.fJoints.Items[Index]].Index=Skin.fJoints.Items[Index]);
     fNodeMatrices[Skin.fJointMatrixOffset+Index]:=Skin.fInverseBindMatrices.Items[Index]*fNodes[Skin.fJoints.Items[Index]].WorkMatrix;
    end;
   end;
  end;
 end;
 procedure ProcessBoundingBoxNode(const aNodeIndex:TpvSizeInt);
 var Index:TPasGLTFSizeInt;
     Node:TpvScene3D.TGroup.TNode;
     InstanceNode,OtherInstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     AABB,OtherAABB:PpvAABB;
     Filled:PBoolean;
 begin
  InstanceNode:=@fNodes[aNodeIndex];
  Node:=fGroup.fNodes[aNodeIndex];
  AABB:=@InstanceNode^.BoundingBoxes[aSwapChainImageIndex];
  Filled:=@InstanceNode^.BoundingBoxFilled[aSwapChainImageIndex];
  for Index:=0 to Node.Children.Count-1 do begin
   ProcessBoundingBoxNode(Node.Children[Index].Index);
   OtherInstanceNode:=@fNodes[Node.Children[Index].Index];
   if OtherInstanceNode^.BoundingBoxFilled[aSwapChainImageIndex] then begin
    OtherAABB:=@OtherInstanceNode^.BoundingBoxes[aSwapChainImageIndex];
    if Filled^ then begin
     AABB^:=AABB^.Combine(OtherAABB^);
    end else begin
     Filled^:=true;
     AABB^:=OtherAABB^;
    end;
   end;
  end;
 end;
var Index:TPasGLTFSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    Animation:TpvScene3D.TGroup.TInstance.TAnimation;
    Node:TpvScene3D.TGroup.TNode;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    AABB:TpvAABB;
begin

 fActives[aSwapChainImageIndex]:=fActive;

 if fActive then begin

  Scene:=GetScene;

  if assigned(Scene) then begin

   fScenes[aSwapChainImageIndex]:=Scene;

   //CurrentSkinShaderStorageBufferObjectHandle:=0;

   for Index:=0 to length(fLightNodes)-1 do begin
    fLightNodes[Index]:=-1;
   end;

   for Index:=0 to Scene.Nodes.Count-1 do begin
    ResetNode(Scene.Nodes[Index].Index);
   end;

   for Index:=0 to length(fNodes)-1 do begin
    fNodes[Index].Processed:=false;
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

   for Index:=0 to length(fNodes)-1 do begin
    if not fNodes[Index].Processed then begin
     if assigned(fNodes[Index].Light) then begin
      FreeAndNil(fNodes[Index].Light);
     end;
    end;
   end;

   ProcessSkins;

   fNodeMatrices[0]:=fModelMatrix;

   for Index:=0 to fGroup.fNodes.Count-1 do begin
    Node:=fGroup.fNodes[Index];
    InstanceNode:=@fNodes[Index];
    fNodeMatrices[Node.Index+1]:=InstanceNode^.WorkMatrix;
    if length(InstanceNode^.WorkWeights)>0 then begin
     Move(InstanceNode^.WorkWeights[0],fMorphTargetVertexWeights[Node.fWeightsOffset],length(InstanceNode^.WorkWeights)*SizeOf(TpvFloat));
    end;
   end;

   for Index:=0 to fGroup.fNodes.Count-1 do begin
    Node:=fGroup.fNodes[Index];
    InstanceNode:=@fNodes[Index];
    if assigned(Node.fMesh) then begin
     InstanceNode^.BoundingBoxes[aSwapChainImageIndex]:=Node.fMesh.fBoundingBox.Transform(InstanceNode^.WorkMatrix*fModelMatrix);
     InstanceNode^.BoundingBoxFilled[aSwapChainImageIndex]:=true;
    end else begin
     InstanceNode^.BoundingBoxes[aSwapChainImageIndex]:=TpvAABB.Create(TpvVector3.Origin,TpvVector3.Origin);
     InstanceNode^.BoundingBoxFilled[aSwapChainImageIndex]:=false;
    end;
   end;

   for Index:=0 to Scene.fNodes.Count-1 do begin
    ProcessBoundingBoxNode(Scene.fNodes[Index].Index);
   end;

  end;

  fVulkanData:=fVulkanDatas[aSwapChainImageIndex];
  if assigned(fVulkanData) then begin
   fVulkanData.Update(aSwapChainImageIndex);
  end;

  fBoundingBox:=fGroup.fBoundingBox.Transform(fModelMatrix);
  if assigned(Scene) then begin
   for Index:=0 to Scene.fNodes.Count-1 do begin
    InstanceNode:=@fNodes[Scene.fNodes[Index].fIndex];
    if InstanceNode^.BoundingBoxFilled[aSwapChainImageIndex] then begin
     fBoundingBox:=fBoundingBox.Combine(InstanceNode^.BoundingBoxes[aSwapChainImageIndex]);
    end;
   end;
  end;

  if fAABBTreeProxy<0 then begin
   fAABBTreeProxy:=fGroup.fSceneInstance.fAABBTree.CreateProxy(fBoundingBox,TpvPtrInt(Pointer(self)));
  end else begin
   fGroup.fSceneInstance.fAABBTree.MoveProxy(fAABBTreeProxy,fBoundingBox,TpvVector3.Create(1.0,1.0,1.0));
  end;

  fPreviousActive:=true;

 end else if fPreviousActive then begin

  if fAABBTreeProxy>=0 then begin
   try
    if assigned(fGroup) and
       assigned(fGroup.fSceneInstance) and
       assigned(fGroup.fSceneInstance.fAABBTree) then begin
     fGroup.fSceneInstance.fAABBTree.DestroyProxy(fAABBTreeProxy);
    end;
   finally
    fAABBTreeProxy:=-1;
   end;
  end;

  for Index:=0 to length(fNodes)-1 do begin
   if assigned(fNodes[Index].Light) then begin
    FreeAndNil(fNodes[Index].Light);
   end;
  end;

  fPreviousActive:=false;

 end;

end;

procedure TpvScene3D.TGroup.TInstance.Prepare(const aSwapChainImageIndex:TpvSizeInt;
                                              const aRenderPassIndex:TpvSizeInt;
                                              const aFrustum:TpvFrustum;
                                              const aFrustumCulling:boolean);
var VisibleBit:TpvUInt32;
 procedure ProcessNode(const aNodeIndex:TpvSizeInt;const aMask:TpvUInt32);
 var NodeIndex:TpvSizeInt;
     Mask:TpvUInt32;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Node:TpvScene3D.TGroup.TNode;
 begin
  if aNodeIndex>=0 then begin
   InstanceNode:=@fNodes[aNodeIndex];
   Mask:=aMask;
   if InstanceNode^.BoundingBoxFilled[aSwapChainImageIndex] and not (((Mask and $80000000)<>0) and (aFrustum.AABBInFrustum(InstanceNode^.BoundingBoxes[aSwapChainImageIndex],Mask)=TpvFrustum.COMPLETE_OUT)) then begin
    TPasMPInterlocked.BitwiseOr(InstanceNode^.VisibleBitmap,VisibleBit);
    Node:=fGroup.fNodes[aNodeIndex];
    for NodeIndex:=0 to Node.fChildren.Count-1 do begin
     ProcessNode(Node.fChildren[NodeIndex].fIndex,Mask);
    end;
   end;
  end;
 end;
var NodeIndex:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
begin
 VisibleBit:=TpvUInt32(1) shl aRenderPassIndex;
 if fActives[aSwapChainImageIndex] and ((fVisibleBitmap and (TpvUInt32(1) shl aRenderPassIndex))<>0) then begin
  if aFrustumCulling then begin
   for NodeIndex:=0 to length(fNodes)-1 do begin
    TPasMPInterlocked.BitwiseAnd(fNodes[NodeIndex].VisibleBitmap,not VisibleBit);
   end;
   Scene:=fScenes[aSwapChainImageIndex];
   if assigned(Scene) then begin
    for NodeIndex:=0 to Scene.fNodes.Count-1 do begin
     ProcessNode(Scene.fNodes[NodeIndex].fIndex,$ffffffff);
    end;
   end;
  end else begin
   for NodeIndex:=0 to length(fNodes)-1 do begin
    TPasMPInterlocked.BitwiseOr(fNodes[NodeIndex].VisibleBitmap,VisibleBit);
   end;
  end;
 end else begin
  for NodeIndex:=0 to length(fNodes)-1 do begin
   TPasMPInterlocked.BitwiseAnd(fNodes[NodeIndex].VisibleBitmap,not VisibleBit);
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                                           const aSwapChainImageIndex:TpvSizeInt;
                                           const aRenderPassIndex:TpvSizeInt;
                                           const aCommandBuffer:TpvVulkanCommandBuffer;
                                           var aPipeline:TpvVulkanPipeline;
                                           const aPipelineLayout:TpvVulkanPipelineLayout;
                                           const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes);
var SceneMaterialIndex,PrimitiveIndexRangeIndex,
    IndicesStart,IndicesCount:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    SceneMaterial:TpvScene3D.TGroup.TScene.TMaterial;
    Material:TpvScene3D.TMaterial;
    PrimitiveIndexRanges:TpvScene3D.TGroup.TScene.TMaterial.PPrimitiveIndexRanges;
    PrimitiveIndexRange:TpvScene3D.TGroup.TScene.TMaterial.PPrimitiveIndexRange;
    Culling,MeshFirst,MaterialFirst:boolean;
    VisibleBit:TpvUInt32;
 procedure Flush;
 var Pipeline:TpvVulkanPipeline;
     PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
 begin
  if IndicesCount>0 then begin
   PrimitiveTopology:=PrimitiveIndexRange^.PrimitiveTopology;
   Pipeline:=aGraphicsPipelines[PrimitiveTopology,Material.fData.DoubleSided];
   if aPipeline<>Pipeline then begin
    aPipeline:=Pipeline;
    if assigned(Pipeline) then begin
     aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,Pipeline.Handle);
    end;
   end;
   if MeshFirst then begin
    MeshFirst:=false;
    aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         aPipelineLayout.Handle,
                                         0,
                                         1,
                                         @fVulkanDescriptorSets[aSwapChainImageIndex].Handle,
                                         0,
                                         nil);
   end;
   if MaterialFirst then begin
    MaterialFirst:=false;
    aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         aPipelineLayout.Handle,
                                         1,
                                         1,
                                         @Material.fVulkanDescriptorSet.Handle,
                                         0,
                                         nil);
   end;
   aCommandBuffer.CmdDrawIndexed(IndicesCount,1,IndicesStart,0,0);
   IndicesCount:=0;
  end;
 end;
begin
 if fActives[aSwapChainImageIndex] and ((fVisibleBitmap and (TpvUInt32(1) shl aRenderPassIndex))<>0) then begin
  Culling:=fGroup.fCulling;
  Scene:=fScenes[aSwapChainImageIndex];
  if assigned(Scene) then begin
   VisibleBit:=TpvUInt32(1) shl aRenderPassIndex;
   MeshFirst:=true;
   for SceneMaterialIndex:=0 to Scene.fMaterials.Count-1 do begin
    SceneMaterial:=Scene.fMaterials[SceneMaterialIndex];
    if SceneMaterial.fCountIndices>0 then begin
     Material:=SceneMaterial.fMaterial;
     if Material.fData.AlphaMode in aMaterialAlphaModes then begin
      MaterialFirst:=true;
      if Culling then begin
       PrimitiveIndexRanges:=@SceneMaterial.fPrimitiveIndexRanges;
      end else begin
       PrimitiveIndexRanges:=@SceneMaterial.fCombinedPrimitiveIndexRanges;
      end;
      IndicesStart:=0;
      IndicesCount:=0;
      for PrimitiveIndexRangeIndex:=0 to PrimitiveIndexRanges^.Count-1 do begin
       PrimitiveIndexRange:=@PrimitiveIndexRanges^.Items[PrimitiveIndexRangeIndex];
       if (PrimitiveIndexRange^.Count>0) and
          ((not Culling) or
           ((fNodes[PrimitiveIndexRange^.Node].VisibleBitmap and VisibleBit)<>0)) then begin
        if (IndicesCount=0) or ((IndicesStart+IndicesCount)<>PrimitiveIndexRange^.Index) then begin
         Flush;
         IndicesStart:=PrimitiveIndexRange^.Index;
        end;
        inc(IndicesCount,PrimitiveIndexRange^.Count);
       end;
      end;
      Flush;
     end;
    end;
   end;
  end;
 end;
end;

{ TpvScene3D }

constructor TpvScene3D.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
var Index:TpvSizeInt;
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

 for Index:=0 to length(fLights)-1 do begin
  fLights[Index]:=TpvScene3D.TLights.Create;
  fLights[Index].OwnsObjects:=true;
 end;

 for Index:=0 to length(fCountLights)-1 do begin
  fCountLights[Index]:=0;
 end;

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

 fMeshVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);

 // Group - Morph target vertices
 fMeshVulkanDescriptorSetLayout.AddBinding(0,
                                           VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                           1,
                                           TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),
                                           []);

 // Group - Joint blocks
 fMeshVulkanDescriptorSetLayout.AddBinding(1,
                                           VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                           1,
                                           TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),
                                           []);

 // Instance - Node matrices
 fMeshVulkanDescriptorSetLayout.AddBinding(2,
                                           VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                           1,
                                           TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),
                                           []);

 // Instance - Morph target weights
 fMeshVulkanDescriptorSetLayout.AddBinding(3,
                                           VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                           1,
                                           TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT),
                                           []);

 fMeshVulkanDescriptorSetLayout.Initialize;

 fMaterialVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fMaterialVulkanDescriptorSetLayout.AddBinding(0,
                                               VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                               1,
                                               TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                               []);
 fMaterialVulkanDescriptorSetLayout.AddBinding(1,
                                               VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                               11,
                                               TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                               []);
 fMaterialVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.Initialize;

 fLightAABBTree:=TpvBVHDynamicAABBTree.Create;

 fLightAABBTreeGeneration:=0;

 for Index:=0 to length(fLightAABBTreeStates)-1 do begin
  fLightAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to length(fLightAABBTreeStateGenerations)-1 do begin
  fLightAABBTreeStateGenerations[Index]:=fLightAABBTreeGeneration-1;
 end;

 for Index:=0 to length(fLightBuffers)-1 do begin
  fLightBuffers[Index]:=TpvScene3D.TLightBuffer.Create(self);
 end;

 for Index:=0 to length(fSwapChainImageBufferMemoryBarriers)-1 do begin
  fSwapChainImageBufferMemoryBarriers[Index].Initialize;
 end;

 fAABBTree:=TpvBVHDynamicAABBTree.Create;

 for Index:=0 to length(fAABBTreeStates)-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
 end;

end;

destructor TpvScene3D.Destroy;
var Index:TpvSizeInt;
begin

 Unload;

 for Index:=0 to length(fGlobalVulkanDescriptorSets)-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);

 for Index:=0 to length(fAABBTreeStates)-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to length(fSwapChainImageBufferMemoryBarriers)-1 do begin
  fSwapChainImageBufferMemoryBarriers[Index].Finalize;
 end;

 FreeAndNil(fAABBTree);

 for Index:=0 to length(fLightAABBTreeStates)-1 do begin
  fLightAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to length(fLightBuffers)-1 do begin
  FreeAndNil(fLightBuffers[Index]);
 end;

 FreeAndNil(fLightAABBTree);

 FreeAndNil(fMeshVulkanDescriptorSetLayout);

 FreeAndNil(fMaterialVulkanDescriptorSetLayout);

 FreeAndNil(fGlobalVulkanDescriptorSetLayout);

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

 for Index:=0 to length(fLights)-1 do begin
  FreeAndNil(fLights[Index]);
 end;

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

procedure TpvScene3D.AddSwapChainImageBufferMemoryBarrier(const aSwapChainImageIndex:TpvSizeInt;
                                                          const aBuffer:TpvVulkanBuffer);
var Index:TpvSizeInt;
    BufferMemoryBarrier:PVkBufferMemoryBarrier;
begin
 if assigned(aBuffer) then begin
  Index:=fSwapChainImageBufferMemoryBarriers[aSwapChainImageIndex].AddNew;
  BufferMemoryBarrier:=@fSwapChainImageBufferMemoryBarriers[aSwapChainImageIndex].Items[Index];
  FillChar(BufferMemoryBarrier^,SizeOf(TVkBufferMemoryBarrier),#0);
  BufferMemoryBarrier^.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
  BufferMemoryBarrier^.srcAccessMask:=TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT);
  BufferMemoryBarrier^.dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT) or
                                      TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or
                                      TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT) or
                                      TVkAccessFlags(VK_ACCESS_UNIFORM_READ_BIT) or
                                      TVkAccessFlags(VK_ACCESS_INDEX_READ_BIT) or
                                      TVkAccessFlags(VK_ACCESS_INPUT_ATTACHMENT_READ_BIT);
  BufferMemoryBarrier^.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  BufferMemoryBarrier^.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  BufferMemoryBarrier^.buffer:=aBuffer.Handle;
  BufferMemoryBarrier^.offset:=aBuffer.Memory.Offset;
  BufferMemoryBarrier^.size:=aBuffer.Memory.Size;
 end;
end;

procedure TpvScene3D.UploadWhiteTexture;
const Pixels:array[0..63] of TpvUInt32=(TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                        TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                        TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                        TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                        TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                        TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                        TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                        TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff));
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
                                                         8,
                                                         8,
                                                         0,
                                                         0,
                                                         1,
                                                         0,
                                                         [TpvVulkanTextureUsageFlag.General,
                                                          TpvVulkanTextureUsageFlag.TransferDst,
                                                          TpvVulkanTextureUsageFlag.TransferSrc,
                                                          TpvVulkanTextureUsageFlag.Sampled],
                                                         @Pixels,
                                                         SizeOf(TpvUInt32)*64,
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
const Pixels:array[0..63] of TpvUInt32=(TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
                                        TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
                                        TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
                                        TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
                                        TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
                                        TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
                                        TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
                                        TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080));
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
                                                                    8,
                                                                    8,
                                                                    0,
                                                                    0,
                                                                    1,
                                                                    0,
                                                                    [TpvVulkanTextureUsageFlag.General,
                                                                     TpvVulkanTextureUsageFlag.TransferDst,
                                                                     TpvVulkanTextureUsageFlag.TransferSrc,
                                                                     TpvVulkanTextureUsageFlag.Sampled],
                                                                    @Pixels,
                                                                    SizeOf(TpvUInt32)*64,
                                                                    false,
                                                                    false,
                                                                    0,
                                                                    true);
        fDefaultNormalMapTexture.UpdateSampler;
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
    Index:TpvSizeInt;
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try
     UploadWhiteTexture;
     for Index:=0 to length(fLightBuffers)-1 do begin
      fLightBuffers[Index].Upload;
     end;
     fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),length(fGlobalVulkanDescriptorSets));
     fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,length(fGlobalVulkanDescriptorSets)*2);
     fGlobalVulkanDescriptorPool.Initialize;
     for Index:=0 to length(fGlobalVulkanDescriptorSets)-1 do begin
      fGlobalVulkanDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                        fGlobalVulkanDescriptorSetLayout);
      fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(0,
                                                              0,
                                                              1,
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                              [],
                                                              [fLightBuffers[Index].fLightItemsVulkanBuffer.DescriptorBufferInfo],
                                                              [],
                                                              false);
      fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(1,
                                                              0,
                                                              1,
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                              [],
                                                              [fLightBuffers[Index].fLightTreeVulkanBuffer.DescriptorBufferInfo],
                                                              [],
                                                              false);
      fGlobalVulkanDescriptorSets[Index].Flush;
     end;
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
    Index:TpvSizeInt;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     for Index:=0 to length(fGlobalVulkanDescriptorSets)-1 do begin
      FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
     end;
     FreeAndNil(fGlobalVulkanDescriptorPool);
     for Index:=0 to length(fLightBuffers)-1 do begin
      fLightBuffers[Index].Unload;
     end;
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

function TpvScene3D.GetLightUserDataIndex(const aUserData:TpvPtrInt):TpvUInt32;
begin
 result:=TpvScene3D.TLight(Pointer(aUserData)).fLightItemIndex;
end;

procedure TpvScene3D.Update(const aSwapChainImageIndex:TpvSizeInt);
var Group:TpvScene3D.TGroup;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    LightAABBTreeState,AABBTreeState:TpvBVHDynamicAABBTree.PState;
    First:boolean;
    OldGeneration:TpvUInt32;
    LightBuffer:TpvScene3D.TLightBuffer;
begin

 fCountLights[aSwapChainImageIndex]:=0;

 for Group in fGroups do begin
  Group.Update(aSwapChainImageIndex);
 end;

 OldGeneration:=fLightAABBTreeStateGenerations[aSwapChainImageIndex];
 if TPasMPInterlocked.CompareExchange(fLightAABBTreeStateGenerations[aSwapChainImageIndex],fLightAABBTreeGeneration,OldGeneration)=OldGeneration then begin

  LightAABBTreeState:=@fLightAABBTreeStates[aSwapChainImageIndex];

  if (length(fLightAABBTree.Nodes)>0) and (fLightAABBTree.Root>=0) then begin
   if length(LightAABBTreeState^.TreeNodes)<length(fLightAABBTree.Nodes) then begin
    LightAABBTreeState^.TreeNodes:=copy(fLightAABBTree.Nodes);
   end else begin
    Move(fLightAABBTree.Nodes[0],LightAABBTreeState^.TreeNodes[0],length(fLightAABBTree.Nodes)*SizeOf(TpvBVHDynamicAABBTree.TTreeNode));
   end;
   LightAABBTreeState^.Root:=fLightAABBTree.Root;
  end else begin
   LightAABBTreeState^.Root:=-1;
  end;

  LightBuffer:=fLightBuffers[aSwapChainImageIndex];
  CollectLightAABBTreeLights(LightAABBTreeState^.TreeNodes,LightAABBTreeState^.Root,LightBuffer.fLightItems);
  fLightAABBTree.GetGPUSkipListNodes(LightBuffer.fLightTree,GetLightUserDataIndex);
  LightBuffer.Update(aSwapChainImageIndex);

 end;

 AABBTreeState:=@fAABBTreeStates[aSwapChainImageIndex];
 if (length(fAABBTree.Nodes)>0) and (fAABBTree.Root>=0) then begin
  if length(AABBTreeState^.TreeNodes)<length(fAABBTree.Nodes) then begin
   AABBTreeState^.TreeNodes:=copy(fAABBTree.Nodes);
  end else begin
   Move(fAABBTree.Nodes[0],AABBTreeState^.TreeNodes[0],length(fAABBTree.Nodes)*SizeOf(TpvBVHDynamicAABBTree.TTreeNode));
  end;
  AABBTreeState^.Root:=fAABBTree.Root;
 end else begin
  AABBTreeState^.Root:=-1;
 end;

 First:=true;
 fBoundingBox.Min:=TpvVector3.Origin;
 fBoundingBox.Max:=TpvVector3.Origin;
 for GroupInstance in fGroupInstances do begin
  if GroupInstance.fActive then begin
   if First then begin
    First:=false;
    fBoundingBox:=GroupInstance.fBoundingBox;
   end else begin
    fBoundingBox:=fBoundingBox.Combine(GroupInstance.fBoundingBox);
   end;
  end;
 end;

end;

procedure TpvScene3D.CullAABBTreeWithFrustum(const aFrustum:TpvFrustum;
                                             const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                             const aRoot:TpvSizeInt;
                                             const aVisibleBit:TPasMPUInt32);
 procedure ProcessNode(const aNode:TpvSizeint;const aMask:TpvUInt32);
 var TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Mask:TpvUInt32;
 begin
  if aNode>=0 then begin
   TreeNode:=@aTreeNodes[aNode];
   Mask:=aMask;
   if not (((Mask and $80000000)<>0) and (aFrustum.AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)) then begin
    if TreeNode^.UserData<>0 then begin
     TPasMPInterlocked.BitwiseOr(TpvScene3D.TGroup.TInstance(TreeNode^.UserData).fVisibleBitmap,aVisibleBit);
    end;
    if TreeNode^.Children[0]>=0 then begin
     ProcessNode(TreeNode^.Children[0],Mask);
    end;
    if TreeNode^.Children[1]>=0 then begin
     ProcessNode(TreeNode^.Children[1],Mask);
    end;
   end;
  end;
 end;
type PStackItem=^TStackItem;
     TStackItem=record
      Node:TpvSizeInt;
      Mask:TpvUInt32;
     end;
var StackPointer:TpvSizeInt;
    StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Mask:TpvUInt32;
    Stack:array[0..31] of TStackItem;
begin
 if (aRoot>=0) and (length(aTreeNodes)>0) then begin
  Stack[0].Node:=aRoot;
  Stack[0].Mask:=$ffffffff;
  StackPointer:=1;
  while StackPointer>0 do begin
   dec(StackPointer);
   StackItem:=@Stack[StackPointer];
   Node:=StackItem^.Node;
   Mask:=StackItem^.Mask;
   while Node>=0 do begin
    TreeNode:=@aTreeNodes[Node];
    if ((Mask and $80000000)<>0) and (aFrustum.AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT) then begin
     break;
    end;
    if TreeNode^.UserData<>0 then begin
     TPasMPInterlocked.BitwiseOr(TpvScene3D.TGroup.TInstance(TreeNode^.UserData).fVisibleBitmap,aVisibleBit);
    end;
    if (StackPointer>=High(Stack)) and ((TreeNode^.Children[0]>=0) or (TreeNode^.Children[1]>=0)) then begin
     if TreeNode^.Children[0]>=0 then begin
      ProcessNode(TreeNode^.Children[0],Mask);
     end;
     if TreeNode^.Children[1]>=0 then begin
      ProcessNode(TreeNode^.Children[1],Mask);
     end;
    end else begin
     if TreeNode^.Children[0]>=0 then begin
      if TreeNode^.Children[1]>=0 then begin
       StackItem:=@Stack[StackPointer];
       StackItem^.Node:=TreeNode^.Children[1];
       StackItem^.Mask:=Mask;
       inc(StackPointer);
      end;
      Node:=TreeNode^.Children[0];
      continue;
     end else begin
      if TreeNode^.Children[1]>=0 then begin
       Node:=TreeNode^.Children[1];
       continue;
      end;
     end;
    end;
    break;
   end;
  end;
 end;
end;

function TpvScene3DCompareIndirectLights(const a,b:pointer):TpvInt32;
begin
 result:=Sign((ord(TpvScene3D.TLight(b).fData.fType_=TpvScene3D.TLightData.TType.Directional) and 1)-
              (ord(TpvScene3D.TLight(a).fData.fType_=TpvScene3D.TLightData.TType.Directional) and 1));
 if result=0 then begin
  result:=Sign(TpvScene3D.TLight(b).fViewSpacePosition.z-TpvScene3D.TLight(a).fViewSpacePosition.z);
  if result=0 then begin
  end;
 end;
end;

procedure TpvScene3D.CullLightAABBTreeWithFrustum(const aSwapChainImageIndex:TpvSizeInt;
                                                  const aFrustum:PpvFrustum;
                                                  const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                                  const aRoot:TpvSizeInt);
 procedure ProcessNode(const aNode:TpvSizeint;const aMask:TpvUInt32);
 var TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
     Mask:TpvUInt32;
 begin
  if aNode>=0 then begin
   TreeNode:=@aTreeNodes[aNode];
   Mask:=aMask;
   if not (assigned(aFrustum) and (((Mask and $80000000)<>0) and (aFrustum^.AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT))) then begin
    if TreeNode^.UserData<>0 then begin
     if fCountIndirectLights[aSwapChainImageIndex]<MaxVisibleLights then begin
      fIndirectLights[aSwapChainImageIndex,fCountIndirectLights[aSwapChainImageIndex]]:=TpvScene3D.TLight(Pointer(TreeNode^.UserData));
      inc(fCountIndirectLights[aSwapChainImageIndex]);
     end;
    end;
    if TreeNode^.Children[0]>=0 then begin
     ProcessNode(TreeNode^.Children[0],Mask);
    end;
    if TreeNode^.Children[1]>=0 then begin
     ProcessNode(TreeNode^.Children[1],Mask);
    end;
   end;
  end;
 end;
type PStackItem=^TStackItem;
     TStackItem=record
      Node:TpvSizeInt;
      Mask:TpvUInt32;
     end;
var StackPointer:TpvSizeInt;
    StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Mask:TpvUInt32;
    Stack:array[0..31] of TStackItem;
begin
 if (aRoot>=0) and (length(aTreeNodes)>0) then begin
  Stack[0].Node:=aRoot;
  Stack[0].Mask:=$ffffffff;
  StackPointer:=1;
  while StackPointer>0 do begin
   dec(StackPointer);
   StackItem:=@Stack[StackPointer];
   Node:=StackItem^.Node;
   Mask:=StackItem^.Mask;
   while Node>=0 do begin
    TreeNode:=@aTreeNodes[Node];
    if assigned(aFrustum) and (((Mask and $80000000)<>0) and (aFrustum^.AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)) then begin
     break;
    end;
    if TreeNode^.UserData<>0 then begin
     if fCountIndirectLights[aSwapChainImageIndex]<MaxVisibleLights then begin
      fIndirectLights[aSwapChainImageIndex,fCountIndirectLights[aSwapChainImageIndex]]:=TpvScene3D.TLight(Pointer(TreeNode^.UserData));
      inc(fCountIndirectLights[aSwapChainImageIndex]);
     end;
    end;
    if (StackPointer>=High(Stack)) and ((TreeNode^.Children[0]>=0) or (TreeNode^.Children[1]>=0)) then begin
     if TreeNode^.Children[0]>=0 then begin
      ProcessNode(TreeNode^.Children[0],Mask);
     end;
     if TreeNode^.Children[1]>=0 then begin
      ProcessNode(TreeNode^.Children[1],Mask);
     end;
    end else begin
     if TreeNode^.Children[0]>=0 then begin
      if TreeNode^.Children[1]>=0 then begin
       StackItem:=@Stack[StackPointer];
       StackItem^.Node:=TreeNode^.Children[1];
       StackItem^.Mask:=Mask;
       inc(StackPointer);
      end;
      Node:=TreeNode^.Children[0];
      continue;
     end else begin
      if TreeNode^.Children[1]>=0 then begin
       Node:=TreeNode^.Children[1];
       continue;
      end;
     end;
    end;
    break;
   end;
  end;
 end;
end;

procedure TpvScene3D.CollectLightAABBTreeLights(const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                                const aRoot:TpvSizeInt;
                                                var aLightItemArray:TpvScene3D.TLightItems);
 procedure ProcessLight(const aLight:TpvScene3D.TLight);
 var LightItem:TpvScene3D.PLightItem;
     InnerConeAngleCosinus,OuterConeAngleCosinus:TpvScalar;
 begin
  if aLightItemArray.Count<MaxVisibleLights then begin
   aLight.fLightItemIndex:=aLightItemArray.AddNew;
   LightItem:=@aLightItemArray.Items[aLight.fLightItemIndex];
   LightItem^.Type_:=TpvUInt32(aLight.fData.Type_);
   LightItem^.ShadowMapIndex:=0;
   InnerConeAngleCosinus:=cos(aLight.fData.InnerConeAngle);
   OuterConeAngleCosinus:=cos(aLight.fData.OuterConeAngle);
  {LightItem^.InnerConeCosinus:=InnerConeAngleCosinus;
   LightItem^.OuterConeCosinus:=OuterConeAngleCosinus;}
   LightItem^.LightAngleScale:=1.0/Max(1e-5,InnerConeAngleCosinus-OuterConeAngleCosinus);
   LightItem^.LightAngleOffset:=-(OuterConeAngleCosinus*LightItem^.LightAngleScale);
   LightItem^.ColorIntensity:=TpvVector4.InlineableCreate(aLight.fData.fColor,aLight.fData.fIntensity);
   LightItem^.PositionRange:=TpvVector4.InlineableCreate(aLight.fPosition,aLight.fData.fRange);
   LightItem^.DirectionZFar:=TpvVector4.InlineableCreate(aLight.fDirection,0.0);
   LightItem^.ShadowMapMatrix:=TpvMatrix4x4.Identity;
  end else begin
   aLight.fLightItemIndex:=-1;
  end;
 end;
 procedure ProcessNode(const aNode:TpvSizeint);
 var TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
 begin
  if aNode>=0 then begin
   TreeNode:=@aTreeNodes[aNode];
   if TreeNode^.UserData<>0 then begin
    ProcessLight(TpvScene3D.TLight(Pointer(TreeNode^.UserData)));
   end;
   if TreeNode^.Children[0]>=0 then begin
    ProcessNode(TreeNode^.Children[0]);
   end;
   if TreeNode^.Children[1]>=0 then begin
    ProcessNode(TreeNode^.Children[1]);
   end;
  end;
 end;
type PStackItem=^TStackItem;
     TStackItem=record
      Node:TpvSizeInt;
     end;
var StackPointer:TpvSizeInt;
    StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Stack:array[0..31] of TStackItem;
begin
 aLightItemArray.Count:=0;
 if (aRoot>=0) and (length(aTreeNodes)>0) then begin
  Stack[0].Node:=aRoot;
  StackPointer:=1;
  while StackPointer>0 do begin
   dec(StackPointer);
   StackItem:=@Stack[StackPointer];
   Node:=StackItem^.Node;
   while Node>=0 do begin
    TreeNode:=@aTreeNodes[Node];
    if TreeNode^.UserData<>0 then begin
     ProcessLight(TpvScene3D.TLight(Pointer(TreeNode^.UserData)));
    end;
    if (StackPointer>=High(Stack)) and ((TreeNode^.Children[0]>=0) or (TreeNode^.Children[1]>=0)) then begin
     if TreeNode^.Children[0]>=0 then begin
      ProcessNode(TreeNode^.Children[0]);
     end;
     if TreeNode^.Children[1]>=0 then begin
      ProcessNode(TreeNode^.Children[1]);
     end;
    end else begin
     if TreeNode^.Children[0]>=0 then begin
      if TreeNode^.Children[1]>=0 then begin
       StackItem:=@Stack[StackPointer];
       StackItem^.Node:=TreeNode^.Children[1];
       inc(StackPointer);
      end;
      Node:=TreeNode^.Children[0];
      continue;
     end else begin
      if TreeNode^.Children[1]>=0 then begin
       Node:=TreeNode^.Children[1];
       continue;
      end;
     end;
    end;
    break;
   end;
  end;
 end;
end;

procedure TpvScene3D.PrepareLights(const aSwapChainImageIndex:TpvSizeInt;
                                   const aViewMatrix:TpvMatrix4x4;
                                   const aProjectionMatrix:TpvMatrix4x4;
                                   const aViewPortWidth:TpvInt32;
                                   const aViewPortHeight:TpvInt32;
                                   const aFrustum:TpvFrustum;
                                   const aFrustumCulling:boolean=true);
var Index:TpvSizeInt;
   {Lights:TpvScene3D.TLights;
    Light:TpvScene3D.TLight;
    ViewProjectionMatrix:TpvMatrix4x4;
    ViewPort:TpvFloatClipRect;}
    AABBTreeState:TpvBVHDynamicAABBTree.PState;
begin

{ViewProjectionMatrix:=aViewMatrix*aProjectionMatrix;

 ViewPort[0]:=0;
 ViewPort[1]:=0;
 ViewPort[2]:=aViewPortWidth;
 ViewPort[3]:=aViewPortHeight;   }

 //Lights:=fLights[aSwapChainImageIndex];

 fCountIndirectLights[aSwapChainImageIndex]:=0;

 AABBTreeState:=@fLightAABBTreeStates[aSwapChainImageIndex];

 if aFrustumCulling then begin
  CullLightAABBTreeWithFrustum(aSwapChainImageIndex,@aFrustum,AABBTreeState^.TreeNodes,AABBTreeState^.Root);
 end else begin
  CullLightAABBTreeWithFrustum(aSwapChainImageIndex,nil,AABBTreeState^.TreeNodes,AABBTreeState^.Root);
 end;

 if fCountIndirectLights[aSwapChainImageIndex]>0 then begin
// IndirectIntroSort(@fIndirectLights[aSwapChainImageIndex,0],0,fCountIndirectLights[aSwapChainImageIndex],TpvScene3DCompareIndirectLights);
 end;

end;

procedure TpvScene3D.Prepare(const aSwapChainImageIndex:TpvSizeInt;
                             const aRenderPassIndex:TpvSizeInt;
                             const aViewMatrix:TpvMatrix4x4;
                             const aProjectionMatrix:TpvMatrix4x4;
                             const aViewPortWidth:TpvInt32;
                             const aViewPortHeight:TpvInt32;
                             const aFrustumCulling:boolean=true);
var VisibleBit:TPasMPUInt32;
    Frustum:TpvFrustum;
    Group:TpvScene3D.TGroup;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    AABBTreeState:TpvBVHDynamicAABBTree.PState;
begin

 VisibleBit:=TpvUInt32(1) shl (aRenderPassIndex and 31);

 if aFrustumCulling then begin

  for GroupInstance in fGroupInstances do begin
   TPasMPInterlocked.BitwiseAnd(GroupInstance.fVisibleBitmap,not VisibleBit);
  end;

  Frustum.Init(aViewMatrix,aProjectionMatrix);

  AABBTreeState:=@fAABBTreeStates[aSwapChainImageIndex];

  CullAABBTreeWithFrustum(Frustum,AABBTreeState^.TreeNodes,AABBTreeState^.Root,VisibleBit);

 end else begin

  for GroupInstance in fGroupInstances do begin
   TPasMPInterlocked.BitwiseOr(GroupInstance.fVisibleBitmap,VisibleBit);
  end;

 end;

 for Group in fGroups do begin
  Group.Prepare(aSwapChainImageIndex,
                aRenderPassIndex,
                Frustum,
                aFrustumCulling);
 end;

 PrepareLights(aSwapChainImageIndex,
               aViewMatrix,
               aProjectionMatrix,
               aViewPortWidth,
               aViewPortHeight,
               Frustum,
               aFrustumCulling);

end;

procedure TpvScene3D.Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                          const aSwapChainImageIndex:TpvSizeInt;
                          const aRenderPassIndex:TpvSizeInt;
                          const aViewMatrix:TpvMatrix4x4;
                          const aProjectionMatrix:TpvMatrix4x4;
                          const aCommandBuffer:TpvVulkanCommandBuffer;
                          const aPipelineLayout:TpvVulkanPipelineLayout;
                          const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
var VertexStagePushConstants:TpvScene3D.TVertexStagePushConstants;
    Group:TpvScene3D.TGroup;
    VisibleBit:TPasMPUInt32;
    Pipeline:TpvVulkanPipeline;
begin

 Pipeline:=nil;

 VisibleBit:=TpvUInt32(1) shl (aRenderPassIndex and 31);

 VertexStagePushConstants.ViewMatrix:=aViewMatrix;
 VertexStagePushConstants.ProjectionMatrix:=aProjectionMatrix;

 if fSwapChainImageBufferMemoryBarriers[aSwapChainImageIndex].Count>0 then begin
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or pvApplication.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                    0,
                                    0,
                                    nil,
                                    fSwapChainImageBufferMemoryBarriers[aSwapChainImageIndex].Count,
                                    @fSwapChainImageBufferMemoryBarriers[aSwapChainImageIndex].Items[0],
                                    0,
                                    nil);
  fSwapChainImageBufferMemoryBarriers[aSwapChainImageIndex].Count:=0;
 end;

 aCommandBuffer.CmdPushConstants(aPipelineLayout.Handle,
                                 TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT),
                                 0,
                                 SizeOf(TpvScene3D.TVertexStagePushConstants),
                                 @VertexStagePushConstants);

 aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                      aPipelineLayout.Handle,
                                      2,
                                      1,
                                      @fGlobalVulkanDescriptorSets[aSwapChainImageIndex].Handle,
                                      0,
                                      nil);

 for Group in fGroups do begin
  Group.Draw(aGraphicsPipelines,
             aSwapChainImageIndex,
             aRenderPassIndex,
             aCommandBuffer,
             Pipeline,
             aPipelineLayout,
             aMaterialAlphaModes);
 end;

end;

procedure TpvScene3D.GetZNearZFar(const aViewMatrix:TpvMatrix4x4;
                                  const aAspectRatio:TpvScalar;
                                  out aZNear:TpvScalar;
                                  out aZFar:TpvScalar);
var x,y,z:TpVInt32;
    InverseViewMatrix:TpvMatrix4x4;
    CameraPosition,CameraDirection,Corner:TpvVector3;
    Dot:TpvScalar;
begin

 InverseViewMatrix:=aViewMatrix.Inverse;

 CameraPosition:=PpvVector3(@InverseViewMatrix.RawComponents[3,0])^;

 CameraDirection:=-PpvVector3(@InverseViewMatrix.RawComponents[2,0])^.Normalize;

 Corner:=fBoundingBox.Min;
 Dot:=CameraDirection.Dot(Corner-CameraPosition);
 aZNear:=Dot;
 aZFar:=Dot;

 for x:=0 to 1 do begin
  for y:=0 to 1 do begin
   for z:=0 to 1 do begin
    Corner:=TpvVector3.Create(fBoundingBox.MinMax[x].x,fBoundingBox.MinMax[y].y,fBoundingBox.MinMax[z].z);
    Dot:=CameraDirection.Dot(Corner-CameraPosition);
    aZNear:=Min(aZNear,Dot);
    aZFar:=Max(aZFar,Dot);
   end;
  end;
 end;

 aZNear:=Max(0.1,aZNear-0.1);
 aZFar:=Max(0.2,aZFar+0.1);

end;

initialization
end.

