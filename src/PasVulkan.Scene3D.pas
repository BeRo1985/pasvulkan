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
     PasVulkan.IDManager,
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
       const MaxRenderPassIndices=32;
             MaxVisibleLights=65536;
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
                    Flags=10;
                    MaterialID=11;
            end;
            TCachedVertexAttributeBindingLocations=class
             public
              const Position=0;
                    MaterialID=1;
                    NormalSign=2;
                    Tangent=3;
                    TexCoord0=4;
                    TexCoord1=5;
                    Color0=6;
                    ModelScaleDummy=7;
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
            TView=packed record
             ViewMatrix:TpvMatrix4x4;
             ProjectionMatrix:TpvMatrix4x4;
             InverseViewMatrix:TpvMatrix4x4;
             InverseProjectionMatrix:TpvMatrix4x4;
            end;
            PView=^TView;
            TViews=TpvDynamicArray<TView>;
            TScalarSum=record
             public
              x:TpvDouble;
              FactorSum:TpvDouble;
             public
              procedure Clear; inline;
              procedure Add(const aX,aFactor:TpvDouble); inline;
              function Get(const aDefaultX:TpvDouble=0.0):TpvDouble; inline;
            end;
            TVector2Sum=record
             public
              x:TpvDouble;
              y:TpvDouble;
              FactorSum:TpvDouble;
             public
              procedure Clear; inline;
              procedure Add(const aX,aY,aFactor:TpvDouble); overload; inline;
              procedure Add(const aVector:TpvVector2;const aFactor:TpvDouble); overload; inline;
              function Get(const aDefaultX:TpvDouble=0.0;const aDefaultY:TpvDouble=0.0):TpvVector2; overload; inline;
              function Get(const aDefault:TpvVector2):TpvVector2; overload; inline;
            end;
            TVector3Sum=record
             public
              x:TpvDouble;
              y:TpvDouble;
              z:TpvDouble;
              FactorSum:TpvDouble;
             public
              procedure Clear; inline;
              procedure Add(const aX,aY,aZ,aFactor:TpvDouble); overload; inline;
              procedure Add(const aVector:TpvVector3;const aFactor:TpvDouble); overload; inline;
              function Get(const aDefaultX:TpvDouble=0.0;const aDefaultY:TpvDouble=0.0;const aDefaultZ:TpvDouble=0.0):TpvVector3; overload; inline;
              function Get(const aDefault:TpvVector3):TpvVector3; overload; inline;
            end;
            TVector4Sum=record
             public
              x:TpvDouble;
              y:TpvDouble;
              z:TpvDouble;
              w:TpvDouble;
              FactorSum:TpvDouble;
             public
              procedure Clear; inline;
              procedure Add(const aX,aY,aZ,aW,aFactor:TpvDouble); overload; inline;
              procedure Add(const aVector:TpvVector4;const aFactor:TpvDouble); overload; inline;
              function Get(const aDefaultX:TpvDouble=0.0;const aDefaultY:TpvDouble=0.0;const aDefaultZ:TpvDouble=0.0;const aDefaultW:TpvDouble=0.0):TpvVector4; overload; inline;
              function Get(const aDefault:TpvVector4):TpvVector4; overload; inline;
            end;
       const MaxViews=65536 div SizeOf(TView);
       type TID=TpvUInt32;
            TIDManager=class(TpvGenericIDManager<TID>);
            TGlobalViewUniformBuffer=record
             Items:array[0..MaxViews-1] of TView;
            end;
            PGlobalViewUniformBuffer=^TGlobalViewUniformBuffer;
            TGlobalVulkanViewUniformBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
            TMeshComputeStagePushConstants=record
             IndexOffset:UInt32;
             CountIndices:UInt32;
            end;
            PMeshComputeStagePushConstants=^TMeshComputeStagePushConstants;
            TVertexStagePushConstants=record
             ViewBaseIndex:UInt32;
             CountViews:UInt32;
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
               CountJointBlocks:TpvUInt16;           // + 2 = 58 (unsigned 16-bit count of joint blocks)
               Flags:TpvUInt16;                      // + 2 = 60 (unsigned 16-bit flags)
               MaterialID:TpvUInt32;                 // + 4 = 64 (unsigned 32-bit material ID)
              );                                     //  ==   ==
              true:(                                 //  64   64 per vertex
               Padding:array[0..63] of TpvUInt8;
              );
            end;
            PVertex=^TVertex;
            TVertices=array of TVertex;
            TCachedVertex=packed record              // Minimum required cached vertex structure for to be GLTF 2.0 conformant
             case boolean of
              false:(
               Position:TpvVector3;                  //  12   12 (32-bit float 3D vector)
               MaterialID:TpvUInt32;                 // + 4 = 16 (unsigned 32-bit material ID)
               NormalSign:TInt16Vector4;             // + 8 = 24 (signed 16-bit Normal + TBN sign)
               Tangent:TInt16Vector4;                // + 8 = 32 (signed 16-bit Tangent + Placeholder)
               TexCoord0:TpvVector2;                 // + 8 = 40 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               TexCoord1:TpvVector2;                 // + 8 = 48 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               Color0:TpvHalfFloatVector4;           // + 8 = 56 (must be at least half-float for HDR)
               ModelScaleDummy:TpvHalfFloatVector4;  // + 8 = 64 (half-float)
              );                                     //  ==   ==
              true:(                                 //  64   64 per vertex
               Padding:array[0..63] of TpvUInt8;
              );
            end;
            PCachedVertex=^TCachedVertex;
            TCachedVertices=array of TCachedVertex;
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
            TOnSetRenderPassResources=procedure(const aCommandBuffer:TpvVulkanCommandBuffer;
                                                const aPipelineLayout:TpvVulkanPipelineLayout;
                                                const aRenderPassIndex:TpvSizeInt;
                                                const aPreviousInFlightFrameIndex:TpvSizeInt;
                                                const aInFlightFrameIndex:TpvSizeInt) of object;
            { TBaseObject }
            TBaseObject=class(TpvResource)
             private
              fSceneInstance:TpvScene3D;
              fID:TID;
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
              property ID:TID read fID;
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
              type TKind=
                    (
                     WhiteTexture=0,
                     DefaultNormalMapTexture=1,
                     ResourceTexture=2
                    );
                   THashData=packed record
                    MessageDigest:TpvHashSHA3.TMessageDigest;
                   end;
                   PHashData=^THashData;
             private
              fKind:TKind;
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
              procedure AssignFromWhiteTexture;
              procedure AssignFromDefaultNormalMapTexture;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceImage:TPasGLTF.TImage);
            end;
            TImageClass=class of TImage;
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
              procedure AssignFromDefault;
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
              procedure AssignFromWhiteTexture;
              procedure AssignFromDefaultNormalMapTexture;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceTexture:TPasGLTF.TTexture;const aImageMap:TImages;const aSamplerMap:TSamplers);
              function GetDescriptorImageInfo(const aSRGB:boolean):TVkDescriptorImageInfo;
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
                   TAlignedMatrix3x2=array[0..2] of TpvVector2;
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
                            function ToAlignedMatrix3x2:TAlignedMatrix3x2;
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
                   TIridescence=record
                    Active:boolean;
                    Factor:TpvFloat;
                    Texture:TTextureReference;
                    Ior:TpvFloat;
                    ThicknessMinimum:TpvFloat;
                    ThicknessMaximum:TpvFloat;
                    ThicknessTexture:TTextureReference;
                   end;
                   TTransmission=record
                    Active:boolean;
                    Factor:TpvFloat;
                    Texture:TTextureReference;
                   end;
                   TVolume=record
                    Active:boolean;
                    ThicknessFactor:TpvFloat;
                    ThicknessTexture:TTextureReference;
                    AttenuationColor:TpvVector3;
                    AttenuationDistance:TpvFloat;
                   end;
                   TShaderData=packed record // 2048 bytes
                    case boolean of
                     false:(
                      BaseColorFactor:TpvVector4;
                      SpecularFactor:TpvVector4; // actually TpvVector3, but for easier and more convenient alignment reasons a TpvVector4
                      EmissiveFactor:TpvVector4; // w = EmissiveStrength
                      MetallicRoughnessNormalScaleOcclusionStrengthFactor:TpvVector4;
                      SheenColorFactorSheenIntensityFactor:TpvVector4;
                      ClearcoatFactorClearcoatRoughnessFactor:TpvVector4;
                      IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum:TpvVector4;
                      IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance:TpvVector4;
                      VolumeAttenuationColor:TpvVector4;
                      // uvec4 AlphaCutOffFlags begin
                       AlphaCutOff:TpvFloat; // for with uintBitsToFloat on GLSL code side
                       Flags:TpvUInt32;
                       Textures0:TPasGLTFUInt32;
                       Textures1:TPasGLTFUInt32;
                       // uvec4 uAlphaCutOffFlags end
                       Textures:array[0..15] of TpvInt32;
                       TextureTransforms:array[0..15] of TAlignedMatrix3x2;
                     );
                     true:(
                      //Padding:array[0..2047] of TpvUInt8;
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
                    EmissiveFactor:TpvVector4; // w = EmissiveStrength
                    EmissiveTexture:TTextureReference;
                    PBRMetallicRoughness:TPBRMetallicRoughness;
                    PBRSpecularGlossiness:TPBRSpecularGlossiness;
                    PBRSheen:TPBRSheen;
                    PBRClearCoat:TPBRClearCoat;
                    Unlit:TUnlit;
                    IOR:TpvFloat;
                    Iridescence:TIridescence;
                    Transmission:TTransmission;
                    Volume:TVolume;
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
                     EmissiveFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
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
                     Iridescence:(
                      Active:false;
                      Factor:0.0;
                      Texture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      Ior:1.3;
                      ThicknessMinimum:100.0;
                      ThicknessMaximum:400.0;
                      ThicknessTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     );
                     Transmission:(
                      Active:false;
                      Factor:0.0;
                      Texture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     );
                     Volume:(
                      Active:false;
                      ThicknessFactor:0.0;
                      ThicknessTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      AttenuationColor:(x:1.0;y:1.0;z:1.0);
                      AttenuationDistance:Infinity;
                     );
                    );
                   DefaultShaderData:TShaderData=
                    (
                     BaseColorFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SpecularFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     EmissiveFactor:(x:0.0;y:0.0;z:0.0;w:1.0);
                     MetallicRoughnessNormalScaleOcclusionStrengthFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SheenColorFactorSheenIntensityFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     ClearcoatFactorClearcoatRoughnessFactor:(x:0.0;y:0.0;z:1.0;w:1.0);
                     IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum:(x:1.5;y:0.0;z:1.3;w:100.0);
                     IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance:(x:400.0;y:0.0;z:0.0;w:Infinity);
                     VolumeAttenuationColor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     AlphaCutOff:1.0;
                     Flags:0;
                     Textures0:0;
                     Textures1:0;
                     Textures:(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
                     TextureTransforms:(
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0)),
                      ((x:1.0;y:0.0),(x:0.0;y:1.0),(x:0.0;y:0.0))
                     );
                    );
             private
              fData:TData;
              fShaderData:TShaderData;
              fLock:TPasMPSpinLock;
              fVisible:boolean;
{             fShaderDataUniformBlockBuffer:TpvVulkanBuffer;
              fVulkanDescriptorPool:TpvVulkanDescriptorPool;
              fVulkanDescriptorSet:TpvVulkanDescriptorSet;}
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure AssignFromEmpty;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMaterial:TPasGLTF.TMaterial;const aTextureMap:TTextures);
              procedure FillShaderData;
            end;
            TMaterials=TpvObjectGenericList<TMaterial>;
            TCameraData=record
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
             public
              case Type_:TCameraData.TType of
               TCameraData.TType.Orthographic:(
                Orthographic:TOrthographic;
               );
               TCameraData.TType.Perspective:(
                Perspective:TPerspective;
               );
            end;
            PCameraData=^TCameraData;
            TLightData=record
             public
              type TType=
                    (
                     None=0,
                     Directional=1,
                     Point=2,
                     Spot=3,
                     PrimaryDirectional=4
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
            PLightData=^TLightData;
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
              procedure Update(const aInFlightFrameIndex:TpvSizeInt);
            end;
            TLightBuffers=array[0..MaxInFlightFrames-1] of TLightBuffer;
            { TLight }
            TLight=class
             private
              fSceneInstance:TpvScene3D;
              fVisible:boolean;
              fData:TpvScene3D.TLightData;
              fDataPointer:TpvScene3D.PLightData;
              fLight:pointer;
              fInstanceLight:pointer;
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
              property DataPointer:TpvScene3D.PLightData read fDataPointer write fDataPointer;
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
                                   None=0,
                                   Translation,
                                   Rotation,
                                   Scale,
                                   Weights,
                                   Pointer_,
                                   PointerNodeRotation,
                                   PointerNodeScale,
                                   PointerNodeTranslation,
                                   PointerNodeWeights,
                                   PointerMeshWeights,
                                   PointerCameraOrthographicXMag,
                                   PointerCameraOrthographicYMag,
                                   PointerCameraOrthographicZFar,
                                   PointerCameraOrthographicZNear,
                                   PointerCameraPerspectiveAspectRatio,
                                   PointerCameraPerspectiveYFov,
                                   PointerCameraPerspectiveZFar,
                                   PointerCameraPerspectiveZNear,
                                   PointerMaterialPBRMetallicRoughnessBaseColorFactor,
                                   PointerMaterialPBRMetallicRoughnessMetallicFactor,
                                   PointerMaterialPBRMetallicRoughnessRoughnessFactor,
                                   PointerMaterialAlphaCutOff,
                                   PointerMaterialEmissiveFactor,
                                   PointerMaterialNormalTextureScale,
                                   PointerMaterialOcclusionTextureStrength,
                                   PointerPunctualLightColor,
                                   PointerPunctualLightIntensity,
                                   PointerPunctualLightRange,
                                   PointerPunctualLightSpotInnerConeAngle,
                                   PointerPunctualLightSpotOuterConeAngle,
                                   PointerMaterialPBRClearCoatFactor,
                                   PointerMaterialPBRClearCoatRoughnessFactor,
                                   PointerMaterialEmissiveStrength,
                                   PointerMaterialIOR,
                                   PointerMaterialPBRIridescenceFactor,
                                   PointerMaterialPBRIridescenceIor,
                                   PointerMaterialPBRIridescenceMinimum,
                                   PointerMaterialPBRIridescenceMaximum,
                                   PointerMaterialPBRSheenColorFactor,
                                   PointerMaterialPBRSheenRoughnessFactor,
                                   PointerMaterialPBRSpecularFactor,
                                   PointerMaterialPBRSpecularColorFactor,
                                   PointerMaterialPBRTransmissionFactor,
                                   PointerMaterialPBRVolumeThicknessFactor,
                                   PointerMaterialPBRVolumeAttenuationDistance,
                                   PointerMaterialPBRVolumeAttenuationColor,
                                   PointerTextureOffset,
                                   PointerTextureRotation,
                                   PointerTextureScale
                                  );
                                 TInterpolation=
                                  (
                                   Linear,
                                   Step,
                                   CubicSpline
                                  );
                           public
                            Name:TpvUTF8String;
                            Target:TTarget;
                            TargetPointer:TpvUTF8String;
                            TargetIndex:TpvSizeInt;
                            TargetSubIndex:TpvSizeInt;
                            Interpolation:TInterpolation;
                            InputTimeArray:TpvDoubleDynamicArray;
                            OutputScalarArray:TpvFloatDynamicArray;
                            OutputVector2Array:TpvVector2Array;
                            OutputVector3Array:TpvVector3Array;
                            OutputVector4Array:TpvVector4Array;
                            Last:TPasGLTFSizeInt;
                          end;
                          PChannel=^TChannel;
                          TChannels=array of TChannel;
                          TDefaultChannel=record
                           public
                            Target:TpvScene3D.TGroup.TAnimation.TChannel.TTarget;
                            TargetIndex:TpvSizeInt;
                            TargetSubIndex:TpvSizeInt;
                          end;
                          PDefaultChannel=^TDefaultChannel;
                          TDefaultChannels=array of TDefaultChannel;
                    private
                     fIndex:TpvSizeInt;
                     fChannels:TChannels;
                     fDefaultChannels:TDefaultChannels;
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
                   { TCamera }
                   TCamera=class(TGroupObject)
                    private
                     fIndex:TpvSizeInt;
                     fCameraData:TpvScene3D.TCameraData;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceCamera:TPasGLTF.TCamera);
                    public
                     property CameraData:TpvScene3D.TCameraData read fCameraData write fCameraData;
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
                          TReferencedByNodes=TpvDynamicArray<TPasGLTFSizeInt>;
                    private
                     fIndex:TpvSizeInt;
                     fPrimitives:TPrimitives;
                     fBoundingBox:TpvAABB;
                     fWeights:TpvFloatDynamicArray;
                     fNodeMeshInstances:TpvSizeInt;
                     fReferencedByNodes:TReferencedByNodes;
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
                     type TPrimitiveIndexRange=record
                           PrimitiveTopology:TPrimitiveTopology;
                           Index:TpvSizeInt;
                           Count:TpvSizeInt;
                           Node:TpvSizeInt;
                          end;
                          PPrimitiveIndexRange=^TPrimitiveIndexRange;
                          TPrimitiveIndexRanges=TpvDynamicArray<TpvScene3D.TGroup.TScene.TPrimitiveIndexRange>;
                          PPrimitiveIndexRanges=^TPrimitiveIndexRanges;
                          { TMaterial }
                          TMaterial=class
                           public
                           private
                            fMaterial:TpvScene3D.TMaterial;
                            fPrimitiveIndexRanges:TpvScene3D.TGroup.TScene.TPrimitiveIndexRanges;
                            fCombinedPrimitiveIndexRanges:TpvScene3D.TGroup.TScene.TPrimitiveIndexRanges;
                            fCombinedPrimitiveUniqueIndexRanges:TpvScene3D.TGroup.TScene.TPrimitiveIndexRanges;
                            fStartIndex:TpvSizeInt;
                            fCountIndices:TpvSizeInt;
                            fStartUniqueIndex:TpvSizeInt;
                            fCountUniqueIndices:TpvSizeInt;
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
                     fCombinedPrimitiveUniqueIndexRanges:TpvScene3D.TGroup.TScene.TPrimitiveIndexRanges;
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
                                  public
                                   type TType=
                                         (
                                          None,
                                          Node,
                                          Light,
                                          Camera
                                         );
                                  private
                                   fType:TType;
                                   fTarget:Pointer;
                                   fOverwrite:TpvSizeInt;
                                 end;
                                 TChannels=TpvObjectGenericList<TChannel>;
                           private
                            fFactor:TpvFloat;
                            fTime:TpvDouble;
                            fShadowTime:TpvDouble;
                            fComplete:LongBool;
                            fChannels:TChannels;
                           public
                            constructor Create; reintroduce;
                            destructor Destroy; override;
                           published
                            property Factor:TpvFloat read fFactor write fFactor;
                            property Time:TpvDouble read fTime write fTime;
                            property ShadowTime:TpvDouble read fShadowTime write fShadowTime;
                            property Complete:LongBool read fComplete write fComplete;
                          end;
                          TAnimations=array of TpvScene3D.TGroup.TInstance.TAnimation;
                          TNode=record
                           public
                            type TOverwriteFlag=
                                  (
                                   Defaults,
                                   DefaultTranslation,
                                   DefaultRotation,
                                   DefaultScale,
                                   DefaultWeights,
                                   Translation,
                                   Rotation,
                                   Scale,
                                   Weights,
                                   Pointer_
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
                            OverwriteWeightsSum:TpvDoubleDynamicArray;
                            WorkWeights:TpvFloatDynamicArray;
                            WorkMatrix:TpvMatrix4x4;
                            VisibleBitmap:TpvUInt32;
                            Light:TpvScene3D.TLight;
                            BoundingBoxes:array[0..MaxInFlightFrames-1] of TpvAABB;
                            BoundingBoxFilled:array[0..MaxInFlightFrames-1] of boolean;
                            CacheVerticesGenerations:array[0..MaxInFlightFrames-1] of TpvUInt32;
                            CacheVerticesGeneration:TpvUInt32;
                            CacheVerticesDirtyCounter:TpvUInt32;
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
                          { TLight }
                          TLight=class
                           public
                            type TOverwriteFlag=
                                         (
                                          Defaults,
                                          DefaultColor,
                                          DefaultIntensity,
                                          DefaultRange,
                                          DefaultSpotInnerConeAngle,
                                          DefaultSpotOuterConeAngle,
                                          Color,
                                          Intensity,
                                          Range,
                                          SpotInnerConeAngle,
                                          SpotOuterConeAngle
                                         );
                                        TOverwriteFlags=set of TOverwriteFlag;
                                        TOverwrite=record
                                         public
                                          Flags:TOverwriteFlags;
                                          Factor:TpvFloat;
                                          Color:TpvVector3;
                                          Intensity:TpvFloat;
                                          Range:TpvFloat;
                                          SpotInnerConeAngle:TpvFloat;
                                          SpotOuterConeAngle:TpvFloat;
                                        end;
                                        POverwrite=^TOverwrite;
                                        TOverwrites=array of TOverwrite;
                           private
                            fInstance:TInstance;
                            fLight:TpvScene3D.TGroup.TLight;
                            fData:TpvScene3D.TLightData;
                            fWorkData:TpvScene3D.TLightData;
                            fEffectiveData:TpvScene3D.PLightData;
                            fOverwrites:TOverwrites;
                            fCountOverwrites:TpvSizeInt;
                           public
                            constructor Create(const aInstance:TpvScene3D.TGroup.TInstance;const aLight:TpvScene3D.TGroup.TLight);
                            destructor Destroy; override;
                            procedure Update;
                           public
                            property Light:TpvScene3D.TGroup.TLight read fLight;
                            property Data:TpvScene3D.TLightData read fData write fData;
                            property WorkData:TpvScene3D.TLightData read fWorkData write fWorkData;
                            property EffectiveData:TpvScene3D.PLightData read fEffectiveData;
                          end;
                          TLights=TpvObjectGenericList<TpvScene3D.TGroup.TInstance.TLight>;
                          { TCamera }
                          TCamera=class
                           public
                            type TOverwriteFlag=
                                         (
                                          Defaults,
                                          DefaultOrthographicXMag,
                                          DefaultOrthographicYMag,
                                          DefaultOrthographicZFar,
                                          DefaultOrthographicZNear,
                                          DefaultPerspectiveAspectRatio,
                                          DefaultPerspectiveYFov,
                                          DefaultPerspectiveZFar,
                                          DefaultPerspectiveZNear,
                                          OrthographicXMag,
                                          OrthographicYMag,
                                          OrthographicZFar,
                                          OrthographicZNear,
                                          PerspectiveAspectRatio,
                                          PerspectiveYFov,
                                          PerspectiveZFar,
                                          PerspectiveZNear
                                         );
                                        TOverwriteFlags=set of TOverwriteFlag;
                                        TOverwrite=record
                                         public
                                          Flags:TOverwriteFlags;
                                          Factor:TpvFloat;
                                          OrthographicXMag:TpvFloat;
                                          OrthographicYMag:TpvFloat;
                                          OrthographicZFar:TpvFloat;
                                          OrthographicZNear:TpvFloat;
                                          PerspectiveAspectRatio:TpvFloat;
                                          PerspectiveYFov:TpvFloat;
                                          PerspectiveZFar:TpvFloat;
                                          PerspectiveZNear:TpvFloat;
                                        end;
                                        POverwrite=^TOverwrite;
                                        TOverwrites=array of TOverwrite;
                           private
                            fInstance:TInstance;
                            fCamera:TpvScene3D.TGroup.TCamera;
                            fData:TpvScene3D.TCameraData;
                            fWorkData:TpvScene3D.TCameraData;
                            fEffectiveData:TpvScene3D.PCameraData;
                            fOverwrites:TOverwrites;
                            fCountOverwrites:TpvSizeInt;
                           public
                            constructor Create(const aInstance:TpvScene3D.TGroup.TInstance;const aCamera:TpvScene3D.TGroup.TCamera);
                            destructor Destroy; override;
                            procedure Update;
                           public
                            property Camera:TpvScene3D.TGroup.TCamera read fCamera;
                            property Data:TpvScene3D.TCameraData read fData write fData;
                            property WorkData:TpvScene3D.TCameraData read fWorkData write fWorkData;
                            property EffectiveData:TpvScene3D.PCameraData read fEffectiveData;
                          end;
                          TCameras=TpvObjectGenericList<TpvScene3D.TGroup.TInstance.TCamera>;
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
                            procedure Update(const aInFlightFrameIndex:TpvSizeInt);
                           published
                            property NodeMatricesBuffer:TpvVulkanBuffer read fNodeMatricesBuffer;
                            property MorphTargetVertexWeightsBuffer:TpvVulkanBuffer read fMorphTargetVertexWeightsBuffer;
                          end;
                          TVulkanDatas=array[0..MaxInFlightFrames-1] of TVulkanData;
                    private
                     fGroup:TGroup;
                     fLock:TPasMPSpinLock;
                     fActive:boolean;
                     fPreviousActive:boolean;
                     fScene:TPasGLTFSizeInt;
                     fAnimations:TpvScene3D.TGroup.TInstance.TAnimations;
                     fNodes:TpvScene3D.TGroup.TInstance.TNodes;
                     fSkins:TpvScene3D.TGroup.TInstance.TSkins;
                     fCameras:TpvScene3D.TGroup.TInstance.TCameras;
                     fLights:TpvScene3D.TGroup.TInstance.TLights;
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
                     fVulkanComputeDescriptorPool:TpvVulkanDescriptorPool;
                     fVulkanComputeDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                     fScenes:array[0..MaxInFlightFrames-1] of TpvScene3D.TGroup.TScene;
                     fActives:array[0..MaxInFlightFrames-1] of boolean;
                     fAABBTreeProxy:TpvSizeInt;
                     fVisibleBitmap:TPasMPUInt32;
                     fCacheVerticesNodeDirtyBitmap:array of TpvUInt32;
                     function GetAutomation(const aIndex:TPasGLTFSizeInt):TpvScene3D.TGroup.TInstance.TAnimation;
                     procedure SetScene(const aScene:TpvSizeInt);
                     function GetScene:TpvScene3D.TGroup.TScene;
                     procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                       const aRenderPassIndex:TpvSizeInt;
                                       const aFrustums:TpvFrustumDynamicArray);
                     procedure UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                                    const aInFlightFrameIndex:TpvSizeInt;
                                                    const aCommandBuffer:TpvVulkanCommandBuffer;
                                                    const aPipelineLayout:TpvVulkanPipelineLayout);
                     procedure Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                                    const aPreviousInFlightFrameIndex:TpvSizeInt;
                                    const aInFlightFrameIndex:TpvSizeInt;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aCommandBuffer:TpvVulkanCommandBuffer;
                                    var aPipeline:TpvVulkanPipeline;
                                    const aPipelineLayout:TpvVulkanPipelineLayout;
                                    const aOnSetRenderPassResources:TOnSetRenderPassResources;
                                    const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
                    public
                     constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
                     destructor Destroy; override;
                     procedure AfterConstruction; override;
                     procedure BeforeDestruction; override;
                     procedure Upload; override;
                     procedure Unload; override;
                     procedure UpdateInvisible;
                     procedure Update(const aInFlightFrameIndex:TpvSizeInt);
                    published
                     property Group:TGroup read fGroup write fGroup;
                     property Active:boolean read fActive write fActive;
                     property Scene:TpvSizeInt read fScene write SetScene;
                     property Cameras:TpvScene3D.TGroup.TInstance.TCameras read fCameras;
                     property Lights:TpvScene3D.TGroup.TInstance.TLights read fLights;
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
              fAnimations:TpvScene3D.TGroup.TAnimations;
              fCameras:TpvScene3D.TGroup.TCameras;
              fMeshes:TpvScene3D.TGroup.TMeshes;
              fSkins:TpvScene3D.TGroup.TSkins;
              fLights:TpvScene3D.TGroup.TLights;
              fNodes:TpvScene3D.TGroup.TNodes;
              fScenes:TpvScene3D.TGroup.TScenes;
              fScene:TpvScene3D.TGroup.TScene;
              fVertices:TGroupVertices;
              fIndices:TGroupIndices;
              fPerMaterialCondensedIndices:TGroupIndices;
              fPerMaterialCondensedUniqueIndices:TGroupIndices;
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
              fVulkanCachedVertexBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
              //fVulkanIndexBuffer:TpvVulkanBuffer;
              fVulkanMaterialIndexBuffer:TpvVulkanBuffer;
              fVulkanMaterialUniqueIndexBuffer:TpvVulkanBuffer;
              fVulkanMorphTargetVertexBuffer:TpvVulkanBuffer;
              fVulkanJointBlockBuffer:TpvVulkanBuffer;
              fInstanceListLock:TPasMPSlimReaderWriterLock;
              fInstances:TInstances;
              fBoundingBox:TpvAABB;
              fSetGroupResourcesDone:array[0..MaxRenderPassIndices-1] of boolean;
              fCachedVerticesUpdated:boolean;
              procedure ConstructBuffers;
              procedure CollectMaterialPrimitives;
              procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                const aRenderPassIndex:TpvSizeInt;
                                const aFrustums:TpvFrustumDynamicArray);
              procedure SetGroupResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aPipelineLayout:TpvVulkanPipelineLayout;
                                          const aRenderPassIndex:TpvSizeInt;
                                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                                          const aInFlightFrameIndex:TpvSizeInt);
              procedure UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                             const aInFlightFrameIndex:TpvSizeInt;
                                             const aCommandBuffer:TpvVulkanCommandBuffer;
                                             const aPipelineLayout:TpvVulkanPipelineLayout);
              procedure Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                             const aPreviousInFlightFrameIndex:TpvSizeInt;
                             const aInFlightFrameIndex:TpvSizeInt;
                             const aRenderPassIndex:TpvSizeInt;
                             const aCommandBuffer:TpvVulkanCommandBuffer;
                             var aPipeline:TpvVulkanPipeline;
                             const aPipelineLayout:TpvVulkanPipelineLayout;
                             const aOnSetRenderPassResources:TOnSetRenderPassResources;
                             const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure Update(const aInFlightFrameIndex:TpvSizeInt);
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument);
              function BeginLoad(const aStream:TStream):boolean; override;
              function EndLoad:boolean; override;
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
            TImageIDHashMap=TpvHashMap<TID,TImage>;
            TSamplerIDHashMap=TpvHashMap<TID,TSampler>;
            TTextureIDHashMap=TpvHashMap<TID,TTexture>;
            TMaterialIDHashMap=TpvHashMap<TID,TMaterial>;
            TImageHashMap=TpvHashMap<TImage.THashData,TImage>;
            TSamplerHashMap=TpvHashMap<TSampler.THashData,TSampler>;
            TTextureHashMap=TpvHashMap<TTexture.THashData,TTexture>;
            TMaterialHashMap=TpvHashMap<TMaterial.THashData,TMaterial>;
            TBufferMemoryBarriers=TpvDynamicArray<TVkBufferMemoryBarrier>;
            TInFlightFrameBufferMemoryBarriers=array[0..MaxInFlightFrames-1] of TBufferMemoryBarriers;
            TMaterialBufferData=array[0..65535] of TMaterial.TShaderData;
      private
       fLock:TPasMPSpinLock;
       fUploaded:TPasMPBool32;
       fDefaultSampler:TSampler;
       fWhiteImage:TImage;
       fWhiteTexture:TTexture;
       fDefaultNormalMapImage:TImage;
       fDefaultNormalMapTexture:TTexture;
       fMeshComputeVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanViews:array[0..MaxInFlightFrames-1] of TGlobalViewUniformBuffer;
       fGlobalVulkanViewUniformBuffers:TGlobalVulkanViewUniformBuffers;
       fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
       fMaterialBufferData:TMaterialBufferData;
       fVulkanMaterialDataBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanMaterialUniformBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fTechniques:TpvTechniques;
       fImageListLock:TPasMPSlimReaderWriterLock;
       fImages:TImages;
       fImageIDManager:TIDManager;
       fImageIDHashMap:TImageIDHashMap;
       fImageHashMap:TImageHashMap;
       fSamplerListLock:TPasMPSlimReaderWriterLock;
       fSamplers:TSamplers;
       fSamplerIDManager:TIDManager;
       fSamplerIDHashMap:TSamplerIDHashMap;
       fSamplerHashMap:TSamplerHashMap;
       fTextureListLock:TPasMPSlimReaderWriterLock;
       fTextures:TTextures;
       fTextureIDManager:TIDManager;
       fTextureIDHashMap:TTextureIDHashMap;
       fTextureHashMap:TTextureHashMap;
       fMaterialListLock:TPasMPSlimReaderWriterLock;
       fMaterials:TMaterials;
       fMaterialIDManager:TIDManager;
       fMaterialIDHashMap:TMaterialIDHashMap;
       fMaterialHashMap:TMaterialHashMap;
       fEmptyMaterial:TpvScene3D.TMaterial;
       fLights:array[0..MaxInFlightFrames-1] of TpvScene3D.TLights;
       fCountLights:array[0..MaxInFlightFrames-1] of TpvSizeInt;
       fIndirectLights:array[0..MaxInFlightFrames-1,0..MaxVisibleLights-1] of TpvScene3D.TLight;
       fCountIndirectLights:array[0..MaxInFlightFrames-1] of TpvSizeInt;
       fGroupListLock:TPasMPSlimReaderWriterLock;
       fGroups:TGroups;
       fGroupInstanceListLock:TPasMPSlimReaderWriterLock;
       fGroupInstances:TGroup.TInstances;
       fLightAABBTree:TpvBVHDynamicAABBTree;
       fLightAABBTreeGeneration:TpvUInt32;
       fLightAABBTreeStates:array[0..MaxInFlightFrames-1] of TpvBVHDynamicAABBTree.TState;
       fLightAABBTreeStateGenerations:array[0..MaxInFlightFrames-1] of TpvUInt32;
       fLightBuffers:TpvScene3D.TLightBuffers;
       fAABBTree:TpvBVHDynamicAABBTree;
       fAABBTreeStates:array[0..MaxInFlightFrames-1] of TpvBVHDynamicAABBTree.TState;
       fBoundingBox:TpvAABB;
       fInFlightFrameBufferMemoryBarriers:TInFlightFrameBufferMemoryBarriers;
       fPreviousViews:TViews;
       fViews:TViews;
       fVertexStagePushConstants:array[0..MaxRenderPassIndices-1] of TpvScene3D.TVertexStagePushConstants;
       fSetGlobalResourcesDone:array[0..MaxRenderPassIndices-1] of boolean;
       fUseBufferDeviceAddress:boolean;
       fHasTransmission:boolean;
       fImageInfos:array[0..65535] of TVkDescriptorImageInfo;
       procedure AddInFlightFrameBufferMemoryBarrier(const aInFlightFrameIndex:TpvSizeInt;
                                                      const aBuffer:TpvVulkanBuffer);
       procedure CullAABBTreeWithFrustums(const aFrustums:TpvFrustumDynamicArray;
                                          const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                          const aRoot:TpvSizeInt;
                                          const aVisibleBit:TPasMPUInt32);
       procedure CullLightAABBTreeWithFrustums(const aInFlightFrameIndex:TpvSizeInt;
                                               const aFrustums:TpvFrustumDynamicArray;
                                               const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                               const aRoot:TpvSizeInt);
       procedure CollectLightAABBTreeLights(const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                            const aRoot:TpvSizeInt;
                                            var aLightItemArray:TpvScene3D.TLightItems);
       function GetLightUserDataIndex(const aUserData:TpvPtrInt):TpvUInt32;
       procedure SetGlobalResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                    const aPipelineLayout:TpvVulkanPipelineLayout;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aInFlightFrameIndex:TpvSizeInt);
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aUseBufferDeviceAddress:boolean=true); reintroduce;
       destructor Destroy; override;
       procedure Upload;
       procedure Unload;
       procedure Update(const aInFlightFrameIndex:TpvSizeInt);
       procedure TransferViewsToPreviousViews;
       procedure ClearViews;
       function AddView(const aView:TpvScene3D.TView):TpvSizeInt;
       function AddViews(const aViews:array of TpvScene3D.TView):TpvSizeInt;
       procedure UpdateViews(const aInFlightFrameIndex:TpvSizeInt);
       procedure PrepareLights(const aInFlightFrameIndex:TpvSizeInt;
                               const aViewBaseIndex:TpvSizeInt;
                               const aCountViews:TpvSizeInt;
                               const aViewPortWidth:TpvInt32;
                               const aViewPortHeight:TpvInt32;
                               const aFrustums:TpvFrustumDynamicArray);
       procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;
                         const aRenderPassIndex:TpvSizeInt;
                         const aViewBaseIndex:TpvSizeInt;
                         const aCountViews:TpvSizeInt;
                         const aViewPortWidth:TpvInt32;
                         const aViewPortHeight:TpvInt32;
                         const aLights:boolean=true;
                         const aFrustumCulling:boolean=true);
       procedure UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                      const aInFlightFrameIndex:TpvSizeInt;
                                      const aCommandBuffer:TpvVulkanCommandBuffer;
                                      const aPipelineLayout:TpvVulkanPipelineLayout);
       procedure Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                      const aPreviousInFlightFrameIndex:TpvSizeInt;
                      const aInFlightFrameIndex:TpvSizeInt;
                      const aRenderPassIndex:TpvSizeInt;
                      const aViewBaseIndex:TpvSizeInt;
                      const aCountViews:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aPipelineLayout:TpvVulkanPipelineLayout;
                      const aOnSetRenderPassResources:TOnSetRenderPassResources;
                      const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
       procedure GetZNearZFar(const aViewMatrix:TpvMatrix4x4;
                              const aAspectRatio:TpvScalar;
                              out aZNear:TpvScalar;
                              out aZFar:TpvScalar);
       procedure InitializeGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline;const aWithPreviousPosition:boolean=false);
      public
       property BoundingBox:TpvAABB read fBoundingBox;
       property GlobalVulkanViewUniformBuffers:TGlobalVulkanViewUniformBuffers read fGlobalVulkanViewUniformBuffers;
      published
       property MeshComputeVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fMeshComputeVulkanDescriptorSetLayout;
       property GlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fGlobalVulkanDescriptorSetLayout;
       property HasTransmission:boolean read fHasTransmission;
       property UseBufferDeviceAddress:boolean read fUseBufferDeviceAddress write fUseBufferDeviceAddress;
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

{ TpvScene3D.TScalarSum }

procedure TpvScene3D.TScalarSum.Clear;
begin
 x:=0.0;
 FactorSum:=0.0;
end;

procedure TpvScene3D.TScalarSum.Add(const aX,aFactor:TpvDouble);
begin
 x:=x+(aX*aFactor);
 FactorSum:=FactorSum+aFactor;
end;

function TpvScene3D.TScalarSum.Get(const aDefaultX:TpvDouble):TpvDouble;
begin
 if IsZero(FactorSum) then begin
  result:=aDefaultX;
 end else begin
  result:=x/FactorSum;
 end;
end;

{ TpvScene3D.TVector2Sum }

procedure TpvScene3D.TVector2Sum.Clear;
begin
 x:=0.0;
 y:=0.0;
 FactorSum:=0.0;
end;

procedure TpvScene3D.TVector2Sum.Add(const aX,aY,aFactor:TpvDouble);
begin
 x:=x+(aX*aFactor);
 y:=y+(aY*aFactor);
 FactorSum:=FactorSum+aFactor;
end;

procedure TpvScene3D.TVector2Sum.Add(const aVector:TpvVector2;const aFactor:TpvDouble);
begin
 x:=x+(aVector.x*aFactor);
 y:=y+(aVector.y*aFactor);
 FactorSum:=FactorSum+aFactor;
end;

function TpvScene3D.TVector2Sum.Get(const aDefaultX:TpvDouble;const aDefaultY:TpvDouble):TpvVector2;
var Factor:TpvDouble;
begin
 if IsZero(FactorSum) then begin
  result.x:=aDefaultX;
  result.y:=aDefaultY;
 end else begin
  Factor:=1.0/FactorSum;
  result.x:=x*Factor;
  result.y:=y*Factor;
 end;
end;

function TpvScene3D.TVector2Sum.Get(const aDefault:TpvVector2):TpvVector2;
var Factor:TpvDouble;
begin
 if IsZero(FactorSum) then begin
  result:=aDefault;
 end else begin
  Factor:=1.0/FactorSum;
  result.x:=x*Factor;
  result.y:=y*Factor;
 end;
end;

{ TpvScene3D.TVector3Sum }

procedure TpvScene3D.TVector3Sum.Clear;
begin
 x:=0.0;
 y:=0.0;
 z:=0.0;
 FactorSum:=0.0;
end;

procedure TpvScene3D.TVector3Sum.Add(const aX,aY,aZ,aFactor:TpvDouble);
begin
 x:=x+(aX*aFactor);
 y:=y+(aY*aFactor);
 z:=z+(aZ*aFactor);
 FactorSum:=FactorSum+aFactor;
end;

procedure TpvScene3D.TVector3Sum.Add(const aVector:TpvVector3;const aFactor:TpvDouble);
begin
 x:=x+(aVector.x*aFactor);
 y:=y+(aVector.y*aFactor);
 z:=z+(aVector.z*aFactor);
 FactorSum:=FactorSum+aFactor;
end;

function TpvScene3D.TVector3Sum.Get(const aDefaultX:TpvDouble;const aDefaultY:TpvDouble;const aDefaultZ:TpvDouble):TpvVector3;
var Factor:TpvDouble;
begin
 if IsZero(FactorSum) then begin
  result.x:=aDefaultX;
  result.y:=aDefaultY;
  result.z:=aDefaultZ;
 end else begin
  Factor:=1.0/FactorSum;
  result.x:=x*Factor;
  result.y:=y*Factor;
  result.z:=z*Factor;
 end;
end;

function TpvScene3D.TVector3Sum.Get(const aDefault:TpvVector3):TpvVector3;
var Factor:TpvDouble;
begin
 if IsZero(FactorSum) then begin
  result:=aDefault;
 end else begin
  Factor:=1.0/FactorSum;
  result.x:=x*Factor;
  result.y:=y*Factor;
  result.z:=z*Factor;
 end;
end;

{ TpvScene3D.TVector4Sum }

procedure TpvScene3D.TVector4Sum.Clear;
begin
 x:=0.0;
 y:=0.0;
 z:=0.0;
 w:=0.0;
 FactorSum:=0.0;
end;

procedure TpvScene3D.TVector4Sum.Add(const aX,aY,aZ,aW,aFactor:TpvDouble);
begin
 x:=x+(aX*aFactor);
 y:=y+(aY*aFactor);
 z:=z+(aZ*aFactor);
 w:=w+(aW*aFactor);
 FactorSum:=FactorSum+aFactor;
end;

procedure TpvScene3D.TVector4Sum.Add(const aVector:TpvVector4;const aFactor:TpvDouble);
begin
 x:=x+(aVector.x*aFactor);
 y:=y+(aVector.y*aFactor);
 z:=z+(aVector.z*aFactor);
 w:=w+(aVector.w*aFactor);
 FactorSum:=FactorSum+aFactor;
end;

function TpvScene3D.TVector4Sum.Get(const aDefaultX:TpvDouble;const aDefaultY:TpvDouble;const aDefaultZ:TpvDouble;const aDefaultW:TpvDouble):TpvVector4;
var Factor:TpvDouble;
begin
 if IsZero(FactorSum) then begin
  result.x:=aDefaultX;
  result.y:=aDefaultY;
  result.z:=aDefaultZ;
  result.w:=aDefaultW;
 end else begin
  Factor:=1.0/FactorSum;
  result.x:=x*Factor;
  result.y:=y*Factor;
  result.z:=z*Factor;
  result.w:=w*Factor;
 end;
end;

function TpvScene3D.TVector4Sum.Get(const aDefault:TpvVector4):TpvVector4;
var Factor:TpvDouble;
begin
 if IsZero(FactorSum) then begin
  result:=aDefault;
 end else begin
  Factor:=1.0/FactorSum;
  result.x:=x*Factor;
  result.y:=y*Factor;
  result.z:=z*Factor;
  result.w:=w*Factor;
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

 ReleaseFrameDelay:=MaxInFlightFrames+1;

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
   fID:=fSceneInstance.fImageIDManager.AllocateID;
   fSceneInstance.fImageIDHashMap.Add(fID,self);
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
    if fID>0 then begin
     if fSceneInstance.fImageIDHashMap[fID]=self then begin
      fSceneInstance.fImageIDHashMap.Delete(fID);
     end;
     fSceneInstance.fImageIDManager.FreeID(fID);
     fID:=0;
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
const WhiteTexturePixels:array[0..63] of TpvUInt32=(TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                                    TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                                    TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                                    TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                                    TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                                    TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                                    TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),
                                                    TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff),TpvUInt32($ffffffff));
     DefaultNormalMapTexturePixels:array[0..63] of TpvUInt32=(TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),TpvUInt32($80808080),
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
         case fKind of
          TpvScene3D.TImage.TKind.WhiteTexture:begin
           fTexture:=TpvVulkanTexture.CreateFromMemory(pvApplication.VulkanDevice,
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
                                                       @WhiteTexturePixels,
                                                       SizeOf(TpvUInt32)*64,
                                                       false,
                                                       false,
                                                       0,
                                                       true,
                                                       true);
          end;
          TpvScene3D.TImage.TKind.DefaultNormalMapTexture:begin
           fTexture:=TpvVulkanTexture.CreateFromMemory(pvApplication.VulkanDevice,
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
                                                       @DefaultNormalMapTexturePixels,
                                                       SizeOf(TpvUInt32)*64,
                                                       false,
                                                       false,
                                                       0,
                                                       true,
                                                       false);
          end;
          else begin
           fTexture:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                      GraphicsQueue,
                                                      GraphicsCommandBuffer,
                                                      GraphicsFence,
                                                      GraphicsQueue,
                                                      GraphicsCommandBuffer,
                                                      GraphicsFence,
                                                      fResourceDataStream,
                                                      true,
                                                      false,
                                                      true);
          end;
         end;
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

procedure TpvScene3D.TImage.AssignFromWhiteTexture;
begin
 fName:=#0+'WhiteTexture';
 fKind:=TpvScene3D.TImage.TKind.WhiteTexture;
 fResourceDataStream.Clear;
end;

procedure TpvScene3D.TImage.AssignFromDefaultNormalMapTexture;
begin
 fName:=#0+'DefaultNormalMapTexture';
 fKind:=TpvScene3D.TImage.TKind.DefaultNormalMapTexture;
 fResourceDataStream.Clear;
end;

procedure TpvScene3D.TImage.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceImage:TPasGLTF.TImage);
begin
 fName:=aSourceImage.Name;
 fKind:=TpvScene3D.TImage.TKind.ResourceTexture;
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
   fID:=fSceneInstance.fSamplerIDManager.AllocateID;
   fSceneInstance.fSamplerIDHashMap.Add(fID,self);
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
    if fID>0 then begin
     if fSceneInstance.fSamplerIDHashMap[fID]=self then begin
      fSceneInstance.fSamplerIDHashMap.Delete(fID);
     end;
     fSceneInstance.fSamplerIDManager.FreeID(fID);
     fID:=0;
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

procedure TpvScene3D.TSampler.AssignFromDefault;
begin
 fName:='';
 fMinFilter:=VK_FILTER_LINEAR;
 fMagFilter:=VK_FILTER_LINEAR;
 fMipmapMode:=VK_SAMPLER_MIPMAP_MODE_NEAREST;
 fAddressModeS:=VK_SAMPLER_ADDRESS_MODE_REPEAT;
 fAddressModeT:=VK_SAMPLER_ADDRESS_MODE_REPEAT;
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
   fID:=fSceneInstance.fTextureIDManager.AllocateID;
   fSceneInstance.fTextureIDHashMap.Add(fID,self);
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
    if fID>0 then begin
     if fSceneInstance.fTextureIDHashMap[fID]=self then begin
      fSceneInstance.fTextureIDHashMap.Delete(fID);
     end;
     fSceneInstance.fTextureIDManager.FreeID(fID);
     fID:=0;
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
  if not fUploaded then begin
   fUploaded:=true;
   if assigned(fImage) then begin
    fImage.Upload;
   end;
   if assigned(fSampler) then begin
    fSampler.Upload;
   end;
  end;
 end;
end;

procedure TpvScene3D.TTexture.Unload;
begin
 fUploaded:=false;
end;

function TpvScene3D.TTexture.GetDescriptorImageInfo(const aSRGB:boolean):TVkDescriptorImageInfo;
begin
 if assigned(fSampler) and assigned(fSampler.fSampler) then begin
  result.Sampler:=fSampler.fSampler.Handle;
 end else begin
  result.Sampler:=VK_NULL_HANDLE;
 end;
 if ASRGB and assigned(fImage) and assigned(fImage.fTexture.SRGBImageView) then begin
  result.ImageView:=fImage.fTexture.SRGBImageView.Handle;
 end else if assigned(fImage) and assigned(fImage.fTexture.ImageView) then begin
  result.ImageView:=fImage.fTexture.ImageView.Handle;
 end else begin
  result.ImageView:=VK_NULL_HANDLE;
 end;
 result.ImageLayout:=fImage.fTexture.ImageLayout;
end;

function TpvScene3D.TTexture.GetHashData:THashData;
begin
 if assigned(fImage) then begin
  result.Image:=fImage;
 end else begin
  result.Image:=nil;
 end;
 if assigned(fSampler) then begin
  result.Sampler:=fSampler;
 end else begin
  result.Sampler:=nil;
 end;
end;

procedure TpvScene3D.TTexture.AssignFromWhiteTexture;
begin
 fName:=#0+'WhiteTexture';
 fImage:=fSceneInstance.fWhiteImage;
 fSampler:=fSceneInstance.fDefaultSampler;
end;

procedure TpvScene3D.TTexture.AssignFromDefaultNormalMapTexture;
begin

 fName:=#0+'DefaultNormalMapTexture';

 fImage:=fSceneInstance.fDefaultNormalMapImage;
 fImage.IncRef;

 fSampler:=fSceneInstance.fDefaultSampler;
 fSampler.IncRef;

end;

procedure TpvScene3D.TTexture.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceTexture:TPasGLTF.TTexture;const aImageMap:TImages;const aSamplerMap:TSamplers);
begin

 fName:=aSourceTexture.Name;

 if (aSourceTexture.Source>=0) and (aSourceTexture.Source<aImageMap.Count) then begin
  fImage:=aImageMap[aSourceTexture.Source];
 end else begin
  fImage:=nil;
//raise EPasGLTFInvalidDocument.Create('Image index out of range');
 end;
 if assigned(fImage) then begin
  fImage.IncRef;
 end;

 if (aSourceTexture.Sampler>=0) and (aSourceTexture.Sampler<aSamplerMap.Count) then begin
  fSampler:=aSamplerMap[aSourceTexture.Sampler];
 end else begin
  fSampler:=SceneInstance.fDefaultSampler;
//raise EPasGLTFInvalidDocument.Create('Sampler index out of range');
 end;
 if assigned(fSampler) then begin
  fSampler.IncRef;
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

function TpvScene3D.TMaterial.TTextureReference.TTransform.ToAlignedMatrix3x2:TAlignedMatrix3x2;
var Temporary:TpvMatrix4x4;
begin
 Temporary:=ToMatrix4x4;
 result[0]:=PpvVector2(pointer(@Temporary.RawComponents[0,0]))^;
 result[1]:=PpvVector2(pointer(@Temporary.RawComponents[1,0]))^;
 result[2]:=PpvVector2(pointer(@Temporary.RawComponents[3,0]))^;
end;

{ TpvScene3D.TMaterial }

constructor TpvScene3D.TMaterial.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fData:=DefaultData;

 fLock:=TPasMPSpinLock.Create;

 fVisible:=true;

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
 if assigned(fData.Iridescence.Texture.Texture) then begin
  try
   fData.Iridescence.Texture.Texture.DecRef;
  finally
   fData.Iridescence.Texture.Texture:=nil;
  end;
 end;
 if assigned(fData.Iridescence.ThicknessTexture.Texture) then begin
  try
   fData.Iridescence.ThicknessTexture.Texture.DecRef;
  finally
   fData.Iridescence.ThicknessTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.Transmission.Texture.Texture) then begin
  try
   fData.Transmission.Texture.Texture.DecRef;
  finally
   fData.Transmission.Texture.Texture:=nil;
  end;
 end;
 if assigned(fData.Volume.ThicknessTexture.Texture) then begin
  try
   fData.Volume.ThicknessTexture.Texture.DecRef;
  finally
   fData.Volume.ThicknessTexture.Texture:=nil;
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
   fID:=fSceneInstance.fMaterialIDManager.AllocateID;
   fSceneInstance.fMaterialIDHashMap.Add(fID,self);
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
   if assigned(fData.Iridescence.Texture.Texture) then begin
    try
     fData.Iridescence.Texture.Texture.DecRef;
    finally
     fData.Iridescence.Texture.Texture:=nil;
    end;
   end;
   if assigned(fData.Iridescence.ThicknessTexture.Texture) then begin
    try
     fData.Iridescence.ThicknessTexture.Texture.DecRef;
    finally
     fData.Iridescence.ThicknessTexture.Texture:=nil;
    end;
   end;
   if assigned(fData.Transmission.Texture.Texture) then begin
    try
     fData.Transmission.Texture.Texture.DecRef;
    finally
     fData.Transmission.Texture.Texture:=nil;
    end;
   end;
   if assigned(fData.Volume.ThicknessTexture.Texture) then begin
    try
     fData.Volume.ThicknessTexture.Texture.DecRef;
    finally
     fData.Volume.ThicknessTexture.Texture:=nil;
    end;
   end;
   fSceneInstance.fMaterialListLock.Acquire;
   try
    fSceneInstance.fMaterials.Remove(self);
    if fSceneInstance.fMaterialHashMap[fData]=self then begin
     fSceneInstance.fMaterialHashMap.Delete(fData);
    end;
    if fID>0 then begin
     if fSceneInstance.fMaterialIDHashMap[fID]=self then begin
      fSceneInstance.fMaterialIDHashMap.Delete(fID);
     end;
     fSceneInstance.fMaterialIDManager.FreeID(fID);
     fID:=0;
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
begin

 if (fReferenceCounter>0) and not fUploaded then begin

  fLock.Acquire;
  try

   if (fReferenceCounter>0) and not fUploaded then begin

    try

     if assigned(fData.NormalTexture.Texture) then begin
      fData.NormalTexture.Texture.Upload;
     end else begin
      fSceneInstance.fDefaultNormalMapTexture.Upload;
     end;

     if assigned(fData.OcclusionTexture.Texture) then begin
      fData.OcclusionTexture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     if assigned(fData.EmissiveTexture.Texture) then begin
      fData.EmissiveTexture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     case fData.ShadingModel of
      TpvScene3D.TMaterial.TShadingModel.PBRMetallicRoughness:begin
       if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
        fData.PBRMetallicRoughness.BaseColorTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;
       if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
        fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;
       if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) then begin
        fData.PBRMetallicRoughness.SpecularTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;
       if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
        fData.PBRMetallicRoughness.SpecularColorTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;
      end;
      TpvScene3D.TMaterial.TShadingModel.PBRSpecularGlossiness:begin
       if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
        fData.PBRSpecularGlossiness.DiffuseTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;
       if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
        fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;
       fSceneInstance.fWhiteTexture.Upload;
      end;
      TpvScene3D.TMaterial.TShadingModel.Unlit:begin
       if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
        fData.PBRMetallicRoughness.BaseColorTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;
       fSceneInstance.fWhiteTexture.Upload;
      end;
      else begin
       fSceneInstance.fWhiteTexture.Upload;
      end;
     end;

     if assigned(fData.PBRSheen.ColorIntensityTexture.Texture) then begin
      fData.PBRSheen.ColorIntensityTexture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
      fData.PBRClearCoat.NormalTexture.Texture.Upload;
     end else begin
      fSceneInstance.fDefaultNormalMapTexture.Upload;
     end;

     if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
      fData.PBRClearCoat.RoughnessTexture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     if assigned(fData.PBRClearCoat.Texture.Texture) then begin
      fData.PBRClearCoat.Texture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     if assigned(fData.Iridescence.Texture.Texture) then begin
      fData.Iridescence.Texture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     if assigned(fData.Iridescence.ThicknessTexture.Texture) then begin
      fData.Iridescence.ThicknessTexture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     if assigned(fData.Transmission.Texture.Texture) then begin
      fData.Transmission.Texture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
     end;

     if assigned(fData.Volume.ThicknessTexture.Texture) then begin
      fData.Volume.ThicknessTexture.Texture.Upload;
     end else begin
      fSceneInstance.fWhiteTexture.Upload;
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

procedure TpvScene3D.TMaterial.Unload;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
{    FreeAndNil(fVulkanDescriptorSet);
     FreeAndNil(fVulkanDescriptorPool);
     FreeAndNil(fShaderDataUniformBlockBuffer);}
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

procedure TpvScene3D.TMaterial.AssignFromEmpty;
var Index:TpvSizeInt;
    JSONItem:TPasJSONItem;
    JSONObject:TPasJSONItemObject;
begin

 fName:='';

 fData:=TpvScene3D.TMaterial.DefaultData;

 FillShaderData;

end;

procedure TpvScene3D.TMaterial.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMaterial:TPasGLTF.TMaterial;const aTextureMap:TTextures);
var Index:TpvSizeInt;
    JSONItem:TPasJSONItem;
    JSONObject:TPasJSONItemObject;
begin

 fName:=aSourceMaterial.Name;

 fVisible:=true;

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
  fData.EmissiveFactor:=TpvVector4.InlineableCreate(aSourceMaterial.EmissiveFactor[0],aSourceMaterial.EmissiveFactor[1],aSourceMaterial.EmissiveFactor[2],1.0);
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
  if IsZero(fData.PBRMetallicRoughness.BaseColorFactor.w) and (fData.AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Blend) then begin
   fVisible:=false;
  end;
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
   if IsZero(fData.PBRSpecularGlossiness.DiffuseFactor.w) and (fData.AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Blend) then begin
    fVisible:=false;
   end;
  end else begin
   fData.ShadingModel:=TMaterial.TShadingModel.PBRMetallicRoughness;
   if IsZero(fData.PBRMetallicRoughness.BaseColorFactor.w) and (fData.AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Blend) then begin
    fVisible:=false;
   end;
  end;
 end;

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_emissive_strength'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fData.EmissiveFactor.w:=TPasJSON.GetNumber(JSONObject.Properties['emissiveStrength'],1.0);
  end;
 end;

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_sheen'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fVisible:=true;
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
   fVisible:=true;
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
   fData.IOR:=TPasJSON.GetNumber(TPasJSONItemObject(JSONItem).Properties['ior'],1.5);
  end;
 end;

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_iridescence'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fData.Iridescence.Active:=true;
   fData.Iridescence.Factor:=TPasJSON.GetNumber(JSONObject.Properties['iridescenceFactor'],0.0);
   JSONItem:=JSONObject.Properties['iridescenceTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.Iridescence.Texture.Texture:=aTextureMap[Index];
     if assigned(fData.Iridescence.Texture.Texture) then begin
      fData.Iridescence.Texture.Texture.IncRef;
     end;
    end else begin
     fData.Iridescence.Texture.Texture:=nil;
    end;
    fData.Iridescence.Texture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.Iridescence.Texture.Transform.AssignFromGLTF(fData.Iridescence.Texture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
   fData.Iridescence.Ior:=TPasJSON.GetNumber(JSONObject.Properties['iridescenceIor'],1.3);
   fData.Iridescence.ThicknessMinimum:=TPasJSON.GetNumber(JSONObject.Properties['iridescenceThicknessMinimum'],100.0);
   fData.Iridescence.ThicknessMaximum:=TPasJSON.GetNumber(JSONObject.Properties['iridescenceThicknessMaximum'],400.0);
   JSONItem:=JSONObject.Properties['iridescenceThicknessTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.Iridescence.ThicknessTexture.Texture:=aTextureMap[Index];
     if assigned(fData.Iridescence.ThicknessTexture.Texture) then begin
      fData.Iridescence.ThicknessTexture.Texture.IncRef;
     end;
    end else begin
     fData.Iridescence.ThicknessTexture.Texture:=nil;
    end;
    fData.Iridescence.ThicknessTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.Iridescence.ThicknessTexture.Transform.AssignFromGLTF(fData.Iridescence.ThicknessTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
  end;
 end;

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_transmission'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fSceneInstance.fHasTransmission:=true;
   fData.Transmission.Active:=true;
   if fData.AlphaMode=TpvScene3D.TMaterial.TAlphaMode.Opaque then begin
    fData.AlphaMode:=TpvScene3D.TMaterial.TAlphaMode.Mask;
    fData.AlphaCutOff:=-1e-4;
   end;
   fData.Transmission.Factor:=TPasJSON.GetNumber(JSONObject.Properties['transmissionFactor'],0.0);
   JSONItem:=JSONObject.Properties['transmissionTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.Transmission.Texture.Texture:=aTextureMap[Index];
     if assigned(fData.Transmission.Texture.Texture) then begin
      fData.Transmission.Texture.Texture.IncRef;
     end;
    end else begin
     fData.Transmission.Texture.Texture:=nil;
    end;
    fData.Transmission.Texture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.Transmission.Texture.Transform.AssignFromGLTF(fData.Transmission.Texture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
  end;
 end;

 begin
  JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_volume'];
  if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
   JSONObject:=TPasJSONItemObject(JSONItem);
   fData.Volume.Active:=true;
   fData.Volume.ThicknessFactor:=TPasJSON.GetNumber(JSONObject.Properties['thicknessFactor'],0.0);
   JSONItem:=JSONObject.Properties['thicknessTexture'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
    if (Index>=0) and (Index<aTextureMap.Count) then begin
     fData.Volume.ThicknessTexture.Texture:=aTextureMap[Index];
     if assigned(fData.Volume.ThicknessTexture.Texture) then begin
      fData.Volume.ThicknessTexture.Texture.IncRef;
     end;
    end else begin
     fData.Volume.ThicknessTexture.Texture:=nil;
    end;
    fData.Volume.ThicknessTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
    fData.Volume.ThicknessTexture.Transform.AssignFromGLTF(fData.Volume.ThicknessTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
   end;
   fData.Volume.AttenuationDistance:=TPasJSON.GetNumber(JSONObject.Properties['attenuationDistance'],Infinity);
   JSONItem:=JSONObject.Properties['attenuationColor'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3) then begin
    fData.Volume.AttenuationColor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],1.0);
    fData.Volume.AttenuationColor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],1.0);
    fData.Volume.AttenuationColor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],1.0);
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
 fShaderData.Textures0:=0;
 fShaderData.Textures1:=0;
 case fData.ShadingModel of
  TMaterial.TShadingModel.PBRMetallicRoughness:begin
   fShaderData.Flags:=fShaderData.Flags or ((0 and $f) shl 0);
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 0);
    fShaderData.Textures[0]:=(fData.PBRMetallicRoughness.BaseColorTexture.Texture.ID and $ffff) or ((fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[0]:=fData.PBRMetallicRoughness.BaseColorTexture.Transform.ToAlignedMatrix3x2;
   end;
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 1);
    fShaderData.Textures[1]:=(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.ID and $ffff) or ((fData.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[1]:=fData.PBRMetallicRoughness.MetallicRoughnessTexture.Transform.ToAlignedMatrix3x2;
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
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 9);
    fShaderData.Textures[9]:=(fData.PBRMetallicRoughness.SpecularTexture.Texture.ID and $ffff) or ((fData.PBRMetallicRoughness.SpecularTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[9]:=fData.PBRMetallicRoughness.SpecularTexture.Transform.ToAlignedMatrix3x2;
   end;
   if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 10);
    fShaderData.Textures[10]:=(fData.PBRMetallicRoughness.SpecularColorTexture.Texture.ID and $ffff) or ((fData.PBRMetallicRoughness.SpecularColorTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[10]:=fData.PBRMetallicRoughness.SpecularColorTexture.Transform.ToAlignedMatrix3x2;
   end;
  end;
  TMaterial.TShadingModel.PBRSpecularGlossiness:begin
   fShaderData.Flags:=fShaderData.Flags or ((1 and $f) shl 0);
   if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 0);
    fShaderData.Textures[0]:=(fData.PBRSpecularGlossiness.DiffuseTexture.Texture.ID and $ffff) or ((fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[0]:=fData.PBRSpecularGlossiness.DiffuseTexture.Transform.ToAlignedMatrix3x2;
   end;
   if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 1);
    fShaderData.Textures[1]:=(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.ID and $ffff) or ((fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[1]:=fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Transform.ToAlignedMatrix3x2;
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
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 1);
    fShaderData.Textures[0]:=(fData.PBRMetallicRoughness.BaseColorTexture.Texture.ID and $ffff) or ((fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[0]:=fData.PBRMetallicRoughness.BaseColorTexture.Transform.ToAlignedMatrix3x2;
   end;
   fShaderData.BaseColorFactor:=TpvVector4.InlineableCreate(fData.PBRMetallicRoughness.BaseColorFactor[0],fData.PBRMetallicRoughness.BaseColorFactor[1],fData.PBRMetallicRoughness.BaseColorFactor[2],fData.PBRMetallicRoughness.BaseColorFactor[3]);
  end;
  else begin
   Assert(false);
  end;
 end;
 if assigned(fData.NormalTexture.Texture) then begin
  fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 2);
  fShaderData.Textures[2]:=(fData.NormalTexture.Texture.ID and $ffff) or ((fData.NormalTexture.TexCoord and $f) shl 16);
  fShaderData.TextureTransforms[2]:=fData.NormalTexture.Transform.ToAlignedMatrix3x2;
 end;
 if assigned(fData.OcclusionTexture.Texture) then begin
  fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 3);
  fShaderData.Textures[3]:=(fData.OcclusionTexture.Texture.ID and $ffff) or ((fData.OcclusionTexture.TexCoord and $f) shl 16);
  fShaderData.TextureTransforms[3]:=fData.OcclusionTexture.Transform.ToAlignedMatrix3x2;
 end;
 if assigned(fData.EmissiveTexture.Texture) then begin
  fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 4);
  fShaderData.Textures[4]:=(fData.EmissiveTexture.Texture.ID and $ffff) or ((fData.EmissiveTexture.TexCoord and $f) shl 16);
  fShaderData.TextureTransforms[4]:=fData.EmissiveTexture.Transform.ToAlignedMatrix3x2;
 end;
 fShaderData.EmissiveFactor[0]:=fData.EmissiveFactor[0];
 fShaderData.EmissiveFactor[1]:=fData.EmissiveFactor[1];
 fShaderData.EmissiveFactor[2]:=fData.EmissiveFactor[2];
 fShaderData.EmissiveFactor[3]:=fData.EmissiveFactor[3];

 if fData.PBRSheen.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 7);
  fShaderData.SheenColorFactorSheenIntensityFactor[0]:=fData.PBRSheen.ColorFactor[0];
  fShaderData.SheenColorFactorSheenIntensityFactor[1]:=fData.PBRSheen.ColorFactor[1];
  fShaderData.SheenColorFactorSheenIntensityFactor[2]:=fData.PBRSheen.ColorFactor[2];
  fShaderData.SheenColorFactorSheenIntensityFactor[3]:=fData.PBRSheen.IntensityFactor;
  if assigned(fData.PBRSheen.ColorIntensityTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 5);
   fShaderData.Textures[5]:=(fData.PBRSheen.ColorIntensityTexture.Texture.ID and $ffff) or ((fData.PBRSheen.ColorIntensityTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[5]:=fData.PBRSheen.ColorIntensityTexture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 if fData.PBRClearCoat.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 8);
  fShaderData.ClearcoatFactorClearcoatRoughnessFactor[0]:=fData.PBRClearCoat.Factor;
  fShaderData.ClearcoatFactorClearcoatRoughnessFactor[1]:=fData.PBRClearCoat.RoughnessFactor;
  if assigned(fData.PBRClearCoat.Texture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 6);
   fShaderData.Textures[6]:=(fData.PBRClearCoat.Texture.Texture.ID and $ffff) or ((fData.PBRClearCoat.Texture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[6]:=fData.PBRClearCoat.Texture.Transform.ToAlignedMatrix3x2;
  end;
  if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 7);
   fShaderData.Textures[7]:=(fData.PBRClearCoat.RoughnessTexture.Texture.ID and $ffff) or ((fData.PBRClearCoat.RoughnessTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[7]:=fData.PBRClearCoat.RoughnessTexture.Transform.ToAlignedMatrix3x2;
  end;
  if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 8);
   fShaderData.Textures[8]:=(fData.PBRClearCoat.NormalTexture.Texture.ID and $ffff) or ((fData.PBRClearCoat.NormalTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[8]:=fData.PBRClearCoat.NormalTexture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 fShaderData.IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum[0]:=fData.IOR;

 if fData.Iridescence.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 10);
  fShaderData.IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum[1]:=fData.Iridescence.Factor;
  fShaderData.IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum[2]:=fData.Iridescence.Ior;
  fShaderData.IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum[3]:=fData.Iridescence.ThicknessMinimum;
  fShaderData.IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance[0]:=fData.Iridescence.ThicknessMaximum;
  if assigned(fData.Iridescence.Texture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 11);
   fShaderData.Textures[11]:=(fData.Iridescence.Texture.Texture.ID and $ffff) or ((fData.Iridescence.Texture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[11]:=fData.Iridescence.Texture.Transform.ToAlignedMatrix3x2;
  end;
  if assigned(fData.Iridescence.Texture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 12);
   fShaderData.Textures[12]:=(fData.Iridescence.Texture.Texture.ID and $ffff) or ((fData.Iridescence.ThicknessTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[12]:=fData.Iridescence.Texture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 if fData.Transmission.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 11);
  fShaderData.IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance[1]:=fData.Transmission.Factor;
  if assigned(fData.Transmission.Texture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 13);
   fShaderData.Textures[13]:=(fData.Transmission.Texture.Texture.ID and $ffff) or ((fData.Transmission.Texture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[13]:=fData.Transmission.Texture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 if fData.Volume.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 12);
  fShaderData.IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance[2]:=fData.Volume.ThicknessFactor;
  fShaderData.IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance[3]:=fData.Volume.AttenuationDistance;
  fShaderData.VolumeAttenuationColor[0]:=fData.Volume.AttenuationColor[0];
  fShaderData.VolumeAttenuationColor[1]:=fData.Volume.AttenuationColor[1];
  fShaderData.VolumeAttenuationColor[2]:=fData.Volume.AttenuationColor[2];
  if assigned(fData.Volume.ThicknessTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 14);
   fShaderData.Textures[14]:=(fData.Volume.ThicknessTexture.Texture.ID and $ffff) or ((fData.Volume.ThicknessTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[14]:=fData.Volume.ThicknessTexture.Transform.ToAlignedMatrix3x2;
  end;
 end;

end;

{ TpvScene3D.TLight }

constructor TpvScene3D.TLight.Create(const aSceneInstance:TpvScene3D);
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 fAABBTreeProxy:=-1;
 fInstanceLight:=nil;
 fDataPointer:=@fData;
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
    Data:TpvScene3D.PLightData;
begin
 Data:=fDataPointer;
 if Data^.fVisible then begin
  Position:=(fMatrix*TpvVector3.Origin).xyz;
  Direction:=(((fMatrix*DownZ).xyz)-Position).Normalize;
  fPosition:=Position;
  fDirection:=Direction;
  case Data^.Type_ of
   TpvScene3D.TLightData.TType.Point,
   TpvScene3D.TLightData.TType.Spot:begin
    if Data^.fRange>1e-7 then begin
     // float distanceByRange = currentDistance / light.positionRange.w;
     // lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange * distanceByRange * distanceByRange), 0.0, 1.0) / (currentDistance * currentDistance);
     Radius:=Data^.fRange;
    end else begin
     // lightAttenuation *= 1.0 / (currentDistance * currentDistance);
     Luminance:=Data^.Color.Dot(LinearRGBLuminance);
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
  case Data^.Type_ of
   TpvScene3D.TLightData.TType.Directional,
   TpvScene3D.TLightData.TType.PrimaryDirectional:begin
    AABB.Min:=TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity);
    AABB.Max:=TpvVector3.InlineableCreate(Infinity,Infinity,Infinity);
   end;
   TpvScene3D.TLightData.TType.Point:begin
    AABB.Min:=Position-TpvVector3.InlineableCreate(Radius,Radius,Radius);
    AABB.Max:=Position+TpvVector3.InlineableCreate(Radius,Radius,Radius);
   end;
   TpvScene3D.TLightData.TType.Spot:begin
    OppositeLength:=Tan(Data^.fOuterConeAngle{*0.5})*Radius;
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
   fAABBTreeProxy:=fSceneInstance.fLightAABBTree.CreateProxy(fBoundingBox,TpvPtrInt(Pointer(self)));
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

procedure TpvScene3D.TLightBuffer.Update(const aInFlightFrameIndex:TpvSizeInt);
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
{  fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fLightItemsVulkanBuffer);
   fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fLightTreeVulkanBuffer);}
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
 fChannels:=nil;
 fDefaultChannels:=nil;
end;

destructor TpvScene3D.TGroup.TAnimation.Destroy;
begin
 fChannels:=nil;
 fDefaultChannels:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TAnimation.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceAnimation:TPasGLTF.TAnimation);
var Index,ChannelIndex,ValueIndex,StringPosition,StartStringPosition:TPasGLTFSizeInt;
    SourceAnimationChannel:TPasGLTF.TAnimation.TChannel;
    SourceAnimationSampler:TPasGLTF.TAnimation.TSampler;
    DestinationAnimationChannel:TAnimation.PChannel;
    OutputVector2Array:TPasGLTF.TVector2DynamicArray;
    OutputVector3Array:TPasGLTF.TVector3DynamicArray;
    OutputVector4Array:TPasGLTF.TVector4DynamicArray;
    OutputScalarArray:TPasGLTFFloatDynamicArray;
    OutputScalar64Array:TPasGLTFDoubleDynamicArray;
    JSONItem:TPasJSONItem;
    TargetPointerString,TargetPointerSubString:TpvUTF8String;
    TargetPointerStrings:array of TpvUTF8String;
begin

 fName:=aSourceAnimation.Name;

 SetLength(fChannels,aSourceAnimation.Channels.Count);

 for ChannelIndex:=0 to aSourceAnimation.Channels.Count-1 do begin

  SourceAnimationChannel:=aSourceAnimation.Channels[ChannelIndex];

  DestinationAnimationChannel:=@fChannels[ChannelIndex];

  DestinationAnimationChannel^.Last:=-1;

  DestinationAnimationChannel^.TargetIndex:=-1;

  DestinationAnimationChannel^.TargetSubIndex:=-1;

  if SourceAnimationChannel.Target.Path='translation' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Translation;
   DestinationAnimationChannel^.TargetIndex:=SourceAnimationChannel.Target.Node;
  end else if SourceAnimationChannel.Target.Path='rotation' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Rotation;
   DestinationAnimationChannel^.TargetIndex:=SourceAnimationChannel.Target.Node;
  end else if SourceAnimationChannel.Target.Path='scale' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Scale;
   DestinationAnimationChannel^.TargetIndex:=SourceAnimationChannel.Target.Node;
  end else if SourceAnimationChannel.Target.Path='weights' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Weights;
   DestinationAnimationChannel^.TargetIndex:=SourceAnimationChannel.Target.Node;
  end else if SourceAnimationChannel.Target.Path='pointer' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Pointer_;
   if assigned(SourceAnimationChannel.Target.Extensions) then begin
    JSONItem:=SourceAnimationChannel.Target.Extensions.Properties['KHR_animation_pointer'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
     DestinationAnimationChannel^.TargetPointer:=TPasJSON.GetString(TPasJSONItemObject(JSONItem).Properties['pointer'],'');
     TargetPointerString:=DestinationAnimationChannel^.TargetPointer;
     TargetPointerStrings:=nil;
     try
      StringPosition:=1;
      while StringPosition<=length(TargetPointerString) do begin
       while (StringPosition<=length(TargetPointerString)) and (TargetPointerString[StringPosition]='/') do begin
        inc(StringPosition);
       end;
       StartStringPosition:=StringPosition;
       while (StringPosition<=length(TargetPointerString)) and (TargetPointerString[StringPosition]<>'/') do begin
        inc(StringPosition);
       end;
       if StartStringPosition<StringPosition then begin
        TargetPointerSubString:=copy(TargetPointerString,StartStringPosition,StringPosition-StartStringPosition);
        TargetPointerStrings:=TargetPointerStrings+[TargetPointerSubString];
       end;
      end;
      if length(TargetPointerStrings)>0 then begin
       if TargetPointerStrings[0]='nodes' then begin
        if length(TargetPointerStrings)>2 then begin
         DestinationAnimationChannel^.TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
         if TargetPointerStrings[2]='rotation' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerNodeRotation;
         end else if TargetPointerStrings[2]='scale' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerNodeScale;
         end else if TargetPointerStrings[2]='translation' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerNodeTranslation;
         end else if TargetPointerStrings[2]='weights' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerNodeWeights;
         end;
        end;
       end else if TargetPointerStrings[0]='meshes' then begin
        if length(TargetPointerStrings)>2 then begin
         DestinationAnimationChannel^.TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
         if TargetPointerStrings[2]='weights' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMeshWeights;
         end;
        end;
       end else if TargetPointerStrings[0]='cameras' then begin
        if length(TargetPointerStrings)>3 then begin
         DestinationAnimationChannel^.TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
         if TargetPointerStrings[2]='orthographic' then begin
          if TargetPointerStrings[3]='xmag' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag;
          end else if TargetPointerStrings[3]='ymag' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag;
          end else if TargetPointerStrings[3]='zfar' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar;
          end else if TargetPointerStrings[3]='znear' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear;
          end;
         end else if TargetPointerStrings[2]='perspective' then begin
          if TargetPointerStrings[3]='aspectRatio' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio;
          end else if TargetPointerStrings[3]='yfov' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov;
          end else if TargetPointerStrings[3]='zfar' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar;
          end else if TargetPointerStrings[3]='znear' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear;
          end;
         end;
        end;
       end else if TargetPointerStrings[0]='materials' then begin
        if length(TargetPointerStrings)>2 then begin
         DestinationAnimationChannel.TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
         if TargetPointerStrings[2]='pbrMetallicRoughness' then begin
          if length(TargetPointerStrings)>3 then begin
           if TargetPointerStrings[3]='baseColorFactor' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor;
           end else if TargetPointerStrings[3]='metallicFactor' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor;
           end else if TargetPointerStrings[3]='roughnessFactor' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor;
           end else if TargetPointerStrings[3]='znear' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear;
           end;
          end;
         end else if TargetPointerStrings[2]='alphaCutoff' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff;
         end else if TargetPointerStrings[2]='emissiveFactor' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength;
         end else if TargetPointerStrings[2]='normalTexture' then begin
          if length(TargetPointerStrings)>3 then begin
           if TargetPointerStrings[3]='scale' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale;
           end;
          end;
         end else if TargetPointerStrings[2]='occlusionTexture' then begin
          if length(TargetPointerStrings)>3 then begin
           if TargetPointerStrings[3]='scale' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength;
           end;
          end;
         end;
        end;
       end else if TargetPointerStrings[0]='extensions' then begin
        if (length(TargetPointerStrings)>4) and
           (TargetPointerStrings[1]='KHR_lights_punctual') and
           (TargetPointerStrings[2]='lights') then begin
         DestinationAnimationChannel.TargetIndex:=StrToIntDef(TargetPointerStrings[3],0);
         if TargetPointerStrings[4]='color' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerPunctualLightColor;
         end else if TargetPointerStrings[4]='intensity' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerPunctualLightIntensity;
         end else if TargetPointerStrings[4]='range' then begin
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerPunctualLightRange;
         end else if (TargetPointerStrings[4]='spot') and (length(TargetPointerStrings)>5) then begin
          if TargetPointerStrings[5]='innerConeAngle' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle;
          end else if TargetPointerStrings[5]='outerConeAngle' then begin
           DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle;
          end;
         end;
        end;
       end;
      end;
     finally
      TargetPointerStrings:=nil;
     end;
    end;
   end;
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
    TAnimation.TChannel.TTarget.Scale,
    TAnimation.TChannel.TTarget.PointerNodeTranslation,
    TAnimation.TChannel.TTarget.PointerNodeScale,
    TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor,
    TAnimation.TChannel.TTarget.PointerPunctualLightColor:begin
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
    TAnimation.TChannel.TTarget.Rotation,
    TAnimation.TChannel.TTarget.PointerNodeRotation,
    TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor:begin
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
    TAnimation.TChannel.TTarget.Weights,
    TAnimation.TChannel.TTarget.PointerNodeWeights,
    TAnimation.TChannel.TTarget.PointerMeshWeights,
    TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag,
    TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag,
    TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar,
    TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear,
    TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio,
    TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov,
    TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar,
    TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear,
    TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor,
    TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff,
    TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale,
    TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength,
    TAnimation.TChannel.TTarget.PointerPunctualLightIntensity,
    TAnimation.TChannel.TTarget.PointerPunctualLightRange,
    TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle,
    TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
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
    else {TAnimation.TChannel.TTarget.Pointer_:}begin
     // Ignore
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
    result:=Max(result,Channel^.InputTimeArray[length(Channel^.InputTimeArray)-1]);
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
   fCameraData.Type_:=TpvScene3D.TCameraData.TType.None;
  end;
  TPasGLTF.TCamera.TType.Orthographic:begin
   fCameraData.Type_:=TpvScene3D.TCameraData.TType.Orthographic;
   fCameraData.Orthographic.XMag:=aSourceCamera.Orthographic.XMag;
   fCameraData.Orthographic.YMag:=aSourceCamera.Orthographic.YMag;
   fCameraData.Orthographic.ZNear:=aSourceCamera.Orthographic.ZNear;
   fCameraData.Orthographic.ZFar:=aSourceCamera.Orthographic.ZFar;
  end;
  TPasGLTF.TCamera.TType.Perspective:begin
   fCameraData.Type_:=TpvScene3D.TCameraData.TType.Perspective;
   fCameraData.Perspective.AspectRatio:=aSourceCamera.Perspective.AspectRatio;
   fCameraData.Perspective.YFoV:=aSourceCamera.Perspective.YFoV;
   fCameraData.Perspective.ZNear:=aSourceCamera.Perspective.ZNear;
   fCameraData.Perspective.ZFar:=aSourceCamera.Perspective.ZFar;
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
 fReferencedByNodes.Initialize;
end;

destructor TpvScene3D.TGroup.TMesh.Destroy;
var Index:TpvSizeInt;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
    EmptyMaterial:TpvScene3D.TMaterial;
begin
 EmptyMaterial:=fGroup.fSceneInstance.fEmptyMaterial;
 for Index:=0 to length(fPrimitives)-1 do begin
  Primitive:=@fPrimitives[Index];
  if assigned(Primitive^.Material) then begin
   try
    if Primitive^.Material<>EmptyMaterial then begin
     Primitive^.Material.DecRef;
    end;
   finally
    Primitive^.Material:=nil;
   end;
  end;
 end;
 fPrimitives:=nil;
 fReferencedByNodes.Finalize;
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
    Vertex:PVertex;
    OldVertex:TVertex;
    MorphTargetVertex:PMorphTargetVertex;
    MorphTargetVertexIndex,
    MaterialID:TpvUInt32;
begin

 result:=fNodeMeshInstances;
 inc(fNodeMeshInstances);

 fReferencedByNodes.Add(aNodeIndex);

 if result=0 then begin

  for PrimitiveIndex:=0 to length(fPrimitives)-1 do begin
   Primitive:=@fPrimitives[PrimitiveIndex];
   if assigned(Primitive^.Material) then begin
    MaterialID:=Primitive^.Material.ID;
   end else begin
    MaterialID:=0;
   end;
   NodeMeshPrimitiveInstanceIndex:=Primitive^.NodeMeshPrimitiveInstances.AddNew;
   NodeMeshPrimitiveInstance:=@Primitive^.NodeMeshPrimitiveInstances.Items[NodeMeshPrimitiveInstanceIndex];
   NodeMeshPrimitiveInstance^.MorphTargetBaseIndex:=Primitive^.MorphTargetBaseIndex;
   NodeMeshPrimitiveInstance^.StartBufferVertexOffset:=Primitive^.StartBufferVertexOffset;
   NodeMeshPrimitiveInstance^.StartBufferIndexOffset:=Primitive^.StartBufferIndexOffset;
   for VertexIndex:=TpvSizeInt(Primitive^.StartBufferVertexOffset) to TpvSizeInt(Primitive^.StartBufferVertexOffset+Primitive^.CountVertices)-1 do begin
    Vertex:=@fGroup.fVertices.Items[VertexIndex];
    Vertex^.NodeIndex:=aNodeIndex+1;
    Vertex^.MaterialID:=MaterialID;
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

   if assigned(Primitive^.Material) then begin
    MaterialID:=Primitive^.Material.ID;
   end else begin
    MaterialID:=0;
   end;

   NodeMeshPrimitiveInstanceIndex:=Primitive^.NodeMeshPrimitiveInstances.AddNew;
   NodeMeshPrimitiveInstance:=@Primitive^.NodeMeshPrimitiveInstances.Items[NodeMeshPrimitiveInstanceIndex];

   NodeMeshPrimitiveInstance^.MorphTargetBaseIndex:=fGroup.fMorphTargetCount;
   inc(fGroup.fMorphTargetCount,length(Primitive^.Targets));

   NodeMeshPrimitiveInstance^.StartBufferVertexOffset:=fGroup.fVertices.Count;
   for VertexIndex:=TpvSizeInt(Primitive^.StartBufferVertexOffset) to TpvSizeInt(Primitive^.StartBufferVertexOffset+Primitive^.CountVertices)-1 do begin
    OldVertex:=fGroup.fVertices.Items[VertexIndex];
    NewVertexIndex:=fGroup.fVertices.Add(OldVertex);
    Vertex:=@fGroup.fVertices.Items[NewVertexIndex];
    Vertex^.NodeIndex:=aNodeIndex+1;
    Vertex^.MaterialID:=MaterialID;
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
      fGroup.fJointBlocks.Items[NewJointBlockIndex]:=fGroup.fJointBlocks.Items[OldVertex.JointBlockBaseIndex+JointBlockIndex];
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
       DestinationMeshPrimitive^.Material:=fGroup.fSceneInstance.fEmptyMaterial;
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
 fCombinedPrimitiveUniqueIndexRanges.Initialize;
end;

destructor TpvScene3D.TGroup.TScene.TMaterial.Destroy;
begin
 fPrimitiveIndexRanges.Finalize;
 fCombinedPrimitiveUniqueIndexRanges.Finalize;
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
 fCombinedPrimitiveUniqueIndexRanges.Initialize;
end;

destructor TpvScene3D.TGroup.TScene.Destroy;
begin
 fCombinedPrimitiveUniqueIndexRanges.Finalize;
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

 fPerMaterialCondensedUniqueIndices.Initialize;

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

 fPerMaterialCondensedUniqueIndices.Finalize;

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
 var Index:TpvSizeInt;
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
  if fPerMaterialCondensedUniqueIndices.Count=0 then begin
   fPerMaterialCondensedUniqueIndices.Add(0);
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
  fPerMaterialCondensedUniqueIndices.Finish;
  fMorphTargetVertices.Finish;
  fJointBlocks.Finish;

  fVulkanVertexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                              fVertices.Count*SizeOf(TVertex),
                                              TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
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
                                 TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);

  for Index:=0 to MaxInFlightFrames-1 do begin
   fVulkanCachedVertexBuffers[Index]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                             fVertices.Count*SizeOf(TCachedVertex),
                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                             [],
                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                                            );
   fVulkanCachedVertexBuffers[Index].ClearData(UniversalQueue,
                                               UniversalCommandBuffer,
                                               UniversalFence,
                                               0,
                                               fVertices.Count*SizeOf(TCachedVertex),
                                               TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);
  end;

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

  fVulkanMaterialUniqueIndexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                           fPerMaterialCondensedUniqueIndices.Count*SizeOf(TVkUInt32),
                                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                           [],
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                                          );
  fVulkanMaterialUniqueIndexBuffer.UploadData(UniversalQueue,
                                              UniversalCommandBuffer,
                                              UniversalFence,
                                              fPerMaterialCondensedUniqueIndices.Items[0],
                                              0,
                                              fPerMaterialCondensedUniqueIndices.Count*SizeOf(TVkUInt32),
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
 fLock.Acquire;
 try
  for Instance in fInstances do begin
   Instance.Upload;
  end;
 finally
  fLock.Release;
 end;
end;

procedure TpvScene3D.TGroup.Unload;
var Instance:TpvScene3D.TGroup.TInstance;
    Index:TpvSizeInt;
begin
 fLock.Acquire;
 try
  for Instance in fInstances do begin
   Instance.Unload;
  end;
 finally
  fLock.Release;
 end;
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     FreeAndNil(fVulkanVertexBuffer);
     for Index:=0 to MaxInFlightFrames-1 do begin
      FreeAndNil(fVulkanCachedVertexBuffers[Index]);
     end;
//   FreeAndNil(fVulkanIndexBuffer);
     FreeAndNil(fVulkanMaterialIndexBuffer);
     FreeAndNil(fVulkanMaterialUniqueIndexBuffer);
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
type TIndexBitmap=array of TpvUInt32;
 procedure ProcessNode(const aScene:TpvScene3D.TGroup.TScene;const aNode:TpvScene3D.TGroup.TNode);
 var PrimitiveIndex:TpvSizeInt;
     Mesh:TpvScene3D.TGroup.TMesh;
     Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
     Node:TpvScene3D.TGroup.TNode;
     Material:TpvScene3D.TMaterial;
     SceneMaterial:TpvScene3D.TGroup.TScene.TMaterial;
     NodeMeshPrimitiveInstance:TpvScene3D.TGroup.TMesh.TPrimitive.PNodeMeshPrimitiveInstance;
     PrimitiveIndexRange:TpvScene3D.TGroup.TScene.TPrimitiveIndexRange;
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
    PrimitiveIndexRange:TpvScene3D.TGroup.TScene.PPrimitiveIndexRange;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    Index,FoundIndex,FoundUniqueIndex,PrimitiveIndexRangeIndex,IndexValue,IndexIndex,Count:TpvSizeInt;
    IndexBitmap:TIndexBitmap;
begin

 IndexBitmap:=nil;

 try

  SetLength(IndexBitmap,(fVertices.Count+31) shr 5);
  FillChar(IndexBitmap[0],length(IndexBitmap)*SizeOf(TpvUInt32),0);

  for Scene in fScenes do begin

   for Node in Scene.fNodes do begin
    ProcessNode(Scene,Node);
   end;

   for SceneMaterial in Scene.fMaterials do begin

    SceneMaterial.fStartIndex:=fPerMaterialCondensedIndices.Count;
    SceneMaterial.fCountIndices:=0;

    SceneMaterial.fStartUniqueIndex:=fPerMaterialCondensedUniqueIndices.Count;
    SceneMaterial.fCountUniqueIndices:=0;

    SceneMaterial.fPrimitiveIndexRanges.Finish;

    for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to High(TpvScene3D.TPrimitiveTopology) do begin

     for PrimitiveIndexRangeIndex:=0 to SceneMaterial.fPrimitiveIndexRanges.Count-1 do begin

      PrimitiveIndexRange:=@SceneMaterial.fPrimitiveIndexRanges.Items[PrimitiveIndexRangeIndex];

      if (PrimitiveIndexRange^.Count>0) and (PrimitiveIndexRange^.PrimitiveTopology=PrimitiveTopology) then begin

       if PrimitiveIndexRange^.Count>0 then begin
        FoundUniqueIndex:=SceneMaterial.fCombinedPrimitiveUniqueIndexRanges.AddNew;
        SceneMaterial.fCombinedPrimitiveUniqueIndexRanges.Items[FoundUniqueIndex].PrimitiveTopology:=PrimitiveTopology;
        SceneMaterial.fCombinedPrimitiveUniqueIndexRanges.Items[FoundUniqueIndex].Index:=fPerMaterialCondensedUniqueIndices.Count;
        SceneMaterial.fCombinedPrimitiveUniqueIndexRanges.Items[FoundUniqueIndex].Count:=0;
        SceneMaterial.fCombinedPrimitiveUniqueIndexRanges.Items[FoundUniqueIndex].Node:=PrimitiveIndexRange^.Node;
        Count:=0;
        for IndexIndex:=PrimitiveIndexRange^.Index to (PrimitiveIndexRange^.Index+PrimitiveIndexRange^.Count)-1 do begin
         IndexValue:=fIndices.Items[IndexIndex];
         if (IndexBitmap[IndexValue shr 5] and (TpvUInt32(1) shl (IndexValue and 31)))=0 then begin
          IndexBitmap[IndexValue shr 5]:=IndexBitmap[IndexValue shr 5] or (TpvUInt32(1) shl (IndexValue and 31));
          fPerMaterialCondensedUniqueIndices.Add(fIndices.Items[IndexIndex]);
          inc(Count);
         end;
        end;
        for IndexIndex:=PrimitiveIndexRange^.Index to (PrimitiveIndexRange^.Index+PrimitiveIndexRange^.Count)-1 do begin
         IndexValue:=fIndices.Items[IndexIndex];
         IndexBitmap[IndexValue shr 5]:=IndexBitmap[IndexValue shr 5] and not (TpvUInt32(1) shl (IndexValue and 31));
        end;
        inc(SceneMaterial.fCombinedPrimitiveUniqueIndexRanges.Items[FoundUniqueIndex].Count,Count);
       end;

       begin
        FoundIndex:=-1;
        for Index:=0 to SceneMaterial.fCombinedPrimitiveIndexRanges.Count-1 do begin
         if (SceneMaterial.fCombinedPrimitiveIndexRanges.Items[Index].PrimitiveTopology=PrimitiveTopology) or
            ((not fCulling) or
             (SceneMaterial.fCombinedPrimitiveIndexRanges.Items[Index].Node=PrimitiveIndexRange^.Node)) then begin
          FoundIndex:=Index;
          break;
         end;
        end;
        if FoundIndex<0 then begin
         FoundIndex:=SceneMaterial.fCombinedPrimitiveIndexRanges.AddNew;
         SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].PrimitiveTopology:=PrimitiveTopology;
         SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].Index:=fPerMaterialCondensedIndices.Count;
         SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].Count:=0;
         SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].Node:=PrimitiveIndexRange^.Node;
        end;
        begin
         IndexValue:=fPerMaterialCondensedIndices.Count;
         fPerMaterialCondensedIndices.Add(copy(fIndices.Items,PrimitiveIndexRange^.Index,PrimitiveIndexRange^.Count));
         PrimitiveIndexRange^.Index:=IndexValue;
         inc(SceneMaterial.fCombinedPrimitiveIndexRanges.Items[FoundIndex].Count,PrimitiveIndexRange^.Count);
        end;
       end;

      end;
     end;
    end;

    SceneMaterial.fCountIndices:=fPerMaterialCondensedIndices.Count-SceneMaterial.fStartIndex;

    SceneMaterial.fCountUniqueIndices:=fPerMaterialCondensedUniqueIndices.Count-SceneMaterial.fStartUniqueIndex;

    if SceneMaterial.fCombinedPrimitiveUniqueIndexRanges.Count>0 then begin
     Scene.fCombinedPrimitiveUniqueIndexRanges.Add(SceneMaterial.fCombinedPrimitiveUniqueIndexRanges);
    end;

   end;

   Scene.fCombinedPrimitiveUniqueIndexRanges.Finish;

  end;

  fPerMaterialCondensedIndices.Finish;

  fPerMaterialCondensedUniqueIndices.Finish;

 finally
  IndexBitmap:=nil;
 end;

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
   Image:=TImage.Create(pvApplication.ResourceManager,fSceneInstance);
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
   Sampler:=TSampler.Create(pvApplication.ResourceManager,fSceneInstance);
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
   Texture:=TTexture.Create(pvApplication.ResourceManager,fSceneInstance);
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
   Material:=TpvScene3D.TMaterial.Create(pvApplication.ResourceManager,fSceneInstance);
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
 type TTargetHashMap=TpvHashMap<TpvUInt64,TpvSizeInt>;
      TTargetArrayList=TpvDynamicArrayList<TpvUInt64>;
      TTargetUsedBitmap=array of TpvUInt32;
 var Index,ChannelIndex,TargetIndex,CountDefaultChannels:TpvSizeInt;
     SourceAnimation:TPasGLTF.TAnimation;
     Animation:TpvScene3D.TGroup.TAnimation;
     Channel:TpvScene3D.TGroup.TAnimation.PChannel;
     DefaultChannel:TpvScene3D.TGroup.TAnimation.PDefaultChannel;
     TargetHashMap:TTargetHashMap;
     TargetArrayList:TTargetArrayList;
     TargetUsedBitmap:TTargetUsedBitmap;
     CompactCode:TpvUInt64;
 begin
  TargetHashMap:=TTargetHashMap.Create(-1);
  try
   TargetArrayList:=TTargetArrayList.Create;
   try
    for Index:=0 to aSourceDocument.Animations.Count-1 do begin
     SourceAnimation:=aSourceDocument.Animations[Index];
     Animation:=TAnimation.Create(self,Index);
     try
      Animation.AssignFromGLTF(aSourceDocument,SourceAnimation);
      for ChannelIndex:=0 to length(Animation.fChannels)-1 do begin
       Channel:=@Animation.fChannels[ChannelIndex];
       if Channel^.TargetIndex>=0 then begin
        CompactCode:=(TpvUInt64(TpvUInt64(TpvInt32(Channel^.Target)) and TpvUInt64($ffff)) shl 48) or
                     (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetIndex)+1) and TpvUInt64($ffffffff)) shl 16) or
                     (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetSubIndex)+1) and TpvUInt64($ffff)) shl 0);
        TargetIndex:=TargetHashMap[CompactCode];
        if TargetIndex<0 then begin
         TargetHashMap[CompactCode]:=TargetArrayList.Add(CompactCode);
        end;
       end;
      end;
     finally
      fAnimations.Add(Animation);
     end;
    end;
    if TargetArrayList.Count>0 then begin
     TargetUsedBitmap:=nil;
     try
      SetLength(TargetUsedBitmap,(TargetArrayList.Count+31) shr 5);
      for Index:=0 to fAnimations.Count-1 do begin
       Animation:=fAnimations[Index];
       FillChar(TargetUsedBitmap[0],length(TargetUsedBitmap)*SizeOf(TpvUInt32),#0);
       for ChannelIndex:=0 to length(Animation.fChannels)-1 do begin
        Channel:=@Animation.fChannels[ChannelIndex];
        if Channel^.TargetIndex>=0 then begin
         CompactCode:=(TpvUInt64(TpvUInt64(TpvInt32(Channel^.Target)) and TpvUInt64($ffff)) shl 48) or
                      (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetIndex)+1) and TpvUInt64($ffffffff)) shl 16) or
                      (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetSubIndex)+1) and TpvUInt64($ffff)) shl 0);
         TargetIndex:=TargetHashMap[CompactCode];
         if (TargetIndex>=0) and (TargetIndex<TargetArrayList.Count) then begin
          TargetUsedBitmap[TargetIndex shr 5]:=TargetUsedBitmap[TargetIndex shr 5] or (TpvUInt32(1) shl (TargetIndex and 31));
         end;
        end;
       end;
       CountDefaultChannels:=0;
       for TargetIndex:=0 to TargetArrayList.Count-1 do begin
        if (TargetUsedBitmap[TargetIndex shr 5] and (TpvUInt32(1) shl (TargetIndex and 31)))=0 then begin
         inc(CountDefaultChannels);
        end;
       end;
       if CountDefaultChannels>0 then begin
        SetLength(Animation.fDefaultChannels,CountDefaultChannels);
        CountDefaultChannels:=0;
        for TargetIndex:=0 to TargetArrayList.Count-1 do begin
         if (TargetUsedBitmap[TargetIndex shr 5] and (TpvUInt32(1) shl (TargetIndex and 31)))=0 then begin
          DefaultChannel:=@Animation.fDefaultChannels[CountDefaultChannels];
          inc(CountDefaultChannels);
          CompactCode:=TargetArrayList[TargetIndex];
          DefaultChannel^.Target:=TpvScene3D.TGroup.TAnimation.TChannel.TTarget(TpvInt32(TpvUInt64(TpvUInt64(CompactCode) shr 48)));
          DefaultChannel^.TargetIndex:=TpvSizeInt(TpvUInt64(TpvUInt64(TpvUInt64(CompactCode) shr 16) and TpvUInt64($ffffffff)))-1;
          DefaultChannel^.TargetSubIndex:=TpvSizeInt(TpvUInt64(TpvUInt64(TpvUInt64(CompactCode) shr 0) and TpvUInt64($ffff)))-1;
         end;
        end;
       end;
      end;
     finally
      TargetUsedBitmap:=nil;
     end;
    end;
   finally
    FreeAndNil(TargetArrayList);
   end;
  finally
   FreeAndNil(TargetHashMap);
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
  fNodes.Clear;
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
 var First:boolean;
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
    if First then begin
     First:=false;
     fBoundingBox:=Node.fMesh.fBoundingBox.Transform(Matrix);
    end else begin
     fBoundingBox:=fBoundingBox.Combine(Node.fMesh.fBoundingBox.Transform(Matrix));
    end;
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
  First:=true;
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

function TpvScene3D.TGroup.BeginLoad(const aStream:TStream):boolean;
var GLTF:TPasGLTF.TDocument;
begin
 result:=false;
 if assigned(aStream) then begin
  try
   GLTF:=TPasGLTF.TDocument.Create;
   try
    if (length(FileName)>0) and (FileExists(FileName)) then begin
     GLTF.RootPath:=ExtractFilePath(ExpandFileName(FileName));
    end;
    GLTF.LoadFromStream(aStream);
    AssignFromGLTF(GLTF);
   finally
    FreeAndNil(GLTF);
   end;
   result:=true;
  except
  end;
 end;
end;

function TpvScene3D.TGroup.EndLoad:boolean;
begin
 result:=inherited EndLoad;
 if result then begin
  if SceneInstance.fUploaded then begin
   Upload;
  end;
 end;
end;

procedure TpvScene3D.TGroup.Update(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Update(aInFlightFrameIndex);
 end;
end;

procedure TpvScene3D.TGroup.Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aFrustums:TpvFrustumDynamicArray);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Prepare(aInFlightFrameIndex,
                   aRenderPassIndex,
                   aFrustums);
 end;
end;

procedure TpvScene3D.TGroup.SetGroupResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                              const aPipelineLayout:TpvVulkanPipelineLayout;
                                              const aRenderPassIndex:TpvSizeInt;
                                              const aPreviousInFlightFrameIndex:TpvSizeInt;
                                              const aInFlightFrameIndex:TpvSizeInt);
const Offsets:TVkDeviceSize=0;
begin
 if not fSetGroupResourcesDone[aRenderPassIndex] then begin
  fSetGroupResourcesDone[aRenderPassIndex]:=true;
  aCommandBuffer.CmdBindVertexBuffers(0,1,@fVulkanCachedVertexBuffers[aInFlightFrameIndex].Handle,@Offsets);
  if aPreviousInFlightFrameIndex>=0 then begin
   aCommandBuffer.CmdBindVertexBuffers(1,1,@fVulkanCachedVertexBuffers[aPreviousInFlightFrameIndex].Handle,@Offsets);
  end;
  aCommandBuffer.CmdBindIndexBuffer(fVulkanMaterialIndexBuffer.Handle,0,TVkIndexType.VK_INDEX_TYPE_UINT32);
 end;
end;

procedure TpvScene3D.TGroup.UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                                 const aInFlightFrameIndex:TpvSizeInt;
                                                 const aCommandBuffer:TpvVulkanCommandBuffer;
                                                 const aPipelineLayout:TpvVulkanPipelineLayout);
var Instance:TpvScene3D.TGroup.TInstance;
    BufferMemoryBarrier:TVkBufferMemoryBarrier;
begin
 fCachedVerticesUpdated:=false;
 for Instance in fInstances do begin
  Instance.UpdateCachedVertices(aPipeline,
                                aInFlightFrameIndex,
                                aCommandBuffer,
                                aPipelineLayout);
 end;
 if fCachedVerticesUpdated then begin
  FillChar(BufferMemoryBarrier,SizeOf(TVkBufferMemoryBarrier),#0);
  BufferMemoryBarrier.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
  BufferMemoryBarrier.pNext:=nil;
  BufferMemoryBarrier.buffer:=fVulkanCachedVertexBuffers[aInFlightFrameIndex].Handle;
  BufferMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
  BufferMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT);
  BufferMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  BufferMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  BufferMemoryBarrier.offset:=0;
  BufferMemoryBarrier.size:=VK_WHOLE_SIZE;
  aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT),
                                    0,
                                    0,nil,
                                    1,@BufferMemoryBarrier,
                                    0,nil);
 end;
end;

procedure TpvScene3D.TGroup.Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                                 const aPreviousInFlightFrameIndex:TpvSizeInt;
                                 const aInFlightFrameIndex:TpvSizeInt;
                                 const aRenderPassIndex:TpvSizeInt;
                                 const aCommandBuffer:TpvVulkanCommandBuffer;
                                 var aPipeline:TpvVulkanPipeline;
                                 const aPipelineLayout:TpvVulkanPipelineLayout;
                                 const aOnSetRenderPassResources:TOnSetRenderPassResources;
                                 const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 fSetGroupResourcesDone[aRenderPassIndex]:=false;
 for Instance in fInstances do begin
  Instance.Draw(aGraphicsPipelines,
                aPreviousInFlightFrameIndex,
                aInFlightFrameIndex,
                aRenderPassIndex,
                aCommandBuffer,
                aPipeline,
                aPipelineLayout,
                aOnSetRenderPassResources,
                aMaterialAlphaModes);
 end;
end;

function TpvScene3D.TGroup.CreateInstance:TpvScene3D.TGroup.TInstance;
begin
 result:=TpvScene3D.TGroup.TInstance.Create(ResourceManager,self);
end;

{ TpvScene3D.TGroup.TInstance.TLight }

constructor TpvScene3D.TGroup.TInstance.TLight.Create(const aInstance:TpvScene3D.TGroup.TInstance;const aLight:TpvScene3D.TGroup.TLight);
begin
 inherited Create;
 fInstance:=aInstance;
 fLight:=aLight;
 fData:=fLight.fData;
 fWorkData:=fLight.fData;
 fEffectiveData:=@fData;
 fOverwrites:=nil;
 fCountOverwrites:=0;
end;

destructor TpvScene3D.TGroup.TInstance.TLight.Destroy;
begin
 fOverwrites:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TInstance.TLight.Update;
var Index:TpvSizeInt;
    Factor:TpvDouble;
    Overwrite:TpvScene3D.TGroup.TInstance.TLight.POverwrite;
    ColorSum:TpvScene3D.TVector3Sum;
    IntensitySum:TpvScene3D.TScalarSum;
    RangeSum:TpvScene3D.TScalarSum;
    SpotInnerConeAngleSum:TpvScene3D.TScalarSum;
    SpotOuterConeAngleSum:TpvScene3D.TScalarSum;
begin
 if (fCountOverwrites=0) or not (fData.fVisible or fWorkData.fVisible) then begin
  if fEffectiveData=@fWorkData then begin
   fWorkData:=fData;
  end;
  fEffectiveData:=@fData;
 end else begin
  fEffectiveData:=@fWorkData;
  ColorSum.Clear;
  IntensitySum.Clear;
  RangeSum.Clear;
  SpotInnerConeAngleSum.Clear;
  SpotOuterConeAngleSum.Clear;
  for Index:=0 to fCountOverwrites-1 do begin
   Overwrite:=@fOverwrites[Index];
   Factor:=Overwrite.Factor;
   if not IsZero(Factor) then begin
    if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Defaults in Overwrite^.Flags then begin
     ColorSum.Add(fData.fColor,Factor);
     IntensitySum.Add(fData.fIntensity,Factor);
     RangeSum.Add(fData.fRange,Factor);
     SpotInnerConeAngleSum.Add(fData.fInnerConeAngle,Factor);
     SpotOuterConeAngleSum.Add(fData.fOuterConeAngle,Factor);
    end else begin
     if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Color in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultColor in Overwrite^.Flags then begin
       ColorSum.Add(fData.fColor,Factor);
      end else begin
       ColorSum.Add(Overwrite^.Color,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Intensity in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultIntensity in Overwrite^.Flags then begin
       IntensitySum.Add(fData.fIntensity,Factor);
      end else begin
       IntensitySum.Add(Overwrite^.Intensity,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Range in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultRange in Overwrite^.Flags then begin
       RangeSum.Add(fData.fRange,Factor);
      end else begin
       RangeSum.Add(Overwrite^.Range,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.SpotInnerConeAngle in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultSpotInnerConeAngle in Overwrite^.Flags then begin
       SpotInnerConeAngleSum.Add(fData.fInnerConeAngle,Factor);
      end else begin
       SpotInnerConeAngleSum.Add(Overwrite^.SpotInnerConeAngle,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.SpotOuterConeAngle in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultSpotOuterConeAngle in Overwrite^.Flags then begin
       SpotOuterConeAngleSum.Add(fData.fOuterConeAngle,Factor);
      end else begin
       SpotOuterConeAngleSum.Add(Overwrite^.SpotOuterConeAngle,Factor);
      end;
     end;
    end;
   end;
  end;
  fWorkData.fColor:=ColorSum.Get(fData.fColor);
  fWorkData.fIntensity:=IntensitySum.Get(fData.fIntensity);
  fWorkData.fRange:=RangeSum.Get(fData.fRange);
  fWorkData.fInnerConeAngle:=SpotInnerConeAngleSum.Get(fData.fInnerConeAngle);
  fWorkData.fOuterConeAngle:=SpotOuterConeAngleSum.Get(fData.fOuterConeAngle);
 end;
end;

{ TpvScene3D.TGroup.TInstance.TCamera }

constructor TpvScene3D.TGroup.TInstance.TCamera.Create(const aInstance:TpvScene3D.TGroup.TInstance;const aCamera:TpvScene3D.TGroup.TCamera);
begin
 inherited Create;
 fInstance:=aInstance;
 fCamera:=aCamera;
 fData:=fCamera.fCameraData;
 fWorkData:=fCamera.fCameraData;
 fEffectiveData:=@fData;
 fOverwrites:=nil;
 fCountOverwrites:=0;
end;

destructor TpvScene3D.TGroup.TInstance.TCamera.Destroy;
begin
 fOverwrites:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TInstance.TCamera.Update;
var Index:TpvSizeInt;
    Factor:TpvDouble;
    Overwrite:TpvScene3D.TGroup.TInstance.TCamera.POverwrite;
    OrthographicXMagSum:TpvScene3D.TScalarSum;
    OrthographicYMagSum:TpvScene3D.TScalarSum;
    OrthographicZFarSum:TpvScene3D.TScalarSum;
    OrthographicZNearSum:TpvScene3D.TScalarSum;
    PerspectiveAspectRatioSum:TpvScene3D.TScalarSum;
    PerspectiveYFovSum:TpvScene3D.TScalarSum;
    PerspectiveZFarSum:TpvScene3D.TScalarSum;
    PerspectiveZNearSum:TpvScene3D.TScalarSum;
begin
 if fCountOverwrites=0 then begin
  if fEffectiveData=@fWorkData then begin
   fWorkData:=fData;
  end;
  fEffectiveData:=@fData;
 end else begin
  fEffectiveData:=@fWorkData;
  OrthographicXMagSum.Clear;
  OrthographicYMagSum.Clear;
  OrthographicZFarSum.Clear;
  OrthographicZNearSum.Clear;
  PerspectiveAspectRatioSum.Clear;
  PerspectiveYFovSum.Clear;
  PerspectiveZFarSum.Clear;
  PerspectiveZNearSum.Clear;
  for Index:=0 to fCountOverwrites-1 do begin
   Overwrite:=@fOverwrites[Index];
   Factor:=Overwrite.Factor;
   if not IsZero(Factor) then begin
    if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.Defaults in Overwrite^.Flags then begin
     case fData.Type_ of
      TpvScene3D.TCameraData.TType.Orthographic:begin
       OrthographicXMagSum.Add(fData.Orthographic.XMag,Factor);
       OrthographicYMagSum.Add(fData.Orthographic.YMag,Factor);
       OrthographicZFarSum.Add(fData.Orthographic.ZFar,Factor);
       OrthographicZNearSum.Add(fData.Orthographic.ZNear,Factor);
      end;
      TpvScene3D.TCameraData.TType.Perspective:begin
       PerspectiveAspectRatioSum.Add(fData.Perspective.AspectRatio,Factor);
       PerspectiveYFovSum.Add(fData.Perspective.YFoV,Factor);
       PerspectiveZFarSum.Add(fData.Perspective.ZFar,Factor);
       PerspectiveZNearSum.Add(fData.Perspective.ZNear,Factor);
      end;
      else begin
      end;
     end;
    end else begin
     case fData.Type_ of
      TpvScene3D.TCameraData.TType.Orthographic:begin
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicXMag in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicXMag in Overwrite^.Flags then begin
         OrthographicXMagSum.Add(fData.Orthographic.XMag,Factor);
        end else begin
         OrthographicXMagSum.Add(Overwrite^.OrthographicXMag,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicYMag in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicYMag in Overwrite^.Flags then begin
         OrthographicYMagSum.Add(fData.Orthographic.YMag,Factor);
        end else begin
         OrthographicYMagSum.Add(Overwrite^.OrthographicYMag,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicZFar in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicZFar in Overwrite^.Flags then begin
         OrthographicZFarSum.Add(fData.Orthographic.ZFar,Factor);
        end else begin
         OrthographicZFarSum.Add(Overwrite^.OrthographicZFar,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicZNear in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicZNear in Overwrite^.Flags then begin
         OrthographicZNearSum.Add(fData.Orthographic.ZNear,Factor);
        end else begin
         OrthographicZNearSum.Add(Overwrite^.OrthographicZNear,Factor);
        end;
       end;
      end;
      TpvScene3D.TCameraData.TType.Perspective:begin
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveAspectRatio in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveAspectRatio in Overwrite^.Flags then begin
         PerspectiveAspectRatioSum.Add(fData.Perspective.AspectRatio,Factor);
        end else begin
         PerspectiveAspectRatioSum.Add(Overwrite^.PerspectiveAspectRatio,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveYFov in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveYFov in Overwrite^.Flags then begin
         PerspectiveYFovSum.Add(fData.Perspective.YFoV,Factor);
        end else begin
         PerspectiveYFovSum.Add(Overwrite^.PerspectiveYFov,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveZFar in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveZFar in Overwrite^.Flags then begin
         PerspectiveZFarSum.Add(fData.Perspective.ZFar,Factor);
        end else begin
         PerspectiveZFarSum.Add(Overwrite^.PerspectiveZFar,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveZNear in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveZNear in Overwrite^.Flags then begin
         PerspectiveZNearSum.Add(fData.Perspective.ZNear,Factor);
        end else begin
         PerspectiveZNearSum.Add(Overwrite^.PerspectiveZNear,Factor);
        end;
       end;
      end;
      else begin
      end;
     end;
    end;
   end;
  end;
  case fData.Type_ of
   TpvScene3D.TCameraData.TType.Orthographic:begin
    fWorkData.Orthographic.XMag:=OrthographicXMagSum.Get(fData.Orthographic.XMag);
    fWorkData.Orthographic.YMag:=OrthographicYMagSum.Get(fData.Orthographic.YMag);
    fWorkData.Orthographic.ZFar:=OrthographicZFarSum.Get(fData.Orthographic.ZFar);
    fWorkData.Orthographic.ZNear:=OrthographicZNearSum.Get(fData.Orthographic.ZNear);
   end;
   TpvScene3D.TCameraData.TType.Perspective:begin
    fWorkData.Perspective.AspectRatio:=PerspectiveAspectRatioSum.Get(fData.Perspective.AspectRatio);
    fWorkData.Perspective.YFoV:=PerspectiveYFovSum.Get(fData.Perspective.YFov);
    fWorkData.Perspective.ZFar:=PerspectiveZFarSum.Get(fData.Perspective.ZFar);
    fWorkData.Perspective.ZNear:=PerspectiveZNearSum.Get(fData.Perspective.ZNear);
   end;
   else begin
   end;
  end;
 end;
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

procedure TpvScene3D.TGroup.TInstance.TVulkanData.Update(const aInFlightFrameIndex:TpvSizeInt);
begin
 Upload;
 if fUploaded then begin
  fNodeMatricesBuffer.UpdateData(fInstance.fNodeMatrices[0],0,length(fInstance.fNodeMatrices)*SizeOf(TpvMatrix4x4),FlushUpdateData);
  fMorphTargetVertexWeightsBuffer.UpdateData(fInstance.fMorphTargetVertexWeights[0],0,length(fInstance.fMorphTargetVertexWeights)*SizeOf(TpvFloat),FlushUpdateData);
{ fInstance.fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fNodeMatricesBuffer);
  fInstance.fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fMorphTargetVertexWeightsBuffer);}
 end;
end;

{ TpvScene3D.TGroup.TInstance.TAnimation }

constructor TpvScene3D.TGroup.TInstance.TAnimation.Create;
begin
 inherited Create;
 fChannels:=TChannels.Create;
 fChannels.OwnsObjects:=true;
 fTime:=0.0;
 fShadowTime:=0.0;
 fComplete:=false;
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
    Animation:TpvScene3D.TGroup.TAnimation;
    Light:TpvScene3D.TGroup.TInstance.TLight;
    Camera:TpvScene3D.TGroup.TInstance.TCamera;
begin
 inherited Create(aResourceManager,aParent);
 if aParent is TGroup then begin
  fGroup:=TpvScene3D.TGroup(aParent);
 end else begin
  fGroup:=nil;
 end;
 fLock:=TPasMPSpinLock.Create;
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
 begin
  fLights:=TpvScene3D.TGroup.TInstance.TLights.Create;
  fLights.OwnsObjects:=true;
  for Index:=0 to fGroup.fLights.Count-1 do begin
   Light:=TpvScene3D.TGroup.TInstance.TLight.Create(self,fGroup.fLights[Index]);
   try
    SetLength(Light.fOverwrites,fGroup.fAnimations.Count+1);
   finally
    fLights.Add(Light);
   end;
  end;
 end;
 begin
  fCameras:=TpvScene3D.TGroup.TInstance.TCameras.Create;
  fCameras.OwnsObjects:=true;
  for Index:=0 to fGroup.fCameras.Count-1 do begin
   Camera:=TpvScene3D.TGroup.TInstance.TCamera.Create(self,fGroup.fCameras[Index]);
   try
    SetLength(Camera.fOverwrites,fGroup.fAnimations.Count+1);
   finally
    fCameras.Add(Camera);
   end;
  end;
 end;
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
  SetLength(InstanceNode^.OverwriteWeightsSum,length(Node.fWeights));
  SetLength(InstanceNode^.Overwrites,fGroup.fAnimations.Count+1);
  for OtherIndex:=0 to fGroup.fAnimations.Count do begin
   SetLength(InstanceNode^.Overwrites[OtherIndex].Weights,length(Node.fWeights));
  end;
  for OtherIndex:=0 to MaxInFlightFrames-1 do begin
   InstanceNode^.CacheVerticesGenerations[OtherIndex]:=0;
  end;
  InstanceNode^.CacheVerticesGeneration:=1;
  InstanceNode^.CacheVerticesDirtyCounter:=1;
 end;
 SetLength(fAnimations,fGroup.fAnimations.Count+1);
 for Index:=0 to length(fAnimations)-1 do begin
  fAnimations[Index]:=TpvScene3D.TGroup.TInstance.TAnimation.Create;
  if Index>0 then begin
   Animation:=fGroup.fAnimations[Index-1];
   for OtherIndex:=0 to (length(Animation.fChannels)+length(Animation.fDefaultChannels))-1 do begin
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
 SetLength(fCacheVerticesNodeDirtyBitmap,((length(fNodes)+31) shr 5)+1);
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
 FreeAndNil(fCameras);
 FreeAndNil(fLights);
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
 fCacheVerticesNodeDirtyBitmap:=nil;
 fNodes:=nil;
 fSkins:=nil;
 fAnimations:=nil;
 fNodeMatrices:=nil;
 fMorphTargetVertexWeights:=nil;
 fGroup:=nil;
 FreeAndNil(fLock);
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

  fLock.Acquire;
  try

   if not fUploaded then begin

    try

     SetLength(fNodeMatrices,fGroup.fNodes.Count+fGroup.fCountJointNodeMatrices+1);

     SetLength(fMorphTargetVertexWeights,Max(Max(fGroup.fMorphTargetCount,fGroup.fCountNodeWeights),1));

     for Index:=0 to length(fVulkanDatas)-1 do begin
      fVulkanDatas[Index].Upload;
     end;

     fVulkanComputeDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                                  TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                                  length(fVulkanComputeDescriptorSets));
     fVulkanComputeDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,length(fVulkanComputeDescriptorSets)*7);
     fVulkanComputeDescriptorPool.Initialize;

     for Index:=0 to length(fVulkanComputeDescriptorSets)-1 do begin

      DescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanComputeDescriptorPool,
                                                   fSceneInstance.fMeshComputeVulkanDescriptorSetLayout);
      try
       DescriptorSet.WriteToDescriptorSet(0,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                          [],
                                          [fGroup.fVulkanVertexBuffer.DescriptorBufferInfo],
                                          [],
                                          false);
       DescriptorSet.WriteToDescriptorSet(1,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                          [],
                                          [fGroup.fVulkanCachedVertexBuffers[Index].DescriptorBufferInfo],
                                          [],
                                          false);
       DescriptorSet.WriteToDescriptorSet(2,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                          [],
                                          [fGroup.fVulkanMaterialUniqueIndexBuffer.DescriptorBufferInfo],
                                          [],
                                          false);
       DescriptorSet.WriteToDescriptorSet(3,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                          [],
                                          [fGroup.fVulkanMorphTargetVertexBuffer.DescriptorBufferInfo],
                                          [],
                                          false);
       DescriptorSet.WriteToDescriptorSet(4,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                          [],
                                          [fGroup.fVulkanJointBlockBuffer.DescriptorBufferInfo],
                                          [],
                                          false);
       DescriptorSet.WriteToDescriptorSet(5,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                          [],
                                          [fVulkanDatas[Index].fNodeMatricesBuffer.DescriptorBufferInfo],
                                          [],
                                          false);
       DescriptorSet.WriteToDescriptorSet(6,
                                          0,
                                          1,
                                          TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                          [],
                                          [fVulkanDatas[Index].fMorphTargetVertexWeightsBuffer.DescriptorBufferInfo],
                                          [],
                                          false);
       DescriptorSet.Flush;
      finally
       fVulkanComputeDescriptorSets[Index]:=DescriptorSet;
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

procedure TpvScene3D.TGroup.TInstance.Unload;
var Index:TpvSizeInt;
begin
 if fUploaded then begin
  fLock.Acquire;
  try
   if fUploaded then begin
    try
     for Index:=0 to length(fVulkanComputeDescriptorSets)-1 do begin
      FreeAndNil(fVulkanComputeDescriptorSets[Index]);
     end;
     FreeAndNil(fVulkanComputeDescriptorPool);
     for Index:=0 to length(fVulkanDatas)-1 do begin
      fVulkanDatas[Index].Unload;
     end;
     fNodeMatrices:=nil;
     fMorphTargetVertexWeights:=nil;
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
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

procedure TpvScene3D.TGroup.TInstance.Update(const aInFlightFrameIndex:TpvSizeInt);
var CullFace,Blend:TPasGLTFInt32;
 procedure ResetLights;
 var Index:TPasGLTFSizeInt;
     InstanceLight:TpvScene3D.TGroup.TInstance.TLight;
 begin
  for Index:=0 to fLights.Count-1 do begin
   InstanceLight:=fLights[Index];
   InstanceLight.fCountOverwrites:=0;
  end;
 end;
 procedure ResetCameras;
 var Index:TPasGLTFSizeInt;
     InstanceCamera:TpvScene3D.TGroup.TInstance.TCamera;
 begin
  for Index:=0 to fCameras.Count-1 do begin
   InstanceCamera:=fCameras[Index];
   InstanceCamera.fCountOverwrites:=0;
  end;
 end;
 procedure ResetNode(const aNodeIndex:TPasGLTFSizeInt);
 var Index:TPasGLTFSizeInt;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Node:TpvScene3D.TGroup.TNode;
 begin
  InstanceNode:=@fNodes[aNodeIndex];
  Node:=fGroup.fNodes[aNodeIndex];
  InstanceNode^.CountOverwrites:=0;
  for Index:=0 to Node.Children.Count-1 do begin
   ResetNode(Node.Children[Index].Index);
  end;
 end;
 procedure ProcessBaseOverwrite(const aFactor:TPasGLTFFloat);
 var Index:TPasGLTFSizeInt;
     InstanceLight:TpvScene3D.TGroup.TInstance.TLight;
     InstanceCamera:TpvScene3D.TGroup.TInstance.TCamera;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     LightOverwrite:TpvScene3D.TGroup.TInstance.TLight.POverwrite;
     CameraOverwrite:TpvScene3D.TGroup.TInstance.TCamera.POverwrite;
     NodeOverwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
 begin
  if aFactor>=-0.5 then begin
   for Index:=0 to fLights.Count-1 do begin
    InstanceLight:=fLights[Index];
    if InstanceLight.fCountOverwrites<length(InstanceLight.fOverwrites) then begin
     LightOverwrite:=@InstanceLight.fOverwrites[InstanceLight.fCountOverwrites];
     LightOverwrite^.Flags:=[TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Defaults];
     LightOverwrite^.Factor:=Max(aFactor,0.0);
     inc(InstanceLight.fCountOverwrites);
    end;
   end;
   for Index:=0 to fCameras.Count-1 do begin
    InstanceCamera:=fCameras[Index];
    if InstanceCamera.fCountOverwrites<length(InstanceCamera.fOverwrites) then begin
     CameraOverwrite:=@InstanceCamera.fOverwrites[InstanceCamera.fCountOverwrites];
     CameraOverwrite^.Flags:=[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.Defaults];
     CameraOverwrite^.Factor:=Max(aFactor,0.0);
     inc(InstanceCamera.fCountOverwrites);
    end;
   end;
   for Index:=0 to fGroup.fNodes.Count-1 do begin
    InstanceNode:=@fNodes[Index];
    if InstanceNode^.CountOverwrites<length(InstanceNode^.Overwrites) then begin
     NodeOverwrite:=@InstanceNode^.Overwrites[InstanceNode^.CountOverwrites];
     NodeOverwrite^.Flags:=[TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Defaults];
     NodeOverwrite^.Factor:=Max(aFactor,0.0);
     inc(InstanceNode^.CountOverwrites);
    end;
   end;
  end;
 end;
 procedure ProcessAnimation(const aAnimationIndex:TpvSizeInt;const aAnimationTime:TpvDouble;const aFactor:TpvFloat);
  procedure ProcessScalar(out aScalar:TpvFloat;
                          const aAnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
                          const aTimeIndex0:TpvSizeInt;
                          const aTimeIndex1:TpvSizeInt;
                          const aKeyDelta:TpvDouble;
                          const aFactor:TpvDouble);
  var SqrFactor,CubeFactor:TpvDouble;
  begin
   case aAnimationChannel^.Interpolation of
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
     aScalar:=(aAnimationChannel^.OutputScalarArray[aTimeIndex0]*(1.0-aFactor))+
              (aAnimationChannel^.OutputScalarArray[aTimeIndex1]*aFactor);
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
     aScalar:=aAnimationChannel^.OutputScalarArray[aTimeIndex0];
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
     SqrFactor:=sqr(aFactor);
     CubeFactor:=SqrFactor*aFactor;
     aScalar:=(((aAnimationChannel^.OutputScalarArray[(aTimeIndex0*3)+1]*(((2.0*CubeFactor)-(3.0*SqrFactor))+1.0))+
               (aAnimationChannel^.OutputScalarArray[(aTimeIndex1*3)+0]*(aKeyDelta*((CubeFactor-(2.0*SqrFactor))+aFactor))))+
                (aAnimationChannel^.OutputScalarArray[(aTimeIndex1*3)+1]*((3.0*SqrFactor)-(2.0*CubeFactor))))+
                 (aAnimationChannel^.OutputScalarArray[(aTimeIndex1*3)+0]*(aKeyDelta*(CubeFactor-SqrFactor)));
    end;
    else begin
     Assert(false);
    end;
   end;
  end;
  procedure ProcessVector2(out aVector2:TpvVector2;
                           const aAnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
                           const aTimeIndex0:TpvSizeInt;
                           const aTimeIndex1:TpvSizeInt;
                           const aKeyDelta:TpvDouble;
                           const aFactor:TpvDouble);
  var SqrFactor,CubeFactor:TpvDouble;
  begin
   case aAnimationChannel^.Interpolation of
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
     aVector2:=(aAnimationChannel^.OutputVector2Array[aTimeIndex0]*(1.0-aFactor))+
               (aAnimationChannel^.OutputVector2Array[aTimeIndex1]*aFactor);
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
     aVector2:=aAnimationChannel^.OutputVector2Array[aTimeIndex0];
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
     SqrFactor:=sqr(aFactor);
     CubeFactor:=SqrFactor*aFactor;
     aVector2:=(((aAnimationChannel^.OutputVector2Array[(aTimeIndex0*3)+1]*(((2.0*CubeFactor)-(3.0*SqrFactor))+1.0))+
                (aAnimationChannel^.OutputVector2Array[(aTimeIndex1*3)+0]*(aKeyDelta*((CubeFactor-(2.0*SqrFactor))+aFactor))))+
                 (aAnimationChannel^.OutputVector2Array[(aTimeIndex1*3)+1]*((3.0*SqrFactor)-(2.0*CubeFactor))))+
                  (aAnimationChannel^.OutputVector2Array[(aTimeIndex1*3)+0]*(aKeyDelta*(CubeFactor-SqrFactor)));
    end;
    else begin
     Assert(false);
    end;
   end;
  end;
  procedure ProcessVector3(out aVector3:TpvVector3;
                           const aAnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
                           const aTimeIndex0:TpvSizeInt;
                           const aTimeIndex1:TpvSizeInt;
                           const aKeyDelta:TpvDouble;
                           const aFactor:TpvDouble);
  var SqrFactor,CubeFactor:TpvDouble;
  begin
   case aAnimationChannel^.Interpolation of
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
     aVector3:=(aAnimationChannel^.OutputVector3Array[aTimeIndex0]*(1.0-aFactor))+
               (aAnimationChannel^.OutputVector3Array[aTimeIndex1]*aFactor);
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
     aVector3:=aAnimationChannel^.OutputVector3Array[aTimeIndex0];
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
     SqrFactor:=sqr(aFactor);
     CubeFactor:=SqrFactor*aFactor;
     aVector3:=(((aAnimationChannel^.OutputVector3Array[(aTimeIndex0*3)+1]*(((2.0*CubeFactor)-(3.0*SqrFactor))+1.0))+
                (aAnimationChannel^.OutputVector3Array[(aTimeIndex1*3)+0]*(aKeyDelta*((CubeFactor-(2.0*SqrFactor))+aFactor))))+
                 (aAnimationChannel^.OutputVector3Array[(aTimeIndex1*3)+1]*((3.0*SqrFactor)-(2.0*CubeFactor))))+
                  (aAnimationChannel^.OutputVector3Array[(aTimeIndex1*3)+0]*(aKeyDelta*(CubeFactor-SqrFactor)));
    end;
    else begin
     Assert(false);
    end;
   end;
  end;
  procedure ProcessVector4(out aVector4:TpvVector4;
                           const aAnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
                           const aTimeIndex0:TpvSizeInt;
                           const aTimeIndex1:TpvSizeInt;
                           const aKeyDelta:TpvDouble;
                           const aFactor:TpvDouble;
                           const aRotation:boolean);
  var SqrFactor,CubeFactor:TpvDouble;
  begin
   case aAnimationChannel^.Interpolation of
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
     if aRotation then begin
//    aVector4:=aAnimationChannel^.OutputVector4Array[aTimeIndex0].Slerp(aAnimationChannel^.OutputVector4Array[aTimeIndex1],aFactor);
      aVector4:=TpvQuaternion.Create(aAnimationChannel^.OutputVector4Array[aTimeIndex0]).Slerp(TpvQuaternion.Create(aAnimationChannel^.OutputVector4Array[aTimeIndex1]),aFactor).Vector;
     end else begin
      aVector4:=(aAnimationChannel^.OutputVector4Array[aTimeIndex0]*(1.0-aFactor))+
                (aAnimationChannel^.OutputVector4Array[aTimeIndex1]*aFactor);
     end;
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
     aVector4:=aAnimationChannel^.OutputVector4Array[aTimeIndex0];
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
     SqrFactor:=sqr(aFactor);
     CubeFactor:=SqrFactor*aFactor;
     aVector4:=(((aAnimationChannel^.OutputVector4Array[(aTimeIndex0*3)+1]*(((2.0*CubeFactor)-(3.0*SqrFactor))+1.0))+
                (aAnimationChannel^.OutputVector4Array[(aTimeIndex1*3)+0]*(aKeyDelta*((CubeFactor-(2.0*SqrFactor))+aFactor))))+
                 (aAnimationChannel^.OutputVector4Array[(aTimeIndex1*3)+1]*((3.0*SqrFactor)-(2.0*CubeFactor))))+
                  (aAnimationChannel^.OutputVector4Array[(aTimeIndex1*3)+0]*(aKeyDelta*(CubeFactor-SqrFactor)));
    end;
    else begin
     Assert(false);
    end;
   end;
  end;
  procedure ProcessWeights(const aNode:TpvScene3D.TGroup.TInstance.PNode;
                           const aNodeOverwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
                           const aAnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
                           const aTimeIndex0:TpvSizeInt;
                           const aTimeIndex1:TpvSizeInt;
                           const aKeyDelta:TpvDouble;
                           const aFactor:TpvDouble);
  var CountWeights,WeightIndex:TpvSizeInt;
      InvFactor,SqrFactor,CubeFactor:TpvDouble;
  begin
   CountWeights:=length(aNode^.WorkWeights);
   Include(aNodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Weights);
   case aAnimationChannel^.Interpolation of
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
     InvFactor:=1.0-aFactor;
     for WeightIndex:=0 to CountWeights-1 do begin
      aNodeOverwrite^.Weights[WeightIndex]:=(aAnimationChannel^.OutputScalarArray[(aTimeIndex0*CountWeights)+WeightIndex]*InvFactor)+
                                            (aAnimationChannel^.OutputScalarArray[(aTimeIndex1*CountWeights)+WeightIndex]*aFactor);
     end;
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
     for WeightIndex:=0 to CountWeights-1 do begin
      aNodeOverwrite^.Weights[WeightIndex]:=aAnimationChannel^.OutputScalarArray[(aTimeIndex0*CountWeights)+WeightIndex];
     end;
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
     SqrFactor:=sqr(aFactor);
     CubeFactor:=SqrFactor*aFactor;
     for WeightIndex:=0 to CountWeights-1 do begin
      aNodeOverwrite^.Weights[WeightIndex]:=((((2.0*CubeFactor)-(3.0*SqrFactor))+1.0)*aAnimationChannel^.OutputScalarArray[(((aTimeIndex0*3)+1)*CountWeights)+WeightIndex])+
                                             (((CubeFactor-(2.0*SqrFactor))+aFactor)*aKeyDelta*aAnimationChannel^.OutputScalarArray[(((aTimeIndex0*3)+2)*CountWeights)+WeightIndex])+
                                             (((3.0*SqrFactor)-(2.0*CubeFactor))*aAnimationChannel^.OutputScalarArray[(((aTimeIndex1*3)+1)*CountWeights)+WeightIndex])+
                                             ((CubeFactor-SqrFactor)*aKeyDelta*aAnimationChannel^.OutputScalarArray[(((aTimeIndex1*3)+0)*CountWeights)+WeightIndex]);
     end;
    end;
    else begin
     Assert(false);
    end;
   end;
  end;
 var ChannelIndex,
     InstanceChannelIndex,
     CountInstanceChannels,
     InputTimeArrayIndex,
     ReferenceNodeIndex,
     NodeIndex,
     ElementIndex,
     l,r,m:TpvSizeInt;
     Animation:TpvScene3D.TGroup.TAnimation;
     AnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
     AnimationDefaultChannel:TpvScene3D.TGroup.TAnimation.PDefaultChannel;
     InstanceAnimation:TpvScene3D.TGroup.TInstance.TAnimation;
     InstanceAnimationChannel:TpvScene3D.TGroup.TInstance.TAnimation.TChannel;
     //Node:TpvScene3D.TGroup.TNode;
     Node:TpvScene3D.TGroup.TInstance.PNode;
     Time,Factor,Value,KeyDelta,v0,v1,a,b:TpvDouble;
     Scalar:TpvFloat;
     Vector3:TpvVector3;
     Vector4:TpvVector4;
     TimeIndices:array[0..1] of TpvSizeInt;
     NodeOverwrite:TpvScene3D.TGroup.TInstance.TNode.POverwrite;
     Mesh:TpvScene3D.TGroup.TMesh;
     Light:TpvScene3D.TGroup.TInstance.TLight;
     LightOverwrite:TpvScene3D.TGroup.TInstance.TLight.POverwrite;
     Camera:TpvScene3D.TGroup.TInstance.TCamera;
     CameraOverwrite:TpvScene3D.TGroup.TInstance.TCamera.POverwrite;
 begin

  Animation:=fGroup.fAnimations[aAnimationIndex];

  InstanceAnimation:=fAnimations[aAnimationIndex+1];

  if InstanceAnimation.Complete then begin
   CountInstanceChannels:=InstanceAnimation.fChannels.Count;
  end else begin
   CountInstanceChannels:=length(Animation.fChannels);
  end;
  for InstanceChannelIndex:=0 to CountInstanceChannels-1 do begin
   InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
   InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.None;
   InstanceAnimationChannel.fTarget:=nil;
   InstanceAnimationChannel.fOverwrite:=-1;
  end;

  CountInstanceChannels:=0;

  for ChannelIndex:=0 to length(Animation.fChannels)-1 do begin

   AnimationChannel:=@Animation.fChannels[ChannelIndex];

   if (AnimationChannel.TargetIndex>=0) and (length(AnimationChannel.InputTimeArray)>0) then begin

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

     case AnimationChannel^.Target of

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin

       Node:=@fNodes[AnimationChannel^.TargetIndex];

       NodeOverwrite:=nil;

       if aFactor>=-0.5 then begin

        InstanceAnimationChannel:=nil;
        for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
         if (InstanceAnimation.fChannels[InstanceChannelIndex].fType=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Node) and
            (InstanceAnimation.fChannels[InstanceChannelIndex].fTarget=Node) then begin
          InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
          break;
         end;
        end;
        if assigned(InstanceAnimationChannel) then begin
         if assigned(Node) then begin
          NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
         end else begin
          NodeOverwrite:=nil;
         end;
        end else if assigned(Node) and
                    (Node.CountOverwrites<length(Node.Overwrites)) and
                    (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
         InstanceChannelIndex:=CountInstanceChannels;
         inc(CountInstanceChannels);
         InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
         InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Node;
         InstanceAnimationChannel.fTarget:=Node;
         InstanceAnimationChannel.fOverwrite:=Node.CountOverwrites;
         inc(Node.CountOverwrites);
         NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
         NodeOverwrite^.Flags:=[];
         NodeOverwrite^.Factor:=Max(aFactor,0.0);
        end else begin
         NodeOverwrite:=nil;
        end;

        if assigned(NodeOverwrite) then begin
         case AnimationChannel^.Target of
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale:begin
           ProcessVector3(Vector3,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           case AnimationChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation:begin
             Include(NodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Translation);
             NodeOverwrite^.Translation:=Vector3;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale:begin
             Include(NodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Scale);
             NodeOverwrite^.Scale:=Vector3;
            end;
            else begin
            end;
           end;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation:begin
           ProcessVector4(Vector4,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor,true);
           case AnimationChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation:begin
             Include(NodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation);
             NodeOverwrite^.Rotation.Vector:=Vector4;
            end;
            else begin
            end;
           end;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin
           ProcessWeights(Node,NodeOverwrite,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
          end;
          else begin
          end;
         end;
        end;

       end;

      end;

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMeshWeights:begin

       if (AnimationChannel^.TargetIndex>=0) and (AnimationChannel^.TargetIndex<fGroup.fMeshes.Count) then begin

        Mesh:=fGroup.fMeshes[AnimationChannel^.TargetIndex];

        for ReferenceNodeIndex:=0 to Mesh.fReferencedByNodes.Count-1 do begin

         NodeIndex:=Mesh.fReferencedByNodes.Items[ReferenceNodeIndex];

         if (NodeIndex>=0) and (NodeIndex<fGroup.fNodes.Count) then begin

          Node:=@fNodes[NodeIndex];

          NodeOverwrite:=nil;

          if aFactor>=-0.5 then begin

           InstanceAnimationChannel:=nil;
           for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
            if (InstanceAnimation.fChannels[InstanceChannelIndex].fType=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Node) and
               (InstanceAnimation.fChannels[InstanceChannelIndex].fTarget=Node) then begin
             InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
             break;
            end;
           end;
           if assigned(InstanceAnimationChannel) then begin
            if assigned(Node) then begin
             NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
            end else begin
             NodeOverwrite:=nil;
            end;
           end else if assigned(Node) and
                       (Node.CountOverwrites<length(Node.Overwrites)) and
                       (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
            InstanceChannelIndex:=CountInstanceChannels;
            inc(CountInstanceChannels);
            InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
            InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Node;
            InstanceAnimationChannel.fTarget:=Node;
            InstanceAnimationChannel.fOverwrite:=Node.CountOverwrites;
            inc(Node.CountOverwrites);
            NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
            NodeOverwrite^.Flags:=[];
            NodeOverwrite^.Factor:=Max(aFactor,0.0);
           end else begin
            NodeOverwrite:=nil;
           end;

           if assigned(NodeOverwrite) then begin
            ProcessWeights(Node,NodeOverwrite,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           end;

          end;

         end;

        end;

       end;

      end;

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin

       Light:=fLights[AnimationChannel^.TargetIndex];

       LightOverwrite:=nil;

       if aFactor>=-0.5 then begin
        InstanceAnimationChannel:=nil;
        for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
         if (InstanceAnimation.fChannels[InstanceChannelIndex].fType=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Light) and
            (InstanceAnimation.fChannels[InstanceChannelIndex].fTarget=Light) then begin
          InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
          break;
         end;
        end;
        if assigned(InstanceAnimationChannel) then begin
         if assigned(Light) then begin
          LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannel.fOverwrite];
         end else begin
          LightOverwrite:=nil;
         end;
        end else if assigned(Light) and
                    (Light.fCountOverwrites<length(Light.fOverwrites)) and
                    (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
         InstanceChannelIndex:=CountInstanceChannels;
         inc(CountInstanceChannels);
         InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
         InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Light;
         InstanceAnimationChannel.fTarget:=Light;
         InstanceAnimationChannel.fOverwrite:=Light.fCountOverwrites;
         inc(Light.fCountOverwrites);
         LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannel.fOverwrite];
         LightOverwrite^.Flags:=[];
         LightOverwrite^.Factor:=Max(aFactor,0.0);
        end else begin
         LightOverwrite:=nil;
        end;

        if assigned(LightOverwrite) then begin
         case AnimationChannel^.Target of
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor:begin
           ProcessVector3(Vector3,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Color);
           LightOverwrite^.Color:=Vector3;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Intensity);
           LightOverwrite^.Intensity:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Range);
           LightOverwrite^.Range:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.SpotInnerConeAngle);
           LightOverwrite^.SpotInnerConeAngle:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.SpotOuterConeAngle);
           LightOverwrite^.SpotOuterConeAngle:=Scalar;
          end;
          else begin
          end;
         end;
        end;

       end;

      end;

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear:begin

       Camera:=fCameras[AnimationChannel^.TargetIndex];

       CameraOverwrite:=nil;

       if aFactor>=-0.5 then begin
        InstanceAnimationChannel:=nil;
        for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
         if (InstanceAnimation.fChannels[InstanceChannelIndex].fType=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Camera) and
            (InstanceAnimation.fChannels[InstanceChannelIndex].fTarget=Camera) then begin
          InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
          break;
         end;
        end;
        if assigned(InstanceAnimationChannel) then begin
         if assigned(Camera) then begin
          CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannel.fOverwrite];
         end else begin
          CameraOverwrite:=nil;
         end;
        end else if assigned(Camera) and
                    (Camera.fCountOverwrites<length(Camera.fOverwrites)) and
                    (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
         InstanceChannelIndex:=CountInstanceChannels;
         inc(CountInstanceChannels);
         InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
         InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Camera;
         InstanceAnimationChannel.fTarget:=Camera;
         InstanceAnimationChannel.fOverwrite:=Camera.fCountOverwrites;
         inc(Camera.fCountOverwrites);
         CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannel.fOverwrite];
         CameraOverwrite^.Flags:=[];
         CameraOverwrite^.Factor:=Max(aFactor,0.0);
        end else begin
         CameraOverwrite:=nil;
        end;

        if assigned(CameraOverwrite) then begin
         case AnimationChannel^.Target of
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicXMag);
           CameraOverwrite^.OrthographicXMag:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicYMag);
           CameraOverwrite^.OrthographicYMag:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicZFar);
           CameraOverwrite^.OrthographicZFar:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicZNear);
           CameraOverwrite^.OrthographicZNear:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveAspectRatio);
           CameraOverwrite^.PerspectiveAspectRatio:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveYFov);
           CameraOverwrite^.PerspectiveYFov:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveZFar);
           CameraOverwrite^.PerspectiveZFar:=Scalar;
          end;
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear:begin
           ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
           Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveZNear);
           CameraOverwrite^.PerspectiveZNear:=Scalar;
          end;
          else begin
          end;
         end;
        end;

       end;

      end;

      else begin
      end;

     end;

    end;

   end;

  end;

  if InstanceAnimation.Complete then begin

   for ChannelIndex:=0 to length(Animation.fDefaultChannels)-1 do begin

    AnimationDefaultChannel:=@Animation.fDefaultChannels[ChannelIndex];

    case AnimationDefaultChannel^.Target of

     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin
      Node:=@fNodes[AnimationDefaultChannel^.TargetIndex];
      NodeOverwrite:=nil;
      if aFactor>=-0.5 then begin
       InstanceAnimationChannel:=nil;
       for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
        if (InstanceAnimation.fChannels[InstanceChannelIndex].fType=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Node) and
           (InstanceAnimation.fChannels[InstanceChannelIndex].fTarget=Node) then begin
         InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
         break;
        end;
       end;
       if assigned(InstanceAnimationChannel) then begin
        NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
       end else if (Node.CountOverwrites<length(Node.Overwrites)) and
                   (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
        InstanceChannelIndex:=CountInstanceChannels;
        inc(CountInstanceChannels);
        InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
        InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Node;
        InstanceAnimationChannel.fTarget:=Node;
        InstanceAnimationChannel.fOverwrite:=Node.CountOverwrites;
        inc(Node.CountOverwrites);
        NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannel.fOverwrite];
        NodeOverwrite^.Flags:=[];
        NodeOverwrite^.Factor:=Max(aFactor,0.0);
       end;
       if assigned(NodeOverwrite) then begin
        case AnimationDefaultChannel^.Target of
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation:begin
          NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultTranslation,
                                                      TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Translation];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale:begin
          NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultScale,
                                                      TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Scale];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation:begin
          NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultRotation,
                                                      TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin
          NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultWeights,
                                                      TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Weights];
         end;
         else begin
         end;
        end;
       end;
      end;
     end;

     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
      Light:=fLights[AnimationDefaultChannel^.TargetIndex];
      LightOverwrite:=nil;
      if aFactor>=-0.5 then begin
       InstanceAnimationChannel:=nil;
       for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
        if (InstanceAnimation.fChannels[InstanceChannelIndex].fType=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Light) and
           (InstanceAnimation.fChannels[InstanceChannelIndex].fTarget=Light) then begin
         InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
         break;
        end;
       end;
       if assigned(InstanceAnimationChannel) then begin
        if assigned(Light) then begin
         LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannel.fOverwrite];
        end else begin
         LightOverwrite:=nil;
        end;
       end else if assigned(Light) and
                   (Light.fCountOverwrites<length(Light.fOverwrites)) and
                   (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
        InstanceChannelIndex:=CountInstanceChannels;
        inc(CountInstanceChannels);
        InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
        InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Light;
        InstanceAnimationChannel.fTarget:=Light;
        InstanceAnimationChannel.fOverwrite:=Light.fCountOverwrites;
        inc(Light.fCountOverwrites);
        LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannel.fOverwrite];
        LightOverwrite^.Flags:=[];
        LightOverwrite^.Factor:=Max(aFactor,0.0);
       end else begin
        LightOverwrite:=nil;
       end;
       if assigned(LightOverwrite) then begin
        case AnimationDefaultChannel^.Target of
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor:begin
          LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultColor,
                                                        TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Color];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity:begin
          LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultIntensity,
                                                        TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Intensity];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange:begin
          LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultRange,
                                                        TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.Range];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle:begin
          LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultSpotInnerConeAngle,
                                                        TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.SpotInnerConeAngle];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
          LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.DefaultSpotOuterConeAngle,
                                                        TpvScene3D.TGroup.TInstance.TLight.TOverwriteFlag.SpotOuterConeAngle];
         end;
         else begin
         end;
        end;
       end;
      end;
     end;

     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar,
     TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear:begin
      Camera:=fCameras[AnimationDefaultChannel^.TargetIndex];
      CameraOverwrite:=nil;
      if aFactor>=-0.5 then begin
       InstanceAnimationChannel:=nil;
       for InstanceChannelIndex:=CountInstanceChannels-1 downto 0 do begin
        if (InstanceAnimation.fChannels[InstanceChannelIndex].fType=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Camera) and
           (InstanceAnimation.fChannels[InstanceChannelIndex].fTarget=Camera) then begin
         InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
         break;
        end;
       end;
       if assigned(InstanceAnimationChannel) then begin
        if assigned(Camera) then begin
         CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannel.fOverwrite];
        end else begin
         CameraOverwrite:=nil;
        end;
       end else if assigned(Camera) and
                   (Camera.fCountOverwrites<length(Camera.fOverwrites)) and
                   (CountInstanceChannels<InstanceAnimation.fChannels.Count) then begin
        InstanceChannelIndex:=CountInstanceChannels;
        inc(CountInstanceChannels);
        InstanceAnimationChannel:=InstanceAnimation.fChannels[InstanceChannelIndex];
        InstanceAnimationChannel.fType:=TpvScene3D.TGroup.TInstance.TAnimation.TChannel.TType.Camera;
        InstanceAnimationChannel.fTarget:=Camera;
        InstanceAnimationChannel.fOverwrite:=Camera.fCountOverwrites;
        inc(Camera.fCountOverwrites);
        CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannel.fOverwrite];
        CameraOverwrite^.Flags:=[];
        CameraOverwrite^.Factor:=Max(aFactor,0.0);
       end else begin
        CameraOverwrite:=nil;
       end;
       if assigned(CameraOverwrite) then begin
        case AnimationDefaultChannel^.Target of
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicXMag,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicXMag];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicYMag,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicYMag];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicZFar,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicZFar];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultOrthographicZNear,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.OrthographicZNear];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveAspectRatio,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveAspectRatio];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveYFov,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveYFov];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveZFar,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveZFar];
         end;
         TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear:begin
          CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.DefaultPerspectiveZNear,
                                                          TpvScene3D.TGroup.TInstance.TCamera.TOverwriteFlag.PerspectiveZNear];
         end;
         else begin
         end;
        end;
       end;
      end;
     end;

     else begin
     end;

    end;

   end;

  end;

 end;
 procedure ProcessNode(const aNodeIndex:TpvSizeInt;const aMatrix:TpvMatrix4x4;aDirty:boolean);
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
     FirstWeights,SkinUsed,Dirty:boolean;
     Light:TpvScene3D.TLight;
     InstanceLight:TpvScene3D.TGroup.TInstance.TLight;
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
  Dirty:=aDirty;
  if InstanceNode^.CountOverwrites>0 then begin
   Dirty:=true;
   SkinUsed:=true;
   TranslationSum.Clear;
   ScaleSum.Clear;
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
      TranslationSum.Add(Node.fTranslation,Factor);
      ScaleSum.Add(Node.fScale,Factor);
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
       if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultTranslation in Overwrite^.Flags then begin
        TranslationSum.Add(Node.fTranslation,Factor);
       end else begin
        TranslationSum.Add(Overwrite^.Translation,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Scale in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultScale in Overwrite^.Flags then begin
        ScaleSum.Add(Node.fScale,Factor);
       end else begin
        ScaleSum.Add(Overwrite^.Scale,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Rotation in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultRotation in Overwrite^.Flags then begin
        AddRotation(Node.fRotation,Factor);
       end else begin
        AddRotation(Overwrite^.Rotation,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.Weights in Overwrite^.Flags then begin
       if FirstWeights then begin
        FirstWeights:=false;
        for OtherIndex:=0 to length(InstanceNode^.OverwriteWeightsSum)-1 do begin
         InstanceNode^.OverwriteWeightsSum[OtherIndex]:=0.0;
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TNode.TOverwriteFlag.DefaultWeights in Overwrite^.Flags then begin
        for OtherIndex:=0 to Min(length(InstanceNode^.OverwriteWeightsSum),length(Node.fWeights))-1 do begin
         InstanceNode^.OverwriteWeightsSum[OtherIndex]:=InstanceNode^.OverwriteWeightsSum[OtherIndex]+(Node.fWeights[OtherIndex]*Factor);
        end;
       end else begin
        for OtherIndex:=0 to Min(length(InstanceNode^.OverwriteWeightsSum),length(Overwrite^.Weights))-1 do begin
         InstanceNode^.OverwriteWeightsSum[OtherIndex]:=InstanceNode^.OverwriteWeightsSum[OtherIndex]+(Overwrite^.Weights[OtherIndex]*Factor);
        end;
       end;
       WeightsFactorSum:=WeightsFactorSum+Factor;
      end;
     end;
    end;
   end;
   Translation:=TranslationSum.Get(Node.fTranslation);
   Scale:=ScaleSum.Get(Node.fScale);
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
   Translation:=Node.fTranslation;
   Scale:=Node.fScale;
   Rotation:=Node.fRotation;
   for Index:=0 to Min(length(InstanceNode^.WorkWeights),length(Node.fWeights))-1 do begin
    InstanceNode^.WorkWeights[Index]:=Node.fWeights[Index];
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
  Dirty:=Dirty or (assigned(Node.fSkin) or (length(Node.fWeights)>0));
  if assigned(Node.fLight) then begin
   InstanceLight:=fLights[Node.fLight.fIndex];
   if assigned(InstanceNode^.Light) then begin
    Light:=InstanceNode^.Light;
    if (Light.fMatrix<>Matrix) or (Light.fDataPointer<>InstanceLight.fEffectiveData) then begin
     Light.fMatrix:=Matrix;
     Light.fDataPointer:=InstanceLight.fEffectiveData;
     Light.Update;
    end;
   end else begin
    Light:=TpvScene3D.TLight.Create(fSceneInstance);
    try
     Light.fLight:=Node.fLight;
     Light.fInstanceLight:=InstanceLight;
     Light.fData:=Node.fLight.fData;
     Light.fDataPointer:=InstanceLight.fEffectiveData;
     Light.fMatrix:=Matrix;
     Light.Update;
    finally
     InstanceNode^.Light:=Light;
    end;
   end;
  end;
  if Dirty and (InstanceNode^.CacheVerticesDirtyCounter<2) then begin
   InstanceNode^.CacheVerticesDirtyCounter:=2;
  end;
  for Index:=0 to Node.Children.Count-1 do begin
   ProcessNode(Node.Children[Index].Index,Matrix,Dirty);
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
  AABB:=@InstanceNode^.BoundingBoxes[aInFlightFrameIndex];
  Filled:=@InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex];
  for Index:=0 to Node.Children.Count-1 do begin
   ProcessBoundingBoxNode(Node.Children[Index].Index);
   OtherInstanceNode:=@fNodes[Node.Children[Index].Index];
   if OtherInstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
    OtherAABB:=@OtherInstanceNode^.BoundingBoxes[aInFlightFrameIndex];
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

 fActives[aInFlightFrameIndex]:=fActive;

 if fActive then begin

  Scene:=GetScene;

  if assigned(Scene) then begin

   fScenes[aInFlightFrameIndex]:=Scene;

   //CurrentSkinShaderStorageBufferObjectHandle:=0;

   for Index:=0 to length(fLightNodes)-1 do begin
    fLightNodes[Index]:=-1;
   end;

   ResetLights;

   ResetCameras;

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

   for Index:=0 to fLights.Count-1 do begin
    fLights[Index].Update;
   end;

   for Index:=0 to fCameras.Count-1 do begin
    fCameras[Index].Update;
   end;

   for Index:=0 to Scene.fNodes.Count-1 do begin
    ProcessNode(Scene.fNodes[Index].Index,TpvMatrix4x4.Identity,false);
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
     InstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=Node.fMesh.fBoundingBox.Transform(InstanceNode^.WorkMatrix*fModelMatrix);
     InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=true;
    end else begin
     InstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=TpvAABB.Create(TpvVector3.Origin,TpvVector3.Origin);
     InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=false;
    end;
   end;

   for Index:=0 to Scene.fNodes.Count-1 do begin
    ProcessBoundingBoxNode(Scene.fNodes[Index].Index);
   end;

  end;

  fVulkanData:=fVulkanDatas[aInFlightFrameIndex];
  if assigned(fVulkanData) then begin
   fVulkanData.Update(aInFlightFrameIndex);
  end;

  fBoundingBox:=fGroup.fBoundingBox.Transform(fModelMatrix);
  if assigned(Scene) then begin
   for Index:=0 to Scene.fNodes.Count-1 do begin
    InstanceNode:=@fNodes[Scene.fNodes[Index].fIndex];
    if InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
     fBoundingBox:=fBoundingBox.Combine(InstanceNode^.BoundingBoxes[aInFlightFrameIndex]);
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

procedure TpvScene3D.TGroup.TInstance.Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                              const aRenderPassIndex:TpvSizeInt;
                                              const aFrustums:TpvFrustumDynamicArray);
var VisibleBit:TpvUInt32;
 procedure ProcessNode(const aNodeIndex:TpvSizeInt;const aMask:TpvUInt32);
 var Index,NodeIndex:TpvSizeInt;
     Mask:TpvUInt32;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Node:TpvScene3D.TGroup.TNode;
     OK:boolean;
 begin
  if aNodeIndex>=0 then begin
   InstanceNode:=@fNodes[aNodeIndex];
   Mask:=aMask;
   if InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
    if length(aFrustums)>0 then begin
     if length(aFrustums)=1 then begin
      OK:=not ((((Mask and $80000000)<>0) and (aFrustums[0].AABBInFrustum(InstanceNode^.BoundingBoxes[aInFlightFrameIndex],Mask)=TpvFrustum.COMPLETE_OUT)));
     end else begin
      OK:=false;
      for Index:=0 to length(aFrustums)-1 do begin
       if aFrustums[Index].AABBInFrustum(InstanceNode^.BoundingBoxes[aInFlightFrameIndex])<>TpvFrustum.COMPLETE_OUT then begin
        OK:=true;
        break;
       end;
      end;
     end;
    end else begin
     OK:=true;
    end;
    if OK then begin
     TPasMPInterlocked.BitwiseOr(InstanceNode^.VisibleBitmap,VisibleBit);
     Node:=fGroup.fNodes[aNodeIndex];
     for NodeIndex:=0 to Node.fChildren.Count-1 do begin
      ProcessNode(Node.fChildren[NodeIndex].fIndex,Mask);
     end;
    end;
   end;
  end;
 end;
var NodeIndex:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
begin
 VisibleBit:=TpvUInt32(1) shl aRenderPassIndex;
 if fActives[aInFlightFrameIndex] and ((fVisibleBitmap and (TpvUInt32(1) shl aRenderPassIndex))<>0) then begin
  if length(aFrustums)>0 then begin
   for NodeIndex:=0 to length(fNodes)-1 do begin
    TPasMPInterlocked.BitwiseAnd(fNodes[NodeIndex].VisibleBitmap,not VisibleBit);
   end;
   Scene:=fScenes[aInFlightFrameIndex];
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

procedure TpvScene3D.TGroup.TInstance.UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                                           const aInFlightFrameIndex:TpvSizeInt;
                                                           const aCommandBuffer:TpvVulkanCommandBuffer;
                                                           const aPipelineLayout:TpvVulkanPipelineLayout);
var NodeIndex,PrimitiveIndexRangeIndex,IndicesStart,IndicesCount,
    InFlightFrameIndex:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    PrimitiveIndexRanges:TpvScene3D.TGroup.TScene.PPrimitiveIndexRanges;
    PrimitiveIndexRange:TpvScene3D.TGroup.TScene.PPrimitiveIndexRange;
    Node:TpvScene3D.TGroup.TInstance.PNode;
    FirstFlush:boolean;
 procedure Flush;
 var MeshComputeStagePushConstants:TpvScene3D.TMeshComputeStagePushConstants;
 begin
  if IndicesCount>0 then begin
   fGroup.fCachedVerticesUpdated:=true;
   if FirstFlush then begin
    FirstFlush:=false;
    aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                         aPipelineLayout.Handle,
                                         0,
                                         1,
                                         @fVulkanComputeDescriptorSets[aInFlightFrameIndex].Handle,
                                         0,
                                         nil);
   end;
   MeshComputeStagePushConstants.IndexOffset:=IndicesStart;
   MeshComputeStagePushConstants.CountIndices:=IndicesCount;
   aCommandBuffer.CmdPushConstants(aPipelineLayout.Handle,
                                   TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                   0,
                                   SizeOf(TpvScene3D.TMeshComputeStagePushConstants),
                                   @MeshComputeStagePushConstants);
   aCommandBuffer.CmdDispatch((IndicesCount+127) shr 7,1,1);
   IndicesCount:=0;
  end;
 end;
begin
 if fActives[aInFlightFrameIndex] then begin
  Scene:=fScenes[aInFlightFrameIndex];
  if assigned(Scene) then begin
   FillChar(fCacheVerticesNodeDirtyBitmap[0],Length(fCacheVerticesNodeDirtyBitmap)*SizeOf(TpvUInt32),#0);
   for NodeIndex:=0 to length(fNodes)-1 do begin
    Node:=@fNodes[NodeIndex];
    if Node^.CacheVerticesDirtyCounter>0 then begin
     dec(Node^.CacheVerticesDirtyCounter);
     inc(Node^.CacheVerticesGeneration);
     if Node^.CacheVerticesGeneration=0 then begin
      // Handle generation value overflow
      Node^.CacheVerticesGeneration:=1;
      for InFlightFrameIndex:=0 to MaxInFlightFrames-1 do begin
       Node^.CacheVerticesGenerations[aInFlightFrameIndex]:=0;
      end;
     end;
    end;
    if Node^.CacheVerticesGenerations[aInFlightFrameIndex]<>Node^.CacheVerticesGeneration then begin
     Node^.CacheVerticesGenerations[aInFlightFrameIndex]:=Node^.CacheVerticesGeneration;
     fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5]:=fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5] or (TpvUInt32(1) shl (NodeIndex and 31));
    end;
   end;
   FirstFlush:=true;
   PrimitiveIndexRanges:=@Scene.fCombinedPrimitiveUniqueIndexRanges;
   IndicesStart:=0;
   IndicesCount:=0;
   for PrimitiveIndexRangeIndex:=0 to PrimitiveIndexRanges^.Count-1 do begin
    PrimitiveIndexRange:=@PrimitiveIndexRanges^.Items[PrimitiveIndexRangeIndex];
    if PrimitiveIndexRange^.Count>0 then begin
     NodeIndex:=PrimitiveIndexRange^.Node;
     if (fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5] and (TpvUInt32(1) shl (NodeIndex and 31)))<>0 then begin
      if (IndicesCount=0) or ((IndicesStart+IndicesCount)<>PrimitiveIndexRange^.Index) then begin
       Flush;
       IndicesStart:=PrimitiveIndexRange^.Index;
      end;
      inc(IndicesCount,PrimitiveIndexRange^.Count);
     end;
    end;
   end;
   Flush;
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                                           const aPreviousInFlightFrameIndex:TpvSizeInt;
                                           const aInFlightFrameIndex:TpvSizeInt;
                                           const aRenderPassIndex:TpvSizeInt;
                                           const aCommandBuffer:TpvVulkanCommandBuffer;
                                           var aPipeline:TpvVulkanPipeline;
                                           const aPipelineLayout:TpvVulkanPipelineLayout;
                                           const aOnSetRenderPassResources:TOnSetRenderPassResources;
                                           const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes);
var SceneMaterialIndex,PrimitiveIndexRangeIndex,
    IndicesStart,IndicesCount:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    SceneMaterial:TpvScene3D.TGroup.TScene.TMaterial;
    Material:TpvScene3D.TMaterial;
    PrimitiveIndexRanges:TpvScene3D.TGroup.TScene.PPrimitiveIndexRanges;
    PrimitiveIndexRange:TpvScene3D.TGroup.TScene.PPrimitiveIndexRange;
    LastPrimitiveTopology,PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    LastDoubleSided,DoubleSided,First,Culling,FirstFlush:boolean;
    VisibleBit:TpvUInt32;
 procedure Flush;
 var Pipeline:TpvVulkanPipeline;
     WasFirstFlush:boolean;
 begin

  if IndicesCount>0 then begin

   Pipeline:=aGraphicsPipelines[LastPrimitiveTopology,LastDoubleSided];
   if aPipeline<>Pipeline then begin
    aPipeline:=Pipeline;
    if assigned(Pipeline) then begin
     aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,Pipeline.Handle);
    end;
   end;

   WasFirstFlush:=FirstFlush;
   if FirstFlush then begin
    FirstFlush:=false;
    fSceneInstance.SetGlobalResources(aCommandBuffer,aPipelineLayout,aRenderPassIndex,aInFlightFrameIndex);
   end;

   if WasFirstFlush then begin

    fGroup.SetGroupResources(aCommandBuffer,aPipelineLayout,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);

    if assigned(aOnSetRenderPassResources) then begin
     aOnSetRenderPassResources(aCommandBuffer,aPipelineLayout,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);
    end;

   end;

   aCommandBuffer.CmdDrawIndexed(IndicesCount,1,IndicesStart,0,0);

   First:=false;

   LastPrimitiveTopology:=PrimitiveTopology;

   LastDoubleSided:=DoubleSided;

  end else begin

   if First then begin
    First:=false;
    LastPrimitiveTopology:=PrimitiveTopology;
    LastDoubleSided:=DoubleSided;
   end;

  end;

  IndicesStart:=PrimitiveIndexRange^.Index;
  IndicesCount:=0;

 end;
begin
 if fActives[aInFlightFrameIndex] and ((fVisibleBitmap and (TpvUInt32(1) shl aRenderPassIndex))<>0) then begin

  Culling:=fGroup.fCulling;

  Scene:=fScenes[aInFlightFrameIndex];

  if assigned(Scene) then begin

   LastPrimitiveTopology:=TpvScene3D.TPrimitiveTopology.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
   PrimitiveTopology:=TpvScene3D.TPrimitiveTopology.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;

   LastDoubleSided:=false;
   DoubleSided:=false;

   Material:=nil;

   VisibleBit:=TpvUInt32(1) shl aRenderPassIndex;

   First:=true;

   FirstFlush:=true;

   IndicesStart:=0;
   IndicesCount:=0;

   for SceneMaterialIndex:=0 to Scene.fMaterials.Count-1 do begin

    SceneMaterial:=Scene.fMaterials[SceneMaterialIndex];

    if SceneMaterial.fCountIndices>0 then begin

     Material:=SceneMaterial.fMaterial;

     if Material.fVisible and (Material.fData.AlphaMode in aMaterialAlphaModes) then begin

      DoubleSided:=Material.fData.DoubleSided;

      if Culling then begin
       PrimitiveIndexRanges:=@SceneMaterial.fPrimitiveIndexRanges;
      end else begin
       PrimitiveIndexRanges:=@SceneMaterial.fCombinedPrimitiveIndexRanges;
      end;

      for PrimitiveIndexRangeIndex:=0 to PrimitiveIndexRanges^.Count-1 do begin

       PrimitiveIndexRange:=@PrimitiveIndexRanges^.Items[PrimitiveIndexRangeIndex];

       if ((PrimitiveIndexRange^.Count>0) and
           ((not Culling) or
            ((fNodes[PrimitiveIndexRange^.Node].VisibleBitmap and VisibleBit)<>0))) then begin

        PrimitiveTopology:=PrimitiveIndexRange^.PrimitiveTopology;

        if (LastPrimitiveTopology<>PrimitiveTopology) or
           (LastDoubleSided<>DoubleSided) or
           (IndicesCount=0) or
           ((IndicesStart+IndicesCount)<>PrimitiveIndexRange^.Index) then begin
         Flush;
        end;

        inc(IndicesCount,PrimitiveIndexRange^.Count);

        if First then begin
         First:=false;
         LastPrimitiveTopology:=PrimitiveTopology;
         LastDoubleSided:=DoubleSided;
        end;

       end;

      end;

      //Flush;

     end;

    end;

   end;

   Flush;

  end;

 end;

end;

{ TpvScene3D }

constructor TpvScene3D.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource;const aUseBufferDeviceAddress:boolean);
var Index:TpvSizeInt;
begin

 inherited Create(aResourceManager,aParent);

 fLock:=TPasMPSpinLock.Create;

 fUseBufferDeviceAddress:=aUseBufferDeviceAddress;

 fUploaded:=false;

 fHasTransmission:=false;

 fTechniques:=TpvTechniques.Create;

 fImageListLock:=TPasMPSlimReaderWriterLock.Create;

 fImages:=TImages.Create;
 fImages.OwnsObjects:=false;

 fImageIDManager:=TIDManager.Create;

 fImageIDHashMap:=TImageIDHashMap.Create(nil);

 fImageHashMap:=TImageHashMap.Create(nil);

 fSamplerListLock:=TPasMPSlimReaderWriterLock.Create;

 fSamplers:=TSamplers.Create;
 fSamplers.OwnsObjects:=false;

 fSamplerIDManager:=TIDManager.Create;

 fSamplerIDHashMap:=TSamplerIDHashMap.Create(nil);

 fSamplerHashMap:=TSamplerHashMap.Create(nil);

 fTextureListLock:=TPasMPSlimReaderWriterLock.Create;

 fTextures:=TTextures.Create;
 fTextures.OwnsObjects:=false;

 fTextureIDManager:=TIDManager.Create;

 fTextureIDHashMap:=TTextureIDHashMap.Create(nil);

 fTextureHashMap:=TTextureHashMap.Create(nil);

 fMaterialListLock:=TPasMPSlimReaderWriterLock.Create;

 fMaterials:=TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 fMaterialIDManager:=TIDManager.Create;

 fMaterialIDHashMap:=TMaterialIDHashMap.Create(nil);

 fMaterialHashMap:=TMaterialHashMap.Create(nil);

 fDefaultSampler:=TSampler.Create(pvApplication.ResourceManager,self);
 fDefaultSampler.AssignFromDefault;
 fDefaultSampler.IncRef;

 fWhiteImage:=TpvScene3D.TImage.Create(pvApplication.ResourceManager,self);
 fWhiteImage.AssignFromWhiteTexture;
 fWhiteImage.IncRef;

 fWhiteTexture:=TpvScene3D.TTexture.Create(pvApplication.ResourceManager,self);
 fWhiteTexture.AssignFromWhiteTexture;
 fWhiteTexture.IncRef;

 fDefaultNormalMapImage:=TpvScene3D.TImage.Create(pvApplication.ResourceManager,self);
 fDefaultNormalMapImage.AssignFromDefaultNormalMapTexture;
 fDefaultNormalMapImage.IncRef;

 fDefaultNormalMapTexture:=TpvScene3D.TTexture.Create(pvApplication.ResourceManager,self);
 fDefaultNormalMapTexture.AssignFromDefaultNormalMapTexture;
 fDefaultNormalMapTexture.IncRef;

 fEmptyMaterial:=TpvScene3D.TMaterial.Create(pvApplication.ResourceManager,self);
 fEmptyMaterial.AssignFromEmpty;
 fEmptyMaterial.IncRef;

 for Index:=0 to length(fLights)-1 do begin
  fLights[Index]:=TpvScene3D.TLights.Create;
  fLights[Index].OwnsObjects:=true;
 end;

 for Index:=0 to length(fCountLights)-1 do begin
  fCountLights[Index]:=0;
 end;

 fPreviousViews.Initialize;

 fViews.Initialize;

 fGroupListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroups:=TGroups.Create;
 fGroups.OwnsObjects:=false;

 fGroupInstanceListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroupInstances:=TGroup.TInstances.Create;
 fGroupInstances.OwnsObjects:=false;

 ReleaseFrameDelay:=MaxInFlightFrames+1;

 fMeshComputeVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);

 // Group - Vertices
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(0,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 // Group - Cached vertices
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(1,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 // Group - Indices
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(2,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 // Group - Morph target vertices
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(3,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 // Group - Joint blocks
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(4,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 // Instance - Node matrices
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(5,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 // Instance - Morph target weights
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(6,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 fMeshComputeVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice,TVkDescriptorSetLayoutCreateFlags(VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT),true);
 fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                             VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(1,
                                             VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 fGlobalVulkanDescriptorSetLayout.AddBinding(2,
                                             VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                             1,
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             []);
 if fUseBufferDeviceAddress then begin
  fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                              VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                              1,
                                              TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                              []);
 end else begin
  fGlobalVulkanDescriptorSetLayout.AddBinding(3,
                                              VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                              1,
                                              TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                              []);
 end;
 fGlobalVulkanDescriptorSetLayout.AddBinding(4,
                                             VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                             length(fImageInfos),
                                             TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                             [],
                                             TVkDescriptorBindingFlags(VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT) or
                                             TVkDescriptorBindingFlags(VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT) or
                                             TVkDescriptorBindingFlags(VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT));
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

 for Index:=0 to length(fInFlightFrameBufferMemoryBarriers)-1 do begin
  fInFlightFrameBufferMemoryBarriers[Index].Initialize;
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

 for Index:=0 to length(fGlobalVulkanViewUniformBuffers)-1 do begin
  FreeAndNil(fGlobalVulkanViewUniformBuffers[Index]);
 end;

 for Index:=0 to length(fAABBTreeStates)-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to length(fInFlightFrameBufferMemoryBarriers)-1 do begin
  fInFlightFrameBufferMemoryBarriers[Index].Finalize;
 end;

 FreeAndNil(fAABBTree);

 for Index:=0 to length(fLightAABBTreeStates)-1 do begin
  fLightAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to length(fLightBuffers)-1 do begin
  FreeAndNil(fLightBuffers[Index]);
 end;

 FreeAndNil(fLightAABBTree);

 FreeAndNil(fMeshComputeVulkanDescriptorSetLayout);

 FreeAndNil(fGlobalVulkanDescriptorSetLayout);

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
 FreeAndNil(fMaterialIDHashMap);
 FreeAndNil(fMaterialIDManager);
 FreeAndNil(fMaterialListLock);

 FreeAndNil(fWhiteTexture);

 FreeAndNil(fDefaultNormalMapTexture);

 while fTextures.Count>0 do begin
  fTextures[fTextures.Count-1].Free;
 end;
 FreeAndNil(fTextures);
 FreeAndNil(fTextureHashMap);
 FreeAndNil(fTextureIDHashMap);
 FreeAndNil(fTextureIDManager);
 FreeAndNil(fTextureListLock);

 FreeAndNil(fDefaultSampler);

 while fSamplers.Count>0 do begin
  fSamplers[fSamplers.Count-1].Free;
 end;
 FreeAndNil(fSamplers);
 FreeAndNil(fSamplerHashMap);
 FreeAndNil(fSamplerIDHashMap);
 FreeAndNil(fSamplerIDManager);
 FreeAndNil(fSamplerListLock);

 FreeAndNil(fWhiteImage);

 FreeAndNil(fDefaultNormalMapImage);

 while fImages.Count>0 do begin
  fImages[fImages.Count-1].Free;
 end;
 FreeAndNil(fImages);
 FreeAndNil(fImageHashMap);
 FreeAndNil(fImageIDHashMap);
 FreeAndNil(fImageIDManager);
 FreeAndNil(fImageListLock);

 FreeAndNil(fTechniques);

 fPreviousViews.Finalize;

 fViews.Finalize;

 FreeAndNil(fLock);

 inherited Destroy;
end;

procedure TpvScene3D.AddInFlightFrameBufferMemoryBarrier(const aInFlightFrameIndex:TpvSizeInt;
                                                          const aBuffer:TpvVulkanBuffer);
var Index:TpvSizeInt;
    BufferMemoryBarrier:PVkBufferMemoryBarrier;
begin
 if assigned(aBuffer) then begin
  Index:=fInFlightFrameBufferMemoryBarriers[aInFlightFrameIndex].AddNew;
  BufferMemoryBarrier:=@fInFlightFrameBufferMemoryBarriers[aInFlightFrameIndex].Items[Index];
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

procedure TpvScene3D.Upload;
var Group:TGroup;
    Index:TpvSizeInt;
    ViewUniformBuffer:TpvVulkanBuffer;
    Material:TMaterial;
    Texture:TTexture;
    UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
    DeviceAddress:TVkDeviceAddress;
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try
     fDefaultSampler.Upload;
     for Index:=0 to length(fLightBuffers)-1 do begin
      fLightBuffers[Index].Upload;
     end;
     for Index:=0 to length(fGlobalVulkanViewUniformBuffers)-1 do begin
      ViewUniformBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                SizeOf(TGlobalViewUniformBuffer),
                                                TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                [],
                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                0,
                                                0,
                                                0,
                                                0,
                                                [TpvVulkanBufferFlag.PersistentMapped]);
      try

      finally
       fGlobalVulkanViewUniformBuffers[Index]:=ViewUniformBuffer;
      end;
     end;
     fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),length(fImageInfos)*length(fGlobalVulkanDescriptorSets));
     fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,length(fGlobalVulkanDescriptorSets)*3);
     fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,length(fGlobalVulkanDescriptorSets)*4);
     fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,length(fGlobalVulkanDescriptorSets)*length(fImageInfos));
     fGlobalVulkanDescriptorPool.Initialize;
     for Group in fGroups do begin
      if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
       Group.Upload;
      end;
     end;
     for Index:=0 to length(TMaterialBufferData)-1 do begin
      fMaterialBufferData[Index]:=TMaterial.DefaultShaderData;
     end;
     for Index:=0 to fMaterials.Count-1 do begin
      Material:=fMaterials[Index];
      if (Material.ID>0) and (Material.ID<length(TMaterialBufferData)) then begin
       fMaterialBufferData[Material.ID]:=Material.fShaderData;
      end;
     end;

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

         for Index:=0 to length(fVulkanMaterialDataBuffers)-1 do begin

          fVulkanMaterialDataBuffers[Index]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                    SizeOf(TMaterialBufferData),
                                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),
                                                                    TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                    [],
                                                                    0,
                                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    []);

          fVulkanMaterialDataBuffers[Index].UploadData(UniversalQueue,
                                                       UniversalCommandBuffer,
                                                       UniversalFence,
                                                       fMaterialBufferData,
                                                       0,
                                                       SizeOf(TMaterialBufferData),
                                                       TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);

          if fUseBufferDeviceAddress then begin

           DeviceAddress:=fVulkanMaterialDataBuffers[Index].DeviceAddress;

           fVulkanMaterialUniformBuffers[Index]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                                        SizeOf(TVkDeviceAddress),
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
           fVulkanMaterialUniformBuffers[Index].UploadData(UniversalQueue,
                                                           UniversalCommandBuffer,
                                                           UniversalFence,
                                                           DeviceAddress,
                                                           0,
                                                           SizeOf(TVkDeviceAddress),
                                                           TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);

          end else begin

           fVulkanMaterialUniformBuffers[Index]:=nil;

          end;

         end;

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

     for Index:=0 to length(fImageInfos)-1 do begin
      fImageInfos[Index]:=fWhiteTexture.GetDescriptorImageInfo(false);
     end;
     for Index:=0 to fTextures.Count-1 do begin
      Texture:=fTextures[Index];
      if Texture.fUploaded and (Texture.ID>0) and (((Texture.ID*2)+1)<length(fImageInfos)) then begin
       fImageInfos[(Texture.ID*2)+0]:=Texture.GetDescriptorImageInfo(false);
       fImageInfos[(Texture.ID*2)+1]:=Texture.GetDescriptorImageInfo(true);
      end;
     end;
     for Index:=0 to length(fGlobalVulkanDescriptorSets)-1 do begin
      fGlobalVulkanDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                        fGlobalVulkanDescriptorSetLayout);
      fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(0,
                                                              0,
                                                              1,
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                              [],
                                                              [fGlobalVulkanViewUniformBuffers[Index].DescriptorBufferInfo],
                                                              [],
                                                              false);
      fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(1,
                                                              0,
                                                              1,
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                              [],
                                                              [fLightBuffers[Index].fLightItemsVulkanBuffer.DescriptorBufferInfo],
                                                              [],
                                                              false);
      fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(2,
                                                              0,
                                                              1,
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                              [],
                                                              [fLightBuffers[Index].fLightTreeVulkanBuffer.DescriptorBufferInfo],
                                                              [],
                                                              false);
      if fUseBufferDeviceAddress then begin
       fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(3,
                                                               0,
                                                               1,
                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                               [],
                                                               [fVulkanMaterialUniformBuffers[Index].DescriptorBufferInfo],
                                                               [],
                                                               false);
      end else begin
       fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(3,
                                                               0,
                                                               1,
                                                               TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                               [],
                                                               [fVulkanMaterialDataBuffers[Index].DescriptorBufferInfo],
                                                               [],
                                                               false);
      end;
      fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(4,
                                                              0,
                                                              length(fImageInfos),
                                                              TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                              fImageInfos,
                                                              [],
                                                              [],
                                                              false);
      fGlobalVulkanDescriptorSets[Index].Flush;
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
     for Index:=0 to length(fGlobalVulkanViewUniformBuffers)-1 do begin
      FreeAndNil(fGlobalVulkanViewUniformBuffers[Index]);
     end;
     for Index:=0 to length(fLightBuffers)-1 do begin
      fLightBuffers[Index].Unload;
     end;
     for Group in fGroups do begin
      if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
       Group.Unload;
      end;
     end;
     for Index:=0 to length(fVulkanMaterialUniformBuffers)-1 do begin
      FreeAndNil(fVulkanMaterialUniformBuffers[Index]);
     end;
     for Index:=0 to length(fVulkanMaterialDataBuffers)-1 do begin
      FreeAndNil(fVulkanMaterialDataBuffers[Index]);
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
     fDefaultSampler.Unload;
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

procedure TpvScene3D.Update(const aInFlightFrameIndex:TpvSizeInt);
var Group:TpvScene3D.TGroup;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    LightAABBTreeState,AABBTreeState:TpvBVHDynamicAABBTree.PState;
    First:boolean;
    OldGeneration:TpvUInt32;
    LightBuffer:TpvScene3D.TLightBuffer;
begin

 fCountLights[aInFlightFrameIndex]:=0;

 for Group in fGroups do begin
  Group.Update(aInFlightFrameIndex);
 end;

 OldGeneration:=fLightAABBTreeStateGenerations[aInFlightFrameIndex];
 if TPasMPInterlocked.CompareExchange(fLightAABBTreeStateGenerations[aInFlightFrameIndex],fLightAABBTreeGeneration,OldGeneration)=OldGeneration then begin

  LightAABBTreeState:=@fLightAABBTreeStates[aInFlightFrameIndex];

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

  LightBuffer:=fLightBuffers[aInFlightFrameIndex];
  CollectLightAABBTreeLights(LightAABBTreeState^.TreeNodes,LightAABBTreeState^.Root,LightBuffer.fLightItems);
  fLightAABBTree.GetGPUSkipListNodes(LightBuffer.fLightTree,GetLightUserDataIndex);
  LightBuffer.Update(aInFlightFrameIndex);

 end;

 AABBTreeState:=@fAABBTreeStates[aInFlightFrameIndex];
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

procedure TpvScene3D.CullAABBTreeWithFrustums(const aFrustums:TpvFrustumDynamicArray;
                                              const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                              const aRoot:TpvSizeInt;
                                              const aVisibleBit:TPasMPUInt32);
 procedure ProcessNode(const aNode:TpvSizeint;const aMask:TpvUInt32);
 var Index:TpvSizeInt;
     TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
     Mask:TpvUInt32;
     OK:boolean;
 begin
  if aNode>=0 then begin
   TreeNode:=@aTreeNodes[aNode];
   Mask:=aMask;
   if length(aFrustums)>0 then begin
    if length(aFrustums)=1 then begin
     OK:=not ((((Mask and $80000000)<>0) and (aFrustums[0].AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)));
    end else begin
     OK:=false;
     for Index:=0 to length(aFrustums)-1 do begin
      if aFrustums[Index].AABBInFrustum(TreeNode^.AABB)<>TpvFrustum.COMPLETE_OUT then begin
       OK:=true;
       break;
      end;
     end;
    end;
   end else begin
    OK:=true;
   end;
   if OK then begin
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
var Index,StackPointer:TpvSizeInt;
    StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Mask:TpvUInt32;
    Stack:array[0..31] of TStackItem;
    OK:boolean;
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
    if length(aFrustums)>0 then begin
     if length(aFrustums)=1 then begin
      OK:=not ((((Mask and $80000000)<>0) and (aFrustums[0].AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)));
     end else begin
      OK:=false;
      for Index:=0 to length(aFrustums)-1 do begin
       if aFrustums[Index].AABBInFrustum(TreeNode^.AABB)<>TpvFrustum.COMPLETE_OUT then begin
        OK:=true;
        break;
       end;
      end;
     end;
    end else begin
     OK:=true;
    end;
    if OK then begin
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
    end;
    break;
   end;
  end;
 end;
end;

function TpvScene3DCompareIndirectLights(const a,b:pointer):TpvInt32;
begin
 result:=Sign((ord(TpvScene3D.TLight(b).fData.fType_=TpvScene3D.TLightData.TType.PrimaryDirectional) and 1)-
              (ord(TpvScene3D.TLight(a).fData.fType_=TpvScene3D.TLightData.TType.PrimaryDirectional) and 1));
 if result=0 then begin
  result:=Sign((ord(TpvScene3D.TLight(b).fData.fType_=TpvScene3D.TLightData.TType.Directional) and 1)-
               (ord(TpvScene3D.TLight(a).fData.fType_=TpvScene3D.TLightData.TType.Directional) and 1));
  if result=0 then begin
   result:=Sign(TpvScene3D.TLight(b).fViewSpacePosition.z-TpvScene3D.TLight(a).fViewSpacePosition.z);
   if result=0 then begin
   end;
  end;
 end;
end;

procedure TpvScene3D.CullLightAABBTreeWithFrustums(const aInFlightFrameIndex:TpvSizeInt;
                                                   const aFrustums:TpvFrustumDynamicArray;
                                                   const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                                   const aRoot:TpvSizeInt);
 procedure ProcessNode(const aNode:TpvSizeint;const aMask:TpvUInt32);
 var Index:TpvSizeInt;
     TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
     Mask:TpvUInt32;
     OK:boolean;
 begin
  if aNode>=0 then begin
   TreeNode:=@aTreeNodes[aNode];
   Mask:=aMask;
   if length(aFrustums)>0 then begin
    if length(aFrustums)=1 then begin
     OK:=not ((((Mask and $80000000)<>0) and (aFrustums[0].AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)));
    end else begin
     OK:=false;
     for Index:=0 to length(aFrustums)-1 do begin
      if aFrustums[Index].AABBInFrustum(TreeNode^.AABB)<>TpvFrustum.COMPLETE_OUT then begin
       OK:=true;
       break;
      end;
     end;
    end;
   end else begin
    OK:=true;
   end;
   if OK then begin
    if TreeNode^.UserData<>0 then begin
     if fCountIndirectLights[aInFlightFrameIndex]<MaxVisibleLights then begin
      fIndirectLights[aInFlightFrameIndex,fCountIndirectLights[aInFlightFrameIndex]]:=TpvScene3D.TLight(Pointer(TreeNode^.UserData));
      inc(fCountIndirectLights[aInFlightFrameIndex]);
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
var Index,StackPointer:TpvSizeInt;
    StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Mask:TpvUInt32;
    Stack:array[0..31] of TStackItem;
    OK:boolean;
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
    if length(aFrustums)>0 then begin
     if length(aFrustums)=1 then begin
      OK:=not ((((Mask and $80000000)<>0) and (aFrustums[0].AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)));
     end else begin
      OK:=false;
      for Index:=0 to length(aFrustums)-1 do begin
       if aFrustums[Index].AABBInFrustum(TreeNode^.AABB)<>TpvFrustum.COMPLETE_OUT then begin
        OK:=true;
        break;
       end;
      end;
     end;
    end else begin
     OK:=true;
    end;
    if OK then begin
     if TreeNode^.UserData<>0 then begin
      if fCountIndirectLights[aInFlightFrameIndex]<MaxVisibleLights then begin
       fIndirectLights[aInFlightFrameIndex,fCountIndirectLights[aInFlightFrameIndex]]:=TpvScene3D.TLight(Pointer(TreeNode^.UserData));
       inc(fCountIndirectLights[aInFlightFrameIndex]);
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
   LightItem^.Type_:=TpvUInt32(aLight.fDataPointer^.Type_);
   LightItem^.ShadowMapIndex:=0;
   InnerConeAngleCosinus:=cos(aLight.fDataPointer^.InnerConeAngle);
   OuterConeAngleCosinus:=cos(aLight.fDataPointer^.OuterConeAngle);
  {LightItem^.InnerConeCosinus:=InnerConeAngleCosinus;
   LightItem^.OuterConeCosinus:=OuterConeAngleCosinus;}
   LightItem^.LightAngleScale:=1.0/Max(1e-5,InnerConeAngleCosinus-OuterConeAngleCosinus);
   LightItem^.LightAngleOffset:=-(OuterConeAngleCosinus*LightItem^.LightAngleScale);
   LightItem^.ColorIntensity:=TpvVector4.InlineableCreate(aLight.fDataPointer^.fColor,aLight.fDataPointer^.fIntensity);
   LightItem^.PositionRange:=TpvVector4.InlineableCreate(aLight.fPosition,aLight.fDataPointer^.fRange);
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

procedure TpvScene3D.TransferViewsToPreviousViews;
begin
 fPreviousViews.Count:=0;
 fPreviousViews.Add(fViews);
end;

procedure TpvScene3D.ClearViews;
begin
 fViews.Count:=0;
end;

function TpvScene3D.AddView(const aView:TpvScene3D.TView):TpvSizeInt;
begin
 if (fViews.Count+1)<TpvScene3D.MaxViews then begin
  result:=fViews.Add(aView);
 end else begin
  result:=-1;
 end;
end;

function TpvScene3D.AddViews(const aViews:array of TpvScene3D.TView):TpvSizeInt;
begin
 if (length(aViews)>0) and ((fViews.Count+length(aViews))<TpvScene3D.MaxViews) then begin
  result:=fViews.Add(aViews);
 end else begin
  result:=-1;
 end;
end;

procedure TpvScene3D.UpdateViews(const aInFlightFrameIndex:TpvSizeInt);
begin
 if fViews.Count>0 then begin
  Move(fViews.Items[0],
       fGlobalVulkanViews[aInFlightFrameIndex].Items[0],
       fViews.Count*SizeOf(TpvScene3D.TView));
  if fPreviousViews.Count=0 then begin
   fPreviousViews.Add(fViews);
  end;
  Move(fPreviousViews.Items[0],
       fGlobalVulkanViews[aInFlightFrameIndex].Items[fViews.Count],
       fPreviousViews.Count*SizeOf(TpvScene3D.TView));
  if assigned(fGlobalVulkanViewUniformBuffers[aInFlightFrameIndex]) then begin
   fGlobalVulkanViewUniformBuffers[aInFlightFrameIndex].UpdateData(fViews.Items[0],
                                                                    0,
                                                                    (fViews.Count+fPreviousViews.Count)*SizeOf(TpvScene3D.TView),
                                                                    FlushUpdateData
                                                                   );
  end;
 end;
end;

procedure TpvScene3D.PrepareLights(const aInFlightFrameIndex:TpvSizeInt;
                                   const aViewBaseIndex:TpvSizeInt;
                                   const aCountViews:TpvSizeInt;
                                   const aViewPortWidth:TpvInt32;
                                   const aViewPortHeight:TpvInt32;
                                   const aFrustums:TpvFrustumDynamicArray);
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

 //Lights:=fLights[aInFlightFrameIndex];

 fCountIndirectLights[aInFlightFrameIndex]:=0;

 AABBTreeState:=@fLightAABBTreeStates[aInFlightFrameIndex];

 CullLightAABBTreeWithFrustums(aInFlightFrameIndex,aFrustums,AABBTreeState^.TreeNodes,AABBTreeState^.Root);

 if fCountIndirectLights[aInFlightFrameIndex]>0 then begin
// IndirectIntroSort(@fIndirectLights[aInFlightFrameIndex,0],0,fCountIndirectLights[aInFlightFrameIndex],TpvScene3DCompareIndirectLights);
 end;

end;

procedure TpvScene3D.Prepare(const aInFlightFrameIndex:TpvSizeInt;
                             const aRenderPassIndex:TpvSizeInt;
                             const aViewBaseIndex:TpvSizeInt;
                             const aCountViews:TpvSizeInt;
                             const aViewPortWidth:TpvInt32;
                             const aViewPortHeight:TpvInt32;
                             const aLights:boolean=true;
                             const aFrustumCulling:boolean=true);
var Index:TpvSizeInt;
    VisibleBit:TPasMPUInt32;
    Frustums:TpvFrustumDynamicArray;
    Group:TpvScene3D.TGroup;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    AABBTreeState:TpvBVHDynamicAABBTree.PState;
    View:TpvScene3D.PView;
begin

 if (aViewBaseIndex>=0) and (aCountViews>0) then begin

  VisibleBit:=TpvUInt32(1) shl (aRenderPassIndex and 31);

  Frustums:=nil;
  try

   if aFrustumCulling then begin

    SetLength(Frustums,aCountViews);

    for GroupInstance in fGroupInstances do begin
     TPasMPInterlocked.BitwiseAnd(GroupInstance.fVisibleBitmap,not VisibleBit);
    end;

    AABBTreeState:=@fAABBTreeStates[aInFlightFrameIndex];

    for Index:=0 to aCountViews-1 do begin
     View:=@fGlobalVulkanViews[aInFlightFrameIndex].Items[aViewBaseIndex+Index];
     Frustums[Index].Init(View^.ViewMatrix,View^.ProjectionMatrix);
    end;

    CullAABBTreeWithFrustums(Frustums,AABBTreeState^.TreeNodes,AABBTreeState^.Root,VisibleBit);

   end else begin

    for GroupInstance in fGroupInstances do begin
     TPasMPInterlocked.BitwiseOr(GroupInstance.fVisibleBitmap,VisibleBit);
    end;

   end;

   for Group in fGroups do begin
    if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
     Group.Prepare(aInFlightFrameIndex,
                   aRenderPassIndex,
                   Frustums);
    end;
   end;

   if aLights then begin
    PrepareLights(aInFlightFrameIndex,
                  aViewBaseIndex,
                  aCountViews,
                  aViewPortWidth,
                  aViewPortHeight,
                  Frustums);
   end else begin
    fCountIndirectLights[aInFlightFrameIndex]:=0;
   end;

  finally
   Frustums:=nil;
  end;

 end;

end;

procedure TpvScene3D.SetGlobalResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                        const aPipelineLayout:TpvVulkanPipelineLayout;
                                        const aRenderPassIndex:TpvSizeInt;
                                        const aInFlightFrameIndex:TpvSizeInt);
begin
 if not fSetGlobalResourcesDone[aRenderPassIndex] then begin
  fSetGlobalResourcesDone[aRenderPassIndex]:=true;
  aCommandBuffer.CmdPushConstants(aPipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT),
                                  0,
                                  SizeOf(TpvScene3D.TVertexStagePushConstants),
                                  @fVertexStagePushConstants[aRenderPassIndex]);
  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       aPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);
 end;
end;

procedure TpvScene3D.UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                          const aInFlightFrameIndex:TpvSizeInt;
                                          const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aPipelineLayout:TpvVulkanPipelineLayout);
var Group:TpvScene3D.TGroup;
begin
 for Group in fGroups do begin
  if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
   Group.UpdateCachedVertices(aPipeline,
                              aInFlightFrameIndex,
                              aCommandBuffer,
                              aPipelineLayout);
  end;
 end;
end;

procedure TpvScene3D.Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                          const aInFlightFrameIndex:TpvSizeInt;
                          const aRenderPassIndex:TpvSizeInt;
                          const aViewBaseIndex:TpvSizeInt;
                          const aCountViews:TpvSizeInt;
                          const aCommandBuffer:TpvVulkanCommandBuffer;
                          const aPipelineLayout:TpvVulkanPipelineLayout;
                          const aOnSetRenderPassResources:TOnSetRenderPassResources;
                          const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes);
var VertexStagePushConstants:TpvScene3D.PVertexStagePushConstants;
    Group:TpvScene3D.TGroup;
    VisibleBit:TPasMPUInt32;
    Pipeline:TpvVulkanPipeline;
begin

 if (aViewBaseIndex>=0) and (aCountViews>0) then begin

  Pipeline:=nil;

  VisibleBit:=TpvUInt32(1) shl (aRenderPassIndex and 31);

  VertexStagePushConstants:=@fVertexStagePushConstants[aRenderPassIndex];
  VertexStagePushConstants^.ViewBaseIndex:=aViewBaseIndex;
  VertexStagePushConstants^.CountViews:=aCountViews;

  if fInFlightFrameBufferMemoryBarriers[aInFlightFrameIndex].Count>0 then begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or pvApplication.VulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                     0,
                                     0,
                                     nil,
                                     fInFlightFrameBufferMemoryBarriers[aInFlightFrameIndex].Count,
                                     @fInFlightFrameBufferMemoryBarriers[aInFlightFrameIndex].Items[0],
                                     0,
                                     nil);
   fInFlightFrameBufferMemoryBarriers[aInFlightFrameIndex].Count:=0;
  end;

  fSetGlobalResourcesDone[aRenderPassIndex]:=false;

  for Group in fGroups do begin
   if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
    Group.Draw(aGraphicsPipelines,
               aPreviousInFlightFrameIndex,
               aInFlightFrameIndex,
               aRenderPassIndex,
               aCommandBuffer,
               Pipeline,
               aPipelineLayout,
               aOnSetRenderPassResources,
               aMaterialAlphaModes);
   end;
  end;

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

procedure TpvScene3D.InitializeGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline;const aWithPreviousPosition:boolean=false);
begin
 aPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3D.TCachedVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 if aWithPreviousPosition then begin
  aPipeline.VertexInputState.AddVertexInputBindingDescription(1,SizeOf(TpvScene3D.TCachedVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 end;
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.Position)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.MaterialID)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R16G16B16A16_SNORM,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.NormalSign)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R16G16B16_SNORM,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.Tangent)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.TexCoord0)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(5,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.TexCoord1)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(6,0,VK_FORMAT_R16G16B16A16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.Color0)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(7,0,VK_FORMAT_R16G16B16A16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.ModelScaleDummy)));
 if aWithPreviousPosition then begin
  aPipeline.VertexInputState.AddVertexInputAttributeDescription(8,1,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PCachedVertex(nil)^.Position)));
 end;
end;

initialization
end.

