(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2019, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
     PasVulkan.Framework;

type EpvScene3D=class(Exception);

     IpvScene3D=interface(IpvResource)['{82D8BA65-626A-4A20-829E-51480B151116}']
     end;

     TpvScene3D=class(TpvResource,IpvScene3D)
      public
       type TVertexAttributeBindingLocations=class
             public
              const Position=0;
                    MorphTargetVertexBaseIndex=1;
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
            TVertex=packed record                    // Minimum required vertex structure for to be GLTF 2.0 conformant
             case boolean of
              false:(
               Position:TpvVector3;                  //  12   12 (32-bit float 3D vector)
               MorphTargetVertexBaseIndex:TpvUInt32; // + 4 = 16 (unsigned 32-bit morph target vertex base index)
               TangentSpace:TInt16Vector4;           // + 8 = 24 (signed 16-bit integer QTangent)
               TexCoord0:TpvVector2;                 // + 8 = 32 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               TexCoord1:TpvVector2;                 // + 8 = 40 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               Color0:TpvHalfFloatVector4;           // + 8 = 48 (must be at least half-float for HDR)
               Joints:TUInt16Vector4;                // + 8 = 56 (node-wise indirect indices for a 16-bit index to 32-bit index lookup table)
               Weights:TpvHalfFloatVector4;          // + 8 = 64 (half-float should be enough for it)
              );                                     //  ==   ==
              true:(                                 //  64   64 per vertex
               Padding:array[0..63] of TpvUInt8;
              );
            end;
            PVertex=^TVertex;
            TVertices=array of TVertex;
            IBaseObject=interface(IpvResource)['{9B6429EC-861D-4266-A7CB-408724C6AD27}']
             procedure Upload;
             procedure Unload;
            end;
            TBaseObject=class(TpvResource,IBaseObject)
             private
              fSceneInstance:TpvScene3D;
              fName:TpvUTF8String;
              fUploaded:TPasMPBool32;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Upload; virtual;
              procedure Unload; virtual;
             public
              property SceneInstance:TpvScene3D read fSceneInstance;
             published
              property Name:TpvUTF8String read fName write fName;
              property Uploaded:TPasMPBool32 read fUploaded;
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
              procedure Upload; override;
              procedure Unload; override;
              function GetHashData:THashData;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceImage:TPasGLTF.TImage);
            end;
            TIImage=TpvGenericList<IImage>;
            TImages=TpvObjectGenericList<TImage>;
            ISampler=interface(IBaseObject)['{BD753AB1-76A3-43F4-ADD9-4EF41DD280B4}']
            end;
            TSampler=class(TBaseObject,ISampler)
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
              procedure Upload; override;
              procedure Unload; override;
              function GetHashData:THashData;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSampler:TPasGLTF.TSampler);
             published
              property Sampler:TpvVulkanSampler read fSampler;
            end;
            TISampler=TpvGenericList<ISampler>;
            TSamplers=TpvObjectGenericList<TSampler>;
            ITexture=interface(IBaseObject)['{910CB49F-5700-49AD-8C48-49DF517E7850}']
            end;
            TTexture=class(TBaseObject,ITexture)
             public
              type THashData=packed record
                    Image:TImage;
                    Sampler:TSampler;
                   end;
                   PHashData=^THashData;
             private
              fImage:IImage;
              fSampler:ISampler;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Upload; override;
              procedure Unload; override;
              function GetHashData:THashData;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceTexture:TPasGLTF.TTexture;const aImageMap:TImages;const aSamplerMap:TSamplers);
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
                   TShaderData=packed record // 128 bytes (and 80 bytes when without padding in the moment)
                    case boolean of
                     false:(
                      BaseColorFactor:TpvVector4;
                      SpecularFactor:TpvVector4; // actually TpvVector3, but for easier and more convenient alignment reasons a TpvVector4
                      EmissiveFactor:TpvVector4; // actually TpvVector3, but for easier and more convenient alignment reasons a TpvVector4
                      MetallicRoughnessNormalScaleOcclusionStrengthFactor:TpvVector4;
                      // uvec4 AlphaCutOffFlags begin
                       AlphaCutOff:TpvFloat; // for with uintBitsToFloat on GLSL code side
                       Flags:TpvUInt32;
                       TextureCoords:TpvUInt32;
                       Reversed:TpvUInt32;
                      // uvec4 uAlphaCutOffFlags end
                     );
                     true:(
                      Padding:array[0..127] of TpvUInt8;
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
              fLock:TPasMPSpinLock;
              fShaderDataUniformBlockBuffer:TpvVulkanBuffer;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMaterial:TPasGLTF.TMaterial;const aTextureMap:TTextures);
              procedure FillShaderData;
            end;
            TIMaterials=TpvGenericList<IMaterial>;
            TMaterials=TpvObjectGenericList<TMaterial>;
            TGroup=class(TBaseObject,IGroup) // A group is a GLTF scene in a uber-scene
             public
              type TNode=class;
                   TMesh=class;
                   TMorphTargetVertex=packed record
                    case boolean of
                     false:(
                      Position:TpvVector4;               //  16    0
                      Normal:TpvHalfFloatVector3;        // + 6   16
                      Tangent:TpvHalfFloatVector3;       // + 6   22
                      Reversed:TpvUInt16;                // + 2   24
                      Index:TpvUInt32;                   // + 4   28
                      Next:TpvUInt32;                    // + 4   32
                     );                                  //  ==   ==
                     true:(                              //  32   32 per vertex
                      Padding:array[0..32] of TpvUInt8;
                     );
                   end;
                   PMorphTargetVertex=^TMorphTargetVertex;
                   TMorphTargetVertexDynamicArray=array of TMorphTargetVertex;
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
                     function GetAnimationBeginTime:TpvFloat;
                     function GetAnimationEndTime:TpvFloat;
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
                     fType:TType;
                     fOrthographic:TOrthographic;
                     fPerspective:TPerspective;
                    public
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceCamera:TPasGLTF.TCamera);
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
                            MorphTargetBaseIndex:TpvSizeUInt;
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
                     fStorageBufferObjectOffset:TpvSizeUInt;
                     fStorageBufferObjectSize:TpvSizeUInt;
                    public
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceSkin:TPasGLTF.TSkin);
                   end;
                   TSkins=TpvObjectGenericList<TSkin>;
                   TSkinDynamicArray=TpvDynamicArray<TSkin>;
                   TNodes=TpvObjectGenericList<TNode>;
                   TNode=class(TGroupObject)
                    public
                     type TChildNodeIndices=TpvDynamicArray<TpvSizeInt>;
                    private
                     fChildNodeIndices:TChildNodeIndices;
                     fChildren:TNodes;
                     fMesh:TMesh;
                     fCamera:TCamera;
                     fSkin:TSkin;
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
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceNode:TPasGLTF.TNode);
                    published
                     property Children:TNodes read fChildren;
                     property Camera:TCamera read fCamera;
                     property Mesh:TMesh read fMesh;
                     property Skin:TSkin read fSkin;
                   end;
                   TScene=class(TGroupObject)
                    private
                     fNodes:TNodes;
                    public
                     constructor Create(const aGroup:TGroup); override;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceScene:TPasGLTF.TScene);
                    published
                     property Nodes:TNodes read fNodes;
                   end;
                   TScenes=TpvObjectGenericList<TScene>;
             private
              fMaximalCountInstances:TpvSizeInt;
              fObjects:TIBaseObjects;
              fAnimations:TAnimations;
              fCameras:TCameras;
              fMeshes:TMeshes;
              fSkins:TSkins;
              fNodes:TNodes;
              fScenes:TScenes;
              fScene:TScene;
              fSkinStorageBufferSize:TpvSizeInt;
              fMorphTargetCount:TpvSizeInt;
              fMorphTargetShaderStorageBufferObject:TMorphTargetShaderStorageBufferObject;
              fMorphTargetVertexCount:TpvSizeInt;
              fMorphTargetVertexShaderStorageBufferObject:TMorphTargetVertexShaderStorageBufferObject;
              fNodeShaderStorageBufferObject:TNodeShaderStorageBufferObject;
              fLock:TPasMPSpinLock;
              procedure ConstructBuffers;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aMaximalCountInstances:TpvSizeInt=1);
             published
              property Objects:TIBaseObjects read fObjects;
              property Animations:TAnimations read fAnimations;
              property Cameras:TCameras read fCameras;
              property Meshes:TMeshes read fMeshes;
              property Skins:TSkins read fSkins;
              property Nodes:TNodes read fNodes;
              property Scenes:TScenes read fScenes;
              property Scene:TScene read fScene;
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
            TImageHashMap=TpvHashMap<TImage.THashData,TImage>;
            TSamplerHashMap=TpvHashMap<TSampler.THashData,TSampler>;
            TTextureHashMap=TpvHashMap<TTexture.THashData,TTexture>;
            TMaterialHashMap=TpvHashMap<TMaterial.THashData,TMaterial>;
      private
       fLock:TPasMPSpinLock;
       fUploaded:TPasMPBool32;
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
       fGroupInstances:TGroupInstances;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
       destructor Destroy; override;
       procedure Upload;
       procedure Unload;
     end;

implementation

uses PasVulkan.Application;

{ TpvScene3D.TBaseObject }

constructor TpvScene3D.TBaseObject.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fSceneInstance:=aParent as TpvScene3D;

 ReleaseFrameDelay:=MaxSwapChainImages+1;

 fUploaded:=false;

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

procedure TpvScene3D.TBaseObject.Upload;
begin
end;

procedure TpvScene3D.TBaseObject.Unload;
begin
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
  if fSceneInstance.fImageHashMap[fHashData]=self then begin
   fSceneInstance.fImageHashMap.Delete(fHashData);
  end;
 finally
  fSceneInstance.fImageListLock.Release;
 end;
 inherited BeforeDestruction;
end;


procedure TpvScene3D.TImage.Upload;
var GraphicsQueue:TpvVulkanQueue;
    GraphicsCommandPool:TpvVulkanCommandPool;
    GraphicsCommandBuffer:TpvVulkanCommandBuffer;
    GraphicsFence:TpvVulkanFence;
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try
     GraphicsQueue:=TpvVulkanQueue.Create(pvApplication.VulkanDevice,
                                          pvApplication.VulkanDevice.GraphicsQueue.Handle,
                                          pvApplication.VulkanDevice.GraphicsQueueFamilyIndex);
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
      FreeAndNil(GraphicsQueue);
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
 fSceneInstance.fSamplerListLock.Acquire;
 try
  fSceneInstance.fSamplers.Add(self);
 finally
  fSceneInstance.fSamplerListLock.Release;
 end;
end;

procedure TpvScene3D.TSampler.BeforeDestruction;
var HashData:THashData;
begin
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
 inherited BeforeDestruction;
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
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
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
var HashData:THashData;
begin
 HashData:=GetHashData;
 fImage:=nil;
 fSampler:=nil;
 fSceneInstance.fTextureListLock.Acquire;
 try
  fSceneInstance.fTextures.Remove(self);
  if fSceneInstance.fTextureHashMap[HashData]=self then begin
   fSceneInstance.fTextureHashMap.Delete(HashData);
  end;
 finally
  fSceneInstance.fTextureListLock.Release;
 end;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TTexture.Upload;
begin
 if assigned(fImage) then begin
  fImage.Upload;
 end;
 if assigned(fSampler) then begin
  fSampler.Upload;
 end;
end;

procedure TpvScene3D.TTexture.Unload;
begin
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
  fImage:=aImageMap[aSourceTexture.Source].InstanceInterface as TpvScene3D.IImage;
 end else begin
  raise EPasGLTFInvalidDocument.Create('Image index out of range');
 end;
 if (aSourceTexture.Sampler>=0) and (aSourceTexture.Sampler<aSamplerMap.Count) then begin
  fSampler:=aSamplerMap[aSourceTexture.Sampler].InstanceInterface as TpvScene3D.ISampler;
 end else begin
  raise EPasGLTFInvalidDocument.Create('Sampler index out of range');
 end;
end;

{ TpvScene3D.TMaterial }

constructor TpvScene3D.TMaterial.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

 fSceneInstance:=aParent as TpvScene3D;

 fData:=DefaultData;

 fLock:=TPasMPSpinLock.Create;

end;

destructor TpvScene3D.TMaterial.Destroy;
begin
 FreeAndNil(fLock);
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
  if fSceneInstance.fMaterialHashMap[fData]=self then begin
   fSceneInstance.fMaterialHashMap.Delete(fData);
  end;
 finally
  fSceneInstance.fMaterialListLock.Release;
 end;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TMaterial.Upload;
var UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try
     if assigned(fData.NormalTexture.Texture) then begin
      fData.NormalTexture.Texture.Upload;
     end;
     if assigned(fData.OcclusionTexture.Texture) then begin
      fData.OcclusionTexture.Texture.Upload;
     end;
     if assigned(fData.EmissiveTexture.Texture) then begin
      fData.EmissiveTexture.Texture.Upload;
     end;
     if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
      fData.PBRMetallicRoughness.BaseColorTexture.Texture.Upload;
     end;
     if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
      fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.Upload;
     end;
     if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
      fData.PBRSpecularGlossiness.DiffuseTexture.Texture.Upload;
     end;
     if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
      fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.Upload;
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
     UniversalQueue:=TpvVulkanQueue.Create(pvApplication.VulkanDevice,
                                           pvApplication.VulkanDevice.UniversalQueue.Handle,
                                           pvApplication.VulkanDevice.UniversalQueueFamilyIndex);
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
      FreeAndNil(UniversalQueue);
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
   fData.EmissiveTexture.Texture:=aTextureMap[aSourceMaterial.EmissiveTexture.Index].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.EmissiveTexture.Texture:=nil;
  end;
  fData.EmissiveTexture.TexCoord:=aSourceMaterial.EmissiveTexture.TexCoord;
  if (aSourceMaterial.NormalTexture.Index>=0) and (aSourceMaterial.NormalTexture.Index<aTextureMap.Count) then begin
   fData.NormalTexture.Texture:=aTextureMap[aSourceMaterial.NormalTexture.Index].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.NormalTexture.Texture:=nil;
  end;
  fData.NormalTexture.TexCoord:=aSourceMaterial.NormalTexture.TexCoord;
  fData.NormalTextureScale:=aSourceMaterial.NormalTexture.Scale;
  if (aSourceMaterial.OcclusionTexture.Index>=0) and (aSourceMaterial.OcclusionTexture.Index<aTextureMap.Count) then begin
   fData.OcclusionTexture.Texture:=aTextureMap[aSourceMaterial.OcclusionTexture.Index].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.OcclusionTexture.Texture:=nil;
  end;
  fData.OcclusionTexture.TexCoord:=aSourceMaterial.OcclusionTexture.TexCoord;
  fData.OcclusionTextureStrength:=aSourceMaterial.OcclusionTexture.Strength;
 end;

 begin
  fData.PBRMetallicRoughness.BaseColorFactor:=TpvVector4.InlineableCreate(aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[0],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[1],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[2],aSourceMaterial.PBRMetallicRoughness.BaseColorFactor[3]);
  if (aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index>=0) and (aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index<aTextureMap.Count) then begin
   fData.PBRMetallicRoughness.BaseColorTexture.Texture:=aTextureMap[aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.Index].InstanceInterface as TpvScene3D.ITexture;
  end else begin
   fData.PBRMetallicRoughness.BaseColorTexture.Texture:=nil;
  end;
  fData.PBRMetallicRoughness.BaseColorTexture.TexCoord:=aSourceMaterial.PBRMetallicRoughness.BaseColorTexture.TexCoord;
  fData.PBRMetallicRoughness.RoughnessFactor:=aSourceMaterial.PBRMetallicRoughness.RoughnessFactor;
  fData.PBRMetallicRoughness.MetallicFactor:=aSourceMaterial.PBRMetallicRoughness.MetallicFactor;
  if (aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index>=0) and (aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index<aTextureMap.Count) then begin
   fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture:=aTextureMap[aSourceMaterial.PBRMetallicRoughness.MetallicRoughnessTexture.Index].InstanceInterface as TpvScene3D.ITexture;
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
     if (Index>=0) and (Index<aTextureMap.Count) then begin
      fData.PBRSpecularGlossiness.DiffuseTexture.Texture:=aTextureMap[Index].InstanceInterface as TpvScene3D.ITexture;
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
     if (Index>=0) and (Index<aTextureMap.Count) then begin
      fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture:=aTextureMap[Index].InstanceInterface as TpvScene3D.ITexture;
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
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (0 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
   end;
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (1 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord and $f) shl (1 shl 2));
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
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (0 shl 2))) or (TpvUInt32(fData.PBRSpecularGlossiness.DiffuseTexture.TexCoord and $f) shl (0 shl 2));
   end;
   if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (1 shl 2))) or (TpvUInt32(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.TexCoord and $f) shl (1 shl 2));
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
    fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (0 shl 2))) or (TpvUInt32(fData.PBRMetallicRoughness.BaseColorTexture.TexCoord and $f) shl (0 shl 2));
   end;
   fShaderData.BaseColorFactor:=TpvVector4.InlineableCreate(fData.PBRMetallicRoughness.BaseColorFactor[0],fData.PBRMetallicRoughness.BaseColorFactor[1],fData.PBRMetallicRoughness.BaseColorFactor[2],fData.PBRMetallicRoughness.BaseColorFactor[3]);
  end;
  else begin
   Assert(false);
  end;
 end;
 if assigned(fData.NormalTexture.Texture) then begin
  fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (2 shl 2))) or (TpvUInt32(fData.NormalTexture.TexCoord and $f) shl (2 shl 2));
 end;
 if assigned(fData.OcclusionTexture.Texture) then begin
  fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (3 shl 2))) or (TpvUInt32(fData.OcclusionTexture.TexCoord and $f) shl (3 shl 2));
 end;
 if assigned(fData.EmissiveTexture.Texture) then begin
  fShaderData.TextureCoords:=(fShaderData.TextureCoords and not ($f shl (4 shl 2))) or (TpvUInt32(fData.EmissiveTexture.TexCoord and $f) shl (4 shl 2));
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

constructor TpvScene3D.TGroup.TCamera.Create(const aGroup:TGroup);
begin
 inherited Create(aGroup);
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
    MaxCountTargets:TpvSizeInt;
    SourceMeshPrimitive:TPasGLTF.TMesh.TPrimitive;
    SourceMeshPrimitiveTarget:TPasGLTF.TAttributes;
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

 fName:=aSourceMesh.Name;

 SetLength(fPrimitives,aSourceMesh.Primitives.Count);

 fBoundingBox:=TpvAABB.Create(TpvVector3.InlineableCreate(Infinity,Infinity,Infinity),
                              TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity));

//DestinationMesh^.JointWeights:=nil;

 MaxCountTargets:=0;

 for PrimitiveIndex:=0 to aSourceMesh.Primitives.Count-1 do begin

  SourceMeshPrimitive:=aSourceMesh.Primitives.Items[PrimitiveIndex];

  DestinationMeshPrimitive:=@fPrimitives[PrimitiveIndex];

  DestinationMeshPrimitive^.Material:=SourceMeshPrimitive.Material;

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

  begin
   // Generate vertex array buffer
   SetLength(DestinationMeshPrimitive^.Vertices,length(TemporaryPositions));
   for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
    Vertex:=@DestinationMeshPrimitive^.Vertices[VertexIndex];
    FillChar(Vertex^,SizeOf(TVertex),#0);
    Vertex^.Position:=TpvVector3(pointer(@TemporaryPositions[VertexIndex])^);
    Vertex^.MorphTargetVertexBaseIndex:=TpvUInt32($ffffffff);
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
       Vertex:=@DestinationMeshPrimitive^.Vertices[VertexIndex];
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
     SetLength(DestinationMeshPrimitiveTarget^.Vertices,length(DestinationMeshPrimitive^.Vertices));
     for VertexIndex:=0 to length(DestinationMeshPrimitiveTarget^.Vertices)-1 do begin
      DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex];
      Vertex:=@DestinationMeshPrimitive^.Vertices[VertexIndex];
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

   if length(DestinationMeshPrimitive^.Targets)>0 then begin
    for VertexIndex:=0 to length(DestinationMeshPrimitive^.Vertices)-1 do begin
     Vertex:=@DestinationMeshPrimitive^.Vertices[VertexIndex];
     Vertex^.MorphTargetVertexBaseIndex:=fGroup.fMorphTargetVertexCount;
     inc(fGroup.fMorphTargetVertexCount,length(DestinationMeshPrimitive^.Targets));
    end;
   end else begin
    for VertexIndex:=0 to length(DestinationMeshPrimitive^.Vertices)-1 do begin
     Vertex:=@DestinationMeshPrimitive^.Vertices[VertexIndex];
     Vertex^.MorphTargetVertexBaseIndex:=TpvUInt32($ffffffff);
    end;
   end;

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

{ TpvScene3D.TNode }

constructor TpvScene3D.TGroup.TNode.Create(const aGroup:TGroup);
begin

 inherited Create(aGroup);

 fChildNodeIndices.Initialize;

 fChildren:=TNodes.Create;
 fChildren.OwnsObjects:=false;

 fMesh:=nil;

 fSkin:=nil;

 fShaderStorageBufferObjectOffset:=0;

 fShaderStorageBufferObjectSize:=0;

end;

destructor TpvScene3D.TGroup.TNode.Destroy;
begin

 fMesh:=nil;

 fSkin:=nil;

 FreeAndNil(fChildren);

 fChildNodeIndices.Finalize;

 inherited Destroy;

end;

procedure TpvScene3D.TGroup.TNode.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceNode:TPasGLTF.TNode);
var WeightIndex,ChildrenIndex,Count:TPasGLTFSizeInt;
    Mesh:TMesh;
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

constructor TpvScene3D.TGroup.TScene.Create(const aGroup:TGroup);
begin
 inherited Create(aGroup);
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

 fObjects:=TIBaseObjects.Create;

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

 fScenes:=TScenes.Create;
 fScenes.OwnsObjects:=true;

 fSkinStorageBufferSize:=0;

 fMorphTargetCount:=0;

 fMorphTargetVertexCount:=0;

 fMaximalCountInstances:=1;

end;

destructor TpvScene3D.TGroup.Destroy;
begin

 FreeAndNil(fScenes);

 FreeAndNil(fNodes);

 FreeAndNil(fSkins);

 FreeAndNil(fMeshes);

 FreeAndNil(fCameras);

 FreeAndNil(fAnimations);

 FreeAndNil(fObjects);

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
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try

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
 procedure InitializeMorphTargetBuffers;
  procedure FillMorphTargetVertexShaderStorageBufferObject(const aMorphTargetVertexShaderStorageBufferObject:PMorphTargetVertexShaderStorageBufferObject;
                                                           const aPrimitive:TMesh.PPrimitive;
                                                           const aDestinationVertex:PMorphTargetVertex;
                                                           const aTargetBaseIndex:TpvSizeInt);
  var TargetIndex,
      VertexIndex,
      MorphTargetVertexIndex:TpvSizeInt;
      Vertex:PVertex;
      SourceVertex:TMesh.TPrimitive.TTarget.PTargetVertex;
      DestinationVertex:PMorphTargetVertex;
      Target:TMesh.TPrimitive.PTarget;
  begin
   for VertexIndex:=0 to length(aPrimitive^.Vertices)-1 do begin
    Vertex:=@aPrimitive^.Vertices[VertexIndex];
    for TargetIndex:=0 to length(aPrimitive^.Targets)-1 do begin
     MorphTargetVertexIndex:=TpvSizeInt(Vertex^.MorphTargetVertexBaseIndex)+TargetIndex;
     Target:=@aPrimitive^.Targets[TargetIndex];
     SourceVertex:=@Target^.Vertices[VertexIndex];
     DestinationVertex:=aDestinationVertex;
     inc(DestinationVertex,MorphTargetVertexIndex);
     DestinationVertex^.Position:=TpvVector4.InlineableCreate(SourceVertex^.Position,0.0);
     DestinationVertex^.Normal:=SourceVertex^.Normal;
     DestinationVertex^.Tangent:=SourceVertex^.Tangent;
     DestinationVertex^.Index:=aTargetBaseIndex+TargetIndex;
     if (TargetIndex+1)<length(aPrimitive^.Targets) then begin
      DestinationVertex^.Next:=MorphTargetVertexIndex+1;
     end else begin
      DestinationVertex^.Next:=TpvUInt32($ffffffff);
     end;
    end;
   end;
  end;
 var MeshIndex,
     PrimitiveIndex,
     TargetIndex,
     Index,
     ItemDataSize:TpvSizeInt;
     Mesh:TMesh;
     Primitive:TMesh.PPrimitive;
 begin
  fMorphTargetCount:=0;
  fMorphTargetShaderStorageBufferObject.Count:=0;
  fMorphTargetShaderStorageBufferObject.Size:=0;
  fMorphTargetShaderStorageBufferObject.Data:=nil;
  try
   fMorphTargetVertexShaderStorageBufferObject.Count:=fMorphTargetVertexCount;
   fMorphTargetVertexShaderStorageBufferObject.Size:=fMorphTargetVertexCount*SizeOf(TMorphTargetVertex);
   fMorphTargetVertexShaderStorageBufferObject.Data:=nil;
   try
    if fMorphTargetVertexShaderStorageBufferObject.Size>0 then begin
     SetLength(fMorphTargetVertexShaderStorageBufferObject.Data,fMorphTargetVertexShaderStorageBufferObject.Size);
     FillChar(fMorphTargetVertexShaderStorageBufferObject.Data[0],fMorphTargetVertexShaderStorageBufferObject.Size,#$ff);
     for MeshIndex:=0 to fMeshes.Count-1 do begin
      Mesh:=fMeshes[MeshIndex];
      for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin
       Primitive:=@Mesh.fPrimitives[PrimitiveIndex];
       if length(Primitive^.Targets)>0 then begin
        Primitive^.MorphTargetBaseIndex:=fMorphTargetCount;
        FillMorphTargetVertexShaderStorageBufferObject(@fMorphTargetVertexShaderStorageBufferObject,
                                                       Primitive,
                                                       pointer(@fMorphTargetVertexShaderStorageBufferObject.Data[0]),
                                                       Primitive^.MorphTargetBaseIndex);
        inc(fMorphTargetCount,length(Primitive^.Targets));
       end else begin
        Primitive^.MorphTargetBaseIndex:=0;
       end;
      end;
     end;
    end;
   finally
    SetLength(fMorphTargetVertexShaderStorageBufferObject.Data,fMorphTargetVertexShaderStorageBufferObject.Size);
   end;
  finally
   fMorphTargetShaderStorageBufferObject.Count:=fMorphTargetCount*fMaximalCountInstances;
   fMorphTargetShaderStorageBufferObject.Size:=fMorphTargetShaderStorageBufferObject.Count*SizeOf(TpvFloat);
   SetLength(fMorphTargetShaderStorageBufferObject.Data,fMorphTargetShaderStorageBufferObject.Count*MaxSwapChainImages);
  end;
 end;
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
 InitializeMorphTargetBuffers;
 InitializeNodeMeshPrimitiveShaderStorageBufferObject;
end;

procedure TpvScene3D.TGroup.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aMaximalCountInstances:TpvSizeInt=1);
var ImageMap:TpvScene3D.TImages;
    SamplerMap:TpvScene3D.TSamplers;
    TextureMap:TpvScene3D.TTextures;
    MaterialMap:TpvScene3D.TMaterials;
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
   Animation:=TAnimation.Create(self);
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
   Camera:=TCamera.Create(self);
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
  fMorphTargetVertexCount:=0;
  for Index:=0 to aSourceDocument.Meshes.Count-1 do begin
   SourceMesh:=aSourceDocument.Meshes[Index];
   Mesh:=TMesh.Create(self);
   try
    Mesh.AssignFromGLTF(aSourceDocument,SourceMesh);
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
   Skin:=TSkin.Create(self);
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
 procedure ProcessNodes;
 var Index:TpvSizeInt;
     SourceNode:TPasGLTF.TNode;
     Node:TNode;
 begin
  for Index:=0 to aSourceDocument.Nodes.Count-1 do begin
   SourceNode:=aSourceDocument.Nodes[Index];
   Node:=TNode.Create(self);
   try
    Node.AssignFromGLTF(aSourceDocument,SourceNode);
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
   Scene:=TScene.Create(self);
   try
    Scene.AssignFromGLTF(aSourceDocument,SourceScene);
   finally
    fScenes.Add(Scene);
   end;
  end;
 end;
begin

 fMaximalCountInstances:=aMaximalCountInstances;

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

 ConstructBuffers;

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
 fGroupInstances:=TGroupInstances.Create;
 fGroupInstances.OwnsObjects:=false;

 ReleaseFrameDelay:=MaxSwapChainImages+1;

end;

destructor TpvScene3D.Destroy;
begin

 Unload;

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

 FreeAndNil(fLock);

 inherited Destroy;
end;

procedure TpvScene3D.Upload;
var Group:TGroup;
begin
 if not fUploaded then begin
  fLock.Acquire;
  try
   if not fUploaded then begin
    try
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
    finally
     fUploaded:=false;
    end;
   end;
  finally
   fLock.Release;
  end;
 end;
end;

initialization
end.

