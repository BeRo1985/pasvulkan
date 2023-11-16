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
     Kraft,
     PUCU,
     PasMP,
     PasJSON,
     PasGLTF,
     PasVulkan.Types,
     PasVulkan.Utils,
     PasVulkan.Math,
     PasVulkan.Hash.xxHash64,
     PasVulkan.Collections,
     PasVulkan.HighResolutionTimer,
     PasVulkan.IDManager,
     PasVulkan.Resources,
     PasVulkan.Techniques,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.BVH.DynamicAABBTree,
     PasVulkan.BVH.Triangles,
     PasVulkan.PooledObject,
     PasVulkan.Frustum,
     PasVulkan.BufferRangeAllocator,
     POCA;

type EpvScene3D=class(Exception);

     { TpvScene3D }

     TpvScene3D=class(TpvResource)
      public
       const MaxRenderPassIndices=32;
             MaxRendererInstances=32;
             MaxVisibleLights=65536;
             MaxDebugPrimitiveVertices=1 shl 20;
             MaxParticles=65536; // <= Must be power of two
             ParticleIndexMask=MaxParticles-1;
             MaxParticleVertices=MaxParticles*3;
             LightClusterSizeX=16;
             LightClusterSizeY=8;
             LightClusterSizeZ=32;
             LightClusterSize=LightClusterSizeX*LightClusterSizeY*LightClusterSizeZ;
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
       type TPrimitiveTopology=
             (
              Points=0,
              Lines=1,
              Triangles=2
             );
          //TPrimitiveTopology=VK_PRIMITIVE_TOPOLOGY_POINT_LIST..VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN;
            PPrimitiveTopology=^TPrimitiveTopology;
       const VulkanPrimitiveTopologies:array[TPrimitiveTopology] of TVkPrimitiveTopology=
              (
               VK_PRIMITIVE_TOPOLOGY_POINT_LIST,
               VK_PRIMITIVE_TOPOLOGY_LINE_LIST,
               VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
              );
       type TDoubleSided=boolean;
            TFrontFacesInversed=boolean;
            TFaceCullingMode=
             (
              None=0,
              Normal=1,
              Inversed=2
             );
            TBufferStreamingMode=
             (
              Direct=0,
              Staging=1
             );
            TGraphicsPipelines=array[TPrimitiveTopology,TFaceCullingMode] of TpvVulkanPipeline;
            TTextureRawIndex=
             (
              None=-1,
              PBRMetallicRoughnessBaseColorTexture=0,
              PBRMetallicRoughnessMetallicRoughnessTexture=1,
              PBRSpecularGlossinessDiffuseTexture=0,
              PBRSpecularGlossinessSpecularGlossinessTexture=1,
              PBRUnlitColorTexture=0,
              NormalTexture=2,
              OcclusionTexture=3,
              EmissiveTexture=4,
              PBRSheenColorTexture=5,
              PBRSheenRoughnessTexture=6,
              PBRClearCoatTexture=7,
              PBRClearCoatRoughnessTexture=8,
              PBRClearCoatNormalTexture=9,
              PBRSpecularSpecularTexture=10,
              PBRSpecularSpecularColorTexture=11,
              PBRIridescenceTexture=12,
              PBRIridescenceThicknessTexture=13,
              PBRTransmissionTexture=14,
              PBRVolumeThicknessTexture=15,
              PBRAnisotropyTexture=16,
              Dummy=256
             );
            TTextureIndex=
             (
              None=-1,
              PBRMetallicRoughnessBaseColorTexture=0,
              PBRMetallicRoughnessMetallicRoughnessTexture=1,
              NormalTexture=2,
              OcclusionTexture=3,
              EmissiveTexture=4,
              PBRSheenColorTexture=5,
              PBRSheenRoughnessTexture=6,
              PBRClearCoatTexture=7,
              PBRClearCoatRoughnessTexture=8,
              PBRClearCoatNormalTexture=9,
              PBRSpecularSpecularTexture=10,
              PBRSpecularSpecularColorTexture=11,
              PBRIridescenceTexture=12,
              PBRIridescenceThicknessTexture=13,
              PBRTransmissionTexture=14,
              PBRVolumeThicknessTexture=15,
              PBRSpecularGlossinessDiffuseTexture=16,
              PBRSpecularGlossinessSpecularGlossinessTexture=17,
              PBRUnlitColorTexture=18,
              PBRAnisotropyTexture=19
             );
            TVertexAttributeBinBoundingBoxesdingLocations=class
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
            TCachedRaytracingVertexAttributeBindingLocations=class
             public
              const Position=0;
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
            PSizeIntDynamicArray=^TSizeIntDynamicArray;
            TSizeIntDynamicArrayEx=array of TpvSizeInt;
            TView=packed record
             ViewMatrix:TpvMatrix4x4;
             ProjectionMatrix:TpvMatrix4x4;
             InverseViewMatrix:TpvMatrix4x4;
             InverseProjectionMatrix:TpvMatrix4x4;
            end;
            PView=^TView;
            TViews=TpvDynamicArray<TView>;
            TRendererInstanceIDManager=TpvGenericIDManager<TpvUInt32>;
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
            TInFlightFrameAABBs=array[0..MaxInFlightFrames-1] of TpvAABB;
       const MaxViews=65536 div SizeOf(TView);
       type TID=TpvUInt32;
            TIDManager=class(TpvGenericIDManager<TID>);
            TViewUniformBuffer=record
             Items:array[0..MaxViews-1] of TView;
            end;
            PViewUniformBuffer=^TViewUniformBuffer;
            TVulkanViewUniformBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
            TFreeQueueItem=record
             Counter:TpvInt32;
             Data:TObject;
            end;
            PFreeQueueItem=^TFreeQueueItem;
            TFreeQueue=TpvDynamicArrayList<TFreeQueueItem>;
            TGlobalVulkanInstanceMatrixBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
            TGlobalVulkanInstanceMatrixDynamicArray=TpvDynamicArray<TpvMatrix4x4>;
            PGlobalVulkanInstanceMatrixDynamicArray=^TGlobalVulkanInstanceMatrixDynamicArray;
            TGlobalVulkanInstanceMatrixDynamicArrays=array[0..MaxInFlightFrames-1] of TGlobalVulkanInstanceMatrixDynamicArray;
            TMeshComputeStagePushConstants=record
             IndexOffset:UInt32;
             CountIndices:UInt32;
            end;
            PMeshComputeStagePushConstants=^TMeshComputeStagePushConstants;
            TVertexStagePushConstants=record
             ViewBaseIndex:UInt32;
             CountViews:UInt32;
             CountAllViews:UInt32;
             FrameIndex:UInt32;
             Jitter:TpvVector4;
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
               MaterialID:TpvUInt32;                 // + 4 = 64 (unsigned 24-bit material ID)
              );                                     //  ==   ==
              true:(                                 //  80   80 per vertex
               Padding:array[0..63] of TpvUInt8;
              );
            end;
            PVertex=^TVertex;
            TVertices=array of TVertex;
            TGPUDynamicVertex=packed record
             case boolean of
              false:(
               Position:TpvVector3;                  //  12   12 (32-bit float 3D vector)
               MorphTargetVertexBaseIndex:TpvUInt32; // + 4 = 16 (unsigned 32-bit morph target vertex base index)

               JointBlockBaseIndex:TpvUInt32;        // + 4 = 20 (unsigned 32-bit joint block base index)
               CountJointBlocks:TpvUInt32;           // + 4 = 24 (unsigned 16-bit count of joint blocks)
               RootNode:TpvUInt32;                   // + 4 = 44 (unsigned 32-bit root node)
               NodeIndex:TpvUInt32;                  // + 4 = 32 (unsigned 32-bit node index)

               Normal:TInt16Vector2;                 // + 4 = 36 (signed 16-bit oct-encoded normal)
               Tangent:TInt16Vector2;                // + 4 = 40 (signed 16-bit oct-encoded tangent)
               Flags:TpvUInt32;                      // + 4 = 44 (unsigned 32-bit flags)
               Generation:TpvUInt32;                 // + 4 = 48 (unsigned 32-bit generation)

              );                                     //  ==   ==
              true:(                                 //  48   48 per vertex
               Padding:array[0..47] of TpvUInt8;
              );
            end;
            PGPUDynamicVertex=^TGPUDynamicVertex;
            TGPUDynamicVertices=array of TGPUDynamicVertex;
            TGPUStaticVertex=packed record              // Minimum required vertex structure for static data for to be GLTF 2.0 conformant
             case boolean of
              false:(
               TexCoord0:TpvVector2;                 //   8 = 8 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               TexCoord1:TpvVector2;                 // + 8 = 16 (must be full 32-bit float, for 0.0 .. 1.0 out-of-range texcoords)
               Color0:TpvHalfFloatVector4;           // + 8 = 24 (must be at least half-float for HDR)
               MaterialID:TpvUInt32;                 // + 4 = 28 (unsigned 32-bit material ID)
               Unused0:TpvUInt32;                    // + 4 = 32
              );                                     //  ==   ==
              true:(                                 //  32   32 per vertex
               Padding:array[0..31] of TpvUInt8;
              );
            end;
            PGPUStaticVertex=^TGPUStaticVertex;
            TGPUStaticVertices=array of TGPUStaticVertex;
            TGPUCachedVertex=packed record              // Minimum required cached vertex structure for to be GLTF 2.0 conformant
             case boolean of
              false:(
               Position:TpvVector3;                  //  12   12 (32-bit float 3D vector)
               NormalSign:TInt16Vector4;             // + 8 = 20 (signed 16-bit Normal + TBN sign)
               TangentXYZModelScaleX:TInt16Vector4;  // + 8 = 28 (signed 16-bit Tangent + model scale xy)
               ModelScaleYZ:TpvHalfFloatVector2;     // + 4 = 32 (model scale yz)
              );                                     //  ==   ==
              true:(                                 //  32   32 per vertex
               Padding:array[0..31] of TpvUInt8;
              );
            end;
            PGPUCachedVertex=^TGPUCachedVertex;
            TGPUCachedVertices=array of TGPUCachedVertex;
            TGPUCachedVertexGeneration=packed record
             case boolean of
              false:(
               Generation:TpvUInt32;                 //  4 = 4 (unsigned 32-bit geeration)
              );                                     //  =   =
              true:(                                 //  4   4 per vertex
               Padding:array[0..3] of TpvUInt8;
              );
            end;
            PGPUCachedVertexGeneration=^TGPUCachedVertexGeneration;
            TGPUCachedVertexGenerations=array of TGPUCachedVertexGeneration;
            TGPUCachedRaytracingVertex=packed record
             case boolean of
              false:(
               Position:TpvVector3;                  //  12   12 (32-bit float 3D vector)
               Reversed:TpvUInt32;                   // + 4 = 16 (unsigned 32-bit material ID)
              );                                     //  ==   ==
              true:(                                 //  16   16 per vertex
               Padding:array[0..15] of TpvUInt8;
              );
            end;
            PGPUCachedRaytracingVertex=^TGPUCachedRaytracingVertex;
            TGPUCachedRaytracingVertices=array of TGPUCachedRaytracingVertex;
            { TDebugPrimitiveVertex }
            TDebugPrimitiveVertex=packed record
             public
              constructor Create(const aPosition:TpvVector3;const aColor:TpvVector4);
             public
              case boolean of
               false:(
                Position:TpvVector3;                  //  12    0
                Color:TpvHalfFloatVector4;            // + 8 =  8
               );                                     //  ==   ==
               true:(                                 //  24   24 per vertex
                Padding:array[0..23] of TpvUInt8;
               );
            end;
            PDebugPrimitiveVertex=^TDebugPrimitiveVertex;
            TDebugPrimitiveVertices=array of TDebugPrimitiveVertex;
            TDebugPrimitiveVertexDynamicArray=class(TpvDynamicArrayList<TDebugPrimitiveVertex>)
            end;
            TDebugPrimitiveVertexDynamicArrays=array[0..MaxInFlightFrames-1] of TDebugPrimitiveVertexDynamicArray;
            TParticle=record
             LastGeneration:TpvUInt64;
             Generation:TpvUInt64;
             LastPosition:TpvVector3;
             Position:TpvVector3;
             RotationStart:TpvFloat;
             RotationEnd:TpvFloat;
             SizeStart:TpvVector2;
             SizeEnd:TpvVector2;
             ColorStart:TpvVector4;
             ColorEnd:TpvVector4;
             Velocity:TpvVector3;
             Gravity:TpvVector3;
             Age:TpvDouble;
             LifeTime:TpvDouble;
             LastTime:TpvFloat;
             Time:TpvFloat;
             TextureID:TpvUInt32;
            end;
            PParticle=^TParticle;
            TParticles=array[0..MaxParticles-1] of TParticle;
            PParticles=^TParticles;
            TParticleAliveBitmap=array[0..((MaxParticles+31) shr 5)-1] of TpvUInt32;
            PParticleAliveBitmap=^TParticleAliveBitmap;
            TParticleVertex=packed record
             Position:TpvVector3;           //   12
             Rotation:TpvFloat;             //    4
             QuadCoord:TpvHalfFloatVector2; //    4
             TextureID:TpvUInt32;           //    4
             Size:TpvVector2;               //    8
             Color:TpvHalfFloatVector4;     //    8
            end;                            // = 40 bytes per particle vertex
            PParticleVertex=^TParticleVertex;
            TParticleVertices=array[0..(MaxParticles*3)-1] of TParticleVertex;
            PParticleVertices=^TParticleVertices;
            TInFlightFrameParticleVertices=array[0..MaxInFlightFrames-1] of TParticleVertices; // 18MB in total at the moment
            PInFlightFrameParticleVertices=^TInFlightFrameParticleVertices;
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
                                                const aRendererInstance:TObject;
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
              fDataLoaded:TPasMPBool32;
              fInLoadData:TPasMPBool32;
              fUploaded:TPasMPBool32;
              fInUpload:TPasMPBool32;
              fAdded:TPasMPBool32;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure PrepareDeferredFree; override;
              procedure Remove; virtual;
              procedure LoadData; virtual;
              procedure Upload; virtual;
              procedure Unload; virtual;
              procedure IncRef; virtual;
              procedure DecRef; virtual;
              procedure DecRefWithNondeferredFree; virtual;
             public
              property SceneInstance:TpvScene3D read fSceneInstance;
              property ID:TID read fID;
             published
              property Name:TpvUTF8String read fName write fName;
              property DataLoaded:TPasMPBool32 read fDataLoaded;
              property Uploaded:TPasMPBool32 read fUploaded;
              property ReferenceCounter:Int32 read fReferenceCounter;
            end;
            TBaseObjects=TpvObjectGenericList<TBaseObject>;
            { TBakedMesh }
            TBakedMesh=class
             public
              type { TTriangle }
                   TTriangle=record
                    public
                     type TTriangleFlag=
                           (
                            DoubleSided,
                            Opaque,
                            Transparent,
                            Static,
                            Animated
                           );
                          PTriangleFlag=^TTriangleFlag;
                          TTriangleFlags=set of TTriangleFlag;
                          PTriangleFlags=^TTriangleFlags;
                    public
                     Positions:array[0..2] of TpvVector3;
                     Normals:array[0..2] of TpvVector3;
                     Normal:TpvVector3;
                     Flags:TTriangleFlags;
                     MetaFlags:TpvUInt32;
                    public
                     class function Create:TpvScene3D.TBakedMesh.TTriangle; static;
                     procedure Assign(const aFrom:TpvScene3D.TBakedMesh.TTriangle);
                     function RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;var aTime,aU,aV:TpvScalar):boolean;
                   end;
                   PTriangle=^TTriangle;
                   TTriangles=class(TpvDynamicArrayList<TpvScene3D.TBakedMesh.TTriangle>)
                   end;
             private
              fTriangles:TpvScene3D.TBakedMesh.TTriangles;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
              procedure Combine(const aWith:TBakedMesh);
             published
              property Triangles:TTriangles read fTriangles;
            end;
            { TPotentiallyVisibleSet }
            TPotentiallyVisibleSet=class
             public
              type TFileSignature=array[0..3] of AnsiChar;
                   TFileHeader=packed record
                    Signature:TFileSignature;
                    Version:TpvUInt32;
                    BitmapOneDimensionSize:TpvUInt32;
                    BitmapSize:TpvUInt32;
                    BitmapDataSize:TpvUInt32;
                    CountNodes:TpvUInt32;
                   end;
                   PFileHeader=^TFileHeader;
                   TFileNode=packed record
                    AABB:TpvAABB;
                    Left:TpvInt32;
                    Right:TpvInt32;
                    SkipCount:TpvInt32;
                   end;
                   PFileNode=^TFileNode;
                   TFileNodes=array of TFileNode;
              const FileSignature:TpvScene3D.TPotentiallyVisibleSet.TFileSignature='PVS'#0;
                    FileVersion:TpvUInt32=TpvUInt32($00000001);
                    NoNodeIndex=TpvUInt32($ffffffff);
                    CountRayCheckTapPoints=17;
                    RayCheckTapPoints:array[0..CountRayCheckTapPoints-1] of TpvVector3=
                     (

                      (x:0.5;y:0.5;z:0.5),

                      (x:0.015625;y:0.015625;z:0.015625),
                      (x:0.984375;y:0.015625;z:0.015625),
                      (x:0.015625;y:0.984375;z:0.015625),
                      (x:0.984375;y:0.984375;z:0.015625),
                      (x:0.015625;y:0.015625;z:0.984375),
                      (x:0.984375;y:0.015625;z:0.984375),
                      (x:0.015625;y:0.984375;z:0.984375),
                      (x:0.984375;y:0.984375;z:0.984375),

                      (x:0.25;y:0.25;z:0.25),
                      (x:0.75;y:0.25;z:0.25),
                      (x:0.25;y:0.75;z:0.25),
                      (x:0.75;y:0.75;z:0.25),
                      (x:0.25;y:0.25;z:0.75),
                      (x:0.75;y:0.25;z:0.75),
                      (x:0.25;y:0.75;z:0.75),
                      (x:0.75;y:0.75;z:0.75)

                     );
              type TNodeIndexPair=TpvUInt64; // Hi 32-bit second pair index + Lo 32-bit first pair index
                   PNodeIndexPair=^TNodeIndexPair;
                   TNodeIndexPairList=class(TpvDynamicArrayList<TNodeIndexPair>);
                   TNodeIndex=TpvUInt32;
                   PNodeIndex=^TNodeIndex;
                   TNodeIndexList=class(TpvDynamicArrayList<TNodeIndex>);
                   TBitmap=array of TpvUInt32;
                   TSubdivisonMode=
                    (
                     MeshBVH,
                     UniformGrid,
                     ManualZones
                    );
                   { TNode }
                   TNode=class//(TpvPooledObject)
                    private
                     fOwner:TPotentiallyVisibleSet;
                     fParent:TNode;
                     fLevel:TpvUInt32;
                     fAABB:TpvAABB;
                     fLeft:TpvScene3D.TPotentiallyVisibleSet.TNode;
                     fRight:TpvScene3D.TPotentiallyVisibleSet.TNode;
                     fIndex:TpvUInt32;
                     fSkipCount:TpvUInt32;
                     //fTag:TpvPtrInt;
                     fVisibleNodeList:TpvScene3D.TPotentiallyVisibleSet.TNodeIndexList;
                     fMultipleReaderSingleWriterLockState:TPasMPInt32;
                    public
                     constructor Create(const aOwner:TPotentiallyVisibleSet;const aParent:TpvScene3D.TPotentiallyVisibleSet.TNode); reintroduce;
                     destructor Destroy; override;
                     procedure AddVisibleNodeIndex(const aNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex);
                    published
                     property Owner:TPotentiallyVisibleSet read fOwner;
                     property Parent:TNode read fParent;
                     property Level:TpvUInt32 read fLevel;
                     property Left:TNode read fLeft;
                     property Right:TNode read fRight;
                     property VisibleNodeList:TpvScene3D.TPotentiallyVisibleSet.TNodeIndexList read fVisibleNodeList;
                    public
                     property AABB:TpvAABB read fAABB;
                    published
                     property Index:TpvUInt32 read fIndex;
                     property SkipCount:TpvUInt32 read fSkipCount;
                   end;
                   { TNodes }
                   TNodes=class(TpvObjectGenericList<TpvScene3D.TPotentiallyVisibleSet.TNode>)
                    public
                     procedure SortByIndex;
                   end;
                   TViewNodeIndices=array[0..MaxViews-1] of TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
                   TManualBoundingBoxes=class(TpvDynamicArrayList<TpvAABB>);
             private
              fSubdivisonMode:TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode;
              fSubdivisonOneDimensionSize:TpvSizeInt;
              fBakedMesh:TpvScene3D.TBakedMesh;
              fTriangleBVH:TpvTriangleBVH;
              fAABB:TpvAABB;
              fRoot:TpvScene3D.TPotentiallyVisibleSet.TNode;
              fNodes:TpvScene3D.TPotentiallyVisibleSet.TNodes;
              fManualBoundingBoxes:TpvScene3D.TPotentiallyVisibleSet.TManualBoundingBoxes;
              fBitmapOneDimensionSize:TpvUInt32;
              fBitmapSize:TpvUInt32;
              fBitmap:TBitmap;
              fPasMPInstance:TPasMP;
//            fViewNodeIndices:TViewNodeIndices;
              function GetNodeVisibility(const aNodeAIndex,aNodeBIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex):boolean;
              procedure SetNodeVisibility(const aNodeAIndex,aNodeBIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;const aVisibility:boolean);
              function RayCastTriangle(const aUserData:TpvPtrInt;const aRayOrigin,aRayDirection:TpvVector3;out aTime:TpvFloat;out aStop:boolean):boolean;
              procedure NodePairVisibilityCheckRayParallelForJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
              procedure NodePairVisibilityCheckParallelForJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
              procedure Load(const aStream:TStream);
              procedure Save(const aStream:TStream);
              procedure Build(const aBakedMesh:TpvScene3D.TBakedMesh;const aMaxDepth:TpvInt32=8;const aPasMPInstance:TPasMP=nil);
              function GetNodeIndexByPosition(const aPosition:TpvVector3):TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
              function GetNodeIndexByAABB(const aAABB:TpvAABB):TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
             published
              property SubdivisonMode:TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode read fSubdivisonMode write fSubdivisonMode;
              property SubdivisonOneDimensionSize:TpvSizeInt read fSubdivisonOneDimensionSize write fSubdivisonOneDimensionSize;
              property ManualBoundingBoxes:TpvScene3D.TPotentiallyVisibleSet.TManualBoundingBoxes read fManualBoundingBoxes;
             public
              property AABB:TpvAABB read fAABB;
             public
              property Nodes:TpvScene3D.TPotentiallyVisibleSet.TNodes read fNodes;
              property NodeVisibility[const aNodeAIndex,aNodeBIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex]:boolean read GetNodeVisibility write SetNodeVisibility; default;
             published
              property Root:TpvScene3D.TPotentiallyVisibleSet.TNode read fRoot;
            end;
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
                     DefaultParticleTexture=2,
                     ResourceTexture=3
                    );
                   THashData=packed record
                    MessageDigest:TpvHashXXHash64.TMessageDigest;
                    FirstBytes:array[0..127-SizeOf(TpvHashXXHash64.TMessageDigest)] of TpvUInt8;
                   end;
                   PHashData=^THashData;
             private
              fKind:TKind;
              fResourceDataStream:TMemoryStream;
              fHashData:THashData;
              fTexture:TpvVulkanTexture;
              fLock:TPasMPSpinLock;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure LoadData; override;
              procedure Upload; override;
              procedure Unload; override;
              function BeginLoad(const aStream:TStream):boolean; override;
              function EndLoad:boolean; override;
              function GetHashData:THashData;
              procedure AssignFromWhiteTexture;
              procedure AssignFromDefaultNormalMapTexture;
              procedure AssignFromDefaultParticleTexture;
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
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure LoadData; override;
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
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure LoadData; override;
              procedure Upload; override;
              procedure Unload; override;
              function GetHashData:THashData;
              procedure AssignFromWhiteTexture;
              procedure AssignFromDefaultNormalMapTexture;
              procedure AssignFromDefaultParticleTexture;
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
                    ColorFactor:TpvVector3;
                    ColorTexture:TTextureReference;
                    RoughnessFactor:TpvFloat;
                    RoughnessTexture:TTextureReference;
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
                   TAnisotropy=record
                    Active:boolean;
                    AnisotropyStrength:TpvFloat;
                    AnisotropyRotation:TpvFloat;
                    AnisotropyTexture:TTextureReference;
                   end;
                   TShaderData=packed record
                    case boolean of
                     false:(
                      BaseColorFactor:TpvVector4;
                      SpecularFactor:TpvVector4; // actually TpvVector3, but for easier and more convenient alignment reasons a TpvVector4
                      EmissiveFactor:TpvVector4; // w = EmissiveStrength
                      MetallicRoughnessNormalScaleOcclusionStrengthFactor:TpvVector4;
                      SheenColorFactorSheenRoughnessFactor:TpvVector4;
                      ClearcoatFactorClearcoatRoughnessFactor:TpvVector4;
                      IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum:TpvVector4;
                      IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance:TpvVector4;
                      // uvec4 Begin
                       VolumeAttenuationColor:TpvVector3;
                       AnisotropyStrength:TpvUInt16;
                       AnisotropyRotation:TpvUInt16;
                      // uvec4 End
                      // uvec4 AlphaCutOffFlags begin
                       AlphaCutOff:TpvFloat; // for with uintBitsToFloat on GLSL code side
                       Flags:TpvUInt32;
                       Textures0:TPasGLTFUInt32;
                       Textures1:TPasGLTFUInt32;
                       // uvec4 uAlphaCutOffFlags end
                       Textures:array[0..19] of TpvInt32;
                       TextureTransforms:array[0..19] of TAlignedMatrix3x2;
                     );
                     true:(
                      //Padding:array[0..2047] of TpvUInt8;
                     );
                   end;
                   PShaderData=^TShaderData;
                   { TData }
                   TData=record
                    public
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
                     Anisotropy:TAnisotropy;
                     AnimatedTextureMask:TpvUInt64;
                     function GetTextureTransform(const aTextureIndex:TpvScene3D.TTextureIndex):TpvScene3D.TMaterial.TTextureReference.PTransform;
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
                      ColorFactor:(x:0.0;y:0.0;z:0.0);
                      ColorTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                      RoughnessFactor:0.0;
                      RoughnessTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
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
                     Anisotropy:(
                      Active:false;
                      AnisotropyStrength:0.0;
                      AnisotropyRotation:0.0;
                      AnisotropyTexture:(Texture:nil;TexCoord:0;Transform:(Active:false;Offset:(x:0.0;y:0.0);Rotation:0.0;Scale:(x:1.0;y:1.0)));
                     );
                     AnimatedTextureMask:0;
                    );
                   DefaultShaderData:TShaderData=
                    (
                     BaseColorFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SpecularFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     EmissiveFactor:(x:0.0;y:0.0;z:0.0;w:1.0);
                     MetallicRoughnessNormalScaleOcclusionStrengthFactor:(x:1.0;y:1.0;z:1.0;w:1.0);
                     SheenColorFactorSheenRoughnessFactor:(x:0.0;y:0.0;z:0.0;w:0.0);
                     ClearcoatFactorClearcoatRoughnessFactor:(x:0.0;y:0.0;z:1.0;w:1.0);
                     IORIridescenceFactorIridescenceIorIridescenceThicknessMinimum:(x:1.5;y:0.0;z:1.3;w:100.0);
                     IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance:(x:400.0;y:0.0;z:0.0;w:Infinity);
                     VolumeAttenuationColor:(x:1.0;y:1.0;z:1.0);
                     AnisotropyStrength:0;
                     AnisotropyRotation:0;
                     AlphaCutOff:1.0;
                     Flags:0;
                     Textures0:0;
                     Textures1:0;
                     Textures:(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
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
              fGeneration:TpvUInt64;
              fVisible:boolean;
{             fShaderDataUniformBlockBuffer:TpvVulkanBuffer;
              fVulkanDescriptorPool:TpvVulkanDescriptorPool;
              fVulkanDescriptorSet:TpvVulkanDescriptorSet;}
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure LoadData; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure Assign(const aFrom:TMaterial);
              procedure AssignFromEmpty;
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceMaterial:TPasGLTF.TMaterial;const aTextureMap:TTextures);
              procedure FillShaderData;
            end;
            TMaterials=TpvObjectGenericList<TMaterial>;
            TCameraData=record
             public
              type TCameraType=
                    (
                     None=0,
                     Orthographic=1,
                     Perspective=2
                    );
                   TType=TCameraType;
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
              case Type_:TCameraData.TCameraType of
               TCameraType.Orthographic:(
                Orthographic:TOrthographic;
               );
               TCameraType.Perspective:(
                Perspective:TPerspective;
               );
            end;
            PCameraData=^TCameraData;
            TLightData=record
             public
              type TLightType=
                    (
                     None=0,
                     Directional=1,
                     Point=2,
                     Spot=3,
                     PrimaryDirectional=4
                    );
                   TType=TLightType;
             private
              fType_:TLightType;
              fIntensity:TpvFloat;
              fRange:TpvFloat;
              fInnerConeAngle:TpvFloat;
              fOuterConeAngle:TpvFloat;
              fColor:TpvVector3;
              fCastShadows:boolean;
              fVisible:boolean;
              fAlways:boolean;
             public
              property Type_:TLightType read fType_ write fType_;
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
            TLightMetaInfo=packed record
             MinBounds:TpvVector4;
             MaxBounds:TpvVector4;
            end;
            PLightMetaInfo=^TLightMetaInfo;
            TLightMetaInfos=array[0..MaxVisibleLights-1] of TLightMetaInfo;
            PTLightMetaInfos=^TLightMetaInfos;
            { TLightBuffer }
            TLightBuffer=class
             private
              fSceneInstance:TpvScene3D;
              fInFlightFrameIndex:TpvSizeInt;
              fUploaded:TPasMPBool32;
              fLightItems:TLightItems;
              fLightAABBTreeGeneration:TpvUInt64;
              fNewLightAABBTreeGeneration:TpvUInt64;
              fLightTree:TpvBVHDynamicAABBTree.TSkipListNodeArray;
              fLightMetaInfos:TLightMetaInfos;
              fLightItemsVulkanBuffer:TpvVulkanBuffer;
              fLightTreeVulkanBuffer:TpvVulkanBuffer;
              fLightMetaInfoVulkanBuffer:TpvVulkanBuffer;
             public
              constructor Create(const aSceneInstance:TpvScene3D;const aInFlightFrameIndex:TpvSizeInt); reintroduce;
              destructor Destroy; override;
              procedure Upload;
              procedure Unload;
              procedure PrepareFrame;
              procedure UploadFrame;
             public
              property LightItems:TLightItems read fLightItems;
              property LightMetaInfoVulkanBuffer:TpvVulkanBuffer read fLightMetaInfoVulkanBuffer;
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
              property Type_:TpvScene3D.TLightData.TLightType read fData.fType_ write fData.fType_;
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
            TVertexDynamicArray=TpvDynamicArray<TVertex>;
            TGPUDynamicVertexDynamicArray=TpvDynamicArray<TGPUDynamicVertex>;
            TGPUStaticVertexDynamicArray=TpvDynamicArray<TGPUStaticVertex>;
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
            TIndicesDynamicArray=TpvDynamicArray<TVkUInt32>;
            TJointBlocksDynamicArray=TpvDynamicArray<TJointBlock>;
            TMatricesDynamicArray=TpvDynamicArray<TpvMatrix4x4>;
            TFloatsDynamicArray=TpvDynamicArray<TpvFloat>;
            TVkMultiDrawIndexedInfoEXTDynamicArray=TpvDynamicArray<TVkMultiDrawIndexedInfoEXT>;
            { TDrawChoreographyBatchItem }
            TDrawChoreographyBatchItem=class
             private
              fGroup:TpvScene3D.TGroup;
              fGroupInstance:TObject;
              fAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
              fPrimitiveTopology:TpvScene3D.TPrimitiveTopology;
              fDoubleSided:boolean;
              fMaterial:TpvScene3D.TMaterial;
              fNode:TObject;
              fObjectIndex:TpvUInt32;
              fMesh:TObject;
              fMeshPrimitive:TpvSizeInt;
              fStartIndex:TpvSizeInt;
              fCountIndices:TpvSizeInt;
             public
              constructor Create; reintroduce;
              function Clone:TDrawChoreographyBatchItem;
              class function CompareTo(const aCurrent,aOther:TpvScene3D.TDrawChoreographyBatchItem):TpvInt32; static;
              class function IndexOrderCompareTo(const aCurrent,aOther:TpvScene3D.TDrawChoreographyBatchItem):TpvInt32; static;
             published
              property Group:TpvScene3D.TGroup read fGroup write fGroup;
              property GroupInstance:TObject read fGroupInstance write fGroupInstance;
              property AlphaMode:TpvScene3D.TMaterial.TAlphaMode read fAlphaMode write fAlphaMode;
              property PrimitiveTopology:TpvScene3D.TPrimitiveTopology read fPrimitiveTopology write fPrimitiveTopology;
              property DoubleSided:boolean read fDoubleSided write fDoubleSided;
              property Material:TpvScene3D.TMaterial read fMaterial write fMaterial;
              property Node:TObject read fNode write fNode;
              property ObjectIndex:TpvUInt32 read fObjectIndex write fObjectIndex;
              property Mesh:TObject read fMesh write fMesh;
              property MeshPrimitive:TpvSizeInt read fMeshPrimitive write fMeshPrimitive;
              property StartIndex:TpvSizeInt read fStartIndex write fStartIndex;
              property CountIndices:TpvSizeInt read fCountIndices write fCountIndices;
            end;
            TDrawChoreographyBatchItemArray=array of TDrawChoreographyBatchItem;
            { TDrawChoreographyBatchItems }
            TDrawChoreographyBatchItems=class(TpvObjectGenericList<TDrawChoreographyBatchItem>)
             public
              procedure GroupInstanceClone(const aFrom:TDrawChoreographyBatchItems;const aGroupInstance:TObject;const aIsUnique:Boolean);
              procedure Sort;
              procedure IndexOrderSort;
            end;
            TDrawChoreographyBatchItemBuckets=array[TPrimitiveTopology,TFaceCullingMode] of TDrawChoreographyBatchItems;
            PDrawChoreographyBatchItemBuckets=^TDrawChoreographyBatchItemBuckets;
            TDrawChoreographyBatchItemMaterialAlphaModeBuckets=array[TpvScene3D.TMaterial.TAlphaMode] of TDrawChoreographyBatchItemBuckets;
            PDrawChoreographyBatchItemMaterialAlphaModeBuckets=^TDrawChoreographyBatchItemMaterialAlphaModeBuckets;
            TDrawChoreographyBatchItemRenderPassBuckets=array[0..MaxRenderPassIndices-1] of TDrawChoreographyBatchItemMaterialAlphaModeBuckets;
            PDrawChoreographyBatchItemRenderPassBuckets=^TDrawChoreographyBatchItemRenderPassBuckets;
            TDrawChoreographyBatchItemFrameBuckets=array[0..MaxInFlightFrames-1] of TDrawChoreographyBatchItemRenderPassBuckets;
            PDrawChoreographyBatchItemFrameBuckets=^TDrawChoreographyBatchItemFrameBuckets;
            TGPUDrawIndexedIndirectCommand=packed record
             public
              case TpvUInt8 of
               0:(
                DrawIndexedIndirectCommand:TVkDrawIndexedIndirectCommand;
                ObjectIndex:TpvUInt32;
                Padding0:array[1..2] of TpvUInt32;
                BoundingSphere:TpvVector4;
                Padding1:array[0..3] of TpvUInt32;
               );
               1:(
                Alignment:array[0..63] of TpvUInt8;
               );
            end;
            PGPUDrawIndexedIndirectCommand=^TGPUDrawIndexedIndirectCommand;
            TGPUDrawIndexedIndirectCommands=array of TGPUDrawIndexedIndirectCommand;
            TGPUDrawIndexedIndirectCommandDynamicArray=TpvDynamicArray<TGPUDrawIndexedIndirectCommand>;
            PGPUDrawIndexedIndirectCommandDynamicArray=^TGPUDrawIndexedIndirectCommandDynamicArray;
            TPerInFlightFrameGPUDrawIndexedIndirectCommandDynamicArrays=array[0..MaxInFlightFrames-1] of TGPUDrawIndexedIndirectCommandDynamicArray;
            PPerInFlightFrameGPUDrawIndexedIndirectCommandDynamicArrays=^TPerInFlightFrameGPUDrawIndexedIndirectCommandDynamicArrays;
            TPerInFlightFrameGPUDrawIndexedIndirectCommandSizeValues=array[0..MaxInFlightFrames-1] of TpvSizeInt;
            TPerInFlightFrameGPUDrawIndexedIndirectCommandBuffers=array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
            TPerInFlightFrameGPUCulledArray=array[0..MaxInFlightFrames-1,0..MaxRenderPassIndices-1] of Boolean;
            PPerInFlightFrameGPUCulledArray=^TPerInFlightFrameGPUCulledArray;
            TPerInFlightFrameGPUCountObjectIndicesArray=array[0..MaxInFlightFrames-1] of TpvSizeInt;
            PPerInFlightFrameGPUCountObjectIndicesArray=^TPerInFlightFrameGPUCountObjectIndicesArray;
            TDrawChoreographyBatchRange=record
             AlphaMode:TpvScene3D.TMaterial.TAlphaMode;
             PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
             FaceCullingMode:TpvScene3D.TFaceCullingMode;
             DrawCallIndex:TpvUInt32;
             FirstCommand:TpvUInt32;
             CountCommands:TpvUInt32;
            end;
            PDrawChoreographyBatchRange=^TDrawChoreographyBatchRange;
            TDrawChoreographyBatchRangeDynamicArray=TpvDynamicArray<TDrawChoreographyBatchRange>;
            PDrawChoreographyBatchRangeDynamicArray=^TDrawChoreographyBatchRangeDynamicArray;
            TDrawChoreographyBatchRangeRenderPassBuckets=array[0..MaxRenderPassIndices-1] of TDrawChoreographyBatchRangeDynamicArray;
            PDrawChoreographyBatchRangeRenderPassBuckets=^TDrawChoreographyBatchRangeRenderPassBuckets;
            TDrawChoreographyBatchRangeFrameBuckets=array[0..MaxInFlightFrames-1] of TDrawChoreographyBatchRangeRenderPassBuckets;
            PDrawChoreographyBatchRangeFrameBuckets=^TDrawChoreographyBatchRangeFrameBuckets;
            { TVulkanLongTermStaticBufferData }
            TVulkanLongTermStaticBufferData=class
             private
              fSceneInstance:TpvScene3D;
              fVulkanDynamicVertexBuffer:TpvVulkanBuffer;
              fVulkanStaticVertexBuffer:TpvVulkanBuffer;
              fVulkanDrawIndexBuffer:TpvVulkanBuffer;
              fVulkanDrawUniqueIndexBuffer:TpvVulkanBuffer;
              fVulkanMorphTargetVertexBuffer:TpvVulkanBuffer;
              fVulkanJointBlockBuffer:TpvVulkanBuffer;
              fVulkanComputeDescriptorPool:TpvVulkanDescriptorPool;
              fVulkanComputeDescriptorSet:TpvVulkanDescriptorSet;
              fReleaseFrameCounter:TpvSizeInt;
             public
              constructor Create(const aSceneInstance:TpvScene3D); reintroduce;
              destructor Destroy; override;
              function Check:Boolean;
              procedure Update;
              procedure UpdateReleaseFrameCounter;
            end;
            TVulkanLongTermStaticBufferDataArray=array[0..MaxInFlightFrames-1] of TVulkanLongTermStaticBufferData;
            { TVulkanLongTermStaticBuffers }
            TVulkanLongTermStaticBuffers=class
             private
              fSceneInstance:TpvScene3D;
              fBufferDataArray:TVulkanLongTermStaticBufferDataArray;
              fBufferData:TVulkanLongTermStaticBufferData;
              fCurrentIndex:TpvSizeInt;
             public
              constructor Create(const aSceneInstance:TpvScene3D); reintroduce;
              destructor Destroy; override;
              procedure Update;
             published
              property BufferData:TVulkanLongTermStaticBufferData read fBufferData;
            end;
            { TVulkanShortTermDynamicBufferData } 
            TVulkanShortTermDynamicBufferData=class
             private
              fSceneInstance:TpvScene3D;
              fInFlightFrameIndex:TpvSizeInt;
              fVulkanCachedVertexBuffer:TpvVulkanBuffer;
              fVulkanCachedVertexGenerationBuffer:TpvVulkanBuffer;
              fVulkanCachedRaytracingVertexBuffer:TpvVulkanBuffer;
              fVulkanNodeMatricesBuffer:TpvVulkanBuffer;
              fVulkanMorphTargetVertexWeightsBuffer:TpvVulkanBuffer;
              fVulkanComputeDescriptorPool:TpvVulkanDescriptorPool;
              fVulkanComputeDescriptorSet:TpvVulkanDescriptorSet;
             public
              constructor Create(const aSceneInstance:TpvScene3D;const aInFlightFrameIndex:TpvSizeInt); reintroduce;
              destructor Destroy; override;
              procedure Update;
            end;
            TVulkanShortTermDynamicBufferDataArray=array[0..MaxInFlightFrames-1] of TVulkanShortTermDynamicBufferData;
            { TVulkanShortTermDynamicBuffers }
            TVulkanShortTermDynamicBuffers=class
             private
              fSceneInstance:TpvScene3D;
              fBufferDataArray:TVulkanShortTermDynamicBufferDataArray;
              fBufferData:TVulkanShortTermDynamicBufferData;
              fCurrentIndex:TpvSizeInt;
             public
              constructor Create(const aSceneInstance:TpvScene3D); reintroduce;
              destructor Destroy; override;
              procedure Update(const aInFlightFrameIndex:TpvSizeInt);
             public
              property BufferDataArray:TVulkanShortTermDynamicBufferDataArray read fBufferDataArray;
             published
              property BufferData:TVulkanShortTermDynamicBufferData read fBufferData;
            end;
            { TGroup }
            TGroup=class(TBaseObject) // A group is a GLTF scene in a uber-scene
             public
              type TNode=class;
                   TMesh=class;
                   TScene=class;
                   TGroupVertices=TpvDynamicArray<TVertex>;
                   TGroupIndices=TpvDynamicArray<TVkUInt32>;
                   TGroupJointBlocks=TpvDynamicArray<TJointBlock>;
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
                   TMaterialMap=array of TpvUInt32;
                   TMaterialIDMapArrayIndexHashMap=TpvHashMap<TID,TpvSizeInt>;
                   TMaterialNameMapArrayIndexHashMap=TpvStringHashMap<TpvSizeInt>;
                   TMaterialIDMapArray=TpvGenericList<TpvSizeInt>;
                   TMaterialIDMapArrays=TpvObjectGenericList<TMaterialIDMapArray>;
                   TGroupObject=class
                    private
                     fName:TpvUTF8String;
                     fGroup:TGroup;
                    public
                     constructor Create(const aGroup:TGroup); reintroduce; virtual;
                     destructor Destroy; override;
                    published
                     property Group:TGroup read fGroup write fGroup;
                     property Name:TpvUTF8String read fName write fName;
                   end;
                   { TAnimation }
                   TAnimation=class(TGroupObject)
                    public
                     type { TChannel }
                          TChannel=record
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
                                   PointerPunctualLightColor,
                                   PointerPunctualLightIntensity,
                                   PointerPunctualLightRange,
                                   PointerPunctualLightSpotInnerConeAngle,
                                   PointerPunctualLightSpotOuterConeAngle,
                                   PointerMaterialPBRMetallicRoughnessBaseColorFactor,
                                   PointerMaterialPBRMetallicRoughnessMetallicFactor,
                                   PointerMaterialPBRMetallicRoughnessRoughnessFactor,
                                   PointerMaterialAlphaCutOff,
                                   PointerMaterialEmissiveFactor,
                                   PointerMaterialNormalTextureScale,
                                   PointerMaterialOcclusionTextureStrength,
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
                                   PointerMaterialPBRAnisotropyStrength,
                                   PointerMaterialPBRAnisotropyRotation,
                                   PointerTextureOffset,
                                   PointerTextureRotation,
                                   PointerTextureScale
                                  );
                                 TTargetSet=set of TTarget;
                                 TInterpolation=
                                  (
                                   Linear,
                                   Step,
                                   CubicSpline
                                  );
                            const MaterialTargets:TTargetSet=
                                   [
                                    TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor,
                                    TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor,
                                    TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor,
                                    TTarget.PointerMaterialAlphaCutOff,
                                    TTarget.PointerMaterialEmissiveFactor,
                                    TTarget.PointerMaterialNormalTextureScale,
                                    TTarget.PointerMaterialOcclusionTextureStrength,
                                    TTarget.PointerMaterialPBRClearCoatFactor,
                                    TTarget.PointerMaterialPBRClearCoatRoughnessFactor,
                                    TTarget.PointerMaterialEmissiveStrength,
                                    TTarget.PointerMaterialIOR,
                                    TTarget.PointerMaterialPBRIridescenceFactor,
                                    TTarget.PointerMaterialPBRIridescenceIor,
                                    TTarget.PointerMaterialPBRIridescenceMinimum,
                                    TTarget.PointerMaterialPBRIridescenceMaximum,
                                    TTarget.PointerMaterialPBRSheenColorFactor,
                                    TTarget.PointerMaterialPBRSheenRoughnessFactor,
                                    TTarget.PointerMaterialPBRSpecularFactor,
                                    TTarget.PointerMaterialPBRSpecularColorFactor,
                                    TTarget.PointerMaterialPBRTransmissionFactor,
                                    TTarget.PointerMaterialPBRVolumeThicknessFactor,
                                    TTarget.PointerMaterialPBRVolumeAttenuationDistance,
                                    TTarget.PointerMaterialPBRVolumeAttenuationColor,
                                    TTarget.PointerMaterialPBRAnisotropyStrength,
                                    TTarget.PointerMaterialPBRAnisotropyRotation,
                                    TTarget.PointerTextureOffset,
                                    TTarget.PointerTextureRotation,
                                    TTarget.PointerTextureScale
                                   ];
                                  TextureTargets:TTargetSet=
                                   [
                                    TTarget.PointerTextureOffset,
                                    TTarget.PointerTextureRotation,
                                    TTarget.PointerTextureScale
                                   ];
                           public
                            Name:TpvUTF8String;
                            Target:TTarget;
                            TargetPointer:TpvUTF8String;
                            TargetIndex:TpvSizeInt;
                            TargetSubIndex:TpvSizeInt;
                            TargetInstanceIndex:TpvSizeInt;
                            Interpolation:TInterpolation;
                            InputTimeArray:TpvDoubleDynamicArray;
                            OutputScalarArray:TpvFloatDynamicArray;
                            OutputVector2Array:TpvVector2Array;
                            OutputVector3Array:TpvVector3Array;
                            OutputVector4Array:TpvVector4Array;
                            procedure SetTarget(const aTargetPath:TpvUTF8String;const aTargetNode:TpvSizeInt);
                            procedure SetInterpolation(const aInterpolation:TpvUTF8String);
                          end;
                          PChannel=^TChannel;
                          TChannels=array of TChannel;
                          TDefaultChannel=record
                           public
                            Target:TpvScene3D.TGroup.TAnimation.TChannel.TTarget;
                            TargetIndex:TpvSizeInt;
                            TargetSubIndex:TpvSizeInt;
                            TargetInstanceIndex:TpvSizeInt;
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
                            PrimitiveTopology:TPrimitiveTopology;
                            MaterialID:TpvInt64;
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
                     fBoundingSphere:TpvSphere;
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
                     type TNodeFlag=
                           (
                            TransformAnimated,
                            SkinAnimated,
                            WeightsAnimated
                           );
                          PNodeFlag=^TNodeFlag;
                          TNodeFlags=set of TNodeFlag;
                          PNodeFlags=^TNodeFlags;
                          TChildNodeIndices=TpvDynamicArray<TpvSizeInt>;
                          TUsedByScenesList=TpvObjectGenericList<TpvScene3D.TGroup.TScene>;
                          TUsedJoint=record
                           Joint:TpvSizeInt;
                           Weight:TpvScalar;
                           AABB:TpvAABB;
                          end;
                          PUsedJoint=^TUsedJoint;
                          TUsedJoints=TpvDynamicArray<TpvScene3D.TGroup.TNode.TUsedJoint>;
                          PUsedJoints=^TUsedJoints;
                    private
                     fIndex:TpvSizeInt;
                     fFlags:TNodeFlags;
                     fUsedByScenesList:TUsedByScenesList;
                     fChildNodeIndices:TChildNodeIndices;
                     fChildren:TNodes;
                     fSplittedChildren:TNodes;
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
                     fDrawChoreographyBatchItemIndices:TSizeIntDynamicArray;
                     fDrawChoreographyBatchUniqueItemIndices:TSizeIntDynamicArray;
                     fUsedJoints:TpvScene3D.TGroup.TNode.TUsedJoints;
                     procedure Finish;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceNode:TPasGLTF.TNode;const aLightMap:TpvScene3D.TGroup.TLights);
                    published
                     property Index:TpvSizeInt read fIndex;
                     property Flags:TNodeFlags read fFlags write fFlags;
                     property Children:TNodes read fChildren;
                     property Camera:TCamera read fCamera;
                     property Mesh:TMesh read fMesh;
                     property NodeMeshInstanceIndex:TPasGLTFSizeInt read fNodeMeshInstanceIndex;
                     property Skin:TSkin read fSkin;
                   end;
                   { TUsedVisibleDrawNodes }
                   TUsedVisibleDrawNodes=TpvObjectGenericList<TpvScene3D.TGroup.TNode>;
                   { TPrimitiveIndexRange }
                   TPrimitiveIndexRange=record
                    Index:TpvSizeInt;
                    Count:TpvSizeInt;
                   end;
                   { TScene }
                   TScene=class(TGroupObject)
                    public
                     type TSkipListItem=record
                           public
                            NodeIndex:TpvSizeInt;
                            Level:TpvSizeInt;
                            SkipCount:TpvSizeInt;
                          end;
                          PSkipListItem=^TSkipListItem;
                          TSkipList=array of TSkipListItem;
                    private
                     fIndex:TpvSizeInt;
                     fNodes:TpvScene3D.TGroup.TNodes;
                     fAllNodes:TpvScene3D.TGroup.TNodes;
                     fTransformAnimatedNodes:TpvScene3D.TGroup.TNodes;
                     fSkinOrWeightsAnimatedNodes:TpvScene3D.TGroup.TNodes;
                     fStaticNodes:TpvScene3D.TGroup.TNodes;
                     fDrawChoreographyBatchItems:TDrawChoreographyBatchItems;
                     fDrawChoreographyBatchUniqueItems:TDrawChoreographyBatchItems;
                     fSkipList:TSkipList;
                     procedure ConstructSkipList;
                    public
                     constructor Create(const aGroup:TGroup;const aIndex:TpvSizeInt); reintroduce;
                     destructor Destroy; override;
                     procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceScene:TPasGLTF.TScene);
                    published
                     property Index:TpvSizeInt read fIndex;
                     property Nodes:TpvScene3D.TGroup.TNodes read fNodes;
                     property AllNodes:TpvScene3D.TGroup.TNodes read fAllNodes;
                     property TransformAnimatedNodes:TpvScene3D.TGroup.TNodes read fTransformAnimatedNodes;
                     property SkinOrWeightsAnimatedNodes:TpvScene3D.TGroup.TNodes read fSkinOrWeightsAnimatedNodes;
                     property StaticNodes:TpvScene3D.TGroup.TNodes read fStaticNodes;
                   end;
                   TScenes=TpvObjectGenericList<TScene>;
                   { TInstance }
                   TInstance=class(TBaseObject)
                    public
                     type { TAnimation }
                          TAnimation=class
                           public
                            type TChannelOverwrite=TpvSizeInt;
                                 PChannelOverwrite=^TChannelOverwrite;
                                 TChannelOverwrites=array of TChannelOverwrite;
                                 TLastIndices=array of TpvSizeInt;
                           private
                            fFactor:TpvFloat;
                            fTime:TpvDouble;
                            fLastIndices:TLastIndices;
                            fShadowTime:TpvDouble;
                            fComplete:LongBool;
                            fChannelOverwrites:TChannelOverwrites;
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
                            type TNodeOverwriteFlag=
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
                                 TNodeOverwriteFlags=set of TNodeOverwriteFlag;
                                 TNodeOverwrite=record
                                  public
                                   Flags:TNodeOverwriteFlags;
                                   Translation:TpvVector3;
                                   Rotation:TpvQuaternion;
                                   Scale:TpvVector3;
                                   Weights:TpvFloatDynamicArray;
                                   Factor:TpvFloat;
                                 end;
                                 PNodeOverwrite=^TNodeOverwrite;
                                 TNodeOverwrites=array of TNodeOverwrite;
                                 TInstanceNodeFlag=
                                  (
                                   InverseFrontFaces
                                  );
                                 PInstanceNodeFlag=^TInstanceNodeFlag;
                                 TInstanceNodeFlags=set of TInstanceNodeFlag;
                           public
                            Processed:LongBool;
                            Flags:TInstanceNodeFlags;
                            Overwrites:TNodeOverwrites;
                            CountOverwrites:TpvSizeInt;
                            OverwriteWeightsSum:TpvDoubleDynamicArray;
                            WorkWeights:TpvFloatDynamicArray;
                            WorkMatrix:TpvMatrix4x4;
                            Light:TpvScene3D.TLight;
                            WorkMatrices:array[0..MaxInFlightFrames-1] of TpvMatrix4x4;
                            BoundingBoxes:array[0..MaxInFlightFrames-1] of TpvAABB;
                            BoundingBoxFilled:array[0..MaxInFlightFrames-1] of boolean;
                            PotentiallyVisibleSetNodeIndices:array[0..MaxInFlightFrames-1] of TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
                            CacheVerticesGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
                            CacheVerticesGeneration:TpvUInt64;
                            CacheVerticesDirtyCounter:TpvUInt32;
                            AABBTreeProxy:TpvSizeInt;
                            Parents:array[0..MaxInFlightFrames-1] of TpvSizeInt;
                            CullVisibleIDs:array[0..MaxInFlightFrames-1] of TpvSizeInt;
                            CullObjectID:TpvUInt32;
                           public
                            function InverseFrontFaces:boolean; inline;
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
                          TOnNodeMatrix=function(const aInstance:TInstance;aNode,InstanceNode:pointer;var Matrix:TpvMatrix4x4):Boolean of object;
                          TNodeMatrices=array of TpvMatrix4x4;
                          TMorphTargetVertexWeights=array of TpvFloat;
                          { TLight }
                          TLight=class
                           public
                            type TLightOverwriteFlag=
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
                                 TLightOverwriteFlags=set of TLightOverwriteFlag;
                                 TLightOverwrite=record
                                  public
                                   Flags:TLightOverwriteFlags;
                                   Factor:TpvFloat;
                                   Color:TpvVector3;
                                   Intensity:TpvFloat;
                                   Range:TpvFloat;
                                   SpotInnerConeAngle:TpvFloat;
                                   SpotOuterConeAngle:TpvFloat;
                                 end;
                                 PLightOverwrite=^TLightOverwrite;
                                 TLightOverwrites=array of TLightOverwrite;
                           private
                            fInstance:TInstance;
                            fLight:TpvScene3D.TGroup.TLight;
                            fData:TpvScene3D.TLightData;
                            fWorkData:TpvScene3D.TLightData;
                            fEffectiveData:TpvScene3D.PLightData;
                            fOverwrites:TLightOverwrites;
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
                            type TCameraOverwriteFlag=
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
                                 TCameraOverwriteFlags=set of TCameraOverwriteFlag;
                                 TCameraOverwrite=record
                                  public
                                   Flags:TCameraOverwriteFlags;
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
                                 PCameraOverwrite=^TCameraOverwrite;
                                 TCameraOverwrites=array of TCameraOverwrite;
                           private
                            fInstance:TInstance;
                            fCamera:TpvScene3D.TGroup.TCamera;
                            fData:TpvScene3D.TCameraData;
                            fWorkData:TpvScene3D.TCameraData;
                            fEffectiveData:TpvScene3D.PCameraData;
                            fOverwrites:TCameraOverwrites;
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
                          { TMaterial }
                          TMaterial=class
                           public
                            type TMaterialOverwriteFlag=
                                  (
                                   Defaults,
                                   DefaultMaterialPBRMetallicRoughnessBaseColorFactor,
                                   DefaultMaterialPBRMetallicRoughnessMetallicFactor,
                                   DefaultMaterialPBRMetallicRoughnessRoughnessFactor,
                                   DefaultMaterialAlphaCutOff,
                                   DefaultMaterialEmissiveFactor,
                                   DefaultMaterialNormalTextureScale,
                                   DefaultMaterialOcclusionTextureStrength,
                                   DefaultMaterialPBRClearCoatFactor,
                                   DefaultMaterialPBRClearCoatRoughnessFactor,
                                   DefaultMaterialEmissiveStrength,
                                   DefaultMaterialIOR,
                                   DefaultMaterialPBRIridescenceFactor,
                                   DefaultMaterialPBRIridescenceIor,
                                   DefaultMaterialPBRIridescenceMinimum,
                                   DefaultMaterialPBRIridescenceMaximum,
                                   DefaultMaterialPBRSheenColorFactor,
                                   DefaultMaterialPBRSheenRoughnessFactor,
                                   DefaultMaterialPBRSpecularFactor,
                                   DefaultMaterialPBRSpecularColorFactor,
                                   DefaultMaterialPBRTransmissionFactor,
                                   DefaultMaterialPBRVolumeThicknessFactor,
                                   DefaultMaterialPBRVolumeAttenuationDistance,
                                   DefaultMaterialPBRVolumeAttenuationColor,
                                   DefaultMaterialPBRAnisotropyStrength,
                                   DefaultMaterialPBRAnisotropyRotation,
                                   DefaultTextureOffset,
                                   DefaultTextureRotation,
                                   DefaultTextureScale,
                                   MaterialPBRMetallicRoughnessBaseColorFactor,
                                   MaterialPBRMetallicRoughnessMetallicFactor,
                                   MaterialPBRMetallicRoughnessRoughnessFactor,
                                   MaterialAlphaCutOff,
                                   MaterialEmissiveFactor,
                                   MaterialNormalTextureScale,
                                   MaterialOcclusionTextureStrength,
                                   MaterialPBRClearCoatFactor,
                                   MaterialPBRClearCoatRoughnessFactor,
                                   MaterialEmissiveStrength,
                                   MaterialIOR,
                                   MaterialPBRIridescenceFactor,
                                   MaterialPBRIridescenceIor,
                                   MaterialPBRIridescenceMinimum,
                                   MaterialPBRIridescenceMaximum,
                                   MaterialPBRSheenColorFactor,
                                   MaterialPBRSheenRoughnessFactor,
                                   MaterialPBRSpecularFactor,
                                   MaterialPBRSpecularColorFactor,
                                   MaterialPBRTransmissionFactor,
                                   MaterialPBRVolumeThicknessFactor,
                                   MaterialPBRVolumeAttenuationDistance,
                                   MaterialPBRVolumeAttenuationColor,
                                   MaterialPBRAnisotropyStrength,
                                   MaterialPBRAnisotropyRotation,
                                   TextureOffset,
                                   TextureRotation,
                                   TextureScale
                                  );
                                 TMaterialOverwriteFlags=set of TMaterialOverwriteFlag;
                                 TMaterialOverwrite=record
                                  public
                                   Flags:TMaterialOverwriteFlags;
                                   Factor:TpvFloat;
                                   case SubIndex:TpvSizeInt of
                                    -1:(
                                     MaterialPBRMetallicRoughnessBaseColorFactor:TpvVector4;
                                     MaterialPBRMetallicRoughnessMetallicFactor:TpvFloat;
                                     MaterialPBRMetallicRoughnessRoughnessFactor:TpvFloat;
                                     MaterialAlphaCutOff:TpvFloat;
                                     MaterialEmissiveFactor:TpvVector3;
                                     MaterialNormalTextureScale:TpvFloat;
                                     MaterialOcclusionTextureStrength:TpvFloat;
                                     MaterialPBRClearCoatFactor:TpvFloat;
                                     MaterialPBRClearCoatRoughnessFactor:TpvFloat;
                                     MaterialEmissiveStrength:TpvFloat;
                                     MaterialIOR:TpvFloat;
                                     MaterialPBRIridescenceFactor:TpvFloat;
                                     MaterialPBRIridescenceIor:TpvFloat;
                                     MaterialPBRIridescenceMinimum:TpvFloat;
                                     MaterialPBRIridescenceMaximum:TpvFloat;
                                     MaterialPBRSheenColorFactor:TpvVector3;
                                     MaterialPBRSheenRoughnessFactor:TpvFloat;
                                     MaterialPBRSpecularFactor:TpvFloat;
                                     MaterialPBRSpecularColorFactor:TpvVector3;
                                     MaterialPBRTransmissionFactor:TpvFloat;
                                     MaterialPBRVolumeThicknessFactor:TpvFloat;
                                     MaterialPBRVolumeAttenuationDistance:TpvFloat;
                                     MaterialPBRVolumeAttenuationColor:TpvVector3;
                                     MaterialPBRAnisotropyStrength:TpvFloat;
                                     MaterialPBRAnisotropyRotation:TpvFloat;
                                    );
                                    0:(
                                     TextureOffset:TpvVector2;
                                     TextureRotation:TpvFloat;
                                     TextureScale:TpvVector2;
                                    );
                                 end;
                                 PMaterialOverwrite=^TMaterialOverwrite;
                                 TMaterialOverwrites=array of TMaterialOverwrite;
                           private
                            fInstance:TInstance;
                            fMaterial:TpvScene3D.TMaterial;
                            fData:TpvScene3D.TMaterial.TData;
                            fWorkData:TpvScene3D.TMaterial.TData;
                            fEffectiveData:TpvScene3D.TMaterial.PData;
                            fOverwrites:TMaterialOverwrites;
                            fCountOverwrites:TpvSizeInt;
                            fTextureOffsetSums:array[TpvScene3D.TTextureIndex] of TpvScene3D.TVector2Sum;
                            fTextureRotationSums:array[TpvScene3D.TTextureIndex] of TpvScene3D.TScalarSum;
                            fTextureScaleSums:array[TpvScene3D.TTextureIndex] of TpvScene3D.TVector2Sum;
                           public
                            constructor Create(const aInstance:TpvScene3D.TGroup.TInstance;const aMaterial:TpvScene3D.TMaterial);
                            destructor Destroy; override;
                            procedure Update;
                           public
                            property Material:TpvScene3D.TMaterial read fMaterial;
                            property Data:TpvScene3D.TMaterial.TData read fData write fData;
                            property WorkData:TpvScene3D.TMaterial.TData read fWorkData write fWorkData;
                            property EffectiveData:TpvScene3D.TMaterial.PData read fEffectiveData;
                          end;
                          TMaterials=TpvObjectGenericList<TpvScene3D.TGroup.TInstance.TMaterial>;
                          { TScene }
                          TScene=class
                           private
                            fInstance:TInstance;
                            fScene:TpvScene3D.TGroup.TScene;
                            fDrawChoreographyBatchItems:TDrawChoreographyBatchItems;
                            fDrawChoreographyBatchUniqueItems:TDrawChoreographyBatchItems;
                           public
                            constructor Create(const aInstance:TpvScene3D.TGroup.TInstance;const aScene:TpvScene3D.TGroup.TScene);
                            destructor Destroy; override;
                          end;
                          TScenes=TpvObjectGenericList<TpvScene3D.TGroup.TInstance.TScene>;
                          { TRenderInstance }
                          TRenderInstance=class
                           private
                            fInstance:TpvScene3D.TGroup.TInstance;
                            fActive:Boolean;
                            fFirst:Boolean;
                            fIndex:TpvSizeInt;
                            fPotentiallyVisibleSetNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
                            fModelMatrix:TpvMatrix4x4;
                            fPreviousModelMatrix:TpvMatrix4x4;
                            fBoundingBox:TpvAABB;
                           public
                            constructor Create(const aInstance:TpvScene3D.TGroup.TInstance); reintroduce;
                            destructor Destroy; override;
                            procedure AfterConstruction; override;
                            procedure BeforeDestruction; override;
                            procedure Remove;
                           public
                            property ModelMatrix:TpvMatrix4x4 read fModelMatrix write fModelMatrix;
                           published
                            property Active:Boolean read fActive write fActive;
                          end;
                          TRenderInstances=TpvObjectGenericList<TRenderInstance>;
                          TPerInFlightFrameRenderInstance=record
                           PotentiallyVisibleSetNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
                           BoundingBox:TpvAABB;
                           ModelMatrix:TpvMatrix4x4;
                           PreviousModelMatrix:TpvMatrix4x4;
                          end;
                          PPerInFlightFrameRenderInstance=^TPerInFlightFrameRenderInstance;
                          TPerInFlightFrameRenderInstanceDynamicArray=TpvDynamicArray<TPerInFlightFrameRenderInstance>;
                          PPerInFlightFrameRenderInstanceDynamicArray=^TPerInFlightFrameRenderInstanceDynamicArray;
                          TPerInFlightFrameRenderInstances=array[0..MaxInFlightFrames-1] of TPerInFlightFrameRenderInstanceDynamicArray;
                          TCullVisibleBitmap=array of TpvUInt32;
                          TCullVisibleBitmaps=array[0..MaxInFlightFrames-1] of TCullVisibleBitmap;
                          TCullVisibleNodePath=array of TpvSizeInt;
                          TCullVisibleNodePaths=array[0..MaxInFlightFrames-1] of TCullVisibleNodePath;
                          TOnNodeFilter=function(const aInFlightFrameIndex:TpvSizeInt;const aRendererInstance:TObject;const aRenderPassIndex:TpvSizeInt;const aGroup:TpvScene3D.TGroup;const aGroupInstance:TpvScene3D.TGroup.TInstance;const aNode:TpvScene3D.TGroup.TNode;const aInstanceNode:TpvScene3D.TGroup.TInstance.PNode):boolean of object;
                          type TAABBTreeSkipListItem=record
                                public
                                 AABB:TpvAABB;
                                 UserData:TpvUInt32;
                                 NodeIndex:TpvSizeInt;
                                 Level:TpvSizeInt;
                                 SkipCount:TpvSizeInt;
                               end;
                               PAABBTreeSkipListItem=^TAABBTreeSkipListItem;
                               TAABBTreeSkipListItems=array of TAABBTreeSkipListItem;
                          TAABBTreeSkipList=record
                           public
                            Items:TAABBTreeSkipListItems;
                            Count:TpvSizeInt;
                          end;
                          PAABBTreeSkipList=^TAABBTreeSkipList;
                    private
                     fGroup:TGroup;
                     fLock:TPasMPSpinLock;
                     fActive:boolean;
                     fHeadless:boolean;
                     fPreviousActive:boolean;
                     fUseRenderInstances:boolean;
                     fIsNewInstance:TPasMPBool32;
                     fScene:TPasGLTFSizeInt;
                     fMaterialMap:TpvScene3D.TGroup.TMaterialMap;
                     fDuplicatedMaterials:TpvScene3D.TMaterials;
                     fMaterials:TpvScene3D.TGroup.TInstance.TMaterials;
                     fAnimations:TpvScene3D.TGroup.TInstance.TAnimations;
                     fNodes:TpvScene3D.TGroup.TInstance.TNodes;
                     fSkins:TpvScene3D.TGroup.TInstance.TSkins;
                     fScenes:TpvScene3D.TGroup.TInstance.TScenes;
                     fCameras:TpvScene3D.TGroup.TInstance.TCameras;
                     fLights:TpvScene3D.TGroup.TInstance.TLights;
                     fLightNodes:TNodeIndices;
                     fLightShadowMapMatrices:TPasGLTF.TMatrix4x4DynamicArray;
                     fLightShadowMapZFarValues:TPasGLTFFloatDynamicArray;
                     fBoundingBox:TpvAABB;
                     fBoundingBoxes:array[0..MaxInFlightFrames-1] of TpvAABB;
                     fBoundingSpheres:array[0..MaxInFlightFrames-1] of TpvSphere;
                     fUserData:pointer;
                     fOnNodeMatrixPre:TOnNodeMatrix;
                     fOnNodeMatrixPost:TOnNodeMatrix;
                     fOnNodeFilter:TpvScene3D.TGroup.TInstance.TOnNodeFilter;
                     fUploaded:boolean;
                     fDirtyCounter:TPasMPInt32;
                     fModelMatrix:TpvMatrix4x4;
                     fNodeMatrices:TNodeMatrices;
                     fMorphTargetVertexWeights:TMorphTargetVertexWeights;
                     fRenderInstanceLock:TpvInt32;
                     fRenderInstances:TRenderInstances;
                     fPerInFlightFrameRenderInstances:TPerInFlightFrameRenderInstances;
                    public
                     fVulkanPerInFlightFrameFirstInstances:array[0..MaxInFlightFrames-1,0..MaxRendererInstances-1,0..MaxRenderPassIndices-1] of TpvSizeInt;
                     fVulkanPerInFlightFrameInstancesCounts:array[0..MaxInFlightFrames-1,0..MaxRendererInstances-1,0..MaxRenderPassIndices-1] of TpvSizeInt;
                    private
                     fActiveScenes:array[0..MaxInFlightFrames-1] of TpvScene3D.TGroup.TScene;
                     fActives:array[0..MaxInFlightFrames-1] of boolean;
                     fPotentiallyVisibleSetNodeIndices:array[0..MaxInFlightFrames-1] of TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
                     fCullVisibleBitmapLocks:array[0..MaxInFlightFrames-1] of TPasMPInt32;
                     fCullVisibleBitmaps:TCullVisibleBitmaps;
                     fAABBTreeProxy:TpvSizeInt;
                     fAABBTree:TpvBVHDynamicAABBTree;
                     fAABBTreeStates:array[0..MaxInFlightFrames-1] of TpvBVHDynamicAABBTree.TState;
                     fAABBTreeSkipLists:array[0..MaxInFlightFrames-1] of TAABBTreeSkipList;
                     fVulkanVertexBufferOffset:TpvInt64;
                     fVulkanVertexBufferCount:TpvInt64;
                     fVulkanDrawIndexBufferOffset:TpvInt64;
                     fVulkanDrawIndexBufferCount:TpvInt64;
                     fVulkanDrawUniqueIndexBufferOffset:TpvInt64;
                     fVulkanDrawUniqueIndexBufferCount:TpvInt64;
                     fVulkanMorphTargetVertexBufferOffset:TpvInt64;
                     fVulkanMorphTargetVertexBufferCount:TpvInt64;
                     fVulkanJointBlockBufferOffset:TpvInt64;
                     fVulkanJointBlockBufferCount:TpvInt64;
                    public
                     fVulkanNodeMatricesBufferOffset:TpvInt64;
                     fVulkanNodeMatricesBufferCount:TpvInt64;
                    private
                     fVulkanMorphTargetVertexWeightsBufferOffset:TpvInt64;
                     fVulkanMorphTargetVertexWeightsBufferCount:TpvInt64;
                     fCacheVerticesNodeDirtyBitmap:array of TpvUInt32;
                     function GetAutomation(const aIndex:TPasGLTFSizeInt):TpvScene3D.TGroup.TInstance.TAnimation;
                     procedure SetScene(const aScene:TpvSizeInt);
                     function GetScene:TpvScene3D.TGroup.TScene;
                     procedure SetModelMatrix(const aModelMatrix:TpvMatrix4x4);
                     procedure PreparePerInFlightFrameRenderInstances(const aInFlightFrameIndex:TpvSizeInt;
                                                                      const aRenderPassIndex:TpvSizeInt;
                                                                      const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                                                                      const aViewBaseIndex:TpvSizeInt;
                                                                      const aCountViews:TpvSizeInt;
                                                                      const aFrustums:TpvFrustumDynamicArray;
                                                                      const aPotentiallyVisibleSetCulling:boolean;
                                                                      out aFirstInstance:TpvSizeInt;
                                                                      out aInstancesCount:TpvSizeInt);
                     procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                       const aRendererInstance:TObject;
                                       const aRenderPassIndex:TpvSizeInt;
                                       const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                                       const aViewBaseIndex:TpvSizeInt;
                                       const aCountViews:TpvSizeInt;
                                       const aFrustums:TpvFrustumDynamicArray;
                                       const aPotentiallyVisibleSetCulling:boolean;
                                       const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes;
                                       const aFrustumCullMask:TpvUInt32);
                     procedure GetBakedMeshProcessMorphSkinNode(const aBakedMesh:TpvScene3D.TBakedMesh;
                                                                const aNode:TpvScene3D.TGroup.TNode;
                                                                const aInstanceNode:TpvScene3D.TGroup.TInstance.PNode;
                                                                const aRelative:Boolean;
                                                                const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
                     procedure UpdateCachedVertices(const aInFlightFrameIndex:TpvSizeInt);
                    public
                     constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil;const aHeadless:Boolean=false); reintroduce;
                     destructor Destroy; override;
                     procedure AfterConstruction; override;
                     procedure BeforeDestruction; override;
                     procedure Remove; override;
                     procedure LoadData; override;
                     procedure Upload; override;
                     procedure Unload; override;
                     procedure UpdateInvisible;
                     procedure Check(const aInFlightFrameIndex:TpvSizeInt);
                     procedure Update(const aInFlightFrameIndex:TpvSizeInt);
                     procedure PrepareFrame(const aInFlightFrameIndex:TpvSizeInt);
                     procedure UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
                     function GetBakedMeshFromSplittedNode(const aNode:TpvScene3D.TGroup.TNode;
                                                           const aRelative:boolean=false;
                                                           const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                                                           const aNodeFilter:TOnNodeFilter=nil):TpvScene3D.TBakedMesh;
                     function GetBakedMeshFromSplittedNodeList(const aNodes:TpvScene3D.TGroup.TNodes;
                                                               const aRelative:boolean=false;
                                                               const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                                                               const aNodeFilter:TOnNodeFilter=nil):TpvScene3D.TBakedMesh;
                     function GetBakedMesh(const aRelative:boolean=false;
                                           const aWithDynamicMeshs:boolean=false;
                                           const aRootNodeIndex:TpvSizeInt=-1;
                                           const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                                           const aNodeFilter:TOnNodeFilter=nil):TpvScene3D.TBakedMesh;
                     function GetCamera(const aNodeIndex:TPasGLTFSizeInt;
                                        out aCameraMatrix:TpvMatrix4x4;
                                        out aViewMatrix:TpvMatrix4x4;
                                        out aProjectionMatrix:TpvMatrix4x4;
                                        const aReversedZ:boolean=false;
                                        const aInfiniteFarPlane:boolean=false;
                                        const aZNear:PpvFloat=nil;
                                        const aZFar:PpvFloat=nil;
                                        const aAspectRatio:TpvFloat=0.0):boolean;
                     procedure SetDirty;
                     function CreateRenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
                    published
                     property Group:TGroup read fGroup write fGroup;
                     property Active:boolean read fActive write fActive;
                     property UseRenderInstances:boolean read fUseRenderInstances write fUseRenderInstances;
                     property Scene:TpvSizeInt read fScene write SetScene;
                     property Cameras:TpvScene3D.TGroup.TInstance.TCameras read fCameras;
                     property Lights:TpvScene3D.TGroup.TInstance.TLights read fLights;
                    public
                     property Nodes:TpvScene3D.TGroup.TInstance.TNodes read fNodes;
                     property Skins:TpvScene3D.TGroup.TInstance.TSkins read fSkins;
                     property UserData:pointer read fUserData write fUserData;
                     property ModelMatrix:TpvMatrix4x4 read fModelMatrix write SetModelMatrix;
                     property RenderInstances:TRenderInstances read fRenderInstances;
                    public
                     property Automations[const aIndex:TPasGLTFSizeInt]:TpvScene3D.TGroup.TInstance.TAnimation read GetAutomation;
                    published
                     property OnNodeMatrixPre:TOnNodeMatrix read fOnNodeMatrixPre write fOnNodeMatrixPre;
                     property OnNodeMatrixPost:TOnNodeMatrix read fOnNodeMatrixPost write fOnNodeMatrixPost;
                     property OnNodeFilter:TpvScene3D.TGroup.TInstance.TOnNodeFilter read fOnNodeFilter write fOnNodeFilter;
                   end;
                   TInstances=TpvObjectGenericList<TInstance>;
                   TMaterialsToDuplicate=TpvObjectGenericList<TpvScene3D.TMaterial>;
                   TNodeNameIndexHashMap=TpvStringHashMap<TpvSizeInt>;
                   TCameraNodeIndices=TpvGenericList<TpvSizeInt>;
                   TCameraNameIndexHashMap=TpvStringHashMap<TpvSizeInt>;
                   TMeshNameIndexHashMap=TpvStringHashMap<TpvSizeInt>;
             private
              fCulling:boolean;
              fDynamicAABBTreeCulling:boolean;
              fHeadless:boolean;
              fObjects:TBaseObjects;
              fMaterialsToDuplicate:TpvScene3D.TGroup.TMaterialsToDuplicate;
              fMaterials:TpvScene3D.TMaterials;
              fMaterialMap:TpvScene3D.TGroup.TMaterialMap;
              fMaterialIDMapArrayIndexHashMap:TpvScene3D.TGroup.TMaterialIDMapArrayIndexHashMap;
              fMaterialNameMapArrayIndexHashMap:TpvScene3D.TGroup.TMaterialNameMapArrayIndexHashMap;
              fMaterialIDMapArrays:TpvScene3D.TGroup.TMaterialIDMapArrays;
              fAnimations:TpvScene3D.TGroup.TAnimations;
              fCountInstanceAnimationChannels:TpvSizeInt;
              fCameras:TpvScene3D.TGroup.TCameras;
              fCameraNameIndexHashMap:TCameraNameIndexHashMap;
              fMeshes:TpvScene3D.TGroup.TMeshes;
              fMeshNameIndexHashMap:TMeshNameIndexHashMap;
              fSkins:TpvScene3D.TGroup.TSkins;
              fLights:TpvScene3D.TGroup.TLights;
              fNodes:TpvScene3D.TGroup.TNodes;
              fScenes:TpvScene3D.TGroup.TScenes;
              fScene:TpvScene3D.TGroup.TScene;
              fVertices:TGroupVertices;
              fIndices:TGroupIndices;
              fDrawChoreographyBatchCondensedIndices:TGroupIndices;
              fDrawChoreographyBatchCondensedUniqueIndices:TGroupIndices;
              fJointBlocks:TGroupJointBlocks;
              fJointBlockOffsets:TSizeIntDynamicArrayEx;
              fMorphTargetVertices:TMorphTargetVertexDynamicArray;
              fSkinStorageBufferSize:TpvSizeInt;
              fMorphTargetCount:TpvSizeInt;
              fCountNodeWeights:TpvSizeInt;
              fCountJointNodeMatrices:TpvSizeInt;
              fLock:TPasMPSpinLock;
              fInstanceListLock:TPasMPSlimReaderWriterLock;
              fInstances:TInstances;
              fMaximumCountInstances:TpvSizeint;
              fBoundingBox:TpvAABB;
              fUsedVisibleDrawNodes:TUsedVisibleDrawNodes;
              fDrawChoreographyBatchItems:TDrawChoreographyBatchItems;
              fDrawChoreographyBatchUniqueItems:TDrawChoreographyBatchItems;
              fNodeNameIndexHashMap:TpvScene3D.TGroup.TNodeNameIndexHashMap;
              fCameraNodeIndices:TpvScene3D.TGroup.TCameraNodeIndices;
              fCachedVertexBufferMemoryBarriers:TVkBufferMemoryBarrierArray;
              fOnNodeFilter:TpvScene3D.TGroup.TInstance.TOnNodeFilter;
              procedure ConstructBuffers;
              procedure MarkAnimatedElements;
              procedure CollectAllSceneNodesAndSplitNodesIntoAnimatedOrNotAnimatedSubtreesPerScene;
              procedure CollectUsedVisibleDrawNodes;
              procedure CollectMaterials;
              procedure ConstructDrawChoreographyBatchItems;
              procedure UpdateCachedVertices(const aInFlightFrameIndex:TpvSizeInt);
              function GetNodeIndexByName(const aNodeName:TpvUTF8String):TpvSizeInt;
              function GetNodeByName(const aNodeName:TpvUTF8String):TpvScene3D.TGroup.TNode;
              function AssetGetURI(const aURI:TPasGLTFUTF8String):TStream;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure LoadData; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure Check(const aInFlightFrameIndex:TpvSizeInt);
              procedure Update(const aInFlightFrameIndex:TpvSizeInt);
              procedure PrepareFrame(const aInFlightFrameIndex:TpvSizeInt);
              procedure UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument);
              function BeginLoad(const aStream:TStream):boolean; override;
              function EndLoad:boolean; override;
              function CreateInstance(const aHeadless:Boolean=false):TpvScene3D.TGroup.TInstance;
             public
              property BoundingBox:TpvAABB read fBoundingBox;
              property NodeIndexByName[const aNodeName:TpvUTF8String]:TpvSizeInt read GetNodeIndexByName;
              property NodeByName[const aNodeName:TpvUTF8String]:TpvScene3D.TGroup.TNode read GetNodeByName;
              property CameraNodeIndices:TpvScene3D.TGroup.TCameraNodeIndices read fCameraNodeIndices;
             published
              property Culling:boolean read fCulling write fCulling;
              property DynamicAABBTreeCulling:boolean read fDynamicAABBTreeCulling write fDynamicAABBTreeCulling;
              property Headless:boolean read fHeadless write fHeadless;
              property Objects:TBaseObjects read fObjects;
              property Animations:TAnimations read fAnimations;
              property Cameras:TCameras read fCameras;
              property Meshes:TMeshes read fMeshes;
              property Skins:TSkins read fSkins;
              property Lights:TpvScene3D.TGroup.TLights read fLights;
              property Nodes:TNodes read fNodes;
              property Scenes:TScenes read fScenes;
              property Scene:TScene read fScene;
              property MaximumCountInstances:TpvSizeint read fMaximumCountInstances write fMaximumCountInstances;
              property OnNodeFilter:TpvScene3D.TGroup.TInstance.TOnNodeFilter read fOnNodeFilter write fOnNodeFilter;
            end;
            TGroups=TpvObjectGenericList<TpvScene3D.TGroup>;
            TImageIDHashMap=TpvHashMap<TID,TImage>;
            TSamplerIDHashMap=TpvHashMap<TID,TSampler>;
            TTextureIDHashMap=TpvHashMap<TID,TTexture>;
            TMaterialIDHashMap=TpvHashMap<TID,TMaterial>;
            TMaterialIDDirtyMap=array[0..(($10000+31) shr 5)-1] of TPasMPUInt32;
            PMaterialIDDirtyMap=^TMaterialIDDirtyMap;
            TMaterialIDDirtyMaps=array[0..MaxInFlightFrames-1] of TMaterialIDDirtyMap;
            TMaterialIDMap=array[0..$ffff] of TMaterial;
            TMaterialGenerations=array[0..$ffff] of TpvUInt64;
            TImageHashMap=TpvHashMap<TImage.THashData,TImage>;
            TSamplerHashMap=TpvHashMap<TSampler.THashData,TSampler>;
            TTextureHashMap=TpvHashMap<TTexture.THashData,TTexture>;
            TMaterialHashMap=TpvHashMap<TMaterial.THashData,TMaterial>;
            TBufferMemoryBarriers=TpvDynamicArray<TVkBufferMemoryBarrier>;
            TInFlightFrameBufferMemoryBarriers=array[0..MaxInFlightFrames-1] of TBufferMemoryBarriers;
            TMaterialBufferData=array[0..65535] of TMaterial.TShaderData;
            PMaterialBufferData=^TMaterialBufferData;
            TImageInfos=array[0..65535] of TVkDescriptorImageInfo;
            TGlobalVulkanDescriptorSets=array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
            TVertexStagePushConstantArray=array[0..MaxRenderPassIndices-1] of TpvScene3D.TVertexStagePushConstants;
            TInFlightFrameLights=array[0..MaxInFlightFrames-1] of TpvScene3D.TLights;
            TCountInFlightFrameLights=array[0..MaxInFlightFrames-1] of TpvSizeInt;
            TCachedVertexRange=record
             Offset:TpvSizeInt;
             Count:TpvSizeInt;
            end;
            PCachedVertexRange=^TCachedVertexRange;
            TCachedVertexRanges=TpvDynamicArray<TCachedVertexRange>;
            TInFlightFrameMaterialBufferDataGenerations=array[0..MaxInFlightFrames-1] of TMaterialGenerations;
            TSetGlobalResourcesDone=array[0..MaxRenderPassIndices-1] of boolean;
      public
       const DoubleSidedFaceCullingModes:array[TDoubleSided,TFrontFacesInversed] of TFaceCullingMode=
              (
               (TFaceCullingMode.Normal,TFaceCullingMode.Inversed),
               (TFaceCullingMode.None,TFaceCullingMode.None)
              );
      private
       fLock:TPasMPSpinLock;
       fLoadLock:TPasMPSpinLock;
       fVulkanDevice:TpvVulkanDevice;
       fUploaded:TPasMPBool32;
       fInUpload:TPasMPBool32;
       fRendererInstanceIDManager:TRendererInstanceIDManager;
       fFreeQueueLock:TPasMPSlimReaderWriterLock;
       fFreeQueue:TFreeQueue;
       fObjectListLock:TPasMPCriticalSection;
       fObjectList:TpvObjectList;
       fPotentiallyVisibleSet:TpvScene3D.TPotentiallyVisibleSet;
       fBufferStreamingMode:TBufferStreamingMode;
       fMultiDrawSupport:Boolean;
       fMaxMultiDrawCount:TpvUInt32;
       fHardwareRaytracingSupport:Boolean;
       fAccelerationStructureInputBufferUsageFlags:TVkBufferUsageFlags;
       fDefaultSampler:TSampler;
       fWhiteImage:TImage;
       fWhiteTexture:TTexture;
       fDefaultNormalMapImage:TImage;
       fDefaultNormalMapTexture:TTexture;
       fDefaultParticleImage:TImage;
       fDefaultParticleTexture:TTexture;
       fMeshComputeVulkanDescriptorSet0Layout:TpvVulkanDescriptorSetLayout;
       fMeshComputeVulkanDescriptorSet1Layout:TpvVulkanDescriptorSetLayout;
       fVulkanStagingQueue:TpvVulkanQueue;
       fVulkanStagingCommandPool:TpvVulkanCommandPool;
       fVulkanStagingCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanStagingFence:TpvVulkanFence;
       fImageDescriptorGenerationLock:TPasMPSpinLock;
       fImageDescriptorProcessedGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fImageDescriptorGeneration:TpvUInt64;
       fImageDescriptorProcessedGeneration:TpvUInt64;
//     fGlobalVulkanViewUniformStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fGlobalVulkanInstanceMatrixDynamicArrays:TGlobalVulkanInstanceMatrixDynamicArrays;
       fGlobalVulkanInstanceMatrixBuffers:TGlobalVulkanInstanceMatrixBuffers;
       fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fGlobalVulkanDescriptorSets:TGlobalVulkanDescriptorSets;
{      fVulkanLightItemsStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanLightTreeStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanLightMetaInfoStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;}
       fInFlightFrameMaterialBufferData:array[0..MaxInFlightFrames-1] of TMaterialBufferData;
       fInFlightFrameMaterialBufferDataGenerations:TInFlightFrameMaterialBufferDataGenerations;
//     fVulkanMaterialDataStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanMaterialDataBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanMaterialUniformBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fTechniques:TpvTechniques;
       fCullObjectIDLock:TPasMPSlimReaderWriterLock;
       fCullObjectIDManager:TIDManager;
       fMaxCullObjectID:TpvUInt32;
       fImageListLock:TPasMPCriticalSection;
       fImages:TImages;
       fImageIDManager:TIDManager;
       fImageIDHashMap:TImageIDHashMap;
       fImageHashMap:TImageHashMap;
       fSamplerListLock:TPasMPCriticalSection;
       fSamplers:TSamplers;
       fSamplerIDManager:TIDManager;
       fSamplerIDHashMap:TSamplerIDHashMap;
       fSamplerHashMap:TSamplerHashMap;
       fTextureListLock:TPasMPCriticalSection;
       fTextures:TTextures;
       fTextureIDManager:TIDManager;
       fTextureIDHashMap:TTextureIDHashMap;
       fTextureHashMap:TTextureHashMap;
       fMaterialListLock:TPasMPCriticalSection;
       fMaterials:TMaterials;
       fMaxMaterialID:TPasMPInt32;
       fMaterialIDManager:TIDManager;
       fMaterialIDHashMap:TMaterialIDHashMap;
       fMaterialIDDirtyMaps:TMaterialIDDirtyMaps;
       fMaterialIDToUpdateDirtyMaps:TMaterialIDDirtyMaps;
       fMaterialIDMap:TMaterialIDMap;
       fMaterialHashMap:TMaterialHashMap;
       fEmptyMaterial:TpvScene3D.TMaterial;
       fMaterialDataProcessedGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fMaterialDataUpdatedGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fMaterialDataGeneration:TpvUInt64;
       fMaterialDataGenerationLock:TPasMPSpinLock;
       fLights:TInFlightFrameLights;
       fCountLights:TCountInFlightFrameLights;
       fIndirectLights:array[0..MaxInFlightFrames-1,0..MaxVisibleLights-1] of TpvScene3D.TLight;
       fCountIndirectLights:array[0..MaxInFlightFrames-1] of TpvSizeInt;
       fGroupListLock:TPasMPSlimReaderWriterLock;
       fGroups:TGroups;
       fGroupInstanceListLock:TPasMPSlimReaderWriterLock;
       fGroupInstances:TGroup.TInstances;
       fLightAABBTree:TpvBVHDynamicAABBTree;
       fLightAABBTreeGeneration:TpvUInt64;
       fLightAABBTreeStates:array[0..MaxInFlightFrames-1] of TpvBVHDynamicAABBTree.TState;
       fLightAABBTreeStateGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fLightBuffers:TpvScene3D.TLightBuffers;
       fAABBTree:TpvBVHDynamicAABBTree;
       fAABBTreeStates:array[0..MaxInFlightFrames-1] of TpvBVHDynamicAABBTree.TState;
       fBoundingBox:TpvAABB;
       fInFlightFrameBoundingBoxes:TInFlightFrameAABBs;
       fCountInFlightFrames:TpvSizeInt;
       fUseBufferDeviceAddress:boolean;
       fHasTransmission:boolean;
       fImageInfos:TpvScene3D.TImageInfos;
       fInFlightFrameImageInfos:array[0..MaxInFlightFrames-1] of TpvScene3D.TImageInfos;
       fInFlightFrameImageInfoImageDescriptorGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fInFlightFrameImageInfoImageDescriptorUploadedGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fPrimaryLightDirection:TpvVector3;
       fPrimaryShadowMapLightDirection:TpvVector3;
       fDebugPrimitiveVertexDynamicArrays:TpvScene3D.TDebugPrimitiveVertexDynamicArrays;
       fVulkanDebugPrimitiveVertexBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fOnNodeFilter:TpvScene3D.TGroup.TInstance.TOnNodeFilter;
       fParticles:TParticles;
       fPointerToParticles:PParticles;
       fParticleAliveBitmap:TParticleAliveBitmap;
       fParticleIndexCounter:TpvUInt32;
       fInFlightFrameParticleVertices:TInFlightFrameParticleVertices;
       fCountInFlightFrameParticleVertices:array[0..MaxInFlightFrames-1] of TpvUInt32;
       fVulkanParticleVertexBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fSkyBoxBrightnessFactor:TpvScalar;
       fBufferRangeAllocatorLock:TPasMPCriticalSection;
       fVulkanDynamicVertexBufferData:TGPUDynamicVertexDynamicArray;
       fVulkanStaticVertexBufferData:TGPUStaticVertexDynamicArray;
       fVulkanDrawIndexBufferData:TIndicesDynamicArray;
       fVulkanDrawUniqueIndexBufferData:TIndicesDynamicArray;
       fVulkanMorphTargetVertexBufferData:TMorphTargetVertexDynamicArray;
       fVulkanJointBlockBufferData:TJointBlocksDynamicArray;
       fVkMultiDrawIndexedInfoEXTDynamicArray:TVkMultiDrawIndexedInfoEXTDynamicArray;
       fVkMultiDrawIndexedInfoEXTFirstInstance:TpvSizeInt;
       fVkMultiDrawIndexedInfoEXTInstancesCount:TpvSizeInt;
      public
       fVulkanNodeMatricesBufferData:array[0..MaxInFlightFrames-1] of TMatricesDynamicArray;
      private
       fVulkanMorphTargetVertexWeightsBufferData:array[0..MaxInFlightFrames-1] of TFloatsDynamicArray;
       fVulkanVertexBufferRangeAllocator:TpvBufferRangeAllocator;
       fVulkanDrawIndexBufferRangeAllocator:TpvBufferRangeAllocator;
       fVulkanDrawUniqueIndexBufferRangeAllocator:TpvBufferRangeAllocator;
       fVulkanMorphTargetVertexBufferRangeAllocator:TpvBufferRangeAllocator;
       fVulkanJointBlockBufferRangeAllocator:TpvBufferRangeAllocator;
       fVulkanNodeMatricesBufferRangeAllocator:TpvBufferRangeAllocator;
       fVulkanMorphTargetVertexWeightsBufferRangeAllocator:TpvBufferRangeAllocator;
       fVulkanLongTermStaticBuffers:TVulkanLongTermStaticBuffers;
      public
       fVulkanShortTermDynamicBuffers:TVulkanShortTermDynamicBuffers;
      private
       fCachedVertexRanges:TCachedVertexRanges;
       fMeshGenerationCounter:TpvUInt32;
       fNewInstanceListLock:TPasMPSlimReaderWriterLock;
       fNewInstances:TpvScene3D.TGroup.TInstances;
       procedure NewImageDescriptorGeneration;
       procedure NewMaterialDataGeneration;
       procedure CullLights(const aInFlightFrameIndex:TpvSizeInt;
                            const aFrustums:TpvFrustumDynamicArray;
                            const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                            const aRoot:TpvSizeInt);
       procedure CollectLights(const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                            const aRoot:TpvSizeInt;
                                            var aLightItemArray:TpvScene3D.TLightItems;
                                            var aLightMetaInfoArray:TpvScene3D.TLightMetaInfos);
       procedure CullAndPrepareGroupInstances(const aInFlightFrameIndex:TpvSizeInt;
                                              const aRendererInstance:TObject;
                                              const aRenderPassIndex:TpvSizeInt;
                                              const aViews:TpvScene3D.TViews;
                                              const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                                              const aViewBaseIndex:TpvSizeInt;
                                              const aCountViews:TpvSizeInt;
                                              const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes;
                                              const aPotentiallyVisibleSetCulling:boolean;
                                              const aFrustums:TpvFrustumDynamicArray;
                                              const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                              const aRoot:TpvSizeInt);
       function GetLightUserDataIndex(const aUserData:TpvPtrInt):TpvUInt32;
      public
       procedure SetGlobalResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                    const aPipelineLayout:TpvVulkanPipelineLayout;
                                    const aRendererInstance:TObject;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aPreviousInFlightFrameIndex:TpvSizeInt;
                                    const aInFlightFrameIndex:TpvSizeInt);
      private
       procedure ProcessFreeQueue;
      public
       class function EncodeModeFlags(const aAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
                                      const aPrimitiveTopology:TpvScene3D.TPrimitiveTopology;
                                      const aFaceCullingMode:TpvScene3D.TFaceCullingMode):TpvUInt32; static;
       class procedure DecodeModeFlags(const aFlags:TpvUInt32;
                                       out aAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
                                       out aPrimitiveTopology:TpvScene3D.TPrimitiveTopology;
                                       out aFaceCullingMode:TpvScene3D.TFaceCullingMode); static;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil;const aVulkanDevice:TpvVulkanDevice=nil;const aUseBufferDeviceAddress:boolean=true;const aCountInFlightFrames:TpvSizeInt=MaxInFlightFrames); reintroduce;
       destructor Destroy; override;
       procedure AddToFreeQueue(const aObject:TObject;const aFrameDelay:TpvInt32=-1);
       procedure Upload;
       procedure Unload;
       procedure ResetFrame(const aInFlightFrameIndex:TpvSizeInt);
       procedure Check(const aInFlightFrameIndex:TpvSizeInt);
       procedure Update(const aInFlightFrameIndex:TpvSizeInt);
       procedure PrepareFrame(const aInFlightFrameIndex:TpvSizeInt);
       procedure BeginFrame(const aInFlightFrameIndex:TpvSizeInt);
       procedure EndFrame(const aInFlightFrameIndex:TpvSizeInt);
//     procedure FinalizeViews(const aInFlightFrameIndex:TpvSizeInt);
       procedure UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
       procedure PrepareLights(const aInFlightFrameIndex:TpvSizeInt;
                               const aViewBaseIndex:TpvSizeInt;
                               const aCountViews:TpvSizeInt;
                               const aViewPortWidth:TpvInt32;
                               const aViewPortHeight:TpvInt32;
                               const aFrustums:TpvFrustumDynamicArray);
       procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;
                         const aRendererInstance:TObject;
                         const aRenderPassIndex:TpvSizeInt;
                         const aViews:TpvScene3D.TViews;
                         const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                         const aViewBaseIndex:TpvSizeInt;
                         const aCountViews:TpvSizeInt;
                         const aViewPortWidth:TpvInt32;
                         const aViewPortHeight:TpvInt32;
                         const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                         const aLights:boolean=true;
                         const aFrustumCulling:boolean=true;
                         const aPotentiallyVisibleSetCulling:boolean=true;
                         const aGPUCulling:boolean=true);
       procedure UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                      const aInFlightFrameIndex:TpvSizeInt;
                                      const aCommandBuffer:TpvVulkanCommandBuffer;
                                      const aPipelineLayout:TpvVulkanPipelineLayout);
       procedure DrawDebugPrimitives(const aRendererInstance:TObject;
                                     const aGraphicsPipeline:TpvVulkanGraphicsPipeline;
                                     const aPreviousInFlightFrameIndex:TpvSizeInt;
                                     const aInFlightFrameIndex:TpvSizeInt;
                                     const aRenderPassIndex:TpvSizeInt;
                                     const aViewBaseIndex:TpvSizeInt;
                                     const aCountViews:TpvSizeInt;
                                     const aFrameIndex:TpvSizeInt;
                                     const aCommandBuffer:TpvVulkanCommandBuffer;
                                     const aPipelineLayout:TpvVulkanPipelineLayout;
                                     const aOnSetRenderPassResources:TpvScene3D.TOnSetRenderPassResources);
       procedure DrawParticles(const aRendererInstance:TObject;
                               const aGraphicsPipeline:TpvVulkanGraphicsPipeline;
                               const aPreviousInFlightFrameIndex:TpvSizeInt;
                               const aInFlightFrameIndex:TpvSizeInt;
                               const aRenderPassIndex:TpvSizeInt;
                               const aViewBaseIndex:TpvSizeInt;
                               const aCountViews:TpvSizeInt;
                               const aFrameIndex:TpvSizeInt;
                               const aCommandBuffer:TpvVulkanCommandBuffer;
                               const aPipelineLayout:TpvVulkanPipelineLayout;
                               const aOnSetRenderPassResources:TpvScene3D.TOnSetRenderPassResources);
       procedure Draw(const aRendererInstance:TObject;
                      const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                      const aPreviousInFlightFrameIndex:TpvSizeInt;
                      const aInFlightFrameIndex:TpvSizeInt;
                      const aRenderPassIndex:TpvSizeInt;
                      const aViewBaseIndex:TpvSizeInt;
                      const aCountViews:TpvSizeInt;
                      const aFrameIndex:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aPipelineLayout:TpvVulkanPipelineLayout;
                      const aOnSetRenderPassResources:TpvScene3D.TOnSetRenderPassResources;
                      const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                      const aJitter:PpvVector4=nil;
                      const aDisocclusions:Boolean=false);
       procedure GetZNearZFar(const aViewMatrix:TpvMatrix4x4;
                              const aAspectRatio:TpvScalar;
                              out aZNear:TpvScalar;
                              out aZFar:TpvScalar);
       procedure InitializeGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline;const aWithPreviousPosition:boolean=false);
       procedure InitializeDebugPrimitiveGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline);
       procedure InitializeParticleGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline);
       procedure StoreParticleStates;
       procedure UpdateParticleStates(const aDeltaTime:TpvDouble);
       procedure InterpolateParticleStates(const aInFlightFrameIndex:TpvSizeInt;const aAlpha:TpvDouble);
       procedure DeleteAllParticles;
       function AddParticle(const aPosition:TpvVector3;
                            const aVelocity:TpvVector3;
                            const aGravity:TpvVector3;
                            const aRotationStart:TpvFloat;
                            const aRotationEnd:TpvFloat;
                            const aSizeStart:TpvVector2;
                            const aSizeEnd:TpvVector2;
                            const aColorStart:TpvVector4;
                            const aColorEnd:TpvVector4;
                            const aLifeTime:TpvScalar;
                            const aTextureID:TpvUInt32;
                            const aAdditiveBlending:boolean):TpvSizeInt;
      public
       property BoundingBox:TpvAABB read fBoundingBox;
       property InFlightFrameBoundingBoxes:TInFlightFrameAABBs read fInFlightFrameBoundingBoxes;
       property GlobalVulkanInstanceMatrixBuffers:TGlobalVulkanInstanceMatrixBuffers read fGlobalVulkanInstanceMatrixBuffers;
       property GlobalVulkanDescriptorSets:TGlobalVulkanDescriptorSets read fGlobalVulkanDescriptorSets;
       property PrimaryLightDirection:TpvVector3 read fPrimaryLightDirection write fPrimaryLightDirection;
       property PrimaryShadowMapLightDirection:TpvVector3 read fPrimaryShadowMapLightDirection write fPrimaryShadowMapLightDirection;
       property LightBuffers:TpvScene3D.TLightBuffers read fLightBuffers;
       property DebugPrimitiveVertexDynamicArrays:TpvScene3D.TDebugPrimitiveVertexDynamicArrays read fDebugPrimitiveVertexDynamicArrays;
       property Particles:PParticles read fPointerToParticles;
       property SkyBoxBrightnessFactor:TpvScalar read fSkyBoxBrightnessFactor write fSkyBoxBrightnessFactor;
      public
       property WhiteImage:TImage read fWhiteImage;
       property WhiteTexture:TTexture read fWhiteTexture;
       property DefaultParticleImage:TImage read fDefaultParticleImage;
       property DefaultParticleTexture:TTexture read fDefaultParticleTexture;
       property Lights:TInFlightFrameLights read fLights;
       property CountLights:TCountInFlightFrameLights read fCountLights;
      published
       property VulkanStagingQueue:TpvVulkanQueue read fVulkanStagingQueue;
       property VulkanStagingCommandPool:TpvVulkanCommandPool read fVulkanStagingCommandPool;
       property VulkanStagingCommandBuffer:TpvVulkanCommandBuffer read fVulkanStagingCommandBuffer;
       property VulkanStagingFence:TpvVulkanFence read fVulkanStagingFence;
      public
       property MaxCullObjectID:TpvUInt32 read fMaxCullObjectID;
      published
       property RendererInstanceIDManager:TRendererInstanceIDManager read fRendererInstanceIDManager;
       property PotentiallyVisibleSet:TpvScene3D.TPotentiallyVisibleSet read fPotentiallyVisibleSet;
       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;
       property MeshComputeVulkanDescriptorSet0Layout:TpvVulkanDescriptorSetLayout read fMeshComputeVulkanDescriptorSet0Layout;
       property MeshComputeVulkanDescriptorSet1Layout:TpvVulkanDescriptorSetLayout read fMeshComputeVulkanDescriptorSet1Layout;
       property GlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fGlobalVulkanDescriptorSetLayout;
       property HasTransmission:boolean read fHasTransmission;
       property UseBufferDeviceAddress:boolean read fUseBufferDeviceAddress write fUseBufferDeviceAddress;
       property CountInFlightFrames:TpvSizeInt read fCountInFlightFrames;
       property BufferStreamingMode:TBufferStreamingMode read fBufferStreamingMode write fBufferStreamingMode;
       property MultiDrawSupport:boolean read fMultiDrawSupport;
       property MaxMultiDrawCount:TpvUInt32 read fMaxMultiDrawCount write fMaxMultiDrawCount;
       property HardwareRaytracingSupport:Boolean read fHardwareRaytracingSupport;
       property AccelerationStructureInputBufferUsageFlags:TVkBufferUsageFlags read fAccelerationStructureInputBufferUsageFlags;
       property OnNodeFilter:TpvScene3D.TGroup.TInstance.TOnNodeFilter read fOnNodeFilter write fOnNodeFilter;
     end;

implementation

uses PasVulkan.Scene3D.Renderer.Instance;

const FlushUpdateData=false;

type TAnimationChannelTargetOverwriteGroupMap=array[TpvScene3D.TGroup.TAnimation.TChannel.TTarget] of TpvUInt64;

var AnimationChannelTargetOverwriteGroupMap:TAnimationChannelTargetOverwriteGroupMap;

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

type { TPOCAScene3DGroupAnimationChannel }
     TPOCAScene3DGroupAnimationChannel=class(TPOCANativeObject)
      private
       fAnimation:TpvScene3D.TGroup.TAnimation;
       fChannelIndex:TpvSizeInt;
       fElementSize:TpvSizeInt;
       fCount:TpvSizeInt;
      public
       constructor Create(const aInstance:PPOCAInstance;const aContext:PPOCAContext;const aPrototype,aConstructor:PPOCAValue;const aExpandable:boolean); override;
       destructor Destroy; override;
      published
       function createKeyFrame(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
       function finish(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
     end;

{ TPOCAScene3DGroupAnimation }

constructor TPOCAScene3DGroupAnimationChannel.Create(const aInstance:PPOCAInstance;const aContext:PPOCAContext;const aPrototype,aConstructor:PPOCAValue;const aExpandable:boolean);
begin
 inherited Create(aInstance,aContext,aPrototype,aConstructor,aExpandable);
end;

destructor TPOCAScene3DGroupAnimationChannel.Destroy;
begin
 inherited Destroy;
end;

function TPOCAScene3DGroupAnimationChannel.createKeyFrame(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
var Index,SubIndex:TpvSizeInt;
    Time:TpvDouble;
    ArrayValue,SubArrayValue:TPOCAValue;
    Elements:array[0..2] of TpvVector4;
    Channel:TpvScene3D.TGroup.TAnimation.PChannel;
    OK:boolean;
begin
 OK:=false;
 if (aCountArguments>=1) and ((fElementSize>=0) and (fElementSize<=4)) then begin
  Channel:=@fAnimation.fChannels[fChannelIndex];
  FillChar(Elements,SizeOf(Elements),#0);
  Time:=POCAGetNumberValue(aContext,aArguments^[0]);
  if (aCountArguments>=2) and (POCAGetValueType(aArguments^[1])=pvtARRAY) then begin
   ArrayValue:=aArguments^[1];
   if Channel^.Interpolation=TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
    if POCAArraySize(ArrayValue)=3 then begin
     OK:=true;
     for Index:=0 to 2 do begin
      SubArrayValue:=POCAArrayGet(ArrayValue,Index);
      if (POCAGetValueType(SubArrayValue)=pvtARRAY) and (POCAArraySize(SubArrayValue)>0) then begin
       for SubIndex:=0 to Min(Min(POCAArraySize(SubArrayValue),fElementSize),4)-1 do begin
        Elements[Index].Components[SubIndex]:=POCAGetNumberValue(aContext,POCAArrayGet(SubArrayValue,SubIndex));
       end;
      end else begin
       OK:=false;
       break;
      end;
     end;
    end;
   end else begin
    if POCAArraySize(ArrayValue)>0 then begin
     for Index:=0 to Min(Min(POCAArraySize(ArrayValue),fElementSize),4)-1 do begin
      Elements[0].Components[Index]:=POCAGetNumberValue(aContext,POCAArrayGet(ArrayValue,Index));
     end;
     OK:=true;
    end;
   end;
  end else begin
   if Channel^.Interpolation<>TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
    for Index:=0 to Min(Min(aCountArguments-1,fElementSize),4)-1 do begin
     Elements[0].Components[Index]:=POCAGetNumberValue(aContext,aArguments^[1+Index]);
    end;
    OK:=true;
   end;
  end;
  if OK then begin
   Index:=fCount;
   inc(fCount);
   if length(Channel^.InputTimeArray)<=fCount then begin
    SetLength(Channel^.InputTimeArray,fCount*2);
    if Channel^.Interpolation=TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
     case fElementSize of
      2:begin
       SetLength(Channel^.OutputVector2Array,fCount*6);
      end;
      3:begin
       SetLength(Channel^.OutputVector3Array,fCount*6);
      end;
      4:begin
       SetLength(Channel^.OutputVector4Array,fCount*6);
      end;
      else begin
       SetLength(Channel^.OutputScalarArray,fCount*6);
      end;
     end;
    end else begin
     case fElementSize of
      2:begin
       SetLength(Channel^.OutputVector2Array,fCount*2);
      end;
      3:begin
       SetLength(Channel^.OutputVector3Array,fCount*2);
      end;
      4:begin
       SetLength(Channel^.OutputVector4Array,fCount*2);
      end;
      else begin
       SetLength(Channel^.OutputScalarArray,fCount*2);
      end;
     end;
    end;
   end;
   Channel^.InputTimeArray[Index]:=Time;
   if Channel^.Interpolation=TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
    case fElementSize of
     2:begin
      Channel^.OutputVector2Array[(Index*3)+0]:=Elements[0].xy;
      Channel^.OutputVector2Array[(Index*3)+1]:=Elements[1].xy;
      Channel^.OutputVector2Array[(Index*3)+2]:=Elements[2].xy;
     end;
     3:begin
      Channel^.OutputVector3Array[(Index*3)+0]:=Elements[0].xyz;
      Channel^.OutputVector3Array[(Index*3)+1]:=Elements[1].xyz;
      Channel^.OutputVector3Array[(Index*3)+2]:=Elements[2].xyz;
     end;
     4:begin
      Channel^.OutputVector4Array[(Index*3)+0]:=Elements[0].xyzw;
      Channel^.OutputVector4Array[(Index*3)+1]:=Elements[1].xyzw;
      Channel^.OutputVector4Array[(Index*3)+2]:=Elements[2].xyzw;
     end;
     else begin
      Channel^.OutputScalarArray[(Index*3)+0]:=Elements[0].x;
      Channel^.OutputScalarArray[(Index*3)+1]:=Elements[1].x;
      Channel^.OutputScalarArray[(Index*3)+2]:=Elements[2].x;
     end;
    end;
   end else begin
    case fElementSize of
     2:begin
      Channel^.OutputVector2Array[Index]:=Elements[0].xy;
     end;
     3:begin
      Channel^.OutputVector3Array[Index]:=Elements[0].xyz;
     end;
     4:begin
      Channel^.OutputVector4Array[Index]:=Elements[0].xyzw;
     end;
     else begin
      Channel^.OutputScalarArray[Index]:=Elements[0].x;
     end;
    end;
   end;
  end;
 end;
 if OK then begin
  result.Num:=1;
 end else begin
  result.Num:=0;
 end;
end;

function TPOCAScene3DGroupAnimationChannel.finish(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
var Channel:TpvScene3D.TGroup.TAnimation.PChannel;
    Index:TpvSizeInt;
begin
 Channel:=@fAnimation.fChannels[fChannelIndex];
 SetLength(Channel^.InputTimeArray,fCount);
 case fElementSize of
  2:begin
   if Channel^.Interpolation=TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
    SetLength(Channel^.OutputVector2Array,fCount*3);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvVector2>.Swap(Channel^.OutputVector2Array[(Index*3)+0],Channel^.OutputVector2Array[((Index+1)*3)+0]);
      TpvSwap<TpvVector2>.Swap(Channel^.OutputVector2Array[(Index*3)+1],Channel^.OutputVector2Array[((Index+1)*3)+1]);
      TpvSwap<TpvVector2>.Swap(Channel^.OutputVector2Array[(Index*3)+2],Channel^.OutputVector2Array[((Index+1)*3)+2]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end else begin
    SetLength(Channel^.OutputVector2Array,fCount);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvVector2>.Swap(Channel^.OutputVector2Array[Index],Channel^.OutputVector2Array[Index+1]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end;
  end;
  3:begin
   if Channel^.Interpolation=TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
    SetLength(Channel^.OutputVector3Array,fCount*3);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvVector3>.Swap(Channel^.OutputVector3Array[(Index*3)+0],Channel^.OutputVector3Array[((Index+1)*3)+0]);
      TpvSwap<TpvVector3>.Swap(Channel^.OutputVector3Array[(Index*3)+1],Channel^.OutputVector3Array[((Index+1)*3)+1]);
      TpvSwap<TpvVector3>.Swap(Channel^.OutputVector3Array[(Index*3)+2],Channel^.OutputVector3Array[((Index+1)*3)+2]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end else begin
    SetLength(Channel^.OutputVector3Array,fCount);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvVector3>.Swap(Channel^.OutputVector3Array[Index],Channel^.OutputVector3Array[Index+1]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end;
  end;
  4:begin
   if Channel^.Interpolation=TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
    SetLength(Channel^.OutputVector4Array,fCount*3);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvVector4>.Swap(Channel^.OutputVector4Array[(Index*3)+0],Channel^.OutputVector4Array[((Index+1)*3)+0]);
      TpvSwap<TpvVector4>.Swap(Channel^.OutputVector4Array[(Index*3)+1],Channel^.OutputVector4Array[((Index+1)*3)+1]);
      TpvSwap<TpvVector4>.Swap(Channel^.OutputVector4Array[(Index*3)+2],Channel^.OutputVector4Array[((Index+1)*3)+2]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end else begin
    SetLength(Channel^.OutputVector4Array,fCount);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvVector4>.Swap(Channel^.OutputVector4Array[Index],Channel^.OutputVector4Array[Index+1]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end;
  end;
  else begin
   if Channel^.Interpolation=TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline then begin
    SetLength(Channel^.OutputScalarArray,fCount*3);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvFloat>.Swap(Channel^.OutputScalarArray[(Index*3)+0],Channel^.OutputScalarArray[((Index+1)*3)+0]);
      TpvSwap<TpvFloat>.Swap(Channel^.OutputScalarArray[(Index*3)+1],Channel^.OutputScalarArray[((Index+1)*3)+1]);
      TpvSwap<TpvFloat>.Swap(Channel^.OutputScalarArray[(Index*3)+2],Channel^.OutputScalarArray[((Index+1)*3)+2]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end else begin
    SetLength(Channel^.OutputScalarArray,fCount);
    Index:=0;
    while Index<(fCount-1) do begin
     if Channel^.InputTimeArray[Index]>Channel^.InputTimeArray[Index+1] then begin
      TpvSwap<TpvDouble>.Swap(Channel^.InputTimeArray[Index],Channel^.InputTimeArray[Index+1]);
      TpvSwap<TpvFloat>.Swap(Channel^.OutputScalarArray[Index],Channel^.OutputScalarArray[Index+1]);
      if Index>0 then begin
       dec(Index);
      end else begin
       inc(Index);
      end;
     end else begin
      inc(Index);
     end;
    end;
   end;
  end;
 end;
 result:=POCAValueNull;
end;

type { TPOCAScene3DGroupAnimation }
     TPOCAScene3DGroupAnimation=class(TPOCANativeObject)
      private
       fAnimation:TpvScene3D.TGroup.TAnimation;
      public
       constructor Create(const aInstance:PPOCAInstance;const aContext:PPOCAContext;const aPrototype,aConstructor:PPOCAValue;const aExpandable:boolean); override;
       destructor Destroy; override;
      published
       function createChannel(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
     end;

{ TPOCAScene3DGroupAnimation }

constructor TPOCAScene3DGroupAnimation.Create(const aInstance:PPOCAInstance;const aContext:PPOCAContext;const aPrototype,aConstructor:PPOCAValue;const aExpandable:boolean);
begin
 inherited Create(aInstance,aContext,aPrototype,aConstructor,aExpandable);
end;

destructor TPOCAScene3DGroupAnimation.Destroy;
begin
 inherited Destroy;
end;

function TPOCAScene3DGroupAnimation.createChannel(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
var Index,ElementSize:TpvSizeInt;
    Path:TpvUTF8String;
    Interpolation:TpvUTF8String;
    Channel:TpvScene3D.TGroup.TAnimation.PChannel;
    POCAScene3DGroupAnimationChannel:TPOCAScene3DGroupAnimationChannel;
begin
 if aCountArguments=3 then begin
  Path:=POCAGetStringValue(aContext,aArguments^[0]);
  Interpolation:=POCAGetStringValue(aContext,aArguments^[1]);
  ElementSize:=trunc(POCAGetNumberValue(aContext,aArguments^[2]));
  if ElementSize>0 then begin
   Index:=length(fAnimation.fChannels);
   SetLength(fAnimation.fChannels,Index+1);
   Channel:=@fAnimation.fChannels[Index];
   Channel^.SetTarget('pointer/'+Path,-1);
   Channel^.SetInterpolation(Interpolation);
   POCAScene3DGroupAnimationChannel:=TPOCAScene3DGroupAnimationChannel.Create(aContext^.Instance,aContext,nil,nil,false);
   POCAScene3DGroupAnimationChannel.fAnimation:=fAnimation;
   POCAScene3DGroupAnimationChannel.fChannelIndex:=Index;
   POCAScene3DGroupAnimationChannel.fElementSize:=ElementSize;
   POCAScene3DGroupAnimationChannel.fCount:=0;
   result:=POCANewNativeObject(aContext,POCAScene3DGroupAnimationChannel);
  end else begin
   result:=POCAValueNull;
  end;
 end else begin
  result:=POCAValueNull;
 end;
end;

type { TPOCAScene3DGroup }
     TPOCAScene3DGroup=class(TPOCANativeObject)
      private
       fGroup:TpvScene3D.TGroup;
      public
       constructor Create(const aInstance:PPOCAInstance;const aContext:PPOCAContext;const aPrototype,aConstructor:PPOCAValue;const aExpandable:boolean); override;
       destructor Destroy; override;
      published
       function getMaterialID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
       function getCameraID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
       function getMeshID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
       function getNodeID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
       function createAnimation(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
     end;

{ TPOCAScene3DGroup }

constructor TPOCAScene3DGroup.Create(const aInstance:PPOCAInstance;const aContext:PPOCAContext;const aPrototype,aConstructor:PPOCAValue;const aExpandable:boolean);
begin
 inherited Create(aInstance,aContext,aPrototype,aConstructor,aExpandable);
end;

destructor TPOCAScene3DGroup.Destroy;
begin
 inherited Destroy;
end;

function TPOCAScene3DGroup.getMaterialID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
begin
 if aCountArguments>0 then begin
  result.Num:=fGroup.fMaterialNameMapArrayIndexHashMap[POCAGetStringValue(aContext,aArguments^[0])];
 end else begin
  result.Num:=-1;
 end;
end;

function TPOCAScene3DGroup.getCameraID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
begin
 if aCountArguments>0 then begin
  result.Num:=fGroup.fCameraNameIndexHashMap[POCAGetStringValue(aContext,aArguments^[0])];
 end else begin
  result.Num:=-1;
 end;
end;

function TPOCAScene3DGroup.getMeshID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
begin
 if aCountArguments>0 then begin
  result.Num:=fGroup.fMeshNameIndexHashMap[POCAGetStringValue(aContext,aArguments^[0])];
 end else begin
  result.Num:=-1;
 end;
end;

function TPOCAScene3DGroup.getNodeID(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
begin
 if aCountArguments>0 then begin
  result.Num:=fGroup.fNodeNameIndexHashMap[POCAGetStringValue(aContext,aArguments^[0])];
 end else begin
  result.Num:=-1;
 end;
end;

function TPOCAScene3DGroup.createAnimation(const aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TpvInt32):TPOCAValue;
var Animation:TpvScene3D.TGroup.TAnimation;
    POCAScene3DGroupAnimation:TPOCAScene3DGroupAnimation;
begin
 Animation:=TpvScene3D.TGroup.TAnimation.Create(fGroup,fGroup.fAnimations.Count);
 try
  if aCountArguments>0 then begin
   Animation.fName:=POCAGetStringValue(aContext,aArguments^[0]);
  end;
 finally
  fGroup.fAnimations.Add(Animation);
 end;
 POCAScene3DGroupAnimation:=TPOCAScene3DGroupAnimation.Create(aContext^.Instance,aContext,nil,nil,false);
 POCAScene3DGroupAnimation.fAnimation:=Animation;
 result:=POCANewNativeObject(aContext,POCAScene3DGroupAnimation);
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

{ TpvScene3D.TDebugPrimitiveVertex }

constructor TpvScene3D.TDebugPrimitiveVertex.Create(const aPosition:TpvVector3;const aColor:TpvVector4);
begin
 Position:=aPosition;
 Color.x:=aColor.x;
 Color.y:=aColor.y;
 Color.z:=aColor.z;
 Color.w:=aColor.w;
end;

{ TpvScene3D.TBaseObject }

constructor TpvScene3D.TBaseObject.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil);
var Current:TpvResource;
begin
 inherited Create(aResourceManager,aParent,aMetaResource);

 if assigned(Parent) then begin
  Current:=Parent;
  while assigned(Current) and not (Current is TpvScene3D) do begin
   Current:=Current.Parent;
  end;
  if assigned(Current) and (Current is TpvScene3D) then begin
   fSceneInstance:=TpvScene3D(Current);
   fSceneInstance.fObjectListLock.Acquire;
   try
    fSceneInstance.fObjectList.Add(self);
   finally
    fSceneInstance.fObjectListLock.Release;
   end;
  end else begin
   fSceneInstance:=nil;
  end;
 end else begin
  fSceneInstance:=nil;
 end;

{if assigned(fSceneInstance) then begin
  ReleaseFrameDelay:=Max(MaxInFlightFrames+1,fSceneInstance.fCountInFlightFrames+1);
 end else begin
  ReleaseFrameDelay:=MaxInFlightFrames+1;
 end;}
 ReleaseFrameDelay:=(MaxInFlightFrames*2)+1;

 fDataLoaded:=false;

 fInLoadData:=false;

 fUploaded:=false;

 fInUpload:=false;

 fAdded:=false;

 fReferenceCounter:=0;

end;

destructor TpvScene3D.TBaseObject.Destroy;
var Index:TpvSizeInt;
begin
 if assigned(fSceneInstance) then begin
  fSceneInstance.fObjectListLock.Acquire;
  try
   Index:=fSceneInstance.fObjectList.IndexOf(self);
   if Index>=0 then begin
    fSceneInstance.fObjectList.Extract(Index);
   end;
  finally
   fSceneInstance.fObjectListLock.Release;
  end;
 end;
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

procedure TpvScene3D.TBaseObject.PrepareDeferredFree;
begin
 Remove;
end;

procedure TpvScene3D.TBaseObject.Remove;
begin
end;

procedure TpvScene3D.TBaseObject.LoadData;
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

procedure TpvScene3D.TBaseObject.DecRefWithNondeferredFree;
begin
 if assigned(self) and (TPasMPInterlocked.Decrement(fReferenceCounter)=0) then begin
  try
   Remove;
  finally
   Free;
  end;
 end;
end;

{ TpvScene3D.TBakedMesh.TTriangle }

class function TpvScene3D.TBakedMesh.TTriangle.Create:TpvScene3D.TBakedMesh.TTriangle;
begin
 FillChar(result,SizeOf(TpvScene3D.TBakedMesh.TTriangle),#0);
end;

procedure TpvScene3D.TBakedMesh.TTriangle.Assign(const aFrom:TpvScene3D.TBakedMesh.TTriangle);
begin
 Positions:=aFrom.Positions;
 Normals:=aFrom.Normals;
 Normal:=aFrom.Normal;
 Flags:=aFrom.Flags;
 MetaFlags:=aFrom.MetaFlags;
end;

function TpvScene3D.TBakedMesh.TTriangle.RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;var aTime,aU,aV:TpvScalar):boolean;
const EPSILON=1e-7;
var e0,e1,p,t,q:TpvVector3;
    Determinant,InverseDeterminant:TpvScalar;
begin
 result:=false;

 e0.x:=Positions[1].x-Positions[0].x;
 e0.y:=Positions[1].y-Positions[0].y;
 e0.z:=Positions[1].z-Positions[0].z;
 e1.x:=Positions[2].x-Positions[0].x;
 e1.y:=Positions[2].y-Positions[0].y;
 e1.z:=Positions[2].z-Positions[0].z;

 p.x:=(aRayDirection.y*e1.z)-(aRayDirection.z*e1.y);
 p.y:=(aRayDirection.z*e1.x)-(aRayDirection.x*e1.z);
 p.z:=(aRayDirection.x*e1.y)-(aRayDirection.y*e1.x);

 Determinant:=(e0.x*p.x)+(e0.y*p.y)+(e0.z*p.z);
 if Determinant<EPSILON then begin
  exit;
 end;

 InverseDeterminant:=1.0/Determinant;

 t.x:=aRayOrigin.x-Positions[0].x;
 t.y:=aRayOrigin.y-Positions[0].y;
 t.z:=aRayOrigin.z-Positions[0].z;

 aU:=((t.x*p.x)+(t.y*p.y)+(t.z*p.z))*InverseDeterminant;
 if (aU<0.0) or (aU>1.0) then begin
  exit;
 end;

 q.x:=(t.y*e0.z)-(t.z*e0.y);
 q.y:=(t.z*e0.x)-(t.x*e0.z);
 q.z:=(t.x*e0.y)-(t.y*e0.x);

 aV:=((aRayDirection.x*q.x)+(aRayDirection.y*q.y)+(aRayDirection.z*q.z))*InverseDeterminant;
 if (aV<0.0) or ((aU+aV)>1.0) then begin
  exit;
 end;

 aTime:=((e1.x*q.x)+(e1.y*q.y)+(e1.z*q.z))*InverseDeterminant;

 result:=true;
end;

{ TpvScene3D.TBakedMesh }

constructor TpvScene3D.TBakedMesh.Create;
begin
 inherited Create;
 fTriangles:=TpvScene3D.TBakedMesh.TTriangles.Create;
end;

destructor TpvScene3D.TBakedMesh.Destroy;
begin
 FreeAndNil(fTriangles);
 inherited Destroy;
end;

procedure TpvScene3D.TBakedMesh.Combine(const aWith:TBakedMesh);
begin
 fTriangles.Add(aWith.fTriangles);
end;

{ TpvScene3D.TPotentiallyVisibleSet.TNode }

constructor TpvScene3D.TPotentiallyVisibleSet.TNode.Create(const aOwner:TPotentiallyVisibleSet;const aParent:TpvScene3D.TPotentiallyVisibleSet.TNode);
begin
 inherited Create;
 fOwner:=aOwner;
 fOwner.fNodes.Add(self);
 fParent:=aParent;
 if assigned(fParent) then begin
  fLevel:=fParent.fLevel+1;
 end else begin
  fLevel:=0;
 end;
 fVisibleNodeList:=nil;
 fMultipleReaderSingleWriterLockState:=0;
end;

destructor TpvScene3D.TPotentiallyVisibleSet.TNode.Destroy;
begin
 if assigned(fParent) then begin
  if fParent.fLeft=self then begin
   fParent.fLeft:=nil;
  end else if fParent.fRight=self then begin
   fParent.fRight:=nil;
  end;
 end;
 if assigned(fLeft) then begin
  fLeft.fParent:=nil;
 end;
 if assigned(fRight) then begin
  fRight.fParent:=nil;
 end;
 FreeAndNil(fVisibleNodeList);
 inherited Destroy;
end;

procedure TpvScene3D.TPotentiallyVisibleSet.TNode.AddVisibleNodeIndex(const aNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex);
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fMultipleReaderSingleWriterLockState);
 try
  if not assigned(fVisibleNodeList) then begin
   fVisibleNodeList:=TpvScene3D.TPotentiallyVisibleSet.TNodeIndexList.Create;
  end;
  fVisibleNodeList.Add(aNodeIndex);
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fMultipleReaderSingleWriterLockState);
 end;
end;

{ TpvScene3D.TPotentiallyVisibleSet.TNodes }

function TpvScene3D_TPotentiallyVisibleSet_TNodes_SortByIndex_CompareFunc(const a,b:TpvPointer):TpvInt32;
begin
 result:=Sign(TpvInt64(TpvScene3D.TPotentiallyVisibleSet.TNode(a).fIndex)-TpvInt64(TpvScene3D.TPotentiallyVisibleSet.TNode(b).fIndex));
end;

procedure TpvScene3D.TPotentiallyVisibleSet.TNodes.SortByIndex;
begin
 if Count>1 then begin
  IndirectIntroSort(PointerToItems,0,Count-1,TpvScene3D_TPotentiallyVisibleSet_TNodes_SortByIndex_CompareFunc);
 end;
end;

{ TpvScene3D.TPotentiallyVisibleSet }

constructor TpvScene3D.TPotentiallyVisibleSet.Create;
begin
 inherited Create;

 fRoot:=nil;

 fSubdivisonMode:=TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode.MeshBVH;

 fSubdivisonOneDimensionSize:=8;

 fManualBoundingBoxes:=TpvScene3D.TPotentiallyVisibleSet.TManualBoundingBoxes.Create;

 fBitmapOneDimensionSize:=0;

 fBitmapSize:=0;

 fBitmap:=nil;

 fNodes:=TpvScene3D.TPotentiallyVisibleSet.TNodes.Create;
 fNodes.OwnsObjects:=true;

//FillChar(fViewNodeIndices,SizeOf(TViewNodeIndices),#$ff);

end;

destructor TpvScene3D.TPotentiallyVisibleSet.Destroy;
begin
 FreeAndNil(fNodes);
 FreeAndNil(fManualBoundingBoxes);
 fRoot:=nil;
 fBitmap:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TPotentiallyVisibleSet.Load(const aStream:TStream);
var NodeIndex,OtherNodeIndex:TpvSizeInt;
    MemoryStream:TMemoryStream;
    FileHeader:TpvScene3D.TPotentiallyVisibleSet.TFileHeader;
    FileNode:TpvScene3D.TPotentiallyVisibleSet.TFileNode;
    Node:TpvScene3D.TPotentiallyVisibleSet.TNode;
begin
 fNodes.Clear;
 fRoot:=nil;
 fBitmapOneDimensionSize:=0;
 fBitmapSize:=0;
 fBitmap:=nil;
 MemoryStream:=TMemoryStream.Create;
 try

  MemoryStream.CopyFrom(aStream,aStream.Size-aStream.Position);
  MemoryStream.Seek(0,soBeginning);

  MemoryStream.ReadBuffer(FileHeader,SizeOf(TpvScene3D.TPotentiallyVisibleSet.TFileHeader));
  if (FileHeader.Signature=TpvScene3D.TPotentiallyVisibleSet.FileSignature) and
     (FileHeader.Version=TpvScene3D.TPotentiallyVisibleSet.FileVersion) and
     (FileHeader.CountNodes>0) then begin

   fBitmapOneDimensionSize:=FileHeader.BitmapOneDimensionSize;

   fBitmapSize:=FileHeader.BitmapSize;

   SetLength(fBitmap,FileHeader.BitmapDataSize);

   MemoryStream.ReadBuffer(fBitmap[0],length(fBitmap)*SizeOf(TpvUInt32));

   for NodeIndex:=0 to FileHeader.CountNodes-1 do begin
    TpvScene3D.TPotentiallyVisibleSet.TNode.Create(self,nil);
   end;

   for NodeIndex:=0 to FileHeader.CountNodes-1 do begin
    MemoryStream.ReadBuffer(FileNode,SizeOf(TpvScene3D.TPotentiallyVisibleSet.TFileNode));
    Node:=fNodes[NodeIndex];
    Node.fAABB:=FileNode.AABB;
    if FileNode.Left>=0 then begin
     Node.fLeft:=fNodes[FileNode.Left];
     Node.fLeft.fParent:=Node;
    end;
    if FileNode.Right>=0 then begin
     Node.fRight:=fNodes[FileNode.Right];
     Node.fRight.fParent:=Node;
    end;
    Node.fSkipCount:=FileNode.SkipCount;
   end;

   fAABB:=fNodes[0].fAABB;

   for NodeIndex:=0 to FileHeader.CountNodes-1 do begin
    for OtherNodeIndex:=NodeIndex+1 to FileHeader.CountNodes-1 do begin
     if GetNodeVisibility(NodeIndex,OtherNodeIndex) then begin
      fNodes[NodeIndex].AddVisibleNodeIndex(OtherNodeIndex);
      fNodes[OtherNodeIndex].AddVisibleNodeIndex(NodeIndex);
     end;
    end;
   end;

  end;

 finally
  FreeAndNil(MemoryStream);
 end;
end;

procedure TpvScene3D.TPotentiallyVisibleSet.Save(const aStream:TStream);
var NodeIndex:TpvSizeInt;
    MemoryStream:TMemoryStream;
    FileHeader:TpvScene3D.TPotentiallyVisibleSet.TFileHeader;
    FileNode:TpvScene3D.TPotentiallyVisibleSet.TFileNode;
    Node:TpvScene3D.TPotentiallyVisibleSet.TNode;
begin
 MemoryStream:=TMemoryStream.Create;
 try

  FileHeader.Signature:=TpvScene3D.TPotentiallyVisibleSet.FileSignature;
  FileHeader.Version:=TpvScene3D.TPotentiallyVisibleSet.FileVersion;
  FileHeader.BitmapOneDimensionSize:=fBitmapOneDimensionSize;
  FileHeader.BitmapSize:=fBitmapSize;
  FileHeader.BitmapDataSize:=length(fBitmap);
  FileHeader.CountNodes:=fNodes.Count;
  MemoryStream.WriteBuffer(FileHeader,SizeOf(TpvScene3D.TPotentiallyVisibleSet.TFileHeader));

  MemoryStream.WriteBuffer(fBitmap[0],length(fBitmap)*SizeOf(TpvUInt32));

  for NodeIndex:=0 to fNodes.Count-1 do begin
   Node:=fNodes[NodeIndex];
   FileNode.AABB:=Node.AABB;
   if assigned(Node.fLeft) then begin
    FileNode.Left:=Node.fLeft.fIndex;
   end else begin
    FileNode.Left:=-1;
   end;
   if assigned(Node.fRight) then begin
    FileNode.Right:=Node.fRight.fIndex;
   end else begin
    FileNode.Right:=-1;
   end;
   FileNode.SkipCount:=Node.fSkipCount;
   MemoryStream.WriteBuffer(FileNode,SizeOf(TpvScene3D.TPotentiallyVisibleSet.TFileNode));
  end;

  MemoryStream.Seek(0,soBeginning);
  aStream.CopyFrom(MemoryStream,MemoryStream.Size);

 finally
  FreeAndNil(MemoryStream);
 end;
end;

function TpvScene3D.TPotentiallyVisibleSet.GetNodeVisibility(const aNodeAIndex,aNodeBIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex):boolean;
var BitIndex:TpvUInt64;
begin
 if (aNodeAIndex<fBitmapOneDimensionSize) and (aNodeBIndex<fBitmapOneDimensionSize) then begin
  BitIndex:=(aNodeAIndex*fBitmapOneDimensionSize)+aNodeBIndex;
  result:=(fBitmap[BitIndex shr 5] and (TpvUInt32(1) shl (BitIndex and 31)))<>0;
 end else begin
  result:=true;
 end;
end;

procedure TpvScene3D.TPotentiallyVisibleSet.SetNodeVisibility(const aNodeAIndex,aNodeBIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;const aVisibility:boolean);
var BitIndex:TpvUInt64;
begin
 if (aNodeAIndex<fBitmapOneDimensionSize) and (aNodeBIndex<fBitmapOneDimensionSize) then begin
  BitIndex:=(aNodeAIndex*fBitmapOneDimensionSize)+aNodeBIndex;
  if aVisibility then begin
   TPasMPInterlocked.BitwiseOr(fBitmap[BitIndex shr 5],TpvUInt32(1) shl (BitIndex and 31));
  end else begin
   TPasMPInterlocked.BitwiseAnd(fBitmap[BitIndex shr 5],not TpvUInt32(TpvUInt32(1) shl (BitIndex and 31)));
  end;
 end;
end;

function TpvScene3D.TPotentiallyVisibleSet.RayCastTriangle(const aUserData:TpvPtrInt;const aRayOrigin,aRayDirection:TpvVector3;out aTime:TpvFloat;out aStop:boolean):boolean;
var u,v:TpvScalar;
begin
 if (aUserData>0) and (aUserData<=fBakedMesh.fTriangles.Count) and (TpvUInt32(aUserData)<>High(TpvUInt32)) then begin
  result:=fBakedMesh.fTriangles[aUserData-1].RayIntersection(aRayOrigin,aRayDirection,aTime,u,v);
 end else begin
  result:=false;
 end;
end;

procedure TpvScene3D.TPotentiallyVisibleSet.NodePairVisibilityCheckRayParallelForJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
var Index,TapAIndex,TapBIndex:TPasMPNativeInt;
    NodeIndexPair:TpvScene3D.TPotentiallyVisibleSet.TNodeIndexPair;
    NodeAIndex,NodeBIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
    NodeA,NodeB:TpvScene3D.TPotentiallyVisibleSet.TNode;
    TapA,TapB,RayOrigin,RayDirection:TpvVector3;
    Time,Len:TpvScalar;
    Hit:boolean;
    Intersection:TpvTriangleBVHIntersection;
begin

 NodeIndexPair:=TpvScene3D.TPotentiallyVisibleSet.TNodeIndexPair(aData^);

 NodeAIndex:=TpvUInt32(NodeIndexPair and TpvUInt32($ffffffff));
 NodeBIndex:=TpvUInt32((NodeIndexPair shr 32) and TpvUInt32($ffffffff));

 if not (GetNodeVisibility(NodeAIndex,NodeBIndex) or GetNodeVisibility(NodeBIndex,NodeAIndex)) then begin

  NodeA:=fNodes[NodeAIndex];
  NodeB:=fNodes[NodeBIndex];

  for Index:=aFromIndex to aToIndex do begin

   TapAIndex:=Index div TpvScene3D.TPotentiallyVisibleSet.CountRayCheckTapPoints;
   TapBIndex:=Index-(TapAIndex*TpvScene3D.TPotentiallyVisibleSet.CountRayCheckTapPoints);

   TapA:=NodeA.fAABB.Min+((NodeA.fAABB.Max-NodeA.fAABB.Min)*TpvScene3D.TPotentiallyVisibleSet.RayCheckTapPoints[TapAIndex]);
   TapB:=NodeB.fAABB.Min+((NodeB.fAABB.Max-NodeB.fAABB.Min)*TpvScene3D.TPotentiallyVisibleSet.RayCheckTapPoints[TapBIndex]);

   Len:=(TapB-TapA).Length;

   RayOrigin:=TapA;
   RayDirection:=(TapB-TapA).Normalize;
   if NodeB.fAABB.RayIntersection(RayOrigin,RayDirection,Time) and (Time>=0.0) then begin
    if not fTriangleBVH.LineIntersection(TapA,RayOrigin+(RayDirection*Time)) then begin
     SetNodeVisibility(NodeAIndex,NodeBIndex,true);
     SetNodeVisibility(NodeBIndex,NodeAIndex,true);
     break;
    end;
   end;

   RayOrigin:=TapB;
   RayDirection:=(TapA-TapB).Normalize;
   if NodeA.fAABB.RayIntersection(RayOrigin,RayDirection,Time) and (Time>=0.0) then begin
    if not fTriangleBVH.LineIntersection(TapB,RayOrigin+(RayDirection*Time)) then begin
     SetNodeVisibility(NodeAIndex,NodeBIndex,true);
     SetNodeVisibility(NodeBIndex,NodeAIndex,true);
     break;
    end;
   end;

   if (GetNodeVisibility(NodeAIndex,NodeBIndex) or GetNodeVisibility(NodeBIndex,NodeAIndex)) then begin
    break;
   end;

  end;

 end;

end;

procedure TpvScene3D.TPotentiallyVisibleSet.NodePairVisibilityCheckParallelForJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
var Index:TPasMPNativeInt;
    NodeAIndex,NodeBIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
    NodeIndexPairList:TpvScene3D.TPotentiallyVisibleSet.TNodeIndexPairList;
    NodeIndexPair:TpvScene3D.TPotentiallyVisibleSet.TNodeIndexPair;
    NodeA,NodeB:TpvScene3D.TPotentiallyVisibleSet.TNode;
    MutuallyVisible:boolean;
begin

 NodeIndexPairList:=TpvScene3D.TPotentiallyVisibleSet.TNodeIndexPairList(aData);

 for Index:=aFromIndex to aToIndex do begin

  NodeIndexPair:=NodeIndexPairList[Index];

  NodeAIndex:=TpvUInt32(NodeIndexPair and TpvUInt32($ffffffff));
  NodeBIndex:=TpvUInt32((NodeIndexPair shr 32) and TpvUInt32($ffffffff));

  if not (GetNodeVisibility(NodeAIndex,NodeBIndex) or GetNodeVisibility(NodeBIndex,NodeAIndex)) then begin

   NodeA:=fNodes[NodeAIndex];
   NodeB:=fNodes[NodeBIndex];

   MutuallyVisible:=NodeA.fAABB.Intersect(NodeB.fAABB);

   if not MutuallyVisible then begin
    fPasMPInstance.Invoke(fPasMPInstance.ParallelFor(@NodeIndexPair,0,TpvScene3D.TPotentiallyVisibleSet.CountRayCheckTapPoints*TpvScene3D.TPotentiallyVisibleSet.CountRayCheckTapPoints,NodePairVisibilityCheckRayParallelForJob,1,PasMPDefaultDepth,nil,0,0));
    MutuallyVisible:=GetNodeVisibility(NodeAIndex,NodeBIndex) or GetNodeVisibility(NodeBIndex,NodeAIndex);
   end;

   if MutuallyVisible then begin
    SetNodeVisibility(NodeAIndex,NodeBIndex,true);
    SetNodeVisibility(NodeBIndex,NodeAIndex,true);
    NodeA.AddVisibleNodeIndex(NodeBIndex);
    NodeB.AddVisibleNodeIndex(NodeAIndex);
   end;

  end;

 end;

end;

procedure TpvScene3D.TPotentiallyVisibleSet.Build(const aBakedMesh:TpvScene3D.TBakedMesh;const aMaxDepth:TpvInt32;const aPasMPInstance:TPasMP);
type TStackItem=record
      Node:TpvScene3D.TPotentiallyVisibleSet.TNode;
      NodeIndex:TpvSizeInt;
      MetaData:TpvInt32;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicStack<TStackItem>;
var TriangleIndex,NodeIndexCounter,Index,OtherIndex,x,y,z:TpvSizeInt;
    BakedTriangle:TpvScene3D.TBakedMesh.PTriangle;
    StackItem,NewStackItem:TStackItem;
    Stack:TStack;
    NodeIndexPairList:TpvScene3D.TPotentiallyVisibleSet.TNodeIndexPairList;
    TemporaryAABB:TpvAABB;
    DynamicAABBTree:TpvBVHDynamicAABBTree;
begin

 fNodes.Clear;

 fRoot:=nil;

 if assigned(aBakedMesh) then begin

  fBakedMesh:=aBakedMesh;
  try

   if assigned(aPasMPInstance) then begin
    fPasMPInstance:=aPasMPInstance;
   end else begin
    fPasMPInstance:=TPasMP.GetGlobalInstance;
   end;

   fTriangleBVH:=TpvTriangleBVH.Create(fPasMPInstance);
   try

    for TriangleIndex:=0 to fBakedMesh.Triangles.Count-1 do begin
     BakedTriangle:=@fBakedMesh.Triangles.ItemArray[TriangleIndex];
     fTriangleBVH.AddTriangle(BakedTriangle^.Positions[0],
                              BakedTriangle^.Positions[1],
                              BakedTriangle^.Positions[2],
                              nil,
                              TriangleIndex,
                              TpvUInt32($ffffffff));
    end;

    fTriangleBVH.Build;

    if fTriangleBVH.CountSkipListNodes>0 then begin

     fAABB.Min:=fTriangleBVH.SkipListNodes[0].Min.Vector3;
     fAABB.Max:=fTriangleBVH.SkipListNodes[0].Max.Vector3;

     case fSubdivisonMode of

      TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode.UniformGrid,
      TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode.ManualZones:begin

       DynamicAABBTree:=TpvBVHDynamicAABBTree.Create;
       try

        case fSubdivisonMode of

         TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode.UniformGrid:begin

          Index:=0;

          for z:=0 to fSubdivisonOneDimensionSize-1 do begin
           TemporaryAABB.Min.z:=FloatLerp(fAABB.Min.z,fAABB.Max.z,z/fSubdivisonOneDimensionSize);
           TemporaryAABB.Max.z:=FloatLerp(fAABB.Min.z,fAABB.Max.z,(z+1)/fSubdivisonOneDimensionSize);
           for y:=0 to fSubdivisonOneDimensionSize-1 do begin
            TemporaryAABB.Min.y:=FloatLerp(fAABB.Min.y,fAABB.Max.y,y/fSubdivisonOneDimensionSize);
            TemporaryAABB.Max.y:=FloatLerp(fAABB.Min.y,fAABB.Max.y,(y+1)/fSubdivisonOneDimensionSize);
            for x:=0 to fSubdivisonOneDimensionSize-1 do begin
             TemporaryAABB.Min.x:=FloatLerp(fAABB.Min.x,fAABB.Max.x,z/fSubdivisonOneDimensionSize);
             TemporaryAABB.Max.x:=FloatLerp(fAABB.Min.x,fAABB.Max.x,(z+1)/fSubdivisonOneDimensionSize);
             DynamicAABBTree.CreateProxy(TemporaryAABB,Index+1);
             inc(Index);
            end;
           end;
          end;

          DynamicAABBTree.Rebuild;

         end;

         TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode.ManualZones:begin

          for Index:=0 to fManualBoundingBoxes.Count-1 do begin
           DynamicAABBTree.CreateProxy(fManualBoundingBoxes.Items[Index],Index+1);
          end;

          DynamicAABBTree.Rebuild;

         end;

         else begin

         end;

        end;

        Stack.Initialize;
        try
         NewStackItem.Node:=nil;
         NewStackItem.NodeIndex:=DynamicAABBTree.Root;
         NewStackItem.MetaData:=0;
         Stack.Push(NewStackItem);
         while Stack.Pop(StackItem) do begin
          if (not assigned(StackItem.Node)) or
             (fSubdivisonMode<>TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode.MeshBVH) or
             (StackItem.Node.fLevel<aMaxDepth) then begin
           NewStackItem.Node:=TpvScene3D.TPotentiallyVisibleSet.TNode.Create(self,StackItem.Node);
           //NewStackItem.Node.fTag:=DynamicAABBTree.Nodes[StackItem.NodeIndex].UserData-1;
           if not assigned(fRoot) then begin
            fRoot:=NewStackItem.Node;
           end;
           if assigned(StackItem.Node) then begin
            if StackItem.MetaData>0 then begin
             StackItem.Node.fRight:=NewStackItem.Node;
            end else begin
             StackItem.Node.fLeft:=NewStackItem.Node;
            end;
           end;
           NewStackItem.Node.fAABB:=DynamicAABBTree.Nodes[StackItem.NodeIndex].AABB;
           if DynamicAABBTree.Nodes[StackItem.NodeIndex].Children[1]>=0 then begin
            NewStackItem.NodeIndex:=DynamicAABBTree.Nodes[StackItem.NodeIndex].Children[1];
            NewStackItem.MetaData:=1;
            Stack.Push(NewStackItem);
           end;
           if DynamicAABBTree.Nodes[StackItem.NodeIndex].Children[0]>=0 then begin
            NewStackItem.NodeIndex:=DynamicAABBTree.Nodes[StackItem.NodeIndex].Children[0];
            NewStackItem.MetaData:=0;
            Stack.Push(NewStackItem);
           end;
          end;
         end;
        finally
         Stack.Finalize;
        end;

       finally
        FreeAndNil(DynamicAABBTree);
       end;

      end;

      else {TpvScene3D.TPotentiallyVisibleSet.TSubdivisonMode.MeshBVH:}begin

       Stack.Initialize;
       try
        NewStackItem.Node:=nil;
        NewStackItem.NodeIndex:=fTriangleBVH.TreeNodeRoot;
        NewStackItem.MetaData:=0;
        Stack.Push(NewStackItem);
        while Stack.Pop(StackItem) do begin
         if (not assigned(StackItem.Node)) or (StackItem.Node.fLevel<aMaxDepth) then begin
          NewStackItem.Node:=TpvScene3D.TPotentiallyVisibleSet.TNode.Create(self,StackItem.Node);
          //NewStackItem.Node.fTag:=0;//fTriangleDynamicAABBTree.Nodes[StackItem.NodeIndex].UserData-1;
          if not assigned(fRoot) then begin
           fRoot:=NewStackItem.Node;
          end;
          if assigned(StackItem.Node) then begin
           if StackItem.MetaData>0 then begin
            StackItem.Node.fRight:=NewStackItem.Node;
           end else begin
            StackItem.Node.fLeft:=NewStackItem.Node;
           end;
          end;
          NewStackItem.Node.fAABB:=fTriangleBVH.TreeNodes[StackItem.NodeIndex].Bounds;
          if fTriangleBVH.TreeNodes[StackItem.NodeIndex].FirstLeftChild>=0 then begin
           NewStackItem.NodeIndex:=fTriangleBVH.TreeNodes[StackItem.NodeIndex].FirstLeftChild+1;
           NewStackItem.MetaData:=1;
           Stack.Push(NewStackItem);
           NewStackItem.NodeIndex:=fTriangleBVH.TreeNodes[StackItem.NodeIndex].FirstLeftChild+0;
           NewStackItem.MetaData:=0;
           Stack.Push(NewStackItem);
          end;
         end;
        end;
       finally
        Stack.Finalize;
       end;

      end;
     end;

     Stack.Initialize;
     try
      NodeIndexCounter:=0;
      NewStackItem.Node:=fRoot;
      NewStackItem.MetaData:=0;
      Stack.Push(NewStackItem);
      while Stack.Pop(StackItem) do begin
       case StackItem.MetaData of
        0:begin
         StackItem.Node.fIndex:=NodeIndexCounter;
         StackItem.Node.fSkipCount:=0;
         inc(NodeIndexCounter);
         NewStackItem.Node:=StackItem.Node;
         NewStackItem.MetaData:=1;
         Stack.Push(NewStackItem);
         if assigned(StackItem.Node.fRight) then begin
          NewStackItem.Node:=StackItem.Node.fRight;
          NewStackItem.MetaData:=0;
          Stack.Push(NewStackItem);
         end;
         if assigned(StackItem.Node.fLeft) then begin
          NewStackItem.Node:=StackItem.Node.fLeft;
          NewStackItem.MetaData:=0;
          Stack.Push(NewStackItem);
         end;
        end;
        1:begin
         StackItem.Node.fSkipCount:=NodeIndexCounter-StackItem.Node.fIndex;
        end;
       end;
      end;
     finally
      Stack.Finalize;
     end;

     fNodes.SortByIndex;

     fBitmapOneDimensionSize:=fNodes.Count;
     fBitmapSize:=fBitmapOneDimensionSize*fBitmapOneDimensionSize;
     SetLength(fBitmap,(fBitmapSize+31) shr 5);
     if length(fBitmap)>0 then begin
      FillChar(fBitmap[0],length(fBitmap)*SizeOf(TpVUInt32),#0);
     end;

     NodeIndexPairList:=TpvScene3D.TPotentiallyVisibleSet.TNodeIndexPairList.Create;
     try
      //Count:=((fNodes.Count*fNodes.Count) shr 1)-(fNodes.Count shr 1);
      for Index:=0 to fNodes.Count-1 do begin
       for OtherIndex:=Index+1 to fNodes.Count-1 do begin
        NodeIndexPairList.Add((TpvUInt64(OtherIndex) shl 32) or TpvUInt64(Index));
       end;
      end;
      if NodeIndexPairList.Count>0 then begin
       fPasMPInstance.Invoke(fPasMPInstance.ParallelFor(NodeIndexPairList,0,NodeIndexPairList.Count-1,NodePairVisibilityCheckParallelForJob,1,PasMPDefaultDepth,nil,0,0));
      end;
     finally
      FreeAndNil(NodeIndexPairList);
     end;

    end;

   finally
    FreeAndNil(fTriangleBVH);
   end;

  finally
   fBakedMesh:=nil;
  end;

 end;

end;

function TpvScene3D.TPotentiallyVisibleSet.GetNodeIndexByPosition(const aPosition:TpvVector3):TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
var Index,Count:TpvUInt32;
    Node:TpvScene3D.TPotentiallyVisibleSet.TNode;
begin
 result:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
 Index:=0;
 Count:=fNodes.Count;
 while Index<Count do begin
  Node:=fNodes[Index];
  if Node.fAABB.Contains(aPosition) then begin
   result:=Index;
   inc(Index);
  end else begin
   if Node.fSkipCount>0 then begin
    inc(Index,Node.fSkipCount);
   end else begin
    break;
   end;
  end;
 end;
end;

function TpvScene3D.TPotentiallyVisibleSet.GetNodeIndexByAABB(const aAABB:TpvAABB):TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
var Index,Count:TpvUInt32;
    Node:TpvScene3D.TPotentiallyVisibleSet.TNode;
begin
 result:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
 Index:=0;
 Count:=fNodes.Count;
 while Index<Count do begin
  Node:=fNodes[Index];
  if Node.fAABB.Contains(aAABB) then begin
   result:=Index;
   inc(Index);
  end else begin
   if Node.fSkipCount>0 then begin
    inc(Index,Node.fSkipCount);
   end else begin
    break;
   end;
  end;
 end;
end;

{ TpvScene3D.TImage }

constructor TpvScene3D.TImage.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil);
begin
 inherited Create(aResourceManager,aParent,aMetaResource);
 fResourceDataStream:=TMemoryStream.Create;
 fLock:=TPasMPSpinLock.Create;
 fTexture:=nil;
end;

destructor TpvScene3D.TImage.Destroy;
begin
 Unload;
 FreeAndNil(fTexture);
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
    fSceneInstance.NewImageDescriptorGeneration;
   finally
    fSceneInstance.fImageListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

procedure TpvScene3D.TImage.LoadData;
// 8x8 texture data, since some GPU drivers seems to reject smaller texture sizes like 1x1 textures.
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
var TemporaryPixels:array of TpvUInt32;
    x,y,w,h:TpvInt32;
begin
 if (not fDataLoaded) and not fInLoadData then begin
  fInLoadData:=true;
  try
   if (fReferenceCounter>0) and not fDataLoaded then begin
    fLock.Acquire;
    try
     if (fReferenceCounter>0) and not fDataLoaded then begin
      try
       FreeAndNil(fTexture); // to avoid memory leaks already beforehand
       if assigned(fSceneInstance.fVulkanDevice) then begin
        fTexture:=TpvVulkanTexture.Create(fSceneInstance.fVulkanDevice);
        fTexture.DoFreeDataAfterFinish:=false;
        case fKind of
         TpvScene3D.TImage.TKind.WhiteTexture:begin
          fTexture.LoadFromMemory(VK_FORMAT_R8G8B8A8_UNORM,
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
          fTexture.LoadFromMemory(VK_FORMAT_R8G8B8A8_UNORM,
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
         TpvScene3D.TImage.TKind.DefaultParticleTexture:begin
          w:=64;
          h:=64;
          TemporaryPixels:=nil;
          SetLength(TemporaryPixels,w*h);
          try
           for y:=0 to h-1 do begin
            for x:=0 to w-1 do begin
             TemporaryPixels[(y*w)+x]:=$00ffffff or
                            (TpvUInt32(
                             Min(
                              Max(
                               round(
                                SmoothStep(1.0,
                                           0.25,
                                           TpvVector2.InlineableCreate(
                                            ((x/w)-0.5)*2.0,
                                            ((y/h)-0.5)*2.0
                                           ).Length)*255.0
                                          ),
                                0),
                               255
                              )) shl 24);
            end;
           end;
           fTexture.LoadFromMemory(VK_FORMAT_R8G8B8A8_UNORM,
                                   VK_SAMPLE_COUNT_1_BIT,
                                   w,
                                   h,
                                   0,
                                   0,
                                   1,
                                   0,
                                   [TpvVulkanTextureUsageFlag.General,
                                    TpvVulkanTextureUsageFlag.TransferDst,
                                    TpvVulkanTextureUsageFlag.TransferSrc,
                                    TpvVulkanTextureUsageFlag.Sampled],
                                   @TemporaryPixels[0],
                                   SizeOf(TpvUInt32)*length(TemporaryPixels),
                                   false,
                                   false,
                                   0,
                                   true,
                                   false);
          finally
           TemporaryPixels:=nil;
          end;
         end;
         else begin
          fTexture.LoadFromImage(fResourceDataStream,
                                 true,
                                 false,
                                 true);
         end;
        end;
       end;
      finally
       fDataLoaded:=true;
      end;
     end;
    finally
     fLock.Release;
    end;
   end;
  finally
   fInLoadData:=false;
  end;
 end;
end;

procedure TpvScene3D.TImage.Upload;
var UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
begin
 LoadData;
 if (not fUploaded) and not fInUpload then begin
  fInUpload:=true;
  try
   if (fReferenceCounter>0) and not fUploaded then begin
    fLock.Acquire;
    try
     if (fReferenceCounter>0) and not fUploaded then begin
      if assigned(fTexture) and assigned(fSceneInstance.fVulkanDevice) then begin
       try
        UniversalQueue:=fSceneInstance.fVulkanDevice.UniversalQueue;
        try
         UniversalCommandPool:=TpvVulkanCommandPool.Create(fSceneInstance.fVulkanDevice,
                                                           fSceneInstance.fVulkanDevice.UniversalQueueFamilyIndex,
                                                           TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
         try
          UniversalCommandBuffer:=TpvVulkanCommandBuffer.Create(UniversalCommandPool,
                                                                VK_COMMAND_BUFFER_LEVEL_PRIMARY);
          try
           UniversalFence:=TpvVulkanFence.Create(fSceneInstance.fVulkanDevice);
           try
            fTexture.Finish(UniversalQueue,
                            UniversalCommandBuffer,
                            UniversalFence,
                            UniversalQueue,
                            UniversalCommandBuffer,
                            UniversalFence);
            if assigned(fTexture.Image) then begin
             fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fTexture.Image.Handle,VK_OBJECT_TYPE_IMAGE,'TpvScene3D.TImage["'+trim(fName)+'"].Image');
            end;
            if assigned(fTexture.ImageView) then begin
             fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fTexture.ImageView.Handle,VK_OBJECT_TYPE_IMAGE_VIEW,'TpvScene3D.TImage["'+trim(fName)+'"].ImageView');
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
       finally
        fUploaded:=true;
       end;
      end;
     end;
    finally
     fLock.Release;
    end;
   end;
  finally
   fInUpload:=false;
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
     if assigned(fTexture) then begin
      fTexture.Unload;
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

function TpvScene3D.TImage.BeginLoad(const aStream:TStream):boolean;
begin
 result:=false;
 if assigned(aStream) then begin
  try
   aStream.Seek(0,soBeginning);
   fResourceDataStream.CopyFrom(aStream,aStream.Size);
   LoadData;
   result:=true;
  except
  end;
 end;
end;

function TpvScene3D.TImage.EndLoad:boolean;
begin
 result:=inherited EndLoad;
 if result then begin
  if fSceneInstance.fUploaded then begin
   Upload;
   fSceneInstance.NewImageDescriptorGeneration;
  end;
 end;
end;

function TpvScene3D.TImage.GetHashData:THashData;
begin
 FillChar(result,SizeOf(THashData),#0);
 if fResourceDataStream.Size>0 then begin
  result.MessageDigest:=TpvHashXXHash64.Process(fResourceDataStream.Memory,fResourceDataStream.Size,0);
  Move(fResourceDataStream.Memory^,result.FirstBytes,Min(SizeOf(result.FirstBytes),fResourceDataStream.Size));
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

procedure TpvScene3D.TImage.AssignFromDefaultParticleTexture;
begin
 fName:=#0+'DefaultParticleTexture';
 fKind:=TpvScene3D.TImage.TKind.DefaultParticleTexture;
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

constructor TpvScene3D.TSampler.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil);
begin
 inherited Create(aResourceManager,aParent,aMetaResource);
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
    fSceneInstance.NewImageDescriptorGeneration;
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

procedure TpvScene3D.TSampler.LoadData;
begin
end;

procedure TpvScene3D.TSampler.Upload;
begin
 if not fInUpload then begin
  fInUpload:=true;
  try
   if (fReferenceCounter>0) and not fUploaded then begin
    fLock.Acquire;
    try
     if (fReferenceCounter>0) and not fUploaded then begin
      try
       if assigned(fSceneInstance.fVulkanDevice) then begin
        fSampler:=TpvVulkanSampler.Create(fSceneInstance.fVulkanDevice,
                                          fMagFilter,
                                          fMinFilter,
                                          fMipmapMode,
                                          fAddressModeS,
                                          fAddressModeT,
                                          VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                          0.0,
                                          fSceneInstance.fVulkanDevice.PhysicalDevice.Properties.limits.maxSamplerAnisotropy>1.0,
                                          Max(1.0,fSceneInstance.fVulkanDevice.PhysicalDevice.Properties.limits.maxSamplerAnisotropy),
                                          false,
                                          VK_COMPARE_OP_ALWAYS,
                                          0.0,
                                          65535.0,
                                          VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
                                          false);
        if fSampler.Handle<>VK_NULL_HANDLE then begin
         fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fSampler.Handle,VK_OBJECT_TYPE_SAMPLER,'TpvScene3D.TSampler["'+trim(fName)+'"]');
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
  finally
   fInUpload:=false;
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

constructor TpvScene3D.TTexture.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil);
begin
 inherited Create(aResourceManager,aParent,aMetaResource);

 fImage:=nil;

 fSampler:=nil;

end;

destructor TpvScene3D.TTexture.Destroy;
begin

 Unload;

 if assigned(fImage) then begin
  try
   if assigned(fSceneInstance) then begin
    fSceneInstance.fImageListLock.Acquire;
    try
     fImage.DecRef;
    finally
     fSceneInstance.fImageListLock.Release;
    end;
   end;
  finally
   fImage:=nil;
  end;
 end;

 if assigned(fSampler) then begin
  try
   if assigned(fSceneInstance) then begin
    fSceneInstance.fSamplerListLock.Acquire;
    try
     fSampler.DecRef;
    finally
     fSceneInstance.fSamplerListLock.Release;
    end;
   end;
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
    fSceneInstance.NewImageDescriptorGeneration;
    if assigned(fImage) then begin
     try
      if assigned(fSceneInstance) then begin
       fSceneInstance.fImageListLock.Acquire;
       try
        fImage.DecRef;
       finally
        fSceneInstance.fImageListLock.Release;
       end;
      end;
     finally
      fImage:=nil;
     end;
    end;
    if assigned(fSampler) then begin
     try
      if assigned(fSceneInstance) then begin
       fSceneInstance.fSamplerListLock.Acquire;
       try
        fSampler.DecRef;
       finally
        fSceneInstance.fSamplerListLock.Release;
       end;
      end;
     finally
      fSampler:=nil;
     end;
    end;
   finally
    fSceneInstance.fTextureListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

procedure TpvScene3D.TTexture.LoadData;
begin
end;

procedure TpvScene3D.TTexture.Upload;
begin
 if not fInUpload then begin
  fInUpload:=true;
  try
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
  finally
   fInUpload:=false;
  end;
 end;
end;

procedure TpvScene3D.TTexture.Unload;
begin
 fUploaded:=false;
end;

function TpvScene3D.TTexture.GetDescriptorImageInfo(const aSRGB:boolean):TVkDescriptorImageInfo;
begin
 if assigned(fSampler) and (fSampler.fReferenceCounter>0) and assigned(fSampler.fSampler) then begin
  result.Sampler:=fSampler.fSampler.Handle;
 end else begin
  result.Sampler:=VK_NULL_HANDLE;
 end;
 if ASRGB and assigned(fImage) and (fImage.fReferenceCounter>0) and assigned(fImage.fTexture.SRGBImageView) then begin
  result.ImageView:=fImage.fTexture.SRGBImageView.Handle;
 end else if assigned(fImage) and (fImage.fReferenceCounter>0) and assigned(fImage.fTexture.ImageView) then begin
  result.ImageView:=fImage.fTexture.ImageView.Handle;
 end else begin
  result.ImageView:=VK_NULL_HANDLE;
 end;
 if assigned(fImage) and (fImage.fReferenceCounter>0) then begin
  result.ImageLayout:=fImage.fTexture.ImageLayout;
 end else begin
  result.ImageLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 end;
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

 fSceneInstance.fTextureListLock.Acquire;
 try

  fSceneInstance.fImageListLock.Acquire;
  try
   fImage:=fSceneInstance.fWhiteImage;
   fImage.IncRef;
  finally
   fSceneInstance.fImageListLock.Release;
  end;

  fSceneInstance.fSamplerListLock.Acquire;
  try
   fSampler:=fSceneInstance.fDefaultSampler;
   fSampler.IncRef;
  finally
   fSceneInstance.fSamplerListLock.Release;
  end;

 finally
  fSceneInstance.fTextureListLock.Release;
 end;

end;

procedure TpvScene3D.TTexture.AssignFromDefaultNormalMapTexture;
begin

 fName:=#0+'DefaultNormalMapTexture';

 fSceneInstance.fTextureListLock.Acquire;
 try

  fSceneInstance.fImageListLock.Acquire;
  try
   fImage:=fSceneInstance.fDefaultNormalMapImage;
   fImage.IncRef;
  finally
   fSceneInstance.fImageListLock.Release;
  end;

  fSceneInstance.fSamplerListLock.Acquire;
  try
   fSampler:=fSceneInstance.fDefaultSampler;
   fSampler.IncRef;
  finally
   fSceneInstance.fSamplerListLock.Release;
  end;

 finally
  fSceneInstance.fTextureListLock.Release;
 end;

end;

procedure TpvScene3D.TTexture.AssignFromDefaultParticleTexture;
begin

 fName:=#0+'DefaultParticleTexture';

 fSceneInstance.fTextureListLock.Acquire;
 try

  fSceneInstance.fImageListLock.Acquire;
  try
   fImage:=fSceneInstance.fDefaultParticleImage;
   fImage.IncRef;
  finally
   fSceneInstance.fImageListLock.Release;
  end;

  fSceneInstance.fSamplerListLock.Acquire;
  try
   fSampler:=fSceneInstance.fDefaultSampler;
   fSampler.IncRef;
  finally
   fSceneInstance.fSamplerListLock.Release;
  end;

 finally
  fSceneInstance.fTextureListLock.Release;
 end;

end;

procedure TpvScene3D.TTexture.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceTexture:TPasGLTF.TTexture;const aImageMap:TImages;const aSamplerMap:TSamplers);
var TextureBASISUJSONItem:TPasJSONItem;
    TextureBASISUSource:TpvInt64;
begin

 fName:=aSourceTexture.Name;

 fSceneInstance.fTextureListLock.Acquire;
 try

  fSceneInstance.fImageListLock.Acquire;
  try
   TextureBASISUSource:=-1;
   if assigned(aSourceTexture.Extensions) and (aSourceTexture.Extensions is TPasJSONItemObject) then begin
    TextureBASISUJSONItem:=TPasJSONItemObject(aSourceTexture.Extensions).Properties['KHR_texture_basisu'];
    if assigned(TextureBASISUJSONItem) and (TextureBASISUJSONItem is TPasJSONItemObject) then begin
     TextureBASISUSource:=TPasJSON.GetInt64(TPasJSONItemObject(TextureBASISUJSONItem).Properties['source'],-1);
    end;
   end;
   if (TextureBASISUSource>=0) and (TextureBASISUSource<aImageMap.Count) then begin
    fImage:=aImageMap[TextureBASISUSource];
   end else if (aSourceTexture.Source>=0) and (aSourceTexture.Source<aImageMap.Count) then begin
    fImage:=aImageMap[aSourceTexture.Source];
   end else begin
    fImage:=nil;
  //raise EPasGLTFInvalidDocument.Create('Image index out of range');
   end;
   if assigned(fImage) then begin
    fImage.IncRef;
   end;
  finally
   fSceneInstance.fImageListLock.Release;
  end;

  fSceneInstance.fSamplerListLock.Acquire;
  try
   if (aSourceTexture.Sampler>=0) and (aSourceTexture.Sampler<aSamplerMap.Count) then begin
    fSampler:=aSamplerMap[aSourceTexture.Sampler];
   end else begin
    fSampler:=SceneInstance.fDefaultSampler;
  //raise EPasGLTFInvalidDocument.Create('Sampler index out of range');
   end;
   if assigned(fSampler) then begin
    fSampler.IncRef;
   end;
  finally
   fSceneInstance.fSamplerListLock.Release;
  end;


 finally
  fSceneInstance.fTextureListLock.Release;
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

{ TpvScene3D.TMaterial.TData }

function TpvScene3D.TMaterial.TData.GetTextureTransform(const aTextureIndex:TpvScene3D.TTextureIndex):TpvScene3D.TMaterial.TTextureReference.PTransform;
begin
 case aTextureIndex of
  TpvScene3D.TTextureIndex.PBRMetallicRoughnessBaseColorTexture:begin
   result:=@PBRMetallicRoughness.BaseColorTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRMetallicRoughnessMetallicRoughnessTexture:begin
   result:=@PBRMetallicRoughness.MetallicRoughnessTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRSpecularGlossinessDiffuseTexture:begin
   result:=@PBRSpecularGlossiness.DiffuseTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRSpecularGlossinessSpecularGlossinessTexture:begin
   result:=@PBRSpecularGlossiness.SpecularGlossinessTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRUnlitColorTexture:begin
   result:=@PBRMetallicRoughness.BaseColorTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.NormalTexture:begin
   result:=@PBRMetallicRoughness.BaseColorTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.OcclusionTexture:begin
   result:=@OcclusionTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.EmissiveTexture:begin
   result:=@EmissiveTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRSheenColorTexture:begin
   result:=@PBRSheen.ColorTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRSheenRoughnessTexture:begin
   result:=@PBRSheen.RoughnessTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRClearCoatTexture:begin
   result:=@PBRClearCoat.Texture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRClearCoatRoughnessTexture:begin
   result:=@PBRClearCoat.RoughnessTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRClearCoatNormalTexture:begin
   result:=@PBRClearCoat.NormalTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRSpecularSpecularTexture:begin
   result:=@PBRMetallicRoughness.SpecularTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRSpecularSpecularColorTexture:begin
   result:=@PBRMetallicRoughness.SpecularColorTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRIridescenceTexture:begin
   result:=@Iridescence.Texture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRIridescenceThicknessTexture:begin
   result:=@Iridescence.ThicknessTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRTransmissionTexture:begin
   result:=@Transmission.Texture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRVolumeThicknessTexture:begin
   result:=@Volume.ThicknessTexture.Transform;
  end;
  TpvScene3D.TTextureIndex.PBRAnisotropyTexture:begin
   result:=@Anisotropy.AnisotropyTexture.Transform;
  end;
  else begin
   result:=nil;
  end;
 end;
end;

{ TpvScene3D.TMaterial }

constructor TpvScene3D.TMaterial.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil);
begin
 inherited Create(aResourceManager,aParent,aMetaResource);

 fData:=DefaultData;

 fLock:=TPasMPSpinLock.Create;

 fGeneration:=0;

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
 if assigned(fData.PBRSheen.ColorTexture.Texture) then begin
  try
   fData.PBRSheen.ColorTexture.Texture.DecRef;
  finally
   fData.PBRSheen.ColorTexture.Texture:=nil;
  end;
 end;
 if assigned(fData.PBRSheen.RoughnessTexture.Texture) then begin
  try
   fData.PBRSheen.RoughnessTexture.Texture.DecRef;
  finally
   fData.PBRSheen.ColorTexture.Texture:=nil;
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
 if assigned(fData.Anisotropy.AnisotropyTexture.Texture) then begin
  try
   fData.Anisotropy.AnisotropyTexture.Texture.DecRef;
  finally
   fData.Anisotropy.AnisotropyTexture.Texture:=nil;
  end;
 end;
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvScene3D.TMaterial.AfterConstruction;
var InFlightFrameIndex:TpvSizeInt;
begin
 inherited AfterConstruction;
 try
  fSceneInstance.fMaterialListLock.Acquire;
  try
   fSceneInstance.fMaterials.Add(self);
   fID:=fSceneInstance.fMaterialIDManager.AllocateID;
   fSceneInstance.fMaterialIDHashMap.Add(fID,self);
   if (fID>0) and (fID<$10000) then begin
    fSceneInstance.fMaterialIDMap[fID]:=self;
    for InFlightFrameIndex:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
     TPasMPInterlocked.BitwiseOr(fSceneInstance.fMaterialIDDirtyMaps[InFlightFrameIndex,fID shr 5],TPasMPUInt32(1) shl (fID and 31));
    end;
   end;
   if fSceneInstance.fMaxMaterialID<fID then begin
    fSceneInstance.fMaxMaterialID:=fID;
   end;
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
var InFlightFrameIndex:TpvSizeInt;
begin
 if fAdded then begin
  try
   fSceneInstance.fMaterialListLock.Acquire;
   try
    fSceneInstance.fMaterials.Remove(self);
    if fSceneInstance.fMaterialHashMap[fData]=self then begin
     fSceneInstance.fMaterialHashMap.Delete(fData);
    end;
    if fID>0 then begin
     if fID<$10000 then begin
      fSceneInstance.fMaterialIDMap[fID]:=nil;
      for InFlightFrameIndex:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
       TPasMPInterlocked.BitwiseOr(fSceneInstance.fMaterialIDDirtyMaps[InFlightFrameIndex,fID shr 5],TPasMPUInt32(1) shl (fID and 31));
      end;
     end;
     if fSceneInstance.fMaterialIDHashMap[fID]=self then begin
      fSceneInstance.fMaterialIDHashMap.Delete(fID);
     end;
     fSceneInstance.fMaterialIDManager.FreeID(fID);
     fID:=0;
    end;
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
    if assigned(fData.PBRSheen.ColorTexture.Texture) then begin
     try
      fData.PBRSheen.ColorTexture.Texture.DecRef;
     finally
      fData.PBRSheen.ColorTexture.Texture:=nil;
     end;
    end;
    if assigned(fData.PBRSheen.RoughnessTexture.Texture) then begin
     try
      fData.PBRSheen.RoughnessTexture.Texture.DecRef;
     finally
      fData.PBRSheen.RoughnessTexture.Texture:=nil;
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
    if assigned(fData.Anisotropy.AnisotropyTexture.Texture) then begin
     try
      fData.Anisotropy.AnisotropyTexture.Texture.DecRef;
     finally
      fData.Anisotropy.AnisotropyTexture.Texture:=nil;
     end;
    end;
    fSceneInstance.NewMaterialDataGeneration;
   finally
    fSceneInstance.fMaterialListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

procedure TpvScene3D.TMaterial.LoadData;
begin
end;

procedure TpvScene3D.TMaterial.Upload;
var UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
begin

 if not fInUpload then begin
  fInUpload:=true;
  try

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

       if assigned(fData.PBRSheen.ColorTexture.Texture) then begin
        fData.PBRSheen.ColorTexture.Texture.Upload;
       end else begin
        fSceneInstance.fWhiteTexture.Upload;
       end;

       if assigned(fData.PBRSheen.RoughnessTexture.Texture) then begin
        fData.PBRSheen.RoughnessTexture.Texture.Upload;
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

       if assigned(fData.Anisotropy.AnisotropyTexture.Texture) then begin
        fData.Anisotropy.AnisotropyTexture.Texture.Upload;
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

  finally
   fInUpload:=false;
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

procedure TpvScene3D.TMaterial.Assign(const aFrom:TMaterial);
begin
 fSceneInstance.fTextureListLock.Acquire;
 try
  fName:=aFrom.fName;
  fData:=aFrom.fData;
  fShaderData:=aFrom.fShaderData;
  begin
   if assigned(fData.EmissiveTexture.Texture) then begin
    fData.EmissiveTexture.Texture.IncRef;
   end;
   if assigned(fData.NormalTexture.Texture) then begin
    fData.NormalTexture.Texture.IncRef;
   end;
   if assigned(fData.OcclusionTexture.Texture) then begin
    fData.OcclusionTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRMetallicRoughness.BaseColorTexture.Texture) then begin
    fData.PBRMetallicRoughness.BaseColorTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture) then begin
    fData.PBRMetallicRoughness.MetallicRoughnessTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRMetallicRoughness.SpecularTexture.Texture) then begin
    fData.PBRMetallicRoughness.SpecularTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
    fData.PBRMetallicRoughness.SpecularColorTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRSpecularGlossiness.DiffuseTexture.Texture) then begin
    fData.PBRSpecularGlossiness.DiffuseTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture) then begin
    fData.PBRSpecularGlossiness.SpecularGlossinessTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRSheen.ColorTexture.Texture) then begin
    fData.PBRSheen.ColorTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRSheen.RoughnessTexture.Texture) then begin
    fData.PBRSheen.RoughnessTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRClearCoat.Texture.Texture) then begin
    fData.PBRClearCoat.Texture.Texture.IncRef;
   end;
   if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
    fData.PBRClearCoat.RoughnessTexture.Texture.IncRef;
   end;
   if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
    fData.PBRClearCoat.NormalTexture.Texture.IncRef;
   end;
   if assigned(fData.Iridescence.Texture.Texture) then begin
    fData.Iridescence.Texture.Texture.IncRef;
   end;
   if assigned(fData.Iridescence.ThicknessTexture.Texture) then begin
    fData.Iridescence.ThicknessTexture.Texture.IncRef;
   end;
   if assigned(fData.Transmission.Texture.Texture) then begin
    fData.Transmission.Texture.Texture.IncRef;
   end;
   if assigned(fData.Volume.ThicknessTexture.Texture) then begin
    fData.Volume.ThicknessTexture.Texture.IncRef;
   end;
   if assigned(fData.Anisotropy.AnisotropyTexture.Texture) then begin
    fData.Anisotropy.AnisotropyTexture.Texture.IncRef;
   end;
  end;
 finally
  fSceneInstance.fTextureListLock.Release;
 end;
end;

procedure TpvScene3D.TMaterial.AssignFromEmpty;
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

 fSceneInstance.fTextureListLock.Acquire;
 try

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
    fData.PBRSheen.RoughnessFactor:=TPasJSON.GetNumber(JSONObject.Properties['sheenRoughnessFactor'],TPasJSON.GetNumber(JSONObject.Properties['intensityFactor'],TPasJSON.GetNumber(JSONObject.Properties['sheenFactor'],0.0)));
    JSONItem:=JSONObject.Properties['sheenColorFactor'];
    if not assigned(JSONItem) then begin
     JSONItem:=JSONObject.Properties['sheenColor'];
    end;
    if assigned(JSONItem) and (JSONItem is TPasJSONItemArray) and (TPasJSONItemArray(JSONItem).Count=3) then begin
     fData.PBRSheen.ColorFactor[0]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0],0.0);
     fData.PBRSheen.ColorFactor[1]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1],0.0);
     fData.PBRSheen.ColorFactor[2]:=TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2],0.0);
    end;
    JSONItem:=JSONObject.Properties['sheenColorTexture'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
     Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
     if (Index>=0) and (Index<aTextureMap.Count) then begin
      fData.PBRSheen.ColorTexture.Texture:=aTextureMap[Index];
      if assigned(fData.PBRSheen.ColorTexture.Texture) then begin
       fData.PBRSheen.ColorTexture.Texture.IncRef;
      end;
     end else begin
      fData.PBRSheen.ColorTexture.Texture:=nil;
     end;
     fData.PBRSheen.ColorTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
     fData.PBRSheen.ColorTexture.Transform.AssignFromGLTF(fData.PBRSheen.ColorTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
    end;
    JSONItem:=JSONObject.Properties['sheenRoughnessTexture'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
     Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
     if (Index>=0) and (Index<aTextureMap.Count) then begin
      fData.PBRSheen.RoughnessTexture.Texture:=aTextureMap[Index];
      if assigned(fData.PBRSheen.RoughnessTexture.Texture) then begin
       fData.PBRSheen.RoughnessTexture.Texture.IncRef;
      end;
     end else begin
      fData.PBRSheen.RoughnessTexture.Texture:=nil;
     end;
     fData.PBRSheen.RoughnessTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
     fData.PBRSheen.RoughnessTexture.Transform.AssignFromGLTF(fData.PBRSheen.RoughnessTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
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

  begin
   JSONItem:=aSourceMaterial.Extensions.Properties['KHR_materials_anisotropy'];
   if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
    JSONObject:=TPasJSONItemObject(JSONItem);
    fData.Anisotropy.Active:=true;
    fData.Anisotropy.AnisotropyStrength:=TPasJSON.GetNumber(JSONObject.Properties['anisotropyStrength'],0.0);
    fData.Anisotropy.AnisotropyRotation:=TPasJSON.GetNumber(JSONObject.Properties['anisotropyRotation'],0.0);
    JSONItem:=JSONObject.Properties['anisotropyTexture'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
     Index:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['index'],-1);
     if (Index>=0) and (Index<aTextureMap.Count) then begin
      fData.Anisotropy.AnisotropyTexture.Texture:=aTextureMap[Index];
      if assigned(fData.Anisotropy.AnisotropyTexture.Texture) then begin
       fData.Anisotropy.AnisotropyTexture.Texture.IncRef;
      end;
     end else begin
      fData.Anisotropy.AnisotropyTexture.Texture:=nil;
     end;
     fData.Anisotropy.AnisotropyTexture.TexCoord:=TPasJSON.GetInt64(TPasJSONItemObject(JSONItem).Properties['texCoord'],0);
     fData.Anisotropy.AnisotropyTexture.Transform.AssignFromGLTF(fData.Anisotropy.AnisotropyTexture,TPasJSONItemObject(JSONItem).Properties['extensions']);
    end;
   end;
  end;

 finally
  fSceneInstance.fTextureListLock.Release;
 end;

 FillShaderData;

end;

procedure TpvScene3D.TMaterial.FillShaderData;
var InFlightFrameIndex:TpvSizeInt;
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
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 10);
    fShaderData.Textures[10]:=(fData.PBRMetallicRoughness.SpecularTexture.Texture.ID and $ffff) or ((fData.PBRMetallicRoughness.SpecularTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[10]:=fData.PBRMetallicRoughness.SpecularTexture.Transform.ToAlignedMatrix3x2;
   end;
   if assigned(fData.PBRMetallicRoughness.SpecularColorTexture.Texture) then begin
    fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 11);
    fShaderData.Textures[11]:=(fData.PBRMetallicRoughness.SpecularColorTexture.Texture.ID and $ffff) or ((fData.PBRMetallicRoughness.SpecularColorTexture.TexCoord and $f) shl 16);
    fShaderData.TextureTransforms[11]:=fData.PBRMetallicRoughness.SpecularColorTexture.Transform.ToAlignedMatrix3x2;
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
  fShaderData.SheenColorFactorSheenRoughnessFactor[0]:=fData.PBRSheen.ColorFactor[0];
  fShaderData.SheenColorFactorSheenRoughnessFactor[1]:=fData.PBRSheen.ColorFactor[1];
  fShaderData.SheenColorFactorSheenRoughnessFactor[2]:=fData.PBRSheen.ColorFactor[2];
  fShaderData.SheenColorFactorSheenRoughnessFactor[3]:=fData.PBRSheen.RoughnessFactor;
  if assigned(fData.PBRSheen.ColorTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 5);
   fShaderData.Textures[5]:=(fData.PBRSheen.ColorTexture.Texture.ID and $ffff) or ((fData.PBRSheen.ColorTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[5]:=fData.PBRSheen.ColorTexture.Transform.ToAlignedMatrix3x2;
  end;
  if assigned(fData.PBRSheen.RoughnessTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 6);
   fShaderData.Textures[6]:=(fData.PBRSheen.RoughnessTexture.Texture.ID and $ffff) or ((fData.PBRSheen.RoughnessTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[6]:=fData.PBRSheen.RoughnessTexture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 if fData.PBRClearCoat.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 8);
  fShaderData.ClearcoatFactorClearcoatRoughnessFactor[0]:=fData.PBRClearCoat.Factor;
  fShaderData.ClearcoatFactorClearcoatRoughnessFactor[1]:=fData.PBRClearCoat.RoughnessFactor;
  if assigned(fData.PBRClearCoat.Texture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 7);
   fShaderData.Textures[7]:=(fData.PBRClearCoat.Texture.Texture.ID and $ffff) or ((fData.PBRClearCoat.Texture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[7]:=fData.PBRClearCoat.Texture.Transform.ToAlignedMatrix3x2;
  end;
  if assigned(fData.PBRClearCoat.RoughnessTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 8);
   fShaderData.Textures[8]:=(fData.PBRClearCoat.RoughnessTexture.Texture.ID and $ffff) or ((fData.PBRClearCoat.RoughnessTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[8]:=fData.PBRClearCoat.RoughnessTexture.Transform.ToAlignedMatrix3x2;
  end;
  if assigned(fData.PBRClearCoat.NormalTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 9);
   fShaderData.Textures[9]:=(fData.PBRClearCoat.NormalTexture.Texture.ID and $ffff) or ((fData.PBRClearCoat.NormalTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[9]:=fData.PBRClearCoat.NormalTexture.Transform.ToAlignedMatrix3x2;
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
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 12);
   fShaderData.Textures[12]:=(fData.Iridescence.Texture.Texture.ID and $ffff) or ((fData.Iridescence.Texture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[12]:=fData.Iridescence.Texture.Transform.ToAlignedMatrix3x2;
  end;
  if assigned(fData.Iridescence.ThicknessTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 13);
   fShaderData.Textures[13]:=(fData.Iridescence.ThicknessTexture.Texture.ID and $ffff) or ((fData.Iridescence.ThicknessTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[13]:=fData.Iridescence.ThicknessTexture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 if fData.Transmission.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 11);
  fShaderData.IridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance[1]:=fData.Transmission.Factor;
  if assigned(fData.Transmission.Texture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 14);
   fShaderData.Textures[14]:=(fData.Transmission.Texture.Texture.ID and $ffff) or ((fData.Transmission.Texture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[14]:=fData.Transmission.Texture.Transform.ToAlignedMatrix3x2;
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
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 15);
   fShaderData.Textures[15]:=(fData.Volume.ThicknessTexture.Texture.ID and $ffff) or ((fData.Volume.ThicknessTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[15]:=fData.Volume.ThicknessTexture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 if fData.Anisotropy.Active then begin
  fShaderData.Flags:=fShaderData.Flags or (1 shl 13);
  TpvHalfFloat(pointer(@fShaderData.AnisotropyStrength)^):=fData.Anisotropy.AnisotropyStrength;
  TpvHalfFloat(pointer(@fShaderData.AnisotropyRotation)^):=fData.Anisotropy.AnisotropyRotation;
  if assigned(fData.Anisotropy.AnisotropyTexture.Texture) then begin
   fShaderData.Textures0:=fShaderData.Textures0 or (1 shl 16);
   fShaderData.Textures[16]:=(fData.Anisotropy.AnisotropyTexture.Texture.ID and $ffff) or ((fData.Anisotropy.AnisotropyTexture.TexCoord and $f) shl 16);
   fShaderData.TextureTransforms[16]:=fData.Anisotropy.AnisotropyTexture.Transform.ToAlignedMatrix3x2;
  end;
 end;

 TPasMPInterlocked.Increment(fGeneration);

 if assigned(fSceneInstance) and (fID>0) and (fID<$10000) then begin
  for InFlightFrameIndex:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
   TPasMPInterlocked.BitwiseOr(fSceneInstance.fMaterialIDDirtyMaps[InFlightFrameIndex,fID shr 5],TPasMPUInt32(1) shl (fID and 31));
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
   TpvScene3D.TLightData.TLightType.Point,
   TpvScene3D.TLightData.TLightType.Spot:begin
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
  if Data^.Type_=TpvScene3D.TLightData.TLightType.PrimaryDirectional then begin
   fSceneInstance.fPrimaryShadowMapLightDirection:=Direction;
  end;
  case Data^.Type_ of
   TpvScene3D.TLightData.TLightType.Directional,
   TpvScene3D.TLightData.TLightType.PrimaryDirectional:begin
    AABB.Min:=TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity);
    AABB.Max:=TpvVector3.InlineableCreate(Infinity,Infinity,Infinity);
   end;
   TpvScene3D.TLightData.TLightType.Point:begin
    AABB.Min:=Position-TpvVector3.InlineableCreate(Radius,Radius,Radius);
    AABB.Max:=Position+TpvVector3.InlineableCreate(Radius,Radius,Radius);
   end;
   TpvScene3D.TLightData.TLightType.Spot:begin
    OppositeLength:=Tan(Data^.fOuterConeAngle{*0.5})*Radius;
    OBB.Center:=fMatrix*TpvVector3.InlineableCreate(0.0,0.0,-Radius*0.5);
    OBB.HalfExtents:=TpvVector3.InlineableCreate(OppositeLength,OppositeLength,Radius*0.5);
    OBB.Matrix:=fMatrix.ToMatrix3x3;
    AABB:=TpvAABB.CreateFromOBB(OBB);
   end;
   else {TpvScene3D.TLightData.TLightType.None:}begin
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
 end;
end;

{ TpvScene3D.TLightBuffer }

constructor TpvScene3D.TLightBuffer.Create(const aSceneInstance:TpvScene3D;const aInFlightFrameIndex:TpvSizeInt);
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 fInFlightFrameIndex:=aInFlightFrameIndex;
 fUploaded:=false;
 fLightTree.Initialize;
 fLightAABBTreeGeneration:=fSceneInstance.fLightAABBTreeGeneration-3;
 fNewLightAABBTreeGeneration:=fSceneInstance.fLightAABBTreeGeneration-2;
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

// fLightMetaInfos:=nil;

   if assigned(fSceneInstance.fVulkanDevice) then begin

    case fSceneInstance.fBufferStreamingMode of

     TBufferStreamingMode.Direct:begin

      fLightItemsVulkanBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                      MaxVisibleLights*SizeOf(TpvScene3D.TLightItem),
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                      [],
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      [TpvVulkanBufferFlag.PersistentMapped]
                                                     );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fLightItemsVulkanBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fLightItemsVulkanBuffer');

      fLightTreeVulkanBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                     (MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode),
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     [],
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     [TpvVulkanBufferFlag.PersistentMapped]
                                                    );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fLightTreeVulkanBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fLightTreeVulkanBuffer');

      fLightMetaInfoVulkanBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                         MaxVisibleLights*SizeOf(TpvScene3D.TLightMetaInfo),
                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                         [],
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         [TpvVulkanBufferFlag.PersistentMapped]
                                                        );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fLightMetaInfoVulkanBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fLightMetaInfoVulkanBuffer');

     end;

     TBufferStreamingMode.Staging:begin

      fLightItemsVulkanBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                      MaxVisibleLights*SizeOf(TpvScene3D.TLightItem),
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                      [],
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                      0,
                                                      0,
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      []
                                                     );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fLightItemsVulkanBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fLightItemsVulkanBuffer');

      fLightTreeVulkanBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                     (MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode),
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     [],
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                     0,
                                                     0,
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     []
                                                    );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fLightTreeVulkanBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fLightTreeVulkanBuffer');

      fLightMetaInfoVulkanBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                         MaxVisibleLights*SizeOf(TpvScene3D.TLightMetaInfo),
                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                         [],
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                         0,
                                                         0,
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         []
                                                        );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fLightMetaInfoVulkanBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.fLightMetaInfoVulkanBuffer');

     end;

     else begin
      Assert(false);
     end;

    end;

   end;
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
   FreeAndNil(fLightMetaInfoVulkanBuffer);
  finally
   fUploaded:=false;
  end;
 end;
end;

procedure TpvScene3D.TLightBuffer.PrepareFrame;
begin
end;

procedure TpvScene3D.TLightBuffer.UploadFrame;
const EmptyGPUSkipListNode:TpvBVHDynamicAABBTree.TSkipListNode=
       (AABBMin:(x:0.0;y:0.0;z:0.0);
        SkipCount:0;
        AABBMax:(x:0.0;y:0.0;z:0.0);
        UserData:TpvUInt32($ffffffff)
       );
begin
 if fUploaded then begin

  if fLightAABBTreeGeneration<>fNewLightAABBTreeGeneration then begin

   fLightAABBTreeGeneration:=fNewLightAABBTreeGeneration;

   if assigned(fSceneInstance.fVulkanDevice) then begin

    case fSceneInstance.fBufferStreamingMode of

     TBufferStreamingMode.Direct:begin

      if fLightItems.Count>0 then begin
       fLightItemsVulkanBuffer.UpdateData(fLightItems.Items[0],0,Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightItem),FlushUpdateData);
      end;
      if fLightTree.Count>0 then begin
       fLightTreeVulkanBuffer.UpdateData(fLightTree.Items[0],0,Min(fLightTree.Count,MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode),FlushUpdateData);
      end else begin
       fLightTreeVulkanBuffer.UpdateData(EmptyGPUSkipListNode,0,SizeOf(TpvBVHDynamicAABBTree.TSkipListNode),FlushUpdateData);
      end;
      if fLightItems.Count>0 then begin
       fLightMetaInfoVulkanBuffer.UpdateData(fLightMetaInfos[0],0,Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightMetaInfo),FlushUpdateData);
      end;

     end;

     TBufferStreamingMode.Staging:begin
      if fLightItems.Count>0 then begin
       fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                         fSceneInstance.fVulkanStagingCommandBuffer,
                                                         fSceneInstance.fVulkanStagingFence,
                                                         fLightItems.Items[0],
                                                         fLightItemsVulkanBuffer,
                                                         0,
                                                         Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightItem));
      end;
      if fLightTree.Count>0 then begin
       fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                         fSceneInstance.fVulkanStagingCommandBuffer,
                                                         fSceneInstance.fVulkanStagingFence,
                                                         fLightTree.Items[0],
                                                         fLightTreeVulkanBuffer,
                                                         0,
                                                         Min(fLightTree.Count,MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode));
      end else begin
       fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                         fSceneInstance.fVulkanStagingCommandBuffer,
                                                         fSceneInstance.fVulkanStagingFence,
                                                         EmptyGPUSkipListNode,
                                                         fLightTreeVulkanBuffer,
                                                         0,
                                                         SizeOf(TpvBVHDynamicAABBTree.TSkipListNode));
      end;
      if fLightItems.Count>0 then begin
       fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                         fSceneInstance.fVulkanStagingCommandBuffer,
                                                         fSceneInstance.fVulkanStagingFence,
                                                         fLightMetaInfos[0],
                                                         fLightMetaInfoVulkanBuffer,
                                                         0,
                                                         Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightMetaInfo));
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

{ TpvScene3D.TDrawChoreographyBatchItem }

constructor TpvScene3D.TDrawChoreographyBatchItem.Create;
begin
 inherited Create;
end;

function TpvScene3D.TDrawChoreographyBatchItem.Clone:TDrawChoreographyBatchItem;
begin
 result:=TDrawChoreographyBatchItem.Create;
 result.fGroup:=fGroup;
 result.fGroupInstance:=fGroupInstance;
 result.fAlphaMode:=fAlphaMode;
 result.fPrimitiveTopology:=fPrimitiveTopology;
 result.fDoubleSided:=fDoubleSided;
 result.fMaterial:=fMaterial;
 result.fNode:=fNode;
 result.fMesh:=fMesh;
 result.fMeshPrimitive:=fMeshPrimitive;
 result.fStartIndex:=fStartIndex;
 result.fCountIndices:=fCountIndices;
end;

class function TpvScene3D.TDrawChoreographyBatchItem.CompareTo(const aCurrent,aOther:TpvScene3D.TDrawChoreographyBatchItem):TpvInt32;
begin
 result:=Sign(TpvInt32(aCurrent.fAlphaMode)-TpvInt32(aOther.fAlphaMode));
 if result=0 then begin
  result:=Sign(TpvInt32(aCurrent.fPrimitiveTopology)-TpvInt32(aOther.fPrimitiveTopology));
  if result=0 then begin
   result:=Sign(TpvInt32(ord(aCurrent.fDoubleSided) and 1)-TpvInt32(ord(aOther.fDoubleSided) and 1));
   if result=0 then begin
    result:=Sign(TpvPtrInt(aCurrent.fMaterial)-TpvPtrInt(aOther.fMaterial));
    if result=0 then begin
     result:=Sign(TpvPtrInt(aCurrent.fNode)-TpvPtrInt(aOther.fNode));
     if result=0 then begin
      result:=Sign(TpvPtrInt(aCurrent.fMesh)-TpvPtrInt(aOther.fMesh));
      if result=0 then begin
       result:=Sign(aCurrent.MeshPrimitive-aOther.MeshPrimitive);
       if result=0 then begin
        result:=Sign(aCurrent.fStartIndex-aOther.fStartIndex);
        if result=0 then begin
         result:=Sign(aCurrent.fCountIndices-aOther.fCountIndices);
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
end;

class function TpvScene3D.TDrawChoreographyBatchItem.IndexOrderCompareTo(const aCurrent,aOther:TpvScene3D.TDrawChoreographyBatchItem):TpvInt32;
begin
 result:=Sign(aCurrent.fStartIndex-aOther.fStartIndex);
 if result=0 then begin
  result:=Sign(aCurrent.fCountIndices-aOther.fCountIndices);
 end;
end;

{ TpvScene3D.TGroup.TDrawChoreographyBatchItems }

procedure TpvScene3D.TDrawChoreographyBatchItems.GroupInstanceClone(const aFrom:TDrawChoreographyBatchItems;const aGroupInstance:TObject;const aIsUnique:Boolean);
var DrawChoreographyBatchItem,NewDrawChoreographyBatchItem:TpvScene3D.TDrawChoreographyBatchItem;
begin
 for DrawChoreographyBatchItem in aFrom do begin
  NewDrawChoreographyBatchItem:=DrawChoreographyBatchItem.Clone;
  try
   NewDrawChoreographyBatchItem.fGroupInstance:=aGroupInstance;
   if assigned(aGroupInstance) then begin
    if assigned(DrawChoreographyBatchItem.Node) then begin
     NewDrawChoreographyBatchItem.fObjectIndex:=TpvScene3D.TGroup.TInstance(aGroupInstance).Nodes[TpvScene3D.TGroup.TNode(DrawChoreographyBatchItem.Node).fIndex].CullObjectID;
    end else begin
     NewDrawChoreographyBatchItem.fObjectIndex:=0;
    end;
    if aIsUnique then begin
     inc(NewDrawChoreographyBatchItem.fStartIndex,TpvScene3D.TGroup.TInstance(aGroupInstance).fVulkanDrawUniqueIndexBufferOffset);
    end else begin
     inc(NewDrawChoreographyBatchItem.fStartIndex,TpvScene3D.TGroup.TInstance(aGroupInstance).fVulkanDrawIndexBufferOffset);
    end;
   end;
  finally
   Add(NewDrawChoreographyBatchItem);
  end;
 end;
end;

procedure TpvScene3D.TDrawChoreographyBatchItems.Sort;
begin
 if Count>1 then begin
  TpvTypedSort<TpvScene3D.TDrawChoreographyBatchItem>.IntroSort(PointerToItems,
                                                                0,
                                                                Count-1,
                                                                TpvScene3D.TDrawChoreographyBatchItem.CompareTo);
 end;
end;

procedure TpvScene3D.TDrawChoreographyBatchItems.IndexOrderSort;
begin
 if Count>1 then begin
  TpvTypedSort<TpvScene3D.TDrawChoreographyBatchItem>.IntroSort(PointerToItems,
                                                                0,
                                                                Count-1,
                                                                TpvScene3D.TDrawChoreographyBatchItem.IndexOrderCompareTo);
 end;
end;

{ TpvScene3D.TVulkanLongTermStaticBufferData }

constructor TpvScene3D.TVulkanLongTermStaticBufferData.Create(const aSceneInstance:TpvScene3D);
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 fVulkanDynamicVertexBuffer:=nil;
 fVulkanStaticVertexBuffer:=nil;
 fVulkanDrawIndexBuffer:=nil;
 fVulkanDrawUniqueIndexBuffer:=nil;
 fVulkanMorphTargetVertexBuffer:=nil;
 fVulkanJointBlockBuffer:=nil;
 fVulkanComputeDescriptorPool:=nil;
 fVulkanComputeDescriptorSet:=nil;
end;

destructor TpvScene3D.TVulkanLongTermStaticBufferData.Destroy;
begin
 FreeAndNil(fVulkanComputeDescriptorSet);
 FreeAndNil(fVulkanComputeDescriptorPool);
 FreeAndNil(fVulkanDynamicVertexBuffer);
 FreeAndNil(fVulkanStaticVertexBuffer);
 FreeAndNil(fVulkanDrawIndexBuffer);
 FreeAndNil(fVulkanDrawUniqueIndexBuffer);
 FreeAndNil(fVulkanMorphTargetVertexBuffer);
 FreeAndNil(fVulkanJointBlockBuffer);
 inherited Destroy;
end;

function TpvScene3D.TVulkanLongTermStaticBufferData.Check:Boolean;
begin
 result:=((not assigned(fVulkanDynamicVertexBuffer)) and
          (not assigned(fVulkanStaticVertexBuffer)) and
          (not assigned(fVulkanDrawIndexBuffer)) and
          (not assigned(fVulkanDrawUniqueIndexBuffer)) and
          (not assigned(fVulkanMorphTargetVertexBuffer)) and
          (not assigned(fVulkanJointBlockBuffer))) or
         ((assigned(fVulkanDynamicVertexBuffer) and ((Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUDynamicVertex))<=fVulkanDynamicVertexBuffer.Size)) and
          (assigned(fVulkanStaticVertexBuffer) and ((Max(1,fSceneInstance.fVulkanStaticVertexBufferData.Count)*SizeOf(TGPUStaticVertex))<=fVulkanStaticVertexBuffer.Size)) and
          (assigned(fVulkanDrawIndexBuffer) and ((Max(1,fSceneInstance.fVulkanDrawIndexBufferData.Count)*SizeOf(TpvUInt32))<=fVulkanDrawIndexBuffer.Size)) and
          (assigned(fVulkanDrawUniqueIndexBuffer) and ((Max(1,fSceneInstance.fVulkanDrawUniqueIndexBufferData.Count)*SizeOf(TpvUInt32))<=fVulkanDrawUniqueIndexBuffer.Size)) and
          (assigned(fVulkanMorphTargetVertexBuffer) and ((Max(1,fSceneInstance.fVulkanMorphTargetVertexBufferData.Count)*SizeOf(TMorphTargetVertex))<=fVulkanMorphTargetVertexBuffer.Size)) and
          (assigned(fVulkanJointBlockBuffer) and ((Max(1,fSceneInstance.fVulkanJointBlockBufferData.Count)*SizeOf(TJointBlock))<=fVulkanJointBlockBuffer.Size)));
end;

procedure TpvScene3D.TVulkanLongTermStaticBufferData.Update;
var GroupInstance:TpvScene3D.TGroup.TInstance;
begin

 if assigned(fSceneInstance) and assigned(fSceneInstance.fVulkanDevice) then begin

  if ((not assigned(fVulkanDynamicVertexBuffer)) or (fVulkanDynamicVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUDynamicVertex)))) or
     ((not assigned(fVulkanStaticVertexBuffer)) or (fVulkanStaticVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanStaticVertexBufferData.Count)*SizeOf(TGPUStaticVertex)))) or
     ((not assigned(fVulkanDrawIndexBuffer)) or (fVulkanDrawIndexBuffer.Size<(Max(1,fSceneInstance.fVulkanDrawIndexBufferData.Count)*SizeOf(TpvUInt32)))) or
     ((not assigned(fVulkanDrawUniqueIndexBuffer)) or (fVulkanDrawUniqueIndexBuffer.Size<(Max(1,fSceneInstance.fVulkanDrawUniqueIndexBufferData.Count)*SizeOf(TpvUInt32)))) or
     ((not assigned(fVulkanMorphTargetVertexBuffer)) or (fVulkanMorphTargetVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanMorphTargetVertexBufferData.Count)*SizeOf(TMorphTargetVertex)))) or
     ((not assigned(fVulkanJointBlockBuffer)) or (fVulkanJointBlockBuffer.Size<(Max(1,fSceneInstance.fVulkanJointBlockBufferData.Count)*SizeOf(TJointBlock)))) then begin

   // Just reupload all buffers in this case, since the size of the buffers has changed (larger than before)
   // or the buffers are not yet allocated

   FreeAndNil(fVulkanComputeDescriptorSet);
   FreeAndNil(fVulkanComputeDescriptorPool);

   if (not assigned(fVulkanDynamicVertexBuffer)) or (fVulkanDynamicVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUDynamicVertex))) then begin
    FreeAndNil(fVulkanDynamicVertexBuffer);
    fVulkanDynamicVertexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                       Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUDynamicVertex),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or fSceneInstance.fAccelerationStructureInputBufferUsageFlags,
                                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                       [],
                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                       0,
                                                       0,
                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       []
                                                      );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanDynamicVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanDynamicVertexBuffer');
   end;
   if fSceneInstance.fVulkanDynamicVertexBufferData.Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanDynamicVertexBufferData.Items[0],
                                                      fVulkanDynamicVertexBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanDynamicVertexBufferData.Count*SizeOf(TGPUDynamicVertex));
   end;

   if (not assigned(fVulkanStaticVertexBuffer)) or (fVulkanStaticVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanStaticVertexBufferData.Count)*SizeOf(TGPUStaticVertex))) then begin
    FreeAndNil(fVulkanStaticVertexBuffer);
    fVulkanStaticVertexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                      Max(1,fSceneInstance.fVulkanStaticVertexBufferData.Count)*SizeOf(TGPUStaticVertex),
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or fSceneInstance.fAccelerationStructureInputBufferUsageFlags,
                                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                      [],
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                      0,
                                                      0,
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      []
                                                     );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanStaticVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanStaticVertexBuffer');
   end;
   if fSceneInstance.fVulkanStaticVertexBufferData.Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanStaticVertexBufferData.Items[0],
                                                      fVulkanStaticVertexBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanStaticVertexBufferData.Count*SizeOf(TGPUStaticVertex));
   end;

   if (not assigned(fVulkanDrawIndexBuffer)) or (fVulkanDrawIndexBuffer.Size<(Max(1,fSceneInstance.fVulkanDrawIndexBufferData.Count)*SizeOf(TpvUInt32))) then begin
    FreeAndNil(fVulkanDrawIndexBuffer);
    fVulkanDrawIndexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                   Max(1,fSceneInstance.fVulkanDrawIndexBufferData.Count)*SizeOf(TpvUInt32),
                                                   TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or fSceneInstance.fAccelerationStructureInputBufferUsageFlags,
                                                   TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                   [],
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                   0,
                                                   0,
                                                   TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   []
                                                  );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanDrawIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanDrawIndexBuffer');
   end;
   if fSceneInstance.fVulkanDrawIndexBufferData.Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanDrawIndexBufferData.Items[0],
                                                      fVulkanDrawIndexBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanDrawIndexBufferData.Count*SizeOf(TpvUInt32));
   end;

   if (not assigned(fVulkanDrawUniqueIndexBuffer)) or (fVulkanDrawUniqueIndexBuffer.Size<(Max(1,fSceneInstance.fVulkanDrawUniqueIndexBufferData.Count)*SizeOf(TpvUInt32))) then begin
    FreeAndNil(fVulkanDrawUniqueIndexBuffer);
    fVulkanDrawUniqueIndexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                         Max(1,fSceneInstance.fVulkanDrawUniqueIndexBufferData.Count)*SizeOf(TpvUInt32),
                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or fSceneInstance.fAccelerationStructureInputBufferUsageFlags,
                                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                         [],
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                         0,
                                                         0,
                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         []
                                                        );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanDrawUniqueIndexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanDrawUniqueIndexBufferData');
   end;
   if fSceneInstance.fVulkanDrawUniqueIndexBufferData.Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanDrawUniqueIndexBufferData.Items[0],
                                                      fVulkanDrawUniqueIndexBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanDrawUniqueIndexBufferData.Count*SizeOf(TpvUInt32));
   end;

   if (not assigned(fVulkanMorphTargetVertexBuffer)) or (fVulkanMorphTargetVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanMorphTargetVertexBufferData.Count)*SizeOf(TMorphTargetVertex))) then begin
    FreeAndNil(fVulkanMorphTargetVertexBuffer);
    fVulkanMorphTargetVertexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                           Max(1,fSceneInstance.fVulkanMorphTargetVertexBufferData.Count)*SizeOf(TMorphTargetVertex),
                                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                           [],
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                           0,
                                                           0,
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                           0,
                                                           0,
                                                           0,
                                                           0,
                                                           []
                                                          );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanMorphTargetVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanMorphTargetVertexBuffer');
   end;
   if fSceneInstance.fVulkanMorphTargetVertexBufferData.Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanMorphTargetVertexBufferData.Items[0],
                                                      fVulkanMorphTargetVertexBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanMorphTargetVertexBufferData.Count*SizeOf(TMorphTargetVertex));
   end;

   if (not assigned(fVulkanJointBlockBuffer)) or (fVulkanJointBlockBuffer.Size<(Max(1,fSceneInstance.fVulkanJointBlockBufferData.Count)*SizeOf(TJointBlock))) then begin
    FreeAndNil(fVulkanJointBlockBuffer);
    fVulkanJointBlockBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                    Max(1,fSceneInstance.fVulkanJointBlockBufferData.Count)*SizeOf(TJointBlock),
                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                    TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                    [],
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                    0,
                                                    0,
                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    []
                                                   );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanJointBlockBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanJointBlockBuffer');
   end;
   if fSceneInstance.fVulkanJointBlockBufferData.Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanJointBlockBufferData.Items[0],
                                                      fVulkanJointBlockBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanJointBlockBufferData.Count*SizeOf(TJointBlock));
   end;

   fSceneInstance.fNewInstanceListLock.Acquire;
   try
    try
     for GroupInstance in fSceneInstance.fNewInstances do begin
      TPasMPInterlocked.Write(GroupInstance.fIsNewInstance,TPasMPBool32(false));
     end;
    finally
     fSceneInstance.fNewInstances.Clear;
    end;
   finally
    fSceneInstance.fNewInstanceListLock.Release;
   end;

   begin

    fVulkanComputeDescriptorPool:=TpvVulkanDescriptorPool.Create(fSceneInstance.fVulkanDevice,
                                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                                 1);
    fVulkanComputeDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,4);
    fVulkanComputeDescriptorPool.Initialize;
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanComputeDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanComputeDescriptorPool');

    fVulkanComputeDescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanComputeDescriptorPool,
                                                               fSceneInstance.fMeshComputeVulkanDescriptorSet0Layout);
    try
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(0,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [{fVulkanVertexBuffer}fVulkanDynamicVertexBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(1,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [fVulkanDrawUniqueIndexBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(2,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [fVulkanMorphTargetVertexBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(3,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [fVulkanJointBlockBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
    finally
     fVulkanComputeDescriptorSet.Flush;
     fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanComputeDescriptorSet.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3D.TVulkanLongTermStaticBufferData.fVulkanComputeDescriptorSet');
    end;

   end;

  end else begin

   fSceneInstance.fNewInstanceListLock.Acquire;
   try

    try

     for GroupInstance in fSceneInstance.fNewInstances do begin

      if TPasMPInterlocked.CompareExchange(GroupInstance.fIsNewInstance,TPasMPBool32(false),TPasMPBool32(true)) then begin

       if GroupInstance.fVulkanVertexBufferCount>0 then begin

        fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                          fSceneInstance.fVulkanStagingCommandBuffer,
                                                          fSceneInstance.fVulkanStagingFence,
                                                          fSceneInstance.fVulkanDynamicVertexBufferData.Items[GroupInstance.fVulkanVertexBufferOffset],
                                                          fVulkanDynamicVertexBuffer,
                                                          GroupInstance.fVulkanVertexBufferOffset*SizeOf(TGPUDynamicVertex),
                                                          GroupInstance.fVulkanVertexBufferCount*SizeOf(TGPUDynamicVertex));

        fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                          fSceneInstance.fVulkanStagingCommandBuffer,
                                                          fSceneInstance.fVulkanStagingFence,
                                                          fSceneInstance.fVulkanStaticVertexBufferData.Items[GroupInstance.fVulkanVertexBufferOffset],
                                                          fVulkanStaticVertexBuffer,
                                                          GroupInstance.fVulkanVertexBufferOffset*SizeOf(TGPUStaticVertex),
                                                          GroupInstance.fVulkanVertexBufferCount*SizeOf(TGPUStaticVertex));

       end;

       if GroupInstance.fVulkanDrawIndexBufferCount>0 then begin
        fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                          fSceneInstance.fVulkanStagingCommandBuffer,
                                                          fSceneInstance.fVulkanStagingFence,
                                                          fSceneInstance.fVulkanDrawIndexBufferData.Items[GroupInstance.fVulkanDrawIndexBufferOffset],
                                                          fVulkanDrawIndexBuffer,
                                                          GroupInstance.fVulkanDrawIndexBufferOffset*SizeOf(TpvUInt32),
                                                          GroupInstance.fVulkanDrawIndexBufferCount*SizeOf(TpvUInt32));
       end;

       if GroupInstance.fVulkanDrawUniqueIndexBufferCount>0 then begin
        fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                          fSceneInstance.fVulkanStagingCommandBuffer,
                                                          fSceneInstance.fVulkanStagingFence,
                                                          fSceneInstance.fVulkanDrawUniqueIndexBufferData.Items[GroupInstance.fVulkanDrawUniqueIndexBufferOffset],
                                                          fVulkanDrawUniqueIndexBuffer,
                                                          GroupInstance.fVulkanDrawUniqueIndexBufferOffset*SizeOf(TpvUInt32),
                                                          GroupInstance.fVulkanDrawUniqueIndexBufferCount*SizeOf(TpvUInt32));
       end;

       if GroupInstance.fVulkanMorphTargetVertexBufferCount>0 then begin
        fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                          fSceneInstance.fVulkanStagingCommandBuffer,
                                                          fSceneInstance.fVulkanStagingFence,
                                                          fSceneInstance.fVulkanMorphTargetVertexBufferData.Items[GroupInstance.fVulkanMorphTargetVertexBufferOffset],
                                                          fVulkanMorphTargetVertexBuffer,
                                                          GroupInstance.fVulkanMorphTargetVertexBufferOffset*SizeOf(TMorphTargetVertex),
                                                          GroupInstance.fVulkanMorphTargetVertexBufferCount*SizeOf(TMorphTargetVertex));
       end;

       if GroupInstance.fVulkanJointBlockBufferCount>0 then begin
        fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                          fSceneInstance.fVulkanStagingCommandBuffer,
                                                          fSceneInstance.fVulkanStagingFence,
                                                          fSceneInstance.fVulkanJointBlockBufferData.Items[GroupInstance.fVulkanJointBlockBufferOffset],
                                                          fVulkanJointBlockBuffer,
                                                          GroupInstance.fVulkanJointBlockBufferOffset*SizeOf(TJointBlock),
                                                          GroupInstance.fVulkanJointBlockBufferCount*SizeOf(TJointBlock));
       end;

      end;

     end;

    finally
     fSceneInstance.fNewInstances.Clear;
    end;

   finally
    fSceneInstance.fNewInstanceListLock.Release;
   end;

  end;

 end;

 fReleaseFrameCounter:=fSceneInstance.fCountInFlightFrames+1; // The decrementing counter is used to determine if the buffer is still in use by the GPU or not

end;

procedure TpvScene3D.TVulkanLongTermStaticBufferData.UpdateReleaseFrameCounter;
begin
 if fReleaseFrameCounter>0 then begin
  dec(fReleaseFrameCounter);
  if fReleaseFrameCounter=0 then begin
   // The buffers are no longer in use by the GPU, so we can free it safely
   FreeAndNil(fVulkanDynamicVertexBuffer);
   FreeAndNil(fVulkanStaticVertexBuffer);
   FreeAndNil(fVulkanDrawIndexBuffer);
   FreeAndNil(fVulkanDrawUniqueIndexBuffer);
   FreeAndNil(fVulkanMorphTargetVertexBuffer);
   FreeAndNil(fVulkanJointBlockBuffer);
  end; 
 end;
end;

{ TpvScene3D.TVulkanLongTermStaticBuffers }

constructor TpvScene3D.TVulkanLongTermStaticBuffers.Create(const aSceneInstance:TpvScene3D);
var Index:TpvSizeInt;
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 for Index:=0 to MaxInFlightFrames-1 do begin
  fBufferDataArray[Index]:=TpvScene3D.TVulkanLongTermStaticBufferData.Create(fSceneInstance);
 end;
 fCurrentIndex:=0;
 fBufferData:=fBufferDataArray[fCurrentIndex];
end;

destructor TpvScene3D.TVulkanLongTermStaticBuffers.Destroy;
var Index:TpvSizeInt;
begin
 for Index:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fBufferDataArray[Index]);
 end;
 inherited Destroy;
end;

procedure TpvScene3D.TVulkanLongTermStaticBuffers.Update;
var Index:TpvSizeInt;
begin
 if not fBufferDataArray[fCurrentIndex].Check then begin
  inc(fCurrentIndex);
  if fCurrentIndex>=MaxInFlightFrames then begin
   fCurrentIndex:=0;
  end;
 end;
 fBufferData:=fBufferDataArray[fCurrentIndex];
 fBufferData.Update;
 for Index:=0 to MaxInFlightFrames-1 do begin
  if Index<>fCurrentIndex then begin
   fBufferDataArray[Index].UpdateReleaseFrameCounter;
  end;
 end;
end;

{ TVulkanShortTermDynamicBufferData } 

constructor TpvScene3D.TVulkanShortTermDynamicBufferData.Create(const aSceneInstance:TpvScene3D;const aInFlightFrameIndex:TpvSizeInt);
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 fInFlightFrameIndex:=aInFlightFrameIndex;
 fVulkanCachedVertexBuffer:=nil;
 fVulkanCachedVertexGenerationBuffer:=nil;
 fVulkanCachedRaytracingVertexBuffer:=nil;
 fVulkanNodeMatricesBuffer:=nil;
 fVulkanMorphTargetVertexWeightsBuffer:=nil;
 fVulkanComputeDescriptorPool:=nil;
 fVulkanComputeDescriptorSet:=nil;
end;

destructor TpvScene3D.TVulkanShortTermDynamicBufferData.Destroy;
begin
 FreeAndNil(fVulkanComputeDescriptorSet);
 FreeAndNil(fVulkanComputeDescriptorPool);
 FreeAndNil(fVulkanCachedVertexBuffer);
 FreeAndNil(fVulkanCachedVertexGenerationBuffer);
 FreeAndNil(fVulkanCachedRaytracingVertexBuffer);
 FreeAndNil(fVulkanNodeMatricesBuffer);
 FreeAndNil(fVulkanMorphTargetVertexWeightsBuffer);
 inherited Destroy;
end;

procedure TpvScene3D.TVulkanShortTermDynamicBufferData.Update;
var GroupInstanceNodeIndex:TpvSizeInt;
    Group:TpvScene3D.TGroup;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    GroupInstanceNode:TpvScene3D.TGroup.TInstance.PNode;
begin

 if assigned(fSceneInstance) and assigned(fSceneInstance.fVulkanDevice) then begin

  if ((not assigned(fVulkanCachedVertexBuffer)) or (fVulkanCachedVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedVertex)))) or
     ((not assigned(fVulkanCachedVertexGenerationBuffer)) or (fVulkanCachedVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedVertexGeneration)))) or
     (fSceneInstance.fHardwareRaytracingSupport and ((not assigned(fVulkanCachedRaytracingVertexBuffer)) or (fVulkanCachedRaytracingVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedRaytracingVertex))))) or
     ((not assigned(fVulkanNodeMatricesBuffer)) or (fVulkanNodeMatricesBuffer.Size<(Max(1,fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvMatrix4x4)))) or
     ((not assigned(fVulkanMorphTargetVertexWeightsBuffer)) or (fVulkanMorphTargetVertexWeightsBuffer.Size<(Max(1,fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvFloat)))) then begin

   // Just reupload all buffers in this case, since the size of the buffers has changed (larger than before)
   // or the buffers are not yet allocated 

   FreeAndNil(fVulkanComputeDescriptorSet);
   FreeAndNil(fVulkanComputeDescriptorPool);

   for Group in fSceneInstance.fGroups do begin
    if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
     for GroupInstance in Group.fInstances do begin
      for GroupInstanceNodeIndex:=0 to length(GroupInstance.fNodes)-1 do begin
       GroupInstanceNode:=@GroupInstance.fNodes[GroupInstanceNodeIndex];
       if GroupInstanceNode^.CacheVerticesDirtyCounter<=fSceneInstance.fCountInFlightFrames then begin
        GroupInstanceNode^.CacheVerticesDirtyCounter:=fSceneInstance.fCountInFlightFrames;
       end;
      end;
     end;
    end;
   end;

   if (not assigned(fVulkanCachedVertexBuffer)) or (fVulkanCachedVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedVertex))) then begin
    FreeAndNil(fVulkanCachedVertexBuffer);
    fVulkanCachedVertexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                      Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedVertex),
                                                      TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or fSceneInstance.fAccelerationStructureInputBufferUsageFlags,
                                                      TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                      [],
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                      0,
                                                      0,
                                                      TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      []
                                                     );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanCachedVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanCachedVertexBuffer');
   end;

   if (not assigned(fVulkanCachedVertexGenerationBuffer)) or (fVulkanCachedVertexGenerationBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedVertexGeneration))) then begin
    FreeAndNil(fVulkanCachedVertexGenerationBuffer);
    fVulkanCachedVertexGenerationBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                                Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedVertexGeneration),
                                                                TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or fSceneInstance.fAccelerationStructureInputBufferUsageFlags,
                                                                TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                [],
                                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                0,
                                                                0,
                                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                0,
                                                                0,
                                                                0,
                                                                0,
                                                                []
                                                               );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanCachedVertexGenerationBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanCachedVertexBufferGeneration');
   end;

   if fSceneInstance.fHardwareRaytracingSupport and ((not assigned(fVulkanCachedRaytracingVertexBuffer)) or (fVulkanCachedRaytracingVertexBuffer.Size<(Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedRaytracingVertex)))) then begin
    FreeAndNil(fVulkanCachedRaytracingVertexBuffer);
    fVulkanCachedRaytracingVertexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                                Max(1,fSceneInstance.fVulkanDynamicVertexBufferData.Count)*SizeOf(TGPUCachedRaytracingVertex),
                                                                TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or fSceneInstance.fAccelerationStructureInputBufferUsageFlags,
                                                                TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                [],
                                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                0,
                                                                0,
                                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                0,
                                                                0,
                                                                0,
                                                                0,
                                                                []
                                                               );
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanCachedRaytracingVertexBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanCachedRaytracingVertexBuffer');
   end;

   if (not assigned(fVulkanNodeMatricesBuffer)) or (fVulkanNodeMatricesBuffer.Size<(Max(1,fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvMatrix4x4))) then begin
    FreeAndNil(fVulkanNodeMatricesBuffer);
    case fSceneInstance.fBufferStreamingMode of
     TBufferStreamingMode.Direct:begin
      fVulkanNodeMatricesBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                        Max(1,fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvMatrix4x4),
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                        [],
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        [TpvVulkanBufferFlag.PersistentMapped]
                                                       );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanNodeMatricesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanNodeMatricesBuffer');
     end;
     TBufferStreamingMode.Staging:begin
      fVulkanNodeMatricesBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                        Max(1,fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvMatrix4x4),
                                                        TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                        TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                        [],
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                        0,
                                                        0,
                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        []
                                                       );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanNodeMatricesBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanNodeMatricesBuffer');
     end;
     else begin
      Assert(false);
     end;
    end;
   end;
   if fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Items[0],
                                                      fVulkanNodeMatricesBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count*SizeOf(TpvMatrix4x4)); 
   end;

   if (not assigned(fVulkanMorphTargetVertexWeightsBuffer)) or (fVulkanMorphTargetVertexWeightsBuffer.Size<(Max(1,fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvFloat))) then begin
    FreeAndNil(fVulkanMorphTargetVertexWeightsBuffer);
    case fSceneInstance.fBufferStreamingMode of
     TBufferStreamingMode.Direct:begin
      fVulkanMorphTargetVertexWeightsBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                                    Max(1,fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvFloat),
                                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                    TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                    [],
                                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    [TpvVulkanBufferFlag.PersistentMapped]
                                                                   );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanMorphTargetVertexWeightsBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanMorphTargetVertexWeightsBuffer');
     end;
     TBufferStreamingMode.Staging:begin
      fVulkanMorphTargetVertexWeightsBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                                    Max(1,fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count)*SizeOf(TpvFloat),
                                                                    TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                    TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                    [],
                                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                    0,
                                                                    0,
                                                                    TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    0,
                                                                    []
                                                                   );
      fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanMorphTargetVertexWeightsBuffer.Handle,VK_OBJECT_TYPE_BUFFER,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanMorphTargetVertexWeightsBuffer');
     end;
     else begin
      Assert(false);
     end;
    end;
   end;
   if fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count>0 then begin
    fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                      fSceneInstance.fVulkanStagingCommandBuffer,
                                                      fSceneInstance.fVulkanStagingFence,
                                                      fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Items[0],
                                                      fVulkanMorphTargetVertexWeightsBuffer,
                                                      0,
                                                      fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count*SizeOf(TpvFloat));
   end;

   begin

    fVulkanComputeDescriptorPool:=TpvVulkanDescriptorPool.Create(fSceneInstance.fVulkanDevice,
                                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                                 1);
    if fSceneInstance.fHardwareRaytracingSupport then begin
     fVulkanComputeDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,5);
    end else begin
     fVulkanComputeDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,4);
    end;
    fVulkanComputeDescriptorPool.Initialize;
    fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanComputeDescriptorPool.Handle,VK_OBJECT_TYPE_DESCRIPTOR_POOL,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanComputeDescriptorPool');

    fVulkanComputeDescriptorSet:=TpvVulkanDescriptorSet.Create(fVulkanComputeDescriptorPool,
                                                               fSceneInstance.fMeshComputeVulkanDescriptorSet1Layout);
    try
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(0,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [fVulkanCachedVertexBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(1,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [fVulkanCachedVertexGenerationBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(2,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [fVulkanNodeMatricesBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
     fVulkanComputeDescriptorSet.WriteToDescriptorSet(3,
                                                      0,
                                                      1,
                                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                      [],
                                                      [fVulkanMorphTargetVertexWeightsBuffer.DescriptorBufferInfo],
                                                      [],
                                                      false);
     if fSceneInstance.fHardwareRaytracingSupport then begin
      fVulkanComputeDescriptorSet.WriteToDescriptorSet(4,
                                                       0,
                                                       1,
                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                       [],
                                                       [fVulkanCachedRaytracingVertexBuffer.DescriptorBufferInfo],
                                                       [],
                                                       false);
     end;
    finally
     fVulkanComputeDescriptorSet.Flush;
     fSceneInstance.fVulkanDevice.DebugUtils.SetObjectName(fVulkanComputeDescriptorSet.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET,'TpvScene3D.TVulkanShortTermDynamicBufferData.fVulkanComputeDescriptorSet');
    end;

   end;

  end else begin

   // Just overwrite all buffers in this case, since it is dynamic data in any case

   case fSceneInstance.fBufferStreamingMode of

    TBufferStreamingMode.Direct:begin

     if fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count>0 then begin
      fVulkanNodeMatricesBuffer.UpdateData(fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Items[0],
                                           0,
                                           fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count*SizeOf(TpvMatrix4x4),
                                           FlushUpdateData);
     end;

     if fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count>0 then begin
      fVulkanMorphTargetVertexWeightsBuffer.UpdateData(fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Items[0],
                                                       0,
                                                       fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count*SizeOf(TpvFloat),
                                                       FlushUpdateData);
     end;

    end;

    TBufferStreamingMode.Staging:begin

     if fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count>0 then begin
      fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                        fSceneInstance.fVulkanStagingCommandBuffer,
                                                        fSceneInstance.fVulkanStagingFence,
                                                        fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Items[0],
                                                        fVulkanNodeMatricesBuffer,
                                                        0,
                                                        fSceneInstance.fVulkanNodeMatricesBufferData[fInFlightFrameIndex].Count*SizeOf(TpvMatrix4x4));
     end;

     if fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count>0 then begin
      fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                        fSceneInstance.fVulkanStagingCommandBuffer,
                                                        fSceneInstance.fVulkanStagingFence,
                                                        fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Items[0],
                                                        fVulkanMorphTargetVertexWeightsBuffer,
                                                        0,
                                                        fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[fInFlightFrameIndex].Count*SizeOf(TpvFloat));
     end;

    end;

    else begin
     Assert(false);
    end;
   end;

  end;

 end;

end;

{ TpvScene3D.TVulkanShortTermDynamicBuffers }

constructor TpvScene3D.TVulkanShortTermDynamicBuffers.Create(const aSceneInstance:TpvScene3D);
var Index:TpvSizeInt;
begin
 inherited Create;
 fSceneInstance:=aSceneInstance;
 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fBufferDataArray[Index]:=TVulkanShortTermDynamicBufferData.Create(fSceneInstance,Index);
 end;
 fCurrentIndex:=0;
 fBufferData:=fBufferDataArray[fCurrentIndex];
end;

destructor TpvScene3D.TVulkanShortTermDynamicBuffers.Destroy;
var Index:TpvSizeInt;
begin
 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  FreeAndNil(fBufferDataArray[Index]);
 end;
 inherited Destroy;
end;

procedure TpvScene3D.TVulkanShortTermDynamicBuffers.Update(const aInFlightFrameIndex:TpvSizeInt);
begin
 fCurrentIndex:=aInFlightFrameIndex; // just the in flight frame index without manual index cycling
 fBufferData:=fBufferDataArray[fCurrentIndex];
 fBufferData.Update;
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

{ TpvScene3D.TGroup.TAnimation.TChannel }

procedure TpvScene3D.TGroup.TAnimation.TChannel.SetTarget(const aTargetPath:TpvUTF8String;const aTargetNode:TpvSizeInt);
var StringPosition,StartStringPosition:TpvSizeInt;
    TargetPointerString,TargetPointerSubString:TpvUTF8String;
    TargetPointerStrings:array of TpvUTF8String;
    ChannelTarget:TAnimation.TChannel.TTarget;
    TextureRawIndex:TpvScene3D.TTextureIndex;
begin

 TargetIndex:=-1;

 TargetSubIndex:=-1;

 if aTargetPath='translation' then begin
  Target:=TAnimation.TChannel.TTarget.Translation;
  TargetIndex:=aTargetNode;
 end else if aTargetPath='rotation' then begin
  Target:=TAnimation.TChannel.TTarget.Rotation;
  TargetIndex:=aTargetNode;
 end else if aTargetPath='scale' then begin
  Target:=TAnimation.TChannel.TTarget.Scale;
  TargetIndex:=aTargetNode;
 end else if aTargetPath='weights' then begin
  Target:=TAnimation.TChannel.TTarget.Weights;
  TargetIndex:=aTargetNode;
 end else if (length(aTargetPath)>=8) and
             (aTargetPath[1]='p') and
             (aTargetPath[2]='o') and
             (aTargetPath[3]='i') and
             (aTargetPath[4]='n') and
             (aTargetPath[5]='t') and
             (aTargetPath[6]='e') and
             (aTargetPath[7]='r') and
             (aTargetPath[8]='/') then begin
  if (length(aTargetPath)>=9) and (aTargetPath[9]='/') then begin
   TargetPointer:=copy(aTargetPath,9,length(aTargetPath)-8);
  end else begin
   TargetPointer:=copy(aTargetPath,8,length(aTargetPath)-7);
  end;
  TargetPointerString:=TargetPointer;
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
      TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
      if TargetPointerStrings[2]='rotation' then begin
       Target:=TAnimation.TChannel.TTarget.PointerNodeRotation;
      end else if TargetPointerStrings[2]='scale' then begin
       Target:=TAnimation.TChannel.TTarget.PointerNodeScale;
      end else if TargetPointerStrings[2]='translation' then begin
       Target:=TAnimation.TChannel.TTarget.PointerNodeTranslation;
      end else if TargetPointerStrings[2]='weights' then begin
       Target:=TAnimation.TChannel.TTarget.PointerNodeWeights;
      end;
     end;
    end else if TargetPointerStrings[0]='meshes' then begin
     if length(TargetPointerStrings)>2 then begin
      TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
      if TargetPointerStrings[2]='weights' then begin
       Target:=TAnimation.TChannel.TTarget.PointerMeshWeights;
      end;
     end;
    end else if TargetPointerStrings[0]='cameras' then begin
     if length(TargetPointerStrings)>3 then begin
      TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
      if TargetPointerStrings[2]='orthographic' then begin
       if TargetPointerStrings[3]='xmag' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag;
       end else if TargetPointerStrings[3]='ymag' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag;
       end else if TargetPointerStrings[3]='zfar' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar;
       end else if TargetPointerStrings[3]='znear' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear;
       end;
      end else if TargetPointerStrings[2]='perspective' then begin
       if TargetPointerStrings[3]='aspectRatio' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio;
       end else if TargetPointerStrings[3]='yfov' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov;
       end else if TargetPointerStrings[3]='zfar' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar;
       end else if TargetPointerStrings[3]='znear' then begin
        Target:=TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear;
       end;
      end;
     end;
    end else if TargetPointerStrings[0]='materials' then begin
     if length(TargetPointerStrings)>2 then begin
      TargetIndex:=StrToIntDef(TargetPointerStrings[1],0);
      if (length(TargetPointerStrings)>4) and
         ((TargetPointerStrings[length(TargetPointerStrings)-3]='extensions') and
          (TargetPointerStrings[length(TargetPointerStrings)-2]='KHR_texture_transform') and
          ((TargetPointerStrings[length(TargetPointerStrings)-1]='offset') or
           (TargetPointerStrings[length(TargetPointerStrings)-1]='scale') or
           (TargetPointerStrings[length(TargetPointerStrings)-1]='rotation'))) then begin
       if TargetPointerStrings[length(TargetPointerStrings)-1]='offset' then begin
        ChannelTarget:=TAnimation.TChannel.TTarget.PointerTextureOffset;
       end else if TargetPointerStrings[length(TargetPointerStrings)-1]='scale' then begin
        ChannelTarget:=TAnimation.TChannel.TTarget.PointerTextureScale;
       end else{if TargetPointerStrings[length(TargetPointerStrings)-1]='rotation' then}begin
        ChannelTarget:=TAnimation.TChannel.TTarget.PointerTextureRotation;
       end;
       TextureRawIndex:=TpvScene3D.TTextureIndex.None;
       case length(TargetPointerStrings) of
        6:begin
         if TargetPointerStrings[2]='emissiveTexture' then begin
          TextureRawIndex:=TpvScene3D.TTextureIndex.EmissiveTexture;
         end else if TargetPointerStrings[2]='normalTexture' then begin
          TextureRawIndex:=TpvScene3D.TTextureIndex.NormalTexture;
         end else if TargetPointerStrings[2]='occlusionTexture' then begin
          TextureRawIndex:=TpvScene3D.TTextureIndex.OcclusionTexture;
         end;
        end;
        7:begin
         if TargetPointerStrings[2]='pbrMetallicRoughness' then begin
          if TargetPointerStrings[3]='baseColorTexture' then begin
           TextureRawIndex:=TpvScene3D.TTextureIndex.PBRMetallicRoughnessBaseColorTexture;
          end else if TargetPointerStrings[3]='metallicRoughnessTexture' then begin
           TextureRawIndex:=TpvScene3D.TTextureIndex.PBRMetallicRoughnessMetallicRoughnessTexture;
          end;
         end;
        end;
        8:begin
         if TargetPointerStrings[2]='extensions' then begin
          if TargetPointerStrings[3]='pbrSpecularGlossiness' then begin
           if TargetPointerStrings[4]='diffuseTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRSpecularGlossinessDiffuseTexture;
           end else if TargetPointerStrings[4]='specularGlossinessTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRSpecularGlossinessSpecularGlossinessTexture;
           end;
          end else if TargetPointerStrings[3]='pbrClearCoat' then begin
           if TargetPointerStrings[4]='clearcoatTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRClearCoatTexture;
           end else if TargetPointerStrings[4]='clearcoatRoughnessTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRClearCoatRoughnessTexture;
           end else if TargetPointerStrings[4]='clearcoatNormalTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRClearCoatNormalTexture;
           end;
          end else if TargetPointerStrings[3]='pbrSheen' then begin
           if TargetPointerStrings[4]='sheenColorTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRSheenColorTexture;
           end else if TargetPointerStrings[4]='sheenRoughnessTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRSheenRoughnessTexture;
           end;
          end else if TargetPointerStrings[3]='pbrSpecular' then begin
           if TargetPointerStrings[4]='specularTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRSpecularSpecularTexture;
           end else if TargetPointerStrings[4]='specularColorTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRSpecularSpecularColorTexture;
           end;
          end else if TargetPointerStrings[3]='pbrIridescence' then begin
           if TargetPointerStrings[4]='iridesceneTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRIridescenceTexture;
           end else if TargetPointerStrings[4]='iridescenceThicknessTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRIridescenceThicknessTexture;
           end;
          end else if TargetPointerStrings[3]='pbrTransmission' then begin
           if TargetPointerStrings[4]='transmissionTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRTransmissionTexture;
           end;
          end else if TargetPointerStrings[3]='pbrVolume' then begin
           if TargetPointerStrings[4]='thicknessTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRVolumeThicknessTexture;
           end;
          end else if TargetPointerStrings[3]='pbrAnisotropy' then begin
           if TargetPointerStrings[4]='anisotropyTexture' then begin
            TextureRawIndex:=TpvScene3D.TTextureIndex.PBRAnisotropyTexture;
           end;
          end;
         end;
        end;
       end;
       if TextureRawIndex<>TpvScene3D.TTextureIndex.None then begin
        Target:=ChannelTarget;
        TargetSubIndex:=TpvSizeInt(TextureRawIndex);
       end;
      end else if TargetPointerStrings[2]='pbrMetallicRoughness' then begin
       if length(TargetPointerStrings)>3 then begin
        if TargetPointerStrings[3]='baseColorFactor' then begin
         Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor;
        end else if TargetPointerStrings[3]='metallicFactor' then begin
         Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor;
        end else if TargetPointerStrings[3]='roughnessFactor' then begin
         Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor;
        end else if TargetPointerStrings[3]='znear' then begin
         Target:=TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear;
        end;
       end;
      end else if TargetPointerStrings[2]='alphaCutoff' then begin
       Target:=TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff;
      end else if TargetPointerStrings[2]='emissiveFactor' then begin
       Target:=TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor;
      end else if TargetPointerStrings[2]='normalTexture' then begin
       if length(TargetPointerStrings)>3 then begin
        if TargetPointerStrings[3]='scale' then begin
         Target:=TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale;
        end;
       end;
      end else if TargetPointerStrings[2]='occlusionTexture' then begin
       if length(TargetPointerStrings)>3 then begin
        if TargetPointerStrings[3]='strength' then begin
         Target:=TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength;
        end;
       end;
      end else if TargetPointerStrings[2]='extensions' then begin
       if length(TargetPointerStrings)>3 then begin
        if TargetPointerStrings[3]='KHR_materials_emissive_strength' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='emissiveStrength' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength;
          end;
         end;
        end else if TargetPointerStrings[3]='KHR_materials_ior' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='ior' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialIOR;
          end;
         end;
        end else if TargetPointerStrings[3]='KHR_materials_transmission' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='transmissionFactor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor;
          end;
         end;
        end else if TargetPointerStrings[3]='KHR_materials_iridescence' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='iridescenceFactor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor;
          end else if TargetPointerStrings[4]='iridescenceIor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor;
          end else if TargetPointerStrings[4]='iridescenceThicknessMinimum' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum;
          end else if TargetPointerStrings[4]='iridescenceThicknessMaximum' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum;
          end;
         end;
        end else if TargetPointerStrings[3]='KHR_materials_volume' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='attenuationColor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor;
          end else if TargetPointerStrings[4]='attenuationDistance' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance;
          end else if TargetPointerStrings[4]='thicknessFactor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor;
          end;
         end;
        end else if TargetPointerStrings[3]='KHR_materials_sheen' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='sheenColorFactor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor;
          end else if TargetPointerStrings[4]='sheenRoughnessFactor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor;
          end;
         end;
        end else if TargetPointerStrings[3]='KHR_materials_specular' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='specularFactor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor;
          end else if TargetPointerStrings[4]='specularColorFactor' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor;
          end;
         end;
        end else if TargetPointerStrings[3]='KHR_materials_transform' then begin
         // TODO
        end else if TargetPointerStrings[3]='KHR_materials_anisotropy' then begin
         if length(TargetPointerStrings)>4 then begin
          if TargetPointerStrings[4]='anisotropyStrength' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength;
          end else if TargetPointerStrings[4]='anisotropyRotation' then begin
           Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation;
          end;
         end;
        end;
       end;
      end;
     end;
    end else if TargetPointerStrings[0]='extensions' then begin
     if (length(TargetPointerStrings)>4) and
        (TargetPointerStrings[1]='KHR_lights_punctual') and
        (TargetPointerStrings[2]='lights') then begin
      TargetIndex:=StrToIntDef(TargetPointerStrings[3],0);
      if TargetPointerStrings[4]='color' then begin
       Target:=TAnimation.TChannel.TTarget.PointerPunctualLightColor;
      end else if TargetPointerStrings[4]='intensity' then begin
       Target:=TAnimation.TChannel.TTarget.PointerPunctualLightIntensity;
      end else if TargetPointerStrings[4]='range' then begin
       Target:=TAnimation.TChannel.TTarget.PointerPunctualLightRange;
      end else if (TargetPointerStrings[4]='spot') and (length(TargetPointerStrings)>5) then begin
       if TargetPointerStrings[5]='innerConeAngle' then begin
        Target:=TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle;
       end else if TargetPointerStrings[5]='outerConeAngle' then begin
        Target:=TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle;
       end;
      end;
     end;
    end;
   end;
  finally
   TargetPointerStrings:=nil;
  end;
 end else begin
  raise EpvScene3D.Create('Non-supported animation channel target path "'+String(aTargetPath)+'"');
 end;

end;

procedure TpvScene3D.TGroup.TAnimation.TChannel.SetInterpolation(const aInterpolation:TpvUTF8String);
begin
 if aInterpolation='cubicspline' then begin
  Interpolation:=TAnimation.TChannel.TInterpolation.CubicSpline;
 end else if aInterpolation='step' then begin
  Interpolation:=TAnimation.TChannel.TInterpolation.Step;
 end else{if aInterpolation='linear' then}begin
  Interpolation:=TAnimation.TChannel.TInterpolation.Linear;
 end;
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
var ChannelIndex,ValueIndex:TPasGLTFSizeInt;
    SourceAnimationChannel:TPasGLTF.TAnimation.TChannel;
    SourceAnimationSampler:TPasGLTF.TAnimation.TSampler;
    DestinationAnimationChannel:TAnimation.PChannel;
    OutputVector2Array:TPasGLTF.TVector2DynamicArray;
    OutputVector3Array:TPasGLTF.TVector3DynamicArray;
    OutputVector4Array:TPasGLTF.TVector4DynamicArray;
    OutputScalarArray:TPasGLTFFloatDynamicArray;
    OutputScalar64Array:TPasGLTFDoubleDynamicArray;
    JSONItem:TPasJSONItem;
begin

 fName:=aSourceAnimation.Name;

 SetLength(fChannels,aSourceAnimation.Channels.Count);

 for ChannelIndex:=0 to aSourceAnimation.Channels.Count-1 do begin

  SourceAnimationChannel:=aSourceAnimation.Channels[ChannelIndex];

  DestinationAnimationChannel:=@fChannels[ChannelIndex];

  DestinationAnimationChannel^.TargetIndex:=-1;

  DestinationAnimationChannel^.TargetSubIndex:=-1;

  if (SourceAnimationChannel.Target.Path='translation') or
     (SourceAnimationChannel.Target.Path='rotation') or
     (SourceAnimationChannel.Target.Path='scale') or
     (SourceAnimationChannel.Target.Path='weights') then begin
   DestinationAnimationChannel^.SetTarget(SourceAnimationChannel.Target.Path,SourceAnimationChannel.Target.Node);
  end else if SourceAnimationChannel.Target.Path='pointer' then begin
   DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.Pointer_;
   if assigned(SourceAnimationChannel.Target.Extensions) then begin
    JSONItem:=SourceAnimationChannel.Target.Extensions.Properties['KHR_animation_pointer'];
    if assigned(JSONItem) and (JSONItem is TPasJSONItemObject) then begin
     DestinationAnimationChannel^.SetTarget('pointer/'+TPasJSON.GetString(TPasJSONItemObject(JSONItem).Properties['pointer'],''),-1);
    end;
   end;
  end else begin
   raise EpvScene3D.Create('Non-supported animation channel target path "'+String(SourceAnimationChannel.Target.Path)+'"');
  end;

  if (SourceAnimationChannel.Sampler>=0) and (SourceAnimationChannel.Sampler<aSourceAnimation.Samplers.Count) then begin
   SourceAnimationSampler:=aSourceAnimation.Samplers[SourceAnimationChannel.Sampler];
   case SourceAnimationSampler.Interpolation of
    TPasGLTF.TAnimation.TSampler.TSamplerInterpolationType.Linear:begin
     DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.Linear;
    end;
    TPasGLTF.TAnimation.TSampler.TSamplerInterpolationType.Step:begin
     DestinationAnimationChannel^.Interpolation:=TAnimation.TChannel.TInterpolation.Step;
    end;
    TPasGLTF.TAnimation.TSampler.TSamplerInterpolationType.CubicSpline:begin
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
    TAnimation.TChannel.TTarget.PointerTextureOffset,
    TAnimation.TChannel.TTarget.PointerTextureScale:begin
     OutputVector2Array:=aSourceDocument.Accessors[SourceAnimationSampler.Output].DecodeAsVector2Array(false);
     try
      SetLength(DestinationAnimationChannel^.OutputVector2Array,length(OutputVector2Array));
      if length(OutputVector2Array)>0 then begin
       Move(OutputVector2Array[0],DestinationAnimationChannel^.OutputVector2Array[0],length(OutputVector2Array)*SizeOf(TpvVector2));
      end;
     finally
      OutputVector2Array:=nil;
     end;
    end;
    TAnimation.TChannel.TTarget.Translation,
    TAnimation.TChannel.TTarget.Scale,
    TAnimation.TChannel.TTarget.PointerNodeTranslation,
    TAnimation.TChannel.TTarget.PointerNodeScale,
    TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor,
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
    TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatRoughnessFactor,
    TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength,
    TAnimation.TChannel.TTarget.PointerMaterialIOR,
    TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum,
    TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum,
    TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor,
    TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance,
    TAnimation.TChannel.TTarget.PointerTextureRotation,
    TAnimation.TChannel.TTarget.PointerPunctualLightIntensity,
    TAnimation.TChannel.TTarget.PointerPunctualLightRange,
    TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle,
    TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle,
    TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength,
    TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation:begin
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
  TPasGLTF.TCamera.TCameraType.None:begin
   fCameraData.Type_:=TpvScene3D.TCameraData.TCameraType.None;
  end;
  TPasGLTF.TCamera.TCameraType.Orthographic:begin
   fCameraData.Type_:=TpvScene3D.TCameraData.TCameraType.Orthographic;
   fCameraData.Orthographic.XMag:=aSourceCamera.Orthographic.XMag;
   fCameraData.Orthographic.YMag:=aSourceCamera.Orthographic.YMag;
   fCameraData.Orthographic.ZNear:=aSourceCamera.Orthographic.ZNear;
   fCameraData.Orthographic.ZFar:=aSourceCamera.Orthographic.ZFar;
  end;
  TPasGLTF.TCamera.TCameraType.Perspective:begin
   fCameraData.Type_:=TpvScene3D.TCameraData.TCameraType.Perspective;
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
 fGroup:=aGroup;
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
 if assigned(fGroup) and assigned(fGroup.fSceneInstance) then begin
  EmptyMaterial:=fGroup.fSceneInstance.fEmptyMaterial;
 end else begin
  EmptyMaterial:=nil;
 end;
 for Index:=0 to length(fPrimitives)-1 do begin
  Primitive:=@fPrimitives[Index];
  if assigned(Primitive^.Material) then begin
   try
    if Primitive^.Material<>EmptyMaterial then begin
     if assigned(fGroup.fSceneInstance.fMaterialListLock) then begin
      fGroup.fSceneInstance.fMaterialListLock.Acquire;
      try
       Primitive^.Material.DecRef;
      finally
       fGroup.fSceneInstance.fMaterialListLock.Release;
      end;
     end;
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
{  if assigned(Primitive^.Material) then begin
    MaterialID:=Primitive^.Material.ID;
   end else begin
    MaterialID:=0;
   end;}
   MaterialID:=Primitive^.MaterialID+1; // +1 because 0 = empty material
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

{  if assigned(Primitive^.Material) then begin
    MaterialID:=Primitive^.Material.ID;
   end else begin
    MaterialID:=0;
   end;}

   MaterialID:=Primitive^.MaterialID+1; // +1 because 0 = empty material

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
    TemporaryLoadedIndices,
    TemporaryIndices,
    TemporaryTriangleIndices:TPasGLTFUInt32DynamicArray;
    SourceMeshPrimitiveMode:TPasGLTF.TMesh.TPrimitive.TMode;
    Normal,Tangent,Bitangent,p1p0,p2p0,TemporaryPosition:TpvVector3;
    p0,p1,p2:PpvVector3;
    t1t0,t2t0:TpvVector2;
    t0,t1,t2:PpvVector2;
    TangentSpaceMatrix:TpvMatrix3x3;
    TangentSpaceQuaternion:TpvQuaternion;
    Vertex:PVertex;
    Area:TPasGLTFFloat;
    HasMorphVertexTargets,
    HasJoints,
    DoNeedCalculateTangents,
    BoundingBoxFirst:boolean;
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

  fBoundingSphere:=TpvSphere.Create(TpvVector3.Origin,Infinity);

  BoundingBoxFirst:=true;

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

      fGroup.fSceneInstance.fMaterialListLock.Acquire;
      try
       if (SourceMeshPrimitive.Material>=0) and (SourceMeshPrimitive.Material<aMaterialMap.Count) then begin
        DestinationMeshPrimitive^.MaterialID:=SourceMeshPrimitive.Material;
        DestinationMeshPrimitive^.Material:=aMaterialMap[SourceMeshPrimitive.Material];
        if assigned(DestinationMeshPrimitive^.Material) then begin
         DestinationMeshPrimitive^.Material.IncRef;
        end;
       end else begin
        DestinationMeshPrimitive^.MaterialID:=-1;
        DestinationMeshPrimitive^.Material:=fGroup.fSceneInstance.fEmptyMaterial;
       end;
      finally
       fGroup.fSceneInstance.fMaterialListLock.Release;
      end;

      HasJoints:=false;

      CountJointBlocks:=0;

      begin
       // Load accessor data
       begin
        AccessorIndex:=SourceMeshPrimitive.Attributes['POSITION'];
        if AccessorIndex>=0 then begin
         TemporaryPositions:=aSourceDocument.Accessors[AccessorIndex].DecodeAsVector3Array(true);
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
        TemporaryLoadedIndices:=aSourceDocument.Accessors[SourceMeshPrimitive.Indices].DecodeAsUInt32Array(false);
       end else begin
        SetLength(TemporaryLoadedIndices,length(TemporaryPositions));
        for IndexIndex:=0 to length(TemporaryLoadedIndices)-1 do begin
         TemporaryLoadedIndices[IndexIndex]:=IndexIndex;
        end;
       end;
       // Convert loops, strips and fans to pure list variants
       case SourceMeshPrimitive.Mode of
        TPasGLTF.TMesh.TPrimitive.TMode.Points:begin
         SourceMeshPrimitiveMode:=TPasGLTF.TMesh.TPrimitive.TMode.Points;
         TemporaryIndices:=TemporaryLoadedIndices;
         TemporaryTriangleIndices:=nil;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.Lines:begin
         SourceMeshPrimitiveMode:=TPasGLTF.TMesh.TPrimitive.TMode.Lines;
         TemporaryIndices:=TemporaryLoadedIndices;
         TemporaryTriangleIndices:=nil;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.LineLoop:begin
         SourceMeshPrimitiveMode:=TPasGLTF.TMesh.TPrimitive.TMode.Lines;
         TemporaryIndices:=nil;
         SetLength(TemporaryIndices,length(TemporaryLoadedIndices)*2);
         for IndexIndex:=0 to length(TemporaryLoadedIndices)-2 do begin
          TemporaryIndices[(IndexIndex*2)+0]:=TemporaryLoadedIndices[IndexIndex+0];
          TemporaryIndices[(IndexIndex*2)+1]:=TemporaryLoadedIndices[IndexIndex+1];
         end;
         if length(TemporaryLoadedIndices)>0 then begin
          TemporaryIndices[((length(TemporaryLoadedIndices)-1)*2)+0]:=TemporaryLoadedIndices[length(TemporaryLoadedIndices)-1];
          TemporaryIndices[((length(TemporaryLoadedIndices)-1)*2)+1]:=0;
         end;
         TemporaryTriangleIndices:=nil;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.LineStrip:begin
         SourceMeshPrimitiveMode:=TPasGLTF.TMesh.TPrimitive.TMode.Lines;
         TemporaryIndices:=nil;
         SetLength(TemporaryIndices,(length(TemporaryLoadedIndices)-1)*2);
         for IndexIndex:=0 to length(TemporaryLoadedIndices)-2 do begin
          TemporaryIndices[(IndexIndex*2)+0]:=TemporaryLoadedIndices[IndexIndex+0];
          TemporaryIndices[(IndexIndex*2)+1]:=TemporaryLoadedIndices[IndexIndex+1];
         end;
         TemporaryTriangleIndices:=nil;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.Triangles:begin
         SourceMeshPrimitiveMode:=TPasGLTF.TMesh.TPrimitive.TMode.Triangles;
         TemporaryIndices:=TemporaryLoadedIndices;
         TemporaryTriangleIndices:=TemporaryIndices;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip:begin
         SourceMeshPrimitiveMode:=TPasGLTF.TMesh.TPrimitive.TMode.Triangles;
         TemporaryIndices:=nil;
         SetLength(TemporaryIndices,(length(TemporaryLoadedIndices)-2)*3);
         for IndexIndex:=0 to length(TemporaryLoadedIndices)-3 do begin
          if (IndexIndex and 1)<>0 then begin
           TemporaryIndices[(IndexIndex*3)+0]:=TemporaryLoadedIndices[IndexIndex+0];
           TemporaryIndices[(IndexIndex*3)+1]:=TemporaryLoadedIndices[IndexIndex+1];
           TemporaryIndices[(IndexIndex*3)+2]:=TemporaryLoadedIndices[IndexIndex+2];
          end else begin
           TemporaryIndices[(IndexIndex*3)+0]:=TemporaryLoadedIndices[IndexIndex+0];
           TemporaryIndices[(IndexIndex*3)+1]:=TemporaryLoadedIndices[IndexIndex+2];
           TemporaryIndices[(IndexIndex*3)+2]:=TemporaryLoadedIndices[IndexIndex+1];
          end;
         end;
         TemporaryTriangleIndices:=TemporaryIndices;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan:begin
         SourceMeshPrimitiveMode:=TPasGLTF.TMesh.TPrimitive.TMode.Triangles;
         TemporaryIndices:=nil;
         SetLength(TemporaryIndices,(length(TemporaryLoadedIndices)-2)*3);
         for IndexIndex:=2 to length(TemporaryLoadedIndices)-1 do begin
          TemporaryIndices[((IndexIndex-1)*3)+0]:=TemporaryLoadedIndices[0];
          TemporaryIndices[((IndexIndex-1)*3)+1]:=TemporaryLoadedIndices[IndexIndex-1];
          TemporaryIndices[((IndexIndex-1)*3)+2]:=TemporaryLoadedIndices[IndexIndex];
         end;
         TemporaryTriangleIndices:=TemporaryIndices;
        end;
        else begin
         SourceMeshPrimitiveMode:=SourceMeshPrimitive.Mode;
         TemporaryIndices:=TemporaryLoadedIndices;
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
           Tangent[0]:=((t1t0[1]*p2p0[0])-(t2t0[1]*p1p0[0]));
           Tangent[1]:=((t1t0[1]*p2p0[1])-(t2t0[1]*p1p0[1]));
           Tangent[2]:=((t1t0[1]*p2p0[2])-(t2t0[1]*p1p0[2]));
           Bitangent[0]:=((t1t0[0]*p2p0[0])-(t2t0[0]*p1p0[0]));
           Bitangent[1]:=((t1t0[0]*p2p0[1])-(t2t0[0]*p1p0[1]));
           Bitangent[2]:=((t1t0[0]*p2p0[2])-(t2t0[0]*p1p0[2]));
           Tangent:=Tangent.Normalize;
           Bitangent:=Bitangent.Normalize;
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
       case SourceMeshPrimitiveMode of
        TPasGLTF.TMesh.TPrimitive.TMode.Points:begin
         DestinationMeshPrimitive^.PrimitiveTopology:=TpvScene3D.TPrimitiveTopology.Points;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.Lines,
        TPasGLTF.TMesh.TPrimitive.TMode.LineLoop,
        TPasGLTF.TMesh.TPrimitive.TMode.LineStrip:begin
         DestinationMeshPrimitive^.PrimitiveTopology:=TpvScene3D.TPrimitiveTopology.Lines;
        end;
        TPasGLTF.TMesh.TPrimitive.TMode.Triangles,
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip,
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan:begin
         DestinationMeshPrimitive^.PrimitiveTopology:=TpvScene3D.TPrimitiveTopology.Triangles;
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
       if length(TemporaryIndices)>0 then begin
        for IndexIndex:=0 to length(TemporaryIndices)-1 do begin
         VertexIndex:=TemporaryIndices[IndexIndex];
         if (VertexIndex>=0) and (VertexIndex<length(TemporaryPositions)) then begin
          if BoundingBoxFirst then begin
           BoundingBoxFirst:=false;
           fBoundingBox.Min[0]:=TemporaryPositions[VertexIndex,0];
           fBoundingBox.Min[1]:=TemporaryPositions[VertexIndex,1];
           fBoundingBox.Min[2]:=TemporaryPositions[VertexIndex,2];
           fBoundingBox.Max[0]:=TemporaryPositions[VertexIndex,0];
           fBoundingBox.Max[1]:=TemporaryPositions[VertexIndex,1];
           fBoundingBox.Max[2]:=TemporaryPositions[VertexIndex,2];
          end else begin
           fBoundingBox.Min[0]:=Min(fBoundingBox.Min[0],TemporaryPositions[VertexIndex,0]);
           fBoundingBox.Min[1]:=Min(fBoundingBox.Min[1],TemporaryPositions[VertexIndex,1]);
           fBoundingBox.Min[2]:=Min(fBoundingBox.Min[2],TemporaryPositions[VertexIndex,2]);
           fBoundingBox.Max[0]:=Max(fBoundingBox.Max[0],TemporaryPositions[VertexIndex,0]);
           fBoundingBox.Max[1]:=Max(fBoundingBox.Max[1],TemporaryPositions[VertexIndex,1]);
           fBoundingBox.Max[2]:=Max(fBoundingBox.Max[2],TemporaryPositions[VertexIndex,2]);
          end;
         end;
        end;
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

        if not BoundingBoxFirst then begin
         for VertexIndex:=0 to length(TemporaryPositions)-1 do begin
          DestinationMeshPrimitiveTargetVertex:=@DestinationMeshPrimitiveTarget^.Vertices[VertexIndex];
          Vertex:=@DestinationMeshPrimitiveVertices[VertexIndex];
          TemporaryPosition.x:=Vertex^.Position[0]+DestinationMeshPrimitiveTargetVertex^.Position[0];
          TemporaryPosition.y:=Vertex^.Position[1]+DestinationMeshPrimitiveTargetVertex^.Position[1];
          TemporaryPosition.z:=Vertex^.Position[2]+DestinationMeshPrimitiveTargetVertex^.Position[2];
          fBoundingBox.Min.x:=Min(fBoundingBox.Min.x,TemporaryPosition.x);
          fBoundingBox.Min.y:=Min(fBoundingBox.Min.y,TemporaryPosition.y);
          fBoundingBox.Min.z:=Min(fBoundingBox.Min.z,TemporaryPosition.z);
          fBoundingBox.Max.x:=Max(fBoundingBox.Max.x,TemporaryPosition.x);
          fBoundingBox.Max.y:=Max(fBoundingBox.Max.y,TemporaryPosition.y);
          fBoundingBox.Max.z:=Max(fBoundingBox.Max.z,TemporaryPosition.z);
         end;
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

 fBoundingSphere:=TpvSphere.CreateFromAABB(fBoundingBox);

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
 fData.fType_:=TpvScene3D.TLightData.TLightType.None;
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
   fData.fType_:=TpvScene3D.TLightData.TLightType.Directional;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end else if TypeString='point' then begin
   fData.fType_:=TpvScene3D.TLightData.TLightType.Point;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountCubeMapShadowMaps;
    inc(fCountCubeMapShadowMaps);
   end;}
  end else if TypeString='spot' then begin
   fData.fType_:=TpvScene3D.TLightData.TLightType.Spot;
{  if fCastShadows then begin
    fShadowMapIndex:=fCountNormalShadowMaps;
    inc(fCountNormalShadowMaps);
   end;}
  end else begin
   fData.fType_:=TpvScene3D.TLightData.TLightType.None;
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

 fFlags:=[];

 fChildren:=TNodes.Create;
 fChildren.OwnsObjects:=false;

 fSplittedChildren:=TNodes.Create;
 fSplittedChildren.OwnsObjects:=false;

 fUsedByScenesList:=TUsedByScenesList.Create;
 fUsedByScenesList.OwnsObjects:=false;

 fUsedJoints.Initialize;

 fMesh:=nil;

 fNodeMeshInstanceIndex:=-1;

 fSkin:=nil;

 fLight:=nil;

 fDrawChoreographyBatchItemIndices.Initialize;

 fDrawChoreographyBatchUniqueItemIndices.Initialize;

end;

destructor TpvScene3D.TGroup.TNode.Destroy;
begin

 fMesh:=nil;

 fSkin:=nil;

 fLight:=nil;

 fUsedJoints.Finalize;

 FreeAndNil(fUsedByScenesList);

 FreeAndNil(fSplittedChildren);

 FreeAndNil(fChildren);

 fChildNodeIndices.Finalize;

 fDrawChoreographyBatchItemIndices.Finalize;

 fDrawChoreographyBatchUniqueItemIndices.Finalize;

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

 fFlags:=[];
 if assigned(fSkin) then begin
  Include(fFlags,TpvScene3D.TGroup.TNode.TNodeFlag.SkinAnimated);
 end;
 if length(fWeights)>0 then begin
  Include(fFlags,TpvScene3D.TGroup.TNode.TNodeFlag.WeightsAnimated);
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

 fAllNodes:=TNodes.Create;
 fAllNodes.OwnsObjects:=false;

 fTransformAnimatedNodes:=TpvScene3D.TGroup.TNodes.Create;
 fTransformAnimatedNodes.OwnsObjects:=false;

 fSkinOrWeightsAnimatedNodes:=TpvScene3D.TGroup.TNodes.Create;
 fSkinOrWeightsAnimatedNodes.OwnsObjects:=false;

 fStaticNodes:=TpvScene3D.TGroup.TNodes.Create;
 fStaticNodes.OwnsObjects:=false;

 fDrawChoreographyBatchItems:=TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchItems.OwnsObjects:=false;

 fDrawChoreographyBatchUniqueItems:=TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchUniqueItems.OwnsObjects:=false;

 fSkipList:=nil;

end;

destructor TpvScene3D.TGroup.TScene.Destroy;
begin
 fSkipList:=nil;
 FreeAndNil(fDrawChoreographyBatchItems);
 FreeAndNil(fDrawChoreographyBatchUniqueItems);
 FreeAndNil(fTransformAnimatedNodes);
 FreeAndNil(fSkinOrWeightsAnimatedNodes);
 FreeAndNil(fStaticNodes);
 FreeAndNil(fAllNodes);
 FreeAndNil(fNodes);
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TScene.ConstructSkipList;
type TStackItem=record
      NodeIndex:TpvSizeInt;
      Pass:TpvSizeInt;
      Level:TpvSizeInt;
      SkipListItemIndex:TpvSizeInt;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicStack<TStackItem>;
var NodeIndex,SkipListItemCount,SkipListItemIndex:TpvSizeInt;
    Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvScene3D.TGroup.TNode;
    SkipListItem:TpvScene3D.TGroup.TScene.PSkipListItem;
begin

 Stack.Initialize;
 try

  for NodeIndex:=fNodes.Count-1 downto 0 do begin
   NewStackItem.NodeIndex:=fNodes[NodeIndex].Index;
   NewStackItem.Pass:=0;
   NewStackItem.Level:=0;
   NewStackItem.SkipListItemIndex:=0;
   Stack.Push(NewStackItem);
  end;

  SkipListItemCount:=0;
  fSkipList:=nil;
  try

   while Stack.Pop(StackItem) do begin

    case StackItem.Pass of

     0:begin

      Node:=fGroup.fNodes[StackItem.NodeIndex];

      SkipListItemIndex:=SkipListItemCount;
      inc(SkipListItemCount);

      if length(fSkipList)<SkipListItemCount then begin
       SetLength(fSkipList,SkipListItemCount+((SkipListItemCount+1) shr 1));
      end;

      SkipListItem:=@fSkipList[SkipListItemIndex];
      SkipListItem^.NodeIndex:=StackItem.NodeIndex;
      SkipListItem^.Level:=StackItem.Level;
      SkipListItem^.SkipCount:=1;

      NewStackItem.NodeIndex:=StackItem.NodeIndex;
      NewStackItem.Pass:=1;
      NewStackItem.Level:=StackItem.Level;
      NewStackItem.SkipListItemIndex:=SkipListItemIndex;
      Stack.Push(NewStackItem);

      for NodeIndex:=Node.fChildren.Count-1 downto 0 do begin
       NewStackItem.NodeIndex:=Node.fChildren[NodeIndex].Index;
       NewStackItem.Pass:=0;
       NewStackItem.Level:=StackItem.Level+1;
       NewStackItem.SkipListItemIndex:=0;
       Stack.Push(NewStackItem);
      end;

     end;

     1:begin
      fSkipList[StackItem.SkipListItemIndex].SkipCount:=SkipListItemCount-StackItem.SkipListItemIndex;
     end;

     else begin
      Assert(false);
     end;

    end;

   end;

  finally
   SetLength(fSkipList,SkipListItemCount);
  end;

 finally
  Stack.Finalize;
 end;

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

constructor TpvScene3D.TGroup.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil);
begin
 inherited Create(aResourceManager,aParent,aMetaResource);

 fHeadless:=false;

 fMaximumCountInstances:=-1;

 fLock:=TPasMPSpinLock.Create;

 fObjects:=TBaseObjects.Create;
 fObjects.OwnsObjects:=false;

 fMaterialsToDuplicate:=TpvScene3D.TGroup.TMaterialsToDuplicate.Create;
 fMaterialsToDuplicate.OwnsObjects:=false;

 fMaterials:=TpvScene3D.TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 fMaterialMap:=nil;

 fMaterialIDMapArrayIndexHashMap:=TpvScene3D.TGroup.TMaterialIDMapArrayIndexHashMap.Create(-1);

 fMaterialNameMapArrayIndexHashMap:=TpvScene3D.TGroup.TMaterialNameMapArrayIndexHashMap.Create(-1);

 fMaterialIDMapArrays:=TpvScene3D.TGroup.TMaterialIDMapArrays.Create;
 fMaterialIDMapArrays.OwnsObjects:=true;

 fAnimations:=TAnimations.Create;
 fAnimations.OwnsObjects:=true;

 fCameras:=TCameras.Create;
 fCameras.OwnsObjects:=true;

 fCameraNameIndexHashMap:=TCameraNameIndexHashMap.Create(-1);

 fMeshes:=TMeshes.Create;
 fMeshes.OwnsObjects:=true;

 fMeshNameIndexHashMap:=TMeshNameIndexHashMap.Create(-1);

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

 fDrawChoreographyBatchCondensedIndices.Initialize;

 fDrawChoreographyBatchCondensedUniqueIndices.Initialize;

 fMorphTargetVertices.Initialize;

 fJointBlocks.Initialize;

 fJointBlockOffsets:=nil;

 fSkinStorageBufferSize:=0;

 fMorphTargetCount:=0;

 fCachedVertexBufferMemoryBarriers:=nil;

 fCulling:=true;

 fDynamicAABBTreeCulling:=false;

 fUsedVisibleDrawNodes:=TUsedVisibleDrawNodes.Create;
 fUsedVisibleDrawNodes.OwnsObjects:=false;

 fDrawChoreographyBatchItems:=TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchItems.OwnsObjects:=true;

 fDrawChoreographyBatchUniqueItems:=TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchUniqueItems.OwnsObjects:=true;

 fInstanceListLock:=TPasMPSlimReaderWriterLock.Create;

 fInstances:=TInstances.Create;
 fInstances.OwnsObjects:=false;

 fNodeNameIndexHashMap:=TpvScene3D.TGroup.TNodeNameIndexHashMap.Create(-1);

 fCameraNodeIndices:=TpvScene3D.TGroup.TCameraNodeIndices.Create;

 fOnNodeFilter:=nil;

end;

destructor TpvScene3D.TGroup.Destroy;
var Material:TpvScene3D.TMaterial;
begin

 Unload;

 while fInstances.Count>0 do begin
  fInstances[fInstances.Count-1].Free;
 end;
 FreeAndNil(fInstances);
 FreeAndNil(fInstanceListLock);

 FreeAndNil(fUsedVisibleDrawNodes);

 FreeAndNil(fDrawChoreographyBatchItems);

 FreeAndNil(fDrawChoreographyBatchUniqueItems);

 FreeAndNil(fScenes);

 FreeAndNil(fLights);

 FreeAndNil(fNodes);

 FreeAndNil(fSkins);

 FreeAndNil(fMeshes);

 FreeAndNil(fCameras);

 FreeAndNil(fAnimations);

 FreeAndNil(fObjects);

 FreeAndNil(fCameraNameIndexHashMap);

 FreeAndNil(fMeshNameIndexHashMap);

 if not (assigned(fSceneInstance) and ((not assigned(fSceneInstance.fMaterials)) or (fSceneInstance.fMaterials.Count=0))) then begin
  if assigned(fSceneInstance) then begin
   fSceneInstance.fMaterialListLock.Acquire;
  end;
  try
   for Material in fMaterials do begin
    Material.DecRef;
   end;
  finally
   if assigned(fSceneInstance) then begin
    fSceneInstance.fMaterialListLock.Release;
   end;
  end;
 end;
 FreeAndNil(fMaterials);

 FreeAndNil(fMaterialsToDuplicate);

 FreeAndNil(fMaterialNameMapArrayIndexHashMap);

 FreeAndNil(fMaterialIDMapArrayIndexHashMap);

 FreeAndNil(fMaterialIDMapArrays);

 fMaterialMap:=nil;

 fDrawChoreographyBatchCondensedIndices.Finalize;

 fDrawChoreographyBatchCondensedUniqueIndices.Finalize;

 fIndices.Finalize;

 fVertices.Finalize;

 fMorphTargetVertices.Finalize;

 fJointBlocks.Finalize;

 fJointBlockOffsets:=nil;

 fCachedVertexBufferMemoryBarriers:=nil;

 FreeAndNil(fNodeNameIndexHashMap);

 FreeAndNil(fCameraNodeIndices);

 FreeAndNil(fLock);

 inherited Destroy;

end;

procedure TpvScene3D.TGroup.AfterConstruction;
begin
 inherited AfterConstruction;
 if not fAdded then begin
  try
   fSceneInstance.fGroupListLock.Acquire;
   try
    fSceneInstance.fGroups.Add(self);
   finally
    fSceneInstance.fGroupListLock.Release;
   end;
  finally
   fAdded:=true;
  end;
 end;
end;

procedure TpvScene3D.TGroup.BeforeDestruction;
begin
 Remove;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TGroup.Remove;
begin
 if fAdded then begin
  try
   fObjects.Clear;
   fSceneInstance.fGroupListLock.Acquire;
   try
    fSceneInstance.fGroups.Remove(self);
   finally
    fSceneInstance.fGroupListLock.Release;
   end;
  finally
   fAdded:=false;
  end;
 end;
end;

procedure TpvScene3D.TGroup.LoadData;
begin
end;

procedure TpvScene3D.TGroup.Upload;
var Index:TpvSizeInt;
    Node:TpvScene3D.TGroup.TNode;
    Mesh:TpvScene3D.TGroup.TMesh;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
    Material:TpvScene3D.TMaterial;
    Instance:TpvScene3D.TGroup.TInstance;
begin
 if not fInUpload then begin

  fInUpload:=true;
  try

   if not fUploaded then begin
    fLock.Acquire;
    try

     if not fUploaded then begin
      try

       if assigned(fSceneInstance.fVulkanDevice) and not fHeadless then begin

        if fVertices.Count=0 then begin
         fVertices.AddNew;
        end;
        if fIndices.Count=0 then begin
         fIndices.Add(0);
        end;
        if fDrawChoreographyBatchCondensedIndices.Count=0 then begin
         fDrawChoreographyBatchCondensedIndices.Add(0);
        end;
        if fDrawChoreographyBatchCondensedUniqueIndices.Count=0 then begin
         fDrawChoreographyBatchCondensedUniqueIndices.Add(0);
        end;
        if fMorphTargetVertices.Count=0 then begin
         fMorphTargetVertices.AddNew;
        end;
        if fJointBlocks.Count=0 then begin
         fJointBlocks.AddNew;
        end;

        fVertices.Finish;
        fIndices.Finish;
        fDrawChoreographyBatchCondensedIndices.Finish;
        fDrawChoreographyBatchCondensedUniqueIndices.Finish;
        fMorphTargetVertices.Finish;
        fJointBlocks.Finish;

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

        fSceneInstance.NewImageDescriptorGeneration;

        fSceneInstance.NewMaterialDataGeneration;

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

  finally
   fInUpload:=false;
  end;

 end;

end;

procedure TpvScene3D.TGroup.Unload;
var Instance:TpvScene3D.TGroup.TInstance;
//  Index:TpvSizeInt;
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
     if not fHeadless then begin
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

procedure TpvScene3D.TGroup.ConstructBuffers;
begin
end;

procedure TpvScene3D.TGroup.MarkAnimatedElements;
var Animation:TpvScene3D.TGroup.TAnimation;
    AnimationChannelIndex:TpvSizeInt;
    AnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
begin
 for Animation in Animations do begin
  for AnimationChannelIndex:=0 to length(Animation.fChannels)-1 do begin
   AnimationChannel:=@Animation.fChannels[AnimationChannelIndex];
   case AnimationChannel^.Target of
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation,
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation,
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale:begin
     if (AnimationChannel^.TargetIndex>=0) and (AnimationChannel^.TargetIndex<fNodes.Count) then begin
      Include(fNodes[AnimationChannel^.TargetIndex].fFlags,TpvScene3D.TGroup.TNode.TNodeFlag.TransformAnimated);
     end;
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
    TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin
     if (AnimationChannel^.TargetIndex>=0) and (AnimationChannel^.TargetIndex<fNodes.Count) then begin
      Include(fNodes[AnimationChannel^.TargetIndex].fFlags,TpvScene3D.TGroup.TNode.TNodeFlag.WeightsAnimated);
     end;
    end;
    else begin
    end;
   end;
  end;
 end;
end;

procedure TpvScene3D.TGroup.CollectAllSceneNodesAndSplitNodesIntoAnimatedOrNotAnimatedSubtreesPerScene;
type TStackItem=record
      Node:TpvScene3D.TGroup.TNode;
      Parent:TpvScene3D.TGroup.TNode;
      WithinAnimatedPath:Boolean;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicStack<TStackItem>;
     TNodeHashMap=TpvHashMap<TpvScene3D.TGroup.TNode,boolean>;
var Stack:TStack;
    Scene:TpvScene3D.TGroup.TScene;
    Node:TpvScene3D.TGroup.TNode;
    NodeHashMap:TNodeHashMap;
    StackItem,NewStackItem:TStackItem;
begin

 Stack.Initialize;
 try

  NodeHashMap:=TNodeHashMap.Create(false);
  try

   for Scene in fScenes do begin

    Scene.fAllNodes.Clear;
    Scene.fTransformAnimatedNodes.Clear;
    Scene.fSkinOrWeightsAnimatedNodes.Clear;
    Scene.fStaticNodes.Clear;

    NodeHashMap.Clear;

    for Node in Scene.Nodes do begin
     if not NodeHashMap.ExistKey(Node) then begin
      NodeHashMap.Add(Node,true);
      NewStackItem.Node:=Node;
      NewStackItem.Parent:=nil;
      NewStackItem.WithinAnimatedPath:=false;
      Stack.Push(NewStackItem);
     end;
    end;

    NodeHashMap.Clear;

    while Stack.Pop(StackItem) do begin
     if not NodeHashMap.ExistKey(StackItem.Node) then begin
      Scene.fAllNodes.Add(StackItem.Node);
      NodeHashMap.Add(StackItem.Node,true);
      if (TpvScene3D.TGroup.TNode.TNodeFlag.SkinAnimated in StackItem.Node.fFlags) or
         (TpvScene3D.TGroup.TNode.TNodeFlag.WeightsAnimated in StackItem.Node.fFlags) then begin
       Scene.fSkinOrWeightsAnimatedNodes.Add(StackItem.Node);
       NewStackItem.WithinAnimatedPath:=true;
      end else if TpvScene3D.TGroup.TNode.TNodeFlag.TransformAnimated in StackItem.Node.fFlags then begin
       Scene.fTransformAnimatedNodes.Add(StackItem.Node);
       NewStackItem.WithinAnimatedPath:=true;
      end else if not (StackItem.WithinAnimatedPath or assigned(StackItem.Parent)) then begin
       Scene.fStaticNodes.Add(StackItem.Node);
       NewStackItem.WithinAnimatedPath:=false;
      end else begin
       NewStackItem.WithinAnimatedPath:=StackItem.WithinAnimatedPath;
       if assigned(StackItem.Parent) then begin
        StackItem.Parent.fSplittedChildren.Add(StackItem.Node);
       end;
      end;
      NewStackItem.Parent:=StackItem.Node;
      for Node in StackItem.Node.Children do begin
       NewStackItem.Node:=Node;
       Stack.Push(NewStackItem);
      end;
     end;
    end;
   end;

  finally
   FreeAndNil(NodeHashMap);
  end;

 finally
  Stack.Finalize;
 end;

end;

procedure TpvScene3D.TGroup.CollectUsedVisibleDrawNodes;
type TNodeStack=TpvDynamicStack<TpvScene3D.TGroup.TNode>;
     TNodeHashMap=TpvHashMap<TpvScene3D.TGroup.TNode,boolean>;
var NodeIndex,PrimitiveIndex:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    NodeStack:TNodeStack;
    Node:TpvScene3D.TGroup.TNode;
    Mesh:TpvScene3D.TGroup.TMesh;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
    NodeHashMap:TNodeHashMap;
    SceneNodeHashMap:TNodeHashMap;
begin
 for Node in fNodes do begin
  Node.fUsedByScenesList.Clear;
 end;
 NodeHashMap:=TNodeHashMap.Create(false);
 try
  NodeStack.Initialize;
  try
   for Scene in fScenes do begin
    SceneNodeHashMap:=TNodeHashMap.Create(false);
    try
     for NodeIndex:=Scene.fNodes.Count-1 downto 0 do begin
      NodeStack.Push(Scene.fNodes[NodeIndex]);
     end;
     while NodeStack.Pop(Node) do begin
      if not SceneNodeHashMap[Node] then begin
       SceneNodeHashMap[Node]:=true;
       Node.fUsedByScenesList.Add(Scene);
      end;
      Mesh:=Node.fMesh;
      if (not NodeHashMap[Node]) and assigned(Mesh) and (Node.fNodeMeshInstanceIndex>=0) then begin
       for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin
        Primitive:=@Mesh.fPrimitives[PrimitiveIndex];
        if (Primitive^.CountIndices>0) and
           assigned(Primitive^.Material) and
           (Node.fNodeMeshInstanceIndex<Primitive^.NodeMeshPrimitiveInstances.Count) then begin
         NodeHashMap[Node]:=true;
         fUsedVisibleDrawNodes.Add(Node);
         break;
        end;
       end;
      end;
      for NodeIndex:=Node.Children.Count-1 downto 0 do begin
       NodeStack.Push(Node.Children[NodeIndex]);
      end;
     end;
    finally
     FreeAndNil(SceneNodeHashMap);
    end;
   end;
  finally
   NodeStack.Finalize;
  end;
 finally
  FreeAndNil(NodeHashMap);
 end;
end;

procedure TpvScene3D.TGroup.CollectMaterials;
var PrimitiveIndex:TpvSizeInt;
    Node:TpvScene3D.TGroup.TNode;
    Mesh:TpvScene3D.TGroup.TMesh;
    Scene:TpvScene3D.TGroup.TScene;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
    Material:TpvScene3D.TMaterial;
begin
 for Node in fUsedVisibleDrawNodes do begin
  Mesh:=Node.fMesh;
  if assigned(Mesh) and (Node.fNodeMeshInstanceIndex>=0) then begin
   for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin
    Primitive:=@Mesh.fPrimitives[PrimitiveIndex];
    if assigned(Primitive) then begin
     Material:=Primitive^.Material;
     if assigned(Material) and (Node.fNodeMeshInstanceIndex<Primitive^.NodeMeshPrimitiveInstances.Count) then begin
      for Scene in Node.fUsedByScenesList do begin
      end;
     end;
    end;
   end;
  end;
 end;
end;

procedure TpvScene3D.TGroup.ConstructDrawChoreographyBatchItems;
type TNodeStack=TpvDynamicStack<TpvScene3D.TGroup.TNode>;
     TIndexBitmap=array of TpvUInt32;
var NodeIndex,PrimitiveIndex,StartIndex,DrawChoreographyBatchItemIndex,
    IndexIndex,VertexIndex,CountIndices:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    NodeStack:TNodeStack;
    Node:TpvScene3D.TGroup.TNode;
    Mesh:TpvScene3D.TGroup.TMesh;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
    Material:TpvScene3D.TMaterial;
    NodeMeshPrimitiveInstance:TpvScene3D.TGroup.TMesh.TPrimitive.PNodeMeshPrimitiveInstance;
    DrawChoreographyBatchItem,OtherDrawChoreographyBatchItem:TDrawChoreographyBatchItem;
    IndexBitmap:TIndexBitmap;
begin

 fDrawChoreographyBatchItems.Clear;

 for Node in fUsedVisibleDrawNodes do begin
  Mesh:=Node.fMesh;
  if assigned(Mesh) and (Node.fNodeMeshInstanceIndex>=0) then begin
   for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin
    Primitive:=@Mesh.fPrimitives[PrimitiveIndex];
    if assigned(Primitive) then begin
     Material:=Primitive^.Material;
     if assigned(Material) and (Node.fNodeMeshInstanceIndex<Primitive^.NodeMeshPrimitiveInstances.Count) then begin
      NodeMeshPrimitiveInstance:=@Primitive^.NodeMeshPrimitiveInstances.Items[Node.fNodeMeshInstanceIndex];
      if Primitive^.CountIndices>0 then begin
       DrawChoreographyBatchItem:=TDrawChoreographyBatchItem.Create;
       try
        DrawChoreographyBatchItem.fGroup:=self;
        DrawChoreographyBatchItem.fGroupInstance:=nil;
        DrawChoreographyBatchItem.fAlphaMode:=Material.fData.AlphaMode;
        DrawChoreographyBatchItem.fPrimitiveTopology:=Primitive^.PrimitiveTopology;
        DrawChoreographyBatchItem.fDoubleSided:=Material.fData.DoubleSided;
        DrawChoreographyBatchItem.fMaterial:=Material;
        DrawChoreographyBatchItem.fNode:=Node;
        DrawChoreographyBatchItem.fMesh:=Mesh;
        DrawChoreographyBatchItem.fMeshPrimitive:=PrimitiveIndex;
        DrawChoreographyBatchItem.fStartIndex:=NodeMeshPrimitiveInstance^.StartBufferIndexOffset;
        DrawChoreographyBatchItem.fCountIndices:=Primitive^.CountIndices;
       finally
        fDrawChoreographyBatchItems.Add(DrawChoreographyBatchItem);
       end;
      end;
     end;
    end;
   end;
  end;
 end;

 fDrawChoreographyBatchItems.Sort;

 fDrawChoreographyBatchCondensedIndices.Clear;
 for DrawChoreographyBatchItem in fDrawChoreographyBatchItems do begin
  StartIndex:=DrawChoreographyBatchItem.fStartIndex;
  DrawChoreographyBatchItem.fStartIndex:=fDrawChoreographyBatchCondensedIndices.Count;
  fDrawChoreographyBatchCondensedIndices.Add(copy(fIndices.Items,StartIndex,DrawChoreographyBatchItem.fCountIndices));
 end;
 fDrawChoreographyBatchCondensedIndices.Finish;

 IndexBitmap:=nil;
 try

  SetLength(IndexBitmap,(fVertices.Count+31) shr 5);
  FillChar(IndexBitmap[0],length(IndexBitmap)*SizeOf(TpvUInt32),0);

  fDrawChoreographyBatchCondensedUniqueIndices.Clear;

  for DrawChoreographyBatchItem in fDrawChoreographyBatchItems do begin

   Node:=TpvScene3D.TGroup.TNode(DrawChoreographyBatchItem.fNode);

   StartIndex:=fDrawChoreographyBatchCondensedUniqueIndices.Count;
   for IndexIndex:=DrawChoreographyBatchItem.fStartIndex to (DrawChoreographyBatchItem.fStartIndex+DrawChoreographyBatchItem.fCountIndices)-1 do begin
    VertexIndex:=fDrawChoreographyBatchCondensedIndices.Items[IndexIndex];
    if (IndexBitmap[VertexIndex shr 5] and (TpvUInt32(1) shl (VertexIndex and 31)))=0 then begin
     IndexBitmap[VertexIndex shr 5]:=IndexBitmap[VertexIndex shr 5] or (TpvUInt32(1) shl (VertexIndex and 31));
     fDrawChoreographyBatchCondensedUniqueIndices.Add(VertexIndex);
    end;
   end;

   if false then begin
    // Not really needed, because every mesh primitive instance in a node gets a own clone instance.
    for IndexIndex:=DrawChoreographyBatchItem.fStartIndex to (DrawChoreographyBatchItem.fStartIndex+DrawChoreographyBatchItem.fCountIndices)-1 do begin
     VertexIndex:=fDrawChoreographyBatchCondensedIndices.Items[IndexIndex];
     IndexBitmap[VertexIndex shr 5]:=IndexBitmap[VertexIndex shr 5] and not (TpvUInt32(1) shl (VertexIndex and 31));
    end;
   end;

   CountIndices:=fDrawChoreographyBatchCondensedUniqueIndices.Count-StartIndex;
   if CountIndices>0 then begin
    OtherDrawChoreographyBatchItem:=TDrawChoreographyBatchItem.Create;
    try
     OtherDrawChoreographyBatchItem.fGroup:=self;
     OtherDrawChoreographyBatchItem.fGroupInstance:=nil;
     OtherDrawChoreographyBatchItem.fAlphaMode:=DrawChoreographyBatchItem.fAlphaMode;
     OtherDrawChoreographyBatchItem.fPrimitiveTopology:=DrawChoreographyBatchItem.fPrimitiveTopology;
     OtherDrawChoreographyBatchItem.fDoubleSided:=DrawChoreographyBatchItem.fDoubleSided;
     OtherDrawChoreographyBatchItem.fMaterial:=DrawChoreographyBatchItem.fMaterial;
     OtherDrawChoreographyBatchItem.fNode:=DrawChoreographyBatchItem.fNode;
     OtherDrawChoreographyBatchItem.fMesh:=DrawChoreographyBatchItem.fMesh;
     OtherDrawChoreographyBatchItem.fMeshPrimitive:=DrawChoreographyBatchItem.fMeshPrimitive;
     OtherDrawChoreographyBatchItem.fStartIndex:=StartIndex;
     OtherDrawChoreographyBatchItem.fCountIndices:=CountIndices;
    finally
     fDrawChoreographyBatchUniqueItems.Add(OtherDrawChoreographyBatchItem);
    end;
   end else begin
    OtherDrawChoreographyBatchItem:=nil;
   end;

   for Scene in Node.fUsedByScenesList do begin
    Scene.fDrawChoreographyBatchItems.Add(DrawChoreographyBatchItem);
    if assigned(OtherDrawChoreographyBatchItem) then begin
     Scene.fDrawChoreographyBatchUniqueItems.Add(OtherDrawChoreographyBatchItem);
    end;
   end;

  end;
  fDrawChoreographyBatchCondensedUniqueIndices.Finish;

  for NodeIndex:=0 to fNodes.Count-1 do begin
   Node:=fNodes[NodeIndex];
   Node.fDrawChoreographyBatchItemIndices.Clear;
   Node.fDrawChoreographyBatchUniqueItemIndices.Clear;
  end;

  for DrawChoreographyBatchItemIndex:=0 to fDrawChoreographyBatchItems.Count-1 do begin
   Node:=TpvScene3D.TGroup.TNode(fDrawChoreographyBatchItems[DrawChoreographyBatchItemIndex].fNode);
   if assigned(Node) then begin
    Node.fDrawChoreographyBatchItemIndices.Add(DrawChoreographyBatchItemIndex);
   end;
  end;

  for DrawChoreographyBatchItemIndex:=0 to fDrawChoreographyBatchUniqueItems.Count-1 do begin
   Node:=TpvScene3D.TGroup.TNode(fDrawChoreographyBatchUniqueItems[DrawChoreographyBatchItemIndex].fNode);
   if assigned(Node) then begin
    Node.fDrawChoreographyBatchUniqueItemIndices.Add(DrawChoreographyBatchItemIndex);
   end;
  end;

  for NodeIndex:=0 to fNodes.Count-1 do begin
   Node:=fNodes[NodeIndex];
   Node.fDrawChoreographyBatchItemIndices.Finish;
   Node.fDrawChoreographyBatchUniqueItemIndices.Finish;
  end;

 finally
  IndexBitmap:=nil;
 end;

end;

function TpvScene3D.TGroup.AssetGetURI(const aURI:TPasGLTFUTF8String):TStream;
var FileName:TPasGLTFUTF8String;
begin
 FileName:=ExpandRelativePath(aURI,AssetBasePath);
 if pvApplication.Assets.ExistAsset(FileName) then begin
  result:=pvApplication.Assets.GetAssetStream(FileName);
 end else begin
  result:=nil;
 end;
end;

procedure TpvScene3D.TGroup.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument);
var LightMap:TpvScene3D.TGroup.TLights;
    ImageMap:TpvScene3D.TImages;
    SamplerMap:TpvScene3D.TSamplers;
    TextureMap:TpvScene3D.TTextures;
    NewImages:TpvScene3D.TImages;
    NewSamplers:TpvScene3D.TSamplers;
    NewTextures:TpvScene3D.TTextures;
    //MaterialMap:TpvScene3D.TMaterials;
    HasLights:boolean;
    POCACodeString:TpvUTF8String;
 procedure ProcessImages;
 var Index:TpvSizeInt;
     SourceImage:TPasGLTF.TImage;
     Image,
     HashedImage,
     CurrentImage:TImage;
     HashData:TImage.THashData;
 begin
  for Index:=0 to aSourceDocument.Images.Count-1 do begin
   SourceImage:=aSourceDocument.Images[Index];
   Image:=TImage.Create(ResourceManager,fSceneInstance);
   try
    fSceneInstance.fImageListLock.Acquire;
    try
     Image.AssignFromGLTF(aSourceDocument,SourceImage);
     HashData:=Image.GetHashData;
     HashedImage:=fSceneInstance.fImageHashMap[HashData];
     if assigned(HashedImage) then begin
      ImageMap.Add(HashedImage);
      CurrentImage:=HashedImage;
     end else begin
      Image.fHashData:=HashData;
      fSceneInstance.fImageHashMap[HashData]:=Image;
      ImageMap.Add(Image);
      CurrentImage:=Image;
      Image:=nil;
     end;
     if assigned(CurrentImage) and (NewImages.IndexOf(CurrentImage)<0) then begin
      CurrentImage.IncRef;
      CurrentImage.LoadData;
      NewImages.Add(CurrentImage);
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
     HashedSampler,
     CurrentSampler:TSampler;
     HashData:TSampler.THashData;
 begin
  for Index:=0 to aSourceDocument.Samplers.Count-1 do begin
   SourceSampler:=aSourceDocument.Samplers[Index];
   Sampler:=TSampler.Create(ResourceManager,fSceneInstance);
   try
    fSceneInstance.fSamplerListLock.Acquire;
    try
     Sampler.AssignFromGLTF(aSourceDocument,SourceSampler);
     HashData:=Sampler.GetHashData;
     HashedSampler:=fSceneInstance.fSamplerHashMap[HashData];
     if assigned(HashedSampler) then begin
      SamplerMap.Add(HashedSampler);
      CurrentSampler:=HashedSampler;
     end else begin
      fSceneInstance.fSamplerHashMap[HashData]:=Sampler;
      SamplerMap.Add(Sampler);
      CurrentSampler:=Sampler;
      Sampler:=nil;
     end;
     if assigned(CurrentSampler) and (NewSamplers.IndexOf(CurrentSampler)<0) then begin
      CurrentSampler.IncRef;
      NewSamplers.Add(CurrentSampler);
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
     HashedTexture,
     CurrentTexture:TTexture;
     HashData:TTexture.THashData;
 begin
  for Index:=0 to aSourceDocument.Textures.Count-1 do begin
   SourceTexture:=aSourceDocument.Textures[Index];
   Texture:=TTexture.Create(ResourceManager,fSceneInstance);
   try
    fSceneInstance.fTextureListLock.Acquire;
    try
     Texture.AssignFromGLTF(aSourceDocument,SourceTexture,ImageMap,SamplerMap);
     HashData:=Texture.GetHashData;
     HashedTexture:=fSceneInstance.fTextureHashMap[HashData];
     if assigned(HashedTexture) then begin
      TextureMap.Add(HashedTexture);
      CurrentTexture:=HashedTexture;
     end else begin
      fSceneInstance.fTextureHashMap[HashData]:=Texture;
      TextureMap.Add(Texture);
      CurrentTexture:=Texture;
      Texture:=nil;
     end;
     if assigned(CurrentTexture) and (NewTextures.IndexOf(CurrentTexture)<0) then begin
      CurrentTexture.IncRef;
      NewTextures.Add(CurrentTexture);
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
 var Index,MaterialIDMapArrayIndex:TpvSizeInt;
     SourceMaterial:TPasGLTF.TMaterial;
     Material,
     HashedMaterial:TpvScene3D.TMaterial;
     HashData:TpvScene3D.TMaterial.THashData;
     MaterialIDMapArray:TMaterialIDMapArray;
 begin

  SetLength(fMaterialMap,aSourceDocument.Materials.Count+1);

  fMaterialMap[0]:=fSceneInstance.fEmptyMaterial.fID;

  fSceneInstance.fMaterialListLock.Acquire;
  try

   for Index:=0 to aSourceDocument.Materials.Count-1 do begin

    SourceMaterial:=aSourceDocument.Materials[Index];

    Material:=TpvScene3D.TMaterial.Create(ResourceManager,fSceneInstance);
    try

     Material.AssignFromGLTF(aSourceDocument,SourceMaterial,TextureMap);

     HashData:=Material.fData;

     HashedMaterial:=fSceneInstance.fMaterialHashMap[HashData];
     if assigned(HashedMaterial) then begin
      fMaterials.Add(HashedMaterial);
     end else begin
      fSceneInstance.fMaterialHashMap[HashData]:=Material;
      fMaterials.Add(Material);
      Material:=nil;
     end;

    finally
     FreeAndNil(Material);
    end;

    Material:=fMaterials[Index];
    try

     fMaterialMap[Index+1]:=Material.fID;

     if length(trim(Material.fName))>0 then begin
      fMaterialNameMapArrayIndexHashMap.Add(Material.fName,Index);
     end;

     MaterialIDMapArrayIndex:=fMaterialIDMapArrayIndexHashMap[Material.fID];
     if MaterialIDMapArrayIndex<0 then begin
      MaterialIDMapArray:=TMaterialIDMapArray.Create;
      MaterialIDMapArrayIndex:=fMaterialIDMapArrays.Add(MaterialIDMapArray);
      fMaterialIDMapArrayIndexHashMap.Add(Material.fID,MaterialIDMapArrayIndex);
     end else begin
      MaterialIDMapArray:=fMaterialIDMapArrays[MaterialIDMapArrayIndex];
     end;
     MaterialIDMapArray.Add(Index);

    finally
     Material.IncRef;
    end;

   end;

  finally
   fSceneInstance.fMaterialListLock.Release;
  end;

 end;
 procedure ProcessAnimations;
 var Index:TpvSizeInt;
     SourceAnimation:TPasGLTF.TAnimation;
     Animation:TpvScene3D.TGroup.TAnimation;
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
 procedure ExecuteCode;
 var POCAInstance:PPOCAInstance;
     POCAContext:PPOCAContext;
     POCACode:TPOCAValue;
     Code:TpvUTF8String;
     POCAScene3DGroup:TPOCAScene3DGroup;
 begin
  if length(POCACodeString)>0 then begin
   Code:=PUCUUTF8Trim(PUCUUTF8Correct(POCACodeString));
   if length(Code)>0 then begin
    POCAInstance:=POCAInstanceCreate;
    try
     POCAContext:=POCAContextCreate(POCAInstance);
     try
      try
       POCAScene3DGroup:=TPOCAScene3DGroup.Create(POCAInstance,POCAContext,nil,nil,false);
       POCAScene3DGroup.fGroup:=self;
       POCAHashSet(POCAContext,
                   POCAInstance.Globals.Namespace,
                   POCANewUniqueString(POCAContext,'Group'),
                   POCANewNativeObject(POCAContext,POCAScene3DGroup));
       POCACode:=POCACompile(POCAInstance,POCAContext,Code,'<CODE>');
       POCACall(POCAContext,POCACode,nil,0,POCAValueNull,POCAInstance^.Globals.Namespace);
      except
       on e:EPOCASyntaxError do begin
        // Ignore
       end;
       on e:EPOCARuntimeError do begin
        // Ignore
       end;
       on e:EPOCAScriptError do begin
        // Ignore
       end;
       on e:Exception do begin
        raise;
       end;
      end;
     finally
      POCAContextDestroy(POCAContext);
     end;
    finally
     POCAInstanceDestroy(POCAInstance);
    end;
   end;
  end;
 end;
 procedure PostProcessAnimations;
 type TMaterialHashMap=TpvHashMap<TpvSizeInt,TpvSizeInt>;
      TMaterialArrayList=TpvDynamicArrayList<TpvSizeInt>;
      TTargetHashMap=TpvHashMap<TpvUInt64,TpvSizeInt>;
      TTargetArrayList=TpvDynamicArrayList<TpvUInt64>;
      TTargetUsedBitmap=array of TpvUInt32;
 var Index,ChannelIndex,TargetIndex,CountDefaultChannels,
     MaterialArrayIndex,MaterialIDMapArrayIndex,
     MaterialIndex,InstanceChannelTargetIndex:TpvSizeInt;
     SourceAnimation:TPasGLTF.TAnimation;
     Animation:TpvScene3D.TGroup.TAnimation;
     Channel:TpvScene3D.TGroup.TAnimation.PChannel;
     DefaultChannel:TpvScene3D.TGroup.TAnimation.PDefaultChannel;
     MaterialHashMap:TMaterialHashMap;
     MaterialArrayList:TMaterialArrayList;
     TargetHashMap:TTargetHashMap;
     InstanceChannelTargetHashMap:TTargetHashMap;
     TargetArrayList:TTargetArrayList;
     TargetUsedBitmap:TTargetUsedBitmap;
     CompactCode:TpvUInt64;
     Material,DuplicatedMaterial:TpvScene3D.TMaterial;
     MaterialIDMapArray:TpvScene3D.TGroup.TMaterialIDMapArray;
     TextureTransform:TpvScene3D.TMaterial.TTextureReference.PTransform;
     OK:boolean;
 begin
  fCountInstanceAnimationChannels:=0;
  MaterialHashMap:=TMaterialHashMap.Create(-1);
  try
   MaterialArrayList:=TMaterialArrayList.Create;
   try
    InstanceChannelTargetHashMap:=TTargetHashMap.Create(-1);
    try
     TargetHashMap:=TTargetHashMap.Create(-1);
     try
      TargetArrayList:=TTargetArrayList.Create;
      try
       for Index:=0 to fAnimations.Count-1 do begin
        Animation:=fAnimations[Index];
        for ChannelIndex:=0 to length(Animation.fChannels)-1 do begin
         Channel:=@Animation.fChannels[ChannelIndex];
         Channel^.TargetInstanceIndex:=-1;
         case Channel^.Target of
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatRoughnessFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialIOR,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation,
          TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin
           OK:=false;
           if (Channel^.TargetIndex>=0) and (Channel^.TargetIndex<fMaterials.Count) then begin
            MaterialIndex:=Channel^.TargetIndex;
            Material:=fMaterials[MaterialIndex];
            MaterialIDMapArrayIndex:=fMaterialIDMapArrayIndexHashMap[Material.fID];
            if MaterialIDMapArrayIndex>=0 then begin
             MaterialIDMapArray:=fMaterialIDMapArrays[MaterialIDMapArrayIndex];
             if MaterialIDMapArray.Count>0 then begin
              if MaterialIDMapArray.Count>=2 then begin
               fSceneInstance.fMaterialListLock.Acquire;
               try
                DuplicatedMaterial:=TpvScene3D.TMaterial.Create(ResourceManager,fSceneInstance);
                try
                 DuplicatedMaterial.Assign(Material);
                 Material.DecRef;
                 Material:=DuplicatedMaterial;
                 Material.IncRef;
                 MaterialIDMapArray.Remove(MaterialIndex);
                 MaterialIDMapArray:=TMaterialIDMapArray.Create;
                 fMaterialIDMapArrayIndexHashMap.Add(Material.fID,fMaterialIDMapArrays.Add(MaterialIDMapArray));
                 MaterialIDMapArray.Add(MaterialIndex);
                finally
                 fMaterials[MaterialIndex]:=Material;
                 fMaterialMap[Index+1]:=Material.fID;
                end;
               finally
                fSceneInstance.fMaterialListLock.Release;
               end;
              end;
              //Channel^.TargetIndex:=Material.fID;
              MaterialArrayIndex:=MaterialHashMap[Material.fID];
              if MaterialArrayIndex<0 then begin
               MaterialArrayIndex:=MaterialArrayList.Add(Material.fID);
               MaterialHashMap.Add(Material.fID,MaterialArrayIndex);
               fMaterialsToDuplicate.Add(Material);
              end;
              OK:=true;
             end;
            end;
            if assigned(Material) and (Channel^.TargetSubIndex>=0) then begin
             case Channel^.Target of
              TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset,
              TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation,
              TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin
               Material.fData.AnimatedTextureMask:=Material.fData.AnimatedTextureMask or (TpvUInt64(1) shl TpvSizeInt(Channel^.TargetSubIndex));
               TextureTransform:=Material.fData.GetTextureTransform(TpvScene3D.TTextureIndex(Channel^.TargetSubIndex));
               if assigned(TextureTransform) then begin
                TextureTransform^.Active:=true;
               end;
              end;
             end;
{            if (Channel^.Target in TpvScene3D.TGroup.TAnimation.TChannel.TextureTargets) and
                (Channel^.TargetSubIndex>=0) then begin
              Material.fData.AnimatedTextureMask:=Material.fData.AnimatedTextureMask or (TpvUInt64(1) shl TpvSizeInt(Channel^.TargetSubIndex));
             end;}
            end;
           end;
           if not OK then begin
            Channel^.TargetIndex:=-1;
           end;
          end;
         end;
{         if Channel^.Target in TpvScene3D.TGroup.TAnimation.TChannel.MaterialTargets then begin
         end;}
        end;
        for ChannelIndex:=0 to length(Animation.fChannels)-1 do begin
         Channel:=@Animation.fChannels[ChannelIndex];
         if Channel^.TargetIndex>=0 then begin
          begin
           CompactCode:=(TpvUInt64(TpvUInt64(TpvInt32(Channel^.Target)) and TpvUInt64($ffff)) shl 48) or
                        (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetIndex)+1) and TpvUInt64($ffffffff)) shl 16) or
                        (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetSubIndex)+1) and TpvUInt64($ffff)) shl 0);
           TargetIndex:=TargetHashMap[CompactCode];
           if TargetIndex<0 then begin
            TargetHashMap[CompactCode]:=TargetArrayList.Add(CompactCode);
           end;
          end;
          begin
           CompactCode:=(TpvUInt64(TpvUInt64(TpvInt32(AnimationChannelTargetOverwriteGroupMap[Channel^.Target])) and TpvUInt64($ffff)) shl 48) or
                        (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetIndex)+1) and TpvUInt64($ffffffff)) shl 16) or
                        (TpvUInt64(TpvUInt64(TpvInt64(Channel^.TargetSubIndex)+1) and TpvUInt64($ffff)) shl 0);
           InstanceChannelTargetIndex:=InstanceChannelTargetHashMap[CompactCode];
           if InstanceChannelTargetIndex<0 then begin
            InstanceChannelTargetIndex:=fCountInstanceAnimationChannels;
            inc(fCountInstanceAnimationChannels);
            InstanceChannelTargetHashMap[CompactCode]:=InstanceChannelTargetIndex;
           end;
           Channel^.TargetInstanceIndex:=InstanceChannelTargetIndex;
          end;
         end;
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
             begin
              CompactCode:=TargetArrayList[TargetIndex];
              DefaultChannel^.Target:=TpvScene3D.TGroup.TAnimation.TChannel.TTarget(TpvInt32(TpvUInt64(TpvUInt64(CompactCode) shr 48)));
              DefaultChannel^.TargetIndex:=TpvSizeInt(TpvUInt64(TpvUInt64(TpvUInt64(CompactCode) shr 16) and TpvUInt64($ffffffff)))-1;
              DefaultChannel^.TargetSubIndex:=TpvSizeInt(TpvUInt64(TpvUInt64(TpvUInt64(CompactCode) shr 0) and TpvUInt64($ffff)))-1;
             end;
             begin
              CompactCode:=(TpvUInt64(TpvUInt64(TpvInt32(AnimationChannelTargetOverwriteGroupMap[DefaultChannel^.Target])) and TpvUInt64($ffff)) shl 48) or
                           (TpvUInt64(TpvUInt64(TpvInt64(DefaultChannel^.TargetIndex)+1) and TpvUInt64($ffffffff)) shl 16) or
                           (TpvUInt64(TpvUInt64(TpvInt64(DefaultChannel^.TargetSubIndex)+1) and TpvUInt64($ffff)) shl 0);
              InstanceChannelTargetIndex:=InstanceChannelTargetHashMap[CompactCode];
              if InstanceChannelTargetIndex<0 then begin
               InstanceChannelTargetIndex:=fCountInstanceAnimationChannels;
               inc(fCountInstanceAnimationChannels);
               InstanceChannelTargetHashMap[CompactCode]:=InstanceChannelTargetIndex;
              end;
              DefaultChannel^.TargetInstanceIndex:=InstanceChannelTargetIndex;
             end;
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
    finally
     FreeAndNil(InstanceChannelTargetHashMap);
    end;
   finally
    FreeAndNil(MaterialArrayList);
   end;
  finally
   FreeAndNil(MaterialHashMap);
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
    if length(trim(Camera.fName))>0 then begin
     fCameraNameIndexHashMap.Add(Camera.fName,Index);
    end;
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
    if length(trim(Mesh.fName))>0 then begin
     fMeshNameIndexHashMap.Add(Mesh.fName,Index);
    end;
    Mesh.AssignFromGLTF(aSourceDocument,SourceMesh,fMaterials);
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
 type TPOCAFileHashMap=TpvStringHashMap<Boolean>;
 var Index,Offset:TpvSizeInt;
     SourceNode:TPasGLTF.TNode;
     Node:TNode;
     TemporaryString:TPasJSONUTF8String;
     TemporaryStream:TStream;
     POCAFileHashMap:TPOCAFileHashMap;
 begin
  POCAFileHashMap:=TPOCAFileHashMap.Create(false);
  try
   fCountNodeWeights:=0;
   fNodes.Clear;
   for Index:=0 to aSourceDocument.Nodes.Count-1 do begin
    SourceNode:=aSourceDocument.Nodes[Index];
    Node:=TNode.Create(self,Index);
    try
     Node.AssignFromGLTF(aSourceDocument,SourceNode,LightMap);
     if assigned(SourceNode.Extras) then begin
      begin
       TemporaryString:=TPasJSON.GetString(SourceNode.Extras.Properties['pocacode'],'');
       if length(TemporaryString)>0 then begin
        TemporaryString:='(function(){'+#13#10+TemporaryString+#13#10+'})();'+#13#10;
        POCACodeString:=POCACodeString+TemporaryString;
       end else begin
        TemporaryString:=TPasJSON.GetString(SourceNode.Extras.Properties['pocafile'],'');
        if length(TemporaryString)>0 then begin
         if not POCAFileHashMap.ExistKey(TemporaryString) then begin
          POCAFileHashMap.Add(TemporaryString,true);
          if pvApplication.Assets.ExistAsset(TemporaryString) then begin
           TemporaryStream:=pvApplication.Assets.GetAssetStream(TemporaryString);
          end else begin
           TemporaryStream:=nil;
          end;
          if not assigned(TemporaryStream) then begin
           TemporaryStream:=aSourceDocument.GetURI(TemporaryString);
          end;
          if assigned(TemporaryStream) then begin
           try
            TemporaryString:='';
            if TemporaryStream.Size>0 then begin
             SetLength(TemporaryString,TemporaryStream.Size);
             TemporaryStream.ReadBuffer(TemporaryString[1],TemporaryStream.Size);
             TemporaryString:='(function(){'+#13#10+TemporaryString+#13#10+'})();'+#13#10;
             POCACodeString:=POCACodeString+TemporaryString;
            end;
           finally
            FreeAndNil(TemporaryStream);
           end;
          end;
         end;
        end;
       end;
      end;
     end;
    finally
     fNodes.Add(Node);
    end;
   end;
  finally
   FreeAndNil(POCAFileHashMap);
  end;
  fNodeNameIndexHashMap.Clear;
  fCameraNodeIndices.Clear;
  for Index:=0 to fNodes.Count-1 do begin
   Node:=fNodes[Index];
   Node.Finish;
   fNodeNameIndexHashMap.Add(Node.fName,Index);
   if assigned(Node.Camera) then begin
    fCameraNodeIndices.Add(Index);
   end;
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
      Visible:boolean;
      BoundingBox:TpvAABB;
  begin
   Node:=fNodes[aNodeIndex];
   Matrix:=((TpvMatrix4x4.CreateScale(Node.fScale)*
             (TpvMatrix4x4.CreateFromQuaternion(Node.fRotation)*
              TpvMatrix4x4.CreateTranslation(Node.fTranslation)))*Node.fMatrix)*aMatrix;
   if assigned(Node.fMesh) then begin
    Visible:=false;
    for Index:=0 to length(Node.fMesh.fPrimitives)-1 do begin
     if assigned(Node.fMesh.fPrimitives[Index].Material) and Node.fMesh.fPrimitives[Index].Material.fVisible then begin
      Visible:=true;
      break;
     end;
    end;
    if Visible then begin
     BoundingBox:=Node.fMesh.fBoundingBox.Transform(Matrix);
     if First then begin
      First:=false;
      fBoundingBox:=BoundingBox;
     end else begin
      fBoundingBox:=fBoundingBox.Combine(BoundingBox);
     end;
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
 procedure CollectNodeUsedJoints;
 // This procedure goes through all nodes and collects their used joints and their associated used weights and AABBs,
 // for later runtime calculation of a conservative worst-case bounding box for a node with a skinned animated mesh,
 // For that, it identifies and records the joints used by each node. It iterates through all nodes, determining which
 // joints influence the vertices of the node's mesh. A bounding box for each joint will be calculated, accounting for
 // mesh vertices including approximate influences of morph target vertices, and aggregates this information in a list
 // of used joints for each node. This list is later used to approximate the bounding box for skinned animated meshes.
 type TJointIndexHashMap=TpvHashMap<TpvSizeInt,TpvSizeInt>;
 var PrimitiveIndex,VertexIndex,JointBlockIndex,JointIndex,Joint,UsedJointIndex:TpvSizeInt;
     MorphTargetVertexIndex:TpvUInt32;
     MorphTargetVertex:TpvScene3D.PMorphTargetVertex;
     JointIndexHashMap:TJointIndexHashMap;
     Node:TpvScene3D.TGroup.TNode;
     Mesh:TpvScene3D.TGroup.TMesh;
     Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
     Vertex:TpvScene3D.PVertex;
     JointBlock:TpvScene3D.PJointBlock;
     Weight:TpvScalar;
     UsedJoint:TpvScene3D.TGroup.TNode.PUsedJoint;
     AABB:TpvAABB;
 begin

  // Create a hash map to store joint indices
  JointIndexHashMap:=TJointIndexHashMap.Create(-1);
  try

   // Iterate over each node
   for Node in fNodes do begin

    Mesh:=Node.Mesh;

    // Check if the node has a mesh
    if assigned(Mesh) then begin

     // Reset the list of used joints for the node
     Node.fUsedJoints.Clear;

     // Clear the hash map of joint indices
     JointIndexHashMap.Clear;

     // Iterate over each primitive of the mesh
     for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin

      Primitive:=@Mesh.fPrimitives[PrimitiveIndex];

      // Check if the primitive has vertices
      if Primitive^.CountVertices>0 then begin

       // Iterate over each vertex of the primitive
       for VertexIndex:=TpvSizeInt(Primitive^.StartBufferVertexOffset) to TpvSizeInt(Primitive^.StartBufferVertexOffset+Primitive^.CountVertices)-1 do begin

        Vertex:=@fVertices.Items[VertexIndex];

        // Check if the vertex has joint blocks
        if Vertex^.CountJointBlocks>0 then begin

         // Initialize the axis-aligned bounding box (AABB) with the vertex position
         AABB.Min:=Vertex^.Position;
         AABB.Max:=Vertex^.Position;

         // Process and adjust bounding box for morph target vertices. The loop continues until
         // it encounters the sentinel value TpvUInt32($ffffffff), which indicates the end of
         // the morph target vertex chain. This value is used as a marker to signify that
         // there are no more morph target vertices to process.
         MorphTargetVertexIndex:=Vertex^.MorphTargetVertexBaseIndex;
         while MorphTargetVertexIndex<>TpvUInt32($ffffffff) do begin
          MorphTargetVertex:=@fMorphTargetVertices.Items[MorphTargetVertexIndex];
          AABB.DirectCombineVector3(Vertex^.Position+MorphTargetVertex^.Position.xyz); // Assume a weight value of 1.0 for an approximate result
          MorphTargetVertexIndex:=MorphTargetVertex^.Next;
         end;

         // Iterate over joint blocks that influence the current vertex
         for JointBlockIndex:=Vertex^.JointBlockBaseIndex to (Vertex^.JointBlockBaseIndex+Vertex^.CountJointBlocks)-1 do begin

          JointBlock:=@fJointBlocks.Items[JointBlockIndex];

          // Process each joint in the joint block
          for JointIndex:=0 to 3 do begin

           Joint:=JointBlock^.Joints[JointIndex];
           Weight:=JointBlock^.Weights[JointIndex];

           // Check if joint is valid and weight is not zero
           if (Joint>=0) and not IsZero(Weight) then begin

            // Check if joint is already in the used joints list
            if JointIndexHashMap.TryGet(Joint,UsedJointIndex) then begin

             // If yes, update the data of the used joint

             UsedJoint:=@Node.fUsedJoints.Items[UsedJointIndex];
             UsedJoint^.Weight:=Max(abs(UsedJoint^.Weight),abs(Weight))*Sign(Weight);
             UsedJoint^.AABB.DirectCombine(AABB);

            end else begin

             // If no, add the joint to the used joints list with initial data 

             UsedJointIndex:=Node.fUsedJoints.AddNew;

             JointIndexHashMap.Add(Joint,UsedJointIndex);

             UsedJoint:=@Node.fUsedJoints.Items[UsedJointIndex];
             UsedJoint^.Joint:=Joint;
             UsedJoint^.Weight:=Weight;
             UsedJoint^.AABB:=AABB;

            end;

           end;

          end;

         end;

        end;

       end;

      end;

     end;

     // Finalize the list of used joints for the node (freezing the dynamic allocated array to its final size) 
     Node.fUsedJoints.Finish; 

    end;

   end;

  finally

   // Release resources for the joint index hash map
   FreeAndNil(JointIndexHashMap);
   
  end;

 end;
var Image:TpvScene3D.TImage;
    Sampler:TpvScene3D.TSampler;
    Texture:TpvScene3D.TTexture;
    Scene:TpvScene3D.TGroup.TScene;
begin

 POCACodeString:='';

 HasLights:=aSourceDocument.ExtensionsUsed.IndexOf('KHR_lights_punctual')>=0;

 LightMap:=TpvScene3D.TGroup.TLights.Create;
 LightMap.OwnsObjects:=false;
 try

  ProcessLights;

  ImageMap:=TpvScene3D.TImages.Create;
  ImageMap.OwnsObjects:=false;
  try

   NewImages:=TpvScene3D.TImages.Create;
   NewImages.OwnsObjects:=false;
   try

    ProcessImages;

    SamplerMap:=TpvScene3D.TSamplers.Create;
    SamplerMap.OwnsObjects:=false;
    try

     NewSamplers:=TpvScene3D.TSamplers.Create;
     NewSamplers.OwnsObjects:=false;
     try

      ProcessSamplers;

      TextureMap:=TpvScene3D.TTextures.Create;
      TextureMap.OwnsObjects:=false;
      try

       NewTextures:=TpvScene3D.TTextures.Create;
       NewTextures.OwnsObjects:=false;
       try

        ProcessTextures;

   {    MaterialMap:=TpvScene3D.TMaterials.Create;
        MaterialMap.OwnsObjects:=false;}
        try

         ProcessMaterials;

         ProcessCameras;

         ProcessMeshes;

         ProcessSkins;

         ProcessNodes;

         ProcessScenes;

         ProcessAnimations;

         ExecuteCode;

         PostProcessAnimations;

         MarkAnimatedElements;

         CollectAllSceneNodesAndSplitNodesIntoAnimatedOrNotAnimatedSubtreesPerScene;

         if (aSourceDocument.Scene>=0) and (aSourceDocument.Scene<fScenes.Count) then begin
          fScene:=fScenes[aSourceDocument.Scene];
         end else if fScenes.Count>0 then begin
          fScene:=fScenes[0];
         end else begin
          fScene:=nil;
         end;

         CalculateBoundingBox;

        finally
   //    FreeAndNil(MaterialMap);
        end;

       finally
        try
         fSceneInstance.fTextureListLock.Acquire;
         try
          for Texture in NewTextures do begin
           Texture.DecRef;
          end;
         finally
          fSceneInstance.fTextureListLock.Release;
         end;
        finally
         FreeAndNil(NewTextures);
        end;
       end;

      finally
       FreeAndNil(TextureMap);
      end;

     finally
      try
       fSceneInstance.fSamplerListLock.Acquire;
       try
        for Sampler in NewSamplers do begin
         Sampler.DecRef;
        end;
       finally
        fSceneInstance.fSamplerListLock.Release;
       end;
      finally
       FreeAndNil(NewSamplers);
      end;
     end;

    finally
     FreeAndNil(SamplerMap);
    end;

   finally
    try
     fSceneInstance.fImageListLock.Acquire;
     try
      for Image in NewImages do begin
       Image.DecRef;
      end;
     finally
      fSceneInstance.fImageListLock.Release;
     end;
    finally
     FreeAndNil(NewImages);
    end;
   end;

  finally
   FreeAndNil(ImageMap);
  end;

 finally
  FreeAndNil(LightMap);
 end;

 ConstructBuffers;

 CollectUsedVisibleDrawNodes;

 CollectMaterials;

 CollectNodeUsedJoints;

 ConstructDrawChoreographyBatchItems;

 for Scene in fScenes do begin
  Scene.ConstructSkipList;
 end;

end;

function TpvScene3D.TGroup.BeginLoad(const aStream:TStream):boolean;
var GLTF:TPasGLTF.TDocument;
begin
 result:=false;
 fSceneInstance.fLoadLock.Acquire;
 try
  pvApplication.Log(LOG_DEBUG,'TpvScene3D.TGroup.BeginLoad','Entering...');
  try
   if assigned(aStream) then begin
    GLTF:=TPasGLTF.TDocument.Create;
    try
     if (length(FileName)>0) and (FileExists(FileName)) then begin
      GLTF.RootPath:=ExtractFilePath(ExpandFileName(FileName));
     end;
     if IsAsset then begin
      GLTF.GetURI:=AssetGetURI;
     end;
     GLTF.LoadFromStream(aStream);
     AssignFromGLTF(GLTF);
    finally
     FreeAndNil(GLTF);
    end;
    result:=true;
   end;
  finally
   pvApplication.Log(LOG_DEBUG,'TpvScene3D.TGroup.BeginLoad','Leaving...');
  end;
 finally
  fSceneInstance.fLoadLock.Release;
 end;
end;

function TpvScene3D.TGroup.EndLoad:boolean;
begin
 fSceneInstance.fLoadLock.Acquire;
 try
  pvApplication.Log(LOG_DEBUG,'TpvScene3D.TGroup.EndLoad','Entering...');
  try
   result:=inherited EndLoad;
   if result then begin
    if SceneInstance.fUploaded then begin
     Upload;
     fSceneInstance.NewImageDescriptorGeneration;
     fSceneInstance.NewMaterialDataGeneration;
    end;
   end;
  finally
   pvApplication.Log(LOG_DEBUG,'TpvScene3D.TGroup.EndLoad','Leaving...');
  end;
 finally
  fSceneInstance.fLoadLock.Release;
 end;
end;

procedure TpvScene3D.TGroup.Check(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Check(aInFlightFrameIndex);
 end;
end;

procedure TpvScene3D.TGroup.Update(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Update(aInFlightFrameIndex);
 end;
end;

procedure TpvScene3D.TGroup.PrepareFrame(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 if not fHeadless then begin
  for Instance in fInstances do begin
   Instance.PrepareFrame(aInFlightFrameIndex);
  end;
 end;
end;

procedure TpvScene3D.TGroup.UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 if not fHeadless then begin
  for Instance in fInstances do begin
   Instance.UploadFrame(aInFlightFrameIndex);
  end;
 end;
end;

procedure TpvScene3D.TGroup.UpdateCachedVertices(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 if assigned(fSceneInstance.fVulkanDevice) and not fHeadless then begin
  for Instance in fInstances do begin
   Instance.UpdateCachedVertices(aInFlightFrameIndex);
  end;
 end;
end;

function TpvScene3D.TGroup.CreateInstance(const aHeadless:Boolean=false):TpvScene3D.TGroup.TInstance;
begin
 if (fMaximumCountInstances<0) or (fInstances.Count<fMaximumCountInstances) then begin
  result:=TpvScene3D.TGroup.TInstance.Create(ResourceManager,self,nil,fHeadless or aHeadless);
 end else begin
  result:=nil;
 end;
end;

function TpvScene3D.TGroup.GetNodeIndexByName(const aNodeName:TpvUTF8String):TpvSizeInt;
begin
 result:=fNodeNameIndexHashMap[aNodeName];
end;

function TpvScene3D.TGroup.GetNodeByName(const aNodeName:TpvUTF8String):TpvScene3D.TGroup.TNode;
var NodeIndex:TpvSizeInt;
begin
 NodeIndex:=fNodeNameIndexHashMap[aNodeName];
 if NodeIndex>=0 then begin
  result:=fNodes[NodeIndex];
 end else begin
  result:=nil;
 end;
end;

{ TpvScene3D.TGroup.TInstance.TNode }

function TpvScene3D.TGroup.TInstance.TNode.InverseFrontFaces:boolean;
begin
 result:=TpvScene3D.TGroup.TInstance.TNode.TInstanceNodeFlag.InverseFrontFaces in Flags;
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
    Overwrite:TpvScene3D.TGroup.TInstance.TLight.PLightOverwrite;
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
    if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Defaults in Overwrite^.Flags then begin
     ColorSum.Add(fData.fColor,Factor);
     IntensitySum.Add(fData.fIntensity,Factor);
     RangeSum.Add(fData.fRange,Factor);
     SpotInnerConeAngleSum.Add(fData.fInnerConeAngle,Factor);
     SpotOuterConeAngleSum.Add(fData.fOuterConeAngle,Factor);
    end else begin
     if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Color in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultColor in Overwrite^.Flags then begin
       ColorSum.Add(fData.fColor,Factor);
      end else begin
       ColorSum.Add(Overwrite^.Color,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Intensity in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultIntensity in Overwrite^.Flags then begin
       IntensitySum.Add(fData.fIntensity,Factor);
      end else begin
       IntensitySum.Add(Overwrite^.Intensity,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Range in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultRange in Overwrite^.Flags then begin
       RangeSum.Add(fData.fRange,Factor);
      end else begin
       RangeSum.Add(Overwrite^.Range,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.SpotInnerConeAngle in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultSpotInnerConeAngle in Overwrite^.Flags then begin
       SpotInnerConeAngleSum.Add(fData.fInnerConeAngle,Factor);
      end else begin
       SpotInnerConeAngleSum.Add(Overwrite^.SpotInnerConeAngle,Factor);
      end;
     end;
     if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.SpotOuterConeAngle in Overwrite^.Flags then begin
      if TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultSpotOuterConeAngle in Overwrite^.Flags then begin
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
    Overwrite:TpvScene3D.TGroup.TInstance.TCamera.PCameraOverwrite;
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
    if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.Defaults in Overwrite^.Flags then begin
     case fData.Type_ of
      TpvScene3D.TCameraData.TCameraType.Orthographic:begin
       OrthographicXMagSum.Add(fData.Orthographic.XMag,Factor);
       OrthographicYMagSum.Add(fData.Orthographic.YMag,Factor);
       OrthographicZFarSum.Add(fData.Orthographic.ZFar,Factor);
       OrthographicZNearSum.Add(fData.Orthographic.ZNear,Factor);
      end;
      TpvScene3D.TCameraData.TCameraType.Perspective:begin
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
      TpvScene3D.TCameraData.TCameraType.Orthographic:begin
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicXMag in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicXMag in Overwrite^.Flags then begin
         OrthographicXMagSum.Add(fData.Orthographic.XMag,Factor);
        end else begin
         OrthographicXMagSum.Add(Overwrite^.OrthographicXMag,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicYMag in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicYMag in Overwrite^.Flags then begin
         OrthographicYMagSum.Add(fData.Orthographic.YMag,Factor);
        end else begin
         OrthographicYMagSum.Add(Overwrite^.OrthographicYMag,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicZFar in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicZFar in Overwrite^.Flags then begin
         OrthographicZFarSum.Add(fData.Orthographic.ZFar,Factor);
        end else begin
         OrthographicZFarSum.Add(Overwrite^.OrthographicZFar,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicZNear in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicZNear in Overwrite^.Flags then begin
         OrthographicZNearSum.Add(fData.Orthographic.ZNear,Factor);
        end else begin
         OrthographicZNearSum.Add(Overwrite^.OrthographicZNear,Factor);
        end;
       end;
      end;
      TpvScene3D.TCameraData.TCameraType.Perspective:begin
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveAspectRatio in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveAspectRatio in Overwrite^.Flags then begin
         PerspectiveAspectRatioSum.Add(fData.Perspective.AspectRatio,Factor);
        end else begin
         PerspectiveAspectRatioSum.Add(Overwrite^.PerspectiveAspectRatio,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveYFov in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveYFov in Overwrite^.Flags then begin
         PerspectiveYFovSum.Add(fData.Perspective.YFoV,Factor);
        end else begin
         PerspectiveYFovSum.Add(Overwrite^.PerspectiveYFov,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveZFar in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveZFar in Overwrite^.Flags then begin
         PerspectiveZFarSum.Add(fData.Perspective.ZFar,Factor);
        end else begin
         PerspectiveZFarSum.Add(Overwrite^.PerspectiveZFar,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveZNear in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveZNear in Overwrite^.Flags then begin
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
   TpvScene3D.TCameraData.TCameraType.Orthographic:begin
    fWorkData.Orthographic.XMag:=OrthographicXMagSum.Get(fData.Orthographic.XMag);
    fWorkData.Orthographic.YMag:=OrthographicYMagSum.Get(fData.Orthographic.YMag);
    fWorkData.Orthographic.ZFar:=OrthographicZFarSum.Get(fData.Orthographic.ZFar);
    fWorkData.Orthographic.ZNear:=OrthographicZNearSum.Get(fData.Orthographic.ZNear);
   end;
   TpvScene3D.TCameraData.TCameraType.Perspective:begin
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

{ TpvScene3D.TGroup.TInstance.TMaterial }

constructor TpvScene3D.TGroup.TInstance.TMaterial.Create(const aInstance:TpvScene3D.TGroup.TInstance;const aMaterial:TpvScene3D.TMaterial);
begin
 inherited Create;
 fInstance:=aInstance;
 fMaterial:=aMaterial;
 fData:=fMaterial.fData;
 fWorkData:=fMaterial.fData;
 fEffectiveData:=@fData;
 fOverwrites:=nil;
 fCountOverwrites:=0;
end;

destructor TpvScene3D.TGroup.TInstance.TMaterial.Destroy;
begin
 fOverwrites:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TInstance.TMaterial.Update;
var Index,AnimatedTextureIndex:TpvSizeInt;
    Factor:TpvDouble;
    Overwrite:TpvScene3D.TGroup.TInstance.TMaterial.PMaterialOverwrite;
    MaterialPBRMetallicRoughnessBaseColorFactorSum:TpvScene3D.TVector4Sum;
    MaterialPBRMetallicRoughnessMetallicFactorSum:TpvScene3D.TScalarSum;
    MaterialPBRMetallicRoughnessRoughnessFactorSum:TpvScene3D.TScalarSum;
    MaterialAlphaCutOffSum:TpvScene3D.TScalarSum;
    MaterialEmissiveFactorSum:TpvScene3D.TVector3Sum;
    MaterialNormalTextureScaleSum:TpvScene3D.TScalarSum;
    MaterialOcclusionTextureStrengthSum:TpvScene3D.TScalarSum;
    MaterialPBRClearCoatFactorSum:TpvScene3D.TScalarSum;
    MaterialPBRClearCoatRoughnessFactorSum:TpvScene3D.TScalarSum;
    MaterialEmissiveStrengthSum:TpvScene3D.TScalarSum;
    MaterialIORSum:TpvScene3D.TScalarSum;
    MaterialPBRIridescenceFactorSum:TpvScene3D.TScalarSum;
    MaterialPBRIridescenceIorSum:TpvScene3D.TScalarSum;
    MaterialPBRIridescenceMinimumSum:TpvScene3D.TScalarSum;
    MaterialPBRIridescenceMaximumSum:TpvScene3D.TScalarSum;
    MaterialPBRSheenColorFactorSum:TpvScene3D.TVector3Sum;
    MaterialPBRSheenRoughnessFactorSum:TpvScene3D.TScalarSum;
    MaterialPBRSpecularFactorSum:TpvScene3D.TScalarSum;
    MaterialPBRSpecularColorFactorSum:TpvScene3D.TVector3Sum;
    MaterialPBRTransmissionFactorSum:TpvScene3D.TScalarSum;
    MaterialPBRVolumeThicknessFactorSum:TpvScene3D.TScalarSum;
    MaterialPBRVolumeAttenuationDistanceSum:TpvScene3D.TScalarSum;
    MaterialPBRVolumeAttenuationColorSum:TpvScene3D.TVector3Sum;
    MaterialPBRAnisotropyStrengthSum:TpvScene3D.TScalarSum;
    MaterialPBRAnisotropyRotationSum:TpvScene3D.TScalarSum;
    AnimatedTextureMask:TpvUInt64;
    TextureTransform:TpvScene3D.TMaterial.TTextureReference.PTransform;
    WorkTextureTransform:TpvScene3D.TMaterial.TTextureReference.PTransform;
    DoUpdate:boolean;
begin
 DoUpdate:=false;
 if fCountOverwrites=0 then begin
  if fEffectiveData=@fWorkData then begin
   fWorkData:=fData;
   DoUpdate:=true;
  end;
  fEffectiveData:=@fData;
 end else begin
  fEffectiveData:=@fWorkData;
  MaterialPBRMetallicRoughnessBaseColorFactorSum.Clear;
  MaterialPBRMetallicRoughnessMetallicFactorSum.Clear;
  MaterialPBRMetallicRoughnessRoughnessFactorSum.Clear;
  MaterialAlphaCutOffSum.Clear;
  MaterialEmissiveFactorSum.Clear;
  MaterialNormalTextureScaleSum.Clear;
  MaterialOcclusionTextureStrengthSum.Clear;
  MaterialPBRClearCoatFactorSum.Clear;
  MaterialPBRClearCoatRoughnessFactorSum.Clear;
  MaterialEmissiveStrengthSum.Clear;
  MaterialIORSum.Clear;
  MaterialPBRIridescenceFactorSum.Clear;
  MaterialPBRIridescenceIorSum.Clear;
  MaterialPBRIridescenceMinimumSum.Clear;
  MaterialPBRIridescenceMaximumSum.Clear;
  MaterialPBRSheenColorFactorSum.Clear;
  MaterialPBRSheenRoughnessFactorSum.Clear;
  MaterialPBRSpecularFactorSum.Clear;
  MaterialPBRSpecularColorFactorSum.Clear;
  MaterialPBRTransmissionFactorSum.Clear;
  MaterialPBRVolumeThicknessFactorSum.Clear;
  MaterialPBRVolumeAttenuationDistanceSum.Clear;
  MaterialPBRVolumeAttenuationColorSum.Clear;
  MaterialPBRAnisotropyStrengthSum.Clear;
  MaterialPBRAnisotropyRotationSum.Clear;
  begin
   AnimatedTextureMask:=fData.AnimatedTextureMask;
   while AnimatedTextureMask<>0 do begin
    AnimatedTextureIndex:=TPasMPMath.FindFirstSetBit64(AnimatedTextureMask);
    fTextureOffsetSums[TpvScene3D.TTextureIndex(AnimatedTextureIndex)].Clear;
    fTextureRotationSums[TpvScene3D.TTextureIndex(AnimatedTextureIndex)].Clear;
    fTextureScaleSums[TpvScene3D.TTextureIndex(AnimatedTextureIndex)].Clear;
    AnimatedTextureMask:=AnimatedTextureMask and (AnimatedTextureMask-1);
   end;
  end;
  for Index:=0 to fCountOverwrites-1 do begin
   Overwrite:=@fOverwrites[Index];
   Factor:=Overwrite.Factor;
   if not IsZero(Factor) then begin
    if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.Defaults in Overwrite^.Flags then begin
     if Overwrite^.SubIndex<0 then begin
      // Material
      MaterialPBRMetallicRoughnessBaseColorFactorSum.Add(fData.PBRMetallicRoughness.BaseColorFactor,Factor);
      MaterialPBRMetallicRoughnessMetallicFactorSum.Add(fData.PBRMetallicRoughness.MetallicFactor,Factor);
      MaterialPBRMetallicRoughnessRoughnessFactorSum.Add(fData.PBRMetallicRoughness.RoughnessFactor,Factor);
      MaterialAlphaCutOffSum.Add(fData.AlphaCutOff,Factor);
      MaterialEmissiveFactorSum.Add(fData.EmissiveFactor.xyz,Factor);
      MaterialNormalTextureScaleSum.Add(fData.NormalTextureScale,Factor);
      MaterialOcclusionTextureStrengthSum.Add(fData.OcclusionTextureStrength,Factor);
      MaterialPBRClearCoatFactorSum.Add(fData.PBRClearCoat.Factor,Factor);
      MaterialPBRClearCoatRoughnessFactorSum.Add(fData.PBRClearCoat.RoughnessFactor,Factor);
      MaterialEmissiveStrengthSum.Add(fData.EmissiveFactor[3],Factor);
      MaterialIORSum.Add(fData.IOR,Factor);
      MaterialPBRIridescenceFactorSum.Add(fData.Iridescence.Factor,Factor);
      MaterialPBRIridescenceIorSum.Add(fData.Iridescence.Ior,Factor);
      MaterialPBRIridescenceMinimumSum.Add(fData.Iridescence.ThicknessMinimum,Factor);
      MaterialPBRIridescenceMaximumSum.Add(fData.Iridescence.ThicknessMaximum,Factor);
      MaterialPBRSheenColorFactorSum.Add(fData.PBRSheen.ColorFactor,Factor);
      MaterialPBRSheenRoughnessFactorSum.Add(fData.PBRSheen.RoughnessFactor,Factor);
      MaterialPBRSpecularFactorSum.Add(fData.PBRMetallicRoughness.SpecularFactor,Factor);
      MaterialPBRSpecularColorFactorSum.Add(fData.PBRMetallicRoughness.SpecularColorFactor,Factor);
      MaterialPBRTransmissionFactorSum.Add(fData.Transmission.Factor,Factor);
      MaterialPBRVolumeThicknessFactorSum.Add(fData.Volume.ThicknessFactor,Factor);
      MaterialPBRVolumeAttenuationColorSum.Add(fData.Volume.AttenuationColor,Factor);
      MaterialPBRVolumeAttenuationDistanceSum.Add(fData.Volume.AttenuationDistance,Factor);
      MaterialPBRAnisotropyStrengthSum.Add(fData.Anisotropy.AnisotropyStrength,Factor);
      MaterialPBRAnisotropyRotationSum.Add(fData.Anisotropy.AnisotropyRotation,Factor);
     end else begin
      // Texture
      TextureTransform:=fData.GetTextureTransform(TpvScene3D.TTextureIndex(Overwrite^.SubIndex));
      if assigned(TextureTransform) then begin
       fTextureOffsetSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(TextureTransform^.Offset,Factor);
       fTextureRotationSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(TextureTransform^.Rotation,Factor);
       fTextureScaleSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(TextureTransform^.Scale,Factor);
      end;
     end;
    end else begin
     if Overwrite^.SubIndex<0 then begin
      // Material
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessBaseColorFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRMetallicRoughnessBaseColorFactor in Overwrite^.Flags then begin
        MaterialPBRMetallicRoughnessBaseColorFactorSum.Add(fData.PBRMetallicRoughness.BaseColorFactor,Factor);
       end else begin
        MaterialPBRMetallicRoughnessBaseColorFactorSum.Add(Overwrite^.MaterialPBRMetallicRoughnessBaseColorFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessMetallicFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRMetallicRoughnessMetallicFactor in Overwrite^.Flags then begin
        MaterialPBRMetallicRoughnessMetallicFactorSum.Add(fData.PBRMetallicRoughness.MetallicFactor,Factor);
       end else begin
        MaterialPBRMetallicRoughnessMetallicFactorSum.Add(Overwrite^.MaterialPBRMetallicRoughnessMetallicFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessRoughnessFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRMetallicRoughnessRoughnessFactor in Overwrite^.Flags then begin
        MaterialPBRMetallicRoughnessRoughnessFactorSum.Add(fData.PBRMetallicRoughness.RoughnessFactor,Factor);
       end else begin
        MaterialPBRMetallicRoughnessRoughnessFactorSum.Add(Overwrite^.MaterialPBRMetallicRoughnessRoughnessFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialAlphaCutOff in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialAlphaCutOff in Overwrite^.Flags then begin
        MaterialAlphaCutOffSum.Add(fData.AlphaCutOff,Factor);
       end else begin
        MaterialAlphaCutOffSum.Add(Overwrite^.MaterialAlphaCutOff,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialEmissiveFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialEmissiveFactor in Overwrite^.Flags then begin
        MaterialEmissiveFactorSum.Add(fData.EmissiveFactor.xyz,Factor);
       end else begin
        MaterialEmissiveFactorSum.Add(Overwrite^.MaterialEmissiveFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialNormalTextureScale in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialNormalTextureScale in Overwrite^.Flags then begin
        MaterialNormalTextureScaleSum.Add(fData.NormalTextureScale,Factor);
       end else begin
        MaterialNormalTextureScaleSum.Add(Overwrite^.MaterialNormalTextureScale,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialOcclusionTextureStrength in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialOcclusionTextureStrength in Overwrite^.Flags then begin
        MaterialOcclusionTextureStrengthSum.Add(fData.OcclusionTextureStrength,Factor);
       end else begin
        MaterialOcclusionTextureStrengthSum.Add(Overwrite^.MaterialOcclusionTextureStrength,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRClearCoatFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRClearCoatFactor in Overwrite^.Flags then begin
        MaterialPBRClearCoatFactorSum.Add(fData.PBRClearCoat.Factor,Factor);
       end else begin
        MaterialPBRClearCoatFactorSum.Add(Overwrite^.MaterialPBRClearCoatFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRClearCoatRoughnessFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRClearCoatRoughnessFactor in Overwrite^.Flags then begin
        MaterialPBRClearCoatRoughnessFactorSum.Add(fData.PBRClearCoat.RoughnessFactor,Factor);
       end else begin
        MaterialPBRClearCoatRoughnessFactorSum.Add(Overwrite^.MaterialPBRClearCoatRoughnessFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialEmissiveStrength in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialEmissiveStrength in Overwrite^.Flags then begin
        MaterialEmissiveStrengthSum.Add(fData.EmissiveFactor[3],Factor);
       end else begin
        MaterialEmissiveStrengthSum.Add(Overwrite^.MaterialEmissiveStrength,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialIOR in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialIOR in Overwrite^.Flags then begin
        MaterialIORSum.Add(fData.IOR,Factor);
       end else begin
        MaterialIORSum.Add(Overwrite^.MaterialIOR,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceFactor in Overwrite^.Flags then begin
        MaterialPBRIridescenceFactorSum.Add(fData.Iridescence.Factor,Factor);
       end else begin
        MaterialPBRIridescenceFactorSum.Add(Overwrite^.MaterialPBRIridescenceFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceIor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceIor in Overwrite^.Flags then begin
        MaterialPBRIridescenceIorSum.Add(fData.Iridescence.Ior,Factor);
       end else begin
        MaterialPBRIridescenceIorSum.Add(Overwrite^.MaterialPBRIridescenceIor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceMinimum in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceMinimum in Overwrite^.Flags then begin
        MaterialPBRIridescenceMinimumSum.Add(fData.Iridescence.ThicknessMinimum,Factor);
       end else begin
        MaterialPBRIridescenceMinimumSum.Add(Overwrite^.MaterialPBRIridescenceMinimum,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceMaximum in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceMaximum in Overwrite^.Flags then begin
        MaterialPBRIridescenceMaximumSum.Add(fData.Iridescence.ThicknessMaximum,Factor);
       end else begin
        MaterialPBRIridescenceMaximumSum.Add(Overwrite^.MaterialPBRIridescenceMaximum,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSheenColorFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSheenColorFactor in Overwrite^.Flags then begin
        MaterialPBRSheenColorFactorSum.Add(fData.PBRSheen.ColorFactor,Factor);
       end else begin
        MaterialPBRSheenColorFactorSum.Add(Overwrite^.MaterialPBRSheenColorFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSheenRoughnessFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSheenRoughnessFactor in Overwrite^.Flags then begin
        MaterialPBRSheenRoughnessFactorSum.Add(fData.PBRSheen.RoughnessFactor,Factor);
       end else begin
        MaterialPBRSheenRoughnessFactorSum.Add(Overwrite^.MaterialPBRSheenRoughnessFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSpecularFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSpecularFactor in Overwrite^.Flags then begin
        MaterialPBRSpecularFactorSum.Add(fData.PBRMetallicRoughness.SpecularFactor,Factor);
       end else begin
        MaterialPBRSpecularFactorSum.Add(Overwrite^.MaterialPBRSpecularFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSpecularColorFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSpecularColorFactor in Overwrite^.Flags then begin
        MaterialPBRSpecularColorFactorSum.Add(fData.PBRMetallicRoughness.SpecularColorFactor,Factor);
       end else begin
        MaterialPBRSpecularColorFactorSum.Add(Overwrite^.MaterialPBRSpecularColorFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRTransmissionFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRTransmissionFactor in Overwrite^.Flags then begin
        MaterialPBRTransmissionFactorSum.Add(fData.Transmission.Factor,Factor);
       end else begin
        MaterialPBRTransmissionFactorSum.Add(Overwrite^.MaterialPBRTransmissionFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeThicknessFactor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRVolumeThicknessFactor in Overwrite^.Flags then begin
        MaterialPBRVolumeThicknessFactorSum.Add(fData.Volume.ThicknessFactor,Factor);
       end else begin
        MaterialPBRVolumeThicknessFactorSum.Add(Overwrite^.MaterialPBRVolumeThicknessFactor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeAttenuationDistance in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRVolumeAttenuationDistance in Overwrite^.Flags then begin
        MaterialPBRVolumeAttenuationDistanceSum.Add(fData.Volume.AttenuationDistance,Factor);
       end else begin
        MaterialPBRVolumeAttenuationDistanceSum.Add(Overwrite^.MaterialPBRVolumeAttenuationDistance,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeAttenuationColor in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRVolumeAttenuationColor in Overwrite^.Flags then begin
        MaterialPBRVolumeAttenuationColorSum.Add(fData.Volume.AttenuationColor,Factor);
       end else begin
        MaterialPBRVolumeAttenuationColorSum.Add(Overwrite^.MaterialPBRVolumeAttenuationColor,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRAnisotropyStrength in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRAnisotropyStrength in Overwrite^.Flags then begin
        MaterialPBRAnisotropyStrengthSum.Add(fData.Anisotropy.AnisotropyStrength,Factor);
       end else begin
        MaterialPBRAnisotropyStrengthSum.Add(Overwrite^.MaterialPBRAnisotropyStrength,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRAnisotropyRotation in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRAnisotropyRotation in Overwrite^.Flags then begin
        MaterialPBRAnisotropyRotationSum.Add(fData.Anisotropy.AnisotropyRotation,Factor);
       end else begin
        MaterialPBRAnisotropyRotationSum.Add(Overwrite^.MaterialPBRAnisotropyRotation,Factor);
       end;
      end;
     end else begin
      // Texture
      TextureTransform:=fData.GetTextureTransform(TpvScene3D.TTextureIndex(Overwrite^.SubIndex));
      if assigned(TextureTransform) then begin
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureOffset in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultTextureOffset in Overwrite^.Flags then begin
         fTextureOffsetSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(TextureTransform^.Offset,Factor);
        end else begin
         fTextureOffsetSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(Overwrite^.TextureOffset,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureRotation in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultTextureRotation in Overwrite^.Flags then begin
         fTextureRotationSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(TextureTransform^.Rotation,Factor);
        end else begin
         fTextureRotationSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(Overwrite^.TextureRotation,Factor);
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureScale in Overwrite^.Flags then begin
        if TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultTextureScale in Overwrite^.Flags then begin
         fTextureScaleSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(TextureTransform^.Scale,Factor);
        end else begin
         fTextureScaleSums[TpvScene3D.TTextureIndex(Overwrite^.SubIndex)].Add(Overwrite^.TextureScale,Factor);
        end;
       end;
      end;
     end;
    end;
   end;
  end;
  fWorkData.PBRMetallicRoughness.BaseColorFactor:=MaterialPBRMetallicRoughnessBaseColorFactorSum.Get(fData.PBRMetallicRoughness.BaseColorFactor);
  fWorkData.PBRMetallicRoughness.MetallicFactor:=MaterialPBRMetallicRoughnessMetallicFactorSum.Get(fData.PBRMetallicRoughness.MetallicFactor);
  fWorkData.PBRMetallicRoughness.RoughnessFactor:=MaterialPBRMetallicRoughnessRoughnessFactorSum.Get(fData.PBRMetallicRoughness.RoughnessFactor);
  fWorkData.AlphaCutOff:=MaterialAlphaCutOffSum.Get(fData.AlphaCutOff);
  fWorkData.EmissiveFactor.xyz:=MaterialEmissiveFactorSum.Get(fData.EmissiveFactor.xyz);
  fWorkData.NormalTextureScale:=MaterialNormalTextureScaleSum.Get(fData.NormalTextureScale);
  fWorkData.OcclusionTextureStrength:=MaterialOcclusionTextureStrengthSum.Get(fData.OcclusionTextureStrength);
  fWorkData.PBRClearCoat.Factor:=MaterialOcclusionTextureStrengthSum.Get(fData.PBRClearCoat.Factor);
  fWorkData.PBRClearCoat.RoughnessFactor:=MaterialPBRClearCoatRoughnessFactorSum.Get(fData.PBRClearCoat.RoughnessFactor);
  fWorkData.EmissiveFactor[3]:=MaterialEmissiveStrengthSum.Get(fData.EmissiveFactor[3]);
  fWorkData.IOR:=MaterialIORSum.Get(fData.IOR);
  fWorkData.Iridescence.Factor:=MaterialPBRIridescenceFactorSum.Get(fData.Iridescence.Factor);
  fWorkData.Iridescence.Ior:=MaterialPBRIridescenceIorSum.Get(fData.Iridescence.Ior);
  fWorkData.Iridescence.ThicknessMinimum:=MaterialPBRIridescenceMinimumSum.Get(fData.Iridescence.ThicknessMinimum);
  fWorkData.Iridescence.ThicknessMaximum:=MaterialPBRIridescenceMaximumSum.Get(fData.Iridescence.ThicknessMaximum);
  fWorkData.PBRSheen.ColorFactor:=MaterialPBRSheenColorFactorSum.Get(fData.PBRSheen.ColorFactor);
  fWorkData.PBRSheen.RoughnessFactor:=MaterialPBRSheenRoughnessFactorSum.Get(fData.PBRSheen.RoughnessFactor);
  fWorkData.PBRMetallicRoughness.SpecularFactor:=MaterialPBRSpecularFactorSum.Get(fData.PBRMetallicRoughness.SpecularFactor);
  fWorkData.PBRMetallicRoughness.SpecularColorFactor:=MaterialPBRSpecularColorFactorSum.Get(fData.PBRMetallicRoughness.SpecularColorFactor);
  fWorkData.Transmission.Factor:=MaterialPBRTransmissionFactorSum.Get(fData.Transmission.Factor);
  fWorkData.Volume.ThicknessFactor:=MaterialPBRVolumeThicknessFactorSum.Get(fData.Volume.ThicknessFactor);
  fWorkData.Volume.AttenuationColor:=MaterialPBRVolumeAttenuationColorSum.Get(fData.Volume.AttenuationColor);
  fWorkData.Volume.AttenuationDistance:=MaterialPBRVolumeAttenuationDistanceSum.Get(fData.Volume.AttenuationDistance);
  fWorkData.Anisotropy.AnisotropyStrength:=MaterialPBRAnisotropyStrengthSum.Get(fWorkData.Anisotropy.AnisotropyStrength);
  fWorkData.Anisotropy.AnisotropyRotation:=MaterialPBRAnisotropyRotationSum.Get(fWorkData.Anisotropy.AnisotropyRotation);
  begin
   AnimatedTextureMask:=fData.AnimatedTextureMask;
   while AnimatedTextureMask<>0 do begin
    AnimatedTextureIndex:=TPasMPMath.FindFirstSetBit64(AnimatedTextureMask);
    TextureTransform:=fData.GetTextureTransform(TpvScene3D.TTextureIndex(AnimatedTextureIndex));
    if assigned(TextureTransform) then begin
     WorkTextureTransform:=fWorkData.GetTextureTransform(TpvScene3D.TTextureIndex(AnimatedTextureIndex));
     if assigned(WorkTextureTransform) then begin
      WorkTextureTransform^.Offset:=fTextureOffsetSums[TpvScene3D.TTextureIndex(AnimatedTextureIndex)].Get(TextureTransform^.Offset);
      WorkTextureTransform^.Rotation:=fTextureRotationSums[TpvScene3D.TTextureIndex(AnimatedTextureIndex)].Get(TextureTransform^.Rotation);
      WorkTextureTransform^.Scale:=fTextureScaleSums[TpvScene3D.TTextureIndex(AnimatedTextureIndex)].Get(TextureTransform^.Scale);
     end;
    end;
    AnimatedTextureMask:=AnimatedTextureMask and (AnimatedTextureMask-1);
   end;
  end;
  DoUpdate:=true;
 end;
 if DoUpdate then begin
  fMaterial.fData:=fEffectiveData^;
  fMaterial.FillShaderData;
 end;
end;

{ TpvScene3D.TGroup.TInstance.TScene }

constructor TpvScene3D.TGroup.TInstance.TScene.Create(const aInstance:TpvScene3D.TGroup.TInstance;const aScene:TpvScene3D.TGroup.TScene);
begin
 inherited Create;

 fInstance:=aInstance;

 fScene:=aScene;

 fDrawChoreographyBatchItems:=TpvScene3D.TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchItems.OwnsObjects:=true;
 fDrawChoreographyBatchItems.GroupInstanceClone(fScene.fDrawChoreographyBatchItems,fInstance,false);

 fDrawChoreographyBatchUniqueItems:=TpvScene3D.TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchUniqueItems.OwnsObjects:=true;
 fDrawChoreographyBatchUniqueItems.GroupInstanceClone(fScene.fDrawChoreographyBatchUniqueItems,fInstance,true);

end;

destructor TpvScene3D.TGroup.TInstance.TScene.Destroy;
begin

 FreeAndNil(fDrawChoreographyBatchItems);

 FreeAndNil(fDrawChoreographyBatchUniqueItems);

 inherited Destroy;
end;

{ TpvScene3D.TGroup.TInstance.TRenderInstance }

constructor TpvScene3D.TGroup.TInstance.TRenderInstance.Create(const aInstance:TpvScene3D.TGroup.TInstance);
begin
 inherited Create;
 fInstance:=aInstance;
 fActive:=true;
 fFirst:=true;
 fIndex:=-1;
 fPotentiallyVisibleSetNodeIndex:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
 fModelMatrix:=TpvMatrix4x4.Identity;
 fPreviousModelMatrix:=TpvMatrix4x4.Identity;
end;

destructor TpvScene3D.TGroup.TInstance.TRenderInstance.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TInstance.TRenderInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fInstance) then begin
  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fInstance.fRenderInstanceLock);
  try
   if assigned(fInstance.fRenderInstances) then begin
    fIndex:=fInstance.fRenderInstances.Add(self);
   end else begin
    fIndex:=-1;
   end;
  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fInstance.fRenderInstanceLock);
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.TRenderInstance.BeforeDestruction;
var LastIndex:TpvSizeInt;
    OtherRenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
begin
 if (fIndex>=0) and assigned(fInstance) then begin
  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fInstance.fRenderInstanceLock);
  try
   try
    if assigned(fInstance.fRenderInstances) then begin
     if fInstance.fRenderInstances.Count>1 then begin
      LastIndex:=fInstance.fRenderInstances.Count-1;
      if fIndex<>LastIndex then begin
       OtherRenderInstance:=fInstance.fRenderInstances[LastIndex];
       fInstance.fRenderInstances.Exchange(fIndex,LastIndex);
       OtherRenderInstance.fIndex:=fIndex;
       fIndex:=LastIndex;
      end;
     end;
     fInstance.fRenderInstances.ExtractIndex(fIndex);
    end;
   finally
    fIndex:=-1;
   end;
  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fInstance.fRenderInstanceLock);
  end;
 end;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TGroup.TInstance.TRenderInstance.Remove;
begin
 Free;
end;

{ TpvScene3D.TGroup.TInstance.TAnimation }

constructor TpvScene3D.TGroup.TInstance.TAnimation.Create;
begin
 inherited Create;
 fChannelOverwrites:=nil;
 fTime:=0.0;
 fLastIndices:=nil;
 fShadowTime:=0.0;
 fComplete:=false;
end;

destructor TpvScene3D.TGroup.TInstance.TAnimation.Destroy;
begin
 fLastIndices:=nil;
 fChannelOverwrites:=nil;
 inherited Destroy;
end;

{ TpvScene3D.TGroup.TInstance }

constructor TpvScene3D.TGroup.TInstance.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aMetaResource:TpvMetaResource=nil;const aHeadless:Boolean=false);
var Index,OtherIndex,MaterialIndex,MaterialIDMapArrayIndex:TpvSizeInt;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    Node:TpvScene3D.TGroup.TNode;
    Animation:TpvScene3D.TGroup.TAnimation;
    Light:TpvScene3D.TGroup.TInstance.TLight;
    Camera:TpvScene3D.TGroup.TInstance.TCamera;
    InstanceMaterial:TpvScene3D.TGroup.TInstance.TMaterial;
    MaterialToDuplicate,DuplicatedMaterial,Material:TpvScene3D.TMaterial;
    MaterialIDMapArray:TpvScene3D.TGroup.TMaterialIDMapArray;
    Generation:TpvUInt32;
    SrcVertex,DstVertex:PVertex;
    DstDynamicVertex:PGPUDynamicVertex;
    DstStaticVertex:PGPUStaticVertex;
    SrcMorphTargetVertex,DstMorphTargetVertex:PMorphTargetVertex;
    SrcJointBlock,DstJointBlock:PJointBlock;
begin
 inherited Create(aResourceManager,aParent,aMetaResource);

 if aParent is TGroup then begin
  fGroup:=TpvScene3D.TGroup(aParent);
 end else begin
  fGroup:=nil;
 end;

 fLock:=TPasMPSpinLock.Create;

 fActive:=true;

 fHeadless:=aHeadless;

 fUseRenderInstances:=false;

 fPreviousActive:=false;

 fUploaded:=false;

 fModelMatrix:=TpvMatrix4x4.Identity;

 fDirtyCounter:=1;

 fScene:=-1;

 fNodes:=nil;

 fSkins:=nil;

 fMaterialMap:=nil;

 fDuplicatedMaterials:=TpvScene3D.TMaterials.Create;
 fDuplicatedMaterials.OwnsObjects:=false;

 fRenderInstanceLock:=0;

 fRenderInstances:=TpvScene3D.TGroup.TInstance.TRenderInstances.Create;
 fRenderInstances.OwnsObjects:=true;

 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fPerInFlightFrameRenderInstances[Index].Initialize;
 end;

 FillChar(fVulkanPerInFlightFrameFirstInstances,SizeOf(fVulkanPerInFlightFrameFirstInstances),#0);

 FillChar(fVulkanPerInFlightFrameInstancesCounts,SizeOf(fVulkanPerInFlightFrameInstancesCounts),#0);

 fAnimations:=nil;

 begin

  if fGroup.fMaterialsToDuplicate.Count=0 then begin

   fMaterialMap:=fGroup.fMaterialMap;

  end else begin

   fMaterialMap:=copy(fGroup.fMaterialMap,0,length(fGroup.fMaterialMap));

   for Index:=0 to fGroup.fMaterialsToDuplicate.Count-1 do begin
    MaterialToDuplicate:=fGroup.fMaterialsToDuplicate[Index];
    if assigned(MaterialToDuplicate) then begin
     MaterialIDMapArrayIndex:=fGroup.fMaterialIDMapArrayIndexHashMap[MaterialToDuplicate.fID];
     if (MaterialIDMapArrayIndex>=0) and (MaterialIDMapArrayIndex<fGroup.fMaterialIDMapArrays.Count) then begin
      MaterialIDMapArray:=fGroup.fMaterialIDMapArrays[MaterialIDMapArrayIndex];
      DuplicatedMaterial:=TpvScene3D.TMaterial.Create(ResourceManager,fSceneInstance);
      try
       DuplicatedMaterial.Assign(MaterialToDuplicate);
       for MaterialIndex in MaterialIDMapArray do begin
        fMaterialMap[MaterialIndex+1]:=DuplicatedMaterial.fID;
       end;
      finally
       try
        DuplicatedMaterial.IncRef;
       finally
        fDuplicatedMaterials.Add(DuplicatedMaterial);
       end;
      end;
     end;
    end;

   end;

  end;

  fMaterials:=TpvScene3D.TGroup.TInstance.TMaterials.Create;
  fMaterials.OwnsObjects:=true;
  for Index:=0 to fGroup.fMaterials.Count-1 do begin
   InstanceMaterial:=nil;
   try
    if fMaterialMap[Index+1]<>fGroup.fMaterialMap[Index+1] then begin
     Material:=fSceneInstance.fMaterialIDHashMap[fMaterialMap[Index+1]];
     if assigned(Material) then begin
      InstanceMaterial:=TpvScene3D.TGroup.TInstance.TMaterial.Create(self,Material);
      try
       SetLength(InstanceMaterial.fOverwrites,fGroup.fAnimations.Count+1);
      finally
      end;
     end;
    end;
   finally
    fMaterials.Add(InstanceMaterial);
   end;
  end;

 end;

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
  InstanceNode^.Flags:=[];
  SetLength(InstanceNode^.WorkWeights,length(Node.fWeights));
  SetLength(InstanceNode^.OverwriteWeightsSum,length(Node.fWeights));
  SetLength(InstanceNode^.Overwrites,fGroup.fAnimations.Count+1);
  for OtherIndex:=0 to fGroup.fAnimations.Count do begin
   SetLength(InstanceNode^.Overwrites[OtherIndex].Weights,length(Node.fWeights));
  end;
  for OtherIndex:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
   InstanceNode^.PotentiallyVisibleSetNodeIndices[OtherIndex]:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
   InstanceNode^.CacheVerticesGenerations[OtherIndex]:=0;
  end;
  InstanceNode^.CacheVerticesGeneration:=1;
  InstanceNode^.CacheVerticesDirtyCounter:=1;
  InstanceNode^.AABBTreeProxy:=-1;
 end;

 fSceneInstance.fCullObjectIDLock.Acquire;
 try
  for Index:=0 to fGroup.fNodes.Count-1 do begin
   Node:=fGroup.fNodes[Index];
   InstanceNode:=@fNodes[Index];
   if assigned(Node.Mesh) then begin
    InstanceNode^.CullObjectID:=fSceneInstance.fCullObjectIDManager.AllocateID;
    if fSceneInstance.fMaxCullObjectID<InstanceNode^.CullObjectID then begin
     fSceneInstance.fMaxCullObjectID:=InstanceNode^.CullObjectID;
    end;
   end else begin
    InstanceNode^.CullObjectID:=0;
   end;
  end;
 finally
  fSceneInstance.fCullObjectIDLock.Release;
 end;

 SetLength(fAnimations,fGroup.fAnimations.Count+1);
 for Index:=0 to length(fAnimations)-1 do begin
  fAnimations[Index]:=TpvScene3D.TGroup.TInstance.TAnimation.Create;
  if Index>0 then begin
   Animation:=fGroup.fAnimations[Index-1];
   SetLength(fAnimations[Index].fLastIndices,length(Animation.fChannels));
   for OtherIndex:=0 to length(fAnimations[Index].fLastIndices)-1 do begin
    fAnimations[Index].fLastIndices[OtherIndex]:=0;
   end;
   SetLength(fAnimations[Index].fChannelOverwrites,length(Animation.fChannels)+length(Animation.fDefaultChannels));
  end;
 end;

 fAnimations[0].Factor:=1.0;

 fNodeMatrices:=nil;

 fMorphTargetVertexWeights:=nil;

 SetLength(fNodeMatrices,fGroup.fNodes.Count+fGroup.fCountJointNodeMatrices+1);

 SetLength(fMorphTargetVertexWeights,Max(Max(fGroup.fMorphTargetCount,fGroup.fCountNodeWeights),1));

 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fPotentiallyVisibleSetNodeIndices[Index]:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
 end;

 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fCullVisibleBitmapLocks[Index]:=0;
  fCullVisibleBitmaps[Index]:=nil;
  if fGroup.fDynamicAABBTreeCulling then begin
   SetLength(fCullVisibleBitmaps[Index],(length(fNodes)+31) shr 5);
  end;
 end;

 fAABBTreeProxy:=-1;

 SetLength(fCacheVerticesNodeDirtyBitmap,((length(fNodes)+31) shr 5)+1);

 fOnNodeFilter:=nil;

 if not aHeadless then begin

  repeat
   Generation:=TPasMPInterlocked.Increment(fGroup.fSceneInstance.fMeshGenerationCounter);
  until Generation<>0;

  fSceneInstance.fBufferRangeAllocatorLock.Acquire;
  try

   fVulkanVertexBufferCount:=fGroup.fVertices.Count;
   fVulkanVertexBufferOffset:=fSceneInstance.fVulkanVertexBufferRangeAllocator.Allocate(fVulkanVertexBufferCount);
// writeln(fVulkanVertexBufferOffset);

   fVulkanDrawIndexBufferCount:=fGroup.fDrawChoreographyBatchCondensedIndices.Count;
   fVulkanDrawIndexBufferOffset:=fSceneInstance.fVulkanDrawIndexBufferRangeAllocator.Allocate(fVulkanDrawIndexBufferCount);

   fVulkanDrawUniqueIndexBufferCount:=fGroup.fDrawChoreographyBatchCondensedUniqueIndices.Count;
   fVulkanDrawUniqueIndexBufferOffset:=fSceneInstance.fVulkanDrawUniqueIndexBufferRangeAllocator.Allocate(fVulkanDrawUniqueIndexBufferCount);

   fVulkanMorphTargetVertexBufferCount:=fGroup.fMorphTargetVertices.Count;
   fVulkanMorphTargetVertexBufferOffset:=fSceneInstance.fVulkanMorphTargetVertexBufferRangeAllocator.Allocate(fVulkanMorphTargetVertexBufferCount);

   fVulkanJointBlockBufferCount:=fGroup.fJointBlocks.Count;
   fVulkanJointBlockBufferOffset:=fSceneInstance.fVulkanJointBlockBufferRangeAllocator.Allocate(fVulkanJointBlockBufferCount);

   fVulkanNodeMatricesBufferCount:=length(fNodeMatrices);
   fVulkanNodeMatricesBufferOffset:=fSceneInstance.fVulkanNodeMatricesBufferRangeAllocator.Allocate(fVulkanNodeMatricesBufferCount);

   fVulkanMorphTargetVertexWeightsBufferCount:=length(fMorphTargetVertexWeights);
   fVulkanMorphTargetVertexWeightsBufferOffset:=fSceneInstance.fVulkanMorphTargetVertexWeightsBufferRangeAllocator.Allocate(fVulkanMorphTargetVertexWeightsBufferCount);

   if fSceneInstance.fVulkanDynamicVertexBufferData.Count<(fVulkanVertexBufferOffset+fVulkanVertexBufferCount) then begin
    fSceneInstance.fVulkanDynamicVertexBufferData.Resize(fVulkanVertexBufferOffset+fVulkanVertexBufferCount);
   end;

   if fSceneInstance.fVulkanStaticVertexBufferData.Count<(fVulkanVertexBufferOffset+fVulkanVertexBufferCount) then begin
    fSceneInstance.fVulkanStaticVertexBufferData.Resize(fVulkanVertexBufferOffset+fVulkanVertexBufferCount);
   end;

   if fSceneInstance.fVulkanDrawIndexBufferData.Count<(fVulkanDrawIndexBufferOffset+fVulkanDrawIndexBufferCount) then begin
    fSceneInstance.fVulkanDrawIndexBufferData.Resize(fVulkanDrawIndexBufferOffset+fVulkanDrawIndexBufferCount);
   end;

   if fSceneInstance.fVulkanDrawUniqueIndexBufferData.Count<(fVulkanDrawUniqueIndexBufferOffset+fVulkanDrawUniqueIndexBufferCount) then begin
    fSceneInstance.fVulkanDrawUniqueIndexBufferData.Resize(fVulkanDrawUniqueIndexBufferOffset+fVulkanDrawUniqueIndexBufferCount);
   end;

   if fSceneInstance.fVulkanMorphTargetVertexBufferData.Count<(fVulkanMorphTargetVertexBufferOffset+fVulkanMorphTargetVertexBufferCount) then begin
    fSceneInstance.fVulkanMorphTargetVertexBufferData.Resize(fVulkanMorphTargetVertexBufferOffset+fVulkanMorphTargetVertexBufferCount);
   end;

   if fSceneInstance.fVulkanJointBlockBufferData.Count<(fVulkanJointBlockBufferOffset+fVulkanJointBlockBufferCount) then begin
    fSceneInstance.fVulkanJointBlockBufferData.Resize(fVulkanJointBlockBufferOffset+fVulkanJointBlockBufferCount);
   end;

   for Index:=0 to fSceneInstance.CountInFlightFrames-1 do begin

    if fSceneInstance.fVulkanNodeMatricesBufferData[Index].Count<(fVulkanNodeMatricesBufferOffset+fVulkanNodeMatricesBufferCount) then begin
     fSceneInstance.fVulkanNodeMatricesBufferData[Index].Resize(fVulkanNodeMatricesBufferOffset+fVulkanNodeMatricesBufferCount);
    end;

    if fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[Index].Count<(fVulkanMorphTargetVertexBufferOffset+fVulkanMorphTargetVertexBufferCount) then begin
     fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[Index].Resize(fVulkanMorphTargetVertexBufferOffset+fVulkanMorphTargetVertexBufferCount);
    end;

   end;

   for Index:=0 to fGroup.fVertices.Count-1 do begin

    SrcVertex:=@fGroup.fVertices.Items[Index];

    DstDynamicVertex:=@fSceneInstance.fVulkanDynamicVertexBufferData.Items[fVulkanVertexBufferOffset+Index];
    DstDynamicVertex^.Position:=SrcVertex^.Position;
    if SrcVertex^.MorphTargetVertexBaseIndex<>TpvUInt32($ffffffff) then begin
     DstDynamicVertex^.MorphTargetVertexBaseIndex:=SrcVertex^.MorphTargetVertexBaseIndex+fVulkanMorphTargetVertexBufferOffset;
    end else begin
     DstDynamicVertex^.MorphTargetVertexBaseIndex:=TpvUInt32($ffffffff);
    end;
    if (SrcVertex^.JointBlockBaseIndex<>TpvUInt32($ffffffff)) and (SrcVertex^.CountJointBlocks>0) then begin
     DstDynamicVertex^.JointBlockBaseIndex:=SrcVertex^.JointBlockBaseIndex+fVulkanJointBlockBufferOffset;
     DstDynamicVertex^.CountJointBlocks:=SrcVertex^.CountJointBlocks;
    end else begin
     DstDynamicVertex^.JointBlockBaseIndex:=TpvUInt32($ffffffff);
     DstDynamicVertex^.CountJointBlocks:=0;
    end;
    DstDynamicVertex^.RootNode:=fVulkanNodeMatricesBufferOffset;
    DstDynamicVertex^.NodeIndex:=SrcVertex^.NodeIndex+fVulkanNodeMatricesBufferOffset;
    DstDynamicVertex^.Normal:=SrcVertex^.Normal;
    DstDynamicVertex^.Tangent:=SrcVertex^.Tangent;
    DstDynamicVertex^.Flags:=SrcVertex^.Flags;
    DstDynamicVertex^.Generation:=Generation;

    DstStaticVertex:=@fSceneInstance.fVulkanStaticVertexBufferData.Items[fVulkanVertexBufferOffset+Index];
    DstStaticVertex^.TexCoord0:=SrcVertex^.TexCoord0;
    DstStaticVertex^.TexCoord1:=SrcVertex^.TexCoord1;
    DstStaticVertex^.Color0:=SrcVertex^.Color0;
    DstStaticVertex^.MaterialID:=fMaterialMap[SrcVertex^.MaterialID];
    DstStaticVertex^.Unused0:=0;

   end;

   for Index:=0 to fGroup.fDrawChoreographyBatchCondensedIndices.Count-1 do begin
    fSceneInstance.fVulkanDrawIndexBufferData.Items[fVulkanDrawIndexBufferOffset+Index]:=fGroup.fDrawChoreographyBatchCondensedIndices.Items[Index]+fVulkanVertexBufferOffset;
   end;

   for Index:=0 to fGroup.fDrawChoreographyBatchCondensedUniqueIndices.Count-1 do begin
    fSceneInstance.fVulkanDrawUniqueIndexBufferData.Items[fVulkanDrawUniqueIndexBufferOffset+Index]:=fGroup.fDrawChoreographyBatchCondensedUniqueIndices.Items[Index]+fVulkanVertexBufferOffset;
   end;

   for Index:=0 to fGroup.fMorphTargetVertices.Count-1 do begin
    SrcMorphTargetVertex:=@fGroup.fMorphTargetVertices.Items[Index];
    DstMorphTargetVertex:=@fSceneInstance.fVulkanMorphTargetVertexBufferData.Items[fVulkanMorphTargetVertexBufferOffset+Index];
    DstMorphTargetVertex^:=SrcMorphTargetVertex^;
    inc(DstMorphTargetVertex^.Index,fVulkanMorphTargetVertexWeightsBufferOffset);
    if DstMorphTargetVertex^.Next<>TpvUInt32($ffffffff) then begin
     inc(DstMorphTargetVertex^.Next,fVulkanMorphTargetVertexBufferOffset);
    end;
   end;

   for Index:=0 to fGroup.fJointBlocks.Count-1 do begin
    SrcJointBlock:=@fGroup.fJointBlocks.Items[Index];
    DstJointBlock:=@fSceneInstance.fVulkanJointBlockBufferData.Items[fVulkanJointBlockBufferOffset+Index];
    DstJointBlock^:=SrcJointBlock^;
    inc(DstJointBlock^.Joints[0],fVulkanNodeMatricesBufferOffset);
    inc(DstJointBlock^.Joints[1],fVulkanNodeMatricesBufferOffset);
    inc(DstJointBlock^.Joints[2],fVulkanNodeMatricesBufferOffset);
    inc(DstJointBlock^.Joints[3],fVulkanNodeMatricesBufferOffset);
   end;

  finally
   fSceneInstance.fBufferRangeAllocatorLock.Release;
  end;

 end else begin

  fVulkanVertexBufferOffset:=-1;
  fVulkanDrawIndexBufferOffset:=-1;
  fVulkanDrawUniqueIndexBufferOffset:=-1;
  fVulkanMorphTargetVertexBufferOffset:=-1;
  fVulkanJointBlockBufferOffset:=-1;
  fVulkanNodeMatricesBufferOffset:=-1;
  fVulkanMorphTargetVertexWeightsBufferOffset:=-1;
  fVulkanVertexBufferCount:=0;
  fVulkanDrawIndexBufferCount:=0;
  fVulkanDrawUniqueIndexBufferCount:=0;
  fVulkanMorphTargetVertexBufferCount:=0;
  fVulkanJointBlockBufferCount:=0;
  fVulkanNodeMatricesBufferCount:=0;
  fVulkanMorphTargetVertexWeightsBufferCount:=0;

 end;

 fScenes:=TpvScene3D.TGroup.TInstance.TScenes.Create;
 fScenes.OwnsObjects:=true;
 for Index:=0 to fGroup.fScenes.Count-1 do begin
  fScenes.Add(TpvScene3D.TGroup.TInstance.TScene.Create(self,fGroup.fScenes[Index]));
 end;

 if fGroup.fDynamicAABBTreeCulling then begin
  fAABBTree:=TpvBVHDynamicAABBTree.Create;
 end else begin
  fAABBTree:=nil;
 end;

 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
  fAABBTreeStates[Index].Root:=-1;
  fAABBTreeStates[Index].Generation:=High(TpvUInt64);
  fAABBTreeSkipLists[Index].Items:=nil;
  fAABBTreeSkipLists[Index].Count:=0;
 end;

end;

destructor TpvScene3D.TGroup.TInstance.Destroy;
var Index:TPasGLTFSizeInt;
    RenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
begin

 Unload;

 if assigned(fRenderInstances) then begin
  TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fRenderInstanceLock);
  try
   try
    while fRenderInstances.Count>0 do begin
     RenderInstance:=fRenderInstances.ExtractIndex(fRenderInstances.Count-1);
     try
      RenderInstance.fIndex:=-1;
     finally
      FreeAndNil(RenderInstance);
     end;
    end;
   finally
    FreeAndNil(fRenderInstances);
   end;
  finally
   TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fRenderInstanceLock);
  end;
 end;

 if assigned(fSceneInstance) and not fHeadless then begin
  fSceneInstance.fBufferRangeAllocatorLock.Acquire;
  try
   fSceneInstance.fVulkanVertexBufferRangeAllocator.Release(fVulkanVertexBufferOffset);
   fSceneInstance.fVulkanDrawIndexBufferRangeAllocator.Release(fVulkanDrawIndexBufferOffset);
   fSceneInstance.fVulkanDrawUniqueIndexBufferRangeAllocator.Release(fVulkanDrawUniqueIndexBufferOffset);
   fSceneInstance.fVulkanMorphTargetVertexBufferRangeAllocator.Release(fVulkanMorphTargetVertexBufferOffset);
   fSceneInstance.fVulkanJointBlockBufferRangeAllocator.Release(fVulkanJointBlockBufferOffset);
   fSceneInstance.fVulkanNodeMatricesBufferRangeAllocator.Release(fVulkanNodeMatricesBufferOffset);
   fSceneInstance.fVulkanMorphTargetVertexWeightsBufferRangeAllocator.Release(fVulkanMorphTargetVertexWeightsBufferOffset);
  finally
   fSceneInstance.fBufferRangeAllocatorLock.Release;
  end;
  fVulkanVertexBufferOffset:=-1;
  fVulkanDrawIndexBufferOffset:=-1;
  fVulkanDrawUniqueIndexBufferOffset:=-1;
  fVulkanMorphTargetVertexBufferOffset:=-1;
  fVulkanJointBlockBufferOffset:=-1;
  fVulkanNodeMatricesBufferOffset:=-1;
  fVulkanMorphTargetVertexWeightsBufferOffset:=-1;
  fVulkanVertexBufferCount:=0;
  fVulkanDrawIndexBufferCount:=0;
  fVulkanDrawUniqueIndexBufferCount:=0;
  fVulkanMorphTargetVertexBufferCount:=0;
  fVulkanJointBlockBufferCount:=0;
  fVulkanNodeMatricesBufferCount:=0;
  fVulkanMorphTargetVertexWeightsBufferCount:=0;
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

 FreeAndNil(fScenes);

 FreeAndNil(fCameras);

 FreeAndNil(fLights);

 fSceneInstance.fCullObjectIDLock.Acquire;
 try
  for Index:=0 to length(fNodes)-1 do begin
   InstanceNode:=@fNodes[Index];
   if InstanceNode^.CullObjectID>0 then begin
    try
     fSceneInstance.fCullObjectIDManager.FreeID(InstanceNode^.CullObjectID);
    finally
     InstanceNode^.CullObjectID:=0;
    end;
   end;
  end;
 finally
  fSceneInstance.fCullObjectIDLock.Release;
 end;

 for Index:=0 to length(fNodes)-1 do begin
  if assigned(fNodes[Index].Light) then begin
   FreeAndNil(fNodes[Index].Light);
  end;
 end;

 for Index:=0 to length(fAnimations)-1 do begin
  FreeAndNil(fAnimations[Index]);
 end;

 FreeAndNil(fMaterials);

 fCacheVerticesNodeDirtyBitmap:=nil;

 if assigned(fDuplicatedMaterials) then begin
  for Index:=0 to fDuplicatedMaterials.Count-1 do begin
   fDuplicatedMaterials[Index].DecRef;
  end;
  FreeAndNil(fDuplicatedMaterials);
 end;

 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fPerInFlightFrameRenderInstances[Index].Finalize;
 end;

 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fCullVisibleBitmapLocks[Index]:=0;
  fCullVisibleBitmaps[Index]:=nil;
 end;

 FreeAndNil(fAABBTree);

 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
  fAABBTreeStates[Index].Root:=-1;
  fAABBTreeStates[Index].Generation:=High(TpvUInt64);
  fAABBTreeSkipLists[Index].Items:=nil;
  fAABBTreeSkipLists[Index].Count:=0;
 end;

 fNodes:=nil;

 fSkins:=nil;

 fMaterialMap:=nil;

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
 if not fAdded then begin
  try
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
   begin
    fSceneInstance.fNewInstanceListLock.Acquire;
    try
     fSceneInstance.fNewInstances.Add(self);
    finally
     fSceneInstance.fNewInstanceListLock.Release;
    end;
    TPasMPInterlocked.Write(fIsNewInstance,TPasMPBool32(true));
   end;
  finally
   fAdded:=true;
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.BeforeDestruction;
begin
 Remove;
 inherited BeforeDestruction;
end;

procedure TpvScene3D.TGroup.TInstance.Remove;
begin
 if fAdded then begin
  try
   UpdateInvisible;
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
    if TPasMPInterlocked.CompareExchange(fIsNewInstance,TPasMPBool32(false),TPasMPBool32(true)) then begin
     fSceneInstance.fNewInstanceListLock.Acquire;
     try
      fSceneInstance.fNewInstances.Remove(self);
     finally
      fSceneInstance.fNewInstanceListLock.Release;
     end;
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
  finally
   fAdded:=false;
  end;
 end;
end;

function TpvScene3D.TGroup.TInstance.GetAutomation(const aIndex:TPasGLTFSizeInt):TpvScene3D.TGroup.TInstance.TAnimation;
begin
 result:=fAnimations[aIndex+1];
end;

procedure TpvScene3D.TGroup.TInstance.SetScene(const aScene:TpvSizeInt);
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

procedure TpvScene3D.TGroup.TInstance.LoadData;
begin
end;

procedure TpvScene3D.TGroup.TInstance.Upload;
var Index:TpvSizeInt;
    DescriptorSet:TpvVulkanDescriptorSet;
    UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
    DynamicVertices:TGPUDynamicVertices;
    StaticVertices:TGPUStaticVertices;
    SrcVertex:PVertex;
    DstDynamicVertex:PGPUDynamicVertex;
    DstStaticVertex:PGPUStaticVertex;
    Generation:TpvUInt32;
begin

 if not fInUpload then begin

  fInUpload:=true;
  try

   inherited Upload;

   if not fUploaded then begin

    fGroup.Upload;

    fLock.Acquire;
    try

     if not fUploaded then begin

      try

       if assigned(fSceneInstance.fVulkanDevice) and not fHeadless then begin

       end;

      finally
       fUploaded:=true;
      end;

     end;

    finally
     fLock.Release;
    end;

   end;

  finally
   fInUpload:=false;
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
     if not fHeadless then begin
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

procedure TpvScene3D.TGroup.TInstance.UpdateInvisible;
var Index:TPasGLTFSizeInt;
begin
 for Index:=0 to length(fNodes)-1 do begin
  if assigned(fNodes[Index].Light) then begin
   FreeAndNil(fNodes[Index].Light);
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.Check(const aInFlightFrameIndex:TpvSizeInt);
begin
 if aInFlightFrameIndex>=0 then begin
  Upload;
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
   if assigned(InstanceLight) then begin
    InstanceLight.fCountOverwrites:=0;
   end;
  end;
 end;
 procedure ResetCameras;
 var Index:TPasGLTFSizeInt;
     InstanceCamera:TpvScene3D.TGroup.TInstance.TCamera;
 begin
  for Index:=0 to fCameras.Count-1 do begin
   InstanceCamera:=fCameras[Index];
   if assigned(InstanceCamera) then begin
    InstanceCamera.fCountOverwrites:=0;
   end;
  end;
 end;
 procedure ResetMaterials;
 var Index:TPasGLTFSizeInt;
     InstanceMaterial:TpvScene3D.TGroup.TInstance.TMaterial;
 begin
  for Index:=0 to fMaterials.Count-1 do begin
   InstanceMaterial:=fMaterials[Index];
   if assigned(InstanceMaterial) then begin
    InstanceMaterial.fCountOverwrites:=0;
   end;
  end;
 end;
 procedure ResetNodes;
 var Index:TPasGLTFSizeInt;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
 begin
  for Index:=0 to length(fNodes)-1 do begin
   InstanceNode:=@fNodes[Index];
   InstanceNode^.Processed:=false;
   InstanceNode^.CountOverwrites:=0;
  end;
 end;
 procedure ProcessBaseOverwrite(const aFactor:TPasGLTFFloat);
 var Index:TPasGLTFSizeInt;
     InstanceLight:TpvScene3D.TGroup.TInstance.TLight;
     InstanceCamera:TpvScene3D.TGroup.TInstance.TCamera;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     LightOverwrite:TpvScene3D.TGroup.TInstance.TLight.PLightOverwrite;
     CameraOverwrite:TpvScene3D.TGroup.TInstance.TCamera.PCameraOverwrite;
     NodeOverwrite:TpvScene3D.TGroup.TInstance.TNode.PNodeOverwrite;
 begin
  if aFactor>=-0.5 then begin
   for Index:=0 to fLights.Count-1 do begin
    InstanceLight:=fLights[Index];
    if InstanceLight.fCountOverwrites<length(InstanceLight.fOverwrites) then begin
     LightOverwrite:=@InstanceLight.fOverwrites[InstanceLight.fCountOverwrites];
     LightOverwrite^.Flags:=[TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Defaults];
     LightOverwrite^.Factor:=Max(aFactor,0.0);
     inc(InstanceLight.fCountOverwrites);
    end;
   end;
   for Index:=0 to fCameras.Count-1 do begin
    InstanceCamera:=fCameras[Index];
    if InstanceCamera.fCountOverwrites<length(InstanceCamera.fOverwrites) then begin
     CameraOverwrite:=@InstanceCamera.fOverwrites[InstanceCamera.fCountOverwrites];
     CameraOverwrite^.Flags:=[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.Defaults];
     CameraOverwrite^.Factor:=Max(aFactor,0.0);
     inc(InstanceCamera.fCountOverwrites);
    end;
   end;
   for Index:=0 to fGroup.fNodes.Count-1 do begin
    InstanceNode:=@fNodes[Index];
    if InstanceNode^.CountOverwrites<length(InstanceNode^.Overwrites) then begin
     NodeOverwrite:=@InstanceNode^.Overwrites[InstanceNode^.CountOverwrites];
     NodeOverwrite^.Flags:=[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Defaults];
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
     if (aFactor>=1.0) or SameValue(aFactor,1.0) then begin
      aScalar:=aAnimationChannel^.OutputScalarArray[aTimeIndex1];
     end else begin
      aScalar:=aAnimationChannel^.OutputScalarArray[aTimeIndex0];
     end;
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
     if (aFactor>=1.0) or SameValue(aFactor,1.0) then begin
      aVector2:=aAnimationChannel^.OutputVector2Array[aTimeIndex1];
     end else begin
      aVector2:=aAnimationChannel^.OutputVector2Array[aTimeIndex0];
     end;
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
     if (aFactor>=1.0) or SameValue(aFactor,1.0) then begin
      aVector3:=aAnimationChannel^.OutputVector3Array[aTimeIndex1];
     end else begin
      aVector3:=aAnimationChannel^.OutputVector3Array[aTimeIndex0];
     end;
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
     if (aFactor>=1.0) or SameValue(aFactor,1.0) then begin
      aVector4:=aAnimationChannel^.OutputVector4Array[aTimeIndex1];
     end else begin
      aVector4:=aAnimationChannel^.OutputVector4Array[aTimeIndex0];
     end;
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.CubicSpline:begin
     SqrFactor:=sqr(aFactor);
     CubeFactor:=SqrFactor*aFactor;
     aVector4:=(((aAnimationChannel^.OutputVector4Array[(aTimeIndex0*3)+1]*(((2.0*CubeFactor)-(3.0*SqrFactor))+1.0))+
                (aAnimationChannel^.OutputVector4Array[(aTimeIndex1*3)+0]*(aKeyDelta*((CubeFactor-(2.0*SqrFactor))+aFactor))))+
                 (aAnimationChannel^.OutputVector4Array[(aTimeIndex1*3)+1]*((3.0*SqrFactor)-(2.0*CubeFactor))))+
                  (aAnimationChannel^.OutputVector4Array[(aTimeIndex1*3)+0]*(aKeyDelta*(CubeFactor-SqrFactor)));
     aVector4:=TpvQuaternion.Create(aVector4).Normalize.Vector;
    end;
    else begin
     Assert(false);
    end;
   end;
  end;
  procedure ProcessWeights(const aNode:TpvScene3D.TGroup.TInstance.PNode;
                           const aNodeOverwrite:TpvScene3D.TGroup.TInstance.TNode.PNodeOverwrite;
                           const aAnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
                           const aTimeIndex0:TpvSizeInt;
                           const aTimeIndex1:TpvSizeInt;
                           const aKeyDelta:TpvDouble;
                           const aFactor:TpvDouble);
  var CountWeights,WeightIndex:TpvSizeInt;
      InvFactor,SqrFactor,CubeFactor:TpvDouble;
  begin
   CountWeights:=length(aNode^.WorkWeights);
   Include(aNodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Weights);
   case aAnimationChannel^.Interpolation of
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Linear:begin
     InvFactor:=1.0-aFactor;
     for WeightIndex:=0 to CountWeights-1 do begin
      aNodeOverwrite^.Weights[WeightIndex]:=(aAnimationChannel^.OutputScalarArray[(aTimeIndex0*CountWeights)+WeightIndex]*InvFactor)+
                                            (aAnimationChannel^.OutputScalarArray[(aTimeIndex1*CountWeights)+WeightIndex]*aFactor);
     end;
    end;
    TpvScene3D.TGroup.TAnimation.TChannel.TInterpolation.Step:begin
     if (aFactor>=1.0) or SameValue(aFactor,1.0) then begin
      for WeightIndex:=0 to CountWeights-1 do begin
       aNodeOverwrite^.Weights[WeightIndex]:=aAnimationChannel^.OutputScalarArray[(aTimeIndex1*CountWeights)+WeightIndex];
      end;
     end else begin
      for WeightIndex:=0 to CountWeights-1 do begin
       aNodeOverwrite^.Weights[WeightIndex]:=aAnimationChannel^.OutputScalarArray[(aTimeIndex0*CountWeights)+WeightIndex];
      end;
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
     CountTimeIndices,
     LastIndex,
     LowIndex,
     HighIndex,
     MidIndex,
     TargetSubIndex:TpvSizeInt;
     Animation:TpvScene3D.TGroup.TAnimation;
     AnimationChannel:TpvScene3D.TGroup.TAnimation.PChannel;
     AnimationDefaultChannel:TpvScene3D.TGroup.TAnimation.PDefaultChannel;
     InstanceAnimation:TpvScene3D.TGroup.TInstance.TAnimation;
     InstanceAnimationChannelOverwrite:TpvScene3D.TGroup.TInstance.TAnimation.PChannelOverwrite;
     //Node:TpvScene3D.TGroup.TNode;
     Node:TpvScene3D.TGroup.TInstance.PNode;
     Time,Factor,Value,KeyDelta,v0,v1,a,b:TpvDouble;
     Scalar:TpvFloat;
     Vector2:TpvVector2;
     Vector3:TpvVector3;
     Vector4:TpvVector4;
     TimeIndices:array[0..1] of TpvSizeInt;
     NodeOverwrite:TpvScene3D.TGroup.TInstance.TNode.PNodeOverwrite;
     Mesh:TpvScene3D.TGroup.TMesh;
     Light:TpvScene3D.TGroup.TInstance.TLight;
     LightOverwrite:TpvScene3D.TGroup.TInstance.TLight.PLightOverwrite;
     Camera:TpvScene3D.TGroup.TInstance.TCamera;
     CameraOverwrite:TpvScene3D.TGroup.TInstance.TCamera.PCameraOverwrite;
     Material:TpvScene3D.TGroup.TInstance.TMaterial;
     MaterialOverwrite:TpvScene3D.TGroup.TInstance.TMaterial.PMaterialOverwrite;
 begin

  Animation:=fGroup.fAnimations[aAnimationIndex];

  InstanceAnimation:=fAnimations[aAnimationIndex+1];

  if InstanceAnimation.Complete then begin
   CountInstanceChannels:=length(InstanceAnimation.fChannelOverwrites);
  end else begin
   CountInstanceChannels:=length(Animation.fChannels);
  end;
  if length(InstanceAnimation.fChannelOverwrites)>0 then begin
   FillChar(InstanceAnimation.fChannelOverwrites[0],length(InstanceAnimation.fChannelOverwrites)*SizeOf(TpvScene3D.TGroup.TInstance.TAnimation.TChannelOverwrite),$ff);
  end;

  CountInstanceChannels:=0;

  for ChannelIndex:=0 to length(Animation.fChannels)-1 do begin

   AnimationChannel:=@Animation.fChannels[ChannelIndex];

   if (AnimationChannel^.TargetInstanceIndex>=0) and
      (AnimationChannel^.TargetIndex>=0) and
      (length(AnimationChannel^.InputTimeArray)>0) then begin

    LastIndex:=InstanceAnimation.fLastIndices[ChannelIndex];

    CountTimeIndices:=length(AnimationChannel^.InputTimeArray);

    Time:=Min(Max(aAnimationTime,AnimationChannel^.InputTimeArray[0]),AnimationChannel^.InputTimeArray[CountTimeIndices-1]);

    TimeIndices[1]:=-1;

    if (LastIndex>0) and
       (LastIndex<CountTimeIndices) and
       (AnimationChannel^.InputTimeArray[LastIndex-1]<=Time) then begin
     for InputTimeArrayIndex:=LastIndex to Min(LastIndex+3,CountTimeIndices-1) do begin
      if (AnimationChannel^.InputTimeArray[InputTimeArrayIndex-1]<=Time) and
         (Time<AnimationChannel^.InputTimeArray[InputTimeArrayIndex]) then begin
       TimeIndices[1]:=InputTimeArrayIndex;
       break;
      end;
     end;
    end;

    if TimeIndices[1]<0 then begin
     if (CountTimeIndices>=2) and (Time<AnimationChannel^.InputTimeArray[1]) then begin
      TimeIndices[1]:=1;
     end else if Time<AnimationChannel^.InputTimeArray[0] then begin
      TimeIndices[1]:=0;
     end else if Time>=AnimationChannel^.InputTimeArray[CountTimeIndices-1] then begin
      TimeIndices[1]:=CountTimeIndices-1;
     end else begin
      TimeIndices[1]:=-1;
      LowIndex:=0;
      HighIndex:=CountTimeIndices-1;
      while LowIndex<=HighIndex do begin
       MidIndex:=LowIndex+((HighIndex-LowIndex) shr 1);
       case Sign(AnimationChannel^.InputTimeArray[MidIndex]-Time) of
        -1:begin
         LowIndex:=MidIndex+1;
        end;
        1:begin
         HighIndex:=MidIndex-1;
        end;
        else begin
         TimeIndices[1]:=MidIndex+1;
         break;
        end;
       end;
      end;
      if TimeIndices[1]<0 then begin
       if HighIndex<0 then begin
        TimeIndices[1]:=1;
       end else if LowIndex>=CountTimeIndices then begin
        TimeIndices[1]:=CountTimeIndices-1;
       end else begin
        if LowIndex<HighIndex then begin
         TimeIndices[1]:=LowIndex+1;
        end else begin
         TimeIndices[1]:=HighIndex+1;
        end;
        if TimeIndices[1]<1 then begin
         TimeIndices[1]:=1;
        end;
       end;
      end;
      if TimeIndices[1]>=CountTimeIndices then begin
       TimeIndices[1]:=CountTimeIndices-1;
      end;
     end;
    end;

    if (
        (TimeIndices[1]<1) or
        (TimeIndices[1]>=CountTimeIndices)
       ) or
       (
        (AnimationChannel^.InputTimeArray[TimeIndices[1]-1]>Time) or
        (
         (
          ((TimeIndices[1]+1)<CountTimeIndices) and
          (Time>=AnimationChannel^.InputTimeArray[TimeIndices[1]])
         ) or
         (
          ((TimeIndices[1]+1)=CountTimeIndices) and
          (Time>AnimationChannel^.InputTimeArray[TimeIndices[1]])
         )
        )
       ) then begin
     if TimeIndices[1]<1 then begin
      TimeIndices[1]:=1;
     end;
     if CountTimeIndices<2 then begin
      TimeIndices[1]:=1;
     end else begin
      if TimeIndices[1]>=CountTimeIndices then begin
       TimeIndices[1]:=CountTimeIndices-1;
      end;
      if (AnimationChannel^.InputTimeArray[TimeIndices[1]-1]>Time) or
         (Time>=AnimationChannel^.InputTimeArray[TimeIndices[1]]) then begin
       while (TimeIndices[1]>1) and
             (AnimationChannel^.InputTimeArray[TimeIndices[1]-1]>Time) do begin
        dec(TimeIndices[1]);
       end;
       while ((TimeIndices[1]+1)<CountTimeIndices) and
             (Time>=AnimationChannel^.InputTimeArray[TimeIndices[1]]) do begin
        inc(TimeIndices[1]);
       end;
       if TimeIndices[1]<1 then begin
        TimeIndices[1]:=1;
       end else if TimeIndices[1]>=CountTimeIndices then begin
        TimeIndices[1]:=CountTimeIndices-1;
       end;
      end;
     end;
    end;

    if TimeIndices[1]<0 then begin
     TimeIndices[1]:=0;
     for InputTimeArrayIndex:=1 to CountTimeIndices-1 do begin
      if (AnimationChannel^.InputTimeArray[InputTimeArrayIndex-1]<=Time) and
         (AnimationChannel^.InputTimeArray[InputTimeArrayIndex]>=Time) then begin
       TimeIndices[1]:=InputTimeArrayIndex;
       break;
      end;
     end;
    end;

    InstanceAnimation.fLastIndices[ChannelIndex]:=TimeIndices[1];

    if TimeIndices[1]>=0 then begin

     if TimeIndices[1]>=CountTimeIndices then begin
      TimeIndices[1]:=CountTimeIndices-1;
     end;

     TimeIndices[0]:=TimeIndices[1]-1;
     if TimeIndices[0]<0 then begin
      TimeIndices[0]:=0;
     end;

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

       if (AnimationChannel^.TargetIndex>=0) and (AnimationChannel^.TargetIndex<length(fNodes)) then begin

        Node:=@fNodes[AnimationChannel^.TargetIndex];

        NodeOverwrite:=nil;

        if aFactor>=-0.5 then begin

         InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Node.CountOverwrites<length(Node.Overwrites) then begin
            InstanceAnimationChannelOverwrite^:=Node.CountOverwrites;
            inc(Node.CountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Node.Overwrites)) then begin
             NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
             NodeOverwrite^.Flags:=[];
             NodeOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             NodeOverwrite:=nil;
            end;
           end else begin
            NodeOverwrite:=nil;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Node.Overwrites)) then begin
            NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            NodeOverwrite:=nil;
           end;
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
               Include(NodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Translation);
               NodeOverwrite^.Translation:=Vector3;
              end;
              TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
              TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale:begin
               Include(NodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Scale);
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
               Include(NodeOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Rotation);
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

           InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
           if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

            InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
            if InstanceAnimationChannelOverwrite^<0 then begin
             if Node.CountOverwrites<length(Node.Overwrites) then begin
              InstanceAnimationChannelOverwrite^:=Node.CountOverwrites;
              inc(Node.CountOverwrites);
              if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Node.Overwrites)) then begin
               NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
               NodeOverwrite^.Flags:=[];
               NodeOverwrite^.Factor:=Max(aFactor,0.0);
              end else begin
               NodeOverwrite:=nil;
              end;
             end else begin
              NodeOverwrite:=nil;
             end;
            end else begin
             if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Node.Overwrites)) then begin
              NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
             end else begin
              NodeOverwrite:=nil;
             end;
            end;

            if assigned(NodeOverwrite) then begin
             ProcessWeights(Node,NodeOverwrite,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
            end;

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

       if (AnimationChannel^.TargetIndex>=0) and (AnimationChannel^.TargetIndex<fLights.Count) then begin

        Light:=fLights[AnimationChannel^.TargetIndex];

        LightOverwrite:=nil;

        if aFactor>=-0.5 then begin

         InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Light.fCountOverwrites<length(Light.fOverwrites) then begin
            InstanceAnimationChannelOverwrite^:=Light.fCountOverwrites;
            inc(Light.fCountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Light.fOverwrites)) then begin
             LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
             LightOverwrite^.Flags:=[];
             LightOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             LightOverwrite:=nil;
            end;
           end else begin
            LightOverwrite:=nil;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Light.fOverwrites)) then begin
            LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            LightOverwrite:=nil;
           end;
          end;

          if assigned(LightOverwrite) then begin

           case AnimationChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor:begin
             ProcessVector3(Vector3,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Color);
             LightOverwrite^.Color:=Vector3;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Intensity);
             LightOverwrite^.Intensity:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Range);
             LightOverwrite^.Range:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.SpotInnerConeAngle);
             LightOverwrite^.SpotInnerConeAngle:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(LightOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.SpotOuterConeAngle);
             LightOverwrite^.SpotOuterConeAngle:=Scalar;
            end;
            else begin
            end;
           end;
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

       if (AnimationChannel^.TargetIndex>=0) and (AnimationChannel^.TargetIndex<fCameras.Count) then begin

        Camera:=fCameras[AnimationChannel^.TargetIndex];

        CameraOverwrite:=nil;

        if aFactor>=-0.5 then begin

         InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Camera.fCountOverwrites<length(Camera.fOverwrites) then begin
            InstanceAnimationChannelOverwrite^:=Camera.fCountOverwrites;
            inc(Camera.fCountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Camera.fOverwrites)) then begin
             CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
             CameraOverwrite^.Flags:=[];
             CameraOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             CameraOverwrite:=nil;
            end;
           end else begin
            CameraOverwrite:=nil;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Camera.fOverwrites)) then begin
            CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            CameraOverwrite:=nil;
           end;
          end;

          if assigned(CameraOverwrite) then begin

           case AnimationChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicXMag);
             CameraOverwrite^.OrthographicXMag:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicYMag);
             CameraOverwrite^.OrthographicYMag:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicZFar);
             CameraOverwrite^.OrthographicZFar:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicZNear);
             CameraOverwrite^.OrthographicZNear:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveAspectRatio);
             CameraOverwrite^.PerspectiveAspectRatio:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveYFov);
             CameraOverwrite^.PerspectiveYFov:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveZFar);
             CameraOverwrite^.PerspectiveZFar:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(CameraOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveZNear);
             CameraOverwrite^.PerspectiveZNear:=Scalar;
            end;
            else begin
            end;
           end;
          end;

         end;

        end;

       end;

      end;

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatRoughnessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialIOR,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin

       if (AnimationChannel^.TargetIndex>=0) and (AnimationChannel^.TargetIndex<fMaterials.Count) then begin

        Material:=fMaterials[AnimationChannel^.TargetIndex];

 {      if not assigned(Material) then begin
         Material:=fMaterials[AnimationChannel^.TargetIndex];
        end;}

        TargetSubIndex:=AnimationChannel^.TargetSubIndex;

        MaterialOverwrite:=nil;

        if (aFactor>=-0.5) and assigned(Material) then begin

         InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Material.fCountOverwrites<length(Material.fOverwrites) then begin
            InstanceAnimationChannelOverwrite^:=Material.fCountOverwrites;
            inc(Material.fCountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Material.fOverwrites)) then begin
             MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
             MaterialOverwrite^.Flags:=[];
             MaterialOverwrite^.SubIndex:=TargetSubIndex;
             MaterialOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             MaterialOverwrite:=nil;
            end;
           end else begin
            MaterialOverwrite:=nil;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Material.fOverwrites)) then begin
            MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            MaterialOverwrite:=nil;
           end;
          end;

          if assigned(MaterialOverwrite) then begin

           case AnimationChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor:begin
             ProcessVector4(Vector4,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor,false);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessBaseColorFactor);
             MaterialOverwrite^.MaterialPBRMetallicRoughnessBaseColorFactor:=Vector4;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessMetallicFactor);
             MaterialOverwrite^.MaterialPBRMetallicRoughnessMetallicFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessRoughnessFactor);
             MaterialOverwrite^.MaterialPBRMetallicRoughnessRoughnessFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialAlphaCutOff);
             MaterialOverwrite^.MaterialAlphaCutOff:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor:begin
             ProcessVector3(Vector3,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialEmissiveFactor);
             MaterialOverwrite^.MaterialEmissiveFactor:=Vector3;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialNormalTextureScale);
             MaterialOverwrite^.MaterialNormalTextureScale:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialOcclusionTextureStrength);
             MaterialOverwrite^.MaterialOcclusionTextureStrength:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRClearCoatFactor);
             MaterialOverwrite^.MaterialPBRClearCoatFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatRoughnessFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRClearCoatRoughnessFactor);
             MaterialOverwrite^.MaterialPBRClearCoatRoughnessFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialEmissiveStrength);
             MaterialOverwrite^.MaterialEmissiveStrength:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialIOR:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialIOR);
             MaterialOverwrite^.MaterialIOR:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceFactor);
             MaterialOverwrite^.MaterialPBRIridescenceFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceIor);
             MaterialOverwrite^.MaterialPBRIridescenceIor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceMinimum);
             MaterialOverwrite^.MaterialPBRIridescenceMinimum:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceMaximum);
             MaterialOverwrite^.MaterialPBRIridescenceMaximum:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor:begin
             ProcessVector3(Vector3,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSheenColorFactor);
             MaterialOverwrite^.MaterialPBRSheenColorFactor:=Vector3;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSheenRoughnessFactor);
             MaterialOverwrite^.MaterialPBRSheenRoughnessFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSpecularFactor);
             MaterialOverwrite^.MaterialPBRSpecularFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor:begin
             ProcessVector3(Vector3,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSpecularColorFactor);
             MaterialOverwrite^.MaterialPBRSpecularColorFactor:=Vector3;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRTransmissionFactor);
             MaterialOverwrite^.MaterialPBRTransmissionFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeThicknessFactor);
             MaterialOverwrite^.MaterialPBRVolumeThicknessFactor:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeAttenuationDistance);
             MaterialOverwrite^.MaterialPBRVolumeAttenuationDistance:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor:begin
             ProcessVector3(Vector3,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeAttenuationColor);
             MaterialOverwrite^.MaterialPBRVolumeAttenuationColor:=Vector3;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureRotation);
             MaterialOverwrite^.MaterialPBRAnisotropyStrength:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureRotation);
             MaterialOverwrite^.MaterialPBRAnisotropyRotation:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset:begin
             ProcessVector2(Vector2,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureOffset);
             MaterialOverwrite^.TextureOffset:=Vector2;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation:begin
             ProcessScalar(Scalar,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureRotation);
             MaterialOverwrite^.TextureRotation:=Scalar;
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin
             ProcessVector2(Vector2,AnimationChannel,TimeIndices[0],TimeIndices[1],KeyDelta,Factor);
             Include(MaterialOverwrite^.Flags,TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureScale);
             MaterialOverwrite^.TextureScale:=Vector2;
            end;
            else begin
            end;
           end;
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

    if AnimationDefaultChannel^.TargetInstanceIndex>=0 then begin

     case AnimationDefaultChannel^.Target of

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin
       if (AnimationDefaultChannel^.TargetIndex>=0) and (AnimationDefaultChannel^.TargetIndex<length(fNodes)) then begin
        Node:=@fNodes[AnimationDefaultChannel^.TargetIndex];
        NodeOverwrite:=nil;
        if aFactor>=-0.5 then begin
         InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Node.CountOverwrites<length(Node.Overwrites) then begin
            InstanceAnimationChannelOverwrite^:=Node.CountOverwrites;
            inc(Node.CountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Node.Overwrites)) then begin
             NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
             NodeOverwrite^.Flags:=[];
             NodeOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             NodeOverwrite:=nil;
            end;
           end else begin
            NodeOverwrite:=nil;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Node.Overwrites)) then begin
            NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            NodeOverwrite:=nil;
           end;
          end;
          if assigned(NodeOverwrite) then begin
           case AnimationDefaultChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation:begin
             NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultTranslation,
                                                         TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Translation];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale:begin
             NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultScale,
                                                         TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Scale];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation:begin
             NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultRotation,
                                                         TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Rotation];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin
             NodeOverwrite^.Flags:=NodeOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultWeights,
                                                         TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Weights];
            end;
            else begin
            end;
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
       if (AnimationDefaultChannel^.TargetIndex>=0) and (AnimationDefaultChannel^.TargetIndex<fLights.Count) then begin
        Light:=fLights[AnimationDefaultChannel^.TargetIndex];
        LightOverwrite:=nil;
        if aFactor>=-0.5 then begin
         InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Light.fCountOverwrites<length(Light.fOverwrites) then begin
            InstanceAnimationChannelOverwrite^:=Light.fCountOverwrites;
            inc(Light.fCountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Light.fOverwrites)) then begin
             LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
             LightOverwrite^.Flags:=[];
             LightOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             LightOverwrite:=nil;
            end;
           end else begin
            LightOverwrite:=nil;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Light.fOverwrites)) then begin
            LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            LightOverwrite:=nil;
           end;
          end;
          if assigned(LightOverwrite) then begin
           case AnimationDefaultChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor:begin
             LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultColor,
                                                           TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Color];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity:begin
             LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultIntensity,
                                                           TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Intensity];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange:begin
             LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultRange,
                                                           TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.Range];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle:begin
             LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultSpotInnerConeAngle,
                                                           TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.SpotInnerConeAngle];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
             LightOverwrite^.Flags:=LightOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.DefaultSpotOuterConeAngle,
                                                           TpvScene3D.TGroup.TInstance.TLight.TLightOverwriteFlag.SpotOuterConeAngle];
            end;
            else begin
            end;
           end;
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
       if (AnimationDefaultChannel^.TargetIndex>=0) and (AnimationDefaultChannel^.TargetIndex<fCameras.Count) then begin
        Camera:=fCameras[AnimationDefaultChannel^.TargetIndex];
        CameraOverwrite:=nil;
        if aFactor>=-0.5 then begin
         InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Camera.fCountOverwrites<length(Camera.fOverwrites) then begin
            InstanceAnimationChannelOverwrite^:=Camera.fCountOverwrites;
            inc(Camera.fCountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Camera.fOverwrites)) then begin
             CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
             CameraOverwrite^.Flags:=[];
             CameraOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             CameraOverwrite:=nil;
            end;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Camera.fOverwrites)) then begin
            CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            CameraOverwrite:=nil;
           end;
          end;
          if assigned(CameraOverwrite) then begin
           case AnimationDefaultChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicXMag,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicXMag];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicYMag,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicYMag];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicZFar,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicZFar];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultOrthographicZNear,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.OrthographicZNear];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveAspectRatio,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveAspectRatio];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveYFov,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveYFov];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveZFar,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveZFar];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear:begin
             CameraOverwrite^.Flags:=CameraOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.DefaultPerspectiveZNear,
                                                             TpvScene3D.TGroup.TInstance.TCamera.TCameraOverwriteFlag.PerspectiveZNear];
            end;
            else begin
            end;
           end;
          end;
         end;
        end;
       end;
      end;

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatRoughnessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialIOR,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin
       if (AnimationDefaultChannel^.TargetIndex>=0) and (AnimationDefaultChannel^.TargetIndex<fMaterials.Count) then begin
        Material:=fMaterials[AnimationDefaultChannel^.TargetIndex];
        TargetSubIndex:=AnimationDefaultChannel^.TargetSubIndex;
        MaterialOverwrite:=nil;
        if (aFactor>=-0.5) and assigned(Material) then begin
         InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
         if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
          InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
          if InstanceAnimationChannelOverwrite^<0 then begin
           if Material.fCountOverwrites<length(Material.fOverwrites) then begin
            InstanceAnimationChannelOverwrite^:=Material.fCountOverwrites;
            inc(Material.fCountOverwrites);
            if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Material.fOverwrites)) then begin
             MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
             MaterialOverwrite^.Flags:=[];
             MaterialOverwrite^.SubIndex:=TargetSubIndex;
             MaterialOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             MaterialOverwrite:=nil;
            end;
           end else begin
            MaterialOverwrite:=nil;
           end;
          end else begin
           if (InstanceAnimationChannelOverwrite^>=0) and (InstanceAnimationChannelOverwrite^<length(Material.fOverwrites)) then begin
            MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
           end else begin
            MaterialOverwrite:=nil;
           end;
          end;
          if assigned(MaterialOverwrite) then begin
           case AnimationDefaultChannel^.Target of
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRMetallicRoughnessBaseColorFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessBaseColorFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRMetallicRoughnessMetallicFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessMetallicFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRMetallicRoughnessRoughnessFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRMetallicRoughnessRoughnessFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialAlphaCutOff,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialAlphaCutOff];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialEmissiveFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialEmissiveFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialNormalTextureScale,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialNormalTextureScale];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialOcclusionTextureStrength,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialOcclusionTextureStrength];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRClearCoatFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRClearCoatFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatRoughnessFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRClearCoatRoughnessFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRClearCoatRoughnessFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialEmissiveStrength,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialEmissiveStrength];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialIOR:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialIOR,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialIOR];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceIor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceIor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceMinimum,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceMinimum];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRIridescenceMaximum,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRIridescenceMaximum];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSheenColorFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSheenColorFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSheenRoughnessFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSheenRoughnessFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSpecularFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSpecularFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRSpecularColorFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRSpecularColorFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRTransmissionFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRTransmissionFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRVolumeThicknessFactor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeThicknessFactor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRVolumeAttenuationDistance,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeAttenuationDistance];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRVolumeAttenuationColor,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRVolumeAttenuationColor];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRAnisotropyStrength,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRAnisotropyStrength];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultMaterialPBRAnisotropyRotation,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.MaterialPBRAnisotropyRotation];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultTextureOffset,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureOffset];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultTextureRotation,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureRotation];
            end;
            TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin
             MaterialOverwrite^.Flags:=MaterialOverwrite^.Flags+[TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.DefaultTextureScale,
                                                                 TpvScene3D.TGroup.TInstance.TMaterial.TMaterialOverwriteFlag.TextureScale];
            end;
            else begin
            end;
           end;
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
     Overwrite:TpvScene3D.TGroup.TInstance.TNode.PNodeOverwrite;
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
  if (InstanceNode^.CountOverwrites>0) and (Node.Flags<>[]) then begin
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
     if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Defaults in Overwrite^.Flags then begin
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
      if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Translation in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultTranslation in Overwrite^.Flags then begin
        TranslationSum.Add(Node.fTranslation,Factor);
       end else begin
        TranslationSum.Add(Overwrite^.Translation,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Scale in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultScale in Overwrite^.Flags then begin
        ScaleSum.Add(Node.fScale,Factor);
       end else begin
        ScaleSum.Add(Overwrite^.Scale,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Rotation in Overwrite^.Flags then begin
       if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultRotation in Overwrite^.Flags then begin
        AddRotation(Node.fRotation,Factor);
       end else begin
        AddRotation(Overwrite^.Rotation,Factor);
       end;
      end;
      if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Weights in Overwrite^.Flags then begin
       if FirstWeights then begin
        FirstWeights:=false;
        for OtherIndex:=0 to length(InstanceNode^.OverwriteWeightsSum)-1 do begin
         InstanceNode^.OverwriteWeightsSum[OtherIndex]:=0.0;
        end;
       end;
       if TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.DefaultWeights in Overwrite^.Flags then begin
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
   if fOnNodeMatrixPre(self,Node,InstanceNode,Matrix) then begin
    Dirty:=true;
   end;
  end;
  Matrix:=Matrix*Node.fMatrix;
  if assigned(fOnNodeMatrixPost) then begin
   if fOnNodeMatrixPost(self,Node,InstanceNode,Matrix) then begin
    Dirty:=true;
   end;
  end;
  Matrix:=Matrix*aMatrix;
  InstanceNode^.WorkMatrix:=Matrix;
  if assigned(Node.fMesh) then begin
   if Matrix.Determinant<0.0 then begin
    Include(InstanceNode^.Flags,TpvScene3D.TGroup.TInstance.TNode.TInstanceNodeFlag.InverseFrontFaces);
   end else begin
    Exclude(InstanceNode^.Flags,TpvScene3D.TGroup.TInstance.TNode.TInstanceNodeFlag.InverseFrontFaces);
   end;
   if {SkinUsed and} assigned(Node.fSkin) then begin
    fSkins[Node.fSkin.Index].Used:=true;
   end;
  end;
  Dirty:=Dirty or (assigned(Node.fSkin) or (length(Node.fWeights)>0));
  if aInFlightFrameIndex>=0 then begin
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
 procedure ProcessBoundingBoxNodeRecursive(const aNodeIndex:TpvSizeInt);
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
   ProcessBoundingBoxNodeRecursive(Node.Children[Index].Index);
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
 procedure ProcessBoundingSceneBoxNodesWithManualStack(const aScene:TpvScene3D.TGroup.TScene);
 type TStackItem=record
       NodeIndex:TpvSizeInt;       
       Pass:TpvSizeInt;
      end;
      PStackItem=^TStackItem;
      TStack=TpvDynamicFastStack<TStackItem>;
 var Stack:TStack;
     StackItem:TStackItem;
     NewStackItem:PStackItem;
     Index:TpvSizeInt;
     Node:TpvScene3D.TGroup.TNode;
     InstanceNode,OtherInstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     AABB:PpvAABB;
     Filled:PBoolean;
 begin
  if aScene.fNodes.Count>0 then begin
   Stack.Initialize;
   try   
    for Index:=aScene.fNodes.Count-1 downto 0 do begin
     NewStackItem:=Stack.PushIndirect;
     NewStackItem^.NodeIndex:=aScene.fNodes[Index].Index;
     NewStackItem^.Pass:=0;
    end;
    while Stack.Pop(StackItem) do begin   
     Node:=fGroup.fNodes[StackItem.NodeIndex];
     case StackItem.Pass of
      0:begin
       if Node.Children.Count>0 then begin 
        NewStackItem:=Stack.PushIndirect;
        NewStackItem^.NodeIndex:=StackItem.NodeIndex;
        NewStackItem^.Pass:=1;
        for Index:=Node.Children.Count-1 downto 0 do begin
         NewStackItem:=Stack.PushIndirect;
         NewStackItem^.NodeIndex:=Node.Children[Index].Index;
         NewStackItem^.Pass:=0;
        end; 
       end; 
      end;  
      1:begin 
       if Node.Children.Count>0 then begin 
        InstanceNode:=@fNodes[StackItem.NodeIndex];
        AABB:=@InstanceNode^.BoundingBoxes[aInFlightFrameIndex];
        Filled:=@InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex];
        for Index:=0 to Node.Children.Count-1 do begin
         OtherInstanceNode:=@fNodes[Node.Children[Index].Index];
         if OtherInstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
          AABB^:=AABB^.Combine(OtherInstanceNode^.BoundingBoxes[aInFlightFrameIndex]);
         end else begin
          Filled^:=true;
          AABB^:=OtherInstanceNode^.BoundingBoxes[aInFlightFrameIndex];
         end;
        end; 
       end;
      end; 
     end;
    end; 
   finally
    Stack.Finalize;
   end;
  end; 
 end;
 procedure ProcessBoundingSceneBoxNodes(const aScene:TpvScene3D.TGroup.TScene);
 var Index:TpvSizeInt;
 begin
  if length(fNodes)<64 then begin
   for Index:=0 to aScene.fNodes.Count-1 do begin
    ProcessBoundingBoxNodeRecursive(aScene.fNodes[Index].Index);
   end;
  end else begin
   ProcessBoundingSceneBoxNodesWithManualStack(aScene);
  end;
 end;
 procedure ProcessMorphSkinNode(const aNode:TpvScene3D.TGroup.TNode;const aInstanceNode:TpvScene3D.TGroup.TInstance.PNode);
 var PrimitiveIndex,VertexIndex,JointBlockIndex,JointIndex:TpvSizeInt;
     MorphTargetVertexIndex:TpvUInt32;
     Mesh:TpvScene3D.TGroup.TMesh;
     Skin:TpvScene3D.TGroup.TSkin;
     InverseMatrix,Matrix,ModelNodeMatrix,ModelNodeMatrixEx:TpvMatrix4x4;
     Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
     Vertex:TpvScene3D.PVertex;
     Position:TpvVector3;
     JointBlock:PJointBlock;
     DynamicBoundingBox:TpvAABB;
     MorphTargetVertex:TpvScene3D.PMorphTargetVertex;
     OK:boolean;
 begin
  Mesh:=aNode.fMesh;
  if assigned(Mesh) then begin
   OK:=false;
   Skin:=aNode.fSkin;
   if assigned(Skin) then begin
    InverseMatrix:=aInstanceNode^.WorkMatrix.Inverse;
   end else begin
    InverseMatrix:=TpvMatrix4x4.Identity;
   end;
   ModelNodeMatrixEx:=aInstanceNode^.WorkMatrix*fModelMatrix;
   DynamicBoundingBox.Min:=TpvVector3.InlineableCreate(Infinity,Infinity,Infinity);
   DynamicBoundingBox.Max:=TpvVector3.InlineableCreate(-Infinity,-Infinity,-Infinity);
   for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin
    Primitive:=@Mesh.fPrimitives[PrimitiveIndex];
    for VertexIndex:=Primitive^.StartBufferVertexOffset to (Primitive^.StartBufferVertexOffset+Primitive^.CountVertices)-1 do begin
     Vertex:=@Group.fVertices.Items[VertexIndex];
     Position:=Vertex^.Position;
     MorphTargetVertexIndex:=Vertex^.MorphTargetVertexBaseIndex;
     while MorphTargetVertexIndex<>TpvUInt32($ffffffff) do begin
      MorphTargetVertex:=@Group.fMorphTargetVertices.Items[MorphTargetVertexIndex];
      Position:=Position+(MorphTargetVertex^.Position.xyz*fMorphTargetVertexWeights[MorphTargetVertex^.Index]);
      MorphTargetVertexIndex:=MorphTargetVertex^.Next;
     end;
     ModelNodeMatrix:=ModelNodeMatrixEx;
     if Vertex^.CountJointBlocks>0 then begin
      Matrix:=TpvMatrix4x4.Identity;
      for JointBlockIndex:=Vertex^.JointBlockBaseIndex to (Vertex^.JointBlockBaseIndex+Vertex^.CountJointBlocks)-1 do begin
       JointBlock:=@fGroup.fJointBlocks.Items[JointBlockIndex];
       for JointIndex:=0 to 3 do begin
        Matrix:=Matrix+((fNodeMatrices[JointBlock^.Joints[JointIndex]]*InverseMatrix)*JointBlock^.Weights[JointIndex]);
       end;
      end;
      ModelNodeMatrix:=Matrix*ModelNodeMatrix;
     end;
     Position:=ModelNodeMatrix.MulHomogen(Position);
     DynamicBoundingBox.Min[0]:=Min(DynamicBoundingBox.Min[0],Position[0]);
     DynamicBoundingBox.Min[1]:=Min(DynamicBoundingBox.Min[1],Position[1]);
     DynamicBoundingBox.Min[2]:=Min(DynamicBoundingBox.Min[2],Position[2]);
     DynamicBoundingBox.Max[0]:=Max(DynamicBoundingBox.Max[0],Position[0]);
     DynamicBoundingBox.Max[1]:=Max(DynamicBoundingBox.Max[1],Position[1]);
     DynamicBoundingBox.Max[2]:=Max(DynamicBoundingBox.Max[2],Position[2]);
     OK:=true;
    end;
   end;
   if OK then begin
    aInstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=DynamicBoundingBox;
   end else begin
    aInstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=Mesh.fBoundingBox.Transform(aInstanceNode^.WorkMatrix*fModelMatrix);
   end;
   aInstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=true;
  end;
 end;
 procedure ProcessSkinNode(const aNode:TpvScene3D.TGroup.TNode;const aInstanceNode:TpvScene3D.TGroup.TInstance.PNode);
 // This procedure calculates a conservative worst-case bounding box for a node with a skinned animated mesh. It 
 // approximates the bounding box for a given animation state using the mesh bounding box (which includes already
 // conservative morph vertices) and the joints used by the node. This method is not completely accurate but offers a 
 // balance between performance and precision, as it avoids recalculating based on current transformed vertex positions.
 var JointIndex:TpvSizeInt;
     Mesh:TpvScene3D.TGroup.TMesh;
     Skin:TpvScene3D.TGroup.TSkin;
     InverseMatrix:TpvMatrix4x4;
     UsedJoint:TpvScene3D.TGroup.TNode.PUsedJoint;
     BoundingBox:TpvAABB;
 begin

  Mesh:=aNode.fMesh;
  if assigned(Mesh) then begin

   Skin:=aNode.fSkin;

   // Obtain the inverse of the node's world matrix, if it is needed.
   if assigned(Skin) then begin
    InverseMatrix:=aInstanceNode^.WorkMatrix.Inverse;
   end else begin
    InverseMatrix:=TpvMatrix4x4.Identity;
   end;

   // Initialize the bounding box with the mesh bounding box, which already includes the worst-case scenario for morph vertices.   
   BoundingBox:=Mesh.fBoundingBox; 

   // Iterate over all joints used by the current node, considering each joint's largest weight which is used by the mesh vertices of the node.
   // This aims to be conservative but not necessarily 100% accurate.
   for JointIndex:=0 to aNode.fUsedJoints.Count-1 do begin

    UsedJoint:=@aNode.fUsedJoints.Items[JointIndex];

    // Update the bounding box by combining it with the transformed by-the-joint-affected-vertices bounding box using the joint matrix.
    BoundingBox:=BoundingBox.Combine(UsedJoint^.AABB.Transform((fNodeMatrices[UsedJoint^.Joint]*InverseMatrix)*UsedJoint^.Weight));

   end;

   // Transform the final bounding box using the node and model matrices and store it in the instance node.
   aInstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=BoundingBox.Transform(aInstanceNode^.WorkMatrix*fModelMatrix);

   // Indicate that the bounding box has been calculated for the instance node.
   aInstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=true;

  end;

 end;
 procedure GenerateAABBTreeSkipList;
 type TStackItem=record
       NodeIndex:TpvSizeInt;
       Pass:TpvSizeInt;
       Level:TpvSizeInt;
       SkipListItemIndex:TpvSizeInt;
      end;
      PStackItem=^TStackItem;
      TStack=TpvDynamicStack<TStackItem>;
 var NodeIndex,SkipListItemCount,SkipListItemIndex:TpvSizeInt;
     Stack:TStack;
     StackItem,NewStackItem:TStackItem;
     Node:TpvBVHDynamicAABBTree.PTreeNode;
     SkipList:TpvScene3D.TGroup.TInstance.PAABBTreeSkipList;
     SkipListItem:TpvScene3D.TGroup.TInstance.PAABBTreeSkipListItem;
 begin

  Stack.Initialize;
  try

   if fAABBTree.Root>=0 then begin
    NewStackItem.NodeIndex:=fAABBTree.Root;
    NewStackItem.Pass:=0;
    NewStackItem.Level:=0;
    NewStackItem.SkipListItemIndex:=0;
    Stack.Push(NewStackItem);
   end;

   SkipListItemCount:=0;

   SkipList:=@fAABBTreeSkipLists[aInFlightFrameIndex];
   SkipList^.Count:=0;
   try

    while Stack.Pop(StackItem) do begin

     case StackItem.Pass of

      0:begin

       Node:=@fAABBTree.Nodes[StackItem.NodeIndex];

       SkipListItemIndex:=SkipListItemCount;
       inc(SkipListItemCount);

       if length(SkipList^.Items)<SkipListItemCount then begin
        SetLength(SkipList^.Items,SkipListItemCount+((SkipListItemCount+1) shr 1));
       end;

       SkipListItem:=@SkipList^.Items[SkipListItemIndex];
       SkipListItem^.AABB:=Node.AABB;
       SkipListItem^.UserData:=Node.UserData;
       SkipListItem^.NodeIndex:=StackItem.NodeIndex;
       SkipListItem^.Level:=StackItem.Level;
       SkipListItem^.SkipCount:=1;

       NewStackItem.NodeIndex:=StackItem.NodeIndex;
       NewStackItem.Pass:=1;
       NewStackItem.Level:=StackItem.Level;
       NewStackItem.SkipListItemIndex:=SkipListItemIndex;
       Stack.Push(NewStackItem);

       if Node^.Children[1]>=0 then begin
        NewStackItem.NodeIndex:=Node^.Children[1];
        NewStackItem.Pass:=0;
        NewStackItem.Level:=StackItem.Level+1;
        NewStackItem.SkipListItemIndex:=0;
        Stack.Push(NewStackItem);
       end;

       if Node^.Children[0]>=0 then begin
        NewStackItem.NodeIndex:=Node^.Children[0];
        NewStackItem.Pass:=0;
        NewStackItem.Level:=StackItem.Level+1;
        NewStackItem.SkipListItemIndex:=0;
        Stack.Push(NewStackItem);
       end;

      end;

      1:begin
       SkipList^.Items[StackItem.SkipListItemIndex].SkipCount:=SkipListItemCount-StackItem.SkipListItemIndex;
      end;

      else begin
       Assert(false);
      end;

     end;

    end;

   finally
    SkipList^.Count:=SkipListItemCount;
   end;

  finally
   Stack.Finalize;
  end;

 end;
var Index,PerInFlightFrameRenderInstanceIndex:TPasGLTFSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    Animation:TpvScene3D.TGroup.TInstance.TAnimation;
    Node:TpvScene3D.TGroup.TNode;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    InstanceMaterial:TpvScene3D.TGroup.TInstance.TMaterial;
    HasMaterialUpdate,Dirty:boolean;
    RenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
    PerInFlightFrameRenderInstance:TpvScene3D.TGroup.TInstance.PPerInFlightFrameRenderInstance;
    AABBTreeState:TpvBVHDynamicAABBTree.PState;
    AABBTreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    AABBTreeNodePotentiallyVisibleSet:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
begin

 if aInFlightFrameIndex>=0 then begin

  fActives[aInFlightFrameIndex]:=fActive;

 end;

 if fActive then begin

  Scene:=GetScene;

  if assigned(Scene) then begin

   if (aInFlightFrameIndex>=0) and (fActiveScenes[aInFlightFrameIndex]<>Scene) then begin
    fActiveScenes[aInFlightFrameIndex]:=Scene;
   end;

   //CurrentSkinShaderStorageBufferObjectHandle:=0;

   for Index:=0 to length(fLightNodes)-1 do begin
    fLightNodes[Index]:=-1;
   end;

   ResetLights;

   ResetCameras;

   ResetMaterials;

   ResetNodes;

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

   if aInFlightFrameIndex>=0 then begin

    for Index:=0 to fLights.Count-1 do begin
     fLights[Index].Update;
    end;

    for Index:=0 to fCameras.Count-1 do begin
     fCameras[Index].Update;
    end;

   end;

   HasMaterialUpdate:=false;
   for Index:=0 to fMaterials.Count-1 do begin
    InstanceMaterial:=fMaterials[Index];
    if assigned(InstanceMaterial) then begin
     InstanceMaterial.Update;
     HasMaterialUpdate:=true;
    end;
   end;
   if HasMaterialUpdate then begin
    SceneInstance.NewMaterialDataGeneration;
   end;

   Dirty:=fDirtyCounter>0;
   if Dirty then begin
    dec(fDirtyCounter);
   end;

   for Index:=0 to Scene.fNodes.Count-1 do begin
    ProcessNode(Scene.fNodes[Index].Index,TpvMatrix4x4.Identity,Dirty);
   end;

   if aInFlightFrameIndex>=0 then begin
    for Index:=0 to length(fNodes)-1 do begin
     InstanceNode:=@fNodes[Index];
     if not InstanceNode^.Processed then begin
      if assigned(InstanceNode^.Light) then begin
       FreeAndNil(InstanceNode^.Light);
      end;
      if assigned(fAABBTree) and (InstanceNode^.AABBTreeProxy>=0) then begin
       try
        fAABBTree.DestroyProxy(InstanceNode^.AABBTreeProxy);
       finally
        InstanceNode^.AABBTreeProxy:=-1;
       end;
      end;
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

   if aInFlightFrameIndex>=0 then begin

    for Index:=0 to fGroup.fNodes.Count-1 do begin
     Node:=fGroup.fNodes[Index];
     InstanceNode:=@fNodes[Index];
     if assigned(Node.fMesh) then begin
     {
      if assigned(Node.fSkin) or (length(Node.fWeights)>0) or (length(Node.fMesh.fWeights)>0) then begin
       ProcessMorphSkinNode(Node,InstanceNode);
      end else//}
      if assigned(Node.fSkin) then begin
       ProcessSkinNode(Node,InstanceNode);
      end else begin
       InstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=Node.fMesh.fBoundingBox.Transform(InstanceNode^.WorkMatrix*fModelMatrix);
       InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=true;
      end;
     end else begin
      InstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=TpvAABB.Create(TpvVector3.Origin,TpvVector3.Origin);
      InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=false;
     end;
    end;

    ProcessBoundingSceneBoxNodes(Scene);

    for Index:=0 to fGroup.fNodes.Count-1 do begin
     Node:=fGroup.fNodes[Index];
     InstanceNode:=@fNodes[Index];
     if InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
      if assigned(fGroup.fSceneInstance.fPotentiallyVisibleSet) and
         ((InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
          ((InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) and not
           fSceneInstance.fPotentiallyVisibleSet.fNodes[InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]].fAABB.Contains(InstanceNode^.BoundingBoxes[aInFlightFrameIndex]))) then begin
       InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=fGroup.fSceneInstance.fPotentiallyVisibleSet.GetNodeIndexByAABB(InstanceNode^.BoundingBoxes[aInFlightFrameIndex]);
      end;
     end else begin
      InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
     end;
    end;

    if assigned(fAABBTree) then begin
     for Index:=0 to Scene.fAllNodes.Count-1 do begin
      Node:=Scene.fAllNodes[Index];
      InstanceNode:=@fNodes[Node.Index];
      if InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] and assigned(Node.Mesh) then begin
       if InstanceNode^.AABBTreeProxy<0 then begin
        InstanceNode^.AABBTreeProxy:=fAABBTree.CreateProxy(InstanceNode^.BoundingBoxes[aInFlightFrameIndex],TpvPtrInt(Node.fIndex)+1);
       end else begin
        fAABBTree.MoveProxy(InstanceNode^.AABBTreeProxy,InstanceNode^.BoundingBoxes[aInFlightFrameIndex],TpvVector3.Origin);
       end;
      end else if InstanceNode^.AABBTreeProxy>=0 then begin
       fAABBTree.DestroyProxy(InstanceNode^.AABBTreeProxy);
       InstanceNode^.AABBTreeProxy:=-1;
      end;
     end;
    end;

   end;

  end;

  fBoundingBox:=fGroup.fBoundingBox.Transform(fModelMatrix);
  if assigned(Scene) and (aInFlightFrameIndex>=0) then begin
   for Index:=0 to Scene.fNodes.Count-1 do begin
    InstanceNode:=@fNodes[Scene.fNodes[Index].fIndex];
    if InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
     fBoundingBox:=fBoundingBox.Combine(InstanceNode^.BoundingBoxes[aInFlightFrameIndex]);
    end;
   end;
  end;
  if aInFlightFrameIndex>=0 then begin
   fBoundingBoxes[aInFlightFrameIndex]:=fBoundingBox;
   fBoundingSpheres[aInFlightFrameIndex]:=TpvSphere.CreateFromAABB(fBoundingBox);
  end;

  if aInFlightFrameIndex>=0 then begin
   fPerInFlightFrameRenderInstances[aInFlightFrameIndex].Count:=0;
   if fUseRenderInstances then begin
    for Index:=0 to fRenderInstances.Count-1 do begin
     RenderInstance:=fRenderInstances[Index];
     if RenderInstance.fActive then begin
      RenderInstance.fBoundingBox:=fBoundingBox.Transform(RenderInstance.fModelMatrix);
      if assigned(fGroup.fSceneInstance.fPotentiallyVisibleSet) and
         ((RenderInstance.fPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
          ((RenderInstance.fPotentiallyVisibleSetNodeIndex<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) and not
           fSceneInstance.fPotentiallyVisibleSet.fNodes[RenderInstance.fPotentiallyVisibleSetNodeIndex].fAABB.Contains(RenderInstance.fBoundingBox))) then begin
       RenderInstance.fPotentiallyVisibleSetNodeIndex:=fGroup.fSceneInstance.fPotentiallyVisibleSet.GetNodeIndexByAABB(RenderInstance.fBoundingBox);
      end;
      PerInFlightFrameRenderInstanceIndex:=fPerInFlightFrameRenderInstances[aInFlightFrameIndex].AddNew;
      PerInFlightFrameRenderInstance:=@fPerInFlightFrameRenderInstances[aInFlightFrameIndex].Items[PerInFlightFrameRenderInstanceIndex];
      PerInFlightFrameRenderInstance^.PotentiallyVisibleSetNodeIndex:=RenderInstance.fPotentiallyVisibleSetNodeIndex;
      PerInFlightFrameRenderInstance^.BoundingBox:=RenderInstance.fBoundingBox;
      PerInFlightFrameRenderInstance^.ModelMatrix:=RenderInstance.fModelMatrix;
      if RenderInstance.fFirst then begin
       RenderInstance.fFirst:=false;
       PerInFlightFrameRenderInstance^.PreviousModelMatrix:=RenderInstance.fModelMatrix;
      end else begin
       PerInFlightFrameRenderInstance^.ModelMatrix:=RenderInstance.fPreviousModelMatrix;
      end;
      RenderInstance.fPreviousModelMatrix:=RenderInstance.fModelMatrix;
     end else begin
      RenderInstance.fFirst:=true;
      RenderInstance.fPotentiallyVisibleSetNodeIndex:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
     end;
    end;
   end;
  end;

  if aInFlightFrameIndex>=0 then begin
   if fUseRenderInstances then begin
    fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
   end else if assigned(fGroup.fSceneInstance.fPotentiallyVisibleSet) and
               ((fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
                ((fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) and not
                 fSceneInstance.fPotentiallyVisibleSet.fNodes[fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]].fAABB.Contains(fBoundingBox))) then begin
    fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=fGroup.fSceneInstance.fPotentiallyVisibleSet.GetNodeIndexByAABB(fBoundingBox);
   end;
  end;

  if fAABBTreeProxy<0 then begin
   fAABBTreeProxy:=fGroup.fSceneInstance.fAABBTree.CreateProxy(fBoundingBox,TpvPtrInt(Pointer(self)));
  end else begin
   if fUseRenderInstances then begin
    fGroup.fSceneInstance.fAABBTree.MoveProxy(fAABBTreeProxy,TpvAABB.Create(TpvVector3.InlineableCreate(-65536.0,-65536.0,-65536.0),TpvVector3.InlineableCreate(65536.0,65536.0,65536.0)),TpvVector3.Create(1.0,1.0,1.0));
   end else begin
    fGroup.fSceneInstance.fAABBTree.MoveProxy(fAABBTreeProxy,fBoundingBox,TpvVector3.Create(1.0,1.0,1.0));
   end;
  end;

  fPreviousActive:=true;

 end else if fPreviousActive then begin

  if aInFlightFrameIndex>=0 then begin
   fActiveScenes[aInFlightFrameIndex]:=nil;
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

  for Index:=0 to length(fNodes)-1 do begin
   InstanceNode:=@fNodes[Index];
   if assigned(InstanceNode^.Light) then begin
    FreeAndNil(InstanceNode^.Light);
   end;
   if assigned(fAABBTree) and (InstanceNode^.AABBTreeProxy>=0) then begin
    try
     fAABBTree.DestroyProxy(InstanceNode^.AABBTreeProxy);
    finally
     InstanceNode^.AABBTreeProxy:=-1;
    end;
   end;
  end;

  for Index:=0 to fRenderInstances.Count-1 do begin
   RenderInstance:=fRenderInstances[Index];
   RenderInstance.fFirst:=true;
   RenderInstance.fPotentiallyVisibleSetNodeIndex:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
  end;

  if aInFlightFrameIndex>=0 then begin

   fPerInFlightFrameRenderInstances[aInFlightFrameIndex].Count:=0;

   fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;

   fBoundingBoxes[aInFlightFrameIndex]:=TpvAABB.Create(TpvVector3.Origin,TpvVector3.Origin);

  end;

  fPreviousActive:=false;

 end;

 if aInFlightFrameIndex>=0 then begin
  AABBTreeState:=@fAABBTreeStates[aInFlightFrameIndex];
  if assigned(fAABBTree) then begin
   fAABBTree.UpdateGeneration;
   if AABBTreeState^.Generation<>fAABBTree.Generation then begin
    AABBTreeState^.Generation:=fAABBTree.Generation;
    if (length(fAABBTree.Nodes)>0) and (fAABBTree.Root>=0) then begin
     if assigned(fGroup.fSceneInstance.fPotentiallyVisibleSet) then begin
      for Index:=0 to length(fAABBTree.Nodes)-1 do begin
       AABBTreeNode:=@fAABBTree.Nodes[Index];
       if (AABBTreeNode^.UserData=0) or ((AABBTreeNode^.UserData and TpvUInt32($80000000))<>0) then begin
        if AABBTreeNode^.UserData=0 then begin
         AABBTreeNodePotentiallyVisibleSet:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
        end else begin
         AABBTreeNodePotentiallyVisibleSet:=AABBTreeNode^.UserData and TpvUInt32($7fffffff);
        end;
        if ((AABBTreeNodePotentiallyVisibleSet=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
            ((AABBTreeNodePotentiallyVisibleSet<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) and not
            fSceneInstance.fPotentiallyVisibleSet.fNodes[AABBTreeNodePotentiallyVisibleSet].fAABB.Contains(AABBTreeNode^.AABB))) then begin
         AABBTreeNodePotentiallyVisibleSet:=fGroup.fSceneInstance.fPotentiallyVisibleSet.GetNodeIndexByAABB(AABBTreeNode^.AABB);
         if AABBTreeNodePotentiallyVisibleSet=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
          AABBTreeNode^.UserData:=0;
         end else begin
          AABBTreeNode^.UserData:=AABBTreeNodePotentiallyVisibleSet or TpvUInt32($80000000);
         end;
        end;
       end;
      end;
     end;
{    if length(AABBTreeState^.TreeNodes)<length(fAABBTree.Nodes) then begin
      AABBTreeState^.TreeNodes:=copy(fAABBTree.Nodes);
     end else begin
      Move(fAABBTree.Nodes[0],AABBTreeState^.TreeNodes[0],length(fAABBTree.Nodes)*SizeOf(TpvBVHDynamicAABBTree.TTreeNode));
     end;
     AABBTreeState^.Root:=fAABBTree.Root;}
     GenerateAABBTreeSkipList;
    end else begin
//   AABBTreeState^.Root:=-1;
     fAABBTreeSkipLists[aInFlightFrameIndex].Count:=0;
    end;
   end;
  end else begin
// AABBTreeState^.Root:=-1;
   AABBTreeState^.Generation:=High(TpvUInt64);
   fAABBTreeSkipLists[aInFlightFrameIndex].Count:=0;
  end;
 end;

end;

procedure TpvScene3D.TGroup.TInstance.PrepareFrame(const aInFlightFrameIndex:TpvSizeInt);
begin
 if (aInFlightFrameIndex>=0) and
    fActives[aInFlightFrameIndex] and
    assigned(fActiveScenes[aInFlightFrameIndex]) and
    not fHeadless then begin
  if (length(fNodeMatrices)>0) and (length(fNodeMatrices)=fVulkanNodeMatricesBufferCount) then begin
   Move(fNodeMatrices[0],fSceneInstance.fVulkanNodeMatricesBufferData[aInFlightFrameIndex].Items[fVulkanNodeMatricesBufferOffset],length(fNodeMatrices)*SizeOf(TpvMatrix4x4));
  end;
  if (length(fMorphTargetVertexWeights)>0) and (length(fMorphTargetVertexWeights)=fVulkanMorphTargetVertexWeightsBufferCount) then begin
   Move(fMorphTargetVertexWeights[0],fSceneInstance.fVulkanMorphTargetVertexWeightsBufferData[aInFlightFrameIndex].Items[fVulkanMorphTargetVertexWeightsBufferOffset],length(fMorphTargetVertexWeights)*SizeOf(TpvFloat));
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
begin
 if (aInFlightFrameIndex>=0) and
    fActives[aInFlightFrameIndex] and
    assigned(fActiveScenes[aInFlightFrameIndex]) and
    not fHeadless then begin
 end;
end;

procedure TpvScene3D.TGroup.TInstance.GetBakedMeshProcessMorphSkinNode(const aBakedMesh:TpvScene3D.TBakedMesh;
                                                                       const aNode:TpvScene3D.TGroup.TNode;
                                                                       const aInstanceNode:TpvScene3D.TGroup.TInstance.PNode;
                                                                       const aRelative:Boolean;
                                                                       const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]);
type TBakedVertex=record
      Position:TpvVector3;
      Normal:TpvVector3;
     end;
     PBakedVertex=^TBakedVertex;
     TBakedVertices=array of TBakedVertex;
     TTemporaryTriangleIndices=array of TpvSizeInt;
var PrimitiveIndex,VertexIndex,JointBlockIndex,JointIndex,IndexIndex,SideIndex:TpvSizeInt;
    MorphTargetVertexIndex:TpvUInt32;
    Mesh:TpvScene3D.TGroup.TMesh;
    Skin:TpvScene3D.TGroup.TSkin;
    InverseMatrix,Matrix,ModelNodeMatrix,ModelNodeMatrixEx:TpvMatrix4x4;
    Primitive:TpvScene3D.TGroup.TMesh.PPrimitive;
    Vertex:TpvScene3D.PVertex;
    Position,Normal:TpvVector3;
    JointBlock:PJointBlock;
    MorphTargetVertex:TpvScene3D.PMorphTargetVertex;
    BakedVertices:TBakedVertices;
    BakedVertex:PBakedVertex;
    BakedTriangle:TpvScene3D.TBakedMesh.PTriangle;
    TemporaryTriangleIndices:TTemporaryTriangleIndices;
begin
 BakedVertices:=nil;
 try
  Mesh:=aNode.fMesh;
  if assigned(Mesh) then begin
   Skin:=aNode.fSkin;
   if assigned(Skin) then begin
    InverseMatrix:=aInstanceNode^.WorkMatrix.Inverse;
   end else begin
    InverseMatrix:=TpvMatrix4x4.Identity;
   end;
   ModelNodeMatrixEx:=aInstanceNode^.WorkMatrix;
   if not aRelative then begin
    ModelNodeMatrixEx:=ModelNodeMatrixEx*fModelMatrix;
   end;
   for PrimitiveIndex:=0 to length(Mesh.fPrimitives)-1 do begin
    Primitive:=@Mesh.fPrimitives[PrimitiveIndex];
    if assigned(Primitive^.Material) and
       (Primitive^.Material.fData.AlphaMode in aMaterialAlphaModes) then begin
     case Primitive^.PrimitiveTopology of
      TpvScene3D.TPrimitiveTopology.Triangles:begin
       SetLength(BakedVertices,Primitive^.CountVertices);
       for VertexIndex:=Primitive^.StartBufferVertexOffset to (Primitive^.StartBufferVertexOffset+Primitive^.CountVertices)-1 do begin
        Vertex:=@Group.fVertices.Items[VertexIndex];
        BakedVertex:=@BakedVertices[VertexIndex-Primitive^.StartBufferVertexOffset];
        Position:=Vertex^.Position;
        Normal:=OctDecode(Vertex^.Normal);
        MorphTargetVertexIndex:=Vertex^.MorphTargetVertexBaseIndex;
        while MorphTargetVertexIndex<>TpvUInt32($ffffffff) do begin
         MorphTargetVertex:=@Group.fMorphTargetVertices.Items[MorphTargetVertexIndex];
         Position:=Position+(MorphTargetVertex^.Position.xyz*fMorphTargetVertexWeights[MorphTargetVertex^.Index]);
         Normal:=Normal+(MorphTargetVertex^.Normal.xyz*fMorphTargetVertexWeights[MorphTargetVertex^.Index]);
         MorphTargetVertexIndex:=MorphTargetVertex^.Next;
        end;
        Normal:=Normal.Normalize;
        ModelNodeMatrix:=ModelNodeMatrixEx;
        if Vertex^.CountJointBlocks>0 then begin
         Matrix:=TpvMatrix4x4.Identity;
         for JointBlockIndex:=Vertex^.JointBlockBaseIndex to (Vertex^.JointBlockBaseIndex+Vertex^.CountJointBlocks)-1 do begin
          JointBlock:=@fGroup.fJointBlocks.Items[JointBlockIndex];
          for JointIndex:=0 to 3 do begin
           Matrix:=Matrix+((fNodeMatrices[JointBlock^.Joints[JointIndex]]*InverseMatrix)*JointBlock^.Weights[JointIndex]);
          end;
         end;
         ModelNodeMatrix:=Matrix*ModelNodeMatrix;
        end;
        BakedVertex^.Position:=ModelNodeMatrix.MulHomogen(Position);
        BakedVertex^.Normal:=ModelNodeMatrix.Transpose.Inverse.MulBasis(Normal);
       end;
       TemporaryTriangleIndices:=nil;
       try
        SetLength(TemporaryTriangleIndices,Primitive^.CountIndices);
        for IndexIndex:=Primitive^.StartBufferIndexOffset to (Primitive^.StartBufferIndexOffset+Primitive^.CountIndices)-1 do begin
         TemporaryTriangleIndices[IndexIndex-Primitive^.StartBufferIndexOffset]:=Group.fIndices.Items[IndexIndex]-Primitive^.StartBufferVertexOffset;
        end;
        IndexIndex:=0;
        while (IndexIndex+2)<length(TemporaryTriangleIndices) do begin
         for SideIndex:=0 to ord(Primitive^.Material.fData.DoubleSided) and 1 do begin
          BakedTriangle:=pointer(aBakedMesh.fTriangles.AddNew);
          try
           if SideIndex>0 then begin
            BakedTriangle^.Positions[0]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Position;
            BakedTriangle^.Positions[1]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Position;
            BakedTriangle^.Positions[2]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Position;
            BakedTriangle^.Normals[0]:=-BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal;
            BakedTriangle^.Normals[1]:=-BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal;
            BakedTriangle^.Normals[2]:=-BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal;
            BakedTriangle^.Normal:=-(BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal).Normalize;
           end else begin
            BakedTriangle^.Positions[0]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Position;
            BakedTriangle^.Positions[1]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Position;
            BakedTriangle^.Positions[2]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Position;
            BakedTriangle^.Normals[0]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal;
            BakedTriangle^.Normals[1]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal;
            BakedTriangle^.Normals[2]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal;
            BakedTriangle^.Normal:=(BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal).Normalize;
           end;
          finally
          end;
         end;
         inc(IndexIndex,3);
        end;
       finally
        TemporaryTriangleIndices:=nil;
       end;
      end;
      else begin
      end;
     end;
    end;
   end;
  end;
 finally
  BakedVertices:=nil;
 end;
end;

function TpvScene3D.TGroup.TInstance.GetBakedMeshFromSplittedNode(const aNode:TpvScene3D.TGroup.TNode;
                                                                  const aRelative:boolean=false;
                                                                  const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                                                                  const aNodeFilter:TOnNodeFilter=nil):TpvScene3D.TBakedMesh;
type TNodeStack=TpvDynamicStack<TpvScene3D.TGroup.TNode>;
var NodeStack:TNodeStack;
    Node,ChildNode:TpvScene3D.TGroup.TNode;
begin
 result:=TpvScene3D.TBakedMesh.Create;
 if assigned(Node) then begin
  NodeStack.Initialize;
  try
   NodeStack.Push(aNode);
   while not NodeStack.Pop(Node) do begin
    GetBakedMeshProcessMorphSkinNode(result,
                                     Node,
                                     @fNodes[Node.Index],
                                     aRelative,
                                     aMaterialAlphaModes);
    for ChildNode in Node.fSplittedChildren do begin
     NodeStack.Push(ChildNode);
    end;
   end;
  finally
   NodeStack.Finalize;
  end;
 end;
end;

function TpvScene3D.TGroup.TInstance.GetBakedMeshFromSplittedNodeList(const aNodes:TpvScene3D.TGroup.TNodes;
                                                                      const aRelative:boolean=false;
                                                                      const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                                                                      const aNodeFilter:TOnNodeFilter=nil):TpvScene3D.TBakedMesh;
type TNodeStack=TpvDynamicStack<TpvScene3D.TGroup.TNode>;
var NodeStack:TNodeStack;
    Node,ChildNode:TpvScene3D.TGroup.TNode;
begin
 result:=TpvScene3D.TBakedMesh.Create;
 if assigned(Node) then begin
  NodeStack.Initialize;
  try
   for Node in aNodes do begin
    NodeStack.Push(Node);
   end;
   while not NodeStack.Pop(Node) do begin
    GetBakedMeshProcessMorphSkinNode(result,
                                     Node,
                                     @fNodes[Node.Index],
                                     aRelative,
                                     aMaterialAlphaModes);
    for ChildNode in Node.fSplittedChildren do begin
     NodeStack.Push(ChildNode);
    end;
   end;
  finally
   NodeStack.Finalize;
  end;
 end;
end;

function TpvScene3D.TGroup.TInstance.GetBakedMesh(const aRelative:boolean=false;
                                                  const aWithDynamicMeshs:boolean=false;
                                                  const aRootNodeIndex:TpvSizeInt=-1;
                                                  const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                                                  const aNodeFilter:TOnNodeFilter=nil):TpvScene3D.TBakedMesh;
type TNodeStack=TpvDynamicStack<TpvSizeInt>;
var Index,NodeIndex:TpvSizeInt;
    NodeStack:TNodeStack;
    GroupScene:TpvScene3D.TGroup.TScene;
    GroupNode:TpvScene3D.TGroup.TNode;
    GroupInstanceNode:TpvScene3D.TGroup.TInstance.PNode;
begin
 result:=TpvScene3D.TBakedMesh.Create;
 NodeStack.Initialize;
 try
  if (aRootNodeIndex>=0) and (aRootNodeIndex<fGroup.fNodes.Count) then begin
   NodeStack.Push(aRootNodeIndex);
  end else begin
   if (fScene>=0) and (fScene<fGroup.fScenes.Count) then begin
    GroupScene:=fGroup.fScenes[fScene];
   end else if fGroup.fScenes.Count>0 then begin
    GroupScene:=fGroup.fScenes[0];
   end else begin
    GroupScene:=nil;
   end;
   if assigned(GroupScene) then begin
    for Index:=GroupScene.fNodes.Count-1 downto 0 do begin
     NodeStack.Push(GroupScene.fNodes[Index].fIndex);
    end;
   end;
  end;
  while NodeStack.Pop(NodeIndex) do begin
   GroupNode:=fGroup.fNodes[NodeIndex];
   GroupInstanceNode:=@fNodes[NodeIndex];
   if ((aRootNodeIndex>=0) and
       (NodeIndex=aRootNodeIndex)) or
      (aWithDynamicMeshs or
       ((not aWithDynamicMeshs) and
        ((GroupInstanceNode^.CountOverwrites=0) or
         ((GroupInstanceNode^.CountOverwrites=1) and
          ((GroupInstanceNode^.Overwrites[0].Flags=[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Defaults])))))) then begin
    for Index:=GroupNode.fChildren.Count-1 downto 0 do begin
     NodeStack.Push(GroupNode.fChildren[Index].fIndex);
    end;
    if assigned(GroupNode.fMesh) and
       (aWithDynamicMeshs or
         ((not aWithDynamicMeshs) and
          ((not assigned(GroupNode.Skin)) and
           (length(GroupNode.fWeights)=0) and
           ((GroupInstanceNode^.CountOverwrites=0) or
            ((GroupInstanceNode^.CountOverwrites=1) and
             ((GroupInstanceNode^.Overwrites[0].Flags=[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Defaults]))))))) and
         ((not assigned(aNodeFilter)) or aNodeFilter(-1,nil,-1,fGroup,self,GroupNode,GroupInstanceNode)) then begin
     GetBakedMeshProcessMorphSkinNode(result,
                                      GroupNode,
                                      GroupInstanceNode,
                                      aRelative,
                                      aMaterialAlphaModes);
    end;
   end;
  end;
 finally
  NodeStack.Finalize;
 end;
end;

function TpvScene3D.TGroup.TInstance.GetCamera(const aNodeIndex:TPasGLTFSizeInt;
                                               out aCameraMatrix:TpvMatrix4x4;
                                               out aViewMatrix:TpvMatrix4x4;
                                               out aProjectionMatrix:TpvMatrix4x4;
                                               const aReversedZ:boolean;
                                               const aInfiniteFarPlane:boolean;
                                               const aZNear:PpvFloat;
                                               const aZFar:PpvFloat;
                                               const aAspectRatio:TpvFloat):boolean;
const DEG2RAD=PI/180;
var NodeMatrix:TpvMatrix4x4;
    Camera:TpvScene3D.TGroup.TInstance.TCamera;
    AspectRatio:TpvFloat;
begin
 result:=((aNodeIndex>=0) and (aNodeIndex<fGroup.fNodes.Count)) and assigned(fGroup.fNodes[aNodeIndex].Camera);
 if result then begin
  Camera:=fCameras[fGroup.fNodes[aNodeIndex].Camera.Index];
  NodeMatrix:=fNodes[aNodeIndex].WorkMatrix;
  aCameraMatrix:=NodeMatrix;
  aViewMatrix:=NodeMatrix.Inverse;
  case Camera.EffectiveData^.Type_ of
   TpvScene3D.TCameraData.TCameraType.Orthographic:begin
    if aReversedZ or (Camera.EffectiveData^.Orthographic.ZFar<0) then begin
     aProjectionMatrix:=TpvMatrix4x4.CreateOrthoRightHandedZeroToOne(-Camera.EffectiveData^.Orthographic.XMag,
                                                                     Camera.EffectiveData^.Orthographic.XMag,
                                                                     -Camera.EffectiveData^.Orthographic.YMag,
                                                                     Camera.EffectiveData^.Orthographic.YMag,
                                                                     Camera.EffectiveData^.Orthographic.ZFar,
                                                                     Camera.EffectiveData^.Orthographic.ZNear);
    end else begin
     aProjectionMatrix:=TpvMatrix4x4.CreateOrthoRightHandedZeroToOne(-Camera.EffectiveData^.Orthographic.XMag,
                                                                     Camera.EffectiveData^.Orthographic.XMag,
                                                                     -Camera.EffectiveData^.Orthographic.YMag,
                                                                     Camera.EffectiveData^.Orthographic.YMag,
                                                                     Camera.EffectiveData^.Orthographic.ZNear,
                                                                     Camera.EffectiveData^.Orthographic.ZFar);
    end;
    if assigned(aZNear) then begin
     aZNear^:=Camera.EffectiveData^.Orthographic.ZNear;
    end;
    if assigned(aZFar) then begin
     aZFar^:=Camera.EffectiveData^.Orthographic.ZFar;
    end;
    aProjectionMatrix:=aProjectionMatrix*TpvMatrix4x4.FlipYClipSpace;
   end;
   TpvScene3D.TCameraData.TCameraType.Perspective:begin
     if ((aAspectRatio<0.0) and not IsZero(aAspectRatio)) or
        IsZero(Camera.EffectiveData^.Perspective.AspectRatio) then begin
     AspectRatio:=abs(aAspectRatio);
    end else begin
     AspectRatio:=Camera.EffectiveData^.Perspective.AspectRatio;
    end;
    if aReversedZ or (Camera.EffectiveData^.Perspective.ZFar<0.0) then begin
     aProjectionMatrix:=TpvMatrix4x4.CreatePerspectiveRightHandedOneToZero(Camera.EffectiveData^.Perspective.YFov*RAD2DEG,
                                                                           AspectRatio,
                                                                           abs(Camera.EffectiveData^.Perspective.ZNear),
                                                                           IfThen(IsInfinite(Camera.EffectiveData^.Perspective.ZFar) or aInfiniteFarPlane,1024.0,abs(Camera.EffectiveData^.Perspective.ZFar)));
    end else begin
     aProjectionMatrix:=TpvMatrix4x4.CreatePerspectiveRightHandedZeroToOne(Camera.EffectiveData^.Perspective.YFov*RAD2DEG,
                                                                           AspectRatio,
                                                                           abs(Camera.EffectiveData^.Perspective.ZNear),
                                                                           IfThen(IsInfinite(Camera.EffectiveData^.Perspective.ZFar) or aInfiniteFarPlane,1024.0,abs(Camera.EffectiveData^.Perspective.ZFar)));
    end;
    if (Camera.EffectiveData^.Perspective.ZFar<0.0) or aReversedZ then begin
     if IsInfinite(Camera.EffectiveData^.Perspective.ZFar) or aInfiniteFarPlane then begin
      // Convert to reversed infinite Z
      aProjectionMatrix.RawComponents[2,2]:=0.0;
      aProjectionMatrix.RawComponents[2,3]:=-1.0;
      aProjectionMatrix.RawComponents[3,2]:=abs(Camera.EffectiveData^.Perspective.ZNear);
      if assigned(aZNear) then begin
       aZNear^:=Camera.EffectiveData^.Perspective.ZNear;
      end;
      if assigned(aZFar) then begin
       aZFar^:=-Infinity;
      end;
     end else begin
      // Convert to reversed non-infinite Z
      aProjectionMatrix.RawComponents[2,2]:=abs(Camera.EffectiveData^.Perspective.ZNear)/(abs(Camera.EffectiveData^.Perspective.ZFar)-abs(Camera.EffectiveData^.Perspective.ZNear));
      aProjectionMatrix.RawComponents[2,3]:=-1.0;
      aProjectionMatrix.RawComponents[3,2]:=(abs(Camera.EffectiveData^.Perspective.ZNear)*abs(Camera.EffectiveData^.Perspective.ZFar))/(abs(Camera.EffectiveData^.Perspective.ZFar)-abs(Camera.EffectiveData^.Perspective.ZNear));
      if assigned(aZNear) then begin
       aZNear^:=Camera.EffectiveData^.Perspective.ZNear;
      end;
      if assigned(aZFar) then begin
       aZFar^:=Camera.EffectiveData^.Perspective.ZFar;
      end;
     end;
    end else begin
     if assigned(aZNear) then begin
      aZNear^:=Camera.EffectiveData^.Perspective.ZNear;
     end;
     if assigned(aZFar) then begin
      aZFar^:=Camera.EffectiveData^.Perspective.ZFar;
     end;
    end;
    aProjectionMatrix:=aProjectionMatrix*TpvMatrix4x4.FlipYClipSpace;
   end;
   else begin
    result:=false;
   end;
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.SetDirty;
begin
 fDirtyCounter:=fGroup.fSceneInstance.fCountInFlightFrames+1;
end;

function TpvScene3D.TGroup.TInstance.CreateRenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
begin
 result:=TpvScene3D.TGroup.TInstance.TRenderInstance.Create(self);
 fUseRenderInstances:=true;
end;

procedure TpvScene3D.TGroup.TInstance.SetModelMatrix(const aModelMatrix:TpvMatrix4x4);
begin
 if not CompareMem(@fModelMatrix,@aModelMatrix,SizeOf(TpvMatrix4x4)) then begin
  fModelMatrix:=aModelMatrix;
  SetDirty;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.PreparePerInFlightFrameRenderInstances(const aInFlightFrameIndex:TpvSizeInt;
                                                                             const aRenderPassIndex:TpvSizeInt;
                                                                             const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                                                                             const aViewBaseIndex:TpvSizeInt;
                                                                             const aCountViews:TpvSizeInt;
                                                                             const aFrustums:TpvFrustumDynamicArray;
                                                                             const aPotentiallyVisibleSetCulling:boolean;
                                                                             out aFirstInstance:TpvSizeInt;
                                                                             out aInstancesCount:TpvSizeInt);
var PerInFlightFrameRenderInstanceIndex,FrustumIndex,ViewIndex:TpvSizeInt;
    ViewPotentiallyVisibleSetNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
    DoCulling,PotentiallyVisible:boolean;
    GlobalVulkanInstanceMatrixDynamicArray:PGlobalVulkanInstanceMatrixDynamicArray;
    PerInFlightFrameRenderInstanceDynamicArray:TpvScene3D.TGroup.TInstance.PPerInFlightFrameRenderInstanceDynamicArray;
    PerInFlightFrameRenderInstance:TpvScene3D.TGroup.TInstance.PPerInFlightFrameRenderInstance;
begin

 if fUseRenderInstances then begin

  GlobalVulkanInstanceMatrixDynamicArray:=@fSceneInstance.fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex];

  aFirstInstance:=GlobalVulkanInstanceMatrixDynamicArray^.Count shr 1;
  aInstancesCount:=0;

  DoCulling:=fGroup.fCulling and ((length(aFrustums)>0) or aPotentiallyVisibleSetCulling);

  PerInFlightFrameRenderInstanceDynamicArray:=@fPerInFlightFrameRenderInstances[aInFlightFrameIndex];

  for PerInFlightFrameRenderInstanceIndex:=0 to PerInFlightFrameRenderInstanceDynamicArray^.Count-1 do begin

   PerInFlightFrameRenderInstance:=@PerInFlightFrameRenderInstanceDynamicArray.Items[PerInFlightFrameRenderInstanceIndex];

   PotentiallyVisible:=true;

   if DoCulling then begin

    if length(aFrustums)>0 then begin
     PotentiallyVisible:=false;
     for FrustumIndex:=0 to length(aFrustums)-1 do begin
      if aFrustums[FrustumIndex].AABBInFrustum(PerInFlightFrameRenderInstance^.BoundingBox)<>TpvFrustum.COMPLETE_OUT then begin
       PotentiallyVisible:=true;
       break;
      end;
     end;
    end;

    if PotentiallyVisible and aPotentiallyVisibleSetCulling then begin
     if PerInFlightFrameRenderInstance^.PotentiallyVisibleSetNodeIndex<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
      PotentiallyVisible:=false;
      for ViewIndex:=aViewBaseIndex to (aViewBaseIndex+aCountViews)-1 do begin
       ViewPotentiallyVisibleSetNodeIndex:=aViewNodeIndices[ViewIndex];
       if (ViewPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
          fSceneInstance.fPotentiallyVisibleSet.GetNodeVisibility(PerInFlightFrameRenderInstance^.PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex) then begin
        PotentiallyVisible:=true;
        break;
       end;
      end;
     end;
    end;

   end;

   if PotentiallyVisible then begin
    GlobalVulkanInstanceMatrixDynamicArray^.Add(PerInFlightFrameRenderInstance^.ModelMatrix);
    GlobalVulkanInstanceMatrixDynamicArray^.Add(PerInFlightFrameRenderInstance^.PreviousModelMatrix);
    inc(aInstancesCount);
   end;

  end;

 end else begin

  aFirstInstance:=0;
  aInstancesCount:=1;

 end;

end;

procedure TpvScene3D.TGroup.TInstance.Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                              const aRendererInstance:TObject;
                                              const aRenderPassIndex:TpvSizeInt;
                                              const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                                              const aViewBaseIndex:TpvSizeInt;
                                              const aCountViews:TpvSizeInt;
                                              const aFrustums:TpvFrustumDynamicArray;
                                              const aPotentiallyVisibleSetCulling:boolean;
                                              const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes;
                                              const aFrustumCullMask:TpvUInt32);
var ViewIndex,FrustumIndex,SkipListItemIndex,SkipListItemCount,DrawChoreographyBatchItemIndex,
    FirstInstance,InstancesCount:TpvSizeInt;
    PotentiallyVisibleSetNodeIndex,
    ViewPotentiallyVisibleSetNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
    Masks:array[-1..15] of TpvUInt32;
    RendererInstanceID:TpvUInt32;
    HaveNodeFilter:Boolean;
    GroupOnNodeFilter,GlobalOnNodeFilter:TpvScene3D.TGroup.TInstance.TOnNodeFilter;
    Scene:TpvScene3D.TGroup.TScene;
    Node:TpvScene3D.TGroup.TNode;
    InstanceScene:TpvScene3D.TGroup.TInstance.TScene;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    DrawChoreographyBatchItemMaterialAlphaModeBuckets:PDrawChoreographyBatchItemMaterialAlphaModeBuckets;
    PotentiallyVisible,DoCulling:boolean;
    AABBTreeSkipList:TpvScene3D.TGroup.TInstance.PAABBTreeSkipList;
    AABBTreeSkipListItem:TpvScene3D.TGroup.TInstance.PAABBTreeSkipListItem;
    SkipListItem:TpvScene3D.TGroup.TScene.PSkipListItem;
    DrawChoreographyBatchItemIndices:PSizeIntDynamicArray;
    DrawChoreographyBatchItem:TpvScene3D.TDrawChoreographyBatchItem;
begin

 FirstInstance:=0;
 InstancesCount:=0;

 if fActives[aInFlightFrameIndex] then begin

  GroupOnNodeFilter:=fGroup.fOnNodeFilter;
  GlobalOnNodeFilter:=fGroup.fSceneInstance.fOnNodeFilter;

  Scene:=fActiveScenes[aInFlightFrameIndex];

  if assigned(Scene) then begin

   PotentiallyVisible:=true;

   DoCulling:=fGroup.fCulling and not fUseRenderInstances;

   DrawChoreographyBatchItemMaterialAlphaModeBuckets:=@TpvScene3DRendererInstance(aRendererInstance).DrawChoreographyBatchItemFrameBuckets[aInFlightFrameIndex,aRenderPassIndex];

   InstanceScene:=fScenes[Scene.Index];

   if assigned(fAABBTree) then begin

    AABBTreeSkipList:=@fAABBTreeSkipLists[aInFlightFrameIndex];

    if AABBTreeSkipList^.Count>0 then begin

     HaveNodeFilter:=(length(fCullVisibleBitmaps[aInFlightFrameIndex])>0) and
                     (assigned(fOnNodeFilter) or
                      assigned(GroupOnNodeFilter) or
                      assigned(GlobalOnNodeFilter));

     if HaveNodeFilter then begin
      TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fCullVisibleBitmapLocks[aInFlightFrameIndex]);
     end;
     try

      if HaveNodeFilter then begin

       FillChar(fCullVisibleBitmaps[aInFlightFrameIndex][0],length(fCullVisibleBitmaps[aInFlightFrameIndex])*SizeOf(TpvUInt32),#0);

       SkipListItemIndex:=0;
       SkipListItemCount:=length(Scene.fSkipList);
       while SkipListItemIndex<SkipListItemCount do begin
        SkipListItem:=@Scene.fSkipList[SkipListItemIndex];
        Node:=fGroup.fNodes[SkipListItem^.NodeIndex];
        InstanceNode:=@fNodes[SkipListItem^.NodeIndex];
        if (((not assigned(fOnNodeFilter)) or fOnNodeFilter(aInFlightFrameIndex,aRendererInstance,aRenderPassIndex,Group,self,Node,InstanceNode)) and
            ((not assigned(GroupOnNodeFilter)) or GroupOnNodeFilter(aInFlightFrameIndex,aRendererInstance,aRenderPassIndex,Group,self,Node,InstanceNode)) and
            ((not assigned(GlobalOnNodeFilter)) or GlobalOnNodeFilter(aInFlightFrameIndex,aRendererInstance,aRenderPassIndex,Group,self,Node,InstanceNode))) then begin
         fCullVisibleBitmaps[aInFlightFrameIndex][SkipListItem^.NodeIndex shr 5]:=fCullVisibleBitmaps[aInFlightFrameIndex][SkipListItem^.NodeIndex shr 5] or (TpvUInt32(1) shl (SkipListItem^.NodeIndex and 31));
         inc(SkipListItemIndex);
        end else begin
         if SkipListItem^.SkipCount<=0 then begin
          break;
         end else begin
          inc(SkipListItemIndex,SkipListItem^.SkipCount);
         end;
        end;
       end;

      end;

      Masks[-1]:=TpvUInt32($ffffffff);

      SkipListItemIndex:=0;
      SkipListItemCount:=AABBTreeSkipList^.Count;
      while SkipListItemIndex<SkipListItemCount do begin

       AABBTreeSkipListItem:=@AABBTreeSkipList.Items[SkipListItemIndex];

       PotentiallyVisible:=true;

       if DoCulling then begin

        if aPotentiallyVisibleSetCulling then begin

         if (AABBTreeSkipListItem^.UserData and TpvUInt32($80000000))<>0 then begin

          PotentiallyVisibleSetNodeIndex:=AABBTreeSkipListItem^.UserData and TpvUInt32($7fffffff);
          if PotentiallyVisibleSetNodeIndex<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
           PotentiallyVisible:=false;
           for ViewIndex:=aViewBaseIndex to (aViewBaseIndex+aCountViews)-1 do begin
            ViewPotentiallyVisibleSetNodeIndex:=aViewNodeIndices[ViewIndex];
            if (ViewPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
               fSceneInstance.fPotentiallyVisibleSet.GetNodeVisibility(PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex) then begin
             PotentiallyVisible:=true;
             break;
            end;
           end;
          end;

         end else if (AABBTreeSkipListItem^.UserData>=1) and (AABBTreeSkipListItem^.UserData<=TpvUInt32($7fffffff)) then begin

          PotentiallyVisibleSetNodeIndex:=fNodes[AABBTreeSkipListItem^.UserData-1].PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex];
          if PotentiallyVisibleSetNodeIndex<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
           PotentiallyVisible:=false;
           for ViewIndex:=aViewBaseIndex to (aViewBaseIndex+aCountViews)-1 do begin
            ViewPotentiallyVisibleSetNodeIndex:=aViewNodeIndices[ViewIndex];
            if (ViewPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
               fSceneInstance.fPotentiallyVisibleSet.GetNodeVisibility(PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex) then begin
             PotentiallyVisible:=true;
             break;
            end;
           end;
          end;

         end;

        end;

        if PotentiallyVisible then begin
         if length(aFrustums)>0 then begin
          if length(aFrustums)=1 then begin
           if Masks[AABBTreeSkipListItem^.Level-1]<>$40000000 then begin
            if AABBTreeSkipListItem^.Level<=High(Masks) then begin
             Masks[AABBTreeSkipListItem^.Level]:=Masks[AABBTreeSkipListItem^.Level-1];
             PotentiallyVisible:=not ((((Masks[AABBTreeSkipListItem^.Level] and $80000000)<>0) and (aFrustums[0].AABBInFrustum(AABBTreeSkipListItem^.AABB,Masks[AABBTreeSkipListItem^.Level])=TpvFrustum.COMPLETE_OUT)));
            end else begin
             PotentiallyVisible:=aFrustums[0].AABBInFrustum(AABBTreeSkipListItem^.AABB)<>TpvFrustum.COMPLETE_OUT;
            end;
           end;
          end else begin
           PotentiallyVisible:=false;
           for FrustumIndex:=0 to length(aFrustums)-1 do begin
            if aFrustums[FrustumIndex].AABBInFrustum(AABBTreeSkipListItem^.AABB)<>TpvFrustum.COMPLETE_OUT then begin
             PotentiallyVisible:=true;
             break;
            end;
           end;
          end;
         end;
        end;

       end;

       if PotentiallyVisible then begin

        if (AABBTreeSkipListItem^.UserData>=1) and (AABBTreeSkipListItem^.UserData<=TpvUInt32($7fffffff)) then begin

         Node:=fGroup.fNodes[AABBTreeSkipListItem^.UserData-1];

         if (not HaveNodeFilter) or
            ((fCullVisibleBitmaps[aInFlightFrameIndex][Node.Index shr 5] and (TpvUInt32(1) shl (Node.Index and 31))<>0)) then begin

          InstanceNode:=@fNodes[AABBTreeSkipListItem^.UserData-1];

          DrawChoreographyBatchItemIndices:=@Node.fDrawChoreographyBatchItemIndices;
          for DrawChoreographyBatchItemIndex:=0 to DrawChoreographyBatchItemIndices^.Count-1 do begin
           DrawChoreographyBatchItem:=InstanceScene.fDrawChoreographyBatchItems[DrawChoreographyBatchItemIndices^.Items[DrawChoreographyBatchItemIndex]];
           if DrawChoreographyBatchItem.fMaterial.fVisible and
              (DrawChoreographyBatchItem.fAlphaMode in aMaterialAlphaModes) and
             (DrawChoreographyBatchItem.fCountIndices>0) then begin
            DrawChoreographyBatchItemMaterialAlphaModeBuckets^[DrawChoreographyBatchItem.fAlphaMode,
                                                               DrawChoreographyBatchItem.fPrimitiveTopology,
                                                               DoubleSidedFaceCullingModes[DrawChoreographyBatchItem.fDoubleSided,
                                                                                           InstanceNode^.InverseFrontFaces]].Add(DrawChoreographyBatchItem);
           end;
          end;

         end;

        end;

        inc(SkipListItemIndex);

       end else begin

        if AABBTreeSkipListItem^.SkipCount<=0 then begin
         break;
        end else begin
         inc(SkipListItemIndex,AABBTreeSkipListItem^.SkipCount);
        end;

       end;

      end;

     finally
      if HaveNodeFilter then begin
       TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fCullVisibleBitmapLocks[aInFlightFrameIndex]);
      end;
     end;

    end;

   end else begin

    Masks[-1]:=TpvUInt32($ffffffff);

    SkipListItemIndex:=0;

    SkipListItemCount:=length(Scene.fSkipList);

    while SkipListItemIndex<SkipListItemCount do begin

     SkipListItem:=@Scene.fSkipList[SkipListItemIndex];

     Node:=fGroup.fNodes[SkipListItem^.NodeIndex];

     InstanceNode:=@fNodes[SkipListItem^.NodeIndex];

     PotentiallyVisible:=true;

     if DoCulling then begin

      if aPotentiallyVisibleSetCulling then begin
       PotentiallyVisibleSetNodeIndex:=InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex];
       if PotentiallyVisibleSetNodeIndex<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
        PotentiallyVisible:=false;
        for ViewIndex:=aViewBaseIndex to (aViewBaseIndex+aCountViews)-1 do begin
         ViewPotentiallyVisibleSetNodeIndex:=aViewNodeIndices[ViewIndex];
         if (ViewPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
            fSceneInstance.fPotentiallyVisibleSet.GetNodeVisibility(PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex) then begin
          PotentiallyVisible:=true;
          break;
         end;
        end;
       end;
      end;

      if PotentiallyVisible then begin
       if InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
        if length(aFrustums)>0 then begin
         if length(aFrustums)=1 then begin
          if Masks[SkipListItem^.Level-1]<>$40000000 then begin
           if SkipListItem^.Level<=High(Masks) then begin
            Masks[SkipListItem^.Level]:=Masks[SkipListItem^.Level-1];
            PotentiallyVisible:=not ((((Masks[SkipListItem^.Level] and $80000000)<>0) and (aFrustums[0].AABBInFrustum(InstanceNode^.BoundingBoxes[aInFlightFrameIndex],Masks[SkipListItem^.Level])=TpvFrustum.COMPLETE_OUT)));
           end else begin
            PotentiallyVisible:=aFrustums[0].AABBInFrustum(InstanceNode^.BoundingBoxes[aInFlightFrameIndex])<>TpvFrustum.COMPLETE_OUT;
           end;
          end;
         end else begin
          PotentiallyVisible:=false;
          for FrustumIndex:=0 to length(aFrustums)-1 do begin
           if aFrustums[FrustumIndex].AABBInFrustum(InstanceNode^.BoundingBoxes[aInFlightFrameIndex])<>TpvFrustum.COMPLETE_OUT then begin
            PotentiallyVisible:=true;
            break;
           end;
          end;
         end;
        end;
       end;
      end;

     end;

     if PotentiallyVisible and
        (((not assigned(fOnNodeFilter)) or fOnNodeFilter(aInFlightFrameIndex,aRendererInstance,aRenderPassIndex,Group,self,Node,InstanceNode)) and
         ((not assigned(GroupOnNodeFilter)) or GroupOnNodeFilter(aInFlightFrameIndex,aRendererInstance,aRenderPassIndex,Group,self,Node,InstanceNode)) and
         ((not assigned(GlobalOnNodeFilter)) or GlobalOnNodeFilter(aInFlightFrameIndex,aRendererInstance,aRenderPassIndex,Group,self,Node,InstanceNode))) then begin

      DrawChoreographyBatchItemIndices:=@Node.fDrawChoreographyBatchItemIndices;
      for DrawChoreographyBatchItemIndex:=0 to DrawChoreographyBatchItemIndices^.Count-1 do begin
       DrawChoreographyBatchItem:=InstanceScene.fDrawChoreographyBatchItems[DrawChoreographyBatchItemIndices^.Items[DrawChoreographyBatchItemIndex]];
       if DrawChoreographyBatchItem.fMaterial.fVisible and
          (DrawChoreographyBatchItem.fAlphaMode in aMaterialAlphaModes) and
         (DrawChoreographyBatchItem.fCountIndices>0) then begin
        DrawChoreographyBatchItemMaterialAlphaModeBuckets^[DrawChoreographyBatchItem.fAlphaMode,
                                                           DrawChoreographyBatchItem.fPrimitiveTopology,
                                                           DoubleSidedFaceCullingModes[DrawChoreographyBatchItem.fDoubleSided,
                                                                                       InstanceNode^.InverseFrontFaces]].Add(DrawChoreographyBatchItem);
       end;
      end;

      inc(SkipListItemIndex);

     end else begin

      if SkipListItem^.SkipCount<=0 then begin
       break;
      end else begin
       inc(SkipListItemIndex,SkipListItem^.SkipCount);
      end;

     end;

    end;

   end;

   PreparePerInFlightFrameRenderInstances(aInFlightFrameIndex,
                                          aRenderPassIndex,
                                          aViewNodeIndices,
                                          aViewBaseIndex,
                                          aCountViews,
                                          aFrustums,
                                          aPotentiallyVisibleSetCulling,
                                          FirstInstance,
                                          InstancesCount);

  end;

 end;

 RendererInstanceID:=TpvScene3DRendererInstance(aRendererInstance).ID;

 fVulkanPerInFlightFrameFirstInstances[aInFlightFrameIndex,RendererInstanceID,aRenderPassIndex]:=FirstInstance;
 fVulkanPerInFlightFrameInstancesCounts[aInFlightFrameIndex,RendererInstanceID,aRenderPassIndex]:=InstancesCount;

end;

procedure TpvScene3D.TGroup.TInstance.UpdateCachedVertices(const aInFlightFrameIndex:TpvSizeInt);
var NodeIndex,IndicesStart,IndicesCount,InFlightFrameIndex,
    DrawChoreographyBatchUniqueItemIndex,
    CountDrawChoreographyBatchUniqueItems:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    Node:TpvScene3D.TGroup.TInstance.PNode;
    DrawChoreographyBatchUniqueItem:TpvScene3D.TDrawChoreographyBatchItem;
    CachedVertexRange:TpvScene3D.TCachedVertexRange;
begin

 if fActives[aInFlightFrameIndex] and not fHeadless then begin

  Scene:=fActiveScenes[aInFlightFrameIndex];

  if assigned(Scene) then begin

   FillChar(fCacheVerticesNodeDirtyBitmap[0],Length(fCacheVerticesNodeDirtyBitmap)*SizeOf(TpvUInt32),#$0);

   for NodeIndex:=0 to length(fNodes)-1 do begin
    Node:=@fNodes[NodeIndex];

    if Node^.CacheVerticesDirtyCounter>0 then begin

     dec(Node^.CacheVerticesDirtyCounter);

     inc(Node^.CacheVerticesGeneration);

     if Node^.CacheVerticesGeneration=0 then begin

      // Handle generation value overflow
      Node^.CacheVerticesGeneration:=1;

      for InFlightFrameIndex:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
       Node^.CacheVerticesGenerations[aInFlightFrameIndex]:=0;
      end;

     end;

    end;

    if Node^.CacheVerticesGenerations[aInFlightFrameIndex]<>Node^.CacheVerticesGeneration then begin
     Node^.CacheVerticesGenerations[aInFlightFrameIndex]:=Node^.CacheVerticesGeneration;
     fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5]:=fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5] or (TpvUInt32(1) shl (NodeIndex and 31));
    end;

   end;

   IndicesStart:=0;
   IndicesCount:=0;

   DrawChoreographyBatchUniqueItemIndex:=0;
   CountDrawChoreographyBatchUniqueItems:=Scene.fDrawChoreographyBatchUniqueItems.Count;

   while DrawChoreographyBatchUniqueItemIndex<CountDrawChoreographyBatchUniqueItems do begin

    DrawChoreographyBatchUniqueItem:=Scene.fDrawChoreographyBatchUniqueItems[DrawChoreographyBatchUniqueItemIndex];
    inc(DrawChoreographyBatchUniqueItemIndex);

    NodeIndex:=TpvScene3D.TGroup.TNode(DrawChoreographyBatchUniqueItem.Node).fIndex;

    if (fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5] and (TpvUInt32(1) shl (NodeIndex and 31)))<>0 then begin

     IndicesStart:=DrawChoreographyBatchUniqueItem.fStartIndex;
     IndicesCount:=DrawChoreographyBatchUniqueItem.fCountIndices;

     while DrawChoreographyBatchUniqueItemIndex<CountDrawChoreographyBatchUniqueItems do begin

      DrawChoreographyBatchUniqueItem:=Scene.fDrawChoreographyBatchUniqueItems[DrawChoreographyBatchUniqueItemIndex];

      NodeIndex:=TpvScene3D.TGroup.TNode(DrawChoreographyBatchUniqueItem.Node).fIndex;

      if ((fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5] and (TpvUInt32(1) shl (NodeIndex and 31)))<>0) and
         ((IndicesStart+IndicesCount)=DrawChoreographyBatchUniqueItem.fStartIndex) then begin

       inc(IndicesCount,DrawChoreographyBatchUniqueItem.fCountIndices);
       inc(DrawChoreographyBatchUniqueItemIndex);

      end else begin
       break;
      end;

     end;

     if IndicesCount>0 then begin

      CachedVertexRange.Offset:=fVulkanDrawUniqueIndexBufferOffset+IndicesStart;
      CachedVertexRange.Count:=IndicesCount;

      fGroup.fSceneInstance.fCachedVertexRanges.Add(CachedVertexRange);

     end;

    end;

   end;

  end;

 end;

end;

{ TpvScene3D }

constructor TpvScene3D.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource;const aMetaResource:TpvMetaResource;const aVulkanDevice:TpvVulkanDevice;const aUseBufferDeviceAddress:boolean;const aCountInFlightFrames:TpvSizeInt);
var Index,InFlightFrameIndex,RenderPassIndex:TpvSizeInt;
    MaterialAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TPrimitiveTopology;
    FaceCullingMode:TFaceCullingMode;
begin

 inherited Create(aResourceManager,aParent,aMetaResource);

 fLoadLock:=TPasMPSpinLock.Create;

 if assigned(aVulkanDevice) then begin
  fVulkanDevice:=aVulkanDevice;
 end else if assigned(pvApplication) then begin
  fVulkanDevice:=pvApplication.VulkanDevice;
 end else begin
  fVulkanDevice:=nil;
 end;

 fRendererInstanceIDManager:=TRendererInstanceIDManager.Create;

 fFreeQueueLock:=TPasMPSlimReaderWriterLock.Create;

 fFreeQueue:=TFreeQueue.Create;

 fObjectListLock:=TPasMPCriticalSection.Create;

 fObjectList:=TpvObjectList.Create;
 fObjectList.OwnsObjects:=false;

 fCountInFlightFrames:=Min(Max(aCountInFlightFrames,1),MaxInFlightFrames);

 fLock:=TPasMPSpinLock.Create;

 fImageDescriptorGenerationLock:=TPasMPSpinLock.Create;

 fMaterialDataGenerationLock:=TPasMPSpinLock.Create;

(*{$ifdef Linux}
 if TpvVulkanVendorID(fVulkanDevice.PhysicalDevice.Properties.vendorID)=TpvVulkanVendorID.NVIDIA then begin
  fBufferStreamingMode:=TBufferStreamingMode.Staging;
 end else {$endif}*)if assigned(fVulkanDevice) and fVulkanDevice.MemoryManager.CompleteTotalMemoryMappable then begin
  fBufferStreamingMode:=TBufferStreamingMode.Direct;
 end else begin
  fBufferStreamingMode:=TBufferStreamingMode.Staging;
 end;

 fMultiDrawSupport:=(fVulkanDevice.EnabledExtensionNames.IndexOf(VK_EXT_MULTI_DRAW_EXTENSION_NAME)>0) and
                    (fVulkanDevice.MultiDrawFeaturesEXT.multiDraw<>VK_FALSE);

 fMaxMultiDrawCount:=fVulkanDevice.PhysicalDevice.MultiDrawPropertiesEXT.maxMultiDrawCount;

 fHardwareRaytracingSupport:=(fVulkanDevice.RayTracingPipelineFeaturesKHR.rayTracingPipeline<>VK_FALSE) and
                             (fVulkanDevice.RayQueryFeaturesKHR.rayQuery<>VK_FALSE);

 if fHardwareRaytracingSupport then begin
  fAccelerationStructureInputBufferUsageFlags:=TVkBufferUsageFlags(VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR) or
                                               TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT);
 end else begin
  fAccelerationStructureInputBufferUsageFlags:=TVkBufferUsageFlags(0);
 end;

//A!
//fDrawBufferStorageMode:=TDrawBufferStorageMode.SeparateBuffers;

 fMeshGenerationCounter:=1;

 fNewInstanceListLock:=TPasMPSlimReaderWriterLock.Create;

 fNewInstances:=TpvScene3D.TGroup.TInstances.Create;
 fNewInstances.OwnsObjects:=false;

 fBufferRangeAllocatorLock:=TPasMPCriticalSection.Create;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fGlobalVulkanInstanceMatrixDynamicArrays[Index].Initialize;
  fGlobalVulkanInstanceMatrixDynamicArrays[Index].Resize(65536);
  fGlobalVulkanInstanceMatrixDynamicArrays[Index].Count:=0;
  fGlobalVulkanInstanceMatrixDynamicArrays[Index].Add(TpvMatrix4x4.Identity);
  fGlobalVulkanInstanceMatrixDynamicArrays[Index].Add(TpvMatrix4x4.Identity);
 end;

 fVulkanDynamicVertexBufferData.Initialize;
 fVulkanStaticVertexBufferData.Initialize;
 fVulkanDrawIndexBufferData.Initialize;
 fVulkanDrawUniqueIndexBufferData.Initialize;
 fVulkanMorphTargetVertexBufferData.Initialize;
 fVulkanJointBlockBufferData.Initialize;

 fVkMultiDrawIndexedInfoEXTDynamicArray.Initialize;
 fVkMultiDrawIndexedInfoEXTFirstInstance:=0;
 fVkMultiDrawIndexedInfoEXTInstancesCount:=1;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fVulkanNodeMatricesBufferData[Index].Initialize;
  fVulkanMorphTargetVertexWeightsBufferData[Index].Initialize;
 end;

 fVulkanVertexBufferRangeAllocator:=TpvBufferRangeAllocator.Create;

 fVulkanDrawIndexBufferRangeAllocator:=TpvBufferRangeAllocator.Create;

 fVulkanDrawUniqueIndexBufferRangeAllocator:=TpvBufferRangeAllocator.Create;

 fVulkanMorphTargetVertexBufferRangeAllocator:=TpvBufferRangeAllocator.Create;

 fVulkanJointBlockBufferRangeAllocator:=TpvBufferRangeAllocator.Create;

 fVulkanNodeMatricesBufferRangeAllocator:=TpvBufferRangeAllocator.Create;

 fVulkanMorphTargetVertexWeightsBufferRangeAllocator:=TpvBufferRangeAllocator.Create;

 fVulkanLongTermStaticBuffers:=TpvScene3D.TVulkanLongTermStaticBuffers.Create(self);

 fVulkanShortTermDynamicBuffers:=TpvScene3D.TVulkanShortTermDynamicBuffers.Create(self);

 fCachedVertexRanges.Initialize;

 fUseBufferDeviceAddress:=aUseBufferDeviceAddress;

 fUploaded:=false;

 fInUpload:=false;

 fHasTransmission:=false;

 fPotentiallyVisibleSet:=TpvScene3D.TPotentiallyVisibleSet.Create;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fDebugPrimitiveVertexDynamicArrays[Index]:=TpvScene3D.TDebugPrimitiveVertexDynamicArray.Create;
 end;

 fPointerToParticles:=@fParticles;

 FillChar(fParticleAliveBitmap,SizeOf(TParticleAliveBitmap),#0);

 fParticleIndexCounter:=0;

 FillChar(fCountInFlightFrameParticleVertices,SizeOf(fCountInFlightFrameParticleVertices),#0);

 fSkyBoxBrightnessFactor:=1.0;

 fTechniques:=TpvTechniques.Create;

 fCullObjectIDLock:=TPasMPSlimReaderWriterLock.Create;

 fCullObjectIDManager:=TpvScene3D.TIDManager.Create;

 fMaxCullObjectID:=0;

 fImageListLock:=TPasMPCriticalSection.Create;

 fImages:=TImages.Create;
 fImages.OwnsObjects:=false;

 fImageIDManager:=TIDManager.Create;

 fImageIDHashMap:=TImageIDHashMap.Create(nil);

 fImageHashMap:=TImageHashMap.Create(nil);

 fSamplerListLock:=TPasMPCriticalSection.Create;

 fSamplers:=TSamplers.Create;
 fSamplers.OwnsObjects:=false;

 fSamplerIDManager:=TIDManager.Create;

 fSamplerIDHashMap:=TSamplerIDHashMap.Create(nil);

 fSamplerHashMap:=TSamplerHashMap.Create(nil);

 fTextureListLock:=TPasMPCriticalSection.Create;

 fTextures:=TTextures.Create;
 fTextures.OwnsObjects:=false;

 fTextureIDManager:=TIDManager.Create;

 fTextureIDHashMap:=TTextureIDHashMap.Create(nil);

 fTextureHashMap:=TTextureHashMap.Create(nil);

 fMaterialListLock:=TPasMPCriticalSection.Create;

 fMaterials:=TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 fMaxMaterialID:=2;

 fMaterialIDManager:=TIDManager.Create;

 fMaterialIDHashMap:=TMaterialIDHashMap.Create(nil);

 FillChar(fMaterialIDDirtyMaps,SizeOf(TMaterialIDDirtyMaps),#0);

 FillChar(fMaterialIDToUpdateDirtyMaps,SizeOf(TMaterialIDDirtyMaps),#0);

 FillChar(fMaterialIDMap,SizeOf(TMaterialIDMap),#0);

 fMaterialHashMap:=TMaterialHashMap.Create(nil);

 FillChar(fInFlightFrameMaterialBufferDataGenerations,SizeOf(TInFlightFrameMaterialBufferDataGenerations),#$ff);

 fDefaultSampler:=TSampler.Create(ResourceManager,self);
 fDefaultSampler.AssignFromDefault;
 fDefaultSampler.IncRef;

 fWhiteImage:=TpvScene3D.TImage.Create(ResourceManager,self);
 fWhiteImage.AssignFromWhiteTexture;
 fWhiteImage.IncRef;
 fWhiteImage.LoadData;

 fWhiteTexture:=TpvScene3D.TTexture.Create(ResourceManager,self);
 fWhiteTexture.AssignFromWhiteTexture;
 fWhiteTexture.IncRef;

 fDefaultNormalMapImage:=TpvScene3D.TImage.Create(ResourceManager,self);
 fDefaultNormalMapImage.AssignFromDefaultNormalMapTexture;
 fDefaultNormalMapImage.IncRef;
 fDefaultNormalMapImage.LoadData;

 fDefaultNormalMapTexture:=TpvScene3D.TTexture.Create(ResourceManager,self);
 fDefaultNormalMapTexture.AssignFromDefaultNormalMapTexture;
 fDefaultNormalMapTexture.IncRef;

 fDefaultParticleImage:=TpvScene3D.TImage.Create(ResourceManager,self);
 fDefaultParticleImage.AssignFromDefaultParticleTexture;
 fDefaultParticleImage.IncRef;
 fDefaultParticleImage.LoadData;

 fDefaultParticleTexture:=TpvScene3D.TTexture.Create(ResourceManager,self);
 fDefaultParticleTexture.AssignFromDefaultParticleTexture;
 fDefaultParticleTexture.IncRef;

 fEmptyMaterial:=TpvScene3D.TMaterial.Create(ResourceManager,self);
 fEmptyMaterial.AssignFromEmpty;
 fEmptyMaterial.IncRef;

 fPrimaryLightDirection:=TpvVector3.InlineableCreate(0.5,-1.0,-1.0).Normalize;

 fPrimaryShadowMapLightDirection:=TpvVector3.InlineableCreate(0.5,-1.0,-1.0).Normalize;

//fPrimaryLightDirection:=TpvVector3.InlineableCreate(0.333333333333,-0.666666666666,-0.666666666666).Normalize;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLights[Index]:=TpvScene3D.TLights.Create;
  fLights[Index].OwnsObjects:=true;
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fCountLights[Index]:=0;
 end;

 fGroupListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroups:=TGroups.Create;
 fGroups.OwnsObjects:=false;

 fGroupInstanceListLock:=TPasMPSlimReaderWriterLock.Create;
 fGroupInstances:=TGroup.TInstances.Create;
 fGroupInstances.OwnsObjects:=false;

 ReleaseFrameDelay:=fCountInFlightFrames+1;

 if assigned(fVulkanDevice) then begin

  fMeshComputeVulkanDescriptorSet0Layout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);

  // Group - Vertices
  fMeshComputeVulkanDescriptorSet0Layout.AddBinding(0,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  // Group - Indices
  fMeshComputeVulkanDescriptorSet0Layout.AddBinding(1,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  // Group - Morph target vertices
  fMeshComputeVulkanDescriptorSet0Layout.AddBinding(2,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  // Group - Joint blocks
  fMeshComputeVulkanDescriptorSet0Layout.AddBinding(3,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  fMeshComputeVulkanDescriptorSet0Layout.Initialize;
  fVulkanDevice.DebugUtils.SetObjectName(fMeshComputeVulkanDescriptorSet0Layout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3D.fMeshComputeVulkanDescriptorSet0Layout');

  //////

  fMeshComputeVulkanDescriptorSet1Layout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);

  // Group - Cached vertices
  fMeshComputeVulkanDescriptorSet1Layout.AddBinding(0,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  // Group - Cached vertex generations
  fMeshComputeVulkanDescriptorSet1Layout.AddBinding(1,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  // Instance - Node matrices
  fMeshComputeVulkanDescriptorSet1Layout.AddBinding(2,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  // Instance - Morph target weights
  fMeshComputeVulkanDescriptorSet1Layout.AddBinding(3,
                                                    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                    1,
                                                    TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                    []);

  if fHardwareRaytracingSupport then begin
   // Group - Cached raytracing vertices
   fMeshComputeVulkanDescriptorSet1Layout.AddBinding(4,
                                                     VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                     1,
                                                     TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                     []);
  end;

  fMeshComputeVulkanDescriptorSet1Layout.Initialize;
  fVulkanDevice.DebugUtils.SetObjectName(fMeshComputeVulkanDescriptorSet1Layout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3D.fMeshComputeVulkanDescriptorSet1Layout');

  //////

  fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice,TVkDescriptorSetLayoutCreateFlags(VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT),true);
  fGlobalVulkanDescriptorSetLayout.AddBinding(0,
                                              VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
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
  fVulkanDevice.DebugUtils.SetObjectName(fGlobalVulkanDescriptorSetLayout.Handle,VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,'TpvScene3D.fGlobalVulkanDescriptorSetLayout');

 end;

 fLightAABBTree:=TpvBVHDynamicAABBTree.Create;

 fLightAABBTreeGeneration:=0;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightAABBTreeStates[Index].TreeNodes:=nil;
  fLightAABBTreeStates[Index].Root:=-1;
  fLightAABBTreeStates[Index].Generation:=High(TpvUInt64);
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightAABBTreeStateGenerations[Index]:=fLightAABBTreeGeneration-1;
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightBuffers[Index]:=TpvScene3D.TLightBuffer.Create(self,Index);
 end;

 fAABBTree:=TpvBVHDynamicAABBTree.Create;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
  fAABBTreeStates[Index].Root:=-1;
  fAABBTreeStates[Index].Generation:=High(TpvUInt64);
 end;

 fOnNodeFilter:=nil;

end;

destructor TpvScene3D.Destroy;
var Index,InFlightFrameIndex,RenderPassIndex:TpvSizeInt;
    MaterialAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TPrimitiveTopology;
    FaceCullingMode:TFaceCullingMode;
    CurrentObject:TObject;
begin

 for Index:=0 to fFreeQueue.Count-1 do begin
  FreeAndNil(fFreeQueue.ItemArray[Index].Data);
 end;
 FreeAndNil(fFreeQueue);

 FreeAndNil(fFreeQueueLock);

 if assigned(ResourceManager) then begin
  ResourceManager.DestroyDelayedFreeingObjectsWithParent(self);
 end;

 Unload;

 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);

 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanInstanceMatrixBuffers[Index]);
 end;

{for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanViewUniformStagingBuffers[Index]);
 end;}

 for Index:=0 to fCountInFlightFrames-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
  fAABBTreeStates[Index].Root:=-1;
  fAABBTreeStates[Index].Generation:=High(TpvUInt64);
 end;

 FreeAndNil(fAABBTree);

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightAABBTreeStates[Index].TreeNodes:=nil;
  fLightAABBTreeStates[Index].Root:=-1;
  fLightAABBTreeStates[Index].Generation:=High(TpvUInt64);
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fLightBuffers[Index]);
 end;

 FreeAndNil(fLightAABBTree);

 FreeAndNil(fMeshComputeVulkanDescriptorSet0Layout);

 FreeAndNil(fMeshComputeVulkanDescriptorSet1Layout);

 FreeAndNil(fGlobalVulkanDescriptorSetLayout);

 while fGroupInstances.Count>0 do begin
  fGroupInstances[fGroupInstances.Count-1].Free;
 end;
 FreeAndNil(fGroupInstances);
 FreeAndNil(fGroupInstanceListLock);

 FreeAndNil(fNewInstances);
 FreeAndNil(fNewInstanceListLock);

 while fGroups.Count>0 do begin
  fGroups[fGroups.Count-1].Free;
 end;
 FreeAndNil(fGroups);
 FreeAndNil(fGroupListLock);

 for Index:=0 to fCountInFlightFrames-1 do begin
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

 FreeAndNil(fDefaultParticleTexture);

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

 FreeAndNil(fDefaultParticleImage);

 while fImages.Count>0 do begin
  fImages[fImages.Count-1].Free;
 end;
 FreeAndNil(fImages);
 FreeAndNil(fImageHashMap);
 FreeAndNil(fImageIDHashMap);
 FreeAndNil(fImageIDManager);
 FreeAndNil(fImageListLock);

 FreeAndNil(fCullObjectIDManager);

 FreeAndNil(fCullObjectIDLock);

 FreeAndNil(fTechniques);

 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fDebugPrimitiveVertexDynamicArrays[Index]);
 end;

 FreeAndNil(fImageDescriptorGenerationLock);

 FreeAndNil(fMaterialDataGenerationLock);

 FreeAndNil(fPotentiallyVisibleSet);

 fObjectListLock.Acquire;
 try
  Index:=0;
  while fObjectList.Count>0 do begin
   CurrentObject:=fObjectList.Extract(0);
   FreeAndNil(CurrentObject);
  end;
 finally
  fObjectListLock.Release;
 end;

 FreeAndNil(fObjectListLock);

 FreeAndNil(fLock);

 FreeAndNil(fLoadLock);

 fVulkanDynamicVertexBufferData.Finalize;
 fVulkanStaticVertexBufferData.Finalize;
 fVulkanDrawIndexBufferData.Finalize;
 fVulkanDrawUniqueIndexBufferData.Finalize;
 fVulkanMorphTargetVertexBufferData.Finalize;
 fVulkanJointBlockBufferData.Finalize;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fGlobalVulkanInstanceMatrixDynamicArrays[Index].Finalize;
 end;

 fVkMultiDrawIndexedInfoEXTDynamicArray.Finalize;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fVulkanNodeMatricesBufferData[Index].Finalize;
  fVulkanMorphTargetVertexWeightsBufferData[Index].Finalize;
 end;

 fCachedVertexRanges.Finalize;

 FreeAndNil(fVulkanVertexBufferRangeAllocator);

 FreeAndNil(fVulkanDrawIndexBufferRangeAllocator);

 FreeAndNil(fVulkanDrawUniqueIndexBufferRangeAllocator);

 FreeAndNil(fVulkanMorphTargetVertexBufferRangeAllocator);

 FreeAndNil(fVulkanJointBlockBufferRangeAllocator);

 FreeAndNil(fVulkanNodeMatricesBufferRangeAllocator);

 FreeAndNil(fVulkanMorphTargetVertexWeightsBufferRangeAllocator);

 FreeAndNil(fVulkanShortTermDynamicBuffers);

 FreeAndNil(fVulkanLongTermStaticBuffers);

 FreeAndNil(fBufferRangeAllocatorLock);

 FreeAndNil(fRendererInstanceIDManager);

 inherited Destroy;
end;

class function TpvScene3D.EncodeModeFlags(const aAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
                                          const aPrimitiveTopology:TpvScene3D.TPrimitiveTopology;
                                          const aFaceCullingMode:TpvScene3D.TFaceCullingMode):TpvUInt32;
begin
 result:=((TpvUInt32(aAlphaMode) and 3) shl 0) or
         ((TpvUInt32(aPrimitiveTopology) and 3) shl 2) or
         ((TpvUInt32(aFaceCullingMode) and 3) shl 4);
end;

class procedure TpvScene3D.DecodeModeFlags(const aFlags:TpvUInt32;
                                           out aAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
                                           out aPrimitiveTopology:TpvScene3D.TPrimitiveTopology;
                                           out aFaceCullingMode:TpvScene3D.TFaceCullingMode);
begin
 aAlphaMode:=TpvScene3D.TMaterial.TAlphaMode(TpvUInt32((aFlags shr 0) and 3));
 aPrimitiveTopology:=TpvScene3D.TPrimitiveTopology(TpvUInt32((aFlags shr 2) and 3));
 aFaceCullingMode:=TpvScene3D.TFaceCullingMode(TpvUInt32((aFlags shr 4) and 3));
end;

procedure TpvScene3D.ProcessFreeQueue;
var Index:TpvSizeInt;
    Item:PFreeQueueItem;
begin
 fFreeQueueLock.Acquire;
 try
  Index:=0;
  while Index<fFreeQueue.Count do begin
   Item:=@fFreeQueue.ItemArray[Index];
   if Item^.Counter>0 then begin
    dec(Item^.Counter);
    inc(Index);
   end else begin
    FreeAndNil(Item^.Data);
    fFreeQueue.Delete(Index);
   end;
  end;
 finally
  fFreeQueueLock.Release;
 end;
end;

procedure TpvScene3D.AddToFreeQueue(const aObject:TObject;const aFrameDelay:TpvInt32);
var Item:PFreeQueueItem;
begin
 if assigned(aObject) then begin
  fFreeQueueLock.Acquire;
  try
   Item:=fFreeQueue.AddNew;
   if aFrameDelay<0 then begin
    Item^.Counter:=fCountInFlightFrames;
   end else begin
    Item^.Counter:=aFrameDelay;
   end;
   Item^.Data:=aObject;
  finally
   fFreeQueueLock.Release;
  end;
 end;
end;

procedure TpvScene3D.NewImageDescriptorGeneration;
var Index:TpvSizeInt;
begin
 if assigned(fImageDescriptorGenerationLock) then begin
  fImageDescriptorGenerationLock.Acquire;
  try
   inc(fImageDescriptorGeneration);
   if fImageDescriptorGeneration=0 then begin
    fImageDescriptorGeneration:=1;
    fImageDescriptorProcessedGeneration:=0;
    for Index:=0 to fCountInFlightFrames-1 do begin
     fImageDescriptorProcessedGenerations[Index]:=High(TpvUInt64)-1;
    end;
   end;
  finally
   fImageDescriptorGenerationLock.Release;
  end;
 end;
end;

procedure TpvScene3D.NewMaterialDataGeneration;
var Index:TpvSizeInt;
begin
 if assigned(fMaterialDataGenerationLock) then begin
  fMaterialDataGenerationLock.Acquire;
  try
   inc(fMaterialDataGeneration);
   if fMaterialDataGeneration=0 then begin
    fMaterialDataGeneration:=1;
    for Index:=0 to fCountInFlightFrames-1 do begin
     fMaterialDataProcessedGenerations[Index]:=0;
     fMaterialDataUpdatedGenerations[Index]:=0;
    end;
    FillChar(fInFlightFrameMaterialBufferDataGenerations,SizeOf(TInFlightFrameMaterialBufferDataGenerations),#$ff);
   end;
  finally
   fMaterialDataGenerationLock.Release;
  end;
 end;
end;

procedure TpvScene3D.Upload;
var Group:TGroup;
    Index,
    InFlightFrameIndex,
    MaterialBufferDataSize:TpvSizeInt;
    MaxMaterialID:TpvInt32;
    ViewUniformBuffer:TpvVulkanBuffer;
    Material:TMaterial;
    Texture:TTexture;
    UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
    DeviceAddress:TVkDeviceAddress;
begin
 if not fInUpload then begin
  fInUpload:=true;
  try
   if not fUploaded then begin
    fLock.Acquire;
    try
     if not fUploaded then begin
      try

       if assigned(fVulkanDevice) then begin

        fDefaultSampler.Upload;

        fWhiteTexture.Upload;

        fDefaultParticleTexture.Upload;

        fVulkanStagingQueue:=fVulkanDevice.UniversalQueue;

        fVulkanStagingCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                                               fVulkanDevice.UniversalQueueFamilyIndex,
                                                               TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

        fVulkanStagingCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanStagingCommandPool,
                                                                   VK_COMMAND_BUFFER_LEVEL_PRIMARY);

        fVulkanStagingFence:=TpvVulkanFence.Create(fVulkanDevice);

        case fBufferStreamingMode of

         TBufferStreamingMode.Direct:begin

 {        for Index:=0 to fCountInFlightFrames-1 do begin

           fVulkanLightItemsStagingBuffers[Index]:=nil;

           fVulkanLightTreeStagingBuffers[Index]:=nil;

           fVulkanLightMetaInfoStagingBuffers[Index]:=nil;

           fGlobalVulkanViewUniformStagingBuffers[Index]:=nil;

          end;}

          for Index:=0 to fCountInFlightFrames-1 do begin
           fGlobalVulkanInstanceMatrixBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                             Max(1,length(fGlobalVulkanInstanceMatrixDynamicArrays[Index].Items))*SizeOf(TpvMatrix4x4),
                                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                             [],
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             [TpvVulkanBufferFlag.PersistentMapped]);
          end;

          for Index:=0 to fCountInFlightFrames-1 do begin
           fVulkanDebugPrimitiveVertexBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                             SizeOf(TpvScene3D.TDebugPrimitiveVertex)*MaxDebugPrimitiveVertices,
                                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or fAccelerationStructureInputBufferUsageFlags,
                                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                             [],
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             [TpvVulkanBufferFlag.PersistentMapped]);
          end;

          for Index:=0 to fCountInFlightFrames-1 do begin
           fVulkanParticleVertexBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                       SizeOf(TpvScene3D.TParticleVertex)*MaxParticleVertices,
                                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or fAccelerationStructureInputBufferUsageFlags,
                                                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                       [],
                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                       0,
                                                                       0,
                                                                       0,
                                                                       0,
                                                                       0,
                                                                       0,
                                                                       [TpvVulkanBufferFlag.PersistentMapped]);
          end;

         end;

         TBufferStreamingMode.Staging:begin

 {        for Index:=0 to fCountInFlightFrames-1 do begin

           fVulkanLightItemsStagingBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                          MaxVisibleLights*SizeOf(TpvScene3D.TLightItem),
                                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                                          TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                          [],
                                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                          0,
                                                                          0,
                                                                          0,
                                                                          0,
                                                                          0,
                                                                          0,
                                                                          [TpvVulkanBufferFlag.PersistentMapped]
                                                                         );

           fVulkanLightTreeStagingBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                         (MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode),
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                         [],
                                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         [TpvVulkanBufferFlag.PersistentMapped]
                                                                        );

           fVulkanLightMetaInfoStagingBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                             MaxVisibleLights*SizeOf(TpvScene3D.TLightMetaInfo),
                                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                             [],
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             [TpvVulkanBufferFlag.PersistentMapped]
                                                                            );

           fGlobalVulkanViewUniformStagingBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                                 SizeOf(TViewUniformBuffer),
                                                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT),
                                                                                 TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                 [],
                                                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                                 TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 0,
                                                                                 [TpvVulkanBufferFlag.PersistentMapped]);

          end;}

          for Index:=0 to fCountInFlightFrames-1 do begin
           fGlobalVulkanInstanceMatrixBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                             Max(1,length(fGlobalVulkanInstanceMatrixDynamicArrays[Index].Items))*SizeOf(TpvMatrix4x4),
                                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                             [],
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                             0,
                                                                             0,
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             []);
          end;

          for Index:=0 to fCountInFlightFrames-1 do begin
           fVulkanDebugPrimitiveVertexBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                             SizeOf(TpvScene3D.TDebugPrimitiveVertex)*MaxDebugPrimitiveVertices,
                                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or fAccelerationStructureInputBufferUsageFlags,
                                                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                             [],
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                             0,
                                                                             0,
                                                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             0,
                                                                             []);
          end;

          for Index:=0 to fCountInFlightFrames-1 do begin
           fVulkanParticleVertexBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                       SizeOf(TpvScene3D.TParticleVertex)*MaxParticleVertices,
                                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or fAccelerationStructureInputBufferUsageFlags,
                                                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                       [],
                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                       0,
                                                                       0,
                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                       0,
                                                                       0,
                                                                       0,
                                                                       0,
                                                                       []);
          end;

         end;

         else begin
          Assert(false);
         end;

        end;

        for Index:=0 to fCountInFlightFrames-1 do begin
         fLightBuffers[Index].Upload;
        end;

        fGlobalVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(fVulkanDevice,TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT) or TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT),length(fImageInfos)*length(fGlobalVulkanDescriptorSets));
        fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,length(fGlobalVulkanDescriptorSets)*3);
        fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,length(fGlobalVulkanDescriptorSets)*5);
        fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,length(fGlobalVulkanDescriptorSets)*length(fImageInfos));
        fGlobalVulkanDescriptorPool.Initialize;

        for Group in fGroups do begin
         if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
          Group.Upload;
         end;
        end;

        for Index:=0 to High(TMaterialBufferData) do begin
         fInFlightFrameMaterialBufferData[0,Index]:=TMaterial.DefaultShaderData;
        end;

        FillChar(fInFlightFrameMaterialBufferDataGenerations,SizeOf(TInFlightFrameMaterialBufferDataGenerations),#$ff);

        MaxMaterialID:=0;

        fMaterialDataGeneration:=1;

        for Index:=0 to fMaterials.Count-1 do begin
         Material:=fMaterials[Index];
         if (Material.ID>0) and (Material.ID<=High(TMaterialBufferData)) then begin
          fInFlightFrameMaterialBufferData[0,Material.ID]:=Material.fShaderData;
          fInFlightFrameMaterialBufferDataGenerations[0,Material.ID]:=Material.fGeneration;
          if MaxMaterialID<Material.ID then begin
           MaxMaterialID:=Material.ID;
          end;
         end;
        end;

        for Index:=1 to fCountInFlightFrames-1 do begin
         fInFlightFrameMaterialBufferData[Index]:=fInFlightFrameMaterialBufferData[0];
         fInFlightFrameMaterialBufferDataGenerations[Index]:=fInFlightFrameMaterialBufferDataGenerations[0];
        end;

   //   MaterialBufferDataSize:=Min(RoundUpToPowerOfTwo((MaxMaterialID+1)*SizeOf(TMaterial.TShaderData)),SizeOf(TMaterialBufferData));

        MaterialBufferDataSize:=SizeOf(TMaterialBufferData);

        UniversalQueue:=fVulkanDevice.UniversalQueue;
        try
         UniversalCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                                           fVulkanDevice.UniversalQueueFamilyIndex,
                                                           TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
         try
          UniversalCommandBuffer:=TpvVulkanCommandBuffer.Create(UniversalCommandPool,
                                                                VK_COMMAND_BUFFER_LEVEL_PRIMARY);
          try
           UniversalFence:=TpvVulkanFence.Create(fVulkanDevice);
           try

            case fBufferStreamingMode of

             TBufferStreamingMode.Direct:begin

 {            for Index:=0 to fCountInFlightFrames-1 do begin
               fVulkanMaterialDataStagingBuffers[Index]:=nil;
              end;}

             end;

             TBufferStreamingMode.Staging:begin

 {            for Index:=0 to fCountInFlightFrames-1 do begin

               fVulkanMaterialDataStagingBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                                MaterialBufferDataSize,
                                                                                TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                                                                TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                                                TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                [],
                                                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                                TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                0,
                                                                                0,
                                                                                0,
                                                                                0,
                                                                                0,
                                                                                0,
                                                                                []);

                fVulkanMemoryStaging.Upload(UniversalQueue,
                                            UniversalCommandBuffer,
                                            UniversalFence,
                                            fMaterialBufferData,
                                            fVulkanMaterialDataStagingBuffers[Index],
                                            0,
                                            MaterialBufferDataSize);

 (*            fVulkanMaterialDataStagingBuffers[Index].UploadData(UniversalQueue,
                                                                   UniversalCommandBuffer,
                                                                   UniversalFence,
                                                                   fMaterialBufferData,
                                                                   0,
                                                                   MaterialBufferDataSize,
                                                                   TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);//*)

              end; }

             end;

             else begin
              Assert(false);
             end;

            end;

            for Index:=0 to fCountInFlightFrames-1 do begin

             case fBufferStreamingMode of

              TBufferStreamingMode.Direct:begin

               fVulkanMaterialDataBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                         MaterialBufferDataSize,
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),
                                                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                         [],
                                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         [TpvVulkanBufferFlag.PersistentMapped]);

                fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                   UniversalCommandBuffer,
                                                   UniversalFence,
                                                   fInFlightFrameMaterialBufferData[Index],
                                                   fVulkanMaterialDataBuffers[Index],
                                                   0,
                                                   MaterialBufferDataSize);

 {             fVulkanMaterialDataBuffers[Index].UploadData(UniversalQueue,
                                                            UniversalCommandBuffer,
                                                            UniversalFence,
                                                            fMaterialBufferData,
                                                            0,
                                                            MaterialBufferDataSize,
                                                            TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);}

              end;

              TBufferStreamingMode.Staging:begin

               fVulkanMaterialDataBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                         MaterialBufferDataSize,
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR),
                                                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                         [],
                                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                         0,
                                                                         0,
                                                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         0,
                                                                         []);

                fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                   UniversalCommandBuffer,
                                                   UniversalFence,
                                                   fInFlightFrameMaterialBufferData[Index],
                                                   fVulkanMaterialDataBuffers[Index],
                                                   0,
                                                   MaterialBufferDataSize);

              end;

              else begin
               Assert(false);
              end;

             end;

             if fUseBufferDeviceAddress then begin

              DeviceAddress:=fVulkanMaterialDataBuffers[Index].DeviceAddress;

              fVulkanMaterialUniformBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                           SizeOf(TVkDeviceAddress),
                                                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                           [],
                                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                           0,
                                                                           0,
                                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                           0,
                                                                           0,
                                                                           0,
                                                                           0,
                                                                           []);
              fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                 UniversalCommandBuffer,
                                                 UniversalFence,
                                                 DeviceAddress,
                                                 fVulkanMaterialUniformBuffers[Index],
                                                 0,
                                                 SizeOf(TVkDeviceAddress));
 {            fVulkanMaterialUniformBuffers[Index].UploadData(UniversalQueue,
                                                              UniversalCommandBuffer,
                                                              UniversalFence,
                                                              DeviceAddress,
                                                              0,
                                                              SizeOf(TVkDeviceAddress),
                                                              TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);}

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
         if Texture.fUploaded and (Texture.fReferenceCounter>0) and (Texture.ID>0) and (((Texture.ID*2)+1)<length(fImageInfos)) then begin
          fImageInfos[(Texture.ID*2)+0]:=Texture.GetDescriptorImageInfo(false);
          fImageInfos[(Texture.ID*2)+1]:=Texture.GetDescriptorImageInfo(true);
         end;
        end;

        for Index:=0 to fCountInFlightFrames-1 do begin
         fInFlightFrameImageInfos[Index]:=fImageInfos;
         fInFlightFrameImageInfoImageDescriptorGenerations[Index]:=High(TpvUInt64)-2;
         fInFlightFrameImageInfoImageDescriptorUploadedGenerations[Index]:=High(TpvUInt64)-4;
        end;

        for Index:=0 to fCountInFlightFrames-1 do begin
         fGlobalVulkanDescriptorSets[Index]:=TpvVulkanDescriptorSet.Create(fGlobalVulkanDescriptorPool,
                                                                           fGlobalVulkanDescriptorSetLayout);
         fGlobalVulkanDescriptorSets[Index].WriteToDescriptorSet(0,
                                                                 0,
                                                                 1,
                                                                 TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                 [],
                                                                 [fGlobalVulkanInstanceMatrixBuffers[Index].DescriptorBufferInfo],
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

        for Index:=0 to fCountInFlightFrames-1 do begin
         fImageDescriptorProcessedGenerations[Index]:=0;
        end;

        fImageDescriptorGeneration:=0;
        fImageDescriptorProcessedGeneration:=0;

        for Index:=0 to fCountInFlightFrames-1 do begin
         fMaterialDataProcessedGenerations[Index]:=0;
         fMaterialDataUpdatedGenerations[Index]:=0;
        end;

        fMaterialDataGeneration:=1;

       end;

      finally
       fUploaded:=true;
      end;

     end;

    finally
     fLock.Release;
    end;
   end;
  finally
   fInUpload:=false;
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

     for Index:=0 to fCountInFlightFrames-1 do begin
      FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
     end;

     FreeAndNil(fGlobalVulkanDescriptorPool);

     for Index:=0 to fCountInFlightFrames-1 do begin
      FreeAndNil(fVulkanDebugPrimitiveVertexBuffers[Index]);
      FreeAndNil(fVulkanParticleVertexBuffers[Index]);
     end;

{    for Index:=0 to fCountInFlightFrames-1 do begin
      FreeAndNil(fGlobalVulkanViewUniformStagingBuffers[Index]);
     end;}

     for Index:=0 to fCountInFlightFrames-1 do begin
      fLightBuffers[Index].Unload;
     end;

{    for Index:=0 to fCountInFlightFrames-1 do begin
      FreeAndNil(fVulkanLightItemsStagingBuffers[Index]);
      FreeAndNil(fVulkanLightTreeStagingBuffers[Index]);
      FreeAndNil(fVulkanLightMetaInfoStagingBuffers[Index]);
     end;}

     for Group in fGroups do begin
      if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
       Group.Unload;
      end;
     end;

     for Index:=0 to fCountInFlightFrames-1 do begin

      FreeAndNil(fVulkanMaterialUniformBuffers[Index]);

      FreeAndNil(fVulkanMaterialDataBuffers[Index]);

//    FreeAndNil(fVulkanMaterialDataStagingBuffers[Index]);

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

     FreeAndNil(fVulkanStagingFence);

     FreeAndNil(fVulkanStagingCommandBuffer);

     FreeAndNil(fVulkanStagingCommandPool);

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

procedure TpvScene3D.ResetFrame(const aInFlightFrameIndex:TpvSizeInt);
var GlobalVulkanInstanceMatrixDynamicArray:PGlobalVulkanInstanceMatrixDynamicArray;
begin

 GlobalVulkanInstanceMatrixDynamicArray:=@fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex];
 if GlobalVulkanInstanceMatrixDynamicArray^.Count<2 then begin
  GlobalVulkanInstanceMatrixDynamicArray^.Count:=0;
  GlobalVulkanInstanceMatrixDynamicArray^.Add(TpvMatrix4x4.Identity);
  GlobalVulkanInstanceMatrixDynamicArray^.Add(TpvMatrix4x4.Identity);
 end;
 GlobalVulkanInstanceMatrixDynamicArray^.Count:=2;

end;

procedure TpvScene3D.Check(const aInFlightFrameIndex:TpvSizeInt);
var Group:TpvScene3D.TGroup;
begin
 ProcessFreeQueue;
 for Group in fGroups do begin
  Group.Check(aInFlightFrameIndex);
 end;
end;

procedure TpvScene3D.Update(const aInFlightFrameIndex:TpvSizeInt);
var Index,MaterialBufferDataOffset,MaterialBufferDataSize:TpvSizeInt;
    MinMaterialID,MaxMaterialID:TpvInt32;
    Group:TpvScene3D.TGroup;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    LightAABBTreeState,AABBTreeState:TpvBVHDynamicAABBTree.PState;
    First:boolean;
    OldGeneration,NewGeneration:TpvUInt64;
    LightBuffer:TpvScene3D.TLightBuffer;
    Texture:TpvScene3D.TTexture;
    Material:TpvScene3D.TMaterial;
begin

 fCountLights[aInFlightFrameIndex]:=0;

 for Group in fGroups do begin
  Group.Update(aInFlightFrameIndex);
 end;

 AABBTreeState:=@fAABBTreeStates[aInFlightFrameIndex];
 fAABBTree.UpdateGeneration;
 if AABBTreeState^.Generation<>fAABBTree.Generation then begin
  AABBTreeState^.Generation:=fAABBTree.Generation;
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
 if First or IsZero(fBoundingBox.Radius) then begin
  fBoundingBox.Min:=TpvVector3.InlineableCreate(-1.0,-1.0,-1.0);
  fBoundingBox.Max:=TpvVector3.InlineableCreate(1.0,1.0,-1.0);
 end;

 fInFlightFrameBoundingBoxes[aInFlightFrameIndex]:=fBoundingBox;

end;

procedure TpvScene3D.CullLights(const aInFlightFrameIndex:TpvSizeInt;
                                const aFrustums:TpvFrustumDynamicArray;
                                const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                const aRoot:TpvSizeInt);
type TStackItem=record
      Node:TpvSizeInt;
      Mask:TpvUInt32;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicFastStack<TStackItem>;
var Index:TpvSizeInt;
    StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Mask:TpvUInt32;
    Stack:TStack;
    PotentiallyVisible:boolean;
begin
 if (aRoot>=0) and (length(aTreeNodes)>0) then begin
  Stack.Initialize;
  try
   StackItem:=Stack.PushIndirect;
   StackItem^.Node:=aRoot;
   StackItem^.Mask:=$ffffffff;
   while Stack.PopIndirect(StackItem) do begin
    Node:=StackItem^.Node;
    Mask:=StackItem^.Mask;
    while Node>=0 do begin
     TreeNode:=@aTreeNodes[Node];
     if length(aFrustums)>0 then begin
      if length(aFrustums)=1 then begin
       PotentiallyVisible:=not ((((Mask and $80000000)<>0) and (aFrustums[0].AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)));
      end else begin
       PotentiallyVisible:=false;
       for Index:=0 to length(aFrustums)-1 do begin
        if aFrustums[Index].AABBInFrustum(TreeNode^.AABB)<>TpvFrustum.COMPLETE_OUT then begin
         PotentiallyVisible:=true;
         break;
        end;
       end;
      end;
     end else begin
      PotentiallyVisible:=true;
     end;
     if PotentiallyVisible then begin
      if TreeNode^.UserData<>0 then begin
       if fCountIndirectLights[aInFlightFrameIndex]<MaxVisibleLights then begin
        fIndirectLights[aInFlightFrameIndex,fCountIndirectLights[aInFlightFrameIndex]]:=TpvScene3D.TLight(Pointer(TreeNode^.UserData));
        inc(fCountIndirectLights[aInFlightFrameIndex]);
       end;
      end;
      if TreeNode^.Children[0]>=0 then begin
       if TreeNode^.Children[1]>=0 then begin
        StackItem:=Stack.PushIndirect;
        StackItem^.Node:=TreeNode^.Children[1];
        StackItem^.Mask:=Mask;
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
  finally
   Stack.Finalize;
  end;
 end;
end;

procedure TpvScene3D.CollectLights(const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                   const aRoot:TpvSizeInt;
                                   var aLightItemArray:TpvScene3D.TLightItems;
                                   var aLightMetaInfoArray:TpvScene3D.TLightMetaInfos);
type TStackItem=record
      Node:TpvSizeInt;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicFastStack<TStackItem>;
var StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Light:TpvScene3D.TLight;
    LightItem:TpvScene3D.PLightItem;
    LightMetaInfo:TpvScene3D.PLightMetaInfo;
    InnerConeAngleCosinus,OuterConeAngleCosinus:TpvScalar;
    Stack:TStack;
begin
 aLightItemArray.Count:=0;
 if (aRoot>=0) and (length(aTreeNodes)>0) then begin
  Stack.Initialize;
  try
   StackItem:=Stack.PushIndirect;
   StackItem^.Node:=aRoot;
   while Stack.PopIndirect(StackItem) do begin
    Node:=StackItem^.Node;
    while Node>=0 do begin
     TreeNode:=@aTreeNodes[Node];
     if TreeNode^.UserData<>0 then begin
      Light:=TpvScene3D.TLight(Pointer(TreeNode^.UserData));
      if aLightItemArray.Count<MaxVisibleLights then begin
       Light.fLightItemIndex:=aLightItemArray.AddNew;
       LightItem:=@aLightItemArray.Items[Light.fLightItemIndex];
       LightItem^.Type_:=TpvUInt32(Light.fDataPointer^.Type_);
       LightItem^.ShadowMapIndex:=0;
       InnerConeAngleCosinus:=cos(Light.fDataPointer^.InnerConeAngle);
       OuterConeAngleCosinus:=cos(Light.fDataPointer^.OuterConeAngle);
      {LightItem^.InnerConeCosinus:=InnerConeAngleCosinus;
       LightItem^.OuterConeCosinus:=OuterConeAngleCosinus;}
       LightItem^.LightAngleScale:=1.0/Max(1e-5,InnerConeAngleCosinus-OuterConeAngleCosinus);
       LightItem^.LightAngleOffset:=-(OuterConeAngleCosinus*LightItem^.LightAngleScale);
       LightItem^.ColorIntensity:=TpvVector4.InlineableCreate(Light.fDataPointer^.fColor,Light.fDataPointer^.fIntensity);
       LightItem^.PositionRange:=TpvVector4.InlineableCreate(Light.fPosition,Light.fDataPointer^.fRange);
       LightItem^.DirectionZFar:=TpvVector4.InlineableCreate(Light.fDirection,0.0);
       LightItem^.ShadowMapMatrix:=TpvMatrix4x4.Identity;
       LightMetaInfo:=@aLightMetaInfoArray[Light.fLightItemIndex];
       LightMetaInfo^.MinBounds:=TpvVector4.Create(Light.fBoundingBox.Min,TpvUInt32(Light.fDataPointer^.Type_));
       LightMetaInfo^.MaxBounds:=TpvVector4.Create(Light.fBoundingBox.Max,Light.fBoundingBox.Radius);
      end else begin
       Light.fLightItemIndex:=-1;
      end;
     end;
     if TreeNode^.Children[0]>=0 then begin
      if TreeNode^.Children[1]>=0 then begin
       StackItem:=Stack.PushIndirect;
       StackItem^.Node:=TreeNode^.Children[1];
      end;
      Node:=TreeNode^.Children[0];
      continue;
     end else begin
      if TreeNode^.Children[1]>=0 then begin
       Node:=TreeNode^.Children[1];
       continue;
      end;
     end;
     break;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

procedure TpvScene3D.PrepareFrame(const aInFlightFrameIndex:TpvSizeInt);
var Index,ItemID:TpvSizeInt;
    OldGeneration,NewGeneration:TpvUInt64;
    DirtyBits:TPasMPUInt32;
    LightBuffer:TpvScene3D.TLightBuffer;
    LightAABBTreeState:TpvBVHDynamicAABBTree.PState;
    Group:TpvScene3D.TGroup;
    Texture:TpvScene3D.TTexture;
    Material:TpvScene3D.TMaterial;
    MaterialIDDirtyMap:TpvScene3D.PMaterialIDDirtyMap;
begin

 if assigned(fVulkanDevice) then begin

  for Group in fGroups do begin
   Group.PrepareFrame(aInFlightFrameIndex);
  end;

  if (fImageDescriptorProcessedGeneration<>fImageDescriptorGeneration) or
     (fImageDescriptorProcessedGenerations[aInFlightFrameIndex]<>fImageDescriptorProcessedGeneration) then begin

   fImageDescriptorGenerationLock.Acquire;
   try

    if fImageDescriptorProcessedGeneration<>fImageDescriptorGeneration then begin
     fImageDescriptorProcessedGeneration:=fImageDescriptorGeneration;
     fImageInfos[0]:=fWhiteTexture.GetDescriptorImageInfo(false);
     for Index:=1 to length(fImageInfos)-1 do begin
      fImageInfos[Index]:=fImageInfos[0];
     end;
     for Index:=0 to fTextures.Count-1 do begin
      Texture:=fTextures[Index];
      if Texture.fUploaded and (Texture.fReferenceCounter>0) and (Texture.ID>0) and (((Texture.ID*2)+1)<length(fImageInfos)) then begin
       fImageInfos[(Texture.ID*2)+0]:=Texture.GetDescriptorImageInfo(false);
       fImageInfos[(Texture.ID*2)+1]:=Texture.GetDescriptorImageInfo(true);
      end;
     end;
    end;

    if fImageDescriptorProcessedGenerations[aInFlightFrameIndex]<>fImageDescriptorProcessedGeneration then begin
     fImageDescriptorProcessedGenerations[aInFlightFrameIndex]:=fImageDescriptorProcessedGeneration;
     fInFlightFrameImageInfos[aInFlightFrameIndex]:=fImageInfos;
     inc(fInFlightFrameImageInfoImageDescriptorGenerations[aInFlightFrameIndex]);
    end;

   finally
    fImageDescriptorGenerationLock.Release;
   end;

  end;

  if fMaterialDataProcessedGenerations[aInFlightFrameIndex]<>fMaterialDataGeneration then begin

   fMaterialDataGenerationLock.Acquire;
   try

    if fMaterialDataProcessedGenerations[aInFlightFrameIndex]<>fMaterialDataGeneration then begin

     fMaterialDataProcessedGenerations[aInFlightFrameIndex]:=fMaterialDataGeneration;

     MaterialIDDirtyMap:=@fMaterialIDDirtyMaps[aInFlightFrameIndex];
     for Index:=0 to Min((fMaxMaterialID+32) shr 5,High(TMaterialIDDirtyMap)) do begin
      DirtyBits:=TPasMPInterlocked.Exchange(MaterialIDDirtyMap^[Index],0);
      if DirtyBits<>0 then begin
       TPasMPInterlocked.BitwiseOr(fMaterialIDToUpdateDirtyMaps[aInFlightFrameIndex,Index],DirtyBits);
      end;
      while DirtyBits<>0 do begin
       ItemID:=(Index shl 5) or TPasMPMath.FindFirstSetBit32(DirtyBits);
       if (ItemID>=0) and (ItemID<length(fMaterialIDMap)) then begin
        Material:=fMaterialIDMap[ItemID];
        if assigned(Material) then begin
         fInFlightFrameMaterialBufferData[aInFlightFrameIndex,ItemID]:=Material.fShaderData;
         fInFlightFrameMaterialBufferDataGenerations[aInFlightFrameIndex,ItemID]:=Material.fGeneration;
        end else begin
         fInFlightFrameMaterialBufferData[aInFlightFrameIndex,ItemID]:=TMaterial.DefaultShaderData;
         fInFlightFrameMaterialBufferDataGenerations[aInFlightFrameIndex,ItemID]:=High(TpvUInt64);
        end;
       end;
       DirtyBits:=DirtyBits and (DirtyBits-1);
      end;
     end;

    end;

   finally
    fMaterialDataGenerationLock.Release;
   end;

  end;

  OldGeneration:=fLightAABBTreeStateGenerations[aInFlightFrameIndex];
  NewGeneration:=fLightAABBTreeGeneration;
  if (OldGeneration<>NewGeneration) and
     (TPasMPInterlocked.CompareExchange(fLightAABBTreeStateGenerations[aInFlightFrameIndex],NewGeneration,OldGeneration)=OldGeneration) then begin

   LightAABBTreeState:=@fLightAABBTreeStates[aInFlightFrameIndex];
   fLightAABBTree.UpdateGeneration;
   if LightAABBTreeState^.Generation<>fLightAABBTree.Generation then begin
    LightAABBTreeState^.Generation:=fLightAABBTree.Generation;
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
   end;

   LightBuffer:=fLightBuffers[aInFlightFrameIndex];
   CollectLights(LightAABBTreeState^.TreeNodes,LightAABBTreeState^.Root,LightBuffer.fLightItems,LightBuffer.fLightMetaInfos);
   fLightAABBTree.GetSkipListNodes(LightBuffer.fLightTree,GetLightUserDataIndex);
   LightBuffer.fNewLightAABBTreeGeneration:=fLightAABBTreeGeneration;

  end;

 end;

end;

procedure TpvScene3D.BeginFrame(const aInFlightFrameIndex:TpvSizeInt);
begin
end;

procedure TpvScene3D.EndFrame(const aInFlightFrameIndex:TpvSizeInt);
begin
end;

procedure TpvScene3D.UploadFrame(const aInFlightFrameIndex:TpvSizeInt);
var Index,ItemID,RenderPassIndex:TpvSizeInt;
    Size:TVkDeviceSize;
    Group:TpvScene3D.TGroup;
    MaterialIDDirtyMap:PMaterialIDDirtyMap;
    DirtyBits,MinMaterialID,MaxMaterialID:TpvUInt32;
    MaterialBufferDataOffset,MaterialBufferDataSize:TpvSizeUInt;
    InFlightFrameMaterialBufferVulkanGPUData:PMaterialBufferData;
    VulkanMaterialDataBuffer:TpvVulkanBuffer;
begin

 if assigned(fVulkanDevice) then begin

  for Group in fGroups do begin
   Group.UploadFrame(aInFlightFrameIndex);
  end;

  begin
   fVulkanLongTermStaticBuffers.Update;
   fVulkanShortTermDynamicBuffers.Update(aInFlightFrameIndex);
  end;

  if fInFlightFrameImageInfoImageDescriptorUploadedGenerations[aInFlightFrameIndex]<>fInFlightFrameImageInfoImageDescriptorGenerations[aInFlightFrameIndex] then begin
   fImageDescriptorGenerationLock.Acquire;
   try
    if fInFlightFrameImageInfoImageDescriptorUploadedGenerations[aInFlightFrameIndex]<>fInFlightFrameImageInfoImageDescriptorGenerations[aInFlightFrameIndex] then begin
     fInFlightFrameImageInfoImageDescriptorUploadedGenerations[aInFlightFrameIndex]:=fInFlightFrameImageInfoImageDescriptorGenerations[aInFlightFrameIndex];
     fGlobalVulkanDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(4,
                                                                           0,
                                                                           length(fInFlightFrameImageInfos[aInFlightFrameIndex]),
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                           fInFlightFrameImageInfos[aInFlightFrameIndex][0],
                                                                           [],
                                                                           [],
                                                                           true);
    end;
   finally
    fImageDescriptorGenerationLock.Release;
   end;
  end;

  if fMaterialDataUpdatedGenerations[aInFlightFrameIndex]<>fMaterialDataProcessedGenerations[aInFlightFrameIndex] then begin

   fMaterialDataGenerationLock.Acquire;
   try

    if fMaterialDataUpdatedGenerations[aInFlightFrameIndex]<>fMaterialDataProcessedGenerations[aInFlightFrameIndex] then begin

     case fBufferStreamingMode of

      TBufferStreamingMode.Direct:begin

       if fMaterialDataUpdatedGenerations[aInFlightFrameIndex]=0 then begin

        MaterialIDDirtyMap:=@fMaterialIDToUpdateDirtyMaps[aInFlightFrameIndex];
        for Index:=0 to Min((fMaxMaterialID+32) shr 5,High(TMaterialIDDirtyMap)) do begin
         TPasMPInterlocked.Write(MaterialIDDirtyMap^[Index],0);
        end;

        fVulkanMaterialDataBuffers[aInFlightFrameIndex].UpdateData(fInFlightFrameMaterialBufferData[aInFlightFrameIndex,0],
                                                                   0,
                                                                   fVulkanMaterialDataBuffers[aInFlightFrameIndex].Size);

       end else begin

        VulkanMaterialDataBuffer:=fVulkanMaterialDataBuffers[aInFlightFrameIndex];

        if (VulkanMaterialDataBuffer.MemoryPropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0 then begin
         InFlightFrameMaterialBufferVulkanGPUData:=VulkanMaterialDataBuffer.Memory.MapMemory(0,VulkanMaterialDataBuffer.Size);
         try
          if assigned(InFlightFrameMaterialBufferVulkanGPUData) then begin
           MaterialIDDirtyMap:=@fMaterialIDToUpdateDirtyMaps[aInFlightFrameIndex];
           for Index:=0 to Min((fMaxMaterialID+32) shr 5,High(TMaterialIDDirtyMap)) do begin
            DirtyBits:=TPasMPInterlocked.Exchange(MaterialIDDirtyMap^[Index],0);
            while DirtyBits<>0 do begin
             ItemID:=(Index shl 5) or TPasMPMath.FindFirstSetBit32(DirtyBits);
             if (ItemID>=0) and (ItemID<length(fMaterialIDMap)) then begin
              InFlightFrameMaterialBufferVulkanGPUData^[ItemID]:=fInFlightFrameMaterialBufferData[aInFlightFrameIndex,ItemID];
             end;
             DirtyBits:=DirtyBits and (DirtyBits-1);
            end;
           end;
           VulkanMaterialDataBuffer.Flush(InFlightFrameMaterialBufferVulkanGPUData,0,VulkanMaterialDataBuffer.Size,false);
          end else begin
           raise EpvVulkanException.Create('Vulkan buffer memory block map failed');
          end;
         finally
          VulkanMaterialDataBuffer.Memory.UnmapMemory;
         end;
        end else begin
         raise EpvVulkanException.Create('Vulkan buffer memory block map failed');
        end;

       end;

      end;

      TBufferStreamingMode.Staging:begin

       if fMaterialDataUpdatedGenerations[aInFlightFrameIndex]=0 then begin

        MaterialIDDirtyMap:=@fMaterialIDToUpdateDirtyMaps[aInFlightFrameIndex];
        for Index:=0 to Min((fMaxMaterialID+32) shr 5,High(TMaterialIDDirtyMap)) do begin
         TPasMPInterlocked.Write(MaterialIDDirtyMap^[Index],0);
        end;

        fVulkanDevice.MemoryStaging.Upload(fVulkanStagingQueue,
                                           fVulkanStagingCommandBuffer,
                                           fVulkanStagingFence,
                                           fInFlightFrameMaterialBufferData[aInFlightFrameIndex,0],
                                           fVulkanMaterialDataBuffers[aInFlightFrameIndex],
                                           0,
                                           fVulkanMaterialDataBuffers[aInFlightFrameIndex].Size);

       end else begin

        MaterialIDDirtyMap:=@fMaterialIDToUpdateDirtyMaps[aInFlightFrameIndex];
        MinMaterialID:=$ffff;
        MaxMaterialID:=$0000;
        for Index:=0 to Min((fMaxMaterialID+32) shr 5,High(TMaterialIDDirtyMap)) do begin
         DirtyBits:=TPasMPInterlocked.Exchange(MaterialIDDirtyMap^[Index],0);
         while DirtyBits<>0 do begin
          ItemID:=(Index shl 5) or TPasMPMath.FindFirstSetBit32(DirtyBits);
          if (ItemID>=0) and (ItemID<length(fMaterialIDMap)) then begin
           if MinMaterialID>ItemID then begin
            MinMaterialID:=ItemID;
           end;
           if MaxMaterialID<ItemID then begin
            MaxMaterialID:=ItemID;
           end;
          end;
          DirtyBits:=DirtyBits and (DirtyBits-1);
         end;
        end;

        MaterialBufferDataOffset:=SizeOf(TMaterial.TShaderData)*MinMaterialID;
        MaterialBufferDataSize:=SizeOf(TMaterial.TShaderData)*(MaxMaterialID+1);

        if MaterialBufferDataOffset<MaterialBufferDataSize then begin

         fVulkanDevice.MemoryStaging.Upload(fVulkanStagingQueue,
                                            fVulkanStagingCommandBuffer,
                                            fVulkanStagingFence,
                                            fInFlightFrameMaterialBufferData[aInFlightFrameIndex,MinMaterialID],
                                            fVulkanMaterialDataBuffers[aInFlightFrameIndex],
                                            MaterialBufferDataOffset,
                                            MaterialBufferDataSize-MaterialBufferDataOffset);

        end;

       end;

      end;

      else begin
       Assert(false);
      end;

     end;

     fMaterialDataUpdatedGenerations[aInFlightFrameIndex]:=fMaterialDataProcessedGenerations[aInFlightFrameIndex];

    end;

   finally
    fMaterialDataGenerationLock.Release;
   end;

  end;

  LightBuffers[aInFlightFrameIndex].UploadFrame;

  if fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex].Count>0 then begin

   if assigned(fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex]) then begin

    Size:=Max(1,length(fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex].Items))*SizeOf(TpvMatrix4x4);
    if fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex].Size<Size then begin

     FreeMem(fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex]);

     case fBufferStreamingMode of

      TBufferStreamingMode.Direct:begin
       fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                                       Size,
                                                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                       [],
                                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                                                       0,
                                                                                       0,
                                                                                       0,
                                                                                       0,
                                                                                       0,
                                                                                       0,
                                                                                       [TpvVulkanBufferFlag.PersistentMapped]);
      end;

      TBufferStreamingMode.Staging:begin
       fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                                       Size,
                                                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                                                                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                                                       [],
                                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                       0,
                                                                                       0,
                                                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                                                                                       0,
                                                                                       0,
                                                                                       0,
                                                                                       0,
                                                                                       []);
      end;

      else begin
       Assert(false);
      end;

     end;

     fGlobalVulkanDescriptorSets[aInFlightFrameIndex].WriteToDescriptorSet(0,
                                                                           0,
                                                                           1,
                                                                           TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                                                           [],
                                                                           [fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex].DescriptorBufferInfo],
                                                                           [],
                                                                           true);

    end;

    case fBufferStreamingMode of

     TBufferStreamingMode.Direct:begin
      fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex].UpdateData(fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex].Items[0],
                                                                         0,
                                                                         fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex].Count*SizeOf(TpvMatrix4x4),
                                                                         FlushUpdateData
                                                                        );
     end;

     TBufferStreamingMode.Staging:begin
      fVulkanDevice.MemoryStaging.Upload(fVulkanStagingQueue,
                                         fVulkanStagingCommandBuffer,
                                         fVulkanStagingFence,
                                         fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex].Items[0],
                                         fGlobalVulkanInstanceMatrixBuffers[aInFlightFrameIndex],
                                         0,
                                         fGlobalVulkanInstanceMatrixDynamicArrays[aInFlightFrameIndex].Count*SizeOf(TpvMatrix4x4));
     end;

     else begin
      Assert(false);
     end;

    end;

   end;

  end;

  if fDebugPrimitiveVertexDynamicArrays[aInFlightFrameIndex].Count>0 then begin
   fVulkanDevice.MemoryStaging.Upload(fVulkanStagingQueue,
                                      fVulkanStagingCommandBuffer,
                                      fVulkanStagingFence,
                                      fDebugPrimitiveVertexDynamicArrays[aInFlightFrameIndex].ItemArray[0],
                                      fVulkanDebugPrimitiveVertexBuffers[aInFlightFrameIndex],
                                      0,
                                      SizeOf(TpvScene3D.TDebugPrimitiveVertex)*Min(fDebugPrimitiveVertexDynamicArrays[aInFlightFrameIndex].Count,TpvScene3D.MaxDebugPrimitiveVertices));
  end;

  if fCountInFlightFrameParticleVertices[aInFlightFrameIndex]>0 then begin
   fVulkanDevice.MemoryStaging.Upload(fVulkanStagingQueue,
                                      fVulkanStagingCommandBuffer,
                                      fVulkanStagingFence,
                                      fInFlightFrameParticleVertices[aInFlightFrameIndex][0],
                                      fVulkanParticleVertexBuffers[aInFlightFrameIndex],
                                      0,
                                      SizeOf(TpvScene3D.TParticleVertex)*Min(fCountInFlightFrameParticleVertices[aInFlightFrameIndex],TpvScene3D.MaxParticleVertices));
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

 CullLights(aInFlightFrameIndex,aFrustums,AABBTreeState^.TreeNodes,AABBTreeState^.Root);

 if fCountIndirectLights[aInFlightFrameIndex]>0 then begin
// IndirectIntroSort(@fIndirectLights[aInFlightFrameIndex,0],0,fCountIndirectLights[aInFlightFrameIndex],TpvScene3DCompareIndirectLights);
 end;

end;

function TpvScene3DCompareIndirectLights(const a,b:pointer):TpvInt32;
begin
 result:=Sign((ord(TpvScene3D.TLight(b).fData.fType_=TpvScene3D.TLightData.TLightType.PrimaryDirectional) and 1)-
              (ord(TpvScene3D.TLight(a).fData.fType_=TpvScene3D.TLightData.TLightType.PrimaryDirectional) and 1));
 if result=0 then begin
  result:=Sign((ord(TpvScene3D.TLight(b).fData.fType_=TpvScene3D.TLightData.TLightType.Directional) and 1)-
               (ord(TpvScene3D.TLight(a).fData.fType_=TpvScene3D.TLightData.TLightType.Directional) and 1));
  if result=0 then begin
   result:=Sign(TpvScene3D.TLight(b).fViewSpacePosition.z-TpvScene3D.TLight(a).fViewSpacePosition.z);
   if result=0 then begin
   end;
  end;
 end;
end;

procedure TpvScene3D.CullAndPrepareGroupInstances(const aInFlightFrameIndex:TpvSizeInt;
                                                  const aRendererInstance:TObject;
                                                  const aRenderPassIndex:TpvSizeInt;
                                                  const aViews:TpvScene3D.TViews;
                                                  const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                                                  const aViewBaseIndex:TpvSizeInt;
                                                  const aCountViews:TpvSizeInt;
                                                  const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes;
                                                  const aPotentiallyVisibleSetCulling:boolean;
                                                  const aFrustums:TpvFrustumDynamicArray;
                                                  const aTreeNodes:TpvBVHDynamicAABBTree.TTreeNodes;
                                                  const aRoot:TpvSizeInt);
type TStackItem=record
      Node:TpvSizeInt;
      Mask:TpvUInt32;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicFastStack<TStackItem>;
var Index,ViewIndex:TpvSizeInt;
    PotentiallyVisibleSetNodeIndex,
    ViewPotentiallyVisibleSetNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
    StackItem:PStackItem;
    Node:TpvSizeInt;
    TreeNode:TpvBVHDynamicAABBTree.PTreeNode;
    Mask:TpvUInt32;
    Stack:TStack;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    PotentiallyVisible:boolean;
begin

 if (aRoot>=0) and (length(aTreeNodes)>0) then begin

  Stack.Initialize;
  try

   StackItem:=Stack.PushIndirect;
   StackItem^.Node:=aRoot;
   StackItem^.Mask:=$ffffffff;

   while Stack.PopIndirect(StackItem) do begin

    Node:=StackItem^.Node;

    Mask:=StackItem^.Mask;

    while Node>=0 do begin

     TreeNode:=@aTreeNodes[Node];

     if length(aFrustums)>0 then begin
      if length(aFrustums)=1 then begin
       PotentiallyVisible:=not ((((Mask and $80000000)<>0) and (aFrustums[0].AABBInFrustum(TreeNode^.AABB,Mask)=TpvFrustum.COMPLETE_OUT)));
      end else begin
       PotentiallyVisible:=false;
       for Index:=0 to length(aFrustums)-1 do begin
        if aFrustums[Index].AABBInFrustum(TreeNode^.AABB)<>TpvFrustum.COMPLETE_OUT then begin
         PotentiallyVisible:=true;
         break;
        end;
       end;
      end;
     end else begin
      PotentiallyVisible:=true;
     end;

     if PotentiallyVisible then begin

      if TreeNode^.UserData<>0 then begin

       GroupInstance:=TpvScene3D.TGroup.TInstance(TreeNode^.UserData);

       if (not GroupInstance.fHeadless) and
          (GroupInstance.Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,
                                                  TpvResource.TAsyncLoadState.Done]) then begin

        if aPotentiallyVisibleSetCulling then begin
         PotentiallyVisibleSetNodeIndex:=GroupInstance.fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex];
         if PotentiallyVisibleSetNodeIndex<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
          PotentiallyVisible:=false;
          for ViewIndex:=aViewBaseIndex to (aViewBaseIndex+aCountViews)-1 do begin
           ViewPotentiallyVisibleSetNodeIndex:=aViewNodeIndices[ViewIndex];
           if (ViewPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
              fPotentiallyVisibleSet.GetNodeVisibility(PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex) then begin
            PotentiallyVisible:=true;
            break;
           end;
          end;
         end;
        end;

        if PotentiallyVisible then begin
         GroupInstance.Prepare(aInFlightFrameIndex,
                               aRendererInstance,
                               aRenderPassIndex,
                               aViewNodeIndices,
                               aViewBaseIndex,
                               aCountViews,
                               aFrustums,
                               aPotentiallyVisibleSetCulling,
                               aMaterialAlphaModes,
                               Mask);
        end;

       end;

      end;

      if TreeNode^.Children[0]>=0 then begin
       if TreeNode^.Children[1]>=0 then begin
        StackItem:=Stack.PushIndirect;
        StackItem^.Node:=TreeNode^.Children[1];
        StackItem^.Mask:=Mask;
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

  finally
   Stack.Finalize;
  end;

 end;

end;

procedure TpvScene3D.Prepare(const aInFlightFrameIndex:TpvSizeInt;
                             const aRendererInstance:TObject;
                             const aRenderPassIndex:TpvSizeInt;
                             const aViews:TpvScene3D.TViews;
                             const aViewNodeIndices:TpvScene3D.TPotentiallyVisibleSet.TViewNodeIndices;
                             const aViewBaseIndex:TpvSizeInt;
                             const aCountViews:TpvSizeInt;
                             const aViewPortWidth:TpvInt32;
                             const aViewPortHeight:TpvInt32;
                             const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes;
                             const aLights:boolean;
                             const aFrustumCulling:boolean;
                             const aPotentiallyVisibleSetCulling:boolean;
                             const aGPUCulling:boolean=true);
var Index:TpvSizeInt;
    MaterialAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    FaceCullingMode:TpvScene3D.TFaceCullingMode;
    Frustums:TpvFrustumDynamicArray;
    Group:TpvScene3D.TGroup;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    AABBTreeState:TpvBVHDynamicAABBTree.PState;
    View:TpvScene3D.PView;
    DrawChoreographyBatchItems:TDrawChoreographyBatchItems;
begin

 for MaterialAlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to high(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to high(TpvScene3D.TPrimitiveTopology) do begin
   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to high(TpvScene3D.TFaceCullingMode) do begin
    TpvScene3DRendererInstance(aRendererInstance).DrawChoreographyBatchItemFrameBuckets[aInFlightFrameIndex,aRenderPassIndex,MaterialAlphaMode,PrimitiveTopology,FaceCullingMode].ClearNoFree;
   end;
  end;
 end;

 if (aViewBaseIndex>=0) and (aCountViews>0) then begin

  Frustums:=nil;
  try

   if aFrustumCulling or aPotentiallyVisibleSetCulling then begin

    AABBTreeState:=@fAABBTreeStates[aInFlightFrameIndex];

    if aFrustumCulling then begin
     SetLength(Frustums,aCountViews);
     for Index:=0 to aCountViews-1 do begin
      View:=@aViews.Items[aViewBaseIndex+Index];
      Frustums[Index].Init(View^.ViewMatrix,View^.ProjectionMatrix);
     end;
    end;

    CullAndPrepareGroupInstances(aInFlightFrameIndex,
                                 aRendererInstance,
                                 aRenderPassIndex,
                                 aViews,
                                 aViewNodeIndices,
                                 aViewBaseIndex,
                                 aCountViews,
                                 aMaterialAlphaModes,
                                 aPotentiallyVisibleSetCulling,
                                 Frustums,
                                 AABBTreeState^.TreeNodes,
                                 AABBTreeState^.Root
                                );

   end else begin

    for Group in fGroups do begin
     if (Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done]) and not Group.fHeadless then begin
      for GroupInstance in Group.fInstances do begin
       GroupInstance.Prepare(aInFlightFrameIndex,
                             aRendererInstance,
                             aRenderPassIndex,
                             aViewNodeIndices,
                             aViewBaseIndex,
                             aCountViews,
                             Frustums,
                             aPotentiallyVisibleSetCulling,
                             aMaterialAlphaModes,
                             TpvUInt32($ffffffff));
      end;
     end;
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

 for MaterialAlphaMode:=Low(TpvScene3D.TMaterial.TAlphaMode) to high(TpvScene3D.TMaterial.TAlphaMode) do begin
  for PrimitiveTopology:=Low(TpvScene3D.TPrimitiveTopology) to high(TpvScene3D.TPrimitiveTopology) do begin
   for FaceCullingMode:=Low(TpvScene3D.TFaceCullingMode) to high(TpvScene3D.TFaceCullingMode) do begin
    DrawChoreographyBatchItems:=TpvScene3DRendererInstance(aRendererInstance).DrawChoreographyBatchItemFrameBuckets[aInFlightFrameIndex,aRenderPassIndex,MaterialAlphaMode,PrimitiveTopology,FaceCullingMode];
    if DrawChoreographyBatchItems.Count>1 then begin
     DrawChoreographyBatchItems.IndexOrderSort;
    end;
   end;
  end;
 end;

 TpvScene3DRendererInstance(aRendererInstance).PrepareDraw(aInFlightFrameIndex,
                                                           aRenderPassIndex,
                                                           aMaterialAlphaModes,
                                                           aGPUCulling);

end;

procedure TpvScene3D.SetGlobalResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                        const aPipelineLayout:TpvVulkanPipelineLayout;
                                        const aRendererInstance:TObject;
                                        const aRenderPassIndex:TpvSizeInt;
                                        const aPreviousInFlightFrameIndex:TpvSizeInt;
                                        const aInFlightFrameIndex:TpvSizeInt);
const Offsets:array[0..4] of TVkDeviceSize=(0,0,0,0,0);
var BufferHandles:array[0..4] of TVkBuffer;
    Count:TpvSizeInt;
begin

 if not TpvScene3DRendererInstance(aRendererInstance).fSetGlobalResourcesDone[aRenderPassIndex] then begin

  TpvScene3DRendererInstance(aRendererInstance).fSetGlobalResourcesDone[aRenderPassIndex]:=true;

  aCommandBuffer.CmdPushConstants(aPipelineLayout.Handle,
                                  TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_VERTEX_BIT),
                                  0,
                                  SizeOf(TpvScene3D.TVertexStagePushConstants),
                                  @TpvScene3DRendererInstance(aRendererInstance).VertexStagePushConstants[aRenderPassIndex]);

  aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                       aPipelineLayout.Handle,
                                       0,
                                       1,
                                       @fGlobalVulkanDescriptorSets[aInFlightFrameIndex].Handle,
                                       0,
                                       nil);

  begin

   BufferHandles[0]:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexBuffer.Handle;
   BufferHandles[1]:=fVulkanLongTermStaticBuffers.fBufferDataArray[fVulkanLongTermStaticBuffers.fCurrentIndex].fVulkanStaticVertexBuffer.Handle;
   if aPreviousInFlightFrameIndex>=0 then begin
    if assigned(fVulkanShortTermDynamicBuffers.fBufferDataArray[aPreviousInFlightFrameIndex].fVulkanCachedVertexBuffer) and
       (fVulkanShortTermDynamicBuffers.fBufferDataArray[aPreviousInFlightFrameIndex].fVulkanCachedVertexBuffer.Size>=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexBuffer.Size) then begin
     BufferHandles[2]:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aPreviousInFlightFrameIndex].fVulkanCachedVertexBuffer.Handle;
    end else begin
     BufferHandles[2]:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexBuffer.Handle;
    end;
    BufferHandles[3]:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexGenerationBuffer.Handle;
    if assigned(fVulkanShortTermDynamicBuffers.fBufferDataArray[aPreviousInFlightFrameIndex].fVulkanCachedVertexGenerationBuffer) and
       (fVulkanShortTermDynamicBuffers.fBufferDataArray[aPreviousInFlightFrameIndex].fVulkanCachedVertexGenerationBuffer.Size>=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexGenerationBuffer.Size) then begin
     BufferHandles[4]:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aPreviousInFlightFrameIndex].fVulkanCachedVertexGenerationBuffer.Handle;
    end else begin
     BufferHandles[4]:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexGenerationBuffer.Handle;
    end;
    Count:=5;
   end else begin
    Count:=2;
   end;
   aCommandBuffer.CmdBindVertexBuffers(0,Count,@BufferHandles[0],@Offsets[0]);

   aCommandBuffer.CmdBindIndexBuffer(fVulkanLongTermStaticBuffers.fBufferDataArray[fVulkanLongTermStaticBuffers.fCurrentIndex].fVulkanDrawIndexBuffer.Handle,0,TVkIndexType.VK_INDEX_TYPE_UINT32);

  end;

 end;

end;

function TpvScene3DUpdateCachedVerticesCompare(const a,b:TpvScene3D.TCachedVertexRange):TpvInt32;
begin
 result:=Sign(a.Offset-b.Offset);
end;

procedure TpvScene3D.UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                          const aInFlightFrameIndex:TpvSizeInt;
                                          const aCommandBuffer:TpvVulkanCommandBuffer;
                                          const aPipelineLayout:TpvVulkanPipelineLayout);
var Index,OtherIndex,Count:TpvSizeInt;
    Group:TpvScene3D.TGroup;
    Current,Next:PCachedVertexRange;
    BufferMemoryBarriers:array[0..2] of TVkBufferMemoryBarrier;
    MeshComputeStagePushConstants:TpvScene3D.TMeshComputeStagePushConstants;
begin

 fCachedVertexRanges.Count:=0;

 for Group in fGroups do begin
  if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
   Group.UpdateCachedVertices(aInFlightFrameIndex);
  end;
 end;

 if fCachedVertexRanges.Count>0 then begin

  if fCachedVertexRanges.Count>1 then begin

   // Sort ranges
   TpvTypedSort<TpvScene3D.TCachedVertexRange>.IntroSort(@fCachedVertexRanges.Items[0],
                                                         0,
                                                         fCachedVertexRanges.Count-1,
                                                         TpvScene3DUpdateCachedVerticesCompare);

   // Consolidate ranges
   Index:=0;
   while (Index+1)<fCachedVertexRanges.Count do begin
    Current:=@fCachedVertexRanges.Items[Index];
    Next:=@fCachedVertexRanges.Items[Index+1];
    if (Current^.Offset+Current^.Count)>=Next^.Offset then begin
     Current^.Count:=(Next^.Offset+Next^.Count)-Current^.Offset;
     if (Index+2)<fCachedVertexRanges.Count then begin
//    Move(fCachedVertexRanges.Items[Index+2],fCachedVertexRanges.Items[Index+1],((Count-2)-Index)*SizeOf(TCachedVertexRange));
      for OtherIndex:=Index+1 to fCachedVertexRanges.Count-2 do begin
       fCachedVertexRanges.Items[OtherIndex]:=fCachedVertexRanges.Items[OtherIndex+1];
      end;
     end;
     dec(fCachedVertexRanges.Count);
    end else begin
     inc(Index);
    end;
   end;

  end;

  begin

   aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                        aPipelineLayout.Handle,
                                        0,
                                        1,
                                        @fVulkanLongTermStaticBuffers.fBufferDataArray[fVulkanLongTermStaticBuffers.fCurrentIndex].fVulkanComputeDescriptorSet.Handle,
                                        0,
                                        nil);

   aCommandBuffer.CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_COMPUTE,
                                        aPipelineLayout.Handle,
                                        1,
                                        1,
                                        @fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanComputeDescriptorSet.Handle,
                                        0,
                                        nil);

   for Index:=0 to fCachedVertexRanges.Count-1 do begin

    Current:=@fCachedVertexRanges.Items[Index];

    MeshComputeStagePushConstants.IndexOffset:=Current^.Offset;
    MeshComputeStagePushConstants.CountIndices:=Current^.Count;

    aCommandBuffer.CmdPushConstants(aPipelineLayout.Handle,
                                    TVkShaderStageFlags(TVkShaderStageFlagBits.VK_SHADER_STAGE_COMPUTE_BIT),
                                    0,
                                    SizeOf(TpvScene3D.TMeshComputeStagePushConstants),
                                    @MeshComputeStagePushConstants);

    aCommandBuffer.CmdDispatch((Current^.Count+127) shr 7,1,1);

   end;

   FillChar(BufferMemoryBarriers[0],SizeOf(TVkBufferMemoryBarrier),#0);
   BufferMemoryBarriers[0].sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
   BufferMemoryBarriers[0].pNext:=nil;
   BufferMemoryBarriers[0].buffer:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexBuffer.Handle;
   BufferMemoryBarriers[0].srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   BufferMemoryBarriers[0].dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT);
   BufferMemoryBarriers[0].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   BufferMemoryBarriers[0].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   BufferMemoryBarriers[0].offset:=0;
   BufferMemoryBarriers[0].size:=VK_WHOLE_SIZE;
   Count:=1;

   FillChar(BufferMemoryBarriers[1],SizeOf(TVkBufferMemoryBarrier),#0);
   BufferMemoryBarriers[1].sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
   BufferMemoryBarriers[1].pNext:=nil;
   BufferMemoryBarriers[1].buffer:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedVertexGenerationBuffer.Handle;
   BufferMemoryBarriers[1].srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
   BufferMemoryBarriers[1].dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT);
   BufferMemoryBarriers[1].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   BufferMemoryBarriers[1].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
   BufferMemoryBarriers[1].offset:=0;
   BufferMemoryBarriers[1].size:=VK_WHOLE_SIZE;
   Count:=2;

   if fHardwareRaytracingSupport then begin

    FillChar(BufferMemoryBarriers[2],SizeOf(TVkBufferMemoryBarrier),#0);
    BufferMemoryBarriers[2].sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
    BufferMemoryBarriers[2].pNext:=nil;
    BufferMemoryBarriers[2].buffer:=fVulkanShortTermDynamicBuffers.fBufferDataArray[aInFlightFrameIndex].fVulkanCachedRaytracingVertexBuffer.Handle;
    BufferMemoryBarriers[2].srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
    BufferMemoryBarriers[2].dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT) or TVkAccessFlags(VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR);
    BufferMemoryBarriers[2].srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
    BufferMemoryBarriers[2].dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
    BufferMemoryBarriers[2].offset:=0;
    BufferMemoryBarriers[2].size:=VK_WHOLE_SIZE;
    inc(Count);

   end;

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
                                     IfThen(fHardwareRaytracingSupport,TVkPipelineStageFlags(VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR),0),
                                     0,
                                     0,nil,
                                     Count,@BufferMemoryBarriers[0],
                                     0,nil);

  end;

 end;
end;

procedure TpvScene3D.DrawDebugPrimitives(const aRendererInstance:TObject;
                                         const aGraphicsPipeline:TpvVulkanGraphicsPipeline;
                                         const aPreviousInFlightFrameIndex:TpvSizeInt;
                                         const aInFlightFrameIndex:TpvSizeInt;
                                         const aRenderPassIndex:TpvSizeInt;
                                         const aViewBaseIndex:TpvSizeInt;
                                         const aCountViews:TpvSizeInt;
                                         const aFrameIndex:TpvSizeInt;
                                         const aCommandBuffer:TpvVulkanCommandBuffer;
                                         const aPipelineLayout:TpvVulkanPipelineLayout;
                                         const aOnSetRenderPassResources:TpvScene3D.TOnSetRenderPassResources);
const Offsets:TVkDeviceSize=0;
//var VertexStagePushConstants:TpvScene3D.PVertexStagePushConstants;
begin
 if (aViewBaseIndex>=0) and (aCountViews>0) and (fDebugPrimitiveVertexDynamicArrays[aInFlightFrameIndex].Count>0) then begin

{ VertexStagePushConstants:=@fVertexStagePushConstants[aRenderPassIndex];
  VertexStagePushConstants^.ViewBaseIndex:=aViewBaseIndex;
  VertexStagePushConstants^.CountViews:=aCountViews;
  VertexStagePushConstants^.CountAllViews:=fViews.Count;
  VertexStagePushConstants^.FrameIndex:=aFrameIndex;
  VertexStagePushConstants^.Jitter:=TpvVector4.Null;

  fSetGlobalResourcesDone[aRenderPassIndex]:=false;}
  SetGlobalResources(aCommandBuffer,aPipelineLayout,aRendererInstance,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);

  if assigned(aOnSetRenderPassResources) then begin
   aOnSetRenderPassResources(aCommandBuffer,aPipelineLayout,aRendererInstance,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);
  end;

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,aGraphicsPipeline.Handle);
  aCommandBuffer.CmdBindVertexBuffers(0,1,@fVulkanDebugPrimitiveVertexBuffers[aInFlightFrameIndex].Handle,@Offsets);
  aCommandBuffer.CmdDraw(Min(fDebugPrimitiveVertexDynamicArrays[aInFlightFrameIndex].Count,TpvScene3D.MaxDebugPrimitiveVertices),1,0,0);

 end;
end;

procedure TpvScene3D.DrawParticles(const aRendererInstance:TObject;
                                   const aGraphicsPipeline:TpvVulkanGraphicsPipeline;
                                   const aPreviousInFlightFrameIndex:TpvSizeInt;
                                   const aInFlightFrameIndex:TpvSizeInt;
                                   const aRenderPassIndex:TpvSizeInt;
                                   const aViewBaseIndex:TpvSizeInt;
                                   const aCountViews:TpvSizeInt;
                                   const aFrameIndex:TpvSizeInt;
                                   const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aPipelineLayout:TpvVulkanPipelineLayout;
                                   const aOnSetRenderPassResources:TpvScene3D.TOnSetRenderPassResources);
const Offsets:TVkDeviceSize=0;
//var VertexStagePushConstants:TpvScene3D.PVertexStagePushConstants;
begin

 if (aViewBaseIndex>=0) and (aCountViews>0) and (fCountInFlightFrameParticleVertices[aInFlightFrameIndex]>0) then begin

{ VertexStagePushConstants:=@fVertexStagePushConstants[aRenderPassIndex];
  VertexStagePushConstants^.ViewBaseIndex:=aViewBaseIndex;
  VertexStagePushConstants^.CountViews:=aCountViews;
  VertexStagePushConstants^.CountAllViews:=fViews.Count;
  VertexStagePushConstants^.FrameIndex:=aFrameIndex;
  VertexStagePushConstants^.Jitter:=TpvVector4.Null;

  fSetGlobalResourcesDone[aRenderPassIndex]:=false;}
  SetGlobalResources(aCommandBuffer,aPipelineLayout,aRendererInstance,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);

  if assigned(aOnSetRenderPassResources) then begin
   aOnSetRenderPassResources(aCommandBuffer,aPipelineLayout,aRendererInstance,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);
  end;

  aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,aGraphicsPipeline.Handle);
  aCommandBuffer.CmdBindVertexBuffers(0,1,@fVulkanParticleVertexBuffers[aInFlightFrameIndex].Handle,@Offsets);
  aCommandBuffer.CmdDraw(Min(fCountInFlightFrameParticleVertices[aInFlightFrameIndex],TpvScene3D.MaxParticleVertices),1,0,0);

 end;

end;

procedure TpvScene3D.Draw(const aRendererInstance:TObject;
                          const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                          const aInFlightFrameIndex:TpvSizeInt;
                          const aRenderPassIndex:TpvSizeInt;
                          const aViewBaseIndex:TpvSizeInt;
                          const aCountViews:TpvSizeInt;
                          const aFrameIndex:TpvSizeInt;
                          const aCommandBuffer:TpvVulkanCommandBuffer;
                          const aPipelineLayout:TpvVulkanPipelineLayout;
                          const aOnSetRenderPassResources:TpvScene3D.TOnSetRenderPassResources;
                          const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes;
                          const aJitter:PpvVector4;
                          const aDisocclusions:Boolean);
begin
 TpvScene3DRendererInstance(aRendererInstance).ExecuteDraw(aPreviousInFlightFrameIndex,
                                                           aInFlightFrameIndex,
                                                           aRenderPassIndex,
                                                           aViewBaseIndex,
                                                           aCountViews,
                                                           aFrameIndex,
                                                           aMaterialAlphaModes,
                                                           aGraphicsPipelines,
                                                           aCommandBuffer,
                                                           aPipelineLayout,
                                                           aOnSetRenderPassResources,
                                                           aJitter,
                                                           aDisocclusions);
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
 aPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3D.TGPUCachedVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 aPipeline.VertexInputState.AddVertexInputBindingDescription(1,SizeOf(TpvScene3D.TGPUStaticVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 if aWithPreviousPosition then begin
  aPipeline.VertexInputState.AddVertexInputBindingDescription(2,SizeOf(TpvScene3D.TGPUCachedVertex),VK_VERTEX_INPUT_RATE_VERTEX);
  aPipeline.VertexInputState.AddVertexInputBindingDescription(3,SizeOf(TpvScene3D.TGPUCachedVertexGeneration),VK_VERTEX_INPUT_RATE_VERTEX);
  aPipeline.VertexInputState.AddVertexInputBindingDescription(4,SizeOf(TpvScene3D.TGPUCachedVertexGeneration),VK_VERTEX_INPUT_RATE_VERTEX);
 end;
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PGPUCachedVertex(nil)^.Position)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R16G16B16A16_SNORM,TVkPtrUInt(pointer(@TpvScene3D.PGPUCachedVertex(nil)^.NormalSign)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R16G16B16_SNORM,TVkPtrUInt(pointer(@TpvScene3D.PGPUCachedVertex(nil)^.TangentXYZModelScaleX)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R16G16B16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PGPUCachedVertex(nil)^.TangentXYZModelScaleX[3])));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(4,1,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PGPUStaticVertex(nil)^.TexCoord0)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(5,1,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PGPUStaticVertex(nil)^.TexCoord1)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(6,1,VK_FORMAT_R16G16B16A16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PGPUStaticVertex(nil)^.Color0)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(7,1,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PGPUStaticVertex(nil)^.MaterialID)));
 if aWithPreviousPosition then begin
  aPipeline.VertexInputState.AddVertexInputAttributeDescription(8,2,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PGPUCachedVertex(nil)^.Position)));
  aPipeline.VertexInputState.AddVertexInputAttributeDescription(9,3,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PGPUCachedVertexGeneration(nil)^.Generation)));
  aPipeline.VertexInputState.AddVertexInputAttributeDescription(10,4,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PGPUCachedVertexGeneration(nil)^.Generation)));
 end;
end;

procedure TpvScene3D.InitializeDebugPrimitiveGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline);
begin
 aPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3D.TDebugPrimitiveVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PDebugPrimitiveVertex(nil)^.Position)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R16G16B16A16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PDebugPrimitiveVertex(nil)^.Color)));
end;

procedure TpvScene3D.InitializeParticleGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline);
begin
 aPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TpvScene3D.TParticleVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PParticleVertex(nil)^.Position)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PParticleVertex(nil)^.Rotation)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R16G16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PParticleVertex(nil)^.QuadCoord)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32_UINT,TVkPtrUInt(pointer(@TpvScene3D.PParticleVertex(nil)^.TextureID)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PParticleVertex(nil)^.Size)));
 aPipeline.VertexInputState.AddVertexInputAttributeDescription(5,0,VK_FORMAT_R16G16B16A16_SFLOAT,TVkPtrUInt(pointer(@TpvScene3D.PParticleVertex(nil)^.Color)));
end;

procedure InitializeAnimationChannelTargetOverwriteGroupMap;
var Target:TpvScene3D.TGroup.TAnimation.TChannel.TTarget;
    Index:TpvUInt64;
begin
 for Target:=Low(TpvScene3D.TGroup.TAnimation.TChannel.TTarget) to High(TpvScene3D.TGroup.TAnimation.TChannel.TTarget) do begin

  case Target of

   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Translation,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Rotation,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Scale,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.Weights,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeTranslation,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeRotation,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeScale,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerNodeWeights:begin
    Index:=1;
   end;

   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
    Index:=2;
   end;

   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicXMag,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicYMag,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZFar,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraOrthographicZNear,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveAspectRatio,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveYFov,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZFar,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerCameraPerspectiveZNear:begin
    Index:=3;
   end;

   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessBaseColorFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessMetallicFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRMetallicRoughnessRoughnessFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialAlphaCutOff,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRClearCoatRoughnessFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialIOR,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyStrength,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerMaterialPBRAnisotropyRotation,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation,
   TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin
    Index:=4;
   end;

   else begin
    Index:=0;
   end;

  end;

  AnimationChannelTargetOverwriteGroupMap[Target]:=Index;

 end;

end;

procedure TpvScene3D.StoreParticleStates;
var ParticleAliveBitmapIndex,ParticleAliveBitmapValue,
    ParticleBaseIndex,ParticleBitIndex,ParticleIndex:TpvUInt32;
    Particle:PParticle;
begin
 for ParticleAliveBitmapIndex:=0 to length(fParticleAliveBitmap)-1 do begin
  ParticleAliveBitmapValue:=fParticleAliveBitmap[ParticleAliveBitmapIndex];
  if ParticleAliveBitmapValue<>0 then begin
   ParticleBaseIndex:=ParticleAliveBitmapIndex shl 5;
   repeat
    ParticleBitIndex:=TPasMPMath.BitScanForward32(ParticleAliveBitmapValue);
    ParticleIndex:=ParticleBaseIndex+ParticleBitIndex;
    Particle:=@fParticles[ParticleIndex];
    Particle^.LastGeneration:=Particle^.Generation;
    Particle^.LastPosition:=Particle^.Position;
    Particle^.LastTime:=Particle^.Age/Particle^.LifeTime;
    ParticleAliveBitmapValue:=ParticleAliveBitmapValue and (ParticleAliveBitmapValue-1);
   until ParticleAliveBitmapValue=0;
  end;
 end;
end;

procedure TpvScene3D.UpdateParticleStates(const aDeltaTime:TpvDouble);
var ParticleAliveBitmapIndex,ParticleAliveBitmapValue,
    ParticleBaseIndex,ParticleBitIndex,ParticleIndex:TpvUInt32;
    Particle:PParticle;
begin
 for ParticleAliveBitmapIndex:=0 to length(fParticleAliveBitmap)-1 do begin
  ParticleAliveBitmapValue:=fParticleAliveBitmap[ParticleAliveBitmapIndex];
  if ParticleAliveBitmapValue<>0 then begin
   ParticleBaseIndex:=ParticleAliveBitmapIndex shl 5;
   repeat
    ParticleBitIndex:=TPasMPMath.BitScanForward32(ParticleAliveBitmapValue);
    ParticleIndex:=ParticleBaseIndex+ParticleBitIndex;
    Particle:=@fParticles[ParticleIndex];
    if (Particle^.Age>=Particle^.LifeTime) or IsZero(Particle^.LifeTime) then begin
     fParticleAliveBitmap[ParticleAliveBitmapIndex]:=fParticleAliveBitmap[ParticleAliveBitmapIndex] and not (TpvUInt32(1) shl ParticleBitIndex);
    end else begin
     Particle^.Position:=Particle^.Position+(Particle^.Velocity*aDeltaTime);
     Particle^.Velocity:=Particle^.Velocity+(Particle^.Gravity*aDeltaTime);
     Particle^.Time:=Particle^.Age/Particle^.LifeTime;
     Particle^.Age:=Particle^.Age+aDeltaTime;
    end;
    ParticleAliveBitmapValue:=ParticleAliveBitmapValue and (ParticleAliveBitmapValue-1);
   until ParticleAliveBitmapValue=0;
  end;
 end;
end;

procedure TpvScene3D.InterpolateParticleStates(const aInFlightFrameIndex:TpvSizeInt;const aAlpha:TpvDouble);
var ParticleAliveBitmapIndex,ParticleAliveBitmapValue,
    ParticleBaseIndex,ParticleBitIndex,ParticleIndex,
    CountVertices,TextureID:TpvUInt32;
    Particle:PParticle;
    Time,Rotation:TpvFloat;
    Position:TpvVector3;
    Size:TpvVector2;
    HalfFloatColor:TpvHalfFloatVector4;
    ParticleVertices:TpvScene3D.PParticleVertices;
    ParticleVertex:PParticleVertex;
begin

 CountVertices:=0;

 ParticleVertices:=@fInFlightFrameParticleVertices[aInFlightFrameIndex];

 for ParticleAliveBitmapIndex:=0 to length(fParticleAliveBitmap)-1 do begin

  ParticleAliveBitmapValue:=fParticleAliveBitmap[ParticleAliveBitmapIndex];

  if ParticleAliveBitmapValue<>0 then begin

   ParticleBaseIndex:=ParticleAliveBitmapIndex shl 5;

   repeat

    ParticleBitIndex:=TPasMPMath.BitScanForward32(ParticleAliveBitmapValue);

    ParticleIndex:=ParticleBaseIndex+ParticleBitIndex;

    Particle:=@fParticles[ParticleIndex];

    if Particle^.LastGeneration=Particle^.Generation then begin
     // Same generation, so interpolate for an already existing particle
     Position:=Particle^.LastPosition.Lerp(Particle^.Position,aAlpha);
     Time:=FloatLerp(Particle^.LastTime,Particle^.Time,aAlpha);
    end else begin
     // Different generation, so it is a fresh new particle => consider it as a particle without previous state
     Position:=Particle^.Position;
     Time:=0.0;
    end;

    Rotation:=FloatLerp(Particle^.RotationStart,Particle^.RotationEnd,Time);

    Size:=Particle^.SizeStart.Lerp(Particle^.SizeEnd,Time);

    HalfFloatColor.x:=FloatLerp(Particle^.ColorStart.x,Particle^.ColorEnd.x,Time);
    HalfFloatColor.y:=FloatLerp(Particle^.ColorStart.y,Particle^.ColorEnd.y,Time);
    HalfFloatColor.z:=FloatLerp(Particle^.ColorStart.z,Particle^.ColorEnd.z,Time);
    HalfFloatColor.w:=FloatLerp(Particle^.ColorStart.w,Particle^.ColorEnd.w,Time);

    TextureID:=Particle^.TextureID;

    // Each particle has an oversized triangle that contains the actual particle quad.
    // This might increase overdraw, but it reduces the number of vertices used.

    ParticleVertex:=@ParticleVertices^[CountVertices+0];
    ParticleVertex^.Position:=Position;
    ParticleVertex^.Rotation:=Rotation;
    ParticleVertex^.QuadCoord.x:=0.0;
    ParticleVertex^.QuadCoord.y:=0.0;
    ParticleVertex^.TextureID:=TextureID;
    ParticleVertex^.Size:=Size;
    ParticleVertex^.Color:=HalfFloatColor;

    ParticleVertex:=@ParticleVertices^[CountVertices+1];
    ParticleVertex^.Position:=Position;
    ParticleVertex^.Rotation:=Rotation;
    ParticleVertex^.QuadCoord.x:=2.0;
    ParticleVertex^.QuadCoord.y:=0.0;
    ParticleVertex^.TextureID:=TextureID;
    ParticleVertex^.Size:=Size;
    ParticleVertex^.Color:=HalfFloatColor;

    ParticleVertex:=@ParticleVertices^[CountVertices+2];
    ParticleVertex^.Position:=Position;
    ParticleVertex^.Rotation:=Rotation;
    ParticleVertex^.QuadCoord.x:=0.0;
    ParticleVertex^.QuadCoord.y:=2.0;
    ParticleVertex^.TextureID:=TextureID;
    ParticleVertex^.Size:=Size;
    ParticleVertex^.Color:=HalfFloatColor;

    inc(CountVertices,3);

    ParticleAliveBitmapValue:=ParticleAliveBitmapValue and (ParticleAliveBitmapValue-1);

   until ParticleAliveBitmapValue=0;

  end;

 end;

 fCountInFlightFrameParticleVertices[aInFlightFrameIndex]:=CountVertices;

end;

procedure TpvScene3D.DeleteAllParticles;
begin
 FillChar(fParticleAliveBitmap,SizeOf(TParticleAliveBitmap),#0); // really so simple as that 
end;

function TpvScene3D.AddParticle(const aPosition:TpvVector3;
                                const aVelocity:TpvVector3;
                                const aGravity:TpvVector3;
                                const aRotationStart:TpvFloat;
                                const aRotationEnd:TpvFloat;
                                const aSizeStart:TpvVector2;
                                const aSizeEnd:TpvVector2;
                                const aColorStart:TpvVector4;
                                const aColorEnd:TpvVector4;
                                const aLifeTime:TpvScalar;
                                const aTextureID:TpvUInt32;
                                const aAdditiveBlending:boolean):TpvSizeInt;
var Particle:PParticle;
begin
 // No free list, because of simple wraparound-based ring buffer style allocation, so we don't also check for the agest particle as performance optimization
 result:=fParticleIndexCounter and ParticleIndexMask;
 fParticleIndexCounter:=(fParticleIndexCounter+1) and ParticleIndexMask;
 fParticleAliveBitmap[result shr 5]:=fParticleAliveBitmap[result shr 5] or (TpvUInt32(1) shl (result and 31));
 Particle:=@fParticles[result];
 Particle^.Generation:=Particle^.Generation+1;
 Particle^.Position:=aPosition;
 Particle^.Velocity:=aVelocity;
 Particle^.Gravity:=aGravity;
 Particle^.RotationStart:=aRotationStart;
 Particle^.RotationEnd:=aRotationEnd;
 Particle^.SizeStart:=aSizeStart;
 Particle^.SizeEnd:=aSizeEnd;
 Particle^.ColorStart:=aColorStart;
 Particle^.ColorEnd:=aColorEnd;
 Particle^.LastTime:=0.0;
 Particle^.Time:=0.0;
 Particle^.Age:=0.0;
 Particle^.LifeTime:=aLifeTime;
 if aAdditiveBlending then begin
  Particle^.TextureID:=aTextureID or TpvUInt32($80000000);
 end else begin
  Particle^.TextureID:=aTextureID;
 end;
end;

initialization
 InitializeAnimationChannelTargetOverwriteGroupMap;
end.


