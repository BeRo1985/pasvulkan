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
     PasVulkan.PooledObject,
     PasVulkan.Frustum;

type EpvScene3D=class(Exception);

     { TpvScene3D }

     TpvScene3D=class(TpvResource)
      public
       const MaxRenderPassIndices=32;
             MaxVisibleLights=65536;
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
       type TPrimitiveTopology=VK_PRIMITIVE_TOPOLOGY_POINT_LIST..VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN;
            TDoubleSided=boolean;
            TFrontFacesInversed=boolean;
            TFaceCullingMode=
             (
              None,
              Normal,
              Inversed
             );
            TBufferStreamingMode=
             (
              Direct,
              Staging
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
              PBRUnlitColorTexture=18
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
            TInFlightFrameAABBs=array[0..MaxInFlightFrames-1] of TpvAABB;
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
              fInUpload:TPasMPBool32;
              fAdded:TPasMPBool32;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure PrepareDeferredFree; override;
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
              property ReferenceCounter:Int32 read fReferenceCounter;
            end;
            TBaseObjects=TpvObjectGenericList<TBaseObject>;
            { TBakedMesh }
            TBakedMesh=class
             public
              type { TTriangle }
                   TTriangle=class(TpvPooledObject)
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
                     procedure Assign(const aFrom:TpvScene3D.TBakedMesh.TTriangle);
                     function RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;var aTime,aU,v:TpvScalar):boolean;
                   end;
                   TTriangles=class(TpvObjectGenericList<TpvScene3D.TBakedMesh.TTriangle>)
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
                   TNode=class(TpvPooledObject)
                    private
                     fOwner:TPotentiallyVisibleSet;
                     fParent:TNode;
                     fLevel:TpvUInt32;
                     fAABB:TpvAABB;
                     fLeft:TpvScene3D.TPotentiallyVisibleSet.TNode;
                     fRight:TpvScene3D.TPotentiallyVisibleSet.TNode;
                     fIndex:TpvUInt32;
                     fSkipCount:TpvUInt32;
                     fTag:TpvPtrInt;
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
              fTriangleDynamicAABBTree:TpvBVHDynamicAABBTree;
              fTriangleDynamicAABBTreeSkipList:TpvBVHDynamicAABBTree.TSkipList;
              fAABB:TpvAABB;
              fRoot:TpvScene3D.TPotentiallyVisibleSet.TNode;
              fNodes:TpvScene3D.TPotentiallyVisibleSet.TNodes;
              fManualBoundingBoxes:TpvScene3D.TPotentiallyVisibleSet.TManualBoundingBoxes;
              fBitmapOneDimensionSize:TpvUInt32;
              fBitmapSize:TpvUInt32;
              fBitmap:TBitmap;
              fPasMPInstance:TPasMP;
              fViewNodeIndices:TViewNodeIndices;
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
              function BeginLoad(const aStream:TStream):boolean; override;
              function EndLoad:boolean; override;
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
                   TShaderData=packed record // 2048 bytes
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
              fGeneration:TpvUInt64;
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
              procedure PrepareGPUUpdate;
              procedure ExecuteGPUUpdate;
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
            { TGroup }
            TGroup=class(TBaseObject) // A group is a GLTF scene in a uber-scene
             public
              type TNode=class;
                   TMesh=class;
                   TScene=class;
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
                   TMaterialMap=array of TpvUInt32;
                   TMaterialIDMapArrayIndexHashMap=TpvHashMap<TID,TpvSizeInt>;
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
                            PrimitiveMode:TVkPrimitiveTopology;
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
                          TUsedByScenesList=TpvObjectGenericList<TpvScene3D.TGroup.TScene>;
                    private
                     fIndex:TpvSizeInt;
                     fUsedByScenesList:TUsedByScenesList;
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
                   { TDrawChoreographyBatchItem }
                   TDrawChoreographyBatchItem=class
                    private
                     fAlphaMode:TpvScene3D.TMaterial.TAlphaMode;
                     fPrimitiveTopology:TpvScene3D.TPrimitiveTopology;
                     fDoubleSided:boolean;
                     fMaterial:TpvScene3D.TMaterial;
                     fNode:TpvScene3D.TGroup.TNode;
                     fMesh:TpvScene3D.TGroup.TMesh;
                     fMeshPrimitive:TpvSizeInt;
                     fStartIndex:TpvSizeInt;
                     fCountIndices:TpvSizeInt;
                    public
                     function CompareTo(const aOther:TpvScene3D.TGroup.TDrawChoreographyBatchItem):TpvInt32;
                    published
                     property AlphaMode:TpvScene3D.TMaterial.TAlphaMode read fAlphaMode write fAlphaMode;
                     property PrimitiveTopology:TpvScene3D.TPrimitiveTopology read fPrimitiveTopology write fPrimitiveTopology;
                     property DoubleSided:boolean read fDoubleSided write fDoubleSided;
                     property Material:TpvScene3D.TMaterial read fMaterial write fMaterial;
                     property Node:TpvScene3D.TGroup.TNode read fNode write fNode;
                     property Mesh:TpvScene3D.TGroup.TMesh read fMesh write fMesh;
                     property MeshPrimitive:TpvSizeInt read fMeshPrimitive write fMeshPrimitive;
                     property StartIndex:TpvSizeInt read fStartIndex write fStartIndex;
                     property CountIndices:TpvSizeInt read fCountIndices write fCountIndices;
                   end;
                   { TDrawChoreographyBatchItems }
                   TDrawChoreographyBatchItems=class(TpvObjectGenericList<TDrawChoreographyBatchItem>)
                    public
                     procedure Sort;
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
                    private
                     fIndex:TpvSizeInt;
                     fNodes:TNodes;
                     fDrawChoreographyBatchItems:TDrawChoreographyBatchItems;
                     fDrawChoreographyBatchUniqueItems:TDrawChoreographyBatchItems;
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
                                 TNodeFlag=
                                  (
                                   InverseFrontFaces
                                  );
                                 PNodeFlag=^TNodeFlag;
                                 TNodeFlags=set of TNodeFlag;
                           public
                            Processed:LongBool;
                            Flags:TNodeFlags;
                            Overwrites:TNodeOverwrites;
                            CountOverwrites:TpvSizeInt;
                            OverwriteWeightsSum:TpvDoubleDynamicArray;
                            WorkWeights:TpvFloatDynamicArray;
                            WorkMatrix:TpvMatrix4x4;
                            VisibleBitmap:TpvUInt32;
                            Light:TpvScene3D.TLight;
                            BoundingBoxes:array[0..MaxInFlightFrames-1] of TpvAABB;
                            BoundingBoxFilled:array[0..MaxInFlightFrames-1] of boolean;
                            PotentiallyVisibleSetNodeIndices:array[0..MaxInFlightFrames-1] of TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
                            CacheVerticesGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
                            CacheVerticesGeneration:TpvUInt64;
                            CacheVerticesDirtyCounter:TpvUInt32;
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
                          TOnNodeMatrix=procedure(const aInstance:TInstance;aNode,InstanceNode:pointer;var Matrix:TpvMatrix4x4) of object;
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
                          { TVulkanData }
                          TVulkanData=class
                           private
                            fUploaded:boolean;
                            fInUpload:boolean;
                            fInstance:TInstance;
                            fNodeMatricesBuffer:TpvVulkanBuffer;
                            fMorphTargetVertexWeightsBuffer:TpvVulkanBuffer;
                            fNodeMatrices:TNodeMatrices;
                            fMorphTargetVertexWeights:TMorphTargetVertexWeights;
                           public
                            constructor Create(const aInstance:TInstance); reintroduce;
                            destructor Destroy; override;
                            procedure Upload;
                            procedure Unload;
                            procedure PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
                            procedure ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
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
                     fMaterialMap:TpvScene3D.TGroup.TMaterialMap;
                     fDuplicatedMaterials:TpvScene3D.TMaterials;
                     fMaterials:TpvScene3D.TGroup.TInstance.TMaterials;
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
                     fVulkanMaterialIDMapBuffer:TpvVulkanBuffer;
                     fVulkanComputeDescriptorPool:TpvVulkanDescriptorPool;
                     fVulkanComputeDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
                     fScenes:array[0..MaxInFlightFrames-1] of TpvScene3D.TGroup.TScene;
                     fActives:array[0..MaxInFlightFrames-1] of boolean;
                     fPotentiallyVisibleSetNodeIndices:array[0..MaxInFlightFrames-1] of TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
                     fAABBTreeProxy:TpvSizeInt;
                     fVisibleBitmap:TPasMPUInt32;
                     fCacheVerticesNodeDirtyBitmap:array of TpvUInt32;
                     function GetAutomation(const aIndex:TPasGLTFSizeInt):TpvScene3D.TGroup.TInstance.TAnimation;
                     procedure SetScene(const aScene:TpvSizeInt);
                     function GetScene:TpvScene3D.TGroup.TScene;
                     procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                       const aRenderPassIndex:TpvSizeInt;
                                       const aViewBaseIndex:TpvSizeInt;
                                       const aCountViews:TpvSizeInt;
                                       const aFrustums:TpvFrustumDynamicArray;
                                       const aPotentiallyVisibleSetCulling:boolean);
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
                     procedure Remove; override;
                     procedure Upload; override;
                     procedure Unload; override;
                     procedure UpdateInvisible;
                     procedure Check(const aInFlightFrameIndex:TpvSizeInt);
                     procedure Update(const aInFlightFrameIndex:TpvSizeInt);
                     procedure PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
                     procedure ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
                     function GetBakedMesh(const aRelative:boolean=false;
                                           const aWithDynamicMeshs:boolean=false;
                                           const aRootNodeIndex:TpvSizeInt=-1;
                                           const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]):TpvScene3D.TBakedMesh;
                     function GetCamera(const aNodeIndex:TPasGLTFSizeInt;
                                        out aCameraMatrix:TpvMatrix4x4;
                                        out aViewMatrix:TpvMatrix4x4;
                                        out aProjectionMatrix:TpvMatrix4x4;
                                        const aReversedZ:boolean=false;
                                        const aInfiniteFarPlane:boolean=false;
                                        const aZNear:PpvFloat=nil;
                                        const aZFar:PpvFloat=nil;
                                        const aAspectRatio:TpvFloat=0.0):boolean;
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
                   TMaterialsToDuplicate=TpvObjectGenericList<TpvScene3D.TMaterial>;
                   TNodeNameIndexHashMap=TpvStringHashMap<TpvSizeInt>;
                   TCameraNodeIndices=TpvGenericList<TpvSizeInt>;
             private
              fCulling:boolean;
              fObjects:TBaseObjects;
              fMaterialsToDuplicate:TpvScene3D.TGroup.TMaterialsToDuplicate;
              fMaterials:TpvScene3D.TMaterials;
              fMaterialMap:TpvScene3D.TGroup.TMaterialMap;
              fMaterialIDMapArrayIndexHashMap:TpvScene3D.TGroup.TMaterialIDMapArrayIndexHashMap;
              fMaterialIDMapArrays:TpvScene3D.TGroup.TMaterialIDMapArrays;
              fAnimations:TpvScene3D.TGroup.TAnimations;
              fCountInstanceAnimationChannels:TpvSizeInt;
              fCameras:TpvScene3D.TGroup.TCameras;
              fMeshes:TpvScene3D.TGroup.TMeshes;
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
              fNodeShaderStorageBufferObject:TNodeShaderStorageBufferObject;
              fLock:TPasMPSpinLock;
              fVulkanVertexBuffer:TpvVulkanBuffer;
              fVulkanCachedVertexBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
              //fVulkanIndexBuffer:TpvVulkanBuffer;
              fVulkanDrawIndexBuffer:TpvVulkanBuffer;
              fVulkanDrawUniqueIndexBuffer:TpvVulkanBuffer;
{             fVulkanMaterialIndexBuffer:TpvVulkanBuffer;
              fVulkanMaterialUniqueIndexBuffer:TpvVulkanBuffer;}
              fVulkanMorphTargetVertexBuffer:TpvVulkanBuffer;
              fVulkanJointBlockBuffer:TpvVulkanBuffer;
{             fVulkanNodeMatricesStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
              fVulkanMorphTargetVertexWeightsStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;}
              fInstanceListLock:TPasMPSlimReaderWriterLock;
              fInstances:TInstances;
              fBoundingBox:TpvAABB;
              fSetGroupResourcesDone:array[0..MaxRenderPassIndices-1] of boolean;
              fCachedVerticesUpdated:boolean;
              fUsedVisibleDrawNodes:TUsedVisibleDrawNodes;
              fDrawChoreographyBatchItems:TDrawChoreographyBatchItems;
              fDrawChoreographyBatchUniqueItems:TDrawChoreographyBatchItems;
              fNodeNameIndexHashMap:TpvScene3D.TGroup.TNodeNameIndexHashMap;
              fCameraNodeIndices:TpvScene3D.TGroup.TCameraNodeIndices;
              procedure ConstructBuffers;
              procedure CollectUsedVisibleDrawNodes;
              procedure CollectMaterials;
              procedure ConstructDrawChoreographyBatchItems;
              procedure Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                const aRenderPassIndex:TpvSizeInt;
                                const aViewBaseIndex:TpvSizeInt;
                                const aCountViews:TpvSizeInt;
                                const aFrustums:TpvFrustumDynamicArray;
                                const aPotentiallyVisibleSetCulling:boolean);
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
              function GetNodeIndexByName(const aNodeName:TpvUTF8String):TpvSizeInt;
              function GetNodeByName(const aNodeName:TpvUTF8String):TpvScene3D.TGroup.TNode;
             public
              constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure Remove; override;
              procedure Upload; override;
              procedure Unload; override;
              procedure Check(const aInFlightFrameIndex:TpvSizeInt);
              procedure Update(const aInFlightFrameIndex:TpvSizeInt);
              procedure PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
              procedure ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
              procedure AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument);
              function BeginLoad(const aStream:TStream):boolean; override;
              function EndLoad:boolean; override;
              function CreateInstance:TpvScene3D.TGroup.TInstance;
             public
              property BoundingBox:TpvAABB read fBoundingBox;
              property NodeIndexByName[const aNodeName:TpvUTF8String]:TpvSizeInt read GetNodeIndexByName;
              property NodeByName[const aNodeName:TpvUTF8String]:TpvScene3D.TGroup.TNode read GetNodeByName;
              property CameraNodeIndices:TpvScene3D.TGroup.TCameraNodeIndices read fCameraNodeIndices;
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
            TMaterialIDDirtyMap=array[0..(($10000+31) shr 5)-1] of TPasMPUint32;
            TMaterialIDMap=array[0..$ffff] of TMaterial;
            TMaterialGenerations=array[0..$ffff] of TpvUInt64;
            TImageHashMap=TpvHashMap<TImage.THashData,TImage>;
            TSamplerHashMap=TpvHashMap<TSampler.THashData,TSampler>;
            TTextureHashMap=TpvHashMap<TTexture.THashData,TTexture>;
            TMaterialHashMap=TpvHashMap<TMaterial.THashData,TMaterial>;
            TBufferMemoryBarriers=TpvDynamicArray<TVkBufferMemoryBarrier>;
            TInFlightFrameBufferMemoryBarriers=array[0..MaxInFlightFrames-1] of TBufferMemoryBarriers;
            TMaterialBufferData=array[0..65535] of TMaterial.TShaderData;
            TImageInfos=array[0..65535] of TVkDescriptorImageInfo;
      public
       const DoubleSidedFaceCullingModes:array[TDoubleSided,TFrontFacesInversed] of TFaceCullingMode=
              (
               (TFaceCullingMode.Normal,TFaceCullingMode.Inversed),
               (TFaceCullingMode.None,TFaceCullingMode.None)
              );
      private
       fLock:TPasMPSpinLock;
       fVulkanDevice:TpvVulkanDevice;
       fUploaded:TPasMPBool32;
       fInUpload:TPasMPBool32;
       fPotentiallyVisibleSet:TpvScene3D.TPotentiallyVisibleSet;
       fBufferStreamingMode:TBufferStreamingMode;
       fDefaultSampler:TSampler;
       fWhiteImage:TImage;
       fWhiteTexture:TTexture;
       fDefaultNormalMapImage:TImage;
       fDefaultNormalMapTexture:TTexture;
       fMeshComputeVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanStagingQueue:TpvVulkanQueue;
       fVulkanStagingCommandPool:TpvVulkanCommandPool;
       fVulkanStagingCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanStagingFence:TpvVulkanFence;
       fImageDescriptorGenerationLock:TPasMPSpinLock;
       fImageDescriptorProcessedGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fImageDescriptorGeneration:TpvUInt64;
       fImageDescriptorProcessedGeneration:TpvUInt64;
       fGlobalVulkanViews:array[0..MaxInFlightFrames-1] of TGlobalViewUniformBuffer;
//     fGlobalVulkanViewUniformStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fGlobalVulkanViewUniformBuffers:TGlobalVulkanViewUniformBuffers;
       fGlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fGlobalVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fGlobalVulkanDescriptorSets:array[0..MaxInFlightFrames-1] of TpvVulkanDescriptorSet;
{      fVulkanLightItemsStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanLightTreeStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanLightMetaInfoStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;}
       fMaterialBufferData:TMaterialBufferData;
       fInFlightFrameMaterialBufferData:array[0..MaxInFlightFrames-1] of TMaterialBufferData;
       fInFlightFrameMaterialBufferDataOffsets:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fInFlightFrameMaterialBufferDataSizes:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fInFlightFrameMaterialBufferDataMinMaterialID:array[0..MaxInFlightFrames-1] of TpvUInt32;
       fInFlightFrameMaterialBufferDataGeneration:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fInFlightFrameMaterialBufferDataUploadedGeneration:array[0..MaxInFlightFrames-1] of TpvUInt64;
//     fVulkanMaterialDataStagingBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanMaterialDataBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanMaterialUniformBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fTechniques:TpvTechniques;
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
       fMaterialIDManager:TIDManager;
       fMaterialIDHashMap:TMaterialIDHashMap;
       fMaterialIDDirtyMap:TMaterialIDDirtyMap;
       fMaterialIDMap:TMaterialIDMap;
       fMaterialHashMap:TMaterialHashMap;
       fEmptyMaterial:TpvScene3D.TMaterial;
       fMaterialDataMaterialGenerations:TMaterialGenerations;
       fMaterialDataProcessedGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fMaterialDataProcessedMinChangedID:array[0..MaxInFlightFrames-1] of TpvUInt32;
       fMaterialDataProcessedMaxChangedID:array[0..MaxInFlightFrames-1] of TpvUInt32;
       fMaterialDataGeneration:TpvUInt64;
       fMaterialDataProcessedGeneration:TpvUInt64;
       fMaterialDataGenerationLock:TPasMPSpinLock;
       fLights:array[0..MaxInFlightFrames-1] of TpvScene3D.TLights;
       fCountLights:array[0..MaxInFlightFrames-1] of TpvSizeInt;
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
       fInFlightFrameBufferMemoryBarriers:TInFlightFrameBufferMemoryBarriers;
       fPreviousViews:TViews;
       fViews:TViews;
       fVertexStagePushConstants:array[0..MaxRenderPassIndices-1] of TpvScene3D.TVertexStagePushConstants;
       fSetGlobalResourcesDone:array[0..MaxRenderPassIndices-1] of boolean;
       fCountInFlightFrames:TpvSizeInt;
       fUseBufferDeviceAddress:boolean;
       fHasTransmission:boolean;
       fVulkanBufferCopyBatchItemArrays:array[0..MaxInFlightFrames-1] of TpvVulkanBufferCopyBatchItemArray;
       fImageInfos:TpvScene3D.TImageInfos;
       fInFlightFrameImageInfos:array[0..MaxInFlightFrames-1] of TpvScene3D.TImageInfos;
       fInFlightFrameImageInfoImageDescriptorGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fInFlightFrameImageInfoImageDescriptorUploadedGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fRenderPassIndexCounter:TPasMPInt32;
       fPrimaryLightDirection:TpvVector3;
       procedure NewImageDescriptorGeneration;
       procedure NewMaterialDataGeneration;
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
                                            var aLightItemArray:TpvScene3D.TLightItems;
                                            var aLightMetaInfoArray:TpvScene3D.TLightMetaInfos);
       function GetLightUserDataIndex(const aUserData:TpvPtrInt):TpvUInt32;
       procedure SetGlobalResources(const aCommandBuffer:TpvVulkanCommandBuffer;
                                    const aPipelineLayout:TpvVulkanPipelineLayout;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aInFlightFrameIndex:TpvSizeInt);
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil;const aVulkanDevice:TpvVulkanDevice=nil;const aUseBufferDeviceAddress:boolean=true;const aCountInFlightFrames:TpvSizeInt=MaxInFlightFrames); reintroduce;
       destructor Destroy; override;
       procedure Upload;
       procedure Unload;
       procedure ResetRenderPasses;
       function AcquireRenderPassIndex:TpvSizeInt;
       procedure Check(const aInFlightFrameIndex:TpvSizeInt);
       procedure Update(const aInFlightFrameIndex:TpvSizeInt);
       procedure PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
       procedure ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
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
                         const aFrustumCulling:boolean=true;
                         const aPotentiallyVisibleSetCulling:boolean=true);
       procedure UpdateCachedVertices(const aPipeline:TpvVulkanPipeline;
                                      const aInFlightFrameIndex:TpvSizeInt;
                                      const aCommandBuffer:TpvVulkanCommandBuffer;
                                      const aPipelineLayout:TpvVulkanPipelineLayout);
       function NeedFlush(const aInFlightFrameIndex:TpvSizeInt):boolean;
       function Flush(const aInFlightFrameIndex:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer):boolean;
       procedure Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                      const aPreviousInFlightFrameIndex:TpvSizeInt;
                      const aInFlightFrameIndex:TpvSizeInt;
                      const aRenderPassIndex:TpvSizeInt;
                      const aViewBaseIndex:TpvSizeInt;
                      const aCountViews:TpvSizeInt;
                      const aFrameIndex:TpvSizeInt;
                      const aCommandBuffer:TpvVulkanCommandBuffer;
                      const aPipelineLayout:TpvVulkanPipelineLayout;
                      const aOnSetRenderPassResources:TOnSetRenderPassResources;
                      const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask];
                      const aJitter:PpvVector4=nil);
       procedure GetZNearZFar(const aViewMatrix:TpvMatrix4x4;
                              const aAspectRatio:TpvScalar;
                              out aZNear:TpvScalar;
                              out aZFar:TpvScalar);
       procedure InitializeGraphicsPipeline(const aPipeline:TpvVulkanGraphicsPipeline;const aWithPreviousPosition:boolean=false);
      public
       property BoundingBox:TpvAABB read fBoundingBox;
       property InFlightFrameBoundingBoxes:TInFlightFrameAABBs read fInFlightFrameBoundingBoxes;
       property GlobalVulkanViewUniformBuffers:TGlobalVulkanViewUniformBuffers read fGlobalVulkanViewUniformBuffers;
       property Views:TViews read fViews;
       property PrimaryLightDirection:TpvVector3 read fPrimaryLightDirection write fPrimaryLightDirection;
       property LightBuffers:TpvScene3D.TLightBuffers read fLightBuffers;
      published
       property PotentiallyVisibleSet:TpvScene3D.TPotentiallyVisibleSet read fPotentiallyVisibleSet;
       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;
       property MeshComputeVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fMeshComputeVulkanDescriptorSetLayout;
       property GlobalVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout read fGlobalVulkanDescriptorSetLayout;
       property HasTransmission:boolean read fHasTransmission;
       property UseBufferDeviceAddress:boolean read fUseBufferDeviceAddress write fUseBufferDeviceAddress;
       property CountInFlightFrames:TpvSizeInt read fCountInFlightFrames;
       property BufferStreamingMode:TBufferStreamingMode read fBufferStreamingMode write fBufferStreamingMode;
     end;

implementation

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

{if assigned(fSceneInstance) then begin
  ReleaseFrameDelay:=Max(MaxInFlightFrames+1,fSceneInstance.fCountInFlightFrames+1);
 end else begin
  ReleaseFrameDelay:=MaxInFlightFrames+1;
 end;}
 ReleaseFrameDelay:=(MaxInFlightFrames*2)+1;

 fUploaded:=false;

 fInUpload:=false;

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

procedure TpvScene3D.TBaseObject.PrepareDeferredFree;
begin
 Remove;
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

{ TpvScene3D.TBakedMesh.TTriangle }

procedure TpvScene3D.TBakedMesh.TTriangle.Assign(const aFrom:TpvScene3D.TBakedMesh.TTriangle);
begin
 Positions:=aFrom.Positions;
 Normals:=aFrom.Normals;
 Normal:=aFrom.Normal;
 Flags:=aFrom.Flags;
 MetaFlags:=aFrom.MetaFlags;
end;

function TpvScene3D.TBakedMesh.TTriangle.RayIntersection(const aRayOrigin,aRayDirection:TpvVector3;var aTime,aU,v:TpvScalar):boolean;
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

 t.x:=aRayOrigin.x-Positions[0].x;
 t.y:=aRayOrigin.y-Positions[0].y;
 t.z:=aRayOrigin.z-Positions[0].z;

 aU:=(t.x*p.x)+(t.y*p.y)+(t.z*p.z);
 if (aU<0.0) or (aU>Determinant) then begin
  exit;
 end;

 q.x:=(t.y*e0.z)-(t.z*e0.y);
 q.y:=(t.z*e0.x)-(t.x*e0.z);
 q.z:=(t.x*e0.y)-(t.y*e0.x);

 v:=(aRayDirection.x*q.x)+(aRayDirection.y*q.y)+(aRayDirection.z*q.z);
 if (v<0.0) or ((aU+v)>Determinant) then begin
  exit;
 end;

 aTime:=(e1.x*q.x)+(e1.y*q.y)+(e1.z*q.z);
 if abs(Determinant)<EPSILON then begin
  Determinant:=0.01;
 end;
 InverseDeterminant:=1.0/Determinant;
 aTime:=aTime*InverseDeterminant;
 aU:=aU*InverseDeterminant;
 v:=v*InverseDeterminant;

 result:=true;
end;


{ TpvScene3D.TBakedMesh }

constructor TpvScene3D.TBakedMesh.Create;
begin
 inherited Create;
 fTriangles:=TpvScene3D.TBakedMesh.TTriangles.Create;
 fTriangles.OwnsObjects:=true;
end;

destructor TpvScene3D.TBakedMesh.Destroy;
begin
 FreeAndNil(fTriangles);
 inherited Destroy;
end;

procedure TpvScene3D.TBakedMesh.Combine(const aWith:TBakedMesh);
var SrcTriangle,NewTriangle:TpvScene3D.TBakedMesh.TTriangle;
begin
 for SrcTriangle in aWith.fTriangles do begin
  NewTriangle:=TpvScene3D.TBakedMesh.TTriangle.Create;
  try
   fTriangles.Add(SrcTriangle);
  finally
   FreeAndNil(NewTriangle);
  end;
 end;
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

 fSubdivisonOneDimensionSize:=16;

 fManualBoundingBoxes:=TpvScene3D.TPotentiallyVisibleSet.TManualBoundingBoxes.Create;

 fBitmapOneDimensionSize:=0;

 fBitmapSize:=0;

 fBitmap:=nil;

 fNodes:=TpvScene3D.TPotentiallyVisibleSet.TNodes.Create;
 fNodes.OwnsObjects:=true;

 FillChar(fViewNodeIndices,SizeOf(TViewNodeIndices),#$ff);

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
    fNodes.Add(TpvScene3D.TPotentiallyVisibleSet.TNode.Create(self,nil));
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
    Time,HitTime:TpvScalar;
    HitUserData:TpvUInt32;
    Hit:boolean;
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

   RayOrigin:=TapA;
   RayDirection:=(TapB-TapA).Normalize;
   if NodeB.fAABB.RayIntersection(RayOrigin,RayDirection,Time) then begin
    Hit:=fTriangleDynamicAABBTreeSkipList.RayCast(RayOrigin,RayDirection,HitTime,HitUserData,true,RayCastTriangle);
    if (Hit and (HitTime>=(Time-EPSILON))) or not Hit then begin
     SetNodeVisibility(NodeAIndex,NodeBIndex,true);
     SetNodeVisibility(NodeBIndex,NodeAIndex,true);
     break;
    end;
   end;

   RayOrigin:=TapB;
   RayDirection:=(TapA-TapB).Normalize;
   if NodeA.fAABB.RayIntersection(RayOrigin,RayDirection,Time) then begin
    Hit:=fTriangleDynamicAABBTreeSkipList.RayCast(RayOrigin,RayDirection,HitTime,HitUserData,true,RayCastTriangle);
    if (Hit and (HitTime>=(Time-EPSILON))) or not Hit then begin
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
    BakedTriangle:TpvScene3D.TBakedMesh.TTriangle;
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

   fTriangleDynamicAABBTree:=TpvBVHDynamicAABBTree.Create;
   try

    for TriangleIndex:=0 to fBakedMesh.Triangles.Count-1 do begin
     BakedTriangle:=fBakedMesh.Triangles[TriangleIndex];
     TemporaryAABB.Min.x:=Min(Min(BakedTriangle.Positions[0].x,BakedTriangle.Positions[1].x),BakedTriangle.Positions[2].x);
     TemporaryAABB.Min.y:=Min(Min(BakedTriangle.Positions[0].y,BakedTriangle.Positions[1].y),BakedTriangle.Positions[2].y);
     TemporaryAABB.Min.z:=Min(Min(BakedTriangle.Positions[0].z,BakedTriangle.Positions[1].z),BakedTriangle.Positions[2].z);
     TemporaryAABB.Max.x:=Max(Max(BakedTriangle.Positions[0].x,BakedTriangle.Positions[1].x),BakedTriangle.Positions[2].x);
     TemporaryAABB.Max.y:=Max(Max(BakedTriangle.Positions[0].y,BakedTriangle.Positions[1].y),BakedTriangle.Positions[2].y);
     TemporaryAABB.Max.z:=Max(Max(BakedTriangle.Positions[0].z,BakedTriangle.Positions[1].z),BakedTriangle.Positions[2].z);
     fTriangleDynamicAABBTree.CreateProxy(TemporaryAABB,TriangleIndex+1);
    end;

    fTriangleDynamicAABBTree.Rebuild;

    fTriangleDynamicAABBTreeSkipList:=TpvBVHDynamicAABBTree.TSkipList.Create(fTriangleDynamicAABBTree,nil);
    try

     if fTriangleDynamicAABBTreeSkipList.NodeArray.Count>0 then begin

      fAABB.Min:=fTriangleDynamicAABBTreeSkipList.NodeArray.Items[0].AABBMin;
      fAABB.Max:=fTriangleDynamicAABBTreeSkipList.NodeArray.Items[0].AABBMax;

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
            NewStackItem.Node.fTag:=DynamicAABBTree.Nodes[StackItem.NodeIndex].UserData-1;
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
         NewStackItem.NodeIndex:=fTriangleDynamicAABBTree.Root;
         NewStackItem.MetaData:=0;
         Stack.Push(NewStackItem);
         while Stack.Pop(StackItem) do begin
          if (not assigned(StackItem.Node)) or (StackItem.Node.fLevel<aMaxDepth) then begin
           NewStackItem.Node:=TpvScene3D.TPotentiallyVisibleSet.TNode.Create(self,StackItem.Node);
           NewStackItem.Node.fTag:=fTriangleDynamicAABBTree.Nodes[StackItem.NodeIndex].UserData-1;
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
           NewStackItem.Node.fAABB:=fTriangleDynamicAABBTree.Nodes[StackItem.NodeIndex].AABB;
           if fTriangleDynamicAABBTree.Nodes[StackItem.NodeIndex].Children[1]>=0 then begin
            NewStackItem.NodeIndex:=fTriangleDynamicAABBTree.Nodes[StackItem.NodeIndex].Children[1];
            NewStackItem.MetaData:=1;
            Stack.Push(NewStackItem);
           end;
           if fTriangleDynamicAABBTree.Nodes[StackItem.NodeIndex].Children[0]>=0 then begin
            NewStackItem.NodeIndex:=fTriangleDynamicAABBTree.Nodes[StackItem.NodeIndex].Children[0];
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
        if assigned(aPasMPInstance) then begin
         fPasMPInstance:=aPasMPInstance;
        end else begin
         fPasMPInstance:=TPasMP.GetGlobalInstance;
        end;
        fPasMPInstance.Invoke(fPasMPInstance.ParallelFor(NodeIndexPairList,0,NodeIndexPairList.Count-1,NodePairVisibilityCheckParallelForJob,1,PasMPDefaultDepth,nil,0,0));
       end;
      finally
       FreeAndNil(NodeIndexPairList);
      end;

     end;

    finally
     FreeAndNil(fTriangleDynamicAABBTreeSkipList);
    end;

   finally
    FreeAndNil(fTriangleDynamicAABBTree);
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
    fSceneInstance.NewImageDescriptorGeneration;
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
           case fKind of
            TpvScene3D.TImage.TKind.WhiteTexture:begin
             fTexture:=TpvVulkanTexture.CreateFromMemory(fSceneInstance.fVulkanDevice,
                                                         UniversalQueue,
                                                         UniversalCommandBuffer,
                                                         UniversalFence,
                                                         UniversalQueue,
                                                         UniversalCommandBuffer,
                                                         UniversalFence,
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
             fTexture:=TpvVulkanTexture.CreateFromMemory(fSceneInstance.fVulkanDevice,
                                                         UniversalQueue,
                                                         UniversalCommandBuffer,
                                                         UniversalFence,
                                                         UniversalQueue,
                                                         UniversalCommandBuffer,
                                                         UniversalFence,
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
             fTexture:=TpvVulkanTexture.CreateFromImage(fSceneInstance.fVulkanDevice,
                                                        UniversalQueue,
                                                        UniversalCommandBuffer,
                                                        UniversalFence,
                                                        UniversalQueue,
                                                        UniversalCommandBuffer,
                                                        UniversalFence,
                                                        fResourceDataStream,
                                                        true,
                                                        false,
                                                        true);
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

function TpvScene3D.TImage.BeginLoad(const aStream:TStream):boolean;
begin
 result:=false;
 if assigned(aStream) then begin
  try
   aStream.Seek(0,soBeginning);
   fResourceDataStream.CopyFrom(aStream,aStream.Size);
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

procedure TpvScene3D.TTexture.AssignFromGLTF(const aSourceDocument:TPasGLTF.TDocument;const aSourceTexture:TPasGLTF.TTexture;const aImageMap:TImages;const aSamplerMap:TSamplers);
begin

 fName:=aSourceTexture.Name;

 fSceneInstance.fTextureListLock.Acquire;
 try

  fSceneInstance.fImageListLock.Acquire;
  try
   if (aSourceTexture.Source>=0) and (aSourceTexture.Source<aImageMap.Count) then begin
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
  else begin
   result:=nil;
  end;
 end;
end;

{ TpvScene3D.TMaterial }

constructor TpvScene3D.TMaterial.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);

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
   if (fID>0) and (fID<$10000) then begin
    fSceneInstance.fMaterialIDMap[fID]:=self;
    TPasMPInterlocked.BitwiseOr(fSceneInstance.fMaterialIDDirtyMap[fID shr 5],TPasMPUInt32(1) shl (fID and 31));
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
      TPasMPInterlocked.BitwiseOr(fSceneInstance.fMaterialIDDirtyMap[fID shr 5],TPasMPUInt32(1) shl (fID and 31));
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
    fSceneInstance.NewMaterialDataGeneration;
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

 finally
  fSceneInstance.fTextureListLock.Release;
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

 TPasMPInterlocked.Increment(fGeneration);

 if assigned(fSceneInstance) and (fID>0) and (fID<$10000) then begin
  TPasMPInterlocked.BitwiseOr(fSceneInstance.fMaterialIDDirtyMap[fID shr 5],TPasMPUInt32(1) shl (fID and 31));
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

    end;

    else begin
     Assert(false);
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

procedure TpvScene3D.TLightBuffer.PrepareGPUUpdate;
begin
end;

procedure TpvScene3D.TLightBuffer.ExecuteGPUUpdate;
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

{    fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fLightItemsVulkanBuffer);
     fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fLightTreeVulkanBuffer);
     fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fLightMetaInfoVulkanBuffer);}

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
{     fSceneInstance.fVulkanLightItemsStagingBuffers[fInFlightFrameIndex].UpdateData(fLightItems.Items[0],0,Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightItem),FlushUpdateData);
      fLightItemsVulkanBuffer.CopyFrom(fSceneInstance.fVulkanBufferCopyBatchItemArrays[fInFlightFrameIndex],
                                       fSceneInstance.fVulkanLightItemsStagingBuffers[fInFlightFrameIndex],
                                       0,
                                       0,
                                       Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightItem));}
     end;
     if fLightTree.Count>0 then begin
      fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                        fSceneInstance.fVulkanStagingCommandBuffer,
                                                        fSceneInstance.fVulkanStagingFence,
                                                        fLightTree.Items[0],
                                                        fLightTreeVulkanBuffer,
                                                        0,
                                                        Min(fLightTree.Count,MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode));
{     fSceneInstance.fVulkanLightTreeStagingBuffers[fInFlightFrameIndex].UpdateData(fLightTree.Items[0],0,Min(fLightTree.Count,MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode),FlushUpdateData);
      fLightTreeVulkanBuffer.CopyFrom(fSceneInstance.fVulkanBufferCopyBatchItemArrays[fInFlightFrameIndex],
                                      fSceneInstance.fVulkanLightTreeStagingBuffers[fInFlightFrameIndex],
                                      0,
                                      0,
                                      Min(fLightTree.Count,MaxVisibleLights*4)*SizeOf(TpvBVHDynamicAABBTree.TSkipListNode));}
     end else begin
      fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                        fSceneInstance.fVulkanStagingCommandBuffer,
                                                        fSceneInstance.fVulkanStagingFence,
                                                        EmptyGPUSkipListNode,
                                                        fLightTreeVulkanBuffer,
                                                        0,
                                                        SizeOf(TpvBVHDynamicAABBTree.TSkipListNode));
{     fSceneInstance.fVulkanLightTreeStagingBuffers[fInFlightFrameIndex].UpdateData(EmptyGPUSkipListNode,0,SizeOf(TpvBVHDynamicAABBTree.TSkipListNode),FlushUpdateData);
      fLightTreeVulkanBuffer.CopyFrom(fSceneInstance.fVulkanBufferCopyBatchItemArrays[fInFlightFrameIndex],
                                      fSceneInstance.fVulkanLightTreeStagingBuffers[fInFlightFrameIndex],
                                      0,
                                      0,
                                      SizeOf(TpvBVHDynamicAABBTree.TSkipListNode));}
     end;
     if fLightItems.Count>0 then begin
      fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fSceneInstance.fVulkanStagingQueue,
                                                        fSceneInstance.fVulkanStagingCommandBuffer,
                                                        fSceneInstance.fVulkanStagingFence,
                                                        fLightMetaInfos[0],
                                                        fLightMetaInfoVulkanBuffer,
                                                        0,
                                                        Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightMetaInfo));
{     fSceneInstance.fVulkanLightMetaInfoStagingBuffers[fInFlightFrameIndex].UpdateData(fLightMetaInfos[0],0,Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightMetaInfo),FlushUpdateData);
      fLightMetaInfoVulkanBuffer.CopyFrom(fSceneInstance.fVulkanBufferCopyBatchItemArrays[fInFlightFrameIndex],
                                          fSceneInstance.fVulkanLightMetaInfoStagingBuffers[fInFlightFrameIndex],
                                          0,
                                          0,
                                          Min(fLightItems.Count,MaxVisibleLights)*SizeOf(TpvScene3D.TLightMetaInfo));}
     end;
    end;

    else begin
     Assert(false);
    end;

   end;

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
    Target:TAnimation.TChannel.TTarget;
    TextureRawIndex:TpvScene3D.TTextureIndex;
begin

 fName:=aSourceAnimation.Name;

 SetLength(fChannels,aSourceAnimation.Channels.Count);

 for ChannelIndex:=0 to aSourceAnimation.Channels.Count-1 do begin

  SourceAnimationChannel:=aSourceAnimation.Channels[ChannelIndex];

  DestinationAnimationChannel:=@fChannels[ChannelIndex];

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
         if (length(TargetPointerStrings)>4) and
            ((TargetPointerStrings[length(TargetPointerStrings)-3]='extensions') and
             (TargetPointerStrings[length(TargetPointerStrings)-2]='KHR_texture_transform') and
             ((TargetPointerStrings[length(TargetPointerStrings)-1]='offset') or
              (TargetPointerStrings[length(TargetPointerStrings)-1]='scale') or
              (TargetPointerStrings[length(TargetPointerStrings)-1]='rotation'))) then begin
          if TargetPointerStrings[length(TargetPointerStrings)-1]='offset' then begin
           Target:=TAnimation.TChannel.TTarget.PointerTextureOffset;
          end else if TargetPointerStrings[length(TargetPointerStrings)-1]='scale' then begin
           Target:=TAnimation.TChannel.TTarget.PointerTextureScale;
          end else{if TargetPointerStrings[length(TargetPointerStrings)-1]='rotation' then}begin
           Target:=TAnimation.TChannel.TTarget.PointerTextureRotation;
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
             end;
            end;
           end;
          end;
          if TextureRawIndex<>TpvScene3D.TTextureIndex.None then begin
           DestinationAnimationChannel.Target:=Target;
           DestinationAnimationChannel.TargetSubIndex:=TpvSizeInt(TextureRawIndex);
          end;
         end else if TargetPointerStrings[2]='pbrMetallicRoughness' then begin
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
          DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialEmissiveFactor;
         end else if TargetPointerStrings[2]='normalTexture' then begin
          if length(TargetPointerStrings)>3 then begin
           if TargetPointerStrings[3]='scale' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialNormalTextureScale;
           end;
          end;
         end else if TargetPointerStrings[2]='occlusionTexture' then begin
          if length(TargetPointerStrings)>3 then begin
           if TargetPointerStrings[3]='strength' then begin
            DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialOcclusionTextureStrength;
           end;
          end;
         end else if TargetPointerStrings[2]='extensions' then begin
          if length(TargetPointerStrings)>3 then begin
           if TargetPointerStrings[3]='KHR_materials_emissive_strength' then begin
            if length(TargetPointerStrings)>4 then begin
             if TargetPointerStrings[4]='emissiveStrength' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialEmissiveStrength;
             end;
            end;
           end else if TargetPointerStrings[3]='KHR_materials_ior' then begin
            if length(TargetPointerStrings)>4 then begin
             if TargetPointerStrings[4]='ior' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialIOR;
             end;
            end;
           end else if TargetPointerStrings[3]='KHR_materials_transmission' then begin
            if length(TargetPointerStrings)>4 then begin
             if TargetPointerStrings[4]='transmissionFactor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRTransmissionFactor;
             end;
            end;
           end else if TargetPointerStrings[3]='KHR_materials_iridescence' then begin
            if length(TargetPointerStrings)>4 then begin
             if TargetPointerStrings[4]='iridescenceFactor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceFactor;
             end else if TargetPointerStrings[4]='iridescenceIor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceIor;
             end else if TargetPointerStrings[4]='iridescenceThicknessMinimum' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMinimum;
             end else if TargetPointerStrings[4]='iridescenceThicknessMaximum' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRIridescenceMaximum;
             end;
            end;
           end else if TargetPointerStrings[3]='KHR_materials_volume' then begin
            if length(TargetPointerStrings)>4 then begin
             if TargetPointerStrings[4]='attenuationColor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationColor;
             end else if TargetPointerStrings[4]='attenuationDistance' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeAttenuationDistance;
             end else if TargetPointerStrings[4]='thicknessFactor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRVolumeThicknessFactor;
             end;
            end;
           end else if TargetPointerStrings[3]='KHR_materials_sheen' then begin
            if length(TargetPointerStrings)>4 then begin
             if TargetPointerStrings[4]='sheenColorFactor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSheenColorFactor;
             end else if TargetPointerStrings[4]='sheenRoughnessFactor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSheenRoughnessFactor;
             end;
            end;
           end else if TargetPointerStrings[3]='KHR_materials_specular' then begin
            if length(TargetPointerStrings)>4 then begin
             if TargetPointerStrings[4]='specularFactor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularFactor;
             end else if TargetPointerStrings[4]='specularColorFactor' then begin
              DestinationAnimationChannel^.Target:=TAnimation.TChannel.TTarget.PointerMaterialPBRSpecularColorFactor;
             end;
            end;
           end else if TargetPointerStrings[3]='KHR_materials_transform' then begin
            // TODO
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
     fGroup.fSceneInstance.fMaterialListLock.Acquire;
     try
      Primitive^.Material.DecRef;
     finally
      fGroup.fSceneInstance.fMaterialListLock.Release;
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

 fChildren:=TNodes.Create;
 fChildren.OwnsObjects:=false;

 fUsedByScenesList:=TUsedByScenesList.Create;
 fUsedByScenesList.OwnsObjects:=false;

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

 FreeAndNil(fUsedByScenesList);

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

{ TpvScene3D.TGroup.TDrawChoreographyBatchItem }

function TpvScene3D.TGroup.TDrawChoreographyBatchItem.CompareTo(const aOther:TpvScene3D.TGroup.TDrawChoreographyBatchItem):TpvInt32;
begin
 result:=Sign(TpvInt32(fAlphaMode)-TpvInt32(aOther.fAlphaMode));
 if result=0 then begin
  result:=Sign(TpvInt32(fPrimitiveTopology)-TpvInt32(aOther.fPrimitiveTopology));
  if result=0 then begin
   result:=Sign(TpvInt32(ord(fDoubleSided) and 1)-TpvInt32(ord(aOther.fDoubleSided) and 1));
   if result=0 then begin
    result:=Sign(TpvPtrInt(fMaterial)-TpvPtrInt(aOther.fMaterial));
    if result=0 then begin
     result:=Sign(TpvPtrInt(fNode)-TpvPtrInt(aOther.fNode));
     if result=0 then begin
      result:=Sign(TpvPtrInt(fMesh)-TpvPtrInt(aOther.fMesh));
      if result=0 then begin
       result:=Sign(MeshPrimitive-aOther.MeshPrimitive);
       if result=0 then begin
        result:=Sign(fStartIndex-aOther.fStartIndex);
        if result=0 then begin
         result:=Sign(fCountIndices-aOther.fCountIndices);
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
end;

{ TpvScene3D.TGroup.TDrawChoreographyBatchItems }

procedure TpvScene3D.TGroup.TDrawChoreographyBatchItems.Sort;
type PStackItem=^TStackItem;
     TStackItem=record
      Left,Right,Depth:TpvSizeInt;
     end;
var Left,Right,Depth,i,j,Middle,Size,Parent,Child,Pivot,iA,iB,iC:TpvSizeInt;
    StackItem:PStackItem;
    Stack:array[0..31] of TStackItem;
begin
 if Count>1 then begin
  StackItem:=@Stack[0];
  StackItem^.Left:=0;
  StackItem^.Right:=Count-1;
  StackItem^.Depth:=IntLog2(Count) shl 1;
  inc(StackItem);
  while TpvPtrUInt(TpvPointer(StackItem))>TpvPtrUInt(TpvPointer(@Stack[0])) do begin
   dec(StackItem);
   Left:=StackItem^.Left;
   Right:=StackItem^.Right;
   Depth:=StackItem^.Depth;
   Size:=(Right-Left)+1;
   if Size<16 then begin
    // Insertion sort
    iA:=Left;
    iB:=iA+1;
    while iB<=Right do begin
     iC:=iB;
     while (iA>=Left) and
           (iC>=Left) and
           (Items[iA].CompareTo(Items[iC])>0) do begin
      Exchange(iA,iC);
      dec(iA);
      dec(iC);
     end;
     iA:=iB;
     inc(iB);
    end;
   end else begin
    if (Depth=0) or (TpvPtrUInt(TpvPointer(StackItem))>=TpvPtrUInt(TpvPointer(@Stack[high(Stack)-1]))) then begin
     // Heap sort
     i:=Size div 2;
     repeat
      if i>0 then begin
       dec(i);
      end else begin
       dec(Size);
       if Size>0 then begin
        Exchange(Left+Size,Left);
       end else begin
        break;
       end;
      end;
      Parent:=i;
      repeat
       Child:=(Parent*2)+1;
       if Child<Size then begin
        if (Child<(Size-1)) and (Items[Left+Child].CompareTo(Items[Left+Child+1])<0) then begin
         inc(Child);
        end;
        if Items[Left+Parent].CompareTo(Items[Left+Child])<0 then begin
         Exchange(Left+Parent,Left+Child);
         Parent:=Child;
         continue;
        end;
       end;
       break;
      until false;
     until false;
    end else begin
     // Quick sort width median-of-three optimization
     Middle:=Left+((Right-Left) shr 1);
     if (Right-Left)>3 then begin
      if Items[Left].CompareTo(Items[Middle])>0 then begin
       Exchange(Left,Middle);
      end;
      if Items[Left].CompareTo(Items[Right])>0 then begin
       Exchange(Left,Right);
      end;
      if Items[Middle].CompareTo(Items[Right])>0 then begin
       Exchange(Middle,Right);
      end;
     end;
     Pivot:=Middle;
     i:=Left;
     j:=Right;
     repeat
      while (i<Right) and (Items[i].CompareTo(Items[Pivot])<0) do begin
       inc(i);
      end;
      while (j>=i) and (Items[j].CompareTo(Items[Pivot])>0) do begin
       dec(j);
      end;
      if i>j then begin
       break;
      end else begin
       if i<>j then begin
        Exchange(i,j);
        if Pivot=i then begin
         Pivot:=j;
        end else if Pivot=j then begin
         Pivot:=i;
        end;
       end;
       inc(i);
       dec(j);
      end;
     until false;
     if i<Right then begin
      StackItem^.Left:=i;
      StackItem^.Right:=Right;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
     if Left<j then begin
      StackItem^.Left:=Left;
      StackItem^.Right:=j;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
    end;
   end;
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
 fDrawChoreographyBatchItems:=TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchItems.OwnsObjects:=false;
 fDrawChoreographyBatchUniqueItems:=TDrawChoreographyBatchItems.Create;
 fDrawChoreographyBatchUniqueItems.OwnsObjects:=false;
end;

destructor TpvScene3D.TGroup.TScene.Destroy;
begin
 FreeAndNil(fDrawChoreographyBatchItems);
 FreeAndNil(fDrawChoreographyBatchUniqueItems);
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

 fMaterialsToDuplicate:=TpvScene3D.TGroup.TMaterialsToDuplicate.Create;
 fMaterialsToDuplicate.OwnsObjects:=false;

 fMaterials:=TpvScene3D.TMaterials.Create;
 fMaterials.OwnsObjects:=false;

 fMaterialMap:=nil;

 fMaterialIDMapArrayIndexHashMap:=TpvScene3D.TGroup.TMaterialIDMapArrayIndexHashMap.Create(-1);

 fMaterialIDMapArrays:=TpvScene3D.TGroup.TMaterialIDMapArrays.Create;
 fMaterialIDMapArrays.OwnsObjects:=true;

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

 fDrawChoreographyBatchCondensedIndices.Initialize;

 fDrawChoreographyBatchCondensedUniqueIndices.Initialize;

 fMorphTargetVertices.Initialize;

 fJointBlocks.Initialize;

 fJointBlockOffsets:=nil;

 fSkinStorageBufferSize:=0;

 fMorphTargetCount:=0;

 fCulling:=false;

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
 FreeAndNil(fMaterials);

 FreeAndNil(fMaterialsToDuplicate);

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

  fVulkanVertexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                              fVertices.Count*SizeOf(TVertex),
                                              TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
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
  fSceneInstance.fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                    UniversalCommandBuffer,
                                                    UniversalFence,
                                                    fVertices.Items[0],
                                                    fVulkanVertexBuffer,
                                                    0,
                                                    fVertices.Count*SizeOf(TVertex));
{ fVulkanVertexBuffer.UploadData(UniversalQueue,
                                 UniversalCommandBuffer,
                                 UniversalFence,
                                 fVertices.Items[0],
                                 0,
                                 fVertices.Count*SizeOf(TVertex),
                                 TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);}

  for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
   fVulkanCachedVertexBuffers[Index]:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                             fVertices.Count*SizeOf(TCachedVertex),
                                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
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
   fSceneInstance.fVulkanDevice.MemoryStaging.Zero(UniversalQueue,
                                                   UniversalCommandBuffer,
                                                   UniversalFence,
                                                   fVulkanCachedVertexBuffers[Index],
                                                   0,
                                                   fVertices.Count*SizeOf(TCachedVertex));
{  fVulkanCachedVertexBuffers[Index].ClearData(UniversalQueue,
                                               UniversalCommandBuffer,
                                               UniversalFence,
                                               0,
                                               fVertices.Count*SizeOf(TCachedVertex),
                                               TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);}
  end;

{ fVulkanIndexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                             fIndices.Count*SizeOf(TVkUInt32),
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
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
  fVulkanIndexBuffer.UploadData(UniversalQueue,
                                UniversalCommandBuffer,
                                UniversalFence,
                                fIndices.Items[0],
                                0,
                                fIndices.Count*SizeOf(TVkUInt32),
                                TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);//}

  fVulkanDrawIndexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                 fDrawChoreographyBatchCondensedIndices.Count*SizeOf(TVkUInt32),
                                                 TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
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
  fSceneInstance.fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                    UniversalCommandBuffer,
                                                    UniversalFence,
                                                    fDrawChoreographyBatchCondensedIndices.Items[0],
                                                    fVulkanDrawIndexBuffer,
                                                    0,
                                                    fDrawChoreographyBatchCondensedIndices.Count*SizeOf(TVkUInt32));
{ fVulkanDrawIndexBuffer.UploadData(UniversalQueue,
                                    UniversalCommandBuffer,
                                    UniversalFence,
                                    fDrawChoreographyBatchCondensedIndices.Items[0],
                                    0,
                                    fDrawChoreographyBatchCondensedIndices.Count*SizeOf(TVkUInt32),
                                    TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);}

  fVulkanDrawUniqueIndexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                       fDrawChoreographyBatchCondensedUniqueIndices.Count*SizeOf(TVkUInt32),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
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
  fSceneInstance.fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                    UniversalCommandBuffer,
                                                    UniversalFence,
                                                    fDrawChoreographyBatchCondensedUniqueIndices.Items[0],
                                                    fVulkanDrawUniqueIndexBuffer,
                                                    0,
                                                    fDrawChoreographyBatchCondensedUniqueIndices.Count*SizeOf(TVkUInt32));
{ fVulkanDrawUniqueIndexBuffer.UploadData(UniversalQueue,
                                          UniversalCommandBuffer,
                                          UniversalFence,
                                          fDrawChoreographyBatchCondensedUniqueIndices.Items[0],
                                          0,
                                          fDrawChoreographyBatchCondensedUniqueIndices.Count*SizeOf(TVkUInt32),
                                          TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);}

  fVulkanMorphTargetVertexBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                         fMorphTargetVertices.Count*SizeOf(TMorphTargetVertex),
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
  fSceneInstance.fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                    UniversalCommandBuffer,
                                                    UniversalFence,
                                                    fMorphTargetVertices.Items[0],
                                                    fVulkanMorphTargetVertexBuffer,
                                                    0,
                                                    fMorphTargetVertices.Count*SizeOf(TMorphTargetVertex));
{ fVulkanMorphTargetVertexBuffer.UploadData(UniversalQueue,
                                            UniversalCommandBuffer,
                                            UniversalFence,
                                            fMorphTargetVertices.Items[0],
                                            0,
                                            fMorphTargetVertices.Count*SizeOf(TMorphTargetVertex),
                                            TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);}

  fVulkanJointBlockBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                  fJointBlocks.Count*SizeOf(TJointBlock),
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
  fSceneInstance.fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                    UniversalCommandBuffer,
                                                    UniversalFence,
                                                    fJointBlocks.Items[0],
                                                    fVulkanJointBlockBuffer,
                                                    0,
                                                    fJointBlocks.Count*SizeOf(TJointBlock));
{ fVulkanJointBlockBuffer.UploadData(UniversalQueue,
                                     UniversalCommandBuffer,
                                     UniversalFence,
                                     fJointBlocks.Items[0],
                                     0,
                                     fJointBlocks.Count*SizeOf(TJointBlock),
                                     TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);}

  case fSceneInstance.fBufferStreamingMode of

   TBufferStreamingMode.Direct:begin

{   for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin

     fVulkanNodeMatricesStagingBuffers[Index]:=nil;

     fVulkanMorphTargetVertexWeightsStagingBuffers[Index]:=nil;

    end;}

   end;

   TBufferStreamingMode.Staging:begin

{   for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin

     fVulkanNodeMatricesStagingBuffers[Index]:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                                      (fNodes.Count+fCountJointNodeMatrices+1)*SizeOf(TpvMatrix4x4),
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
                                                                      [TpvVulkanBufferFlag.PersistentMapped]
                                                                     );

     fVulkanMorphTargetVertexWeightsStagingBuffers[Index]:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                                                  (Max(Max(fMorphTargetCount,fCountNodeWeights),1))*SizeOf(TpvFloat),
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
                                                                                  [TpvVulkanBufferFlag.PersistentMapped]
                                                                                 );

    end;}

   end;

   else begin
    Assert(false);
   end;

  end;

 end;
var Instance:TpvScene3D.TGroup.TInstance;
begin
 if not fInUpload then begin
  fInUpload:=true;
  try
   if not fUploaded then begin
    fLock.Acquire;
    try
     if not fUploaded then begin
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
       fSceneInstance.NewImageDescriptorGeneration;
       fSceneInstance.NewMaterialDataGeneration;
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
     for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
      FreeAndNil(fVulkanCachedVertexBuffers[Index]);
     end;
//   FreeAndNil(fVulkanIndexBuffer);
     FreeAndNil(fVulkanDrawIndexBuffer);
     FreeAndNil(fVulkanDrawUniqueIndexBuffer);
     FreeAndNil(fVulkanMorphTargetVertexBuffer);
     FreeAndNil(fVulkanJointBlockBuffer);
{    for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
      FreeAndNil(fVulkanNodeMatricesStagingBuffers[Index]);
      FreeAndNil(fVulkanMorphTargetVertexWeightsStagingBuffers[Index]);
     end;}
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
        DrawChoreographyBatchItem.fAlphaMode:=Material.fData.AlphaMode;
        DrawChoreographyBatchItem.fPrimitiveTopology:=Primitive^.PrimitiveMode;
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

   Node:=DrawChoreographyBatchItem.fNode;

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

 finally
  IndexBitmap:=nil;
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
       for Index:=0 to aSourceDocument.Animations.Count-1 do begin
        SourceAnimation:=aSourceDocument.Animations[Index];
        Animation:=TAnimation.Create(self,Index);
        try
         Animation.AssignFromGLTF(aSourceDocument,SourceAnimation);
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
               end;
              end;
{             if (Channel^.Target in TpvScene3D.TGroup.TAnimation.TChannel.TextureTargets) and
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
var Image:TpvScene3D.TImage;
    Sampler:TpvScene3D.TSampler;
    Texture:TpvScene3D.TTexture;
begin

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

 ConstructDrawChoreographyBatchItems;

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
   fSceneInstance.NewImageDescriptorGeneration;
   fSceneInstance.NewMaterialDataGeneration;
  end;
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

procedure TpvScene3D.TGroup.PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.PrepareGPUUpdate(aInFlightFrameIndex);
 end;
end;

procedure TpvScene3D.TGroup.ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.ExecuteGPUUpdate(aInFlightFrameIndex);
 end;
end;

procedure TpvScene3D.TGroup.Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                    const aRenderPassIndex:TpvSizeInt;
                                    const aViewBaseIndex:TpvSizeInt;
                                    const aCountViews:TpvSizeInt;
                                    const aFrustums:TpvFrustumDynamicArray;
                                    const aPotentiallyVisibleSetCulling:boolean);
var Instance:TpvScene3D.TGroup.TInstance;
begin
 for Instance in fInstances do begin
  Instance.Prepare(aInFlightFrameIndex,
                   aRenderPassIndex,
                   aViewBaseIndex,
                   aCountViews,
                   aFrustums,
                   aPotentiallyVisibleSetCulling);
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
  aCommandBuffer.CmdBindIndexBuffer(fVulkanDrawIndexBuffer.Handle,0,TVkIndexType.VK_INDEX_TYPE_UINT32);
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
 result:=TpvScene3D.TGroup.TInstance.TNode.TNodeFlag.InverseFrontFaces in Flags;
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

{ TpvScene3D.TGroup.TInstance.TVulkanData }

constructor TpvScene3D.TGroup.TInstance.TVulkanData.Create(const aInstance:TGroup.TInstance);
begin
 inherited Create;
 fUploaded:=false;
 fInUpload:=false;
 fInstance:=aInstance;
 fNodeMatricesBuffer:=nil;
 fMorphTargetVertexWeightsBuffer:=nil;
 fNodeMatrices:=nil;
 fMorphTargetVertexWeights:=nil;
end;

destructor TpvScene3D.TGroup.TInstance.TVulkanData.Destroy;
begin
 Unload;
 fNodeMatrices:=nil;
 fMorphTargetVertexWeights:=nil;
 inherited Destroy;
end;

procedure TpvScene3D.TGroup.TInstance.TVulkanData.Upload;
begin

 if not fInUpload then begin

  fInUpload:=true;
  try

   if not fUploaded then begin

    try

     case fInstance.fSceneInstance.fBufferStreamingMode of

      TBufferStreamingMode.Direct:begin

       fNodeMatricesBuffer:=TpvVulkanBuffer.Create(fInstance.fSceneInstance.fVulkanDevice,
                                                   length(fInstance.fNodeMatrices)*SizeOf(TpvMatrix4x4),
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

       fMorphTargetVertexWeightsBuffer:=TpvVulkanBuffer.Create(fInstance.fSceneInstance.fVulkanDevice,
                                                               length(fInstance.fMorphTargetVertexWeights)*SizeOf(TpvFloat),
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

      end;

      TBufferStreamingMode.Staging:begin

       fNodeMatricesBuffer:=TpvVulkanBuffer.Create(fInstance.fSceneInstance.fVulkanDevice,
                                                   length(fInstance.fNodeMatrices)*SizeOf(TpvMatrix4x4),
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

       fMorphTargetVertexWeightsBuffer:=TpvVulkanBuffer.Create(fInstance.fSceneInstance.fVulkanDevice,
                                                               length(fInstance.fMorphTargetVertexWeights)*SizeOf(TpvFloat),
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

      end;

      else begin
       Assert(false);
      end;

     end;
    finally
     fUploaded:=true;
    end;
   end;

  finally
   fInUpload:=false;
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

procedure TpvScene3D.TGroup.TInstance.TVulkanData.PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
begin

 if length(fNodeMatrices)<>length(fInstance.fNodeMatrices) then begin
  SetLength(fNodeMatrices,length(fInstance.fNodeMatrices));
 end;
 if length(fNodeMatrices)>0 then begin
  Move(fInstance.fNodeMatrices[0],fNodeMatrices[0],length(fNodeMatrices)*SizeOf(TpvMatrix4x4));
 end;

 if length(fMorphTargetVertexWeights)<>length(fInstance.fMorphTargetVertexWeights) then begin
  SetLength(fMorphTargetVertexWeights,length(fInstance.fMorphTargetVertexWeights));
 end;
 if length(fMorphTargetVertexWeights)>0 then begin
  Move(fInstance.fMorphTargetVertexWeights[0],fMorphTargetVertexWeights[0],length(fMorphTargetVertexWeights)*SizeOf(TpvFloat));
 end;

end;

procedure TpvScene3D.TGroup.TInstance.TVulkanData.ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
begin
 Upload;
 if fUploaded then begin

  case fInstance.fSceneInstance.fBufferStreamingMode of

   TBufferStreamingMode.Direct:begin

    fNodeMatricesBuffer.UpdateData(fNodeMatrices[0],0,length(fNodeMatrices)*SizeOf(TpvMatrix4x4),FlushUpdateData);

    fMorphTargetVertexWeightsBuffer.UpdateData(fMorphTargetVertexWeights[0],0,length(fMorphTargetVertexWeights)*SizeOf(TpvFloat),FlushUpdateData);

   end;

   TBufferStreamingMode.Staging:begin

    fInstance.fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fInstance.fSceneInstance.fVulkanStagingQueue,
                                                                fInstance.fSceneInstance.fVulkanStagingCommandBuffer,
                                                                fInstance.fSceneInstance.fVulkanStagingFence,
                                                                fNodeMatrices[0],
                                                                fNodeMatricesBuffer,
                                                                0,
                                                                length(fNodeMatrices)*SizeOf(TpvMatrix4x4));

    fInstance.fSceneInstance.fVulkanDevice.MemoryStaging.Upload(fInstance.fSceneInstance.fVulkanStagingQueue,
                                                                fInstance.fSceneInstance.fVulkanStagingCommandBuffer,
                                                                fInstance.fSceneInstance.fVulkanStagingFence,
                                                                fMorphTargetVertexWeights[0],
                                                                fMorphTargetVertexWeightsBuffer,
                                                                0,
                                                                length(fMorphTargetVertexWeights)*SizeOf(TpvFloat));

{   fInstance.fGroup.fVulkanNodeMatricesStagingBuffers[aInFlightFrameIndex].UpdateData(fNodeMatrices[0],0,length(fNodeMatrices)*SizeOf(TpvMatrix4x4),FlushUpdateData);

    fNodeMatricesBuffer.CopyFrom(fInstance.fSceneInstance.fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex],
                                 fInstance.fGroup.fVulkanNodeMatricesStagingBuffers[aInFlightFrameIndex],
                                 0,
                                 0,
                                 length(fNodeMatrices)*SizeOf(TpvMatrix4x4));


    fInstance.fGroup.fVulkanMorphTargetVertexWeightsStagingBuffers[aInFlightFrameIndex].UpdateData(fMorphTargetVertexWeights[0],0,length(fMorphTargetVertexWeights)*SizeOf(TpvFloat),FlushUpdateData);

    fMorphTargetVertexWeightsBuffer.CopyFrom(fInstance.fSceneInstance.fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex],
                                             fInstance.fGroup.fVulkanMorphTargetVertexWeightsStagingBuffers[aInFlightFrameIndex],
                                             0,
                                             0,
                                             length(fMorphTargetVertexWeights)*SizeOf(TpvFloat));}

   end;

   else begin
    Assert(false);
   end;

  end;

{ fInstance.fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fNodeMatricesBuffer);
  fInstance.fSceneInstance.AddInFlightFrameBufferMemoryBarrier(aInFlightFrameIndex,fMorphTargetVertexWeightsBuffer);}

 end;
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

constructor TpvScene3D.TGroup.TInstance.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
var Index,OtherIndex,MaterialIndex,MaterialIDMapArrayIndex:TpvSizeInt;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    Node:TpvScene3D.TGroup.TNode;
    Animation:TpvScene3D.TGroup.TAnimation;
    Light:TpvScene3D.TGroup.TInstance.TLight;
    Camera:TpvScene3D.TGroup.TInstance.TCamera;
    InstanceMaterial:TpvScene3D.TGroup.TInstance.TMaterial;
    MaterialToDuplicate,DuplicatedMaterial,Material:TpvScene3D.TMaterial;
    MaterialIDMapArray:TpvScene3D.TGroup.TMaterialIDMapArray;
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

 fMaterialMap:=nil;

 fDuplicatedMaterials:=TpvScene3D.TMaterials.Create;
 fDuplicatedMaterials.OwnsObjects:=false;

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
  fVulkanDatas[Index]:=TpvScene3D.TGroup.TInstance.TVulkanData.Create(self);
  fPotentiallyVisibleSetNodeIndices[Index]:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
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
 for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
  FreeAndNil(fVulkanDatas[Index]);
 end;
 for Index:=0 to length(fAnimations)-1 do begin
  FreeAndNil(fAnimations[Index]);
 end;
 FreeAndNil(fMaterials);
 fCacheVerticesNodeDirtyBitmap:=nil;
 for Index:=0 to fDuplicatedMaterials.Count-1 do begin
  fDuplicatedMaterials[Index].DecRef;
 end;
 FreeAndNil(fDuplicatedMaterials);
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
    UniversalQueue:TpvVulkanQueue;
    UniversalCommandPool:TpvVulkanCommandPool;
    UniversalCommandBuffer:TpvVulkanCommandBuffer;
    UniversalFence:TpvVulkanFence;
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

{      SetLength(fNodeMatrices,fGroup.fNodes.Count+fGroup.fCountJointNodeMatrices+1);

       SetLength(fMorphTargetVertexWeights,Max(Max(fGroup.fMorphTargetCount,fGroup.fCountNodeWeights),1));}

       for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
        fVulkanDatas[Index].Upload;
       end;

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

           fVulkanMaterialIDMapBuffer:=TpvVulkanBuffer.Create(fSceneInstance.fVulkanDevice,
                                                              length(fMaterialMap)*SizeOf(TpvUInt32),
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

            fSceneInstance.fVulkanDevice.MemoryStaging.Upload(UniversalQueue,
                                                              UniversalCommandBuffer,
                                                              UniversalFence,
                                                              fMaterialMap[0],
                                                              fVulkanMaterialIDMapBuffer,
                                                              0,
                                                              length(fMaterialMap)*SizeOf(TpvUInt32));

{          fVulkanMaterialIDMapBuffer.UploadData(UniversalQueue,
                                                 UniversalCommandBuffer,
                                                 UniversalFence,
                                                 fMaterialMap[0],
                                                 0,
                                                 length(fMaterialMap)*SizeOf(TpvUInt32),
                                                 TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic);//}

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
       end;

       fVulkanComputeDescriptorPool:=TpvVulkanDescriptorPool.Create(fSceneInstance.fVulkanDevice,
                                                                    TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                                    length(fVulkanComputeDescriptorSets));
       fVulkanComputeDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,length(fVulkanComputeDescriptorSets)*8);
       fVulkanComputeDescriptorPool.Initialize;

       for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin

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
                                            [fGroup.fVulkanDrawUniqueIndexBuffer.DescriptorBufferInfo],
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
         DescriptorSet.WriteToDescriptorSet(7,
                                            0,
                                            1,
                                            TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                            [],
                                            [fVulkanMaterialIDMapBuffer.DescriptorBufferInfo],
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
     for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
      FreeAndNil(fVulkanComputeDescriptorSets[Index]);
     end;
     FreeAndNil(fVulkanComputeDescriptorPool);
     FreeAndNil(fVulkanMaterialIDMapBuffer);
     for Index:=0 to fSceneInstance.fCountInFlightFrames-1 do begin
      fVulkanDatas[Index].Unload;
     end;
{    fNodeMatrices:=nil;
     fMorphTargetVertexWeights:=nil;}
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

       Node:=@fNodes[AnimationChannel^.TargetIndex];

       NodeOverwrite:=nil;

       if aFactor>=-0.5 then begin

        InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
        if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

         InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
         if InstanceAnimationChannelOverwrite^<0 then begin
          InstanceAnimationChannelOverwrite^:=Node.CountOverwrites;
          inc(Node.CountOverwrites);
          NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
          NodeOverwrite^.Flags:=[];
          NodeOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
         end;

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
             InstanceAnimationChannelOverwrite^:=Node.CountOverwrites;
             inc(Node.CountOverwrites);
             NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
             NodeOverwrite^.Flags:=[];
             NodeOverwrite^.Factor:=Max(aFactor,0.0);
            end else begin
             NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
            end;

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

        InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
        if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

         InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
         if InstanceAnimationChannelOverwrite^<0 then begin
          InstanceAnimationChannelOverwrite^:=Light.fCountOverwrites;
          inc(Light.fCountOverwrites);
          LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
          LightOverwrite^.Flags:=[];
          LightOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
         end;

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

        InstanceChannelIndex:=AnimationChannel^.TargetInstanceIndex;
        if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin

         InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
         if InstanceAnimationChannelOverwrite^<0 then begin
          InstanceAnimationChannelOverwrite^:=Camera.fCountOverwrites;
          inc(Camera.fCountOverwrites);
          CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
          CameraOverwrite^.Flags:=[];
          CameraOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
         end;

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
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin

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
          InstanceAnimationChannelOverwrite^:=Material.fCountOverwrites;
          inc(Material.fCountOverwrites);
          MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
          MaterialOverwrite^.Flags:=[];
          MaterialOverwrite^.SubIndex:=TargetSubIndex;
          MaterialOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
         end;

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
       Node:=@fNodes[AnimationDefaultChannel^.TargetIndex];
       NodeOverwrite:=nil;
       if aFactor>=-0.5 then begin
        InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
        if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
         InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
         if InstanceAnimationChannelOverwrite^<0 then begin
          InstanceAnimationChannelOverwrite^:=Node.CountOverwrites;
          inc(Node.CountOverwrites);
          NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
          NodeOverwrite^.Flags:=[];
          NodeOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          NodeOverwrite:=@Node.Overwrites[InstanceAnimationChannelOverwrite^];
         end;
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

      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightColor,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightIntensity,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightRange,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotInnerConeAngle,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerPunctualLightSpotOuterConeAngle:begin
       Light:=fLights[AnimationDefaultChannel^.TargetIndex];
       LightOverwrite:=nil;
       if aFactor>=-0.5 then begin
        InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
        if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
         InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
         if InstanceAnimationChannelOverwrite^<0 then begin
          InstanceAnimationChannelOverwrite^:=Light.fCountOverwrites;
          inc(Light.fCountOverwrites);
          LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
          LightOverwrite^.Flags:=[];
          LightOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          LightOverwrite:=@Light.fOverwrites[InstanceAnimationChannelOverwrite^];
         end;
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
        InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
        if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
         InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
         if InstanceAnimationChannelOverwrite^<0 then begin
          InstanceAnimationChannelOverwrite^:=Camera.fCountOverwrites;
          inc(Camera.fCountOverwrites);
          CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
          CameraOverwrite^.Flags:=[];
          CameraOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          CameraOverwrite:=@Camera.fOverwrites[InstanceAnimationChannelOverwrite^];
         end;
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
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureOffset,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureRotation,
      TpvScene3D.TGroup.TAnimation.TChannel.TTarget.PointerTextureScale:begin
       Material:=fMaterials[AnimationDefaultChannel^.TargetIndex];
       TargetSubIndex:=AnimationDefaultChannel^.TargetSubIndex;
       MaterialOverwrite:=nil;
       if (aFactor>=-0.5) and assigned(Material) then begin
        InstanceChannelIndex:=AnimationDefaultChannel^.TargetInstanceIndex;
        if (InstanceChannelIndex>=0) and (InstanceChannelIndex<length(InstanceAnimation.fChannelOverwrites)) then begin
         InstanceAnimationChannelOverwrite:=@InstanceAnimation.fChannelOverwrites[InstanceChannelIndex];
         if InstanceAnimationChannelOverwrite^<0 then begin
          InstanceAnimationChannelOverwrite^:=Material.fCountOverwrites;
          inc(Material.fCountOverwrites);
          MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
          MaterialOverwrite^.Flags:=[];
          MaterialOverwrite^.SubIndex:=TargetSubIndex;
          MaterialOverwrite^.Factor:=Max(aFactor,0.0);
         end else begin
          MaterialOverwrite:=@Material.fOverwrites[InstanceAnimationChannelOverwrite^];
         end;
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
   fOnNodeMatrixPre(self,Node,InstanceNode,Matrix);
  end;
  Matrix:=Matrix*Node.fMatrix;
  if assigned(fOnNodeMatrixPost) then begin
   fOnNodeMatrixPost(self,Node,InstanceNode,Matrix);
  end;
  Matrix:=Matrix*aMatrix;
  InstanceNode^.WorkMatrix:=Matrix;
  if assigned(Node.fMesh) then begin
   if Matrix.Determinant<0.0 then begin
    Include(InstanceNode^.Flags,TpvScene3D.TGroup.TInstance.TNode.TNodeFlag.InverseFrontFaces);
   end else begin
    Exclude(InstanceNode^.Flags,TpvScene3D.TGroup.TInstance.TNode.TNodeFlag.InverseFrontFaces);
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
     MorphTargetVertex:TpvScene3D.TGroup.PMorphTargetVertex;
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
var Index:TPasGLTFSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    Animation:TpvScene3D.TGroup.TInstance.TAnimation;
    Node:TpvScene3D.TGroup.TNode;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    InstanceMaterial:TpvScene3D.TGroup.TInstance.TMaterial;
    AABB:TpvAABB;
    HasMaterialUpdate:boolean;
begin

 if aInFlightFrameIndex>=0 then begin

  fActives[aInFlightFrameIndex]:=fActive;

 end;

 if fActive then begin

  Scene:=GetScene;

  if assigned(Scene) then begin

   if aInFlightFrameIndex>=0 then begin
    fScenes[aInFlightFrameIndex]:=Scene;
   end;

   //CurrentSkinShaderStorageBufferObjectHandle:=0;

   for Index:=0 to length(fLightNodes)-1 do begin
    fLightNodes[Index]:=-1;
   end;

   ResetLights;

   ResetCameras;

   ResetMaterials;

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

   for Index:=0 to Scene.fNodes.Count-1 do begin
    ProcessNode(Scene.fNodes[Index].Index,TpvMatrix4x4.Identity,false);
   end;

   if aInFlightFrameIndex>=0 then begin
    for Index:=0 to length(fNodes)-1 do begin
     if not fNodes[Index].Processed then begin
      if assigned(fNodes[Index].Light) then begin
       FreeAndNil(fNodes[Index].Light);
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
     {if assigned(Node.fSkin) or (length(Node.fWeights)>0) or (length(Node.fMesh.fWeights)>0) then begin
       ProcessMorphSkinNode(Node,InstanceNode);
      end else}begin
       InstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=Node.fMesh.fBoundingBox.Transform(InstanceNode^.WorkMatrix*fModelMatrix);
       InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=true;
      end;
     end else begin
      InstanceNode^.BoundingBoxes[aInFlightFrameIndex]:=TpvAABB.Create(TpvVector3.Origin,TpvVector3.Origin);
      InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex]:=false;
     end;
    end;

    for Index:=0 to Scene.fNodes.Count-1 do begin
     ProcessBoundingBoxNode(Scene.fNodes[Index].Index);
    end;

    for Index:=0 to fGroup.fNodes.Count-1 do begin
     Node:=fGroup.fNodes[Index];
     InstanceNode:=@fNodes[Index];
     if InstanceNode^.BoundingBoxFilled[aInFlightFrameIndex] then begin
      if (InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
         ((InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) and not
          fSceneInstance.fPotentiallyVisibleSet.fNodes[InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]].fAABB.Intersect(InstanceNode^.BoundingBoxes[aInFlightFrameIndex])) then begin
       InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=fSceneInstance.fPotentiallyVisibleSet.GetNodeIndexByAABB(InstanceNode^.BoundingBoxes[aInFlightFrameIndex]);
      end;
     end else begin
      InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex;
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
   if (fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
      ((fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]<>TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) and not
       fSceneInstance.fPotentiallyVisibleSet.fNodes[fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]].fAABB.Intersect(fBoundingBox)) then begin
    fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=fSceneInstance.fPotentiallyVisibleSet.GetNodeIndexByAABB(fBoundingBox);
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

  fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex]:=fSceneInstance.fPotentiallyVisibleSet.NoNodeIndex;

  fPreviousActive:=false;

 end;

end;

procedure TpvScene3D.TGroup.TInstance.PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
begin
 if (aInFlightFrameIndex>=0) and
    fActives[aInFlightFrameIndex] and
    assigned(fScenes[aInFlightFrameIndex]) then begin
  fVulkanData:=fVulkanDatas[aInFlightFrameIndex];
  if assigned(fVulkanData) then begin
   fVulkanData.PrepareGPUUpdate(aInFlightFrameIndex);
  end;
 end;
end;

procedure TpvScene3D.TGroup.TInstance.ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
begin
 if (aInFlightFrameIndex>=0) and
    fActives[aInFlightFrameIndex] and
    assigned(fScenes[aInFlightFrameIndex]) then begin
  fVulkanData:=fVulkanDatas[aInFlightFrameIndex];
  if assigned(fVulkanData) then begin
   fVulkanData.ExecuteGPUUpdate(aInFlightFrameIndex);
  end;
 end;
end;

function TpvScene3D.TGroup.TInstance.GetBakedMesh(const aRelative:boolean=false;
                                                  const aWithDynamicMeshs:boolean=false;
                                                  const aRootNodeIndex:TpvSizeInt=-1;
                                                  const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes=[TpvScene3D.TMaterial.TAlphaMode.Opaque,TpvScene3D.TMaterial.TAlphaMode.Blend,TpvScene3D.TMaterial.TAlphaMode.Mask]):TpvScene3D.TBakedMesh;
var BakedMesh:TpvScene3D.TBakedMesh;
 procedure ProcessMorphSkinNode(const aNode:TpvScene3D.TGroup.TNode;const aInstanceNode:TpvScene3D.TGroup.TInstance.PNode);
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
     MorphTargetVertex:TpvScene3D.TGroup.PMorphTargetVertex;
     BakedVertices:TBakedVertices;
     BakedVertex:PBakedVertex;
     BakedTriangle:TpvScene3D.TBakedMesh.TTriangle;
     TemporaryTriangleIndices:TTemporaryTriangleIndices;
 begin
  BakedVertices:=nil;
  try
   Mesh:=aNode.fMesh;
   if assigned(Mesh) and
      (aWithDynamicMeshs or
       ((not aWithDynamicMeshs) and
        ((not assigned(aNode.Skin)) and
         (length(aNode.fWeights)=0) and
         ((aInstanceNode^.CountOverwrites=0) or
          ((aInstanceNode^.CountOverwrites=1) and
           ((aInstanceNode^.Overwrites[0].Flags=[TpvScene3D.TGroup.TInstance.TNode.TNodeOverwriteFlag.Defaults]))))))) then begin
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
      case Primitive^.PrimitiveMode of
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN:begin
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
         case Primitive^.PrimitiveMode of
          VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST:begin
           SetLength(TemporaryTriangleIndices,Primitive^.CountIndices);
           for IndexIndex:=Primitive^.StartBufferIndexOffset to (Primitive^.StartBufferIndexOffset+Primitive^.CountIndices)-1 do begin
            TemporaryTriangleIndices[IndexIndex-Primitive^.StartBufferIndexOffset]:=Group.fIndices.Items[IndexIndex]-Primitive^.StartBufferVertexOffset;
           end;
          end;
          VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP:begin
           SetLength(TemporaryTriangleIndices,(Primitive^.CountIndices-2)*3);
           for IndexIndex:=0 to Primitive^.CountIndices-3 do begin
            if (IndexIndex and 1)<>0 then begin
             TemporaryTriangleIndices[(IndexIndex*3)+0]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+IndexIndex+0]-Primitive^.StartBufferVertexOffset;
             TemporaryTriangleIndices[(IndexIndex*3)+1]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+IndexIndex+1]-Primitive^.StartBufferVertexOffset;
             TemporaryTriangleIndices[(IndexIndex*3)+2]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+IndexIndex+2]-Primitive^.StartBufferVertexOffset;
            end else begin
             TemporaryTriangleIndices[(IndexIndex*3)+0]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+IndexIndex+0]-Primitive^.StartBufferVertexOffset;
             TemporaryTriangleIndices[(IndexIndex*3)+1]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+IndexIndex+2]-Primitive^.StartBufferVertexOffset;
             TemporaryTriangleIndices[(IndexIndex*3)+2]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+IndexIndex+1]-Primitive^.StartBufferVertexOffset;
            end;
           end;
          end;
          VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN:begin
           SetLength(TemporaryTriangleIndices,(Primitive^.CountIndices-2)*3);
           for IndexIndex:=2 to Primitive^.CountIndices-1 do begin
            TemporaryTriangleIndices[((IndexIndex-1)*3)+0]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+0]-Primitive^.StartBufferVertexOffset;
            TemporaryTriangleIndices[((IndexIndex-1)*3)+1]:=Group.fIndices.Items[(Primitive^.StartBufferIndexOffset+IndexIndex)-1]-Primitive^.StartBufferVertexOffset;
            TemporaryTriangleIndices[((IndexIndex-1)*3)+2]:=Group.fIndices.Items[Primitive^.StartBufferIndexOffset+IndexIndex]-Primitive^.StartBufferVertexOffset;
           end;
          end;
          else begin
          end;
         end;
         IndexIndex:=0;
         while (IndexIndex+2)<length(TemporaryTriangleIndices) do begin
          for SideIndex:=0 to ord(Primitive^.Material.fData.DoubleSided) and 1 do begin
           BakedTriangle:=TpvScene3D.TBakedMesh.TTriangle.Create;
           try
            if SideIndex>0 then begin
             BakedTriangle.Positions[0]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Position;
             BakedTriangle.Positions[1]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Position;
             BakedTriangle.Positions[2]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Position;
             BakedTriangle.Normals[0]:=-BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal;
             BakedTriangle.Normals[1]:=-BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal;
             BakedTriangle.Normals[2]:=-BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal;
             BakedTriangle.Normal:=-(BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal).Normalize;
            end else begin
             BakedTriangle.Positions[0]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Position;
             BakedTriangle.Positions[1]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Position;
             BakedTriangle.Positions[2]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Position;
             BakedTriangle.Normals[0]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal;
             BakedTriangle.Normals[1]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal;
             BakedTriangle.Normals[2]:=BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal;
             BakedTriangle.Normal:=(BakedVertices[TemporaryTriangleIndices[IndexIndex+0]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+1]].Normal+BakedVertices[TemporaryTriangleIndices[IndexIndex+2]].Normal).Normalize;
            end;
           finally
            BakedMesh.fTriangles.Add(BakedTriangle);
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
type TNodeStack=TpvDynamicStack<TpvSizeInt>;
var Index,NodeIndex:TpvSizeInt;
    NodeStack:TNodeStack;
    GroupScene:TpvScene3D.TGroup.TScene;
    GroupNode:TpvScene3D.TGroup.TNode;
    GroupInstanceNode:TpvScene3D.TGroup.TInstance.PNode;
begin
 BakedMesh:=TpvScene3D.TBakedMesh.Create;
 try
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
     if assigned(GroupNode.fMesh) then begin
      ProcessMorphSkinNode(GroupNode,GroupInstanceNode);
     end;
    end;
   end;
  finally
   NodeStack.Finalize;
  end;
 finally
  result:=BakedMesh;
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

procedure TpvScene3D.TGroup.TInstance.Prepare(const aInFlightFrameIndex:TpvSizeInt;
                                              const aRenderPassIndex:TpvSizeInt;
                                              const aViewBaseIndex:TpvSizeInt;
                                              const aCountViews:TpvSizeInt;
                                              const aFrustums:TpvFrustumDynamicArray;
                                              const aPotentiallyVisibleSetCulling:boolean);
var VisibleBit:TpvUInt32;
 procedure ProcessNode(const aNodeIndex:TpvSizeInt;const aMask:TpvUInt32);
 var Index,NodeIndex,ViewIndex:TpvSizeInt;
     PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
     Mask:TpvUInt32;
     InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
     Node:TpvScene3D.TGroup.TNode;
     OK:boolean;
 begin
  if aNodeIndex>=0 then begin
   InstanceNode:=@fNodes[aNodeIndex];
   Mask:=aMask;
   if aPotentiallyVisibleSetCulling then begin
    PotentiallyVisibleSetNodeIndex:=InstanceNode^.PotentiallyVisibleSetNodeIndices[aInFlightFrameIndex];
    if PotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
     OK:=true;
    end else begin
     OK:=false;
     for ViewIndex:=aViewBaseIndex to (aViewBaseIndex+aCountViews)-1 do begin
      ViewPotentiallyVisibleSetNodeIndex:=fSceneInstance.fPotentiallyVisibleSet.fViewNodeIndices[ViewIndex];
      if (ViewPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
         fSceneInstance.fPotentiallyVisibleSet.GetNodeVisibility(PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex) then begin
       OK:=true;
       break;
      end;
     end;
    end;
   end else begin
    OK:=true;
   end;
   if OK then begin
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
 end;
var NodeIndex,ViewIndex:TpvSizeInt;
    PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex:TpvScene3D.TPotentiallyVisibleSet.TNodeIndex;
    Scene:TpvScene3D.TGroup.TScene;
    OK:boolean;
begin
 VisibleBit:=TpvUInt32(1) shl aRenderPassIndex;
 if fActives[aInFlightFrameIndex] and ((fVisibleBitmap and (TpvUInt32(1) shl aRenderPassIndex))<>0) then begin
  if (length(aFrustums)>0) or aPotentiallyVisibleSetCulling then begin
   for NodeIndex:=0 to length(fNodes)-1 do begin
    TPasMPInterlocked.BitwiseAnd(fNodes[NodeIndex].VisibleBitmap,not VisibleBit);
   end;
   Scene:=fScenes[aInFlightFrameIndex];
   if assigned(Scene) then begin
    if aPotentiallyVisibleSetCulling then begin
     PotentiallyVisibleSetNodeIndex:=fPotentiallyVisibleSetNodeIndices[aInFlightFrameIndex];
     if PotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex then begin
      OK:=true;
     end else begin
      OK:=false;
      for ViewIndex:=aViewBaseIndex to (aViewBaseIndex+aCountViews)-1 do begin
       ViewPotentiallyVisibleSetNodeIndex:=fSceneInstance.fPotentiallyVisibleSet.fViewNodeIndices[ViewIndex];
       if (ViewPotentiallyVisibleSetNodeIndex=TpvScene3D.TPotentiallyVisibleSet.NoNodeIndex) or
          fSceneInstance.fPotentiallyVisibleSet.GetNodeVisibility(PotentiallyVisibleSetNodeIndex,ViewPotentiallyVisibleSetNodeIndex) then begin
        OK:=true;
        break;
       end;
      end;
     end;
    end else begin
     OK:=true;
    end;
    if OK then begin
     for NodeIndex:=0 to Scene.fNodes.Count-1 do begin
      ProcessNode(Scene.fNodes[NodeIndex].fIndex,$ffffffff);
     end;
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
var NodeIndex,IndicesStart,IndicesCount,InFlightFrameIndex,
    DrawChoreographyBatchUniqueItemIndex,
    CountDrawChoreographyBatchUniqueItems:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    Node:TpvScene3D.TGroup.TInstance.PNode;
    FirstFlush:boolean;
    DrawChoreographyBatchUniqueItem:TpvScene3D.TGroup.TDrawChoreographyBatchItem;
    MeshComputeStagePushConstants:TpvScene3D.TMeshComputeStagePushConstants;
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

   FirstFlush:=true;

   IndicesStart:=0;
   IndicesCount:=0;

   DrawChoreographyBatchUniqueItemIndex:=0;
   CountDrawChoreographyBatchUniqueItems:=Scene.fDrawChoreographyBatchUniqueItems.Count;

   while DrawChoreographyBatchUniqueItemIndex<CountDrawChoreographyBatchUniqueItems do begin

    DrawChoreographyBatchUniqueItem:=Scene.fDrawChoreographyBatchUniqueItems[DrawChoreographyBatchUniqueItemIndex];
    inc(DrawChoreographyBatchUniqueItemIndex);

    NodeIndex:=DrawChoreographyBatchUniqueItem.Node.fIndex;

    if (fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5] and (TpvUInt32(1) shl (NodeIndex and 31)))<>0 then begin

     IndicesStart:=DrawChoreographyBatchUniqueItem.fStartIndex;
     IndicesCount:=DrawChoreographyBatchUniqueItem.fCountIndices;

     while DrawChoreographyBatchUniqueItemIndex<CountDrawChoreographyBatchUniqueItems do begin

      DrawChoreographyBatchUniqueItem:=Scene.fDrawChoreographyBatchUniqueItems[DrawChoreographyBatchUniqueItemIndex];

      NodeIndex:=DrawChoreographyBatchUniqueItem.Node.fIndex;

      if ((fCacheVerticesNodeDirtyBitmap[NodeIndex shr 5] and (TpvUInt32(1) shl (NodeIndex and 31)))<>0) and
         ((IndicesStart+IndicesCount)=DrawChoreographyBatchUniqueItem.fStartIndex) then begin

       inc(IndicesCount,DrawChoreographyBatchUniqueItem.fCountIndices);
       inc(DrawChoreographyBatchUniqueItemIndex);

      end else begin
       break;
      end;

     end;

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

     end;

    end;

   end;

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
var IndicesStart,IndicesCount,
    DrawChoreographyBatchItemIndex,
    CountDrawChoreographyBatchItems:TpvSizeInt;
    Scene:TpvScene3D.TGroup.TScene;
    InstanceNode:TpvScene3D.TGroup.TInstance.PNode;
    PrimitiveTopology:TpvScene3D.TPrimitiveTopology;
    DoubleSided,InverseFrontFaces,Culling,FirstFlush:boolean;
    VisibleBit:TpvUInt32;
    DrawChoreographyBatchItem:TpvScene3D.TGroup.TDrawChoreographyBatchItem;
    Pipeline:TpvVulkanPipeline;
begin
 if fActives[aInFlightFrameIndex] and ((fVisibleBitmap and (TpvUInt32(1) shl aRenderPassIndex))<>0) then begin

  Culling:=fGroup.fCulling;

  Scene:=fScenes[aInFlightFrameIndex];

  if assigned(Scene) then begin

   VisibleBit:=TpvUInt32(1) shl aRenderPassIndex;

   FirstFlush:=true;

   InverseFrontFaces:=false;

   IndicesStart:=0;
   IndicesCount:=0;

   DrawChoreographyBatchItemIndex:=0;
   CountDrawChoreographyBatchItems:=Scene.fDrawChoreographyBatchItems.Count;

   while DrawChoreographyBatchItemIndex<CountDrawChoreographyBatchItems do begin

    DrawChoreographyBatchItem:=Scene.fDrawChoreographyBatchItems[DrawChoreographyBatchItemIndex];
    inc(DrawChoreographyBatchItemIndex);

    if DrawChoreographyBatchItem.fMaterial.fVisible and
       (DrawChoreographyBatchItem.fAlphaMode in aMaterialAlphaModes) and
       (DrawChoreographyBatchItem.fCountIndices>0) then begin

     InstanceNode:=@fNodes[DrawChoreographyBatchItem.Node.fIndex];

     if ((not Culling) or ((InstanceNode^.VisibleBitmap and VisibleBit)<>0)) then begin

      IndicesStart:=DrawChoreographyBatchItem.fStartIndex;
      IndicesCount:=DrawChoreographyBatchItem.fCountIndices;

      PrimitiveTopology:=DrawChoreographyBatchItem.fPrimitiveTopology;

      DoubleSided:=DrawChoreographyBatchItem.fDoubleSided;

      InverseFrontFaces:=InstanceNode^.InverseFrontFaces;

      while DrawChoreographyBatchItemIndex<CountDrawChoreographyBatchItems do begin

       DrawChoreographyBatchItem:=Scene.fDrawChoreographyBatchItems[DrawChoreographyBatchItemIndex];

       if DrawChoreographyBatchItem.fMaterial.fVisible and
          (DrawChoreographyBatchItem.fAlphaMode in aMaterialAlphaModes) and
          (DrawChoreographyBatchItem.fCountIndices>0) then begin

        InstanceNode:=@fNodes[DrawChoreographyBatchItem.Node.fIndex];

        if ((not Culling) or ((InstanceNode.VisibleBitmap and VisibleBit)<>0)) and
           (DrawChoreographyBatchItem.fPrimitiveTopology=PrimitiveTopology) and
           (DrawChoreographyBatchItem.fDoubleSided=DoubleSided) and
           (DoubleSided or (InstanceNode^.InverseFrontFaces=InverseFrontFaces)) and
           ((IndicesStart+IndicesCount)=DrawChoreographyBatchItem.fStartIndex) then begin

         inc(IndicesCount,DrawChoreographyBatchItem.fCountIndices);
         inc(DrawChoreographyBatchItemIndex);

         continue;

        end;

       end;

       break;

      end;

      if IndicesCount>0 then begin

       Pipeline:=aGraphicsPipelines[PrimitiveTopology,DoubleSidedFaceCullingModes[DoubleSided,InverseFrontFaces]];
       if aPipeline<>Pipeline then begin
        aPipeline:=Pipeline;
        if assigned(Pipeline) then begin
         aCommandBuffer.CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,Pipeline.Handle);
        end;
       end;

       if FirstFlush then begin

        FirstFlush:=false;

        fSceneInstance.SetGlobalResources(aCommandBuffer,aPipelineLayout,aRenderPassIndex,aInFlightFrameIndex);

        fGroup.SetGroupResources(aCommandBuffer,aPipelineLayout,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);

        if assigned(aOnSetRenderPassResources) then begin
         aOnSetRenderPassResources(aCommandBuffer,aPipelineLayout,aRenderPassIndex,aPreviousInFlightFrameIndex,aInFlightFrameIndex);
        end;

       end;

       if assigned(aPipeline) then begin
        aCommandBuffer.CmdDrawIndexed(IndicesCount,1,IndicesStart,0,0);
       end;

      end;

     end;

    end;

   end;

  end;

 end;

end;

{ TpvScene3D }

constructor TpvScene3D.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource;const aVulkanDevice:TpvVulkanDevice;const aUseBufferDeviceAddress:boolean;const aCountInFlightFrames:TpvSizeInt);
var Index:TpvSizeInt;
begin

 inherited Create(aResourceManager,aParent);

 if assigned(aVulkanDevice) then begin
  fVulkanDevice:=aVulkanDevice;
 end else if assigned(pvApplication) then begin
  fVulkanDevice:=pvApplication.VulkanDevice;
 end else begin
  fVulkanDevice:=nil;
 end;

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

 fUseBufferDeviceAddress:=aUseBufferDeviceAddress;

 fUploaded:=false;

 fInUpload:=false;

 fHasTransmission:=false;

 fPotentiallyVisibleSet:=TpvScene3D.TPotentiallyVisibleSet.Create;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fVulkanBufferCopyBatchItemArrays[Index].Initialize;
 end;

 fTechniques:=TpvTechniques.Create;

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

 fMaterialIDManager:=TIDManager.Create;

 fMaterialIDHashMap:=TMaterialIDHashMap.Create(nil);

 FillChar(fMaterialIDDirtyMap,SizeOf(TMaterialIDDirtyMap),#0);

 FillChar(fMaterialIDMap,SizeOf(TMaterialIDMap),#0);

 fMaterialHashMap:=TMaterialHashMap.Create(nil);

 FillChar(fMaterialDataMaterialGenerations,SizeOf(fMaterialDataMaterialGenerations),#$ff);

 fDefaultSampler:=TSampler.Create(ResourceManager,self);
 fDefaultSampler.AssignFromDefault;
 fDefaultSampler.IncRef;

 fWhiteImage:=TpvScene3D.TImage.Create(ResourceManager,self);
 fWhiteImage.AssignFromWhiteTexture;
 fWhiteImage.IncRef;

 fWhiteTexture:=TpvScene3D.TTexture.Create(ResourceManager,self);
 fWhiteTexture.AssignFromWhiteTexture;
 fWhiteTexture.IncRef;

 fDefaultNormalMapImage:=TpvScene3D.TImage.Create(ResourceManager,self);
 fDefaultNormalMapImage.AssignFromDefaultNormalMapTexture;
 fDefaultNormalMapImage.IncRef;

 fDefaultNormalMapTexture:=TpvScene3D.TTexture.Create(ResourceManager,self);
 fDefaultNormalMapTexture.AssignFromDefaultNormalMapTexture;
 fDefaultNormalMapTexture.IncRef;

 fEmptyMaterial:=TpvScene3D.TMaterial.Create(ResourceManager,self);
 fEmptyMaterial.AssignFromEmpty;
 fEmptyMaterial.IncRef;

 fPrimaryLightDirection:=TpvVector3.InlineableCreate(0.5,-1.0,-1.0).Normalize;

//fPrimaryLightDirection:=TpvVector3.InlineableCreate(0.333333333333,-0.666666666666,-0.666666666666).Normalize;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLights[Index]:=TpvScene3D.TLights.Create;
  fLights[Index].OwnsObjects:=true;
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
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

 ReleaseFrameDelay:=fCountInFlightFrames+1;

 fMeshComputeVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice);

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

 // Instance - Material ID map
 fMeshComputeVulkanDescriptorSetLayout.AddBinding(7,
                                                  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                                  1,
                                                  TVkShaderStageFlags(VK_SHADER_STAGE_COMPUTE_BIT),
                                                  []);

 fMeshComputeVulkanDescriptorSetLayout.Initialize;

 fGlobalVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fVulkanDevice,TVkDescriptorSetLayoutCreateFlags(VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT),true);
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

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightAABBTreeStateGenerations[Index]:=fLightAABBTreeGeneration-1;
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightBuffers[Index]:=TpvScene3D.TLightBuffer.Create(self,Index);
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fInFlightFrameBufferMemoryBarriers[Index].Initialize;
 end;

 fAABBTree:=TpvBVHDynamicAABBTree.Create;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
 end;

end;

destructor TpvScene3D.Destroy;
var Index:TpvSizeInt;
begin

 Unload;

 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanDescriptorSets[Index]);
 end;
 FreeAndNil(fGlobalVulkanDescriptorPool);

 for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanViewUniformBuffers[Index]);
 end;

{for Index:=0 to fCountInFlightFrames-1 do begin
  FreeAndNil(fGlobalVulkanViewUniformStagingBuffers[Index]);
 end;}

 for Index:=0 to fCountInFlightFrames-1 do begin
  fAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
  fInFlightFrameBufferMemoryBarriers[Index].Finalize;
 end;

 FreeAndNil(fAABBTree);

 for Index:=0 to fCountInFlightFrames-1 do begin
  fLightAABBTreeStates[Index].TreeNodes:=nil;
 end;

 for Index:=0 to fCountInFlightFrames-1 do begin
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

 for Index:=0 to fCountInFlightFrames-1 do begin
  fVulkanBufferCopyBatchItemArrays[Index].Finalize;
 end;

 FreeAndNil(fImageDescriptorGenerationLock);

 FreeAndNil(fMaterialDataGenerationLock);

 FreeAndNil(fPotentiallyVisibleSet);

 FreeAndNil(fLock);

 inherited Destroy;
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
    fMaterialDataProcessedGeneration:=0;
    for Index:=0 to fCountInFlightFrames-1 do begin
     fMaterialDataProcessedGenerations[Index]:=0;
     fMaterialDataProcessedMinChangedID[Index]:=0;
     fMaterialDataProcessedMaxChangedID[Index]:=$ffff;
    end;
    FillChar(fMaterialDataMaterialGenerations,SizeOf(fMaterialDataMaterialGenerations),#$ff);
   end;
  finally
   fMaterialDataGenerationLock.Release;
  end;
 end;
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
  BufferMemoryBarrier^.srcAccessMask:=TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or
                                      TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
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

       fDefaultSampler.Upload;

       fWhiteTexture.Upload;

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
          fGlobalVulkanViewUniformBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                         SizeOf(TGlobalViewUniformBuffer),
                                                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
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
                                                                                SizeOf(TGlobalViewUniformBuffer),
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
          fGlobalVulkanViewUniformBuffers[Index]:=TpvVulkanBuffer.Create(fVulkanDevice,
                                                                         SizeOf(TGlobalViewUniformBuffer),
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
       fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,length(fGlobalVulkanDescriptorSets)*4);
       fGlobalVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,length(fGlobalVulkanDescriptorSets)*length(fImageInfos));
       fGlobalVulkanDescriptorPool.Initialize;

       for Group in fGroups do begin
        if Group.AsyncLoadState in [TpvResource.TAsyncLoadState.None,TpvResource.TAsyncLoadState.Done] then begin
         Group.Upload;
        end;
       end;

       for Index:=0 to length(fMaterialBufferData)-1 do begin
        fMaterialBufferData[Index]:=TMaterial.DefaultShaderData;
       end;

       FillChar(fMaterialDataMaterialGenerations,SizeOf(fMaterialDataMaterialGenerations),#$ff);

       MaxMaterialID:=0;

       fMaterialDataGeneration:=1;

       fMaterialDataProcessedGeneration:=0;

       for Index:=0 to fMaterials.Count-1 do begin
        Material:=fMaterials[Index];
        if (Material.ID>0) and (Material.ID<length(fMaterialBufferData)) then begin
         fMaterialBufferData[Material.ID]:=Material.fShaderData;
         fMaterialDataMaterialGenerations[Material.ID]:=Material.fGeneration;
         if MaxMaterialID<Material.ID then begin
          MaxMaterialID:=Material.ID;
         end;
        end;
       end;

       for Index:=0 to fCountInFlightFrames-1 do begin
        fInFlightFrameMaterialBufferData[Index]:=fMaterialBufferData;
        fInFlightFrameMaterialBufferDataOffsets[Index]:=0;
        fInFlightFrameMaterialBufferDataSizes[Index]:=0;
        fInFlightFrameMaterialBufferDataMinMaterialID[Index]:=0;
        fInFlightFrameMaterialBufferDataGeneration[Index]:=0;
        fInFlightFrameMaterialBufferDataUploadedGeneration[Index]:=0;
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
                                                  fMaterialBufferData,
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
                                                  fMaterialBufferData,
                                                  fVulkanMaterialDataBuffers[Index],
                                                  0,
                                                  MaterialBufferDataSize);

{             fVulkanMaterialDataBuffers[Index].CopyFrom(fVulkanBufferCopyBatchItemArrays[Index],
                                                         fVulkanMaterialDataStagingBuffers[Index],
                                                         0,
                                                         0,
                                                         MaterialBufferDataSize);}

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

       for Index:=0 to fCountInFlightFrames-1 do begin
        fImageDescriptorProcessedGenerations[Index]:=0;
       end;

       fImageDescriptorGeneration:=0;
       fImageDescriptorProcessedGeneration:=0;

       for Index:=0 to fCountInFlightFrames-1 do begin
        fMaterialDataProcessedGenerations[Index]:=0;
        fMaterialDataProcessedMaxChangedID[Index]:=$ffff;
        fMaterialDataProcessedMinChangedID[Index]:=0;
       end;

       fMaterialDataGeneration:=1;

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
      FreeAndNil(fGlobalVulkanViewUniformBuffers[Index]);
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

procedure TpvScene3D.ResetRenderPasses;
begin
 TPasMPInterlocked.Write(fRenderPassIndexCounter,0);
end;

function TpvScene3D.AcquireRenderPassIndex:TpvSizeInt;
begin
 result:=TPasMPInterlocked.Increment(fRenderPassIndexCounter);
end;

procedure TpvScene3D.Check(const aInFlightFrameIndex:TpvSizeInt);
var Group:TpvScene3D.TGroup;
begin
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
 if First or IsZero(fBoundingBox.Radius) then begin
  fBoundingBox.Min:=TpvVector3.InlineableCreate(-1.0,-1.0,-1.0);
  fBoundingBox.Max:=TpvVector3.InlineableCreate(1.0,1.0,-1.0);
 end;

 fInFlightFrameBoundingBoxes[aInFlightFrameIndex]:=fBoundingBox;

end;

procedure TpvScene3D.PrepareGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
var Index,ItemID,BaseItemID,MaterialBufferDataOffset,MaterialBufferDataSize:TpvSizeInt;
    MinMaterialID,MaxMaterialID:TpvUInt32;
    OldGeneration,NewGeneration:TpvUInt64;
    DirtyBits:TPasMPUInt32;
    LightBuffer:TpvScene3D.TLightBuffer;
    LightAABBTreeState,AABBTreeState:TpvBVHDynamicAABBTree.PState;
    Group:TpvScene3D.TGroup;
    Texture:TpvScene3D.TTexture;
    GroupInstance:TpvScene3D.TGroup.TInstance;
    First:boolean;
    Material:TpvScene3D.TMaterial;
begin

 for Group in fGroups do begin
  Group.PrepareGPUUpdate(aInFlightFrameIndex);
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

 if (fMaterialDataProcessedGeneration<>fMaterialDataGeneration) or
    (fMaterialDataProcessedGenerations[aInFlightFrameIndex]<>fMaterialDataProcessedGeneration) then begin

  fMaterialDataGenerationLock.Acquire;
  try

   if fMaterialDataProcessedGeneration<>fMaterialDataGeneration then begin
    fMaterialDataProcessedGeneration:=fMaterialDataGeneration;
    MinMaterialID:=$ffff;
    MaxMaterialID:=0;
    BaseItemID:=0;
    for Index:=0 to length(fMaterialIDDirtyMap)-1 do begin
     DirtyBits:=TPasMPInterlocked.Exchange(fMaterialIDDirtyMap[Index],0);
     while DirtyBits<>0 do begin
      ItemID:=BaseItemID+TPasMPMath.FindFirstSetBit32(DirtyBits);
      if (ItemID>=0) and (ItemID<length(fMaterialIDMap)) then begin
       Material:=fMaterialIDMap[ItemID];
       if assigned(Material) then begin
        fMaterialBufferData[ItemID]:=Material.fShaderData;
        fMaterialDataMaterialGenerations[ItemID]:=Material.fGeneration;
       end else begin
        fMaterialBufferData[ItemID]:=TMaterial.DefaultShaderData;
        fMaterialDataMaterialGenerations[ItemID]:=High(TpvUInt64);
       end;
       if MinMaterialID>ItemID then begin
        MinMaterialID:=ItemID;
       end;
       if MaxMaterialID<ItemID then begin
        MaxMaterialID:=ItemID;
       end;
      end;
      DirtyBits:=DirtyBits and (DirtyBits-1);
     end;
     inc(BaseItemID,32);
    end;
    if MinMaterialID<=MaxMaterialID then begin
     for Index:=0 to CountInFlightFrames-1 do begin
      if fMaterialDataProcessedMinChangedID[Index]>MinMaterialID then begin
       fMaterialDataProcessedMinChangedID[Index]:=MinMaterialID;
      end;
      if fMaterialDataProcessedMaxChangedID[Index]<MaxMaterialID then begin
       fMaterialDataProcessedMaxChangedID[Index]:=MaxMaterialID;
      end;
     end;
    end;
   end;

   if fMaterialDataProcessedGenerations[aInFlightFrameIndex]<>fMaterialDataProcessedGeneration then begin
    fMaterialDataProcessedGenerations[aInFlightFrameIndex]:=fMaterialDataProcessedGeneration;
    MinMaterialID:=fMaterialDataProcessedMinChangedID[aInFlightFrameIndex];
    MaxMaterialID:=fMaterialDataProcessedMaxChangedID[aInFlightFrameIndex];
    if MinMaterialID<=MaxMaterialID then begin
     fMaterialDataProcessedMinChangedID[aInFlightFrameIndex]:=$ffff;
     fMaterialDataProcessedMaxChangedID[aInFlightFrameIndex]:=$0000;
     MaterialBufferDataOffset:=SizeOf(TMaterial.TShaderData)*MinMaterialID;
     MaterialBufferDataSize:=SizeOf(TMaterial.TShaderData)*(MaxMaterialID+1);
     fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex]:=MaterialBufferDataOffset;
     fInFlightFrameMaterialBufferDataSizes[aInFlightFrameIndex]:=MaterialBufferDataSize;
     fInFlightFrameMaterialBufferDataMinMaterialID[aInFlightFrameIndex]:=MinMaterialID;
     inc(fInFlightFrameMaterialBufferDataGeneration[aInFlightFrameIndex]);
     Move(fMaterialBufferData[fInFlightFrameMaterialBufferDataMinMaterialID[aInFlightFrameIndex]],
          fInFlightFrameMaterialBufferData[aInFlightFrameIndex,fInFlightFrameMaterialBufferDataMinMaterialID[aInFlightFrameIndex]],
          MaterialBufferDataSize-MaterialBufferDataOffset);
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
  CollectLightAABBTreeLights(LightAABBTreeState^.TreeNodes,LightAABBTreeState^.Root,LightBuffer.fLightItems,LightBuffer.fLightMetaInfos);
  fLightAABBTree.GetSkipListNodes(LightBuffer.fLightTree,GetLightUserDataIndex);
  LightBuffer.fNewLightAABBTreeGeneration:=fLightAABBTreeGeneration;

 end;

end;

procedure TpvScene3D.ExecuteGPUUpdate(const aInFlightFrameIndex:TpvSizeInt);
var Group:TpvScene3D.TGroup;
begin

 for Group in fGroups do begin
  Group.ExecuteGPUUpdate(aInFlightFrameIndex);
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

 if fInFlightFrameMaterialBufferDataUploadedGeneration[aInFlightFrameIndex]<>fInFlightFrameMaterialBufferDataGeneration[aInFlightFrameIndex] then begin

  fMaterialDataGenerationLock.Acquire;
  try

   if fInFlightFrameMaterialBufferDataUploadedGeneration[aInFlightFrameIndex]<>fInFlightFrameMaterialBufferDataGeneration[aInFlightFrameIndex] then begin

    fInFlightFrameMaterialBufferDataUploadedGeneration[aInFlightFrameIndex]:=fInFlightFrameMaterialBufferDataGeneration[aInFlightFrameIndex];

    if fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex]<fInFlightFrameMaterialBufferDataSizes[aInFlightFrameIndex] then begin

     case fBufferStreamingMode of

      TBufferStreamingMode.Direct:begin

       fVulkanMaterialDataBuffers[aInFlightFrameIndex].UpdateData(fInFlightFrameMaterialBufferData[aInFlightFrameIndex,fInFlightFrameMaterialBufferDataMinMaterialID[aInFlightFrameIndex]],
                                                                  fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex],
                                                                  fInFlightFrameMaterialBufferDataSizes[aInFlightFrameIndex]-fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex]);

      end;

      TBufferStreamingMode.Staging:begin

       fVulkanDevice.MemoryStaging.Upload(fVulkanStagingQueue,
                                          fVulkanStagingCommandBuffer,
                                          fVulkanStagingFence,
                                          fInFlightFrameMaterialBufferData[aInFlightFrameIndex,fInFlightFrameMaterialBufferDataMinMaterialID[aInFlightFrameIndex]],
                                          fVulkanMaterialDataBuffers[aInFlightFrameIndex],
                                          fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex],
                                          fInFlightFrameMaterialBufferDataSizes[aInFlightFrameIndex]-fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex]);

{      fVulkanMaterialDataStagingBuffers[aInFlightFrameIndex].UpdateData(fInFlightFrameMaterialBufferData[aInFlightFrameIndex,fInFlightFrameMaterialBufferDataMinMaterialID[aInFlightFrameIndex]],
                                                                         fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex],
                                                                         fInFlightFrameMaterialBufferDataSizes[aInFlightFrameIndex]-fInFlightFrameMaterialBufferDataOffsets[aInFlightFrameIndex]);

       fVulkanMaterialDataBuffers[aInFlightFrameIndex].CopyFrom(fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex],
                                                                fVulkanMaterialDataStagingBuffers[aInFlightFrameIndex],
                                                                0,
                                                                0,
                                                                fInFlightFrameMaterialBufferDataSizes[aInFlightFrameIndex]);}

      end;

      else begin
       Assert(false);
      end;

     end;

    end;

   end;

  finally
   fMaterialDataGenerationLock.Release;
  end;

 end;

 LightBuffers[aInFlightFrameIndex].ExecuteGPUUpdate;

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
                                                var aLightItemArray:TpvScene3D.TLightItems;
                                                var aLightMetaInfoArray:TpvScene3D.TLightMetaInfos);
 procedure ProcessLight(const aLight:TpvScene3D.TLight);
 var LightItem:TpvScene3D.PLightItem;
     LightMetaInfo:TpvScene3D.PLightMetaInfo;
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
   LightMetaInfo:=@aLightMetaInfoArray[aLight.fLightItemIndex];
   LightMetaInfo^.MinBounds:=TpvVector4.Create(aLight.fBoundingBox.Min,TpvUInt32(aLight.fDataPointer^.Type_));
   LightMetaInfo^.MaxBounds:=TpvVector4.Create(aLight.fBoundingBox.Max,aLight.fBoundingBox.Radius);
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
var ViewIndex:TpvSizeInt;
begin
 if fViews.Count>0 then begin
  for ViewIndex:=0 to fViews.Count-1 do begin
   fPotentiallyVisibleSet.fViewNodeIndices[ViewIndex]:=fPotentiallyVisibleSet.GetNodeIndexByPosition(Views.Items[ViewIndex].InverseViewMatrix.Translation.xyz);
  end;
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
   case fBufferStreamingMode of
    TBufferStreamingMode.Direct:begin
     fGlobalVulkanViewUniformBuffers[aInFlightFrameIndex].UpdateData(fGlobalVulkanViews[aInFlightFrameIndex].Items[0],
                                                                     0,
                                                                     (fViews.Count+fPreviousViews.Count)*SizeOf(TpvScene3D.TView),
                                                                     FlushUpdateData
                                                                    );
    end;
    TBufferStreamingMode.Staging:begin
     fVulkanDevice.MemoryStaging.Upload(fVulkanStagingQueue,
                                        fVulkanStagingCommandBuffer,
                                        fVulkanStagingFence,
                                        fGlobalVulkanViews[aInFlightFrameIndex].Items[0],
                                        fGlobalVulkanViewUniformBuffers[aInFlightFrameIndex],
                                        0,
                                        (fViews.Count+fPreviousViews.Count)*SizeOf(TpvScene3D.TView));
{    fGlobalVulkanViewUniformStagingBuffers[aInFlightFrameIndex].UpdateData(fGlobalVulkanViews[aInFlightFrameIndex].Items[0],
                                                                            0,
                                                                            (fViews.Count+fPreviousViews.Count)*SizeOf(TpvScene3D.TView),
                                                                            FlushUpdateData
                                                                           );
     fGlobalVulkanViewUniformBuffers[aInFlightFrameIndex].CopyFrom(fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex],
                                                                   fGlobalVulkanViewUniformStagingBuffers[aInFlightFrameIndex],
                                                                   0,
                                                                   0,
                                                                   (fViews.Count+fPreviousViews.Count)*SizeOf(TpvScene3D.TView));}
    end;
    else begin
     Assert(false);
    end;
   end;
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
                             const aFrustumCulling:boolean=true;
                             const aPotentiallyVisibleSetCulling:boolean=true);
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
                   aViewBaseIndex,
                   aCountViews,
                   Frustums,
                   aPotentiallyVisibleSetCulling);
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

function TpvScene3D.NeedFlush(const aInFlightFrameIndex:TpvSizeInt):boolean;
begin
 result:=fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex].Count>0;
end;

function TpvScene3D.Flush(const aInFlightFrameIndex:TpvSizeInt;
                          const aCommandBuffer:TpvVulkanCommandBuffer):boolean;
begin
 result:=fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex].Count>0;
 if result then begin
  try
   TpvVulkanBuffer.ProcessCopyBatch(aCommandBuffer,
                                    fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex],
                                    true);
  finally
   fVulkanBufferCopyBatchItemArrays[aInFlightFrameIndex].Count:=0;
  end;
 end;
end;

procedure TpvScene3D.Draw(const aGraphicsPipelines:TpvScene3D.TGraphicsPipelines;
                          const aPreviousInFlightFrameIndex:TpvSizeInt;
                          const aInFlightFrameIndex:TpvSizeInt;
                          const aRenderPassIndex:TpvSizeInt;
                          const aViewBaseIndex:TpvSizeInt;
                          const aCountViews:TpvSizeInt;
                          const aFrameIndex:TpvSizeInt;
                          const aCommandBuffer:TpvVulkanCommandBuffer;
                          const aPipelineLayout:TpvVulkanPipelineLayout;
                          const aOnSetRenderPassResources:TOnSetRenderPassResources;
                          const aMaterialAlphaModes:TpvScene3D.TMaterial.TAlphaModes;
                          const aJitter:PpvVector4);
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
  VertexStagePushConstants^.CountAllViews:=fViews.Count;
  VertexStagePushConstants^.FrameIndex:=aFrameIndex;
  if assigned(aJitter) then begin
   VertexStagePushConstants^.Jitter:=aJitter^;
  end else begin
   VertexStagePushConstants^.Jitter:=TpvVector4.Null;
  end;

  if fInFlightFrameBufferMemoryBarriers[aInFlightFrameIndex].Count>0 then begin
   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_HOST_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT) or fVulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
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

initialization
 InitializeAnimationChannelTargetOverwriteGroupMap;
end.

