(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.FrameFrame.pas                  *
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
unit PasVulkan.FrameGraph;
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
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Utils;

// Inspired from:
//   https://www.ea.com/frostbite/news/framegraph-extensible-rendering-architecture-in-frostbite
//   https://www.gdcvault.com/play/1024612/FrameGraph-Extensible-Rendering-Architecture-in
//   https://www.slideshare.net/DICEStudio/framegraph-extensible-rendering-architecture-in-frostbite
//   http://themaister.net/blog/2017/08/15/render-graphs-and-vulkan-a-deep-dive/

// Attention: It is still work in progress

type EpvFrameGraph=class(Exception);

     EpvFrameGraphEmptyName=class(EpvFrameGraph);

     EpvFrameGraphDuplicateName=class(EpvFrameGraph);

     EpvFrameGraphMissingQueue=class(EpvFrameGraph);

     EpvFrameGraphMismatchImageSize=class(EpvFrameGraph);

     EpvFrameGraphMissedGeneratorPassForResource=class(EpvFrameGraph);

     EpvFrameGraphResourceUsedAsInputAndOutputInTheSamePassAtTheSameTime=class(EpvFrameGraph);

     EpvFrameGraphRecursion=class(EpvFrameGraph);

     TpvFrameGraph=class
      public
       type TBufferSubresourceRange=record
             public
              Offset:TVkDeviceSize;
              Range:TVkDeviceSize;
              constructor Create(const aOffset,aRange:TVkDeviceSize);
            end;
            PBufferSubresourceRange=^TBufferSubresourceRange;
            TLoadOp=record
             public
              type TKind=
                    (
                     Load,
                     Clear,
                     DontCare
                    );
              const Values:array[TKind] of TVkAttachmentLoadOp=
                     (
                      VK_ATTACHMENT_LOAD_OP_LOAD,
                      VK_ATTACHMENT_LOAD_OP_CLEAR,
                      VK_ATTACHMENT_LOAD_OP_DONT_CARE
                     );
             public
              Kind:TKind;
              ClearColor:TpvVector4;
              constructor Create(const aKind:TKind); overload;
              constructor Create(const aKind:TKind;
                                 const aClearColor:TpvVector4); overload;
            end;
            PLoadOp=^TLoadOp;
            TStoreOp=record
             public
              type TKind=
                    (
                     Store,
                     DontCare
                    );
              const Values:array[TKind] of TVkAttachmentStoreOp=
                     (
                      VK_ATTACHMENT_STORE_OP_STORE,
                      VK_ATTACHMENT_STORE_OP_DONT_CARE
                     );
             public
              Kind:TKind;
              constructor Create(const aKind:TKind);
            end;
            PStoreOp=^TStoreOp;
            TImageType=
             (
              Undefined,
              Surface,
              Color,
              Depth,
              DepthStencil,
              Stencil
             );
            TImageTypeHelper=record helper for TImageType
             public
              function GetAspectMask:TVkImageAspectFlags;
            end;
            PImageType=^TImageType;
            TImageSize=packed record
             public
              type TKind=
                    (
                     Undefined,
                     Absolute,
                     SurfaceDependent
                    );
                   PKind=^TKind;
             public
              Kind:TKind;
              Size:TpvVector4;
              class function CreateEmpty:TImageSize; static;
              constructor Create(const aKind:TImageSize.TKind;
                                 const aWidth:tpvFloat=1.0;
                                 const aHeight:TpvFloat=1.0;
                                 const aDepth:TpvFloat=1.0;
                                 const aLayers:TpvFloat=1.0); overload;
              constructor Create(const aKind:TImageSize.TKind;
                                 const aSize:TpvVector2;
                                 const aDepth:TpvFloat=1.0;
                                 const aLayers:TpvFloat=1.0); overload;
              constructor Create(const aKind:TImageSize.TKind;
                                 const aSize:TpvVector3;
                                 const aLayers:TpvFloat=1.0); overload;
              constructor Create(const aKind:TImageSize.TKind;
                                 const aSize:TpvVector4); overload;
              class operator Equal(const aLeft,aRight:TImageSize):boolean;
              class operator NotEqual(const aLeft,aRight:TImageSize):boolean;
            end;
            PImageSize=^TImageSize;
            TPhysicalPass=class;
            TPhysicalPasses=TpvObjectGenericList<TPhysicalPass>;
            TQueue=class
             private
              fFrameGraph:TpvFrameGraph;
              fPhysicalQueue:TpvVulkanQueue;
              fPhysicalPasses:TPhysicalPasses;
              fCommandPool:TpvVulkanCommandPool;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPhysicalQueue:TpvVulkanQueue); reintroduce;
              destructor Destroy; override;
            end;
            TQueues=TpvObjectGenericList<TQueue>;
            TQueueFamilyIndices=TpvDynamicArray<TVkUInt32>;
            TResourceType=class
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fPersientent:boolean;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aPersientent:boolean); reintroduce; virtual;
              destructor Destroy; override;
             published
              property FrameGraph:TpvFrameGraph read fFrameGraph;
              property Name:TpvRawByteString read fName;
              property Persientent:boolean read fPersientent write fPersientent;
            end;
            TImageResourceType=class(TResourceType)
             private
              fFormat:TVkFormat;
              fSamples:TVkSampleCountFlagBits;
              fImageType:TImageType;
              fImageSize:TImageSize;
              fImageUsage:TVkImageUsageFlags;
              fCountMipMapLevels:TVkUInt32;
              fComponents:TVkComponentMapping;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aPersientent:boolean;
                                 const aFormat:TVkFormat;
                                 const aSamples:TVkSampleCountFlagBits;
                                 const aImageType:TImageType;
                                 const aImageSize:TImageSize;
                                 const aImageUsage:TVkImageUsageFlags;
                                 const aCountMipMapLevels:TVkUInt32;
                                 const aComponents:TVkComponentMapping); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aPersientent:boolean;
                                 const aFormat:TVkFormat;
                                 const aSamples:TVkSampleCountFlagBits;
                                 const aImageType:TImageType;
                                 const aImageSize:TImageSize;
                                 const aImageUsage:TVkImageUsageFlags;
                                 const aCountMipMapLevels:TVkUInt32); reintroduce; overload;
              destructor Destroy; override;
             public
              property ImageType:TImageType read fImageType;
              property ImageSize:TImageSize read fImageSize;
              property Components:TVkComponentMapping read fComponents;
             published
              property Format:TVkFormat read fFormat;
              property Samples:TVkSampleCountFlagBits read fSamples;
              property ImageUsage:TVkImageUsageFlags read fImageUsage;
              property CountMipMapLevels:TVkUInt32 read fCountMipMapLevels;
            end;
            TBufferResourceType=class(TResourceType)
            end;
            TResource=class;
            TResourceList=TpvObjectGenericList<TResource>;
            TResourceNameHashMap=TpvStringHashMap<TResource>;
            TResourceTypeList=TpvObjectGenericList<TResourceType>;
            TResourceTypeNameHashMap=TpvStringHashMap<TResourceType>;
            TResourceTransition=class;
            TResourceTransitionList=TpvObjectGenericList<TResourceTransition>;
            TResourcePhysicalData=class
             private
              fFrameGraph:TpvFrameGraph;
              fResourceType:TResourceType;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); reintroduce; virtual;
              destructor Destroy; override;
              procedure Show; virtual;
              procedure Hide; virtual;
              procedure AfterCreateSwapChain; virtual;
              procedure BeforeDestroySwapChain; virtual;
            end;
            TResourcePhysicalImageData=class(TResourcePhysicalData)
             private
              fIsSurface:boolean;
              fImageUsageFlags:TVkImageUsageFlags;
              fFormat:TVkFormat;
              fExtent:TVkExtent3D;
              fCountMipMaps:TpvSizeInt;
              fCountArrayLayers:TpvSizeInt;
              fSamples:TVkSampleCountFlagBits;
              fTiling:TVkImageTiling;
              fInitialLayout:TVkImageLayout;
              fFirstInitialLayout:TVkImageLayout;
              fImageCreateFlags:TVkImageCreateFlags;
              fImageType:TVkImageType;
              fSharingMode:TVkSharingMode;
              fImageSubresourceRange:TVkImageSubresourceRange;
              fImageViewType:TVkImageViewType;
              fComponents:TVkComponentMapping;
              fVulkanImages:array[0..MaxSwapChainImages-1] of TpvVulkanImage;
              fVulkanImageViews:array[0..MaxSwapChainImages-1] of TpvVulkanImageView;
              fVulkanMemoryBlocks:array[0..MaxSwapChainImages-1] of TpvVulkanDeviceMemoryBlock;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); override;
              destructor Destroy; override;
              procedure Show; override;
              procedure Hide; override;
              procedure AfterCreateSwapChain; override;
              procedure BeforeDestroySwapChain; override;
             published
{             property VulkanImage:TpvVulkanImage read fVulkanImage write fVulkanImage;
              property VulkanImageView:TpvVulkanImageView read fVulkanImageView write fVulkanImageView;
              property VulkanMemoryBlock:TpvVulkanDeviceMemoryBlock read fVulkanMemoryBlock write fVulkanMemoryBlock;}
            end;
            TResourcePhysicalBufferData=class(TResourcePhysicalData)
             // TODO
            end;
            TResourceReuseGroup=class
             private
              fFrameGraph:TpvFrameGraph;
              fResourceType:TResourceType;
              fResources:TResourceList;
              fResourcePhysicalData:TResourcePhysicalData;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); reintroduce;
              destructor Destroy; override;
              procedure Show; virtual;
              procedure Hide; virtual;
              procedure AfterCreateSwapChain; virtual;
              procedure BeforeDestroySwapChain; virtual;
            end;
            TResourceReuseGroupList=TpvObjectGenericList<TResourceReuseGroup>;
            TResource=class
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fResourceType:TResourceType;
              fResourceTransitions:TResourceTransitionList;
              fMinimumPhysicalPassStepIndex:TpvSizeInt;
              fMaximumPhysicalPassStepIndex:TpvSizeInt;
              fResourceReuseGroup:TResourceReuseGroup;
              fAssociatedMemoryData:TObject;
              fUsed:boolean;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aResourceType:TResourceType=nil); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aResourceTypeName:TpvRawByteString); reintroduce; overload;
              destructor Destroy; override;
             published
              property FrameGraph:TpvFrameGraph read fFrameGraph;
              property Name:TpvRawByteString read fName;
              property ResourceType:TResourceType read fResourceType;
              property MinimumPhysicalPassStepIndex:TpvSizeInt read fMinimumPhysicalPassStepIndex;
              property MaximumPhysicalPassStepIndex:TpvSizeInt read fMaximumPhysicalPassStepIndex;
              property AssociatedMemoryData:TObject read fAssociatedMemoryData write fAssociatedMemoryData;
              property Used:boolean read fUsed;
            end;
            TPass=class;
            TResourceTransition=class
             public
              type TKind=
                    (
                     ImageInput,
                     ImageOutput,
                     ImageResolveOutput,
                     ImageDepthOutput,
                     ImageDepthInput,
                     BufferInput,
                     BufferOutput
                    );
                    PKind=^TKind;
                    TKinds=set of TKind;
                    PKinds=^TKinds;
                    TFlag=
                    (
                     FullAreaReadAccess,
                     PreviousFrameInput,
                     NextFrameOutput
                    );
                    PFlag=^TFlag;
                    TFlags=set of TFlag;
                    PFlags=^TFlags;
                const AllImages=
                       [
                        TKind.ImageInput,
                        TKind.ImageOutput,
                        TKind.ImageResolveOutput,
                        TKind.ImageDepthOutput,
                        TKind.ImageDepthInput
                       ];
                      AllImageInputs=
                       [
                        TKind.ImageInput,
                        TKind.ImageDepthInput
                       ];
                      AllImageOutputs=
                       [
                        TKind.ImageOutput,
                        TKind.ImageResolveOutput,
                        TKind.ImageDepthOutput
                       ];
                      AllInputs=
                       [
                        TKind.ImageInput,
                        TKind.ImageDepthInput,
                        TKind.BufferInput
                       ];
                      AllOutputs=
                       [
                        TKind.ImageOutput,
                        TKind.ImageResolveOutput,
                        TKind.ImageDepthOutput,
                        TKind.BufferOutput
                       ];
                      AllInputsOutputs=AllInputs+AllOutputs;
             private
              fFrameGraph:TpvFrameGraph;
              fPass:TPass;
              fResource:TResource;
              fKind:TKind;
              fFlags:TFlags;
              fLayout:TVkImageLayout;
              fLoadOp:TLoadOp;
              fResolveResource:TResource;
              fPipelineStage:TVkPipelineStageFlags;
              fAccessFlags:TVkAccessFlags;
              fBufferSubresourceRange:TBufferSubresourceRange;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPass:TPass;
                                 const aResource:TResource;
                                 const aKind:TKind;
                                 const aFlags:TFlags); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPass:TPass;
                                 const aResource:TResource;
                                 const aKind:TKind;
                                 const aFlags:TFlags;
                                 const aLayout:TVkImageLayout;
                                 const aLoadOp:TLoadOp); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPass:TPass;
                                 const aResource:TResource;
                                 const aKind:TKind;
                                 const aFlags:TFlags;
                                 const aPipelineStage:TVkPipelineStageFlags;
                                 const aAccessFlags:TVkAccessFlags;
                                 const aBufferSubresourceRange:TBufferSubresourceRange); reintroduce; overload;
              destructor Destroy; override;
             public
              property Load:TLoadOp read fLoadOp write fLoadOp;
              property BufferSubresourceRange:TBufferSubresourceRange read fBufferSubresourceRange write fBufferSubresourceRange;
             published
              property FrameGraph:TpvFrameGraph read fFrameGraph;
              property Pass:TPass read fPass;
              property Resource:TResource read fResource;
              property Kind:TKind read fKind;
              property Flags:TFlags read fFlags;
              property Layout:TVkImageLayout read fLayout write fLayout;
              property ResolveResource:TResource read fResolveResource;
              property PipelineStage:TVkPipelineStageFlags read fPipelineStage write fPipelineStage;
              property AccessFlags:TVkAccessFlags read fAccessFlags write fAccessFlags;
            end;
            TPassList=TpvObjectGenericList<TPass>;
            TPassNameHashMap=TpvStringHashMap<TPass>;
            TComputePass=class;
            TRenderPass=class;
            TPhysicalPass=class
             public
              type TPipelineBarrierGroup=class
                    public
                     type TVkMemoryBarrierDynamicArray=TpvDynamicArray<TVkMemoryBarrier>;
                          TVkBufferMemoryBarrierDynamicArray=TpvDynamicArray<TVkBufferMemoryBarrier>;
                          TVkImageMemoryBarrierDynamicArray=TpvDynamicArray<TVkImageMemoryBarrier>;
                          TBarrierMapItemKind=
                           (
                            Memory,
                            Buffer,
                            Image
                           );
                          TBarrierMapItem=record
                           Kind:TBarrierMapItemKind;
                           BarrierIndex:TpvSizeInt;
                           ResourcePhysicalData:TResourcePhysicalData;
                          end;
                          PBarrierMapItem=^TBarrierMapItem;
                          TBarrierMapItemDynamicArray=TpvDynamicArray<TBarrierMapItem>;
                    private
                     fSrcStageMask:TVkPipelineStageFlags;
                     fDstStageMask:TVkPipelineStageFlags;
                     fDependencyFlags:TVkDependencyFlags;
                     fBarrierMapItemDynamicArray:TBarrierMapItemDynamicArray;
                     fMemoryBarrierDynamicArray:TVkMemoryBarrierDynamicArray;
                     fBufferMemoryBarrierDynamicArray:TVkBufferMemoryBarrierDynamicArray;
                     fImageMemoryBarrierDynamicArray:TVkImageMemoryBarrierDynamicArray;
                     fWorkMemoryBarrierDynamicArray:array[0..MaxSwapChainImages-1] of TVkMemoryBarrierDynamicArray;
                     fWorkBufferMemoryBarrierDynamicArray:array[0..MaxSwapChainImages-1] of TVkBufferMemoryBarrierDynamicArray;
                     fWorkImageMemoryBarrierDynamicArray:array[0..MaxSwapChainImages-1] of TVkImageMemoryBarrierDynamicArray;
                    public
                     constructor Create(const aSrcStageMask:TVkPipelineStageFlags;
                                        const aDstStageMask:TVkPipelineStageFlags;
                                        const aDependencyFlags:TVkDependencyFlags);
                     destructor Destroy; override;
                     procedure Show;
                     procedure Hide;
                     procedure AfterCreateSwapChain;
                     procedure BeforeDestroySwapChain;
                    published
                     property SrcStageMask:TVkPipelineStageFlags read fSrcStageMask write fSrcStageMask;
                     property DstStageMask:TVkPipelineStageFlags read fDstStageMask write fDstStageMask;
                     property DependencyFlags:TVkDependencyFlags read fDependencyFlags write fDependencyFlags;
                   end;
                   TPipelineBarrierGroups=TpvObjectGenericList<TPipelineBarrierGroup>;
                   TWaitingSemaphore=record
                    SignallingPhysicalPass:TPhysicalPass;
                    DstStageMask:TVkPipelineStageFlags;
                   end;
                   PWaitingSemaphore=^TWaitingSemaphore;
                   TWaitingSemaphores=TpvDynamicArray<TWaitingSemaphore>;
                   TWaitingSemaphoreHandles=TpvDynamicArray<TVkSemaphore>;
                   TWaitingSemaphoreDstStageMasks=TpvDynamicArray<TVkPipelineStageFlags>;
             private
              fFrameGraph:TpvFrameGraph;
              fIndex:TpvSizeInt;
              fProcessed:boolean;
              fQueue:TQueue;
              fInputDependencies:TPhysicalPasses;
              fOutputDependencies:TPhysicalPasses;
              fBeforePipelineBarrierGroups:TPipelineBarrierGroups;
              fAfterPipelineBarrierGroups:TPipelineBarrierGroups;
              fCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;
              fSignallingSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
              fWaitingSemaphores:TWaitingSemaphores;
              fWaitingSemaphoreHandles:TWaitingSemaphoreHandles;
              fWaitingSemaphoreDstStageMasks:TWaitingSemaphoreDstStageMasks;
              fSubmitInfos:array[0..MaxSwapChainImages-1] of TVkSubmitInfo;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;const aQueue:TQueue); reintroduce; virtual;
              destructor Destroy; override;
              procedure Show; virtual;
              procedure Hide; virtual;
              procedure AfterCreateSwapChain; virtual;
              procedure BeforeDestroySwapChain; virtual;
            end;
            TPhysicalComputePass=class(TPhysicalPass)
             private
              fComputePass:TComputePass;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;const aComputePass:TComputePass); reintroduce;
              destructor Destroy; override;
              procedure Show; override;
              procedure Hide; override;
              procedure AfterCreateSwapChain; override;
              procedure BeforeDestroySwapChain; override;
            end;
            TPhysicalRenderPass=class(TPhysicalPass)
             public
              type TSubPass=class;
                   TSubPasses=TpvObjectGenericList<TSubPass>;
                   TSubPassDependency=record
                    SrcSubPass:TSubPass;
                    DstSubPass:TSubPass;
                    SrcStageMask:TVkPipelineStageFlags;
                    DstStageMask:TVkPipelineStageFlags;
                    SrcAccessMask:TVkAccessFlags;
                    DstAccessMask:TVkAccessFlags;
                    DependencyFlags:TVkDependencyFlags;
                   end;
                   PSubPassDependency=^TSubPassDependency;
                   TSubPassDependencies=TpvDynamicArray<TSubPassDependency>;
                   TSubPass=class
                    private
                     fPhysicalRenderPass:TPhysicalRenderPass;
                     fIndex:TpvSizeInt;
                     fRenderPass:TRenderPass;
                    public
                     constructor Create(const aPhysicalRenderPass:TPhysicalRenderPass;
                                        const aRenderPass:TRenderPass); reintroduce;
                     destructor Destroy; override;
                     procedure Show; virtual;
                     procedure Hide; virtual;
                     procedure AfterCreateSwapChain; virtual;
                     procedure BeforeDestroySwapChain; virtual;
                   end;
             private
              fSubPasses:TSubPasses;
              fSubPassDependencies:TSubPassDependencies;
              fMultiView:boolean;
              fVulkanRenderPass:TpvVulkanRenderPass;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;const aQueue:TQueue); override;
              destructor Destroy; override;
              procedure Show; override;
              procedure Hide; override;
              procedure AfterCreateSwapChain; override;
              procedure BeforeDestroySwapChain; override;
            end;
            TPass=class
             public
              type TFlag=
                    (
                     Toggleable,
                     Enabled,
                     Used,
                     Processed,
                     Marked,
                     SubPass
                    );
                   PFlag=^TFlag;
                   TFlags=set of TFlag;
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fFlags:TFlags;
              fQueue:TQueue;
              fResources:TResourceList;
              fResourceTransitions:TResourceTransitionList;
              fPreviousPasses:TPassList;
              fNextPasses:TPassList;
              fIndex:TpvSizeInt;
              fTag:TpvSizeInt;
              fPhysicalPass:TPhysicalPass;
              fTopologicalSortIndex:TpvSizeInt;
              fDoubleBufferedEnabledState:array[0..1] of longbool;
              function GetEnabled:boolean;
              procedure SetEnabled(const aEnabled:boolean);
              procedure SetName(const aName:TpvRawByteString);
              function AddImageResource(const aResourceTypeName:TpvRawByteString;
                                        const aResourceName:TpvRawByteString;
                                        const aKind:TResourceTransition.TKind;
                                        const aFlags:TResourceTransition.TFlags;
                                        const aLayout:TVkImageLayout;
                                        const aLoadOp:TLoadOp):TResourceTransition; overload;
              function AddBufferResource(const aResourceTypeName:TpvRawByteString;
                                         const aResourceName:TpvRawByteString;
                                         const aKind:TResourceTransition.TKind;
                                         const aFlags:TResourceTransition.TFlags;
                                         const aPipelineStage:TVkPipelineStageFlags;
                                         const aAccessFlags:TVkAccessFlags;
                                         const aBufferSubresourceRange:TBufferSubresourceRange):TResourceTransition; overload;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); reintroduce; virtual;
              destructor Destroy; override;
              procedure AddImageInput(const aResourceTypeName:TpvRawByteString;
                                      const aResourceName:TpvRawByteString;
                                      const aLayout:TVkImageLayout;
                                      const aFlags:TResourceTransition.TFlags=[]);
              procedure AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aLayout:TVkImageLayout;
                                       const aLoadOp:TLoadOp;
                                       const aFlags:TResourceTransition.TFlags=[]);
              procedure AddImageResolveOutput(const aResourceTypeName:TpvRawByteString;
                                              const aResourceName:TpvRawByteString;
                                              const aResourceSourceName:TpvRawByteString;
                                              const aLayout:TVkImageLayout;
                                              const aLoadOp:TLoadOp;
                                              const aFlags:TResourceTransition.TFlags=[]);
              procedure AddImageDepthInput(const aResourceTypeName:TpvRawByteString;
                                           const aResourceName:TpvRawByteString;
                                           const aLayout:TVkImageLayout;
                                           const aFlags:TResourceTransition.TFlags=[]);
              procedure AddImageDepthOutput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aLoadOp:TLoadOp;
                                            const aFlags:TResourceTransition.TFlags=[]);
              procedure AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aPipelineStage:TVkPipelineStageFlags;
                                       const aAccessFlags:TVkAccessFlags;
                                       const aBufferSubresourceRange:TBufferSubresourceRange;
                                       const aFlags:TResourceTransition.TFlags=[]); overload;
              procedure AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aPipelineStage:TVkPipelineStageFlags;
                                       const aAccessFlags:TVkAccessFlags;
                                       const aFlags:TResourceTransition.TFlags=[]); overload;
              procedure AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                        const aResourceName:TpvRawByteString;
                                        const aPipelineStage:TVkPipelineStageFlags;
                                        const aAccessFlags:TVkAccessFlags;
                                        const aBufferSubresourceRange:TBufferSubresourceRange;
                                        const aFlags:TResourceTransition.TFlags=[]); overload;
              procedure AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                        const aResourceName:TpvRawByteString;
                                        const aPipelineStage:TVkPipelineStageFlags;
                                        const aAccessFlags:TVkAccessFlags;
                                        const aFlags:TResourceTransition.TFlags=[]); overload;
             public
              procedure Setup; virtual;
              procedure Execute; virtual;
             published
              property FrameGraph:TpvFrameGraph read fFrameGraph;
              property Name:TpvRawByteString read fName write SetName;
              property Queue:TQueue read fQueue write fQueue;
              property Enabled:boolean read GetEnabled write SetEnabled;
            end;
            TComputePass=class(TPass)
             private
             public
             published
            end;
            TRenderPass=class(TPass)
             private
              fMultiViewMask:TpvUInt32;
              fSize:TImageSize;
              fPhysicalRenderPassSubPass:TPhysicalRenderPass.TSubPass;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); override;
              destructor Destroy; override;
             public
              property Size:TImageSize read fSize write fSize;
             published
              property MultiViewMask:TpvUInt32 read fMultiViewMask write fMultiViewMask;
            end;
      private
       fVulkanDevice:TpvVulkanDevice;
       fSurfaceWidth:TpvSizeInt;
       fSurfaceHeight:TpvSizeInt;
       fSurfaceColorFormat:TVkFormat;
       fSurfaceDepthFormat:TVkFormat;
       fSurfaceImages:array[0..MaxSwapChainImages-1] of TpvVulkanImage;
       fCountSwapChainImages:TpvSizeInt;
       fQueues:TQueues;
       fQueueFamilyIndices:TQueueFamilyIndices;
       fUniversalQueue:TQueue;
       fGraphicsQueue:TQueue;
       fComputeQueue:TQueue;
       fTransferQueue:TQueue;
       fPresentQueue:TQueue;
       fResourceTypes:TResourceTypeList;
       fResourceTypeNameHashMap:TResourceTypeNameHashMap;
       fResources:TResourceList;
       fResourceNameHashMap:TResourceNameHashMap;
       fResourceTransitions:TResourceTransitionList;
       fResourceReuseGroups:TResourceReuseGroupList;
       fPasses:TPassList;
       fPassNameHashMap:TPassNameHashMap;
       fTopologicalSortedPasses:TPassList;
       fRootPass:TPass;
       fEnforcedRootPass:TPass;
       fMaximumOverallPhysicalPassIndex:TpvSizeInt;
       fValid:boolean;
       fCanDoParallelProcessing:boolean;
       fPhysicalPasses:TPhysicalPasses;
       fRootPhysicalPass:TPhysicalPass;
       fVulkanUniversalQueueCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanUniversalQueueCommandBufferFence:TpvVulkanFence;
       fDrawFrameIndex:TpvSizeInt;
      public
       constructor Create(const aVulkanDevice:TpvVulkanDevice);
       destructor Destroy; override;
      public
       procedure SetSwapChain(const aSwapChain:TpvVulkanSwapChain;
                              const aSurfaceDepthFormat:TVkFormat);
       function AddQueue(const aPhysicalQueue:TpvVulkanQueue):TQueue;
       function AddImageResourceType(const aName:TpvRawByteString;
                                     const aPersientent:boolean;
                                     const aFormat:TVkFormat;
                                     const aSamples:TVkSampleCountFlagBits;
                                     const aImageType:TImageType;
                                     const aImageSize:TImageSize;
                                     const aImageUsage:TVkImageUsageFlags;
                                     const aCountMipMapLevels:TVkUInt32;
                                     const aComponents:TVkComponentMapping):TResourceType; overload;
       function AddImageResourceType(const aName:TpvRawByteString;
                                     const aPersientent:boolean;
                                     const aFormat:TVkFormat;
                                     const aSamples:TVkSampleCountFlagBits;
                                     const aImageType:TImageType;
                                     const aImageSize:TImageSize;
                                     const aImageUsage:TVkImageUsageFlags;
                                     const aCountMipMapLevels:TVkUInt32):TResourceType; overload;
      public
       procedure Setup; virtual;
       procedure Compile; virtual;
       procedure Show; virtual;
       procedure Hide; virtual;
       procedure AfterCreateSwapChain; virtual;
       procedure BeforeDestroySwapChain; virtual;
      private
       procedure ExecuteQueue(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aQueue:TQueue);
       procedure ExecuteQueueParallelForJobMethod(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
      public
       procedure Update(const aUpdateFrameIndex:TpvSizeInt); virtual;
       procedure Draw(const aDrawFrameIndex:TpvSizeInt); virtual;
      published
       property CanDoParallelProcessing:boolean read fCanDoParallelProcessing write fCanDoParallelProcessing;
       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;
       property SurfaceWidth:TpvSizeInt read fSurfaceWidth write fSurfaceWidth;
       property SurfaceHeight:TpvSizeInt read fSurfaceHeight write fSurfaceHeight;
       property SurfaceColorFormat:TVkFormat read fSurfaceColorFormat write fSurfaceColorFormat;
       property SurfaceDepthFormat:TVkFormat read fSurfaceDepthFormat write fSurfaceDepthFormat;
       property CountSwapChainImages:TpvSizeInt read fCountSwapChainImages write fCountSwapChainImages;
       property Queues:TQueues read fQueues;
       property UniversalQueue:TQueue read fUniversalQueue;
       property GraphicsQueue:TQueue read fGraphicsQueue;
       property ComputeQueue:TQueue read fComputeQueue;
       property TransferQueue:TQueue read fTransferQueue;
       property PresentQueue:TQueue read fPresentQueue;
       property ResourceTypes:TResourceTypeList read fResourceTypes;
       property ResourceTypeByName:TResourceTypeNameHashMap read fResourceTypeNameHashMap;
       property Resources:TResourceList read fResources;
       property ResourceByName:TResourceNameHashMap read fResourceNameHashMap;
       property Passes:TPassList read fPasses;
       property PassByName:TPassNameHashMap read fPassNameHashMap;
       property RootPass:TPass read fRootPass;
       property EnforcedRootPass:TPass read fEnforcedRootPass write fEnforcedRootPass;
       property RootPhysicalPass:TPhysicalPass read fRootPhysicalPass;
     end;

implementation

function ComparePhysicalRenderPassSubPassDependencies(const a,b:TpvFrameGraph.TPhysicalRenderPass.TSubpassDependency):TpvInt32;
begin
 if a.SrcSubPass.fIndex<b.SrcSubPass.fIndex then begin
  result:=-1;
 end else if a.SrcSubPass.fIndex>b.SrcSubPass.fIndex then begin
  result:=1;
 end else if a.DstSubPass.fIndex<b.DstSubPass.fIndex then begin
  result:=-1;
 end else if a.DstSubPass.fIndex>b.DstSubPass.fIndex then begin
  result:=1;
 end else begin
  result:=0;
 end;
end;

{ TpvFrameGraph.TBufferSubresourceRange }

constructor TpvFrameGraph.TBufferSubresourceRange.Create(const aOffset,aRange:TVkDeviceSize);
begin
 Offset:=aOffset;
 Range:=aRange;
end;

{ TpvFrameGraph.TLoadOp }

constructor TpvFrameGraph.TLoadOp.Create(const aKind:TKind);
begin
 Kind:=aKind;
 ClearColor:=TpvVector4.InlineableCreate(1.0,1.0,1.0,1.0);
end;

constructor TpvFrameGraph.TLoadOp.Create(const aKind:TKind;const aClearColor:TpvVector4);
begin
 Kind:=aKind;
 ClearColor:=aClearColor;
end;

{ TpvFrameGraph.TStoreOp }

constructor TpvFrameGraph.TStoreOp.Create(const aKind:TKind);
begin
 Kind:=aKind;
end;

{ TpvFrameGraph.TImageTypeHelper }

function TpvFrameGraph.TImageTypeHelper.GetAspectMask:TVkImageAspectFlags;
begin
 case self of
  TpvFrameGraph.TImageType.Surface:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  end;
  TpvFrameGraph.TImageType.Color:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  end;
  TpvFrameGraph.TImageType.Depth:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT);
  end;
  TpvFrameGraph.TImageType.DepthStencil:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT) or TVkImageAspectFlags(VK_IMAGE_ASPECT_STENCIL_BIT);
  end;
  TpvFrameGraph.TImageType.Stencil:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_STENCIL_BIT);
  end;
  else {TpvFrameGraph.TImageType.Undefined:}begin
   result:=TVkImageAspectFlags(0);
  end;
 end;
end;

{ TpvFrameGraph.TImageSize }

class function TpvFrameGraph.TImageSize.CreateEmpty:TImageSize;
begin
 result.Kind:=TpvFrameGraph.TImageSize.TKind.Undefined;
 result.Size:=TpvVector4.Null;
end;

constructor TpvFrameGraph.TImageSize.Create(const aKind:TImageSize.TKind;
                                            const aWidth:TpvFloat=1.0;
                                            const aHeight:TpvFloat=1.0;
                                            const aDepth:TpvFloat=1.0;
                                            const aLayers:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector4.InlineableCreate(aWidth,aHeight,aDepth,aLayers);
end;

constructor TpvFrameGraph.TImageSize.Create(const aKind:TImageSize.TKind;
                                            const aSize:TpvVector2;
                                            const aDepth:TpvFloat=1.0;
                                            const aLayers:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector4.InlineableCreate(aSize.x,aSize.y,aDepth,aLayers);
end;

constructor TpvFrameGraph.TImageSize.Create(const aKind:TImageSize.TKind;
                                            const aSize:TpvVector3;
                                            const aLayers:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector4.InlineableCreate(aSize,aLayers);
end;

constructor TpvFrameGraph.TImageSize.Create(const aKind:TImageSize.TKind;
                                            const aSize:TpvVector4);
begin
 Kind:=aKind;
 Size:=aSize;
end;

class operator TpvFrameGraph.TImageSize.Equal(const aLeft,aRight:TImageSize):boolean;
begin
 result:=(aLeft.Kind=aRight.Kind) and
         (aLeft.Size=aRight.Size);
end;

class operator TpvFrameGraph.TImageSize.NotEqual(const aLeft,aRight:TImageSize):boolean;
begin
 result:=(aLeft.Kind<>aRight.Kind) or
         (aLeft.Size<>aRight.Size);
end;

{ TpvFrameGraph.TQueue }

constructor TpvFrameGraph.TQueue.Create(const aFrameGraph:TpvFrameGraph;
                                        const aPhysicalQueue:TpvVulkanQueue);
begin

 inherited Create;

 fFrameGraph:=aFrameGraph;

 fPhysicalQueue:=aPhysicalQueue;

 fPhysicalPasses:=TPhysicalPasses.Create;
 fPhysicalPasses.OwnsObjects:=false;

 fCommandPool:=TpvVulkanCommandPool.Create(fFrameGraph.fVulkanDevice,
                                           fPhysicalQueue.QueueFamilyIndex,
                                           TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

end;

destructor TpvFrameGraph.TQueue.Destroy;
begin

 FreeAndNil(fPhysicalPasses);

 FreeAndNil(fCommandPool);

 inherited Destroy;

end;

{ TpvFrameGraph.TResourceType }

constructor TpvFrameGraph.TResourceType.Create;
begin
 inherited Create;
 if length(trim(String(aName)))=0 then begin
  raise EpvFrameGraphEmptyName.Create('Empty name');
 end;
 if aFrameGraph.fResourceTypeNameHashMap.ExistKey(aName) then begin
  raise EpvFrameGraphDuplicateName.Create('Duplicate name');
 end;
 fFrameGraph:=aFrameGraph;
 fName:=aName;
 fFrameGraph.fResourceTypes.Add(self);
 fFrameGraph.fResourceTypeNameHashMap.Add(fName,self);
 fPersientent:=aPersientent;
end;

destructor TpvFrameGraph.TResourceType.Destroy;
begin
 inherited Destroy;
end;

{ TpvFrameGraph.TImageResourceType }

constructor TpvFrameGraph.TImageResourceType.Create(const aFrameGraph:TpvFrameGraph;
                                                    const aName:TpvRawByteString;
                                                    const aPersientent:boolean;
                                                    const aFormat:TVkFormat;
                                                    const aSamples:TVkSampleCountFlagBits;
                                                    const aImageType:TImageType;
                                                    const aImageSize:TImageSize;
                                                    const aImageUsage:TVkImageUsageFlags;
                                                    const aCountMipMapLevels:TVkUInt32;
                                                    const aComponents:TVkComponentMapping);
begin
 Create(aFrameGraph,
        aName,
        aPersientent);
 fFormat:=aFormat;
 fSamples:=aSamples;
 fImageType:=aImageType;
 fImageSize:=aImageSize;
 fImageUsage:=aImageUsage;
 fCountMipMapLevels:=aCountMipMapLevels;
 fComponents:=aComponents;
end;

constructor TpvFrameGraph.TImageResourceType.Create(const aFrameGraph:TpvFrameGraph;
                                                    const aName:TpvRawByteString;
                                                    const aPersientent:boolean;
                                                    const aFormat:TVkFormat;
                                                    const aSamples:TVkSampleCountFlagBits;
                                                    const aImageType:TImageType;
                                                    const aImageSize:TImageSize;
                                                    const aImageUsage:TVkImageUsageFlags;
                                                    const aCountMipMapLevels:TVkUInt32);
begin
 Create(aFrameGraph,
        aName,
        aPersientent,
        aFormat,
        aSamples,
        aImageType,
        aImageSize,
        aImageUsage,
        aCountMipMapLevels,
        TVkComponentMapping.Create(VK_COMPONENT_SWIZZLE_R,
                                   VK_COMPONENT_SWIZZLE_G,
                                   VK_COMPONENT_SWIZZLE_B,
                                   VK_COMPONENT_SWIZZLE_A));
end;


destructor TpvFrameGraph.TImageResourceType.Destroy;
begin
 inherited Destroy;
end;

{ TpvFrameGraph.TResourcePhysicalData }

constructor TpvFrameGraph.TResourcePhysicalData.Create(const aFrameGraph:TpvFrameGraph);
begin
 inherited Create;
 fFrameGraph:=aFrameGraph;
end;

destructor TpvFrameGraph.TResourcePhysicalData.Destroy;
begin
 inherited Destroy;
end;

procedure TpvFrameGraph.TResourcePhysicalData.Show;
begin
end;

procedure TpvFrameGraph.TResourcePhysicalData.Hide;
begin
end;

procedure TpvFrameGraph.TResourcePhysicalData.AfterCreateSwapChain;
begin
end;

procedure TpvFrameGraph.TResourcePhysicalData.BeforeDestroySwapChain;
begin
end;

{ TpvFrameGraph.TResourcePhysicalImageData }

constructor TpvFrameGraph.TResourcePhysicalImageData.Create(const aFrameGraph:TpvFrameGraph);
begin
 inherited Create(aFrameGraph);
 fIsSurface:=false;
 fImageUsageFlags:=TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT);
 fFormat:=VK_FORMAT_B8G8R8A8_UNORM;
 fExtent:=TVkExtent3D.Create(1,1,1);
 fCountMipMaps:=1;
 fCountArrayLayers:=1;
 fSamples:=TVkSampleCountFlagBits(VK_SAMPLE_COUNT_1_BIT);
 fTiling:=VK_IMAGE_TILING_OPTIMAL;
 fInitialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
 fImageCreateFlags:=TVkImageCreateFlags(0);
 fImageType:=VK_IMAGE_TYPE_2D;
 fSharingMode:=VK_SHARING_MODE_EXCLUSIVE;
end;

destructor TpvFrameGraph.TResourcePhysicalImageData.Destroy;
var SwapChainImageIndex:TpvSizeInt;
begin
 if fIsSurface then begin
  for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
   fVulkanImageViews[SwapChainImageIndex]:=nil;
   fVulkanImages[SwapChainImageIndex]:=nil;
   fVulkanMemoryBlocks[SwapChainImageIndex]:=nil;
  end;
 end else begin
  for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
   FreeAndNil(fVulkanImageViews[SwapChainImageIndex]);
   FreeAndNil(fVulkanImages[SwapChainImageIndex]);
   FreeAndNil(fVulkanMemoryBlocks[SwapChainImageIndex]);
  end;
 end;
 inherited Destroy;
end;

procedure TpvFrameGraph.TResourcePhysicalImageData.Show;
begin

end;

procedure TpvFrameGraph.TResourcePhysicalImageData.Hide;
begin

end;

procedure TpvFrameGraph.TResourcePhysicalImageData.AfterCreateSwapChain;
var SwapChainImageIndex:TpvSizeInt;
    ImageResourceType:TImageResourceType;
    MemoryRequirements:TVkMemoryRequirements;
    RequiresDedicatedAllocation,
    PrefersDedicatedAllocation:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    MemoryAllocationType:TpvVulkanDeviceMemoryAllocationType;
    Format:TVkFormat;
begin

 Assert(fResourceType is TImageResourceType);

 ImageResourceType:=TImageResourceType(fResourceType);

 case ImageResourceType.fImageSize.Kind of
  TpvFrameGraph.TImageSize.TKind.Absolute:begin
   fExtent.width:=Max(1,trunc(ImageResourceType.fImageSize.Size.x));
   fExtent.height:=Max(1,trunc(ImageResourceType.fImageSize.Size.y));
   fExtent.depth:=Max(1,trunc(ImageResourceType.fImageSize.Size.z));
  end;
  TpvFrameGraph.TImageSize.TKind.SurfaceDependent:begin
   fExtent.width:=Max(1,trunc(ImageResourceType.fImageSize.Size.x*fFrameGraph.fSurfaceWidth));
   fExtent.height:=Max(1,trunc(ImageResourceType.fImageSize.Size.y*fFrameGraph.fSurfaceHeight));
   fExtent.depth:=Max(1,trunc(ImageResourceType.fImageSize.Size.z));
  end;
  else {TpvFrameGraph.TImageSize.TKind.Undefined:}begin
  end;
 end;

 if fIsSurface then begin

  for SwapChainImageIndex:=0 to Min(Max(fFrameGraph.fCountSwapChainImages,1),MaxSwapChainImages)-1 do begin
   fVulkanImageViews[SwapChainImageIndex]:=fFrameGraph.fSurfaceImages[SwapChainImageIndex].ImageView;
   fVulkanImages[SwapChainImageIndex]:=fFrameGraph.fSurfaceImages[SwapChainImageIndex];
   fVulkanMemoryBlocks[SwapChainImageIndex]:=nil;
  end;

 end else begin

  Format:=fFormat;

  if Format=VK_FORMAT_UNDEFINED then begin
   case (fResourceType as TImageResourceType).fImageType of
    TImageType.Color:begin
     Format:=fFrameGraph.fSurfaceColorFormat;
    end;
    TImageType.Depth,
    TImageType.DepthStencil,
    TImageType.Stencil:begin
     Format:=fFrameGraph.fSurfaceDepthFormat;
    end;
   end;
  end;

  for SwapChainImageIndex:=0 to Min(Max(fFrameGraph.fCountSwapChainImages,1),MaxSwapChainImages)-1 do begin

   fVulkanImages[SwapChainImageIndex]:=TpvVulkanImage.Create(fFrameGraph.fVulkanDevice,
                                                             0,
                                                             fImageType,
                                                             Format,
                                                             fExtent.width,
                                                             fExtent.height,
                                                             fExtent.depth,
                                                             fCountMipMaps,
                                                             fCountArrayLayers,
                                                             fSamples,
                                                             fTiling,
                                                             fImageUsageFlags, // or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                                             fSharingMode,
                                                             fFrameGraph.fQueueFamilyIndices.Count,
                                                             @fFrameGraph.fQueueFamilyIndices.Items[0],
                                                             VK_IMAGE_LAYOUT_UNDEFINED);

   MemoryRequirements:=fFrameGraph.fVulkanDevice.MemoryManager.GetImageMemoryRequirements(fVulkanImages[SwapChainImageIndex].Handle,
                                                                                          RequiresDedicatedAllocation,
                                                                                          PrefersDedicatedAllocation);

   MemoryBlockFlags:=[];

   if RequiresDedicatedAllocation or PrefersDedicatedAllocation then begin
    Include(MemoryBlockFlags,TpvVulkanDeviceMemoryBlockFlag.DedicatedAllocation);
   end;

   if fTiling=VK_IMAGE_TILING_OPTIMAL then begin
    MemoryAllocationType:=TpvVulkanDeviceMemoryAllocationType.ImageOptimal;
   end else begin
    MemoryAllocationType:=TpvVulkanDeviceMemoryAllocationType.ImageLinear;
   end;

   fVulkanMemoryBlocks[SwapChainImageIndex]:=fFrameGraph.fVulkanDevice.MemoryManager.AllocateMemoryBlock(MemoryBlockFlags,
                                                                                                        MemoryRequirements.size,
                                                                                                        MemoryRequirements.alignment,
                                                                                                        MemoryRequirements.memoryTypeBits,
                                                                                                        TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                                                                        0,
                                                                                                        0,
                                                                                                        0,
                                                                                                        0,
                                                                                                        0,
                                                                                                        MemoryAllocationType,
                                                                                                        @fVulkanImages[SwapChainImageIndex].Handle);
   if not assigned(fVulkanMemoryBlocks[SwapChainImageIndex]) then begin
    raise EpvVulkanMemoryAllocationException.Create('Memory for image resource couldn''t be allocated!');
   end;

   VulkanCheckResult(fFrameGraph.fVulkanDevice.Commands.BindImageMemory(fVulkanImages[SwapChainImageIndex].Device.Handle,
                                                                        fVulkanImages[SwapChainImageIndex].Handle,
                                                                        fVulkanMemoryBlocks[SwapChainImageIndex].MemoryChunk.Handle,
                                                                        fVulkanMemoryBlocks[SwapChainImageIndex].Offset));

   fVulkanImageViews[SwapChainImageIndex]:=TpvVulkanImageView.Create(fFrameGraph.fVulkanDevice,
                                                                     fVulkanImages[SwapChainImageIndex],
                                                                     fImageViewType,
                                                                     Format,
                                                                     fComponents.r,
                                                                     fComponents.g,
                                                                     fComponents.b,
                                                                     fComponents.a,
                                                                     fImageSubresourceRange.aspectMask,
                                                                     0,
                                                                     fCountMipMaps,
                                                                     0,
                                                                     fCountArrayLayers);

   if fFirstInitialLayout<>VK_IMAGE_LAYOUT_UNDEFINED then begin
    if (fImageUsageFlags and TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT))<>0 then begin
     fVulkanImages[SwapChainImageIndex].SetLayout(fImageSubresourceRange.aspectMask,
                                                  VK_IMAGE_LAYOUT_UNDEFINED,
                                                  fFirstInitialLayout,
                                                  TVkAccessFlags(0),
                                                  TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or
                                                  TVkAccessFlags(VK_ACCESS_INPUT_ATTACHMENT_READ_BIT),
                                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                                  fFrameGraph.fVulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                                  nil,
                                                  fFrameGraph.fVulkanUniversalQueueCommandBuffer,
                                                  fFrameGraph.fUniversalQueue.fPhysicalQueue,
                                                  fFrameGraph.fVulkanUniversalQueueCommandBufferFence,
                                                  true);
    end else begin
     case fFirstInitialLayout of
      VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
       fVulkanImages[SwapChainImageIndex].SetLayout(fImageSubresourceRange.aspectMask,
                                                    VK_IMAGE_LAYOUT_UNDEFINED,
                                                    VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                    TVkAccessFlags(0),
                                                    TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                    nil,
                                                    fFrameGraph.fVulkanUniversalQueueCommandBuffer,
                                                    fFrameGraph.fUniversalQueue.fPhysicalQueue,
                                                    fFrameGraph.fVulkanUniversalQueueCommandBufferFence,
                                                    true);
      end;
      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
       fVulkanImages[SwapChainImageIndex].SetLayout(fImageSubresourceRange.aspectMask,
                                                    VK_IMAGE_LAYOUT_UNDEFINED,
                                                    VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                    TVkAccessFlags(0),
                                                    TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                                                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                                    nil,
                                                    fFrameGraph.fVulkanUniversalQueueCommandBuffer,
                                                    fFrameGraph.fUniversalQueue.fPhysicalQueue,
                                                    fFrameGraph.fVulkanUniversalQueueCommandBufferFence,
                                                    true);
      end;
      else begin
       raise EpvVulkanException.Create('Invalid frame buffer attachment');
      end;
     end;
    end;
   end;

  end;

 end;

end;

procedure TpvFrameGraph.TResourcePhysicalImageData.BeforeDestroySwapChain;
var SwapChainImageIndex:TpvSizeInt;
begin
 if fIsSurface then begin
  for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
   fVulkanImageViews[SwapChainImageIndex]:=nil;
   fVulkanImages[SwapChainImageIndex]:=nil;
   fVulkanMemoryBlocks[SwapChainImageIndex]:=nil;
  end;
 end else begin
  for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
   FreeAndNil(fVulkanImageViews[SwapChainImageIndex]);
   FreeAndNil(fVulkanImages[SwapChainImageIndex]);
   FreeAndNil(fVulkanMemoryBlocks[SwapChainImageIndex]);
  end;
 end;
end;

{ TpvFrameGraph.TResourceReuseGroup }

constructor TpvFrameGraph.TResourceReuseGroup.Create(const aFrameGraph:TpvFrameGraph);
begin
 inherited Create;
 fFrameGraph:=aFrameGraph;
 fFrameGraph.fResourceReuseGroups.Add(self);
 fResourceType:=nil;
 fResources:=TResourceList.Create;
 fResources.OwnsObjects:=false;
 fResourcePhysicalData:=nil;
end;

destructor TpvFrameGraph.TResourceReuseGroup.Destroy;
begin
 FreeAndNil(fResources);
 FreeAndNil(fResourcePhysicalData);
 inherited Destroy;
end;

procedure TpvFrameGraph.TResourceReuseGroup.Show;
begin
 if assigned(fResourcePhysicalData) then begin
  fResourcePhysicalData.Show;
 end;
end;

procedure TpvFrameGraph.TResourceReuseGroup.Hide;
begin
 if assigned(fResourcePhysicalData) then begin
  fResourcePhysicalData.Hide;
 end;
end;

procedure TpvFrameGraph.TResourceReuseGroup.AfterCreateSwapChain;
begin
 if assigned(fResourcePhysicalData) then begin
  fResourcePhysicalData.AfterCreateSwapChain;
 end;
end;

procedure TpvFrameGraph.TResourceReuseGroup.BeforeDestroySwapChain;
begin
 if assigned(fResourcePhysicalData) then begin
  fResourcePhysicalData.BeforeDestroySwapChain;
 end;
end;

{ TpvFrameGraph.TResource }

constructor TpvFrameGraph.TResource.Create(const aFrameGraph:TpvFrameGraph;
                                           const aName:TpvRawByteString;
                                           const aResourceType:TResourceType=nil);
begin

 inherited Create;

 if length(trim(String(aName)))=0 then begin
  raise EpvFrameGraphEmptyName.Create('Empty name');
 end;

 if aFrameGraph.fResourceNameHashMap.ExistKey(aName) then begin
  raise EpvFrameGraphDuplicateName.Create('Duplicate name');
 end;

 fFrameGraph:=aFrameGraph;

 fName:=aName;

 fResourceType:=aResourceType;

 fResourceTransitions:=TResourceTransitionList.Create;
 fResourceTransitions.OwnsObjects:=false;

 fMinimumPhysicalPassStepIndex:=High(TpvSizeInt);
 fMaximumPhysicalPassStepIndex:=Low(TpvSizeInt);

 fUsed:=false;

 fFrameGraph.fResources.Add(self);

 fFrameGraph.fResourceNameHashMap.Add(fName,self);
end;

constructor TpvFrameGraph.TResource.Create(const aFrameGraph:TpvFrameGraph;
                                           const aName:TpvRawByteString;
                                           const aResourceTypeName:TpvRawByteString);
begin
 Create(aFrameGraph,aName,aFrameGraph.ResourceTypeByName[aResourceTypeName]);
end;

destructor TpvFrameGraph.TResource.Destroy;
begin
 FreeAndNil(fResourceTransitions);
 inherited Destroy;
end;

{ TpvFrameGraph.TResourceTransition }

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aKind:TKind;
                                                     const aFlags:TFlags);
begin
 inherited Create;
 fFrameGraph:=aFrameGraph;
 fFrameGraph.fResourceTransitions.Add(self);
 fPass:=aPass;
 fResource:=aResource;
 fKind:=aKind;
 fFlags:=aFlags;
 fResource.fResourceTransitions.Add(self);
 fPass.fResourceTransitions.Add(self);
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aKind:TKind;
                                                     const aFlags:TFlags;
                                                     const aLayout:TVkImageLayout;
                                                     const aLoadOp:TLoadOp);
begin
 Create(aFrameGraph,aPass,aResource,aKind,aFlags);
 fLayout:=aLayout;
 fLoadOp:=aLoadOp;
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aKind:TKind;
                                                     const aFlags:TFlags;
                                                     const aPipelineStage:TVkPipelineStageFlags;
                                                     const aAccessFlags:TVkAccessFlags;
                                                     const aBufferSubresourceRange:TBufferSubresourceRange);
begin
 Create(aFrameGraph,aPass,aResource,aKind,aFlags);
 fPipelineStage:=aPipelineStage;
 fAccessFlags:=aAccessFlags;
 fBufferSubresourceRange:=aBufferSubresourceRange;
end;

destructor TpvFrameGraph.TResourceTransition.Destroy;
begin
 inherited Destroy;
end;

{ TpvFrameGraph.TPass }

constructor TpvFrameGraph.TPass.Create(const aFrameGraph:TpvFrameGraph);
begin

 inherited Create;

 fFrameGraph:=aFrameGraph;
 fName:='';

 fFrameGraph.fPasses.Add(self);

 fQueue:=fFrameGraph.fUniversalQueue;

 fResources:=TResourceList.Create;
 fResources.OwnsObjects:=false;

 fResourceTransitions:=TResourceTransitionList.Create;
 fResourceTransitions.OwnsObjects:=false;

 fPreviousPasses:=TPassList.Create;
 fPreviousPasses.OwnsObjects:=false;

 fNextPasses:=TPassList.Create;
 fNextPasses.OwnsObjects:=false;

 fFlags:=[TFlag.Enabled];

 fPhysicalPass:=nil;

end;

destructor TpvFrameGraph.TPass.Destroy;
begin

 FreeAndNil(fResources);

 FreeAndNil(fResourceTransitions);

 FreeAndNil(fPreviousPasses);

 FreeAndNil(fNextPasses);

 inherited Destroy;

end;

function TpvFrameGraph.TPass.GetEnabled:boolean;
begin
 result:=TFlag.Enabled in fFlags;
end;

procedure TpvFrameGraph.TPass.SetEnabled(const aEnabled:boolean);
begin
 if aEnabled<>(TFlag.Enabled in fFlags) then begin
  if aEnabled then begin
   Include(fFlags,TFlag.Enabled);
  end else begin
   Exclude(fFlags,TFlag.Enabled);
  end;
 end;
end;

procedure TpvFrameGraph.TPass.SetName(const aName:TpvRawByteString);
begin
 if fName<>aName then begin
  if length(fName)>0 then begin
   fFrameGraph.fPassNameHashMap.Delete(fName);
  end;
  fName:=aName;
  if length(fName)>0 then begin
   if fFrameGraph.fPassNameHashMap.ExistKey(fName) then begin
    raise EpvFrameGraphDuplicateName.Create('Duplicate name');
   end;
   fFrameGraph.fPassNameHashMap.Add(fName,self);
  end;
 end;
end;

function TpvFrameGraph.TPass.AddImageResource(const aResourceTypeName:TpvRawByteString;
                                              const aResourceName:TpvRawByteString;
                                              const aKind:TResourceTransition.TKind;
                                              const aFlags:TResourceTransition.TFlags;
                                              const aLayout:TVkImageLayout;
                                              const aLoadOp:TLoadOp):TResourceTransition;
var ResourceType:TResourceType;
    Resource:TResource;
begin
 ResourceType:=fFrameGraph.fResourceTypeNameHashMap[aResourceTypeName];
 if not assigned(ResourceType) then begin
  raise EpvFrameGraph.Create('Invalid resource type');
 end;
 Resource:=fFrameGraph.fResourceNameHashMap[aResourceName];
 if assigned(Resource) then begin
  if Resource.fResourceType<>ResourceType then begin
   raise EpvFrameGraph.Create('Resource type mismatch');
  end;
 end else begin
  Resource:=TResource.Create(fFrameGraph,aResourceName,ResourceType);
 end;
 if not (ResourceType is TImageResourceType) then begin
  raise EpvFrameGraph.Create('Resource meta type mismatch');
 end;
 result:=TResourceTransition.Create(fFrameGraph,
                                    self,
                                    Resource,
                                    aKind,
                                    aFlags,
                                    aLayout,
                                    aLoadOp);
 fFrameGraph.fValid:=false;
end;

function TpvFrameGraph.TPass.AddBufferResource(const aResourceTypeName:TpvRawByteString;
                                               const aResourceName:TpvRawByteString;
                                               const aKind:TResourceTransition.TKind;
                                               const aFlags:TResourceTransition.TFlags;
                                               const aPipelineStage:TVkPipelineStageFlags;
                                               const aAccessFlags:TVkAccessFlags;
                                               const aBufferSubresourceRange:TBufferSubresourceRange):TResourceTransition;
var ResourceType:TResourceType;
    Resource:TResource;
begin
 ResourceType:=fFrameGraph.fResourceTypeNameHashMap[aResourceTypeName];
 if not assigned(ResourceType) then begin
  raise EpvFrameGraph.Create('Invalid resource type');
 end;
 Resource:=fFrameGraph.fResourceNameHashMap[aResourceName];
 if assigned(Resource) then begin
  if Resource.fResourceType<>ResourceType then begin
   raise EpvFrameGraph.Create('Resource type mismatch');
  end;
 end else begin
  Resource:=TResource.Create(fFrameGraph,aResourceName,ResourceType);
 end;
 if not (ResourceType is TBufferResourceType) then begin
  raise EpvFrameGraph.Create('Resource meta type mismatch');
 end;
 result:=TResourceTransition.Create(fFrameGraph,
                                    self,
                                    Resource,
                                    aKind,
                                    aFlags,
                                    aPipelineStage,
                                    aAccessFlags,
                                    aBufferSubresourceRange);
 fFrameGraph.fValid:=false;
end;

procedure TpvFrameGraph.TPass.AddImageInput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aFlags:TResourceTransition.TFlags=[]);
begin
 AddImageResource(aResourceTypeName,
                  aResourceName,
                  TResourceTransition.TKind.ImageInput,
                  aFlags,
                  aLayout,
                  TLoadOp.Create(TLoadOp.TKind.Load));
end;

procedure TpvFrameGraph.TPass.AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aLayout:TVkImageLayout;
                                             const aLoadOp:TLoadOp;
                                             const aFlags:TResourceTransition.TFlags=[]);
begin
 AddImageResource(aResourceTypeName,
                  aResourceName,
                  TResourceTransition.TKind.ImageOutput,
                  aFlags,
                  aLayout,
                  aLoadOp);
end;

procedure TpvFrameGraph.TPass.AddImageResolveOutput(const aResourceTypeName:TpvRawByteString;
                                                    const aResourceName:TpvRawByteString;
                                                    const aResourceSourceName:TpvRawByteString;
                                                    const aLayout:TVkImageLayout;
                                                    const aLoadOp:TLoadOp;
                                                    const aFlags:TResourceTransition.TFlags=[]);
var ResourceSource:TResource;
begin
 ResourceSource:=fFrameGraph.fResourceNameHashMap[aResourceSourceName];
 if not assigned(ResourceSource) then begin
  raise EpvFrameGraph.Create('Invalid source resource');
 end;
 AddImageResource(aResourceTypeName,
                  aResourceName,
                  TResourceTransition.TKind.ImageResolveOutput,
                  aFlags,
                  aLayout,
                  aLoadOp).fResolveResource:=ResourceSource;
end;

procedure TpvFrameGraph.TPass.AddImageDepthInput(const aResourceTypeName:TpvRawByteString;
                                                 const aResourceName:TpvRawByteString;
                                                 const aLayout:TVkImageLayout;
                                                 const aFlags:TResourceTransition.TFlags=[]);
begin
 AddImageResource(aResourceTypeName,
                  aResourceName,
                  TResourceTransition.TKind.ImageDepthInput,
                  aFlags,
                  aLayout,
                  TLoadOp.Create(TLoadOp.TKind.Load));
end;

procedure TpvFrameGraph.TPass.AddImageDepthOutput(const aResourceTypeName:TpvRawByteString;
                                                  const aResourceName:TpvRawByteString;
                                                  const aLayout:TVkImageLayout;
                                                  const aLoadOp:TLoadOp;
                                                  const aFlags:TResourceTransition.TFlags=[]);
begin
 AddImageResource(aResourceTypeName,
                  aResourceName,
                  TResourceTransition.TKind.ImageDepthOutput,
                  aFlags,
                  aLayout,
                  aLoadOp);
end;

procedure TpvFrameGraph.TPass.AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aPipelineStage:TVkPipelineStageFlags;
                                             const aAccessFlags:TVkAccessFlags;
                                             const aBufferSubresourceRange:TBufferSubresourceRange;
                                             const aFlags:TResourceTransition.TFlags=[]);
begin
 AddBufferResource(aResourceTypeName,
                   aResourceName,
                   TResourceTransition.TKind.BufferInput,
                   aFlags,
                   aPipelineStage,
                   aAccessFlags,
                   aBufferSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aPipelineStage:TVkPipelineStageFlags;
                                             const aAccessFlags:TVkAccessFlags;
                                             const aFlags:TResourceTransition.TFlags=[]);
begin
 AddBufferInput(aResourceTypeName,
                aResourceName,
                aPipelineStage,
                aAccessFlags,
                TBufferSubresourceRange.Create(0,VK_WHOLE_SIZE),
                aFlags);
end;

procedure TpvFrameGraph.TPass.AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                              const aResourceName:TpvRawByteString;
                                              const aPipelineStage:TVkPipelineStageFlags;
                                              const aAccessFlags:TVkAccessFlags;
                                              const aBufferSubresourceRange:TBufferSubresourceRange;
                                              const aFlags:TResourceTransition.TFlags=[]);
begin
 AddBufferResource(aResourceTypeName,
                   aResourceName,
                   TResourceTransition.TKind.BufferOutput,
                   aFlags,
                   aPipelineStage,
                   aAccessFlags,
                   aBufferSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                              const aResourceName:TpvRawByteString;
                                              const aPipelineStage:TVkPipelineStageFlags;
                                              const aAccessFlags:TVkAccessFlags;
                                              const aFlags:TResourceTransition.TFlags=[]);
begin
 AddBufferOutput(aResourceTypeName,
                 aResourceName,
                 aPipelineStage,
                 aAccessFlags,
                 TBufferSubresourceRange.Create(0,VK_WHOLE_SIZE),
                 aFlags);
end;

procedure TpvFrameGraph.TPass.Setup;
begin

end;

procedure TpvFrameGraph.TPass.Execute;
begin

end;

{ TpvFrameGraph.TRenderPass }

constructor TpvFrameGraph.TRenderPass.Create(const aFrameGraph:TpvFrameGraph);
begin
 inherited Create(aFrameGraph);
 fPhysicalRenderPassSubPass:=nil;
end;

destructor TpvFrameGraph.TRenderPass.Destroy;
begin
 inherited Destroy;
end;

{ TpvFrameGraph.TPhysicalPass.TPipelineBarrierGroup }

constructor TpvFrameGraph.TPhysicalPass.TPipelineBarrierGroup.Create(const aSrcStageMask:TVkPipelineStageFlags;
                                                                     const aDstStageMask:TVkPipelineStageFlags;
                                                                     const aDependencyFlags:TVkDependencyFlags);
var Index:TpvSizeInt;
begin
 inherited Create;
 fSrcStageMask:=aSrcStageMask;
 fDstStageMask:=aDstStageMask;
 fDependencyFlags:=aDependencyFlags;
 fBarrierMapItemDynamicArray.Initialize;
 fMemoryBarrierDynamicArray.Initialize;
 fBufferMemoryBarrierDynamicArray.Initialize;
 fImageMemoryBarrierDynamicArray.Initialize;
 for Index:=0 to MaxSwapChainImages-1 do begin
  fWorkMemoryBarrierDynamicArray[Index].Initialize;
  fWorkBufferMemoryBarrierDynamicArray[Index].Initialize;
  fWorkImageMemoryBarrierDynamicArray[Index].Initialize;
 end;
end;

destructor TpvFrameGraph.TPhysicalPass.TPipelineBarrierGroup.Destroy;
var SwapChainImageIndex:TpvSizeInt;
begin
 fBarrierMapItemDynamicArray.Finalize;
 fMemoryBarrierDynamicArray.Finalize;
 fBufferMemoryBarrierDynamicArray.Finalize;
 fImageMemoryBarrierDynamicArray.Finalize;
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  fWorkMemoryBarrierDynamicArray[SwapChainImageIndex].Finalize;
  fWorkBufferMemoryBarrierDynamicArray[SwapChainImageIndex].Finalize;
  fWorkImageMemoryBarrierDynamicArray[SwapChainImageIndex].Finalize;
 end;
 inherited Destroy;
end;

procedure TpvFrameGraph.TPhysicalPass.TPipelineBarrierGroup.Show;
begin
end;

procedure TpvFrameGraph.TPhysicalPass.TPipelineBarrierGroup.Hide;
begin
end;

procedure TpvFrameGraph.TPhysicalPass.TPipelineBarrierGroup.AfterCreateSwapChain;
var SwapChainImageIndex,
    BarrierMapItemIndex:TpvSizeInt;
    BarrierMapItem:PBarrierMapItem;
    MemoryBarrier:PVkMemoryBarrier;
    BufferMemoryBarrier:PVkBufferMemoryBarrier;
    ImageMemoryBarrier:PVkImageMemoryBarrier;
begin
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  fWorkMemoryBarrierDynamicArray[SwapChainImageIndex].Assign(fMemoryBarrierDynamicArray);
  fWorkBufferMemoryBarrierDynamicArray[SwapChainImageIndex].Assign(fBufferMemoryBarrierDynamicArray);
  fWorkImageMemoryBarrierDynamicArray[SwapChainImageIndex].Assign(fImageMemoryBarrierDynamicArray);
  for BarrierMapItemIndex:=0 to fBarrierMapItemDynamicArray.Count-1 do begin
   BarrierMapItem:=@fBarrierMapItemDynamicArray.Items[BarrierMapItemIndex];
   case BarrierMapItem^.Kind of
    TBarrierMapItemKind.Memory:begin
     Assert((BarrierMapItem^.BarrierIndex>=0) and (BarrierMapItem^.BarrierIndex<fMemoryBarrierDynamicArray.Count));
     MemoryBarrier:=@fWorkMemoryBarrierDynamicArray[SwapChainImageIndex].Items[BarrierMapItem^.BarrierIndex];
     if assigned(MemoryBarrier) then begin
      // Nothing needed to do
     end;
    end;
    TBarrierMapItemKind.Buffer:begin
     Assert((BarrierMapItem^.BarrierIndex>=0) and (BarrierMapItem^.BarrierIndex<fBufferMemoryBarrierDynamicArray.Count));
     BufferMemoryBarrier:=@fWorkBufferMemoryBarrierDynamicArray[SwapChainImageIndex].Items[BarrierMapItem^.BarrierIndex];
     Assert(assigned(BarrierMapItem^.ResourcePhysicalData));
     if BarrierMapItem^.ResourcePhysicalData is TResourcePhysicalBufferData then begin
      // TODO
//    BufferMemoryBarrier^.buffer:=TResourcePhysicalBufferData(BarrierMapItem^.ResourcePhysicalData).fVulkanBuffers[SwapChainImageIndex].Handle;
      Assert(false,'TODO');
     end else begin
      Assert(false);
     end;
    end;
    TBarrierMapItemKind.Image:begin
     Assert((BarrierMapItem^.BarrierIndex>=0) and (BarrierMapItem^.BarrierIndex<fImageMemoryBarrierDynamicArray.Count));
     ImageMemoryBarrier:=@fWorkImageMemoryBarrierDynamicArray[SwapChainImageIndex].Items[BarrierMapItem^.BarrierIndex];
     Assert(assigned(BarrierMapItem^.ResourcePhysicalData));
     if BarrierMapItem^.ResourcePhysicalData is TResourcePhysicalImageData then begin
      ImageMemoryBarrier^.image:=TResourcePhysicalImageData(BarrierMapItem^.ResourcePhysicalData).fVulkanImages[SwapChainImageIndex].Handle;
     end else begin
      Assert(false);
     end;
    end;
    else begin
     Assert(false);
    end;
   end;
  end;
 end;
end;

procedure TpvFrameGraph.TPhysicalPass.TPipelineBarrierGroup.BeforeDestroySwapChain;
var SwapChainImageIndex:TpvSizeInt;
begin
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  fWorkMemoryBarrierDynamicArray[SwapChainImageIndex].Clear;
  fWorkBufferMemoryBarrierDynamicArray[SwapChainImageIndex].Clear;
  fWorkImageMemoryBarrierDynamicArray[SwapChainImageIndex].Clear;
 end;
end;

{ TpvFrameGraph.TPhysicalPass }

constructor TpvFrameGraph.TPhysicalPass.Create(const aFrameGraph:TpvFrameGraph;const aQueue:TQueue);
var SwapChainImageIndex:TpvSizeInt;
begin

 inherited Create;

 fFrameGraph:=aFrameGraph;

 fQueue:=aQueue;

 fInputDependencies:=TPhysicalPasses.Create;
 fInputDependencies.OwnsObjects:=false;

 fOutputDependencies:=TPhysicalPasses.Create;
 fOutputDependencies.OwnsObjects:=false;

 fBeforePipelineBarrierGroups:=TPipelineBarrierGroups.Create;
 fBeforePipelineBarrierGroups.OwnsObjects:=false;

 fAfterPipelineBarrierGroups:=TPipelineBarrierGroups.Create;
 fAfterPipelineBarrierGroups.OwnsObjects:=false;

 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  fCommandBuffers[SwapChainImageIndex]:=nil;
  fSignallingSemaphores[SwapChainImageIndex]:=nil;
 end;

 fWaitingSemaphores.Initialize;

 fWaitingSemaphoreHandles.Initialize;

 fWaitingSemaphoreDstStageMasks.Initialize;

end;

destructor TpvFrameGraph.TPhysicalPass.Destroy;
var SwapChainImageIndex:TpvSizeInt;
begin
 fWaitingSemaphores.Finalize;
 fWaitingSemaphoreHandles.Finalize;
 fWaitingSemaphoreDstStageMasks.Finalize;
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fCommandBuffers[SwapChainImageIndex]);
  FreeAndNil(fSignallingSemaphores[SwapChainImageIndex]);
 end;
 FreeAndNil(fBeforePipelineBarrierGroups);
 FreeAndNil(fAfterPipelineBarrierGroups);
 FreeAndNil(fInputDependencies);
 FreeAndNil(fOutputDependencies);
 inherited Destroy;
end;

procedure TpvFrameGraph.TPhysicalPass.Show;
var PipelineBarrierGroup:TPipelineBarrierGroup;
begin
 for PipelineBarrierGroup in fBeforePipelineBarrierGroups do begin
  PipelineBarrierGroup.Show;
 end;
 for PipelineBarrierGroup in fAfterPipelineBarrierGroups do begin
  PipelineBarrierGroup.Show;
 end;
end;

procedure TpvFrameGraph.TPhysicalPass.Hide;
var PipelineBarrierGroup:TPipelineBarrierGroup;
begin
 for PipelineBarrierGroup in fBeforePipelineBarrierGroups do begin
  PipelineBarrierGroup.Hide;
 end;
 for PipelineBarrierGroup in fAfterPipelineBarrierGroups do begin
  PipelineBarrierGroup.Hide;
 end;
end;

procedure TpvFrameGraph.TPhysicalPass.AfterCreateSwapChain;
var SwapChainImageIndex:TpvSizeInt;
    PipelineBarrierGroup:TPipelineBarrierGroup;
begin
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  fCommandBuffers[SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fQueue.fCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fSignallingSemaphores[SwapChainImageIndex]:=TpvVulkanSemaphore.Create(fFrameGraph.fVulkanDevice);
 end;
 for PipelineBarrierGroup in fBeforePipelineBarrierGroups do begin
  PipelineBarrierGroup.AfterCreateSwapChain;
 end;
 for PipelineBarrierGroup in fAfterPipelineBarrierGroups do begin
  PipelineBarrierGroup.AfterCreateSwapChain;
 end;
end;

procedure TpvFrameGraph.TPhysicalPass.BeforeDestroySwapChain;
var SwapChainImageIndex:TpvSizeInt;
    PipelineBarrierGroup:TPipelineBarrierGroup;
begin
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fCommandBuffers[SwapChainImageIndex]);
  FreeAndNil(fSignallingSemaphores[SwapChainImageIndex]);
 end;
 for PipelineBarrierGroup in fBeforePipelineBarrierGroups do begin
  PipelineBarrierGroup.BeforeDestroySwapChain;
 end;
 for PipelineBarrierGroup in fAfterPipelineBarrierGroups do begin
  PipelineBarrierGroup.BeforeDestroySwapChain;
 end;
end;

{ TpvFrameGraph.TVulkanComputePass }

constructor TpvFrameGraph.TPhysicalComputePass.Create(const aFrameGraph:TpvFrameGraph;
                                                      const aComputePass:TComputePass);
begin
 inherited Create(aFrameGraph,aComputePass.fQueue);
 fComputePass:=aComputePass;
end;

destructor TpvFrameGraph.TPhysicalComputePass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvFrameGraph.TPhysicalComputePass.Show;
begin
 inherited Show;
end;

procedure TpvFrameGraph.TPhysicalComputePass.Hide;
begin
 inherited Hide;
end;

procedure TpvFrameGraph.TPhysicalComputePass.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TpvFrameGraph.TPhysicalComputePass.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

{ TpvFrameGraph.TVulkanRenderPass.TSubPass }

constructor TpvFrameGraph.TPhysicalRenderPass.TSubPass.Create(const aPhysicalRenderPass:TPhysicalRenderPass;
                                                              const aRenderPass:TRenderPass);
begin
 inherited Create;
 fPhysicalRenderPass:=aPhysicalRenderPass;
 fRenderPass:=aRenderPass;
end;

destructor TpvFrameGraph.TPhysicalRenderPass.TSubPass.Destroy;
begin
 inherited Destroy;
end;

procedure TpvFrameGraph.TPhysicalRenderPass.TSubPass.Show;
begin
end;

procedure TpvFrameGraph.TPhysicalRenderPass.TSubPass.Hide;
begin
end;

procedure TpvFrameGraph.TPhysicalRenderPass.TSubPass.AfterCreateSwapChain;
begin
end;

procedure TpvFrameGraph.TPhysicalRenderPass.TSubPass.BeforeDestroySwapChain;
begin
end;

{ TpvFrameGraph.TVulkanRenderPass }

constructor TpvFrameGraph.TPhysicalRenderPass.Create(const aFrameGraph:TpvFrameGraph;const aQueue:TQueue);
begin
 inherited Create(aFrameGraph,aQueue);
 fSubPasses:=TSubPasses.Create;
 fSubPasses.OwnsObjects:=true;
 fSubPassDependencies.Initialize;
 fVulkanRenderPass:=nil;
end;

destructor TpvFrameGraph.TPhysicalRenderPass.Destroy;
begin
 fSubPassDependencies.Finalize;
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fSubPasses);
 inherited Destroy;
end;

procedure TpvFrameGraph.TPhysicalRenderPass.Show;
var SubPass:TSubPass;
begin
 inherited Show;
 for SubPass in fSubPasses do begin
  SubPass.Show;
 end;
end;

procedure TpvFrameGraph.TPhysicalRenderPass.Hide;
var SubPass:TSubPass;
begin
 for SubPass in fSubPasses do begin
  SubPass.Hide;
 end;
 inherited Hide;
end;

procedure TpvFrameGraph.TPhysicalRenderPass.AfterCreateSwapChain;
type TAttachment=record
      Resource:TResource;
      ImageResourceType:TImageResourceType;
      Index:TVkInt32;
      Format:TVkFormat;
      LoadOp:TVkAttachmentLoadOp;
      StoreOp:TVkAttachmentStoreOp;
      StencilLoadOp:TVkAttachmentLoadOp;
      StencilStoreOp:TVkAttachmentStoreOp;
      InitialLayout:TVkImageLayout;
      FinalLayout:TVkImageLayout;
      ImageUsageFlags:TVkImageUsageFlags;
      ClearValueInitialized:boolean;
      ClearValue:TVkClearValue;
     end;
     PAttachment=^TAttachment;
     TAttachments=TpvDynamicArray<TAttachment>;
     TInt32AttachmentLists=TpvDynamicArray<TpvInt32>;
     TUInt32AttachmentLists=TpvDynamicArray<TpvUInt32>;
var AttachmentIndex,
    OtherAttachmentIndex,
    SubPassDependencyIndex,
    SubPassIndex:TpvSizeInt;
    SubPass,
    OtherSubPass:TSubPass;
    RenderPass:TRenderPass;
    Resource:TResource;
    ResourceType:TResourceType;
    ImageResourceType:TImageResourceType;
    ResourceTransition,
    OtherResourceTransition,
    FromResourceTransition,
    ToResourceTransition:TResourceTransition;
    Attachments:TAttachments;
    Attachment:PAttachment;
    InputAttachments,
    ColorAttachments,
    ResolveAttachments:TInt32AttachmentLists;
    PreserveAttachments:TUInt32AttachmentLists;
    DepthStencilAttachment:TpvInt64;
    SrcSubPassIndex,
    DstSubPassIndex:TVkUInt32;
    SrcStageMask,
    DstStageMask:TVkPipelineStageFlags;
    SubPassDependency:PSubPassDependency;
    Format:TVkFormat;
    UsedNow,
    UsedBefore,
    UsedAfter,
    IsSurfaceOrPersistent,
    HasResolveOutputs,
    Found:boolean;
    AttachmentDescriptionFlags:TVkAttachmentDescriptionFlags;
begin

 inherited AfterCreateSwapChain;

 for SubPass in fSubPasses do begin
  SubPass.AfterCreateSwapChain;
 end;

{fVulkanRenderPass:=TpvVulkanRenderPass.Create(fFrameGraph.fVulkanDevice);
 fVulkanRenderPass.}

 fVulkanRenderPass:=nil;

 if assigned(fVulkanRenderPass) then begin

  Attachments.Initialize;
  try

   for SubPass in fSubPasses do begin
    RenderPass:=SubPass.fRenderPass;
    for ResourceTransition in RenderPass.fResourceTransitions do begin
     ResourceType:=ResourceTransition.fResource.fResourceType;
     if ResourceTransition.Kind in TResourceTransition.AllImages then begin
      Assert(ResourceType is TImageResourceType);
      Found:=false;
      for AttachmentIndex:=0 to Attachments.Count-1 do begin
       Attachment:=@Attachments.Items[AttachmentIndex];
       if Attachment^.Resource=ResourceTransition.fResource then begin
        Found:=true;
        break;
       end;
      end;
      if not Found then begin
       ImageResourceType:=TImageResourceType(ResourceType);
       AttachmentIndex:=Attachments.AddNew;
       Attachment:=@Attachments.Items[AttachmentIndex];
       Attachment^.Resource:=ResourceTransition.fResource;
       Attachment^.ImageResourceType:=ImageResourceType;
       Attachment^.Index:=AttachmentIndex;
       Attachment^.Format:=ImageResourceType.fFormat;
       if Attachment^.Format=VK_FORMAT_UNDEFINED then begin
        case ImageResourceType.fImageType of
         TImageType.Color:begin
          Attachment^.Format:=fFrameGraph.fSurfaceColorFormat;
         end;
         TImageType.Depth,
         TImageType.DepthStencil,
         TImageType.Stencil:begin
          Attachment^.Format:=fFrameGraph.fSurfaceDepthFormat;
         end;
        end;
       end;
       Attachment^.LoadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
       Attachment^.StoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
       Attachment^.StencilLoadOp:=VK_ATTACHMENT_LOAD_OP_DONT_CARE;
       Attachment^.StencilStoreOp:=VK_ATTACHMENT_STORE_OP_DONT_CARE;
       Attachment^.InitialLayout:=ResourceTransition.fLayout;
       Attachment^.FinalLayout:=ResourceTransition.fLayout;
       Attachment^.ImageUsageFlags:=0;
       Attachment^.ClearValueInitialized:=false;
      end;
     end;
    end;
   end;

   Attachments.Finish;

   for SubPass in fSubPasses do begin
    RenderPass:=SubPass.fRenderPass;
    for ResourceTransition in RenderPass.fResourceTransitions do begin
     for AttachmentIndex:=0 to Attachments.Count-1 do begin
      Attachment:=@Attachments.Items[AttachmentIndex];
      if Attachment^.Resource=ResourceTransition.fResource then begin
       ImageResourceType:=Attachment^.ImageResourceType;
       if (Attachment^.LoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE) and (ImageResourceType.fImageType in [TImageType.Surface,TImageType.Color,TImageType.Depth]) then begin
        Attachment^.LoadOp:=TLoadOp.Values[ResourceTransition.fLoadOp.Kind];
       end;
       if (Attachment^.StencilLoadOp=VK_ATTACHMENT_LOAD_OP_DONT_CARE) and (ImageResourceType.fImageType in [TImageType.DepthStencil,TImageType.Stencil]) then begin
        Attachment^.StencilLoadOp:=TLoadOp.Values[ResourceTransition.fLoadOp.Kind];
       end;
       case ResourceTransition.fLayout of
        VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
        end;
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT);
        end;
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT);
        end;
        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT);
        end;
        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_SRC_BIT);
        end;
        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT);
        end;
        VK_IMAGE_LAYOUT_PRESENT_SRC_KHR:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT);
        end;
        VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_TRANSFER_DST_BIT);
        end;
        VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT);
        end;
        VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT);
        end;
        VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV:begin
         Attachment^.ImageUsageFlags:=Attachment^.ImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT);
        end;
       end;
       Attachment^.FinalLayout:=ResourceTransition.fLayout;
       if not Attachment^.ClearValueInitialized then begin
        Attachment^.ClearValueInitialized:=true;
        if ImageResourceType.fImageType in [TImageType.DepthStencil,TImageType.Stencil,TImageType.Depth] then begin
         Attachment^.ClearValue.depthStencil.depth:=ResourceTransition.fLoadOp.ClearColor[0];
         Attachment^.ClearValue.depthStencil.stencil:=trunc(ResourceTransition.fLoadOp.ClearColor[1]);
        end else begin
         Attachment^.ClearValue.color.float32[0]:=ResourceTransition.fLoadOp.ClearColor[0];
         Attachment^.ClearValue.color.float32[1]:=ResourceTransition.fLoadOp.ClearColor[1];
         Attachment^.ClearValue.color.float32[2]:=ResourceTransition.fLoadOp.ClearColor[2];
         Attachment^.ClearValue.color.float32[3]:=ResourceTransition.fLoadOp.ClearColor[3];
        end;
       end;
       break;
      end;
     end;
    end;
   end;

   InputAttachments.Initialize;
   ColorAttachments.Initialize;
   ResolveAttachments.Initialize;
   PreserveAttachments.Initialize;
   try

    for SubPassIndex:=0 to fSubPasses.Count-1 do begin

     SubPass:=fSubPasses[SubPassIndex];

     RenderPass:=SubPass.fRenderPass;

     InputAttachments.Clear;
     ColorAttachments.Clear;
     ResolveAttachments.Clear;
     PreserveAttachments.Clear;
     DepthStencilAttachment:=-1;

     HasResolveOutputs:=false;
     for ResourceTransition in RenderPass.fResourceTransitions do begin
      if ResourceTransition.fKind=TResourceTransition.TKind.ImageResolveOutput then begin
       HasResolveOutputs:=true;
       break;
      end;
     end;

     for ResourceTransition in RenderPass.fResourceTransitions do begin
      case ResourceTransition.fKind of
       TResourceTransition.TKind.ImageInput:begin
        for AttachmentIndex:=0 to Attachments.Count-1 do begin
         if Attachments.Items[AttachmentIndex].Resource=ResourceTransition.fResource then begin
          InputAttachments.Add(fVulkanRenderPass.AddAttachmentReference(AttachmentIndex,
                                                                        ResourceTransition.fLayout));
          break;
         end;
        end;
       end;
       TResourceTransition.TKind.ImageOutput:begin
        for AttachmentIndex:=0 to Attachments.Count-1 do begin
         if Attachments.Items[AttachmentIndex].Resource=ResourceTransition.fResource then begin
          ColorAttachments.Add(fVulkanRenderPass.AddAttachmentReference(AttachmentIndex,
                                                                        ResourceTransition.fLayout));
          for OtherResourceTransition in RenderPass.fResourceTransitions do begin
           if (ResourceTransition<>OtherResourceTransition) and
              (OtherResourceTransition.ResolveResource=ResourceTransition.Resource) then begin
            Found:=false;
            for OtherAttachmentIndex:=0 to Attachments.Count-1 do begin
             if Attachments.Items[OtherAttachmentIndex].Resource=ResourceTransition.fResource then begin
              ResolveAttachments.Add(fVulkanRenderPass.AddAttachmentReference(OtherAttachmentIndex,
                                                                              OtherResourceTransition.fLayout));
              Found:=true;
              break;
             end;
            end;
            if not Found then begin
             ResolveAttachments.Add(fVulkanRenderPass.AddAttachmentReference(VK_ATTACHMENT_UNUSED,
                                                                             VK_IMAGE_LAYOUT_UNDEFINED));
            end;
            break;
           end;
          end;
          break;
         end;
        end;
       end;
       TResourceTransition.TKind.ImageDepthInput,TResourceTransition.TKind.ImageDepthOutput:begin
        for AttachmentIndex:=0 to Attachments.Count-1 do begin
         if Attachments.Items[AttachmentIndex].Resource=ResourceTransition.fResource then begin
          DepthStencilAttachment:=AttachmentIndex;
          break;
         end;
        end;
       end;
      end;
     end;

     for AttachmentIndex:=0 to Attachments.Count-1 do begin
      Attachment:=@Attachments.Items[AttachmentIndex];
      Resource:=Attachment^.Resource;
      UsedNow:=false;
      for ResourceTransition in SubPass.fRenderPass.fResourceTransitions do begin
       if ResourceTransition.Resource=Resource then begin
        UsedNow:=true;
        break;
       end;
      end;
      UsedBefore:=Resource.fMinimumPhysicalPassStepIndex<SubPass.fPhysicalRenderPass.fIndex;
      UsedAfter:=SubPass.fPhysicalRenderPass.fIndex<Resource.fMaximumPhysicalPassStepIndex;
      IsSurfaceOrPersistent:=(Attachment^.ImageResourceType.fImageType=TImageType.Surface) or Attachment^.ImageResourceType.fPersientent;
      if UsedBefore and (not UsedNow) and (UsedAfter or IsSurfaceOrPersistent) then begin
       PreserveAttachments.Add(fVulkanRenderPass.AddAttachmentReference(AttachmentIndex,
                                                                        Attachments.Items[AttachmentIndex].FinalLayout));
      end;
      if (SubPassIndex>0) and (UsedAfter or isSurfaceOrPersistent) then begin
       case Attachment^.ImageResourceType.fImageType of
        TImageType.Surface,TImageType.Color,TImageType.Depth:begin
         Attachment^.StoreOp:=VK_ATTACHMENT_STORE_OP_STORE;
        end;
        TImageType.DepthStencil:begin
         Attachment^.StoreOp:=VK_ATTACHMENT_STORE_OP_STORE;
         Attachment^.StencilStoreOp:=VK_ATTACHMENT_STORE_OP_STORE;
        end;
        TImageType.Stencil:begin
         Attachment^.StencilStoreOp:=VK_ATTACHMENT_STORE_OP_STORE;
        end;
       end;
      end;
     end;

     InputAttachments.Finish;
     ColorAttachments.Finish;
     ResolveAttachments.Finish;
     PreserveAttachments.Finish;

     if DepthStencilAttachment<0 then begin
      DepthStencilAttachment:=VK_ATTACHMENT_UNUSED;
     end;

     fVulkanRenderPass.AddSubpassDescription(0,
                                             VK_PIPELINE_BIND_POINT_GRAPHICS,
                                             InputAttachments.Items,
                                             ColorAttachments.Items,
                                             ResolveAttachments.Items,
                                             DepthStencilAttachment,
                                             PreserveAttachments.Items
                                            );

    end;
   finally
    InputAttachments.Finalize;
    ColorAttachments.Finalize;
    ResolveAttachments.Finalize;
    PreserveAttachments.Finalize;
   end;

   for SubPassDependencyIndex:=0 to fSubPassDependencies.Count-1 do begin
    SubPassDependency:=@fSubPassDependencies.Items[SubPassDependencyIndex];
    if assigned(SubPassDependency^.SrcSubPass) then begin
     SrcSubPassIndex:=SubPassDependency^.SrcSubPass.fIndex;
    end else begin
     SrcSubPassIndex:=VK_SUBPASS_EXTERNAL;
    end;
    if assigned(SubPassDependency.DstSubPass) then begin
     DstSubPassIndex:=SubPassDependency^.DstSubPass.fIndex;
    end else begin
     DstSubPassIndex:=VK_SUBPASS_EXTERNAL;
    end;
    fVulkanRenderPass.AddSubpassDependency(SrcSubPassIndex,
                                           DstSubPassIndex,
                                           SubPassDependency^.SrcStageMask,
                                           SubPassDependency^.DstStageMask,
                                           SubPassDependency^.SrcAccessMask,
                                           SubPassDependency^.DstAccessMask,
                                           SubPassDependency^.DependencyFlags);
   end;

   for AttachmentIndex:=0 to Attachments.Count-1 do begin
    Attachment:=@Attachments.Items[AttachmentIndex];
    AttachmentDescriptionFlags:=0;
    if Attachment^.Resource.fResourceReuseGroup.fResources.Count>1 then begin
     AttachmentDescriptionFlags:=AttachmentDescriptionFlags or TVkAttachmentDescriptionFlags(VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT);
    end;
    fVulkanRenderPass.AddAttachmentDescription(AttachmentDescriptionFlags,
                                               Attachment^.Format,
                                               Attachment^.ImageResourceType.fSamples,
                                               Attachment^.LoadOp,
                                               Attachment^.StoreOp,
                                               Attachment^.StencilLoadOp,
                                               Attachment^.StencilStoreOp,
                                               Attachment^.InitialLayout,
                                               Attachment^.FinalLayout
                                              );
   end;

   fVulkanRenderPass.Initialize;

  finally
   Attachments.Finalize;
  end;

 end;

end;

procedure TpvFrameGraph.TPhysicalRenderPass.BeforeDestroySwapChain;
var SubPass:TSubPass;
begin

 FreeAndNil(fVulkanRenderPass);

 for SubPass in fSubPasses do begin
  SubPass.BeforeDestroySwapChain;
 end;

 inherited BeforeDestroySwapChain;

end;

{ TpvFrameGraph }

constructor TpvFrameGraph.Create(const aVulkanDevice:TpvVulkanDevice);
begin

 inherited Create;

 fVulkanDevice:=aVulkanDevice;

 fSurfaceWidth:=1;
 fSurfaceHeight:=1;

 fSurfaceColorFormat:=VK_FORMAT_B8G8R8A8_UNORM;

 fSurfaceDepthFormat:=VK_FORMAT_D32_SFLOAT;

 fCountSwapChainImages:=MaxSwapChainImages;

 fCanDoParallelProcessing:=false;

 fQueues:=TQueues.Create;
 fQueues.OwnsObjects:=true;

 fQueueFamilyIndices.Initialize;

 fResourceTypes:=TResourceTypeList.Create;
 fResourceTypes.OwnsObjects:=true;

 fResourceTypeNameHashMap:=TResourceTypeNameHashMap.Create(nil);

 fResources:=TResourceList.Create;
 fResources.OwnsObjects:=true;

 fResourceNameHashMap:=TResourceNameHashMap.Create(nil);

 fResourceTransitions:=TResourceTransitionList.Create;
 fResourceTransitions.OwnsObjects:=true;

 fResourceReuseGroups:=TResourceReuseGroupList.Create;
 fResourceReuseGroups.OwnsObjects:=true;

 fPasses:=TPassList.Create;
 fPasses.OwnsObjects:=true;

 fPassNameHashMap:=TPassNameHashMap.Create(nil);

 fTopologicalSortedPasses:=TPassList.Create;
 fTopologicalSortedPasses.OwnsObjects:=false;

 fPhysicalPasses:=TPhysicalPasses.Create;
 fPhysicalPasses.OwnsObjects:=true;

 fUniversalQueue:=AddQueue(fVulkanDevice.UniversalQueue);

 fGraphicsQueue:=AddQueue(fVulkanDevice.GraphicsQueue);

 fComputeQueue:=AddQueue(fVulkanDevice.ComputeQueue);

 fTransferQueue:=AddQueue(fVulkanDevice.TransferQueue);

 fPresentQueue:=AddQueue(fVulkanDevice.PresentQueue);

 fVulkanUniversalQueueCommandBuffer:=TpvVulkanCommandBuffer.Create(fUniversalQueue.fCommandPool,
                                                                   VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanUniversalQueueCommandBufferFence:=TpvVulkanFence.Create(fVulkanDevice);

end;

destructor TpvFrameGraph.Destroy;
begin

 FreeAndNil(fPhysicalPasses);

 FreeAndNil(fResourceTypes);

 FreeAndNil(fResourceTypeNameHashMap);

 FreeAndNil(fResources);

 FreeAndNil(fResourceNameHashMap);

 FreeAndNil(fResourceTransitions);

 FreeAndNil(fResourceReuseGroups);

 FreeAndNil(fTopologicalSortedPasses);

 FreeAndNil(fPasses);

 FreeAndNil(fPassNameHashMap);

 FreeAndNil(fVulkanUniversalQueueCommandBufferFence);

 FreeAndNil(fVulkanUniversalQueueCommandBuffer);

 FreeAndNil(fQueues);

 fQueueFamilyIndices.Finalize;

 inherited Destroy;

end;

procedure TpvFrameGraph.SetSwapChain(const aSwapChain:TpvVulkanSwapChain;
                                     const aSurfaceDepthFormat:TVkFormat);
var SwapChainImageIndex:TpvSizeInt;
begin
 fSurfaceWidth:=aSwapChain.Width;
 fSurfaceHeight:=aSwapChain.Height;
 fCountSwapChainImages:=aSwapChain.CountImages;
 fSurfaceColorFormat:=aSwapChain.ImageFormat;
 fSurfaceDepthFormat:=aSurfaceDepthFormat;
 for SwapChainImageIndex:=0 to fCountSwapChainImages-1 do begin
  fSurfaceImages[SwapChainImageIndex]:=aSwapChain.Images[SwapChainImageIndex];
 end;
end;

function TpvFrameGraph.AddQueue(const aPhysicalQueue:TpvVulkanQueue):TQueue;
var Index:TpvSizeInt;
    Found:boolean;
    Queue:TQueue;
begin
 result:=nil;
 for Queue in fQueues do begin
  if Queue.fPhysicalQueue=aPhysicalQueue then begin
   result:=Queue;
   break;
  end;
 end;
 if not assigned(result) then begin
  Found:=false;
  for Index:=0 to fQueueFamilyIndices.Count-1 do begin
   if fQueueFamilyIndices.Items[Index]=aPhysicalQueue.QueueFamilyIndex then begin
    Found:=true;
    break;
   end;
  end;
  if not Found then begin
   fQueueFamilyIndices.Add(aPhysicalQueue.QueueFamilyIndex);
  end;
  result:=TQueue.Create(self,aPhysicalQueue);
  fQueues.Add(result);
 end;
end;

function TpvFrameGraph.AddImageResourceType(const aName:TpvRawByteString;
                                            const aPersientent:boolean;
                                            const aFormat:TVkFormat;
                                            const aSamples:TVkSampleCountFlagBits;
                                            const aImageType:TImageType;
                                            const aImageSize:TImageSize;
                                            const aImageUsage:TVkImageUsageFlags;
                                            const aCountMipMapLevels:TVkUInt32;
                                            const aComponents:TVkComponentMapping):TResourceType;
begin
 result:=TImageResourceType.Create(self,aName,aPersientent,aFormat,aSamples,aImageType,aImageSize,aImageUsage,aCountMipMapLevels,aComponents);
end;

function TpvFrameGraph.AddImageResourceType(const aName:TpvRawByteString;
                                            const aPersientent:boolean;
                                            const aFormat:TVkFormat;
                                            const aSamples:TVkSampleCountFlagBits;
                                            const aImageType:TImageType;
                                            const aImageSize:TImageSize;
                                            const aImageUsage:TVkImageUsageFlags;
                                            const aCountMipMapLevels:TVkUInt32):TResourceType;
begin
 result:=TImageResourceType.Create(self,aName,aPersientent,aFormat,aSamples,aImageType,aImageSize,aImageUsage,aCountMipMapLevels);
end;

procedure TpvFrameGraph.Setup;
begin

end;

procedure TpvFrameGraph.Compile;
type TBeforeAfter=(Before,After);
 function GetPipelineStageMask(const aResourceTransition:TResourceTransition):TVkPipelineStageFlags;
 begin
  case aResourceTransition.fKind of
   TpvFrameGraph.TResourceTransition.TKind.ImageInput,
   TpvFrameGraph.TResourceTransition.TKind.ImageOutput,
   TpvFrameGraph.TResourceTransition.TKind.ImageResolveOutput,
   TpvFrameGraph.TResourceTransition.TKind.ImageDepthInput,
   TpvFrameGraph.TResourceTransition.TKind.ImageDepthOutput:begin
    case aResourceTransition.fLayout of
     VK_IMAGE_LAYOUT_GENERAL,
     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
      result:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT) or
              TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or
              TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
              TVkPipelineStageFlags(VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT) or
              TVkPipelineStageFlags(VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT) or
              TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
     end;
     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      result:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,
     VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
     VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL:begin
      result:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
              TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT);
     end;
     else {VK_IMAGE_LAYOUT_UNDEFINED:}begin
      result:=0;
     end;
    end;
   end;
   TpvFrameGraph.TResourceTransition.TKind.BufferInput,
   TpvFrameGraph.TResourceTransition.TKind.BufferOutput:begin
    result:=aResourceTransition.PipelineStage;
   end;
   else begin
    result:=0;
   end;
  end;
 end;
 procedure GetPipelineStageMasks(const aFromResourceTransition:TResourceTransition;
                                 const aToResourceTransition:TResourceTransition;
                                 out aSrcStageMask:TVkPipelineStageFlags;
                                 out aDstStageMask:TVkPipelineStageFlags);
 begin
  case aFromResourceTransition.fKind of
   TpvFrameGraph.TResourceTransition.TKind.ImageOutput,
   TpvFrameGraph.TResourceTransition.TKind.ImageResolveOutput,
   TpvFrameGraph.TResourceTransition.TKind.ImageDepthOutput,
   TpvFrameGraph.TResourceTransition.TKind.BufferOutput:begin
    aSrcStageMask:=GetPipelineStageMask(aFromResourceTransition);
   end;
   else begin
    aSrcStageMask:=0;
   end;
  end;
  case aToResourceTransition.fKind of
   TpvFrameGraph.TResourceTransition.TKind.ImageInput,
   TpvFrameGraph.TResourceTransition.TKind.ImageDepthInput,
   TpvFrameGraph.TResourceTransition.TKind.BufferInput:begin
    aDstStageMask:=GetPipelineStageMask(aToResourceTransition);
   end;
   else begin
    aDstStageMask:=0;
   end;
  end;
 end;
 procedure GetAccessMasks(const aFromResourceTransition:TResourceTransition;
                          const aToResourceTransition:TResourceTransition;
                          out aSrcAccessMask:TVkAccessFlags;
                          out aDstAccessMask:TVkAccessFlags);
 begin
  case aFromResourceTransition.fKind of
   TpvFrameGraph.TResourceTransition.TKind.ImageOutput,
   TpvFrameGraph.TResourceTransition.TKind.ImageResolveOutput,
   TpvFrameGraph.TResourceTransition.TKind.ImageDepthOutput:begin
    case aFromResourceTransition.fLayout of
     VK_IMAGE_LAYOUT_GENERAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or
                      TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or
                      TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
     VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or
                      TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
     end;
     else {VK_IMAGE_LAYOUT_UNDEFINED:}begin
      aSrcAccessMask:=0;
     end;
    end;
   end;
   TpvFrameGraph.TResourceTransition.TKind.BufferOutput:begin
    aSrcAccessMask:=aFromResourceTransition.fAccessFlags;
   end;
   else begin
    aSrcAccessMask:=0;
   end;
  end;
  case aToResourceTransition.fKind of
   TpvFrameGraph.TResourceTransition.TKind.ImageInput,
   TpvFrameGraph.TResourceTransition.TKind.ImageDepthInput:begin
    case aToResourceTransition.fLayout of
     VK_IMAGE_LAYOUT_GENERAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or
                      TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or
                      TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
     VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or
                      TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
     end;
     else {VK_IMAGE_LAYOUT_UNDEFINED:}begin
      aDstAccessMask:=0;
     end;
    end;
   end;
   TpvFrameGraph.TResourceTransition.TKind.BufferInput:begin
    aDstAccessMask:=aFromResourceTransition.fAccessFlags;
   end;
   else begin
    aDstAccessMask:=0;
   end;
  end;
 end;
 procedure IndexingPasses;
 var Index:TpvSizeInt;
 begin
  // Indexing passes
  for Index:=0 to fPasses.Count-1 do begin
   fPasses[Index].fIndex:=Index;
  end;
 end;
 procedure ValidateAttachmentImages;
 var Pass:TPass;
     RenderPass:TRenderPass;
     ResourceTransition:TResourceTransition;
 begin
  // Validate that all input attachment images, output attachment images have the same size as defined
  // in the render pass and that all passes have a assigned queue
  for Pass in fPasses do begin
   if not assigned(Pass.fQueue) then begin
    raise EpvFrameGraphMissingQueue.Create('Pass "'+String(Pass.fName)+'" is without assigned queue');
   end;
   if Pass is TRenderPass then begin
    RenderPass:=Pass as TRenderPass;
    for ResourceTransition in RenderPass.fResourceTransitions do begin
     if (ResourceTransition.fKind in TResourceTransition.AllImages) and
        (ResourceTransition.fResource.fResourceType is TImageResourceType) and
        (TImageResourceType(ResourceTransition.fResource.fResourceType).fImageSize<>RenderPass.fSize) then begin
      raise EpvFrameGraphMismatchImageSize.Create('Mismatch attachment image size between pass "'+String(Pass.fName)+'" and resource "'+String(ResourceTransition.fResource.fName)+'"');
     end;
    end;
   end;
  end;
 end;
 procedure ValidateResources;
 type TResourceDynamicArray=TpvDynamicArray<TResource>;
 var Pass:TPass;
     Resource:TResource;
     ResourceTransition:TResourceTransition;
     ResourceDynamicArray:TResourceDynamicArray;
     OK:boolean;
 begin

  // Validate that all resources have at least one pass, which outputs this one resource
  for Resource in fResources do begin
   OK:=false;
   for ResourceTransition in Resource.fResourceTransitions do begin
    if ResourceTransition.fKind in TResourceTransition.AllOutputs then begin
     OK:=true;
     break;
    end;
   end;
   if not OK then begin
    raise EpvFrameGraphMissedGeneratorPassForResource.Create('Missed generator pass for resource "'+String(Resource.fName)+'"');
   end;
  end;

  // Validate that all resources do not have input and output transitions at a same pass at the same time
  ResourceDynamicArray.Initialize;
  try
   for Pass in fPasses do begin
    ResourceDynamicArray.Clear;
    for ResourceTransition in Pass.fResourceTransitions do begin
     if ResourceTransition.fKind in TResourceTransition.AllInputs then begin
      ResourceDynamicArray.Add(ResourceTransition.fResource);
     end;
    end;
    for ResourceTransition in Pass.fResourceTransitions do begin
     if ResourceTransition.fKind in TResourceTransition.AllOutputs then begin
      for Resource in ResourceDynamicArray.Items do begin
       if Resource=ResourceTransition.fResource then begin
        raise EpvFrameGraphResourceUsedAsInputAndOutputInTheSamePassAtTheSameTime.Create('Resource "'+String(Resource.Name)+'" is used as input and output in pass "'+String(Pass.fName)+'" at the same time');
       end;
      end;
     end;
    end;
   end;
  finally
   ResourceDynamicArray.Finalize;
  end;

 end;
 procedure FindRootPass;
 var Pass:TPass;
     RenderPass:TRenderPass;
     Resource:TResource;
     ResourceTransition:TResourceTransition;
     Temporary:TpvSizeUInt;
 begin
  // Find root pass (a render pass, which have only a single attachment image output to a surface/swapchain)
  fRootPass:=fEnforcedRootPass;
  if not assigned(fRootPass) then begin
   for Pass in fPasses do begin
    if Pass is TRenderPass then begin
     RenderPass:=Pass as TRenderPass;
     Temporary:=0;
     for ResourceTransition in RenderPass.fResourceTransitions do begin
      if ResourceTransition.fKind in TResourceTransition.AllImages then begin
       Resource:=ResourceTransition.fResource;
       if (Resource.fResourceType is TImageResourceType) and
          (TImageResourceType(Resource.fResourceType).fImageType=TImageType.Surface) then begin
        Temporary:=Temporary or 1;
       end else if (Resource.fResourceType is TImageResourceType) and
                   (TImageResourceType(Resource.fResourceType).fImageType=TImageType.Depth) then begin
        Temporary:=Temporary or 2;
        break;
       end;
      end;
     end;
     if Temporary=1 then begin
      fRootPass:=Pass;
      break;
     end;
    end;
   end;
   if not assigned(fRootPass) then begin
    raise EpvFrameGraph.Create('No root pass found');
   end;
  end;
 end;
 procedure CreateDirectedAcyclicGraphOfGraphPasses;
 type TAction=
       (
        Process,
        Unmark,
        Add
       );
      TStackItem=record
       Action:TAction;
       Pass:TPass;
      end;
      PStackItem=^TStackItem;
      TStack=TpvDynamicStack<TStackItem>;
  function NewStackItem(const aAction:TAction;const aPass:TPass):TStackItem;
  begin
   result.Action:=aAction;
   result.Pass:=aPass;
  end;
 var Index,
     Count,
     Weight:TpvSizeInt;
     Stack:TStack;
     StackItem:TStackItem;
     Pass,
     OtherPass:TPass;
     ResourceTransition,
     OtherResourceTransition:TResourceTransition;
     Resource:TResource;
 begin
  // Construct the directed acyclic graph by doing a modified-DFS-based topological sort at the same time
  Stack.Initialize;
  try
   fTopologicalSortedPasses.Clear;
   for Pass in fPasses do begin
    Pass.fPhysicalPass:=nil;
    if Pass is TRenderPass then begin
     TRenderPass(Pass).fPhysicalRenderPassSubPass:=nil;
    end;
    Pass.fFlags:=Pass.fFlags-[TPass.TFlag.Used,TPass.TFlag.Processed,TPass.TFlag.Marked];
    Pass.fPreviousPasses.Clear;
    Pass.fNextPasses.Clear;
   end;
   Stack.Push(NewStackItem(TAction.Process,fRootPass));
   while Stack.Pop(StackItem) do begin
    Pass:=StackItem.Pass;
    case StackItem.Action of
     TAction.Process:begin
      if TPass.TFlag.Marked in Pass.fFlags then begin
       raise EpvFrameGraphRecursion.Create('Recursion detected');
      end;
      Include(Pass.fFlags,TPass.TFlag.Marked);
      if not (TPass.TFlag.Processed in Pass.fFlags) then begin
       Pass.fFlags:=Pass.fFlags+[TPass.TFlag.Used,TPass.TFlag.Processed];
       for ResourceTransition in Pass.fResourceTransitions do begin
        if (ResourceTransition.fKind in TResourceTransition.AllInputs) and
           not (TResourceTransition.TFlag.PreviousFrameInput in ResourceTransition.Flags) then begin
         Resource:=ResourceTransition.Resource;
         for OtherResourceTransition in Resource.fResourceTransitions do begin
          if (ResourceTransition<>OtherResourceTransition) and
             (Pass<>OtherResourceTransition.fPass) and
             (OtherResourceTransition.fKind in TResourceTransition.AllOutputs) then begin
           if Pass.fPreviousPasses.IndexOf(OtherResourceTransition.fPass)<0 then begin
            Pass.fPreviousPasses.Add(OtherResourceTransition.fPass);
           end;
           if OtherResourceTransition.fPass.fNextPasses.IndexOf(Pass)<0 then begin
            OtherResourceTransition.fPass.fNextPasses.Add(Pass);
           end;
          end;
         end;
        end;
       end;
       if Pass is TRenderPass then begin
        // Pre-sort for better subpass grouping at a later point
        Index:=0;
        Count:=Pass.fPreviousPasses.Count;
        while (Index+1)<Count do begin
         Passes[0]:=Pass.fPreviousPasses[Index];
         Passes[1]:=Pass.fPreviousPasses[Index+1];
         if Passes[0].fQueue<>Passes[1].fQueue then begin
          Weight:=(ord(Passes[0].fQueue=Pass.fQueue) and 1)-(ord(Passes[1].fQueue=Pass.fQueue) and 1);
          if Weight=0 then begin
           if TpvPtrUInt(Passes[0].fQueue)<TpvPtrUInt(Passes[1].fQueue) then begin
            Weight:=-1;
           end else begin
            Weight:=1;
           end;
          end;
         end else begin
          Weight:=(ord(Passes[0] is TRenderPass) and 1)-(ord(Passes[1] is TRenderPass) and 1);
          if Weight=0 then begin
           Weight:=(ord(TRenderPass(Passes[0]).fSize=TRenderPass(Pass).fSize) and 1)-
                   (ord(TRenderPass(Passes[1]).fSize=TRenderPass(Pass).fSize) and 1);
          end;
         end;
         if Weight<0 then begin
          Pass.fPreviousPasses.Exchange(Index,Index+1);
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
       Stack.Push(NewStackItem(TAction.Add,Pass));
      end;
      Stack.Push(NewStackItem(TAction.Unmark,Pass));
      for OtherPass in Pass.fPreviousPasses do begin
       Stack.Push(NewStackItem(TAction.Process,OtherPass));
      end;
     end;
     TAction.Unmark:begin
      Exclude(Pass.fFlags,TPass.TFlag.Marked);
     end;
     TAction.Add:begin
      Pass.fTopologicalSortIndex:=fTopologicalSortedPasses.Add(Pass);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
 procedure CreatePhysicalPasses;
 var Index,
     Count:TpvSizeInt;
     Pass,
     OtherPass:TPass;
     PhysicalRenderPass:TPhysicalRenderPass;
 begin
  // Create physical passes together with merging render passes to sub passes of a real
  // physical render pass
  fPhysicalPasses.Clear;
  fMaximumOverallPhysicalPassIndex:=0;
  Index:=0;
  Count:=fTopologicalSortedPasses.Count;
  while Index<Count do begin
   Pass:=fTopologicalSortedPasses[Index];
   if Pass is TComputePass then begin
    Pass.fPhysicalPass:=TPhysicalComputePass.Create(self,TComputePass(Pass));
    Pass.fPhysicalPass.fIndex:=fPhysicalPasses.Add(Pass.fPhysicalPass);
    inc(Index);
   end else if Pass is TRenderPass then begin
    PhysicalRenderPass:=TPhysicalRenderPass.Create(self,Pass.fQueue);
    Pass.fPhysicalPass:=PhysicalRenderPass;
    Pass.fPhysicalPass.fIndex:=fPhysicalPasses.Add(Pass.fPhysicalPass);
    TRenderPass(Pass).fPhysicalRenderPassSubPass:=TPhysicalRenderPass.TSubPass.Create(PhysicalRenderPass,TRenderPass(Pass));
    TRenderPass(Pass).fPhysicalRenderPassSubPass.fIndex:=PhysicalRenderPass.fSubPasses.Add(TRenderPass(Pass).fPhysicalRenderPassSubPass);
    PhysicalRenderPass.fMultiView:=TRenderPass(Pass).fMultiViewMask<>0;
    inc(Index);
    if not (TPass.TFlag.Toggleable in Pass.fFlags) then begin
     while Index<Count do begin
      OtherPass:=fTopologicalSortedPasses[Index];
      if (not (TPass.TFlag.Toggleable in OtherPass.fFlags)) and
         (OtherPass is TRenderPass) and
         (TRenderPass(OtherPass).fQueue=TRenderPass(Pass).fQueue) and
         (TRenderPass(OtherPass).fSize=TRenderPass(Pass).fSize) then begin
       OtherPass.fPhysicalPass:=Pass.fPhysicalPass;
       TRenderPass(OtherPass).fPhysicalRenderPassSubPass:=TPhysicalRenderPass.TSubPass.Create(PhysicalRenderPass,TRenderPass(OtherPass));
       TRenderPass(OtherPass).fPhysicalRenderPassSubPass.fIndex:=PhysicalRenderPass.fSubPasses.Add(TRenderPass(OtherPass).fPhysicalRenderPassSubPass);
       PhysicalRenderPass.fMultiView:=PhysicalRenderPass.fMultiView or (TRenderPass(OtherPass).fMultiViewMask<>0);
       fMaximumOverallPhysicalPassIndex:=Max(fMaximumOverallPhysicalPassIndex,OtherPass.fPhysicalPass.fIndex);
       inc(Index);
      end else begin
       break;
      end;
     end;
    end;
   end else begin
    inc(Index);
   end;
   fMaximumOverallPhysicalPassIndex:=Max(fMaximumOverallPhysicalPassIndex,Pass.fPhysicalPass.fIndex);
  end;
 end;
 procedure FindRootPhysicalPass;
 var Pass:TPass;
 begin
  fRootPhysicalPass:=nil;
  for Pass in fPasses do begin
   if (TPass.TFlag.Used in Pass.fFlags) and
      assigned(Pass.fPhysicalPass) and
      (Pass=fRootPass) then begin
    fRootPhysicalPass:=Pass.fPhysicalPass;
    break;
   end;
  end;
  if not assigned(fRootPhysicalPass) then begin
   raise EpvFrameGraph.Create('No root physical pass found');
  end;
 end;
 procedure TransferDependenciesFromGraphPassesToPhysicalPasses;
 var Pass,
     OtherPass:TPass;
     ResourceTransition,
     OtherResourceTransition:TResourceTransition;
     Resource:TResource;
 begin
  // Transfer the dependency informations from the graph passes to the physical passes
  for Pass in fPasses do begin
   if (TPass.TFlag.Used in Pass.fFlags) and
      assigned(Pass.fPhysicalPass) then begin
    for ResourceTransition in Pass.fResourceTransitions do begin
     Resource:=ResourceTransition.fResource;
     for OtherResourceTransition in Resource.fResourceTransitions do begin
      if (ResourceTransition<>OtherResourceTransition) and
         (ResourceTransition.fPass<>OtherResourceTransition.fPass) and
         (ResourceTransition.fPass.fPhysicalPass<>OtherResourceTransition.fPass.fPhysicalPass) and
         (TPass.TFlag.Used in OtherResourceTransition.fPass.fFlags) and
         assigned(OtherResourceTransition.fPass.fPhysicalPass) then begin
       OtherPass:=OtherResourceTransition.fPass;
       if (ResourceTransition.fKind in TResourceTransition.AllInputs) and
          (OtherResourceTransition.fKind in TResourceTransition.AllOutputs) and
          (Pass.fPhysicalPass.fInputDependencies.IndexOf(OtherPass.fPhysicalPass)<0) then begin
        Pass.fPhysicalPass.fInputDependencies.Add(OtherPass.fPhysicalPass);
       end;
       if (ResourceTransition.fKind in TResourceTransition.AllOutputs) and
          (OtherResourceTransition.fKind in TResourceTransition.AllInputs) and
          (Pass.fPhysicalPass.fOutputDependencies.IndexOf(OtherPass.fPhysicalPass)<0) then begin
        Pass.fPhysicalPass.fOutputDependencies.Add(OtherPass.fPhysicalPass);
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 procedure CalculateResourceLifetimes;
 var Pass:TPass;
     Resource:TResource;
     ResourceTransition:TResourceTransition;
 begin
  // Calculate resource lifetimes (from minimum physical pass step index to maximum
  // physical pass step index) for calculating aliasing and reusing of resources at a later point
  for Resource in fResources do begin
   Resource.fMinimumPhysicalPassStepIndex:=High(TpvSizeInt);
   Resource.fMaximumPhysicalPassStepIndex:=Low(TpvSizeInt);
   for ResourceTransition in Resource.fResourceTransitions do begin
    Pass:=ResourceTransition.fPass;
    if assigned(Pass.fPhysicalPass) then begin
     if ((ResourceTransition.fFlags*[TResourceTransition.TFlag.PreviousFrameInput,
                                     TResourceTransition.TFlag.NextFrameOutput])<>[]) or
        ((ResourceTransition.fResource.fResourceType is TImageResourceType) and
         (TImageResourceType(ResourceTransition.fResource.fResourceType).fImageType=TImageType.Surface)) then begin
      // In this cases, this one resource must life from the begin to the end of the whole
      // directed acyclic graph for the simplicity of safety, because it can be still optimized
      // in a better way later
      if not Resource.fUsed then begin
       Resource.fUsed:=true;
       Resource.fMinimumPhysicalPassStepIndex:=0;
       Resource.fMaximumPhysicalPassStepIndex:=fMaximumOverallPhysicalPassIndex;
      end;
     end else begin
      if Resource.fUsed then begin
       Resource.fMinimumPhysicalPassStepIndex:=Min(Resource.fMinimumPhysicalPassStepIndex,Pass.fPhysicalPass.fIndex);
       Resource.fMaximumPhysicalPassStepIndex:=Max(Resource.fMaximumPhysicalPassStepIndex,Pass.fPhysicalPass.fIndex);
      end else begin
       Resource.fUsed:=true;
       Resource.fMinimumPhysicalPassStepIndex:=Pass.fPhysicalPass.fIndex;
       Resource.fMaximumPhysicalPassStepIndex:=Pass.fPhysicalPass.fIndex;
      end;
     end;
    end;
   end;
  end;
 end;
 procedure CreateResourceReuseGroups;
  function CanResourceReused(const aResource:TResource):boolean;
  begin
   result:=(not aResource.fResourceType.fPersientent) and
           (not ((aResource.fResourceType is TImageResourceType) and
                 (TImageResourceType(aResource.fResourceType).fImageType=TImageType.Surface)));
  end;
 var Index,
     OtherIndex:TpvSizeInt;
     Resource,
     OtherResource:TResource;
     ResourceTransition:TResourceTransition;
 begin
  // Calculate resource reuse groups, depending on the non-intersecting resource lifetime span
  // segments and resource types
  for Resource in fResources do begin
   Resource.fResourceReuseGroup:=nil;
  end;
  fResourceReuseGroups.Clear;
  for Index:=0 to fResources.Count-1 do begin
   Resource:=fResources.Items[Index];
   if not assigned(Resource.fResourceReuseGroup) then begin
    Resource.fResourceReuseGroup:=TResourceReuseGroup.Create(self);
    Resource.fResourceReuseGroup.fResourceType:=Resource.fResourceType;
    Resource.fResourceReuseGroup.fResources.Add(Resource);
    if CanResourceReused(Resource) then begin
     for OtherIndex:=Index+1 to fResources.Count-1 do begin
      OtherResource:=fResources.Items[OtherIndex];
      if (not assigned(OtherResource.fResourceReuseGroup)) and
         (Resource.fResourceType=OtherResource.fResourceType) and
         CanResourceReused(OtherResource) and
         (Min(Resource.fMaximumPhysicalPassStepIndex,
              OtherResource.fMaximumPhysicalPassStepIndex)>Max(Resource.fMinimumPhysicalPassStepIndex,
                                                               OtherResource.fMinimumPhysicalPassStepIndex)) then begin
       OtherResource.fResourceReuseGroup:=Resource.fResourceReuseGroup;
       OtherResource.fResourceReuseGroup.fResources.Add(OtherResource);
      end;
     end;
    end;
   end;
  end;
 end;
 procedure CreatePhysicalPassQueueSequences;
 var PhysicalPass:TPhysicalPass;
 begin
  // fPhysicalPasses is already toplogically sorted, so it's easy here
  for PhysicalPass in fPhysicalPasses do begin
   PhysicalPass.fQueue.fPhysicalPasses.Add(PhysicalPass);
  end;
 end;
 procedure CreateResourceReuseGroupData;
 var MinimumTopologicalSortIndex:TpvSizeInt;
     ResourceReuseGroup:TResourceReuseGroup;
     ResourceType:TResourceType;
     ImageResourceType:TImageResourceType;
     BufferResourceType:TBufferResourceType;
     ResourcePhysicalImageData:TResourcePhysicalImageData;
     ResourcePhysicalBufferData:TResourcePhysicalBufferData;
     Resource:TResource;
     ResourceTransition:TResourceTransition;
 begin
  // Create data for the resource reuse groups
  for ResourceReuseGroup in fResourceReuseGroups do begin
   ResourceType:=ResourceReuseGroup.fResourceType;
   if ResourceType is TImageResourceType then begin
    ImageResourceType:=TImageResourceType(ResourceType);
    if not assigned(ResourceReuseGroup.fResourcePhysicalData) then begin
     ResourceReuseGroup.fResourcePhysicalData:=TResourcePhysicalImageData.Create(self);
     ResourcePhysicalImageData:=TResourcePhysicalImageData(ResourceReuseGroup.fResourcePhysicalData);
     ResourcePhysicalImageData.fResourceType:=ResourceType;
     ResourcePhysicalImageData.fIsSurface:=ImageResourceType.fImageType=TImageType.Surface;
     ResourcePhysicalImageData.fImageUsageFlags:=TVkImageUsageFlags(ImageResourceType.fImageUsage);
     ResourcePhysicalImageData.fFormat:=ImageResourceType.fFormat;
     ResourcePhysicalImageData.fExtent.width:=Max(1,trunc(ImageResourceType.fImageSize.Size.x));
     ResourcePhysicalImageData.fExtent.height:=Max(1,trunc(ImageResourceType.fImageSize.Size.y));
     ResourcePhysicalImageData.fExtent.depth:=Max(1,trunc(ImageResourceType.fImageSize.Size.z));
     ResourcePhysicalImageData.fCountMipMaps:=1;
     ResourcePhysicalImageData.fCountArrayLayers:=trunc(ImageResourceType.fImageSize.Size.w);
     ResourcePhysicalImageData.fSamples:=ImageResourceType.fSamples;
     ResourcePhysicalImageData.fTiling:=VK_IMAGE_TILING_OPTIMAL;
     ResourcePhysicalImageData.fInitialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
     ResourcePhysicalImageData.fFirstInitialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
     MinimumTopologicalSortIndex:=High(TpvSizeInt);
     for Resource in ResourceReuseGroup.fResources do begin
      for ResourceTransition in Resource.fResourceTransitions do begin
       if ResourceTransition.fPass.fTopologicalSortIndex<MinimumTopologicalSortIndex then begin
        MinimumTopologicalSortIndex:=ResourceTransition.fPass.fTopologicalSortIndex;
        ResourcePhysicalImageData.fFirstInitialLayout:=ResourceTransition.fLayout;
       end;
      end;
     end;
     ResourcePhysicalImageData.fImageCreateFlags:=0;
     if ResourcePhysicalImageData.fExtent.depth>1 then begin
      ResourcePhysicalImageData.fImageType:=VK_IMAGE_TYPE_3D;
     end else begin
      ResourcePhysicalImageData.fImageType:=VK_IMAGE_TYPE_2D;
     end;
     ResourcePhysicalImageData.fSharingMode:=VK_SHARING_MODE_EXCLUSIVE;
     ResourcePhysicalImageData.fImageSubresourceRange.aspectMask:=ImageResourceType.fImageType.GetAspectMask;
     ResourcePhysicalImageData.fImageSubresourceRange.baseMipLevel:=0;
     ResourcePhysicalImageData.fImageSubresourceRange.levelCount:=ImageResourceType.fCountMipMapLevels;
     ResourcePhysicalImageData.fImageSubresourceRange.baseArrayLayer:=0;
     ResourcePhysicalImageData.fImageSubresourceRange.layerCount:=ResourcePhysicalImageData.fCountArrayLayers;
     if ResourcePhysicalImageData.fExtent.depth>1 then begin
      if ResourcePhysicalImageData.fImageSubresourceRange.layerCount>1 then begin
       raise EpvFrameGraph.Create('3D array image not supported');
      end else begin
       ResourcePhysicalImageData.fImageViewType:=VK_IMAGE_VIEW_TYPE_3D;
      end;
     end else begin
      if ResourcePhysicalImageData.fImageSubresourceRange.layerCount>1 then begin
       ResourcePhysicalImageData.fImageViewType:=VK_IMAGE_VIEW_TYPE_2D_ARRAY;
      end else begin
       ResourcePhysicalImageData.fImageViewType:=VK_IMAGE_VIEW_TYPE_2D;
      end;
     end;
     ResourcePhysicalImageData.fComponents:=ImageResourceType.fComponents;
    end;
   end else if ResourceType is TBufferResourceType then begin
    ResourceReuseGroup.fResourcePhysicalData:=TResourcePhysicalBufferData.Create(self);
    ResourcePhysicalBufferData:=TResourcePhysicalBufferData(ResourceReuseGroup.fResourcePhysicalData);
    ResourcePhysicalBufferData.fResourceType:=ResourceType;
    // TODO
    Assert(false,'TODO');
   end else begin
    raise EpvFrameGraph.Create('Invalid resource type');
   end;
  end;
 end;
 procedure CreatePhysicalPassPipelineBarriersAndPhysicalRenderPassSubPassDependencies;
  procedure AddSubPassDependency(const aSubPassDependencies:TPhysicalRenderPass.TSubPassDependencies;
                                 const aSubPassDependency:TPhysicalRenderPass.TSubPassDependency);
  var Index:TpvSizeInt;
      SubPassDependency:TPhysicalRenderPass.PSubPassDependency;
  begin
   SubPassDependency:=nil;
   for Index:=0 to aSubPassDependencies.Count-1 do begin
    if (aSubPassDependencies.Items[Index].SrcSubPass=aSubPassDependency.SrcSubPass) and
       (aSubPassDependencies.Items[Index].DstSubPass=aSubPassDependency.DstSubPass) then begin
     SubPassDependency:=@aSubPassDependencies.Items[Index];
    end;
   end;
   if assigned(SubPassDependency) then begin
    SubPassDependency^.SrcStageMask:=SubPassDependency^.SrcStageMask or aSubPassDependency.SrcStageMask;
    SubPassDependency^.DstStageMask:=SubPassDependency^.DstStageMask or aSubPassDependency.DstStageMask;
    SubPassDependency^.SrcAccessMask:=SubPassDependency^.SrcAccessMask or aSubPassDependency.SrcAccessMask;
    SubPassDependency^.DstAccessMask:=SubPassDependency^.DstAccessMask or aSubPassDependency.DstAccessMask;
    SubPassDependency^.DependencyFlags:=SubPassDependency^.DependencyFlags or aSubPassDependency.DependencyFlags;
   end else begin
    aSubPassDependencies.Add(aSubPassDependency);
   end;
  end;
  procedure AddSemaphoreSignalWait(const aSignallingPhysicalPass:TPhysicalPass;
                                   const aWaitingPhysicalPass:TPhysicalPass;
                                   const aDstStageMask:TVkPipelineStageFlags);
  var WaitingSemaphoreIndex:TpvSizeInt;
      WaitingSemaphore:TPhysicalPass.PWaitingSemaphore;
  begin
   WaitingSemaphore:=nil;
   for WaitingSemaphoreIndex:=0 to aWaitingPhysicalPass.fWaitingSemaphores.Count-1 do begin
    if aWaitingPhysicalPass.fWaitingSemaphores.Items[WaitingSemaphoreIndex].SignallingPhysicalPass=aSignallingPhysicalPass then begin
     WaitingSemaphore:=@aWaitingPhysicalPass.fWaitingSemaphores.Items[WaitingSemaphoreIndex];
     break;
    end;
   end;
   if not assigned(WaitingSemaphore) then begin
    WaitingSemaphoreIndex:=aWaitingPhysicalPass.fWaitingSemaphores.AddNew;
    WaitingSemaphore:=@aWaitingPhysicalPass.fWaitingSemaphores.Items[WaitingSemaphoreIndex];
    WaitingSemaphore^.SignallingPhysicalPass:=aSignallingPhysicalPass;
    WaitingSemaphore^.DstStageMask:=0;
   end;
   WaitingSemaphore^.DstStageMask:=WaitingSemaphore^.DstStageMask or aDstStageMask;
  end;
  procedure AddPipelineBarrier(const aBeforeAfter:TBeforeAfter;
                               const aPhysicalPass:TPhysicalPass;
                               const aResourcePhysicalData:TResourcePhysicalData;
                               const aFromResourceTransition:TResourceTransition;
                               const aToResourceTransition:TResourceTransition;
                               const aSrcQueueFamilyIndex:TVkUInt32;
                               const aDstQueueFamilyIndex:TVkUInt32;
                               const aSrcStageMask:TVkPipelineStageFlags;
                               const aDstStageMask:TVkPipelineStageFlags;
                               const aSrcAccessMask:TVkAccessFlags;
                               const aDstAccessMask:TVkAccessFlags;
                               const aDependencyFlags:TVkDependencyFlags);
  var PipelineBarrierGroupIndex:TpvSizeInt;
      PipelineBarrierGroups:TPhysicalPass.TPipelineBarrierGroups;
      PipelineBarrierGroup,
      FoundPipelineBarrierGroup:TPhysicalPass.TPipelineBarrierGroup;
      BarrierMapItem:TPhysicalPass.TPipelineBarrierGroup.TBarrierMapItem;
      BufferMemoryBarrier:TVkBufferMemoryBarrier;
      ImageMemoryBarrier:TVkImageMemoryBarrier;
  begin
   case aBeforeAfter of
    TBeforeAfter.Before:begin
     PipelineBarrierGroups:=aPhysicalPass.fBeforePipelineBarrierGroups;
    end;
    TBeforeAfter.After:begin
     PipelineBarrierGroups:=aPhysicalPass.fAfterPipelineBarrierGroups;
    end;
    else begin
     PipelineBarrierGroups:=nil;
    end;
   end;
   if not assigned(PipelineBarrierGroups) then begin
    raise EpvFrameGraph.Create('Invalid error 2018-10-06-23-37-0000');
   end;
   FoundPipelineBarrierGroup:=nil;
   for PipelineBarrierGroupIndex:=0 to PipelineBarrierGroups.Count-1 do begin
    PipelineBarrierGroup:=PipelineBarrierGroups[PipelineBarrierGroupIndex];
    if (PipelineBarrierGroup.fSrcStageMask=aSrcStageMask) and
       (PipelineBarrierGroup.fDstStageMask=aDstStageMask) and
       (PipelineBarrierGroup.fDependencyFlags=aDependencyFlags) then begin
     FoundPipelineBarrierGroup:=PipelineBarrierGroup;
     break;
    end;
   end;
   if assigned(FoundPipelineBarrierGroup) then begin
    PipelineBarrierGroup:=FoundPipelineBarrierGroup;
   end else begin
    PipelineBarrierGroup:=TPhysicalPass.TPipelineBarrierGroup.Create(aSrcStageMask,aDstStageMask,aDependencyFlags);
    PipelineBarrierGroups.Add(PipelineBarrierGroup);
   end;
   // TODO
   if aResourcePhysicalData is TResourcePhysicalImageData then begin
    FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
    ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
    ImageMemoryBarrier.srcAccessMask:=aSrcAccessMask;
    ImageMemoryBarrier.dstAccessMask:=aDstAccessMask;
    ImageMemoryBarrier.oldLayout:=aFromResourceTransition.fLayout;
    ImageMemoryBarrier.newLayout:=aToResourceTransition.fLayout;
    ImageMemoryBarrier.srcQueueFamilyIndex:=aSrcQueueFamilyIndex;
    ImageMemoryBarrier.dstQueueFamilyIndex:=aDstQueueFamilyIndex;
    ImageMemoryBarrier.image:=0;
    ImageMemoryBarrier.subresourceRange:=TResourcePhysicalImageData(aResourcePhysicalData).fImageSubresourceRange;
    BarrierMapItem.Kind:=TPhysicalPass.TPipelineBarrierGroup.TBarrierMapItemKind.Image;
    BarrierMapItem.BarrierIndex:=PipelineBarrierGroup.fImageMemoryBarrierDynamicArray.Add(ImageMemoryBarrier);
    BarrierMapItem.ResourcePhysicalData:=aResourcePhysicalData;
    PipelineBarrierGroup.fBarrierMapItemDynamicArray.Add(BarrierMapItem);
   end else if aResourcePhysicalData is TResourcePhysicalBufferData then begin
    FillChar(BufferMemoryBarrier,SizeOf(TVkBufferMemoryBarrier),#0);
    BufferMemoryBarrier.sType:=VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER;
    BufferMemoryBarrier.srcAccessMask:=aSrcAccessMask;
    BufferMemoryBarrier.dstAccessMask:=aDstAccessMask;
    BufferMemoryBarrier.srcQueueFamilyIndex:=aSrcQueueFamilyIndex;
    BufferMemoryBarrier.dstQueueFamilyIndex:=aDstQueueFamilyIndex;
    BufferMemoryBarrier.buffer:=0;
    case aBeforeAfter of
     TBeforeAfter.Before:begin
      BufferMemoryBarrier.offset:=aFromResourceTransition.fBufferSubresourceRange.Offset;
      BufferMemoryBarrier.size:=aFromResourceTransition.fBufferSubresourceRange.Range;
     end;
     TBeforeAfter.After:begin
      BufferMemoryBarrier.offset:=aToResourceTransition.fBufferSubresourceRange.Offset;
      BufferMemoryBarrier.size:=aToResourceTransition.fBufferSubresourceRange.Range;
     end;
    end;
    BarrierMapItem.Kind:=TPhysicalPass.TPipelineBarrierGroup.TBarrierMapItemKind.Buffer;
    BarrierMapItem.BarrierIndex:=PipelineBarrierGroup.fBufferMemoryBarrierDynamicArray.Add(BufferMemoryBarrier);
    BarrierMapItem.ResourcePhysicalData:=aResourcePhysicalData;
    PipelineBarrierGroup.fBarrierMapItemDynamicArray.Add(BarrierMapItem);
   end else begin
    Assert(false);
   end;
  end;
 var ResourceTransitionIndex,
     OtherResourceTransitionIndex,
     PipelineBarrierGroupIndex:TpvSizeInt;
     Resource:TResource;
     ResourceTransition,
     OtherResourceTransition:TResourceTransition;
     SubPassDependency:TPhysicalRenderPass.TSubPassDependency;
     SrcQueueFamilyIndex,
     DstQueueFamilyIndex:TVkUInt32;
     SrcStageMask,
     DstStageMask:TVkPipelineStageFlags;
     SrcAccessMask,
     DstAccessMask:TVkAccessFlags;
     PipelineBarrierGroup,
     FoundPipelineBarrierGroup:TPhysicalPass.TPipelineBarrierGroup;
     DependencyFlags:TVkDependencyFlags;
 begin
  for Resource in fResources do begin
   for ResourceTransitionIndex:=0 to Resource.fResourceTransitions.Count-1 do begin
    ResourceTransition:=Resource.fResourceTransitions[ResourceTransitionIndex];
    if (ResourceTransition.fKind in TResourceTransition.AllOutputs) and
       (TPass.TFlag.Used in ResourceTransition.fPass.fFlags) and
       assigned(ResourceTransition.fPass.fPhysicalPass) then begin
     for OtherResourceTransitionIndex:=0 to Resource.fResourceTransitions.Count-1 do begin
      if ResourceTransitionIndex<>OtherResourceTransitionIndex then begin
       OtherResourceTransition:=Resource.fResourceTransitions[OtherResourceTransitionIndex];
       if (ResourceTransition<>OtherResourceTransition) and
          (ResourceTransition.fPass<>OtherResourceTransition.fPass) and
          (OtherResourceTransition.fKind in TResourceTransition.AllInputs) and
          (TPass.TFlag.Used in OtherResourceTransition.fPass.fFlags) and
          assigned(OtherResourceTransition.fPass.fPhysicalPass) then begin
        if (ResourceTransition.fPass is TRenderPass) and
           (OtherResourceTransition.fPass is TRenderPass) and
           (ResourceTransition.fPass.fPhysicalPass is TPhysicalRenderPass) and
           (OtherResourceTransition.fPass.fPhysicalPass is TPhysicalRenderPass) and
           (ResourceTransition.fKind in TResourceTransition.AllImageOutputs) and
           (OtherResourceTransition.fKind in TResourceTransition.AllImageInputs) then begin
         GetPipelineStageMasks(ResourceTransition,
                               OtherResourceTransition,
                               SubPassDependency.SrcStageMask,
                               SubPassDependency.DstStageMask
                              );
         GetAccessMasks(ResourceTransition,
                        OtherResourceTransition,
                        SubPassDependency.SrcAccessMask,
                        SubPassDependency.DstAccessMask
                       );
         SubPassDependency.DependencyFlags:=TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT);
         if ResourceTransition.fPass.fPhysicalPass=OtherResourceTransition.fPass.fPhysicalPass then begin
          SubPassDependency.SrcSubPass:=TRenderPass(ResourceTransition.fPass).fPhysicalRenderPassSubPass;
          SubPassDependency.DstSubPass:=TRenderPass(OtherResourceTransition.fPass).fPhysicalRenderPassSubPass;
          AddSubPassDependency(TPhysicalRenderPass(ResourceTransition.fPass.fPhysicalPass).fSubPassDependencies,SubPassDependency);
         end else begin
          SubPassDependency.SrcSubPass:=TRenderPass(ResourceTransition.fPass).fPhysicalRenderPassSubPass;
          SubPassDependency.DstSubPass:=nil;
          AddSubPassDependency(TPhysicalRenderPass(ResourceTransition.fPass.fPhysicalPass).fSubPassDependencies,SubPassDependency);
          SubPassDependency.SrcSubPass:=nil;
          SubPassDependency.DstSubPass:=TRenderPass(OtherResourceTransition.fPass).fPhysicalRenderPassSubPass;
          AddSubPassDependency(TPhysicalRenderPass(OtherResourceTransition.fPass.fPhysicalPass).fSubPassDependencies,SubPassDependency);
         end;
        end else begin
         if ResourceTransition.fPass.fQueue.fPhysicalQueue.QueueFamilyIndex<>OtherResourceTransition.fPass.fQueue.fPhysicalQueue.QueueFamilyIndex then begin
          SrcQueueFamilyIndex:=ResourceTransition.fPass.fQueue.fPhysicalQueue.QueueFamilyIndex;
          DstQueueFamilyIndex:=OtherResourceTransition.fPass.fQueue.fPhysicalQueue.QueueFamilyIndex;
         end else begin
          SrcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
          DstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
         end;
         GetPipelineStageMasks(ResourceTransition,
                               OtherResourceTransition,
                               SrcStageMask,
                               DstStageMask
                              );
         GetAccessMasks(ResourceTransition,
                        OtherResourceTransition,
                        SrcAccessMask,
                        DstAccessMask
                       );
         DependencyFlags:=0;
         if ResourceTransition.fPass.fQueue.fPhysicalQueue.QueueFamilyIndex<>OtherResourceTransition.fPass.fQueue.fPhysicalQueue.QueueFamilyIndex then begin
          AddPipelineBarrier(TBeforeAfter.After, // Release
                             ResourceTransition.fPass.fPhysicalPass,
                             Resource.fResourceReuseGroup.fResourcePhysicalData,
                             ResourceTransition,
                             OtherResourceTransition,
                             SrcQueueFamilyIndex,
                             DstQueueFamilyIndex,
                             SrcStageMask,
                             TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                             SrcAccessMask,
                             0,
                             DependencyFlags
                            );
          AddPipelineBarrier(TBeforeAfter.Before, // Acquire
                             OtherResourceTransition.fPass.fPhysicalPass,
                             Resource.fResourceReuseGroup.fResourcePhysicalData,
                             ResourceTransition,
                             OtherResourceTransition,
                             SrcQueueFamilyIndex,
                             DstQueueFamilyIndex,
                             TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                             DstStageMask,
                             0,
                             DstAccessMask,
                             DependencyFlags
                            );
          AddSemaphoreSignalWait(ResourceTransition.fPass.fPhysicalPass, // Signalling / After
                                 OtherResourceTransition.fPass.fPhysicalPass, // Waiting / Before
                                 DstStageMask
                                );
         end else begin
          AddPipelineBarrier(TBeforeAfter.Before,
                             OtherResourceTransition.fPass.fPhysicalPass,
                             Resource.fResourceReuseGroup.fResourcePhysicalData,
                             ResourceTransition,
                             OtherResourceTransition,
                             SrcQueueFamilyIndex,
                             DstQueueFamilyIndex,
                             SrcStageMask,
                             DstStageMask,
                             SrcAccessMask,
                             DstAccessMask,
                             DependencyFlags
                            );
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 procedure SortPhysicalRenderPassSubPassDependencies;
 var PhysicalPass:TPhysicalPass;
     PhysicalRenderPass:TPhysicalRenderPass;
 begin
  for PhysicalPass in fPhysicalPasses do begin
   if PhysicalPass is TPhysicalRenderPass then begin
    PhysicalRenderPass:=TPhysicalRenderPass(PhysicalPass);
    if PhysicalRenderPass.fSubPassDependencies.Count>1 then begin
     TpvTypedSort<TPhysicalRenderPass.TSubPassDependency>.IntroSort(@PhysicalRenderPass.fSubPassDependencies.Items[0],
                                                                    0,
                                                                    PhysicalRenderPass.fSubPassDependencies.Count-1,
                                                                    ComparePhysicalRenderPassSubPassDependencies);
    end;
   end;
  end;
 end;
begin

 fPhysicalPasses.Clear;

 IndexingPasses;

 ValidateAttachmentImages;

 ValidateResources;

 FindRootPass;

 CreateDirectedAcyclicGraphOfGraphPasses;

 CreatePhysicalPasses;

 FindRootPhysicalPass;

 TransferDependenciesFromGraphPassesToPhysicalPasses;

 CalculateResourceLifetimes;

 CreateResourceReuseGroups;

 CreatePhysicalPassQueueSequences;

 CreateResourceReuseGroupData;

 CreatePhysicalPassPipelineBarriersAndPhysicalRenderPassSubPassDependencies;

 SortPhysicalRenderPassSubPassDependencies;

end;

procedure TpvFrameGraph.Show;
var ResourceReuseGroup:TResourceReuseGroup;
    PhysicalPass:TPhysicalPass;
begin
 for ResourceReuseGroup in fResourceReuseGroups do begin
  ResourceReuseGroup.Show;
 end;
 for PhysicalPass in fPhysicalPasses do begin
  PhysicalPass.Show;
 end;
end;

procedure TpvFrameGraph.Hide;
var ResourceReuseGroup:TResourceReuseGroup;
    PhysicalPass:TPhysicalPass;
begin
 for PhysicalPass in fPhysicalPasses do begin
  PhysicalPass.Hide;
 end;
 for ResourceReuseGroup in fResourceReuseGroups do begin
  ResourceReuseGroup.Hide;
 end;
end;

procedure TpvFrameGraph.AfterCreateSwapChain;
var SwapChainImageIndex,
    WaitingSemaphoreIndex:TpvSizeInt;
    ResourceReuseGroup:TResourceReuseGroup;
    PhysicalPass:TPhysicalPass;
    SubmitInfo:PVkSubmitInfo;
    WaitingSemaphore:TPhysicalPass.PWaitingSemaphore;
begin
 for ResourceReuseGroup in fResourceReuseGroups do begin
  ResourceReuseGroup.AfterCreateSwapChain;
 end;
 for PhysicalPass in fPhysicalPasses do begin
  PhysicalPass.AfterCreateSwapChain;
 end;
 for PhysicalPass in fPhysicalPasses do begin
  begin
   PhysicalPass.fWaitingSemaphoreHandles.Clear;
   for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
    for WaitingSemaphoreIndex:=0 to PhysicalPass.fWaitingSemaphores.Count-1 do begin
     WaitingSemaphore:=@PhysicalPass.fWaitingSemaphores.Items[WaitingSemaphoreIndex];
     PhysicalPass.fWaitingSemaphoreHandles.Add(WaitingSemaphore^.SignallingPhysicalPass.fSignallingSemaphores[SwapChainImageIndex].Handle);
    end;
   end;
   PhysicalPass.fWaitingSemaphoreHandles.Finish;
  end;
  begin
   PhysicalPass.fWaitingSemaphoreDstStageMasks.Clear;
   for WaitingSemaphoreIndex:=0 to PhysicalPass.fWaitingSemaphores.Count-1 do begin
    WaitingSemaphore:=@PhysicalPass.fWaitingSemaphores.Items[WaitingSemaphoreIndex];
    PhysicalPass.fWaitingSemaphoreDstStageMasks.Add(WaitingSemaphore^.DstStageMask);
   end;
   PhysicalPass.fWaitingSemaphoreDstStageMasks.Finish;
  end;
  for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
   SubmitInfo:=@PhysicalPass.fSubmitInfos[SwapChainImageIndex];
   FillChar(SubmitInfo^,SizeOf(TVkSubmitInfo),#0);
   SubmitInfo^.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
   SubmitInfo^.pNext:=nil;
   SubmitInfo^.waitSemaphoreCount:=PhysicalPass.fWaitingSemaphores.Count;
   if SubmitInfo^.waitSemaphoreCount>0 then begin
    SubmitInfo^.pWaitSemaphores:=@PhysicalPass.fWaitingSemaphoreHandles.Items[PhysicalPass.fWaitingSemaphores.Count*SwapChainImageIndex];
    SubmitInfo^.pWaitDstStageMask:=@PhysicalPass.fWaitingSemaphores.Items[0];
   end;
   SubmitInfo^.commandBufferCount:=1;
   SubmitInfo^.pCommandBuffers:=@PhysicalPass.fCommandBuffers[SwapChainImageIndex].Handle;
   SubmitInfo^.signalSemaphoreCount:=1;
   SubmitInfo^.pSignalSemaphores:=@PhysicalPass.fSignallingSemaphores[SwapChainImageIndex].Handle;
  end;
 end;
end;

procedure TpvFrameGraph.BeforeDestroySwapChain;
var ResourceReuseGroup:TResourceReuseGroup;
    PhysicalPass:TPhysicalPass;
begin
 for PhysicalPass in fPhysicalPasses do begin
  PhysicalPass.BeforeDestroySwapChain;
 end;
 for ResourceReuseGroup in fResourceReuseGroups do begin
  ResourceReuseGroup.BeforeDestroySwapChain;
 end;
end;

procedure TpvFrameGraph.Update(const aUpdateFrameIndex:TpvSizeInt);
var QueueIndex,Index,SubPassIndex:TpvSizeInt;
    Queue:TQueue;
    PhysicalPass:TPhysicalPass;
    PhysicalComputePass:TPhysicalComputePass;
    PhysicalRenderPass:TPhysicalRenderPass;
    PhysicalRenderPassSubPass:TPhysicalRenderPass.TSubPass;
begin
 for QueueIndex:=0 to fQueues.Count-1 do begin
  Queue:=fQueues[QueueIndex];
  for Index:=0 to Queue.fPhysicalPasses.Count-1 do begin
   PhysicalPass:=Queue.fPhysicalPasses[Index];
   if assigned(PhysicalPass) then begin
    if PhysicalPass is TPhysicalComputePass then begin
     PhysicalComputePass:=TPhysicalComputePass(PhysicalPass);
     PhysicalComputePass.fComputePass.fDoubleBufferedEnabledState[aUpdateFrameIndex and 1]:=TPass.TFlag.Enabled in PhysicalComputePass.fComputePass.fFlags;
    end else if PhysicalPass is TPhysicalRenderPass then begin
     PhysicalRenderPass:=TPhysicalRenderPass(PhysicalPass);
     for SubPassIndex:=0 to PhysicalRenderPass.fSubPasses.Count-1 do begin
      PhysicalRenderPassSubPass:=PhysicalRenderPass.fSubPasses[SubPassIndex];
      PhysicalRenderPassSubPass.fRenderPass.fDoubleBufferedEnabledState[aUpdateFrameIndex and 1]:=TPass.TFlag.Enabled in PhysicalRenderPassSubPass.fRenderPass.fFlags;
     end;
    end;
   end;
  end;
 end;
end;

procedure TpvFrameGraph.ExecuteQueue(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aQueue:TQueue);
var Index,SubPassIndex:TpvSizeInt;
    PhysicalPass:TPhysicalPass;
    PhysicalComputePass:TPhysicalComputePass;
    PhysicalRenderPass:TPhysicalRenderPass;
    PhysicalRenderPassSubPass:TPhysicalRenderPass.TSubPass;
begin
 for Index:=0 to aQueue.fPhysicalPasses.Count-1 do begin
  PhysicalPass:=aQueue.fPhysicalPasses[Index];
  if assigned(PhysicalPass) then begin
   if PhysicalPass is TPhysicalComputePass then begin
    PhysicalComputePass:=TPhysicalComputePass(PhysicalPass);
    if PhysicalComputePass.fComputePass.fDoubleBufferedEnabledState[fDrawFrameIndex and 1] then begin
     // TODO
    end;
   end else if PhysicalPass is TPhysicalRenderPass then begin
    PhysicalRenderPass:=TPhysicalRenderPass(PhysicalPass);
    for SubPassIndex:=0 to PhysicalRenderPass.fSubPasses.Count-1 do begin
     PhysicalRenderPassSubPass:=PhysicalRenderPass.fSubPasses[SubPassIndex];
     if PhysicalRenderPassSubPass.fRenderPass.fDoubleBufferedEnabledState[fDrawFrameIndex and 1] then begin
      // TODO
     end;
    end;
   end;
  end;
 end;
end;

procedure TpvFrameGraph.ExecuteQueueParallelForJobMethod(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
var Index:TPasMPNativeInt;
begin
 for Index:=aFromIndex to aToIndex do begin
  ExecuteQueue(aJob,aThreadIndex,fQueues[Index]);
 end;
end;

procedure TpvFrameGraph.Draw(const aDrawFrameIndex:TpvSizeInt);
begin
 fDrawFrameIndex:=aDrawFrameIndex;
 if fCanDoParallelProcessing and assigned(pvApplication) then begin
  pvApplication.PasMPInstance.ParallelFor(nil,0,fQueues.Count-1,ExecuteQueueParallelForJobMethod,1,16,nil,0);
 end else begin
  ExecuteQueueParallelForJobMethod(nil,0,nil,0,fQueues.Count-1);
 end;
end;

end.
