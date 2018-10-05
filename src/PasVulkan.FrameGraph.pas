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

     EpvFrameGraphMismatchAttachmentSize=class(EpvFrameGraph);

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
             public
              Kind:TKind;
              constructor Create(const aKind:TKind);
            end;
            PStoreOp=^TStoreOp;
            TAttachmentType=
             (
              Undefined,
              Surface,
              Color,
              Depth,
              DepthStencil,
              Stencil
             );
            TAttachmentTypeHelper=record helper for TAttachmentType
             public
              function GetAspectMask:TVkImageAspectFlags;
            end;
            PAttachmentType=^TAttachmentType;
            TAttachmentSize=packed record
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
              class function CreateEmpty:TAttachmentSize; static;
              constructor Create(const aKind:TAttachmentSize.TKind;
                                 const aWidth:tpvFloat=1.0;
                                 const aHeight:TpvFloat=1.0;
                                 const aDepth:TpvFloat=1.0;
                                 const aLayers:TpvFloat=1.0); overload;
              constructor Create(const aKind:TAttachmentSize.TKind;
                                 const aSize:TpvVector2;
                                 const aDepth:TpvFloat=1.0;
                                 const aLayers:TpvFloat=1.0); overload;
              constructor Create(const aKind:TAttachmentSize.TKind;
                                 const aSize:TpvVector3;
                                 const aLayers:TpvFloat=1.0); overload;
              constructor Create(const aKind:TAttachmentSize.TKind;
                                 const aSize:TpvVector4); overload;
              class operator Equal(const aLeft,aRight:TAttachmentSize):boolean;
              class operator NotEqual(const aLeft,aRight:TAttachmentSize):boolean;
            end;
            PAttachmentSize=^TAttachmentSize;
            TPhysicalPass=class;
            TPhysicalPasses=TpvObjectGenericList<TPhysicalPass>;
            TQueue=class
             private
              fFrameGraph:TpvFrameGraph;
              fPhysicalQueue:TpvVulkanQueue;
              fPhysicalPasses:TPhysicalPasses;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPhysicalQueue:TpvVulkanQueue); reintroduce;
              destructor Destroy; override;
            end;
            TQueues=TpvObjectGenericList<TQueue>;
            TResourceType=class
             public
              type TMetaType=
                    (
                     None,
                     Attachment,
                     Image,
                     Buffer
                    );
                    PMetaType=^TMetaType;
                    TAttachmentData=record
                     public
                      Format:TVkFormat;
                      Samples:TVkSampleCountFlagBits;
                      AttachmentType:TAttachmentType;
                      AttachmentSize:TAttachmentSize;
                      ImageUsage:TVkImageUsageFlags;
                      Components:TVkComponentMapping;
                      class function CreateEmpty:TAttachmentData; static;
                      constructor Create(const aFormat:TVkFormat;
                                         const aSamples:TVkSampleCountFlagBits;
                                         const aAttachmentType:TAttachmentType;
                                         const aAttachmentSize:TAttachmentSize;
                                         const aImageUsage:TVkImageUsageFlags;
                                         const aComponents:TVkComponentMapping); overload;
                      class operator Equal(const aLeft,aRight:TAttachmentData):boolean;
                      class operator NotEqual(const aLeft,aRight:TAttachmentData):boolean;
                    end;
                    PAttachmentData=^TAttachmentData;
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fPersientent:boolean;
              fMetaType:TMetaType;
              fAttachmentData:TAttachmentData;
              fPointerToAttachmentData:PAttachmentData;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aPersientent:boolean;
                                 const aMetaType:TMetaType;
                                 const aAttachmentData:TAttachmentData); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aPersientent:boolean;
                                 const aMetaType:TMetaType); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aPersientent:boolean;
                                 const aFormat:TVkFormat;
                                 const aSamples:TVkSampleCountFlagBits;
                                 const aAttachmentType:TAttachmentType;
                                 const aAttachmentSize:TAttachmentSize;
                                 const aImageUsage:TVkImageUsageFlags;
                                 const aComponents:TVkComponentMapping); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aPersientent:boolean;
                                 const aFormat:TVkFormat;
                                 const aSamples:TVkSampleCountFlagBits;
                                 const aAttachmentType:TAttachmentType;
                                 const aAttachmentSize:TAttachmentSize;
                                 const aImageUsage:TVkImageUsageFlags); reintroduce; overload;
              destructor Destroy; override;
             public
              property AttachmentData:TAttachmentData read fAttachmentData write fAttachmentData;
              property PointerToAttachmentData:PAttachmentData read fPointerToAttachmentData write fPointerToAttachmentData;
             published
              property FrameGraph:TpvFrameGraph read fFrameGraph;
              property Name:TpvRawByteString read fName;
              property Persientent:boolean read fPersientent write fPersientent;
              property MetaType:TMetaType read fMetaType write fMetaType;
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
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); reintroduce; virtual;
              destructor Destroy; override;
              procedure Show; virtual;
              procedure Hide; virtual;
              procedure AfterCreateSwapChain; virtual;
              procedure BeforeDestroySwapChain; virtual;
            end;
            TResourcePhysicalAttachmentData=class(TResourcePhysicalData)
             private
              fResourceType:TResourceType;
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
              fVulkanImage:array[0..MaxSwapChainImages-1] of TpvVulkanImage;
              fVulkanImageView:array[0..MaxSwapChainImages-1] of TpvVulkanImageView;
              fVulkanMemoryBlock:array[0..MaxSwapChainImages-1] of TpvVulkanDeviceMemoryBlock;
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
                     AttachmentInput,
                     AttachmentOutput,
                     AttachmentResolveOutput,
                     AttachmentDepthOutput,
                     AttachmentDepthInput,
                     BufferInput,
                     BufferOutput,
                     ImageInput,
                     ImageOutput
                    );
                    PKind=^TKind;
                    TKinds=set of TKind;
                    PKinds=^TKinds;
                    TFlag=
                    (
                     PreviousFrameInput,
                     NextFrameOutput
                    );
                    PFlag=^TFlag;
                    TFlags=set of TFlag;
                    PFlags=^TFlags;
                const AllAttachments=
                       [
                        TKind.AttachmentInput,
                        TKind.AttachmentOutput,
                        TKind.AttachmentResolveOutput,
                        TKind.AttachmentDepthOutput,
                        TKind.AttachmentDepthInput
                       ];
                      AllAttachmentInputs=
                       [
                        TKind.AttachmentInput,
                        TKind.AttachmentDepthInput
                       ];
                      AllAttachmentOutputs=
                       [
                        TKind.AttachmentOutput,
                        TKind.AttachmentResolveOutput,
                        TKind.AttachmentDepthOutput
                       ];
                      AllInputs=
                       [
                        TKind.AttachmentInput,
                        TKind.AttachmentDepthInput,
                        TKind.BufferInput,
                        TKind.ImageInput
                       ];
                      AllOutputs=
                       [
                        TKind.AttachmentOutput,
                        TKind.AttachmentResolveOutput,
                        TKind.AttachmentDepthOutput,
                        TKind.BufferOutput,
                        TKind.ImageOutput
                       ];
                      AllInputsOutputs=AllInputs+AllOutputs;
             private
              fFrameGraph:TpvFrameGraph;
              fPass:TPass;
              fResource:TResource;
              fKind:TKind;
              fFlags:TFlags;
              fLayout:TVkImageLayout;
              fLoad:TLoadOp;
              fResolveResource:TResource;
              fImageSubresourceRange:TVkImageSubresourceRange;
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
                                 const aLoadOp:TLoadOp;
                                 const aImageSubresourceRange:TVkImageSubresourceRange); reintroduce; overload;
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
              property Load:TLoadOp read fLoad write fLoad;
              property ImageSubresourceRange:TVkImageSubresourceRange read fImageSubresourceRange write fImageSubresourceRange;
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
             private
              fFrameGraph:TpvFrameGraph;
              fIndex:TpvSizeInt;
              fProcessed:boolean;
              fQueue:TQueue;
              fInputDependencies:TPhysicalPasses;
              fOutputDependencies:TPhysicalPasses;
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
              function AddResource(const aResourceTypeName:TpvRawByteString;
                                   const aResourceName:TpvRawByteString;
                                   const aKind:TResourceTransition.TKind;
                                   const aFlags:TResourceTransition.TFlags;
                                   const aLayout:TVkImageLayout;
                                   const aLoadOp:TLoadOp):TResourceTransition; overload;
              function AddResource(const aResourceTypeName:TpvRawByteString;
                                   const aResourceName:TpvRawByteString;
                                   const aKind:TResourceTransition.TKind;
                                   const aFlags:TResourceTransition.TFlags;
                                   const aPipelineStage:TVkPipelineStageFlags;
                                   const aAccessFlags:TVkAccessFlags;
                                   const aBufferSubresourceRange:TBufferSubresourceRange):TResourceTransition; overload;
              function AddResource(const aResourceTypeName:TpvRawByteString;
                                   const aResourceName:TpvRawByteString;
                                   const aKind:TResourceTransition.TKind;
                                   const aFlags:TResourceTransition.TFlags;
                                   const aLayout:TVkImageLayout;
                                   const aLoadOp:TLoadOp;
                                   const aImageSubresourceRange:TVkImageSubresourceRange):TResourceTransition; overload;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); reintroduce; virtual;
              destructor Destroy; override;
              procedure AddAttachmentInput(const aResourceTypeName:TpvRawByteString;
                                           const aResourceName:TpvRawByteString;
                                           const aLayout:TVkImageLayout;
                                           const aFlags:TResourceTransition.TFlags=[]);
              procedure AddAttachmentOutput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aLoadOp:TLoadOp;
                                            const aFlags:TResourceTransition.TFlags=[]);
              procedure AddAttachmentResolveOutput(const aResourceTypeName:TpvRawByteString;
                                                   const aResourceName:TpvRawByteString;
                                                   const aResourceSourceName:TpvRawByteString;
                                                   const aLayout:TVkImageLayout;
                                                   const aLoadOp:TLoadOp;
                                                   const aFlags:TResourceTransition.TFlags=[]);
              procedure AddAttachmentDepthInput(const aResourceTypeName:TpvRawByteString;
                                                const aResourceName:TpvRawByteString;
                                                const aLayout:TVkImageLayout;
                                                const aFlags:TResourceTransition.TFlags=[]);
              procedure AddAttachmentDepthOutput(const aResourceTypeName:TpvRawByteString;
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
              procedure AddImageInput(const aResourceTypeName:TpvRawByteString;
                                      const aResourceName:TpvRawByteString;
                                      const aLayout:TVkImageLayout;
                                      const aImageSubresourceRange:TVkImageSubresourceRange;
                                      const aFlags:TResourceTransition.TFlags=[]); overload;
              procedure AddImageInput(const aResourceTypeName:TpvRawByteString;
                                      const aResourceName:TpvRawByteString;
                                      const aLayout:TVkImageLayout;
                                      const aFlags:TResourceTransition.TFlags=[]); overload;
              procedure AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aLayout:TVkImageLayout;
                                       const aLoadOp:TLoadOp;
                                       const aImageSubresourceRange:TVkImageSubresourceRange;
                                       const aFlags:TResourceTransition.TFlags=[]); overload;
              procedure AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aLayout:TVkImageLayout;
                                       const aLoadOp:TLoadOp;
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
              fAttachmentSize:TAttachmentSize;
              fPhysicalRenderPassSubPass:TPhysicalRenderPass.TSubPass;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); override;
              destructor Destroy; override;
             public
              property AttachmentSize:TAttachmentSize read fAttachmentSize write fAttachmentSize;
             published
              property MultiViewMask:TpvUInt32 read fMultiViewMask write fMultiViewMask;
            end;
      private
       fVulkanDevice:TpvVulkanDevice;
       fSurfaceWidth:TpvSizeInt;
       fSurfaceHeight:TpvSizeInt;
       fCountSwapChainImages:TpvSizeInt;
       fQueues:TQueues;
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
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fDrawFrameIndex:TpvSizeInt;
      public
       constructor Create(const aVulkanDevice:TpvVulkanDevice);
       destructor Destroy; override;
      public
       function AddQueue(const aPhysicalQueue:TpvVulkanQueue):TQueue;
       function AddResourceType(const aName:TpvRawByteString;
                                const aPersientent:boolean;
                                const aMetaType:TResourceType.TMetaType;
                                const aAttachmentData:TResourceType.TAttachmentData):TResourceType; overload;
       function AddResourceType(const aName:TpvRawByteString;
                                const aPersientent:boolean;
                                const aMetaType:TResourceType.TMetaType):TResourceType; overload;
       function AddResourceType(const aName:TpvRawByteString;
                                const aPersientent:boolean;
                                const aFormat:TVkFormat;
                                const aSamples:TVkSampleCountFlagBits;
                                const aAttachmentType:TAttachmentType;
                                const aAttachmentSize:TAttachmentSize;
                                const aImageUsage:TVkImageUsageFlags;
                                const aComponents:TVkComponentMapping):TResourceType; overload;
       function AddResourceType(const aName:TpvRawByteString;
                                const aPersientent:boolean;
                                const aFormat:TVkFormat;
                                const aSamples:TVkSampleCountFlagBits;
                                const aAttachmentType:TAttachmentType;
                                const aAttachmentSize:TAttachmentSize;
                                const aImageUsage:TVkImageUsageFlags):TResourceType; overload;
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
       property CountSwapChainImages:TpvSizeInt read fCountSwapChainImages write fCountSwapChainImages;
       property Queues:TQueues read fQueues;
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

{ TpvFrameGraph.TAttachmentTypeHelper }

function TpvFrameGraph.TAttachmentTypeHelper.GetAspectMask:TVkImageAspectFlags;
begin
 case self of
  TpvFrameGraph.TAttachmentType.Surface:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  end;
  TpvFrameGraph.TAttachmentType.Color:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  end;
  TpvFrameGraph.TAttachmentType.Depth:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT);
  end;
  TpvFrameGraph.TAttachmentType.DepthStencil:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT) or TVkImageAspectFlags(VK_IMAGE_ASPECT_STENCIL_BIT);
  end;
  TpvFrameGraph.TAttachmentType.Stencil:begin
   result:=TVkImageAspectFlags(VK_IMAGE_ASPECT_STENCIL_BIT);
  end;
  else {TpvFrameGraph.TAttachmentType.Undefined:}begin
   result:=TVkImageAspectFlags(0);
  end;
 end;
end;

{ TpvFrameGraph.TAttachmentSize }

class function TpvFrameGraph.TAttachmentSize.CreateEmpty:TAttachmentSize;
begin
 result.Kind:=TpvFrameGraph.TAttachmentSize.TKind.Undefined;
 result.Size:=TpvVector4.Null;
end;

constructor TpvFrameGraph.TAttachmentSize.Create(const aKind:TAttachmentSize.TKind;
                                                 const aWidth:TpvFloat=1.0;
                                                 const aHeight:TpvFloat=1.0;
                                                 const aDepth:TpvFloat=1.0;
                                                 const aLayers:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector4.InlineableCreate(aWidth,aHeight,aDepth,aLayers);
end;

constructor TpvFrameGraph.TAttachmentSize.Create(const aKind:TAttachmentSize.TKind;
                                                 const aSize:TpvVector2;
                                                 const aDepth:TpvFloat=1.0;
                                                 const aLayers:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector4.InlineableCreate(aSize.x,aSize.y,aDepth,aLayers);
end;

constructor TpvFrameGraph.TAttachmentSize.Create(const aKind:TAttachmentSize.TKind;
                                                 const aSize:TpvVector3;
                                                 const aLayers:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector4.InlineableCreate(aSize,aLayers);
end;

constructor TpvFrameGraph.TAttachmentSize.Create(const aKind:TAttachmentSize.TKind;
                                                 const aSize:TpvVector4);
begin
 Kind:=aKind;
 Size:=aSize;
end;

class operator TpvFrameGraph.TAttachmentSize.Equal(const aLeft,aRight:TAttachmentSize):boolean;
begin
 result:=(aLeft.Kind=aRight.Kind) and
         (aLeft.Size=aRight.Size);
end;

class operator TpvFrameGraph.TAttachmentSize.NotEqual(const aLeft,aRight:TAttachmentSize):boolean;
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
end;

destructor TpvFrameGraph.TQueue.Destroy;
begin
 FreeAndNil(fPhysicalPasses);
 inherited Destroy;
end;

{ TpvFrameGraph.TResourceType.TAttachmentData }

class function TpvFrameGraph.TResourceType.TAttachmentData.CreateEmpty:TAttachmentData;
begin
 result.Format:=VK_FORMAT_UNDEFINED;
 result.Samples:=VK_SAMPLE_COUNT_1_BIT;
 result.AttachmentType:=TAttachmentType.Undefined;
 result.AttachmentSize:=TAttachmentSize.CreateEmpty;
 result.ImageUsage:=TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
 result.Components:=TVkComponentMapping.Create(VK_COMPONENT_SWIZZLE_R,
                                               VK_COMPONENT_SWIZZLE_G,
                                               VK_COMPONENT_SWIZZLE_B,
                                               VK_COMPONENT_SWIZZLE_A);
end;

constructor TpvFrameGraph.TResourceType.TAttachmentData.Create(const aFormat:TVkFormat;
                                                               const aSamples:TVkSampleCountFlagBits;
                                                               const aAttachmentType:TAttachmentType;
                                                               const aAttachmentSize:TAttachmentSize;
                                                               const aImageUsage:TVkImageUsageFlags;
                                                               const aComponents:TVkComponentMapping);
begin
 Format:=aFormat;
 Samples:=aSamples;
 AttachmentType:=aAttachmentType;
 AttachmentSize:=aAttachmentSize;
 ImageUsage:=aImageUsage;
 Components:=aComponents;
end;

class operator TpvFrameGraph.TResourceType.TAttachmentData.Equal(const aLeft,aRight:TAttachmentData):boolean;
begin
 result:=(aLeft.Format=aRight.Format) and
         (aLeft.Samples=aRight.Samples) and
         (aLeft.AttachmentType=aRight.AttachmentType) and
         (aLeft.AttachmentSize=aRight.AttachmentSize) and
         (aLeft.ImageUsage=aRight.ImageUsage) and
         (aLeft.Components.r=aRight.Components.r) and
         (aLeft.Components.g=aRight.Components.g) and
         (aLeft.Components.b=aRight.Components.b) and
         (aLeft.Components.a=aRight.Components.a);
end;

class operator TpvFrameGraph.TResourceType.TAttachmentData.NotEqual(const aLeft,aRight:TAttachmentData):boolean;
begin
 result:=(aLeft.Format<>aRight.Format) or
         (aLeft.Samples<>aRight.Samples) or
         (aLeft.AttachmentType<>aRight.AttachmentType) or
         (aLeft.AttachmentSize<>aRight.AttachmentSize) or
         (aLeft.ImageUsage<>aRight.ImageUsage) or
         (aLeft.Components.r<>aRight.Components.r) or
         (aLeft.Components.g<>aRight.Components.g) or
         (aLeft.Components.b<>aRight.Components.b) or
         (aLeft.Components.a<>aRight.Components.a);
end;

{ TpvFrameGraph.TResourceType }

constructor TpvFrameGraph.TResourceType.Create(const aFrameGraph:TpvFrameGraph;
                                               const aName:TpvRawByteString;
                                               const aPersientent:boolean;
                                               const aMetaType:TMetaType;
                                               const aAttachmentData:TAttachmentData);
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
 fMetaType:=aMetaType;
 fAttachmentData:=aAttachmentData;
 fPointerToAttachmentData:=@fAttachmentData;
end;

constructor TpvFrameGraph.TResourceType.Create(const aFrameGraph:TpvFrameGraph;
                                               const aName:TpvRawByteString;
                                               const aPersientent:boolean;
                                               const aMetaType:TMetaType);
begin
 Create(aFrameGraph,
        aName,
        aPersientent,
        aMetaType,
        TAttachmentData.CreateEmpty);
end;

constructor TpvFrameGraph.TResourceType.Create(const aFrameGraph:TpvFrameGraph;
                                               const aName:TpvRawByteString;
                                               const aPersientent:boolean;
                                               const aFormat:TVkFormat;
                                               const aSamples:TVkSampleCountFlagBits;
                                               const aAttachmentType:TAttachmentType;
                                               const aAttachmentSize:TAttachmentSize;
                                               const aImageUsage:TVkImageUsageFlags;
                                               const aComponents:TVkComponentMapping);
begin
 Create(aFrameGraph,
        aName,
        aPersientent,
        TMetaType.Attachment,
        TAttachmentData.Create(aFormat,
                               aSamples,
                               aAttachmentType,
                               aAttachmentSize,
                               aImageUsage,
                               aComponents));
end;

constructor TpvFrameGraph.TResourceType.Create(const aFrameGraph:TpvFrameGraph;
                                               const aName:TpvRawByteString;
                                               const aPersientent:boolean;
                                               const aFormat:TVkFormat;
                                               const aSamples:TVkSampleCountFlagBits;
                                               const aAttachmentType:TAttachmentType;
                                               const aAttachmentSize:TAttachmentSize;
                                               const aImageUsage:TVkImageUsageFlags);
begin
 Create(aFrameGraph,
        aName,
        aPersientent,
        aFormat,
        aSamples,
        aAttachmentType,
        aAttachmentSize,
        aImageUsage,
        TVkComponentMapping.Create(VK_COMPONENT_SWIZZLE_R,
                                   VK_COMPONENT_SWIZZLE_G,
                                   VK_COMPONENT_SWIZZLE_B,
                                   VK_COMPONENT_SWIZZLE_A));
end;

destructor TpvFrameGraph.TResourceType.Destroy;
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

{ TpvFrameGraph.TResourcePhysicalAttachmentData }

constructor TpvFrameGraph.TResourcePhysicalAttachmentData.Create(const aFrameGraph:TpvFrameGraph);
begin
 inherited Create(aFrameGraph);
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

destructor TpvFrameGraph.TResourcePhysicalAttachmentData.Destroy;
var SwapChainImageIndex:TpvSizeInt;
begin
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanImageView[SwapChainImageIndex]);
  FreeAndNil(fVulkanImage[SwapChainImageIndex]);
  FreeAndNil(fVulkanMemoryBlock[SwapChainImageIndex]);
 end;
 inherited Destroy;
end;

procedure TpvFrameGraph.TResourcePhysicalAttachmentData.Show;
begin

end;

procedure TpvFrameGraph.TResourcePhysicalAttachmentData.Hide;
begin

end;

procedure TpvFrameGraph.TResourcePhysicalAttachmentData.AfterCreateSwapChain;
var SwapChainImageIndex:TpvSizeInt;
    MemoryRequirements:TVkMemoryRequirements;
    RequiresDedicatedAllocation,
    PrefersDedicatedAllocation:boolean;
    MemoryBlockFlags:TpvVulkanDeviceMemoryBlockFlags;
    MemoryAllocationType:TpvVulkanDeviceMemoryAllocationType;
begin

 case fResourceType.fAttachmentData.AttachmentSize.Kind of
  TpvFrameGraph.TAttachmentSize.TKind.Absolute:begin
   fExtent.width:=Max(1,trunc(fResourceType.fAttachmentData.AttachmentSize.Size.x));
   fExtent.height:=Max(1,trunc(fResourceType.fAttachmentData.AttachmentSize.Size.y));
   fExtent.depth:=Max(1,trunc(fResourceType.fAttachmentData.AttachmentSize.Size.z));
  end;
  TpvFrameGraph.TAttachmentSize.TKind.SurfaceDependent:begin
   fExtent.width:=Max(1,trunc(fResourceType.fAttachmentData.AttachmentSize.Size.x*fFrameGraph.fSurfaceWidth));
   fExtent.height:=Max(1,trunc(fResourceType.fAttachmentData.AttachmentSize.Size.y*fFrameGraph.fSurfaceHeight));
   fExtent.depth:=Max(1,trunc(fResourceType.fAttachmentData.AttachmentSize.Size.z));
  end;
  else {TpvFrameGraph.TAttachmentSize.TKind.Undefined:}begin
  end;
 end;

 for SwapChainImageIndex:=0 to Min(Max(fFrameGraph.fCountSwapChainImages,1),MaxSwapChainImages)-1 do begin

  fVulkanImage[SwapChainImageIndex]:=TpvVulkanImage.Create(fFrameGraph.fVulkanDevice,
                                                           0,
                                                           fImageType,
                                                           fFormat,
                                                           fExtent.width,
                                                           fExtent.height,
                                                           fExtent.depth,
                                                           fCountMipMaps,
                                                           fCountArrayLayers,
                                                           fSamples,
                                                           fTiling,
                                                           fImageUsageFlags or TVkImageUsageFlags(VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
                                                           fSharingMode,
                                                           0,
                                                           nil,
                                                           VK_IMAGE_LAYOUT_UNDEFINED);

  MemoryRequirements:=fFrameGraph.fVulkanDevice.MemoryManager.GetImageMemoryRequirements(fVulkanImage[SwapChainImageIndex].Handle,
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

  fVulkanMemoryBlock[SwapChainImageIndex]:=fFrameGraph.fVulkanDevice.MemoryManager.AllocateMemoryBlock(MemoryBlockFlags,
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
                                                                                                       @fVulkanImage[SwapChainImageIndex].Handle);
  if not assigned(fVulkanMemoryBlock[SwapChainImageIndex]) then begin
   raise EpvVulkanMemoryAllocationException.Create('Memory for frame buffer attachment couldn''t be allocated!');
  end;

  VulkanCheckResult(fFrameGraph.fVulkanDevice.Commands.BindImageMemory(fVulkanImage[SwapChainImageIndex].Device.Handle,
                                                                       fVulkanImage[SwapChainImageIndex].Handle,
                                                                       fVulkanMemoryBlock[SwapChainImageIndex].MemoryChunk.Handle,
                                                                       fVulkanMemoryBlock[SwapChainImageIndex].Offset));

  fVulkanImageView[SwapChainImageIndex]:=TpvVulkanImageView.Create(fFrameGraph.fVulkanDevice,
                                                                   fVulkanImage[SwapChainImageIndex],
                                                                   fImageViewType,
                                                                   fFormat,
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
    fVulkanImage[SwapChainImageIndex].SetLayout(fImageSubresourceRange.aspectMask,
                                                VK_IMAGE_LAYOUT_UNDEFINED,
                                                fFirstInitialLayout,
                                                TVkAccessFlags(0),
                                                TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or
                                                TVkAccessFlags(VK_ACCESS_INPUT_ATTACHMENT_READ_BIT),
                                                TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                                fFrameGraph.fVulkanDevice.PhysicalDevice.PipelineStageAllShaderBits,
                                                nil,
                                                fFrameGraph.fVulkanGraphicsCommandBuffer,
                                                fFrameGraph.fVulkanDevice.GraphicsQueue,
                                                fFrameGraph.fVulkanGraphicsCommandBufferFence,
                                                true);
   end else begin
    case fFirstInitialLayout of
     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      fVulkanImage[SwapChainImageIndex].SetLayout(fImageSubresourceRange.aspectMask,
                                                  VK_IMAGE_LAYOUT_UNDEFINED,
                                                  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                  TVkAccessFlags(0),
                                                  TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                  nil,
                                                  fFrameGraph.fVulkanGraphicsCommandBuffer,
                                                  fFrameGraph.fVulkanDevice.GraphicsQueue,
                                                  fFrameGraph.fVulkanGraphicsCommandBufferFence,
                                                  true);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
      fVulkanImage[SwapChainImageIndex].SetLayout(fImageSubresourceRange.aspectMask,
                                                  VK_IMAGE_LAYOUT_UNDEFINED,
                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                  TVkAccessFlags(0),
                                                  TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT),
                                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or
                                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                                  nil,
                                                  fFrameGraph.fVulkanGraphicsCommandBuffer,
                                                  fFrameGraph.fVulkanDevice.GraphicsQueue,
                                                  fFrameGraph.fVulkanGraphicsCommandBufferFence,
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

procedure TpvFrameGraph.TResourcePhysicalAttachmentData.BeforeDestroySwapChain;
var SwapChainImageIndex:TpvSizeInt;
begin
 for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanImageView[SwapChainImageIndex]);
  FreeAndNil(fVulkanImage[SwapChainImageIndex]);
  FreeAndNil(fVulkanMemoryBlock[SwapChainImageIndex]);
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
                                                     const aLoadOp:TLoadOp;
                                                     const aImageSubresourceRange:TVkImageSubresourceRange);
begin
 Create(aFrameGraph,aPass,aResource,aKind,aFlags);
 fLayout:=aLayout;
 fLoad:=aLoadOp;
 fImageSubresourceRange:=aImageSubresourceRange;
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aKind:TKind;
                                                     const aFlags:TFlags;
                                                     const aLayout:TVkImageLayout;
                                                     const aLoadOp:TLoadOp);
begin
 Create(aFrameGraph,aPass,aResource,aKind,aFlags,aLayout,aLoadOp,TVkImageSubresourceRange.Create(0,0,1,0,1));
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

 fQueue:=nil;

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

function TpvFrameGraph.TPass.AddResource(const aResourceTypeName:TpvRawByteString;
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
 if ResourceType.fMetaType<>TResourceType.TMetaType.Attachment then begin
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

function TpvFrameGraph.TPass.AddResource(const aResourceTypeName:TpvRawByteString;
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
 if ResourceType.fMetaType<>TResourceType.TMetaType.Buffer then begin
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

function TpvFrameGraph.TPass.AddResource(const aResourceTypeName:TpvRawByteString;
                                         const aResourceName:TpvRawByteString;
                                         const aKind:TResourceTransition.TKind;
                                         const aFlags:TResourceTransition.TFlags;
                                         const aLayout:TVkImageLayout;
                                         const aLoadOp:TLoadOp;
                                         const aImageSubresourceRange:TVkImageSubresourceRange):TResourceTransition;
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
 if not (ResourceType.fMetaType in [TResourceType.TMetaType.Attachment,
                                    TResourceType.TMetaType.Image]) then begin
  raise EpvFrameGraph.Create('Resource meta type mismatch');
 end;
 result:=TResourceTransition.Create(fFrameGraph,
                                    self,
                                    Resource,
                                    aKind,
                                    aFlags,
                                    aLayout,
                                    aLoadOp,
                                    aImageSubresourceRange);
 fFrameGraph.fValid:=false;
end;

procedure TpvFrameGraph.TPass.AddAttachmentInput(const aResourceTypeName:TpvRawByteString;
                                                 const aResourceName:TpvRawByteString;
                                                 const aLayout:TVkImageLayout;
                                                 const aFlags:TResourceTransition.TFlags=[]);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             TResourceTransition.TKind.AttachmentInput,
             aFlags,
             aLayout,
             TLoadOp.Create(TLoadOp.TKind.Load));
end;

procedure TpvFrameGraph.TPass.AddAttachmentOutput(const aResourceTypeName:TpvRawByteString;
                                                  const aResourceName:TpvRawByteString;
                                                  const aLayout:TVkImageLayout;
                                                  const aLoadOp:TLoadOp;
                                                  const aFlags:TResourceTransition.TFlags=[]);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             TResourceTransition.TKind.AttachmentOutput,
             aFlags,
             aLayout,
             aLoadOp);
end;

procedure TpvFrameGraph.TPass.AddAttachmentResolveOutput(const aResourceTypeName:TpvRawByteString;
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
 AddResource(aResourceTypeName,
             aResourceName,
             TResourceTransition.TKind.AttachmentResolveOutput,
             aFlags,
             aLayout,
             aLoadOp).fResolveResource:=ResourceSource;
end;

procedure TpvFrameGraph.TPass.AddAttachmentDepthInput(const aResourceTypeName:TpvRawByteString;
                                                      const aResourceName:TpvRawByteString;
                                                      const aLayout:TVkImageLayout;
                                                      const aFlags:TResourceTransition.TFlags=[]);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             TResourceTransition.TKind.AttachmentDepthInput,
             aFlags,
             aLayout,
             TLoadOp.Create(TLoadOp.TKind.Load));
end;

procedure TpvFrameGraph.TPass.AddAttachmentDepthOutput(const aResourceTypeName:TpvRawByteString;
                                                       const aResourceName:TpvRawByteString;
                                                       const aLayout:TVkImageLayout;
                                                       const aLoadOp:TLoadOp;
                                                       const aFlags:TResourceTransition.TFlags=[]);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             TResourceTransition.TKind.AttachmentDepthOutput,
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
 AddResource(aResourceTypeName,
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
 AddResource(aResourceTypeName,
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

procedure TpvFrameGraph.TPass.AddImageInput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aImageSubresourceRange:TVkImageSubresourceRange;
                                            const aFlags:TResourceTransition.TFlags=[]);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             TResourceTransition.TKind.ImageInput,
             aFlags,
             aLayout,
             TLoadOp.Create(TLoadOp.TKind.Load),
             aImageSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddImageInput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aFlags:TResourceTransition.TFlags=[]);
begin
 AddImageInput(aResourceTypeName,
               aResourceName,
               aLayout,
               TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                               0,
                                               VK_REMAINING_MIP_LEVELS,
                                               0,
                                               VK_REMAINING_ARRAY_LAYERS),
               aFlags);
end;

procedure TpvFrameGraph.TPass.AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aLayout:TVkImageLayout;
                                             const aLoadOp:TLoadOp;
                                             const aImageSubresourceRange:TVkImageSubresourceRange;
                                             const aFlags:TResourceTransition.TFlags=[]);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             TResourceTransition.TKind.ImageOutput,
             aFlags,
             aLayout,
             aLoadOp,
             aImageSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aLayout:TVkImageLayout;
                                             const aLoadOp:TLoadOp;
                                             const aFlags:TResourceTransition.TFlags=[]);
begin
 AddImageOutput(aResourceTypeName,
                aResourceName,
                aLayout,
                aLoadOp,
                TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                0,
                                                VK_REMAINING_MIP_LEVELS,
                                                0,
                                                VK_REMAINING_ARRAY_LAYERS),
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

{ TpvFrameGraph.TPhysicalPass }

constructor TpvFrameGraph.TPhysicalPass.Create(const aFrameGraph:TpvFrameGraph;const aQueue:TQueue);
begin

 inherited Create;

 fFrameGraph:=aFrameGraph;

 fQueue:=aQueue;

 fInputDependencies:=TPhysicalPasses.Create;
 fInputDependencies.OwnsObjects:=false;

 fOutputDependencies:=TPhysicalPasses.Create;
 fOutputDependencies.OwnsObjects:=false;

end;

destructor TpvFrameGraph.TPhysicalPass.Destroy;
begin
 FreeAndNil(fInputDependencies);
 FreeAndNil(fOutputDependencies);
 inherited Destroy;
end;

procedure TpvFrameGraph.TPhysicalPass.Show;
begin

end;

procedure TpvFrameGraph.TPhysicalPass.Hide;
begin

end;

procedure TpvFrameGraph.TPhysicalPass.AfterCreateSwapChain;
begin

end;

procedure TpvFrameGraph.TPhysicalPass.BeforeDestroySwapChain;
begin

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
end;

procedure TpvFrameGraph.TPhysicalComputePass.Hide;
begin
end;

procedure TpvFrameGraph.TPhysicalComputePass.AfterCreateSwapChain;
begin
end;

procedure TpvFrameGraph.TPhysicalComputePass.BeforeDestroySwapChain;
begin
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
end;

procedure TpvFrameGraph.TPhysicalRenderPass.AfterCreateSwapChain;
type TInt32AttachmentLists=TpvDynamicArray<TpvInt32>;
     TUInt32AttachmentLists=TpvDynamicArray<TpvUInt32>;
var SubPassDependencyIndex:TpvSizeInt;
    SubPass,
    OtherSubPass:TSubPass;
    RenderPass:TRenderPass;
    ResourceTransition,
    FromResourceTransition,
    ToResourceTransition:TResourceTransition;
    InputAttachments,
    ColorAttachments,
    ResolveAttachments:TInt32AttachmentLists;
    PreserveAttachments:TUInt32AttachmentLists;
    DepthStencilAttachment:TpvInt32;
    SrcSubPassIndex,
    DstSubPassIndex:TVkUInt32;
    SrcStageMask,
    DstStageMask:TVkPipelineStageFlags;
    SubPassDependency:PSubPassDependency;
begin

 for SubPass in fSubPasses do begin
  SubPass.AfterCreateSwapChain;
 end;

{fVulkanRenderPass:=TpvVulkanRenderPass.Create(fFrameGraph.fVulkanDevice);
 fVulkanRenderPass.}

 InputAttachments.Initialize;
 ColorAttachments.Initialize;
 ResolveAttachments.Initialize;
 PreserveAttachments.Initialize;
 try

  DepthStencilAttachment:=-1;

  for SubPass in fSubPasses do begin
   RenderPass:=SubPass.fRenderPass;
   for ResourceTransition in RenderPass.fResourceTransitions do begin
    case ResourceTransition.Kind of
     TpvFrameGraph.TResourceTransition.TKind.AttachmentInput,
     TpvFrameGraph.TResourceTransition.TKind.AttachmentOutput,
     TpvFrameGraph.TResourceTransition.TKind.AttachmentResolveOutput,
     TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthOutput,
     TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthInput:begin
     // ResourceTransition.
     end;
    end;
   end;
  end;

  InputAttachments.Finish;
  ColorAttachments.Finish;
  ResolveAttachments.Finish;
  PreserveAttachments.Finish;

  fVulkanRenderPass:=nil;

  if assigned(fVulkanRenderPass) then begin

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

  end;

 finally
  InputAttachments.Finalize;
  ColorAttachments.Finalize;
  ResolveAttachments.Finalize;
  PreserveAttachments.Finalize;
 end;

end;

procedure TpvFrameGraph.TPhysicalRenderPass.BeforeDestroySwapChain;
var SubPass:TSubPass;
begin

 FreeAndNil(fVulkanRenderPass);

 for SubPass in fSubPasses do begin
  SubPass.BeforeDestroySwapChain;
 end;
end;

{ TpvFrameGraph }

constructor TpvFrameGraph.Create(const aVulkanDevice:TpvVulkanDevice);
begin

 inherited Create;

 fVulkanDevice:=aVulkanDevice;

 fSurfaceWidth:=1;
 fSurfaceHeight:=1;

 fCountSwapChainImages:=MaxSwapChainImages;

 fCanDoParallelProcessing:=false;

 fQueues:=TQueues.Create;
 fQueues.OwnsObjects:=true;

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

 fVulkanGraphicsCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                                         fVulkanDevice.GraphicsQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanGraphicsCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(fVulkanDevice);

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

 FreeAndNil(fQueues);

 FreeAndNil(fVulkanGraphicsCommandBufferFence);

 FreeAndNil(fVulkanGraphicsCommandBuffer);

 FreeAndNil(fVulkanGraphicsCommandPool);

 inherited Destroy;

end;

function TpvFrameGraph.AddQueue(const aPhysicalQueue:TpvVulkanQueue):TQueue;
var Queue:TQueue;
begin
 result:=nil;
 for Queue in fQueues do begin
  if Queue.fPhysicalQueue=aPhysicalQueue then begin
   result:=Queue;
   break;
  end;
 end;
 if not assigned(result) then begin
  result:=TQueue.Create(self,aPhysicalQueue);
  fQueues.Add(result);
 end;
end;

function TpvFrameGraph.AddResourceType(const aName:TpvRawByteString;
                                       const aPersientent:boolean;
                                       const aMetaType:TResourceType.TMetaType;
                                       const aAttachmentData:TResourceType.TAttachmentData):TResourceType;
begin
 result:=TResourceType.Create(self,aName,aPersientent,aMetaType,aAttachmentData);
end;

function TpvFrameGraph.AddResourceType(const aName:TpvRawByteString;
                                       const aPersientent:boolean;
                                       const aMetaType:TResourceType.TMetaType):TResourceType;
begin
 result:=TResourceType.Create(self,aName,aPersientent,aMetaType);
end;

function TpvFrameGraph.AddResourceType(const aName:TpvRawByteString;
                                       const aPersientent:boolean;
                                       const aFormat:TVkFormat;
                                       const aSamples:TVkSampleCountFlagBits;
                                       const aAttachmentType:TAttachmentType;
                                       const aAttachmentSize:TAttachmentSize;
                                       const aImageUsage:TVkImageUsageFlags;
                                       const aComponents:TVkComponentMapping):TResourceType;
begin
 result:=TResourceType.Create(self,aName,aPersientent,aFormat,aSamples,aAttachmentType,aAttachmentSize,aImageUsage,aComponents);
end;

function TpvFrameGraph.AddResourceType(const aName:TpvRawByteString;
                                       const aPersientent:boolean;
                                       const aFormat:TVkFormat;
                                       const aSamples:TVkSampleCountFlagBits;
                                       const aAttachmentType:TAttachmentType;
                                       const aAttachmentSize:TAttachmentSize;
                                       const aImageUsage:TVkImageUsageFlags):TResourceType;
begin
 result:=TResourceType.Create(self,aName,aPersientent,aFormat,aSamples,aAttachmentType,aAttachmentSize,aImageUsage);
end;

procedure TpvFrameGraph.Setup;
begin

end;

procedure TpvFrameGraph.Compile;
 function GetPipelineStageMask(const aResourceTransition:TResourceTransition):TVkPipelineStageFlags;
 begin
  case aResourceTransition.fKind of
   TpvFrameGraph.TResourceTransition.TKind.AttachmentInput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentOutput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentResolveOutput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthInput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthOutput:begin
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
   TpvFrameGraph.TResourceTransition.TKind.AttachmentOutput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentResolveOutput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthOutput,
   TpvFrameGraph.TResourceTransition.TKind.BufferOutput:begin
    aSrcStageMask:=GetPipelineStageMask(aFromResourceTransition);
   end;
   else begin
    aSrcStageMask:=0;
   end;
  end;
  case aToResourceTransition.fKind of
   TpvFrameGraph.TResourceTransition.TKind.AttachmentInput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthInput,
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
   TpvFrameGraph.TResourceTransition.TKind.AttachmentOutput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentResolveOutput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthOutput:begin
    case aFromResourceTransition.fLayout of
     VK_IMAGE_LAYOUT_GENERAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
     VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL:begin
      aSrcAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT) or
                      TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT);
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
   TpvFrameGraph.TResourceTransition.TKind.AttachmentInput,
   TpvFrameGraph.TResourceTransition.TKind.AttachmentDepthInput:begin
    case aToResourceTransition.fLayout of
     VK_IMAGE_LAYOUT_GENERAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT);
     end;
     VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
     VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL:begin
      aDstAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT) or
                      TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT);
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
 procedure ValidateAttachments;
 var Pass:TPass;
     RenderPass:TRenderPass;
     ResourceTransition:TResourceTransition;
 begin
  // Validate that all attachments have the same size as defined in the render pass and that alll passes have a assigned queue
  for Pass in fPasses do begin
   if not assigned(Pass.fQueue) then begin
    raise EpvFrameGraphMissingQueue.Create('Pass "'+String(Pass.fName)+'" is without assigned queue');
   end;
   if Pass is TRenderPass then begin
    RenderPass:=Pass as TRenderPass;
    for ResourceTransition in RenderPass.fResourceTransitions do begin
     if (ResourceTransition.fKind in TResourceTransition.AllAttachments) and
        (ResourceTransition.fResource.fResourceType.fAttachmentData.AttachmentSize<>RenderPass.fAttachmentSize) then begin
      raise EpvFrameGraphMismatchAttachmentSize.Create('Mismatch attachment size between pass "'+String(Pass.fName)+'" and resource "'+String(ResourceTransition.fResource.fName)+'"');
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
  // Find root pass (a render pass, which have only a single attachment output to a surface/swapchain)
  fRootPass:=fEnforcedRootPass;
  if not assigned(fRootPass) then begin
   for Pass in fPasses do begin
    if Pass is TRenderPass then begin
     RenderPass:=Pass as TRenderPass;
     Temporary:=0;
     for ResourceTransition in RenderPass.fResourceTransitions do begin
      if ResourceTransition.fKind in TResourceTransition.AllAttachmentOutputs then begin
       Resource:=ResourceTransition.fResource;
       if (Resource.fResourceType.fMetaType=TResourceType.TMetaType.Attachment) and
          (Resource.fResourceType.fAttachmentData.AttachmentType=TAttachmentType.Surface) then begin
        Temporary:=Temporary or 1;
       end else if not ((Resource.fResourceType.fMetaType=TResourceType.TMetaType.Attachment) and
                        (Resource.fResourceType.fAttachmentData.AttachmentType=TAttachmentType.Depth)) then begin
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
           Weight:=(ord(TRenderPass(Passes[0]).fAttachmentSize=TRenderPass(Pass).fAttachmentSize) and 1)-
                   (ord(TRenderPass(Passes[1]).fAttachmentSize=TRenderPass(Pass).fAttachmentSize) and 1);
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
         (TRenderPass(OtherPass).fAttachmentSize=TRenderPass(Pass).fAttachmentSize) then begin
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
        ((ResourceTransition.fResource.fResourceType.fMetaType=TResourceType.TMetaType.Attachment) and
         (ResourceTransition.fResource.fResourceType.fAttachmentData.AttachmentType=TAttachmentType.Surface)) then begin
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
   result:=(aResource.fResourceType.fAttachmentData.AttachmentType<>TAttachmentType.Surface) and
           (not ((aResource.fResourceType.fMetaType<>TResourceType.TMetaType.Attachment) and
                 assigned(aResource.fAssociatedMemoryData)));
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
     ResourcePhysicalAttachmentData:TResourcePhysicalAttachmentData;
     Resource:TResource;
     ResourceTransition:TResourceTransition;
 begin
  // Create data for the resource reuse groups
  for ResourceReuseGroup in fResourceReuseGroups do begin
   ResourceType:=ResourceReuseGroup.fResourceType;
   case ResourceType.fMetaType of
    TpvFrameGraph.TResourceType.TMetaType.Attachment:begin
     if not assigned(ResourceReuseGroup.fResourcePhysicalData) then begin
      ResourceReuseGroup.fResourcePhysicalData:=TResourcePhysicalAttachmentData.Create(self);
      ResourcePhysicalAttachmentData:=TResourcePhysicalAttachmentData(ResourceReuseGroup.fResourcePhysicalData);
      ResourcePhysicalAttachmentData.fResourceType:=ResourceType;
      ResourcePhysicalAttachmentData.fImageUsageFlags:=TVkImageUsageFlags(ResourceType.AttachmentData.ImageUsage);
      ResourcePhysicalAttachmentData.fFormat:=ResourceType.AttachmentData.Format;
      ResourcePhysicalAttachmentData.fExtent.width:=Max(1,trunc(ResourceType.AttachmentData.AttachmentSize.Size.x));
      ResourcePhysicalAttachmentData.fExtent.height:=Max(1,trunc(ResourceType.AttachmentData.AttachmentSize.Size.y));
      ResourcePhysicalAttachmentData.fExtent.depth:=Max(1,trunc(ResourceType.AttachmentData.AttachmentSize.Size.z));
      ResourcePhysicalAttachmentData.fCountMipMaps:=1;
      ResourcePhysicalAttachmentData.fCountArrayLayers:=trunc(ResourceType.AttachmentData.AttachmentSize.Size.w);
      ResourcePhysicalAttachmentData.fSamples:=ResourceType.AttachmentData.Samples;
      ResourcePhysicalAttachmentData.fTiling:=VK_IMAGE_TILING_OPTIMAL;
      ResourcePhysicalAttachmentData.fInitialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
      ResourcePhysicalAttachmentData.fFirstInitialLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
      MinimumTopologicalSortIndex:=High(TpvSizeInt);
      for Resource in ResourceReuseGroup.fResources do begin
       for ResourceTransition in Resource.fResourceTransitions do begin
        if ResourceTransition.fPass.fTopologicalSortIndex<MinimumTopologicalSortIndex then begin
         MinimumTopologicalSortIndex:=ResourceTransition.fPass.fTopologicalSortIndex;
         ResourcePhysicalAttachmentData.fFirstInitialLayout:=ResourceTransition.fLayout;
        end;
       end;
      end;
      ResourcePhysicalAttachmentData.fImageCreateFlags:=0;
      if ResourcePhysicalAttachmentData.fExtent.depth>1 then begin
       ResourcePhysicalAttachmentData.fImageType:=VK_IMAGE_TYPE_3D;
      end else begin
       ResourcePhysicalAttachmentData.fImageType:=VK_IMAGE_TYPE_2D;
      end;
      ResourcePhysicalAttachmentData.fSharingMode:=VK_SHARING_MODE_EXCLUSIVE;
      ResourcePhysicalAttachmentData.fImageSubresourceRange.aspectMask:=ResourceType.AttachmentData.AttachmentType.GetAspectMask;
      ResourcePhysicalAttachmentData.fImageSubresourceRange.baseMipLevel:=0;
      ResourcePhysicalAttachmentData.fImageSubresourceRange.levelCount:=1;
      ResourcePhysicalAttachmentData.fImageSubresourceRange.baseArrayLayer:=0;
      ResourcePhysicalAttachmentData.fImageSubresourceRange.layerCount:=ResourcePhysicalAttachmentData.fCountArrayLayers;
      if ResourcePhysicalAttachmentData.fExtent.depth>1 then begin
       if ResourcePhysicalAttachmentData.fImageSubresourceRange.layerCount>1 then begin
        raise EpvFrameGraph.Create('3D array image not supported');
       end else begin
        ResourcePhysicalAttachmentData.fImageViewType:=VK_IMAGE_VIEW_TYPE_3D;
       end;
      end else begin
       if ResourcePhysicalAttachmentData.fImageSubresourceRange.layerCount>1 then begin
        ResourcePhysicalAttachmentData.fImageViewType:=VK_IMAGE_VIEW_TYPE_2D_ARRAY;
       end else begin
        ResourcePhysicalAttachmentData.fImageViewType:=VK_IMAGE_VIEW_TYPE_2D;
       end;
      end;
      ResourcePhysicalAttachmentData.fComponents:=ResourceType.fAttachmentData.Components;
     end;
    end;
    TpvFrameGraph.TResourceType.TMetaType.Image:begin
    end;
    TpvFrameGraph.TResourceType.TMetaType.Buffer:begin
    end;
    else {TpvFrameGraph.TResourceType.TMetaType.None:}begin
 //  raise EpvFrameGraph.Create('Invalid meta type');
    end;
   end;
  end;
 end;
 procedure CreatePhysicalRenderPassSubPassDependencies;
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
 var ResourceTransitionIndex,
     OtherResourceTransitionIndex:TpvSizeInt;
     Resource:TResource;
     ResourceTransition,
     OtherResourceTransition:TResourceTransition;
     SubPassDependency:TPhysicalRenderPass.TSubPassDependency;
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
           (OtherResourceTransition.fPass.fPhysicalPass is TPhysicalRenderPass) then begin
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

 IndexingPasses;

 ValidateAttachments;

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

 CreatePhysicalRenderPassSubPassDependencies;

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
var ResourceReuseGroup:TResourceReuseGroup;
    PhysicalPass:TPhysicalPass;
begin
 for ResourceReuseGroup in fResourceReuseGroups do begin
  ResourceReuseGroup.AfterCreateSwapChain;
 end;
 for PhysicalPass in fPhysicalPasses do begin
  PhysicalPass.AfterCreateSwapChain;
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
