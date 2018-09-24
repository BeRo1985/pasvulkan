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
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework;

// Inspired from:
//   https://www.ea.com/frostbite/news/framegraph-extensible-rendering-architecture-in-frostbite
//   https://www.gdcvault.com/play/1024612/FrameGraph-Extensible-Rendering-Architecture-in
//   https://www.slideshare.net/DICEStudio/framegraph-extensible-rendering-architecture-in-frostbite
//   http://themaister.net/blog/2017/08/15/render-graphs-and-vulkan-a-deep-dive/

// Attention: It is still work in progress

type EpvFrameGraph=class(Exception);

     EpvFrameGraphEmptyName=class(EpvFrameGraph);

     EpvFrameGraphDuplicateName=class(EpvFrameGraph);

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
              Size:TpvVector3;
              class function CreateEmpty:TAttachmentSize; static;
              constructor Create(const aKind:TAttachmentSize.TKind;
                                 const aWidth:tpvFloat=1.0;
                                 const aHeight:TpvFloat=1.0;
                                 const aDepth:TpvFloat=1.0); overload;
              constructor Create(const aKind:TAttachmentSize.TKind;
                                 const aSize:TpvVector2;
                                 const aDepth:TpvFloat=1.0); overload;
              constructor Create(const aKind:TAttachmentSize.TKind;
                                 const aSize:TpvVector3); overload;
              class operator Equal(const aLeft,aRight:TAttachmentSize):boolean;
              class operator NotEqual(const aLeft,aRight:TAttachmentSize):boolean;
            end;
            PAttachmentSize=^TAttachmentSize;
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
            TResourceReuseGroup=class
             private
              fFrameGraph:TpvFrameGraph;
              fResourceType:TResourceType;
              fResources:TResourceList;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); reintroduce;
              destructor Destroy; override;
            end;
            TResourceReuseGroupList=TpvObjectGenericList<TResourceReuseGroup>;
            TResource=class
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fResourceType:TResourceType;
              fResourceTransitions:TResourceTransitionList;
              fMinimumPassStepIndex:TpvSizeInt;
              fMaximumPassStepIndex:TpvSizeInt;
              fResourceReuseGroup:TResourceReuseGroup;
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
              property MinimumPassStepIndex:TpvSizeInt read fMinimumPassStepIndex;
              property MaximumPassStepIndex:TpvSizeInt read fMaximumPassStepIndex;
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
            TPass=class
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fResources:TResourceList;
              fResourceTransitions:TResourceTransitionList;
              fPreviousPasses:TPassList;
              fNextPasses:TPassList;
              fTag:TpvSizeInt;
              fMinimumStepIndex:TpvSizeInt;
              fMaximumStepIndex:TpvSizeInt;
              fEnabled:boolean;
              fUsed:boolean;
              fProcessed:boolean;
              fMarked:boolean;
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
              property Enabled:boolean read fEnabled write fEnabled;
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
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); override;
              destructor Destroy; override;
             public
              property AttachmentSize:TAttachmentSize read fAttachmentSize write fAttachmentSize;
             published
              property MultiViewMask:TpvUInt32 read fMultiViewMask write fMultiViewMask;
            end;
      private
       fResourceTypes:TResourceTypeList;
       fResourceTypeNameHashMap:TResourceTypeNameHashMap;
       fResources:TResourceList;
       fResourceNameHashMap:TResourceNameHashMap;
       fResourceTransitions:TResourceTransitionList;
       fResourceReuseGroups:TResourceReuseGroupList;
       fPasses:TPassList;
       fPassNameHashMap:TPassNameHashMap;
       fRootPass:TPass;
       fEnforcedRootPass:TPass;
       fMaximumOverallPassStepIndex:TpvSizeInt;
       fValid:boolean;
      public
       constructor Create;
       destructor Destroy; override;
      public
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
       procedure AfterCreateSwapChain; virtual;
       procedure BeforeDestroySwapChain; virtual;
       procedure Execute; virtual;
      published
       property ResourceTypes:TResourceTypeList read fResourceTypes;
       property ResourceTypeByName:TResourceTypeNameHashMap read fResourceTypeNameHashMap;
       property Resources:TResourceList read fResources;
       property ResourceByName:TResourceNameHashMap read fResourceNameHashMap;
       property Passes:TPassList read fPasses;
       property PassByName:TPassNameHashMap read fPassNameHashMap;
       property RootPass:TPass read fRootPass;
       property EnforcedRootPass:TPass read fEnforcedRootPass write fEnforcedRootPass;
     end;

implementation

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

{ TpvFrameGraph.TAttachmentSize }

class function TpvFrameGraph.TAttachmentSize.CreateEmpty:TAttachmentSize;
begin
 result.Kind:=TpvFrameGraph.TAttachmentSize.TKind.Undefined;
 result.Size:=TpvVector3.Null;
end;

constructor TpvFrameGraph.TAttachmentSize.Create(const aKind:TAttachmentSize.TKind;
                                                 const aWidth:TpvFloat=1.0;
                                                 const aHeight:TpvFloat=1.0;
                                                 const aDepth:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector3.InlineableCreate(aWidth,aHeight,aDepth);
end;

constructor TpvFrameGraph.TAttachmentSize.Create(const aKind:TAttachmentSize.TKind;
                                                 const aSize:TpvVector2;
                                                 const aDepth:TpvFloat=1.0);
begin
 Kind:=aKind;
 Size:=TpvVector3.InlineableCreate(aSize.x,aSize.y,aDepth);
end;

constructor TpvFrameGraph.TAttachmentSize.Create(const aKind:TAttachmentSize.TKind;
                                                 const aSize:TpvVector3);
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

{ TpvFrameGraph.TResourceReuseGroup }

constructor TpvFrameGraph.TResourceReuseGroup.Create(const aFrameGraph:TpvFrameGraph);
begin
 inherited Create;
 fFrameGraph:=aFrameGraph;
 fFrameGraph.fResourceReuseGroups.Add(self);
 fResourceType:=nil;
 fResources:=TResourceList.Create;
 fResources.OwnsObjects:=false;
end;

destructor TpvFrameGraph.TResourceReuseGroup.Destroy;
begin
 FreeAndNil(fResources);
 inherited Destroy;
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

 fMinimumPassStepIndex:=High(TpvSizeInt);
 fMaximumPassStepIndex:=Low(TpvSizeInt);

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

 fResources:=TResourceList.Create;
 fResources.OwnsObjects:=false;

 fResourceTransitions:=TResourceTransitionList.Create;
 fResourceTransitions.OwnsObjects:=false;

 fPreviousPasses:=TPassList.Create;
 fPreviousPasses.OwnsObjects:=false;

 fNextPasses:=TPassList.Create;
 fNextPasses.OwnsObjects:=false;

 fEnabled:=true;

end;

destructor TpvFrameGraph.TPass.Destroy;
begin

 FreeAndNil(fResources);

 FreeAndNil(fResourceTransitions);

 FreeAndNil(fPreviousPasses);

 FreeAndNil(fNextPasses);

 inherited Destroy;

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
end;

destructor TpvFrameGraph.TRenderPass.Destroy;
begin
 inherited Destroy;
end;

{ TpvFrameGraph }

constructor TpvFrameGraph.Create;
begin

 inherited Create;

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

end;

destructor TpvFrameGraph.Destroy;
begin

 FreeAndNil(fResourceTypes);

 FreeAndNil(fResourceTypeNameHashMap);

 FreeAndNil(fResources);

 FreeAndNil(fResourceNameHashMap);

 FreeAndNil(fResourceTransitions);

 FreeAndNil(fResourceReuseGroups);

 FreeAndNil(fPasses);

 FreeAndNil(fPassNameHashMap);

 inherited Destroy;

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
type TAction=
      (
       Process,
       Unmark
      );
     TStackItem=record
      Action:TAction;
      Pass:TPass;
      StepIndex:TpvSizeInt;
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicStack<TStackItem>;
     TResourceDynamicArray=TpvDynamicArray<TResource>;
     TAttachmentSizeTagHashMap=TpvHashMap<TAttachmentSize,TpvSizeInt>;
 function NewStackItem(const aAction:TAction;const aPass:TPass;const aStep:TpvSizeInt):TStackItem;
 begin
  result.Action:=aAction;
  result.Pass:=aPass;
  result.StepIndex:=aStep;
 end;
var Temporary,
    Index,
    OtherIndex,
    BaseStackCount,
    TagCounter,
    FoundTag,
    MaximumOverallPassStepIndex:TpvSizeInt;
    Pass:TPass;
    RenderPass:TRenderPass;
    ResourceTransition,
    OtherResourceTransition:TResourceTransition;
    Resource,
    OtherResource:TResource;
    Stack:TStack;
    StackItem:TStackItem;
    OK:boolean;
    ResourceDynamicArray:TResourceDynamicArray;
    AttachmentSizeTagHashMap:TAttachmentSizeTagHashMap;
    ResourceReuseGroup:TResourceReuseGroup;
    ResourceType:TResourceType;
begin

 // Validate that all attachments have the same size as defined in the render pass
 for Pass in fPasses do begin
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

 // Construct the directed acyclic graph
 Stack.Initialize;
 try
  MaximumOverallPassStepIndex:=0;
  for Pass in fPasses do begin
   Pass.fMinimumStepIndex:=-1;
   Pass.fMaximumStepIndex:=-1;
   Pass.fUsed:=false;
   Pass.fProcessed:=false;
   Pass.fMarked:=false;
   Pass.fPreviousPasses.Clear;
   Pass.fNextPasses.Clear;
  end;
  Stack.Push(NewStackItem(TAction.Process,fRootPass,0));
  while Stack.Pop(StackItem) do begin
   Pass:=StackItem.Pass;
   case StackItem.Action of
    TAction.Process:begin
     if Pass.fMarked then begin
      raise EpvFrameGraphRecursion.Create('Recursion detected');
     end;
     if Pass.fProcessed then begin
      Pass.fMinimumStepIndex:=Min(Pass.fMinimumStepIndex,StackItem.StepIndex);
      Pass.fMaximumStepIndex:=Max(Pass.fMaximumStepIndex,StackItem.StepIndex);
     end else begin
      Pass.fMarked:=true;
      Pass.fUsed:=true;
      Pass.fMinimumStepIndex:=StackItem.StepIndex;
      Pass.fMaximumStepIndex:=StackItem.StepIndex;
     end;
     MaximumOverallPassStepIndex:=Max(MaximumOverallPassStepIndex,StackItem.StepIndex);
     Stack.Push(NewStackItem(TAction.Unmark,Pass,StackItem.StepIndex));
     BaseStackCount:=Stack.Count;
     for ResourceTransition in Pass.fResourceTransitions do begin
      if (ResourceTransition.fKind in TResourceTransition.AllInputs) and
         not (TResourceTransition.TFlag.PreviousFrameInput in ResourceTransition.Flags) then begin
       Resource:=ResourceTransition.Resource;
       for OtherResourceTransition in Resource.fResourceTransitions do begin
        if (ResourceTransition<>OtherResourceTransition) and
           (Pass<>OtherResourceTransition.fPass) and
           (OtherResourceTransition.fKind in TResourceTransition.AllOutputs) then begin
         if not Pass.fProcessed then begin
          if Pass.fPreviousPasses.IndexOf(OtherResourceTransition.fPass)<0 then begin
           Pass.fPreviousPasses.Add(OtherResourceTransition.fPass);
          end;
          if OtherResourceTransition.fPass.fNextPasses.IndexOf(Pass)<0 then begin
           OtherResourceTransition.fPass.fNextPasses.Add(Pass);
          end;
         end;
         OK:=true;
         for Index:=Stack.Count-1 downto BaseStackCount do begin
          if Stack.Items[Index].Pass=OtherResourceTransition.fPass then begin
           OK:=false;
           break;
          end;
         end;
         if OK then begin
          Stack.Push(NewStackItem(TAction.Process,OtherResourceTransition.fPass,StackItem.StepIndex+1));
         end;
        end;
       end;
      end;
      Pass.fProcessed:=true;
     end;
    end;
    TAction.Unmark:begin
     Pass.fMarked:=false;
    end;
   end;
  end;
  fMaximumOverallPassStepIndex:=MaximumOverallPassStepIndex;
 finally
  Stack.Finalize;
 end;

 // Try to tag passes with same atachment sizes (for example for subpass grouping), but where
 // compute passes gets always their own tags
 AttachmentSizeTagHashMap:=TAttachmentSizeTagHashMap.Create(-1);
 try
  TagCounter:=0;
  for Pass in fPasses do begin
   if Pass is TComputePass then begin
    Pass.fTag:=TagCounter;
    inc(TagCounter);
   end else if Pass is TRenderPass then begin
    if not AttachmentSizeTagHashMap.TryGet(TRenderPass(Pass).fAttachmentSize,FoundTag) then begin
     FoundTag:=TagCounter;
     inc(TagCounter);
     AttachmentSizeTagHashMap.Add(TRenderPass(Pass).fAttachmentSize,FoundTag);
    end;
    Pass.fTag:=FoundTag;
   end;
  end;
 finally
  FreeAndNil(AttachmentSizeTagHashMap);
 end;

 // Calculate resource lifetimes (from minimum pass step index to maximum pass step index) for
 // calculating aliasing and reusing of resources at a later point
 for Resource in fResources do begin
  Resource.fMinimumPassStepIndex:=High(TpvSizeInt);
  Resource.fMaximumPassStepIndex:=Low(TpvSizeInt);
  for ResourceTransition in Resource.fResourceTransitions do begin
   Pass:=ResourceTransition.fPass;
   if Pass.fMinimumStepIndex>=0 then begin
    if ((ResourceTransition.fFlags*[TResourceTransition.TFlag.PreviousFrameInput,
                                    TResourceTransition.TFlag.NextFrameOutput])<>[]) or
       ((ResourceTransition.fResource.fResourceType.fMetaType=TResourceType.TMetaType.Attachment) and
        (ResourceTransition.fResource.fResourceType.fAttachmentData.AttachmentType=TAttachmentType.Surface)) then begin
     // In this cases, this one resource must life from the begin to the end of the whole
     // directed acyclic graph for the simplicity of safety, because it can be still optimized
     // in a better way later
     if not Resource.fUsed then begin
      Resource.fUsed:=true;
      Resource.fMinimumPassStepIndex:=0;
      Resource.fMaximumPassStepIndex:=MaximumOverallPassStepIndex;
     end;
    end else begin
     if Resource.fUsed then begin
      Resource.fMinimumPassStepIndex:=Min(Resource.fMinimumPassStepIndex,Pass.fMinimumStepIndex);
      Resource.fMaximumPassStepIndex:=Max(Resource.fMaximumPassStepIndex,Pass.fMaximumStepIndex);
     end else begin
      Resource.fUsed:=true;
      Resource.fMinimumPassStepIndex:=Pass.fMinimumStepIndex;
      Resource.fMaximumPassStepIndex:=Pass.fMaximumStepIndex;
     end;
    end;
   end;
  end;
 end;

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
   Resource.fResourceReuseGroup.fResources.Add(Resource);
   for OtherIndex:=Index+1 to fResources.Count-1 do begin
    OtherResource:=fResources.Items[OtherIndex];
    if (not assigned(OtherResource.fResourceReuseGroup)) and
       (Resource.fResourceType=OtherResource.fResourceType) and
       (Min(Resource.MaximumPassStepIndex,
            OtherResource.MaximumPassStepIndex)>Max(Resource.MinimumPassStepIndex,
                                                    OtherResource.MinimumPassStepIndex)) then begin
     OtherResource.fResourceReuseGroup:=Resource.fResourceReuseGroup;
     OtherResource.fResourceReuseGroup.fResources.Add(OtherResource);
    end;
   end;
  end;
 end;

 // Create meta data for Vulkan images, image views, frame buffers, render passes and so on, for
 // processing this data in AfterCreateSwapChain and BeforeDestroySwapChain
 for ResourceReuseGroup in fResourceReuseGroups do begin
  ResourceType:=ResourceReuseGroup.fResourceType;
  case ResourceType.fMetaType of
   TpvFrameGraph.TResourceType.TMetaType.Attachment:begin
    case ResourceType.fAttachmentData.AttachmentType of
     TpvFrameGraph.TAttachmentType.Surface:begin
     end;
     TpvFrameGraph.TAttachmentType.Color,
     TpvFrameGraph.TAttachmentType.Depth,
     TpvFrameGraph.TAttachmentType.DepthStencil,
     TpvFrameGraph.TAttachmentType.Stencil:begin
     end;
     else {TpvFrameGraph.TAttachmentType.Undefined:}begin
     end;
    end;
   end;
   TpvFrameGraph.TResourceType.TMetaType.Image,
   TpvFrameGraph.TResourceType.TMetaType.Buffer:begin
   end;
   else {TpvFrameGraph.TResourceType.TMetaType.None:}begin
   end;
  end;
 end;
 for Pass in fPasses do begin
  if Pass.fUsed then begin
   if Pass is TComputePass then begin
   end else if Pass is TRenderPass then begin
   end;
  end;
 end;

end;

procedure TpvFrameGraph.AfterCreateSwapChain;
begin

end;

procedure TpvFrameGraph.BeforeDestroySwapChain;
begin

end;

procedure TpvFrameGraph.Execute;
begin

end;

end.
