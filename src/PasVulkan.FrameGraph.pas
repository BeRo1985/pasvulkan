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
            TAttachmentSize=record
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
            TResourceTypeList=TpvObjectGenericList<TResourceType>;
            TResourceTypeNameHashMap=TpvStringHashMap<TResourceType>;
            TResourceTransition=class;
            TResourceTransitionList=TpvObjectGenericList<TResourceTransition>;
            TResource=class
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fResourceType:TResourceType;
              fResourceTransitions:TResourceTransitionList;
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
            end;
            TResourceList=TpvObjectGenericList<TResource>;
            TResourceNameHashMap=TpvStringHashMap<TResource>;
            TPass=class;
            TResourceTransition=class
             public
              type TFlag=
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
                    PFlag=^TFlag;
                    TFlags=set of TFlag;
                    PFlags=^TFlags;
                const AllAttachments=
                       [
                        TFlag.AttachmentInput,
                        TFlag.AttachmentOutput,
                        TFlag.AttachmentResolveOutput,
                        TFlag.AttachmentDepthOutput,
                        TFlag.AttachmentDepthInput
                       ];
                      AllAttachmentInputs=
                       [
                        TFlag.AttachmentInput,
                        TFlag.AttachmentDepthInput
                       ];
                      AllAttachmentOutputs=
                       [
                        TFlag.AttachmentOutput,
                        TFlag.AttachmentResolveOutput,
                        TFlag.AttachmentDepthOutput
                       ];
                      AllInputs=
                       [
                        TFlag.AttachmentInput,
                        TFlag.AttachmentDepthInput,
                        TFlag.BufferInput,
                        TFlag.ImageInput
                       ];
                      AllOutputs=
                       [
                        TFlag.AttachmentOutput,
                        TFlag.AttachmentResolveOutput,
                        TFlag.AttachmentDepthOutput,
                        TFlag.BufferOutput,
                        TFlag.ImageOutput
                       ];
                      AllInputsOutputs=AllInputs+AllOutputs;
             private
              fFrameGraph:TpvFrameGraph;
              fPass:TPass;
              fResource:TResource;
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
                                 const aFlags:TFlags); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPass:TPass;
                                 const aResource:TResource;
                                 const aFlags:TFlags;
                                 const aLayout:TVkImageLayout;
                                 const aLoadOp:TLoadOp;
                                 const aImageSubresourceRange:TVkImageSubresourceRange); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPass:TPass;
                                 const aResource:TResource;
                                 const aFlags:TFlags;
                                 const aLayout:TVkImageLayout;
                                 const aLoadOp:TLoadOp); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPass:TPass;
                                 const aResource:TResource;
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
              fResourceList:TResourceList;
              fResourceTransitions:TResourceTransitionList;
              fPreviousPasses:TPassList;
              fNextPasses:TPassList;
              fEnabled:boolean;
              fProcessed:boolean;
              fMarked:boolean;
              procedure SetName(const aName:TpvRawByteString);
              function AddResource(const aResourceTypeName:TpvRawByteString;
                                   const aResourceName:TpvRawByteString;
                                   const aFlags:TResourceTransition.TFlags;
                                   const aLayout:TVkImageLayout;
                                   const aLoadOp:TLoadOp):TResourceTransition; overload;
              function AddResource(const aResourceTypeName:TpvRawByteString;
                                   const aResourceName:TpvRawByteString;
                                   const aFlags:TResourceTransition.TFlags;
                                   const aPipelineStage:TVkPipelineStageFlags;
                                   const aAccessFlags:TVkAccessFlags;
                                   const aBufferSubresourceRange:TBufferSubresourceRange):TResourceTransition; overload;
              function AddResource(const aResourceTypeName:TpvRawByteString;
                                   const aResourceName:TpvRawByteString;
                                   const aFlags:TResourceTransition.TFlags;
                                   const aLayout:TVkImageLayout;
                                   const aLoadOp:TLoadOp;
                                   const aImageSubresourceRange:TVkImageSubresourceRange):TResourceTransition; overload;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph); reintroduce; virtual;
              destructor Destroy; override;
              procedure AddAttachmentInput(const aResourceTypeName:TpvRawByteString;
                                           const aResourceName:TpvRawByteString;
                                           const aLayout:TVkImageLayout);
              procedure AddAttachmentOutput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aLoadOp:TLoadOp);
              procedure AddAttachmentResolveOutput(const aResourceTypeName:TpvRawByteString;
                                                   const aResourceName:TpvRawByteString;
                                                   const aResourceSourceName:TpvRawByteString;
                                                   const aLayout:TVkImageLayout;
                                                   const aLoadOp:TLoadOp);
              procedure AddAttachmentDepthInput(const aResourceTypeName:TpvRawByteString;
                                                const aResourceName:TpvRawByteString;
                                                const aLayout:TVkImageLayout);
              procedure AddAttachmentDepthOutput(const aResourceTypeName:TpvRawByteString;
                                                 const aResourceName:TpvRawByteString;
                                                 const aLayout:TVkImageLayout;
                                                 const aLoadOp:TLoadOp);
              procedure AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aPipelineStage:TVkPipelineStageFlags;
                                       const aAccessFlags:TVkAccessFlags;
                                       const aBufferSubresourceRange:TBufferSubresourceRange); overload;
              procedure AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aPipelineStage:TVkPipelineStageFlags;
                                       const aAccessFlags:TVkAccessFlags); overload;
              procedure AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                        const aResourceName:TpvRawByteString;
                                        const aPipelineStage:TVkPipelineStageFlags;
                                        const aAccessFlags:TVkAccessFlags;
                                        const aBufferSubresourceRange:TBufferSubresourceRange); overload;
              procedure AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                        const aResourceName:TpvRawByteString;
                                        const aPipelineStage:TVkPipelineStageFlags;
                                        const aAccessFlags:TVkAccessFlags); overload;
              procedure AddImageInput(const aResourceTypeName:TpvRawByteString;
                                      const aResourceName:TpvRawByteString;
                                      const aLayout:TVkImageLayout;
                                      const aImageSubresourceRange:TVkImageSubresourceRange); overload;
              procedure AddImageInput(const aResourceTypeName:TpvRawByteString;
                                      const aResourceName:TpvRawByteString;
                                      const aLayout:TVkImageLayout); overload;
              procedure AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aLayout:TVkImageLayout;
                                       const aLoadOp:TLoadOp;
                                       const aImageSubresourceRange:TVkImageSubresourceRange); overload;
              procedure AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                       const aResourceName:TpvRawByteString;
                                       const aLayout:TVkImageLayout;
                                       const aLoadOp:TLoadOp); overload;
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
       fResourceTypeList:TResourceTypeList;
       fResourceTypeNameHashMap:TResourceTypeNameHashMap;
       fResourceList:TResourceList;
       fResourceNameHashMap:TResourceNameHashMap;
       fResourceTransitions:TResourceTransitionList;
       fPassList:TPassList;
       fPassNameHashMap:TPassNameHashMap;
       fRootPass:TPass;
       fEnforcedRootPass:TPass;
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
       property ResourceTypes:TResourceTypeList read fResourceTypeList;
       property ResourceTypeByName:TResourceTypeNameHashMap read fResourceTypeNameHashMap;
       property Resources:TResourceList read fResourceList;
       property ResourceByName:TResourceNameHashMap read fResourceNameHashMap;
       property Passes:TPassList read fPassList;
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
 fFrameGraph.fResourceTypeList.Add(self);
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
 if assigned(fFrameGraph) then begin
  fFrameGraph.fResourceTypeList.Remove(self);
  fFrameGraph.fResourceTypeNameHashMap.Delete(fName);
 end;
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
 fFrameGraph.fResourceList.Add(self);
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
 if assigned(fFrameGraph) then begin
  fFrameGraph.fResourceList.Remove(self);
  fFrameGraph.fResourceNameHashMap.Delete(fName);
 end;
 inherited Destroy;
end;

{ TpvFrameGraph.TResourceTransition }

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aFlags:TFlags);
begin
 inherited Create;
 fFrameGraph:=aFrameGraph;
 fFrameGraph.fResourceTransitions.Add(self);
 fPass:=aPass;
 fResource:=aResource;
 fFlags:=aFlags;
 fResource.fResourceTransitions.Add(self);
 fPass.fResourceTransitions.Add(self);
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aFlags:TFlags;
                                                     const aLayout:TVkImageLayout;
                                                     const aLoadOp:TLoadOp;
                                                     const aImageSubresourceRange:TVkImageSubresourceRange);
begin
 Create(aFrameGraph,aPass,aResource,aFlags);
 fLayout:=aLayout;
 fLoad:=aLoadOp;
 fImageSubresourceRange:=aImageSubresourceRange;
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aFlags:TFlags;
                                                     const aLayout:TVkImageLayout;
                                                     const aLoadOp:TLoadOp);
begin
 Create(aFrameGraph,aPass,aResource,aFlags,aLayout,aLoadOp,TVkImageSubresourceRange.Create(0,0,1,0,1));
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aFlags:TFlags;
                                                     const aPipelineStage:TVkPipelineStageFlags;
                                                     const aAccessFlags:TVkAccessFlags;
                                                     const aBufferSubresourceRange:TBufferSubresourceRange);
begin
 Create(aFrameGraph,aPass,aResource,aFlags);
 fPipelineStage:=aPipelineStage;
 fAccessFlags:=aAccessFlags;
 fBufferSubresourceRange:=aBufferSubresourceRange;
end;

destructor TpvFrameGraph.TResourceTransition.Destroy;
begin
 if assigned(fFrameGraph) then begin
  fFrameGraph.fResourceTransitions.Remove(self);
 end;
 inherited Destroy;
end;

{ TpvFrameGraph.TPass }

constructor TpvFrameGraph.TPass.Create(const aFrameGraph:TpvFrameGraph);
begin

 inherited Create;

 fFrameGraph:=aFrameGraph;
 fName:='';

 fFrameGraph.fPassList.Add(self);

 fResourceList:=TResourceList.Create;
 fResourceList.OwnsObjects:=false;

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

 FreeAndNil(fResourceList);

 FreeAndNil(fResourceTransitions);

 FreeAndNil(fPreviousPasses);

 FreeAndNil(fNextPasses);

 if assigned(fFrameGraph) then begin
  fFrameGraph.fPassList.Remove(self);
  if length(fName)>0 then begin
   fFrameGraph.fPassNameHashMap.Delete(fName);
  end;
 end;

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
                                    aFlags,
                                    aLayout,
                                    aLoadOp);
 fFrameGraph.fValid:=false;
end;

function TpvFrameGraph.TPass.AddResource(const aResourceTypeName:TpvRawByteString;
                                         const aResourceName:TpvRawByteString;
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
                                    aFlags,
                                    aPipelineStage,
                                    aAccessFlags,
                                    aBufferSubresourceRange);
 fFrameGraph.fValid:=false;
end;

function TpvFrameGraph.TPass.AddResource(const aResourceTypeName:TpvRawByteString;
                                         const aResourceName:TpvRawByteString;
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
                                    aFlags,
                                    aLayout,
                                    aLoadOp,
                                    aImageSubresourceRange);
 fFrameGraph.fValid:=false;
end;

procedure TpvFrameGraph.TPass.AddAttachmentInput(const aResourceTypeName:TpvRawByteString;
                                                 const aResourceName:TpvRawByteString;
                                                 const aLayout:TVkImageLayout);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.AttachmentInput],
             aLayout,
             TLoadOp.Create(TLoadOp.TKind.Load));
end;

procedure TpvFrameGraph.TPass.AddAttachmentOutput(const aResourceTypeName:TpvRawByteString;
                                                  const aResourceName:TpvRawByteString;
                                                  const aLayout:TVkImageLayout;
                                                  const aLoadOp:TLoadOp);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.AttachmentOutput],
             aLayout,
             aLoadOp);
end;

procedure TpvFrameGraph.TPass.AddAttachmentResolveOutput(const aResourceTypeName:TpvRawByteString;
                                                         const aResourceName:TpvRawByteString;
                                                         const aResourceSourceName:TpvRawByteString;
                                                         const aLayout:TVkImageLayout;
                                                         const aLoadOp:TLoadOp);
var ResourceSource:TResource;
begin
 ResourceSource:=fFrameGraph.fResourceNameHashMap[aResourceSourceName];
 if not assigned(ResourceSource) then begin
  raise EpvFrameGraph.Create('Invalid source resource');
 end;
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.AttachmentResolveOutput],
             aLayout,
             aLoadOp).fResolveResource:=ResourceSource;
end;

procedure TpvFrameGraph.TPass.AddAttachmentDepthInput(const aResourceTypeName:TpvRawByteString;
                                                      const aResourceName:TpvRawByteString;
                                                      const aLayout:TVkImageLayout);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.AttachmentDepthInput],
             aLayout,
             TLoadOp.Create(TLoadOp.TKind.Load));
end;

procedure TpvFrameGraph.TPass.AddAttachmentDepthOutput(const aResourceTypeName:TpvRawByteString;
                                                       const aResourceName:TpvRawByteString;
                                                       const aLayout:TVkImageLayout;
                                                       const aLoadOp:TLoadOp);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.AttachmentDepthOutput],
             aLayout,
             aLoadOp);
end;

procedure TpvFrameGraph.TPass.AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aPipelineStage:TVkPipelineStageFlags;
                                             const aAccessFlags:TVkAccessFlags;
                                             const aBufferSubresourceRange:TBufferSubresourceRange);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.BufferInput],
             aPipelineStage,
             aAccessFlags,
             aBufferSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddBufferInput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aPipelineStage:TVkPipelineStageFlags;
                                             const aAccessFlags:TVkAccessFlags);
begin
 AddBufferInput(aResourceTypeName,
                aResourceName,
                aPipelineStage,
                aAccessFlags,
                TBufferSubresourceRange.Create(0,VK_WHOLE_SIZE));
end;

procedure TpvFrameGraph.TPass.AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                              const aResourceName:TpvRawByteString;
                                              const aPipelineStage:TVkPipelineStageFlags;
                                              const aAccessFlags:TVkAccessFlags;
                                              const aBufferSubresourceRange:TBufferSubresourceRange);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.BufferOutput],
             aPipelineStage,
             aAccessFlags,
             aBufferSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddBufferOutput(const aResourceTypeName:TpvRawByteString;
                                              const aResourceName:TpvRawByteString;
                                              const aPipelineStage:TVkPipelineStageFlags;
                                              const aAccessFlags:TVkAccessFlags);
begin
 AddBufferOutput(aResourceTypeName,
                 aResourceName,
                 aPipelineStage,
                 aAccessFlags,
                 TBufferSubresourceRange.Create(0,VK_WHOLE_SIZE));
end;

procedure TpvFrameGraph.TPass.AddImageInput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aImageSubresourceRange:TVkImageSubresourceRange);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.ImageInput],
             aLayout,
             TLoadOp.Create(TLoadOp.TKind.Load),
             aImageSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddImageInput(const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout);
begin
 AddImageInput(aResourceTypeName,
               aResourceName,
               aLayout,
               TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                               0,
                                               VK_REMAINING_MIP_LEVELS,
                                               0,
                                               VK_REMAINING_ARRAY_LAYERS));
end;

procedure TpvFrameGraph.TPass.AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aLayout:TVkImageLayout;
                                             const aLoadOp:TLoadOp;
                                             const aImageSubresourceRange:TVkImageSubresourceRange);
begin
 AddResource(aResourceTypeName,
             aResourceName,
             [TResourceTransition.TFlag.ImageOutput],
             aLayout,
             aLoadOp,
             aImageSubresourceRange);
end;

procedure TpvFrameGraph.TPass.AddImageOutput(const aResourceTypeName:TpvRawByteString;
                                             const aResourceName:TpvRawByteString;
                                             const aLayout:TVkImageLayout;
                                             const aLoadOp:TLoadOp);
begin
 AddImageOutput(aResourceTypeName,
                aResourceName,
                aLayout,
                aLoadOp,
                TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                0,
                                                VK_REMAINING_MIP_LEVELS,
                                                0,
                                                VK_REMAINING_ARRAY_LAYERS));
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

 fResourceTypeList:=TResourceTypeList.Create;
 fResourceTypeList.OwnsObjects:=false;

 fResourceTypeNameHashMap:=TResourceTypeNameHashMap.Create(nil);

 fResourceList:=TResourceList.Create;
 fResourceList.OwnsObjects:=false;

 fResourceNameHashMap:=TResourceNameHashMap.Create(nil);

 fResourceTransitions:=TResourceTransitionList.Create;
 fResourceTransitions.OwnsObjects:=false;

 fPassList:=TPassList.Create;
 fPassList.OwnsObjects:=false;

 fPassNameHashMap:=TPassNameHashMap.Create(nil);

end;

destructor TpvFrameGraph.Destroy;
begin

 while fResourceTypeList.Count>0 do begin
  fResourceTypeList.Items[fResourceTypeList.Count-1].Free;
 end;
 FreeAndNil(fResourceTypeList);

 FreeAndNil(fResourceTypeNameHashMap);

 while fResourceList.Count>0 do begin
  fResourceList.Items[fResourceList.Count-1].Free;
 end;
 FreeAndNil(fResourceList);

 FreeAndNil(fResourceNameHashMap);

 while fResourceTransitions.Count>0 do begin
  fResourceTransitions.Items[fResourceTransitions.Count-1].Free;
 end;
 FreeAndNil(fResourceTransitions);

 while fPassList.Count>0 do begin
  fPassList.Items[fPassList.Count-1].Free;
 end;
 FreeAndNil(fPassList);

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
     end;
     PStackItem=^TStackItem;
     TStack=TpvDynamicStack<TStackItem>;
 function NewStackItem(const aAction:TAction;const aPass:TPass):TStackItem;
 begin
  result.Action:=aAction;
  result.Pass:=aPass;
 end;
var Temporary,Index,BaseStackCount:TpvSizeInt;
    Pass:TPass;
    RenderPass:TRenderPass;
    ResourceTransition,
    OtherResourceTransition:TResourceTransition;
    Resource:TResource;
    Stack:TStack;
    StackItem:TStackItem;
    OK:boolean;
begin

 // Find root pass (a render pass, which have only a single attachment output to a surface/swapchain)
 fRootPass:=fEnforcedRootPass;
 if not assigned(fRootPass) then begin
  for Pass in fPassList do begin
   if Pass is TRenderPass then begin
    RenderPass:=Pass as TRenderPass;
    Temporary:=0;
    for ResourceTransition in RenderPass.fResourceTransitions do begin
     if (ResourceTransition.fFlags*TResourceTransition.AllAttachmentOutputs)<>[] then begin
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
  for Pass in fPassList do begin
   Pass.fProcessed:=false;
   Pass.fMarked:=false;
   Pass.fPreviousPasses.Clear;
   Pass.fNextPasses.Clear;
  end;
  Stack.Push(NewStackItem(TAction.Process,fRootPass));
  while Stack.Pop(StackItem) do begin
   Pass:=StackItem.Pass;
   case StackItem.Action of
    TAction.Process:begin
     if Pass.fMarked then begin
      raise EpvFrameGraphRecursion.Create('Recursion detected');
     end;
     if not Pass.fProcessed then begin
      Pass.fMarked:=true;
      Stack.Push(NewStackItem(TAction.Unmark,Pass));
      BaseStackCount:=Stack.Count;
      for ResourceTransition in Pass.fResourceTransitions do begin
       if (ResourceTransition.Flags*TResourceTransition.AllInputs)<>[] then begin
        Resource:=ResourceTransition.Resource;
        for OtherResourceTransition in Resource.fResourceTransitions do begin
         if (ResourceTransition<>OtherResourceTransition) and
            ((OtherResourceTransition.fFlags*TResourceTransition.AllOutputs)<>[]) then begin
          if Pass.fPreviousPasses.IndexOf(OtherResourceTransition.fPass)<0 then begin
           Pass.fPreviousPasses.Add(OtherResourceTransition.fPass);
          end;
          if OtherResourceTransition.fPass.fNextPasses.IndexOf(Pass)<0 then begin
           OtherResourceTransition.fPass.fNextPasses.Add(Pass);
          end;
          OK:=true;
          for Index:=Stack.Count-1 downto BaseStackCount do begin
           if Stack.Items[Index].Pass=OtherResourceTransition.fPass then begin
            OK:=false;
            break;
           end;
          end;
          if OK then begin
           Stack.Push(NewStackItem(TAction.Process,OtherResourceTransition.fPass));
          end;
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
 finally
  Stack.Finalize;
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
