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

     TpvFrameGraph=class
      public
       type TBufferSubresourceRange=record
             Offset:TVkDeviceSize;
             Range:TVkDeviceSize;
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
            TResource=class
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fResourceType:TResourceType;
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
                                 const aLoad:TLoadOp;
                                 const aImageSubresourceRange:TVkImageSubresourceRange); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aPass:TPass;
                                 const aResource:TResource;
                                 const aFlags:TFlags;
                                 const aLayout:TVkImageLayout;
                                 const aLoad:TLoadOp); reintroduce; overload;
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
            TResourceTransitionList=TpvObjectGenericList<TResourceTransition>;
            TPassArray=array of TPass;
            TPassArrayArray=array of TPassArray;
            TPass=class
             private
              fFrameGraph:TpvFrameGraph;
              fName:TpvRawByteString;
              fRequiredPasses:TPassArrayArray;
              fCountRequiredPasses:TpvSizeInt;
              fConflictedPasses:TPassArray;
              fCountConflictedPasses:TpvSizeInt;
              fResourceTransitionList:TResourceTransitionList;
              fEnabled:boolean;
             public
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString); reintroduce; virtual;
              destructor Destroy; override;
              procedure RequiresPass(const aPasses:array of TPass); overload;
              procedure RequiresPass(const aPasses:array of TpvRawByteString); overload;
              procedure ConflictsWithPass(const aPass:TPass); overload;
              procedure ConflictsWithPass(const aPass:TpvRawByteString); overload;
             published
              property FrameGraph:TpvFrameGraph read fFrameGraph;
              property Name:TpvRawByteString read fName;
              property Enabled:boolean read fEnabled write fEnabled;
            end;
            TPassList=TpvObjectGenericList<TPass>;
            TPassNameHashMap=TpvStringHashMap<TPass>;
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
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aMultiViewMask:TpvUInt32;
                                 const aAttachmentSize:TAttachmentSize); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aAttachmentSize:TAttachmentSize); reintroduce; overload;
              constructor Create(const aFrameGraph:TpvFrameGraph;
                                 const aName:TpvRawByteString;
                                 const aMultiViewMask:TpvUInt32=0); reintroduce; overload;
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
       fResourceTransitionList:TResourceTransitionList;
       fPassList:TPassList;
       fPassNameHashMap:TPassNameHashMap;
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
       function AddRenderPass(const aName:TpvRawByteString;
                              const aMultiViewMask:TpvUInt32;
                              const aAttachmentSize:TAttachmentSize):TRenderPass; overload;
       function AddRenderPass(const aName:TpvRawByteString;
                              const aAttachmentSize:TAttachmentSize):TRenderPass; overload;
       function AddRenderPass(const aName:TpvRawByteString;
                              const aMultiViewMask:TpvUInt32=0):TRenderPass; overload;
      public
       procedure AddAttachmentInput(const aPassName:TpvRawByteString;
                                    const aResourceTypeName:TpvRawByteString;
                                    const aResourceName:TpvRawByteString;
                                    const aLayout:TVkImageLayout);
      public
       procedure AddAttachmentOutput(const aPassName:TpvRawByteString;
                                     const aResourceTypeName:TpvRawByteString;
                                     const aResourceName:TpvRawByteString;
                                     const aLayout:TVkImageLayout;
                                     const aLoadOp:TLoadOp);
      public
       procedure AddAttachmentResolveOutput(const aPassName:TpvRawByteString;
                                            const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aResourceSourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aLoadOp:TLoadOp);
      public
       procedure AddAttachmentDepthInput(const aPassName:TpvRawByteString;
                                         const aResourceTypeName:TpvRawByteString;
                                         const aResourceName:TpvRawByteString;
                                         const aLayout:TVkImageLayout);
      public
       procedure AddAttachmentDepthOutput(const aPassName:TpvRawByteString;
                                          const aResourceTypeName:TpvRawByteString;
                                          const aResourceName:TpvRawByteString;
                                          const aLayout:TVkImageLayout;
                                          const aLoadOp:TLoadOp);
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
     end;

implementation

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
 fFrameGraph.fResourceTransitionList.Add(self);
 fPass:=aPass;
 fResource:=aResource;
 fFlags:=aFlags;
 fPass.fResourceTransitionList.Add(self);
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aFlags:TFlags;
                                                     const aLayout:TVkImageLayout;
                                                     const aLoad:TLoadOp;
                                                     const aImageSubresourceRange:TVkImageSubresourceRange);
begin
 Create(aFrameGraph,aPass,aResource,aFlags);
 fLayout:=aLayout;
 fLoad:=aLoad;
 fImageSubresourceRange:=aImageSubresourceRange;
end;

constructor TpvFrameGraph.TResourceTransition.Create(const aFrameGraph:TpvFrameGraph;
                                                     const aPass:TPass;
                                                     const aResource:TResource;
                                                     const aFlags:TFlags;
                                                     const aLayout:TVkImageLayout;
                                                     const aLoad:TLoadOp);
begin
 Create(aFrameGraph,aPass,aResource,aFlags,aLayout,aLoad,TVkImageSubresourceRange.Create(0,0,1,0,1));
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
  fFrameGraph.fResourceTransitionList.Remove(self);
 end;
 inherited Destroy;
end;

{ TpvFrameGraph.TPass }

constructor TpvFrameGraph.TPass.Create(const aFrameGraph:TpvFrameGraph;
                                       const aName:TpvRawByteString);
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

 fFrameGraph.fPassList.Add(self);
 fFrameGraph.fPassNameHashMap.Add(fName,self);

 fRequiredPasses:=nil;
 fCountRequiredPasses:=0;

 fConflictedPasses:=nil;
 fCountConflictedPasses:=0;

 fResourceTransitionList:=TResourceTransitionList.Create;
 fResourceTransitionList.OwnsObjects:=false;

 fEnabled:=true;

end;

destructor TpvFrameGraph.TPass.Destroy;
begin

 fRequiredPasses:=nil;

 fConflictedPasses:=nil;

 FreeAndNil(fResourceTransitionList);

 if assigned(fFrameGraph) then begin
  fFrameGraph.fPassList.Remove(self);
  fFrameGraph.fPassNameHashMap.Delete(fName);
 end;

 inherited Destroy;

end;

procedure TpvFrameGraph.TPass.RequiresPass(const aPasses:array of TPass);
var Index:TpvSizeInt;
begin
 if length(aPasses)>0 then begin
  for Index:=0 to length(aPasses)-1 do begin
   if not assigned(aPasses[Index]) then begin
    raise EpvFrameGraph.Create('Invalid pass argument');
   end;
  end;
  Index:=fCountRequiredPasses;
  if length(fRequiredPasses)<=fCountRequiredPasses then begin
   SetLength(fRequiredPasses,(fCountRequiredPasses*3) div 2);
  end;
  SetLength(fRequiredPasses[Index],length(aPasses));
  Move(aPasses[0],fRequiredPasses[Index,0],length(aPasses)*SizeOf(TPass));
 end;
end;

procedure TpvFrameGraph.TPass.RequiresPass(const aPasses:array of TpvRawByteString);
var Index:TpvSizeInt;
    Passes:TPassArray;
begin
 if length(aPasses)>0 then begin
  SetLength(Passes,length(aPasses));
  for Index:=0 to length(aPasses)-1 do begin
   Passes[Index]:=fFrameGraph.fPassNameHashMap[aPasses[Index]];
  end;
  RequiresPass(Passes);
 end;
end;

procedure TpvFrameGraph.TPass.ConflictsWithPass(const aPass:TPass);
var Index:TpvSizeInt;
begin
 if not assigned(aPass) then begin
  raise EpvFrameGraph.Create('Invalid pass argument');
 end;
 Index:=fCountConflictedPasses;
 if length(fConflictedPasses)<=fCountConflictedPasses then begin
  SetLength(fConflictedPasses,(fCountConflictedPasses*3) div 2);
 end;
 fConflictedPasses[Index]:=aPass;
end;

procedure TpvFrameGraph.TPass.ConflictsWithPass(const aPass:TpvRawByteString);
begin
 ConflictsWithPass(fFrameGraph.fPassNameHashMap[aPass]);
end;

{ TpvFrameGraph.TRenderPass }

constructor TpvFrameGraph.TRenderPass.Create(const aFrameGraph:TpvFrameGraph;
                                             const aName:TpvRawByteString;
                                             const aMultiViewMask:TpvUInt32;
                                             const aAttachmentSize:TAttachmentSize);
begin
 inherited Create(aFrameGraph,aName);
 fMultiViewMask:=aMultiViewMask;
 fAttachmentSize:=aAttachmentSize;
end;

constructor TpvFrameGraph.TRenderPass.Create(const aFrameGraph:TpvFrameGraph;
                                             const aName:TpvRawByteString;
                                             const aAttachmentSize:TAttachmentSize);
begin
 inherited Create(aFrameGraph,aName);
 fMultiViewMask:=0;
 fAttachmentSize:=aAttachmentSize;
end;

constructor TpvFrameGraph.TRenderPass.Create(const aFrameGraph:TpvFrameGraph;
                                             const aName:TpvRawByteString;
                                             const aMultiViewMask:TpvUInt32=0);
begin
 Create(aFrameGraph,
        aName,
        aMultiViewMask,
        TAttachmentSize.Create(TAttachmentSize.TKind.SurfaceDependent,1.0,1.0));
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

 fResourceTransitionList:=TResourceTransitionList.Create;
 fResourceTransitionList.OwnsObjects:=false;

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

 while fResourceTransitionList.Count>0 do begin
  fResourceTransitionList.Items[fResourceTransitionList.Count-1].Free;
 end;
 FreeAndNil(fResourceTransitionList);

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

function TpvFrameGraph.AddRenderPass(const aName:TpvRawByteString;
                                     const aMultiViewMask:TpvUInt32;
                                     const aAttachmentSize:TAttachmentSize):TRenderPass;
begin
 result:=TRenderPass.Create(self,aName,aMultiViewMask,aAttachmentSize);
end;

function TpvFrameGraph.AddRenderPass(const aName:TpvRawByteString;
                                     const aAttachmentSize:TAttachmentSize):TRenderPass;
begin
 result:=TRenderPass.Create(self,aName,aAttachmentSize);
end;

function TpvFrameGraph.AddRenderPass(const aName:TpvRawByteString;
                                     const aMultiViewMask:TpvUInt32=0):TRenderPass;
begin
 result:=TRenderPass.Create(self,aName,aMultiViewMask);
end;

procedure TpvFrameGraph.AddAttachmentInput(const aPassName:TpvRawByteString;
                                           const aResourceTypeName:TpvRawByteString;
                                           const aResourceName:TpvRawByteString;
                                           const aLayout:TVkImageLayout);
var Pass:TPass;
    ResourceType:TResourceType;
    Resource:TResource;
begin
 Pass:=fPassNameHashMap[aPassName];
 if not assigned(Pass) then begin
  raise EpvFrameGraph.Create('Invalid pass');
 end;
 ResourceType:=fResourceTypeNameHashMap[aResourceTypeName];
 if not assigned(ResourceType) then begin
  raise EpvFrameGraph.Create('Invalid resource type');
 end;
 Resource:=fResourceNameHashMap[aResourceTypeName];
 if assigned(Resource) then begin
  if Resource.fResourceType<>ResourceType then begin
   raise EpvFrameGraph.Create('Resource type mismatch');
  end;
 end else begin
  Resource:=TResource.Create(self,aResourceName,ResourceType);
 end;
 if ResourceType.fMetaType<>TResourceType.TMetaType.Attachment then begin
  raise EpvFrameGraph.Create('Resource meta type mismatch');
 end;
 TResourceTransition.Create(self,
                            Pass,
                            Resource,
                            [TResourceTransition.TFlag.AttachmentInput],
                            aLayout,
                            TLoadOp.Create(TLoadOp.TKind.Load));
 fValid:=false;
end;

procedure TpvFrameGraph.AddAttachmentOutput(const aPassName:TpvRawByteString;
                                            const aResourceTypeName:TpvRawByteString;
                                            const aResourceName:TpvRawByteString;
                                            const aLayout:TVkImageLayout;
                                            const aLoadOp:TLoadOp);
var Pass:TPass;
    ResourceType:TResourceType;
    Resource:TResource;
begin
 Pass:=fPassNameHashMap[aPassName];
 if not assigned(Pass) then begin
  raise EpvFrameGraph.Create('Invalid pass');
 end;
 ResourceType:=fResourceTypeNameHashMap[aResourceTypeName];
 if not assigned(ResourceType) then begin
  raise EpvFrameGraph.Create('Invalid resource type');
 end;
 Resource:=fResourceNameHashMap[aResourceTypeName];
 if assigned(Resource) then begin
  if Resource.fResourceType<>ResourceType then begin
   raise EpvFrameGraph.Create('Resource type mismatch');
  end;
 end else begin
  Resource:=TResource.Create(self,aResourceName,ResourceType);
 end;
 if ResourceType.fMetaType<>TResourceType.TMetaType.Attachment then begin
  raise EpvFrameGraph.Create('Resource meta type mismatch');
 end;
 TResourceTransition.Create(self,
                            Pass,
                            Resource,
                            [TResourceTransition.TFlag.AttachmentOutput],
                            aLayout,
                            aLoadOp);
 fValid:=false;
end;

procedure TpvFrameGraph.AddAttachmentResolveOutput(const aPassName:TpvRawByteString;
                                                   const aResourceTypeName:TpvRawByteString;
                                                   const aResourceName:TpvRawByteString;
                                                   const aResourceSourceName:TpvRawByteString;
                                                   const aLayout:TVkImageLayout;
                                                   const aLoadOp:TLoadOp);
begin
end;

procedure TpvFrameGraph.AddAttachmentDepthInput(const aPassName:TpvRawByteString;
                                                const aResourceTypeName:TpvRawByteString;
                                                const aResourceName:TpvRawByteString;
                                                const aLayout:TVkImageLayout);
begin
end;

procedure TpvFrameGraph.AddAttachmentDepthOutput(const aPassName:TpvRawByteString;
                                                 const aResourceTypeName:TpvRawByteString;
                                                 const aResourceName:TpvRawByteString;
                                                 const aLayout:TVkImageLayout;
                                                 const aLoadOp:TLoadOp);
begin
end;

procedure TpvFrameGraph.Setup;
begin

end;

procedure TpvFrameGraph.Compile;
begin

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
