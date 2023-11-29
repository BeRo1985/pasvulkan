(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2023, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Planet;
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

uses Classes,
     SysUtils,
     Math,
     PasMP,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.TimerQuery,
     PasVulkan.Collections,
     PasVulkan.CircularDoublyLinkedList,
     PasVulkan.VirtualReality,
     PasVulkan.Scene3D.Renderer.Globals,
     PasVulkan.Scene3D.Renderer.Image2D;

type TpvScene3DPlanets=class;

     { TpvScene3DPlanet }
     TpvScene3DPlanet=class
      public
       type THeightValue=TpvFloat;
            PHeightValue=^THeightValue;
            THeightMap=array of THeightValue;
            { TData }
            TData=class // one ground truth instance and one or more in-flight instances for flawlessly parallel rendering
             private    // All 2D maps are octahedral projected maps in this implementation (not equirectangular projected maps or cube maps)
              fPlanet:TpvScene3DPlanet;
              fInFlightFrameIndex:TpvInt32; // -1 is the ground truth instance, >=0 are the in-flight frame instances
              fHeightMap:THeightMap; // only on the ground truth instance, otherwise nil
              fHeightMapImage:TpvScene3DRendererImage2D; // R32_SFLOAT (at least for now, just for the sake of simplicity, later maybe R16_UNORM or R16_SNORM)
              fNormalMapImage:TpvScene3DRendererImage2D; // R16G16_SFLOAT (octahedral)
              fTangentBitangentMapImage:TpvScene3DRendererImage2D; // R16RG16B16A16_SFLOAT (octahedral-wise)              
              fModelMatrix:TpvMatrix4x4;
              fReady:TPasMPBool32;
             public 
              constructor Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32); reintroduce;
              destructor Destroy; override; 
              procedure TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                   const aInFlightFrameData:TData;
                                   const aFromSrcQueueFamilyIndex:TpvUInt32=VK_QUEUE_FAMILY_IGNORED;
                                   const aToSrcQueueFamilyIndex:TpvUInt32=VK_QUEUE_FAMILY_IGNORED;
                                   const aDstQueueFamilyIndex:TpvUInt32=VK_QUEUE_FAMILY_IGNORED);
             published
              property Planet:TpvScene3DPlanet read fPlanet;
              property InFlightFrameIndex:TpvInt32 read fInFlightFrameIndex;
              property HeightMap:THeightMap read fHeightMap;              
              property HeightMapImage:TpvScene3DRendererImage2D read fHeightMapImage;
              property NormalMapImage:TpvScene3DRendererImage2D read fNormalMapImage;
              property TangentBitangentMapImage:TpvScene3DRendererImage2D read fTangentBitangentMapImage; 
             public
              property ModelMatrix:TpvMatrix4x4 read fModelMatrix write fModelMatrix; 
              property Ready:TPasMPBool32 read fReady write fReady;
            end;
            TInFlightFrameDataList=TpvObjectGenericList<TData>;
      private
       fScene3D:TObject;
       fHeightMapResolution:TpvInt32;
       fCountSpherePoints:TpvSizeInt;
       fBottomRadius:TpvFloat; // Start of the lowest planet ground
       fTopRadius:TpvFloat; // End of the atmosphere
       fHeightMapScale:TpvFloat; // Scale factor for the height map
       fData:TData;
       fInFlightFrameDataList:TInFlightFrameDataList;
       fReleaseFrameCounter:TpvInt32;
       fReady:TPasMPBool32;
       fInFlightFrameReady:array[0..MaxInFlightFrames-1] of TPasMPBool32;
      public
      constructor Create(const aScene3D:TObject;     
                          const aHeightMapResolution:TpvInt32=2048;
                          const aCountSpherePoints:TpvSizeInt=65536;
                          const aBottomRadius:TpvFloat=6371000.0;
                          const aTopRadius:TpvFloat=6471000.0;
                          const aHeightMapScale:TpvFloat=1000.0); reintroduce;
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release;
       function HandleRelease:boolean;
      published
       property Scene3D:TObject read fScene3D;
       property HeightMapResolution:TpvInt32 read fHeightMapResolution;
       property CountSpherePoints:TpvSizeInt read fCountSpherePoints;
       property BottomRadius:TpvFloat read fBottomRadius;
       property TopRadius:TpvFloat read fTopRadius;
       property Data:TData read fData;
       property InFlightFrameDataList:TInFlightFrameDataList read fInFlightFrameDataList;
     end;

     TpvScene3DPlanets=class(TpvObjectGenericList<TpvScene3DPlanet>)
      private
       fScene3D:TObject;
       fLock:TPasMPCriticalSection;
      public
       constructor Create(const aScene3D:TObject); reintroduce;
       destructor Destroy; override;
      published
       property Scene3D:TObject read fScene3D;
       property Lock:TPasMPCriticalSection read fLock;
     end;

implementation

uses PasVulkan.Scene3D;

{ TpvScene3DPlanet.TData }

constructor TpvScene3DPlanet.TData.Create(const aPlanet:TpvScene3DPlanet;const aInFlightFrameIndex:TpvInt32);
begin
  
 inherited Create;

 fPlanet:=aPlanet;

 fInFlightFrameIndex:=aInFlightFrameIndex;

 if fInFlightFrameIndex<0 then begin
  fHeightMap:=nil;
  SetLength(fHeightMap,fPlanet.fHeightMapResolution*fPlanet.fHeightMapResolution);
 end else begin
  fHeightMap:=nil;
 end;

 fModelMatrix:=TpvMatrix4x4.Identity;

 if assigned(TpvScene3D(fPlanet.fScene3D).VulkanDevice) then begin

  fHeightMapImage:=TpvScene3DRendererImage2D.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                    fPlanet.fHeightMapResolution,
                                                    fPlanet.fHeightMapResolution,
                                                    VK_FORMAT_R32_SFLOAT,
                                                    VK_SAMPLE_COUNT_1_BIT,
                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

  fNormalMapImage:=TpvScene3DRendererImage2D.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                    fPlanet.fHeightMapResolution,
                                                    fPlanet.fHeightMapResolution,
                                                    VK_FORMAT_R16G16_SFLOAT,
                                                    VK_SAMPLE_COUNT_1_BIT,
                                                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

  fTangentBitangentMapImage:=TpvScene3DRendererImage2D.Create(TpvScene3D(fPlanet.fScene3D).VulkanDevice,
                                                              fPlanet.fHeightMapResolution,
                                                              fPlanet.fHeightMapResolution,
                                                              VK_FORMAT_R16G16B16A16_SFLOAT,
                                                              VK_SAMPLE_COUNT_1_BIT,
                                                              VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
  
 end else begin

  fHeightMapImage:=nil;

  fNormalMapImage:=nil;

  fTangentBitangentMapImage:=nil;

 end;

end;

destructor TpvScene3DPlanet.TData.Destroy;
begin
 fHeightMap:=nil;
 FreeAndNil(fHeightMapImage);
 FreeAndNil(fNormalMapImage);
 FreeAndNil(fTangentBitangentMapImage);
 inherited Destroy;
end;

procedure TpvScene3DPlanet.TData.TransferTo(const aCommandBuffer:TpvVulkanCommandBuffer;
                                            const aInFlightFrameData:TData;
                                            const aFromSrcQueueFamilyIndex:TpvUInt32;
                                            const aToSrcQueueFamilyIndex:TpvUInt32;
                                            const aDstQueueFamilyIndex:TpvUInt32);
var Index,CountImageMemoryBarriers:TpvSizeInt;
    ImageSubresourceRange:TVkImageSubresourceRange;
    ImageMemoryBarriers:array[0..5] of TVkImageMemoryBarrier;
    ImageBlit:TVkImageBlit;
begin
  
 if assigned(TpvScene3D(fPlanet.fScene3D).VulkanDevice) then begin

  ImageSubresourceRange:=TVkImageSubresourceRange.Create(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                         0,
                                                         1,
                                                         0,
                                                         1);

  //////////////////////////// 
 
  begin                                                      

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);      

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange); 

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aToSrcQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aToSrcQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aToSrcQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);                                                     

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     0,
                                     0,nil,
                                     0,nil,
                                     6,@ImageMemoryBarriers[0]);                                                 
  end;   
 
  //////////////////////////// 

  begin
    
   FillChar(ImageBlit,SizeOf(TVkImageBlit),#0);
   ImageBlit.srcSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageBlit.srcSubresource.mipLevel:=0;
   ImageBlit.srcSubresource.baseArrayLayer:=0;
   ImageBlit.srcSubresource.layerCount:=1;
   ImageBlit.srcOffsets[0].x:=0;
   ImageBlit.srcOffsets[0].y:=0;
   ImageBlit.srcOffsets[0].z:=0;
   ImageBlit.srcOffsets[1].x:=fPlanet.fHeightMapResolution;
   ImageBlit.srcOffsets[1].y:=fPlanet.fHeightMapResolution;
   ImageBlit.srcOffsets[1].z:=1;
   ImageBlit.dstSubresource.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageBlit.dstSubresource.mipLevel:=0;
   ImageBlit.dstSubresource.baseArrayLayer:=0;
   ImageBlit.dstSubresource.layerCount:=1;
   ImageBlit.dstOffsets[0].x:=0;
   ImageBlit.dstOffsets[0].y:=0;
   ImageBlit.dstOffsets[0].z:=0;
   ImageBlit.dstOffsets[1].x:=fPlanet.fHeightMapResolution;
   ImageBlit.dstOffsets[1].y:=fPlanet.fHeightMapResolution;
   ImageBlit.dstOffsets[1].z:=1;

   aCommandBuffer.CmdBlitImage(fHeightMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               1,@ImageBlit,
                               VK_FILTER_NEAREST);

   aCommandBuffer.CmdBlitImage(fNormalMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               1,@ImageBlit,
                               VK_FILTER_NEAREST);

   aCommandBuffer.CmdBlitImage(fTangentBitangentMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                               aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                               VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                               1,@ImageBlit,
                               VK_FILTER_NEAREST);         

  end;

  //////////////////////////// 

  begin

   ImageMemoryBarriers[0]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[1]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[2]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aFromSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[3]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        aInFlightFrameData.fHeightMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   ImageMemoryBarriers[4]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        aInFlightFrameData.fNormalMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);   

   ImageMemoryBarriers[5]:=TVkImageMemoryBarrier.Create(TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT),
                                                        TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_SHADER_WRITE_BIT),
                                                        VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                                        VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aDstQueueFamilyIndex),
                                                        IfThen(aToSrcQueueFamilyIndex=aDstQueueFamilyIndex,VK_QUEUE_FAMILY_IGNORED,aFromSrcQueueFamilyIndex),
                                                        aInFlightFrameData.fTangentBitangentMapImage.VulkanImage.Handle,
                                                        ImageSubresourceRange);

   aCommandBuffer.CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_TRANSFER_BIT),
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_INPUT_BIT) or
                                     TVkPipelineStageFlags(VK_PIPELINE_STAGE_VERTEX_SHADER_BIT),
                                     0,
                                     0,
                                     nil,
                                     0,nil,
                                     6,@ImageMemoryBarriers[0]);
      
  end;

  ////////////////////////////

 end;

 aInFlightFrameData.fModelMatrix:=fModelMatrix;

 aInFlightFrameData.fReady:=fReady;

end;

{ TpvScene3DPlanet }

constructor TpvScene3DPlanet.Create(const aScene3D:TObject;
                                    const aHeightMapResolution:TpvInt32;
                                    const aCountSpherePoints:TpvSizeInt;
                                    const aBottomRadius:TpvFloat;
                                    const aTopRadius:TpvFloat;
                                    const aHeightMapScale:TpvFloat);
var InFlightFrameIndex:TpvSizeInt;
begin

 inherited Create;

 fScene3D:=aScene3D;

 fHeightMapResolution:=RoundUpToPowerOfTwo(Min(Max(aHeightMapResolution,128),8192));

 fCountSpherePoints:=Min(Max(aCountSpherePoints,32),16777216);

 fBottomRadius:=aBottomRadius;

 fTopRadius:=aTopRadius;

 fHeightMapScale:=aHeightMapScale;

 fData:=TData.Create(self,-1);

 fInFlightFrameDataList:=TInFlightFrameDataList.Create(true);
 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  fInFlightFrameDataList.Add(TData.Create(self,InFlightFrameIndex));
 end;

 fReleaseFrameCounter:=-1;

 fReady:=true;

 for InFlightFrameIndex:=0 to TpvScene3D(fScene3D).CountInFlightFrames-1 do begin
  fInFlightFrameReady[InFlightFrameIndex]:=false;
 end; 

end;

destructor TpvScene3DPlanet.Destroy;
begin
 
 FreeAndNil(fData);
 
 FreeAndNil(fInFlightFrameDataList);

 inherited Destroy;

end;

procedure TpvScene3DPlanet.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fScene3D) then begin
  TpvScene3D(fScene3D).Planets.Lock.Acquire;
  try  
   TpvScene3D(fScene3D).Planets.Add(self); 
  finally 
   TpvScene3D(fScene3D).Planets.Lock.Release;
  end; 
 end;
end;

procedure TpvScene3DPlanet.BeforeDestruction;
var Index:TpvSizeInt;
begin
 if assigned(fScene3D) then begin
  TpvScene3D(fScene3D).Planets.Lock.Acquire;
  try  
   Index:=TpvScene3D(fScene3D).Planets.IndexOf(self);
   if Index>=0 then begin
    TpvScene3D(fScene3D).Planets.Extract(Index); // not delete ir remove, since we don't want to free ourself here already.
   end;
  finally 
   TpvScene3D(fScene3D).Planets.Lock.Release;
  end; 
 end; 
 inherited BeforeDestruction;
end;

procedure TpvScene3DPlanet.Release;
begin
 if fReleaseFrameCounter<0 then begin
  fReleaseFrameCounter:=TpvScene3D(fScene3D).CountInFlightFrames;
  fReady:=false;
 end;
end;

function TpvScene3DPlanet.HandleRelease:boolean;
begin
 if fReleaseFrameCounter>0 then begin
  result:=TPasMPInterlocked.Decrement(fReleaseFrameCounter)=0;
  if result then begin
   Free;
  end;
 end else begin
  result:=false;
 end; 
end;

{ TpvScene3DPlanets }

constructor TpvScene3DPlanets.Create(const aScene3D:TObject);
begin
 inherited Create(true);
 fScene3D:=aScene3D;
 fLock:=TPasMPCriticalSection.Create;
end;

destructor TpvScene3DPlanets.Destroy;
begin
 FreeAndNil(fLock);
 inherited Destroy;
end; 

end.

