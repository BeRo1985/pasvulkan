(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Renderer.IBLDescriptor;
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
     PasVulkan.Framework;

type { TpvScene3DRendererIBLDescriptor }     
     TpvScene3DRendererIBLDescriptor=class
      private
       fVulkanDevice:TpvVulkanDevice;
       fDescriptorSet:TpvVulkanDescriptorSet;
       fBinding:TpvSizeInt;
       fSampler:TVkSampler;
       fGGXDescriptorImageInfo:TVkDescriptorImageInfo;
       fCharlieDescriptorImageInfo:TVkDescriptorImageInfo;
       fLambertianDescriptorImageInfo:TVkDescriptorImageInfo;
       fPointerToGGXDescriptorImageInfo:PVkDescriptorImageInfo;
       fPointerToCharlieDescriptorImageInfo:PVkDescriptorImageInfo;
       fPointerToLambertianDescriptorImageInfo:PVkDescriptorImageInfo;
       fDirty:Boolean;
       procedure SetGGXImageView(const aGGXImageView:TVkImageView);
       procedure SetCharlieImageView(const aCharlieImageView:TVkImageView);
       procedure SetLambertianImageView(const aLambertianImageView:TVkImageView);
      public
       constructor Create(const aVulkanDevice:TpvVulkanDevice;const aDescriptorSet:TpvVulkanDescriptorSet;const aBinding:TpvSizeInt;const aSampler:TVkSampler);
       destructor Destroy; override;
       procedure Update(const aInstant:Boolean=false);
       procedure SetFrom(const aScene3D,aRendererInstance:TObject;const aInFlightFrameIndex:TpvSizeInt);
      public
       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;
       property DescriptorSet:TpvVulkanDescriptorSet read fDescriptorSet;
       property Binding:TpvSizeInt read fBinding;
       property GGXImageView:TVkImageView read fGGXDescriptorImageInfo.ImageView write SetGGXImageView;
       property CharlieImageView:TVkImageView read fCharlieDescriptorImageInfo.ImageView write SetCharlieImageView;
       property LambertianImageView:TVkImageView read fLambertianDescriptorImageInfo.ImageView write SetLambertianImageView;
       property GGXDescriptorImageInfo:PVkDescriptorImageInfo read fPointerToGGXDescriptorImageInfo;
       property CharlieDescriptorImageInfo:PVkDescriptorImageInfo read fPointerToCharlieDescriptorImageInfo;
       property LambertianDescriptorImageInfo:PVkDescriptorImageInfo read fPointerToLambertianDescriptorImageInfo;
     end; 

implementation

uses PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance;

constructor TpvScene3DRendererIBLDescriptor.Create(const aVulkanDevice:TpvVulkanDevice;const aDescriptorSet:TpvVulkanDescriptorSet;const aBinding:TpvSizeInt;const aSampler:TVkSampler);
begin
 inherited Create;

 fVulkanDevice:=aVulkanDevice;
 fDescriptorSet:=aDescriptorSet;

 fBinding:=aBinding;

 fGGXDescriptorImageInfo:=TVkDescriptorImageInfo.Create(aSampler,VK_NULL_HANDLE,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 fCharlieDescriptorImageInfo:=TVkDescriptorImageInfo.Create(aSampler,VK_NULL_HANDLE,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
 fLambertianDescriptorImageInfo:=TVkDescriptorImageInfo.Create(aSampler,VK_NULL_HANDLE,VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);

 fPointerToGGXDescriptorImageInfo:=@fGGXDescriptorImageInfo;
 fPointerToCharlieDescriptorImageInfo:=@fCharlieDescriptorImageInfo;
 fPointerToLambertianDescriptorImageInfo:=@fLambertianDescriptorImageInfo;

 fDirty:=true;

end;

destructor TpvScene3DRendererIBLDescriptor.Destroy;
begin
 inherited Destroy;
end;

procedure TpvScene3DRendererIBLDescriptor.SetGGXImageView(const aGGXImageView:TVkImageView);
begin
 if fGGXDescriptorImageInfo.ImageView<>aGGXImageView then begin
  fGGXDescriptorImageInfo.ImageView:=aGGXImageView;
  fDirty:=true;
 end;
end;

procedure TpvScene3DRendererIBLDescriptor.SetCharlieImageView(const aCharlieImageView:TVkImageView);
begin
 if fCharlieDescriptorImageInfo.ImageView<>aCharlieImageView then begin
  fCharlieDescriptorImageInfo.ImageView:=aCharlieImageView;
  fDirty:=true;
 end;
end;

procedure TpvScene3DRendererIBLDescriptor.SetLambertianImageView(const aLambertianImageView:TVkImageView);
begin
 if fLambertianDescriptorImageInfo.ImageView<>aLambertianImageView then begin
  fLambertianDescriptorImageInfo.ImageView:=aLambertianImageView;
  fDirty:=true;
 end;
end;

procedure TpvScene3DRendererIBLDescriptor.Update(const aInstant:Boolean=false);
begin
 if fDirty then begin
  fDirty:=false;
  fDescriptorSet.WriteToDescriptorSet(fBinding,
                                      0,
                                      3,
                                      TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                      [fGGXDescriptorImageInfo,
                                       fCharlieDescriptorImageInfo,
                                       fLambertianDescriptorImageInfo],
                                      [],
                                      [],
                                      aInstant);
 end;
end;

procedure TpvScene3DRendererIBLDescriptor.SetFrom(const aScene3D,aRendererInstance:TObject;const aInFlightFrameIndex:TpvSizeInt);
begin

 if assigned(aRendererInstance) then begin

  if assigned(TpvScene3DRendererInstance(aRendererInstance).ImageBasedLightingReflectionProbeCubeMaps) then begin
   SetGGXImageView(TpvScene3DRendererInstance(aRendererInstance).ImageBasedLightingReflectionProbeCubeMaps.GGXDescriptorImageInfos[aInFlightFrameIndex].imageView);
   SetCharlieImageView(TpvScene3DRendererInstance(aRendererInstance).ImageBasedLightingReflectionProbeCubeMaps.CharlieDescriptorImageInfos[aInFlightFrameIndex].imageView);
   SetLambertianImageView(TpvScene3DRendererInstance(aRendererInstance).ImageBasedLightingReflectionProbeCubeMaps.LambertianDescriptorImageInfos[aInFlightFrameIndex].imageView);
   exit;
  end;

  if assigned(TpvScene3DRendererInstance(aRendererInstance).Renderer) then begin
   SetGGXImageView(TpvScene3DRendererInstance(aRendererInstance).Renderer.ImageBasedLightingEnvMapCubeMaps.GGXDescriptorImageInfo.imageView);
   SetCharlieImageView(TpvScene3DRendererInstance(aRendererInstance).Renderer.ImageBasedLightingEnvMapCubeMaps.CharlieDescriptorImageInfo.imageView);
   SetLambertianImageView(TpvScene3DRendererInstance(aRendererInstance).Renderer.ImageBasedLightingEnvMapCubeMaps.LambertianDescriptorImageInfo.imageView);
   exit;
  end;

 end;

end;

end.
