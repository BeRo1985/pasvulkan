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
unit PasVulkan.Scene3D.Renderer.Globals;
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

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.VirtualReality,
     PasVulkan.VirtualFileSystem;

type TpvScene3DRendererAntialiasingMode=
      (
       Auto=0,
       None,
       DSAA,
       FXAA,
       SMAA,
       MSAA,
       MSAASMAA,
       TAA
      );

     PpvScene3DRendererAntialiasingMode=^TpvScene3DRendererAntialiasingMode;

     TpvScene3DRendererShadowMode=
      (
       Auto=0,
       None=1,
       PCF=2,
       DPCF=3,
       PCSS=4,
       MSM=5
      );

     PpvScene3DRendererShadowMode=^TpvScene3DRendererShadowMode;

     TpvScene3DRendererTransparencyMode=
      (
       Auto=0,
       Direct,
       SPINLOCKOIT,
       INTERLOCKOIT,
       LOOPOIT,
       WBOIT,
       MBOIT,
       SPINLOCKDFAOIT,
       INTERLOCKDFAOIT
      );

     PpvScene3DRendererTransparencyMode=^TpvScene3DRendererTransparencyMode;

     TpvScene3DRendererDepthOfFieldMode=
      (
       Auto=0,
       None,
       HalfResSeparateNearFar,
       HalfResBruteforce,
       FullResHexagon,
       FullResBruteforce
      );

     PpvScene3DRendererDepthOfFieldMode=^TpvScene3DRendererDepthOfFieldMode;

     TpvScene3DRendererLensMode=
      (
       Auto=0,
       None,
       DownUpsample
      );

     PpvScene3DRendererLensMode=^TpvScene3DRendererLensMode;

     TpvScene3DRendererGlobalIlluminationMode=
      (
       Auto=0,
       
       // No global illumination. Here in this case, it is just StaticEnvironmentMap but with a empty black environment map, for to minimize the count 
       // of the shader variants, and a cubemap lookup costs almost nothing these days.
       //None,
       
       // The simplest and fastest way to add global illumination to a scene is to use a static IBL environment map, for example from the sky. 
       StaticEnvironmentMap, 

       // A camera reflection probe is a cubemap that is updated every frame to reflect the scene around it. Nintendo seems to use this technique in some 
       // of their Nintendo Switch games. It may seem like the wrong approach at first glance, but apparently it still seems to work well, at least when 
       // used well in a targeted way.
       CameraReflectionProbe,

       // The idea of "Radiance hints" is based on reflective shadow maps (RSMs), where but instead of depth, the albedo color is stored in the RSMs. And
       // with cascaded radiance hints, the scene is split into multiple cascades, where each cascade has its own voxel-grid-like state. This technique is
       // very similar to cascaded shadow maps (CSMs), only that it is used for global illumination instead of shadows.
       CascadedRadianceHints,

       // At Voxel cone tracing, the scene is voxelized and then cone traced in an approximated way with help of mipmaps of the 3D voxel texture. This is 
       // a rather good technique, but it has some drawbacks, for example light leaking artifacts at thin walls. And with cascaded voxel cone tracing, the
       // scene is split into multiple cascades, where each cascade has its own voxel grid map. This technique is very similar to cascaded shadow maps (CSMs),
       // only that it is used for global illumination instead of shadows.
       CascadedVoxelConeTracing

{
       // Possible further options on my todo list for the future:

       // And finally, the most accurate and most expensive way to add global illumination to a scene is to use hardware ray tracing for 1SPP path tracing
       // together with temporal denoising. This technique is extremely accurate, but also extremely expensive. But it is also the only way to get the most 
       // accurate global illumination. But sadly it needs hardware support for ray tracing, and this is currently only available on Nvidia RTX graphics cards,
       // newer AMD GPUs and Intel graphics cards. But maybe in the future, ray tracing will be more common and available on all graphics cards, and then it
       // will be the best way to add global illumination to a scene. But until then, it is only a nice to have feature for the future.
       PathTracingWithTemporalDenoising
      
       // I'll avoid Light Propagation Volumes (LPVs) because I do think that it is no more worth to implement it, because it is no real alternative to
       // radiance hints in my own opinion, when radiance hints is really good implemented once. And I'll also avoid Screen Space Global Illumination 
       // (SSGI) because it misses out-of-screen information.

      }
      );

     PpvScene3DRendererGlobalIlluminatonMode=^TpvScene3DRendererGlobalIlluminationMode;

var pvScene3DShaderVirtualFileSystem:TpvVirtualFileSystem=nil;

implementation

uses PasVulkan.Scene3D.Assets;

initialization
{$if declared(get_pasvulkan_scene3dshaders_zip_data) and declared(get_pasvulkan_scene3dshaders_zip_size)}
 pvScene3DShaderVirtualFileSystem:=TpvVirtualFileSystem.Create(get_pasvulkan_scene3dshaders_zip_data,get_pasvulkan_scene3dshaders_zip_size,{$ifdef Windows}'d:\GitHub\pasvulkan\src\assets\shaders\scene3d\scene3dshaders.zip'{$else}'/home/bero/Projects/GitHub/pasvulkan/src/assets/shaders/scene3d/scene3dshaders.zip'{$endif});
{$else}
 pvScene3DShaderVirtualFileSystem:=TpvVirtualFileSystem.Create(@PasVulkan.Scene3D.Assets.Scene3DSPIRVShadersData[0],PasVulkan.Scene3D.Assets.Scene3DSPIRVShadersDataSize,{$ifdef Windows}'d:\GitHub\pasvulkan\src\assets\shaders\scene3d\scene3dshaders.zip'{$else}'/home/bero/Projects/GitHub/pasvulkan/src/assets/shaders/scene3d/scene3dshaders.zip'{$endif});
{$ifend}
finalization
 FreeAndNil(pvScene3DShaderVirtualFileSystem);
end.
