@echo off
rem Compile the Flexible Wavelet Video (FWV) compute shaders to SPIR-V (one .spv per .comp).
rem Run after editing any shader; ..\..\assets\convert.dpr then embeds the .spv into the engine
rem (PasVulkanAssets.inc) as the FlexibleWaveletVideo<Name>SPIRV Pascal byte-array constants.

for %%s in (*.comp) do "%VULKAN_SDK%\Bin\glslc.exe" -O --target-env=vulkan -fshader-stage=compute "%%s" -o "%%~ns.spv"
rem color.comp is compiled a second time with -DHAS_ALPHA: the alpha-aware variant that also writes the decoded alpha plane.
"%VULKAN_SDK%\Bin\glslc.exe" -O --target-env=vulkan -fshader-stage=compute -DHAS_ALPHA color.comp -o color_alpha.spv
rem the HDR color shaders likewise get -DHAS_ALPHA variants (write the decoded alpha plane into the HDR / scRGB swapchain A).
"%VULKAN_SDK%\Bin\glslc.exe" -O --target-env=vulkan -fshader-stage=compute -DHAS_ALPHA color_hdr.comp -o color_hdr_alpha.spv
"%VULKAN_SDK%\Bin\glslc.exe" -O --target-env=vulkan -fshader-stage=compute -DHAS_ALPHA color_hdr_scrgb.comp -o color_hdr_scrgb_alpha.spv
