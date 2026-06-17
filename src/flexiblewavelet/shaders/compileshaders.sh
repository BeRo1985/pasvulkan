#!/bin/bash
# Compile the Flexible Wavelet Video (FWV) compute shaders to SPIR-V (one .spv per .comp).
# Run after editing any shader; ../../assets/convert.dpr then embeds the .spv into the engine
# (PasVulkanAssets.inc) as the FlexibleWaveletVideo<Name>SPIRV Pascal byte-array constants.

for shader in *.comp; do
  glslc -O --target-env=vulkan -fshader-stage=compute "$shader" -o "${shader%.comp}.spv"
done
# color.comp is compiled a second time with -DHAS_ALPHA: the alpha-aware variant that also writes the decoded alpha plane.
glslc -O --target-env=vulkan -fshader-stage=compute -DHAS_ALPHA color.comp -o color_alpha.spv
# the HDR color shaders likewise get -DHAS_ALPHA variants (write the decoded alpha plane into the HDR / scRGB swapchain A).
glslc -O --target-env=vulkan -fshader-stage=compute -DHAS_ALPHA color_hdr.comp -o color_hdr_alpha.spv
glslc -O --target-env=vulkan -fshader-stage=compute -DHAS_ALPHA color_hdr_scrgb.comp -o color_hdr_scrgb_alpha.spv
# motion_refine.comp is compiled a second time with -DSPATIAL_FULL: the --mv-predict=spatial-full variant (fuller search, weaker bias).
glslc -O --target-env=vulkan -fshader-stage=compute -DSPATIAL_FULL motion_refine.comp -o motion_refine_full.spv
