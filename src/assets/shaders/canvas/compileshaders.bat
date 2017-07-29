@echo off
set FILLTYPE_NO_TEXTURE=0
set FILLTYPE_TEXTURE=1
set FILLTYPE_ATLAS_TEXTURE=2

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=vertex -o canvas_vert.spv canvas.vert
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -o canvas_frag_no_texture.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_TEXTURE% -o canvas_frag_texture.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_ATLAS_TEXTURE% -o canvas_frag_atlas_texture.spv canvas.frag

for %%f in (*.spv) do (
  spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
)








