@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.vert -o mesh_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -o mesh_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DALPHATEST -o mesh_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V brdf_ggx.frag -o brdf_ggx_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V fullscreen.vert -o fullscreen_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap.vert -o cubemap_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap_sky.frag -o cubemap_sky_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap_sky.frag -DFAST -o cubemap_sky_fast_frag.spv
for %%f in (*.spv) do (
  rem spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
  rem spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const --strength-reduction --simplify-instructions --remove-duplicates -O %%f -o %%f
)
copy /y mesh_vert.spv ..\..\..\assets\shaders\mesh_vert.spv
copy /y mesh_frag.spv ..\..\..\assets\shaders\mesh_frag.spv
copy /y mesh_masked_frag.spv ..\..\..\assets\shaders\mesh_masked_frag.spv
rem del /f /q *.spv


