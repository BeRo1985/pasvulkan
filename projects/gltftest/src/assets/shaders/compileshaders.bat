@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V lightclustergridbuild.comp -o lightclustergridbuild_comp.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.vert -o mesh_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -o mesh_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DALPHATEST -o mesh_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V brdf_charlie.frag -o brdf_charlie_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V brdf_ggx.frag -o brdf_ggx_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V fullscreen.vert -o fullscreen_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap.vert -o cubemap_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap_sky.frag -o cubemap_sky_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap_sky.frag -DFAST -o cubemap_sky_fast_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap_charlie_filter.comp -o cubemap_charlie_filter_comp.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap_ggx_filter.comp -o cubemap_ggx_filter_comp.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cubemap_lambertian_filter.comp -o cubemap_lambertian_filter_comp.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V passthrough.vert -o passthrough_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V dummy.frag -o dummy_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V skybox.vert -o skybox_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V skybox.frag -o skybox_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V tonemapping.frag -o tonemapping_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V antialiasing.frag -o antialiasing_frag.spv
for %%f in (*.spv) do (
  rem spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
  rem spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const --strength-reduction --simplify-instructions --remove-duplicates -O %%f -o %%f
)
copy /y *.spv ..\..\..\assets\shaders\*.spv
del /f /q *.spv


