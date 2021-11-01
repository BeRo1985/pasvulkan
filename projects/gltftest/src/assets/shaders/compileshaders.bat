@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V lightclustergridbuild.comp -o lightclustergridbuild_comp.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.vert -o mesh_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -o mesh_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DALPHATEST -o mesh_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -DREVERSEDZ -o mesh_oit_spinlock_reversedz_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -DREVERSEDZ -DALPHATEST -o mesh_oit_spinlock_reversedz_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -o mesh_oit_spinlock_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -DALPHATEST -o mesh_oit_spinlock_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -DREVERSEDZ -DINTERLOCK -o mesh_oit_interlock_reversedz_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -DREVERSEDZ -DINTERLOCK -DMASKED -o mesh_oit_interlock_reversedz_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -DINTERLOCK -o mesh_oit_interlock_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DLIGHTS -DSHADOWS -DOIT -DINTERLOCK -DMASKED -o mesh_oit_interlock_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DDEPTHONLY -o mesh_depth_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DDEPTHONLY -DALPHATEST -o mesh_depth_masked_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V oit_resolve.frag -o oit_resolve_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V oit_resolve.frag -DREVERSEDZ -o oit_resolve_reversedz_frag.spv
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
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V skybox_realtime.frag -o skybox_realtime_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V tonemapping.frag -o tonemapping_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V antialiasing.frag -o antialiasing_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V blit.frag -o blit_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V msm_blur.frag -o msm_blur_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V msm_blur.vert -o msm_blur_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V msm_resolve.frag -o msm_resolve_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V msm_resolve.vert -o msm_resolve_vert.spv
for %%f in (*.spv) do (
  rem "%VULKAN_SDK%/Bin32/spirv-opt.exe" --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
  rem "%VULKAN_SDK%/Bin32/spirv-opt.exe" --strip-debug --unify-const --flatten-decorations --eliminate-dead-const --strength-reduction --simplify-instructions --remove-duplicates -O %%f -o %%f
)
copy /y *.spv ..\..\..\assets\shaders\*.spv
del /f /q *.spv


