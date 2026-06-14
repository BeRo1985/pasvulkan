@echo off

rem Compiles the shaders used by the PasVulkan.ScreenGUIBase virtual-reality content-projection screen.
rem The resulting SPIR-V modules stay in this directory and are embedded into PasVulkanAssets.inc by ..\..\convert.dpr.

"%VULKAN_SDK%/Bin/glslangValidator.exe" -V fullscreen.vert -o fullscreen_vert.spv
"%VULKAN_SDK%/Bin/glslangValidator.exe" -V contentprojection.frag -o contentprojection_frag.spv
"%VULKAN_SDK%/Bin/glslangValidator.exe" -V blit.frag -o blit_frag.spv
