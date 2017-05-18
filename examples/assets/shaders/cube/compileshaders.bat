@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cube.vert -o cube_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cube.frag -o cube_frag.spv

