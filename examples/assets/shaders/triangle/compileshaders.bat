@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V triangle.vert -o triangle_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V triangle.frag -o triangle_frag.spv

