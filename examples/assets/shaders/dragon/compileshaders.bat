@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V dragon.vert -o dragon_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V dragon.frag -o dragon_frag.spv

