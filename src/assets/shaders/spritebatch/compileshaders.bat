@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V spritebatch.vert -o spritebatch_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V spritebatch.frag -o spritebatch_frag.spv
