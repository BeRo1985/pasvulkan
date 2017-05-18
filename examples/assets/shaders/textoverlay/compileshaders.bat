@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V textoverlay.vert -o textoverlay_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V textoverlay.frag -o textoverlay_frag.spv
