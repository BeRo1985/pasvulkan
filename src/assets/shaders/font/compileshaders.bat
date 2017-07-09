@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V font.vert -o font_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V font.frag -o font_frag.spv
