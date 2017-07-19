@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V canvas.vert -o canvas_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V canvas.frag -o canvas_frag.spv
