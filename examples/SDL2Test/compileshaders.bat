@echo off
"C:/VulkanSDK/1.0.46.0/Bin32/glslangValidator.exe" -V triangle.vert -o triangle_vert.spv
"C:/VulkanSDK/1.0.46.0/Bin32/glslangValidator.exe" -V triangle.frag -o triangle_frag.spv
pause
