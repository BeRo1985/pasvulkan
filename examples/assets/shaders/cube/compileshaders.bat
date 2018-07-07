@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cube.vert -o cube_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V cube.frag -o cube_frag.spv
for %%f in (*.spv) do (
  spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
)


