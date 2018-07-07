@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V dragon.vert -o dragon_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V dragon.frag -o dragon_frag.spv
for %%f in (*.spv) do (
  spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
)


