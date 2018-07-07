@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V textoverlay.vert -o textoverlay_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V textoverlay.frag -o textoverlay_frag.spv
for %%f in (*.spv) do (
  spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
)
