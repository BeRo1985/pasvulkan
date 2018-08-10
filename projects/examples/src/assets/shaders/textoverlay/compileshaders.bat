@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V textoverlay.vert -o textoverlay_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V textoverlay.frag -o textoverlay_frag.spv
for %%f in (*.spv) do (
  spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
)
copy /y textoverlay_vert.spv ..\..\..\..\assets\textoverlay_vert.spv
copy /y textoverlay_frag.spv ..\..\..\..\assets\textoverlay_frag.spv
del /f /q *.spv
