@echo off
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.vert -o mesh_vert.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -o mesh_frag.spv
"%VULKAN_SDK%/Bin32/glslangValidator.exe" -V mesh.frag -DALPHATEST -o mesh_masked_frag.spv
for %%f in (*.spv) do (
  rem spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
  rem spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const --strength-reduction --simplify-instructions --remove-duplicates -O %%f -o %%f
)
copy /y mesh_vert.spv ..\..\..\assets\shaders\mesh_vert.spv
copy /y mesh_frag.spv ..\..\..\assets\shaders\mesh_frag.spv
copy /y mesh_masked_frag.spv ..\..\..\assets\shaders\mesh_masked_frag.spv
rem del /f /q *.spv


