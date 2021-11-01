@echo off
powershell ./compileshaders.ps1
for %%f in (*.spv) do (
  rem "%VULKAN_SDK%/Bin32/spirv-opt.exe" --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
  rem "%VULKAN_SDK%/Bin32/spirv-opt.exe" --strip-debug --unify-const --flatten-decorations --eliminate-dead-const --strength-reduction --simplify-instructions --remove-duplicates -O %%f -o %%f
)
copy /y *.spv ..\..\..\assets\shaders\*.spv
del /f /q *.spv


