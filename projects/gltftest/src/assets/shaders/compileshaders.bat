@echo off
wsl.exe ./compileshaders.sh
rem powershell ./compileshaders.ps1
for %%f in (*.spv) do (
  rem "%VULKAN_SDK%/Bin/spirv-opt.exe" --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
  rem "%VULKAN_SDK%/Bin/spirv-opt.exe" --strip-debug --unify-const --flatten-decorations --eliminate-dead-const --strength-reduction --simplify-instructions --remove-duplicates -O %%f -o %%f
)
rem copy /y *.spv ..\..\..\assets\shaders\*.spv
rem del /f /q *.spv


