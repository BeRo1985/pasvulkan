@echo off
rem Compile the videoexample present-path-A composite fragment shader to SPIR-V. The .spv output goes to the RUNTIME
rem asset dir (..\..\..\assets\shaders\) and is loaded at runtime via pvApplication.Assets.GetAssetStream
rem ('shaders/fwv_composite.frag.spv') in UnitScreenMain (it is NOT embedded into the binary). Run after editing the shader.
if not exist "..\..\..\assets\shaders" mkdir "..\..\..\assets\shaders"
"%VULKAN_SDK%\Bin\glslc.exe" -O --target-env=vulkan -fshader-stage=fragment fwv_composite.frag -o "..\..\..\assets\shaders\fwv_composite.frag.spv"
