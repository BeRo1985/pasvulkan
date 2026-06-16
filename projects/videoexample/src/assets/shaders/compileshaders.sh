#!/bin/bash
# Compile the videoexample present-path-A composite fragment shader to SPIR-V. The .spv output goes to the RUNTIME
# asset dir (../../../assets/shaders/) and is loaded at runtime via pvApplication.Assets.GetAssetStream
# ('shaders/fwv_composite.frag.spv') in UnitScreenMain (it is NOT embedded into the binary). Run after editing the shader.
cd "$(dirname "$0")"
mkdir -p ../../../assets/shaders
glslc -O --target-env=vulkan -fshader-stage=fragment fwv_composite.frag -o ../../../assets/shaders/fwv_composite.frag.spv
