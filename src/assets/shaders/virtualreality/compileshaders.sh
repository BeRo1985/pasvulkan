#!/bin/bash

# Compiles the shaders used by the PasVulkan.ScreenGUIBase virtual-reality content-projection screen.
# The resulting SPIR-V modules stay in this directory and are embedded into PasVulkanAssets.inc by ../../convert.dpr.

glslangValidator -V fullscreen.vert -o fullscreen_vert.spv
glslangValidator -V contentprojection.frag -o contentprojection_frag.spv
glslangValidator -V blit.frag -o blit_frag.spv
