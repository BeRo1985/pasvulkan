@echo off
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl -fshader-stage=vertex -o canvas_vert.spv canvas.vert
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl -fshader-stage=fragment -DATLAS_TEXTURE -o canvas_frag_atlas_texture.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl -fshader-stage=fragment -DTEXTURE -o canvas_frag_texture.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl -fshader-stage=fragment -DNO_TEXTURE -o canvas_frag_no_texture.spv canvas.frag

