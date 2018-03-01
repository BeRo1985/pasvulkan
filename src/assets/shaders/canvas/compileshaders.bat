@echo off
set FILLTYPE_NO_TEXTURE=0
set FILLTYPE_TEXTURE=1
set FILLTYPE_ATLAS_TEXTURE=2

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=vertex -DUSECLIPDISTANCE=0 -o canvas_vert.spv canvas.vert

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -o canvas_frag_no_texture.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -o canvas_frag_texture.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_ATLAS_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -o canvas_frag_atlas_texture.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -DGUI_ELEMENTS -o canvas_frag_gui_no_texture.spv canvas.frag

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=0 -o canvas_frag_no_texture_no_blending.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=0 -o canvas_frag_texture_no_blending.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_ATLAS_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=0 -o canvas_frag_atlas_texture_no_blending.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=0 -DGUI_ELEMENTS -o canvas_frag_gui_no_texture_no_blending.spv canvas.frag

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -o canvas_frag_no_texture_no_blending_no_discard.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -o canvas_frag_texture_no_blending_no_discard.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_ATLAS_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -o canvas_frag_atlas_texture_no_blending_no_discard.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=0 -DUSENODISCARD=1 -DGUI_ELEMENTS -o canvas_frag_gui_no_texture_no_blending_no_discard.spv canvas.frag

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=vertex -DUSECLIPDISTANCE=1 -o canvas_vert_clip_distance.spv canvas.vert

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -o canvas_frag_no_texture_clip_distance.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -o canvas_frag_texture_clip_distance.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_ATLAS_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -o canvas_frag_atlas_texture_clip_distance.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=1 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -DGUI_ELEMENTS -o canvas_frag_gui_no_texture_clip_distance.spv canvas.frag

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=0 -o canvas_frag_no_texture_no_blending_clip_distance.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=0 -o canvas_frag_texture_no_blending_clip_distance.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_ATLAS_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=0 -o canvas_frag_atlas_texture_no_blending_clip_distance.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=0 -DGUI_ELEMENTS -o canvas_frag_gui_no_texture_no_blending_clip_distance.spv canvas.frag

"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -o canvas_frag_no_texture_no_blending_clip_distance_no_discard.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -o canvas_frag_texture_no_blending_clip_distance_no_discard.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_ATLAS_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -o canvas_frag_atlas_texture_no_blending_clip_distance_no_discard.spv canvas.frag
"%VULKAN_SDK%/Bin32/glslc.exe" -x glsl --target-env=vulkan -fshader-stage=fragment -DFILLTYPE=%FILLTYPE_NO_TEXTURE% -DBLENDING=0 -DUSECLIPDISTANCE=1 -DUSENODISCARD=1 -DGUI_ELEMENTS -o canvas_frag_gui_no_texture_no_blending_clip_distance_no_discard.spv canvas.frag

for %%f in (*.spv) do (
  spirv-opt --strip-debug --unify-const --flatten-decorations --eliminate-dead-const %%f -o %%f
)








