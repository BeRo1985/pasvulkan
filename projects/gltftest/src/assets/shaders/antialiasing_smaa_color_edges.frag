#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

vec4 SMAA_RT_METRICS;
#define SMAA_GLSL_4
#define SMAA_PRESET_HIGH
#include "smaa.glsl"

void main(){
     SMAA_RT_METRICS.zw = vec2(textureSize(uTexture, 0).xy);
     SMAA_RT_METRICS.xy = vec2(1.0) / SMAA_RT_METRICS.zw;
     
}
