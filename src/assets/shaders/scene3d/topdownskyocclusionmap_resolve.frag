#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_ARB_shader_viewport_layer_array : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out float oOutputZ;

layout(set = 0, binding = 0) uniform sampler2D uTextureDepth;

layout(push_constant) uniform PushConstants { 
  mat4 inverseViewProjectionMatrix; 
} pushConstants;

/* clang-format on */

void main() {
  float depth = textureLod(uTextureDepth, inTexCoord, 0).x;
  vec4 clipSpacePosition = vec4(fma(vec3(inTexCoord, depth), vec2(2.0, 1.0).xxy, vec2(-1.0, 0.0).xxy), 1.0);
  vec4 worldSpacePosition = pushConstants.inverseViewProjectionMatrix * clipSpacePosition;
  oOutputZ = worldSpacePosition.z / worldSpacePosition.w;
}
