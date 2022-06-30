#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_ARB_shader_viewport_layer_array : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 oOutput;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
};

layout(std140, set = 0, binding = 0) uniform uboViews {
  View views[512]; // 65536 / (64 * 2) = 512
} uView;

#ifdef MULTIVIEW
layout(set = 0, binding = 1) uniform sampler2DArray uTextureDepth;
layout(set = 0, binding = 2) uniform sampler2DArray uTextureNormals;
#else
layout(set = 0, binding = 1) uniform sampler2D uTextureDepth;
layout(set = 0, binding = 2) uniform sampler2D uTextureNormals;
#endif

/* clang-format on */

mat4 inverseProjectionMatrix = inverse(uView.views[int(gl_ViewIndex)].projectionMatrix);

float linearizeDepth(float z){  
  vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(fma(z, 2.0, -1.0)), inverseProjectionMatrix[3].zw);
  return v.x / v.y;
}     

vec3 signedOctDecode(vec3 normal) {
  vec2 outNormal;
  outNormal = vec2(normal.xx + vec2(-normal.y, normal.y - 1.0));
  return normalize(vec3(outNormal, fma(normal.z, 2.0, -1.0) * (1.0 - (abs(outNormal.x) + abs(outNormal.y)))));
}

void main() {

}
