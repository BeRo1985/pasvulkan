#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput;

// layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

layout(push_constant) uniform PushConstants {
  int frameCounter;  //
} pushConstants;

void main() {
  float time = 0.0;
  uvec3 x = uvec3(uvec2(gl_FragCoord.xy), uint(pushConstants.frameCounter));
  const uint k = 1103515245u;
  x = ((x >> 8u) ^ x.yzx) * k;
  x = ((x >> 8u) ^ x.yzx) * k;
  x = ((x >> 8u) ^ x.yzx) * k;
  outFragColor = subpassLoad(uSubpassInput) + vec4(((vec3(x) * (1.0 / float(0xffffffffu))) - vec3(0.5)) * (0.375 / 256.0), 0.0);
  // outFragColor = subpassLoad(uSubpassInput) + vec4(vec3(((fract((vec3(dot(vec2(171.0, 231.0), vec2(gl_FragCoord.xy) + vec2(ivec2(int(pushConstants.frameCounter & 0xff)))))) / vec3(103.0, 71.0, 97.0)) - vec3(0.5)) / vec3(255.0)) * 0.375), 0.0);
  // outFragColor = textureLod(uTexture, vec3(inTexCoord, float(gl_ViewIndex)), 0.0);
}
