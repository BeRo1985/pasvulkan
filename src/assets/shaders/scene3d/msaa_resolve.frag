#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_GOOGLE_include_directive : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(set = 0, binding = 0) uniform sampler2DMSArray uTexture;

layout(push_constant) uniform PushConstants { 
  int countSamples; 
} pushConstants;

/* clang-format on */

#include "bidirectional_tonemapping.glsl"
#include "premultiplied_alpha.glsl"

void main() {
  ivec3 position = ivec3(ivec2(gl_FragCoord.xy), int(gl_ViewIndex));
  vec4 color = vec4(0.0);
  int samples = pushConstants.countSamples;
  for (int i = 0; i < samples; i++) {
    color += ApplyToneMapping(texelFetch(uTexture, position, i));
  }
  outColor = ApplyInverseToneMapping(color / samples);   
}