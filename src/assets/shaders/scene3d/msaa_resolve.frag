#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_GOOGLE_include_directive : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInputMS uSubPassInputMSAA;

layout(push_constant) uniform PushConstants { 
  int countSamples; 
} pushConstants;

/* clang-format on */

#include "bidirectional_tonemapping.glsl"
#include "premultiplied_alpha.glsl"

void main() {
  vec4 color = vec4(0.0);
  int samples = pushConstants.countSamples;
  for (int i = 0; i < samples; i++) {
    color += ApplyToneMapping(subpassLoad(uSubPassInputMSAA, i));
  }
  outColor = ApplyInverseToneMapping(color / float(samples));   
}