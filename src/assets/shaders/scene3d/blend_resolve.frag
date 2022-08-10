#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInputOpaque;

#ifdef MSAA
layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInputMS uSubpassInputTransparent;
#else
layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput uSubpassInputTransparent;
#endif

#ifdef MSAA
layout(push_constant) uniform PushConstants { 
  int countSamples; 
} pushConstants;
#endif

/* clang-format on */

#if defined(MSAA)
#include "bidirectional_tonemapping.glsl"
#include "premultiplied_alpha.glsl"
#endif

void blend(inout vec4 target, const in vec4 source) {                  //
  target += (1.0 - target.a) * vec4(source.xyz * source.a, source.a);  //
}

void main() {
  vec4 color = vec4(0.0);

#ifdef MSAA
  vec4 transparency = vec4(0.0);
  int countSamples = pushConstants.countSamples;
  for (int sampleIndex = 0; sampleIndex < countSamples; sampleIndex++) {
    transparency += ApplyToneMapping(subpassLoad(uSubpassInputTransparent, sampleIndex));
  }
  transparency = ApplyInverseToneMapping(transparency / float(countSamples));
#else
  vec4 transparency = subpassLoad(uSubpassInputTransparent);
#endif
  bool hasTransparency = transparency.w > 1e-4;
  blend(color, transparency);

  blend(color, subpassLoad(uSubpassInputOpaque));

  outColor = vec4(color.xyz, hasTransparency ? 0.0 : 1.0);
}