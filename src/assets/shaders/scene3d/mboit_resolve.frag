#version 450 core

#extension GL_EXT_multiview : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

#ifdef MSAA
layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInputMS uSubpassInputOpaque;

layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInputMS uSubpassInputTransparent;

layout(input_attachment_index = 2, set = 0, binding = 2) uniform subpassInputMS uSubpassInputMoments0;
#else
layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInputOpaque;

layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput uSubpassInputTransparent;

layout(input_attachment_index = 2, set = 0, binding = 2) uniform subpassInput uSubpassInputMoments0;
#endif

/* clang-format on */

void main() {
#ifdef MSAA
  vec4 opaque = subpassLoad(uSubpassInputOpaque, gl_SampleID);
  vec4 transparent = subpassLoad(uSubpassInputTransparent, gl_SampleID);
  float b0 = subpassLoad(uSubpassInputMoments0, gl_SampleID).x;
#else
  vec4 opaque = subpassLoad(uSubpassInputOpaque);
  vec4 transparent = subpassLoad(uSubpassInputTransparent);
  float b0 = subpassLoad(uSubpassInputMoments0).x;
#endif

  vec4 color = vec4(0.0);

  if (b0 < 0.00100050033) {
    color = vec4(opaque.xyz, 1.0);
  } else {
    float total_transmittance = exp(-b0);
    if (isinf(b0) || isnan(b0)) {
      total_transmittance = 1e7;
    }
    color = vec4((total_transmittance * opaque.xyz) + (((1.0 - total_transmittance) / transparent.w) * vec3(transparent.xyz)), 0.0);
  }

  outColor = color;

#ifdef MSAA
  // In case of MSAA, a extra resolve pass will generate the final color, together with tone mapping and inverse tone mapping for correct HDR handling,
  // instead to do it in this same shader, for to simplify the complete process.
#endif    

}