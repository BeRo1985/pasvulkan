#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassCurrent;
layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput uSubpassVelocity;
layout(set = 0, binding = 2) uniform sampler2DArray uTexturePrevious;

#include "bidirectional_tonemapping.glsl"

void main(){
  
  vec4 current = ApplyToneMapping(subpassLoad(uSubpassCurrent));
  
  vec2 velocity = subpassLoad(uSubpassVelocity).xy;
  
  vec4 previous = ApplyToneMapping(textureLod(uTexturePrevious, vec3(inTexCoord - velocity, float(gl_ViewIndex)), 0.0));

  float delta = ((current.a * current.a) - (previous.a * previous.a)) * (1.0 / 5.0);
  float weight = clamp(1.0 - (sqrt(delta) * 30.0), 0.0, 1.0) * 0.5;

  outFragColor = ApplyInverseToneMapping(mix(previous, current, weight));
}
