#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec4 inColor;
layout(location = 1) in float inEdgeDistance;

layout(location = 0) out vec4 outFragColor;


void main(){
  float thickness = 3.0;
  float d = 1.0 - clamp(abs(inEdgeDistance) / thickness, 0.0, 1.0);
  outFragColor = inColor * d;
}
