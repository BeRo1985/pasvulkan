#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec4 inColor;

layout(location = 0) out vec4 outFragColor;

#ifdef VELOCITY
layout(location = 1) out vec2 outVelocity;
#endif

void main(){
  outFragColor = inColor;
#ifdef VELOCITY
  outVelocity = vec2(0.0); // Just no velocity for debug primitives for the sake of simplicity
#endif  
}
