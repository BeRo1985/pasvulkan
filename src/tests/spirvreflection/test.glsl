#version 450

#pragma shader_stage(fragment)

layout(location = 0) in vec4 inColor;

layout(location = 0) out vec4 outColor;

layout(binding = 0, set = 0) uniform inUniformBuffer {
  mat4 modelViewProjectionMatrix[2];
  vec4 testVector;
} uboGlobals;



void main(){
  outColor = inColor;
}

 
