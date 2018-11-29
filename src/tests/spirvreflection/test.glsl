#version 450

#pragma shader_stage(fragment)

layout(location = 0) in vec4 inColor;

layout(location = 0) out vec4 outColor;

layout(binding = 0, set = 0) uniform inUniformBuffer {
  mat4 modelViewProjectionMatrix[2];
  vec4 testVector;
} uboGlobals;

layout(push_constant) uniform PushConstants {
  layout(offset = 0) mat4 transformMatrix;
  layout(offset = 64) mat4 fillMatrix;
} pushConstants;

layout(binding = 1, set = 0) buffer inShaderStorageBuffer {
  vec4 testVectors[2];
  mat4 bones[];
} ssboGlobals;

void main(){
  outColor = inColor;
}

 
