#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 outFragColor;

layout (set = 0, binding = 0) uniform samplerCube uTexture;

void main(){
   outFragColor = texture(uTexture, normalize(inPosition));
}