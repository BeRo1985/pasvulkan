#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 outFragColor;

layout (set = 0, binding = 1) uniform samplerCube uTexture;

void main(){
#if 0
   outFragColor = vec4(0.25, 0.25, 1.0, 1.0);
#else   
   outFragColor = texture(uTexture, normalize(inPosition));
#endif
}