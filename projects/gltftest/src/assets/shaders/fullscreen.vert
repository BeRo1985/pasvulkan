#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

//layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec2 outTexCoord;

void main(){
  outTexCoord = vec2((gl_VertexIndex >> 1) * 2.0, (gl_VertexIndex & 1) * 2.0);
  gl_Position = vec4(((gl_VertexIndex >> 1) * 4.0) - 1.0, ((gl_VertexIndex & 1) * 4.0) - 1.0, 0.0, 1.0);
}
