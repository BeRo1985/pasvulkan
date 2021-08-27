#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

//layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec3 outPosition;

layout (push_constant) uniform PushConstants {
  mat4 viewProjectionMatrix;
} pushConstants;

void main(){
  int vertexID = int(gl_VertexIndex),
      vertexIndex = vertexID % 3,
      faceIndex = vertexID / 3,
      stripVertexID = faceIndex + (((faceIndex & 1) == 0) ? (2 - vertexIndex) : vertexIndex),
       reversed = int(stripVertexID > 6),
       index = (reversed == 1) ? (13 - stripVertexID) : stripVertexID;
   outPosition = (vec3(ivec3(int((index < 3) || (index == 4)), reversed ^ int((index > 0) && (index < 4)), reversed ^ int((index < 2) || (index > 5)))) * 2.0) - vec3(1.0);
   gl_Position = (pushConstants.viewProjectionMatrix * vec4(outPosition, 1.0)).xyww;
}