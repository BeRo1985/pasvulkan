#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) out vec3 outPosition; // xyz = position, w = cascade index
layout(location = 1) flat out int outCascadeIndex;
layout(location = 2) flat out mat4 outViewProjectionMatrix;

/* clang-format off */

layout(push_constant) uniform PushConstants {
  uint viewBaseIndex; 
  uint countViews;    
  int gridSizeBits;     
  int cascadeIndex;   
} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 0, binding = 0, std140) uniform uboViews {
   View views[256];
} uView;

/* clang-format on */

void main() {
  View view = uView.views[pushConstants.viewBaseIndex + uint(gl_ViewIndex)];
  outPosition = vec3(ivec3((ivec3(int(gl_VertexIndex)) >> ivec3(0, pushConstants.gridSizeBits, pushConstants.gridSizeBits << 1)) & ivec3(int((1 << pushConstants.gridSizeBits) - 1))));
  outCascadeIndex = int(pushConstants.cascadeIndex);
  outViewProjectionMatrix = view.projectionMatrix * view.viewMatrix;
}