#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(push_constant) uniform PushConstants {
  float factor;
} pushConstants;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassScene;

layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput uSubpassBloom;

//layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

void main(){
  outFragColor = fma(vec4(subpassLoad(uSubpassBloom)), vec4(pushConstants.factor), vec4(subpassLoad(uSubpassScene)));
}
