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

layout(set = 0, binding = 1) uniform sampler2DArray uTextureBloom;

void main(){
  outFragColor = mix(subpassLoad(uSubpassScene), 
                     textureLod(uTextureBloom, vec3(inTexCoord, gl_ViewIndex), 0.0),
                     pushConstants.factor);
}
