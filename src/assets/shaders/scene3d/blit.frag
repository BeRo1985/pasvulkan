#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput;

//layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

void main(){
  outFragColor = subpassLoad(uSubpassInput);
//outFragColor = textureLod(uTexture, vec3(inTexCoord, float(gl_ViewIndex)), 0.0);
}
