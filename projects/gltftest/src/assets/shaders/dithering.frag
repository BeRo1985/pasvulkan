#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput;

//layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

layout (push_constant) uniform PushConstants {
  int frameCounter;
} pushConstants;

void main(){
  float time = 0.0; 
  outFragColor = subpassLoad(uSubpassInput) + vec4(vec3(((fract((vec3(dot(vec2(171.0, 231.0), vec2(gl_FragCoord.xy) + vec2(ivec2(int(pushConstants.frameCounter & 0xff)))))) / vec3(103.0, 71.0, 97.0)) - vec3(0.5)) / vec3(255.0)) * 0.375), 0.0);
//outFragColor = textureLod(uTexture, vec3(inTexCoord, float(gl_ViewIndex)), 0.0);
}
