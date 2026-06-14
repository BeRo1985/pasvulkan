#version 450 core

#extension GL_EXT_multiview : enable

#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

void main(){     
  outColor = textureLod(uTexture, vec3(inTexCoord, float(gl_ViewIndex)), 0.0);
}