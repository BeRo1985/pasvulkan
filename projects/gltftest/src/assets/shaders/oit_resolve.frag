#version 450 core

#extension GL_EXT_multiview : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInputOpaque;
layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput uSubpassInputTransparent;

void main(){
  vec4 c = subpassLoad(uSubpassInputOpaque);  
  vec4 t = subpassLoad(uSubpassInputTransparent);  
  outColor = vec4(t + ((1.0 - t.a) * c));
}