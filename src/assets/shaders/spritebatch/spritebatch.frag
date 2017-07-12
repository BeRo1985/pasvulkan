#version 450 core

layout (location = 0) in vec2 inPosition;
layout (location = 1) in vec3 inTexCoord;
layout (location = 2) in vec4 inColor;

layout (binding = 0) uniform sampler2DArray uTexture;

layout (location = 0) out vec4 outFragColor;

void main(void){
  vec4 color = texture(uTexture, inTexCoord) * inColor; 
  if(color.a < 1e-10){
    discard;
  }
  outFragColor = color;
}