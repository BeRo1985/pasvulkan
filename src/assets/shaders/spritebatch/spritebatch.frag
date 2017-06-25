#version 450 core

layout (location = 0) in vec2 inPosition;
layout (location = 1) in vec2 inTexCoord;
layout (location = 2) in vec4 inColor;

layout (binding = 0) uniform sampler2D uTexture;

layout (location = 0) out vec4 outFragColor;

void main(void){
  outFragColor = texture(uTexture, inTexCoord) * inColor;
}
