#version 450 core

layout (location = 0) in vec3 inUV;
layout (location = 1) in vec3 inColor;

layout (binding = 0) uniform UBO {
	vec2 uFontSize;
} ubo; 

layout (binding = 1) uniform sampler2DArray uSamplerFont;

layout (location = 0) out vec4 outFragColor;

void main(void){
	outFragColor = vec4(vec3(vec3(texture(uSamplerFont, inUV).r) * inColor), 1.0);
}
