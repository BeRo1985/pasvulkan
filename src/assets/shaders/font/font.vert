#version 450 core

layout (location = 0) in vec2 inPosition;
layout (location = 1) in vec2 inTexCoord;
layout (location = 2) in vec4 inColor;

layout (location = 0) out vec2 outPosition;
layout (location = 1) out vec2 outTexCoord;
layout (location = 2) out vec4 outColor;

out gl_PerVertex {
    vec4 gl_Position;   
};

void main(void){
  outPosition = inPosition;
  outTexCoord = inTexCoord;
  outColor = inColor;
  gl_Position = vec4(inPosition, 0.0, 1.0);
}