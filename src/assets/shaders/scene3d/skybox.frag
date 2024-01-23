#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 outFragColor;

layout (set = 0, binding = 1) uniform samplerCube uTexture;

#include "skybox.glsl"
#include "env_starlight.glsl"

void main(){
  switch(pushConstants.mode){
    case 1u:{
      // Realtime starlight
      outFragColor = vec4(getStarlight(normalize(inPosition), float(uint(pushConstants.widthHeight >> 16u))) * pushConstants.skyBoxBrightnessFactor, 1.0);
      break;
    }
    default:{
      // Cube map
      outFragColor = texture(uTexture, normalize(inPosition)) * vec2(pushConstants.skyBoxBrightnessFactor, 1.0).xxxy;
      break;
    }
  }
}