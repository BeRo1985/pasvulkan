#version 450 core

// Puts a generated frame together for presentation: the generated picture, which has no HUD, the HUD of the
// rendered frame after it on top with premultiplied alpha, as the content projection does it outside of VR,
// and then the same conversion into the target color space as at the end of every rendered frame, see
// framebuffer_blit.frag. In the debug view, generated frames get a tint and a row of boxes in the upper left
// corner, one per generated frame of the interval, filled up to this one.

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_control_flow_attributes : enable
#extension GL_EXT_samplerless_texture_functions : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(set = 0, binding = 0) uniform texture2D uTextureColor;
layout(set = 0, binding = 1) uniform sampler2D uTextureHUD;

const uint COLOR_SPACE_SRGB_NONLINEAR_SDR = 0u;
const uint COLOR_SPACE_EXTENDED_SRGB_LINEAR = 1u;

const uint FLAG_HUD = 1u;
const uint FLAG_DEBUG = 2u;

layout(push_constant) uniform PushConstants {
  uint colorSpace;
  uint frameCounter; // for the dithering, different for every presented frame
  uint flags;
  uint index; // of the generated frame, 1 .. count
  uint count;
} pushConstants;

#define FRAGMENT_SHADER
#include "srgb.glsl"
#include "dithering.glsl"

void main(){

  ivec2 position = ivec2(gl_FragCoord.xy);

  vec4 color = texelFetch(uTextureColor, position, 0);

  if((pushConstants.flags & FLAG_DEBUG) != 0u){
    color.xyz = mix(color.xyz, vec3(1.0, 0.0, 1.0), 0.25);
    ivec2 boxPosition = position - ivec2(8);
    if(all(greaterThanEqual(boxPosition, ivec2(0))) && (boxPosition.y < 16)){
      uint boxIndex = uint(boxPosition.x / 24);
      if((boxIndex < pushConstants.count) && ((boxPosition.x % 24) < 16)){
        color.xyz = (boxIndex < pushConstants.index) ? vec3(1.0) : vec3(0.1);
      }
    }
  }

  if((pushConstants.flags & FLAG_HUD) != 0u){
    vec4 hud = texture(uTextureHUD, inTexCoord);
    color = (color * (1.0 - hud.w)) + hud; // Pre-multiplied alpha blending
  }

  switch(pushConstants.colorSpace){
    case COLOR_SPACE_EXTENDED_SRGB_LINEAR:{
      // Nothing to do, since the input is already linear sRGB
      break;
    }
    default:{ //case COLOR_SPACE_SRGB_NONLINEAR_SDR:
      color.xyz = clamp(ditherSRGB(clamp(color.xyz, vec3(0.0), vec3(1.0)), position, int(pushConstants.frameCounter)), vec3(0.0), vec3(1.0));
      break;
    }
  }

  outFragColor = color;

}
