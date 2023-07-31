#version 450 core

#define PARTICLE_FRAGMENT_SHADER

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable

#if defined(LOCKOIT) || defined(DFAOIT)
  #extension GL_ARB_post_depth_coverage : enable
  #ifdef INTERLOCK
    #extension GL_ARB_fragment_shader_interlock : enable
    #define beginInvocationInterlock beginInvocationInterlockARB
    #define endInvocationInterlock endInvocationInterlockARB
    #ifdef MSAA
      layout(early_fragment_tests, post_depth_coverage, sample_interlock_ordered) in;
    #else
      layout(early_fragment_tests, post_depth_coverage, pixel_interlock_ordered) in;
    #endif
  #else
    #if defined(ALPHATEST)
      layout(post_depth_coverage) in;
    #else
      layout(early_fragment_tests, post_depth_coverage) in;
    #endif
  #endif
#elif !defined(ALPHATEST)
  layout(early_fragment_tests) in;
#endif

layout(location = 0) in vec3 inViewSpacePosition;
layout(location = 1) in vec2 inTexCoord;
layout(location = 2) in vec4 inColor;
layout(location = 3) flat in uint inTextureID;

// Specialization constants are sadly unusable due to dead slow shader stage compilation times with several minutes "per" pipeline, 
// when the validation layers and a debugger (GDB, LLDB, etc.) are active at the same time!
#undef USE_SPECIALIZATION_CONSTANTS
#ifdef USE_SPECIALIZATION_CONSTANTS
layout (constant_id = 0) const bool UseReversedZ = true;
#endif

// Global descriptor set

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(std140, set = 0, binding = 0) uniform uboViews {
  View views[256];
} uView;

layout(set = 0, binding = 4) uniform sampler2D u2DTextures[];

///layout(set = 0, binding = 4) uniform samplerCube uCubeTextures[];

#define TRANSPARENCY_DECLARATION
#include "transparency.glsl"
#undef TRANSPARENCY_DECLARATION

/* clang-format on */

#define TRANSPARENCY_GLOBALS
#include "transparency.glsl"
#undef TRANSPARENCY_GLOBALS

void main() {

  bool additiveBlending = (inTextureID & 0x80000000u) != 0; // Reuse the MSB of the texture ID to indicate additive blending

#ifdef DEPTHONLY
#if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || defined(DFAOIT)
  float alpha = (any(lessThan(inTexCoord, vec2(0.0))) || any(greaterThan(inTexCoord, vec2(1.0)))) ? 0.0 : (texture(u2DTextures[nonuniformEXT(((inTextureID & 0x3fff) << 1) | (int(1/*sRGB*/) & 1))], inTexCoord).w * inColor.w);  
#endif
#else
  vec4 finalColor = (any(lessThan(inTexCoord, vec2(0.0))) || any(greaterThan(inTexCoord, vec2(1.0)))) ? vec4(0.0) : (texture(u2DTextures[nonuniformEXT(((inTextureID & 0x3fff) << 1) | (int(1/*sRGB*/) & 1))], inTexCoord) * inColor);
  float alpha = finalColor.w;
#if !(defined(WBOIT) || defined(MBOIT))
#ifndef BLEND 
  outFragColor = finalColor;
#endif
#endif
#endif

#define TRANSPARENCY_IMPLEMENTATION
#include "transparency.glsl"
#undef TRANSPARENCY_IMPLEMENTATION

}

/*oid main() {
  outFragColor = vec4(vec3(mix(0.25, 1.0, max(0.0, dot(workNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
//outFragColor = vec4(texture(uTexture, inTexCoord)) * vec4(vec3(mix(0.25, 1.0, max(0.0, dot(workNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
}*/
