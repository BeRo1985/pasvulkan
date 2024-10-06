#ifndef MESH_PUSHCONSTANTS_GLSL
#define MESH_PUSHCONSTANTS_GLSL

layout (push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint frameIndex;
  vec4 jitter;
  uvec4 timeSecondsTimeFractionalSecond; // x = timeSeconds (uint), y = timeFractionalSecond (float), z = unused, w = unused
} pushConstants;

#endif