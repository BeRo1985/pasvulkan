#ifndef MESH_PUSHCONSTANTS_GLSL
#define MESH_PUSHCONSTANTS_GLSL

layout (push_constant) uniform PushConstants {
  
  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint frameIndex;
  
  vec4 jitter;
  
  uvec4 timeSecondsTimeFractionalSecondWidthHeight; // x = timeSeconds (uint), y = timeFractionalSecond (float), z = width, w = height
  
  uint raytracingFlags;
  uint unused0; // Padding to ensure 16-byte alignment
  uint unused1; // Padding to ensure 16-byte alignment
  uint unused2; // Padding to ensure 16-byte alignment
  
} pushConstants;

#endif