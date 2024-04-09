#ifndef PLANET_GRASS_GLSL
#define PLANET_GRASS_GLSL

layout(push_constant) uniform PushConstants {

  mat4 modelMatrix; 

  uint viewBaseIndex;
  uint countViews;
  uint countQuadPointsInOneDirection; 
  uint countAllViews;
  
  uint resolutionXY;  
  int frameIndex; 
  vec2 jitter;

} pushConstants;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif
