#ifndef PLANET_GRASS_GLSL
#define PLANET_GRASS_GLSL

layout(push_constant) uniform PushConstants {

  mat4 modelMatrix; 

  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint countVerticesPerBladeEdge;
  
  float maximumDistance;
  float grassHeight;
  float grassThickness;
  float time;

  uint tileMapResolution;
  uint tileResolution;  
  uint resolutionXY;
  int frameIndex; 

  vec2 jitter;

} pushConstants;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif
