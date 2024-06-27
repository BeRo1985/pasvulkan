#ifndef PLANET_GRASS_GLSL
#define PLANET_GRASS_GLSL

layout(push_constant) uniform PushConstants {

  mat4 modelMatrix; 

  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint maximalCountBladesPerPatch;
  
  float maximumDistance;
  float grassHeight;
  float grassThickness;
  float time;

  uint tileMapResolution;
  uint tileResolution;  
  uint lod;
  int frameIndex; 

#if defined(MESH_SHADER_EMULATION)
  uint maximalCountTaskIndices;
  uint maximalCountVertices;
  uint maximalCountIndices;
  uint invocationVariants;
#else  
  vec2 jitter;
  uint invocationVariants;
#endif

} pushConstants;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif
