#ifndef PLANET_TERRAIN_GLSL
#define PLANET_TERRAIN_GLSL

#define MESHLET_K 8u
#define MESHLET_VERT_COUNT ((MESHLET_K + 1u) * (MESHLET_K + 1u))
#define MESHLET_PRIM_COUNT (MESHLET_K * MESHLET_K * 2u)

// raytracingFlags bit 3: meshlet debug colors
#define PLANET_TERRAIN_MESHLET_DEBUG_COLORS_BIT 3u
// raytracingFlags bit 4: enable per-tile frustum culling via visibility bitmap (only set for FinalView passes)
#define PLANET_TERRAIN_FRUSTUM_CULL_BIT 4u

#if defined(USE_BUFFER_REFERENCE)
  #define USE_PLANET_BUFFER_REFERENCE
#endif

#include "planet_data.glsl"

layout(push_constant) uniform PushConstants {

  // First uvec4
  uint viewBaseIndex;
  uint countViews;
  uint countQuadPointsInOneDirection;
  uint countAllViews;

  // Second uvec4
  uint resolutionXY;
  float tessellationFactor;
  uint timeSeconds;
  float timeFractionalSecond;

  // Third uvec4
  int frameIndex;
  uint raytracingFlags;
#if defined(USE_BUFFER_REFERENCE)
  PlanetData planetData;
#else
  uvec2 unusedPlanetData;
#endif

  // Fourth uvec4
  vec4 jitter;

  // Fifth uvec4
  vec4 raytracingOffsetConstants;

} pushConstants;

#if defined(USE_BUFFER_REFERENCE)
PlanetData planetData = pushConstants.planetData;
#endif

#endif // PLANET_TERRAIN_GLSL
