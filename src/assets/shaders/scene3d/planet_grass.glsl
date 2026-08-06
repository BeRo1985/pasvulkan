#ifndef PLANET_GRASS_GLSL
#define PLANET_GRASS_GLSL

// The push constants below carry the planet data block by reference, so its type has to be known by the
// time they are declared. The fragment and vertex stages have it already, they go through
// globaldescriptorset.glsl first; the task and mesh stages include this file before anything else, so it
// is pulled in here for them. Both guarded, so whoever got there first stays in charge.
// By reference where the build has buffer references at all (the extension is only requested there), and
// through the descriptor at set 2, binding 1 otherwise, which is where the planet data buffer is bound
// anyway. planet_data.glsl covers both and names the variable planetData either way.
#include "bufferreference_definitions.glsl"
#if defined(USE_BUFFER_REFERENCE)
#define USE_PLANET_BUFFER_REFERENCE
#endif
#include "planet_data.glsl"

layout(push_constant) uniform PushConstants {

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
  uint flags; // bit 0: meshlet debug colors
  int frameIndex; 

  uint timeSeconds; // The current time in seconds
  float timeFractionalSecond; // The current time in fractional seconds
  float previousTime; // Previous time - used by VELOCITY to recalculate previous frame's wind/animation state
  uint raytracingFlags;

  uint maximalCountTaskIndices;
  uint maximalCountVertices;
  uint maximalCountIndices;
  uint invocationVariants;

  vec4 jitter;

  // The planet's own data block, which carries the model matrix this pass used to receive taken apart
  // into a position and a quaternion, plus everything else about the planet - the decal group mask among
  // it. Last in the block, so every group above stays on its sixteen-byte boundary.
#if defined(USE_PLANET_BUFFER_REFERENCE)
  PlanetData planetData;
#else
  uvec2 unusedPlanetData; // Ignored in this case
#endif

} pushConstants;

#if defined(USE_PLANET_BUFFER_REFERENCE)
PlanetData planetData = pushConstants.planetData; // For to avoid changing the code below
#endif

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif
