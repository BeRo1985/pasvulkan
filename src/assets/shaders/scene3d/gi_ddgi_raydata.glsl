#ifndef GI_DDGI_RAYDATA_GLSL
#define GI_DDGI_RAYDATA_GLSL

// =====================================================================================================================
//  DDGI ray-data contract — the single modular seam between any trace PRODUCER (compute + ray-query, SDF tracing, or a
//  ray-generation/closest-hit RT-pipeline producer) and the technique-agnostic CONSUMERS (irradiance/visibility blend,
//  relocation, classification). Mirrors RTXGI's split of ProbeTraceRGS (engine-side, swappable) vs ProbeBlendingCS (SDK).
//
//  Ray-data image layout: image2D [ rayIndex x globalProbeIndex ], RGBA16F:  rgb = radiance towards the probe, a = distance.
//    - rays [0, GI_DDGI_FIXED_RAYS)            FIXED rays (only when relocation is enabled): unrotated directions;
//                                              a = SIGNED RAW distance (negative = backface hit), rgb = 0 (not blended).
//                                              Consumed by the relocation + classification passes.
//    - rays [GI_DDGI_FIXED_RAYS, raysPerProbe) RANDOM rays: per-frame-rotated directions; a = distance, backface-shortened
//                                              and clamped to the local visibility scale; rgb = shaded radiance.
//                                              Consumed by the irradiance + visibility blend.
//
//  Requires global_illumination_ddgi.glsl included first (GI_DDGI_* defines, ddgiSphericalFibonacci, ddgiRayDirection,
//  GI_DDGI_RAY_START, GI_DDGI_VISIBILITY_MAX_DISTANCE_SCALE). Backend-agnostic: encodes from plain hit primitives, so it
//  does not depend on the ray-query gather layer.
// =====================================================================================================================

// Is ray rayIndex a fixed (relocation/classification) ray rather than a random (blended) ray?
bool ddgiRayIsFixed(const in uint rayIndex){
#if GI_DDGI_PROBE_RELOCATION
  return rayIndex < uint(GI_DDGI_FIXED_RAYS);
#else
  return false;
#endif
}

// Split-aware ray direction. Producer AND consumers MUST use this so they agree on directions:
//   fixed rays -> unrotated spherical Fibonacci over the fixed set; random rays -> the per-frame-rotated set.
vec3 ddgiTraceRayDirection(const in uint rayIndex, const in mat3 randomRotation){
#if GI_DDGI_PROBE_RELOCATION
  if(rayIndex < uint(GI_DDGI_FIXED_RAYS)){
    return ddgiSphericalFibonacci(float(rayIndex), float(GI_DDGI_FIXED_RAYS));
  }
#endif
  return ddgiRayDirection(int(rayIndex), randomRotation);
}

// Encode one traced ray into the ray-data image value, applying the fixed-vs-random distance convention above.
//   aShadedRadiance : shaded radiance towards the probe (random rays); ignored for fixed rays.
//   aHit/aBackface/aHitDistance : closest-hit result from any backend; aHit=false => sky/miss.
//   aMissDistance : the distance to store on a miss (typically the ray tMax).
//   aCellSize     : cascade cell size, for the local visibility distance clamp.
vec4 ddgiEncodeRayData(const in uint rayIndex,
                       const in vec3 aShadedRadiance,
                       const in bool aHit,
                       const in bool aBackface,
                       const in float aHitDistance,
                       const in float aMissDistance,
                       const in float aCellSize){
#if GI_DDGI_PROBE_RELOCATION
  if(rayIndex < uint(GI_DDGI_FIXED_RAYS)){
    // Fixed ray: signed (negative = backface), unclamped distance; no radiance (not blended).
    return vec4(0.0, 0.0, 0.0, aHit ? (aBackface ? -aHitDistance : aHitDistance) : aMissDistance);
  }
#endif
  // Random ray: shaded radiance + backface-shortened, locally-clamped distance.
  float dist = aHit ? aHitDistance : aMissDistance;
  if(aHit && aBackface){
    dist *= 0.2; // Majercik: register a thin slab right behind the probe as a near occluder for the Chebyshev test
  }
  dist = min(dist, GI_DDGI_VISIBILITY_MAX_DISTANCE_SCALE * aCellSize);
  return vec4(aShadedRadiance, dist);
}

#endif // GI_DDGI_RAYDATA_GLSL
