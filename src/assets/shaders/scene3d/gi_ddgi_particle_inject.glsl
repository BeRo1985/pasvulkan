#ifndef GI_DDGI_PARTICLE_INJECT_GLSL
#define GI_DDGI_PARTICLE_INJECT_GLSL

// =====================================================================================================================
//  Shared particle-LBVH injection for the DDGI producers (ray-query trace + RSM backends + RSM splat).
//
//  Particles are NOT in any hardware ray-tracing BLAS (too many, too dynamic), so each producer software-traces the per-frame
//  GPU-built particle LBVH and injects the result into the probe ray: a closest OPAQUE particle nearer than the current
//  (geometry / VPL) hit OVERRIDES the radiance, shortens the stored hit distance and clears the backface flag (a particle is a
//  facing emitter, not geometry the probe is embedded in); transparent/additive particles add their emission on top, bounded
//  by the closest opaque distance (so the visibility moments see the opaque particle too).
//
//  Prerequisites (the includer must set these up first):
//    - #include "gi_ddgi_pushconstants.glsl" (uses pushConstants.particleBVH device addresses + emissiveGIParticleCount.z)
//    - GL_EXT_buffer_reference / GL_EXT_buffer_reference_uvec2 enabled (the LBVH is reached descriptor-free by device address)
//
//  Does nothing when the alive particle count is 0 (the Pascal side pushes 0 then), so the call is always safe.
// =====================================================================================================================

#include "particle_bvh.glsl"        // ParticleBVHEmitterRef / ParticleBVHNodeRef structs (BDA)
#include "particle_bvh_trace.glsl"  // particleBVHClosestOpaque / particleBVHAdditiveEmission

void ddgiInjectParticles(const in vec3 origin, const in vec3 direction, const in float tMin, const in float tMaxMiss,
                         inout vec3 radiance, inout bool hit, inout bool backface, inout float hitDistance){

  uint particleCount = uint(pushConstants.emissiveGIParticleCount.z);
  if(particleCount == 0u){
    return;
  }

  ParticleBVHEmitterRef particleEmitters = ParticleBVHEmitterRef(pushConstants.particleBVH.xy);
  ParticleBVHNodeRef particleNodes = ParticleBVHNodeRef(pushConstants.particleBVH.zw);

  // Opaque particle: a closer one occludes, replaces the shaded radiance and shortens the stored hit distance (so the
  // visibility moments register it). Bound the search at the current hit (or the miss tMax when the ray gathered nothing).
  float particleBound = hit ? hitDistance : tMaxMiss;
  vec3 opaqueEmission;
  float opaqueT = particleBVHClosestOpaque(particleNodes, particleEmitters, origin, direction, tMin, particleBound, particleCount, opaqueEmission);
  if(opaqueT < particleBound){
    radiance = opaqueEmission;
    hitDistance = opaqueT;
    hit = true;
    backface = false;
    particleBound = opaqueT;
  }

  // Transparent/additive particles add emission without occluding, bounded by the final closest opaque distance.
  radiance += particleBVHAdditiveEmission(particleNodes, particleEmitters, origin, direction, tMin, particleBound, particleCount);
}

#endif // GI_DDGI_PARTICLE_INJECT_GLSL
