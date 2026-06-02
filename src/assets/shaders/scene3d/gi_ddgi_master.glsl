#ifndef GI_DDGI_MASTER_GLSL
#define GI_DDGI_MASTER_GLSL

// =====================================================================================================================
//  DDGI master (Buffer Device Address) — bindless access to the point-access DDGI sub-buffers.
//
//  One per-in-flight "master" BDA buffer holds device-address pointers to the sub-buffers that are accessed by exact texel
//  (never hardware-filtered) and therefore live in plain storage buffers instead of images: ray-data, probe-data, and the
//  SH-irradiance. Compute passes receive the master via push constant; the fragment consumers (once they read a sub-buffer)
//  via a tiny UBO field. Bilinear-sampled resources (visibility octahedral atlas, OCT-mode irradiance atlas) stay descriptor
//  -bound sampled images and are NOT part of the master.
//
//  The master is grown across the BDA migration phases. Pointers that a phase has not enabled yet are null on the CPU side
//  and untouched by the shaders. The cascade globals + per-frame params are folded in only in the final phase; until then
//  they stay in the UBO / push constant.
//
//  Requires GL_EXT_buffer_reference (+ _uvec2) enabled at the top of the including shader (an #extension here would be
//  mid-file = invalid). All DDGI compute + fragment consumers already enable it (ray-query / USE_MATERIAL_BUFFER_REFERENCE).
// =====================================================================================================================

layout(buffer_reference, std430, buffer_reference_align = 16) buffer DDGIRayDataBuffer { vec4 data[]; };       // rgb = shaded radiance, a = distance (signed for fixed rays); idx = globalProbe*raysPerProbe + ray
layout(buffer_reference, std430, buffer_reference_align = 16) buffer DDGIProbeDataBuffer { vec4 data[]; };     // xyz = relocation offset, w = state; idx = physical probe slot + cascade*probesPerCascade   (phase 3)
layout(buffer_reference, std430, buffer_reference_align = 16) buffer DDGIIrradianceSHBuffer { vec4 data[]; };  // packed RGB SH (SH storage only); DDGI_SH_IMAGE_COUNT vec4 per probe                          (phase 4)

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer DDGIMaster {
  DDGIRayDataBuffer rayData;           // phase 1
  DDGIProbeDataBuffer probeData;       // null until phase 3
  DDGIIrradianceSHBuffer irradianceSH; // null until phase 4
};

// Ray-data linear index: rows of raysPerProbe per probe (matches the old image layout image[y=globalProbe][x=ray]).
uint ddgiRayDataIndex(const in uint globalProbeIndex, const in uint rayIndex, const in uint raysPerProbe){
  return (globalProbeIndex * raysPerProbe) + rayIndex;
}
// The accessors take the sub-buffer reference directly (not the master) so the caller can hoist the single master->sub-pointer
// dereference into a local once per invocation, instead of re-reading it from the master on every call (hot loops).
vec4 ddgiLoadRay(const in DDGIRayDataBuffer aRayData, const in uint globalProbeIndex, const in uint rayIndex, const in uint raysPerProbe){
  return aRayData.data[ddgiRayDataIndex(globalProbeIndex, rayIndex, raysPerProbe)];
}
void ddgiStoreRay(const in DDGIRayDataBuffer aRayData, const in uint globalProbeIndex, const in uint rayIndex, const in uint raysPerProbe, const in vec4 aValue){
  aRayData.data[ddgiRayDataIndex(globalProbeIndex, rayIndex, raysPerProbe)] = aValue;
}

// Probe-data linear index (one vec4 per physical probe slot): matches the old 3D image addressing
// ivec3(probeCoord.xy, probeCoord.z + cascade*GI_DDGI_PROBES_Z) flattened row-major (x fastest).  (phase 3 onward via master)
uint ddgiProbeDataIndex(const in ivec3 probeCoord, const in int cascadeIndex){
  return uint(probeCoord.x + (probeCoord.y * GI_DDGI_PROBES_X) + ((probeCoord.z + (cascadeIndex * GI_DDGI_PROBES_Z)) * GI_DDGI_PROBES_X * GI_DDGI_PROBES_Y));
}
vec4 ddgiLoadProbeDataBuffer(const in DDGIProbeDataBuffer aProbeData, const in ivec3 probeCoord, const in int cascadeIndex){
  return aProbeData.data[ddgiProbeDataIndex(probeCoord, cascadeIndex)];
}
void ddgiStoreProbeDataBuffer(const in DDGIProbeDataBuffer aProbeData, const in ivec3 probeCoord, const in int cascadeIndex, const in vec4 aValue){
  aProbeData.data[ddgiProbeDataIndex(probeCoord, cascadeIndex)] = aValue;
}

#ifdef DDGI_SH_IMAGE_COUNT
// SH-irradiance (SH storage only): DDGI_SH_IMAGE_COUNT packed vec4 per probe, interleaved (probe-major). Index =
// probeLinear * DDGI_SH_IMAGE_COUNT + i. Same per-probe flattening as ddgiProbeDataIndex / the old SH 3D-image texel.
uint ddgiSHBufferIndex(const in ivec3 probeCoord, const in int cascadeIndex, const in int i){
  return (ddgiProbeDataIndex(probeCoord, cascadeIndex) * uint(DDGI_SH_IMAGE_COUNT)) + uint(i);
}
vec4 ddgiLoadSHVec4(const in DDGIIrradianceSHBuffer aSH, const in ivec3 probeCoord, const in int cascadeIndex, const in int i){
  return aSH.data[ddgiSHBufferIndex(probeCoord, cascadeIndex, i)];
}
void ddgiStoreSHVec4(const in DDGIIrradianceSHBuffer aSH, const in ivec3 probeCoord, const in int cascadeIndex, const in int i, const in vec4 aValue){
  aSH.data[ddgiSHBufferIndex(probeCoord, cascadeIndex, i)] = aValue;
}
#endif

#endif // GI_DDGI_MASTER_GLSL
