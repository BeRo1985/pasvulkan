#ifndef GI_DDGI_MULTIBOUNCE_GLSL
#define GI_DDGI_MULTIBOUNCE_GLSL

// =====================================================================================================================
//  Shared previous-frame probe-field reads for the DDGI producers (ray-query trace + RSM backends + RSM splat).
//
//  Read-only views of the irradiance/visibility field this in-flight slot still holds from the PREVIOUS frame — the update
//  stages overwrite these images only AFTER the producer runs — used as the multi-bounce feedback term, plus the per-probe
//  relocation offset. These back the probe-sampling function prototypes declared by global_illumination_ddgi.glsl
//  (ddgiEvaluateIrradiance / ddgiSampleVisibility / ddgiLoadProbeData / ddgiLoadIrradianceSH), so the in-header
//  ddgiSampleIrradiance(...) resolves against them.
//
//  Prerequisites (the includer must set these up first):
//    - GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET defined (the DDGI resource set; reads sit at its binding 2 and 3)
//    - #include "global_illumination_ddgi.glsl" (with GLOBAL_ILLUMINATION_DDGI_SAMPLE) BEFORE this file, for the GI_DDGI_*
//      dimension constants, the storage-mode SH aliases, the octahedral addressing helpers and the ddgiData SSBO accessors
//    - GL_EXT_buffer_reference enabled (for the SH / probe-data BDA sub-buffers reached through the ddgiData master)
//
//  Octahedral storage (GI_DDGI_STORAGE_OCT) reads the irradiance atlas at binding 2; SH storage reads the master's
//  irradianceSH BDA buffer instead (no image binding). Visibility is always the binding-3 mean/mean^2 atlas.
// =====================================================================================================================

#if GI_DDGI_MULTIBOUNCE

#if GI_DDGI_STORAGE_IS_SH
// SH multi-bounce read comes from the master's irradianceSH BDA buffer; ddgiLoadIrradianceSH is defined further below.
#else
layout(set = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET, binding = 2, rgba16f) uniform readonly image2D uDDGIIrradianceOctRead;

vec3 ddgiEvaluateIrradiance(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 normal){
  vec2 uv = ddgiProbeOctUV(probeCoord, cascadeIndex, normal, GI_DDGI_IRRADIANCE_OCT_SIZE, GI_DDGI_IRRADIANCE_OCT_FULL);
  ivec2 texel = ivec2(uv * vec2(ddgiAtlasSize(GI_DDGI_IRRADIANCE_OCT_FULL)));
  return max(vec3(0.0), imageLoad(uDDGIIrradianceOctRead, texel).rgb);
}
#endif

layout(set = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET, binding = 3, rg32f) uniform readonly image2D uDDGIVisibilityMomentsRead; // x = mean dist, y = mean dist^2

vec3 ddgiSampleVisibility(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 direction){
  vec2 uv = ddgiProbeOctUV(probeCoord, cascadeIndex, direction, GI_DDGI_VISIBILITY_OCT_SIZE, GI_DDGI_VISIBILITY_OCT_FULL);
  ivec2 texel = ivec2(uv * vec2(ddgiAtlasSize(GI_DDGI_VISIBILITY_OCT_FULL))); // point sample is fine for the secondary feedback term
  // The multibounce gather discards the sky-visibility term, so only the distance moments are read here (z = 0); the sky atlas
  // is therefore not bound in the producers, avoiding a clash with the env cubemaps at binding 4.
  return vec3(imageLoad(uDDGIVisibilityMomentsRead, texel).xy, 0.0); // x = mean dist, y = mean dist^2, z = sky (unused here)
}

#endif // GI_DDGI_MULTIBOUNCE

#if GI_DDGI_PROBE_RELOCATION
// Probe data (xyz = relocation offset, w = state) lives in the master's probe-data BDA buffer (written by the relocation/
// classification passes). Used for the relocated probe/ray origin and by the probe-sampling functions (multi-bounce).
vec4 ddgiLoadProbeData(const in ivec3 probeCoord, const in int cascadeIndex){
  DDGIProbeDataBuffer pd = ddgiData.probeData; // launder through a local (readonly master field -> non-readonly ref) + hoist
  return ddgiLoadProbeDataBuffer(pd, probeCoord, cascadeIndex);
}
#endif

#if GI_DDGI_MULTIBOUNCE && GI_DDGI_STORAGE_IS_SH
// Previous-frame SH irradiance (multi-bounce feedback) from the master's irradianceSH BDA buffer (same packing the update
// pass writes; one contiguous load of the whole probe).
DDGI_SH_TYPE ddgiLoadIrradianceSH(const in ivec3 probeCoord, const in int cascadeIndex){
  DDGIIrradianceSHBuffer shBuf = ddgiData.irradianceSH; // hoist the master->sub-pointer deref once
  DDGISHProbe p = ddgiLoadSHProbe(shBuf, probeCoord, cascadeIndex);
  vec4 a = p.c[0]; vec4 b = p.c[1]; vec4 c = p.c[2];
#if GI_DDGI_STORAGE == GI_DDGI_STORAGE_L2_VALUE
  vec4 d = p.c[3]; vec4 e = p.c[4]; vec4 f = p.c[5]; vec4 g = p.c[6];
  return SHC3CoefficientsL2Create(vec3(a.x, a.y, a.z), vec3(a.w, b.x, b.y), vec3(b.z, b.w, c.x), vec3(c.y, c.z, c.w),
                                  vec3(d.x, d.y, d.z), vec3(d.w, e.x, e.y), vec3(e.z, e.w, f.x), vec3(f.y, f.z, f.w),
                                  vec3(g.x, g.y, g.z));
#else
  return SHC3CoefficientsL1Create(vec3(a.x, a.y, a.z), vec3(a.w, b.x, b.y), vec3(b.z, b.w, c.x), vec3(c.y, c.z, c.w));
#endif
}
#endif

#endif // GI_DDGI_MULTIBOUNCE_GLSL
