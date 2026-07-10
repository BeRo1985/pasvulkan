#ifndef GI_DUGI_MULTIBOUNCE_GLSL
#define GI_DUGI_MULTIBOUNCE_GLSL

// =====================================================================================================================
//  Shared previous-frame probe-field reads for the DUGI producers (ray-query trace + RSM backends + RSM splat).
//
//  Read-only views of the irradiance/visibility field this in-flight slot still holds from the PREVIOUS frame — the update
//  stages overwrite these images only AFTER the producer runs — used as the multi-bounce feedback term, plus the per-probe
//  relocation offset. These back the probe-sampling function prototypes declared by global_illumination_dugi.glsl
//  (dugiEvaluateIrradiance / dugiSampleVisibility / dugiLoadProbeData / dugiLoadIrradianceSH), so the in-header
//  dugiSampleIrradiance(...) resolves against them.
//
//  Prerequisites (the includer must set these up first):
//    - GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET defined (the DUGI resource set; reads sit at its binding 2 and 3)
//    - #include "global_illumination_dugi.glsl" (with GLOBAL_ILLUMINATION_DUGI_SAMPLE) BEFORE this file, for the GI_DUGI_*
//      dimension constants, the storage-mode SH aliases, the octahedral addressing helpers and the dugiData SSBO accessors
//    - GL_EXT_buffer_reference enabled (for the SH / probe-data BDA sub-buffers reached through the dugiData master)
//
//  Octahedral storage (GI_DUGI_STORAGE_OCT) reads the irradiance atlas at binding 2; SH storage reads the master's
//  irradianceSH BDA buffer instead (no image binding). Visibility is always the binding-3 mean/mean^2 atlas.
// =====================================================================================================================

#if GI_DUGI_MULTIBOUNCE

#if GI_DUGI_STORAGE_IS_SH
// SH multi-bounce read comes from the master's irradianceSH BDA buffer; dugiLoadIrradianceSH is defined further below.
#else
layout(set = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET, binding = 2, rgba16f) uniform readonly image2D uDUGIIrradianceOctRead;

vec4 dugiEvaluateIrradiance(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 normal){
  vec2 uv = dugiProbeOctUV(probeCoord, cascadeIndex, normal, GI_DUGI_IRRADIANCE_OCT_SIZE, GI_DUGI_IRRADIANCE_OCT_FULL);
  ivec2 texel = ivec2(uv * vec2(dugiAtlasSize(GI_DUGI_IRRADIANCE_OCT_FULL)));
  // Raw perceptually ENCODED atlas value in rgb (pow(A, 1/GAMMA), see GI_DUGI_IRRADIANCE_ENCODING_GAMMA) plus the linear
  // cosine-hemispherical sky fraction in a (unused by the feedback); the cage gather (dugiSampleIrradianceInCascade)
  // decodes the rgb once after its weight normalization. The producers keep the GI_DUGI_OCT_IRRADIANCE_SCALE default of
  // 1.0, so the multi-bounce feedback stays on the stored A = E/PI scale.
  vec4 irradianceSkyFraction = imageLoad(uDUGIIrradianceOctRead, texel);
  return vec4(max(vec3(0.0), irradianceSkyFraction.rgb), irradianceSkyFraction.a);
}
#endif

layout(set = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET, binding = 3, rg32f) uniform readonly image2D uDUGIVisibilityMomentsRead; // x = mean dist, y = mean dist^2

vec3 dugiSampleVisibility(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 direction){
  vec2 uv = dugiProbeOctUV(probeCoord, cascadeIndex, direction, GI_DUGI_VISIBILITY_OCT_SIZE, GI_DUGI_VISIBILITY_OCT_FULL);
  ivec2 texel = ivec2(uv * vec2(dugiAtlasSize(GI_DUGI_VISIBILITY_OCT_FULL))); // point sample is fine for the secondary feedback term
  // The multibounce gather discards the sky-visibility term, so only the distance moments are read here (z = 0); the sky atlas
  // is therefore not bound in the producers, avoiding a clash with the env cubemaps at binding 4.
  return vec3(imageLoad(uDUGIVisibilityMomentsRead, texel).xy, 0.0); // x = mean dist, y = mean dist^2, z = sky (unused here)
}

#endif // GI_DUGI_MULTIBOUNCE

#if GI_DUGI_PROBE_RELOCATION
// Probe data (xyz = relocation offset, w = state) lives in the master's probe-data BDA buffer (written by the relocation/
// classification passes). Used for the relocated probe/ray origin and by the probe-sampling functions (multi-bounce).
vec4 dugiLoadProbeData(const in ivec3 probeCoord, const in int cascadeIndex){
  DUGIProbeDataBuffer pd = dugiData.probeData; // launder through a local (readonly master field -> non-readonly ref) + hoist
  return dugiLoadProbeDataBuffer(pd, probeCoord, cascadeIndex);
}
#endif

#if GI_DUGI_MULTIBOUNCE && GI_DUGI_STORAGE_IS_SH
// Previous-frame SH irradiance (multi-bounce feedback) from the master's irradianceSH BDA buffer (same packing the update
// pass writes; one contiguous load of the whole probe).
DUGI_SH_TYPE dugiLoadIrradianceSH(const in ivec3 probeCoord, const in int cascadeIndex){
  DUGIIrradianceSHBuffer shBuf = dugiData.irradianceSH; // hoist the master->sub-pointer deref once
  DUGISHProbe p = dugiLoadSHProbe(shBuf, probeCoord, cascadeIndex);
  vec4 a = p.c[0]; vec4 b = p.c[1]; vec4 c = p.c[2];
#if GI_DUGI_STORAGE == GI_DUGI_STORAGE_L2_VALUE
  vec4 d = p.c[3]; vec4 e = p.c[4]; vec4 f = p.c[5]; vec4 g = p.c[6];
  return SHC3CoefficientsL2Create(vec3(a.x, a.y, a.z), vec3(a.w, b.x, b.y), vec3(b.z, b.w, c.x), vec3(c.y, c.z, c.w),
                                  vec3(d.x, d.y, d.z), vec3(d.w, e.x, e.y), vec3(e.z, e.w, f.x), vec3(f.y, f.z, f.w),
                                  vec3(g.x, g.y, g.z));
#else
  return SHC3CoefficientsL1Create(vec3(a.x, a.y, a.z), vec3(a.w, b.x, b.y), vec3(b.z, b.w, c.x), vec3(c.y, c.z, c.w));
#endif
}
#endif

#endif // GI_DUGI_MULTIBOUNCE_GLSL
