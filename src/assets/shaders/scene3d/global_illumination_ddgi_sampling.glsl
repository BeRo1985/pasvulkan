#ifndef GLOBAL_ILLUMINATION_DDGI_SAMPLING_GLSL
#define GLOBAL_ILLUMINATION_DDGI_SAMPLING_GLSL

// Shared fragment-side DDGI probe-field sampling. Factors out the descriptor-set declarations (UBO + irradiance + visibility)
// and the per-consumer texelFetch loaders that were otherwise duplicated across mesh.frag / planet_renderpass.frag /
// planet_grass.frag / planet_water.frag.
//
// The including shader must, before the #include:
//   - have octahedral.glsl reachable (octEncode, used by ddgiProbeOctUV) — the SH headers are pulled in by
//     global_illumination_ddgi.glsl itself under SH storage,
//   - #define DDGI_DESCRIPTOR_SET to the descriptor-set index the DDGI probe data is bound to (mesh.frag = 2, planets = 4),
//   - only include this in the GLOBAL_ILLUMINATION_DDGI build variant.
//
// (The DDGI compute passes - trace / irradiance update / visibility update - read the probe images as *storage* images via
// imageLoad and therefore keep their own loaders; this include is for the *sampled* fragment-shading consumers only.)

#ifndef DDGI_DESCRIPTOR_SET
  #error "global_illumination_ddgi_sampling.glsl: #define DDGI_DESCRIPTOR_SET (the probe-field descriptor set index) before including."
#endif

#define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET DDGI_DESCRIPTOR_SET
#define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_BINDING 0
#define GLOBAL_ILLUMINATION_DDGI_SAMPLE
#include "global_illumination_ddgi.glsl" // pulls in gi_ddgi_data.glsl -> the `ddgiData` SSBO (cascade globals + sub-buffer pointers) at this set's binding 0

// The DDGI data block — cascade globals + the BDA sub-buffer pointers (probe-data, SH-irradiance, ...) — is the std430 SSBO
// `ddgiData` declared at this set's binding 0 by gi_ddgi_data.glsl (via global_illumination_ddgi.glsl above). The fragment
// reads its globals + the probe-data / SH-irradiance pointers from it directly; no separate master UBO any more (the old
// binding 3 is freed).

#if GI_DDGI_STORAGE_IS_SH
  // RGB spherical harmonics: one contiguous DDGISHProbe (DDGI_SH_IMAGE_COUNT packed vec4) per probe in the master's
  // irradianceSH BDA buffer (no sampler) — loaded as a whole element for coalesced access.
  DDGI_SH_TYPE ddgiLoadIrradianceSH(const in ivec3 probeCoord, const in int cascadeIndex){
    DDGISHProbe p = ddgiData.irradianceSH.probes[ddgiProbeDataIndex(probeCoord, cascadeIndex)];
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
#else
  layout(set = DDGI_DESCRIPTOR_SET, binding = 1) uniform sampler2D uDDGIIrradianceOct;
  vec3 ddgiEvaluateIrradiance(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 normal){
    vec2 uv = ddgiProbeOctUV(probeCoord, cascadeIndex, normal, GI_DDGI_IRRADIANCE_OCT_SIZE, GI_DDGI_IRRADIANCE_OCT_FULL);
    return max(vec3(0.0), textureLod(uDDGIIrradianceOct, uv, 0.0).rgb);
  }
#endif

layout(set = DDGI_DESCRIPTOR_SET, binding = 2) uniform sampler2D uDDGIVisibilityMoments; // x = mean dist, y = mean dist^2 (RG32F)
layout(set = DDGI_DESCRIPTOR_SET, binding = 4) uniform sampler2D uDDGIVisibilitySky;     // x = sky visibility (R8, 0..1)
vec3 ddgiSampleVisibility(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 direction){
  vec2 uv = ddgiProbeOctUV(probeCoord, cascadeIndex, direction, GI_DDGI_VISIBILITY_OCT_SIZE, GI_DDGI_VISIBILITY_OCT_FULL);
  return vec3(textureLod(uDDGIVisibilityMoments, uv, 0.0).xy, textureLod(uDDGIVisibilitySky, uv, 0.0).x); // x = mean dist, y = mean dist^2, z = sky visibility
}

#if GI_DDGI_PROBE_RELOCATION
// Per-probe data (xyz = world-space relocation offset, w = state) lives in the master's probe-data BDA buffer.
vec4 ddgiLoadProbeData(const in ivec3 probeCoord, const in int cascadeIndex){
  return ddgiData.probeData.data[ddgiProbeDataIndex(probeCoord, cascadeIndex)];
}
#endif

#endif // GLOBAL_ILLUMINATION_DDGI_SAMPLING_GLSL
