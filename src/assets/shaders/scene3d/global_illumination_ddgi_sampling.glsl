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
#include "global_illumination_ddgi.glsl"

#if GI_DDGI_STORAGE_IS_SH
  // RGB spherical harmonics packed into DDGI_SH_IMAGE_COUNT RGBA16F 3D textures (L1 = 3, L2 = 7).
  layout(set = DDGI_DESCRIPTOR_SET, binding = 1) uniform sampler3D uDDGIIrradianceSH[DDGI_SH_IMAGE_COUNT];
  DDGI_SH_TYPE ddgiLoadIrradianceSH(const in ivec3 probeCoord, const in int cascadeIndex){
    ivec3 texel = ivec3(probeCoord.xy, probeCoord.z + (cascadeIndex * GI_DDGI_PROBES_Z));
    vec4 a = texelFetch(uDDGIIrradianceSH[0], texel, 0);
    vec4 b = texelFetch(uDDGIIrradianceSH[1], texel, 0);
    vec4 c = texelFetch(uDDGIIrradianceSH[2], texel, 0);
#if GI_DDGI_STORAGE == GI_DDGI_STORAGE_L2_VALUE
    vec4 d = texelFetch(uDDGIIrradianceSH[3], texel, 0);
    vec4 e = texelFetch(uDDGIIrradianceSH[4], texel, 0);
    vec4 f = texelFetch(uDDGIIrradianceSH[5], texel, 0);
    vec4 g = texelFetch(uDDGIIrradianceSH[6], texel, 0);
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

layout(set = DDGI_DESCRIPTOR_SET, binding = 2) uniform sampler2D uDDGIVisibility;
vec3 ddgiSampleVisibility(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 direction){
  vec2 uv = ddgiProbeOctUV(probeCoord, cascadeIndex, direction, GI_DDGI_VISIBILITY_OCT_SIZE, GI_DDGI_VISIBILITY_OCT_FULL);
  return textureLod(uDDGIVisibility, uv, 0.0).rgb; // x = mean dist, y = mean dist^2, z = sky visibility
}

#if GI_DDGI_PROBE_RELOCATION
// Per-probe data (xyz = world-space relocation offset, w = state) written by gi_ddgi_relocation.comp. One texel per probe;
// cascades are stacked along Z like the SH irradiance images.
layout(set = DDGI_DESCRIPTOR_SET, binding = 3) uniform sampler3D uDDGIProbeData;
vec4 ddgiLoadProbeData(const in ivec3 probeCoord, const in int cascadeIndex){
  return texelFetch(uDDGIProbeData, ivec3(probeCoord.xy, probeCoord.z + (cascadeIndex * GI_DDGI_PROBES_Z)), 0);
}
#endif

#endif // GLOBAL_ILLUMINATION_DDGI_SAMPLING_GLSL
