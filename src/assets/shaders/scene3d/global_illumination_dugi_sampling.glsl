#ifndef GLOBAL_ILLUMINATION_DUGI_SAMPLING_GLSL
#define GLOBAL_ILLUMINATION_DUGI_SAMPLING_GLSL

// Shared fragment-side DUGI probe-field sampling. Factors out the descriptor-set declarations (UBO + irradiance + visibility)
// and the per-consumer texelFetch loaders that were otherwise duplicated across mesh.frag / planet_renderpass.frag /
// planet_grass.frag / planet_water.frag.
//
// The including shader must, before the #include:
//   - have octahedral.glsl reachable (octEncode, used by dugiProbeOctUV) — the SH headers are pulled in by
//     global_illumination_dugi.glsl itself under SH storage,
//   - #define DUGI_DESCRIPTOR_SET to the descriptor-set index the DUGI probe data is bound to (mesh.frag = 2, planets = 4),
//   - only include this in the GLOBAL_ILLUMINATION_DUGI build variant.
//
// (The DUGI compute passes - trace / irradiance update / visibility update - read the probe images as *storage* images via
// imageLoad and therefore keep their own loaders; this include is for the *sampled* fragment-shading consumers only.)

#ifndef DUGI_DESCRIPTOR_SET
  #error "global_illumination_dugi_sampling.glsl: #define DUGI_DESCRIPTOR_SET (the probe-field descriptor set index) before including."
#endif

#define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET DUGI_DESCRIPTOR_SET
#define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_BINDING 0
#define GLOBAL_ILLUMINATION_DUGI_SAMPLE
#define GI_DUGI_OCT_IRRADIANCE_SCALE 3.14159265358979 // shading sample-time PI of the A = E/PI split, applied after the cage decode (the producers keep 1.0; see global_illumination_dugi.glsl)
#include "global_illumination_dugi.glsl" // pulls in global_illumination_dugi_data.glsl -> the `dugiData` SSBO (cascade globals + sub-buffer pointers) at this set's binding 0

// The DUGI data block — cascade globals + the BDA sub-buffer pointers (probe-data, SH-irradiance, ...) — is the std430 SSBO
// `dugiData` declared at this set's binding 0 by global_illumination_dugi_data.glsl (via global_illumination_dugi.glsl above). The fragment
// reads its globals + the probe-data / SH-irradiance pointers from it directly; no separate master UBO any more (the old
// binding 3 is freed).

#if GI_DUGI_STORAGE_IS_SH

  // RGB spherical harmonics: one contiguous DUGISHProbe (DUGI_SH_IMAGE_COUNT packed vec4) per probe in the master's
  // irradianceSH BDA buffer (no sampler) — loaded as a whole element for coalesced access.
  DUGI_SH_TYPE dugiLoadIrradianceSH(const in ivec3 probeCoord, const in int cascadeIndex){
    DUGISHProbe p = dugiData.irradianceSH.probes[dugiProbeDataIndex(probeCoord, cascadeIndex)];
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

#else

  layout(set = DUGI_DESCRIPTOR_SET, binding = 1) uniform sampler2D uDUGIIrradianceOct;

  vec3 dugiEvaluateIrradiance(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 normal){
    vec2 uv = dugiProbeOctUV(probeCoord, cascadeIndex, normal, GI_DUGI_IRRADIANCE_OCT_SIZE, GI_DUGI_IRRADIANCE_OCT_FULL);
    // The atlas stores the cosine-weighted MEAN incident radiance A = E/PI, perceptually ENCODED (pow(A, 1/GAMMA), see
    // GI_DUGI_IRRADIANCE_ENCODING_GAMMA): return the raw encoded sample so both the hardware bilinear of this fetch and the
    // 8-probe cage weighting interpolate in encoded space; dugiSampleIrradianceInCascade decodes once after its weight
    // normalization and applies the sample-time PI of the A = E/PI split there (GI_DUGI_OCT_IRRADIANCE_SCALE above, like
    // RTXGI; shading then applies albedo/PI). The trace's own multibounce read stays on the A scale (scale 1.0).
    return max(vec3(0.0), textureLod(uDUGIIrradianceOct, uv, 0.0).rgb);
  }

#endif

layout(set = DUGI_DESCRIPTOR_SET, binding = 2) uniform sampler2D uDUGIVisibilityMoments; // x = mean dist, y = mean dist^2 (RG32F)
layout(set = DUGI_DESCRIPTOR_SET, binding = 4) uniform sampler2D uDUGIVisibilitySky;     // x = sky visibility (R8, 0..1)
vec3 dugiSampleVisibility(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 direction){
  vec2 uv = dugiProbeOctUV(probeCoord, cascadeIndex, direction, GI_DUGI_VISIBILITY_OCT_SIZE, GI_DUGI_VISIBILITY_OCT_FULL);
  return vec3(textureLod(uDUGIVisibilityMoments, uv, 0.0).xy, textureLod(uDUGIVisibilitySky, uv, 0.0).x); // x = mean dist, y = mean dist^2, z = sky visibility
}

#if GI_DUGI_PROBE_RELOCATION
// Per-probe data (xyz = world-space relocation offset, w = state) lives in the master's probe-data BDA buffer.
vec4 dugiLoadProbeData(const in ivec3 probeCoord, const in int cascadeIndex){
  return dugiData.probeData.data[dugiProbeDataIndex(probeCoord, cascadeIndex)];
}
#endif

#if defined(GI_DUGI_GLOSSY_RADIANCE)
// Glossy prefiltered-radiance octahedral atlas, binding 5. RGB9E5 (default) is sampled as a uint texture (it is not
// reliably hardware-linear-filterable) and bilinear-filtered manually with a decode per tap; the RGBA16F fallback uses a
// hardware-bilinear sampler. The guard band (filled by global_illumination_dugi_border_update.comp) makes the edge taps correct either way.
#include "rgb9e5.glsl"
#ifdef GI_DUGI_GLOSSY_RGB9E5
layout(set = DUGI_DESCRIPTOR_SET, binding = 5) uniform usampler2D uDUGIGlossyRadiance; // R32_UINT alias of the E5B9G9R9 atlas
#else
layout(set = DUGI_DESCRIPTOR_SET, binding = 5) uniform sampler2D uDUGIGlossyRadiance;  // RGBA16F atlas
#endif
vec3 dugiEvaluateGlossyRadiance(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 reflectionDirection){
  vec2 oct = fma(octEncode(normalize(reflectionDirection)), vec2(0.5), vec2(0.5)); // [-1,1] -> [0,1]
  vec2 originTexel = vec2(dugiProbeTileOrigin(probeCoord, cascadeIndex, GI_DUGI_GLOSSY_OCT_FULL));
  vec2 texel = originTexel + (oct * float(GI_DUGI_GLOSSY_OCT_SIZE));
#ifdef GI_DUGI_GLOSSY_RGB9E5
  vec2 t = texel - vec2(0.5);
  ivec2 base = ivec2(floor(t));
  vec2 f = t - vec2(base);
  vec3 c00 = decodeRGB9E5(texelFetch(uDUGIGlossyRadiance, base + ivec2(0, 0), 0).x);
  vec3 c10 = decodeRGB9E5(texelFetch(uDUGIGlossyRadiance, base + ivec2(1, 0), 0).x);
  vec3 c01 = decodeRGB9E5(texelFetch(uDUGIGlossyRadiance, base + ivec2(0, 1), 0).x);
  vec3 c11 = decodeRGB9E5(texelFetch(uDUGIGlossyRadiance, base + ivec2(1, 1), 0).x);
  return max(vec3(0.0), mix(mix(c00, c10, f.x), mix(c01, c11, f.x), f.y));
#else
  vec2 uv = texel / vec2(dugiAtlasSize(GI_DUGI_GLOSSY_OCT_FULL));
  return max(vec3(0.0), textureLod(uDUGIGlossyRadiance, uv, 0.0).rgb);
#endif
}
#endif

#endif // GLOBAL_ILLUMINATION_DUGI_SAMPLING_GLSL
