#ifndef GLOBAL_ILLUMINATION_DDGI_GLSL
#define GLOBAL_ILLUMINATION_DDGI_GLSL

// =====================================================================================================================
//  Dynamic Diffuse Global Illumination (DDGI) - shared probe field definitions, addressing and sampling.
//
//  Based on:
//    - "Dynamic Diffuse Global Illumination with Ray-Traced Irradiance Fields", Majercik, Guertin, Nowrouzezahrai,
//      McGuire, JCGT 2019. https://jcgt.org/published/0008/02/01/
//    - "Scaling Probe-Based Real-Time Dynamic Global Illumination for Production", Majercik et al. 2021.
//
//  This engine variant reuses the cascaded radiance hints snapping infrastructure for probe placement: instead of one
//  irradiance volume, we keep GI_DDGI_CASCADES nested probe grids that snap to the camera, so a small per-cascade probe
//  count covers both near and far field. Each probe stores:
//    - irradiance, either as L1 spherical harmonics in a 3D volume (GI_DDGI_STORAGE_SH, default) or as an octahedral
//      irradiance tile in a 2D atlas (GI_DDGI_STORAGE_OCT) - switchable via the GI_DDGI_STORAGE define.
//    - visibility, always as an octahedral mean / mean-squared distance tile in a 2D atlas, used for the Chebyshev
//      visibility test that prevents the light leaking that plain irradiance volumes (and radiance hints) suffer from.
//
//  The probe radiance is gathered by tracing rays against the scene TLAS; see gi_ddgi_trace.comp / gi_ddgi_probe_update.comp.
// =====================================================================================================================

#include "octahedral.glsl" // octEncode / octDecode (unit vector <-> [-1,1]^2 signed octahedral mapping)

// --- Storage mode -----------------------------------------------------------------------------------------------------
#define GI_DDGI_STORAGE_SH_VALUE 0
#define GI_DDGI_STORAGE_OCT_VALUE 1
#ifndef GI_DDGI_STORAGE
  #define GI_DDGI_STORAGE GI_DDGI_STORAGE_SH_VALUE
#endif

// Convenience define mirroring GI_DDGI_STORAGE for consumers that select via defined()/!defined() (e.g. mesh.frag's
// IBL block, which is kept for octahedral storage but replaced by the SH dominant-light path for SH storage).
#if GI_DDGI_STORAGE == GI_DDGI_STORAGE_OCT_VALUE
  #define GLOBAL_ILLUMINATION_DDGI_OCT_STORAGE
#endif

// --- Probe field dimensions -------------------------------------------------------------------------------------------
#ifndef GI_DDGI_CASCADES
  #define GI_DDGI_CASCADES 4
#endif
#ifndef GI_DDGI_PROBES_X
  #define GI_DDGI_PROBES_X 16
#endif
#ifndef GI_DDGI_PROBES_Y
  #define GI_DDGI_PROBES_Y 16
#endif
#ifndef GI_DDGI_PROBES_Z
  #define GI_DDGI_PROBES_Z 16
#endif
#define GI_DDGI_PROBES_PER_CASCADE (GI_DDGI_PROBES_X * GI_DDGI_PROBES_Y * GI_DDGI_PROBES_Z)

// Octahedral tile sizes (interior texels; one guard-band texel is added on each side in the atlas for bilinear filtering).
#ifndef GI_DDGI_IRRADIANCE_OCT_SIZE
  #define GI_DDGI_IRRADIANCE_OCT_SIZE 8
#endif
#ifndef GI_DDGI_VISIBILITY_OCT_SIZE
  #define GI_DDGI_VISIBILITY_OCT_SIZE 16
#endif
#define GI_DDGI_IRRADIANCE_OCT_FULL (GI_DDGI_IRRADIANCE_OCT_SIZE + 2)
#define GI_DDGI_VISIBILITY_OCT_FULL (GI_DDGI_VISIBILITY_OCT_SIZE + 2)

// Number of rays traced per probe per frame.
#ifndef GI_DDGI_RAYS_PER_PROBE
  #define GI_DDGI_RAYS_PER_PROBE 128
#endif

// Temporal blend hysteresis when integrating new ray results into the stored probe data (closer to 1 = more stable / slower).
#ifndef GI_DDGI_HYSTERESIS
  #define GI_DDGI_HYSTERESIS 0.97
#endif

// Sharpness exponent applied to the Chebyshev weight; higher values darken leaking transitions more aggressively.
#ifndef GI_DDGI_VISIBILITY_SHARPNESS
  #define GI_DDGI_VISIBILITY_SHARPNESS 8.0
#endif

const ivec3 uDDGIProbeCounts = ivec3(GI_DDGI_PROBES_X, GI_DDGI_PROBES_Y, GI_DDGI_PROBES_Z);

// --- Uniform data -----------------------------------------------------------------------------------------------------
// Mirrors the cascaded radiance hints volume uniform layout (one entry per cascade) so the CPU-side snapping code can be
// shared. AABBMin/Max/Scale/Center are the probe grid bounds in world space; the probes sit on the grid lattice spanning
// the AABB, i.e. probe (i,j,k) is at AABBMin + (i,j,k) * cellSize, with cellSize = (AABBMax-AABBMin)/(probeCounts-1).
#ifdef GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET
layout(set = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET, binding = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_BINDING, std140) uniform uboGlobalIlluminationDDGIData {
  vec4 ddgiCascadeAABBMin[GI_DDGI_CASCADES];        // xyz = world space min corner of the probe lattice
  vec4 ddgiCascadeAABBMax[GI_DDGI_CASCADES];        // xyz = world space max corner of the probe lattice
  vec4 ddgiCascadeAABBScale[GI_DDGI_CASCADES];      // xyz = 1.0 / (max - min)
  vec4 ddgiCascadeCellSizes[GI_DDGI_CASCADES];      // xyz = world space spacing between adjacent probes, w = max probe ray distance
  vec4 ddgiCascadeAABBCenter[GI_DDGI_CASCADES];     // xyz = AABB center (for cascade fade computation)
  vec4 ddgiCascadeAABBFadeStart[GI_DDGI_CASCADES];  // xyz = distance from center where this cascade begins to fade out
  vec4 ddgiCascadeAABBFadeEnd[GI_DDGI_CASCADES];    // xyz = distance from center where this cascade is fully faded out
  ivec4 ddgiCascadeProbeScroll[GI_DDGI_CASCADES];   // xyz = toroidal scroll offset in probe units, w = valid flag
} ddgiData;
#endif

// =====================================================================================================================
//  Addressing helpers
// =====================================================================================================================

// World position -> continuous probe-grid coordinate within a cascade (0..probeCounts-1 spans the AABB).
#ifdef GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET
vec3 ddgiWorldToProbeGrid(const in vec3 worldPosition, const in int cascadeIndex){
  vec3 normalized = (worldPosition - ddgiData.ddgiCascadeAABBMin[cascadeIndex].xyz) * ddgiData.ddgiCascadeAABBScale[cascadeIndex].xyz;
  return normalized * vec3(uDDGIProbeCounts - ivec3(1));
}

vec3 ddgiProbeGridToWorld(const in ivec3 probeCoord, const in int cascadeIndex){
  vec3 t = vec3(probeCoord) / vec3(uDDGIProbeCounts - ivec3(1));
  return mix(ddgiData.ddgiCascadeAABBMin[cascadeIndex].xyz, ddgiData.ddgiCascadeAABBMax[cascadeIndex].xyz, t);
}
#endif

// Linear probe index within a cascade from integer probe coordinates.
int ddgiProbeIndex(const in ivec3 probeCoord){
  return (((probeCoord.z * GI_DDGI_PROBES_Y) + probeCoord.y) * GI_DDGI_PROBES_X) + probeCoord.x;
}

// Inverse of ddgiProbeIndex: integer probe coordinates from a linear index within a cascade.
ivec3 ddgiProbeCoordFromIndex(const in int probeIndex){
  int x = probeIndex % GI_DDGI_PROBES_X;
  int y = (probeIndex / GI_DDGI_PROBES_X) % GI_DDGI_PROBES_Y;
  int z = probeIndex / (GI_DDGI_PROBES_X * GI_DDGI_PROBES_Y);
  return ivec3(x, y, z);
}

// Evenly distributed direction on the unit sphere (spherical Fibonacci / golden spiral) for ray index i of n.
vec3 ddgiSphericalFibonacci(const in float i, const in float n){
  const float PHI = 1.6180339887498949; // golden ratio
  float phi = 6.2831853071795864 * fract(i * (PHI - 1.0));
  float cosTheta = 1.0 - ((2.0 * i) + 1.0) * (1.0 / n);
  float sinTheta = sqrt(clamp(1.0 - (cosTheta * cosTheta), 0.0, 1.0));
  return vec3(cos(phi) * sinTheta, sin(phi) * sinTheta, cosTheta);
}

// The traced direction for a given ray index, rotated by a per-frame random rotation so that, over several frames, the
// whole sphere is covered while only GI_DDGI_RAYS_PER_PROBE rays are traced per frame. Both the trace and update shaders
// call this with the same rotation (passed as a push constant) so they agree on the directions without storing them.
vec3 ddgiRayDirection(const in int rayIndex, const in mat3 randomRotation){
  return normalize(randomRotation * ddgiSphericalFibonacci(float(rayIndex), float(GI_DDGI_RAYS_PER_PROBE)));
}

// Octahedral atlases pack the probes of a cascade row-major into a 2D grid of tiles. We lay out all cascades vertically
// (one cascade block per GI_DDGI_PROBES_Z*... rows) so a single 2D texture array layer or a tall 2D texture can hold them.
// tilesPerRow chosen as GI_DDGI_PROBES_X * GI_DDGI_PROBES_Y wide is wasteful; instead we use a square-ish layout.
const int GI_DDGI_TILES_PER_ROW = GI_DDGI_PROBES_X; // one row of the atlas holds one X-row of probes

// Top-left interior texel (in full-tile units, i.e. including guard band) of a probe tile inside the atlas for a given
// per-probe full tile size.
ivec2 ddgiProbeTileOrigin(const in ivec3 probeCoord, const in int cascadeIndex, const in int fullTileSize){
  // Atlas grid coordinate of the tile: x advances with probe.x, y advances with probe.y then probe.z then cascade.
  int tileX = probeCoord.x;
  int tileY = probeCoord.y + (GI_DDGI_PROBES_Y * (probeCoord.z + (GI_DDGI_PROBES_Z * cascadeIndex)));
  return (ivec2(tileX, tileY) * fullTileSize) + ivec2(1); // +1 to skip the guard-band texel
}

// Atlas dimensions in texels for a given per-probe full tile size.
ivec2 ddgiAtlasSize(const in int fullTileSize){
  return ivec2(GI_DDGI_PROBES_X, GI_DDGI_PROBES_Y * GI_DDGI_PROBES_Z * GI_DDGI_CASCADES) * fullTileSize;
}

// Normalized [0,1] atlas UV for a direction in a probe's octahedral tile (for sampling with a linear sampler; the guard
// band makes bilinear taps at tile edges correct).
vec2 ddgiProbeOctUV(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 direction, const in int interiorSize, const in int fullTileSize){
  vec2 oct = fma(octEncode(normalize(direction)), vec2(0.5), vec2(0.5)); // [-1,1] -> [0,1]
  vec2 originTexel = vec2(ddgiProbeTileOrigin(probeCoord, cascadeIndex, fullTileSize));
  vec2 texel = originTexel + (oct * float(interiorSize));
  return texel / vec2(ddgiAtlasSize(fullTileSize));
}

// =====================================================================================================================
//  Probe data declarations and sampling (only when sampling, i.e. in mesh.frag or the probe update shader)
// =====================================================================================================================
#ifdef GLOBAL_ILLUMINATION_DDGI_SAMPLE

  // Irradiance storage.
  #if GI_DDGI_STORAGE == GI_DDGI_STORAGE_SH_VALUE
    // L1 RGB spherical harmonics packed into 3 RGBA16F 3D textures per cascade.
    //   tex0 = (c0.rgb, c1.r), tex1 = (c1.gb, c2.rg), tex2 = (c2.b, c3.rgb)
    // The 3D texture coordinate addresses the probe lattice (size = probe counts, with the cascade stacked along Z).
    #include "sphericalharmonics.glsl"

    // Defined by each consumer against its own resources: the probe update shader loads from a storage image, the
    // shading pass loads from a sampled texture. Returns the stored *radiance* L1 SH of the probe.
    SHC3CoefficientsL1 ddgiLoadIrradianceSH(const in ivec3 probeCoord, const in int cascadeIndex);

    // Evaluate the diffuse irradiance E(n) for a normal direction: convolve the stored radiance SH with the clamped
    // cosine lobe and evaluate it in the normal direction. The caller multiplies by albedo/PI to get outgoing radiance.
    vec3 ddgiEvaluateIrradiance(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 normal){
      SHC3CoefficientsL1 sh = SHC3CoefficientsL1ConvolveWithCosineLobe(ddgiLoadIrradianceSH(probeCoord, cascadeIndex));
      return max(vec3(0.0), EvaluateSHC3CoefficientsL1(sh, normalize(normal)));
    }
  #else
    // Octahedral irradiance atlas (RGBA16F).
    vec3 ddgiEvaluateIrradiance(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 normal);
  #endif

  // Visibility octahedral atlas (RGBA16F): x = mean distance, y = mean distance squared (Chebyshev), z = sky visibility
  // (fraction of probe rays in that direction that escaped to the sky / missed geometry, used as the IBL occlusion factor).
  vec3 ddgiSampleVisibility(const in ivec3 probeCoord, const in int cascadeIndex, const in vec3 direction);

  // ---------------------------------------------------------------------------------------------------------------------
  //  Sample the irradiance field at a world position for a surface with the given normal, with Chebyshev visibility
  //  weighting (the DDGI leak-reduction term) and trilinear + backface weighting. Returns diffuse irradiance.
  // ---------------------------------------------------------------------------------------------------------------------
  vec3 ddgiSampleIrradianceInCascade(const in vec3 worldPosition, const in vec3 normal, const in vec3 viewDirection, const in int cascadeIndex, out float skyVisibility){
    vec3 gridCoord = ddgiWorldToProbeGrid(worldPosition, cascadeIndex);
    ivec3 baseProbe = ivec3(floor(gridCoord));
    vec3 frac = gridCoord - vec3(baseProbe);

    // Bias the sample position slightly along the normal and away from the view to reduce self-shadowing of the probes.
    vec3 biasedPosition = worldPosition + (normal * 0.0) + (viewDirection * 0.0);

    vec3 sumIrradiance = vec3(0.0);
    float sumSkyVisibility = 0.0;
    float sumWeight = 0.0;

    for(int i = 0; i < 8; i++){
      ivec3 offset = ivec3(i & 1, (i >> 1) & 1, (i >> 2) & 1);
      ivec3 probeCoord = clamp(baseProbe + offset, ivec3(0), uDDGIProbeCounts - ivec3(1));

      vec3 trilinear = mix(vec3(1.0) - frac, frac, vec3(offset));
      float weight = trilinear.x * trilinear.y * trilinear.z;

      vec3 probeWorld = ddgiProbeGridToWorld(probeCoord, cascadeIndex);
      vec3 probeToPoint = biasedPosition - probeWorld;
      vec3 dirToProbe = normalize(-probeToPoint);

      // Backface / smooth wrap weight: probes "behind" the surface contribute less.
      float wrap = (dot(dirToProbe, normal) + 1.0) * 0.5;
      weight *= (wrap * wrap) + 0.2;

      // Chebyshev visibility test against the probe's stored octahedral depth statistics.
      float distToProbe = length(probeToPoint);
      vec3 vis = ddgiSampleVisibility(probeCoord, cascadeIndex, normalize(probeToPoint));
      vec2 moments = vis.xy;
      float meanDist = moments.x;
      float chebyshev = 1.0;
      if(distToProbe > meanDist){
        float variance = abs((meanDist * meanDist) - moments.y);
        float d = distToProbe - meanDist;
        chebyshev = variance / (variance + (d * d));
        chebyshev = max(0.0, chebyshev * chebyshev * chebyshev); // sharpen
      }
      weight *= chebyshev;

      // Avoid zero contribution everywhere by keeping a tiny epsilon, then apply a small power to crush near-zero weights.
      const float crushThreshold = 0.2;
      if(weight < crushThreshold){
        weight *= (weight * weight) * (1.0 / (crushThreshold * crushThreshold));
      }

      weight = max(weight, 1e-6);

      sumIrradiance += ddgiEvaluateIrradiance(probeCoord, cascadeIndex, normal) * weight;
      // Sky visibility for IBL occlusion: how open the surface hemisphere (normal direction) is to the sky at this probe.
      sumSkyVisibility += ddgiSampleVisibility(probeCoord, cascadeIndex, normal).z * weight;
      sumWeight += weight;
    }

    skyVisibility = (sumWeight > 0.0) ? clamp(sumSkyVisibility / sumWeight, 0.0, 1.0) : 0.0;
    return (sumWeight > 0.0) ? (sumIrradiance / sumWeight) : vec3(0.0);
  }

  // Select cascade by AABB containment with fade-based blending between cascades, then sample. Returns diffuse irradiance;
  // skyVisibility (out) is the IBL occlusion factor (1 = fully open to the sky, 0 = enclosed), 1 outside all cascades.
  vec3 ddgiSampleIrradiance(const in vec3 worldPosition, const in vec3 normal, const in vec3 viewDirection, out float skyVisibility){
    int cascadeIndex = 0;
    while(((cascadeIndex + 1) < GI_DDGI_CASCADES) &&
          (any(lessThan(worldPosition, ddgiData.ddgiCascadeAABBMin[cascadeIndex].xyz)) ||
           any(greaterThan(worldPosition, ddgiData.ddgiCascadeAABBMax[cascadeIndex].xyz)))){
      cascadeIndex++;
    }

    vec3 result = vec3(0.0);
    float sumSkyVisibility = 0.0;
    float sumWeight = 0.0;
    float current = 1.0;
    for(int c = cascadeIndex; c < GI_DDGI_CASCADES; c++){
      float weight;
      if(c == (GI_DDGI_CASCADES - 1)){
        weight = current;
        current = 0.0;
      }else if(all(greaterThanEqual(worldPosition, ddgiData.ddgiCascadeAABBMin[c].xyz)) &&
               all(lessThanEqual(worldPosition, ddgiData.ddgiCascadeAABBMax[c].xyz))){
        vec3 fade = smoothstep(ddgiData.ddgiCascadeAABBFadeStart[c].xyz,
                               ddgiData.ddgiCascadeAABBFadeEnd[c].xyz,
                               abs(worldPosition - ddgiData.ddgiCascadeAABBCenter[c].xyz));
        float f = 1.0 - clamp(max(max(fade.x, fade.y), fade.z), 0.0, 1.0);
        weight = current * f;
        current *= 1.0 - f;
      }else{
        break;
      }
      if(weight > 1e-6){
        float cascadeSkyVisibility;
        result += ddgiSampleIrradianceInCascade(worldPosition, normal, viewDirection, c, cascadeSkyVisibility) * weight;
        sumSkyVisibility += cascadeSkyVisibility * weight;
        sumWeight += weight;
      }
      if(current < 1e-6){
        break;
      }
    }
    skyVisibility = (sumWeight > 0.0) ? clamp(sumSkyVisibility / sumWeight, 0.0, 1.0) : 1.0;
    return result;
  }

  #if GI_DDGI_STORAGE == GI_DDGI_STORAGE_SH_VALUE
  // ---------------------------------------------------------------------------------------------------------------------
  //  Same sampling as ddgiSampleIrradiance* but returning the blended *radiance* L1 SH (pre cosine-lobe) instead of the
  //  evaluated diffuse irradiance. The SH-storage shading path uses this to extract a dominant directional light (proper
  //  specular via the analytic BRDF) plus a residual ambient SH (diffuse), mirroring the cascaded radiance hints path.
  // ---------------------------------------------------------------------------------------------------------------------
  SHC3CoefficientsL1 ddgiSampleRadianceSHInCascade(const in vec3 worldPosition, const in vec3 normal, const in vec3 viewDirection, const in int cascadeIndex, out float skyVisibility){
    vec3 gridCoord = ddgiWorldToProbeGrid(worldPosition, cascadeIndex);
    ivec3 baseProbe = ivec3(floor(gridCoord));
    vec3 frac = gridCoord - vec3(baseProbe);

    vec3 biasedPosition = worldPosition;

    SHC3CoefficientsL1 sumSH = SHC3CoefficientsL1Zero();
    float sumSkyVisibility = 0.0;
    float sumWeight = 0.0;

    for(int i = 0; i < 8; i++){
      ivec3 offset = ivec3(i & 1, (i >> 1) & 1, (i >> 2) & 1);
      ivec3 probeCoord = clamp(baseProbe + offset, ivec3(0), uDDGIProbeCounts - ivec3(1));

      vec3 trilinear = mix(vec3(1.0) - frac, frac, vec3(offset));
      float weight = trilinear.x * trilinear.y * trilinear.z;

      vec3 probeWorld = ddgiProbeGridToWorld(probeCoord, cascadeIndex);
      vec3 probeToPoint = biasedPosition - probeWorld;
      vec3 dirToProbe = normalize(-probeToPoint);

      float wrap = (dot(dirToProbe, normal) + 1.0) * 0.5;
      weight *= (wrap * wrap) + 0.2;

      float distToProbe = length(probeToPoint);
      vec3 vis = ddgiSampleVisibility(probeCoord, cascadeIndex, normalize(probeToPoint));
      vec2 moments = vis.xy;
      float meanDist = moments.x;
      float chebyshev = 1.0;
      if(distToProbe > meanDist){
        float variance = abs((meanDist * meanDist) - moments.y);
        float d = distToProbe - meanDist;
        chebyshev = variance / (variance + (d * d));
        chebyshev = max(0.0, chebyshev * chebyshev * chebyshev);
      }
      weight *= chebyshev;

      const float crushThreshold = 0.2;
      if(weight < crushThreshold){
        weight *= (weight * weight) * (1.0 / (crushThreshold * crushThreshold));
      }

      weight = max(weight, 1e-6);

      sumSH = SHC3CoefficientsL1Add(sumSH, SHC3CoefficientsL1Mul(ddgiLoadIrradianceSH(probeCoord, cascadeIndex), weight));
      sumSkyVisibility += ddgiSampleVisibility(probeCoord, cascadeIndex, normal).z * weight;
      sumWeight += weight;
    }

    skyVisibility = (sumWeight > 0.0) ? clamp(sumSkyVisibility / sumWeight, 0.0, 1.0) : 0.0;
    return (sumWeight > 0.0) ? SHC3CoefficientsL1Mul(sumSH, 1.0 / sumWeight) : SHC3CoefficientsL1Zero();
  }

  SHC3CoefficientsL1 ddgiSampleRadianceSH(const in vec3 worldPosition, const in vec3 normal, const in vec3 viewDirection, out float skyVisibility){
    int cascadeIndex = 0;
    while(((cascadeIndex + 1) < GI_DDGI_CASCADES) &&
          (any(lessThan(worldPosition, ddgiData.ddgiCascadeAABBMin[cascadeIndex].xyz)) ||
           any(greaterThan(worldPosition, ddgiData.ddgiCascadeAABBMax[cascadeIndex].xyz)))){
      cascadeIndex++;
    }

    SHC3CoefficientsL1 result = SHC3CoefficientsL1Zero();
    float sumSkyVisibility = 0.0;
    float sumWeight = 0.0;
    float current = 1.0;
    for(int c = cascadeIndex; c < GI_DDGI_CASCADES; c++){
      float weight;
      if(c == (GI_DDGI_CASCADES - 1)){
        weight = current;
        current = 0.0;
      }else if(all(greaterThanEqual(worldPosition, ddgiData.ddgiCascadeAABBMin[c].xyz)) &&
               all(lessThanEqual(worldPosition, ddgiData.ddgiCascadeAABBMax[c].xyz))){
        vec3 fade = smoothstep(ddgiData.ddgiCascadeAABBFadeStart[c].xyz,
                               ddgiData.ddgiCascadeAABBFadeEnd[c].xyz,
                               abs(worldPosition - ddgiData.ddgiCascadeAABBCenter[c].xyz));
        float f = 1.0 - clamp(max(max(fade.x, fade.y), fade.z), 0.0, 1.0);
        weight = current * f;
        current *= 1.0 - f;
      }else{
        break;
      }
      if(weight > 1e-6){
        float cascadeSkyVisibility;
        result = SHC3CoefficientsL1Add(result, SHC3CoefficientsL1Mul(ddgiSampleRadianceSHInCascade(worldPosition, normal, viewDirection, c, cascadeSkyVisibility), weight));
        sumSkyVisibility += cascadeSkyVisibility * weight;
        sumWeight += weight;
      }
      if(current < 1e-6){
        break;
      }
    }
    skyVisibility = (sumWeight > 0.0) ? clamp(sumSkyVisibility / sumWeight, 0.0, 1.0) : 1.0;
    return result;
  }
  #endif // GI_DDGI_STORAGE == GI_DDGI_STORAGE_SH_VALUE

#endif // GLOBAL_ILLUMINATION_DDGI_SAMPLE

#endif // GLOBAL_ILLUMINATION_DDGI_GLSL
