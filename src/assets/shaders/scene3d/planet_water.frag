#version 460 core

#pragma shader_stage(fragment)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_control_flow_attributes : enable
#if defined(USEDEMOTE)
  #extension GL_EXT_demote_to_helper_invocation : enable
#endif
#ifdef WIREFRAME
  #extension GL_EXT_fragment_shader_barycentric : enable
  #define HAVE_PERVERTEX
#endif

#if defined(TESSELLATION)
layout(early_fragment_tests) in;
#endif
      
// MSAA_FAST = MSAA input but not MSAA output, so that the water isn't multisampled then.

#define LIGHTCLUSTERS
#define FRUSTUMCLUSTERGRID

#define LIGHTS 
#define SHADOWS

#include "bufferreference_definitions.glsl"

#if defined(TESSELLATION)
layout(location = 0) in InBlock {
  vec3 localPosition;
  vec3 position;
  vec3 sphereNormal;
  vec3 normal;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
//vec4 jitter;
  float mapValue;
  float waterOverSurface;
  float underWater;
  flat uint meshletID;
} inBlock;
#elif defined(UNDERWATER)
layout(location = 0) in InBlock {
  vec2 texCoord;
  float underWater;
} inBlock;
#else
layout(location = 0) in vec2 inTexCoord;
#endif

layout(location = 0) out vec4 outFragColor;

#if defined(VELOCITY)
  layout(location = 1) out vec2 outVelocity;
#elif defined(REFLECTIVESHADOWMAPOUTPUT)
  layout(location = 1) out vec4 outFragNormalUsed; // xyz = normal, w = 1.0 if normal was used, 0.0 otherwise (by clearing the normal buffer to vec4(0.0))
#endif

#if !(defined(TESSELLATION) || defined(UNDERWATER))
#ifdef MSAA
#ifndef MSAA_FAST
layout(input_attachment_index = 0, set = 1, binding = 9) uniform subpassInputMS uOITImgDepth; // Ignored/Unused in the MSAA_FAST case 
#endif
#else
layout(input_attachment_index = 0, set = 1, binding = 9) uniform subpassInput uOITImgDepth;
#endif
#endif // !(defined(TESSELLATION) || defined(UNDERWATER))

#if defined(TESSELLATION)
#define inViewSpacePosition inBlock.viewSpacePosition
#define inWorldSpacePosition inBlock.worldSpacePosition
#define inCameraRelativePosition inBlock.cameraRelativePosition
#else
vec3 viewSpacePosition;
vec3 worldSpacePosition;
vec3 cameraRelativePosition;

#define inViewSpacePosition viewSpacePosition
#define inWorldSpacePosition worldSpacePosition
#define inCameraRelativePosition cameraRelativePosition
#endif

// Global descriptor set

#define PLANETS
#ifdef RAYTRACING
  #define USE_MATERIAL_BUFFER_REFERENCE // needed for raytracing
#endif
#include "globaldescriptorset.glsl"
#undef PLANETS

// Pass descriptor set

#include "mesh_rendering_pass_descriptorset.glsl"
  
/*layout(set = 1, binding = 6, std430) readonly buffer ImageBasedSphericalHarmonicsMetaData {
  vec4 dominantLightDirection;
  vec4 dominantLightColor;
  vec4 ambientLightColor;
} imageBasedSphericalHarmonicsMetaData;*/

#ifdef FRUSTUMCLUSTERGRID
layout (set = 1, binding = 6, std140) readonly uniform FrustumClusterGridGlobals {
  uvec4 tileSizeZNearZFar; 
  vec4 viewRect;
  uvec4 countLightsViewIndexSizeOffsetedViewIndex;
  uvec4 clusterSize;
  vec4 scaleBiasMax;
} uFrustumClusterGridGlobals;

layout (set = 1, binding = 7, std430) readonly buffer FrustumClusterGridIndexList {
   uint frustumClusterGridIndexList[];
};

layout (set = 1, binding = 8, std430) readonly buffer FrustumClusterGridData {
  uvec4 frustumClusterGridData[]; // x = start light index, y = count lights, z = start decal index, w = count decals
};
#endif

// Per planet descriptor set

layout(set = 2, binding = 0) uniform sampler2D uPlanetTextures[]; // 0 = height map, 1 = normal map, 2 = tangent bitangent map
layout(set = 2, binding = 0) uniform sampler2DArray uPlanetArrayTextures[]; // 0 = height map, 1 = normal map, 2 = tangent bitangent map

// Per water render pass descriptor set

#if !(defined(TESSELLATION) || defined(UNDERWATER))
layout(set = 3, binding = 2) uniform sampler2DArray uTextureWaterAcceleration;
#endif

#define globalRaytracingFlags pushConstants.flags

#define PLANET_WATER
#include "planet_renderpass.glsl"

#define FRAGMENT_SHADER

#define WATER_FRAGMENT_SHADER

#define TRANSMISSION
#define TRANSMISSION_FORCED
#define VOLUMEATTENUTATION_FORCED

#include "math.glsl"

#ifdef RAYTRACING
  #include "raytracing.glsl"
#endif

#include "octahedral.glsl"
#include "octahedralmap.glsl"
#include "tangentspacebasis.glsl" 
#include "planet_noise.glsl"

float transmissionFactor = 1.0;
float volumeThickness = 0.005;
float volumeAttenuationDistance = 1.0 / 0.0; // +INF
vec3 volumeAttenuationColor = vec3(1.0); 
float volumeDispersion = 0.0;

float airIOR = 1.0;
float waterIOR = 1.3325;

#define IOR_TO_F0(ior) ((ior - 1.0) * (ior - 1.0)) / ((ior + 1.0) * (ior + 1.0))

float waterF0 = IOR_TO_F0(waterIOR) * IOR_TO_F0(waterIOR);

const vec3 inModelScale = vec3(1.0); 

float ior = waterIOR / airIOR;
 
int inViewIndex = int(gl_ViewIndex);

#define LIGHTING_GLOBALS
#include "lighting.glsl"
#undef LIGHTING_GLOBALS

#define UseEnvMap
#define UseEnvMapGGX
#undef UseEnvMapCharlie
#undef UseEnvMapLambertian

#include "roughness.glsl"

#include "meshlet.glsl"

vec3 imageLightBasedLightDirection = vec3(0.0, 1.0, 0.0);// imageBasedSphericalHarmonicsMetaData.dominantLightDirection.xyz;

vec3 viewDirection;

vec3 workNormal;

uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
mat4 viewMatrix = uView.views[viewIndex].viewMatrix;
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;
mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
mat4 inverseProjectionMatrix = uView.views[viewIndex].inverseProjectionMatrix;

#if !(defined(TESSELLATION) || defined(UNDERWATER))
mat4 viewProjectionMatrix = projectionMatrix * viewMatrix;
mat4 inverseViewProjectionMatrix = inverseViewMatrix * inverseProjectionMatrix;

float linearizeDepth(float z){
#if 1
  vec2 v = (inverseProjectionMatrix * vec4(vec3(fma(inTexCoord, vec2(2.0), vec2(-1.0)), z), 1.0)).zw;
#else
  vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(z), inverseProjectionMatrix[3].zw);
#endif
  return v.x / v.y;
}

float delinearizeDepth(float z){
#if 1
  vec2 v = (projectionMatrix * vec4(vec3(fma(inTexCoord, vec2(2.0), vec2(-1.0)), z), 1.0)).zw;
#else
  vec2 v = fma(projectionMatrix[2].zw, vec2(z), projectionMatrix[3].zw);
#endif
  return v.x / v.y;
}
#endif

#define NOTEXCOORDS
#define inFrameIndex pushConstants.frameIndex
#include "shadows.glsl"

#undef ENABLE_ANISOTROPIC
#define SCREEN_SPACE_REFLECTIONS
#include "pbr.glsl"

const vec3 planetCenter = vec3(0.0); // The planet is at the origin in planet space
float planetBottomRadius = planetData.bottomRadiusTopRadiusHeightMapScale.x;
float planetTopRadius = planetData.bottomRadiusTopRadiusHeightMapScale.y;

mat4 planetModelMatrix = planetData.modelMatrix;
mat4 planetInverseModelMatrix = inverse(planetModelMatrix);

#include "planet_textures.glsl"

#include "planet_water.glsl"

vec3 safeNormalize(vec3 v){
  return (length(v) > 0.0) ? normalize(v) : vec3(0.0);
}

// Accumulate a single Gerstner-style wave's normal gradient onto normalOffset.
// d3: 3D wave direction (unit vector in sphere tangent plane), k: wavenumber (rad/m),
// A: visual amplitude (normal-space, dimensionless), pos: planet-local position (meters).
// waveSpeed (global) controls the animation rate.
vec3 waveWindDir = vec3(1.0, 0.0, 0.0);
float waveAmplitude = 0.0;
float waveFrequency = 0.05;
float waveSteepness = 0.5;
float waveSpeed = 0.5;
float waveWhitecapFactor = 1.0;
float waveWindFactor     = 1.0; // multiplier for wind-based Gerstner normal contribution (0=off, 1=full)
float uvWaveAmplitude    = 0.0; // octUV-based wave normal strength (0=off)
float uvWaveFrequency    = 5.0; // spatial wave cycles per octahedral UV unit [0,1]
float uvWaveSpeed        = 0.3; // UV wave animation speed (UV units/s)
float uvWaveSteepness    = 0.5; // UV wave steepness / sharpness
float uvWaveFactor       = 1.0; // overall UV wave contribution multiplier (0=off, 1=full)
float uvWaveScale        = 10.0; // UV coordinate scale applied to octUV before wave phases (higher = finer ripples)
vec3  whitecapColor          = vec3(1.0);  // whitecap foam color (linear RGB)
float whitecapPatternScale   = 24.0;  // FBM breakup pattern scale
float whitecapSlopeThreshLow  = 0.05; // heightmap slope where whitecaps begin
float whitecapSlopeThreshHigh = 0.20; // heightmap slope where whitecaps are full
float whitecapBreakupLow     = 0.35;  // FBM breakup smoothstep low threshold
float whitecapBreakupHigh    = 0.75;  // FBM breakup smoothstep high threshold

void accumulateWaveNormal(vec3 d3, float k, float A, vec3 pos, inout vec3 normalOffset){
  float phase = k * dot(d3, pos) - (waveSpeed * pushConstants.time);
  normalOffset -= A * cos(phase) * d3;
}

// Accumulate a single octahedral-UV-space wave onto normalOffset.
// uvDir: normalised 2D direction in oct UV space, k: wavenumber (cycles per UV unit),
// A: amplitude, uv: oct UV coords, t: time * uvWaveSpeed,
// tanU/tanV: sphere tangent vectors aligned with the oct UV +U/+V axes.
void accumulateUVWaveNormal(vec2 uvDir, float k, float A, vec2 uv, float t, inout vec3 normalOffset, vec3 tanU, vec3 tanV){
  float phase = k * dot(uvDir, uv) - t;
  float dPhase = A * cos(phase);
  normalOffset -= (dPhase * uvDir.x) * tanU + (dPhase * uvDir.y) * tanV;
}

vec3 getWaterNormal(vec3 position){

  vec3 n = normalize(position);

#if 1
  float texScale = 1.0 / 4096.0;

  vec3 normal;

  {

    // Calculate the normal as the average of the normals of some temporary virtual triangles
    
    // a(-1, -1) b( 0, -1) c( 1, -1)
    // d(-1,  0) e( 0,  0) f( 1,  0)
    // g(-1,  1) h( 0,  1) i( 1,  1)

    vec2 euv = octPlanetUnsignedEncode(n);
    
    vec2 auv = wrapOctahedralCoordinates(euv + (vec2(-1.0, -1.0) * texScale)); // -1, -1
    vec2 buv = wrapOctahedralCoordinates(euv + (vec2(0.0, -1.0) * texScale)); //  0, -1
    vec2 cuv = wrapOctahedralCoordinates(euv + (vec2(1.0, -1.0) * texScale)); //  1, -1
    vec2 duv = wrapOctahedralCoordinates(euv + (vec2(-1.0, 0.0) * texScale)); // -1,  0
    vec2 fuv = wrapOctahedralCoordinates(euv + (vec2(1.0, 0.0) * texScale)); //  1,  0
    vec2 guv = wrapOctahedralCoordinates(euv + (vec2(-1.0, 1.0) * texScale)); // -1,  1
    vec2 huv = wrapOctahedralCoordinates(euv + (vec2(0.0, 1.0) * texScale)); //  0,  1
    vec2 iuv = wrapOctahedralCoordinates(euv + (vec2(1.0, 1.0) * texScale)); //  1,  1

    float eh = getSphereHeight(euv);

    float ah = getSphereHeightEx(auv);
    float bh = getSphereHeightEx(buv);
    float ch = getSphereHeightEx(cuv);
    float dh = getSphereHeightEx(duv);
    float fh = getSphereHeightEx(fuv);
    float gh = getSphereHeightEx(guv);
    float hh = getSphereHeightEx(huv);
    float ih = getSphereHeightEx(iuv);

    vec3 a = octPlanetUnsignedDecode(auv) * ((ah > 0.0) ? ah : eh);
    vec3 b = octPlanetUnsignedDecode(buv) * ((bh > 0.0) ? bh : eh);
    vec3 c = octPlanetUnsignedDecode(cuv) * ((ch > 0.0) ? ch : eh);
    vec3 d = octPlanetUnsignedDecode(duv) * ((dh > 0.0) ? dh : eh);
    vec3 e = n * eh;
    vec3 f = octPlanetUnsignedDecode(fuv) * ((fh > 0.0) ? fh : eh);
    vec3 g = octPlanetUnsignedDecode(guv) * ((gh > 0.0) ? gh : eh);
    vec3 h = octPlanetUnsignedDecode(huv) * ((hh > 0.0) ? hh : eh);
    vec3 i = octPlanetUnsignedDecode(iuv) * ((ih > 0.0) ? ih : eh);

    // Calculate the smoothed normal at point e as the average of the normals of the surrounding triangles in triangle fan order:
    normal = safeNormalize(
      safeNormalize(cross(a - e, b - e)) + // Triangle EAB
      safeNormalize(cross(b - e, c - e)) + // Triangle EBC          
      safeNormalize(cross(c - e, f - e)) + // Triangle EDA
      safeNormalize(cross(f - e, i - e)) + // Triangle EFI
      safeNormalize(cross(i - e, h - e)) + // Triangle EIH
      safeNormalize(cross(h - e, g - e)) + // Triangle EHG
      safeNormalize(cross(g - e, d - e)) + // Triangle EGD
      safeNormalize(cross(d - e, a - e))   // Triangle EDA
    );   

  }       

  // Add Gerstner wave detail normal perturbation (4 wave trains, wind-directed).
  // Controlled by waveWindDir, waveAmplitude, waveFrequency, waveSteepness, waveSpeed
  // which are unpacked from planetData.waterWaveParams in main().
  if((waveAmplitude > 0.0) && (waveFrequency > 0.0) && (waveWindFactor > 0.0)){
    vec3 sphereN = normalize(position);
    // Project global wind direction onto the tangent plane at this sphere point.
    vec3 wd = waveWindDir - (dot(waveWindDir, sphereN) * sphereN);
    float wdLen = length(wd);
    if(wdLen > 0.001){
      wd /= wdLen;
      vec3 wdB = cross(sphereN, wd);
      vec3 normalOffset = vec3(0.0);
      // Wave 1: primary wind direction (full weight)
      accumulateWaveNormal(wd,                                           waveFrequency,       waveAmplitude,        position, normalOffset);
      // Wave 2: +30 deg, 0.7x frequency, 0.5x amplitude
      accumulateWaveNormal((0.866025 * wd) + (0.5      * wdB),          waveFrequency * 0.7, waveAmplitude * 0.5,  position, normalOffset);
      // Wave 3: -45 deg, 1.3x frequency, 0.35x amplitude
      accumulateWaveNormal((0.707107 * wd) - (0.707107 * wdB),          waveFrequency * 1.3, waveAmplitude * 0.35, position, normalOffset);
      // Wave 4: +60 deg, 2.1x frequency, 0.2x amplitude (high-frequency chop)
      accumulateWaveNormal((0.5      * wd) + (0.866025 * wdB),          waveFrequency * 2.1, waveAmplitude * 0.2,  position, normalOffset);
      normal = normalize(normal + (normalOffset * waveSteepness * waveWindFactor));
    }
  }

  // UV-based (octahedral UV) wave normal perturbation — wind-independent omnidirectional detail.
  // Driven by uvWaveAmplitude, uvWaveFrequency, uvWaveSpeed, uvWaveSteepness, uvWaveFactor, uvWaveScale
  // unpacked from planetData.waterUVWaveParams in main().
  if((uvWaveAmplitude > 0.0) && (uvWaveFrequency > 0.0) && (uvWaveFactor > 0.0)){
    vec2 octUV = octPlanetUnsignedEncode(n);
    const float octEps = 1.0 / 2048.0;
    // Tangent basis from unscaled octUV (sphere surface derivatives, not wave UV).
    vec3 tanU = normalize(octPlanetUnsignedDecode(vec2(octUV.x + octEps, octUV.y)) - octPlanetUnsignedDecode(vec2(octUV.x - octEps, octUV.y)));
    vec3 tanV = normalize(octPlanetUnsignedDecode(vec2(octUV.x, octUV.y + octEps)) - octPlanetUnsignedDecode(vec2(octUV.x, octUV.y - octEps)));
    vec2 scaledUV = wrapOctahedralCoordinates(octUV * uvWaveScale);
    float t = pushConstants.time * uvWaveSpeed;
    vec3 uvNormalOffset = vec3(0.0);
    // Wave 1: along UV U-axis (full weight)
    accumulateUVWaveNormal(vec2(1.0, 0.0),             uvWaveFrequency,        uvWaveAmplitude,        scaledUV, t,        uvNormalOffset, tanU, tanV);
    // Wave 2: along UV V-axis, 0.73x frequency, 0.6x amplitude
    accumulateUVWaveNormal(vec2(0.0, 1.0),             uvWaveFrequency * 0.73, uvWaveAmplitude * 0.6,  scaledUV, t * 1.1,  uvNormalOffset, tanU, tanV);
    // Wave 3: diagonal UV (+45 deg), 1.4x frequency, 0.35x amplitude
    accumulateUVWaveNormal(vec2(0.707107, 0.707107),   uvWaveFrequency * 1.4,  uvWaveAmplitude * 0.35, scaledUV, t * 0.8,  uvNormalOffset, tanU, tanV);
    // Wave 4: diagonal UV (-45 deg), 2.1x frequency, 0.2x amplitude (high-frequency chop)
    accumulateUVWaveNormal(vec2(0.707107, -0.707107),  uvWaveFrequency * 2.1,  uvWaveAmplitude * 0.2,  scaledUV, t * 1.3,  uvNormalOffset, tanU, tanV);
    normal = normalize(normal + (uvNormalOffset * uvWaveSteepness * uvWaveFactor));
  }

  return normal;
#else

  const vec2 uvOfs = vec2(1.0 / 4096.0, 0.0);

  vec2 uv = octPlanetUnsignedEncode(n);
  vec2 uv00 = wrapOctahedralCoordinates(uv - uvOfs.xy);
  vec2 uv01 = wrapOctahedralCoordinates(uv + uvOfs.xy);
  vec2 uv10 = wrapOctahedralCoordinates(uv - uvOfs.yx);
  vec2 uv11 = wrapOctahedralCoordinates(uv + uvOfs.yx);

  float h = getSphereHeight(uv); 
  float h00 = getSphereHeightEx(uv00);
  float h01 = getSphereHeightEx(uv01);
  float h10 = getSphereHeightEx(uv10);
  float h11 = getSphereHeightEx(uv11);

  vec3 p = n * h; 
  vec3 p00 = octPlanetUnsignedDecode(uv00) * ((h00 > 0.0) ? h00 : h);
  vec3 p01 = octPlanetUnsignedDecode(uv01) * ((h01 > 0.0) ? h01 : h);
  vec3 p10 = octPlanetUnsignedDecode(uv10) * ((h10 > 0.0) ? h10 : h);
  vec3 p11 = octPlanetUnsignedDecode(uv11) * ((h11 > 0.0) ? h11 : h);
  
  vec3 tangent = (distance(p00, p01) > 0.0)
                    ? normalize(p01 - p00) 
                    : ((distance(p10, p11) > 0.0) 
                        ? normalize(cross(normalize(p11 - p10), p)) 
                        : normalize(p - p00));

  vec3 bitangent = (distance(p10, p11) > 0.0) 
                      ? normalize(p11 - p10) 
                      : ((distance(p01, p00) > 0.0)
                          ? normalize(cross(normalize(p01 - p00), p)) 
                          : normalize(p - p10));

  return normalize(cross(tangent, bitangent));
#endif
}

float fresnelGet(float costheta, float ior){
  float r0 = (1.0f - ior) / (1.0f + ior);
  r0 *= r0;
  float x = 1.0 - costheta;
  return r0 + ((1.0 - r0) * (x * x * x));
}

float fresnelDielectric(vec3 Incoming, vec3 Normal, float eta){
  // compute fresnel reflectance without explicitly computing the refracted direction 
  float c = abs(dot(Incoming, Normal));
  float g = ((eta * eta) - 1.0) + (c * c);
  float result;
  if(g > 0.0){
    g = sqrt(g);
    float A = (g - c) / (g + c);
    float B = ((c * (g + c)) - 1.0) / ((c * (g - c)) + 1.0);
    result = (0.5 * A * A) * (1.0 + (B * B));
  }else{
    result = 1.0;  /* TIR (no refracted component) */
  }
  return result;
}

float getFresnel(vec3 incident, vec3 normal, float iorIn, float iorOut){
  vec2 cosit = vec2(clamp(dot(incident, normal), -1.0, 1.0), 0.0);
  vec2 etait = (cosit.x > 0.0) ? vec2(iorIn, iorOut) : vec2(iorOut, iorIn);
  float sint = (etait.x / etait.y) * sqrt(max(0.0, 1.0 - (cosit.x * cosit.x)));
  if(sint >= 1.0){
    return 1.0;
  }else{
    cosit = vec2(abs(cosit.x), sqrt(max(0.0, 1.0 - (sint * sint))));
    return length(vec2((etait.y * cosit.x) - (etait.x * cosit.y), (etait.x * cosit.x) - (etait.y * cosit.y)) / vec2((etait.y * cosit.x) + (etait.x * cosit.y), (etait.x * cosit.x) + (etait.y * cosit.y))) * 0.5;
  }
}

float HenyeyGreenstein(float mu, float inG){
  return (1.0 - (inG * inG)) / (pow((1.0 + (inG * inG)) - (2.0 * inG * mu), 1.5) * 12.5663706144);
}

#define PROCESSLIGHT processLight 

vec3 waterBaseColor = pow(vec3(0.555555, 0.777777, 1.0), vec3(2.5));//vec3(0.5, 0.7, 0.9); // default; overridden from planetData.waterBaseColorIORs in main()

vec3 waterDiffuseColor = vec3(0.0);
vec3 waterSpecularColor = vec3(0.0);

vec3 waterSubscattering = vec3(0.0);

// Downwelling irradiance reaching the water surface from direct (shadow-attenuated) lights.
// Accumulated in processLight and combined with IBL diffuse to modulate the deep-water
// scattering color so the volume stays dark at night / in shadow and bright at day.
vec3 waterDownwellingIrradiance = vec3(0.0);

vec3 waterColor; //vec3(0.090195, 0.115685, 0.12745);

float waterDepth;

void processLight(const in vec3 lightColor, 
                  const in vec3 lightLit, 
                  const in vec3 lightDirection){

  float mu = dot(lightDirection, -viewDirection);

  waterSubscattering += HenyeyGreenstein(mu, 0.5) * waterColor * lightColor * lightLit * (1.0 - clamp(exp(-waterDepth * 0.01), 0.0, 1.0));  

  // Downwelling irradiance onto the water surface from above (shadow/visibility-aware via
  // lightLit from the caller, which carries the per-light lightAttenuation including shadows).
  // Above/below water sign flipping is already handled by the caller via workNormal.
  waterDownwellingIrradiance += lightColor * lightLit * max(0.0, dot(workNormal, lightDirection));

//waterSubscattering += HenyeyGreenstein(mu, 0.5) * waterColor * lightColor * max(0.0, waterDepth * 0.01);

} 

// --- Shore foam helpers ----------------------------------------------------
// Uses the shared gradient-noise FBM from planet_noise.glsl so the foam pattern
// stays stable on the sphere surface while avoiding axis-aligned grid artefacts
// of naive value-noise.

#define SHORE_FOAM_LEGACY_VALUE_NOISE

#ifdef SHORE_FOAM_LEGACY_VALUE_NOISE
// Small, self-contained 3D value-noise FBM sampled in local-planet space so
// the foam pattern stays stable on the sphere surface while being cheap.
float shoreFoamHash(vec3 p){
  return hash44ChaCha20(vec4(p, 0.0)).x;
}

float shoreFoamNoise(vec3 p){
  vec3 i = floor(p);
  vec3 f = fract(p);
  vec3 u = f * f * (3.0 - (2.0 * f));
  float n000 = shoreFoamHash(i + vec3(0.0, 0.0, 0.0));
  float n100 = shoreFoamHash(i + vec3(1.0, 0.0, 0.0));
  float n010 = shoreFoamHash(i + vec3(0.0, 1.0, 0.0));
  float n110 = shoreFoamHash(i + vec3(1.0, 1.0, 0.0));
  float n001 = shoreFoamHash(i + vec3(0.0, 0.0, 1.0));
  float n101 = shoreFoamHash(i + vec3(1.0, 0.0, 1.0));
  float n011 = shoreFoamHash(i + vec3(0.0, 1.0, 1.0));
  float n111 = shoreFoamHash(i + vec3(1.0, 1.0, 1.0));
  return mix(mix(mix(n000, n100, u.x), mix(n010, n110, u.x), u.y),
             mix(mix(n001, n101, u.x), mix(n011, n111, u.x), u.y),
             u.z);
}

float shoreFoamFBM(vec3 p){
  float f = 0.0;
  float a = 0.5;
  for(int i = 0; i < 4; i++){
    f += shoreFoamNoise(p) * a;
    p = (p * 2.03) + vec3(17.13, 23.71, 29.17);
    a *= 0.5;
  }
  return f;
}
#endif

// Shared shore-foam overlay. Returns aBaseColor unchanged for waterDepth values above the foam
// range or when the foam is disabled; otherwise blends the configured foam color on top, using
// aPlanetSpacePos as the pattern domain so the foam stays locked to the planet surface.
vec3 applyShoreFoam(vec3 aBaseColor, vec3 aPlanetSpacePos, float aShoreDepth){
  vec3 result = aBaseColor;
  vec4 waterShoreFoam0 = vec4(unpackHalf2x16(planetData.waterShoreFoam.x), unpackHalf2x16(planetData.waterShoreFoam.y));
  vec4 waterShoreFoam1 = vec4(unpackHalf2x16(planetData.waterShoreFoam.z), unpackHalf2x16(planetData.waterShoreFoam.w));
  if(waterShoreFoam1.w > 0.0){
    float shoreMask = 1.0 - smoothstep(waterShoreFoam1.x, waterShoreFoam0.w, aShoreDepth);
    if(shoreMask > 0.0){
      vec3 foamUV = aPlanetSpacePos * waterShoreFoam1.y;
      float foamPhase = pushConstants.time * waterShoreFoam1.z;
#ifdef SHORE_FOAM_LEGACY_VALUE_NOISE
      float foamA = shoreFoamFBM(foamUV + vec3(0.0, 0.0, foamPhase));
      float foamB = shoreFoamFBM((foamUV * 1.73) + vec3(foamPhase * 0.7, -foamPhase * 0.5, 0.0));
      float foamPattern = clamp((foamA * 1.4) - (foamB * 0.6) - 0.25, 0.0, 1.0);
#else
      // Domain-warp via a cheap low-frequency offset noise to break up any
      // residual lattice regularity, then sample two decorrelated FBMs and
      // combine them with a soft smoothstep for an organic foam shape.
      vec3 warp = vec3(planetGradientNoise(foamUV * 0.5 + vec3(foamPhase, 0.0, 0.0)),
                       planetGradientNoise(foamUV * 0.5 + vec3(0.0, foamPhase, 0.0)),
                       planetGradientNoise(foamUV * 0.5 + vec3(0.0, 0.0, foamPhase))) * 0.35;
      float foamA = planetNoiseFBM((foamUV + warp) + vec3(0.0, 0.0, foamPhase));
      float foamB = planetNoiseFBM(((foamUV * 1.73) + warp) + vec3(foamPhase * 0.7, -foamPhase * 0.5, 0.0));
      float foamPattern = smoothstep(0.35, 0.75, foamA - (foamB * 0.4));
#endif
      float foamAmount = clamp(shoreMask * foamPattern * waterShoreFoam1.w, 0.0, 1.0);
      // Modulate the (typically white) foam color by ambient IBL + shadow-attenuated direct
      // downwelling irradiance so foam darkens at night / in shadow instead of glowing white.
      vec3 foamIrradiance = getIBLDiffuse(workNormal) + waterDownwellingIrradiance;
      vec3 foamLit = waterShoreFoam0.xyz * foamIrradiance;
      result = mix(result, foamLit, foamAmount);
    }
  }
  return result;
}

// Whitecap (breaking wave crest) mask: combines Gerstner wave-crest phase
// detection with an FBM breakup to produce ragged, animated foam patches at
// wave crests. Returns 0 when amplitude*steepness is below threshold.
float computeWhitecapMask(vec3 position){
  float globalCoverage = max(0.0, waveWhitecapFactor);
  if(globalCoverage <= 0.0){
    return 0.0;
  }
  // Whitecap is driven purely by the gradient of the water simulation heightmap in
  // sphere-correct (round-planet) tangent space — no wave-phase re-computation.
  // High water surface slope (steep wave face) => whitecap.
  vec3 n = normalize(position);
  vec2 octUV = octPlanetUnsignedEncode(n);
  const vec2 uvOfs = vec2(1.0 / 4096.0, 0.0);
  vec2 uv00 = wrapOctahedralCoordinates(octUV - uvOfs.xy);
  vec2 uv01 = wrapOctahedralCoordinates(octUV + uvOfs.xy);
  vec2 uv10 = wrapOctahedralCoordinates(octUV - uvOfs.yx);
  vec2 uv11 = wrapOctahedralCoordinates(octUV + uvOfs.yx);
  // Water simulation heights at neighbours (pure water height, no terrain offset needed for gradient).
  float wh00 = getWaterHeightData(uv00);
  float wh01 = getWaterHeightData(uv01);
  float wh10 = getWaterHeightData(uv10);
  float wh11 = getWaterHeightData(uv11);
  // Total surface heights for sphere-correct 3D distances (terrain + water).
  float h   = getSphereHeight(octUV);
  float h00 = getSphereHeightEx(uv00);
  float h01 = getSphereHeightEx(uv01);
  float h10 = getSphereHeightEx(uv10);
  float h11 = getSphereHeightEx(uv11);
  vec3 p    = n * h;
  vec3 p00  = octPlanetUnsignedDecode(uv00) * ((h00 > 0.0) ? h00 : h);
  vec3 p01  = octPlanetUnsignedDecode(uv01) * ((h01 > 0.0) ? h01 : h);
  vec3 p10  = octPlanetUnsignedDecode(uv10) * ((h10 > 0.0) ? h10 : h);
  vec3 p11  = octPlanetUnsignedDecode(uv11) * ((h11 > 0.0) ? h11 : h);
  // Sphere-correct 3D surface distances for gradient normalisation.
  float distU = max(1e-6, length(p01 - p00));
  float distV = max(1e-6, length(p11 - p10));
  // Water height gradient in heightmap tangent space (dimensionless, m/m).
  float gradU = (wh01 - wh00) / distU;
  float gradV = (wh11 - wh10) / distV;
  float gradMag = sqrt((gradU * gradU) + (gradV * gradV));
  // Threshold: scale steepness thresholds with wave amplitude so the whitecap
  // coverage adapts automatically when wave settings change.
  float slopeThreshLow  = whitecapSlopeThreshLow;
  float slopeThreshHigh = whitecapSlopeThreshHigh;
  float crest = smoothstep(slopeThreshLow, slopeThreshHigh, gradMag);
  // FBM breakup pattern: use own whitecap patternscale.
  vec3 foamUV    = position * whitecapPatternScale;
  float foamPhase = pushConstants.time * waveSpeed * 0.25;
  vec3 warp      = vec3(planetGradientNoise(foamUV * 0.5 + vec3(foamPhase,        0.0,          0.0        )),
                        planetGradientNoise(foamUV * 0.5 + vec3(0.0,              foamPhase,    0.0        )),
                        planetGradientNoise(foamUV * 0.5 + vec3(0.0,              0.0,          foamPhase  ))) * 0.35;
  float foamA    = planetNoiseFBM((foamUV + warp) + vec3(0.0, 0.0, foamPhase));
  float foamB    = planetNoiseFBM(((foamUV * 1.73) + warp) + vec3(foamPhase * 0.7, -foamPhase * 0.5, 0.0));
  float foamBreakup = smoothstep(whitecapBreakupLow, whitecapBreakupHigh, foamA - (foamB * 0.4));
  return clamp(globalCoverage * crest * foamBreakup, 0.0, 1.0);
}

// Apply whitecap foam to aBaseColor, lit by the same sky+sun irradiance as
// shore foam so whitecaps darken at night rather than glowing white.
vec3 applyWhitecaps(vec3 aBaseColor, vec3 aPlanetSpacePos){
  if((waveAmplitude * waveSteepness) <= 0.06){
    return aBaseColor;
  }
  float mask = computeWhitecapMask(aPlanetSpacePos);
  if(mask <= 0.0){
    return aBaseColor;
  }
  vec3 foamIrradiance  = getIBLDiffuse(workNormal) + waterDownwellingIrradiance;
  vec3 foamLit         = whitecapColor * foamIrradiance;
  return mix(aBaseColor, foamLit, mask);
}

vec4 doShade(float opaqueDepth, float surfaceDepth, bool underWater){

  waterDepth = opaqueDepth - surfaceDepth;

  vec4 albedo = vec4(1.0);  
  vec3 baseColor = vec3(1.0);
  vec4 occlusionRoughnessMetallic = vec4(1.0, 0.0, 0.9, 0.0);

  // The blade normal is rotated slightly to the left or right depending on the x texture coordinate for
  // to fake roundness of the blade without real more complex geometry
  vec3 normal = workNormal;
 
  float NdotV;
  normal = getViewClampedNormal(normal, viewDirection, NdotV);
  NdotV = clamp(NdotV, 0.0, 1.0);

  float occlusion = clamp(occlusionRoughnessMetallic.x, 0.0, 1.0);
    
  vec2 metallicRoughness = clamp(occlusionRoughnessMetallic.zy, vec2(0.0, 1e-3), vec2(1.0));

  float metallic = metallicRoughness.x;

  vec4 diffuseColorAlpha = vec4(max(vec3(0.0), albedo.xyz * (1.0 - metallicRoughness.x)), albedo.w);

  //vec3 F0Dielectric = mix(vec3(waterF0), albedo.xyz, metallicRoughness.x);
  vec3 F0Dielectric = vec3(0.04);

  vec3 F90 = vec3(1.0);
  vec3 F90Dielectric = vec3(1.0);

  float transparency = 0.0;

  float refractiveAngle = 0.0;

  float perceptualRoughness = metallicRoughness.y;

  float kernelRoughness;
  {
    const float SIGMA2 = 0.15915494, KAPPA = 0.18;        
    vec3 dx = dFdx(workNormal), dy = dFdy(workNormal);
    kernelRoughness = min(KAPPA, (2.0 * SIGMA2) * (dot(dx, dx) + dot(dy, dy)));
    perceptualRoughness = sqrt(clamp((perceptualRoughness * perceptualRoughness) + kernelRoughness, 0.0, 1.0));
  }  

  float alphaRoughness = perceptualRoughness * perceptualRoughness;

  diffuseOcclusion = occlusion * ambientOcclusion;
  specularOcclusion = getSpecularOcclusion(clamp(dot(normal, viewDirection), 0.0, 1.0), diffuseOcclusion, alphaRoughness);

  // Horizon specular occlusion
  {
    vec3 reflectedVector = reflect(-viewDirection, normal);
    float horizon = min(1.0 + dot(reflectedVector, normal), 1.0);
    specularOcclusion *= horizon * horizon;         
  }

  const vec3 sheenColor = vec3(0.0);
  const float sheenRoughness = 0.0;

  const vec3 clearcoatF0 = vec3(0.04);
  const vec3 clearcoatF90 = vec3(0.0);
  vec3 clearcoatNormal = normal;
  const float clearcoatFactor = 1.0;
  const float clearcoatRoughness = 1.0;

  float litIntensity = 1.0;

  const float specularWeight = 1.0;//0.255;

  const float iblWeight = 1.0;

  vec3 triangleNormal = normal;

#if 0

  float ior = underWater ? 0.66 : 1.33;
  float eta = max(ior, 1e-5);
  
  float fresnel = clamp(fresnelDielectric(-viewDirection, normal, eta), 0.0, 1.0);
  //float fresnel = pow(1.0 - max(dot(normal, -viewDirection), 0.0), 3.0) * 1.0;

  vec4 color = vec2(0.0, 1.0).xxxy; 		

  vec3 reflection = vec3(0.1); 		
  
  vec3 refraction = getIBLVolumeRefraction(normal.xyz, 
                                           viewDirection,
                                                 clamp(waterDepth * 0.1, 0.0, 0.25),//perceptualRoughness,
                                                 vec3(1.0), //diffuseColorAlpha.xyz, 
                                                 //vec3(0.04), //F0, 
                                                 //vec3(1.0), //F90,
                                                 inWorldSpacePosition,
/*                                          perceptualRoughness,
                                           diffuseColorAlpha.xyz, F0, F90,
                                           inWorldSpacePosition,*/
                                           ior, 
                                           volumeThickness, 
                                           volumeAttenuationColor, 
                                           volumeAttenuationDistance,
                                           volumeDispersion);      

  color.xyz = mix(refraction, reflection, fresnel) * waterBaseColor;  

  return color;

#else

  //diffuseOutput = vec3(0.0);

  //vec3(0.015625) * edgeFactor() * fma(clamp(dot(normal, vec3(0.0, 1.0, 0.0)), 0.0, 1.0), 1.0, 0.0), 1.0);
  vec4 color = vec4(0.0, 0.0, 0.0, 1.0);
  
  float fresnel = clamp(fresnelDielectric(-viewDirection, normal, underWater ? airIOR / waterIOR : waterIOR / airIOR), 0.0, 1.0);
  //float fresnel = pow(1.0 - max(dot(normal, viewDirection), 0.0), 3.0) * 1.0;

  //float fresnel = clamp(fresnelGet(max(0.0, dot(viewDirection, normal)), underWater ? airIOR / waterIOR : waterIOR / airIOR), 0.0, 1.0);
 /*float fresnel;
  {
    float ior = underWater ? airIOR / waterIOR : waterIOR / airIOR;
    float r0 = (1.0 - ior) / (1.0 + ior);
    float x = 1.0 - max(0.0, dot(viewDirection, normal));
    fresnel = mix(x * x * x, 1.0, r0 * r0);
  }*/
  //float fresnel = clamp(getFresnel(-viewDirection, normal, underWater ? airIOR : waterIOR, underWater ? waterIOR : airIOR), 0.0, 1.0);
  
/*if(underWater){
    
    vec3 r = textureLod(uPassTextures[1], vec3(inTexCoord, gl_ViewIndex), 1.0).xyz;
    color = vec4(r, 1.0);

  }else*/{

   /*vec4 hitPosition = vec4(viewDirection * hitTime, 1.0);
    hitPosition = inverseViewMatrix * hitPosition;
    hitPosition /= hitPosition.w;

    hitWaterDepth = underWater ? hitTime : distance(hitPosition.xyz, inWorldSpacePosition);

    waterDepth = getWaterHeightData(octPlanetUnsignedEncode(normalize(inWorldSpacePosition)));*/
    
    float waterHeight = getWaterHeightData(octPlanetUnsignedEncode(normalize(inWorldSpacePosition)));

// waterColor = pow(vec3(0.6862, 0.8823, 0.9411), vec3(2.2));//pow(waterBaseColor, vec3(mix(1.0, 2.0, clamp(waterDepth * 0.1, 0.0, 1.0))));
    waterColor = waterBaseColor;//pow(waterBaseColor, vec3(mix(1.0, 2.0, clamp(waterDepth * 0.1, 0.0, 1.0))));
    
#define LIGHTING_INITIALIZATION
#include "lighting.glsl"
#undef LIGHTING_INITIALIZATION

   const bool receiveShadows = true; 
   
#define LIGHTING_IMPLEMENTATION
#include "lighting.glsl"
#undef LIGHTING_IMPLEMENTATION

    vec3 iblDiffuse = getIBLDiffuse(normal) * baseColor.xyz;
    vec3 iblSpecularMetal = getIBLRadianceGGX(normal, viewDirection, perceptualRoughness);
    vec3 iblSpecularDielectric = iblSpecularMetal;
    vec3 iblMetalFresnel = getIBLGGXFresnel(normal, viewDirection, perceptualRoughness, baseColor.xyz, 1.0);
    vec3 iblMetalBRDF = iblMetalFresnel * iblSpecularMetal;
    vec3 iblDielectricFresnel = getIBLGGXFresnel(normal, viewDirection, perceptualRoughness, F0Dielectric, specularWeight);    
    vec3 iblDielectricBRDF = mix(iblDiffuse * diffuseOcclusion, iblSpecularDielectric * specularOcclusion, iblDielectricFresnel);
    vec3 iblResultColor = mix(iblDielectricBRDF, iblMetalBRDF * specularOcclusion, metallic); // Dielectric/metallic mix
    vec3 iblSpecular = iblResultColor;

//    vec3 iblSpecular = getIBLRadianceGGX(normal, perceptualRoughness, F0Dielectric, specularWeight, viewDirection, litIntensity, imageLightBasedLightDirection) * iblWeight;

    vec3 transmissionOutput = vec3(0.0);

#if defined(TRANSMISSION)

    transmissionOutput = getIBLVolumeRefraction(normal.xyz, 
                                                 viewDirection,
                                                 clamp(waterDepth * 0.01, 0.0, 1.0), //perceptualRoughness,
                                                 vec3(1.0), //diffuseColorAlpha.xyz, 
                                                 //vec3(waterF0), //F0Dielectric, 
                                                 //vec3(1.0), //F90,
                                                 inWorldSpacePosition,
                                                 waterIOR, 
                                                 volumeThickness, 
                                                 volumeAttenuationColor, 
                                                 volumeAttenuationDistance,
                                                 volumeDispersion);        

#endif

    vec4 screenSpaceReflection = underWater 
                                   ? vec4(0.0) 
                                   : vec4(iblSpecular, 1.0); //getScreenSpaceReflection(worldSpacePosition, normal, -viewDirection, 0.0, vec4(iblSpecular, 1.0));

    vec3 reflection = mix(screenSpaceReflection.xyz, screenSpaceReflection.xyz * albedo.xyz, screenSpaceReflection.w) + colorOutput;
 
  // reflection = vec3(0.1);

#if defined(TRANSMISSION) 
    vec3 refraction = transmissionOutput;
#else
    vec3 refraction = vec3(0.0);
#endif

    // Beer-Lambert per-channel absorption attenuating refraction across the vertical water column,
    // with the deep-water scattering color as the asymptotic floor for fully-attenuated light.
    // The deep-water color represents multiple-scattered downwelling irradiance, so it is
    // modulated by the sum of IBL diffuse (sky) and per-light shadow-attenuated downwelling
    // irradiance (waterDownwellingIrradiance accumulated in processLight) so the volume stays
    // lighting-consistent (dark at night / in shadow, bright at day).
    // waterAbsorption.w (IOR-based fade amount) blends the Beer-Lambert result toward the PBR-correct
    // mix(refraction, waterF0, 1-exp(-depth)) IOR-based water volume appearance.
    vec4 waterAbsorption = vec4(unpackHalf2x16(planetData.waterAbsorptionDeepColor.x), unpackHalf2x16(planetData.waterAbsorptionDeepColor.y));
    vec4 waterDeepColor = vec4(unpackHalf2x16(planetData.waterAbsorptionDeepColor.z), unpackHalf2x16(planetData.waterAbsorptionDeepColor.w));
    vec3 waterDeepIrradiance = getIBLDiffuse(underWater ? -normal : normal) + waterDownwellingIrradiance;
    vec3 waterDeepLit = waterDeepColor.xyz * waterDeepIrradiance;
    refraction = mix(mix(refraction, waterDeepLit, clamp(vec3(1.0) - exp(-waterDepth * waterAbsorption.xyz), vec3(0.0), vec3(1.0))),
                     mix(refraction, vec3(waterF0), clamp(1.0 - exp(-waterDepth * 1.0), 0.0, 1.0)),
                     clamp(waterAbsorption.w, 0.0, 1.0));

    vec3 waterShade = mix(refraction * waterColor, reflection * waterColor, fresnel) + waterSubscattering;
#if defined(TESSELLATION)
    // Shore foam overlay: fades in where the water becomes shallow and saturates near the
    // waterline. Pattern is a cheap 3D FBM sampled in planet-space (see applyShoreFoam).
    waterShade = applyShoreFoam(waterShade, inBlock.position, waterDepth);
    // Whitecap foam on wave crests, driven by waveAmplitude*waveSteepness threshold.
    waterShade = applyWhitecaps(waterShade, inBlock.position);
#endif

    color.xyz = mix(
      texelFetch(uPassTextures[1], ivec3(gl_FragCoord.xy, gl_ViewIndex), 0).xyz,
      waterShade,
      clamp(1.0 - exp(-max(waterHeight, waterDepth) * 6.0), 0.0, 1.0)
      //clamp(1.0 - exp(-mix(waterHeight, waterDepth, max(0.0, dot(normal, viewDirection))) * 6.0), 0.0, 1.0)
    );

  //  color.xyz = vec3(waterDepth * 0.01);
    
   //color.xyz = max(vec3(0.0), refraction);

//    color.xyz = texelFetch(uPassTextures[1], ivec3(gl_FragCoord.xy, gl_ViewIndex), 0).xyz;

//  color.xyz = mix(refraction, mix(refraction, reflection + diffuse + specularOutput, fresnel), clamp(hitTime * 0.1, 0.0, 1.0));

  }

  //color.xyz = reflection;

  //color.xyz = waterBaseColor * max(0.0, dot(normal, vec3(0.0, 0.0, 1.0)));

  return color;
#endif

}


void main(){
  {
    // Unpack configurable water IORs, base color and (re-)derive waterF0 from waterIOR so the
    // whole shader picks up the per-planet values configured via TpvScene3DPlanet.
    vec4 baseColor4 = vec4(unpackHalf2x16(planetData.waterBaseColorIORs.x), unpackHalf2x16(planetData.waterBaseColorIORs.y));
    vec4 iors4 = vec4(unpackHalf2x16(planetData.waterBaseColorIORs.z), unpackHalf2x16(planetData.waterBaseColorIORs.w));
    waterBaseColor = baseColor4.xyz;
    waterIOR = (iors4.x > 0.0) ? iors4.x : 1.3325;
    airIOR = (iors4.y > 0.0) ? iors4.y : 1.0;
    float f0 = IOR_TO_F0(waterIOR);
    waterF0 = f0 * f0;
    ior = waterIOR / airIOR;
  }
  {
    // Unpack wave parameters for the procedural Gerstner detail pass in getWaterNormal.
    // wp0: (windDirX, windDirY, windDirZ, waveAmplitude)
    // wp1: (waveFrequency, waveSteepness, waveSpeed, whitecapFactor)
    vec4 wp0 = vec4(unpackHalf2x16(planetData.waterWaveParams.x), unpackHalf2x16(planetData.waterWaveParams.y));
    vec4 wp1 = vec4(unpackHalf2x16(planetData.waterWaveParams.z), unpackHalf2x16(planetData.waterWaveParams.w));
    float wdLen = length(wp0.xyz);
    waveWindDir = (wdLen > 1e-3) ? (wp0.xyz / wdLen) : vec3(1.0, 0.0, 0.0);
    waveAmplitude = wp0.w;
    waveFrequency = wp1.x;
    waveSteepness = wp1.y;
    waveSpeed = wp1.z;
    waveWhitecapFactor = wp1.w;
    {
      // wp2: (uvWaveAmplitude, uvWaveFrequency, uvWaveSpeed, uvWaveSteepness)
      // wp3: (uvWaveFactor, waveWindFactor, uvWaveScale, unused)
      vec4 wp2 = vec4(unpackHalf2x16(planetData.waterUVWaveParams.x), unpackHalf2x16(planetData.waterUVWaveParams.y));
      vec4 wp3 = vec4(unpackHalf2x16(planetData.waterUVWaveParams.z), unpackHalf2x16(planetData.waterUVWaveParams.w));
      uvWaveAmplitude = wp2.x;
      uvWaveFrequency = wp2.y;
      uvWaveSpeed     = wp2.z;
      uvWaveSteepness = wp2.w;
      uvWaveFactor    = wp3.x;
      waveWindFactor  = wp3.y;
      uvWaveScale     = wp3.z;
    }
  }
  {
    // Unpack whitecap-specific parameters.
    // wcp0: (colorR, colorG, colorB, patternScale)
    // wcp1: (slopeThreshLow, slopeThreshHigh, breakupLow, breakupHigh)
    vec4 wcp0 = vec4(unpackHalf2x16(planetData.waterWhitecapParams.x), unpackHalf2x16(planetData.waterWhitecapParams.y));
    vec4 wcp1 = vec4(unpackHalf2x16(planetData.waterWhitecapParams.z), unpackHalf2x16(planetData.waterWhitecapParams.w));
    whitecapColor          = wcp0.xyz;
    whitecapPatternScale   = wcp0.w;
    whitecapSlopeThreshLow  = wcp1.x;
    whitecapSlopeThreshHigh = wcp1.y;
    whitecapBreakupLow     = wcp1.z;
    whitecapBreakupHigh    = wcp1.w;
  }

#if defined(TESSELLATION)
 
  workNormal = normalize((planetModelMatrix * vec4(getWaterNormal(inBlock.position), 0.0)).xyz) * ((inBlock.underWater > 0.0) ? -1.0 : 1.0);
//workNormal = normalize((planetModelMatrix * vec4(mapNormal(inBlock.localPosition), 0.0)).xyz) * ((inBlock.underWater > 0.0) ? -1.0 : 1.0);

  viewDirection = normalize(-inCameraRelativePosition);

  float opaqueDepth = texelFetch(uPassTextures[2], ivec3(gl_FragCoord.xy, gl_ViewIndex), 0).x;
  {
#if 0
    vec2 uv = (vec2(gl_FragCoord.xy) + vec2(0.5)) / vec2(textureSize(uPassTextures[2], 0).xy);
    vec4 opaqueViewSpace = inverseProjectionMatrix * vec4(fma(uv, vec2(2.0), vec2(-1.0)), opaqueDepth, 1.0);
    opaqueViewSpace /= opaqueViewSpace.w;
    opaqueDepth = -opaqueViewSpace.z; 
#else
    vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(opaqueDepth), inverseProjectionMatrix[3].zw);
    opaqueDepth = -(v.x / v.y);
#endif
  }

  float surfaceDepth = -inBlock.viewSpacePosition.z;

  vec4 finalColor = vec4(0.0, 0.0, 0.0, 1.0);//doShade(abs(inBlock.viewSpacePosition.z), inBlock.underWater > 0.0);
  
  if((inBlock.underWater > 0.0) /*&& (inBlock.mapValue < 0.0)*/){
    finalColor = vec4(textureLod(uPassTextures[1], vec3((vec2(gl_FragCoord.xy) + vec2(0.5)) / vec2(float(uint(pushConstants.resolutionXY & 0xffffu)), float(uint(pushConstants.resolutionXY >> 16u))), gl_ViewIndex), 1.0).xyz * waterBaseColor * waterBaseColor, 1.0);
  }else{
    finalColor = doShade(opaqueDepth, surfaceDepth, inBlock.underWater > 0.0);
  }

  outFragColor = vec4(clamp(finalColor.xyz * finalColor.w, vec3(-65504.0), vec3(65504.0)), finalColor.w);

  if((inBlock.meshletID & 0x80000000u) != 0u) {
    outFragColor = vec4(meshletDebugColor(inBlock.meshletID & 0x7fffffffu), 1.0);
  }

#elif defined(UNDERWATER)

  vec4 finalColor = vec4(textureLod(uPassTextures[1], vec3(inBlock.texCoord, gl_ViewIndex), 1.0).xyz * waterBaseColor * waterBaseColor, 1.0);

  // Shore-foam overlay for the underwater fullscreen pass: reconstruct the ground geometry's
  // planet-space position from the opaque depth buffer and compare against the water surface
  // height at that sphere direction. Where the two are close, we are at a shallow shore spot and
  // applyShoreFoam tints the foam color on top of the underwater look.
  {
    float rawDepth = texelFetch(uPassTextures[2], ivec3(gl_FragCoord.xy, gl_ViewIndex), 0).x;
    vec4 clipPos = vec4(fma(inBlock.texCoord, vec2(2.0), vec2(-1.0)), rawDepth, 1.0);
    vec4 viewPos = inverseProjectionMatrix * clipPos;
    viewPos /= viewPos.w;
    vec3 worldPos = (inverseViewMatrix * viewPos).xyz;
    vec3 planetPos = (planetInverseModelMatrix * vec4(worldPos, 1.0)).xyz;
    float groundRadius = length(planetPos);
    if(groundRadius > 1e-3){
      vec3 sphereNormal = planetPos / groundRadius;
      float waterRadius = getSphereHeightEx(octPlanetUnsignedEncode(sphereNormal));
      if(waterRadius > 0.0){
        float shoreDepth = max(0.0, waterRadius - groundRadius);
        finalColor.xyz = applyShoreFoam(finalColor.xyz, planetPos, shoreDepth);
      }
    }
  }

  outFragColor = vec4(clamp(finalColor.xyz * finalColor.w, vec3(-65504.0), vec3(65504.0)), finalColor.w);

#else
#ifdef MULTIVIEW
  vec3 texCoord = vec3(inTexCoord, float(gl_ViewIndex));
#else
  vec2 texCoord = inTexCoord;
#endif

#if defined(MSAA) && !defined(MSAA_FAST) 
  
  // With MSAA, this fullscreen water rendering pass per ray marching will be become SSAA actually effectively,
  // where each sample is processed separately.

  vec2 resolution = vec2(textureSize(uPassTextures[2], 0).xy);

  texCoord.xy += vec2(gl_SamplePosition.xy) / resolution;

#endif
  
  bool reversedZ = projectionMatrix[2][3] < -1e-7;
  
  //bool infiniteFarPlane = reversedZ && ((abs(projectionMatrix[2][2]) < 1e-7) && (abs(projectionMatrix[3][2]) > 1e-7));

  vec4 nearPlane = vec4(fma(texCoord.xy, vec2(2.0), vec2(-1.0)), reversedZ ? 1.0 : 0.0, 1.0);

  vec4 cameraPosition = vec4((inverseProjectionMatrix * nearPlane).xyz, 1.0); 
  cameraPosition /= cameraPosition.w;

  vec4 cameraDirection = vec4((inverseProjectionMatrix * nearPlane).xyz, 0.0); 
      
/*vec4 primaryRayOrigin = inverseViewProjectionMatrix * vec4(fma(texCoord.xy, vec2(2.0), vec2(-1.0)), reversedZ ? 1.0 : 0.0, 1.0);
  primaryRayOrigin /= primaryRayOrigin.w;*/

  vec3 rayOrigin = inverseViewMatrix[3].xyz;

  vec3 rayDirection = normalize((inverseViewMatrix * cameraDirection).xyz);
  
  // Transform world space ray origin and direction to planet space for simplicity, so that the planet is at the origin and 
  // correctly oriented. This is not strictly necessary, but it simplifies the math. 
  rayOrigin = (planetInverseModelMatrix * vec4(rayOrigin, 1.0)).xyz;
  rayDirection = (planetInverseModelMatrix * vec4(rayDirection, 0.0)).xyz;

  viewDirection = -rayDirection;

  float hitRayTime;
   
  bool hit = false;

  float hitDepth = 0.0;
  
  vec4 finalColor = vec4(0.0);

  // Pre-check if the ray intersects the planet's bounding sphere
  if(intersectRaySphere(vec4(planetCenter, planetTopRadius * 1.0), 
                        rayOrigin,
                        rayDirection,     
                        hitRayTime)){

    // Get the hit time from the lower resolution water prepass, so that the ray does not need to be traced if the ray does not hit the planet
    // and so that we can skip empty space as much as possible for faster ray marching. 
    float prepassTime = 0.0;//textureLod(uTextureWaterAcceleration, vec3(inTexCoord, gl_ViewIndex), 0.0).x;

    if(prepassTime > 0.0){ 
      hitRayTime = max(hitRayTime, prepassTime);
    }

    bool underWater = map(rayOrigin) <= 0.0;

#ifdef MSAA 
#if defined(MSAA_FAST)
    // In the MSAA_FAST case, the depth is fetched from the pre-resolved MSAA depth buffer, not from the actual MSAA depth buffer, since
    // the water is not multisampled here, even if the input is multisampled but also pre-resolved. 
    float opaqueDepth = texelFetch(uPassTextures[2], ivec3(gl_FragCoord.xy, gl_ViewIndex), 0).x;
#else
    // In the MSAA case, the depth is fetched from the actual MSAA depth buffer, since the water is multisampled here, or better said,
    // supersampled, since all fragment samples are processed separately, not just the geometric edges as like at MSAA otherwise with
    // geometry triangles.
    float opaqueDepth = subpassLoad(uOITImgDepth, gl_SampleID).x; 
#endif
#else
    // And without MSAA at all, the depth is just fetched from the non-MSAA depth buffer, since we are not multisampled here at all anyway.
    float opaqueDepth = subpassLoad(uOITImgDepth).x; 
#endif 

    float opaqueLinearDepth = -linearizeDepth(opaqueDepth);

    bool inside = length(rayOrigin - planetCenter) <= planetTopRadius;

    rayOrigin += (inside ? vec3(0.0) : (rayDirection * hitRayTime));

    float maxTime = min(
      opaqueLinearDepth,
      max(
        length((planetCenter - (rayDirection * planetBottomRadius)) - rayOrigin),
        length((planetCenter - (rayDirection * planetTopRadius)) - rayOrigin)
      )
    );

#ifndef ONLY_UNDERWATER
    float hitTime;

    vec3 hitPoint;

#if 0
    if((prepassTime >= 0.0) &&
      //acceleratedRayMarching(rayOrigin, rayDirection, 0.0, maxTime, 0.6, underWater ? 0.9 : 1.0, hitTime)
      standardRayMarching(rayOrigin, rayDirection, 0.0, maxTime, hitTime)
      )
#else
    if(planetRayMarching(rayOrigin, rayDirection, maxTime, hitTime))
#endif
    {

      hitPoint = rayOrigin + (rayDirection * hitTime); // in planet space

      worldSpacePosition = (planetModelMatrix * vec4(hitPoint, 1.0)).xyz;

      viewSpacePosition = (viewMatrix * vec4(worldSpacePosition, 1.0)).xyz;

      hit = opaqueLinearDepth >= -viewSpacePosition.z;    
      
    }

    if(hit){

      hitDepth = delinearizeDepth(viewSpacePosition.z);

      workNormal = normalize((planetModelMatrix * vec4(mapNormal(hitPoint), 0.0)).xyz) * (underWater ? -1.0 : 1.0);

      cameraRelativePosition = worldSpacePosition - cameraPosition.xyz;

      finalColor = doShade(maxTime, hitTime, underWater);

//    finalColor = vec4(workNormal.xyz * 0.1, 1.0);//doShade();
  
    }else 
#endif
    if(underWater){

      vec3 r = textureLod(uPassTextures[1], vec3(inTexCoord, gl_ViewIndex), 1.0).xyz;
      finalColor = vec4(r * waterBaseColor * waterBaseColor, 1.0);

      hitDepth = opaqueDepth;

      hit = true;

    }     
    
  }  

  if(!hit){
    // If the ray does not hit the planet, discard the fragment, since it is not visible. Use demote if available. 
#if defined(USEDEMOTE)
    demote;
#else 
    discard;
#endif
  }

  outFragColor = vec4(clamp(finalColor.xyz * finalColor.w, vec3(-65504.0), vec3(65504.0)), finalColor.w);
#endif
} 