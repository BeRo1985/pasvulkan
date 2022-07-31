#version 450 core

#define NUM_SHADOW_CASCADES 4

#ifdef USE_MATERIAL_BUFFER_REFERENCE
  #undef NOBUFFERREFERENCE
#elif defined(USE_MATERIAL_SSBO)
  #define NOBUFFERREFERENCE
#endif

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#if defined(USEDEMOTE)
  #extension GL_EXT_demote_to_helper_invocation : enable
#endif
#extension GL_EXT_nonuniform_qualifier : enable

#ifndef NOBUFFERREFERENCE
  #define sizeof(Type) (uint64_t(Type(uint64_t(0))+1))
  #extension GL_EXT_shader_explicit_arithmetic_types_int64 : enable 
  #extension GL_EXT_buffer_reference2 : enable 
  #ifndef USEINT64
    #extension GL_EXT_buffer_reference_uvec2 : enable 
  #endif
#endif

#if defined(LOCKOIT)
  #extension GL_ARB_post_depth_coverage : enable
  #ifdef INTERLOCK
    #extension GL_ARB_fragment_shader_interlock : enable
    #define beginInvocationInterlock beginInvocationInterlockARB
    #define endInvocationInterlock endInvocationInterlockARB
    layout(early_fragment_tests, post_depth_coverage, pixel_interlock_ordered) in;
  #else
    #if defined(ALPHATEST)
      layout(post_depth_coverage) in;
    #else
      layout(early_fragment_tests, post_depth_coverage) in;
    #endif
  #endif
#elif !defined(ALPHATEST)
  layout(early_fragment_tests) in;
#endif

layout(location = 0) in vec3 inWorldSpacePosition;
layout(location = 1) in vec3 inViewSpacePosition;
layout(location = 2) in vec3 inCameraRelativePosition;
layout(location = 3) in vec3 inTangent;
layout(location = 4) in vec3 inBitangent;
layout(location = 5) in vec3 inNormal;
layout(location = 6) in vec2 inTexCoord0;
layout(location = 7) in vec2 inTexCoord1;
layout(location = 8) in vec4 inColor0;
layout(location = 9) in vec3 inModelScale;
layout(location = 10) flat in uint inMaterialID;
layout(location = 11) flat in int inViewIndex;
#ifdef VELOCITY
layout(location = 12) in vec4 inPreviousClipSpace;
layout(location = 13) in vec4 inCurrentClipSpace;
#endif

#ifdef DEPTHONLY
  #if defined(MBOIT) && defined(MBOITPASS1)
    layout(location = 0) out vec4 outFragMBOITMoments0;
    layout(location = 1) out vec4 outFragMBOITMoments1;
  #elif defined(VELOCITY)
    layout(location = 0) out vec2 outFragVelocity;
    layout(location = 1) out vec3 outFragNormal;
  #endif
#else
  #if defined(WBOIT)
    layout(location = 0) out vec4 outFragWBOITAccumulation;
    layout(location = 1) out vec4 outFragWBOITRevealage;
  #elif defined(MBOIT)
    #if defined(MBOITPASS1)
      layout(location = 0) out vec4 outFragMBOITMoments0;
      layout(location = 1) out vec4 outFragMBOITMoments1;
    #elif defined(MBOITPASS2)
      layout(location = 0) out vec4 outFragColor;
    #endif
  #else
    layout(location = 0) out vec4 outFragColor;
    #ifdef EXTRAEMISSIONOUTPUT
      layout(location = 1) out vec4 outFragEmission;
    #endif
  #endif
#endif

const int TEXTURE_BRDF_GGX = 0;
const int TEXTURE_BRDF_CHARLIE = 1;
const int TEXTURE_BRDF_SHEEN_E = 2;
const int TEXTURE_ENVMAP_GGX = 3;
const int TEXTURE_ENVMAP_CHARLIE = 4;
const int TEXTURE_ENVMAP_LAMBERTIAN = 5;

const int TEXTURE_BASE_INDEX = 10;

// Global descriptor set

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

#ifdef NOBUFFERREFERENCE
struct Material {
  vec4 baseColorFactor;
  vec4 specularFactor;
  vec4 emissiveFactor;
  vec4 metallicRoughnessNormalScaleOcclusionStrengthFactor;
  vec4 sheenColorFactorSheenIntensityFactor;
  vec4 clearcoatFactorClearcoatRoughnessFactor;
  vec4 iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum;
  vec4 iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance;
  vec4 volumeAttenuationColor;
  uvec4 alphaCutOffFlagsTex0Tex1;
  int textures[16];
  mat3x2 textureTransforms[16];
};
#endif

layout(std140, set = 0, binding = 0) uniform uboViews {
  View views[256];
} uView;

#ifdef LIGHTS
struct Light {
  uvec4 metaData;
  vec4 colorIntensity;
  vec4 positionRange;
  vec4 directionZFar;
  mat4 shadowMapMatrix;
};

layout(std430, set = 0, binding = 1) readonly buffer LightItemData {
//uvec4 lightMetaData;
  Light lights[];
};

struct LightTreeNode {
  uvec4 aabbMinSkipCount;
  uvec4 aabbMaxUserData;
};

layout(std430, set = 0, binding = 2) readonly buffer LightTreeNodeData {
  LightTreeNode lightTreeNodes[];
};

#endif

#ifdef NOBUFFERREFERENCE

layout(std430, set = 0, binding = 3) readonly buffer Materials {
  Material materials[];
};

#else

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer Material {
  vec4 baseColorFactor;
  vec4 specularFactor;
  vec4 emissiveFactor;
  vec4 metallicRoughnessNormalScaleOcclusionStrengthFactor;
  vec4 sheenColorFactorSheenIntensityFactor;
  vec4 clearcoatFactorClearcoatRoughnessFactor;
  vec4 iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum;
  vec4 iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance;
  vec4 volumeAttenuationColor;
  uvec4 alphaCutOffFlagsTex0Tex1;
  int textures[16];
  mat3x2 textureTransforms[16];
};

layout(std140, set = 0, binding = 3) uniform Materials {
  Material materials;
} uMaterials;

#endif

layout(set = 0, binding = 4) uniform sampler2D u2DTextures[];

layout(set = 0, binding = 4) uniform samplerCube uCubeTextures[];

// Pass descriptor set

#ifdef DEPTHONLY
#else
layout(set = 1, binding = 0) uniform sampler2D uImageBasedLightingBRDFTextures[];  // 0 = GGX, 1 = Charlie, 2 = Sheen E

layout(set = 1, binding = 1) uniform samplerCube uImageBasedLightingEnvMaps[];  // 0 = GGX, 1 = Charlie, 2 = Lambertian

#ifdef SHADOWS
const uint SHADOWMAP_MODE_NONE = 1;
const uint SHADOWMAP_MODE_PCF = 2;
const uint SHADOWMAP_MODE_DPCF = 3;
const uint SHADOWMAP_MODE_PCSS = 4;
const uint SHADOWMAP_MODE_MSM = 5;

layout(std140, set = 1, binding = 2) uniform uboCascadedShadowMaps {
  mat4 shadowMapMatrices[NUM_SHADOW_CASCADES];
  vec4 shadowMapSplitDepthsScales[NUM_SHADOW_CASCADES];
  vec4 constantBiasNormalBiasSlopeBiasClamp[NUM_SHADOW_CASCADES];
  uvec4 metaData; // x = type
} uCascadedShadowMaps;

layout(set = 1, binding = 3) uniform sampler2DArray uCascadedShadowMapTexture;

#ifdef PCFPCSS

// Yay! Binding Aliasing! :-)
layout(set = 1, binding = 3) uniform sampler2DArrayShadow uCascadedShadowMapTextureShadow;

#endif

#endif

layout(set = 1, binding = 4) uniform sampler2DArray uPassTextures[]; // 0 = SSAO, 1 = Opaque frame buffer

#endif

#if defined(WBOIT)

  layout(std140, set = 1, binding = 5) uniform uboWBOIT {
    vec4 wboitZNearZFar;
  } uWBOIT;

#elif defined(MBOIT)

  layout(std140, set = 1, binding = 5) uniform uboMBOIT {
    vec4 mboitZNearZFar;
  } uMBOIT;

  #if defined(MBOITPASS1)
  #elif defined(MBOITPASS2)
    #ifdef MSAA
      layout(input_attachment_index = 0, set = 1, binding = 6) uniform subpassInputMS uMBOITMoments0;
      layout(input_attachment_index = 1, set = 1, binding = 7) uniform subpassInputMS uMBOITMoments1;
    #else
      layout(input_attachment_index = 0, set = 1, binding = 6) uniform subpassInput uMBOITMoments0;
      layout(input_attachment_index = 1, set = 1, binding = 7) uniform subpassInput uMBOITMoments1;
    #endif
  #endif

#elif defined(LOCKOIT)

  #ifdef MSAA
    layout(input_attachment_index = 0, set = 1, binding = 5) uniform subpassInputMS uOITImgDepth;
  #else
    layout(input_attachment_index = 0, set = 1, binding = 5) uniform subpassInput uOITImgDepth;
  #endif
  layout(set = 1, binding = 6, rgba32ui) uniform coherent uimageBuffer uOITImgABuffer;
  layout(set = 1, binding = 7, r32ui) uniform coherent uimage2DArray uOITImgAux;
  #ifdef SPINLOCK
    layout(set = 1, binding = 8, r32ui) uniform coherent uimage2DArray uOITImgSpinLock;
    layout(std140, set = 1, binding = 9) uniform uboOIT {
      ivec4 oitViewPort;
    } uOIT;
  #endif
  #ifdef INTERLOCK
    layout(std140, set = 1, binding = 8) uniform uboOIT {
      ivec4 oitViewPort;
    } uOIT;
  #endif

#elif defined(LOOPOIT)

  layout(std140, set = 1, binding = 5) uniform uboOIT {
    ivec4 oitViewPort;
  } uOIT;
  #ifdef MSAA
    layout(input_attachment_index = 0, set = 1, binding = 6) uniform subpassInputMS uOITImgDepth;
  #else
    layout(input_attachment_index = 0, set = 1, binding = 6) uniform subpassInput uOITImgDepth;
  #endif
  #if defined(LOOPOIT_PASS1)
    layout(set = 1, binding = 7, r32ui) uniform coherent uimageBuffer uOITImgZBuffer;
  #else
    layout(set = 1, binding = 7, r32ui) uniform readonly uimageBuffer uOITImgZBuffer;
    layout(set = 1, binding = 8, rg32ui) uniform coherent uimageBuffer uOITImgABuffer;
    #ifdef MSAA    
      layout(set = 1, binding = 9, r32ui) uniform coherent uimageBuffer uOITImgSBuffer;
    #endif
  #endif 

#endif

/* clang-format on */

float sq(float t){
  return t * t; //
}

vec2 sq(vec2 t){
  return t * t; //
}

vec3 sq(vec3 t){
  return t * t; //
}

vec4 sq(vec4 t){
  return t * t; //
}

#if 0
vec3 convertLinearRGBToSRGB(vec3 c) {
  return mix((pow(c, vec3(1.0 / 2.4)) * vec3(1.055)) - vec3(5.5e-2), c * vec3(12.92), lessThan(c, vec3(3.1308e-3)));  //
}

vec4 convertLinearRGBToSRGB(vec4 c) {
  return vec4(convertLinearRGBToSRGB(c.xyz), c.w);  //
}

vec3 convertSRGBToLinearRGB(vec3 c) {
  return mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)), c / vec3(12.92), lessThan(c, vec3(4.045e-2)));  //
}

vec4 convertSRGBToLinearRGB(vec4 c) {
  return vec4(convertSRGBToLinearRGB(c.xyz), c.w);  //
}
#endif

#if defined(WBOIT)
#elif defined(MBOIT)
 #include "mboit.glsl"
#endif

#ifdef DEPTHONLY
#else
#include "roughness.glsl"

float envMapMaxLevelGGX, envMapMaxLevelCharlie;

const float PI = 3.14159265358979323846,     //
    PI2 = 6.283185307179586476925286766559,  //
    OneOverPI = 1.0 / PI;

float cavity, ambientOcclusion, specularOcclusion;
uint flags, shadingModel;

vec3 iridescenceFresnel = vec3(0.0);
vec3 iridescenceF0 = vec3(0.0);
float iridescenceFactor = 0.0;
float iridescenceIor = 1.3;
float iridescenceThickness = 400.0;

#if defined(BLEND) || defined(LOOPOIT) || defined(LOCKOIT) || defined(MBOIT) || defined(WBOIT)
  #define TRANSMISSION
#endif

#if defined(TRANSMISSION)
float transmissionFactor = 0.0;

float volumeThickness = 0.0;
vec3 volumeAttenuationColor = vec3(1.0); 
float volumeAttenuationDistance = 1.0 / 0.0; // +INF
#endif

float applyIorToRoughness(float roughness, float ior) {
  // Scale roughness with IOR so that an IOR of 1.0 results in no microfacet refraction and an IOR of 1.5 results in the default amount of microfacet refraction.
  return roughness * clamp(fma(ior, 2.0, -2.0), 0.0, 1.0);
}

vec3 approximateAnalyticBRDF(vec3 specularColor, float NoV, float roughness) {
  const vec4 c0 = vec4(-1.0, -0.0275, -0.572, 0.022);
  const vec4 c1 = vec4(1.0, 0.0425, 1.04, -0.04);
  vec4 r = fma(c0, vec4(roughness), c1);
  vec2 AB = fma(vec2(-1.04, 1.04), vec2((min(r.x * r.x, exp2(-9.28 * NoV)) * r.x) + r.y), r.zw);
  return fma(specularColor, AB.xxx, AB.yyy);
}

vec3 F_Schlick(vec3 f0, vec3 f90, float VdotH) {
  return mix(f0, f90, pow(clamp(1.0 - VdotH, 0.0, 1.0), 5.0));  //
}

float F_Schlick(float f0, float f90, float VdotH) {
  float x = clamp(1.0 - VdotH, 0.0, 1.0);
  float x2 = x * x;
  return mix(f0, f90, x * x2 * x2);  
}

float F_Schlick(float f0, float VdotH) {
  return F_Schlick(f0, 1.0, VdotH);
}

vec3 F_Schlick(vec3 f0, float VdotH) {
  return F_Schlick(f0, vec3(1.0), VdotH);
}

vec3 Schlick_to_F0(vec3 f, vec3 f90, float VdotH) {
  float x = clamp(1.0 - VdotH, 0.0, 1.0);
  float x2 = x * x;
  float x5 = clamp(x * x2 * x2, 0.0, 0.9999);

  return (f - f90 * x5) / (1.0 - x5);
}

float Schlick_to_F0(float f, float f90, float VdotH) {
  float x = clamp(1.0 - VdotH, 0.0, 1.0);
  float x2 = x * x;
  float x5 = clamp(x * x2 * x2, 0.0, 0.9999);

  return (f - f90 * x5) / (1.0 - x5);
}

vec3 Schlick_to_F0(vec3 f, float VdotH) { return Schlick_to_F0(f, vec3(1.0), VdotH); }

float Schlick_to_F0(float f, float VdotH) { return Schlick_to_F0(f, 1.0, VdotH); }

float V_GGX(float NdotL, float NdotV, float alphaRoughness) {
  float alphaRoughnessSq = alphaRoughness * alphaRoughness;
  float GGX = (NdotL * sqrt(((NdotV * NdotV) * (1.0 - alphaRoughnessSq)) + alphaRoughnessSq)) +  //
              (NdotV * sqrt(((NdotL * NdotL) * (1.0 - alphaRoughnessSq)) + alphaRoughnessSq));
  return (GGX > 0.0) ? (0.5 / GGX) : 0.0;
}

float D_GGX(float NdotH, float alphaRoughness) {
  float alphaRoughnessSq = alphaRoughness * alphaRoughness;
  float f = ((NdotH * NdotH) * (alphaRoughnessSq - 1.0)) + 1.0;
  return alphaRoughnessSq / (PI * (f * f));
}

float lambdaSheenNumericHelper(float x, float alphaG) {
  float oneMinusAlphaSq = (1.0 - alphaG) * (1.0 - alphaG);
  return ((mix(21.5473, 25.3245, oneMinusAlphaSq) /          //
           (1.0 + (mix(3.82987, 3.32435, oneMinusAlphaSq) *  //
                   pow(x, mix(0.19823, 0.16801, oneMinusAlphaSq))))) +
          (mix(-1.97760, -1.27393, oneMinusAlphaSq) * x)) +  //
         mix(-4.32054, -4.85967, oneMinusAlphaSq);
}

float lambdaSheen(float cosTheta, float alphaG) {
  return (abs(cosTheta) < 0.5) ?  //
             exp(lambdaSheenNumericHelper(cosTheta, alphaG))
                               :  //
             exp((2.0 * lambdaSheenNumericHelper(0.5, alphaG)) - lambdaSheenNumericHelper(1.0 - cosTheta, alphaG));
}

float V_Sheen(float NdotL, float NdotV, float sheenRoughness) {
  sheenRoughness = max(sheenRoughness, 0.000001);
  float alphaG = sheenRoughness * sheenRoughness;
  return clamp(1.0 / (((1.0 + lambdaSheen(NdotV, alphaG)) + lambdaSheen(NdotL, alphaG)) * (4.0 * NdotV * NdotL)), 0.0, 1.0);
}

float D_Charlie(float sheenRoughness, float NdotH) {
  sheenRoughness = max(sheenRoughness, 0.000001);
  float invR = 1.0 / (sheenRoughness * sheenRoughness);
  return ((2.0 + invR) * pow(1.0 - (NdotH * NdotH), invR * 0.5)) / (2.0 * PI);
}

vec3 BRDF_lambertian(vec3 f0, vec3 f90, vec3 diffuseColor, float specularWeight, float VdotH) {
  return (1.0 - (specularWeight * mix(F_Schlick(f0, f90, VdotH), vec3(max(max(iridescenceF0.x, iridescenceF0.y), iridescenceF0.z)), iridescenceFactor))) * (diffuseColor * OneOverPI);  //
}

vec3 BRDF_specularGGX(vec3 f0, vec3 f90, float alphaRoughness, float specularWeight, float VdotH, float NdotL, float NdotV, float NdotH) {
  return specularWeight * mix(F_Schlick(f0, f90, VdotH), iridescenceFresnel, iridescenceFactor) * V_GGX(NdotL, NdotV, alphaRoughness) * D_GGX(NdotH, alphaRoughness);  //
}

vec3 BRDF_specularSheen(vec3 sheenColor, float sheenRoughness, float NdotL, float NdotV, float NdotH) {
  return sheenColor * D_Charlie(sheenRoughness, NdotH) * V_Sheen(NdotL, NdotV, sheenRoughness);  //
}

/////////////////////////////

vec3 getPunctualRadianceTransmission(vec3 normal, vec3 view, vec3 pointToLight, float alphaRoughness, vec3 f0, vec3 f90, vec3 baseColor, float ior) {
  float transmissionRougness = applyIorToRoughness(alphaRoughness, ior);

  vec3 n = normalize(normal);  // Outward direction of surface point
  vec3 v = normalize(view);    // Direction from surface point to view
  vec3 l = normalize(pointToLight);
  vec3 l_mirror = normalize(l + (2.0 * n * dot(-l, n)));  // Mirror light reflection vector on surface
  vec3 h = normalize(l_mirror + v);                       // Halfway vector between transmission light vector and v

  float D = D_GGX(clamp(dot(n, h), 0.0, 1.0), transmissionRougness);
  vec3 F = F_Schlick(f0, f90, clamp(dot(v, h), 0.0, 1.0));
  float Vis = V_GGX(clamp(dot(n, l_mirror), 0.0, 1.0), clamp(dot(n, v), 0.0, 1.0), transmissionRougness);

  // Transmission BTDF
  return (1.0 - F) * baseColor * D * Vis;
}

/////////////////////////////

// Compute attenuated light as it travels through a volume.
vec3 applyVolumeAttenuation(vec3 radiance, float transmissionDistance, vec3 attenuationColor, float attenuationDistance) {
  if (isinf(attenuationDistance) || (attenuationDistance == 0.0)) {
    // Attenuation distance is +∞ (which we indicate by zero), i.e. the transmitted color is not attenuated at all.
    return radiance;
  } else {
    // Compute light attenuation using Beer's law.
    vec3 attenuationCoefficient = -log(attenuationColor) / attenuationDistance;
    vec3 transmittance = exp(-attenuationCoefficient * transmissionDistance);  // Beer's law
    return transmittance * radiance;
  }
}

vec3 getVolumeTransmissionRay(vec3 n, vec3 v, float thickness, float ior) {
  return normalize(refract(-v, normalize(n), 1.0 / ior)) * thickness * inModelScale;
}

/////////////////////////////

// XYZ to sRGB color space
const mat3 XYZ_TO_REC709 = mat3(3.2404542, -0.9692660, 0.0556434, -1.5371385, 1.8760108, -0.2040259, -0.4985314, 0.0415560, 1.0572252);

// Assume air interface for top
// Note: We don't handle the case fresnel0 == 1
vec3 Fresnel0ToIor(vec3 fresnel0) {
  vec3 sqrtF0 = sqrt(fresnel0);
  return (vec3(1.0) + sqrtF0) / (vec3(1.0) - sqrtF0);
}

// Conversion FO/IOR
vec3 IorToFresnel0(vec3 transmittedIor, float incidentIor) { return sq((transmittedIor - vec3(incidentIor)) / (transmittedIor + vec3(incidentIor))); }

// ior is a value between 1.0 and 3.0. 1.0 is air interface
float IorToFresnel0(float transmittedIor, float incidentIor) { return sq((transmittedIor - incidentIor) / (transmittedIor + incidentIor)); }

// Fresnel equations for dielectric/dielectric interfaces.
// Ref: https://belcour.github.io/blog/research/2017/05/01/brdf-thin-film.html
// Evaluation XYZ sensitivity curves in Fourier space
vec3 evalSensitivity(float OPD, vec3 shift) {
  float phase = 2.0 * PI * OPD * 1.0e-9;
  vec3 val = vec3(5.4856e-13, 4.4201e-13, 5.2481e-13);
  vec3 pos = vec3(1.6810e+06, 1.7953e+06, 2.2084e+06);
  vec3 var = vec3(4.3278e+09, 9.3046e+09, 6.6121e+09);

  vec3 xyz = val * sqrt(2.0 * PI * var) * cos(pos * phase + shift) * exp(-sq(phase) * var);
  xyz.x += 9.7470e-14 * sqrt(2.0 * PI * 4.5282e+09) * cos(2.2399e+06 * phase + shift[0]) * exp(-4.5282e+09 * sq(phase));
  xyz /= 1.0685e-7;

  vec3 srgb = XYZ_TO_REC709 * xyz;
  return srgb;
}

vec3 evalIridescence(float outsideIOR, float eta2, float cosTheta1, float thinFilmThickness, vec3 baseF0) {
  vec3 I;

  // Force iridescenceIor -> outsideIOR when thinFilmThickness -> 0.0
  float iridescenceIor = mix(outsideIOR, eta2, smoothstep(0.0, 0.03, thinFilmThickness));
  // Evaluate the cosTheta on the base layer (Snell law)
  float sinTheta2Sq = sq(outsideIOR / iridescenceIor) * (1.0 - sq(cosTheta1));

  // Handle TIR:
  float cosTheta2Sq = 1.0 - sinTheta2Sq;
  if (cosTheta2Sq < 0.0) {
    return vec3(1.0);
  }

  float cosTheta2 = sqrt(cosTheta2Sq);

  // First interface
  float R0 = IorToFresnel0(iridescenceIor, outsideIOR);
  float R12 = F_Schlick(R0, cosTheta1);
  float R21 = R12;
  float T121 = 1.0 - R12;
  float phi12 = 0.0;
  if (iridescenceIor < outsideIOR) phi12 = PI;
  float phi21 = PI - phi12;

  // Second interface
  vec3 baseIOR = Fresnel0ToIor(clamp(baseF0, 0.0, 0.9999));  // guard against 1.0
  vec3 R1 = IorToFresnel0(baseIOR, iridescenceIor);
  vec3 R23 = F_Schlick(R1, cosTheta2);
  vec3 phi23 = vec3(0.0);
  if (baseIOR[0] < iridescenceIor) phi23[0] = PI;
  if (baseIOR[1] < iridescenceIor) phi23[1] = PI;
  if (baseIOR[2] < iridescenceIor) phi23[2] = PI;

  // Phase shift
  float OPD = 2.0 * iridescenceIor * thinFilmThickness * cosTheta2;
  vec3 phi = vec3(phi21) + phi23;

  // Compound terms
  vec3 R123 = clamp(R12 * R23, 1e-5, 0.9999);
  vec3 r123 = sqrt(R123);
  vec3 Rs = sq(T121) * R23 / (vec3(1.0) - R123);

  // Reflectance term for m = 0 (DC term amplitude)
  vec3 C0 = R12 + Rs;
  I = C0;

  // Reflectance term for m > 0 (pairs of diracs)
  vec3 Cm = Rs - T121;
  for (int m = 1; m <= 2; ++m) {
    Cm *= r123;
    vec3 Sm = 2.0 * evalSensitivity(float(m) * OPD, float(m) * phi);
    I += Cm * Sm;
  }

  // Since out of gamut colors might be produced, negative color values are clamped to 0.
  return max(I, vec3(0.0));
}

////////////////////////////

vec3 diffuseOutput = vec3(0.0);
vec3 specularOutput = vec3(0.0);
vec3 sheenOutput = vec3(0.0);
vec3 clearcoatOutput = vec3(0.0);
vec3 clearcoatFresnel = vec3(0.0);
#if defined(TRANSMISSION)
vec3 transmissionOutput = vec3(0.0);
#endif

float albedoSheenScaling = 1.0;

float albedoSheenScalingLUT(const in float NdotV, const in float sheenRoughnessFactor) {
  return texture(uImageBasedLightingBRDFTextures[2], vec2(NdotV, sheenRoughnessFactor)).x;  //
}

float getSpecularOcclusion(const in float NdotV, const in float ao, const in float roughness){
  return clamp((pow(NdotV + ao, /*roughness * roughness*/exp2((-16.0 * roughness) - 1.0)) - 1.0) + ao, 0.0, 1.0); 
} 

void doSingleLight(const in vec3 lightColor, const in vec3 lightLit, const in vec3 lightDirection, const in vec3 normal, const in vec3 diffuseColor, const in vec3 F0, const in vec3 F90, const in vec3 viewDirection, const in float refractiveAngle, const in float materialTransparency, const in float alphaRoughness, const in float materialCavity, const in vec4 sheenColorIntensityFactor, const in float sheenRoughness, const in vec3 clearcoatNormal, const in vec3 clearcoatF0, const float clearcoatRoughness, const in float specularWeight) {
  vec3 halfVector = normalize(viewDirection + lightDirection);
  float nDotL = clamp(dot(normal, lightDirection), 0.0, 1.0);
  float nDotV = clamp(dot(normal, viewDirection), 0.0, 1.0);
  float nDotH = clamp(dot(normal, halfVector), 0.0, 1.0);
  float vDotH = clamp(dot(viewDirection, halfVector), 0.0, 1.0);
  vec3 lit = vec3((materialCavity * nDotL * lightColor) * lightLit);
  diffuseOutput += BRDF_lambertian(F0, F90, diffuseColor, specularWeight, vDotH) * lit;
  specularOutput += BRDF_specularGGX(F0, F90, alphaRoughness, specularWeight, vDotH, nDotL, nDotV, nDotH) * specularOcclusion * lit;
  if ((flags & (1u << 7u)) != 0u) {
    float sheenColorMax = max(max(sheenColorIntensityFactor.x, sheenColorIntensityFactor.y), sheenColorIntensityFactor.z);
    albedoSheenScaling = min(1.0 - (sheenColorMax * albedoSheenScalingLUT(nDotV, sheenRoughness)), 1.0 - (sheenColorMax * albedoSheenScalingLUT(nDotL, sheenRoughness)));
    sheenOutput += BRDF_specularSheen(sheenColorIntensityFactor.xyz * sheenColorIntensityFactor.w, sheenRoughness, nDotL, nDotV, nDotH) * lit;
  }
  if ((flags & (1u << 8u)) != 0u) {
    float nDotL = clamp(dot(clearcoatNormal, lightDirection), 1e-5, 1.0);
    float nDotV = clamp(abs(dot(clearcoatNormal, viewDirection)) + 1e-5, 0.0, 1.0);
    float nDotH = clamp(dot(clearcoatNormal, halfVector), 0.0, 1.0);
    vec3 lit = vec3((materialCavity * nDotL * lightColor) * lightLit);
    clearcoatOutput += F_Schlick(clearcoatF0, vec3(1.0), vDotH) *  //
                       D_GGX(nDotH, clearcoatRoughness) *          //
                       V_GGX(nDotV, nDotL, clearcoatRoughness) * specularWeight * specularOcclusion * lit;
  }
}

vec4 getEnvMap(sampler2D texEnvMap, vec3 rayDirection, float texLOD) {
  rayDirection = normalize(rayDirection);
  return textureLod(texEnvMap, (vec2((atan(rayDirection.z, rayDirection.x) / PI2) + 0.5, acos(rayDirection.y) / 3.1415926535897932384626433832795)), texLOD);
}

vec3 getIBLRadianceLambertian(const in vec3 normal, const in vec3 viewDirection, const in float roughness, const in vec3 diffuseColor, const in vec3 F0, const in float specularWeight) {
  float ao = cavity * ambientOcclusion;
  float NdotV = clamp(dot(normal, viewDirection), 0.0, 1.0);
  vec2 brdfSamplePoint = clamp(vec2(NdotV, roughness), vec2(0.0), vec2(1.0));
  vec2 f_ab = texture(uImageBasedLightingBRDFTextures[0], brdfSamplePoint).rg;
  vec3 irradiance = texture(uImageBasedLightingEnvMaps[2], normal.xyz, 0.0).xyz;
  vec3 mixedF0 = mix(F0, vec3(max(max(iridescenceF0.x, iridescenceF0.y), iridescenceF0.z)), iridescenceFactor);
  vec3 Fr = max(vec3(1.0 - roughness), mixedF0) - mixedF0;
  vec3 k_S = mixedF0 + (Fr * pow(1.0 - NdotV, 5.0));
  vec3 FssEss = (specularWeight * k_S * f_ab.x) + f_ab.y;
  float Ems = 1.0 - (f_ab.x + f_ab.y);
  vec3 F_avg = specularWeight * (mixedF0 + ((1.0 - mixedF0) / 21.0));
  vec3 FmsEms = (Ems * FssEss * F_avg) / (vec3(1.0) - (F_avg * Ems));
  vec3 k_D = (diffuseColor * ((1.0 - FssEss) + FmsEms) * ao);
  return (FmsEms + k_D) * irradiance;
}

vec3 getIBLRadianceGGX(const in vec3 normal, const in float roughness, const in vec3 F0, const in float specularWeight, const in vec3 viewDirection, const in float litIntensity, const in vec3 imageLightBasedLightDirection) {
  vec3 reflectionVector = normalize(reflect(-viewDirection, normal));
  float NdotV = clamp(dot(normal, viewDirection), 0.0, 1.0),                                                                            //
      ao = cavity * ambientOcclusion,                                                                                                   //
      lit = mix(1.0, litIntensity, max(0.0, dot(reflectionVector, -imageLightBasedLightDirection) * (1.0 - (roughness * roughness)))),  //
      specularOcclusion = getSpecularOcclusion(NdotV, ao * lit, roughness);
  vec2 brdf = texture(uImageBasedLightingBRDFTextures[0], clamp(vec2(NdotV, roughness), vec2(0.0), vec2(1.0)), 0.0).xy;
  return (texture(uImageBasedLightingEnvMaps[0],  //
                  reflectionVector,               //
                  roughnessToMipMapLevel(roughness, envMapMaxLevelGGX))
              .xyz *                                                                     //
          fma(mix(F0 + ((max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - NdotV, 5.0)),  //
                  iridescenceFresnel,                                                    //
                  iridescenceFactor),                                                    //
              brdf.xxx,                                                                  //
              brdf.yyy * clamp(max(max(F0.x, F0.y), F0.z) * 50.0, 0.0, 1.0)) *           //
          specularWeight *                                                               //
          specularOcclusion *                                                            //
          1.0);
}

vec3 getIBLRadianceCharlie(vec3 normal, vec3 viewDirection, float sheenRoughness, vec3 sheenColor) {
  float ao = cavity * ambientOcclusion;
  float NdotV = clamp(dot(normal.xyz, viewDirection), 0.0, 1.0);
  vec3 reflectionVector = normalize(reflect(-viewDirection, normal));
  return texture(uImageBasedLightingEnvMaps[1],  //
                 reflectionVector,               //
                 roughnessToMipMapLevel(sheenRoughness, envMapMaxLevelCharlie))
             .xyz *    //
         sheenColor *  //
         texture(uImageBasedLightingBRDFTextures[1], clamp(vec2(NdotV, sheenRoughness), vec2(0.0), vec2(1.0)), 0.0).x *
         ao;
}

#ifdef TRANSMISSION
vec4 cubic(float v) {
  vec4 n = vec4(1.0, 2.0, 3.0, 4.0) - v;
  n *= n * n;
  vec3 t = vec3(n.x, fma(n.xy, vec2(-4.0), n.yz)) + vec2(0.0, 6.0 * n.x).xxy;
  return vec4(t, ((6.0 - t.x) - t.y) - t.z) * (1.0 / 6.0);
}

vec4 textureBicubicEx(const in sampler2DArray tex, vec3 uvw, int lod) {
  vec2 textureResolution = textureSize(tex, lod).xy,  //
      uv = fma(uvw.xy, textureResolution, vec2(-0.5)),            //
      fuv = fract(uv);
  uv -= fuv;
  vec4 xcubic = cubic(fuv.x),                                                             //
      ycubic = cubic(fuv.y),                                                              //
      c = uv.xxyy + vec2(-0.5, 1.5).xyxy,                                                 //
      s = vec4(xcubic.xz + xcubic.yw, ycubic.xz + ycubic.yw),                             //
      o = (c + (vec4(xcubic.yw, ycubic.yw) / s)) * (vec2(1.0) / textureResolution).xxyy;  //
  s.xy = s.xz / (s.xz + s.yw);
  return mix(mix(textureLod(tex, vec3(o.yw, uvw.z), float(lod)), textureLod(tex, vec3(o.xw, uvw.t), float(lod)), s.x),  //
             mix(textureLod(tex, vec3(o.yz, uvw.z), float(lod)), textureLod(tex, vec3(o.xz, uvw.z), float(lod)), s.x), s.y);
}

vec4 textureBicubic(const in sampler2DArray tex, vec3 uvw, float lod, int maxLod) {
  int ilod = int(floor(lod));
  lod -= float(ilod); 
  return (lod < float(maxLod)) ? mix(textureBicubicEx(tex, uvw, ilod), textureBicubicEx(tex, uvw, ilod + 1), lod) : textureBicubicEx(tex, uvw, maxLod);
}

vec4 betterTextureEx(const in sampler2DArray tex, vec3 uvw, int lod) {
  vec2 textureResolution = textureSize(uPassTextures[1], lod).xy;
  vec2 uv = fma(uvw.xy, textureResolution, vec2(0.5));
  vec2 fuv = fract(uv);
  return textureLod(tex, vec3((floor(uv) + ((fuv * fuv) * fma(fuv, vec2(-2.0), vec2(3.0))) - vec2(0.5)) / textureResolution, uvw.z), float(lod));
}

vec4 betterTexture(const in sampler2DArray tex, vec3 uvw, float lod, int maxLod) {
  int ilod = int(floor(lod));
  lod -= float(ilod); 
  return (lod < float(maxLod)) ? mix(betterTextureEx(tex, uvw, ilod), betterTextureEx(tex, uvw, ilod + 1), lod) : betterTextureEx(tex, uvw, maxLod);
}

vec3 getTransmissionSample(vec2 fragCoord, float roughness, float ior) {
  int maxLod = int(textureQueryLevels(uPassTextures[1]));
  float framebufferLod = float(maxLod) * applyIorToRoughness(roughness, ior);
#if 1
  vec3 transmittedLight = (framebufferLod < 1e-4) ? //
                           betterTexture(uPassTextures[1], vec3(fragCoord.xy, inViewIndex), framebufferLod, maxLod).xyz :  //                           
                           textureBicubic(uPassTextures[1], vec3(fragCoord.xy, inViewIndex), framebufferLod, maxLod).xyz; //
#else
  vec3 transmittedLight = texture(uPassTextures[1], vec3(fragCoord.xy, inViewIndex), framebufferLod).xyz;
#endif
  return transmittedLight;
}

vec3 getIBLVolumeRefraction(vec3 n, vec3 v, float perceptualRoughness, vec3 baseColor, vec3 f0, vec3 f90, vec3 position, float ior, float thickness, vec3 attenuationColor, float attenuationDistance) {
  vec3 transmissionRay = getVolumeTransmissionRay(n, v, thickness, ior);
  vec3 refractedRayExit = position + transmissionRay;

  // Project refracted vector on the framebuffer, while mapping to normalized device coordinates.
  vec4 ndcPos = uView.views[inViewIndex].projectionMatrix * uView.views[inViewIndex].viewMatrix * vec4(refractedRayExit, 1.0);
  vec2 refractionCoords = fma(ndcPos.xy / ndcPos.w, vec2(0.5), vec2(0.5));
  
  // Sample framebuffer to get pixel the refracted ray hits.
  vec3 transmittedLight = getTransmissionSample(refractionCoords, perceptualRoughness, ior);

  vec3 attenuatedColor = applyVolumeAttenuation(transmittedLight, length(transmissionRay), attenuationColor, attenuationDistance);

  // Sample GGX LUT to get the specular component.
  float NdotV = clamp(dot(n, v), 0.0, 1.0);
  vec2 brdfSamplePoint = clamp(vec2(NdotV, perceptualRoughness), vec2(0.0, 0.0), vec2(1.0, 1.0));
  vec2 brdf = texture(uImageBasedLightingBRDFTextures[0], brdfSamplePoint).xy;
  vec3 specularColor = (f0 * brdf.x) + (f90 * brdf.y);

  return (1.0 - specularColor) * attenuatedColor * baseColor;
}
#endif

#ifdef SHADOWS

#ifdef PCFPCSS

#ifdef UseReceiverPlaneDepthBias
#undef UseReceiverPlaneDepthBias
#endif

const int SHADOW_TAP_COUNT = 16;

const vec2 PoissonDiskSamples[16] = vec2[](
  vec2(-0.94201624, -0.39906216), 
  vec2(0.94558609, -0.76890725), 
  vec2(-0.094184101, -0.92938870), 
  vec2(0.34495938, 0.29387760), 
  vec2(-0.91588581, 0.45771432), 
  vec2(-0.81544232, -0.87912464), 
  vec2(-0.38277543, 0.27676845), 
  vec2(0.97484398, 0.75648379), 
  vec2(0.44323325, -0.97511554), 
  vec2(0.53742981, -0.47373420), 
  vec2(-0.26496911, -0.41893023), 
  vec2(0.79197514, 0.19090188), 
  vec2(-0.24188840, 0.99706507), 
  vec2(-0.81409955, 0.91437590), 
  vec2(0.19984126, 0.78641367), 
  vec2(0.14383161, -0.14100790)
);

vec2 computeReceiverPlaneDepthBias(const vec3 position) {
  // see: GDC '06: Shadow Mapping: GPU-based Tips and Techniques
  // Chain rule to compute dz/du and dz/dv
  // |dz/du|   |du/dx du/dy|^-T   |dz/dx|
  // |dz/dv| = |dv/dx dv/dy|    * |dz/dy|
  vec3 duvz_dx = dFdx(position);
  vec3 duvz_dy = dFdy(position);
  vec2 dz_duv = inverse(transpose(mat2(duvz_dx.xy, duvz_dy.xy))) * vec2(duvz_dx.z, duvz_dy.z);
  return dz_duv;
}

vec3 getOffsetedBiasedWorldPositionForShadowMapping(const in vec4 values, const in vec3 lightDirection){
  vec3 worldSpacePosition = inWorldSpacePosition;
  {
    vec3 worldSpaceNormal = inNormal;
    float cos_alpha = clamp(dot(worldSpaceNormal, lightDirection), 0.0, 1.0);
    float offset_scale_N = sqrt(1.0 - (cos_alpha * cos_alpha));   // sin(acos(L·N))
    float offset_scale_L = offset_scale_N / max(5e-4, cos_alpha); // tan(acos(L·N))
    vec2 offsets = fma(vec2(offset_scale_N, min(2.0, offset_scale_L)), vec2(values.yz), vec2(0.0, values.x));
    if(values.w > 1e-6){
      offsets.xy = clamp(offsets.xy, vec2(-values.w), vec2(values.w));
    }
    worldSpacePosition += (worldSpaceNormal * offsets.x) + (lightDirection * offsets.y);
  } 
  return worldSpacePosition;  
}

float doPCFSample(const in sampler2DArrayShadow shadowMapArray, const in vec3 pBaseUVS, const in float pU, const in float pV, const in float pZ, const in vec2 pShadowMapSizeInv){
#if 1
  // Use hardware PCF
  return texture(shadowMapArray, vec4(pBaseUVS + vec3(vec2(vec2(pU, pV) * pShadowMapSizeInv), 0.0), pZ));
#else
  // Emulate hardware PCF with textureGather as reference
  return dot(textureGather(shadowMapArray, pBaseUVS + vec3(vec2(vec2(pU, pV) * pShadowMapSizeInv), 0.0), pZ), vec4(0.25));
#endif
}

float DoPCF(const in sampler2DArrayShadow shadowMapArray,
            const in int cascadedShadowMapIndex,
            const in vec4 shadowMapPosition){
  vec3 lShadowMapSpaceUVZ = shadowMapPosition.xyz;   
#define OptimizedPCFFilterSize 7
#if OptimizedPCFFilterSize != 2
  vec2 lShadowMapSize = vec2(textureSize(shadowMapArray, 0).xy);
  vec2 lShadowMapSizeInv = vec2(1.0) / lShadowMapSize;
 
  float lZReceiver = lShadowMapSpaceUVZ.z;
  
  vec2 lUV = lShadowMapSpaceUVZ.xy * lShadowMapSize;

  vec3 lBaseUVS = vec3(floor(lUV + vec2(0.5)), floor(cascadedShadowMapIndex + 0.5));

  float lS = (lUV.x + 0.5) - lBaseUVS.x;
  float lT = (lUV.y + 0.5) - lBaseUVS.y;

  lBaseUVS.xy = (lBaseUVS.xy - vec2(0.5)) * lShadowMapSizeInv;
#endif
  float lSum = 0.0;
#if OptimizedPCFFilterSize == 2
  lSum = doPCFSample(shadowMapArray, vec3(lShadowMapSpaceUVZ.xy, float(pShadowMapSlice)), 0.0, 0.0, lShadowMapSpaceUVZ.z, vec2(0.0));
#elif OptimizedPCFFilterSize == 3

  float lUW0 = 3.0 - (2.0 * lS);
  float lUW1 = 1.0 + (2.0 * lS);

  float lU0 = ((2.0 - lS) / lUW0) - 1.0;
  float lU1 = (lS / lUW1) + 1.0;

  float lVW0 = 3.0 - (2.0 * lT);
  float lVW1 = 1.0 + (2.0 * lT);

  float lV0 = ((2.0 - lT) / lVW0) - 1.0;
  float lV1 = (lT / lVW1) + 1.0;

  lSum += (lUW0 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV0, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV0, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW0 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV1, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV1, lZReceiver, lShadowMapSizeInv);

  lSum *= 1.0 / 16.0;
#elif OptimizedPCFFilterSize == 5

  float lUW0 = 4.0 - (3.0 * lS);
  float lUW1 = 7.0;
  float lUW2 = 1.0 + (3.0 * lS);

  float lU0 = ((3.0 - (2.0 * lS)) / lUW0) - 2.0;
  float lU1 = (3.0 + lS) / lUW1;
  float lU2 = (lS / lUW2) + 2.0;

  float lVW0 = 4.0 - (3.0 * lT);
  float lVW1 = 7.0;
  float lVW2 = 1.0 + (3.0 * lT);

  float lV0 = ((3.0 - (2.0 * lT)) / lVW0) - 2.0;
  float lV1 = (3.0 + lT) / lVW1;
  float lV2 = (lT / lVW2) + 2.0;

  lSum += (lUW0 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV0, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV0, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW2 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU2, lV0, lZReceiver, lShadowMapSizeInv);

  lSum += (lUW0 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV1, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV1, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW2 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU2, lV1, lZReceiver, lShadowMapSizeInv);

  lSum += (lUW0 * lVW2) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV2, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW2) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV2, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW2 * lVW2) * doPCFSample(shadowMapArray, lBaseUVS, lU2, lV2, lZReceiver, lShadowMapSizeInv);

  lSum *= 1.0 / 144.0;

#elif OptimizedPCFFilterSize == 7

  float lUW0 = (5.0 * lS) - 6;
  float lUW1 = (11.0 * lS) - 28.0;
  float lUW2 = -((11.0 * lS) + 17.0);
  float lUW3 = -((5.0 * lS) + 1.0);

  float lU0 = ((4.0 * lS) - 5.0) / lUW0 - 3.0;
  float lU1 = ((4.0 * lS) - 16.0) / lUW1 - 1.0;
  float lU2 = (-(((7.0 * lS) + 5.0)) / lUW2) + 1.0;
  float lU3 = (-(lS / lUW3)) + 3.0;

  float lVW0 = ((5.0 * lT) - 6.0);
  float lVW1 = ((11.0 * lT) - 28.0);
  float lVW2 = -((11.0 * lT) + 17.0);
  float lVW3 = -((5.0 * lT) + 1.0);

  float lV0 = (((4.0 * lT) - 5.0) / lVW0) - 3.0;
  float lV1 = (((4.0 * lT) - 16.0) / lVW1) - 1.0;
  float lV2 = ((-((7.0 * lT) + 5)) / lVW2) + 1.0;
  float lV3 = (-(lT / lVW3)) + 3.0;

  lSum += (lUW0 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV0, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV0, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW2 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU2, lV0, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW3 * lVW0) * doPCFSample(shadowMapArray, lBaseUVS, lU3, lV0, lZReceiver, lShadowMapSizeInv);

  lSum += (lUW0 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV1, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV1, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW2 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU2, lV1, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW3 * lVW1) * doPCFSample(shadowMapArray, lBaseUVS, lU3, lV1, lZReceiver, lShadowMapSizeInv);

  lSum += (lUW0 * lVW2) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV2, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW2) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV2, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW2 * lVW2) * doPCFSample(shadowMapArray, lBaseUVS, lU2, lV2, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW3 * lVW2) * doPCFSample(shadowMapArray, lBaseUVS, lU3, lV2, lZReceiver, lShadowMapSizeInv);

  lSum += (lUW0 * lVW3) * doPCFSample(shadowMapArray, lBaseUVS, lU0, lV3, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW1 * lVW3) * doPCFSample(shadowMapArray, lBaseUVS, lU1, lV3, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW2 * lVW3) * doPCFSample(shadowMapArray, lBaseUVS, lU2, lV3, lZReceiver, lShadowMapSizeInv);
  lSum += (lUW3 * lVW3) * doPCFSample(shadowMapArray, lBaseUVS, lU3, lV3, lZReceiver, lShadowMapSizeInv);

  lSum *= 1.0 / 2704.0;

#endif

  return 1.0 - clamp(lSum, 0.0, 1.0);
}
                                
float ContactHardenPCFKernel(const float occluders,
                             const float occluderDistSum,
                             const float lightDistanceNormalized,
                             const float mul){

  if(occluderDistSum == 0.0){
    return 1.0;
  }else{

    float occluderAvgDist = occluderDistSum / occluders;

    float w = 1.0 / (mul * SHADOW_TAP_COUNT);

    float pcfWeight = clamp(occluderAvgDist / max(1e-6, lightDistanceNormalized), 0.0, 1.0);

    float percentageOccluded = clamp(occluders * w, 0.0, 1.0);

    percentageOccluded = fma(percentageOccluded, 2.0, -1.0);
    float occludedSign = sign(percentageOccluded);
    percentageOccluded = fma(percentageOccluded, -occludedSign, 1.0);

    return 1.0 - fma((1.0 - mix(percentageOccluded * percentageOccluded * percentageOccluded, percentageOccluded, pcfWeight)) * occludedSign, 0.5, 0.5);

  }  
}

// Shadow2DPCFMultipleTapPCFContactHardend
float DoDPCF(const in sampler2DArray shadowMapArray, 
             const in int cascadedShadowMapIndex,
             const in vec4 shadowPosition){
  
  vec3 texelSize = vec3(vec2(1.0) / textureSize(shadowMapArray, 0).xy, 0);
  
  float rotationAngle = fract(sin(dot(vec4(inTexCoord0.xy, gl_FragCoord.xy), vec4(12.9898, 78.233, 45.164, 94.673))) * 43758.5453) * 6.28318530718;
  vec2 rotation = vec2(sin(rotationAngle + vec2(0.0, 1.57079632679)));
  mat2 rotationMatrix = mat2(rotation.y, rotation.x, -rotation.x, rotation.y);
  
  float occluders = 0.0;  
  float occluderDistSum = 0.0;
  
  vec2 penumbraSize = uCascadedShadowMaps.shadowMapSplitDepthsScales[cascadedShadowMapIndex].w * texelSize.xy;
    
#ifdef UseReceiverPlaneDepthBias
  vec2 dz_duv = computeReceiverPlaneDepthBias(shadowPosition.xyz); 
  if((isnan(dz_duv.x) || isinf(dz_duv.x) || isnan(dz_duv.y) || isinf(dz_duv.y))){
    dz_duv = vec2(0.0);
  }
#if 1
  const float countFactor = 1.0;
  for(int tapIndex = 0; tapIndex < SHADOW_TAP_COUNT; tapIndex++){
    vec2 offset = PoissonDiskSamples[tapIndex] * rotationMatrix * penumbraSize;
    vec2 uv = shadowPosition.xy + offset;
    float sampleDepth = textureLod(shadowMapArray, vec3(uv, float(cascadedShadowMapIndex)), 0.0).x;
    float sampleDistance = sampleDepth - shadowPosition.z;
    float sampleOccluder = step(dot(offset, dz_duv), sampleDistance);
    occluders += sampleOccluder;
    occluderDistSum += sampleDistance * sampleOccluder;
  }
#else  
  const float countFactor = 4.0;
  for(int tapIndex = 0; tapIndex < SHADOW_TAP_COUNT; tapIndex++){
    vec2 offset = PoissonDiskSamples[tapIndex] * rotationMatrix * penumbraSize;
    vec4 samples = textureGather(shadowMapArray, vec3(shadowPosition.xy + offset, float(cascadedShadowMapIndex))); // 01, 11, 10, 00  
    vec4 sampleDistances = samples - vec4(shadowPosition.z);
    vec4 sampleOccluders = step(vec4(dot(offset + texelSize.zy, dz_duv), 
                                     dot(offset + texelSize.xy, dz_duv), 
                                     dot(offset + texelSize.xz, dz_duv), 
                                     dot(offset, dz_duv)), sampleDistances);
    occluders += dot(sampleOccluders, vec4(1.0));
    occluderDistSum += dot(sampleDistances * sampleOccluders, vec4(1.0));
  }
#endif
#else  
  const float countFactor = 4.0;
  for(int tapIndex = 0; tapIndex < SHADOW_TAP_COUNT; tapIndex++){
    vec2 offset = PoissonDiskSamples[tapIndex] * rotationMatrix * penumbraSize;
    vec4 samples = textureGather(shadowMapArray, vec3(shadowPosition.xy + offset, float(cascadedShadowMapIndex))); // 01, 11, 10, 00  
    vec4 sampleDistances = samples - vec4(shadowPosition.z);
    vec4 sampleOccluders = step(0.0, sampleDistances);
    occluders += dot(sampleOccluders, vec4(1.0));
    occluderDistSum += dot(sampleDistances * sampleOccluders, vec4(1.0));
  }
#endif
  return 1.0 - ContactHardenPCFKernel(occluders, occluderDistSum, shadowPosition.z, countFactor);
}  

float DoPCSS(const in sampler2DArray shadowMapArray, 
             const in int cascadedShadowMapIndex,
             const in vec4 shadowPosition){
  // TODO
  return 1.0;
} 

float doCascadedShadowMapShadow(const in int cascadedShadowMapIndex, const in vec3 lightDirection) {
  float value = 1.0;
  vec3 worldSpacePosition = getOffsetedBiasedWorldPositionForShadowMapping(uCascadedShadowMaps.constantBiasNormalBiasSlopeBiasClamp[cascadedShadowMapIndex], lightDirection);
  vec4 shadowPosition = uCascadedShadowMaps.shadowMapMatrices[cascadedShadowMapIndex] * vec4(worldSpacePosition, 1.0);
  shadowPosition = fma(shadowPosition / shadowPosition.w, vec2(0.5, 1.0).xxyy, vec2(0.5, 0.0).xxyy);
  if(all(greaterThanEqual(shadowPosition, vec4(0.0))) && all(lessThanEqual(shadowPosition, vec4(1.0)))){
    switch(uCascadedShadowMaps.metaData.x){
      case SHADOWMAP_MODE_PCF:{
        value = DoPCF(uCascadedShadowMapTextureShadow, cascadedShadowMapIndex, shadowPosition);
        break;
      }
      case SHADOWMAP_MODE_DPCF:{
        value = DoDPCF(uCascadedShadowMapTexture, cascadedShadowMapIndex, shadowPosition);
        break;
      }
      case SHADOWMAP_MODE_PCSS:{
        value = DoPCSS(uCascadedShadowMapTexture, cascadedShadowMapIndex, shadowPosition);
        break;
      }
      default:{
        break;
      }
    }
  }
  return value;
}

#else

float computeMSM(in vec4 moments, in float fragmentDepth, in float depthBias, in float momentBias) {
  vec4 b = mix(moments, vec4(0.5), momentBias);
  vec3 z;
  z[0] = fragmentDepth - depthBias;
  float L32D22 = fma(-b[0], b[1], b[2]);
  float D22 = fma(-b[0], b[0], b[1]);
  float squaredDepthVariance = fma(-b[1], b[1], b[3]);
  float D33D22 = dot(vec2(squaredDepthVariance, -L32D22), vec2(D22, L32D22));
  float InvD22 = 1.0 / D22;
  float L32 = L32D22 * InvD22;
  vec3 c = vec3(1.0, z[0], z[0] * z[0]);
  c[1] -= b.x;
  c[2] -= b.y + (L32 * c[1]);
  c[1] *= InvD22;
  c[2] *= D22 / D33D22;
  c[1] -= L32 * c[2];
  c[0] -= dot(c.yz, b.xy);
  float InvC2 = 1.0 / c[2];
  float p = c[1] * InvC2;
  float q = c[0] * InvC2;
  float D = (p * p * 0.25) - q;
  float r = sqrt(D);
  z[1] = (p * -0.5) - r;
  z[2] = (p * -0.5) + r;
  vec4 switchVal = (z[2] < z[0]) ? vec4(z[1], z[0], 1.0, 1.0) : ((z[1] < z[0]) ? vec4(z[0], z[1], 0.0, 1.0) : vec4(0.0));
  float quotient = (switchVal[0] * z[2] - b[0] * (switchVal[0] + z[2]) + b[1]) / ((z[2] - switchVal[1]) * (z[0] - z[1]));
  return 1.0 - clamp((switchVal[2] + (switchVal[3] * quotient)), 0.0, 1.0);
}

float linearStep(float a, float b, float v) {
  return clamp((v - a) / (b - a), 0.0, 1.0);  //
}

float reduceLightBleeding(float pMax, float amount) {
  return linearStep(amount, 1.0, pMax);  //
}

float getMSMShadowIntensity(vec4 moments, float depth, float depthBias, float momentBias) {
  vec4 b = mix(moments, vec4(0.5), momentBias);
  float                                                  //
      d = depth - depthBias,                             //
      l32d22 = fma(-b.x, b.y, b.z),                      //
      d22 = fma(-b.x, b.x, b.y),                         //
      squaredDepthVariance = fma(-b.y, b.y, b.w),        //
      d33d22 = dot(vec2(squaredDepthVariance, -l32d22),  //
                   vec2(d22, l32d22)),                   //
      invD22 = 1.0 / d22,                                //
      l32 = l32d22 * invD22;
  vec3 c = vec3(1.0, d - b.x, d * d);
  c.z -= b.y + (l32 * c.y);
  c.yz *= vec2(invD22, d22 / d33d22);
  c.y -= l32 * c.z;
  c.x -= dot(c.yz, b.xy);
  vec2 pq = c.yx / c.z;
  vec3 z = vec3(d, vec2(-(pq.x * 0.5)) + (vec2(-1.0, 1.0) * sqrt(((pq.x * pq.x) * 0.25) - pq.y)));
  vec4 s = (z.z < z.x) ? vec3(z.y, z.x, 1.0).xyzz : ((z.y < z.x) ? vec4(z.x, z.y, 0.0, 1.0) : vec4(0.0));
  return 1.0 - clamp((s.z + (s.w * ((((s.x * z.z) - (b.x * (s.x + z.z))) + b.y) / ((z.z - s.y) * (z.x - z.y))))), 0.0, 1.0); // * 1.03
}

float doCascadedShadowMapShadow(const in int cascadedShadowMapIndex, const in vec3 lightDirection) {
  mat4 shadowMapMatrix = uCascadedShadowMaps.shadowMapMatrices[cascadedShadowMapIndex];
  vec4 shadowNDC = shadowMapMatrix * vec4(inWorldSpacePosition, 1.0);
  shadowNDC /= shadowNDC.w;
  shadowNDC.xy = fma(shadowNDC.xy, vec2(0.5), vec2(0.5));
  if (all(greaterThanEqual(shadowNDC, vec4(0.0))) && all(lessThanEqual(shadowNDC, vec4(1.0)))) {
    vec4 moments = (textureLod(uCascadedShadowMapTexture, vec3(shadowNDC.xy, float(int(cascadedShadowMapIndex))), 0.0) +  //
                    vec2(-0.035955884801, 0.0).xyyy) *                                                                    //
                   mat4(0.2227744146, 0.0771972861, 0.7926986636, 0.0319417555,                                           //
                        0.1549679261, 0.1394629426, 0.7963415838, -0.172282317,                                           //
                        0.1451988946, 0.2120202157, 0.7258694464, -0.2758014811,                                          //
                        0.163127443, 0.2591432266, 0.6539092497, -0.3376131734);
    float depthBias = clamp(0.005 * tan(acos(clamp(dot(inNormal, -lightDirection), -1.0, 1.0))), 0.0, 0.1) * 0.15;
    return clamp(reduceLightBleeding(getMSMShadowIntensity(moments, shadowNDC.z, depthBias, 3e-4), 0.25), 0.0, 1.0);
  } else {
    return 1.0;
  }
}

#endif
#endif
#endif

#ifdef NOBUFFERREFERENCE
#define material materials[inMaterialID]
//Material material = materials[inMaterialID];
#else
  #ifdef USEINT64
    Material material = uMaterials.materials[inMaterialID];
  #else
    Material material;
  #endif
#endif

const uint smPBRMetallicRoughness = 0u,  //
    smPBRSpecularGlossiness = 1u,        //
    smUnlit = 2u;                        //

#if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || !defined(DEPTHONLY) 

uvec2 textureFlags;
vec2 texCoords[2];
vec2 texCoords_dFdx[2];
vec2 texCoords_dFdy[2];

int getTexCoordID(const in int textureIndex){
  return material.textures[textureIndex]; 
}

vec2 textureUV(const in int textureIndex) {
  int textureID = getTexCoordID(textureIndex); 
  return (textureID >= 0) ? (material.textureTransforms[textureIndex] * vec3(texCoords[(textureID >> 16) & 0xf], 1.0)).xy : inTexCoord0;
}

ivec2 texture2DSize(const in int textureIndex) {
  int textureID = getTexCoordID(textureIndex); 
  return (textureID >= 0) ? ivec2(textureSize(u2DTextures[nonuniformEXT(textureID & 0x3fff)], 0).xy) : ivec2(0);
}

vec4 textureFetch(const in int textureIndex, const in vec4 defaultValue, const bool sRGB) {
  int textureID = getTexCoordID(textureIndex);
  if(textureID >= 0){
    int texCoordIndex = int((textureID >> 16) & 0xf); 
    mat3x2 m = material.textureTransforms[textureIndex];
    return textureGrad(u2DTextures[nonuniformEXT(((textureID & 0x3fff) << 1) | (int(sRGB) & 1))], //
                        (m * vec3(texCoords[texCoordIndex], 1.0)).xy,   //
                        (m * vec3(texCoords_dFdx[texCoordIndex], 0.0)).xy,  //
                        (m * vec3(texCoords_dFdy[texCoordIndex], 0.0)).xy);
 }else{
   return defaultValue;
 } 
}

#endif

void main() {
#if !(defined(NOBUFFERREFERENCE) || defined(USEINT64))
  material = uMaterials.materials;
  {
    uvec2 materialPointer = uvec2(material);  
    uint carry;
    materialPointer.x = uaddCarry(materialPointer.x, uint(inMaterialID * uint(sizeof(Material))), carry);
    materialPointer.y += carry;
    material = Material(materialPointer);
  }
#endif
#if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || !defined(DEPTHONLY) 
  textureFlags = material.alphaCutOffFlagsTex0Tex1.zw;
  texCoords[0] = inTexCoord0;
  texCoords[1] = inTexCoord1;
  texCoords_dFdx[0] = dFdx(inTexCoord0);
  texCoords_dFdx[1] = dFdx(inTexCoord1);
  texCoords_dFdy[0] = dFdy(inTexCoord0);
  texCoords_dFdy[1] = dFdy(inTexCoord1);
#endif
#ifndef DEPTHONLY
  envMapMaxLevelGGX = textureQueryLevels(uImageBasedLightingEnvMaps[0]);
  envMapMaxLevelCharlie = textureQueryLevels(uImageBasedLightingEnvMaps[1]);
  flags = material.alphaCutOffFlagsTex0Tex1.y;
  shadingModel = (flags >> 0u) & 0xfu;
#endif
#ifdef DEPTHONLY
#if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) 
  float alpha = textureFetch(0, vec4(1.0), true).w * material.baseColorFactor.w * inColor0.w;
#endif
#else
  vec4 color = vec4(0.0);
#ifdef EXTRAEMISSIONOUTPUT
  vec4 emissionColor = vec4(0.0);
#endif
  float litIntensity = 1.0;
  switch (shadingModel) {
    case smPBRMetallicRoughness:
    case smPBRSpecularGlossiness: {
      vec4 diffuseColorAlpha = vec4(1.0);
      float ior = material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.x;
      vec3 F0 = vec3((abs(ior - 1.5) < 1e-6) ? 0.04 : pow((ior - 1.0) / (ior + 1.0), 2.0));
      vec3 F90 = vec3(1.0);
      float perceptualRoughness = 1.0;
      float specularWeight = 1.0;
      switch (shadingModel) {
        case smPBRMetallicRoughness: {
          vec3 specularColorFactor = material.specularFactor.xyz;
          specularWeight = material.specularFactor.w;
          if ((flags & (1u << 9u)) != 0u) {
            specularWeight *= textureFetch(9, vec4(1.0), false).w;
            specularColorFactor *= textureFetch(10, vec4(1.0), true).xyz;
          }
          vec3 dielectricSpecularF0 = clamp(F0 * specularColorFactor, vec3(0.0), vec3(1.0));
          vec4 baseColor = textureFetch(0, vec4(1.0), true) * material.baseColorFactor;
          vec2 metallicRoughness = clamp(textureFetch(1, vec4(1.0), false).zy * material.metallicRoughnessNormalScaleOcclusionStrengthFactor.xy, vec2(0.0, 1e-3), vec2(1.0));
          diffuseColorAlpha = vec4(max(vec3(0.0), baseColor.xyz * (1.0 - metallicRoughness.x)), baseColor.w);
          F0 = mix(dielectricSpecularF0, baseColor.xyz, metallicRoughness.x);
          perceptualRoughness = metallicRoughness.y;
          break;
        }
        case smPBRSpecularGlossiness: {
          vec4 specularGlossiness = textureFetch(1, vec4(1.0), true) * vec4(material.specularFactor.xyz, material.metallicRoughnessNormalScaleOcclusionStrengthFactor.y);
          diffuseColorAlpha = textureFetch(0, vec4(1.0), true) * material.baseColorFactor;
          F0 = specularGlossiness.xyz;
          diffuseColorAlpha.xyz *= max(0.0, 1.0 - max(max(F0.x, F0.y), F0.z));
          perceptualRoughness = clamp(1.0 - specularGlossiness.w, 1e-3, 1.0);
          break;
        }
      }

#undef UseGeometryRoughness
#ifdef UseGeometryRoughness
      const float minimumRoughness = 0.0525;
      float geometryRoughness;
      {
        vec3 dxy = max(abs(dFdx(inNormal)), abs(dFdy(inNormal)));
        geometryRoughness = max(max(dxy.x, dxy.y), dxy.z);
      }

      perceptualRoughness = min(max(perceptualRoughness, minimumRoughness) + geometryRoughness, 1.0);
#else        
      // Vlachos 2015, "Advanced VR Rendering"
      // Kaplanyan 2016, "Stable specular highlights"
      // Tokuyoshi 2017, "Error Reduction and Simplification for Shading Anti-Aliasing"
      // Tokuyoshi and Kaplanyan 2019, "Improved Geometric Specular Antialiasing"
      // Tokuyoshi and Kaplanyan 2021, "Stable Geometric Specular Antialiasing with Projected-Space NDF Filtering"
      // ===========================================================================================================
      // In the original paper, this implementation is intended for deferred rendering, but here it is also used 
      // for forward rendering (as described in Tokuyoshi and Kaplanyan 2019 and 2021). This is mainly because 
      // the forward version requires an expensive transformation of the half-vector by the tangent frame for each
      // light. Thus, this is an approximation based on world-space normals, but it works well enough for what is 
      // needed and is an clearly improvement over the implementation based on Vlachos 2015.
      float kernelRoughness;
      {
        const float SIGMA2 = 0.15915494, KAPPA = 0.18;        
        vec3 dx = dFdx(inNormal), dy = dFdy(inNormal);
        kernelRoughness = min(KAPPA, (2.0 * SIGMA2) * (dot(dx, dx) + dot(dy, dy)));
        float roughness = perceptualRoughness * perceptualRoughness;
        perceptualRoughness = sqrt(sqrt(clamp((roughness * roughness) + kernelRoughness, 0.0, 1.0)));
      }
#endif

      float alphaRoughness = perceptualRoughness * perceptualRoughness;

      vec3 normal;
      if ((textureFlags.x & (1 << 2)) != 0) {
        vec4 normalTexture = textureFetch(2, vec2(0.0, 1.0).xxyx, false);
        normal = normalize(                                                                                                                      //
            mat3(normalize(inTangent), normalize(inBitangent), normalize(inNormal)) *                                                            //
            normalize((normalTexture.xyz - vec3(0.5)) * (vec2(material.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0))  //
        );
      } else {
        normal = normalize(inNormal);
      }
      normal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;

      vec4 occlusionTexture = textureFetch(3, vec4(1.0), false);

      cavity = clamp(mix(1.0, occlusionTexture.x, material.metallicRoughnessNormalScaleOcclusionStrengthFactor.w), 0.0, 1.0);

      vec4 emissiveTexture = textureFetch(4, vec4(1.0), true);

      float transparency = 0.0;
      float refractiveAngle = 0.0;
      float shadow = 1.0;
  #if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || defined(BLEND)
      ambientOcclusion = 1.0;
  #else
      ambientOcclusion = ((textureFlags.x & (1 << 3)) != 0) ? 1.0 : texelFetch(uPassTextures[0], ivec3(gl_FragCoord.xy, int(gl_ViewIndex)), 0).x;
  #endif

      vec3 viewDirection = normalize(-inCameraRelativePosition);

      if ((flags & (1u << 10u)) != 0u) {
        iridescenceFresnel = F0;
        iridescenceF0 = F0;
        iridescenceFactor = material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.y * (((textureFlags.x & (1 << 11)) != 0) ? textureFetch(11, vec4(1.0), false).x : 1.0);
        iridescenceIor = material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.z;
        if ((textureFlags.x & (1 << 12)) != 0){
          iridescenceThickness = mix(material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.w, material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.x, textureFetch(12, vec4(1.0), false).y);  
        }else{
          iridescenceThickness = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.x;  
        }
        if(iridescenceThickness == 0.0){
          iridescenceFactor = 0.0;
        }  
        if(iridescenceFactor > 0.0){
          float NdotV = clamp(dot(normal, viewDirection), 0.0, 1.0);
          iridescenceFresnel = evalIridescence(1.0, iridescenceIor, NdotV, iridescenceThickness, F0);
          iridescenceF0 = Schlick_to_F0(iridescenceFresnel, NdotV);          
        }
      }

#if defined(TRANSMISSION)
      if ((flags & (1u << 11u)) != 0u) {
        transmissionFactor = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.y * (((textureFlags.x & (1 << 13)) != 0) ? textureFetch(13, vec4(1.0), false).x : 1.0);  
      }
      if ((flags & (1u << 12u)) != 0u) {
        volumeThickness = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.z * (((textureFlags.x & (1 << 14)) != 0) ? textureFetch(14, vec4(1.0), false).y : 1.0);  
        volumeAttenuationDistance = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.w;        
        volumeAttenuationColor = material.volumeAttenuationColor.xyz;        
      }
#endif

      vec3 imageLightBasedLightDirection = vec3(0.0, 0.0, -1.0);

      vec4 sheenColorIntensityFactor = vec4(1.0);
      float sheenRoughness = 0.0;
      if ((flags & (1u << 7u)) != 0u) {
        sheenColorIntensityFactor = material.sheenColorFactorSheenIntensityFactor;
        if ((textureFlags.x & (1 << 5)) != 0) {
          sheenColorIntensityFactor *= textureFetch(5, vec4(1.0), true);
        }
        sheenRoughness = max(perceptualRoughness, 1e-7);
      }

      vec3 clearcoatF0 = vec3(0.04);
      vec3 clearcoatF90 = vec3(0.0);
      vec3 clearcoatNormal = normal;
      float clearcoatFactor = 1.0;
      float clearcoatRoughness = 1.0;
      if ((flags & (1u << 8u)) != 0u) {
        clearcoatFactor = material.clearcoatFactorClearcoatRoughnessFactor.x;
        clearcoatRoughness = material.clearcoatFactorClearcoatRoughnessFactor.y;
        if ((textureFlags.x & (1 << 6)) != 0) {
          clearcoatFactor *= textureFetch(6, vec4(1.0), false).x;
        }
        if ((textureFlags.x & (1 << 7)) != 0) {
          clearcoatRoughness *= textureFetch(7, vec4(1.0), false).y;
        }
        if ((textureFlags.x & (1 << 8)) != 0) {
          vec4 normalTexture = textureFetch(8, vec2(0.0, 1.0).xxyx, false);
          clearcoatNormal = normalize(mat3(normalize(inTangent), normalize(inBitangent), normalize(inNormal)) * normalize((normalTexture.xyz - vec3(0.5)) * (vec2(material.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0)));
        } else {
          clearcoatNormal = normalize(inNormal);
        }
        clearcoatNormal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;
#ifdef UseGeometryRoughness        
        clearcoatRoughness = min(max(clearcoatRoughness, minimumRoughness) + geometryRoughness, 1.0);
#else
        {
          float roughness = clearcoatRoughness * clearcoatRoughness;
          clearcoatRoughness = sqrt(sqrt(clamp((roughness * roughness) + kernelRoughness, 0.0, 1.0)));
        }
#endif
      }

      specularOcclusion = getSpecularOcclusion(clamp(dot(normal, viewDirection), 0.0, 1.0), cavity * ambientOcclusion, alphaRoughness);

#ifdef LIGHTS
      uint lightTreeNodeIndex = 0;
      uint lightTreeNodeCount = lightTreeNodes[0].aabbMinSkipCount.w;
      while (lightTreeNodeIndex < lightTreeNodeCount) {
        LightTreeNode lightTreeNode = lightTreeNodes[lightTreeNodeIndex];
        vec3 aabbMin = vec3(uintBitsToFloat(uvec3(lightTreeNode.aabbMinSkipCount.xyz)));
        vec3 aabbMax = vec3(uintBitsToFloat(uvec3(lightTreeNode.aabbMaxUserData.xyz)));
        if (all(greaterThanEqual(inWorldSpacePosition.xyz, aabbMin)) && all(lessThanEqual(inWorldSpacePosition.xyz, aabbMax))) {
          if (lightTreeNode.aabbMaxUserData.w != 0xffffffffu) {
            Light light = lights[lightTreeNode.aabbMaxUserData.w];
            float lightAttenuation = 1.0;
            vec3 lightDirection;
            vec3 lightPosition = light.positionRange.xyz; 
            vec3 lightVector = lightPosition - inWorldSpacePosition.xyz;
            vec3 normalizedLightVector = normalize(lightVector);
#ifdef SHADOWS
            if (/*(uShadows != 0) &&*/ ((light.metaData.y & 0x80000000u) == 0u) && (uCascadedShadowMaps.metaData.x != SHADOWMAP_MODE_NONE)) {
              switch (light.metaData.x) {
#if 0
                case 1u: { // Directional 
                  // imageLightBasedLightDirection = light.directionZFar.xyz;
                  // fall-through
                }
                case 3u: {  // Spot
                  vec4 shadowNDC = light.shadowMapMatrix * vec4(inWorldSpacePosition, 1.0);                  
                  shadowNDC /= shadowNDC.w;
                  if (all(greaterThanEqual(shadowNDC, vec4(-1.0))) && all(lessThanEqual(shadowNDC, vec4(1.0)))) {
                    shadowNDC.xyz = fma(shadowNDC.xyz, vec3(0.5), vec3(0.5));
                    vec4 moments = (textureLod(uNormalShadowMapArrayTexture, vec3(shadowNDC.xy, float(int(light.metaData.y))), 0.0) + vec2(-0.035955884801, 0.0).xyyy) * mat4(0.2227744146, 0.0771972861, 0.7926986636, 0.0319417555, 0.1549679261, 0.1394629426, 0.7963415838, -0.172282317, 0.1451988946, 0.2120202157, 0.7258694464, -0.2758014811, 0.163127443, 0.2591432266, 0.6539092497, -0.3376131734);
                    lightAttenuation *= reduceLightBleeding(getMSMShadowIntensity(moments, shadowNDC.z, 5e-3, 1e-2), 0.0);
                  }
                  break;
                }
                case 2u: {  // Point
                  float znear = 1e-2, zfar = max(1.0, light.directionZFar.w);
                  vec3 vector = light.positionRange.xyz - inWorldSpacePosition;
                  vec4 moments = (textureLod(uCubeMapShadowMapArrayTexture, vec4(vec3(normalize(vector)), float(int(light.metaData.y))), 0.0) + vec2(-0.035955884801, 0.0).xyyy) * mat4(0.2227744146, 0.0771972861, 0.7926986636, 0.0319417555, 0.1549679261, 0.1394629426, 0.7963415838, -0.172282317, 0.1451988946, 0.2120202157, 0.7258694464, -0.2758014811, 0.163127443, 0.2591432266, 0.6539092497, -0.3376131734);
                  lightAttenuation *= reduceLightBleeding(getMSMShadowIntensity(moments, clamp((length(vector) - znear) / (zfar - znear), 0.0, 1.0), 5e-3, 1e-2), 0.0);
                  break;
                }
#endif
                case 4u: {  // Primary directional
                  imageLightBasedLightDirection = light.directionZFar.xyz;
                  litIntensity = lightAttenuation;
                  float viewSpaceDepth = -inViewSpacePosition.z;
                  for (int cascadedShadowMapIndex = 0; cascadedShadowMapIndex < NUM_SHADOW_CASCADES; cascadedShadowMapIndex++) {
                    vec2 shadowMapSplitDepth = uCascadedShadowMaps.shadowMapSplitDepthsScales[cascadedShadowMapIndex].xy;
                    if ((viewSpaceDepth >= shadowMapSplitDepth.x) && (viewSpaceDepth <= shadowMapSplitDepth.y)) {
                      float shadow = doCascadedShadowMapShadow(cascadedShadowMapIndex, -light.directionZFar.xyz);
                      int nextCascadedShadowMapIndex = cascadedShadowMapIndex + 1;
                      if (nextCascadedShadowMapIndex < NUM_SHADOW_CASCADES) {
                        vec2 nextShadowMapSplitDepth = uCascadedShadowMaps.shadowMapSplitDepthsScales[nextCascadedShadowMapIndex].xy;
                        if ((viewSpaceDepth >= nextShadowMapSplitDepth.x) && (viewSpaceDepth <= nextShadowMapSplitDepth.y)) {
                          float splitFade = smoothstep(nextShadowMapSplitDepth.x, shadowMapSplitDepth.y, viewSpaceDepth);
                          if (splitFade > 0.0) {
                            shadow = mix(shadow, doCascadedShadowMapShadow(nextCascadedShadowMapIndex, -light.directionZFar.xyz), splitFade);
                          }
                        }
                      }
                      lightAttenuation *= shadow;
                      break;
                    }
                  }
                  break;
                }
              }
#if 0              
              if (lightIndex == 0) {
                litIntensity = lightAttenuation;
              }
#endif
            }

            float lightAttenuationEx = lightAttenuation;
#endif
            switch (light.metaData.x) {
              case 1u: {  // Directional
                lightDirection = -light.directionZFar.xyz;
                break;
              }
              case 2u: {  // Point
                lightDirection = normalizedLightVector;
                break;
              }
              case 3u: {  // Spot
#if 1
                float angularAttenuation = clamp(fma(dot(normalize(light.directionZFar.xyz), -normalizedLightVector), uintBitsToFloat(light.metaData.z), uintBitsToFloat(light.metaData.w)), 0.0, 1.0);
#else
                // Just for as reference
                float innerConeCosinus = uintBitsToFloat(light.metaData.z);
                float outerConeCosinus = uintBitsToFloat(light.metaData.w);
                float actualCosinus = dot(normalize(light.directionZFar.xyz), -normalizedLightVector);
                float angularAttenuation = mix(0.0, mix(smoothstep(outerConeCosinus, innerConeCosinus, actualCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
#endif
                lightAttenuation *= angularAttenuation * angularAttenuation;
                lightDirection = normalizedLightVector;
                break;
              }
              case 4u: {  // Primary directional
                imageLightBasedLightDirection = lightDirection = -light.directionZFar.xyz;
                break;
              }
              default: {
                continue;
              }
            }
            switch (light.metaData.x) {
              case 2u:    // Point
              case 3u: {  // Spot
                if (light.positionRange.w >= 0.0) {
                  float currentDistance = length(lightVector);
                  if (currentDistance > 0.0) {
                    lightAttenuation *= 1.0 / (currentDistance * currentDistance);
                    if (light.positionRange.w > 0.0) {
                      float distanceByRange = currentDistance / light.positionRange.w;
                      lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange * distanceByRange * distanceByRange), 0.0, 1.0);
                    }
                  }
                }
                break;
              }
            }
            if (lightAttenuation > 0.0) {
              doSingleLight(light.colorIntensity.xyz * light.colorIntensity.w,  //
                            vec3(lightAttenuation),                             //
                            lightDirection,                                     //
                            normal.xyz,                                         //
                            diffuseColorAlpha.xyz,                              //
                            F0,                                                 //
                            F90,                                                //
                            viewDirection,                                      //
                            refractiveAngle,                                    //
                            transparency,                                       //
                            alphaRoughness,                                     //
                            cavity,                                             //
                            sheenColorIntensityFactor,                          //
                            sheenRoughness,                                     //
                            clearcoatNormal,                                    //
                            clearcoatF0,                                        //
                            clearcoatRoughness,                                 //
                            specularWeight);                                    //
#ifdef TRANSMISSION
              if ((flags & (1u << 11u)) != 0u) {
                // If the light ray travels through the geometry, use the point it exits the geometry again.
                // That will change the angle to the light source, if the material refracts the light ray.
                vec3 transmissionRay = getVolumeTransmissionRay(normal.xyz, viewDirection, volumeThickness, ior);
                vec3 pointToLight = ((light.metaData.x == 0) ? lightDirection : lightVector) - transmissionRay;
                vec3 normalizedLightVector = normalize(pointToLight);
                float lightAttenuation = lightAttenuationEx;
                switch (light.metaData.x) {
                  case 3u: {  // Spot
    #if 1
                    float angularAttenuation = clamp(fma(dot(normalize(light.directionZFar.xyz), -normalizedLightVector), uintBitsToFloat(light.metaData.z), uintBitsToFloat(light.metaData.w)), 0.0, 1.0);
    #else
                    // Just for as reference
                    float innerConeCosinus = uintBitsToFloat(light.metaData.z);
                    float outerConeCosinus = uintBitsToFloat(light.metaData.w);
                    float actualCosinus = dot(normalize(light.directionZFar.xyz), -normalizedLightVector);
                    float angularAttenuation = mix(0.0, mix(smoothstep(outerConeCosinus, innerConeCosinus, actualCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
    #endif
                    lightAttenuation *= angularAttenuation * angularAttenuation;
                    lightDirection = normalizedLightVector;
                    break;
                  }
                }
                switch (light.metaData.x) {
                  case 2u:    // Point
                  case 3u: {  // Spot
                    if (light.positionRange.w >= 0.0) {
                      float currentDistance = length(pointToLight);
                      if (currentDistance > 0.0) {
                        lightAttenuation *= 1.0 / (currentDistance * currentDistance);
                        if (light.positionRange.w > 0.0) {
                          float distanceByRange = currentDistance / light.positionRange.w;
                          lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange * distanceByRange * distanceByRange), 0.0, 1.0);
                        }
                      }
                    }
                    break;
                  }
                }
                vec3 transmittedLight = lightAttenuation * getPunctualRadianceTransmission(normal.xyz, viewDirection, normalizedLightVector, alphaRoughness, F0, F90, diffuseColorAlpha.xyz, ior);
                if ((flags & (1u << 12u)) != 0u) {
                  transmittedLight = applyVolumeAttenuation(transmittedLight, length(transmissionRay), volumeAttenuationColor, volumeAttenuationDistance);
                }
                transmissionOutput += transmittedLight;
              }
#endif
            }
          }
          lightTreeNodeIndex++;
        } else {
          lightTreeNodeIndex += max(1u, lightTreeNode.aabbMinSkipCount.w);
        }
      }
/*    if (lightTreeNodeIndex == 0u) {
        doSingleLight(vec3(1.7, 1.15, 0.70),              //
                      vec3(1.0),                          //
                      normalize(-vec3(0.5, -1.0, -1.0)),  //
                      normal.xyz,                         //
                      diffuseColorAlpha.xyz,              //
                      F0,                                 //
                      F90,                                //
                      viewDirection,                      //
                      refractiveAngle,                    //
                      transparency,                       //
                      alphaRoughness,                     //
                      cavity,                             //
                      sheenColorIntensityFactor,          //
                      sheenRoughness,                     //
                      clearcoatNormal,                    //
                      clearcoatF0,                        //
                      clearcoatRoughness,                 //
                      specularWeight);                    //
      }*/
#elif 1
      doSingleLight(vec3(1.7, 1.15, 0.70),              //
                    vec3(1.0),                          //
                    normalize(-vec3(0.5, -1.0, -1.0)),  //
                    normal.xyz,                         //
                    diffuseColorAlpha.xyz,              //
                    F0,                                 //
                    F90,                                //
                    viewDirection,                      //
                    refractiveAngle,                    //
                    transparency,                       //
                    alphaRoughness,                     //
                    cavity,                             //
                    sheenColorIntensityFactor,          //
                    sheenRoughness,                     //
                    clearcoatNormal,                    //
                    clearcoatF0,                        //
                    clearcoatRoughness,                 //
                    specularWeight);                    //
#endif
      diffuseOutput += getIBLRadianceLambertian(normal, viewDirection, perceptualRoughness, diffuseColorAlpha.xyz, F0, specularWeight);
      specularOutput += getIBLRadianceGGX(normal, perceptualRoughness, F0, specularWeight, viewDirection, litIntensity, imageLightBasedLightDirection);
      if ((flags & (1u << 7u)) != 0u) {
        sheenOutput += getIBLRadianceCharlie(normal, viewDirection, sheenRoughness, sheenColorIntensityFactor.xyz);
      }
      if ((flags & (1u << 8u)) != 0u) {
        clearcoatOutput += getIBLRadianceGGX(clearcoatNormal, clearcoatRoughness, clearcoatF0.xyz, 1.0, viewDirection, litIntensity, imageLightBasedLightDirection);
        clearcoatFresnel = F_Schlick(clearcoatF0, clearcoatF90, clamp(dot(clearcoatNormal, viewDirection), 0.0, 1.0));
      }
#if defined(TRANSMISSION)
      if ((flags & (1u << 11u)) != 0u) {
        transmissionOutput += getIBLVolumeRefraction(normal.xyz, viewDirection,
                                                     perceptualRoughness,
                                                     diffuseColorAlpha.xyz, F0, F90,
                                                     inWorldSpacePosition,
                                                     ior, 
                                                     volumeThickness, 
                                                     volumeAttenuationColor, 
                                                     volumeAttenuationDistance);        
      }
#endif
      vec3 emissiveOutput = emissiveTexture.xyz * material.emissiveFactor.xyz * material.emissiveFactor.w;
      color = vec2(0.0, diffuseColorAlpha.w).xxxy;
#ifndef EXTRAEMISSIONOUTPUT
      color.xyz += emissiveOutput;
#endif
#if defined(TRANSMISSION)
      color.xyz += mix(diffuseOutput, transmissionOutput, transmissionFactor);
#else
      color.xyz += diffuseOutput;
#endif
      color.xyz += specularOutput;
      color.xyz = fma(color.xyz, vec3(albedoSheenScaling), sheenOutput);
      color.xyz = fma(color.xyz, vec3(1.0 - (clearcoatFactor * clearcoatFresnel)), clearcoatOutput);
#ifdef EXTRAEMISSIONOUTPUT
      emissionColor.xyz = emissiveOutput * (1.0 - (clearcoatFactor * clearcoatFresnel));
#endif
      break;
    }
    case smUnlit: {
      color = textureFetch(0, vec4(1.0), true) * material.baseColorFactor * vec2((litIntensity * 0.25) + 0.75, 1.0).xxxy;
      break;
    }
  }
  float alpha = color.w * inColor0.w, outputAlpha = mix(1.0, color.w * inColor0.w, float(int(uint((flags >> 5u) & 1u))));
  vec4 finalColor = vec4(color.xyz * inColor0.xyz, outputAlpha);
#if !(defined(WBOIT) || defined(MBOIT))
#ifndef BLEND 
  outFragColor = finalColor;
#endif
#ifdef EXTRAEMISSIONOUTPUT
  outFragEmission = vec4(emissionColor.xyz * inColor0.xyz, outputAlpha);
#endif
#endif
#endif

#ifdef ALPHATEST
  #if defined(NODISCARD)  
    float fragDepth;
  #endif
  if (alpha < uintBitsToFloat(material.alphaCutOffFlagsTex0Tex1.x)) {
  #if defined(WBOIT) || defined(LOCKOIT) || defined(LOCKOIT_PASS2)
    finalColor = vec4(alpha = 0.0);    
  #elif defined(LOCKOIT_PASS1)
    alpha = 0.0;    
  #elif defined(MBOIT)
    #if defined(MBOIT) && defined(MBOITPASS1)    
      alpha = 0.0;    
    #else
      finalColor = vec4(alpha = 0.0);    
    #endif
  #else 
    #if defined(NODISCARD)  
      // Workaround for Intel (i)GPUs, which've problems with discarding fragments in 2x2 fragment blocks at alpha-test usage
      #if defined(REVERSEDZ)
        fragDepth = -0.1;
      #else
        fragDepth = 1.1;
      #endif
    #else
      #if defined(USEDEMOTE)
        demote;
      #else
        discard;
      #endif
    #endif
  #endif
  }else{
  #if defined(NODISCARD)  
    fragDepth = gl_FragCoord.z;
  #endif
  #if defined(WBOIT) || defined(MBOIT) || defined(LOCKOIT) || defined(LOOPOIT)
    #if defined(WBOIT) || defined(LOCKOIT) || defined(LOOPOIT_PASS2)
      finalColor.w = alpha = 1.0;    
    #elif defined(LOOPOIT_PASS1)
      alpha = 1.0;    
    #elif defined(MBOIT) && defined(MBOITPASS1)    
      alpha = 1.0;    
    #else
      finalColor.w = alpha = 1.0;    
    #endif
  #endif
  }
  #if defined(NODISCARD)  
    gl_FragDepth = fragDepth;
  #endif
  #if !(defined(WBOIT) || defined(MBOIT) || defined(LOCKOIT) || defined(LOOPOIT))
    #ifdef MSAA
      #if 0
        vec2 alphaTextureSize = vec2(texture2DSize(0));
        vec2 alphaTextureUV = textureUV(0) * alphaTextureSize;
        vec4 alphaDUV = vec4(vec2(dFdx(alphaTextureUV)), vec2(dFdy(alphaTextureUV)));
        alpha *= 1.0 + (max(0.0, max(dot(alphaDUV.xy, alphaDUV.xy), dot(alphaDUV.zw, alphaDUV.zw)) * 0.5) * 0.25);
      #endif
      #if 1
        alpha = clamp(((alpha - uintBitsToFloat(material.alphaCutOffFlagsTex0Tex1.x)) / max(fwidth(alpha), 1e-4)) + 0.5, 0.0, 1.0);
      #endif  
      if (alpha < 1e-2) {
        alpha = 0.0;
      }
      #ifndef DEPTHONLY  
        outFragColor.w = finalColor.w = alpha;
      #endif
    #endif
  #endif
#endif

#if defined(WBOIT)

  float depth = fma((log(clamp(-inViewSpacePosition.z, uWBOIT.wboitZNearZFar.x, uWBOIT.wboitZNearZFar.y)) - uWBOIT.wboitZNearZFar.z) / (uWBOIT.wboitZNearZFar.w - uWBOIT.wboitZNearZFar.z), 2.0, -1.0); 
  float transmittance = clamp(1.0 - alpha, 1e-4, 1.0);
  finalColor.xyz *= finalColor.w;
  float weight = min(1.0, fma(max(max(finalColor.x, finalColor.y), max(finalColor.z, finalColor.w)), 40.0, 0.01)) * clamp(depth, 1e-2, 3e3); //clamp(0.03 / (1e-5 + pow(abs(inViewSpacePosition.z) / 200.0, 4.0)), 1e-2, 3e3);
  outFragWBOITAccumulation = finalColor * weight; 
  outFragWBOITRevealage = vec4(finalColor.w);

#elif defined(MBOIT)

  float depth = MBOIT_WarpDepth(clamp(-inViewSpacePosition.z, uMBOIT.mboitZNearZFar.x, uMBOIT.mboitZNearZFar.y), uMBOIT.mboitZNearZFar.z, uMBOIT.mboitZNearZFar.w);
  float transmittance = clamp(1.0 - alpha, 1e-4, 1.0);
#ifdef MBOITPASS1
  {
    float b0;
    vec4 b1234;
    vec4 b56;
    MBOIT6_GenerateMoments(depth, transmittance, b0, b1234, b56);
    outFragMBOITMoments0 = vec4(b0, b1234.xyz);
    outFragMBOITMoments1 = vec4(b1234.w, b56.xy, 0.0);
  }
#elif defined(MBOITPASS2)
  {
#ifdef MSAA
    vec4 mboitMoments0 = subpassLoad(uMBOITMoments0, gl_SampleID); 
    vec4 mboitMoments1 = subpassLoad(uMBOITMoments1, gl_SampleID); 
#else    
    vec4 mboitMoments0 = subpassLoad(uMBOITMoments0); 
    vec4 mboitMoments1 = subpassLoad(uMBOITMoments1); 
#endif
    float b0 = mboitMoments0.x;
    vec4 b1234 = vec4(mboitMoments0.yzw, mboitMoments1.x);
    vec4 b56 = vec3(mboitMoments1.yz, 0.0).xyzz;
    float transmittance_at_depth = 1.0;
    float total_transmittance = 1.0;
    MBOIT6_ResolveMoments(transmittance_at_depth,  //
                          total_transmittance,     //
                          depth,                   //
                          5e-5,                    // moment_bias
                          0.04,                    // overestimation
                          b0,                      //
                          b1234,                   //
                          b56);
    if(isinf(transmittance_at_depth) || isnan(transmittance_at_depth)){
      transmittance_at_depth = 1.0;
    }
    outFragColor = vec4(finalColor.xyz, 1.0) * (finalColor.w * transmittance_at_depth);
  } 
#endif

#elif defined(LOCKOIT)

  int oitMultiViewIndex = int(gl_ViewIndex);
  ivec3 oitCoord = ivec3(ivec2(gl_FragCoord.xy), oitMultiViewIndex);
  uint oitStoreMask = uint(gl_SampleMaskIn[0]);

#ifdef INTERLOCK
  beginInvocationInterlock();
#endif

  // Workaround for missing VK_EXT_post_depth_coverage support on AMD GPUs older than RDNA,
  // namely, an extra OIT renderpass with an fragment-shader-based depth check on the depth 
  // buffer values from the previous forward rendering pass, which should fix problems with 
  // transparent and opaque objects in MSAA, even without VK_EXT_post_depth_coverage support,
  // at least I hope it so:
  uint oitCurrentDepth = floatBitsToUint(gl_FragCoord.z);
 #ifdef MSAA 
  uint oitDepth = floatBitsToUint(subpassLoad(uOITImgDepth, gl_SampleID).r); 
 #else
  uint oitDepth = floatBitsToUint(subpassLoad(uOITImgDepth).r); 
 #endif 
  if(
#ifdef REVERSEDZ
     (oitCurrentDepth >= oitDepth) &&  
#else
     (oitCurrentDepth <= oitDepth) &&  
#endif
     (min(alpha, finalColor.w) > 0.0)
    ){

#ifndef IGNORELOCKOIT
    const int oitViewSize = int(uOIT.oitViewPort.z);
    const int oitCountLayers = int(uOIT.oitViewPort.w & 0xffffu);
    const int oitMultiViewSize = oitViewSize * oitCountLayers;
    const int oitABufferBaseIndex = ((oitCoord.y * int(uOIT.oitViewPort.x)) + oitCoord.x) + (oitMultiViewSize * oitMultiViewIndex);

    uvec4 oitStoreValue = uvec4(packHalf2x16(finalColor.xy), packHalf2x16(finalColor.zw), oitCurrentDepth, oitStoreMask);

#ifdef SPINLOCK
    bool oitDone = /*gl_HelperInvocation ||*/ (oitStoreMask == 0);
    while(!oitDone){
      uint oitOld = imageAtomicExchange(uOITImgSpinLock, oitCoord, 1u);
      if(oitOld == 0u){
#endif
       const uint oitAuxCounter = imageLoad(uOITImgAux, oitCoord).x;
#if defined(MSAA)
        bool mustInsert = true;
        for(int oitIndex = 0; oitIndex < oitAuxCounter; oitIndex++){
          uvec4 oitFragment = imageLoad(uOITImgABuffer, oitABufferBaseIndex + (oitIndex * oitViewSize));
          if((oitFragment.w != 0u) && (oitFragment.z == oitStoreValue.z)){
            mustInsert = false;
            oitFragment.w |= oitStoreMask;
            imageStore(uOITImgABuffer, oitABufferBaseIndex + (oitIndex * oitViewSize), oitFragment);
            break;
          }
        }
        if(mustInsert)
#endif
        {
          imageStore(uOITImgAux, oitCoord, uvec4(oitAuxCounter + 1, 0, 0, 0));
          if(oitAuxCounter < oitCountLayers){
            imageStore(uOITImgABuffer, oitABufferBaseIndex + (int(oitAuxCounter) * oitViewSize), oitStoreValue);
            finalColor = vec4(0.0);
          }else{
            int oitFurthest = 0;
  #ifdef REVERSEDZ
            uint oitMaxDepth = 0xffffffffu;
  #else
            uint oitMaxDepth = 0;
  #endif          
            for(int oitIndex = 0; oitIndex < oitCountLayers; oitIndex++){
              uint oitTestDepth = imageLoad(uOITImgABuffer, oitABufferBaseIndex + (oitIndex * oitViewSize)).z;
              if(
  #ifdef REVERSEDZ
                  (oitTestDepth < oitMaxDepth)
  #else
                  (oitTestDepth > oitMaxDepth)
  #endif
                ){
                oitMaxDepth = oitTestDepth;
                oitFurthest = oitIndex;
              }
            }

            if(
  #ifdef REVERSEDZ
              (oitMaxDepth < oitStoreValue.z)
  #else
              (oitMaxDepth > oitStoreValue.z)
  #endif          
              ){
              int oitIndex = oitABufferBaseIndex + (oitFurthest * oitViewSize);
              uvec4 oitOldValue = imageLoad(uOITImgABuffer, oitIndex);
              finalColor = vec4(vec2(unpackHalf2x16(oitOldValue.x)), vec2(unpackHalf2x16(oitOldValue.y)));
              imageStore(uOITImgABuffer, oitIndex, oitStoreValue);
            }
          }
        }
#ifdef SPINLOCK
        imageAtomicExchange(uOITImgSpinLock, oitCoord, 0u);        
        oitDone = true;
      }
    }
#endif
#endif
  } else {
    finalColor = vec4(0.0);
  }

#ifdef INTERLOCK
  endInvocationInterlock();
#endif

  outFragColor = vec4(finalColor.xyz * finalColor.w, finalColor.w);

#elif defined(LOOPOIT)

  int oitMultiViewIndex = int(gl_ViewIndex);
  ivec3 oitCoord = ivec3(ivec2(gl_FragCoord.xy), oitMultiViewIndex);
#ifdef MSAA
  uint oitStoreMask = uint(gl_SampleMaskIn[0]);
#else
  uint oitStoreMask = 0x00000001u;
#endif  

  // Workaround for missing VK_EXT_post_depth_coverage support on AMD GPUs older than RDNA,
  // namely, an extra OIT renderpass with an fragment-shader-based depth check on the depth 
  // buffer values from the previous forward rendering pass, which should fix problems with 
  // transparent and opaque objects in MSAA, even without VK_EXT_post_depth_coverage support,
  // at least I hope it so:
  uint oitCurrentDepth = floatBitsToUint(gl_FragCoord.z);
 #ifdef MSAA 
  uint oitDepth = floatBitsToUint(subpassLoad(uOITImgDepth, gl_SampleID).r); 
 #else
  uint oitDepth = floatBitsToUint(subpassLoad(uOITImgDepth).r); 
 #endif 
 if(
#ifdef REVERSEDZ
     (oitCurrentDepth >= oitDepth) &&  
#else
     (oitCurrentDepth <= oitDepth) &&  
#endif
#if defined(LOOPOIT_PASS1)
     (alpha > 0.0)
#elif defined(LOOPOIT_PASS2)
     (min(alpha, finalColor.w) > 0.0)
#else
     true    
#endif
    ){

    const int oitViewSize = int(uOIT.oitViewPort.z);
    const int oitCountLayers = int(uOIT.oitViewPort.w & 0xffffu);
    const int oitMultiViewSize = oitViewSize * oitCountLayers;
    const int oitBufferBaseIndex = (((oitCoord.y * int(uOIT.oitViewPort.x)) + oitCoord.x) * oitCountLayers) + (oitMultiViewSize * oitMultiViewIndex);

    #if defined(LOOPOIT_PASS1)

      for(int oitLayerIndex = 0; oitLayerIndex < oitCountLayers; oitLayerIndex++){
#ifdef REVERSEDZ
        uint oitDepth = imageAtomicMax(uOITImgZBuffer, oitBufferBaseIndex + oitLayerIndex, oitCurrentDepth);
        if((oitDepth == 0x00000000u) || (oitDepth == oitCurrentDepth)){
          break;
        }
        oitCurrentDepth = min(oitCurrentDepth, oitDepth);
#else
        uint oitDepth = imageAtomicMin(uOITImgZBuffer, oitBufferBaseIndex + oitLayerIndex, oitCurrentDepth);
        if((oitDepth == 0xFFFFFFFFu) || (oitDepth == oitCurrentDepth)){
          break;
        }
        oitCurrentDepth = max(oitCurrentDepth, oitDepth);
#endif
      }

#ifndef DEPTHONLY    
      outFragColor = vec4(0.0);
#endif

    #elif defined(LOOPOIT_PASS2)

      if(imageLoad(uOITImgZBuffer, oitBufferBaseIndex + (oitCountLayers - 1)).x
#ifdef REVERSEDZ
         >
#else 
         <
#endif
         oitCurrentDepth){
#ifndef DEPTHONLY    
        outFragColor = vec4(finalColor.xyz * finalColor.w, finalColor.w);
#endif
      }else{
        int oitStart = 0;
        int oitEnd = oitCountLayers - 1;
        while(oitStart < oitEnd){
          int oitMid = (oitStart + oitEnd) >> 1;
          uint oitDepth = imageLoad(uOITImgZBuffer, oitBufferBaseIndex + oitMid).x;
          if(oitDepth
#ifdef REVERSEDZ
             > 
#else
             <
#endif
            oitCurrentDepth){
            oitStart = oitMid + 1; 
          }else{
            oitEnd = oitMid; 
          }
        }    

#ifdef MSAA
        imageAtomicOr(uOITImgSBuffer, oitBufferBaseIndex + oitStart, oitStoreMask);
#endif

        imageStore(uOITImgABuffer,
                   oitBufferBaseIndex + oitStart, 
                   uvec3(packHalf2x16(finalColor.xy), packHalf2x16(finalColor.zw), 0u).xyzz
                  );

     #ifndef DEPTHONLY    
        outFragColor = vec4(0.0);
#endif

      }  

    #endif
  }

#elif defined(BLEND)

  outFragColor = vec4(finalColor.xyz * finalColor.w, finalColor.w);

#endif

#ifdef VELOCITY
  outFragVelocity = (inCurrentClipSpace.xy / inCurrentClipSpace.w) - (inPreviousClipSpace.xy / inPreviousClipSpace.w);

  vec3 normal = normalize(inNormal);
  normal /= (abs(normal.x) + abs(normal.y) + abs(normal.z));
  outFragNormal = normalize(vec3(fma(normal.xx, vec2(0.5, -0.5), vec2(fma(normal.y, 0.5, 0.5))), clamp(normal.z * 3.402823e+38, 0.0, 1.0)));  
#endif

}

/*oid main() {
  outFragColor = vec4(vec3(mix(0.25, 1.0, max(0.0, dot(inNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
//outFragColor = vec4(texture(uTexture, inTexCoord)) * vec4(vec3(mix(0.25, 1.0, max(0.0, dot(inNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
}*/