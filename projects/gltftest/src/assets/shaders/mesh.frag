#version 450 core

#define NUM_SHADOW_CASCADES 4

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

#ifndef ALPHATEST
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

#ifdef DEPTHONLY
#if defined(MBOIT) && defined(MBOITPASS1)
layout(location = 0) out vec4 outFragMBOITMoments0;
layout(location = 1) out vec4 outFragMBOITMoments1;
#endif
#else
#if defined(MBOIT)
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

/* clang-format off */
layout(std140, set = 1, binding = 0) uniform uboMaterial {
  vec4 baseColorFactor;
  vec4 specularFactor;
  vec4 emissiveFactor;
  vec4 metallicRoughnessNormalScaleOcclusionStrengthFactor;
  vec4 sheenColorFactorSheenIntensityFactor;
  vec4 clearcoatFactorClearcoatRoughnessFactor;
  vec4 ior;
  uvec4 alphaCutOffFlagsTex0Tex1;
  mat4 textureTransforms[16];
} uMaterial;

layout(set = 1, binding = 1) uniform sampler2D uTextures[];

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
};

layout(std140, set = 2, binding = 0) uniform uboViews {
  View views[512];
} uView;

#ifdef LIGHTS
struct Light {
  uvec4 metaData;
  vec4 colorIntensity;
  vec4 positionRange;
  vec4 directionZFar;
  mat4 shadowMapMatrix;
};

layout(std430, set = 2, binding = 1) buffer LightItemData {
//uvec4 lightMetaData;
  Light lights[];
};

struct LightTreeNode {
  uvec4 aabbMinSkipCount;
  uvec4 aabbMaxUserData;
};

layout(std430, set = 2, binding = 2) buffer LightTreeNodeData {
  LightTreeNode lightTreeNodes[];
};
#endif

#if defined(MBOIT)

layout(std140, set = 3, binding = 4) uniform uboMBOIT {
  vec4 mboitZNearZFar;
} uMBOIT;

#if defined(MBOITPASS1)
#elif defined(MBOITPASS2)
#ifdef MSAA
layout(input_attachment_index = 0, set = 3, binding = 5) uniform subpassInputMS uMBOITMoments0;
layout(input_attachment_index = 1, set = 3, binding = 6) uniform subpassInputMS uMBOITMoments1;
#else
layout(input_attachment_index = 0, set = 3, binding = 5) uniform subpassInput uMBOITMoments0;
layout(input_attachment_index = 1, set = 3, binding = 6) uniform subpassInput uMBOITMoments1;
#endif
#endif

#endif

#ifdef DEPTHONLY
#else
layout(set = 3, binding = 0) uniform sampler2D uImageBasedLightingBRDFTextures[];  // 0 = GGX, 1 = Charlie, 2 = Sheen E

layout(set = 3, binding = 1) uniform samplerCube uImageBasedLightingEnvMaps[];  // 0 = GGX, 1 = Charlie, 2 = Lambertian

#ifdef SHADOWS
layout(std140, set = 3, binding = 2) uniform uboCascadedShadowMaps {
  mat4 shadowMapMatrices[NUM_SHADOW_CASCADES];
  vec4 shadowMapSplitDepths[NUM_SHADOW_CASCADES];
} uCascadedShadowMaps;

layout(set = 3, binding = 3) uniform sampler2DArray uCascadedShadowMapTexture;

#endif

#endif

/* clang-format on */

vec3 convertLinearRGBToSRGB(vec3 c) {
  return mix((pow(c, vec3(1.0 / 2.4)) * vec3(1.055)) - vec3(5.5e-2), c * vec3(12.92), lessThan(c, vec3(3.1308e-3)));  //
}

vec3 convertSRGBToLinearRGB(vec3 c) {
  return mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)), c / vec3(12.92), lessThan(c, vec3(4.045e-2)));  //
}

vec4 convertSRGBToLinearRGB(vec4 c) {
  return vec4(convertSRGBToLinearRGB(c.xyz), c.w);  //
}

#ifdef MBOIT
 #include "mboit.glsl"
#endif

#ifdef DEPTHONLY
#else
#include "roughness.glsl"

float envMapMaxLevelGGX, envMapMaxLevelCharlie;

const float PI = 3.14159265358979323846,     //
    PI2 = 6.283185307179586476925286766559,  //
    OneOverPI = 1.0 / PI;

float cavity, ambientOcclusion;
uint flags, shadingModel;

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
  return (1.0 - (specularWeight * F_Schlick(f0, f90, VdotH))) * (diffuseColor * OneOverPI);  //
}

vec3 BRDF_specularGGX(vec3 f0, vec3 f90, float alphaRoughness, float specularWeight, float VdotH, float NdotL, float NdotV, float NdotH) {
  return specularWeight * F_Schlick(f0, f90, VdotH) * V_GGX(NdotL, NdotV, alphaRoughness) * D_GGX(NdotH, alphaRoughness);  //
}

vec3 BRDF_specularSheen(vec3 sheenColor, float sheenRoughness, float NdotL, float NdotV, float NdotH) {
  return sheenColor * D_Charlie(sheenRoughness, NdotH) * V_Sheen(NdotL, NdotV, sheenRoughness);  //
}

vec3 diffuseOutput = vec3(0.0);
vec3 specularOutput = vec3(0.0);
vec3 sheenOutput = vec3(0.0);
vec3 clearcoatOutput = vec3(0.0);
vec3 clearcoatFresnel = vec3(0.0);

float albedoSheenScaling = 1.0;

float albedoSheenScalingLUT(float NdotV, float sheenRoughnessFactor) {
  return texture(uImageBasedLightingBRDFTextures[2], vec2(NdotV, sheenRoughnessFactor)).r;  //
}

void doSingleLight(const in vec3 lightColor, const in vec3 lightLit, const in vec3 lightDirection, const in vec3 normal, const in vec3 diffuseColor, const in vec3 F0, const in vec3 F90, const in vec3 viewDirection, const in float refractiveAngle, const in float materialTransparency, const in float alphaRoughness, const in float materialCavity, const in vec4 sheenColorIntensityFactor, const in float sheenRoughness, const in vec3 clearcoatNormal, const in vec3 clearcoatF0, const float clearcoatRoughness, const in float specularWeight) {
  vec3 halfVector = normalize(viewDirection + lightDirection);
  float nDotL = clamp(dot(normal, lightDirection), 0.0, 1.0);
  float nDotV = clamp(dot(normal, viewDirection), 0.0, 1.0);
  float nDotH = clamp(dot(normal, halfVector), 0.0, 1.0);
  float vDotH = clamp(dot(viewDirection, halfVector), 0.0, 1.0);
  vec3 lit = vec3((materialCavity * nDotL * lightColor) * lightLit);
  diffuseOutput += BRDF_lambertian(F0, F90, diffuseColor, specularWeight, vDotH) * lit;
  specularOutput += BRDF_specularGGX(F0, F90, alphaRoughness, specularWeight, vDotH, nDotL, nDotV, nDotH) * lit;
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
                       V_GGX(nDotV, nDotL, clearcoatRoughness) * specularWeight * lit;
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
  vec3 Fr = max(vec3(1.0 - roughness), F0) - F0;
  vec3 k_S = F0 + (Fr * pow(1.0 - NdotV, 5.0));
  vec3 FssEss = (specularWeight * k_S * f_ab.x) + f_ab.y;
  float Ems = 1.0 - (f_ab.x + f_ab.y);
  vec3 F_avg = specularWeight * (F0 + ((1.0 - F0) / 21.0));
  vec3 FmsEms = (Ems * FssEss * F_avg) / (vec3(1.0) - (F_avg * Ems));
  vec3 k_D = (diffuseColor * ((1.0 - FssEss) + FmsEms) * ao);
  return (FmsEms + k_D) * irradiance;
}

vec3 getIBLRadianceGGX(const in vec3 normal, const in float roughness, const in vec3 F0, const in float specularWeight, const in vec3 viewDirection, const in float litIntensity, const in vec3 imageLightBasedLightDirection) {
  vec3 reflectionVector = normalize(reflect(-viewDirection, normal));
  float NdotV = clamp(dot(normal, viewDirection), 0.0, 1.0),                                                                            //
      ao = cavity * ambientOcclusion,                                                                                                   //
      lit = mix(1.0, litIntensity, max(0.0, dot(reflectionVector, -imageLightBasedLightDirection) * (1.0 - (roughness * roughness)))),  //
      specularOcclusion = clamp((pow(NdotV + (ao * lit), roughness * roughness) - 1.0) + (ao * lit), 0.0, 1.0);
  vec2 brdf = texture(uImageBasedLightingBRDFTextures[0], clamp(vec2(NdotV, roughness), vec2(0.0), vec2(1.0)), 0.0).xy;
  return (texture(uImageBasedLightingEnvMaps[0],  //
                  reflectionVector,               //
                  roughnessToMipMapLevel(roughness, envMapMaxLevelGGX))
              .xyz *                                                                 //
          fma(F0 + ((max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - NdotV, 5.0)),  //
              brdf.xxx,                                                              //
              brdf.yyy * clamp(max(max(F0.x, F0.y), F0.z) * 50.0, 0.0, 1.0)) *       //
          specularWeight *                                                           //
          specularOcclusion *                                                        //
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

#ifdef SHADOWS

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
  return 1.0 - clamp((s.z + (s.w * ((((s.x * z.z) - (b.x * (s.x + z.z))) + b.y) / ((z.z - s.y) * (z.x - z.y))))) * 1.03, 0.0, 1.0);
}

float doCascadedShadowMapMSMShadow(const in int cascadedShadowMapIndex, const in vec3 lightDirection) {
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

const uint smPBRMetallicRoughness = 0u,  //
    smPBRSpecularGlossiness = 1u,        //
    smUnlit = 2u;                        //

uvec2 texCoordIndices = uMaterial.alphaCutOffFlagsTex0Tex1.zw;
vec2 texCoords[2] = vec2[2](inTexCoord0, inTexCoord1);

vec2 textureUV(const sampler2D tex, const in int textureIndex, const in vec4 defaultValue) {
  uint which = (texCoordIndices[textureIndex >> 3] >> ((uint(textureIndex) & 7u) << 2u)) & 0xfu;
  return (which < 0x2u) ? (uMaterial.textureTransforms[textureIndex] * vec3(texCoords[int(which)], 1.0).xyzz).xy : inTexCoord0;
}

vec4 textureFetch(const sampler2D tex, const in int textureIndex, const in vec4 defaultValue) {
  uint which = (texCoordIndices[textureIndex >> 3] >> ((uint(textureIndex) & 7u) << 2u)) & 0xfu;
  return (which < 0x2u) ? texture(tex, (uMaterial.textureTransforms[textureIndex] * vec3(texCoords[int(which)], 1.0).xyzz).xy) : defaultValue;
}

vec4 textureFetchSRGB(const sampler2D tex, const in int textureIndex, const in vec4 defaultValue) {
  uint which = (texCoordIndices[textureIndex >> 3] >> ((uint(textureIndex) & 7u) << 2u)) & 0xfu;
  vec4 texel;
  if (which < 0x2u) {
    texel = texture(tex, (uMaterial.textureTransforms[textureIndex] * vec3(texCoords[int(which)], 1.0).xyzz).xy);
    texel.xyz = convertSRGBToLinearRGB(texel.xyz);
  } else {
    texel = defaultValue;
  }
  return texel;
}

void main() {
#ifndef DEPTHONLY
  envMapMaxLevelGGX = textureQueryLevels(uImageBasedLightingEnvMaps[0]);
  envMapMaxLevelCharlie = textureQueryLevels(uImageBasedLightingEnvMaps[1]);
  flags = uMaterial.alphaCutOffFlagsTex0Tex1.y;
  shadingModel = (flags >> 0u) & 0xfu;
#endif
#ifdef DEPTHONLY
  float alpha = textureFetch(uTextures[0], 0, vec4(1.0)).w * uMaterial.baseColorFactor.w * inColor0.w;
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
      vec3 specularColorFactor = vec3(1.0);
      float ior = uMaterial.ior.x;
      vec3 F0 = vec3((abs(ior - 1.5) < 1e-6) ? 0.04 : pow((ior - 1.0) / (ior + 1.0), 2.0));
      vec3 F90 = vec3(1.0);
      float specularFactor = 1.0;
      float perceptualRoughness = 1.0;
      float specularWeight = 1.0;
      switch (shadingModel) {
        case smPBRMetallicRoughness: {
          specularFactor = uMaterial.specularFactor.w;
          specularColorFactor = uMaterial.specularFactor.xyz;
          if ((flags & (1u << 9u)) != 0u) {
            specularFactor *= textureFetch(uTextures[9], 9, vec4(1.0)).x;
            specularColorFactor *= textureFetchSRGB(uTextures[10], 10, vec4(1.0)).xyz;
          }
          vec3 dielectricSpecularF0 = clamp(F0 * specularColorFactor * specularFactor, vec3(0.0), vec3(1.0));
          vec4 baseColor = textureFetchSRGB(uTextures[0], 0, vec4(1.0)) * uMaterial.baseColorFactor;
          vec2 metallicRoughness = clamp(textureFetch(uTextures[1], 1, vec4(1.0)).zy * uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.xy, vec2(0.0, 1e-3), vec2(1.0));
          diffuseColorAlpha = vec4(max(vec3(0.0), baseColor.xyz * (1.0 - metallicRoughness.x)), baseColor.w);
          F0 = mix(dielectricSpecularF0, baseColor.xyz, metallicRoughness.x);
          perceptualRoughness = metallicRoughness.y;
          break;
        }
        case smPBRSpecularGlossiness: {
          vec4 specularGlossiness = textureFetchSRGB(uTextures[1], 1, vec4(1.0)) * vec4(uMaterial.specularFactor.xyz, uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.y);
          diffuseColorAlpha = textureFetchSRGB(uTextures[0], 0, vec4(1.0)) * uMaterial.baseColorFactor;
          F0 = specularGlossiness.xyz;
          perceptualRoughness = clamp(1.0 - specularGlossiness.w, 1e-3, 1.0);
          break;
        }
      }

      float geometryRoughness;
      {
        vec3 dxy = max(abs(dFdx(inNormal)), abs(dFdy(inNormal)));
        geometryRoughness = max(max(dxy.x, dxy.y), dxy.z);
      }

      perceptualRoughness = min(max(perceptualRoughness, 0.0525) + geometryRoughness, 1.0);

      float alphaRoughness = perceptualRoughness * perceptualRoughness;

      vec3 normal;
      if ((texCoordIndices.x & 0x00000f00u) != 0x00000f00u) {
        vec4 normalTexture = textureFetch(uTextures[2], 2, vec2(0.0, 1.0).xxyx);
        normal = normalize(                                                                                                                      //
            mat3(normalize(inTangent), normalize(inBitangent), normalize(inNormal)) *                                                            //
            normalize((normalTexture.xyz - vec3(0.5)) * (vec2(uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0))  //
        );
      } else {
        normal = normalize(inNormal);
      }
      normal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;

      vec4 occlusionTexture = textureFetch(uTextures[3], 3, vec4(1.0));

      cavity = clamp(mix(1.0, occlusionTexture.x, uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.w), 0.0, 1.0);

      vec4 emissiveTexture = textureFetchSRGB(uTextures[4], 4, vec4(1.0));

      float transparency = 0.0;
      float refractiveAngle = 0.0;
      float shadow = 1.0;
      ambientOcclusion = 1.0;

      vec3 viewDirection = normalize(-inCameraRelativePosition);

      vec3 imageLightBasedLightDirection = vec3(0.0, 0.0, -1.0);

      vec4 sheenColorIntensityFactor = vec4(1.0);
      float sheenRoughness = 0.0;
      if ((flags & (1u << 7u)) != 0u) {
        sheenColorIntensityFactor = uMaterial.sheenColorFactorSheenIntensityFactor;
        if ((texCoordIndices.x & 0x00f00000u) != 0x00f00000u) {
          sheenColorIntensityFactor *= textureFetchSRGB(uTextures[5], 5, vec4(1.0));
        }
        sheenRoughness = max(perceptualRoughness, 1e-7);
      }

      vec3 clearcoatF0 = vec3(0.04);
      vec3 clearcoatF90 = vec3(0.0);
      vec3 clearcoatNormal = normal;
      float clearcoatFactor = 1.0;
      float clearcoatRoughness = 1.0;
      if ((flags & (1u << 8u)) != 0u) {
        clearcoatFactor = uMaterial.clearcoatFactorClearcoatRoughnessFactor.x;
        clearcoatRoughness = uMaterial.clearcoatFactorClearcoatRoughnessFactor.y;
        if ((texCoordIndices.x & 0x0f000000u) != 0x0f000000u) {
          clearcoatFactor *= textureFetch(uTextures[6], 6, vec4(1.0)).x;
        }
        if ((texCoordIndices.x & 0xf0000000u) != 0xf0000000u) {
          clearcoatRoughness *= textureFetch(uTextures[7], 7, vec4(1.0)).y;
        }
        if ((texCoordIndices.y & 0x0000000fu) != 0x0000000fu) {
          vec4 normalTexture = textureFetch(uTextures[8], 8, vec2(0.0, 1.0).xxyx);
          clearcoatNormal = normalize(mat3(normalize(inTangent), normalize(inBitangent), normalize(inNormal)) * normalize((normalTexture.xyz - vec3(0.5)) * (vec2(uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0)));
        } else {
          clearcoatNormal = normalize(inNormal);
        }
        clearcoatNormal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;
        clearcoatRoughness = min(max(clearcoatRoughness, 0.0525) + geometryRoughness, 1.0);
      }

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
            vec3 lightVector = light.positionRange.xyz - inWorldSpacePosition.xyz;
            vec3 normalizedLightVector = normalize(lightVector);
#ifdef SHADOWS
            if (/*(uShadows != 0) &&*/ ((light.metaData.y & 0x80000000u) == 0u)) {
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
                    vec2 shadowMapSplitDepth = uCascadedShadowMaps.shadowMapSplitDepths[cascadedShadowMapIndex].xy;
                    if ((viewSpaceDepth >= shadowMapSplitDepth.x) && (viewSpaceDepth <= shadowMapSplitDepth.y)) {
                      float shadow = doCascadedShadowMapMSMShadow(cascadedShadowMapIndex, light.directionZFar.xyz);
                      int nextCascadedShadowMapIndex = cascadedShadowMapIndex + 1;
                      if (nextCascadedShadowMapIndex < NUM_SHADOW_CASCADES) {
                        vec2 nextShadowMapSplitDepth = uCascadedShadowMaps.shadowMapSplitDepths[nextCascadedShadowMapIndex].xy;
                        if ((viewSpaceDepth >= nextShadowMapSplitDepth.x) && (viewSpaceDepth <= nextShadowMapSplitDepth.y)) {
                          float splitFade = smoothstep(nextShadowMapSplitDepth.x, shadowMapSplitDepth.y, viewSpaceDepth);
                          if (splitFade > 0.0) {
                            shadow = mix(shadow, doCascadedShadowMapMSMShadow(nextCascadedShadowMapIndex, light.directionZFar.xyz), splitFade);
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
      vec3 emissiveOutput = emissiveTexture.xyz * uMaterial.emissiveFactor.xyz;
      color = vec2(0.0, diffuseColorAlpha.w).xxxy;
#ifndef EXTRAEMISSIONOUTPUT
      color.xyz += emissiveOutput;
#endif
      color.xyz += diffuseOutput;
      color.xyz += specularOutput;
      color.xyz = fma(color.xyz, vec3(albedoSheenScaling), sheenOutput);
      color.xyz = fma(color.xyz, vec3(1.0 - (clearcoatFactor * clearcoatFresnel)), clearcoatOutput);
#ifdef EXTRAEMISSIONOUTPUT
      emissionColor.xyz = emissiveOutput * (1.0 - (clearcoatFactor * clearcoatFresnel));
#endif
      break;
    }
    case smUnlit: {
      color = textureFetchSRGB(uTextures[0], 0, vec4(1.0)) * uMaterial.baseColorFactor * vec2((litIntensity * 0.25) + 0.75, 1.0).xxxy;
      break;
    }
  }
  float alpha = color.w * inColor0.w, outputAlpha = mix(1.0, color.w * inColor0.w, float(int(uint((flags >> 5u) & 1u))));
  vec4 finalColor = vec4(color.xyz * inColor0.xyz, outputAlpha);
#if !defined(MBOIT)
  outFragColor = finalColor;
#ifdef EXTRAEMISSIONOUTPUT
  outFragEmission = vec4(emissionColor.xyz * inColor0.xyz, outputAlpha);
#endif
#endif
#endif

#ifdef ALPHATEST
  if (alpha < uintBitsToFloat(uMaterial.alphaCutOffFlagsTex0Tex1.x)) {
#if defined(MBOIT)
#if defined(MBOIT) && defined(MBOITPASS1)    
    alpha = 0.0;    
#else
    finalColor = vec4(alpha = 0.0);    
#endif
#else 
    discard;
#endif
#if defined(MBOIT)
  }else{
#if defined(MBOIT) && defined(MBOITPASS1)    
    alpha = 1.0;    
#else
    finalColor.w = alpha = 1.0;    
#endif
#endif
  }
#if !defined(MBOIT)
#ifdef MSAA
#if 0
  vec2 alphaTextureSize = textureSize(uTextures[0]).xy;
  vec2 alphaTextureUV = textureUV(0) * alphaTextureSize;
  vec4 alphaDXY = vec4(vec2(dFdx(alphaTextureXY)), vec2(dFdy(alphaTextureXY)));
  alpha *= 1.0 + (max(0.0, max(dot(alphaDXY.xy, alphaDXY.xy), dot(alphaDXY.zw, alphaDXY.zw)) * 0.5) * 0.25);
#endif
  alpha = clamp(((alpha - uintBitsToFloat(uMaterial.alphaCutOffFlagsTex0Tex1.x)) / max(fwidth(alpha), 1e-4)) + 0.5, 0.0, 1.0);
  if (alpha < 1e-2) {
    alpha = 0.0;
  }
#ifndef DEPTHONLY  
  outFragColor.w = finalColor.w = alpha;
#endif
#endif
#endif
#endif

#if defined(MBOIT)
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
#endif

}

/*oid main() {
  outFragColor = vec4(vec3(mix(0.25, 1.0, max(0.0, dot(inNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
//outFragColor = vec4(texture(uTexture, inTexCoord)) * vec4(vec3(mix(0.25, 1.0, max(0.0, dot(inNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
}*/