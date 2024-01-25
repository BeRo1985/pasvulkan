#ifndef PBR_GLSL
#define PBR_GLSL

#include "math.glsl"

float cavity = 1.0;

float specularOcclusion = 1.0;

float ambientOcclusion = 1.0;

vec3 iridescenceFresnel = vec3(0.0);
vec3 iridescenceF0 = vec3(0.0);
float iridescenceFactor = 0.0;
float iridescenceIor = 1.3;
float iridescenceThickness = 400.0;

#ifdef ENABLE_ANISOTROPIC
bool anisotropyActive;
vec3 anisotropyDirection;
vec3 anisotropyT;
vec3 anisotropyB;
float anisotropyStrength;
float alphaRoughnessAnisotropyT;
float alphaRoughnessAnisotropyB;
float anisotropyTdotV;
float anisotropyBdotV;
float anisotropyTdotL;
float anisotropyBdotL;
float anisotropyTdotH;
float anisotropyBdotH;
#endif

vec3 diffuseOutput = vec3(0.0);
vec3 specularOutput = vec3(0.0);
vec3 sheenOutput = vec3(0.0);
vec3 clearcoatOutput = vec3(0.0);
vec3 clearcoatFresnel = vec3(0.0);
#if defined(TRANSMISSION)
vec3 transmissionOutput = vec3(0.0);
#endif

float albedoSheenScaling = 1.0;

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
#ifdef ENABLE_ANISOTROPIC
  float GGX;
  if (anisotropyActive) {
    GGX = (NdotL * length(vec3(alphaRoughnessAnisotropyT * anisotropyTdotV, alphaRoughnessAnisotropyB * anisotropyBdotV, NdotV))) + //
          (NdotV * length(vec3(alphaRoughnessAnisotropyT * anisotropyTdotL, alphaRoughnessAnisotropyB * anisotropyBdotL, NdotL)));
  }else{
    float alphaRoughnessSq = alphaRoughness * alphaRoughness;
    GGX = (NdotL * sqrt(((NdotV * NdotV) * (1.0 - alphaRoughnessSq)) + alphaRoughnessSq)) +  //
          (NdotV * sqrt(((NdotL * NdotL) * (1.0 - alphaRoughnessSq)) + alphaRoughnessSq));
  }
  return (GGX > 0.0) ? clamp(0.5 / GGX, 0.0, 1.0) : 0.0;
#else
  float alphaRoughnessSq = alphaRoughness * alphaRoughness;
  float GGX = (NdotL * sqrt(((NdotV * NdotV) * (1.0 - alphaRoughnessSq)) + alphaRoughnessSq)) +  //
              (NdotV * sqrt(((NdotL * NdotL) * (1.0 - alphaRoughnessSq)) + alphaRoughnessSq));
  return (GGX > 0.0) ? (0.5 / GGX) : 0.0;
#endif  
}

float D_GGX(float NdotH, float alphaRoughness) {
#ifdef ENABLE_ANISOTROPIC
  if (anisotropyActive) {
    float a2 = alphaRoughnessAnisotropyT * alphaRoughnessAnisotropyB;
    vec3 f = vec3(alphaRoughnessAnisotropyB * anisotropyTdotH, alphaRoughnessAnisotropyT * anisotropyBdotH, a2 * NdotH);
    return (a2 * pow2(a2 / dot(f, f))) / PI;  
  }else{
    float alphaRoughnessSq = alphaRoughness * alphaRoughness;
    float f = ((NdotH * NdotH) * (alphaRoughnessSq - 1.0)) + 1.0;
    return alphaRoughnessSq / (PI * (f * f));
  }
#else
  float alphaRoughnessSq = alphaRoughness * alphaRoughness;
  float f = ((NdotH * NdotH) * (alphaRoughnessSq - 1.0)) + 1.0;
  return alphaRoughnessSq / (PI * (f * f));
#endif
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

float getSpecularOcclusion(const in float NdotV, const in float ao, const in float roughness){
  return clamp((pow(NdotV + ao, /*roughness * roughness*/exp2((-16.0 * roughness) - 1.0)) - 1.0) + ao, 0.0, 1.0); 
} 

float albedoSheenScalingLUT(const in float NdotV, const in float sheenRoughnessFactor) {
  return textureLod(uImageBasedLightingBRDFTextures[2], vec2(NdotV, sheenRoughnessFactor), 0.0).x;  //
}

void doSingleLight(const in vec3 lightColor, 
                   const in vec3 lightLit, 
                   const in vec3 lightDirection, 
                   const in vec3 normal, 
                   const in vec3 diffuseColor, 
                   const in vec3 F0, 
                   const in vec3 F90, 
                   const in vec3 viewDirection, 
                   const in float refractiveAngle, 
                   const in float materialTransparency,
                   const in float alphaRoughness, 
                   const in float materialCavity, 
                   const in vec3 sheenColor, 
                   const in float sheenRoughness,
                   const in vec3 clearcoatNormal, 
                   const in vec3 clearcoatF0,
                   const float clearcoatRoughness, 
                   const in float specularWeight){
  float nDotL = clamp(dot(normal, lightDirection), 0.0, 1.0);
  float nDotV = clamp(dot(normal, viewDirection), 0.0, 1.0);
  if((nDotL > 0.0) || (nDotV > 0.0)){
    vec3 halfVector = normalize(viewDirection + lightDirection);
    float nDotH = clamp(dot(normal, halfVector), 0.0, 1.0);
    float vDotH = clamp(dot(viewDirection, halfVector), 0.0, 1.0);
    vec3 lit = vec3((materialCavity * nDotL * lightColor) * lightLit);
#ifdef ENABLE_ANISOTROPIC
    anisotropyTdotL = dot(anisotropyT, lightDirection);
    anisotropyBdotL = dot(anisotropyB, lightDirection);
    anisotropyTdotH = dot(anisotropyT, halfVector);
    anisotropyBdotH = dot(anisotropyB, halfVector);
#endif
    diffuseOutput += BRDF_lambertian(F0, F90, diffuseColor, specularWeight, vDotH) * lit;
    specularOutput += BRDF_specularGGX(F0, F90, alphaRoughness, specularWeight, vDotH, nDotL, nDotV, nDotH) * specularOcclusion * lit;
#if defined(CAN_HAVE_EXTENDED_PBR_MATERIAL)
    if ((flags & (1u << 7u)) != 0u) {
      float sheenColorMax = max(max(sheenColor.x, sheenColor.y), sheenColor.z);
      albedoSheenScaling = min(1.0 - (sheenColorMax * albedoSheenScalingLUT(nDotV, sheenRoughness)), //
                               1.0 - (sheenColorMax * albedoSheenScalingLUT(nDotL, sheenRoughness)));
      sheenOutput += BRDF_specularSheen(sheenColor, sheenRoughness, nDotL, nDotV, nDotH) * lit;
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
#endif
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
  vec2 f_ab = textureLod(uImageBasedLightingBRDFTextures[0], brdfSamplePoint, 0.0).xy;
  vec3 irradiance = textureLod(uImageBasedLightingEnvMaps[2], normal.xyz, 0.0).xyz;
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

vec3 getIBLRadianceGGX(in vec3 normal, const in float roughness, const in vec3 F0, const in float specularWeight, const in vec3 viewDirection, const in float litIntensity, const in vec3 imageLightBasedLightDirection) {
  float NdotV = clamp(dot(normal, viewDirection), 0.0, 1.0);
#ifdef ENABLE_ANISOTROPIC
  if(anisotropyActive){
  //float tangentRoughness = mix(roughness, 1.0, anisotropyStrength * anisotropyStrength);  
    normal = normalize(mix(cross(cross(anisotropyDirection, viewDirection), anisotropyDirection), normal, pow4(1.0 - (anisotropyStrength * (1.0 - roughness)))));
  }
#endif
  vec3 reflectionVector = normalize(reflect(-viewDirection, normal));
  float ao = cavity * ambientOcclusion,                                                                                                   //
      lit = mix(1.0, litIntensity, max(0.0, dot(reflectionVector, -imageLightBasedLightDirection) * (1.0 - (roughness * roughness)))),  //
      specularOcclusion = getSpecularOcclusion(NdotV, ao * lit, roughness);
  vec2 brdf = textureLod(uImageBasedLightingBRDFTextures[0], clamp(vec2(NdotV, roughness), vec2(0.0), vec2(1.0)), 0.0).xy;
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
         textureLod(uImageBasedLightingBRDFTextures[1], clamp(vec2(NdotV, sheenRoughness), vec2(0.0), vec2(1.0)), 0.0).x *
         ao;
}

#endif // PBR_GLSL