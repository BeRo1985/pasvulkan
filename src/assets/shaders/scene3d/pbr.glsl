#ifndef PBR_GLSL
#define PBR_GLSL

#include "math.glsl"

float cavity = 1.0;

float specularOcclusion = 1.0;

vec3 iridescenceFresnel = vec3(0.0);
vec3 iridescenceF0 = vec3(0.0);
float iridescenceFactor = 0.0;
float iridescenceIor = 1.3;
float iridescenceThickness = 400.0;

#define ENABLE_ANISOTROPIC
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

#endif // PBR_GLSL