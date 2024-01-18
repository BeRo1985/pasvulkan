#version 450 core

#pragma shader_stage(fragment)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_control_flow_attributes : enable
#ifdef WIREFRAME
  #extension GL_EXT_fragment_shader_barycentric : enable
#endif

layout(location = 0) in InBlock {
  vec3 position;
  vec3 sphereNormal;
  vec3 normal;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif  
} inBlock;

layout(location = 0) out vec4 outFragColor;
#ifdef VELOCITY
layout(location = 1) out vec2 outVelocity;
#endif

// Global descriptor set

#define PLANETS
#include "globaldescriptorset.glsl"
#undef PLANETS

// Pass descriptor set

layout(set = 1, binding = 1) uniform sampler2D uImageBasedLightingBRDFTextures[];  // 0 = GGX, 1 = Charlie, 2 = Sheen E

layout(set = 1, binding = 2) uniform samplerCube uImageBasedLightingEnvMaps[];  // 0 = GGX, 1 = Charlie, 2 = Lambertian

layout(set = 1, binding = 3, std430) readonly buffer ImageBasedSphericalHarmonicsMetaData {
  vec4 dominantLightDirection;
  vec4 dominantLightColor;
  vec4 ambientLightColor;
} imageBasedSphericalHarmonicsMetaData;

// Per planet descriptor set

layout(set = 2, binding = 0) uniform sampler2D uTextures[]; // 0 = height map, 1 = normal map, 2 = tangent bitangent map

#include "planet_renderpass.glsl"

#define FRAGMENT_SHADER

#include "octahedral.glsl"
#include "octahedralmap.glsl"
#include "tangentspacebasis.glsl" 

float envMapMaxLevelGGX = max(0.0, textureQueryLevels(uImageBasedLightingEnvMaps[0]) - 1.0);

#include "roughness.glsl"

vec3 imageLightBasedLightDirection = imageBasedSphericalHarmonicsMetaData.dominantLightDirection.xyz;

vec3 sphereNormal = normalize(inBlock.sphereNormal.xyz); // re-normalize, because of vertex interpolation

vec3 viewDirection = normalize(-inBlock.cameraRelativePosition);

mat3 tangentSpaceBasis; // tangent, bitangent, normal
vec3 tangentSpaceViewDirection;
vec2 tangentSpaceViewDirectionXYOverZ;

float ambientOcclusion = 1.0;
vec3 iridescenceF0 = vec3(0.04);
vec3 iridescenceFresnel = vec3(0.0);
float iridescenceFactor = 0.0;

#ifdef WIREFRAME 
float edgeFactor(){
  const float sqrt0d5Mul0d5 = 0.3535533905932738; // sqrt(0.5) * 0.5 - Half of the length of the diagonal of a square with a side length of 1.0
  const vec3 edge = gl_BaryCoordEXT, 
             edgeDX = dFdxFine(edge), 
             edgeDY = dFdyFine(edge), 
             edgeDXY = sqrt((edgeDX * edgeDX) + (edgeDY * edgeDY)),
             edgeRemapped = smoothstep(vec3(0.0), edgeDXY * sqrt0d5Mul0d5, fma(edgeDXY, vec3(-sqrt0d5Mul0d5), edge));
  return 1.0 - min(min(edgeRemapped.x, edgeRemapped.y), edgeRemapped.z);
}   
#endif

float getSpecularOcclusion(const in float NdotV, const in float ao, const in float roughness){
  return clamp((pow(NdotV + ao, /*roughness * roughness*/exp2((-16.0 * roughness) - 1.0)) - 1.0) + ao, 0.0, 1.0); 
} 

vec3 getIBLRadianceLambertian(const in vec3 normal, const in vec3 viewDirection, const in float roughness, const in vec3 diffuseColor, const in vec3 F0, const in float specularWeight) {
  float ao = ambientOcclusion;
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
  float ao = ambientOcclusion,                                                                                                   //
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

void parallaxMapping(){  

  // Not the common known parallax mapping, since it doesn't work in tangent space but in world space, due to the fact that 
  // layered multiplanar (bi-/triplanar) mapping is used, which would be very difficult to implement in tangent space.
  // Therefore it is a bit more like raymarching than parallax mapping in the common sense. 

  const float OFFSET_SCALE = 1.0; 
  const float PARALLAX_SCALE = 0.5;
  const float OFFSET_BIAS = 0.0; 
  const int COUNT_FIRST_ITERATIONS = 12;
  const int COUNT_SECOND_ITERATIONS = 4; 

  vec3 rayDirection = normalize(inBlock.cameraRelativePosition);

#if 1 
  vec3 displacementVector = rayDirection - (tangentSpaceBasis[2] * dot(tangentSpaceBasis[2], rayDirection));
  displacementVector /= (abs(dot(displacementVector, rayDirection))) + OFFSET_SCALE;
#else
  vec3 displacementVector = rayDirection; // just the ray direction because bi-/triplanar mapping uses the position in world space for the texture lookup 
#endif

  vec4 offsetVector = vec4(displacementVector * PARALLAX_SCALE, -1.0) / float(COUNT_FIRST_ITERATIONS);
  vec4 offsetBest = vec4(multiplanarP - (offsetVector.xyz * OFFSET_BIAS), 1.0);

  float height = 1.0;

  vec4 lastOffsetBest = offsetBest;

  // First do a linear search to find a good starting point 
  [[unroll]] for(int iterationIndex = 0; iterationIndex < COUNT_FIRST_ITERATIONS; iterationIndex++){
    multiplanarP = offsetBest.xyz;
    if((height = getLayeredMultiplanarHeight()) < offsetBest.w){
      lastOffsetBest = offsetBest;
      offsetBest += offsetVector;
    }else{ 
      break;
    }    
  }

#if 0

  offsetBest -= offsetVector;

  // Now do a binary search to find the best offset 
  [[unroll]] for(int iterationIndex = 0; iterationIndex < COUNT_SECOND_ITERATIONS; iterationIndex++){
    multiplanarP = (lastOffsetBest = (offsetBest += (offsetVector *= 0.5))).xyz;    
    offsetBest -= ((offsetBest.w < (height = getLayeredMultiplanarHeight())) ? 1.0 : 0.0) * offsetVector;
  }

#else

  // Now do a binary search to find the best offset 
  [[unroll]] for(int iterationIndex = 0; iterationIndex < COUNT_SECOND_ITERATIONS; iterationIndex++, offsetVector *= 0.5){
    multiplanarP = (lastOffsetBest = offsetBest).xyz;
    offsetBest += offsetVector * (step(height = getLayeredMultiplanarHeight(), offsetBest.w) - 0.5);    
  }
  
#endif

  // Mix the last and the best offset to get a smooth transition between the two
  offsetBest = mix(lastOffsetBest, offsetBest, clamp((height - lastOffsetBest.w) / max(1e-7, offsetBest.w - lastOffsetBest.w), 0.0, 1.0));

  multiplanarP = offsetBest.xyz;

}

void main(){

  layerMaterialSetup(sphereNormal);
 
#ifdef EXTERNAL_VERTICES
  vec3 normal = inBlock.normal.xyz;
#else
  vec3 normal = normalize((planetData.normalMatrix * vec4(normalize(fma(texturePlanetOctahedralMap(uTextures[1], sphereNormal).xyz, vec3(2.0), vec3(-1.0))), 0.0)).xyz);
#endif
  vec3 tangent = normalize(cross((abs(normal.y) < 0.999999) ? vec3(0.0, 1.0, 0.0) : vec3(0.0, 0.0, 1.0), normal));
  vec3 bitangent = normalize(cross(normal, tangent));

  tangentSpaceBasis = mat3(tangent, bitangent, normal);

  tangentSpaceViewDirection = normalize(tangentSpaceBasis * viewDirection);
  tangentSpaceViewDirectionXYOverZ = tangentSpaceViewDirection.xy / tangentSpaceViewDirection.z;

  multiplanarSetup(inBlock.position, dFdx(inBlock.position), dFdy(inBlock.position), normal);

  if((planetData.flagsResolutions.x & (1u << 2u)) != 0){
    parallaxMapping();
  }

  vec4 albedo = vec4(0.0);
  vec4 normalHeight = vec4(0.0);
  vec4 occlusionRoughnessMetallic = vec4(0.0);

  {
    float weightSum = 0.0;
    [[unroll]] for(int layerIndex = 0; layerIndex < 4; layerIndex++){
      const float weight = layerMaterialWeights[layerIndex];
      if(weight > 0.0){        
        albedo += multiplanarTexture(u2DTextures[(GetMaterialAlbedoTextureIndex(layerMaterials[layerIndex]) << 1) | 1], GetMaterialScale(layerMaterials[layerIndex])) * weight;
        normalHeight += multiplanarTexture(u2DTextures[(GetMaterialNormalHeightTextureIndex(layerMaterials[layerIndex]) << 1) | 0], GetMaterialScale(layerMaterials[layerIndex])) * weight;
        occlusionRoughnessMetallic += multiplanarTexture(u2DTextures[(GetMaterialOcclusionRoughnessMetallicTextureIndex(layerMaterials[layerIndex]) << 1) | 0], GetMaterialScale(layerMaterials[layerIndex])) * weight;
        weightSum += weight;
      }
    }
    float factor = 1.0 / max(1e-7, weightSum);
    albedo *= factor;
    normalHeight *= factor;
    occlusionRoughnessMetallic *= factor;
  }

  vec3 workNormal = normalize(mat3(tangent, bitangent, normal) * normalize(fma(normalHeight.xyz, vec3(2.0), vec3(-1.0))));
 
  ambientOcclusion = clamp(occlusionRoughnessMetallic.x, 0.0, 1.0);

  vec2 metallicRoughness = clamp(occlusionRoughnessMetallic.zy, vec2(0.0, 1e-3), vec2(1.0));

  vec4 diffuseColorAlpha = vec4(max(vec3(0.0), albedo.xyz * (1.0 - metallicRoughness.x)), albedo.w);

  vec3 F0 = mix(vec3(0.04), albedo.xyz, metallicRoughness.x);

  float perceptualRoughness = metallicRoughness.y;

  float kernelRoughness;
  {
    const float SIGMA2 = 0.15915494, KAPPA = 0.18;        
    vec3 dx = dFdx(workNormal), dy = dFdy(workNormal);
    kernelRoughness = min(KAPPA, (2.0 * SIGMA2) * (dot(dx, dx) + dot(dy, dy)));
    perceptualRoughness = sqrt(clamp((perceptualRoughness * perceptualRoughness) + kernelRoughness, 0.0, 1.0));
  }  

  float litIntensity = 1.0;

  vec3 diffuseOutput = vec3(0.0);
  vec3 specularOutput = vec3(0.0);

  const float specularWeight = 1.0;

  const float iblWeight = 1.0;

  diffuseOutput += getIBLRadianceLambertian(normal, viewDirection, perceptualRoughness, diffuseColorAlpha.xyz, F0, specularWeight) * iblWeight;
  specularOutput += getIBLRadianceGGX(normal, perceptualRoughness, F0, specularWeight, viewDirection, litIntensity, imageLightBasedLightDirection) * iblWeight;
       
  //vec3(0.015625) * edgeFactor() * fma(clamp(dot(normal, vec3(0.0, 1.0, 0.0)), 0.0, 1.0), 1.0, 0.0), 1.0);
  vec4 c = vec4(diffuseOutput + specularOutput, 1.0);
  
  if(planetData.selected.w > 1e-6){
    float d = length(sphereNormal - normalize(planetData.selected.xyz)) - planetData.selected.w;
    float t = fwidth(d) * 1.41421356237;
    c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), vec3(1.0, 0.0, 0.0), 0.5), smoothstep(t, -t, d) * 0.5);
  }

#ifdef WIREFRAME
  if((planetData.flagsResolutions.x & (1u << 0u)) != 0){
    c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), vec3(0.0, 1.0, 1.0), 0.5), edgeFactor());
  }
#endif  

  outFragColor = c;

#ifdef VELOCITY
  outVelocity = (inBlock.currentClipSpace.xy / inBlock.currentClipSpace.w) - (inBlock.previousClipSpace.xy / inBlock.previousClipSpace.w);
#endif

}