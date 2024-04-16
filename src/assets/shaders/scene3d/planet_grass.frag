#version 460 core

#pragma shader_stage(fragment)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_control_flow_attributes : enable
#ifdef WIREFRAME
  #extension GL_EXT_fragment_shader_barycentric : enable
  #define HAVE_PERVERTEX
#endif

#define LIGHTS 
#define SHADOWS

#include "bufferreference_definitions.glsl"

#if defined(RAYTRACING)

#if defined(WIREFRAME) 
layout(location = 0) pervertexEXT in vec3 inWorldSpacePositionPerVertex[];
#else
layout(location = 0) in vec3 inWorldSpacePosition;
#endif

layout(location = 1) in InBlock {
  vec3 position;
  vec3 normal;
  vec2 texCoord;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif  
} inBlock;

#else

layout(location = 0) in InBlock {
  vec3 position;
  vec3 normal;
  vec2 texCoord;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif  
} inBlock;

#endif

layout(location = 0) out vec4 outFragColor;
#if defined(VELOCITY)
  layout(location = 1) out vec2 outVelocity;
#elif defined(REFLECTIVESHADOWMAPOUTPUT)
  layout(location = 1) out vec4 outFragNormalUsed; // xyz = normal, w = 1.0 if normal was used, 0.0 otherwise (by clearing the normal buffer to vec4(0.0))
#endif

#define inViewSpacePosition inBlock.viewSpacePosition
#define inWorldSpacePosition inBlock.worldSpacePosition
#define inCameraRelativePosition inBlock.cameraRelativePosition

// Global descriptor set

#define PLANETS
#ifdef RAYTRACING
  #define USE_MATERIAL_BUFFER_REFERENCE // needed for raytracing
#endif
#include "globaldescriptorset.glsl"
#undef PLANETS

// Pass descriptor set

#include "mesh_rendering_pass_descriptorset.glsl"
  
layout(set = 1, binding = 6, std430) readonly buffer ImageBasedSphericalHarmonicsMetaData {
  vec4 dominantLightDirection;
  vec4 dominantLightColor;
  vec4 ambientLightColor;
} imageBasedSphericalHarmonicsMetaData;

// Per planet descriptor set

layout(set = 2, binding = 0) uniform sampler2D uTextures[]; // 0 = height map, 1 = normal map, 2 = tangent bitangent map

#include "planet_grass.glsl"

#define FRAGMENT_SHADER

#include "math.glsl"

#include "srgb.glsl"

#ifdef RAYTRACING
  #include "raytracing.glsl"
#endif

#include "octahedral.glsl"
#include "octahedralmap.glsl"
#include "tangentspacebasis.glsl" 

#define LIGHTING_GLOBALS
#include "lighting.glsl"
#undef LIGHTING_GLOBALS

float envMapMaxLevelGGX = max(0.0, textureQueryLevels(uImageBasedLightingEnvMaps[0]) - 1.0);
float envMapMaxLevelCharlie = 0.0;//max(0.0, textureQueryLevels(uImageBasedLightingEnvMaps[1]) - 1.0);

#include "roughness.glsl"

vec3 imageLightBasedLightDirection = imageBasedSphericalHarmonicsMetaData.dominantLightDirection.xyz;

vec3 viewDirection = normalize(-inBlock.cameraRelativePosition);

mat3 tangentSpaceBasis; // tangent, bitangent, normal
vec3 tangentSpaceViewDirection;
vec2 tangentSpaceViewDirectionXYOverZ;

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

vec3 workNormal;

#define NOTEXCOORDS
#define inFrameIndex pushConstants.frameIndex
#include "shadows.glsl"

#undef ENABLE_ANISOTROPIC
#include "pbr.glsl"

void main(){

  float sideSign = gl_FrontFacing ? 1.0 : -1.0;

  vec3 normal = inBlock.normal.xyz * sideSign;
  if(dot(normal, inBlock.viewSpacePosition) > 0.0){
    normal = -normal;
    sideSign = -sideSign;
  }
  vec3 tangent = normalize(cross((abs(normal.y) < 0.999999) ? vec3(0.0, 1.0, 0.0) : vec3(0.0, 0.0, 1.0), normal));
  vec3 bitangent = normalize(cross(normal, tangent));

#ifdef RAYTRACING
  // The geometric normal is needed for raytracing ray offseting
#ifdef WIREFRAME
  vec3 triangleNormal = normalize(
                          cross(
                            inWorldSpacePositionPerVertex[1] - inWorldSpacePositionPerVertex[0], 
                            inWorldSpacePositionPerVertex[2] - inWorldSpacePositionPerVertex[0]
                          )
                        ) * sideSign;
#else
  vec3 triangleNormal = normalize(cross(dFdyFine(inBlock.cameraRelativePosition), dFdxFine(inBlock.cameraRelativePosition)));
#endif
#endif

  tangentSpaceBasis = mat3(tangent, bitangent, normal);

  tangentSpaceViewDirection = normalize(tangentSpaceBasis * viewDirection);
  tangentSpaceViewDirectionXYOverZ = tangentSpaceViewDirection.xy / tangentSpaceViewDirection.z;

  const vec3 baseColorSRGB = vec3(52.0, 106.0, 0.0); // vec3(74.0, 149.0, 0.0); 
  const vec3 baseColorLinearRGB = convertSRGBToLinearRGB(baseColorSRGB * 0.00392156862745098);

  const float fakeSelfShadowing = clamp(inBlock.texCoord.y, 0.1, 1.0); 

  vec4 albedo = vec4(baseColorLinearRGB, 1.0);  
  vec4 normalHeight = vec4(0.5, 0.5, 0.5, 0.5);
  vec4 occlusionRoughnessMetallic = vec4(fakeSelfShadowing, 0.0, 0.0, 0.0);
  
  workNormal = normalize(mat3(tangent, bitangent, normal) * normalize(fma(normalHeight.xyz, vec3(2.0), vec3(-1.0))));
 
  cavity = clamp(occlusionRoughnessMetallic.x, 0.0, 1.0);
    
  vec2 metallicRoughness = clamp(occlusionRoughnessMetallic.zy, vec2(0.0, 1e-3), vec2(1.0));

  vec4 diffuseColorAlpha = vec4(max(vec3(0.0), albedo.xyz * (1.0 - metallicRoughness.x)), albedo.w);

  vec3 F0 = mix(vec3(0.04), albedo.xyz, metallicRoughness.x);

  vec3 F90 = vec3(1.0);

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

  specularOcclusion = getSpecularOcclusion(clamp(dot(normal, viewDirection), 0.0, 1.0), cavity, alphaRoughness);

  const vec3 sheenColor = vec3(0.0);
  const float sheenRoughness = 0.0;

  const vec3 clearcoatF0 = vec3(0.04);
  const vec3 clearcoatF90 = vec3(0.0);
  vec3 clearcoatNormal = normal;
  const float clearcoatFactor = 1.0;
  const float clearcoatRoughness = 1.0;

  float litIntensity = 1.0;

  const float specularWeight = 1.0;

  const float iblWeight = 1.0;

#define LIGHTING_INITIALIZATION
#include "lighting.glsl"
#undef LIGHTING_INITIALIZATION

#define LIGHTING_IMPLEMENTATION
#include "lighting.glsl"
#undef LIGHTING_IMPLEMENTATION

  diffuseOutput += getIBLRadianceLambertian(normal, viewDirection, perceptualRoughness, diffuseColorAlpha.xyz, F0, specularWeight) * iblWeight;
  specularOutput += getIBLRadianceGGX(normal, perceptualRoughness, F0, specularWeight, viewDirection, litIntensity, imageLightBasedLightDirection) * iblWeight;
       
  //vec3(0.015625) * edgeFactor() * fma(clamp(dot(normal, vec3(0.0, 1.0, 0.0)), 0.0, 1.0), 1.0, 0.0), 1.0);
  vec4 c = vec4(diffuseOutput + specularOutput, 1.0);
  
#ifdef WIREFRAME
/*if((planetData.flagsResolutions.x & (1u << 0u)) != 0){
    c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), vec3(0.0, 1.0, 1.0), 0.5), edgeFactor());
  }*/
#endif  


#if defined(SHADOWS) && 0
  {
    vec4 d = shadowCascadeVisualizationColor();
    c = mix(c, d, d.w * 0.25);
  } 
#endif
   
  outFragColor = vec4(clamp(c.xyz, vec3(-65504.0), vec3(65504.0)), c.w);

#if defined(VELOCITY)
  outVelocity = (inBlock.currentClipSpace.xy / inBlock.currentClipSpace.w) - (inBlock.previousClipSpace.xy / inBlock.previousClipSpace.w);
#elif defined(REFLECTIVESHADOWMAPOUTPUT)
  outFragNormalUsed = vec4(vec3(fma(normalize(workNormal), vec3(0.5), vec3(0.5))), 1.0);  
#endif

}