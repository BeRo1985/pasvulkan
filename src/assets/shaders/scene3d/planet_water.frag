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

// MSAA_FAST = MSAA input but not MSAA output, so that the water isn't multisampled then.

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
  vec2 jitter;
  float underWater;
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

#ifdef MSAA
#ifndef MSAA_FAST
layout(input_attachment_index = 0, set = 1, binding = 9) uniform subpassInputMS uOITImgDepth; // Ignored/Unused in the MSAA_FAST case 
#endif
#else
layout(input_attachment_index = 0, set = 1, binding = 9) uniform subpassInput uOITImgDepth;
#endif

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

// Per planet descriptor set

layout(set = 2, binding = 0) uniform sampler2D uPlanetTextures[]; // 0 = height map, 1 = normal map, 2 = tangent bitangent map

// Per render pass descriptor set

#if !defined(TESSELLATION)
layout(set = 3, binding = 0) uniform sampler2DArray uTextureWaterAcceleration;
#endif

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

float transmissionFactor = 1.0;
float volumeThickness = 0.005;
float volumeAttenuationDistance = 1.0 / 0.0; // +INF
vec3 volumeAttenuationColor = vec3(1.0); 
float volumeDispersion = 0.0;

float ior = 1.33; // 1.33 = water

const vec3 inModelScale = vec3(1.0); 

int inViewIndex = int(gl_ViewIndex);

#define LIGHTING_GLOBALS
#include "lighting.glsl"
#undef LIGHTING_GLOBALS

float envMapMaxLevelGGX = max(0.0, textureQueryLevels(uImageBasedLightingEnvMaps[0]) - 1.0);
float envMapMaxLevelCharlie = 0.0;//max(0.0, textureQueryLevels(uImageBasedLightingEnvMaps[1]) - 1.0);

#include "roughness.glsl"

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

float fresnelDielectric(vec3 Incoming, vec3 Normal, float eta){
  // compute fresnel reflectance without explicitly computing the refracted direction 
  float c = abs(dot(Incoming, Normal));
  float g = ((eta * eta) - 1.0) + (c * c);
  float result;
  if(g > 0.0){
    g = sqrt(g);
    float A = (g - c) / (g + c);
    float B = ((c * (g + c)) - 1.0) / ((c * (g - c)) + 1.0);
    result = 0.5 * A * A *(1.0 + B * B);
  }else{
    result = 1.0;  /* TIR (no refracted component) */
  }
  return result;
}

#define PROCESSLIGHT processLight 

const vec3 waterBaseColor = vec3(0.5, 0.7, 0.9);

vec3 waterDiffuseColor = vec3(0.0);
vec3 waterSpecularColor = vec3(0.0);

void processLight(const in vec3 lightColor, 
                  const in vec3 lightLit, 
                  const in vec3 lightDirection){

} 

vec4 doShade(float hitTime, bool underWater){

  vec4 albedo = vec4(1.0);  
  vec4 occlusionRoughnessMetallic = underWater ? vec4(1.0, 0.0, 0.9, 0.0) :  vec4(1.0, 0.0, 0.9, 0.0);

  // The blade normal is rotated slightly to the left or right depending on the x texture coordinate for
  // to fake roundness of the blade without real more complex geometry
  vec3 normal = workNormal;
 
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

  vec3 triangleNormal = normal;

#if 0

  ior = underWater ? 0.66 : 1.33;
  float eta = max(ior, 1e-5);
  
  float fresnel = clamp(fresnelDielectric(-viewDirection, normal, eta), 0.0, 1.0);
  //float fresnel = pow(1.0 - max(dot(normal, -viewDirection), 0.0), 3.0) * 1.0;

  vec4 color = vec2(0.0, 1.0).xxxy; 		

  vec3 reflection = vec3(0.001); 		
  
  vec3 refraction = getIBLVolumeRefraction(normal.xyz, 
                                           viewDirection,
                                           perceptualRoughness,
                                           diffuseColorAlpha.xyz, F0, F90,
                                           inWorldSpacePosition,
                                           ior, 
                                           volumeThickness, 
                                           volumeAttenuationColor, 
                                           volumeAttenuationDistance,
                                           volumeDispersion);      

  color.xyz = mix(reflection, refraction, clamp(dot(viewDirection, normal), 0.0, 1.0));
  

  return color;

#else

  //diffuseOutput = vec3(0.0);

  //vec3(0.015625) * edgeFactor() * fma(clamp(dot(normal, vec3(0.0, 1.0, 0.0)), 0.0, 1.0), 1.0, 0.0), 1.0);
  vec4 color = vec4(0.0, 0.0, 0.0, 1.0);
  
  //float fresnel = clamp(fresnelDielectric(-viewDirection, normal, 1.0 / ior), 0.0, 1.0);
  float fresnel = pow(1.0 - max(dot(normal, -viewDirection), 0.0), 3.0) * 1.0;
  
/*if(underWater){
    
    vec3 r = textureLod(uPassTextures[1], vec3(inTexCoord, gl_ViewIndex), 1.0).xyz;
    color = vec4(r, 1.0);

  }else*/{

#define LIGHTING_INITIALIZATION
#include "lighting.glsl"
#undef LIGHTING_INITIALIZATION

#define LIGHTING_IMPLEMENTATION
#include "lighting.glsl"
#undef LIGHTING_IMPLEMENTATION

    diffuseOutput += getIBLRadianceLambertian(normal, viewDirection, perceptualRoughness, diffuseColorAlpha.xyz, F0, specularWeight) * iblWeight;
    vec3 iblSpecular = getIBLRadianceGGX(normal, perceptualRoughness, F0, specularWeight, viewDirection, litIntensity, imageLightBasedLightDirection) * iblWeight;
     
#if defined(TRANSMISSION)

    transmissionOutput += getIBLVolumeRefraction(normal.xyz, 
                                                 viewDirection,
                                                 clamp(hitTime * 0.1, 0.0, 0.25),//perceptualRoughness,
                                                 vec3(1.0), //diffuseColorAlpha.xyz, 
                                                 vec3(0.04), //F0, 
                                                 vec3(1.0), //F90,
                                                 inWorldSpacePosition,
                                                 ior, 
                                                 volumeThickness, 
                                                 volumeAttenuationColor, 
                                                 volumeAttenuationDistance,
                                                 volumeDispersion);        

#endif

    vec4 screenSpaceReflection = underWater 
                                   ? vec4(0.0) 
                                   : vec4(iblSpecular, 1.0); //getScreenSpaceReflection(worldSpacePosition, normal, -viewDirection, 0.0, vec4(iblSpecular, 1.0));

    vec3 reflection = mix(screenSpaceReflection.xyz, screenSpaceReflection.xyz * albedo.xyz, screenSpaceReflection.w) + 
#if defined(TRANSMISSION) 
       mix(diffuseOutput, transmissionOutput, transmissionFactor) +
#else
       diffuseOutput +
#endif
      specularOutput;

#if defined(TRANSMISSION) 
    vec3 refraction = transmissionOutput;
#else
    vec3 refraction = vec3(0.0);
#endif

    color.xyz = mix(refraction, reflection, fresnel) * waterBaseColor;

//  color.xyz = mix(refraction, mix(refraction, reflection + diffuse + specularOutput, fresnel), clamp(hitTime * 0.1, 0.0, 1.0));

  }

  //color.xyz = reflection;

  //color.xyz = waterBaseColor * max(0.0, dot(normal, vec3(0.0, 0.0, 1.0)));

  return color;
#endif

}

void main(){
#if defined(TESSELLATION)

  workNormal = normalize((planetModelMatrix * vec4(mapNormal(inBlock.localPosition), 0.0)).xyz) * ((inBlock.underWater > 0.0) ? -1.0 : 1.0);

  viewDirection = normalize(-inCameraRelativePosition);

  vec4 finalColor = doShade(abs(inBlock.viewSpacePosition.z), inBlock.underWater > 0.0);

  outFragColor = vec4(clamp(finalColor.xyz * finalColor.w, vec3(-65504.0), vec3(65504.0)), finalColor.w);

#elif defined(UNDERWATER)

  vec4 finalColor = vec4(textureLod(uPassTextures[1], vec3(inBlock.texCoord, gl_ViewIndex), 1.0).xyz * waterBaseColor * waterBaseColor, 1.0);

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

      finalColor = doShade(maxTime - hitTime, underWater);

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