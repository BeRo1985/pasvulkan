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

#define LIGHTS 
#define SHADOWS

#include "bufferreference_definitions.glsl"

#if defined(LOCKOIT) || defined(DFAOIT)
  #ifdef INTERLOCK
    #extension GL_ARB_fragment_shader_interlock : enable
    #define beginInvocationInterlock beginInvocationInterlockARB
    #define endInvocationInterlock endInvocationInterlockARB
    #ifdef MSAA
      layout(sample_interlock_ordered) in;
    #else
      layout(pixel_interlock_ordered) in;
    #endif
  #else
    #if defined(ALPHATEST)
    #else
    #endif
  #endif
#elif !defined(ALPHATEST)
#endif

layout(location = 0) in vec2 inTexCoord;

#if defined(VELOCITY)
  layout(location = 1) out vec2 outVelocity;
#elif defined(REFLECTIVESHADOWMAPOUTPUT)
  layout(location = 1) out vec4 outFragNormalUsed; // xyz = normal, w = 1.0 if normal was used, 0.0 otherwise (by clearing the normal buffer to vec4(0.0))
#endif

vec3 viewSpacePosition;
vec3 worldSpacePosition;
vec3 cameraRelativePosition;

#define inViewSpacePosition viewSpacePosition
#define inWorldSpacePosition worldSpacePosition
#define inCameraRelativePosition cameraRelativePosition

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

layout(set = 2, binding = 0) uniform sampler2D uTextures[]; // 0 = height map, 1 = normal map, 2 = tangent bitangent map

#define PLANET_WATER
#include "planet_renderpass.glsl"

#define FRAGMENT_SHADER

#define WATER_FRAGMENT_SHADER

#if defined(LOCKOIT) || defined(DFAOIT) || defined(WBOIT) || defined(MBOIT) || defined(LOOPOIT) || defined(BLEND)
 #define TRANSMISSION
 #define TRANSMISSION_FORCED
 #define VOLUMEATTENUTATION_FORCED
#endif

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

#define NOTEXCOORDS
#define inFrameIndex pushConstants.frameIndex
#include "shadows.glsl"

#undef ENABLE_ANISOTROPIC
#include "pbr.glsl"

#define TRANSPARENCY_DECLARATION
#include "transparency.glsl"
#undef TRANSPARENCY_DECLARATION

#define TRANSPARENCY_GLOBALS
#include "transparency.glsl"
#undef TRANSPARENCY_GLOBALS

#if 1
// This solveQuadraticRoots function offers a significant improvement over the old solveQuadraticRoots
// function in terms of numerical stability and accuracy. In computing, especially for floating-point
// numbers, the representation and precision of real numbers are limited, which can lead to issues
// like loss of significance and catastrophic cancellation. This problem is particularly acute when
// dealing with values that are very close in magnitude but have opposite signs.
// The old solveQuadraticRoots function uses a direct approach to calculate the roots of the quadratic
// equation, which suffers from these numerical stability issues. Specifically, when 'b' and the
// square root of the discriminant ('d') in the quadratic formula have values close to each other
// but opposite in sign, it can lead to significant errors due to rounding and cancellation.
// This solveQuadraticRoots function addresses this by using an alternative formulation:
// q = -0.5 * (b + sign(b) * sqrt(b^2 - 4ac))
// t0 = q / a
// t1 = c / q
// This approach ensures that the terms added to compute 'q' always have the same sign, thus avoiding
// the catastrophic cancellation that can occur in the OldSolveQuadraticRoots function. By doing so,
// this solveQuadraticRoots provides more reliable and accurate results, particularly in edge cases 
// where precision is crucial.
bool solveQuadraticRoots(float a, float b, float c, out vec2 t) {
  float discriminant = (b * b) - ((a * c) * 4.0);
  if(discriminant < 0.0){
    t = vec2(0.0);
    return false;
  }else{
    if (discriminant == 0.0) {
      t = vec2(((-0.5 * b) / a));
    } else {
      float q = (b + (sqrt(discriminant) * (b > 0.0 ? 1.0 : -1.0))) * (-0.5);
      t = vec2(q / a, c / q);
      if(t.x > t.y){
        t = t.yx;
      }
    }  
    return true;
  }
}
#else
bool solveQuadraticRoots(float a, float b, float c, out vec2 t) {
  float discriminant = (b * b) - ((a * c) * 4.0);
  if(discriminant < 0.0){
    t = vec2(0.0);
    return false;
  }else{
    float a2 = a * 2.0;
    if(abs(a2) < 1e-7){
      t = vec2(0.0);
      return false;
    }else{
      float inverseDenominator = 1.0 / a2;
      if(abs(discriminant) < 1e-7){
        t = vec2((-b) * inverseDenominator);
      }else{
        t = fma(vec2(sqrt(discriminant)), vec2(-1.0, 1.0), vec2(-b)) * inverseDenominator;
        if(t.x > t.y){
          t = t.yx;
        }
      }
      return true;
    }
  }
}
#endif

bool intersectRaySphere(vec4 sphere, vec3 rayOrigin, vec3 rayDirection, out float time) {
  vec3 sphereCenterToRayOrigin = rayOrigin - sphere.xyz;
  vec2 t;
  bool result = solveQuadraticRoots(dot(rayDirection, rayDirection), 
                                    dot(rayDirection, sphereCenterToRayOrigin) * 2.0, 
                                    dot(sphereCenterToRayOrigin, sphereCenterToRayOrigin) - (sphere.w * sphere.w), 
                                    t);
  if(result){
    if(t.x > t.y){
      t = t.yx;
    }
    if(t.x < 0.0){
      t.x = t.y;
      if(t.x < 0.0){
        result = false;
      }
    }    
    time = t.x;
  }
  return result;
}

const vec3 planetCenter = vec3(0.0); // The planet is at the origin in planet space
float planetBottomRadius = planetData.bottomRadiusTopRadiusHeightMapScale.x;
float planetTopRadius = planetData.bottomRadiusTopRadiusHeightMapScale.y;

mat4 planetModelMatrix = planetData.modelMatrix;
mat4 planetInverseModelMatrix = inverse(planetModelMatrix);

uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
mat4 viewMatrix = uView.views[viewIndex].viewMatrix;
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;
mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
mat4 inverseProjectionMatrix = uView.views[viewIndex].inverseProjectionMatrix;
mat4 viewProjectionMatrix = projectionMatrix * viewMatrix;
mat4 inverseViewProjectionMatrix = inverseViewMatrix * inverseProjectionMatrix;

float linearizeDepth(float z){
#if 0
  vec2 v = (inverseProjectionMatrix * vec4(vec3(fma(inTexCoord, vec2(2.0), vec2(-1.0)), z), 1.0)).zw;
#else
  vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(z), inverseProjectionMatrix[3].zw);
#endif
  return v.x / v.y;
}

float delinearizeDepth(float z){
#if 0
  vec2 v = (projectionMatrix * vec4(vec3(fma(inTexCoord, vec2(2.0), vec2(-1.0)), z), 1.0)).zw;
#else
  vec2 v = fma(projectionMatrix[2].zw, vec2(z), inverseProjectionMatrix[3].zw);
#endif
  return v.x / v.y;
}

float map(vec3 p){
  vec3 n = normalize(planetCenter - p);
  //float w = getwaves(octEqualAreaUnsignedEncode(n) * 64.0, 6) * 0.1; 
  float h = 0.75;//textureBicubicOctahedralMap(uImageHeightMap, n).x + w;
  float r = length(planetCenter - p) - mix(planetBottomRadius, planetTopRadius, h);
  return r;
}

vec3 mapNormal(vec3 p) {
  vec2 e = vec2(1e-2, 0.0); // 0.01 meters for now for the epsilon for the normal calculation
  return normalize(
    vec3(
      map(p + e.xyy) - map(p - e.xyy),
      map(p + e.yxy) - map(p - e.yxy),
      map(p + e.yyx) - map(p - e.yyx)
    )  
  );
}

const int MAX_MARCHING_STEPS = 256;

const float PRECISION = 1e-4;

float INFINITY = uintBitsToFloat(0x7f800000u); 

int countSteps = 0;

// Based on https://www.researchgate.net/publication/329152815_Accelerating_Sphere_Tracing
bool acceleratedRayMarching(vec3 rayOrigin, vec3 rayDirection, float startTime, float maxTime, float w, out float hitTime){ // accelerated ray marching
  float previousR = 0.0; 
  float currentR = 0.0;
  float nextR = INFINITY;
  float stepDistance = 0.0;
  float time = startTime;
  vec2 closest = vec2(INFINITY, 0.0);    
  for(int i = 0; (i < MAX_MARCHING_STEPS) && (nextR >= PRECISION) && (time < maxTime); i++){
    float currentSignedDistance = map(fma(rayDirection, vec3(time + stepDistance), rayOrigin));
    if(closest.x > abs(currentSignedDistance)){
      closest = vec2(abs(currentSignedDistance), time);
    }
    nextR = currentSignedDistance;
    if(stepDistance > (currentR + nextR)){
      stepDistance = currentR;
      currentSignedDistance = map(fma(rayDirection, vec3(time + stepDistance), rayOrigin));
      nextR = currentSignedDistance;
    }
    time += stepDistance;
    previousR = currentR;
    currentR = nextR;
    stepDistance = currentR + ((w * currentR) * (((stepDistance - previousR) + currentR) / ((stepDistance + previousR) - currentR)));
    countSteps++;
  }    
  bool hit = false;
  if((hitTime <= maxTime) && (nextR < PRECISION)){
    hit = true;
    hitTime = min(time, maxTime);    
  }else if(closest.x < 1e-2){
    hit = true;
    hitTime = closest.y;
  }
  return hit;
}

bool standardRayMarching(vec3 rayOrigin, vec3 rayDirection, float startTime, float maxTime, out float hitTime){

  bool hit = false;
  
  float t = startTime;

  float timeStep = max(1.0, maxTime) / (float(MAX_MARCHING_STEPS) * 0.125); 
  
  float closest = INFINITY;
  float closestT = 0.0;

  float secondClosest = INFINITY;
  float secondClosestT = 0.0;
  float previousDT = 0.0;

  for(int i = 0; (i < MAX_MARCHING_STEPS) && (t < maxTime); i++){

    vec3 rayPosition = fma(rayDirection, vec3(t), rayOrigin);

    float dt = map(rayPosition);
    if(dt < closest){
      closest = dt;
      closestT = t;
    }
    
    if((secondClosest > dt) && (previousDT < dt)){
        secondClosest = dt;
        secondClosestT = t;
    }      

    if(dt < PRECISION){
      hit = true;
      hitTime = t;
      break;
    }    

    t += clamp(abs(dt) * 0.5, 1e-6, timeStep) * ((dt < 0.0) ? -1.0 : 1.0);
    
    previousDT = dt;

    countSteps++;
    
  }       
  
  if((!hit) && (closest < 1e-2)){
   // hit = true;
    hitTime = closestT;
  }

  return hit;

} 

vec4 doShade(){

/*const vec3 baseColorSRGB = vec3(52.0, 106.0, 0.0); // vec3(74.0, 149.0, 0.0); 
  const vec3 baseColorLinearRGB = convertSRGBToLinearRGB(baseColorSRGB * 0.00392156862745098);*/
  const vec3 baseColorLinearRGB = vec3(0.5, 0.7, 0.9);

  vec4 albedo = vec4(baseColorLinearRGB, 1.0);  
  vec4 occlusionRoughnessMetallic = vec4(1.0, 1.0, 1.0, 0.0);

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
 
#define LIGHTING_INITIALIZATION
#include "lighting.glsl"
#undef LIGHTING_INITIALIZATION

#define LIGHTING_IMPLEMENTATION
#include "lighting.glsl"
#undef LIGHTING_IMPLEMENTATION

  diffuseOutput += getIBLRadianceLambertian(normal, viewDirection, perceptualRoughness, diffuseColorAlpha.xyz, F0, specularWeight) * iblWeight;
  specularOutput += getIBLRadianceGGX(normal, perceptualRoughness, F0, specularWeight, viewDirection, litIntensity, imageLightBasedLightDirection) * iblWeight;
       
#if defined(TRANSMISSION)

  float fresnel = clamp(1.0 - dot(normal, -viewDirection), 0.0, 1.0);
  fresnel = min(pow(fresnel, 3.0), 0.5);

  transmissionOutput += getIBLVolumeRefraction(normal.xyz, 
                                               viewDirection,
                                               perceptualRoughness,
                                               diffuseColorAlpha.xyz, F0, F90,
                                               inWorldSpacePosition,
                                               ior, 
                                               volumeThickness, 
                                               volumeAttenuationColor, 
                                               volumeAttenuationDistance,
                                               volumeDispersion);        

#endif

  //vec3(0.015625) * edgeFactor() * fma(clamp(dot(normal, vec3(0.0, 1.0, 0.0)), 0.0, 1.0), 1.0, 0.0), 1.0);
  vec4 color = vec4(0.0, 0.0, 0.0, 1.0);

#if defined(TRANSMISSION) 
  color.xyz += mix(diffuseOutput, transmissionOutput, transmissionFactor);
#else
  color.xyz += diffuseOutput;
#endif

  color.xyz += specularOutput;

  return color;

}
void main(){

#ifdef MULTIVIEW
  vec3 texCoord = vec3(inTexCoord, float(gl_ViewIndex));
#else
  vec2 texCoord = inTexCoord;
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

  float hitRayTime;
   
  bool hit = false;

  float hitDepth = 0.0;

  vec4 finalColor = vec4(0.0);

  // Pre-check if the ray intersects the planet's bounding sphere
  if(intersectRaySphere(vec4(planetCenter, planetTopRadius * 1.0), 
                        rayOrigin,
                        rayDirection,     
                        hitRayTime)){

#if defined(LOCKOIT) || defined(DFAOIT) || defined(WBOIT) || defined(MBOIT) || defined(LOOPOIT) //|| defined(BLEND)
  #ifdef MSAA 
    float opaqueDepth = subpassLoad(uOITImgDepth, gl_SampleID).r; 
  #else
    float opaqueDepth = subpassLoad(uOITImgDepth).r; 
  #endif 
#elif defined(BLEND)
    float opaqueDepth = textureLod(uPassTextures[2], vec3(inTexCoord, float(gl_ViewIndex)), 0.0).x;
#else
    float opaqueDepth = 1.0; // To satisfy the IDE GLSL syntax checker, since this shader will be never compiled without OIT
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

    float hitTime;

    //if(acceleratedRayMarching(rayOrigin, rayDirection, 0.0, maxTime, 0.6, hitTime)){
    if(standardRayMarching(rayOrigin, rayDirection, 0.0, maxTime, hitTime)){

      vec3 hitPoint = rayOrigin + (rayDirection * hitTime); // in planet space

      worldSpacePosition = (planetModelMatrix * vec4(hitPoint, 1.0)).xyz;

      viewSpacePosition = (viewMatrix * vec4(worldSpacePosition, 1.0)).xyz;

      cameraRelativePosition = worldSpacePosition - cameraPosition.xyz;

      hit = true;    

      hitDepth = delinearizeDepth(viewSpacePosition.z);

      workNormal = mapNormal(hitPoint);

      //gl_FragDepth = hitDepth;  

      finalColor = doShade();
      
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

  float alpha = finalColor.w;

  bool additiveBlending = false;

#define OVERRIDED_DEPTH hitDepth // instead gl_FragCoord.z 
#if defined(LOCKOIT) || defined(DFAOIT) || defined(WBOIT) || defined(MBOIT) || defined(LOOPOIT) || defined(BLEND)
#define TRANSPARENCY_IMPLEMENTATION
#include "transparency.glsl"
#undef TRANSPARENCY_IMPLEMENTATION
#else
  outFragColor = vec4(clamp(finalColor.xyz * finalColor.w, vec3(-65504.0), vec3(65504.0)), finalColor.w);
#endif

} 