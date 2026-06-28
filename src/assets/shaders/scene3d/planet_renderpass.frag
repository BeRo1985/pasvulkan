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

#define PLANET_RENDERPASS

#define PLANET_RENDERPASS_FRAGMENT_SHADER

#define LIGHTS
#define SHADOWS

#define LIGHTCLUSTERS
#define FRUSTUMCLUSTERGRID

#include "bufferreference_definitions.glsl"

#if defined(RAYTRACING)

#define MSAA_RAYOFFSET_WORKAROUND

#if defined(WIREFRAME)
layout(location = 0) pervertexEXT in vec3 inWorldSpacePositionPerVertex[];
#else
layout(location = 0) in vec3 inWorldSpacePosition;
#endif

layout(location = 1) flat in vec3 inCameraPosition;

layout(location = 2) in InBlock {
  vec3 position;
  vec3 sphereNormal;
  vec3 normal;
  vec3 triplanarNormal;
  vec3 triplanarPosition;
//vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  flat uint meshletID;
//vec3 cameraRelativePosition;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif
} inBlock;

#define inViewSpacePosition inBlock.viewSpacePosition

#define inTriplanarPosition inBlock.triplanarPosition

//#define inWorldSpacePosition inBlock.worldSpacePosition

#if defined(WIREFRAME)
vec3 inWorldSpacePosition = (inWorldSpacePositionPerVertex[0] * gl_BaryCoordEXT.x) + (inWorldSpacePositionPerVertex[1] * gl_BaryCoordEXT.y) + (inWorldSpacePositionPerVertex[2] * gl_BaryCoordEXT.z);
#endif

vec3 inCameraRelativePosition = inWorldSpacePosition - inCameraPosition;

#else

layout(location = 0) in InBlock {
  vec3 position;
  vec3 sphereNormal;
  vec3 normal;
  vec3 triplanarNormal;
  vec3 triplanarPosition;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  flat uint meshletID;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif
} inBlock;

#define inViewSpacePosition inBlock.viewSpacePosition
#define inTriplanarPosition inBlock.triplanarPosition
#define inWorldSpacePosition inBlock.worldSpacePosition
#define inCameraRelativePosition inBlock.cameraRelativePosition

#endif

#ifdef VOXELIZATION
// Extra per-fragment inputs from planet_renderpass_voxelization.geom (the planet vertex block occupies locations 0..8).
layout(location = 10) flat in uint inCascadeIndex;
layout(location = 11) in vec3 inVoxelPosition;
#endif

layout(location = 0) out vec4 outFragColor;
#if defined(VELOCITY)
  layout(location = 1) out vec2 outVelocity;
#elif defined(REFLECTIVESHADOWMAPOUTPUT)
  layout(location = 1) out vec4 outFragNormalUsed; // xyz = normal, w = 1.0 if normal was used, 0.0 otherwise (by clearing the normal buffer to vec4(0.0))
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

layout(set = 1, binding = 6, std430) readonly buffer ImageBasedSphericalHarmonicsMetaData {
  vec4 dominantLightDirection;
  vec4 dominantLightColor;
  vec4 ambientLightColor;
} imageBasedSphericalHarmonicsMetaData;

#ifdef FRUSTUMCLUSTERGRID
layout (set = 1, binding = 8, std140) readonly uniform FrustumClusterGridGlobals {
  uvec4 tileSizeZNearZFar;
  vec4 viewRect;
  uvec4 countLightsViewIndexSizeOffsetedViewIndex;
  uvec4 clusterSize;
  vec4 scaleBiasMax;
} uFrustumClusterGridGlobals;

layout (set = 1, binding = 9, std430) readonly buffer FrustumClusterGridIndexList {
   uint frustumClusterGridIndexList[];
};

layout (set = 1, binding = 10, std430) readonly buffer FrustumClusterGridData {
  uvec4 frustumClusterGridData[]; // x = start light index, y = count lights, z = start decal index, w = count decals
};
#endif

// Per planet descriptor set

// Aliased textures, because some are array textures and some are not
layout(set = 2, binding = 0) uniform sampler2D uPlanetTextures[]; // 0 = height map, 1 = normal map, 2 = blend map, 3 = grass map, 4 = water map, 5 = brushes, 6 = rain map, 7 = atmosphere map
layout(set = 2, binding = 0) uniform sampler2DArray uPlanetArrayTextures[]; // 0 = height map, 1 = normal map, 2 = blend map, 3 = grass map, 4 = water map, 5 = brushes, 6 = rain map, 7 = atmosphere map

#define globalRaytracingFlags pushConstants.flags

#include "planet_textures.glsl"

#define RainTexture uPlanetTextures[PLANET_TEXTURE_RAINTEXTURE]
#define RainNormalTexture uPlanetTextures[PLANET_TEXTURE_RAINNORMALTEXTURE]
#define RainStreaksNormalTexture uPlanetTextures[PLANET_TEXTURE_RAINSTREAKSNORMALTEXTURE]

#include "planet_wetness.glsl"
#include "planet_renderpass.glsl"

#include "gi_globals.glsl"

#include "gi_debug.glsl"

#include "pbr_wetness.glsl"

#define FRAGMENT_SHADER

const vec3 inModelScale = vec3(1.0);

#include "math.glsl"

#ifdef RAYTRACING
  #include "raytracing.glsl"
#endif

#include "octahedral.glsl"
#include "octahedralmap.glsl"
#include "tangentspacebasis.glsl"

#ifdef VOXELIZATION
// Cascaded voxel cone tracing: the planet surface is injected into the voxel volume at its own dedicated descriptor set 4
// (sets 0..3 are already used: global, mesh-rendering-pass, planet textures, terrain-mesh SSBO in the mesh-shader path).
#include "rgb9e5.glsl"
#define VOXELIZATION_DESCRIPTOR_SET 4
#include "voxelization_globals.glsl"
#endif

#ifndef VOXELIZATION
// Lighting / IBL globals are skipped for voxelization: the pass descriptor set (uPassTextures, shadow maps, env maps) is not
// declared under VOXELIZATION (see mesh_rendering_pass_descriptorset.glsl), and voxelization only stores albedo + normal.
#define LIGHTING_GLOBALS
#include "lighting.glsl"
#undef LIGHTING_GLOBALS

#define UseEnvMap
#define UseEnvMapGGX
#undef UseEnvMapCharlie
#undef UseEnvMapLambertian

#include "roughness.glsl"
#endif

#if defined(GLOBAL_ILLUMINATION_DUGI)
  // DUGI probe field. The planet passes already use sets 0..2 (global, mesh-rendering-pass, planet textures) and set 3 may
  // hold path-specific data (terrain-mesh SSBO in the mesh-shader path, empty placeholder in the vertex path), so every GI
  // volume mode lives at a fixed dedicated set 4 across all planet pipelines, mirroring mesh.frag's dedicated GI set.
  #define DUGI_DESCRIPTOR_SET 4
  #include "global_illumination_dugi_sampling.glsl"
#endif
#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS)
  // Cascaded radiance hints SH volume at the same dedicated set 4 (mirroring the DUGI probe field above and mesh.frag's CRH
  // descriptor). binding 0 = the cascade uniform (declared inside the include), binding 1 = the SH cascade 3D textures.
  #define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET 4
  #define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_BINDING 0
  layout(set = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET, binding = 1) uniform sampler3D uTexGlobalIlluminationCascadedRadianceHintsSHVolumes[];
  #define GLOBAL_ILLUMINATION_VOLUME_MESH_FRAGMENT
  #include "global_illumination_cascaded_radiance_hints.glsl"
#endif
#if defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)
  // Cascaded voxel cone tracing grid at the same dedicated set 4 (mirroring mesh.frag's CVCT descriptor): binding 0 = the
  // voxel grid uniform, binding 1 = the per-cascade occlusion 3D textures, binding 2 = the per-cascade radiance 3D textures.
  layout(set = 4, binding = 0, std140) readonly uniform VoxelGridData {
    #include "voxelgriddata_uniforms.glsl"
  } voxelGridData;
  layout(set = 4, binding = 1) uniform sampler3D uVoxelGridOcclusion[];
  layout(set = 4, binding = 2) uniform sampler3D uVoxelGridRadiance[];
  #include "global_illumination_voxel_cone_tracing.glsl"
#endif

vec3 imageLightBasedLightDirection = vec3(0.0, 0.0, -1.0); // imageBasedSphericalHarmonicsMetaData.dominantLightDirection.xyz;

vec3 sphereNormal = normalize(inBlock.sphereNormal.xyz); // re-normalize, because of vertex interpolation

vec3 viewDirection = normalize(-inCameraRelativePosition);

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

void parallaxMapping(){

  // Not the common known parallax mapping, since it doesn't work in tangent space but in world space, due to the fact that
  // layered multiplanar (bi-/triplanar) mapping is used, which would be very difficult to implement in tangent space.
  // Therefore it is a bit more like raymarching than parallax mapping in the common sense.

  const float OFFSET_SCALE = 1.0;
  const float PARALLAX_SCALE = 0.5;
  const float OFFSET_BIAS = 0.0;
  const int COUNT_FIRST_ITERATIONS = 12;
  const int COUNT_SECOND_ITERATIONS = 4;

  vec3 rayDirection = normalize(inCameraRelativePosition);

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

vec3 workNormal;

#ifndef VOXELIZATION
#define NOTEXCOORDS
#define inFrameIndex pushConstants.frameIndex
#include "shadows.glsl"

#undef ENABLE_ANISOTROPIC
#include "pbr.glsl"
#endif
#include "blendnormals.glsl"
#include "decals.glsl"
#include "meshlet.glsl"

void main(){

  layerMaterialSetup(sphereNormal);

  layerMaterialWeights = mat2x4(
    texturePlanetOctahedralMapArray(uPlanetArrayTextures[PLANET_TEXTURE_BLENDMAP], sphereNormal, 0),
    texturePlanetOctahedralMapArray(uPlanetArrayTextures[PLANET_TEXTURE_BLENDMAP], sphereNormal, 1)
  );

  layerMaterialGrass = clamp(texturePlanetOctahedralMap(uPlanetTextures[PLANET_TEXTURE_GRASSMAP], sphereNormal).x, 0.0, 1.0);

  vec4 wetness = getWetness(sphereNormal);

#ifdef EXTERNAL_VERTICES
  workNormal = inBlock.normal.xyz;
  vec3 triplanarNormal = inBlock.triplanarNormal.xyz;
#else
  workNormal = normalize((planetData.normalMatrix * vec4(normalize(fma(texturePlanetOctahedralMap(uPlanetTextures[PLANET_TEXTURE_NORMALMAP], sphereNormal).xyz, vec3(2.0), vec3(-1.0))), 0.0)).xyz);
  vec3 triplanarNormal = normalize((planetData.triplanarNormalMatrix * vec4(normalize(fma(texturePlanetOctahedralMap(uPlanetTextures[PLANET_TEXTURE_NORMALMAP], sphereNormal).xyz, vec3(2.0), vec3(-1.0))), 0.0)).xyz);
#endif
  vec3 workTangent = normalize(cross((abs(workNormal.y) < 0.999999) ? vec3(0.0, 1.0, 0.0) : vec3(0.0, 0.0, 1.0), workNormal));
  vec3 workBitangent = normalize(cross(workNormal, workTangent));

#ifdef RAYTRACING
  // The geometric normal is needed for raytracing ray offseting
#ifdef WIREFRAME
  vec3 triangleNormal = normalize(
                          cross(
                            inWorldSpacePositionPerVertex[1] - inWorldSpacePositionPerVertex[0],
                            inWorldSpacePositionPerVertex[2] - inWorldSpacePositionPerVertex[0]
                          )
                        );
#else
  vec3 triangleNormal = normalize(cross(dFdyFine(inCameraRelativePosition), dFdxFine(inCameraRelativePosition)));
#endif
#endif

  tangentSpaceBasis = mat3(workTangent, workBitangent, workNormal);

  tangentSpaceViewDirection = normalize(tangentSpaceBasis * viewDirection);
  tangentSpaceViewDirectionXYOverZ = tangentSpaceViewDirection.xy / tangentSpaceViewDirection.z;

  multiplanarSetup(inBlock.triplanarPosition, dFdx(inBlock.triplanarPosition), dFdy(inBlock.triplanarPosition), triplanarNormal);

  if((planetData.flagsResolutions.x & (1u << 2u)) != 0){
    parallaxMapping();
  }

  vec4 albedo = vec4(0.0);
  vec4 normalHeight = vec4(0.0);
  vec4 occlusionRoughnessMetallic = vec4(0.0);

  {

    float weightSum = 0.0;
    float maxWeight = 0.0;
    [[unroll]] for(int layerTopLevelIndex = 0; layerTopLevelIndex < 2; layerTopLevelIndex++){
      const vec4 weights = layerMaterialWeights[layerTopLevelIndex];
      if(any(greaterThan(weights, vec4(0.0)))){
        [[unroll]] for(int layerBottomLevelIndex = 0; layerBottomLevelIndex < 4; layerBottomLevelIndex++){
          const float weight = weights[layerBottomLevelIndex];
          if(weight > 0.0){
            const int layerIndex = (layerTopLevelIndex << 2) | layerBottomLevelIndex;
            const PlanetMaterial layerMaterial = layerMaterials[layerIndex];
            albedo += multiplanarTexture(u2DTextures[(GetPlanetMaterialAlbedoTextureIndex(layerMaterial) << 1) | 1], GetPlanetMaterialScale(layerMaterial)) * weight;
            normalHeight += multiplanarTexture(u2DTextures[(GetPlanetMaterialNormalHeightTextureIndex(layerMaterial) << 1) | 0], GetPlanetMaterialScale(layerMaterial)) * weight;
            occlusionRoughnessMetallic += multiplanarTexture(u2DTextures[(GetPlanetMaterialOcclusionRoughnessMetallicTextureIndex(layerMaterial) << 1) | 0], GetPlanetMaterialScale(layerMaterial)) * weight;
            weightSum += weight;
            maxWeight = max(maxWeight, weight);
          }
        }
      }
    }

    // Process the default ground texture if the weight sum is less than fadeEnd
    {

      // Define the range for the soft transition
      const float fadeStart = 0.0; // Begin of fading
      const float fadeEnd = 1.0; // Full fading

      // Calculate the factor for the default weight
      const float defaultWeightFactor = clamp((fadeEnd - weightSum) / (fadeEnd - fadeStart), 0.0, 1.0);

      // Calculate the weight of the default ground texture
      const float defaultWeight = defaultWeightFactor;

      if(defaultWeight > 0.0){

        const PlanetMaterial defaultMaterial = layerMaterials[15];
        albedo += multiplanarTexture(u2DTextures[(GetPlanetMaterialAlbedoTextureIndex(defaultMaterial) << 1) | 1], GetPlanetMaterialScale(defaultMaterial)) * defaultWeight;
        normalHeight += multiplanarTexture(u2DTextures[(GetPlanetMaterialNormalHeightTextureIndex(defaultMaterial) << 1) | 0], GetPlanetMaterialScale(defaultMaterial)) * defaultWeight;
        occlusionRoughnessMetallic += multiplanarTexture(u2DTextures[(GetPlanetMaterialOcclusionRoughnessMetallicTextureIndex(defaultMaterial) << 1) | 0], GetPlanetMaterialScale(defaultMaterial)) * defaultWeight;
        weightSum += defaultWeight;

      }

    }

    // Process the grass texture if the grass value is greater than 0.0
    float grass = layerMaterialGrass;
    if(grass > 0.0){

      // Normalize the weights before adding the grass texture
      if(weightSum > 0.0){
        float factor = 1.0 / max(1e-7, weightSum);
        albedo *= factor;
        normalHeight *= factor;
        occlusionRoughnessMetallic *= factor;
        weightSum *= factor;
      }

      // Optional attenuation of the current textures based on the grass value
      float f = pow(1.0 - grass, 16.0);
      albedo *= f;
      normalHeight *= f;
      occlusionRoughnessMetallic *= f;
      weightSum *= f;

      // Add the grass texture
      const float weight = grass;
      const PlanetMaterial grassMaterial = layerMaterials[14];
      albedo += multiplanarTexture(u2DTextures[(GetPlanetMaterialAlbedoTextureIndex(grassMaterial) << 1) | 1], GetPlanetMaterialScale(grassMaterial)) * weight;
      normalHeight += multiplanarTexture(u2DTextures[(GetPlanetMaterialNormalHeightTextureIndex(grassMaterial) << 1) | 0], GetPlanetMaterialScale(grassMaterial)) * weight;
      occlusionRoughnessMetallic += multiplanarTexture(u2DTextures[(GetPlanetMaterialOcclusionRoughnessMetallicTextureIndex(grassMaterial) << 1) | 0], GetPlanetMaterialScale(grassMaterial)) * weight;
      weightSum += weight;

    }

    // Normalize the weights
    if(weightSum > 0.0){
      float factor = 1.0 / max(1e-7, weightSum);
      albedo *= factor;
      normalHeight *= factor;
      occlusionRoughnessMetallic *= factor;
    }

  }

  float surfaceHeight = texturePlanetOctahedralMap(uPlanetTextures[PLANET_TEXTURE_HEIGHTMAP], sphereNormal).x;

  albedo.xyz *= mix(planetData.minMaxHeightFactor.y, planetData.minMaxHeightFactor.w, pow(clamp((surfaceHeight - planetData.minMaxHeightFactor.x) / (planetData.minMaxHeightFactor.z - planetData.minMaxHeightFactor.x), 0.0, 1.0), planetData.heightFactorExponent));

#ifdef VOXELIZATION
  // Voxelization injects albedo + macro surface normal (no lighting; the voxel radiance transfer pass lights the voxels
  // later from the cascaded shadow map + light BVH). The planet terrain has no emissive channel, so emission is zero.
  {
    vec4 baseColor = vec4(max(albedo.xyz, vec3(0.0)), 1.0);
    vec4 emissionColor = vec4(0.0);
    vec3 normal = normalize(workNormal);
    bool voxelDoubleSided = false;
#include "voxelization_fragment.glsl"
  }
#else

  //vec3 F0Dielectric = mix(vec3(0.04), albedo.xyz, metallicRoughness.x);
  vec3 F0Dielectric = vec3(0.04);
  vec3 F90 = vec3(1.0);
  vec3 F90Dielectric = vec3(1.0);
  float specularWeight = 1.0;

  // Apply decals BEFORE wetness
  vec3 decalNormal = vec3(0.0, 0.0, 1.0);
  float decalNormalBlend = 0.0;

  applyDecals(
    albedo,
    occlusionRoughnessMetallic.z,
    occlusionRoughnessMetallic.y,
    occlusionRoughnessMetallic.x,
    F0Dielectric,
    F90Dielectric,
    specularWeight,
    decalNormal,
    decalNormalBlend,
    inWorldSpacePosition,
    workNormal,
    inViewSpacePosition,
    vec3(0.04)  // Base IOR F0 for dielectrics
  );

  vec4 wetnessNormal = vec4(0.0);
  const float rainTime = float(uint(pushConstants.timeSeconds & 4095u)) + pushConstants.timeFractionalSecond;
  applyPBRWetness(
    wetness,
    inWorldSpacePosition,
    mat3(workTangent, workBitangent, workNormal),
    albedo.xyz,
    wetnessNormal,
    occlusionRoughnessMetallic.z, // metallic
    occlusionRoughnessMetallic.y, // roughness
    occlusionRoughnessMetallic.x, // occlusion
    RainTexture,
    RainNormalTexture,
    RainStreaksNormalTexture,
    rainTime,
    1.0,
    true // Extended effect, which includes the rain streaks and puddles
  );

  vec3 normal = normalize(fma(normalHeight.xyz, vec3(2.0), vec3(-1.0)));
  if(decalNormalBlend > 0.0){
    normal = blendNormals(normal, decalNormal, decalNormalBlend);
  }
  normal = normalize(mat3(workTangent, workBitangent, workNormal) * blendNormals(normal, wetnessNormal.xyz, wetnessNormal.w));

  float NdotV;
  normal = getViewClampedNormal(normal, viewDirection, NdotV);
  NdotV = clamp(NdotV, 0.0, 1.0);

  vec3 baseColor = albedo.xyz;

  float occlusion = clamp(occlusionRoughnessMetallic.x, 0.0, 1.0);

  vec2 metallicRoughness = clamp(occlusionRoughnessMetallic.zy, vec2(0.0, 1e-3), vec2(1.0));

  float metallic = metallicRoughness.x;

  vec4 diffuseColorAlpha = vec4(max(vec3(0.0), albedo.xyz * (1.0 - metallicRoughness.x)), albedo.w);

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

  const float iblWeight = 1.0;

#define LIGHTING_INITIALIZATION
#include "lighting.glsl"
#undef LIGHTING_INITIALIZATION

  const bool receiveShadows = true;

  // The RSM albedo output variant writes the raw albedo and lets the GI producer re-light it, so all lighting (the direct
  // light loop + the IBL/GI block below) is unnecessary work — skip it entirely.
#if !defined(RSMALBEDO)
#define LIGHTING_IMPLEMENTATION
#include "lighting.glsl"
#undef LIGHTING_IMPLEMENTATION

  // GI / IBL / direct-light debug channel isolation (renderer-instance CycleGlobalIlluminationDebugMode -> Ctrl+Shift+F): the
  // selected channel index sits in the spare bits of the planet flags word (below the soft-shadow sample count). colorOutput
  // currently holds only the analytic direct lighting (the GI volume and the environment IBL are added below), so snapshot it
  // here for the direct-light-only channel. 0 = off (normal shading).
  uint giDebugDisplay = (pushConstants.flags >> GI_DEBUG_DISPLAY_PLANETFLAGS_SHIFT) & GI_DEBUG_DISPLAY_VALUE_MASK;
  vec3 giDebugGIDiffuse = vec3(0.0);
  vec3 giDebugGISpecular = vec3(0.0);
  vec3 giDebugIBLSpecular = vec3(0.0);
  vec3 giDebugIBLDiffuse = vec3(0.0);
  vec3 giDebugDirectLight = colorOutput;

  // ---- GI / IBL indirect lighting: bind canonical inputs, then include the shared shading (gi_surface_shading.glsl) ----
  vec3 giWorldPos = inWorldSpacePosition;
  vec3 giNormal = normal;
  vec3 giViewDir = viewDirection;
  vec3 giBaseColor = baseColor.xyz;
  vec3 giF0Dielectric = F0Dielectric;
  float giMetallic = metallic;
  float giRoughness = perceptualRoughness;
  float giSpecularWeight = specularWeight;
  float giDiffuseOcclusion = diffuseOcclusion;
  float giSpecularOcclusion = specularOcclusion;
  // doSingleLight (dominant-light) inputs for the CRH / DUGI-SH paths. Planet terrain has no sheen / clearcoat material, so
  // those lobe inputs are neutral; the env-IBL lobe blocks stay MESH_FRAGMENT-only and do not compile here.
  vec3 giF90 = F90;
  vec3 giF90Dielectric = F90Dielectric;
  float giRefractiveAngle = refractiveAngle;
  float giTransparency = transparency;
  float giAlphaRoughness = alphaRoughness;
  vec3 giSheenColor = vec3(0.0);
  float giSheenRoughness = 0.0;
  vec3 giClearcoatNormal = giNormal;
  vec3 giClearcoatFresnel = vec3(0.0);
  float giClearcoatFactor = 0.0;
  float giClearcoatRoughness = 1.0;
#define PLANET_FRAGMENT
#include "gi_surface_shading.glsl"
#undef PLANET_FRAGMENT
  if(giDebugDisplay != 0u){
    // GI debug cycle (Ctrl+Shift+F): replace the shaded colour with the single selected indirect / direct lighting channel.
    switch(giDebugDisplay){
      case GI_DEBUG_DISPLAY_GI_DIFFUSE: {
        colorOutput = giDebugGIDiffuse;
        break;
      }
      case GI_DEBUG_DISPLAY_GI_SPECULAR: {
        colorOutput = giDebugGISpecular;
        break;
      }
      case GI_DEBUG_DISPLAY_IBL_SPECULAR: {
        colorOutput = giDebugIBLSpecular;
        break;
      }
      case GI_DEBUG_DISPLAY_IBL_DIFFUSE: {
        colorOutput = giDebugIBLDiffuse;
        break;
      }
      case GI_DEBUG_DISPLAY_DIRECT_LIGHT: {
        colorOutput = giDebugDirectLight;
        break;
      }
      default: {
        break;
      }
    }
  }
#endif // !RSMALBEDO

  //vec3(0.015625) * edgeFactor() * fma(clamp(dot(normal, vec3(0.0, 1.0, 0.0)), 0.0, 1.0), 1.0, 0.0), 1.0);
#if defined(RSMALBEDO)
  // RSM albedo output: write the raw diffuse albedo (already metallic-demoted) instead of the shaded flux; the GI producer
  // applies the lighting itself, so the probe field is not double-lit.
  vec4 c = vec4(diffuseColorAlpha.xyz, 1.0);
#else
  vec4 c = vec4(colorOutput, 1.0);
#endif

  if(planetData.selected.w > 1e-6){

    const vec4 selectedColor = vec4(unpackHalf2x16(planetData.selectedColorBrushIndexBrushRotation.x), unpackHalf2x16(planetData.selectedColorBrushIndexBrushRotation.y));

    const uint brushIndex = planetData.selectedColorBrushIndexBrushRotation.z;

    if(brushIndex == 0u){

      // Circle brush

      const float d = length(sphereNormal - normalize(planetData.selected.xyz)) - planetData.selected.w;

#if 0
      float t = fwidth(d);
      float l = max(1e-6, planetData.selected.w * 0.25);
      if((d < l) && ((t < (l * 2.0)) && !(isnan(t) || isinf(t)))){ // to prevent artifacts at normal discontinuities and edges
        t = clamp(t * 1.41421356237, 1e-3, 1e-2); // minimize the possibility of artifacts at normal discontinuities and edges even more, by limiting the range of t to a reasonable value range
//      c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), selectedColor.xyz, selectedColor.w), smoothstep(t, -t, d) * 0.5);
        c.xyz = mix(c.xyz, selectedColor.xyz, selectedColor.w * smoothstep(t, -t, d));
      }
#else
      float t = planetData.selectedInnerRadius;
//    c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), selectedColor.xyz, selectedColor.w), smoothstep(0.0, -t, d) * 0.5);
      c.xyz = mix(c.xyz, selectedColor.xyz, selectedColor.w * smoothstep(0.0, -t, d));
#endif

    }else if(brushIndex <= 255u){

      // Brush texture

      const float brushRotation = uintBitsToFloat(planetData.selectedColorBrushIndexBrushRotation.w);

      const vec3 n = normalize(planetData.selected.xyz),
                 p = sphereNormal;

      vec3 t = n.yzx - n.zxy,
           b = normalize(cross(n, t = normalize(t - dot(t, n)))),
           o = p - n;

      if(brushRotation != 0.0){
        const vec2 rotationSinCos = sin(vec2(brushRotation) + vec2(0.0, 1.57079632679));
        const vec3 ot = t, ob = b;
        t = (ot * rotationSinCos.y) - (ob * rotationSinCos.x);
        b = (ot * rotationSinCos.x) + (ob * rotationSinCos.y);
      }

      vec2 uv = vec2(dot(o, t), dot(o, b)) / planetData.selected.w;

      float d = smoothstep(1.0, 1.0 - (1.0 / length(vec2(textureSize(uPlanetArrayTextures[PLANET_TEXTURE_SMOOTHEDBRUSHES], 0).xy))), max(abs(uv.x), abs(uv.y)));

      d *= smoothstep(-1e-4, 1e-4, dot(p, n)); // When we are on the back side of the planet, we need to clear the brush, but smoothly.

      if(d > 0.0){

        // Smooth level interpolation between the 16 brush textures
        float smoothLevel = clamp(planetData.selectedInnerRadius * 15.0, 0.0, 15.0); // Scale from [0.0, 1.0] to [0.0, 15.0]
        float smoothLevelFract = fract(smoothLevel);
        uint smoothLevelInt = uint(smoothLevel);
        uint smoothLevelIntNext = min(smoothLevelInt + 1u, 15u);

        vec2 uvCoords = fma(uv, vec2(0.5), vec2(0.5));

        // Sample both levels
        float value0 = textureLod(uPlanetArrayTextures[PLANET_TEXTURE_SMOOTHEDBRUSHES + smoothLevelInt], vec3(uvCoords, float(brushIndex)), 0.0).x;
        float value1 = textureLod(uPlanetArrayTextures[PLANET_TEXTURE_SMOOTHEDBRUSHES + smoothLevelIntNext], vec3(uvCoords, float(brushIndex)), 0.0).x;

        // Interpolate between levels
        d *= mix(value0, value1, smoothLevelFract);

      }

//    c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), selectedColor.xyz, selectedColor.w), d);
      c.xyz = mix(c.xyz, selectedColor.xyz, selectedColor.w * d);

    }

  }

#ifdef WIREFRAME
  if((planetData.flagsResolutions.x & (1u << 0u)) != 0){
    c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), vec3(0.0, 1.0, 1.0), 0.5), edgeFactor());
  }
#endif


#if defined(SHADOWS) && 0
  {
    vec4 d = shadowCascadeVisualizationColor();
    c = mix(c, d, d.w * 0.25);
  }
#endif

  if((inBlock.meshletID & 0x80000000u) != 0u){
    c.xyz = meshletDebugColor(inBlock.meshletID & 0x7fffffffu);
  }

  outFragColor = vec4(clamp(c.xyz, vec3(-65504.0), vec3(65504.0)), c.w);

#if defined(VELOCITY)
  outVelocity = (((inBlock.currentClipSpace.xy / inBlock.currentClipSpace.w) - pushConstants.jitter.xy) - ((inBlock.previousClipSpace.xy / inBlock.previousClipSpace.w) - pushConstants.jitter.zw)) * 0.5;
#elif defined(REFLECTIVESHADOWMAPOUTPUT)
  outFragNormalUsed = vec4(vec3(fma(normalize(workNormal), vec3(0.5), vec3(0.5))), 1.0);
#endif

#endif // !VOXELIZATION (the lighting + output path)

}
