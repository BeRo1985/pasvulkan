#ifndef GI_RT_GATHER_GLSL
#define GI_RT_GATHER_GLSL

// =====================================================================================================================
//  Shared ray-traced "probe gather" layer for the ray-traced global illumination techniques (DDGI and surfel GI).
//
//  This include encapsulates the common operation that both DDGI probes and surfels need: shoot a ray into the scene,
//  find the closest hit, and compute the outgoing radiance towards the ray origin at that hit point. The radiance
//  accounts for:
//    - emissive meshes               (the hit material's emissive term)
//    - all analytic light sources    (traversal of the light BVH, identical light model as the rasterization path)
//    - ray-traced shadows            (a shadow ray per contributing light against the same TLAS)
//    - the sky / environment on miss (provided by the includer via the GI_GATHER_SKY macro)
//    - previous-frame indirect light (one-bounce feedback term, passed in by the caller for "infinite" bounces)
//
//  Prerequisites (the includer must set these up *before* including this file):
//    #define RAYTRACING
//    #define LIGHTS
//    #define USE_MATERIAL_BUFFER_REFERENCE
//    #define USE_BUFFER_REFERENCE
//    #include "globaldescriptorset.glsl"   // -> TLAS (binding 8), uRaytracingData (9), uMaterials (5), lights[] (1),
//                                          //    lightTreeNodes[] (2), u2DTextures (10)
//    #include "raytracing.glsl"            // -> ray query helpers, raytracingTextureFetch, raytracingOffsetRay, ...
//
//  Optional configuration defines (sensible defaults below):
//    GI_GATHER_TRACE_SHADOWS   1   - trace a shadow ray per light (set 0 to inject lights unshadowed, much cheaper)
//    GI_GATHER_SHADOW_TMAX     1e8 - maximum directional-light shadow ray length in meters
//    GI_GATHER_SKY(dir)        ->  vec3 expression returning the sky/environment radiance for a missed ray direction
//                                  (defaults to black; the trace passes override it to sample the sky)
//    GI_GATHER_DEFAULT_PLANET_ALBEDO  vec3(0.25) - albedo used for planet hits (planet materials are not unpacked here)
// =====================================================================================================================

#ifdef RAYTRACING

#ifndef GI_GATHER_TRACE_SHADOWS
  #define GI_GATHER_TRACE_SHADOWS 1
#endif

#ifndef GI_GATHER_SHADOW_TMAX
  #define GI_GATHER_SHADOW_TMAX 1e8
#endif

#ifndef GI_GATHER_SKY
  #define GI_GATHER_SKY(dir) vec3(0.0)
#endif

#ifndef GI_GATHER_DEFAULT_PLANET_ALBEDO
  #define GI_GATHER_DEFAULT_PLANET_ALBEDO vec3(0.25)
#endif

#ifndef GI_GATHER_OneOverPI
  #define GI_GATHER_OneOverPI 0.3183098861837907
#endif

// Result of a single gather ray: the surface that was hit (if any) together with everything needed to shade it.
struct GIGatherSurface {
  vec3 position;     // world space hit position (meters)
  vec3 normal;       // shading normal at the hit, oriented against the ray
  vec3 albedo;       // diffuse albedo (base color), linear
  vec3 emission;     // emissive radiance, linear
  float hitDistance; // distance from ray origin to hit; negative when the ray missed
  bool hit;          // true when the ray hit geometry, false on a sky/environment miss
};

// ---------------------------------------------------------------------------------------------------------------------
//  Closest-hit query with material extraction.
//
//  Mirrors tracePrimaryBasicGeometryRay() from raytracing.glsl for the geometry part, but additionally unpacks the hit
//  material so we can shade it (base color + emissive). It runs the alpha-handling proceed loop so cut-out / blended
//  geometry behaves the same as in the rasterization path.
// ---------------------------------------------------------------------------------------------------------------------
GIGatherSurface giGatherClosestHit(const in vec3 origin, const in vec3 direction, const in float tMin, const in float tMax, const in uint cullMask){

  GIGatherSurface s;
  s.position = origin + (direction * tMax);
  s.normal = -direction;
  s.albedo = vec3(0.0);
  s.emission = vec3(0.0);
  s.hitDistance = -1.0;
  s.hit = false;

  rayQueryEXT rayQuery;
  rayQueryInitializeEXT(rayQuery, uRaytracingTopLevelAccelerationStructure, 0u, cullMask, origin, tMin, direction, tMax);

  float temporaryAlpha;
  rayProceedEXTAlphaHandlingBasedLoop(rayQuery, true, temporaryAlpha);

  if(rayQueryGetIntersectionTypeEXT(rayQuery, true) != gl_RayQueryCommittedIntersectionTriangleEXT){
    rayQueryTerminateEXT(rayQuery);
    return s; // miss
  }

  s.hit = true;
  s.hitDistance = rayQueryGetIntersectionTEXT(rayQuery, true);

  int geometryID = rayQueryGetIntersectionGeometryIndexEXT(rayQuery, true);

  int geometryInstanceOffset = rayQueryGetIntersectionInstanceCustomIndexEXT(rayQuery, true);
  if((geometryInstanceOffset & 0x00800000) != 0){
    const int instanceID = rayQueryGetIntersectionInstanceIdEXT(rayQuery, true);
    geometryInstanceOffset = int(uRaytracingData.geometryInstanceOffsets.geometryInstanceOffsets[instanceID]);
  }

  RaytracingGeometryItem geometryItem = uRaytracingData.geometryItems.geometryItems[geometryInstanceOffset + geometryID];

  int primitiveID = rayQueryGetIntersectionPrimitiveIndexEXT(rayQuery, true);

  vec3 barycentrics = vec3(0.0, rayQueryGetIntersectionBarycentricsEXT(rayQuery, true));
  barycentrics.x = 1.0 - (barycentrics.y + barycentrics.z);

  uint indexOffset = geometryItem.indexOffset + (uint(primitiveID) * 3u);

  switch(geometryItem.objectType){

    case 0u:{ // Mesh

      uvec3 indices = uvec3(
        uRaytracingData.meshIndices.meshIndices[indexOffset + 0u],
        uRaytracingData.meshIndices.meshIndices[indexOffset + 1u],
        uRaytracingData.meshIndices.meshIndices[indexOffset + 2u]
      );

      uvec4 vertexPositionNormalXYArray[3] = uvec4[3](
        uRaytracingData.meshDynamicVertices.meshDynamicVertices[indices.x].positionNormalXY,
        uRaytracingData.meshDynamicVertices.meshDynamicVertices[indices.y].positionNormalXY,
        uRaytracingData.meshDynamicVertices.meshDynamicVertices[indices.z].positionNormalXY
      );

      vec3 vertexPositionArray[3] = vec3[3](
        uintBitsToFloat(vertexPositionNormalXYArray[0].xyz),
        uintBitsToFloat(vertexPositionNormalXYArray[1].xyz),
        uintBitsToFloat(vertexPositionNormalXYArray[2].xyz)
      );

      vec3 vertexNormalArray[3] = vec3[3](
        normalize(vec3(unpackSnorm2x16(vertexPositionNormalXYArray[0].w), unpackSnorm2x16(uRaytracingData.meshDynamicVertices.meshDynamicVertices[indices.x].normalZSignTangentXYZModelScaleXYZ.x).x)),
        normalize(vec3(unpackSnorm2x16(vertexPositionNormalXYArray[1].w), unpackSnorm2x16(uRaytracingData.meshDynamicVertices.meshDynamicVertices[indices.y].normalZSignTangentXYZModelScaleXYZ.x).x)),
        normalize(vec3(unpackSnorm2x16(vertexPositionNormalXYArray[2].w), unpackSnorm2x16(uRaytracingData.meshDynamicVertices.meshDynamicVertices[indices.z].normalZSignTangentXYZModelScaleXYZ.x).x))
      );

      s.position = (barycentrics.x * vertexPositionArray[0]) + (barycentrics.y * vertexPositionArray[1]) + (barycentrics.z * vertexPositionArray[2]);
      s.normal = normalize((barycentrics.x * vertexNormalArray[0]) + (barycentrics.y * vertexNormalArray[1]) + (barycentrics.z * vertexNormalArray[2]));
      if(dot(s.normal, direction) > 0.0){
        s.normal = -s.normal;
      }

      vec4 vertexTexCoordsArray[3] = vec4[3](
        uRaytracingData.meshStaticVertices.meshStaticVertices[indices.x].texCoords,
        uRaytracingData.meshStaticVertices.meshStaticVertices[indices.y].texCoords,
        uRaytracingData.meshStaticVertices.meshStaticVertices[indices.z].texCoords
      );

      vec4 vertexColorArray[3] = vec4[3](
        vec4(unpackHalf2x16(uRaytracingData.meshStaticVertices.meshStaticVertices[indices.x].color0MaterialID.x), unpackHalf2x16(uRaytracingData.meshStaticVertices.meshStaticVertices[indices.x].color0MaterialID.y)),
        vec4(unpackHalf2x16(uRaytracingData.meshStaticVertices.meshStaticVertices[indices.y].color0MaterialID.x), unpackHalf2x16(uRaytracingData.meshStaticVertices.meshStaticVertices[indices.y].color0MaterialID.y)),
        vec4(unpackHalf2x16(uRaytracingData.meshStaticVertices.meshStaticVertices[indices.z].color0MaterialID.x), unpackHalf2x16(uRaytracingData.meshStaticVertices.meshStaticVertices[indices.z].color0MaterialID.y))
      );

      vec4 vertexTexCoords = (barycentrics.x * vertexTexCoordsArray[0]) + (barycentrics.y * vertexTexCoordsArray[1]) + (barycentrics.z * vertexTexCoordsArray[2]);
      vec4 vertexColor = (barycentrics.x * vertexColorArray[0]) + (barycentrics.y * vertexColorArray[1]) + (barycentrics.z * vertexColorArray[2]);
      vec2 texCoords[2] = vec2[2]( vertexTexCoords.xy, vertexTexCoords.zw );

      Material material = uMaterials.materials[geometryItem.materialIndex];

      // Base color (texture index 0, sRGB) modulated by the base color factor and vertex color.
      s.albedo = raytracingTextureFetch(material, 0, vec4(1.0), true, texCoords).xyz * material.baseColorFactor.xyz * vertexColor.xyz;

      // Emissive (texture index 4, sRGB) modulated by the emissive factor (xyz) and strength (w) and vertex color.
      s.emission = raytracingTextureFetch(material, 4, vec4(1.0), true, texCoords).xyz * material.emissiveFactor.xyz * material.emissiveFactor.w * vertexColor.xyz;

      break;

    }

    case 2u:{ // Planet - geometry only; planet materials are not unpacked here, use a neutral albedo. TODO: planet material.

      mat4x3 objectToWorld = rayQueryGetIntersectionObjectToWorldEXT(rayQuery, true);

      ReferencedPlanetDataArray referencedPlanetDataArray = uRaytracingData.referencedPlanetDataArray;
      PlanetData planetData = referencedPlanetDataArray.planetData[geometryItem.objectIndex];
      RaytracingPlanetVertices raytracingPlanetVertices = RaytracingPlanetVertices(uvec2(planetData.verticesIndices.xy));
      RaytracingPlanetIndices raytracingPlanetIndices = RaytracingPlanetIndices(uvec2(planetData.verticesIndices.zw));

      uvec3 indices = uvec3(
        raytracingPlanetIndices.planetIndices[indexOffset + 0u],
        raytracingPlanetIndices.planetIndices[indexOffset + 1u],
        raytracingPlanetIndices.planetIndices[indexOffset + 2u]
      );

      vec3 vertexPositionArray[3] = vec3[3](
        objectToWorld * vec4(uintBitsToFloat(raytracingPlanetVertices.planetVertices[indices.x].xyz), 1.0),
        objectToWorld * vec4(uintBitsToFloat(raytracingPlanetVertices.planetVertices[indices.y].xyz), 1.0),
        objectToWorld * vec4(uintBitsToFloat(raytracingPlanetVertices.planetVertices[indices.z].xyz), 1.0)
      );

      vec3 vertexNormalArray[3] = vec3[3](
        normalize(objectToWorld * vec4(octSignedDecode(unpackSnorm2x16(raytracingPlanetVertices.planetVertices[indices.x].w)), 0.0)),
        normalize(objectToWorld * vec4(octSignedDecode(unpackSnorm2x16(raytracingPlanetVertices.planetVertices[indices.y].w)), 0.0)),
        normalize(objectToWorld * vec4(octSignedDecode(unpackSnorm2x16(raytracingPlanetVertices.planetVertices[indices.z].w)), 0.0))
      );

      s.position = (barycentrics.x * vertexPositionArray[0]) + (barycentrics.y * vertexPositionArray[1]) + (barycentrics.z * vertexPositionArray[2]);
      s.normal = normalize((barycentrics.x * vertexNormalArray[0]) + (barycentrics.y * vertexNormalArray[1]) + (barycentrics.z * vertexNormalArray[2]));
      if(dot(s.normal, direction) > 0.0){
        s.normal = -s.normal;
      }
      s.albedo = GI_GATHER_DEFAULT_PLANET_ALBEDO;
      s.emission = vec3(0.0);

      break;

    }

    default:{ // Particles and anything else: treat as a faintly lit facing surface so it does not punch a hole in the GI.
      s.position = origin + (direction * s.hitDistance);
      s.normal = -direction;
      s.albedo = vec3(0.0);
      s.emission = vec3(0.0);
      break;
    }

  }

  rayQueryTerminateEXT(rayQuery);

  return s;
}

// ---------------------------------------------------------------------------------------------------------------------
//  Lambertian contribution of a single light at a gather hit, with an optional ray-traced shadow. This mirrors the
//  light model used by the voxel cone tracing radiance transfer (voxelEvaluateLight) so the indirect light matches the
//  direct lighting, but resolves visibility with an actual shadow ray instead of a shadow map.
// ---------------------------------------------------------------------------------------------------------------------
vec3 giGatherEvaluateLight(const in Light light, const in vec3 worldPosition, const in vec3 normal){
  uint lightType = light.metaData.x & 0x0000000fu;
  vec3 pointToLightVector;
  vec3 pointToLightDirection;
  float shadowTMax;
  if((lightType == 1u) || (lightType == 4u)){ // Directional / primary directional (sun)
    pointToLightDirection = normalize(-light.directionRange.xyz);
    pointToLightVector = pointToLightDirection * float(GI_GATHER_SHADOW_TMAX);
    shadowTMax = float(GI_GATHER_SHADOW_TMAX);
  }else{ // Point, spot, view directional
    pointToLightVector = light.positionRadius.xyz - worldPosition;
    pointToLightDirection = normalize(pointToLightVector);
    shadowTMax = length(pointToLightVector);
  }

  float NdotL = clamp(dot(normal, pointToLightDirection), 0.0, 1.0);
  if(NdotL <= 0.0){
    return vec3(0.0);
  }

  float attenuation = 1.0;
  if(lightType == 3u){ // Spot cone angular attenuation
    float angularAttenuation = clamp(fma(dot(normalize(light.directionRange.xyz), -pointToLightDirection), uintBitsToFloat(light.metaData.z), uintBitsToFloat(light.metaData.w)), 0.0, 1.0);
    attenuation *= angularAttenuation * angularAttenuation;
  }
  if((lightType == 2u) || (lightType == 3u) || (lightType == 5u)){ // Distance attenuation for positional lights
    if(light.directionRange.w >= 0.0){
      float currentDistance = length(pointToLightVector);
      if(currentDistance > 0.0){
        attenuation *= 1.0 / (currentDistance * currentDistance);
        if(light.directionRange.w > 0.0){
          float distanceByRange = currentDistance / light.directionRange.w;
          distanceByRange *= distanceByRange;
          attenuation *= clamp(1.0 - (distanceByRange * distanceByRange), 0.0, 1.0);
        }
      }
    }
  }
  if(attenuation <= 0.0){
    return vec3(0.0);
  }

  float shadow = 1.0;
#if GI_GATHER_TRACE_SHADOWS
  shadow = getRaytracedFastHardShadow(worldPosition, normal, pointToLightDirection, uintBitsToFloat(0x00000001u), shadowTMax);
#endif

  return (light.colorIntensity.xyz * light.colorIntensity.w) * (NdotL * attenuation * shadow);
}

// Accumulate direct lighting at a gather hit by traversing the light BVH (same traversal as voxelEvaluateLighting).
vec3 giGatherEvaluateLighting(const in vec3 worldPosition, const in vec3 normal){
  vec3 result = vec3(0.0);
  uint lightTreeNodeCount = lightTreeNodes[0].aabbMinSkipCount.w;
  uint lightTreeNodeIndex = 0u;
  while(lightTreeNodeIndex < lightTreeNodeCount){
    LightTreeNode lightTreeNode = lightTreeNodes[lightTreeNodeIndex];
    vec3 aabbMin = uintBitsToFloat(lightTreeNode.aabbMinSkipCount.xyz);
    vec3 aabbMax = uintBitsToFloat(lightTreeNode.aabbMaxUserData.xyz);
    if(all(greaterThanEqual(worldPosition, aabbMin)) && all(lessThanEqual(worldPosition, aabbMax))){
      if(lightTreeNode.aabbMaxUserData.w != 0xffffffffu){
        result += giGatherEvaluateLight(lights[lightTreeNode.aabbMaxUserData.w], worldPosition, normal);
      }
      lightTreeNodeIndex++;
    }else{
      lightTreeNodeIndex += max(1u, lightTreeNode.aabbMinSkipCount.w);
    }
  }
  return result;
}

// ---------------------------------------------------------------------------------------------------------------------
//  Outgoing radiance towards the ray origin at a gather hit.
//    Lo = emission + (albedo / PI) * (directLight + previousFrameIndirect)
//  previousFrameIndirect is the irradiance that the caller sampled from the *previous* frame's GI data structure at the
//  hit point (probe field for DDGI, hash grid for surfels). Passing it in here gives multi-bounce ("infinite bounce")
//  lighting almost for free; pass vec3(0.0) to disable it.
// ---------------------------------------------------------------------------------------------------------------------
vec3 giGatherShadeHit(const in GIGatherSurface surface, const in vec3 previousFrameIndirect){
  vec3 directLight = giGatherEvaluateLighting(surface.position, surface.normal);
  return surface.emission + (surface.albedo * float(GI_GATHER_OneOverPI) * (directLight + previousFrameIndirect));
}

// Convenience: trace one gather ray and return its radiance, sampling the sky on a miss. previousFrameIndirect is only
// applied at geometry hits.
vec3 giGatherTraceRadiance(const in vec3 origin, const in vec3 direction, const in float tMin, const in float tMax, const in uint cullMask, const in vec3 previousFrameIndirect, out float hitDistance){
  GIGatherSurface surface = giGatherClosestHit(origin, direction, tMin, tMax, cullMask);
  hitDistance = surface.hitDistance;
  if(!surface.hit){
    return GI_GATHER_SKY(direction);
  }
  return giGatherShadeHit(surface, previousFrameIndirect);
}

#endif // RAYTRACING

#endif // GI_RT_GATHER_GLSL
