#ifndef DECALS_GLSL
#define DECALS_GLSL

#ifdef LIGHTS

#include "blendnormals.glsl"

// Overlay blend mode
vec3 decalOverlayBlend(vec3 base, vec3 blend) {
  return mix(2.0 * base * blend, 1.0 - (2.0 * ((1.0 - base) * (1.0 - blend)), step(0.5, base));
}

// Apply decals to material properties - Full PBR workflow
void applyDecals(
  inout vec4 baseColor,           // RGBA - albedo + alpha
  inout float metallic,           // Metallic value
  inout float perceptualRoughness,// Roughness value
  inout float occlusion,          // Ambient occlusion
  inout vec3 F0Dielectric,        // Base reflectance for dielectrics
  inout vec3 F90Dielectric,       // Grazing angle reflectance
  inout float specularWeight,     // Specular intensity
  inout vec3 normalTangentSpace,  // Tangent space normal
  in vec3 worldPosition,
  in vec3 worldNormal,
  in vec3 viewSpacePosition,
  in vec3 baseIOR_F0Dielectric    // Base IOR-derived F0 for neutral specular calculation
) {
  
#if defined(LIGHTCLUSTERS)
  // ===========================================================================
  // CLUSTER-BASED DECAL LOOKUP (EXACT SAME pattern as lighting.glsl)
  // ===========================================================================
  
  // Decal cluster grid (reuses same cluster calculation as lights)
  uvec3 clusterXYZ = uvec3(uvec2(uvec2(gl_FragCoord.xy) / uFrustumClusterGridGlobals.tileSizeZNearZFar.xy), 
                           uint(clamp(fma(log2(-viewSpacePosition.z), uFrustumClusterGridGlobals.scaleBiasMax.x, uFrustumClusterGridGlobals.scaleBiasMax.y), 0.0, uFrustumClusterGridGlobals.scaleBiasMax.z)));
  uint clusterIndex = clamp((((clusterXYZ.z * uFrustumClusterGridGlobals.clusterSize.y) + clusterXYZ.y) * uFrustumClusterGridGlobals.clusterSize.x) + clusterXYZ.x, 0u, uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.z) +
                      (uint(gl_ViewIndex + uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.w) * uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.z);
  uvec2 clusterDecalData = frustumClusterGridData[clusterIndex].zw; // z = offset, w = count (and ignore light data for now)
  for(uint clusterDecalIndex = clusterDecalData.x, clusterCountDecals = clusterDecalData.y; clusterCountDecals > 0u; clusterDecalIndex++, clusterCountDecals--){
    {
      {
        Decal decal = decals[frustumClusterGridIndexList[clusterDecalIndex]];
#else
  // ===========================================================================
  // BVH/SKIP-TREE DECAL LOOKUP (EXACT SAME pattern as lighting.glsl)
  // ===========================================================================
  
  // Decal BVH
  uint decalTreeNodeIndex = 0;
  uint decalTreeNodeCount = decalTreeNodes[0].aabbMinSkipCount.w;
  while (decalTreeNodeIndex < decalTreeNodeCount) {
    DecalTreeNode decalTreeNode = decalTreeNodes[decalTreeNodeIndex];
    vec3 aabbMin = vec3(uintBitsToFloat(uvec3(decalTreeNode.aabbMinSkipCount.xyz)));
    vec3 aabbMax = vec3(uintBitsToFloat(uvec3(decalTreeNode.aabbMaxUserData.xyz)));
    if (all(greaterThanEqual(worldPosition, aabbMin)) && all(lessThanEqual(worldPosition, aabbMax))) {
      if (decalTreeNode.aabbMaxUserData.w != 0xffffffffu) {
        Decal decal = decals[decalTreeNode.aabbMaxUserData.w];
#endif
        
        // Project world position into decal OBB space
        vec3 decalSpacePos = (decal.worldToDecalMatrix * vec4(worldPosition, 1.0)).xyz;
        
        // Check if fragment is inside decal box (OBB bounds: -0.5 to 0.5)
        if(all(greaterThan(decalSpacePos + 0.5, vec3(0.0))) && 
           all(lessThan(decalSpacePos, vec3(0.5)))) {
          
          // Calculate UVs [0,1]
          vec2 decalUV = decalSpacePos.xy + 0.5;
          decalUV = decalUV * decal.uvScaleOffset.xy + decal.uvScaleOffset.zw;
          
          // Edge fade (soft edges at decal boundaries)
          vec2 edgeDist = min(decalUV, 1.0 - decalUV) * 2.0;
          float edgeFade = smoothstep(0.0, uintBitsToFloat(decal.blendParams.z), min(edgeDist.x, edgeDist.y));
          
          // Angle fade (fade based on surface orientation vs decal forward)
          float angleFade = saturate(dot(normalize(worldNormal), normalize(decal.decalForward.xyz)));
          angleFade = pow(angleFade, uintBitsToFloat(decal.blendParams.y));
          
          // Sample decal textures from unified u2DTextures array
          vec4 decalAlbedo = (decal.textureIndices.x >= 0) ? texture(u2DTextures[nonuniformEXT((decal.textureIndices.x & 0x3fff) << 1) | 1], decalUV) : vec4(1.0);
          vec3 decalNormalTangentSpace = (decal.textureIndices.y >= 0) ? (texture(u2DTextures[nonuniformEXT((decal.textureIndices.y & 0x3fff) << 1)], decalUV).xyz * 2.0 - 1.0) : vec3(0.0, 0.0, 1.0);
          vec3 decalORM = (decal.textureIndices.z >= 0) ? texture(u2DTextures[nonuniformEXT((decal.textureIndices.z & 0x3fff) << 1)], decalUV).xyz : vec3(1.0);
          vec4 decalSpecular = (decal.textureIndices.w >= 0) ? texture(u2DTextures[nonuniformEXT((decal.textureIndices.w & 0x3fff) << 1)], decalUV) : vec4(1.0, 1.0, 1.0, 1.0);
          vec3 decalEmissive = (decal.textureIndices2.x >= 0) ? texture(u2DTextures[nonuniformEXT((decal.textureIndices2.x & 0x3fff) << 1) | 1], decalUV).xyz : vec3(0.0);
          
          // Extract ORM components
          float decalOcclusion = decalORM.x;
          float decalRoughness = decalORM.y;
          float decalMetallic = decalORM.z;
          
          // Extract specular components
          vec3 decalSpecularColorFactor = decalSpecular.xyz;
          float decalSpecularWeight = decalSpecular.w;
          
          // Calculate decal's Fresnel properties
          vec3 decalF0Dielectric = min(baseIOR_F0Dielectric * decalSpecularColorFactor, vec3(1.0));
          vec3 decalF90Dielectric = vec3(decalSpecularWeight);
          
          // Combined blend factor
          float blend = decalAlbedo.a * uintBitsToFloat(decal.blendParams.x) * angleFade * edgeFade;
          
          // Apply blend mode
          uint blendMode = decal.blendParams.w;
          switch(blendMode) {
            case 0u: {  // Alpha blend (standard)
              baseColor.xyz = mix(baseColor.xyz, decalAlbedo.xyz, blend);
              metallic = mix(metallic, decalMetallic, blend);
              perceptualRoughness = mix(perceptualRoughness, decalRoughness, blend);
              occlusion = mix(occlusion, decalOcclusion, blend);
              F0Dielectric = mix(F0Dielectric, decalF0Dielectric, blend);
              F90Dielectric = mix(F90Dielectric, decalF90Dielectric, blend);
              specularWeight = mix(specularWeight, decalSpecularWeight, blend);
              break;
            }
            case 1u: {  // Multiply (dirt/grime/darkening)
              baseColor.xyz *= mix(vec3(1.0), decalAlbedo.xyz, blend);
              // Less influence on material properties for multiply
              metallic = mix(metallic, decalMetallic, blend * 0.5);
              perceptualRoughness = mix(perceptualRoughness, decalRoughness, blend * 0.5);
              occlusion = mix(occlusion, decalOcclusion, blend);
              // Don't modify specular for multiply mode
              break;
            }
            case 2u: {  // Overlay (painted markings)
              baseColor.xyz = mix(baseColor.xyz, decalOverlayBlend(baseColor.xyz, decalAlbedo.xyz), blend);
              metallic = mix(metallic, decalMetallic, blend);
              perceptualRoughness = mix(perceptualRoughness, decalRoughness, blend);
              occlusion = mix(occlusion, decalOcclusion, blend);
              F0Dielectric = mix(F0Dielectric, decalF0Dielectric, blend);
              F90Dielectric = mix(F90Dielectric, decalF90Dielectric, blend);
              specularWeight = mix(specularWeight, decalSpecularWeight, blend);
              break;
            }
            case 3u: {  // Additive (glowing effects)
              baseColor.xyz += decalAlbedo.xyz * blend;
              // Don't modify material properties for additive
              break;
            }
            default: {
              // Alpha blend as fallback
              baseColor.xyz = mix(baseColor.xyz, decalAlbedo.xyz, blend);
              metallic = mix(metallic, decalMetallic, blend);
              perceptualRoughness = mix(perceptualRoughness, decalRoughness, blend);
              occlusion = mix(occlusion, decalOcclusion, blend);
              F0Dielectric = mix(F0Dielectric, decalF0Dielectric, blend);
              F90Dielectric = mix(F90Dielectric, decalF90Dielectric, blend);
              specularWeight = mix(specularWeight, decalSpecularWeight, blend);
              break;
            }
          }
          
          // Blend normals
          normalTangentSpace = blendNormals(normalTangentSpace, decalNormalTangentSpace, blend);
#if defined(LIGHTCLUSTERS)
        }
      }
    }
  }
#else
        }
      }
      decalTreeNodeIndex++;
    } else {
      decalTreeNodeIndex += max(1u, decalTreeNode.aabbMinSkipCount.w);
    }
  }
#endif
}

// Simplified decal application for unlit materials
// Only applies albedo, no material properties or normals
void applyDecalsUnlit(
  inout vec3 color,               // RGB color
  in vec3 worldPosition,
  in vec3 worldNormal,
  in vec3 viewSpacePosition
) {
  
#if defined(LIGHTCLUSTERS)
  // Decal cluster grid
  uvec3 clusterXYZ = uvec3(uvec2(uvec2(gl_FragCoord.xy) / uFrustumClusterGridGlobals.tileSizeZNearZFar.xy), 
                           uint(clamp(fma(log2(-viewSpacePosition.z), uFrustumClusterGridGlobals.scaleBiasMax.x, uFrustumClusterGridGlobals.scaleBiasMax.y), 0.0, uFrustumClusterGridGlobals.scaleBiasMax.z)));
  uint clusterIndex = clamp((((clusterXYZ.z * uFrustumClusterGridGlobals.clusterSize.y) + clusterXYZ.y) * uFrustumClusterGridGlobals.clusterSize.x) + clusterXYZ.x, 0u, uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.z) +
                      (uint(gl_ViewIndex + uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.w) * uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.z);
  uvec2 clusterDecalData = frustumClusterGridData[clusterIndex].zw;
  for(uint clusterDecalIndex = clusterDecalData.x, clusterCountDecals = clusterDecalData.y; clusterCountDecals > 0u; clusterDecalIndex++, clusterCountDecals--){
    {
      {
        Decal decal = decals[frustumClusterGridIndexList[clusterDecalIndex]];
#else
  // Decal BVH
  uint decalTreeNodeIndex = 0;
  uint decalTreeNodeCount = decalTreeNodes[0].aabbMinSkipCount.w;
  while (decalTreeNodeIndex < decalTreeNodeCount) {
    DecalTreeNode decalTreeNode = decalTreeNodes[decalTreeNodeIndex];
    vec3 aabbMin = vec3(uintBitsToFloat(uvec3(decalTreeNode.aabbMinSkipCount.xyz)));
    vec3 aabbMax = vec3(uintBitsToFloat(uvec3(decalTreeNode.aabbMaxUserData.xyz)));
    if (all(greaterThanEqual(worldPosition, aabbMin)) && all(lessThanEqual(worldPosition, aabbMax))) {
      if (decalTreeNode.aabbMaxUserData.w != 0xffffffffu) {
        Decal decal = decals[decalTreeNode.aabbMaxUserData.w];
#endif
    
        // Project world position into decal OBB space
        vec3 decalSpacePos = (decal.worldToDecalMatrix * vec4(worldPosition, 1.0)).xyz;
        
        // Check if fragment is inside decal box
        if(all(greaterThan(decalSpacePos + 0.5, vec3(0.0))) && 
           all(lessThan(decalSpacePos, vec3(0.5)))) {
          
          // Calculate UVs
          vec2 decalUV = decalSpacePos.xy + 0.5;
          decalUV = decalUV * decal.uvScaleOffset.xy + decal.uvScaleOffset.zw;
          
          // Edge fade
          vec2 edgeDist = min(decalUV, 1.0 - decalUV) * 2.0;
          float edgeFade = smoothstep(0.0, uintBitsToFloat(decal.blendParams.z), min(edgeDist.x, edgeDist.y));
          
          // Angle fade
          float angleFade = saturate(dot(normalize(worldNormal), normalize(decal.decalForward.xyz)));
          angleFade = pow(angleFade, uintBitsToFloat(decal.blendParams.y));
          
          // Sample only albedo texture for unlit
          vec4 decalAlbedo = (decal.textureIndices.x >= 0) ? texture(u2DTextures[nonuniformEXT((decal.textureIndices.x & 0x3fff) << 1) | 1], decalUV) : vec4(1.0);
          
          // Combined blend factor
          float blend = decalAlbedo.w * uintBitsToFloat(decal.blendParams.x) * angleFade * edgeFade;
          
          // Apply blend mode (only color, no material properties)
          uint blendMode = decal.blendParams.w;
          switch(blendMode) {
            case 0u: {  // Alpha blend
              color = mix(color, decalAlbedo.xyz, blend);
              break;
            }
            case 1u: {  // Multiply
              color *= mix(vec3(1.0), decalAlbedo.xyz, blend);
              break;
            }
            case 2u: {  // Overlay
              color = mix(color, decalOverlayBlend(color, decalAlbedo.xyz), blend);
              break;
            }
            case 3u: {  // Additive
              color += decalAlbedo.xyz * blend;
              break;
            }
            default: {
              color = mix(color, decalAlbedo.xyz, blend);
              break;
            }
          }
#if defined(LIGHTCLUSTERS)
        }
      }
    }
  }
#else
        }
      }
      decalTreeNodeIndex++;
    } else {
      decalTreeNodeIndex += max(1u, decalTreeNode.aabbMinSkipCount.w);
    }
  }
#endif
}

#endif // LIGHTS

#endif // DECALS_GLSL
