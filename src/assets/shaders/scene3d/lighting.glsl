#if defined(LIGHTING_GLOBALS)

#ifdef RAYTRACING
#ifdef RAYTRACED_SOFT_SHADOWS
  #include "bluenoise.glsl"
  #include "pcg.glsl"
  //#include "possiondisks.glsl"
  #include "tangentspacebasis.glsl"
#endif
#endif

#elif defined(LIGHTING_INITIALIZATION)

#ifdef RAYTRACING
#ifdef RAYTRACED_SOFT_SHADOWS
  uvec3 shadowDiscRandomValues = pcgHash33(uvec3(uvec2(gl_FragCoord.xy), uint(pushConstants.frameIndex)));
  float shadowDiscRotationAngle = fract((uintBitsToFloat(((shadowDiscRandomValues.x >> 9u) & 0x007fffffu) | 0x3f800000u) - 1.0) + (float(int(pushConstants.frameIndex & 4095)) * 0.61803398875)) * 6.283185307179586476925286766559;
  vec2 shadowDiscRotation = vec2(sin(vec2(shadowDiscRotationAngle) + vec2(1.5707963267948966, 0.0)));
  mat2 shadowDiscRotationMatrix = mat2(shadowDiscRotation.x, shadowDiscRotation.y, -shadowDiscRotation.y, shadowDiscRotation.x);
#endif
#endif

#elif defined(LIGHTING_IMPLEMENTATION)

#ifdef LIGHTS
#if defined(REFLECTIVESHADOWMAPOUTPUT)
      if(lights[0].metaData.x == 4u){ // Only the first light is supported for RSMs, and only when it is the primary directional light 
        for(int lightIndex = 0; lightIndex < 1; lightIndex++){
          {
            Light light = lights[lightIndex];
#elif defined(LIGHTCLUSTERS)
      // Light cluster grid
      uvec3 clusterXYZ = uvec3(uvec2(uvec2(gl_FragCoord.xy) / uFrustumClusterGridGlobals.tileSizeZNearZFar.xy), 
                               uint(clamp(fma(log2(-inViewSpacePosition.z), uFrustumClusterGridGlobals.scaleBiasMax.x, uFrustumClusterGridGlobals.scaleBiasMax.y), 0.0, uFrustumClusterGridGlobals.scaleBiasMax.z)));
      uint clusterIndex = clamp((((clusterXYZ.z * uFrustumClusterGridGlobals.clusterSize.y) + clusterXYZ.y) * uFrustumClusterGridGlobals.clusterSize.x) + clusterXYZ.x, 0u, uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.z) +
                          (uint(gl_ViewIndex + uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.w) * uFrustumClusterGridGlobals.countLightsViewIndexSizeOffsetedViewIndex.z);
      uvec2 clusterData = frustumClusterGridData[clusterIndex].xy; // x = index, y = count and ignore decal data for now  
      for(uint clusterLightIndex = clusterData.x, clusterCountLights = clusterData.y; clusterCountLights > 0u; clusterLightIndex++, clusterCountLights--){
        {
          Light light = lights[frustumClusterGridIndexList[clusterLightIndex]];
          if(distance(light.positionRadius.xyz, inWorldSpacePosition.xyz) <= light.positionRadius.w){
#else
      // Light BVH
      uint lightTreeNodeIndex = 0;
      uint lightTreeNodeCount = lightTreeNodes[0].aabbMinSkipCount.w;
      while (lightTreeNodeIndex < lightTreeNodeCount) {
        LightTreeNode lightTreeNode = lightTreeNodes[lightTreeNodeIndex];
        vec3 aabbMin = vec3(uintBitsToFloat(uvec3(lightTreeNode.aabbMinSkipCount.xyz)));
        vec3 aabbMax = vec3(uintBitsToFloat(uvec3(lightTreeNode.aabbMaxUserData.xyz)));
        if (all(greaterThanEqual(inWorldSpacePosition.xyz, aabbMin)) && all(lessThanEqual(inWorldSpacePosition.xyz, aabbMax))) {
          if (lightTreeNode.aabbMaxUserData.w != 0xffffffffu) {
            Light light = lights[lightTreeNode.aabbMaxUserData.w];
#endif
            float lightAttenuation = 1.0;
            vec3 lightDirection;
            vec3 lightPosition = light.positionRadius.xyz; 
            vec3 lightVector = lightPosition - inWorldSpacePosition.xyz;
            vec3 normalizedLightVector = normalize(lightVector);
#ifdef SHADOWS
#if defined(RAYTRACING)
            vec3 rayOrigin = inWorldSpacePosition.xyz;
            vec3 rayNormal = triangleNormal;
            float rayOffset = 0.0;
#endif
#if !defined(REFLECTIVESHADOWMAPOUTPUT)
            if (/*(uShadows != 0) &&*/receiveShadows && ((light.metaData.y & 0x80000000u) == 0u) && (uCascadedShadowMaps.metaData.x != SHADOWMAP_MODE_NONE)) {
              switch (light.metaData.x) {
#if !defined(REFLECTIVESHADOWMAPOUTPUT)
#if defined(RAYTRACING)
                case 1u: { // Directional 
                  lightAttenuation *= getRaytracedHardShadow(rayOrigin, rayNormal, normalize(-light.directionRange.xyz), rayOffset, 10000000.0);
                  break;
                }
                case 2u: {  // Point
                  // Fall-through, because same raytracing attempt as for spot lights. 
                }
                case 3u: {  // Spot
                  lightAttenuation *= getRaytracedHardShadow(rayOrigin, rayNormal, normalizedLightVector, rayOffset, min(10000000.0, length(lightVector)));
                  break;
                }
#elif 0
                case 1u: { // Directional 
                  // imageLightBasedLightDirection = light.directionRange.xyz;
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
                  float znear = 1e-2, zfar = 0.0; // TODO
                  vec3 vector = light.positionRadius.xyz - inWorldSpacePosition;
                  vec4 moments = (textureLod(uCubeMapShadowMapArrayTexture, vec4(vec3(normalize(vector)), float(int(light.metaData.y))), 0.0) + vec2(-0.035955884801, 0.0).xyyy) * mat4(0.2227744146, 0.0771972861, 0.7926986636, 0.0319417555, 0.1549679261, 0.1394629426, 0.7963415838, -0.172282317, 0.1451988946, 0.2120202157, 0.7258694464, -0.2758014811, 0.163127443, 0.2591432266, 0.6539092497, -0.3376131734);
                  lightAttenuation *= reduceLightBleeding(getMSMShadowIntensity(moments, clamp((length(vector) - znear) / (zfar - znear), 0.0, 1.0), 5e-3, 1e-2), 0.0);
                  break;
                }
#endif
#endif
                case 4u: {  // Primary directional
                  imageLightBasedLightDirection = light.directionRange.xyz;
                  litIntensity = lightAttenuation;
#if defined(RAYTRACING)
#ifdef RAYTRACED_SOFT_SHADOWS
                  if(true){
                    // Soft shadow
                    const int countSamples = 8;
                    vec3 lightNormal = normalize(-light.directionRange.xyz);
                    vec3 lightTangent = normalize(cross(lightNormal, getPerpendicularVector(lightNormal)));
                    vec3 lightBitangent = cross(lightNormal, lightTangent);
                    float shadow = 0.0;
                    for(int i = 0; i < countSamples; i++){
                      vec2 sampleXY = (shadowDiscRotationMatrix * BlueNoise2DDisc[(i + int(shadowDiscRandomValues.y)) & BlueNoise2DDiscMask]) * 1e-2;
                      vec3 sampleDirection = normalize(lightNormal + (sampleXY.x * lightTangent) + (sampleXY.y * lightBitangent));
                      shadow += getRaytracedHardShadow(rayOrigin, rayNormal, sampleDirection, rayOffset, 10000000.0);
                    }
                    lightAttenuation *= shadow / float(countSamples);                    
                  }else{
                    // Hard shadow 
                    lightAttenuation *= getRaytracedHardShadow(rayOrigin, rayNormal, normalize(-light.directionRange.xyz), rayOffset, 10000000.0);
                  }
#else
                  lightAttenuation *= getRaytracedHardShadow(rayOrigin, rayNormal, normalize(-light.directionRange.xyz), rayOffset, 10000000.0);
#endif
#else
                  float viewSpaceDepth = -inViewSpacePosition.z;
#ifdef UseReceiverPlaneDepthBias
                  // Outside of doCascadedShadowMapShadow as an own loop, for the reason, that the partial derivative based
                  // computeReceiverPlaneDepthBias function can work correctly then, when all cascaded shadow map slice
                  // position are already known in advance, and always at any time and at any real current cascaded shadow 
                  // map slice. Because otherwise one can see dFdx/dFdy caused artefacts on cascaded shadow map border
                  // transitions.  
                  {
                    const vec3 lightDirection = -light.directionRange.xyz;
                    for(int cascadedShadowMapIndex = 0; cascadedShadowMapIndex < NUM_SHADOW_CASCADES; cascadedShadowMapIndex++){
                      vec3 worldSpacePosition = getOffsetedBiasedWorldPositionForShadowMapping(uCascadedShadowMaps.constantBiasNormalBiasSlopeBiasClamp[cascadedShadowMapIndex], lightDirection);
                      vec4 shadowPosition = uCascadedShadowMaps.shadowMapMatrices[cascadedShadowMapIndex] * vec4(worldSpacePosition, 1.0);
                      shadowPosition = fma(shadowPosition / shadowPosition.w, vec2(0.5, 1.0).xxyy, vec2(0.5, 0.0).xxyy);
                      cascadedShadowMapPositions[cascadedShadowMapIndex] = shadowPosition;
                    }
                  }
#endif

                  float shadow = 1.0;

                  vec3 shadowUVW;

                  // Find the first cascaded shadow map slice, which is responsible for the current fragment.
                  int cascadedShadowMapIndex = 0;
                  while(cascadedShadowMapIndex < NUM_SHADOW_CASCADES) {
                    shadow = doCascadedShadowMapShadow(cascadedShadowMapIndex, -light.directionRange.xyz, shadowUVW);
                    if (shadow < 0.0){
                      // The current fragment is outside of the current cascaded shadow map slice, so try the next one.
                      cascadedShadowMapIndex++;
                    }else{
                      // The current fragment is inside of the current cascaded shadow map slice, so use it.
                      break;
                    }
                  }

                  if((cascadedShadowMapIndex + 1) < NUM_SHADOW_CASCADES){
                    // Calculate the factor by fading out the shadow map at the edges itself, with 20% corner threshold.
                    // This gives better results than fading by view depth, which is used often elsewhere, where each 
                    // cascaded shadow map slice has a different depth range.
                    vec3 edgeFactor = clamp((clamp(abs(shadowUVW), vec3(0.0), vec3(1.0)) - vec3(0.8)) * 5.0, vec3(0.0), vec3(1.0)); 
                    float factor = clamp(max(edgeFactor.x, max(edgeFactor.y, edgeFactor.z)) * 1.05, 0.0, 1.0); // 5% over the edgeFactor for reducing the shadow map transition artefacts at the cascaded shadow map slice borders.
                    if(factor > 0.0){
                      // The current fragment is inside of the current cascaded shadow map slice, but also inside of the next one.
                      // So fade between the two shadow map slices. But notice that nextShadow can also -1.0, when the current fragment
                      // is outside of the next cascaded shadow map slice. In this case we fade into the no shadow case for smooth
                      // shadow map transitions even at the whole cascaded shadow map slice border.
                      float nextShadow = doCascadedShadowMapShadow(cascadedShadowMapIndex + 1, -light.directionRange.xyz, shadowUVW);
                      shadow = mix(shadow, (nextShadow < 0.0) ? 1.0 : nextShadow, factor); 
                    }
                  }

                  if(shadow < 0.0){
                    shadow = 1.0; // The current fragment is outside of the cascaded shadow map range, so use no shadow then instead.
                  } 

                  lightAttenuation *= clamp(shadow, 0.0, 1.0); // Clamp just for safety, should not be necessary, but don't hurt either.
#endif // RAYTRACING
                  break;
                }
              }
#if 0              
              if (lightIndex == 0) {
                litIntensity = lightAttenuation;
              }
#endif
            }
#endif // !defined(REFLECTIVESHADOWMAPOUTPUT)
            float lightAttenuationEx = lightAttenuation;
#endif // SHADOWS
            switch (light.metaData.x) {
#if !defined(REFLECTIVESHADOWMAPOUTPUT)
              case 1u: {  // Directional
                lightDirection = -light.directionRange.xyz;
                break;
              }
              case 2u: {  // Point
                lightDirection = normalizedLightVector;
                break;
              }
              case 3u: {  // Spot
#if 1
                float angularAttenuation = clamp(fma(dot(normalize(light.directionRange.xyz), -normalizedLightVector), uintBitsToFloat(light.metaData.z), uintBitsToFloat(light.metaData.w)), 0.0, 1.0);
#else
                // Just for as reference
                float innerConeCosinus = uintBitsToFloat(light.metaData.z);
                float outerConeCosinus = uintBitsToFloat(light.metaData.w);
                float actualCosinus = dot(normalize(light.directionRange.xyz), -normalizedLightVector);                
                float angularAttenuation = (actualCosinus > outerConeCosinus) ? 0.0 : ((actualCosinus < innerConeCosinus) ? ((actualCosinus - outerConeCosinus) / (innerConeCosinus - outerConeCosinus))) : 1.0;
//              float angularAttenuation = mix(0.0, mix((actualCosinus - outerConeCosinus) / (innerConeCosinus - outerConeCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
//              float angularAttenuation = mix(0.0, mix(smoothstep(outerConeCosinus, innerConeCosinus, actualCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
#endif
                lightAttenuation *= angularAttenuation * angularAttenuation;
                lightDirection = normalizedLightVector;
                break;
              }
#endif // !defined(REFLECTIVESHADOWMAPOUTPUT)
              case 4u: {  // Primary directional
                imageLightBasedLightDirection = lightDirection = -light.directionRange.xyz;
                break;
              }
              default: {
                continue;
              }
            }
#if !defined(REFLECTIVESHADOWMAPOUTPUT)
            switch (light.metaData.x) {
              case 2u:    // Point
              case 3u: {  // Spot
                if (light.directionRange.w >= 0.0) {
                  float currentDistance = length(lightVector);
                  if (currentDistance > 0.0) {
                    lightAttenuation *= 1.0 / (currentDistance * currentDistance);
                    if (light.directionRange.w > 0.0) {
                      float distanceByRange = currentDistance / light.directionRange.w;
                      distanceByRange *= distanceByRange;
                      lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange), 0.0, 1.0);
                    }
                  }
                }
                break;
              }
            }
#endif
            if((lightAttenuation > 0.0) 
#ifdef CAN_HAVE_EXTENDED_PBR_MATERIAL
               || ((flags & ((1u << 7u) | (1u << 8u))) != 0u)
#endif
               ){
#if defined(REFLECTIVESHADOWMAPOUTPUT)
              diffuseOutput += lightAttenuation * light.colorIntensity.xyz * light.colorIntensity.w * diffuseColorAlpha.xyz; // * clamp(dot(normal, lightDirection), 0.0, 1.0);
#elif defined(PROCESSLIGHT)
              PROCESSLIGHT(light.colorIntensity.xyz * light.colorIntensity.w,  //
                            vec3(lightAttenuation),                            //
                            lightDirection); 
#else
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
                            sheenColor,                                         //
                            sheenRoughness,                                     //
                            clearcoatNormal,                                    //
                            clearcoatF0,                                        //
                            clearcoatRoughness,                                 //
                            specularWeight);                                    //
#endif
#ifdef TRANSMISSION
#ifndef TRANSMISSION_FORCED
              if((flags & (1u << 11u)) != 0u) 
#endif
              {
                // If the light ray travels through the geometry, use the point it exits the geometry again.
                // That will change the angle to the light source, if the material refracts the light ray.
                if(abs(volumeDispersion) > 1e-7){
                  float realIOR = 1.0 / ior;
                  float iorDispersionSpread = 0.04 * volumeDispersion * (realIOR - 1.0);
                  vec3 iorValues = vec3(1.0 / (realIOR - iorDispersionSpread), ior, 1.0 / (realIOR + iorDispersionSpread));
                  for(int i = 0; i < 3; i++){
                    vec3 transmissionRay = getVolumeTransmissionRay(normal.xyz, viewDirection, volumeThickness, iorValues[i]);
                    vec3 pointToLight = ((light.metaData.x == 0) ? lightDirection : lightVector) - transmissionRay;
                    vec3 normalizedLightVector = normalize(pointToLight);
                    float lightAttenuation = lightAttenuationEx;
                    switch (light.metaData.x) {
                      case 3u: {  // Spot 
    #if 1
                        float angularAttenuation = clamp(fma(dot(normalize(light.directionRange.xyz), -normalizedLightVector), uintBitsToFloat(light.metaData.z), uintBitsToFloat(light.metaData.w)), 0.0, 1.0);
    #else
                        // Just for as reference
                        float innerConeCosinus = uintBitsToFloat(light.metaData.z);
                        float outerConeCosinus = uintBitsToFloat(light.metaData.w);
                        float actualCosinus = dot(normalize(light.directionRange.xyz), -normalizedLightVector);
                        float angularAttenuation = (actualCosinus > outerConeCosinus) ? 0.0 : ((actualCosinus < innerConeCosinus) ? ((actualCosinus - outerConeCosinus) / (innerConeCosinus - outerConeCosinus))) : 1.0;
    //                  float angularAttenuation = mix(0.0, mix((actualCosinus - outerConeCosinus) / (innerConeCosinus - outerConeCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
    //                  float angularAttenuation = mix(0.0, mix(smoothstep(outerConeCosinus, innerConeCosinus, actualCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
    #endif
                        lightAttenuation *= angularAttenuation * angularAttenuation;
                        lightDirection = normalizedLightVector;
                        break;
                      }
                    }
                    switch (light.metaData.x) {
                      case 2u:    // Point
                      case 3u: {  // Spot
                        if (light.directionRange.w >= 0.0) {
                          float currentDistance = length(pointToLight);
                          if (currentDistance > 0.0) {
                            lightAttenuation *= 1.0 / (currentDistance * currentDistance);
                            if (light.directionRange.w > 0.0) {
                              float distanceByRange = currentDistance / light.directionRange.w;
                              lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange * distanceByRange * distanceByRange), 0.0, 1.0);
                            }
                          }
                        }
                        break;
                      }
                    }
                    vec3 transmittedLight = lightAttenuation * getPunctualRadianceTransmission(normal.xyz, viewDirection, normalizedLightVector, alphaRoughness, F0, F90, diffuseColorAlpha.xyz, iorValues[i]);
#ifndef VOLUMEATTENUTATION_FORCED
                    if((flags & (1u << 12u)) != 0u)
#endif
                    {
                      transmittedLight = applyVolumeAttenuation(transmittedLight, length(transmissionRay), volumeAttenuationColor, volumeAttenuationDistance);
                    }
                    transmissionOutput[i] += transmittedLight[i];
                  }
                }else{
                  vec3 transmissionRay = getVolumeTransmissionRay(normal.xyz, viewDirection, volumeThickness, ior);
                  vec3 pointToLight = ((light.metaData.x == 0) ? lightDirection : lightVector) - transmissionRay;
                  vec3 normalizedLightVector = normalize(pointToLight);
                  float lightAttenuation = lightAttenuationEx;
                  switch (light.metaData.x) {
                    case 3u: {  // Spot
  #if 1
                      float angularAttenuation = clamp(fma(dot(normalize(light.directionRange.xyz), -normalizedLightVector), uintBitsToFloat(light.metaData.z), uintBitsToFloat(light.metaData.w)), 0.0, 1.0);
  #else
                      // Just for as reference
                      float innerConeCosinus = uintBitsToFloat(light.metaData.z);
                      float outerConeCosinus = uintBitsToFloat(light.metaData.w);
                      float actualCosinus = dot(normalize(light.directionRange.xyz), -normalizedLightVector);
                      float angularAttenuation = (actualCosinus > outerConeCosinus) ? 0.0 : ((actualCosinus < innerConeCosinus) ? ((actualCosinus - outerConeCosinus) / (innerConeCosinus - outerConeCosinus))) : 1.0;
  //                  float angularAttenuation = mix(0.0, mix((actualCosinus - outerConeCosinus) / (innerConeCosinus - outerConeCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
  //                  float angularAttenuation = mix(0.0, mix(smoothstep(outerConeCosinus, innerConeCosinus, actualCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
  #endif
                      lightAttenuation *= angularAttenuation * angularAttenuation;
                      lightDirection = normalizedLightVector;
                      break;
                    }
                  }
                  switch (light.metaData.x) {
                    case 2u:    // Point
                    case 3u: {  // Spot
                      if (light.directionRange.w >= 0.0) {
                        float currentDistance = length(pointToLight);
                        if (currentDistance > 0.0) {
                          lightAttenuation *= 1.0 / (currentDistance * currentDistance);
                          if (light.directionRange.w > 0.0) {
                            float distanceByRange = currentDistance / light.directionRange.w;
                            lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange * distanceByRange * distanceByRange), 0.0, 1.0);
                          }
                        }
                      }
                      break;
                    }
                  }
                  vec3 transmittedLight = lightAttenuation * getPunctualRadianceTransmission(normal.xyz, viewDirection, normalizedLightVector, alphaRoughness, F0, F90, diffuseColorAlpha.xyz, ior);
#ifndef VOLUMEATTENUTATION_FORCED
                  if((flags & (1u << 12u)) != 0u) 
#endif
                  {
                    transmittedLight = applyVolumeAttenuation(transmittedLight, length(transmissionRay), volumeAttenuationColor, volumeAttenuationDistance);
                  }
                  transmissionOutput += transmittedLight;
                }  
              }
#endif // TRANSMISSION
            }
#if defined(REFLECTIVESHADOWMAPOUTPUT)
          }
        }
      }
#elif defined(LIGHTCLUSTERS)
          }
        }
      }
#else
          }
          lightTreeNodeIndex++;
        } else {
          lightTreeNodeIndex += max(1u, lightTreeNode.aabbMinSkipCount.w);
        }
      }
#endif
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
                      sheenColor,                         //
                      sheenRoughness,                     //
                      clearcoatNormal,                    //
                      clearcoatF0,                        //
                      clearcoatRoughness,                 //
                      specularWeight);                    //
      }*/
#elif 0
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
                    sheenColor,                         //
                    sheenRoughness,                     //
                    clearcoatNormal,                    //
                    clearcoatF0,                        //
                    clearcoatRoughness,                 //
                    specularWeight);                    //
#endif

#endif