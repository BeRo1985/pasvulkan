#ifndef VOXELIZATION_FRAGMENT_GLSL
#define VOXELIZATION_FRAGMENT_GLSL

#ifdef VOXELIZATION
  vec4 clipMap = voxelGridData.clipMaps[inClipMapIndex];

  uint voxelGridSize = voxelGridData.gridSize;
  
  uvec3 volumePosition = ivec3((inWorldSpacePosition - float(clipMap.xyz)) / float(clipMap.w)); 

  if(all(greaterThanEqual(volumePosition, ivec3(0))) && all(lessThan(volumePosition, ivec3(voxelGridSize)))){

#ifdef OCCLUSION_VOXELIZATION

    uint volumeIndex = (((((uint(inClipMapIndex) * voxelGridSize) + uint(volumePosition.z)) * voxelGridSize) + uint(volumePosition.y)) * voxelGridSize) + uint(volumePosition.x);

    // 24.8 bit fixed point
    atomicAdd(voxelGridColors.data[volumeIndex], uint(alpha * 256.0));
  
    atomicAdd(voxelGridCounters.data[volumeIndex], 1u); 

#else

    uint volumeBaseIndex = ((((((uint(inClipMapIndex) * voxelGridSize) + uint(volumePosition.z)) * voxelGridSize) + uint(volumePosition.y)) * voxelGridSize) + uint(volumePosition.x)) * 6;

    uint countAnisotropicAxisDirectionSides;

    uvec3 anisotropicAxisDirectionSideOffsets[2];

    if((flags & (1u << 6u)) != 0u){
      countAnisotropicAxisDirectionSides = 2u; // Double-sided
      anisotropicAxisDirectionSideOffsets[0] = uvec3(0u, 1u, 2u);
      anisotropicAxisDirectionSideOffsets[1] = uvec3(3u, 4u, 5u);
    }else{
      countAnisotropicAxisDirectionSides = 1u; // Single-sided
      anisotropicAxisDirectionSideOffsets[0] = uvec3(
        (workNormal.x > 0.0) ? 0u : 3u, 
        (workNormal.y > 0.0) ? 1u : 4u, 
        (workNormal.z > 0.0) ? 2u : 5u
      );
    } 

    vec3 anisotropicDirectionWeights = abs(workNormal);

    for(uint anisotropicAxisDirectionSideIndex = 0u; anisotropicAxisDirectionSideIndex < countAnisotropicAxisDirectionSides; anisotropicAxisDirectionSideIndex++){

      uvec3 anisotropicDirectionOffsets = anisotropicAxisDirectionSideOffsets[anisotropicAxisDirectionSideIndex];

      vec4 anisotropicPremultipliedColor = vec4(finalColor.xyz, 1.0) * finalColor.w;

      [[unroll]]            
      for(uint anisotropicAxisDirectionIndex = 0u; anisotropicAxisDirectionIndex < 3u; anisotropicAxisDirectionIndex++){

        float anisotropicAxisDirectionWeight = anisotropicDirectionWeights[anisotropicAxisDirectionIndex];

        if(anisotropicAxisDirectionWeight > 0.0){
          
          uint volumeIndex = volumeBaseIndex + anisotropicDirectionOffsets[anisotropicAxisDirectionIndex];

          uint volumeColorIndex = volumeIndex << 2u;

          vec4 anisotropicAxisDirectionColor = anisotropicPremultipliedColor * anisotropicAxisDirectionWeight;

  #if defined(USESHADERBUFFERFLOAT32ATOMICADD)
          // 32 bit floating point 
          atomicAdd(voxelGridColors.data[volumeColorIndex | 0u], anisotropicAxisDirectionColor.x);
          atomicAdd(voxelGridColors.data[volumeColorIndex | 1u], anisotropicAxisDirectionColor.y);
          atomicAdd(voxelGridColors.data[volumeColorIndex | 2u], anisotropicAxisDirectionColor.z);
          atomicAdd(voxelGridColors.data[volumeColorIndex | 3u], anisotropicAxisDirectionColor.w);    
  #else
          // 22.12 bit fixed point
          uvec4 anisotropicAxisDirectionColorFixedPoint = uvec4(anisotropicAxisDirectionColor * 4096.0); 
          atomicAdd(voxelGridColors.data[volumeColorIndex | 0u], anisotropicAxisDirectionColorFixedPoint.x);
          atomicAdd(voxelGridColors.data[volumeColorIndex | 1u], anisotropicAxisDirectionColorFixedPoint.y);
          atomicAdd(voxelGridColors.data[volumeColorIndex | 2u], anisotropicAxisDirectionColorFixedPoint.z);
          atomicAdd(voxelGridColors.data[volumeColorIndex | 3u], anisotropicAxisDirectionColorFixedPoint.w);
  #endif

          atomicAdd(voxelGridCounters.data[volumeIndex], 1u); 

        }

      }   

    }

#endif

  }  
 
#endif

#endif