#ifndef VOXELIZATION_FRAGMENT_GLSL
#define VOXELIZATION_FRAGMENT_GLSL

#ifdef VOXELIZATION
  vec4 clipMap = voxelGridData.clipMaps[inClipMapIndex];

  uint voxelGridSize = voxelGridData.gridSize;
  
  uvec3 volumePosition = uvec3(inVoxelPosition * float(voxelGridSize)); 

// uvec3 volumePosition = uvec3(ivec3((inWorldSpacePosition - vec3(clipMap.xyz)) / float(voxelGridData.cellSizes[inClipMapIndex]))) + uvec3(voxelGridSize >> 1u); 

  if(all(greaterThanEqual(volumePosition, ivec3(0))) && all(lessThan(volumePosition, ivec3(voxelGridSize)))){

#if defined(META_VOXELIZATION)

    uint volumeBaseIndex = 
      (
        (
          (
            (
              (uint(inClipMapIndex) * voxelGridSize) + uint(volumePosition.z)
            ) * voxelGridSize
          ) + uint(volumePosition.y)
        ) * voxelGridSize
      ) + uint(volumePosition.x);

    uint volumeIndex = volumeBaseIndex << 1u;

    if(atomicAdd(voxelGridContentMetaData.data[volumeIndex + 2], 1u) < voxelGridData.maxLocalFragmentCount){

      uint volumeCellIndex = atomicAdd(voxelGridContentMetaData.data[0], 1u);

      if(volumeCellIndex < voxelGridData.maxGlobalFragmentCount){

#if 1
        uvec4 data[2] = uvec4[2](
          uvec4(
            atomicExchange(voxelGridContentMetaData.data[volumeIndex + 3], volumeCellIndex + 1u), // next cell index (1-based, because 0 is the end of the list)
            packHalf2x16(vec2(baseColor.xy)), // base color red and green as 16 bit floats
            packHalf2x16(vec2(baseColor.zw)), // base color blue and alpha as 16 bit floats
            packHalf2x16(vec2(emissionColor.xy)) // emission color red and green as 16 bit floats
          ),
          uvec4(
            packHalf2x16(vec2(emissionColor.z, normal.x)), // emission color blue and normal x as 16 bit floats
            packHalf2x16(vec2(normal.yz)), // normal y and z as 16 bit floats
            0u, // unused
            0u  // unused
          )
        );
#else
        uvec4 data[2] = uvec4[2](
          uvec4(
            atomicExchange(voxelGridContentMetaData.data[volumeIndex + 3], volumeCellIndex + 1u), // next cell index (1-based, because 0 is the end of the list)
            encodeRGB9E5(vec2(baseColor.xyz)), // base color as RGB9E5
            encodeRGB9E5(vec2(emissionColor.xyz)), // emission color as RGB9E5
            packHalf2x16(vec2(baseColor.w, normal.x)) // base color alpha and normal x as 16 bit floats
          ),
          uvec4(
            packHalf2x16(vec2(normal.yz)), // normal y and z as 16 bit floats
            0u, // unused
            0u, // unused
            0u  // unused
          )
        );
#endif

        voxelGridContentData.data[(volumeCellIndex << 1u) | 0u] = data[0];
        voxelGridContentData.data[(volumeCellIndex << 1u) | 1u] = data[1];
    
      }

    }       

#elif defined(OCCLUSION_VOXELIZATION)

    uint volumeIndex = (((((uint(inClipMapIndex) * voxelGridSize) + uint(volumePosition.z)) * voxelGridSize) + uint(volumePosition.y)) * voxelGridSize) + uint(volumePosition.x);

    // 24.8 bit fixed point
    atomicAdd(voxelGridColors.data[volumeIndex], uint(alpha * 256.0));
  
    atomicAdd(voxelGridCounters.data[volumeIndex], 1u); 

    outFragColor = vec4(alpha);

#else
/*
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

    vec4 anisotropicPremultipliedColor = vec4(finalColor.xyz, 1.0) * finalColor.w;

    outFragColor = vec4(finalColor);

    for(uint anisotropicAxisDirectionSideIndex = 0u; anisotropicAxisDirectionSideIndex < countAnisotropicAxisDirectionSides; anisotropicAxisDirectionSideIndex++){

      uvec3 anisotropicDirectionOffsets = anisotropicAxisDirectionSideOffsets[anisotropicAxisDirectionSideIndex];

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

    }*/   

#endif

  }else{
    outFragColor = vec4(0.0);
  }  
 
#endif

#endif