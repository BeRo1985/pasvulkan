#ifndef GLOBAL_ILLUMINATION_VOXEL_CONE_TRACING_GLSL
#define GLOBAL_ILLUMINATION_VOXEL_CONE_TRACING_GLSL

/*
layout (set = 1, binding = 8, std140) readonly uniform VoxelGridData {
  #include "voxelgriddata_uniforms.glsl"
} voxelGridData;

layout(set = 1, binding = 9) uniform sampler3D uVoxelGridOcclusion[];

layout(set = 1, binding = 10) uniform sampler3D uVoxelGridRadiance[];
*/

const float CVCT_INDIRECT_DIST_K = 0.01;

// Converts the PBR roughness to a voxel cone tracing aperture angle  
#define CVCT_ROUGHNESSTOVOXELCONETRACINGAPERTUREANGLE_METHOD 0
float cvctRoughnessToVoxelConeTracingApertureAngle(float roughness){
  roughness = clamp(roughness, 0.0, 1.0);
#if ROUGHNESSTOVOXELCONETRACINGAPERTUREANGLE_METHOD == 0
	return tan(0.0003474660443456835 + (roughness * (1.3331290497744692 - (roughness * 0.5040552688878546))));
#elif ROUGHNESSTOVOXELCONETRACINGAPERTUREANGLE_METHOD == 1
	return tan(acos(pow(0.244, 1.0 / (clamp(2.0 / max(1e-4, (roughness * roughness)) - 2.0, 4.0, 1024.0 * 16.0) + 1.0))));
#else
  return clamp(tan((PI * (0.5 * 0.75)) * max(0.0, roughness)), 0.00174533102, 3.14159265359);
#endif  
}              

// Calculate the direction weights for a given direction
#define CVCT_GETDIRECTIONWEIGHTS_METHOD 1
vec3 cvctGetDirectionWeights(vec3 direction){
#if CVCT_GETDIRECTIONWEIGHTS_METHOD == 0
  vec3 d = abs(normalize(direction));
  return d / dot(d, vec3(1.0));
#elif CVCT_GETDIRECTIONWEIGHTS_METHOD == 1
  return abs(direction);
#else
  return direction * direction;
#endif
}

///////////////////////////////////////////////////////////////////////////////////////////

// Occlusion is isotropic, so we only need to sample one direction

// Fetch a voxel from the voxel grid
float cvctFetchVoxelOcclusion(const in ivec3 position, const in vec3 direction, const in int mipMapLevel, const in int clipMapIndex){
  return texelFetch(uVoxelGridOcclusion[clipMapIndex], position, mipMapLevel).x;
}  

// Fetch a voxel from the voxel grid per trilinear interpolation
float cvctGetTrilinearInterpolatedVoxelOcclusion(const in vec3 position, const in vec3 direction, const in float mipMapLevel, const in int clipMapIndex){
  return textureLod(uVoxelGridOcclusion[clipMapIndex], position, mipMapLevel).x;
}

///////////////////////////////////////////////////////////////////////////////////////////

// Radiance is anisotropic, so we need to sample 3 directions

// Fetch a voxel from the voxel grid
vec4 cvctFetchVoxelRadiance(const in ivec3 position, const in vec3 direction, const in int mipMapLevel, const in int clipMapIndex){
  bvec3 negativeDirection = lessThan(direction, vec3(0.0));
  vec3 directionWeights = getDirectionWeights(direction);
  ivec3 textureIndices = ivec3(negativeDirection.x ? 1 : 0, negativeDirection.y ? 3 : 2, negativeDirection.z ? 5 : 4) + ivec3(clipMapIndex * 6);
  return (texelFetch(uVoxelGridRadiance[textureIndices.x], position, mipMapLevel) * directionWeights.x) +
         (texelFetch(uVoxelGridRadiance[textureIndices.y], position, mipMapLevel) * directionWeights.y) +
         (texelFetch(uVoxelGridRadiance[textureIndices.z], position, mipMapLevel) * directionWeights.z);
}        

// Fetch a voxel from the voxel grid per trilinear interpolation
vec4 cvctGetTrilinearInterpolatedVoxelRadiance(const in vec3 position, const in vec3 direction, const in float mipMapLevel, const in int clipMapIndex){
  bvec3 negativeDirection = lessThan(direction, vec3(0.0));
  vec3 directionWeights = getDirectionWeights(direction);
  ivec3 textureIndices = ivec3(negativeDirection.x ? 1 : 0, negativeDirection.y ? 3 : 2, negativeDirection.z ? 5 : 4) + ivec3(clipMapIndex * 6);
  return (textureLod(uVoxelGridRadiance[textureIndices.x], position, mipMapLevel) * directionWeights.x) +
         (textureLod(uVoxelGridRadiance[textureIndices.y], position, mipMapLevel) * directionWeights.y) +
         (textureLod(uVoxelGridRadiance[textureIndices.z], position, mipMapLevel) * directionWeights.z);
}        

///////////////////////////////////////////////////////////////////////////////////////////

// Generate jitter noise for a given position
vec4 cvctVoxelJitterNoise(vec4 p4){
	p4 = fract(p4 * vec4(443.897, 441.423, 437.195, 444.129));
  p4 += dot(p4, p4.wzxy + vec4(19.19));
  return fract((p4.xxyz + p4.yzzw) * p4.zywx);
}               

// Trace a radiance cone for a given position and direction, and return the accumulated occlusion
vec4 cvctTraceRadianceCone(vec3 from, 
                           vec3 direction,
                           float aperture,
                           float offset,
                           float maxDistance){
  
  // Load into local variable to avoid multiple memory accesses            
  vec4 clipMaps[4] = voxelGridData.clipMaps; 
  float gridSize = float(voxelGridData.gridSize);
  float halfOverGridSize = 0.5 / gridSize;
  float oneOverGridSize = 1.0 / gridSize;
  vec4 clipMapToWorldScaleFactors = voxelGridData.cellSizes * gridSize;  
  vec4 worldToClipMapScaleFactors = vec4(1.0) / clipMapToWorldScaleFactors;
  uint countClipMaps = voxelGridData.countClipMaps;

  // Calculate the doubled aperture angle for the cone
  float doubledAperture = max(1.0 / gridSize, vec4(2.0 * aperture));

  // Set the starting distance
  float dist = offset;
  
  // Initialize the accumulator to zero, since we start at the beginning of the cone
  vec4 accumulator = vec4(0.0);                       

  direction = normalize(direction);                    

  // Setup the texture indices and direction weights
  bvec3 negativeDirection = lessThan(direction, vec3(0.0));  
  vec3 directionWeights = getDirectionWeights(direction);
  ivec3 textureIndices = ivec3(negativeDirection.x ? 1 : 0, negativeDirection.y ? 3 : 2, negativeDirection.z ? 5 : 4) + ivec3(clipMapIndex * 6);
  
  //maxDistance = min(maxDistance, 1.41421356237);
  //dist += cvctVoxelJitterNoise(vec4(from.xyz + to.xyz + normal.xyz, tc.x)).x * s;

  // Initialize the starting position
  vec3 position = from + (direction * dist);

  // Find the starting clipmap index 
  uint clipMapIndex = 0u;
  bool foundClipMap = false;
  vec3 currentClipMapAAABMin = vec3(uintBitsToFloat(0x7f800000u)); // +inf
  vec3 currentClipMapAAABMax = vec3(uintBitsToFloat(0xff800000u)); // -inf
  for(uint clipMapIndexCounter = 0u; clipMapIndexCounter < countClipMaps; clipMapIndexCounter++){
    if(all(greaterThanEqual(position, voxelGridData.clipMapAABBMin[clipMapIndexCounter].xyz)) && 
       all(lessThanEqual(position, voxelGridData.clipMapAABBMax[clipMapIndexCounter].xyz))){
      clipMapIndex = clipMapIndexCounter;
      currentClipMapAAABMin = voxelGridData.clipMapAABBMin[clipMapIndex].xyz;
      currentClipMapAAABMax = voxelGridData.clipMapAABBMax[clipMapIndex].xyz;
      foundClipMap = true;
      break;
    }
  }

  // If we found a clipmap, we can start tracing the cone, otherwise we are done
  if(foundClipMap){

    // The actual tracing loop
    while((accumulator < 1.0) && (dist < maxDistance)){
      
      // Check if we are still in the current clipmap
      if(any(lessThan(position, currentClipMapAAABMin)) || any(greaterThan(position, currentClipMapAAABMax))){

        // If not, find the next clipmap
        bool foundClipMap = false;
        for(uint clipMapIndexCounter = clipMapIndex + 1; clipMapIndexCounter < countClipMaps; clipMapIndexCounter++){
          if(all(greaterThanEqual(position, voxelGridData.clipMapAABBMin[clipMapIndexCounter].xyz)) && 
             all(lessThanEqual(position, voxelGridData.clipMapAABBMax[clipMapIndexCounter].xyz))){
            clipMapIndex = clipMapIndexCounter;
            currentClipMapAAABMin = voxelGridData.clipMapAABBMin[clipMapIndex].xyz;
            currentClipMapAAABMax = voxelGridData.clipMapAABBMax[clipMapIndex].xyz;
            foundClipMap = true;
            break;
          }
        }

        // If we didn't find a clipmap anymore, we are done and can break out of the loop
        if(!foundClipMap){
          break;
        }

      }else{

        // If we are still in the current clipmap, we can calculate the diameter of the cone at the current position, and 
        // do nothing in this else branch (dummy else branch, just for this comment, the compiler should optimize this away,
        // hopefully)

      }

      // Calculate the diameter of the cone at the current position 
      float diameter = max( halfOverGridSize, doubledAperture * (dist * worldToClipMapScaleFactors[clipMapIndex]));

      // Calculate the mip map level to use for the current position
      float mipMapLevel = max(0.0, log2((diameter * gridSize) + 1.0));   

      // Calculate the texture position
      vec3 texturePosition = (position - clipMaps[clipMapIndex].xyz) / clipMaps[clipMapIndex].w;

      // Accumulate the occlusion from the ansitropic radiance texture, where the ansitropic occlusion is stored in the alpha channel
      accumulator += (1.0 - accumulator) * ((textureLod(uVoxelGridRadiance[textureIndices.x], texturePosition, mipMapLevel) * directionWeights.x) +
                                            (textureLod(uVoxelGridRadiance[textureIndices.y], texturePosition, mipMapLevel) * directionWeights.y) +
                                            (textureLod(uVoxelGridRadiance[textureIndices.z], texturePosition, mipMapLevel) * directionWeights.z));

      // Move the position forward
      dist += max(diameter, oneOverGridSize) * clipMapToWorldScaleFactors[clipMapIndex];

      // Get the new position
      position = from + (direction * dist);

    } 

  } 

  // Return the accumulated occlusion
  return clamp(1.0 - accumulator, 0.0, 1.0);

}	

// Trace a cone for a given position and direction, and return the accumulated occlusion
float cvctTraceShadowCone(vec3 normal, 
                          vec3 from, 
                          vec3 to){
  
  const float aperture = tan(radians(5.0));

  const float s = 1.0 / 4.0;

  // Load into local variable to avoid multiple memory accesses            
  vec4 clipMaps[4] = voxelGridData.clipMaps; 
  float gridSize = float(voxelGridData.gridSize);
  float halfOverGridSize = 0.5 / gridSize;
  float oneOverGridSize = 1.0 / gridSize;
  vec4 clipMapToWorldScaleFactors = voxelGridData.cellSizes * gridSize;  
  vec4 worldToClipMapScaleFactors = vec4(1.0) / clipMapToWorldScaleFactors;
  uint countClipMaps = voxelGridData.countClipMaps;

  // Calculate the doubled aperture angle for the cone
  float doubledAperture = max(1.0 / gridSize, vec4(2.0 * aperture));
  
  from += normal * (2.0  * oneOverGridSize * clipMapToWorldScaleFactors[0]);
  
  vec3 direction = to - from;
  
  // Calculate the maximum distance we can travel
  float maxDistance = length(direction);

  // Set the starting distance
  float dist = 2.5 * oneOverGridSize * clipMapToWorldScaleFactors[0];
  
  // Initialize the accumulator to zero, since we start at the beginning of the cone
  float accumulator = 0.0;                       

  direction = normalize(direction);                    

  // Setup the texture indices and direction weights
  bvec3 negativeDirection = lessThan(direction, vec3(0.0));  
  vec3 directionWeights = getDirectionWeights(direction);
  ivec3 textureIndices = ivec3(negativeDirection.x ? 1 : 0, negativeDirection.y ? 3 : 2, negativeDirection.z ? 5 : 4) + ivec3(clipMapIndex * 6);
  
  //maxDistance = min(maxDistance, 1.41421356237);
  dist += cvctVoxelJitterNoise(vec4(from.xyz + to.xyz + normal.xyz, tc.x)).x * s;

  // Initialize the starting position
  vec3 position = from + (direction * dist);

  // Find the starting clipmap index 
  uint clipMapIndex = 0u;
  bool foundClipMap = false;
  vec3 currentClipMapAAABMin = vec3(uintBitsToFloat(0x7f800000u)); // +inf
  vec3 currentClipMapAAABMax = vec3(uintBitsToFloat(0xff800000u)); // -inf
  for(uint clipMapIndexCounter = 0u; clipMapIndexCounter < countClipMaps; clipMapIndexCounter++){
    if(all(greaterThanEqual(position, voxelGridData.clipMapAABBMin[clipMapIndexCounter])) && 
       all(lessThanEqual(position, voxelGridData.clipMapAABBMax[clipMapIndexCounter]))){
      clipMapIndex = clipMapIndexCounter;
      currentClipMapAAABMin = voxelGridData.clipMapAABBMin[clipMapIndex];
      currentClipMapAAABMax = voxelGridData.clipMapAABBMax[clipMapIndex];
      foundClipMap = true;
      break;
    }
  }

  // If we found a clipmap, we can start tracing the cone, otherwise we are done
  if(foundClipMap){
  
    // The actual tracing loop
    while((accumulator < 1.0) && (dist < maxDistance)){
      
      // Check if we are still in the current clipmap
      if(any(lessThan(position, currentClipMapAAABMin)) || any(greaterThan(position, currentClipMapAAABMax))){

        // If not, find the next clipmap
        bool foundClipMap = false;
        for(uint clipMapIndexCounter = clipMapIndex + 1; clipMapIndexCounter < countClipMaps; clipMapIndexCounter++){
          if(all(greaterThanEqual(position, voxelGridData.clipMapAABBMin[clipMapIndexCounter])) && 
             all(lessThanEqual(position, voxelGridData.clipMapAABBMax[clipMapIndexCounter]))){
            clipMapIndex = clipMapIndexCounter;
            currentClipMapAAABMin = voxelGridData.clipMapAABBMin[clipMapIndex];
            currentClipMapAAABMax = voxelGridData.clipMapAABBMax[clipMapIndex];
            foundClipMap = true;
            break;
          }
        }

        // If we didn't find a clipmap anymore, we are done and can break out of the loop
        if(!foundClipMap){
          break;
        }

      }else{

        // If we are still in the current clipmap, we can calculate the diameter of the cone at the current position, and 
        // do nothing in this else branch (dummy else branch, just for this comment, the compiler should optimize this away,
        // hopefully)

      }

      // Calculate the diameter of the cone at the current position 
      float diameter = max( halfOverGridSize, doubledAperture * (dist * worldToClipMapScaleFactors[clipMapIndex]));

      // Calculate the mip map level to use for the current position
      float mipMapLevel = max(0.0, log2((diameter * gridSize) + 1.0));   

      // Calculate the texture position
      vec3 texturePosition = (position - clipMaps[clipMapIndex].xyz) / clipMaps[clipMapIndex].w;

      // Accumulate the occlusion from the ansitropic radiance texture, where the ansitropic occlusion is stored in the alpha channel
      accumulator += (1.0 - accumulator) * ((textureLod(uVoxelGridRadiance[textureIndices.x], texturePosition, mipMapLevel).w * directionWeights.x) +
                                            (textureLod(uVoxelGridRadiance[textureIndices.y], texturePosition, mipMapLevel).w * directionWeights.y) +
                                            (textureLod(uVoxelGridRadiance[textureIndices.z], texturePosition, mipMapLevel).w * directionWeights.z));

      // Move the position forward
      dist += max(diameter, oneOverGridSize) * clipMapToWorldScaleFactors[clipMapIndex] * s;

      // Get the new position
      position = from + (direction * dist);

    } 

  }

  // Return the accumulated occlusion
  return clamp(1.0 - accumulator, 0.0, 1.0);

}	

// Trace a cone for a given position and direction, and return the accumulated occlusion
float cvctTraceOcclusionCone(vec3 from, 
                             vec3 direction,
                             float aperture,
                             float offset,
                             float maxDistance){
    
  // Load into local variable to avoid multiple memory accesses            
  vec4 clipMaps[4] = voxelGridData.clipMaps; 
  float gridSize = float(voxelGridData.gridSize);
  float halfOverGridSize = 0.5 / gridSize;
  float oneOverGridSize = 1.0 / gridSize;
  vec4 clipMapToWorldScaleFactors = voxelGridData.cellSizes * gridSize;  
  vec4 worldToClipMapScaleFactors = vec4(1.0) / clipMapToWorldScaleFactors;
  uint countClipMaps = voxelGridData.countClipMaps;

  // Calculate the doubled aperture angle for the cone
  float doubledAperture = max(1.0 / gridSize, vec4(2.0 * aperture));
  
  from += normal * (2.0  * oneOverGridSize * clipMapToWorldScaleFactors[0]);
  
  // Set the starting distance
  float dist = offset;
  
  // Initialize the accumulator to zero, since we start at the beginning of the cone
  float accumulator = 0.0;                       

  direction = normalize(direction);                    

  //maxDistance = min(maxDistance, 1.41421356237);
  dist += cvctVoxelJitterNoise(vec4(from.xyz + to.xyz + normal.xyz, tc.x)).x * s;

  // Initialize the starting position
  vec3 position = from + (direction * dist);

  // Find the starting clipmap index 
  uint clipMapIndex = 0u;
  bool foundClipMap = false;
  vec3 currentClipMapAAABMin = vec3(uintBitsToFloat(0x7f800000u)); // +inf
  vec3 currentClipMapAAABMax = vec3(uintBitsToFloat(0xff800000u)); // -inf
  for(uint clipMapIndexCounter = 0u; clipMapIndexCounter < countClipMaps; clipMapIndexCounter++){
    if(all(greaterThanEqual(position, voxelGridData.clipMapAABBMin[clipMapIndexCounter])) && 
       all(lessThanEqual(position, voxelGridData.clipMapAABBMax[clipMapIndexCounter]))){
      clipMapIndex = clipMapIndexCounter;
      currentClipMapAAABMin = voxelGridData.clipMapAABBMin[clipMapIndex];
      currentClipMapAAABMax = voxelGridData.clipMapAABBMax[clipMapIndex];
      foundClipMap = true;
      break;
    }
  }

  // If we found a clipmap, we can start tracing the cone, otherwise we are done
  if(foundClipMap){
  
    // The actual tracing loop
    while((accumulator < 1.0) && (dist < maxDistance)){
      
      // Check if we are still in the current clipmap
      if(any(lessThan(position, currentClipMapAAABMin)) || any(greaterThan(position, currentClipMapAAABMax))){

        // If not, find the next clipmap
        bool foundClipMap = false;
        for(uint clipMapIndexCounter = clipMapIndex + 1; clipMapIndexCounter < countClipMaps; clipMapIndexCounter++){
          if(all(greaterThanEqual(position, voxelGridData.clipMapAABBMin[clipMapIndexCounter])) && 
             all(lessThanEqual(position, voxelGridData.clipMapAABBMax[clipMapIndexCounter]))){
            clipMapIndex = clipMapIndexCounter;
            currentClipMapAAABMin = voxelGridData.clipMapAABBMin[clipMapIndex];
            currentClipMapAAABMax = voxelGridData.clipMapAABBMax[clipMapIndex];
            foundClipMap = true;
            break;
          }
        }

        // If we didn't find a clipmap anymore, we are done and can break out of the loop
        if(!foundClipMap){
          break;
        }

      }else{

        // If we are still in the current clipmap, we can calculate the diameter of the cone at the current position, and 
        // do nothing in this else branch (dummy else branch, just for this comment, the compiler should optimize this away,
        // hopefully)

      }

      // Calculate the diameter of the cone at the current position 
      float diameter = max( halfOverGridSize, doubledAperture * (dist * worldToClipMapScaleFactors[clipMapIndex]));

      // Calculate the mip map level to use for the current position
      float mipMapLevel = max(0.0, log2((diameter * gridSize) + 1.0));   

      // Calculate the texture position
      vec3 texturePosition = (position - clipMaps[clipMapIndex].xyz) / clipMaps[clipMapIndex].w;

      // Accumulate the occlusion from the isotropic occulsion texture
      accumulator += (1.0 - accumulator) * textureLod(uVoxelGridOcclusion[clipMapIndex], texturePosition, mipMapLevel).x;

      // Move the position forward
      dist += max(diameter, oneOverGridSize) * clipMapToWorldScaleFactors[clipMapIndex];

      // Get the new position
      position = from + (direction * dist);

    } 

  }

  // Return the accumulated occlusion
  return clamp(1.0 - accumulator, 0.0, 1.0);

}	

// Create a rotation matrix from an axis and an angle
mat3 cvctRotationMatrix(vec3 axis, float angle){
  axis = normalize(axis);
  vec2 sc = sin(vec2(angle) + vec2(0.0, 1.57079632679)); // sin and cos
  float oc = 1.0 - sc.y;    
  vec3 as = axis * sc.x;
  return (mat3(axis.x * axis,
               axis.y * axis,
               axis.z * axis) * oc) + 
          mat3(sc.y, -as.z,
               as.y, as.z,
               sc.y, -as.x,
               -as.y, 
               as.x,
               sc.y);                
}                     

// Calculate the sky light occlusion for a starting position and a direction
vec3 cvctSkyLightOcclusion(vec3 from, 
                           vec3 normal){
#ifndef NUM_SKY_CONES 
 #define NUM_SKY_CONES 5
#endif
//$define indirectDiffuseLightJitter
#if NUM_SKY_CONES == 9
	const float angleMix = 0.5, 
              coneOffset = -0.01,
              aperture = tan(radians(22.5)),
              offset = 4.0 * voxelGridData.cellSizes[0],
              maxDistance = 2.0 * voxelGridData.cellSizes[voxelGridData.countClipMaps - 1] * float(voxelGridData.gridSize);
	vec3 u = normalize(normal),
#if 0
       v = cross(vec3(0.0, 1.0, 0.0), u),
       w = cross(vec3(0.0, 0.0, 1.0), u),
       ortho = normalize((length(v) < length(w)) ? w : v),
#else
       v = normalize(vec3(0.99146, 0.11664, 0.05832)),
       ortho = normalize((abs(dot(u, v)) > 0.99999) ? cross(vec3(0.0, 1.0, 0.0), u) : cross(v, u)),
#endif
       ortho2 = normalize(cross(ortho, normal)),
       corner = 0.5 * (ortho + ortho2), 
       corner2 = 0.5 * (ortho - ortho2);
  vec3 normalOffset = normal * (1.0 + (4.0 * 0.70710678118)) * voxelGridData.cellSizes[0], 
       coneOrigin = from + normalOffset;       
  return ((cvctTraceOcclusionCone(coneOrigin + (coneOffset * normal), normal, aperture, offset, maxDistance) * 1.0) +
           ((cvctTraceOcclusionCone(coneOrigin + (coneOffset * ortho), mix(normal, ortho, angleMix), aperture, offset, maxDistance) +
             cvctTraceOcclusionCone(coneOrigin - (coneOffset * ortho), mix(normal, -ortho, angleMix), aperture, offset, maxDistance) +
             cvctTraceOcclusionCone(coneOrigin + (coneOffset * ortho2), mix(normal, ortho2, angleMix), aperture, offset, maxDistance) +
             cvctTraceOcclusionCone(coneOrigin - (coneOffset * ortho2), mix(normal, -ortho2, angleMix), aperture, offset, maxDistance)) * 1.0) +
           ((cvctTraceOcclusionCone(coneOrigin + (coneOffset * corner), mix(normal, corner, angleMix), aperture, offset, maxDistance) +
             cvctTraceOcclusionCone(coneOrigin - (coneOffset * corner), mix(normal, -corner, angleMix), aperture, offset, maxDistance) +
             cvctTraceOcclusionCone(coneOrigin + (coneOffset * corner2), mix(normal, corner2, angleMix), aperture, offset, maxDistance) +
             cvctTraceOcclusionCone(coneOrigin - (coneOffset * corner2), mix(normal, -corner2, angleMix), aperture, offset, maxDistance)) * 1.0)) / 9.0;
#else
#if NUM_SKY_CONES == 1
	const vec3 coneDirections[1] = vec3[1](
                                   vec3(0.0, 0.0, 1.0)
                                 );  
	const float coneWeights[1] = float[1](
                                 1.0
                               );  
	const float coneApertures[1] = float[1]( // tan(63.4349488)
                                   2.0
                                 );  
#elif NUM_SKY_CONES == 5
	const vec3 coneDirections[5] = vec3[5](
                                   vec3(0.0, 0.0, 1.0),
                                   vec3(0.0, 0.707106781, 0.707106781),
                                   vec3(0.0, -0.707106781, 0.707106781),
                                   vec3(0.707106781, 0.0, 0.707106781),
                                   vec3(-0.707106781, 0.0, 0.707106781)
                                 );  
	const float coneWeights[5] = float[5](
                                 0.28, 
                                 0.18, 
                                 0.18, 
                                 0.18, 
                                 0.18
                               );  
	const float coneApertures[5] = float[5]( // tan(45)
                                   1.0, 
                                   1.0, 
                                   1.0, 
                                   1.0, 
                                   1.0 
                                 );  
#elif NUM_SKY_CONES == 6
#if 0
	const vec3 coneDirections[6] = vec3[6](
                                   normalize(vec3(0.0, 0.0, 1.0)),
                                   normalize(vec3(-0.794654, 0.607062, 0.000000)),
                                   normalize(vec3(0.642889, 0.607062, 0.467086)), 
                                   normalize(vec3(0.642889, 0.607062, -0.467086)),
                                   normalize(vec3(-0.245562, 0.607062, 0.755761)),
                                   normalize(vec3(-0.245562, 0.607062, -0.755761))
                                 );  
	const float coneWeights[6] = float[6](
                                 1.0, 
                                 0.607,
                                 0.607, 
                                 0.607, 
                                 0.607, 
                                 0.607
                               );  
	const float coneApertures[6] = float[6]( 
                                   0.5, 
                                   0.549092, 
                                   0.549092, 
                                   0.549092, 
                                   0.549092, 
                                   0.549092
                                 );  
#else
	const vec3 coneDirections[6] = vec3[6](
                                   vec3(0.0, 0.0, 1.0),
                                   vec3(0.0, 0.866025, 0.5),
                                   vec3(0.823639, 0.267617, 0.5),
                                   vec3(0.509037, -0.700629, 0.5),
                                   vec3(-0.509037, -0.700629, 0.5),
                                   vec3(-0.823639, 0.267617, 0.5)
                                 );  
	const float coneWeights[6] = float[6](
#if 0
                                 3.14159 * 0.25, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0
#else
                                 0.25, 
                                 0.15,
                                 0.15, 
                                 0.15, 
                                 0.15, 
                                 0.15
#endif
                               );  
	const float coneApertures[6] = float[6]( // tan(30)
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919 
                                 );  
#endif
#elif NUM_SKY_CONES == 8
	const vec3 coneDirections[8] = vec3[8](
                                    vec3(0.57735, 0.57735, 0.57735),      
                                    vec3(-0.57735, -0.57735, 0.57735),     
                                    vec3(-0.903007, 0.182696, 0.388844),    
                                    vec3(0.903007, -0.182696, 0.388844),     
                                    vec3(0.388844, -0.903007, 0.182696),      
                                    vec3(-0.388844, 0.903007, 0.182696),       
                                    vec3(-0.182696, 0.388844, 0.903007),        
                                    vec3(0.182696, -0.388844, 0.903007)          
                                  );  
	const float coneWeights[8] = float[8](
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0
                               );  
	const float coneApertures[8] = float[8]( 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325
                                 );  
#elif NUM_SKY_CONES == 16
	const vec3 coneDirections[16] = vec3[16](
                                    vec3(0.898904, 0.435512, 0.0479745),
                                    vec3(0.898904, -0.0479745, 0.435512),
                                    vec3(-0.898904, -0.435512, 0.0479745),
                                    vec3(-0.898904, 0.0479745, 0.435512),
                                    vec3(0.0479745, 0.898904, 0.435512),
                                    vec3(-0.435512, 0.898904, 0.0479745),
                                    vec3(-0.0479745, -0.898904, 0.435512),
                                    vec3(0.435512, -0.898904, 0.0479745),
                                    vec3(0.435512, 0.0479745, 0.898904),
                                    vec3(-0.435512, -0.0479745, 0.898904),
                                    vec3(0.0479745, -0.435512, 0.898904),
                                    vec3(-0.0479745, 0.435512, 0.898904),
                                    vec3(0.57735, 0.57735, 0.57735),
                                    vec3(0.57735, -0.57735, 0.57735),
                                    vec3(-0.57735, 0.57735, 0.57735),
                                    vec3(-0.57735, -0.57735, 0.57735)
                                  );  
	const float coneWeights[16] = float[16](
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0
                                );  
	const float coneApertures[16] = float[16]( 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595
                                 );  
#endif
  const float coneOffset = -0.01,
              offset = 4.0 * voxelGridData.cellSizes[0],
              maxDistance = 2.0 * voxelGridData.cellSizes[voxelGridData.countClipMaps - 1] * float(voxelGridData.gridSize);
  normal = normalize(normal);
  vec3 normalOffset = normal * (1.0 + (4.0 * 0.70710678118)) * voxelGridData.cellSizes[0], 
       coneOrigin = from + normalOffset,
       t0 = cross(vec3(0.0, 1.0, 0.0), normal),
       t1 = cross(vec3(0.0, 0.0, 1.0), normal),
       tangent = normalize((length(t0) < length(t1)) ? t1 : t0),
       bitangent = normalize(cross(tangent, normal));
  tangent = safeNormalize(cross(bitangent, normal));      
  mat3 tangentSpace =
#ifdef CVCT_SKY_LIGHT_OCCLUSION_JITTER
                      cvctRotationMatrix(normal, cvctVoxelJitterNoise(vec4(from + normal + (vec3(gl_FragCoord.xyz) * 1.0), fract(tc.x * 0.01) * 100.0)).x) *
#endif
                      mat3(tangent, 
                           bitangent, 
                           normal);
  vec2 occlusion = vec2(0.0);  
  [[unroll]] for(int i = 0; i < NUM_SKY_CONES; i++){
    vec3 direction = tangentSpace * coneDirections[i].xyz;
/*  if(dot(direction, tangentSpace[2]) >= 0.0)*/{
      occlusion += vec2(cvctTraceOcclusionCone(coneOrigin + (coneOffset * direction), 
                                          direction, 
                                          coneApertures[i], 
                                          offset, 
                                          maxDistance),
                     1.0) * 
                coneWeights[i];
    }
  }
  return occlusion.x / max(occlusion.y, 1e-6);
#endif
}

// Calculate the radiance for a starting position and a direction
vec3 cvctIndirectDiffuseLight(vec3 from, 
                              vec3 normal){
#ifndef NUM_CONES 
 #define NUM_CONES 5
#endif
//$define indirectDiffuseLightJitter
#if NUM_CONES == 9
	const float angleMix = 0.5, 
              coneOffset = -0.01,
              aperture = tan(radians(22.5)),
              offset = 4.0 * voxelGridData.cellSizes[0],
              maxDistance = 2.0 * voxelGridData.cellSizes[voxelGridData.countClipMaps - 1] * float(voxelGridData.gridSize);
	vec3 u = normalize(normal),
#if 0
       v = cross(vec3(0.0, 1.0, 0.0), u),
       w = cross(vec3(0.0, 0.0, 1.0), u),
       ortho = normalize((length(v) < length(w)) ? w : v),
#else
       v = normalize(vec3(0.99146, 0.11664, 0.05832)),
       ortho = normalize((abs(dot(u, v)) > 0.99999) ? cross(vec3(0.0, 1.0, 0.0), u) : cross(v, u)),
#endif
       ortho2 = normalize(cross(ortho, normal)),
       corner = 0.5 * (ortho + ortho2), 
       corner2 = 0.5 * (ortho - ortho2);
  vec3 normalOffset = normal * (1.0 + (4.0 * 0.70710678118)) * voxelGridData.cellSizes[0], 
       coneOrigin = from + normalOffset;       
  return ((cvctTraceRadianceCone(coneOrigin + (coneOffset * normal), normal, aperture, offset, maxDistance).xyz * 1.0) +
           ((cvctTraceRadianceCone(coneOrigin + (coneOffset * ortho), mix(normal, ortho, angleMix), aperture, offset, maxDistance).xyz +
             cvctTraceRadianceCone(coneOrigin - (coneOffset * ortho), mix(normal, -ortho, angleMix), aperture, offset, maxDistance).xyz +
             cvctTraceRadianceCone(coneOrigin + (coneOffset * ortho2), mix(normal, ortho2, angleMix), aperture, offset, maxDistance).xyz +
             cvctTraceRadianceCone(coneOrigin - (coneOffset * ortho2), mix(normal, -ortho2, angleMix), aperture, offset, maxDistance).xyz) * 1.0) +
           ((cvctTraceRadianceCone(coneOrigin + (coneOffset * corner), mix(normal, corner, angleMix), aperture, offset, maxDistance).xyz +
             cvctTraceRadianceCone(coneOrigin - (coneOffset * corner), mix(normal, -corner, angleMix), aperture, offset, maxDistance).xyz +
             cvctTraceRadianceCone(coneOrigin + (coneOffset * corner2), mix(normal, corner2, angleMix), aperture, offset, maxDistance).xyz +
             cvctTraceRadianceCone(coneOrigin - (coneOffset * corner2), mix(normal, -corner2, angleMix), aperture, offset, maxDistance).xyz) * 1.0)) / 9.0;
#else
#if NUM_CONES == 1
	const vec3 coneDirections[1] = vec3[1](
                                   vec3(0.0, 0.0, 1.0)
                                 );  
	const float coneWeights[1] = float[1](
                                 1.0
                               );  
	const float coneApertures[1] = float[1]( // tan(63.4349488)
                                   2.0
                                 );  
#elif NUM_CONES == 5
	const vec3 coneDirections[5] = vec3[5](
                                   vec3(0.0, 0.0, 1.0),
                                   vec3(0.0, 0.707106781, 0.707106781),
                                   vec3(0.0, -0.707106781, 0.707106781),
                                   vec3(0.707106781, 0.0, 0.707106781),
                                   vec3(-0.707106781, 0.0, 0.707106781)
                                 );  
	const float coneWeights[5] = float[5](
                                 0.28, 
                                 0.18, 
                                 0.18, 
                                 0.18, 
                                 0.18
                               );  
	const float coneApertures[5] = float[5]( // tan(45)
                                   1.0, 
                                   1.0, 
                                   1.0, 
                                   1.0, 
                                   1.0 
                                 );  
#elif NUM_CONES == 6
#if 0
	const vec3 coneDirections[6] = vec3[6](
                                   normalize(vec3(0.0, 0.0, 1.0)),
                                   normalize(vec3(-0.794654, 0.607062, 0.000000)),
                                   normalize(vec3(0.642889, 0.607062, 0.467086)), 
                                   normalize(vec3(0.642889, 0.607062, -0.467086)),
                                   normalize(vec3(-0.245562, 0.607062, 0.755761)),
                                   normalize(vec3(-0.245562, 0.607062, -0.755761))
                                 );  
	const float coneWeights[6] = float[6](
                                 1.0, 
                                 0.607,
                                 0.607, 
                                 0.607, 
                                 0.607, 
                                 0.607
                               );  
	const float coneApertures[6] = float[6]( 
                                   0.5, 
                                   0.549092, 
                                   0.549092, 
                                   0.549092, 
                                   0.549092, 
                                   0.549092
                                 );  
#else
	const vec3 coneDirections[6] = vec3[6](
                                   vec3(0.0, 0.0, 1.0),
                                   vec3(0.0, 0.866025, 0.5),
                                   vec3(0.823639, 0.267617, 0.5),
                                   vec3(0.509037, -0.700629, 0.5),
                                   vec3(-0.509037, -0.700629, 0.5),
                                   vec3(-0.823639, 0.267617, 0.5)
                                 );  
	const float coneWeights[6] = float[6](
#if 0
                                 3.14159 * 0.25, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0, 
                                 (3.14159 * 3.0) / 20.0
#else
                                 0.25, 
                                 0.15,
                                 0.15, 
                                 0.15, 
                                 0.15, 
                                 0.15
#endif
                               );  
	const float coneApertures[6] = float[6]( // tan(30)
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919, 
                                   0.57735026919 
                                 );  
#endif
#elif NUM_CONES == 8
	const vec3 coneDirections[8] = vec3[8](
                                    vec3(0.57735, 0.57735, 0.57735),      
                                    vec3(-0.57735, -0.57735, 0.57735),     
                                    vec3(-0.903007, 0.182696, 0.388844),    
                                    vec3(0.903007, -0.182696, 0.388844),     
                                    vec3(0.388844, -0.903007, 0.182696),      
                                    vec3(-0.388844, 0.903007, 0.182696),       
                                    vec3(-0.182696, 0.388844, 0.903007),        
                                    vec3(0.182696, -0.388844, 0.903007)          
                                  );  
	const float coneWeights[8] = float[8](
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0, 
                                 1.0 / 8.0
                               );  
	const float coneApertures[8] = float[8]( 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325, 
                                   0.4363325
                                 );  
#elif NUM_CONES == 16
	const vec3 coneDirections[16] = vec3[16](
                                    vec3(0.898904, 0.435512, 0.0479745),
                                    vec3(0.898904, -0.0479745, 0.435512),
                                    vec3(-0.898904, -0.435512, 0.0479745),
                                    vec3(-0.898904, 0.0479745, 0.435512),
                                    vec3(0.0479745, 0.898904, 0.435512),
                                    vec3(-0.435512, 0.898904, 0.0479745),
                                    vec3(-0.0479745, -0.898904, 0.435512),
                                    vec3(0.435512, -0.898904, 0.0479745),
                                    vec3(0.435512, 0.0479745, 0.898904),
                                    vec3(-0.435512, -0.0479745, 0.898904),
                                    vec3(0.0479745, -0.435512, 0.898904),
                                    vec3(-0.0479745, 0.435512, 0.898904),
                                    vec3(0.57735, 0.57735, 0.57735),
                                    vec3(0.57735, -0.57735, 0.57735),
                                    vec3(-0.57735, 0.57735, 0.57735),
                                    vec3(-0.57735, -0.57735, 0.57735)
                                  );  
	const float coneWeights[16] = float[16](
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0, 
                                  1.0 / 16.0
                                );  
	const float coneApertures[16] = float[16]( 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595, 
                                    0.3141595
                                 );  
#endif
  const float coneOffset = -0.01,
              offset = 4.0 * voxelGridData.cellSizes[0],
              maxDistance = 2.0 * voxelGridData.cellSizes[voxelGridData.countClipMaps - 1] * float(voxelGridData.gridSize);
  normal = normalize(normal);
  vec3 normalOffset = normal * (1.0 + (4.0 * 0.70710678118)) * voxelGridData.cellSizes[0], 
       coneOrigin = from + normalOffset,
       t0 = cross(vec3(0.0, 1.0, 0.0), normal),
       t1 = cross(vec3(0.0, 0.0, 1.0), normal),
       tangent = normalize((length(t0) < length(t1)) ? t1 : t0),
       bitangent = normalize(cross(tangent, normal));
  tangent = safeNormalize(cross(bitangent, normal));      
  mat3 tangentSpace =
#ifdef CVCT_INDIRECT_DIRECT_LIGHT_JITTER
                      cvctRotationMatrix(normal, cvctVoxelJitterNoise(vec4(from + normal + (vec3(gl_FragCoord.xyz) * 1.0), fract(tc.x * 0.01) * 100.0)).x) *
#endif
                      mat3(tangent, 
                           bitangent, 
                           normal);
  vec4 color = vec4(0.0);
  [[unroll]] for(int i = 0; i < NUM_CONES; i++){
    vec3 direction = tangentSpace * coneDirections[i].xyz;
/*  if(dot(direction, tangentSpace[2]) >= 0.0)*/{
      color += vec4(cvctTraceRadianceCone(coneOrigin + (coneOffset * direction), 
                                          direction, 
                                          coneApertures[i], 
                                          offset, 
                                          maxDistance).xyz,
                     1.0) * 
                coneWeights[i];
    }
  }
  return color.xyz / max(color.w, 1e-6);
#endif
}

// Calculate the specular light for a starting position, a normal, the view direction, the aperture angle and the maximal distance
vec3 cvctIndirectSpecularLight(vec3 from, 
                               vec3 normal, 
                               vec3 viewDirection,
                               float aperture, 
                               float maxDistance){
  normal = normalize(normal);
  return cvctTraceRadianceCone(from + (normal * 2.0 * voxelGridData.cellSizes[0]), 
                               normalize(reflect(normalize(viewDirection), normal)), 
                               aperture, 
                               2.0 * voxelGridData.cellSizes[0], 
                               maxDistance).xyz;
}

// Calculate the refractive light for a starting position, a normal, the view direction, the aperture angle, the index of refraction and the maximal distance
vec3 cvctIndirectRefractiveLight(vec3 from, 
                                 vec3 normal, 
                                 vec3 viewDirection, 
                                 float aperture, 
                                 float indexOfRefraction, 
                                 float maxDistance){
  normal = normalize(normal);
	return cvctTraceRadianceCone(from + (normal * voxelGridData.cellSizes[0]), 
                               normalize(refract(normalize(viewDirection), normal, 1.0 / indexOfRefraction)), 
                               aperture, 
                               voxelGridData.cellSizes[0], 
                               maxDistance).xyz;
}                                   

#endif // GLOBAL_ILLUMINATION_VOXEL_CONE_TRACING_GLSL