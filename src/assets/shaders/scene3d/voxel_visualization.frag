#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_control_flow_attributes : enable
#if defined(USEDEMOTE)
  #extension GL_EXT_demote_to_helper_invocation : enable
#endif

layout(location = 0) in vec3 inRayOrigin;
layout(location = 1) in vec3 inRayDirection;

layout(location = 0) out vec4 outFragColor;

layout (set = 1, binding = 0, std140) readonly uniform VoxelGridData {
  #include "voxelgriddata_uniforms.glsl"
} voxelGridData;

 layout(set = 1, binding = 1) uniform sampler3D uVoxelGridOcclusion[];

 layout(set = 1, binding = 2) uniform sampler3D uVoxelGridRadiance[];

struct Intersection {
   float dist;
   vec4 voxel;
   //vec3 position;
   bool inside;
};

#ifdef DebugRayGrid 
vec4 debugColor = vec4(0.0);
#endif               

bool voxelTrace(in int cascadeIndex,
                in vec3 rayOrigin, 
                in vec3 rayDirection,
                inout Intersection intersection){
  
  bool hit = false; //traceGeometry(rayOrigin, rayDirection, intersection);
  
  uint gridSize = voxelGridData.gridSizes[cascadeIndex >> 2][cascadeIndex & 3];

  const int maxSteps = int(2 * ceil(length(vec3(float(gridSize)))));
 
  const vec3 cascadeMin = voxelGridData.cascadeAABBMin[cascadeIndex].xyz;
  const vec3 cascadeMax = voxelGridData.cascadeAABBMax[cascadeIndex].xyz;

  const float timeScale = voxelGridData.cascadeToWorldScales[cascadeIndex >> 2][cascadeIndex & 3] / float(gridSize); // Assuming that all the cascade grid bound axes are equally-sized
  
  vec3 inversedRayDirection = abs(vec3(length(rayDirection)) / rayDirection) * sign(rayDirection),
       omin = (min(cascadeMin, cascadeMax) - rayOrigin) * inversedRayDirection,
       omax = (max(cascadeMin, cascadeMax) - rayOrigin) * inversedRayDirection,
       vmin = min(omin, omax),
       vmax = max(omin, omax),
       r = vec3(max(vmin.x, max(vmin.y, vmin.z)),
                min(vmax.x, min(vmax.y, vmax.z)),
                0.0);
  
  if((r.x <= r.y) && (r.y >= 0.0) && (r.x <= intersection.dist)){
               
    intersection.inside = (r.x < 0.0) ? true : false;

    r.x = max(0.0, r.x);
    
    intersection.dist = clamp(intersection.dist, r.x, r.y);
    
    rayOrigin = (voxelGridData.worldToCascadeGridMatrices[cascadeIndex] * vec4(rayOrigin + (rayDirection * r.x), 1.0)).xyz;

    ivec3 position = ivec3(floor(clamp(rayOrigin, vec3(0.0), vec3(float(gridSize) - 1.0)))),
          positionStep = ivec3(sign(inversedRayDirection));
          
    vec3 sideDistanceStep = inversedRayDirection * positionStep,
         sideDistance = ((position - rayOrigin) + vec3(0.5) + (positionStep * 0.5)) * inversedRayDirection;   
        
    for(int stepIndex = 0; stepIndex < maxSteps; stepIndex++){      
         
      vec3 times = (((position - rayOrigin) + vec3(0.5)) - (positionStep * 0.5)) * inversedRayDirection;
                   
      float time = max(times.x, max(times.y, times.z));
      
      uvec3 positionUnsigned = uvec3(position);
      
      if((((time * timeScale) + r.x) > intersection.dist) ||
         any(greaterThanEqual(positionUnsigned, uvec3(gridSize)))){
        break;
      }

      vec4 voxel = vec4(0.0);
      {
        int mipMapLevel = 0;
        ivec3 position = ivec3(positionUnsigned) >> mipMapLevel;
        bvec3 negativeDirection = lessThan(rayDirection, vec3(0.0));
        vec3 directionWeights = abs(rayDirection);
        ivec3 textureIndices = ivec3(negativeDirection.x ? 1 : 0, negativeDirection.y ? 3 : 2, negativeDirection.z ? 5 : 4) + ivec3(cascadeIndex * 6);
        voxel = (texelFetch(uVoxelGridRadiance[textureIndices.x], position, mipMapLevel) * directionWeights.x) +
                (texelFetch(uVoxelGridRadiance[textureIndices.y], position, mipMapLevel) * directionWeights.y) +
                (texelFetch(uVoxelGridRadiance[textureIndices.z], position, mipMapLevel) * directionWeights.z);
      }

      if(length(voxel) > 0.0){
        vec3 worldPosition = (voxelGridData.cascadeGridToWorldMatrices[cascadeIndex] * vec4(rayOrigin + (((position - rayOrigin) + vec3(0.5)) - (positionStep * 0.5)), 1.0)).xyz;
        if(!((cascadeIndex > 0) && 
             ((all(greaterThanEqual(worldPosition, voxelGridData.cascadeAABBMin[cascadeIndex - 1].xyz)) && 
               all(lessThanEqual(worldPosition, voxelGridData.cascadeAABBMax[cascadeIndex - 1].xyz)))))){
#ifdef DebugRayGrid 
          debugColor = max(debugColor, vec4(0.0, 0.0, 0.5, 0.9));
#endif
          intersection.dist = (time * timeScale) + r.x;
          intersection.voxel = voxel / voxel.w;
          //intersection.position = (voxelGridData.cascadeGridToWorldMatrices[cascadeIndex] * vec4(rayOrigin + (((position - rayOrigin) + vec3(0.5)) - (positionStep * 0.5)), 1.0)).xyz;
          hit = true;
          break;
        }
      }
         
      ivec3 mask = ivec3(lessThanEqual(sideDistance.xyz, min(sideDistance.yzx, sideDistance.zxy)));
		  sideDistance += sideDistanceStep * mask;
      position += positionStep * mask; 
              
    }
   
#ifdef DebugRayGrid 
    debugColor = max(debugColor, vec4(1.0, 1.0, 1.0, 0.9));
#endif
   
  }
  
	return hit;    
}


void main(){

  vec3 rayOrigin = inRayOrigin;

  // This normalization is necessary because the vertex shader outputs cubic coordinates which are not normalized. By normalizing the ray direction, 
  // the cubic to spherical conversion is compatible with any projection matrix, independent of the near and far planes and their Z-directions 
  // (e.g. 0 to 1, -1 to 1, 1 to -1 or even 1 to 0) and of perspective or orthographic projection.
  vec3 rayDirection = normalize(inRayDirection);

  const float infinity = uintBitsToFloat(0x7f800000u); // +infinity

  bool hasBestIntersection = false;

  Intersection bestIntersection = { infinity, vec4(0.0)/*, vec3(0.0)*/, false };

  for(uint cascadeIndex = 0u; cascadeIndex < voxelGridData.countCascades; cascadeIndex++){
    Intersection intersection = { infinity, vec4(0.0)/*, vec3(0.0)*/, false };
    if(voxelTrace(int(cascadeIndex), rayOrigin, rayDirection, intersection)){
      if((!hasBestIntersection) || (intersection.dist < bestIntersection.dist)){
        hasBestIntersection = true;
        bestIntersection = intersection;
        if(intersection.inside){ 
          break;
        }
      }
    }
  }

  if(hasBestIntersection){
    outFragColor = bestIntersection.voxel;
  }else{
#if defined(USEDEMOTE)
    demote;
#else
    discard;  
#endif
  } 
  
}