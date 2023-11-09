#version 450 core

//#define SHADERDEBUG

#ifndef VOXELIZATION
#extension GL_EXT_multiview : enable
#endif
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#if defined(SHADERDEBUG) && !defined(VELOCITY)
#extension GL_EXT_debug_printf : enable
#endif

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec4 inNormalSign;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec3 inModelScale;
layout(location = 4) in vec2 inTexCoord0;
layout(location = 5) in vec2 inTexCoord1;
layout(location = 6) in vec4 inColor0;
layout(location = 7) in uint inMaterialID;
#ifdef VELOCITY
layout(location = 8) in vec3 inPreviousPosition;
layout(location = 9) in uint inGeneration;
layout(location = 10) in uint inPreviousGeneration;
#endif

layout(location = 0) out vec3 outWorldSpacePosition;
layout(location = 1) out vec3 outViewSpacePosition;
layout(location = 2) out vec3 outCameraRelativePosition;
layout(location = 3) out vec3 outTangent;
layout(location = 4) out vec3 outBitangent;
layout(location = 5) out vec3 outNormal;
layout(location = 6) out vec2 outTexCoord0;
layout(location = 7) out vec2 outTexCoord1;
layout(location = 8) out vec4 outColor0;
layout(location = 9) out vec3 outModelScale;
layout(location = 10) flat out uint outMaterialID;
#ifndef VOXELIZATION
layout(location = 11) flat out int outViewIndex;
layout(location = 12) flat out uint outFrameIndex;
#ifdef VELOCITY
layout(location = 13) flat out vec4 outJitter;
layout(location = 14) out vec4 outPreviousClipSpace;
layout(location = 15) out vec4 outCurrentClipSpace;
#else
layout(location = 13) flat out vec2 outJitter;
#endif
#endif

/* clang-format off */

/*#ifdef VOXELIZATION

// Should be the same as in the geometry shader, since the minimum "maximum-size" of push constants is 128 bytes
layout (push_constant) uniform PushConstants {
  uint viewIndex; // for the main primary view (in VR mode just simply the left eye, which will use as the primary view for the lighting for the voxelization then) 
} pushConstants;

#else
*/
layout (push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint frameIndex;
  vec4 jitter;
} pushConstants;

//#endif

// Global descriptor set

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

layout(set = 0, binding = 0, std430) readonly buffer InstanceMatrices {
  mat4 instanceMatrices[]; // pair-wise: 0 = base, 1 = previous (for velocity)
};

out gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
};

/* clang-format on */

void main() {

#ifdef VOXELIZATION
  uint viewIndex = pushConstants.viewBaseIndex;
#else
  uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
#endif
 
  mat3 tangentSpace;
  {
    vec3 tangent = inTangent.xyz;
    vec3 normal = inNormalSign.xyz;
    tangentSpace = mat3(tangent, normalize(cross(normal, tangent)) * inNormalSign.w, normal);
  }

  tangentSpace[0] = normalize(tangentSpace[0]);
  tangentSpace[1] = normalize(tangentSpace[1]);
  tangentSpace[2] = normalize(tangentSpace[2]);

  View view = uView.views[viewIndex];

#if defined(SHADERDEBUG) && !(defined(VELOCITY) || defined(VOXELIZATION))
  if(gl_VertexIndex == 0){
    mat4 m = /*view.projectionMatrix * view.viewMatrix;*/ view.inverseProjectionMatrix;
    debugPrintfEXT("view-index %i matrix: %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f", 
                   viewIndex, 
                   m[0][0], 
                   m[0][1],
                   m[0][2],
                   m[0][3],
                   m[1][0], 
                   m[1][1],
                   m[1][2],
                   m[1][3],
                   m[2][0], 
                   m[2][1],
                   m[2][2],
                   m[2][3],
                   m[3][0], 
                   m[3][1],
                   m[3][2],
                   m[3][3]);
  }
#endif

#if 1
  // The actual standard approach
  vec3 cameraPosition = view.inverseViewMatrix[3].xyz;
#else
  // This approach assumes that the view matrix has no scaling or skewing, but only rotation and translation.
  vec3 cameraPosition = (-view.viewMatrix[3].xyz) * mat3(view.viewMatrix);
#endif

  vec3 modelScale = inModelScale; 
  vec3 position = inPosition;
#ifdef VELOCITY  
  vec3 previousPosition = inPreviousPosition;
#endif

  // gl_InstanceIndex is always 0 for non-instanced rendering, where we don't need to do this anyway then, and skip the transformations 
  // for to save some cycles and memory bandwidth, given the branch is always not taken in the current thread warp on the GPU.
  if(gl_InstanceIndex > 0){  
    // The base mesh data is assumed to be non-pretransformed by its origin. If it is pretransformed by its origin, it will be treated
    // as a delta transformation. It is because the mesh vertices are pretransformed by a compute shader, but this was originally only 
    // for non-instanced meshes. Therefore, the original to-be-instanced mesh data should be non-pretransformed by its origin.
    mat4 instanceMatrix = instanceMatrices[gl_InstanceIndex << 1]; 
    modelScale *= vec3(length(instanceMatrix[0].xyz), length(instanceMatrix[1].xyz), length(instanceMatrix[2].xyz)); // needed for transmissive materials
    position = (instanceMatrix * vec4(position, 1.0)).xyz;
    tangentSpace = transpose(inverse(mat3(instanceMatrix))) * tangentSpace;   
#ifdef VELOCITY  
    previousPosition = (instanceMatrices[(gl_InstanceIndex << 1) | 1]* vec4(previousPosition, 1.0)).xyz;
#endif
  }

  vec3 worldSpacePosition = position;

  vec4 viewSpacePosition = view.viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outWorldSpacePosition = worldSpacePosition;
  outViewSpacePosition = viewSpacePosition.xyz;
  outCameraRelativePosition = worldSpacePosition - cameraPosition;
  outTangent = tangentSpace[0];
  outBitangent = tangentSpace[1];
  outNormal = tangentSpace[2];
  outTexCoord0 = inTexCoord0;
  outTexCoord1 = inTexCoord1;
  outColor0 = inColor0;
  outModelScale = modelScale;
  outMaterialID = inMaterialID;
#ifndef VOXELIZATION
  outViewIndex = int(viewIndex); 
  outFrameIndex = pushConstants.frameIndex;
#endif

#ifdef VOXELIZATION
  gl_Position = vec4(0.0, 0.0, 0.0, 1.0); // Overrided by geometry shader anyway
#elif defined(VELOCITY)

  outCurrentClipSpace = (view.projectionMatrix * view.viewMatrix) * vec4(position, 1.0);

  View previousView = uView.views[viewIndex + pushConstants.countAllViews];
  if(uint(inGeneration) != uint(inPreviousGeneration)){
    outPreviousClipSpace = outCurrentClipSpace;
  }else{  
    outPreviousClipSpace = (previousView.projectionMatrix * previousView.viewMatrix) * vec4(previousPosition, 1.0);
  }

  gl_Position = outCurrentClipSpace;

  outJitter = pushConstants.jitter;

#else
  
  gl_Position = (view.projectionMatrix * view.viewMatrix) * vec4(position, 1.0);
  
  outJitter = pushConstants.jitter.xy;

#endif

  gl_PointSize = 1.0;
}
