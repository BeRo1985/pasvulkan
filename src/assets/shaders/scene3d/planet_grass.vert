#version 450 core

// This vertex shader is a part of the mesh shader emulation for the grass rendering, when mesh shaders could not be used for
// some reason, for example, because the hardware does not support them, or because the Vulkan implementation does not support
// multi-view rendering with mesh shaders, or when for ray tracing all vertex data are needed at once upfront anyway and not 
// in a streaming fashion as mesh shaders would provide. 

#pragma shader_stage(vertex)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_control_flow_attributes : enable

#include "bufferreference_definitions.glsl"

layout(location = 0) in vec4 inPositionXYZTexCoordU;
layout(location = 1) in vec4 inNormalXYZTexCoordV;

#if defined(RAYTRACING)

layout(location = 0) out vec3 outWorldSpacePosition;

layout(location = 1) out OutBlock {
  vec3 position;
  vec3 normal;
  vec2 texCoord;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif  
} outBlock;

#else

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 normal;
  vec2 texCoord;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif  
} outBlock;
#endif

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256];
} uView;

// Global descriptor set

#define PLANETS
#include "globaldescriptorset.glsl"
#undef PLANETS

#include "planet_grass.glsl"

#include "octahedralmap.glsl"
#include "tangentspacebasis.glsl" 

uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
mat4 viewMatrix = uView.views[viewIndex].viewMatrix;
mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;

void main(){          

  mat4 viewProjectionMatrix = projectionMatrix * viewMatrix;

#if 1
  // The actual standard approach
  vec3 cameraPosition = inverseViewMatrix[3].xyz;
#else
  // This approach assumes that the view matrix has no scaling or skewing, but only rotation and translation.
  vec3 cameraPosition = (-viewMatrix[3].xyz) * mat3(viewMatrix);
#endif   

  vec3 position = (pushConstants.modelMatrix * vec4(inPositionXYZTexCoordU.xyz, 1.0)).xyz;

  vec3 worldSpacePosition = position;

  vec3 normal = inNormalXYZTexCoordV.xyz; // octSignedDecode(inOctahedralEncodedNormal);
  
  vec4 viewSpacePosition = viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outBlock.position = position;         
  outBlock.normal = normalize(transpose(inverse(mat3(pushConstants.modelMatrix))) * normal);
  outBlock.texCoord = vec2(inPositionXYZTexCoordU.w, inNormalXYZTexCoordV.w);
  outBlock.worldSpacePosition = worldSpacePosition;
  outBlock.viewSpacePosition = viewSpacePosition.xyz;  
  outBlock.cameraRelativePosition = worldSpacePosition - cameraPosition;
  outBlock.jitter = pushConstants.jitter;
#ifdef VELOCITY
  outBlock.currentClipSpace = viewProjectionMatrix * vec4(position, 1.0);
  outBlock.previousClipSpace = (uView.views[viewIndex + pushConstants.countAllViews].projectionMatrix * uView.views[viewIndex + pushConstants.countAllViews].viewMatrix) * vec4(position, 1.0);
#endif

#if defined(RAYTRACING)
  outWorldSpacePosition = worldSpacePosition;
#endif

  gl_Position = viewProjectionMatrix * vec4(position, 1.0);

}