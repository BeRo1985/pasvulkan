#version 450 core

#pragma shader_stage(tesseval)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(quads, equal_spacing, ccw) in;

layout(location = 0) in InBlock {
  vec3 position;
  vec3 normal;
} inBlocks[];

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 tangent;
  vec3 bitangent;
  vec3 normal;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 outCurrentClipSpace;
#endif  
} outBlock;

in gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
	float gl_ClipDistance[];
} gl_in[];

out gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
	float gl_ClipDistance[];
};

layout(push_constant) uniform PushConstants {
  int viewBaseIndex;
  int countViews;
  int countQuadPointsInOneDirection; 
  int countAllViews;
  float bottomRadius;
  float topRadius;
  float resolutionX;  
  float resolutionY;  
  vec2 jitter;
} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 0, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

layout(set = 0, binding = 1) uniform sampler2D uTextureHeightMap; // xyz = normal, w = height

#include "octahedralmap.glsl"
#include "tangentspacebasis.glsl" 

int viewIndex = pushConstants.viewBaseIndex + int(gl_ViewIndex);
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

  vec3 position = mix(mix(inBlocks[0].position, inBlocks[1].position, gl_TessCoord.x),
                      mix(inBlocks[3].position, inBlocks[2].position, gl_TessCoord.x), 
                      gl_TessCoord.y);
  
  vec3 normal = normalize(mix(mix(inBlocks[0].normal, inBlocks[1].normal, gl_TessCoord.x), 
                              mix(inBlocks[3].normal, inBlocks[2].normal, gl_TessCoord.x),
                              gl_TessCoord.y));
 
  vec4 texel = textureCatmullRomOctahedralMap(uTextureHeightMap, normal);
  
  position += normal * texel.w;

  mat3 tbn = getTangentSpaceFromNormal(texel.xyz);

  vec3 worldSpacePosition = position;

  vec4 viewSpacePosition = viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outBlock.position = position;         
  outBlock.tangent = tbn[0]; 
  outBlock.bitangent = tbn[1];
  outBlock.normal = tbn[2]; 
  outBlock.worldSpacePosition = worldSpacePosition;
  outBlock.viewSpacePosition = viewSpacePosition.xyz;  
  outBlock.cameraRelativePosition = worldSpacePosition - cameraPosition;
  outBlock.jitter = pushConstants.jitter;
#ifdef VELOCITY
  outBlock.currentClipSpace = (projectionMatrix * viewMatrix) * vec4(position, 1.0);
  outBlock.previousClipSpace = (uView.views[viewIndex + pushConstants.countAllViews].projectionMatrix * uView.views[viewIndex + pushConstants.countAllViews].viewMatrix) * vec4(position, 1.0);
#endif

	gl_Position = viewProjectionMatrix * vec4(position, 1.0);
  
}
