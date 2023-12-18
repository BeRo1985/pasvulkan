#version 450 core

#pragma shader_stage(tesseval)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

#ifdef TRIANGLES
layout(triangles, fractional_even_spacing, ccw) in;
#else
layout(quads, fractional_even_spacing, ccw) in; 
#endif

layout(location = 0) in InBlock {
  vec3 position;
  vec3 normal;
} inBlocks[];

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 sphereNormal;
  vec3 normal;
  vec3 edge;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
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

  mat4 modelMatrix;
  
  uint viewBaseIndex;
  uint countViews;
  uint countQuadPointsInOneDirection; 
  uint countAllViews;
  
  float bottomRadius;
  float topRadius;
  float resolutionX;  
  float resolutionY;  
  
  float heightMapScale;
  float tessellationFactor;
  vec2 jitter;

  vec4 selected; // xy = octahedral map coordinates, z = radius, w = unused

} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

// Global descriptor set

layout(set = 0, binding = 0, std140) uniform uboViews {
  View views[256];
} uView;

// Per planet descriptor set

layout(set = 1, binding = 0) uniform sampler2D uTextures[]; // 0 = height map, 1 = normal map, 2 = tangent bitangent map

#include "octahedral.glsl"
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

#ifdef TRIANGLES
 
  // Barycentric coordinates

  //vec3 position = (inBlocks[0].position * gl_TessCoord.x) + (inBlocks[1].position * gl_TessCoord.y) + (inBlocks[2].position * gl_TessCoord.z);

  vec3 sphereNormal = normalize((inBlocks[0].normal * gl_TessCoord.x) + (inBlocks[1].normal * gl_TessCoord.y) + (inBlocks[2].normal * gl_TessCoord.z));

#else

  // Bilinear interpolation

  /*
  vec3 position = mix(mix(inBlocks[0].position, inBlocks[1].position, gl_TessCoord.x),
                      mix(inBlocks[2].position, inBlocks[3].position, gl_TessCoord.x), 
                      gl_TessCoord.y);*/
  
  vec3 sphereNormal = normalize(mix(mix(inBlocks[0].normal, inBlocks[1].normal, gl_TessCoord.x), 
                                    mix(inBlocks[3].normal, inBlocks[2].normal, gl_TessCoord.x),
                               gl_TessCoord.y));
#endif
 
  //position += sphereNormal * textureCatmullRomPlanetOctahedralMap(uTextures[0], sphereNormal).x * pushConstants.heightMapScale;
 
  vec3 position = (pushConstants.modelMatrix * vec4(sphereNormal * (pushConstants.bottomRadius + (textureCatmullRomPlanetOctahedralMap(uTextures[0], sphereNormal).x * pushConstants.heightMapScale)), 1.0)).xyz;

  vec3 worldSpacePosition = position;

  vec4 viewSpacePosition = viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  vec3 normal = textureCatmullRomPlanetOctahedralMap(uTextures[1], sphereNormal).xyz;

  outBlock.position = position;   
  outBlock.sphereNormal = sphereNormal;      
  outBlock.normal = normalize(transpose(inverse(mat3(pushConstants.modelMatrix))) * normal);
  outBlock.edge = vec3(1.0);
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
