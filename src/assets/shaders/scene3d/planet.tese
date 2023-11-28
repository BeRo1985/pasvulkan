#version 450 core

#pragma shader_stage(tesseval)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(quads, equal_spacing, ccw) in;

layout(location = 0) in InBlock {
  vec3 position;
  vec3 tangent;
  vec3 bitangent;
  vec3 normal;
  vec3 uvw;   
} inBlocks[];

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 tangent;
  vec3 bitangent;
  vec3 normal;
  vec3 uvw;   
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
                      gl_TessCoord.y),
  
       tangent = normalize(mix(mix(inBlocks[0].tangent, inBlocks[1].tangent, gl_TessCoord.x), 
                               mix(inBlocks[3].tangent, inBlocks[2].tangent, gl_TessCoord.x), 
                               gl_TessCoord.y)),
  
       bitangent = normalize(mix(mix(inBlocks[0].bitangent, inBlocks[1].bitangent, gl_TessCoord.x), 
                                 mix(inBlocks[3].bitangent, inBlocks[2].bitangent, gl_TessCoord.x), 
                                 gl_TessCoord.y)),
  
       normal = normalize(mix(mix(inBlocks[0].normal, inBlocks[1].normal, gl_TessCoord.x), 
                              mix(inBlocks[3].normal, inBlocks[2].normal, gl_TessCoord.x),
                              gl_TessCoord.y)),

       uvw = mix(mix(inBlocks[0].uvw, inBlocks[1].uvw, gl_TessCoord.x), 
                 mix(inBlocks[3].uvw, inBlocks[2].uvw, gl_TessCoord.x), 
                     gl_TessCoord.y);
                     
#if 1

//tangent = cross(bitangent = cross(normal, normalize(tangent - (dot(tangent, normal) * normal))), normal);
/*vec3 bitangent = normalize(cross(vec3(0.0, 1.0, 0.0), normal)),
                tangent = normalize(cross(normal, bitangent));
  tangent = normalize(tangent - (tangent * dot(tangent, normal)));
  bitangent = normalize(bitangent - (bitangent * dot(bitangent, normal)));      */
  
  mat3 tangentSpace = mat3(tangent, bitangent, normal);
 
  vec4 nm = textureCatmullRomOctahedralMap(uTextureHeightMap, normal);
  
  position += tangentSpace[2] * nm.w;

  normal = nm.xyz;

  tangent = cross(bitangent = cross(normal, normalize(tangent - (dot(tangent, normal) * normal))), normal); // recalculate tangent and bitangent
    
#endif

  vec3 worldSpacePosition = position;

  vec4 viewSpacePosition = viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outBlock.position = position;         
  outBlock.tangent = tangent;
  outBlock.bitangent = bitangent;
  outBlock.normal = normal;
  outBlock.uvw = uvw;
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
