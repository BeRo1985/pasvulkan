#version 450 core 

#pragma shader_stage(geometry)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(triangles) in; 
layout(triangle_strip) out; 
layout(max_vertices = 3) out; 

layout(location = 0) in InBlock { 
  vec3 position;
  vec3 sphereNormal;
  vec3 tangent;
  vec3 bitangent;
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
} inBlocks[]; 

layout(location = 0) out OutBlock { 
  vec3 position;
  vec3 sphereNormal;
  vec3 tangent;
  vec3 bitangent;
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

void main(){ 
  for (int i = 0; i < 3; i++) { 
    gl_Position = gl_in[i].gl_Position; 
    outBlock.position = inBlocks[i].position;
    outBlock.sphereNormal = inBlocks[i].sphereNormal;
    outBlock.tangent = inBlocks[i].tangent;
    outBlock.bitangent = inBlocks[i].bitangent;
    outBlock.normal = inBlocks[i].normal;
    outBlock.edge = vec3((i == 0) ? 1.0 : 0.0, (i == 1) ? 1.0 : 0.0, (i == 2) ? 1.0 : 0.0);
    outBlock.worldSpacePosition = inBlocks[i].worldSpacePosition;
    outBlock.viewSpacePosition = inBlocks[i].viewSpacePosition;
    outBlock.cameraRelativePosition = inBlocks[i].cameraRelativePosition;
    outBlock.jitter = inBlocks[i].jitter;
#ifdef VELOCITY
    outBlock.previousClipSpace = inBlocks[i].previousClipSpace;
    outBlock.currentClipSpace = inBlocks[i].currentClipSpace;
#endif
    EmitVertex(); 
  } 
  EndPrimitive(); 
} 