#version 450 core

#pragma shader_stage(fragment)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in InBlock {
  vec3 position;
  vec3 tangent;
  vec3 bitangent;
  vec3 normal;
  vec2 edge; 
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 currentClipSpace;
#endif  
} inBlock;

layout(location = 0) out vec4 outFragColor;
#ifdef VELOCITY
layout(location = 1) out vec2 outVelocity;
#endif

// Per planet descriptor set

//layout(set = 1, binding = 0) uniform sampler2D u

float edgeFactor(){
   vec2 a = smoothstep(vec2(0.0), (abs(dFdx(inBlock.edge)) + abs(dFdy(inBlock.edge))) * 1.414, inBlock.edge);
   return min(a.x, a.y);
}          

void main(){

  outFragColor = vec4(fma(inBlock.normal, vec3(0.5), vec3(0.5)) * edgeFactor(), 1.0);

#ifdef VELOCITY
  outVelocity = (inBlock.currentClipSpace.xy / inBlock.currentClipSpace.w) - (inBlock.previousClipSpace.xy / inBlock.previousClipSpace.w);
#endif

}