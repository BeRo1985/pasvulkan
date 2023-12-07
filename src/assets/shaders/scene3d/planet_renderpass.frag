#version 450 core

#pragma shader_stage(fragment)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

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
} inBlock;

layout(location = 0) out vec4 outFragColor;
#ifdef VELOCITY
layout(location = 1) out vec2 outVelocity;
#endif

// Per planet descriptor set

//layout(set = 1, binding = 0) uniform sampler2D u

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
  float tessellationFactor; // = factor / referenceMinEdgeSize, for to avoid at least one division in the shader 
  vec2 jitter;

  vec4 selected; // xyz = octahedral map coordinates, w = radius

} pushConstants;

float edgeFactor(){
   vec3 a = smoothstep(vec3(0.0), (abs(dFdx(inBlock.edge)) + abs(dFdy(inBlock.edge))) * 1.414, inBlock.edge);
   return min(min(a.x, a.y), a.z);
}          

void main(){

  vec4 c = vec4(vec3(1.0) * edgeFactor() * fma(clamp(dot(inBlock.normal, vec3(0.0, 1.0, 0.0)), -1.0, 1.0), 0.5, 0.5), 1.0);

  if(pushConstants.selected.w > 1e-6){
    float d = length(normalize(inBlock.sphereNormal.xyz) - normalize(pushConstants.selected.xyz)) - pushConstants.selected.w;
    float t = fwidth(d) * 1.41421356237;
    c.xyz = mix(c.xyz, mix(vec3(1.0) - clamp(c.zxy, vec3(1.0), vec3(1.0)), vec3(1.0, 0.0, 0.0), 0.5), smoothstep(t, -t, d) * 0.5);
  }

  outFragColor = c;

#ifdef VELOCITY
  outVelocity = (inBlock.currentClipSpace.xy / inBlock.currentClipSpace.w) - (inBlock.previousClipSpace.xy / inBlock.previousClipSpace.w);
#endif

}