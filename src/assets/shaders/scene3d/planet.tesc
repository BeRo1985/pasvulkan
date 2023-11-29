#version 450 core

#pragma shader_stage(tesscontrol)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(vertices = 4) out;

layout(location = 0) in InBlock {
  vec3 position;
  vec3 normal;
} inBlocks[];

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 normal;
} outBlocks[];

layout(push_constant) uniform PushConstants {
  int viewBaseIndex;
  int countViews;
  int countQuadPointsInOneDirection; 
  int countAllViews;
  float bottomRadius;
  float topRadius;
  float resolutionX;  
  float resolutionY;  
  float heightMapScale;
  float dummy;
  vec2 jitter;
} pushConstants;

vec2 resolution = vec2(pushConstants.resolutionX, pushConstants.resolutionY);

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 0, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

int viewIndex = pushConstants.viewBaseIndex + int(gl_ViewIndex);
mat4 viewMatrix = uView.views[viewIndex].viewMatrix;
mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;
// mat4 inverseProjectionMatrix = uView.views[viewIndex].inverseProjectionMatrix;

float remap(const in float value,
            const in float oldMin,
            const in float oldMax,
            const in float newMin,
            const in float newMax){
  return (((value - oldMin) / (oldMax - oldMin)) * (newMax - newMin)) + newMin; 
}

float AdaptiveTessellation(vec3 p0, vec3 p1){
#ifdef DoShadowMap
  return 64.0;
#else
//return 1.0;
  float distanceToCamera = length(viewMatrix * vec4(mix(p0, p1, 0.5), 1.0)),
        tesselationTriangleSize = mix(mix(128.0, 
                                          1.0, 
                                          pow(1.0 - clamp(remap(distanceToCamera , 128.0, 1024.0, 0.0, 1.0), 0.0, 1.0), 2.0)),
                                      1.0,
                                      1.0 - clamp(remap(distanceToCamera, 0.0, 512.0, 0.0, 1.0), 0.0, 1.0));
// tesselationTriangleSize = -log2(clamp(distanceToCamera * 0.005, 0.0, 1.0));
  vec4 vc = viewMatrix * vec4((p0 + p1) * 0.5, 1.0),
       vr = vec2(length(p1 - p0) * 0.5, 0.0).xyyy,
       v0 = projectionMatrix * (vc - vr),
       v1 = projectionMatrix * (vc + vr),
       v = ((vec4(v0.xy / v0.w, v1.xy / v1.w) * 0.5) + vec4(0.5)) * resolution.xyxy;
 	return clamp(distance(v.xy, v.zw) / tesselationTriangleSize, 1.0, 64.0);
/*  return min(clamp(distance(v.xy, v.zw) / tesselationTriangleSize, 1.0, 64.0),
             mix(1.0, 64.0, pow(1.0 - clamp(remap(length(viewMatrix * vec4(mix(p0, p1, 0.5), 1.0)), 2048.0, 4096.0, 0.0, 1.0), 0.0, 1.0), 8.0)));*/
#endif
}
void main(){	 
  bool visible = true;
#ifndef DoShadowMap
  vec3 aabbMin = min(min(min(inBlocks[0].position, inBlocks[1].position), inBlocks[2].position), inBlocks[3].position),
       aabbMax = max(max(max(inBlocks[0].position, inBlocks[1].position), inBlocks[2].position), inBlocks[3].position);
  vec4 sphere = vec4((aabbMin + aabbMax) * 0.5, length(aabbMax - aabbMin) * 0.5);
  if(distance(sphere.xyz, inverseViewMatrix[3].xyz) < 65536.0){
#if 0
    for(int i = 0; i < 6; i++){
      if((dot(vec4(sphere.xyz, 1.0), frustumPlanes[i]) + sphere.w) < 0.0){
        visible = false;
        break;
      }
    }
#endif
    if(visible){
      vec3 quadNormal = normalize(inBlocks[0].normal + inBlocks[1].normal + inBlocks[2].normal + inBlocks[3].normal),
           planetCenterToCamera = inverseViewMatrix[3].xyz, // - vec3(0.0, -pushConstants.topRadius, 0.0),
           planetCenterToCameraDirection = normalize(planetCenterToCamera);
      float thresholdAngle = mix(10.0, // at ground 
                                 120.0, // in space
                                 pow(clamp((length(planetCenterToCamera) - pushConstants.bottomRadius) / (pushConstants.topRadius - pushConstants.bottomRadius), 0.0, 1.0), 4.0));
      if(dot(quadNormal, planetCenterToCameraDirection) < cos(radians(thresholdAngle) * 0.5)){
        visible = false;
      }    
    }
  }else{   
    visible = false;
  }             
#endif
  if(visible){
	  gl_TessLevelOuter[0] = AdaptiveTessellation(inBlocks[0].position, inBlocks[3].position);
	  gl_TessLevelOuter[1] = AdaptiveTessellation(inBlocks[0].position, inBlocks[1].position);
	  gl_TessLevelOuter[2] = AdaptiveTessellation(inBlocks[1].position, inBlocks[2].position);
	  gl_TessLevelOuter[3] = AdaptiveTessellation(inBlocks[3].position, inBlocks[2].position);
	  gl_TessLevelInner[0] = (gl_TessLevelOuter[1] + gl_TessLevelOuter[3]) * 0.5;
    gl_TessLevelInner[1] = (gl_TessLevelOuter[0] + gl_TessLevelOuter[2]) * 0.5;
  }else{
	  gl_TessLevelOuter[0] = -1.0;
	  gl_TessLevelOuter[1] = -1.0;   
	  gl_TessLevelOuter[2] = -1.0;
	  gl_TessLevelOuter[3] = -1.0;
	  gl_TessLevelInner[0] = -1.0;
    gl_TessLevelInner[1] = -1.0;
  }
  outBlocks[gl_InvocationID].position = inBlocks[gl_InvocationID].position;
	outBlocks[gl_InvocationID].normal = inBlocks[gl_InvocationID].normal;
}   
