#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 outFragColor;

layout (set = 0, binding = 1) uniform samplerCube uTexture;

#include "skybox.glsl"
#include "env_starlight.glsl"

#ifdef SKYBOX_CACHED_REPROJECTION
#ifdef SKYBOX_CACHED_REPROJECTION_RGB9E5
#include "rgb9e5.glsl"
#endif

// Reprojection for cached starlight rendering
// Projects current pixel's world direction into previous frame's screen space
vec4 reprojectStarlight(const vec3 worldDirection, const vec2 currentUV, const ivec2 screenSize) {
  
  // Get previous frame's view matrices (stored at offset countAllViews, same pattern as mesh.vert velocity)
  uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
  View previousView = uView.views[viewIndex + pushConstants.countAllViews];
  
  // Transform world direction to previous frame's clip space
  // Since skybox is at infinity, we only need rotation (translation doesn't matter)
  vec4 previousClip = previousView.projectionMatrix * vec4(mat3(previousView.viewMatrix) * worldDirection, 1.0);
  
  // Perspective divide to get NDC
  vec2 previousNDC = previousClip.xy / previousClip.w;
  
  // Convert to UV [0,1]
  vec2 previousUV = fma(previousNDC, vec2(0.5), vec2(0.5));
  
  // Check if the previous UV is within valid screen bounds
  // Using a small margin to avoid edge artifacts
  const float margin = 0.001;
  bool isValid = all(greaterThanEqual(previousUV, vec2(margin))) && 
                 all(lessThanEqual(previousUV, vec2(1.0 - margin)));
  
  // Also reject if behind camera in previous frame
  isValid = isValid && (previousClip.w > 0.0);
  
  // Stochastic refresh: periodically recompute some pixels to avoid precision drift
  // Using a simple pattern based on frame index and pixel position
  uvec2 pixelCoord = uvec2(currentUV * vec2(screenSize));
  uint refreshPattern = (pixelCoord.x ^ pixelCoord.y ^ pushConstants.frameIndex) & 0x3Fu; // Every 64 frames per pixel
  bool forceRefresh = (refreshPattern == 0u);
  
  if (isValid && !forceRefresh) {
    // Sample from history buffer
    vec3 historyCoord = vec3(previousUV, float(gl_ViewIndex));
    return texture(uHistoryTexture, historyCoord);
  } else {
    // Compute fresh starlight for this pixel
    return vec4(clamp(getStarlight(worldDirection) * pushConstants.skyBoxBrightnessFactor, vec3(-65504.0), vec3(65504.0)), 1.0);
  }
}

#endif

void main(){
  const vec3 direction = normalize((pushConstants.orientation * vec4(normalize(inPosition), 0.0)).xyz); 
  switch(pushConstants.mode){
    case 1u:{
      // Realtime starlight
#ifdef SKYBOX_CACHED_REPROJECTION
      // Cached reprojection mode: reuse previous frame where possible
      ivec2 screenSize = ivec2(pushConstants.widthHeight & 0xFFFFu, pushConstants.widthHeight >> 16u);
      vec2 currentUV = gl_FragCoord.xy / vec2(screenSize);
      outFragColor = reprojectStarlight(direction, currentUV, screenSize);
      // Store result to history image for next frame
#if defined(SKYBOX_CACHED_REPROJECTION_RGB9E5)
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), uvec4(encodeRGB9E5(outFragColor.rgb)));
#elif defined(SKYBOX_CACHED_REPROJECTION_RGBA16F)
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), outFragColor);
#else
#error "SKYBOX_CACHED_REPROJECTION requires either SKYBOX_CACHED_REPROJECTION_RGB9E5 or SKYBOX_CACHED_REPROJECTION_RGBA16F"
#endif
#else
      // Full computation every frame
      outFragColor = vec4(clamp(getStarlight(direction) * pushConstants.skyBoxBrightnessFactor, vec3(-65504.0), vec3(65504.0)), 1.0);
#endif
      break;
    }
    default:{
      // Cube map
      vec4 color = texture(uTexture, direction) * vec2(pushConstants.skyBoxBrightnessFactor, 1.0).xxxy;
      outFragColor = vec4(clamp(color.xyz, vec3(-65504.0), vec3(65504.0)), color.w);
#ifdef SKYBOX_CACHED_REPROJECTION
      // Also store cube map result to history for consistency
#if defined(SKYBOX_CACHED_REPROJECTION_RGB9E5)
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), uvec4(encodeRGB9E5(outFragColor.rgb)));
#elif defined(SKYBOX_CACHED_REPROJECTION_RGBA16F)
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), outFragColor);
#else
#error "SKYBOX_CACHED_REPROJECTION requires either SKYBOX_CACHED_REPROJECTION_RGB9E5 or SKYBOX_CACHED_REPROJECTION_RGBA16F"
#endif
#endif
      break;
    } 
  }
}