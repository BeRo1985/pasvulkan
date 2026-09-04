#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec3 inPosition;
#ifdef SKYBOX_CACHED_REPROJECTION
layout(location = 1) in vec4 inPreviousClipSpacePosition;
#endif

layout(location = 0) out vec4 outFragColor;

layout (set = 0, binding = 1) uniform samplerCube uTexture;

#include "skybox.glsl"
#include "env_starlight.glsl"
#include "sun_disc.glsl"
#include "sky_gradient_sun.glsl"

float skyBoxFactor = pushConstants.skyBoxIntensityFactor * pushConstants.skyBoxBrightnessFactor;

// The sun for the cube map and starlight skies, in absolute luminances - see the sky parameter table in
// skybox.glsl. This is what lets there be a sun with no atmosphere in front of it: out in space, or over a
// world that has not been given any air yet. Where there IS an atmosphere, its pass runs after this one and
// multiplies whatever stands here by its transmittance before adding its own scattered light, so the sun
// reddens and dims on the way down without anybody having to arrange it.
//
// A radiance of zero is the whole of the off switch: the scene sends zeroes when the sun is not the sky
// box's to draw, and then this costs one compare.
vec3 getSkyBoxSun(const in vec3 aDirection){
  vec3 discRadiance = pushConstants.skyParameters0.xyz;
  if(all(lessThanEqual(discRadiance, vec3(0.0)))){
    return vec3(0.0);
  }
  float radius = max(pushConstants.skyParameters0.w, 1e-6);
  vec3 sunDirection = -normalize(pushConstants.lightDirection.xyz);
  float angle = acos(clamp(dot(aDirection, sunDirection), -1.0, 1.0));
  return sunDiscRadiance(angle,
                         radius,
                         pushConstants.skyParameters1.w,
                         pushConstants.skyParameters2.x,
                         discRadiance,
                         pushConstants.skyParameters1.xyz,
                         pushConstants.skyParameters2.y);
}

#ifdef SKYBOX_CACHED_REPROJECTION
#ifdef SKYBOX_CACHED_REPROJECTION_RGB9E5
#include "rgb9e5.glsl"
#endif

#undef SKYBOX_CACHED_REPROJECTION_DEBUG       

#ifdef SKYBOX_CACHED_REPROJECTION_DEBUG       
#define getStarlight getStarlightDebug
vec3 getStarlightDebug(const vec3 worldDirection){
  return vec3(fma(vec3(normalize(worldDirection)), vec3(0.5), vec3(0.5))); // Debug: visualize direction as color
}
#endif

// Reprojection for cached starlight rendering
// Uses previous clip space position from vertex shader (interpolated)
vec4 reprojectStarlight(const vec3 worldDirection, const vec2 currentUV, const ivec2 screenSize) {
  
  // Previous clip position comes from vertex shader, properly interpolated
  vec4 previousClip = inPreviousClipSpacePosition;
  
  // Perspective divide
  vec2 previousNDC = previousClip.xy / previousClip.w;
  
  // Convert to UV [0,1]
  vec2 previousUV = fma(previousNDC, vec2(0.5), vec2(0.5));
  
  // Check if the previous UV is within valid screen bounds
  // Using a 1-pixel margin to avoid edge artifacts
  vec2 margin = 1.0 / vec2(screenSize);
  bool isValid = all(greaterThanEqual(previousUV, margin)) && 
                 all(lessThanEqual(previousUV, vec2(1.0) - margin));
  
  // Also reject if behind camera in previous frame
  // For direction vectors with w=0, clip.w = -viewDir.z, so positive w means forward-facing (viewDir.z < 0)
  isValid = isValid && (previousClip.w > 0.0);

  // Reject if the reprojection moved too far (indicates large rotation or feedback risk)
  vec2 motionVector = abs(previousUV - currentUV);
  bool tooMuchMotion = any(greaterThan(motionVector, vec2(0.15))); // 15% of screen max movement
  isValid = isValid && !tooMuchMotion;
  
  // Stochastic refresh: periodically recompute some pixels to avoid precision drift
  // Using a simple pattern based on frame index and pixel position
  uvec2 pixelCoord = uvec2(currentUV * vec2(screenSize));
  uint refreshPattern = (pixelCoord.x ^ pixelCoord.y ^ pushConstants.frameIndex) & 0x3fu; // Every 64 frames per pixel
  bool forceRefresh = (refreshPattern == 0u);
  
  if(isValid && !forceRefresh){
    // Sample from history buffer
    vec3 historyCoord = vec3(previousUV, float(gl_ViewIndex));
    vec4 historySample = texture(uHistoryTexture, historyCoord);
#ifdef SKYBOX_CACHED_REPROJECTION_RGBA16F
    // For RGBA16F: alpha = 0 means pixel was never written (cleared before draw)
    // This catches hidden/off-screen pixels from previous frame
    if(historySample.a < 0.5){
      // Pixel was not rendered in previous frame, recompute
      return vec4(clamp(getStarlight(worldDirection) * skyBoxFactor, vec3(-65504.0), vec3(65504.0)), 1.0);
    }else{
#ifdef SKYBOX_CACHED_REPROJECTION_DEBUG       
      historySample.xy = vec2(0.0); // debug scaling to visualize history usage 
#endif
      return historySample;
    }
#elif defined(SKYBOX_CACHED_REPROJECTION_RGB9E5)
    // For RGB9E5: all zeros means pixel was never written (cleared before draw)
    // Pure black sky is physically impossible, so this is a safe sentinel
    if(all(equal(historySample.rgb, vec3(0.0)))){
      // Pixel was not rendered in previous frame, recompute
      return vec4(clamp(getStarlight(worldDirection) * skyBoxFactor, vec3(-65504.0), vec3(65504.0)), 1.0);
    }else{
#ifdef SKYBOX_CACHED_REPROJECTION_DEBUG       
      historySample.xy = vec2(0.0); // debug scaling to visualize history usage 
#endif
      return historySample;
    }
#else    
    return historySample;
#endif
  }else{
    // Compute fresh starlight for this pixel (alpha = 1.0 marks as valid)
    return vec4(clamp(getStarlight(worldDirection) * skyBoxFactor, vec3(-65504.0), vec3(65504.0)), 1.0);
  }
}

#endif

void main(){
  const vec3 direction = normalize(inPosition);
  switch(pushConstants.mode & 0xffffu){
    case 1u:{
      // Realtime starlight
#ifdef SKYBOX_CACHED_REPROJECTION
      // Cached reprojection mode: reuse previous frame where possible
      ivec2 screenSize = ivec2(pushConstants.widthHeight & 0xFFFFu, pushConstants.widthHeight >> 16u);
      vec2 currentUV = gl_FragCoord.xy / vec2(screenSize);
      outFragColor = reprojectStarlight(direction, currentUV, screenSize);
      // Store result to history image for next frame
#if defined(SKYBOX_CACHED_REPROJECTION_RGB9E5)
      // Use max() to ensure we never encode to zero (our sentinel for unwritten pixels)
      const vec3 minRGB9E5 = vec3(6.1e-5); // Minimum non-zero RGB9E5 representable value
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), uvec4(encodeRGB9E5(max(outFragColor.rgb, minRGB9E5))));
#elif defined(SKYBOX_CACHED_REPROJECTION_RGBA16F)
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), outFragColor);
#else
#error "SKYBOX_CACHED_REPROJECTION requires either SKYBOX_CACHED_REPROJECTION_RGB9E5 or SKYBOX_CACHED_REPROJECTION_RGBA16F"
#endif
      // And the sun on top, AFTER the history has been written. It must not go into the history and must
      // not come out of it: it is a few pixels across, hard edged and brighter than everything else, so the
      // reprojection's fifteen percent motion reject and its one-in-sixty-four refresh would have it
      // smearing and flickering across the sky at every turn of the camera. The stars cache well because
      // they are fixed to the sky and dim; the sun is neither.
      outFragColor.xyz = clamp(outFragColor.xyz + getSkyBoxSun(direction), vec3(-65504.0), vec3(65504.0));
#else
      // Full computation every frame
      outFragColor = vec4(clamp((getStarlight(direction) * skyBoxFactor) + getSkyBoxSun(direction), vec3(-65504.0), vec3(65504.0)), 1.0);
#endif
      break;
    }
    case 2u:{
      // Transparent color key magenta
      outFragColor = vec4(1.0, 0.0, 1.0, 1.0);
      break;
    }
    case 3u:{
      // Stylized gradient sky: a vertical top/horizon/bottom colour ramp, the sun (see
      // sky_gradient_sun.glsl), and optional cheap hash stars in the upper sky.
      // Named locally, because in this mode the three sky parameters are the palette; see skybox.glsl.
      vec4 gradientTopColor = pushConstants.skyParameters0;
      vec4 gradientHorizonColor = pushConstants.skyParameters1;
      vec4 gradientBottomColor = pushConstants.skyParameters2;
      float upness = direction.y;
      float ramp = pow(clamp(abs(upness), 0.0, 1.0), 0.5);
      vec3 skyColor = (upness >= 0.0)
                        ? mix(gradientHorizonColor.xyz, gradientTopColor.xyz, ramp)
                        : mix(gradientHorizonColor.xyz, gradientBottomColor.xyz, ramp);
      skyColor += skyGradientSun(direction,
                                 -normalize(pushConstants.lightDirection.xyz),
                                 gradientHorizonColor.xyz,
                                 gradientHorizonColor.w,
                                 gradientBottomColor.w,
                                 float(pushConstants.mode >> 16u) / 65535.0);
      float starIntensity = gradientTopColor.w;
      if((starIntensity > 0.0) && (upness > 0.0)){
        vec3 cell = floor(direction * 220.0);
        float hash = fract(sin(dot(cell, vec3(12.9898, 78.233, 37.719))) * 43758.5453);
        float star = step(0.9985, hash) * starIntensity * smoothstep(0.0, 0.25, upness);
        skyColor += vec3(star);
      }
      skyColor *= pushConstants.skyBoxBrightnessFactor;
      outFragColor = vec4(clamp(skyColor, vec3(-65504.0), vec3(65504.0)), 1.0);
      break;
    }
    default:{
      // Cube map
      vec4 color = texture(uTexture, direction) * vec2(pushConstants.skyBoxBrightnessFactor, 1.0).xxxy; // no pre-multiplied skyBoxIntensityFactor here, because it is already baked into the cube map
      outFragColor = vec4(clamp(color.xyz, vec3(-65504.0), vec3(65504.0)), color.w);
#ifdef SKYBOX_CACHED_REPROJECTION
      // Also store cube map result to history for consistency
#if defined(SKYBOX_CACHED_REPROJECTION_RGB9E5)
      // Use max() to ensure we never encode to zero (our sentinel for unwritten pixels)
      const vec3 minRGB9E5 = vec3(6.1e-5); // Minimum non-zero RGB9E5 representable value
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), uvec4(encodeRGB9E5(max(outFragColor.rgb, minRGB9E5))));
#elif defined(SKYBOX_CACHED_REPROJECTION_RGBA16F)
      imageStore(uHistoryImage, ivec3(gl_FragCoord.xy, gl_ViewIndex), outFragColor);
#else
#error "SKYBOX_CACHED_REPROJECTION requires either SKYBOX_CACHED_REPROJECTION_RGB9E5 or SKYBOX_CACHED_REPROJECTION_RGBA16F"
#endif
#endif
      // The sun on top, and deliberately NOT scaled by skyBoxBrightnessFactor above: that one is there to
      // dim a star field down to where a night sky belongs, and the sun's luminance is an absolute figure
      // that has no business being divided by a hundred and twenty-eight along with the stars.
      outFragColor.xyz = clamp(outFragColor.xyz + getSkyBoxSun(direction), vec3(-65504.0), vec3(65504.0));
      break;
    }
  }
}