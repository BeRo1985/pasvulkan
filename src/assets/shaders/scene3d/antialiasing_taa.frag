#version 450 core

#define ColorSpaceRGB 0
#define ColorSpaceYCoCg 1

#define ColorSpace ColorSpaceYCoCg

#define UseSimple 0

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_control_flow_attributes : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(set = 0, binding = 0) uniform sampler2DArray uCurrentColorTexture;
layout(set = 0, binding = 1) uniform sampler2DArray uCurrentDepthTexture;
layout(set = 0, binding = 2) uniform sampler2DArray uCurrentVelocityTexture;
layout(set = 0, binding = 3) uniform sampler2DArray uHistoryColorTexture;
layout(set = 0, binding = 4) uniform sampler2DArray uHistoryDepthTexture;
layout(set = 0, binding = 5) uniform sampler2DArray uHistoryVelocityTexture;

const uint FLAG_FIRST_FRAME_DISOCCLUSION = 1u << 0u; // First frame disocclusion
const uint FLAG_TRANSLUCENT_DISOCCLUSION = 1u << 1u; // Translucent disocclusion
const uint FLAG_VELOCITY_DISOCCLUSION = 1u << 2u; // Velocity disocclusion
const uint FLAG_DEPTH_DISOCCLUSION = 1u << 3u; // Depth disocclusion
const uint FLAG_INCLUDE_BACKGROUND = 1u << 4u; // Include background in the temporal antialiasing.
const uint FLAG_VARIANCE_CLIPPING = 1u << 5u; // Variance clipping
const uint FLAG_CHROMA_SHRINKING = 1u << 6u; // Chroma shrinking
const uint FLAG_CLIPPING = 1u << 7u; // Clipping
const uint FLAG_LUMINANCE_WEIGHTING = 1u << 8u; // Luminance weighting
const uint FLAG_USE_FALLBACK_FXAA = 1u << 9u; // Use fallback FXAA for disoccluded or otherwise rejected areas.
const uint FLAG_DISABLE_TEMPORAL_ANTIALIASING = 1u << 10u; // For debugging purposes and for showing the raw jittered input without any temporal antialiasing when FLAG_USE_FALLBACK_FXAA is even not set.

layout(push_constant, std140) uniform PushConstants {

  uint baseViewIndex;
  uint countViews;
  uint flags;
  float varianceClipGamma;

  float backgroundFeedbackMin;
  float backgroundFeedbackMax;
  float translucentFeedbackMin;
  float translucentFeedbackMax;

  float opaqueFeedbackMin;
  float opaqueFeedbackMax;
  float ZMul;
  float ZAdd;

  float disocclusionDebugFactor;
  float velocityDisocclusionThreshold;
  float depthDisocclusionRelativeThreshold; // part of the distance itself, so it means the same near and far
  float sharpingFactor;

  float depthDisocclusionSlopeScale; // in texels of the surface's own depth slope
  float depthDisocclusionFloor;      // in reciprocal world units, below which everything is equally far away
  vec2 jitterUV;

} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

mat4 inverseProjectionMatrix = uView.views[pushConstants.baseViewIndex + uint(gl_ViewIndex)].inverseProjectionMatrix;

// Linearize depth
float LinearizeDepth(float depth, vec2 uv){
#if 0
  vec2 v = (inverseProjectionMatrix * vec4(vec3(fma(uv, vec2(2.0), vec2(-1.0)), depth), 1.0)).zw;
#else
  vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(depth), inverseProjectionMatrix[3].zw);
#endif
  return -(v.x / v.y);
}

// Reciprocal of the linearized depth. Exactly 1.0 / LinearizeDepth, but written the other way round so
// that it stays finite where that one does not: with a reversed infinite far plane the far plane sits at a
// linearized depth of infinity, and a test built on differences of those ends up comparing infinities. In
// reciprocal form the far plane is simply zero, for the reversed infinite and the ordinary finite
// projection alike, so a single formulation covers both. It is also the quantity a reversed-Z buffer
// already stores, so its precision is spread evenly over the range instead of piling up near the eye.
float InverseLinearDepth(const in float depth){
  vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(depth), inverseProjectionMatrix[3].zw);
  return -(v.y / v.x);
}

// Reciprocal linearized depth of one texel, unfiltered. Avoiding the filter is the whole point: a
// bilinearly mixed depth across a silhouette is a depth that no surface in the scene actually has, and
// testing against it reports a disocclusion along every silhouette in the picture.
float InverseLinearDepthAt(const in sampler2DArray tex, const in ivec2 coord, const in ivec2 maxCoord, const in int layer){
  return InverseLinearDepth(texelFetch(tex, ivec3(clamp(coord, ivec2(0), maxCoord), layer), 0).x);
}

// Get the luminance of a RGB color
float Luminance(vec4 color){
  return dot(color.xyz, vec3(0.2125, 0.7154, 0.0721));
}

#include "bidirectional_tonemapping.glsl"

// Tone mapping
vec4 Tonemap(vec4 color){
  return ApplyToneMapping(color);
  //return vec4(color.xyz / (Luminance(color) + 1.0), color.w);
}

// Inverse tone mapping
vec4 Untonemap(vec4 color){
  return ApplyInverseToneMapping(color);
  //return vec4(color.xyz / max(1.0 - Luminance(color), 1e-4), color.w);
}

#if ColorSpace == ColorSpaceYCoCg

// RGB to YCoCg conversion
vec4 RGBToYCoCg(in vec4 c){
//return vec4(vec3(c.yy + ((c.x + c.z) * vec2(0.5, -0.5)), c.x - c.z).xzy * 0.5, 1.0);
  return vec4(mat3(0.25, 0.5, -0.25, 0.5, 0, 0.5, 0.25, -0.5, -0.25) * c.xyz, c.w);
}

// YCoCg to RGB conversion
vec4 YCoCgToRGB(in vec4 c){
//return vec4((c.xxx + vec3(c.yz, -c.y)) - vec2(c.z, 0.0).xyx, c.w);
  return vec4(mat3(1.0, 1.0, 1.0, 1.0, 0.0, -1.0, -1.0, 1.0, -1.0) * c.xyz, c.w);
}

#define ConvertFromRGB RGBToYCoCg
#define ConvertToRGB YCoCgToRGB

#else

#define ConvertFromRGB
#define ConvertToRGB

#endif

// Clip a point to an axis-aligned bounding box
vec4 ClipAABB(vec4 q, vec4 p, vec3 aabbMin, vec3 aabbMax){
#if 0
  vec3 p_clip = (aabbMin + aabbMax) * 0.5;
	vec3 e_clip = fma(aabbMax - aabbMin, vec3(0.5), vec3(1e-7));
	vec4 v_clip = q - vec4(p_clip, p.w);
	vec3 a_unit = abs(v_clip.xyz / e_clip);
	float maxUnit = max(a_unit.x, max(a_unit.y, a_unit.z));
	return (maxUnit > 1.0) ? vec4(vec4(p_clip, p.w) + (v_clip / maxUnit)) : q;
#else
  const float FLT_MIN = uintBitsToFloat(0x00800000u); // 1.17549435e-38
//const float FLT_MAX = uintBitsToFloat(0x7f7fffffu); // 3.40282347e+38
  vec4 r = q - p;
  vec3 rmax = aabbMax - p.xyz, rmin = aabbMin - p.xyz;
  if(r.x > (rmax.x + FLT_MIN)){
    r *= rmax.x / r.x;
  }
  if(r.y > (rmax.y + FLT_MIN)){
    r *= rmax.y / r.y;
  }
  if(r.z > (rmax.z + FLT_MIN)){
    r *= rmax.z / r.z;
  }
  if(r.x < (rmin.x - FLT_MIN)){
    r *= rmin.x / r.x;
  }
  if(r.y < (rmin.y - FLT_MIN)){
    r *= rmin.y / r.y;
  }
  if(r.z < (rmin.z - FLT_MIN)){
    r *= rmin.z / r.z;
  }
  return p + r;
#endif
}

// Catmull-Rom texture sampling with 9-tap filtering by exploiting the bilinear filtering of the texture hardware.
vec4 textureCatmullRom(const in sampler2DArray tex, const in vec3 uvw, const in float lod){
  vec2 texSize = textureSize(tex, int(lod)).xy,
       uv = uvw.xy,
       samplePos = uv * texSize,
       p11 = floor(samplePos - vec2(0.5)) + vec2(0.5),
       t = samplePos - p11,
       tt = t * t,
       ttt = tt * t,
       w0 = (tt - (ttt * 0.5)) - (0.5 * t),
       w1 = ((ttt * 1.5) - (tt * 2.5)) + vec2(1.0),
       w2 = ((tt * 2.0) - (ttt * 1.5)) + (t * 0.5),
       w3 = (ttt * 0.5) - (tt * 0.5),
       w4 = w1 + w2,
       p00 = (p11 - vec2(1.0)) / texSize,
       p33 = (p11 + vec2(2.0)) / texSize,
       p12 = (p11 + (w2 / w4)) / texSize;
  return (((textureLod(tex, vec3(vec2(p00.x, p00.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p00.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x, p00.y), uvw.z), float(lod)) * w3.x)) * w0.y) +
         (((textureLod(tex, vec3(vec2(p00.x, p12.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p12.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x, p12.y), uvw.z), float(lod)) * w3.x)) * w4.y) +
         (((textureLod(tex, vec3(vec2(p00.x, p33.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p33.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x, p33.y), uvw.z), float(lod)) * w3.x)) * w3.y);
}

// Catmull-Rom over tone mapped texels. The optimized nine-tap form cannot be used here: its
// p12 tap leans on the hardware bilinear filter, and that mixes in linear HDR, where a value
// far above the display's clipping point stays there for almost the whole coverage ramp - the
// same trap that made the SMAA blend pass leave shallow edges untouched. Compressing each
// texel before it is weighted is the whole point, so the taps are fetched individually.
// Note that wx/wy .x and .w are negative (the sharpening lobes), so the sum is no longer a
// convex mix and may leave the operator's domain; the ClipAABB right after this catches that.
vec4 TonemappedTextureCatmullRom(const in sampler2DArray tex, const in vec3 uvw, const in int lod){
  const ivec2 texSize = textureSize(tex, lod).xy;
  const ivec2 maxCoord = texSize - ivec2(1);
  const int layer = int(uvw.z);
  const vec2 samplePos = fma(uvw.xy, vec2(texSize), vec2(-0.5));
  const ivec2 base = ivec2(floor(samplePos));
  const vec2 f = samplePos - vec2(base), ff = f * f, fff = ff * f;
  const vec4 wx = vec4((ff.x - (fff.x * 0.5)) - (0.5 * f.x), ((fff.x * 1.5) - (ff.x * 2.5)) + 1.0, ((ff.x * 2.0) - (fff.x * 1.5)) + (f.x * 0.5), (fff.x * 0.5) - (ff.x * 0.5)),
             wy = vec4((ff.y - (fff.y * 0.5)) - (0.5 * f.y), ((fff.y * 1.5) - (ff.y * 2.5)) + 1.0, ((ff.y * 2.0) - (fff.y * 1.5)) + (f.y * 0.5), (fff.y * 0.5) - (ff.y * 0.5));
  const ivec4 cx = clamp(base.x + ivec4(-1, 0, 1, 2), ivec4(0), ivec4(maxCoord.x)),
              cy = clamp(base.y + ivec4(-1, 0, 1, 2), ivec4(0), ivec4(maxCoord.y));
  return (((Tonemap(texelFetch(tex, ivec3(cx.x, cy.x, layer), lod)) * wx.x) + (Tonemap(texelFetch(tex, ivec3(cx.y, cy.x, layer), lod)) * wx.y) +
           (Tonemap(texelFetch(tex, ivec3(cx.z, cy.x, layer), lod)) * wx.z) + (Tonemap(texelFetch(tex, ivec3(cx.w, cy.x, layer), lod)) * wx.w)) * wy.x) +
         (((Tonemap(texelFetch(tex, ivec3(cx.x, cy.y, layer), lod)) * wx.x) + (Tonemap(texelFetch(tex, ivec3(cx.y, cy.y, layer), lod)) * wx.y) +
           (Tonemap(texelFetch(tex, ivec3(cx.z, cy.y, layer), lod)) * wx.z) + (Tonemap(texelFetch(tex, ivec3(cx.w, cy.y, layer), lod)) * wx.w)) * wy.y) +
         (((Tonemap(texelFetch(tex, ivec3(cx.x, cy.z, layer), lod)) * wx.x) + (Tonemap(texelFetch(tex, ivec3(cx.y, cy.z, layer), lod)) * wx.y) +
           (Tonemap(texelFetch(tex, ivec3(cx.z, cy.z, layer), lod)) * wx.z) + (Tonemap(texelFetch(tex, ivec3(cx.w, cy.z, layer), lod)) * wx.w)) * wy.z) +
         (((Tonemap(texelFetch(tex, ivec3(cx.x, cy.w, layer), lod)) * wx.x) + (Tonemap(texelFetch(tex, ivec3(cx.y, cy.w, layer), lod)) * wx.y) +
           (Tonemap(texelFetch(tex, ivec3(cx.z, cy.w, layer), lod)) * wx.z) + (Tonemap(texelFetch(tex, ivec3(cx.w, cy.w, layer), lod)) * wx.w)) * wy.w);
}

// Sacht-Nehab3 texture sampling with 9-tap filtering by exploiting the bilinear filtering of the texture hardware.
vec4 textureSachtNehab3(const in sampler2DArray tex, const in vec3 uvw, const in float lod){
 vec2 texSize = textureSize(tex, int(lod)).xy,
       uv = uvw.xy,
       samplePos = uv * texSize,
       p11 = floor(samplePos - vec2(0.5)) + vec2(0.5),
       t = samplePos - p11,
       tt = t * t,
       ttt = tt * t,
       w0 = (((0.218848 - (0.497801 * t)) + (0.370818 * tt)) - (0.0899247 * ttt)),
       w1 = (((0.562591 + (0.0446542 * t)) - (0.700012 * tt)) + (0.309387 * ttt)),
       w2 = (((0.216621 + (0.427208 * t)) + (0.228149 * tt)) - (0.309387 * ttt)),
       w3 = (((0.00194006 + (0.0259387 * t)) + (0.101044 * tt)) + (0.0899247 * ttt)),
       w4 = w1 + w2,
       p00 = (p11 - vec2(1.0)) / texSize,
       p33 = (p11 + vec2(2.0)) / texSize,
       p12 = (p11 + (w2 / w4)) / texSize;
  return (((textureLod(tex, vec3(vec2(p00.x, p00.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p00.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x, p00.y), uvw.z), float(lod)) * w3.x)) * w0.y) +
         (((textureLod(tex, vec3(vec2(p00.x, p12.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p12.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x, p12.y), uvw.z), float(lod)) * w3.x)) * w4.y) +
         (((textureLod(tex, vec3(vec2(p00.x, p33.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p33.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x, p33.y), uvw.z), float(lod)) * w3.x)) * w3.y);
}

// Fallback FXAA for disoccluded areas
vec4 FallbackFXAA(const in vec2 invTexSize){
  const vec2 fragCoordInvScale = invTexSize;
  vec4 p = vec4(inTexCoord, vec2(inTexCoord - (fragCoordInvScale * (0.5 + (1.0 / 4.0)))));
  const float FXAA_SPAN_MAX = 8.0,
              FXAA_REDUCE_MUL = 1.0 / 8.0,
              FXAA_REDUCE_MIN = 1.0 / 128.0;
  vec3 rgbNW = ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0).xyz),
       rgbNE = ApplyToneMapping(textureLodOffset(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0, ivec2(1, 0)).xyz),
       rgbSW = ApplyToneMapping(textureLodOffset(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0, ivec2(0, 1)).xyz),
       rgbSE = ApplyToneMapping(textureLodOffset(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0, ivec2(1, 1)).xyz),
       rgbM = ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy, float(gl_ViewIndex)), 0.0).xyz),
       luma = vec3(0.2126, 0.7152, 0.0722);
  float lumaNW = dot(rgbNW, luma),
        lumaNE = dot(rgbNE, luma),
        lumaSW = dot(rgbSW, luma),
        lumaSE = dot(rgbSE, luma),
        lumaM = dot(rgbM, luma),
        lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE))),
        lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));
  vec2 dir = vec2(-((lumaNW + lumaNE) - (lumaSW + lumaSE)), ((lumaNW + lumaSW) - (lumaNE + lumaSE)));
  float dirReduce = max((lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL), FXAA_REDUCE_MIN),
  rcpDirMin = 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);
  dir = min(vec2(FXAA_SPAN_MAX, FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX), dir * rcpDirMin)) * fragCoordInvScale;
  vec4 rgbA = (1.0 / 2.0) * (ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((1.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw) + ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((2.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw)),
       rgbB = (rgbA * (1.0 / 2.0)) + ((1.0 / 4.0) * (ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((0.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw) + ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((3.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw)));
  float lumaB = dot(rgbB.xyz, luma);
  return clamp(ApplyInverseToneMapping(((lumaB < lumaMin) || (lumaB > lumaMax)) ? rgbA : rgbB), vec4(0.0), vec4(65504.0));
}

// Check for disocclusions and return true if disoccluded, otherwise false.
bool IsDisoccluded(const in vec3 uvw, const in vec3 historyUVW, const in vec4 current, const in vec2 invTexSize, const in vec2 depthTransform){

  // First frame disocclusion or disable temporal antialiasing
  if((pushConstants.flags & (FLAG_FIRST_FRAME_DISOCCLUSION | FLAG_DISABLE_TEMPORAL_ANTIALIASING)) != 0u){
    return true;
  }

  // Screen disocclusion
  if(any(lessThan(historyUVW.xy, vec2(0.0))) || any(greaterThan(historyUVW.xy, vec2(1.0)))){
    return true;
  }

  // Optional translucency disocclusion, for optionally to force of different handling of translucent surfaces without temporal antialiasing,
  // since these have no valid motion vector data.
  if(((pushConstants.flags & FLAG_TRANSLUCENT_DISOCCLUSION) != 0u) && (current.w < 1e-7)){
    return true;
  }

  // Optional velocity disocclusion for further reducing ghosting artifacts.
  if((pushConstants.flags & FLAG_VELOCITY_DISOCCLUSION) != 0u){
    const vec2 historyVelocity = textureLod(uHistoryVelocityTexture, historyUVW, 0.0).xy;
    if(length(textureLod(uCurrentVelocityTexture, uvw, 0.0).xy - historyVelocity) > pushConstants.velocityDisocclusionThreshold){
      return true;
    }
  }

  // Otherwise we're not disoccluded
  return false;

}

// How far the history at the reprojected position can still be trusted on the strength of its depth, as a
// factor between zero and one rather than as a verdict.
//
// A verdict is what this used to be, and it was the wrong shape for the job. The velocity is dilated
// towards the closest of the neighbouring samples, so along every depth discontinuity the background
// pixels get reprojected by the FOREGROUND's motion vector - deliberately, so that the foreground edge is
// the one that stays stable. Their history really is wrong, so the test fires, and it is right to. But a
// moving camera puts such a band along every silhouette in the picture in every frame, and answering with
// a flat no drops all of them to the fallback, which is precisely the aliasing that shows up. Weighing the
// history down instead keeps most of what temporal accumulation is worth in those bands while still
// suppressing the ghost, and a genuine disocclusion drives the factor to zero anyway, where the caller's
// own threshold hands it to the fallback as before.
//
// The comparison happens in reciprocal linearized depth, which is finite for a reversed infinite far plane
// as well as for an ordinary finite one, so no case distinction between the two is needed. The tolerance
// has to be built rather than picked, because a single distance does not mean the same thing twice in a
// perspective picture: a relative part, so that it scales with the distance itself, plus whatever the
// surface's own slope already accounts for, plus a floor beyond which everything is equally far away.
float DepthDisocclusionWeight(const in vec3 uvw, const in vec3 historyUVW){

  const ivec2 depthTextureSize = textureSize(uCurrentDepthTexture, 0).xy;
  const ivec2 maxCoord = depthTextureSize - ivec2(1);
  const int layer = int(uvw.z);

  const ivec2 currentCoord = ivec2(uvw.xy * vec2(depthTextureSize));
  const float currentInverseDepth = InverseLinearDepthAt(uCurrentDepthTexture, currentCoord, maxCoord, layer);

  // How much the depth changes over one texel here. On a surface seen at a grazing angle - a road
  // filling the lower half of the screen, say - it changes a great deal from one texel to the next
  // without anything having been disoccluded, and a test that does not allow for that throws away the
  // history of the whole surface. This is the slope the reprojection is permitted to have moved along.
  //
  // Both sides of each axis are looked at, and the SMALLER of the two is taken. On a smooth surface
  // the two agree and the choice does not matter, but on a silhouette one of them steps across the
  // jump: a centred difference would average that in and a maximum would take it outright, and either
  // one inflates the tolerance exactly where the disocclusions are, switching the test off where it is
  // the whole point. The smaller one is the side that is still the same surface. The diagonals are
  // left out on purpose - the gradient of a plane is already fully determined by its two axes.
  const float slopeX = min(abs(InverseLinearDepthAt(uCurrentDepthTexture, currentCoord + ivec2( 1,  0), maxCoord, layer) - currentInverseDepth),
                           abs(InverseLinearDepthAt(uCurrentDepthTexture, currentCoord + ivec2(-1,  0), maxCoord, layer) - currentInverseDepth));
  const float slopeY = min(abs(InverseLinearDepthAt(uCurrentDepthTexture, currentCoord + ivec2( 0,  1), maxCoord, layer) - currentInverseDepth),
                           abs(InverseLinearDepthAt(uCurrentDepthTexture, currentCoord + ivec2( 0, -1), maxCoord, layer) - currentInverseDepth));
  const float slope = slopeX + slopeY;

  // The reprojected position lands between texels, so take the nearest of the four it falls between
  // rather than a filtered value. Which of the four is nearest is exactly the question being asked, so
  // picking the closest one is not a fudge: it asks whether ANY of the surfaces the history could
  // plausibly have come from matches, and only reports a disocclusion when none of them does.
  const vec2 historyPosition = fma(historyUVW.xy, vec2(depthTextureSize), vec2(-0.5));
  const ivec2 historyBase = ivec2(floor(historyPosition));
  float historyInverseDepth = InverseLinearDepthAt(uHistoryDepthTexture, historyBase + ivec2(0, 0), maxCoord, layer);
  float difference = abs(historyInverseDepth - currentInverseDepth);

  float candidate = InverseLinearDepthAt(uHistoryDepthTexture, historyBase + ivec2(1, 0), maxCoord, layer);
  float candidateDifference = abs(candidate - currentInverseDepth);
  if(candidateDifference < difference){
    difference = candidateDifference;
    historyInverseDepth = candidate;
  }

  candidate = InverseLinearDepthAt(uHistoryDepthTexture, historyBase + ivec2(0, 1), maxCoord, layer);
  candidateDifference = abs(candidate - currentInverseDepth);
  if(candidateDifference < difference){
    difference = candidateDifference;
    historyInverseDepth = candidate;
  }

  candidate = InverseLinearDepthAt(uHistoryDepthTexture, historyBase + ivec2(1, 1), maxCoord, layer);
  candidateDifference = abs(candidate - currentInverseDepth);
  if(candidateDifference < difference){
    difference = candidateDifference;
    historyInverseDepth = candidate;
  }

  const float tolerance = fma(max(currentInverseDepth, historyInverseDepth),
                              pushConstants.depthDisocclusionRelativeThreshold,
                              slope * pushConstants.depthDisocclusionSlopeScale) +
                          pushConstants.depthDisocclusionFloor;

  // Full trust up to the tolerance, none from twice it onwards, straight line in between. Expressed as a
  // ratio so that it needs no scale of its own - the tolerance already carries the units.
  return clamp(2.0 - (difference / max(tolerance, 1e-9)), 0.0, 1.0);

}

void main() {

  vec2 texSize = vec2(textureSize(uCurrentColorTexture, 0).xy);
  vec2 invTexSize = vec2(1.0) / texSize;

  vec4 color = vec4(0.0);

  vec3 uvw = vec3(inTexCoord, float(gl_ViewIndex));

#if 0
  vec4 current = textureLod(uCurrentColorTexture, uvw - vec3(pushConstants.jitterUV, 0.0), 0.0); // With unjittering
#else
  vec4 current = textureLod(uCurrentColorTexture, uvw, 0.0); // Without unjittering
#endif

  vec2 depthTransform = vec2(pushConstants.ZMul, pushConstants.ZAdd);

  // Find the closest depth sample and its attached information
  vec4 velocityUVWZ;
  {
    vec3 depthSamples[9] = vec3[9](
      vec3(-1.0, -1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2(-1.0, -1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3( 0.0, -1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 0.0, -1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3( 1.0, -1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 1.0, -1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3(-1.0,  0.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2(-1.0,  0.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3( 0.0,  0.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 0.0,  0.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3( 1.0,  0.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 1.0,  0.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3(-1.0,  1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2(-1.0,  1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3( 0.0,  1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 0.0,  1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y)),
      vec3( 1.0,  1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 1.0,  1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y))
    );
    vec3 bestDepth = depthSamples[0];
    if(bestDepth.z < depthSamples[1].z){ bestDepth = depthSamples[1]; }
    if(bestDepth.z < depthSamples[2].z){ bestDepth = depthSamples[2]; }
    if(bestDepth.z < depthSamples[3].z){ bestDepth = depthSamples[3]; }
    if(bestDepth.z < depthSamples[4].z){ bestDepth = depthSamples[4]; }
    if(bestDepth.z < depthSamples[5].z){ bestDepth = depthSamples[5]; }
    if(bestDepth.z < depthSamples[6].z){ bestDepth = depthSamples[6]; }
    if(bestDepth.z < depthSamples[7].z){ bestDepth = depthSamples[7]; }
    if(bestDepth.z < depthSamples[8].z){ bestDepth = depthSamples[8]; }
    velocityUVWZ = vec4(fma(bestDepth.xy, invTexSize, uvw.xy), uvw.z, bestDepth.z);
  }

  // Check for far plane, but avoid translucent surfaces which does writes also no depth data like the background
  bool isBackground = ((velocityUVWZ.w < 1e-7) && (current.w > 0.5));

  // Check if we're in the far plane and the background should be included in the temporal antialiasing or not
  if(((pushConstants.flags & FLAG_INCLUDE_BACKGROUND) == 0u) && isBackground){

    // We're in the far plane, so no temporal antialiasing or similar, so that background und similiar things are always sharp.

    color = current;

  }else{

    // Otherwise do our job.

    // Get the current velocity
    vec2 currentVelocity = textureLod(uCurrentVelocityTexture, velocityUVWZ.xyz, 0.0).xy;

    // Offset the history UVW by the current velocity
    vec3 historyUVW = uvw - vec3(currentVelocity, 0.0);

    // Get the current color samples
    vec4 currentSamples[9] = vec4[9](
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1, -1)))), // a 0
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0, -1)))), // b 1
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1, -1)))), // c 2
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  0)))), // d 3
      current = ConvertFromRGB(Tonemap(current)), // ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  0)))), // e 4
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  0)))), // f 5
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  1)))), // g 6
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  1)))), // h 7
      ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  1))))  // i 8
    );

    // Convert the current color to YCoCg color space and apply tonemapping
    // current = ConvertFromRGB(Tonemap(current));

#if 1
    // Soft minimum and maximum ("Hybrid Reconstruction Antialiasing")
    //        1         0 1 2
    // (min 3 4 5 + min 3 4 5) * 0.5
    //        7         6 7 8
    vec4 minimumColor = min(min(min(min(currentSamples[1], currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[7]),
         maximumColor = max(max(max(max(currentSamples[1], currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[7]);
    minimumColor = (minimumColor + min(min(min(min(minimumColor, currentSamples[0]), currentSamples[2]), currentSamples[6]), currentSamples[8])) * 0.5;
    maximumColor = (maximumColor + max(max(max(max(maximumColor, currentSamples[0]), currentSamples[2]), currentSamples[6]), currentSamples[8])) * 0.5;
#else
    // Simple minimum and maximum
    vec4 minimumColor = min(min(min(min(min(min(min(min(currentSamples[0], currentSamples[1]), currentSamples[2]), currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[6]), currentSamples[7]), currentSamples[8]),
         maximumColor = max(max(max(max(max(max(max(max(currentSamples[0], currentSamples[1]), currentSamples[2]), currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[6]), currentSamples[7]), currentSamples[8]);
#endif

    // Average color
    vec4 averageColor = (currentSamples[0] + currentSamples[1] + currentSamples[2] + currentSamples[3] + currentSamples[4] + currentSamples[5] + currentSamples[6] + currentSamples[7] + currentSamples[8]) * (1.0 / 9.0);

    if((pushConstants.flags & FLAG_VARIANCE_CLIPPING) != 0u){
      // Variance clipping ("An Excursion in Temporal Supersampling")
      vec4 m0 = currentSamples[0],
            m1 = currentSamples[0] * currentSamples[0];
      for(int i = 1; i < 9; i++) {
        vec4 currentSample = currentSamples[i];
        m0 += currentSample;
        m1 += currentSample * currentSample;
      }
      m0 *= 1.0 / 9.0;
      m1 *= 1.0 / 9.0;
      vec4 sigma = sqrt(m1 - (m0 * m0)) * pushConstants.varianceClipGamma;
      minimumColor = max(minimumColor, m0 - sigma);
      maximumColor = min(maximumColor, m0 + sigma);
    }

#if ColorSpace == ColorSpaceYCoCg
    // Shrink chroma extents for luminance-chroma-based color spaces like YCoCg, YCbCr, YUV, etc.
    if((pushConstants.flags & FLAG_CHROMA_SHRINKING) != 0u){
       // TODO: Fix this for very bright colors (=> butterfly artifacts later at bloom)
      vec2 chromaExtent = vec2(maximumColor.x - minimumColor.x) * 0.125;
      vec2 chromaCenter = current.yz;
      minimumColor.yz = chromaCenter - chromaExtent;
      maximumColor.yz = chromaCenter + chromaExtent;
      averageColor.yz = chromaCenter;
    }
#endif

    float blendWeight;

    vec4 historySample;

    // Check for disocclusion / rejection
    if(IsDisoccluded(uvw, historyUVW, current, invTexSize, depthTransform.xy)){

      // Disoccluded / rejected

      // Mark as rejected because of disocclusion (weight = 0.0)
      blendWeight = 0.0;

      // No valid history sample in this case
      historySample = vec4(0.0);

    }else{

      // Not disoccluded / rejected

      // Initial weight for blending (weight = 1.0), which will be modified later if needed
      blendWeight = 1.0;

      // Weigh the history down where its depth says it belongs to another surface than the one in front of
      // us now. Not a rejection: see the note at DepthDisocclusionWeight for why this must be gradual.
      if((pushConstants.flags & FLAG_DEPTH_DISOCCLUSION) != 0u){
        blendWeight *= DepthDisocclusionWeight(uvw, historyUVW);
      }

      // Get the history color sample, convert it to YCoCg color space and apply tonemapping
      historySample = ConvertFromRGB(TonemappedTextureCatmullRom(uHistoryColorTexture, historyUVW, 0));

      // Clip the history color sample to the current minimum and maximum color values
      if((pushConstants.flags & FLAG_CLIPPING) != 0u){
        historySample = ClipAABB(historySample, clamp(averageColor, minimumColor, maximumColor), minimumColor.xyz, maximumColor.xyz);
      }

      // Luminance weighting with different feedback coefficients for opaque and translucent surfaces
      if((pushConstants.flags & FLAG_LUMINANCE_WEIGHTING) != 0u){
  #if ColorSpace == ColorSpaceYCoCg
        float currentLuminance = current.x;
        float historyLuminance = historySample.x;
  #else
        float currentLuminance = Luminance(current);
        float historyLuminance = Luminance(historySample);
  #endif
        float unbiasedWeight = 1.0 - (abs(currentLuminance - historyLuminance) / max(currentLuminance, max(historyLuminance, 0.2)));
        float unbiasedWeightSquaredClamped = clamp(unbiasedWeight * unbiasedWeight, 0.0, 1.0);
        float luminanceDisocclusionBasedBlendFactor = isBackground
          ? mix(pushConstants.backgroundFeedbackMin, pushConstants.backgroundFeedbackMax, unbiasedWeightSquaredClamped) // Background
          : mix(
              mix(pushConstants.translucentFeedbackMin, pushConstants.translucentFeedbackMax, unbiasedWeightSquaredClamped), // Translucent
              mix(pushConstants.opaqueFeedbackMin, pushConstants.opaqueFeedbackMax, unbiasedWeightSquaredClamped), // Opaque
              clamp(current.w, 0.0, 1.0) // In the alpha channel of the current color sample the translucency/opacity factor is stored, 0.0 = full translucent, 1.0 = full opaque
            );

        blendWeight *= luminanceDisocclusionBasedBlendFactor;

      }

    }

    // Optionally apply sharping when enabled
    if(pushConstants.sharpingFactor > 1e-7){
      current += (vec4(1.0) - exp(-(current - clamp(averageColor, minimumColor, maximumColor)))) * pushConstants.sharpingFactor;
    }

    // Check for valid history sample for blending (valid = not rejected, for example by disocclusion)
    if(blendWeight > 1e-7){

      // When valid, blend the current and history color samples based on the blend weight
      color = clamp(Untonemap(ConvertToRGB(mix(current, historySample, blendWeight))), vec4(0.0), vec4(65504.0));

    }else{

      // When not valid, use the current color sample or use fallback FXAA when enabled.

      if((pushConstants.flags & FLAG_USE_FALLBACK_FXAA) != 0u){
        // Use fallback FXAA for to have still a more or less initial antialiased result in rejected areas
        // But attentation, FXAA don't use the sharpened color calculated above, so it isn't post-sharped then.
        color = FallbackFXAA(invTexSize);
      }else{
        // Use the current color sample without blending directly
        color = clamp(Untonemap(ConvertToRGB(current)), vec4(0.0), vec4(65504.0));
      }

      color = mix(color, vec4(1.0, 0.0, 0.0, 1.0), pushConstants.disocclusionDebugFactor);

    }

  }

  outFragColor = color;

}
