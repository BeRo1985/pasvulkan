#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

#if defined(SHADERDEBUG)
#extension GL_EXT_debug_printf : enable
#endif

// Radial (zoom) blur: every pixel is smeared along the line between it and a centre, by an amount that
// grows with its distance from that centre. The middle of the picture stays sharp and the edges streak
// outwards, which is what speed looks like from inside the thing that is moving.
//
// It is not a velocity-buffer motion blur and does not pretend to be: it knows nothing about what moved
// or where. It is the effect a camera gives when the whole world is coming at it, and for a racing game
// looking forwards that happens to be very nearly right - the true screen-space motion of a static world
// under a camera flying into it IS radial about the point being flown towards.
//
// Sits in the post chain after tone mapping, so it works on displayable colours: doing it in HDR before
// the tone map smears the clipped highlights into long white bars instead of streaks.

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

layout(push_constant) uniform PushConstants {
  vec4 centreStrengthInner;    // xy = centre in [0..1] texture coordinates, z = strength, w = inner radius
  float squaredFallOffFactor;  // 0 = the drag grows straight from the inner radius outwards, 1 = squared
} pushConstants;

// Enough that the streak reads as a streak rather than as a row of copies, few enough that it costs one
// texture fetch per tap and no more.
const int CountTaps = 12;

// A per-pixel offset of up to one tap, so the taps of neighbouring pixels do not line up and the streak
// does not band into visible steps. Cheap hash, no texture needed.
float dither(const in vec2 aFragCoord){
  return fract(sin(dot(aFragCoord, vec2(12.9898, 78.233))) * 43758.5453);
}

void main(){

  float strength = pushConstants.centreStrengthInner.z;

#if defined(SHADERDEBUG)
  // One pixel's worth, once per frame, straight out of the shader: what the push constants actually say
  // here. It answers "does the number the game sets arrive?" without a screenshot and without guessing -
  // which is the question that cost several compare-two-runs rounds before it was asked this way.
  if(all(equal(ivec2(gl_FragCoord.xy), ivec2(1, 1)))){
    debugPrintfEXT("radialblur: centre %f %f, strength %f, inner %f",
                   pushConstants.centreStrengthInner.x,
                   pushConstants.centreStrengthInner.y,
                   pushConstants.centreStrengthInner.z,
                   pushConstants.centreStrengthInner.w);
  }
#endif

  if(strength <= 0.0){
    outFragColor = textureLod(uTexture, vec3(inTexCoord, gl_ViewIndex), 0.0);
    return;
  }

  vec2 centre = pushConstants.centreStrengthInner.xy;
  vec2 toCentre = inTexCoord - centre;

  // Nothing at all in the middle, growing outwards from the inner radius. Blur that reaches the centre
  // takes the thing being looked at with it, which is the one place it must not.
  //
  // How fast it grows is a dial rather than a decision. Squared was the first try and it made the effect
  // invisible everywhere but in the last few pixels of the corners - at a third of the way out the square
  // has already cut the drag to a ninth, so the whole middle band of the picture, which is most of the
  // picture, got nothing. Straight is the other end: the mid-field smears, and the effect reads, at the
  // cost of coming closer to what the eye is actually on. Everything in between is available, which is
  // the point, because which of the two is right is a matter of taste and of the track.
  float inner = pushConstants.centreStrengthInner.w;
  float falloff = clamp((length(toCentre) - inner) / max(1.0 - inner, 1e-4), 0.0, 1.0);
  float reach = strength * (falloff * mix(1.0, falloff, pushConstants.squaredFallOffFactor));

  vec4 sum = vec4(0.0);
  float step = reach / float(CountTaps);
  float start = dither(gl_FragCoord.xy) * step;

  for(int index = 0; index < CountTaps; index++){
    vec2 texCoord = inTexCoord - (toCentre * (start + (step * float(index))));
    sum += textureLod(uTexture, vec3(clamp(texCoord, vec2(0.0), vec2(1.0)), gl_ViewIndex), 0.0);
  }

  outFragColor = sum * (1.0 / float(CountTaps));

}
