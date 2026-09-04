#ifndef SUN_DISC_GLSL
#define SUN_DISC_GLSL

// The shape of a drawn sun, in one place.
//
// There is more than one place in this renderer that puts a sun on the sky: the atmosphere does it in
// GetSunLuminance (atmosphere_common.glsl), and the stylised gradient sky does it in sky_gradient_sun.glsl.
// They draw into different worlds - the atmosphere works in absolute luminances that are afterwards
// multiplied by the transmittance of the air, the gradient sky works in multiples of its own palette - so
// the BRIGHTNESS has to stay with each of them. What must not differ is the SHAPE: how wide the disc is,
// how its edge ends, how it darkens towards its rim, and how much dimmer it has to get when it is drawn
// larger than it really is. That is what lives here, unit-free, so that the two cannot drift apart.
//
// Every angle in here is in radians, measured from the centre of the sun.

// Half at aHalfWidth, and never quite zero. Written on the angle rather than as a power of the cosine
// because the powers a disc this small needs run into the hundreds, where the precision goes.
float sunDiscFalloff(const in float aAngle, const in float aHalfWidth){
  float t = aAngle / max(aHalfWidth, 1e-6);
  return exp2(-(t * t));
}

// One inside the disc, zero outside it. aEdgeSoftness is the width of the transition as a fraction of the
// radius, so it holds when the sun is made larger or smaller; zero gives the hard edge the atmosphere has
// always had. A soft edge is worth having as soon as the disc is drawn any larger than the real sun: a
// hard-edged circle of a value the display cannot hold is exactly what temporal antialiasing cannot settle
// on. Not fwidth: the cube map side of this is a compute shader and has no derivatives to take.
float sunDiscCoverage(const in float aAngle, const in float aRadius, const in float aEdgeSoftness){
  float edge = aRadius * aEdgeSoftness;
  return (edge > 1e-9) ? (1.0 - smoothstep(aRadius - edge, aRadius + edge, aAngle))
                       : (1.0 - step(aRadius, aAngle));
}

// The sun is a sphere of glowing gas, not a disc of paint: towards its rim the line of sight leaves through
// cooler, higher layers, so it is dimmer there. aAmount is the classical linear coefficient - zero is the
// flat disc, around 0.6 is what the eye sees. It costs three instructions and it is the whole difference
// between a sun and a sticker, once the disc is big enough to have a rim at all.
float sunDiscLimbDarkening(const in float aAngle, const in float aRadius, const in float aAmount){
  float mu = sqrt(max(0.0, 1.0 - ((aAngle * aAngle) / max(aRadius * aRadius, 1e-12))));
  return 1.0 - (aAmount * (1.0 - mu));
}

// Drawing the sun larger than it is means drawing more of the frame at that value, and the automatic
// exposure reads the whole frame: a sun made three times as wide is nine times the area at the same
// luminance, the exposure stops down to cover it, and the picture goes dark whenever the camera faces the
// sun - which is the opposite of what making it larger was for. Keeping the flux constant instead
// (luminance times area) leaves the exposure where it was and lets the sun simply be bigger.
//
// aAmount fades between the two: one keeps the flux, zero keeps the luminance and lets it get brighter with
// its area. At aDrawnRadius == aBaseRadius the result is exactly one either way.
float sunDiscEnergyScale(const in float aBaseRadius, const in float aDrawnRadius, const in float aAmount){
  float ratio = aBaseRadius / max(aDrawnRadius, 1e-6);
  return mix(1.0, ratio * ratio, clamp(aAmount, 0.0, 1.0));
}

// The disc plus the tight halo around it, composed. The caller passes radiances rather than a brightness
// and a set of weights, because what those weights mean is the caller's business - see the header above.
// An aureole of zero costs nothing and leaves the bare disc.
vec3 sunDiscRadiance(const in float aAngle,
                     const in float aRadius,
                     const in float aEdgeSoftness,
                     const in float aLimbDarkening,
                     const in vec3 aDiscRadiance,
                     const in vec3 aAureoleRadiance,
                     const in float aAureoleWidth){
  vec3 result = aDiscRadiance * (sunDiscCoverage(aAngle, aRadius, aEdgeSoftness) *
                                 sunDiscLimbDarkening(aAngle, aRadius, aLimbDarkening));
  if(any(greaterThan(aAureoleRadiance, vec3(0.0)))){
    result += aAureoleRadiance * sunDiscFalloff(aAngle, aRadius * aAureoleWidth);
  }
  return result;
}

#endif
