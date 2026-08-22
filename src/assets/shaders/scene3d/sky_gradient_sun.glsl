#ifndef SKY_GRADIENT_SUN_GLSL
#define SKY_GRADIENT_SUN_GLSL

// The sun of the stylized gradient sky.
//
// Shared by the visible sky (skybox.frag, mode 3) and by the cube map the same sky is baked into for the
// IBL (cubemap_gradient.comp), because the two have to agree: a sun that is drawn in one place and lit
// from another is a light with no source and a source with no light.
//
// It is three things, not one:
//
//   the DISC   - small and hard-edged, a few tenths of a degree across, and far brighter than anything
//                else in the frame, so it clips to white and the bloom pass makes the star around it.
//                That overbright core is the whole difference between a sun and a pale spot: a sun is
//                not a bright colour, it is a value the display cannot hold.
//   the AUREOLE- the tight halo immediately around it, a few degrees wide, from light scattered forward
//                by the air. It is what keeps the disc from looking pasted on.
//   the GLOW   - the broad brightening of the sky towards the sun, tens of degrees wide and dim. It
//                carries most of the sun's contribution to the ambient light, which matters because the
//                cube map baked from this is what lights everything that is not hit by the directional
//                light directly.
//
// What it replaced was a single smoothstep across fifteen degrees at eight times the horizon colour: one
// soft blob, the same brightness as a bright cloud, with no core and no falloff beyond its own edge. It
// read as a circle drawn on the sky, which is exactly what it was.
//
// aSunRadius is the ANGULAR RADIUS IN RADIANS. It used to be a threshold on 1-cos(angle), where the 0.035
// the tracks ask for meant a disc fifteen degrees across; read as radians the same number is two degrees,
// and the sensible values are smaller still - the real sun is 0.0047.

// How far above the sky the disc sits. It is meant to clip, and by a lot; anything that merely reaches
// the top of the range comes out as a flat white circle instead of a point of light with a halo.
const float SkyGradientSunDiscOverbright = 150.0;

// The aureole, as a multiple of the disc radius and a fraction of the sun's brightness.
const float SkyGradientSunAureoleWidth = 8.0;
const float SkyGradientSunAureoleStrength = 0.9;

// And the broad glow, whose half-width is an angle in radians rather than a multiple of anything - it is
// a property of the air, not of how big the sun happens to be drawn. Fifteen degrees, which is about what
// the old blob covered, so the light the cube map bakes from this stays roughly what it was.
const float SkyGradientSunGlowWidth = 0.26;
const float SkyGradientSunGlowStrength = 0.55;

// Half at aHalfWidth, and never quite zero. Written on the angle rather than as a power of the cosine
// because the powers a disc this small needs run into the hundreds, where the precision goes.
float skyGradientSunFalloff(const in float aAngle, const in float aHalfWidth){
  float t = aAngle / max(aHalfWidth, 1e-6);
  return exp2(-(t * t));
}

vec3 skyGradientSun(const in vec3 aDirection,
                    const in vec3 aSunDirection,
                    const in vec3 aHorizonColor,
                    const in float aSunRadius,
                    const in float aSunBrightness){

  float cosAngle = clamp(dot(aDirection, aSunDirection), -1.0, 1.0);
  float angle = acos(cosAngle);

  float radius = max(aSunRadius, 1e-5);

  // The hue of the horizon without its brightness, so a dark sky palette does not also make a dark sun.
  // The disc is white where the sun stands high and takes that hue as it comes down to the horizon, which
  // is the reddening everyone has seen and nobody has to be told about.
  float horizonPeak = max(max(aHorizonColor.x, aHorizonColor.y), max(aHorizonColor.z, 1e-4));
  vec3 horizonHue = aHorizonColor / horizonPeak;
  float lowness = 1.0 - clamp(aSunDirection.y * 3.0, 0.0, 1.0);
  vec3 tint = mix(vec3(1.0), horizonHue, lowness);

  // A soft edge about one pixel wide at the resolutions and fields of view this is looked at from, kept
  // as a fraction of the radius so it holds when the sun is made larger or smaller. Not fwidth: the cube
  // map side of this is a compute shader and has no derivatives to take.
  float edge = radius * 0.08;
  float disc = 1.0 - smoothstep(radius - edge, radius + edge, angle);

  float aureole = skyGradientSunFalloff(angle, radius * SkyGradientSunAureoleWidth);
  float glow = skyGradientSunFalloff(angle, SkyGradientSunGlowWidth);

  return ((tint * (aSunBrightness * SkyGradientSunDiscOverbright)) * disc) +
         ((tint * (aSunBrightness * SkyGradientSunAureoleStrength)) * aureole) +
         ((aHorizonColor * (aSunBrightness * SkyGradientSunGlowStrength)) * glow);

}

#endif
