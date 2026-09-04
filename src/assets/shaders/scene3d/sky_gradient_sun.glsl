#ifndef SKY_GRADIENT_SUN_GLSL
#define SKY_GRADIENT_SUN_GLSL

#include "sun_disc.glsl"

// The sun of the stylized gradient sky.
//
// The shape of it - the disc, its edge, its rim, its falloff - is not written here but in sun_disc.glsl,
// which the atmosphere's own sun uses as well, so that the two cannot end up disagreeing about how wide a
// sun is. What stays here is everything that is particular to THIS sky: its brightness convention, which
// is a multiple of the palette rather than an absolute luminance, and the horizon tinting below.
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

// How far above the sky the disc sits. It is meant to clip, and by a long way: the exposure is worked out
// from the whole frame, so looking towards the sun raises the average and pulls the exposure down, and a
// disc that only just reached the top of the range at a normal exposure lands well under it there - which
// is where a sun stops being a sun. Measured rather than picked: at a hundred and fifty the disc came out
// around two hundred of two hundred and fifty-five with the sky in frame, a pale dot.
const float SkyGradientSunDiscOverbright = 1200.0;

// The aureole, as a multiple of the disc radius and a fraction of the sun's brightness.
const float SkyGradientSunAureoleWidth = 6.0;
const float SkyGradientSunAureoleStrength = 0.55;

// And the broad glow, whose half-width is an angle in radians rather than a multiple of anything - it is
// a property of the air, not of how big the sun happens to be drawn.
//
// Six degrees, not the fifteen it first had. Fifteen was chosen to carry about as much energy as the blob
// this replaced, so that the light baked from the cube map would not change - but on screen it filled the
// frame with a wash whenever the camera faced the sun, which is worse than what it replaced and takes the
// exposure down with it. The directional light carries the scene; what the sky adds to it is a bonus, and
// GradientEnvironmentIntensityFactor is the knob for that.
const float SkyGradientSunGlowWidth = 0.10;
const float SkyGradientSunGlowStrength = 0.18;

// A soft edge about one pixel wide at the resolutions and fields of view this is looked at from, kept as a
// fraction of the radius so it holds when the sun is made larger or smaller.
const float SkyGradientSunEdgeSoftness = 0.08;

// aHalo scales the aureole and the glow together. At one they are as written above; at zero nothing is
// left but the disc - a circle a few pixels across and nothing around it, which is what the sun really
// looks like from here, with the halo and the streaks left to the bloom to make.
vec3 skyGradientSun(const in vec3 aDirection,
                    const in vec3 aSunDirection,
                    const in vec3 aHorizonColor,
                    const in float aSunRadius,
                    const in float aSunBrightness,
                    const in float aHalo){

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

  float disc = sunDiscCoverage(angle, radius, SkyGradientSunEdgeSoftness);

  float halo = clamp(aHalo, 0.0, 1.0);
  float aureole = sunDiscFalloff(angle, radius * SkyGradientSunAureoleWidth) * halo;
  float glow = sunDiscFalloff(angle, SkyGradientSunGlowWidth) * halo;

  return ((tint * (aSunBrightness * SkyGradientSunDiscOverbright)) * disc) +
         ((tint * (aSunBrightness * SkyGradientSunAureoleStrength)) * aureole) +
         ((aHorizonColor * (aSunBrightness * SkyGradientSunGlowStrength)) * glow);

}

#endif
