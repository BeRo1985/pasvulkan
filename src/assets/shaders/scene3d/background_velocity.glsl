#ifndef BACKGROUND_VELOCITY_GLSL
#define BACKGROUND_VELOCITY_GLSL

// The motion vector of a pixel with nothing drawn into it, in the convention of the velocity buffer: half the
// difference of the current and the previous position in clip space, each without its jitter.
//
// For the most part such a pixel is the sky, which is a direction rather than a place and moves with the rotation
// of the camera alone. That is what reprojecting a point at the far plane gives. An atmosphere seen from outside,
// or from high above its dense layers, is something else: the bright rim around a planet is fixed to the planet.
// When the camera circles the planet, the rim stays where it is on the screen while the rotation of the camera says
// that it moves, the history is fetched from the wrong place, and the rim trembles under every temporal
// antialiasing and upscaling.
//
// So every atmosphere is looked at along the ray of the pixel. Where the ray passes through it, the density along
// the ray is nearly a Gaussian around the point of the closest approach to the centre, and that is where the light
// of the rim comes from. This point is reprojected with the transform of the atmosphere of the previous frame, so
// that a planet which moves takes its rim along. Two things decide how much its motion counts against the one of
// the far plane:
//
// - How much of the atmosphere the ray meets, as the opacity along the whole chord. Behind the thin outer layers
//   lie the stars, and they do move with the rotation of the camera alone.
// - Whether that point lies well in front of the camera at all. From within the dense layers, on the ground, the
//   sky is all around and there is no rim to follow. What is seen there changes with the direction only, so the
//   far plane is the right answer again, and a point a few metres away would turn every step into a parallax
//   which the sky does not have.
//
// The includer declares struct View before, and defines BACKGROUND_VELOCITY_SET and BACKGROUND_VELOCITY_BINDING as
// the place of the list of the atmospheres, which TpvScene3DAtmospheres.GetGPUMotions fills.

#define MAX_ATMOSPHERE_MOTIONS 16

struct AtmosphereMotion {
  mat4 inverseTransform;   // world space of this frame into the space of the atmosphere
  mat4 previousTransform;  // the space of the atmosphere into world space of the previous frame
  vec4 rayleighExtinction; // xyz = extinction at the bottom, w = scale of the exponent of the density over the altitude
  vec4 mieExtinction;      // the same for the Mie part
  vec4 radii;              // x = bottom radius, y = top radius, z = density scale, w = unused
};

layout(set = BACKGROUND_VELOCITY_SET, binding = BACKGROUND_VELOCITY_BINDING, std140) uniform uboAtmosphereMotions {
  uvec4 countAtmosphereMotions; // x = count, yzw = unused
  AtmosphereMotion atmosphereMotions[MAX_ATMOSPHERE_MOTIONS];
} uAtmosphereMotions;

// Optical depth of one exponential density profile along a whole chord, whose closest approach to the centre is
// at the given radius and altitude. Along the chord the density falls off as a Gaussian of the distance from that
// point, with the radius times the scale height as its variance, so that its integral is a square root away. A
// profile without any falloff fills the whole chord evenly.
vec3 BackgroundVelocityChordOpticalDepth(const in vec4 extinction, const in float closestRadius, const in float closestAltitude, const in float chordLength){
  return extinction.xyz * ((extinction.w < -1e-6) ? (exp(extinction.w * closestAltitude) * sqrt((6.28318530718 * closestRadius) / -extinction.w)) : chordLength);
}

vec2 GetBackgroundVelocity(const in View view, const in View previousView, const in vec2 clipSpacePosition, const in float farDepth, const in vec4 jitter){

  vec2 currentPosition = clipSpacePosition - jitter.xy;

  // The motion of the camera alone. Unprojected with the current view and projected again with the previous one,
  // a point at an infinite far plane is a direction and takes the rotation of the camera only.
  vec2 velocity = vec2(0.0);
  {
    vec4 worldSpacePosition = view.inverseViewMatrix * (view.inverseProjectionMatrix * vec4(clipSpacePosition, farDepth, 1.0));
    vec4 previousClipSpacePosition = previousView.projectionMatrix * (previousView.viewMatrix * worldSpacePosition);
    if(abs(previousClipSpacePosition.w) > 1e-7){
      velocity = (currentPosition - ((previousClipSpacePosition.xy / previousClipSpacePosition.w) - jitter.zw)) * 0.5;
    }
  }

  uint countAtmospheres = min(uAtmosphereMotions.countAtmosphereMotions.x, uint(MAX_ATMOSPHERE_MOTIONS));

  if(countAtmospheres > 0u){

    // The ray of the pixel, from the camera through its point on the near plane, which is finite for every kind of
    // projection, unlike the one on the far plane.
    vec3 rayOrigin = view.inverseViewMatrix[3].xyz;
    vec4 nearWorldSpacePosition = view.inverseViewMatrix * (view.inverseProjectionMatrix * vec4(clipSpacePosition, 1.0 - farDepth, 1.0));
    vec3 rayDirection = normalize((nearWorldSpacePosition.xyz / nearWorldSpacePosition.w) - rayOrigin);

    // Of several atmospheres the one which dominates the pixel wins. They hardly ever overlap on the screen, and a
    // mixture of two motions would be the motion of neither of them.
    float bestWeight = 0.0;
    vec2 bestVelocity = velocity;

    for(uint atmosphereIndex = 0u; atmosphereIndex < countAtmospheres; atmosphereIndex++){

      AtmosphereMotion atmosphereMotion = uAtmosphereMotions.atmosphereMotions[atmosphereIndex];

      float bottomRadius = atmosphereMotion.radii.x;
      float topRadius = atmosphereMotion.radii.y;

      // In the space of the atmosphere, which is centred on its planet and in the units of its radii.
      vec3 origin = (atmosphereMotion.inverseTransform * vec4(rayOrigin, 1.0)).xyz;
      vec3 direction = (atmosphereMotion.inverseTransform * vec4(rayDirection, 0.0)).xyz;
      float directionLength = length(direction);

      if(directionLength > 1e-12){

        direction /= directionLength;

        // Distance along the ray to the closest approach to the centre, and how far from the centre that is.
        float closestDistance = -dot(origin, direction);
        float closestRadiusSquared = max(dot(origin, origin) - (closestDistance * closestDistance), 0.0);

        float halfChordSquared = (topRadius * topRadius) - closestRadiusSquared;

        // Only a ray which passes through the atmosphere in front of the camera.
        if((halfChordSquared > 0.0) && ((closestDistance + sqrt(halfChordSquared)) > 0.0)){

          float halfChord = sqrt(halfChordSquared);

          float closestRadius = sqrt(closestRadiusSquared);

          // The point whose motion is taken. A ray which runs into the ground of the atmosphere, where no planet was
          // drawn, ends there.
          float distance = closestDistance;
          float groundSquared = (bottomRadius * bottomRadius) - closestRadiusSquared;
          if(groundSquared > 0.0){
            distance = min(distance, closestDistance - sqrt(groundSquared));
          }

          if(distance > 0.0){

            // Opacity along the chord, by the strongest colour channel. Below the ground the density is that of the
            // ground.
            float closestAltitude = max(closestRadius - bottomRadius, 0.0);
            float densityRadius = max(closestRadius, bottomRadius);
            vec3 opticalDepth = (BackgroundVelocityChordOpticalDepth(atmosphereMotion.rayleighExtinction, densityRadius, closestAltitude, 2.0 * halfChord) +
                                 BackgroundVelocityChordOpticalDepth(atmosphereMotion.mieExtinction, densityRadius, closestAltitude, 2.0 * halfChord)) * atmosphereMotion.radii.z;
            float opacity = 1.0 - exp(-max(max(opticalDepth.x, opticalDepth.y), opticalDepth.z));

            // How far the point lies in front of the camera, or behind where the ray enters the atmosphere when the
            // camera is outside, in widths of the Gaussian. Beyond two of them it is a rim seen from afar, below
            // that the camera is in the middle of what it looks at.
            float scaleHeight = (atmosphereMotion.rayleighExtinction.w < -1e-6) ? (-1.0 / atmosphereMotion.rayleighExtinction.w) : (topRadius - bottomRadius);
            float width = sqrt(densityRadius * scaleHeight);
            float inFront = smoothstep(0.0, 2.0, min(distance, halfChord) / max(width, 1e-6));

            float weight = opacity * inFront;

            if(weight > bestWeight){
              vec4 previousWorldSpacePosition = atmosphereMotion.previousTransform * vec4(origin + (direction * distance), 1.0);
              vec4 previousClipSpacePosition = previousView.projectionMatrix * (previousView.viewMatrix * previousWorldSpacePosition);
              if(previousClipSpacePosition.w > 1e-7){
                bestWeight = weight;
                bestVelocity = (currentPosition - ((previousClipSpacePosition.xy / previousClipSpacePosition.w) - jitter.zw)) * 0.5;
              }
            }

          }

        }

      }

    }

    velocity = mix(velocity, bestVelocity, bestWeight);

  }

  return velocity;

}

#endif // BACKGROUND_VELOCITY_GLSL
