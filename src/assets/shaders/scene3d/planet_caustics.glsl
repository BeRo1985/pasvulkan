#ifndef PLANET_CAUSTICS_GLSL
#define PLANET_CAUSTICS_GLSL

// Evaluates one caustic layer: two cross-coupled sinusoidal grids multiplied
// together so bright spikes appear only where both crests coincide.
float getCausticLayer(vec3 p, float t) {
  return sin(p.x + (sin((p.z * 0.67) + (t * 0.81)) * 0.55) + (t * 0.53)) *
         cos(p.z + (sin((p.x * 0.73) + (t * 1.07)) * 0.55) + (t * 0.71));
}

// Returns a [0, 1] caustic intensity for a planet-local position.
// pos:       planet-local 3D fragment position
// time:      current time in seconds
// scale:     spatial frequency (inverse position units); larger = finer pattern
// speed:     animation speed multiplier
// fadeDepth: depth (position units) at which intensity falls to 1/e (~0.37)
// waterDepth: water column height at this point; <= 0 means dry
float getCausticIntensity(vec3 pos, float time, float scale, float speed, float fadeDepth, float waterDepth) {
  float result = exp(-waterDepth / max(fadeDepth, 0.01));
  if(result >= 1e-3){
    vec3 p = pos * scale;
    float t = time * speed;
    float c1 = getCausticLayer(p, t);
    float c2 = getCausticLayer((p * 1.37) + vec3(7.31, 0.0, 2.13), (t * 0.83) + 1.37);
    result *= pow(max(0.0, (c1 + c2) * 0.5), 3.0);
  }
  return result;
}

#endif // PLANET_CAUSTICS_GLSL
