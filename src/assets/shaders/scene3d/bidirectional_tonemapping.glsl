#ifndef BIDIRECTIONAL_TONEMAPPING_GLSL
#define BIDIRECTIONAL_TONEMAPPING_GLSL

// Bidirectional temporal tonemapping for antialiasing, upsampling, downsampling, and so on.

vec3 ApplyToneMapping(vec3 color){
#if 0
  // ACESFilmic - https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
  return (color * ((2.51 * color) + vec3(0.03))) / (color * ((2.43 * color) + vec3(0.59)) + vec3(0.14));
#elif 1
  return color / (max(max(color.x, color.y), color.z) + 1.0);
#else
  return color / (dot(color, vec3(0.2125, 0.7154, 0.0721)) + 1.0);
#endif
}

vec3 ApplyInverseToneMapping(vec3 color){
#if 0
  // ACESFilmic - https://www.wolframalpha.com/input?i=2.51y%5E2%2B.03y%3Dx%282.43y%5E2%2B.59y%2B.14%29+solve+for+y}
  return (sqrt((-10127.0 * color * color) + (13702.0 * color) + vec3(9.0)) + (59.0 * color) - vec3(3.0)) / (502.0 - vec3(486.0 * color)); 
#elif 1
  return color / max(1.0 - max(max(color.x, color.y), color.z), 1e-5);
#else
  return color / max(1.0 - dot(color, vec3(0.2125, 0.7154, 0.0721)), 1e-5);
#endif
}

vec4 ApplyToneMapping(vec4 color){
  return vec4(vec3(ApplyToneMapping(color.xyz)), color.w);
}

vec4 ApplyInverseToneMapping(vec4 color){
  return vec4(vec3(ApplyInverseToneMapping(color.xyz)), color.w);
}

#endif