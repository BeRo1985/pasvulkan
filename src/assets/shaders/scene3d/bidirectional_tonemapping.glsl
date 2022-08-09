#ifndef BIDIRECTIONAL_TONEMAPPING_GLSL
#define BIDIRECTIONAL_TONEMAPPING_GLSL

// Bidirectional temporal tonemapping for antialiasing, upsampling, downsampling, and so on.

float Luminance(vec3 color){
    return dot(color, vec3(0.2125, 0.7154, 0.0721));
}

vec3 ApplyToneMapping(vec3 color){
  return color / (Luminance(color) + 1.0);
}

vec3 ApplyInverseToneMapping(vec3 color){
  return color / max(1.0 - Luminance(color), 1e-4);
}

vec4 ApplyToneMapping(vec4 color){
  return vec4(vec3(ApplyToneMapping(color.xyz)), color.w);
}

vec4 ApplyInverseToneMapping(vec4 color){
  return vec4(vec3(ApplyInverseToneMapping(color.xyz)), color.w);
}

#endif