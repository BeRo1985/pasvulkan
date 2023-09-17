#ifndef ANTIALIASING_SRGB_GLSL
#define ANTIALIASING_SRGB_GLSL

#include "bidirectional_tonemapping.glsl"

vec3 convertLinearRGBToSRGB(vec3 c) {
  return mix((pow(c, vec3(1.0 / 2.4)) * vec3(1.055)) - vec3(5.5e-2), c * vec3(12.92), lessThan(c, vec3(3.1308e-3)));  //
}

vec4 convertLinearRGBToSRGB(vec4 c) {
  return vec4(convertLinearRGBToSRGB(c.xyz), c.w);  //
}

vec3 convertSRGBToLinearRGB(vec3 c) {
  return mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)), c / vec3(12.92), lessThan(c, vec3(4.045e-2)));  //
}

vec4 convertSRGBToLinearRGB(vec4 c) {
  return vec4(convertSRGBToLinearRGB(c.xyz), c.w);  //
}

vec4 SRGBin(vec4 color) {
  return convertLinearRGBToSRGB(ApplyToneMapping(color));
}

vec4 SRGBout(vec4 color) {
  return convertSRGBToLinearRGB(ApplyInverseToneMapping(color));
}

vec4 SRGBawareTextureLOD(sampler2DArray tex, vec3 texCoord, float lod) {
  
  int intlod = int(lod);

  ivec2 texSize = textureSize(tex, intlod).xy;

  vec2 uv_adjusted = fma(texCoord.xy, vec2(texSize), -vec2(0.5));
  vec2 f = fract(uv_adjusted);
  
  ivec2 i0 = ivec2(uv_adjusted);
  ivec2 i1 = i0 + ivec2(1, 0);
  ivec2 i2 = i0 + ivec2(0, 1);
  ivec2 i3 = i0 + ivec2(1, 1);

  vec4 t0 = SRGBin(texelFetch(tex, ivec3(i0, int(texCoord.z)), intlod));
  vec4 t1 = SRGBin(texelFetch(tex, ivec3(i1, int(texCoord.z)), intlod));
  vec4 t2 = SRGBin(texelFetch(tex, ivec3(i2, int(texCoord.z)), intlod));
  vec4 t3 = SRGBin(texelFetch(tex, ivec3(i3, int(texCoord.z)), intlod));

  vec4 r = SRGBout(mix(mix(t0, t1, f.x), mix(t2, t3, f.x), f.y));
  return r;
}
 

#endif 