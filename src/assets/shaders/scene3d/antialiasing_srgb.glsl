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
  return ApplyInverseToneMapping(convertSRGBToLinearRGB(color));
}

vec4 SRGBawareTexture(sampler2DArray tex, vec3 texCoord, float lod) {
  
  int intlod = int(lod);

  ivec2 texSize = textureSize(tex, intlod).xy;
  
  texCoord.xy *= vec2(texSize);
  
  vec2 texelCenter = floor(texCoord.xy - vec2(0.5)) + vec2(0.5);
  
  vec2 fracTexCoords = texCoord.xy - texelCenter;

  texCoord.xy = texelCenter / vec2(texSize);
  
  vec4 t0 = SRGBin(textureOffset(tex, texCoord, ivec2(0, 0), lod));
  vec4 t1 = SRGBin(textureOffset(tex, texCoord, ivec2(1, 0), lod));
  vec4 t2 = SRGBin(textureOffset(tex, texCoord, ivec2(0, 1), lod));
  vec4 t3 = SRGBin(textureOffset(tex, texCoord, ivec2(1, 1), lod));

  vec4 r = SRGBout(mix(mix(t0, t1, fracTexCoords.x), mix(t2, t3, fracTexCoords.x), fracTexCoords.y));
  return r;
}
 
vec4 SRGBGammaCorrectedTexture(sampler2DArray tex, vec3 texCoord, float lod) {

  int intlod = int(lod);

  ivec2 texSize = textureSize(tex, intlod).xy;
  
  texCoord.xy *= vec2(texSize);
  
  vec2 texelCenter = floor(texCoord.xy - vec2(0.5)) + vec2(0.5);
  
  vec2 fracTexCoords = texCoord.xy - texelCenter;

  texCoord.xy = texelCenter / vec2(texSize);
  
  vec4 t0 = SRGBin(textureOffset(tex, texCoord, ivec2(0, 0), lod));
  vec4 t1 = SRGBin(textureOffset(tex, texCoord, ivec2(1, 0), lod));
  vec4 t2 = SRGBin(textureOffset(tex, texCoord, ivec2(0, 1), lod));
  vec4 t3 = SRGBin(textureOffset(tex, texCoord, ivec2(1, 1), lod));

  vec4 r = mix(mix(t0, t1, fracTexCoords.x), mix(t2, t3, fracTexCoords.x), fracTexCoords.y);
  return r;
}
 
vec4 SRGBGammaCorrectedTextureOffset(sampler2DArray tex, vec3 texCoord, float lod, ivec2 offset) {

  int intlod = int(lod);

  ivec2 texSize = textureSize(tex, intlod).xy;
  
  texCoord.xy *= vec2(texSize);
  
  vec2 texelCenter = floor(texCoord.xy - vec2(0.5)) + vec2(0.5) + vec2(offset);
  
  vec2 fracTexCoords = texCoord.xy - texelCenter;

  texCoord.xy = texelCenter / vec2(texSize);
  
  vec4 t0 = SRGBin(textureOffset(tex, texCoord, ivec2(0, 0), lod));
  vec4 t1 = SRGBin(textureOffset(tex, texCoord, ivec2(1, 0), lod));
  vec4 t2 = SRGBin(textureOffset(tex, texCoord, ivec2(0, 1), lod));
  vec4 t3 = SRGBin(textureOffset(tex, texCoord, ivec2(1, 1), lod));

  vec4 r = mix(mix(t0, t1, fracTexCoords.x), mix(t2, t3, fracTexCoords.x), fracTexCoords.y);
  return r;
}
 
#endif 