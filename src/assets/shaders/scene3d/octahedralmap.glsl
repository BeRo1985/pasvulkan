#ifndef OCTAHEDRALMAP_GLSL
#define OCTAHEDRALMAP_GLSL

#include "textureutils.glsl"

ivec2 wrapOctahedralTexelCoordinates(const in ivec2 texel, const in ivec2 texSize) {
  ivec2 wrapped = ((texel % texSize) + texSize) % texSize;
  return ((((texel.x / texSize.x) + (texel.y / texSize.y)) & 1) != 0) ? (texSize - (wrapped + ivec2(1))) : wrapped;
}

vec4 textureOctahedralMap(const in sampler2D tex, vec3 direction) {
  direction = normalize(direction); // just for to make sure that it is normalized 
  vec2 uv = direction.xy / (abs(direction.x) + abs(direction.y) + abs(direction.z));
  uv = fma((direction.z < 0.0) ? ((1.0 - abs(uv.yx)) * vec2((uv.x >= 0.0) ? 1.0 : -1.0, (uv.y >= 0.0) ? 1.0 : -1.0)) : uv, vec2(0.5), vec2(0.5));
  ivec2 texSize = textureSize(tex, 0).xy;
  vec2 invTexSize = vec2(1.0) / vec2(texSize);
  if(any(lessThanEqual(uv, invTexSize)) || any(greaterThanEqual(uv, vec2(1.0) - invTexSize))){
   // Handle edges with manual bilinear interpolation using texelFetch for correct octahedral texel edge mirroring 
   uv *= texSize;
   ivec2 baseCoord = ivec2(floor(uv));
   vec2 fractionalPart = uv - vec2(baseCoord);
   return mix(mix(texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(0, 0), texSize), 0), 
                  texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(1, 0), texSize), 0), fractionalPart.x), 
              mix(texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(0, 1), texSize), 0), 
                  texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(1, 1), texSize), 0), fractionalPart.x), fractionalPart.y);
  }else{
    // Non-edge texels can be sampled directly with textureLod
    return textureLod(tex, uv, 0.0);
  }
}

vec4 textureCatmullRomOctahedralMap(const in sampler2D tex, vec3 direction) {
  direction = normalize(direction); // just for to make sure that it is normalized 
  vec2 uv = direction.xy / (abs(direction.x) + abs(direction.y) + abs(direction.z));
  uv = fma((direction.z < 0.0) ? ((1.0 - abs(uv.yx)) * vec2((uv.x >= 0.0) ? 1.0 : -1.0, (uv.y >= 0.0) ? 1.0 : -1.0)) : uv, vec2(0.5), vec2(0.5));
  ivec2 texSize = textureSize(tex, 0).xy;
  vec2 invTexSize = vec2(1.0) / vec2(texSize);
  if(any(lessThanEqual(uv, invTexSize)) || any(greaterThanEqual(uv, vec2(1.0) - invTexSize))){
   // Handle edges with manual bilinear interpolation using texelFetch for correct octahedral texel edge mirroring 
   uv *= texSize;
   ivec2 baseCoord = ivec2(floor(uv));
   vec2 fractionalPart = uv - vec2(baseCoord);
   return mix(mix(texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(0, 0), texSize), 0), 
                  texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(1, 0), texSize), 0), fractionalPart.x), 
              mix(texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(0, 1), texSize), 0), 
                  texelFetch(tex, wrapOctahedralTexelCoordinates(baseCoord + ivec2(1, 1), texSize), 0), fractionalPart.x), fractionalPart.y);
  }else{
    // Non-edge texels can be sampled directly with textureLod
    return textureCatmullRom(tex, uv, 0);
  }
}

#endif
