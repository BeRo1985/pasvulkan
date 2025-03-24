#ifndef PCG_GLSL
#define PCG_GLSL

uint pcgRandom(inout uint state){
  uint oldstate = state;
  state = (oldstate * 747796405u) + 2891336453u;
  uint xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
  uint rot = oldstate >> 59u;
  return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

uint pcgHash11(const in uint v){
  uint s = (v * 747796405u) + 2891336453u;
  uint w = ((s >> ((s >> 28u) + 4u)) ^ s) * 277803737u;
  return (w >> 22u) ^ w;
}

uint pcgHash21(const in uvec2 v){
  return pcgHash11(v.x + pcgHash11(v.y));
}

uint pcgHash31(const in uvec3 v){
  return pcgHash11(v.x + pcgHash11(v.y + pcgHash11(v.z)));
}

uint pcgHash41(const in uvec4 v){
  return pcgHash11(v.x + pcgHash11(v.y + pcgHash11(v.z + pcgHash11(v.w))));
}

uvec2 pcgHash22(uvec2 v){
  v = (v * 1664525u) + 1013904223u;
  v.x += v.y * 1664525u;
  v.y += v.x * 1664525u;
  v = v ^ (v>>16u);
  v.x += v.y * 1664525u;
  v.y += v.x * 1664525u;
  v = v ^ (v>>16u);
  return v;
}

uvec3 pcgHash33(uvec3 v){
  v = (v * 1664525u) + 1013904223u;
  v.x += v.y * v.z;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  v = v ^ (v >> 16u);
  v.x += v.y * v.z;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  return v;
}

uvec3 pcgHash33_16(uvec3 v){
  v = (v * 12829u) + 47989u;
  v.x += v.y * v.z;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  v.x += v.y * v.z;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  v >>= 16u;
  return v;
}

uvec4 pcgHash44(uvec4 v){
  v = (v * 1664525u) + 1013904223u;
  v.x += v.y * v.w;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  v.w += v.y * v.z;
  v = v ^ (v >> 16u);
  v.x += v.y * v.w;
  v.y += v.z * v.x;
  v.z += v.x * v.y;
  v.w += v.y * v.z;
  return v;
}

float pcgHash(const in vec2 p){
  return uintBitsToFloat(((pcgHash21(floatBitsToUint(p)) >> 9u) & 0x007fffffu) | 0x3f800000u) - 1.0;
}

float pcgTileableNoise(in vec2 p, in float scale){
  p *= scale;
  vec2 f = fract(p);
  p = floor(p);
  f = (f * f) * fma(f, vec2(-2.0), vec2(3.0));
  const vec2 o = vec2(0.0, 1.0);
  return mix(mix(pcgHash(mod(p + o.xx, vec2(scale))), pcgHash(mod(p + o.yx, vec2(scale))), f.x),
             mix(pcgHash(mod(p + o.xy, vec2(scale))), pcgHash(mod(p + o.yy, vec2(scale))), f.x), f.y);    
}

float pcgTileableFBM(in vec2 p, in float scale, in uint octaves){
  vec2 sum = vec2(0.0);
  float amplitude = 1.0;
  float frequency = 1.0;
  for(uint i = 0u; i < octaves; i++){
    sum += vec2(pcgTileableNoise(p * frequency, scale), 1.0) * amplitude;
    amplitude *= 0.5;
    frequency *= 2.0;
  }
  return sum.x / sum.y;
}

#endif