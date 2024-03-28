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

#endif