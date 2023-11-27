#ifndef OCTAHEDRAL_GLSL
#define OCTAHEDRAL_GLSL

vec2 octEncode(in vec3 v) {
  v = normalize(v); // just for to make sure that it is normalized
  vec2 result = v.xy / (abs(v.x) + abs(v.y) + abs(v.z));
  return (v.z < 0.0) ? ((1.0 - abs(result.yx)) * vec2((result.x >= 0.0) ? 1.0 : -1.0, (result.y >= 0.0) ? 1.0 : -1.0)) : result;
}

vec3 octDecode(vec2 oct) {
  vec3 v = vec3(oct.xy, 1.0 - (abs(oct.x) + abs(oct.y)));
  return normalize((v.z < 0.0) ? vec3((1.0 - abs(v.yx)) * vec2((v.x >= 0.0) ? 1.0 : -1.0, (v.y >= 0.0) ? 1.0 : -1.0), v.z) : v);
}

#endif
