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

vec2 rectNormalToOctahedral(vec3 normal){
  normal = normalize(normal); // just for to make sure that it is normalized
  normal /= abs(normal.x) + abs(normal.y) + abs(normal.z);
  vec2 t = fma(step(vec2(0.0), normal.xz), vec2(2.0), vec2(-1.0));
  return fma((normal.y > 0.0) ? normal.xz : fma(abs(normal.zx), -t, t), vec2(0.5), vec2(0.5));
}

vec3 octahedralToRectNormal(vec2 uv){
  uv = fma(uv, vec2(2.0), vec2(-1.0)); 
  vec2 absUV = abs(uv);
  float l = absUV.x + absUV.y;
  uv = (l > 1.0) ? ((vec2(1.0) - absUV.yx) * fma(step(vec2(0.0), uv), vec2(2.0), vec2(-1.0))) : uv;
  return normalize(vec3(uv.x, 1.0 - l, uv.y));
}

vec2 sphereNormalToOctahedral(vec3 normal){
  const float oneOverHalfPi = 0.6366197723675814;
  normal = normalize(normal); // just for to make sure that it is normalized
  vec3 absNormal = abs(normal);
#if 1
  vec2 pitchYaw = vec2(atan(length(absNormal.xz), absNormal.y), atan(absNormal.x, max(absNormal.z, 1e-17))) * oneOverHalfPi;
#else
  vec2 pitchYaw = vec2(acos(absNormal.y), acos(normalize(absNormal.xz).y)) * oneOverHalfPi;
#endif
  vec2 s = fma(step(vec2(0.0), normal.xz), vec2(2.0), vec2(-1.0));
  vec2 uv = s.xy * vec2(pitchYaw.y, 1.0 - pitchYaw.y) * pitchYaw.x;
  return fma((normal.y < 0.0) ? fma(abs(uv.yx), -s, s) : uv, vec2(0.5), vec2(0.5));
}

vec3 octahedralToSphereNormal(vec2 uv){
  const float halfPI = 1.5707963267948966;
  uv = fma(uv, vec2(2.0), vec2(-1.0)); 
  vec2 absUV = abs(uv);
  float absUVSum = absUV.x + absUV.y;
  vec2 s = fma(step(vec2(0.0), uv), vec2(2.0), vec2(-1.0));
  uv = (absUVSum > 1.0) ? ((vec2(1.0) - abs(uv.yx)) * s) : uv;
  vec4 pitchYawSinCos = sin(vec2(vec2(absUVSum, abs(uv.x) / max(1e-17, abs(uv.x) + abs(uv.y))) * halfPI).xxyy + vec2(0.0, halfPI).xyxy); 
  return normalize(vec3(pitchYawSinCos.xx * pitchYawSinCos.zw * s.xy, pitchYawSinCos.y).xzy);
}

#endif
