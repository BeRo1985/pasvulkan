#ifndef OCTAHEDRAL_GLSL
#define OCTAHEDRAL_GLSL

#define OCT_EQUAL_AREA_FOR_PLANET

vec2 octNonEqualAreaSignedEncode(vec3 vector) {
  vector = normalize(vector); // just for to make sure that it is normalized
#if 1
  vec2 result = vector.xy / (abs(vector.x) + abs(vector.y) + abs(vector.z));
  return (vector.z < 0.0) ? ((1.0 - abs(result.yx)) * vec2((result.x >= 0.0) ? 1.0 : -1.0, (result.y >= 0.0) ? 1.0 : -1.0)) : result;
#else
  vector /= abs(vector.x) + abs(vector.y) + abs(vector.z);
  vec2 t = fma(step(vec2(0.0), vector.xy), vec2(2.0), vec2(-1.0));
  return (vector.z < 0.0) ? fma(abs(vector.yx), -t, t) : vector.xy;
#endif
}

vec2 octNonEqualAreaUnsignedEncode(vec3 vector) {
  return fma(octNonEqualAreaSignedEncode(vector), vec2(0.5), vec2(0.5));
}

vec3 octNonEqualAreaSignedDecode(vec2 uv) {
#if 1
  vec3 v = vec3(uv.xy, 1.0 - (abs(uv.x) + abs(uv.y)));
  return normalize((v.z < 0.0) ? vec3((1.0 - abs(v.yx)) * vec2((v.x >= 0.0) ? 1.0 : -1.0, (v.y >= 0.0) ? 1.0 : -1.0), v.z) : v);
#else
  vec2 absUV = abs(uv);
  float l = absUV.x + absUV.y;
  uv = (l > 1.0) ? ((vec2(1.0) - absUV.yx) * fma(step(vec2(0.0), uv), vec2(2.0), vec2(-1.0))) : uv;
  return normalize(vec3(uv.xy, 1.0 - l));
#endif
}

vec3 octNonEqualAreaUnsignedDecode(vec2 uv) {
  return octNonEqualAreaSignedDecode(fma(uv, vec2(2.0), vec2(-1.0)));
}

vec2 octEqualAreaSignedEncode(vec3 vector){
  const float oneOverHalfPi = 0.6366197723675814;
  vector = normalize(vector); // just for to make sure that it is normalized
  vec3 absVector = abs(vector);
#if 1
  vec2 phiTheta = vec2(atan(absVector.x, max(absVector.y, 1e-17)), atan(length(absVector.xy), absVector.z)) * oneOverHalfPi;
#else
  vec2 phiTheta = vec2(acos(normalize(absVector.xy).y), acos(absVector.z)) * oneOverHalfPi;
#endif
  vec2 s = fma(step(vec2(0.0), vector.xy), vec2(2.0), vec2(-1.0));
  vec2 uv = s.xy * vec2(phiTheta.x, 1.0 - phiTheta.x) * phiTheta.y;
  return (vector.z < 0.0) ? fma(abs(uv.yx), -s, s) : uv;
}

vec2 octEqualAreaUnsignedEncode(vec3 vector){
  return fma(octEqualAreaSignedEncode(vector), vec2(0.5), vec2(0.5));
}

vec3 octEqualAreaSignedDecode(vec2 uv){
  const float halfPI = 1.5707963267948966;
  vec2 absUV = abs(uv);
  float absUVSum = absUV.x + absUV.y;
  vec2 s = fma(step(vec2(0.0), uv), vec2(2.0), vec2(-1.0));
  uv = (absUVSum > 1.0) ? ((vec2(1.0) - abs(uv.yx)) * s) : uv;
  vec4 phiThetaSinCos = sin(vec2(vec2(abs(uv.x) / max(1e-17, abs(uv.x) + abs(uv.y)), absUVSum) * halfPI).xxyy + vec2(0.0, halfPI).xyxy); 
  return normalize(vec3(phiThetaSinCos.xy * phiThetaSinCos.zz * s.xy, phiThetaSinCos.w));
}

vec3 octEqualAreaUnsignedDecode(vec2 uv){
  return octEqualAreaSignedDecode(fma(uv, vec2(2.0), vec2(-1.0)));
}

#ifdef OCT_EQUAL_AREA_AS_DEFAULT
  #define octEncode octEqualAreaSignedEncode
  #define octDecode octEqualAreaSignedDecode
  #define octSignedEncode octEqualAreaSignedEncode
  #define octSignedDecode octEqualAreaSignedDecode
  #define octUnsignedEncode octEqualAreaUnsignedEncode
  #define octUnsignedDecode octEqualAreaUnsignedDecode
#else
  #define octEncode octNonEqualAreaSignedEncode
  #define octDecode octNonEqualAreaSignedDecode
  #define octSignedEncode octNonEqualAreaSignedEncode
  #define octSignedDecode octNonEqualAreaSignedDecode
  #define octUnsignedEncode octNonEqualAreaUnsignedEncode
  #define octUnsignedDecode octNonEqualAreaUnsignedDecode
#endif

#ifdef OCT_EQUAL_AREA_FOR_PLANET
  #define octPlanetEncode octEqualAreaSignedEncode
  #define octPlanetDecode octEqualAreaSignedDecode
  #define octPlanetSignedEncode octEqualAreaSignedEncode
  #define octPlanetSignedDecode octEqualAreaSignedDecode
  #define octPlanetUnsignedEncode octEqualAreaUnsignedEncode
  #define octPlanetUnsignedDecode octEqualAreaUnsignedDecode
#else
  #define octPlanetEncode octNonEqualAreaSignedEncode
  #define octPlanetDecode octNonEqualAreaSignedDecode
  #define octPlanetSignedEncode octNonEqualAreaSignedEncode
  #define octPlanetSignedDecode octNonEqualAreaSignedDecode
  #define octPlanetUnsignedEncode octNonEqualAreaUnsignedEncode
  #define octPlanetUnsignedDecode octNonEqualAreaUnsignedDecode
#endif

#endif
