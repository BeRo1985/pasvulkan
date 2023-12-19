#ifndef OCTAHEDRAL_GLSL
#define OCTAHEDRAL_GLSL

#define OCT_PLANET_BASE_AXIS_X 0
#define OCT_PLANET_BASE_AXIS_Y 1
#define OCT_PLANET_BASE_AXIS_Z 2

#define OCT_PLANET_BASE_AXIS OCT_PLANET_BASE_AXIS_Z

#define OCT_EQUAL_AREA_FOR_PLANET

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
  vector = normalize(vector); // just for to make sure that it is normalized
  const float oneOverHalfPi = 0.6366197723675814;
#if 1
  vec2 uv = vec2(sqrt(1.0 - abs(vector.z)));
  uv.y *= atan(abs(vector.y), max(1e-17, abs(vector.x))) * oneOverHalfPi;
  uv.x -= uv.y;
  return ((vector.z < 0.0) ? (vec2(1.0) - uv.yx) : uv.xy) * vec2((vector.x < 0.0) ? -1.0 : 1.0, (vector.y < 0.0) ? -1.0 : 1.0);
#elif 0
  vec3 absVector = abs(vector);
  vec2 phiTheta = vec2(atan(absVector.x, max(1e-17, absVector.y)) * oneOverHalfPi, sqrt(1.0 - absVector.z));
  vec2 s = fma(step(vec2(0.0), vector.xy), vec2(2.0), vec2(-1.0));
  vec2 uv = s.xy * vec2(phiTheta.x, 1.0 - phiTheta.x) * phiTheta.y;
  return (vector.z < 0.0) ? fma(abs(uv.yx), -s, s) : uv;
#else 
  vec3 absVector = abs(vector);
  vec2 phiTheta = vec2(atan(absVector.x, max(1e-17, absVector.y)), acos(absVector.z)) * oneOverHalfPi;
  vec2 s = fma(step(vec2(0.0), vector.xy), vec2(2.0), vec2(-1.0));
  vec2 uv = s.xy * vec2(phiTheta.x, 1.0 - phiTheta.x) * phiTheta.y;
  return (vector.z < 0.0) ? fma(abs(uv.yx), -s, s) : uv;
#endif
}

vec2 octEqualAreaUnsignedEncode(vec3 vector){
  return fma(octEqualAreaSignedEncode(vector), vec2(0.5), vec2(0.5));
}

vec3 octEqualAreaSignedDecode(vec2 uv){
  const float halfPI = 1.5707963267948966;
  vec2 absUV = abs(uv);
#if 1
  const float PIover4 = 0.7853981633974483;
  float d = 1.0 - (absUV.x + absUV.y), r = 1.0 - abs(d);
  vec2 phiCosSin = sin(vec2((r != 0.0) ? (((absUV.y - absUV.x) / max(1e-17, r)) + 1.0) * PIover4 : 0.0) + vec2(halfPI, 0.0));
  return normalize(vec3(abs(phiCosSin * (r * sqrt(2.0 - (r * r)))) * vec2((uv.x < 0.0) ? -1.0 : 1.0, (uv.y < 0.0) ? -1.0 : 1.0), (1.0 - (r * r)) * ((d < 0.0) ? -1.0 : 1.0)));  
#elif 0 
  float absUVSum = absUV.x + absUV.y;
  vec2 s = fma(step(vec2(0.0), uv), vec2(2.0), vec2(-1.0));
  uv = (absUVSum > 1.0) ? ((vec2(1.0) - abs(uv.yx)) * s) : uv;
  float d = 1.0 - absUVSum, r = 1.0 - abs(d);   
  vec4 phiThetaSinCos = vec4(sin(vec2((abs(uv.x) / max(1e-17, abs(uv.x) + abs(uv.y))) * halfPI) + vec2(0.0, halfPI)), r * sqrt(2.0 - (r * r)), 1.0 - (r * r)); 
  return normalize(vec3(phiThetaSinCos.xy * phiThetaSinCos.zz * s.xy, (d < 0.0) ? -phiThetaSinCos.w : phiThetaSinCos.w));
 #else
  float absUVSum = absUV.x + absUV.y;
  vec2 s = fma(step(vec2(0.0), uv), vec2(2.0), vec2(-1.0));
  uv = (absUVSum > 1.0) ? ((vec2(1.0) - abs(uv.yx)) * s) : uv;
  vec4 phiThetaSinCos = sin(vec2(vec2(abs(uv.x) / max(1e-17, abs(uv.x) + abs(uv.y)), absUVSum) * halfPI).xxyy + vec2(0.0, halfPI).xyxy); 
  return normalize(vec3(phiThetaSinCos.xy * phiThetaSinCos.zz * s.xy, phiThetaSinCos.w));
#endif
}

vec3 octEqualAreaUnsignedDecode(vec2 uv){
  return octEqualAreaSignedDecode(fma(uv, vec2(2.0), vec2(-1.0)));
}

#if (OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_X) || (OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_Y)

vec2 octPlanetSignedEncode(vec3 vector){
#if OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_X
  vector = vector.zxy;
#elif OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_Y
  vector = vector.yzx; 
#elif OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_Z
  // Nothing to do in this case
#endif
#ifdef OCT_EQUAL_AREA_FOR_PLANET
  return octEqualAreaSignedEncode(vector);
#else
  return octNonEqualAreaSigneEncode(vector);
#endif
}

vec2 octPlanetUnsignedEncode(vec3 vector){
  return fma(octPlanetSignedEncode(vector), vec2(0.5), vec2(0.5));
}

vec3 octPlanetSignedDecode(vec2 uv){
#ifdef OCT_EQUAL_AREA_FOR_PLANET
  vec3 vector = octEqualAreaSignedDecode(uv);
#else
  vec3 vector = octNonEqualAreaSignedDecode(uv);
#endif
#if OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_X
  return vector.yzx;
#elif OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_Y
  return vector.zxy;
#elif OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_Z
  return vector;
#endif
}

vec3 octPlanetUnsignedDecode(vec2 uv){
  return octPlanetSignedDecode(fma(uv, vec2(2.0), vec2(-1.0)));
}

#define octPlanetDecode octPlanetSignedDecode
#define octPlanetEncode octPlanetSignedEncode

#elif OCT_PLANET_BASE_AXIS == OCT_PLANET_BASE_AXIS_Z

  #ifdef OCT_EQUAL_AREA_FOR_PLANET
    #define octPlanetDecode octEqualAreaSignedDecode    
    #define octPlanetEncode octEqualAreaSignedEncode
    #define octPlanetUnsignedDecode octEqualAreaUnsignedDecode
    #define octPlanetUnsignedEncode octEqualAreaUnsignedEncode
    #define octPlanetSignedDecode octEqualAreaSignedDecode
    #define octPlanetSignedEncode octEqualAreaSignedEncode
  #else
    #define octPlanetDecode octNonEqualAreaSignedDecode
    #define octPlanetEncode octNonEqualAreaSignedEncode
    #define octPlanetUnsignedDecode octNonEqualAreaUnsignedDecode
    #define octPlanetUnsignedEncode octNonEqualAreaUnsignedEncode
    #define octPlanetSignedDecode octNonEqualAreaSignedDecode
    #define octPlanetSignedEncode octNonEqualAreaSignedEncode
  #endif

#endif

#endif
