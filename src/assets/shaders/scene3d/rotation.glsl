#ifndef ROTATION_GLSL
#define ROTATION_GLSL

/*
vec3 rotate(const in vec3 v, const in vec3 axis, const in float angle){
  vec2 sc = vec2(sin(vec2(angle) + vec2(0.0, 1.57079632679))); 
  vec3 a = cross(axis, v);
  return fma(a, vec3(sc.x), fma(cross(axis, a), vec3(1.0 - sc.y), v));
} 
*/

vec3 rotate(const in vec3 v, const in vec3 axis, const in float angle){
  vec2 sc = vec2(sin(vec2(angle) + vec2(0.0, 1.57079632679)));  
  return fma(cross(axis, v), vec3(sc.x), mix(dot(v, axis) * axis, v, sc.y));
}

vec3 rotate(const in vec3 v, const in vec4 axisAngle){
  vec2 sc = vec2(sin(vec2(axisAngle.w) + vec2(0.0, 1.57079632679)));  
  return fma(cross(axisAngle.xyz, v), vec3(sc.x), mix(dot(v, axisAngle.xyz) * axisAngle.xyz, v, sc.y));
}

vec3 rotateX(const in vec3 p, const in float a){
  vec2 sc = vec2(sin(vec2(a) + vec2(0.0, 1.57079632679))); 
  return vec3(p.x, (sc.y * p.y) - (sc.x * p.z), (sc.x * p.y) + (sc.y * p.z));
}

vec3 rotateY(const in vec3 p, const in float a){
  vec2 sc = vec2(sin(vec2(a) + vec2(0.0, 1.57079632679))); 
  return vec3((sc.y * p.x) + (sc.x * p.z), p.y, (sc.y * p.z) - (sc.x * p.x));
}

vec3 rotateZ(const in vec3 p, const in float a){
  vec2 sc = vec2(sin(vec2(a) + vec2(0.0, 1.57079632679))); 
  return vec3((sc.y * p.x) + (sc.x * p.y), (sc.y * p.y) - (sc.x * p.x), p.z);
}

mat3 getVectorAlignmentMatrix(const in vec3 fromVector, const in vec3 toVector, inout float inversion){
	float c = dot(fromVector, toVector);
  inversion *= (c < 0.0) ? -1.0 : 1.0;
	vec3 v = cross(fromVector, toVector);
	mat3 m = mat3(0.0, v.z, -v.y, -v.z, 0.0, v.x, v.y, -v.x, 0.0);	
	float s = length(v);
  s *= s;
	return (mat3(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0) + m) + ((m * m) * ((1.0 - c) / (s * s)));
}

#endif