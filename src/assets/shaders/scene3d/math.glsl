#ifndef MATH_GLSL
#define MATH_GLSL

const float PI = 3.14159265358979323846;
const float PI2 = 6.283185307179586476925286766559;
const float OneOverPI = 0.3183098861837907; //1.0 / PI;

float sq(float t){
  return t * t; //
}

vec2 sq(vec2 t){
  return t * t; //
}

vec3 sq(vec3 t){
  return t * t; //
}

vec4 sq(vec4 t){
  return t * t; //
}

float pow2(float t){
  return t * t; //
}

vec2 pow2(vec2 t){
  return t * t; //
}

vec3 pow2(vec3 t){
  return t * t; //
}

vec4 pow2(vec4 t){
  return t * t; //
}

float pow4(float t){
  return t * t * t * t;  
}

vec2 pow4(vec2 t){
  return t * t * t * t;  
}

vec3 pow4(vec3 t){
  return t * t * t * t;  
}

vec4 pow4(vec4 t){
  return t * t * t * t;  
}

#endif