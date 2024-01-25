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

float pow3(float t){
  return t * t * t; //
}

vec2 pow3(vec2 t){
  return t * t * t; //
}

vec3 pow3(vec3 t){
  return t * t * t; //
}

vec4 pow3(vec4 t){
  return t * t * t; //
}

float pow4(float t){
  t *= t;
  return t * t; 
}

vec2 pow4(vec2 t){
  t *= t;
  return t * t; 
}

vec3 pow4(vec3 t){
  t *= t;
  return t * t; 
}

vec4 pow4(vec4 t){
  t *= t;
  return t * t; 
}

float linearStep(float a, float b, float v) {
  return clamp((v - a) / (b - a), 0.0, 1.0);  //
}

vec2 linearStep(vec2 a, vec2 b, vec2 v) {
  return clamp((v - a) / (b - a), vec2(0.0), vec2(1.0));  //
}

vec3 linearStep(vec3 a, vec3 b, vec3 v) {
  return clamp((v - a) / (b - a), vec3(0.0), vec3(1.0));  //
}

vec4 linearStep(vec4 a, vec4 b, vec4 v) {
  return clamp((v - a) / (b - a), vec4(0.0), vec4(1.0));  //
}

#endif