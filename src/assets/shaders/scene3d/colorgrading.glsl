#ifndef COLOR_GRADING_GLSL
#define COLOR_GRADING_GLSL

#include "colortemperature.glsl"

const vec3 LinearRGBLuminanceWeighting = vec3(0.2126, 0.7152, 0.0722); // Rec. 709 / Linear RGB

vec3 applyColorHueRotation(vec3 c, float hueRotationAngle){ 
  vec3 hueRotationValues = vec3(0.57735, sin(vec2(radians(hueRotationAngle)) + vec2(0.0, 1.57079632679)));
  return mix(hueRotationValues.xxx * dot(hueRotationValues.xxx, c), c, hueRotationValues.z) + (cross(hueRotationValues.xxx, c) * hueRotationValues.y);
}

vec3 applyColorSaturation(vec3 c, float saturation){
  return max(vec3(0.0), mix(vec3(dot(c, LinearRGBLuminanceWeighting)), c, saturation));
}

vec3 applyColorContrast(vec3 c, float contrast){
  return mix(vec3(0.5), c, contrast);
}

#endif 