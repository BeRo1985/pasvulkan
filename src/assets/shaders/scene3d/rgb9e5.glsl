#ifndef RGB9E5_GLSL
#define RGB9E5_GLSL

uint encodeRGB9E5(vec3 color){
  color = clamp(color, 0.0, 65408.0);
  float maxChannel = max(max(color.x, color.y), color.z);
  int sharedExponent = max((int((uint(floatBitsToUint(maxChannel) & 0x7f800000u) >> 23u)) - 127) + 16, -16);
  float denominator = exp2(float(sharedExponent - 24));  
  int maximum = int(floor((maxChannel / denominator) + 0.5));
  if(maximum >= 512){
    denominator *= 2.0;
    sharedExponent++;
  }
  uvec3 t = uvec3(floor((color / denominator) + vec3(0.5)));
  return t.x | (t.y << 9u) | (t.z << 18u) | (uint(sharedExponent) << 27u);
}

vec3 decodeRGB9E5(uint color){
  return vec3(
    float(bitfieldExtract(color, 0, 9)),
    float(bitfieldExtract(color, 9, 9)),
    float(bitfieldExtract(color, 18, 9))
  ) * exp2(float(int(bitfieldExtract(color, 27, 5)) - 24));
}

#endif