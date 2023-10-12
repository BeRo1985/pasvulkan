#ifndef RGB9E5_GLSL
#define RGB9E5_GLSL

uint encodeRGB9E5(vec3 color){
  color = clamp(color, 0.0, 65408.0);
  float maxChannel = max(max(color.x, color.y), color.z);
  int sharedExponent = max((int((uint(floatBitsToUint(maxChannel) & 0x7f800000u) >> 23u)) - 127) + 16, -16);
  float denominator = exp2(float(sharedExponent - 25));  
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
    float(bitfieldExtract(color, 0u, 9u)),
    float(bitfieldExtract(color, 9u, 9u)),
    float(bitfieldExtract(color, 18u, 9u))
  ) * exp2(float(bitfieldExtract(color, 27u, 5u) - 25));
}

#endif