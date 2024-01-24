#ifndef RGB9E5_GLSL
#define RGB9E5_GLSL

#undef REVERSED_RGB9E5

uint encodeRGB9E5(vec3 color){
  color = clamp(color, 0.0, 65408.0);
  float maxChannel = max(max(color.x, color.y), color.z);
  int sharedExponent = max(int((uint(floatBitsToUint(maxChannel) & 0x7f800000u) >> 23u)) - 127, -16) + 16;
  float denominator = exp2(float(sharedExponent - 24));  
  int maximum = int(floor((maxChannel / denominator) + 0.5));
  if(maximum >= 512){
    denominator *= 2.0;
    sharedExponent++;
  }
  uvec3 t = uvec3(floor((color / denominator) + vec3(0.5))) & uvec3(0x1ffu);
#ifdef REVERSED_RGB9E5
  return (t.x << 23u) | (t.y << 14u) | (t.z << 5u) | ((uint(sharedExponent) & 0x1fu) << 0u);
#else
  return t.x | (t.y << 9u) | (t.z << 18u) | (uint(sharedExponent) << 27u);
#endif
}

vec3 decodeRGB9E5(uint color){
#ifdef REVERSED_RGB9E5
  return vec3(
    float(bitfieldExtract(color, 23, 9)),
    float(bitfieldExtract(color, 14, 9)),
    float(bitfieldExtract(color, 5, 9))
  ) * exp2(float(int(bitfieldExtract(color, 0, 5)) - 24));
#else 
  return vec3(
    float(bitfieldExtract(color, 0, 9)),
    float(bitfieldExtract(color, 9, 9)),
    float(bitfieldExtract(color, 18, 9))
  ) * exp2(float(int(bitfieldExtract(color, 27, 5)) - 24));
#endif
}

#endif