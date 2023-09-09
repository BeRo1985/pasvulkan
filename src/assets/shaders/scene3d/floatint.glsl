#ifndef FLOATINT_GLSL
#define FLOATINT_GLSL

// Convert a float to a uint, preserving order.
uint mapFloat(float value){
  uint temporary = floatBitsToUint(value);
  return temporary ^ (uint(uint(-int(uint(temporary >> 31u)))) | 0x80000000u);
}

// Convert a uint to a float, preserving order.
float unmapFloat(uint value){
  return uintBitsToFloat(value ^ (((value >> 31u) - 1u) | 0x80000000u));
}

#endif 