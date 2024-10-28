#ifndef RGB9E5_GLSL
#define RGB9E5_GLSL

uint encodeRGB9E5(vec3 color){
  color = clamp(color, vec3(0.0), vec3(65408.0));
#if 0

  // Cast the color floats to uint
  const uvec3 castedColor = floatBitsToUint(color.xyz);

  // Extract the exponents
  uvec3 exponents = (castedColor & uvec3(0x7f800000u)) >> uvec3(23u);

  // Extract the mantissas and ORing the explicit 1 at the 9th bit
  uvec3 mantissas = (castedColor & uvec3(0x00ffffffu)) | uvec3(0x00800000u);

  // if the exponent is larger than what can be represented in 999e5, then clamp to the maximum exponanent and mantissa
  bvec3 overflowMask = greaterThan(exponents, uvec3(127u + 15u));
  exponents = mix(exponents, uvec3(127u + 15u), overflowMask);
  mantissas = mix(mantissas, uvec3(0x00ffffffu), overflowMask);

  // Find the largest exponent
  uint maximumExponent = max(max(exponents.x, exponents.y), exponents.z);
  
  // If the largest exponent is smaller than the representable minimum, then clamp to zero
  if(maximumExponent < (127u - 16u)){
    maximumExponent = 127u - 16u;
    mantissas = uvec3(0u);
  }

  // Rounding, adds half lsb before truncating the value
  mantissas += (uvec3(0x00004000u)) << (uvec3(maximumExponent) - exponents);

  // Shift the mantissas to the correct position, based on the difference between the max exponent and the component's exponent
  mantissas = mix(
    mantissas >> (uvec3(maximumExponent + 15u) - exponents),
    uvec3(0x0), 
    greaterThan(uvec3(maximumExponent) - exponents, uvec3(8u))
  ) & uvec3(0x1ffu);

  // Combine the components
  return (mantissas.x << 0u) | (mantissas.y << 9u) | (mantissas.z << 18u) | (uint(maximumExponent - (127u - 16u)) << 27u);
     
#else  
  float maxChannel = max(max(color.x, color.y), color.z);
  int sharedExponent = clamp(int((uint(floatBitsToUint(maxChannel) & 0x7f800000u) >> 23u)) - (127 - 16), 0, 31);
  float denominator = exp2(float(sharedExponent - 24));  
  int maximum = int(floor((maxChannel / denominator) + 0.5));
  if(maximum >= 512){
    denominator *= 2.0;
    sharedExponent++;
  }
  uvec3 t = uvec3(floor((color / denominator) + vec3(0.5))) & uvec3(0x1ffu);
  return t.x | (t.y << 9u) | (t.z << 18u) | (uint(sharedExponent) << 27u);
#endif
}

vec3 decodeRGB9E5(uint color){
#if 0

  // Extract the shared exponent with +1 compensation for the implicit 1
  const uint sharedExponent = ((color >> 27u) & 0x1fu) + (127u - 16u);

  // Extract the mantissas
  uvec3 mantissas = (uvec3(color) >> uvec3(0u, 9u, 18u)) & uvec3(0x1ffu);

  // Get the most significant bit of the mantissas and add 1 to the shift which is the amount of bits to shift the expanded mantissas for 32-bit floats
  const uvec3 shifts = uvec3(findMSB(mantissas & uvec3(0x1ffu))) + uvec3(1u); 

  // Expand the mantissas for 32-bit floats
  mantissas = (mantissas << (uvec3(9u) - shifts)) & uvec3(0xffu);
  mantissas = (mantissas << uvec3(15u)) | (mantissas << uvec3(7u)) | (mantissas >> uvec3(1u));

  // Construct the exponents
  const uvec3 exponents = (mix((uvec3(sharedExponent) + shifts) - uvec3(9u), uvec3(0u), equal(shifts, uvec3(0u))) & uvec3(0xffu)) << uvec3(23u);

  // Combine the components
  return uintBitsToFloat(exponents | mantissas);

#else
  return vec3(
    float(bitfieldExtract(color, 0, 9)),
    float(bitfieldExtract(color, 9, 9)),
    float(bitfieldExtract(color, 18, 9))
  ) * exp2(float(int(bitfieldExtract(color, 27, 5)) - 24));
#endif
}

#endif