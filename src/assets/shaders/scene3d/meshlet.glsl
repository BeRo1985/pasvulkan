#ifndef MESHLET_GLSL
#define MESHLET_GLSL

vec3 meshletDebugColor(uint id){
  const vec3 oneDiv255 = vec3(1.0 / 255.0);
  const uvec3 primeMultipliers = uvec3(2654435761u, 2246822519u, 3266489917u);
  const uvec3 values = id * primeMultipliers;
  const uvec3 masked = (values >> uvec3(24u)) & uvec3(0xffu);
  const uvec3 quantized = masked & uvec3(0x30u); // Quantize to 4 levels per channel (0, 48, 96, 144 - 2 bits)
  const vec3 color = vec3(quantized) * oneDiv255 * 0.125;
  return color;
}

#endif