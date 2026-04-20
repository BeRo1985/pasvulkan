#ifndef MESHLET_GLSL
#define MESHLET_GLSL

#define MESHLET_DEBUG_COLOR_VARIANT_NONE 0
#define MESHLET_DEBUG_COLOR_VARIANT_FORMULA_1 1
#define MESHLET_DEBUG_COLOR_VARIANT_FORMULA_2 2
#define MESHLET_DEBUG_COLOR_VARIANT_FORMULA_3 3
#define MESHLET_DEBUG_COLOR_VARIANT_PREDEFINED 4

#define MESHLET_DEBUG_COLOR_VARIANT MESHLET_DEBUG_COLOR_VARIANT_FORMULA_3

#if MESHLET_DEBUG_COLOR_VARIANT == MESHLET_DEBUG_COLOR_VARIANT_PREDEFINED
#if 0
const vec3 MeshletDebugColors[16] = vec3[16](
    vec3(0.90, 0.20, 0.20), // red
    vec3(0.20, 0.90, 0.20), // green
    vec3(0.20, 0.45, 0.95), // blue
    vec3(0.95, 0.85, 0.20), // yellow
    vec3(0.95, 0.20, 0.95), // magenta
    vec3(0.20, 0.90, 0.90), // cyan
    vec3(0.95, 0.55, 0.20), // orange
    vec3(0.55, 0.95, 0.20), // lime
    vec3(0.55, 0.20, 0.95), // violet
    vec3(0.95, 0.20, 0.55), // pink
    vec3(0.20, 0.55, 0.95), // sky
    vec3(0.20, 0.95, 0.55), // mint
    vec3(0.95, 0.70, 0.35), // amber
    vec3(0.35, 0.95, 0.70), // aqua-green
    vec3(0.70, 0.35, 0.95), // purple
    vec3(0.95, 0.35, 0.70)  // rose
);
#else
const vec3 MeshletDebugColors[16] = vec3[16](
    vec3(1.00, 0.00, 0.00), // red
    vec3(0.00, 1.00, 0.00), // green
    vec3(0.00, 0.00, 1.00), // blue
    vec3(1.00, 1.00, 0.00), // yellow
    vec3(1.00, 0.00, 1.00), // magenta
    vec3(0.00, 1.00, 1.00), // cyan
    vec3(1.00, 0.50, 0.25), // orange
    vec3(0.50, 1.00, 0.25), // lime
    vec3(0.50, 0.25, 1.00), // violet
    vec3(1.00, 0.25, 0.50), // pink
    vec3(0.25, 0.50, 1.00), // sky
    vec3(0.25, 1.00, 0.50), // mint
    vec3(1.00, 0.75, 0.25), // amber
    vec3(0.25, 1.00, 0.75), // aqua-green
    vec3(0.75, 0.25, 1.00), // purple
    vec3(1.00, 0.25, 0.75)  // rose
);
#endif
#endif

vec3 meshletDebugColor(uint id){
#if MESHLET_DEBUG_COLOR_VARIANT == MESHLET_DEBUG_COLOR_VARIANT_PREDEFINED
  return MeshletDebugColors[id & 0xfu];
#elif MESHLET_DEBUG_COLOR_VARIANT == MESHLET_DEBUG_COLOR_VARIANT_FORMULA_1
  const vec3 oneDiv255 = vec3(1.0 / 255.0);
  const uvec3 primeMultipliers = uvec3(2654435761u, 2246822519u, 3266489917u);
  const uvec3 values = id * primeMultipliers;
  const uvec3 masked = (values >> uvec3(24u)) & uvec3(0xffu);
  const uvec3 quantized = masked & uvec3(0x30u); // Quantize to 4 levels per channel (0, 48, 96, 144 - 2 bits)
  const vec3 color = vec3(quantized) * oneDiv255 * 0.125;
  return color;
#elif MESHLET_DEBUG_COLOR_VARIANT == MESHLET_DEBUG_COLOR_VARIANT_FORMULA_2
  const vec3 oneDiv255 = vec3(1.0 / 255.0);
  return vec3(uvec3((uvec3((id * 747796405u) + 2891336453u) >> uvec3(0u, 8u, 16u)) & uvec3(255u))) * oneDiv255;
#elif MESHLET_DEBUG_COLOR_VARIANT == MESHLET_DEBUG_COLOR_VARIANT_FORMULA_3
  const vec3 oneDiv255 = vec3(1.0 / 255.0);
  return vec3(uvec3((uvec3(id) * uvec3(16807u, 48271u, 40692u)) & uvec3(0xffu))) * oneDiv255;
#else
  return vec3(1.0); // white for none or unknown variants
#endif
}

#endif