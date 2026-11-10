#ifndef PLANET_MASKMAP_GLSL
#define PLANET_MASKMAP_GLSL

// Planet mask map: four layers per texel (rgba8ui), whose meaning is up to the application, in the same octahedral
// equal-area layout as the height map, but with a resolution of its own. Each layer holds the signed distance of
// the texel centre from the edge of the masked area, positive inside, in 1/32 of a texel and biased by 128, so 0 is
// far outside (and a cleared image an empty mask), 255 far inside and the edge lies at 128. The distances saturate
// four texels away from the edge, which is all that the bilinear reconstruction of the edge needs. Unlike yes/no
// texels, which only tell on which side of a texel centre the edge passes, the distances tell where it passes, so
// the reconstructed edge follows the drawn shape to a fraction of a texel at any angle to the raster. A height map
// pass scales its brush weight by one minus the covered share of the layers it was told to respect, so that masked
// ground stays as it is.
//
// The mask is rasterized with integer arithmetic only, so that every GPU produces exactly the same mask from the
// same input. Floating point arithmetic, transcendental functions and texture filtering are not specified exactly
// by Vulkan and do differ between vendors and drivers, whereas integer arithmetic is exact everywhere. Directions
// are fixed point vectors with 14 fractional bits, so 1.0 is 16384.
//
// Requires octahedral.glsl (for wrapOctahedralTexelCoordinates) to be included before.

#define PLANET_MASKMAP_ONE 16384

// Distance units per texel.
#define PLANET_MASKMAP_DISTANCE_UNITS 32

// The side of a texel on a map of 1 x 1 texels as chord length on the unit sphere, 14 bit fixed point: the sphere
// has the area 4 pi, so a texel of an equal-area map of size x size texels has the side sqrt(4 pi) / size, and
// 16384 * sqrt(4 pi) is 58080.
#define PLANET_MASKMAP_TEXEL_CHORD 58080

// The width of the soft edge in distance units: a quarter of a texel, which is one height map texel while the
// height map has four times the resolution of the mask map.
#define PLANET_MASKMAP_EDGE_WIDTH 8.0

// floor(sqrt(v)), exact for every v below 65535^2. The float estimate is only a starting point, which the integer
// correction steps then move onto the exact result, so the outcome does not depend on the precision of sqrt.
uint planetMaskMapISqrt(const in uint v){
  uint r = uint(sqrt(float(v)));
  for(int i = 0; (i < 4) && ((r * r) > v); i++){
    r--;
  }
  for(int i = 0; (i < 4) && (((r + 1u) * (r + 1u)) <= v); i++){
    r++;
  }
  return r;
}

// sin(x * pi / 2) for x in [0, 1], both as 14 bit fixed point. A Taylor polynomial up to x^9 with coefficients of
// 16 fractional bits, which is accurate to about 1e-4, while every product stays below 2^31.
int planetMaskMapSinHalfPi(const in int x){
  int x2 = (x * x) >> 14;
  int p = 11;
  p = -307 + ((p * x2) >> 14);
  p = 5223 + ((p * x2) >> 14);
  p = -42334 + ((p * x2) >> 14);
  p = 102944 + ((p * x2) >> 14);
  return (p * x) >> 16;
}

// The direction of the centre of a mask texel on a map of size x size texels: the integer twin of
// octPlanetUnsignedDecode((vec2(texel) + vec2(0.5)) / vec2(size)) for the equal-area octahedral layout around the
// z axis. Accurate to about 3e-4 (two centimeters on a planet of 80 meters radius) and, above all, the same on
// every device.
ivec3 planetMaskMapTexelDirection(const in ivec2 texel, const in int size){
  // Signed octahedral coordinates times size, always odd, so never zero.
  int a = (texel.x << 1) + 1 - size;
  int b = (texel.y << 1) + 1 - size;
  int absA = abs(a);
  int absB = abs(b);
  int sum = absA + absB;
  // The inner diamond holds the hemisphere around +z, the four outer triangles the one around -z.
  bool upper = sum <= size;
  // The ring radius r of the equal-area mapping times size, and the angle around z as a fraction of it, where the
  // angle is that fraction times pi / 2.
  int ringRadius = upper ? sum : ((size << 1) - sum);
  int angle = upper ? absB : (size - absA);
  int r = int((uint(ringRadius) << 14u) / uint(size));
  // r^2 with 28 fractional bits, since near the poles r^2 would vanish in 14 bits.
  uint rr = uint(r * r);
  int z = PLANET_MASKMAP_ONE - int(rr >> 14u);
  // r * sqrt(2 - r^2), which is the length of the xy part.
  int m = (r * int(planetMaskMapISqrt((2u << 28u) - rr))) >> 14;
  int t = int((uint(angle) << 14u) / uint(ringRadius));
  int x = (planetMaskMapSinHalfPi(PLANET_MASKMAP_ONE - t) * m) >> 14;
  int y = (planetMaskMapSinHalfPi(t) * m) >> 14;
  return ivec3((a < 0) ? -x : x, (b < 0) ? -y : y, upper ? z : -z);
}

// The chord length, 14 bit fixed point, at which the distances of a map of size x size texels saturate, rounded up.
int planetMaskMapDistanceLimit(const in int size){
  return ((128 * PLANET_MASKMAP_TEXEL_CHORD) / PLANET_MASKMAP_DISTANCE_UNITS + (size - 1)) / size;
}

// A chord length within [-planetMaskMapDistanceLimit(size), planetMaskMapDistanceLimit(size)] in distance units,
// rounded to the nearest, halves away from zero.
int planetMaskMapChordToUnits(const in int chord, const in int size){
  int units = int(((uint(abs(chord) * size) * uint(PLANET_MASKMAP_DISTANCE_UNITS)) + uint(PLANET_MASKMAP_TEXEL_CHORD >> 1)) / uint(PLANET_MASKMAP_TEXEL_CHORD));
  return (chord < 0) ? -units : units;
}

// The signed distance of the direction p from the edge of the circle given by its centre (xyz) and its radius as a
// chord length on the unit sphere (w), all of them 14 bit fixed point. Positive inside, measured as the difference of
// the chord lengths, and limited to [-limit, limit]. A negative radius covers the whole sphere.
int planetMaskMapCircleDistance(const in ivec3 p, const in ivec4 circle, const in int limit){
  if(circle.w < 0){
    return limit;
  }
  ivec3 d = abs(p - circle.xyz);
  if(any(greaterThan(d, ivec3(circle.w + limit)))){
    return -limit;
  }
  // Every component is at most twice one, so the sum of the squares stays below 3 * 2^30.
  int chord = int(planetMaskMapISqrt(uint(d.x * d.x) + uint(d.y * d.y) + uint(d.z * d.z)));
  return clamp(circle.w - chord, -limit, limit);
}

// The signed distances of the four layers at uv (the unsigned octahedral coordinates of the point, where mask texel
// centres lie at (i + 0.5) / size), bilinearly interpolated, in distance units, positive inside. While the height
// map resolution is a power of two multiple of the mask resolution and uv is a height map texel centre, the weights
// are exact binary fractions and the distances small integers, so even these float steps come out the same on every
// device.
vec4 planetMaskMapDistances(const in usampler2D maskMap, const in vec2 uv){
  ivec2 size = textureSize(maskMap, 0);
  vec2 t = fma(uv, vec2(size), vec2(-0.5));
  ivec2 base = ivec2(floor(t));
  vec2 f = t - vec2(base);
  vec4 a = vec4(texelFetch(maskMap, wrapOctahedralTexelCoordinates(base, size), 0));
  vec4 b = vec4(texelFetch(maskMap, wrapOctahedralTexelCoordinates(base + ivec2(1, 0), size), 0));
  vec4 c = vec4(texelFetch(maskMap, wrapOctahedralTexelCoordinates(base + ivec2(0, 1), size), 0));
  vec4 d = vec4(texelFetch(maskMap, wrapOctahedralTexelCoordinates(base + ivec2(1, 1), size), 0));
  return mix(mix(a, b, f.x), mix(c, d, f.x), f.y) - vec4(128.0);
}

// The covered share in [0, 1] from interpolated distances: the largest of the layers named by the low four bits of
// maskBits, each going from 0 to 1 across the soft edge.
float planetMaskMapLayerCoverage(const in vec4 distances, const in uint maskBits){
  vec4 coverages = clamp(fma(distances, vec4(1.0 / PLANET_MASKMAP_EDGE_WIDTH), vec4(0.5)), vec4(0.0), vec4(1.0)) *
                   vec4(notEqual(uvec4(maskBits) & uvec4(1u, 2u, 4u, 8u), uvec4(0u)));
  return max(max(coverages.x, coverages.y), max(coverages.z, coverages.w));
}

// The covered share in [0, 1] at uv of the layers named by the low four bits of maskBits, zero without any.
float planetMaskMapCoverage(const in usampler2D maskMap, const in vec2 uv, const in uint maskBits){
  if((maskBits & 15u) == 0u){
    return 0.0;
  }else{
    return planetMaskMapLayerCoverage(planetMaskMapDistances(maskMap, uv), maskBits);
  }
}

#endif
