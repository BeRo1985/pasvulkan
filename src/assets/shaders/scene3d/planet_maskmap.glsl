#ifndef PLANET_MASKMAP_GLSL
#define PLANET_MASKMAP_GLSL

// Planet mask map: one 32 bit word of application defined mask bits per texel, in the same octahedral equal-area
// layout as the height map, but with a resolution of its own. A height map pass scales its brush weight by one
// minus the covered share of the mask bits it was told to respect, so that masked ground stays as it is.
//
// The mask is rasterized with integer arithmetic only, so that every GPU produces exactly the same mask from the
// same input. Floating point arithmetic, transcendental functions and texture filtering are not specified exactly
// by Vulkan and do differ between vendors and drivers, whereas integer arithmetic is exact everywhere. Directions
// are fixed point vectors with 14 fractional bits, so 1.0 is 16384.
//
// Requires octahedral.glsl (for wrapOctahedralTexelCoordinates) to be included before.

#define PLANET_MASKMAP_ONE 16384

// floor(sqrt(v)), exact for v up to 2^30. The float estimate is only a starting point, which the integer
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

// Whether the direction p lies inside the circle given by its centre (xyz) and its radius as a chord length on the
// unit sphere (w), all of them 14 bit fixed point. A negative radius covers the whole sphere.
bool planetMaskMapInsideCircle(const in ivec3 p, const in ivec4 circle){
  if(circle.w < 0){
    return true;
  }
  ivec3 d = abs(p - circle.xyz);
  if(any(greaterThan(d, ivec3(circle.w)))){
    return false;
  }
  return (uint(d.x * d.x) + uint(d.y * d.y) + uint(d.z * d.z)) <= uint(circle.w * circle.w);
}

// The four mask words around uv (the unsigned octahedral coordinates of the point, where mask texel centres lie at
// (i + 0.5) / size) together with their bilinear weights, for evaluating one or several sets of mask bits.
void planetMaskMapGather(const in usampler2D maskMap, const in vec2 uv, out uvec4 words, out vec4 weights){
  ivec2 size = textureSize(maskMap, 0);
  vec2 t = fma(uv, vec2(size), vec2(-0.5));
  ivec2 base = ivec2(floor(t));
  vec2 f = t - vec2(base);
  words = uvec4(texelFetch(maskMap, wrapOctahedralTexelCoordinates(base, size), 0).x,
                texelFetch(maskMap, wrapOctahedralTexelCoordinates(base + ivec2(1, 0), size), 0).x,
                texelFetch(maskMap, wrapOctahedralTexelCoordinates(base + ivec2(0, 1), size), 0).x,
                texelFetch(maskMap, wrapOctahedralTexelCoordinates(base + ivec2(1, 1), size), 0).x);
  weights = vec4((1.0 - f.x) * (1.0 - f.y), f.x * (1.0 - f.y), (1.0 - f.x) * f.y, f.x * f.y);
}

// The covered share in [0, 1] of the gathered texels which have any of maskBits set.
float planetMaskMapGatheredCoverage(const in uvec4 words, const in vec4 weights, const in uint maskBits){
  return dot(vec4(notEqual(words & uvec4(maskBits), uvec4(0u))), weights);
}

// The covered share in [0, 1] at uv of the texels which have any of maskBits set, zero without any bits. While the
// height map resolution is a power of two multiple of the mask resolution, the weights are exact binary fractions,
// so even this float step comes out the same on every device.
float planetMaskMapCoverage(const in usampler2D maskMap, const in vec2 uv, const in uint maskBits){
  if(maskBits == 0u){
    return 0.0;
  }else{
    uvec4 words;
    vec4 weights;
    planetMaskMapGather(maskMap, uv, words, weights);
    return planetMaskMapGatheredCoverage(words, weights, maskBits);
  }
}

#endif
