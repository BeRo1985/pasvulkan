#ifndef SPHERICALHARMONICS_GLSL
#define SPHERICALHARMONICS_GLSL

#extension GL_EXT_control_flow_attributes : enable

const float SH_PI = 3.141592653589793;
const float SH_SQRT_PI = 1.7724538509055159;

const float SH_COSINE_A0 = SH_PI;
const float SH_COSINE_A1 = (2.0 * SH_PI) / 3.0;
const float SH_COSINE_A2 = SH_PI * 0.25;

const float SH_BASIS_L0 = 1.0 / (2.0 * SH_SQRT_PI);
const float SH_BASIS_L1 = sqrt(3.0) / (2.0 * SH_SQRT_PI);
const float SH_BASIS_L2_MN2 = sqrt(15.0) / (2.0 * SH_SQRT_PI);
const float SH_BASIS_L2_MN1 = sqrt(15.0) / (2.0 * SH_SQRT_PI);
const float SH_BASIS_L2_M0 = sqrt(5.0) / (4.0 * SH_SQRT_PI);
const float SH_BASIS_L2_M1 = sqrt(15.0) / (2.0 * SH_SQRT_PI);
const float SH_BASIS_L2_M2 = sqrt(15.0) / (4.0 * SH_SQRT_PI);

#define SH_L1_COUNT_COEFFICIENTS 4
#define SH_L2_COUNT_COEFFICIENTS 9

#define SH_VALUE float
#define SH_VEC2 vec2
#define SH_VEC3 vec3
#define SH_VEC4 vec4
#define SH_RGB vec3

struct SHCoefficientsL1 {
  SH_VALUE coefficients[SH_L1_COUNT_COEFFICIENTS];
};

#define PackedSHCoefficientsL1 uvec2

struct SHRGBCoefficientsL1 {
  SH_RGB coefficients[SH_L1_COUNT_COEFFICIENTS];
};

struct PackedSHRGBCoefficientsL1 {
  uvec2 coefficients[3];
};

struct SHCoefficientsL2 {
  SH_VALUE coefficients[SH_L2_COUNT_COEFFICIENTS];
};

struct SHRGBCoefficientsL2 {
  SH_RGB coefficients[SH_L2_COUNT_COEFFICIENTS];
};

SHCoefficientsL1 SHCoefficientsL1Zero() {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      0.0,
      0.0,
      0.0,
      0.0
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1Create(const in SH_VALUE c0, const in SH_VALUE c1, const in SH_VALUE c2, const in SH_VALUE c3) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      c0,
      c1,
      c2,
      c3
    )
  );
}

PackedSHCoefficientsL1 SHCoefficientsL1Pack(const in SHCoefficientsL1 sh) {
  return PackedSHCoefficientsL1(
    packHalf2x16(vec2(sh.coefficients[0], sh.coefficients[1])),
    packHalf2x16(vec2(sh.coefficients[2], sh.coefficients[3]))
  );
}

SHCoefficientsL1 SHCoefficientsL1Unpack(const in PackedSHCoefficientsL1 packed) {
  vec4 unpacked = vec4(unpackHalf2x16(packed.x), unpackHalf2x16(packed.y));
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      unpacked.x,
      unpacked.y,
      unpacked.z,
      unpacked.w
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Zero() {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0)
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Create(const in SH_RGB c0, const in SH_RGB c1, const in SH_RGB c2, const in SH_RGB c3) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      c0,
      c1,
      c2,
      c3
    )
  );
}

PackedSHRGBCoefficientsL1 SHRGBCoefficientsL1Pack(const in SHRGBCoefficientsL1 sh) {
  return PackedSHRGBCoefficientsL1(
    uvec2[3](
      uvec2(packHalf2x16(vec2(sh.coefficients[0].r, sh.coefficients[0].g)), packHalf2x16(vec2(sh.coefficients[0].b, sh.coefficients[1].r))),
      uvec2(packHalf2x16(vec2(sh.coefficients[1].g, sh.coefficients[1].b)), packHalf2x16(vec2(sh.coefficients[2].r, sh.coefficients[2].g))),
      uvec2(packHalf2x16(vec2(sh.coefficients[2].b, sh.coefficients[3].r)), packHalf2x16(vec2(sh.coefficients[3].g, sh.coefficients[3].b)))
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Unpack(const in PackedSHRGBCoefficientsL1 packed) {
  vec4 unpacked0 = vec4(unpackHalf2x16(packed.coefficients[0].x), unpackHalf2x16(packed.coefficients[0].y));
  vec4 unpacked1 = vec4(unpackHalf2x16(packed.coefficients[1].x), unpackHalf2x16(packed.coefficients[1].y));
  vec4 unpacked2 = vec4(unpackHalf2x16(packed.coefficients[2].x), unpackHalf2x16(packed.coefficients[2].y));
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      SH_RGB(unpacked0.x, unpacked0.y, unpacked0.z),
      SH_RGB(unpacked0.w, unpacked1.x, unpacked1.y),
      SH_RGB(unpacked1.z, unpacked1.w, unpacked2.x),
      SH_RGB(unpacked2.y, unpacked2.z, unpacked2.w)
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2Zero() {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      0.0,
      0.0,
      0.0,
      0.0,
      0.0,
      0.0,
      0.0,
      0.0,
      0.0
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2Create(const in SH_VALUE c0, const in SH_VALUE c1, const in SH_VALUE c2, const in SH_VALUE c3, const in SH_VALUE c4, const in SH_VALUE c5, const in SH_VALUE c6, const in SH_VALUE c7, const in SH_VALUE c8) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      c0,
      c1,
      c2,
      c3,
      c4,
      c5,
      c6,
      c7,
      c8
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Zero() {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0),
      SH_RGB(0.0, 0.0, 0.0)
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Create(const in SH_RGB c0, const in SH_RGB c1, const in SH_RGB c2, const in SH_RGB c3, const in SH_RGB c4, const in SH_RGB c5, const in SH_RGB c6, const in SH_RGB c7, const in SH_RGB c8) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      c0,
      c1,
      c2,
      c3,
      c4,
      c5,
      c6,
      c7,
      c8
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1Add(const in SHCoefficientsL1 a, const in SHCoefficientsL1 b) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] + b.coefficients[0],
      a.coefficients[1] + b.coefficients[1],
      a.coefficients[2] + b.coefficients[2],
      a.coefficients[3] + b.coefficients[3]
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Add(const in SHRGBCoefficientsL1 a, const in SHRGBCoefficientsL1 b) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] + b.coefficients[0],
      a.coefficients[1] + b.coefficients[1],
      a.coefficients[2] + b.coefficients[2],
      a.coefficients[3] + b.coefficients[3]
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2Add(const in SHCoefficientsL2 a, const in SHCoefficientsL2 b) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] + b.coefficients[0],
      a.coefficients[1] + b.coefficients[1],
      a.coefficients[2] + b.coefficients[2],
      a.coefficients[3] + b.coefficients[3],
      a.coefficients[4] + b.coefficients[4],
      a.coefficients[5] + b.coefficients[5],
      a.coefficients[6] + b.coefficients[6],
      a.coefficients[7] + b.coefficients[7],
      a.coefficients[8] + b.coefficients[8]
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Add(const in SHRGBCoefficientsL2 a, const in SHRGBCoefficientsL2 b) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] + b.coefficients[0],
      a.coefficients[1] + b.coefficients[1],
      a.coefficients[2] + b.coefficients[2],
      a.coefficients[3] + b.coefficients[3],
      a.coefficients[4] + b.coefficients[4],
      a.coefficients[5] + b.coefficients[5],
      a.coefficients[6] + b.coefficients[6],
      a.coefficients[7] + b.coefficients[7],
      a.coefficients[8] + b.coefficients[8]
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1Sub(const in SHCoefficientsL1 a, const in SHCoefficientsL1 b) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] - b.coefficients[0],
      a.coefficients[1] - b.coefficients[1],
      a.coefficients[2] - b.coefficients[2],
      a.coefficients[3] - b.coefficients[3]
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Sub(const in SHRGBCoefficientsL1 a, const in SHRGBCoefficientsL1 b) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] - b.coefficients[0],
      a.coefficients[1] - b.coefficients[1],
      a.coefficients[2] - b.coefficients[2],
      a.coefficients[3] - b.coefficients[3]
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2Sub(const in SHCoefficientsL2 a, const in SHCoefficientsL2 b) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] - b.coefficients[0],
      a.coefficients[1] - b.coefficients[1],
      a.coefficients[2] - b.coefficients[2],
      a.coefficients[3] - b.coefficients[3],
      a.coefficients[4] - b.coefficients[4],
      a.coefficients[5] - b.coefficients[5],
      a.coefficients[6] - b.coefficients[6],
      a.coefficients[7] - b.coefficients[7],
      a.coefficients[8] - b.coefficients[8]
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Sub(const in SHRGBCoefficientsL2 a, const in SHRGBCoefficientsL2 b) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] - b.coefficients[0],
      a.coefficients[1] - b.coefficients[1],
      a.coefficients[2] - b.coefficients[2],
      a.coefficients[3] - b.coefficients[3],
      a.coefficients[4] - b.coefficients[4],
      a.coefficients[5] - b.coefficients[5],
      a.coefficients[6] - b.coefficients[6],
      a.coefficients[7] - b.coefficients[7],
      a.coefficients[8] - b.coefficients[8]
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1Mul(const in SHCoefficientsL1 a, const in SH_VALUE b) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] * b,
      a.coefficients[1] * b,
      a.coefficients[2] * b,
      a.coefficients[3] * b
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Mul(const in SHRGBCoefficientsL1 a, const in SH_VALUE b) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] * b,
      a.coefficients[1] * b,
      a.coefficients[2] * b,
      a.coefficients[3] * b
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2Mul(const in SHCoefficientsL2 a, const in SH_VALUE b) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] * b,
      a.coefficients[1] * b,
      a.coefficients[2] * b,
      a.coefficients[3] * b,
      a.coefficients[4] * b,
      a.coefficients[5] * b,
      a.coefficients[6] * b,
      a.coefficients[7] * b,
      a.coefficients[8] * b
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Mul(const in SHRGBCoefficientsL2 a, const in SH_VALUE b) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] * b,
      a.coefficients[1] * b,
      a.coefficients[2] * b,
      a.coefficients[3] * b,
      a.coefficients[4] * b,
      a.coefficients[5] * b,
      a.coefficients[6] * b,
      a.coefficients[7] * b,
      a.coefficients[8] * b
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1Div(const in SHCoefficientsL1 a, const in SH_VALUE b) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] / b,
      a.coefficients[1] / b,
      a.coefficients[2] / b,
      a.coefficients[3] / b
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Div(const in SHRGBCoefficientsL1 a, const in SH_VALUE b) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      a.coefficients[0] / b,
      a.coefficients[1] / b,
      a.coefficients[2] / b,
      a.coefficients[3] / b
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2Div(const in SHCoefficientsL2 a, const in SH_VALUE b) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] / b,
      a.coefficients[1] / b,
      a.coefficients[2] / b,
      a.coefficients[3] / b,
      a.coefficients[4] / b,
      a.coefficients[5] / b,
      a.coefficients[6] / b,
      a.coefficients[7] / b,
      a.coefficients[8] / b
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Div(const in SHRGBCoefficientsL2 a, const in SH_VALUE b) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      a.coefficients[0] / b,
      a.coefficients[1] / b,
      a.coefficients[2] / b,
      a.coefficients[3] / b,
      a.coefficients[4] / b,
      a.coefficients[5] / b,
      a.coefficients[6] / b,
      a.coefficients[7] / b,
      a.coefficients[8] / b
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1FromL2(const in SHCoefficientsL2 sh){
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0],
      sh.coefficients[1],
      sh.coefficients[2],
      sh.coefficients[3]
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1FromL2(const in SHRGBCoefficientsL2 sh){
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0],
      sh.coefficients[1],
      sh.coefficients[2],
      sh.coefficients[3]
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1FromSHCoefficientsL1(const in SHCoefficientsL1 sh){
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      SH_RGB(sh.coefficients[0]),
      SH_RGB(sh.coefficients[1]),
      SH_RGB(sh.coefficients[2]),
      SH_RGB(sh.coefficients[3])
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2FromSHCoefficientsL2(const in SHCoefficientsL2 sh){
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      SH_RGB(sh.coefficients[0]),
      SH_RGB(sh.coefficients[1]),
      SH_RGB(sh.coefficients[2]),
      SH_RGB(sh.coefficients[3]),
      SH_RGB(sh.coefficients[4]),
      SH_RGB(sh.coefficients[5]),
      SH_RGB(sh.coefficients[6]),
      SH_RGB(sh.coefficients[7]),
      SH_RGB(sh.coefficients[8])
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1Lerp(const in SHCoefficientsL1 a, const in SHCoefficientsL1 b, const in SH_VALUE t) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      mix(a.coefficients[0], b.coefficients[0], t),
      mix(a.coefficients[1], b.coefficients[1], t),
      mix(a.coefficients[2], b.coefficients[2], t),
      mix(a.coefficients[3], b.coefficients[3], t)
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Lerp(const in SHRGBCoefficientsL1 a, const in SHRGBCoefficientsL1 b, const in SH_VALUE t) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      mix(a.coefficients[0], b.coefficients[0], t),
      mix(a.coefficients[1], b.coefficients[1], t),
      mix(a.coefficients[2], b.coefficients[2], t),
      mix(a.coefficients[3], b.coefficients[3], t)
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2Lerp(const in SHCoefficientsL2 a, const in SHCoefficientsL2 b, const in SH_VALUE t) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      mix(a.coefficients[0], b.coefficients[0], t),
      mix(a.coefficients[1], b.coefficients[1], t),
      mix(a.coefficients[2], b.coefficients[2], t),
      mix(a.coefficients[3], b.coefficients[3], t),
      mix(a.coefficients[4], b.coefficients[4], t),
      mix(a.coefficients[5], b.coefficients[5], t),
      mix(a.coefficients[6], b.coefficients[6], t),
      mix(a.coefficients[7], b.coefficients[7], t),
      mix(a.coefficients[8], b.coefficients[8], t)
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Lerp(const in SHRGBCoefficientsL2 a, const in SHRGBCoefficientsL2 b, const in SH_VALUE t) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      mix(a.coefficients[0], b.coefficients[0], t),
      mix(a.coefficients[1], b.coefficients[1], t),
      mix(a.coefficients[2], b.coefficients[2], t),
      mix(a.coefficients[3], b.coefficients[3], t),
      mix(a.coefficients[4], b.coefficients[4], t),
      mix(a.coefficients[5], b.coefficients[5], t),
      mix(a.coefficients[6], b.coefficients[6], t),
      mix(a.coefficients[7], b.coefficients[7], t),
      mix(a.coefficients[8], b.coefficients[8], t)
    )
  );
}

SHCoefficientsL1 ProjectOntoSHCoefficientsL1(SH_VEC3 direction, SH_VALUE value) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      value * SH_BASIS_L0,
      value * SH_BASIS_L1 * direction.y,
      value * SH_BASIS_L1 * direction.z,
      value * SH_BASIS_L1 * direction.x
    )
  );
}

SHRGBCoefficientsL1 ProjectOntoSHRGBCoefficientsL1(SH_VEC3 direction, SH_RGB value) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      value * SH_BASIS_L0,
      value * SH_BASIS_L1 * direction.y,
      value * SH_BASIS_L1 * direction.z,
      value * SH_BASIS_L1 * direction.x
    )
  );
}

SHCoefficientsL2 ProjectOntoSHCoefficientsL2(SH_VEC3 direction, SH_VALUE value) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      value * SH_BASIS_L0,
      value * SH_BASIS_L1 * direction.y,
      value * SH_BASIS_L1 * direction.z,
      value * SH_BASIS_L1 * direction.x,
      value * SH_BASIS_L2_MN2 * direction.x * direction.y,
      value * SH_BASIS_L2_MN1 * direction.y * direction.z,
      value * SH_BASIS_L2_M0 * ((3.0 * (direction.z * direction.z)) - 1.0),
      value * SH_BASIS_L2_M1 * direction.x * direction.z,
      value * SH_BASIS_L2_M2 * ((direction.x * direction.x) - (direction.y * direction.y))
    )
  );
}

SHRGBCoefficientsL2 ProjectOntoSHRGBCoefficientsL2(SH_VEC3 direction, SH_RGB value) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      value * SH_BASIS_L0,
      value * SH_BASIS_L1 * direction.y,
      value * SH_BASIS_L1 * direction.z,
      value * SH_BASIS_L1 * direction.x,
      value * SH_BASIS_L2_MN2 * direction.x * direction.y,
      value * SH_BASIS_L2_MN1 * direction.y * direction.z,
      value * SH_BASIS_L2_M0 * ((3.0 * (direction.z * direction.z)) - 1.0),
      value * SH_BASIS_L2_M1 * direction.x * direction.z,
      value * SH_BASIS_L2_M2 * ((direction.x * direction.x) - (direction.y * direction.y))
    )
  );
}

SH_VALUE DotSHCoefficientsL1(const in SHCoefficientsL1 a, const in SHCoefficientsL1 b) {
  return (a.coefficients[0] * b.coefficients[0]) +
         (a.coefficients[1] * b.coefficients[1]) +
         (a.coefficients[2] * b.coefficients[2]) +
         (a.coefficients[3] * b.coefficients[3]);
}

SH_RGB DotSHRGBCoefficientsL1(const in SHRGBCoefficientsL1 a, const in SHRGBCoefficientsL1 b) {
  return (a.coefficients[0] * b.coefficients[0]) +
         (a.coefficients[1] * b.coefficients[1]) +
         (a.coefficients[2] * b.coefficients[2]) +
         (a.coefficients[3] * b.coefficients[3]);
}

SH_VALUE DotSHCoefficientsL2(const in SHCoefficientsL2 a, const in SHCoefficientsL2 b) {
  return (a.coefficients[0] * b.coefficients[0]) +
         (a.coefficients[1] * b.coefficients[1]) +
         (a.coefficients[2] * b.coefficients[2]) +
         (a.coefficients[3] * b.coefficients[3]) +
         (a.coefficients[4] * b.coefficients[4]) +
         (a.coefficients[5] * b.coefficients[5]) +
         (a.coefficients[6] * b.coefficients[6]) +
         (a.coefficients[7] * b.coefficients[7]) +
         (a.coefficients[8] * b.coefficients[8]);
}

SH_RGB DotSHRGBCoefficientsL2(const in SHRGBCoefficientsL2 a, const in SHRGBCoefficientsL2 b) {
  return (a.coefficients[0] * b.coefficients[0]) +
         (a.coefficients[1] * b.coefficients[1]) +
         (a.coefficients[2] * b.coefficients[2]) +
         (a.coefficients[3] * b.coefficients[3]) +
         (a.coefficients[4] * b.coefficients[4]) +
         (a.coefficients[5] * b.coefficients[5]) +
         (a.coefficients[6] * b.coefficients[6]) +
         (a.coefficients[7] * b.coefficients[7]) +
         (a.coefficients[8] * b.coefficients[8]);
}

SH_VALUE EvaluateSHCoefficientsL1(const in SHCoefficientsL1 sh, const in SH_VEC3 direction) {
  return (sh.coefficients[0] * SH_BASIS_L0) +
         (sh.coefficients[1] * SH_BASIS_L1 * direction.y) +
         (sh.coefficients[2] * SH_BASIS_L1 * direction.z) +
         (sh.coefficients[3] * SH_BASIS_L1 * direction.x);
}

SH_RGB EvaluateSHRGBCoefficientsL1(const in SHRGBCoefficientsL1 sh, const in SH_VEC3 direction) {
  return (sh.coefficients[0] * SH_BASIS_L0) +
         (sh.coefficients[1] * SH_BASIS_L1 * direction.y) +
         (sh.coefficients[2] * SH_BASIS_L1 * direction.z) +
         (sh.coefficients[3] * SH_BASIS_L1 * direction.x);
}

SH_VALUE EvaluateSHCoefficientsL2(const in SHCoefficientsL2 sh, const in SH_VEC3 direction) {
  return (sh.coefficients[0] * SH_BASIS_L0) +
         (sh.coefficients[1] * SH_BASIS_L1 * direction.y) +
         (sh.coefficients[2] * SH_BASIS_L1 * direction.z) +
         (sh.coefficients[3] * SH_BASIS_L1 * direction.x) +
         (sh.coefficients[4] * SH_BASIS_L2_MN2 * direction.x * direction.y) +
         (sh.coefficients[5] * SH_BASIS_L2_MN1 * direction.y * direction.z) +
         (sh.coefficients[6] * SH_BASIS_L2_M0 * ((3.0 * (direction.z * direction.z)) - 1.0)) +
         (sh.coefficients[7] * SH_BASIS_L2_M1 * direction.x * direction.z) +
         (sh.coefficients[8] * SH_BASIS_L2_M2 * ((direction.x * direction.x) - (direction.y * direction.y)));
}

SH_RGB EvaluateSHRGBCoefficientsL2(const in SHRGBCoefficientsL2 sh, const in SH_VEC3 direction) {
  return (sh.coefficients[0] * SH_BASIS_L0) +
         (sh.coefficients[1] * SH_BASIS_L1 * direction.y) +
         (sh.coefficients[2] * SH_BASIS_L1 * direction.z) +
         (sh.coefficients[3] * SH_BASIS_L1 * direction.x) +
         (sh.coefficients[4] * SH_BASIS_L2_MN2 * direction.x * direction.y) +
         (sh.coefficients[5] * SH_BASIS_L2_MN1 * direction.y * direction.z) +
         (sh.coefficients[6] * SH_BASIS_L2_M0 * ((3.0 * (direction.z * direction.z)) - 1.0)) +
         (sh.coefficients[7] * SH_BASIS_L2_M1 * direction.x * direction.z) +
         (sh.coefficients[8] * SH_BASIS_L2_M2 * ((direction.x * direction.x) - (direction.y * direction.y)));
}

SHCoefficientsL1 SHCoefficientsL1ConvolveWithZonalHarmonics(const in SHCoefficientsL1 sh, const in SH_VEC2 zonalHarmonics) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0] * zonalHarmonics.x,
      sh.coefficients[1] * zonalHarmonics.y,
      sh.coefficients[2] * zonalHarmonics.y,
      sh.coefficients[3] * zonalHarmonics.y
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1ConvolveWithZonalHarmonics(const in SHRGBCoefficientsL1 sh, const in SH_VEC2 zonalHarmonics) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0] * zonalHarmonics.x,
      sh.coefficients[1] * zonalHarmonics.y,
      sh.coefficients[2] * zonalHarmonics.y,
      sh.coefficients[3] * zonalHarmonics.y
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2ConvolveWithZonalHarmonics(const in SHCoefficientsL2 sh, const in SH_VEC3 zonalHarmonics) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      sh.coefficients[0] * zonalHarmonics.x,
      sh.coefficients[1] * zonalHarmonics.y,
      sh.coefficients[2] * zonalHarmonics.y,
      sh.coefficients[3] * zonalHarmonics.y,
      sh.coefficients[4] * zonalHarmonics.z,
      sh.coefficients[5] * zonalHarmonics.z,
      sh.coefficients[6] * zonalHarmonics.z,
      sh.coefficients[7] * zonalHarmonics.z,
      sh.coefficients[8] * zonalHarmonics.z
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2ConvolveWithZonalHarmonics(const in SHRGBCoefficientsL2 sh, const in SH_VEC3 zonalHarmonics) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      sh.coefficients[0] * zonalHarmonics.x,
      sh.coefficients[1] * zonalHarmonics.y,
      sh.coefficients[2] * zonalHarmonics.y,
      sh.coefficients[3] * zonalHarmonics.y,
      sh.coefficients[4] * zonalHarmonics.z,
      sh.coefficients[5] * zonalHarmonics.z,
      sh.coefficients[6] * zonalHarmonics.z,
      sh.coefficients[7] * zonalHarmonics.z,
      sh.coefficients[8] * zonalHarmonics.z
    )
  );
}

SHCoefficientsL1 SHCoefficientsL1ConvolveWithCosineLobe(const in SHCoefficientsL1 sh) {
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0] * SH_COSINE_A0,
      sh.coefficients[1] * SH_COSINE_A1,
      sh.coefficients[2] * SH_COSINE_A1,
      sh.coefficients[3] * SH_COSINE_A1
    )
  );
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1ConvolveWithCosineLobe(const in SHRGBCoefficientsL1 sh) {
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0] * SH_COSINE_A0,
      sh.coefficients[1] * SH_COSINE_A1,
      sh.coefficients[2] * SH_COSINE_A1,
      sh.coefficients[3] * SH_COSINE_A1
    )
  );
}

SHCoefficientsL2 SHCoefficientsL2ConvolveWithCosineLobe(const in SHCoefficientsL2 sh) {
  return SHCoefficientsL2(
    SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
      sh.coefficients[0] * SH_COSINE_A0,
      sh.coefficients[1] * SH_COSINE_A1,
      sh.coefficients[2] * SH_COSINE_A1,
      sh.coefficients[3] * SH_COSINE_A1,
      sh.coefficients[4] * SH_COSINE_A2,
      sh.coefficients[5] * SH_COSINE_A2,
      sh.coefficients[6] * SH_COSINE_A2,
      sh.coefficients[7] * SH_COSINE_A2,
      sh.coefficients[8] * SH_COSINE_A2
    )
  );
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2ConvolveWithCosineLobe(const in SHRGBCoefficientsL2 sh) {
  return SHRGBCoefficientsL2(
    SH_RGB[SH_L2_COUNT_COEFFICIENTS](
      sh.coefficients[0] * SH_COSINE_A0,
      sh.coefficients[1] * SH_COSINE_A1,
      sh.coefficients[2] * SH_COSINE_A1,
      sh.coefficients[3] * SH_COSINE_A1,
      sh.coefficients[4] * SH_COSINE_A2,
      sh.coefficients[5] * SH_COSINE_A2,
      sh.coefficients[6] * SH_COSINE_A2,
      sh.coefficients[7] * SH_COSINE_A2,
      sh.coefficients[8] * SH_COSINE_A2
    )
  );
}

SH_VEC3 SHCoefficientsL1GetOptimalLinearDirection(const in SHCoefficientsL1 sh) {
  return normalize(SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]));
}

SH_VEC3 SHRGBCoefficientsL1GetOptimalLinearDirection(const in SHRGBCoefficientsL1 sh) {
  return normalize(
    SH_VEC3(
      dot(sh.coefficients[3], SH_VEC3(1.0)), 
      dot(sh.coefficients[1], SH_VEC3(1.0)),
      dot(sh.coefficients[2], SH_VEC3(1.0))
    )
  );
}

SH_VEC3 SHCoefficientsL2GetOptimalLinearDirection(const in SHCoefficientsL2 sh) {
  return normalize(SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]));
}

SH_VEC3 SHRGBCoefficientsL2GetOptimalLinearDirection(const in SHRGBCoefficientsL2 sh) {
  return normalize(
    SH_VEC3(
      dot(sh.coefficients[3], SH_VEC3(1.0)), 
      dot(sh.coefficients[1], SH_VEC3(1.0)),
      dot(sh.coefficients[2], SH_VEC3(1.0))
    )
  );
}

void SHCoefficientsL1ApproximateDirectionalLight(const in SHCoefficientsL1 sh, out SH_VEC3 direction, out SH_VALUE intensity) {
  SHCoefficientsL1 dirSH = ProjectOntoSHCoefficientsL1(direction = SHCoefficientsL1GetOptimalLinearDirection(sh), 1.0);
  dirSH.coefficients[0] = 0.0;
  intensity = DotSHCoefficientsL1(dirSH, sh) * (867.0 / (316.0 * SH_PI));
}

void SHRGBCoefficientsL1ApproximateDirectionalLight(const in SHRGBCoefficientsL1 sh, out SH_VEC3 direction, out SH_RGB color) {
  SHRGBCoefficientsL1 dirSH = ProjectOntoSHRGBCoefficientsL1(direction = SHRGBCoefficientsL1GetOptimalLinearDirection(sh), SH_VEC3(1.0));
  dirSH.coefficients[0] = SH_VEC3(0.0);
  color = DotSHRGBCoefficientsL1(dirSH, sh) * (867.0 / (316.0 * SH_PI));
}

// Calculates the irradiance from a given SH coefficient set and normal vector.
// This function first applies a cosine lobe convolution to the radiance, then evaluates it using the given normal vector.
// Note that the resulting irradiance is not divided by PI: for Lambertian diffuse calculations,
// ensure to include the 1/PI division as part of the Lambertian BRDF.
// For instance: vec3 diffuse = CalculateIrradiance(sh, normal) * diffuseAlbedo / PI;
SH_VALUE SHRCoefficientsL1CalculateIrradiance(const in SHCoefficientsL1 sh, const in SH_VEC3 normal) {
  return EvaluateSHCoefficientsL1(SHCoefficientsL1ConvolveWithCosineLobe(sh), normal);
}

SH_RGB SHRGBCoefficientsL1CalculateIrradiance(const in SHRGBCoefficientsL1 sh, const in SH_VEC3 normal) {
  return EvaluateSHRGBCoefficientsL1(SHRGBCoefficientsL1ConvolveWithCosineLobe(sh), normal);
}

SH_VALUE SHRCoefficientsL2CalculateIrradiance(const in SHCoefficientsL2 sh, const in SH_VEC3 normal) {
  return EvaluateSHCoefficientsL2(SHCoefficientsL2ConvolveWithCosineLobe(sh), normal);
}

SH_RGB SHRGBCoefficientsL2CalculateIrradiance(const in SHRGBCoefficientsL2 sh, const in SH_VEC3 normal) {
  return EvaluateSHRGBCoefficientsL2(SHRGBCoefficientsL2ConvolveWithCosineLobe(sh), normal);
}

// Calculates the irradiance from a set of L1 SH coeffecients using a non-linear fit
// Note that the resulting irradiance is not divided by PI: for Lambertian diffuse calculations,
// ensure to include the 1/PI division as part of the Lambertian BRDF.
// For instance: vec3 diffuse = CalculateIrradiance(sh, normal) * diffuseAlbedo / PI;
SH_VALUE SHRCoefficientsL1CalculateIrradianceGeomerics(const in SHCoefficientsL1 sh, const in SH_VEC3 normal) {
  SH_VALUE R0 = max(1e-5, sh.coefficients[0]);
  SH_VEC3 R1 = 0.5 * SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]);
  SH_VALUE lenR1 = max(1e-5, length(R1));
  SH_VALUE q = 0.5 * (1.0 + dot(R1 / lenR1, normal));
  SH_VALUE p = 1.0 + ((2.0 * lenR1) / R0);
  SH_VALUE a = ((1.0 - (lenR1 / R0)) / (1.0 + (lenR1 / R0)));
  return R0 * (a + (1.0 - a) * (p + 1.0) * pow(abs(q), p));
}

SH_RGB SHRGBCoefficientsL1CalculateIrradianceGeomerics(const in SHRGBCoefficientsL1 sh, const in SH_VEC3 normal) {
  return SH_VEC3(
    SHRCoefficientsL1CalculateIrradianceGeomerics(
      SHCoefficientsL1(
        SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
          sh.coefficients[0].x,
          sh.coefficients[1].x,
          sh.coefficients[2].x,
          sh.coefficients[3].x
        )
      ),
      normal
    ),
    SHRCoefficientsL1CalculateIrradianceGeomerics(
      SHCoefficientsL1(
        SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
          sh.coefficients[0].y,
          sh.coefficients[1].y,
          sh.coefficients[2].y,
          sh.coefficients[3].y
        )
      ),
      normal
    ),
    SHRCoefficientsL1CalculateIrradianceGeomerics(
      SHCoefficientsL1(
        SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
          sh.coefficients[0].z,
          sh.coefficients[1].z,
          sh.coefficients[2].z,
          sh.coefficients[3].z
        )
      ),
      normal
    )
  );
}

SH_VALUE SHRCoefficientsL2CalculateIrradianceGeomerics(const in SHCoefficientsL2 sh, const in SH_VEC3 normal) {
  SH_VALUE R0 = max(1e-5, sh.coefficients[0]);
  SH_VEC3 R1 = 0.5 * SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]);
  SH_VALUE lenR1 = max(1e-5, length(R1));
  SH_VALUE q = 0.5 * (1.0 + dot(R1 / lenR1, normal));
  SH_VALUE p = 1.0 + ((2.0 * lenR1) / R0);
  SH_VALUE a = ((1.0 - (lenR1 / R0)) / (1.0 + (lenR1 / R0)));
  return R0 * (a + (1.0 - a) * (p + 1.0) * pow(abs(q), p));
}

SH_RGB SHRGBCoefficientsL2CalculateIrradianceGeomerics(const in SHRGBCoefficientsL2 sh, const in SH_VEC3 normal) {
  return SH_VEC3(
    SHRCoefficientsL2CalculateIrradianceGeomerics(
      SHCoefficientsL2(
        SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
          sh.coefficients[0].x,
          sh.coefficients[1].x,
          sh.coefficients[2].x,
          sh.coefficients[3].x,
          sh.coefficients[4].x,
          sh.coefficients[5].x,
          sh.coefficients[6].x,
          sh.coefficients[7].x,
          sh.coefficients[8].x
        )
      ),
      normal
    ),
    SHRCoefficientsL2CalculateIrradianceGeomerics(
      SHCoefficientsL2(
        SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
          sh.coefficients[0].y,
          sh.coefficients[1].y,
          sh.coefficients[2].y,
          sh.coefficients[3].y,
          sh.coefficients[4].y,
          sh.coefficients[5].y,
          sh.coefficients[6].y,
          sh.coefficients[7].y,
          sh.coefficients[8].y
        )
      ),
      normal
    ),
    SHRCoefficientsL2CalculateIrradianceGeomerics(
      SHCoefficientsL2(
        SH_VALUE[SH_L2_COUNT_COEFFICIENTS](
          sh.coefficients[0].z,
          sh.coefficients[1].z,
          sh.coefficients[2].z,
          sh.coefficients[3].z,
          sh.coefficients[4].z,
          sh.coefficients[5].z,
          sh.coefficients[6].z,
          sh.coefficients[7].z,
          sh.coefficients[8].z
        )
      ),
      normal
    )
  );
}

SH_VALUE SHCoefficientsL1CalculateIrradianceL3ZoneHarmonicsHallucinate(const in SHCoefficientsL1 sh, const in SH_VEC3 normal) {
  SH_VEC3 zonalAxis = normalize(SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]));
  SH_VALUE ratio = abs(dot(SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]), zonalAxis)) / sh.coefficients[0];
  SH_VALUE zonalL2Coeff = sh.coefficients[0] * ((0.08 * ratio) + (0.6 * (ratio * ratio)));
  SH_VALUE fZ = dot(zonalAxis, normal);
  SH_VALUE zhDir = sqrt(5.0 / (16.0 * SH_PI)) * ((3.0 * (fZ * fZ)) - 1.0);
  SH_VALUE baseIrradiance = SHRCoefficientsL1CalculateIrradiance(sh, normal);
  return baseIrradiance + ((SH_PI * 0.25) * zonalL2Coeff * zhDir);
}

SH_RGB SHRGBCoefficientsL1CalculateIrradianceL3ZoneHarmonicsHallucinate(const in SHRGBCoefficientsL1 sh, const in SH_VEC3 normal) {
  SH_VEC3 lumCoefficients = SH_VEC3(0.2126, 0.7152, 0.0722);
  SH_VEC3 zonalAxis = normalize(SH_VEC3(dot(sh.coefficients[3], lumCoefficients), dot(sh.coefficients[1], lumCoefficients), dot(sh.coefficients[2], lumCoefficients)));
  SH_VEC3 ratio = SH_VEC3(
    abs(dot(SH_VEC3(sh.coefficients[3].x, sh.coefficients[1].x, sh.coefficients[2].x), zonalAxis)) / sh.coefficients[0].x,
    abs(dot(SH_VEC3(sh.coefficients[3].y, sh.coefficients[1].y, sh.coefficients[2].y), zonalAxis)) / sh.coefficients[0].y,
    abs(dot(SH_VEC3(sh.coefficients[3].z, sh.coefficients[1].z, sh.coefficients[2].z), zonalAxis)) / sh.coefficients[0].z
  );
  SH_VEC3 zonalL2Coeff = sh.coefficients[0] * ((0.08 * ratio) + (0.6 * (ratio * ratio)));
  SH_VALUE fZ = dot(zonalAxis, normal);
  SH_VALUE zhDir = sqrt(5.0 / (16.0 * SH_PI)) * ((3.0 * (fZ * fZ)) - 1.0);
  SH_RGB baseIrradiance = SHRGBCoefficientsL1CalculateIrradiance(sh, normal);
  return baseIrradiance + ((SH_PI * 0.25) * zonalL2Coeff * zhDir);
}

SH_VALUE SHCoefficientsL2CalculateIrradianceL3ZoneHarmonicsHallucinate(const in SHCoefficientsL2 sh, const in SH_VEC3 normal) {
  SH_VEC3 zonalAxis = normalize(SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]));
  SH_VALUE ratio = abs(dot(SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]), zonalAxis)) / sh.coefficients[0];
  SH_VALUE zonalL2Coeff = sh.coefficients[0] * ((0.08 * ratio) + (0.6 * (ratio * ratio)));
  SH_VALUE fZ = dot(zonalAxis, normal);
  SH_VALUE zhDir = sqrt(5.0 / (16.0 * SH_PI)) * ((3.0 * (fZ * fZ)) - 1.0);
  SH_VALUE baseIrradiance = SHRCoefficientsL2CalculateIrradiance(sh, normal);
  return baseIrradiance + ((SH_PI * 0.25) * zonalL2Coeff * zhDir);
}

SH_RGB SHRGBCoefficientsL2CalculateIrradianceL3ZoneHarmonicsHallucinate(const in SHRGBCoefficientsL2 sh, const in SH_VEC3 normal) {
  SH_VEC3 lumCoefficients = SH_VEC3(0.2126, 0.7152, 0.0722);
  SH_VEC3 zonalAxis = normalize(SH_VEC3(dot(sh.coefficients[3], lumCoefficients), dot(sh.coefficients[1], lumCoefficients), dot(sh.coefficients[2], lumCoefficients)));
  SH_VEC3 ratio = SH_VEC3(
    abs(dot(SH_VEC3(sh.coefficients[3].x, sh.coefficients[1].x, sh.coefficients[2].x), zonalAxis)) / sh.coefficients[0].x,
    abs(dot(SH_VEC3(sh.coefficients[3].y, sh.coefficients[1].y, sh.coefficients[2].y), zonalAxis)) / sh.coefficients[0].y,
    abs(dot(SH_VEC3(sh.coefficients[3].z, sh.coefficients[1].z, sh.coefficients[2].z), zonalAxis)) / sh.coefficients[0].z
  );
  SH_VEC3 zonalL2Coeff = sh.coefficients[0] * ((0.08 * ratio) + (0.6 * (ratio * ratio)));
  SH_VALUE fZ = dot(zonalAxis, normal);
  SH_VALUE zhDir = sqrt(5.0 / (16.0 * SH_PI)) * ((3.0 * (fZ * fZ)) - 1.0);
  SH_RGB baseIrradiance = SHRGBCoefficientsL2CalculateIrradiance(sh, normal);
  return baseIrradiance + ((SH_PI * 0.25) * zonalL2Coeff * zhDir);
}

SH_VEC2 ApproximateGGXAsL1ZH(const in SH_VALUE ggxAlpha) {
  return SH_VEC2(1.0, 1.66711256633276 / (1.65715038133932 + ggxAlpha));
}

SH_VEC3 ApproximateGGXAsL2ZH(const in SH_VALUE ggxAlpha) {
  return SH_VEC3(
    1.0,
    1.66711256633276 / (1.65715038133932 + ggxAlpha),
    (1.56127990596116 / (0.96989757593282 + ggxAlpha)) - 0.599972342361123
  );
}

SHCoefficientsL1 SHCoefficientsL1ConvolveWithGGX(const in SHCoefficientsL1 sh, const in SH_VALUE ggxAlpha) {
  return SHCoefficientsL1ConvolveWithZonalHarmonics(sh, ApproximateGGXAsL1ZH(ggxAlpha));
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1ConvolveWithGGX(const in SHRGBCoefficientsL1 sh, const in SH_VALUE ggxAlpha) {
  return SHRGBCoefficientsL1ConvolveWithZonalHarmonics(sh, ApproximateGGXAsL1ZH(ggxAlpha));
}

SHCoefficientsL2 SHCoefficientsL2ConvolveWithGGX(const in SHCoefficientsL2 sh, const in SH_VALUE ggxAlpha) {
  return SHCoefficientsL2ConvolveWithZonalHarmonics(sh, ApproximateGGXAsL2ZH(ggxAlpha));
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2ConvolveWithGGX(const in SHRGBCoefficientsL2 sh, const in SH_VALUE ggxAlpha) {
  return SHRGBCoefficientsL2ConvolveWithZonalHarmonics(sh, ApproximateGGXAsL2ZH(ggxAlpha));
}

void SHCoefficientsL1ExtractSpecularDirectionalLight(const in SHCoefficientsL1 sh, const in SH_VALUE sqrtRoughness, out SH_VEC3 direction, out SH_VALUE intensity, out SH_VALUE modifiedSqrtRoughness) {
  SH_VEC3 avgL1 = SH_VEC3(sh.coefficients[3], sh.coefficients[1], sh.coefficients[2]) * 0.5;
  SH_VALUE avgL1len = max(1e-5, length(avgL1));
  direction = avgL1 / avgL1len;
  intensity = EvaluateSHCoefficientsL1(sh, direction) * SH_PI;
  modifiedSqrtRoughness = clamp(sqrtRoughness / sqrt(avgL1len), 0.0, 1.0);
}

void SHRGBCoefficientsL1ExtractSpecularDirectionalLight(const in SHRGBCoefficientsL1 sh, const in SH_VALUE sqrtRoughness, out SH_VEC3 direction, out SH_RGB color, out SH_VALUE modifiedSqrtRoughness) {
  SH_VEC3 avgL1 = SH_VEC3(
    dot(sh.coefficients[3] / sh.coefficients[0], SH_VEC3(1.0 / 3.0)),
    dot(sh.coefficients[1] / sh.coefficients[0], SH_VEC3(1.0 / 3.0)),
    dot(sh.coefficients[2] / sh.coefficients[0], SH_VEC3(1.0 / 3.0))
  ) * 0.5;
  SH_VALUE avgL1len = max(1e-5, length(avgL1));
  direction = avgL1 / avgL1len;
  color = EvaluateSHRGBCoefficientsL1(sh, direction) * SH_PI;
  modifiedSqrtRoughness = clamp(sqrtRoughness / sqrt(avgL1len), 0.0, 1.0);
}

SHCoefficientsL1 SHCoefficientsL1Rotate(const in SHCoefficientsL1 sh, const in mat3 rotation) {
  const SH_VALUE r00 = rotation[0][0];
  const SH_VALUE r10 = rotation[1][0];
  const SH_VALUE r20 = -rotation[2][0];
  const SH_VALUE r01 = rotation[0][1];
  const SH_VALUE r11 = rotation[1][1];
  const SH_VALUE r21 = -rotation[2][1];
  const SH_VALUE r02 = -rotation[0][2];
  const SH_VALUE r12 = -rotation[1][2];
  const SH_VALUE r22 = rotation[2][2];
  return SHCoefficientsL1(
    SH_VALUE[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0],
      ((r11 * sh.coefficients[1]) - (r12 * sh.coefficients[2]) + (r10 * sh.coefficients[3])),
      ((-r21 * sh.coefficients[1]) + (r22 * sh.coefficients[2]) - (r20 * sh.coefficients[3])),
      ((r01 * sh.coefficients[1]) - (r02 * sh.coefficients[2]) + (r00 * sh.coefficients[3]))
    )
  );  
}

SHRGBCoefficientsL1 SHRGBCoefficientsL1Rotate(const in SHRGBCoefficientsL1 sh, const in mat3 rotation) {
  const SH_VALUE r00 = rotation[0][0];
  const SH_VALUE r10 = rotation[1][0];
  const SH_VALUE r20 = -rotation[2][0];
  const SH_VALUE r01 = rotation[0][1];
  const SH_VALUE r11 = rotation[1][1];
  const SH_VALUE r21 = -rotation[2][1];
  const SH_VALUE r02 = -rotation[0][2];
  const SH_VALUE r12 = -rotation[1][2];
  const SH_VALUE r22 = rotation[2][2];
  return SHRGBCoefficientsL1(
    SH_RGB[SH_L1_COUNT_COEFFICIENTS](
      sh.coefficients[0],
      ((r11 * sh.coefficients[1]) - (r12 * sh.coefficients[2]) + (r10 * sh.coefficients[3])),
      ((-r21 * sh.coefficients[1]) + (r22 * sh.coefficients[2]) - (r20 * sh.coefficients[3])),
      ((r01 * sh.coefficients[1]) - (r02 * sh.coefficients[2]) + (r00 * sh.coefficients[3]))
    )
  );  
}

SHCoefficientsL2 SHCoefficientsL2Rotate(const in SHCoefficientsL2 sh, const in mat3 rotation) {
  const SH_VALUE r00 = rotation[0][0];
  const SH_VALUE r10 = rotation[1][0];
  const SH_VALUE r20 = -rotation[2][0];
  const SH_VALUE r01 = rotation[0][1];
  const SH_VALUE r11 = rotation[1][1];
  const SH_VALUE r21 = -rotation[2][1];
  const SH_VALUE r02 = -rotation[0][2];
  const SH_VALUE r12 = -rotation[1][2];
  const SH_VALUE r22 = rotation[2][2];
  SHCoefficientsL2 result;
  result.coefficients[0] = sh.coefficients[0];
  result.coefficients[1] = ((r11 * sh.coefficients[1]) - (r12 * sh.coefficients[2]) + (r10 * sh.coefficients[3]));
  result.coefficients[2] = ((-r21 * sh.coefficients[1]) + (r22 * sh.coefficients[2]) - (r20 * sh.coefficients[3]));
  result.coefficients[3] = ((r01 * sh.coefficients[1]) - (r02 * sh.coefficients[2]) + (r00 * sh.coefficients[3]));
  const SH_VALUE t41 = r01 * r00;
  const SH_VALUE t43 = r11 * r10;
  const SH_VALUE t48 = r11 * r12;
  const SH_VALUE t50 = r01 * r02;
  const SH_VALUE t55 = r02 * r02;
  const SH_VALUE t57 = r22 * r22;
  const SH_VALUE t58 = r12 * r12;
  const SH_VALUE t61 = r00 * r02;
  const SH_VALUE t63 = r10 * r12;
  const SH_VALUE t68 = r10 * r10;
  const SH_VALUE t70 = r01 * r01;
  const SH_VALUE t72 = r11 * r11;
  const SH_VALUE t74 = r00 * r00;
  const SH_VALUE t76 = r21 * r21;
  const SH_VALUE t78 = r20 * r20;
  const SH_VALUE v173 = 0.1732050808;
  const SH_VALUE v577 = 0.5773502693;
  const SH_VALUE v115 = 0.1154700539;
  const SH_VALUE v288 = 0.2886751347;
  const SH_VALUE v866 = 0.8660254040;
  SH_VALUE r[25];
  r[0] = (r11 * r00) + (r01 * r10);
  r[1] = (-(r01 * r12)) - (r11 * r02);
  r[2] = v173 * r02 * r12;
  r[3] = (-(r10 * r02)) - (r00 * r12);
  r[4] = (r00 * r10) - (r01 * r11);
  r[5] = (-(r11 * r20)) - (r21 * r10);
  r[6] = (r11 * r22) + (r21 * r12);
  r[7] = -(v173 * r22 * r12);
  r[8] = (r20 * r12) + (r10 * r22);
  r[9] = (-(r10 * r20)) + (r11 * r21);
  r[10] = (-(v577 * (t41 + t43))) + (v115 * r21 * r20);
  r[11] = (v577 * (t48 + t50)) - (v115 * r21 * r22);
  r[12] = ((-0.5) * (t55 + t58)) + t57;
  r[13] = (v577 * (t61 + t63)) - (v115 * r20 * r22);
  r[14] = (v288 * (((t70 - t68) + t72) - t74)) - (v577 * (t76 - t78));
  r[15] = (-(r01 * r20)) - (r21 * r00);
  r[16] = (r01 * r22) + (r21 * r02);
  r[17] = -(v173 * r22 * r02);
  r[18] = (r00 * r22) + (r20 * r02);
  r[19] = (-(r00 * r20)) + (r01 * r21);
  r[20] = t41 - t43;
  r[21] = ((-t50) + t48);
  r[22] = v866 * (t55 - t58);
  r[23] = t63 - t61;
  r[24] = 0.5 * (((t74 - t68) - t70) + t72);
  [[unroll]] for(int i = 0; i < 5; i++){
    const int base = i * 5;
    result.coefficients[4 + i] = ((r[base + 0] * sh.coefficients[4]) + (r[base + 1] * sh.coefficients[5]) + (r[base + 2] * sh.coefficients[6]) + (r[base + 3] * sh.coefficients[7]) + (r[base + 4] * sh.coefficients[8]));
  }
  return result;
}

SHRGBCoefficientsL2 SHRGBCoefficientsL2Rotate(const in SHRGBCoefficientsL2 sh, const in mat3 rotation) {
  const SH_VALUE r00 = rotation[0][0];
  const SH_VALUE r10 = rotation[1][0];
  const SH_VALUE r20 = -rotation[2][0];
  const SH_VALUE r01 = rotation[0][1];
  const SH_VALUE r11 = rotation[1][1];
  const SH_VALUE r21 = -rotation[2][1];
  const SH_VALUE r02 = -rotation[0][2];
  const SH_VALUE r12 = -rotation[1][2];
  const SH_VALUE r22 = rotation[2][2];
  SHRGBCoefficientsL2 result;
  result.coefficients[0] = sh.coefficients[0];
  result.coefficients[1] = ((r11 * sh.coefficients[1]) - (r12 * sh.coefficients[2]) + (r10 * sh.coefficients[3]));
  result.coefficients[2] = ((-r21 * sh.coefficients[1]) + (r22 * sh.coefficients[2]) - (r20 * sh.coefficients[3]));
  result.coefficients[3] = ((r01 * sh.coefficients[1]) - (r02 * sh.coefficients[2]) + (r00 * sh.coefficients[3]));
  const SH_VALUE t41 = r01 * r00;
  const SH_VALUE t43 = r11 * r10;
  const SH_VALUE t48 = r11 * r12;
  const SH_VALUE t50 = r01 * r02;
  const SH_VALUE t55 = r02 * r02;
  const SH_VALUE t57 = r22 * r22;
  const SH_VALUE t58 = r12 * r12;
  const SH_VALUE t61 = r00 * r02;
  const SH_VALUE t63 = r10 * r12;
  const SH_VALUE t68 = r10 * r10;
  const SH_VALUE t70 = r01 * r01;
  const SH_VALUE t72 = r11 * r11;
  const SH_VALUE t74 = r00 * r00;
  const SH_VALUE t76 = r21 * r21;
  const SH_VALUE t78 = r20 * r20;
  const SH_VALUE v173 = 0.1732050808;
  const SH_VALUE v577 = 0.5773502693;
  const SH_VALUE v115 = 0.1154700539;
  const SH_VALUE v288 = 0.2886751347;
  const SH_VALUE v866 = 0.8660254040;
  SH_VALUE r[25];
  r[0] = (r11 * r00) + (r01 * r10);
  r[1] = (-(r01 * r12)) - (r11 * r02);
  r[2] = v173 * r02 * r12;
  r[3] = (-(r10 * r02)) - (r00 * r12);
  r[4] = (r00 * r10) - (r01 * r11);
  r[5] = (-(r11 * r20)) - (r21 * r10);
  r[6] = (r11 * r22) + (r21 * r12);
  r[7] = -(v173 * r22 * r12);
  r[8] = (r20 * r12) + (r10 * r22);
  r[9] = (-(r10 * r20)) + (r11 * r21);
  r[10] = (-(v577 * (t41 + t43))) + (v115 * r21 * r20);
  r[11] = (v577 * (t48 + t50)) - (v115 * r21 * r22);
  r[12] = ((-0.5) * (t55 + t58)) + t57;
  r[13] = (v577 * (t61 + t63)) - (v115 * r20 * r22);
  r[14] = (v288 * (((t70 - t68) + t72) - t74)) - (v577 * (t76 - t78));
  r[15] = (-(r01 * r20)) - (r21 * r00);
  r[16] = (r01 * r22) + (r21 * r02);
  r[17] = -(v173 * r22 * r02);
  r[18] = (r00 * r22) + (r20 * r02);
  r[19] = (-(r00 * r20)) + (r01 * r21);
  r[20] = t41 - t43;
  r[21] = ((-t50) + t48);
  r[22] = v866 * (t55 - t58);
  r[23] = t63 - t61;
  r[24] = 0.5 * (((t74 - t68) - t70) + t72);
  [[unroll]] for(int i = 0; i < 5; i++){
    const int base = i * 5;
    result.coefficients[4 + i] = ((r[base + 0] * sh.coefficients[4]) + (r[base + 1] * sh.coefficients[5]) + (r[base + 2] * sh.coefficients[6]) + (r[base + 3] * sh.coefficients[7]) + (r[base + 4] * sh.coefficients[8]));
  }
  return result;
}

#endif