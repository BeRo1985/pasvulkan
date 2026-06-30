// Shared bit-packing / vertex-decode helpers (mirrors the unpack* helpers in drawinfo.glsl).
//
// These do not depend on -allow-glsl: the half pairs go through the HLSL/Slang f16tof32 intrinsic
// and the snorm pairs through the standard sign-extend-and-normalize integer math. The
// PackedCachedVertex / PackedStaticVertex accessors on top of them decode the engine's packed
// vertex layout and are shared by every geometry path.

module utils;

import gputypes;

// Two snorm16 values from the low / high 16 bits, normalized by 32767 and clamped to [-1, 1].
public float2 unpackSnorm2x16(uint p) {
  int lo = int(p << 16) >> 16;  // low 16 bits, arithmetic-shifted to sign-extend
  int hi = int(p) >> 16;        // high 16 bits, sign-extended
  return clamp(float2(float(lo), float(hi)) * (1.0 / 32767.0), float2(-1.0), float2(1.0));
}

// Two half-floats from the low / high 16 bits.
public float2 unpackHalf2x16(uint p) {
  return f16tof32(uint2(p, p >> 16));
}

public float3 unpackPosition(PackedCachedVertex v) {
  return float3(v.posX, v.posY, v.posZ);
}

// xyz = object-space normal, w = bitangent sign.
public float4 unpackNormalSign(PackedCachedVertex v) {
  float2 xy = unpackSnorm2x16(v.normalXY);
  float2 zw = unpackSnorm2x16(v.normalZSign);
  return float4(xy, zw);
}

public float3 unpackTangent(PackedCachedVertex v) {
  float2 xy = unpackSnorm2x16(v.tangentXY);
  float z = unpackSnorm2x16(v.tangentZModelScaleX).x;
  return float3(xy, z);
}

public float3 unpackModelScale(PackedCachedVertex v) {
  float scaleX = unpackHalf2x16(v.tangentZModelScaleX).y;
  float2 scaleYZ = unpackHalf2x16(v.modelScaleYZ);
  return float3(scaleX, scaleYZ);
}

public float4 unpackColor0(PackedStaticVertex v) {
  return float4(unpackHalf2x16(v.colorRG), unpackHalf2x16(v.colorBA));
}
