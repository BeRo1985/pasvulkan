// Frustum plane extraction and sphere / AABB culling tests. Mirrors frustum.glsl.
//
// With -matrix-layout-row-major the GLSL row/column order ports verbatim: transpose(m) is the real
// matrix transpose, t[i] is column i, m[c][r] is the GLSL element. frustumSetup returns the Frustum
// instead of taking an out parameter (the only deviation from the GLSL signature).

module frustum;

public static const int FrustumLeft = 0;
public static const int FrustumRight = 1;
public static const int FrustumBottom = 2;
public static const int FrustumTop = 3;
public static const int FrustumNear = 4;
public static const int FrustumFar = 5;

public struct Frustum {
  public float4 planes[6];
  public int maximumPlaneSide;
}

public Frustum frustumSetup(float4x4 m) {
  Frustum frustum;

  // Only for 0.0 .. 1.0 depth range projection matrices.
  bool reverseZ = m[2][3] < -1e-7;
  bool infiniteFarPlane = reverseZ && ((abs(m[2][2]) < 1e-7) && (abs(m[3][2]) > 1e-7));
  frustum.maximumPlaneSide = infiniteFarPlane ? FrustumNear : FrustumFar;  // avoid far-plane culling for infinite-far frustums
  float4x4 t = transpose(m);
  frustum.planes[FrustumLeft] = normalize(t[3] + t[0]);
  frustum.planes[FrustumRight] = normalize(t[3] - t[0]);
  frustum.planes[FrustumBottom] = normalize(t[3] + t[1]);
  frustum.planes[FrustumTop] = normalize(t[3] - t[1]);
  if (reverseZ) {
    frustum.planes[FrustumNear] = normalize(t[3] - t[2]);
    frustum.planes[FrustumFar] = normalize(t[2]);
  } else {
    frustum.planes[FrustumNear] = normalize(t[2]);
    frustum.planes[FrustumFar] = normalize(t[3] - t[2]);
  }
  return frustum;
}

public bool frustumCullingAABBTest(const Frustum frustum, float3 aabbMin, float3 aabbMax) {
  for (int frustumPlaneIndex = 0; frustumPlaneIndex <= frustum.maximumPlaneSide; frustumPlaneIndex++) {
    float4 plane = frustum.planes[frustumPlaneIndex];
    float3 corner = lerp(aabbMin, aabbMax, float3(plane.xyz > float3(0.0)));
    if (dot(float4(corner, 1.0), plane) < 0.0) {
      return false;
    }
  }
  return true;
}

public bool frustumCullingSphereTest(const Frustum frustum, float4 sphere) {
  for (int frustumPlaneIndex = 0; frustumPlaneIndex <= frustum.maximumPlaneSide; frustumPlaneIndex++) {
    if (dot(float4(sphere.xyz, 1.0), frustum.planes[frustumPlaneIndex]) < -sphere.w) {
      return false;
    }
  }
  return true;
}
