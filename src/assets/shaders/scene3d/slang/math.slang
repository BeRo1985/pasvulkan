// Shared matrix and geometry math helpers (mirrors adjugate.glsl and the active variant of
// projectsphere.glsl).
//
// Matrix convention: with -matrix-layout-row-major Slang loads the matrices in the same GLSL
// row/column order, so m[i] is column i, m[c][r] is the GLSL element and float3x3(c0, c1, c2) is
// built from columns -- the indexing ports verbatim, no transpose. For the multiply we use the
// mulmv / mulvm / mulmm wrappers below, called in GLSL operand order (mulmv(m, v) = GLSL m * v,
// mulmm(a, b) = GLSL a * b). They hide the one quirk that under row-major HLSL mul() takes the
// operands reversed; this keeps pure Slang (no -allow-glsl) with GLSL-readable call sites and no
// raw-mul footgun.

module math;

// GLSL-order matrix-multiply wrappers. Under -matrix-layout-row-major, HLSL mul() reverses the
// operands -- GPU-verified: mul(v, m) computes M*v, mul(b, a) computes A*B -- so the reversal lives
// here only and every call site reads in natural GLSL order.

// GLSL m * v
[ForceInline] public float4 mulmv(float4x4 m, float4 v) {
  return mul(v, m);
}

// GLSL m * v (3x3)
[ForceInline] public float3 mulmv(float3x3 m, float3 v) {
  return mul(v, m);
}

// GLSL v * m (row vector)
[ForceInline] public float4 mulvm(float4 v, float4x4 m) {
  return mul(m, v);
}

// GLSL v * m (row vector, 3x3)
[ForceInline] public float3 mulvm(float3 v, float3x3 m) {
  return mul(m, v);
}

// GLSL a * b
[ForceInline] public float4x4 mulmm(float4x4 a, float4x4 b) {
  return mul(b, a);
}

// GLSL a * b (3x3)
[ForceInline] public float3x3 mulmm(float3x3 a, float3x3 b) {
  return mul(b, a);
}

// adjugate(M) = det(M) * transpose(inverse(M)): the robust normal-transform matrix (defined even
// for singular M, no inverse needed). It carries a det(M) factor; for det(M) >= 0 that is a
// positive scale that normalize() removes.
public float3x3 adjugate(float4x4 m) {
  return float3x3(cross(m[1].xyz, m[2].xyz), cross(m[2].xyz, m[0].xyz), cross(m[0].xyz, m[1].xyz));
}

// Orientation-correct variant: re-applies sign(det(M)) so the result matches transpose(inverse(M)).
// For det(M) < 0 (mirrored transforms, e.g. a negative scale axis) this keeps the shading normal
// flipped consistently with the rasterizer winding flip. det(M) == 0 falls back to plain adjugate.
public float3x3 adjugateSignDet(float4x4 m) {
  float3 c0 = cross(m[1].xyz, m[2].xyz);
  float3x3 a = float3x3(c0, cross(m[2].xyz, m[0].xyz), cross(m[0].xyz, m[1].xyz));
  return (dot(m[0].xyz, c0) < 0.0) ? (-a) : a;  // dot(m[0].xyz, c0) = det(M)
}

// View-space sphere to screen-space AABB projection for Hi-Z occlusion culling (projectsphere.glsl
// variant 3, the robust one). The center is in view space with negative z into the screen, so the
// sphere is past the near plane when ((-center.z) - radius) < zNear. The returned aabb is xy = min
// corner, zw = max corner in [0,1] screen space; returns false when the sphere is behind zNear.
public bool projectSphere(float3 center, const float radius, const float zNear, const float4x4 projectionMatrix, out float4 aabb, bool zNearTest) {

  if (zNearTest && (((-center.z) - radius) < zNear)) {

    aabb = float4(0.0);
    return false;

  } else {

    // Just conservative enough to avoid false negatives.
    center.z = min(-zNear, center.z);  // clamp center to the near plane

    float3 screenMin = mulmv(projectionMatrix, float4(center - float2(radius, 0.0).xxy, 1.0)).xyw;
    screenMin.xy /= screenMin.z;

    float3 screenMax = mulmv(projectionMatrix, float4(center + float2(radius, 0.0).xxy, 1.0)).xyw;
    screenMax.xy /= screenMax.z;

    aabb = fma(
      float4(min(screenMin.xy, screenMax.xy), max(screenMin.xy, screenMax.xy)),
      float4(0.5),
      float4(0.5)
    );

    return true;

  }
}
