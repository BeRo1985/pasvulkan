// Shared debug-visualization helpers.
//
// meshletDebugColor mirrors the active hash variant in meshlet.glsl: a deterministic hash of an id
// into a distinct color, for visualizing meshlet boundaries (or any per-id grouping).

module debug;

public float3 meshletDebugColor(uint id) {
  id = (id ^ (id >> 17u)) * 0xed5ad4bbu;
  id = (id ^ (id >> 11u)) * 0xac4c1b51u;
  id = (id ^ (id >> 15u)) * 0x31848babu;
  float3 color = float3(uint3((uint3(id ^ (id >> 14u)) >> uint3(0u, 8u, 16u)) & uint3(0xffu))) * (1.0 / 255.0);

  color = clamp(color, float3(0.0), float3(1.0));

  // Lift very dark colors so boundaries stay visible on dark backgrounds.
  if (dot(color, float3(0.2126, 0.7152, 0.0722)) < 0.125) {
    color = lerp(float3(0.125), float3(1.0), color);
  }
  return color;
}
