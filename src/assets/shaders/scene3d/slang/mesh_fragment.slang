// Fragment-stage entry for the mesh geometry path, consuming the shared VertexInputVarying produced
// by either the vertex-stage or the mesh-shader path. For now a debug visualization of materialID
// and the interpolated UVs; the full mesh.frag shading port comes later.

import gputypes;
import mesh_bindings;
import mesh_globals;
import mesh_meshlet_config;  // velocity link-time constant

// The input type must match the vertex / mesh output (same velocity value), so the fragment is
// variant per velocity too. The debug shading itself uses only always-present fields.
[shader("pixel")]
float4 fragmentMain(VertexInputVarying<velocity> input) : SV_Target0 {
  float3 debugColor = float3(frac(float(input.materialID) * 0.125), frac(input.texCoord0));
  return float4(debugColor, 1.0);
}
