// Fragment-stage entry for the mesh geometry path, consuming the shared VertexInputVarying produced
// by either the vertex-stage or the mesh-shader path. For now a debug visualization of materialID
// and the interpolated UVs; the full mesh.frag shading port comes later.

import gputypes;
import mesh_bindings;
import mesh_globals;

[shader("pixel")]
float4 fragmentMain(VertexInputVarying input) : SV_Target0 {
  float3 debugColor = float3(frac(float(input.materialID) * 0.125), frac(input.texCoord0));
  return float4(debugColor, 1.0);
}
