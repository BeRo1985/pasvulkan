// Vertex-stage entry for the mesh geometry path (Slang port of mesh.vert). Pulls the vertex via BDA
// (gl_VertexIndex is the global big-buffer vertex index, gl_InstanceIndex the DrawInfo index) and
// runs the shared transform, producing the same VertexInputVarying as the mesh-shader path so the
// fragment shader is identical for both.

import gputypes;
import mesh_bindings;
import mesh_globals;
import mesh_meshlet_config;  // velocity link-time constant

[shader("vertex")]
VertexInputVarying<velocity> vertexMain(uint vertexID : SV_VertexID, uint instanceID : SV_InstanceID, uint viewID : SV_ViewID) {
  uint viewIndex = pushConstants.viewBaseIndex + viewID;
  GlobalBDAPointers bda = globalBDAPointersBuffer[0];
  VertexTransform t = makeVertexTransform(instanceID, viewIndex, bda);
  return transformVertex<velocity>(vertexID, bda, t, instanceID);
}
