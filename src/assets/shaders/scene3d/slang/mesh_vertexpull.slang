// BDA vertex pulling for the mesh path.
//
// Fetches a vertex from the global big-buffer via buffer device address (the same global vertex
// index the CPU-side compaction and the mesh cull compute shader use) and transforms it into clip
// space using the per-draw world transform from the MatrixPair buffer and the view matrices.

module mesh_vertexpull;

import gputypes;
import mesh_bindings;

// One pulled and transformed vertex.
public struct PulledVertex {
  public float4 clipSpacePosition;
  public float3 worldSpacePosition;
  public float2 texCoord0;
  public uint materialID;
}

public PulledVertex pullVertex(uint vertexID, uint instanceID, uint viewIndex) {
  DrawInfo drawInfo = drawInfoItems[instanceID];
  GlobalBDAPointers bda = globalBDAPointersBuffer[0];

  // Global big-buffer vertex index, like mesh.vert.
  PackedCachedVertex cv = bda.cachedVertices[vertexID];
  PackedStaticVertex sv = bda.staticVertices[vertexID];

  float3 position = float3(cv.posX, cv.posY, cv.posZ);

  // matrixID 0 = identity sentinel for pre-transformed meshes.
  MatrixPair matrixPair = bda.matrixPairs[drawInfo.matrixID];
  float4x4 modelMatrix = matrixPair.modelMatrix;

  View view = uView.views[viewIndex];

  float4 worldSpacePosition = modelMatrix * float4(position, 1.0);
  float4 clipSpacePosition = view.projectionMatrix * (view.viewMatrix * worldSpacePosition);

  PulledVertex result;
  result.clipSpacePosition = clipSpacePosition;
  result.worldSpacePosition = worldSpacePosition.xyz;
  result.texCoord0 = sv.texCoord0;
  result.materialID = sv.materialID;
  return result;
}
