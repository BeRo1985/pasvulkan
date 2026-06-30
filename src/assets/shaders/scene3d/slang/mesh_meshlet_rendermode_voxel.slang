// The Voxelization render mode for the meshlet path: each meshlet is tested against every voxel
// cascade AABB and emitted once per (meshlet, cascade, chunk), and the mesh stage projects each
// triangle along its dominant axis into the cascade. The shared IMeshRenderMode contract lives in
// mesh_meshlet_config.

module mesh_meshlet_rendermode_voxel;

import mesh_meshlet_config;
import voxelgriddata;

// Voxel grid cascade data (set 1, binding 1): cascade AABBs / transforms for the task cull and the
// mesh per-primitive projection. Only the voxel entries reference it.
[[vk::binding(1, 1)]] public ConstantBuffer<VoxelGridData> voxelGridData;

// Voxel payload: one entry per visible (meshlet, cascade, chunk) triple. A meshlet with more than
// MAX_VOXEL_MESHLET_PRIMITIVES (42) triangles is split into chunks so every triangle is voxelized;
// the layer entry packs the chunk in the high bits and the cascade in the low bits, i.e.
// (chunk << VOXEL_CASCADE_BITS) | cascade.
public struct VoxelTaskPayload {
  public uint meshletDescriptorBase;
  public uint meshObjectID;
  public uint count;
  public uint meshletLocalIndices[TASK_PAYLOAD_CAPACITY_VOXEL];
  public uint layerIndices[TASK_PAYLOAD_CAPACITY_VOXEL];
}

public struct VoxelMode : IMeshRenderMode {
  public typealias Payload = VoxelTaskPayload;
}
