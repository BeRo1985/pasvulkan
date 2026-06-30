// The RSM / layer-routing render mode for the meshlet path: like the View mode it culls each meshlet
// against every active view (frustum + Hi-Z), but instead of emitting once and relying on multiview
// it emits one entry per visible meshlet x view pair and the mesh stage routes each primitive to the
// matching array layer via gl_Layer (for reflective shadow maps / CSM cascades without multiview).
// The shared IMeshRenderMode contract lives in mesh_meshlet_config.

module mesh_meshlet_rendermode_rsm;

import mesh_meshlet_config;

// RSM payload: one entry per visible (meshlet, view) pair. Capacity matches the View mode
// (TASK_GROUP_SIZE), with the per-entry layer (view / cascade index) carried alongside.
public struct RSMTaskPayload {
  public uint meshletDescriptorBase;
  public uint meshObjectID;
  public uint count;
  public uint meshletLocalIndices[TASK_PAYLOAD_CAPACITY_VIEW];
  public uint layerIndices[TASK_PAYLOAD_CAPACITY_VIEW];
}

public struct RSMMode : IMeshRenderMode {
  public typealias Payload = RSMTaskPayload;
}
