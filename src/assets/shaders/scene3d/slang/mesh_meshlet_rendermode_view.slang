// The View render mode for the meshlet path: standard camera rendering, culling each meshlet
// against every active view (frustum + Hi-Z) and emitting it once if any view sees it. The shared
// IMeshRenderMode contract lives in mesh_meshlet_config.

module mesh_meshlet_rendermode_view;

import mesh_meshlet_config;

// View payload: the visible-meshlet list for one task workgroup (one entry per surviving meshlet).
public struct ViewTaskPayload {
  public uint meshletDescriptorBase;                              // base index into meshletDescriptors[]
  public uint meshObjectID;                                       // DrawInfo index (model matrix, material, ...)
  public uint count;                                              // number of meshlets emitted
  public uint meshletLocalIndices[TASK_PAYLOAD_CAPACITY_VIEW];    // local indices relative to the base
}

public struct ViewMode : IMeshRenderMode {
  public typealias Payload = ViewTaskPayload;
}
