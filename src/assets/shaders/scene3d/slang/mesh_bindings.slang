// Descriptor-set and push-constant declarations for the mesh path.
//
// These mirror the GLSL descriptor sets exactly (globaldescriptorset.glsl,
// mesh_rendering_pass_descriptorset.glsl, mesh_pushconstants.glsl) so the produced SPIR-V is
// drop-in compatible with the Pascal-side pipeline layout. Everything is public so the other
// modules of the mesh path can reference it.

module mesh_bindings;

import gputypes;

// --- set 0 (global descriptor set) ---

// binding 0: per-draw DrawInfo array (std430 SSBO).
[[vk::binding(0, 0)]] public StructuredBuffer<DrawInfo> drawInfoItems;

// binding 7: single GlobalBDAPointers struct (std430 SSBO).
[[vk::binding(7, 0)]] public StructuredBuffer<GlobalBDAPointers> globalBDAPointersBuffer;

// binding 8: bindless combined 2D samplers.
[[vk::binding(8, 0)]] public Sampler2D u2DTextures[];

// --- set 1 (mesh rendering pass descriptor set) ---

public struct ViewsUBO {
  public View views[256];
}

// binding 0: view matrices UBO (std140).
[[vk::binding(0, 1)]] public ConstantBuffer<ViewsUBO> uView;

// binding 7: bindless Hi-Z depth pyramids (sampler2DArray per view layer), sampled by the meshlet
// task shader for occlusion culling. Only the cull (Pass 1) task stage references it; the
// vertex/fragment and non-Hi-Z entries leave it unused (dead-code-eliminated there).
[[vk::binding(7, 1)]] public Sampler2DArray uTextureDepths[];

// --- push constants (mesh_pushconstants.glsl, with the task/mesh-stage reuse of the trailing
// fields). The vertex/fragment entries read only the leading fields; the meshlet task stage also
// reads meshDrawCommands* (the indirect-command buffer address) and the two cull thresholds. ---

public struct PushConstants {
  public uint viewBaseIndex;
  public uint countViews;
  public uint countAllViews;
  public uint frameIndex;
  public float4 jitter;
  public uint4 timeSecondsTimeFractionalSecondWidthHeight;
  public uint raytracingFlags;
  public uint drawFlags;
  public uint textureDepthIndex;
  public uint meshDrawCommandsBDALow;
  public uint meshDrawCommandsBDAHigh;
  public float maximumDistance;
  public float areaTooSmallThreshold;
}

[[vk::push_constant]] public PushConstants pushConstants;
