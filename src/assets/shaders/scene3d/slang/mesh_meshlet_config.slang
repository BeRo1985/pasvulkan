// Compile-time configuration for the mesh-shader path.
//
// The boolean/int axes are specialization constants, set at pipeline creation via
// VkSpecializationInfo, so a single SPIR-V per render-mode entry point covers all flag combinations
// without a build-time permutation explosion. Axes that change the entry-point I/O signature are
// separate entry points instead: the render mode (View / Voxelization / RSM), because each needs a
// different compile-time mesh output layout, and the no-task-shader variant, because it drops the
// task payload input. The shared per-mode logic goes through the render-mode structs.

module mesh_meshlet_config;

// Specialization constants (resolved at pipeline creation).
[SpecializationConstant] public const bool velocity      = false;
[SpecializationConstant] public const int  cullPass       = 1;     // 0 = frustum only, 1 = + Hi-Z
[SpecializationConstant] public const bool selectionMask = false;

// A render mode parameterizes the task / mesh entry points with its task->mesh payload type (the
// capacity differs sharply: View / RSM emit at most one entry per meshlet, Voxelization one per
// (meshlet, cascade, chunk)). The mesh output layout must be a compile-time constant, so each mode
// is a separate pair of entry points; this interface is the contract they share.
public interface IMeshRenderMode {
  associatedtype Payload;
}

// Constants mirroring mesh.task / mesh.mesh.
public static const uint TASK_GROUP_SIZE = 32;
public static const uint MESH_GROUP_SIZE = 64;
public static const uint MAX_MESHLET_VERTICES = 64;
public static const uint MAX_MESHLET_PRIMITIVES = 126;
public static const uint MAX_VOXEL_MESHLET_PRIMITIVES = 42;
public static const uint MAX_VOXEL_MESHLET_VERTICES = (MAX_VOXEL_MESHLET_PRIMITIVES * 3);  // 126, vertices are not shared
public static const uint MAX_VOXEL_PRIM_CHUNKS = 3;                 // ceil(126 / 42)
public static const uint VOXEL_CASCADE_BITS = 4;
public static const uint VOXEL_CASCADE_MASK = 0xf;
public static const uint FLAG_MESHLET_CULLING_ENABLED = (1u << 3);

// Task payload capacity per render mode: View / RSM emit at most one entry per meshlet; Voxel emits
// one per (meshlet, cascade, chunk), so up to TASK_GROUP_SIZE * 8 cascades * chunks.
public static const uint TASK_PAYLOAD_CAPACITY_VIEW  = TASK_GROUP_SIZE;
public static const uint TASK_PAYLOAD_CAPACITY_VOXEL = ((TASK_GROUP_SIZE * 8) * MAX_VOXEL_PRIM_CHUNKS);
