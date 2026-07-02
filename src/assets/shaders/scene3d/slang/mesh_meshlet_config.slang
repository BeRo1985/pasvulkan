// Compile-time configuration for the mesh-shader path.
//
// The boolean/int axes are link-time constants (extern static const): their values are supplied by a
// tiny provider module at link, so each flag combination is a distinct SPIR-V variant (1:1 with the
// GLSL -D variant set). Link-time constants are needed because the varying pruning drives them
// through Conditional<T, flag>, which requires a compile-time value (a specialization constant is
// rejected there). Pure control-flow flags (cullPass / selectionMask) could later be collapsed to
// [SpecializationConstant] to reduce the variant count, since only their declaration changes -- the
// if(flag) usage stays identical. The render mode (View / Voxelization / RSM) and the no-task-shader
// variant remain separate entry points (they change the entry I/O signature).

module mesh_meshlet_config;

// Link-time constants, provided by a small provider module at link (extern = value comes from
// another module). Declared without a default so a variant must always bind a value.
public extern static const bool velocity;
public extern static const int  cullPass;       // 0 = frustum only, 1 = + Hi-Z
public extern static const bool selectionMask;

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
