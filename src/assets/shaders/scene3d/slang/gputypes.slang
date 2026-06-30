// GPU struct definitions for the Slang mesh path.
//
// These are byte-exact mirrors of the GLSL / Pascal layouts (drawinfo.glsl,
// globaldescriptorset.glsl, mesh_rendering_pass_descriptorset.glsl and the matching
// TGPU* records in PasVulkan.Scene3D.pas). The Slang side is compiled with
// -fvk-use-scalar-layout (so StructuredBuffer / pointer access matches the std430 /
// buffer_reference(std430) layouts the GLSL shaders use), -matrix-layout-row-major (so the matrix
// members load in GLSL row/column order — emitted ColMajor in SPIR-V, matching Pascal's
// column-major upload) and -allow-glsl (enabling the GLSL '*' matrix operator used by the math).
//
// Only the buffer-device-address slots this path dereferences carry a typed pointer; the rest
// stay as uint2 so the overall 112-byte layout stays byte-exact with the GLSL side.

module gputypes;

// 32 bytes, mirrors PackedCachedVertex / TGPUCachedVertex.
public struct PackedCachedVertex {
  public float posX;
  public float posY;
  public float posZ;
  public uint normalXY;             // snorm16(normal.x) | snorm16(normal.y)
  public uint normalZSign;          // snorm16(normal.z) | snorm16(bitangentSign)
  public uint tangentXY;            // snorm16(tangent.x) | snorm16(tangent.y)
  public uint tangentZModelScaleX;  // snorm16(tangent.z) | half(modelScaleX)
  public uint modelScaleYZ;         // half(modelScaleY) | half(modelScaleZ)
}

// 32 bytes, mirrors PackedStaticVertex / TGPUStaticVertex.
public struct PackedStaticVertex {
  public float2 texCoord0;
  public float2 texCoord1;
  public uint colorRG;              // half(r) | half(g)
  public uint colorBA;              // half(b) | half(a)
  public uint materialID;
  public uint unused;
}

// 32 bytes, mirrors drawinfo.glsl DrawInfo.
public struct DrawInfo {
  public uint matrixID;
  public uint instanceDataIndex;
  public uint meshObjectID;
  public uint flags;
  public uint nodeMatricesIndex;
  public uint meshletDescriptorBase;
  public uint meshletBoundingSphereBase;
  public uint meshletVisibilityBase;
}

// 128 bytes, mirrors drawinfo.glsl MatrixPair (current + previous world transform).
public struct MatrixPair {
  public float4x4 modelMatrix;
  public float4x4 previousModelMatrix;
}

// 32 bytes, mirrors drawinfo.glsl MeshletDescriptor.
public struct MeshletDescriptor {
  public float4 boundingSphere;  // xyz = center (object space), w = radius
  public uint vertexOffset;      // into the global meshlet vertex remap buffer
  public uint vertexCount;       // unique vertices in this meshlet
  public uint primitiveOffset;   // into the global meshlet primitive buffer
  public uint primitiveCount;    // triangles in this meshlet
}

// 32 bytes, mirrors drawinfo.glsl MeshDrawCommand (indirect command metadata).
public struct MeshDrawCommand {
  public uint4 cmd0;  // x = groupCountX, y = 1, z = 1, w = meshletBaseIndex
  public uint4 cmd1;  // x = meshletCount, y = meshObjectID, z = boundingSphereIndex, w = flags
}

// 112 bytes, mirrors drawinfo.glsl GlobalBDAPointers. Each slot is a uvec2 device address
// on the GLSL side (8 bytes); the slots this path dereferences are typed device pointers
// (also 8 bytes), the rest stay uint2 to keep the 112-byte layout exact.
public struct GlobalBDAPointers {
  public PackedCachedVertex* cachedVertices;
  public PackedStaticVertex* staticVertices;
  public PackedCachedVertex* previousCachedVertices;
  public uint* generations;
  public uint* previousGenerations;
  public MatrixPair* matrixPairs;
  public uint2 lodInfoBDA;
  public uint2 lodNeededCurrentBDA;
  public MeshletDescriptor* meshletDescriptors;  // meshletDescriptorBDA
  public uint* meshletVertices;                  // meshletVertexBDA (global vertex index remap)
  public uint* meshletPrimitives;                // meshletPrimitiveBDA (3x uint8 per uint32)
  public float4* meshletBoundingSpheres;         // meshletBoundingSphereBDA (per-instance)
  public uint2 nodeMatricesBDA;
  public uint2 cloudsShadowMapBDA;
}

// 256 bytes, mirrors mesh_rendering_pass_descriptorset.glsl View. The UBO is std140 on the
// GLSL side, but the struct is mat4-only so std140 / std430 / scalar are all identical here.
public struct View {
  public float4x4 viewMatrix;
  public float4x4 projectionMatrix;
  public float4x4 inverseViewMatrix;
  public float4x4 inverseProjectionMatrix;
}
