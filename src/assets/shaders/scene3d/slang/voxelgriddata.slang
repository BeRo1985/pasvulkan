// Voxel cascade grid uniforms, shared by the voxelization and voxel-cone-tracing paths. Mirrors
// voxelgriddata_uniforms.glsl.
//
// On the GLSL side this is a std140 UBO; the struct is built entirely from 16-byte-aligned array
// elements (float4x4 / int4 / float4 / uint4) followed by trailing scalars, so std140 and scalar
// place the field offsets identically here.

module voxelgriddata;

public struct VoxelGridData {
  public float4x4 worldToCascadeClipSpaceMatrices[8];   // world to cascade clip space
  public float4x4 worldToCascadeNormalizedMatrices[8];  // world to cascade, normalized [0,1] in xyz
  public float4x4 worldToCascadeGridMatrices[8];        // world to cascade grid space
  public float4x4 cascadeGridToWorldMatrices[8];        // cascade grid to world space
  public int4 cascadeAvoidAABBGridMin[8];               // grid space
  public int4 cascadeAvoidAABBGridMax[8];               // grid space
  public float4 cascadeAABBMin[8];                      // world space
  public float4 cascadeAABBMax[8];                      // world space
  public float4 cascadeAABBFadeStart[8];
  public float4 cascadeAABBFadeEnd[8];
  public float4 cascadeCenterHalfExtents[8];            // xyz = center (world), w = half-extent of a voxel
  public float4 worldToCascadeScales[2];               // one world-to-cascade scale per cascade
  public float4 cascadeToWorldScales[2];               // one cascade-to-world scale per cascade
  public float4 cascadeCellSizes[2];                   // voxel size in world space
  public float4 oneOverGridSizes[2];                   // 1.0 / gridSize per cascade
  public uint4 gridSizes[2];                           // voxel count per cascade dimension
  public uint4 dataOffsets[2];                         // cascade offsets
  public uint countCascades;                           // up to 4 cascades
  public uint hardwareConservativeRasterization;       // 0 = false, 1 = true
  public uint maxGlobalFragmentCount;                  // max fragments per voxel globally
  public uint maxLocalFragmentCount;                   // max fragments per voxel locally
  public float emissiveGIScale;                        // GI emissive master scale
  public float emissiveGIMax;                          // GI emissive master cap
}
