  mat4 worldToCascadeClipSpaceMatrices[4]; // world-to-cascade matrices (in clip space)
  mat4 worldToCascadeNormalizedMatrices[4]; // world-to-cascade matrices (normalized to [0, 1] in x, y and z)
  vec4 cascadeAABBMin[4]; // in world-space
  vec4 cascadeAABBMax[4]; // in world-space
  vec4 cascadeAABBFadeStart[4];
  vec4 cascadeAABBFadeEnd[4];
  vec4 cascadeCenterHalfExtents[4]; // xyz = center in world-space, w = half-extent of a voxel 
  vec4 worldToCascadeScales; // a world-to-cascade-scale component per cascade
  vec4 cascadeToWorldScales; // a cascade-to-world-scale component per cascade
  vec4 cascadeCellSizes; // size of a voxel in world-space
  uint gridSize; // number of voxels in a cascade in a single dimension
  uint countCascades; // maximum 4 cascades
  uint hardwareConservativeRasterization; // 0 = false, 1 = true
  uint maxGlobalFragmentCount; // maximum number of fragments per voxel globally
  uint maxLocalFragmentCount; // maximum number of fragments per voxel locally