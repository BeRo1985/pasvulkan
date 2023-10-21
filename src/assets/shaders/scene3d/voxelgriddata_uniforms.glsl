  vec4 clipMaps[4]; // xyz = center in world-space, w = half-extent of a voxel 
  vec4 clipMapAABBMin[4]; // in world-space
  vec4 clipMapAABBMax[4]; // in world-space
  vec4 cellSizes; // size of a voxel in world-space
  uint gridSize; // number of voxels in a clipmap in a single dimension
  uint countClipMaps; // maximum 4 clipmaps
  uint hardwareConservativeRasterization; // 0 = false, 1 = true
  uint maxGlobalFragmentCount; // maximum number of fragments per voxel globally
  uint maxLocalFragmentCount; // maximum number of fragments per voxel locally