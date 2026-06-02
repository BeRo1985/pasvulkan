#ifndef GLOBAL_ILLUMINATION_SURFEL_GLSL
#define GLOBAL_ILLUMINATION_SURFEL_GLSL

// =====================================================================================================================
//  Surfel-based global illumination — shared data structures, world-space hash grid addressing and SH irradiance
//  sampling, in the spirit of EA SEED's "Global Illumination Based on Surfels" (Halen, 2021).
//
//  A surfel is an oriented surface element (position + normal + radius) that persistently caches indirect radiance as
//  an L1 RGB spherical-harmonics probe. Surfels live in a fixed GPU pool and are indexed by a world-space spatial hash
//  grid so the spawn / coverage / trace / shading passes can find the surfels near any world position in O(cell). Each
//  frame a few rays per surfel are traced against the TLAS (via gi_rt_gather.glsl) and the result is integrated into the
//  surfel's SH as a running average, giving temporally converged, multi-bounce-capable indirect lighting.
//
//  This header is shared by:
//    - the surfel compute passes  (gi_surfel_*.comp)        — they declare the buffers read-write and #include this for
//                                                              the record layout + hashing,
//    - the shading consumers      (mesh.frag, planet_*.frag) — they #define GLOBAL_ILLUMINATION_SURFEL_SAMPLE and a
//                                                              descriptor set index, and this header then declares the
//                                                              read-only buffers + giSurfelSampleIrradiance().
//
//  The GPU buffer layouts below MUST stay in sync with the Pascal side (TpvScene3DRendererInstance surfel resources).
// =====================================================================================================================

#include "sphericalharmonics.glsl"

// --- Compile-time capacity / hashing configuration (must match the Pascal-side allocation) --------------------------

#ifndef GI_SURFEL_MAX_COUNT
  #define GI_SURFEL_MAX_COUNT 65536           // surfel pool capacity
#endif

#ifndef GI_SURFEL_HASH_CELL_COUNT
  #define GI_SURFEL_HASH_CELL_COUNT 131072    // spatial-hash bucket count; MUST be a power of two (>= ~2x the pool)
#endif

#ifndef GI_SURFEL_MAX_PER_CELL
  #define GI_SURFEL_MAX_PER_CELL 32           // surfel index slots stored per hash cell (overflow is dropped)
#endif

#define GI_SURFEL_HASH_CELL_MASK (uint(GI_SURFEL_HASH_CELL_COUNT) - 1u)

// Surfel "alive" flag bit.
#define GI_SURFEL_FLAG_ALIVE 1u

// --- Surfel record (std430, 64 bytes) -------------------------------------------------------------------------------
// Layout mirrored by the Pascal record TpvScene3DRendererInstanceSurfel.
struct Surfel {
  vec4 positionRadius;   // xyz = world position (meters), w = radius (meters)
  vec4 normalCount;      // xyz = world-space surface normal, w = accumulated sample count (as float)
  uvec2 sh[3];           // packed SHC3CoefficientsL1 — L1 RGB radiance SH (half-float packed; 24 bytes)
  uint lastFrame;        // frame index this surfel was last touched (for recycling stale surfels)
  uint flags;            // GI_SURFEL_FLAG_* bits
};

// --- Surfel uniform parameters (std140 UBO) -------------------------------------------------------------------------
// Layout mirrored by the Pascal record TpvScene3DRendererInstanceSurfelUniformBufferData.
struct SurfelUniforms {
  vec4 cameraPositionCellSize;  // xyz = camera world position, w = base hash cell size (meters)
  uvec4 countsFrame;            // x = maxSurfels, y = hashCellCount, z = maxPerCell, w = frameIndex
  vec4 params;                  // x = surfel radius, y = temporal hysteresis (0..1), z = recycle frame age, w = spawn coverage threshold
};

// --- World-space hash grid ------------------------------------------------------------------------------------------

// The integer cell coordinate a world position falls into. Fixed cell size for now (a future improvement is camera
// distance dependent clipmap-style cell scaling à la SEED; that only changes this function + the hash level term).
ivec3 giSurfelCellCoord(const in vec3 worldPosition, const in float cellSize){
  return ivec3(floor(worldPosition / max(cellSize, 1e-4)));
}

// Spatial hash of a cell coordinate into [0, GI_SURFEL_HASH_CELL_COUNT). Classic three-prime xor hash.
uint giSurfelCellHash(const in ivec3 cellCoord){
  uint h = (uint(cellCoord.x) * 73856093u) ^ (uint(cellCoord.y) * 19349663u) ^ (uint(cellCoord.z) * 83492791u);
  return h & GI_SURFEL_HASH_CELL_MASK;
}

// =====================================================================================================================
//  Shading-side sampling path (mesh.frag / planet_*.frag).
//
//  The includer must, before the #include:
//    - #define GLOBAL_ILLUMINATION_SURFEL_SAMPLE
//    - #define GI_SURFEL_DESCRIPTOR_SET <set index> (mesh.frag = 2, planets = 4)
//  This then declares the read-only surfel buffers and giSurfelSampleIrradiance().
// =====================================================================================================================

#ifdef GLOBAL_ILLUMINATION_SURFEL_SAMPLE

#ifndef GI_SURFEL_DESCRIPTOR_SET
  #error "global_illumination_surfel.glsl: #define GI_SURFEL_DESCRIPTOR_SET before including with GLOBAL_ILLUMINATION_SURFEL_SAMPLE."
#endif

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 0, std140) uniform SurfelUniformBuffer {
  SurfelUniforms surfelData;
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 1, std430) readonly buffer SurfelBuffer {
  Surfel surfels[];
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 2, std430) readonly buffer SurfelGridCellBuffer {
  uint surfelGridCells[]; // GI_SURFEL_HASH_CELL_COUNT * GI_SURFEL_MAX_PER_CELL surfel indices
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 3, std430) readonly buffer SurfelGridCellCountBuffer {
  uint surfelGridCellCounts[]; // GI_SURFEL_HASH_CELL_COUNT live-surfel counts per cell
};

// Gather the surfels in the hash cell containing worldPosition and blend their cosine-convolved SH irradiance, weighted
// by spatial proximity (smooth falloff over the surfel radius) and normal agreement (back-facing surfels rejected). The
// cell size is chosen (Pascal side) >= the surfel radius so a single-cell gather already covers a point's neighbourhood;
// returns irradiance E (multiply by albedo/PI like getIBLDiffuse's result for the diffuse contribution).
vec3 giSurfelSampleIrradiance(const in vec3 worldPosition, const in vec3 normal){
  float cellSize = surfelData.cameraPositionCellSize.w;
  ivec3 cellCoord = giSurfelCellCoord(worldPosition, cellSize);
  uint cell = giSurfelCellHash(cellCoord);

  uint count = min(surfelGridCellCounts[cell], uint(GI_SURFEL_MAX_PER_CELL));
  uint base = cell * uint(GI_SURFEL_MAX_PER_CELL);

  SHC3CoefficientsL1 accumSH = SHC3CoefficientsL1Zero();
  float accumWeight = 0.0;

  for(uint i = 0u; i < count; i++){
    uint surfelIndex = surfelGridCells[base + i];
    if(surfelIndex >= uint(GI_SURFEL_MAX_COUNT)){
      continue;
    }
    Surfel surfel = surfels[surfelIndex];
    if((surfel.flags & GI_SURFEL_FLAG_ALIVE) == 0u){
      continue;
    }

    vec3 toSurfel = surfel.positionRadius.xyz - worldPosition;
    float dist = length(toSurfel);
    float radius = max(surfel.positionRadius.w, 1e-3);
    if(dist >= radius){
      continue; // outside this surfel's influence
    }

    // Normal agreement: reject surfels facing away from the shaded surface (avoids leaking through thin geometry).
    float normalWeight = clamp(dot(surfel.normalCount.xyz, normal), 0.0, 1.0);
    if(normalWeight <= 0.0){
      continue;
    }

    // Smooth spatial falloff towards the surfel radius.
    float spatialWeight = 1.0 - (dist / radius);
    spatialWeight *= spatialWeight;

    float weight = spatialWeight * normalWeight;
    if(weight <= 0.0){
      continue;
    }

    SHC3CoefficientsL1 sh = SHC3CoefficientsL1Unpack(PackedSHC3CoefficientsL1(uvec2[3](surfel.sh[0], surfel.sh[1], surfel.sh[2])));
    accumSH = SHC3CoefficientsL1Add(accumSH, SHC3CoefficientsL1Mul(sh, weight));
    accumWeight += weight;
  }

  if(accumWeight <= 0.0){
    return vec3(0.0);
  }

  accumSH = SHC3CoefficientsL1Div(accumSH, accumWeight);
  return max(vec3(0.0), SHC3CoefficientsL1CalculateIrradiance(accumSH, normal)); // cosine-convolved evaluation -> irradiance E
}

#endif // GLOBAL_ILLUMINATION_SURFEL_SAMPLE

// =====================================================================================================================
//  Compute-side read-write path (gi_surfel_*.comp).
//
//  The includer must, before the #include:
//    - #define GLOBAL_ILLUMINATION_SURFEL_COMPUTE
//    - #define GI_SURFEL_DESCRIPTOR_SET <set index>   (the surfel passes use set 0)
//  This declares the buffers read-write plus the allocator / grid-insert helpers shared by the passes.
//
//  Free-list allocation is race-free via parity double-buffering: each frame the RECYCLE pass rebuilds the free list
//  into bank (frameIndex & 1), and the SPAWN pass of the NEXT frame consumes the bank the previous recycle filled
//  (bank ^ 1). The clear pass zeroes the spawn ticket counter and the write bank's count at frame start; the read bank
//  is never written in the same frame the spawn pass reads it, so no read/write hazard exists.
// =====================================================================================================================

#ifdef GLOBAL_ILLUMINATION_SURFEL_COMPUTE

#ifndef GI_SURFEL_DESCRIPTOR_SET
  #error "global_illumination_surfel.glsl: #define GI_SURFEL_DESCRIPTOR_SET before including with GLOBAL_ILLUMINATION_SURFEL_COMPUTE."
#endif

#define GI_SURFEL_INVALID_INDEX 0xffffffffu

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 0, std140) uniform SurfelUniformBuffer {
  SurfelUniforms surfelData;
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 1, std430) buffer SurfelBuffer {
  Surfel surfels[];
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 2, std430) buffer SurfelGridCellBuffer {
  uint surfelGridCells[]; // GI_SURFEL_HASH_CELL_COUNT * GI_SURFEL_MAX_PER_CELL
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 3, std430) buffer SurfelGridCellCountBuffer {
  uint surfelGridCellCounts[]; // GI_SURFEL_HASH_CELL_COUNT
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 4, std430) buffer SurfelStatsBuffer {
  uint surfelSpawnCursor;   // atomic spawn ticket (zeroed each frame by the clear pass)
  uint surfelAliveCount;    // live surfel count (debug / coverage budgeting)
  uint surfelFreeCount0;    // free-list length of bank 0
  uint surfelFreeCount1;    // free-list length of bank 1
};

layout(set = GI_SURFEL_DESCRIPTOR_SET, binding = 5, std430) buffer SurfelFreeListBuffer {
  uint surfelFreeList[]; // 2 * GI_SURFEL_MAX_COUNT (two parity banks)
};

uint giSurfelFrameIndex(){ return surfelData.countsFrame.w; }
uint giSurfelFreeBankWrite(){ return giSurfelFrameIndex() & 1u; }
uint giSurfelFreeBankRead(){ return giSurfelFreeBankWrite() ^ 1u; }

// Insert a surfel index into its world-space hash cell (atomic append, overflow dropped).
void giSurfelGridInsert(const in uint surfelIndex, const in vec3 worldPosition){
  uint cell = giSurfelCellHash(giSurfelCellCoord(worldPosition, surfelData.cameraPositionCellSize.w));
  uint slot = atomicAdd(surfelGridCellCounts[cell], 1u);
  if(slot < uint(GI_SURFEL_MAX_PER_CELL)){
    surfelGridCells[(cell * uint(GI_SURFEL_MAX_PER_CELL)) + slot] = surfelIndex;
  }
}

// Allocate a free surfel slot from the read bank (the list the previous frame's recycle filled). Returns
// GI_SURFEL_INVALID_INDEX when the pool is exhausted for this frame.
uint giSurfelAllocate(){
  uint bank = giSurfelFreeBankRead();
  uint available = (bank == 0u) ? surfelFreeCount0 : surfelFreeCount1;
  uint ticket = atomicAdd(surfelSpawnCursor, 1u);
  if(ticket >= available){
    return GI_SURFEL_INVALID_INDEX;
  }
  return surfelFreeList[(bank * uint(GI_SURFEL_MAX_COUNT)) + ticket];
}

// Push a free slot onto the write bank's free list (used by the recycle pass to rebuild the list for next frame).
void giSurfelFree(const in uint surfelIndex){
  uint bank = giSurfelFreeBankWrite();
  uint w;
  if(bank == 0u){
    w = atomicAdd(surfelFreeCount0, 1u);
  }else{
    w = atomicAdd(surfelFreeCount1, 1u);
  }
  if(w < uint(GI_SURFEL_MAX_COUNT)){
    surfelFreeList[(bank * uint(GI_SURFEL_MAX_COUNT)) + w] = surfelIndex;
  }
}

// Coverage estimate at a world position/normal: the summed proximity/normal weight of the surfels in the cell. The
// spawn pass spawns a new surfel when this is below surfelData.params.w. Mirrors the shading gather's weighting.
float giSurfelCoverage(const in vec3 worldPosition, const in vec3 normal){
  uint cell = giSurfelCellHash(giSurfelCellCoord(worldPosition, surfelData.cameraPositionCellSize.w));
  uint count = min(surfelGridCellCounts[cell], uint(GI_SURFEL_MAX_PER_CELL));
  uint base = cell * uint(GI_SURFEL_MAX_PER_CELL);
  float coverage = 0.0;
  for(uint i = 0u; i < count; i++){
    uint surfelIndex = surfelGridCells[base + i];
    if(surfelIndex >= uint(GI_SURFEL_MAX_COUNT)){
      continue;
    }
    Surfel surfel = surfels[surfelIndex];
    if((surfel.flags & GI_SURFEL_FLAG_ALIVE) == 0u){
      continue;
    }
    vec3 toSurfel = surfel.positionRadius.xyz - worldPosition;
    float dist = length(toSurfel);
    float radius = max(surfel.positionRadius.w, 1e-3);
    if(dist >= radius){
      continue;
    }
    float normalWeight = clamp(dot(surfel.normalCount.xyz, normal), 0.0, 1.0);
    float spatialWeight = 1.0 - (dist / radius);
    coverage += (spatialWeight * spatialWeight) * normalWeight;
  }
  return coverage;
}

// Compute-side irradiance gather (same weighting as the shading-side giSurfelSampleIrradiance, but on the read-write
// buffers) — used by the trace pass for the previous-frame multi-bounce feedback term.
vec3 giSurfelGatherIrradiance(const in vec3 worldPosition, const in vec3 normal){
  uint cell = giSurfelCellHash(giSurfelCellCoord(worldPosition, surfelData.cameraPositionCellSize.w));
  uint count = min(surfelGridCellCounts[cell], uint(GI_SURFEL_MAX_PER_CELL));
  uint base = cell * uint(GI_SURFEL_MAX_PER_CELL);
  SHC3CoefficientsL1 accumSH = SHC3CoefficientsL1Zero();
  float accumWeight = 0.0;
  for(uint i = 0u; i < count; i++){
    uint surfelIndex = surfelGridCells[base + i];
    if(surfelIndex >= uint(GI_SURFEL_MAX_COUNT)){
      continue;
    }
    Surfel surfel = surfels[surfelIndex];
    if((surfel.flags & GI_SURFEL_FLAG_ALIVE) == 0u){
      continue;
    }
    vec3 toSurfel = surfel.positionRadius.xyz - worldPosition;
    float dist = length(toSurfel);
    float radius = max(surfel.positionRadius.w, 1e-3);
    if(dist >= radius){
      continue;
    }
    float normalWeight = clamp(dot(surfel.normalCount.xyz, normal), 0.0, 1.0);
    if(normalWeight <= 0.0){
      continue;
    }
    float spatialWeight = 1.0 - (dist / radius);
    spatialWeight *= spatialWeight;
    float weight = spatialWeight * normalWeight;
    if(weight <= 0.0){
      continue;
    }
    SHC3CoefficientsL1 sh = SHC3CoefficientsL1Unpack(PackedSHC3CoefficientsL1(uvec2[3](surfel.sh[0], surfel.sh[1], surfel.sh[2])));
    accumSH = SHC3CoefficientsL1Add(accumSH, SHC3CoefficientsL1Mul(sh, weight));
    accumWeight += weight;
  }
  if(accumWeight <= 0.0){
    return vec3(0.0);
  }
  accumSH = SHC3CoefficientsL1Div(accumSH, accumWeight);
  return max(vec3(0.0), SHC3CoefficientsL1CalculateIrradiance(accumSH, normal));
}

#endif // GLOBAL_ILLUMINATION_SURFEL_COMPUTE

#endif // GLOBAL_ILLUMINATION_SURFEL_GLSL
