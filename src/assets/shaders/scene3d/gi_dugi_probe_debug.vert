#version 460 core

// DUGI probe debug visualization — vertex shader.
//
// Fully procedural octahedral sphere (no vertex/index buffer): GI_DUGI_PROBE_DEBUG_GRID x GI_DUGI_PROBE_DEBUG_GRID quad grid,
// each grid vertex -> octahedral UV -> octDecode -> unit sphere direction. Instanced per probe over ALL cascades, unculled:
//   gl_InstanceIndex -> (cascadeIndex, localProbe) -> probe grid coord -> probe world position (via dugiData helpers).
// Vertex world = probeWorld + dir * radius, radius = 0.125 * min cascade cell size. Outputs the outward direction + the probe
// coord/cascade so the fragment shader colours the sphere with that probe's directional irradiance (dugiEvaluateIrradiance).

#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_multiview : enable
#extension GL_EXT_buffer_reference : enable
#extension GL_EXT_buffer_reference2 : enable
#extension GL_EXT_scalar_block_layout : enable
#extension GL_EXT_shader_explicit_arithmetic_types_int64 : enable

/* clang-format off */

#define GI_DUGI_PROBE_DEBUG_GRID 16
#define GI_DUGI_PROBE_DEBUG_RADIUS_FACTOR 0.125

#include "octahedral.glsl"

// dugiData (cascade AABBMin / CellSizes / ProbeScroll) + the probe<->grid<->world helpers. No GLOBAL_ILLUMINATION_DUGI_SAMPLE
// here -> only the data block + helpers, no image samplers (those are fragment-side).
#define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET 2
#define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_BINDING 0
#include "global_illumination_dugi.glsl"

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256];
} uView;

layout(push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint flags;                                            // GI_DUGI_FLAG_* bitmask; bit0 = inactive-probe early-out (cull inactive probes) — clear shows all probes
} pushConstants;

layout(location = 0) out vec3 outDirection;
layout(location = 1) flat out ivec3 outProbeCoord;
layout(location = 2) flat out int outCascadeIndex;
layout(location = 3) flat out float outActive; // relocation state (1 = active, 0 = inactive/inside geometry); 1 when relocation off

void main(){

  // --- Procedural octahedral-sphere vertex (2 triangles per grid quad) ---
  // Bitwise quad scheme: a quad is two triangles
  //
  //   0,0 v0--v1 1,0
  //       |\   |
  //       | \t0|
  //       |t1\ |
  //       |   \|
  //   0,1 v3--v2 1,1
  //
  // 0xe24 = 3,2,0,2,1,0 packs the per-vertex quad-corner index (output order 0,1,2, 0,2,3),
  // 0xb4  = 0b10110100  packs the corner -> (x,y) UV bits for corners 0..3.
  const uint countQuadPointsInOneDirection = uint(GI_DUGI_PROBE_DEBUG_GRID);
  uint vertexIndex = uint(gl_VertexIndex);
  uint quadIndex = vertexIndex / 6u;
  uint quadVertexIndex = (0xe24u >> ((vertexIndex - (quadIndex * 6u)) << 1u)) & 3u;
  uint quadVertexUVIndex = (0xb4u >> (quadVertexIndex << 1u)) & 3u;
  uvec2 quadVertexUV = uvec2(quadVertexUVIndex & 1u, quadVertexUVIndex >> 1u);

  uvec2 quadXY;
  quadXY.y = quadIndex / countQuadPointsInOneDirection;
  quadXY.x = quadIndex - (quadXY.y * countQuadPointsInOneDirection);

  vec3 direction = normalize(octUnsignedDecode(vec2(quadXY + quadVertexUV) / vec2(countQuadPointsInOneDirection)));

  // --- Per-probe placement: gl_InstanceIndex spans all cascades ---
  int instanceIndex = gl_InstanceIndex;
  int cascadeIndex = instanceIndex / GI_DUGI_PROBES_PER_CASCADE;
  int localProbe = instanceIndex - (cascadeIndex * GI_DUGI_PROBES_PER_CASCADE);
  ivec3 probeCoord = dugiProbeCoordFromIndex(localProbe);

  vec3 probeWorld = dugiProbeGridToWorld(probeCoord, cascadeIndex);
  vec3 cellSize = dugiData.dugiCascadeCellSizes[cascadeIndex].xyz;
  float radius = GI_DUGI_PROBE_DEBUG_RADIUS_FACTOR * min(cellSize.x, min(cellSize.y, cellSize.z));

  float probeActive = 1.0;
#if GI_DUGI_PROBE_RELOCATION
  // Place each sphere at the probe's ACTUAL relocated position (gi_dugi_relocation.comp pushes probes out of geometry, up to
  // GI_DUGI_PROBE_MAX_OFFSET*cellSize). The per-probe data is indexed by the PHYSICAL (toroidal) slot, exactly like the shading
  // gather. w = state (1 active, 0 inactive/inside-geometry -> dimmed by the fragment shader). probeData is null when off, so guard.
  ivec3 physProbeCoord = dugiProbePhysicalCoord(probeCoord, cascadeIndex);
  // Index the probe-data BDA sub-buffer directly (dugiData is readonly here, so we can't pass the buffer reference to the
  // non-readonly dugiLoadProbeDataBuffer helper) — same access pattern as dugiLoadIrradianceSH in the sampling include.
  vec4 probeData = dugiData.probeData.data[dugiProbeDataIndex(physProbeCoord, cascadeIndex)];
  probeWorld += probeData.xyz;
  probeActive = probeData.w;
#endif

  vec3 worldPosition = probeWorld + (direction * radius);

  outDirection = direction;
  outProbeCoord = probeCoord;
  outCascadeIndex = cascadeIndex;
  outActive = probeActive;

  // The inactive cull is tied to the inactive-probe early-out: when that optimization is disabled (flag clear), every probe
  // is updated, so show all of them (A/B) — keep the inactive ones, the fragment shader still dims them by outActive.
  bool earlyOut = (pushConstants.flags & GI_DUGI_FLAG_INACTIVE_PROBE_EARLY_OUT) != 0u;

  if(earlyOut && (probeActive < 0.5)){

    // Inactive probe

    // Cull the sphere if the probe is inactive (inside geometry) — the fragment shader will dim it, but we don't want to
    // waste rasterization on it. The sphere is still generated for all probes so the mesh can be instanced over all probes
    // without a separate culling pass.

    gl_Position = vec4(0.0, 0.0, 0.0, 0.0);

  }else{

    // Active probe (or early-out disabled): transform the vertex to clip space.
    //
    uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
    gl_Position = uView.views[viewIndex].projectionMatrix * (uView.views[viewIndex].viewMatrix * vec4(worldPosition, 1.0));

  }

}
