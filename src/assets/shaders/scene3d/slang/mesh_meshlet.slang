// Task + mesh shader entry points for the meshlet geometry path (Slang port of mesh.task /
// mesh.mesh). Each render mode (View / Voxelization / RSM) is a separate pair of entry points,
// because the mesh-stage output layout must be a compile-time constant; the per-mode payload type
// comes from its IMeshRenderMode implementation. The shared framework here is the indirect-command
// pull, per-view setup, world-sphere build and Hi-Z occlusion test; the fragment-facing varying
// (VertexInputVarying) and the standard vertex transform live in mesh_globals.
//
// Matrix convention as in math.slang: with -matrix-layout-row-major the GLSL row/column order ports
// verbatim (m[i] = column i, m[c][r] = the GLSL element) and the GLSL '*' operator (via -allow-glsl)
// is the transform / product, so the math mirrors mesh.task / mesh.mesh directly.

import gputypes;
import mesh_bindings;
import mesh_globals;
import mesh_meshlet_config;
import mesh_meshlet_rendermode_view;
import mesh_meshlet_rendermode_voxel;
import mesh_meshlet_rendermode_rsm;
import utils;
import math;
import frustum;
import debug;
import voxelgriddata;

// Per-vertex varyings for the voxelization mesh entry, locations matching the voxelization path of
// mesh.mesh so the voxelization fragment shader is unchanged. Vertices are not shared (3 per
// triangle), gl_Position carries the dominant-axis projection, and gl_Layer (VoxelPrimitive, per
// primitive) routes the cascade.
public struct VoxelVarying {
  float4 position : SV_Position;
  [[vk::location(0)]] float3 worldSpacePosition : LOC0;
  [[vk::location(1)]] float3 viewSpacePosition : LOC1;
  [[vk::location(2)]] float3 cameraRelativePosition : LOC2;
  [[vk::location(3)]] float4 tangentSign : LOC3;
  [[vk::location(4)]] float3 normal : LOC4;
  [[vk::location(5)]] float2 texCoord0 : LOC5;
  [[vk::location(6)]] float2 texCoord1 : LOC6;
  [[vk::location(7)]] float4 color0 : LOC7;
  [[vk::location(8)]] float3 modelScale : LOC8;
  [[vk::location(9)]] nointerpolation uint materialID : LOC9;
  [[vk::location(10)]] nointerpolation uint instanceDataIndex : LOC10;
  [[vk::location(11)]] nointerpolation float3 aabbMin : LOC11;
  [[vk::location(12)]] nointerpolation float3 aabbMax : LOC12;
  [[vk::location(13)]] nointerpolation uint cascadeIndex : LOC13;
  [[vk::location(14)]] float3 voxelPosition : LOC14;
  [[vk::location(15)]] nointerpolation float3 vertex0 : LOC15;
  [[vk::location(16)]] nointerpolation float3 vertex1 : LOC16;
  [[vk::location(17)]] nointerpolation float3 vertex2 : LOC17;
}

// Per-primitive output that routes each triangle to an array layer (replaces
// gl_MeshPrimitivesEXT[i].gl_Layer), shared by the voxelization (cascade) and RSM / layer-routing
// (view/cascade) mesh entries.
public struct MeshletPrimitive {
  uint layer : SV_RenderTargetArrayIndex;
}

// Transform a bounding sphere by a model matrix: the radius grows by the largest column length
// (the maximum axis scale). Mirrors sphereTransform in mesh.task / mesh_cull.comp.
float4 sphereTransform(float4 sphere, float4x4 transform) {
  float maxAxisLengthSq = max(max(dot(transform[0].xyz, transform[0].xyz), dot(transform[1].xyz, transform[1].xyz)), dot(transform[2].xyz, transform[2].xyz));
  return float4(mulmv(transform, float4(sphere.xyz, 1.0)).xyz,
                max(sphere.w * max(1.0, sqrt(maxAxisLengthSq)), 0.0));
}

// World-space bounding sphere for a meshlet, shared by all task entries: prefer the per-instance
// buffer (mesh_update_meshlet_boundingspheres.comp), fall back to the descriptor sphere, transformed
// into world space by the model matrix. hasSphere is false when neither source has a valid
// (positive-radius) sphere.
float4 computeWorldSphere(uint meshObjectID, uint meshletBase, uint meshletLocalIndex, uint meshletGlobalIndex,
                          float4x4 modelMatrix, GlobalBDAPointers bda, out bool hasSphere) {
  float4 worldSphere = float4(0.0);
  hasSphere = false;

  uint meshletBoundingSphereBase = drawInfoItems[meshObjectID].meshletBoundingSphereBase;
  if ((meshletBoundingSphereBase != 0xffffffffu) && (uint64_t(bda.meshletBoundingSpheres) != 0u)) {
    uint sphereIndex = meshletBoundingSphereBase +
                       (meshletBase - drawInfoItems[meshObjectID].meshletDescriptorBase) +
                       meshletLocalIndex;
    worldSphere = bda.meshletBoundingSpheres[sphereIndex];
    hasSphere = worldSphere.w > 0.0;
    if (hasSphere) {
      worldSphere = sphereTransform(worldSphere, modelMatrix);
    }
  }
  if (!hasSphere) {
    float4 descriptorSphere = bda.meshletDescriptors[meshletGlobalIndex].boundingSphere;
    if (descriptorSphere.w > 0.0) {
      worldSphere = sphereTransform(descriptorSphere, modelMatrix);
      hasSphere = true;
    }
  }
  return worldSphere;
}

// Hi-Z occlusion test for one meshlet in one view. Returns false when the meshlet is occluded or
// too small in this view, true otherwise (including when the sphere crosses the near plane, where
// it cannot be conservatively tested). Mirrors the Pass-1 block in mesh.task. The depth texture
// index is a (dynamically uniform) push constant, so it needs no NonUniformResourceIndex.
bool meshletPassesHiZ(float4 viewSpaceSphere, float zNear, float4x4 projectionMatrix,
                      float4x4 inverseProjectionMatrix, uint depthIndex, uint layer,
                      float areaTooSmallThreshold) {
  float4 aabb;
  if (!projectSphere(viewSpaceSphere.xyz, viewSpaceSphere.w, zNear, projectionMatrix, aabb, true)) {
    return true;
  }

  uint width, height, elements, levels;
  uTextureDepths[depthIndex].GetDimensions(0, width, height, elements, levels);
  float2 viewPortSize = float2(float(width), float(height));
  float2 size = (aabb.zw - aabb.xy) * viewPortSize;

  if ((areaTooSmallThreshold > 0.0) && (max(size.x, size.y) < areaTooSmallThreshold)) {
    return false;
  }

  float level = clamp(floor(log2(max(size.x, size.y))), 0.0, float(int(levels) - 1));
  float sampledDepth = uTextureDepths[depthIndex].SampleLevel(float3(lerp(aabb.xy, aabb.zw, 0.5), float(layer)), level).x;
  float2 depthZ = mulmv(inverseProjectionMatrix, float4((aabb.xy + aabb.zw) - float2(1.0), sampledDepth, 1.0)).zw;
  float depth = -(depthZ.x / depthZ.y);

  return !(((-viewSpaceSphere.z) - viewSpaceSphere.w) > depth);
}

// Standard meshlet emission (vertex transform + triangle indices), shared by the View task / no-task
// and RSM mesh entries. The caller resolves meshletGlobalIndex / meshObjectID (from the task payload
// or the indirect draw command) and the view index (multiview SV_ViewID or the layer-routing index);
// the per-vertex transform itself is the shared transformVertex from mesh_globals.
void emitStandardMeshlet(uint localInvocationIndex, uint viewIndex, uint meshletGlobalIndex, uint meshObjectID, GlobalBDAPointers bda,
                         out vertices VertexInputVarying<velocity> vertices[MAX_MESHLET_VERTICES],
                         out indices uint3 triangles[MAX_MESHLET_PRIMITIVES]) {
  MeshletDescriptor descriptor = bda.meshletDescriptors[meshletGlobalIndex];
  uint vertexCount = min(descriptor.vertexCount, MAX_MESHLET_VERTICES);
  uint primitiveCount = min(descriptor.primitiveCount, MAX_MESHLET_PRIMITIVES);

  SetMeshOutputCounts(vertexCount, primitiveCount);

  VertexTransform t = makeVertexTransform(meshObjectID, viewIndex, bda);

  for (uint i = localInvocationIndex; i < vertexCount; i += MESH_GROUP_SIZE) {
    uint globalVertexIndex = bda.meshletVertices[descriptor.vertexOffset + i];
    vertices[i] = transformVertex<velocity>(globalVertexIndex, bda, t, meshletGlobalIndex);
  }

  for (uint i = localInvocationIndex; i < primitiveCount; i += MESH_GROUP_SIZE) {
    // Unpack 3x uint8 triangle indices from each uint32.
    triangles[i] = (uint3(bda.meshletPrimitives[descriptor.primitiveOffset + i]) >> uint3(0u, 8u, 16u)) & uint3(0xffu);
  }
}

// --- View render mode ---

// Per-view data precomputed by the first task threads (same pattern as mesh.task / mesh_cull.comp).
groupshared float4x4 gViewViewMatrices[8];
groupshared float4x4 gViewProjectionMatrices[8];
groupshared Frustum gViewFrustums[8];
groupshared float gViewZNears[8];
groupshared float4x4 gViewModelMatrix;
groupshared ViewTaskPayload gViewPayload;

[shader("amplification")]
[numthreads(TASK_GROUP_SIZE, 1, 1)]
void viewTaskMain(uint localInvocationIndex : SV_GroupIndex,
                  uint groupID : SV_GroupID,
                  uint drawIndex : SV_DrawIndex) {
  uint countViews = min(pushConstants.countViews, 8u);

  GlobalBDAPointers bda = globalBDAPointersBuffer[0];

  // Indirect command for this draw (gl_DrawID from vkCmdDrawMeshTasksIndirectCountEXT).
  MeshDrawCommand* meshDrawCommands = (MeshDrawCommand*)((uint64_t(pushConstants.meshDrawCommandsBDAHigh) << 32) | uint64_t(pushConstants.meshDrawCommandsBDALow));
  MeshDrawCommand cmd = meshDrawCommands[drawIndex];
  uint meshletBase = cmd.cmd0.w;
  uint meshletCount = cmd.cmd1.x;
  uint meshObjectID = cmd.cmd1.y;

  if (localInvocationIndex == 0u) {
    gViewModelMatrix = bda.matrixPairs[drawInfoItems[meshObjectID].matrixID].modelMatrix;
    gViewPayload.meshletDescriptorBase = meshletBase;
    gViewPayload.meshObjectID = meshObjectID;
    gViewPayload.count = 0u;
  }

  if (localInvocationIndex < countViews) {
    uint viewIndex = pushConstants.viewBaseIndex + localInvocationIndex;
    View view = uView.views[viewIndex];
    gViewViewMatrices[localInvocationIndex] = view.viewMatrix;
    gViewProjectionMatrices[localInvocationIndex] = view.projectionMatrix;
    gViewFrustums[localInvocationIndex] = frustumSetup(view.projectionMatrix);
    if (cullPass == 1) {
      gViewZNears[localInvocationIndex] = abs(view.inverseProjectionMatrix[3][2] / view.inverseProjectionMatrix[2][3]);
    }
  }

  GroupMemoryBarrierWithGroupSync();

  uint meshletLocalIndex = (groupID * TASK_GROUP_SIZE) + localInvocationIndex;

  if (meshletLocalIndex < meshletCount) {
    uint meshletGlobalIndex = meshletBase + meshletLocalIndex;

    bool hasSphere;
    float4 worldSphere = computeWorldSphere(meshObjectID, meshletBase, meshletLocalIndex, meshletGlobalIndex, gViewModelMatrix, bda, hasSphere);

    bool distanceCulled = false;
    if (hasSphere && (pushConstants.maximumDistance > 0.0)) {
      float3 cameraPosition = uView.views[0].inverseViewMatrix[3].xyz;
      if ((distance(cameraPosition, worldSphere.xyz) - worldSphere.w) > pushConstants.maximumDistance) {
        distanceCulled = true;
      }
    }

    bool visible;
    if (selectionMask) {
      // Selection x-ray: emit every meshlet, no frustum / Hi-Z / distance culling.
      visible = true;
    } else {
      visible = false;
      if (((pushConstants.drawFlags & FLAG_MESHLET_CULLING_ENABLED) != 0u) && hasSphere && !distanceCulled) {
        for (uint viewIndex = 0u; viewIndex < countViews; viewIndex++) {
          float4 viewSpaceSphere = float4(mulmv(gViewViewMatrices[viewIndex], float4(worldSphere.xyz, 1.0)).xyz, worldSphere.w);
          if (!frustumCullingSphereTest(gViewFrustums[viewIndex], viewSpaceSphere)) {
            continue;
          }
          if (cullPass == 1) {
            float4x4 inverseProjectionMatrix = uView.views[pushConstants.viewBaseIndex + viewIndex].inverseProjectionMatrix;
            if (!meshletPassesHiZ(viewSpaceSphere, gViewZNears[viewIndex], gViewProjectionMatrices[viewIndex],
                                  inverseProjectionMatrix, pushConstants.textureDepthIndex, viewIndex,
                                  pushConstants.areaTooSmallThreshold)) {
              continue;
            }
          }
          visible = true;
          break;
        }
      } else if (!distanceCulled) {
        visible = true;
      }
    }

    if (visible) {
      uint slot;
      InterlockedAdd(gViewPayload.count, 1u, slot);
      if (slot < TASK_PAYLOAD_CAPACITY_VIEW) {
        gViewPayload.meshletLocalIndices[slot] = meshletLocalIndex;
      }
    }
  }

  GroupMemoryBarrierWithGroupSync();

  uint emitCount = min(gViewPayload.count, TASK_PAYLOAD_CAPACITY_VIEW);
  DispatchMesh(emitCount, 1u, 1u, gViewPayload);
}

[shader("mesh")]
[numthreads(MESH_GROUP_SIZE, 1, 1)]
[outputtopology("triangle")]
void viewMeshMain(uint localInvocationIndex : SV_GroupIndex,
                  uint groupID : SV_GroupID,
                  uint viewID : SV_ViewID,
                  in payload ViewTaskPayload payload,
                  out vertices VertexInputVarying<velocity> vertices[MAX_MESHLET_VERTICES],
                  out indices uint3 triangles[MAX_MESHLET_PRIMITIVES]) {
  uint meshletGlobalIndex = payload.meshletDescriptorBase + payload.meshletLocalIndices[groupID];
  GlobalBDAPointers bda = globalBDAPointersBuffer[0];
  emitStandardMeshlet(localInvocationIndex, pushConstants.viewBaseIndex + viewID, meshletGlobalIndex, payload.meshObjectID, bda, vertices, triangles);
}

// No task shader: mesh_cull.comp already expanded one indirect draw command per meshlet, so the
// meshlet and object come straight from the command (gl_DrawID) instead of a task payload. noTaskShader
// changes the entry signature (no payload input) so it is a separate entry, not a spec constant.
[shader("mesh")]
[numthreads(MESH_GROUP_SIZE, 1, 1)]
[outputtopology("triangle")]
void viewMeshNoTaskMain(uint localInvocationIndex : SV_GroupIndex,
                        uint viewID : SV_ViewID,
                        uint drawIndex : SV_DrawIndex,
                        out vertices VertexInputVarying<velocity> vertices[MAX_MESHLET_VERTICES],
                        out indices uint3 triangles[MAX_MESHLET_PRIMITIVES]) {
  GlobalBDAPointers bda = globalBDAPointersBuffer[0];
  MeshDrawCommand* meshDrawCommands = (MeshDrawCommand*)((uint64_t(pushConstants.meshDrawCommandsBDAHigh) << 32) | uint64_t(pushConstants.meshDrawCommandsBDALow));
  MeshDrawCommand cmd = meshDrawCommands[drawIndex];
  emitStandardMeshlet(localInvocationIndex, pushConstants.viewBaseIndex + viewID, cmd.cmd0.w, cmd.cmd1.y, bda, vertices, triangles);
}

// --- Voxelization render mode ---

groupshared float4x4 gVoxelModelMatrix;
groupshared VoxelTaskPayload gVoxelPayload;

[shader("amplification")]
[numthreads(TASK_GROUP_SIZE, 1, 1)]
void voxelTaskMain(uint localInvocationIndex : SV_GroupIndex,
                   uint groupID : SV_GroupID,
                   uint drawIndex : SV_DrawIndex) {
  GlobalBDAPointers bda = globalBDAPointersBuffer[0];

  MeshDrawCommand* meshDrawCommands = (MeshDrawCommand*)((uint64_t(pushConstants.meshDrawCommandsBDAHigh) << 32) | uint64_t(pushConstants.meshDrawCommandsBDALow));
  MeshDrawCommand cmd = meshDrawCommands[drawIndex];
  uint meshletBase = cmd.cmd0.w;
  uint meshletCount = cmd.cmd1.x;
  uint meshObjectID = cmd.cmd1.y;

  if (localInvocationIndex == 0u) {
    gVoxelModelMatrix = bda.matrixPairs[drawInfoItems[meshObjectID].matrixID].modelMatrix;
    gVoxelPayload.meshletDescriptorBase = meshletBase;
    gVoxelPayload.meshObjectID = meshObjectID;
    gVoxelPayload.count = 0u;
  }

  GroupMemoryBarrierWithGroupSync();

  uint meshletLocalIndex = (groupID * TASK_GROUP_SIZE) + localInvocationIndex;

  if (meshletLocalIndex < meshletCount) {
    uint meshletGlobalIndex = meshletBase + meshletLocalIndex;

    bool hasSphere;
    float4 worldSphere = computeWorldSphere(meshObjectID, meshletBase, meshletLocalIndex, meshletGlobalIndex, gVoxelModelMatrix, bda, hasSphere);

    bool distanceCulled = false;
    if (hasSphere && (pushConstants.maximumDistance > 0.0)) {
      float3 cameraPosition = uView.views[0].inverseViewMatrix[3].xyz;
      if ((distance(cameraPosition, worldSphere.xyz) - worldSphere.w) > pushConstants.maximumDistance) {
        distanceCulled = true;
      }
    }

    // A meshlet with more than MAX_VOXEL_MESHLET_PRIMITIVES triangles is split into numChunks chunks,
    // one (meshlet, cascade, chunk) entry emitted per visible chunk so every triangle is voxelized.
    uint meshletPrimitiveCount = min(bda.meshletDescriptors[meshletGlobalIndex].primitiveCount, MAX_MESHLET_PRIMITIVES);
    uint numChunks = (meshletPrimitiveCount + (MAX_VOXEL_MESHLET_PRIMITIVES - 1u)) / MAX_VOXEL_MESHLET_PRIMITIVES;
    uint countCascades = voxelGridData.countCascades;

    if (((pushConstants.drawFlags & FLAG_MESHLET_CULLING_ENABLED) != 0u) && hasSphere && !distanceCulled) {
      for (uint cascadeIndex = 0u; cascadeIndex < countCascades; cascadeIndex++) {
        // Sphere-vs-AABB test against the world-space cascade AABB.
        float3 closestPoint = clamp(worldSphere.xyz, voxelGridData.cascadeAABBMin[cascadeIndex].xyz, voxelGridData.cascadeAABBMax[cascadeIndex].xyz);
        float3 diff = closestPoint - worldSphere.xyz;
        if (dot(diff, diff) <= (worldSphere.w * worldSphere.w)) {
          for (uint chunkIndex = 0u; chunkIndex < numChunks; chunkIndex++) {
            uint slot;
            InterlockedAdd(gVoxelPayload.count, 1u, slot);
            if (slot < TASK_PAYLOAD_CAPACITY_VOXEL) {
              gVoxelPayload.meshletLocalIndices[slot] = meshletLocalIndex;
              gVoxelPayload.layerIndices[slot] = (chunkIndex << VOXEL_CASCADE_BITS) | cascadeIndex;
            }
          }
        }
      }
    } else if (!distanceCulled) {
      // Culling disabled or no valid sphere -> emit for all cascades x chunks (conservative).
      for (uint cascadeIndex = 0u; cascadeIndex < countCascades; cascadeIndex++) {
        for (uint chunkIndex = 0u; chunkIndex < numChunks; chunkIndex++) {
          uint slot;
          InterlockedAdd(gVoxelPayload.count, 1u, slot);
          if (slot < TASK_PAYLOAD_CAPACITY_VOXEL) {
            gVoxelPayload.meshletLocalIndices[slot] = meshletLocalIndex;
            gVoxelPayload.layerIndices[slot] = (chunkIndex << VOXEL_CASCADE_BITS) | cascadeIndex;
          }
        }
      }
    }
  }

  GroupMemoryBarrierWithGroupSync();

  uint emitCount = min(gVoxelPayload.count, TASK_PAYLOAD_CAPACITY_VOXEL);
  DispatchMesh(emitCount, 1u, 1u, gVoxelPayload);
}

// Maps a triangle's dominant-axis component order, indexed by the dominant axis (x=0, y=1, z=2).
static const int3 voxelDominantAxisComponentOrders[3] = { int3(2, 1, 0), int3(0, 2, 1), int3(0, 1, 2) };

[shader("mesh")]
[numthreads(MESH_GROUP_SIZE, 1, 1)]
[outputtopology("triangle")]
void voxelMeshMain(uint localInvocationIndex : SV_GroupIndex,
                   uint groupID : SV_GroupID,
                   in payload VoxelTaskPayload payload,
                   out vertices VoxelVarying vertices[MAX_VOXEL_MESHLET_VERTICES],
                   out indices uint3 triangles[MAX_VOXEL_MESHLET_PRIMITIVES],
                   out primitives MeshletPrimitive primitives[MAX_VOXEL_MESHLET_PRIMITIVES]) {
  uint meshletLocalIndex = payload.meshletLocalIndices[groupID];
  uint meshletGlobalIndex = payload.meshletDescriptorBase + meshletLocalIndex;
  uint meshObjectID = payload.meshObjectID;
  uint packedLayer = payload.layerIndices[groupID];
  uint cascadeIndex = packedLayer & VOXEL_CASCADE_MASK;
  uint chunkIndex = packedLayer >> VOXEL_CASCADE_BITS;

  GlobalBDAPointers bda = globalBDAPointersBuffer[0];

  MeshletDescriptor descriptor = bda.meshletDescriptors[meshletGlobalIndex];
  uint primitiveCount = min(descriptor.primitiveCount, MAX_MESHLET_PRIMITIVES);

  // This workgroup voxelizes one chunk of up to MAX_VOXEL_MESHLET_PRIMITIVES triangles; the vertices
  // are not shared (3 per triangle, dominant-axis projection differs per triangle).
  uint chunkPrimitiveStart = chunkIndex * MAX_VOXEL_MESHLET_PRIMITIVES;
  uint chunkPrimitiveCount = (chunkPrimitiveStart < primitiveCount) ? min(MAX_VOXEL_MESHLET_PRIMITIVES, primitiveCount - chunkPrimitiveStart) : 0u;
  SetMeshOutputCounts(chunkPrimitiveCount * 3u, chunkPrimitiveCount);

  DrawInfo drawInfo = drawInfoItems[meshObjectID];
  MatrixPair matrixPair = bda.matrixPairs[drawInfo.matrixID];
  float4x4 modelMatrix = matrixPair.modelMatrix;

  View view = uView.views[pushConstants.viewBaseIndex];
  float3 cameraPosition = view.inverseViewMatrix[3].xyz;
  float4x4 modelViewMatrix = mulmm(view.viewMatrix, modelMatrix);
  float3x3 adjugateModelMatrix = adjugate(modelMatrix);
  float3 modelColumnLengths = float3(length(modelMatrix[0].xyz), length(modelMatrix[1].xyz), length(modelMatrix[2].xyz));

  float4x4 worldToCascadeClipSpace = voxelGridData.worldToCascadeClipSpaceMatrices[cascadeIndex];
  float cascadeHalfExtent = voxelGridData.cascadeCenterHalfExtents[cascadeIndex].w;
  bool hwConservativeRaster = voxelGridData.hardwareConservativeRasterization != 0u;
  bool debugColors = (pushConstants.drawFlags & 1u) != 0u;

  for (uint i = localInvocationIndex; i < chunkPrimitiveCount; i += MESH_GROUP_SIZE) {
    // i is the chunk-relative output index; srcPrim is the absolute primitive index in the meshlet.
    uint srcPrim = chunkPrimitiveStart + i;
    uint3 localTri = (uint3(bda.meshletPrimitives[descriptor.primitiveOffset + srcPrim]) >> uint3(0u, 8u, 16u)) & uint3(0xffu);

    float3 worldPos[3];
    for (int v = 0; v < 3; v++) {
      uint globalVertexIndex = bda.meshletVertices[descriptor.vertexOffset + localTri[v]];
      worldPos[v] = mulmv(modelMatrix, float4(unpackPosition(bda.cachedVertices[globalVertexIndex]), 1.0)).xyz;
    }

    float3 cascadeSpacePositions[3] = {
      mulmv(worldToCascadeClipSpace, float4(worldPos[0], 1.0)).xyz,
      mulmv(worldToCascadeClipSpace, float4(worldPos[1], 1.0)).xyz,
      mulmv(worldToCascadeClipSpace, float4(worldPos[2], 1.0)).xyz
    };

    // Dominant-axis projection (same as mesh_voxelization.geom): pick the axis the triangle faces
    // most strongly and project onto the other two so it rasterizes at the highest voxel resolution.
    float3 faceNormal = cross(worldPos[1] - worldPos[0], worldPos[2] - worldPos[0]);
    int3 vertexIndexOrder = (faceNormal.z > 0.0) ? int3(0, 1, 2) : int3(0, 2, 1);
    faceNormal = abs(faceNormal);
    int dominantAxisIndex = (faceNormal.y > faceNormal.x) ? ((faceNormal.y > faceNormal.z) ? 1 : 2) : ((faceNormal.x > faceNormal.z) ? 0 : 2);
    int3 daco = voxelDominantAxisComponentOrders[dominantAxisIndex];

    float4 projectionVertices[3] = {
      float4(cascadeSpacePositions[0][daco.x], cascadeSpacePositions[0][daco.y], cascadeSpacePositions[0][daco.z], 1.0),
      float4(cascadeSpacePositions[1][daco.x], cascadeSpacePositions[1][daco.y], cascadeSpacePositions[1][daco.z], 1.0),
      float4(cascadeSpacePositions[2][daco.x], cascadeSpacePositions[2][daco.y], cascadeSpacePositions[2][daco.z], 1.0)
    };

    // Conservative rasterization expansion when there is no hardware support.
    if (!hwConservativeRaster) {
      float2 sides[3] = {
        normalize(projectionVertices[vertexIndexOrder[1]].xy - projectionVertices[vertexIndexOrder[0]].xy),
        normalize(projectionVertices[vertexIndexOrder[2]].xy - projectionVertices[vertexIndexOrder[1]].xy),
        normalize(projectionVertices[vertexIndexOrder[0]].xy - projectionVertices[vertexIndexOrder[2]].xy)
      };
      float texelSize = 1.41421356237 / cascadeHalfExtent;
      projectionVertices[vertexIndexOrder[0]].xy += normalize(sides[2] - sides[0]) * texelSize;
      projectionVertices[vertexIndexOrder[1]].xy += normalize(sides[0] - sides[1]) * texelSize;
      projectionVertices[vertexIndexOrder[2]].xy += normalize(sides[1] - sides[2]) * texelSize;
    }

    float3 aabbMin = min(min(worldPos[0], worldPos[1]), worldPos[2]);
    float3 aabbMax = max(max(worldPos[0], worldPos[1]), worldPos[2]);

    uint baseVertex = i * 3u;
    for (int v = 0; v < 3; v++) {
      int currentVertexIndex = vertexIndexOrder[v];
      uint outIndex = baseVertex + uint(v);

      uint globalVertexIndex = bda.meshletVertices[descriptor.vertexOffset + localTri[currentVertexIndex]];
      PackedCachedVertex cv = bda.cachedVertices[globalVertexIndex];
      PackedStaticVertex sv = bda.staticVertices[globalVertexIndex];

      float3 position = unpackPosition(cv);
      float4 normalSign = unpackNormalSign(cv);
      float3 worldTangent = mulmv(adjugateModelMatrix, normalize(unpackTangent(cv)));
      float3 worldNormal = mulmv(adjugateModelMatrix, normalize(normalSign.xyz));

      float4 viewSpacePosition = mulmv(modelViewMatrix, float4(position, 1.0));
      viewSpacePosition.xyz /= viewSpacePosition.w;

      VoxelVarying o;
      o.position = float4(projectionVertices[currentVertexIndex].xy, fma(projectionVertices[currentVertexIndex].z, 0.5, 0.5), 1.0);
      o.worldSpacePosition = worldPos[currentVertexIndex];
      o.viewSpacePosition = viewSpacePosition.xyz;
      o.cameraRelativePosition = worldPos[currentVertexIndex] - cameraPosition;
      o.tangentSign = float4(worldTangent, normalSign.w);
      o.normal = worldNormal;
      o.texCoord0 = sv.texCoord0;
      o.texCoord1 = sv.texCoord1;
      o.color0 = debugColors ? float4(meshletDebugColor(meshletGlobalIndex), 1.0) : unpackColor0(sv);
      o.modelScale = unpackModelScale(cv) * modelColumnLengths;
      o.materialID = sv.materialID;
      o.instanceDataIndex = drawInfo.instanceDataIndex | (debugColors ? 0x80000000u : 0u);
      o.aabbMin = aabbMin;
      o.aabbMax = aabbMax;
      o.cascadeIndex = cascadeIndex;
      o.voxelPosition = fma(cascadeSpacePositions[currentVertexIndex], float3(0.5), float3(0.5));
      o.vertex0 = worldPos[vertexIndexOrder[0]];
      o.vertex1 = worldPos[vertexIndexOrder[1]];
      o.vertex2 = worldPos[vertexIndexOrder[2]];
      vertices[outIndex] = o;
    }

    triangles[i] = uint3(baseVertex, baseVertex + 1u, baseVertex + 2u);
    primitives[i].layer = cascadeIndex;
  }
}

// --- RSM / layer-routing render mode ---

groupshared float4x4 gRSMViewMatrices[8];
groupshared float4x4 gRSMProjectionMatrices[8];
groupshared Frustum gRSMFrustums[8];
groupshared float gRSMZNears[8];
groupshared float4x4 gRSMModelMatrix;
groupshared RSMTaskPayload gRSMPayload;

[shader("amplification")]
[numthreads(TASK_GROUP_SIZE, 1, 1)]
void rsmTaskMain(uint localInvocationIndex : SV_GroupIndex,
                 uint groupID : SV_GroupID,
                 uint drawIndex : SV_DrawIndex) {
  uint countViews = min(pushConstants.countViews, 8u);

  GlobalBDAPointers bda = globalBDAPointersBuffer[0];

  MeshDrawCommand* meshDrawCommands = (MeshDrawCommand*)((uint64_t(pushConstants.meshDrawCommandsBDAHigh) << 32) | uint64_t(pushConstants.meshDrawCommandsBDALow));
  MeshDrawCommand cmd = meshDrawCommands[drawIndex];
  uint meshletBase = cmd.cmd0.w;
  uint meshletCount = cmd.cmd1.x;
  uint meshObjectID = cmd.cmd1.y;

  if (localInvocationIndex == 0u) {
    gRSMModelMatrix = bda.matrixPairs[drawInfoItems[meshObjectID].matrixID].modelMatrix;
    gRSMPayload.meshletDescriptorBase = meshletBase;
    gRSMPayload.meshObjectID = meshObjectID;
    gRSMPayload.count = 0u;
  }

  if (localInvocationIndex < countViews) {
    uint viewIndex = pushConstants.viewBaseIndex + localInvocationIndex;
    View view = uView.views[viewIndex];
    gRSMViewMatrices[localInvocationIndex] = view.viewMatrix;
    gRSMProjectionMatrices[localInvocationIndex] = view.projectionMatrix;
    gRSMFrustums[localInvocationIndex] = frustumSetup(view.projectionMatrix);
    if (cullPass == 1) {
      gRSMZNears[localInvocationIndex] = abs(view.inverseProjectionMatrix[3][2] / view.inverseProjectionMatrix[2][3]);
    }
  }

  GroupMemoryBarrierWithGroupSync();

  uint meshletLocalIndex = (groupID * TASK_GROUP_SIZE) + localInvocationIndex;

  if (meshletLocalIndex < meshletCount) {
    uint meshletGlobalIndex = meshletBase + meshletLocalIndex;

    bool hasSphere;
    float4 worldSphere = computeWorldSphere(meshObjectID, meshletBase, meshletLocalIndex, meshletGlobalIndex, gRSMModelMatrix, bda, hasSphere);

    bool distanceCulled = false;
    if (hasSphere && (pushConstants.maximumDistance > 0.0)) {
      float3 cameraPosition = uView.views[0].inverseViewMatrix[3].xyz;
      if ((distance(cameraPosition, worldSphere.xyz) - worldSphere.w) > pushConstants.maximumDistance) {
        distanceCulled = true;
      }
    }

    if (((pushConstants.drawFlags & FLAG_MESHLET_CULLING_ENABLED) != 0u) && hasSphere && !distanceCulled) {
      // Test against each view independently; emit one (meshlet, view) entry per visible view.
      for (uint viewIndex = 0u; viewIndex < countViews; viewIndex++) {
        float4 viewSpaceSphere = float4(mulmv(gRSMViewMatrices[viewIndex], float4(worldSphere.xyz, 1.0)).xyz, worldSphere.w);
        if (!frustumCullingSphereTest(gRSMFrustums[viewIndex], viewSpaceSphere)) {
          continue;
        }
        if (cullPass == 1) {
          float4x4 inverseProjectionMatrix = uView.views[pushConstants.viewBaseIndex + viewIndex].inverseProjectionMatrix;
          if (!meshletPassesHiZ(viewSpaceSphere, gRSMZNears[viewIndex], gRSMProjectionMatrices[viewIndex],
                                inverseProjectionMatrix, pushConstants.textureDepthIndex, viewIndex,
                                pushConstants.areaTooSmallThreshold)) {
            continue;
          }
        }
        uint slot;
        InterlockedAdd(gRSMPayload.count, 1u, slot);
        if (slot < TASK_PAYLOAD_CAPACITY_VIEW) {
          gRSMPayload.meshletLocalIndices[slot] = meshletLocalIndex;
          gRSMPayload.layerIndices[slot] = viewIndex;
        }
      }
    } else if (!distanceCulled) {
      // Culling disabled or no valid sphere -> emit for all views (conservative).
      for (uint viewIndex = 0u; viewIndex < countViews; viewIndex++) {
        uint slot;
        InterlockedAdd(gRSMPayload.count, 1u, slot);
        if (slot < TASK_PAYLOAD_CAPACITY_VIEW) {
          gRSMPayload.meshletLocalIndices[slot] = meshletLocalIndex;
          gRSMPayload.layerIndices[slot] = viewIndex;
        }
      }
    }
  }

  GroupMemoryBarrierWithGroupSync();

  uint emitCount = min(gRSMPayload.count, TASK_PAYLOAD_CAPACITY_VIEW);
  DispatchMesh(emitCount, 1u, 1u, gRSMPayload);
}

[shader("mesh")]
[numthreads(MESH_GROUP_SIZE, 1, 1)]
[outputtopology("triangle")]
void rsmMeshMain(uint localInvocationIndex : SV_GroupIndex,
                 uint groupID : SV_GroupID,
                 in payload RSMTaskPayload payload,
                 out vertices VertexInputVarying<velocity> vertices[MAX_MESHLET_VERTICES],
                 out indices uint3 triangles[MAX_MESHLET_PRIMITIVES],
                 out primitives MeshletPrimitive primitives[MAX_MESHLET_PRIMITIVES]) {
  uint meshletLocalIndex = payload.meshletLocalIndices[groupID];
  uint layerIndex = payload.layerIndices[groupID];
  uint meshletGlobalIndex = payload.meshletDescriptorBase + meshletLocalIndex;

  GlobalBDAPointers bda = globalBDAPointersBuffer[0];

  // Layer routing: the view index comes from the payload (per cascade), not from multiview.
  emitStandardMeshlet(localInvocationIndex, pushConstants.viewBaseIndex + layerIndex, meshletGlobalIndex, payload.meshObjectID, bda, vertices, triangles);

  uint primitiveCount = min(bda.meshletDescriptors[meshletGlobalIndex].primitiveCount, MAX_MESHLET_PRIMITIVES);
  for (uint i = localInvocationIndex; i < primitiveCount; i += MESH_GROUP_SIZE) {
    primitives[i].layer = layerIndex;
  }
}
