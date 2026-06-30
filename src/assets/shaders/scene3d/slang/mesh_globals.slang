// Shared declarations for the mesh geometry path: the fragment-facing varying struct that both the
// vertex-stage and the mesh-shader entries produce, the per-draw transform state and per-vertex
// transform that fill it, and the bindless material-texture access. The stage entry points live in
// their own files (mesh_vertex / mesh_meshlet / mesh_fragment) and import this module.

module mesh_globals;

import gputypes;
import mesh_bindings;
import mesh_meshlet_config;  // velocity spec constant
import utils;
import math;
import debug;

// Per-vertex varyings, locations identical to mesh.vert / mesh.mesh, so the fragment shader is the
// same for the vertex-stage and mesh-shader geometry paths. The velocity slots (13/14) are always
// declared; the velocity spec constant only gates whether they carry a real previous-frame position.
public struct VertexInputVarying {
  public float4 position : SV_Position;
  [[vk::location(0)]] public float3 worldSpacePosition : LOC0;
  [[vk::location(1)]] public float3 viewSpacePosition : LOC1;
  [[vk::location(2)]] public float3 cameraRelativePosition : LOC2;
  [[vk::location(3)]] public float4 tangentSign : LOC3;
  [[vk::location(4)]] public float3 normal : LOC4;
  [[vk::location(5)]] public float2 texCoord0 : LOC5;
  [[vk::location(6)]] public float2 texCoord1 : LOC6;
  [[vk::location(7)]] public float4 color0 : LOC7;
  [[vk::location(8)]] public float3 modelScale : LOC8;
  [[vk::location(9)]] public nointerpolation uint materialID : LOC9;
  [[vk::location(10)]] public nointerpolation uint instanceDataIndex : LOC10;
  [[vk::location(11)]] public nointerpolation int viewIndex : LOC11;
  [[vk::location(12)]] public nointerpolation uint frameIndex : LOC12;
  [[vk::location(13)]] public float4 previousClipSpace : LOC13;
  [[vk::location(14)]] public float4 currentClipSpace : LOC14;
}

// Per-draw transform state (matrices + per-instance ids), computed once per draw / meshlet and
// reused for every vertex. Shared by the vertex-stage and mesh-shader paths.
public struct VertexTransform {
  public float4x4 modelMatrix;
  public float4x4 modelViewMatrix;
  public float3x3 adjugateModelMatrix;
  public float3 modelColumnLengths;
  public float3 cameraPosition;
  public float4x4 projectionMatrix;
  public float4x4 previousModelViewProjectionMatrix;
  public uint instanceDataIndex;
  public int viewIndex;
}

public VertexTransform makeVertexTransform(uint meshObjectID, uint viewIndex, GlobalBDAPointers bda) {
  DrawInfo drawInfo = drawInfoItems[meshObjectID];
  MatrixPair matrixPair = bda.matrixPairs[drawInfo.matrixID];
  float4x4 modelMatrix = matrixPair.modelMatrix;
  View view = uView.views[viewIndex];

  VertexTransform t;
  t.modelMatrix = modelMatrix;
  t.modelViewMatrix = mulmm(view.viewMatrix, modelMatrix);
  t.adjugateModelMatrix = adjugate(modelMatrix);
  t.modelColumnLengths = float3(length(modelMatrix[0].xyz), length(modelMatrix[1].xyz), length(modelMatrix[2].xyz));
  t.cameraPosition = view.inverseViewMatrix[3].xyz;
  t.projectionMatrix = view.projectionMatrix;
  t.instanceDataIndex = drawInfo.instanceDataIndex;
  t.viewIndex = int(viewIndex);

  t.previousModelViewProjectionMatrix = float4x4(0.0);
  if (velocity) {
    View previousView = uView.views[viewIndex + pushConstants.countAllViews];
    t.previousModelViewProjectionMatrix = mulmm(previousView.projectionMatrix, mulmm(previousView.viewMatrix, matrixPair.previousModelMatrix));
  }
  return t;
}

// The standard per-vertex transform shared by the vertex-stage and mesh-shader paths. debugColorID
// feeds the per-id debug color (the meshlet index for the mesh path, the instance index for the
// vertex path).
public VertexInputVarying transformVertex(uint globalVertexIndex, GlobalBDAPointers bda, VertexTransform t, uint debugColorID) {
  PackedCachedVertex cv = bda.cachedVertices[globalVertexIndex];
  PackedStaticVertex sv = bda.staticVertices[globalVertexIndex];

  float3 position = unpackPosition(cv);
  float4 normalSign = unpackNormalSign(cv);
  float3 tangent = unpackTangent(cv);
  float3 modelScale = unpackModelScale(cv) * t.modelColumnLengths;

  // Only the tangent (T) and normal (N) of the tangent frame are output downstream, so the
  // bitangent column is not built; both are carried into world space by the adjugate.
  float3 worldTangent = mulmv(t.adjugateModelMatrix, normalize(tangent));
  float3 worldNormal = mulmv(t.adjugateModelMatrix, normalize(normalSign.xyz));

  float4 viewSpacePosition = mulmv(t.modelViewMatrix, float4(position, 1.0));
  float4 clipSpacePosition = mulmv(t.projectionMatrix, viewSpacePosition);
  viewSpacePosition.xyz /= viewSpacePosition.w;
  float3 worldSpacePosition = mulmv(t.modelMatrix, float4(position, 1.0)).xyz;

  bool debugColors = (pushConstants.drawFlags & 1u) != 0u;

  VertexInputVarying o;
  o.position = clipSpacePosition;
  o.worldSpacePosition = worldSpacePosition;
  o.viewSpacePosition = viewSpacePosition.xyz;
  o.cameraRelativePosition = worldSpacePosition - t.cameraPosition;
  o.tangentSign = float4(worldTangent, normalSign.w);
  o.normal = worldNormal;
  o.texCoord0 = sv.texCoord0;
  o.texCoord1 = sv.texCoord1;
  o.color0 = debugColors ? float4(meshletDebugColor(debugColorID), 1.0) : unpackColor0(sv);
  o.modelScale = modelScale;
  o.materialID = sv.materialID;
  o.instanceDataIndex = t.instanceDataIndex | (debugColors ? 0x80000000u : 0u);
  o.viewIndex = t.viewIndex;
  o.frameIndex = pushConstants.frameIndex;

  float4 previousClipSpace = clipSpacePosition;
  if (velocity) {
    // A generation mismatch means the previous-frame vertex is not comparable -> zero motion.
    if (bda.generations[globalVertexIndex] == bda.previousGenerations[globalVertexIndex]) {
      float3 previousPosition = unpackPosition(bda.previousCachedVertices[globalVertexIndex]);
      previousClipSpace = mulmv(t.previousModelViewProjectionMatrix, float4(previousPosition, 1.0));
    }
  }
  o.currentClipSpace = clipSpacePosition;
  o.previousClipSpace = previousClipSpace;

  return o;
}

// Bindless material texture access: the single capsule point for the descriptor model, so a switch
// to DescriptorHandle / descriptor heap stays local and does not touch call sites.
public float4 sampleMaterialTexture(uint textureSlot, float2 uv) {
  return u2DTextures[NonUniformResourceIndex(textureSlot)].Sample(uv);
}
