// Slang mesh shader: the BDA-vertex-pulling mesh path, Slang port of mesh.vert / mesh.frag.
//
// The descriptor and push-constant declarations live in mesh_bindings.slang and the vertex
// fetch/transform in mesh_vertexpull.slang; both mirror the GLSL layout so the SPIR-V is drop-in
// compatible with the Pascal-side pipeline.

import gputypes;
import mesh_bindings;
import mesh_vertexpull;

// --- stage interface ---

struct VSOutput {
  float4 position : SV_Position;
  float3 worldSpacePosition : WORLDPOS;
  float2 texCoord0 : TEXCOORD0;
  nointerpolation uint materialID : MATERIALID;
  nointerpolation int viewIndex : VIEWINDEX;
}

// --- vertex stage ---

[shader("vertex")]
VSOutput vertexMain(uint vertexID : SV_VertexID, uint instanceID : SV_InstanceID, uint viewID : SV_ViewID) {
  uint viewIndex = pushConstants.viewBaseIndex + viewID;
  PulledVertex v = pullVertex(vertexID, instanceID, viewIndex);

  VSOutput output;
  output.position = v.clipSpacePosition;
  output.worldSpacePosition = v.worldSpacePosition;
  output.texCoord0 = v.texCoord0;
  output.materialID = v.materialID;
  output.viewIndex = int(viewIndex);
  return output;
}

// --- material texture access: the single capsule point for the bindless descriptor model, so a
// switch from the classic descriptor array to DescriptorHandle / descriptor heap stays local and
// does not touch call sites ---

float4 sampleMaterialTexture(uint textureSlot, float2 uv) {
  return u2DTextures[NonUniformResourceIndex(textureSlot)].Sample(uv);
}

// --- fragment stage ---

[shader("pixel")]
float4 fragmentMain(VSOutput input) : SV_Target0 {
  // Debug visualization of materialID and the interpolated UVs.
  float3 debugColor = float3(frac(float(input.materialID) * 0.125),
                             frac(input.texCoord0));
  return float4(debugColor, 1.0);
}
