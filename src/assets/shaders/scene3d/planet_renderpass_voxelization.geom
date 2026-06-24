#version 450 core

#pragma shader_stage(geometry)

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable

// Per-cascade amplification for the planet surface voxelization (vertex+geometry path). One geometry-shader invocation
// per cascade (COUNT_CLIPMAPS is defined per cascade count by the build script, exactly like mesh_voxelization.geom).
// The planet vertex shader output block is passed through unchanged so the fragment shader can still compute the planet
// albedo from sphereNormal / triplanarPosition; only the dominant-axis projection + cascade routing is added here.

layout(triangles, invocations = COUNT_CLIPMAPS) in;
layout(triangle_strip, max_vertices = 3) out;

// Must match the non-raytracing OutBlock of planet_renderpass.vert.
layout(location = 0) in PlanetVertexBlock {
  vec3 position;
  vec3 sphereNormal;
  vec3 normal;
  vec3 triplanarNormal;
  vec3 triplanarPosition;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  flat uint meshletID;
} inBlock[];

layout(location = 0) out PlanetVertexBlock {
  vec3 position;
  vec3 sphereNormal;
  vec3 normal;
  vec3 triplanarNormal;
  vec3 triplanarPosition;
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  flat uint meshletID;
} outBlock;

layout(location = 10) flat out uint outCascadeIndex;
layout(location = 11) out vec3 outVoxelPosition;

#define VOXELIZATION
#define VOXELIZATION_DESCRIPTOR_SET 4
#include "voxelization_globals.glsl"

void main(){

  uint cascadeIndex = uint(gl_InvocationID);

  if(cascadeIndex < voxelGridData.countCascades){ // Just to be sure alongside the invocations count above

    vec3 worldPositions[3] = vec3[3](
      inBlock[0].worldSpacePosition,
      inBlock[1].worldSpacePosition,
      inBlock[2].worldSpacePosition
    );

    // Per-cascade cull: if this triangle's world AABB doesn't overlap this cascade's volume, emit nothing. This is the big
    // win for the vertex+geometry path — it skips rasterization and, crucially, the heavy planet albedo-splat fragment work
    // for the (vast) part of the planet that lies outside the cascade around the camera.
    vec3 triAABBMin = min(worldPositions[0], min(worldPositions[1], worldPositions[2]));
    vec3 triAABBMax = max(worldPositions[0], max(worldPositions[1], worldPositions[2]));
    if(!(all(lessThanEqual(triAABBMin, voxelGridData.cascadeAABBMax[cascadeIndex].xyz)) &&
         all(greaterThanEqual(triAABBMax, voxelGridData.cascadeAABBMin[cascadeIndex].xyz)))){
      return;
    }

    vec3 cascadeSpacePositions[3] = vec3[3](
      vec3(voxelGridData.worldToCascadeClipSpaceMatrices[cascadeIndex] * vec4(worldPositions[0], 1.0)).xyz,
      vec3(voxelGridData.worldToCascadeClipSpaceMatrices[cascadeIndex] * vec4(worldPositions[1], 1.0)).xyz,
      vec3(voxelGridData.worldToCascadeClipSpaceMatrices[cascadeIndex] * vec4(worldPositions[2], 1.0)).xyz
    );

    vec3 faceNormal = cross(worldPositions[1] - worldPositions[0], worldPositions[2] - worldPositions[0]);

    ivec3 vertexIndexOrder = (faceNormal.z > 0.0) ? ivec3(0, 1, 2) : ivec3(0, 2, 1);

    faceNormal = abs(faceNormal);

    int dominantAxisIndex = (faceNormal.y > faceNormal.x) ? ((faceNormal.y > faceNormal.z) ? 1 : 2) : ((faceNormal.x > faceNormal.z) ? 0 : 2);

    const ivec3 dominantAxisComponentOrders[3] = ivec3[3](
      ivec3(2, 1, 0), // zyx
      ivec3(0, 2, 1), // xzy
      ivec3(0, 1, 2)  // xyz
    );

    ivec3 dominantAxisComponentOrder = dominantAxisComponentOrders[dominantAxisIndex];

    vec4 projectionVertices[3] = vec4[3](
      vec4(cascadeSpacePositions[0][dominantAxisComponentOrder.x], cascadeSpacePositions[0][dominantAxisComponentOrder.y], cascadeSpacePositions[0][dominantAxisComponentOrder.z], 1.0),
      vec4(cascadeSpacePositions[1][dominantAxisComponentOrder.x], cascadeSpacePositions[1][dominantAxisComponentOrder.y], cascadeSpacePositions[1][dominantAxisComponentOrder.z], 1.0),
      vec4(cascadeSpacePositions[2][dominantAxisComponentOrder.x], cascadeSpacePositions[2][dominantAxisComponentOrder.y], cascadeSpacePositions[2][dominantAxisComponentOrder.z], 1.0)
    );

    // Without hardware conservative rasterization, expand the triangle by one texel along its edge bisectors to avoid holes.
    if(voxelGridData.hardwareConservativeRasterization == 0u){
      vec2 sides[3] = vec2[3](
        vec2(normalize(projectionVertices[vertexIndexOrder[1]].xy - projectionVertices[vertexIndexOrder[0]].xy)),
        vec2(normalize(projectionVertices[vertexIndexOrder[2]].xy - projectionVertices[vertexIndexOrder[1]].xy)),
        vec2(normalize(projectionVertices[vertexIndexOrder[0]].xy - projectionVertices[vertexIndexOrder[2]].xy))
      );
      float texelSize = 1.41421356237 / voxelGridData.cascadeCenterHalfExtents[cascadeIndex].w;
      projectionVertices[vertexIndexOrder[0]].xy += normalize(sides[2] - sides[0]) * texelSize;
      projectionVertices[vertexIndexOrder[1]].xy += normalize(sides[0] - sides[1]) * texelSize;
      projectionVertices[vertexIndexOrder[2]].xy += normalize(sides[1] - sides[2]) * texelSize;
    }

    for(int vertexIndex = 0; vertexIndex < 3; vertexIndex++){

      int currentVertexIndex = vertexIndexOrder[vertexIndex];

      outBlock.position = inBlock[currentVertexIndex].position;
      outBlock.sphereNormal = inBlock[currentVertexIndex].sphereNormal;
      outBlock.normal = inBlock[currentVertexIndex].normal;
      outBlock.triplanarNormal = inBlock[currentVertexIndex].triplanarNormal;
      outBlock.triplanarPosition = inBlock[currentVertexIndex].triplanarPosition;
      outBlock.worldSpacePosition = inBlock[currentVertexIndex].worldSpacePosition;
      outBlock.viewSpacePosition = inBlock[currentVertexIndex].viewSpacePosition;
      outBlock.cameraRelativePosition = inBlock[currentVertexIndex].cameraRelativePosition;
      outBlock.meshletID = inBlock[currentVertexIndex].meshletID;

      outCascadeIndex = cascadeIndex;

      outVoxelPosition = fma(cascadeSpacePositions[currentVertexIndex].xyz, vec3(0.5), vec3(0.5));

      gl_Position = vec4(projectionVertices[currentVertexIndex].xy, fma(projectionVertices[currentVertexIndex].z, 0.5, 0.5), 1.0);

      EmitVertex();

    }

    EndPrimitive();

  }

}
