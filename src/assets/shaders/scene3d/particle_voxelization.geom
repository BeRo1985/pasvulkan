#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable

layout(triangles, invocations = COUNT_CLIPMAPS) in; // COUNT_CLIPMAPS is defined by the compiling call in the build script 
layout(triangle_strip, max_vertices = 3) out;

layout(location = 0) in vec3 inWorldSpacePosition[];
layout(location = 1) in vec2 inTexCoord[];
layout(location = 2) in vec4 inColor[];
layout(location = 3) flat in uint inTextureID[];

layout(location = 0) out vec3 outWorldSpacePosition;
layout(location = 1) out vec2 outTexCoord;
layout(location = 2) out vec4 outColor;
layout(location = 3) out vec3 outNormal;
layout(location = 4) flat out uint outTextureID;
layout(location = 5) flat out vec3 outAABBMin;
layout(location = 6) flat out vec3 outAABBMax;
layout(location = 7) flat out uint outClipMapIndex;

#define VOXELIZATION
#include "voxelization_globals.glsl"

void main(){

  uint clipMapIndex = uint(gl_InvocationID);

  if(clipMapIndex < voxelGridData.countClipMaps){ // Just for to be sure alongside the invocations count above
    
    vec3 clipMapSpacePositions[3] = vec3[3](
      vec3((inWorldSpacePosition[0] - voxelGridData.clipMaps[clipMapIndex].xyz) / voxelGridData.clipMaps[clipMapIndex].w),
      vec3((inWorldSpacePosition[1] - voxelGridData.clipMaps[clipMapIndex].xyz) / voxelGridData.clipMaps[clipMapIndex].w),
      vec3((inWorldSpacePosition[2] - voxelGridData.clipMaps[clipMapIndex].xyz) / voxelGridData.clipMaps[clipMapIndex].w)
    );   

    vec3 faceNormal = cross(inWorldSpacePosition[1] - inWorldSpacePosition[0], inWorldSpacePosition[2] - inWorldSpacePosition[0]);

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
      vec4(
        clipMapSpacePositions[0][dominantAxisComponentOrder.x],
        clipMapSpacePositions[0][dominantAxisComponentOrder.y], 
        0.0, //clipMapSpacePositions[0][dominantAxisComponentOrder.z], 
        1.0
      ),
      vec4(
        clipMapSpacePositions[1][dominantAxisComponentOrder.x], 
        clipMapSpacePositions[1][dominantAxisComponentOrder.y], 
        0.0, //clipMapSpacePositions[1][dominantAxisComponentOrder.z], 
        1.0
      ),
      vec4(
        clipMapSpacePositions[2][dominantAxisComponentOrder.x], 
        clipMapSpacePositions[2][dominantAxisComponentOrder.y], 
        0.0, //clipMapSpacePositions[2][dominantAxisComponentOrder.z], 
        1.0
      )
    );

    // When using no hardware conservative rasterization, we need to expand the triangle by one texel in each direction manually to avoid holes in the voxelization.
    if(voxelGridData.hardwareConservativeRasterization == 0u){
      vec2 sides[3] = vec2[3](
        vec2(normalize(projectionVertices[vertexIndexOrder[1]].xy - projectionVertices[vertexIndexOrder[0]].xy)),
        vec2(normalize(projectionVertices[vertexIndexOrder[2]].xy - projectionVertices[vertexIndexOrder[1]].xy)),
        vec2(normalize(projectionVertices[vertexIndexOrder[0]].xy - projectionVertices[vertexIndexOrder[2]].xy))
      );
      float texelSize = 1.41421356237 / voxelGridData.clipMaps[clipMapIndex].w;
      projectionVertices[vertexIndexOrder[0]].xy += normalize(sides[2] - sides[0]) * texelSize;
      projectionVertices[vertexIndexOrder[1]].xy += normalize(sides[0] - sides[1]) * texelSize;
      projectionVertices[vertexIndexOrder[2]].xy += normalize(sides[1] - sides[2]) * texelSize;
    }

    vec3 aabbMin = min(min(inWorldSpacePosition[0], inWorldSpacePosition[1]), inWorldSpacePosition[2]);
    vec3 aabbMax = max(max(inWorldSpacePosition[0], inWorldSpacePosition[1]), inWorldSpacePosition[2]);
    
    for(int vertexIndex = 0; vertexIndex < 3; vertexIndex++){

      int currentVertexIndex = vertexIndexOrder[vertexIndex];

      outWorldSpacePosition = inWorldSpacePosition[currentVertexIndex];

      outTexCoord = inTexCoord[currentVertexIndex];
      
      outColor = inColor[currentVertexIndex];

      outNormal = faceNormal;

      outTextureID = inTextureID[currentVertexIndex];

      outAABBMin = aabbMin;

      outAABBMax = aabbMax;    

      outClipMapIndex = clipMapIndex;

      gl_Position = projectionVertices[currentVertexIndex];

      //gl_ViewportIndex = clipMapIndex;

      EmitVertex();

    }

    EndPrimitive();

  }

}