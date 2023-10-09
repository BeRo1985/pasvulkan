#version 450 core

#define CLIPMAP_SHIFT 2 // for 4 clipmaps
#define CLIPMAP_SIZE (1 << CLIPMAP_SHIFT)
#define CLIPMAP_MASK (CLIPMAP_SIZE - 1)
#define COUNT_CLIPMAPS CLIPMAP_SIZE

#define COUNT_AXIS 3 // for 3 axis in 3D space

layout(triangles, invocations = COUNT_CLIPMAPS) in;
layout(triangle_strip, max_vertices = 3) out;

layout(location = 0) in vec3 inWorldSpacePosition[];
layout(location = 1) in vec3 inTangent[];
layout(location = 2) in vec3 inBitangent[];
layout(location = 3) in vec3 inNormal[];
layout(location = 4) in vec2 inTexCoord0[];
layout(location = 5) in vec2 inTexCoord1[];
layout(location = 6) in vec4 inColor0[];
layout(location = 7) in vec3 intModelScale[];
layout(location = 8) flat in uint inMaterialID[];

layout(location = 0) out vec3 outWorldSpacePosition;
layout(location = 1) out vec3 outTangent;
layout(location = 2) out vec3 outBitangent;
layout(location = 3) out vec3 outNormal;
layout(location = 4) out vec2 outTexCoord0;
layout(location = 5) out vec2 outTexCoord1;
layout(location = 6) out vec4 outColor0;
layout(location = 7) out vec3 outModelScale;
layout(location = 8) flat out uint outMaterialID;
layout(location = 9) flat out vec3 outAABBMin;
layout(location = 10) flat out vec3 outAABBMax;

layout(push_constant) uniform PushConstants {
  vec4 clipMaps[COUNT_CLIPMAPS]; // xyz = center in world-space, w = extent of a voxel 
  int hardwareConservativeRasterization; // 0 = false, 1 = true
} pushConstants;

void main(){

  int clipMapIndex = int(gl_InvocationID);
  
  vec3 clipMapSpacePositions[3] = vec3[3](
    vec3((inWorldSpacePosition[0] - pushConstants.clipMaps[clipMapIndex].xyz) / pushConstants.clipMaps[clipMapIndex].w),
    vec3((inWorldSpacePosition[1] - pushConstants.clipMaps[clipMapIndex].xyz) / pushConstants.clipMaps[clipMapIndex].w),
    vec3((inWorldSpacePosition[2] - pushConstants.clipMaps[clipMapIndex].xyz) / pushConstants.clipMaps[clipMapIndex].w)
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
      clipMapSpacePositions[0][dominantAxisComponentOrder.z], 
      1.0
    ),
    vec4(
      clipMapSpacePositions[1][dominantAxisComponentOrder.x], 
      clipMapSpacePositions[1][dominantAxisComponentOrder.y], 
      clipMapSpacePositions[1][dominantAxisComponentOrder.z], 
      1.0
    ),
    vec4(
      clipMapSpacePositions[2][dominantAxisComponentOrder.x], 
      clipMapSpacePositions[2][dominantAxisComponentOrder.y], 
      clipMapSpacePositions[2][dominantAxisComponentOrder.z], 
      1.0
    )
  );

  // When using no hardware conservative rasterization, we need to expand the triangle by one texel in each direction manually to avoid holes in the voxelization.
  if(pushConstants.hardwareConservativeRasterization == 0){
    vec2 sides[3] = vec2[3](
      vec2(normalize(projectionVertices[vertexIndexOrder[1]].xy - projectionVertices[vertexIndexOrder[0]].xy)),
      vec2(normalize(projectionVertices[vertexIndexOrder[2]].xy - projectionVertices[vertexIndexOrder[1]].xy)),
      vec2(normalize(projectionVertices[vertexIndexOrder[0]].xy - projectionVertices[vertexIndexOrder[2]].xy))
    );
    float texelSize = 1.41421356237 / pushConstants.clipMaps[clipMapIndex].w;
    projectionVertices[vertexIndexOrder[0]].xy += normalize(sides[2] - sides[0]) * texelSize;
    projectionVertices[vertexIndexOrder[1]].xy += normalize(sides[0] - sides[1]) * texelSize;
    projectionVertices[vertexIndexOrder[2]].xy += normalize(sides[1] - sides[2]) * texelSize;
  }

  vec3 aabbMin = min(min(inWorldSpacePosition[0], inWorldSpacePosition[1]), inWorldSpacePosition[2]);
  vec3 aabbMax = max(max(inWorldSpacePosition[0], inWorldSpacePosition[1]), inWorldSpacePosition[2]);
  
  for(int vertexIndex = 0; vertexIndex < 3; vertexIndex++){

    int currentVertexIndex = vertexIndexOrder[vertexIndex];

    outWorldSpacePosition = inWorldSpacePosition[currentVertexIndex];
        
    outTangent = inTangent[currentVertexIndex];
    outBitangent = inBitangent[currentVertexIndex];
    outNormal = inNormal[currentVertexIndex];

    outTexCoord0 = inTexCoord0[currentVertexIndex];
    outTexCoord1 = inTexCoord1[currentVertexIndex];

    outColor0 = inColor0[currentVertexIndex];

    outModelScale = intModelScale[currentVertexIndex];

    outMaterialID = inMaterialID[currentVertexIndex];

    outAABBMin = aabbMin;

    outAABBMax = aabbMax;    

    gl_Position = projectionVertices[currentVertexIndex];

    gl_ViewportIndex = clipMapIndex;

    EmitVertex();

  }

  EndPrimitive();

}