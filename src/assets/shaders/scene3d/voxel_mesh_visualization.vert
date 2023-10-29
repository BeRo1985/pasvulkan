#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable

#if defined(USEGEOMETRYSHADER)
layout(location = 0) out vec3 outPosition; 
layout(location = 1) flat out int outCascadeIndex;
layout(location = 2) flat out mat4 outViewProjectionMatrix;
#else
layout(location = 0) out vec4 outColor;  
#endif

/* clang-format off */

layout(push_constant) uniform PushConstants {
  uint viewBaseIndex; 
  uint countViews;    
  uint gridSizeBits;     
  uint cascadeIndex;   
} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 0, binding = 0, std140) uniform uboViews {
   View views[256];
} uView;

#if !defined(USEGEOMETRYSHADER)
layout (set = 1, binding = 0, std140) readonly uniform VoxelGridData {
  #include "voxelgriddata_uniforms.glsl"
} voxelGridData;

layout(set = 1, binding = 1) uniform sampler3D uVoxelGridOcclusion[];

layout(set = 1, binding = 2) uniform sampler3D uVoxelGridRadiance[];
#endif

/* clang-format on */

void main() {
#if defined(USEGEOMETRYSHADER)

  outPosition = vec3(ivec3((ivec3(int(gl_VertexIndex)) >> (ivec3(0, 1, 2) * ivec3(int(pushConstants.gridSizeBits)))) & ivec3(int(uint((1u << pushConstants.gridSizeBits) - 1u)))));
  outCascadeIndex = int(pushConstants.cascadeIndex);
  outViewProjectionMatrix = uView.views[pushConstants.viewBaseIndex + uint(gl_ViewIndex)].projectionMatrix * uView.views[pushConstants.viewBaseIndex + uint(gl_ViewIndex)].viewMatrix;

#else

  uint cascadeIndex = pushConstants.cascadeIndex;

  uint gridSize = 1u << pushConstants.gridSizeBits;
  uint gridMask = gridSize - 1u;

  uint vertexIndex = uint(gl_VertexIndex);

  uint triangleIndex = vertexIndex / 3u;
  uint triangleVertexIndex = vertexIndex - (triangleIndex * 3u);

  uint cubeFaceIndex = triangleIndex >> 1u;
  uint cubeFaceTriangleIndex = triangleIndex & 1u; 

  uint cubeIndex = cubeFaceIndex / 6u;
  uint cubeSideIndex = cubeFaceIndex - (cubeIndex * 6u);

  ivec3 voxelPosition = ivec3(uvec3(uvec3(uvec3(cubeIndex) >> (uvec3(0u, 1u, 2u) * uint(pushConstants.gridSizeBits))) & uvec3(gridMask)));

  bool valid = ((cascadeIndex == 0u) || // First cascade is always the highest resolution cascade, so no further check is needed here
                !(all(greaterThanEqual(voxelPosition, voxelGridData.cascadeAvoidAABBGridMin[cascadeIndex].xyz)) &&
                  all(lessThan(voxelPosition, voxelGridData.cascadeAvoidAABBGridMax[cascadeIndex].xyz))));

  vec4 voxel = valid ? texelFetch(uVoxelGridRadiance[(cascadeIndex * 6) + cubeSideIndex], voxelPosition, 0) : vec4(0.0);

  if(dot(voxel, voxel) > 0.0){

    outColor = voxel;

    const ivec3 vertices[8] = ivec3[8](
      ivec3(0, 0, 0),
      ivec3(1, 0, 0),
      ivec3(0, 1, 0),
      ivec3(1, 1, 0),
      ivec3(0, 0, 1),
      ivec3(1, 0, 1),
      ivec3(0, 1, 1),
      ivec3(1, 1, 1)
    );

    const ivec4 quadIndicesArray[6] = ivec4[6](
      ivec4(0, 2, 6, 4), // -X
      ivec4(1, 5, 7, 3), // +X
      ivec4(0, 4, 5, 1), // -Y
      ivec4(2, 3, 7, 6), // +Y
      ivec4(0, 1, 3, 2), // -Z
      ivec4(4, 6, 7, 5)  // +Z
    );

    const ivec3 quadTriangleIndices[2] = ivec3[2](
      ivec3(0, 1, 2),
      ivec3(0, 2, 3)
    );

    gl_Position = (uView.views[pushConstants.viewBaseIndex + uint(gl_ViewIndex)].projectionMatrix * 
                   uView.views[pushConstants.viewBaseIndex + uint(gl_ViewIndex)].viewMatrix) * 
                   vec4(fma(vec3(ivec3(voxelPosition + vertices[quadIndicesArray[cubeSideIndex][quadTriangleIndices[cubeFaceTriangleIndex][triangleVertexIndex]]])), 
                            vec3(voxelGridData.cascadeCellSizes[cascadeIndex >> 2u][cascadeIndex & 3u]), 
                            voxelGridData.cascadeAABBMin[cascadeIndex].xyz), 
                        1.0);

  }else{

    outColor = vec4(0.0);

    // Generate degenerated out-of-clip-space gl_Position to avoid rendering
    gl_Position = vec4(2.0, 2.0, 2.0, 1.0);
    
  }    

#endif
}
