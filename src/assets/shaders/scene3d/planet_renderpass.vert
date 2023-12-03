#version 450 core

#pragma shader_stage(vertex)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

#ifdef EXTERNAL_VERTICES
layout(location = 0) in vec3 inVector;
#endif

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 normal;
  vec3 planetCenterToCamera;
} outBlock;

layout(push_constant) uniform PushConstants {
  
  mat4 modelMatrix;
  
  uint viewBaseIndex;
  uint countViews;
  uint countQuadPointsInOneDirection; 
  uint countAllViews;
  
  float bottomRadius;
  float topRadius;
  float resolutionX;  
  float resolutionY;  
  
  float heightMapScale;
  float tessellationFactor;
  vec2 jitter;

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

uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix; 

#ifndef EXTERNAL_VERTICES
uint countQuadPointsInOneDirection = pushConstants.countQuadPointsInOneDirection;
uint countSideQuads = countQuadPointsInOneDirection * countQuadPointsInOneDirection;
#ifdef TRIANGLES
uint countTotalVertices = countSideQuads * (6u * 6u); // 2 triangles per quad, 3 vertices per triangle, 6 sides
#else
uint countTotalVertices = countSideQuads * (6u * 4u); // 4 vertices per quad, 6 sides
#endif

const ivec2 offsets[4] = ivec2[](
  ivec2(0, 0),  
  ivec2(0, 1),  
  ivec2(1, 1),  
  ivec2(1, 0)
);  

const mat3 sideMatrices[6] = mat3[6](
  mat3(vec3(0.0, 0.0, -1.0), vec3(0.0, -1.0, 0.0), vec3(-1.0, 0.0, 0.0)), // pos x
  mat3(vec3(0.0, 0.0, 1.0), vec3(0.0, -1.0, 0.0), vec3(1.0, 0.0, 0.0)),   // neg x
  mat3(vec3(1.0, 0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 1.0, 0.0)),   // pos y
  mat3(vec3(1.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, -1.0, 0.0)),   // neg y
  mat3(vec3(1.0, 0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, 0.0, -1.0)),  // pos z
  mat3(vec3(-1.0, 0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, 0.0, 1.0))   // neg z
);  

const float HalfPI = 1.5707963267948966, // 1.570796326794896619231,
            PI = 3.141592653589793, // 3.141592653589793238463,
            PI2 = 6.283185307179586; // 6.283185307179586476925   
            
vec3 getNormal(mat3 m, vec2 uv){
  vec3 unitCube = m * vec3((uv - vec2(0.5)) * 2.0, 1.0),
#if 1
       // Spherified Cube
       // http://mathproofs.blogspot.com/2005/07/mapping-cube-to-sphere.html
       // http://petrocket.blogspot.com/2010/04/sphere-to-cube-mapping.html
       unitCubeSquared = unitCube * unitCube,
       unitCubeSquaredDiv2 = unitCubeSquared * 0.5,
       unitCubeSquaredDiv3 = unitCubeSquared / 3.0,
       mappedSphere = unitCube * sqrt(((1.0 - unitCubeSquaredDiv2.yzx) - 
                                       unitCubeSquaredDiv2.zxy) + 
                                      (unitCubeSquared.yzx * 
                                       unitCubeSquaredDiv3.zxy)),
       normal = normalize(mappedSphere);
#else
       // Normalized Cube
       normal = normalize(unitCube);
#endif
  return normal; 
}
#endif

#if 0
const mat3 tangentTransformMatrix = mat3(vec3(0.0, 0.0, -1.0), vec3(0.0, -1.0, 0.0), vec3(1.0, 0.0, 0.0)),
           bitangentTransformMatrix = mat3(vec3(-1.0, 0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 1.0, 0.0));
#endif

void main(){          
#ifdef EXTERNAL_VERTICES
  vec3 normal = normalize(inVector),
       position = (pushConstants.modelMatrix * vec4(normal * pushConstants.bottomRadius, 1.0)).xyz;
#if 0
  vec3 tangent = getNormal(normalMatrix * tangentTransformMatrix, uv),
       bitangent = getNormal(normalMatrix * bitangentTransformMatrix, uv);
  tangent = normalize(tangent - (dot(tangent, normal) * normal));
  bitangent = normalize(bitangent - (dot(bitangent, normal) * normal));
  tangent = cross(bitangent, normal);
  bitangent = cross(normal, tangent);
#endif
  outBlock.position = position;    
  outBlock.normal = normal;
  outBlock.planetCenterToCamera = inverseViewMatrix[3].xyz - (pushConstants.modelMatrix * vec2(0.0, 1.0).xxxy).xyz; 
#else       
  uint vertexIndex = uint(gl_VertexIndex);
  if(vertexIndex < countTotalVertices){   
#ifdef TRIANGLES
   uint triangleIndex = vertexIndex / 3u,
        triangleVertexIndex = vertexIndex - (triangleIndex * 3u),
        quadIndex = triangleIndex >> 1u,
        quadVertexIndex = uvec3[2]( uvec3(0u, 1u, 2u), uvec3(0u, 2u, 3u))[triangleIndex & 1u][triangleVertexIndex];
#else
    uint quadIndex = vertexIndex >> 2u,
         quadVertexIndex = vertexIndex & 3u;
#endif
    uint sideIndex = quadIndex / countSideQuads,
         sideQuadIndex = quadIndex - (sideIndex * countSideQuads);
    sideIndex %= 6u;
    vec2 uv = vec2(uvec2(sideQuadIndex % countQuadPointsInOneDirection,
                        sideQuadIndex / countQuadPointsInOneDirection) + 
                   offsets[3u - quadVertexIndex]) / vec2(countQuadPointsInOneDirection);
    mat3 normalMatrix = sideMatrices[sideIndex];       
    vec3 normal = getNormal(normalMatrix, uv),
         position = (pushConstants.modelMatrix * vec4(normal * pushConstants.bottomRadius, 1.0)).xyz;
#if 0
    vec3 tangent = getNormal(normalMatrix * tangentTransformMatrix, uv),
         bitangent = getNormal(normalMatrix * bitangentTransformMatrix, uv);
    tangent = normalize(tangent - (dot(tangent, normal) * normal));
    bitangent = normalize(bitangent - (dot(bitangent, normal) * normal));
    tangent = cross(bitangent, normal);
    bitangent = cross(normal, tangent);
#endif
    outBlock.position = position;    
    outBlock.normal = normal;
    outBlock.planetCenterToCamera = inverseViewMatrix[3].xyz - (pushConstants.modelMatrix * vec2(0.0, 1.0).xxxy).xyz; 
  }else{
    outBlock.position = outBlock.normal = vec3(0.0);
  }  
#endif
}