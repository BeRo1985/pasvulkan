#ifndef GLOBAL_DESCRIPTOR_SET_GLSL
#define GLOBAL_DESCRIPTOR_SET_GLSL 

//#ifdef MESHS
#if !defined(USE_MATERIAL_BUFFER_REFERENCE)
struct Material {
  vec4 baseColorFactor;
  vec4 specularFactor;
  vec4 emissiveFactor;
  vec4 metallicRoughnessNormalScaleOcclusionStrengthFactor;
  vec4 sheenColorFactorSheenRoughnessFactor;
  vec4 clearcoatFactorClearcoatRoughnessFactor;
  vec4 iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum;
  vec4 iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance;
  uvec4 volumeAttenuationColorAnisotropyStrengthAnisotropyRotation;
  uvec4 dispersionUnused; // x = dispersion, y, z, w = unused
  uvec4 alphaCutOffFlagsTex0Tex1;
  int textures[20];
  mat3x2 textureTransforms[20];
};
#endif

layout(set = 0, binding = 0, std430) readonly buffer InstanceMatrices {
  mat4 instanceMatrices[];
};
//#endif // MESHS

#ifdef LIGHTS
struct Light {
  uvec4 metaData;
  vec4 colorIntensity;
  vec4 positionRange;
  vec4 directionZFar;
  mat4 shadowMapMatrix;
};

layout(set = 0, binding = 1, std430) readonly buffer LightItemData {
//uvec4 lightMetaData;
  Light lights[];
};

struct LightTreeNode {
  uvec4 aabbMinSkipCount;
  uvec4 aabbMaxUserData;
};

layout(set = 0, binding = 2, std430) readonly buffer LightTreeNodeData {
  LightTreeNode lightTreeNodes[];
};

#endif // LIGHTS

//#ifdef MESHS
#if defined(USE_MATERIAL_BUFFER_REFERENCE)

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer Material {
  vec4 baseColorFactor;
  vec4 specularFactor;
  vec4 emissiveFactor;
  vec4 metallicRoughnessNormalScaleOcclusionStrengthFactor;
  vec4 sheenColorFactorSheenRoughnessFactor;
  vec4 clearcoatFactorClearcoatRoughnessFactor;
  vec4 iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum;
  vec4 iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance;
  uvec4 volumeAttenuationColorAnisotropyStrengthAnisotropyRotation;
  uvec4 dispersionUnused; // x = dispersion, y, z, w = unused
  uvec4 alphaCutOffFlagsTex0Tex1;
  int textures[20];
  mat3x2 textureTransforms[20];
};

layout(set = 0, binding = 3, std140) uniform Materials {
  Material materials;
} uMaterials;

#else

layout(set = 0, binding = 3, std430) readonly buffer Materials {
  Material materials[];
};
#endif // defined(USE_MATERIAL_BUFFER_REFERENCE)
//#endif // MESHS

#ifdef RAYTRACING

#if 0
struct RaytracingPlanetBufRefMaterial {
  uint albedo;
  uint normalHeight;
  uint occlusionRoughnessMetallic;
  float scale;
}; 
#define GetRaytracingPlanetMaterialAlbedoTextureIndex(m) (m).albedo
#define GetRaytracingPlanetMaterialNormalHeightTextureIndex(m) (m).normalHeight
#define GetRaytracingPlanetMaterialOcclusionRoughnessMetallicTextureIndex(m) (m).occlusionRoughnessMetallic
#define GetRaytracingPlanetMaterialScale(m) (m).scale
#else
#define RaytracingPlanetBufRefMaterial uvec4  // x = albedo, y = normalHeight, z = occlusionRoughnessMetallic, w = scale (float)
#define GetRaytracingPlanetBufRefMaterialAlbedoTextureIndex(m) (m).x
#define GetRaytracingPlanetBufRefMaterialNormalHeightTextureIndex(m) (m).y
#define GetRaytracingPlanetBufRefMaterialOcclusionRoughnessMetallicTextureIndex(m) (m).z
#define GetRaytracingPlanetBufRefMaterialScale(m) (uintBitsToFloat((m).w))
#endif

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingPlanetBufRefData {

  mat4 modelMatrix;

  mat4 normalMatrix; // normalMatrix = mat4(transpose(inverse(mat3(modelMatrix)))) for to save computation in the shader, and mat4 instead of mat3 for alignment/padding rules of std430

  vec4 bottomRadiusTopRadiusHeightMapScale; // x = bottomRadius, y = topRadius, z = heightMapScale, w = unused

  uvec4 flagsResolutions; // x = flags, y = resolution (2x 16-bit: tile map resolution, tile resolution), z = unused, w = unused

  vec4 selected; // xyz = octahedral map coordinates, w = radius   

  RaytracingPlanetBufRefMaterial materials[16];

};

layout(buffer_reference, std430, buffer_reference_align = 8) readonly buffer RaytracingPlanetBufRefDataArray {
  RaytracingPlanetBufRefData planetBufRefData[];
};

layout(buffer_reference, std430, buffer_reference_align = 4) readonly buffer RaytracingGeometryInstanceOffsets {
  uint geometryInstanceOffsets[];
};

struct RaytracingGeometryItem {
  uint type;
  uint objectIndex;
  uint materialIndex;
  uint indexOffset;
};

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingGeometryItems {
  RaytracingGeometryItem geometryItems[];
};

struct RaytracingMeshStaticVertex {
  vec4 texCoords; // xy = texCoord A0, zw = texCoord 1
  uvec4 color0MaterialID; // xy = color (half float RGBA), z = material ID, w = unused
};

struct RaytracingMeshDynamicVertex {
  uvec4 positionNormalXY; // xyz = position (32-bit float), w = normal x y (16-bit signed normalized)
  uvec4 normalZSignTangentXYZModelScaleXYZ; // x = normal z + sign of tangent z (16-bit signed normalized), y = tangent x y (16-bit signed normalized), z = tangent z + model scale x (16-bit float), w = model scale y z (16-bit float)
};

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingMeshStaticVertices {
  RaytracingMeshStaticVertex meshStaticVertices[];
};

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingMeshDynamicVertices {
  RaytracingMeshDynamicVertex meshDynamicVertices[];
};

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingMeshIndices {
  uint meshIndices[];
};

struct RaytracingParticleVertex {
  vec4 positionRotation; // xyz = position (32-bit float), w = rotation (32-bit float)
  uvec4 quadCoordTextureID; // x = quadCoord (half float XY), y = textureID (32-bit unsigned int), zw = size XY (32-bit floats)
  uvec4 colorUnused; // xy = color (half float RGBA), zw = unused 
}; // 48 bytes per vertex

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingParticleVertices {
  RaytracingParticleVertex particleVertices[];
};

#define RaytracingPlanetVertex uvec4 // xyz = position (32-bit float), w = octahedral normal (2x signed normalized 16-bit)

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingPlanetVertices {
  RaytracingPlanetVertex planetVertices[];
};

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer RaytracingPlanetVerticesArray {
  RaytracingPlanetVertices planetVertices[];
};

layout(set = 0, binding = 4) uniform accelerationStructureEXT uRaytracingTopLevelAccelerationStructure;

layout(set = 0, binding = 5) uniform buffer RaytracingData {
  RaytracingGeometryInstanceOffsets geometryInstanceOffsets;
  RaytracingGeometryItems geometryItems;
  RaytracingMeshStaticVertices meshStaticVertices;
  RaytracingMeshDynamicVertices meshDynamicVertices;
  RaytracingMeshIndices meshIndices;  
  RaytracingParticleVertices particleVertices;
  RaytracingPlanetBufRefDataArray planetBufRefDataArray;
  RaytracingPlanetVerticesArray planetVerticesArray;
} uRaytracingData;

layout(set = 0, binding = 6) uniform sampler2D u2DTextures[];

layout(set = 0, binding = 6) uniform samplerCube uCubeTextures[];

#else

layout(set = 0, binding = 4) uniform sampler2D u2DTextures[];

layout(set = 0, binding = 4) uniform samplerCube uCubeTextures[];

#endif // RAYTRACING

#endif