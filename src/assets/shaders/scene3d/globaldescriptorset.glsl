#ifndef GLOBAL_DESCRIPTOR_SET_GLSL
#define GLOBAL_DESCRIPTOR_SET_GLSL 

#ifdef MESHS
#ifdef NOBUFFERREFERENCE
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
#endif // MESHS

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

#ifdef MESHS
#ifdef NOBUFFERREFERENCE

layout(set = 0, binding = 3, std430) readonly buffer Materials {
  Material materials[];
};

#else

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

#endif // NOBUFFERREFERENCE
#endif // MESHS

layout(set = 0, binding = 4) uniform sampler2D u2DTextures[];

layout(set = 0, binding = 4) uniform samplerCube uCubeTextures[];

#endif