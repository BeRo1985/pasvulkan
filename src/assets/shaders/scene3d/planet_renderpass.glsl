#ifndef PLANET_RENDERPASS_GLSL
#define PLANET_RENDERPASS_GLSL

#if 0
struct Material {
  uint albedo;
  uint normalHeight;
  uint occlusionRoughnessMetallic;
  float scale;
}; 
#define GetMaterialAlbedoTextureIndex(m) (m).albedo
#define GetMaterialNormalHeightTextureIndex(m) (m).normalHeight
#define GetMaterialOcclusionRoughnessMetallicTextureIndex(m) (m).occlusionRoughnessMetallic
#define GetMaterialScale(m) (m).scale
#else
#define Material uvec4  // x = albedo, y = normalHeight, z = occlusionRoughnessMetallic, w = scale (float)
#define GetMaterialAlbedoTextureIndex(m) (m).x
#define GetMaterialNormalHeightTextureIndex(m) (m).y
#define GetMaterialOcclusionRoughnessMetallicTextureIndex(m) (m).z
#define GetMaterialScale(m) (uintBitsToFloat((m).w))
#endif

layout(set = 2, binding = 1, std430) readonly buffer PlanetData {

  mat4 modelMatrix;

  mat4 normalMatrix; // normalMatrix = mat4(transpose(inverse(mat3(modelMatrix)))) for to save computation in the shader, and mat4 instead of mat3 for alignment/padding rules of std430

  vec4 bottomRadiusTopRadiusHeightMapScale; // x = bottomRadius, y = topRadius, z = heightMapScale, w = unused

  uvec4 flagsResolutions; // x = flags, y = resolution (2x 16-bit: tile map resolution, tile resolution), z = unused, w = unused

  vec4 selected; // xyz = octahedral map coordinates, w = radius   

  Material materials[16];

} planetData;

layout(push_constant) uniform PushConstants {

  uint viewBaseIndex;
  uint countViews;
  uint countQuadPointsInOneDirection; 
  uint countAllViews;
  
  uint resolutionXY;  
  float tessellationFactor; // = factor / referenceMinEdgeSize, for to avoid at least one division in the shader 
  vec2 jitter;

} pushConstants;

#endif
