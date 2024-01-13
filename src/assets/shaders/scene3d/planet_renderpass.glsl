#ifndef PLANET_RENDERPASS_GLSL
#define PLANET_RENDERPASS_GLSL

#if 0
struct Material {
  uint albedo;
  uint normalHeight;
  uint occlusionRoughnessMetallic;
  uint reserved; // for alignment
}; 
#define GetMaterialAlbedoTexture(m) (m).albedo
#define GetMaterialNormalHeightTexture(m) (m).normalHeight
#define GetMaterialOcclusionRoughnessMetallicTexture(m) (m).occlusionRoughnessMetallic
#else
#define Material uvec4  // x = albedo, y = normalHeight, z = occlusionRoughnessMetallic, w = reserved
#define GetMaterialAlbedoTexture(m) (m).x
#define GetMaterialNormalHeightTexture(m) (m).y
#define GetMaterialOcclusionRoughnessMetallicTexture(m) (m).z
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
