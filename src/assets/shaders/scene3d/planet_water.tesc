#version 450 core

#pragma shader_stage(tesscontrol)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_control_flow_attributes : enable
#extension GL_GOOGLE_include_directive : enable



#ifndef BUFFERREFERENCE_DEFINITIONS_GLSL
#define BUFFERREFERENCE_DEFINITIONS_GLSL

#if defined(RAYTRACING)
  #define USE_BUFFER_REFERENCE // Explicitly enable this, ray tracing needs buffer reference support anyway.
  #define USE_MATERIAL_BUFFER_REFERENCE // Explicitly enable this, ray tracing needs buffer reference support anyway. 
  #define USE_INT64 // Enable 64-bit integers for on GPU with hardware ray tracing for simplicity. Therefore, GPUs with hardware ray tracing that do not support 64-bit integers are plain and simple not supported for the sake of simplicity. 
#endif

#if defined(USE_MATERIAL_BUFFER_REFERENCE) 
 #define USE_BUFFER_REFERENCE
#endif

#if defined(USE_BUFFER_REFERENCE) 
  #extension GL_EXT_shader_explicit_arithmetic_types_int64 : enable 
  //#extension GL_EXT_buffer_reference : enable 
  #extension GL_EXT_buffer_reference2 : enable 
  #ifdef USE_INT64
    //#extension GL_ARB_gpu_shader_int64 : enable
  #else
    #extension GL_EXT_buffer_reference_uvec2 : enable 
  #endif
  #define sizeof(Type) (uint64_t(Type(uint64_t(0))+1))
#endif

#endif // BUFFERREFERENCE_DEFINITIONS_GLSL


#ifdef TRIANGLES
layout(vertices = 3) out;
#else
layout(vertices = 4) out;
#endif

layout(location = 0) in InBlock {
  vec3 position;
  vec3 normal;
  vec3 planetCenterToCamera;
  uint flags;
} inBlocks[];

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 normal;
  uint flags;
} outBlocks[];

// Global descriptor set

#define PLANETS


#ifndef GLOBAL_DESCRIPTOR_SET_GLSL
#define GLOBAL_DESCRIPTOR_SET_GLSL 

#ifdef RAYTRACING
  #extension GL_EXT_ray_tracing : enable
  #extension GL_EXT_ray_query : enable
  #extension GL_EXT_ray_flags_primitive_culling : enable
#endif

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

#if defined(USE_BUFFER_REFERENCE) 
#if 0
struct PlanetMaterial {
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
#define PlanetMaterial uvec4  // x = albedo, y = normalHeight, z = occlusionRoughnessMetallic, w = scale (float)
#define GetPlanetMaterialAlbedoTextureIndex(m) (m).x
#define GetPlanetMaterialNormalHeightTextureIndex(m) (m).y
#define GetPlanetMaterialOcclusionRoughnessMetallicTextureIndex(m) (m).z
#define GetPlanetMaterialScale(m) (uintBitsToFloat((m).w))
#endif

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer PlanetData {

  mat4 modelMatrix;

  mat4 normalMatrix; // normalMatrix = mat4(transpose(inverse(mat3(modelMatrix)))) for to save computation in the shader, and mat4 instead of mat3 for alignment/padding rules of std430

  vec4 bottomRadiusTopRadiusHeightMapScale; // x = bottomRadius, y = topRadius, z = heightMapScale, w = unused

  uvec4 flagsResolutions; // x = flags, y = resolution (2x 16-bit: tile map resolution, tile resolution)

  uvec4 verticesIndices; // xy = vertices device address, zw = indices device address

  vec4 selected; // xyz = octahedral map coordinates, w = radius   

  PlanetMaterial materials[16];

};
#endif

#ifdef RAYTRACING

layout(buffer_reference, std430, buffer_reference_align = 8) readonly buffer ReferencedPlanetDataArray {
  PlanetData planetData[];
};

layout(buffer_reference, std430, buffer_reference_align = 4) readonly buffer RaytracingGeometryInstanceOffsets {
  uint geometryInstanceOffsets[];
};

struct RaytracingGeometryItem {
  uint objectType;
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

layout(buffer_reference, std430, buffer_reference_align = 4) readonly buffer RaytracingPlanetIndices {
  uint planetIndices[];
};

layout(set = 0, binding = 4) uniform accelerationStructureEXT uRaytracingTopLevelAccelerationStructure;

layout(set = 0, std140, binding = 5) uniform RaytracingData {
  RaytracingGeometryInstanceOffsets geometryInstanceOffsets;
  RaytracingGeometryItems geometryItems;
  RaytracingMeshStaticVertices meshStaticVertices;
  RaytracingMeshDynamicVertices meshDynamicVertices;
  RaytracingMeshIndices meshIndices;  
  RaytracingParticleVertices particleVertices;
  ReferencedPlanetDataArray referencedPlanetDataArray;
} uRaytracingData;

layout(set = 0, binding = 6) uniform sampler2D u2DTextures[];

layout(set = 0, binding = 6) uniform samplerCube uCubeTextures[];

#else

layout(set = 0, binding = 4) uniform sampler2D u2DTextures[];

layout(set = 0, binding = 4) uniform samplerCube uCubeTextures[];

#endif // RAYTRACING

#endif

#undef PLANETS

#define PLANET_WATER


#ifndef PLANET_RENDERPASS_GLSL
#define PLANET_RENDERPASS_GLSL

#if !defined(USE_BUFFER_REFERENCE) 
#if 0
struct PlanetMaterial {
  uint albedo;
  uint normalHeight;
  uint occlusionRoughnessMetallic;
  float scale;
}; 
#define GetPlanetMaterialAlbedoTextureIndex(m) (m).albedo
#define GetPlanetMaterialNormalHeightTextureIndex(m) (m).normalHeight
#define GetPlanetMaterialOcclusionRoughnessMetallicTextureIndex(m) (m).occlusionRoughnessMetallic
#define GetPlanetMaterialScale(m) (m).scale
#else
#define PlanetMaterial uvec4  // x = albedo, y = normalHeight, z = occlusionRoughnessMetallic, w = scale (float)
#define GetPlanetMaterialAlbedoTextureIndex(m) (m).x
#define GetPlanetMaterialNormalHeightTextureIndex(m) (m).y
#define GetPlanetMaterialOcclusionRoughnessMetallicTextureIndex(m) (m).z
#define GetPlanetMaterialScale(m) (uintBitsToFloat((m).w))
#endif

layout(set = 2, binding = 1, std430) readonly buffer PlanetData {

  mat4 modelMatrix;

  mat4 normalMatrix; // normalMatrix = mat4(transpose(inverse(mat3(modelMatrix)))) for to save computation in the shader, and mat4 instead of mat3 for alignment/padding rules of std430

  vec4 bottomRadiusTopRadiusHeightMapScale; // x = bottomRadius, y = topRadius, z = heightMapScale, w = unused

  uvec4 flagsResolutions; // x = flags, y = resolution (2x 16-bit: tile map resolution, tile resolution)

  uvec4 verticesIndices; // xy = vertices device address, zw = indices device address

  vec4 selected; // xyz = octahedral map coordinates, w = radius   

  PlanetMaterial materials[16];

} planetData;
#endif

#if defined(PLANET_WATER)
layout(push_constant) uniform PushConstants {

  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint countQuadPointsInOneDirection; 
  
  uint resolutionXY;  
  float tessellationFactor; // = factor / referenceMinEdgeSize, for to avoid at least one division in the shader 
  vec2 jitter;

  int frameIndex; 
  float time;
#if defined(USE_BUFFER_REFERENCE) 
  PlanetData planetData;
#else
  uvec2 unusedPlanetData; // Ignored in this case  
#endif

  uint tileMapResolution;

} pushConstants;

#else
layout(push_constant) uniform PushConstants {

  uint viewBaseIndex;
  uint countViews;
  uint countQuadPointsInOneDirection; 
  uint countAllViews;
  
  uint resolutionXY;  
  float tessellationFactor; // = factor / referenceMinEdgeSize, for to avoid at least one division in the shader 
  vec2 jitter;

  int frameIndex; 
  int reversed;
#if defined(USE_BUFFER_REFERENCE) 
  PlanetData planetData;
#else
  uvec2 unusedPlanetData; // Ignored in this case  
#endif

} pushConstants;
#endif // defined(PLANET_WATER)

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined(USE_BUFFER_REFERENCE) 
PlanetData planetData = pushConstants.planetData; // For to avoid changing the code below
#endif

#if !defined(PLANET_WATER)

PlanetMaterial layerMaterials[4];
vec4 layerMaterialWeights;

void layerMaterialSetup(vec3 sphereNormal){

  layerMaterials[0] = planetData.materials[0];
  layerMaterials[1] = planetData.materials[1];
  layerMaterials[2] = planetData.materials[2];
  layerMaterials[3] = planetData.materials[3];
   
  layerMaterialWeights = vec4(1.0, 0.0, 0.0, 0.0);

}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

float textureHash11(uint q){
	uvec2 n = q * uvec2(1597334673U, 3812015801U);
	q = (n.x ^ n.y) * 1597334673U;
  return ((uintBitsToFloat(uint(uint(((q >> 9u) & uint(0x007fffffu)) | uint(0x3f800000u))))) - 1.0);
}

float textureHash11(float p){
	uvec2 n = uint(int(p)) * uvec2(1597334673U, 3812015801U);
	uint q = (n.x ^ n.y) * 1597334673U;
  return ((uintBitsToFloat(uint(uint(((q >> 9u) & uint(0x007fffffu)) | uint(0x3f800000u))))) - 1.0);
}

float textureHash12(uvec2 q){
	q *= uvec2(1597334673U, 3812015801U);
	uint n = (q.x ^ q.y) * 1597334673U;
  return ((uintBitsToFloat(uint(uint(((n >> 9u) & uint(0x007fffffu)) | uint(0x3f800000u))))) - 1.0);
}

float textureHash12(vec2 p){
	uvec2 q = uvec2(ivec2(p)) * uvec2(1597334673U, 3812015801U);
	uint n = (q.x ^ q.y) * 1597334673U;
  return ((uintBitsToFloat(uint(uint(((n >> 9u) & uint(0x007fffffu)) | uint(0x3f800000u))))) - 1.0);
}

vec2 textureHash22(uvec2 q){
  q *= uvec2(1597334673U, 3812015801U);
  q = (q.x ^ q.y) * uvec2(1597334673U, 3812015801U);
  return vec2(vec2(uintBitsToFloat(uvec2(uvec2(((q >> 9u) & uvec2(0x007fffffu)) | uvec2(0x3f800000u))))) - vec2(1.0));
}

vec2 textureHash22(vec2 p){
  uvec2 q = uvec2(ivec2(p)) * uvec2(1597334673U, 3812015801U);
  q = (q.x ^ q.y) * uvec2(1597334673U, 3812015801U);
  return vec2(vec2(uintBitsToFloat(uvec2(uvec2(((q >> 9u) & uvec2(0x007fffffu)) | uvec2(0x3f800000u))))) - vec2(1.0));
}

float textureNoise11(float p){
  float f = fract(p);
  p -= f;
  f = (f * f) * (3.0 - (2.0 * f));
  return mix(textureHash11(p + 0.0), textureHash11(p + 1.0), f); 
}

float textureNoise12(vec2 p){
  vec2 f = fract(p);
  p -= f;
  f = (f * f) * (3.0 - (2.0 * f));
  vec2 n = vec2(0.0, 1.0);
  return mix(mix(textureHash12(p + n.xx), textureHash12(p + n.yx), f.x),
             mix(textureHash12(p + n.xy), textureHash12(p + n.yy), f.x), f.y);
}

vec2 textureNoise22(vec2 p){
  vec2 f = fract(p);
  p -= f;
  f = (f * f) * (3.0 - (2.0 * f));
  vec2 n = vec2(0.0, 1.0);
  return mix(mix(textureHash22(p + n.xx), textureHash22(p + n.yx), f.x),
             mix(textureHash22(p + n.xy), textureHash22(p + n.yy), f.x), f.y);
  
}

vec4 textureNoTile(const in sampler2D tex, in vec2 uv, const in vec2 duvdx, const in vec2 duvdy){
#if 0
  return textureGrad(tex, uv, duvdx, duvdy);
#else

  // sample variation pattern   
  float k = clamp(textureNoise12(uv), 0.0, 1.0); // low-frequency noise lookup per hash function
    
  // compute index for 8 variation patterns in total  
  float l = k * 8.0;
  float ia = floor(l);
  float f = l - ia;
  float ib = ia + 1.0;
    
  // offsets for the different virtual patterns      
#if 1
  vec2 offa = fma(textureNoise22(vec2(13.0, 17.0) * ia), vec2(2.0), vec2(-1.0));
  vec2 offb = fma(textureNoise22(vec2(13.0, 17.0) * ib), vec2(2.0), vec2(-1.0));
#else 
  vec2 offa = sin(vec2(3.0, 7.0) * ia); // can replace with any other hash
  vec2 offb = sin(vec2(3.0, 7.0) * ib); // can replace with any other hash 
#endif

  // sample the two closest virtual patterns   
  vec4 cola = textureGrad(tex, uv + offa, duvdx, duvdy);
  vec4 colb = textureGrad(tex, uv + offb, duvdx, duvdy);
    
  // interpolate between the two virtual patterns  
  return mix(cola, colb, smoothstep(0.2, 0.8, f - (0.1 * dot(cola - colb, vec4(1.0)))));
#endif
}

//#define TRIPLANAR

vec3 multiplanarP;
vec3 multiplanarDX;
vec3 multiplanarDY;

const float multiplanarK = 6.0;

#ifdef TRIPLANAR
// Triplanar
vec3 multiplanarM;
#else
// Biplanar
ivec3 multiplanarMA;
ivec3 multiplanarMI;
ivec3 multiplanarME;
vec2 multiplanarM;
#endif

void multiplanarSetup(vec3 position, vec3 positionDX, vec3 positionDY, vec3 normal){

  multiplanarP = position;

  multiplanarDX = positionDX;
  multiplanarDY = positionDY;

  normal = normalize(normal);

#ifdef TRIPLANAR

  multiplanarM = pow(abs(normal), vec3(multiplanarK));
  multiplanarM /= (multiplanarM.x + multiplanarM.y + multiplanarM.z);

#else

  vec3 absNormal = abs(normal);

  multiplanarMA = ((absNormal.x > absNormal.y) && (absNormal.x > absNormal.z)) ? ivec3(0, 1, 2) : ((absNormal.y > absNormal.z) ? ivec3(1, 2, 0) : ivec3(2, 0, 1));    
  multiplanarMI = ((absNormal.x < absNormal.y) && (absNormal.x < absNormal.z)) ? ivec3(0, 1, 2) : ((absNormal.y < absNormal.z) ? ivec3(1, 2, 0) : ivec3(2, 0, 1));
  multiplanarME = ivec3(3) - (multiplanarMI + multiplanarMA);
  multiplanarM = pow(clamp((vec2(absNormal[multiplanarMA.x], absNormal[multiplanarME.x]) - vec2(0.5773)) / vec2(1.0 - 0.5773), vec2(0.0), vec2(1.0)), vec2(multiplanarK * (1.0 / 8.0)));
  multiplanarM /= (multiplanarM.x + multiplanarM.y);

#endif
 
}

vec4 multiplanarTexture(const in sampler2D tex, float scale){
#ifdef TRIPLANAR
  return (textureNoTile(tex, multiplanarP.yz * scale, multiplanarDX.yz * scale, multiplanarDY.yz * scale) * multiplanarM.x) +
         (textureNoTile(tex, multiplanarP.zx * scale, multiplanarDX.zx * scale, multiplanarDY.zx * scale) * multiplanarM.y) + 
         (textureNoTile(tex, multiplanarP.xy * scale, multiplanarDX.xy * scale, multiplanarDY.xy * scale) * multiplanarM.z);
#else
 return (textureNoTile(
            tex, 
            vec2(multiplanarP[multiplanarMA.y], multiplanarP[multiplanarMA.z]) * scale,
            vec2(multiplanarDX[multiplanarMA.y], multiplanarDX[multiplanarMA.z]) * scale,
            vec2(multiplanarDY[multiplanarMA.y], multiplanarDY[multiplanarMA.z]) * scale
          ) * multiplanarM.x
        ) +
        (textureNoTile(
           tex, 
           vec2(multiplanarP[multiplanarME.y], multiplanarP[multiplanarME.z]) * scale,
           vec2(multiplanarDX[multiplanarME.y], multiplanarDX[multiplanarME.z]) * scale,
           vec2(multiplanarDY[multiplanarME.y], multiplanarDY[multiplanarME.z]) * scale
          ) * multiplanarM.y
        );
#endif
}

vec3 getLayeredMultiplanarAlbedo(){
  vec4 albedoWeightSum = vec4(0.0);
  [[unroll]] for(int layerIndex = 0; layerIndex < 4; layerIndex++){
    if(layerMaterialWeights[layerIndex] > 0.0){
      albedoWeightSum += vec4(multiplanarTexture(u2DTextures[(GetPlanetMaterialAlbedoTextureIndex(layerMaterials[layerIndex]) << 1) | 1], GetPlanetMaterialScale(layerMaterials[layerIndex])).xyz, 1.0) * layerMaterialWeights[layerIndex];
    }
  }
  return albedoWeightSum.xyz / max(1e-7, albedoWeightSum.w);
}

vec3 getLayeredMultiplanarNormal(){
  vec4 normalWeightSum = vec4(0.0);
  [[unroll]] for(int layerIndex = 0; layerIndex < 4; layerIndex++){
    if(layerMaterialWeights[layerIndex] > 0.0){
      normalWeightSum += vec4(multiplanarTexture(u2DTextures[(GetPlanetMaterialNormalHeightTextureIndex(layerMaterials[layerIndex]) << 1) | 0], GetPlanetMaterialScale(layerMaterials[layerIndex])).xyz, 1.0) * layerMaterialWeights[layerIndex];
    }
  }
  return normalWeightSum.xyz / max(1e-7, normalWeightSum.w);
}

float getLayeredMultiplanarHeight(){
  vec2 heightWeightSum = vec2(0.0);
  [[unroll]] for(int layerIndex = 0; layerIndex < 4; layerIndex++){
    if(layerMaterialWeights[layerIndex] > 0.0){
      heightWeightSum += vec2(multiplanarTexture(u2DTextures[(GetPlanetMaterialNormalHeightTextureIndex(layerMaterials[layerIndex]) << 1) | 0], GetPlanetMaterialScale(layerMaterials[layerIndex])).w, 1.0) * layerMaterialWeights[layerIndex];
    }
  }
  return heightWeightSum.x / max(1e-7, heightWeightSum.y);
}

vec3 getLayeredMultiplanarOcclusionRoughnessMetallic(){
  vec4 occlusionRoughnessMetallicWeightSum = vec4(0.0);
  [[unroll]] for(int layerIndex = 0; layerIndex < 4; layerIndex++){
    if(layerMaterialWeights[layerIndex] > 0.0){
      occlusionRoughnessMetallicWeightSum += vec4(multiplanarTexture(u2DTextures[(GetPlanetMaterialOcclusionRoughnessMetallicTextureIndex(layerMaterials[layerIndex]) << 1) | 0], GetPlanetMaterialScale(layerMaterials[layerIndex])).xyz, 1.0) * layerMaterialWeights[layerIndex];
    }
  }
  return occlusionRoughnessMetallicWeightSum.xyz / max(1e-7, occlusionRoughnessMetallicWeightSum.w);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif // !defined(PLANET_WATER)

#endif


  
vec2 resolution = vec2(float(uint(pushConstants.resolutionXY & 0xffffu)), float(uint(pushConstants.resolutionXY >> 16u)));

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256];
} uView;
 
uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
mat4 viewMatrix = uView.views[viewIndex].viewMatrix;
mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;
// mat4 inverseProjectionMatrix = uView.views[viewIndex].inverseProjectionMatrix;

float remap(const in float value,
            const in float oldMin,
            const in float oldMax,
            const in float newMin,
            const in float newMax){
  return (((value - oldMin) / (oldMax - oldMin)) * (newMax - newMin)) + newMin; 
}

float AdaptiveTessellation(vec3 p0, vec3 p1){
  vec4 vc = viewMatrix * vec4(mix(p0, p1, 0.5), 1.0),
       vr = vec2(distance(p0, p1) * 0.5, 0.0).xxyy,
       v0 = projectionMatrix * (vc - vr),
       v1 = projectionMatrix * (vc + vr),
       v = fma(vec4(v0.xy / v0.w, v1.xy / v1.w), vec4(0.5), vec4(0.5)) * resolution.xyxy;
 	return clamp(distance(v.xy, v.zw) * pushConstants.tessellationFactor, 1.0, 64.0);
}
void main(){	 
  const uint anyFlags = inBlocks[0].flags | 
                        inBlocks[1].flags | 
                        inBlocks[2].flags |
#ifndef TRIANGLES
                        inBlocks[3].flags |             
#endif
                        0u;
  const uint maskedFlags = inBlocks[0].flags & 
                           inBlocks[1].flags & 
                           inBlocks[2].flags &
#ifndef TRIANGLES
                           inBlocks[3].flags &
#endif
                           0xffffffffu; 
  const bool underWater = (maskedFlags & (1u << 0u)) != 0u;
  const bool isVisible = (anyFlags & (1u << 1u)) != 0u;
  const bool isWaterVisible = (anyFlags & (1u << 2u)) != 0u;
  const bool aboveGround = true;//(anyFlags & (1u << 3u)) != 0u; // Not used, since the vertex shader has a too roughly map processing resolution to determine this correctly in every case.   
  bool visible = isVisible && isWaterVisible && aboveGround && !underWater;
  if(visible){
#ifdef TRIANGLES
    vec3 faceNormal = normalize(inBlocks[0].normal + inBlocks[1].normal + inBlocks[2].normal);
#else
    vec3 faceNormal = normalize(inBlocks[0].normal + inBlocks[1].normal + inBlocks[2].normal + inBlocks[3].normal);
#endif
    vec3 planetCenterToCamera = (inBlocks[0].planetCenterToCamera + 
                                 inBlocks[1].planetCenterToCamera + 
                                 inBlocks[2].planetCenterToCamera + 
                                 inBlocks[3].planetCenterToCamera) * 0.25;
    vec3 planetCenterToCameraDirection = normalize(planetCenterToCamera);
    if(dot(faceNormal, planetCenterToCameraDirection) < 0.0){
      // Because the planet is a sphere, the face is visible if the angle between the face normal and the vector from the planet center to the 
      // camera is less than 90 degrees, and invisible otherwise.
      visible = false;
    }    
    if(visible){
#ifdef TRIANGLES
  #define COUNT_VERTICES 3
#else
  #define COUNT_VERTICES 4
#endif
      vec4 vertices[COUNT_VERTICES] = vec4[COUNT_VERTICES](
        viewMatrix * vec4(inBlocks[0].position, 1.0),
        viewMatrix * vec4(inBlocks[1].position, 1.0),
#ifdef TRIANGLES
        viewMatrix * vec4(inBlocks[2].position, 1.0)
#else
        viewMatrix * vec4(inBlocks[2].position, 1.0),
        viewMatrix * vec4(inBlocks[3].position, 1.0)
#endif
      );
      int countVerticesInFrontOfNearPlane = 0; 
      [[unroll]] for(int i = 0; i < COUNT_VERTICES; i++){
        if(vertices[i].w < 0.0){
          countVerticesInFrontOfNearPlane++;
          vertices[i].w *= -1.0;
        }
      } 
      if(countVerticesInFrontOfNearPlane == COUNT_VERTICES){
        // All vertices are behind the near plane, so it is invisible.
        visible = false;
      }else{
        // Otherwise, it is visible if all vertices are inside the screen space bounds, as quick&dirty cheap frustum-like culling. 
        [[unroll]] for(int i = 0; i < COUNT_VERTICES; i++){
          vertices[i].xy = (vertices[i].xy / (vertices[i].w * 2.0)) + vec2(0.5);  
        }
#ifdef TRIANGLES
        visible = all(lessThanEqual(min(min(vertices[0].xy, vertices[1].xy), vertices[2].xy), vec2(1.0))) && 
                  all(greaterThanEqual(max(max(vertices[0].xy, vertices[1].xy), vertices[2].xy), vec2(-1.0)));
#else
        visible = all(lessThanEqual(min(min(min(vertices[0].xy, vertices[1].xy), vertices[2].xy), vertices[3].xy), vec2(1.0))) && 
                  all(greaterThanEqual(max(max(max(vertices[0].xy, vertices[1].xy), vertices[2].xy), vertices[3].xy), vec2(-1.0)));
#endif        
      }   
    }
  }
  if(visible){
#ifdef TRIANGLES
    gl_TessLevelOuter[0] = AdaptiveTessellation(inBlocks[1].position, inBlocks[2].position);
    gl_TessLevelOuter[1] = AdaptiveTessellation(inBlocks[2].position, inBlocks[0].position);
    gl_TessLevelOuter[2] = AdaptiveTessellation(inBlocks[0].position, inBlocks[1].position);
    gl_TessLevelInner[0] = mix(gl_TessLevelOuter[0], gl_TessLevelOuter[2], 0.5);
#else
  	gl_TessLevelOuter[0] = AdaptiveTessellation(inBlocks[3].position, inBlocks[0].position);
	  gl_TessLevelOuter[1] = AdaptiveTessellation(inBlocks[0].position, inBlocks[1].position);
	  gl_TessLevelOuter[2] = AdaptiveTessellation(inBlocks[1].position, inBlocks[2].position);
	  gl_TessLevelOuter[3] = AdaptiveTessellation(inBlocks[2].position, inBlocks[3].position);
	  gl_TessLevelInner[0] = mix(gl_TessLevelOuter[0], gl_TessLevelOuter[3], 0.5);
    gl_TessLevelInner[1] = mix(gl_TessLevelOuter[2], gl_TessLevelOuter[1], 0.5);
/*  gl_TessLevelOuter[0] = AdaptiveTessellation(inBlocks[2].position, inBlocks[0].position);
	  gl_TessLevelOuter[1] = AdaptiveTessellation(inBlocks[0].position, inBlocks[1].position);
	  gl_TessLevelOuter[2] = AdaptiveTessellation(inBlocks[1].position, inBlocks[3].position);
	  gl_TessLevelOuter[3] = AdaptiveTessellation(inBlocks[3].position, inBlocks[2].position);
	  gl_TessLevelInner[0] = mix(gl_TessLevelOuter[0], gl_TessLevelOuter[3], 0.5);
    gl_TessLevelInner[1] = mix(gl_TessLevelOuter[2], gl_TessLevelOuter[1], 0.5);*/
#endif
  }else{
#ifdef TRIANGLES
    gl_TessLevelOuter[0] = -1.0;
    gl_TessLevelOuter[1] = -1.0;   
    gl_TessLevelOuter[2] = -1.0;
    gl_TessLevelInner[0] = -1.0;  
#else
	  gl_TessLevelOuter[0] = -1.0;
	  gl_TessLevelOuter[1] = -1.0;   
	  gl_TessLevelOuter[2] = -1.0;
	  gl_TessLevelOuter[3] = -1.0;
	  gl_TessLevelInner[0] = -1.0;
    gl_TessLevelInner[1] = -1.0;
#endif
  }
  outBlocks[gl_InvocationID].position = inBlocks[gl_InvocationID].position;
	outBlocks[gl_InvocationID].normal = inBlocks[gl_InvocationID].normal;
	outBlocks[gl_InvocationID].flags = inBlocks[gl_InvocationID].flags;
}  
