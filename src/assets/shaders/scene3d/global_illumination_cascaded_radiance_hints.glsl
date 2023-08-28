#ifndef GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS_GLSL
#define GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS_GLSL

//#define GI_SPECULAR_FAST
#define GI_SPECULAR_MULTIPLE_TAPS

//#define GI_VISUALIZE_CASCADES

#define GI_COMPRESSION 0

#if GI_COMPRESSION == 0  
  // Full RGB spherical harmonics
  #define GI_COUNT_COMPRESSED_SPHERICAL_HARMONICS_COEFS 14
#elif GI_COMPRESSION == 1
  // Y3Co2Cg2 spherical harmonics
  #define GI_COUNT_COMPRESSED_SPHERICAL_HARMONICS_COEFS 9
#elif GI_COMPRESSION == 2
  // Y3Co1Cg1 spherical harmonics
  #define GI_COUNT_COMPRESSED_SPHERICAL_HARMONICS_COEFS 6
#endif

#define GI_CASCADES 4
#define GI_MAX_WIDTH 32
#define GI_MAX_HEIGHT 32
#define GI_MAX_DEPTH 32
#define GI_ONE_SIZE (GI_MAX_WIDTH * GI_MAX_HEIGHT * GI_MAX_DEPTH)
#define GI_SIZE (GI_ONE_SIZE * GI_CASCADES)

#define VPL_NORMAL_BIAS 0.01

#define VPL_EMISSIVE_NORMAL_BIAS 0.0

#define GV_NORMAL_BIAS (-0.01)

#define GI_SECONDARY_OCCLUSION_FACTOR 0.01

#define GI_SECONDARY_BOUNCE_FACTOR 0.01

layout(std140) uniform uboGlobalIlluminationData {
  vec4 globalIlluminationVolumeAABBMin[GI_CASCADES];
  vec4 globalIlluminationVolumeAABBMax[GI_CASCADES];
  vec4 globalIlluminationVolumeAABBScale[GI_CASCADES];
  vec4 globalIlluminationVolumeAABBSnappedCenter[GI_CASCADES];
  vec4 globalIlluminationVolumeAABBCenter[GI_CASCADES];
  vec4 globalIlluminationVolumeAABBFadeStart[GI_CASCADES];
  vec4 globalIlluminationVolumeAABBFadeEnd[GI_CASCADES];
  ivec4 globalIlluminationVolumeAABBDeltas[GI_CASCADES];
  mat4 globalIlluminationVolumeViewProjectionMatrices[GI_CASCADES * 3];
};
 
const ivec3 uGlobalIlluminationVolumeSize = ivec3(GI_MAX_WIDTH, GI_MAX_HEIGHT, GI_MAX_DEPTH);
const ivec3 uGlobalIlluminationCascadedVolumeSize = ivec3(GI_MAX_WIDTH, GI_MAX_HEIGHT, GI_MAX_DEPTH * GI_CASCADES);
const vec3 uGlobalIlluminationVolumeSizeVector = vec3(GI_MAX_WIDTH, GI_MAX_HEIGHT, GI_MAX_DEPTH);
const vec3 uGlobalIlluminationVolumeSizeInvVector = vec3(1.0) / vec3(GI_MAX_WIDTH, GI_MAX_HEIGHT, GI_MAX_DEPTH);
const vec3 uGlobalIlluminationVolumeSizeExVector = vec3(GI_MAX_WIDTH, GI_MAX_HEIGHT, (GI_MAX_DEPTH + 2) * GI_CASCADES);
const vec3 uGlobalIlluminationVolumeSizeExInvVector = vec3(1.0) / uGlobalIlluminationVolumeSizeExVector;

vec3 globalIlluminationHash(vec3 lPosition){
  lPosition = fract(lPosition * vec3(5.3983, 5.4427, 6.9371));
  lPosition += dot(lPosition.yzx, lPosition.xyz  + vec3(21.5351, 14.3137, 15.3219));
	return fract(vec3(lPosition.x * lPosition.z * 95.4337, lPosition.x * lPosition.y * 97.597, lPosition.y * lPosition.z * 93.8365));
}

vec3 globalIlluminationRandom(vec3 lPosition){
  return globalIlluminationHash(lPosition * 37.0);
}

vec3 globalIlluminationVolumeGet3DTexturePosition(vec4 pPosition){
  return clamp(((clamp(pPosition.xyz, vec3(0.0), vec3(1.0)) * uGlobalIlluminationVolumeSizeVector) + vec3(0.0, 0.0, 1.0 + (float(GI_MAX_DEPTH + 2) * pPosition.w))) * uGlobalIlluminationVolumeSizeExInvVector, vec3(0.0), vec3(1.0));
}

int globalIlluminationVolumeGetBaseIndex(const in ivec4 pPosition){
  return clamp((((((pPosition.w * GI_MAX_DEPTH) + pPosition.z) * GI_MAX_HEIGHT) + pPosition.y) * GI_MAX_WIDTH) + pPosition.x, 0, GI_SIZE);
}

int globalIlluminationVolumeGetCellIndex(const in int pCascadeIndex, const in vec3 pPosition){
  vec3 lAABBMin = globalIlluminationVolumeAABBMin[pCascadeIndex].xyz;
  vec3 lAABBMax = globalIlluminationVolumeAABBMax[pCascadeIndex].xyz;
  int lIndex = -1;
  if(all(greaterThanEqual(pPosition, lAABBMin)) && all(lessThanEqual(pPosition, lAABBMax))){
    vec3 lfPosition = clamp((pPosition - lAABBMin) * globalIlluminationVolumeAABBScale[pCascadeIndex].xyz, vec3(0.0), vec3(1.0));
    ivec3 liPosition = clamp(ivec3(floor(lfPosition * uGlobalIlluminationVolumeSizeVector)), ivec3(0), ivec3(uGlobalIlluminationVolumeSize));
    lIndex = clamp((((((pCascadeIndex * GI_MAX_DEPTH) + liPosition.z) * GI_MAX_HEIGHT) + liPosition.y) * GI_MAX_WIDTH) + liPosition.x, 0, GI_SIZE);    
  }
  return lIndex;  
}

void globalIlluminationSphericalHarmonicsEncode(in vec3 pDirection, in vec3 pC, out vec4 pSphericalHarmonics[9]){
  pSphericalHarmonics[0].xyz = 0.282094792 * pC;
  pSphericalHarmonics[1].xyz = ((-0.488602512) * pDirection.y) * pC;
  pSphericalHarmonics[2].xyz = (0.488602512 * pDirection.z) * pC;
  pSphericalHarmonics[3].xyz = ((-0.488602512) * pDirection.x) * pC;
  pSphericalHarmonics[4].xyz = (1.092548431 * (pDirection.x * pDirection.y)) * pC;
  pSphericalHarmonics[5].xyz = ((-1.092548431) * (pDirection.y * pDirection.z)) * pC;
  pSphericalHarmonics[6].xyz = ((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pC;
  pSphericalHarmonics[7].xyz = ((-1.092548431) * (pDirection.x * pDirection.z)) * pC;
  pSphericalHarmonics[8].xyz = (0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pC;
}

void globalIlluminationSphericalHarmonicsEncode(in vec3 pDirection, in float pValue, out float pSphericalHarmonics[9]){
  pSphericalHarmonics[0] = 0.282094792 * pValue;
  pSphericalHarmonics[1] = ((-0.488602512) * pDirection.y) * pValue;
  pSphericalHarmonics[2] = (0.488602512 * pDirection.z) * pValue;
  pSphericalHarmonics[3] = ((-0.488602512) * pDirection.x) * pValue;
  pSphericalHarmonics[4] = (1.092548431 * (pDirection.x * pDirection.y)) * pValue;
  pSphericalHarmonics[5] = ((-1.092548431) * (pDirection.y * pDirection.z)) * pValue;
  pSphericalHarmonics[6] = ((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pValue;
  pSphericalHarmonics[7] = ((-1.092548431) * (pDirection.x * pDirection.z)) * pValue;
  pSphericalHarmonics[8] = (0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pValue;
}

void globalIlluminationSphericalHarmonicsEncodeAndAccumulate(in vec3 pDirection, in vec3 pC, inout vec4 pSphericalHarmonics[9]){
  pSphericalHarmonics[0].xyz += 0.282094792 * pC;
  pSphericalHarmonics[1].xyz += ((-0.488602512) * pDirection.y) * pC;
  pSphericalHarmonics[2].xyz += (0.488602512 * pDirection.z) * pC;
  pSphericalHarmonics[3].xyz += ((-0.488602512) * pDirection.x) * pC;
  pSphericalHarmonics[4].xyz += (1.092548431 * (pDirection.x * pDirection.y)) * pC;
  pSphericalHarmonics[5].xyz += ((-1.092548431) * (pDirection.y * pDirection.z)) * pC;
  pSphericalHarmonics[6].xyz += ((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pC;
  pSphericalHarmonics[7].xyz += ((-1.092548431) * (pDirection.x * pDirection.z)) * pC;
  pSphericalHarmonics[8].xyz += (0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pC;
}

void globalIlluminationSphericalHarmonicsEncodeAndAccumulate(in vec3 pDirection, in float pValue, inout float pSphericalHarmonics[9]){
  pSphericalHarmonics[0] += 0.282094792 * pValue;
  pSphericalHarmonics[1] += ((-0.488602512) * pDirection.y) * pValue;
  pSphericalHarmonics[2] += (0.488602512 * pDirection.z) * pValue;
  pSphericalHarmonics[3] += ((-0.488602512) * pDirection.x) * pValue;
  pSphericalHarmonics[4] += (1.092548431 * (pDirection.x * pDirection.y)) * pValue;
  pSphericalHarmonics[5] += ((-1.092548431) * (pDirection.y * pDirection.z)) * pValue;
  pSphericalHarmonics[6] += ((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pValue;
  pSphericalHarmonics[7] += ((-1.092548431) * (pDirection.x * pDirection.z)) * pValue;
  pSphericalHarmonics[8] += (0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pValue;
}

vec3 globalIlluminationSphericalHarmonicsDecode(in vec3 pDirection, in vec4 pSphericalHarmonics[9]){
  return (0.282094792 * pSphericalHarmonics[0].xyz) +
         (0.488602512 * ((pSphericalHarmonics[2].xyz * pDirection.z) -
                         ((pSphericalHarmonics[1].xyz * pDirection.y) +  
                          (pSphericalHarmonics[3].xyz * pDirection.x)))) +
         (1.092548431 * ((pSphericalHarmonics[4].xyz * (pDirection.x * pDirection.y)) -
                         ((pSphericalHarmonics[5].xyz * (pDirection.y * pDirection.z)) + 
                          (pSphericalHarmonics[7].xyz * (pDirection.x * pDirection.z))))) + 
         (((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pSphericalHarmonics[6].xyz) +
         ((0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8].xyz);
}

float globalIlluminationSphericalHarmonicsDecode(in vec3 pDirection, in float pSphericalHarmonics[9]){
  return (0.282094792 * pSphericalHarmonics[0]) +
         (0.488602512 * ((pSphericalHarmonics[2] * pDirection.z) -
                         ((pSphericalHarmonics[1] * pDirection.y) + 
                          (pSphericalHarmonics[3] * pDirection.x)))) +
         (1.092548431 * ((pSphericalHarmonics[4] * (pDirection.x * pDirection.y)) -
                         ((pSphericalHarmonics[5] * (pDirection.y * pDirection.z)) + 
                          (pSphericalHarmonics[7] * (pDirection.x * pDirection.z))))) + 
         (((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pSphericalHarmonics[6]) +
         ((0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8]);
}

vec3 globalIlluminationSphericalHarmonicsDecodeWithCosineLobe(in vec3 pDirection, in vec4 pSphericalHarmonics[9]){
  return (0.886226925 * pSphericalHarmonics[0].xyz) +
         (1.02332671 * ((pSphericalHarmonics[2].xyz * pDirection.z) -
                        ((pSphericalHarmonics[1].xyz * pDirection.y) + 
                         (pSphericalHarmonics[3].xyz * pDirection.x)))) +
         (0.858086 * ((pSphericalHarmonics[4].xyz * (pDirection.x * pDirection.y)) - 
                      ((pSphericalHarmonics[5].xyz * (pDirection.y * pDirection.z)) + 
                       (pSphericalHarmonics[7].xyz * (pDirection.x * pDirection.z))))) + 
         (((0.743125 * (pDirection.z * pDirection.z)) - 0.247708) * pSphericalHarmonics[6].xyz) +
         ((0.429043 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8].xyz);
}

float globalIlluminationSphericalHarmonicsDecodeWithCosineLobe(in vec3 pDirection, in float pSphericalHarmonics[9]){
  return (0.886226925 * pSphericalHarmonics[0]) +
         (1.02332671 * ((pSphericalHarmonics[2] * pDirection.z) -
                        ((pSphericalHarmonics[1] * pDirection.y) +
                         (pSphericalHarmonics[3] * pDirection.x)))) +
         (0.858086 * ((pSphericalHarmonics[4] * (pDirection.x * pDirection.y)) -
                      ((pSphericalHarmonics[5] * (pDirection.y * pDirection.z)) + 
                       (pSphericalHarmonics[7] * (pDirection.x * pDirection.z))))) + 
         (((0.743125 * (pDirection.z * pDirection.z)) - 0.247708) * pSphericalHarmonics[6]) +
         ((0.429043 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8]);
}

vec3 globalIlluminationConvertRGBToYCoCg(vec3 c){
	return vec3(dot(c, vec3(0.25, 0.5, 0.25)), dot(c, vec3(0.5, 0.0, -0.5)), dot(c, vec3(-0.25, 0.5, -0.25)));
}

vec3 globalIlluminationConvertYCoCgToRGB(vec3 c){
  return vec3(dot(c, vec3(1.0, 1.0, -1.0)), dot(c, vec3(1.0, 0.0, 1.0)), dot(c, vec3(1.0, -1.0, -1.0))); 
}

#if GI_COMPRESSION == 0
  #define globalIlluminationEncodeColor(c) (c) 
  #define globalIlluminationDecodeColor(c) (c) 
#else
  #define globalIlluminationEncodeColor(c) globalIlluminationConvertRGBToYCoCg(c) 
  #define globalIlluminationDecodeColor(c) globalIlluminationConvertYCoCgToRGB(c) 
#endif

void globalIlluminationCompressedSphericalHarmonicsEncode(in vec3 pDirection, in vec3 pC, out vec3 pSphericalHarmonics[9]){
  pSphericalHarmonics[0] = 0.282094792 * pC;
  pSphericalHarmonics[1] = ((-0.488602512) * pDirection.y) * pC;
  pSphericalHarmonics[2] = (0.488602512 * pDirection.z) * pC;
  pSphericalHarmonics[3] = ((-0.488602512) * pDirection.x) * pC;
  pSphericalHarmonics[4] = (1.092548431 * (pDirection.x * pDirection.y)) * pC;
  pSphericalHarmonics[5] = ((-1.092548431) * (pDirection.y * pDirection.z)) * pC;
  pSphericalHarmonics[6] = ((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pC;
  pSphericalHarmonics[7] = ((-1.092548431) * (pDirection.x * pDirection.z)) * pC;
  pSphericalHarmonics[8] = (0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pC;
}

void globalIlluminationCompressedSphericalHarmonicsEncodeAndAccumulate(in vec3 pDirection, in vec3 pC, inout vec3 pSphericalHarmonics[9]){
  pSphericalHarmonics[0] += 0.282094792 * pC;
  pSphericalHarmonics[1] += ((-0.488602512) * pDirection.y) * pC;
  pSphericalHarmonics[2] += (0.488602512 * pDirection.z) * pC;
  pSphericalHarmonics[3] += ((-0.488602512) * pDirection.x) * pC;
  pSphericalHarmonics[4] += (1.092548431 * (pDirection.x * pDirection.y)) * pC;
  pSphericalHarmonics[5] += ((-1.092548431) * (pDirection.y * pDirection.z)) * pC;
  pSphericalHarmonics[6] += ((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pC;
  pSphericalHarmonics[7] += ((-1.092548431) * (pDirection.x * pDirection.z)) * pC;
  pSphericalHarmonics[8] += (0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pC;
}

vec3 globalIlluminationCompressedSphericalHarmonicsDecodeWithCosineLobe(in vec3 pDirection, in vec3 pSphericalHarmonics[9]){
#if GI_COMPRESSION == 0
  return (0.886226925 * pSphericalHarmonics[0]) +
         (1.02332671 * ((pSphericalHarmonics[2] * pDirection.z) - 
                        ((pSphericalHarmonics[1] * pDirection.y) +
                         (pSphericalHarmonics[3] * pDirection.x)))) +
         (0.858086 * ((pSphericalHarmonics[4] * (pDirection.x * pDirection.y)) -
                      ((pSphericalHarmonics[5] * (pDirection.y * pDirection.z)) + 
                       (pSphericalHarmonics[7] * (pDirection.x * pDirection.z))))) + 
         (((0.743125 * (pDirection.z * pDirection.z)) - 0.247708) * pSphericalHarmonics[6]) +
         ((0.429043 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8]);
#elif GI_COMPRESSION == 1
  return (0.886226925 * pSphericalHarmonics[0]) +
         (1.02332671 * ((pSphericalHarmonics[2] * pDirection.z) -
                        ((pSphericalHarmonics[1] * pDirection.y) + 
                         (pSphericalHarmonics[3] * pDirection.x)))) +
         vec3((0.858086 * ((pSphericalHarmonics[4].x * (pDirection.x * pDirection.y)) - 
                           ((pSphericalHarmonics[5].x * (pDirection.y * pDirection.z)) + 
                            (pSphericalHarmonics[7].x * (pDirection.x * pDirection.z))))) + 
              (((0.743125 * (pDirection.z * pDirection.z)) - 0.247708) * pSphericalHarmonics[6].x) +
              ((0.429043 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8].x), vec2(0.0));
#elif GI_COMPRESSION == 2
  return (0.886226925 * pSphericalHarmonics[0]) +
         vec3((1.02332671 * ((pSphericalHarmonics[2].x * pDirection.z) -
                             ((pSphericalHarmonics[1].x * pDirection.y) +
                              (pSphericalHarmonics[3].x * pDirection.x)))) +
              (0.858086 * ((pSphericalHarmonics[4].x * (pDirection.x * pDirection.y)) - 
                           ((pSphericalHarmonics[5].x * (pDirection.y * pDirection.z)) + 
                            (pSphericalHarmonics[7].x * (pDirection.x * pDirection.z))))) + 
              (((0.743125 * (pDirection.z * pDirection.z)) - 0.247708) * pSphericalHarmonics[6].x) +
              ((0.429043 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8].x), vec2(0.0));
#else  
  return vec3(0.0);
#endif 
}

vec3 globalIlluminationCompressedSphericalHarmonicsDecode(in vec3 pDirection, in vec3 pSphericalHarmonics[9]){
#if GI_COMPRESSION == 0
  return (0.282094792 * pSphericalHarmonics[0]) +
         (0.488602512 * ((pSphericalHarmonics[2] * pDirection.z) -
                         ((pSphericalHarmonics[1] * pDirection.y) +  
                          (pSphericalHarmonics[3] * pDirection.x)))) +
         (1.092548431 * ((pSphericalHarmonics[4] * (pDirection.x * pDirection.y)) - 
                         ((pSphericalHarmonics[5] * (pDirection.y * pDirection.z)) + 
                          (pSphericalHarmonics[7] * (pDirection.x * pDirection.z))))) + 
         (((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pSphericalHarmonics[6]) +
         ((0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8]);
#elif GI_COMPRESSION == 1
  return (0.282094792 * pSphericalHarmonics[0]) +
         (0.488602512 * ((pSphericalHarmonics[2] * pDirection.z) - 
                         ((pSphericalHarmonics[1] * pDirection.y) +
                          (pSphericalHarmonics[3] * pDirection.x)))) +
         vec3((1.092548431 * ((pSphericalHarmonics[4].x * (pDirection.x * pDirection.y)) -
                              ((pSphericalHarmonics[5].x * (pDirection.y * pDirection.z)) + 
                               (pSphericalHarmonics[7].x * (pDirection.x * pDirection.z))))) + 
              (((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pSphericalHarmonics[6].x) +
              ((0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8].x), vec2(0.0));
#elif GI_COMPRESSION == 2
  return (0.282094792 * pSphericalHarmonics[0]) +         
         vec3((0.488602512 * ((pSphericalHarmonics[2].x * pDirection.z) -
                              ((pSphericalHarmonics[1].x * pDirection.y) + 
                               (pSphericalHarmonics[3].x * pDirection.x)))) +
              (1.092548431 * ((pSphericalHarmonics[4].x * (pDirection.x * pDirection.y)) - 
                              ((pSphericalHarmonics[5].x * (pDirection.y * pDirection.z)) + 
                               (pSphericalHarmonics[7].x * (pDirection.x * pDirection.z))))) + 
              (((0.946174695 * (pDirection.z * pDirection.z)) - 0.315391565) * pSphericalHarmonics[6].x) +
              ((0.546274215 * ((pDirection.x * pDirection.x) - (pDirection.y * pDirection.y))) * pSphericalHarmonics[8].x), vec2(0.0));
#else  
  return vec3(0.0);
#endif 
}

void globalIlluminationSphericalHarmonicsExtract(const in vec3 pSphericalHarmonics[9], out vec3 ambient, out vec3 directional, out vec3 direction){
  const float g_sh1 = 0.2820947917738781;                                                     // 0.5 * sqrt(1.0 / pi)
  const float g_sh2 = 0.4886025119029199;                                                     // 0.5 * sqrt(3.0 / pi) 
  const float g_sh3 = 1.0925484305920790;                                                     // 0.5 * sqrt(15.0 / pi) 
  const float g_sh4 = 0.3153915652525200;                                                     // 0.25 * sqrt(5.0 / pi) 
  const float g_sh5 = 0.5462742152960395;                                                     // 0.25 * sqrt(15.0 / pi) 
  const float directionalLightNormalizationFactor = 2.9567930857315701;                       // (16.0 * pi) / 17.0
  const float ambientLightNormalizationFactor = 3.5449077018110321;                           // 2.0 * sqrt(pi)
  const float inverseAmbientLightNormalizationFactor = 1.0 / ambientLightNormalizationFactor;  
  direction = normalize((normalize(vec3(-pSphericalHarmonics[3].r, -pSphericalHarmonics[1].r, pSphericalHarmonics[2].r)) * 0.3) + 
                        (normalize(vec3(-pSphericalHarmonics[3].g, -pSphericalHarmonics[1].g, pSphericalHarmonics[2].g)) * 0.59) + 
                        (normalize(vec3(-pSphericalHarmonics[3].b, -pSphericalHarmonics[1].b, pSphericalHarmonics[2].b)) * 0.11));
  vec3 sh0l = vec3(g_sh1, -(g_sh2 * direction.y), g_sh2 * direction.z) * directionalLightNormalizationFactor;
  vec3 sh1l = vec3(-(g_sh2 * direction.x), g_sh3 * (direction.y * direction.x), -(g_sh3 * (direction.y * direction.z))) * directionalLightNormalizationFactor;
  vec3 sh2l = vec3(g_sh4 * ((3.0 * (direction.z * direction.z)) - 1.0), -(g_sh3 * (direction.x * direction.z)), g_sh5 * ((direction.x * direction.x) - (direction.y * direction.y))) * directionalLightNormalizationFactor;
  directional = max(vec3(0.0), vec3(dot(vec3(pSphericalHarmonics[0].r, pSphericalHarmonics[1].r, pSphericalHarmonics[2].r), sh0l) + dot(vec3(pSphericalHarmonics[3].r, pSphericalHarmonics[4].r, pSphericalHarmonics[5].r), sh1l) + dot(vec3(pSphericalHarmonics[6].r, pSphericalHarmonics[7].r, pSphericalHarmonics[8].r), sh2l),
                                    dot(vec3(pSphericalHarmonics[0].g, pSphericalHarmonics[1].g, pSphericalHarmonics[2].g), sh0l) + dot(vec3(pSphericalHarmonics[3].g, pSphericalHarmonics[4].g, pSphericalHarmonics[5].g), sh1l) + dot(vec3(pSphericalHarmonics[6].g, pSphericalHarmonics[7].g, pSphericalHarmonics[8].g), sh2l),
                                    dot(vec3(pSphericalHarmonics[0].b, pSphericalHarmonics[1].b, pSphericalHarmonics[2].b), sh0l) + dot(vec3(pSphericalHarmonics[3].b, pSphericalHarmonics[4].b, pSphericalHarmonics[5].b), sh1l) + dot(vec3(pSphericalHarmonics[6].b, pSphericalHarmonics[7].b, pSphericalHarmonics[8].b), sh2l)) /
                                   (dot(sh0l, sh0l) + dot(sh1l, sh1l) + dot(sh2l, sh2l)));
  ambient = max(vec3(0.0), (pSphericalHarmonics[0].rgb - (directional * g_sh1)) * inverseAmbientLightNormalizationFactor);  
}

void globalIlluminationSphericalHarmonicsExtractDominantLight(const in vec3 pSphericalHarmonics[9], out vec3 directional, out vec3 direction){
  const float g_sh1 = 0.2820947917738781;                                                     // 0.5 * sqrt(1.0 / pi)
  const float g_sh2 = 0.4886025119029199;                                                     // 0.5 * sqrt(3.0 / pi) 
  const float g_sh3 = 1.0925484305920790;                                                     // 0.5 * sqrt(15.0 / pi) 
  const float g_sh4 = 0.3153915652525200;                                                     // 0.25 * sqrt(5.0 / pi) 
  const float g_sh5 = 0.5462742152960395;                                                     // 0.25 * sqrt(15.0 / pi) 
  const float directionalLightNormalizationFactor = 2.9567930857315701;                       // (16.0 * pi) / 17.0
  const float ambientLightNormalizationFactor = 3.5449077018110321;                           // 2.0 * sqrt(pi)
  const float inverseAmbientLightNormalizationFactor = 1.0 / ambientLightNormalizationFactor;  
  direction = normalize((normalize(vec3(-pSphericalHarmonics[3].r, -pSphericalHarmonics[1].r, pSphericalHarmonics[2].r)) * 0.3) + 
                        (normalize(vec3(-pSphericalHarmonics[3].g, -pSphericalHarmonics[1].g, pSphericalHarmonics[2].g)) * 0.59) + 
                        (normalize(vec3(-pSphericalHarmonics[3].b, -pSphericalHarmonics[1].b, pSphericalHarmonics[2].b)) * 0.11));
  vec3 sh0l = vec3(g_sh1, -(g_sh2 * direction.y), g_sh2 * direction.z) * directionalLightNormalizationFactor;
  vec3 sh1l = vec3(-(g_sh2 * direction.x), g_sh3 * (direction.y * direction.x), -(g_sh3 * (direction.y * direction.z))) * directionalLightNormalizationFactor;
  vec3 sh2l = vec3(g_sh4 * ((3.0 * (direction.z * direction.z)) - 1.0), -(g_sh3 * (direction.x * direction.z)), g_sh5 * ((direction.x * direction.x) - (direction.y * direction.y))) * directionalLightNormalizationFactor;
  directional = max(vec3(0.0), vec3(dot(vec3(pSphericalHarmonics[0].r, pSphericalHarmonics[1].r, pSphericalHarmonics[2].r), sh0l) + dot(vec3(pSphericalHarmonics[3].r, pSphericalHarmonics[4].r, pSphericalHarmonics[5].r), sh1l) + dot(vec3(pSphericalHarmonics[6].r, pSphericalHarmonics[7].r, pSphericalHarmonics[8].r), sh2l),
                                    dot(vec3(pSphericalHarmonics[0].g, pSphericalHarmonics[1].g, pSphericalHarmonics[2].g), sh0l) + dot(vec3(pSphericalHarmonics[3].g, pSphericalHarmonics[4].g, pSphericalHarmonics[5].g), sh1l) + dot(vec3(pSphericalHarmonics[6].g, pSphericalHarmonics[7].g, pSphericalHarmonics[8].g), sh2l),
                                    dot(vec3(pSphericalHarmonics[0].b, pSphericalHarmonics[1].b, pSphericalHarmonics[2].b), sh0l) + dot(vec3(pSphericalHarmonics[3].b, pSphericalHarmonics[4].b, pSphericalHarmonics[5].b), sh1l) + dot(vec3(pSphericalHarmonics[6].b, pSphericalHarmonics[7].b, pSphericalHarmonics[8].b), sh2l)) /
                                   (dot(sh0l, sh0l) + dot(sh1l, sh1l) + dot(sh2l, sh2l)));
}

void  globalIlluminationSphericalHarmonicsMultiply(inout vec3 y[9], const in vec3 f[9], const in vec3 g[9]){
  vec3 tf, tg, t;

  // [0,0]: 0,
  y[0]  = (0.282094792935999980)*f[0]*g[0];

  // [1,1]: 0,6,8,
  tf = (0.282094791773000010)*f[0]+(-0.126156626101000010)*f[6]+(-0.218509686119999990)*f[8];
  tg = (0.282094791773000010)*g[0]+(-0.126156626101000010)*g[6]+(-0.218509686119999990)*g[8];
  y[1]  = tf*g[1]+tg*f[1];
  t = f[1]*g[1];
  y[0] += (0.282094791773000010)*t;
  y[6]  = (-0.126156626101000010)*t;
  y[8]  = (-0.218509686119999990)*t;

  // [1,2]: 5,
  tf = (0.218509686118000010)*f[5];
  tg = (0.218509686118000010)*g[5];
  y[1] += tf*g[2]+tg*f[2];
  y[2]  = tf*g[1]+tg*f[1];
  t = f[1]*g[2]+f[2]*g[1];
  y[5]  = (0.218509686118000010)*t;

  // [1,3]: 4,
  tf = (0.218509686114999990)*f[4];
  tg = (0.218509686114999990)*g[4];
  y[1] += tf*g[3]+tg*f[3];
  y[3]  = tf*g[1]+tg*f[1];
  t = f[1]*g[3]+f[3]*g[1];
  y[4]  = (0.218509686114999990)*t;

  // [2,2]: 0,6,
  tf = (0.282094795249000000)*f[0]+(0.252313259986999990)*f[6];
  tg = (0.282094795249000000)*g[0]+(0.252313259986999990)*g[6];
  y[2] += tf*g[2]+tg*f[2];
  t = f[2]*g[2];
  y[0] += (0.282094795249000000)*t;
  y[6] += (0.252313259986999990)*t;

  // [2,3]: 7,
  tf = (0.218509686118000010)*f[7];
  tg = (0.218509686118000010)*g[7];
  y[2] += tf*g[3]+tg*f[3];
  y[3] += tf*g[2]+tg*f[2];
  t = f[2]*g[3]+f[3]*g[2];
  y[7]  = (0.218509686118000010)*t;

  // [3,3]: 0,6,8,
  tf = (0.282094791773000010)*f[0]+(-0.126156626101000010)*f[6]+(0.218509686119999990)*f[8];
  tg = (0.282094791773000010)*g[0]+(-0.126156626101000010)*g[6]+(0.218509686119999990)*g[8];
  y[3] += tf*g[3]+tg*f[3];
  t = f[3]*g[3];
  y[0] += (0.282094791773000010)*t;
  y[6] += (-0.126156626101000010)*t;
  y[8] += (0.218509686119999990)*t;

  // [4,4]: 0,6,
  tf = (0.282094791770000020)*f[0]+(-0.180223751576000010)*f[6];
  tg = (0.282094791770000020)*g[0]+(-0.180223751576000010)*g[6];
  y[4] += tf*g[4]+tg*f[4];
  t = f[4]*g[4];
  y[0] += (0.282094791770000020)*t;
  y[6] += (-0.180223751576000010)*t;

  // [4,5]: 7,
  tf = (0.156078347226000000)*f[7];
  tg = (0.156078347226000000)*g[7];
  y[4] += tf*g[5]+tg*f[5];
  y[5] += tf*g[4]+tg*f[4];
  t = f[4]*g[5]+f[5]*g[4];
  y[7] += (0.156078347226000000)*t;

  // [5,5]: 0,6,8,
  tf = (0.282094791773999990)*f[0]+(0.090111875786499998)*f[6]+(-0.156078347227999990)*f[8];
  tg = (0.282094791773999990)*g[0]+(0.090111875786499998)*g[6]+(-0.156078347227999990)*g[8];
  y[5] += tf*g[5]+tg*f[5];
  t = f[5]*g[5];
  y[0] += (0.282094791773999990)*t;
  y[6] += (0.090111875786499998)*t;
  y[8] += (-0.156078347227999990)*t;

  // [6,6]: 0,6,
  tf = (0.282094797560000000)*f[0];
  tg = (0.282094797560000000)*g[0];
  y[6] += tf*g[6]+tg*f[6];
  t = f[6]*g[6];
  y[0] += (0.282094797560000000)*t;
  y[6] += (0.180223764527000010)*t;

  // [7,7]: 0,6,8,
  tf = (0.282094791773999990)*f[0]+(0.090111875786499998)*f[6]+(0.156078347227999990)*f[8];
  tg = (0.282094791773999990)*g[0]+(0.090111875786499998)*g[6]+(0.156078347227999990)*g[8];
  y[7] += tf*g[7]+tg*f[7];
  t = f[7]*g[7];
  y[0] += (0.282094791773999990)*t;
  y[6] += (0.090111875786499998)*t;
  y[8] += (0.156078347227999990)*t;

  // [8,8]: 0,6,
  tf = (0.282094791770000020)*f[0]+(-0.180223751576000010)*f[6];
  tg = (0.282094791770000020)*g[0]+(-0.180223751576000010)*g[6];
  y[8] += tf*g[8]+tg*f[8];
  t = f[8]*g[8];
  y[0] += (0.282094791770000020)*t;
  y[6] += (-0.180223751576000010)*t;

  // multiply count=120
}

#endif