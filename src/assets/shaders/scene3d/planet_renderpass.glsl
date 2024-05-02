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
