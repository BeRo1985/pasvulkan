#version 460 core

// Based on:
//
// GPU Pro 7: Real-Time Volumetric Cloudscapes - A. Schneider
//     Follow up presentation: http://advances.realtimerendering.com/s2017/Nubis%20-%20Authoring%20Realtime%20Volumetric%20Cloudscapes%20with%20the%20Decima%20Engine%20-%20Final%20.pdf
// R. Hogfeldt, "Convincing Cloud Rendering An Implementation of Real-Time Dynamic Volumetric Clouds in Frostbite"
// F. Bauer, "Creating the Atmospheric World of Red Dead Redemption 2: A Complete and Integrated Solution" in Advances in Real-Time Rendering in Games, Siggraph 2019.
// 
// Multi scattering approximation: http://magnuswrenninge.com/wp-content/uploads/2010/03/Wrenninge-OzTheGreatAndVolumetric.pdf
// Participating media and volumetric integration: https://media.contentapi.ea.com/content/dam/eacom/frostbite/files/s2016-pbs-frostbite-sky-clouds-new.pdf
//     Small example: https://www.shadertoy.com/view/XlBSRz
// 
// https://github.com/turanszkij/WickedEngine/blob/8f4f4e8649e34cf7f6b90d61674305ada4f4e2f0/WickedEngine/shaders/volumetricCloudHF.hlsl
// https://github.com/turanszkij/WickedEngine/blob/8f4f4e8649e34cf7f6b90d61674305ada4f4e2f0/WickedEngine/shaders/volumetricCloud_renderCS.hlsl

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_multiview : enable
#extension GL_EXT_samplerless_texture_functions : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_control_flow_attributes : enable
#ifdef RAYTRACING
  #extension GL_EXT_buffer_reference : enable
  #define USE_BUFFER_REFERENCE
  #define USE_MATERIAL_BUFFER_REFERENCE
#endif

#include "bufferreference_definitions.glsl"

/* clang-format off */

//layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

/* clang-format on */

#define MULTISCATAPPROX_ENABLED
#ifdef SHADOWS
 #define SHADOWS_ENABLED
 #define NOTEXCOORDS
#else
 #undef SHADOWS_ENABLED
#endif

#define FLAGS_USE_FAST_SKY 1u
#define FLAGS_USE_FAST_AERIAL_PERSPECTIVE 2u
#define FLAGS_USE_BLUE_NOISE 4u
#define FLAGS_SHADOWS 8u

// Push constants
layout(push_constant, std140) uniform PushConstants {
  int baseViewIndex;
  int countViews;
  int frameIndex;
  uint flags;
} pushConstants;

#include "globaldescriptorset.glsl"

#define PI PII
#include "math.glsl"
#undef PI

#ifdef SHADOWS
#define SPECIAL_SHADOWS

#if defined(RAYTRACING)
  #include "raytracing.glsl"
#endif

#if 1 //!defined(RAYTRACING)
#define NUM_SHADOW_CASCADES 4
const uint SHADOWMAP_MODE_NONE = 1;
const uint SHADOWMAP_MODE_PCF = 2;
const uint SHADOWMAP_MODE_DPCF = 3;
const uint SHADOWMAP_MODE_PCSS = 4;
const uint SHADOWMAP_MODE_MSM = 5;

#define inFrameIndex pushConstants.frameIndex

layout(set = 2, binding = 4, std140) uniform uboCascadedShadowMaps {
  mat4 shadowMapMatrices[NUM_SHADOW_CASCADES];
  vec4 shadowMapSplitDepthsScales[NUM_SHADOW_CASCADES];
  vec4 constantBiasNormalBiasSlopeBiasClamp[NUM_SHADOW_CASCADES];
  uvec4 metaData; // x = type
} uCascadedShadowMaps;

layout(set = 2, binding = 5) uniform sampler2DArray uCascadedShadowMapTexture;

#ifdef PCFPCSS

// Yay! Binding Aliasing! :-)
layout(set = 2, binding = 5) uniform sampler2DArrayShadow uCascadedShadowMapTextureShadow;

#endif // PCFPCSS
#endif // !RAYTRACING 

vec3 inWorldSpacePosition, workNormal;
#endif // SHADOWS

#include "shadows.glsl"

#include "atmosphere_common.glsl"

layout(location = 0) in vec2 inTexCoord;

#ifdef DUALBLEND
layout(location = 0) out vec4 outInscattering;
layout(location = 1) out vec4 outTransmittance;
layout(location = 2) out float outDepth; // linear depth with infinite for far plane (requires 32-bit floating point target buffer)
#else
layout(location = 0) out vec4 outInscattering; // w = monochromatic transmittance as alpha
layout(location = 1) out float outDepth; // linear depth with infinite for far plane (requires 32-bit floating point target buffer)
#endif

layout(set = 2, binding = 0, std430) buffer AtmosphereParametersBuffer {
  AtmosphereParameters atmosphereParameters;
} uAtmosphereParameters;

layout(set = 2, binding = 1) uniform sampler2D uCloud2DTextures[];

#define uCloudTextureSkyLuminance uCloud2DTextures[0]
#define uCloudTextureTransmittanceLUT uCloud2DTextures[1]

layout(set = 2, binding = 2) uniform sampler3D uCloud3DTextures[];

#define uCloudTextureShapeNoise uCloud3DTextures[0]
#define uCloudTextureDetailNoise uCloud3DTextures[1]
#define uCloudTextureCurlNoise uCloud3DTextures[2]

layout(set = 2, binding = 3) uniform samplerCube uCloudCubeTextures[];

#define uCloudTextureSkyLuminanceLUT uCloudCubeTextures[0]
#define uCloudTextureWeatherMap uCloudCubeTextures[1]

float bayer2(vec2 a){
  a = floor(a);
  return fract(dot(a, vec2(0.5, a.y * 0.75)));
}

float bayer4(vec2 a){
   return fma(bayer2(a * 0.5), 0.25, bayer2(a));
} 

float bayer8(vec2 a){
   return fma(bayer4(a * 0.5), 0.25, bayer4(a));
} 

float bayer16(vec2 a){
   return fma(bayer8(a * 0.5), 0.25, bayer8(a));
} 

float bayer32(vec2 a){
   return fma(bayer16(a * 0.5), 0.25, bayer16(a));
} 

float bayer64(vec2 a){
   return fma(bayer32(a * 0.5), 0.25, bayer32(a));
} 
                  
float bayer128(vec2 a){
   return fma(bayer64(a * 0.5), 0.25, bayer64(a));
} 
 
float bayer256(vec2 a){
   return fma(bayer128(a * 0.5), 0.25, bayer128(a));
} 
                        
vec2 rayIntersectSphere(vec3 rayOrigin, vec3 rayDirection, vec4 sphere){
  vec3 sphereCenterToRayOrigin = rayOrigin - sphere.xyz;
  float a = dot(rayDirection, rayDirection),
        b = dot(rayDirection, sphereCenterToRayOrigin) * 2.0,
        c = dot(sphereCenterToRayOrigin, sphereCenterToRayOrigin) - (sphere.w * sphere.w); 
  float discriminant = (b * b) - ((a * c) * 4.0);
  if(discriminant < 0.0){
    return vec2(-1.0);
  }else if(discriminant == 0.0){
    return vec2((-0.5 * b) / a);
  }else{
    float q = (b + (sqrt(discriminant) * ((b > 0.0) ? 1.0 : -1.0))) * (-0.5);
    return vec2(q / a, c / q);
  }  
}                      

float getHeightFractionForPoint(const in vec3 position){
  float height = length(position);  
  if((height >= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) && (height <= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight)){
    return clamp((height - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) / (uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight), 0.0, 1.0);
  }else if((height >= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.StartHeight) && (height <= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.EndHeight)){
    return clamp((height - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.StartHeight) / (uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.EndHeight - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.StartHeight), 0.0, 1.0);
  }else{  
    return 0.0;
  }
}

float getDensityHeightGradientForPoint(const in vec3 position, const in float heightFraction, const in vec4 weatherData){
  const vec3 weatherTypeMask = vec3(1.0 - clamp(weatherData.y * 2.0, 0.0, 1.0), 1.0 - (abs(weatherData.y - 0.5) * 2.0), clamp(weatherData.y - 0.5, 0.0, 1.0) * 2.0);
  const vec4 heightGradient = uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.heightGradients * weatherTypeMask;
  return smoothstep(heightGradient.x, heightGradient.y, heightFraction) * smoothstep(heightGradient.w, heightGradient.z, heightFraction);
}
                             
vec3 scaleLayerLowCloudPosition(vec3 position){
  return position * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.PositionScale;
}                                        

vec3 scaleLayerHighCloudPosition(vec3 position){
  return position * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.PositionScale;
}

#include "rotation.glsl"

vec4 getWeatherData(const in vec3 position, const float mipMapLevel){
  return clamp(
    fma(
      textureLod(uCloudTextureWeatherMap, normalize(position), mipMapLevel),
      uAtmosphereParameters.atmosphereParameters.VolumetricClouds.coverageTypeWetnessTopFactors, 
      uAtmosphereParameters.atmosphereParameters.VolumetricClouds.coverageTypeWetnessTopOffsets
    ),
    vec4(0.0),
    vec4(1.0)
  );
}                                     

//////////////////////////////////////////////////////////////////////////
            
float getLayerHighCloudNoiseHash(ivec3 p){
  vec3 p3 = fract(vec3(p) * 0.1031);
  p3 += dot(p3, p3.zyx + vec3(31.32));
  return fract((p3.x + p3.y) * p3.z);
}

float getLayerHighCloudNoise(vec3 p){
  ivec3 i = ivec3(floor(p));
  vec3 f = fract(p);  
  f *= f * (3.0 - (2.0 * f));  
  ivec2 e = ivec2(0, 1);	
  return mix(mix(mix(getLayerHighCloudNoiseHash(i + e.xxx), getLayerHighCloudNoiseHash(i + e.yxx), f.x),
                 mix(getLayerHighCloudNoiseHash(i + e.xyx), getLayerHighCloudNoiseHash(i + e.yyx), f.x), f.y),
             mix(mix(getLayerHighCloudNoiseHash(i + e.xxy), getLayerHighCloudNoiseHash(i + e.yxy), f.x),
                 mix(getLayerHighCloudNoiseHash(i + e.xyy), getLayerHighCloudNoiseHash(i + e.yyy), f.x), f.y), f.z);
}

float layerHighCloudRotationTime = uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.Speed;
mat3 layerHighCloudRotationBase = rotationMatrix(normalize(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationBase.xyz), uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationBase.w * layerHighCloudRotationTime);
mat3 layerHighCloudRotationOctave1 = rotationMatrix(normalize(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationOctave1.xyz), uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationOctave1.w * layerHighCloudRotationTime);
mat3 layerHighCloudRotationOctave2 = rotationMatrix(normalize(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationOctave2.xyz), uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationOctave2.w * layerHighCloudRotationTime);
mat3 layerHighCloudRotationOctave3 = rotationMatrix(normalize(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationOctave3.xyz), uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.RotationOctave3.w * layerHighCloudRotationTime);

float getLayerHighClouds(const in vec3 p, const in VolumetricCloudLayerHigh cloudLayerParameters, const in vec4 weatherData){
  float h = length(p);
  if((weatherData.w > 1e-4) && (h >= cloudLayerParameters.StartHeight) && (h <= cloudLayerParameters.EndHeight)){
    vec3 cloudCoord = layerHighCloudRotationBase * (p * cloudLayerParameters.PositionScale);
    float noise = getLayerHighCloudNoise(cloudCoord * cloudLayerParameters.OctaveScales.x) * cloudLayerParameters.OctaveFactors.x;
 	  noise += getLayerHighCloudNoise((layerHighCloudRotationOctave1 * cloudCoord) * cloudLayerParameters.OctaveScales.y) * cloudLayerParameters.OctaveFactors.y;
    noise += getLayerHighCloudNoise((layerHighCloudRotationOctave2 * cloudCoord) * cloudLayerParameters.OctaveScales.z) * cloudLayerParameters.OctaveFactors.z;
    noise += getLayerHighCloudNoise((layerHighCloudRotationOctave3 * cloudCoord) * cloudLayerParameters.OctaveScales.w) * cloudLayerParameters.OctaveFactors.w;
    float horizonHeightPercent = clamp((h - cloudLayerParameters.StartHeight) / (cloudLayerParameters.EndHeight - cloudLayerParameters.StartHeight), 0.0, 1.0);
    return smoothstep(cloudLayerParameters.CoverMin, cloudLayerParameters.CoverMax, noise) *
           (smoothstep(0.0, cloudLayerParameters.FadeMin, horizonHeightPercent) *
            smoothstep(1.0, 1.0 - cloudLayerParameters.FadeMax, horizonHeightPercent)) *
           cloudLayerParameters.Density *
           weatherData.w;
  }else{
    return 0.0;    
  }            
}
                                                       
//////////////////////////////////////////////////////////////////////////

vec3 decodeCURL(vec3 c){
	return fma(c, vec3(2.0), vec3(-1.0));
}

vec3 encodeCURL(vec3 c){
	return fma(c, vec3(0.5), vec3(0.5));
}

float remap(const in float x, const in float a0, const in float a1, const in float b0, const in float b1){
//return mix(b0, b1, (x - a0) / (a1 - a0));
  return b0 + (((x - a0) / (a1 - a0)) * (b1 - b0));
}

float remapClamped(const in float x, const in float a0, const in float a1, const in float b0, const in float b1){
  return mix(b0, b1, clamp((x - a0) / (a1 - a0), 0.0, 1.0));
}

float remap01(const in float x, const in float l, const in float h){
  return clamp((x - l) / (h - l), 0.0, 1.0);
}

float getLowResCloudDensity(const in vec3 position, const in vec3 offset, const in vec4 weatherData, const float mipMapLevel){
            
  float height = length(position);
  
  if((weatherData.x > 1e-4) && (height >= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) && (height <= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight)){

    // Layer low clouds
                       
    float heightFraction = clamp((height - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) / (uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight), 0.0, 1.0);
                       
    // Read the low-frequency Perlin-Worley and Worley noises
    vec4 lowFrequencyNoises = textureLod(uCloudTextureShapeNoise, scaleLayerLowCloudPosition(position + offset), mipMapLevel);
                                                                                                                     
    // Build an FBM out of the low frequency Worley noises that can be used to add detail to the low-frequency Perlin-Worley noise
    float lowFrequencyFBM = dot(lowFrequencyNoises.yzw, vec3(0.625, 0.25, 0.125));
  
    // Define the base cloud shape by dilating it with the low-frequency FBM made of Worley noise
    float baseCloud = remap(lowFrequencyNoises.x, -(1.0 - lowFrequencyFBM), 1.0, 0.0, 1.0);
    
    // Get the density-height gradient using the density height function
    float densityHeightGradient = getDensityHeightGradientForPoint(position, heightFraction, weatherData);
    
    // Apply the height funct ion to the base cloud shape .
    baseCloud *= densityHeightGradient;
    
    // Cloud coverage is stored in weather data's red channel .
    float cloudCoverage = weatherData.x;
    
    // Use remap to apply the cloud coverage attribute .
    float baseCloudWithCoverage = remap(baseCloud, 1.0 - cloudCoverage, 1.0, 0.0, 1.0);
  
    // Multiply the result by the cloud coverage attribute so that smaller clouds are lighter and more aesthetically pleasing
    baseCloudWithCoverage *= cloudCoverage;
    
    return clamp(baseCloudWithCoverage, 0.0, 1.0);
    
  }else if((weatherData.w > 1e-4) && (height >= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.StartHeight) && (height <= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.EndHeight)){

    // Layer high clouds
    
    return getLayerHighClouds(position, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh, weatherData);
    
  }else{
  
    return 0.0;
    
  } 
  
  
}     


void main(){

}