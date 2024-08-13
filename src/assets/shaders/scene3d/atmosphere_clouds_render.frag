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

#include "math.glsl"

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

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

#ifdef MSAA

#ifdef MULTIVIEW
layout(set = 2, binding = 0) uniform texture2DMSArray uDepthTexture;
#else
layout(set = 2, binding = 0) uniform texture2DMS uDepthTexture;
#endif

#else

#ifdef MULTIVIEW
layout(set = 2, binding = 0) uniform texture2DArray uDepthTexture; 
#else
layout(set = 2, binding = 0) uniform texture2D uDepthTexture;
#endif

#endif

layout(set = 2, binding = 1, std430) buffer AtmosphereParametersBuffer {
  AtmosphereParameters atmosphereParameters;
} uAtmosphereParameters;

layout(set = 2, binding = 2) uniform sampler2D uTextureBlueNoise;

layout(set = 2, binding = 3) uniform sampler2D uCloud2DTextures[];

#define uCloudTextureSkyLuminance uCloud2DTextures[0]
#define uCloudTextureTransmittanceLUT uCloud2DTextures[1]

layout(set = 2, binding = 4) uniform sampler3D uCloud3DTextures[];

#define uCloudTextureShapeNoise uCloud3DTextures[0]
#define uCloudTextureDetailNoise uCloud3DTextures[1]
#define uCloudTextureCurlNoise uCloud3DTextures[2]

layout(set = 2, binding = 5) uniform samplerCube uCloudCubeTextures[];

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

vec2 intersectSphere(vec3 rayOrigin, vec3 rayDirection, vec4 sphere){
  vec3 v = rayOrigin - sphere.xyz;
  float b = dot(v, rayDirection),
        c = dot(v, v) - (sphere.w * sphere.w),
        d = (b * b) - c;
  return (d < 0.0) 
             ? vec2(-1.0)                                // No intersection
             : ((vec2(-1.0, 1.0) * sqrt(d)) - vec2(b));  // Intersection
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

mat3 layerLowWindRotation, layerLowCurlRotation;

float getLowResCloudDensity(const in vec3 position, const in mat3 windRotation, const in vec4 weatherData, const float mipMapLevel){
            
  float height = length(position);
  
  if((weatherData.x > 1e-4) && (height >= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) && (height <= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight)){

    // Layer low clouds
                       
    float heightFraction = clamp((height - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) / (uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight), 0.0, 1.0);
                       
    // Read the low-frequency Perlin-Worley and Worley noises
    vec4 lowFrequencyNoises = textureLod(uCloudTextureShapeNoise, scaleLayerLowCloudPosition(windRotation * position) * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.ShapeNoiseScale, mipMapLevel);
                                                                                                                      
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

float getHighResCloudDensity(const in vec3 position, const in mat3 windRotation, const in vec3 curlOffset, const in vec4 weatherData, const float lowResDensity, const float mipMapLevel){
                           
  float height = length(position);
  
  if((weatherData.x > 1e-4) && (height >= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) && (height <= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight)){

    // Layer low clouds
  
    float heightFraction = clamp((height - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight) / (uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.EndHeight - uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight), 0.0, 1.0);
                                       
    // Sample high-frequency noises
    vec3 highFrequencyNoises = textureLod(
      uCloudTextureDetailNoise,
      scaleLayerLowCloudPosition(
        (windRotation * position) +
        (curlOffset.xyz * (1.0 - heightFraction) * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.CurlScale)
      ) * 
      uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.DetailNoiseScale,
      mipMapLevel
    ).xyz;

    // Build high frequency Worley noise FBM
    float highFrequencyFBM = dot(highFrequencyNoises, vec3(0.625, 0.25, 0.125));
  
    // Transition from wispy shapes to billowy shapes over height 
    float highFrequencyNoiseModifer = mix(highFrequencyFBM, 
                                          1.0 - highFrequencyFBM, 
                                          clamp(heightFraction * 10.0, 0.0, 1.0));
  
    // Erode the base cloud shape with the distorted high-frequency Worley noises
    return clamp(remap(lowResDensity, highFrequencyNoiseModifer * 0.2, 1.0, 0.0, 1.0), 0.0, 1.0);
    
  }else{

    // For other cloud layers, return just the low resolution density, as high resolution density is not needed or already included in the low resolution density

    return clamp(lowResDensity, 0.0, 1.0);
  
  }

}

/*float scaleDensity(float density){
  return density;
}*/

float powderTerm(float density, float cosAngle){
	return mix(1.0, clamp(1.0 - exp(-(density * 2.0)), 0.0, 1.0), clamp(fma(cosAngle, -0.5, 0.5), 0.0, 1.0));
}                                 
  
float powderTerm(float density){
  return clamp(1.0 - exp(-(density * 2.0)), 0.0, 1.0);
}                                 
 
float beerTerm(float density){
  return exp(-density);
}

float beerLaw(float density){
	return max(exp(-density), exp(-density * 0.5) * 0.7);
}

float henyeyGreensteinPhase(float cosAngle, float g){
  float g2 = g * g;
  return ((1.0 - g2) / pow((1.0 + g2) - (2.0 * g * cosAngle), 3.0 / 2.0)) / (4.0 * PI);
}

float getSunPhase(vec3 rayDirection, vec3 sunDirection, float g) {
  float g2 = g * g;
  return (1.0 - g2) / (pow((1.0 + g2) - ((2.0 * g) * dot(rayDirection, sunDirection)), 3.0 / 2.0) * (4.0 * PI));
}

const vec3 randomVectors[8] = vec3[](
	vec3( 0.38051305,  0.92453449, -0.02111345),
	vec3(-0.50625799, -0.03590792, -0.86163418),
	vec3(-0.32509218, -0.94557439,  0.01428793),
	vec3( 0.09026238, -0.27376545,  0.95755165),
	vec3( 0.28128598,  0.42443639, -0.86065785),
	vec3(-0.16852403,  0.14748697,  0.97460106),
	vec3(-0.86065785,  0.28128598,  0.42443639),
	vec3( 0.73454242, -0.17479357,  0.27376545)
);

float sampleShadow(const in vec3 rayOrigin,
                   const in vec3 rayDirection,
                   const in float rayLength,
                   const in bool highResCloudDensity,
                   const float mipMapLevel){
                   
  vec2 tTopSolutions = intersectSphere(rayOrigin, rayDirection, vec2(0.0, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.EndHeight).xxxy);
  if(tTopSolutions.y >= 0.0){
    
    vec2 tMinMax = tTopSolutions;             

    vec2 tGroundSolutions = intersectSphere(rayOrigin, rayDirection, vec2(0.0, uAtmosphereParameters.atmosphereParameters.BottomRadius).xxxy);
    if(tGroundSolutions.x >= 0.0){
      return 0.0; // Planet blocks all sun light
    }

    vec2 tBottomSolutions = intersectSphere(rayOrigin, rayDirection, vec2(0.0, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight).xxxy);

    if((tBottomSolutions.x < 0.0) && (tBottomSolutions.y >= 0.0)){
      // Below clouds
      tMinMax.x = min(tMinMax.x, tBottomSolutions.y);
    }
    
    tMinMax = clamp(tMinMax, vec2(0.0), vec2(rayLength));
 
    if(tMinMax.x < tMinMax.y){
                   
      const int numSteps = 7; 
      float r = 1.0, timeStep = rayLength / float(numSteps), time = timeStep * 0.5;         
      for(int i = 0; i < numSteps; i++){
        vec3 position = rayOrigin + (rayDirection * time);
        if(length(position) > uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.EndHeight){
          break;
        }else{
          vec4 weatherData = getWeatherData(position, mipMapLevel + 1.0);
          float density = getLowResCloudDensity(position, layerLowWindRotation, weatherData, mipMapLevel + 1.0);            
          if(highResCloudDensity){
            // If are ray march is hasn't absorbed too much light yet, we use the high res cloud data to calculate the self occlusion of the cloud 
            density = getHighResCloudDensity(position, layerLowWindRotation, vec3(0.0), weatherData, density, mipMapLevel + 1.0);
          }
          r *= exp(-(density * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.ShadowDensity * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.DensityScale * timeStep));
          time += timeStep;  
        }
      }
      
      return r;
      
    }
    
   }
   
   return 1.0;
   
}

float sampleCloudDensityAlongCone(const in vec3 rayOrigin,
                                  const in vec3 rayDirection,
                                  const in float rayLength,
                                  const in float rayLengthFarMultipler,
                                  const in bool highResCloudDensity,
                                  const float mipMapLevel){
  const int numSteps = 7;
  float coneSpreadMultipler = length(rayDirection) * (rayLength / float(numSteps + 1)),
        densityAlongCone = 0.0;
  vec3 position = rayOrigin;
  for(int stepIndex = 0; stepIndex <= numSteps; stepIndex++){
    position = (stepIndex == numSteps) 
                 ? (rayOrigin + (rayDirection * (rayLength * rayLengthFarMultipler)))
                 : (position + (rayDirection + (coneSpreadMultipler * randomVectors[stepIndex] * float(stepIndex))));
    if(length(position) <= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.EndHeight){
      vec4 weatherData = getWeatherData(position, mipMapLevel + 1.0);                    
      float density = getLowResCloudDensity(position, layerLowWindRotation, weatherData, mipMapLevel + 1.0); 
      if(highResCloudDensity){
        // If are ray march is hasn't absorbed too much light yet, 
        // we use the high res cloud data to calculate the self occlusion of the cloud 
        vec3 curlOffsetVector = decodeCURL(textureLod(uCloudTextureCurlNoise, scaleLayerLowCloudPosition(layerLowWindRotation * position), mipMapLevel + 1.0).xyz) * (1.0 * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Scale);
        density = getHighResCloudDensity(position, layerLowWindRotation, curlOffsetVector, weatherData, density, mipMapLevel + 1.0); 
      }
      densityAlongCone += density * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LightingDensity * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.DensityScale;
    }
  }                 
  return densityAlongCone;
} 

void traceVolumetricClouds(vec3 rayOrigin, 
                           vec3 rayDirection, 
                           float depthBufferValue,
                           ivec2 threadPosition,
                           out vec3 inscattering,
                           out vec3 transmittance, 
                           out float depth){

  inscattering = vec3(0.0);
  
  transmittance = vec3(1.0);

  vec3 toSunDirection = normalize(-getSunDirection(uAtmosphereParameters.atmosphereParameters));
  
  float cosAngle = dot(rayDirection, toSunDirection);

  float forwardScatteringPhase = henyeyGreensteinPhase(cosAngle, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.ForwardScatteringG);
  float backwardScatteringPhase = henyeyGreensteinPhase(cosAngle, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.BackwardScatteringG);

  vec3 scattering = uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Scattering.xyz;
  vec3 absorption = uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Absorption.xyz;
  vec3 extinction = absorption + scattering;
  
  vec3 sunColor = uAtmosphereParameters.atmosphereParameters.SolarIlluminance.xyz;

  vec2 weightedDepth = vec2(0.0);
 
  vec2 tTopSolutions = intersectSphere(rayOrigin, rayDirection, vec2(0.0, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerHigh.EndHeight).xxxy);
  if(tTopSolutions.y >= 0.0){

   float distanceToPlanetCenter = length(rayOrigin);
    
    vec3 viewNormal = normalize(rayOrigin);
    
    vec2 tMinMax = tTopSolutions;             

    vec2 tBottomSolutions = intersectSphere(rayOrigin, rayDirection, vec2(0.0, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.StartHeight).xxxy);

    vec2 tGroundSolutions = intersectSphere(rayOrigin, rayDirection, vec2(0.0, uAtmosphereParameters.atmosphereParameters.BottomRadius).xxxy);

    if((tBottomSolutions.x < 0.0) && (tBottomSolutions.y >= 0.0)){
      // Below clouds
      tMinMax.x = min(tMinMax.x, tBottomSolutions.y);
    }else if(tBottomSolutions.x >= 0.0){
      // Inside or above clouds
      if(tGroundSolutions.x >= 0.0){
        tMinMax.y = min(tMinMax.y, tBottomSolutions.x);
      } 
    }

    if(tGroundSolutions.x >= 0.0){
      // Above ground
      tMinMax.y = min(tMinMax.y, tGroundSolutions.x);
    }else if(tGroundSolutions.y >= 0.0){ 
      // Below ground
      if(dot(rayDirection, viewNormal) < 0.0){
        tMinMax = vec2(1.0, 0.0);//min(tMinMax.y, tGroundSolutions.y);
      }
    }
    
    tMinMax = max(vec2(0.0), tMinMax); 
    
    if(tMinMax.x < tMinMax.y){

      float mipMapLevel = 0.0;
      int countSteps = int(mix(128.0, 64.0, smoothstep(0.0, 1.0, dot(rayDirection, viewNormal))));
        
      float density = 0.0;  
      float cloudTestDensity = 0.0;
      float previousSampledDensity = 0.0;
      int zeroDensitySampleCounter = 0;

#if 1
      float offset = fract(texelFetch(uTextureBlueNoise, ivec2(threadPosition) & ivec2(1023), 0).x + (float(pushConstants.frameIndex) * 0.61803398875)); 
#else
      float offset = fract(bayer256(ivec2(threadPosition) & ivec2(1023)) + (float(pushConstants.frameIndex) * 0.61803398875)); 
#endif
    
      float rayLength = tMinMax.y - tMinMax.x,
            timeStep = rayLength / float(countSteps),
            time = fma(offset, timeStep, tMinMax.x);
                
      //float sunPhase = getSunPhase(rayDirection, sunDirection, -cloudsForwardScatteringG);
      
      for(int stepIndex = 0; (stepIndex < countSteps) && (time < tMinMax.y); stepIndex++){
    
        vec3 position = fma(rayDirection, vec3(time), rayOrigin);
  
        vec4 weatherData = getWeatherData(position, mipMapLevel);
        
        float density;
        if(max(weatherData.x, weatherData.w) > 1e-4){
          density = getLowResCloudDensity(position, layerLowWindRotation, weatherData, mipMapLevel);
        }else{
          density = 0.0;
        }
        
        if(density > 1e-4){
        
          if(zeroDensitySampleCounter > 0){
            zeroDensitySampleCounter = 0;            
            weatherData = getWeatherData(position = fma(rayDirection, vec3(time -= timeStep), rayOrigin), mipMapLevel);
            if(max(weatherData.x, weatherData.w) > 1e-4){
              density = getLowResCloudDensity(position, layerLowWindRotation, weatherData, mipMapLevel);
            }else{
              time += timeStep;          
              continue;
            }
          }            
        
          vec3 curlOffsetVector = decodeCURL(
            textureLod(
              uCloudTextureCurlNoise,
              scaleLayerLowCloudPosition((layerLowWindRotation * position) * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.AdvanceCurlScale),
              mipMapLevel).xyz
          ) * 
            (1.0 * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Scale) * 
            uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LayerLow.AdvanceCurlAmplitude *
            1.0;
                                                          
          density = getHighResCloudDensity(position, layerLowWindRotation, curlOffsetVector, weatherData, density, mipMapLevel);                                                                                  
          density *= uAtmosphereParameters.atmosphereParameters.VolumetricClouds.ViewDensity * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.DensityScale;
          
          if(density > 1e-4){
          
            float heightFraction = getHeightFractionForPoint(position);
          
            float scatteringCoefficient = density;
            float extinctionCoefficient = max(1e-10, density);
     
            float sunLightTerm = max(0.0, dot(normalize(position), toSunDirection));
            
            float densityAlongCone = sampleCloudDensityAlongCone(
              position, 
              toSunDirection, 
              uAtmosphereParameters.atmosphereParameters.VolumetricClouds.DensityAlongConeLength,
              uAtmosphereParameters.atmosphereParameters.VolumetricClouds.DensityAlongConeLengthFarMultiplier, 
              any(greaterThan(transmittance, vec3(0.3))), 
              mipMapLevel
            );  
//          float shadowTowardsLight = sampleShadow(position, toSunDirection, any(greaterThan(transmittance, vec3(0.3))), mipMapLevel);  
  
            float lightEnergy = beerTerm(densityAlongCone * timeStep) * 
                                powderTerm(density * timeStep, cosAngle) *             
                                sunLightTerm *
#if 0
                                fma(max(forwardScatteringPhase, backwardScatteringPhase), 0.07, 0.8) *
#else                                
                                fma(mix(forwardScatteringPhase, backwardScatteringPhase, 0.5), 1.0, 0.0) *
#endif                                                                
                                2.0;
                                
            vec3 directScatting = vec3(lightEnergy) * sunColor;
            
            // Fake multiple scattering 
            vec3 indirectScattering = clamp(pow(3.0 * scatteringCoefficient, 0.5), 0.7, 1.0) *
                                      textureLod(uCloudTextureSkyLuminanceLUT, normalize(position), 0.0).xyz * 
                                      beerTerm(density) *
                                      //sunLightTerm *
                                      vec3(1.0);
            
            vec3 sampledScattering = (directScatting + indirectScattering) * scatteringCoefficient;
             
            weightedDepth += vec2(length(position - rayOrigin), 1.0) * min(transmittance.x, min(transmittance.y, transmittance.z)); 
                                                             
#if 1
            // See slide 28 at http://www.frostbite.com/2015/08/physically-based-unified-volumetric-rendering-in-frostbite/
            vec3 sampledExtinction = max(vec3(1e-10), density * extinction);
            vec3 sampledTransmittance = exp(-sampledExtinction * timeStep);
            vec3 integratedSampledScattering = (sampledScattering - (sampledScattering * sampledTransmittance)) / sampledExtinction;
            inscattering += transmittance * integratedSampledScattering;
            transmittance *= sampledTransmittance;     
#else        
            inscattering += transmittance * sampledScattering * ((1.0 - exp(-(extinctionCoefficient * timeStep))) / extinctionCoefficient);
            transmittance *= exp(-extinctionCoefficient * timeStep * extinction);     
#endif      
              
#if 0
            inscattering += sampleScattering * 
                            scatteringCoefficient *
                            ((1.0 - exp(-(extinctionCoefficient * timeStep))) / extinctionCoefficient) *
                            transmittance * 
                            1.0;       
            transmittance *= exp(-extinctionCoefficient * timeStep);
#endif
            
            if(all(lessThan(transmittance, vec3(1e-4)))){
              break;
            }
                   
          }
         
          zeroDensitySampleCounter = 0;

          time += timeStep;          
         
        }else{

          zeroDensitySampleCounter++;
          
          time += timeStep * 2.0;  
         
        }                                                          
        
        
      }  
      
    }
    
  }
  
  depth = (weightedDepth.y > 0.0) ? (weightedDepth.x / weightedDepth.y) : uintBitsToFloat(0x7f800000u); 

}

void main(){

  layerLowWindRotation = layerLowCurlRotation = mat3(1.0);

  int viewIndex = pushConstants.baseViewIndex + int(gl_ViewIndex);
  View view = uView.views[viewIndex];

/*vec2 pixPos = vec2(gl_FragCoord.xy) + vec2(0.5);
  vec2 uv = pixPos / pushConstants.resolution;*/

  vec2 uv = inTexCoord; 

  vec3 worldPos, worldDir;
  GetCameraPositionDirection(worldPos, worldDir, view.viewMatrix, view.projectionMatrix, view.inverseViewMatrix, view.inverseProjectionMatrix, uv);
  
  worldPos = (uAtmosphereParameters.atmosphereParameters.inverseTransform * vec4(worldPos, 1.0)).xyz;
  worldDir = normalize((uAtmosphereParameters.atmosphereParameters.inverseTransform * vec4(worldDir, 0.0)).xyz);


}