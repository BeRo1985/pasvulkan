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

//#include "atmosphere_clouds_noise.glsl"
#include "tangentspacebasis.glsl"

float remap(const in float x, const in float a0, const in float a1, const in float b0, const in float b1){
  return (clamp((x - a0) / (a1 - a0), 0.0, 1.0) * (b1 - b0)) + b0;
}

float remapClamped(const in float x, const in float a0, const in float a1, const in float b0, const in float b1){
  return mix(b0, b1, clamp((x - a0) / (a1 - a0), 0.0, 1.0));
}

float remap01(const in float x, const in float l, const in float h){
  return clamp((x - l) / (h - l), 0.0, 1.0);
}

vec3 remap01Unclamped(const in vec3 x, const in vec3 l, const in vec3 h){
  return (x - l) / (h - l);
}

#include "shadows.glsl"

#include "atmosphere_common.glsl"

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outInscattering;
layout(location = 1) out float outDepth;

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

layout(set = 2, binding = 3) uniform samplerCube uCloudWeatherMapTextures[];

#define uCloudTextureWeatherMap0 uCloudWeatherMapTextures[0]
#define uCloudTextureWeatherMap1 uCloudWeatherMapTextures[1]

const int MS_COUNT = 2;

const float LOCAL_LIGHTS_INTENSITY_MULTIPLIER = 65504.0; // 100000.0 

const float CLOUD_DENSITY_THRESHOLD = 1e-3;

int MAX_STEP_COUNT = int(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.MaxStepCount);
float LOD_Min = uAtmosphereParameters.atmosphereParameters.VolumetricClouds.LODMin;
const float LOD_Max = 3.0;
int SHADOW_SAMPLE_COUNT = int(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.ShadowSampleCount);
int GROUND_CONTRIBUTION_SAMPLE_COUNT = int(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.GroundContributionSampleCount);

struct ParticipatingMedia {
	vec3 scatteringCoefficients[MS_COUNT];
	vec3 extinctionCoefficients[MS_COUNT];
	vec3 transmittanceToLight[MS_COUNT];
};

ParticipatingMedia SampleParticipatingMedia(vec3 baseAlbedo, vec3 baseExtinctionCoefficients, float baseMsScatteringFactor, float baseMsExtinctionFactor, vec3 initialTransmittanceToLight){

	const vec3 scatteringCoefficients = baseAlbedo * baseExtinctionCoefficients;

	ParticipatingMedia participatingMedia;
	participatingMedia.scatteringCoefficients[0] = scatteringCoefficients;
	participatingMedia.extinctionCoefficients[0] = baseExtinctionCoefficients;
	participatingMedia.transmittanceToLight[0] = initialTransmittanceToLight;

	float MsScatteringFactor = baseMsScatteringFactor;
	float MsExtinctionFactor = baseMsExtinctionFactor;

	[[unroll]] for (int ms = 1; ms < MS_COUNT; ms++){
		participatingMedia.scatteringCoefficients[ms] = participatingMedia.scatteringCoefficients[ms - 1] * MsScatteringFactor;
		participatingMedia.extinctionCoefficients[ms] = participatingMedia.extinctionCoefficients[ms - 1] * MsExtinctionFactor;
		MsScatteringFactor *= MsScatteringFactor;
		MsExtinctionFactor *= MsExtinctionFactor;

		participatingMedia.transmittanceToLight[ms] = initialTransmittanceToLight;
	}

	return participatingMedia;
}

void OpaqueShadow(inout ParticipatingMedia participatingMedia, vec3 worldPosition){
#ifdef SHADOWS
  inWorldSpacePosition = worldPosition;
	float shadow = getFastFurthestCascadedShadow();
	[[unroll]] for (int ms = 0; ms < MS_COUNT; ms++){
		participatingMedia.transmittanceToLight[ms] *= shadow;
	}
#endif
}

float GetHeightFractionForPoint(const in AtmosphereParameters atmosphereParameters, vec3 pos){
	float planetRadius = atmosphereParameters.BottomRadius;
	vec3 planetCenterWorld = vec3(0.0); //atmosphereParameters.inverseTransform[3].xyz;
	return clamp((distance(pos, planetCenterWorld) - (planetRadius + atmosphereParameters.VolumetricClouds.CloudStartHeight)) / atmosphereParameters.VolumetricClouds.CloudThickness, 0.0, 1.0);
}

float SampleGradient(vec4 gradient, float heightFraction){
	return smoothstep(gradient.x, gradient.y, heightFraction) - smoothstep(gradient.z, gradient.w, heightFraction);
}

vec3 GetWeatherTypeMask(vec3 weatherData){
	float weatherType = weatherData.g;    
	float smallType = 1.0 - clamp(weatherType * 2.0, 0.0, 1.0);
	float mediumType = 1.0f - abs(weatherType - 0.5) * 2.0;
	float largeType = clamp(weatherType - 0.5, 0.0, 1.0) * 2.0;
	return vec3(smallType, mediumType, largeType);
}

vec4 GetHeightGradientType(vec3 weatherTypeMask, const in VolumetricCloudLayer parameters){
	return (parameters.GradientSmall * weatherTypeMask.x) + (parameters.GradientMedium * weatherTypeMask.y) + (parameters.GradientLarge * weatherTypeMask.z);
}

vec4 GetAnvilDeformationType(vec3 weatherTypeMask, const in VolumetricCloudLayer parameters){
	return (parameters.AnvilDeformationSmall * weatherTypeMask.x) + (parameters.AnvilDeformationMedium * weatherTypeMask.y) + (parameters.AnvilDeformationLarge * weatherTypeMask.z);
}

float SampleHeightGradient(float heightFraction, vec3 weatherData, const in VolumetricCloudLayer parameters){
	vec3 weatherTypeMask = GetWeatherTypeMask(weatherData);
	vec4 heightGradient = GetHeightGradientType(weatherTypeMask, parameters);
	return SampleGradient(heightGradient, heightFraction);
}

float SampleAnvilDeformation(float heightFraction, vec3 weatherData, const in VolumetricCloudLayer parameters){
  vec3 weatherTypeMask = GetWeatherTypeMask(weatherData);
	vec4 heightGradient = GetHeightGradientType(weatherTypeMask, parameters);
	vec4 anvilDeformation = GetAnvilDeformationType(weatherTypeMask, parameters);
	return clamp(pow(clamp(heightFraction + anvilDeformation.g + (1.0 - heightGradient.a), 0.0, 1.0), anvilDeformation.r) + pow(clamp((1.0 - heightFraction) + anvilDeformation.a + heightGradient.r, 0.0, 1.0), anvilDeformation.b), 0.0, 1.0);
}

vec3 SampleWeather(const in samplerCube textureWeatherMap, vec3 p, float heightFraction, const in VolumetricCloudLayer parameters){
	
  vec3 pos = p.xyz;// + parameters.coverageWindOffset;

	//pos += heightFraction * parameters.coverageWindDirection * parameters.layer.skewAlongCoverageWindDirection;
	
	vec3 weatherData = textureLod(textureWeatherMap, pos /* parameters.layer.weatherScale*/, 0.0).xyz;
	
    // Apply effects for coverage
	weatherData.x = remapClamped(weatherData.x * parameters.CoverageAmount, 0.0, 1.0, parameters.CoverageMinimum, 1.0);
	weatherData.y = remapClamped(weatherData.y * parameters.TypeAmount, 0.0, 1.0, parameters.TypeMinimum, 1.0);
	weatherData.z = remapClamped(weatherData.z * parameters.RainAmount, 0.0, 1.0, parameters.RainMinimum, 1.0);
	
	return weatherData;
}

bool ValidCloudDensity(float heightFraction, vec3 weatherData, const in VolumetricCloudLayer parameters){
	vec3 weatherTypeMask = GetWeatherTypeMask(weatherData);	
	vec4 heightGradient = GetHeightGradientType(weatherTypeMask, parameters);
	float coverage = weatherData.x * SampleAnvilDeformation(heightFraction, weatherData, parameters);
	return (coverage > 0.05) && (heightGradient.x < heightFraction) && (heightGradient.w > heightFraction);
}

bool ValidCloudDensityLayers(float heightFraction, vec3 weatherDataFirst, vec3 weatherDataSecond, const in VolumetricCloudLayer parametersFirst, const in VolumetricCloudLayer parametersSecond){
	bool validCloudDensityFirst = ValidCloudDensity(heightFraction, weatherDataFirst, parametersFirst);
	bool validCloudDensitySecond = ValidCloudDensity(heightFraction, weatherDataSecond, parametersSecond);
	return validCloudDensityFirst || validCloudDensitySecond;
}

vec3 DecodeCurlNoise(vec3 c){
	return fma(c, vec3(2.0), vec3(-1.0));
}

float SampleCloudDensity(const in sampler3D textureShapeNoise,
                         const in sampler3D textureDetailNoise, 
                         const in sampler3D textureCurlNoise, 
                         vec3 p, 
                         float heightFraction, 
                         const in VolumetricCloudLayer parameters, 
                         vec3 weatherData, 
                         float lod, 
                         bool sampleDetail){
  
  if (!ValidCloudDensity(heightFraction, weatherData, parameters)){
		return 0.0;
	}

  mat3 tangentSpaceBasis = getTangentSpaceFromNormal(normalize(p)) * length(p);

  vec3 lowFrequencyPos = tangentSpaceBasis[2] + (tangentSpaceBasis[0] / 3.0) + (tangentSpaceBasis[1] / 7.0);
	//lowFrequencyPos += (tangentSpaceBasis[0] * parameters.WindOffset.x) + (tangentSpaceBasis[1] * parameters.WindOffset.y);
	//lowFrequencyPos += heightFraction * parameters.windDirection * parameters.layer.skewAlongWindDirection;
	
	vec4 lowFrequencyNoises = texture(textureShapeNoise, lowFrequencyPos * parameters.TotalNoiseScale, lod);

	float lowFrequencyFBM = clamp(dot(lowFrequencyNoises.yzw, vec3(0.625, 0.25, 0.125)), 0.0, 1.0);

	float cloudSample = remap(lowFrequencyNoises.x, -(1.0 - lowFrequencyFBM), 1.0, 0.0, 1.0);

	float densityHeightGradient = SampleHeightGradient(heightFraction, weatherData, parameters);
	cloudSample *= densityHeightGradient;

	float cloudCoverage = weatherData.x;
	
	cloudSample = remap(cloudSample, 1.0 - cloudCoverage, 1.0, 0.0, 1.0);
	cloudSample *= cloudCoverage;

	float densityAnvilDeformation = SampleAnvilDeformation(heightFraction, weatherData, parameters);
	cloudSample *= densityAnvilDeformation;

	if((cloudSample > 0.0) && sampleDetail){
		vec3 highFrequencyPos = p;// + parameters.windOffset;
		//highFrequencyPos += heightFraction * parameters.windDirection * parameters.layer.skewAlongWindDirection;
		
		vec3 curlNoise = DecodeCurlNoise(texture(textureCurlNoise, p * parameters.CurlScale * parameters.TotalNoiseScale, lod).xyz);
		highFrequencyPos += vec3(curlNoise.xzy) * clamp(pow(1.0 - heightFraction, parameters.CurlNoiseHeightFraction), 0.0, 1.0) * parameters.CurlNoiseModifier;

		vec3 highFrequencyNoises = texture(textureDetailNoise, highFrequencyPos * parameters.DetailScale * parameters.TotalNoiseScale, lod).xyz;
    
    float highFrequencyFBM = clamp(dot(highFrequencyNoises.xyz, vec3(0.625, 0.25, 0.125)), 0.0, 1.0);
		
		float highFrequenceNoiseModifier = mix(1.0 - highFrequencyFBM, highFrequencyFBM, clamp(heightFraction * parameters.DetailNoiseHeightFraction, 0.0, 1.0));
        
		cloudSample = remap(cloudSample, highFrequenceNoiseModifier * parameters.DetailNoiseModifier, 1.0, 0.0, 1.0);
	}
	
	return max(cloudSample, 0.0);
}

vec3 SampleAlbedo(float densityFirst, float densitySecond, vec3 weatherDataFirst, vec3 weatherDataSecond){
	vec3 albedoFirst = densityFirst * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Layers[0].Albedo.xyz;
	albedoFirst = pow(clamp(albedoFirst * vec3(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.BeerPowder), vec3(0.0), vec3(1.0)), vec3(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.BeerPowderPower));
	albedoFirst *= (1.0 - weatherDataFirst.z);
	
	vec3 albedoSecond = densitySecond * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Layers[1].Albedo.xyz;
	albedoSecond = pow(clamp(albedoSecond * vec3(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.BeerPowder), vec3(0.0), vec3(1.0)), vec3(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.BeerPowderPower));
	albedoSecond *= (1.0 - weatherDataSecond.z);

	return clamp(albedoFirst + albedoSecond, vec3(0.0), vec3(1.0));
}

vec3 SampleExtinction(float densityFirst, float densitySecond){
	vec3 extinctionFirst = densityFirst * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Layers[0].ExtinctionCoefficient.xyz;
	vec3 extinctionSecond = densitySecond * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.Layers[1].ExtinctionCoefficient.xyz;	
	return clamp(extinctionFirst + extinctionSecond, vec3(0.0), vec3(1.0));
}

void VolumetricShadow(inout ParticipatingMedia participatingMedia, in AtmosphereParameters atmosphereParameters, vec3 worldPosition, vec3 sunDirection, const in VolumetricCloudLayer layerParametersFirst, const in VolumetricCloudLayer layerParametersSecond, float lod){
	
	vec3 extinctionAccumulation[MS_COUNT];

	[[unroll]] for(int ms = 0; ms < MS_COUNT; ms++){
		extinctionAccumulation[ms] = vec3(0.0);
	}
	
	const float sampleCount = SHADOW_SAMPLE_COUNT;
	const float sampleSegmentT = 0.5;
	
	float lodOffset = 0.5;
	for(float s = 0.0; s < sampleCount; s += 1.0){

		float t0 = (s) / sampleCount;
		float t1 = (s + 1.0) / sampleCount;

		t0 *= t0;

		t1 *= t1;

		float delta = t1 - t0;
		
    float t = fma(delta, sampleSegmentT, t0);
		
		float shadowSampleT = atmosphereParameters.VolumetricClouds.ShadowStepLength * t;
		vec3 samplePoint = fma(sunDirection, vec3(shadowSampleT), worldPosition);

		float heightFraction = GetHeightFractionForPoint(atmosphereParameters, samplePoint);
		if((heightFraction < 0.0) || (heightFraction > 1.0)){
			break;
		}
		
		vec3 weatherDataFirst = SampleWeather(uCloudTextureWeatherMap0, samplePoint, heightFraction, layerParametersFirst);
		vec3 weatherDataSecond = SampleWeather(uCloudTextureWeatherMap1, samplePoint, heightFraction, layerParametersSecond);
		
		if(!ValidCloudDensityLayers(heightFraction, weatherDataFirst, weatherDataSecond, layerParametersFirst, layerParametersSecond)){
			continue;
		}

		float shadowCloudDensityFirst = SampleCloudDensity(uCloudTextureShapeNoise, uCloudTextureDetailNoise, uCloudTextureCurlNoise, samplePoint, heightFraction, layerParametersFirst, weatherDataFirst, lod + lodOffset, true);
		float shadowCloudDensitySecond = SampleCloudDensity(uCloudTextureShapeNoise, uCloudTextureDetailNoise, uCloudTextureCurlNoise, samplePoint, heightFraction, layerParametersSecond, weatherDataSecond, lod + lodOffset, true);
    vec3 shadowExtinction = SampleExtinction(shadowCloudDensityFirst, shadowCloudDensitySecond);
		
		ParticipatingMedia shadowParticipatingMedia = SampleParticipatingMedia(vec3(0.0), 
                                                                           shadowExtinction, 
                                                                           atmosphereParameters.VolumetricClouds.MultiScatteringScattering, 
                                                                           atmosphereParameters.VolumetricClouds.MultiScatteringExtinction, 
                                                                           vec3(0.0));
		    
		[[unroll]] for (int ms = 0; ms < MS_COUNT; ms++){
			extinctionAccumulation[ms] += shadowParticipatingMedia.extinctionCoefficients[ms] * delta;
		}

		lodOffset += 0.5;
	}

	[[unroll]] for (int ms = 0; ms < MS_COUNT; ms++){
		participatingMedia.transmittanceToLight[ms] *= exp(-extinctionAccumulation[ms] * atmosphereParameters.VolumetricClouds.ShadowStepLength);
	}

}

void VolumetricGroundContribution(inout vec3 environmentLuminance, const in AtmosphereParameters atmosphere, vec3 worldPosition, vec3 sunDirection, vec3 sunIlluminance, vec3 atmosphereTransmittanceToLight, const in VolumetricCloudLayer layerParametersFirst, const in VolumetricCloudLayer layerParametersSecond, float lod){
	float planetRadius = atmosphere.BottomRadius;
	vec3 planetCenterWorld = vec3(0.0);//atmosphere.inverseTransform[3].xyz;

	float cloudBottomRadius = planetRadius + atmosphere.VolumetricClouds.CloudStartHeight;

	float cloudSampleAltitudde = length(worldPosition - planetCenterWorld);
	float cloudSampleHeightToBottom = cloudSampleAltitudde - cloudBottomRadius; 
  
	vec3 opticalDepth = vec3(0.0);
	
	const float contributionStepLength = min(4000.0, cloudSampleHeightToBottom);
	const vec3 groundScatterDirection = vec3(0.0, -1.0, 0.0);
	
	const float sampleCount = GROUND_CONTRIBUTION_SAMPLE_COUNT;
	const float sampleSegmentT = 0.5;
	
	float lodOffset = 0.5;
	for(float s = 0.0f; s < sampleCount; s += 1.0){

		float t0 = (s) / sampleCount;
		float t1 = (s + 1.0) / sampleCount;

		t0 *= t0;
		t1 *= t1;

		float delta = t1 - t0; 
		float t = t0 + (t1 - t0) * sampleSegmentT; 

		float contributionSampleT = contributionStepLength * t;
		vec3 samplePoint = worldPosition + groundScatterDirection * contributionSampleT; 

		float heightFraction = GetHeightFractionForPoint(atmosphere, samplePoint);
		
		vec3 weatherDataFirst = SampleWeather(uCloudWeatherMapTextures[0], samplePoint, heightFraction, layerParametersFirst);
    vec3 weatherDataSecond = SampleWeather(uCloudWeatherMapTextures[1], samplePoint, heightFraction, layerParametersSecond);
      
		if (!ValidCloudDensityLayers(heightFraction, weatherDataFirst, weatherDataSecond, layerParametersFirst, layerParametersSecond))
		{
			continue;
		}

		float contributionCloudDensityFirst = SampleCloudDensity(uCloudTextureShapeNoise, uCloudTextureDetailNoise, uCloudTextureCurlNoise, samplePoint, heightFraction, layerParametersFirst, weatherDataFirst, lod + lodOffset, true);
		float contributionCloudDensitySecond = SampleCloudDensity(uCloudTextureShapeNoise, uCloudTextureDetailNoise, uCloudTextureCurlNoise, samplePoint, heightFraction, layerParametersSecond, weatherDataSecond, lod + lodOffset, true);
		vec3 contributionExtinction = SampleExtinction(contributionCloudDensityFirst, contributionCloudDensitySecond);

		opticalDepth += contributionExtinction * contributionStepLength * delta;
		
		lodOffset += 0.5;
	}
	
	const vec3 planetSurfaceNormal = vec3(0.0, 1.0, 0.0); 
	const vec3 groundBrdfNdotL = clamp(dot(sunDirection, planetSurfaceNormal), 0.0, 1.0) * (atmosphere.GroundAlbedo.xyz / PI); 

	const float uniformPhase = 1.0 / (4.0 * PI);
	const float groundHemisphereLuminanceIsotropic = (2.0 * PI) * uniformPhase; 
	const vec3 groundToCloudTransfertIsoScatter = groundBrdfNdotL * groundHemisphereLuminanceIsotropic;
	
	const vec3 scatteredLuminance = atmosphereTransmittanceToLight * sunIlluminance * groundToCloudTransfertIsoScatter;

	environmentLuminance += scatteredLuminance * exp(-opticalDepth);
}

struct ParticipatingMediaPhase {
	float phase[MS_COUNT];
};

ParticipatingMediaPhase SampleParticipatingMediaPhase(float basePhase, float baseMsPhaseFactor){
	ParticipatingMediaPhase participatingMediaPhase;
	participatingMediaPhase.phase[0] = basePhase;

  // 1.0 / (4.0 * PI) = 0.07957747154594767
	const float uniformPhase = 0.07957747154594767;

	float MsPhaseFactor = baseMsPhaseFactor;
	
	[[unroll]]for(int ms = 1; ms < MS_COUNT; ms++){
		participatingMediaPhase.phase[ms] = mix(uniformPhase, participatingMediaPhase.phase[0], MsPhaseFactor);
		MsPhaseFactor *= MsPhaseFactor;
	}

	return participatingMediaPhase;
}

float ExponentialIntegral(float x){    
	return 0.5772156649015328606065 + log(1e-4 + abs(x)) + x * (1.0 + x * (0.25 + x * ((1.0 / 18.0) + x * ((1.0 / 96.0) + x * (1.0 / 600.0)))));
}

vec3 SampleAmbientLight(vec3 worldPosition, float heightFraction){
	//float ambientTerm = -cloudDensity * (1.0 - clamp(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.AmbientGroundMultiplier + heightFraction, 0.0, 1.0));
	//float isotropicScatteringTopContribution = max(0.0, exp(ambientTerm) - ambientTerm * ExponentialIntegral(ambientTerm));
  vec3 p = normalize(worldPosition);
	vec3 skyLuminance = textureLod(uCloudTextureSkyLuminance, fma(vec2(atan(p.z, p.x), acos(p.y)), vec2(0.15915494309189535, 0.3183098861837907), vec2(0.5, 0.0)), 0.0).xyz;
	float isotropicScatteringTopContribution = clamp(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.AmbientGroundMultiplier + heightFraction, 0.0, 1.0);
  return skyLuminance * isotropicScatteringTopContribution;
}

vec3 SampleLocalLights(vec3 worldPosition){
  return vec3(0.0);
} 

void VolumetricCloudLighting(const in AtmosphereParameters atmosphereParameters, 
                             vec3 startPosition, 
                             vec3 worldPosition, 
                             vec3 sunDirection, 
                             vec3 sunIlluminance, 
                             float cosTheta, 
                             float stepSize, 
                             float heightFraction,
                             float cloudDensityFirst, 
                             float cloudDensitySecond, 
                             vec3 weatherDataFirst, 
                             vec3 weatherDataSecond,
                             const in VolumetricCloudLayer layerParametersFirst, 
                             const in VolumetricCloudLayer layerParametersSecond, 
                             float lod,
                             inout vec3 luminance, 
                             inout vec3 transmittanceToView, 
                             inout float depthWeightedSum, 
                             inout float depthWeightsSum){
	
	vec3 albedo = SampleAlbedo(cloudDensityFirst, cloudDensitySecond, weatherDataFirst, weatherDataSecond);
	vec3 extinction = SampleExtinction(cloudDensityFirst, cloudDensitySecond);
	
	vec3 atmosphereTransmittanceToLight = GetAtmosphereTransmittance(atmosphereParameters, uCloudTextureTransmittanceLUT, worldPosition, sunDirection); 
	
	ParticipatingMedia participatingMedia = SampleParticipatingMedia(albedo, extinction, atmosphereParameters.VolumetricClouds.MultiScatteringScattering, atmosphereParameters.VolumetricClouds.MultiScatteringExtinction, atmosphereTransmittanceToLight);
	
	vec3 environmentLuminance = SampleAmbientLight(worldPosition, heightFraction);

	if(any(greaterThan(participatingMedia.scatteringCoefficients[0], vec3(0.0)))){
  	OpaqueShadow(participatingMedia, worldPosition);
		
		// Calcualte volumetric shadow
		VolumetricShadow(participatingMedia, atmosphereParameters, worldPosition, sunDirection, layerParametersFirst, layerParametersSecond, lod);

		// Calculate bounced light from ground onto clouds
		const float maxTransmittanceToView = max(max(transmittanceToView.x, transmittanceToView.y), transmittanceToView.z);
		if(maxTransmittanceToView > 0.01){
			VolumetricGroundContribution(environmentLuminance, atmosphereParameters, worldPosition, sunDirection, sunIlluminance, atmosphereTransmittanceToLight, layerParametersFirst, layerParametersSecond, lod);
		}
	}

	float phaseFunction = DualLobPhase(atmosphereParameters.VolumetricClouds.PhaseG, atmosphereParameters.VolumetricClouds.PhaseG2, atmosphereParameters.VolumetricClouds.PhaseBlend, -cosTheta);
  ParticipatingMediaPhase participatingMediaPhase = SampleParticipatingMediaPhase(phaseFunction, atmosphereParameters.VolumetricClouds.MultiScatteringEccentricity);

	float depthWeight = min(transmittanceToView.x, min(transmittanceToView.y, transmittanceToView.z));
	depthWeightedSum += depthWeight * length(worldPosition - startPosition);
	depthWeightsSum += depthWeight;

	vec3 localLightLuminance = SampleLocalLights(worldPosition);

	[[unroll]] for (int ms = MS_COUNT - 1; ms >= 0; ms--){
		const vec3 scatteringCoefficients = participatingMedia.scatteringCoefficients[ms];
		const vec3 extinctionCoefficients = participatingMedia.extinctionCoefficients[ms];
		const vec3 transmittanceToLight = participatingMedia.transmittanceToLight[ms];
		
		vec3 lightLuminance = transmittanceToLight * sunIlluminance * participatingMediaPhase.phase[ms];
		lightLuminance += ((ms == 0) ? environmentLuminance : vec3(0.0)) + localLightLuminance;

		const vec3 scatteredLuminance = lightLuminance * scatteringCoefficients;
		
		// See slide 28 at http://www.frostbite.com/2015/08/physically-based-unified-volumetric-rendering-in-frostbite/ 
		const vec3 clampedExtinctionCoefficients = max(extinctionCoefficients, 1e-7);
		const vec3 sampleTransmittance = exp(-clampedExtinctionCoefficients * stepSize);
		vec3 luminanceIntegral = (scatteredLuminance - scatteredLuminance * sampleTransmittance) / clampedExtinctionCoefficients;
		luminance += transmittanceToView * luminanceIntegral; 

		if(ms == 0){
			transmittanceToView *= sampleTransmittance;
		}
	}

}

float CalculateAtmosphereBlend(float tDepth){
  float fogDistance = clamp(tDepth * uAtmosphereParameters.atmosphereParameters.VolumetricClouds.HorizonBlendAmount, 0.0, 1.0);
	return clamp(smoothstep(0.0, 1.0, pow(fogDistance, uAtmosphereParameters.atmosphereParameters.VolumetricClouds.HorizonBlendPower)), 0.0, 1.0);        
}
 
void RenderClouds(const in AtmosphereParameters atmosphere,
                  uvec3 DTid, 
                  vec2 uv, 
                  float depth, 
                  float farDepth,
                  vec3 depthWorldPosition, 
                  vec3 cameraWorldPosition,
                  vec3 rayOrigin, 
                  vec3 rayDirection, 
                  inout vec4 cloudColor, 
                  inout vec2 cloudDepth){	
	
  const float FLT_MAX = 3.402823466e+38;

	float tMin = -FLT_MAX;
	float tMax = -FLT_MAX;
	float t;
	float tToDepthBuffer;
	float steps;
	float stepSize;
  {
		float planetRadius = atmosphere.BottomRadius;
		vec3 planetCenterWorld = vec3(0.0); //atmosphere.inverseTransform[3].xyz;

		const float cloudBottomRadius = planetRadius + atmosphere.VolumetricClouds.CloudStartHeight;
		const float cloudTopRadius = planetRadius + atmosphere.VolumetricClouds.CloudStartHeight + atmosphere.VolumetricClouds.CloudThickness;
        
		vec2 tTopSolutions = raySphereIntersect(rayOrigin, rayDirection, planetCenterWorld, cloudTopRadius);
		if(any(greaterThan(tTopSolutions, vec2(0.0)))){
			vec2 tBottomSolutions = raySphereIntersect(rayOrigin, rayDirection, planetCenterWorld, cloudBottomRadius);
  		if(any(greaterThan(tBottomSolutions, vec2(0.0)))){
               
				float tempTop = all(greaterThan(tTopSolutions, vec2(0.0))) ? min(tTopSolutions.x, tTopSolutions.y) : max(tTopSolutions.x, tTopSolutions.y);
				float tempBottom = all(greaterThan(tBottomSolutions, vec2(0.0))) ? min(tBottomSolutions.x, tBottomSolutions.y) : max(tBottomSolutions.x, tBottomSolutions.y);
                
				if(all(greaterThan(tBottomSolutions, vec2(0.0)))){
					tempTop = max(0.0f, min(tTopSolutions.x, tTopSolutions.y));
				}

				tMin = min(tempBottom, tempTop);
				tMax = max(tempBottom, tempTop);
      }else{
				tMin = tTopSolutions.x;
				tMax = tTopSolutions.y;
			}
            
			tMin = max(0.0, tMin);
			tMax = max(0.0, tMax);
		}else{
			cloudColor = vec4(0.0, 0.0, 0.0, 0.0);
			cloudDepth = vec2(FLT_MAX);
			return;
		}

		if((tMax <= tMin) || (tMin > atmosphere.VolumetricClouds.RenderDistance)){
			cloudColor = vec4(0.0, 0.0, 0.0, 0.0);
			cloudDepth = vec2(FLT_MAX);
			return;
		}
		
		tToDepthBuffer = length(depthWorldPosition - rayOrigin);

		tMax = (depth == farDepth) ? tMax : min(tMax, tToDepthBuffer);

		tToDepthBuffer = (depth == farDepth) ? FLT_MAX : tToDepthBuffer;
		
		const float marchingDistance = min(atmosphere.VolumetricClouds.MaxMarchingDistance, tMax - tMin);
		tMax = tMin + marchingDistance;

		steps = MAX_STEP_COUNT * clamp((tMax - tMin) * (1.0 / atmosphere.VolumetricClouds.InverseDistanceStepCount), 0.0, 1.0);
		stepSize = (tMax - tMin) / steps;

		float offset = 0.5; // blue-noise offset later

		t = fma(offset, stepSize, tMin);
	}
	
  const VolumetricCloudLayer layerParametersFirst = atmosphere.VolumetricClouds.Layers[0];
	const VolumetricCloudLayer layerParametersSecond = atmosphere.VolumetricClouds.Layers[1];
	
	vec3 sunIlluminance = atmosphere.SolarIlluminance.xyz;
  vec3 sunDirection = getSunDirection(atmosphere);
	
	float cosTheta = dot(rayDirection, sunDirection);
	
	vec3 luminance = vec3(0.0);
	vec3 transmittanceToView = vec3(1.0);
	float depthWeightedSum = 0.0;
	float depthWeightsSum = 0.0;
	
	[[loop]] for (float i = 0.0; i < steps; i++){
		
    vec3 sampleWorldPosition = fma(rayDirection, vec3(t), rayOrigin);

		float heightFraction = GetHeightFractionForPoint(atmosphere, sampleWorldPosition);
		if((heightFraction < 0.0) || (heightFraction > 1.0)){
			break;
		}
		
		vec3 weatherDataFirst = SampleWeather(uCloudWeatherMapTextures[0], sampleWorldPosition, heightFraction, layerParametersFirst);
    vec3 weatherDataSecond = SampleWeather(uCloudWeatherMapTextures[1], sampleWorldPosition, heightFraction, layerParametersSecond);
      
		if (!ValidCloudDensityLayers(heightFraction, weatherDataFirst, weatherDataSecond, layerParametersFirst, layerParametersSecond)){
      int bigStepMarch = int(atmosphere.VolumetricClouds.BigStepMarch); 
			i += bigStepMarch - 1;
			t += float(bigStepMarch) * stepSize;			
			continue;
		}
		
		float tProgress = distance(rayOrigin, sampleWorldPosition);
		float lod = round(tProgress / max(atmosphere.VolumetricClouds.LODDistance, 1e-5));
		lod = clamp(lod, LOD_Min, LOD_Max);
		
		float cloudDensityFirst = SampleCloudDensity(uCloudTextureShapeNoise, uCloudTextureDetailNoise, uCloudTextureCurlNoise, sampleWorldPosition, heightFraction, layerParametersFirst, weatherDataFirst, lod, true);
		float cloudDensitySecond = SampleCloudDensity(uCloudTextureShapeNoise, uCloudTextureDetailNoise, uCloudTextureCurlNoise, sampleWorldPosition, heightFraction, layerParametersSecond, weatherDataSecond, lod, true);

		if (clamp(cloudDensityFirst + cloudDensitySecond, 0.0, 1.0) > CLOUD_DENSITY_THRESHOLD){

			VolumetricCloudLighting(
        atmosphere, 
        rayOrigin, 
        sampleWorldPosition, 
        sunDirection, 
        sunIlluminance, 
        cosTheta, 
        stepSize, 
        heightFraction,
				cloudDensityFirst, 
        cloudDensitySecond, 
        weatherDataFirst, 
        weatherDataSecond,
				layerParametersFirst, 
        layerParametersSecond, 
        lod,
				luminance, 
        transmittanceToView, 
        depthWeightedSum, 
        depthWeightsSum
      );
			
			if (all(lessThan(transmittanceToView, vec3(atmosphere.VolumetricClouds.TransmittanceThreshold)))){
				break;
			}

		}

		t += stepSize;
	}
		
	
	float tDepth = (depthWeightsSum == 0.0) ? tMax : depthWeightedSum / max(depthWeightsSum, 1e-10);
	vec3 sampleWorldPosition = fma(rayDirection, vec3(tDepth), rayOrigin);

	float approxTransmittance = dot(transmittanceToView.xyz, vec3(1.0 / 3.0));
	float grayScaleTransmittance = (approxTransmittance < atmosphere.VolumetricClouds.TransmittanceThreshold) ? 0.0 : approxTransmittance;
	
	// Apply aerial perspective if any clouds is detected (depthWeightsSum)
	if(depthWeightsSum > 0.0){

		vec4 aerialPerspective = vec4(0.0);
		
    if((pushConstants.flags & FLAGS_USE_FAST_AERIAL_PERSPECTIVE) == 0u){

			vec3 worldPosition = (atmosphere.inverseTransform * vec4(sampleWorldPosition, 1.0)).xyz;
			vec3 worldDirection = rayDirection;
		
			// Move to top atmosphere as the starting point for ray marching.
			// This is critical to be after the above to not disrupt above atmosphere tests and voxel selection.
			if(MoveToTopAtmosphere(worldPosition, worldDirection, atmosphere.TopRadius)){
				const float sampleCountIni = 0.0;
				const bool variableSampleCount = true;
#ifdef VOLUMETRICCLOUD_CAPTURE
				const bool perPixelNoise = false;
#else
				const bool perPixelNoise = true;
#endif
				const bool opaque = true;
				const bool ground = false;
				const bool mieRayPhase = true;
				const bool multiScatteringApprox = true;
				const bool volumetricCloudShadow = ((pushConstants.flags & FLAGS_SHADOWS) != 0u) && ((pushConstants.flags & FLAGS_USE_FAST_AERIAL_PERSPECTIVE) == 0u);
				const bool opaqueShadow = (pushConstants.flags & FLAGS_SHADOWS) != 0u;
				//const float opticalDepthScale = atmosphere.aerialPerspectiveScale;
		 	 /*SingleScatteringResult ss = IntegrateScatteredLuminance(
				  atmosphere, 
          DTid.xy, 
          worldPosition, 
          worldDirection, 
          sunDirection, 
          sunIlluminance, 
          tDepth, 
          sampleCountIni, 
          variableSampleCount,
          perPixelNoise, 
          opaque, 
          ground, 
          mieRayPhase, 
          multiScatteringApprox, 
          volumetricCloudShadow, 
          opaqueShadow, 
          texture_transmittancelut, 
          texture_multiscatteringlut, 
          opticalDepthScale
        );
				
				float transmittance = 1.0 - dot(ss.transmittance, vec3(1.0 / 3.0));
				aerialPerspective = vec4(ss.L, transmittance);*/
			}
		}else{
		//	aerialPerspective = GetAerialPerspectiveTransmittance(uv, sampleWorldPosition, rayOrigin, texture_cameravolumelut);
		}

		luminance = (1.0 - aerialPerspective.a) * luminance + aerialPerspective.rgb * (1.0 - approxTransmittance);
	}
	
/*if(depthWeightsSum > 0.0){
		vec4 fog = GetFog(tDepth, rayOrigin, rayDirection);
		//luminance = (1.0 - fog.w) * luminance + fog.xyz * (1.0 - approxTransmittance); 
		luminance = mix(luminance, fog.xyz * (1.0 - approxTransmittance), fog.w);
	}*/

	vec4 color = vec4(luminance, 1.0 - grayScaleTransmittance);

	if(depthWeightsSum > 0.0){
		float atmosphereBlend = CalculateAtmosphereBlend(tDepth);
		color *= 1.0 - atmosphereBlend;
	}

	// Output
	cloudColor = color;
	cloudDepth = vec2(tDepth, tToDepthBuffer); // Linear depth
}

void main(){

}