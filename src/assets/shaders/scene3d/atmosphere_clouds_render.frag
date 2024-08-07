#version 460 core

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

layout(set = 2, binding = 3, std140) uniform uboCascadedShadowMaps {
  mat4 shadowMapMatrices[NUM_SHADOW_CASCADES];
  vec4 shadowMapSplitDepthsScales[NUM_SHADOW_CASCADES];
  vec4 constantBiasNormalBiasSlopeBiasClamp[NUM_SHADOW_CASCADES];
  uvec4 metaData; // x = type
} uCascadedShadowMaps;

layout(set = 2, binding = 4) uniform sampler2DArray uCascadedShadowMapTexture;

#ifdef PCFPCSS

// Yay! Binding Aliasing! :-)
layout(set = 2, binding = 4) uniform sampler2DArrayShadow uCascadedShadowMapTextureShadow;

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

layout(set = 2, binding = 1) uniform sampler3D uCloudTextures[];

#define uCloudTextureShapeNoise uCloudTextures[0]
#define uCloudTextureDetailNoise uCloudTextures[1]
#define uCloudTextureCurlNoise uCloudTextures[2]

layout(set = 2, binding = 2) uniform samplerCube uCloudWeatherMapTextures[];

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
	vec3 planetCenterWorld = atmosphereParameters.inverseTransform[3].xyz;
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

vec3 SampleAmbientLight(float heightFraction){
	//float ambientTerm = -cloudDensity * (1.0 - saturate(GetWeather().volumetric_clouds.CloudAmbientGroundMultiplier + heightFraction));
	//float isotropicScatteringTopContribution = max(0.0, exp(ambientTerm) - ambientTerm * ExponentialIntegral(ambientTerm));
	float isotropicScatteringTopContribution = clamp(uAtmosphereParameters.atmosphereParameters.VolumetricClouds.AmbientGroundMultiplier + heightFraction, 0.0, 1.0);
	vec3 skyLuminance = vec3(1.0);
  return isotropicScatteringTopContribution * skyLuminance;
}

vec3 SampleLocalLights(vec3 worldPosition){
  return vec3(0.0);
} 

void main(){

}