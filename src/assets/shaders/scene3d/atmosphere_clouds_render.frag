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

layout(set = 2, binding = 3) uniform samplerCube uCloudWeatherMapTextures[];

#define uCloudTextureWeatherMap0 uCloudWeatherMapTextures[0]
#define uCloudTextureWeatherMap1 uCloudWeatherMapTextures[1]


void main(){

}