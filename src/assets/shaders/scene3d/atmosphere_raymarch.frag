#version 460 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_multiview : enable
#extension GL_EXT_samplerless_texture_functions : enable
#extension GL_EXT_nonuniform_qualifier : enable
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

layout(set = 2, binding = 7, std140) uniform uboCascadedShadowMaps {
  mat4 shadowMapMatrices[NUM_SHADOW_CASCADES];
  vec4 shadowMapSplitDepthsScales[NUM_SHADOW_CASCADES];
  vec4 constantBiasNormalBiasSlopeBiasClamp[NUM_SHADOW_CASCADES];
  uvec4 metaData; // x = type
} uCascadedShadowMaps;

layout(set = 2, binding = 8) uniform sampler2DArray uCascadedShadowMapTexture;

#ifdef PCFPCSS

// Yay! Binding Aliasing! :-)
layout(set = 2, binding = 8) uniform sampler2DArrayShadow uCascadedShadowMapTextureShadow;

#endif // PCFPCSS
#endif // !RAYTRACING 

vec3 inWorldSpacePosition, workNormal;
#endif // SHADOWS

#include "shadows.glsl"

#include "atmosphere_common.glsl"

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outInscattering;

#ifdef DUALBLEND
layout(location = 1) out vec4 outTransmittance; // component-wise transmittance
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

/*
#ifdef MSAA
layout(input_attachment_index = 0, set = 2, binding = 0) uniform subpassInputMS uSubpassDepth;
#else  
layout(input_attachment_index = 0, set = 2, binding = 0) uniform subpassInput uSubpassDepth;
#endif
*/

layout(set = 2, binding = 1) uniform sampler2D uTransmittanceLutTexture;

layout(set = 2, binding = 2) uniform sampler2D uMultiScatTexture;

layout(set = 2, binding = 3) uniform sampler2DArray uSkyViewLUT;

layout(set = 2, binding = 4) uniform sampler2DArray uCameraVolume;

layout(set = 2, binding = 5) uniform sampler2D uBlueNoise;

layout(set = 2, binding = 6, std430) buffer AtmosphereParametersBuffer {
  AtmosphereParameters atmosphereParameters;
} uAtmosphereParameters;

#include "projectsphere.glsl"

#include "textureutils.glsl"

void main() {

  int viewIndex = pushConstants.baseViewIndex + int(gl_ViewIndex);
  View view = uView.views[viewIndex];

/*vec2 pixPos = vec2(gl_FragCoord.xy) + vec2(0.5);
  vec2 uv = pixPos / pushConstants.resolution;*/

  vec2 uv = inTexCoord; 

  if((pushConstants.flags & FLAGS_USE_BLUE_NOISE) != 0u){
    seedSampleSeedT(uBlueNoise, ivec2(gl_FragCoord.xy), pushConstants.frameIndex);
  }

  vec3 worldPos, worldDir;
  GetCameraPositionDirection(worldPos, worldDir, view.viewMatrix, view.projectionMatrix, view.inverseViewMatrix, view.inverseProjectionMatrix, uv);
  
  worldPos = (uAtmosphereParameters.atmosphereParameters.inverseTransform * vec4(worldPos, 1.0)).xyz;

  vec3 originalWorldPos = worldPos; 

  //worldPos += vec3(0.0, uAtmosphereParameters.atmosphereParameters.BottomRadius, 0.0);

  float viewHeight = max(length(worldPos), uAtmosphereParameters.atmosphereParameters.BottomRadius + 1e-4);  
  vec3 L = vec3(0.0);
/*  
#ifdef MSAA
  float depthBufferValue = subpassLoad(uSubpassDepth, gl_SampleID).x;
#else  
  float depthBufferValue = subpassLoad(uSubpassDepth).x;
#endif*/
#ifdef MSAA
#ifdef MULTIVIEW
  float depthBufferValue = texelFetch(uDepthTexture, ivec3(ivec2(gl_FragCoord.xy), int(gl_ViewIndex)), gl_SampleID).x;
#else
  float depthBufferValue = texelFetch(uDepthTexture, ivec2(gl_FragCoord.xy), gl_SampleID).x;
#endif
#else
#ifdef MULTIVIEW
  float depthBufferValue = texelFetch(uDepthTexture, ivec3(ivec2(gl_FragCoord.xy), int(gl_ViewIndex)), 0).x;
#else
  float depthBufferValue = texelFetch(uDepthTexture, ivec2(gl_FragCoord.xy), 0).x;
#endif
#endif

  vec3 sunDirection = normalize(getSunDirection(uAtmosphereParameters.atmosphereParameters));

#ifdef SHADOWS
  lightDirection = -sunDirection;
#endif

  bool depthIsZFar = depthBufferValue == GetZFarDepthValue(view.projectionMatrix);

  //bool rayHitsAtmosphere = any(greaterThanEqual(raySphereIntersect(worldPos, worldDir, vec3(0.0), atmosphereParameters.TopRadius), vec2(0.0)));

  if(/*rayHitsAtmosphere &&*/ depthIsZFar){

    if((pushConstants.flags & FLAGS_USE_FAST_SKY) != 0u){

      vec2 localUV;
      vec3 UpVector = normalize(worldPos);
      float viewZenithCosAngle = dot(worldDir, UpVector);

      vec3 sideVector = normalize(cross(UpVector, worldDir));		// assumes non parallel vectors
      vec3 forwardVector = normalize(cross(sideVector, UpVector));	// aligns toward the sun light but perpendicular to up vector
      vec2 lightOnPlane = vec2(dot(sunDirection, forwardVector), dot(sunDirection, sideVector));
      lightOnPlane = normalize(lightOnPlane);
      float lightViewCosAngle = lightOnPlane.x;

      bool IntersectGround = raySphereIntersectNearest(worldPos, worldDir, vec3(0.0), uAtmosphereParameters.atmosphereParameters.BottomRadius) >= 0.0;
  
      SkyViewLutParamsToUv(uAtmosphereParameters.atmosphereParameters, IntersectGround, viewZenithCosAngle, lightViewCosAngle, viewHeight, localUV);

#if 0
      localUV = getNiceTextureUV(localUV, vec2(textureSize(uSkyViewLUT, 0).xy));
#endif      

      vec4 inscattering = textureLod(uSkyViewLUT, vec3(localUV, float(viewIndex)), 0.0).xyzw; // xyz = inscatter, w = transmittance (monochromatic)

#ifdef DUALBLEND
      vec3 transmittance = textureLod(uSkyViewLUT, vec3(localUV, float(int(viewIndex + pushConstants.countViews))), 0.0).xyz; // xyz = transmittance, w = non-used
#else
      vec3 transmittance = vec3(inscattering.w); // convert from monochromatic transmittance, not optimal but better than nothing 
#endif

      if(!IntersectGround){
        inscattering.xyz += GetSunLuminance(originalWorldPos, worldDir, sunDirection, uAtmosphereParameters.atmosphereParameters.BottomRadius).xyz * transmittance.xyz;
      }

#ifdef DUALBLEND
      outInscattering = vec4(inscattering.xyz, 1.0);

      outTransmittance = vec4(transmittance.xyz, 1.0);
#else      
      outInscattering = vec4(inscattering.xyz, 1.0 - inscattering.w); // alpha = 1.0 - transmittance 
#endif

      return; // Early out, for avoiding the code path of the more accurate and more bruteforce ray marching approach

    }

  }else{
   
    if((pushConstants.flags & FLAGS_USE_FAST_AERIAL_PERSPECTIVE) != 0u){

      // Fast aerial perspective approximation using a 3D texture

      // (BeRo): Check if we can use the fast aerial perspective approximation, given the camera volume constraints the planet in a way that
      // the voxel resolution is not too inaccurate
      bool fitsInCameraVolume = true;
      if(length(worldPos) >= uAtmosphereParameters.atmosphereParameters.TopRadius){

        vec4 aabb;     
        vec3 transformedCenter = ((view.viewMatrix * uAtmosphereParameters.atmosphereParameters.transform) * vec4(vec3(0.0), 1.0)).xyz;
        if(projectSphere(transformedCenter, uAtmosphereParameters.atmosphereParameters.TopRadius, 0.01, view.projectionMatrix, aabb, false)){

          // camera volume is 32x32 by width and height and 32 by depth, by default

          vec2 aabbSize = (aabb.zw - aabb.xy) * vec2(textureSize(uCameraVolume, 0).xy);

          fitsInCameraVolume = all(greaterThanEqual(aabbSize, vec2(4.0))); // 4x4 pixels minimum, otherwise the voxel resolution is too inaccurate

        }

      }  
  
      if(fitsInCameraVolume){

        // (BeRo): Move ray marching start up to top atmosphere, for to avoid missing the atmosphere in the special case of the camera being
        // far outside the atmosphere.
        //if(length(worldPos) >= uAtmosphereParameters.atmosphereParameters.TopRadius)
        {
          vec2 t = raySphereIntersect(worldPos, worldDir, vec3(0.0), uAtmosphereParameters.atmosphereParameters.TopRadius);
          if(all(greaterThanEqual(t, vec2(0.0)))){
            worldPos += worldDir * min(t.x, t.y);
          }
        }

        mat4 inverseViewProjectionMatrix = view.inverseViewMatrix * view.inverseProjectionMatrix;

        vec4 depthBufferWorldPos = inverseViewProjectionMatrix * vec4(fma(vec2(uv), vec2(2.0), vec2(-1.0)), depthBufferValue, 1.0);
        depthBufferWorldPos /= depthBufferWorldPos.w;

        float tDepth = length((uAtmosphereParameters.atmosphereParameters.inverseTransform * vec4(depthBufferWorldPos.xyz, 1.0)).xyz - worldPos);
        float slice = AerialPerspectiveDepthToSlice(tDepth);
        float Weight = 1.0;
        if(slice < 0.5){
          Weight = clamp(slice * 2.0, 0.0, 1.0);
          slice = 0.5;
        } 
        float w = sqrt(slice / AP_SLICE_COUNT); // squared distribution

#if 0
        vec3 uvw = getNiceTextureUVW(vec3(uv, w), vec3(textureSize(uCameraVolume, 0).xy, float(AP_SLICE_COUNT)));

        uv = uvw.xy;
        w = uvw.z;
#endif

        float baseSlice = w * AP_SLICE_COUNT;
        int sliceIndex = int(floor(baseSlice));
        float sliceWeight = baseSlice - float(sliceIndex);
        int nextSliceIndex = clamp(sliceIndex + 1, 0, AP_SLICE_COUNT_INT - 1);
        sliceIndex = clamp(sliceIndex, 0, AP_SLICE_COUNT_INT - 1);

        // Manual 3D texture lookup from a 2D array texture, since multiview is not supported for 3D textures (no 3D array textures) 
        vec4 inscattering = mix(
                          textureLod(uCameraVolume, vec3(uv, sliceIndex + (viewIndex * AP_SLICE_COUNT_INT)), 0.0),
                          textureLod(uCameraVolume, vec3(uv, nextSliceIndex + (viewIndex * AP_SLICE_COUNT_INT)), 0.0),
                          sliceWeight
                        ) * Weight;

#ifdef DUALBLEND
        vec3 transmittance = mix(
                              textureLod(uCameraVolume, vec3(uv, sliceIndex + ((viewIndex + pushConstants.countViews) * AP_SLICE_COUNT_INT)), 0.0).xyz,
                              textureLod(uCameraVolume, vec3(uv, nextSliceIndex + ((viewIndex + pushConstants.countViews) * AP_SLICE_COUNT_INT)), 0.0).xyz,
                              sliceWeight
                             ) * Weight;
#else
        vec3 transmittance = vec3(inscattering.w); // convert from monochromatic transmittance, not optimal but better than nothing
#endif

        if(depthIsZFar){
          inscattering.xyz += GetSunLuminance(originalWorldPos, worldDir, sunDirection, uAtmosphereParameters.atmosphereParameters.BottomRadius).xyz * transmittance.xyz;  
        }

#ifdef DUALBLEND
        outInscattering = vec4(inscattering.xyz, 1.0);

        outTransmittance = vec4(transmittance, 1.0);
#else
        outInscattering = vec4(inscattering.xyz, 1.0 - inscattering.w); // alpha = 1.0 - transmittance 
#endif
      
        return; // Early out, for avoiding the code path of the more accurate and more bruteforce ray marching approach

      }  

    }

  }

  {

    // The more accurate and more bruteforce ray marching approach  

    vec3 inscattering;
    vec3 transmittance; 

    // Move to top atmosphere as the starting point for ray marching.
    // This is critical to be after the above to not disrupt above atmosphere tests and voxel selection.
    if(!MoveToTopAtmosphere(worldPos, worldDir, uAtmosphereParameters.atmosphereParameters.TopRadius)){
      
      // Ray is not intersecting the atmosphere       
      inscattering = GetSunLuminance(originalWorldPos, worldDir, sunDirection, uAtmosphereParameters.atmosphereParameters.BottomRadius).xyz;
      transmittance = vec3(1.0);

    }else {

      mat4 skyInvViewProjMat = view.inverseViewMatrix * view.inverseProjectionMatrix; 
      const bool ground = false;
      const float sampleCountIni = 0.0;
      const bool variableSampleCount = true;
      const bool mieRayPhase = true;
      SingleScatteringResult ss = IntegrateScatteredLuminance(
        uTransmittanceLutTexture,
        uMultiScatTexture,
        uv, 
        worldPos, 
        worldDir, 
        sunDirection, 
        uAtmosphereParameters.atmosphereParameters, 
        ground, 
        sampleCountIni, 
        depthBufferValue, 
        variableSampleCount,  
        mieRayPhase,
        skyInvViewProjMat,
        -1.0,
        ProjectionMatrixIsReversedZ(view.projectionMatrix)
      );

      inscattering = ss.L;

      if(depthIsZFar){
        inscattering += GetSunLuminance(originalWorldPos, worldDir, sunDirection, uAtmosphereParameters.atmosphereParameters.BottomRadius).xyz * ss.Transmittance;
      }

      transmittance = ss.Transmittance;

    }

#ifdef DUALBLEND
    outInscattering = vec4(inscattering, 1.0);
    outTransmittance = vec4(vec3(clamp(transmittance, vec3(0.0), vec3(1.0))), 1.0);
#else
    outInscattering = vec4(inscattering, 1.0 - clamp(dot(transmittance, vec3(1.0 / 3.0)), 0.0, 1.0)); // alpha = 1.0 - transmittance 
#endif

  }
  
}