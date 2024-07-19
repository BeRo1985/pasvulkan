#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_multiview : enable
#extension GL_EXT_samplerless_texture_functions : enable

/* clang-format off */

//layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

/* clang-format on */

#define MULTISCATAPPROX_ENABLED
#undef SHADOWS_ENABLED

#include "atmosphere_common.glsl"

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outLuminance;

#define FLAGS_USE_FAST_SKY 1u
#define FLAGS_USE_FAST_AERIAL_PERSPECTIVE 2u

// Push constants
layout(push_constant, std140) uniform PushConstants {
  int baseViewIndex;
  int countViews;
  uint flags;
} pushConstants;

#ifdef MSAA

#ifdef MULTIVIEW
layout(set = 0, binding = 0) uniform texture2DMSArray uDepthTexture;
#else
layout(set = 0, binding = 0) uniform texture2DMS uDepthTexture;
#endif

#else

#ifdef MULTIVIEW
layout(set = 0, binding = 0) uniform texture2DArray uDepthTexture; 
#else
layout(set = 0, binding = 0) uniform texture2D uDepthTexture;
#endif

#endif

/*
#ifdef MSAA
layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInputMS uSubpassDepth;
#else  
layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassDepth;
#endif
*/

layout(set = 0, binding = 1) uniform sampler2D uTransmittanceLutTexture;

layout(set = 0, binding = 2) uniform sampler2DArray uMultiScatTexture;

layout(set = 0, binding = 3) uniform sampler2DArray uSkyViewLUT;

layout(set = 0, binding = 4) uniform sampler2DArray uCameraVolume;

layout(set = 0, binding = 5, std430) buffer AtmosphereParametersBuffer {
  AtmosphereParameters atmosphereParameters;
} uAtmosphereParameters;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

AtmosphereParameters atmosphereParameters;
 
#include "projectsphere.glsl"

#include "textureutils.glsl"

void main() {

  atmosphereParameters = uAtmosphereParameters.atmosphereParameters;

  int viewIndex = pushConstants.baseViewIndex + int(gl_ViewIndex);
  View view = uView.views[viewIndex];

/*vec2 pixPos = vec2(gl_FragCoord.xy) + vec2(0.5);
  vec2 uv = pixPos / pushConstants.resolution;*/

  vec2 uv = inTexCoord; 

  vec3 worldPos, worldDir;
  GetCameraPositionDirection(worldPos, worldDir, view.viewMatrix, view.projectionMatrix, view.inverseViewMatrix, view.inverseProjectionMatrix, uv);

  worldPos = (atmosphereParameters.inverseTransform * vec4(worldPos, 1.0)).xyz;

  //worldPos += vec3(0.0, atmosphereParameters.BottomRadius, 0.0);

  float viewHeight = max(length(worldPos), atmosphereParameters.BottomRadius + 1e-4);  
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

  bool depthIsZFar = depthBufferValue == GetZFarDepthValue(view.projectionMatrix);

  if(/*(viewHeight < atmosphereParameters.TopRadius) &&*/ depthIsZFar){

    if((pushConstants.flags & FLAGS_USE_FAST_SKY) != 0u){

      vec2 localUV;
      vec3 UpVector = normalize(worldPos);
      float viewZenithCosAngle = dot(worldDir, UpVector);

      vec3 sideVector = normalize(cross(UpVector, worldDir));		// assumes non parallel vectors
      vec3 forwardVector = normalize(cross(sideVector, UpVector));	// aligns toward the sun light but perpendicular to up vector
      vec2 lightOnPlane = vec2(dot(sunDirection, forwardVector), dot(sunDirection, sideVector));
      lightOnPlane = normalize(lightOnPlane);
      float lightViewCosAngle = lightOnPlane.x;

      bool IntersectGround = raySphereIntersectNearest(worldPos, worldDir, vec3(0.0), atmosphereParameters.BottomRadius) >= 0.0;
  
      SkyViewLutParamsToUv(atmosphereParameters, IntersectGround, viewZenithCosAngle, lightViewCosAngle, viewHeight, localUV);

#if 0
      localUV = getNiceTextureUV(localUV, vec2(textureSize(uSkyViewLUT, 0).xy));
#endif      

      vec4 value = textureLod(uSkyViewLUT, vec3(localUV, float(viewIndex)), 0.0).xyzw; // xyz = inscatter, w = transmittance (monochromatic)

      if(!IntersectGround){
        value.xyz += GetSunLuminance(worldPos, worldDir, sunDirection, atmosphereParameters.BottomRadius).xyz * value.w;
      }

      outLuminance = vec4(value);

      return; // Early out, for avoiding the code path of the more accurate and more bruteforce ray marching approach

    }

  }else{
   
    if((pushConstants.flags & FLAGS_USE_FAST_AERIAL_PERSPECTIVE) != 0u){

      // Fast aerial perspective approximation using a 3D texture

      // (BeRo): Check if we can use the fast aerial perspective approximation, given the camera volume constraints the planet in a way that
      // the voxel resolution is not too inaccurate
      bool fitsInCameraVolume = true;
      if(length(worldPos) >= atmosphereParameters.TopRadius){

        vec4 aabb;     
        vec3 transformedCenter = ((view.viewMatrix * atmosphereParameters.transform) * vec4(vec3(0.0), 1.0)).xyz;
        if(projectSphere(transformedCenter, atmosphereParameters.TopRadius, 0.01, view.projectionMatrix, aabb, false)){

          // camera volume is 32x32 by width and height and 32 by depth, by default

          vec2 aabbSize = (aabb.zw - aabb.xy) * vec2(textureSize(uCameraVolume, 0).xy);

          fitsInCameraVolume = all(greaterThanEqual(aabbSize, vec2(4.0))); // 4x4 pixels minimum, otherwise the voxel resolution is too inaccurate

        }

      }  
  
      if(fitsInCameraVolume){

        // (BeRo): Move ray marching start up to top atmosphere, for to avoid missing the atmosphere in the special case of the camera being
        // far outside the atmosphere.
        //if(length(worldPos) >= atmosphereParameters.TopRadius)
        {
          vec2 t = raySphereIntersect(worldPos, worldDir, vec3(0.0), atmosphereParameters.TopRadius);
          if(all(greaterThanEqual(t, vec2(0.0)))){
            worldPos += worldDir * min(t.x, t.y);
          }
        }

        mat4 inverseViewProjectionMatrix = view.inverseViewMatrix * view.inverseProjectionMatrix;

        vec4 depthBufferWorldPos = inverseViewProjectionMatrix * vec4(fma(vec2(uv), vec2(2.0), vec2(-1.0)), depthBufferValue, 1.0);
        depthBufferWorldPos /= depthBufferWorldPos.w;

        float tDepth = length((atmosphereParameters.inverseTransform * vec4(depthBufferWorldPos.xyz, 1.0)).xyz - worldPos);
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
        vec4 AP = mix(
                    textureLod(uCameraVolume, vec3(uv, sliceIndex + (viewIndex * AP_SLICE_COUNT_INT)), 0.0),
                    textureLod(uCameraVolume, vec3(uv, nextSliceIndex + (viewIndex * AP_SLICE_COUNT_INT)), 0.0),
                    sliceWeight
                  ) * Weight;


        if(depthIsZFar){
          AP.xyz += GetSunLuminance(worldPos, worldDir, sunDirection, atmosphereParameters.BottomRadius).xyz;  
        }

        outLuminance = vec4(AP.xyz, AP.w); 
      
        return; // Early out, for avoiding the code path of the more accurate and more bruteforce ray marching approach

      }  

    }

  }

  {

    // The more accurate and more bruteforce ray marching approach  

    vec3 inscattering;
    float transmittance;

    // Move to top atmosphere as the starting point for ray marching.
    // This is critical to be after the above to not disrupt above atmosphere tests and voxel selection.
    if(!MoveToTopAtmosphere(worldPos, worldDir, atmosphereParameters.TopRadius)){
      
      // Ray is not intersecting the atmosphere       
      inscattering = GetSunLuminance(worldPos, worldDir, sunDirection, atmosphereParameters.BottomRadius).xyz;
      transmittance = 1.0;

    }else {

      mat4 skyInvViewProjMat = view.inverseViewMatrix * view.inverseProjectionMatrix; 
      const bool ground = false;
      const float sampleCountIni = 0.0;
      const bool variableSampleCount = true;
      const bool mieRayPhase = true;
      SingleScatteringResult ss = IntegrateScatteredLuminance(
        uTransmittanceLutTexture,
        uMultiScatTexture,
        viewIndex,
        uv, 
        worldPos, 
        worldDir, 
        sunDirection, 
        atmosphereParameters, 
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
        inscattering += GetSunLuminance(worldPos, worldDir, sunDirection, atmosphereParameters.BottomRadius).xyz * ss.Transmittance;
      }

      transmittance = clamp(dot(ss.Transmittance, vec3(1.0 / 3.0)), 0.0, 1.0);

    }

    outLuminance = vec4(inscattering, 1.0 - transmittance);

  }
  
}