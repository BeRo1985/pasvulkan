#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_multiview : enable

/* clang-format off */

//layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

/* clang-format on */

#undef MULTISCATAPPROX_ENABLED
#undef SHADOWS_ENABLED

#include "atmosphere_common.glsl"

layout(location = 0) out vec4 outLuminance;

// Push constants
layout(push_constant, std140) uniform PushConstants {
  int baseViewIndex;
  int countViews;
  float noHitDepthValue;
  float startDepth;
  vec2 resolution;
} pushConstants;

#ifdef MSAA
layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInputMS uSubpassDepth;
#else  
layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassDepth;
#endif

layout(set = 0, binding = 1) uniform sampler2DArray uSkyViewLUT;

layout(set = 0, binding = 2) uniform sampler2DArray uCameraVolume;

layout(set = 0, binding = 3, std430) buffer AtmosphereParametersBuffer {
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
 
void main() {

  atmosphereParameters = uAtmosphereParameters.atmosphereParameters;

  int viewIndex = pushConstants.baseViewIndex + int(gl_ViewIndex);
  View view = uView.views[viewIndex];

  vec2 pixPos = vec2(gl_FragCoord.xy) + vec2(0.5);
  vec2 uv = pixPos / pushConstants.resolution;

  vec3 WorldPos, WorldDir;
  getCameraPositionDirection(WorldPos, WorldDir, view.viewMatrix, view.projectionMatrix, view.inverseViewMatrix, view.inverseProjectionMatrix, uv);

  WorldPos += vec3(0.0, atmosphereParameters.BottomRadius, 0.0);

  float viewHeight = length(WorldPos);
	vec3 L = vec3(0.0);
#ifdef MSAA
	float DepthBufferValue = subpassLoad(uSubpassDepth, gl_SampleID).x;
#else  
	float DepthBufferValue = subpassLoad(uSubpassDepth).x;
#endif

  vec3 sunDirection = normalize(getSunDirection(uAtmosphereParameters.atmosphereParameters));

	if((viewHeight < atmosphereParameters.TopRadius) && (DepthBufferValue == pushConstants.noHitDepthValue)){

		vec2 uv;
		vec3 UpVector = normalize(WorldPos);
		float viewZenithCosAngle = dot(WorldDir, UpVector);

		vec3 sideVector = normalize(cross(UpVector, WorldDir));		// assumes non parallel vectors
		vec3 forwardVector = normalize(cross(sideVector, UpVector));	// aligns toward the sun light but perpendicular to up vector
		vec2 lightOnPlane = vec2(dot(sunDirection, forwardVector), dot(sunDirection, sideVector));
		lightOnPlane = normalize(lightOnPlane);
		float lightViewCosAngle = lightOnPlane.x;

		bool IntersectGround = raySphereIntersectNearest(WorldPos, WorldDir, vec3(0.0), atmosphereParameters.BottomRadius) >= 0.0;

		SkyViewLutParamsToUv(atmosphereParameters, IntersectGround, viewZenithCosAngle, lightViewCosAngle, viewHeight, uv);

		outLuminance = vec4(
      textureLod(uSkyViewLUT, vec3(uv, float(viewIndex)), 0.0).xyz +
      GetSunLuminance(WorldPos, WorldDir, sunDirection, atmosphereParameters.BottomRadius), 
      1.0
    );

	}else{

    mat4 inverseViewProjectionMatrix = view.inverseProjectionMatrix * view.inverseViewMatrix;

    vec4 DepthBufferWorldPos = inverseViewProjectionMatrix * vec4(fma(vec2(uv), vec2(2.0), vec2(-1.0)), DepthBufferValue, 1.0);
    DepthBufferWorldPos /= DepthBufferWorldPos.w;

    float tDepth = length(DepthBufferWorldPos.xyz - (WorldPos + vec3(0.0, 0.0, -atmosphereParameters.BottomRadius)));
    float Slice = AerialPerspectiveDepthToSlice(tDepth);
    float Weight = 1.0;
    if(Slice < 0.5){
      Weight = clamp(Slice * 2.0, 0.0, 1.0);
      Slice = 0.5;
    } 
    float w = sqrt(Slice / AP_SLICE_COUNT); // squared distribution

    float baseSlice = w * AP_SLICE_COUNT;
    int sliceIndex = int(floor(baseSlice));
    float sliceWeight = baseSlice - float(sliceIndex);
    int nextSliceIndex = clamp(sliceIndex + 1, 0, AP_SLICE_COUNT_INT - 1);
    sliceIndex = clamp(sliceIndex, 0, AP_SLICE_COUNT_INT - 1);

    // Manual 3D texture lookup from a 2D array texture, since multiview is not supported for 3D textures (no 3D array textures) 
    vec4 AP = mix(
                textureLod(uCameraVolume, vec3(pixPos / pushConstants.resolution, sliceIndex + (viewIndex * AP_SLICE_COUNT_INT)), 0.0),
                textureLod(uCameraVolume, vec3(pixPos / pushConstants.resolution, nextSliceIndex + (viewIndex * AP_SLICE_COUNT_INT)), 0.0),
                sliceWeight
              ) * Weight;

    outLuminance = vec4(AP.xyz, AP.w); 

  }
  
}