#version 450 core

#define MESH_FRAGMENT_SHADER

#define CAN_HAVE_EXTENDED_PBR_MATERIAL

#if defined(VOXELIZATION)
  #undef LIGHTS
  #undef SHADOWS
#endif

#ifdef USE_MATERIAL_BUFFER_REFERENCE
  #undef NOBUFFERREFERENCE
#elif defined(USE_MATERIAL_SSBO)
  #define NOBUFFERREFERENCE
#endif

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#if defined(USEDEMOTE)
  #extension GL_EXT_demote_to_helper_invocation : enable
#endif
#extension GL_EXT_nonuniform_qualifier : enable
#if defined(USESHADERBUFFERFLOAT32ATOMICADD)
  #extension GL_EXT_shader_atomic_float : enable
#endif

#extension GL_EXT_control_flow_attributes : enable

#ifndef NOBUFFERREFERENCE
  #define sizeof(Type) (uint64_t(Type(uint64_t(0))+1))
  #extension GL_EXT_shader_explicit_arithmetic_types_int64 : enable 
  #extension GL_EXT_buffer_reference2 : enable 
  #ifndef USEINT64
    #extension GL_EXT_buffer_reference_uvec2 : enable 
  #endif
#endif

#if defined(LOCKOIT) || defined(DFAOIT)
  #extension GL_ARB_post_depth_coverage : enable
  #ifdef INTERLOCK
    #extension GL_ARB_fragment_shader_interlock : enable
    #define beginInvocationInterlock beginInvocationInterlockARB
    #define endInvocationInterlock endInvocationInterlockARB
    #ifdef MSAA
      layout(early_fragment_tests, post_depth_coverage, sample_interlock_ordered) in;
    #else
      layout(early_fragment_tests, post_depth_coverage, pixel_interlock_ordered) in;
    #endif
  #else
    #if defined(ALPHATEST)
      layout(post_depth_coverage) in;
    #else
      layout(early_fragment_tests, post_depth_coverage) in;
    #endif
  #endif
#elif !defined(ALPHATEST)
  layout(early_fragment_tests) in;
#endif

#ifdef VOXELIZATION
layout(location = 0) in vec3 inWorldSpacePosition;
layout(location = 1) in vec3 inViewSpacePosition;
layout(location = 2) in vec3 inCameraRelativePosition;
layout(location = 3) in vec3 inTangent;
layout(location = 4) in vec3 inBitangent;
layout(location = 5) in vec3 inNormal;
layout(location = 6) in vec2 inTexCoord0;
layout(location = 7) in vec2 inTexCoord1;
layout(location = 8) in vec4 inColor0;
layout(location = 9) in vec3 inModelScale;
layout(location = 10) flat in uint inMaterialID;
layout(location = 11) flat in vec3 inAABBMin;
layout(location = 12) flat in vec3 inAABBMax;
layout(location = 13) flat in uint inCascadeIndex; 
layout(location = 14) in vec3 inVoxelPosition; 
layout(location = 15) flat in vec3 inVertex0;
layout(location = 16) flat in vec3 inVertex1;
layout(location = 17) flat in vec3 inVertex2;
#else
layout(location = 0) in vec3 inWorldSpacePosition;
layout(location = 1) in vec3 inViewSpacePosition;
layout(location = 2) in vec3 inCameraRelativePosition;
layout(location = 3) in vec3 inTangent;
layout(location = 4) in vec3 inBitangent;
layout(location = 5) in vec3 inNormal;
layout(location = 6) in vec2 inTexCoord0;
layout(location = 7) in vec2 inTexCoord1;
layout(location = 8) in vec4 inColor0;
layout(location = 9) in vec3 inModelScale;
layout(location = 10) flat in uint inMaterialID;
layout(location = 11) flat in int inViewIndex;
layout(location = 12) flat in uint inFrameIndex;
#ifdef VELOCITY
layout(location = 13) flat in vec4 inJitter;
layout(location = 14) in vec4 inPreviousClipSpace;
layout(location = 15) in vec4 inCurrentClipSpace;
#else
layout(location = 13) flat in vec2 inJitter;
#endif
#endif

#ifdef VOXELIZATION
  // Nothing in this case, since the fragment shader writes to the voxel grid directly.
#elif defined(DEPTHONLY)
#else
  #if defined(VELOCITY) && !(defined(MBOIT) && defined(MBOITPASS1))
    layout(location = 1) out vec2 outFragVelocity;
  #elif defined(EXTRAEMISSIONOUTPUT) && !(defined(WBOIT) || defined(MBOIT))
    layout(location = 1) out vec4 outFragEmission;
  #elif defined(REFLECTIVESHADOWMAPOUTPUT)
    layout(location = 1) out vec4 outFragNormalUsed; // xyz = normal, w = 1.0 if normal was used, 0.0 otherwise (by clearing the normal buffer to vec4(0.0))
    //layout(location = 2) out vec3 outFragPosition; // Can be reconstructed from depth and inversed model view projection matrix 
  #endif
#endif

// Specialization constants are sadly unusable due to dead slow shader stage compilation times with several minutes "per" pipeline, 
// when the validation layers and a debugger (GDB, LLDB, etc.) are active at the same time!
#undef USE_SPECIALIZATION_CONSTANTS
#ifdef USE_SPECIALIZATION_CONSTANTS
layout (constant_id = 0) const bool UseReversedZ = true;
#endif

const int TEXTURE_BRDF_GGX = 0;
const int TEXTURE_BRDF_CHARLIE = 1;
const int TEXTURE_BRDF_SHEEN_E = 2;
const int TEXTURE_ENVMAP_GGX = 3;
const int TEXTURE_ENVMAP_CHARLIE = 4;
const int TEXTURE_ENVMAP_LAMBERTIAN = 5;

const int TEXTURE_BASE_INDEX = 10;

// Global descriptor set

#define MESHS
#include "globaldescriptorset.glsl"
#undef MESHS

// Pass descriptor set

#include "mesh_rendering_pass_descriptorset.glsl"

#ifdef FRUSTUMCLUSTERGRID
layout (set = 1, binding = 6, std140) readonly uniform FrustumClusterGridGlobals {
  uvec4 tileSizeZNearZFar; 
  vec4 viewRect;
  uvec4 countLightsViewIndexSizeOffsetedViewIndex;
  uvec4 clusterSize;
  vec4 scaleBiasMax;
} uFrustumClusterGridGlobals;

layout (set = 1, binding = 7, std430) readonly buffer FrustumClusterGridIndexList {
   uint frustumClusterGridIndexList[];
};

layout (set = 1, binding = 8, std430) readonly buffer FrustumClusterGridData {
  uvec4 frustumClusterGridData[]; // x = start light index, y = count lights, z = start decal index, w = count decals
};

#endif

#ifdef VOXELIZATION
  layout(location = 0) out vec4 outFragColor;
  #include "voxelization_globals.glsl"
#endif

// Extra global illumination descriptor set (optional, if global illumination is enabled) for more easily sharing the same 
// global illumination data between multiple passes (e.g. opaque and transparent passes).

#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS)

  #define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET 2
  #define GLOBAL_ILLUMINATION_VOLUME_UNIFORM_BINDING 0
  layout(set = GLOBAL_ILLUMINATION_VOLUME_UNIFORM_SET, binding = 1) uniform sampler3D uTexGlobalIlluminationCascadedRadianceHintsSHVolumes[];
  #define GLOBAL_ILLUMINATION_VOLUME_MESH_FRAGMENT
  #include "global_illumination_cascaded_radiance_hints.glsl"

#elif defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING) 

  layout (set = 2, binding = 0, std140) readonly uniform VoxelGridData {
    #include "voxelgriddata_uniforms.glsl"
  } voxelGridData;

  layout(set = 2, binding = 1) uniform sampler3D uVoxelGridOcclusion[];

  layout(set = 2, binding = 2) uniform sampler3D uVoxelGridRadiance[];

  #include "global_illumination_voxel_cone_tracing.glsl"

#endif

#ifndef VOXELIZATION
  #define TRANSPARENCY_DECLARATION
  #include "transparency.glsl"
  #undef TRANSPARENCY_DECLARATION
#endif

/* clang-format on */

vec3 workTangent, workBitangent, workNormal;

#include "math.glsl" 
 
#define TRANSPARENCY_GLOBALS
#include "transparency.glsl"
#undef TRANSPARENCY_GLOBALS

#ifdef VOXELIZATION
vec3 cartesianToBarycentric(vec3 p, vec3 a, vec3 b, vec3 c) {
  vec3 v0 = b - a, v1 = c - a, v2 = p - a;
  float d00 = dot(v0, v0), d01 = dot(v0, v1), d11 = dot(v1, v1), d20 = dot(v2, v0), d21 = dot(v2, v1);
  vec2 vw = vec2((d11 * d20) - (d01 * d21), (d00 * d21) - (d01 * d20)) / vec2((d00 * d11) - (d01 * d01));
  return vec3((1.0 - vw.x) - vw.y, vw.xy);
}
#endif

#if !(defined(DEPTHONLY) || defined(VOXELIZATION))

#include "roughness.glsl"

float envMapMaxLevelGGX, envMapMaxLevelCharlie;
 
uint flags, shadingModel;

vec3 parallaxCorrectedReflection(vec3 reflectionDirection){
    
#define PARALLAX_CORRECTION_METHOD 0 // 0 = None, 1 = Offset, 2 = Vector, 3 = Halfway (all without proxy geometry, at the moment) 

#if PARALLAX_CORRECTION_METHOD != 0
//vec3 fragmentWorldPosition = inWorldSpacePosition;
//vec3 cameraWorldPosition = uView.views[inViewIndex].inverseViewMatrix[3].xyz;
#endif

#if PARALLAX_CORRECTION_METHOD == 1

  // The most straightforward way to do parallax correction is to adjust the reflection vector based on the relative positions of the 
  // fragment and the camera. This adjustment will be based on how the view direction intersects with the virtual "bounding box" of the cubemap.
  // Given that a cubemap is, conceptually, a bounding box surrounding the scene, we can think of the parallax correction as finding the intersection 
  // of the view direction with this bounding box and using that point to adjust the reflection vector. Here's an approach to do this:

  // Calculate the normalized view direction, which is the direction from the camera to the fragment.
  vec3 viewDirection = normalize(-inCameraRelativePosition); //normalize(fragmentWorldPosition - cameraWorldPosition);
  
  // Compute the offset between the view direction and the original reflection direction.
  // This offset represents how much the reflection direction should be adjusted to account for the viewer's position.
  vec3 offset = viewDirection - reflectionDirection;
  
  // Apply the offset to the original reflection direction to get the parallax-corrected reflection direction.
  vec3 parallaxCorrectedReflectionDirection = reflectionDirection + offset;

  return normalize(parallaxCorrectedReflectionDirection);

#elif PARALLAX_CORRECTION_METHOD == 2

  // Another approach to parallax correction is to compute the reflection direction as usual and then adjust it based on the relative positions of the
  // fragment and the camera. This adjustment will be based on how the reflection direction intersects with the virtual "bounding box" of the cubemap.
  // Given just the fragment position, camera position, and reflection direction, we can only apply a general parallax correction, assuming a virtual 
  // "bounding box" around the scene. Here's an approach to do this:

  // Normalize the input reflection direction
  vec3 normalizedReflectionDirection = normalize(reflectionDirection);

  // Compute the view direction, which is the direction from the camera to the fragment
  vec3 viewDirection = -inCameraRelativePosition; //fragmentWorldPosition - cameraWorldPosition;
  
  // Create a vector perpendicular to the reflection direction and the view direction.
  vec3 perpendicularVector = cross(normalizedReflectionDirection, viewDirection);
  
  // Create another vector perpendicular to the reflection direction and the first perpendicular vector.
  vec3 correctionVector = cross(perpendicularVector, normalizedReflectionDirection);
  
  // Use the magnitude of the view direction to apply the parallax correction.
  float parallaxMagnitude = length(viewDirection) * 0.5;  // The scale factor (0.5) can be adjusted.
  
  // Apply the parallax correction to the reflection direction.
  // The reflection direction is shifted by a fraction of the parallax-reflected direction.
  vec3 parallaxCorrectedReflectionDirection = normalizedReflectionDirection + (correctionVector * parallaxMagnitude);

  return normalize(parallaxCorrectedReflectionDirection);

#elif PARALLAX_CORRECTION_METHOD == 3

  vec3 localSurfaceNormal = inNormal;

  // Normalize the input reflection direction
  vec3 normalizedReflectionDirection = normalize(reflectionDirection);
  
  // Compute the view direction, which is the direction from the camera to the fragment
  vec3 viewDirection = -inCameraRelativePosition; //fragmentWorldPosition - cameraWorldPosition;
  
  // Calculate the halfway vector between the view direction and the reflection direction.
  // This is often used in shading models, especially for specular reflections.
  vec3 halfwayVector = normalize(viewDirection + normalizedReflectionDirection);
  
  // Compute the reflection of the view direction about the local surface normal.
  // This would be the reflection vector if the surface was a perfect mirror.
  vec3 parallaxReflectedDirection = reflect(viewDirection, localSurfaceNormal);
  
  // Compute a scale factor based on the angle between the halfway vector and the local surface normal.
  // The dot product here effectively measures the cosine of the angle between the two vectors.
  // This factor will be used to adjust the reflection direction based on the viewer's position.
  float parallaxScaleFactor = 0.5 * dot(halfwayVector, localSurfaceNormal);
  
  // Apply the parallax correction to the reflection direction.
  // The reflection direction is shifted by a fraction of the parallax-reflected direction.
  vec3 parallaxCorrectedReflectionDirection = normalizedReflectionDirection + (parallaxReflectedDirection * parallaxScaleFactor);
  
  // Return the normalized parallax-corrected reflection direction.
  return normalize(parallaxCorrectedReflectionDirection);

#else

  return reflectionDirection;

#endif

}

#if defined(BLEND) || defined(LOOPOIT) || defined(LOCKOIT) || defined(MBOIT) || defined(WBOIT) || defined(DFAOIT)
  #define TRANSMISSION
#endif

#if defined(TRANSMISSION)
float transmissionFactor = 0.0;
float volumeThickness = 0.0;
float volumeAttenuationDistance = 1.0 / 0.0; // +INF
vec3 volumeAttenuationColor = vec3(1.0); 
float volumeDispersion = 0.0;
#endif

#define ENABLE_ANISOTROPIC
#include "pbr.glsl"
  
/////////////////////////////

vec3 getPunctualRadianceTransmission(vec3 normal, vec3 view, vec3 pointToLight, float alphaRoughness, vec3 f0, vec3 f90, vec3 baseColor, float ior) {
  float transmissionRougness = applyIorToRoughness(alphaRoughness, ior);

  vec3 n = normalize(normal);  // Outward direction of surface point
  vec3 v = normalize(view);    // Direction from surface point to view
  vec3 l = normalize(pointToLight);
  vec3 l_mirror = normalize(l + (2.0 * n * dot(-l, n)));  // Mirror light reflection vector on surface
  vec3 h = normalize(l_mirror + v);                       // Halfway vector between transmission light vector and v

  float D = D_GGX(clamp(dot(n, h), 0.0, 1.0), transmissionRougness);
  vec3 F = F_Schlick(f0, f90, clamp(dot(v, h), 0.0, 1.0));
  float Vis = V_GGX(clamp(dot(n, l_mirror), 0.0, 1.0), clamp(dot(n, v), 0.0, 1.0), transmissionRougness);

  // Transmission BTDF
  return (1.0 - F) * baseColor * D * Vis;
}

/////////////////////////////

// Compute attenuated light as it travels through a volume.
vec3 applyVolumeAttenuation(vec3 radiance, float transmissionDistance, vec3 attenuationColor, float attenuationDistance) {
  if (isinf(attenuationDistance) || (attenuationDistance == 0.0)) {
    // Attenuation distance is +∞ (which we indicate by zero), i.e. the transmitted color is not attenuated at all.
    return radiance;
  } else {
    // Compute light attenuation using Beer's law.
    vec3 attenuationCoefficient = -log(attenuationColor) / attenuationDistance;
    vec3 transmittance = exp(-attenuationCoefficient * transmissionDistance);  // Beer's law
    return transmittance * radiance;
  }
}

vec3 getVolumeTransmissionRay(vec3 n, vec3 v, float thickness, float ior) {
  return normalize(refract(-v, normalize(n), 1.0 / ior)) * thickness * inModelScale;
}

/////////////////////////////

// XYZ to sRGB color space
const mat3 XYZ_TO_REC709 = mat3(3.2404542, -0.9692660, 0.0556434, -1.5371385, 1.8760108, -0.2040259, -0.4985314, 0.0415560, 1.0572252);

// Assume air interface for top
// Note: We don't handle the case fresnel0 == 1
vec3 Fresnel0ToIor(vec3 fresnel0) {
  vec3 sqrtF0 = sqrt(fresnel0);
  return (vec3(1.0) + sqrtF0) / (vec3(1.0) - sqrtF0);
}

// Conversion FO/IOR
vec3 IorToFresnel0(vec3 transmittedIor, float incidentIor) { return sq((transmittedIor - vec3(incidentIor)) / (transmittedIor + vec3(incidentIor))); }

// ior is a value between 1.0 and 3.0. 1.0 is air interface
float IorToFresnel0(float transmittedIor, float incidentIor) { return sq((transmittedIor - incidentIor) / (transmittedIor + incidentIor)); }

// Fresnel equations for dielectric/dielectric interfaces.
// Ref: https://belcour.github.io/blog/research/2017/05/01/brdf-thin-film.html
// Evaluation XYZ sensitivity curves in Fourier space
vec3 evalSensitivity(float OPD, vec3 shift) {
  float phase = 2.0 * PI * OPD * 1.0e-9;
  vec3 val = vec3(5.4856e-13, 4.4201e-13, 5.2481e-13);
  vec3 pos = vec3(1.6810e+06, 1.7953e+06, 2.2084e+06);
  vec3 var = vec3(4.3278e+09, 9.3046e+09, 6.6121e+09);

  vec3 xyz = val * sqrt(2.0 * PI * var) * cos(pos * phase + shift) * exp(-sq(phase) * var);
  xyz.x += 9.7470e-14 * sqrt(2.0 * PI * 4.5282e+09) * cos(2.2399e+06 * phase + shift[0]) * exp(-4.5282e+09 * sq(phase));
  xyz /= 1.0685e-7;

  vec3 srgb = XYZ_TO_REC709 * xyz;
  return srgb;
}

vec3 evalIridescence(float outsideIOR, float eta2, float cosTheta1, float thinFilmThickness, vec3 baseF0) {
  vec3 I;

  // Force iridescenceIor -> outsideIOR when thinFilmThickness -> 0.0
  float iridescenceIor = mix(outsideIOR, eta2, smoothstep(0.0, 0.03, thinFilmThickness));
  // Evaluate the cosTheta on the base layer (Snell law)
  float sinTheta2Sq = sq(outsideIOR / iridescenceIor) * (1.0 - sq(cosTheta1));

  // Handle TIR:
  float cosTheta2Sq = 1.0 - sinTheta2Sq;
  if (cosTheta2Sq < 0.0) {
    return vec3(1.0);
  }

  float cosTheta2 = sqrt(cosTheta2Sq);

  // First interface
  float R0 = IorToFresnel0(iridescenceIor, outsideIOR);
  float R12 = F_Schlick(R0, cosTheta1);
  float R21 = R12;
  float T121 = 1.0 - R12;
  float phi12 = 0.0;
  if (iridescenceIor < outsideIOR) phi12 = PI;
  float phi21 = PI - phi12;

  // Second interface
  vec3 baseIOR = Fresnel0ToIor(clamp(baseF0, 0.0, 0.9999));  // guard against 1.0
  vec3 R1 = IorToFresnel0(baseIOR, iridescenceIor);
  vec3 R23 = F_Schlick(R1, cosTheta2);
  vec3 phi23 = vec3(0.0);
  if (baseIOR[0] < iridescenceIor) phi23[0] = PI;
  if (baseIOR[1] < iridescenceIor) phi23[1] = PI;
  if (baseIOR[2] < iridescenceIor) phi23[2] = PI;

  // Phase shift
  float OPD = 2.0 * iridescenceIor * thinFilmThickness * cosTheta2;
  vec3 phi = vec3(phi21) + phi23;

  // Compound terms
  vec3 R123 = clamp(R12 * R23, 1e-5, 0.9999);
  vec3 r123 = sqrt(R123);
  vec3 Rs = sq(T121) * R23 / (vec3(1.0) - R123);

  // Reflectance term for m = 0 (DC term amplitude)
  vec3 C0 = R12 + Rs;
  I = C0;

  // Reflectance term for m > 0 (pairs of diracs)
  vec3 Cm = Rs - T121;
  for (int m = 1; m <= 2; ++m) {
    Cm *= r123;
    vec3 Sm = 2.0 * evalSensitivity(float(m) * OPD, float(m) * phi);
    I += Cm * Sm;
  }

  // Since out of gamut colors might be produced, negative color values are clamped to 0.
  return max(I, vec3(0.0));
}

////////////////////////////
 
#ifdef TRANSMISSION
vec4 cubic(float v) {
  vec4 n = vec4(1.0, 2.0, 3.0, 4.0) - v;
  n *= n * n;
  vec3 t = vec3(n.x, fma(n.xy, vec2(-4.0), n.yz)) + vec2(0.0, 6.0 * n.x).xxy;
  return vec4(t, ((6.0 - t.x) - t.y) - t.z) * (1.0 / 6.0);
}

vec4 textureBicubicEx(const in sampler2DArray tex, vec3 uvw, int lod) {
  vec2 textureResolution = textureSize(tex, lod).xy,  //
      uv = fma(uvw.xy, textureResolution, vec2(-0.5)),            //
      fuv = fract(uv);
  uv -= fuv;
  vec4 xcubic = cubic(fuv.x),                                                             //
      ycubic = cubic(fuv.y),                                                              //
      c = uv.xxyy + vec2(-0.5, 1.5).xyxy,                                                 //
      s = vec4(xcubic.xz + xcubic.yw, ycubic.xz + ycubic.yw),                             //
      o = (c + (vec4(xcubic.yw, ycubic.yw) / s)) * (vec2(1.0) / textureResolution).xxyy;  //
  s.xy = s.xz / (s.xz + s.yw);
  return mix(mix(textureLod(tex, vec3(o.yw, uvw.z), float(lod)), textureLod(tex, vec3(o.xw, uvw.t), float(lod)), s.x),  //
             mix(textureLod(tex, vec3(o.yz, uvw.z), float(lod)), textureLod(tex, vec3(o.xz, uvw.z), float(lod)), s.x), s.y);
}

vec4 textureBicubic(const in sampler2DArray tex, vec3 uvw, float lod, int maxLod) {
  int ilod = int(floor(lod));
  lod -= float(ilod); 
  return (lod < float(maxLod)) ? mix(textureBicubicEx(tex, uvw, ilod), textureBicubicEx(tex, uvw, ilod + 1), lod) : textureBicubicEx(tex, uvw, maxLod);
}

vec4 betterTextureEx(const in sampler2DArray tex, vec3 uvw, int lod) {
  vec2 textureResolution = textureSize(uPassTextures[1], lod).xy;
  vec2 uv = fma(uvw.xy, textureResolution, vec2(0.5));
  vec2 fuv = fract(uv);
  return textureLod(tex, vec3((floor(uv) + ((fuv * fuv) * fma(fuv, vec2(-2.0), vec2(3.0))) - vec2(0.5)) / textureResolution, uvw.z), float(lod));
}

vec4 betterTexture(const in sampler2DArray tex, vec3 uvw, float lod, int maxLod) {
  int ilod = int(floor(lod));
  lod -= float(ilod); 
  return (lod < float(maxLod)) ? mix(betterTextureEx(tex, uvw, ilod), betterTextureEx(tex, uvw, ilod + 1), lod) : betterTextureEx(tex, uvw, maxLod);
}

vec3 getTransmissionSample(vec2 fragCoord, float roughness, float ior) {
  int maxLod = int(textureQueryLevels(uPassTextures[1]));
  float framebufferLod = float(maxLod) * applyIorToRoughness(roughness, ior);
#if 1
  vec3 transmittedLight = (framebufferLod < 1e-4) ? //
                           betterTexture(uPassTextures[1], vec3(fragCoord.xy, inViewIndex), framebufferLod, maxLod).xyz :  //                           
                           textureBicubic(uPassTextures[1], vec3(fragCoord.xy, inViewIndex), framebufferLod, maxLod).xyz; //
#else
  vec3 transmittedLight = texture(uPassTextures[1], vec3(fragCoord.xy, inViewIndex), framebufferLod).xyz;
#endif
  return transmittedLight;
}

vec3 getIBLVolumeRefraction(vec3 n, vec3 v, float perceptualRoughness, vec3 baseColor, vec3 f0, vec3 f90, vec3 position, float ior, float thickness, vec3 attenuationColor, float attenuationDistance, float dispersion) {
  
  vec3 attenuatedColor;

  // Sample framebuffer to get pixel the refracted ray hits.
  if(abs(dispersion) > 1e-7){
    
    float realIOR = 1.0 / ior;
    
    float iorDispersionSpread = 0.04 * dispersion * (realIOR - 1.0);
    
    vec3 iorValues = vec3(1.0 / (realIOR - iorDispersionSpread), ior, 1.0 / (realIOR + iorDispersionSpread));
    
    for(int i = 0; i < 3; i++){
      vec3 transmissionRay = getVolumeTransmissionRay(n, v, thickness, iorValues[i]);
      vec3 refractedRayExit = position + transmissionRay;

      // Project refracted vector on the framebuffer, while mapping to normalized device coordinates.
      vec4 ndcPos = uView.views[inViewIndex].projectionMatrix * uView.views[inViewIndex].viewMatrix * vec4(refractedRayExit, 1.0);
      vec2 refractionCoords = fma(ndcPos.xy / ndcPos.w, vec2(0.5), vec2(0.5));

      vec3 transmittedLight = getTransmissionSample(refractionCoords, perceptualRoughness, iorValues[i]);

      attenuatedColor[i] = applyVolumeAttenuation(transmittedLight, length(transmissionRay), attenuationColor, attenuationDistance)[i];    

    }

  }else{

    vec3 transmissionRay = getVolumeTransmissionRay(n, v, thickness, ior);
    vec3 refractedRayExit = position + transmissionRay;

    // Project refracted vector on the framebuffer, while mapping to normalized device coordinates.
    vec4 ndcPos = uView.views[inViewIndex].projectionMatrix * uView.views[inViewIndex].viewMatrix * vec4(refractedRayExit, 1.0);
    vec2 refractionCoords = fma(ndcPos.xy / ndcPos.w, vec2(0.5), vec2(0.5));

    vec3 transmittedLight = getTransmissionSample(refractionCoords, perceptualRoughness, ior);

    attenuatedColor = applyVolumeAttenuation(transmittedLight, length(transmissionRay), attenuationColor, attenuationDistance);  
      
  }
  
  // Sample GGX LUT to get the specular component.
  float NdotV = clamp(dot(n, v), 0.0, 1.0);
  vec2 brdfSamplePoint = clamp(vec2(NdotV, perceptualRoughness), vec2(0.0, 0.0), vec2(1.0, 1.0));
  vec2 brdf = textureLod(uImageBasedLightingBRDFTextures[0], brdfSamplePoint, 0.0).xy;
  vec3 specularColor = (f0 * brdf.x) + (f90 * brdf.y);

  return (1.0 - specularColor) * attenuatedColor * baseColor;
}
#endif

#include "shadows.glsl"

#endif // !defined(DEPTHONLY) || defined(VOXELIZATION) 

#ifdef NOBUFFERREFERENCE
#define material materials[inMaterialID]
//Material material = materials[inMaterialID];
#else
  #ifdef USEINT64
    Material material = uMaterials.materials[inMaterialID];
  #else
    Material material;
  #endif
#endif

const uint smPBRMetallicRoughness = 0u,  //
    smPBRSpecularGlossiness = 1u,        //
    smUnlit = 2u;                        //

#if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || defined(DFAOIT) || !defined(DEPTHONLY)

uvec2 textureFlags;
vec2 texCoords[2];
vec2 texCoords_dFdx[2];
vec2 texCoords_dFdy[2];

int getTexCoordID(const in int textureIndex){
  return material.textures[textureIndex]; 
}

vec2 textureUV(const in int textureIndex) {
  int textureID = getTexCoordID(textureIndex); 
  return (textureID >= 0) ? (material.textureTransforms[textureIndex] * vec3(texCoords[(textureID >> 16) & 0xf], 1.0)).xy : inTexCoord0;
}

ivec2 texture2DSize(const in int textureIndex) {
  int textureID = getTexCoordID(textureIndex); 
  return (textureID >= 0) ? ivec2(textureSize(u2DTextures[nonuniformEXT(textureID & 0x3fff)], 0).xy) : ivec2(0);
}

vec4 textureFetch(const in int textureIndex, const in vec4 defaultValue, const bool sRGB) {
  int textureID = getTexCoordID(textureIndex);
  if(textureID >= 0){
    int texCoordIndex = int((textureID >> 16) & 0xf); 
    mat3x2 m = material.textureTransforms[textureIndex];
    return textureGrad(u2DTextures[nonuniformEXT(((textureID & 0x3fff) << 1) | (int(sRGB) & 1))], //
                        (m * vec3(texCoords[texCoordIndex], 1.0)).xy,   //
                        (m * vec3(texCoords_dFdx[texCoordIndex], 0.0)).xy,  //
                        (m * vec3(texCoords_dFdy[texCoordIndex], 0.0)).xy);
 }else{
   return defaultValue;
 } 
}

#endif

#if defined(VOXELIZATION)
  #include "rgb9e5.glsl"
#endif

void main() {
#ifdef VOXELIZATION
  if(any(lessThan(inWorldSpacePosition.xyz, inAABBMin.xyz)) || 
     any(greaterThan(inWorldSpacePosition.xyz, inAABBMax.xyz)) ||
     (uint(inCascadeIndex) >= uint(voxelGridData.countCascades))){
    outFragColor = vec4(0.0);
    return;
  }
#endif
  {
    float frontFacingSign = gl_FrontFacing ? 1.0 : -1.0;   
    workTangent = inTangent * frontFacingSign;
    workBitangent = inBitangent * frontFacingSign;
    workNormal = inNormal * frontFacingSign;
  }
#if !(defined(NOBUFFERREFERENCE) || defined(USEINT64))
  material = uMaterials.materials;
  {
    uvec2 materialPointer = uvec2(material);  
    uint carry;
    materialPointer.x = uaddCarry(materialPointer.x, uint(inMaterialID * uint(sizeof(Material))), carry);
    materialPointer.y += carry;
    material = Material(materialPointer);
  }
#endif
#if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || defined(DFAOIT) || defined(VOXELIZATION) || !defined(DEPTHONLY)
  textureFlags = material.alphaCutOffFlagsTex0Tex1.zw;
  texCoords[0] = inTexCoord0;
  texCoords[1] = inTexCoord1;
  texCoords_dFdx[0] = dFdxFine(inTexCoord0);
  texCoords_dFdx[1] = dFdxFine(inTexCoord1);
  texCoords_dFdy[0] = dFdyFine(inTexCoord0);
  texCoords_dFdy[1] = dFdyFine(inTexCoord1);
#if !defined(VOXELIZATION)  
  /*if(!any(notEqual(inJitter.xy, vec2(0.0))))*/{
    texCoords[0] -= (texCoords_dFdx[0] * inJitter.x) + (texCoords_dFdy[0] * inJitter.y);
    texCoords[1] -= (texCoords_dFdx[1] * inJitter.x) + (texCoords_dFdy[1] * inJitter.y);
  }  
#endif
#endif
#if !(defined(DEPTHONLY) || defined(VOXELIZATION))
  envMapMaxLevelGGX = max(0.0, textureQueryLevels(uImageBasedLightingEnvMaps[0]) - 1.0);
  envMapMaxLevelCharlie = max(0.0, textureQueryLevels(uImageBasedLightingEnvMaps[1]) - 1.0);
  flags = material.alphaCutOffFlagsTex0Tex1.y;
  shadingModel = (flags >> 0u) & 0xfu;
#endif
#if defined(VOXELIZATION)
  
  uint flags = material.alphaCutOffFlagsTex0Tex1.y;
  
  // For meta voxelization, a very simple BRDF is used, so the data can be reused for various purposes at the later stages, so that
  // new costly voxelization passes are not required to be performed for these cases. Hence also the name meta voxelization, as the
  // voxelization is just performed for to gather meta data, which is then used for various purposes.

  vec4 baseColor = textureFetch(0, vec4(1.0), true) * material.baseColorFactor * inColor0; 
  
  vec4 emissionColor = vec4(textureFetch(4, vec4(1.0), true).xyz * material.emissiveFactor.xyz * material.emissiveFactor.w * inColor0.xyz, baseColor.w);
  
  float alpha = baseColor.w;
  
  vec3 normal;
  if ((textureFlags.x & (1 << 2)) != 0) {
    vec4 normalTexture = textureFetch(2, vec2(0.0, 1.0).xxyx, false);
    normal = normalize(                                                                                                                      //
        mat3(normalize(workTangent), normalize(workBitangent), normalize(workNormal)) *                                                            //
        normalize((normalTexture.xyz - vec3(0.5)) * (vec2(material.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0))  //
    );
  } else {
    normal = normalize(workNormal);
  }
  normal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;

#elif defined(DEPTHONLY)
#if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || defined(DFAOIT)
  float alpha = textureFetch(0, vec4(1.0), true).w * material.baseColorFactor.w * inColor0.w;
#endif
#else
  
  vec4 color = vec4(0.0);
#ifdef EXTRAEMISSIONOUTPUT
  vec4 emissionColor = vec4(0.0);
#endif
#if 0
   // Just for debugging purposes
   color = textureFetch(0, vec4(1.0), true) * material.baseColorFactor;
#else
  float litIntensity = 1.0;
  switch (shadingModel) {
    case smPBRMetallicRoughness:
    case smPBRSpecularGlossiness: {
      vec4 diffuseColorAlpha = vec4(1.0);
      vec4 baseColor = vec4(1.0);
      float ior = material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.x;
      vec3 F0 = vec3((abs(ior - 1.5) < 1e-6) ? 0.04 : pow((ior - 1.0) / (ior + 1.0), 2.0));
      vec3 F90 = vec3(1.0);
      float perceptualRoughness = 1.0;
      float specularWeight = 1.0;
      switch (shadingModel) {
        case smPBRMetallicRoughness: {
          vec3 specularColorFactor = material.specularFactor.xyz;
          specularWeight = material.specularFactor.w;
          if ((flags & (1u << 9u)) != 0u) {
            specularWeight *= textureFetch(10, vec4(1.0), false).w;
            specularColorFactor *= textureFetch(11, vec4(1.0), true).xyz;
          }
          vec3 dielectricSpecularF0 = clamp(F0 * specularColorFactor, vec3(0.0), vec3(1.0));
          baseColor = textureFetch(0, vec4(1.0), true) * material.baseColorFactor;
          vec2 metallicRoughness = clamp(textureFetch(1, vec4(1.0), false).zy * material.metallicRoughnessNormalScaleOcclusionStrengthFactor.xy, vec2(0.0, 1e-3), vec2(1.0));
          diffuseColorAlpha = vec4(max(vec3(0.0), baseColor.xyz * (1.0 - metallicRoughness.x)), baseColor.w);
          F0 = mix(dielectricSpecularF0, baseColor.xyz, metallicRoughness.x);
          perceptualRoughness = metallicRoughness.y;
          break;
        }
        case smPBRSpecularGlossiness: {
          vec4 specularGlossiness = textureFetch(1, vec4(1.0), true) * vec4(material.specularFactor.xyz, material.metallicRoughnessNormalScaleOcclusionStrengthFactor.y);
          baseColor = textureFetch(0, vec4(1.0), true) * material.baseColorFactor;
          F0 = specularGlossiness.xyz;
          diffuseColorAlpha = vec4(baseColor.xyz * max(0.0, 1.0 - max(max(F0.x, F0.y), F0.z)), baseColor.w);
          perceptualRoughness = clamp(1.0 - specularGlossiness.w, 1e-3, 1.0);
          break;
        }
      }

#undef UseGeometryRoughness
#ifdef UseGeometryRoughness

      const float minimumRoughness = 0.0525;
      float geometryRoughness;
      {
        vec3 dxy = max(abs(dFdx(workNormal)), abs(dFdy(workNormal)));
        geometryRoughness = max(max(dxy.x, dxy.y), dxy.z);
      }

      perceptualRoughness = min(max(perceptualRoughness, minimumRoughness) + geometryRoughness, 1.0);

#else 

      // Vlachos 2015, "Advanced VR Rendering"
      // Kaplanyan 2016, "Stable specular highlights"
      // Tokuyoshi 2017, "Error Reduction and Simplification for Shading Anti-Aliasing"
      // Tokuyoshi and Kaplanyan 2019, "Improved Geometric Specular Antialiasing"
      // Tokuyoshi and Kaplanyan 2021, "Stable Geometric Specular Antialiasing with Projected-Space NDF Filtering"
      // ===========================================================================================================
      // In the original paper, this implementation is intended for deferred rendering, but here it is also used 
      // for forward rendering (as described in Tokuyoshi and Kaplanyan 2019 and 2021). This is mainly because 
      // the forward version requires an expensive transformation of the half-vector by the tangent frame for each
      // light. Thus, this is an approximation based on world-space normals, but it works well enough for what is 
      // needed and is an clearly improvement over the implementation based on Vlachos 2015.
      float kernelRoughness;
      {
        const float SIGMA2 = 0.15915494, KAPPA = 0.18;        
        vec3 dx = dFdx(workNormal), dy = dFdy(workNormal);
        kernelRoughness = min(KAPPA, (2.0 * SIGMA2) * (dot(dx, dx) + dot(dy, dy)));
      }

      perceptualRoughness = sqrt(clamp((perceptualRoughness * perceptualRoughness) + kernelRoughness, 0.0, 1.0));

#endif

      float alphaRoughness = perceptualRoughness * perceptualRoughness;

      vec3 normal;
      if ((textureFlags.x & (1 << 2)) != 0) {
        vec4 normalTexture = textureFetch(2, vec2(0.0, 1.0).xxyx, false);
        normal = normalize(                                                                                                                      //
            mat3(normalize(workTangent), normalize(workBitangent), normalize(workNormal)) *                                                            //
            normalize((normalTexture.xyz - vec3(0.5)) * (vec2(material.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0))  //
        );
      } else {
        normal = normalize(workNormal);
      }
      normal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;

      vec4 occlusionTexture = textureFetch(3, vec4(1.0), false);

      cavity = clamp(mix(1.0, occlusionTexture.x, material.metallicRoughnessNormalScaleOcclusionStrengthFactor.w), 0.0, 1.0);

      vec4 emissiveTexture = textureFetch(4, vec4(1.0), true);

      float transparency = 0.0;
      float refractiveAngle = 0.0;
      float shadow = 1.0;
      float screenSpaceAmbientOcclusion = 1.0;
  #if defined(ALPHATEST) || defined(LOOPOIT) || defined(LOCKOIT) || defined(WBOIT) || defined(MBOIT) || defined(DFAOIT) || defined(BLEND) || defined(ENVMAP)
      ambientOcclusion = 1.0;
  #else      
      ivec2 ambientOcclusionTextureSize = ivec2(textureSize(uPassTextures[0], 0).xy);
  #if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS) || defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)
      screenSpaceAmbientOcclusion = texelFetch(uPassTextures[0], ivec3(min(ivec2(gl_FragCoord.xy), ambientOcclusionTextureSize - ivec2(1)), int(gl_ViewIndex)), 0).x;
      ambientOcclusion = screenSpaceAmbientOcclusion;
      //ambientOcclusion = ((textureFlags.x & (1 << 3)) != 0) ? 1.0 : screenSpaceAmbientOcclusion;
  #else
      ambientOcclusion = ((textureFlags.x & (1 << 3)) != 0) ? 1.0 : texelFetch(uPassTextures[0], ivec3(min(ivec2(gl_FragCoord.xy), ambientOcclusionTextureSize - ivec2(1)), int(gl_ViewIndex)), 0).x;
      screenSpaceAmbientOcclusion = ambientOcclusion;
  #endif
  #endif

      vec3 viewDirection = normalize(-inCameraRelativePosition);

      if ((flags & (1u << 10u)) != 0u) {
        iridescenceFresnel = F0;
        iridescenceF0 = F0;
        iridescenceFactor = material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.y * (((textureFlags.x & (1 << 12)) != 0) ? textureFetch(12, vec4(1.0), false).x : 1.0);
        iridescenceIor = material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.z;
        if ((textureFlags.x & (1 << 12)) != 0){
          iridescenceThickness = mix(material.iorIridescenceFactorIridescenceIorIridescenceThicknessMinimum.w, material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.x, textureFetch(13, vec4(1.0), false).y);  
        }else{
          iridescenceThickness = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.x;  
        }
        if(iridescenceThickness == 0.0){
          iridescenceFactor = 0.0;
        }  
        if(iridescenceFactor > 0.0){
          float NdotV = clamp(dot(normal, viewDirection), 0.0, 1.0);
          iridescenceFresnel = evalIridescence(1.0, iridescenceIor, NdotV, iridescenceThickness, F0);
          iridescenceF0 = Schlick_to_F0(iridescenceFresnel, NdotV);          
        }
      }

#if defined(TRANSMISSION)
      if ((flags & (1u << 11u)) != 0u) {
        transmissionFactor = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.y * (((textureFlags.x & (1 << 14)) != 0) ? textureFetch(14, vec4(1.0), false).x : 1.0);  
        if ((flags & (1u << 12u)) != 0u) {
          volumeThickness = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.z * (((textureFlags.x & (1 << 15)) != 0) ? textureFetch(15, vec4(1.0), false).y : 1.0);  
          volumeAttenuationDistance = material.iridescenceThicknessMaximumTransmissionFactorVolumeThicknessFactorVolumeAttenuationDistance.w;        
          volumeAttenuationColor = uintBitsToFloat(material.volumeAttenuationColorAnisotropyStrengthAnisotropyRotation.xyz);        
        }
        if((flags & (1u << 14u)) != 0u){
          volumeDispersion = uintBitsToFloat(material.dispersionUnused.x);
        }
      }
#endif

      vec3 imageLightBasedLightDirection = vec3(0.0, 0.0, -1.0);

      vec3 sheenColor = vec3(0.0);
      float sheenRoughness = 0.0;
      if ((flags & (1u << 7u)) != 0u) {
        sheenColor = material.sheenColorFactorSheenRoughnessFactor.xyz;
        sheenRoughness = material.sheenColorFactorSheenRoughnessFactor.w;
        if ((textureFlags.x & (1 << 5)) != 0) {
          sheenColor *= textureFetch(5, vec4(1.0), true).xyz;
        }
        if ((textureFlags.x & (1 << 6)) != 0) {
          sheenRoughness *= textureFetch(6, vec4(1.0), true).x;
        }
#undef UseGeometryRoughness
#ifdef UseGeometryRoughness
        sheenRoughness = min(max(sheenRoughness, minimumRoughness) + geometryRoughness, 1.0);
#else        
        sheenRoughness = sqrt(clamp((sheenRoughness * sheenRoughness) + kernelRoughness, 0.0, 1.0));
#endif
        sheenRoughness = max(sheenRoughness, 1e-7);
      }

      vec3 clearcoatF0 = vec3(0.04);
      vec3 clearcoatF90 = vec3(0.0);
      vec3 clearcoatNormal = normal;
      float clearcoatFactor = 1.0;
      float clearcoatRoughness = 1.0;
      if ((flags & (1u << 8u)) != 0u) {
        clearcoatFactor = material.clearcoatFactorClearcoatRoughnessFactor.x;
        clearcoatRoughness = material.clearcoatFactorClearcoatRoughnessFactor.y;
        if ((textureFlags.x & (1 << 7)) != 0) {
          clearcoatFactor *= textureFetch(7, vec4(1.0), false).x;
        }
        if ((textureFlags.x & (1 << 8)) != 0) {
          clearcoatRoughness *= textureFetch(8, vec4(1.0), false).y;
        }
        if ((textureFlags.x & (1 << 9)) != 0) {
          vec4 normalTexture = textureFetch(9, vec2(0.0, 1.0).xxyx, false);
          clearcoatNormal = normalize(mat3(normalize(workTangent), normalize(workBitangent), normalize(workNormal)) * normalize((normalTexture.xyz - vec3(0.5)) * (vec2(material.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0)));
        } else {
          clearcoatNormal = normalize(workNormal);
        }
        clearcoatNormal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;
#ifdef UseGeometryRoughness        
        clearcoatRoughness = min(max(clearcoatRoughness, minimumRoughness) + geometryRoughness, 1.0);
#else
        clearcoatRoughness = sqrt(clamp((clearcoatRoughness * clearcoatRoughness) + kernelRoughness, 0.0, 1.0));
#endif
      }

      specularOcclusion = getSpecularOcclusion(clamp(dot(normal, viewDirection), 0.0, 1.0), cavity * ambientOcclusion, alphaRoughness);

#ifdef ENABLE_ANISOTROPIC
      if (anisotropyActive = ((flags & (1u << 13u)) != 0u)) {
        vec2 ansitropicStrengthAnsitropicRotation = unpackHalf2x16(material.volumeAttenuationColorAnisotropyStrengthAnisotropyRotation.w);        
        vec2 directionRotation = vec2(sin(vec2(ansitropicStrengthAnsitropicRotation.y) + vec2(1.5707963267948966, 0.0)));
        mat2 rotationMatrix = mat2(directionRotation.x, directionRotation.y, -directionRotation.y, directionRotation.x);
        vec3 anisotropySample = textureFetch(16, vec4(1.0, 0.5, 1.0, 1.0), false).xyz;
        vec2 direction = rotationMatrix * fma(anisotropySample.xy, vec2(2.0), vec2(-1.0));
        anisotropyT = mat3(workTangent, workBitangent, normal) * normalize(vec3(direction, 0.0));
        anisotropyB = cross(workNormal, anisotropyT);
        anisotropyStrength = clamp(ansitropicStrengthAnsitropicRotation.x * anisotropySample.z, 0.0, 1.0);
        alphaRoughnessAnisotropyT = mix(alphaRoughness, 1.0, anisotropyStrength * anisotropyStrength);
        alphaRoughnessAnisotropyB = clamp(alphaRoughness, 1e-3, 1.0);
        anisotropyTdotV = dot(anisotropyT, viewDirection);
        anisotropyBdotV = dot(anisotropyB, viewDirection);   
      }
#endif

#define LIGHTING_IMPLEMENTATION
#include "lighting.glsl"
#undef LIGHTING_IMPLEMENTATION

#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS)
      {
        vec3 volumeSphericalHarmonics[9];
        globalIlluminationVolumeLookUp(volumeSphericalHarmonics, inWorldSpacePosition.xyz, vec3(0.0), normal.xyz);
#if 0
        vec3 shResidualDiffuse = max(vec3(0.0), globalIlluminationDecodeColor(globalIlluminationCompressedSphericalHarmonicsDecodeWithCosineLobe(normal, volumeSphericalHarmonics)));
        diffuseOutput += shResidualDiffuse * baseColor.xyz * screenSpaceAmbientOcclusion * cavity;
#else
        vec3 shAmbient = vec3(0.0), shDominantDirectionalLightColor = vec3(0.0), shDominantDirectionalLightDirection = vec3(0.0);
        globalIlluminationSphericalHarmonicsExtractAndSubtract(volumeSphericalHarmonics, shAmbient, shDominantDirectionalLightColor, shDominantDirectionalLightDirection);
        vec3 shResidualDiffuse = max(vec3(0.0), globalIlluminationDecodeColor(globalIlluminationCompressedSphericalHarmonicsDecodeWithCosineLobe(normal, volumeSphericalHarmonics)));
        diffuseOutput += shResidualDiffuse * baseColor.xyz * screenSpaceAmbientOcclusion * cavity;
        doSingleLight(shDominantDirectionalLightColor,                    //
                      vec3(screenSpaceAmbientOcclusion * cavity),         //
                      -shDominantDirectionalLightDirection,               //
                      normal.xyz,                                         //
                      diffuseColorAlpha.xyz,                              //
                      F0,                                                 //
                      F90,                                                //
                      viewDirection,                                      //
                      refractiveAngle,                                    //
                      transparency,                                       //
                      alphaRoughness,                                     //
                      cavity,                                             //
                      sheenColor,                                         //
                      sheenRoughness,                                     //
                      clearcoatNormal,                                    //
                      clearcoatF0,                                        //
                      clearcoatRoughness,                                 //
                      specularWeight);                                    //
#endif
      }
#elif defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)
      float iblWeight = 1.0; 
      {
        if(dot(diffuseColorAlpha.xyz, vec3(1.0)) > 1e-6){
          vec4 c = cvctIndirectDiffuseLight(inWorldSpacePosition.xyz, normal.xyz);
          diffuseOutput += c.xyz * diffuseColorAlpha.xyz * screenSpaceAmbientOcclusion * cavity * OneOverPI;
          iblWeight = clamp(1.0 - c.w, 0.0, 1.0);
        }
        if(dot(F0, vec3(1.0)) > 1e-6){
          specularOutput += cvctIndirectSpecularLight(inWorldSpacePosition.xyz, normal.xyz, viewDirection, cvctRoughnessToVoxelConeTracingApertureAngle(perceptualRoughness), 1e+24) * F0 * cavity * OneOverPI;
        }
      }
#endif
#if !defined(REFLECTIVESHADOWMAPOUTPUT) 
#if !(defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS))
#if defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)
//    float iblWeight = 1.0; 
#else
      float iblWeight = 1.0; // for future sky occulsion 
#endif
      diffuseOutput += getIBLRadianceLambertian(normal, viewDirection, perceptualRoughness, diffuseColorAlpha.xyz, F0, specularWeight) * iblWeight;
      specularOutput += getIBLRadianceGGX(normal, perceptualRoughness, F0, specularWeight, viewDirection, litIntensity, imageLightBasedLightDirection) * iblWeight;
      if ((flags & (1u << 7u)) != 0u) {
        sheenOutput += getIBLRadianceCharlie(normal, viewDirection, sheenRoughness, sheenColor) * iblWeight;
      }
      if ((flags & (1u << 8u)) != 0u) {
        clearcoatOutput += getIBLRadianceGGX(clearcoatNormal, clearcoatRoughness, clearcoatF0.xyz, 1.0, viewDirection, litIntensity, imageLightBasedLightDirection) * iblWeight;
        clearcoatFresnel = F_Schlick(clearcoatF0, clearcoatF90, clamp(dot(clearcoatNormal, viewDirection), 0.0, 1.0)) * iblWeight;
      }
#endif
#if defined(TRANSMISSION)
      if ((flags & (1u << 11u)) != 0u) {
        transmissionOutput += getIBLVolumeRefraction(normal.xyz, viewDirection,
                                                     perceptualRoughness,
                                                     diffuseColorAlpha.xyz, F0, F90,
                                                     inWorldSpacePosition,
                                                     ior, 
                                                     volumeThickness, 
                                                     volumeAttenuationColor, 
                                                     volumeAttenuationDistance,
                                                     volumeDispersion);        
      }
#endif
#endif
#if defined(REFLECTIVESHADOWMAPOUTPUT)
      vec3 emissiveOutput = vec3(0.0); // No emissive output for RSMs
#else
      vec3 emissiveOutput = emissiveTexture.xyz * material.emissiveFactor.xyz * material.emissiveFactor.w;
#endif
      color = vec2(0.0, diffuseColorAlpha.w).xxxy;
#ifndef EXTRAEMISSIONOUTPUT
      color.xyz += emissiveOutput;
#endif
#if defined(TRANSMISSION)
      color.xyz += mix(diffuseOutput, transmissionOutput, transmissionFactor);
#else
      color.xyz += diffuseOutput;
#endif
#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS)
#if 0
      color.xyz += globalIlluminationCascadeVisualizationColor(inWorldSpacePosition).xyz;
#endif
#elif defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)
#if 0
      color.xyz += cvctCascadeVisualizationColor(inWorldSpacePosition).xyz;
#endif
#endif
      color.xyz += specularOutput;
      color.xyz = fma(color.xyz, vec3(albedoSheenScaling), sheenOutput);
      color.xyz = fma(color.xyz, vec3(1.0 - (clearcoatFactor * clearcoatFresnel)), clearcoatOutput);
#ifdef EXTRAEMISSIONOUTPUT
      emissionColor.xyz = emissiveOutput * (1.0 - (clearcoatFactor * clearcoatFresnel));
#endif
      break;
    }
    case smUnlit: {
      color = textureFetch(0, vec4(1.0), true) * material.baseColorFactor * vec2((litIntensity * 0.25) + 0.75, 1.0).xxxy;
      break;
    }
  }
#endif
  float alpha = color.w * inColor0.w, outputAlpha = ((flags & 32) != 0) ? (color.w * inColor0.w) : 1.0; // AMD GPUs under Linux doesn't like mix(1.0, color.w * inColor0.w, float(int(uint((flags >> 5u) & 1u)))); due to the unsigned int stuff
  vec4 finalColor = vec4(color.xyz * inColor0.xyz, outputAlpha);
#if !(defined(WBOIT) || defined(MBOIT) || defined(VOXELIZATION))
#ifndef BLEND 
  outFragColor = vec4(clamp(finalColor.xyz, vec3(-65504.0), vec3(65504.0)), finalColor.w);
#endif
#ifdef EXTRAEMISSIONOUTPUT
  outFragEmission = vec4(clamp(emissionColor.xyz * inColor0.xyz, vec3(-65504.0), vec3(65504.0)), outputAlpha);
#endif
#endif
#endif

#if defined(ALPHATEST)
  #if defined(NODISCARD)  
    float fragDepth;
  #endif
  if (alpha < uintBitsToFloat(material.alphaCutOffFlagsTex0Tex1.x)) {
  #if defined(WBOIT) || defined(LOCKOIT) || defined(DFAOIT) || defined(LOCKOIT_PASS2)
    finalColor = vec4(alpha = 0.0);    
  #elif defined(LOCKOIT_PASS1)
    alpha = 0.0;    
  #elif defined(MBOIT)
    #if defined(MBOIT) && defined(MBOITPASS1)    
      alpha = 0.0;    
    #else
      finalColor = vec4(alpha = 0.0);      
    #endif
  #else 
    #if defined(NODISCARD)  
      // Workaround for Intel (i)GPUs, which've problems with discarding fragments in 2x2 fragment blocks at alpha-test usage
#ifdef USE_SPECIALIZATION_CONSTANTS
      fragDepth = UseReversedZ ? -0.1 : 1.1;      
#else
      #if defined(REVERSEDZ)
        fragDepth = -0.1;
      #else
        fragDepth = 1.1;
      #endif
#endif
    #else
      #if defined(USEDEMOTE)
        demote;
      #else
        discard;
      #endif
    #endif
  #endif
  }else{
  #if defined(NODISCARD)  
    fragDepth = gl_FragCoord.z;
  #endif
  #if defined(WBOIT) || defined(MBOIT) || defined(LOCKOIT) || defined(LOOPOIT) || defined(DFAOIT)
    #if defined(WBOIT) || defined(LOCKOIT) || defined(LOOPOIT_PASS2) || defined(DFAOIT)
      finalColor.w = alpha = 1.0;    
    #elif defined(LOOPOIT_PASS1)
      alpha = 1.0;    
    #elif defined(MBOIT) && defined(MBOITPASS1)    
      alpha = 1.0;    
    #else
      finalColor.w = alpha = 1.0;    
    #endif
  #endif
  }
  #if defined(NODISCARD)  
    gl_FragDepth = fragDepth;
  #endif
  #if !(defined(WBOIT) || defined(MBOIT) || defined(LOCKOIT) || defined(LOOPOIT) || defined(DFAOIT))
    #ifdef MSAA
      #if 0
        vec2 alphaTextureSize = vec2(texture2DSize(0));
        vec2 alphaTextureUV = textureUV(0) * alphaTextureSize;
        vec4 alphaDUV = vec4(vec2(dFdx(alphaTextureUV)), vec2(dFdy(alphaTextureUV)));
        alpha *= 1.0 + (max(0.0, max(dot(alphaDUV.xy, alphaDUV.xy), dot(alphaDUV.zw, alphaDUV.zw)) * 0.5) * 0.25);
      #endif
      #if 1
        alpha = clamp(((alpha - uintBitsToFloat(material.alphaCutOffFlagsTex0Tex1.x)) / max(fwidth(alpha), 1e-4)) + 0.5, 0.0, 1.0);
      #endif  
      if (alpha < 1e-2) {
        alpha = 0.0;
      }
      #ifndef DEPTHONLY  
        outFragColor.w = finalColor.w = alpha;
      #endif
    #endif
  #endif
#endif

#if !defined(VOXELIZATION)
  const bool additiveBlending = false; // Mesh does never use additive blending currently, so static compile time constant folding is possible here.
   
#define TRANSPARENCY_IMPLEMENTATION
#include "transparency.glsl"
#undef TRANSPARENCY_IMPLEMENTATION

#if defined(VELOCITY)

  outFragVelocity = (((inCurrentClipSpace.xy / inCurrentClipSpace.w) - inJitter.xy) - ((inPreviousClipSpace.xy / inPreviousClipSpace.w) - inJitter.zw)) * 0.5;
  
#elif defined(REFLECTIVESHADOWMAPOUTPUT)

  vec3 normal = normalize(workNormal);
/*normal /= (abs(normal.x) + abs(normal.y) + abs(normal.z));
  outFragNormalUsed = vec4(vec3(fma(normal.xx, vec2(0.5, -0.5), vec2(fma(normal.y, 0.5, 0.5))), clamp(normal.z * 3.402823e+38, 0.0, 1.0)), 1.0);*/  
  outFragNormalUsed = vec4(vec3(fma(normal.xyz, vec3(0.5), vec3(0.5))), 1.0);  

  //outFragPosition = inWorldSpacePosition.xyz;

#endif
#endif

#ifdef VOXELIZATION
  #include "voxelization_fragment.glsl"   
#endif

}

/*oid main() {
  outFragColor = vec4(vec3(mix(0.25, 1.0, max(0.0, dot(workNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
//outFragColor = vec4(texture(uTexture, inTexCoord)) * vec4(vec3(mix(0.25, 1.0, max(0.0, dot(workNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
}*/
