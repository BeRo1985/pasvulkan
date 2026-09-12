#version 460 core

#pragma shader_stage(fragment)

#undef DEBUG

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_nonuniform_qualifier : enable
#extension GL_EXT_samplerless_texture_functions : enable
#ifdef DEBUG
#extension GL_EXT_debug_printf : enable
#endif

// The screen space water caustics, as a fullscreen draw that blends its contribution into the scene colour
// attachment, rather than as a compute shader that reads the colour, adds to it and writes it back.
//
// The arithmetic below is unchanged from that compute version, and so is everything it reads. Only the last
// two lines differ: the read-modify-write of the scene colour is left to fixed function additive blending,
// which is the same operation and costs the shader nothing.
//
// The reason for the change is not the blending, though - it is where the result may land. The colour a
// compute shader can write is a storage image, and a multisampled image cannot be one without declaring it
// per sample and paying for it per sample. So the compute version wrote into the RESOLVED copy of the scene
// colour, which with MSAA is a side copy taken early: the picture that carries on to the transparency and
// to the final resolve is the multisampled one, and the caustics never reached it. As an attachment there
// is no such split - this draw writes whichever colour the pass is given, multisampled or not.
//
// Sample shading stays off. One invocation per pixel, whose result the ROPs put into every covered sample,
// is exactly right for a screen space term like this one: it is a function of the depth and of the water
// above it, not of which sample of an edge is being looked at.

#define LIGHTCLUSTERS
#define FRUSTUMCLUSTERGRID
#define LIGHTS
#define SHADOWS

#include "bufferreference_definitions.glsl"

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

// Module-scope position aliases used by lighting.glsl / shadows.glsl.
vec3 viewSpacePosition;
vec3 worldSpacePosition;
vec3 cameraRelativePosition;
#define inViewSpacePosition viewSpacePosition
#define inWorldSpacePosition worldSpacePosition
#define inCameraRelativePosition cameraRelativePosition

// Global descriptor set (lights, LightBVH, textures, global UBOs …)
#define PLANETS
#ifdef RAYTRACING
  #define USE_MATERIAL_BUFFER_REFERENCE
#endif
#include "globaldescriptorset.glsl"
#undef PLANETS

// Pass descriptor set (views UBO, BRDF, env maps, shadow map, depth/scene images,
// frustum cluster grid at bindings 6-8)
#include "mesh_rendering_pass_descriptorset.glsl"

#ifdef FRUSTUMCLUSTERGRID
layout(set = 1, binding = 6, std140) readonly uniform FrustumClusterGridGlobals {
  uvec4 tileSizeZNearZFar;
  vec4 viewRect;
  uvec4 countLightsViewIndexSizeOffsetedViewIndex;
  uvec4 clusterSize;
  vec4 scaleBiasMax;
} uFrustumClusterGridGlobals;

layout(set = 1, binding = 7, std430) readonly buffer FrustumClusterGridIndexList {
  uint frustumClusterGridIndexList[];
};

layout(set = 1, binding = 8, std430) readonly buffer FrustumClusterGridData {
  uvec4 frustumClusterGridData[]; // x = start light index, y = count lights
};
#endif

// Per-planet texture array (set 2, bound from Planet.fPlanetDescriptorSets)
layout(set = 2, binding = 0) uniform sampler2D uPlanetTextures[];
layout(set = 2, binding = 0) uniform sampler2DArray uPlanetArrayTextures[];

// Push constants for the water render pass variant (64 bytes = 4 uvec4)
#define globalRaytracingFlags pushConstants.flags
#define inFrameIndex pushConstants.frameIndex
#define PLANET_WATER
#include "planet_renderpass.glsl"

// Per-invocation view index (set in main before shading)
int inViewIndex;

// Lighting helper function declarations
#define LIGHTING_GLOBALS
#include "lighting.glsl"
#undef LIGHTING_GLOBALS

// Dominant IBL direction (not used for caustics but referenced by some paths)
vec3 imageLightBasedLightDirection = vec3(0.0, 1.0, 0.0);

// Per-invocation view matrices
uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
mat4 viewMatrix = uView.views[viewIndex].viewMatrix;
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;
mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
mat4 inverseProjectionMatrix = uView.views[viewIndex].inverseProjectionMatrix;

// Planet-space globals – initialised per-draw in main()
float planetBottomRadius;
float planetTopRadius;
mat4 planetModelMatrix;
mat4 planetInverseModelMatrix;
const vec3 planetCenter = vec3(0.0);
vec3 workNormal; // Surface normal used by shadows.glsl for ray offsets – must be before shadows.glsl

#include "math.glsl"

#ifdef RAYTRACING
  #include "raytracing.glsl"
#endif

// Shadow sampling helpers
#define NOTEXCOORDS
#include "shadows.glsl"

#include "octahedral.glsl"
#include "octahedralmap.glsl"
#include "planet_textures.glsl"
#include "planet_water.glsl"
#include "planet_caustics.glsl"

// Per-light caustic accumulation called by lighting.glsl via PROCESSLIGHT.
vec3 causticAccumulatedColor = vec3(0.0);
vec3 causticNormal;
float causticPattern;

void processCausticsLight(const in vec3 colorIntensity, const in vec3 attenuation, const in vec3 lightDir){
#ifdef DEBUG
  ivec2 px6 = ivec2(gl_FragCoord.xy);
  ivec2 ext6 = textureSize(uPassTextures[2], 0).xy;
  if((inViewIndex == 0) && all(equal(px6, ext6 / 2))){
    float dotVal = dot(causticNormal, lightDir);
    debugPrintfEXT("caustics: PROCESSLIGHT att=%f dotNL=%f ci=%f,%f,%f\n",
                   attenuation.x, dotVal, colorIntensity.x, colorIntensity.y, colorIntensity.z);
  }
#endif
  causticAccumulatedColor += colorIntensity * attenuation.x * max(0.0, dot(causticNormal, lightDir)) * causticPattern;
}

#define PROCESSLIGHT processCausticsLight

void main(){

  int viewLocalIndex = int(gl_ViewIndex);
  ivec2 px = ivec2(gl_FragCoord.xy);
  if(viewLocalIndex >= int(pushConstants.countViews)){
    discard;
  }
  // The depth pyramid's top level, which is the full resolution one and the same size as the colour being
  // drawn into. Taken from the texture rather than from the attachment, which a fragment shader cannot ask.
  ivec2 imageExtent = textureSize(uPassTextures[2], 0).xy;
  if(any(greaterThanEqual(px, imageExtent))){
    discard;
  }

  // Propagate view index so that cluster-grid indexing in lighting.glsl works.
  inViewIndex = viewLocalIndex;

  // Read the opaque terrain depth written by the earlier depth-prepass / mipmap pass.
  float rawDepth = texelFetch(uPassTextures[2], ivec3(px, viewLocalIndex), 0).x;
#ifdef DEBUG
  if((viewLocalIndex == 0) && all(equal(px, imageExtent / 2))){
    debugPrintfEXT("caustics: px=%d,%d ext=%d,%d rawDepth=%f\n", px.x, px.y, imageExtent.x, imageExtent.y, rawDepth);
  }
#endif
  if(rawDepth >= 1.0){
    discard; // sky / far plane
  }

  // Reconstruct view-space and world-space positions from the hardware depth.
  vec2 clipXY = fma(vec2(px) + 0.5, vec2(2.0) / vec2(imageExtent), vec2(-1.0));
  vec4 viewPos4 = inverseProjectionMatrix * vec4(clipXY, rawDepth, 1.0);
  viewPos4 /= viewPos4.w;
  vec3 worldPos = (inverseViewMatrix * viewPos4).xyz;

  // Populate the position aliases expected by lighting.glsl / shadows.glsl.
  viewSpacePosition = viewPos4.xyz;
  worldSpacePosition = worldPos;
  cameraRelativePosition = worldPos - inverseViewMatrix[3].xyz;

  // Initialise planet globals from the per-planet buffer-device-address in the push constant.
  planetBottomRadius = planetData.bottomRadiusTopRadiusHeightMapScale.x;
  planetTopRadius = planetData.bottomRadiusTopRadiusHeightMapScale.y;
  planetModelMatrix = planetData.modelMatrix;
  planetInverseModelMatrix = inverse(planetModelMatrix);

  // Transform world position into the planet's local frame and compute radii.
  vec3 planetPosition = (planetInverseModelMatrix * vec4(worldPos, 1.0)).xyz;
  float groundHeight = length(planetPosition);
  if(groundHeight < 1e-3){
    discard;
  }
  vec3 sphereNormal = planetPosition / groundHeight;

  // Query the water surface radius at this point on the sphere.
  float waterHeight = getSphereHeightEx(octPlanetUnsignedEncode(sphereNormal));
#ifdef DEBUG
  if((viewLocalIndex == 0) && all(equal(px, imageExtent / 2))){
    debugPrintfEXT("caustics: groundHeight=%f waterHeight=%f\n", groundHeight, waterHeight);
  }
#endif
  if(waterHeight <= groundHeight){
    discard; // not underwater at this pixel
  }
  float waterDepth = waterHeight - groundHeight;

  // Caustic parameters packed as half-floats in waterCausticParams.
  vec2 cp0 = unpackHalf2x16(planetData.waterCausticParams.x); // x=intensity, y=scale
  float causticIntensity = cp0.x;
#ifdef DEBUG
  if((viewLocalIndex == 0) && all(equal(px, imageExtent / 2))){
    debugPrintfEXT("caustics: waterDepth=%f causticIntensity=%f waterCausticParams.x=0x%x\n", waterDepth, causticIntensity, planetData.waterCausticParams.x);
  }
#endif
  if(causticIntensity <= 0.0){
    discard;
  }
  float causticScale = cp0.y;
  vec2 cp1 = unpackHalf2x16(planetData.waterCausticParams.y); // x=fadeDepth, y=speed
  float causticFadeDepth = cp1.x;
  float causticSpeed = cp1.y;
  vec2 cp2 = unpackHalf2x16(planetData.waterCausticParams.z); // x=depthThresholdLow, y=depthThresholdHigh

  // Evaluate the animated Voronoi caustic pattern (includes depth fade).
  causticPattern = getCausticIntensity(planetPosition, pushConstants.time,
                                       causticScale, causticSpeed,
                                       causticFadeDepth, waterDepth,
                                       cp2.x, cp2.y);
#ifdef DEBUG
  if((viewLocalIndex == 0) && all(equal(px, imageExtent / 2))){
    debugPrintfEXT("caustics: causticPattern=%f time=%f waterDepth=%f\n", causticPattern, pushConstants.time, waterDepth);
  }
#endif
  if(causticPattern <= 0.0){
    discard;
  }

  // Caustic shading normal is the planet's sphere-surface normal in world space.
  causticNormal = normalize((planetModelMatrix * vec4(sphereNormal, 0.0)).xyz);
  workNormal = causticNormal;

  // Accumulate per-light caustic contributions via processCausticsLight.
  float litIntensity = 1.0;
  const bool receiveShadows = true;
  vec3 triangleNormal = causticNormal;
  causticAccumulatedColor = vec3(0.0);

#define LIGHTING_INITIALIZATION
#include "lighting.glsl"
#undef LIGHTING_INITIALIZATION

#define LIGHTING_IMPLEMENTATION
#include "lighting.glsl"
#undef LIGHTING_IMPLEMENTATION

  // The caustic contribution on its own. The pipeline blends it onto the scene colour with ONE / ONE, so
  // this is the same addition the compute version did by hand, and the zero alpha leaves the destination's
  // alpha alone, as the write-back of an unchanged alpha channel did there.
#ifdef DEBUG
  if((viewLocalIndex == 0) && all(equal(px, imageExtent / 2))){
    debugPrintfEXT("caustics: causticAccumulatedColor=%f,%f,%f\n",
                   causticAccumulatedColor.x, causticAccumulatedColor.y, causticAccumulatedColor.z);
  }
#endif
  outFragColor = vec4(causticIntensity * causticAccumulatedColor * vec3(unpackHalf2x16(planetData.waterCausticParams2.x), unpackHalf2x16(planetData.waterCausticParams2.y).x), // caustic tint color
                      0.0);

}
