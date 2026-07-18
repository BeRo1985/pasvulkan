#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

// Atmosphere-independent distance fog. Fullscreen post-process pass at the start of the HDR
// post chain: it reads the composited scene colour (as an input attachment) and the scene
// depth, reconstructs the view-space position, and blends a fog colour over the pixel by an
// exponential distance term with an optional exponential world-height falloff. Two colour
// sources are provided as separate shader variants: a fixed fog colour, or the environment
// (IBL) cube map sampled in the view direction (define FOG_SAMPLE_ENVIRONMENT). Background /
// sky pixels (far plane) are left untouched so the fog never hides the sky. Under MSAA (define
// FOG_MSAA) the raw multisampled depth is read instead of the reduced depth pyramid and the fog
// factor is computed per sample and averaged (sky samples contribute zero), so a silhouette
// pixel - whose resolved colour is a coverage mix of several surfaces - gets the matching
// fractional fog; a single reduced depth would either fog the sky fraction or leave the
// geometry fraction unfogged, a visible rim line along silhouettes.

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(push_constant) uniform PushConstants {
  vec4 fogColor;           // rgb = fixed fog colour (FixedColor variant), a = unused
  uint viewBaseIndex;      //
  float density;           // base fog density
  float densityMultiplier; // per-frame multiplier (fog zones)
  float heightFalloff;     // exponential world-height falloff, 0 = disabled
  float heightBase;        // world Y at which the height falloff starts
  float environmentLOD;    // cube-map LOD for the environment-colour variant
  uint countSamples;       // MSAA sample count of the raw depth (FOG_MSAA variant)
} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassColor;

#ifdef FOG_MSAA
layout(set = 0, binding = 1) uniform sampler2DMSArray uTextureDepth;
#else
layout(set = 0, binding = 1) uniform sampler2DArray uTextureDepth;
#endif

layout(std140, set = 0, binding = 2) uniform uboViews {
  View views[256];
} uView;

#ifdef FOG_SAMPLE_ENVIRONMENT
layout(set = 0, binding = 3) uniform samplerCube uEnvironmentTexture;
#endif

// The fog blend factor for one raw depth value. Returns false (factor zero) for a sky / far
// plane sample or a degenerate reconstruction, so those are left untouched; on success it also
// outputs the reconstructed view-space position (camera at the origin), which the environment
// variant turns into the fog colour lookup direction.
bool computeFogAmount(const in float rawDepth,
                      const in bool reversedZ,
                      const in mat4 inverseProjectionMatrix,
                      const in mat4 inverseViewMatrix,
                      out float fogAmount,
                      out vec3 viewPosition){

  fogAmount = 0.0;
  viewPosition = vec3(0.0);

  bool hasGeometry = reversedZ ? (rawDepth > 0.0) : (rawDepth < 1.0);
  if(!hasGeometry){
    return false;
  }

  vec4 viewPositionH = inverseProjectionMatrix * vec4(fma(inTexCoord, vec2(2.0), vec2(-1.0)), rawDepth, 1.0);
  viewPosition = viewPositionH.xyz / viewPositionH.w;
  if(any(isinf(viewPosition)) || any(isnan(viewPosition))){
    return false;
  }
  float dist = length(viewPosition);

  float density = pushConstants.density * pushConstants.densityMultiplier;

  vec3 worldPosition = (inverseViewMatrix * vec4(viewPosition, 1.0)).xyz;
  if(pushConstants.heightFalloff > 0.0){
    density *= exp(-max(0.0, worldPosition.y - pushConstants.heightBase) * pushConstants.heightFalloff);
  }

  fogAmount = clamp(1.0 - exp(-max(0.0, dist * density)), 0.0, 1.0);

  return true;

}

void main(){

  vec4 color = subpassLoad(uSubpassColor);
  color.xyz = clamp(color.xyz, vec3(0.0), vec3(65504.0));

  uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
  mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
  mat4 inverseProjectionMatrix = uView.views[viewIndex].inverseProjectionMatrix;
  mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;

  // Reversed-Z is read straight from the projection matrix (as the atmosphere pass does), so this is
  // robust to the engine's reversed-Z infinite-far setup.
  bool reversedZ = projectionMatrix[2][3] < -1e-7;

#ifdef FOG_MSAA
  // Per-sample fog factors, averaged over the GEOMETRY samples only: a silhouette pixel's
  // resolved colour is a coverage mix of geometry and sky, and since the sky already renders as
  // (essentially) the fog colour, fogging the sky fraction is a no-op - so applying the geometry
  // samples' mean factor to the whole resolved pixel reproduces the true per-sample result. An
  // average over ALL samples would dilute the factor by the sky coverage and leave the geometry
  // fraction under-fogged, a residual dark rim along silhouettes. The view direction of the
  // environment colour is taken from any geometry sample - the per-sample directions of one
  // pixel are visually identical.
  float fogAmount = 0.0;
  vec3 fogViewPosition = vec3(0.0);
  int countGeometrySamples = 0;
  int countSamples = int(pushConstants.countSamples);
  for(int sampleIndex = 0; sampleIndex < countSamples; sampleIndex++){
    float rawDepth = texelFetch(uTextureDepth, ivec3(gl_FragCoord.xy, gl_ViewIndex), sampleIndex).x;
    float sampleFogAmount;
    vec3 sampleViewPosition;
    if(computeFogAmount(rawDepth, reversedZ, inverseProjectionMatrix, inverseViewMatrix, sampleFogAmount, sampleViewPosition)){
      fogAmount += sampleFogAmount;
      fogViewPosition = sampleViewPosition;
      countGeometrySamples++;
    }
  }
  if(countGeometrySamples == 0){
    outFragColor = color;
    return;
  }
  fogAmount /= float(countGeometrySamples);
#else
  // The single-sample path: one depth per pixel, sky / degenerate pixels are left untouched.
  float rawDepth = texelFetch(uTextureDepth, ivec3(gl_FragCoord.xy, gl_ViewIndex), 0).x;
  float fogAmount;
  vec3 fogViewPosition;
  if(!computeFogAmount(rawDepth, reversedZ, inverseProjectionMatrix, inverseViewMatrix, fogAmount, fogViewPosition)){
    outFragColor = color;
    return;
  }
#endif

#ifdef FOG_SAMPLE_ENVIRONMENT
  vec3 worldDirection = normalize(mat3(inverseViewMatrix) * normalize(fogViewPosition));
  vec3 fogColor = textureLod(uEnvironmentTexture, worldDirection, pushConstants.environmentLOD).xyz;
#else
  vec3 fogColor = pushConstants.fogColor.xyz;
#endif

  outFragColor = vec4(clamp(mix(color.xyz, fogColor, fogAmount), vec3(0.0), vec3(65504.0)), color.w);

}
