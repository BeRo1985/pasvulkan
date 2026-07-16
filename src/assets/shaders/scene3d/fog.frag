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
// sky pixels (far plane) are left untouched so the fog never hides the sky.

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
  float reversedZ;         // 1.0 = reversed-Z depth buffer, 0.0 = normal
  float pad0;              //
} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassColor;

layout(set = 0, binding = 1) uniform sampler2DArray uTextureDepth;

layout(std140, set = 0, binding = 2) uniform uboViews {
  View views[256];
} uView;

#ifdef FOG_SAMPLE_ENVIRONMENT
layout(set = 0, binding = 3) uniform samplerCube uEnvironmentTexture;
#endif

void main(){

  vec4 color = subpassLoad(uSubpassColor);
  color.xyz = clamp(color.xyz, vec3(0.0), vec3(65504.0));

  uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
  mat4 inverseProjectionMatrix = uView.views[viewIndex].inverseProjectionMatrix;
  mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;

  float rawDepth = texelFetch(uTextureDepth, ivec3(gl_FragCoord.xy, gl_ViewIndex), 0).x;

  // Skybox / background sits on the far plane (reversed-Z: 0.0, normal-Z: 1.0); leave it as is.
  bool isBackground = (pushConstants.reversedZ > 0.5) ? (rawDepth <= 0.0) : (rawDepth >= 1.0);
  if(isBackground){
    outFragColor = color;
    return;
  }

  // View-space position of this pixel (camera at the origin).
  vec4 viewPositionH = inverseProjectionMatrix * vec4(fma(inTexCoord, vec2(2.0), vec2(-1.0)), rawDepth, 1.0);
  vec3 viewPosition = viewPositionH.xyz / viewPositionH.w;
  float dist = length(viewPosition);

  float density = pushConstants.density * pushConstants.densityMultiplier;

  vec3 worldPosition = (inverseViewMatrix * vec4(viewPosition, 1.0)).xyz;
  if(pushConstants.heightFalloff > 0.0){
    density *= exp(-max(0.0, worldPosition.y - pushConstants.heightBase) * pushConstants.heightFalloff);
  }

  float fogAmount = clamp(1.0 - exp(-max(0.0, dist * density)), 0.0, 1.0);

#ifdef FOG_SAMPLE_ENVIRONMENT
  vec3 worldDirection = normalize(mat3(inverseViewMatrix) * normalize(viewPosition));
  vec3 fogColor = textureLod(uEnvironmentTexture, worldDirection, pushConstants.environmentLOD).xyz;
#else
  vec3 fogColor = pushConstants.fogColor.xyz;
#endif

  outFragColor = vec4(clamp(mix(color.xyz, fogColor, fogAmount), vec3(0.0), vec3(65504.0)), color.w);

}
