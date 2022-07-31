#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec3 inPosition;
layout(location = 1) in uint inMaterialID;
layout(location = 2) in vec4 inNormalSign;
layout(location = 3) in vec3 inTangent;
layout(location = 4) in vec2 inTexCoord0;
layout(location = 5) in vec2 inTexCoord1;
layout(location = 6) in vec4 inColor0;
layout(location = 7) in vec3 inModelScale;
#ifdef VELOCITY
layout(location = 8) in vec3 inPreviousPosition;
#endif

layout(location = 0) out vec3 outWorldSpacePosition;
layout(location = 1) out vec3 outViewSpacePosition;
layout(location = 2) out vec3 outCameraRelativePosition;
layout(location = 3) out vec3 outTangent;
layout(location = 4) out vec3 outBitangent;
layout(location = 5) out vec3 outNormal;
layout(location = 6) out vec2 outTexCoord0;
layout(location = 7) out vec2 outTexCoord1;
layout(location = 8) out vec4 outColor0;
layout(location = 9) out vec3 outModelScale;
layout(location = 10) flat out uint outMaterialID;
layout(location = 11) flat out int outViewIndex;
#ifdef VELOCITY
layout(location = 12) out vec4 outPreviousClipSpace;
layout(location = 13) out vec4 outCurrentClipSpace;
#endif

/* clang-format off */
layout (push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
} pushConstants;

// Global descriptor set

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(std140, set = 0, binding = 0) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

#ifdef SHADOWMAP
layout(std140, set = 1, binding = 0) uniform uboShadowMap {
  vec4 lightPositionDirection;
  vec4 constantBiasNormalBiasSlopeBiasClamp; // x = constant bias, y = normal bias, z = slope Bias, w = clamp
} uShadowMap;
#endif

out gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
};

/* clang-format on */

void main() {

  uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);

  mat3 tangentSpace;
  {
    vec3 tangent = inTangent.xyz;
    vec3 normal = inNormalSign.xyz;
    tangentSpace = mat3(tangent, normalize(cross(normal, tangent)) * inNormalSign.w, normal);
  }

  tangentSpace[0] = normalize(tangentSpace[0]);
  tangentSpace[1] = normalize(tangentSpace[1]);
  tangentSpace[2] = normalize(tangentSpace[2]);
  
  View view = uView.views[viewIndex];

#if 1
  // The actual standard approach
  vec3 cameraPosition = view.inverseViewMatrix[3].xyz;
#else
  // This approach assumes that the view matrix has no scaling or skewing, but only rotation and translation.
  vec3 cameraPosition = (-view.viewMatrix[3].xyz) * mat3(view.viewMatrix);
#endif

  vec3 position = inPosition;

#ifdef SHADOWMAP
  vec3 lightDirection = normalize(
                          (uShadowMap.lightPositionDirection.w > 0.5) ?
                            (-uShadowMap.lightPositionDirection.xyz) :
                            (uShadowMap.lightPositionDirection.xyz - position)
                        );
  vec3 normal = normalize(tangentSpace[2]);
  {
    float cos_alpha = clamp(dot(normal, lightDirection), 0.0, 1.0);
    float offset_scale_N = sqrt(1.0 - (cos_alpha * cos_alpha)); // sin(acos(L·N))
    float offset_scale_L = offset_scale_N / cos_alpha;          // tan(acos(L·N))
    vec4 values = uShadowMap.constantBiasNormalBiasSlopeBiasClamp;
    vec2 offsets = fma(vec2(offset_scale_N, min(2.0, offset_scale_L)), vec2(values.yz), vec2(0.0, values.x));
    if(values.w > 1e-6){
      offsets.xy = clamp(offsets.xy, vec2(-values.w), vec2(values.w));
    }
    position -= (normal * offsets.x) + (lightDirection * offsets.y);
  }
#endif

  vec4 worldSpacePosition = vec4(position, 1.0);
  worldSpacePosition.xyz /= worldSpacePosition.w;

  vec4 viewSpacePosition = view.viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outWorldSpacePosition = worldSpacePosition.xyz;
  outViewSpacePosition = viewSpacePosition.xyz;
  outCameraRelativePosition = worldSpacePosition.xyz - cameraPosition;
  outTangent = tangentSpace[0];
  outBitangent = tangentSpace[1];
#ifdef VELOCITY
  outNormal = normalize(transpose(mat3(view.inverseViewMatrix)) * tangentSpace[2]);
#else
  outNormal = tangentSpace[2];
#endif
  outTexCoord0 = inTexCoord0;
  outTexCoord1 = inTexCoord1;
  outColor0 = inColor0;
  outModelScale = inModelScale;
  outMaterialID = inMaterialID;
  outViewIndex = int(viewIndex); 

#ifdef VELOCITY
   View previousView = uView.views[viewIndex + pushConstants.countViews];
   outPreviousClipSpace = (previousView.projectionMatrix * previousView.viewMatrix) * vec4(inPreviousPosition, 1.0);
   gl_Position = outCurrentClipSpace = (view.projectionMatrix * view.viewMatrix) * vec4(position, 1.0);
#else
  gl_Position = (view.projectionMatrix * view.viewMatrix) * vec4(position, 1.0);
#endif

  gl_PointSize = 1.0;
}
