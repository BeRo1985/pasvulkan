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
layout(location = 12) flat out uint outFrameIndex;
#ifdef VELOCITY
layout(location = 13) out vec4 outPreviousClipSpace;
layout(location = 14) out vec4 outCurrentClipSpace;
layout(location = 15) flat out vec4 outJitter;
#else
layout(location = 13) flat out vec2 outJitter;
#endif

/* clang-format off */
layout (push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint frameIndex;
  vec4 jitter;
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

  vec3 worldSpacePosition = position;

  vec4 viewSpacePosition = view.viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outWorldSpacePosition = worldSpacePosition;
  outViewSpacePosition = viewSpacePosition.xyz;
  outCameraRelativePosition = worldSpacePosition - cameraPosition;
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
  outFrameIndex = pushConstants.frameIndex;

#ifdef VELOCITY
  View previousView = uView.views[viewIndex + pushConstants.countAllViews];
  outCurrentClipSpace = (view.projectionMatrix * view.viewMatrix) * vec4(inPosition, 1.0);
  outPreviousClipSpace = (previousView.projectionMatrix * previousView.viewMatrix) * vec4(inPreviousPosition, 1.0);
  gl_Position = outCurrentClipSpace;
  outJitter = pushConstants.jitter;
#else
  gl_Position = (view.projectionMatrix * view.viewMatrix) * vec4(position, 1.0);
  outJitter = pushConstants.jitter.xy;
#endif

  gl_PointSize = 1.0;
}
