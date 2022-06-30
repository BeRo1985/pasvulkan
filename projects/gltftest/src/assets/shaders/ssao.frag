#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_ARB_shader_viewport_layer_array : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 oOutput;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
};

layout(std140, set = 0, binding = 0) uniform uboViews {
  View views[512]; // 65536 / (64 * 2) = 512
} uView;

#ifdef MULTIVIEW
layout(set = 0, binding = 1) uniform sampler2DArray uTextureDepth;
layout(set = 0, binding = 2) uniform sampler2DArray uTextureNormals;
#else
layout(set = 0, binding = 1) uniform sampler2D uTextureDepth;
layout(set = 0, binding = 2) uniform sampler2D uTextureNormals;
#endif

layout (push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint frameIndex;
} pushConstants;

/* clang-format on */

mat4 inverseProjectionMatrix = inverse(uView.views[int(gl_ViewIndex)].projectionMatrix);

float linearizeDepth(float z) {
  vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(fma(z, 2.0, -1.0)), inverseProjectionMatrix[3].zw);
  return v.x / v.y;
}

vec3 signedOctDecode(vec3 normal) {
  vec2 outNormal;
  outNormal = vec2(normal.xx + vec2(-normal.y, normal.y - 1.0));
  return normalize(vec3(outNormal, fma(normal.z, 2.0, -1.0) * (1.0 - (abs(outNormal.x) + abs(outNormal.y)))));
}

const int countKernelSamples = 16;
const vec3 kernelSamples[16] = vec3[](                               //
    vec3(0.5381, 0.1856, -0.4319), vec3(0.1379, 0.2486, 0.4430),     //
    vec3(0.3371, 0.5679, -0.0057), vec3(-0.6999, -0.0451, -0.0019),  //
    vec3(0.0689, -0.1598, -0.8547), vec3(0.0560, 0.0069, -0.1843),   //
    vec3(-0.0146, 0.1402, 0.0762), vec3(0.0100, -0.1924, -0.0344),   //
    vec3(-0.3577, -0.5301, -0.4358), vec3(-0.3169, 0.1063, 0.0158),  //
    vec3(0.0103, -0.5869, 0.0046), vec3(-0.0897, -0.4940, 0.3287),   //
    vec3(0.7119, -0.0154, -0.0918), vec3(-0.0533, 0.0596, -0.5411),  //
    vec3(0.0352, -0.0631, 0.5460), vec3(-0.4776, 0.2847, -0.0271)    //
);
const float radius = 2e-4;
const float area = 0.0075;
const float fallOff = 1e-6;

vec3 hash33(vec3 p) {
  vec3 p3 = fract(p.xyz * vec3(443.8975, 397.2973, 491.1871));
  p3 += dot(p3, p3.yxz + 19.19);
  return fract(vec3((p3.x + p3.y) * p3.z, (p3.x + p3.z) * p3.y, (p3.y + p3.z) * p3.x));
}

void main() {
  vec2 texelSize = vec2(1.0) / vec2(textureSize(uTextureDepth, 0).xy);
#ifdef MULTIVIEW
  vec3 texCoord = vec3(inTexCoord, float(int(gl_ViewIndex)));
#else
  vec2 texCoord = inTexCoord;
#endif
  float depth = linearizeDepth(textureLod(uTextureDepth, texCoord, 0).x);
#define NORMALS
#ifdef NORMALS
  vec3 normal = signedOctDecode(textureLod(uTextureNormals, texCoord, 0).xyz);
#else
  vec4 position = inverseProjectionMatrix * vec4(fma(vec3(inTexCoord, depth), vec3(2.0), vec3(-1.0)), 1.0);
  position.xyz /= position.w;
  vec3 normal = normalize(cross(dFdx(position.xyz), dFdy(position.xyz)));
#endif
  vec3 randomVector = normalize(hash33(vec3(gl_FragCoord.xy, fract(float(pushConstants.frameIndex) / 65536.0))) - vec3(0.5));
  vec3 tangent = normalize(randomVector - (normal * dot(randomVector, normal)));
  vec3 bitangent = cross(normal, tangent);
  mat3 tbn = mat3(tangent, bitangent, normal);
  float occlusion = 0.0;
  float radius_depth = radius / depth;
  for (int i = 0; i < countKernelSamples; i++) {
#ifdef MULTIVIEW
    vec3 offset = vec3(vec3((tbn * kernelSamples[i]) * radius_depth).xy, 0.0);
#else
    vec2 offset = vec3((tbn * kernelSamples[i]) * radius_depth).xy;
#endif
    float depthDifference = depth - linearizeDepth(textureLod(uTextureDepth, texCoord + offset, 0).x);
    occlusion += step(fallOff, depthDifference) * (1.0 - smoothstep(fallOff, area, depthDifference));
  }
  oOutput = vec2(clamp(1.0 - (1.0 * (occlusion / float(countKernelSamples))), 0.0, 1.0), depth).xyyy;
}
