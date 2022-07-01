#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_ARB_shader_viewport_layer_array : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec2 oFragOcclusionDepth;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(std140, set = 0, binding = 0) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

#ifdef MULTIVIEW
layout(set = 0, binding = 1) uniform sampler2DArray uTextureDepth;
#else
layout(set = 0, binding = 1) uniform sampler2D uTextureDepth;
#endif

layout (push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint frameIndex;
} pushConstants;

/* clang-format on */

float viewIndex = float(int(gl_ViewIndex));

mat4 projectionMatrix = uView.views[int(pushConstants.viewBaseIndex) + int(gl_ViewIndex)].projectionMatrix;
mat4 inverseProjectionMatrix = uView.views[int(pushConstants.viewBaseIndex) + int(gl_ViewIndex)].inverseProjectionMatrix;

vec3 fetchPosition(vec2 texCoord) {
#ifdef MULTIVIEW
  vec4 position = inverseProjectionMatrix * vec4(vec3(fma(texCoord, vec2(2.0), vec2(-1.0)), textureLod(uTextureDepth, vec3(texCoord, viewIndex), 0).x), 1.0);
#else
  vec4 position = inverseProjectionMatrix * vec4(vec3(fma(texCoord, vec2(2.0), vec2(-1.0)), textureLod(uTextureDepth, texCoord, 0).x), 1.0);
#endif
  return position.xyz / position.w;
}

float linearizeDepth(float z) {
#if 0
  vec2 v = (inverseProjectionMatrix * vec4(vec3(fma(inTexCoord, vec2(2.0), vec2(-1.0)), z), 1.0)).zw;
#else
  vec2 v = fma(inverseProjectionMatrix[2].zw, vec2(z), inverseProjectionMatrix[3].zw);
#endif
  return v.x / v.y;
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

const float radius = 1.0;
const float bias = 0.1;

vec3 hash33(vec3 p) {
  vec3 p3 = fract(p.xyz * vec3(443.8975, 397.2973, 491.1871));
  p3 += dot(p3, p3.yxz + 19.19);
  return fract(vec3((p3.x + p3.y) * p3.z, (p3.x + p3.z) * p3.y, (p3.y + p3.z) * p3.x));
}

void main() {
#ifdef MULTIVIEW
  vec3 texCoord = vec3(inTexCoord, viewIndex);
#else
  vec2 texCoord = inTexCoord;
#endif
  vec3 position = fetchPosition(texCoord.xy);
  float depth = position.z;
  float occlusion = 0.0;
  if (isinf(depth) || (abs(depth) < 1e-7)) {
    occlusion = 1.0;
  } else {
    vec3 normal;
#if 0
    {
      vec2 texelSize = vec2(1.0) / vec2(textureSize(uTextureDepth, 0).xy); // vec2(dFdx(texCoord.x), dFdy(texCoord.y));
#ifdef MULTIVIEW
      vec3 offsetH = vec3(texelSize.x, 0.0, 0.0);
      vec3 offsetV = vec3(0.0, texelSize.y, 0.0);
#else
      vec2 offsetH = vec2(texelSize.x, 0.0);
      vec2 offsetV = vec2(0.0, texelSize.y);
#endif
      vec3 pl = fetchPosition(texCoord.xy - (offsetH.xy * 1.0));
      vec3 pr = fetchPosition(texCoord.xy + (offsetH.xy * 1.0));
      vec3 pu = fetchPosition(texCoord.xy - (offsetV.xy * 1.0));
      vec3 pd = fetchPosition(texCoord.xy + (offsetV.xy * 1.0));
      vec4 H = vec4(                                                                   //
          pl.z,  //
          pr.z,  //
          linearizeDepth(textureLod(uTextureDepth, texCoord - (offsetH * 2.0), 0).x),  //
          linearizeDepth(textureLod(uTextureDepth, texCoord + (offsetH * 2.0), 0).x)   //
      );
      vec4 V = vec4(                                                                   //
          pu.z,  //
          pd.z,  //
          linearizeDepth(textureLod(uTextureDepth, texCoord - (offsetV * 2.0), 0).x),  //
          linearizeDepth(textureLod(uTextureDepth, texCoord + (offsetV * 2.0), 0).x)   //
      );
      vec4 hve = abs((vec4(H.xy * H.zw, V.xy * V.zw) / fma(vec4(H.zw, V.zw), vec4(2.0), -vec4(H.xy, V.xy))) - vec4(depth));
      normal = normalize(cross((hve.x < hve.y) ? (position - pl) : (pr - position), (hve.z < hve.w) ? (position - pu) : (pd - position)));
    }
#else
    normal = normalize(cross(dFdx(position), dFdy(position))); 
#endif
    vec3 randomVector = normalize(hash33(vec3(gl_FragCoord.xy, /*mod(float(pushConstants.frameIndex), 1024.0)*/ 0.0)) - vec3(0.5));
    vec3 tangent = normalize(randomVector - (normal * dot(randomVector, normal)));
    vec3 bitangent = cross(normal, tangent);
    mat3 tbn = mat3(tangent, bitangent, normal);
    for (int i = 0; i < countKernelSamples; i++) {
      vec4 p = projectionMatrix * vec4(position.xyz + ((tbn * kernelSamples[i]) * radius), 1.0); 
      p.xyz /= p.w;
      p.xy = fma(p.xy, vec2(0.5), vec2(0.5));
#ifdef MULTIVIEW
      float sampleDepth = linearizeDepth(textureLod(uTextureDepth, vec3(p.xy, viewIndex), 0).x);
#else
      float sampleDepth = linearizeDepth(textureLod(uTextureDepth, p.xy, 0).x);
#endif
      occlusion += (sampleDepth >= (depth + bias)) ? smoothstep(0.0, 1.0, radius / abs(depth - sampleDepth)) : 0.0;
    }
    occlusion = clamp(1.0 - (occlusion / float(countKernelSamples)), 0.0, 1.0);
  }
  oFragOcclusionDepth = vec2(occlusion, depth);
}
