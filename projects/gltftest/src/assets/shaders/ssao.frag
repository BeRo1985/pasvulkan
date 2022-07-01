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

layout(set = 0, binding = 2) uniform sampler2DArray uTextureNormals;

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

vec3 signedOctDecode(vec3 normal) {
  vec2 outNormal;
  outNormal = vec2(normal.xx + vec2(-normal.y, normal.y - 1.0));
  return normalize(vec3(outNormal, fma(normal.z, 2.0, -1.0) * (1.0 - (abs(outNormal.x) + abs(outNormal.y)))));
}

#define NUM_SAMPLES 16
#if NUM_SAMPLES == 16
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
#elif NUM_SAMPLES == 32
const int countKernelSamples = 32;
const vec3 kernelSamples[32] = vec3[](                                    //
    vec3(0.04977, -0.04471, 0.04996), vec3(-0.04065, -0.01937, 0.03193),  //
    vec3(0.05599, 0.05979, 0.05766), vec3(-0.00204, -0.0544, 0.06674),    //
    vec3(0.05004, -0.04665, 0.02538), vec3(-0.03188, 0.02046, 0.02251),   //
    vec3(0.05737, -0.02254, 0.07554), vec3(-0.02503, -0.02483, 0.02495),  //
    vec3(-0.01753, 0.01439, 0.00535), vec3(-0.04406, -0.09028, 0.08368),  //
    vec3(-0.01041, -0.03287, 0.01927), vec3(-0.00738, -0.06583, 0.0674),  //
    vec3(0.07683, 0.12697, 0.107), vec3(-0.10479, 0.06544, 0.10174),      //
    vec3(-0.07455, 0.03445, 0.22414), vec3(-0.10851, 0.14234, 0.16644),   //
    vec3(0.13457, -0.02251, 0.13051), vec3(-0.18767, -0.20883, 0.05777),  //
    vec3(-0.00256, -0.002, 0.00407), vec3(-0.22577, 0.31606, 0.08916),    //
    vec3(0.20722, -0.27084, 0.11013), vec3(-0.13086, 0.11929, 0.28022),   //
    vec3(0.05294, -0.22787, 0.14848), vec3(0.14184, 0.04716, 0.13485),    //
    vec3(-0.02358, -0.08097, 0.21913), vec3(0.15865, 0.23046, 0.04372),   //
    vec3(0.08301, -0.30966, 0.06741), vec3(0.38129, 0.33204, 0.52949),    //
    vec3(0.42449, 0.00565, 0.11758), vec3(0.32902, 0.0309, 0.1785),       //
    vec3(0.86736, -0.00273, 0.10014), vec3(0.41729, -0.15485, 0.46251),   //
);
#elif NUM_SAMPLES == 64
const int countKernelSamples = 64;
const vec3 kernelSamples[64] = vec3[](                                     //
    vec3(0.04977, -0.04471, 0.04996), vec3(0.01457, 0.01653, 0.00224),     //
    vec3(-0.04065, -0.01937, 0.03193), vec3(0.01378, -0.09158, 0.04092),   //
    vec3(0.05599, 0.05979, 0.05766), vec3(0.09227, 0.04428, 0.01545),      //
    vec3(-0.00204, -0.0544, 0.06674), vec3(-0.00033, -0.00019, 0.00037),   //
    vec3(0.05004, -0.04665, 0.02538), vec3(0.03813, 0.0314, 0.03287),      //
    vec3(-0.03188, 0.02046, 0.02251), vec3(0.0557, -0.03697, 0.05449),     //
    vec3(0.05737, -0.02254, 0.07554), vec3(-0.01609, -0.00377, 0.05547),   //
    vec3(-0.02503, -0.02483, 0.02495), vec3(-0.03369, 0.02139, 0.0254),    //
    vec3(-0.01753, 0.01439, 0.00535), vec3(0.07336, 0.11205, 0.01101),     //
    vec3(-0.04406, -0.09028, 0.08368), vec3(-0.08328, -0.00168, 0.08499),  //
    vec3(-0.01041, -0.03287, 0.01927), vec3(0.00321, -0.00488, 0.00416),   //
    vec3(-0.00738, -0.06583, 0.0674), vec3(0.09414, -0.008, 0.14335),      //
    vec3(0.07683, 0.12697, 0.107), vec3(0.00039, 0.00045, 0.0003),         //
    vec3(-0.10479, 0.06544, 0.10174), vec3(-0.00445, -0.11964, 0.1619),    //
    vec3(-0.07455, 0.03445, 0.22414), vec3(-0.00276, 0.00308, 0.00292),    //
    vec3(-0.10851, 0.14234, 0.16644), vec3(0.04688, 0.10364, 0.05958),     //
    vec3(0.13457, -0.02251, 0.13051), vec3(-0.16449, -0.15564, 0.12454),   //
    vec3(-0.18767, -0.20883, 0.05777), vec3(-0.04372, 0.08693, 0.0748),    //
    vec3(-0.00256, -0.002, 0.00407), vec3(-0.0967, -0.18226, 0.29949),     //
    vec3(-0.22577, 0.31606, 0.08916), vec3(-0.02751, 0.28719, 0.31718),    //
    vec3(0.20722, -0.27084, 0.11013), vec3(0.0549, 0.10434, 0.32311),      //
    vec3(-0.13086, 0.11929, 0.28022), vec3(0.15404, -0.06537, 0.22984),    //
    vec3(0.05294, -0.22787, 0.14848), vec3(-0.18731, -0.04022, 0.01593),   //
    vec3(0.14184, 0.04716, 0.13485), vec3(-0.04427, 0.05562, 0.05586),     //
    vec3(-0.02358, -0.08097, 0.21913), vec3(-0.14215, 0.19807, 0.00519),   //
    vec3(0.15865, 0.23046, 0.04372), vec3(0.03004, 0.38183, 0.16383),      //
    vec3(0.08301, -0.30966, 0.06741), vec3(0.22695, -0.23535, 0.19367),    //
    vec3(0.38129, 0.33204, 0.52949), vec3(-0.55627, 0.29472, 0.3011),      //
    vec3(0.42449, 0.00565, 0.11758), vec3(0.3665, 0.00359, 0.0857),        //
    vec3(0.32902, 0.0309, 0.1785), vec3(-0.08294, 0.51285, 0.05656),       //
    vec3(0.86736, -0.00273, 0.10014), vec3(0.45574, -0.77201, 0.00384),    //
    vec3(0.41729, -0.15485, 0.46251), vec3(-0.44272, -0.67928, 0.1865)     //
);

#endif

const float radius = 0.5;
const float bias = 0.025;
const float strength = 0.25;

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
#if 1
    { normal = signedOctDecode(textureLod(uTextureNormals, vec3(inTexCoord, viewIndex), 0).xyz); }
#elif 0
    {
      vec2 texelSize = vec2(1.0) / vec2(textureSize(uTextureDepth, 0).xy);  // vec2(dFdx(texCoord.x), dFdy(texCoord.y));
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
          pl.z,                                                                        //
          pr.z,                                                                        //
          linearizeDepth(textureLod(uTextureDepth, texCoord - (offsetH * 2.0), 0).x),  //
          linearizeDepth(textureLod(uTextureDepth, texCoord + (offsetH * 2.0), 0).x)   //
      );
      vec4 V = vec4(                                                                   //
          pu.z,                                                                        //
          pd.z,                                                                        //
          linearizeDepth(textureLod(uTextureDepth, texCoord - (offsetV * 2.0), 0).x),  //
          linearizeDepth(textureLod(uTextureDepth, texCoord + (offsetV * 2.0), 0).x)   //
      );
      vec4 hve = abs((vec4(H.xy * H.zw, V.xy * V.zw) / fma(vec4(H.zw, V.zw), vec4(2.0), -vec4(H.xy, V.xy))) - vec4(depth));
      normal = normalize(cross((hve.x < hve.y) ? (position - pl) : (pr - position), (hve.z < hve.w) ? (position - pu) : (pd - position)));
    }
#else
    normal = normalize(cross(dFdx(position), dFdy(position)));
#endif
    vec3 randomVector = normalize(hash33(vec3(gl_FragCoord.xy, float(uint(pushConstants.frameIndex & 0xfffu)))) - vec3(0.5));
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
    occlusion = clamp(1.0 - (strength * (occlusion / float(countKernelSamples))), 0.0, 1.0);
  }
  oFragOcclusionDepth = vec2(occlusion, depth);
}
