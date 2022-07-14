#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput;

// layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

layout(push_constant) uniform PushConstants {
  int frameCounter;  //
} pushConstants;

vec4 whiteNoise(ivec4 p){
  
  uvec4 v = uvec4(p); 

  // Pre-inter-mixing of all components with all components with a single ChaCha20 cipher round primitive iteration
  v.x += v.y; v.w ^= v.x; v.w = (v.w << 16u) | (v.w >> 16u);
  v.z += v.w; v.y ^= v.z; v.y = (v.y << 12u) | (v.y >> 20u); 
  v.x += v.y; v.w ^= v.x; v.w = (v.w << 8u) | (v.w >> 24u);
  v.z += v.w; v.y ^= v.z; v.y = (v.y << 7u) | (v.y >> 25u); 

  // Full avalanche integer (re-)hashing with as far as possible equal bit distribution probability
  // => http://burtleburtle.net/bob/hash/integer.html  
  v -= (v << 6u);
  v ^= (v >> 17u);
  v -= (v << 9u);
  v ^= (v << 4u);
  v -= (v << 3u);
  v ^= (v << 10u);
  v ^= (v >> 15u);

  // Post-inter-mixing of all components with all components with a single ChaCha20 cipher round primitive iteration
  v.x += v.y; v.w ^= v.x; v.w = (v.w << 16u) | (v.w >> 16u);
  v.z += v.w; v.y ^= v.z; v.y = (v.y << 12u) | (v.y >> 20u); 
  v.x += v.y; v.w ^= v.x; v.w = (v.w << 8u) | (v.w >> 24u);
  v.z += v.w; v.y ^= v.z; v.y = (v.y << 7u) | (v.y >> 25u); 
    
  return vec4(intBitsToFloat(ivec4(uvec4(((v >> 9u) & uvec4(0x007fffffu)) | uvec4(0x3f800000u))))) - vec4(1.0);
   
}      
   
vec4 pseudoBlueNoise(ivec4 p) {
  return clamp((vec4(
                whiteNoise(p + ivec3(-1, -1, 0).xyzz) + 
                whiteNoise(p + ivec3(0, -1, 0).xyzz) + 
                whiteNoise(p + ivec3(1, -1, 0).xyzz) +
                whiteNoise(p + ivec3(-1, 0, 0).xyzz) +
                (whiteNoise(p) * (-8.0)) +
                 whiteNoise(p + ivec3(1, 0, 0).xyzz) +
                whiteNoise(p + ivec3(-1, 1, 0).xyzz) + 
                whiteNoise(p + ivec3(0, 1, 0).xyzz) + 
                whiteNoise(p + ivec3(1, 1, 0).xyzz)
               ) * ((0.5 * 2.1) / 9.0)
              ) + vec4(0.5), vec4(0.0), vec4(1.0));
}

void main() {
#if 0
  outFragColor = subpassLoad(uSubpassInput) + vec4((pseudoBlueNoise(ivec4(gl_FragCoord.xy, pushConstants.frameCounter, 0)).xyz - vec3(0.5)) * (0.375 / 255.0), 0.0);
#elif 1
  uvec3 x = uvec3(uvec2(gl_FragCoord.xy), uint(pushConstants.frameCounter));
  const uint k = 1103515245u;
  x = ((x >> 8u) ^ x.yzx) * k;
  x = ((x >> 8u) ^ x.yzx) * k;
  x = ((x >> 8u) ^ x.yzx) * k;
  outFragColor = subpassLoad(uSubpassInput) + vec4(((vec3(x) * (1.0 / float(0xffffffffu))) - vec3(0.5)) * (0.375 / 255.0), 0.0);
#else
  outFragColor = subpassLoad(uSubpassInput) + vec4(vec3(((fract((vec3(dot(vec2(171.0, 231.0), vec2(gl_FragCoord.xy) + vec2(ivec2(int(pushConstants.frameCounter & 0xff)))))) / vec3(103.0, 71.0, 97.0)) - vec3(0.5)) / vec3(255.0)) * 0.375), 0.0);
#endif
}
