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

vec4 whiteNoise2(ivec4 p){
  
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
    
  return vec4(uintBitsToFloat(uvec4(uvec4(((v >> 9u) & uvec4(0x007fffffu)) | uvec4(0x3f800000u))))) - vec4(1.0);
   
}      

vec3 whiteNoise(ivec3 p){
  const uint k = 1103515245u;
  uvec3 v = uvec3(p); 
  v = ((v >> 8u) ^ v.yzx) * k;
  v = ((v >> 8u) ^ v.yzx) * k;
  v = ((v >> 8u) ^ v.yzx) * k;
  return fma(vec3(vec3(uintBitsToFloat(uvec3(uvec3(((v >> 9u) & uvec3(0x007fffffu)) | uvec3(0x3f800000u))))) - vec3(1.0)), vec3(2.0), vec3(-1.0));
}
   
vec3 pseudoBlueNoise(ivec3 p) {
  return clamp((vec3(
                whiteNoise(p + ivec3(-1, -1, 0)) + 
                whiteNoise(p + ivec3(0, -1, 0)) + 
                whiteNoise(p + ivec3(1, -1, 0)) +
                whiteNoise(p + ivec3(-1, 0, 0)) +
                (whiteNoise(p) * (-8.0)) +
                 whiteNoise(p + ivec3(1, 0, 0)) +
                whiteNoise(p + ivec3(-1, 1, 0)) + 
                whiteNoise(p + ivec3(0, 1, 0)) + 
                whiteNoise(p + ivec3(1, 1, 0))
               ) * ((0.5 * 2.1) / 9.0)
              ) + vec3(0.5), vec3(0.0), vec3(1.0));
}

vec3 convertLinearRGBToSRGB(vec3 c) {
  return mix((pow(c, vec3(1.0 / 2.4)) * vec3(1.055)) - vec3(5.5e-2), c * vec3(12.92), lessThan(c, vec3(3.1308e-3)));  //
}

vec4 convertLinearRGBToSRGB(vec4 c) {
  return vec4(convertLinearRGBToSRGB(c.xyz), c.w);  //
}

vec3 convertSRGBToLinearRGB(vec3 c) {
  return mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)), c / vec3(12.92), lessThan(c, vec3(4.045e-2)));  //
}

vec4 convertSRGBToLinearRGB(vec4 c) {
  return vec4(convertSRGBToLinearRGB(c.xyz), c.w);  //
}
void main() {
#if 1
  vec3 n = fma(pseudoBlueNoise(ivec3(gl_FragCoord.xy + ivec2(pushConstants.frameCounter), 0)).xyz, vec3(2.0), vec3(-1.0));
  n = sign(n) * (vec3(1.0) - sqrt(vec3(1.0) - abs(n)));
  outFragColor = convertSRGBToLinearRGB(convertLinearRGBToSRGB(subpassLoad(uSubpassInput)) + vec4(n * (1.0 / 255.0), 0.0));
#elif 0
  uvec3 v = uvec3(uvec2(gl_FragCoord.xy), uint(pushConstants.frameCounter));
  const uint k = 1103515245u;
  v = ((v >> 8u) ^ v.yzx) * k;
  v = ((v >> 8u) ^ v.yzx) * k;
  v = ((v >> 8u) ^ v.yzx) * k;
  vec3 n = fma(vec3(vec3(uintBitsToFloat(uvec3(uvec3(((v >> 9u) & uvec3(0x007fffffu)) | uvec3(0x3f800000u))))) - vec3(1.0)), vec3(2.0), vec3(-1.0));
  n = sign(n) * (vec3(1.0) - sqrt(vec3(1.0) - abs(n)));
  outFragColor = convertSRGBToLinearRGB(convertLinearRGBToSRGB(subpassLoad(uSubpassInput)) + vec4(n * (1.0 / 255.0), 0.0));
#else
  outFragColor = convertSRGBToLinearRGB(convertLinearRGBToSRGB(subpassLoad(uSubpassInput)) + vec4(vec3(((fract((vec3(dot(vec2(171.0, 231.0), vec2(gl_FragCoord.xy) + vec2(ivec2(int(pushConstants.frameCounter & 0xff)))))) / vec3(103.0, 71.0, 97.0)) - vec3(0.5)) / vec3(255.0)) * 0.375), 0.0));
#endif
}
