#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

const int numSamples = 1024;

vec2 Hammersley(const in int index, const in int numSamples){
  uint reversedIndex = uint(index);
  reversedIndex = (reversedIndex << 16u) | (reversedIndex >> 16u);
  reversedIndex = ((reversedIndex & 0x00ff00ffu) << 8u) | ((reversedIndex & 0xff00ff00u) >> 8u);
  reversedIndex = ((reversedIndex & 0x0f0f0f0fu) << 4u) | ((reversedIndex & 0xf0f0f0f0u) >> 4u);
  reversedIndex = ((reversedIndex & 0x33333333u) << 2u) | ((reversedIndex & 0xccccccccu) >> 2u);
  reversedIndex = ((reversedIndex & 0x55555555u) << 1u) | ((reversedIndex & 0xaaaaaaaau) >> 1u);
  return vec2(fract(float(index) / float(numSamples)), float(reversedIndex) * 2.3283064365386963e-10);
}

vec3 ImportanceSampleGGX(const in vec2 e, const in float roughness){
  float m = roughness * roughness;
  float m2 = m * m;
  float phi = (2.0 * 3.1415926535897932384626433832795) * e.x;
  float cosTheta = sqrt((1.0 - e.y) / (1.0 + ((m2 - 1.0) * e.y)));
  float sinTheta = sqrt(1.0 - (cosTheta * cosTheta));
  return vec3(sinTheta * cos(phi), sinTheta * sin(phi), cosTheta);
}

float specularG(const in float roughness, const in float nDotV, const in float nDotL){
  float a = roughness * roughness;
  float a2 = a * a;
  vec2 GVL = vec2(nDotV, nDotL);
  GVL = GVL + sqrt((GVL * (GVL - (GVL * a2))) + vec2(a2));
  return 1.0 / (GVL.x * GVL.y);
}

void main(){
  float roughness = inTexCoord.x;
  float nDotV = inTexCoord.y;
  vec3 V = vec3(sqrt(1.0 - (nDotV * nDotV)), 0.0, nDotV);
  vec2 r = vec2(0.0);
  for(int i = 0; i < numSamples; i++){
    vec3 H = ImportanceSampleGGX(Hammersley(i, numSamples), roughness);
    vec3 L = -reflect(V, H);
    float nDotL = clamp(L.z, 0.0, 1.0);
    if(nDotL > 0.0){
      float vDotH = clamp(dot(V, H), 0.0, 1.0);
      r += (vec2(1.0, 0.0) + (vec2(-1.0, 1.0) * pow(1.0 - vDotH, 5.0))) * (nDotL * specularG(roughness, nDotV, nDotL) * ((4.0 * vDotH) / clamp(H.z, 0.0, 1.0)));
    }
  }
  outFragColor = vec4(r / float(numSamples), 0.0, 1.0);
}
