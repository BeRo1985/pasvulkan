#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;
layout(location = 1) flat in int inFaceIndex;

layout(location = 0) out vec4 outFragColor;

layout (push_constant) uniform PushConstants {
  int mipMapLevel;
  int maxMipMapLevel;
} pushConstants;

layout (set = 0, binding = 0) uniform samplerCube uTexture;

vec2 Hammersley(const in int index, const in int numSamples){
  uint reversedIndex = uint(index);
  reversedIndex = (reversedIndex << 16u) | (reversedIndex >> 16u);
  reversedIndex = ((reversedIndex & 0x00ff00ffu) << 8u) | ((reversedIndex & 0xff00ff00u) >> 8u);
  reversedIndex = ((reversedIndex & 0x0f0f0f0fu) << 4u) | ((reversedIndex & 0xf0f0f0f0u) >> 4u);
  reversedIndex = ((reversedIndex & 0x33333333u) << 2u) | ((reversedIndex & 0xccccccccu) >> 2u);
  reversedIndex = ((reversedIndex & 0x55555555u) << 1u) | ((reversedIndex & 0xaaaaaaaau) >> 1u);
  return vec2(fract(float(index) / float(numSamples)), float(reversedIndex) * 2.3283064365386963e-10);
}

vec3 ImportanceSampleGGX(const in vec2 e, const in float roughness, const in vec3 normal){
  float m = roughness * roughness;
  float m2 = m * m;
  float phi = 2.0 * 3.1415926535897932384626433832795 * e.x;
  float cosTheta = sqrt((1.0 - e.y) / (1.0 + ((m2 - 1.0) * e.y)));
  float sinTheta = sqrt(1.0 - (cosTheta * cosTheta));
  vec3 h = vec3(sinTheta * cos(phi), sinTheta * sin(phi), cosTheta);
  vec3 tangentZ = normalize(normal);
  vec3 upVector = (abs(tangentZ.z) < 0.999) ? vec3(0.0, 0.0, 1.0) : vec3(1.0, 0.0, 0.0);
  vec3 tangentX = normalize(cross(upVector, tangentZ));
  vec3 tangentY = cross(tangentZ, tangentX);
  return (tangentX * h.x) + (tangentY * h.y) + (tangentZ * h.z);
}

vec3 getCubeMapDirection(in vec2 uv,
                         in int faceIndex){
  vec3 zDir = vec3(ivec3((faceIndex <= 1) ? 1 : 0,
                         (faceIndex & 2) >> 1,
                         (faceIndex & 4) >> 2)) *
             (((faceIndex & 1) == 1) ? -1.0 : 1.0),
       yDir = (faceIndex == 2)
                ? vec3(0.0, 0.0, 1.0)
                : ((faceIndex == 3)
                     ? vec3(0.0, 0.0, -1.0)
                     : vec3(0.0, -1.0, 0.0)),
       xDir = cross(zDir, yDir);
  return normalize((mix(-1.0, 1.0, uv.x) * xDir) +
                   (mix(-1.0, 1.0, uv.y) * yDir) +
                   zDir);
}
void main(){
  vec3 direction = getCubeMapDirection(inTexCoord, inFaceIndex);
  if(pushConstants.mipMapLevel == 0){
    outFragColor = textureLod(uTexture, direction, 0.0);
	}else{
	  float roughness = clamp(exp2((1.0 - float((pushConstants.maxMipMapLevel - 1) - pushConstants.mipMapLevel)) / 1.2), 0.0, 1.0);
	  const int numSamples = 64;
	  vec3 R = direction;
	  vec3 N = R;
	  vec3 V = R;
	  vec4 r = vec4(0.0);
	  float w = 0.0;
	  for(int i = 0; i < numSamples; i++){
	    vec3 H = ImportanceSampleGGX(Hammersley(i, numSamples), roughness, N);
	    vec3 L = -reflect(V, H);
	    float nDotL = clamp(dot(N, L), 0.0, 1.0);
	    if(nDotL > 0.0){
	      vec3 rayDirection = normalize(L);
	      r += textureLod(uTexture, rayDirection, 0.0) * nDotL;
	      w += nDotL;
	    }
	  }
	  outFragColor = r / max(w, 1e-4);
  }
}