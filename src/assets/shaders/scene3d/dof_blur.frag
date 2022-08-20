#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragOutput;

layout(push_constant) uniform PushConstants {
  float maxCoC;
  float fFactor;
  float ngon;
  float downSampleFactor;
  int blurKernelSize;
} pushConstants;

layout(set = 0, binding = 0) uniform sampler2DArray uTextureInput;

const float PI = 3.14159265359;   
const float halfPI = 1.57079632679;

// o = tap sample xy, f = f-factor, n = diaphragm shape ngon, phiShutterMax = max. lens shutter rotation 
vec2 getBokehTapSampleCoord(const in vec2 o, const in float f, const float n, const in float phiShutterMax){
    vec2 ab = (o * 2.0) - vec2(1.0);    
    vec2 phir = ((ab.x * ab.x) > (ab.y * ab.y)) ? vec2((abs(ab.x) > 1e-8) ? ((PI * 0.25) * (ab.y / ab.x)) : 0.0, ab.x) : vec2((abs(ab.y) > 1e-8) ? ((PI * 0.5) - ((PI * 0.25) * (ab.x / ab.y))) : 0.0, ab.y); 
    phir.x += f * phiShutterMax;
    phir.y *= (f > 0.0) ? pow((cos(PI / n) / cos(phir.x - ((2.0 * (PI / n)) * floor(((n * phir.x) + PI) / (2.0 * PI))))), f) : 1.0;
    return sin(vec2(phir.x) + vec2(0.0, halfPI)) * phir.y;
}                   

void main(){
  
  vec2 inputTextureSize = textureSize(uTextureInput, 0).xy; 
 
  vec2 inverseInputTextureSize = vec2(1.0) / inputTextureSize; 

  float aspectRatio = inputTextureSize.y / inputTextureSize.x;

  float margin = inverseInputTextureSize.y * pushConstants.downSampleFactor * 2.0;
  
  vec3 uvw = vec3(inTexCoord.xy, gl_ViewIndex); 

  vec4 centerSample = textureLod(uTextureInput, uvw, 0);

  vec4 farSum = vec4(0.0);
  vec4 nearSum = vec4(0.0);

  int countSquaredRootSamples = pushConstants.blurKernelSize,
      countSamples = countSquaredRootSamples * countSquaredRootSamples;
  for(int sampleIndex = 0; sampleIndex < countSamples; sampleIndex++){            

#if 0
    vec2 offset = sin(vec2(float(sampleIndex) * 2.39996322973) + 
                      vec2(0.0, 1.57079632679)) *
                  (float(sampleIndex) / float(countSamples - 1)) * pushConstants.maxCoC;
#else
    vec2 offset = getBokehTapSampleCoord(vec2(ivec2(sampleIndex / countSquaredRootSamples, 
                                                    sampleIndex % countSquaredRootSamples)) / float(countSquaredRootSamples - 1), 
                                         pushConstants.fFactor, 
                                         pushConstants.ngon,
                                         halfPI) * pushConstants.maxCoC;
#endif       
     
     float offsetDistance = max(1e-7, length(offset));
     
     offset.x *= aspectRatio;

     vec4 sampleTexel = textureLod(uTextureInput, uvw + vec3(offset, 0.0), 0.0);
     
     float farCoC = max(min(centerSample.w, sampleTexel.w), 0.0); 
     
     float farWeight = clamp(((farCoC - offsetDistance) + margin) / margin, 0.0, 1.0);
     float nearWeight = clamp((((-sampleTexel.w) - offsetDistance) + margin) / margin, 0.0, 1.0) * step(inverseInputTextureSize.y * pushConstants.downSampleFactor, -sampleTexel.w);

     farSum += vec4(sampleTexel.xyz, 1.0) * farWeight;
     nearSum += vec4(sampleTexel.xyz, 1.0) * nearWeight;

  }

  farSum.xyz /= ((farSum.w < 1e-7) ? 1.0 : farSum.w);
  nearSum.xyz /= ((nearSum.w < 1e-7) ? 1.0 : nearSum.w);

  //farSum.w = smoothstep(inverseInputTextureSize.y, inverseInputTextureSize.y * 2.0, centerSample.w);

  nearSum.w *= PI / float(countSamples);

  float alpha = clamp(nearSum.w, 0.0, 1.0);

  outFragOutput = vec4(mix(farSum.xyz, nearSum.xyz, alpha), alpha);

}
