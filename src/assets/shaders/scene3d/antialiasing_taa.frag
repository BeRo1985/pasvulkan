#version 450 core

#define ColorSpaceRGB 0
#define ColorSpaceYCoCg 1

#define ColorSpace ColorSpaceRGB 

#define UseSimple 0

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_control_flow_attributes : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(set = 0, binding = 0) uniform sampler2DArray uCurrentColorTexture;
layout(set = 0, binding = 1) uniform sampler2DArray uCurrentDepthTexture;
layout(set = 0, binding = 2) uniform sampler2DArray uHistoryColorTexture;
layout(set = 0, binding = 3) uniform sampler2DArray uHistoryDepthTexture;
layout(set = 0, binding = 4) uniform sampler2DArray uVelocityTexture;

layout(push_constant, std140, row_major) uniform PushConstants {
  
  float translucentCoefficient;
  float opaqueCoefficient;
  float mixCoefficient;
  float varianceClipGamma;
  
  float feedbackMin; 
  float feedbackMax; 
  float ZMul;
  float ZAdd;
  
  vec2 jitterUV;

} pushConstants;

float Luminance(vec4 color){
    return dot(color.xyz, vec3(0.2125, 0.7154, 0.0721));
}

#include "bidirectional_tonemapping.glsl"

vec4 Tonemap(vec4 color){
  return ApplyToneMapping(color);
  //return vec4(color.xyz / (Luminance(color) + 1.0), color.w);
}

vec4 Untonemap(vec4 color){
  return ApplyInverseToneMapping(color); 
  //return vec4(color.xyz / max(1.0 - Luminance(color), 1e-4), color.w);
}

#if ColorSpace == ColorSpaceYCoCg

vec4 RGBToYCoCg(in vec4 c){
  return vec4(vec3(c.yy + ((c.x + c.z) * vec2(0.5, -0.5)), c.x - c.z).xzy * 0.5, 1.0);
//return vec4(mat3(0.25, 0.5, -0.25, 0.5, 0, 0.5, 0.25, -0.5, -0.25) * c.xyz, c.w);
}

vec4 YCoCgToRGB(in vec4 c){
  return vec4((c.xxx + vec3(c.yz, -c.y)) - vec2(c.z, 0.0).xyx, c.w);
//return vec4(mat3(1.0, 1.0, 1.0, 1.0, 0.0, -1.0, -1.0, 1.0, -1.0) * c.xyz, c.w);
}

#define ConvertFromRGB RGBToYCoCg
#define ConvertToRGB YCoCgToRGB

#else

#define ConvertFromRGB
#define ConvertToRGB

#endif

#if UseSimple

void main() {

  vec2 texSize = vec2(textureSize(uCurrentColorTexture, 0).xy);
  vec2 invTexSize = vec2(1.0) / texSize;

  vec4 color = vec4(0.0);

  vec3 uvw = vec3(inTexCoord, float(gl_ViewIndex));

  if(abs(1.0 - pushConstants.opaqueCoefficient) < 1e-5){

    color = textureLod(uCurrentColorTexture, uvw, 0.0);

  }else{

    vec4 currentSamples[9];
    currentSamples[0] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1, -1))));
    currentSamples[1] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0, -1))));
    currentSamples[2] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1, -1))));
    currentSamples[3] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  0))));
    currentSamples[4] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  0))));
    currentSamples[5] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  0))));
    currentSamples[6] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  1))));
    currentSamples[7] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  1))));
    currentSamples[8] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  1))));

    vec4 minimumColor = currentSamples[0],
         maximumColor = currentSamples[0];   
    for(int i = 1; i < 9; i++){
      minimumColor = min(minimumColor, currentSamples[i]);
      maximumColor = max(maximumColor, currentSamples[i]);
    }

    vec3 historyUVW = uvw - vec3(textureLod(uVelocityTexture, uvw, 0.0).xy, 0);

    vec4 historySample = clamp(ConvertFromRGB(Tonemap(texture(uHistoryColorTexture, historyUVW, 0.0))), minimumColor, maximumColor);

    color = mix(historySample, 
                currentSamples[4], 
                vec4(mix(0.9,
                         (any(lessThan(historyUVW.xy, vec2(0.0))) || 
                          any(greaterThan(historyUVW.xy, vec2(1.0)))) 
                          ? 1.0 
                          : 1.0 - pushConstants.opaqueCoefficient,
                          currentSamples[4].w
                        )
                    )
               );    

  }

  outFragColor = Untonemap(ConvertToRGB(color));

}

#else

vec4 ClipAABB(vec4 q, vec4 p, vec3 aabbMin, vec3 aabbMax){	
  vec3 p_clip = (aabbMin + aabbMax) * 0.5;
	vec3 e_clip = fma(aabbMax - aabbMin, vec3(0.5), vec3(1e-7));
	vec4 v_clip = q - vec4(p_clip, p.w);
	vec3 a_unit = abs(v_clip.xyz / e_clip);
	float maxUnit = max(a_unit.x, max(a_unit.y, a_unit.z));
	return (maxUnit > 1.0) ? vec4(vec4(p_clip, p.w) + (v_clip / maxUnit)) : q;
}

void main() {
    
  vec2 texSize = vec2(textureSize(uCurrentColorTexture, 0).xy);
  vec2 invTexSize = vec2(1.0) / texSize;
  
  vec4 color = vec4(0.0);
  
  vec3 uvw = vec3(inTexCoord, float(gl_ViewIndex));

  vec4 current = textureLod(uCurrentColorTexture, uvw /*- vec3(pushConstants.jitterUV, 0.0)*/, 0.0);

  if(abs(1.0 - pushConstants.opaqueCoefficient) < 1e-5){

    // First frame, so do nothing then.

    color = current;

  }else{

    vec2 depthTransform = vec2(pushConstants.ZMul, pushConstants.ZAdd);

    vec4 velocityUVWZ;
    {
      vec3 depthSamples[9];
      depthSamples[0] = vec3(-1.0, -1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2(-1.0, -1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[1] = vec3( 0.0, -1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 0.0, -1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[2] = vec3( 1.0, -1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 1.0, -1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[3] = vec3(-1.0,  0.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2(-1.0,  0.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[4] = vec3( 0.0,  0.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 0.0,  0.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[5] = vec3( 1.0,  0.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 1.0,  0.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[6] = vec3(-1.0,  1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2(-1.0,  1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[7] = vec3( 0.0,  1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 0.0,  1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      depthSamples[8] = vec3( 1.0,  1.0, fma(textureLod(uCurrentDepthTexture, uvw + vec3(vec2(vec2( 1.0,  1.0) * invTexSize), 0), 0.0).x, depthTransform.x, depthTransform.y));
      vec3 bestDepth = depthSamples[0];
      if(bestDepth.z < depthSamples[1].z){ bestDepth = depthSamples[1]; }
      if(bestDepth.z < depthSamples[2].z){ bestDepth = depthSamples[2]; }
      if(bestDepth.z < depthSamples[3].z){ bestDepth = depthSamples[3]; }
      if(bestDepth.z < depthSamples[4].z){ bestDepth = depthSamples[4]; }
      if(bestDepth.z < depthSamples[5].z){ bestDepth = depthSamples[5]; }
      if(bestDepth.z < depthSamples[6].z){ bestDepth = depthSamples[6]; }
      if(bestDepth.z < depthSamples[7].z){ bestDepth = depthSamples[7]; }
      if(bestDepth.z < depthSamples[8].z){ bestDepth = depthSamples[8]; }
      velocityUVWZ = vec4(fma(bestDepth.xy, invTexSize, uvw.xy), uvw.z, bestDepth.z);
    }

    vec3 historyUVW = uvw - vec3(textureLod(uVelocityTexture, velocityUVWZ.xyz, 0.0).xy, 0.0);

    if((velocityUVWZ.w < 1e-7) || any(lessThan(historyUVW.xy, vec2(0.0))) || any(greaterThan(historyUVW.xy, vec2(1.0)))){
      color = current;
    }else{

     
      vec4 currentSamples[9];    
      currentSamples[0] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1, -1)))); // a 0
      currentSamples[1] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0, -1)))); // b 1
      currentSamples[2] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1, -1)))); // c 2
      currentSamples[3] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  0)))); // d 3
      currentSamples[4] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  0)))); // e 4
      currentSamples[5] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  0)))); // f 5
      currentSamples[6] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  1)))); // g 6
      currentSamples[7] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  1)))); // h 7
      currentSamples[8] = ConvertFromRGB(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  1)))); // i 8

      current = ConvertFromRGB(Tonemap(current));

#if 1
      // Soft minimum and maximum ("Hybrid Reconstruction Antialiasing")
      //        1         0 1 2
      // (min 3 4 5 + min 3 4 5) * 0.5
      //        7         6 7 8        
      vec4 minimumColor = min(min(min(min(currentSamples[1], currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[7]),
           maximumColor = max(max(max(max(currentSamples[1], currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[7]);
      minimumColor = (minimumColor + min(min(min(min(minimumColor, currentSamples[0]), currentSamples[2]), currentSamples[6]), currentSamples[8])) * 0.5;
      maximumColor = (maximumColor + max(max(max(max(maximumColor, currentSamples[0]), currentSamples[2]), currentSamples[6]), currentSamples[8])) * 0.5;
#else
      // Simple minimum and maximum
      vec4 minimumColor = min(min(min(min(min(min(min(min(currentSamples[0], currentSamples[1]), currentSamples[2]), currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[6]), currentSamples[7]), currentSamples[8]),
           maximumColor = max(max(max(max(max(max(max(max(currentSamples[0], currentSamples[1]), currentSamples[2]), currentSamples[3]), currentSamples[4]), currentSamples[5]), currentSamples[6]), currentSamples[7]), currentSamples[8]);
#endif

      vec4 averageColor = (currentSamples[0] + currentSamples[1] + currentSamples[2] + currentSamples[3] + currentSamples[4] + currentSamples[5] + currentSamples[6] + currentSamples[7] + currentSamples[8]) * (1.0 / 9.0);
      
      {
        // Variance clipping ("An Excursion in Temporal Supersampling")
        vec4 m0 = currentSamples[0],
             m1 = currentSamples[0] * currentSamples[0];   
        for(int i = 1; i < 9; i++) {
          vec4 currentSample = currentSamples[i]; 
          m0 += currentSample;
          m1 += currentSample * currentSample;
        }
        m0 *= 1.0 / 9.0;
        m1 *= 1.0 / 9.0;
        vec4 sigma = sqrt(m1 - (m0 * m0)) * pushConstants.varianceClipGamma;
        minimumColor = max(minimumColor, m0 - sigma);
        maximumColor = min(maximumColor, m0 + sigma);
      }            

#if ColorSpace == ColorSpaceYCoCg 
    /*{ // TODO: Fix this for very bright colors (=> butterfly artifacts later at bloom) 
        vec2 chromaExtent = vec2(maximumColor.x - minimumColor.x) * 0.125;
        vec2 chromaCenter = current.yz;
        minimumColor.yz = chromaCenter - chromaExtent;
        maximumColor.yz = chromaCenter + chromaExtent;
        averageColor.yz = chromaCenter;
      }*/  
#endif      
        
      vec4 historySample = ConvertFromRGB(Tonemap(texture(uHistoryColorTexture, historyUVW, 0.0)));
      
      historySample = ClipAABB(historySample, clamp(averageColor, minimumColor, maximumColor), minimumColor.xyz, maximumColor.xyz);

#if ColorSpace == ColorSpaceYCoCg
      float currentLuminance = current.x;
      float historyLuminance = historySample.x;    
#else
      float currentLuminance = Luminance(current);
      float historyLuminance = Luminance(historySample);
#endif      
      float unbiasedWeight = 1.0 - (abs(currentLuminance - historyLuminance) / max(currentLuminance, max(historyLuminance, 0.2)));
      vec4 outSample = mix(current, historySample, mix(pushConstants.feedbackMin, pushConstants.feedbackMax, clamp(unbiasedWeight * unbiasedWeight, 0.0, 1.0)));

      color = mix(outSample, 
                  mix(historySample, current, mix(pushConstants.translucentCoefficient, pushConstants.opaqueCoefficient, clamp(currentSamples[4].w, 0.0, 1.0))), 
                  clamp(pushConstants.mixCoefficient, 0.0, 1.0)); 

      color = clamp(Untonemap(ConvertToRGB(color)), vec4(0.0), vec4(65504.0));    

    }

  }

  outFragColor = color;

}
#endif