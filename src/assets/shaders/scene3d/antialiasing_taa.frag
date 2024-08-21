#version 450 core

#define ColorSpaceRGB 0
#define ColorSpaceYCoCg 1

#define ColorSpace ColorSpaceYCoCg 

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
//return vec4(vec3(c.yy + ((c.x + c.z) * vec2(0.5, -0.5)), c.x - c.z).xzy * 0.5, 1.0);
  return vec4(mat3(0.25, 0.5, -0.25, 0.5, 0, 0.5, 0.25, -0.5, -0.25) * c.xyz, c.w);
}

vec4 YCoCgToRGB(in vec4 c){
//return vec4((c.xxx + vec3(c.yz, -c.y)) - vec2(c.z, 0.0).xyx, c.w);
  return vec4(mat3(1.0, 1.0, 1.0, 1.0, 0.0, -1.0, -1.0, 1.0, -1.0) * c.xyz, c.w);
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
#if 0  
  vec3 p_clip = (aabbMin + aabbMax) * 0.5;
	vec3 e_clip = fma(aabbMax - aabbMin, vec3(0.5), vec3(1e-7));
	vec4 v_clip = q - vec4(p_clip, p.w);
	vec3 a_unit = abs(v_clip.xyz / e_clip);
	float maxUnit = max(a_unit.x, max(a_unit.y, a_unit.z));
	return (maxUnit > 1.0) ? vec4(vec4(p_clip, p.w) + (v_clip / maxUnit)) : q;
#else
  const float FLT_MIN = uintBitsToFloat(0x00800000u); // 1.17549435e-38
//const float FLT_MAX = uintBitsToFloat(0x7f7fffffu); // 3.40282347e+38
  vec4 r = q - p;
  vec3 rmax = aabbMax - p.xyz, rmin = aabbMin - p.xyz;
  if(r.x > (rmax.x + FLT_MIN)){
    r *= rmax.x / r.x;
  }
  if(r.y > (rmax.y + FLT_MIN)){
    r *= rmax.y / r.y;
  }
  if(r.z > (rmax.z + FLT_MIN)){
    r *= rmax.z / r.z;
  }
  if(r.x < (rmin.x - FLT_MIN)){
    r *= rmin.x / r.x;
  }
  if(r.y < (rmin.y - FLT_MIN)){
    r *= rmin.y / r.y;
  }
  if(r.z < (rmin.z - FLT_MIN)){
    r *= rmin.z / r.z;
  }
  return p + r;
#endif
}

vec4 textureCatmullRom(const in sampler2DArray tex, const in vec3 uvw, const in float lod){
  vec2 texSize = textureSize(tex, int(lod)).xy,
       uv = uvw.xy,
       samplePos = uv * texSize,
       p11 = floor(samplePos - vec2(0.5)) + vec2(0.5),
       t = samplePos - p11, 
       tt = t * t, 
       ttt = tt * t,
       w0 = (tt - (ttt * 0.5)) - (0.5 * t),
       w1 = ((ttt * 1.5) - (tt * 2.5)) + vec2(1.0),
       w2 = ((tt * 2.0) - (ttt * 1.5)) + (t * 0.5),
       w3 = (ttt * 0.5) - (tt * 0.5),
       w4 = w1 + w2,
       p00 = (p11 - vec2(1.0)) / texSize,
       p33 = (p11 + vec2(2.0)) / texSize,
       p12 = (p11 + (w2 / w4)) / texSize;
  return (((textureLod(tex, vec3(vec2(p00.x,  p00.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p00.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x,  p00.y), uvw.z), float(lod)) * w3.x)) * w0.y) +
         (((textureLod(tex, vec3(vec2(p00.x,  p12.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p12.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x,  p12.y), uvw.z), float(lod)) * w3.x)) * w4.y) +
         (((textureLod(tex, vec3(vec2(p00.x,  p33.y), uvw.z), float(lod)) * w0.x) +
           (textureLod(tex, vec3(vec2(p12.x, p33.y), uvw.z), float(lod)) * w4.x) +
           (textureLod(tex, vec3(vec2(p33.x,  p33.y), uvw.z), float(lod)) * w3.x)) * w3.y);
}

#define UseFallbackFXAA 1
#if UseFallbackFXAA
vec4 fallbackFXAA(const in vec2 invTexSize){
  const vec2 fragCoordInvScale = invTexSize;
  vec4 p = vec4(inTexCoord, vec2(inTexCoord - (fragCoordInvScale * (0.5 + (1.0 / 4.0)))));
  const float FXAA_SPAN_MAX = 8.0,
              FXAA_REDUCE_MUL = 1.0 / 8.0,
              FXAA_REDUCE_MIN = 1.0 / 128.0;
  vec3 rgbNW = ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0).xyz),
       rgbNE = ApplyToneMapping(textureLodOffset(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0, ivec2(1, 0)).xyz),
       rgbSW = ApplyToneMapping(textureLodOffset(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0, ivec2(0, 1)).xyz),
       rgbSE = ApplyToneMapping(textureLodOffset(uCurrentColorTexture, vec3(p.zw, float(gl_ViewIndex)), 0.0, ivec2(1, 1)).xyz),
       rgbM = ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy, float(gl_ViewIndex)), 0.0).xyz),
       luma = vec3(0.2126, 0.7152, 0.0722);
  float lumaNW = dot(rgbNW, luma),
        lumaNE = dot(rgbNE, luma),
        lumaSW = dot(rgbSW, luma),
        lumaSE = dot(rgbSE, luma),
        lumaM = dot(rgbM, luma),
        lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE))), 
        lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));
  vec2 dir = vec2(-((lumaNW + lumaNE) - (lumaSW + lumaSE)), ((lumaNW + lumaSW) - (lumaNE + lumaSE)));
  float dirReduce = max((lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL), FXAA_REDUCE_MIN), 
  rcpDirMin = 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);
  dir = min(vec2(FXAA_SPAN_MAX, FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX), dir * rcpDirMin)) * fragCoordInvScale;
  vec4 rgbA = (1.0 / 2.0) * (ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((1.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw) + ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((2.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw)),
       rgbB = (rgbA * (1.0 / 2.0)) + ((1.0 / 4.0) * (ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((0.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw) + ApplyToneMapping(textureLod(uCurrentColorTexture, vec3(p.xy + (dir * ((3.0 / 3.0) - 0.5)), float(gl_ViewIndex)), 0.0).xyzw)));
  float lumaB = dot(rgbB.xyz, luma);
  return ApplyInverseToneMapping(((lumaB < lumaMin) || (lumaB > lumaMax)) ? rgbA : rgbB);  
}
#endif

void main() {
    
  vec2 texSize = vec2(textureSize(uCurrentColorTexture, 0).xy);
  vec2 invTexSize = vec2(1.0) / texSize;
  
  vec4 color = vec4(0.0);
  
  vec3 uvw = vec3(inTexCoord, float(gl_ViewIndex));

#if 0
  vec4 current = textureLod(uCurrentColorTexture, uvw - vec3(pushConstants.jitterUV, 0.0), 0.0); // With unjittering
#else
  vec4 current = textureLod(uCurrentColorTexture, uvw, 0.0); // Without unjittering
#endif

  if((abs(1.0 - pushConstants.opaqueCoefficient) < 1e-5) ||
#if UseFallbackFXAA
     // Use fallback FXAA on transparent or similar surfaces when translucentCoefficient is basically almost zero and mixCoefficient 
     // is basically almost one.
     ((current.w < 1e-7) && (pushConstants.mixCoefficient > 0.99999) && (pushConstants.translucentCoefficient < 1e-7))
#endif  
    ){

#if UseFallbackFXAA
    // Use fallback FXAA when temporal raster data doesn’t exist yet (for example, on the first frame) as it is also used at 
    // NVIDIA's Adaptive Temporal Antialiasing (ATAA).
    current = fallbackFXAA(invTexSize);
#endif

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
#if UseFallbackFXAA
      // Use fallback FXAA in areas of off-screen disocclusion (where temporal raster data doesn’t exist) as it is also used at 
      // NVIDIA's Adaptive Temporal Antialiasing (ATAA).
      current = fallbackFXAA(invTexSize);
#endif
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
        
      vec4 historySample = ConvertFromRGB(Tonemap(textureCatmullRom(uHistoryColorTexture, historyUVW, 0.0)));
      
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