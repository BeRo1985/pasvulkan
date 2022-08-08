#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

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
  float varianceClipGamma;
} pushConstants;

const mat3 RGBToYCoCgMatrix = mat3(0.25, 0.5, -0.25, 0.5, 0.0, 0.5, 0.25, -0.5, -0.25);

const mat3 YCoCgToRGBMatrix = mat3(1.0, 1.0, 1.0, 1.0, 0.0, -1.0, -1.0, 1.0, -1.0);

float Luminance(vec4 color){
    return dot(color.xyz, vec3(0.2125, 0.7154, 0.0721));
}

vec4 Tonemap(vec4 color){
  return vec4(color.xyz / (Luminance(color) + 1.0), color.w);
}

vec4 Untonemap(vec4 color){
  return vec4(color.xyz / max(1.0 - Luminance(color), 1e-4), color.w);
}

vec4 RGBToYCoCg(in vec4 c){
  return vec4(RGBToYCoCgMatrix * c.xyz, c.w);
}

vec4 YCoCgToRGB(in vec4 c){
  return vec4(YCoCgToRGBMatrix * c.xyz, c.w);
}

vec3 ClipAABB(vec3 point, vec3 aabbMin, vec3 aabbMax){	
  vec3 center = (aabbMin + aabbMax) * 0.5;
	vec3 extents = fma(aabbMax - aabbMin, vec3(0.5), vec3(1e-7));
	vec3 offset = point - center;
	vec3 a = abs(offset.xyz / extents);
	float maxUnit = max(a.x, max(a.y, a.z));
	return (maxUnit > 1.0) ? (center + (offset / maxUnit)) : point;
}

void main() {
    
  vec2 texSize = vec2(textureSize(uCurrentColorTexture, 0).xy);
  vec2 invTexSize = vec2(1.0) / texSize;
  
  vec4 color = vec4(0.0);
  
  vec3 uvw = vec3(inTexCoord, float(gl_ViewIndex));

  vec3 historyUVW = uvw + vec3(textureLod(uVelocityTexture, uvw, 0.0).xy, 0.0);
   
  if((abs(1.0 - pushConstants.opaqueCoefficient) < 1e-5) ||
      (any(lessThan(historyUVW.xy, vec2(0.0))) ||
       any(greaterThan(historyUVW.xy, vec2(1.0))))){

    color = textureLod(uCurrentColorTexture, uvw, 0.0);

  }else{
  
    vec4 currentSamples[9];    
    currentSamples[0] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1, -1)))); // a 0
    currentSamples[1] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0, -1)))); // b 1
    currentSamples[2] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1, -1)))); // c 2
    currentSamples[3] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  0)))); // d 3
    currentSamples[4] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  0)))); // e 4
    currentSamples[5] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  0)))); // f 5
    currentSamples[6] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  1)))); // g 6
    currentSamples[7] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  1)))); // h 7
    currentSamples[8] = RGBToYCoCg(Tonemap(textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  1)))); // i 8
        
    // Soft minimum and maximum ("Hybrid Reconstruction Antialiasing")
    //        1         0 1 2
    // (min 3 4 5 + min 3 4 5) * 0.5
    //        7         6 7 8        
    vec3 minimumColor = min(min(min(min(currentSamples[1].xyz, currentSamples[3].xyz), currentSamples[4].xyz), currentSamples[5].xyz), currentSamples[7].xyz),
         maximumColor = max(max(max(max(currentSamples[1].xyz, currentSamples[3].xyz), currentSamples[4].xyz), currentSamples[5].xyz), currentSamples[7].xyz);
    minimumColor = (minimumColor + min(min(min(min(minimumColor, currentSamples[0].xyz), currentSamples[2].xyz), currentSamples[6].xyz), currentSamples[8].xyz)) * 0.5;
    maximumColor = (maximumColor + max(max(max(max(maximumColor, currentSamples[0].xyz), currentSamples[2].xyz), currentSamples[6].xyz), currentSamples[8].xyz)) * 0.5;
    
    {
      // Variance clipping ("An Excursion in Temporal Supersampling")
      vec3 m0 = currentSamples[0].xyz,
           m1 = currentSamples[0].xyz * currentSamples[0].xyz;   
      for(int i = 1; i < 9; ++i) {
        vec4 currentSample = currentSamples[i]; 
        m0 += currentSample.xyz;
        m1 += currentSample.xyz * currentSample.xyz;
      }
      m0 *= 1.0 / 9.0;
      m1 *= 1.0 / 9.0;
      vec3 sigma = sqrt(m1 - (m0 * m0)) * pushConstants.varianceClipGamma;
      minimumColor = max(minimumColor, m0 - sigma);
      maximumColor = min(maximumColor, m0 + sigma);
    }              
       
    vec4 historySample = RGBToYCoCg(Tonemap(texture(uHistoryColorTexture, historyUVW, 0.0)));
    
    historySample.xyz = ClipAABB(historySample.xyz, minimumColor, maximumColor);

    vec4 blendedSample = mix(historySample, 
                             currentSamples[4], 
                             vec4(mix(pushConstants.translucentCoefficient, pushConstants.opaqueCoefficient, min(historySample.w, currentSamples[4].w))));

    color = Untonemap(YCoCgToRGB(blendedSample));    

  }

  outFragColor = color;

}
