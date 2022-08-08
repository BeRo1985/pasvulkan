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
  float deltaTime;
  float omega;
} pushConstants;

void main() {
    
  vec2 texSize = vec2(textureSize(uCurrentColorTexture, 0).xy);
  vec2 invTexSize = vec2(1.0) / texSize;
  
  vec4 color = vec4(0.0);
  
  vec3 uvw = vec3(inTexCoord, float(gl_ViewIndex));

  float deltaTime = pushConstants.deltaTime;

  if(deltaTime < 1e-6){

    color = textureLod(uCurrentColorTexture, uvw, 0.0);

  }else{
  
    vec4 currentSamples[9];
    currentSamples[0] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1, -1));
    currentSamples[1] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0, -1));
    currentSamples[2] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1, -1));
    currentSamples[3] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  0));
    currentSamples[4] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  0));
    currentSamples[5] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  0));
    currentSamples[6] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2(-1,  1));
    currentSamples[7] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 0,  1));
    currentSamples[8] = textureLodOffset(uCurrentColorTexture, uvw, 0, ivec2( 1,  1));
        
    vec4 minimumColor = currentSamples[0],
         maximumColor = currentSamples[0];   
    for(int i = 1; i < 9; ++i) {
      minimumColor = min(minimumColor, currentSamples[i]);
      maximumColor = max(maximumColor, currentSamples[i]);
    }
           
    vec3 historyUVW = uvw + vec3(textureLod(uVelocityTexture, uvw, 0.0).xy, 0.0);
        
    vec4 historySample = clamp(texture(uHistoryColorTexture, historyUVW, 0.0), minimumColor, maximumColor);

    color = mix(historySample, 
                currentSamples[4], 
                vec4(mix(0.25,
                         (any(lessThan(historyUVW.xy, vec2(0.0))) || 
                          any(greaterThan(historyUVW.xy, vec2(1.0)))) 
                          ? 1.0 
                          : (1.0 - exp((-pushConstants.omega) * deltaTime)),
                          currentSamples[4].w
                        )
                    )
               );    

  }

  outFragColor = color;

}
