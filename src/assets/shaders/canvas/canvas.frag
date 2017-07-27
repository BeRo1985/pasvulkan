#version 450 core

layout(location = 0) in vec2 inPosition; // 2D position
layout(location = 1) in vec4 inColor;    // RGBA Color 
layout(location = 2) in vec3 inTexCoord; // 2D texture coordinate with array texture layer index inside the z component
layout(location = 3) flat in ivec4 inState; // x = Rendering mode, y = object type, z = not used yet, w = not used yet
layout(location = 4) in vec4 inClipRect; // xy = Left Top, zw = Right Bottom
layout(location = 5) in vec4 inMetaInfo; // Various stuff
layout(location = 6) flat in vec2 inBlendFactors; // x = Alpha channel factor, y = multiplication mode factor 

#if defined(ATLAS_TEXTURE)
layout(binding = 0) uniform sampler2DArray uTexture;
#elif defined(TEXTURE)
layout(binding = 0) uniform sampler2D uTexture;
#endif

layout(location = 0) out vec4 outFragColor;

void main(void){
  vec4 color;
#ifdef NO_TEXTURE
  color = inColor;
#else 
  switch(inState.x){ 
    case 1:{
      const float HALF_BY_SQRT_TWO = 0.5 / sqrt(2.0), ONE_BY_THREE = 1.0 / 3.0;     
#if defined(ATLAS_TEXTURE)
      float center = textureLod(uTexture, inTexCoord, 0.0).w;
#else
      float center = textureLod(uTexture, inTexCoord.xy, 0.0).w;
#endif
      vec2 width = vec2(0.5) + (vec2(clamp(abs(dFdx(center)) + abs(dFdy(center)), 0.0, 1.0 / 1.0)) * vec2(-1.0, 1.0));
      vec4 buv = inTexCoord.xyxy + (vec2((dFdx(inTexCoord.xy) + dFdy(inTexCoord.xy)) * HALF_BY_SQRT_TWO).xyxy * vec2(-1.0, 1.0).xxyy);
#if defined(ATLAS_TEXTURE)
    #define ADJUST_TEXCOORD(uv) vec3(uv, inTexCoord.z)
#else
    #define ADJUST_TEXCOORD(uv) uv
#endif
      color = vec4(vec3(1.0), clamp((smoothstep(width.x, width.y, center) + 
                                                dot(smoothstep(width.xxxx, 
                                                               width.yyyy,
                                                               vec4(textureLod(uTexture, ADJUST_TEXCOORD(buv.xy), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.xw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zy), 0.0).w)), vec4(0.5))) * ONE_BY_THREE, 0.0, 1.0));      
      break;
    }
    default:{
#if defined(ATLAS_TEXTURE)
      color = texture(uTexture, inTexCoord);
#else
      color = texture(uTexture, inTexCoord.xy);
#endif
      break;
    }
  }
  color *= inColor;
#endif
  if(inState.y != 0){
    float threshold = length(abs(dFdx(inPosition.xy)) + abs(dFdy(inPosition.xy)));
    switch(inState.y){
      case 0x01:{
        // Distance to line edge
        color.a *= min(smoothstep(0.0, -threshold, -(inMetaInfo.z - abs(inMetaInfo.x))),  // To the line edges left and right
                       smoothstep(0.0, -threshold, -(inMetaInfo.w - abs(inMetaInfo.y)))); // To the line ends
        break;      
      }
      case 0x02:{
        // Distance to line round cap circle       
        color.a *= smoothstep(0.0, -threshold, length(inPosition.xy - inMetaInfo.xy) - inMetaInfo.z);
        break;      
      }
      case 0x03:{
        // Distance to triangle edge 
        color.a *= smoothstep(0.0, -threshold, dot(inPosition.xy - inMetaInfo.xy, normalize(inMetaInfo.xy - inMetaInfo.zw)));
        break;      
      }
    }
  }
  outFragColor = (vec4(color.rgb, inBlendFactors.x) * mix(clamp(floor(color.a + 0.5), 0.0, 1.0), color.a, inBlendFactors.y)) * 
                 (step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w));
}