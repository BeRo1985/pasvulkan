#version 450 core

layout(location = 0) in vec2 inPosition; // 2D position
layout(location = 1) in vec4 inColor;    // RGBA Color 
layout(location = 2) in vec3 inTexCoord; // 2D texture coordinate with array texture layer index inside the z component
layout(location = 3) flat in uint inState; // 2 bits Blending mode (0 = No blending, 1 = Additive blending, 2 = Alpha blending), 2 bits = Rendering mode (0 = Normal, 1 = SDF font), 8 bits = object type 
layout(location = 4) in vec4 inClipRect; // xy = Left Top, zw = Right Bottom
layout(location = 5) in vec4 inMetaInfo; // Various stuff

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
  switch(int((inState >> 2u) & 0x3u)){ 
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
#if 1
  int blendingMode = int((inState >> 0u) & 0x3u);
  color = vec4(color.rgb, float(blendingMode < 2)) * mix(color.a, clamp(floor(color.a + 0.5), 0.0, 1.0), float(blendingMode >= 2));
#else
  switch(int((inState >> 0u) & 0x3u)){
    case 1:{
      // Alpha blending
      color = vec4(color.rgb, 1.0) * color.a;
      break;
    }
    case 2:{
      // Additive blending
      color = vec4(color.rgb, 0.0) * color.a;
      break;
    }
    default:{
      // No blending
      color = vec4(color.rgb, 1.0) * clamp(floor(color.a + 0.5), 0.0, 1.0);
      break;
    }
  }
#endif
  outFragColor = color * 
                 (step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w));
}