#version 450 core
#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec2 inPosition; // 2D position
layout(location = 1) in vec4 inColor;    // RGBA Color 
#if defined(NO_TEXTURE)
layout(location = 2) in vec3 inMetaInfo;
#else
layout(location = 2) in vec3 inTexCoord; // 2D texture coordinate with array texture layer index inside the z component
#endif
layout(location = 3) in vec2 inState;    // x = Blending mode (-1 = No blending, 0 = Additive blending, 1 = Alpha blending), y = Rendering mode (0 = Normal, 1 = SDF font)
layout(location = 4) in vec4 inClipRect; // xy = Left Top, zw = Right Bottom

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
  if(inState.y > 0.5){
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
  }else{
#if defined(ATLAS_TEXTURE)
    color = texture(uTexture, inTexCoord);
#else
    color = texture(uTexture, inTexCoord.xy);
#endif
  }
  color *= inColor;
#endif
  outFragColor = (vec4(color.rgb, abs(inState.x)) * mix(color.a, clamp(floor(color.a + 0.5), 0.0, 1.0), clamp(-inState.x, 0.0, 1.0))) * 
                 (step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w));
}