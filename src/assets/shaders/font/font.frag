#version 450 core

layout (location = 0) in vec2 inPosition;
layout (location = 1) in vec2 inTexCoord;
layout (location = 2) in vec4 inColor;

layout (binding = 0) uniform sampler2D uTexture;

layout (location = 0) out vec4 outFragColor;

const float HALF_BY_SQRT_TWO = 0.5 / sqrt(2.0),
            ONE_BY_THREE = 1.0 / 3.0;      
            
void main(void){
  float center = textureLod(uTexture, inTexCoord, 0.0).w;
  vec2 width = vec2(0.5) + (vec2(clamp(abs(dFdx(center)) + abs(dFdy(center)), 0.0, 1.0 / 1.0)) * vec2(-1.0, 1.0));
  vec4 buv = inTexCoord.xyxy + (vec2((dFdx(inTexCoord.xy) + dFdy(inTexCoord.xy)) * HALF_BY_SQRT_TWO).xyxy * vec2(-1.0, 1.0).xxyy);
  vec4 color = vec4(inColor.xyz, inColor.w * clamp((smoothstep(width.x, width.y, center) + 
                                                    dot(smoothstep(width.xxxx, 
                                                                   width.yyyy,
                                                                   vec4(textureLod(uTexture, buv.xy, 0.0).w,
                                                                        textureLod(uTexture, buv.zw, 0.0).w,
                                                                        textureLod(uTexture, buv.xw, 0.0).w,
                                                                        textureLod(uTexture, buv.zy, 0.0).w)), vec4(0.5))) * ONE_BY_THREE, 0.0, 1.0));
  if(color.a < 1e-10){
    discard;
  }
  outFragColor = color;
}
