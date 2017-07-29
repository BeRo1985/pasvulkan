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

layout(push_constant) uniform PushConstants {
  layout(offset = 0) mat4 transformMatrix;
  layout(offset = 64) mat4 fillMatrix;
} pushConstants;

// Define our own linearstep function for to map distance coverage, when we have sRGB output. 
// Smoothstep's nonlinear response is actually doing some fake-gamma, so it ends up over-correcting when the output is already gamma-correct.
#define TEMPLATE_LINEARSTEP(DATATYPE) \
  DATATYPE linearstep(DATATYPE edge0, DATATYPE edge1, DATATYPE value){ \
    return clamp((value - edge0) / (edge1 - edge0), DATATYPE(0.0), DATATYPE(1.0)); \
  }
TEMPLATE_LINEARSTEP(float)  
TEMPLATE_LINEARSTEP(vec4)  

const float SQRT_0_DOT_5 = sqrt(0.5);

float sdEllipse(vec2 p, in vec2 ab){
  float d;
  if(ab.x == ab.y){
    d = length(p) - ab.x;
  }else{  
    // iq's ellipse distance function in a reformatted form
    p = abs(p); 
    if(p.x > p.y){
      p = p.yx; 
      ab = ab.yx; 
    }	
    float l = (ab.y * ab.y) - (ab.x * ab.x), m = (ab.x * p.x) / l, n = (ab.y * p.y) / l, m2 = m * m, n2 = n * n, 
          c = ((m2 + n2) - 1.0) / 3.0, c3 = c * c  * c, q = c3 + ((m2 * n2) * 2.0), d = c3 + (m2 * n2), g = m + (m * n2),
          co;
    if(d < 0.0){
      float p = acos(q / c3) / 3.0, s = cos(p), t = sin(p) * sqrt(3.0), rx = sqrt((-(c*(s + t + 2.0))) + m2), ry = sqrt((-(c * ((s - t) + 2.0))) + m2);
      co = (((ry + (sign(l) * rx)) + (abs(g) / (rx * ry))) - m) * 0.5;
    }else{
      float h = 2.0 * m * n * sqrt(d), s = sign(q + h) * pow(abs(q + h), 1.0/3.0), u = sign(q - h) * pow(abs(q-h), 1.0 / 3.0), 
            rx = (((-s) - u) - (c * 4.0)) + (2.0 * m2), ry = (s - u) * sqrt(3.0), rm = length(vec2(rx, ry)), p = ry / sqrt(rm - rx);
      co = ((p + ((2.0*g) / rm)) - m) * 0.5;
    }
    vec2 r = vec2(ab.x * co, ab.y * sqrt(1.0 - co*co));
    d = length(r - p) * sign(p.y - r.y);
  }
  return d;
}

void main(void){
  vec4 color;
#ifndef ATLAS_TEXTURE
  mat2x3 fillTransformMatrix = mat2x3(pushConstants.fillMatrix);
#endif
#ifdef NO_TEXTURE
  color = inColor;
#else 
#ifdef ATLAS_TEXTURE
  #define ADJUST_TEXCOORD(uv) vec3(uv, texCoord.z)
  #define texCoord inTexCoord
#else
  #define ADJUST_TEXCOORD(uv) uv
  vec2 texCoord = (inState.z == 0x01) ? (fillTransformMatrix * inPosition).xy : inTexCoord.xy;
#endif
  switch(inState.x){ 
    case 1:{
      const float HALF_BY_SQRT_TWO = 0.5 / sqrt(2.0), ONE_BY_THREE = 1.0 / 3.0;     
      float center = textureLod(uTexture, texCoord, 0.0).w;
      vec2 width = vec2(0.5) + (vec2(-SQRT_0_DOT_5, SQRT_0_DOT_5) * length(vec2(dFdx(center), dFdy(center))));
      vec4 buv = texCoord.xyxy + (vec2((dFdx(texCoord.xy) + dFdy(texCoord.xy)) * HALF_BY_SQRT_TWO).xyxy * vec2(-1.0, 1.0).xxyy);
      color = vec4(vec3(1.0), clamp((linearstep(width.x, width.y, center) + 
                                                dot(linearstep(width.xxxx, 
                                                               width.yyyy,
                                                               vec4(textureLod(uTexture, ADJUST_TEXCOORD(buv.xy), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.xw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zy), 0.0).w)), vec4(0.5))) * ONE_BY_THREE, 0.0, 1.0));      
      break;
    }
    default:{
      color = texture(uTexture, texCoord);
      break;
    }
  }
  color *= inColor;
#endif
#ifndef ATLAS_TEXTURE
  if(inState.z == 0x02){
    vec2 gradientPosition = (fillTransformMatrix * inPosition).xy;      
    float gradientTime = 0.0;
    int gradientFlags = int(pushConstants.fillMatrix[0].w + 0.5);
    switch(gradientFlags & 0x3){
      case 0x01:{
        // Linear gradient
        gradientTime = gradientPosition.x;
        break;
      }
      case 0x02:{
        // Radial gradient
        gradientTime = length(gradientPosition);
        break;
      }
    }
    switch((gradientFlags >> 2) & 0x3){
      case 0x01:{
        // Repeat
        gradientTime = fract(gradientTime);
        break;
      }
      case 0x02:{
        // Mirrored repeat
        gradientTime = fract(1.0 - abs(gradientTime * 2.0));
        break;
      }
    }
    color *= mix(pushConstants.fillMatrix[2], pushConstants.fillMatrix[3], clamp(gradientTime, 0.0, 1.0));
  }
#endif
  if(inState.y != 0){
    float threshold = length(abs(dFdx(inPosition.xy)) + abs(dFdy(inPosition.xy))) * SQRT_0_DOT_5;
    switch(inState.y){
      case 0x01:{
        // Distance to line edge
        color.a *= min(linearstep(0.0, -threshold, -(inMetaInfo.z - abs(inMetaInfo.x))),  // To the line edges left and right
                       linearstep(0.0, -threshold, -(inMetaInfo.w - abs(inMetaInfo.y)))); // To the line ends
        break;      
      }
      case 0x02:{
        // Distance to line round cap circle       
        color.a *= linearstep(0.0, -threshold, length(inPosition.xy - inMetaInfo.xy) - inMetaInfo.z);
        break;      
      }
      case 0x03:{
        // Distance to round line (polygon edge) 
        vec2 pa = inPosition.xy - inMetaInfo.xy, ba = inMetaInfo.zw - inMetaInfo.xy;
        color.a *= linearstep(0.0, -threshold, length(pa - (ba * (clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0)))) - threshold);
        break;      
      }
      case 0x04:{
        // Distance to circle       
        color.a *= linearstep(0.0, -threshold, length(inPosition.xy - inMetaInfo.xy) - inMetaInfo.z);
        break;      
      }
      case 0x05:{
        // Distance to ellipse
        color.a *= linearstep(0.0, -threshold, sdEllipse(inPosition.xy - inMetaInfo.xy, inMetaInfo.zw));
        break;      
      }
      case 0x06:{
        // Distance to rectangle
        vec2 d = abs(inPosition.xy - inMetaInfo.xy) - inMetaInfo.zw;
        color.a *= linearstep(0.0, -threshold, min(max(d.x, d.y), 0.0) + length(max(d, 0.0)));
        break;      
      }
    }
  }
  outFragColor = (vec4(color.rgb, inBlendFactors.x) * mix(clamp(floor(color.a + 0.5), 0.0, 1.0), color.a, inBlendFactors.y)) * 
                 (step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w));
}