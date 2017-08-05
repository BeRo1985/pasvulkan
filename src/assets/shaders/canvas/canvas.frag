#version 450 core

// Copyright (C) 2017, Benjamin 'BeRo' Rosseaux (benjamin@rosseaux.de)
// License: zlib 

#define FILLTYPE_NO_TEXTURE 0
#define FILLTYPE_TEXTURE 1
#define FILLTYPE_ATLAS_TEXTURE 2

#ifndef FILLTYPE
  #define FILLTYPE FILLTYPE_COLOR
#endif

#define SIGNEDDISTANCEDFIELD

layout(location = 0) in vec2 inPosition; // 2D position
layout(location = 1) in vec4 inColor;    // RGBA Color (in linear space, NOT in sRGB non-linear color space!)
layout(location = 2) in vec3 inTexCoord; // 2D texture coordinate with array texture layer index inside the z component
layout(location = 3) flat in ivec4 inState; // x = Rendering mode, y = object type, z = not used yet, w = not used yet
layout(location = 4) in vec4 inClipRect; // xy = Left Top, zw = Right Bottom
layout(location = 5) in vec4 inMetaInfo; // Various stuff

#if FILLTYPE == FILLTYPE_ATLAS_TEXTURE 
layout(binding = 0) uniform sampler2DArray uTexture;
#elif FILLTYPE == FILLTYPE_TEXTURE
layout(binding = 0) uniform sampler2D uTexture;
#endif

layout(location = 0) out vec4 outFragColor;

layout(push_constant) uniform PushConstants {
  layout(offset = 0) mat4 transformMatrix;
  layout(offset = 64) mat4 fillMatrix;
} pushConstants;

// Some facts about a sRGB non-linear frame-buffer from the Vulkan specification:
//   If the numeric format of a framebuffer attachment uses sRGB encoding, the R, G, and B destination color values 
//   (after conversion from fixed-point to floating-point) are considered to be encoded for the sRGB color space and 
//   hence are linearized prior to their use in blending. Each R, G, and B component is converted from nonlinear to 
//   linear as described in the "KHR_DF_TRANSFER_SRGB" section of the Khronos Data Format Specification. If the format 
//   is not sRGB, no linearization is performed.
//   If the numeric format of a framebuffer attachment uses sRGB encoding, then the final R, G and B values are converted 
//   into the nonlinear sRGB representation before being written to the framebuffer attachment as described in the 
//   "KHR_DF_TRANSFER_SRGB" section of the Khronos Data Format Specification.
//   If the framebuffer color attachment numeric format is not sRGB encoded then the resulting cscs values for R, G and B 
//   are unmodified. The value of A is never sRGB encoded. That is, the alpha component is always stored in memory as linear.
// and:
//   If the image format is sRGB, the color components are first converted as if they are UNORM, and then sRGB to linear 
//   conversion is applied to the R, G, and B components as described in the "KHR_DF_TRANSFER_SRGB" section of the Khronos
//   Data Format Specification. The A component, if present, is unchanged.

// Define our own linearstep function for to map distance coverage, when we doing our calculations in the linear color space. 
// Smoothstep's nonlinear response is actually doing some fake-gamma, so it ends up over-correcting when the output is already gamma-correct.
#define TEMPLATE_LINEARSTEP(DATATYPE) \
  DATATYPE linearstep(DATATYPE edge0, DATATYPE edge1, DATATYPE value){ \
    return clamp((value - edge0) / (edge1 - edge0), DATATYPE(0.0), DATATYPE(1.0)); \
  }
TEMPLATE_LINEARSTEP(float)  
TEMPLATE_LINEARSTEP(vec2)  
TEMPLATE_LINEARSTEP(vec3)  
TEMPLATE_LINEARSTEP(vec4)  

vec4 blend(vec4 a, vec4 b){
  return mix(a, b, b.a); 
}           
                                  
const float SQRT_0_DOT_5 = sqrt(0.5);

#ifdef SIGNEDDISTANCEDFIELD
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
#endif

#ifdef GUI_ELEMENTS
float sdRoundedRect(vec2 p, vec2 b, float r){
  b -= vec2(r);
  vec2 d = abs(p) - b;
  return min(max(d.x, d.y), 0.0) + length(max(abs(p) - b, 0.0)) - r;
}
#endif

void main(void){
  vec4 color;
#if (FILLTYPE == FILLTYPE_NO_TEXTURE) || (FILLTYPE == FILLTYPE_TEXTURE)
  mat3x2 fillTransformMatrix = mat3x2(pushConstants.fillMatrix[0].xy, 
                                      pushConstants.fillMatrix[1].xy, 
                                      vec2(pushConstants.fillMatrix[0].z, pushConstants.fillMatrix[1].z));
#endif
#if !((FILLTYPE == FILLTYPE_TEXTURE) || (FILLTYPE == FILLTYPE_ATLAS_TEXTURE))
  color = inColor;
#else 
#if FILLTYPE == FILLTYPE_ATLAS_TEXTURE
  #define ADJUST_TEXCOORD(uv) vec3(uv, texCoord.z)
  #define texCoord inTexCoord
#else
  #define ADJUST_TEXCOORD(uv) uv
  vec2 texCoord = ((inState.z & 0x03) == 0x01) ? (fillTransformMatrix * vec3(inPosition, 1.0)).xy : inTexCoord.xy;
#endif
  switch(inState.x){ 
    case 1:{
      const float HALF_BY_SQRT_TWO = 0.5 / sqrt(2.0), ONE_BY_THREE = 1.0 / 3.0;     
      float center = textureLod(uTexture, texCoord, 0.0).w;
      vec2 centerGradient = vec2(dFdx(center), dFdy(center));       
      float centerGradientSquaredLength = dot(centerGradient, centerGradient);
      if(centerGradientSquaredLength < 1e-4){
        centerGradient = vec2(0.7071);
      }else{
        centerGradient *= inversesqrt(centerGradientSquaredLength);
      }
      vec2 Jdx = dFdx(texCoord.xy), Jdy = dFdy(texCoord.xy);
      vec2 gradient = vec2((centerGradient.x * Jdx.x) + (centerGradient.y * Jdy.x),
                           (centerGradient.x * Jdx.y) + (centerGradient.y * Jdy.y));
      vec2 width = vec2(0.5) + (vec2(-SQRT_0_DOT_5, SQRT_0_DOT_5) * length(gradient));
      vec4 buv = texCoord.xyxy + (vec2((Jdx + Jdy) * HALF_BY_SQRT_TWO).xyxy * vec2(-1.0, 1.0).xxyy);
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
#if FILLTYPE == FILLTYPE_NO_TEXTURE
  if((inState.z & 0x03) >= 0x02){
    vec2 gradientPosition = (fillTransformMatrix * vec3(inPosition, 1.0)).xy;      
    float gradientTime = 0.0;
    switch(inState.z & 0x03){
      case 0x02:{
        // Linear gradient
        gradientTime = gradientPosition.x;
        break;
      }
      case 0x03:{
        // Radial gradient
        gradientTime = length(gradientPosition);
        break;
      }
    }
    switch((inState.z >> 2) & 0x03){
      case 0x01:{
        // Repeat
        gradientTime = fract(gradientTime);
        break;
      }
      case 0x02:{
        // Mirrored repeat
        gradientTime = 1.0 - abs(mod(gradientTime, 2.0) - 1.0);
        break;
      }
    }
    color *= mix(pushConstants.fillMatrix[2], pushConstants.fillMatrix[3], clamp(gradientTime, 0.0, 1.0));
  }
#endif
#ifdef SIGNEDDISTANCEDFIELD
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
#endif
#ifdef GUI_ELEMENTS

#endif
  outFragColor = color * (step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w));
}