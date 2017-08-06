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

#ifdef GUI_ELEMENTS

#define GUI_ELEMENT_WINDOW_HEADER 1
#define GUI_ELEMENT_WINDOW_FILL 2
#define GUI_ELEMENT_WINDOW_DROPSHADOW 3
#define GUI_ELEMENT_MOUSE_CURSOR_ARROW 4
#define GUI_ELEMENT_MOUSE_CURSOR_BEAM 5
#define GUI_ELEMENT_MOUSE_CURSOR_BUSY 6
#define GUI_ELEMENT_MOUSE_CURSOR_CROSS 7
#define GUI_ELEMENT_MOUSE_CURSOR_EW 8
#define GUI_ELEMENT_MOUSE_CURSOR_HELP 9
#define GUI_ELEMENT_MOUSE_CURSOR_LINK 10
#define GUI_ELEMENT_MOUSE_CURSOR_MOVE 11
#define GUI_ELEMENT_MOUSE_CURSOR_NESW 12
#define GUI_ELEMENT_MOUSE_CURSOR_NS 13
#define GUI_ELEMENT_MOUSE_CURSOR_NWSE 14
#define GUI_ELEMENT_MOUSE_CURSOR_PEN 15
#define GUI_ELEMENT_MOUSE_CURSOR_UNAVAILABLE 16
#define GUI_ELEMENT_MOUSE_CURSOR_UP 17     

const float uWindowCornerRadius = 8.0;
const float uWindowHeaderHeight = 32.0;
const float uWindowHeaderCornerRadius = 4.0;
const float uWindowDropShadowSize = 10.0;
const float uButtonCornerRadius = 2.0;
const float uTabBorderWidth = 0.75;
const float uTabInnerMargin = 5.0;
const float uTabMinButtonWidth = 20.0;
const float uTabMaxButtonWidth = 160.0;
const float uTabControlWidth = 20.0;
const float uTabButtonHorizontalPadding = 10.0;
const float uTabButtonVerticalPadding = 2.0; 

#define MAKE_GRAY_COLOR(a, b) vec4(vec3(pow((a) / 255.0, 2.2)), (b) / 255.0)

const vec4 uDropShadow = MAKE_GRAY_COLOR(0.0, 128.0);
const vec4 uTransparent = MAKE_GRAY_COLOR(0.0, 0.0);
const vec4 uBorderDark = MAKE_GRAY_COLOR(29.0, 255.0);
const vec4 uBorderLight = MAKE_GRAY_COLOR(92.0, 255.0);
const vec4 uBorderMedium = MAKE_GRAY_COLOR(35.0, 255.0);
const vec4 uTextColor = MAKE_GRAY_COLOR(255.0, 160.0);
const vec4 uDisabledTextColor = MAKE_GRAY_COLOR(255.0, 80.0);
const vec4 uTextColorShadow = MAKE_GRAY_COLOR(0.0, 160.0);
const vec4 uIconColor = MAKE_GRAY_COLOR(255.0, 160.0);

const vec4 uFocusedButtonGradientTop = MAKE_GRAY_COLOR(64.0, 255.0);
const vec4 uFocusedButtonGradientBottom = MAKE_GRAY_COLOR(48.0, 255.0);
const vec4 uUnfocusedButtonGradientTop = MAKE_GRAY_COLOR(74.0, 255.0);
const vec4 uUnfocusedButtonGradientBottom = MAKE_GRAY_COLOR(58.0, 255.0);
const vec4 uPushedButtonGradientTop = MAKE_GRAY_COLOR(41.0, 255.0);
const vec4 uPushedButtonGradientBottom = MAKE_GRAY_COLOR(29.0, 255.0);

const vec4 uUnfocusedWindowFill = MAKE_GRAY_COLOR(43.0, 230.0);
const vec4 uFocusedWindowFill = MAKE_GRAY_COLOR(45.0, 230.0);

const vec4 uUnfocusedWindowFillBorder = MAKE_GRAY_COLOR(21.5, 230.0);
const vec4 uFocusedWindowFillBorder = MAKE_GRAY_COLOR(22.5, 230.0);

const vec4 uUnfocusedWindowTitle = MAKE_GRAY_COLOR(220.0, 160.0);
const vec4 uFocusedWindowTitle = MAKE_GRAY_COLOR(255.0, 190.0);

const vec4 uUnfocusedWindowHeaderGradientTop = MAKE_GRAY_COLOR(64.0, 255.0);
const vec4 uUnfocusedWindowHeaderGradientBottom = MAKE_GRAY_COLOR(48.0, 255.0);

const vec4 uFocusedWindowHeaderGradientTop = MAKE_GRAY_COLOR(74.0, 255.0);
const vec4 uFocusedWindowHeaderGradientBottom = MAKE_GRAY_COLOR(58.0, 255.0);

const vec4 uUnfocusedWindowHeaderBorderGradientTop = MAKE_GRAY_COLOR(54.0, 255.0);
const vec4 uUnfocusedWindowHeaderBorderGradientBottom = MAKE_GRAY_COLOR(38.0, 255.0);

const vec4 uFocusedWindowHeaderBorderGradientTop = MAKE_GRAY_COLOR(64.0, 255.0);
const vec4 uFocusedWindowHeaderBorderGradientBottom = MAKE_GRAY_COLOR(48.0, 255.0);

const vec4 uUnfocusedWindowHeaderSeperatorTop = MAKE_GRAY_COLOR(92.0, 255.0);
const vec4 uUnfocusedWindowHeaderSeperatorBottom = MAKE_GRAY_COLOR(29.0, 255.0);

const vec4 uFocusedWindowHeaderSeperatorTop = MAKE_GRAY_COLOR(92.0, 255.0);
const vec4 uFocusedWindowHeaderSeperatorBottom = MAKE_GRAY_COLOR(29.0, 255.0);

const vec4 uUnfocusedWindowDropShadow = MAKE_GRAY_COLOR(0.0, 128.0);
const vec4 uFocusedWindowDropShadow = MAKE_GRAY_COLOR(0.0, 128.0);

const float uUnfocusedWindowDropShadowSize = 16.0;
const float uFocusedWindowDropShadowSize = 16.0;

const vec4 uWindowPopup = MAKE_GRAY_COLOR(50.0, 255.0);
const vec4 uWindowPopupTransparent = MAKE_GRAY_COLOR(50.0, 0.0); 

#endif
                                  
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

float sdTriangle(in vec2 p0, in vec2 p1, in vec2 p2, in vec2 p){  
  vec2 e0 = p1 - p0, e1 = p2 - p1, e2 = p0 - p2,
       v0 = p - p0, v1 = p - p1, v2 = p - p2,
       pq0 = v0 - (e0 * clamp(dot(v0, e0) / dot(e0, e0), 0.0, 1.0)),
       pq1 = v1 - (e1 * clamp(dot(v1, e1) / dot(e1, e1), 0.0, 1.0)),
       pq2 = v2 - (e2 * clamp(dot(v2, e2) / dot(e2, e2), 0.0, 1.0)),
       d = min(min(vec2(dot(pq0, pq0), (v0.x * e0.y) - (v0.y * e0.x)),  
                   vec2(dot(pq1, pq1), (v1.x * e1.y) - (v1.y * e1.x))),  
                   vec2(dot(pq2, pq2), (v2.x * e2.y) - (v2.y * e2.x)));  
  return -sqrt(d.x) * sign(d.y);  
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
      vec2 width = vec2(0.5) + (vec2(-SQRT_0_DOT_5, SQRT_0_DOT_5) * length(vec2(dFdx(center), dFdy(center))));
      vec4 buv = texCoord.xyxy + (vec2((dFdx(texCoord.xy) + dFdy(texCoord.xy)) * HALF_BY_SQRT_TWO).xyxy * vec2(-1.0, 1.0).xxyy);
      color = vec4(vec3(1.0), clamp((linearstep(width.x, width.y, center) + 
                                                dot(linearstep(width.xxxx, 
                                                               width.yyyy,
                                                               vec4(textureLod(uTexture, ADJUST_TEXCOORD(buv.xy), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.xw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zy), 0.0).w)), vec4(0.5))) * ONE_BY_THREE, 0.0, 1.0));
      color.a = pow(color.a, 2.2);
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
  {    
    color = vec4(0.0);
    int guiElementIndex = inState.y;
    vec2 pa = inMetaInfo.xy, pb = inMetaInfo.zw, size = pb - pa;
    vec2 p = inPosition.xy - pa;
    float t = length(abs(dFdx(inPosition.xy)) + abs(dFdy(inPosition.xy))) * SQRT_0_DOT_5;   
    float focused = ((guiElementIndex & 0x80) != 0) ? 1.0 : 0.0;
    guiElementIndex &= 0x7f;
    switch(guiElementIndex){
      case GUI_ELEMENT_WINDOW_HEADER:{      
        float d = sdRoundedRect(p - (size * 0.5), 
                                size * 0.5, 
                                mix(uWindowHeaderCornerRadius, 0.0, step(size.y * 0.5, p.y)));      
        color = blend(blend(color,
                            mix(mix(uUnfocusedWindowHeaderGradientTop, 
                                    uFocusedWindowHeaderGradientTop, 
                                    focused),
                                mix(uUnfocusedWindowHeaderGradientBottom, 
                                    uFocusedWindowHeaderGradientBottom, 
                                    focused),
                               linearstep(0.0, size.y, p.y)) * 
                            vec2(1.0, linearstep(t, -t, d)).xxxy),
                      mix(mix(uUnfocusedWindowHeaderBorderGradientTop, 
                              uFocusedWindowHeaderBorderGradientTop, 
                              focused),
                          mix(uUnfocusedWindowHeaderBorderGradientBottom, 
                              uFocusedWindowHeaderBorderGradientBottom, 
                              focused),
                          linearstep(0.0, size.y, p.y)) * 
                      vec2(1.0, linearstep(t, -t, max(d, -(d + (t * 1.0))))).xxxy);
        d = max(max(p.y, -(p.y + (t * 1.0))), ((abs(p.x - (size.x * 0.5)) - ((size.x * 0.5) - uWindowHeaderCornerRadius))));
        color = blend(color,
                      uUnfocusedWindowHeaderSeperatorTop * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        d = max(max(p.y - size.y, -((p.y - size.y) + (t * 1.0))), ((abs(p.x - (size.x * 0.5)) - (size.x * 0.5))));
        color = blend(color,
                      uUnfocusedWindowHeaderSeperatorBottom * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_WINDOW_FILL:{
        float d = sdRoundedRect(p - (size * 0.5), 
                                size * 0.5, 
                                mix(0.0, uWindowCornerRadius, step(size.y * 0.5, p.y)));      
        color = blend(blend(color,
                            mix(uUnfocusedWindowFill, 
                                uFocusedWindowFill, 
                                focused) * 
                            vec2(1.0, linearstep(t, -t, d)).xxxy),
                      mix(uUnfocusedWindowFillBorder, 
                          uFocusedWindowFillBorder, 
                          focused) * 
                      vec2(1.0, linearstep(t, -t, max(d, -(d + (t * 0.5))))).xxxy);
        break;
      }
      case GUI_ELEMENT_WINDOW_DROPSHADOW:{
        float d = sdRoundedRect(p - (size * 0.5), 
                                size * 0.5, 
                                mix(uWindowHeaderCornerRadius, uWindowCornerRadius, step(size.y * 0.5, p.y)));      
        color = blend(color,
                      mix(uUnfocusedWindowDropShadow, 
                          uFocusedWindowDropShadow, 
                          focused) * 
                      vec2(1.0, linearstep(mix(uUnfocusedWindowDropShadowSize, 
                                               uFocusedWindowDropShadowSize, 
                                               focused), 
                                           0.0, 
                                           d) * 
                                           linearstep(-t * 2.0, 0.0, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_ARROW:
      case GUI_ELEMENT_MOUSE_CURSOR_HELP:{
        float a = dot(p, normalize(vec2(-1.0, 0.0)));
        float b = dot(p, normalize(vec2(0.0, 1.0)));
        float c = dot(p, normalize(vec2(1.0, 1.0))) - (size.x * 0.5);
        float e = dot(p, normalize(vec2(-0.707, 0.707)));
        float d = max(min(max(max(a, -b), c), 
                          max(max(max(a, -b), b - (size.y * 0.5)), (-a) - (size.x * 0.707))),
                          -e);
        e = dot(p, normalize(vec2(-0.5, 0.25)));
        d = min(d, max(max((e - (size.x * 0.1)), -(e - (size.x * -0.05))), -min(b - (size.y * 0.25), -a)));
        e = dot(p, normalize(vec2(0.25, 0.5)));
        d = max(d, e - (size.y * 0.8));
        if(guiElementIndex == GUI_ELEMENT_MOUSE_CURSOR_HELP){
          d = min(d, length(p + (size * vec2(-0.85, -0.75))) - (length(size) * 0.08));
          d = min(d, max(length(p + (size * vec2(-0.75, -0.2))) - (length(size) * 0.25),
                         -(length(p + (size * vec2(-0.55, -0.35))) - (length(size) * 0.2))));
        }        
        color = blend(color,
                      vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_BEAM:{
        float d = sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.08, size.y * 0.5), 0.0); 
        d = min(d, sdRoundedRect((p - (size * 0.5)) + vec2(0.0, size.y * 0.4), vec2(size.x * 0.16, size.y * 0.09), 0.0)); 
        d = min(d, sdRoundedRect((p - (size * 0.5)) - vec2(0.0, size.y * 0.4), vec2(size.x * 0.16, size.y * 0.08), 0.0)); 
        d = max(d, -sdRoundedRect((p - (size * 0.5)) - vec2(0.0, size.y * 0.55), vec2(size.x * 0.025, size.y * 0.125), 0.0)); 
        d = max(d, -sdRoundedRect((p - (size * 0.5)) + vec2(0.0, size.y * 0.55), vec2(size.x * 0.025, size.y * 0.125), 0.0)); 
        color = blend(color,
                      vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_BUSY:{
        vec2 o = p - (size * 0.5); 
        float d = max(length(p - (size * 0.5)) - length(size * 0.5),
                      -(length(p - (size * 0.5)) - length(size * 0.25))); 
        float a = atan(o.y, o.x) - inTexCoord.z;
        color = blend(color,
                      vec4(mix(vec3(0.0), 
                               mix(vec3(0.0, 1.0, 1.0), 
                                   vec3(0.0, 0.125, 1.0), 
                                   (sin(a) * 0.5) + 0.5), 
                               linearstep(0.0, -(t * 2.0), d)), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_CROSS:{
        float d = sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.0625, size.y * 0.5), 0.0); 
        d = min(d, sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.5, size.y * 0.0625), 0.0)); 
        d = max(d, -sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.0625, size.y * 0.0625), 0.0)); 
        color = blend(color,
                      vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_EW:
      case GUI_ELEMENT_MOUSE_CURSOR_NESW:
      case GUI_ELEMENT_MOUSE_CURSOR_NS:
      case GUI_ELEMENT_MOUSE_CURSOR_NWSE:{
        p -= (size * 0.5);
        switch(guiElementIndex){
          case GUI_ELEMENT_MOUSE_CURSOR_EW:{
            break;
          }                                        
          case GUI_ELEMENT_MOUSE_CURSOR_NESW:{
            p.xy = vec2((p.x * SQRT_0_DOT_5) - (p.y * SQRT_0_DOT_5), (p.x * SQRT_0_DOT_5) + (p.y * SQRT_0_DOT_5)); 
            break;
          }                                        
          case GUI_ELEMENT_MOUSE_CURSOR_NS:{
            p.xy = p.yx;
            break;
          }                                                
          case GUI_ELEMENT_MOUSE_CURSOR_NWSE:{
            p.xy = vec2((p.x * SQRT_0_DOT_5) + (p.y * SQRT_0_DOT_5), (p.x * SQRT_0_DOT_5) - (p.y * SQRT_0_DOT_5)); 
            break;
          }                                        
        }               
        float d = sdRoundedRect(p, vec2(size.x * 0.375, size.y * 0.08), 0.0); 
        d = min(d, sdTriangle((size * 0.5) + vec2(-size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(-size.x * 0.25, -size.y * 0.25),   
                              (size * 0.5) + vec2(-size.x * 0.5, size.y * 0.0),
                              p + (size * 0.5)));     
        d = min(d, sdTriangle((size * 0.5) + vec2(size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(size.x * 0.5, size.y * 0.0),
                              (size * 0.5) + vec2(size.x * 0.25, -size.y * 0.25),   
                              p + (size * 0.5)));     
        color = blend(color,
                      vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_LINK:{
        p += vec2(size.x * -0.5, size.y * -0.5);
        float d = sdRoundedRect(p + vec2(size.x * 0.5, 0.0), vec2(size.x * 0.125, size.y * 0.5), length(size) * 0.1); 
        d = min(d, sdRoundedRect(p + vec2(size.x * 0.25,  size.y * -0.25), vec2(size.x * 0.125, size.y * 0.5), length(size) * 0.1)); 
        d = min(d, sdRoundedRect(p + vec2(size.x * 0.0,  size.y * -0.35), vec2(size.x * 0.125, size.y * 0.5), length(size) * 0.1)); 
        d = min(d, sdRoundedRect(p + vec2(size.x * -0.25, size.y * -0.45), vec2(size.x * 0.125, size.y * 0.5), length(size) * 0.1)); 
        d = min(d, sdRoundedRect(p + vec2(size.x * 0.125, size.y * -0.625), vec2(size.x * 0.525, size.y * 0.5), length(size) * 0.3)); 
        p += vec2(size.x * 0.625, size.y * -0.625);
        p.xy = vec2((p.x * SQRT_0_DOT_5) + (p.y * SQRT_0_DOT_5), (p.x * SQRT_0_DOT_5) - (p.y * SQRT_0_DOT_5));       
        d = min(d, sdRoundedRect(p, vec2(size.x * 0.5, size.y * 0.15), length(size) * 0.1)); 
        color = blend(color,
                      vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_MOVE:{
        p -= (size * 0.5);
        float d = sdRoundedRect(p, vec2(size.x * 0.375, size.y * 0.08), 0.0); 
        d = min(d, sdTriangle((size * 0.5) + vec2(-size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(-size.x * 0.25, -size.y * 0.25),   
                              (size * 0.5) + vec2(-size.x * 0.5, size.y * 0.0),
                              p + (size * 0.5)));     
        d = min(d, sdTriangle((size * 0.5) + vec2(size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(size.x * 0.5, size.y * 0.0),
                              (size * 0.5) + vec2(size.x * 0.25, -size.y * 0.25),   
                              p + (size * 0.5)));     
        p = p.yx;                      
        d = min(d, sdRoundedRect(p, vec2(size.x * 0.375, size.y * 0.08), 0.0)); 
        d = min(d, sdTriangle((size * 0.5) + vec2(-size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(-size.x * 0.25, -size.y * 0.25),   
                              (size * 0.5) + vec2(-size.x * 0.5, size.y * 0.0),
                              p + (size * 0.5)));     
        d = min(d, sdTriangle((size * 0.5) + vec2(size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(size.x * 0.5, size.y * 0.0),
                              (size * 0.5) + vec2(size.x * 0.25, -size.y * 0.25),   
                              p + (size * 0.5)));     
        color = blend(color,
                      vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_PEN:{
        p -= (size * 0.5);
        p.xy = vec2((p.x * SQRT_0_DOT_5) + (p.y * SQRT_0_DOT_5), (p.x * SQRT_0_DOT_5) - (p.y * SQRT_0_DOT_5)); 
        float d =  sdRoundedRect(p, vec2(size.x * 0.8, size.y * 0.15), size.x * mix(0.8, 0.2, linearstep(0.0, size.x * 0.5, p.x)));
        color = blend(color,
                      vec4(vec3(mix(0.0, 1.0, linearstep(0.0, -(t * 2.0), d))), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;    
      }
      case GUI_ELEMENT_MOUSE_CURSOR_UNAVAILABLE:{
        p -= (size * 0.5);
        color = blend(color,
                      vec4(mix(vec3(1.0, 0.0, 0.0), 
                               vec3(1.0, 1.0, 1.0), 
                               linearstep(-t, 
                                          t, 
                                          min(max(length(p) - length(size), -(length(p) - length(size * 0.3))),
                                              sdRoundedRect(vec2((p.x * SQRT_0_DOT_5) - (p.y * SQRT_0_DOT_5), (p.x * SQRT_0_DOT_5) + (p.y * SQRT_0_DOT_5)),
                                                            vec2(size.x * 0.625, size.y * 0.175), 0.0)))), 1.0) * 
                      vec2(1.0, 
                           linearstep(t, 
                                      -t, 
                                      length(p) - length(size * 0.5))).xxxy);
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_UP:{
        p -= (size * 0.5);
        p = p.yx;                      
        float d = sdRoundedRect(p, vec2(size.x * 0.375, size.y * 0.0625), 0.0); 
        d = min(d, sdTriangle((size * 0.5) + vec2(-size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(-size.x * 0.25, -size.y * 0.25),   
                              (size * 0.5) + vec2(-size.x * 0.5, size.y * 0.0),
                              p + (size * 0.5)));     
        color = blend(color,
                      vec4(mix(vec3(0.0), vec3(1.0), linearstep(0.0, -(t * 2.0), d)), 1.0) * 
                      vec2(1.0, linearstep(t, -t, d)).xxxy);
        break;
      }
    } 
  }
#endif
  outFragColor = color * (step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w));
}