#version 450 core

// Copyright (C) 2017, Benjamin 'BeRo' Rosseaux (benjamin@rosseaux.de)
// License: zlib 

// The PasVulkan canvas is designed for frame buffers and textures, which contains values in linear color space and not
// in the sRGB color space, for correct linear space blending, so keep it in mind, while you are reading this code.

// and therefore, you should use sRGB framebuffer and texture Vulkan TVkFormat formats, if you are using sRGB textures, 
// and sRGB displays, because the GPU itself does the sRGB=>linear (at texel fetches) and linear=>sRGB (at pixel writes)
// conversions then.  

#define FILLTYPE_NO_TEXTURE 0
#define FILLTYPE_TEXTURE 1
#define FILLTYPE_ATLAS_TEXTURE 2

#ifndef FILLTYPE
  #define FILLTYPE FILLTYPE_NO_TEXTURE
#endif

#define SIGNEDDISTANCEDFIELD

layout(early_fragment_tests) in;

layout(location = 0) in vec2 inPosition; // 2D position
layout(location = 1) in vec4 inColor;    // RGBA Color (in linear space, NOT in sRGB non-linear color space!)
#if (FILLTYPE == FILLTYPE_TEXTURE) || (FILLTYPE == FILLTYPE_ATLAS_TEXTURE) || defined(GUI_ELEMENTS) 
layout(location = 2) in vec3 inTexCoord; // 2D texture coordinate with array texture layer index inside the z component
#endif
layout(location = 3) flat in ivec4 inState; // x = Rendering mode, y = object type, z = not used yet, w = not used yet
#if USECLIPDISTANCE
layout(location = 4) in vec4 inMetaInfo; // Various stuff
#else
layout(location = 4) in vec4 inClipRect; // xy = Left Top, zw = Right Bottom
layout(location = 5) in vec4 inMetaInfo; // Various stuff
#endif

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

#if 0
void main(){
  outFragColor = vec4(1.0);
}
#else
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
#define GUI_ELEMENT_BUTTON_UNFOCUSED 4
#define GUI_ELEMENT_BUTTON_FOCUSED 5
#define GUI_ELEMENT_BUTTON_PUSHED 6
#define GUI_ELEMENT_BUTTON_DISABLED 7
#define GUI_ELEMENT_FOCUSED 8
#define GUI_ELEMENT_HOVERED 9
#define GUI_ELEMENT_BOX_UNFOCUSED 10
#define GUI_ELEMENT_BOX_FOCUSED 11
#define GUI_ELEMENT_BOX_DISABLED 12  
#define GUI_ELEMENT_BOX_DARK_UNFOCUSED 13
#define GUI_ELEMENT_BOX_DARK_FOCUSED 14
#define GUI_ELEMENT_BOX_DARK_DISABLED 15  
#define GUI_ELEMENT_PANEL_ENABLED 16
#define GUI_ELEMENT_PANEL_DISABLED 17
#define GUI_ELEMENT_TAB_BUTTON_UNFOCUSED 18
#define GUI_ELEMENT_TAB_BUTTON_FOCUSED 19
#define GUI_ELEMENT_TAB_BUTTON_PUSHED 20
#define GUI_ELEMENT_TAB_BUTTON_DISABLED 21
#define GUI_ELEMENT_COLOR_WHEEL_UNFOCUSED 22
#define GUI_ELEMENT_COLOR_WHEEL_FOCUSED 23
#define GUI_ELEMENT_COLOR_WHEEL_DISABLED 24
#define GUI_ELEMENT_MOUSE_CURSOR_ARROW 64
#define GUI_ELEMENT_MOUSE_CURSOR_BEAM 65
#define GUI_ELEMENT_MOUSE_CURSOR_BUSY 66
#define GUI_ELEMENT_MOUSE_CURSOR_CROSS 67
#define GUI_ELEMENT_MOUSE_CURSOR_EW 68
#define GUI_ELEMENT_MOUSE_CURSOR_HELP 69
#define GUI_ELEMENT_MOUSE_CURSOR_LINK 70
#define GUI_ELEMENT_MOUSE_CURSOR_MOVE 71
#define GUI_ELEMENT_MOUSE_CURSOR_NESW 72
#define GUI_ELEMENT_MOUSE_CURSOR_NS 73
#define GUI_ELEMENT_MOUSE_CURSOR_NWSE 74
#define GUI_ELEMENT_MOUSE_CURSOR_PEN 75
#define GUI_ELEMENT_MOUSE_CURSOR_UNAVAILABLE 76
#define GUI_ELEMENT_MOUSE_CURSOR_UP 77     
#define GUI_ELEMENT_HIDDEN 96

const float uWindowCornerRadius = 2.0;
const float uWindowHeaderHeight = 32.0;
const float uWindowHeaderCornerRadius = 2.0;
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

#define MAKE_COLOR(r, g, b, a) vec4(vec3(pow(vec3(r, g, b) / 255.0, vec3(2.2))), (a) / 255.0)

const vec4 uDropShadow = MAKE_GRAY_COLOR(0.0, 128.0);
const vec4 uTransparent = MAKE_GRAY_COLOR(0.0, 0.0);
const vec4 uBorderDark = MAKE_GRAY_COLOR(29.0, 255.0);
const vec4 uBorderLight = MAKE_GRAY_COLOR(92.0, 255.0);
const vec4 uBorderMedium = MAKE_GRAY_COLOR(35.0, 255.0);
const vec4 uTextColor = MAKE_GRAY_COLOR(255.0, 160.0);
const vec4 uDisabledTextColor = MAKE_GRAY_COLOR(255.0, 80.0);
const vec4 uTextColorShadow = MAKE_GRAY_COLOR(0.0, 160.0);
const vec4 uIconColor = MAKE_GRAY_COLOR(255.0, 160.0);

const vec4 uUnfocusedButtonGradientTop = MAKE_GRAY_COLOR(74.0, 255.0);
const vec4 uUnfocusedButtonGradientBottom = MAKE_GRAY_COLOR(58.0, 255.0);
const vec4 uFocusedButtonGradientTop = MAKE_GRAY_COLOR(64.0, 255.0);
const vec4 uFocusedButtonGradientBottom = MAKE_GRAY_COLOR(48.0, 255.0);
const vec4 uPushedButtonGradientTop = MAKE_GRAY_COLOR(29.0, 255.0);
const vec4 uPushedButtonGradientBottom = MAKE_GRAY_COLOR(41.0, 255.0);
const vec4 uDisabledButtonGradientTop = MAKE_GRAY_COLOR(96.0, 255.0);
const vec4 uDisabledButtonGradientBottom = MAKE_GRAY_COLOR(74.0, 255.0);

const vec4 uUnfocusedBoxGradientTop = MAKE_GRAY_COLOR(74.0, 255.0);
const vec4 uUnfocusedBoxGradientBottom = MAKE_GRAY_COLOR(70.0, 255.0);
const vec4 uFocusedBoxGradientTop = MAKE_GRAY_COLOR(64.0, 255.0);
const vec4 uFocusedBoxGradientBottom = MAKE_GRAY_COLOR(68.0, 255.0);
const vec4 uDisabledBoxGradientTop = MAKE_GRAY_COLOR(94.0, 255.0);
const vec4 uDisabledBoxGradientBottom = MAKE_GRAY_COLOR(98.0, 255.0);   
const vec4 uUnfocusedBoxDarkGradientTop = MAKE_GRAY_COLOR(42.0, 255.0);
const vec4 uUnfocusedBoxDarkGradientBottom = MAKE_GRAY_COLOR(40.0, 255.0);
const vec4 uFocusedBoxDarkGradientTop = MAKE_GRAY_COLOR(38.0, 255.0);
const vec4 uFocusedBoxDarkGradientBottom = MAKE_GRAY_COLOR(32.0, 255.0);
const vec4 uDisabledBoxDarkGradientTop = MAKE_GRAY_COLOR(94.0, 255.0);
const vec4 uDisabledBoxDarkGradientBottom = MAKE_GRAY_COLOR(98.0, 255.0);

const vec4 uEnabledPanelGradientTop = MAKE_GRAY_COLOR(58.0, 255.0);
const vec4 uEnabledPanelGradientBottom = MAKE_GRAY_COLOR(54.0, 255.0);
const vec4 uDisabledPanelGradientTop = MAKE_GRAY_COLOR(96.0, 255.0);
const vec4 uDisabledPanelGradientBottom = MAKE_GRAY_COLOR(74.0, 255.0);

const vec4 uFocused = MAKE_COLOR(255.0, 192.0, 64.0, 255.0);

const vec4 uHovered = MAKE_COLOR(64.0, 192.0, 255.0, 255.0);

const vec4 uUnfocusedWindowFill = MAKE_GRAY_COLOR(43.0, 255.0);
const vec4 uFocusedWindowFill = MAKE_GRAY_COLOR(45.0, 255.0);

const vec4 uUnfocusedWindowFillBorder = MAKE_GRAY_COLOR(21.5, 255.0);
const vec4 uFocusedWindowFillBorder = MAKE_GRAY_COLOR(22.5, 255.0);

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

float sdTabButton(vec2 p, vec2 b, float r){
  // Chrome-style tab button with about 26.565051 angle slope tab shape
#if 1
  // with corner roundness 
  return sdRoundedRect(p, b - vec2((b.y - p.y) * 0.5, 0.0), r);
#else
  const vec2 n = vec2(-0.894427190999914, 0.447213595499961); // vec2(sin(vec2(1.570796326794895, 0.0) + radians(180.0 - 26.565051177078010077161700198))); 
#if 1
  // with corner roundness 
  return sdRoundedRect(p, b - vec2((p.y - b.y) * (n.y / n.x), 0.0), r);
#else
  // without corner roundness 
  return max(abs(p.y) - b.y, -(dot(vec2(abs(p.x) - b.x, p.y - b.y), n)));  
#endif
#endif
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

vec3 barycentricTriangle(in vec2 p0, in vec2 p1, in vec2 p2, in vec2 p){
  vec2 v0 = p1 - p0, v1 = p2 - p0, v2 = p - p0;
  float d00 = dot(v0, v0), d01 = dot(v0, v1), d11 = dot(v1, v1), d20 = dot(v2, v0),
        d21 = dot(v2, v1), d = (d00 * d11) - (d01 * d01);
  vec2 vw = vec2((d11 * d20) - (d01 * d21), (d00 * d21) - (d01 * d20)) / d;
  return vec3(1.0 - (vw.x + vw.y), vw);
}
                      
vec2 rotate(vec2 v, float a){
  vec2 s = sin(vec2(a) + vec2(0.0, 1.57079633));
	return mat2(s.y, -s.x, s.x, s.y) * v;
}                               

vec3 rgb2hsv(vec3 c){
  vec4 k = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0),
       p = mix(vec4(c.bg, k.wz), vec4(c.gb, k.xy), step(c.b, c.g)),
       q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
  float d = q.x - min(q.w, q.y),
        e = 1.0e-10;
  return clamp(vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x), vec3(0.0), vec3(1.0));
}

vec3 hsv2rgb(vec3 c){
    vec4 k = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs((fract(c.xxx + k.xyz) * 6.0) - k.www);
    return clamp(c.z * mix(k.xxx, clamp(p - k.xxx, 0.0, 1.0), c.y), vec3(0.0), vec3(1.0));
}

vec3 convertLinearRGBToSRGB(vec3 c){
  return mix((pow(c, vec3(1.0 / 2.4)) * vec3(1.055)) - vec3(5.5e-2), 
             c * vec3(12.92),     
             lessThan(c, vec3(3.1308e-3)));
}

vec3 convertSRGBToLinearRGB(vec3 c){
  return mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)),
             c / vec3(12.92),
             lessThan(c, vec3(4.045e-2)));
}

vec3 colorWheelConditionalConvertSRGBToLinearRGB(vec3 c){
  return mix(c,
             mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)),
                 c / vec3(12.92),
                 lessThan(c, vec3(4.045e-2))),
            step(0.5, inTexCoord.z));
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
#ifdef SIMPLE_SIGNED_DISTANCE_FIELD_WIDTH_CALCULATION
      vec2 width = vec2(0.5) + (vec2(-SQRT_0_DOT_5, SQRT_0_DOT_5) * length(vec2(dFdx(center), dFdy(center))));
#else
      // Based on: https://www.essentialmath.com/blog/?p=151 but with Adreno issue compensation, which likes to drop tiles on division by zero
      const float NORMALIZATION_THICKNESS_SCALE = SQRT_0_DOT_5 * (0.5 / 4.0); 
      vec2 centerGradient = vec2(dFdx(center), dFdy(center));
      float centerGradientSquaredLength = dot(centerGradient, centerGradient);
      if(centerGradientSquaredLength < 1e-4){
        centerGradient = vec2(SQRT_0_DOT_5); 
      }else{
        centerGradient *= inversesqrt(centerGradientSquaredLength); 
      }
      vec2 Juv = texCoord.xy * textureSize(uTexture, 0).xy,       
           Jdx = dFdx(Juv), 
           Jdy = dFdy(Juv),
           jacobianGradient = vec2((centerGradient.x * Jdx.x) + (centerGradient.y * Jdy.x), 
                                   (centerGradient.x * Jdx.y) + (centerGradient.y * Jdy.y));
      vec2 width = vec2(0.5) + (vec2(-1.0, 1.0) * min(length(jacobianGradient) * NORMALIZATION_THICKNESS_SCALE, 0.5));
#endif
      vec4 buv = texCoord.xyxy + (vec2((dFdx(texCoord.xy) + dFdy(texCoord.xy)) * HALF_BY_SQRT_TWO).xyxy * vec2(-1.0, 1.0).xxyy);
      color = vec4(vec3(1.0), clamp((linearstep(width.x, width.y, center) + 
                                                dot(linearstep(width.xxxx, 
                                                               width.yyyy,
                                                               vec4(textureLod(uTexture, ADJUST_TEXCOORD(buv.xy), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.xw), 0.0).w,
                                                                    textureLod(uTexture, ADJUST_TEXCOORD(buv.zy), 0.0).w)), vec4(0.5))) * ONE_BY_THREE, 0.0, 1.0));
      //color.a = pow(color.a, 2.2);
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
        float fy = linearstep(0.0, size.y, p.y),
              cr = mix(uWindowHeaderCornerRadius, 0.0, step(size.y * 0.5, p.y)), 
              d0 = sdRoundedRect(p - (size * 0.5), size * 0.5, cr),
              d1 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(1.0), cr);      
        color = blend(color, 
                      mix(mix(mix(mix(mix(uUnfocusedWindowHeaderGradientTop, 
                                          uFocusedWindowHeaderGradientTop, 
                                          focused),
                                      mix(uUnfocusedWindowHeaderGradientBottom, 
                                          uFocusedWindowHeaderGradientBottom, 
                                          focused),
                                      fy),
                                mix(mix(uUnfocusedWindowHeaderBorderGradientTop, 
                                        uFocusedWindowHeaderBorderGradientTop, 
                                         focused),
                                    mix(uUnfocusedWindowHeaderBorderGradientBottom, 
                                        uFocusedWindowHeaderBorderGradientBottom, 
                                        focused),
                                        fy),
                                linearstep(-t, t, d1)),
                              uUnfocusedWindowHeaderSeperatorTop, linearstep(1.0 + t, 0.0, p.y)),
                          uUnfocusedWindowHeaderSeperatorBottom, linearstep(size.y - (1.0 + t), size.y, p.y)) *
                      vec2(1.0, linearstep(t, -t, d0)).xxxy);                      
        break;
      }
      case GUI_ELEMENT_WINDOW_FILL:{
        float cr = uWindowCornerRadius, 
              d0 = sdRoundedRect(p - (size * 0.5), size * 0.5, cr),
              d1 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(1.0), cr);      
        color = blend(color, 
                      mix(mix(uUnfocusedWindowFill, 
                              uFocusedWindowFill, 
                               focused),
                          mix(uUnfocusedWindowFillBorder, 
                              uFocusedWindowFillBorder, 
                              focused),
                          linearstep(-t, t, d1)) *
                      vec2(1.0, linearstep(t, -t, d0)).xxxy);                      
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
      case GUI_ELEMENT_BUTTON_UNFOCUSED:
      case GUI_ELEMENT_BUTTON_FOCUSED:
      case GUI_ELEMENT_BUTTON_PUSHED:
      case GUI_ELEMENT_BUTTON_DISABLED:{
        float d0 = sdRoundedRect(p - (size * 0.5), size * 0.5, uButtonCornerRadius),
              d1 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(1.0), uButtonCornerRadius),      
              d2 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(2.0), uButtonCornerRadius),      
              d3 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(3.0), uButtonCornerRadius);      
        vec4 gradientTop,
             gradientBottom,
             borderTowardsLight,
             borderAwayFromLight;
        switch(guiElementIndex){
        	case GUI_ELEMENT_BUTTON_UNFOCUSED:{
            gradientTop = uUnfocusedButtonGradientTop;
            gradientBottom = uUnfocusedButtonGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        	case GUI_ELEMENT_BUTTON_FOCUSED:{
            gradientTop = uFocusedButtonGradientTop;
            gradientBottom = uFocusedButtonGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        	case GUI_ELEMENT_BUTTON_PUSHED:{
            gradientTop = uPushedButtonGradientTop;
            gradientBottom = uPushedButtonGradientBottom;
            borderTowardsLight = uBorderDark;
            borderAwayFromLight = uBorderLight;
          	break;
          }
        	case GUI_ELEMENT_BUTTON_DISABLED:{
            gradientTop = uDisabledButtonGradientTop;
            gradientBottom = uDisabledButtonGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        }
        color = blend(color, 
                      mix(mix(mix(mix((guiElementIndex == GUI_ELEMENT_BUTTON_PUSHED) ?
                                        mix(gradientTop * 0.5f,
                                            gradientTop,  
                                            linearstep(0.0, 6.0, p.y)) :
                                        gradientTop,
                                      gradientBottom, 
                                      linearstep(0.0, size.y, p.y)),
                                  mix(mix(borderTowardsLight, 
                                          uBorderMedium, 
                                          linearstep(0.0, t, p.y - 3.0)), 
                                      borderAwayFromLight, 
                                      linearstep(0.0, t, p.y - (size.y - 3.0))),
                                  linearstep(-t, t, d3)),
                              uBorderMedium, 
                              linearstep(-t, t, d2)),
                            mix(uBorderMedium,
                                uBorderLight,
                                linearstep(-t, t, p.y - (size.y - 1.0))), 
                            linearstep(-t, t, d1)) *
                      vec2(1.0, linearstep(t, -t, d0)).xxxy);                      
        break;
      }
      case GUI_ELEMENT_FOCUSED:{
        float d0 = sdRoundedRect(p - (size * 0.5), size * 0.5, 0.0);      
        float d1 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - 2.0, 0.0);      
        color = blend(color,
                      uFocused * 
                      vec2(1.0, linearstep(t, -t, max(d0, -d1))).xxxy);
        break;
      }
      case GUI_ELEMENT_HOVERED:{
        float d0 = sdRoundedRect(p - (size * 0.5), size * 0.5, 0.0);      
        float d1 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - 2.0, 0.0);      
        color = blend(color,
                      uHovered * 
                      vec2(1.0, linearstep(t, -t, max(d0, -d1))).xxxy);
        break;
      }
      case GUI_ELEMENT_BOX_UNFOCUSED:  
      case GUI_ELEMENT_BOX_FOCUSED:
      case GUI_ELEMENT_BOX_DISABLED:
      case GUI_ELEMENT_BOX_DARK_UNFOCUSED:  
      case GUI_ELEMENT_BOX_DARK_FOCUSED:
      case GUI_ELEMENT_BOX_DARK_DISABLED:{
        float d0 = sdRoundedRect(p - (size * 0.5), size * 0.5, uButtonCornerRadius),
              d1 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(0.5), uButtonCornerRadius),      
              d2 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(1.0), uButtonCornerRadius),      
              d3 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(2.0), uButtonCornerRadius);      
        vec4 gradientTop,
             gradientBottom,
             borderTowardsLight,
             borderAwayFromLight;
        switch(guiElementIndex){
        	case GUI_ELEMENT_BOX_UNFOCUSED:{
            gradientTop = uUnfocusedBoxGradientTop;
            gradientBottom = uUnfocusedBoxGradientBottom;
            borderTowardsLight = uBorderDark; 
            borderAwayFromLight = uBorderMedium;
          	break;
          }
        	case GUI_ELEMENT_BOX_FOCUSED:{
            gradientTop = uFocusedBoxGradientTop;
            gradientBottom = uFocusedBoxGradientBottom;
            borderTowardsLight = uBorderDark; 
            borderAwayFromLight = uBorderMedium;
          	break;
          }
        	case GUI_ELEMENT_BOX_DISABLED:{
            gradientTop = uDisabledBoxGradientTop;
            gradientBottom = uDisabledBoxGradientBottom;
            borderTowardsLight = uBorderDark; 
            borderAwayFromLight = uBorderMedium;
          	break;
          }
        	case GUI_ELEMENT_BOX_DARK_UNFOCUSED:{
            gradientTop = uUnfocusedBoxDarkGradientTop;
            gradientBottom = uUnfocusedBoxDarkGradientBottom;
            borderTowardsLight = uBorderDark; 
            borderAwayFromLight = uBorderMedium;
          	break;
          }
        	case GUI_ELEMENT_BOX_DARK_FOCUSED:{
            gradientTop = uFocusedBoxDarkGradientTop;
            gradientBottom = uFocusedBoxDarkGradientBottom;
            borderTowardsLight = uBorderDark; 
            borderAwayFromLight = uBorderMedium;
          	break;
          }
        	case GUI_ELEMENT_BOX_DARK_DISABLED:{
            gradientTop = uDisabledBoxDarkGradientTop;
            gradientBottom = uDisabledBoxDarkGradientBottom;
            borderTowardsLight = uBorderDark; 
            borderAwayFromLight = uBorderMedium;
          	break;
          }
        }
        color = blend(color, 
                      mix(mix(mix(mix(mix(gradientTop * 0.5f,
                                          gradientTop,  
                                          linearstep(0.0, 6.0, p.y)),
                                      gradientBottom, 
                                      linearstep(0.0, size.y, p.y)),
                                  mix(mix(borderTowardsLight, 
                                          uBorderMedium, 
                                          linearstep(0.0, t, p.y - 3.0)), 
                                      borderAwayFromLight, 
                                      linearstep(0.0, t, p.y - (size.y - 3.0))),
                                  linearstep(-t, t, d3)),
                              uBorderMedium, 
                              linearstep(-t, t, d2)),
                            mix(uBorderMedium,
                                uBorderLight,
                                linearstep(-t, t, p.y - (size.y - 1.0))), 
                            linearstep(-t, t, d1)) *
                      vec2(1.0, linearstep(t, -t, d0)).xxxy);                      
        break;
      }
      case GUI_ELEMENT_PANEL_ENABLED:
      case GUI_ELEMENT_PANEL_DISABLED:{
        float d0 = sdRoundedRect(p - (size * 0.5), size * 0.5, uButtonCornerRadius),
              d1 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(1.0), uButtonCornerRadius),      
              d2 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(2.0), uButtonCornerRadius),      
              d3 = sdRoundedRect(p - (size * 0.5), (size * 0.5) - vec2(3.0), uButtonCornerRadius);      
        vec4 gradientTop,
             gradientBottom,
             borderTowardsLight,
             borderAwayFromLight;
        switch(guiElementIndex){
        	case GUI_ELEMENT_PANEL_ENABLED:{
            gradientTop = uEnabledPanelGradientTop;
            gradientBottom = uEnabledPanelGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        	case GUI_ELEMENT_PANEL_DISABLED:{
            gradientTop = uDisabledPanelGradientTop;
            gradientBottom = uDisabledPanelGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        }
        color = blend(color, 
                      mix(mix(mix(mix(gradientTop,
                                      gradientBottom, 
                                      linearstep(0.0, size.y, p.y)),
                                  mix(mix(borderTowardsLight, 
                                          uBorderMedium, 
                                          linearstep(0.0, t, p.y - 3.0)), 
                                      borderAwayFromLight, 
                                      linearstep(0.0, t, p.y - (size.y - 3.0))),
                                  linearstep(-t, t, d3)),
                              uBorderMedium, 
                              linearstep(-t, t, d2)),
                            mix(uBorderMedium,
                                uBorderLight,
                                linearstep(-t, t, p.y - (size.y - 1.0))), 
                            linearstep(-t, t, d1)) *
                      vec2(1.0, linearstep(t, -t, d0)).xxxy);                      
        break;
      }      
      case GUI_ELEMENT_TAB_BUTTON_UNFOCUSED:
      case GUI_ELEMENT_TAB_BUTTON_FOCUSED:
      case GUI_ELEMENT_TAB_BUTTON_PUSHED:
      case GUI_ELEMENT_TAB_BUTTON_DISABLED:{
        float d0 = sdTabButton(p - (size * 0.5), size * 0.5, uButtonCornerRadius),
              d1 = sdTabButton(p - (size * 0.5), (size * 0.5) - vec2(1.0), uButtonCornerRadius),      
              d2 = sdTabButton(p - (size * 0.5), (size * 0.5) - vec2(2.0), uButtonCornerRadius),      
              d3 = sdTabButton(p - (size * 0.5), (size * 0.5) - vec2(3.0), uButtonCornerRadius);      
        vec4 gradientTop,
             gradientBottom,
             borderTowardsLight,
             borderAwayFromLight;
        switch(guiElementIndex){
        	case GUI_ELEMENT_TAB_BUTTON_UNFOCUSED:{
            gradientTop = uUnfocusedButtonGradientTop;
            gradientBottom = uUnfocusedButtonGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        	case GUI_ELEMENT_TAB_BUTTON_FOCUSED:{
            gradientTop = uFocusedButtonGradientTop;
            gradientBottom = uFocusedButtonGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        	case GUI_ELEMENT_TAB_BUTTON_PUSHED:{
            gradientTop = uPushedButtonGradientTop;
            gradientBottom = uPushedButtonGradientBottom;
            borderTowardsLight = uBorderDark;
            borderAwayFromLight = uBorderLight;
          	break;
          }
        	case GUI_ELEMENT_TAB_BUTTON_DISABLED:{
            gradientTop = uDisabledButtonGradientTop;
            gradientBottom = uDisabledButtonGradientBottom;
            borderTowardsLight = uBorderLight;
            borderAwayFromLight = uBorderDark;
          	break;
          }
        }
        color = blend(color, 
                      mix(mix(mix(mix((guiElementIndex == GUI_ELEMENT_TAB_BUTTON_PUSHED) ?
                                        mix(gradientTop * 0.5f,
                                            gradientTop,  
                                            linearstep(0.0, 6.0, p.y)) :
                                        gradientTop,
                                      gradientBottom, 
                                      linearstep(0.0, size.y, p.y)),
                                  mix(mix(borderTowardsLight, 
                                          uBorderMedium, 
                                          linearstep(0.0, t, p.y - 3.0)), 
                                      borderAwayFromLight, 
                                      linearstep(0.0, t, p.y - (size.y - 3.0))),
                                  linearstep(-t, t, d3)),
                              uBorderMedium, 
                              linearstep(-t, t, d2)),
                            mix(uBorderMedium,
                                uBorderLight,
                                linearstep(-t, t, p.y - (size.y - 1.0))), 
                            linearstep(-t, t, d1)) *
                      vec2(1.0, linearstep(t, -t, d0)).xxxy);                      
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
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_BEAM:{
        float d = sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.08, size.y * 0.5), 0.0); 
        d = min(d, sdRoundedRect((p - (size * 0.5)) + vec2(0.0, size.y * 0.4), vec2(size.x * 0.16, size.y * 0.09), 0.0)); 
        d = min(d, sdRoundedRect((p - (size * 0.5)) - vec2(0.0, size.y * 0.4), vec2(size.x * 0.16, size.y * 0.08), 0.0)); 
        d = max(d, -sdRoundedRect((p - (size * 0.5)) - vec2(0.0, size.y * 0.55), vec2(size.x * 0.025, size.y * 0.125), 0.0)); 
        d = max(d, -sdRoundedRect((p - (size * 0.5)) + vec2(0.0, size.y * 0.55), vec2(size.x * 0.025, size.y * 0.125), 0.0)); 
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_BUSY:{
        vec2 o = p - (size * 0.5); 
        float d = max(length(p - (size * 0.5)) - length(size * 0.5),
                      -(length(p - (size * 0.5)) - length(size * 0.25))); 
        if(focused > 0.5){ 
          float a = atan(o.y, o.x) - inTexCoord.z;
          color = blend(color,
                        vec4(mix(vec3(0.0), 
                                 mix(vec3(0.0, 1.0, 1.0), 
                                     vec3(0.0, 0.125, 1.0), 
                                     (sin(a) * 0.5) + 0.5), 
                                 linearstep(0.0, -(t * 2.0), d)), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);        
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }

        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_CROSS:{
        float d = sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.08, size.y * 0.5), 0.0); 
        d = min(d, sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.5, size.y * 0.08), 0.0)); 
        d = max(d, -sdRoundedRect(p - (size * 0.5), vec2(size.x * 0.08, size.y * 0.08), 0.0)); 
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
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
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
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
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
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
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_PEN:{
        p -= (size * 0.5);
        p.xy = vec2((p.x * SQRT_0_DOT_5) + (p.y * SQRT_0_DOT_5), (p.x * SQRT_0_DOT_5) - (p.y * SQRT_0_DOT_5)); 
        float d =  sdRoundedRect(p, vec2(size.x * 0.8, size.y * 0.15), size.x * mix(0.8, 0.2, linearstep(0.0, size.x * 0.5, p.x)));
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
        break;    
      }
      case GUI_ELEMENT_MOUSE_CURSOR_UNAVAILABLE:{
        p -= (size * 0.5);
        if(focused > 0.5){ 
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
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, length(p) - length(size * 0.5))).xxxy);
        }
        break;
      }
      case GUI_ELEMENT_MOUSE_CURSOR_UP:{
        p -= (size * 0.5);
        p = p.yx;                      
        float d = sdRoundedRect(p, vec2(size.x * 0.375, size.y * 0.08), 0.0); 
        d = min(d, sdTriangle((size * 0.5) + vec2(-size.x * 0.25, size.y * 0.25),
                              (size * 0.5) + vec2(-size.x * 0.25, -size.y * 0.25),   
                              (size * 0.5) + vec2(-size.x * 0.5, size.y * 0.0),
                              p + (size * 0.5)));     
        if(focused > 0.5){ 
          color = blend(color,
                        vec4(vec3(mix(0.0, 1.0, linearstep(-1.0, -(1.0 + (t * 1.0)), d))), 1.0) * 
                        vec2(1.0, linearstep(t, -t, d)).xxxy);
        }else{
          color = blend(color,
                        vec4(vec3(0.0), 0.95) * 
                        vec2(1.0, linearstep(t, -((length(size) * SQRT_0_DOT_5) * 0.125) + t, d)).xxxy);
        }
        break;
      }
      case GUI_ELEMENT_HIDDEN:{
        color = vec4(0.0);
        break;        
      }
      case GUI_ELEMENT_COLOR_WHEEL_UNFOCUSED:
      case GUI_ELEMENT_COLOR_WHEEL_FOCUSED:
      case GUI_ELEMENT_COLOR_WHEEL_DISABLED:{
        float v = 1.0, w = 0.0; 
        switch(guiElementIndex){
          case GUI_ELEMENT_COLOR_WHEEL_UNFOCUSED:{
            break;
          }                                     
          case GUI_ELEMENT_COLOR_WHEEL_FOCUSED:{
            w = 1.0;
            break;
          }                                     
          case GUI_ELEMENT_COLOR_WHEEL_DISABLED:{
            v = 0.75; 
            break;
          }                                     
        }
        vec3 hsv = inColor.xyz;
        p -= (size * 0.5);
        vec2 size2 = vec2(min(size.x, size.y)) * SQRT_0_DOT_5 * 0.95,
             p2 = rotate(p, hsv.x * 6.28318531) + (size2 * 0.5);
        float r = length(size2) * 0.5, 
              r2 = r * SQRT_0_DOT_5,
              pa = atan(p.y, p.x) / 6.28318531,
              pad = fract((pa - fract(hsv.x + 0.495)) + 0.5) - 0.5; 
        vec2 tv0 = (size2 * 0.5) + rotate(vec2(r2, 0.0), radians(0.0)),
             tv1 = (size2 * 0.5) + rotate(vec2(r2, 0.0), radians(120.0)),             
             tv2 = (size2 * 0.5) + rotate(vec2(r2, 0.0), radians(-120.0)),
             tv = tv2 + ((tv1 - tv2) * hsv.z) + ((tv0 - tv1) * (hsv.y * hsv.z));
        float d0 = max(length(p) - r, -(length(p) - (r * 0.75))),
              d1 = sdTriangle(tv0, tv1, tv2, p2),
              d2 = length(tv - p2),
              d3 = max(d2 - (r * 0.08), -(d2 - (r * 0.04))),
              d4 = d2 - (r * mix(0.04, 0.08, 0.5)),
              d5 = d2 - (r * 0.07),
              d6 = max(sdRoundedRect(p2 - vec2(size2.x * 1.118, size2.y * 0.5), vec2(r * 0.13725, r * 0.04), 1e-4), 
                       -sdRoundedRect(p2 - vec2(size2.x * 1.118, size2.y * 0.5), vec2(r * 0.1, r * 0.01), 1e-4));
        vec3 b = clamp(barycentricTriangle(tv0, tv1, tv2, p2), vec3(0.0), vec3(1.0));
//      color = blend(color, vec4(1.0) * linearstep(t, -t, sdRoundedRect(p, size * 0.5, 1e-4))); 
        color = blend(blend(blend(blend(blend(color,    
                                              vec4(colorWheelConditionalConvertSRGBToLinearRGB((hsv2rgb(vec3(hsv.x, 1.0, 1.0)) * b.x) + (vec3(1.0) * b.y) + (vec3(0.0) * b.z)), 1.0) * linearstep(t, -t, d1) * v),
                                        vec4(colorWheelConditionalConvertSRGBToLinearRGB(hsv2rgb(vec3(pa, 1.0, 1.0))), 1.0) * linearstep(t, -t, d0) * v),
                                  vec4(vec3(w), mix(1.0, 0.95, w)) * linearstep(t, -t, d6) * v),
                            vec4(colorWheelConditionalConvertSRGBToLinearRGB(hsv2rgb(hsv)), 1.0) *  linearstep(t, -t, d5) * v),
                       vec4(colorWheelConditionalConvertSRGBToLinearRGB(vec3(mix(1.0 - b.z, b.z, linearstep(t, -t, d4)))), 1.0) * linearstep(t, -t, d3) * 0.5 * v);
        break;
      }
    } 
  }
#endif
#if !USECLIPDISTANCE
#if BLENDING 
  color *= step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w);
#elif !USENODISCARD
//if(step(inClipRect.x, inPosition.x) * step(inClipRect.y, inPosition.y) * step(inPosition.x, inClipRect.z) * step(inPosition.y, inClipRect.w)) < 0.5){
  if(any(lessThan(inPosition.xy, inClipRect.xy)) || 
     any(greaterThan(inPosition.xy, inClipRect.zw))){
    discard;
  }
#endif  
#endif  
  outFragColor = color;
}
#endif
