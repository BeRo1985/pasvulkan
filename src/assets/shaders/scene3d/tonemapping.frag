#version 450 core

#extension GL_EXT_multiview : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput;

#define HDRToneMappingOperator 8

#if HDRToneMappingOperator == 1
vec3 linear(const in vec3 color) { return color; }
#elif HDRToneMappingOperator == 2
vec3 reinhard(in vec3 color) {
  color *= 1.5;
  return color / (vec3(1.0) + color);
}
#elif HDRToneMappingOperator == 3
vec3 hejl(const in vec3 color) {
  vec3 x = max(vec3(0.0), color - vec3(0.004));
  return pow((x * ((6.2 * x) + vec3(0.5))) / max(x * ((6.2 * x) + vec3(1.7)) + vec3(0.06), vec3(1e-8)), vec3(2.2));
}
#elif HDRToneMappingOperator == 4
vec3 hejl2015(vec3 c, float w) {
  vec4 h = vec4(c, w), a = (1.425 * h) + vec4(0.05), f = (((h * a) + vec4(0.004)) / ((h * (a + vec4(0.55)) + vec4(0.0491)))) - vec4(0.0821);
  return f.xyz / f.w;
}
#elif HDRToneMappingOperator == 5
vec3 ACESFilm(const in vec3 x) {
  const float a = 2.51, b = 0.03, c = 2.43, d = 0.59, e = 0.14;
  return clamp((x * ((a * x) + vec3(b))) / (x * ((c * x) + vec3(d)) + vec3(e)), vec3(0.0), vec3(1.0));
}
#elif HDRToneMappingOperator == 6
vec3 ACESFilm(const in vec3 x) {
  const float a = 2.51, b = 0.03, c = 2.43, d = 0.59, e = 0.14;
  return pow(clamp((x * ((a * x) + vec3(b))) / (x * ((c * x) + vec3(d)) + vec3(e)), vec3(0.0), vec3(1.0)), vec3(2.2));
}
#elif HDRToneMappingOperator == 7
vec3 uncharted2(in vec3 color) {
  float A = 0.15;
  float B = 0.50;
  float C = 0.10;
  float D = 0.20;
  float E = 0.02;
  float F = 0.30;
  float W = 11.2;
  float IW = 1.0 / (((W * ((A * W) + (C * B)) + (D * E)) / (W * ((A * W) + B) + (D * F))) - (E / F));
  color *= 5.0;
  return (((color * ((A * color) + vec3(C * B)) + vec3(D * E)) / (color * ((A * color) + vec3(B)) + vec3(D * F))) - vec3(E / F)) * IW;
}
#elif HDRToneMappingOperator == 8
vec3 uchimura(in vec3 x, in float P, in float a, in float m, in float l, in float c, in float b) {
  float l0 = ((P - m) * l) / a;
  float L0 = m - m / a;
  float L1 = m + (1.0 - m) / a;
  float S0 = m + l0;
  float S1 = m + a * l0;
  float C2 = (a * P) / (P - S1);
  float CP = -C2 / P;

  vec3 w0 = vec3(1.0 - smoothstep(0.0, m, x));
  vec3 w2 = vec3(step(m + l0, x));
  vec3 w1 = vec3(1.0 - w0 - w2);

  vec3 T = vec3(m * pow(x / m, vec3(c)) + b);
  vec3 S = vec3(P - (P - S1) * exp(CP * (x - S0)));
  vec3 L = vec3(m + a * (x - m));

  return T * w0 + L * w1 + S * w2;
}

vec3 uchimura(in vec3 x) {
  const float P = 1.0;   // max display brightness
  const float a = 1.0;   // contrast
  const float m = 0.22;  // linear section start
  const float l = 0.4;   // linear section length
  const float c = 1.33;  // black

  return uchimura(x, P, a, m, l, c, 0.0);
}
#elif HDRToneMappingOperator == 9
vec3 lottes(in vec3 x) {
  const vec3 a = vec3(1.6);
  const vec3 d = vec3(0.977);
  const vec3 hdrMax = vec3(8.0);
  const vec3 midIn = vec3(0.18);
  const vec3 midOut = vec3(0.267);

  const vec3 b = (-pow(midIn, a) + pow(hdrMax, a) * midOut) / ((pow(hdrMax, a * d) - pow(midIn, a * d)) * midOut);
  const vec3 c = (pow(hdrMax, a * d) * pow(midIn, a) - pow(hdrMax, a) * pow(midIn, a * d) * midOut) / ((pow(hdrMax, a * d) - pow(midIn, a * d)) * midOut);

  return pow(x, a) / (pow(x, a * d) * b + c);
}
#elif HDRToneMappingOperator == 10

// Source: https://github.com/GPUOpen-LibrariesAndSDKs/Cauldron/blob/master/src/VK/shaders/tonemappers.glsl

float ColToneB(float hdrMax, float contrast, float shoulder, float midIn, float midOut) {  //
  return -((-pow(midIn, contrast) + (midOut * (pow(hdrMax, contrast * shoulder) * pow(midIn, contrast) - pow(hdrMax, contrast) * pow(midIn, contrast * shoulder) * midOut)) / (pow(hdrMax, contrast * shoulder) * midOut - pow(midIn, contrast * shoulder) * midOut)) / (pow(midIn, contrast * shoulder) * midOut));
}

// General tonemapping operator, build 'c' term.
float ColToneC(float hdrMax, float contrast, float shoulder, float midIn, float midOut) {  //
  return (pow(hdrMax, contrast * shoulder) * pow(midIn, contrast) - pow(hdrMax, contrast) * pow(midIn, contrast * shoulder) * midOut) / (pow(hdrMax, contrast * shoulder) * midOut - pow(midIn, contrast * shoulder) * midOut);
}

// General tonemapping operator, p := {contrast,shoulder,b,c}.
float ColTone(float x, vec4 p) {
  float z = pow(x, p.r);
  return z / (pow(z, p.g) * p.b + p.a);
}

vec3 AMDTonemapper(vec3 color) {
  const float hdrMax = 16.0;   // How much HDR range before clipping. HDR modes likely need this pushed up to say 25.0.
  const float contrast = 2.0;  // Use as a baseline to tune the amount of contrast the tonemapper has.
  const float shoulder = 1.0;  // Likely donÆt need to mess with this factor, unless matching existing tonemapper is not working well..
  const float midIn = 0.18;    // most games will have a {0.0 to 1.0} range for LDR so midIn should be 0.18.
  const float midOut = 0.18;   // Use for LDR. For HDR10 10:10:10:2 use maybe 0.18/25.0 to start. For scRGB, I forget what a good starting point is, need to re-calculate.

  float b = ColToneB(hdrMax, contrast, shoulder, midIn, midOut);
  float c = ColToneC(hdrMax, contrast, shoulder, midIn, midOut);

#define EPS 1e-6f
  float peak = max(color.r, max(color.g, color.b));
  peak = max(EPS, peak);

  vec3 ratio = color / peak;
  peak = ColTone(peak, vec4(contrast, shoulder, b, c));
  // then process ratio

  // probably want send these pre-computed (so send over saturation/crossSaturation as a constant)
  float crosstalk = 4.0;                    // controls amount of channel crosstalk
  float saturation = contrast;              // full tonal range saturation control
  float crossSaturation = contrast * 16.0;  // crosstalk saturation

  float white = 1.0;

  // wrap crosstalk in transform
  ratio = pow(abs(ratio), vec3(saturation / crossSaturation));
  ratio = mix(ratio, vec3(white), vec3(pow(peak, crosstalk)));
  ratio = pow(abs(ratio), vec3(crossSaturation));

  // then apply ratio to peak
  color = peak * ratio;
  return color;
}
#endif

vec3 doToneMapping(vec3 color) {
#if HDRToneMappingOperator == 1
  color = clamp(linear(color.xyz), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 2
  color = clamp(reinhard(color.xyz), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 3
  color = clamp(hejl(color.xyz), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 4
  color = clamp(hejl2015(color.xyz, 4.0), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 5
  color = clamp(ACESFilm(color.xyz), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 6
  float m = max(max(color.x, color.y), color.z);
  // color = clamp(pow(ACESFilm(vec3(color)) * (m / color.xyz), vec3(1.0 / 2.2)), vec3(0.0), vec3(1.0));
  color = clamp(pow(ACESFilm(vec3(m)) * (color.xyz / m), vec3(1.0 / 2.2)), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 7
  color = clamp(uncharted2(color.xyz), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 8
  color = clamp(uchimura(color.xyz), vec3(0.0), vec3(1.0));
#elif HDRToneMappingOperator == 9
  color = clamp(lottes(color.xyz), vec3(0.0), vec3(1.0));  
#elif HDRToneMappingOperator == 9
  color = clamp(AMDTonemapper(color.xyz), vec3(0.0), vec3(1.0));
#else
  color = clamp(color.xyz, vec3(0.0), vec3(1.0));
#endif
  return color;
}

void main() {
#if 1
  vec4 c = subpassLoad(uSubpassInput);
  outColor = vec4(doToneMapping(c.xyz), c.w);
#else
  outColor = vec4(1.0);
#endif
}