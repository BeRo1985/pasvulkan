#version 450 core

#extension GL_EXT_multiview : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput;

#define HDRToneMappingOperator 5

#if HDRToneMappingOperator == 1             
vec3 linear(const in vec3 color){
  return color;
} 
#elif HDRToneMappingOperator == 2              
vec3 reinhard(in vec3 color){
  color *= 1.5;
  return color / (vec3(1.0) + color);
} 
#elif HDRToneMappingOperator == 3
vec3 hejl(const in vec3 color) {
  vec3 x = max(vec3(0.0), color - vec3(0.004));
  return pow((x * ((6.2 * x) + vec3(0.5))) / max(x * ((6.2 * x) + vec3(1.7)) + vec3(0.06), vec3(1e-8)), vec3(2.2));
}              
#elif HDRToneMappingOperator == 4
vec3 hejl2015(vec3 c, float w){
  vec4 h = vec4(c, w),
       a = (1.425 * h) + vec4(0.05),
       f = (((h * a) + vec4(0.004)) / ((h * (a + vec4(0.55)) + vec4(0.0491)))) - vec4(0.0821);
  return f.xyz / f.w; 
}
#elif HDRToneMappingOperator == 5
vec3 ACESFilm(const in vec3 x){
  const float a = 2.51, b = 0.03, c = 2.43, d = 0.59, e = 0.14;
  return clamp((x * ((a * x) + vec3(b))) / (x * ((c * x) + vec3(d)) + vec3(e)), vec3(0.0), vec3(1.0)); 
}
#elif HDRToneMappingOperator == 6
vec3 ACESFilm(const in vec3 x){
  const float a = 2.51, b = 0.03, c = 2.43, d = 0.59, e = 0.14;
  return pow(clamp((x * ((a * x) + vec3(b))) / (x * ((c * x) + vec3(d)) + vec3(e)), vec3(0.0), vec3(1.0)), vec3(2.2)); 
}
#elif HDRToneMappingOperator == 7
vec3 uncharted2(in vec3 color){
  float A = 0.15;
  float B = 0.50;
  float C = 0.10;
  float D = 0.20;
  float E = 0.02;
  float F = 0.30;
  float W = 11.2;
  float IW = 1.0 / (((W * ((A * W) + (C * B))+ (D * E)) / (W * ((A*W) + B) + (D * F))) - (E / F));  
  color *= 5.0;
  return (((color * ((A * color) + vec3(C*B)) + vec3(D*E)) / (color * ((A * color) + vec3(B)) + vec3(D * F))) - vec3(E / F)) * IW;
}
#endif

vec3 doToneMapping(vec3 color){
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
  float m = max(max(color.x, 
                    color.y), 
                color.z);
//color = clamp(pow(ACESFilm(vec3(color)) * (m / color.xyz), vec3(1.0 / 2.2)), vec3(0.0), vec3(1.0));  
  color = clamp(pow(ACESFilm(vec3(m)) * (color.xyz / m), vec3(1.0 / 2.2)), vec3(0.0), vec3(1.0));  
#elif HDRToneMappingOperator == 7
  color = clamp(uncharted2(color.xyz), vec3(0.0), vec3(1.0));  
#else
  color = clamp(color.xyz, vec3(0.0), vec3(1.0));  
#endif              
  return color;
}

void main(){
#if 1
  vec4 c = subpassLoad(uSubpassInput);  
  outColor = vec4(doToneMapping(c.xyz), c.w);
#else
  outColor = vec4(1.0);
#endif    
}