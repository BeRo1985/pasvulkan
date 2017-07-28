#version 450 core

layout (location = 0) in vec3 inUV;
layout (location = 1) in vec4 inBackgroundColor;
layout (location = 2) in vec4 inForegroundColor;

layout (binding = 0) uniform UBO {
	float uThreshold;
} ubo;

layout (binding = 1) uniform sampler2DArray uSamplerFont;

layout (location = 0) out vec4 outFragColor;

// Define our own linearstep function for to map distance coverage, when we have sRGB output. 
// Smoothstep's nonlinear response is actually doing some fake-gamma, so it ends up over-correcting when the output is already gamma-correct.
#define TEMPLATE_LINEARSTEP(DATATYPE) \
  DATATYPE linearstep(DATATYPE edge0, DATATYPE edge1, DATATYPE value){ \
    return clamp((value - edge0) / (edge1 - edge0), DATATYPE(0.0), DATATYPE(1.0)); \
  }
TEMPLATE_LINEARSTEP(float)  
TEMPLATE_LINEARSTEP(vec4)  

// SDF with 5 point tap tent filter-based supersampling

const float HALF_BY_SQRT_TWO = 0.5 / sqrt(2.0),
            ONE_BY_THREE = 1.0 / 3.0;
 
void main(void){
  float center = textureLod(uSamplerFont, inUV, 0.0).r,
        width = abs(dFdx(center)) + abs(dFdy(center)); // ubo.uThreshold * 0.25;
  vec4 buv = inUV.xyxy + (vec2((dFdx(inUV.xy) + dFdy(inUV.xy)) * HALF_BY_SQRT_TWO).xyxy * vec2(-1.0, 1.0).xxyy);
  outFragColor = mix(inBackgroundColor, 
                     inForegroundColor, 
                     clamp((linearstep(width, -width, center - 0.5) + 
                            dot(linearstep(vec4(width), 
                                           vec4(-width), 
                                           vec4(textureLod(uSamplerFont, vec3(buv.xy, inUV.z), 0.0).r,
                                                textureLod(uSamplerFont, vec3(buv.zw, inUV.z), 0.0).r,
                                                textureLod(uSamplerFont, vec3(buv.xw, inUV.z), 0.0).r,
                                                textureLod(uSamplerFont, vec3(buv.zy, inUV.z), 0.0).r) - vec4(0.5)), vec4(0.5))) * ONE_BY_THREE, 0.0, 1.0));
}
