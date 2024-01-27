#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable
#extension GL_EXT_control_flow_attributes : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput;

//layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

const uint COLOR_SPACE_SRGB_NONLINEAR = 0;
const uint COLOR_SPACE_EXTENDED_SRGB_LINEAR = 1;
const uint COLOR_SPACE_HDR10_ST2084 = 2;

layout(push_constant) uniform PushConstants {
  uint colorSpace;
} pushConstants;

#include "rec2020.glsl"
#include "rec709.glsl"
#include "rec2084.glsl"
#include "srgb.glsl"

void main(){
  // Input format: 
  //   When using a sRGB texture: 
  //     VK_FORMAT_R8G8B8A8_SRGB or VK_FORMAT_B8G8R8A8_SRGB
  //   When using a linear sRGB texture:
  //     VK_FORMAT_R16G16B16A16_SFLOAT, VK_FORMAT_E5B9G9R9_UFLOAT_PACK32, VK_FORMAT_B10G11R11_UFLOAT_PACK32 or VK_FORMAT_R32G32B32A32_SFLOAT
  // But the input is always linear sRGB, either directly from a linear sRGB texture or from a sRGB texture, since the GPU automatically converts
  // the sRGB texture to linear sRGB through the hardware sampler.
  vec4 color = subpassLoad(uSubpassInput);
//vec4 Color = textureLod(uTexture, vec3(inTexCoord, float(gl_ViewIndex)), 0.0);
  switch(pushConstants.colorSpace){
    case COLOR_SPACE_EXTENDED_SRGB_LINEAR:{
      // Target format: VK_FORMAT_R16G16B16A16_SFLOAT
      // Just forward, no conversion, since the framebuffer is already in linear sRGB, but clip the lower values to 0.0, 
      // for avoiding negative values.
      outFragColor = max(vec4(0.0), color); 
      break;
    }
    case COLOR_SPACE_HDR10_ST2084:{
      // Target format: VK_FORMAT_A2B10G10R10_UNORM_PACK32 or VK_FORMAT_R16G16B16A16_SFLOAT (depends on the swapchain format) 
      const float referenceWhiteNits = 80.0;
      const float st2084max = 10000.0;
      const float hdrScalar = referenceWhiteNits / st2084max;      
      color.xyz = LinearSRGBToLinearRec2020Matrix * color.xyz; // convert to linear Rec. 2020
      color.xyz = convertToREC2084(color.xyz); // convert to ST. 2084
      outFragColor = clamp((color * vec2(hdrScalar, 1.0)).xxxy, vec4(0.0), vec4(1.0));
      break;
    } 
    default:{ //case COLOR_SPACE_SRGB_NONLINEAR:
      // Target format: VK_FORMAT_R8G8B8A8_SRGB or VK_FORMAT_B8G8R8A8_SRGB (depends on the swapchain format)
      // Just forward, no conversion, since the framebuffer is already in sRGB where the GPU transforms it already itself,
      // where the GPU converts the linear sRGB input to sRGB gamma corrected sRGB output through the hardware pipeline,
      // but clamp the values to the range [0.0, 1.0] for ensuring that these are in the output range.
      outFragColor = clamp(color, vec4(0.0), vec4(1.0)); 
      break;                
    }
  }
}
