#version 450 core

#extension GL_EXT_multiview : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInputOpaque;

layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput uSubpassInputTransparent;

layout(set = 0, binding = 2, rgba32ui) uniform coherent uimageBuffer uOITImgABuffer;

layout(set = 0, binding = 3, r32ui) uniform coherent uimageBuffer uOITImgZBuffer;

layout(std140, set = 0, binding = 4) uniform uboOIT {
  uvec4 oitViewPort;  //
} uOIT;
/* clang-format on */

void blend(inout vec4 target, const in vec4 source) {                  //
  target += (1.0 - target.a) * vec4(source.xyz * source.a, source.a);  //
}

#define MAX_MSAA 16
#define MAX_OIT_LAYERS 16

void main() {
  vec4 color = vec4(0.0);

#if 1
  uvec4 oitFragments[MAX_OIT_LAYERS];

  int oitMultiViewIndex = int(gl_ViewIndex);
  ivec3 oitCoord = ivec3(ivec2(gl_FragCoord.xy), oitMultiViewIndex);

  const int oitViewSize = int(uOIT.oitViewPort.z);
  const int oitCountLayers = int(uOIT.oitViewPort.w & 0xffffu);
  const int oitMultiViewSize = oitViewSize * oitCountLayers;
  const int oitBufferBaseIndex = (((oitCoord.y * int(uOIT.oitViewPort.x)) + oitCoord.x) * oitCountLayers) + (oitMultiViewSize * oitMultiViewIndex);

  int oitCountFragments = 0;
  while(oitCountFragments < oitCountLayers){
    if( 
#ifdef REVERSEDZ
      imageLoad(uOITImgZBuffer, oitBufferBaseIndex + oitCountFragments).x != 0x00000000u
#else
      imageLoad(uOITImgZBuffer, oitBufferBaseIndex + oitCountFragments).x != 0xffffffffu
#endif    
      ){
     break; 
    }else{
      oitCountFragments++;
    }
  }

  if (oitCountFragments > 0) {
    for (int oitFragmentIndex = 0; oitFragmentIndex < oitCountFragments; oitFragmentIndex++) {            //
      oitFragments[oitFragmentIndex] = imageLoad(uOITImgABuffer, oitBufferBaseIndex + oitFragmentIndex);  //
    }

#ifdef MSAA
    const int oitMSAA = clamp(int(uOIT.oitViewPort.w >> 16), 1, MAX_MSAA);

#if 1
    vec4 oitMSAAColors[MAX_MSAA];
    for (int oitMSAASampleIndex = 0; oitMSAASampleIndex < oitMSAA; oitMSAASampleIndex++) {  //
      oitMSAAColors[oitMSAASampleIndex] = vec4(0.0);                                        //
    }
    for (int oitFragmentIndex = 0; oitFragmentIndex < oitCountFragments; oitFragmentIndex++) {          //
      if (oitFragments[oitFragmentIndex].w != 0) {                                                      //
        uvec4 fragment = oitFragments[oitFragmentIndex];                                                //
        vec4 fragmentColor = vec4(vec2(unpackHalf2x16(fragment.x)), vec2(unpackHalf2x16(fragment.y)));  //
        for (int oitMSAASampleIndex = 0; oitMSAASampleIndex < oitMSAA; oitMSAASampleIndex++) {          //
          if ((fragment.w & (1u << oitMSAASampleIndex)) != 0) {                                         //
            blend(oitMSAAColors[oitMSAASampleIndex], fragmentColor);                                    //
          }
        }
      }
    }
    for (int oitMSAASampleIndex = 0; oitMSAASampleIndex < oitMSAA; oitMSAASampleIndex++) {  //
      color += oitMSAAColors[oitMSAASampleIndex];                                           //
    }
    color /= oitMSAA;
#else
    for (int oitMSAASampleIndex = 0; oitMSAASampleIndex < oitMSAA; oitMSAASampleIndex++) {
      vec4 sampleColor = vec4(0.0);
      for (int oitFragmentIndex = 0; oitFragmentIndex < oitCountFragments; oitFragmentIndex++) {          //
        if ((oitFragments[oitFragmentIndex].w & (1u << oitMSAASampleIndex)) != 0) {                       //
          uvec4 fragment = oitFragments[oitFragmentIndex];                                                //
          vec4 fragmentColor = vec4(vec2(unpackHalf2x16(fragment.x)), vec2(unpackHalf2x16(fragment.y)));  //
          blend(sampleColor, fragmentColor);                                                              //
        }
      }
      color += sampleColor;
    }
    color /= oitMSAA;
#endif
#else
    for (int oitFragmentIndex = 0; oitFragmentIndex < oitCountFragments; oitFragmentIndex++) {        //
      uvec4 fragment = oitFragments[oitFragmentIndex];                                                //
      vec4 fragmentColor = vec4(vec2(unpackHalf2x16(fragment.x)), vec2(unpackHalf2x16(fragment.y)));  //
      blend(color, fragmentColor);                                                                    //
    }
#endif
  }

#endif

  blend(color, subpassLoad(uSubpassInputTransparent));

  blend(color, subpassLoad(uSubpassInputOpaque));

  outColor = color;
}