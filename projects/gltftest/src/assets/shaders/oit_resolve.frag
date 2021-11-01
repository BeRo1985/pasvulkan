#version 450 core

#extension GL_EXT_multiview : enable

/* clang-format off */
layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInputOpaque;

layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput uSubpassInputTransparent;

layout(set = 0, binding = 2, rgba32ui) uniform coherent uimageBuffer uOITImgABuffer;

layout(set = 0, binding = 3, r32ui) uniform coherent uimage2DArray uOITImgAux;

layout(std140, set = 0, binding = 4) uniform uboOIT {
  uvec4 oitViewPort;  //
} uOIT;
/* clang-format on */

void blend(inout vec4 target, const in vec4 source) {  //
  target += (1.0 - target.a) * source;                 //
}

#define MAX_OIT_LAYERS 8

void sort(inout uvec4 array[MAX_OIT_LAYERS], int count) {
#if MAX_OIT_LAYERS > 1
#if 1
  for (int i = (count - 2); i >= 0; --i) {
    for (int j = 0; j <= i; ++j) {
      if (
#ifdef REVERSEDZ
          (uintBitsToFloat(array[j].z) <= uintBitsToFloat(array[j + 1].z))
#else
          (uintBitsToFloat(array[j].z) >= uintBitsToFloat(array[j + 1].z))
#endif
      ) {
        uvec4 temp = array[j + 1];
        array[j + 1] = array[j];
        array[j] = temp;
      }
    }
  }
#else
  for (int i = 0, j = count - 1; i < j;) {
    if (
#ifdef REVERSEDZ
        (uintBitsToFloat(array[i].z) <= uintBitsToFloat(array[i + 1].z))
#else
        (uintBitsToFloat(array[i].z) >= uintBitsToFloat(array[i + 1].z))
#endif
    ) {
      uvec4 temp = array[i + 1];
      array[i + 1] = array[i];
      array[i] = temp;
      i += (i > 0) ? -1 : 1;
    } else {
      i++;
    }
  }
#endif
#endif  // #if OIT_LAYERS > 1
}

void main() {
  vec4 color = vec4(0.0);

#if 1
  uvec4 oitFragments[MAX_OIT_LAYERS];

  int oitMultiViewIndex = int(gl_ViewIndex);
  ivec3 oitCoord = ivec3(ivec2(gl_FragCoord.xy), oitMultiViewIndex);

  const int oitViewSize = int(uOIT.oitViewPort.z);
  const int oitCountLayers = int(uOIT.oitViewPort.w & 0xffffu);
  const int oitMultiViewSize = oitViewSize * oitCountLayers;
  const int oitABufferBaseIndex = ((oitCoord.y * int(uOIT.oitViewPort.x)) + oitCoord.x) + (oitMultiViewSize * oitMultiViewIndex);

  const int oitCountFragments = min(MAX_OIT_LAYERS, min(oitCountLayers, int(imageLoad(uOITImgAux, oitCoord).r)));

  if (oitCountFragments > 0) {
    
    for (int oitFragmentIndex = 0; oitFragmentIndex < oitCountFragments; oitFragmentIndex++) {                             //
      oitFragments[oitFragmentIndex] = imageLoad(uOITImgABuffer, oitABufferBaseIndex + (oitFragmentIndex * oitViewSize));  //
    }

    sort(oitFragments, oitCountFragments);

#ifdef MSAA
    const int oitMSAA = clamp(int(uOIT.oitViewPort.w >> 16), 1, 16);

    for (int oitMSAASampleIndex = 0; oitMSAASampleIndex < oitMSAA; oitMSAASampleIndex++) {
      vec4 sampleColor = vec4(0.0);
      for (int oitFragmentIndex = 0; oitFragmentIndex < oitCountFragments; oitFragmentIndex++) {          //
        if ((oitFragments[oitFragmentIndex].w & (1 << oitMSAASampleIndex)) != 0) {                        //
          uvec4 fragment = oitFragments[oitFragmentIndex];                                                //
          vec4 fragmentColor = vec4(vec2(unpackHalf2x16(fragment.x)), vec2(unpackHalf2x16(fragment.y)));  //
          blend(sampleColor, fragmentColor);                                                              //
        }
      }
      color += sampleColor;
    }
    color /= oitMSAA;
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