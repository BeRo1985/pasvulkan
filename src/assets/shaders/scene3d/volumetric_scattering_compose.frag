#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

// Volumetric scattering, step three, in the form that runs BEFORE the multisample resolve.
//
// The compute variant beside this one does the same arithmetic on the resolved picture, and at a silhouette
// it cannot be right. A resolved pixel's colour is a coverage mix of two surfaces at two different
// distances, and its resolved depth names only one of them - the NEAREST, deliberately, because "is any
// part of this pixel in front" is the right question for an occlusion test. Whatever single answer the air
// then gives is applied to a colour that is already a blend of two, and
//
//   avg(T) * avg(C)  is not  avg(T * C)
//
// The difference is the coverage-weighted product of the two transmittances against the two colours, which
// is largest exactly where sky meets geometry - a rim along every outline, thin but everywhere, and not
// removable by any amount of care taken over the depth used. It is not an artefact of the half resolution
// or of the upsample: it is the resolve happening before the air is applied instead of after.
//
// So this variant is put before the resolve, on the multisampled colour, with per-sample shading turned on.
// Each invocation is one SAMPLE: it reads that sample's colour through a multisampled input attachment, its
// depth out of the raw multisampled depth, and writes that sample alone. The resolve afterwards averages
// finished pixels, which is what a resolve is for. No averaging is done here, and none is needed - there is
// nothing to average, which is the whole point.
//
// The price is real: the pass runs at sample rate, so four times the work at 4x MSAA, and it must run
// before the resolve, which means the fog beside it has to move there too or the two would be applied to
// two different pictures. Hence a create-time option rather than the default.

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

// The picture so far, still multisampled, as an input attachment - the pass reads and writes the same
// attachment at the same pixel, which is what input attachments are for and the only legal way to do it.
// Loaded per sample, because subpassLoad without a sample index is not defined for a multisampled one.
layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInputMS uSubpassColor;

// The RAW multisampled depth, before the resolve reduced it to one sample per pixel. Only this invocation's
// own sample is read from it.
layout(set = 0, binding = 6) uniform sampler2DMSArray uTextureMSAADepth;

// The half-resolution buffers, the switches, and the upsample itself - shared with the compute variant of
// this same pass, so the two cannot drift apart.
#include "volumetric_scattering_compose.glsl"

void main(){

  ivec3 at = ivec3(ivec2(gl_FragCoord.xy), gl_ViewIndex);

  vec4 here = subpassLoad(uSubpassColor, gl_SampleID);

  if((pushConstants.flagsSampleCountSpare.x & VolumetricScatteringComposeFlagEnabled) == 0u){
    outFragColor = here;
    return;
  }

  // The full-resolution size comes from the depth image rather than from a push constant, so that it is by
  // construction the size the coordinates above are in.
  ivec3 destinationSize = textureSize(uTextureMSAADepth);

  // The four half-resolution texels around this pixel. Shared by every sample of the pixel, but worked out
  // per invocation here - the samples of one pixel are separate invocations and cannot share anything.
  ivec3 scatteringSize;
  ivec2 scatteringBase;
  vec2 scatteringFraction;
  scatteringFootprint(at.xy, destinationSize.xy, scatteringSize, scatteringBase, scatteringFraction);

  bool legacyLook = (pushConstants.flagsSampleCountSpare.x & VolumetricScatteringComposeFlagLegacyLook) != 0u;

  // This sample's own depth, and nothing else. A sky sample gets the sky's distance out of linearDepth by
  // the explicit test there, and then honestly receives the air over that whole distance - which the
  // resolved variant cannot give it, because by then the sky sample no longer exists as a thing of its own.
  vec3 inscattering;
  vec3 extinction;
  resolveScattering(at,
                    scatteringSize,
                    scatteringBase,
                    scatteringFraction,
                    linearDepth(texelFetch(uTextureMSAADepth, at, gl_SampleID).x),
                    legacyLook,
                    inscattering,
                    extinction);

  outFragColor = composeScattering(here, inscattering, extinction);

}
