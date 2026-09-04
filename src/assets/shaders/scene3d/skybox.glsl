#ifndef SKYBOX_GLSL
#define SKYBOX_GLSL

layout(push_constant) uniform PushConstants {

  vec4 currentOrientation;
  vec4 previousOrientation;
  
  vec4 lightDirection;

  uint viewBaseIndex;  //
  uint countViews;     //
  float skyBoxBrightnessFactor; //
  uint widthHeight;    // low 16 bits: width, high 16 bits: height

  uint mode;           // low 16 bits: 0: cube map, 1: realtime starlight, 2: color key, 3: gradient
                       // high 16 bits: the gradient sky's sun halo as 0..65535 (see sky_gradient_sun.glsl).
                       // It rides here rather than in a vec4 of its own because this block already sits
                       // exactly on the 128 byte push-constant limit, and the mode needs two bits of it.
  // Cached reprojection fields (always present, GLSL can truncate at pipeline layout level)
  uint countAllViews;  // Total view count, previous views stored at [viewBaseIndex + countAllViews]
  uint frameIndex;     // For stochastic refresh
  float skyBoxIntensityFactor;

  // Three vec4 whose meaning depends on the low half of mode above. This block sits exactly on the 128 byte
  // push-constant limit, so there is nothing to spare and what a mode does not need another one uses:
  //
  //   mode 3, the stylised gradient sky - its palette, with the sun packed into the w components:
  //     0: rgb = top colour,     w = star intensity
  //     1: rgb = horizon colour, w = the sun's angular radius in radians
  //     2: rgb = bottom colour,  w = the sun's brightness, in multiples of the palette
  //
  //   modes 0 and 1, the cube map and the real time starlight - the sun, and nothing else, in absolute
  //   luminances. All zero when the scene does not have the sky box draw the sun, which then draws nothing:
  //     0: rgb = radiance of the disc, w = its angular radius in radians, artistic scale already applied
  //     1: rgb = radiance of the aureole, w = the disc's edge softness
  //     2: x = limb darkening, y = aureole width, zw = unused
  //
  //   mode 2, the colour key: unused.
  vec4 skyParameters0;
  vec4 skyParameters1;
  vec4 skyParameters2;

} pushConstants;

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 0, binding = 0, std140) uniform uboViews {
   View views[256];
} uView;

#ifdef SKYBOX_CACHED_REPROJECTION

// History buffer from previous frame for reading
layout(set = 0, binding = 2) uniform sampler2DArray uHistoryTexture;

// History image for writing current frame result
#if defined(SKYBOX_CACHED_REPROJECTION_RGB9E5)
// R32_UINT alias for RGB9E5 encoding
layout(set = 0, binding = 3, r32ui) uniform writeonly uimage2DArray uHistoryImage;
#elif defined(SKYBOX_CACHED_REPROJECTION_RGBA16F)
// RGBA16F format
layout(set = 0, binding = 3, rgba16f) uniform writeonly image2DArray uHistoryImage;
#else
#error "SKYBOX_CACHED_REPROJECTION requires either SKYBOX_CACHED_REPROJECTION_RGB9E5 or SKYBOX_CACHED_REPROJECTION_RGBA16F"
#endif

#endif

#endif