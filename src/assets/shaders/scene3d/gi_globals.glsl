#ifndef GI_GLOBALS_GLSL
#define GI_GLOBALS_GLSL

// Shared global-illumination shading constants, available to ALL GI modes (mesh + planet terrain / grass / water surface
// shaders), independent of which GI technique is compiled. Kept here — rather than in a technique-specific include such as
// global_illumination_dugi.glsl — so the cascaded radiance hints and voxel cone tracing paths use the same roughness bands as
// DUGI. All #ifndef-guarded so a build can override them.

// Shading-time roughness band for blending the sharp glossy atlas against the broad source: at/below LO take the sharp
// atlas, at/above HI take the broad source (the atlas prefilter sharpness ~ roughness HI, beyond which the broad source
// is already correct). Only used when GI_DUGI_GLOSSY_RADIANCE.
#ifndef GI_GLOSSY_ROUGHNESS_LO
  #define GI_GLOSSY_ROUGHNESS_LO 0.0
#endif
#ifndef GI_GLOSSY_ROUGHNESS_HI
  #define GI_GLOSSY_ROUGHNESS_HI 0.45
#endif

// Roughness gate for the OCT shading path's env/glossy specular reflection: full at/below LO (glossy surfaces reflect the
// sky/environment correctly), faded to zero at/above HI (matte surfaces barely reflect, so they do not pick up the sky colour
// through the coarse environment cubemap / probe glossy). Tunable; raise HI if too much reflection is lost on semi-glossy
// surfaces, lower it if matte surfaces still show the sky.
#ifndef GI_SPECULAR_ROUGHNESS_LO
  #define GI_SPECULAR_ROUGHNESS_LO 0.1
#endif
#ifndef GI_SPECULAR_ROUGHNESS_HI
  #define GI_SPECULAR_ROUGHNESS_HI 0.35
#endif

#endif // GI_GLOBALS_GLSL
