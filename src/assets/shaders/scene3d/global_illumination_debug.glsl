#ifndef GI_DEBUG_GLSL
#define GI_DEBUG_GLSL

// Global illumination / IBL / direct-light debug channel isolation.
//
// The renderer instance's CycleGlobalIlluminationDebugMode (bound to Ctrl+Shift+F) advances a per-GI-mode debug cycle. The
// "shading" positions of that cycle isolate a single lighting channel in the surface shaders (mesh + planet terrain / grass /
// water): the selected channel index is packed into the spare high bits of the per-pass push-constant flags word and read back
// here. The mesh pass uses drawFlags bits 28..31; the planet passes use their flags word bits 22..25 (the planet flags' top 6
// bits already carry the ray-traced soft-shadow sample count, so the planet range sits just below it). 0 = off (normal
// shading). These values must match the GlobalIlluminationDebugShadingMode codes on the Pascal side
// (PasVulkan.Scene3D.Renderer.Instance).

#define GI_DEBUG_DISPLAY_VALUE_MASK 15u

#define GI_DEBUG_DISPLAY_DRAWFLAGS_SHIFT 28u   // mesh pass: spare high bits of drawFlags
#define GI_DEBUG_DISPLAY_PLANETFLAGS_SHIFT 22u // planet passes: free bits below the soft-shadow sample count in flags

// The GI(probe) and IBL(env) indirect are now one crossfaded result, so the diffuse / specular channels are COMBINED (probe +
// env). The probe-vs-env blend is shown separately as a brightness-weighted heatmap (probe influence: 0 = env/sky, 1 = probe).
#define GI_DEBUG_DISPLAY_NONE              0u
#define GI_DEBUG_DISPLAY_DIRECT_LIGHT      1u
#define GI_DEBUG_DISPLAY_INDIRECT_DIFFUSE  2u
#define GI_DEBUG_DISPLAY_INDIRECT_SPECULAR 3u
#define GI_DEBUG_DISPLAY_PROBE_INFLUENCE   4u

#endif // GI_DEBUG_GLSL
