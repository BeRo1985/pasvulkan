#ifndef GI_DDGI_PUSHCONSTANTS_GLSL
#define GI_DDGI_PUSHCONSTANTS_GLSL

// Shared push-constant block for all DDGI compute passes — the trace PRODUCER and the per-stage update CORE (irradiance,
// visibility, border, relocation, classification). Must byte-match TPushConstants on the Pascal side
// (PasVulkan.Scene3D.Renderer.Passes.GlobalIlluminationDDGITraceComputePass / ...DDGIStageComputePass). Requires DDGIMaster
// to be declared already, so include gi_ddgi_master.glsl (and enable GL_EXT_buffer_reference) BEFORE this header.
//
// Not every stage uses every field (e.g. the border stage touches only params; blend's meaning varies, and ddgiMaster is
// unused by border) — but declaring the full block everywhere keeps the shader push layout identical to the pipeline's push
// range and lets all stages share one Pascal push record.
layout(push_constant) uniform PushConstants {
  vec4 randomRotation0;  // per-frame ray rotation, mat3 column 0 (xyz)
  vec4 randomRotation1;  // mat3 column 1 (xyz)
  vec4 randomRotation2;  // mat3 column 2 (xyz)
  uvec4 params;          // x = frameIndex, y = countCascades, z = probesPerCascade, w = raysPerProbe
  vec4 blend;            // x = temporal hysteresis, y = multi-bounce feedback strength (trace), z = firstFrame flag; exact use varies per pass
  DDGIMaster ddgiMaster; // BDA pointer to the DDGI master buffer (8 bytes, appended after blend); unused by the border stage
} pushConstants;

#endif // GI_DDGI_PUSHCONSTANTS_GLSL
