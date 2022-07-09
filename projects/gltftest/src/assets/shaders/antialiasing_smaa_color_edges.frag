#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(location = 0) in vec2 inTexCoord;
layout(location = 1) in vec4 inOffset0;
layout(location = 2) in vec4 inOffset1;
layout(location = 3) in vec4 inOffset2;

layout(location = 0) out vec2 outFragOutput;

layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

void main() {
  const float SMAA_THRESHOLD = 0.1;
  const float SMAA_LOCAL_CONTRAST_ADAPTATION_FACTOR = 2.0;
  vec2 threshold = vec2(SMAA_THRESHOLD);
  vec4 delta;
  vec3 c = textureLod(uTexture, vec3(inTexCoord, float(gl_ViewIndex)), 0).xyz;
  vec3 t = abs(c - textureLod(uTexture, vec3(inOffset0.xy, float(gl_ViewIndex)), 0).xyz);
  delta.x = max(max(t.r, t.g), t.b);
  t = abs(c - textureLod(uTexture, vec3(inOffset0.zw, float(gl_ViewIndex)), 0).xyz);
  delta.y = max(max(t.r, t.g), t.b);
  vec2 edges = step(threshold, delta.xy);
  if (dot(edges, vec2(1.0, 1.0)) == 0.0) {
    discard;
  } else {
    t = abs(c - textureLod(uTexture, vec3(inOffset1.xy, float(gl_ViewIndex)), 0).xyz);
    delta.z = max(max(t.r, t.g), t.b);
    t = abs(c - textureLod(uTexture, vec3(inOffset1.zw, float(gl_ViewIndex)), 0).xyz);
    delta.w = max(max(t.r, t.g), t.b);
    vec2 maxDelta = max(delta.xy, delta.zw);
    t = abs(c - textureLod(uTexture, vec3(inOffset2.xy, float(gl_ViewIndex)), 0).xyz);
    delta.z = max(max(t.r, t.g), t.b);
    t = abs(c - textureLod(uTexture, vec3(inOffset2.zw, float(gl_ViewIndex)), 0).xyz);
    delta.w = max(max(t.r, t.g), t.b);
    maxDelta = max(maxDelta.xy, delta.zw);
    float finalDelta = max(maxDelta.x, maxDelta.y);
    edges.xy *= step(finalDelta, SMAA_LOCAL_CONTRAST_ADAPTATION_FACTOR * delta.xy);
    outFragOutput = edges;
  }
}
