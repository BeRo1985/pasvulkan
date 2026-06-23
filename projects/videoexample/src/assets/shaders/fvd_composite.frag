#version 450 core
// videoexample present path A: composite the decoded FVD video (rgba, A = the decoded alpha plane) over a chosen
// background so the alpha channel is actually visible. Replaces the engine's plain ToScreenBlit frag (it keeps the
// same fullscreen-triangle vertex shader + binding 0 sampler). For an opaque (non-alpha) stream A=1 -> the video is
// shown unchanged, so this is a safe drop-in. mode 0 = checkerboard, 1 = solid color; key 'G' toggles it live.
layout(location = 0) in vec2 inTexCoord;
layout(location = 0) out vec4 outColor;
layout(set = 0, binding = 0) uniform sampler2D uTexture;
layout(push_constant) uniform PushConstants {
  int mode;          // 0 = checkerboard, 1 = solid color
  int premultiplied; // 1 = the video RGB is premultiplied by alpha (over = rgb + bg*(1-a))
  float checkerSize; // checkerboard cell size in pixels
  float pad;
  vec4 solidColor;  // background for mode 1
} push;
void main(){
  vec3 bg;
  if (push.mode == 0) {
    ivec2 cell = ivec2(floor(gl_FragCoord.xy / max(push.checkerSize, 1.0)));
    bg = (((cell.x + cell.y) & 1) == 0) ? vec3(0.75) : vec3(0.45);
  } else {
    bg = push.solidColor.rgb;
  }
  // The decoded frame is STRAIGHT alpha (RGB not premultiplied) unless the stream flags premultiplied content. Plain
  // bilinear filtering of straight alpha leaks the transparent-area RGB across alpha edges (a color fringe): invisible
  // for SDR (RGB ~[0..1]) but for HDR/scRGB the transparent RGB is huge (scRGB-linear >> 1) -> bright halos. So filter
  // the straight case with PREMULTIPLIED bilinear (premultiply each of the 4 taps BEFORE blending); already-premultiplied
  // content filters correctly with plain bilinear. After filtering, both branches hold a premultiplied (rgb*a, a) value.
  vec4 vid;
  if (push.premultiplied != 0) {
    vid = textureLod(uTexture, inTexCoord, 0.0);
  } else {
    vec2 texSize = vec2(textureSize(uTexture, 0));
    vec2 st = (inTexCoord * texSize) - vec2(0.5);
    vec2 f = fract(st);
    ivec2 base = ivec2(floor(st));
    ivec2 maxXY = ivec2(texSize) - ivec2(1);
    vec4 t00 = texelFetch(uTexture, clamp(base + ivec2(0, 0), ivec2(0), maxXY), 0);
    vec4 t10 = texelFetch(uTexture, clamp(base + ivec2(1, 0), ivec2(0), maxXY), 0);
    vec4 t01 = texelFetch(uTexture, clamp(base + ivec2(0, 1), ivec2(0), maxXY), 0);
    vec4 t11 = texelFetch(uTexture, clamp(base + ivec2(1, 1), ivec2(0), maxXY), 0);
    t00.rgb *= t00.a;
    t10.rgb *= t10.a;
    t01.rgb *= t01.a;
    t11.rgb *= t11.a;
    vid = mix(mix(t00, t10, f.x), mix(t01, t11, f.x), f.y);
  }
  // premultiplied-alpha "over" (vid.rgb is premultiplied by alpha in both branches)
  vec3 rgb = vid.rgb + (bg * (1.0 - vid.a));
  outColor = vec4(rgb, 1.0);
}
