#version 450 core
// videoexample present path A: composite the decoded FWV video (rgba, A = the decoded alpha plane) over a chosen
// background so the alpha channel is actually visible. Replaces the engine's plain ToScreenBlit frag (it keeps the
// same fullscreen-triangle vertex shader + binding 0 sampler). For an opaque (non-alpha) stream A=1 -> the video is
// shown unchanged, so this is a safe drop-in. mode 0 = checkerboard, 1 = solid colour; key 'G' toggles it live.
layout(location = 0) in vec2 inTexCoord;
layout(location = 0) out vec4 outColor;
layout(set = 0, binding = 0) uniform sampler2D uTexture;
layout(push_constant) uniform PushConstants {
  int mode;          // 0 = checkerboard, 1 = solid colour
  int premultiplied; // 1 = the video RGB is premultiplied by alpha (over = rgb + bg*(1-a))
  float checkerSize; // checkerboard cell size in pixels
  float pad;
  vec4 solidColour;  // background for mode 1
} push;
void main(){
  vec4 vid = textureLod(uTexture, inTexCoord, 0.0);
  vec3 bg;
  if (push.mode == 0) {
    ivec2 cell = ivec2(floor(gl_FragCoord.xy / max(push.checkerSize, 1.0)));
    bg = (((cell.x + cell.y) & 1) == 0) ? vec3(0.75) : vec3(0.45);
  } else {
    bg = push.solidColour.rgb;
  }
  vec3 rgb;
  if (push.premultiplied != 0) {
    rgb = vid.rgb + (bg * (1.0 - vid.a)); // premultiplied-alpha "over"
  } else {
    rgb = mix(bg, vid.rgb, vid.a);        // straight-alpha "over"
  }
  outColor = vec4(rgb, 1.0);
}
