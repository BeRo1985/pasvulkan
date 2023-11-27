#ifndef TEXTUREUTILS_GLSL
#define TEXTUREUTILS_GLSL

vec4 textureBicubicCoefficents(const in float v){
  vec4 n = vec4(1.0, 2.0, 3.0, 4.0) - v, s = n * n * n;
  vec3 t = vec3(s.x, s.y - (4.0 * s.x), (s.z - (4.0 * s.y)) + (6.0 * s.x));
  return vec4(t, 6.0 - dot(t, vec3(1.0))) * (1.0 / 6.0);
}

vec4 textureBicubic(const in sampler2D tex, const in vec2 texCoords, const in int lod){
  vec2 texSize = textureSize(tex, lod),
       uv = (texCoords * texSize) - vec2(0.5),
       fxy = fract(uv);
  vec4 xcubic = textureBicubicCoefficents(fxy.x),
       ycubic = textureBicubicCoefficents(fxy.y),
       s = vec4(xcubic.xz + xcubic.yw, ycubic.xz + ycubic.yw),
       offset = (((uv - fxy).xxyy + vec2(-0.5, +1.5).xyxy) + 
                 (vec4(xcubic.yw, ycubic.yw) / s)) * 
                (vec2(1.0) / texSize).xxyy;
  vec3 f = vec3(s.x / (s.x + s.y), s.z / (s.z + s.w), float(lod));
  return mix(mix(textureLod(tex, offset.yw, f.z), textureLod(tex, offset.xw, f.z), f.x), 
             mix(textureLod(tex, offset.yz, f.z), textureLod(tex, offset.xz, f.z), f.x), f.y);
}           

vec4 textureCatmullRomCoefficents(const in float v){
  float t = v, tt = t * t, ttt = tt * t;
  return vec4((tt - (ttt * 0.5)) - (0.5 * t), ((ttt * 1.5) - (tt * 2.5)) + 1.0, ((tt * 2.0) - (ttt * 1.5)) + (t * 0.5), (ttt * 0.5) - (tt * 0.5));  
}

// based on: https://www.decarpentier.nl/2d-catmull-rom-in-4-samples
vec4 textureCatmullRom(const in sampler2D tex, const in vec2 uv, const in int lod){
  vec2 texSize = textureSize(tex, lod);
  vec2 h = fma(fract(fma(uv, texSize * 0.5, vec2(-0.25))), vec2(2.0), vec2(-1.0));
  vec2 f = fract(h);
  vec2 s1 = fma(f, vec2(0.5), vec2(-0.5)) * f;
  vec2 s12 = fma(f, fma(f, vec2(-2.0), vec2(1.5)), vec2(1.0));
  vec2 s34 = fma(f, fma(f, vec2(2.0), vec2(-2.5)), vec2(-0.5));
  vec4 p = vec4((s1 - (f * s12)) / (texSize * s12), ((s1 + s34) - (f * s34)) / (texSize * s34)) + uv.xyxy;
  float s = ((h.x * h.y) > 0.0) ? 1.0 : -1.0;
  vec4 w  = vec4(s12 - (f * s12), s34 * f);
  w = vec4(w.xz * (w.y * s), w.xz * (w.w * s));
  return (textureLod(tex, p.xy, float(lod)) * w.x) + (textureLod(tex, p.zy, float(lod)) * w.y) +
         (textureLod(tex, p.xw, float(lod)) * w.z) + (textureLod(tex, p.zw, float(lod)) * w.w);
}

vec4 textureTriplanar(const in sampler2D t, const in vec3 p, const in vec3 n, const in float k, const in vec3 gx, const in vec3 gy){
//vec2 r = textureSize(t, 0);
  vec3 m = pow(abs(n), vec3(k));
  return ((textureGrad(t, p.yz, gx.yz, gy.yz) * m.x) + 
          (textureGrad(t, p.zx, gx.zx, gy.zx) * m.y) + 
          (textureGrad(t, p.xy, gx.xy, gy.xy) * m.z)) / (m.x + m.y + m.z);
}           

#endif
