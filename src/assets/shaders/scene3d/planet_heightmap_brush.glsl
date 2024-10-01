#ifndef PLANET_HEIGHTMAP_BRUSH_GLSL
#define PLANET_HEIGHTMAP_BRUSH_GLSL

float getBrushTexelValue(const in sampler2DArray tex, int index, float angle, float dist){
  return texture(tex, vec3(fma(vec2(sin(vec2(angle * 3.141592653589793) + vec2(0.0, 1.5707963267948966))), vec2(clamp(dist, 0.0, 1.0) * 0.5), vec2(0.5)), float(index))).x;
}

#endif