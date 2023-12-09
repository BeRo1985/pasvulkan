#ifndef TETRAHEDRAL_GLSL
#define TETRAHEDRAL_GLSL

vec3 tetrahedralDecode(vec2 uv){
 	return vec3(
    fma((uv.x < 0.5) ? uv.x : (1.0 - uv.x), 4.0, -1.0),
    fma(vec2(uv.y, abs(1.0 - abs(1.0 - fma(uv.x, 2.0, -uv.y)))), vec2(2.0), vec2(-1.0))
  );
}

vec2 tetrahedralEncode(vec3 uvw){
  const vec2 v = vec2(-0.5, 0.5);
  vec4 d = vec4(dot(uvw, v.yyy), dot(uvw, v.xxy), dot(uvw, v.yxx), dot(uvw, v.xyx)) / (dot(uvw.xyz, uvw.xyz) * 0.5);  
  uvw /= max(d.x, max(d.y, max(d.z, d.w)));
 	return vec2(
    fma(uvw.x, ((all(greaterThan(uvw.xy + uvw.yz, vec2(0.0))) && ((-uvw.z) - uvw.x < 0.0)) || (((uvw.x + uvw.y) < 0.0) && all(greaterThan(uvw.zz - uvw.yx, vec2(0.0))))) ? -0.25 : 0.25, 0.25),
    fma(uvw.y, 0.5, 0.5)
  );
}

#endif