#ifndef ENV_STARLIGHT_GLSL
#define ENV_STARLIGHT_GLSL

vec3 starlightHash33(uvec3 q){
  q *= uvec3(1597334673u, 3812015801u, 2798796415u);
  q ^= ((q.yzx + uvec3(1013904223u, 826366247u, 3014898611u)) * uvec3(1597334673u, 3812015801u, 2798796415u));
  q ^= ((q.zxy + uvec3(1013904223u, 826366247u, 3014898611u)) * uvec3(1597334673u, 3812015801u, 2798796415u));
  q = (q.x ^ q.y ^ q.z) * uvec3(1597334673u, 3812015801u, 2798796415u);
  return vec3(vec3(uintBitsToFloat(uvec3(uvec3(((q >> 9u) & uvec3(0x007fffffu)) | uvec3(0x3f800000u))))) - vec3(1.0));
}

vec3 starlightHash33(ivec3 p){
  uvec3 q = uvec3(p) * uvec3(1597334673u, 3812015801u, 2798796415u);
  q ^= ((q.yzx + uvec3(1013904223u, 826366247u, 3014898611u)) * uvec3(1597334673u, 3812015801u, 2798796415u));
  q ^= ((q.zxy + uvec3(1013904223u, 826366247u, 3014898611u)) * uvec3(1597334673u, 3812015801u, 2798796415u));
  q = (q.x ^ q.y ^ q.z) * uvec3(1597334673u, 3812015801u, 2798796415u);
  return vec3(vec3(uintBitsToFloat(uvec3(uvec3(((q >> 9u) & uvec3(0x007fffffu)) | uvec3(0x3f800000u))))) - vec3(1.0));
}

vec3 starlightHash33(vec3 p){
  uvec3 q = uvec3(ivec3(p)) * uvec3(1597334673u, 3812015801u, 2798796415u);
  q ^= ((q.yzx + uvec3(1013904223u, 826366247u, 3014898611u)) * uvec3(1597334673u, 3812015801u, 2798796415u));
  q ^= ((q.zxy + uvec3(1013904223u, 826366247u, 3014898611u)) * uvec3(1597334673u, 3812015801u, 2798796415u));
  q = (q.x ^ q.y ^ q.z) * uvec3(1597334673u, 3812015801u, 2798796415u);
  return vec3(vec3(uintBitsToFloat(uvec3(uvec3(((q >> 9u) & uvec3(0x007fffffu)) | uvec3(0x3f800000u))))) - vec3(1.0));
}


vec3 starlightNoise(in vec3 p){
  ivec3 i = ivec3(floor(p));  
  p -= vec3(i);
  vec3 w = (p * p) * (3.0 - (2.0 * p));
  ivec2 o = ivec2(0, 1);
  return mix(mix(mix(starlightHash33(i + o.xxx), starlightHash33(i + o.yxx), w.x),
                 mix(starlightHash33(i + o.xyx), starlightHash33(i + o.yyx), w.x), w.y),
             mix(mix(starlightHash33(i + o.xxy), starlightHash33(i + o.yxy), w.x),
                 mix(starlightHash33(i + o.xyy), starlightHash33(i + o.yyy), w.x), w.y), w.z);                     
}

vec2 starlightVoronoi(in vec3 x){
  x += vec3(1.0); 
  vec3 n = floor(x);
  vec3 f = x - n;
  vec4 m = vec4(uintBitsToFloat(0x7f800000u));
  for(int k = -1; k <= 1; k++){
    for(int j = -1; j <= 1; j++){
      for(int i = -1; i <= 1; i++){
        vec3 g = vec3(ivec3(i, j, k));  
        vec3 o = starlightHash33(n + g);
        vec3 r = (g - f) + o;
	      float d = dot(r, r);
        m = (d < m.w) ? vec4(o, d) : m;
      }  
    }
  }
  return vec2(sqrt(m.w), dot(m.xyz, vec3(1.0)));
}

vec3 getStarlight(
#ifdef COMPUTE_DERIVATIVES
                  vec3 direction,
                  vec3 directionX, 
                  vec3 directionY
#else
                  vec3 direction                  
#endif
                 ){
  const float w = 64.0;  
  vec3 d = normalize(direction) * w;
  vec2 v = starlightVoronoi(d);
#ifdef COMPUTE_DERIVATIVES
  vec2 vdxy = vec2(starlightVoronoi(normalize(directionX) * w).x, starlightVoronoi(normalize(directionY) * w).x) - v.xx;
#else  
  vec2 vdxy = vec2(dFdxFine(v.x), dFdyFine(v.x));
#endif
  float f = max(1.0 / w, length(vdxy)) * 2.0;
  return vec3(clamp(smoothstep(0.0, 1.0, starlightNoise(d + vec3(3.0, 5.0, 7.0)).z) * smoothstep(f, 0.0, v.x) * 0.1, 0.0, 65504.0));
} 

#endif