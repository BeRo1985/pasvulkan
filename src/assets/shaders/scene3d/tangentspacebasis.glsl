#ifndef TANGENTSPACEBASIS_GLSL
#define TANGENTSPACEBASIS_GLSL

void getTangentSpaceBasisFromNormal(in vec3 n, out vec3 t, out vec3 b){
  // https://graphics.pixar.com/library/OrthonormalB/paper.pdf
  float s = sign(sign(n.z) + 0.5),
        a = -1.0 / (s + n.z),
        c = n.x * n.y * a;
  t = vec3(1.0 + s * n.x * n.x * a, s * c, -s * n.x);
  b = vec3(c, s + n.y * n.y * a, -n.y);  
}

mat3 getTangentSpaceFromNormal(vec3 n){
  n = normalize(n);
  vec3 t, b;
  getTangentSpaceBasisFromNormal(n, t, b);
  return mat3(t, b, n);
}  

#endif
