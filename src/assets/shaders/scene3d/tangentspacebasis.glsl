#ifndef TANGENTSPACEBASIS_GLSL
#define TANGENTSPACEBASIS_GLSL

#define TBN_METHOD 0
void getTangentSpaceBasisFromNormal(in vec3 n, out vec3 t, out vec3 b){
#if TBN_METHOD == 1
  vec3 c = vec3(1.0, n.y, -n.x) * (n.y / (1.0 + abs(n.z))),
       d = vec3(n.y, c.yz) * ((n.z >= 0.0) ? 1.0 : -1.0);
  t = vec3(vec2(n.z, 0.0) + d.yz, -n.x);
  b = vec3(c.z, 1.0 - c.y, -d.x);
#elif TBN_METHOD == 2
  float s = (n.z >= 0.0) ? 1.0 : -1.0, 
        c = n.y / (1.0 + abs(n.z)), 
        d = n.y * c, e = -n.x * c;
  t = vec3(n.z + (s * d), (s * e), -n.x);
  b = vec3(e, 1.0 - d, -s * n.y);
#elif TBN_METHOD == 3
  float a = 1.0 / (1.0 + n.z),
        c = -(n.x *n.y * a);
  t = vec3(1.0 - (n.x * n.x * a), c, -n.x);
  b = vec3(c, 1.0 - (n.y * n.y * a), -n.y);	
#elif TBN_METHOD == 4
  float sz = sign(n.z),
        a  = 1.0 / (sz + n.z),
        sx = sz * n.x;
  t = vec3(1.0 - ((sx * n.x) * a), sx * n.y * a, -sx);
  b = vec3(n.x * n.y * a, sz - ((n.y * n.y) * a), -n.y);
#elif TBN_METHOD == 5
  t = normalize(cross(n, vec3(0.0, 0.0, 1.0))), //0.57735026919
  b = cross(n, normalize(t - (dot(t, n) * n)));
  t = cross(b, n); 
#elif TBN_METHOD == 6
  float theta = atan(n.z, n.x), 
        phi = asin(n.y);
  t = normalize(vec3(cos(theta) * sin(phi),
                     -cos(phi),
                     sin(theta) * sin(phi))),
  b = normalize(vec3(sin(theta) * cos(phi),
                     0.0,
                    -cos(theta) * cos(phi)));
  b = cross(n, normalize(t - (dot(t, n) * n)));
  t = cross(b, n);
#else 
  vec3 t0 = cross(vec3(0.0, 0.0, 1.0), n),
       t1 = cross(vec3(0.0, 1.0, 0.0), n);
  t = normalize(length(t0) < length(t1) ? t1 : t0),
  b = cross(n, normalize(t - (dot(t, n) * n)));
  t = normalize(cross(b, n));
#endif
}

mat3 getTangentSpaceFromNormal(vec3 n){
  n = normalize(n);
  vec3 t, b;
  getTangentSpaceBasisFromNormal(n, t, b);
  return mat3(t, b, n);
}  

#endif
