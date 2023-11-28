#ifndef TANGENTSPACEBASIS_GLSL
#define TANGENTSPACEBASIS_GLSL

#define TBN_METHOD 3
void getTangentSpaceBasisFromNormal(in vec3 n, out vec3 t, out vec3 b){
#if TBN_METHOD == 1
  // Has sudden changes sometimes
  vec3 c = vec3(1.0, n.y, -n.x) * (n.y / (1.0 + abs(n.z))),
       d = vec3(n.y, c.yz) * ((n.z >= 0.0) ? 1.0 : -1.0);
  t = vec3(vec2(n.z, 0.0) + d.yz, -n.x);
  b = vec3(c.z, 1.0 - c.y, -d.x);
#elif TBN_METHOD == 2
  // Has sudden changes sometimes
  float s = (n.z >= 0.0) ? 1.0 : -1.0, 
        c = n.y / (1.0 + abs(n.z)), 
        d = n.y * c, 
        e = -n.x * c;
  t = vec3(n.z + (s * d), (s * e), -n.x);
  b = vec3(e, 1.0 - d, -s * n.y);
#elif TBN_METHOD == 3
  // frisvad, http://orbit.dtu.dk/fedora/objects/orbit:113874/datastreams/file_75b66578-222e-4c7d-abdf-f7e255100209/
  if(n.z < -0.999999){
    // Handle the singularity case at n.z is near -1.0 
    // When n.z < -0.999999, the Wolfram Language computation is as follows:
    // n = {0.0, 0.0, -1.0}; (since n.z is set to max(-0.999999, n.z))
    // n[[3]] = Max[-0.999999, n[[3]]];
    // a = n[[2]] / (1.0 + n[[3]]);
    // d = n[[2]] * a;
    // c = -n[[1]] * a;
    // t = Normalize[{n[[3]] + d, c, -n[[1]]}]; -> t = {-1.0, 0.0, 0.0};
    // b = Normalize[{c, 1.0 - d, -n[[2]]}]; -> b = {0.0, 1.0, 0.0};
    t = vec2(-1.0, 0.0).xyy;
    b = vec2(0.0, 1.0).xyx;
  }else{
#if 1
    // Optimized version, by reducing to two products ( http://marc-b-reynolds.github.io/quaternions/2016/07/06/Orthonormal.html ) 
    float a = n.y / (1.0 + n.z),
          d = n.y * a,
          c = -n.x * a;
    t = normalize(vec3(n.z + d, c, -n.x));
    b = normalize(vec3(c, 1.0 - d, -n.y));
#else
    float a = 1.0 / (1.0 + n.z),
    c = -(n.x * n.y * a);
    t = normalize(vec3(1.0 - (n.x * n.x * a), c, -n.x));
    b = normalize(vec3(c, 1.0 - (n.y * n.y * a), -n.y));	
#endif
  }
#elif TBN_METHOD == 4
  // Has sudden changes sometimes 
  float sz = (n.z >= 0.0) ? 1.0 : -1.0,
        a  = 1.0 / (sz + n.z),
        sx = sz * n.x;
  t = vec3(1.0 - ((sx * n.x) * a), sx * n.y * a, -sx);
  b = vec3(n.x * n.y * a, sz - ((n.y * n.y) * a), -n.y);
#elif TBN_METHOD == 5
  // Has just very few sudden changes
  t = normalize(cross(n, vec3(0.0, 0.0, 1.0))), //0.57735026919
  b = cross(n, normalize(t - (dot(t, n) * n)));
  t = cross(b, n); 
#elif TBN_METHOD == 6
  // No sudden changes as well, but computatlionally expensive because of the trigonometric functions as a argument against using this method
  float theta = atan(n.z, n.x), 
        phi = asin(n.y);
  t = normalize(vec3(cos(theta) * sin(phi),
                     -cos(phi),
                     sin(theta) * sin(phi)));
  b = normalize(vec3(sin(theta) * cos(phi),
                     0.0,
                     -cos(theta) * cos(phi)));
  t = normalize(cross(normalize(b - (dot(b, n) * n)), n));
  b = normalize(cross(n, t));
#else 
  // Has sudden changes sometimes
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
