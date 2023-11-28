#ifndef TANGENTSPACEBASIS_GLSL
#define TANGENTSPACEBASIS_GLSL

#define TBN_METHOD 1 
void getTangentSpaceBasisFromNormal(in vec3 n, out vec3 t, out vec3 b){
#if TBN_METHOD == 1
  // frisvad, http://orbit.dtu.dk/fedora/objects/orbit:113874/datastreams/file_75b66578-222e-4c7d-abdf-f7e255100209/
  // No sudden changes in the tangent space basis, but it has a singularity case at n.z is near -1.0
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
#elif TBN_METHOD == 2
  // https://graphics.pixar.com/library/OrthonormalB/paper.pdf good, but it has sudden changes in the tangent space basis because of
  // the sign function, so it is not so good usable in my opinion as the paper authors claim, since I do need smooth transitions
  // between all input neuighboor normals, and that isn't the case with this method. 
  float s = sign(sign(n.z) + 0.5),
        a = -1.0 / (s + n.z),
        c = n.x * n.y * a;
  t = vec3((((s * (n.x * n.x))) * a) + 1.0, s * c, (-s) * n.x);
  b = vec3(c, ((n.y * n.y) * a) + s, -n.y);  
#elif TBN_METHOD == 3
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
  #error "TBN_METHOD not defined"
#endif
}

mat3 getTangentSpaceFromNormal(vec3 n){
  n = normalize(n);
  vec3 t, b;
  getTangentSpaceBasisFromNormal(n, t, b);
  return mat3(t, b, n);
}  

#endif
