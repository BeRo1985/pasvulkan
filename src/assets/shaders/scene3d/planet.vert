#version 450 core

#pragma shader_stage(vertex)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 tangent;
  vec3 bitangent;
  vec3 normal;
  vec3 uvw;   
} outBlock;

layout(push_constant) uniform PushConstants {
  int viewBaseIndex;
  int countViews;
  int countQuadPointsInOneDirection; 
  int countAllViews;
  float bottomRadius;
  float topRadius;
  float resolutionX;  
  float resolutionY;  
  vec2 jitter;
} pushConstants;

int countQuadPointsInOneDirection = pushConstants.countQuadPointsInOneDirection;
int countSideQuads = countQuadPointsInOneDirection * countQuadPointsInOneDirection;
int countTotalVertices = countSideQuads * (6 * 4);

const ivec2 offsets[4] = ivec2[](
  ivec2(0, 0),  
  ivec2(0, 1),  
  ivec2(1, 1),  
  ivec2(1, 0)
);  

const mat3 sideMatrices[6] = mat3[6](
  mat3(vec3(0.0, 0.0, -1.0), vec3(0.0, -1.0, 0.0), vec3(-1.0, 0.0, 0.0)), // pos x
  mat3(vec3(0.0, 0.0, 1.0), vec3(0.0, -1.0, 0.0), vec3(1.0, 0.0, 0.0)),   // neg x
  mat3(vec3(1.0, 0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 1.0, 0.0)),   // pos y
  mat3(vec3(1.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3(0.0, -1.0, 0.0)),   // neg y
  mat3(vec3(1.0, 0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, 0.0, -1.0)),  // pos z
  mat3(vec3(-1.0, 0.0, 0.0), vec3(0.0, -1.0, 0.0), vec3(0.0, 0.0, 1.0))   // neg z
);  

const float HalfPI = 1.5707963267948966, // 1.570796326794896619231,
            PI = 3.141592653589793, // 3.141592653589793238463,
            PI2 = 6.283185307179586; // 6.283185307179586476925   
            
vec3 getNormal(mat3 m, vec2 uv){
  vec3 unitCube = m * vec3((uv - vec2(0.5)) * 2.0, 1.0),
#define MODE 0
#if MODE == 1
       // Spherified Cube
       // http://mathproofs.blogspot.com/2005/07/mapping-cube-to-sphere.html
       // http://petrocket.blogspot.com/2010/04/sphere-to-cube-mapping.html
       unitCubeSquared = unitCube * unitCube,
       unitCubeSquaredDiv2 = unitCubeSquared * 0.5,
       unitCubeSquaredDiv3 = unitCubeSquared / 3.0,
       mappedSphere = unitCube * sqrt(((1.0 - unitCubeSquaredDiv2.yzx) - 
                                       unitCubeSquaredDiv2.zxy) + 
                                      (unitCubeSquared.yzx * 
                                       unitCubeSquaredDiv3.zxy)),
       normal = normalize(mappedSphere);
#else
       // Normalized Cube
       normal = normalize(unitCube);
#endif
  return normal; 
}

#define TBN_METHOD 0

#if TBN_METHOD == 0
const mat3 tangentTransformMatrix = mat3(vec3(0.0, 0.0, -1.0), vec3(0.0, -1.0, 0.0), vec3(1.0, 0.0, 0.0)),
           bitangentTransformMatrix = mat3(vec3(-1.0, 0.0, 0.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 1.0, 0.0));
#else
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
        a  = 1.0f / (sz + n.z),
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

void main(){                 
  int vertexIndex = gl_VertexIndex;
  if(vertexIndex < countTotalVertices){   
    int quadIndex = vertexIndex >> 2,
        quadVertexIndex = vertexIndex & 3,  
        sideIndex = (quadIndex / countSideQuads) % 6,
        sideQuadIndex = quadIndex % countSideQuads;
    vec2 uv = vec2(ivec2(sideQuadIndex % countQuadPointsInOneDirection,
                        sideQuadIndex / countQuadPointsInOneDirection) + 
                   offsets[3 - quadVertexIndex]) / vec2(countQuadPointsInOneDirection);
    mat3 normalMatrix = sideMatrices[sideIndex];       
    vec3 normal = getNormal(normalMatrix, uv),
         position = normal * pushConstants.bottomRadius;
#if TBN_METHOD > 0
    vec3 tangent,                                     
         bitangent;
    getTangentSpaceBasisFromNormal(normal, tangent, bitangent);
#else
    vec3 tangent = getNormal(normalMatrix * tangentTransformMatrix, uv),
         bitangent = getNormal(normalMatrix * bitangentTransformMatrix, uv);
    tangent = normalize(tangent - (dot(tangent, normal) * normal));
    bitangent = normalize(bitangent - (dot(bitangent, normal) * normal));
    tangent = cross(bitangent, normal);
    bitangent = cross(normal, tangent);
#endif
    outBlock.position = position;    
    outBlock.tangent = tangent;
    outBlock.bitangent = bitangent;
    outBlock.normal = normal;
    outBlock.uvw = normal; 
  }else{
    outBlock.position = vec3(0.0);
    outBlock.normal = vec3(0.0);
    outBlock.uvw = vec3(0.0);                       
  }  
}