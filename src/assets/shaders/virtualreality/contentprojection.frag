#version 450 core

#extension GL_EXT_multiview : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outColor;

layout(set = 0, binding = 0) uniform inUniformBufferObject {
  mat4 inverseViewProjectionMatrix[2];  
  vec2 resolution;
} uboGlobals;
                            

layout(set = 0, binding = 1) uniform sampler2D uTextureContent;

const float PI = 3.141592653589793238462643383;

#if 1
const vec3 luma = vec3(0.2126, 0.7152, 0.0722);
#else
const vec3 luma = vec3(0.2989, 0.587, 0.114);
#endif                   

float flipSign = 1.0;

mat2 rotate2D(float t) {
  return mat2(cos(t), sin(t), -sin(t), cos(t));
}

float linearstep(float a, float b, float c){
  return clamp((c - a) / (b - a), 0.0, 1.0);  
}

vec2 safenormalize(vec2 v){
  float l = length(v);
  return (l > 1e-6) ? (v / l) : v;   
}

vec4 alphaBlend(vec4 a, vec4 b){
  return mix(a, b, b.w);
}

vec3 sunPosition = normalize(vec3(0.0, 0.0, -1.0));

vec3 getRayDirection(vec2 offset){
  vec2 ndcUV = fma(clamp(inTexCoord + (offset / uboGlobals.resolution), 
                         vec2(0.0), 
                         vec2(1.0)), 
                   vec2(2.0), 
                   vec2(-1.0));
  vec4 n = uboGlobals.inverseViewProjectionMatrix[gl_ViewIndex] * vec4(ndcUV, -1.0, 1.0),
       f = uboGlobals.inverseViewProjectionMatrix[gl_ViewIndex] * vec4(ndcUV, 1.0, 1.0);
  return normalize((f.xyz / f.w) - (n.xyz / n.w)); 
}  

bool intersectPlane(vec3 rayOrigin, vec3 rayDirection, vec4 plane, inout float t){
	bool hit = false;
	float d = dot(rayDirection, plane.xyz);
	if(abs(d) > 1e-5){
		d = -dot(plane, vec4(rayOrigin, 1.0)) / d;
		if((d > 0.0) && ((d < t) || (t > 65536.0))){
		  t = d;
			hit = true;
		}
	}
	return hit;
}

bool intersectQuad(vec3 rayOrigin, vec3 rayDirection, vec3 position, vec3 dir1, vec3 dir2, inout float distance, inout vec2 uv) {
  vec3 normal = -normalize(cross(dir1, dir2));
  float t = dot(normal, position - rayOrigin) / dot(normal, rayDirection);
  if (t <= 0.0) {
    return false;
  }else{
    vec3 offsetedIntersectionPoint = fma(rayDirection, vec3(t), rayOrigin) - position;
    vec2 sizeSquared = vec2(dot(dir1, dir1), dot(dir2, dir2));
    vec2 p = vec2(dot(offsetedIntersectionPoint, dir1), dot(offsetedIntersectionPoint, dir2));
    bool intersected = all(greaterThan(p, vec2(-1e-6))) && all(lessThanEqual(p, sizeSquared));
    if (intersected && (t <= distance)) {
      distance = t;
      uv = p / sizeSquared;
    }
    return intersected;
  }
}

void main(){
  
  vec4 c = vec2(0.0, 1.0).xxxy;

  float radialBlurStrength = 0.0;
  
  vec3 rayOrigin,
       rayDirection,
       relativeSunPosition;
  {
    vec4 t = uboGlobals.inverseViewProjectionMatrix[gl_ViewIndex] * vec4(fma(inTexCoord, vec2(2.0), vec2(-1.0)), -1.0, 1.0);
    rayOrigin = t.xyz / t.w;  
  }
  rayDirection = getRayDirection(vec2(0.0));
/*  
  {     
    float theta = acos(sunPosition.z),
          phi = all(not(equal(sunPosition.xy, vec2(0.0)))) ? atan(sunPosition.y, sunPosition.x) : (PI * 0.5);
    relativeSunPosition = (
                           mat3(cos(theta), 0.0, sin(theta),
                                0.0, 1.0, 0.0, 
                                -sin(theta), 0.0, cos(theta)
                                
                                ) *
                           mat3(cos(phi), -sin(phi), 0.0, 
                                sin(phi), cos(phi), 0.0,                                
                                0.0, 0.0, 1.0)
                          ) * rayDirection;  
  }

  {
    {
      // Sky gradient
      c = alphaBlend(c,
                     vec4(pow(mix(mix(vec3(0.19215, 0.12941, 0.25882) * 1.414,
                                      vec3(0.0, 0.05, 0.20),               
                                     linearstep(0.0, 0.5, rayDirection.y)),
                                  vec3(0.998, 0.108, 0.47),
                                  mix(0.0, 
                                      pow(linearstep(1.0, 0.0, rayDirection.y), 8.0),
                                      max(0.0, dot(rayDirection.xz, normalize(sunPosition.xz)))) *
                                      linearstep(0.4, 0.3, abs(sunPosition.y))),
                              vec3(2.2)),
                          1.0));
    }  
    {
      // Sun
      c = alphaBlend(c, vec4(pow(mix(vec3(0.998, 0.108, 0.47),
                                     vec3(0.988, 0.769, 0.176),
                                     pow(linearstep(-0.0, 0.125, rayDirection.y - sunPosition.y), 0.5)),
                                 vec3(2.2)),
                            linearstep(cos(radians(16.0)),
                                       cos(radians(15.0)), 
                                       dot(rayDirection, sunPosition)) *
                            mix(pow(linearstep(-1.0, 1.0, sin((rayDirection.y - sunPosition.y) * PI * 256.0)), 2.0),
                                1.0,
                                pow(linearstep(-0.5, 0.25, rayDirection.y - sunPosition.y), 0.5))));
    }
    c = alphaBlend(c, vec4(mix(vec3(0.0078125, 0.015625, 0.25),
                               vec3(0.001953125, 0.0078125, 0.125),
                               pow(linearstep(0.0, -0.125, rayDirection.y), 1.0)), 
                           linearstep(0.0, -1e-3, rayDirection.y)));  
  }
   
  c.w = 1.0;//radialBlurStrength * clamp(1.0 - (c.a * 1024.0), 0.0, 1.0);

//*/

 {
    float time = 1e+16;
    vec2 uv = vec2(0.0); 
    if(intersectQuad(rayOrigin,
                     rayDirection, 
                     vec3(-1.0, -1.0, -2.0), 
                     vec3(2.0, 0.0, 0.0), 
                     vec3(0.0, 2.0, 0.0), 
                     time, 
                     uv)){
      uv.y = 1.0 - uv.y; // Flip Y coordinate
      vec2 texSize = vec2(textureSize(uTextureContent, 0));  
      uv = fma(uv, vec2(2.0), vec2(-1.0)); 
      vec2 aspectCorrect = (texSize.x > texSize.y) ? vec2(1.0, texSize.x / texSize.y) : vec2(texSize.y / texSize.x, 1.0);
      uv = uv * aspectCorrect;
      uv = fma(uv, vec2(0.5), vec2(0.5));
      c = texture(uTextureContent, uv);
      c = mix(vec4(uv, 0.0, 1.0), c, 1.0);    
    }
  }

  //c.xyz = rayDirection.xyz;
  
	outColor = c;
                             
}
