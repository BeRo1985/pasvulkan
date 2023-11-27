#version 450

#pragma shader_stage(tesseval)

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_GOOGLE_include_directive : enable

layout(quads, equal_spacing, ccw) in;

layout(location = 0) in InBlock {
  vec3 position;
  vec3 tangent;
  vec3 bitangent;
  vec3 normal;
  vec3 uvw;   
} inBlocks[];

layout(location = 0) out OutBlock {
  vec3 position;
  vec3 tangent;
  vec3 bitangent;
  vec3 normal;
  vec3 uvw;   
  vec3 worldSpacePosition;
  vec3 viewSpacePosition;
  vec3 cameraRelativePosition;
  vec2 jitter;
#ifdef VELOCITY
  vec4 previousClipSpace;
  vec4 outCurrentClipSpace;
#endif  
} outBlock;

in gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
	float gl_ClipDistance[];
} gl_in[];

out gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
	float gl_ClipDistance[];
};

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

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 0, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

layout(set = 0, binding = 1) uniform sampler2D uTextureHeightMap; // xyz = normal, w = height

#include "octahedralmap.glsl"

int viewIndex = pushConstants.viewBaseIndex + int(gl_ViewIndex);
mat4 viewMatrix = uView.views[viewIndex].viewMatrix;
mat4 projectionMatrix = uView.views[viewIndex].projectionMatrix;
mat4 inverseViewMatrix = uView.views[viewIndex].inverseViewMatrix;

vec4 cubic(const in float v){
  vec4 n = vec4(1.0, 2.0, 3.0, 4.0) - v, s = n * n * n;
  vec3 t = vec3(s.x, s.y - (4.0 * s.x), (s.z - (4.0 * s.y)) + (6.0 * s.x));
  return vec4(t, 6.0 - dot(t, vec3(1.0))) * (1.0 / 6.0);
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

vec4 textureBicubic(const in sampler2D tex, const in vec2 texCoords, const in int lod){
  vec2 texSize = textureSize(tex, lod),
       uv = (texCoords * texSize) - vec2(0.5),
       fxy = fract(uv);
  vec4 xcubic = cubic(fxy.x),
       ycubic = cubic(fxy.y),
       s = vec4(xcubic.xz + xcubic.yw, ycubic.xz + ycubic.yw),
       offset = (((uv - fxy).xxyy + vec2(-0.5, +1.5).xyxy) + 
                 (vec4(xcubic.yw, ycubic.yw) / s)) * 
                (vec2(1.0) / texSize).xxyy;
  vec3 f = vec3(s.x / (s.x + s.y), s.z / (s.z + s.w), float(lod));
  return mix(mix(textureLod(tex, offset.yw, f.z), textureLod(tex, offset.xw, f.z), f.x), 
             mix(textureLod(tex, offset.yz, f.z), textureLod(tex, offset.xz, f.z), f.x), f.y);
}           

vec4 textureTriplanar(const in sampler2D t, const in vec3 p, const in vec3 n, const in float k, const in vec3 gx, const in vec3 gy){
//vec2 r = textureSize(t, 0);
  vec3 m = pow(abs(n), vec3(k));
  return ((textureGrad(t, p.yz, gx.yz, gy.yz) * m.x) + 
          (textureGrad(t, p.zx, gx.zx, gy.zx) * m.y) + 
          (textureGrad(t, p.xy, gx.xy, gy.xy) * m.z)) / (m.x + m.y + m.z);
}           

void main(){	  

  mat4 viewProjectionMatrix = projectionMatrix * viewMatrix;

#if 1
  // The actual standard approach
  vec3 cameraPosition = inverseViewMatrix[3].xyz;
#else
  // This approach assumes that the view matrix has no scaling or skewing, but only rotation and translation.
  vec3 cameraPosition = (-viewMatrix[3].xyz) * mat3(viewMatrix);
#endif

  vec3 position = mix(mix(inBlocks[0].position, inBlocks[1].position, gl_TessCoord.x),
                      mix(inBlocks[3].position, inBlocks[2].position, gl_TessCoord.x), 
                      gl_TessCoord.y),
  
       tangent = normalize(mix(mix(inBlocks[0].tangent, inBlocks[1].tangent, gl_TessCoord.x), 
                               mix(inBlocks[3].tangent, inBlocks[2].tangent, gl_TessCoord.x), 
                               gl_TessCoord.y)),
  
       bitangent = normalize(mix(mix(inBlocks[0].bitangent, inBlocks[1].bitangent, gl_TessCoord.x), 
                                 mix(inBlocks[3].bitangent, inBlocks[2].bitangent, gl_TessCoord.x), 
                                 gl_TessCoord.y)),
  
       normal = normalize(mix(mix(inBlocks[0].normal, inBlocks[1].normal, gl_TessCoord.x), 
                              mix(inBlocks[3].normal, inBlocks[2].normal, gl_TessCoord.x),
                              gl_TessCoord.y)),

       uvw = mix(mix(inBlocks[0].uvw, inBlocks[1].uvw, gl_TessCoord.x), 
                 mix(inBlocks[3].uvw, inBlocks[2].uvw, gl_TessCoord.x), 
                     gl_TessCoord.y);
                     
#if 1

//tangent = cross(bitangent = cross(normal, normalize(tangent - (dot(tangent, normal) * normal))), normal);
/*vec3 bitangent = normalize(cross(vec3(0.0, 1.0, 0.0), normal)),
                tangent = normalize(cross(normal, bitangent));
  tangent = normalize(tangent - (tangent * dot(tangent, normal)));
  bitangent = normalize(bitangent - (bitangent * dot(bitangent, normal)));      */
  
  mat3 tangentSpace = mat3(tangent, bitangent, normal);
 
  vec4 nm = textureOctahedralMap(uTextureHeightMap, normal);
  
  position += tangentSpace[2] * nm.w;

  normal = nm.xyz;

  tangent = cross(bitangent = cross(normal, normalize(tangent - (dot(tangent, normal) * normal))), normal); // recalculate tangent and bitangent
    
#endif

  vec3 worldSpacePosition = position;

  vec4 viewSpacePosition = viewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outBlock.position = position;         
  outBlock.tangent = tangent;
  outBlock.bitangent = bitangent;
  outBlock.normal = normal;
  outBlock.uvw = uvw;
  outBlock.worldSpacePosition = worldSpacePosition;
  outBlock.viewSpacePosition = viewSpacePosition.xyz;  
  outBlock.cameraRelativePosition = worldSpacePosition - cameraPosition;
  outBlock.jitter = pushConstants.jitter;
#ifdef VELOCITY
  outBlock.currentClipSpace = (projectionMatrix * viewMatrix) * vec4(position, 1.0);
  outBlock.previousClipSpace = (uView.views[viewIndex + pushConstants.countAllViews].projectionMatrix * uView.views[viewIndex + pushConstants.countAllViews].viewMatrix) * vec4(position, 1.0);
#endif

	gl_Position = viewProjectionMatrix * vec4(position, 1.0);
  
}
