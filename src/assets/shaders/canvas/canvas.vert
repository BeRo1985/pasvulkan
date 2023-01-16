#version 450 core

// Copyright (C) 2017, Benjamin 'BeRo' Rosseaux (benjamin@rosseaux.de)
// License: zlib 

layout(location = 0) in vec3 inPosition; 
layout(location = 1) in vec4 inColor;    
#if USETEXTURE
layout(location = 2) in vec3 inTexCoord; 
#endif
layout(location = 3) in uint inState;    
layout(location = 4) in vec4 inClipRect; 
layout(location = 5) in vec4 inMetaInfo; 

layout(location = 0) out vec2 outPosition;
layout(location = 1) out vec4 outColor;
#if USETEXTURE
layout(location = 2) out vec3 outTexCoord;
#endif
layout(location = 3) flat out ivec4 outState;    
#if USECLIPDISTANCE
layout(location = 4) out vec4 outMetaInfo; 
#else
layout(location = 4) out vec4 outClipRect; 
layout(location = 5) out vec2 outClipSpacePosition; 
layout(location = 6) out vec4 outMetaInfo; 
#endif

layout(push_constant) uniform PushConstants {
  layout(offset = 0) mat4 transformMatrix;
  layout(offset = 64) mat4 fillMatrix;
} pushConstants;

out gl_PerVertex {
  vec4 gl_Position;
#if USECLIPDISTANCE
  float gl_ClipDistance[];  
#endif
};

void main(void){
  outPosition = inPosition.xy;
  outColor = inColor;
#if USETEXTURE
  outTexCoord = inTexCoord;
#endif
  outState = ivec4(uvec4((inState >> 0u) & 0x3u,
                         (inState >> 2u) & 0xffu,                         
                         (inState >> 10u) & 0xfu,                         
                         (inState >> 14u) & 0xfu));
#if !USECLIPDISTANCE
  outClipRect = inClipRect;
#endif
  outMetaInfo = inMetaInfo;
  vec4 p = pushConstants.transformMatrix * vec4(inPosition.xy, 0.0, 1.0);
  vec2 clipSpacePosition = p.xy / p.w;
  gl_Position = vec4(clipSpacePosition, 1.0 - inPosition.z, 1.0);
#if USECLIPDISTANCE
  gl_ClipDistance[0] = clipSpacePosition.x - inClipRect.x;
  gl_ClipDistance[1] = clipSpacePosition.y - inClipRect.y;
  gl_ClipDistance[2] = inClipRect.z - clipSpacePosition.x;
  gl_ClipDistance[3] = inClipRect.w - clipSpacePosition.y;
#else
  outClipSpacePosition = clipSpacePosition;
#endif
}