#ifndef TANGENTSPACE_GLSL
#define TANGENTSPACE_GLSL

// Copyright 2024, Benjamin 'BeRo' Rosseaux - zlib licensed

/*
** Tangent space encoding and decoding functions from tangent space vectors to a single 32-bit unsigned
** integer in RGB10A2 snorm format and back.
** 
** These functions are used to encode and decode tangent space vectors into a single 32-bit unsigned integer.
** The encoding is done using the RGB10A2 snorm format, which allows to store the tangent space in a single integer.
** The encoding is lossy, but the loss is very small and the precision is enough for most use cases.
** 
** The encoding is done as follows:
** 1. The normal is projected onto the octahedron, which is a 2D shape that represents the normal in a more efficient way.
** 2. The tangent is projected onto the canonical diamond space, which is a 2D space that is aligned with the normal.
** 3. The tangent is projected onto the tangent diamond, which is a 1D space that represents the tangent in a more efficient way.
** 4. The bitangent sign is stored in signed 2 bits as -1.0 or 1.0.
** 5. The values are packed into a single 32-bit unsigned integer using the RGB10A2 snorm format.
** 
** The decoding is done as follows:
** 1. The values are unpacked from the RGB10A2 snorm format.
** 2. The normal is decoded from the octahedron.
** 3. The canonical directions are found.
** 4. The tangent diamond is decoded.
** 5. The tangent is found using the canonical directions and the tangent diamond.
** 6. The bitangent is found using the normal, the tangent and the bitangent sign. 
** 
** Idea based on https://www.jeremyong.com/graphics/2023/01/09/tangent-spaces-and-diamond-encoding/ but with improvements for
** packing into RGB10A2 snorm to a 32-bit unsigned integer.
**
**/

uint encodeTangentSpaceAsRGB10A2SNorm(mat3 tbn){

  // Normalize tangent space vectors, just for the sake of clarity and for to be sure
  tbn[0] = normalize(tbn[0]);
  tbn[1] = normalize(tbn[1]);
  tbn[2] = normalize(tbn[2]);

  // Get the octahedron normal
  const vec3 normal = tbn[2];
  vec2 octahedronalNormal = normal.xy / (abs(normal.x) + abs(normal.y) + abs(normal.z)); 
  octahedronalNormal = (normal.z < 0.0) ? ((1.0 - abs(octahedronalNormal.yx)) * fma(step(vec2(0.0), octahedronalNormal.xy), vec2(2.0), vec2(-1.0))) : octahedronalNormal;
  
  // Find the canonical directions
  const vec3 canonicalDirectionA = normalize((abs(normal.y) > abs(normal.z)) ? vec3(normal.y, -normal.x, 0.0) : vec3(normal.z, 0.0, -normal.x));
  const vec3 canonicalDirectionB = cross(normal, canonicalDirectionA);

  // Project the tangent into the canonical space 
  const vec2 tangentInCanonicalSpace = vec2(dot(tbn[0], canonicalDirectionA), dot(tbn[0], canonicalDirectionB));

  // Find the tangent diamond direction (a diamond is more or less the 2D equivalent of the 3D octahedron here in this case)
  const float tangentDiamond = (1.0 - (tangentInCanonicalSpace.x / (abs(tangentInCanonicalSpace.x) + abs(tangentInCanonicalSpace.y)))) * ((tangentInCanonicalSpace.y < 0.0) ? -1.0 : 1.0) * 0.5;

  // Find the bitangent sign
  const float bittangentSign = (dot(cross(tbn[0], tbn[1]), tbn[2]) < 0.0) ? -1.0 : 1.0; 

  // Encode the tangent space as signed values
  const ivec4 encodedTangentSpace = ivec4(
    ivec2(clamp(octahedronalNormal, vec2(-1.0), vec2(1.0)) * 511.0), // 10 bits including sign
    int(clamp(tangentDiamond, -1.0, 1.0) * 511.0), // 10 bits including sign
    int(clamp(bittangentSign, -1.0, 1.0)) // 2 bits
  );
  
  // Pack the values into RGB10A2 snorm
  return ((uint(encodedTangentSpace.x) & 0x3ffu) << 0u) | 
         ((uint(encodedTangentSpace.y) & 0x3ffu) << 10u) | 
         ((uint(encodedTangentSpace.z) & 0x3ffu) << 20u) | 
         ((uint(encodedTangentSpace.w) & 0x3u) << 30u);

}
  
mat3 decodeTangentSpaceFromRGB10A2SNorm(const in uint encodedTangentSpace){

  // Unpack the values from RGB10A2 snorm
  const ivec4 encodedTangentSpaceUnpacked = ivec4(
    int(uint(encodedTangentSpace << 22u)) >> 22,
    int(uint(encodedTangentSpace << 12u)) >> 22,
    int(uint(encodedTangentSpace << 2u)) >> 22,
    int(uint(encodedTangentSpace << 0u)) >> 30
  );

  // Decode the tangent space
  const vec2 octahedronalNormal = vec2(encodedTangentSpaceUnpacked.xy) / 511.0;
  vec3 normal = vec3(octahedronalNormal, 1.0 - (abs(octahedronalNormal.x) + abs(octahedronalNormal.y)));
  normal = normalize((normal.z < 0.0) ? vec3((1.0 - abs(normal.yx)) * fma(step(vec2(0.0), normal.xy), vec2(2.0), vec2(-1.0)), normal.z) : normal);

  // Find the canonical directions
  const vec3 canonicalDirectionA = normalize((abs(normal.y) > abs(normal.z)) ? vec3(normal.y, -normal.x, 0.0) : vec3(normal.z, 0.0, -normal.x));
  const vec3 canonicalDirectionB = cross(normal, canonicalDirectionA);

  // Decode the tangent diamond direction
  const float tangentDiamond = float(encodedTangentSpaceUnpacked.z);
  const float tangentDiamondSign = (tangentDiamond < 0.0) ? -1.0 : 1.0; // No sign() because for 0.0 in => 1.0 out
  vec2 tangentInCanonicalSpace;
  tangentInCanonicalSpace.x = 1.0 - (tangentDiamond * tangentDiamondSign * 2.0);
  tangentInCanonicalSpace.y = tangentDiamondSign * (1.0 - abs(tangentInCanonicalSpace.x));

  // Decode the tangent
  const vec3 tangent = normalize((tangentInCanonicalSpace.x * canonicalDirectionA) + (tangentInCanonicalSpace.y * canonicalDirectionB));

  // Decode the bitangent
  const vec3 bitangent = normalize(cross(normal, tangent) * float(encodedTangentSpaceUnpacked.w));

  return mat3(tangent, bitangent, normal);

}

#endif
