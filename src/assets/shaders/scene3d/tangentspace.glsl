#ifndef TANGENTSPACE_GLSL
#define TANGENTSPACE_GLSL

uint encodeTangentSpaceAsRGB10A2SNorm(mat3 tbn){

  // Normalized tangent space vectors,just for the sake of clarity and to be sure
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

  // Find the tangent diamond direction
  const float tangentDiamondX = tangentInCanonicalSpace.x / (abs(tangentInCanonicalSpace.x) + abs(tangentInCanonicalSpace.y));
  const float tangentDiamondSignYOver4 = sign(tangentInCanonicalSpace.y) * 0.25;
  const float tangentDiamond = fma((0.5 + tangentDiamondSignYOver4) - (tangentDiamondSignYOver4 * tangentDiamondX), 2.0, -1.0);

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
  const float tangentDiamond = fma(float(encodedTangentSpaceUnpacked.z) / 511.0, 0.5, 0.5);
  const float tangentDiamondSign = sign(tangentDiamond - 0.5);
  vec2 tangentInCanonicalSpace;
  tangentInCanonicalSpace.x = (1.0 + (tangentDiamondSign * 2.0)) - (tangentDiamondSign * 4.0 * tangentDiamond);
  tangentInCanonicalSpace.y = tangentDiamondSign * (1.0 - abs(tangentInCanonicalSpace.x));

  // Decode the tangent
  const vec3 tangent = normalize((tangentInCanonicalSpace.x * canonicalDirectionA) + (tangentInCanonicalSpace.y * canonicalDirectionB));

  // Decode the bitangent
  const vec3 bitangent = normalize(cross(normal, tangent) * float(encodedTangentSpaceUnpacked.w));

  return mat3(tangent, bitangent, normal);

}
#endif
