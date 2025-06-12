#ifndef PBR_WETNESS_GLSL
#define PBR_WETNESS_GLSL

// Wetness for PBR materials

void applyWetness(
  const in vec4 wetness, // x = wetness factor, yzw = normal from planet ground
  const in mat3 tangentSpaceBasis, // tangent space basis matrix (tangent, bitangent, normal)
  inout vec3 albedo, // albedo color
  inout vec3 normal, // normal vector
  inout float metallic, // metallic value 
  inout float roughness, // roughness value
  inout float occlusion // occlusion value
){

  // Not optimal yet, just the foundation for wetness application in PBR as first version.
    
  // Apply wetness to albedo
  albedo = mix(albedo, vec3(0.0, 0.0, 1.0), wetness.x); // Example: blend with blue color for wetness

  // Apply wetness to normal
  normal = normalize(mix(normal, wetness.yzw, wetness.x)); // Blend normal with the provided normal based on wetness factor

  // Apply wetness to metallic and roughness
  metallic = mix(metallic, 1.0, wetness.x); // Increase metallic with wetness
  roughness = mix(roughness, 0.1, wetness.x); // Decrease roughness with wetness
  occlusion = mix(occlusion, 1.0, wetness.x); // Increase occlusion with wetness

} 

#endif