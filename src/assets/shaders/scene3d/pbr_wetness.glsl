#ifndef PBR_WETNESS_GLSL
#define PBR_WETNESS_GLSL

// Wetness for PBR materials

void applyPBRWetness(
  const in vec4 wetness, // x = wetness factor, yzw = normal from planet ground
  const in vec3 position, // world space position for triplanar mapping
  const in mat3 tangentSpaceBasis, // tangent space basis matrix (tangent, bitangent, normal)
  inout vec3 albedo, // albedo color
  out vec4 normal, // output normal vector for normal mapping, w = weight for normal mapping
  inout float metallic, // metallic value 
  inout float roughness, // roughness value
  inout float occlusion // occlusion value
){

  normal = vec4(0.0, 0.0, 1.0, 0.0); // Initialize output normal

  // Not optimal yet, just the foundation for wetness application in PBR as first version.
  if(wetness.x > 0.0){

    // Apply wetness to albedo
    //albedo = mix(albedo, albedo * vec3(0.2, 0.8, 1.0), wetness.x * 0.1); // Blend with blue color for wetness

    // Apply wetness to normal
    //normal = normalize(mix(normal, wetness.yzw, wetness.x * 0.1)); // Blend normal with the provided normal based on wetness factor

    // Apply wetness to metallic and roughness
//  metallic = mix(metallic, 0.0, wetness.x); // Decrease metallic with wetness
    roughness = mix(roughness, roughness * 0.1, wetness.x); // Decrease roughness with wetness
//  occlusion = mix(occlusion, 1.0, wetness.x); // Increase occlusion with wetness

  }

} 

#endif