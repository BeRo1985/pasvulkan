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
  inout float occlusion, // occlusion value
  const in sampler2D rainTexture, // texture for rain effect
  const in sampler2D rainNormalTexture, // texture for rain normal mapping
  const in sampler2D rainStreakNormalTexture, // texture for rain normal mapping
  const float rainTime
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
  //  roughness = mix(roughness, roughness * 0.1, wetness.x); // Decrease roughness with wetness
//  occlusion = mix(occlusion, 1.0, wetness.x); // Increase occlusion with wetness

    float normalDotGround = dot(tangentSpaceBasis[2], wetness.yzw); // Dot product with ground normal to determine wetness effect on normal

    // Triplanar mapping for puddles effect
    vec2 puddlesUV = vec2(
      dot(tangentSpaceBasis[0], position), 
      dot(tangentSpaceBasis[1], position)
    );

    // Calculate fractional time for puddles effect
    vec2 fracPuddleTimes = fract(vec2(rainTime, rainTime + 0.5)); 

    // Puddles effect based on rain texture
    vec2 puddleValues = vec2( 
      texture(rainTexture, puddlesUV.xy).x,  
      texture(rainTexture, puddlesUV.yx).x) - (vec2(1.0) - fracPuddleTimes); 
    puddleValues =
      clamp(
        (vec2(1.0) - vec2(
            distance(vec2(puddleValues.x), vec2(0.1)) / 0.1, 
            distance(vec2(puddleValues.y), vec2(0.1)) / 0.1
          )
        ) * abs(sin(fracPuddleTimes * 3.14159)), vec2(0.0), vec2(1.0));
    vec4 puddles = vec4(
        mix(
          vec3(0.0, 0.0, 1.0),
          fma(texture(rainNormalTexture, puddlesUV).xyz, vec3(2.0), vec3(-1.0)),
          max(puddleValues.x, puddleValues.y)
        ),
        max(puddleValues.x, puddleValues.y)
      ) * 
      clamp((normalDotGround - 0.2) * 5.0, 0.0, 1.0) * // Puddles only on top 
      wetness.x; // Apply wetness factor to puddles, no rain, no puddles 

    // Calcularte the UV coordinates for rain streaks
    vec2 streaksUV = vec2(
      dot(tangentSpaceBasis[1], position),
      dot(tangentSpaceBasis[2], position)
    );

    // Calculate the rain streaks effect
    float underRoof = smoothstep(-0.3, 0.0, normalDotGround);
    float streaks = texture(rainTexture, streaksUV).y * // Get rain streaks from the texture   
                    (1.0 - clamp((normalDotGround - 0.05) * 20.0, 0.0, 1.0)) * // Streaks not on top, only on sides 
                    wetness.x; // Apply wetness factor to streaks, no rain, no streaks
    streaks = smoothstep(
      0.0,
      0.1,
      streaks * 
      clamp(
        texture(
          rainTexture,
          vec2(
            streaksUV.x,
            fract(streaksUV.y + (rainTime * 0.2))
          )
        ).z - 0.5,
        0.0,
        1.0
      )
    ) *
    underRoof; // When normal is facing downwards, no rain flow

    // Calculate the final normal vector based on puddles and streaks
    vec3 rainNormal = 
      mix(
        mix(
          vec3(0.0, 0.0, 1.0), // Default normal
          puddles.xyz,
          puddles.w // Apply puddles effect
        ),
        fma(texture(rainStreakNormalTexture, streaksUV).xyz, vec3(2.0), vec3(-1.0)), // Normal from rain texture
        streaks // Apply streaks effect
      );           
    normal = vec4(rainNormal, 1.0); // Set the normal vector for normal mapping

    // Calculate the wetness and rain factors
    float wet = clamp(wetness.x, 0.0, 1.0); // Clamp wetness factor to [0.0, 1.0]
    float rain = clamp(puddles.w + streaks, 0.0, 1.0); // Get the maximum effect of puddles and streaks

    // Apply wetness to roughness
    roughness = mix(roughness * wet, 0.0, rain); // Apply wetness to roughness based on rain effect

    // Calculate the color based on wetness and metallic factor
    vec3 dryColor = albedo.xyz;
    vec3 wetColor = mix(dryColor * wet, dryColor, metallic); // Darken the albedo color based on wetness, if not metallic, keep the original color
    wetColor = mix(dryColor, wetColor, underRoof); 
    albedo.xyz = wetColor; // Set the albedo color based on wetness and rain effect

    // Apply wetness to metallic
    metallic = mix(metallic, 0.0, rain); // Decrease metallic based on rain and wetness

  }

} 

#endif