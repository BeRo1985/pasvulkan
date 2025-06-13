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
  const in float rainTime,
  const in float rainSpeed
){

  normal = vec4(0.0, 0.0, 1.0, 0.0); // Initialize output normal

  // Not optimal yet, just the foundation for wetness application in PBR as first version.
  if(wetness.x > 0.0){

    // Calculate the scaled position and its derivatives for triplanar mapping
    vec3 scaledPosition = position * 0.25; // Scale position for triplanar mapping
    vec3 dpdx = dFdx(scaledPosition), dpdy = dFdy(scaledPosition); // Calculate derivatives for texture gradients

    // Calculate the tangent space basis from the wetness normal
    // wetness.yzw is the normal from the ground, we need to calculate the tangent and bitangent vectors
    // to create a tangent space basis for triplanar mapping
    vec3 n = wetness.yzw, t = n.yzx - n.zxy, b = normalize(cross(n, t = normalize(t - dot(t, n))));
    mat3 tbn = mat3x3(t, b, n); 

#define USE_PBR_WETNESS_BIPLANAR 1
#if USE_PBR_WETNESS_BIPLANAR
    vec3 absNormal = abs(n);
    ivec3 majorAxis = ((absNormal.x > absNormal.y) && (absNormal.x > absNormal.z)) ? ivec3(0, 1, 2) : ((absNormal.y > absNormal.z) ? ivec3(1, 2, 0) : ivec3(2, 0, 1));
    ivec3 minorAxis = ((absNormal.x < absNormal.y) && (absNormal.x < absNormal.z)) ? ivec3(0, 1, 2) : ((absNormal.y < absNormal.z) ? ivec3(1, 2, 0) : ivec3(2, 0, 1));
    ivec3 medianAxis = (ivec3(3) - minorAxis) - majorAxis;
    vec2 biplanarWeights = pow(clamp((vec2(normal[majorAxis.x], normal[medianAxis.x]) - vec2(0.5773)) / vec2(1.0 - 0.5773), vec2(0.0), vec2(1.0)), vec2(8.0 * 0.125));
    float totalWeight = biplanarWeights.x + biplanarWeights.y;
    if(totalWeight > 0.0){
      biplanarWeights /= totalWeight; // Normalize the weights
    } else {
      biplanarWeights = vec2(1.0 / 2.0); // Default weights if total weight is zero
    }
    #define PBR_WETNESS_FETCH_TEXTURE(source, pos) \
      ( \
        (textureGrad(source, vec2(pos[majorAxis.y], pos[majorAxis.z]), vec2(dpdx[majorAxis.y], dpdx[majorAxis.z]), vec2(dpdy[majorAxis.y], dpdy[majorAxis.z])) * biplanarWeights.x) + \
        (textureGrad(source, vec2(pos[medianAxis.y], pos[medianAxis.z]), vec2(dpdx[medianAxis.y], dpdx[medianAxis.z]), vec2(dpdy[medianAxis.y],dpdy[medianAxis.z])) * biplanarWeights.y) \
      ) 
    #define PBR_WETNESS_FETCH_TEXTURE_CHANNEL(source, pos, c) \
      dot( \
        vec2( \
          textureGrad(source, vec2(pos[majorAxis.y], pos[majorAxis.z]), vec2(dpdx[majorAxis.y], dpdx[majorAxis.z]), vec2(dpdy[majorAxis.y], dpdy[majorAxis.z]))[c], \
          textureGrad(source, vec2(pos[medianAxis.y], pos[medianAxis.z]), vec2(dpdx[medianAxis.y], dpdx[medianAxis.z]), vec2(dpdy[medianAxis.y],dpdy[medianAxis.z]))[c] \
        ), \
        biplanarWeights \
      )
#else
    // Calculate the triplanar weights based on the tangent space basis
    vec3 triplanarWeights = abs(tbn[2]);
    {
      // pow(triplanarWeights, vec3(8.0) => 3x sq 
      triplanarWeights *= triplanarWeights; 
      triplanarWeights *= triplanarWeights; 
      triplanarWeights *= triplanarWeights; 
    }
    float totalWeight = dot(triplanarWeights, vec3(1.0));
    if(totalWeight > 0.0){
      triplanarWeights /= totalWeight; // Normalize the weights
    } else {
      triplanarWeights = vec3(1.0 / 3.0); // Default weights if total weight is zero
    }
    #define PBR_WETNESS_FETCH_TEXTURE(source, pos) \
      ( \
        (textureGrad(source, pos.yz, dpdx.yz, dpdy.yz).xyz * triplanarWeights.x) + \
        (textureGrad(source, pos.zx, dpdx.zx, dpdy.zx).xyz * triplanarWeights.y) + \
        (textureGrad(source, pos.xy, dpdx.xy, dpdy.xy).xyz * triplanarWeights.z) \ 
      )
    #define PBR_WETNESS_FETCH_TEXTURE_CHANNEL(source, pos, c) \
      dot( \
        vec3( \
          textureGrad(source, pos.yz, dpdx.yz, dpdy.yz)[c], \
          textureGrad(source, pos.zx, dpdx.zx, dpdy.zx)[c], \
          textureGrad(source, pos.xy, dpdx.xy, dpdy.xy)[c] \
        ), \
        triplanarWeights \
      )
#endif

    float normalDotGround = dot(tangentSpaceBasis[2], tbn[2]); // Dot product with ground normal to determine wetness effect on normal

    // UV mapping for puddles effect
    vec2 puddlesUV = vec2(
      dot(tbn[0], scaledPosition), 
      dot(tbn[1], scaledPosition)
    );

    // Calculate fractional time for puddles effect
    vec2 fracPuddleTimes = fract(vec2(vec2(rainTime * rainSpeed) + vec2(0.0, 0.5))); // Offset for staggered puddles effect

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

    // Calculate the rain streaks effect
    float underRoof = smoothstep(-0.3, 0.0, normalDotGround);
    float streaks = PBR_WETNESS_FETCH_TEXTURE_CHANNEL(rainTexture, scaledPosition, 1) * // Get rain streaks from the texture
                    (1.0 - clamp((normalDotGround - 0.05) * 20.0, 0.0, 1.0)) * // Streaks not on top, only on sides 
                    wetness.x; // Apply wetness factor to streaks, no rain, no streaks
    vec3 offsetedPosition = scaledPosition + (tbn[2] * (rainTime * rainSpeed * 0.2));
    streaks = smoothstep(
      0.0,
      0.1,
      streaks * 
      clamp(
        PBR_WETNESS_FETCH_TEXTURE_CHANNEL(rainTexture, offsetedPosition, 2) - 0.5, // Adjust the threshold for rain streaks   
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
        fma(PBR_WETNESS_FETCH_TEXTURE(rainStreakNormalTexture, scaledPosition).xyz, vec3(2.0), vec3(-1.0)),
        streaks // Apply streaks effect
      );           
    normal = vec4(rainNormal, 1.0); // Set the normal vector for normal mapping

    // Calculate the wetness and rain factors
    float wet = 1.0 - (clamp(wetness.x, 0.0, 1.0) * 0.66); // Clamp wetness factor to [0.0, 1.0]
    float rain = clamp(puddles.w + streaks, 0.0, 1.0); // Get the maximum effect of puddles and streaks

    // Calculate the color based on wetness and metallic factor
    vec3 dryColor = albedo.xyz;
    vec3 wetColor = mix(dryColor * wet, dryColor, metallic); // Darken the albedo color based on wetness, if not metallic, keep the original color
    wetColor = mix(dryColor, wetColor, underRoof); 
    albedo.xyz = wetColor; // Set the albedo color based on wetness and rain effect

    // Apply wetness to metallic and roughness
    metallic = mix(metallic, 0.0, rain); // Decrease metallic based on rain and wetness
    roughness = mix(roughness * wet, 0.0, rain); // Apply wetness to roughness based on rain effect

  }

} 

#endif