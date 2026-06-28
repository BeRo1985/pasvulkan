// Central global-illumination + environment-IBL surface shading, shared by mesh.frag and the planet terrain / grass passes
// (planet_water.frag stays a special case with its own reflection/refraction handling). This file is an INLINE STATEMENT
// BLOCK, not a set of declarations — it is #included at the point in the fragment shader where the analytic direct lighting
// has been accumulated into colorOutput, and it adds the GI-volume + environment-IBL indirect on top.
//
// The includer must, BEFORE the #include:
//   - hold the analytic direct lighting so far in `colorOutput` (vec3),
//   - declare the debug accumulators (the giDebugDisplay bit source differs: mesh = drawFlags, planet = flags):
//       uint giDebugDisplay; vec3 giDebugGIDiffuse, giDebugGISpecular, giDebugIBLDiffuse, giDebugIBLSpecular, giDebugDirectLight;
//   - bind the canonical gi* surface inputs, and
//   - #define exactly one of MESH_FRAGMENT / PLANET_FRAGMENT (and #undef it after the include).
//
// Canonical inputs (set by the includer): giWorldPos, giNormal, giViewDir (vec3); giBaseColor (vec3); giF0Dielectric (vec3);
// giMetallic, giRoughness (perceptual), giSpecularWeight, giDiffuseOcclusion, giSpecularOcclusion (float); giFlags (uint).
// MESH_FRAGMENT additionally binds the extra-lobe inputs: giSheenColor, giClearcoatNormal, giClearcoatFresnel (vec3);
// giSheenRoughness, giClearcoatRoughness, giClearcoatFactor, giNdotV (float); giIridescenceFresnelMetallic,
// giIridescenceFresnelDielectric (vec3); giIridescenceFactor (float). The dominant-light doSingleLight() call and the
// transmission terms (MESH_FRAGMENT only) additionally use the shader's own native locals directly (F90, refractiveAngle,
// transmissionFactor, ior, volume*/diffuseTransmission* …), since they only ever compile in mesh.frag.

{
  // Environment-IBL residual weights, split into diffuse and specular: 0 for the GI-volume modes (the volume supplies the
  // indirect; CRH overrides the specular weight below for its roughness crossfade), 1 for the pure environment-IBL path.
#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS) || defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING) || defined(GLOBAL_ILLUMINATION_DUGI)
  float giResidualIBLDiffuseWeight = 0.0;
  float giResidualIBLSpecularWeight = 0.0;
#else
  float giResidualIBLDiffuseWeight = 1.0;
  float giResidualIBLSpecularWeight = 1.0;
#endif
  // Final environment-IBL gate: the CVCT path lowers it by the cone diffuse occlusion, the DUGI path by the probe
  // sky-visibility; 1.0 everywhere else.
  float iblWeight = 1.0;

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // GI-volume contribution (per technique)
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Cascaded radiance hints (CRH): SH-encoded indirect diffuse + specular from the radiance-hints volume
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  {
    vec3 giDiffuseColor = mix(giBaseColor, vec3(0.0), giMetallic) * giDiffuseOcclusion;
    vec3 crhVolumeSphericalHarmonics[9];
    globalIlluminationVolumeLookUp(crhVolumeSphericalHarmonics, giWorldPos, vec3(0.0), giNormal);
#if defined(MESH_FRAGMENT)
    // Extract the volume's dominant directional light and shade it analytically (sharper indirect diffuse than a pure cosine
    // decode), then add the remaining residual SH as diffuse so the total stays = the full field's diffuse.
    vec3 shAmbient = vec3(0.0), shDominantDirectionalLightColor = vec3(0.0), shDominantDirectionalLightDirection = vec3(0.0);
    globalIlluminationSphericalHarmonicsExtractAndSubtract(crhVolumeSphericalHarmonics, shAmbient, shDominantDirectionalLightColor, shDominantDirectionalLightDirection);
    vec3 shResidualDiffuse = max(vec3(0.0), globalIlluminationDecodeColor(globalIlluminationCompressedSphericalHarmonicsDecodeWithCosineLobe(giNormal, crhVolumeSphericalHarmonics)));
    giDiffuseColor *= shResidualDiffuse;
    colorOutput += giDiffuseColor;
    giDebugGIDiffuse += giDiffuseColor;
    doSingleLight(shDominantDirectionalLightColor,                    //
                  vec3(giSpecularOcclusion),                          //
                  vec2(1.0, 0.0),                                     // dominant light: diffuse only — the full-field indirect specular reflection below already covers the dominant direction
                  -shDominantDirectionalLightDirection,               //
                  giNormal,                                           //
                  giBaseColor,                                        //
                  giF0Dielectric,                                     //
                  F90,                                                //
                  F90Dielectric,                                      //
                  giViewDir,                                          //
                  refractiveAngle,                                    //
                  transparency,                                       //
                  alphaRoughness,                                     //
                  giMetallic,                                         //
                  giSheenColor,                                       //
                  giSheenRoughness,                                   //
                  giClearcoatNormal,                                  //
                  giClearcoatFresnel,                                 //
                  giClearcoatFactor,                                  //
                  giClearcoatRoughness,                               //
                  giSpecularWeight,                                   //
                  vec3(0.0),                                          //
                  0.0);
#else // PLANET_FRAGMENT
    // Simpler form: cosine-lobe decode of the full SH volume (no dominant-light extraction), metals demoted.
    if(dot(giBaseColor, vec3(1.0)) > 1e-6){
      vec3 crhDiffuse = max(vec3(0.0), globalIlluminationDecodeColor(globalIlluminationCompressedSphericalHarmonicsDecodeWithCosineLobe(giNormal, crhVolumeSphericalHarmonics))) * giDiffuseColor;
      colorOutput += crhDiffuse;
      giDebugGIDiffuse += crhDiffuse;
    }
#endif
    // Indirect specular — the ROUGH side of a roughness crossfade: the radiance-hints volume sampled along the reflection
    // vector (parallax-offset by roughness). The SHARP side comes from the environment-IBL block below (its env reflection is
    // gated by giResidualIBLSpecularWeight); this term takes the complementary (1 - weight).
    giResidualIBLSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_HI, GI_GLOSSY_ROUGHNESS_LO, giRoughness); // 1 = sharp (env reflection), 0 = rough (local SH reflection)
    vec3 crhSpecular = max(vec3(0.0), globalIlluminationGetSpecularColor(giWorldPos, giViewDir, giNormal, giRoughness));
#if defined(MESH_FRAGMENT)
    // Full split-sum BRDF + sheen / clearcoat / iridescence (same as the environment-IBL block).
    vec3 crhMetalFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giBaseColor, 1.0);
    vec3 crhMetalBRDF = crhMetalFresnel * crhSpecular;
    vec3 crhDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giF0Dielectric, giSpecularWeight);
    vec3 crhDielectricBRDF = crhSpecular * giSpecularOcclusion * crhDielectricFresnel;
    if((giFlags & (1u << 10u)) != 0u){ // iridescence
      crhMetalBRDF = mix(crhMetalBRDF, crhSpecular * giIridescenceFresnelMetallic, giIridescenceFactor);
      crhDielectricBRDF = mix(crhDielectricBRDF, crhSpecular * giSpecularOcclusion * giIridescenceFresnelDielectric, giIridescenceFactor);
    }
    vec3 crhSheen = vec3(0.0);
    float crhAlbedoSheenScaling = 1.0;
    if((giFlags & (1u << 7u)) != 0u){ // sheen
      crhSheen = getIBLRadianceCharlie(giNormal, giViewDir, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      crhAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 crhClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDir, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
    vec3 specularColor = mix(crhDielectricBRDF, crhMetalBRDF * giSpecularOcclusion, giMetallic); // dielectric / metallic mix
    specularColor = fma(specularColor, vec3(crhAlbedoSheenScaling), crhSheen);                   // sheen modulation
    specularColor = mix(specularColor, crhClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
#else // PLANET_FRAGMENT
    vec3 specularColor = crhSpecular * getIBLGGXFresnel(giNormal, giViewDir, giRoughness, mix(giF0Dielectric, giBaseColor, giMetallic), mix(giSpecularWeight, 1.0, giMetallic)) * giSpecularOcclusion;
#endif
    specularColor *= 1.0 - giResidualIBLSpecularWeight; // rough side of the crossfade (env-IBL below is the sharp side)
    colorOutput += specularColor;
    giDebugGISpecular += specularColor;
  }

#elif defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Cascaded voxel cone tracing (CVCT): cone-traced indirect diffuse + specular from the voxel grid
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Cascaded voxel cone tracing: cone-traced indirect diffuse + specular from the voxel grid (metals demoted on the diffuse).
  if(dot(giBaseColor, vec3(1.0)) > 1e-6){
    vec4 cvctDiffuse = cvctIndirectDiffuseLight(giWorldPos, giNormal);
    vec3 cvctDiffuseColor = cvctDiffuse.xyz * mix(giBaseColor, vec3(0.0), giMetallic) * giDiffuseOcclusion * OneOverPI;
    colorOutput += cvctDiffuseColor;
    giDebugGIDiffuse += cvctDiffuseColor;
#if defined(MESH_FRAGMENT)
    iblWeight = clamp(1.0 - cvctDiffuse.w, 0.0, 1.0); // suppress the (residual-weighted-off) env IBL where the cones already gathered near-field light
#endif
  }
#if defined(MESH_FRAGMENT)
  if(dot(giF0Dielectric, vec3(1.0)) > 1e-6){
    vec3 cvctSpecular = cvctIndirectSpecularLight(giWorldPos, giNormal, giViewDir, cvctRoughnessToVoxelConeTracingApertureAngle(giRoughness), 1e+24) * giF0Dielectric * giSpecularOcclusion * OneOverPI;
    colorOutput += cvctSpecular;
    giDebugGISpecular += cvctSpecular;
  }
#else // PLANET_FRAGMENT
  vec3 cvctSpecular = cvctIndirectSpecularLight(giWorldPos, giNormal, giViewDir, cvctRoughnessToVoxelConeTracingApertureAngle(giRoughness), 1e+24) * mix(giF0Dielectric, giBaseColor, giMetallic) * giSpecularOcclusion * OneOverPI;
  colorOutput += cvctSpecular;
  giDebugGISpecular += cvctSpecular;
#endif

#elif defined(GLOBAL_ILLUMINATION_DUGI)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dynamic Unified Global Illumination (DUGI): probe-field indirect diffuse + specular from the DUGI probe volume
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  vec3 giDiffuseColor = mix(giBaseColor, vec3(0.0), giMetallic) * giDiffuseOcclusion;
#if defined(MESH_FRAGMENT)
  #if GI_DUGI_STORAGE_IS_SH
  // SH storage (L1 or L2): sample the radiance SH field, extract its dominant directional light (shaded analytically by
  // doSingleLight) and add the remaining residual SH as diffuse — mirroring the CRH path. The environment-IBL block below is
  // disabled for this variant when the glossy-radiance atlas is on (see its #if guard); the specular comes from the dominant
  // light crossfaded against the glossy atlas, or (atlas off) from the env-IBL block + the local SH reflection like CRH.
  {
    float dugiSpecularWeight = 1.0; // scales the dominant-light specular; the glossy atlas / local SH reflection take the complementary 1-weight
    float dugiSkyVisibility;
    DUGI_SH_TYPE dugiRadianceSH = dugiSampleRadianceSH(giWorldPos, giNormal, giViewDir, dugiSkyVisibility);
    vec3 shDominantDirectionalLightColor, shDominantDirectionalLightDirection;
#ifdef GI_DUGI_SH_APPROXIMATE_DOMINANT
    // Approximate dominant directional light + residual SH (DC kept), applied to L1 and L2.
#if GI_DUGI_STORAGE == GI_DUGI_STORAGE_L2_VALUE
    SHC3CoefficientsL1ApproximateDirectionalLight(SHC3CoefficientsL1FromL2(dugiRadianceSH), shDominantDirectionalLightDirection, shDominantDirectionalLightColor);
#else
    SHC3CoefficientsL1ApproximateDirectionalLight(dugiRadianceSH, shDominantDirectionalLightDirection, shDominantDirectionalLightColor);
#endif
    // Residual SH = field minus the extracted dominant light, so it is not double-counted in the diffuse term.
    DUGI_SH_TYPE shResidual = DUGI_SH_SUB(dugiRadianceSH, DUGI_SH_PROJECT(shDominantDirectionalLightDirection, shDominantDirectionalLightColor));
    vec3 shResidualDiffuse = max(vec3(0.0), DUGI_SH_EVALUATE(DUGI_SH_CONVOLVE_COSINE(shResidual), giNormal));
    if(dot(giBaseColor, vec3(1.0)) > 1e-6){
      giDiffuseColor *= shResidualDiffuse * OneOverPI;
      colorOutput += giDiffuseColor;
      giDebugGIDiffuse += giDiffuseColor;
    }
#else
    // Alternative: native extract-and-subtract -> uniform ambient + DC-zeroed residual + dominant light.
    vec3 shAmbient;
    float shModifiedSqrtRoughness;
    DUGI_SH_EXTRACT_DOMINANT(dugiRadianceSH, shAmbient, shDominantDirectionalLightDirection, shDominantDirectionalLightColor, sqrt(clamp(giRoughness, 0.0, 1.0)), shModifiedSqrtRoughness);
    vec3 shResidualDiffuse = max(vec3(0.0), DUGI_SH_EVALUATE(DUGI_SH_CONVOLVE_COSINE(dugiRadianceSH), giNormal));
    if(dot(giBaseColor, vec3(1.0)) > 1e-6){
      giDiffuseColor *= fma(shResidualDiffuse, vec3(OneOverPI), max(vec3(0.0), shAmbient));
      colorOutput += giDiffuseColor;
      giDebugGIDiffuse += giDiffuseColor;
    }
    DUGI_SH_TYPE shResidual = dugiRadianceSH; // extract-and-subtract leaves the residual (DC-zeroed) field in dugiRadianceSH
#endif
#if defined(GI_DUGI_GLOSSY_RESIDUAL) && defined(GI_DUGI_GLOSSY_RADIANCE) && !defined(REFLECTIVESHADOWMAPOUTPUT)
    // Probe-field specular crossfaded by roughness against the dominant directional light: low roughness takes the sharp
    // directional glossy prefiltered-radiance atlas, high roughness the broad dominant-light specular.
    {
      vec3 dugiReflectionVector = normalize(reflect(-giViewDir, giNormal));
      dugiSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness);
      vec3 dugiGlossyRadiance = dugiSampleGlossyRadiance(giWorldPos, giNormal, dugiReflectionVector, giViewDir);
      vec3 dugiGlossyFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, mix(giF0Dielectric, giBaseColor, giMetallic), mix(giSpecularWeight, 1.0, giMetallic));
      vec3 specularColor = dugiGlossyRadiance * dugiGlossyFresnel * giSpecularOcclusion * (1.0 - dugiSpecularWeight);
      colorOutput += specularColor;
      giDebugGISpecular += specularColor;
    }
#elif !defined(REFLECTIVESHADOWMAPOUTPUT)
    // No prefiltered glossy atlas: mirror the CRH specular — dominant light diffuse only (dugiSpecularWeight = 0), and the
    // indirect specular is a roughness crossfade between the env-IBL reflection (sharp) and the local probe SH reflection
    // (rough), each through the split-sum BRDF + sheen / clearcoat / iridescence.
    dugiSpecularWeight = 0.0;
    giResidualIBLSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_HI, GI_GLOSSY_ROUGHNESS_LO, giRoughness); // 1 = sharp (env), 0 = rough (local SH)
    vec3 dugiReflectionVector = normalize(reflect(-giViewDir, giNormal));
    vec3 dugiShReflection = max(vec3(0.0), DUGI_SH_EVALUATE(dugiRadianceSH, dugiReflectionVector));
    vec3 dugiMetalFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giBaseColor, 1.0);
    vec3 dugiMetalBRDF = dugiMetalFresnel * dugiShReflection;
    vec3 dugiDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giF0Dielectric, giSpecularWeight);
    vec3 dugiDielectricBRDF = dugiShReflection * giSpecularOcclusion * dugiDielectricFresnel;
    if((giFlags & (1u << 10u)) != 0u){ // iridescence
      dugiMetalBRDF = mix(dugiMetalBRDF, dugiShReflection * giIridescenceFresnelMetallic, giIridescenceFactor);
      dugiDielectricBRDF = mix(dugiDielectricBRDF, dugiShReflection * giSpecularOcclusion * giIridescenceFresnelDielectric, giIridescenceFactor);
    }
    vec3 dugiSheen = vec3(0.0);
    float dugiAlbedoSheenScaling = 1.0;
    if((giFlags & (1u << 7u)) != 0u){ // sheen
      dugiSheen = getIBLRadianceCharlie(giNormal, giViewDir, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      dugiAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 dugiClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDir, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
    vec3 specularColor = mix(dugiDielectricBRDF, dugiMetalBRDF * giSpecularOcclusion, giMetallic); // dielectric / metallic mix
    specularColor = fma(specularColor, vec3(dugiAlbedoSheenScaling), dugiSheen);                   // sheen modulation
    specularColor = mix(specularColor, dugiClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
    specularColor *= 1.0 - giResidualIBLSpecularWeight;                                            // rough side; env-IBL block adds the sharp side
    colorOutput += specularColor;
    giDebugGISpecular += specularColor;
#endif
    doSingleLight(shDominantDirectionalLightColor,                    //
                  vec3(giSpecularOcclusion),                          //
                  vec2(1.0, dugiSpecularWeight),                      // diffuse kept full; dominant specular = dugiSpecularWeight (atlas crossfade when GI_DUGI_GLOSSY_RADIANCE, else 0)
                  -shDominantDirectionalLightDirection,               //
                  giNormal,                                           //
                  giBaseColor,                                        //
                  giF0Dielectric,                                     //
                  F90,                                                //
                  F90Dielectric,                                      //
                  giViewDir,                                          //
                  refractiveAngle,                                    //
                  transparency,                                       //
                  alphaRoughness,                                     //
                  giMetallic,                                         //
                  giSheenColor,                                       //
                  giSheenRoughness,                                   //
                  giClearcoatNormal,                                  //
                  giClearcoatFresnel,                                 //
                  giClearcoatFactor,                                  //
                  giClearcoatRoughness,                               //
                  giSpecularWeight,                                   //
                  vec3(0.0),                                          //
                  0.0);
  }

 #else // GI_DUGI_STORAGE octahedral

  // Octahedral storage: dugiSampleIrradiance returns the pre-integrated diffuse irradiance E(n) plus a sky-visibility factor.
  // The probe field replaces the environment-IBL diffuse; the env-IBL specular is kept (block below) but occluded by the probe
  // sky-visibility (sampled along the reflection vector) combined with the per-pixel AO.
  {
    float dugiSkyVisibility;
    vec3 dugiIrradiance = dugiSampleIrradiance(giWorldPos, giNormal, giViewDir, normalize(reflect(-giViewDir, giNormal)), dugiSkyVisibility);
    iblWeight = dugiSkyVisibility * (1.0 - smoothstep(GI_SPECULAR_ROUGHNESS_LO, GI_SPECULAR_ROUGHNESS_HI, giRoughness));
    if(dot(giBaseColor, vec3(1.0)) > 1e-6){
      giDiffuseColor *= dugiIrradiance * OneOverPI;
      colorOutput += giDiffuseColor;
      giDebugGIDiffuse += giDiffuseColor;
    }
  }

 #endif

 #else // PLANET_FRAGMENT

  // Planet DUGI: diffuse-irradiance form (storage-agnostic); the probe glossy is blended into the (otherwise off) env specular
  // source below, and the env-IBL term is occluded by the probe sky-visibility (sampled along the reflection vector).
  {
    float dugiSkyVisibility;
    vec3 dugiIrradiance = dugiSampleIrradiance(giWorldPos, giNormal, giViewDir, normalize(reflect(-giViewDir, giNormal)), dugiSkyVisibility);
    if(dot(giBaseColor, vec3(1.0)) > 1e-6){
      colorOutput += dugiIrradiance * giBaseColor * giDiffuseOcclusion * OneOverPI;
      giDebugGIDiffuse += dugiIrradiance * giBaseColor * giDiffuseOcclusion * OneOverPI;
    }
    iblWeight = dugiSkyVisibility * (1.0 - smoothstep(GI_SPECULAR_ROUGHNESS_LO, GI_SPECULAR_ROUGHNESS_HI, giRoughness));
  }

 #endif

#else

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Environment-IBL only: no GI-volume, so the full diffuse + specular comes from the environment reflection (split-sum).
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Environment IBL (split-sum). Disabled for the SH-DUGI + glossy-radiance variant (its specular is fully probe-derived).
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if !(defined(MESH_FRAGMENT) && defined(REFLECTIVESHADOWMAPOUTPUT)) // mesh skips the env IBL for its RSM flux pass; the planet RSM pass keeps it (preserving each shader's original behavior)
#if !(defined(MESH_FRAGMENT) && defined(GLOBAL_ILLUMINATION_DUGI) && (!defined(GLOBAL_ILLUMINATION_DUGI_OCT_STORAGE)) && defined(GI_DUGI_GLOSSY_RADIANCE))
  vec3 iblDiffuse = (giResidualIBLDiffuseWeight > 0.0) ? (getIBLDiffuse(giNormal) * giBaseColor * giResidualIBLDiffuseWeight) : vec3(0.0);

#if defined(MESH_FRAGMENT)
  // Diffuse transmission
  if((giFlags & (1u << 16u)) != 0u){
    vec3 iblDiffuseTransmission = getIBLDiffuse(-giNormal) * diffuseTransmissionColorFactor;
    if((giFlags & (1u << 12u)) != 0u){
      iblDiffuseTransmission = applyVolumeAttenuation(iblDiffuseTransmission, diffuseTransmissionThickness, volumeAttenuationColor, volumeAttenuationDistance);
    }
    iblDiffuse = mix(iblDiffuse, iblDiffuseTransmission, diffuseTransmissionFactor);
  }
#if defined(TRANSMISSION)
  // Transmission
  if((giFlags & (1u << 11u)) != 0u){
    vec3 iblSpecularTransmission = getIBLVolumeRefraction(giNormal,
                                                          giViewDir,
                                                          giRoughness,
                                                          giBaseColor,
                                                          giWorldPos,
                                                          ior,
                                                          volumeThickness,
                                                          volumeAttenuationColor,
                                                          volumeAttenuationDistance,
                                                          volumeDispersion);
    iblDiffuse = mix(iblDiffuse, iblSpecularTransmission, transmissionFactor);
  }
#endif
#endif

  vec3 iblSpecularMetal = (giResidualIBLSpecularWeight > 0.0) ? (getIBLRadianceGGX(giNormal, giViewDir, giRoughness) * giResidualIBLSpecularWeight) : vec3(0.0);
#if defined(GLOBAL_ILLUMINATION_DUGI) && defined(GI_DUGI_GLOSSY_RESIDUAL) && (defined(PLANET_FRAGMENT) || defined(GLOBAL_ILLUMINATION_DUGI_OCT_STORAGE))
  // Blend the probe-derived glossy reflection into the specular source by roughness: rough surfaces take the probe field along
  // the reflection vector (occlusion-aware local colour bleed, not the bright sky cubemap), sharp surfaces keep the
  // environment reflection (the low-res probe atlas cannot resolve a sharp reflection).
  {
    vec3 dugiReflectionVector = normalize(reflect(-giViewDir, giNormal));
    float dugiGlossySkyUnused;
    vec3 dugiGlossyRadiance = dugiSampleIrradiance(giWorldPos, dugiReflectionVector, giViewDir, dugiGlossySkyUnused) * OneOverPI; // broad reflection
#if defined(GI_DUGI_GLOSSY_RADIANCE)
    // Sharp prefiltered-radiance atlas for low roughness, fading to the broad source toward HI.
    vec3 dugiSharpGlossy = dugiSampleGlossyRadiance(giWorldPos, giNormal, dugiReflectionVector, giViewDir);
    dugiGlossyRadiance = mix(dugiSharpGlossy, dugiGlossyRadiance, smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness));
#endif
    iblSpecularMetal = mix(iblSpecularMetal, dugiGlossyRadiance, smoothstep(0.3, 0.8, giRoughness));
  }
#endif
  vec3 iblSpecularDielectric = iblSpecularMetal;
  vec3 iblMetalFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giBaseColor, 1.0);
  vec3 iblMetalBRDF = iblMetalFresnel * iblSpecularMetal;
  vec3 iblDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giF0Dielectric, giSpecularWeight);
  vec3 iblDielectricBRDF = mix(iblDiffuse * giDiffuseOcclusion, iblSpecularDielectric * giSpecularOcclusion, iblDielectricFresnel);
#if defined(MESH_FRAGMENT)
  if((giFlags & (1u << 10u)) != 0u){ // iridescence
    iblMetalBRDF = mix(iblMetalBRDF, iblSpecularMetal * giIridescenceFresnelMetallic, giIridescenceFactor);
    iblDielectricBRDF = mix(iblDielectricBRDF, rgbMix(iblDiffuse * giDiffuseOcclusion, iblSpecularDielectric * giSpecularOcclusion, giIridescenceFresnelDielectric), giIridescenceFactor);
  }
  vec3 iblSheen = vec3(0.0);
  float iblAlbedoSheenScaling = 1.0;
  if((giFlags & (1u << 7u)) != 0u){ // sheen
    iblSheen = getIBLRadianceCharlie(giNormal, giViewDir, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
    iblAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
  }
  vec3 iblClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDir, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
#endif
  vec3 iblResultColor = mix(iblDielectricBRDF, iblMetalBRDF * giSpecularOcclusion, giMetallic); // dielectric / metallic mix
#if defined(MESH_FRAGMENT)
  iblResultColor = fma(iblResultColor, vec3(iblAlbedoSheenScaling), iblSheen);                  // sheen modulation
  iblResultColor = mix(iblResultColor, iblClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
#endif
  colorOutput += iblResultColor * iblWeight; // iblWeight (1 - cone occlusion / sky-visibility) suppresses the env IBL where the GI volume already gathered near-field light; 1.0 on the pure-IBL paths
  if(giDebugDisplay != 0u){
    // Split the environment-IBL result into its diffuse and specular parts for the debug channels: the diffuse part is
    // (1 - dielectric Fresnel) of the dielectric term and only on non-metals; the remainder is the specular part.
    vec3 iblDiffusePart = (iblDiffuse * giDiffuseOcclusion) * (vec3(1.0) - iblDielectricFresnel) * (1.0 - giMetallic);
    giDebugIBLDiffuse += iblDiffusePart * iblWeight;
    giDebugIBLSpecular += (iblResultColor - iblDiffusePart) * iblWeight;
#if defined(GLOBAL_ILLUMINATION_DUGI) && (defined(PLANET_FRAGMENT) || defined(GLOBAL_ILLUMINATION_DUGI_OCT_STORAGE))
    // Octahedral DUGI folds the probe-derived glossy reflection into the environment specular above (not a separate additive
    // term like the SH / voxel cone tracing paths), so the GI-specular channel mirrors that combined specular.
    giDebugGISpecular += (iblResultColor - iblDiffusePart) * iblWeight;
#endif
  }
#endif
#endif
}
