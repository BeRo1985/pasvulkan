// Central global-illumination + environment-IBL surface shading, shared by mesh.frag and the planet terrain / grass passes
// (planet_water.frag stays a special case with its own reflection/refraction handling). This file is an INLINE STATEMENT
// BLOCK, not a set of declarations - it is #included at the point in the fragment shader where the analytic direct lighting
// has been accumulated into colorOutput, and it adds the GI-volume + environment-IBL indirect on top.
//
// The includer must, BEFORE the #include:
//   - hold the analytic direct lighting so far in `colorOutput` (vec3),
//   - declare the debug accumulators (the giDebugDisplay bit source differs: mesh = drawFlags, planet = flags):
//       uint giDebugDisplay; vec3 giDebugGIDiffuse, giDebugGISpecular, giDebugIBLDiffuse, giDebugIBLSpecular, giDebugDirectLight;
//   - bind the canonical gi* surface inputs, and
//   - #define exactly one of MESH_FRAGMENT / PLANET_FRAGMENT (and #undef it after the include).
//
// Both mesh and planet now run the SAME GI algorithm (CRH dominant-light extraction, CVCT, DUGI storage-mode path); only the
// material lobes (sheen / clearcoat / iridescence) and the transmission terms stay MESH_FRAGMENT-only, since planet terrain
// lacks those material features. The planet passes neutral material values for the shared dominant-light doSingleLight() call.
//
// Canonical inputs - set by BOTH includers: giWorldPos, giNormal, giViewDir (vec3); giBaseColor (vec3); giF0Dielectric (vec3);
// giMetallic, giRoughness (perceptual), giSpecularWeight, giDiffuseOcclusion, giSpecularOcclusion (float). Plus the
// dominant-light doSingleLight() inputs: giF90, giF90Dielectric (vec3); giRefractiveAngle, giTransparency, giAlphaRoughness
// (float); giSheenColor, giClearcoatNormal, giClearcoatFresnel (vec3); giSheenRoughness, giClearcoatFactor, giClearcoatRoughness
// (float) - planet binds material-neutral values for these (no sheen / clearcoat).
// MESH_FRAGMENT-only inputs (used solely by the extra-lobe blocks): giFlags (uint); giNdotV (float); giIridescenceFresnelMetallic,
// giIridescenceFresnelDielectric (vec3); giIridescenceFactor (float). The transmission terms (MESH_FRAGMENT only) additionally
// use mesh's own native locals directly (transmissionFactor, ior, volume*/diffuseTransmission* …), since they only compile there.

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
                  vec2(1.0, 0.0),                                     // dominant light: diffuse only - the full-field indirect specular reflection below already covers the dominant direction
                  -shDominantDirectionalLightDirection,               //
                  giNormal,                                           //
                  giBaseColor,                                        //
                  giF0Dielectric,                                     //
                  giF90,                                              //
                  giF90Dielectric,                                    //
                  giViewDir,                                          //
                  giRefractiveAngle,                                  //
                  giTransparency,                                     //
                  giAlphaRoughness,                                   //
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
    // Indirect specular - the ROUGH side of a roughness crossfade: the radiance-hints volume sampled along the reflection
    // vector (parallax-offset by roughness). The SHARP side comes from the environment-IBL block below (its env reflection is
    // gated by giResidualIBLSpecularWeight); this term takes the complementary (1 - weight).
    giResidualIBLSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_HI, GI_GLOSSY_ROUGHNESS_LO, giRoughness); // 1 = sharp (env reflection), 0 = rough (local SH reflection)
    vec3 crhSpecular = max(vec3(0.0), globalIlluminationGetSpecularColor(giWorldPos, giViewDir, giNormal, giRoughness));
    vec3 crhMetalFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giBaseColor, 1.0);
    vec3 crhMetalBRDF = crhMetalFresnel * crhSpecular;
    vec3 crhDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giF0Dielectric, giSpecularWeight);
    vec3 crhDielectricBRDF = crhSpecular * giSpecularOcclusion * crhDielectricFresnel;
#if defined(MESH_FRAGMENT)
    if((giFlags & (1u << 10u)) != 0u){ // iridescence
      crhMetalBRDF = mix(crhMetalBRDF, crhSpecular * giIridescenceFresnelMetallic, giIridescenceFactor);
      crhDielectricBRDF = mix(crhDielectricBRDF, crhSpecular * giSpecularOcclusion * giIridescenceFresnelDielectric, giIridescenceFactor);
    }
#endif
    vec3 specularColor = mix(crhDielectricBRDF, crhMetalBRDF * giSpecularOcclusion, giMetallic); // dielectric / metallic mix
#if defined(MESH_FRAGMENT)
    vec3 crhSheen = vec3(0.0);
    float crhAlbedoSheenScaling = 1.0;
    if((giFlags & (1u << 7u)) != 0u){ // sheen
      crhSheen = getIBLRadianceCharlie(giNormal, giViewDir, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      crhAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 crhClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDir, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
    specularColor = fma(specularColor, vec3(crhAlbedoSheenScaling), crhSheen);                   // sheen modulation
    specularColor = mix(specularColor, crhClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
#endif
    specularColor *= 1.0 - giResidualIBLSpecularWeight; // rough side of the crossfade (env-IBL below is the sharp side)
    colorOutput += specularColor;
    giDebugGISpecular += specularColor;
  }

#elif defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Cascaded voxel cone tracing (CVCT): cone-traced indirect diffuse + specular from the voxel grid (metals demoted on diffuse)
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  if(dot(giBaseColor, vec3(1.0)) > 1e-6){
    vec4 cvctDiffuse = cvctIndirectDiffuseLight(giWorldPos, giNormal);
    vec3 cvctDiffuseColor = cvctDiffuse.xyz * mix(giBaseColor, vec3(0.0), giMetallic) * giDiffuseOcclusion * OneOverPI;
    colorOutput += cvctDiffuseColor;
    giDebugGIDiffuse += cvctDiffuseColor;
    iblWeight = clamp(1.0 - cvctDiffuse.w, 0.0, 1.0); // suppress the (residual-weighted-off) env IBL where the cones already gathered near-field light
  }
  if(dot(giF0Dielectric, vec3(1.0)) > 1e-6){
    vec3 cvctSpecular = cvctIndirectSpecularLight(giWorldPos, giNormal, giViewDir, cvctRoughnessToVoxelConeTracingApertureAngle(giRoughness), 1e+24) * giF0Dielectric * giSpecularOcclusion * OneOverPI;
    colorOutput += cvctSpecular;
    giDebugGISpecular += cvctSpecular;
  }

#elif defined(GLOBAL_ILLUMINATION_DUGI)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dynamic Unified Global Illumination (DUGI): probe-field indirect diffuse + specular from the DUGI probe volume
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  vec3 giDiffuseColor = mix(giBaseColor, vec3(0.0), giMetallic) * giDiffuseOcclusion;
  #if GI_DUGI_STORAGE_IS_SH
  // SH storage (L1 or L2): sample the radiance SH field, extract its dominant directional light (shaded analytically by
  // doSingleLight) and add the remaining residual SH as diffuse - mirroring the CRH path. The environment-IBL block below is
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
    giDiffuseColor *= shResidualDiffuse * OneOverPI;
    colorOutput += giDiffuseColor;
    giDebugGIDiffuse += giDiffuseColor;
#else
    // Alternative: native extract-and-subtract -> uniform ambient + DC-zeroed residual + dominant light.
    vec3 shAmbient;
    float shModifiedSqrtRoughness;
    DUGI_SH_EXTRACT_DOMINANT(dugiRadianceSH, shAmbient, shDominantDirectionalLightDirection, shDominantDirectionalLightColor, sqrt(clamp(giRoughness, 0.0, 1.0)), shModifiedSqrtRoughness);
    vec3 shResidualDiffuse = max(vec3(0.0), DUGI_SH_EVALUATE(DUGI_SH_CONVOLVE_COSINE(dugiRadianceSH), giNormal));
    giDiffuseColor *= fma(shResidualDiffuse, vec3(OneOverPI), max(vec3(0.0), shAmbient));
    colorOutput += giDiffuseColor;
    giDebugGIDiffuse += giDiffuseColor;
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
    // No prefiltered glossy atlas: mirror the CRH specular - dominant light diffuse only (dugiSpecularWeight = 0), and the
    // indirect specular is a roughness crossfade between the env-IBL reflection (sharp) and the local probe SH reflection
    // (rough), each through the split-sum BRDF (+ sheen / clearcoat / iridescence for mesh).
    dugiSpecularWeight = 0.0;
    giResidualIBLSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_HI, GI_GLOSSY_ROUGHNESS_LO, giRoughness); // 1 = sharp (env), 0 = rough (local SH)
    vec3 dugiReflectionVector = normalize(reflect(-giViewDir, giNormal));
    vec3 dugiShReflection = max(vec3(0.0), DUGI_SH_EVALUATE(dugiRadianceSH, dugiReflectionVector));
    vec3 dugiMetalFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giBaseColor, 1.0);
    vec3 dugiMetalBRDF = dugiMetalFresnel * dugiShReflection;
    vec3 dugiDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giF0Dielectric, giSpecularWeight);
    vec3 dugiDielectricBRDF = dugiShReflection * giSpecularOcclusion * dugiDielectricFresnel;
#if defined(MESH_FRAGMENT)
    if((giFlags & (1u << 10u)) != 0u){ // iridescence
      dugiMetalBRDF = mix(dugiMetalBRDF, dugiShReflection * giIridescenceFresnelMetallic, giIridescenceFactor);
      dugiDielectricBRDF = mix(dugiDielectricBRDF, dugiShReflection * giSpecularOcclusion * giIridescenceFresnelDielectric, giIridescenceFactor);
    }
#endif
    vec3 specularColor = mix(dugiDielectricBRDF, dugiMetalBRDF * giSpecularOcclusion, giMetallic); // dielectric / metallic mix
#if defined(MESH_FRAGMENT)
    vec3 dugiSheen = vec3(0.0);
    float dugiAlbedoSheenScaling = 1.0;
    if((giFlags & (1u << 7u)) != 0u){ // sheen
      dugiSheen = getIBLRadianceCharlie(giNormal, giViewDir, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      dugiAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 dugiClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDir, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
    specularColor = fma(specularColor, vec3(dugiAlbedoSheenScaling), dugiSheen);                   // sheen modulation
    specularColor = mix(specularColor, dugiClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
#endif
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
                  giF90,                                              //
                  giF90Dielectric,                                    //
                  giViewDir,                                          //
                  giRefractiveAngle,                                  //
                  giTransparency,                                     //
                  giAlphaRoughness,                                   //
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
  // sky-visibility (sampled along the reflection vector) combined with the per-pixel AO. (DISABLED for now)
  {

    // Diffuse irradiance from the probe field
    float dugiSkyVisibilitySpecular;
    vec3 dugiIrradiance = dugiSampleIrradiance(giWorldPos, giNormal, giViewDir, normalize(reflect(-giViewDir, giNormal)), dugiSkyVisibilitySpecular);
    giDiffuseColor *= dugiIrradiance * OneOverPI;

  #if defined(GI_DUGI_GLOSSY_RADIANCE)

    // Weight for the crossfade the probe-field glossy radiance atlas against the IBL environment map by roughness thresholds.
    dugiSkyVisibilitySpecular *= smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness);

    // Full split-sum from the probe field: the probe irradiance is the diffuse source and the prefiltered glossy-radiance atlas
    // the specular source, run through one Fresnel-weighted dielectric / metallic mix - mirroring the environment-IBL block
    // below (which is skipped for this variant). So the indirect diffuse is correctly (1 - F)-weighted here, not added twice.
    // Probe (local) diffuse + environment (IBL) diffuse, blended by the hemisphere sky-visibility from a dedicated second probe
    // sample along the normal: occluded points keep the local probe diffuse, points open to the sky fade to the cleaner IBL
    // diffuse. Diffuse occlusion is applied ONCE by the split-sum below (like the env-IBL block), so it is not folded in here.
    float dugiSkyVisibilityDiffuse;
    vec3 dugiProbeDiffuse = (dugiSampleIrradiance(giWorldPos, giNormal, giViewDir, dugiSkyVisibilityDiffuse) * OneOverPI) * mix(giBaseColor, vec3(0.0), giMetallic);
    vec3 dugiIBLDiffuse = getIBLDiffuse(giNormal) * mix(giBaseColor, vec3(0.0), giMetallic);
    vec3 dugiDiffuse = mix(dugiProbeDiffuse, dugiIBLDiffuse, dugiSkyVisibilityDiffuse);
  #if defined(MESH_FRAGMENT)
    // Diffuse transmission - back side (-normal), probe <=> IBL blended by the back-side hemisphere sky-visibility, mirroring the front diffuse.
    if((giFlags & (1u << 16u)) != 0u){
      float dugiSkyVisibilityDiffuseBack;
      vec3 dugiProbeDiffuseTransmission = (dugiSampleIrradiance(giWorldPos, -giNormal, giViewDir, dugiSkyVisibilityDiffuseBack) * OneOverPI) * diffuseTransmissionColorFactor;
      vec3 dugiIBLDiffuseTransmission = getIBLDiffuse(-giNormal) * diffuseTransmissionColorFactor;
      vec3 dugiDiffuseTransmission = mix(dugiProbeDiffuseTransmission, dugiIBLDiffuseTransmission, dugiSkyVisibilityDiffuseBack);
      if((giFlags & (1u << 12u)) != 0u){
        dugiDiffuseTransmission = applyVolumeAttenuation(dugiDiffuseTransmission, diffuseTransmissionThickness, volumeAttenuationColor, volumeAttenuationDistance);
      }
      dugiDiffuse = mix(dugiDiffuse, dugiDiffuseTransmission, diffuseTransmissionFactor);
    }
  #if defined(TRANSMISSION)
    // Transmission
    if((giFlags & (1u << 11u)) != 0u){
      vec3 dugiSpecularTransmission = getIBLVolumeRefraction(giNormal,
                                                             giViewDir,
                                                             giRoughness,
                                                             giBaseColor,
                                                             giWorldPos,
                                                             ior,
                                                             volumeThickness,
                                                             volumeAttenuationColor,
                                                             volumeAttenuationDistance,
                                                             volumeDispersion);
      dugiDiffuse = mix(dugiDiffuse, dugiSpecularTransmission, transmissionFactor);
    }
  #endif
  #endif
    // Specular radiance
    // Specular: the probe-field glossy radiance atlas
    vec3 dugiReflectionVector = normalize(reflect(-giViewDir, giNormal));
    float dugiGlossySkyUnused;
    vec3 dugiGlossyRadiance = dugiSampleIrradiance(giWorldPos, dugiReflectionVector, giViewDir, dugiGlossySkyUnused) * OneOverPI; // broad reflection
    // Sharp prefiltered-radiance atlas for low roughness, fading to the broad source toward HI.
    vec3 dugiSharpGlossy = dugiSampleGlossyRadiance(giWorldPos, giNormal, dugiReflectionVector, giViewDir);
    // Crossfade the probe glossy reflection against the environment map by dugiSkyVisibilitySpecular: rough
    // surfaces open to the sky take the broad env reflection, occluded or sharp ones keep the local probe reflection.
    vec3 dugiSpecularMetal = mix(
                               mix(
                                 dugiSharpGlossy,
                                 dugiGlossyRadiance,
                                 smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness)
                               ),
                               getIBLRadianceGGX(giNormal, giViewDir, giRoughness),
                               dugiSkyVisibilitySpecular
                             );
    vec3 dugiSpecularDielectric = dugiSpecularMetal;
    vec3 dugiMetalFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giBaseColor, 1.0);
    vec3 dugiMetalBRDF = dugiMetalFresnel * dugiSpecularMetal;
    vec3 dugiDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDir, giRoughness, giF0Dielectric, giSpecularWeight);
    vec3 dugiDielectricBRDF = mix(dugiDiffuse * giDiffuseOcclusion, dugiSpecularDielectric * giSpecularOcclusion, dugiDielectricFresnel);
  #if defined(MESH_FRAGMENT)
    if((giFlags & (1u << 10u)) != 0u){ // iridescence
      dugiMetalBRDF = mix(dugiMetalBRDF, dugiSpecularMetal * giIridescenceFresnelMetallic, giIridescenceFactor);
      dugiDielectricBRDF = mix(dugiDielectricBRDF, rgbMix(dugiDiffuse * giDiffuseOcclusion, dugiSpecularDielectric * giSpecularOcclusion, giIridescenceFresnelDielectric), giIridescenceFactor);
    }
    vec3 dugiSheen = vec3(0.0);
    float dugiAlbedoSheenScaling = 1.0;
    if((giFlags & (1u << 7u)) != 0u){ // sheen
      dugiSheen = getIBLRadianceCharlie(giNormal, giViewDir, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      dugiAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 dugiClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDir, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
  #endif
    vec3 dugiResultColor = mix(dugiDielectricBRDF, dugiMetalBRDF * giSpecularOcclusion, giMetallic);   // dielectric / metallic mix
  #if defined(MESH_FRAGMENT)
    dugiResultColor = fma(dugiResultColor, vec3(dugiAlbedoSheenScaling), dugiSheen);                   // sheen modulation
    dugiResultColor = mix(dugiResultColor, dugiClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
  #endif
    colorOutput += dugiResultColor;
    if(giDebugDisplay != 0u){
      // Split the combined probe result into its diffuse and specular parts for the debug channels (see the env-IBL block).
      vec3 dugiDiffusePart = (dugiDiffuse * giDiffuseOcclusion) * (vec3(1.0) - dugiDielectricFresnel) * (1.0 - giMetallic);
      giDebugGIDiffuse += dugiDiffusePart;
      giDebugGISpecular += dugiResultColor - dugiDiffusePart;
    }
  #else
    // No glossy atlas: the probe diffuse is added directly and the environment-IBL block below provides the specular reflection.
    colorOutput += giDiffuseColor;
    giDebugGIDiffuse += giDiffuseColor;
    giResidualIBLSpecularWeight = 1.0;
  #endif
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

#if !defined(REFLECTIVESHADOWMAPOUTPUT)
#if !(defined(GLOBAL_ILLUMINATION_DUGI) && defined(GI_DUGI_GLOSSY_RADIANCE))
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
  }
#endif
#endif
}
