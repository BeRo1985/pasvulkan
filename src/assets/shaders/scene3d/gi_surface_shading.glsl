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
// Canonical inputs - set by BOTH includers: giWorldPosition, giNormal, giViewDirection (vec3); giBaseColor (vec3); giF0Dielectric (vec3);
// giMetallic, giRoughness (perceptual), giSpecularWeight, giDiffuseOcclusion, giSpecularOcclusion (float). Plus the
// dominant-light doSingleLight() inputs: giF90, giF90Dielectric (vec3); giRefractiveAngle, giTransparency, giAlphaRoughness
// (float); giSheenColor, giClearcoatNormal, giClearcoatFresnel (vec3); giSheenRoughness, giClearcoatFactor, giClearcoatRoughness
// (float) - planet binds material-neutral values for these (no sheen / clearcoat).
// MESH_FRAGMENT-only inputs (used solely by the extra-lobe blocks): giFlags (uint); giNdotV (float); giIridescenceFresnelMetallic,
// giIridescenceFresnelDielectric (vec3); giIridescenceFactor (float). The transmission terms (MESH_FRAGMENT only) additionally
// use mesh's own native locals directly (transmissionFactor, ior, volume*/diffuseTransmission* …), since they only compile there.

{
  // Environment-IBL residual weights, split into diffuse and specular: 0 for the GI-volume modes (the volume supplies the
  // indirect; CRH overrides the specular weight below for its roughness crossfade, CVCT re-enables both below for its env fill),
  // 1 for the pure environment-IBL path.
#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS) || defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING) || defined(GLOBAL_ILLUMINATION_DUGI)
  float giResidualIBLDiffuseWeight = 0.0;
  float giResidualIBLSpecularWeight = 0.0;
#else
  float giResidualIBLDiffuseWeight = 1.0;
  float giResidualIBLSpecularWeight = 1.0;
#endif
  // Final environment-IBL gate applied to the whole env result. The per-mode env reduction now lives in the residual weights
  // above (CRH's specular crossfade, CVCT's cone coverage) and DUGI skips the env block entirely, so this currently stays 1.0.
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
    globalIlluminationVolumeLookUp(crhVolumeSphericalHarmonics, giWorldPosition, vec3(0.0), giNormal);
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
                  giViewDirection,                                          //
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
    vec3 crhSpecular = max(vec3(0.0), globalIlluminationGetSpecularColor(giWorldPosition, giViewDirection, giNormal, giRoughness));
    vec3 crhMetalFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giBaseColor, 1.0);
    vec3 crhMetalBRDF = crhMetalFresnel * crhSpecular;
    vec3 crhDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giF0Dielectric, giSpecularWeight);
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
      crhSheen = getIBLRadianceCharlie(giNormal, giViewDirection, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      crhAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 crhClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDirection, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
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

  // The environment IBL is the FILL for CVCT: where the cones did not fully gather (1 - cone coverage), the env shows through.
  // Each path drives its own residual weight from the cone's accumulated alpha - diffuse from the diffuse cone, specular from
  // the specular cone - so the env diffuse and env specular fade in independently. iblWeight stays 1.0 (the per-mode env
  // reduction lives in these residual weights now, not in the global gate).
  if(dot(giBaseColor, vec3(1.0)) > 1e-6){
    vec4 cvctDiffuse = cvctIndirectDiffuseLight(giWorldPosition, giNormal);
    vec3 cvctDiffuseColor = cvctDiffuse.xyz * mix(giBaseColor, vec3(0.0), giMetallic) * giDiffuseOcclusion * OneOverPI;
    colorOutput += cvctDiffuseColor;
    giDebugGIDiffuse += cvctDiffuseColor;
    giResidualIBLDiffuseWeight = clamp(1.0 - cvctDiffuse.w, 0.0, 1.0); // env diffuse fills where the diffuse cones did not gather
  }else{
    giResidualIBLDiffuseWeight = 1.0; // no diffuse color, so the env diffuse fills the whole diffuse lobe
  }
  if(dot(giF0Dielectric, vec3(1.0)) > 1e-6){
    giResidualIBLSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_HI, GI_GLOSSY_ROUGHNESS_LO, giRoughness); // 1 = sharp (env reflection), 0 = rough (local SH reflection)
    vec4 cvctSpecular = cvctIndirectSpecularLight(giWorldPosition, giNormal, giViewDirection, cvctRoughnessToVoxelConeTracingApertureAngle(giRoughness), 1e+24);
    vec3 cvctSpecularColor = cvctSpecular.xyz * giF0Dielectric * giSpecularOcclusion * OneOverPI * (1.0 - giResidualIBLSpecularWeight); // rough side of the crossfade (env-IBL below is the sharp side)
    colorOutput += cvctSpecularColor;
    giDebugGISpecular += cvctSpecularColor;
    giResidualIBLSpecularWeight = mix(clamp(1.0 - cvctSpecular.w, 0.0, 1.0), 1.0, giResidualIBLSpecularWeight); // env specular fills where the specular cone did not gather
  }else{
    giResidualIBLSpecularWeight = 1.0; // no specular color, so the env specular fills the whole specular lobe
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
    float dugiSkyVisibilityDiffuse;
    DUGI_SH_TYPE dugiRadianceSH = dugiSampleRadianceSH(giWorldPosition, giNormal, giViewDirection, dugiSkyVisibilityDiffuse); // SH radiance field + sky-visibility along the normal (diffuse hemisphere)
    vec3 shDominantDirectionalLightColor, shDominantDirectionalLightDirection;

    // Dominant directional light extraction + residual SH probe diffuse (mirrors the CRH path)
#ifdef GI_DUGI_SH_APPROXIMATE_DOMINANT
    // Approximate dominant directional light + residual SH (DC kept), applied to L1 and L2.
#if GI_DUGI_STORAGE == GI_DUGI_STORAGE_L2_VALUE
    SHC3CoefficientsL1ApproximateDirectionalLight(SHC3CoefficientsL1FromL2(dugiRadianceSH), shDominantDirectionalLightDirection, shDominantDirectionalLightColor);
#else
    SHC3CoefficientsL1ApproximateDirectionalLight(dugiRadianceSH, shDominantDirectionalLightDirection, shDominantDirectionalLightColor);
#endif
    DUGI_SH_TYPE shResidual = DUGI_SH_SUB(dugiRadianceSH, DUGI_SH_PROJECT(shDominantDirectionalLightDirection, shDominantDirectionalLightColor));
    vec3 shResidualDiffuse = max(vec3(0.0), DUGI_SH_EVALUATE(DUGI_SH_CONVOLVE_COSINE(shResidual), giNormal));
    vec3 dugiProbeDiffuse = giDiffuseColor * shResidualDiffuse * OneOverPI; // residual; the dominant light's diffuse is added by doSingleLight below
#else
    // Alternative: native extract-and-subtract -> uniform ambient + DC-zeroed residual + dominant light.
    vec3 shAmbient;
    float shModifiedSqrtRoughness;
    DUGI_SH_EXTRACT_DOMINANT(dugiRadianceSH, shAmbient, shDominantDirectionalLightDirection, shDominantDirectionalLightColor, sqrt(clamp(giRoughness, 0.0, 1.0)), shModifiedSqrtRoughness);
    vec3 shResidualDiffuse = max(vec3(0.0), DUGI_SH_EVALUATE(DUGI_SH_CONVOLVE_COSINE(dugiRadianceSH), giNormal));
    vec3 dugiProbeDiffuse = giDiffuseColor * fma(shResidualDiffuse, vec3(OneOverPI), max(vec3(0.0), shAmbient));
    DUGI_SH_TYPE shResidual = dugiRadianceSH; // extract-and-subtract leaves the residual (DC-zeroed) field in dugiRadianceSH
#endif

    // Diffuse: SH probe (residual + dominant) <=> IBL + transmission, mirroring the oct path. The residual <=> IBL part is the
    // value dugiDiffuse (occlusion baked, direct-add); the dominant light's diffuse is added separately by doSingleLight below,
    // both weighted (1 - skyVisDiffuse) against the IBL diffuse's skyVisDiffuse.
    float dugiDiffuseWeight = 1.0 - dugiSkyVisibilityDiffuse;
    vec3 dugiIBLDiffuse = getIBLDiffuse(giNormal) * mix(giBaseColor, vec3(0.0), giMetallic) * giDiffuseOcclusion;
    vec3 dugiDiffuse = mix(dugiProbeDiffuse, dugiIBLDiffuse, dugiSkyVisibilityDiffuse); // residual <=> IBL (the dominant is added via doSingleLight)
#if defined(MESH_FRAGMENT)
    // Diffuse transmission - back side (-normal), probe <=> IBL blended by the back-side hemisphere sky-visibility (mirrors oct).
    if((giFlags & (1u << 16u)) != 0u){
      float dugiSkyVisibilityDiffuseBack;
      vec3 dugiProbeDiffuseTransmission = (dugiSampleIrradiance(giWorldPosition, -giNormal, giViewDirection, dugiSkyVisibilityDiffuseBack) * OneOverPI) * diffuseTransmissionColorFactor * giDiffuseOcclusion;
      vec3 dugiIBLDiffuseTransmission = getIBLDiffuse(-giNormal) * diffuseTransmissionColorFactor * giDiffuseOcclusion;
      vec3 dugiDiffuseTransmission = mix(dugiProbeDiffuseTransmission, dugiIBLDiffuseTransmission, dugiSkyVisibilityDiffuseBack);
      if((giFlags & (1u << 12u)) != 0u){
        dugiDiffuseTransmission = applyVolumeAttenuation(dugiDiffuseTransmission, diffuseTransmissionThickness, volumeAttenuationColor, volumeAttenuationDistance);
      }
      dugiDiffuse = mix(dugiDiffuse, dugiDiffuseTransmission, diffuseTransmissionFactor);
      dugiDiffuseWeight *= 1.0 - diffuseTransmissionFactor; // the dominant light's diffuse (doSingleLight below) is transmitted by the same amount
    }
#if defined(TRANSMISSION)
    // Transmission
    if((giFlags & (1u << 11u)) != 0u){
      vec3 dugiSpecularTransmission = getIBLVolumeRefraction(giNormal,
                                                            giViewDirection,
                                                            giRoughness,
                                                            giBaseColor,
                                                            giWorldPosition,
                                                            ior,
                                                            volumeThickness,
                                                            volumeAttenuationColor,
                                                            volumeAttenuationDistance,
                                                            volumeDispersion) * giDiffuseOcclusion;
      dugiDiffuse = mix(dugiDiffuse, dugiSpecularTransmission, transmissionFactor);
      dugiDiffuseWeight *= 1.0 - transmissionFactor; // the dominant light's diffuse (doSingleLight below) is transmitted by the same amount
    }
#endif
#endif
    colorOutput += dugiDiffuse;
    giDebugGIDiffuse += dugiDiffuse;

    // Specular
    float dugiSpecularWeight = 0.0; // dominant-light specular share (set below); 0 in the RSM pass (no specular)
#if !defined(REFLECTIVESHADOWMAPOUTPUT)
    // Layer 1 (DUGI-oct GlossyProbe<=>IBL): local probe reflection <=> env sky by sky-visibility along R.
    vec3 dugiReflectionVector = normalize(reflect(-giViewDirection, giNormal));
    float dugiSkyVisibilitySpecular;
    vec3 dugiBroadReflection = dugiSampleIrradiance(giWorldPosition, dugiReflectionVector, giViewDirection, dugiSkyVisibilitySpecular) * OneOverPI; // broad probe reflection + sky-visibility along R
    dugiSkyVisibilitySpecular *= smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness);
#if defined(GI_DUGI_GLOSSY_RESIDUAL) && defined(GI_DUGI_GLOSSY_RADIANCE)
    vec3 dugiProbeReflection = mix(dugiSampleGlossyRadiance(giWorldPosition, giNormal, dugiReflectionVector, giViewDirection), dugiBroadReflection, smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness)); // sharp atlas <-> broad
#else
    vec3 dugiProbeReflection = max(vec3(0.0), DUGI_SH_EVALUATE(dugiRadianceSH, dugiReflectionVector)); // local SH reflection along R (no atlas)
#endif
    vec3 indirectSpecular = mix(dugiProbeReflection, getIBLRadianceGGX(giNormal, giViewDirection, giRoughness), dugiSkyVisibilitySpecular);
    // Layer 2 (top crossfade): dominant-light specular <=> the indirect reflection by roughness. 1 = sharp (indirect), 0 = rough (dominant).
    float giResidualProbeOrIBLSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_HI, GI_GLOSSY_ROUGHNESS_LO, giRoughness);
    vec3 dugiSpecularMetal = indirectSpecular;
    vec3 dugiSpecularDielectric = dugiSpecularMetal;
    vec3 dugiMetalFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giBaseColor, 1.0);
    vec3 dugiMetalBRDF = dugiMetalFresnel * dugiSpecularMetal;
    vec3 dugiDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giF0Dielectric, giSpecularWeight);
    vec3 dugiDielectricBRDF = dugiSpecularMetal * giSpecularOcclusion * dugiDielectricFresnel; // specular-only (diffuse added above)
#if defined(MESH_FRAGMENT)
    if((giFlags & (1u << 10u)) != 0u){ // iridescence
      dugiMetalBRDF = mix(dugiMetalBRDF, dugiSpecularMetal * giIridescenceFresnelMetallic, giIridescenceFactor);
      dugiDielectricBRDF = mix(dugiDielectricBRDF, dugiSpecularMetal * giSpecularOcclusion * giIridescenceFresnelDielectric, giIridescenceFactor);
    }
#endif

    vec3 specularColor = mix(dugiDielectricBRDF, dugiMetalBRDF * giSpecularOcclusion, giMetallic); // dielectric / metallic mix

#if defined(MESH_FRAGMENT)
    vec3 dugiSheen = vec3(0.0);
    float dugiAlbedoSheenScaling = 1.0;
    if((giFlags & (1u << 7u)) != 0u){ // sheen
      dugiSheen = getIBLRadianceCharlie(giNormal, giViewDirection, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      dugiAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 dugiClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDirection, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
    specularColor = fma(specularColor, vec3(dugiAlbedoSheenScaling), dugiSheen);                   // sheen modulation
    specularColor = mix(specularColor, dugiClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
#endif

    specularColor *= giResidualProbeOrIBLSpecularWeight; // indirect's share (sharp side) of the dominant <=> indirect crossfade
    colorOutput += specularColor;
    giDebugGISpecular += specularColor;
    dugiSpecularWeight = 1.0 - giResidualProbeOrIBLSpecularWeight; // dominant's share (rough side), applied via doSingleLight below

#endif

    doSingleLight(shDominantDirectionalLightColor,                    //
                  vec3(giSpecularOcclusion),                          //
                  vec2(dugiDiffuseWeight, dugiSpecularWeight),        // dominant: diffuse x (1 - skyVisDiffuse), specular x (1 - giResidualProbeOrIBLSpecularWeight)
                  -shDominantDirectionalLightDirection,               //
                  giNormal,                                           //
                  giBaseColor,                                        //
                  giF0Dielectric,                                     //
                  giF90,                                              //
                  giF90Dielectric,                                    //
                  giViewDirection,                                    //
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

    // Diffuse: probe (local) <=> IBL, blended by the hemisphere sky-visibility (a probe sample along the normal)
    // occluded points keep the local probe diffuse, points open to the sky fade to the cleaner IBL diffuse. Diffuse occlusion
    // is applied ONCE by the split-sum below (like the env-IBL block), so it is not folded in here.
    float dugiSkyVisibilityDiffuse;
    vec3 dugiProbeDiffuse = (dugiSampleIrradiance(giWorldPosition, giNormal, giViewDirection, dugiSkyVisibilityDiffuse) * OneOverPI) * mix(giBaseColor, vec3(0.0), giMetallic);
    vec3 dugiIBLDiffuse = getIBLDiffuse(giNormal) * mix(giBaseColor, vec3(0.0), giMetallic);
    vec3 dugiDiffuse = mix(dugiProbeDiffuse, dugiIBLDiffuse, dugiSkyVisibilityDiffuse);
  #if defined(MESH_FRAGMENT)
    // Diffuse transmission - back side (-normal), probe <=> IBL blended by the back-side hemisphere sky-visibility.
    if((giFlags & (1u << 16u)) != 0u){
      float dugiSkyVisibilityDiffuseBack;
      vec3 dugiProbeDiffuseTransmission = (dugiSampleIrradiance(giWorldPosition, -giNormal, giViewDirection, dugiSkyVisibilityDiffuseBack) * OneOverPI) * diffuseTransmissionColorFactor;
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
                                                             giViewDirection,
                                                             giRoughness,
                                                             giBaseColor,
                                                             giWorldPosition,
                                                             ior,
                                                             volumeThickness,
                                                             volumeAttenuationColor,
                                                             volumeAttenuationDistance,
                                                             volumeDispersion);
      dugiDiffuse = mix(dugiDiffuse, dugiSpecularTransmission, transmissionFactor);
    }
  #endif
  #endif
    // Specular: probe reflection <=> env sky, blended by the sky-visibility along R
    // probe reflection = the prefiltered glossy atlas (sharp) fading to the broad probe reflection, or - without the atlas -
    // just the broad probe reflection; then crossfaded against the environment map (occluded/sharp -> probe, open+rough -> sky).
    vec3 dugiReflectionVector = normalize(reflect(-giViewDirection, giNormal));
    float dugiSkyVisibilitySpecular;
    vec3 dugiBroadReflection = dugiSampleIrradiance(giWorldPosition, dugiReflectionVector, giViewDirection, dugiSkyVisibilitySpecular) * OneOverPI; // broad reflection + sky-visibility along R
    dugiSkyVisibilitySpecular *= smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness);
  #if defined(GI_DUGI_GLOSSY_RESIDUAL) && defined(GI_DUGI_GLOSSY_RADIANCE)
    vec3 dugiProbeReflection = mix(dugiSampleGlossyRadiance(giWorldPosition, giNormal, dugiReflectionVector, giViewDirection), dugiBroadReflection, smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness)); // sharp atlas <-> broad
  #else
    vec3 dugiProbeReflection = dugiBroadReflection; // no glossy atlas: just the broad probe reflection
  #endif
    vec3 dugiSpecularMetal = mix(dugiProbeReflection, getIBLRadianceGGX(giNormal, giViewDirection, giRoughness), dugiSkyVisibilitySpecular);
    vec3 dugiSpecularDielectric = dugiSpecularMetal;
    vec3 dugiMetalFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giBaseColor, 1.0);
    vec3 dugiMetalBRDF = dugiMetalFresnel * dugiSpecularMetal;
    vec3 dugiDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giF0Dielectric, giSpecularWeight);
    vec3 dugiDielectricBRDF = mix(dugiDiffuse * giDiffuseOcclusion, dugiSpecularDielectric * giSpecularOcclusion, dugiDielectricFresnel);
  #if defined(MESH_FRAGMENT)
    if((giFlags & (1u << 10u)) != 0u){ // iridescence
      dugiMetalBRDF = mix(dugiMetalBRDF, dugiSpecularMetal * giIridescenceFresnelMetallic, giIridescenceFactor);
      dugiDielectricBRDF = mix(dugiDielectricBRDF, rgbMix(dugiDiffuse * giDiffuseOcclusion, dugiSpecularDielectric * giSpecularOcclusion, giIridescenceFresnelDielectric), giIridescenceFactor);
    }
    vec3 dugiSheen = vec3(0.0);
    float dugiAlbedoSheenScaling = 1.0;
    if((giFlags & (1u << 7u)) != 0u){ // sheen
      dugiSheen = getIBLRadianceCharlie(giNormal, giViewDirection, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
      dugiAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
    }
    vec3 dugiClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDirection, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
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
  }

  #endif

#else

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Environment-IBL only: no GI-volume, so the full diffuse + specular comes from the environment reflection (split-sum).
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Environment IBL (split-sum). DUGI is fully self-contained (it folds the env reflection into its own probe<=>IBL blend in
  // the DUGI scope above), so the env-IBL block is skipped entirely for DUGI — only CRH / CVCT / pure-environment paths use it.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if !defined(REFLECTIVESHADOWMAPOUTPUT)
#if !defined(GLOBAL_ILLUMINATION_DUGI)
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
                                                          giViewDirection,
                                                          giRoughness,
                                                          giBaseColor,
                                                          giWorldPosition,
                                                          ior,
                                                          volumeThickness,
                                                          volumeAttenuationColor,
                                                          volumeAttenuationDistance,
                                                          volumeDispersion);
    iblDiffuse = mix(iblDiffuse, iblSpecularTransmission, transmissionFactor);
  }
#endif
#endif

  vec3 iblSpecularMetal = (giResidualIBLSpecularWeight > 0.0) ? (getIBLRadianceGGX(giNormal, giViewDirection, giRoughness) * giResidualIBLSpecularWeight) : vec3(0.0);
  vec3 iblSpecularDielectric = iblSpecularMetal;
  vec3 iblMetalFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giBaseColor, 1.0);
  vec3 iblMetalBRDF = iblMetalFresnel * iblSpecularMetal;
  vec3 iblDielectricFresnel = getIBLGGXFresnel(giNormal, giViewDirection, giRoughness, giF0Dielectric, giSpecularWeight);
  vec3 iblDielectricBRDF = mix(iblDiffuse * giDiffuseOcclusion, iblSpecularDielectric * giSpecularOcclusion, iblDielectricFresnel);
#if defined(MESH_FRAGMENT)
  if((giFlags & (1u << 10u)) != 0u){ // iridescence
    iblMetalBRDF = mix(iblMetalBRDF, iblSpecularMetal * giIridescenceFresnelMetallic, giIridescenceFactor);
    iblDielectricBRDF = mix(iblDielectricBRDF, rgbMix(iblDiffuse * giDiffuseOcclusion, iblSpecularDielectric * giSpecularOcclusion, giIridescenceFresnelDielectric), giIridescenceFactor);
  }
  vec3 iblSheen = vec3(0.0);
  float iblAlbedoSheenScaling = 1.0;
  if((giFlags & (1u << 7u)) != 0u){ // sheen
    iblSheen = getIBLRadianceCharlie(giNormal, giViewDirection, giSheenRoughness, giSheenColor) * giDiffuseOcclusion;
    iblAlbedoSheenScaling = 1.0 - (max(max(giSheenColor.x, giSheenColor.y), giSheenColor.z) * albedoSheenScalingLUT(giNdotV, giSheenRoughness));
  }
  vec3 iblClearcoatBRDF = ((giFlags & (1u << 8u)) != 0u) ? (getIBLRadianceGGX(giClearcoatNormal, giViewDirection, giClearcoatRoughness) * giDiffuseOcclusion) : vec3(0.0);
#endif
  vec3 iblResultColor = mix(iblDielectricBRDF, iblMetalBRDF * giSpecularOcclusion, giMetallic); // dielectric / metallic mix
#if defined(MESH_FRAGMENT)
  iblResultColor = fma(iblResultColor, vec3(iblAlbedoSheenScaling), iblSheen);                  // sheen modulation
  iblResultColor = mix(iblResultColor, iblClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
#endif
  colorOutput += iblResultColor * iblWeight; // final whole-result env gate (currently 1.0 on every path; the per-mode env reduction lives in the residual weights above)
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
