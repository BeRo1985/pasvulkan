// Central global-illumination + environment-IBL surface shading, shared by mesh.frag and the planet terrain / grass passes
// (planet_water.frag stays a special case with its own reflection/refraction handling). This file is an INLINE STATEMENT
// BLOCK, not a set of declarations - it is #included at the point in the fragment shader where the analytic direct lighting
// has been accumulated into colorOutput, and it adds the GI-volume + environment-IBL indirect on top.
//
// The includer must, BEFORE the #include:
//   - hold the analytic direct lighting so far in `colorOutput` (vec3),
//   - declare the debug accumulators (the giDebugDisplay bit source differs: mesh = drawFlags, planet = flags):
//       uint giDebugDisplay; vec3 giDebugIndirectDiffuse, giDebugIndirectSpecular, giDebugProbeInfluence, giDebugDirectLight;
//       (giDebugProbeInfluence packs .x = brightness-weighted probe luminance, .y = total indirect luminance; heatmap = x / y)
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

#undef IBL_GI_PROBES

{
  // Luminance weights for the debug probe-influence heatmap (brightness-weighted probe-vs-env blend, see the debug splits below).
  const vec3 giDebugLuminance = vec3(0.2126, 0.7152, 0.0722);

  // Environment-IBL residual weights, split into diffuse and specular: 0 for the GI-volume modes (the volume supplies the
  // indirect; CRH overrides the specular weight below for its roughness crossfade, CVCT re-enables both for its env fill, and
  // DUGI drives them from its probe crossfades), 1 for the pure environment-IBL path.
#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS) || defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING) || defined(GLOBAL_ILLUMINATION_DUGI)

  // Variables so that these can be overridden by the GI-volume modes (CRH's specular crossfade, CVCT's cone coverage,
  // DUGI's probe crossfades) and the env block can stay a single, un-gated pass.

  float giResidualIBLDiffuseWeight = 0.0;
  float giResidualIBLSpecularWeight = 0.0;

  // Final environment-IBL gate applied to the whole env result. The per-mode env reduction now lives in the residual weights
  // above (CRH / DUGI crossfades, CVCT's cone coverage), so this currently stays 1.0.
  float iblWeight = 1.0;

#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS) || defined(GLOBAL_ILLUMINATION_DUGI)
  #define IBL_GI_PROBES
  // xyz = Color, w = Weight/Factor
  vec4 iblGIProbeDiffuse = vec4(0.0);
  vec4 iblGIProbeDiffuseTransmission = vec4(0.0);
  vec4 iblGIProbeSpecular = vec4(0.0);
#endif

#else

  // Pure environment-IBL path (no GI-volume): constants so the compiler can fold this into a single block. The residual
  // weights are 1.0 (env diffuse + specular fully applied) and iblWeight is 1.0 (the env block is not gated).
  const float giResidualIBLDiffuseWeight = 1.0;
  const float giResidualIBLSpecularWeight = 1.0;

  const float iblWeight = 1.0;

#endif

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // GI-volume contribution (per technique)
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Cascaded radiance hints (CRH): SH-encoded indirect diffuse + specular from the radiance-hints volume
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  {

    vec3 crhVolumeSphericalHarmonics[9];
    globalIlluminationVolumeLookUp(crhVolumeSphericalHarmonics, giWorldPosition, vec3(0.0), giNormal);
    // Extract the volume's dominant directional light and shade it analytically (sharper indirect diffuse than a pure cosine
    // decode), then add the remaining residual SH as diffuse so the total stays = the full field's diffuse.
    vec3 shAmbient = vec3(0.0), shDominantDirectionalLightColor = vec3(0.0), shDominantDirectionalLightDirection = vec3(0.0);
    globalIlluminationSphericalHarmonicsExtractAndSubtract(crhVolumeSphericalHarmonics, shAmbient, shDominantDirectionalLightColor, shDominantDirectionalLightDirection);
    vec3 shResidualDiffuse = max(vec3(0.0), globalIlluminationDecodeColor(globalIlluminationCompressedSphericalHarmonicsDecodeWithCosineLobe(giNormal, crhVolumeSphericalHarmonics)));

    giResidualIBLDiffuseWeight = 1.0;
    giResidualIBLSpecularWeight = smoothstep(GI_BROAD_ROUGHNESS_HI, GI_BROAD_ROUGHNESS_LO, giRoughness);

    iblWeight = 1.0;

    iblGIProbeDiffuse = vec4(shResidualDiffuse, 1.0);

#if defined(MESH_FRAGMENT)
    if((giFlags & (1u << 16u)) != 0u){
      vec3 crhVolumeSphericalHarmonicsBack[9];
      globalIlluminationVolumeLookUp(crhVolumeSphericalHarmonicsBack, giWorldPosition, vec3(0.0), -giNormal);
      vec3 shAmbientBack = vec3(0.0), shDominantDirectionalLightColorBack = vec3(0.0), shDominantDirectionalLightDirectionBack = vec3(0.0);
      globalIlluminationSphericalHarmonicsExtractAndSubtract(crhVolumeSphericalHarmonicsBack, shAmbientBack, shDominantDirectionalLightColorBack, shDominantDirectionalLightDirectionBack);
      iblGIProbeDiffuseTransmission = vec4(max(vec3(0.0), globalIlluminationDecodeColor(globalIlluminationCompressedSphericalHarmonicsDecodeWithCosineLobe(-giNormal, crhVolumeSphericalHarmonicsBack))), 1.0);
    }
#endif

    iblGIProbeSpecular = vec4(max(vec3(0.0), globalIlluminationGetSpecularColor(giWorldPosition, giViewDirection, giNormal, giRoughness)), smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness));

    vec3 crhDominantDiffusePart, crhDominantSpecularPart;
    doSingleLight(shDominantDirectionalLightColor,                    //
                  vec3(giSpecularOcclusion),                          //
                  vec2(1.0, 1.0 - giResidualIBLSpecularWeight),       //
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
                  0.0,                                                //
                  crhDominantDiffusePart,                             //
                  crhDominantSpecularPart);

    // The dominant directional light is probe-derived: add its real diffuse / specular split (from doSingleLight's output
    // parameters) to the GI debug channels and count it as 100% probe in the probe-influence heatmap.
    if(giDebugDisplay != 0u){
      giDebugIndirectDiffuse += crhDominantDiffusePart;
      giDebugIndirectSpecular += crhDominantSpecularPart;
      float crhDominantLuminance = dot(crhDominantDiffusePart + crhDominantSpecularPart, giDebugLuminance);
      giDebugProbeInfluence += vec3(crhDominantLuminance, crhDominantLuminance, 0.0);
    }

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
    if(giDebugDisplay != 0u){
      giDebugIndirectDiffuse += cvctDiffuseColor;
      float cvctDiffuseLuminance = dot(cvctDiffuseColor, giDebugLuminance);
      giDebugProbeInfluence += vec3(cvctDiffuseLuminance, cvctDiffuseLuminance, 0.0); // cone-traced diffuse = 100% probe
    }
    giResidualIBLDiffuseWeight = clamp(1.0 - cvctDiffuse.w, 0.0, 1.0); // env diffuse fills where the diffuse cones did not gather
  }else{
    giResidualIBLDiffuseWeight = 1.0; // no diffuse color, so the env diffuse fills the whole diffuse lobe
  }
  if(dot(giF0Dielectric, vec3(1.0)) > 1e-6){
    giResidualIBLSpecularWeight = smoothstep(GI_SPECULAR_ROUGHNESS_HI, GI_SPECULAR_ROUGHNESS_LO, giRoughness);
    vec4 cvctSpecular = cvctIndirectSpecularLight(giWorldPosition, giNormal, giViewDirection, cvctRoughnessToVoxelConeTracingApertureAngle(giRoughness), 1e+24);
    vec3 cvctSpecularColor = cvctSpecular.xyz * giF0Dielectric * giSpecularOcclusion * OneOverPI * (1.0 - giResidualIBLSpecularWeight); // rough side of the crossfade (env-IBL below is the sharp side)
    colorOutput += cvctSpecularColor;
    if(giDebugDisplay != 0u){
      giDebugIndirectSpecular += cvctSpecularColor;
      float cvctSpecularLuminance = dot(cvctSpecularColor, giDebugLuminance);
      giDebugProbeInfluence += vec3(cvctSpecularLuminance, cvctSpecularLuminance, 0.0); // cone-traced specular = 100% probe
    }
    giResidualIBLSpecularWeight = mix(clamp(1.0 - cvctSpecular.w, 0.0, 1.0), 1.0, giResidualIBLSpecularWeight); // env specular fills where the specular cone did not gather
  }else{
    giResidualIBLSpecularWeight = 1.0; // no specular color, so the env specular fills the whole specular lobe
  }

#elif defined(GLOBAL_ILLUMINATION_DUGI)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Dynamic Unified Global Illumination (DUGI): probe-field indirect diffuse + specular from the DUGI probe volume
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if GI_DUGI_STORAGE_IS_SH
  // SH storage (L1 or L2): sample the radiance SH field, extract its dominant directional light (shaded analytically by
  // doSingleLight) and feed the residual SH diffuse + local SH / atlas reflection into the shared IBL_GI_PROBES env block
  // below - exactly like the CRH path. The env block does the probe <=> IBL crossfade (by sky-visibility), the BRDF split-sum,
  // the transmission and the debug attribution; only the dominant light's diffuse / specular shares are added here (doSingleLight).
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
    vec3 dugiRawProbeDiffuse = shResidualDiffuse * OneOverPI; // raw residual radiance (the env block applies baseColor + occlusion once); the dominant light's diffuse is added by doSingleLight below
#else
    // Alternative: native extract-and-subtract -> uniform ambient + DC-zeroed residual + dominant light.
    vec3 shAmbient;
    float shModifiedSqrtRoughness;
    DUGI_SH_EXTRACT_DOMINANT(dugiRadianceSH, shAmbient, shDominantDirectionalLightDirection, shDominantDirectionalLightColor, sqrt(clamp(giRoughness, 0.0, 1.0)), shModifiedSqrtRoughness);
    vec3 shResidualDiffuse = max(vec3(0.0), DUGI_SH_EVALUATE(DUGI_SH_CONVOLVE_COSINE(dugiRadianceSH), giNormal));
    vec3 dugiRawProbeDiffuse = fma(shResidualDiffuse, vec3(OneOverPI), max(vec3(0.0), shAmbient)); // raw residual radiance (the env block applies baseColor + occlusion once)
    DUGI_SH_TYPE shResidual = dugiRadianceSH; // extract-and-subtract leaves the residual (DC-zeroed) field in dugiRadianceSH
#endif

    // Diffuse: feed the raw residual SH radiance into the shared env block as a probe (it does probe <=> IBL by sky-visibility,
    // then baseColor, occlusion, transmission and the BRDF split-sum). The probe <=> IBL weight is 1 - sky-visibility along the
    // normal (occluded -> probe, open -> env), mirroring the oct path.
    giResidualIBLDiffuseWeight = 1.0;
    iblWeight = 1.0;
    iblGIProbeDiffuse = vec4(dugiRawProbeDiffuse, 1.0 - dugiSkyVisibilityDiffuse);

    // Dominant-light diffuse share added by doSingleLight below: 1 - sky-visibility, reduced by any transmission so the
    // dominant light is transmitted by the same amount as the rest of the diffuse.
    float dugiDiffuseWeight = 1.0 - dugiSkyVisibilityDiffuse;
#if defined(MESH_FRAGMENT)
    // Diffuse transmission - back side (-normal): feed the raw back-side probe into the env block's transmission slot; it blends
    // it against getIBLDiffuse(-N) by 1 - back sky-visibility and applies diffuseTransmissionColorFactor + volume attenuation.
    if((giFlags & (1u << 16u)) != 0u){
      float dugiSkyVisibilityDiffuseBack;
      vec3 dugiRawProbeDiffuseTransmission = dugiSampleIrradiance(giWorldPosition, -giNormal, giViewDirection, dugiSkyVisibilityDiffuseBack) * OneOverPI;
      iblGIProbeDiffuseTransmission = vec4(dugiRawProbeDiffuseTransmission, 1.0 - dugiSkyVisibilityDiffuseBack);
      dugiDiffuseWeight *= 1.0 - diffuseTransmissionFactor; // the dominant light's diffuse (doSingleLight below) is transmitted by the same amount
    }
#if defined(TRANSMISSION)
    // Transmission: the env block does the volume refraction; here only fade the dominant light's diffuse by the same amount.
    if((giFlags & (1u << 11u)) != 0u){
      dugiDiffuseWeight *= 1.0 - transmissionFactor; // the dominant light's diffuse (doSingleLight below) is transmitted by the same amount
    }
#endif
#endif

    // Specular
    float dugiSpecularWeight = 0.0; // dominant-light specular share (set below); 0 in the RSM pass (no specular)
#if !defined(REFLECTIVESHADOWMAPOUTPUT)
    // Layer 1 (probe reflection <=> env sky by sky-visibility along R): the local probe reflection (sharp glossy atlas fading
    // to the broad probe, or - without the atlas - the local SH reflection) is fed to the env block via iblGIProbeSpecular,
    // which crossfades it against getIBLRadianceGGX. The probe <=> env weight is 1 - sky-visibility along R (occluded -> probe).
    vec3 dugiReflectionVector = normalize(reflect(-giViewDirection, giNormal));
    float dugiSkyVisibilitySpecular;
    vec3 dugiBroadReflection = dugiSampleIrradiance(giWorldPosition, dugiReflectionVector, giViewDirection, dugiSkyVisibilitySpecular) * OneOverPI; // broad probe reflection + sky-visibility along R
    dugiSkyVisibilitySpecular *= smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness);
#if defined(GI_DUGI_GLOSSY_RESIDUAL) && defined(GI_DUGI_GLOSSY_RADIANCE)
    vec3 dugiProbeReflection = mix(dugiSampleGlossyRadiance(giWorldPosition, giNormal, dugiReflectionVector, giViewDirection), dugiBroadReflection, smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness)); // sharp atlas <-> broad
#else
    vec3 dugiProbeReflection = max(vec3(0.0), DUGI_SH_EVALUATE(dugiRadianceSH, dugiReflectionVector)); // local SH reflection along R (no atlas)
#endif
    iblGIProbeSpecular = vec4(dugiProbeReflection, 1.0 - dugiSkyVisibilitySpecular);
    // Layer 2 (top crossfade): the env block scales the indirect specular (and, on the IBL_GI_PROBES paths, sheen / clearcoat
    // too) by giResidualIBLSpecularWeight; the dominant light takes the complementary (1 - weight) via doSingleLight.
    giResidualIBLSpecularWeight = smoothstep(GI_GLOSSY_ROUGHNESS_HI, GI_GLOSSY_ROUGHNESS_LO, giRoughness); // 1 = sharp (indirect), 0 = rough (dominant)
    dugiSpecularWeight = 1.0 - giResidualIBLSpecularWeight; // dominant's share (rough side), applied via doSingleLight below
#endif

    vec3 dugiDominantDiffusePart, dugiDominantSpecularPart;
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
                  0.0,                                                //
                  dugiDominantDiffusePart,                            //
                  dugiDominantSpecularPart);

    // The dominant directional light is probe-derived: add its real diffuse / specular split (from doSingleLight's output
    // parameters) to the GI debug channels and count it as 100% probe in the probe-influence heatmap.
    if(giDebugDisplay != 0u){
      giDebugIndirectDiffuse += dugiDominantDiffusePart;
      giDebugIndirectSpecular += dugiDominantSpecularPart;
      float dugiDominantLuminance = dot(dugiDominantDiffusePart + dugiDominantSpecularPart, giDebugLuminance);
      giDebugProbeInfluence += vec3(dugiDominantLuminance, dugiDominantLuminance, 0.0);
    }
  }

 #else // GI_DUGI_STORAGE octahedral

  // Octahedral storage: dugiSampleIrradiance returns the pre-integrated diffuse irradiance E(n) plus a sky-visibility factor.
  // The probe field replaces the environment-IBL diffuse; the env-IBL specular is kept (block below) but occluded by the probe
  // sky-visibility (sampled along the reflection vector) combined with the per-pixel AO. (DISABLED for now)

  // The env block below applies the BRDF split-sum, baseColor and occlusion; oct only fills the probes. There is no dominant
  // light on the oct path, so both residual weights are fully 1.0 (no dominant-vs-indirect crossfade); iblWeight stays 1.0.
  giResidualIBLDiffuseWeight = 1.0;
  giResidualIBLSpecularWeight = 1.0;

  // Diffuse: probe (local) <=> IBL, blended by the hemisphere sky-visibility (a probe sample along the normal)
  {
    float dugiSkyVisibilityDiffuse;
    vec3 dugiProbeDiffuse = dugiSampleIrradiance(giWorldPosition, giNormal, giViewDirection, dugiSkyVisibilityDiffuse) * OneOverPI;
    iblGIProbeDiffuse = vec4(dugiProbeDiffuse, 1.0 - dugiSkyVisibilityDiffuse);
  }

#if defined(MESH_FRAGMENT)
   // Diffuse transmission - back side (-normal), probe <=> IBL blended by the back-side hemisphere sky-visibility.
   if((giFlags & (1u << 16u)) != 0u){
     float dugiSkyVisibilityDiffuseBack;
     vec3 dugiProbeDiffuseTransmission = dugiSampleIrradiance(giWorldPosition, -giNormal, giViewDirection, dugiSkyVisibilityDiffuseBack) * OneOverPI;
     iblGIProbeDiffuseTransmission = vec4(dugiProbeDiffuseTransmission, 1.0 - dugiSkyVisibilityDiffuseBack);
   }
#endif

  // Specular: probe reflection <=> env sky, blended by the sky-visibility along R
  {
    vec3 dugiReflectionVector = normalize(reflect(-giViewDirection, giNormal));
    float dugiSkyVisibilitySpecular;
    vec3 dugiBroadReflection = dugiSampleIrradiance(giWorldPosition, dugiReflectionVector, giViewDirection, dugiSkyVisibilitySpecular) * OneOverPI; // broad reflection + sky-visibility along R
    dugiSkyVisibilitySpecular *= smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness);
#if defined(GI_DUGI_GLOSSY_RESIDUAL) && defined(GI_DUGI_GLOSSY_RADIANCE)
    vec3 dugiProbeReflection = mix(dugiSampleGlossyRadiance(giWorldPosition, giNormal, dugiReflectionVector, giViewDirection), dugiBroadReflection, smoothstep(GI_GLOSSY_ROUGHNESS_LO, GI_GLOSSY_ROUGHNESS_HI, giRoughness)); // sharp atlas <-> broad
#else
    vec3 dugiProbeReflection = dugiBroadReflection; // no glossy atlas: just the broad probe reflection
#endif
    iblGIProbeSpecular = vec4(dugiProbeReflection, 1.0 - dugiSkyVisibilitySpecular);
  }

#endif

#else

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Environment-IBL only: no GI-volume, so the full diffuse + specular comes from the environment reflection (split-sum).
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#endif

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Environment IBL (split-sum). All GI-volume modes feed it: CRH and DUGI (SH + oct) via IBL_GI_PROBES (the probe<=>env blend),
  // CVCT as an env fill, plus the pure-environment path. Skipped only in the reflective-shadow-map pass.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#if !defined(REFLECTIVESHADOWMAPOUTPUT)

#if (defined(GLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS) || defined(GLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING)) && !defined(IBL_GI_PROBES)
  if(iblWeight > 0.0)
#endif
  {

#ifdef IBL_GI_PROBES
    vec3 iblDiffuse = (giResidualIBLDiffuseWeight > 0.0)
                        ? (
                            (
                              (iblGIProbeDiffuse.w < 1.0)
                                ? mix(getIBLDiffuse(giNormal), iblGIProbeDiffuse.xyz, iblGIProbeDiffuse.w)
                                : iblGIProbeDiffuse.xyz
                            ) * giBaseColor * giResidualIBLDiffuseWeight
                         )
                        : vec3(0.0);
#else
    vec3 iblDiffuse = (giResidualIBLDiffuseWeight > 0.0) ? (getIBLDiffuse(giNormal) * giBaseColor * giResidualIBLDiffuseWeight) : vec3(0.0);
#endif

#if defined(MESH_FRAGMENT)
    // Diffuse transmission
    if((giFlags & (1u << 16u)) != 0u){
#ifdef IBL_GI_PROBES
      vec3 iblDiffuseTransmission = (
                                     (iblGIProbeDiffuseTransmission.w < 1.0)
                                       ? mix(getIBLDiffuse(-giNormal), iblGIProbeDiffuseTransmission.xyz, iblGIProbeDiffuseTransmission.w)
                                       : iblGIProbeDiffuseTransmission.xyz
                                    ) * diffuseTransmissionColorFactor;
#else
      vec3 iblDiffuseTransmission = getIBLDiffuse(-giNormal) * diffuseTransmissionColorFactor;
#endif
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

#ifdef IBL_GI_PROBES
    vec3 iblSpecularMetal = (giResidualIBLSpecularWeight > 0.0)
                               ? (
                                   (
                                     (iblGIProbeSpecular.w < 1.0)
                                       ? mix(getIBLRadianceGGX(giNormal, giViewDirection, giRoughness), iblGIProbeSpecular.xyz, iblGIProbeSpecular.w)
                                       : iblGIProbeSpecular.xyz
                                   ) * giResidualIBLSpecularWeight
                                )
                               : vec3(0.0);
#else
    vec3 iblSpecularMetal = (giResidualIBLSpecularWeight > 0.0) ? (getIBLRadianceGGX(giNormal, giViewDirection, giRoughness) * giResidualIBLSpecularWeight) : vec3(0.0);
#endif

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
#ifdef IBL_GI_PROBES
    // On the dominant-light paths (CRH / DUGI-SH) the indirect sheen + clearcoat are the SHARP-side share of the indirect <=>
    // dominant-light crossfade (the dominant light's sheen / clearcoat is added by doSingleLight, scaled by 1 - this weight),
    // so the reflection part must fade by the same giResidualIBLSpecularWeight as the base - otherwise both representations
    // would be at full strength at high roughness (double-counted sheen / coat). The sheen / coat absorption on the base stays.
    iblResultColor = fma(iblResultColor, vec3(iblAlbedoSheenScaling), iblSheen * giResidualIBLSpecularWeight);                     // sheen modulation (reflection faded)
    iblResultColor = mix(iblResultColor, iblClearcoatBRDF * giResidualIBLSpecularWeight, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation (reflection faded)
#else
    iblResultColor = fma(iblResultColor, vec3(iblAlbedoSheenScaling), iblSheen);                  // sheen modulation
    iblResultColor = mix(iblResultColor, iblClearcoatBRDF, giClearcoatFactor * giClearcoatFresnel); // clearcoat modulation
#endif
#endif

    colorOutput += iblResultColor * iblWeight; // final whole-result env gate (currently 1.0 on every path; the per-mode env reduction lives in the residual weights above)

    if(giDebugDisplay != 0u){
      // Combined indirect diffuse / specular for the debug channels: the diffuse part is (1 - dielectric Fresnel) of the
      // dielectric term and only on non-metals; the remainder is the specular part. The probe-vs-env blend (probe share) goes
      // into the separate brightness-weighted probe-influence heatmap instead of an artificial colour split.
      vec3 iblDiffusePart = (iblDiffuse * giDiffuseOcclusion) * (vec3(1.0) - iblDielectricFresnel) * (1.0 - giMetallic);
      vec3 iblSpecularPart = iblResultColor - iblDiffusePart;
      giDebugIndirectDiffuse += iblDiffusePart * iblWeight;
      giDebugIndirectSpecular += iblSpecularPart * iblWeight;
      float iblDiffuseLuminance = dot(iblDiffusePart, giDebugLuminance) * iblWeight;
      float iblSpecularLuminance = dot(iblSpecularPart, giDebugLuminance) * iblWeight;
#ifdef IBL_GI_PROBES
      // Probe share per lobe (iblGIProbe*.w): CRH / DUGI-SH crossfade the local probe against the environment.
      float giDebugDiffuseProbeShare = clamp(iblGIProbeDiffuse.w, 0.0, 1.0);
      float giDebugSpecularProbeShare = clamp(iblGIProbeSpecular.w, 0.0, 1.0);
      giDebugProbeInfluence += vec3((iblDiffuseLuminance * giDebugDiffuseProbeShare) + (iblSpecularLuminance * giDebugSpecularProbeShare), iblDiffuseLuminance + iblSpecularLuminance, 0.0);
#else
      // CVCT env fill / pure environment IBL: all environment (probe share 0); the CVCT cone above adds the probe part.
      giDebugProbeInfluence += vec3(0.0, iblDiffuseLuminance + iblSpecularLuminance, 0.0);
#endif
    }

  }

#endif

}
